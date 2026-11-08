# Dependency graph checks (igraph) and the per-node status machine.
# Edge direction: `from` is upstream, `to` depends on it. External data is not a node.

# Issue object used across extraction, checks, and the UI report (F8).
new_issue <- function(type, dataset, variables = character(), file = "variable.csv",
                      row = NA_integer_, text = "", message = "", suggestion = "",
                      severity = NULL, paramcd = NULL) {
  row <- suppressWarnings(as.integer(row))
  if (!length(row) || is.na(row[1])) row <- NA_integer_ else row <- row[1]
  type <- as.character(type)[1]
  if (is.null(severity) || !length(severity) || !nzchar(severity[[1]] %||% "")) {
    severity <- if (identical(type, "orphan")) "warning" else "error"
  }
  issue <- list(
    type = type,
    dataset = as.character(dataset %||% "")[1],
    variables = as.list(unique(flat_chr(variables))),
    file = as.character(file %||% "variable.csv")[1],
    row = row,
    text = paste(text %||% ""),
    message = paste(message %||% ""),
    suggestion = paste(suggestion %||% ""),
    severity = as.character(severity)[1]
  )
  # The parameter layer that reported it, so regenerating that layer can replace it.
  if (length(paramcd) && nzchar(as.character(paramcd)[1])) issue$paramcd <- toupper(as.character(paramcd)[1])
  issue
}

normalize_issue <- function(iss, dataset = NULL) {
  if (is.null(iss)) return(NULL)
  new_issue(
    type = iss$type %||% "unresolvable",
    dataset = iss$dataset %||% dataset %||% "",
    variables = iss$variables,
    file = iss$file %||% "variable.csv",
    row = if (is.null(iss$row)) NA_integer_ else iss$row,
    text = iss$text %||% "",
    message = iss$message %||% "",
    suggestion = iss$suggestion %||% "",
    severity = iss$severity,
    paramcd = iss$paramcd
  )
}

# Orphans are warnings. Anything else is an error unless the issue says otherwise.
issue_severity <- function(iss) {
  sev <- tolower(trimws(as.character(iss$severity %||% "")[1]))
  if (length(sev) == 1L && nzchar(sev) && !is.na(sev)) return(sev)
  if (identical(iss$type %||% "", "orphan")) "warning" else "error"
}

# A single record, a data.frame, or a list of records -> list of records.
as_records <- function(x) {
  if (is.null(x)) return(list())
  if (is.data.frame(x)) {
    if (!nrow(x)) return(list())
    return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  }
  if (is.list(x) && !is.null(names(x)) &&
      any(c("from", "id", "ref", "type", "op") %in% names(x)) &&
      !is.list(x$from %||% x$id %||% x$ref %||% x$type %||% x$op)) {
    return(list(x))
  }
  if (!is.list(x)) return(list())
  x
}

parse_node_id <- function(id) {
  id <- as.character(id)[1]
  paramcd <- NULL
  base <- id
  if (grepl("@", id, fixed = TRUE)) {
    paramcd <- sub("^[^@]*@", "", id)
    base <- sub("@[^@]*$", "", id)
    if (!nzchar(paramcd)) paramcd <- NULL
  }
  if (!grepl(".", base, fixed = TRUE)) {
    folder <- if (is.null(paramcd)) base else paste0(base, "@", paramcd)
    return(list(id = id, dataset = "", var = base, paramcd = paramcd, folder = folder))
  }
  dataset <- sub("\\..*$", "", base)
  var <- sub("^[^.]*\\.", "", base)
  folder <- if (is.null(paramcd)) var else paste0(var, "@", paramcd)
  list(id = id, dataset = dataset, var = var, paramcd = paramcd, folder = folder)
}

empty_graph <- function(dataset, spec_hash = NULL) {
  list(
    dataset = toupper(dataset),
    spec_hash = spec_hash %||% NULL,
    nodes = list(),
    groups = list(),
    edges = list(),
    external = list(),
    unresolved = list()
  )
}

normalize_graph <- function(graph) {
  if (is.null(graph)) return(empty_graph(""))
  graph$nodes <- as_records(graph$nodes)
  graph$groups <- as_records(graph$groups)
  graph$edges <- as_records(graph$edges)
  graph$external <- as_records(graph$external)
  graph$unresolved <- as_records(graph$unresolved)
  graph$dataset <- toupper(graph$dataset %||% "")
  # Old files may still carry the removed confirmation flag. Drop it.
  graph$confirmed <- NULL
  graph$confirmed_hash <- NULL
  graph
}

node_ids_of <- function(graph) {
  graph <- normalize_graph(graph)
  if (!length(graph$nodes)) return(character())
  flat_chr(lapply(graph$nodes, function(n) n$id))
}

# ---- checks (F6, F7, F7a) ----

spec_row_for <- function(spec, dataset, var) {
  v <- if (is.null(spec)) NULL else spec$variables
  if (is.null(v) || !nrow(v)) return(NA_integer_)
  hit <- toupper(v$Dataset) == toupper(dataset) & toupper(v$Variable) == toupper(var)
  if (!any(hit) || is.null(v$row)) return(NA_integer_)
  suppressWarnings(as.integer(v$row[which(hit)[1]]))
}

# Column names of each uploaded xpt. Domain is the uppercased file stem (dm.xpt -> DM).
# Delegates to the source catalog when that module is loaded, so values are
# read once and reused. The header-only scan remains for callers that have not
# sourced catalog.R.
data_index <- function(project) {
  if (exists("build_source_catalog", mode = "function") && exists("catalog_index", mode = "function")) {
    return(catalog_index(build_source_catalog(project)))
  }
  dir <- if (is.character(project)) project else (project$dir %||% "")
  dir <- file.path(dir, "data")
  if (!dir.exists(dir)) return(list())
  files <- list.files(dir, pattern = "\\.[Xx][Pp][Tt]$", full.names = TRUE)
  out <- list()
  for (f in files) {
    dom <- toupper(tools::file_path_sans_ext(basename(f)))
    if (dom %in% names(out)) next
    df <- haven::read_xpt(f, n_max = 0L)
    out[[dom]] <- as.character(names(df))
  }
  out
}

# DOMAIN.VAR tokens in a derivation or source sentence.
domain_var_refs <- function(text) {
  text <- toupper(text %||% "")
  out <- list()
  pos <- 1L
  n <- nchar(text)
  while (pos <= n) {
    subtxt <- substr(text, pos, n)
    m <- regexec("\\b([A-Z][A-Z0-9]*)\\.([A-Z][A-Z0-9]*)\\b", subtxt, perl = TRUE)
    r <- regmatches(subtxt, m)[[1]]
    if (!length(r)) break
    out[[length(out) + 1L]] <- list(dom = r[[2]], var = r[[3]], ref = paste0(r[[2]], ".", r[[3]]))
    start <- m[[1]][1]
    pos <- pos + start + nchar(r[[1]]) - 1L
    if (start < 1L) break
  }
  out
}

# VSTESTCD=SYSBP, VSTESTCD = 'SYSBP', VSTESTCD EQ SYSBP.
code_assignments <- function(text) {
  text <- toupper(gsub("[\"`]", "'", text %||% ""))
  out <- list()
  pos <- 1L
  n <- nchar(text)
  pat <- "\\b([A-Z][A-Z0-9_]*(?:TESTCD|PARAMCD|QNAM))\\s*(?:=|EQ)\\s*'?([A-Z0-9_]+)'?"
  while (pos <= n) {
    subtxt <- substr(text, pos, n)
    m <- regexec(pat, subtxt, perl = TRUE)
    r <- regmatches(subtxt, m)[[1]]
    if (!length(r)) break
    out[[length(out) + 1L]] <- list(col = r[[2]], value = r[[3]])
    start <- m[[1]][1]
    pos <- pos + start + nchar(r[[1]]) - 1L
    if (start < 1L) break
  }
  out
}

code_values_of <- function(catalog, dom, col) {
  vals <- catalog$domains[[dom]]$values %||% list()
  if (!length(vals)) return(character())
  i <- match(toupper(col), toupper(names(vals)))
  if (is.na(i)) return(character())
  toupper(as.character(unlist(vals[[i]], use.names = FALSE)))
}

domain_for_code_column <- function(col, text, catalog) {
  doms <- names(catalog$domains %||% list())
  if (!length(doms)) return(NULL)
  mentioned <- unique(vapply(domain_var_refs(text), function(r) r$dom, ""))
  mentioned <- mentioned[mentioned %in% doms]
  col_u <- toupper(col)
  for (d in mentioned) {
    vars <- toupper(vapply(catalog$domains[[d]]$variables %||% list(), function(v) as.character(v$name %||% "")[1], ""))
    valn <- toupper(names(catalog$domains[[d]]$values %||% list()))
    if (col_u %in% vars || col_u %in% valn) return(d)
  }
  for (d in doms) {
    if (col_u %in% c(paste0(d, "TESTCD"), paste0(d, "PARAMCD"), paste0(d, "QNAM"))) return(d)
  }
  NULL
}

# NULL when the defining test code cannot be determined.
# Otherwise list(found, domain, col, value).
parameter_code_hit <- function(paramcd, text, catalog, dataset) {
  if (is.null(catalog) || !length(catalog$domains)) return(NULL)
  text_u <- toupper(text %||% "")
  assigns <- code_assignments(text_u)
  if (length(assigns)) {
    placed <- list()
    for (a in assigns) {
      dom <- domain_for_code_column(a$col, text_u, catalog)
      if (is.null(dom)) next
      vals <- code_values_of(catalog, dom, a$col)
      placed[[length(placed) + 1L]] <- list(
        found = toupper(a$value) %in% vals, domain = dom, col = a$col, value = toupper(a$value))
    }
    if (!length(placed)) return(NULL)
    bad <- Filter(function(h) !isTRUE(h$found), placed)
    if (length(bad)) return(bad[[1]])
    return(placed[[1]])
  }
  doms <- names(catalog$domains)
  refs <- unique(vapply(domain_var_refs(text_u), function(r) r$dom, ""))
  refs <- refs[refs %in% doms & refs != toupper(dataset %||% "")]
  if (!length(refs)) return(NULL)
  checked <- FALSE
  for (dom in refs) {
    vals <- catalog$domains[[dom]]$values %||% list()
    cols <- names(vals)
    cols <- cols[grepl("TESTCD$", toupper(cols))]
    if (!length(cols)) next
    checked <- TRUE
    for (col in cols) {
      got <- toupper(as.character(unlist(vals[[col]], use.names = FALSE)))
      if (toupper(paramcd) %in% got) {
        return(list(found = TRUE, domain = dom, col = col, value = toupper(paramcd)))
      }
    }
  }
  if (!checked) return(NULL)
  dom <- refs[[1]]
  cols <- names(catalog$domains[[dom]]$values %||% list())
  cols <- cols[grepl("TESTCD$", toupper(cols))]
  list(found = FALSE, domain = dom, col = cols[[1]], value = toupper(paramcd))
}

parameter_value_issues <- function(dataset, spec, catalog) {
  params <- if (is.null(spec)) NULL else spec$parameters
  if (is.null(params) || !nrow(params) || is.null(catalog)) return(list())
  params <- params[toupper(params$Dataset) == toupper(dataset), , drop = FALSE]
  if (!nrow(params)) return(list())
  issues <- list()
  for (i in seq_len(nrow(params))) {
    pc <- toupper(trimws(as.character(params$PARAMCD[[i]] %||% "")))
    if (!nzchar(pc)) next
    src <- paste(params$Source[[i]] %||% "", params$Derivation[[i]] %||% "")
    hit <- parameter_code_hit(pc, src, catalog, dataset)
    if (is.null(hit) || isTRUE(hit$found)) next
    issues[[length(issues) + 1L]] <- new_issue(
      "missing_value", dataset, c(pc, hit$col, hit$value),
      file = "parameter.csv",
      row = params$row[[i]],
      text = trimws(src),
      message = paste0(dataset, ".", pc, ": ", hit$col, "=", hit$value,
                       " never occurs in ", hit$domain, ".", hit$col),
      suggestion = paste("Check", hit$value, "against the uploaded", hit$domain, "data."),
      severity = "error"
    )
  }
  issues
}

# Spec text DOMAIN.VAR that is not this dataset, checked against uploaded columns.
spec_ref_issues <- function(dataset, spec, data_index) {
  if (is.null(data_index) || is.null(spec)) return(list())
  issues <- list()
  add_text <- function(text, row, who, file) {
    for (r in domain_var_refs(text)) {
      if (identical(r$dom, toupper(dataset %||% ""))) next
      cols <- data_index[[r$dom]]
      used <- unique(c(who, r$ref))
      if (is.null(cols)) {
        issues[[length(issues) + 1L]] <<- new_issue(
          "missing_data", dataset, used, file = paste0(tolower(r$dom), ".xpt"),
          row = row, text = text,
          message = paste("Uploaded data is missing the", r$dom, "domain referenced by", r$ref),
          suggestion = paste("Upload", paste0(tolower(r$dom), ".xpt"))
        )
      } else if (!(r$var %in% toupper(as.character(cols)))) {
        issues[[length(issues) + 1L]] <<- new_issue(
          "missing_var", dataset, used, file = paste0(tolower(r$dom), ".xpt"),
          row = row, text = text,
          message = paste(r$ref, "is not a column in the uploaded", r$dom, "data"),
          suggestion = "Check the variable name against the xpt."
        )
      }
    }
  }
  vars <- spec$variables
  if (!is.null(vars) && nrow(vars)) {
    sub <- vars[toupper(vars$Dataset) == toupper(dataset), , drop = FALSE]
    for (i in seq_len(nrow(sub))) {
      who <- paste0(toupper(dataset), ".", toupper(sub$Variable[[i]] %||% ""))
      add_text(paste(sub$Source[[i]] %||% "", sub$Derivation[[i]] %||% ""), sub$row[[i]], who, "variable.csv")
    }
  }
  params <- spec$parameters
  if (!is.null(params) && nrow(params)) {
    sub <- params[toupper(params$Dataset) == toupper(dataset), , drop = FALSE]
    for (i in seq_len(nrow(sub))) {
      who <- toupper(sub$PARAMCD[[i]] %||% "")
      add_text(paste(sub$Source[[i]] %||% "", sub$Derivation[[i]] %||% ""), sub$row[[i]], who, "parameter.csv")
    }
  }
  issues
}

check_graph <- function(graph, spec = NULL, data_index = NULL, catalog = NULL) {
  graph <- normalize_graph(graph)
  ds <- graph$dataset
  ids <- node_ids_of(graph)
  issues <- lapply(graph$unresolved, function(iss) normalize_issue(iss, ds))
  issues <- Filter(Negate(is.null), issues)

  add <- function(iss) issues[[length(issues) + 1L]] <<- iss
  valid <- list()
  for (e in graph$edges) {
    from <- as.character(e$from %||% "")
    to <- as.character(e$to %||% "")
    if (!nzchar(from) || !nzchar(to)) next
    missing <- character()
    if (!(from %in% ids)) missing <- c(missing, from)
    if (!(to %in% ids)) missing <- c(missing, to)
    if (length(missing)) {
      add(new_issue(
        "missing_ref", ds, c(missing, to),
        row = spec_row_for(spec, ds, parse_node_id(to)$var),
        text = paste(e$evidence %||% "", from, "->", to),
        message = paste(paste(missing, collapse = ", "), "is not a node in", ds),
        suggestion = "Fix the reference or add the variable to the specification."
      ))
    } else {
      valid[[length(valid) + 1L]] <- list(from = from, to = to)
    }
  }
  # Drop duplicate valid edges. Self-loops stay: they are cycles.
  if (length(valid)) {
    key <- vapply(valid, function(e) paste(e$from, e$to, sep = "\t"), "")
    valid <- valid[!duplicated(key)]
  }

  if (length(ids)) {
    if (length(valid)) {
      el <- data.frame(from = vapply(valid, `[[`, "", "from"),
                       to = vapply(valid, `[[`, "", "to"), stringsAsFactors = FALSE)
      g <- igraph::graph_from_data_frame(el, directed = TRUE,
                                         vertices = data.frame(name = ids, stringsAsFactors = FALSE))
    } else {
      g <- igraph::make_empty_graph(n = length(ids), directed = TRUE)
      igraph::V(g)$name <- ids
    }
    if (!igraph::is_dag(g)) {
      sc <- igraph::components(g, mode = "strong")
      memb <- sc$membership
      loop_idx <- if (igraph::ecount(g)) which(igraph::which_loop(g)) else integer()
      loop_ends <- if (length(loop_idx)) igraph::ends(g, loop_idx, names = TRUE)[, 1] else character()
      for (k in unique(as.integer(memb))) {
        vids <- names(memb)[memb == k]
        cyc <- length(vids) > 1L || any(vids %in% loop_ends)
        if (!cyc) next
        add(new_issue(
          "cycle", ds, vids,
          message = paste("Cycle:", paste(vids, collapse = " -> ")),
          suggestion = "Fix the cycle in the spec, then re-extract the graph."
        ))
      }
    }
    din <- igraph::degree(g, mode = "in")
    dout <- igraph::degree(g, mode = "out")
    # An external ref is a dependency. A direct copy is not an orphan.
    ext_used <- character()
    for (ext in graph$external) ext_used <- unique(c(ext_used, flat_chr(ext$used_by)))
    orphans <- setdiff(names(din)[din == 0L & dout == 0L], ext_used)
    for (id in orphans) {
      pt <- parse_node_id(id)
      add(new_issue(
        "orphan", ds, id, row = spec_row_for(spec, ds, pt$var),
        message = paste(id, "has no dependencies and nothing depends on it."),
        suggestion = "Leave it if it is a direct copy with no upstream ADaM variable; otherwise add the missing edge."
      ))
    }
    order <- if (igraph::is_dag(g)) as.character(igraph::as_ids(igraph::topo_sort(g, mode = "out"))) else NULL
  } else {
    order <- character()
  }

  # A catalog passed in the data_index slot still yields column names.
  if (is.null(catalog) && is.list(data_index) && !is.null(data_index$domains) && is.list(data_index$domains)) {
    catalog <- data_index
    data_index <- if (exists("catalog_index", mode = "function")) catalog_index(catalog) else NULL
  }

  # NULL data_index means "not scanned"; an empty list means nothing is uploaded.
  if (!is.null(data_index)) {
    for (ext in graph$external) {
      dom <- toupper(ext$domain %||% "")
      var <- toupper(ext$var %||% "")
      ref <- ext$ref %||% paste0(dom, ".", var)
      used <- unique(c(flat_chr(ext$used_by), ref))
      cols <- data_index[[dom]]
      if (is.null(cols)) {
        add(new_issue(
          "missing_data", ds, used, file = paste0(tolower(dom), ".xpt"),
          text = ext$evidence %||% ref,
          message = paste("Uploaded data is missing the", dom, "domain referenced by", ref),
          suggestion = paste("Upload", paste0(tolower(dom), ".xpt"))
        ))
      } else if (!var %in% toupper(cols)) {
        add(new_issue(
          "missing_var", ds, used, file = paste0(tolower(dom), ".xpt"),
          text = ext$evidence %||% ref,
          message = paste(ref, "is not a column in the uploaded", dom, "data"),
          suggestion = "Check the variable name against the xpt."
        ))
      }
    }
  }

  for (iss in spec_ref_issues(ds, spec, data_index)) add(iss)
  for (iss in parameter_value_issues(ds, spec, catalog)) add(iss)

  if (length(issues)) {
    key <- vapply(issues, function(iss) {
      paste(iss$type, iss$dataset, paste(flat_chr(iss$variables), collapse = ","), iss$message, sep = "|")
    }, "")
    issues <- issues[!duplicated(key)]
  }
  list(issues = issues, order = order)
}

# Types that block generation. Orphans are reported but do not block.
blocking_issue_types <- function() {
  c("cycle", "missing_ref", "ambiguous", "unresolvable", "missing_data", "missing_var",
    "missing_value", "no_key", "structure")
}

node_blocked <- function(id, var, dataset, issues) {
  if (!length(issues)) return(FALSE)
  any(vapply(issues, function(iss) {
    if (!identical(issue_severity(iss), "error")) return(FALSE)
    if (!((iss$type %||% "") %in% blocking_issue_types())) return(FALSE)
    vars <- flat_chr(iss$variables)
    if (id %in% vars) return(TRUE)
    # A bare variable name blocks only when the issue did not already name specific nodes.
    ids_in <- vars[grepl(".", vars, fixed = TRUE)]
    if (!length(ids_in) && nzchar(var) && var %in% vars &&
        identical(toupper(iss$dataset %||% ""), toupper(dataset %||% ""))) return(TRUE)
    FALSE
  }, logical(1)))
}

# ---- status machine (F9a, F9b, F9c, F9i) ----

empty_side <- function() list(code = NULL, ok = NULL, error = NULL, out_path = NULL, log = NULL)

new_status <- function() {
  list(
    level = "none",
    problem = NULL,
    same_source = FALSE,
    updated = now_iso(),
    sides = list(primary = empty_side(), validation = empty_side())
  )
}

init_statuses <- function(graph) {
  ids <- node_ids_of(graph)
  st <- lapply(ids, function(id) new_status())
  names(st) <- ids
  st
}

level_rank <- function(level) {
  r <- c(none = 0L, L2 = 1L, L3 = 2L)
  hit <- r[[as.character(level)[1]]]
  if (is.null(hit) || is.na(hit)) 0L else hit
}

# `node` carries level and problem. Dependencies do not gate the buttons.
# Generate is always offered. Busy is a runtime 409, not a stored flag.
# Buttons: generate, compare, investigate.
actions <- function(node) {
  lvl <- node$level %||% "none"
  prob <- node$problem
  btns <- "generate"
  if (identical(lvl, "L2")) btns <- c(btns, "compare")
  if (!is.null(prob) && identical(prob$stage, "compare")) btns <- c(btns, "investigate")
  unique(btns)
}

upstream_map <- function(graph) {
  m <- list()
  for (e in normalize_graph(graph)$edges) {
    to <- as.character(e$to %||% "")
    from <- as.character(e$from %||% "")
    if (nzchar(to) && nzchar(from)) m[[to]] <- unique(c(m[[to]] %||% character(), from))
  }
  m
}

downstream_map <- function(graph) {
  m <- list()
  for (e in normalize_graph(graph)$edges) {
    to <- as.character(e$to %||% "")
    from <- as.character(e$from %||% "")
    if (nzchar(to) && nzchar(from)) m[[from]] <- unique(c(m[[from]] %||% character(), to))
  }
  m
}

descendants_of <- function(graph, id) {
  down <- downstream_map(graph)
  seen <- character()
  stack <- down[[id]] %||% character()
  while (length(stack)) {
    cur <- stack[[1]]
    stack <- stack[-1]
    if (!nzchar(cur) || cur %in% seen || identical(cur, id)) next
    seen <- c(seen, cur)
    stack <- c(stack, down[[cur]] %||% character())
  }
  seen
}

# Dependencies are advisory. Descendants keep their level and results.
# Each one records the upstream id that changed. Cleared when that node is generated or re-run.
flag_downstream <- function(statuses, graph, ids) {
  ids <- unique(as.character(ids))
  ids <- ids[nzchar(ids) & !is.na(ids)]
  if (!length(ids) || is.null(graph)) return(statuses)
  for (id in ids) {
    for (d in descendants_of(graph, id)) {
      if (d %in% ids) next
      st <- statuses[[d]] %||% new_status()
      cur <- as.character(unlist(st$upstream_changed, use.names = FALSE))
      cur <- cur[!is.na(cur) & nzchar(cur)]
      st$upstream_changed <- unique(c(cur, id))
      st$updated <- now_iso()
      statuses[[d]] <- st
    }
  }
  statuses
}

# Spec change uses to = "none". The node itself is reset. Descendants are only flagged.
demote_node <- function(statuses, graph, id, to = "none", problem = NULL) {
  st <- statuses[[id]] %||% new_status()
  st$level <- to
  st$problem <- problem
  st$upstream_changed <- NULL
  if (identical(to, "none")) {
    st$same_source <- FALSE
    st$sides <- list(primary = empty_side(), validation = empty_side())
  }
  st$updated <- now_iso()
  statuses[[id]] <- st
  if (!identical(to, "L3")) statuses <- flag_downstream(statuses, graph, id)
  statuses
}

advance_generate <- function(statuses, id, ok, message = NULL, stage = "run",
                             same_source = FALSE, sides = NULL, graph = NULL) {
  st <- statuses[[id]] %||% new_status()
  prev <- st$level %||% "none"
  if (isTRUE(ok)) {
    st$level <- "L2"
    st$problem <- NULL
    st$same_source <- isTRUE(same_source)
  } else {
    if (prev %in% c("L2", "L3")) st$level <- "none"
    st$problem <- list(stage = stage %||% "run", message = message %||% "Generate failed")
  }
  st$upstream_changed <- NULL
  if (!is.null(sides)) st$sides <- sides
  st$updated <- now_iso()
  statuses[[id]] <- st
  if (!is.null(graph)) statuses <- flag_downstream(statuses, graph, id)
  statuses
}

advance_compare <- function(statuses, graph, id, match, message = NULL) {
  st <- statuses[[id]] %||% new_status()
  if (!(st$level %in% c("L2", "L3"))) stop("Compare requires Generated state.", call. = FALSE)
  if (isTRUE(match)) {
    st$level <- "L3"
    st$problem <- NULL
    st$updated <- now_iso()
    statuses[[id]] <- st
    statuses
  } else {
    st$level <- "L2"
    st$problem <- list(stage = "compare", message = message %||% "Values do not match")
    st$updated <- now_iso()
    statuses[[id]] <- st
    flag_downstream(statuses, graph, id)
  }
}

# Annotate every node with actions and ready for the state payload.
annotate_nodes <- function(statuses, graph, issues = list()) {
  graph <- normalize_graph(graph)
  ids <- node_ids_of(graph)
  out <- list()
  for (id in ids) {
    st <- statuses[[id]] %||% new_status()
    changed <- as.character(unlist(st$upstream_changed, use.names = FALSE))
    changed <- changed[!is.na(changed) & nzchar(changed)]
    node <- list(
      id = id,
      level = st$level %||% "none",
      problem = st$problem,
      same_source = isTRUE(st$same_source),
      updated = st$updated %||% "",
      sides = st$sides %||% list(primary = empty_side(), validation = empty_side()),
      upstream_changed = if (length(changed)) as.list(unique(changed)) else NULL
    )
    node$actions <- actions(node)
    # An action is available and the node is not L3. Generate is always an action.
    node$ready <- !identical(node$level %||% "none", "L3") && length(node$actions) > 0
    out[[id]] <- node
  }
  out
}

# Group status is the minimum member level (F9i). problem is that member's problem.
group_status <- function(member_statuses) {
  if (!length(member_statuses)) return(list(level = "none", problem = NULL, same_source = FALSE))
  levels <- vapply(member_statuses, function(s) {
    if (is.character(s)) as.character(s)[1] else (s$level %||% "none")
  }, "")
  ranks <- vapply(levels, level_rank, 0L)
  i <- which.min(ranks)
  worst <- member_statuses[[i]]
  list(
    level = levels[[i]],
    problem = if (is.list(worst)) worst$problem else NULL,
    same_source = all(vapply(member_statuses, function(s) is.list(s) && isTRUE(s$same_source), logical(1)))
  )
}

annotate_groups <- function(graph, node_views) {
  graph <- normalize_graph(graph)
  out <- list()
  for (g in graph$groups) {
    members <- flat_chr(g$members)
    sts <- lapply(members, function(id) {
      v <- node_views[[id]]
      if (is.null(v)) new_status() else v
    })
    gs <- group_status(sts)
    gs$id <- g$id
    gs$paramcd <- g$paramcd %||% NULL
    gs$members <- as.list(members)
    out[[g$id]] <- gs
  }
  out
}
