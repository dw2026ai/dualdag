# Project folder, uploads, and persisted state (.dualdag/state). F2, F2a, F5a, F9h.
#
# <project>/spec/variable.csv, spec/parameter.csv
# <project>/data/<domain>.xpt
# <project>/.dualdag/state/state.json, graph_<DS>.json, log_<DS>.json
# <project>/.dualdag/runs/<DS>/<VAR>/<side>/
# <project>/output/

project_dir_of <- function(p) if (is.character(p)) p else p$dir

blank_side <- function() list(ok = NULL, error = NULL, updated = NULL)

blank_status <- function() {
  list(level = "none", problem = NULL, same_source = FALSE, updated = now_iso(),
       sides = list(primary = blank_side(), validation = blank_side()))
}

empty_state <- function() {
  list(version = 1L, files = list(variable = NULL, parameter = NULL),
       data = empty_named(), nodes = empty_named(), row_hash = empty_named(),
       errors = empty_named(), updated = now_iso())
}

split_node_id <- function(id) {
  if (!is.character(id) || length(id) != 1 || is.na(id) || !nzchar(id)) return(NULL)
  m <- regexec("^([A-Za-z0-9_]+)\\.([A-Za-z0-9_]+)(?:@([A-Za-z0-9_]+))?$", id)
  r <- regmatches(id, m)[[1]]
  if (length(r) < 3) return(NULL)
  list(dataset = toupper(r[2]), var = toupper(r[3]),
       paramcd = if (length(r) >= 4 && nzchar(r[4])) toupper(r[4]) else NULL)
}

safe_ds <- function(ds) {
  ds <- toupper(trimws(if (is.null(ds)) "" else as.character(ds)[1]))
  if (!grepl("^[A-Z][A-Z0-9_]*$", ds)) stop("Invalid dataset name.", call. = FALSE)
  ds
}

canonical_node_id <- function(ds, id) {
  id <- toupper(trimws(as.character(id %||% "")))
  parsed <- split_node_id(id)
  if (is.null(parsed) || !identical(parsed$dataset, ds)) stop("Invalid node id.", call. = FALSE)
  paste0(parsed$dataset, ".", parsed$var, if (is.null(parsed$paramcd)) "" else paste0("@", parsed$paramcd))
}

node_var_folder <- function(id) {
  parsed <- split_node_id(id)
  if (is.null(parsed)) stop("Invalid node id.", call. = FALSE)
  if (is.null(parsed$paramcd)) parsed$var else paste0(parsed$var, "@", parsed$paramcd)
}

node_run_dir <- function(p, ds, id) file.path(project_dir_of(p), ".dualdag", "runs", ds, node_var_folder(id))

graph_path <- function(p, ds) file.path(project_dir_of(p), ".dualdag", "state", paste0("graph_", ds, ".json"))
layout_path <- function(p, ds) file.path(project_dir_of(p), ".dualdag", "state", paste0("layout_", safe_ds(ds), ".json"))
load_layout <- function(p, ds) {
  raw <- read_json(layout_path(p, ds))
  if (is.null(raw) || !is.list(raw) || !is.list(raw$positions)) return(empty_named())
  raw$positions
}
save_layout <- function(p, ds, positions) {
  ds <- safe_ds(ds)
  if (!is.list(positions)) stop("positions must be an object.", call. = FALSE)
  graph <- load_graph(p, ds)
  if (is.null(graph)) stop("No graph.", call. = FALSE)
  allowed <- unique(c(vapply(graph$nodes, function(n) as.character(n$id), ""),
                      unlist(lapply(graph$groups, function(g) flat_chr(g$members)), use.names = FALSE)))
  spec <- read_project_spec(p)
  if (!is.null(spec) && nrow(spec$variables)) {
    vars <- spec$variables
    keys <- vars[toupper(vars$Dataset) == ds & !is.na(suppressWarnings(as.integer(vars$Key))), , drop = FALSE]
    allowed <- unique(c(allowed, paste0(ds, ".", toupper(keys$Variable))))
  }
  clean <- empty_named()
  for (key in names(positions)) {
    id <- canonical_node_id(ds, key)
    if (!(id %in% allowed)) next
    xy <- positions[[key]]
    x <- suppressWarnings(as.numeric(xy$x)); y <- suppressWarnings(as.numeric(xy$y))
    if (length(x) != 1L || length(y) != 1L || !is.finite(x) || !is.finite(y) ||
        abs(x) > 1e6 || abs(y) > 1e6) stop("Invalid layout position.", call. = FALSE)
    clean[[id]] <- list(x = x, y = y)
  }
  write_json(list(positions = clean), layout_path(p, ds))
  clean
}
reset_layout <- function(p, ds) {
  unlink(layout_path(p, ds))
  invisible()
}
log_path <- function(p, ds) file.path(project_dir_of(p), ".dualdag", "state", paste0("log_", ds, ".json"))

read_state_file <- function(p) {
  st <- read_json(file.path(p$state_dir, "state.json"))
  base <- empty_state()
  if (is.null(st) || !is.list(st)) return(base)
  for (k in names(base)) if (is.null(st[[k]])) st[[k]] <- base[[k]]
  if (!is.list(st$files)) st$files <- base$files
  for (ds in names(st$nodes %||% list())) for (id in names(st$nodes[[ds]] %||% list())) {
    node <- st$nodes[[ds]][[id]]
    if (is.list(node) && identical(as.character(node$level %||% "")[1], "L1")) {
      node$level <- "none"
      st$nodes[[ds]][[id]] <- node
    }
  }
  st$version <- 1L
  st
}

write_state <- function(p) {
  p$state$updated <- now_iso()
  p$state$version <- 1L
  write_json(p$state, file.path(p$state_dir, "state.json"))
  if (exists("refresh_all_work", mode = "function", inherits = TRUE)) {
    tryCatch(refresh_all_work(p), error = function(e) NULL)
  }
  invisible(p$state)
}

get_status <- function(p, ds, id) {
  st <- p$state$nodes[[ds]][[id]]
  if (is.null(st) || !is.list(st)) st <- blank_status()
  if (is.null(st$level) || !nzchar(st$level)) st$level <- "none"
  if (identical(as.character(st$level)[1], "L1")) st$level <- "none"
  if (is.null(st$sides) || !is.list(st$sides)) st$sides <- list(primary = blank_side(), validation = blank_side())
  if (is.null(st$sides$primary) || !is.list(st$sides$primary)) st$sides$primary <- blank_side()
  if (is.null(st$sides$validation) || !is.list(st$sides$validation)) st$sides$validation <- blank_side()
  if (is.null(st$same_source)) st$same_source <- FALSE
  changed <- as.character(unlist(st$upstream_changed, use.names = FALSE))
  changed <- changed[!is.na(changed) & nzchar(changed)]
  st$upstream_changed <- if (length(changed)) unique(changed) else NULL
  if (!is.null(st$warnings)) {
    w <- as.character(unlist(st$warnings, use.names = FALSE))
    w <- w[!is.na(w) & nzchar(w)]
    st$warnings <- if (length(w)) unique(w) else NULL
  }
  st
}

put_status <- function(p, ds, id, st, save = TRUE) {
  if (is.null(p$state$nodes) || !is.list(p$state$nodes)) p$state$nodes <- empty_named()
  if (is.null(p$state$nodes[[ds]]) || !is.list(p$state$nodes[[ds]])) p$state$nodes[[ds]] <- empty_named()
  if (identical(as.character(st$level %||% "")[1], "L1")) st$level <- "none"
  st$updated <- now_iso()
  p$state$nodes[[ds]][[id]] <- st
  if (save) write_state(p)
  st
}

clear_node_results <- function(p, ds, id) {
  d <- node_run_dir(p, ds, id)
  if (dir.exists(d)) unlink(d, recursive = TRUE)
}

# Drop this variable's column from both saved work frames. Refresh rebuilds
# work from L3 afterwards; this covers a frame that is still on disk.
drop_work_column <- function(p, ds, id) {
  parsed <- split_node_id(id)
  if (is.null(parsed) || !exists("work_path", mode = "function")) return(invisible())
  cols <- parsed$var
  if (!is.null(parsed$paramcd) && nzchar(parsed$paramcd))
    cols <- c(cols, paste0(parsed$var, "__", parsed$paramcd))
  root <- project_dir_of(p)
  for (side in c("primary", "validation")) {
    path <- work_path(root, ds, side)
    if (!file.exists(path) || dir.exists(path)) next
    df <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.data.frame(df)) next
    drop <- intersect(cols, names(df))
    if (!length(drop)) next
    df[drop] <- NULL
    saveRDS(df, path)
  }
  invisible()
}

# One variable back to Not generated. Results go. Code and notes stay unless delete_code.
# Logs stay either way. Descendants are flagged, not demoted.
reset_one_node <- function(p, ds, id, delete_code = FALSE) {
  base <- node_run_dir(p, ds, id)
  for (side in c("primary", "validation")) {
    unlink(file.path(base, side, "out.rds"))
    unlink(file.path(base, side, c("coder_context.json", "coder_process.json")))
    card_remove(p, ds, id, side)
    if (exists("review_remove", mode = "function")) review_remove(p, ds, id, side)
    if (isTRUE(delete_code)) {
      unlink(c(
        file.path(base, side, "code.R"),
        file.path(base, side, "notes.json"),
        file.path(base, side, "code_note.json")
      ))
    }
  }
  unlink(c(file.path(base, "compare.json"), file.path(base, "qc_report.md")))
  drop_work_column(p, ds, id)
  st <- get_status(p, ds, id)
  st$level <- "none"
  st$problem <- NULL
  st$same_source <- FALSE
  st$upstream_changed <- NULL
  st$warnings <- NULL
  st$sides <- list(primary = blank_side(), validation = blank_side())
  put_status(p, ds, id, st, save = FALSE)
  g <- tryCatch(load_graph(p, ds), error = function(e) NULL)
  flag_downstream_of(p, ds, id, g)
  write_state(p)
  get_status(p, ds, id)
}

variable_fingerprint <- function(row) {
  bits <- c(row$Dataset, row$Variable, row$Label, row$Type, as.character(row$Length),
            as.character(row$Key), row$Source, row$Derivation)
  bits[is.na(bits)] <- ""
  paste(as.character(openssl::md5(charToRaw(paste(bits, collapse = "\r")))))
}

node_fingerprint <- function(spec, id) {
  parsed <- split_node_id(id)
  if (is.null(parsed) || is.null(spec)) return(NULL)
  v <- spec$variables
  ds <- toupper(v$Dataset)
  vars <- toupper(v$Variable)
  if (identical(parsed$var, "RECORDS")) {
    key_rows <- which(ds == parsed$dataset & !is.na(v$Key) & nzchar(trimws(as.character(v$Key))))
    if (!length(key_rows)) return(NULL)
    key_hashes <- vapply(key_rows, function(i) {
      paste0(toupper(v$Variable[i]), ":", variable_fingerprint(v[i, , drop = FALSE]))
    }, "")
    parts <- sort(key_hashes)
    if (!is.null(parsed$paramcd)) {
      a <- spec$parameters
      ph <- which(toupper(a$Dataset) == parsed$dataset & toupper(a$PARAMCD) == parsed$paramcd)
      if (!length(ph)) return(NULL)
      row <- a[ph[1], , drop = FALSE]
      pbits <- c(toupper(row$PARAMCD), row$PARAM, row$Source, row$Derivation)
      pbits[is.na(pbits)] <- ""
      parts <- c(parts, paste(pbits, collapse = "\r"))
    }
    return(paste0(as.character(openssl::md5(charToRaw(paste(parts, collapse = "\n"))))))
  }
  hit <- which(ds == parsed$dataset & vars == parsed$var)
  if (!length(hit)) return(NULL)
  base <- variable_fingerprint(v[hit[1], , drop = FALSE])
  if (is.null(parsed$paramcd)) return(base)
  a <- spec$parameters
  ph <- which(toupper(a$Dataset) == parsed$dataset & toupper(a$PARAMCD) == parsed$paramcd)
  if (!length(ph)) return(NULL)
  extra <- paste(c(a$PARAMCD[ph[1]], a$PARAM[ph[1]], a$Source[ph[1]], a$Derivation[ph[1]]), collapse = "\r")
  paste(as.character(openssl::md5(charToRaw(paste(base, extra, sep = "\n")))))
}

rebuild_row_hash <- function(p, spec) {
  h <- empty_named()
  if (!is.null(spec) && nrow(spec$variables)) {
    v <- spec$variables
    for (i in seq_len(nrow(v))) {
      if (!nzchar(v$Dataset[i]) || !nzchar(v$Variable[i])) next
      id <- paste0(v$Dataset[i], ".", v$Variable[i])
      h[[id]] <- variable_fingerprint(v[i, , drop = FALSE])
    }
  }
  if (!is.null(p$state$nodes)) {
    for (ds in names(p$state$nodes)) {
      for (id in names(p$state$nodes[[ds]])) {
        fp <- node_fingerprint(spec, id)
        if (!is.null(fp)) h[[id]] <- fp
      }
    }
  }
  p$state$row_hash <- h
  invisible(h)
}

as_record_list <- function(x) {
  if (is.null(x)) return(list())
  if (is.data.frame(x)) {
    if (!nrow(x)) return(list())
    return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  }
  if (!is.list(x)) return(list())
  if (!is.null(x$from) || !is.null(x$id) || !is.null(x$ref) || !is.null(x$type) || !is.null(x$var))
    return(list(x))
  lapply(unname(x), function(e) if (is.list(e)) e else list(value = e))
}

coerce_graph <- function(graph, dataset, spec_hash = NULL) {
  if (is.null(graph) || !is.list(graph)) graph <- list()
  graph$dataset <- dataset
  if (!is.null(spec_hash)) graph$spec_hash <- spec_hash
  graph$nodes <- as_record_list(graph$nodes)
  graph$groups <- as_record_list(graph$groups)
  graph$edges <- as_record_list(graph$edges)
  graph$external <- as_record_list(graph$external)
  graph$unresolved <- as_record_list(graph$unresolved)
  # Old graph.json files may still have these. They are not a gate anymore.
  graph$confirmed <- NULL
  graph$confirmed_hash <- NULL
  graph
}

load_graph <- function(p, ds) {
  g <- read_json(graph_path(p, ds))
  if (is.null(g)) return(NULL)
  coerce_graph(g, ds)
}

save_graph <- function(p, graph) {
  ds <- safe_ds(graph$dataset)
  graph <- coerce_graph(graph, ds, if (is.null(graph$spec_hash)) NULL else graph$spec_hash)
  write_json(graph, graph_path(p, ds))
  if (exists("write_dag_md", mode = "function")) write_dag_md(p, graph)
  if (exists("write_network_json", mode = "function")) write_network_json(p, ds)
  invisible(graph)
}

network_dir <- function(p, ds) file.path(project_dir_of(p), ".dualdag", "state", paste0("network_", safe_ds(ds)))

network_link <- function(from, to) {
  from <- as.character(from %||% "")[1]; to <- as.character(to %||% "")[1]
  if (!nzchar(from) || !nzchar(to) || identical(from, to)) return(NULL)
  list(from = from, to = to)
}

write_network_json <- function(p, ds) {
  ds <- safe_ds(ds)
  pg <- if (exists("param_graph_read", mode = "function")) param_graph_read(p, ds) else NULL
  graph <- load_graph(p, ds)
  root <- network_dir(p, ds)
  if (is.null(pg) && is.null(graph)) { unlink(root, recursive = TRUE); return(invisible(NULL)) }
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  hash <- pg$spec_hash %||% graph$spec_hash %||% NULL
  save_if_changed <- function(value, name) {
    path <- file.path(root, name)
    encoded <- jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", na = "null", digits = NA, force = TRUE, pretty = TRUE)
    text <- paste0(as.character(encoded), "\n")
    if (!file.exists(path) || !identical(read_text(path), sub("\n$", "", text))) write_text(text, path)
    path
  }
  if (!is.null(pg)) {
    nodes <- as_record_list(pg$nodes %||% list())
    parameters <- unique(vapply(nodes, function(n) toupper(as.character(n$code %||% n$id %||% "")), ""))
    parameters <- parameters[nzchar(parameters)]
    edge_key <- function(from, to) paste(toupper(as.character(from %||% "")[1]),
      toupper(as.character(to %||% "")[1]), sep = "\t")
    clean_uses <- function(values) {
      out <- list(); seen <- character()
      for (u in as_records(values)) {
        from_var <- as.character(u$from_var %||% "")[1]
        to_var <- as.character(u$to_var %||% "")[1]
        key <- paste(toupper(from_var), toupper(to_var), sep = "\t")
        if (!nzchar(from_var) || !nzchar(to_var) || key %in% seen) next
        seen <- c(seen, key)
        out <- c(out, list(list(from_var = from_var, to_var = to_var)))
      }
      out
    }
    edges <- list(); edge_index <- character()
    for (e in as_record_list(pg$edges %||% list())) {
      from <- as.character(e$from %||% "")[1]; to <- as.character(e$to %||% "")[1]
      key <- edge_key(from, to)
      if (!nzchar(from) || !nzchar(to)) next
      at <- match(key, edge_index)
      if (is.na(at)) {
        edges <- c(edges, list(list(from = from, to = to, uses = clean_uses(e$uses %||% list()))))
        edge_index <- c(edge_index, key)
      } else edges[[at]]$uses <- clean_uses(c(edges[[at]]$uses %||% list(), e$uses %||% list()))
    }
    extraction <- graph$extraction %||% list()
    classes <- as_records(extraction$parameter_classification$parameters)
    carry_over <- vapply(Filter(function(x) identical(x$classification,"carry_over"), classes), function(x) toupper(x$paramcd %||% ""), "")
    if (!length(carry_over)) {
      group_layer <- Filter(function(x) identical(toupper(x$paramcd %||% ""),"CARRY_OVER"), as_records(graph$layers))
      if (length(group_layer)) carry_over <- toupper(flat_chr(group_layer[[1]]$members_parameters))
    }
    carry_over <- carry_over[nzchar(carry_over)]
    save_if_changed(list(dataset = ds, spec_hash = hash, parameters = as.list(parameters),
      carry_over = as.list(carry_over), edges = edges), "_parameters.json")
  }
  if (!is.null(graph)) {
    nodes <- as_record_list(graph$nodes %||% list())
    node_id <- function(n) as.character(n$id %||% "")[1]
    param_of <- function(n) toupper(as.character(n$paramcd %||% "")[1])
    node_ids <- vapply(nodes, node_id, "")
    generated <- unique(toupper(flat_chr(graph$generated_parameters %||% list())))
    has_dataset_nodes <- any(!nzchar(vapply(nodes, param_of, "")))
    carry_members <- unique(vapply(Filter(function(x) identical(x$classification,"carry_over"), as_records(graph$extraction$parameter_classification$parameters)), function(x) toupper(x$paramcd %||% ""), ""))
    if (!length(carry_members)) {
      group_layer <- Filter(function(x) identical(toupper(x$paramcd %||% ""),"CARRY_OVER"), as_records(graph$layers))
      if (length(group_layer)) carry_members <- toupper(flat_chr(group_layer[[1]]$members_parameters))
    }
    carry_generated <- "CARRY_OVER" %in% generated || length(intersect(generated, carry_members)) > 0L
    scopes <- unique(c(setdiff(generated, carry_members), if (carry_generated) "CARRY_OVER", if (length(generated) && has_dataset_nodes) ""))
    semantic_parameter <- function(layer) {
      hit <- Filter(function(x) identical(toupper(x$paramcd %||% ""),toupper(layer)),as_record_list(graph$layers %||% list()))
      members <- if(length(hit)) toupper(flat_chr(hit[[1]]$members_parameters)) else character()
      if(length(members)==1L && !identical(toupper(layer),"CARRY_OVER")) members[[1]] else layer
    }
    make_file <- function(parameter) {
      own <- nodes[vapply(nodes, function(n) identical(param_of(n), parameter), TRUE)]
      ids <- vapply(own, node_id, ""); ids <- ids[nzchar(ids)]
      edges <- list(); inputs <- list()
      add_unique <- function(target, link) {
        if (is.null(link)) return(target)
        key <- paste(link$from, link$to, sep = "\t")
        if (!key %in% vapply(target, function(x) paste(x$from, x$to, sep = "\t"), "")) c(target, list(link)) else target
      }
      for (e in as_record_list(graph$edges %||% list())) {
        from <- as.character(e$from %||% "")[1]; to <- as.character(e$to %||% "")[1]
        if (to %in% ids && from %in% ids) edges <- add_unique(edges, network_link(from, to))
        else if (to %in% ids && (!from %in% node_ids || any(vapply(nodes[node_ids == from], function(n) !nzchar(param_of(n)), TRUE))))
          inputs <- add_unique(inputs, network_link(from, to))
      }
      for (source in as_record_list(graph$external %||% list())) {
        ref <- as.character(source$ref %||% paste(source$domain %||% "", source$var %||% "", sep = "."))[1]
        for (target in flat_chr(source$used_by %||% list())) {
          target_id <- target
          if (!target_id %in% ids) {
            matches <- own[vapply(own, function(n) identical(toupper(as.character(n$var %||% "")), toupper(target)), TRUE)]
            if (length(matches)) target_id <- node_id(matches[[1]])
          }
          if (target_id %in% ids) inputs <- add_unique(inputs, network_link(ref, target_id))
        }
      }
      list(dataset = ds, parameter = if (nzchar(parameter)) semantic_parameter(parameter) else NULL,
        layer_id = if (nzchar(parameter) && !identical(semantic_parameter(parameter),parameter)) parameter else NULL,
        members = if (identical(parameter,"CARRY_OVER")) as.list(carry_members) else NULL, spec_hash = hash,
        variables = as.list(ids), edges = edges, inputs = inputs)
    }
    for (parameter in scopes) {
      filename <- if (nzchar(parameter)) paste0(parameter, ".json") else "_dataset.json"
      make_file(parameter) |> save_if_changed(filename)
    }
  }
  expected <- c(if (!is.null(pg)) "_parameters.json", if (!is.null(graph) && "" %in% scopes) "_dataset.json",
    if (!is.null(graph)) paste0(setdiff(scopes, ""), ".json"))
  existing <- list.files(root, pattern = "\\.json$", full.names = TRUE)
  stale <- existing[!basename(existing) %in% expected]
  if (length(stale)) unlink(stale)
  invisible(root)
}

downstream_ids <- function(graph, ids) {
  if (is.null(graph)) return(character())
  edges <- graph$edges %||% list()
  seen <- character()
  wave <- unique(ids[nzchar(ids)])
  guard <- 0L
  while (length(wave) && guard < 10000L) {
    guard <- guard + 1L
    nxt <- character()
    for (e in edges) {
      if (!is.list(e)) next
      frm <- e$from %||% ""
      to <- e$to %||% ""
      if (nzchar(frm) && nzchar(to) && frm %in% wave && !(to %in% c(seen, wave, nxt))) nxt <- c(nxt, to)
    }
    seen <- c(seen, nxt)
    wave <- unique(nxt)
  }
  unique(seen)
}

upstream_ids <- function(graph, id) {
  if (is.null(graph)) return(character())
  ups <- unlist(lapply(graph$edges %||% list(), function(e) {
    if (is.list(e) && identical(as.character(e$to %||% ""), id)) as.character(e$from %||% "") else NULL
  }), use.names = FALSE)
  if (is.null(ups)) return(character())
  unique(ups[nzchar(ups)])
}

# Dependencies are advisory. Descendants keep their level and results.
# Each records the upstream ids that changed. Cleared when that node is generated or re-run.
flag_downstream_of <- function(p, ds, origin_ids, graph = NULL) {
  if (is.null(graph)) graph <- load_graph(p, ds)
  origin_ids <- unique(as.character(origin_ids %||% character()))
  origin_ids <- origin_ids[nzchar(origin_ids) & !is.na(origin_ids)]
  if (!length(origin_ids) || is.null(graph)) return(invisible(character()))
  causes <- list()
  for (origin in origin_ids) {
    for (id in downstream_ids(graph, origin)) {
      if (!nzchar(id) || id %in% origin_ids) next
      causes[[id]] <- unique(c(causes[[id]] %||% character(), origin))
    }
  }
  for (id in names(causes)) {
    st <- get_status(p, ds, id)
    cur <- if (is.null(st$upstream_changed)) character() else as.character(st$upstream_changed)
    st$upstream_changed <- unique(c(cur, causes[[id]]))
    put_status(p, ds, id, st, save = FALSE)
  }
  invisible(names(causes))
}

demote_all_generated <- function(p) {
  if (is.null(p$state$nodes)) return(invisible())
  for (ds in names(p$state$nodes)) {
    ids <- names(p$state$nodes[[ds]])
    for (id in ids) {
      st <- get_status(p, ds, id)
      if (st$level %in% c("L2", "L3")) {
        st$level <- "none"
        st$problem <- NULL
        st$same_source <- FALSE
        st$upstream_changed <- NULL
        st$sides <- list(primary = blank_side(), validation = blank_side())
        clear_node_results(p, ds, id)
        put_status(p, ds, id, st, save = FALSE)
      }
    }
  }
}

demote_for_spec_change <- function(p, spec) {
  old_h <- p$state$row_hash
  if (is.null(old_h)) old_h <- empty_named()
  none_ids <- list()
  if (is.null(p$state$nodes)) return(invisible())
  for (ds in names(p$state$nodes)) {
    ids <- names(p$state$nodes[[ds]])
    for (id in ids) {
      prev <- old_h[[id]]
      cur <- if (is.null(spec)) NULL else node_fingerprint(spec, id)
      if (is.null(prev)) next
      if (!is.null(cur) && !identical(prev, cur)) {
        put_status(p, ds, id, blank_status(), save = FALSE)
        none_ids[[ds]] <- c(none_ids[[ds]] %||% character(), id)
      } else if (is.null(cur)) {
        graph <- tryCatch(load_graph(p, ds), error = function(e) NULL)
        in_graph <- !is.null(graph_node(graph, id))
        in_spec <- !is.null(spec) && !is.null(node_fingerprint(spec, id))
        if (!in_graph && !in_spec) {
          clear_node_results(p, ds, id)
          p$state$nodes[[ds]][[id]] <- NULL
          none_ids[[ds]] <- c(none_ids[[ds]] %||% character(), id)
        }
      }
    }
  }
  for (ds in names(none_ids)) flag_downstream_of(p, ds, none_ids[[ds]])
  invisible(none_ids)
}

data_hashes <- function(p) {
  cur <- empty_named()
  nms <- list.files(file.path(p$dir, "data"))
  for (nm in nms) {
    if (!grepl("\\.xpt$", nm, ignore.case = TRUE)) next
    cur[[tolower(nm)]] <- file_md5(file.path(p$dir, "data", nm))
  }
  cur
}

sync_external <- function(p) {
  var <- file.path(p$dir, "spec", "variable.csv")
  par <- file.path(p$dir, "spec", "parameter.csv")
  vh <- file_md5(var)
  ph <- file_md5(par)
  old_files <- if (is.list(p$state$files)) p$state$files else list()
  spec_changed <- !identical(old_files$variable, vh) || !identical(old_files$parameter, ph)
  cur <- data_hashes(p)
  old_data <- if (is.null(p$state$data) || !length(p$state$data)) empty_named() else p$state$data
  changed_data <- FALSE
  for (nm in names(old_data)) if (!identical(old_data[[nm]], cur[[nm]])) changed_data <- TRUE
  has_prior <- !is.null(old_files$variable) || !is.null(old_files$parameter) || length(names(old_data)) > 0
  parsed <- NULL
  keep_old_hash <- FALSE
  if (spec_changed && file.exists(var)) {
    parsed <- tryCatch(read_project_spec(p), error = function(e) e)
    if (inherits(parsed, "error")) {
      p$state$errors$spec <- conditionMessage(parsed)
      keep_old_hash <- TRUE
      spec_changed <- FALSE
    } else if (!is.null(p$state$errors$spec)) {
      p$state$errors$spec <- NULL
    }
  }
  if (has_prior && spec_changed) {
    demote_for_spec_change(p, parsed)
  }
  if (has_prior && changed_data) demote_all_generated(p)
  if (keep_old_hash) {
    vh <- old_files$variable
    ph <- old_files$parameter
  }
  p$state$files <- list(variable = vh, parameter = ph)
  p$state$data <- cur
  need_hash <- spec_changed || !length(p$state$row_hash)
  if (!keep_old_hash && need_hash) {
    if (!file.exists(var)) {
      p$state$row_hash <- empty_named()
    } else {
      if (is.null(parsed)) parsed <- tryCatch(read_project_spec(p), error = function(e) NULL)
      if (is.list(parsed)) rebuild_row_hash(p, parsed)
    }
  }
  write_state(p)
  invisible(p)
}

open_project <- function(dir) {
  dir <- normalizePath(path.expand(dir), winslash = "/", mustWork = FALSE)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)
  for (sub in c("spec", "data", file.path(".dualdag", "state"), file.path(".dualdag", "runs"), "output"))
    dir.create(file.path(dir, sub), recursive = TRUE, showWarnings = FALSE)
  p <- new.env(parent = emptyenv())
  p$dir <- dir
  p$state_dir <- file.path(dir, ".dualdag", "state")
  p$state <- read_state_file(p)
  sync_external(p)
  downgrade_empty_l3(p)
  if (exists("refresh_all_work", mode = "function", inherits = TRUE)) refresh_all_work(p)
  card_refresh_project(p)
  p
}

# Remove project work while refusing any path whose resolved target escapes the
# project root. Symlinks inside the project are removed as links, never walked.
project_wipe_work <- function(project) {
  root <- normalizePath(project_dir_of(project), winslash = "/", mustWork = TRUE)
  if (identical(root, "/")) stop("Refusing to wipe the filesystem root as a project.", call. = FALSE)
  inside <- function(path) identical(path, root) || startsWith(path, paste0(sub("/$", "", root), "/"))
  inspect <- function(path) {
    resolved <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (!inside(resolved)) stop("Refusing to delete a path that resolves outside the project.", call. = FALSE)
    link <- Sys.readlink(path)
    if (!is.na(link) && nzchar(link)) return(invisible(NULL))
    info <- file.info(path)
    if (isTRUE(info$isdir)) {
      children <- list.files(path, all.files = TRUE, no.. = TRUE, full.names = TRUE)
      for (child in children) inspect(child)
    }
    invisible(NULL)
  }
  targets <- file.path(root, c(".dualdag", "output"))
  for (target in targets) if (file.exists(target) || dir.exists(target)) inspect(target)
  for (target in targets) {
    if (file.exists(target) || dir.exists(target)) unlink(target, recursive = TRUE, force = TRUE)
  }
  dir.create(file.path(root, ".dualdag", "state"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, ".dualdag", "runs"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "output"), recursive = TRUE, showWarnings = FALSE)
  invisible(TRUE)
}

out_rds_empty <- function(p, ds, id, side) {
  f <- file.path(node_run_dir(p, ds, id), side, "out.rds")
  if (!file.exists(f) || dir.exists(f)) return(FALSE)
  df <- tryCatch(readRDS(f), error = function(e) NULL)
  is.data.frame(df) && nrow(df) == 0L
}

# A Data matched node whose saved result has no rows is not a match. Drop it to none.
# Descendants keep their level and are only marked upstream_changed.
# Missing out.rds is left alone.
downgrade_empty_l3 <- function(p) {
  if (is.null(p$state$nodes) || !length(p$state$nodes)) return(invisible(p))
  changed <- FALSE
  for (ds in names(p$state$nodes)) {
    bad <- character()
    ids <- names(p$state$nodes[[ds]])
    if (is.null(ids)) next
    for (id in ids) {
      st <- get_status(p, ds, id)
      if (!identical(st$level, "L3")) next
      if (!out_rds_empty(p, ds, id, "primary") && !out_rds_empty(p, ds, id, "validation")) next
      st$level <- "none"
      st$problem <- list(stage = "run", message = "produced 0 rows")
      st$upstream_changed <- NULL
      put_status(p, ds, id, st, save = FALSE)
      bad <- c(bad, id)
      changed <- TRUE
    }
    if (length(bad)) flag_downstream_of(p, ds, bad)
  }
  if (changed) write_state(p)
  invisible(p)
}

read_project_spec <- function(p) {
  root <- project_dir_of(p)
  var <- file.path(root, "spec", "variable.csv")
  if (!file.exists(var)) return(NULL)
  par <- file.path(root, "spec", "parameter.csv")
  read_spec(var, if (file.exists(par)) par else NULL)
}

read_project_spec_dir <- function(dir) read_project_spec(dir)

reset_all_work <- function(p) {
  unlink(list.files(p$state_dir, pattern = "^(graph|param_graph|log|layout)_.*\\.json$", full.names = TRUE))
  unlink(list.files(p$state_dir, pattern = "^network_.*$", full.names = TRUE), recursive = TRUE)
  unlink(list.files(p$state_dir, pattern = "^dag_.*\\.md$", full.names = TRUE))
  unlink(file.path(p$dir, ".dualdag", "runs"), recursive = TRUE)
  dir.create(file.path(p$dir, ".dualdag", "runs"), recursive = TRUE, showWarnings = FALSE)
  p$state$nodes <- empty_named()
  p$state$row_hash <- empty_named()
  p$state$errors <- empty_named()
}

reset_dataset_work <- function(p, ds) {
  extra <- if (exists("dag_md_path", mode = "function")) dag_md_path(p, ds) else character()
  unlink(c(graph_path(p, ds), log_path(p, ds), extra))
  if (exists("write_network_json", mode = "function")) write_network_json(p, ds)
  ids <- names(p$state$nodes[[ds]])
  for (id in ids) clear_node_results(p, ds, id)
  p$state$nodes[[ds]] <- empty_named()
  write_state(p)
}

project_upload_spec <- function(p, variable_raw, parameter_raw = NULL) {
  if (!is.raw(variable_raw) || !length(variable_raw)) stop("variable.csv is empty.", call. = FALSE)
  tmp <- tempfile("dualdag-spec-")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  write_bytes(variable_raw, file.path(tmp, "variable.csv"))
  par <- NULL
  if (!is.null(parameter_raw)) {
    if (!is.raw(parameter_raw) || !length(parameter_raw)) stop("parameter.csv is empty.", call. = FALSE)
    write_bytes(parameter_raw, file.path(tmp, "parameter.csv"))
    par <- file.path(tmp, "parameter.csv")
  }
  sp <- read_spec(file.path(tmp, "variable.csv"), par)
  file.copy(file.path(tmp, "variable.csv"), file.path(p$dir, "spec", "variable.csv"), overwrite = TRUE)
  dest_p <- file.path(p$dir, "spec", "parameter.csv")
  if (is.null(par)) unlink(dest_p) else file.copy(par, dest_p, overwrite = TRUE)
  reset_all_work(p)
  p$state$files <- list(
    variable = file_md5(file.path(p$dir, "spec", "variable.csv")),
    parameter = file_md5(dest_p)
  )
  rebuild_row_hash(p, sp)
  if (nrow(sp$variables)) {
    for (i in seq_len(nrow(sp$variables))) {
      ds <- sp$variables$Dataset[i]; vr <- sp$variables$Variable[i]
      if (!nzchar(ds) || !nzchar(vr)) next
      put_status(p, ds, paste0(ds, ".", vr), blank_status(), save = FALSE)
    }
  }
  write_state(p)
  if (exists("build_source_catalog", mode = "function")) {
    tryCatch(build_source_catalog(p, force = FALSE), error = function(e) NULL)
  }
  sp
}

project_upload_data <- function(p, name, bytes) {
  name <- basename(as.character(name %||% ""))
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*\\.xpt$", name, ignore.case = TRUE))
    stop("Upload one .xpt file (letters, numbers, dot, dash, underscore).", call. = FALSE)
  if (!is.raw(bytes) || !length(bytes)) stop("Empty xpt upload.", call. = FALSE)
  low <- tolower(name)
  dest <- file.path(p$dir, "data", low)
  old <- file_md5(dest)
  write_bytes(bytes, dest)
  new <- file_md5(dest)
  if (!is.null(old) && !identical(old, new)) demote_all_generated(p)
  if (is.null(p$state$data) || !is.list(p$state$data)) p$state$data <- empty_named()
  p$state$data[[low]] <- new
  write_state(p)
  if (exists("build_source_catalog", mode = "function")) {
    tryCatch(build_source_catalog(p, force = FALSE), error = function(e) NULL)
  }
  list_data_info(p)
}

project_remove_data <- function(p, name) {
  name <- tolower(basename(as.character(name %||% "")))
  if (!grepl("^[a-z0-9][a-z0-9_.-]*\\.xpt$", name)) stop("Expected an .xpt file name.", call. = FALSE)
  hits <- list.files(file.path(p$dir, "data"), full.names = TRUE)
  hits <- hits[tolower(basename(hits)) == name]
  if (length(hits)) {
    unlink(hits)
    demote_all_generated(p)
  }
  if (!is.null(p$state$data)) p$state$data[[name]] <- NULL
  write_state(p)
  if (exists("build_source_catalog", mode = "function")) {
    tryCatch(build_source_catalog(p, force = FALSE), error = function(e) NULL)
  }
  invisible(TRUE)
}

list_data_info <- function(p) {
  dir <- file.path(project_dir_of(p), "data")
  nms <- list.files(dir)
  nms <- nms[grepl("\\.xpt$", nms, ignore.case = TRUE)]
  out <- list()
  for (nm in nms) {
    low <- tolower(nm)
    path <- file.path(dir, nm)
    if (!identical(nm, low)) {
      dest <- file.path(dir, low)
      if (!file.exists(dest)) {
        file.rename(path, dest)
        path <- dest
        nm <- low
      }
    }
    domain <- toupper(tools::file_path_sans_ext(tolower(nm)))
    info <- tryCatch({
      df <- haven::read_xpt(path)
      list(name = tolower(basename(path)), domain = domain, nrow = nrow(df), ncol = ncol(df), vars = as.list(names(df)))
    }, error = function(e) {
      list(name = tolower(basename(path)), domain = domain, nrow = NULL, ncol = NULL, vars = list(), error = conditionMessage(e))
    })
    out[[length(out) + 1L]] <- info
  }
  out
}

local_data_index <- function(project) {
  info <- list_data_info(project)
  out <- empty_named()
  for (d in info) out[[d$domain]] <- as.character(unlist(d$vars %||% list()))
  out
}

dataset_keys <- function(spec, ds) {
  if (is.null(spec) || !nrow(spec$variables)) return(character())
  v <- spec$variables
  v <- v[v$Dataset == ds & !is.na(v$Key), , drop = FALSE]
  if (!nrow(v)) return(character())
  v <- v[order(v$Key), , drop = FALSE]
  as.character(v$Variable)
}

dataset_node_ids <- function(p, spec, ds) {
  g <- load_graph(p, ds)
  if (!is.null(g) && length(g$nodes)) {
    ids <- vapply(g$nodes, function(n) if (is.list(n)) as.character(n$id %||% "") else "", "")
    ids <- ids[nzchar(ids)]
    if (length(ids)) return(ids)
  }
  if (is.null(spec) || !nrow(spec$variables)) return(character())
  v <- spec$variables
  v <- v[v$Dataset == ds & nzchar(v$Variable), , drop = FALSE]
  paste0(ds, ".", v$Variable)
}

dataset_reextract_required <- function(spec, ds, graph) {
  if (is.null(graph) || is.null(spec)) return(FALSE)
  layered_spec <- nrow(spec_params(spec,ds)) > 0L
  if(layered_spec) return(!isTRUE(graph$layered))
  keys<-dataset_keys(spec,ds)
  if(length(keys)<2L)return(FALSE)
  records_id<-paste0(toupper(ds),".RECORDS")
  !any(vapply(graph$nodes %||% list(),function(n)
    identical(as.character(n$id %||% ""),records_id)&&identical(n$kind,"records")&&is.null(n$paramcd),TRUE))
}

graph_node <- function(graph, id) {
  if (is.null(graph)) return(NULL)
  for (n in graph$nodes %||% list()) if (is.list(n) && identical(as.character(n$id %||% ""), id)) return(n)
  NULL
}

default_node <- function(id) {
  parsed <- split_node_id(id)
  if (is.null(parsed)) stop("Invalid node id.", call. = FALSE)
  list(id = paste0(parsed$dataset, ".", parsed$var, if (is.null(parsed$paramcd)) "" else paste0("@", parsed$paramcd)),
       var = parsed$var, paramcd = parsed$paramcd,
       kind = if (is.null(parsed$paramcd)) "variable" else "param_variable")
}

note_graph_nodes <- function(p, ds, graph) {
  spec <- tryCatch(read_project_spec(p), error = function(e) NULL)
  for (n in graph$nodes %||% list()) {
    id <- if (is.list(n)) as.character(n$id %||% "") else ""
    if (!nzchar(id)) next
    if (is.null(p$state$nodes[[ds]][[id]]))
      put_status(p, ds, id, blank_status(), save = FALSE)
    if (is.list(spec)) {
      fp <- node_fingerprint(spec, id)
      if (!is.null(fp)) p$state$row_hash[[id]] <- fp
    }
  }
  write_state(p)
}

issues_for_node <- function(issues, dataset, node_id) {
  parsed <- split_node_id(node_id)
  var <- if (is.null(parsed)) toupper(node_id) else parsed$var
  Filter(function(iss) {
    if (!is.list(iss)) return(FALSE)
    ds <- toupper(as.character(iss$dataset %||% ""))
    if (length(ds) != 1) ds <- ""
    ds_ok <- !nzchar(ds) || identical(ds, dataset)
    vars <- toupper(as.character(unlist(iss$variables, use.names = FALSE)))
    ds_ok && length(vars) > 0 && (node_id %in% vars || var %in% vars)
  }, if (is.null(issues)) list() else issues)
}

upstream_all_l3 <- function(p, graph, ds, id) {
  ups <- upstream_ids(graph, id)
  if (!length(ups)) return(TRUE)
  all(vapply(ups, function(u) identical(get_status(p, ds, u)$level, "L3"), TRUE))
}

# Key node ids that are not L3 yet. Not a generate gate. When none of them are
# L3, work for that side has 0 rows and the program builds the row frame itself.
missing_key_nodes <- function(p, spec, ds, id) {
  if (is.null(spec)) return(character())
  keys <- dataset_keys(spec, ds)
  if (!length(keys)) return(character())
  parsed <- split_node_id(id)
  if (!is.null(parsed) && is.null(parsed$paramcd) && parsed$var %in% keys) return(character())
  kids <- paste0(ds, ".", keys)
  kids[!vapply(kids, function(k) identical(get_status(p, ds, k)$level, "L3"), TRUE)]
}

build_upstream_df <- function(p, spec, graph, ds, id, side) {
  keys <- dataset_keys(spec, ds)
  ups <- upstream_ids(graph, id)
  acc <- NULL
  for (u in ups) {
    f <- file.path(node_run_dir(p, ds, u), side, "out.rds")
    if (!file.exists(f)) next
    df <- tryCatch(readRDS(f), error = function(e) NULL)
    if (!is.data.frame(df)) next
    if (is.null(acc)) {
      acc <- df
    } else {
      by <- intersect(keys, intersect(names(acc), names(df)))
      if (length(by)) acc <- merge(acc, df, by = by, all = TRUE)
    }
  }
  if (is.null(acc)) {
    if (!length(keys)) return(data.frame())
    acc <- as.data.frame(setNames(replicate(length(keys), character(0), simplify = FALSE), keys), stringsAsFactors = FALSE)
  }
  acc
}

read_file_raw <- function(path) {
  if (!file.exists(path)) return(raw())
  sz <- file.info(path)$size
  if (is.na(sz) || sz <= 0) raw() else readBin(path, "raw", sz)
}
