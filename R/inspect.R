# Whole-dataset code view and the per-node mismatch input table.
# Both stay local: nothing here is sent to the LLM.
#
# dataset_program() reads each side's stored code.R in topological order.
# node_inputs() names the source columns each side's code actually touches
# and lines them up with the two results, one row per key.

inspect_chr <- function(x) {
  if (is.null(x)) return(character())
  out <- as.character(unlist(x, use.names = FALSE))
  out <- out[!is.na(out) & nzchar(out)]
  unique(out)
}

inspect_project <- function(project) {
  if (is.environment(project) && is.character(project$dir) && length(project$dir) == 1L) return(project)
  if (is.character(project) && length(project) == 1L && !is.na(project) && nzchar(project)) return(open_project(project))
  stop("A project is required.", call. = FALSE)
}

inspect_unquote <- function(token, text) {
  if (identical(token, "STR_CONST")) {
    val <- tryCatch(parse(text = text, keep.source = FALSE)[[1]], error = function(e) NULL)
    if (is.null(val)) return(NULL)
    return(as.character(val))
  }
  if (identical(token, "SYMBOL")) return(text)
  NULL
}

inspect_atom <- function(pd, row) {
  tok <- pd$token[row]
  if (tok %in% c("SYMBOL", "STR_CONST")) return(inspect_unquote(tok, pd$text[row]))
  if (!identical(tok, "expr")) return(NULL)
  kids <- which(pd$parent == pd$id[row])
  if (length(kids) != 1L) return(NULL)
  inspect_atom(pd, kids)
}

inspect_token_ids <- function(pd, row) {
  tok <- pd$token[row]
  if (tok %in% c("SYMBOL", "STR_CONST")) return(pd$id[row])
  if (!identical(tok, "expr")) return(integer())
  kids <- which(pd$parent == pd$id[row])
  if (!length(kids)) return(integer())
  unique(unlist(lapply(kids, function(k) inspect_token_ids(pd, k)), use.names = FALSE))
}

# expr of the form <receiver> $ NAME  or  <receiver> [[ "NAME" ]] / [ "NAME" ].
inspect_extract_sub <- function(pd, id) {
  kids <- which(pd$parent == id)
  if (length(kids) < 3L) return(NULL)
  toks <- pd$token[kids]
  op <- which(toks %in% c("$", "'$'", "LBB", "'[['", "[[", "'['", "["))
  if (!length(op)) return(NULL)
  op <- op[1]
  if (op < 2L || op >= length(kids)) return(NULL)
  nm <- inspect_atom(pd, kids[op + 1L])
  if (is.null(nm) || !nzchar(nm)) return(NULL)
  list(recv = pd$id[kids[op - 1L]], name = nm, name_row = kids[op + 1L])
}

inspect_chain <- function(pd, id) {
  names <- character()
  cur <- id
  root <- ""
  guard <- 0L
  repeat {
    guard <- guard + 1L
    if (guard > 40L) break
    kids <- which(pd$parent == cur)
    if (length(kids) == 1L && identical(pd$token[kids], "SYMBOL")) {
      root <- pd$text[kids]
      break
    }
    sub <- inspect_extract_sub(pd, cur)
    if (is.null(sub)) break
    names <- c(sub$name, names)
    cur <- sub$recv
  }
  list(root = root, names = names)
}

inspect_canon <- function(name, choices) {
  if (!length(choices) || is.null(name) || !nzchar(name)) return(NULL)
  hit <- choices[toupper(choices) == toupper(name)]
  if (length(hit)) hit[1] else NULL
}

# Parse code.R. ok=FALSE means the caller should use graph edges instead.
# uses: kind "domain" (origin DOMAIN.VAR) or "upstream" (origin upstream.VAR).
code_input_uses <- function(code, domain_cols, keys = character(), layer_bindings = NULL,
                            dataset_bindings = NULL, source_bindings = NULL) {
  empty <- list(ok = FALSE, uses = list())
  if (is.null(code) || !nzchar(trimws(paste(code, collapse = "\n")))) return(empty)
  parsed <- tryCatch(parse(text = code, keep.source = TRUE), error = function(e) NULL)
  if (is.null(parsed)) return(empty)
  pd <- tryCatch(utils::getParseData(parsed, includeText = TRUE), error = function(e) NULL)
  if (is.null(pd) || !nrow(pd)) return(empty)
  children <- split(seq_len(nrow(pd)), as.character(pd$parent))
  child_rows <- function(id) children[[as.character(id)]] %||% integer()

  domain_cols <- domain_cols %||% list()
  layer_bindings <- layer_bindings %||% character()
  dataset_bindings <- dataset_bindings %||% character()
  source_bindings <- source_bindings %||% character()
  dom_names <- names(domain_cols)
  if (is.null(dom_names)) dom_names <- character()
  keys <- toupper(inspect_chr(keys))
  is_key <- function(var) toupper(var) %in% keys

  expr_ids <- pd$id[pd$token == "expr"]
  chains <- lapply(expr_ids, function(id) inspect_chain(pd, id))

  referenced <- character()
  for (ch in chains) {
    if (!identical(ch$root, "src") || !length(ch$names)) next
    dom <- inspect_canon(ch$names[1], dom_names)
    if (!is.null(dom)) referenced <- unique(c(referenced, dom))
  }

  # dom <- src[["DM"]] keeps a later dom$VAR / dom[["VAR"]] on DM.
  aliases <- setNames(character(), character())
  assign_rows <- which(pd$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"))
  for (row in assign_rows) {
    parent <- pd$parent[row]
    kids <- child_rows(parent)
    op_at <- match(row, kids)
    if (is.na(op_at) || op_at < 2L || op_at >= length(kids)) next
    left_kids <- child_rows(pd$id[kids[op_at - 1L]])
    if (length(left_kids) != 1L || !identical(pd$token[left_kids], "SYMBOL")) next
    ch <- inspect_chain(pd, pd$id[kids[op_at + 1L]])
    if (!identical(ch$root, "src") || length(ch$names) != 1L) next
    dom <- inspect_canon(ch$names[1], dom_names)
    if (!is.null(dom)) aliases[[pd$text[left_kids]]] <- dom
  }

  uses <- list()
  seen <- character()
  bound <- integer()
  excluded <- integer()
  output_names <- character()
  add <- function(kind, origin, domain, variable, node) {
    key <- paste(kind, origin, node %||% "", sep = "\r")
    if (key %in% seen) return()
    seen <<- c(seen, key)
    uses[[length(uses) + 1L]] <<- list(
      kind = kind, origin = origin, domain = domain, variable = variable, node = node
    )
  }

  for (i in seq_along(chains)) {
    ch <- chains[[i]]
    if (!length(ch$names)) next
    sub <- inspect_extract_sub(pd, expr_ids[i])
    name_ids <- if (is.null(sub)) integer() else inspect_token_ids(pd, sub$name_row)
    if (identical(ch$root, "result")) {
      excluded <- unique(c(excluded, name_ids))
      if (length(ch$names)) output_names <- unique(c(output_names, ch$names[length(ch$names)]))
      next
    }
    if (ch$root %in% c("upstream", "work")) {
      var <- ch$names[length(ch$names)]
      if (!is_key(var)) add("upstream", paste0(ch$root, ".", var), NULL, var, NULL)
      bound <- unique(c(bound, name_ids))
      next
    }
    if (identical(ch$root, "layers") && length(ch$names) >= 1L) {
      if(length(ch$names)>=2L) add("layer", paste0("layers$", toupper(ch$names[[1]]), "$", toupper(ch$names[[2]])),
          toupper(ch$names[[1]]), toupper(ch$names[[2]]), NULL) else
        add("layer_all",paste0("layers$",toupper(ch$names[[1]])),toupper(ch$names[[1]]),"*",NULL)
      bound <- unique(c(bound, name_ids))
      next
    }
    current_name <- if (toupper(ch$root) %in% names(dataset_bindings)) dataset_bindings[[toupper(ch$root)]] else NULL
    if (!is.null(current_name) && length(ch$names)) {
      var <- ch$names[[length(ch$names)]]
      if (!is_key(var)) add("upstream", paste0(ch$root, ".", var), NULL, var, NULL)
      bound <- unique(c(bound, name_ids))
      next
    }
    layer_pc <- if (toupper(ch$root) %in% names(layer_bindings)) layer_bindings[[toupper(ch$root)]] else NULL
    if (!is.null(layer_pc)) {
      if (length(ch$names)) add("layer", paste0("layers$", toupper(layer_pc), "$", toupper(ch$names[[length(ch$names)]])),
        toupper(layer_pc), toupper(ch$names[[length(ch$names)]]), NULL)
      bound <- unique(c(bound, name_ids))
      next
    }
    source_dom <- if (toupper(ch$root) %in% names(source_bindings)) source_bindings[[toupper(ch$root)]] else
      if (startsWith(toupper(ch$root),"LINEAGE_SRC_")) substring(toupper(ch$root),13L) else NULL
    if (!is.null(source_dom) && length(ch$names) >= 1L) {
      var <- inspect_canon(ch$names[[length(ch$names)]], domain_cols[[source_dom]] %||% character()) %||% toupper(ch$names[[length(ch$names)]])
      if (!is_key(var)) add("domain", paste0(source_dom, ".", var), source_dom, var, NULL)
      bound <- unique(c(bound, name_ids))
      next
    }
    if (identical(ch$root, "src")) {
      dom <- inspect_canon(ch$names[1], referenced)
      if (is.null(dom)) next
      if (length(ch$names) >= 2L) {
        var <- inspect_canon(ch$names[length(ch$names)], domain_cols[[dom]] %||% character())
        if (!is.null(var) && !is_key(var)) add("domain", paste0(dom, ".", var), dom, var, NULL)
        bound <- unique(c(bound, name_ids))
      }
      next
    }
    alias_dom <- if (ch$root %in% names(aliases)) aliases[[ch$root]] else NULL
    if (!is.null(alias_dom)) {
      var <- inspect_canon(ch$names[length(ch$names)], domain_cols[[alias_dom]] %||% character())
      if (!is.null(var) && !is_key(var)) add("domain", paste0(alias_dom, ".", var), alias_dom, var, NULL)
      bound <- unique(c(bound, name_ids))
    }
  }

  # A bare token that matches a column of a referenced domain, except the
  # name on the left of result$VAR / result[["VAR"]], names already bound to
  # one domain, and a string that only names the column being written into result.
  # String literals are values, not bare column references (for example the
  # "WEIGHT" level in VSTESTCD == "WEIGHT"). Bracket extraction names were
  # already handled above and marked as bound.
  term <- which(pd$token == "SYMBOL" & pd$terminal)
  for (row in term) {
    id <- pd$id[row]
    if (id %in% excluded || id %in% bound) next
    text <- inspect_unquote(pd$token[row], pd$text[row])
    if (is.null(text) || !nzchar(text) || is_key(text)) next
    if (length(output_names) && toupper(text) %in% toupper(output_names)) next
    for (dom in referenced) {
      var <- inspect_canon(text, domain_cols[[dom]] %||% character())
      if (is.null(var)) next
      add("domain", paste0(dom, ".", var), dom, var, NULL)
    }
  }
  resolve_domain <- function(expr_id) {
    ch <- inspect_chain(pd, expr_id)
    if (identical(ch$root, "src") && length(ch$names) == 1L)
      return(inspect_canon(ch$names[[1L]], referenced))
    if (!length(ch$names) && ch$root %in% names(aliases)) return(unname(aliases[[ch$root]]))
    NULL
  }
  add_string_columns <- function(token_rows, domain) {
    if (is.null(domain) || !length(token_rows)) return(invisible())
    for (row in token_rows) {
      name <- inspect_unquote(pd$token[[row]], pd$text[[row]])
      var <- inspect_canon(name, domain_cols[[domain]] %||% character())
      if (!is.null(var) && !is_key(var))
        add("domain", paste0(domain, ".", var), domain, var, NULL)
    }
    invisible()
  }
  selector_string_rows <- function(expr_row, allowed_calls) {
    if (identical(pd$token[[expr_row]], "STR_CONST") && isTRUE(pd$terminal[[expr_row]])) return(expr_row)
    if (!identical(pd$token[[expr_row]], "expr")) return(integer())
    expr_id <- pd$id[[expr_row]]
    kids <- child_rows(expr_id)
    if (length(kids) == 1L) {
      if (identical(pd$token[[kids]], "STR_CONST") && isTRUE(pd$terminal[[kids]])) return(kids[[1L]])
      if (identical(pd$token[[kids]], "expr")) return(selector_string_rows(kids[[1L]], allowed_calls))
    }
    todo <- expr_id
    fn_rows <- integer()
    while (length(todo)) {
      cur <- todo[[1L]]; todo <- todo[-1L]
      child <- child_rows(cur)
      if (length(child)) {
        calls <- child[pd$token[child] == "SYMBOL_FUNCTION_CALL"]
        fn_rows <- c(fn_rows, calls)
        todo <- c(todo, pd$id[child])
      }
    }
    if (!length(fn_rows)) return(integer())
    fn_row <- fn_rows[which.min(pd$id[fn_rows])]
    fn_name <- tolower(pd$text[[fn_row]])
    if (!fn_name %in% allowed_calls) return(integer())
    fn_expr <- pd$parent[[fn_row]]
    call_id <- pd$parent[match(fn_expr, pd$id)]
    args <- child_rows(call_id)
    args <- args[pd$token[args] == "expr" & pd$id[args] != fn_expr]
    unique(unlist(lapply(args, selector_string_rows, allowed_calls = allowed_calls), use.names = FALSE))
  }

  # In x[i, j], only j is a column selector; in x["A"], the single
  # argument selects a column. Resolve x through src or any parsed alias.
  bracket_rows <- which(pd$token %in% c("[", "'['"))
  for (row in bracket_rows) {
    sub_id <- pd$parent[[row]]
    kids <- child_rows(sub_id)
    open_at <- match(row, kids)
    if (is.na(open_at) || open_at < 2L) next
    receiver <- kids[open_at - 1L]
    receiver_expr <- if (identical(pd$token[[receiver]], "expr")) pd$id[[receiver]] else NA_integer_
    if (is.na(receiver_expr)) next
    dom <- resolve_domain(receiver_expr)
    if (is.null(dom)) next
    end_at <- which(seq_along(kids) > open_at & pd$token[kids] %in% c("]", "']'"))
    if (!length(end_at)) next
    end_at <- end_at[[1L]]
    inside <- if (open_at + 1L < end_at) seq.int(open_at + 1L, end_at - 1L) else integer()
    comma_at <- inside[pd$token[kids[inside]] %in% c(",", "','")]
    if (length(comma_at)) {
      start <- comma_at[[1L]] + 1L
      next_comma <- comma_at[comma_at > comma_at[[1L]]]
      finish <- if (length(next_comma)) next_comma[[1L]] - 1L else end_at - 1L
      slot <- if (start <= finish) kids[seq.int(start, finish)] else integer()
    } else {
      slot <- kids[inside]
    }
    slot <- slot[pd$token[slot] == "expr"]
    token_rows <- unique(unlist(lapply(slot, selector_string_rows, allowed_calls = "c"), use.names = FALSE))
    add_string_columns(token_rows, dom)
  }

  # A select call gets its data from the first argument, or from the left
  # operand of |> / %>% when used as a pipe stage. String constants in direct
  # selector arguments and nested c()/all_of()/any_of() calls name
  # columns of that resolved domain only.
  select_rows <- which(pd$token == "SYMBOL_FUNCTION_CALL" & tolower(pd$text) == "select")
  for (row in select_rows) {
    fn_expr <- pd$parent[[row]]
    call_id <- pd$parent[match(fn_expr, pd$id)]
    if (is.na(call_id) || !call_id %in% pd$id) next
    args <- child_rows(call_id)
    args <- args[pd$token[args] == "expr" & pd$id[args] != fn_expr]
    dom <- NULL
    selector_args <- args
    call_parent <- pd$parent[match(call_id, pd$id)]
    if (!is.na(call_parent) && call_parent != 0L) {
      siblings <- child_rows(call_parent)
      pipe_at <- siblings[pd$token[siblings] %in% c("PIPE", "SPECIAL") &
        pd$text[siblings] %in% c("|>", "%>%")]
      if (length(pipe_at)) {
        before <- siblings[pd$id[siblings] < pd$id[pipe_at[[1L]]] & pd$token[siblings] == "expr"]
        if (length(before)) dom <- resolve_domain(pd$id[before[[length(before)]]])
      }
    }
    if (is.null(dom) && length(args)) {
      dom <- resolve_domain(pd$id[args[[1L]]])
      selector_args <- args[-1L]
    }
    if (is.null(dom) || !length(selector_args)) next
    descendants <- unlist(lapply(selector_args, selector_string_rows,
      allowed_calls = c("c", "all_of", "any_of")), use.names = FALSE)
    add_string_columns(unique(descendants), dom)
  }
  list(ok = TRUE, uses = uses)
}

# Domain.column pairs named in Source / Derivation text.
inspect_dot_refs <- function(text) {
  text <- toupper(paste(as.character(text %||% ""), collapse = "\n"))
  if (!nzchar(text)) return(character())
  m <- gregexpr("\\b([A-Z][A-Z0-9]*)\\.([A-Z][A-Z0-9]*)\\b", text, perl = TRUE)
  hit <- regmatches(text, m)[[1]]
  if (!length(hit)) character() else unique(hit)
}

inspect_note_blob <- function(notes) {
  if (is.null(notes)) return("")
  if (is.character(notes)) return(toupper(paste(notes, collapse = "\n")))
  parts <- character()
  if (is.list(notes)) {
    for (n in notes) {
      tx <- if (is.list(n)) n$text else n
      tx <- as.character(tx %||% "")[1]
      if (is.na(tx)) tx <- ""
      parts <- c(parts, tx)
    }
  }
  toupper(paste(parts, collapse = "\n"))
}

inspect_note_mentions <- function(blob, token) {
  token <- toupper(as.character(token %||% "")[1])
  if (!nzchar(blob) || !nzchar(token)) return(FALSE)
  pat <- paste0("\\b", gsub("([.|])", "\\\\\\1", token, perl = TRUE), "\\b")
  grepl(pat, blob, perl = TRUE)
}

# Warning, not a failure. Domain columns the primary code reads must be named
# in this variable's Source/Derivation. Keys are always allowed. work/upstream
# columns are allowed when the spec names that variable. A note that mentions
# the same source suppresses that one warning.
primary_spec_path_warnings <- function(code, spec, dataset, node, domain_cols = NULL, notes = NULL) {
  id <- if (is.list(node)) as.character(node$id %||% "")[1] else as.character(node %||% "")[1]
  parsed <- tryCatch(split_node_id(id), error = function(e) NULL)
  if (is.null(parsed)) return(character())
  dataset <- toupper(as.character(dataset %||% parsed$dataset)[1])
  bits <- character()
  keys <- character()
  if (is.list(spec) && is.data.frame(spec$variables) && nrow(spec$variables)) {
    v <- spec$variables
    ds_rows <- toupper(as.character(v$Dataset)) == dataset
    if ("Key" %in% names(v)) {
      k <- suppressWarnings(as.integer(v$Key))
      keys <- toupper(as.character(v$Variable[!is.na(k) & ds_rows]))
    }
    hit <- ds_rows & toupper(as.character(v$Variable)) == toupper(parsed$var)
    take <- function(x) {
      x <- as.character(x)
      x[is.na(x)] <- ""
      x
    }
    if (any(hit)) bits <- c(bits, take(v$Source[hit]), take(v$Derivation[hit]))
    if (!is.null(parsed$paramcd) && is.data.frame(spec$parameters) && nrow(spec$parameters)) {
      a <- spec$parameters
      ph <- toupper(as.character(a$Dataset)) == dataset & toupper(as.character(a$PARAMCD)) == toupper(parsed$paramcd)
      if (any(ph)) bits <- c(bits, take(a$Source[ph]), take(a$Derivation[ph]))
    }
  }
  blob <- toupper(paste(bits, collapse = "\n"))
  allowed_domain <- inspect_dot_refs(blob)
  uses <- code_input_uses(code, domain_cols %||% list(), keys)
  if (!isTRUE(uses$ok)) return(character())
  note_blob <- inspect_note_blob(notes)
  mentioned <- function(name) {
    name <- toupper(name)
    nzchar(name) && grepl(paste0("\\b", name, "\\b"), blob, perl = TRUE)
  }
  warned <- character()
  for (u in uses$uses) {
    origin <- as.character(u$origin %||% "")[1]
    if (is.na(origin) || !nzchar(origin)) next
    if (identical(u$kind, "domain")) {
      if (toupper(origin) %in% allowed_domain) next
      if (inspect_note_mentions(note_blob, origin)) next
      warned <- c(warned, paste0(
        "Primary uses ", origin, ", which the spec does not name for this variable"
      ))
    } else if (identical(u$kind, "upstream")) {
      vv <- toupper(as.character(u$variable %||% "")[1])
      if (vv %in% keys || mentioned(vv)) next
      if (inspect_note_mentions(note_blob, origin) || inspect_note_mentions(note_blob, vv)) next
      warned <- c(warned, paste0(
        "Primary uses ", origin, ", which the spec does not name for this variable"
      ))
    }
  }
  unique(warned)
}

# Local id list. node_ids_of() needs flat_chr() from llm.R, which this file
# must not require: tests source inspect without the LLM module.
inspect_node_ids <- function(graph) {
  if (is.null(graph)) return(character())
  graph <- normalize_graph(graph)
  ids <- character()
  for (n in graph$nodes %||% list()) {
    if (!is.list(n)) next
    id <- as.character(n$id %||% "")[1]
    if (length(id) == 1L && !is.na(id) && nzchar(id)) ids <- c(ids, id)
  }
  unique(ids)
}

inspect_topo <- function(graph) {
  if (is.null(graph)) return(character())
  graph <- normalize_graph(graph)
  ids <- inspect_node_ids(graph)
  if (!length(ids)) return(character())
  incoming <- setNames(rep(0L, length(ids)), ids)
  outs <- setNames(vector("list", length(ids)), ids)
  seen_edge <- character()
  for (e in graph$edges) {
    from <- as.character(e$from %||% "")[1]
    to <- as.character(e$to %||% "")[1]
    if (!nzchar(from) || !nzchar(to) || identical(from, to)) next
    if (!(from %in% ids) || !(to %in% ids)) next
    key <- paste(from, to, sep = "\t")
    if (key %in% seen_edge) next
    seen_edge <- c(seen_edge, key)
    outs[[from]] <- c(outs[[from]], to)
    incoming[[to]] <- incoming[[to]] + 1L
  }
  ready <- ids[incoming[ids] == 0L]
  out <- character()
  while (length(ready)) {
    n <- ready[1]
    ready <- ready[-1]
    out <- c(out, n)
    for (m in outs[[n]] %||% character()) {
      incoming[[m]] <- incoming[[m]] - 1L
      if (incoming[[m]] == 0L) ready <- c(ready, m)
    }
  }
  c(out, ids[!ids %in% out])
}

inspect_disk_nodes <- function(p, ds) {
  root <- file.path(project_dir_of(p), ".dualdag", "runs", ds)
  if (!dir.exists(root)) return(character())
  folders <- list.dirs(root, recursive = FALSE, full.names = FALSE)
  ids <- character()
  for (folder in folders) {
    id <- paste0(ds, ".", folder)
    if (!is.null(split_node_id(id))) ids <- c(ids, id)
  }
  unique(ids)
}

inspect_side_code <- function(p, ds, id, side) {
  path <- file.path(node_run_dir(p, ds, id), side, "code.R")
  txt <- read_text(path)
  if (is.null(txt) || !nzchar(trimws(txt))) return(NULL)
  txt
}

dataset_program <- function(project, dataset) {
  p <- inspect_project(project)
  ds <- safe_ds(dataset)
  graph <- tryCatch(load_graph(p, ds), error = function(e) NULL)
  order <- inspect_topo(graph)
  order <- unique(c(order, sort(setdiff(inspect_disk_nodes(p, ds), order))))
  one_side <- function(side) {
    nodes <- list()
    for (id in order) {
      code <- inspect_side_code(p, ds, id, side)
      if (is.null(code)) next
      parsed <- split_node_id(id)
      level <- "none"
      st <- tryCatch(get_status(p, ds, id), error = function(e) NULL)
      if (is.list(st) && nzchar(st$level %||% "")) level <- as.character(st$level)[1]
      nodes[[length(nodes) + 1L]] <- list(
        id = id,
        var = if (is.null(parsed)) id else parsed$var,
        status = level,
        code = code
      )
    }
    parts <- vapply(nodes, function(n) {
      visible_status <- c(none = "Not generated", L2 = "Generated", L3 = "Data matched")[[n$status]] %||% "Not generated"
      paste0("# ==== ", n$id, " (", visible_status, ") ====\n", sub("\\s+$", "", n$code), "\n")
    }, character(1))
    script <- if (length(parts)) paste(parts, collapse = "\n") else ""
    list(nodes = nodes, script = script)
  }
  primary <- one_side("primary")
  validation <- one_side("validation")
  shown <- character()
  for (bucket in list(primary$nodes, validation$nodes)) {
    for (n in bucket) if (!n$id %in% shown) shown <- c(shown, n$id)
  }
  shown <- intersect(order, shown)
  list(
    dataset = ds,
    order = as.list(shown),
    primary = primary,
    validation = validation
  )
}

inspect_domain_cols <- function(p) {
  tryCatch(data_index(p), error = function(e) list())
}

inspect_domain_path <- function(p, domain) {
  dir <- file.path(project_dir_of(p), "data")
  if (!dir.exists(dir)) return(NULL)
  files <- list.files(dir, pattern = "\\.[Xx][Pp][Tt]$", full.names = TRUE)
  dom <- toupper(domain)
  for (f in files) {
    if (identical(toupper(tools::file_path_sans_ext(basename(f))), dom)) return(f)
  }
  NULL
}

inspect_read_frame <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  tryCatch(viewer_prepare(viewer_cached_frame(path)), error = function(e) NULL)
}

inspect_sig <- function(df, keys) {
  parts <- lapply(keys, function(k) {
    x <- df[[k]]
    txt <- as.character(x)
    txt[is.na(x)] <- "\001"
    txt
  })
  do.call(paste, c(parts, list(sep = "\r")))
}

# First file-order value per key, plus how many source rows that key had.
inspect_collapse <- function(df, keys, var) {
  if (is.null(df) || !nrow(df) || !all(keys %in% names(df))) return(NULL)
  col <- inspect_canon(var, names(df))
  if (is.null(col)) return(NULL)
  sig <- inspect_sig(df, keys)
  first <- !duplicated(sig)
  n_all <- as.integer(ave(rep(1L, length(sig)), sig, FUN = sum))
  vals <- viewer_display(df[[col]])
  data.frame(sig = sig[first], value = vals[first], n = n_all[first], stringsAsFactors = FALSE)
}

inspect_lookup_node <- function(graph, node_id, var) {
  if (is.null(graph)) return(NULL)
  ups <- upstream_map(graph)[[node_id]] %||% character()
  for (u in ups) {
    parsed <- split_node_id(u)
    if (!is.null(parsed) && identical(toupper(parsed$var), toupper(var))) return(u)
    if (identical(toupper(node_var_folder(u)), toupper(var))) return(u)
  }
  for (n in graph$nodes %||% list()) {
    if (!is.list(n)) next
    id <- as.character(n$id %||% "")
    if (!nzchar(id)) next
    pv <- toupper(as.character(n$var %||% ""))
    if (identical(pv, toupper(var))) return(id)
  }
  NULL
}

inspect_flag <- function(x) {
  if (is.logical(x)) return(isTRUE(x))
  if (is.numeric(x)) return(length(x) == 1L && !is.na(x) && x == 1)
  x <- tolower(trimws(as.character(x %||% "")[1]))
  x %in% c("1", "true", "yes", "y")
}

inspect_page_args <- function(page, size) {
  page <- suppressWarnings(as.integer(page %||% 1L)[1])
  raw_size <- size
  if (is.null(raw_size) || (is.character(raw_size) && !nzchar(raw_size[1]))) raw_size <- 100L
  size <- suppressWarnings(as.integer(raw_size)[1])
  if (!length(page) || is.na(page) || page < 1L) page <- 1L
  if (!length(size) || is.na(size) || size < 1L) size <- 100L
  if (size > 1000L) size <- 1000L
  list(page = page, size = size)
}

inspect_graph_uses <- function(graph, node_id) {
  uses <- list()
  if (is.null(graph)) return(uses)
  ups <- upstream_map(graph)[[node_id]] %||% character()
  for (u in ups) {
    folder <- tryCatch(node_var_folder(u), error = function(e) NULL)
    parsed <- split_node_id(u)
    if (is.null(folder) || is.null(parsed)) next
    uses[[length(uses) + 1L]] <- list(
      kind = "upstream", origin = paste0("upstream.", folder),
      domain = NULL, variable = parsed$var, node = u
    )
  }
  for (ext in graph$external %||% list()) {
    if (!is.list(ext)) next
    used <- toupper(inspect_chr(ext$used_by))
    if (length(used) && !toupper(node_id) %in% used) next
    if (!length(used)) next
    dom <- toupper(as.character(ext$domain %||% "")[1])
    var <- toupper(as.character(ext$var %||% "")[1])
    if (!nzchar(dom) || !nzchar(var)) next
    uses[[length(uses) + 1L]] <- list(
      kind = "domain", origin = paste0(dom, ".", var),
      domain = dom, variable = var, node = NULL
    )
  }
  uses
}

inspect_side_uses <- function(code, domain_cols, keys, graph, node_id) {
  parsed <- code_input_uses(code, domain_cols, keys)
  edges <- inspect_graph_uses(graph, node_id)
  edge_up <- Filter(function(u) identical(u$kind, "upstream"), edges)
  if (!isTRUE(parsed$ok)) {
    return(list(fallback = TRUE, uses = edges))
  }
  uses <- parsed$uses
  have <- vapply(uses, function(u) u$origin %||% "", "")
  for (u in edge_up) if (!u$origin %in% have) uses[[length(uses) + 1L]] <- u
  # A code reference upstream.VAR with no node id is tied to the edge when one matches.
  for (i in seq_along(uses)) {
    u <- uses[[i]]
    if (!identical(u$kind, "upstream") || !is.null(u$node)) next
    hit <- Filter(function(e) identical(toupper(e$variable %||% ""), toupper(u$variable %||% "")), edge_up)
    if (length(hit) == 1L) uses[[i]] <- hit[[1]]
  }
  list(fallback = FALSE, uses = uses)
}

inspect_result_col <- function(df, keys, var) {
  if (is.null(df) || !ncol(df)) return(NULL)
  hit <- inspect_canon(var, names(df))
  if (!is.null(hit)) return(hit)
  rest <- setdiff(names(df), keys)
  if (length(rest)) rest[1] else NULL
}

inspect_cell <- function(value, n, multi) {
  if (is.null(n) || length(n) != 1L || is.na(n) || n <= 0L) return(NULL)
  missing <- is.null(value) || length(value) != 1L || is.na(value)
  if (isTRUE(multi)) {
    shown <- if (missing) "NA" else as.character(value)
    return(paste0(shown, " (n=", n, ")"))
  }
  if (missing) return(NULL)
  as.character(value)
}

inspect_plain <- function(value, n) {
  if (is.null(n) || length(n) != 1L || is.na(n) || n <= 0L) return(NA_character_)
  if (is.null(value) || length(value) != 1L || is.na(value)) return(NA_character_)
  as.character(value)
}

inspect_values_differ <- function(a, b, blank_equals_na = FALSE) {
  if (isTRUE(blank_equals_na) && is.character(a) && is.character(b)) {
    if (!is.na(a) && !nzchar(trimws(a))) a <- NA_character_
    if (!is.na(b) && !nzchar(trimws(b))) b <- NA_character_
  }
  a_na <- is.na(a)
  b_na <- is.na(b)
  if (a_na && b_na) return(FALSE)
  if (a_na || b_na) return(TRUE)
  !identical(a, b)
}

# `cells[[n]] <- NULL` deletes the slot instead of storing a JSON null, so later
# cells — including the diff flag — slide left into the wrong column.
inspect_push <- function(cells, value) {
  cells[length(cells) + 1L] <- list(value)
  cells
}

node_inputs <- function(project, dataset, node, page = 1L, size = 100L, only_diff = FALSE, filter = NULL) {
  p <- inspect_project(project)
  ds <- safe_ds(dataset)
  id <- canonical_node_id(ds, node)
  graph <- tryCatch(load_graph(p, ds), error = function(e) NULL)
  known <- FALSE
  if (!is.null(graph)) known <- id %in% inspect_node_ids(graph)
  if (!known && !dir.exists(node_run_dir(p, ds, id))) {
    viewer_stop("Unknown node.", 404L)
  }
  spec <- tryCatch(read_project_spec(p), error = function(e) NULL)
  keys <- if (is.null(spec)) character() else dataset_keys(spec, ds)
  if (!length(keys)) viewer_stop("This dataset has no Key.", 400L)
  parsed_id <- split_node_id(id)
  var <- parsed_id$var
  pg <- inspect_page_args(page, size)
  only <- inspect_flag(only_diff)
  domain_cols <- inspect_domain_cols(p)
  if (is.null(domain_cols)) domain_cols <- list()

  side_info <- list()
  for (side in c("primary", "validation")) {
    code <- inspect_side_code(p, ds, id, side)
    side_info[[side]] <- inspect_side_uses(code, domain_cols, keys, graph, id)
  }

  domain_acc <- list()
  up_acc <- list()
  remember_domain <- function(u, side) {
    origin <- u$origin
    cur <- domain_acc[[origin]]
    if (is.null(cur)) {
      domain_acc[[origin]] <<- list(
        origin = origin, domain = u$domain, variable = u$variable, sides = side
      )
    } else if (!grepl(side, cur$sides, fixed = TRUE)) {
      cur$sides <- paste(sort(unique(c(strsplit(cur$sides, "+", fixed = TRUE)[[1]], side))), collapse = "+")
      domain_acc[[origin]] <<- cur
    }
  }
  remember_up <- function(u, side) {
    origin <- u$origin
    key <- paste(side, origin, sep = "\r")
    if (!is.null(up_acc[[key]])) return()
    up_acc[[key]] <<- list(
      origin = origin, variable = u$variable, node = u$node, side = side
    )
  }
  for (side in c("primary", "validation")) {
    for (u in side_info[[side]]$uses) {
      if (identical(u$kind, "domain")) {
        if (toupper(u$variable %||% "") %in% toupper(keys)) next
        remember_domain(u, side)
      } else if (identical(u$kind, "upstream")) {
        if (toupper(u$variable %||% "") %in% toupper(keys)) next
        remember_up(u, side)
      }
    }
  }

  sides_of <- function(text) {
    parts <- strsplit(text %||% "", "+", fixed = TRUE)[[1]]
    if (all(c("primary", "validation") %in% parts)) "both" else parts[1]
  }
  domain_cols_out <- lapply(domain_acc, function(u) {
    lab <- sides_of(u$sides)
    list(
      name = u$origin, label = u$origin, role = "input",
      group = if (identical(lab, "both")) "both" else lab,
      sides = lab, origin = u$origin, domain = u$domain, variable = u$variable,
      kind = "domain"
    )
  })
  domain_cols_out <- unname(domain_cols_out)
  up_cols_out <- lapply(up_acc, function(u) {
    list(
      name = u$origin, label = u$origin, role = "input",
      group = u$side, sides = u$side, origin = u$origin,
      domain = NULL, variable = u$variable, node = u$node, side = u$side,
      kind = "upstream"
    )
  })
  up_cols_out <- unname(up_cols_out)
  # Same upstream origin on both sides needs a unique column name.
  up_origins <- vapply(up_cols_out, function(c) c$origin, "")
  for (i in seq_along(up_cols_out)) {
    if (sum(up_origins == up_cols_out[[i]]$origin) > 1L) {
      up_cols_out[[i]]$name <- paste0(up_cols_out[[i]]$side, "_", up_cols_out[[i]]$origin)
    }
  }

  take_group <- function(items, group) {
    hit <- Filter(function(c) identical(c$group, group), items)
    if (!length(hit)) return(list())
    hit[order(vapply(hit, function(c) c$origin %||% c$name, ""))]
  }
  input_cols <- c(
    take_group(domain_cols_out, "primary"),
    take_group(up_cols_out, "primary"),
    take_group(domain_cols_out, "both"),
    take_group(domain_cols_out, "validation"),
    take_group(up_cols_out, "validation")
  )

  # Values. Domain columns come from the xpt; upstream columns from that side's out.rds.
  frames <- list()
  frame_of_domain <- function(dom) {
    if (!is.null(frames[[dom]])) return(frames[[dom]])
    path <- inspect_domain_path(p, dom)
    frames[[dom]] <<- inspect_read_frame(path)
    frames[[dom]]
  }
  collapsed <- list()
  multi_of <- list()
  aligned_of <- list()
  for (col in input_cols) {
    if (!identical(col$kind, "domain")) next
    df <- frame_of_domain(col$domain)
    aligned <- !is.null(df) && all(keys %in% names(df))
    aligned_of[[col$name]] <- aligned
    if (!aligned) {
      collapsed[[col$name]] <- NULL
      multi_of[[col$name]] <- FALSE
      next
    }
    got <- inspect_collapse(df, keys, col$variable)
    collapsed[[col$name]] <- got
    multi_of[[col$name]] <- !is.null(got) && any(got$n > 1L)
  }
  for (col in input_cols) {
    if (!identical(col$kind, "upstream")) next
    node <- col$node
    if (is.null(node)) node <- inspect_lookup_node(graph, id, col$variable)
    path <- if (is.null(node)) NULL else file.path(node_run_dir(p, ds, node), col$side, "out.rds")
    df <- inspect_read_frame(path)
    aligned <- !is.null(df) && all(keys %in% names(df))
    aligned_of[[col$name]] <- aligned
    if (!aligned) {
      collapsed[[col$name]] <- NULL
      multi_of[[col$name]] <- FALSE
      next
    }
    got <- inspect_collapse(df, keys, col$variable)
    collapsed[[col$name]] <- got
    multi_of[[col$name]] <- !is.null(got) && any(got$n > 1L)
  }

  result_info <- list()
  for (side in c("primary", "validation")) {
    path <- file.path(node_run_dir(p, ds, id), side, "out.rds")
    df <- inspect_read_frame(path)
    info <- list(plain = NULL, multi = FALSE, aligned = FALSE, note = "No result", character = FALSE)
    if (is.null(df)) {
      result_info[[side]] <- info
      next
    }
    # A stored frame with the right columns but no rows is empty, not a boolean.
    if (!nrow(df)) {
      info$note <- "Empty result"
      result_info[[side]] <- info
      next
    }
    colname <- inspect_result_col(df, keys, var)
    ok <- !is.null(colname) && all(keys %in% names(df))
    if (!ok) {
      info$note <- "Cannot align"
      result_info[[side]] <- info
      next
    }
    got <- inspect_collapse(df, keys, colname)
    info$plain <- got
    info$character <- is.character(df[[colname]])
    info$multi <- !is.null(got) && any(got$n > 1L)
    info$aligned <- TRUE
    info$note <- if (isTRUE(info$multi)) "Multiple rows" else NULL
    result_info[[side]] <- info
  }

  # Key universe: results first, then every aligned input, so a subject that
  # only exists on one side is still visible.
  acc <- NULL
  take_keys <- function(df) {
    if (is.null(df) || !all(keys %in% names(df)) || !nrow(df)) return()
    sig <- inspect_sig(df, keys)
    keep <- !duplicated(sig)
    piece <- data.frame(df[keep, keys, drop = FALSE], stringsAsFactors = FALSE)
    piece$.sig <- sig[keep]
    if (is.null(acc)) acc <<- piece else {
      extra <- piece[!piece$.sig %in% acc$.sig, , drop = FALSE]
      if (nrow(extra)) acc <<- rbind(acc, extra)
    }
  }
  for (side in c("primary", "validation")) {
    path <- file.path(node_run_dir(p, ds, id), side, "out.rds")
    take_keys(inspect_read_frame(path))
  }
  for (col in input_cols) {
    if (!isTRUE(aligned_of[[col$name]])) next
    src <- if (identical(col$kind, "domain")) frame_of_domain(col$domain) else {
      node <- col$node %||% inspect_lookup_node(graph, id, col$variable)
      path <- if (is.null(node)) NULL else file.path(node_run_dir(p, ds, node), col$side, "out.rds")
      inspect_read_frame(path)
    }
    take_keys(src)
  }
  if (is.null(acc)) {
    acc <- data.frame(.sig = character(), stringsAsFactors = FALSE)
    for (k in keys) acc[[k]] <- character()
  } else {
    acc <- acc[do.call(order, acc[, keys, drop = FALSE]), , drop = FALSE]
    rownames(acc) <- NULL
  }

  value_at <- function(got, sig) {
    if (is.null(got) || !nrow(got)) return(list(value = NA_character_, n = 0L))
    hit <- match(sig, got$sig)
    if (is.na(hit)) return(list(value = NA_character_, n = 0L))
    list(value = got$value[hit], n = got$n[hit])
  }

  finish_col <- function(col, multi, aligned) {
    note <- if (isTRUE(multi)) "Multiple rows" else NULL
    list(
      name = col$name, label = col$label %||% col$name, role = col$role,
      group = col$group, sides = col$sides, origin = col$origin,
      domain = col$domain, variable = col$variable,
      multi = isTRUE(multi), note = note, aligned = if (is.null(aligned)) TRUE else isTRUE(aligned)
    )
  }
  columns <- list()
  for (k in keys) {
    columns[[length(columns) + 1L]] <- list(
      name = k, label = k, role = "key", group = "key", sides = "both",
      origin = NULL, domain = NULL, variable = NULL, multi = FALSE, note = NULL, aligned = TRUE
    )
  }
  for (col in input_cols) {
    columns[[length(columns) + 1L]] <- finish_col(col, multi_of[[col$name]], aligned_of[[col$name]])
  }
  pres_name <- paste0("primary_", var)
  vres_name <- paste0("validation_", var)
  add_result <- function(side, name) {
    info <- result_info[[side]]
    list(
      name = name, label = name, role = "result", group = "result", sides = side,
      origin = NULL, domain = NULL, variable = var, multi = isTRUE(info$multi),
      note = info$note, aligned = isTRUE(info$aligned)
    )
  }
  columns[[length(columns) + 1L]] <- add_result("primary", pres_name)
  columns[[length(columns) + 1L]] <- add_result("validation", vres_name)
  columns[[length(columns) + 1L]] <- list(
    name = "diff", label = "diff", role = "diff", group = "result", sides = "both",
    origin = NULL, domain = NULL, variable = NULL, multi = FALSE, note = NULL, aligned = TRUE
  )

  rows <- list()
  diffs <- logical()
  if (nrow(acc)) {
    for (i in seq_len(nrow(acc))) {
      sig <- acc$.sig[i]
      cells <- list()
      for (k in keys) {
        val <- acc[[k]][i]
        cells <- inspect_push(cells, if (is.na(val)) NULL else as.character(val))
      }
      for (col in input_cols) {
        got <- value_at(collapsed[[col$name]], sig)
        cells <- inspect_push(cells, inspect_cell(got$value, got$n, multi_of[[col$name]]))
      }
      gp <- value_at(result_info$primary$plain, sig)
      gv <- value_at(result_info$validation$plain, sig)
      plain_p <- inspect_plain(gp$value, gp$n)
      plain_v <- inspect_plain(gv$value, gv$n)
      cells <- inspect_push(cells, inspect_cell(gp$value, gp$n, result_info$primary$multi))
      cells <- inspect_push(cells, inspect_cell(gv$value, gv$n, result_info$validation$multi))
      blank_policy <- (!exists("compare_settings", mode = "function") ||
                       isTRUE(compare_settings(p)$blank_equals_na)) &&
        isTRUE(result_info$primary$character) && isTRUE(result_info$validation$character)
      differ <- inspect_values_differ(plain_p, plain_v, blank_policy)
      cells <- inspect_push(cells, differ)
      rows[[length(rows) + 1L]] <- cells
      diffs[[length(diffs) + 1L]] <- differ
    }
  }

  if (only) {
    keep <- which(diffs)
    rows <- if (length(keep)) rows[keep] else list()
  }
  needle <- tolower(trimws(as.character(filter %||% "")[1]))
  if (nzchar(needle)) {
    rows <- Filter(function(row) {
      any(vapply(row, function(cell) {
        if (is.null(cell) || !length(cell) || is.na(cell[[1]])) return(FALSE)
        grepl(needle, tolower(as.character(cell[[1]])), fixed = TRUE)
      }, logical(1)))
    }, rows)
  }
  nrow_all <- length(rows)
  start <- (pg$page - 1L) * pg$size + 1L
  page_rows <- if (!nrow_all || start > nrow_all) list() else {
    end <- min(nrow_all, start + pg$size - 1L)
    rows[seq.int(start, end)]
  }

  list(
    dataset = ds,
    node = id,
    var = var,
    keys = as.list(keys),
    page = pg$page,
    size = pg$size,
    nrow = nrow_all,
    only_diff = only,
    fallback = list(
      primary = isTRUE(side_info$primary$fallback),
      validation = isTRUE(side_info$validation$fallback)
    ),
    columns = columns,
    rows = page_rows
  )
}
