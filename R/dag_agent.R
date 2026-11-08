# Readable dag_<DS>.md next to the graph, on-demand node lookup, and the DAG agent.
# graph_<DS>.json stays the source of truth. Lookups return spec + graph only.

dag_chr <- function(x) {
  if (is.null(x)) return(character())
  if (exists("flat_chr", mode = "function")) return(flat_chr(x))
  x <- unlist(x, use.names = FALSE)
  x <- as.character(x)
  x[!is.na(x)]
}

dag_one_line <- function(x) {
  x <- paste(as.character(x %||% ""), collapse = " ")
  x <- gsub("[\r\n\t]+", " ", x)
  trimws(gsub(" +", " ", x))
}

dag_md_path <- function(p, ds) {
  file.path(project_dir_of(p), ".dualdag", "state", paste0("dag_", safe_ds(ds), ".md"))
}

dag_undo_path <- function(p, ds) file.path(project_dir_of(p), ".dualdag", "state", paste0("dag_undo_", safe_ds(ds), ".json"))

dag_undo_read <- function(p, ds) {
  x <- tryCatch(read_json(dag_undo_path(p, ds)), error = function(e) NULL)
  if (!is.list(x)) list(steps = list()) else list(steps = dag_turn_list(x$steps))
}

dag_undo_write <- function(p, ds, steps) {
  write_json(list(steps = steps), dag_undo_path(p, ds))
  invisible(steps)
}

dag_undo_public <- function(p, ds) {
  steps <- dag_undo_read(p, ds)$steps
  if (!length(steps)) return(list(count = 0L, label = NULL))
  list(count = length(steps), label = steps[[length(steps)]]$label %||% "DAG edit")
}

dag_change_label <- function(changes) {
  bits <- character()
  for (ch in changes) {
    op <- tolower(as.character(ch$op %||% ""))
    if (op %in% c("add_edge", "remove_edge")) bits <- c(bits, paste0(if (op == "add_edge") "+ edge " else "- edge ", ch$from, " -> ", ch$to))
    else if (op == "set_key") bits <- c(bits, paste0("key ", ch$node %||% ch$id, " = ", ch$value))
    else if (op == "set_categories") bits <- c(bits, paste0("categories ", ch$node %||% ch$id))
    else if (op == "add_node") bits <- c(bits, paste0("+ node ", ch$id %||% ch$node))
  }
  if (!length(bits)) "DAG edit" else paste(head(bits, 4L), collapse = ", ")
}

dag_agent_path <- function(p, ds) {
  file.path(project_dir_of(p), ".dualdag", "experts", paste0(safe_ds(ds), "_dag.json"))
}

dag_turn_list <- function(x) {
  if (is.null(x)) return(list())
  if (is.data.frame(x)) {
    if (!nrow(x)) return(list())
    return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  }
  if (!is.list(x)) return(list())
  if (!is.null(x$role) || !is.null(x$text) || (!is.null(x$at) && !is.null(x$op))) return(list(x))
  if (!is.null(x$at) && (is.null(x[[1]]) || !is.list(x[[1]]))) return(list(x))
  out <- list()
  for (e in x) if (is.list(e)) out[[length(out) + 1L]] <- e
  out
}

dag_agent_read <- function(p, ds) {
  raw <- NULL
  path <- dag_agent_path(p, ds)
  if (exists("read_json", mode = "function")) raw <- tryCatch(read_json(path), error = function(e) NULL)
  if (is.null(raw) || !is.list(raw)) return(list(turns = list(), corrections = list()))
  list(turns = dag_turn_list(raw$turns), corrections = dag_turn_list(raw$corrections),
       param_turns = dag_turn_list(raw$param_turns), param_corrections = dag_turn_list(raw$param_corrections),
       param_edits = dag_turn_list(raw$param_edits))
}

dag_agent_write <- function(p, ds, mem) {
  dir.create(dirname(dag_agent_path(p, ds)), recursive = TRUE, showWarnings = FALSE)
  write_json(list(
    turns = mem$turns %||% list(),
    corrections = mem$corrections %||% list(),
    param_turns = mem$param_turns %||% list(), param_corrections = mem$param_corrections %||% list(),
    param_edits = mem$param_edits %||% list()
  ), dag_agent_path(p, ds))
  invisible(mem)
}

dag_agent_public <- function(p, ds) {
  mem <- tryCatch(dag_agent_read(p, ds), error = function(e) list(turns = list(), corrections = list()))
  list(turns = mem$turns %||% list(), corrections = mem$corrections %||% list())
}

dag_agent_clear <- function(p,ds) {
  mem<-dag_agent_read(p,ds); mem$turns<-list(); mem$param_turns<-list(); dag_agent_write(p,ds,mem); invisible(mem)
}

dag_spec_fields <- function(spec, dataset, id) {
  empty <- list(label = "", source = "", derivation = "", key = NA_integer_)
  parsed <- if (exists("split_node_id", mode = "function")) split_node_id(id) else NULL
  if (is.null(parsed) || is.null(spec)) return(empty)
  label <- ""
  source <- ""
  derivation <- ""
  key <- NA_integer_
  vars <- if (exists("spec_vars", mode = "function")) spec_vars(spec, dataset) else NULL
  if (!is.null(vars) && nrow(vars)) {
    hit <- which(toupper(vars$Variable) == parsed$var)
    if (length(hit)) {
      i <- hit[[1]]
      label <- if (exists("field_txt", mode = "function")) field_txt(vars$Label[[i]]) else dag_one_line(vars$Label[[i]])
      source <- if (exists("field_txt", mode = "function")) field_txt(vars$Source[[i]]) else dag_one_line(vars$Source[[i]])
      derivation <- if (exists("field_txt", mode = "function")) field_txt(vars$Derivation[[i]]) else dag_one_line(vars$Derivation[[i]])
      k <- suppressWarnings(as.integer(vars$Key[[i]]))
      if (length(k) == 1L && !is.na(k)) key <- k
    }
  }
  if (!is.null(parsed$paramcd) && exists("spec_params", mode = "function")) {
    params <- spec_params(spec, dataset)
    if (!is.null(params) && nrow(params)) {
      hit <- which(toupper(params$PARAMCD) == parsed$paramcd)
      if (length(hit)) {
        i <- hit[[1]]
        ps <- if (exists("field_txt", mode = "function")) field_txt(params$Source[[i]]) else dag_one_line(params$Source[[i]])
        pd <- if (exists("field_txt", mode = "function")) field_txt(params$Derivation[[i]]) else dag_one_line(params$Derivation[[i]])
        if (nzchar(ps)) source <- paste(c(source, paste0("PARAM ", parsed$paramcd, ": ", ps)), collapse = " | ")
        if (nzchar(pd)) derivation <- paste(c(derivation, paste0("PARAM ", parsed$paramcd, ": ", pd)), collapse = " | ")
        if (!nzchar(label)) {
          label <- if (exists("field_txt", mode = "function")) field_txt(params$PARAM[[i]]) else dag_one_line(params$PARAM[[i]])
        }
      }
    }
  }
  source <- trimws(gsub("^\\|\\s*", "", source))
  derivation <- trimws(gsub("^\\|\\s*", "", derivation))
  list(label = label, source = source, derivation = derivation, key = key)
}

dag_node_key <- function(node, spec_key) {
  if (is.list(node) && !is.null(node$key)) {
    v <- node$key
    if (is.logical(v) && length(v) == 1L && !is.na(v)) return(isTRUE(v))
    if (is.numeric(v) && length(v) == 1L && !is.na(v)) return(v[[1]] != 0)
    s <- tolower(trimws(as.character(v %||% "")[1]))
    if (s %in% c("true", "1", "yes")) return(TRUE)
    if (s %in% c("false", "0", "no")) return(FALSE)
  }
  length(spec_key) == 1L && !is.na(spec_key)
}

dag_parent_lines <- function(graph, id) {
  out <- character()
  for (e in graph$edges %||% list()) {
    if (!is.list(e) || !identical(as.character(e$to %||% ""), id)) next
    frm <- as.character(e$from %||% "")
    if (!nzchar(frm)) next
    out <- c(out, paste0("  - parent ", frm, ": ", dag_one_line(e$evidence)))
  }
  for (e in graph$external %||% list()) {
    if (!is.list(e)) next
    used <- dag_chr(e$used_by)
    if (!(id %in% used)) next
    ref <- as.character(e$ref %||% "")
    if (!nzchar(ref)) next
    out <- c(out, paste0("  - parent ", ref, ": ", dag_one_line(e$evidence)))
  }
  out
}

format_dag_md <- function(graph, spec = NULL, corrections = list()) {
  if (exists("normalize_graph", mode = "function")) graph <- normalize_graph(graph)
  ds <- toupper(as.character(graph$dataset %||% "")[1])
  lines <- c(paste0("# DAG ", ds), "", "## Nodes", "")
  nodes <- graph$nodes %||% list()
  if (!length(nodes)) {
    lines <- c(lines, "(no nodes)", "")
  } else {
    for (n in nodes) {
      if (!is.list(n)) next
      id <- as.character(n$id %||% "")
      if (!nzchar(id)) next
      info <- dag_spec_fields(spec, ds, id)
      label <- dag_one_line(n$label)
      if (!nzchar(label)) label <- info$label
      if (!nzchar(label)) label <- dag_one_line(n$var)
      head <- paste0("- ", id, " | ", label)
      if (dag_node_key(n, info$key)) head <- paste0(head, " | key")
      parents <- dag_parent_lines(graph, id)
      lines <- c(lines, head, if (length(parents)) parents else "  - (no parents)")
    }
    lines <- c(lines, "")
  }
  lines <- c(lines, "## User corrections", "")
  wrote <- FALSE
  for (crr in corrections %||% list()) {
    if (!is.list(crr)) next
    tx <- dag_one_line(crr$text %||% crr$correction)
    if (!nzchar(tx)) next
    at <- dag_one_line(crr$at)
    lines <- c(lines, paste0("- ", if (nzchar(at)) paste0(at, ": ") else "", tx))
    wrote <- TRUE
  }
  if (!wrote) lines <- c(lines, "(none)")
  paste(c(lines, ""), collapse = "\n")
}

write_dag_md <- function(p, graph) {
  ds <- safe_ds(graph$dataset)
  spec <- tryCatch(read_project_spec(p), error = function(e) NULL)
  corr <- tryCatch(dag_agent_read(p, ds)$corrections, error = function(e) list())
  write_text(format_dag_md(graph, spec, corr), dag_md_path(p, ds))
  invisible(dag_md_path(p, ds))
}

dag_cap <- function(txt, n, note) {
  txt <- as.character(txt %||% "")
  if (length(txt) != 1L) txt <- paste(txt, collapse = "\n")
  if (nchar(txt) <= n) return(txt)
  paste0(substr(txt, 1L, n), "\n", note)
}

dag_corrections_block <- function(p, ds) {
  mem <- tryCatch(dag_agent_read(p, ds), error = function(e) list(corrections = list()))
  lines <- character()
  for (crr in mem$corrections %||% list()) {
    if (!is.list(crr)) next
    tx <- dag_one_line(crr$text %||% "")
    if (!nzchar(tx)) next
    at <- dag_one_line(crr$at)
    lines <- c(lines, paste0("- ", if (nzchar(at)) paste0(at, ": ") else "", tx))
  }
  if (!length(lines)) return("")
  paste0(
    "USER_CORRECTIONS_BEGIN\n",
    paste(lines, collapse = "\n"),
    "\nUSER_CORRECTIONS_END\n",
    "These are rules the user already taught. Keep them when you extract or edit the graph.\n"
  )
}

dag_add_correction <- function(p, ds, text, at = NULL) {
  text <- dag_one_line(text)
  if (!nzchar(text)) return(invisible(dag_agent_read(p, ds)))
  mem <- dag_agent_read(p, ds)
  mem$corrections <- c(mem$corrections, list(list(at = at %||% now_iso(), text = text)))
  dag_agent_write(p, ds, mem)
  g <- tryCatch(load_graph(p, ds), error = function(e) NULL)
  if (!is.null(g)) tryCatch(write_dag_md(p, g), error = function(e) NULL)
  invisible(mem)
}

dag_remove_correction <- function(p, ds, at) {
  ds <- safe_ds(ds)
  at <- as.character(at %||% "")[1]
  mem <- dag_agent_read(p, ds)
  mem$corrections <- Filter(function(crr) !identical(as.character(crr$at %||% ""), at), mem$corrections %||% list())
  dag_agent_write(p, ds, mem)
  g <- tryCatch(load_graph(p, ds), error = function(e) NULL)
  if (!is.null(g)) write_dag_md(p, g)
  mem
}

# ---- lookup (spec + graph only; never code, logs, notes, or results) ----

dag_lookup_rule <- function() {
  paste(
    "LOOKUP: you may reply with only {\"lookup\":[\"ADSL.TRTSDT\"]} and no other fields, using at most 10 ids.",
    "The next message then contains those nodes' parents, children, one-line evidence, and spec Source and Derivation.",
    "Lookup is specification and graph information only. It never includes either side's code, logs, notes, or results.",
    "Use at most 2 lookup rounds, then reply with the final JSON for this task.",
    sep = "\n"
  )
}

dag_known_ids <- function(graph) {
  ids <- character()
  for (n in graph$nodes %||% list()) {
    if (is.list(n)) {
      id <- as.character(n$id %||% "")
      if (nzchar(id)) ids <- c(ids, id)
    }
  }
  unique(ids)
}

dag_lookup_block <- function(spec, graph, dataset, ids, allowed_ids = NULL) {
  if (exists("normalize_graph", mode = "function") && is.list(graph)) graph <- normalize_graph(graph)
  dataset <- toupper(as.character(dataset %||% graph$dataset %||% "")[1])
  ids <- unique(dag_chr(ids))
  ids <- ids[nzchar(ids)]
  note <- ""
  if (length(ids) > 10L) {
    note <- "Lookup capped at 10 ids.\n"
    ids <- ids[seq_len(10L)]
  }
  known <- dag_known_ids(graph)
  outside <- if (is.null(allowed_ids)) character() else ids[ids %in% known & !ids %in% allowed_ids]
  ids <- ids[is.null(allowed_ids) | ids %in% allowed_ids]
  unknown <- ids[!ids %in% known]
  ids <- ids[ids %in% known]
  lines <- character()
  if (nzchar(note)) lines <- c(lines, note)
  if (length(unknown)) lines <- c(lines, paste0("Unknown ids ignored: ", paste(unknown, collapse = ", "), "."))
  if (length(outside)) lines <- c(lines, paste0("Outside this variable's lineage: ", paste(outside, collapse = ", "), "."))
  if (!length(ids)) {
    if (!length(lines)) lines <- "No known ids in this lookup."
    return(paste(lines, collapse = "\n"))
  }
  for (id in ids) {
    node <- NULL
    for (n in graph$nodes) if (is.list(n) && identical(as.character(n$id %||% ""), id)) node <- n
    info <- dag_spec_fields(spec, dataset, id)
    label <- if (is.list(node)) dag_one_line(node$label) else ""
    if (!nzchar(label)) label <- info$label
    parents <- character()
    children <- character()
    for (e in graph$edges %||% list()) {
      if (!is.list(e)) next
      frm <- as.character(e$from %||% "")
      to <- as.character(e$to %||% "")
      ev <- dag_one_line(e$evidence)
      if (identical(to, id) && nzchar(frm)) parents <- c(parents, paste0("- ", frm, ": ", ev))
      if (identical(frm, id) && nzchar(to)) children <- c(children, paste0("- ", to, ": ", ev))
    }
    for (e in graph$external %||% list()) {
      if (!is.list(e)) next
      if (!(id %in% dag_chr(e$used_by))) next
      ref <- as.character(e$ref %||% "")
      if (nzchar(ref)) parents <- c(parents, paste0("- ", ref, ": ", dag_one_line(e$evidence)))
    }
    lines <- c(lines, paste0(
      "NODE ", id, "\n",
      "label: ", label, "\n",
      "key: ", if (dag_node_key(node, info$key)) "true" else "false", "\n",
      "parents:\n", if (length(parents)) paste(parents, collapse = "\n") else "(none)", "\n",
      "children:\n", if (length(children)) paste(children, collapse = "\n") else "(none)", "\n",
      "spec_source: ", info$source, "\n",
      "spec_derivation: ", info$derivation
    ))
  }
  paste(lines, collapse = "\n")
}

is_lookup_only <- function(parsed) {
  if (!is.list(parsed) || is.null(parsed$lookup)) return(FALSE)
  if (!is.null(parsed$answer) || !is.null(parsed$code) || !is.null(parsed$changes) || !is.null(parsed$edges)) return(FALSE)
  TRUE
}

lookup_ids_of <- function(parsed) {
  ids <- unique(trimws(toupper(dag_chr(parsed$lookup))))
  ids[nzchar(ids)]
}

llm_reply_with_lookup <- function(cfg, system, user, spec, graph, dataset, max_tokens = 2000L, max_rounds = 2L, allowed_ids = NULL, on_call = NULL) {
  call_usages <- list()
  call_model <- function(sys, prompt) {
    if (is.function(on_call)) on_call(sys, prompt, "started")
    on.exit(if (is.function(on_call)) on_call(sys, prompt, "finished"), add = TRUE)
    raw <- llm_chat(cfg, sys, prompt, max_tokens = max_tokens, json = TRUE)
    call_usages[length(call_usages) + 1L] <<- list(llm_usage_summary(raw$usage, cfg$provider %||% ""))
    raw
  }
  rounds <- 0L
  repeat {
    raw <- call_model(system, user)
    parsed <- parse_json_reply(raw$text)
    if (!is_lookup_only(parsed)) return(list(raw = raw, parsed = parsed, usage = llm_usage_combine(call_usages), call_usages = call_usages))
    if (rounds >= max_rounds) {
      user <- paste0(
        user,
        "\n\nLOOKUP_LIMIT\nNo more lookups. Reply with the final JSON only. Do not send {\"lookup\":[...]}.\n"
      )
      raw <- call_model(system, user)
      parsed <- parse_json_reply(raw$text)
      if (is_lookup_only(parsed)) {
        stop("Lookup limit reached (2 rounds). The model must return the final answer.", call. = FALSE)
      }
      return(list(raw = raw, parsed = parsed, usage = llm_usage_combine(call_usages), call_usages = call_usages))
    }
    rounds <- rounds + 1L
    block <- dag_lookup_block(spec, graph, dataset, lookup_ids_of(parsed), allowed_ids)
    user <- paste0(user, "\n\nLOOKUP_ROUND ", rounds, "\n", block, "\n")
  }
}

# ---- DAG agent ----

dag_system_prompt <- function(dataset, parameter_graph=FALSE) {
  ds <- toupper(as.character(dataset %||% "this dataset")[1])
  paste(
    paste0("You are the dependency-graph agent for dataset ", ds, "."),
    "TASK: dag_ask",
    "You maintain the dependency graph. You never write code.",
    "You never see either side's code, logs, notes, or results. Do not ask for them.",
    if(isTRUE(parameter_graph)) "An edge goes from the parameter needed to the parameter that depends on it." else "An edge goes from the upstream variable to the variable that depends on it.",
    "Return JSON only:",
    '{"answer":"short markdown","changes":[],"correction":null}',
    if(isTRUE(parameter_graph)) "On this Parameter DAG, node ids are PARAMCDs. Use add_edge/remove_edge {from,to,uses:[{from_var,to_var}],evidence}, add_use/remove_use {from,to,from_var,to_var}, add_undefined/remove_undefined {code}, and edit_spec. Changes apply immediately after validation. If unclear, ask a question and return no changes."
    else "On this Variable DAG, changes apply immediately after validation. Changes may contain add_edge {from,to,evidence}, remove_edge {from,to}, set_layer_keys {paramcd,keys,reason}, or edit_spec {file,dataset,key,column,value}. For variable.csv key is Variable; for parameter.csv key is PARAMCD. Allowed columns: Label, Type, Length, Key, Source, Derivation, PARAM. Existing rows only; never change Dataset, Variable, or PARAMCD. When changing Source, also propose the matching edge change. If unclear, ask a question and return no changes.",
    "correction is one sentence learned from the user, or null.",
    "No prose outside the JSON.",
    dag_lookup_rule(),
    sep = "\n"
  )
}

dag_spec_block <- function(spec, dataset, cap = 8000L) {
  if (is.null(spec)) return("")
  ds <- toupper(dataset)
  lines <- "Variable\tSource\tDerivation"
  vars <- if (exists("spec_vars", mode = "function")) spec_vars(spec, ds) else NULL
  if (!is.null(vars) && nrow(vars)) {
    for (i in seq_len(nrow(vars))) {
      cell <- function(x) if (exists("field_txt", mode = "function")) field_txt(x) else dag_one_line(x)
      lines <- c(lines, paste(cell(vars$Variable[[i]]), cell(vars$Source[[i]]), cell(vars$Derivation[[i]]), sep = "\t"))
    }
  }
  params <- if (exists("spec_params", mode = "function")) spec_params(spec, ds) else NULL
  if (!is.null(params) && nrow(params)) {
    for (i in seq_len(nrow(params))) {
      cell <- function(x) if (exists("field_txt", mode = "function")) field_txt(x) else dag_one_line(x)
      lines <- c(lines, paste0(
        "PARAM ", cell(params$PARAMCD[[i]]), "\t", cell(params$Source[[i]]), "\t", cell(params$Derivation[[i]])
      ))
    }
  }
  dag_cap(paste(lines, collapse = "\n"), cap, "… spec rows capped. Reply with {\"lookup\":[\"ID\"]} for a node that was cut.")
}

dag_thread_block <- function(turns, n = 10L) {
  turns <- turns %||% list()
  if (length(turns) > n) turns <- turns[(length(turns) - n + 1L):length(turns)]
  if (!length(turns)) return("")
  lines <- character()
  for (t in turns) {
    if (!is.list(t)) next
    role <- as.character(t$role %||% "user")[1]
    tx <- dag_one_line(t$text %||% "")
    if (nchar(tx) > 500L) tx <- paste0(substr(tx, 1L, 500L), "…")
    if (!nzchar(tx)) next
    extra <- ""
    ch <- t$changes
    if (is.list(ch) && length(ch) && identical(role, "agent")) {
      extra <- paste0(" [", length(dag_normalize_changes(ch)), " proposed changes, applied=", if (isTRUE(t$applied)) "true" else "false", "]")
    }
    lines <- c(lines, paste0(role, ": ", tx, extra))
  }
  if (!length(lines)) return("")
  paste0("THREAD_BEGIN\n", paste(lines, collapse = "\n"), "\nTHREAD_END\n")
}

dag_user_prompt <- function(p, dataset, spec, graph, mem, message, paramcd=NULL, selected=NULL, parameter_graph=FALSE) {
  ds <- toupper(dataset)
  md <- ""
  path <- dag_md_path(p, ds)
  if (file.exists(path)) md <- read_text(path) %||% ""
  if (!nzchar(md)) md <- format_dag_md(graph, spec, mem$corrections %||% list())
  md <- dag_cap(md, 12000L, "… DAG capped. Reply with {\"lookup\":[\"ID\"]} for a node that was cut.")
  msg <- as.character(message %||% "")
  if (nchar(msg) > 4000L) msg <- substr(msg, 1L, 4000L)
  current_keys <- if (!is.null(paramcd) && exists("layer_key_names",mode="function"))
    paste(layer_key_names(spec,ds,paramcd,graph,p),collapse=", ") else ""
  paste0(
    "DATASET: ", ds, "\n",
    if (!is.null(paramcd)) paste0("PARAMCD_ON_SCREEN: ",toupper(paramcd),"\n") else "",
    if (length(selected)) paste0("SELECTED_NODE_IDS: ",paste(toupper(flat_chr(selected)),collapse=", "),"\n") else "",
    if (nzchar(current_keys)) paste0("CURRENT_LAYER_KEYS: ",current_keys,"\n") else "",
    if (!is.null(paramcd) || length(selected)) "When the user identifies keys, propose set_layer_keys with the complete key list for the parameter on screen. Add selected variables for 'these are keys'; remove a named variable for 'is not a key'.\n" else "",
    "You see the DAG file, spec Source and Derivation, user corrections, and this conversation.\n",
    "You do not see code, logs, notes, or results from either side.\n",
    "DAG_BEGIN\n", md, "\nDAG_END\n",
    "SPEC_BEGIN\n", dag_spec_block(spec, ds), "\nSPEC_END\n",
    if(isTRUE(parameter_graph)) paste0("PARAMETER_DAG_BEGIN\n",json_text(param_graph_read(p,ds)),"\nPARAMETER_DAG_END\n") else "",
    dag_corrections_block(p, ds),
    dag_thread_block(mem$turns),
    "MESSAGE_BEGIN\n", msg, "\nMESSAGE_END\n"
  )
}

dag_normalize_changes <- function(x) {
  if (is.null(x)) return(list())
  if (is.data.frame(x)) {
    if (!nrow(x)) return(list())
    return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  }
  if (!is.list(x)) return(list())
  if (!is.null(x$op)) return(list(x))
  out <- list()
  for (e in x) if (is.list(e) && !is.null(e$op)) out[[length(out) + 1L]] <- e
  out
}

dag_parse_bool <- function(v) {
  if (is.logical(v) && length(v) == 1L && !is.na(v)) return(v)
  if (is.numeric(v) && length(v) == 1L && !is.na(v)) return(v[[1]] != 0)
  s <- tolower(trimws(as.character(v %||% "")[1]))
  if (s %in% c("true", "1", "yes")) return(TRUE)
  if (s %in% c("false", "0", "no")) return(FALSE)
  stop("set_key value must be true or false.", call. = FALSE)
}

dag_canon <- function(dataset, id) {
  id <- toupper(trimws(as.character(id %||% "")[1]))
  if (!nzchar(id)) stop("Unknown node: .", call. = FALSE)
  parsed <- split_node_id(id)
  if (is.null(parsed) || !identical(parsed$dataset, dataset)) stop("Unknown node: ", id, ".", call. = FALSE)
  paste0(parsed$dataset, ".", parsed$var, if (is.null(parsed$paramcd)) "" else paste0("@", parsed$paramcd))
}

dag_would_cycle <- function(graph, from, to) {
  if (identical(from, to)) return(TRUE)
  seen <- character()
  wave <- to
  guard <- 0L
  edges <- graph$edges %||% list()
  while (length(wave) && guard < 10000L) {
    guard <- guard + 1L
    nxt <- character()
    for (id in wave) {
      if (id %in% seen) next
      seen <- c(seen, id)
      for (e in edges) {
        if (!is.list(e) || !identical(as.character(e$from %||% ""), id)) next
        dest <- as.character(e$to %||% "")
        if (!nzchar(dest)) next
        if (identical(dest, from)) return(TRUE)
        if (!(dest %in% seen) && !(dest %in% nxt)) nxt <- c(nxt, dest)
      }
    }
    wave <- unique(nxt)
  }
  FALSE
}

dag_mutate <- function(graph, dataset, changes) {
  if (exists("normalize_graph", mode = "function")) graph <- normalize_graph(graph)
  graph$dataset <- dataset
  flags <- list()
  flashes <- list()
  add_flag <- function(id, cause) {
    flags[[length(flags) + 1L]] <<- list(id = id, cause = cause)
  }
  for (ch in changes) {
    op <- tolower(trimws(as.character(ch$op %||% "")[1]))
    if (!nzchar(op)) stop("A graph change is missing its op.", call. = FALSE)
    if (identical(op, "add_edge") || identical(op, "remove_edge")) {
      from <- dag_canon(dataset, ch$from)
      to <- dag_canon(dataset, ch$to)
      known <- dag_known_ids(graph)
      if (!(from %in% known)) stop("Unknown node: ", from, ".", call. = FALSE)
      if (!(to %in% known)) stop("Unknown node: ", to, ".", call. = FALSE)
      if (identical(from, to)) stop("An edge cannot connect a node to itself (", from, ").", call. = FALSE)
      if (identical(op, "add_edge")) {
        if (dag_would_cycle(graph, from, to)) {
          stop("That edge would create a cycle: ", from, " -> ", to, ".", call. = FALSE)
        }
        exists_edge <- FALSE
        for (e in graph$edges %||% list()) {
          if (is.list(e) && identical(as.character(e$from %||% ""), from) && identical(as.character(e$to %||% ""), to))
            exists_edge <- TRUE
        }
        ev <- dag_one_line(ch$evidence)
        if (!nzchar(ev)) ev <- "Added by the DAG agent"
        if (nchar(ev) > 500L) ev <- substr(ev, 1L, 500L)
        if (!exists_edge) {
          graph$edges <- c(graph$edges %||% list(), list(list(
            from = from, to = to, evidence = ev, origin = "dag_agent"
          )))
        }
        flashes[[length(flashes) + 1L]] <- list(from = from, to = to, kind = "add")
        add_flag(to, from)
      } else {
        keep <- list()
        found <- FALSE
        for (e in graph$edges %||% list()) {
          if (!found && is.list(e) && identical(as.character(e$from %||% ""), from) && identical(as.character(e$to %||% ""), to)) {
            found <- TRUE
            next
          }
          if (is.list(e)) keep[[length(keep) + 1L]] <- e
        }
        if (!found) stop("No edge from ", from, " to ", to, ".", call. = FALSE)
        graph$edges <- keep
        flashes[[length(flashes) + 1L]] <- list(from = from, to = to, kind = "remove")
        add_flag(to, from)
      }
    } else if (identical(op, "set_key")) {
      id <- dag_canon(dataset, ch$node %||% ch$id)
      if (!(id %in% dag_known_ids(graph))) stop("Unknown node: ", id, ".", call. = FALSE)
      value <- dag_parse_bool(ch$value)
      for (i in seq_along(graph$nodes)) {
        if (is.list(graph$nodes[[i]]) && identical(as.character(graph$nodes[[i]]$id %||% ""), id))
          graph$nodes[[i]]$key <- value
      }
      add_flag(id, id)
    } else if (identical(op, "set_categories")) {
      id <- dag_canon(dataset, ch$node %||% ch$id)
      if (!(id %in% dag_known_ids(graph))) stop("Unknown node: ", id, ".", call. = FALSE)
      categories <- ch$categories %||% list()
      if (!is.list(categories)) stop("categories must be a list.", call. = FALSE)
      if (!is.null(categories$value)) categories <- list(categories)
      clean <- list()
      for (cat in categories) {
        if (!is.list(cat)) stop("Each category needs value and rule.", call. = FALSE)
        value <- dag_one_line(cat$value)
        rule <- dag_one_line(cat$rule)
        if (!nzchar(value)) stop("A category value is required.", call. = FALSE)
        if (value %in% vapply(clean, function(x) x$value, "")) stop("Duplicate category value.", call. = FALSE)
        clean[[length(clean) + 1L]] <- list(value = value, rule = rule)
      }
      for (i in seq_along(graph$nodes)) if (identical(graph$nodes[[i]]$id, id))
        graph$nodes[[i]]$categories <- clean
    } else if (identical(op, "add_node")) {
      id <- dag_canon(dataset, ch$id %||% ch$node)
      if (id %in% dag_known_ids(graph)) stop("Node ", id, " already exists.", call. = FALSE)
      parsed <- split_node_id(id)
      label <- dag_one_line(ch$label)
      node <- list(
        id = id, var = parsed$var, paramcd = parsed$paramcd,
        kind = if (is.null(parsed$paramcd)) "variable" else "param_variable"
      )
      if (nzchar(label)) node$label <- label
      graph$nodes <- c(graph$nodes %||% list(), list(node))
    } else {
      stop("Unknown graph change: ", op, ".", call. = FALSE)
    }
  }
  list(graph = graph, flags = flags, edges = flashes)
}

dag_flag_upstream <- function(p, ds, id, causes) {
  causes <- unique(as.character(causes))
  causes <- causes[nzchar(causes) & !is.na(causes)]
  if (!length(causes) || !nzchar(id)) return(invisible())
  st <- get_status(p, ds, id)
  lvl <- st$level
  cur <- if (is.null(st$upstream_changed)) character() else as.character(st$upstream_changed)
  cur <- cur[nzchar(cur) & !is.na(cur)]
  st$upstream_changed <- unique(c(cur, causes))
  st$level <- lvl
  put_status(p, ds, id, st, save = FALSE)
  invisible(st)
}

apply_dag_changes <- function(p, dataset, changes) {
  dataset <- safe_ds(dataset)
  graph <- load_graph(p, dataset)
  if (is.null(graph)) stop("No dependency graph yet.", call. = FALSE)
  changes <- dag_normalize_changes(changes)
  if (!length(changes)) stop("No changes to apply.", call. = FALSE)
  key_changes <- Filter(function(change)identical(tolower(as.character(change$op %||% "")),"set_layer_keys"),changes)
  graph_changes <- Filter(function(change)!identical(tolower(as.character(change$op %||% "")),"set_layer_keys"),changes)
  undo_before <- NULL
  if (length(graph_changes)) {
    ids <- character()
    for (ch in graph_changes) {
      op <- tolower(as.character(ch$op %||% ""))
      if (op %in% c("add_edge", "remove_edge")) ids <- c(ids, dag_canon(dataset, ch$to))
      if (op %in% c("set_key", "set_categories")) ids <- c(ids, dag_canon(dataset, ch$node %||% ch$id))
      if (op == "add_node") ids <- c(ids, dag_canon(dataset, ch$id %||% ch$node))
    }
    ids <- unique(ids)
    undo_before <- list(graph = graph, corrections = dag_agent_read(p, dataset)$corrections,
      statuses = lapply(ids, function(id) get_status(p, dataset, id)), ids = ids)
  }
  built <- if(length(graph_changes))dag_mutate(graph,dataset,graph_changes)else list(graph=graph,flags=list(),edges=list())
  if(length(graph_changes))save_graph(p, built$graph)
  affected <- character()
  by_id <- list()
  for (hit in built$flags) by_id[[hit$id]] <- unique(c(by_id[[hit$id]] %||% character(), hit$cause))
  for (id in names(by_id)) {
    dag_flag_upstream(p, dataset, id, by_id[[id]])
    affected <- c(affected, id)
  }
  key_results <- list()
  for(change in key_changes) {
    result <- layer_keys_set(p,dataset,change$paramcd,change$keys,FALSE)
    key_results[[length(key_results)+1L]] <- result
  }
  if (length(affected)) write_state(p)
  if (length(graph_changes)) {
    steps <- dag_undo_read(p, dataset)$steps
    after_updated <- lapply(undo_before$ids, function(id) get_status(p, dataset, id)$updated %||% NULL)
    names(after_updated) <- undo_before$ids
    steps <- c(steps, list(list(graph = undo_before$graph, corrections = undo_before$corrections,
      ids = as.list(undo_before$ids), statuses = undo_before$statuses, after_updated = after_updated,
      label = dag_change_label(graph_changes), at = now_iso())))
    if (length(steps) > 20L) steps <- tail(steps, 20L)
    dag_undo_write(p, dataset, steps)
  }
  mem <- dag_agent_read(p, dataset)
  if (length(mem$turns)) {
    last <- mem$turns[[length(mem$turns)]]
    if (is.list(last) && identical(as.character(last$role %||% ""), "agent")) {
      last$applied <- TRUE
      mem$turns[[length(mem$turns)]] <- last
      dag_agent_write(p, dataset, mem)
    }
  }
  list(
    ok = TRUE,
    affected = as.list(unique(affected)),
    edges = built$edges,
    layer_keys = key_results
  )
}

dag_history_dir <- function(p, ds=NULL) file.path(project_dir_of(p), ".dualdag", "dag_agent_history")

dag_snapshot <- function(p, ds) {
  root <- project_dir_of(p); stamp <- paste0(safe_ds(ds), "_", format(Sys.time(), "%Y%m%dT%H%M%OS6"), "_", Sys.getpid())
  dest <- file.path(dag_history_dir(root, ds), stamp); dir.create(dest, recursive=TRUE, showWarnings=FALSE)
  pg_path <- if(exists("param_graph_path",mode="function")) param_graph_path(root,ds) else file.path(root,".dualdag","state",paste0("param_graph_",safe_ds(ds),".json"))
  files <- c(file.path(root,"spec","variable.csv"), file.path(root,"spec","parameter.csv"),
             graph_path(root,ds), pg_path, dag_agent_path(root,ds),
             file.path(root,".dualdag","state","state.json"))
  names(files) <- c("variable.csv","parameter.csv","graph.json","param_graph.json","agent.json","state.json")
  present <- names(files)[file.exists(files)]
  for (nm in present) file.copy(files[[nm]], file.path(dest,nm), overwrite=TRUE)
  write_json(list(dataset=safe_ds(ds),files=as.list(present), at=now_iso()), file.path(dest,"manifest.json"))
  dest
}

dag_history_finalize <- function(p,ds,dir) {
  manifest <- read_json(file.path(dir,"manifest.json"))
  if(!is.list(manifest)) stop("Could not finalize DAG agent history.",call.=FALSE)
  root <- project_dir_of(p)
  manifest$md5 <- list(
    variable=file_md5(file.path(root,"spec","variable.csv")),
    parameter=file_md5(file.path(root,"spec","parameter.csv")),
    graph=file_md5(graph_path(root,ds))
  )
  manifest$time <- as.numeric(Sys.time())
  manifest$at <- now_iso()
  write_json(manifest,file.path(dir,"manifest.json"))
  invisible(manifest)
}

dag_edit_spec <- function(p, ds, edit) {
  file <- as.character(edit$file %||% "")[1]
  if (!file %in% c("variable.csv","parameter.csv")) stop("edit_spec file must be variable.csv or parameter.csv.",call.=FALSE)
  key <- if (file=="variable.csv") "Variable" else "PARAMCD"
  column <- as.character(edit$column %||% "")[1]
  allowed <- if(file=="variable.csv") c("Label","Type","Length","Key","Source","Derivation") else c("Label","Type","Length","Key","Source","Derivation","PARAM")
  if (!column %in% allowed || !column %in% if(file=="variable.csv") .VAR_COLS else .PAR_COLS)
    stop("Unknown or disallowed column for ",file,": ",column,".",call.=FALSE)
  dataset <- safe_ds(edit$dataset %||% ds); if (!identical(dataset,ds)) stop("edit_spec dataset must match the active dataset.",call.=FALSE)
  row_key <- toupper(trimws(as.character(edit$key %||% "")[1])); if(!nzchar(row_key)) stop("edit_spec key is required.",call.=FALSE)
  path <- file.path(project_dir_of(p),"spec",file); tab <- read_csv_table(path)$df
  cols <- names(tab); ix <- which(toupper(trimws(as.character(tab[["Dataset"]])))==dataset & toupper(trimws(as.character(tab[[key]])))==row_key)
  if(length(ix)!=1L) stop("Unknown or ambiguous ",key," row: ",row_key,".",call.=FALSE)
  old_value <- as.character(tab[[column]][ix]); value <- as.character(edit$value %||% "")[1]; if(is.na(value)) value <- ""
  if(column=="Length" && nzchar(trimws(value)) && !grepl("^[1-9][0-9]*$",trimws(value))) stop("Length must be a positive integer or blank.",call.=FALSE)
  if(column=="Key" && nzchar(trimws(value)) && !grepl("^[1-9][0-9]*$",trimws(value))) stop("Key must be a positive integer or blank.",call.=FALSE)
  tab[[column]][ix] <- value
  tmp <- tempfile(pattern=".dag-agent-",tmpdir=dirname(path)); on.exit(unlink(tmp),add=TRUE)
  utils::write.csv(tab[,cols,drop=FALSE],tmp,row.names=FALSE,quote=TRUE,na="",eol="\n")
  variable_path <- file.path(project_dir_of(p),"spec","variable.csv"); parameter_path <- file.path(project_dir_of(p),"spec","parameter.csv")
  before <- read_spec(variable_path,if(file.exists(parameter_path))parameter_path else NULL)
  parsed <- read_spec(if(file=="variable.csv") tmp else variable_path,
                      if(file=="parameter.csv") tmp else { q<-file.path(project_dir_of(p),"spec","parameter.csv"); if(file.exists(q))q else NULL })
  error_text <- function(x) vapply(Filter(function(z)identical(as.character(z$severity %||% "error"),"error"),x$problems %||% list()),function(z)paste(z$file %||% "",z$message %||% ""),"")
  errors <- setdiff(error_text(parsed),error_text(before))
  if (length(errors)) stop("Edited spec failed validation: ",errors[[1]],call.=FALSE)
  if(exists("sync_external",mode="function")) sync_external(p)
  old_hashes <- p$state$row_hash %||% list()
  if(!file.rename(tmp,path)) stop("Could not replace ",file,".",call.=FALSE)
  if(exists("sync_external",mode="function")) sync_external(p)
  new_hashes <- p$state$row_hash %||% list()
  affected <- names(old_hashes)[vapply(names(old_hashes),function(id) !identical(old_hashes[[id]],new_hashes[[id]]),TRUE)]
  list(file=file,key=row_key,column=column,old=old_value,value=value,reset=length(affected))
}

dag_restore_snapshot <- function(p, ds, dir) {
  current_mem <- tryCatch(dag_agent_read(p,ds),error=function(e)list())
  manifest <- read_json(file.path(dir,"manifest.json")); present <- unlist(manifest$files,use.names=FALSE)
  dests <- c("variable.csv"=file.path(project_dir_of(p),"spec","variable.csv"),
    "parameter.csv"=file.path(project_dir_of(p),"spec","parameter.csv"),
    "graph.json"=graph_path(p,ds), "param_graph.json"=if(exists("param_graph_path",mode="function"))param_graph_path(p,ds) else file.path(project_dir_of(p),".dualdag","state",paste0("param_graph_",safe_ds(ds),".json")),
    "agent.json"=dag_agent_path(p,ds), "state.json"=file.path(project_dir_of(p),".dualdag","state","state.json"))
  for(nm in names(dests)) { src<-file.path(dir,nm); if(nm %in% present) file.copy(src,dests[[nm]],overwrite=TRUE) else unlink(dests[[nm]]) }
  if(is.environment(p) && exists("read_state_file",mode="function")) p$state <- read_state_file(p)
  if (exists("write_network_json", mode = "function")) write_network_json(p, ds)
  restored_mem <- dag_agent_read(p,ds)
  restored_mem$turns <- current_mem$turns %||% restored_mem$turns
  restored_mem$param_turns <- current_mem$param_turns %||% restored_mem$param_turns
  dag_agent_write(p,ds,restored_mem)
  invisible(TRUE)
}

dag_tail <- function(turns, n = 10L) {
  turns <- turns %||% list()
  if (length(turns) > n) turns <- turns[(length(turns) - n + 1L):length(turns)]
  turns
}

dag_ask <- function(cfg, project, dataset, message, paramcd=NULL, selected=NULL, parameter_graph=FALSE) {
  dataset <- safe_ds(dataset)
  message <- trimws(as.character(message %||% "")[1])
  if (!nzchar(message)) stop("message is required.", call. = FALSE)
  if (nchar(message) > 4000L) message <- substr(message, 1L, 4000L)
  root <- project_dir_of(project)
  edit_project <- if (is.environment(project)) project else open_project(root)
  spec <- tryCatch(read_project_spec(root), error = function(e) NULL)
  graph <- tryCatch(load_graph(root, dataset), error = function(e) NULL)
  if (is.null(graph)) stop("No dependency graph yet.", call. = FALSE)
  mem <- dag_agent_read(root, dataset)
  system <- dag_system_prompt(dataset,parameter_graph)
  user <- dag_user_prompt(root, dataset, spec, graph, mem, message,paramcd,selected,parameter_graph)
  answered <- llm_reply_with_lookup(cfg, system, user, spec, graph, dataset, max_tokens = 2000L)
  parsed <- answered$parsed
  answer <- trimws(as.character(parsed$answer %||% "")[1])
  if (!nzchar(answer) || is.na(answer)) stop("The model returned an empty answer.", call. = FALSE)
  changes <- dag_normalize_changes(parsed$changes)
  if(length(changes)) for(i in seq_along(changes)) if(identical(changes[[i]]$op,"set_layer_keys")) {
    proposed <- layer_keys_set(root,dataset,changes[[i]]$paramcd,changes[[i]]$keys,dry_run=TRUE)
    before <- layer_key_names(spec,dataset,changes[[i]]$paramcd,graph,root)
    after <- flat_chr(proposed$keys)
    changes[[i]]$added <- as.list(setdiff(after,before))
    changes[[i]]$removed <- as.list(setdiff(before,after))
    changes[[i]]$reset_count <- proposed$reset_count
  }
  correction <- parsed$correction
  if (is.null(correction)) {
    correction <- NULL
  } else {
    correction <- dag_one_line(correction)
    if (!nzchar(correction) || identical(tolower(correction), "null")) correction <- NULL
  }
  card <- list(); snap <- NULL; validation_error <- NULL
  if(length(changes)) {
    snap <- dag_snapshot(root,dataset)
    tryCatch({
      spec_edits <- Filter(function(ch) identical(tolower(as.character(ch$op %||% "")),"edit_spec"),changes)
      graph_edits <- Filter(function(ch) !identical(tolower(as.character(ch$op %||% "")),"edit_spec"),changes)
      for(ch in spec_edits) {
        ed <- dag_edit_spec(edit_project,dataset,ch); card[[length(card)+1L]] <- list(text=paste0(ed$file," ",ed$key," ",ed$column,": ",if(nzchar(ed$old))ed$old else "(blank)"," → ",if(nzchar(ed$value))ed$value else "(blank)"),reset=ed$reset)
      }
      if(length(graph_edits)) {
        specialized_param_ops <- vapply(graph_edits,function(ch) ch$op %in% c("add_use","remove_use","add_undefined","remove_undefined"),TRUE)
        use_param_graph <- isTRUE(parameter_graph) || any(specialized_param_ops)
        param_ops <- vapply(graph_edits,function(ch) ch$op %in% c("add_edge","remove_edge","add_use","remove_use","add_undefined","remove_undefined"),TRUE)
        if(use_param_graph && !all(param_ops)) stop("Parameter graph operations cannot be mixed with variable DAG operations.",call.=FALSE)
        if(use_param_graph) {
          applied <- param_graph_apply(edit_project,dataset,graph_edits)
          card[[length(card)+1L]] <- list(text="Parameter DAG updated",reset=0L)
        } else {
          applied <- apply_dag_changes(edit_project,dataset,graph_edits)
          for(ch in graph_edits) if(ch$op %in% c("add_edge","remove_edge")) card[[length(card)+1L]] <- list(text=paste0(if(ch$op=="add_edge")"Added edge " else "Removed edge ",ch$from," → ",ch$to),reset=0L)
          for(ch in graph_edits) if(ch$op=="set_layer_keys") card[[length(card)+1L]] <- list(text=paste0("Parameter layer keys updated: ",ch$paramcd),reset=as.integer(ch$reset_count %||% 0L))
        }
      }
      new_spec <- read_project_spec(root)
      pg <- if(exists("param_graph_read",mode="function")) param_graph_read(root,dataset) else NULL
      if(!is.null(pg) && !is.null(new_spec)) { pg$spec_hash <- new_spec$hash; pg$stale <- FALSE; param_graph_write(root,pg) }
      dag_history_finalize(root,dataset,snap)
    }, error=function(e) { validation_error <<- conditionMessage(e); try(dag_restore_snapshot(root,dataset,snap),silent=TRUE); unlink(snap,recursive=TRUE) })
    for(i in seq_along(card)) {
      if((card[[i]]$reset %||% 0)>0) card[[i]]$text <- paste0(card[[i]]$text," · ",card[[i]]$reset," variables reset")
    }
  }
  if(!is.null(validation_error)) {
    refusal_user <- paste0(user,"\n\nVALIDATION_REFUSAL\nThe complete change set was rolled back. Validation said: ",validation_error,
      "\nExplain this refusal to the user. Return JSON with an answer and an empty changes array.")
    refusal_raw <- tryCatch(llm_chat(cfg,system,refusal_user,max_tokens=1200L,json=TRUE),error=function(e)NULL)
    refusal_parsed <- if(is.list(refusal_raw)) tryCatch(parse_json_reply(refusal_raw$text),error=function(e)NULL) else NULL
    refusal_answer <- trimws(as.character(refusal_parsed$answer %||% "")[1])
    answer <- if(nzchar(refusal_answer)) refusal_answer else paste0("I couldn't apply that change: ",validation_error)
    changes <- list(); correction <- NULL; card <- list()
    if(is.list(refusal_raw)) {
      usage <- llm_usage_summary(refusal_raw$usage,cfg$provider %||% "")
      answered$usage <- llm_usage_combine(list(answered$usage,usage))
      answered$raw$model <- refusal_raw$model %||% answered$raw$model
    }
  }
  at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
  mem$turns <- c(mem$turns, list(
    list(at = at, role = "user", text = message),
    list(at = at, role = "agent", text = answer, changes = changes, card = card, applied = length(changes)>0,
         usage = answered$usage)
  ))
  if (!is.null(correction)) {
    mem$corrections <- c(mem$corrections, list(list(at = at, text = correction)))
  }
  dag_agent_write(root, dataset, mem)
  if (!is.null(correction)) write_dag_md(root, graph)
  list(
    answer = answer,
    changes = changes,
    correction = correction,
    model = answered$raw$model %||% "",
    usage = answered$usage,
    turns = dag_tail(mem$turns, 10L)
    ,card = card
  )
}

# Fake DAG agent. One lookup round, then a final JSON answer.
fake_dag_reply <- function(system, user) {
  user <- user %||% ""
  msg <- if (exists("text_block", mode = "function")) text_block(user, "MESSAGE_BEGIN", "MESSAGE_END") else ""
  if (!nzchar(msg)) msg <- user
  if (grepl("lookup forever", msg, ignore.case = TRUE)) {
    return('{"lookup":["NO.SUCH","ADSL.AGE"]}')
  }
  if (!grepl("LOOKUP_ROUND", user, fixed = TRUE)) {
    id <- "ADSL.AGE"
    mu <- toupper(msg)
    m <- regexec("\\b([A-Z][A-Z0-9_]*\\.[A-Z][A-Z0-9_]*(?:@[A-Z0-9_]+)?)\\b", mu, perl = TRUE)
    r <- regmatches(mu, m)[[1]]
    if (length(r) >= 2L) id <- r[[2]]
    return(json_text(list(lookup = list(id, "NOT.A.NODE"))))
  }
  mu <- toupper(msg)
  changes <- list()
  line_value <- function(name) {
    hit <- regmatches(user,regexec(paste0("(?m)^",name,":\\s*([^\\n]*)"),user,perl=TRUE))[[1]]
    if(length(hit)<2L)"" else hit[[2]]
  }
  pc <- toupper(line_value("PARAMCD_ON_SCREEN"))
  if (nzchar(pc) && (grepl("THESE ARE KEYS|MAKE THESE.*KEY",mu) || grepl("IS NOT A KEY|NOT A KEY",mu))) {
    current <- strsplit(line_value("CURRENT_LAYER_KEYS"),",\\s*")[[1]]
    current <- toupper(trimws(current)); current <- current[nzchar(current)]
    picked <- line_value("SELECTED_NODE_IDS")
    picked <- if(nzchar(picked)) strsplit(picked,",\\s*")[[1]] else character()
    picked <- vapply(picked,function(id){parsed<-parse_node_id(id);if(is.null(parsed))"" else parsed$var},"")
    remove <- regmatches(mu,regexec("([A-Z][A-Z0-9_]*)\\s+(?:IS|AS)\\s+NOT\\s+A\\s+KEY",mu,perl=TRUE))[[1]]
    keys <- if(length(remove)>=2L)setdiff(current,remove[[2]]) else unique(c(current,picked))
    changes[[1]] <- list(op="set_layer_keys",paramcd=pc,keys=keys,reason=msg)
  }
  m <- regexec("ADD EDGE\\s+([A-Z0-9_.@]+)\\s*(?:->|→)\\s*([A-Z0-9_.@]+)", mu, perl = TRUE)
  r <- regmatches(mu, m)[[1]]
  if (length(r) >= 3L) {
    changes[[length(changes) + 1L]] <- list(
      op = "add_edge", from = r[[2]], to = r[[3]], evidence = "User asked to add this edge"
    )
  }
  m2 <- regexec("REMOVE EDGE\\s+([A-Z0-9_.@]+)\\s*(?:->|→)\\s*([A-Z0-9_.@]+)", mu, perl = TRUE)
  r2 <- regmatches(mu, m2)[[1]]
  if (length(r2) >= 3L) {
    changes[[length(changes) + 1L]] <- list(op = "remove_edge", from = r2[[2]], to = r2[[3]])
  }
  correction <- NULL
  if (grepl("REMEMBER:", msg, ignore.case = TRUE)) {
    correction <- trimws(sub("(?is).*REMEMBER:\\s*", "", msg, perl = TRUE))
    correction <- trimws(strsplit(correction, "\n", fixed = TRUE)[[1]][[1]])
    if (!nzchar(correction)) correction <- NULL
  }
  answer <- if (length(changes)) "Proposed a graph change." else "The graph is unchanged."
  json_text(list(answer = answer, changes = changes, correction = correction))
}
