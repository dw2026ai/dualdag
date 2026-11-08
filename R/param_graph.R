# Parameter dependency graph. It is independent from graph_<DS>.json and run state.

param_graph_path <- function(p, ds) {
  file.path(project_dir_of(p), ".dualdag", "state", paste0("param_graph_", safe_ds(ds), ".json"))
}

param_graph_read <- function(p, ds) {
  graph <- read_json(param_graph_path(p, ds))
  if (!is.null(graph)) for (i in seq_along(graph$edges %||% list())) graph$edges[[i]]$uses <- param_edge_uses(graph$edges[[i]]$uses, ds)
  graph
}

param_graph_write <- function(p, graph) {
  path <- param_graph_path(p, graph$dataset)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_json(graph, path)
  if (exists("write_network_json", mode = "function")) write_network_json(p, graph$dataset)
  invisible(graph)
}

param_empty_graph <- function(ds, hash = NULL) {
  list(dataset = ds, spec_hash = hash, nodes = list(), edges = list(),
       error = NULL, warnings = list())
}

param_edge_uses <- function(uses, dataset = NULL) {
  normalize <- function(value) {
    value <- toupper(trimws(field_txt(value)))
    prefix <- if (is.null(dataset)) "" else paste0(toupper(dataset), ".")
    if (nzchar(prefix) && startsWith(value, prefix)) substring(value, nchar(prefix) + 1L) else value
  }
  out <- lapply(as_records(uses), function(x) list(from_var=normalize(x$from_var), to_var=normalize(x$to_var)))
  Filter(function(x) nzchar(x$from_var) && nzchar(x$to_var), out)
}

param_defined_nodes <- function(spec, ds) {
  params <- spec_params(spec, ds)
  if (!nrow(params)) return(list())
  codes <- unique(toupper(trimws(params$PARAMCD)))
  codes <- codes[nzchar(codes)]
  lapply(codes, function(code) {
    row <- match(code, toupper(trimws(params$PARAMCD)))
    list(code = code, label = field_txt(params$PARAM[[row]]), defined = TRUE,
         mentioned_in = list())
  })
}

# Stable parameter build order. Ready nodes retain parameter.csv order.
param_layer_order <- function(spec, ds, graph = NULL) {
  params <- spec_params(spec, ds)
  codes <- unique(toupper(trimws(params$PARAMCD)))
  codes <- codes[nzchar(codes)]
  if (!length(codes)) return(list(order = character(), cycle = character()))
  carry <- unique(intersect(toupper(flat_chr(graph$carry_members %||% character())), codes))
  skeleton <- if (length(carry)) graph_skeleton(spec, ds, carry_members=carry) else NULL
  semantic_to_layer <- setNames(codes, codes)
  if (!is.null(skeleton)) for (layer in as_records(skeleton$layers)) {
    members <- toupper(flat_chr(layer$members_parameters %||% list()))
    semantic_to_layer[members] <- toupper(layer$paramcd)
  }
  layer_of <- function(code) {
    code <- toupper(code)
    at <- match(code, names(semantic_to_layer))
    if (!is.na(at)) unname(semantic_to_layer[[at]]) else code
  }
  layers <- unique(vapply(codes, layer_of, ""))
  edges <- Filter(function(e) e$from %in% codes && e$to %in% codes, as_records(graph$edges %||% list()))
  edges <- lapply(edges, function(e) list(from=layer_of(toupper(e$from)),to=layer_of(toupper(e$to))))
  edges <- Filter(function(e) e$from != e$to, edges)
  remaining <- layers
  out <- character()
  while (length(remaining)) {
    ready <- remaining[!vapply(remaining, function(code) any(vapply(edges, function(e)
      identical(toupper(e$to), code) && toupper(e$from) %in% remaining, TRUE)), TRUE)]
    if (!length(ready)) return(list(order = c(out, remaining), cycle = remaining))
    out <- c(out, ready)
    remaining <- setdiff(remaining, ready)
  }
  list(order = out, cycle = character())
}

# Shared by runtime and gate: explicit edge uses plus source-column references.
param_layer_visible_columns <- function(spec, ds, param_graph, code, graph=NULL, project=NULL) {
  params <- spec_params(spec, ds)
  hit <- which(toupper(trimws(params$PARAMCD)) == toupper(code))
  if (!length(hit)) return(list())
  row <- params[hit[[1]], , drop = FALSE]
  out <- list()
  for (e in as_records(param_graph$edges %||% list())) if (toupper(e$to) == toupper(code)) {
    parent <- toupper(e$from)
    uses <- vapply(param_edge_uses(e$uses, ds), `[[`, "", "from_var")
    parent_keys <- if(exists("layer_key_names",mode="function"))layer_key_names(spec,ds,parent,graph,project)else character()
    out[[parent]] <- unique(c(out[[parent]],setdiff(uses,parent_keys)))
  }
  refs <- domain_var_refs(field_txt(row$Source[[1]]))
  source_vars <- vapply(Filter(function(r) toupper(r$dom) == toupper(ds), refs), `[[`, "", "var")
  if (length(source_vars)) for (parent in names(out)) out[[parent]] <- unique(c(out[[parent]], source_vars))
  lapply(out, function(x) unique(toupper(x[nzchar(x)])))
}

param_layer_read_vars <- function(spec, ds, param_graph, code, graph=NULL, project=NULL) {
  unique(unlist(param_layer_visible_columns(spec, ds, param_graph, code,graph,project), use.names=FALSE))
}

param_layer_blocker <- function(project, ds, side, code, spec = NULL, param_graph = NULL) {
  if (is.null(spec)) spec <- load_project_spec(project)
  if (is.null(param_graph) && exists("param_graph_read", mode = "function")) param_graph <- param_graph_read(project, ds)
  if (is.null(param_graph)) return(NULL)
  order <- param_layer_order(spec, ds, param_graph)
  if (length(order$cycle)) stop("Parameter dependency cycle: ", paste(order$cycle, collapse = " -> "), call. = FALSE)
  carry <- unique(intersect(toupper(flat_chr(param_graph$carry_members %||% character())),
    toupper(trimws(spec_params(spec, ds)$PARAMCD))))
  skeleton <- if (length(carry)) graph_skeleton(spec, ds, carry_members=carry) else NULL
  semantic_to_layer <- setNames(toupper(trimws(spec_params(spec, ds)$PARAMCD)),
    toupper(trimws(spec_params(spec, ds)$PARAMCD)))
  if (!is.null(skeleton)) for (layer in as_records(skeleton$layers)) {
    members <- toupper(flat_chr(layer$members_parameters %||% list()))
    semantic_to_layer[members] <- toupper(layer$paramcd)
  }
  layer_of <- function(x) {
    x <- toupper(x)
    at <- match(x, names(semantic_to_layer))
    if (!is.na(at)) unname(semantic_to_layer[[at]]) else x
  }
  target <- layer_of(code)
  idx <- match(target, order$order)
  if (is.na(idx) || idx <= 1L) return(NULL)
  ancestors <- character(); queue <- target
  while (length(queue)) {
    current <- queue[[1]]; queue <- queue[-1L]
    parents <- unique(vapply(Filter(function(e) layer_of(e$to) == current, param_graph$edges %||% list()), function(e) layer_of(e$from), ""))
    fresh <- setdiff(parents, ancestors); ancestors <- unique(c(ancestors, fresh)); queue <- c(queue, fresh)
  }
  nodes <- state_nodes(project, ds)
  graph <- tryCatch(load_project_graph(project, ds), error = function(e) NULL)
  carry <- unique(vapply(toupper(flat_chr(param_graph$carry_members %||% character())), layer_of, ""))
  needed <- list(); queue <- target; visited <- character()
  while (length(queue)) {
    child <- queue[[1L]]; queue <- queue[-1L]
    if (child %in% visited) next
    visited <- c(visited, child)
    incoming <- Filter(function(e) layer_of(e$to) == child, param_graph$edges %||% list())
    for (edge in incoming) {
      parent <- layer_of(edge$from)
      vars <- unique(toupper(vapply(param_edge_uses(edge$uses, ds), `[[`, "", "from_var")))
      vars <- vars[nzchar(vars)]
      needed[[parent]] <- unique(c(needed[[parent]], vars))
      queue <- c(queue, parent)
    }
  }
  for (up in intersect(order$order[seq_len(idx - 1L)], ancestors)) {
    members <- if (!is.null(graph)) Filter(function(n) identical(toupper(n$paramcd %||% ""), up) && n$kind %in% c("records", "param_variable"), graph$nodes) else list()
    if (up %in% carry && length(needed[[up]])) {
      members <- Filter(function(n) identical(toupper(n$paramcd %||% ""), up) && identical(n$kind, "param_variable") &&
        toupper(n$var %||% "") %in% needed[[up]], graph$nodes %||% list())
    }
    ids <- if (length(members)) vapply(members, `[[`, "", "id") else paste0(ds, ".RECORDS@", up)
    topo <- if (!is.null(graph)) inspect_topo(graph) else ids
    ids <- unique(c(topo[topo %in% ids], setdiff(ids, topo)))
    bad <- ids[!vapply(ids, function(id) identical(as.character(nodes[[id]]$level %||% ""), "L3"), TRUE)]
    if (length(bad)) return(list(layer = up, variable = sub(".*\\.", "", sub("@.*", "", bad[[1]]))))
  }
  NULL
}

param_layer_block_message <- function(project, ds, side, code, spec = NULL, param_graph = NULL) {
  if (is.environment(project) && exists("project_dir_of",mode="function")) project <- project_dir_of(project)
  spec <- spec %||% tryCatch(read_project_spec(project), error=function(e) NULL)
  if (!dataset_type_get(project, ds, spec) %in% c("BDS", "BDS-TTE")) return(NULL)
  blocked <- param_layer_blocker(project, ds, side, code, spec, param_graph)
  if (is.null(blocked)) return(NULL)
  paste0("Layer ", code, " is blocked by unfinished layer ", blocked$layer,
    ": ", blocked$variable, " is not Data matched on ", side, ".")
}

param_saved_corrections <- function(p, ds) {
  mem <- tryCatch(dag_agent_read(p, ds), error = function(e) list())
  mem$param_corrections %||% list()
}

param_corrections <- function(p, ds) {
  param_saved_corrections(p, ds)
}

param_corrections_block <- function(p, ds) {
  corrections <- param_saved_corrections(p, ds)
  lines <- vapply(corrections, function(correction) {
    text <- dag_one_line(correction$text %||% "")
    if (!nzchar(text)) return("")
    paste0("- ", text)
  }, "")
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return("")
  paste0("PARAMETER_USER_CORRECTIONS_BEGIN\n", paste(lines, collapse = "\n"),
         "\nPARAMETER_USER_CORRECTIONS_END\n")
}

param_graph_prompt <- function(spec, ds) {
  params <- spec_params(spec, ds)
  vars <- spec_vars(spec, ds)
  lines <- c(paste0("DATASET\t", ds), "PARAMETERS")
  if (nrow(params)) {
    for (i in seq_len(nrow(params))) {
      lines <- c(lines, paste("PARAM", field_txt(params$PARAMCD[[i]]),
                              field_txt(params$PARAM[[i]]), field_txt(params$Source[[i]]),
                              field_txt(params$Derivation[[i]]), sep = "\t"))
    }
  }
  lines <- c(lines, "VARIABLES")
  if (nrow(vars)) {
    for (i in seq_len(nrow(vars))) {
      lines <- c(lines, paste("VAR", field_txt(vars$Variable[[i]]),
                              field_txt(vars$Label[[i]]), field_txt(vars$Source[[i]]),
                              field_txt(vars$Derivation[[i]]), sep = "\t"))
    }
  }
  paste(lines, collapse = "\n")
}

param_graph_system <- function() {
  paste(
    "TASK: param_extract",
    "Extract dependencies between parameters for one dataset. Edge direction: from is read to derive to.",
    "from and to must be PARAMCDs listed in the user prompt. Add an edge only when rows or values of one parameter are read to derive another parameter.",
    "A collected parameter reading source data is not an interparameter edge.",
    "For every edge, uses lists every non-key variable read, with bare variable names from this dataset (for example WEIGHT -> BMI may include AVAL -> AVAL). Do not list key variables.",
    "Use RECORDS -> RECORDS when the upstream parameter's rows determine which output rows exist.",
    "For every edge, evidence must be an exact quote from the supplied CSV row. Do not reject or alter a dependency because its evidence quote is incomplete or paraphrased.",
    "Each edge has uses: [{from_var,to_var}], mapping variables between parameters; RECORDS means parameter rows.",
    "Return JSON only: {\"edges\":[{\"from\":\"\",\"to\":\"\",\"evidence\":\"\",\"uses\":[]}],\"undefined\":[\"\"]}.",
    "Undefined means a parameter code referenced but not defined in parameter.csv. Do not report ordinary words or column names.",
    sep = "\n"
  )
}

param_mentions <- function(code, params, vars) {
  out <- character()
  param_text <- paste(params$PARAMCD, params$PARAM, params$Source, params$Derivation,
                      collapse = " ")
  if (grepl(paste0("\\b", code, "\\b"), param_text, perl = TRUE)) {
    out <- c(out, paste0("parameter.csv: ", code))
  }
  if (nrow(vars)) {
    variable_text <- paste(vars$Variable, vars$Label, vars$Source, vars$Derivation,
                           collapse = " ")
    if (grepl(paste0("\\b", code, "\\b"), variable_text, perl = TRUE)) {
      out <- c(out, paste0("variable.csv: ", code))
    }
  }
  out
}

param_would_cycle <- function(graph, from, to) {
  seen <- character()
  wave <- to
  while (length(wave)) {
    if (from %in% wave) return(TRUE)
    seen <- unique(c(seen, wave))
    edges <- Filter(function(edge) edge$from %in% wave, graph$edges %||% list())
    wave <- unique(vapply(edges, function(edge) edge$to, ""))
    wave <- setdiff(wave, seen)
  }
  FALSE
}

param_extract <- function(cfg, spec, ds, project = NULL) {
  ds <- safe_ds(ds)
  params <- spec_params(spec, ds)
  vars <- spec_vars(spec, ds)
  graph <- param_empty_graph(ds, spec$hash %||% NULL)
  graph$nodes <- param_defined_nodes(spec, ds)
  if (!nrow(params)) return(graph)

  prompt <- param_graph_prompt(spec, ds)
  result <- llm_chat(cfg, param_graph_system(), prompt, max_tokens = 2500L, json = TRUE)
  parsed <- parse_json_reply(result$text)
  codes <- vapply(graph$nodes, function(node) node$code, "")
  undefined <- unique(toupper(unlist(parsed$undefined %||% character(), use.names = FALSE)))
  for (code in undefined) {
    if (!grepl("^[A-Z][A-Z0-9_]*$", code)) next
    mentions <- param_mentions(code, params, vars)
    if (length(mentions) && !code %in% codes) {
      graph$nodes <- c(graph$nodes, list(list(code = code, label = code, defined = FALSE,
                                              mentioned_in = as.list(mentions))))
    }
  }

  known <- vapply(graph$nodes, function(node) node$code, "")
  key_vars <- toupper(vars$Variable[!is.na(suppressWarnings(as.integer(vars$Key))) & suppressWarnings(as.integer(vars$Key)) != 0L])
  normalize_code <- function(value) {
    value <- toupper(trimws(field_txt(value)))
    prefix <- paste0(ds, ".")
    if (startsWith(value, prefix)) substring(value, nchar(prefix) + 1L) else value
  }
  for (edge in as_records(parsed$edges)) {
    from <- normalize_code(edge$from)
    to <- normalize_code(edge$to)
    if (!from %in% known || !to %in% known || identical(from, to)) next
    valid_use <- function(code, var) var == "RECORDS" || var %in% toupper(vars$Variable)
    uses <- param_edge_uses(edge$uses, ds)
    uses <- Filter(function(u) !u$from_var %in% key_vars && !u$to_var %in% key_vars, uses)
    bad <- Filter(function(u) !valid_use(from,u$from_var) || !valid_use(to,u$to_var),uses)
    if (length(bad)) graph$warnings <- c(graph$warnings,list(paste0("Dropped invalid edge use ",from,".",bad[[1]]$from_var," -> ",to,".",bad[[1]]$to_var,".")))
    uses <- Filter(function(u) valid_use(from,u$from_var) && valid_use(to,u$to_var),uses)
    if (param_would_cycle(graph, from, to)) {
      graph$warnings <- c(graph$warnings, list(paste0(
        "Dropped edge ", from, " -> ", to, " because it would create a cycle."
      )))
      next
    }
    duplicate <- any(vapply(graph$edges, function(existing) {
      existing$from == from && existing$to == to
    }, TRUE))
    if (!duplicate) {
      graph$edges <- c(graph$edges, list(list(from = from, to = to,
                                              evidence = field_txt(edge$evidence), origin = "llm", uses=uses)))
    }
  }

  graph
}

param_graph_apply <- function(p, ds, changes) {
  ds <- safe_ds(ds)
  graph <- param_graph_read(p, ds)
  if (is.null(graph)) stop("No parameter graph yet.")
  changes <- dag_normalize_changes(changes)
  if (!length(changes)) stop("No changes to apply.")

  for (change in changes) {
    op <- as.character(change$op %||% "")
    from <- toupper(trimws(as.character(change$from %||% "")[1]))
    to <- toupper(trimws(as.character(change$to %||% "")[1]))
    code <- toupper(trimws(as.character(change$code %||% "")[1]))
    ids <- vapply(graph$nodes, function(node) node$code, "")

    if (op %in% c("add_edge", "remove_edge")) {
      if (!from %in% ids || !to %in% ids) {
        stop("Unknown parameter code: ", if (!from %in% ids) from else to, ".")
      }
      if (identical(from, to)) stop("An edge cannot connect a parameter to itself.")
      index <- which(vapply(graph$edges, function(edge) edge$from == from && edge$to == to,
                            TRUE))
      if (op == "add_edge") {
        if (param_would_cycle(graph, from, to)) {
          stop("That edge would create a cycle: ", from, " -> ", to, ".")
        }
        uses <- param_edge_uses(change$uses)
        vars <- toupper(spec_vars(read_project_spec(p),ds)$Variable)
        valid <- function(code,var) var=="RECORDS" || var %in% vars
        for (u in uses) if (!valid(from,u$from_var) || !valid(to,u$to_var)) stop("Unknown variable in edge use: ",if(!valid(from,u$from_var))u$from_var else u$to_var,".")
        edge <- list(from = from, to = to, evidence = field_txt(change$evidence), origin = "user", uses=uses)
        if (length(index)) {
          graph$edges[[index[[1]]]] <- edge
        } else {
          graph$edges <- c(graph$edges, list(edge))
        }
      } else {
        if (!length(index)) stop("No edge from ", from, " to ", to, ".")
        graph$edges <- graph$edges[-index]
      }
    } else if (op %in% c("add_use", "remove_use")) {
      idx <- which(vapply(graph$edges,function(e)e$from==from && e$to==to,TRUE))
      if (!length(idx)) stop("Unknown parameter edge: ",from," -> ",to,".")
      use <- list(from_var=toupper(field_txt(change$from_var)),to_var=toupper(field_txt(change$to_var)))
      valid <- function(code,var) var=="RECORDS" || var %in% toupper(spec_vars(read_project_spec(p),ds)$Variable)
      if (!valid(from,use$from_var) || !valid(to,use$to_var)) stop("Unknown variable in edge use: ",if(!valid(from,use$from_var)) use$from_var else use$to_var,".")
      old <- param_edge_uses(graph$edges[[idx[[1]]]]$uses)
      match_use <- function(x) identical(x$from_var,use$from_var)&&identical(x$to_var,use$to_var)
      if (op=="add_use" && !any(vapply(old,match_use,TRUE))) old <- c(old,list(use))
      if (op=="remove_use") old <- Filter(function(x)!match_use(x),old)
      graph$edges[[idx[[1]]]]$uses <- old
    } else if (op == "add_undefined") {
      if (!grepl("^[A-Z][A-Z0-9_]*$", code)) stop("Invalid parameter code.")
      if (code %in% ids) stop("Parameter code already exists.")
      graph$nodes <- c(graph$nodes, list(list(code = code, label = field_txt(change$label),
                                              defined = FALSE, mentioned_in = list("user"))))
    } else if (op == "remove_undefined") {
      index <- which(vapply(graph$nodes, function(node) {
        identical(node$code, code) && !isTRUE(node$defined)
      }, TRUE))
      if (!length(index)) stop("Unknown undefined parameter code: ", code, ".")
      graph$nodes <- graph$nodes[-index]
      graph$edges <- Filter(function(edge) edge$from != code && edge$to != code, graph$edges)
    } else {
      stop("Unknown parameter graph change: ", op, ".")
    }
  }

  param_graph_write(p, graph)
  at <- now_iso()
  mem <- dag_agent_read(p, ds)
  correction_text <- paste("Applied parameter graph changes:", paste(vapply(changes, function(change) {
    paste(change$op, change$from %||% change$code %||% "", change$to %||% "")
  }, ""), collapse = "; "))
  mem$param_edits <- c(mem$param_edits %||% list(), list(list(at = at, changes = changes)))
  mem$param_corrections <- c(mem$param_corrections %||% list(),
                             list(list(at = at, text = correction_text)))
  if (length(mem$param_turns)) {
    index <- length(mem$param_turns)
    if (identical(mem$param_turns[[index]]$role, "agent")) {
      mem$param_turns[[index]]$applied <- TRUE
    }
  }
  dag_agent_write(p, ds, mem)
  list(ok = TRUE, graph = graph)
}

param_ask <- function(cfg, p, ds, message) {
  ds <- safe_ds(ds)
  graph <- param_graph_read(p, ds)
  if (is.null(graph)) stop("No parameter graph yet.")
  mem <- dag_agent_read(p, ds)
  message <- trimws(as.character(message %||% "")[1])
  if (!nzchar(message)) stop("message is required.")

  system <- paste(
    "TASK: param_ask",
    "You are the parameter dependency graph assistant. Suggest edits only; the user applies them separately.",
    "Edge from is needed to derive to. add_edge takes uses [{from_var,to_var}]. Also allow add_use and remove_use with from, to, from_var, to_var. Other operations: remove_edge, add_undefined, remove_undefined.",
    "Return JSON {\"answer\":\"\",\"changes\":[],\"correction\":null}.",
    sep = "\n"
  )
  thread <- ""
  if (length(mem$param_turns)) {
    turns <- tail(mem$param_turns, 10L)
    turn_lines <- vapply(turns, function(turn) {
      paste0(turn$role, ": ", dag_one_line(turn$text))
    }, "")
    thread <- paste0("THREAD_BEGIN\n", paste(turn_lines, collapse = "\n"), "\nTHREAD_END\n")
  }
  user <- paste0("PARAM_GRAPH_BEGIN\n", json_text(graph), "\nPARAM_GRAPH_END\n",
                 param_corrections_block(p, ds), thread,
                 "MESSAGE_BEGIN\n", message, "\nMESSAGE_END")
  result <- llm_chat(cfg, system, user, max_tokens = 1600L, json = TRUE)
  usage <- llm_usage_combine(list(llm_usage_summary(result$usage, cfg$provider %||% "")))
  parsed <- parse_json_reply(result$text)
  changes <- dag_normalize_changes(parsed$changes)
  at <- now_iso()
  mem$param_turns <- c(mem$param_turns, list(
    list(at = at, role = "user", text = message),
    list(at = at, role = "agent", text = parsed$answer %||% "", changes = changes,
         applied = FALSE, usage = usage)
  ))
  correction <- dag_one_line(parsed$correction)
  if (nzchar(correction) && tolower(correction) != "null") {
    mem$param_corrections <- c(mem$param_corrections,
                               list(list(at = at, text = correction)))
  } else {
    correction <- NULL
  }
  dag_agent_write(p, ds, mem)
  list(answer = parsed$answer %||% "", changes = changes, correction = correction,
       model = result$model %||% "", usage = usage, turns = dag_tail(mem$param_turns, 10L))
}

fake_param_reply <- function(system, user) {
  task <- fake_task(system)
  if (task == "param_extract") {
    edges <- list()
    param_lines <- Filter(function(line) startsWith(line, "PARAM\t"), strsplit(user, "\n", fixed=TRUE)[[1]])
    codes <- vapply(param_lines, function(line) strsplit(line, "\t", fixed=TRUE)[[1]][[2]], "")
    for (line in param_lines) {
      fields <- strsplit(line, "\t", fixed=TRUE)[[1]]
      if (length(fields) < 5L) next
      to <- fields[[2]]
      refs <- setdiff(codes, to)
      for (from in refs) if (grepl(paste0("\\b", from, "\\b"), fields[[5]], perl=TRUE))
        edges <- c(edges, list(list(from=from, to=to, evidence=paste(from, "source rows"), uses=list(list(from_var="AVAL",to_var="AVAL")))))
    }
    if (grepl("PARAM\\tBMI\\t", user)) {
      edges <- c(edges, list(list(from = "WEIGHT", to = "BMI", evidence = "WEIGHT result", uses=list(list(from_var="AVAL",to_var="AVAL"))),
                    list(from = "HEIGHT", to = "BMI", evidence = "HEIGHT result", uses=list(list(from_var="AVAL",to_var="AVAL")))))
    }
    if (grepl("PARAM\\tWTLS15DT\\t", user)) {
      edges <- c(edges, list(list(from = "WEIGHT", to = "WTLS15DT", evidence = "WEIGHT rows", uses=list(list(from_var="RECORDS",to_var="RECORDS")))))
    }
    undefined <- if (grepl("derived from FOO", user, fixed = TRUE)) list("FOO") else list()
    return(json_text(list(edges = edges, undefined = undefined)))
  }

  message <- text_block(user, "MESSAGE_BEGIN", "MESSAGE_END")
  use_match <- regexec("(ADD|REMOVE) USE\\s+([A-Z][A-Z0-9_]*)\\.([A-Z][A-Z0-9_]*)\\s*->\\s*([A-Z][A-Z0-9_]*)\\.([A-Z][A-Z0-9_]*)", toupper(message), perl=TRUE)
  use_hit <- regmatches(toupper(message),use_match)[[1]]
  if(length(use_hit)==6L) return(json_text(list(answer="I suggest updating that variable mapping.",changes=list(list(op=if(use_hit[[2]]=="ADD")"add_use" else "remove_use",from=use_hit[[3]],from_var=use_hit[[4]],to=use_hit[[5]],to_var=use_hit[[6]])),correction=NULL)))
  match <- regexec("ADD EDGE\\s+([A-Z][A-Z0-9_]*)\\s*(?:->|TO)\\s*([A-Z][A-Z0-9_]*)",
                   toupper(message), perl = TRUE)
  hit <- regmatches(toupper(message), match)[[1]]
  if (length(hit) == 3L) {
    return(json_text(list(
      answer = "I suggest adding that parameter dependency.",
      changes = list(list(op = "add_edge", from = hit[[2]], to = hit[[3]], uses=list(),
                          evidence = "User request")),
      correction = NULL
    )))
  }
  json_text(list(answer = "I can help edit parameter dependencies. Tell me which code depends on which.",
                 changes = list(), correction = NULL))
}

param_extract_job <- function(project_dir, dataset) {
  spec <- read_project_spec(project_dir)
  result <- param_extract(with_cfg(), spec, dataset, project = project_dir)
  if (nrow(spec_params(spec, dataset)))
    result$classification <- extract_classification(with_cfg(), spec, dataset, project_dir)
  result$carry_members <- as.list(vapply(Filter(function(x) identical(x$classification,"carry_over"), result$classification$parameters %||% list()), function(x) toupper(x$paramcd), ""))
  result
}

param_clear_manual_edits <- function(project, dataset) {
  mem <- dag_agent_read(project, dataset)
  mem$param_corrections <- list()
  mem$param_edits <- list()
  dag_agent_write(project, dataset, mem)
}

param_ask_job <- function(project_dir, dataset, message) {
  param_ask(with_cfg(), project_dir, dataset, message)
}

param_failure_graph <- function(project, dataset, spec, error) {
  graph <- param_graph_read(project, dataset)
  if (is.null(graph)) graph <- param_empty_graph(dataset)
  graph$dataset <- dataset
  graph$spec_hash <- spec$hash %||% NULL
  graph$nodes <- c(param_defined_nodes(spec, dataset), Filter(function(node) {
    !isTRUE(node$defined) && length(param_mentions(node$code, spec_params(spec, dataset),
                                                  spec_vars(spec, dataset))) > 0
  }, graph$nodes %||% list()))
  graph$error <- if (inherits(error, "condition")) conditionMessage(error) else as.character(error)[1]
  graph
}

start_param_extract <- function(A, ds, wait = FALSE) {
  ds <- safe_ds(ds)
  spec <- tryCatch(read_project_spec(A$project), error = function(e) NULL)
  params <- if (is.null(spec)) NULL else spec_params(spec, ds)
  if (is.null(params) || !nrow(params)) {
    graph <- param_empty_graph(ds, if (is.null(spec)) NULL else spec$hash)
    param_graph_write(A$project, graph)
    param_clear_manual_edits(A$project, ds)
    return(invisible(json_resp(graph)))
  }

  previous <- param_graph_read(A$project, ds)
  if (!is.null(previous) && length(previous$nodes)) {
    graph <- load_graph(A$project, ds)
    if (!is.null(graph)) for (node in graph$nodes %||% list()) {
      if (!is.null(node$paramcd)) {
        clear_node_results(A$project, ds, node$id)
        put_status(A$project, ds, node$id, blank_status(), save=FALSE)
      }
    }
    if (!is.null(graph)) unlink(graph_path(A$project, ds))
    write_state(A$project)
  }

  id <- job_start(A, "param_extract", ds, NULL)
  promise <- launch(A, "param_extract_job", list(project_dir = A$project$dir, dataset = ds),
                    llm_limit(A))
  done <- promises::then(
    promise,
    onFulfilled = function(graph) {
      job_finish(A, id)
      classification <- graph$classification %||% NULL
      graph$classification <- NULL
      param_graph_write(A$project, graph)
      if (!is.null(spec) && nrow(spec_params(spec, ds)) > 0L) {
        carry <- unique(intersect(toupper(flat_chr(graph$carry_members %||% character())),
                                  toupper(trimws(spec_params(spec, ds)$PARAMCD))))
        skeleton <- graph_skeleton(spec, ds, layer_keys_read(A$project, ds), carry)
        skeleton$generated_parameters <- list()
        if (!is.null(classification)) skeleton$extraction <- list(parameter_classification = classification)
        save_graph(A$project, skeleton)
      }
      param_clear_manual_edits(A$project, ds)
      json_resp(graph)
    },
    onRejected = function(error) {
      job_finish(A, id, error)
      graph <- param_failure_graph(A$project, ds, spec, error)
      param_graph_write(A$project, graph)
      json_resp(graph)
    }
  )
  remember(A, id, done)
  if (wait) done else invisible(done)
}
