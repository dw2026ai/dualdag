# Primary and validation code generation, and isolated callr runs (F9e, F9f).
# The two sides do not see each other's code or results. Both read the same uploaded xpt.

# Whole dataset spec. QC investigation still uses this as a stable system-prompt
# prefix. Side generation and Ask do not: those calls send only the target, its
# upstream chain, and the key rows.
dataset_spec_text <- function(spec, dataset) {
  if (is.null(spec) || is.null(dataset)) return("")
  ds <- toupper(as.character(dataset)[1])
  if (!nzchar(ds) || is.na(ds)) return("")
  vars <- spec_vars(spec, ds)
  params <- spec_params(spec, ds)
  if (!nrow(vars) && !nrow(params)) return("")
  cell <- function(x) {
    if (is.null(x) || length(x) != 1L || is.na(x)) return("")
    field_txt(x)
  }
  lines <- c(
    "DATASET_SPEC",
    paste0("DATASET: ", ds),
    "VARIABLES:",
    "Variable\tLabel\tType\tLength\tKey\tSource\tDerivation"
  )
  if (nrow(vars)) {
    for (i in seq_len(nrow(vars))) {
      lines <- c(lines, paste(
        cell(vars$Variable[[i]]), cell(vars$Label[[i]]), cell(vars$Type[[i]]),
        cell(vars$Length[[i]]), cell(vars$Key[[i]]),
        cell(vars$Source[[i]]), cell(vars$Derivation[[i]]),
        sep = "\t"
      ))
    }
  }
  if (nrow(params)) {
    lines <- c(lines, "PARAMETERS:", "PARAMCD\tPARAM\tSource\tDerivation")
    for (i in seq_len(nrow(params))) {
      lines <- c(lines, paste(
        cell(params$PARAMCD[[i]]), cell(params$PARAM[[i]]),
        cell(params$Source[[i]]), cell(params$Derivation[[i]]),
        sep = "\t"
      ))
    }
  }
  paste(lines, collapse = "\n")
}

side_role_line <- function(side, dataset = NULL) {
  ds <- trimws(as.character(dataset %||% "")[1])
  if (!nzchar(ds) || is.na(ds)) ds <- "this dataset" else ds <- toupper(ds)
  paste0("You are the ", side, "-side R expert for dataset ", ds, ".")
}

side_temp_name <- function(dataset, node = NULL, paramcd = NULL) {
  node <- if (!is.null(node)) node_record(node) else NULL
  pc <- paramcd %||% if (!is.null(node)) node$paramcd else NULL
  ds <- toupper(trimws(as.character(dataset %||% if (!is.null(node)) node$dataset else "DATASET")[1]))
  if (is.null(pc) || !nzchar(as.character(pc)[1]) || is.na(pc)) return(paste0(ds,"_TEMP"))
  safe_pc <- gsub("[^A-Za-z0-9_.]", "_", toupper(trimws(as.character(pc)[1])), perl=TRUE)
  paste0(ds,"_",safe_pc,"_TEMP")
}

side_lineage_name <- function(dataset, node = NULL, paramcd = NULL) {
  node <- if (!is.null(node)) node_record(node) else NULL
  pc <- paramcd %||% if (!is.null(node)) node$paramcd else NULL
  ds <- toupper(trimws(as.character(dataset %||% if (!is.null(node)) node$dataset else "DATASET")[1]))
  if (!is.null(pc) && nzchar(as.character(pc)[1]) && !is.na(pc)) {
    code <- gsub("[^A-Za-z0-9_.]", "_", toupper(trimws(as.character(pc)[1])), perl=TRUE)
    paste0("LINEAGE_",code)
  } else paste0("LINEAGE_",ds)
}

side_program_name <- function(dataset, node = NULL, side = "validation") {
  nr <- if (is.null(node)) NULL else node_record(node)
  if (!is.null(nr) && identical(toupper(nr$paramcd %||% ""), "CARRY_OVER"))
    return(paste0(toupper(dataset), "_CARRY_OVER_TEMP"))
  if (identical(side,"primary")) side_lineage_name(dataset,node) else side_temp_name(dataset,node)
}

side_layer_name <- function(dataset, paramcd, side) {
  if (identical(toupper(as.character(paramcd)[1]), "CARRY_OVER")) return(paste0(toupper(dataset), "_CARRY_OVER_TEMP"))
  if (identical(side,"primary")) side_lineage_name(dataset,paramcd=paramcd) else side_temp_name(dataset,paramcd=paramcd)
}

side_source_name <- function(domain, side) {
  if (identical(side,"primary")) paste0("LINEAGE_SRC_",toupper(domain)) else paste0("src$",domain)
}

side_is_keys_step <- function(node, keys) {
  node <- node_record(node)
  identical(node$kind,"records") || toupper(node$var %||% "") %in% toupper(keys)
}

expected_in_temp_columns <- function(spec, graph, node, dataset = NULL, side = "validation") {
  node <- node_record(node)
  ds <- toupper(as.character(dataset %||% parse_node_id(node$id)$dataset)[1])
  target <- toupper(as.character(node$var %||% "")[1])
  ids <- if (exists("expert_ancestor_ids", mode = "function")) expert_ancestor_ids(graph, node$id) else character()
  nodes <- graph$nodes %||% list()
  ancestors <- vapply(ids, function(id) {
    hit <- Filter(function(n) identical(as.character(n$id %||% ""), id), nodes)
    if (!length(hit) || identical(hit[[1]]$kind, "records")) return("")
    parsed <- parse_node_id(id)
    if (is.null(parsed) || !identical(parsed$dataset, ds)) return("")
    toupper(as.character(hit[[1]]$var %||% parsed$var)[1])
  }, "")
  vars <- spec_vars(spec, ds)
  row <- if (nrow(vars)) which(toupper(vars$Variable) == target) else integer()
  refs <- character()
  if (length(row) && "Source" %in% names(vars) && exists("expert_ref_tokens", mode = "function")) {
    hits <- expert_ref_tokens(vars$Source[[row[[1]]]])
    refs <- sub("^[^.]*\\.", "", hits[sub("\\..*$", "", hits) == ds])
  }
  expected <- if (identical(side, "primary")) ancestors[nzchar(ancestors)] else unique(c(ancestors[nzchar(ancestors)], refs[nzchar(refs)]))
  expected[expected != target]
}

inherited_opening_block <- function(temp_name, expected, side) {
  expected <- unique(as.character(expected %||% character()))
  expected <- expected[nzchar(expected)]
  expected_expr <- if (length(expected)) paste0("c(", paste(vapply(expected, json_text, ""), collapse = ", "), ")") else "c()"
  miss_check <- if (identical(side, "primary")) {
    paste0('if (length(missing)) stop("DUALDAG_MISSING_COLUMN: Missing column: ", missing[[1]], " is not in ', temp_name, ' or the provided data.")')
  } else {
    'if (length(missing)) cat("missing expected columns:", paste(missing, collapse = ", "), "\\n")'
  }
  paste0('cat("inherited rows:", nrow(', temp_name, '), "\\n")\n',
    'cat("inherited columns:", paste(names(', temp_name, '), collapse = ", "), "\\n")\n',
    'expected <- ', expected_expr, '\n',
    'missing <- setdiff(expected, names(', temp_name, '))\n', miss_check, "\n")
}

side_explicit_spec_source_refs <- function(spec, dataset, node, src) {
  node <- node_record(node)
  texts <- character()
  vars <- spec_vars(spec,dataset)
  vrow <- which(toupper(vars$Variable)==toupper(node$var %||% ""))
  if (length(vrow)) texts <- c(texts,as.character(vars$Source[vrow[[1]]]),as.character(vars$Derivation[vrow[[1]]]))
  if (!is.null(node$paramcd)) {
    params <- spec_params(spec,dataset)
    prow <- which(toupper(trimws(params$PARAMCD))==toupper(trimws(node$paramcd)))
    if (length(prow)) texts <- c(texts,as.character(params$Source[prow[[1]]]),as.character(params$Derivation[prow[[1]]]))
  }
  blob <- toupper(paste(texts,collapse="\n"))
  if (!nzchar(blob)) return(character())
  loc <- gregexpr("\\b([A-Z][A-Z0-9_]*)\\.([A-Z][A-Z0-9_]*)\\b",blob,perl=TRUE)[[1]]
  if (identical(loc,-1L)) return(character())
  hits <- regmatches(blob,list(loc))[[1]]
  out <- character()
  src_names <- toupper(names(src) %||% character())
  for (i in seq_along(hits)) {
    ref <- hits[[i]]; dom <- sub("\\..*$","",ref); var <- sub("^[^.]*\\.","",ref)
    if (identical(dom,toupper(dataset))) next
    at <- match(dom,src_names)
    if (is.na(at)) next
    columns <- toupper(names(src[[at]]))
    if (var %in% columns) out <- c(out,paste0(dom,".",var))
  }
  unique(out)
}

side_primary_source_refs <- function(spec, dataset, keys, lineage, node, src) {
  key_refs <- character()
  if (is.data.frame(spec$variables) && nrow(spec$variables) && all(c("Dataset", "Variable", "Key") %in% names(spec$variables))) {
    kv <- spec$variables[toupper(as.character(spec$variables$Dataset)) == toupper(dataset) &
      toupper(as.character(spec$variables$Variable)) %in% toupper(keys), , drop = FALSE]
    if (nrow(kv)) key_refs <- expert_source_pairs(c(kv$Source, kv$Derivation), dataset)
  }
  unique(c(lineage$source_refs, key_refs, side_explicit_spec_source_refs(spec,dataset,node,src)))
}

side_filter_source_frames <- function(src, dataset, side, source_refs, keys) {
  if (!identical(side, "primary")) return(src)
  domains <- unique(sub("\\..*$", "", source_refs))
  if (!identical(toupper(dataset), "ADSL") && "ADSL" %in% toupper(names(src) %||% character())) domains <- unique(c(domains, "ADSL"))
  joins <- unique(c("STUDYID", "USUBJID", keys))
  src <- src[intersect(names(src), names(src)[toupper(names(src)) %in% toupper(domains)])]
  for (dom in names(src)) {
    # Primary programs may always consult the complete ADSL source when
    # building a non-ADSL dataset. This does not change lineage or cards.
    if (!identical(toupper(dataset), "ADSL") && identical(toupper(dom), "ADSL")) next
    keep <- unique(c(sub("^[^.]*\\.", "", source_refs[startsWith(source_refs, paste0(dom, "."))]),
      intersect(joins, names(src[[dom]]))))
    src[[dom]] <- src[[dom]][, intersect(names(src[[dom]]), keep), drop = FALSE]
  }
  src
}

side_available_datasets_block <- function(dataset, node, current_frame, layers, src, side) {
  node <- node_record(node)
  source_remaining <- 4000L
  add_frame <- function(name, status, frame, source = FALSE) {
    if (!is.data.frame(frame)) return(character())
    cols <- paste(names(frame), collapse = ", ")
    truncated <- source && nchar(cols, type = "chars") > source_remaining
    if (source) {
      if (truncated) {
        cols <- substr(cols, 1L, source_remaining)
        source_remaining <<- 0L
      } else source_remaining <<- source_remaining - nchar(cols, type = "chars")
    }
    if (truncated) cols <- paste0(cols, " [source column names truncated at 4000 characters]")
    paste0("- ", name, " (", status, "; ", nrow(frame), " rows): ", cols)
  }
  entries <- add_frame(side_program_name(dataset, node, side), "current", current_frame)
  if (length(layers)) entries <- c(entries, vapply(names(layers), function(code)
    add_frame(side_layer_name(dataset,code,side), "finished", layers[[code]]), ""))
  if (length(src)) entries <- c(entries, vapply(names(src), function(domain)
    add_frame(side_source_name(domain,side), "source", src[[domain]], source=TRUE), ""))
  paste(c("AVAILABLE_DATASETS (already loaded in this program; choose what is useful):", entries), collapse = "\n")
}

side_system_prompt <- function(side, spec = NULL, dataset = NULL, packages = NULL, node = NULL, project = NULL, chat = FALSE, graph = NULL) {
  side <- match.arg(side, c("primary", "validation"))
  # `spec` is unused. The whole-spec prefix stayed on QC and extraction only,
  # so this system prompt is stable across every variable of one side.
  ds <- toupper(trimws(as.character(dataset %||% "DATASET")[1]))
  if (!nzchar(ds) || is.na(ds)) ds <- "DATASET"
  temp <- side_program_name(ds,node,side)
  nr <- if (is.null(node)) NULL else node_record(node)
  key_names <- if (is.null(spec) || is.null(nr)) character() else spec_key_names(spec,ds)
  if (!is.null(nr) && identical(nr$kind,"records")) {
    if (!is.null(nr$paramcd) && exists("layer_key_names",mode="function")) {
      key_names <- layer_key_names(spec,ds,nr$paramcd,graph,project)
    } else if (!is.null(project) && exists("layer_keys_path",mode="function") && file.exists(layer_keys_path(project,ds))) {
      key_names <- layer_key_names(spec,ds,"*",graph,project)
    }
  }
  keys_step <- !is.null(nr) && side_is_keys_step(nr,key_names)
  layered_node <- !is.null(nr) && (!is.null(nr$paramcd) || identical(nr$kind,"dataset_variable"))
  layered_keys_step <- keys_step && layered_node
  records_node <- !is.null(nr) && identical(nr$kind, "records")
  key_check <- if (records_node) {
    key_rows <- vapply(key_names, function(key) paste0(
      'cat("', key, ' type:", typeof(', temp, '[["', key, '"]]), " NA:", sum(is.na(', temp, '[["', key, '"]])), " distinct:", length(unique(', temp, '[["', key, '"]])), "\\n")\n',
      'if (is.numeric(', temp, '[["', key, '"]]) || inherits(', temp, '[["', key, '"]], "Date")) print(summary(', temp, '[["', key, '"]])) else if (is.character(', temp, '[["', key, '"]]) || is.factor(', temp, '[["', key, '"]]) || is.logical(', temp, '[["', key, '"]])) { if (length(unique(', temp, '[["', key, '"]])) <= 20L) print(table(', temp, '[["', key, '"]], useNA = "ifany")) else cat("', key, ': more than 20 distinct values\\n") }'
    ), "")
    paste0('cat("rows:", nrow(', temp, '), "\\n")\n',
      'cat("duplicate key rows:", sum(duplicated(', temp, '[, c(', paste(vapply(key_names,json_text,""),collapse=","), '), drop = FALSE]) | duplicated(', temp, '[, c(', paste(vapply(key_names,json_text,""),collapse=","), '), drop = FALSE], fromLast = TRUE)), "\\n")\n',
      paste(key_rows, collapse="\n"))
  } else NULL
  temp_contract <- if (layered_keys_step) {
    paste0("The Records step creates its own ",temp," with the key columns from whichever AVAILABLE_DATASETS fit. This is a new dataset for this parameter; use the available frames that fit the specification.")
  } else if (keys_step && identical(side,"primary")) {
    paste0("Create ",temp," with the key columns from whichever LINEAGE_ datasets fit.")
  } else if (keys_step) {
    paste0("The Keys step does not inherit a dataset. Create ",temp," from source domains with exactly the key columns.")
  } else {
    paste0("- ",temp,": the current dataset for this side. Every non-keys step starts with this data.frame already present, containing the rows and columns inherited by this step. It is not missing: its key columns and all finished variables are already in it. Add only the target column and leave the dataset under this same name; do not rebuild rows or finished variables from source.")
  }
  carry_group_node <- !is.null(nr) && identical(toupper(nr$paramcd %||% ""),"CARRY_OVER")
  objects_line <- if (identical(side,"primary")) {
    if (carry_group_node) paste0("Build from lineage variables first. A needed column present in names(",temp,") but outside DAG lineage may be used; the app shows an amber warning, not an error or stop. Do not print the app's warning in the program or reply. Only stop for a needed column absent from the frame and provided source data. The output goes only to the shared group dataset ",temp,".") else
      "Build from lineage variables first. A needed column present in names(LINEAGE_<P>) but outside DAG lineage may be used; the app shows an amber warning, not an error or stop. Do not print the app's warning in the program or reply. Only stop for a needed column absent from the frame and provided source data, with Missing column: X is not in LINEAGE_SYSBP or the provided data. The output goes only to your own LINEAGE_ dataset."
  } else "The program runs in a fresh R process. These objects already exist:\n- src: named list of data.frames. Names are uppercase domain codes (DM, VS, ADSL). Use src only to look up values, for example SITEID from DM by USUBJID."
  contract <- paste(
    if (identical(side,"primary")) "The program runs in a fresh R process." else objects_line,
    if (identical(side,"primary")) objects_line else "",
    temp_contract,
    "- keys: character vector of key variable names.",
    if (layered_node || identical(side,"primary")) "The user prompt's AVAILABLE_DATASETS block lists this side's current, finished, and source datasets with row counts and column names only. Use the available objects that fit the specification." else "",
    if (layered_keys_step) paste0("A Records step creates its own parameter dataset with key columns from whichever AVAILABLE_DATASETS fit. Every variable step adds its target column to its existing parameter dataset. At the end, ", temp, " holds this step's result.") else if (layered_node) paste0("Every other step adds its target column to the existing ", temp, ". At the end, ", temp, " holds the result.") else if (identical(side,"primary")) paste0("The Keys step creates ",temp," with its key columns from whichever LINEAGE_ datasets fit. Every other step adds its target column to the existing ",temp,".") else paste0("The Keys step creates ", temp, " from source domains with the key columns. Every other step adds its target column to the existing ", temp, ". At the end, ", temp, " holds the result."),
    if (!keys_step && !chat) paste0("For every non-keys step, this program is one step in a chain: earlier steps on this side already built ", temp, ". Do not start over from the specification or rebuild rows; add only the target variable. Ancestor information is background about where values come from. ", temp, " is the actual inherited data and can contain more variables than those ancestors. Start the program, after note comments, with the REQUIRED_PROGRAM_OPENING block from the user prompt, unchanged.") else if (!keys_step) paste0("For every non-keys step, this program is one step in a chain: earlier steps on this side already built ", temp, ". Do not start over from the specification or rebuild rows; add only the target variable. Ancestor information is background about where values come from. ", temp, " is the actual inherited data and can contain more variables than those ancestors.") else "",
    "User instructions override the specification and this side's default strategy. They are mandatory.",
    "When the user message lists notes, the code must start with a short R comment for each note saying how that note was applied. If notes conflict, the latest note wins.",
    if (!chat && !records_node) paste0("End every generated R program with this ordinary R check-output block, replacing X with the target variable name. Put it after ", temp, " is updated, with no code after it:") else "",
    if (!chat && !records_node) paste0('cat("rows:", nrow(', temp, '), "\\n")') else "",
    if (!chat && !records_node) paste0('if (is.numeric(', temp, '$X) || inherits(', temp, '$X, "Date")) {') else "",
    if (!chat && !records_node) paste0('  print(summary(', temp, '$X))') else "",
    if (!chat && !records_node) paste0('} else if (is.character(', temp, '$X) || is.factor(', temp, '$X) || is.logical(', temp, '$X)) {') else "",
    if (!chat && !records_node) paste0('  if (length(unique(', temp, '$X)) <= 20L) {') else "",
    if (!chat && !records_node) paste0('    print(table(', temp, '$X, useNA = "ifany"))') else "",
    if (!chat && !records_node) '  } else {' else "",
    if (!chat && !records_node) '    cat("X: more than 20 distinct values\\n")' else "",
    if (!chat && !records_node) '  }' else "",
    if (!chat && !records_node) '}' else "",
    if (!chat && !records_node) paste0("The check prints the row count and a summary with NA count for numeric or Date targets; for character, factor, or logical targets it prints a table including NA when there are at most 20 distinct values, otherwise the one-line count limit message. Do not add diagnostics outside this block. The program ends with ", temp, " holding the result.") else "",
    if (!chat && !(isTRUE(graph$layered) && identical(nr$kind, "param_variable"))) "Return a JSON object {\"code\":\"<R script>\",\"same_source\":true|false,\"description\":\"2-3 sentences on meaning, type/units, source/rule, and missingness\"}. No prose outside the JSON." else if (!chat) paste0("Before writing code, answer the HANDLING_QUESTION in the user prompt.\nReturn a JSON object with these fields, in this order:\n{\"approach\":\"<your answer>\",\"code\":\"<R script>\",\"same_source\":true|false,\"description\":\"2-3 sentences on meaning, type/units, source/rule, and missingness\"}\nThe code must do what approach says. No prose outside the JSON.") else "",
    sep = "\n"
  )
  if (records_node) {
    lines <- strsplit(contract,"\n",fixed=TRUE)[[1]]
    json_line <- grep("^Return a JSON object",lines,value=TRUE)
    lines <- lines[!grepl("^Return a JSON object|^For the Keys step,|\\$X|X: more than 20 distinct values|ordinary R check-output block|The check prints the row count",lines)]
    lines <- lines[nzchar(lines)]
    key_prefix <- c("VAR: KEYS",paste0("KEYS: ",paste(key_names,collapse=",")))
    if (chat) {
      contract <- paste(c(lines,key_prefix),collapse="\n")
    } else {
      lead_in <- "For the Keys step, use this key-column check block after the result is updated; no code follows it:"
      description <- paste0("The Keys check prints the row count, duplicate key row count, and each key column's type, NA count, distinct count, and the existing summary or table for that key column, then leaves ", temp, " holding the key columns.")
      contract <- paste(c(lines,key_prefix,lead_in,key_check,description,json_line),collapse="\n")
    }
  }
  role <- if (chat) paste0("You are the ", side, "-side R coder for dataset ", ds, ".") else side_role_line(side, dataset)
  body <- if (identical(side, "primary")) {
    paste(
      role,
      if (!chat) "TASK: generate_primary" else "",
      if (!chat) "You write the primary (production) R program for one ADaM variable." else "",
      "Follow the specification literally. Follow its Source and Derivation step by step, in the order the spec states them.",
      "Take each value from exactly the dataset and variable the spec names, even when the same value is already available elsewhere.",
      "Do not shortcut, simplify, or reinterpret the logic. Do not invent an alternative source.",
      "If the spec is ambiguous or seems wrong, still follow its wording and say so in the top comment.",
      "A user note overrides the specification only for what that note says.",
      "Build from lineage variables first, using the LINEAGE_ datasets listed in the user prompt. Explicit source references in this node's own Source or Derivation are included there.",
      "A needed column present in names(LINEAGE_<P>) but outside the DAG lineage may be used when the rule needs it; the app warns. It is not an error and must not cause a stop or a DUALDAG_MISSING_COLUMN stop. Do not print the app's warning in code or reply. Only stop when a needed column is absent from names(LINEAGE_<P>) and the provided source data.",
      contract,
      sep = "\n"
    )
  } else {
    paste(
      role,
      if (!chat) "TASK: generate_validation" else "",
      if (!chat) "You write an independent validation R program for one ADaM variable." else "",
      "Reason independently from the variable's meaning. Prefer an alternative source when one can produce the same information.",
      "The primary program is not shown to you. Do not ask for it and do not reproduce it.",
      "If only one source exists, rewrite from that same source and set same_source to true. Otherwise set same_source to false.",
      "The target lineage and spec provide context. Use your independent understanding of the variable meaning and may use any available source domain or built same-side variable.",
      contract,
      sep = "\n"
    )
  }
  base <- paste(body,
    if (identical(side, "primary")) 'For a genuinely absent needed column, check names(LINEAGE_<P>) and the provided source data, then stop with the user-facing form: "Missing column: X is not in LINEAGE_SYSBP or the provided data." Substitute the actual frame and column names. Do not stop for a column present in the LINEAGE_ frame merely because it is outside the DAG lineage.' else "",
    r_packages_rule(packages), sep = "\n")
  extra <- if (exists("dag_lookup_rule", mode = "function")) dag_lookup_rule() else ""
  if (nzchar(extra)) paste(base, extra, sep = "\n") else base
}

side_generation_system_prompt <- function(side, spec, graph, node, project = NULL, chat = FALSE) {
  node <- node_record(node)
  graph <- if (is.null(graph)) empty_graph(parse_node_id(node$id)$dataset %||% "") else normalize_graph(graph)
  ds <- graph$dataset %||% parse_node_id(node$id)$dataset
  keys <- if (is.null(spec)) character() else spec_key_names(spec, ds)
  if (isTRUE(graph$layered) && !is.null(node$paramcd)) keys <- layer_key_names(spec,ds,node$paramcd,graph,project)
  system <- side_system_prompt(side, spec, ds, packages = side_r_packages(project, side), node = node, project = project, chat = chat, graph = graph)
  if (!chat && isTRUE(graph$layered) && identical(node$kind, "dataset_variable")) system <- paste(system,
    "This is a dataset-level variable. Build exactly one row per USUBJID from source and return USUBJID plus the target variable.", sep = "\n")
  if (identical(node$kind, "records")) {
    keys <- spec_key_names(spec, ds)
    if (isTRUE(graph$layered) && !is.null(node$paramcd)) keys <- layer_key_names(spec, ds, node$paramcd, graph, project)
    if (!isTRUE(graph$layered) && !is.null(project) && exists("layer_keys_path", mode="function") && file.exists(layer_keys_path(project,ds))) keys <- layer_key_names(spec,ds,"*",graph,project)
    wording <- if (isTRUE(graph$layered)) "This is the Keys step for one parameter layer. Return these key columns and PARAMCD, with one row per record; do not return analysis variables." else "This is the dataset Keys step. Return exactly these compound key columns, one row per record, and no other columns."
    system <- paste(system, wording, paste0("Key columns: ", paste(keys, collapse=", "), "."), sep="\n")
    system <- gsub("Records", "Keys", system, fixed=TRUE)
    system <- gsub("RECORDS", "KEYS", system, fixed=TRUE)
  }
  if (!chat && isTRUE(graph$layered) && identical(node$kind, "param_variable")) system <- paste(system,
    "This variable belongs to one parameter layer. Return only the key columns and the target variable, with one program dedicated to this parameter-variable pair.", sep = "\n")
  system
}

node_record <- function(node, graph = NULL) {
  if (is.list(node) && !is.null(node$id)) return(node)
  pt <- parse_node_id(node)
  resolved <- if (!is.null(graph)) graph_node(graph, pt$id) else NULL
  if (!is.null(resolved)) return(resolved)
  list(id = pt$id, var = pt$var, paramcd = pt$paramcd, kind = if (is.null(pt$paramcd)) "variable" else "param_variable")
}

handling_approach_text <- function(x) {
  if (is.null(x)) return("")
  tryCatch({
    if (is.list(x) || is.expression(x)) x <- unlist(x, recursive=TRUE, use.names=FALSE)
    x <- as.character(x)
    x <- x[!is.na(x) & nzchar(trimws(x))]
    paste(x, collapse=" ")
  }, error=function(e) "")
}

lineage_upstream_ref_allowed <- function(ref, node, allowed_ids) {
  allowed_ids <- as.character(allowed_ids %||% character())
  if (ref %in% allowed_ids) return(TRUE)
  paramcd <- node_record(node)$paramcd %||% NULL
  if (is.null(paramcd) || !nzchar(as.character(paramcd)[1])) return(FALSE)
  paste0(toupper(ref), "@", toupper(as.character(paramcd)[1])) %in%
    toupper(allowed_ids)
}

spec_rows_text <- function(spec, dataset, var) {
  v <- spec_vars(spec, dataset)
  if (!nrow(v)) return("")
  hit <- toupper(v$Variable) == toupper(var)
  if (!any(hit)) return("")
  i <- which(hit)[1]
  paste0(
    "ROW: ", v$row[[i]] %||% "",
    "\nLABEL: ", field_txt(v$Label[[i]]),
    "\nTYPE: ", field_txt(v$Type[[i]]),
    "\nLENGTH: ", v$Length[[i]] %||% "",
    "\nSOURCE: ", field_txt(v$Source[[i]]),
    "\nDERIVATION: ", field_txt(v$Derivation[[i]]),
    if ("Notes" %in% names(v)) paste0("\nNOTES: ", field_txt(v$Notes[[i]])) else ""
  )
}

side_user_prompt <- function(side, spec, graph, node, upstream_vars,
                             qc_feedback = NULL, user_instruction = NULL, catalog = NULL,
                             project = NULL, current_code = NULL, notes = NULL, self_review = NULL, chat = FALSE) {
  side <- match.arg(side, c("primary", "validation"))
  node <- node_record(node)
  graph <- if (is.null(graph)) empty_graph(node$dataset %||% "") else normalize_graph(graph)
  ds <- graph$dataset %||% parse_node_id(node$id)$dataset
  keys <- spec_key_names(spec, ds)
  if (isTRUE(graph$layered) && !is.null(node$paramcd)) keys <- layer_key_names(spec, ds, node$paramcd, graph, project)
  if (!isTRUE(graph$layered) && !is.null(project) && exists("layer_keys_path",mode="function") && file.exists(layer_keys_path(project,ds))) keys <- layer_key_names(spec,ds,"*",graph,project)
  if (isTRUE(graph$layered) && identical(node$kind,"dataset_variable")) keys <- "USUBJID"
  pkg_block <- r_packages_prompt(side_r_packages(project, side))
  ups <- flat_chr(upstream_vars)
  ev <- character()
  for (e in graph$edges) if (identical(e$to, node$id)) ev <- c(ev, paste0(e$from, " | ", e$evidence %||% ""))
  ext <- character()
  for (e in graph$external) {
    used <- flat_chr(e$used_by)
    if (node$id %in% used) ext <- c(ext, paste0(e$ref %||% "", " | ", e$evidence %||% ""))
  }
  lineage <- lineage_context(spec, graph, node, project, side, catalog)
  if (identical(side,"primary")) lineage$text <- gsub("WORK_KEYS:","LINEAGE_KEYS:",lineage$text,fixed=TRUE)
  visible_layers <- list()
  if (isTRUE(graph$layered) && !is.null(node$paramcd) && !is.null(project)) {
    visible_layers <- tryCatch(finished_layers_for_side(project, ds, side, graph, spec, node$paramcd), error=function(e) list())
  }
  param_row <- if (identical(side,"validation") || isTRUE(graph$layered)) {
    if (identical(toupper(node$paramcd %||% ""),"CARRY_OVER")) {
      layer <- Filter(function(x) identical(toupper(x$paramcd %||% ""),"CARRY_OVER"), as_records(graph$layers %||% list()))
      members <- if (length(layer)) flat_chr(layer[[1]]$members_parameters) else character()
      rows <- vapply(members, function(pc) spec_param_row_text(spec,ds,pc), "")
      paste(rows[nzchar(rows)], collapse="\n\n")
    } else spec_param_row_text(spec,ds,node$paramcd)
  } else ""
  group_context <- ""
  if (isTRUE(graph$layered) && !is.null(project) && !identical(toupper(node$paramcd %||% ""), "CARRY_OVER")) {
    pg <- tryCatch(param_graph_read(project, ds), error=function(e) NULL)
    carry_members <- unique(toupper(flat_chr(pg$carry_members %||% character())))
    if (length(carry_members)) group_context <- paste0(
      "CARRY_OVER_MEMBERS: ", paste(carry_members, collapse=", "), "\n",
      "CARRY_OVER_GROUP: members ", paste(carry_members, collapse=", "),
      " share layers[['CARRY_OVER']] and ", toupper(ds), "_CARRY_OVER_TEMP. ",
      "When a source names one of these member parameters, read the complete group and filter its PARAMCD rows; no member-named layer exists.\n")
  }
  extra <- ""
  frame <- if (is.null(project)) NULL else tryCatch(side_prompt_frame(project, ds, side, spec, graph, node), error=function(e) NULL)
  facts <- list(rows=if(is.data.frame(frame)) nrow(frame) else 0L, columns=if(is.data.frame(frame)) names(frame) else character())
  n <- facts$rows
  is_key <- isTRUE(toupper(node$var %||% "") %in% toupper(keys))
  keys_step <- side_is_keys_step(node,keys)
  temp_name <- side_program_name(ds,node,side)
  layered_node <- isTRUE(graph$layered)
  available_block <- side_available_datasets_for_prompt(project,ds,side,spec,graph,node,
    current_frame=if(is.data.frame(frame))frame else data.frame(),layers=visible_layers,lineage=lineage,keys=keys)
  side_rule <- if (identical(side,"primary")) "Build from lineage variables first. A needed column already present in this LINEAGE_ frame may be used even when it is outside the DAG lineage; the app warns, and this is not an error. Do not print the app's warning. Stop only when a needed column is absent from names(this LINEAGE_ frame) and the provided source data." else "Validation may use any built column on this side."
  expected_cols <- if (keys_step) character() else expected_in_temp_columns(spec, graph, node, ds, side)
  vars_label <- if (identical(side,"primary")) "LINEAGE_INPUT_VARS" else "UPSTREAM_VARS"
  dataset_label <- if (identical(side,"primary")) "LINEAGE_DATASET" else "TEMP_DATASET"
  rows_label <- if (identical(side,"primary")) "LINEAGE_FRAME_ROWS" else "TEMP_FRAME_ROWS"
  columns_label <- if (identical(side,"primary")) "LINEAGE_FRAME_COLUMNS" else "TEMP_FRAME_COLUMNS"
  expected_label <- if (identical(side,"primary")) "EXPECTED_IN_LINEAGE" else "EXPECTED_IN_TEMP"
  extra <- paste0(
    "IS_KEY: ", if (is_key) "true" else "false", "\n",
    if(identical(side,"primary"))"LINEAGE_ROWS: "else"WORK_ROWS: ", n, "\n",
    dataset_label, ": ", temp_name, "\n",
    rows_label, ": ", n, "\n",
    columns_label, ": ", paste(facts$columns, collapse=","), "\n",
    if (!keys_step && !chat) paste0(expected_label, ": ", paste(expected_cols, collapse=", "), "\n",
      "Your task is only the target variable in its spec row. These expected columns should already be done; do not rebuild them.\n",
      "REQUIRED_PROGRAM_OPENING_BEGIN\n", inherited_opening_block(temp_name, expected_cols, side), "\nREQUIRED_PROGRAM_OPENING_END\n") else "",
    if (keys_step && layered_node) paste0("Create ",temp_name," with the key columns from whichever available datasets fit.\n") else if (keys_step && identical(side,"primary")) paste0("Create ",temp_name," with the key columns from whichever LINEAGE_ datasets fit.\n") else if (keys_step) paste0("The Keys step does not inherit an existing dataset. Build ",temp_name," from the source domains with its key columns.\n") else paste0("This program is one step in a chain; earlier steps on this side already built ",temp_name,". Do not start over from the spec or rebuild rows; add only the target variable. The ancestor information above is background on where values come from. ",temp_name," is the actual inherited frame and may contain more variables than those ancestors. Its keys and finished variables are present; it is not missing. ",side_rule,"\n"),
    if (layered_node || identical(side,"primary")) paste0(available_block,"\n") else "",
    if (identical(side,"primary")) paste0("Build from the LINEAGE_ datasets listed above first. Do not put outside-lineage columns in EXPECTED_IN_LINEAGE or the opening block check. A needed column present in names(",temp_name,") may be used; the app warns, and this is not a reason to stop. Do not print the app's warning. Only stop with the specified Missing column wording when the needed column is absent from the frame and provided source data. Write output only to ",if(identical(toupper(node$paramcd %||% ""),"CARRY_OVER"))"the shared group dataset "else"your LINEAGE_ dataset ",temp_name,".\n") else ""
  )
  code_block <- ""
  if (is.character(current_code) && length(current_code) >= 1L) {
    code_txt <- paste(current_code, collapse = "\n")
    if (nzchar(code_txt)) code_block <- paste0("CURRENT_CODE_BEGIN\n", code_txt, "\nCURRENT_CODE_END\n")
  }
  notes_block <- ""
  if (is.list(notes) && length(notes)) {
    lines <- character()
    for (i in seq_along(notes)) {
      tx <- notes[[i]]$text %||% ""
      tx <- trimws(gsub("[\r\n]+", " ", as.character(tx)[1]))
      if (nzchar(tx)) lines <- c(lines, paste0(i, ". ", tx))
    }
    if (length(lines)) notes_block <- paste0("NOTES_BEGIN\n", paste(lines, collapse = "\n"), "\nNOTES_END\n")
  }
  review_block <- ""
  if (is.list(self_review) && length(self_review)) review_block <- paste0(
    "SELF_REVIEW_BEGIN\nPREVIOUS_CODE:\n", as.character(self_review$code %||% ""),
    "\nISSUES:\n", paste(unlist(self_review$issues %||% list()), collapse = "\n"),
    if (!is.null(self_review$error)) paste0("\nPREVIOUS_PROGRAM_ERROR:\n", self_review$error) else "",
    "\nSELF_REVIEW_END\n")
  layer_block <- ""
  if (isTRUE(graph$layered) && !is.null(node$paramcd)) {
    if (identical(node$kind, "records")) {
      kv <- spec_vars(spec, ds); kv <- kv[toupper(kv$Variable) %in% keys,,drop=FALSE]
      key_rows <- if (nrow(kv)) paste(vapply(seq_len(nrow(kv)), function(i) paste("KEY",kv$Variable[[i]],field_txt(kv$Source[[i]]),field_txt(kv$Derivation[[i]]),sep="\t"), ""), collapse="\n") else ""
      layer_block <- if (identical(toupper(node$paramcd %||% ""), "CARRY_OVER")) {
        group_layers <- Filter(function(x) identical(toupper(x$paramcd %||% ""), "CARRY_OVER"),
          as_records(graph$layers %||% list()))
        members <- if (length(group_layers)) flat_chr(group_layers[[1]]$members_parameters) else character()
        paste0("KEYS_NODE: return exactly the key columns for the whole CARRY_OVER group, with one row per record. ",
          "For PARAMCD, use only the allowed whole-group member values: ", paste(members, collapse = ", "),
          ". Never use the synthetic group code CARRY_OVER as a PARAMCD value. Key variable spec rows follow:\nKEY_COLUMNS_BEGIN\n",
          key_rows, "\nKEY_COLUMNS_END\n")
      } else paste0("KEYS_NODE: return exactly the key columns for PARAMCD ", node$paramcd,
        ". Include PARAMCD equal to that code. Key variable spec rows follow:\nKEY_COLUMNS_BEGIN\n", key_rows, "\nKEY_COLUMNS_END\n")
    } else {
      kv <- spec_vars(spec,ds); kv <- kv[toupper(kv$Variable) %in% keys,,drop=FALSE]
      key_rows <- if(nrow(kv)) paste(vapply(seq_len(nrow(kv)),function(i)paste("LAYER_KEY",kv$Variable[[i]],field_txt(kv$Source[[i]]),sep="\t"),""),collapse="\n") else ""
      layer_block <- paste0("LAYER_VARIABLE: build this variable only on the records in parameter layer ", node$paramcd,
        ".\n",if(identical(side,"primary"))"KEY_COLUMNS_IN_LINEAGE: "else"KEY_COLUMNS_IN_TEMP: ",paste(keys,collapse=", "),
        paste0(". These named key columns are already present in ",side_program_name(ds,node,side),". If Source names one of these keys, read it from that column.\n"),
        "LAYER_KEY_ROWS_BEGIN\n",key_rows,"\nLAYER_KEY_ROWS_END\n")
    }
  }
  if (identical(node$kind,"records") && is.null(node$paramcd)) {
    kv<-spec_vars(spec,ds);kk<-suppressWarnings(as.integer(kv$Key));kv<-kv[!is.na(kk)&kk!=0L,,drop=FALSE]
    key_rows<-if(nrow(kv))paste(vapply(seq_len(nrow(kv)),function(i)paste("KEY",kv$Variable[[i]],as.character(as.integer(kv$Key[[i]])),field_txt(kv$Source[[i]]),field_txt(kv$Derivation[[i]]),sep="\t"),""),collapse="\n")else ""
    layer_block<-paste0("KEYS_NODE: return these key columns, one row per record; do not create separate key-variable nodes.\nKEY_COLUMNS_BEGIN\n",key_rows,"\nKEY_COLUMNS_END\n")
  }
  if (isTRUE(graph$layered) && identical(node$kind,"dataset_variable")) layer_block <- paste0(layer_block,
    "DATASET_LEVEL_NODE: one row per USUBJID. Build USUBJID from the source named in this variable's Source column.\n")
  handling <- isTRUE(graph$layered) && identical(node$kind,"param_variable") && !chat
  rewrite_approach <- ""
  if (handling && nzchar(code_block)) {
    prior <- ""
    if (!is.null(project)) {
      path <- tryCatch(file.path(side_run_dir(project_dir_of(project),ds,node,side),"approach.json"),error=function(e) "")
      saved <- if (nzchar(path)) tryCatch(read_json(path)$approach,error=function(e)NULL) else NULL
      prior <- handling_approach_text(saved)
    }
    if (!nzchar(prior) && is.list(self_review)) prior <- handling_approach_text(self_review$approach)
    if (nzchar(prior)) rewrite_approach <- paste0("PREVIOUS_APPROACH (same side):\n",prior,
      "\nAnswer the question again for this rewrite. Do not copy the previous answer.\n")
  }
  paste0(
    "SIDE: ", side, "\n",
    "DATASET: ", ds, "\n",
    "NODE: ", node$id, "\n",
    "VAR: ", if (identical(node$kind,"records")) "KEYS" else node$var, "\n",
    "PARAMCD: ", node$paramcd %||% "-", "\n",
    "KEYS: ", paste(keys, collapse = ","), "\n",
    pkg_block,
    vars_label, ": ", paste(ups, collapse = ","), "\n",
    spec_rows_text(spec, ds, node$var), "\n",
    if (nzchar(param_row)) paste0(if (identical(toupper(node$paramcd %||% ""),"CARRY_OVER")) "PARAMETER_ROWS_BEGIN\n" else "PARAMETER_ROW_BEGIN\n",param_row,if (identical(toupper(node$paramcd %||% ""),"CARRY_OVER")) "\nPARAMETER_ROWS_END\n" else "\nPARAMETER_ROW_END\n") else "",
    group_context,
    layer_block,
    "EDGES:\n", paste(ev, collapse = "\n"), "\n",
    "EXTERNAL:\n", paste(ext, collapse = "\n"), "\n",
    "USER_INSTRUCTION: ", field_txt(user_instruction), "\n",
    lineage$text,
    extra,
    code_block,
    notes_block,
    review_block,
    expert_thread_text(project, ds, side, node),
    if (handling) {
      q <- if (identical(side,"primary")) c(
      paste0(if (identical(toupper(node$paramcd %||% ""),"CARRY_OVER")) "Current carry-over group. Members are listed in PARAMETER_ROWS. Current variable: " else paste0("Current parameter: ",node$paramcd,". Current variable: "),node$var,"."),
        if (identical(toupper(node$paramcd %||% ""),"CARRY_OVER")) "How should this variable be handled across the whole carry-over group? Include member-specific differences." else "How should this variable be handled for this parameter?", "Answer in 2-5 short sentences:",
        "1. Does this variable apply to this parameter? If not, what value should it take?",
        "2. Which rows and columns does it use, from which LINEAGE_ datasets?",
        "3. What is specific to this parameter, if anything? Say \"nothing\" if the rule is the same for all parameters.",
        "4. Is anything in the spec ambiguous here? Say how you will follow its wording.") else c(
        paste0(if (identical(toupper(node$paramcd %||% ""),"CARRY_OVER")) "Current carry-over group. Members are listed in PARAMETER_ROWS. Current variable: " else paste0("Current parameter: ",node$paramcd,". Current variable: "),node$var,"."),
        if (identical(toupper(node$paramcd %||% ""),"CARRY_OVER")) "How should this variable be handled across the whole carry-over group? Include member-specific differences." else "How should this variable be handled for this parameter?", "Answer in 2-5 short sentences:",
        "1. Does this variable apply to this parameter? If not, what value should it take?",
        "2. Which rows and columns does it use? You may use any available source.",
        "3. What is specific to this parameter, if anything? Say \"nothing\" if the rule is the same for all parameters.",
        "4. Is anything in the spec ambiguous here? Say what your own understanding is.")
      paste0("\n",rewrite_approach,"HANDLING_QUESTION_BEGIN\n",paste(q,collapse="\n"),"\nHANDLING_QUESTION_END")
    } else ""
  )
}

# The same side-local frame that start_side_proc passes to the worker. Prompts
# expose names and row count only; the primary lineage policy is stated separately.
side_prompt_frame <- function(project, dataset, side, spec, graph, node) {
  keys <- if (isTRUE(graph$layered) && !is.null(node$paramcd)) layer_key_names(spec,dataset,node$paramcd,graph,project) else spec_key_names(spec,dataset)
  if (side_is_keys_step(node,keys)) return(empty_key_frame(keys))
  if (isTRUE(graph$layered) && !is.null(node$paramcd)) {
    frame <- layered_work_frame(project, dataset, side, node, graph, spec)
  } else {
    frame <- build_upstream(project, dataset, node$id, side, graph, keys, spec=spec)
  }
  if (!is.data.frame(frame) || !identical(side, "primary")) return(frame)
  if (isTRUE(graph$layered) && !is.null(node$paramcd)) {
    keys <- layer_key_names(spec,dataset,node$paramcd,graph,project)
    keep <- unique(c(keys, "PARAMCD", vapply(Filter(function(n) identical(n$kind,"dataset_variable"),graph$nodes), `[[`, "", "var"),
      vapply(Filter(function(n) identical(n$paramcd,node$paramcd) && n$kind=="param_variable",graph$nodes), `[[`, "", "var")))
    return(frame[, intersect(names(frame),keep),drop=FALSE])
  }
  allowed <- lineage_context(spec, graph, node, project, side)
  keys <- spec_key_names(spec, dataset)
  keep <- unique(c(keys, vapply(allowed$ids, function(x) parse_node_id(x)$var %||% "", "")))
  frame[, intersect(names(frame), keep), drop=FALSE]
}

side_prompt_layer_bindings <- function(layers, dataset) {
  if (!length(layers)) return(character())
  vapply(names(layers), function(pc) paste0(side_temp_name(dataset,paramcd=pc), " (",pc,"): ",paste(names(layers[[pc]]),collapse=",")), "")
}

side_prompt_finished_layers <- function(project, dataset, side, spec, graph, node) {
  if (is.null(project) || !isTRUE(graph$layered) || is.null(node$paramcd)) return(character())
  visible <- tryCatch(finished_layers_for_side(project,dataset,side,graph,spec,node$paramcd),error=function(e)list())
  side_prompt_layer_bindings(visible,dataset)
}

side_available_datasets_for_prompt <- function(project, dataset, side, spec, graph, node,
                                                current_frame = NULL, layers = NULL,
                                                lineage = NULL, keys = NULL) {
  if (!isTRUE(graph$layered) && !identical(side,"primary")) return("")
  node <- node_record(node)
  if (is.null(keys)) {
    keys <- if (!is.null(node$paramcd)) layer_key_names(spec,dataset,node$paramcd,graph,project) else spec_key_names(spec,dataset)
    if (identical(node$kind,"dataset_variable")) keys <- "USUBJID"
  }
  if (is.null(current_frame)) current_frame <- if (is.null(project)) data.frame() else
    tryCatch(side_prompt_frame(project,dataset,side,spec,graph,node),error=function(e)data.frame())
  if (is.null(layers)) layers <- if (!is.null(project) && !is.null(node$paramcd))
    tryCatch(finished_layers_for_side(project,dataset,side,graph,spec,node$paramcd),error=function(e)list()) else list()
  if (is.null(lineage)) lineage <- lineage_context(spec,graph,node,project,side)
  src <- if (is.null(project)) list() else tryCatch(load_src(project),error=function(e)list())
  source_refs <- if (identical(side,"primary")) side_primary_source_refs(spec,dataset,keys,lineage,node,src) else character()
  src <- side_filter_source_frames(src,dataset,side,source_refs,keys)
  side_available_datasets_block(dataset,node,current_frame,layers,src,side)
}

spec_param_row_text <- function(spec, dataset, paramcd) {
  if (is.null(paramcd) || !nzchar(as.character(paramcd)[1])) return("")
  params <- spec_params(spec,dataset)
  if (!nrow(params)) return("")
  hit <- which(toupper(trimws(params$PARAMCD))==toupper(trimws(as.character(paramcd)[1])))
  if (!length(hit)) return("")
  i <- hit[[1]]
  row <- if (!is.null(params$row) && !is.na(params$row[[i]])) as.character(as.integer(params$row[[i]])) else ""
  paste0("ROW: ",row,"\nPARAMCD: ",field_txt(params$PARAMCD[[i]]),"\nPARAM: ",field_txt(params$PARAM[[i]]),
         "\nSOURCE: ",field_txt(params$Source[[i]]),"\nDERIVATION: ",field_txt(params$Derivation[[i]]))
}

spec_key_names <- function(spec, dataset) {
  v <- spec_vars(spec, dataset)
  if (!nrow(v) || is.null(v$Key)) return(character())
  k <- suppressWarnings(as.integer(v$Key))
  v <- v[!is.na(k), , drop = FALSE]
  if (!nrow(v)) return(character())
  k <- suppressWarnings(as.integer(v$Key))
  v$Variable[order(k)]
}

generate_side <- function(cfg, side, spec, graph, node, upstream_vars,
                          qc_feedback = NULL, user_instruction = NULL, catalog = NULL,
                          project = NULL, current_code = NULL, notes = NULL, self_review = NULL) {
  side <- match.arg(side, c("primary", "validation"))
  node <- node_record(node)
  graph <- if (is.null(graph)) empty_graph(parse_node_id(node$id)$dataset %||% "") else normalize_graph(graph)
  ds <- graph$dataset %||% parse_node_id(node$id)$dataset
  system <- side_generation_system_prompt(side, spec, graph, node, project)
  user <- side_user_prompt(side, spec, graph, node, upstream_vars, qc_feedback, user_instruction, catalog,
                           project = project, current_code = current_code, notes = notes, self_review = self_review)
  if (exists("llm_reply_with_lookup", mode = "function")) {
    scope <- expert_scope_ids(spec, graph, node)
    base_task <- if (!is.null(self_review) || !is.null(user_instruction)) "rewrite" else "generation"
    call_index <- 0L
    on_call <- function(call_system, call_user, phase) {
      if (identical(phase, "started")) {
        call_index <<- call_index + 1L
        task <- if (call_index > 1L) "lookup round" else base_task
        if (exists("coder_window_prompt", mode = "function"))
          coder_window_prompt(project, ds, node$id %||% node, side, call_system, call_user, task,
                              parent_task = if (call_index > 1L) base_task else NULL)
        if (exists("coder_window_event", mode = "function"))
          coder_window_event(project, ds, node$id %||% node, side, "Model call started", task)
      } else if (exists("coder_window_event", mode = "function"))
        coder_window_event(project, ds, node$id %||% node, side, "Model call finished", if (call_index > 1L) "lookup round" else base_task)
    }
    answered <- llm_reply_with_lookup(cfg, system, user, spec, graph, ds, max_tokens = 4000L,
      allowed_ids = if (identical(side, "primary")) scope$ids else NULL,
      on_call = if (is.null(project)) NULL else on_call)
    raw <- answered$raw
    parsed <- answered$parsed
  } else {
    task <- if (!is.null(self_review) || !is.null(user_instruction)) "rewrite" else "generation"
    if (exists("coder_window_prompt", mode = "function")) coder_window_prompt(project, ds, node$id %||% node, side, system, user, task)
    if (exists("coder_window_event", mode = "function")) coder_window_event(project, ds, node$id %||% node, side, "Model call started", task)
    on.exit(if (!is.null(project) && exists("coder_window_event", mode = "function")) coder_window_event(project, ds, node$id %||% node, side, "Model call finished", task), add = TRUE)
    raw <- llm_chat(cfg, system, user, max_tokens = 4000L, json = TRUE)
    parsed <- parse_json_reply(raw$text)
  }
  code <- parsed$code %||% parsed$r_code %||% ""
  if (!nzchar(trimws(code))) stop("The model returned empty code.", call. = FALSE)
  same <- parsed$same_source
  if (is.character(same)) same <- tolower(same[[1]]) %in% c("true", "1", "yes")
  attr(code, "same_source") <- isTRUE(same)
  attr(code, "description") <- as.character(parsed$description %||% "")[1]
  attr(code, "approach") <- handling_approach_text(parsed$approach)
  if (isTRUE(graph$layered) && identical(node$kind,"param_variable") && exists("coder_window_approach", mode="function"))
    coder_window_approach(project, ds, node$id, side, attr(code,"approach"))
  code
}

# ---- fake code generation ----

prompt_field <- function(user, name) {
  # Stay on one line. [[:space:]] would eat the newline and the next field.
  m <- regexec(paste0("(?m)^", name, ":[ \\t]*([^\\r\\n]*)"), user %||% "", perl = TRUE)
  r <- regmatches(user %||% "", m)[[1]]
  if (length(r) < 2L) "" else trimws(r[[2]])
}

copy_code <- function(var, domain, src_var) {
  paste0(
    'dom <- src[["', domain, '"]]\n',
    'if (is.null(dom)) stop("Missing source domain ', domain, '")\n',
    'if (!"', src_var, '" %in% names(dom)) stop("Missing ', domain, '.', src_var, '")\n',
    'miss <- setdiff(keys, names(dom))\n',
    'if (length(miss)) stop("Source domain is missing keys: ", paste(miss, collapse = ", "))\n',
    'result <- data.frame(dom[, keys, drop = FALSE], stringsAsFactors = FALSE)\n',
    'result[["', var, '"]] <- dom[["', src_var, '"]]\n',
    'result <- result[, unique(c(keys, "', var, '")), drop = FALSE]\n',
    'rownames(result) <- NULL\n'
  )
}

add_column_code <- function(var, domain, src_var, key) {
  if (!nzchar(key %||% "")) key <- "USUBJID"
  paste0(
    'dom <- src[["', domain, '"]]\n',
    'if (is.null(dom)) stop("Missing source domain ', domain, '")\n',
    'if (!"', src_var, '" %in% names(dom)) stop("Missing ', domain, '.', src_var, '")\n',
    'if (!nrow(work)) stop("work has no rows")\n',
    'if (!"', key, '" %in% names(dom)) stop("Source domain is missing key ', key, '")\n',
    'result <- data.frame(work[, keys, drop = FALSE], stringsAsFactors = FALSE)\n',
    'result[["', var, '"]] <- dom[["', src_var, '"]][match(as.character(work[["', key, '"]]), as.character(dom[["', key, '"]]))]\n',
    'result <- result[, unique(c(keys, "', var, '")), drop = FALSE]\n',
    'rownames(result) <- NULL\n'
  )
}

note_comment_prefix <- function(user) {
  if (!exists("text_block", mode = "function")) return("")
  block <- text_block(user, "NOTES_BEGIN", "NOTES_END")
  if (!nzchar(trimws(block %||% ""))) return("")
  lines <- strsplit(block, "\n", fixed = TRUE)[[1]]
  out <- character()
  for (ln in lines) {
    ln <- trimws(gsub("#", " ", ln))
    if (!nzchar(ln)) next
    m <- regexec("^([0-9]+)\\.\\s*(.*)$", ln)
    r <- regmatches(ln, m)[[1]]
    out <- c(out, if (length(r) >= 3L) paste0("# note ", r[[2]], ": ", trimws(r[[3]])) else paste0("# note: ", ln))
  }
  if (!length(out)) "" else paste0(paste(out, collapse = "\n"), "\n")
}

fake_inherited_opening_code <- function(user) {
  if (!grepl("REQUIRED_PROGRAM_OPENING_BEGIN", user, fixed = TRUE)) return("")
  side <- prompt_field(user,"SIDE")
  expected_field <- if (identical(side,"primary")) "EXPECTED_IN_LINEAGE" else "EXPECTED_IN_TEMP"
  dataset_field <- if (identical(side,"primary")) "LINEAGE_DATASET" else "TEMP_DATASET"
  expected <- trimws(strsplit(prompt_field(user, expected_field), ",", fixed = TRUE)[[1]])
  expected <- expected[nzchar(expected)]
  inherited_opening_block(prompt_field(user, dataset_field), expected, side)
}

is_number_token <- function(x) grepl("^[0-9]+(\\.[0-9]+)?$", x %||% "")

arith_term <- function(tok) {
  if (is_number_token(tok)) tok else paste0('upstream[["', tok, '"]]')
}

arith_code <- function(var, lhs, op, rhs) {
  vars <- unique(c(lhs, rhs))
  vars <- vars[!vapply(vars, is_number_token, logical(1))]
  need <- if (length(vars)) paste0(
    'need <- c(', paste(sprintf('"%s"', vars), collapse = ", "), ')\n',
    'miss <- setdiff(need, names(upstream))\n',
    'if (length(miss)) stop("upstream is missing: ", paste(miss, collapse = ", "))\n'
  ) else ""
  paste0(
    need,
    'result <- data.frame(upstream[, keys, drop = FALSE], stringsAsFactors = FALSE)\n',
    'result[["', var, '"]] <- ', arith_term(lhs), ' ', op, ' ', arith_term(rhs), '\n',
    'rownames(result) <- NULL\n'
  )
}

fake_check_output_code <- function(var) {
  target <- paste0("result$", var)
  paste0(
    'cat("rows:", nrow(result), "\\n")\n',
    'if (is.numeric(', target, ') || inherits(', target, ', "Date")) {\n',
    '  print(summary(', target, '))\n',
    '} else if (is.character(', target, ') || is.factor(', target, ') || is.logical(', target, ')) {\n',
    '  if (length(unique(', target, ')) <= 20L) {\n',
    '    print(table(', target, ', useNA = "ifany"))\n',
    '  } else {\n',
    '    cat("', var, ': more than 20 distinct values\\n")\n',
    '  }\n',
    '}\n'
  )
}

fake_keys_check_output_code <- function(keys) {
  fake_keys_check_for("result", keys)
}

fake_keys_check_for <- function(object, keys) {
  checks <- vapply(keys, function(key) paste0(
    'cat("',key,' type:", typeof(',object,'[["',key,'"]]), " NA:", sum(is.na(',object,'[["',key,'"]])), " distinct:", length(unique(',object,'[["',key,'"]])), "\\n")\n',
    'if (is.numeric(',object,'[["',key,'"]]) || inherits(',object,'[["',key,'"]], "Date")) print(summary(',object,'[["',key,'"]])) else if (is.character(',object,'[["',key,'"]]) || is.factor(',object,'[["',key,'"]]) || is.logical(',object,'[["',key,'"]])) { if (length(unique(',object,'[["',key,'"]])) <= 20L) print(table(',object,'[["',key,'"]], useNA = "ifany")) else cat("',key,': more than 20 distinct values\\n") }'
  ), "")
  paste0('cat("rows:", nrow(', object, '), "\\n")\n',
    'cat("duplicate key rows:", sum(duplicated(', object, '[, c(',paste(vapply(keys,json_text,""),collapse=","),'), drop = FALSE]) | duplicated(', object, '[, c(',paste(vapply(keys,json_text,""),collapse=","),'), drop = FALSE], fromLast = TRUE)), "\\n")\n',
    paste(checks,collapse="\n"),"\n")
}

fake_primary_check_output_code <- function(user, var) {
  temp <- prompt_field(user, "LINEAGE_DATASET")
  if (!nzchar(temp)) temp <- prompt_field(user, "DATASET_TEMP")
  object <- if (nzchar(temp)) temp else "result"
  prefix <- if (nzchar(temp)) paste0(temp, " <- result\n") else ""
  if (identical(var, "KEYS")) return(paste0(prefix, fake_keys_check_for(object, trimws(strsplit(prompt_field(user,"KEYS"),",",fixed=TRUE)[[1]]))))
  paste0(prefix,
    'cat("rows:", nrow(', object, '), "\\n")\n',
    'if (is.numeric(', object, '$', var, ') || inherits(', object, '$', var, ', "Date")) print(summary(', object, '$', var, ')) else if (is.character(', object, '$', var, ') || is.factor(', object, '$', var, ') || is.logical(', object, '$', var, ')) { if (length(unique(', object, '$', var, ')) <= 20L) print(table(', object, '$', var, ', useNA = "ifany")) else cat("', var, ': more than 20 distinct values\\n") }\n')
}

fake_generate_reply <- function(system, user) {
  task <- fake_task(system)
  validation <- identical(task, "generate_validation")
  var <- prompt_field(user, "VAR")
  if (!nzchar(var)) stop("Fake generator did not find VAR.", call. = FALSE)
  if (grepl("KEYS_NODE:", user, fixed=TRUE)) {
    pc <- prompt_field(user, "PARAMCD"); if(identical(pc,"-"))pc<-""
    keys <- trimws(strsplit(prompt_field(user,"KEYS"),",",fixed=TRUE)[[1]])
    keys <- keys[nzchar(keys)]
    if (identical(pc,"CARRY_OVER")) {
      block <- text_block(user,"PARAMETER_ROWS_BEGIN","PARAMETER_ROWS_END")
      hits <- regmatches(block,gregexpr("(?m)^PARAMCD:[ \\t]*[A-Z][A-Z0-9_]*",block,perl=TRUE))[[1]]
      members <- sub("(?m)^PARAMCD:[ \\t]*","",hits,perl=TRUE)
      key_rows <- strsplit(text_block(user,"KEY_COLUMNS_BEGIN","KEY_COLUMNS_END"),"\n",fixed=TRUE)[[1]]
      mapping <- list()
      for (line in key_rows) {
        f <- strsplit(line,"\t",fixed=TRUE)[[1]]
        refs <- if(length(f)>=3L) domain_var_refs(f[[3]]) else list()
        if(length(f)>=3L && identical(f[[1]],"KEY") && length(refs)) mapping[[f[[2]]]] <- refs[[1]]
      }
      mapped <- Filter(function(x) !toupper(x$dom) %in% c("ADSL","DM"),mapping)
      domain <- if(length(mapped)) toupper(mapped[[1]]$dom) else "VS"
      param_ref <- mapping$PARAMCD %||% list(dom=domain,var="VSTESTCD")
      chunks <- vapply(members,function(member) {
        lines <- c(paste0(".rec <- src[[",json_text(domain),"]]"),
          paste0("if (!is.null(.rec) && ",json_text(param_ref$var)," %in% names(.rec)) .rec <- .rec[toupper(as.character(.rec[[",json_text(param_ref$var),"]])) == ",json_text(member),",,drop=FALSE]"),
          "if (!is.null(.rec) && nrow(.rec)) { .piece <- .rec[,integer(0),drop=FALSE]")
        for(key in keys) {
          ref <- mapping[[key]]
          value <- if(identical(key,"PARAMCD")) json_text(member) else if(!is.null(ref) && identical(toupper(ref$dom),domain)) paste0(".rec[[",json_text(ref$var),"]]") else "rep(NA_character_,nrow(.rec))"
          lines <- c(lines,paste0(".piece[[",json_text(key),"]] <- ",value))
        }
        paste(c(lines,".pieces[[length(.pieces)+1L]] <- .piece","}"),collapse="\n")
      },"")
      code <- paste0(".pieces <- list()\n",paste(chunks,collapse="\n"),"\nresult <- if(length(.pieces)) do.call(rbind,.pieces) else data.frame()\nrownames(result) <- NULL\n",
        if(validation) fake_keys_check_output_code(keys) else fake_primary_check_output_code(user,"KEYS"))
      output_name <- prompt_field(user,if(validation)"TEMP_DATASET" else "LINEAGE_DATASET")
      if(nzchar(output_name)) code <- gsub("\\bresult\\b",output_name,code,perl=TRUE)
      return(json_text(list(code=code,same_source=FALSE,description="Records for the complete carry-over group.")))
    }
    source <- text_block(user,"PARAMETER_ROW_BEGIN","PARAMETER_ROW_END")
    refs <- domain_var_refs(source)
    domain <- if(length(refs)) refs[[1]]$dom else ""
    refvar <- if(length(refs)) refs[[1]]$var else ""
    domain_lit <- json_text(domain); pc_lit <- json_text(pc)
    filter_expr <- if(nzchar(domain)) paste0(".rec <- src[[",domain_lit,"]]\n") else ""
    if(nzchar(domain) && nzchar(refvar)) {
      value <- regmatches(source,regexec(paste0("(?i)",refvar,"\\s*(?:=|EQ)\\s*['\"]?([A-Z0-9_+-]+)"),source,perl=TRUE))[[1]]
      if(length(value)>=2L) filter_expr <- paste0(filter_expr,".rec <- .rec[as.character(.rec[[",json_text(refvar),"]]) == ",json_text(value[[2]]),",,drop=FALSE]\n")
    }
    key_spec <- strsplit(text_block(user,"KEY_COLUMNS_BEGIN","KEY_COLUMNS_END"),"\n",fixed=TRUE)[[1]]
    key_code <- character()
    for (line in key_spec) {
      fields <- strsplit(line,"\t",fixed=TRUE)[[1]]
      if(length(fields)<4L || fields[[1]]!="KEY") next
      key <- fields[[2]]; krefs <- domain_var_refs(fields[[3]])
      ref <- if(length(krefs)) krefs[[1]] else NULL
      if(!nzchar(pc)) {
        if(is.null(ref))next
        refs[[length(refs)+1L]]<-ref
      }
      if(identical(key,"PARAMCD")) key_code <- c(key_code,paste0("result[[",json_text(key),"]] <- ",pc_lit))
      else if(!is.null(ref) && identical(toupper(ref$dom),toupper(domain))) key_code <- c(key_code,
        paste0("result[[",json_text(key),"]] <- .rec[[",json_text(ref$var),"]]"))
      else if(!is.null(ref) && ref$dom %in% c("ADSL","DM")) key_code <- c(key_code,
        paste0("result[[",json_text(key),"]] <- src[[",json_text(ref$dom),"]][match(.rec$USUBJID,src[[",json_text(ref$dom),"]]$USUBJID),",json_text(ref$var),"]"))
    }
    if(!nzchar(pc)) {
      key_defs<-list()
      for(line in key_spec) {
        f<-strsplit(line,"\t",fixed=TRUE)[[1]]
        if(length(f)<4L || f[[1]]!="KEY")next
        kr<-domain_var_refs(f[[4]])
        if(length(kr))key_defs[[length(key_defs)+1L]]<-list(key=f[[2]],ref=kr[[1]],source=f[[4]])
      }
      refs_all<-lapply(key_defs,`[[`,"ref")
      anchor<-which(!vapply(refs_all,function(r)toupper(r$dom)%in%c("ADSL","DM"),TRUE))
      anchor_dom<-if(length(anchor))toupper(refs_all[[anchor[[1]]]]$dom)else if(length(refs_all))toupper(refs_all[[1]]$dom)else ""
      if(!nzchar(anchor_dom))code<-"result <- data.frame()\n" else {
        code<-paste0(".rec <- src[[",json_text(anchor_dom),"]]\nresult <- .rec[,integer(0),drop=FALSE]\n")
        for(d in key_defs) {
          ref<-d$ref;dom<-toupper(ref$dom);col<-json_text(ref$var);key<-json_text(d$key)
          if(identical(dom,anchor_dom)) code<-paste0(code,"result[[",key,"]] <- .rec[[",col,"]]\n") else
            code<-paste0(code,".d_",d$key," <- src[[",json_text(dom),"]]\nresult[[",key,"]] <- .d_",d$key,"[[",col,"]][match(.rec$USUBJID,.d_",d$key,"$USUBJID)]\n")
        }
        code<-paste0(code,"result <- unique(result)\n")
      }
      if (validation) code <- paste0(code, "\n", fake_keys_check_output_code(keys)) else code <- paste0(code, "\n", fake_primary_check_output_code(user, "KEYS"))
      return(json_text(list(code=code,same_source=FALSE,description="Rows for all compound key columns.")))
    }
    dataset <- prompt_field(user,"DATASET")
    if(identical(toupper(domain),toupper(dataset))) {
      parent <- if(grepl("WEIGHT",source,fixed=TRUE))"WEIGHT" else ""
      carry_members <- toupper(trimws(strsplit(prompt_field(user,"CARRY_OVER_MEMBERS"),",",fixed=TRUE)[[1]]))
      if(nzchar(parent) && parent %in% carry_members) code <- paste0(".group <- layers[['CARRY_OVER']]\n.rec <- .group[.group$PARAMCD == ",json_text(parent),",,drop=FALSE]\n") else if(nzchar(parent)) code <- paste0(".rec <- layers[[",json_text(parent),"]]\n") else code <- ".rec <- layers[[1]]\n"
      code <- paste0(code,"result <- .rec[,",paste0("c(",paste(vapply(keys,function(x)json_text(x),""),collapse=","),")"),",drop=FALSE]\nresult$PARAMCD <- ",pc_lit,"\nresult <- unique(result)\n")
    } else code <- paste(c(filter_expr,"result <- .rec[,integer(0),drop=FALSE]",key_code,"result <- unique(result)"),collapse="\n")
    if (validation) code <- paste0(code, "\n", fake_keys_check_output_code(keys)) else code <- paste0(code, "\n", fake_primary_check_output_code(user, "KEYS"))
    return(json_text(list(code=code,same_source=FALSE,description=paste0("Keys for parameter ",pc,"."))))
  }
  src <- prompt_field(user, "SOURCE")
  der <- prompt_field(user, "DERIVATION")
  text <- toupper(paste(src, der))
  ops <- extract_tokens(text, "\\b([A-Z][A-Z0-9_]*|[0-9]+(?:\\.[0-9]+)?)\\s*([-+*/])\\s*([A-Z][A-Z0-9_]*|[0-9]+(?:\\.[0-9]+)?)\\b")
  refs <- extract_tokens(text, "\\b([A-Z][A-Z0-9]*)\\.([A-Z][A-Z0-9]*)\\b")
  if (grepl("LAYER_VARIABLE:",user,fixed=TRUE)) {
    pc <- prompt_field(user,"PARAMCD"); keys <- trimws(strsplit(prompt_field(user,"KEYS"),",",fixed=TRUE)[[1]]); keys <- keys[nzchar(keys)]
    ref <- if(length(refs)) refs[[1]] else NULL
    sr <- strsplit(text_block(user,"LAYER_KEY_ROWS_BEGIN","LAYER_KEY_ROWS_END"),"\n",fixed=TRUE)[[1]]
    mappings <- list()
    for(line in sr) {
      f <- strsplit(line,"\t",fixed=TRUE)[[1]]
      if(length(f)>=3L && f[[1]]=="LAYER_KEY") {
        kr <- domain_var_refs(f[[3]])
        if(length(kr)) mappings[[f[[2]]]] <- kr[[1]]
      }
    }
    code <- paste0("result <- data.frame(work[, ",paste0("c(",paste(vapply(keys,function(x)json_text(x),""),collapse=","),")"),", drop=FALSE], stringsAsFactors=FALSE)\n")
    if(!is.null(ref) && identical(var,"AVAL") && identical(pc,"BMI")) {
      code <- paste0(code,
        if (grepl("CARRY_OVER_TEMP",user,fixed=TRUE)) ".group <- layers[['CARRY_OVER']]\n.w <- .group[.group$PARAMCD == 'WEIGHT',,drop=FALSE]\n.h <- .group[.group$PARAMCD == 'HEIGHT',,drop=FALSE]\n" else ".w <- layers[['WEIGHT']]\n.h <- layers[['HEIGHT']]\n",
        ".sig <- function(d,k) do.call(paste,c(lapply(d[,k,drop=FALSE],function(x)ifelse(is.na(x),'<NA>',as.character(x))),sep='\\034'))\n",
        ".wk <- setdiff(keys,'PARAMCD')\n.wi <- match(.sig(work,.wk),.sig(.w,.wk))\n.hi <- match(.sig(work,.wk),.sig(.h,.wk))\n",
        "result[['AVAL']] <- .w[['AVAL']][.wi] / (.h[['AVAL']][.hi] / 100)^2\n")
    } else if(!is.null(ref) && identical(toupper(ref[[2]]),toupper(prompt_field(user,"DATASET"))) && ref[[3]] %in% c("AVAL","CHG","BASE")) {
      code <- paste0(code,"result[[",json_text(var),"]] <- rep(NA_real_,nrow(work))\n")
    } else if(!is.null(ref) && length(keys) && !is.null(mappings[[keys[[1]]]])) {
      dom <- ref[[2]]; same <- names(mappings)[vapply(mappings,function(x)identical(toupper(x$dom),toupper(dom)),TRUE)]
      if(length(same)) {
        source_keys <- vapply(same,function(k)mappings[[k]]$var,"")
        work_keys <- same
        r_vector <- function(x) paste0("c(",paste(vapply(x,json_text,""),collapse=","),")")
        code <- paste0(code,".d <- src[[",json_text(dom),"]]\n")
        if(identical(dom,"VS") && !identical(pc,"CARRY_OVER")) code <- paste0(code,"if ('VSTESTCD' %in% names(.d)) .d <- .d[.d$VSTESTCD == ",json_text(pc),",,drop=FALSE]\n")
        code <- paste0(code,".d$.layer_value <- .d[[",json_text(ref[[3]]),"]]\n.result <- merge(work[,",r_vector(work_keys),",drop=FALSE],.d[,c(",paste(vapply(source_keys,function(x)json_text(x),""),collapse=","),",'.layer_value'),drop=FALSE],by.x=",r_vector(work_keys),",by.y=",r_vector(source_keys),",all.x=TRUE,sort=FALSE)\n")
        code <- paste0(code,".sig <- function(d,k) do.call(paste,c(lapply(d[,k,drop=FALSE],function(x)ifelse(is.na(x),'<NA>',as.character(x))),sep='\\034'))\n.result_index <- match(.sig(work,",r_vector(work_keys),"),.sig(.result,",r_vector(work_keys),"))\nresult[[",json_text(var),"]] <- .result$.layer_value[.result_index]\n")
      } else code <- paste0(code,"result[[",json_text(var),"]] <- rep(NA,nrow(work))\n")
    } else if(!is.null(ref) && ref[[3]] %in% names(work)) code <- paste0(code,"result[[",json_text(var),"]] <- work[[",json_text(ref[[3]]),"]]\n")
    else code <- paste0(code,"result[[",json_text(var),"]] <- rep(NA,nrow(work))\n")
    code <- paste0(code,"\n",if (!validation) fake_primary_check_output_code(user,var) else fake_check_output_code(var))
    if (identical(pc,"CARRY_OVER")) {
      output_name <- prompt_field(user,if(validation)"TEMP_DATASET" else "LINEAGE_DATASET")
      if(nzchar(output_name)) code <- gsub("\\bresult\\b",output_name,code,perl=TRUE)
    }
    approach <- if (grepl("HANDLING_QUESTION_BEGIN",user,fixed=TRUE)) "This variable applies to the requested parameter and uses the matching parameter rows and named source columns. The parameter-specific rule follows its specification; I follow the stated wording where it is ambiguous." else NULL
    return(json_text(c(if(!is.null(approach))list(approach=approach)else list(),list(code=code,same_source=FALSE,description=paste0(var," is built on layer ",pc,".")))) )
  }
  if (length(ops)) {
    op_id <- vapply(ops, function(r) paste(r[[2]], r[[3]], r[[4]]), "")
    ops <- ops[!duplicated(op_id)]
  }
  if (length(refs)) {
    ref_id <- vapply(refs, function(r) paste(r[[2]], r[[3]], sep = "."), "")
    refs <- refs[!duplicated(ref_id)]
  }
  rows_txt <- prompt_field(user, if (validation) "WORK_ROWS" else "LINEAGE_ROWS")
  work_rows <- suppressWarnings(as.integer(rows_txt))
  use_work <- nzchar(rows_txt) && !is.na(work_rows) && work_rows > 0L
  code <- NULL
  same <- FALSE
  if (grepl("DATASET_LEVEL_NODE:",user,fixed=TRUE) && length(refs)) {
    ref <- refs[[1]]
    if(identical(var,"USUBJID")) code <- paste0("result <- data.frame(USUBJID=unique(src[[",json_text(ref[[2]]),"]][[",json_text(ref[[3]]),"]]), stringsAsFactors=FALSE)\n") else
      code <- paste0(".d <- src[[",json_text(ref[[2]]),"]]\n.i <- match(unique(as.character(.d$USUBJID)),as.character(.d$USUBJID))\nresult <- data.frame(USUBJID=.d$USUBJID[.i], ",var,"=.d[[",json_text(ref[[3]]),"]][.i], stringsAsFactors=FALSE)\n")
    same <- TRUE
  } else if (length(ops) == 1L) {
    code <- arith_code(var, ops[[1]][[2]], ops[[1]][[3]], ops[[1]][[4]])
    same <- TRUE
  } else if (length(refs) >= 1L) {
    # Primary follows the spec's first source. Validation prefers a later alternative.
    pick <- if (validation && length(refs) >= 2L) refs[[2]] else refs[[1]]
    same <- length(refs) == 1L
    if (use_work) {
      key_names <- trimws(strsplit(prompt_field(user, "KEYS"), ",", fixed = TRUE)[[1]])
      key_names <- key_names[nzchar(key_names)]
      if (identical(toupper(pick[[2]]),toupper(prompt_field(user,"DATASET"))) && pick[[3]] %in% toupper(c(key_names,strsplit(prompt_field(user,if(validation)"TEMP_FRAME_COLUMNS"else"LINEAGE_FRAME_COLUMNS"),",",fixed=TRUE)[[1]]))) {
        current <- if (validation) prompt_field(user,"TEMP_DATASET") else prompt_field(user,"LINEAGE_DATASET")
        if (!nzchar(current)) current <- "upstream"
        code <- paste0('result <- data.frame(',current,'[, keys, drop = FALSE], stringsAsFactors = FALSE)\n',
          'result[[',json_text(var),']] <- ',current,'[[',json_text(pick[[3]]),']]\n')
      } else code <- add_column_code(var, pick[[2]], pick[[3]], if (length(key_names)) key_names[[1]] else "")
    } else {
      code <- copy_code(var, pick[[2]], pick[[3]])
    }
  } else {
    code <- paste0('stop("fake LLM could not derive ', var, ' from the specification text")\n')
    same <- FALSE
  }
  if (!validation) same <- FALSE
  code <- paste0(note_comment_prefix(user), fake_inherited_opening_code(user), code,
    if (!validation) fake_primary_check_output_code(user, var) else fake_check_output_code(var))
  approach <- if (grepl("HANDLING_QUESTION_BEGIN", user, fixed=TRUE)) "This variable applies to the requested parameter and uses the matching parameter rows and named source columns. The parameter-specific rule follows its specification; I follow the stated wording where it is ambiguous." else NULL
  json_text(c(if (!is.null(approach)) list(approach=approach) else list(), list(code = code, same_source = same,
                 description = paste0(var, " is derived on the ", if (validation) "validation" else "primary", " side from the specified source and rule. Its R type and missing values follow the generated result."))))
}

# ---- isolated runs ----

side_run_dir <- function(project, dataset, node, side) {
  pt <- parse_node_id(if (is.list(node)) node$id else node)
  file.path(project, ".dualdag", "runs", toupper(dataset), pt$folder, side)
}

plain_frame <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    x <- df[[nm]]
    if (is.factor(x)) {
      df[[nm]] <- as.character(x)
    } else if (inherits(x, "POSIXct")) {
      tz <- attr(x, "tzone", exact = TRUE)
      attributes(x) <- c(list(class = c("POSIXct", "POSIXt")), if (!is.null(tz)) list(tzone = tz))
      df[[nm]] <- x
    } else if (inherits(x, "Date")) {
      attributes(x) <- list(class = "Date")
      df[[nm]] <- x
    } else if (inherits(x, "difftime")) {
      units <- attr(x, "units", exact = TRUE)
      attributes(x) <- list(class = "difftime", units = units %||% "secs")
      df[[nm]] <- x
    } else {
      attributes(x) <- NULL
      df[[nm]] <- x
    }
  }
  rownames(df) <- NULL
  df
}

load_src <- function(project) {
  dir <- file.path(project, "data")
  src <- list()
  if (!dir.exists(dir)) return(src)
  files <- list.files(dir, pattern = "\\.[Xx][Pp][Tt]$", full.names = TRUE)
  for (f in files) {
    dom <- toupper(tools::file_path_sans_ext(basename(f)))
    if (dom %in% names(src)) next
    src[[dom]] <- plain_frame(haven::read_xpt(f))
  }
  src
}

load_project_graph <- function(project, dataset) {
  p <- file.path(project, ".dualdag", "state", sprintf("graph_%s.json", toupper(dataset)))
  if (!file.exists(p)) return(NULL)
  normalize_graph(jsonlite::fromJSON(p, simplifyVector = FALSE))
}

load_project_spec <- function(project) {
  vp <- file.path(project, "spec", "variable.csv")
  if (!file.exists(vp)) return(NULL)
  v <- utils::read.csv(vp, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
  v$row <- seq_len(nrow(v)) + 1L
  if ("Dataset" %in% names(v)) v$Dataset <- toupper(trimws(v$Dataset))
  if ("Variable" %in% names(v)) v$Variable <- toupper(trimws(v$Variable))
  if ("Key" %in% names(v)) v$Key <- suppressWarnings(as.integer(v$Key))
  pp <- file.path(project, "spec", "parameter.csv")
  p <- NULL
  if (file.exists(pp)) {
    p <- utils::read.csv(pp, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
    p$row <- seq_len(nrow(p)) + 1L
    if ("Dataset" %in% names(p)) p$Dataset <- toupper(trimws(p$Dataset))
    if ("PARAMCD" %in% names(p)) p$PARAMCD <- toupper(trimws(p$PARAMCD))
  }
  list(variables = v, parameters = p, datasets = unique(v$Dataset), hash = NULL)
}

empty_key_frame <- function(keys) {
  keys <- flat_chr(keys)
  if (!length(keys)) return(data.frame())
  cols <- replicate(length(keys), character(0), simplify = FALSE)
  names(cols) <- keys
  as.data.frame(cols, stringsAsFactors = FALSE)
}

coerce_key_cols <- function(df, keys) {
  df <- plain_frame(df)
  for (k in intersect(flat_chr(keys), names(df))) df[[k]] <- as.character(df[[k]])
  df
}

work_path <- function(project, dataset, side) {
  file.path(project, ".dualdag", "runs", toupper(as.character(dataset)[1]), "_work", paste0(side, ".rds"))
}

read_project_state <- function(project) {
  path <- file.path(project, ".dualdag", "state", "state.json")
  if (!file.exists(path)) return(NULL)
  if (exists("read_json", mode = "function", inherits = TRUE)) {
    st <- tryCatch(read_json(path), error = function(e) NULL)
    if (is.list(st)) return(st)
  }
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
}

state_nodes <- function(project, dataset) {
  st <- read_project_state(project)
  if (is.null(st) || is.null(st$nodes)) return(list())
  nodes <- st$nodes[[toupper(as.character(dataset)[1])]]
  if (is.null(nodes) || !is.list(nodes)) list() else nodes
}

node_is_l3 <- function(nodes, id) {
  st <- nodes[[id]]
  is.list(st) && identical(as.character(st$level %||% "")[1], "L3")
}

read_side_result <- function(project, dataset, id, side, keys) {
  path <- file.path(side_run_dir(project, dataset, id, side), "out.rds")
  if (!file.exists(path)) return(NULL)
  df <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.data.frame(df) || !nrow(df)) return(NULL)
  parsed <- parse_node_id(id)
  var <- parsed$var %||% ""
  df <- coerce_key_cols(df, keys)
  if (!nzchar(var) || !var %in% names(df)) {
    keep <- intersect(flat_chr(keys), names(df))
    if (!length(keep)) return(NULL)
    return(df[, keep, drop = FALSE])
  }
  df[, intersect(unique(c(flat_chr(keys), var)), names(df)), drop = FALSE]
}

# Key rows plus every L3 column on this side. exclude_id drops the node being regenerated.
node_side_result_available <- function(project, dataset, id, side, include_generated = FALSE, states = NULL) {
  if (is.null(states)) states <- state_nodes(project, dataset)
  if (node_is_l3(states, id)) return(TRUE)
  if (!isTRUE(include_generated)) return(FALSE)
  st <- states[[id]] %||% list()
  isTRUE(st$sides[[side]]$ok) && file.exists(file.path(side_run_dir(project, dataset, id, side), "out.rds"))
}

assemble_work <- function(project, dataset, side, spec = NULL, graph = NULL, exclude_id = NULL,
                          include_generated = FALSE, states = NULL) {
  dataset <- toupper(as.character(dataset)[1])
  side <- match.arg(side, c("primary", "validation"))
  if (is.null(spec)) spec <- load_project_spec(project)
  keys <- if (is.null(spec)) character() else spec_key_names(spec, dataset)
  if(!is.null(spec) && exists("layer_keys_path",mode="function") && file.exists(layer_keys_path(project,dataset))) keys <- layer_key_names(spec,dataset,"*",graph,project)
  nodes <- states %||% state_nodes(project, dataset)
  key_ids <- if (length(keys)) paste0(dataset, ".", keys) else character()
  if (!is.null(exclude_id)) exclude_id <- as.character(exclude_id)[1]
  out <- NULL
  records_id <- paste0(dataset,".RECORDS")
  has_records <- !is.null(graph) && any(vapply(graph$nodes %||% list(),function(n)
    identical(as.character(n$id %||% ""),records_id)&&identical(n$kind,"records")&&is.null(n$paramcd),TRUE))
  if(has_records && node_side_result_available(project,dataset,records_id,side,include_generated,nodes) && !identical(exclude_id,records_id)) {
    out <- read_side_result(project,dataset,records_id,side,keys)
  } else if(!has_records) {
    for (id in key_ids) {
      if (!is.null(exclude_id) && identical(id, exclude_id)) next
      if (!node_side_result_available(project,dataset,id,side,include_generated,nodes)) next
      piece <- read_side_result(project, dataset, id, side, keys)
      if (is.null(piece)) next
      if (is.null(out)) {
        out <- piece
      } else {
        by <- intersect(keys, intersect(names(out), names(piece)))
        extra <- setdiff(names(piece), names(out))
        if (length(by) && length(extra)) {
          out <- merge(out, piece[, c(by, extra), drop = FALSE], by = by, all.x = TRUE, sort = FALSE)
        }
      }
    }
  }
  if (is.null(out)) out <- empty_key_frame(keys)
  if (nrow(out)) {
    for (id in names(nodes)) {
      if (id %in% key_ids) next
      if (!is.null(exclude_id) && identical(id, exclude_id)) next
      if (!node_side_result_available(project,dataset,id,side,include_generated,nodes)) next
      piece <- read_side_result(project, dataset, id, side, keys)
      if (is.null(piece)) next
      parsed <- parse_node_id(id)
      var <- parsed$var %||% ""
      if (!nzchar(var) || !var %in% names(piece)) next
      col <- var
      if (col %in% names(out)) {
        pc <- parsed$paramcd %||% ""
        if (!nzchar(pc)) next
        col <- paste0(var, "__", pc)
        if (col %in% names(out)) next
        names(piece)[names(piece) == var] <- col
      }
      by <- intersect(keys, intersect(names(out), names(piece)))
      if (!length(by) || !col %in% names(piece)) next
      out <- merge(out, piece[, c(by, col), drop = FALSE], by = by, all.x = TRUE, sort = FALSE)
    }
  }
  if (!is.null(exclude_id)) {
    parsed <- parse_node_id(exclude_id)
    ev <- parsed$var %||% ""
    if (nzchar(ev) && ev %in% names(out) && !ev %in% keys) out[[ev]] <- NULL
    pc <- parsed$paramcd %||% ""
    if (nzchar(ev) && nzchar(pc)) {
      alt <- paste0(ev, "__", pc)
      if (alt %in% names(out)) out[[alt]] <- NULL
    }
  }
  for (k in keys) if (!k %in% names(out)) out[[k]] <- if (nrow(out)) NA_character_ else character()
  if (length(keys)) out <- out[, c(keys, setdiff(names(out), keys)), drop = FALSE]
  rownames(out) <- NULL
  out
}

refresh_work_side <- function(project, dataset, side, spec = NULL, graph = NULL) {
  df <- assemble_work(project, dataset, side, spec, graph, exclude_id = NULL)
  path <- work_path(project, dataset, side)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(df, path)
  invisible(df)
}

refresh_work_dataset <- function(project, dataset, spec = NULL, graph = NULL) {
  if (is.null(graph)) graph <- tryCatch(load_graph(project, dataset), error = function(e) NULL)
  # Layered datasets have per-layer work frames. Flattening all parameter rows
  # by the dataset key can multiply repeated subject/parameter records.
  if (isTRUE(graph$layered)) return(invisible())
  for (side in c("primary", "validation")) {
    tryCatch(refresh_work_side(project, dataset, side, spec, graph), error = function(e) NULL)
  }
  invisible()
}

refresh_all_work <- function(p) {
  root <- if (is.character(p)) p else if (exists("project_dir_of", mode = "function")) project_dir_of(p) else p$dir
  st <- if (is.environment(p) && is.list(p$state)) p$state else read_project_state(root)
  if (is.null(st) || is.null(st$nodes) || !length(names(st$nodes))) return(invisible())
  spec <- tryCatch(load_project_spec(root), error = function(e) NULL)
  for (ds in names(st$nodes)) refresh_work_dataset(root, ds, spec, tryCatch(load_graph(root, ds), error = function(e) NULL))
  invisible()
}

build_upstream <- function(project, dataset, node, side, graph, keys, spec = NULL) {
  id <- if (is.list(node)) node$id else as.character(node)[1]
  if (is.null(spec)) spec <- load_project_spec(project)
  assemble_work(project, dataset, side, spec, graph, exclude_id = id)
}

layer_node_ids <- function(graph, code) {
  nodes <- Filter(function(n) identical(toupper(n$paramcd %||% ""), toupper(code)) &&
    n$kind %in% c("records", "param_variable"), graph$nodes %||% list())
  vapply(nodes, `[[`, "", "id")
}

read_layer_frame <- function(project, dataset, side, code, graph, spec, require_finished = TRUE) {
  ids <- layer_node_ids(graph, code)
  states <- state_nodes(project, dataset)
  records_id <- paste0(toupper(dataset), ".RECORDS@", toupper(code))
  if (identical(toupper(code), "CARRY_OVER") && require_finished) {
    available <- ids[vapply(ids, function(id) identical(as.character(states[[id]]$level %||% ""), "L3"), TRUE)]
    if (!records_id %in% available) return(NULL)
    ids <- c(records_id, setdiff(available, records_id))
  } else if (!length(ids) || (require_finished && !all(vapply(ids, function(id)
    identical(as.character(states[[id]]$level %||% ""), "L3"), TRUE)))) return(NULL)
  recpath <- file.path(side_run_dir(project, dataset, records_id, side), "out.rds")
  if (!file.exists(recpath)) return(NULL)
  out <- plain_frame(readRDS(recpath))
  keys <- layer_key_names(spec,dataset,code,graph,project)
  if (!all(keys %in% names(out))) return(NULL)
  for (id in setdiff(ids, records_id)) {
    path <- file.path(side_run_dir(project, dataset, id, side), "out.rds")
    if (!file.exists(path)) next
    piece <- plain_frame(readRDS(path)); var <- parse_node_id(id)$var
    if (!all(keys %in% names(piece)) || !var %in% names(piece)) next
    by <- keys
    out <- merge(out, piece[, unique(c(by, var)), drop=FALSE], by=by, all.x=TRUE, sort=FALSE)
  }
  rownames(out) <- NULL
  out
}

layered_work_frame <- function(project, dataset, side, node, graph, spec, include_generated = FALSE, states = NULL) {
  code <- toupper(node$paramcd %||% "")
  keys <- layer_key_names(spec,dataset,code,graph,project)
  states <- states %||% state_nodes(project, dataset)
  records_id <- paste0(toupper(dataset), ".RECORDS@", code)
  path <- file.path(side_run_dir(project, dataset, records_id, side), "out.rds")
  out <- if (file.exists(path) && (!isTRUE(include_generated) || node_side_result_available(project,dataset,records_id,side,TRUE,states)))
    tryCatch(plain_frame(readRDS(path)), error=function(e) NULL) else NULL
  if (is.null(out)) out <- empty_key_frame(keys)
  ids <- layer_node_ids(graph, code)
  for (id in setdiff(ids, c(records_id, node$id))) {
    if (!node_side_result_available(project,dataset,id,side,include_generated,states)) next
    p <- file.path(side_run_dir(project, dataset, id, side), "out.rds")
    if (!file.exists(p)) next
    piece <- tryCatch(plain_frame(readRDS(p)), error=function(e) NULL); var <- parse_node_id(id)$var
    if (is.null(piece) || !var %in% names(piece)) next
    by <- intersect(keys, intersect(names(out), names(piece)))
    if (length(by)) out <- merge(out, piece[, unique(c(by,var)),drop=FALSE], by=by, all.x=TRUE, sort=FALSE)
  }
  # Dataset-level values are keyed by USUBJID, even when the layer has repeated records.
  dsids <- vapply(Filter(function(n) identical(n$kind, "dataset_variable"), graph$nodes), `[[`, "", "id")
  for (id in dsids) {
    if (!node_side_result_available(project,dataset,id,side,include_generated,states)) next
    p <- file.path(side_run_dir(project, dataset, id, side), "out.rds")
    if (!file.exists(p)) next
    piece <- tryCatch(plain_frame(readRDS(p)), error=function(e) NULL); var <- parse_node_id(id)$var
    if (is.null(piece) || !"USUBJID" %in% names(piece) || !var %in% names(piece)) next
    by <- intersect("USUBJID", names(out)); if (length(by)) out <- merge(out, piece[, c(by,var),drop=FALSE], by=by, all.x=TRUE, sort=FALSE)
  }
  for (k in keys) if (!k %in% names(out)) out[[k]] <- if (nrow(out)) NA_character_ else character()
  out
}

finished_layers_for_side <- function(project, dataset, side, graph, spec, target_code) {
  pg <- tryCatch(param_graph_read(project, dataset), error=function(e) NULL)
  if (is.null(pg)) return(list())
  order <- param_layer_order(spec, dataset, pg)$order
  target_at <- match(toupper(target_code), order)
  before <- if (is.na(target_at) || target_at <= 1L) character() else order[seq_len(target_at-1L)]
  direct_members <- unique(vapply(Filter(function(e) toupper(e$to) == toupper(target_code), pg$edges %||% list()), function(e) toupper(e$from), ""))
  carry <- unique(c(toupper(flat_chr(pg$carry_members %||% character())), vapply(Filter(function(x) identical(x$classification,"carry_over"), as_records(pg$classification$parameters %||% pg$parameter_classification$parameters)), function(x) toupper(x$paramcd %||% ""), "")))
  direct <- unique(ifelse(direct_members %in% carry, "CARRY_OVER", direct_members))
  codes <- if (identical(side, "primary")) intersect(before, direct) else order
  out <- list()
  for (code in codes) {
    frame <- read_layer_frame(project, dataset, side, code, graph, spec)
    if (is.null(frame)) next
    if (identical(side, "primary")) {
      if (identical(code, "CARRY_OVER")) {
        out[[code]] <- frame
        next
      }
      allow <- unique(c(layer_key_names(spec,dataset,code,graph,project),
        param_layer_read_vars(spec, dataset, pg, target_code,graph,project)))
      out[[code]] <- frame[, intersect(allow, names(frame)), drop=FALSE]
    } else out[[code]] <- frame
  }
  out
}

# ---- project R packages ----------------------------------------------------
# One list for both sides, stored beside state.json so a package edit does not
# reset any variable. base, stats, utils, and methods are always available and
# are not stored. An override replaces the list for that side only.

base_r_packages <- function() c("base", "stats", "utils", "methods")

default_r_packages <- function() "dplyr"

pkg_root <- function(project) {
  if (is.null(project)) return(NULL)
  if (is.character(project)) return(as.character(project)[1])
  if (exists("project_dir_of", mode = "function")) return(project_dir_of(project))
  if (is.list(project) && !is.null(project$dir)) return(as.character(project$dir)[1])
  stop("No project directory.", call. = FALSE)
}

r_packages_file <- function(project) {
  file.path(pkg_root(project), ".dualdag", "state", "r_packages.json")
}

valid_pkg_name <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && grepl("^[A-Za-z][A-Za-z0-9.]*$", x)
}

normalize_pkg_names <- function(x) {
  if (is.null(x)) return(character())
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  x <- trimws(as.character(x))
  x <- x[!is.na(x) & nzchar(x)]
  x <- x[!x %in% base_r_packages()]
  bad <- x[!vapply(x, valid_pkg_name, logical(1))]
  if (length(bad)) stop(sprintf("Invalid package name: %s.", bad[[1]]), call. = FALSE)
  if (length(x) > 40L) stop("At most 40 packages.", call. = FALSE)
  unique(x)
}

as_pkg_field <- function(x) {
  if (is.null(x)) return(character())
  if (is.character(x) && length(x) == 1L && !is.na(x)) x <- list(x)
  normalize_pkg_names(x)
}

default_r_package_config <- function() {
  list(
    r_packages = default_r_packages(),
    override = FALSE,
    r_packages_primary = NULL,
    r_packages_validation = NULL
  )
}

read_r_packages <- function(project) {
  if (is.null(project)) return(default_r_package_config())
  path <- r_packages_file(project)
  if (!file.exists(path)) return(default_r_package_config())
  raw <- if (exists("read_json", mode = "function")) tryCatch(read_json(path), error = function(e) NULL) else NULL
  if (!is.list(raw)) return(default_r_package_config())
  pkgs <- if (is.null(raw$r_packages)) default_r_packages() else as_pkg_field(raw$r_packages)
  list(
    r_packages = pkgs,
    override = isTRUE(raw$override),
    r_packages_primary = if (is.null(raw$r_packages_primary)) NULL else as_pkg_field(raw$r_packages_primary),
    r_packages_validation = if (is.null(raw$r_packages_validation)) NULL else as_pkg_field(raw$r_packages_validation)
  )
}

side_r_packages <- function(project, side = "primary") {
  side <- tryCatch(match.arg(as.character(side %||% "primary")[1], c("primary", "validation")), error = function(e) "primary")
  cfg <- read_r_packages(project)
  if (!isTRUE(cfg$override)) return(cfg$r_packages)
  key <- if (identical(side, "validation")) "r_packages_validation" else "r_packages_primary"
  val <- cfg[[key]]
  if (is.null(val)) cfg$r_packages else val
}

r_packages_rule <- function(packages = NULL) {
  pkgs <- if (is.null(packages)) default_r_packages() else as.character(packages)
  pkgs <- pkgs[!is.na(pkgs) & nzchar(pkgs)]
  listed <- if (length(pkgs)) paste(pkgs, collapse = ", ") else "none"
  paste0(
    "Packages already attached on this side: ", listed, ". ",
    "base, stats, utils, and methods are also available. ",
    "Prefer these. If another package is really needed, call library(pkg) at the top of the program."
  )
}

r_packages_prompt <- function(packages) {
  pkgs <- as.character(packages %||% character())
  pkgs <- pkgs[!is.na(pkgs) & nzchar(pkgs)]
  listed <- if (length(pkgs)) paste(pkgs, collapse = ", ") else "none"
  paste0("PACKAGES: ", listed, "\n", r_packages_rule(pkgs), "\n")
}

r_packages_save <- function(project, body) {
  if (!is.list(body)) stop("Expected a JSON object.", call. = FALSE)
  cur <- read_r_packages(project)
  pkgs <- if (is.null(body$r_packages)) cur$r_packages else as_pkg_field(body$r_packages)
  over <- if (is.null(body$override)) isTRUE(cur$override) else isTRUE(body$override)
  primary <- cur$r_packages_primary
  validation <- cur$r_packages_validation
  if (!is.null(body$r_packages_primary)) primary <- as_pkg_field(body$r_packages_primary)
  if (!is.null(body$r_packages_validation)) validation <- as_pkg_field(body$r_packages_validation)
  if (!over) {
    primary <- NULL
    validation <- NULL
  } else {
    if (is.null(primary)) primary <- pkgs
    if (is.null(validation)) validation <- pkgs
  }
  payload <- list(
    r_packages = as.list(pkgs),
    override = over,
    r_packages_primary = if (is.null(primary)) NULL else as.list(primary),
    r_packages_validation = if (is.null(validation)) NULL else as.list(validation)
  )
  path <- r_packages_file(project)
  if (exists("write_json", mode = "function")) write_json(payload, path) else {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", pretty = TRUE)), path)
  }
  r_packages_public(project)
}

r_packages_public <- function(project) {
  cfg <- read_r_packages(project)
  names <- unique(c(
    cfg$r_packages,
    if (is.null(cfg$r_packages_primary)) character() else cfg$r_packages_primary,
    if (is.null(cfg$r_packages_validation)) character() else cfg$r_packages_validation
  ))
  installed <- lapply(names, function(nm) {
    ok <- isTRUE(requireNamespace(nm, quietly = TRUE))
    list(name = nm, version = if (ok) as.character(utils::packageVersion(nm)) else NULL, installed = ok)
  })
  list(
    r_packages = I(cfg$r_packages),
    override = isTRUE(cfg$override),
    r_packages_primary = if (is.null(cfg$r_packages_primary)) NULL else I(cfg$r_packages_primary),
    r_packages_validation = if (is.null(cfg$r_packages_validation)) NULL else I(cfg$r_packages_validation),
    installed = I(installed)
  )
}

expr_package_name <- function(pd, expr_id) {
  kids <- pd[pd$parent == expr_id, , drop = FALSE]
  if (nrow(kids) == 1L && kids$token[[1]] %in% c("SYMBOL", "STR_CONST")) {
    txt <- kids$text[[1]]
    if (identical(kids$token[[1]], "STR_CONST")) {
      txt <- tryCatch(as.character(parse(text = txt)[[1]]), error = function(e) txt)
    }
    return(list(name = txt, literal = TRUE))
  }
  list(name = "", literal = FALSE)
}

# Packages named by library/require/requireNamespace or by pkg:: / pkg:::.
# A parse error returns no hits; the run reports the syntax error itself.
code_package_info <- function(code) {
  code <- paste(as.character(code %||% ""), collapse = "\n")
  if (!nzchar(trimws(code))) return(list(packages = character(), nonliteral_library = FALSE))
  parsed <- tryCatch(parse(text = code, keep.source = TRUE), error = function(e) NULL)
  if (is.null(parsed)) return(list(packages = character(), nonliteral_library = FALSE))
  pd <- getParseData(parsed, includeText = TRUE)
  if (is.null(pd) || !nrow(pd)) return(list(packages = character(), nonliteral_library = FALSE))
  used <- character()
  nonliteral_library <- FALSE
  ns <- pd$token %in% c("NS_GET", "NS_GET_INT")
  for (i in which(ns)) {
    kids <- pd[pd$parent == pd$parent[[i]] & pd$token == "SYMBOL_PACKAGE", , drop = FALSE]
    if (nrow(kids)) used <- c(used, kids$text[[1]])
  }
  calls <- which(pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% c("library", "require", "requireNamespace"))
  for (i in calls) {
    fun_expr <- pd$parent[[i]]
    call_hit <- pd$parent[pd$id == fun_expr]
    if (!length(call_hit)) next
    kids <- pd[pd$parent == call_hit[[1]], , drop = FALSE]
    kids <- kids[order(kids$id), , drop = FALSE]
    name <- ""
    literal <- FALSE
    sub_i <- which(kids$token == "SYMBOL_SUB" & kids$text %in% c("package", "pkg"))
    expr_rows <- kids[kids$token == "expr" & kids$id != fun_expr, , drop = FALSE]
    if (length(sub_i)) {
      after <- kids[(sub_i[[1]] + 1L):nrow(kids), , drop = FALSE]
      named <- after[after$token == "expr", , drop = FALSE]
      if (nrow(named)) {
        got <- expr_package_name(pd, named$id[[1]])
        name <- got$name
        literal <- got$literal
      }
    } else if (nrow(expr_rows)) {
      got <- expr_package_name(pd, expr_rows$id[[1]])
      name <- got$name
      literal <- got$literal
    }
    if (!literal) {
      if (identical(pd$text[[i]], "library")) nonliteral_library <- TRUE
    } else if (nzchar(name)) used <- c(used, name)
  }
  list(packages = unique(used[!is.na(used) & nzchar(used)]), nonliteral_library = nonliteral_library)
}

code_required_packages <- function(code) code_package_info(code)$packages

code_disallowed_packages <- function(code, allowed = NULL) {
  if (isTRUE(code_package_info(code)$nonliteral_library))
    return("A library() call must name a package literally.")
  NULL
}

missing_r_package_names <- function(packages) {
  packages <- as.character(packages %||% character())
  packages <- packages[!is.na(packages) & nzchar(packages)]
  packages <- packages[!packages %in% base_r_packages()]
  unique(packages[!vapply(packages, function(nm) isTRUE(requireNamespace(nm, quietly = TRUE)), logical(1))])
}

package_install_message <- function(packages) {
  bad <- unique(as.character(packages %||% character()))
  bad <- bad[!is.na(bad) & nzchar(bad)]
  if (!length(bad)) return(NULL)
  quoted <- vapply(bad, function(nm) as.character(jsonlite::toJSON(nm, auto_unbox = TRUE)), "")
  install <- if (length(bad) == 1L) paste0("install.packages(", quoted[[1]], ")") else
    paste0("install.packages(c(", paste(quoted, collapse = ", "), "))")
  noun <- if (length(bad) == 1L) "package" else "packages"
  names <- if (length(bad) == 1L) bad[[1]] else paste(bad, collapse = ", ")
  verbs <- if (length(bad) == 1L) "is not installed. Install it" else "are not installed. Install them"
  paste0("This program needs R ", noun, " ", names, ", which ", verbs,
         " in the R at ", R.home(), ": ", install, ". Then press Re-run.")
}

missing_r_packages <- function(packages) {
  package_install_message(missing_r_package_names(packages))
}

missing_r_package_from_error <- function(error) {
  error <- paste(as.character(error %||% ""), collapse = "\n")
  patterns <- c("there is no package called ['\u2018\u2019]([^'\u2018\u2019]+)['\u2018\u2019]",
                "there is no package called \"([^\"]+)\"")
  for (pattern in patterns) {
    hit <- regmatches(error, regexec(pattern, error, ignore.case = TRUE, perl = TRUE))[[1]]
    if (length(hit) > 1L && nzchar(hit[[2]]) ) return(hit[[2]])
  }
  ""
}

is_missing_r_package_error <- function(error) {
  grepl("^This program needs R packages? ", as.character(error %||% "")[1], perl = TRUE) ||
    nzchar(missing_r_package_from_error(error))
}

# Self-contained worker. callr ships this function into a fresh R process.
# log.txt is one comment header, then only the program's own R console output.
# Post-run checks stay in `error`.
side_worker <- function(code, src, upstream, keys, log_path, out_path, workdir, packages = NULL, layers = list(), temp_name = NULL, legacy_temp_name = NULL, output_cols = NULL, dataset_name = NULL, primary = FALSE) {
  if (!is.null(workdir) && nzchar(workdir)) {
    dir.create(workdir, recursive = TRUE, showWarnings = FALSE)
    setwd(workdir)
  }
  if (is.null(keys)) keys <- character()
  box <- new.env(parent = emptyenv())
  box$ok <- FALSE
  box$err <- NULL
  # Parent is the global env so the default packages (stats, utils, methods) are visible to side code.
  env <- new.env(parent = globalenv())
  env$src <- src
  if (isTRUE(primary) && length(src)) for (domain in names(src))
    env[[paste0("LINEAGE_SRC_",toupper(domain))]] <- src[[domain]]
  # `start_side_proc` applies the side's input policy before handing the frame
  # to this worker. Bind every public frame name to that same restricted input.
  inherited <- if (is.data.frame(upstream)) as.data.frame(upstream, stringsAsFactors = FALSE) else data.frame()
  env$work <- inherited
  env$upstream <- inherited
  env$keys <- keys
  env$layers <- layers
  if (is.null(temp_name) || !nzchar(as.character(temp_name)[1])) temp_name <- "DATASET_TEMP"
  env[[as.character(temp_name)[1]]] <- inherited
  if (is.null(legacy_temp_name) || !nzchar(as.character(legacy_temp_name)[1])) legacy_temp_name <- temp_name
  env[[as.character(legacy_temp_name)[1]]] <- inherited
  if (!is.null(dataset_name) && length(layers)) for (pc in names(layers)) {
    safe_pc <- gsub("[^A-Za-z0-9_.]", "_", toupper(pc), perl=TRUE)
    env[[paste0(toupper(dataset_name),"_",safe_pc,"_TEMP")]] <- layers[[pc]]
    if (isTRUE(primary)) env[[paste0("LINEAGE_",safe_pc)]] <- layers[[pc]]
  }
  con <- file(log_path, open = "wt", encoding = "UTF-8")
  sunk <- FALSE
  close_log <- function() {
    if (!isTRUE(sunk)) return()
    sunk <<- FALSE
    try(sink(type = "message"), silent = TRUE)
    try(sink(type = "output"), silent = TRUE)
    try(flush(con), silent = TRUE)
    try(close(con), silent = TRUE)
  }
  on.exit(close_log(), add = TRUE)
  sink(con, type = "output", split = FALSE)
  sink(con, type = "message")
  sunk <- TRUE
  old_opt <- options(warn = 1, keep.source = TRUE)
  on.exit(options(old_opt), add = TRUE)
  if (is.null(packages)) packages <- "dplyr"
  packages <- as.character(packages)
  packages <- packages[!is.na(packages) & nzchar(packages)]
  attach_err <- NULL
  tryCatch({
    vers <- character()
    for (pkg in packages) {
      suppressMessages(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
      vers <- c(vers, paste0(pkg, " ", as.character(utils::packageVersion(pkg))))
    }
    pkg_txt <- if (length(vers)) paste(vers, collapse = ", ") else "none"
    cat("# R ", as.character(getRversion()), "; packages: ", pkg_txt, "\n", sep = "")
  }, error = function(e) {
    attach_err <<- conditionMessage(e)
    cat("Error: ", attach_err, "\n", sep = "")
  })
  if (!is.null(attach_err)) {
    box$err <- attach_err
    close_log()
    return(list(ok = FALSE, error = box$err))
  }
  failed <- FALSE
  tryCatch({
    src_code <- if (is.null(code)) "" else code
    exprs <- parse(text = src_code, keep.source = TRUE)
    source(exprs = exprs, local = env, echo = FALSE, print.eval = TRUE,
           keep.source = TRUE)
  }, error = function(e) {
    failed <<- TRUE
    box$err <- conditionMessage(e)
    call <- conditionCall(e)
    if (is.null(call) || (is.call(call) && identical(call[[1L]], as.name("eval")))) {
      cat("Error: ", box$err, "\n", sep = "")
    } else {
      cat("Error in ", paste(deparse(call, nlines = 1L), collapse = ""), " : ", box$err, "\n", sep = "")
    }
    cat("Execution halted\n")
  })
  close_log()
  if (!failed) {
    # Prefer the established result variable when a stored program creates it.
    # New programs write the side-specific temporary dataset directly.
    value_name <- if (exists("result", envir = env, inherits = FALSE)) "result" else {
      candidates <- unique(c(as.character(temp_name)[1],as.character(legacy_temp_name)[1]))
      frames <- candidates[vapply(candidates,function(nm)exists(nm,envir=env,inherits=FALSE) && is.data.frame(get(nm,envir=env,inherits=FALSE)),TRUE)]
      target_cols <- setdiff(as.character(output_cols %||% character()),as.character(keys %||% character()))
      matching <- frames[vapply(frames,function(nm)length(intersect(target_cols,names(get(nm,envir=env,inherits=FALSE))))>0L,TRUE)]
      changed <- frames[!vapply(frames,function(nm)identical(get(nm,envir=env,inherits=FALSE),inherited),TRUE)]
      if (length(matching)) matching[[1L]] else if (length(changed)) changed[[1L]] else if (length(frames)) frames[[1L]] else as.character(temp_name)[1]
    }
    if (!exists(value_name, envir = env, inherits = FALSE)) {
      box$err <- paste0("Code must update `", as.character(temp_name)[1], "`.")
    } else {
      result <- get(value_name, envir = env, inherits = FALSE)
      if (!is.data.frame(result)) {
        box$err <- paste0("`", value_name, "` must be a data.frame.")
      } else {
        result <- as.data.frame(result, stringsAsFactors = FALSE)
        expected_cols <- if (is.null(output_cols)) character() else as.character(output_cols)
        if (!is.null(output_cols)) result <- result[, intersect(expected_cols, names(result)), drop = FALSE]
        rownames(result) <- NULL
        if (is.data.frame(upstream) && nrow(upstream) > 0L && nrow(result) == 0L) {
          box$err <- "produced 0 rows"
        } else {
          saveRDS(result, out_path)
          box$ok <- TRUE
        }
      }
    }
  }
  list(ok = box$ok, error = box$err)
}

read_log_file <- function(path) {
  if (!file.exists(path)) return("")
  n <- file.info(path)$size
  if (is.na(n) || n <= 0) return("")
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

finish_side_proc <- function(proc, prep, timeout) {
  if (is.null(proc)) {
    log <- paste(c(prep$error, read_log_file(prep$stdout), read_log_file(prep$stderr)), collapse = "\n")
    log <- trimws(log)
    if (nzchar(log)) write_utf8(log, prep$log_path)
    if (exists("save_log_tags", mode = "function")) save_log_tags(prep$log_path, log)
    return(list(ok = FALSE, log = log, out_path = NULL, error = prep$error %||% "Failed to start the side process",
                missing_package = ""))
  }
  proc$wait(as.integer(max(1, timeout) * 1000))
  timed_out <- isTRUE(proc$is_alive())
  if (timed_out) tryCatch(proc$kill(), error = function(e) NULL)
  res <- if (timed_out) list(ok = FALSE, error = "Timed out") else {
    tryCatch(proc$get_result(), error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  }
  log <- read_log_file(prep$log_path)
  ok <- isTRUE(res$ok) && file.exists(prep$out_path)
  missing_pkg <- if (!ok) missing_r_package_from_error(res$error) else ""
  if (!nzchar(missing_pkg) && !ok) missing_pkg <- missing_r_package_from_error(log)
  error <- if (ok) NULL else (res$error %||% "Side failed")
  if (nzchar(missing_pkg)) {
    error <- package_install_message(missing_pkg)
    if (!grepl(error, log, fixed = TRUE)) log <- paste(c(log, error), collapse = "\n")
    write_utf8(log, prep$log_path)
  }
  if (exists("save_log_tags", mode = "function")) save_log_tags(prep$log_path, log)
  list(
    ok = ok,
    log = log,
    out_path = if (file.exists(prep$out_path)) prep$out_path else NULL,
    error = if (ok) NULL else error,
    missing_package = missing_pkg
  )
}

start_side_proc <- function(project, dataset, node, side, code, graph, spec) {
  dataset <- toupper(dataset)
  side <- match.arg(side, c("primary", "validation"))
  dir <- side_run_dir(project, dataset, node, side)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  code_path <- file.path(dir, "code.R")
  write_utf8(code %||% "", code_path)
  if (is.null(graph)) graph <- load_project_graph(project, dataset)
  if (is.null(spec)) spec <- load_project_spec(project)
  keys <- spec_key_names(spec, dataset)
  ni_for_keys <- node_record(node)
  if (isTRUE(graph$layered) && !is.null(ni_for_keys$paramcd)) keys <- layer_key_names(spec,dataset,ni_for_keys$paramcd,graph,project)
  if (!isTRUE(graph$layered) && exists("layer_keys_path",mode="function") && file.exists(layer_keys_path(project,dataset))) keys <- layer_key_names(spec,dataset,"*",graph,project)
  if (!isTRUE(graph$layered) && identical(ni_for_keys$kind,"records")) keys <- unique(flat_chr(graph$key_vars %||% keys))
  prep <- list(
    dir = dir,
    log_path = file.path(dir, "log.txt"),
    out_path = file.path(dir, "out.rds"),
    stdout = file.path(dir, "stdout.txt"),
    stderr = file.path(dir, "stderr.txt"),
    error = NULL
  )
  if (file.exists(prep$out_path)) unlink(prep$out_path)
  pkgs <- side_r_packages(project, side)
  blocked <- code_disallowed_packages(code, pkgs)
  if (is.null(blocked)) blocked <- missing_r_packages(c(pkgs, code_required_packages(code)))
  if (!is.null(blocked)) {
    prep$error <- blocked
    return(list(proc = NULL, prep = prep))
  }
  if (!isTRUE(graph$layered)) tryCatch(refresh_work_side(project, dataset, side, spec, graph), error = function(e) NULL)
  layered_node <- isTRUE(graph$layered) && !is.null((node_record(node))$paramcd)
  upstream <- if (layered_node) NULL else if (side_is_keys_step(ni_for_keys,keys)) empty_key_frame(keys) else
    tryCatch(build_upstream(project, dataset, node, side, graph, keys, spec = spec), error = function(e) e)
  if (inherits(upstream, "error")) {
    prep$error <- conditionMessage(upstream)
    return(list(proc = NULL, prep = prep))
  }
  src <- tryCatch(load_src(project), error = function(e) e)
  if (inherits(src, "error")) {
    prep$error <- conditionMessage(src)
    return(list(proc = NULL, prep = prep))
  }
  source_domain_names <- names(src) %||% character()
  layers <- list()
  if (layered_node) {
    ni <- node_record(node)
    layers <- tryCatch(finished_layers_for_side(project, dataset, side, graph, spec, ni$paramcd), error=function(e) e)
    if (inherits(layers, "error")) { prep$error <- conditionMessage(layers); return(list(proc=NULL, prep=prep)) }
    upstream <- if (identical(ni$kind,"records")) empty_key_frame(keys) else
      tryCatch(layered_work_frame(project, dataset, side, ni, graph, spec), error=function(e) e)
    if (inherits(upstream, "error")) { prep$error <- conditionMessage(upstream); return(list(proc=NULL, prep=prep)) }
  }
  if (identical(side, "primary") && !is.null(spec) && !is.null(graph)) {
    allowed <- lineage_context(spec, graph, node, project, side)
    allowed_source_refs <- side_primary_source_refs(spec, dataset, keys, allowed, node, src)
    cols <- if (exists("data_index", mode = "function")) data_index(project) else list()
    layer_bindings <- character()
    if(length(layers)) {
      legacy_layer_names <- vapply(names(layers),function(pc)toupper(side_temp_name(dataset,paramcd=pc)),"")
      layer_bindings <- setNames(names(layers),legacy_layer_names)
      if (identical(side,"primary")) layer_bindings <- c(layer_bindings,setNames(names(layers),vapply(names(layers),function(pc)toupper(side_lineage_name(dataset,paramcd=pc)),"")))
    }
    dataset_bindings <- character()
    source_bindings <- character()
    if (identical(side,"primary")) {
      dataset_bindings <- setNames(c("current","current"),toupper(c(side_program_name(dataset,node,side),side_temp_name(dataset,node))))
      if (length(source_domain_names)) source_bindings <- setNames(toupper(source_domain_names),paste0("LINEAGE_SRC_",toupper(source_domain_names)))
    }
    uses <- if (exists("code_input_uses", mode = "function")) code_input_uses(code, cols, keys,
      layer_bindings=layer_bindings,dataset_bindings=dataset_bindings,source_bindings=source_bindings) else list(ok = FALSE, uses = list())
    if (isTRUE(uses$ok)) {
      outside <- character()
      layer_allowed <- if (isTRUE(graph$layered) && !is.null((node_record(node))$paramcd)) {
        pg <- tryCatch(param_graph_read(project, dataset), error=function(e) NULL)
        if (!is.null(pg)) {
          allowed <- param_layer_visible_columns(spec, dataset, pg, node_record(node)$paramcd,graph,project)
          carry <- unique(c(toupper(flat_chr(pg$carry_members %||% character())), vapply(Filter(function(x) identical(x$classification,"carry_over"), as_records(pg$classification$parameters %||% pg$parameter_classification$parameters)), function(x) toupper(x$paramcd %||% ""), "")))
          carry_reads <- unlist(allowed[intersect(names(allowed), carry)], use.names=FALSE)
          allowed[intersect(names(allowed), carry)] <- NULL
          if (length(carry_reads)) allowed$CARRY_OVER <- unique(c(allowed$CARRY_OVER,carry_reads))
          allowed
        } else list()
      } else list()
      for (u in uses$uses) {
        if (identical(u$kind, "layer")) {
          pc <- toupper(u$domain %||% ""); col <- toupper(u$variable %||% "")
          if (is.null(layer_allowed[[pc]]) || !col %in% unique(c(keys,layer_allowed[[pc]]))) outside <- c(outside, paste0("layers$",pc,"$",col))
          next
        }
        if (identical(u$kind,"layer_all")) {
          if(is.null(layer_allowed[[toupper(u$domain %||% "")]])) outside<-c(outside,paste0("layers$",toupper(u$domain %||% "")))
          next
        }
        ref <- if (identical(u$kind, "domain")) toupper(u$origin) else paste0(toupper(dataset), ".", toupper(u$variable))
        own_target <- identical(toupper(u$kind), "UPSTREAM") &&
          identical(toupper(u$variable %||% ""), toupper(node_record(node)$var %||% ""))
        adsl_source_exception <- !identical(toupper(dataset), "ADSL") &&
          identical(toupper(side), "PRIMARY") && identical(u$kind, "domain") &&
          startsWith(toupper(u$origin %||% ""), "ADSL.") && "ADSL" %in% source_domain_names
        upstream_allowed <- identical(u$kind, "upstream") && lineage_upstream_ref_allowed(ref, node, allowed$ids)
        if (!own_target && !adsl_source_exception && !upstream_allowed && !ref %in% c(allowed$ids, allowed_source_refs, paste0(toupper(dataset), ".", toupper(keys)))) outside <- c(outside, ref)
      }
      outside <- unique(outside)
    }
    src <- side_filter_source_frames(src, dataset, side, allowed_source_refs, keys)
    keep_work <- unique(c(keys, vapply(allowed$ids, function(x) parse_node_id(x)$var %||% "", "")))
    if (isTRUE(graph$layered) && !is.null((node_record(node))$paramcd)) {
      ni <- node_record(node)
      keep_work <- unique(c(keys, "PARAMCD", vapply(Filter(function(n) identical(n$kind,"dataset_variable"),graph$nodes), `[[`, "", "var"),
        vapply(Filter(function(n) identical(n$paramcd,ni$paramcd) && n$kind=="param_variable",graph$nodes), `[[`, "", "var")))
    }
    upstream <- upstream[, intersect(names(upstream), keep_work), drop = FALSE]
  }
  ni <- node_record(node)
  temp_name <- side_program_name(dataset,ni,side)
  legacy_temp_name <- side_temp_name(dataset,ni)
  output_cols <- if (identical(ni$kind, "records")) keys else if (identical(ni$kind,"dataset_variable")) unique(c("USUBJID",ni$var)) else unique(c(keys, ni$var))
  proc <- tryCatch(
    callr::r_bg(
      side_worker,
      args = list(code = code, src = src, upstream = upstream, keys = keys, layers = layers,
                  temp_name = temp_name, legacy_temp_name=legacy_temp_name, output_cols = output_cols, dataset_name = dataset, primary=identical(side,"primary"),
                  log_path = prep$log_path, out_path = prep$out_path, workdir = dir,
                  packages = pkgs),
      stdout = prep$stdout, stderr = prep$stderr,
      package = FALSE, supervise = TRUE
    ),
    error = function(e) e
  )
  if (inherits(proc, "error")) {
    prep$error <- conditionMessage(proc)
    proc <- NULL
  }
  list(proc = proc, prep = prep)
}

run_side <- function(project, dataset, node, side, code, timeout = 120,
                     graph = NULL, spec = NULL, description = NULL) {
  node <- node_record(node, graph)
  started <- start_side_proc(project, dataset, node, side, code, graph, spec)
  result <- finish_side_proc(started$proc, started$prep, timeout)
  result$process_started <- !is.null(started$proc)
  node_info <- node_record(node, graph)
  if (isTRUE(graph$layered) && isTRUE(result$ok) && identical(node_info$kind, "dataset_variable")) {
    df <- tryCatch(readRDS(result$out_path), error=function(e) NULL)
    if (is.data.frame(df) && "USUBJID" %in% names(df) && anyDuplicated(df$USUBJID)) {
      result$ok <- FALSE; result$error <- "Dataset-level result contains duplicate USUBJID values."
      result$log <- paste(result$log, "Error:", result$error); card_remove(project, dataset, node, side)
    }
  }
  if (!is.null(graph) && isTRUE(result$ok) && identical(node_info$kind, "records")) {
    df <- tryCatch(readRDS(result$out_path), error=function(e) NULL)
    keys <- spec_key_names(spec %||% load_project_spec(project), dataset)
    if(isTRUE(graph$layered)&&!is.null(node_info$paramcd)) keys <- layer_key_names(spec %||% load_project_spec(project),dataset,node_info$paramcd,graph,project)
    if(!isTRUE(graph$layered)&&exists("layer_keys_path",mode="function")&&file.exists(layer_keys_path(project,dataset)))keys<-layer_key_names(spec %||% load_project_spec(project),dataset,"*",graph,project)
    required<-keys
    problem <- if (!is.data.frame(df) || !all(required %in% names(df))) paste0("Records result must contain key columns: ",paste(required,collapse=", "),".") else NULL
    if(is.null(problem) && !is.null(node_info$paramcd)) {
      allowed_params <- if (identical(toupper(node_info$paramcd), "CARRY_OVER")) {
        rec <- Filter(function(x) identical(toupper(x$paramcd %||% ""), "CARRY_OVER"), graph$layers %||% list())
        if (length(rec)) toupper(flat_chr(rec[[1]]$members_parameters)) else character()
      } else toupper(node_info$paramcd)
      bad_param <- is.na(df$PARAMCD) | !toupper(as.character(df$PARAMCD)) %in% allowed_params
      if (any(bad_param)) problem <- if (identical(toupper(node_info$paramcd), "CARRY_OVER"))
        paste0("Records PARAMCD must be one of ", paste(allowed_params, collapse=", "), ".") else
          paste0("Records PARAMCD must equal ", node_info$paramcd, ".")
    }
    if(is.null(problem) && anyDuplicated(df[, required, drop=FALSE])) {
      dup <- duplicated(df[,required,drop=FALSE]) | duplicated(df[,required,drop=FALSE],fromLast=TRUE)
      sample <- paste(vapply(df[which(dup)[[1]],required,drop=FALSE],as.character,""),collapse=" / ")
      problem <- paste0("Records result has duplicate keys (",sum(dup)," duplicate row(s)); sample: ",sample,".")
    }
    if (!is.null(problem)) { result$ok <- FALSE; result$error <- problem; result$log <- paste(result$log, "Error:", problem); card_remove(project, dataset, node, side) }
  }
  if (isTRUE(result$ok)) card_write(project, dataset, node, side, description, graph, spec)
  else card_remove(project, dataset, node, side)
  result
}

# Launch every requested side before waiting, so one error cannot cancel the other.
run_sides <- function(project, dataset, node, codes, timeout = 120,
                      graph = NULL, spec = NULL) {
  node <- node_record(node, graph)
  codes <- codes[names(codes) %in% c("primary", "validation")]
  # Both workdirs exist before either process starts, so the sides can see each other.
  for (side in names(codes)) dir.create(side_run_dir(project, dataset, node, side), recursive = TRUE, showWarnings = FALSE)
  started <- list()
  for (side in names(codes)) {
    started[[side]] <- start_side_proc(project, dataset, node, side, codes[[side]], graph, spec)
  }
  out <- list()
  for (side in names(started)) {
    out[[side]] <- finish_side_proc(started[[side]]$proc, started[[side]]$prep, timeout)
    ni <- node_record(node, graph)
    if (isTRUE(graph$layered) && isTRUE(out[[side]]$ok) && identical(ni$kind,"dataset_variable")) {
      df<-tryCatch(readRDS(out[[side]]$out_path),error=function(e)NULL)
      if(is.data.frame(df)&&"USUBJID"%in%names(df)&&anyDuplicated(df$USUBJID)) {
        out[[side]]$ok<-FALSE;out[[side]]$error<-"Dataset-level result contains duplicate USUBJID values."
        out[[side]]$log<-paste(out[[side]]$log,"Error:",out[[side]]$error)
      }
    }
    if (!is.null(graph) && isTRUE(out[[side]]$ok) && identical(ni$kind, "records")) {
      df <- tryCatch(readRDS(out[[side]]$out_path), error=function(e) NULL); keys <- spec_key_names(spec, dataset)
      if(isTRUE(graph$layered)&&!is.null(ni$paramcd)) keys <- layer_key_names(spec,dataset,ni$paramcd,graph,project)
      if(!isTRUE(graph$layered)&&exists("layer_keys_path",mode="function")&&file.exists(layer_keys_path(project,dataset)))keys<-layer_key_names(spec,dataset,"*",graph,project)
      required<-keys
      problem <- if (!is.data.frame(df) || !all(required %in% names(df))) paste0("Records result must contain key columns: ",paste(required,collapse=", "),".") else NULL
      if (is.null(problem) && !is.null(ni$paramcd)) {
        allowed_params <- if (identical(toupper(ni$paramcd), "CARRY_OVER")) {
          rec <- Filter(function(x) identical(toupper(x$paramcd %||% ""), "CARRY_OVER"), graph$layers %||% list())
          if (length(rec)) toupper(flat_chr(rec[[1]]$members_parameters)) else character()
        } else toupper(ni$paramcd)
        if (any(is.na(df$PARAMCD) | !toupper(as.character(df$PARAMCD)) %in% allowed_params))
          problem <- if (identical(toupper(ni$paramcd), "CARRY_OVER"))
            paste0("Records PARAMCD must be one of ", paste(allowed_params, collapse=", "), ".") else
              paste0("Records PARAMCD must equal ", ni$paramcd, ".")
      }
      if(is.null(problem) && anyDuplicated(df[,required,drop=FALSE])) {
        dup<-duplicated(df[,required,drop=FALSE])|duplicated(df[,required,drop=FALSE],fromLast=TRUE)
        sample<-paste(vapply(df[which(dup)[[1]],required,drop=FALSE],as.character,""),collapse=" / ")
        problem<-paste0("Records result has duplicate keys (",sum(dup)," duplicate row(s)); sample: ",sample,".")
      }
      if (!is.null(problem)) { out[[side]]$ok <- FALSE; out[[side]]$error <- problem; out[[side]]$log <- paste(out[[side]]$log,"Error:",problem) }
    }
    if (isTRUE(out[[side]]$ok)) card_write(project, dataset, node, side, graph = graph, spec = spec)
    else card_remove(project, dataset, node, side)
  }
  out
}

# Re-run code already stored in code.R. Does not call the LLM and does not change the code.
read_stored_code <- function(path) {
  if (!file.exists(path) || dir.exists(path)) return(NULL)
  n <- file.info(path)$size
  if (is.na(n) || n <= 0) return(NULL)
  txt <- rawToChar(readBin(path, "raw", n))
  Encoding(txt) <- "UTF-8"
  if (!nzchar(trimws(txt))) return(NULL)
  txt
}

run_stored_sides <- function(project, dataset, node, side = "both", timeout = 120,
                             graph = NULL, spec = NULL) {
  project <- project_dir_of(project)
  dataset <- toupper(trimws(as.character(dataset)[1]))
  node <- node_record(node, graph)
  side <- as.character(side %||% "both")[1]
  if (!side %in% c("primary", "validation", "both")) {
    stop("side must be primary, validation, or both.", call. = FALSE)
  }
  sides <- if (identical(side, "both")) c("primary", "validation") else side
  codes <- list()
  missing <- character()
  for (s in sides) {
    txt <- read_stored_code(file.path(side_run_dir(project, dataset, node, s), "code.R"))
    if (is.null(txt)) missing <- c(missing, s) else codes[[s]] <- txt
  }
  if (length(missing)) {
    return(list(
      ok = FALSE, missing = missing, results = list(),
      error = paste0("No stored code for ", paste(missing, collapse = " and "), ".")
    ))
  }
  results <- if (length(codes) == 1L) {
    s <- names(codes)[1]
    stats::setNames(
      list(run_side(project, dataset, node, s, codes[[s]], timeout = timeout, graph = graph, spec = spec)),
      s
    )
  } else {
    run_sides(project, dataset, node, codes, timeout = timeout, graph = graph, spec = spec)
  }
  list(ok = TRUE, missing = character(), error = NULL, results = results)
}

# ---- one R expert per side -------------------------------------------------
# Memory is <project>/.dualdag/experts/<DATASET>_<side>.json.
# Only user and agent conversation turns are retained. The prompt reads only this side.

expert_path <- function(project, dataset, side) {
  side <- match.arg(as.character(side %||% "")[1], c("primary", "validation"))
  ds <- toupper(trimws(as.character(dataset %||% "")[1]))
  root <- if (is.character(project)) project else project_dir_of(project)
  file.path(root, ".dualdag", "experts", paste0(ds, "_", side, ".json"))
}

expert_read_file <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !file.exists(path) || dir.exists(path)) return(NULL)
  if (exists("read_text", mode = "function")) return(read_text(path))
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

expert_turns <- function(raw) {
  if (is.null(raw) || !length(raw) || !is.list(raw)) return(list())
  if (!is.null(raw$kind) || !is.null(raw$question) || !is.null(raw$answer) || !is.null(raw$result)) return(list(raw))
  Filter(is.list, raw)
}

expert_read <- function(project, dataset, side) {
  path <- tryCatch(expert_path(project, dataset, side), error = function(e) "")
  if (!nzchar(path) || !file.exists(path)) return(list())
  raw <- if (exists("read_json", mode = "function")) tryCatch(read_json(path), error = function(e) NULL) else {
    tryCatch(jsonlite::fromJSON(expert_read_file(path) %||% "", simplifyVector = FALSE), error = function(e) NULL)
  }
  turns <- expert_turns(raw)
  Filter(function(t) is.list(t) && (t$kind %||% "ask") %in% c("ask", "agent"), turns)
}

expert_append_turn <- function(project, dataset, node, side, turn) {
  if (!is.list(turn)) return(invisible(list()))
  side <- tryCatch(match.arg(as.character(side %||% "")[1], c("primary", "validation")), error = function(e) "")
  if (!nzchar(side)) return(invisible(list()))
  nid <- if (is.list(node)) as.character(node$id %||% "")[1] else as.character(node %||% "")[1]
  turn$at <- as.character(turn$at %||% now_iso())[1]
  turn$node <- nid
  turn$kind <- as.character(turn$kind %||% "ask")[1]
  sid <- if (exists("coder_session_ensure", mode="function")) coder_session_ensure(project,dataset,nid,side) else NULL
  if (!is.null(sid)) { turn$session <- sid; coder_session_append(project,sid,"turns",llm_redact(turn,get_api_key()$key)) }
  hist <- expert_read(project, dataset, side)
  hist[[length(hist) + 1L]] <- turn
  if (length(hist) > 40L) hist <- hist[(length(hist) - 39L):length(hist)]
  path <- expert_path(project, dataset, side)
  if (exists("write_json", mode = "function")) write_json(hist, path) else {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    writeLines(as.character(jsonlite::toJSON(hist, auto_unbox = TRUE, null = "null", pretty = TRUE)), path)
  }
  invisible(hist)
}

expert_clear <- function(project, dataset, side) {
  path <- expert_path(project, dataset, side)
  if (file.exists(path)) unlink(path)
  invisible(TRUE)
}

expert_clip <- function(x, n = 1500L) {
  x <- trimws(gsub("[\r\n]+", " ", as.character(x %||% "")[1]))
  if (!nzchar(x)) return("")
  if (nchar(x) > n) x <- substr(x, 1L, n)
  x
}

expert_format_turn <- function(turn) {
  if (!is.list(turn)) return("")
  kind <- as.character(turn$kind %||% "ask")[1]
  head <- paste0(
    "turn kind=", kind,
    " node=", as.character(turn$node %||% "")[1],
    " at=", as.character(turn$at %||% "")[1]
  )
  add <- function(label, value) {
    value <- expert_clip(value, 600L)
    if (!nzchar(value)) "" else paste0("\n", label, ": ", value)
  }
  body <- paste0(add("Q", turn$message %||% turn$question), add("A", turn$reply %||% turn$answer))
  text <- paste0(head, body)
  if (nchar(text) > 1500L) text <- substr(text, 1L, 1500L)
  text
}

expert_thread_text <- function(project, dataset, side, node) {
  if (is.null(project)) return("")
  turns <- tryCatch(expert_read(project, dataset, side), error = function(e) list())
  if (!length(turns)) return("")
  node_id <- if (is.list(node)) as.character(node$id %||% "")[1] else as.character(node %||% "")[1]
  if (!nzchar(node_id)) return("")
  turns <- Filter(function(turn) identical(as.character(turn$node %||% "")[1],node_id),turns)
  if (!length(turns)) return("")
  if (length(turns) > 10L) turns <- tail(turns, 10L)
  lines <- vapply(turns, expert_format_turn, "")
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return("")
  paste0("THREAD_BEGIN\n", paste(lines, collapse = "\n"), "\nTHREAD_END\n")
}

expert_parents <- function(graph, id) {
  if (is.null(graph) || is.null(graph$edges)) return(character())
  id <- as.character(id %||% "")[1]
  ups <- character()
  for (e in graph$edges) {
    if (!is.list(e)) next
    if (!identical(as.character(e$to %||% "")[1], id)) next
    frm <- as.character(e$from %||% "")[1]
    if (nzchar(frm)) ups <- c(ups, frm)
  }
  unique(ups)
}

expert_ancestor_ids <- function(graph, id) {
  seen <- character()
  wave <- expert_parents(graph, id)
  guard <- 0L
  while (length(wave) && guard < 10000L) {
    guard <- guard + 1L
    wave <- unique(wave[nzchar(wave) & !(wave %in% seen) & wave != id])
    if (!length(wave)) break
    seen <- c(seen, wave)
    nxt <- unique(unlist(lapply(wave, function(u) expert_parents(graph, u)), use.names = FALSE))
    wave <- nxt %||% character()
  }
  seen
}

expert_chain_order <- function(graph, ids) {
  ids <- unique(ids[nzchar(ids)])
  if (!length(ids)) return(character())
  if (exists("inspect_topo", mode = "function")) {
    topo <- tryCatch(inspect_topo(graph), error = function(e) character())
    topo <- topo[topo %in% ids]
    if (length(topo)) return(c(topo, ids[!ids %in% topo]))
  }
  base <- character()
  if (!is.null(graph)) {
    for (n in graph$nodes %||% list()) {
      nid <- if (is.list(n)) as.character(n$id %||% "")[1] else ""
      if (length(nid) == 1L && !is.na(nid) && nzchar(nid) && nid %in% ids) base <- c(base, nid)
    }
  }
  c(base, ids[!ids %in% base])
}

expert_level <- function(project, dataset, id) {
  if (is.null(project)) return("none")
  root <- if (is.character(project)) project else project_dir_of(project)
  nodes <- tryCatch(state_nodes(root, dataset), error = function(e) list())
  st <- nodes[[id]]
  if (!is.list(st)) return("none")
  lvl <- as.character(st$level %||% "none")[1]
  if (!nzchar(lvl) || is.na(lvl)) "none" else lvl
}

expert_side_slot <- function(project, dataset, id, side) {
  if (is.null(project)) return(list())
  root <- if (is.character(project)) project else project_dir_of(project)
  nodes <- tryCatch(state_nodes(root, dataset), error = function(e) list())
  st <- nodes[[id]]
  if (!is.list(st) || !is.list(st$sides)) return(list())
  slot <- st$sides[[side]]
  if (!is.list(slot)) list() else slot
}

expert_side_error <- function(project, dataset, id, side) {
  slot <- expert_side_slot(project, dataset, id, side)
  err <- as.character(slot$error %||% "")[1]
  if (is.na(err)) "" else err
}

expert_work_facts <- function(project, dataset, side, spec = NULL, graph = NULL, exclude_id = NULL) {
  root <- if (is.character(project)) project else project_dir_of(project)
  path <- work_path(root, dataset, side)
  df <- if (file.exists(path)) tryCatch(readRDS(path), error = function(e) NULL) else NULL
  if (!is.data.frame(df)) {
    df <- tryCatch(
      assemble_work(root, dataset, side, spec, graph, exclude_id = exclude_id),
      error = function(e) NULL
    )
  }
  list(
    rows = if (is.data.frame(df)) nrow(df) else 0L,
    columns = if (is.data.frame(df)) names(df) else character()
  )
}

expert_read_code <- function(project, dataset, id, side) {
  root <- if (is.character(project)) project else project_dir_of(project)
  txt <- expert_read_file(file.path(side_run_dir(root, dataset, id, side), "code.R"))
  if (is.null(txt) || !nzchar(trimws(txt))) NULL else txt
}

expert_ref_tokens <- function(text) {
  blob <- toupper(paste(as.character(text %||% ""), collapse = "\n"))
  if (!nzchar(blob)) return(character())
  m <- gregexpr("\\b([A-Z][A-Z0-9_]*)\\.([A-Z][A-Z0-9_]*)\\b", blob, perl = TRUE)
  hits <- regmatches(blob, m)[[1]]
  if (!length(hits)) character() else unique(hits)
}

expert_source_pairs <- function(texts, dataset) {
  hits <- expert_ref_tokens(texts)
  if (!length(hits)) return(character())
  ds <- toupper(as.character(dataset %||% "")[1])
  dom <- sub("\\..*$", "", hits)
  var <- sub("^[^.]*\\.", "", hits)
  keep <- nzchar(dom) & nzchar(var) & dom != ds & dom != var
  unique(hits[keep])
}

lineage_source_path <- function(project, domain) {
  if (is.null(project)) return("")
  root <- if (is.character(project)) project else project_dir_of(project)
  files <- list.files(file.path(root, "data"), pattern = "\\.[Xx][Pp][Tt]$", full.names = TRUE)
  hit <- files[toupper(tools::file_path_sans_ext(basename(files))) == domain]
  if (length(hit)) hit[[1]] else ""
}

lineage_catalog_line <- function(catalog, ref) {
  dom <- sub("\\..*$", "", ref)
  var <- sub("^[^.]*\\.", "", ref)
  d <- catalog$domains[[dom]] %||% list()
  for (v in d$variables %||% list()) {
    if (toupper(as.character(v$name %||% "")[1]) == var)
      return(paste(ref, field_txt(v$label), field_txt(v$type), sep = " | "))
  }
  ref
}

lineage_dataset_rules <- function(spec, dataset) {
  raw <- spec$dataset_rules %||% spec$rules %||% NULL
  if (is.list(raw) && !is.data.frame(raw)) raw <- raw[[dataset]] %||% NULL
  if (is.data.frame(raw) && "Dataset" %in% names(raw)) raw <- raw[toupper(raw$Dataset) == dataset, , drop = FALSE]
  txt <- trimws(paste(as.character(unlist(raw %||% character(), use.names = FALSE)), collapse = "\n"))
  if (nzchar(txt)) paste0("Dataset rules:\n", txt, "\n") else ""
}

lineage_sample_seed <- function(dataset, column) {
  bytes <- utf8ToInt(paste(toupper(dataset), toupper(column), sep = "."))
  seed <- 0
  for (byte in bytes) seed <- (seed * 131 + byte) %% 2147483647
  as.integer(max(1, seed))
}

lineage_sample_rows <- function(x, dataset, column, n = 20L) {
  available <- which(!is.na(x))
  if (length(available) <= n) return(available)
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(lineage_sample_seed(dataset, column))
  sort(sample(available, n))
}

lineage_column_line <- function(ref, x, dataset, key = FALSE) {
  type <- class(x)[[1L]]
  head <- paste0(ref, " <", type, ">")
  if (isTRUE(key)) return(head)
  total <- length(x)
  missing <- sum(is.na(x))
  if (!total) return(paste0(head, " no values (0 NA of 0)"))
  rows <- lineage_sample_rows(x, dataset, ref)
  if (!length(rows)) return(paste0(head, " all NA (", missing, " NA of ", total, ")"))
  values <- if (is.character(x) || is.factor(x)) as.character(x[rows]) else format(x[rows], trim = TRUE)
  if (is.character(x) || is.factor(x)) values <- encodeString(values, quote = '"')
  else values <- trimws(values)
  values <- gsub("[\r\n]+", " ", values)
  paste0(head, " ", length(rows), " sampled: ", paste(values, collapse = ", "),
         " (", missing, " NA of ", total, ")")
}

# Shared reader for source columns used by lineage and review prompts.
.lineage_source_cache <- new.env(parent = emptyenv())
lineage_source_cache_reset <- function() {
  rm(list = ls(.lineage_source_cache, all.names = TRUE), envir = .lineage_source_cache)
  invisible(NULL)
}
lineage_source_cache <- function(path) {
    key <- normalizePath(path, winslash = "/", mustWork = FALSE)
    info <- file.info(path)
    stamp <- if (nrow(info) && !is.na(info$mtime)) paste(info$size, as.numeric(info$mtime)) else "missing"
    hit <- .lineage_source_cache[[key]]
    if (is.null(hit) || !identical(hit$stamp, stamp)) {
      df <- tryCatch(plain_frame(haven::read_xpt(path)), error = function(e) NULL)
      hit <- list(stamp = stamp, data = if (is.data.frame(df)) df else NULL)
      .lineage_source_cache[[key]] <- hit
    }
    hit$data
}

lineage_source_column <- function(project, domain, column) {
  path <- lineage_source_path(project, domain)
  if (!nzchar(path)) return(NULL)
  df <- lineage_source_cache(path)
  if (!is.data.frame(df)) return(NULL)
  hit <- match(toupper(column), toupper(names(df)))
  if (is.na(hit)) NULL else df[[hit]]
}

lineage_param_layer_identity <- function(pg, paramcd) {
  carry <- unique(c(toupper(flat_chr(pg$carry_members %||% character())), vapply(
    Filter(function(x) identical(x$classification, "carry_over"),
      as_records(pg$classification$parameters %||% pg$parameter_classification$parameters)),
    function(x) toupper(x$paramcd %||% ""), "")))
  if (toupper(paramcd) %in% carry) "CARRY_OVER" else toupper(paramcd)
}

lineage_parents <- function(graph, id, project = NULL) {
  parents <- expert_parents(graph, id)
  parsed <- parse_node_id(id)
  if (!is.null(parsed) && !is.null(parsed$paramcd) && !is.null(project)) {
    pg <- tryCatch(param_graph_read(project, parsed$dataset), error = function(e) NULL)
    for (edge in as_records(pg$edges %||% list())) if (toupper(edge$to) == toupper(parsed$paramcd)) {
      for (use in Filter(function(u) identical(toupper(u$to_var), toupper(parsed$var)), param_edge_uses(edge$uses, parsed$dataset))) {
        parent_layer <- lineage_param_layer_identity(pg, edge$from)
        parent <- paste0(parsed$dataset, ".", toupper(use$from_var), "@", parent_layer)
        if (any(vapply(graph$nodes %||% list(), function(n) identical(n$id, parent), TRUE))) parents <- c(parents, parent)
      }
    }
  }
  unique(parents)
}

lineage_selection <- function(graph, node, project = NULL) {
  node <- node_record(node)
  queue <- node$id; seen <- character()
  while (length(queue)) {
    id <- queue[[1L]]; queue <- queue[-1L]
    if (id %in% seen) next
    seen <- c(seen, id)
    queue <- c(queue, lineage_parents(graph, id, project))
  }
  list(target_id = node$id, upstream_ids = setdiff(seen, node$id), ids = seen)
}

lineage_node_ids <- function(graph, node, project = NULL) {
  lineage_selection(graph, node, project)$ids
}

lineage_review_edges <- function(graph, ids, project = NULL) {
  ids <- unique(as.character(ids))
  edges <- Filter(function(e) e$from %in% ids && e$to %in% ids, graph$edges %||% list())
  if (is.null(project)) return(edges)
  ds <- toupper(graph$dataset %||% "")
  pg <- tryCatch(param_graph_read(project, ds), error=function(e) NULL)
  for (id in ids) {
    parsed <- parse_node_id(id)
    if (is.null(parsed$paramcd)) next
    for (pe in as_records(pg$edges %||% list())) if (toupper(pe$to) == toupper(parsed$paramcd)) {
      for (use in Filter(function(u) identical(toupper(u$to_var), toupper(parsed$var)), param_edge_uses(pe$uses, ds))) {
        parent_layer <- lineage_param_layer_identity(pg, pe$from)
        from <- paste0(ds, ".", toupper(use$from_var), "@", parent_layer)
        if (from %in% ids) edges <- c(edges, list(list(from=from, to=id, evidence=pe$evidence %||% "")))
      }
    }
  }
  unique_edges <- list(); keys <- character()
  for (edge in edges) {
    key <- paste(edge$from, edge$to, sep="\t")
    if (!key %in% keys) { keys <- c(keys,key); unique_edges[[length(unique_edges)+1L]] <- edge }
  }
  unique_edges
}

lineage_context <- function(spec, graph, node, project, side, catalog = NULL) {
  node <- node_record(node)
  ds <- toupper(graph$dataset %||% parse_node_id(node$id)$dataset)
  keys <- spec_key_names(spec, ds)
  work_file <- if (is.null(project)) "" else work_path(if (is.character(project)) project else project_dir_of(project), ds, side)
  work <- if (nzchar(work_file) && file.exists(work_file)) tryCatch(readRDS(work_file), error = function(e) NULL) else NULL
  work_names <- if (is.data.frame(work)) toupper(names(work)) else character()
  selection <- lineage_selection(graph, node, project)
  queue <- selection$ids
  seen <- character()
  spec_lines <- character()
  work_lines <- character()
  card_lines <- character()
  input_roles <- list()
  source_refs <- character()
  ancestor_source_refs <- character()
  ancestor_work_source_refs <- character()
  while (length(queue)) {
    id <- queue[[1]]
    queue <- queue[-1]
    if (id %in% seen) next
    seen <- c(seen, id)
    parsed <- parse_node_id(id)
    if (is.null(parsed)) next
    carded <- FALSE
    if (!identical(id,node$id) && !is.null(project)) {
      block <- card_prompt_block(project, ds, id, side)
      if (nzchar(block)) {
        carded <- TRUE
        card_lines <- c(card_lines,block)
        input_roles[[id]] <- "card"
      }
    }
    if (!carded) {
      shown_work <- id != node$id && parsed$var %in% work_names
      if (shown_work) {
        v <- spec_vars(spec, ds)
        hit <- which(toupper(v$Variable) == parsed$var)
        label <- if (length(hit)) field_txt(v$Label[[hit[[1]]]]) else ""
        typ <- if (length(hit)) field_txt(v$Type[[hit[[1]]]]) else ""
        work_lines <- c(work_lines, paste(id, label, typ, sep = " | "))
        input_roles[[id]] <- "work"
        actual <- names(work)[match(parsed$var, work_names)]
        column <- work[[actual]]
        work_lines <- c(work_lines, lineage_column_line(id, column, ds, parsed$var %in% toupper(keys)))
        if (!(parsed$var %in% toupper(keys))) {
          line <- levels_line(id, distinct_levels(column))
          if (nzchar(line)) work_lines <- c(work_lines, line)
        }
      } else {
        row <- spec_rows_text(spec, ds, parsed$var)
        if (!identical(id, node$id) && nzchar(row)) {
          spec_lines <- c(spec_lines, paste0(id, "\n", row))
          input_roles[[id]] <- "spec"
        }
      }
      row <- spec_rows_text(spec, ds, parsed$var)
      refs <- expert_source_pairs(row, ds)
      if (!shown_work || identical(id,node$id)) source_refs <- c(source_refs, refs)
      if (!identical(id,node$id)) ancestor_source_refs <- c(ancestor_source_refs,refs)
      if (shown_work) ancestor_work_source_refs <- c(ancestor_work_source_refs,refs)
      for (e in graph$external %||% list()) if (id %in% flat_chr(e$used_by)) {
        ref <- toupper(as.character(e$ref %||% "")[1])
        if (!shown_work || identical(id,node$id)) source_refs <- c(source_refs,ref)
        if (!identical(id,node$id)) ancestor_source_refs <- c(ancestor_source_refs,ref)
        if (shown_work) ancestor_work_source_refs <- c(ancestor_work_source_refs,ref)
      }
    }
    queue <- c(queue, lineage_parents(graph, id, project))
  }
  # Cards limit prompt expansion, not primary's access to its full lineage.
  prompt_source_refs <- unique(source_refs[grepl("^[A-Z][A-Z0-9_]*\\.[A-Z][A-Z0-9_]*$", source_refs)])
  if(length(card_lines)) prompt_source_refs <- unique(c(prompt_source_refs,ancestor_work_source_refs))
  scope_seen <- selection$ids
  scope_source_refs <- character()
  for (id in scope_seen) {
    parsed <- parse_node_id(id)
    if (is.null(parsed)) next
    row <- spec_rows_text(spec,ds,parsed$var)
    scope_source_refs <- c(scope_source_refs,expert_source_pairs(row,ds))
    for (e in graph$external %||% list()) {
      if (id %in% flat_chr(e$used_by)) scope_source_refs <- c(scope_source_refs,toupper(as.character(e$ref %||% "")[1]))
    }
  }
  scope_source_refs <- unique(scope_source_refs[grepl("^[A-Z][A-Z0-9_]*\\.[A-Z][A-Z0-9_]*$",scope_source_refs)])
  if (identical(side,"validation")) {
    v <- spec_vars(spec,ds)
    target_row <- which(toupper(v$Variable)==toupper(node$var))
    target_source <- if (length(target_row)) field_txt(v$Source[[target_row[[1]]]]) else ""
    prompt_source_refs <- unique(expert_source_pairs(target_source,ds))
    if (length(card_lines)) prompt_source_refs <- unique(c(prompt_source_refs,ancestor_source_refs))
  }
  source_refs <- prompt_source_refs
  source_lines <- character()
  supplied_sources <- character()
  for (ref in source_refs) {
    catalog_line <- lineage_catalog_line(catalog, ref)
    source_lines <- c(source_lines, catalog_line)
    if (nzchar(catalog_line)) supplied_sources <- c(supplied_sources, ref)
    dom <- sub("\\..*$", "", ref)
    var <- sub("^[^.]*\\.", "", ref)
    column <- lineage_source_column(project, dom, var)
    if (!is.null(column)) {
      supplied_sources <- c(supplied_sources, ref)
      source_lines <- c(source_lines, lineage_column_line(ref, column, ds, var %in% toupper(keys)))
      if (!(var %in% toupper(keys))) {
        line <- levels_line(ref, distinct_levels(column))
        if (nzchar(line)) source_lines <- c(source_lines, line)
      }
    }
  }
  if (identical(side,"validation")) {
    excluded_done <- node$id
    queue <- node$id
    seen_desc <- character()
    while (length(queue)) {
      current <- queue[[1L]]; queue <- queue[-1L]
      if (current %in% seen_desc) next
      seen_desc <- c(seen_desc,current)
      children <- vapply(Filter(function(edge) identical(edge$from,current),graph$edges %||% list()),
                         function(edge) edge$to, "")
      excluded_done <- unique(c(excluded_done,children))
      queue <- c(queue,children)
    }
    done <- character()
    if (is.data.frame(work)) for (var in names(work)) {
      vu <- toupper(var)
      candidates <- Filter(function(n) identical(toupper(n$var %||% ""),vu) && n$kind!="records",graph$nodes %||% list())
      if (length(candidates)) {
        same_param <- Filter(function(n) identical(n$paramcd,node$paramcd),candidates)
        chosen <- if (length(same_param)) same_param[[1]] else candidates[[1]]
        id <- chosen$id
      } else id <- paste0(ds,".",vu)
      hit <- which(toupper(v$Variable)==vu)
      label <- if (length(hit)) field_txt(v$Label[[hit[[1]]]]) else ""
      typ <- if (length(hit)) field_txt(v$Type[[hit[[1]]]]) else ""
      if (!id %in% excluded_done) done <- c(done,paste(id,label,typ,sep=" | "))
    }
    domain_map <- list()
    if (is.list(catalog$domains)) {
      for (dom in names(catalog$domains)) {
        domain_map[[toupper(dom)]] <- unique(toupper(vapply(catalog$domains[[dom]]$variables %||% list(),function(x)field_txt(x$name),"")))
      }
    } else if (!is.null(project) && exists("data_index",mode="function")) {
      root <- if (is.character(project)) project else project_dir_of(project)
      domain_map <- data_index(root)
    }
    domain_lines <- vapply(names(domain_map),function(dom)paste0(toupper(dom),": ",paste(unique(domain_map[[dom]][nzchar(domain_map[[dom]])]),collapse=", ")),"")
    domains_block <- paste(c("SOURCE_DOMAINS (names only):",domain_lines),collapse="\n")
    if (nchar(domains_block,type="chars")>4000L) domains_block <- substr(domains_block,1L,4000L)
    target_text <- if (nzchar(spec_rows_text(spec,ds,node$var))) paste0(node$id,"\n",spec_rows_text(spec,ds,node$var)) else ""
    lines <- unique(c(target_text,done,card_lines,source_lines,domains_block))
    context_text <- paste0("LINEAGE:\n",paste(lines[nzchar(lines)],collapse="\n"),"\n",
                           "WORK_KEYS: ",paste(keys,collapse=","),"\n",lineage_dataset_rules(spec,ds))
  } else {
    context_text <- paste0("LINEAGE:\n", paste(unique(c(spec_lines, work_lines, card_lines, source_lines)), collapse = "\n"), "\n",
                           "WORK_KEYS: ", paste(keys, collapse = ","), "\n",
                           lineage_dataset_rules(spec, ds))
  }
  roles <- setNames(vapply(selection$ids, function(id) {
    if (identical(id, selection$target_id)) "target" else input_roles[[id]] %||% "dependency"
  }, ""), selection$ids)
  list(target_id = selection$target_id, upstream_ids = selection$upstream_ids,
       ids = selection$ids, roles = roles,
       source_context = list(requested = unique(prompt_source_refs), supplied = unique(supplied_sources)),
       source_refs = scope_source_refs,
       prompt_ids = seen, prompt_source_refs = prompt_source_refs,
       input_roles = input_roles,
       supplied_sources = unique(supplied_sources),
       text = context_text)
}

expert_scope_ids <- function(spec, graph, node) {
  node <- node_record(node)
  ds <- toupper(graph$dataset %||% parse_node_id(node$id)$dataset %||% "")
  keys <- if (is.null(spec)) character() else spec_key_names(spec, ds)
  anc <- expert_ancestor_ids(graph, node$id)
  key_ids <- if (length(keys)) paste0(ds, ".", toupper(keys)) else character()
  list(
    dataset = ds,
    target = node$id,
    ancestors = anc,
    direct = expert_parents(graph, node$id),
    keys = key_ids,
    ids = unique(c(node$id, anc, key_ids))
  )
}

expert_relevant_spec_text <- function(spec, graph, node) {
  if (is.null(spec)) return("")
  scope <- expert_scope_ids(spec, graph, node)
  ds <- scope$dataset
  if (!nzchar(ds)) return("")
  vars <- unique(toupper(vapply(scope$ids, function(id) {
    p <- parse_node_id(id)
    if (is.null(p)) "" else as.character(p$var %||% "")[1]
  }, "")))
  vars <- vars[nzchar(vars)]
  pcs <- unique(unlist(lapply(scope$ids, function(id) {
    p <- parse_node_id(id)
    if (is.null(p) || is.null(p$paramcd)) character() else toupper(p$paramcd)
  }), use.names = FALSE))
  v <- spec_vars(spec, ds)
  lines <- c("RELEVANT_SPEC", "Variable\tLabel\tType\tLength\tKey\tSource\tDerivation")
  n <- 0L
  if (nrow(v) && length(vars)) {
    for (i in seq_len(nrow(v))) {
      name <- toupper(field_txt(v$Variable[[i]]))
      if (!nzchar(name) || !(name %in% vars)) next
      n <- n + 1L
      lines <- c(lines, paste(
        field_txt(v$Variable[[i]]), field_txt(v$Label[[i]]), field_txt(v$Type[[i]]),
        field_txt(v$Length[[i]]), field_txt(v$Key[[i]]),
        field_txt(v$Source[[i]]), field_txt(v$Derivation[[i]]),
        sep = "\t"
      ))
    }
  }
  params <- spec_params(spec, ds)
  if (nrow(params) && length(pcs)) {
    lines <- c(lines, "PARAMETERS:", "PARAMCD\tPARAM\tSource\tDerivation")
    for (i in seq_len(nrow(params))) {
      pc <- toupper(field_txt(params$PARAMCD[[i]]))
      if (!nzchar(pc) || !(pc %in% pcs)) next
      n <- n + 1L
      lines <- c(lines, paste(
        field_txt(params$PARAMCD[[i]]), field_txt(params$PARAM[[i]]),
        field_txt(params$Source[[i]]), field_txt(params$Derivation[[i]]),
        sep = "\t"
      ))
    }
  }
  if (!n) return("")
  paste(lines, collapse = "\n")
}

expert_index_text <- function(spec, project, dataset) {
  v <- spec_vars(spec, dataset)
  if (!nrow(v)) return("")
  ds <- toupper(as.character(dataset %||% "")[1])
  lines <- character()
  for (i in seq_len(nrow(v))) {
    name <- field_txt(v$Variable[[i]])
    if (!nzchar(name)) next
    lines <- c(lines, paste(name, field_txt(v$Label[[i]]), expert_level(project, ds, paste0(ds, ".", toupper(name))), sep = " | "))
  }
  paste(lines, collapse = "\n")
}

expert_oneline <- function(id, level, code) {
  parsed <- parse_node_id(id)
  var <- if (is.null(parsed)) id else (parsed$var %||% id)
  lvl <- if (!nzchar(level %||% "")) "none" else level
  detail <- ""
  if (nzchar(code %||% "")) {
    for (ln in strsplit(code, "\n", fixed = TRUE)[[1]]) {
      t <- trimws(ln)
      if (startsWith(t, "#")) { detail <- t; break }
    }
    if (!nzchar(detail)) {
      refs <- expert_source_pairs(code, if (is.null(parsed)) "" else parsed$dataset)
      if (length(refs)) detail <- paste(refs, collapse = ", ")
    }
  }
  if (!nzchar(detail)) detail <- "no saved code"
  paste0(var, " (", lvl, "): ", detail)
}

expert_program_text <- function(side, spec, graph, node, project, code_cap = 40000L) {
  if (is.null(project)) return("")
  scope <- expert_scope_ids(spec, graph, node)
  show <- unique(c(scope$ancestors, scope$keys))
  show <- show[nzchar(show) & show != scope$target]
  show <- expert_chain_order(graph, show)
  if (!length(show)) return("")
  texts <- character()
  full <- logical()
  for (nid in show) {
    code <- expert_read_code(project, scope$dataset, nid, side)
    lvl <- expert_level(project, scope$dataset, nid)
    is_full <- nid %in% scope$direct || nid %in% scope$keys
    if (is_full && nzchar(code %||% "")) {
      texts <- c(texts, paste0("# ==== ", nid, " (", lvl, ") ====\n", code))
      full <- c(full, TRUE)
    } else {
      texts <- c(texts, expert_oneline(nid, lvl, code))
      full <- c(full, FALSE)
    }
  }
  keep <- rep(TRUE, length(texts))
  cap <- as.integer(code_cap %||% 40000L)[1]
  if (is.na(cap) || cap < 1L) cap <- 40000L
  repeat {
    blob <- paste(texts[keep], collapse = "\n")
    if (nchar(blob) <= cap) break
    drop <- which(keep & !full)
    if (!length(drop)) break
    keep[drop[1]] <- FALSE
  }
  blob <- paste(texts[keep], collapse = "\n")
  if (nchar(blob) > cap) blob <- substr(blob, 1L, cap)
  blob
}

expert_chain_status <- function(project, dataset, side, ids) {
  if (is.null(project) || !length(ids)) return("")
  lines <- character()
  for (id in ids) {
    slot <- expert_side_slot(project, dataset, id, side)
    ok <- slot$ok
    ok_txt <- if (is.null(ok)) "" else if (isTRUE(ok)) "true" else if (identical(ok, FALSE)) "false" else ""
    err <- gsub("[\r\n]+", " ", expert_side_error(project, dataset, id, side))
    lines <- c(lines, paste0(id, " level=", expert_level(project, dataset, id), " ok=", ok_txt, " error=", err))
  }
  paste(c("SIDE_STATUS_BEGIN", lines, "SIDE_STATUS_END"), collapse = "\n")
}

expert_context_texts <- function(spec, graph, node, project, side) {
  scope <- expert_scope_ids(spec, graph, node)
  bits <- c(spec_rows_text(spec, scope$dataset, node_record(node)$var), expert_relevant_spec_text(spec, graph, node))
  if (!is.null(project)) {
    for (nid in scope$ids) {
      code <- expert_read_code(project, scope$dataset, nid, side)
      if (!is.null(code)) bits <- c(bits, code)
    }
  }
  bits
}

expert_catalog_subset <- function(catalog, pairs) {
  if (is.null(catalog) || !length(pairs) || is.null(catalog$domains)) return("")
  lines <- character()
  for (ref in unique(pairs)) {
    dom <- sub("\\..*$", "", ref)
    var <- sub("^[^.]*\\.", "", ref)
    d <- catalog$domains[[dom]]
    if (is.null(d)) {
      lines <- c(lines, ref)
      next
    }
    lab <- ""
    for (v in d$variables %||% list()) {
      if (identical(toupper(as.character(v$name %||% "")[1]), var)) {
        lab <- as.character(v$label %||% "")[1]
        break
      }
    }
    bit <- if (nzchar(lab) && !is.na(lab)) paste0(var, " [", lab, "]") else var
    nrow <- d$nrow %||% 0L
    lines <- c(lines, paste0(dom, " (", nrow, " rows): ", bit))
    vals <- NULL
    for (nm in names(d$values %||% list())) {
      if (identical(toupper(nm), var)) vals <- d$values[[nm]]
    }
    if (!is.null(vals)) {
      vv <- as.character(unlist(vals, use.names = FALSE))
      vv <- vv[!is.na(vv) & nzchar(vv)]
      if (length(vv)) lines <- c(lines, paste0("  ", var, ": ", paste(vv, collapse = ", ")))
    }
  }
  paste(unique(lines), collapse = "\n")
}

expert_source_catalog <- function(catalog, spec, graph, node, project, side) {
  if (is.null(catalog) || !exists("catalog_prompt_text", mode = "function")) return("")
  scope <- expert_scope_ids(spec, graph, node)
  pairs <- expert_source_pairs(expert_context_texts(spec, graph, node, project, side), scope$dataset)
  if (!length(pairs)) return(catalog_prompt_text(catalog, labels = TRUE))
  expert_catalog_subset(catalog, pairs)
}

expert_picture <- function(side, spec, graph, node, project, code_cap = 40000L) {
  if (is.null(spec) || is.null(graph)) return("")
  side <- match.arg(side, c("primary", "validation"))
  scope <- expert_scope_ids(spec, graph, node)
  ds <- scope$dataset
  bits <- character()
  idx <- expert_index_text(spec, project, ds)
  if (nzchar(idx)) {
    bits <- c(bits, paste0(
      paste0("The variable index lists every variable of this dataset (name, label, level) for orientation only. It is not the specification of the target. The actual inherited dataset is ",side_program_name(ds,node,side),"; primary should build from lineage variables first, but may use a needed column present in the LINEAGE_ frame even when outside DAG lineage; the app warns. Do not print the app's warning. Validation may use any built column on its own side.\n"),
      "VAR_INDEX_BEGIN\n", idx, "\nVAR_INDEX_END\n"
    ))
  }
  rel <- expert_relevant_spec_text(spec, graph, node)
  if (nzchar(rel)) bits <- c(bits, paste0(rel, "\n"))
  status <- expert_chain_status(project, ds, side, scope$ids)
  if (nzchar(status)) bits <- c(bits, paste0(status, "\n"))
  prog <- expert_program_text(side, spec, graph, node, project, code_cap = code_cap)
  if (nzchar(prog)) bits <- c(bits, paste0("PROGRAM_BEGIN\n", prog, "\nPROGRAM_END\n"))
  th <- expert_thread_text(project, ds, side, node)
  if (nzchar(th)) bits <- c(bits, th)
  paste(bits, collapse = "")
}

expert_notes <- function(project, dataset, id, side) {
  if (is.null(project)) return(list())
  root <- if (is.character(project)) project else project_dir_of(project)
  if (exists("read_side_notes", mode = "function")) {
    return(tryCatch(read_side_notes(root, dataset, id, side), error = function(e) list()))
  }
  path <- file.path(side_run_dir(root, dataset, id, side), "notes.json")
  raw <- if (exists("read_json", mode = "function")) tryCatch(read_json(path), error = function(e) NULL) else NULL
  if (is.null(raw)) return(list())
  if (is.list(raw) && !is.null(raw$text)) return(list(raw))
  Filter(function(n) is.list(n) && !is.null(n$text), if (is.list(raw)) raw else list())
}

ask_tail_text <- function(text, n = 200L, max_chars = 12000L) {
  text <- paste(as.character(text %||% ""), collapse = "\n")
  if (!nzchar(text)) return("")
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  if (length(lines) > n) lines <- lines[(length(lines) - n + 1L):length(lines)]
  out <- paste(lines, collapse = "\n")
  if (nchar(out) > max_chars) out <- substr(out, nchar(out) - max_chars + 1L, nchar(out))
  out
}

ask_is_general <- function(question, var) {
  q <- trimws(as.character(question %||% "")[1])
  if (!nzchar(q)) return(TRUE)
  if (nzchar(var %||% "") && grepl(tolower(var), tolower(q), fixed = TRUE)) return(FALSE)
  if (grepl("this code|this log|this variable|this side|this program|the log|the code|explain|\\berror\\b|\\bfailed\\b",
            q, ignore.case = TRUE, perl = TRUE)) return(FALSE)
  TRUE
}

ask_system_prompt <- function(side, dataset = NULL, packages = NULL, node = NULL) {
  side <- match.arg(side, c("primary", "validation"))
  temp <- side_program_name(dataset,node,side)
  base <- paste(
    side_role_line(side, dataset),
    "TASK: ask",
    "You explain this side's R program to a clinical programmer who may not know R.",
    "You see only this side. Do not guess the other side's code, log, notes, or results.",
    if (identical(side,"primary")) "Build from lineage variables first. A needed column present in names(LINEAGE_<P>) but outside DAG lineage may be used; the app warns, and this is not an error or stop. Do not print the app's warning. Use Missing column: X is not in LINEAGE_SYSBP or the provided data only when the needed column is absent from the frame and provided source data. Output goes only to your own LINEAGE_ dataset." else "",
    "Answer briefly: what the code does, what went wrong (quote the error line), the likely cause, and one concrete note that would fix it.",
    paste0("The generated program is one step in a same-side chain. Earlier steps built ",temp,"; that inherited dataset is the actual frame and may contain more columns than the ancestor context. A non-keys step adds only its target to ",temp,"."),
    "Return JSON only: {\"answer\":\"markdown-light text\",\"suggestion\":\"one instruction for the regenerate box, or empty\"}.",
    "No prose outside the JSON.",
    r_packages_rule(packages),
    sep = "\n"
  )
  extra <- if (exists("dag_lookup_rule", mode = "function")) dag_lookup_rule() else ""
  if (nzchar(extra)) paste(base, extra, sep = "\n") else base
}

ask_user_prompt <- function(side, spec, graph, node, question, project = NULL, catalog = NULL,
                            current_code = NULL, notes = NULL, log = NULL) {
  side <- match.arg(side, c("primary", "validation"))
  node <- node_record(node)
  graph <- if (is.null(graph)) empty_graph(parse_node_id(node$id)$dataset %||% "") else normalize_graph(graph)
  ds <- graph$dataset %||% parse_node_id(node$id)$dataset
  keys <- if (is.null(spec)) character() else spec_key_names(spec, ds)
  if (isTRUE(graph$layered) && !is.null(node$paramcd)) keys <- layer_key_names(spec,ds,node$paramcd,graph,project)
  q <- trimws(as.character(question %||% "")[1])
  if (nchar(q) > 4000L) q <- substr(q, 1L, 4000L)
  pkg_block <- r_packages_prompt(side_r_packages(project, side))
  if (ask_is_general(q, node$var %||% "")) {
    frame <- if (is.null(project)) NULL else tryCatch(side_prompt_frame(project,ds,side,spec,graph,node),error=function(e)NULL)
    facts <- list(rows=if(is.data.frame(frame))nrow(frame)else 0L,columns=if(is.data.frame(frame))names(frame)else character())
    available_block <- side_available_datasets_for_prompt(project,ds,side,spec,graph,node,current_frame=frame)
    idx <- expert_index_text(spec, project, ds)
    return(paste0(
      "SIDE: ", side, "\n",
      "DATASET: ", ds, "\n",
      pkg_block,
      "NODE: ", node$id, "\n",
      if (identical(node$kind,"records")) paste0("VAR: KEYS\nKEYS: ",paste(keys,collapse=","),"\n") else "",
      "This question is general. Use the variable index and the thread. Do not assume a single variable's code.\n",
      paste0("The variable index is for orientation only. The actual side-local frame is ",side_program_name(ds,node,side),".\n"),
      if(identical(side,"primary"))"LINEAGE_FRAME_ROWS: "else"TEMP_FRAME_ROWS: ", facts$rows, "\n",
      if(identical(side,"primary"))"LINEAGE_FRAME_COLUMNS: "else"TEMP_FRAME_COLUMNS: ", paste(facts$columns, collapse = ","), "\n",
      if(nzchar(available_block))paste0(available_block,"\n")else "",
      if (nzchar(idx)) paste0("VAR_INDEX_BEGIN\n", idx, "\nVAR_INDEX_END\n") else "",
      expert_thread_text(project, ds, side, node),
      "QUESTION_BEGIN\n", q, "\nQUESTION_END\n"
    ))
  }
  if (is.null(current_code) && !is.null(project)) current_code <- expert_read_code(project, ds, node$id, side)
  if (is.null(notes) && !is.null(project)) notes <- expert_notes(project, ds, node$id, side)
  if (is.null(log) && !is.null(project)) {
    log <- expert_read_file(file.path(side_run_dir(
      if (is.character(project)) project else project_dir_of(project), ds, node$id, side
    ), "log.txt")) %||% ""
  }
  thread <- expert_thread_text(project,ds,side,node)
  code_txt <- paste(as.character(current_code %||% ""), collapse = "\n")
  log_txt <- ask_tail_text(log %||% "")
  notes_block <- ""
  if (is.list(notes) && length(notes)) {
    lines <- character()
    for (i in seq_along(notes)) {
      tx <- trimws(gsub("[\r\n]+", " ", as.character(notes[[i]]$text %||% "")[1]))
      if (nzchar(tx)) lines <- c(lines, paste0(i, ". ", tx))
    }
    if (length(lines)) notes_block <- paste0("NOTES_BEGIN\n", paste(lines, collapse = "\n"), "\nNOTES_END\n")
  }
  frame <- if (is.null(project)) NULL else tryCatch(side_prompt_frame(project,ds,side,spec,graph,node),error=function(e)NULL)
  facts <- list(rows=if(is.data.frame(frame))nrow(frame)else 0L,columns=if(is.data.frame(frame))names(frame)else character())
  lineage <- lineage_context(spec, graph, node, project, side, catalog)
  if (identical(side,"primary")) lineage$text <- gsub("WORK_KEYS:","LINEAGE_KEYS:",lineage$text,fixed=TRUE)
  available_block <- side_available_datasets_for_prompt(project,ds,side,spec,graph,node,current_frame=frame,lineage=lineage,keys=keys)
  err <- expert_side_error(project, ds, node$id, side)
  slot <- expert_side_slot(project, ds, node$id, side)
  ok_txt <- if (is.null(slot$ok)) "" else if (isTRUE(slot$ok)) "true" else if (identical(slot$ok, FALSE)) "false" else ""
  prob <- ""
  if (!is.null(project)) {
    root <- if (is.character(project)) project else project_dir_of(project)
    st <- tryCatch(state_nodes(root, ds)[[node$id]], error = function(e) NULL)
    if (is.list(st) && is.list(st$problem)) {
      msg <- as.character(st$problem$message %||% "")[1]
      stage <- as.character(st$problem$stage %||% "")[1]
      if (nzchar(msg) && (identical(msg, err) || (identical(stage, "validate") && !nzchar(err)))) prob <- msg
    }
  }
  paste0(
    "SIDE: ", side, "\n",
    "DATASET: ", ds, "\n",
    "NODE: ", node$id, "\n",
    "VAR: ", if (identical(node$kind,"records")) "KEYS" else node$var, "\n",
    "PARAMCD: ", node$paramcd %||% "-", "\n",
    "KEYS: ", paste(keys, collapse = ","), "\n",
    pkg_block,
    spec_rows_text(spec, ds, node$var), "\n",
    "LEVEL: ", expert_level(project, ds, node$id), "\n",
    "SIDE_OK: ", ok_txt, "\n",
    "SIDE_ERROR: ", gsub("[\r\n]+", " ", err), "\n",
    if (nzchar(prob)) paste0("PROBLEM: ", gsub("[\r\n]+", " ", prob), "\n") else "",
    lineage$text,
    if(identical(side,"primary"))"LINEAGE_DATASET: "else"TEMP_DATASET: ", side_program_name(ds,node,side), "\n",
    if(identical(side,"primary"))"LINEAGE_FRAME_ROWS: "else"TEMP_FRAME_ROWS: ", facts$rows, "\n",
    if(identical(side,"primary"))"LINEAGE_FRAME_COLUMNS: "else"TEMP_FRAME_COLUMNS: ", paste(facts$columns,collapse=","), "\n",
    if(nzchar(available_block))paste0(available_block,"\n")else "",
    paste0("This program is one step in a chain. Earlier steps on this side built ",side_program_name(ds,node,side),". Ancestor information above is background; this is the actual inherited dataset.\n"),
    if(identical(side,"primary"))paste0("Build from lineage variables first. A needed column present in names(",side_program_name(ds,node,side),") but outside DAG lineage is allowed and produces an amber app warning, not an error or stop. Do not print the app's warning in code or reply. Only stop for a needed column absent from the frame and provided source data, using the exact Missing column wording. Output goes only to your own LINEAGE_ dataset ",side_program_name(ds,node,side),".\n")else "",
    thread,
    notes_block,
    "CODE_BEGIN\n", code_txt, "\nCODE_END\n",
    "LOG_BEGIN\n", log_txt, "\nLOG_END\n",
    "QUESTION_BEGIN\n", q, "\nQUESTION_END\n"
  )
}

ask_side <- function(cfg, project, dataset, node, side, question, catalog = NULL) {
  side <- match.arg(as.character(side %||% "")[1], c("primary", "validation"))
  dataset <- toupper(trimws(as.character(dataset %||% "")[1]))
  node_id <- node_record(node)$id
  question <- trimws(as.character(question %||% "")[1])
  if (!nzchar(question)) stop("question is required.", call. = FALSE)
  root <- if (is.character(project)) project else project_dir_of(project)
  spec <- if (exists("read_project_spec_dir", mode = "function")) {
    tryCatch(read_project_spec_dir(root), error = function(e) NULL)
  } else load_project_spec(root)
  graph <- NULL
  if (exists("load_graph", mode = "function")) {
    graph <- tryCatch(load_graph(root, dataset), error = function(e) NULL)
  }
  if (is.null(graph)) graph <- load_project_graph(root, dataset)
  if (is.null(graph)) graph <- empty_graph(dataset)
  node <- graph_node(graph, node_id) %||% default_node(node_id)
  system <- ask_system_prompt(side, dataset, packages = side_r_packages(root, side), node=node)
  user <- ask_user_prompt(side, spec, graph, node, question, project = root, catalog = catalog)
  if (exists("llm_reply_with_lookup", mode = "function")) {
    answered <- llm_reply_with_lookup(cfg, system, user, spec, graph, dataset, max_tokens = 1200L)
    raw <- answered$raw
    parsed <- answered$parsed
  } else {
    raw <- llm_chat(cfg, system, user, max_tokens = 1200L, json = TRUE)
    parsed <- parse_json_reply(raw$text)
  }
  answer <- trimws(as.character(parsed$answer %||% parsed$text %||% "")[1])
  if (!nzchar(answer)) stop("The model returned an empty answer.", call. = FALSE)
  suggestion <- trimws(gsub("[\r\n]+", " ", as.character(parsed$suggestion %||% "")[1]))
  if (nchar(suggestion) > 500L) suggestion <- substr(suggestion, 1L, 500L)
  expert_append_turn(root, dataset, node, side, list(
    kind = "ask", question = question, answer = answer, suggestion = suggestion
  ))
  list(answer = answer, suggestion = suggestion, model = raw$model %||% "")
}

fake_ask_reply <- function(system, user) {
  block <- if (exists("text_block", mode = "function")) text_block(user %||% "", "LOG_BEGIN", "LOG_END") else ""
  lines <- strsplit(block %||% "", "\n", fixed = TRUE)[[1]]
  hits <- lines[grepl("Error", lines, fixed = TRUE)]
  last <- if (length(hits)) trimws(hits[[length(hits)]]) else ""
  if (nzchar(last)) {
    shown <- if (grepl("`", last, fixed = TRUE)) paste0("\n```\n", last, "\n```") else paste0("`", last, "`")
    answer <- paste0("The log failed at ", shown, ".")
    suggestion <- paste0("Fix this error: ", last)
  } else {
    answer <- "The log has no Error line."
    suggestion <- ""
  }
  json_text(list(answer = answer, suggestion = suggestion))
}

agent_chat_format_instruction <- function() paste(
  "CHAT_REPLY_FORMAT_BEGIN",
  "Answer the user's message briefly and write no code in the reply. For a question, set change to null. For a clear code change request, ask for the change through change.instruction (one sentence) and set description to 2-3 sentences about meaning, type/units, source/rule, and missingness. For an ambiguous request, ask a clarifying question and set change to null.",
  'Return JSON only: {"reply":"text","change":null} or {"reply":"text","change":{"instruction":"one sentence","description":"2-3 sentences about meaning, type/units, source/rule, and missingness"}}.',
  "No prose outside the JSON.", "CHAT_REPLY_FORMAT_END", sep = "\n")

agent_system_prompt <- function(side, spec, dataset, packages = NULL, node = NULL, project = NULL, graph = NULL) {
  system <- if (!is.null(graph)) side_generation_system_prompt(side, spec, graph, node, project, chat = TRUE) else
    side_system_prompt(side, spec, dataset, packages = packages, node = node, project = project, chat = TRUE)
  paste(system, agent_chat_format_instruction(), sep = "\n")
}

agent_upstream_vars <- function(graph, node) {
  vapply(upstream_ids(graph, node$id), function(u) {
    hit <- split_node_id(u)
    if (is.null(hit)) u else hit$var
  }, "")
}

agent_user_prompt <- function(side, spec, graph, node, message, project, catalog = NULL,
                              current_code = NULL, notes = NULL, upstream_vars = NULL) {
  node <- node_record(node)
  ds <- graph$dataset %||% parse_node_id(node$id)$dataset
  if (is.null(current_code)) current_code <- expert_read_code(project, ds, node$id, side)
  if (is.null(notes) && exists("read_side_notes", mode = "function"))
    notes <- tryCatch(read_side_notes(project, ds, node$id, side), error = function(e) list())
  if (is.null(upstream_vars)) upstream_vars <- agent_upstream_vars(graph, node)
  context <- side_user_prompt(side, spec, graph, node, upstream_vars, catalog = catalog,
    project = project, current_code = current_code, notes = notes, chat = TRUE)
  paste0(context, "MESSAGE_BEGIN\n", message, "\nMESSAGE_END\n")
}

agent_decide <- function(cfg, project, dataset, node, side, message, catalog = NULL) {
  side <- match.arg(side, c("primary", "validation"))
  root <- if (is.character(project)) project else project_dir_of(project)
  spec <- read_project_spec_dir(root)
  graph <- load_graph(root, dataset)
  if (is.null(graph)) stop("No dependency graph yet.", call. = FALSE)
  node_id <- node_record(node)$id
  node <- graph_node(graph, node_id) %||% default_node(node_id)
  system <- agent_system_prompt(side, spec, dataset, side_r_packages(root, side), node=node,
                                project = root, graph = graph)
  user <- agent_user_prompt(side, spec, graph, node, message, root, catalog)
  if (exists("coder_window_event", mode = "function")) coder_window_event(root, dataset, node$id, side, "Chat started")
  on.exit(if (exists("coder_window_event", mode = "function")) coder_window_event(root, dataset, node$id, side, "Chat finished"), add = TRUE)
  call_index <- 0L
  call_usages <- list()
  on_call <- function(call_system, call_user, phase) {
    if (identical(phase, "started")) {
      call_index <<- call_index + 1L
      task <- if (call_index > 1L) "lookup round" else "chat"
      if (exists("coder_window_prompt", mode = "function"))
        coder_window_prompt(root, dataset, node$id, side, call_system, call_user, task,
                            parent_task = if (call_index > 1L) "chat" else NULL)
      if (exists("coder_window_event", mode = "function")) coder_window_event(root, dataset, node$id, side, "Model call started", task)
    } else if (exists("coder_window_event", mode = "function")) coder_window_event(root, dataset, node$id, side, "Model call finished",
                              if (call_index > 1L) "lookup round" else "chat")
  }
  if (exists("llm_reply_with_lookup", mode = "function")) {
    scope <- expert_scope_ids(spec, graph, node)
    answered <- llm_reply_with_lookup(cfg, system, user, spec, graph, dataset, max_tokens = 1200L,
      allowed_ids = if (identical(side, "primary")) scope$ids else NULL, on_call = on_call)
    raw <- answered$raw
    parsed <- answered$parsed
    call_usages <- answered$call_usages
    usage <- answered$usage
  } else {
    if (exists("coder_window_prompt", mode = "function")) coder_window_prompt(root, dataset, node$id, side, system, user, "chat")
    if (exists("coder_window_event", mode = "function")) coder_window_event(root, dataset, node$id, side, "Model call started", "chat")
    on.exit(if (exists("coder_window_event", mode = "function")) coder_window_event(root, dataset, node$id, side, "Model call finished", "chat"), add = TRUE)
    raw <- llm_chat(cfg, system, user, max_tokens = 1200L, json = TRUE)
    call_usages <- list(llm_usage_summary(raw$usage, cfg$provider %||% ""))
    usage <- llm_usage_combine(call_usages)
    parsed <- parse_json_reply(raw$text)
  }
  if (is.null(parsed$reply)) {
    reminder_user <- paste0(user, "\nCHAT_RETRY_REMINDER\nReturn the requested chat JSON with a non-empty reply field. Put no code in reply; request code changes only through change.instruction.\n")
    if (exists("coder_window_prompt", mode = "function")) coder_window_prompt(root, dataset, node$id, side, system, reminder_user, "chat", parent_task = "chat")
    if (exists("coder_window_event", mode = "function")) coder_window_event(root, dataset, node$id, side, "Chat retry started")
    raw <- llm_chat(cfg, system, reminder_user, max_tokens = 1200L, json = TRUE)
    call_usages[length(call_usages) + 1L] <- list(llm_usage_summary(raw$usage, cfg$provider %||% ""))
    usage <- llm_usage_combine(call_usages)
    parsed <- parse_json_reply(raw$text)
    if (exists("coder_window_event", mode = "function")) coder_window_event(root, dataset, node$id, side, "Chat retry finished")
  }
  reply <- trimws(as.character(parsed$reply %||% "")[1])
  if (!nzchar(reply) && !is.null(parsed$code)) stop("The model returned code instead of a chat reply after one retry.", call. = FALSE)
  if (!nzchar(reply)) stop("The model returned an empty reply after one retry.", call. = FALSE)
  change <- parsed$change
  instruction <- if (is.list(change)) trimws(gsub("[\r\n]+", " ", as.character(change$instruction %||% "")[1])) else ""
  if (nchar(instruction) > 500L) instruction <- substr(instruction, 1L, 500L)
  list(reply = reply, change = if (nzchar(instruction)) list(instruction = instruction, description = as.character(change$description %||% "")[1]) else NULL,
       model = raw$model %||% "", usage = usage)
}

fake_side_agent_reply <- function(system, user) {
  message <- if (exists("text_block", mode = "function")) text_block(user, "MESSAGE_BEGIN", "MESSAGE_END") else ""
  message <- trimws(message %||% "")
  change <- grepl("^(change|use|make)\\b", message, ignore.case = TRUE, perl = TRUE)
  if (change) return(json_text(list(reply = "I will update this side's code and run it.",
                                    change = list(instruction = sub("[.[:space:]]+$", "", message),
                                                  description = "This variable follows the requested change on this side. Its type and missing values come from the resulting R data."))))
  json_text(list(reply = "I can explain this side's code and run log.", change = NULL))
}
