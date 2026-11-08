# Per-side, post-run self review.  This deliberately consumes only one side's
# files: it must never become a route for primary/validation cross-talk.

review_path <- function(project, dataset, node, side) {
  file.path(side_run_dir(project_dir_of(project), dataset, node, side), "review.json")
}

review_progress_path <- function(project, dataset, node, side) {
  file.path(side_run_dir(project_dir_of(project), dataset, node, side), ".reviewing")
}

review_remove <- function(project, dataset, node, side) unlink(review_path(project, dataset, node, side))

review_code_hash <- function(project, dataset, node, side) {
  p <- file.path(side_run_dir(project_dir_of(project), dataset, node, side), "code.R")
  if (!file.exists(p)) return("")
  unname(as.character(tools::md5sum(p))[[1]])
}

review_read <- function(project, dataset, node, side) {
  r <- read_json(review_path(project, dataset, node, side))
  if (is.null(r)) return(NULL)
  r$stale <- !identical(as.character(r$code_hash %||% "")[1], review_code_hash(project, dataset, node, side))
  r
}

review_card_text <- function(project, dataset, node, side) {
  card <- card_read(project, dataset, node, side)
  if (is.null(card)) return("CARD: unavailable")
  paste0("CARD:\n", json_text(card$facts %||% list()))
}

review_source_lines <- function(project, dataset, node, spec) {
  if (!exists("expert_source_pairs", mode = "function") || !exists("lineage_source_path", mode = "function") ||
      !exists("lineage_source_column", mode = "function")) return("SOURCE_LINEAGE_COLUMNS: unavailable")
  row <- spec_rows_text(spec, dataset, parse_node_id(if (is.list(node)) node$id else node)$var)
  refs <- expert_source_pairs(row, dataset)
  lines <- character()
  for (ref in refs) {
    dom <- sub("\\..*$", "", ref); var <- sub("^[^.]*\\.", "", ref)
    col <- lineage_source_column(project, dom, var)
    if (!is.null(col)) lines <- c(lines, lineage_column_line(ref, col, dataset))
  }
  if (!length(lines)) "SOURCE_LINEAGE_COLUMNS: none available" else paste0("SOURCE_LINEAGE_COLUMNS:\n", paste(unique(lines), collapse = "\n"))
}

review_joined_rows <- function(project, dataset, node, side, graph, spec) {
  id <- if (is.list(node)) node$id else as.character(node)[1]
  target <- parse_node_id(id)$var
  out_path <- file.path(side_run_dir(project, dataset, id, side), "out.rds")
  out <- if (file.exists(out_path)) tryCatch(readRDS(out_path), error = function(e) NULL) else NULL
  if (!is.data.frame(out) || !target %in% names(out)) return("JOINED_EVIDENCE: unavailable")
  parent_ids <- unique(vapply(Filter(function(e) identical(e$to, id), graph$edges %||% list()), function(e) e$from, ""))
  parents <- unique(vapply(parent_ids, function(parent) parse_node_id(parent)$var %||% "", ""))
  if (!length(parents)) return("JOINED_EVIDENCE: no direct parents in work")
  keys <- spec_key_names(spec, dataset)
  if (isTRUE(graph$layered)) {
    info <- node_record(node)
    if (!is.null(info$paramcd)) {
      work <- tryCatch(layered_work_frame(project, dataset, side, info, graph, spec), error = function(e) NULL)
    } else {
      subject_keys <- intersect("USUBJID", keys)
      if (!length(subject_keys)) return("JOINED_EVIDENCE: layered dataset has no USUBJID key")
      work <- empty_key_frame(subject_keys)
      ds_nodes <- Filter(function(n) identical(n$kind, "dataset_variable"), graph$nodes %||% list())
      states <- state_nodes(project, dataset)
      for (n in ds_nodes) {
        if (!identical(as.character(states[[n$id]]$level %||% ""), "L3")) next
        path <- file.path(side_run_dir(project, dataset, n$id, side), "out.rds")
        piece <- if (file.exists(path)) tryCatch(plain_frame(readRDS(path)), error = function(e) NULL) else NULL
        if (!is.data.frame(piece) || !n$var %in% names(piece) || !all(subject_keys %in% names(piece))) next
        work <- merge(work, piece[, unique(c(subject_keys, n$var)), drop = FALSE], by = subject_keys,
                      all.x = TRUE, sort = FALSE)
      }
    }
    keys <- intersect(keys, names(work))
  } else {
    work <- tryCatch(readRDS(work_path(project, dataset, side)), error = function(e) NULL)
  }
  if (!is.data.frame(work)) return("JOINED_EVIDENCE: no work")
  present_parents <- parents[nzchar(parents) & parents %in% names(work)]
  cols <- unique(c(keys, present_parents, target))
  if (!length(present_parents)) return("JOINED_EVIDENCE: direct parents not in work")
  if (!length(keys)) return("JOINED_EVIDENCE: dataset keys unavailable")
  x <- merge(out[, intersect(c(keys, target), names(out)), drop = FALSE], work[, intersect(c(keys, present_parents), names(work)), drop = FALSE], by = keys, all.x = TRUE, sort = FALSE)
  set.seed(731L)
  miss <- which(is.na(x[[target]]))
  take <- unique(c(if (length(miss)) sample(miss, min(5L, length(miss))) else integer(), sample(seq_len(nrow(x)), min(20L, nrow(x)))))
  paste0("JOINED_EVIDENCE (up to 20 rows; includes missing target when present):\n", json_text(x[take[seq_len(min(20L, length(take)))], cols[cols %in% names(x)], drop = FALSE]))
}

self_review_prompt <- function(project, dataset, node, side, spec, graph, user_instruction = NULL, retry = FALSE, run_error = NULL) {
  id <- if (is.list(node)) node$id else as.character(node)[1]
  ni <- node_record(node)
  keys_step <- identical(ni$kind,"records")
  keys <- spec_key_names(spec,dataset)
  if (isTRUE(graph$layered) && !is.null(ni$paramcd)) keys <- layer_key_names(spec,dataset,ni$paramcd,graph,project)
  if (!isTRUE(graph$layered) && exists("layer_keys_path",mode="function") && file.exists(layer_keys_path(project,dataset)))
    keys <- layer_key_names(spec,dataset,"*",graph,project)
  dir <- side_run_dir(project, dataset, id, side)
  code <- read_text(file.path(dir, "code.R"))
  approach <- if (isTRUE(graph$layered) && identical(ni$kind,"param_variable")) handling_approach_text(read_json(file.path(dir,"approach.json"))$approach %||% "") else ""
  log <- as.character(read_text(file.path(dir, "log.txt")) %||% "")
  inherited_listing <- grep("^inherited (rows|columns):", strsplit(log, "\n", fixed = TRUE)[[1]], value = TRUE)
  if (!is.null(run_error)) {
    log_lines <- strsplit(log, "\n", fixed = TRUE)[[1]]
    log <- paste(tail(log_lines, 40L), collapse = "\n")
    if (nchar(log, type = "chars") > 2000L) log <- substr(log, nchar(log, type = "chars") - 1999L, nchar(log, type = "chars"))
  }
  notes <- read_side_notes(project, dataset, id, side)
  temp <- side_temp_name(dataset,node)
  frame <- tryCatch(side_prompt_frame(project,dataset,side,spec,graph,node_record(node)),error=function(e)NULL)
  frame_rows <- if(is.data.frame(frame))nrow(frame)else 0L
  frame_cols <- if(is.data.frame(frame))names(frame)else character()
  records_group_rule <- ""
  if (isTRUE(graph$layered) && keys_step && identical(toupper(ni$paramcd %||% ""), "CARRY_OVER")) {
    group_layers <- Filter(function(x) identical(toupper(x$paramcd %||% ""), "CARRY_OVER"), as_records(graph$layers %||% list()))
    members <- if (length(group_layers)) flat_chr(group_layers[[1]]$members_parameters) else character()
    records_group_rule <- paste0("CARRY_OVER_MEMBERS: ", paste(members, collapse = ", "),
      "\nThe Records result may use only these member PARAMCD values and must never use the synthetic group code CARRY_OVER. ")
  }
  paste(
    side_role_line(side, dataset), "TASK: self_review",
    if (keys_step) paste0("You are reviewing the completed Keys step. Judge whether the result contains exactly the named key columns, with one row per record, and whether its keys follow their specifications.") else if (identical(side, "primary")) "You are reviewing one completed primary R result. Judge whether it follows the target specification literally." else "You are reviewing one completed validation R result. Judge whether it matches the variable's meaning; use the spec as context, and do not mark a spec-path deviation unexpected by itself.",
    "Return only JSON: {\"verdict\":\"expected|unexpected|uncertain\",\"reasoning\":\"2-4 sentences\",\"issues\":[\"...\"],\"spec_question\":null|\"...\"}.",
    if (identical(side, "primary")) "If the code follows literal spec wording but the spec looks wrong, use uncertain and fill spec_question. Do not propose changing code away from the spec." else "Assess the validation result independently; a different source or derivation can be correct.",
    if (keys_step) paste0("VAR: KEYS\nKEYS: ",paste(keys,collapse=","),"\n",records_group_rule,"\nKEY_COLUMN_SPEC:\n",
      paste(vapply(keys,function(k)spec_rows_text(spec,dataset,k),""),collapse="\n\n")) else paste0("TARGET_SPEC:\n", spec_rows_text(spec, dataset, parse_node_id(id)$var)),
    paste0("DATASET_RULES:\n", lineage_dataset_rules(spec, dataset), "\nKEYS: ", paste(keys, collapse = ",")),
    paste0("USER_INSTRUCTION: ", field_txt(user_instruction)),
    paste0("NOTES: ", json_text(notes)),
    paste0("CODE_BEGIN\n", code, "\nCODE_END"),
    if (isTRUE(graph$layered) && identical(ni$kind,"param_variable") && nzchar(approach)) paste0("APPROACH:\n",approach,"\nQuestion: Does the code do what approach says? Report any mismatch through the existing self-review result and issues.") else "",
    paste0("R_LOG_BEGIN\n", log, "\nR_LOG_END"),
    review_card_text(project, dataset, id, side),
    paste0("INHERITED_FRAME_LISTING:\n", if (length(inherited_listing)) paste(inherited_listing, collapse = "\n") else "unavailable"),
    paste0("TEMP_DATASET: ",temp,"\nTEMP_FRAME_ROWS: ",frame_rows,"\nTEMP_FRAME_COLUMNS: ",paste(frame_cols,collapse=","),"\nThis code is one step in this side's chain. Earlier steps built ",temp,"; ancestor information is background, while this is the actual inherited data and can hold more columns. Review whether the target was added to that same frame.",if(identical(side,"primary"))" Primary should build from lineage variables first. A needed column present in the LINEAGE_ frame but outside DAG lineage is allowed and produces an amber app warning, not a failure or reason to stop. Do not print the app's warning. Treat only a needed column absent from the frame and provided source data as a Missing column failure; the user-facing form is Missing column: X is not in LINEAGE_SYSBP or the provided data." else " Validation may use any built column on this side."),
    review_source_lines(project, dataset, id, spec),
    review_joined_rows(project, dataset, id, side, graph, spec),
    expert_thread_text(project,dataset,side,node),
    if (!is.null(run_error)) paste0("ERROR_REVIEW: The side's program failed. Identify what went wrong and how to fix it.\nRUN_ERROR: ", run_error) else "",
    if (isTRUE(retry)) "REVIEW_RETRY: true" else "", sep = "\n\n")
}

self_review <- function(cfg, project, dataset, node, side, spec, graph, user_instruction = NULL, retry = FALSE, run_error = NULL) {
  keys_step <- identical(node_record(node)$kind,"records")
  system <- paste(side_role_line(side, dataset), "TASK: self_review", if(keys_step) "Review the side-local Keys step and its key columns." else paste0("Review the side-local ",side_temp_name(dataset,node)," build step and its target."), "Review only the supplied side. Never request other-side information.", sep = "\n")
  user <- self_review_prompt(project, dataset, node, side, spec, graph, user_instruction, retry, run_error)
  if (exists("coder_window_prompt", mode = "function")) coder_window_prompt(project, dataset, node$id %||% node, side, system, user, "self-review")
  if (exists("coder_window_event", mode = "function")) coder_window_event(project, dataset, node$id %||% node, side, "Model call started", "self-review")
  on.exit(if (!is.null(project) && exists("coder_window_event", mode = "function")) coder_window_event(project, dataset, node$id %||% node, side, "Model call finished", "self-review"), add = TRUE)
  raw <- llm_chat(cfg, system, user, max_tokens = 800L, json = TRUE)
  x <- parse_json_reply(raw$text)
  verdict <- tolower(as.character(x$verdict %||% "uncertain")[1])
  if (!verdict %in% c("expected", "unexpected", "uncertain")) verdict <- "uncertain"
  list(verdict = verdict, reasoning = as.character(x$reasoning %||% "")[1], issues = as.list(x$issues %||% list()),
       spec_question = x$spec_question %||% NULL, code_hash = review_code_hash(project, dataset, node, side), time = now_iso(), retried = FALSE)
}

review_write <- function(project, dataset, node, side, review) {
  write_json(review, review_path(project, dataset, node, side)); review
}

fake_self_review_reply <- function(system, user) {
  bad <- grepl("FAKE_UNEXPECTED", user, fixed = TRUE) && !grepl("REVIEW_RETRY: true", user, fixed = TRUE)
  json_text(list(verdict = if (bad) "unexpected" else "expected", reasoning = if (bad) "The requested test marker requires one retry." else "The result follows the supplied specification and evidence.", issues = if (bad) list("FAKE_UNEXPECTED") else list(), spec_question = NULL))
}
