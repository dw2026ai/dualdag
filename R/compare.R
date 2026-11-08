# Exact per-variable compare by key (F9b) and the mismatch QC report (F9g).
# No tolerance. The user decides; nothing here rewrites code.

qc_system_prompt <- function(spec = NULL, dataset = NULL) {
  body <- paste(
    "TASK: qc_investigate",
    "You investigate a primary-versus-validation mismatch for one variable.",
    "Follow the dataset QC methodology document included in the user message.",
    "Report possible causes: primary code, validation code, an ambiguous specification, source data, or an upstream variable.",
    "The user decides what to do next.",
    "Do not auto-fix code, data, or the specification. Do not change either program.",
    "Return a markdown report and no code edits.",
    sep = "\n"
  )
  prefix <- if (exists("dataset_spec_text", mode = "function")) dataset_spec_text(spec, dataset) else ""
  if (nzchar(prefix)) paste0(prefix, "\n", body) else body
}

qc_user_prompt <- function(dataset, node, diffs, spec_rows, qc_doc) {
  diff_txt <- if (is.null(diffs) || !nrow(diffs)) "(no diff rows)" else {
    utils::capture.output(print(utils::head(as.data.frame(diffs), 30L)))
  }
  spec_txt <- if (is.null(spec_rows)) "" else if (is.data.frame(spec_rows)) {
    paste(utils::capture.output(print(spec_rows)), collapse = "\n")
  } else paste(spec_rows, collapse = "\n")
  paste0(
    "DATASET: ", toupper(dataset %||% ""), "\n",
    "NODE: ", if (is.list(node)) node$id %||% "" else node %||% "", "\n",
    "QC_DOC_BEGIN\n", qc_doc %||% "(no QC document uploaded for this dataset)", "\nQC_DOC_END\n",
    "SPEC_ROWS:\n", spec_txt, "\n",
    "DIFFS:\n", paste(diff_txt, collapse = "\n"), "\n"
  )
}

qc_investigate <- function(cfg, dataset, node, diffs, spec_rows, qc_doc = NULL, spec = NULL) {
  if (is.null(qc_doc)) qc_doc <- qc_get(dataset)
  user <- qc_user_prompt(dataset, node, diffs, spec_rows, qc_doc)
  raw <- llm_chat(cfg, qc_system_prompt(spec, dataset), user, max_tokens = 4000L, json = FALSE)
  raw$text %||% ""
}

fake_qc_reply <- function(system, user) {
  doc <- text_block(user, "QC_DOC_BEGIN", "QC_DOC_END")
  node <- prompt_field(user, "NODE")
  dataset <- prompt_field(user, "DATASET")
  paste0(
    "# QC investigation: ", dataset, " ", node, "\n\n",
    "The values do not match. This report follows the dataset QC document and does not change either program.\n\n",
    "## QC document used\n\n",
    if (nzchar(trimws(doc))) doc else "(none uploaded)",
    "\n\n## Judgement\n\n",
    "Possible causes include the primary code, the validation code, an ambiguous specification line, the source data, or an upstream variable. ",
    "The user decides what to do next. No auto-fix was applied.\n"
  )
}

empty_diff_frame <- function(keys) {
  keys <- flat_chr(keys)
  df <- as.data.frame(matrix(ncol = length(keys) + 2L, nrow = 0L), stringsAsFactors = FALSE)
  names(df) <- c(keys, "primary", "validation")
  df
}

bind_diff_rows <- function(frames, keys) {
  keys <- flat_chr(keys)
  cols <- c(keys, "primary", "validation")
  frames <- Filter(function(df) !is.null(df) && nrow(df) > 0L, frames)
  if (!length(frames)) return(empty_diff_frame(keys))
  frames <- lapply(frames, function(df) {
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    for (nm in cols) if (!nm %in% names(df)) df[[nm]] <- NA
    df[, cols, drop = FALSE]
  })
  out <- do.call(rbind, frames)
  rownames(out) <- NULL
  out
}

harmonize_pair <- function(a, b) {
  a <- as.vector(a)
  b <- as.vector(b)
  if (is.numeric(a) && is.numeric(b)) list(as.numeric(a), as.numeric(b))
  else list(as.character(a), as.character(b))
}

compare_settings <- function(project) {
  list(blank_equals_na = TRUE)
}

compare_blank <- function(x) {
  is.na(x) | (!is.na(x) & !nzchar(trimws(x)))
}

compare_blank_equal_count <- function(p, v, keys, var) {
  # Count only values aligned by key. Duplicate keys remain a compare issue.
  if (length(keys)) {
    a <- p[, unique(c(keys, var)), drop = FALSE]
    b <- v[, unique(c(keys, var)), drop = FALSE]
    names(a)[names(a) == var] <- ".primary"
    names(b)[names(b) == var] <- ".validation"
    joined <- merge(a, b, by = keys, all = FALSE)
    pa <- joined$.primary
    va <- joined$.validation
  } else {
    n <- min(nrow(p), nrow(v))
    pa <- p[[var]][seq_len(n)]
    va <- v[[var]][seq_len(n)]
  }
  if (!length(pa)) return(0L)
  raw_same <- (is.na(pa) & is.na(va)) | (!is.na(pa) & !is.na(va) & pa == va)
  as.integer(sum(compare_blank(pa) & compare_blank(va) & !raw_same))
}

recompare_stored <- function(project, include_matches = FALSE) {
  p <- if (is.character(project)) open_project(project) else project
  changed <- character()
  for (ds in names(p$state$nodes %||% list())) {
    spec <- tryCatch(read_project_spec(p), error = function(e) NULL)
    if (is.null(spec)) next
    keys <- dataset_keys(spec, ds)
    for (id in names(p$state$nodes[[ds]] %||% list())) {
      st <- get_status(p, ds, id)
      if (!st$level %in% c("L2", "L3")) next
      path <- file.path(node_run_dir(p, ds, id), "compare.json")
      old <- read_json(path)
      if (!is.list(old) || is.null(old$match)) next
      if (!include_matches && isTRUE(old$match)) next
      res <- tryCatch(compare_node(p$dir, ds, id, keys), error = function(e) NULL)
      if (is.null(res)) next
      if (!include_matches && !isTRUE(res$match)) next
      if (identical(isTRUE(old$match), isTRUE(res$match)) &&
          identical(as.character(old$summary %||% ""), as.character(res$summary %||% ""))) next
      write_json(res, path)
      before <- st$level
      st$level <- if (isTRUE(res$match)) "L3" else "L2"
      st$problem <- if (isTRUE(res$match)) NULL else list(stage = "compare", message = res$summary)
      put_status(p, ds, id, st, save = FALSE)
      if (identical(before, "L3") && identical(st$level, "L2")) flag_downstream_of(p, ds, id)
      changed <- c(changed, id)
    }
  }
  if (length(changed)) write_state(p)
  unique(changed)
}

prepare_compare_frame <- function(df, keys, var) {
  df <- plain_frame(df)
  if (!var %in% names(df)) stop("Result is missing column ", var, ".", call. = FALSE)
  miss <- setdiff(keys, names(df))
  if (length(miss)) stop("Result is missing key columns: ", paste(miss, collapse = ", "), call. = FALSE)
  df[, unique(c(keys, var)), drop = FALSE]
}

same_key_storage <- function(a, b) {
  ta <- typeof(a)
  tb <- typeof(b)
  if (identical(ta, tb)) return(TRUE)
  num <- c("integer", "double")
  ta %in% num && tb %in% num
}

compare_value_class <- function(x) {
  if (inherits(x, "Date")) return("Date")
  if (inherits(x, "POSIXct")) return("POSIXct")
  if (inherits(x, "difftime")) return("difftime")
  NULL
}

work_row_count <- function(project, dataset, side) {
  n <- 0L
  if (exists("work_path", mode = "function")) {
    path <- work_path(project, dataset, side)
    if (file.exists(path)) {
      df <- tryCatch(readRDS(path), error = function(e) NULL)
      if (is.data.frame(df)) n <- max(n, nrow(df))
    }
  }
  if (exists("assemble_work", mode = "function")) {
    spec <- if (exists("load_project_spec", mode = "function")) {
      tryCatch(load_project_spec(project), error = function(e) NULL)
    } else NULL
    df2 <- tryCatch(assemble_work(project, dataset, side, spec, NULL, NULL), error = function(e) NULL)
    if (is.data.frame(df2)) n <- max(n, nrow(df2))
  }
  n
}

compare_fail <- function(n_primary, n_validation, keys, summary) {
  list(
    match = FALSE,
    n_primary = n_primary,
    n_validation = n_validation,
    n_diff = 0L,
    diffs = empty_diff_frame(keys),
    summary = summary
  )
}

compare_node <- function(project, dataset, node, keys) {
  keys <- flat_chr(keys)
  id <- if (is.list(node)) node$id else as.character(node)[1]
  var <- parse_node_id(id)$var
  p_path <- file.path(side_run_dir(project, dataset, node, "primary"), "out.rds")
  v_path <- file.path(side_run_dir(project, dataset, node, "validation"), "out.rds")
  if (!file.exists(p_path)) stop("Missing primary result for ", id, call. = FALSE)
  if (!file.exists(v_path)) stop("Missing validation result for ", id, call. = FALSE)
  parsed_graph <- tryCatch(load_project_graph(project,dataset),error=function(e)NULL)
  parsed_node <- if (is.list(node)) node else tryCatch(graph_node(parsed_graph, id), error=function(e) NULL)
  layered_node <- isTRUE(parsed_graph$layered)
  layered_dataset_node <- is.list(parsed_node) && identical(parsed_node$kind,"dataset_variable")
  if (layered_node && is.list(parsed_node) && !is.null(parsed_node$paramcd)) {
    spec <- read_project_spec(project)
    keys <- layer_key_names(spec,dataset,parsed_node$paramcd,parsed_graph,project)
  }
  if(!layered_node && !is.null(project) && exists("layer_keys_path",mode="function") && file.exists(layer_keys_path(project,dataset)))
    keys <- layer_key_names(read_project_spec(project),dataset,"*",parsed_graph,project)
  if (is.list(parsed_node) && identical(parsed_node$kind, "records")) {
    p <- plain_frame(readRDS(p_path)); v <- plain_frame(readRDS(v_path))
    all_keys <- if(!is.null(parsed_node$paramcd))layer_key_names(read_project_spec(project),dataset,parsed_node$paramcd,parsed_graph,project) else if(!isTRUE(parsed_graph$layered)&&file.exists(layer_keys_path(project,dataset)))layer_key_names(read_project_spec(project),dataset,"*",parsed_graph,project) else flat_chr(keys)
    missing <- setdiff(all_keys, union(names(p), names(v)))
    if (length(missing)) return(compare_fail(nrow(p), nrow(v), all_keys, paste0("Records missing key column(s): ", paste(missing, collapse=", "))))
    signature <- function(df) {
      if (!all(all_keys %in% names(df))) stop("Records missing key columns.")
      z <- lapply(df[, all_keys, drop=FALSE], function(x) ifelse(is.na(x), "<NA>", as.character(x)))
      do.call(paste, c(z, sep="\034"))
    }
    duplicate_info <- function(df, sig) {
      repeated <- sig[duplicated(sig) | duplicated(sig,fromLast=TRUE)]
      unique_repeated <- unique(repeated)
      if(!length(unique_repeated))return(NULL)
      list(count=length(repeated),sample=df[match(unique_repeated[[1]],sig),all_keys,drop=FALSE])
    }
    ps <- tryCatch(signature(p), error=function(e) character()); vs <- tryCatch(signature(v), error=function(e) character())
    pd <- duplicate_info(p,ps); vd <- duplicate_info(v,vs)
    if(!is.null(pd)||!is.null(vd)) {
      detail <- c(if(!is.null(pd))paste0("primary has ",pd$count," duplicate row(s), sample key ",paste(pd$sample[1,],collapse=" / ")),
                   if(!is.null(vd))paste0("validation has ",vd$count," duplicate row(s), sample key ",paste(vd$sample[1,],collapse=" / ")))
      return(compare_fail(nrow(p),nrow(v),all_keys,paste0("Problem: Records keys do not identify a row; ",paste(detail,collapse="; "),".")))
    }
    p_only <- setdiff(unique(ps), unique(vs)); v_only <- setdiff(unique(vs), unique(ps))
    n_diff <- length(p_only) + length(v_only)
    samples <- c(head(p_only, 3L), head(v_only, 3L))
    diffs <- empty_diff_frame(all_keys)
    if (length(samples)) {
      rows <- lapply(samples, function(s) {
        src <- if (s %in% p_only) p else v
        sig <- if (s %in% p_only) ps else vs
        row <- src[match(s, sig), all_keys, drop=FALSE]
        row$primary <- if (s %in% p_only) "<present>" else NA_character_
        row$validation <- if (s %in% v_only) "<present>" else NA_character_
        row
      })
      diffs <- do.call(rbind, rows); rownames(diffs) <- NULL
    }
    return(list(match=n_diff == 0L, n_primary=nrow(p), n_validation=nrow(v), n_diff=n_diff,
      diffs=diffs, summary=if (n_diff == 0L) "Match" else paste0(n_diff, " differing record key(s)",
      if(!is.null(parsed_node$paramcd))paste0(" for ",parsed_node$paramcd)else "",
      "; primary-only=", length(p_only), ", validation-only=", length(v_only), ".")))
  }
  if(layered_dataset_node) keys <- intersect("USUBJID",keys)
  p <- tryCatch(prepare_compare_frame(readRDS(p_path), keys, var), error = function(e) e)
  v <- tryCatch(prepare_compare_frame(readRDS(v_path), keys, var), error = function(e) e)
  if (inherits(p, "error") || inherits(v, "error")) {
    msg <- paste(unique(c(
      if (inherits(p, "error")) conditionMessage(p) else NULL,
      if (inherits(v, "error")) conditionMessage(v) else NULL
    )), collapse = "; ")
    return(compare_fail(
      if (is.data.frame(p)) nrow(p) else 0L,
      if (is.data.frame(v)) nrow(v) else 0L,
      keys, msg
    ))
  }
  population_note <- ""
  primary_rows <- nrow(p)
  validation_rows <- nrow(v)
  if (layered_dataset_node && "USUBJID" %in% intersect(names(p),names(v))) {
    p_subjects <- unique(as.character(p$USUBJID))
    v_subjects <- unique(as.character(v$USUBJID))
    shared <- intersect(p_subjects,v_subjects)
    primary_only <- setdiff(p_subjects,shared)
    validation_only <- setdiff(v_subjects,shared)
    if (length(primary_only) || length(validation_only)) population_note <- paste0(
      "Compared shared subjects only; primary-only subjects=",length(primary_only),
      ", validation-only subjects=",length(validation_only),".")
    if (length(shared)) {
      p <- p[as.character(p$USUBJID)%in%shared,,drop=FALSE]
      v <- v[as.character(v$USUBJID)%in%shared,,drop=FALSE]
    } else if (nrow(p) && nrow(v)) {
      return(list(match=TRUE,n_primary=primary_rows,n_validation=validation_rows,n_diff=0L,
        diffs=empty_diff_frame(keys),summary=population_note))
    }
  }
  zero <- character()
  if (nrow(p) == 0L) zero <- c(zero, "primary result has 0 rows")
  if (nrow(v) == 0L) zero <- c(zero, "validation result has 0 rows")
  if (length(zero)) return(compare_fail(nrow(p), nrow(v), keys, paste(zero, collapse = "; ")))

  type_msgs <- character()
  pc <- compare_value_class(p[[var]])
  vc <- compare_value_class(v[[var]])
  if (!identical(pc, vc)) {
    type_msgs <- c(type_msgs, sprintf("%s types differ: %s vs %s", var,
                                           pc %||% typeof(p[[var]]), vc %||% typeof(v[[var]])))
  }
  for (k in keys) {
    if (!k %in% names(p) || !k %in% names(v)) next
    if (same_key_storage(p[[k]], v[[k]])) next
    type_msgs <- c(type_msgs, sprintf("key %s types differ: %s vs %s", k, typeof(p[[k]]), typeof(v[[k]])))
  }
  for (k in keys) {
    if (k %in% names(p)) p[[k]] <- as.character(p[[k]])
    if (k %in% names(v)) v[[k]] <- as.character(v[[k]])
  }
  row_msgs <- character()
  n_work_p <- if(layered_node)0L else tryCatch(work_row_count(project, dataset, "primary"), error = function(e) 0L)
  n_work_v <- if(layered_node)0L else tryCatch(work_row_count(project, dataset, "validation"), error = function(e) 0L)
  if (n_work_p > 0L && nrow(p) != n_work_p) {
    row_msgs <- c(row_msgs, sprintf("primary result has %d rows; work has %d", nrow(p), n_work_p))
  }
  if (n_work_v > 0L && nrow(v) != n_work_v) {
    row_msgs <- c(row_msgs, sprintf("validation result has %d rows; work has %d", nrow(v), n_work_v))
  }
  pair <- harmonize_pair(p[[var]], v[[var]])
  character_pair <- is.character(p[[var]]) && is.character(v[[var]])
  blank_policy <- isTRUE(compare_settings(project)$blank_equals_na)
  character_pair <- character_pair && !var %in% keys
  blank_count <- if (character_pair && blank_policy)
    compare_blank_equal_count(p, v, keys, var) else 0L
  p[[var]] <- pair[[1]]
  v[[var]] <- pair[[2]]
  if (character_pair && blank_policy) {
    p[[var]][compare_blank(p[[var]])] <- NA_character_
    v[[var]][compare_blank(v[[var]])] <- NA_character_
  }
  cmp <- tryCatch(suppressWarnings(diffdf::diffdf(
    base = p, compare = v, keys = if (length(keys)) keys else NULL,
    tolerance = 0, file = NULL, suppress_warnings = TRUE,
    check_column_order = FALSE, check_df_class = FALSE
  )), error = function(e) e)
  if (inherits(cmp, "error")) {
    summary <- paste(c(row_msgs, type_msgs, "Compare failed."), collapse = "; ")
    return(compare_fail(nrow(p), nrow(v), keys, summary))
  }
  frames <- list()
  nm <- paste0("VarDiff_", var)
  if (!is.null(cmp[[nm]])) {
    d <- as.data.frame(cmp[[nm]], stringsAsFactors = FALSE)
    part <- if (length(keys)) d[, intersect(keys, names(d)), drop = FALSE] else data.frame(row.names = seq_len(nrow(d)))
    part$primary <- d$BASE
    part$validation <- d$COMPARE
    frames[[length(frames) + 1L]] <- part
  }
  for (extra in c("ExtRowsBase", "ExtRowsComp")) {
    if (is.null(cmp[[extra]])) next
    d <- as.data.frame(cmp[[extra]], stringsAsFactors = FALSE)
    part <- if (length(keys)) d[, intersect(keys, names(d)), drop = FALSE] else d
    part$primary <- NA
    part$validation <- NA
    frames[[length(frames) + 1L]] <- part
  }
  diffs <- bind_diff_rows(frames, keys)
  n_diff <- nrow(diffs)
  has <- isTRUE(diffdf::diffdf_has_issues(cmp))
  blocked <- length(type_msgs) > 0L || length(row_msgs) > 0L
  match <- !has && n_diff == 0L && !blocked
  if (nrow(diffs) > 200L) diffs <- diffs[seq_len(200L), , drop = FALSE]
  summary <- if (match) "Match" else paste(c(
    row_msgs,
    type_msgs,
    if (n_diff > 0L || has || !length(c(row_msgs, type_msgs))) sprintf("%d differing row(s) for %s", n_diff, var) else NULL
  ), collapse = "; ")
  if (nzchar(population_note)) summary <- paste(c(summary,population_note[nzchar(summary)]),collapse="; ")
  if (blank_count > 0L) summary <- paste0(summary, "; ", blank_count,
    " row(s): blank or whitespace-only text and NA treated as equal.")
  list(
    match = match,
    n_primary = primary_rows,
    n_validation = validation_rows,
    n_diff = n_diff,
    diffs = diffs,
    summary = summary
  )
}
