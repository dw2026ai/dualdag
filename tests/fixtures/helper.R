# Test helpers for the engine. Sourced from tests/testthat/test-*.R.

if (!exists("dualdag_engine_loaded", inherits = TRUE)) {
  engine_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
  for (f in c("utils.R", "llm.R", "deps_extract.R", "dag.R", "levels.R", "logs.R", "sides.R", "cards.R", "self_review.R", "compare.R", "qc.R", "export.R")) {
    source(file.path(engine_root, "R", f), local = FALSE)
  }
  dualdag_engine_loaded <- TRUE
}

fixture_path <- function(...) normalizePath(file.path(testthat::test_path(), "..", "fixtures", ...), mustWork = FALSE)

V <- function(ds, var, label = var, type = "Char", length = 8L, key = NA_integer_,
              source = "", derivation = "", row = NA_integer_) {
  data.frame(row = as.integer(row), Dataset = ds, Variable = var, Label = label, Type = type,
             Length = as.integer(length), Key = as.integer(key), Source = source,
             Derivation = derivation, stringsAsFactors = FALSE)
}

P <- function(ds, paramcd, param = paramcd, source = "", derivation = "", row = NA_integer_) {
  data.frame(row = as.integer(row), Dataset = ds, PARAMCD = paramcd, PARAM = param,
             Source = source, Derivation = derivation, stringsAsFactors = FALSE)
}

spec_of <- function(..., params = NULL) {
  v <- do.call(rbind, list(...))
  rownames(v) <- NULL
  if (all(is.na(v$row))) v$row <- seq_len(nrow(v)) + 1L
  if (is.null(params)) {
    params <- data.frame(row = integer(), Dataset = character(), PARAMCD = character(),
                         PARAM = character(), Source = character(), Derivation = character(),
                         stringsAsFactors = FALSE)
  } else {
    params <- do.call(rbind, params)
    rownames(params) <- NULL
    if (all(is.na(params$row))) params$row <- seq_len(nrow(params)) + 1L
  }
  list(variables = v, parameters = params, datasets = unique(v$Dataset), problems = list(),
       hash = paste(v$Dataset, v$Variable, v$Source, v$Derivation, collapse = "|"))
}

read_fixture_spec <- function(variable_csv, parameter_csv = NULL) {
  v <- utils::read.csv(variable_csv, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
  for (nm in c("Dataset", "Variable", "Label", "Type", "Length", "Key", "Source", "Derivation")) {
    if (!nm %in% names(v)) v[[nm]] <- NA
  }
  v$row <- seq_len(nrow(v)) + 1L
  v$Dataset <- toupper(trimws(v$Dataset))
  v$Variable <- toupper(trimws(v$Variable))
  v$Key <- suppressWarnings(as.integer(v$Key))
  v$Length <- suppressWarnings(as.integer(v$Length))
  v$Source[is.na(v$Source)] <- ""
  v$Derivation[is.na(v$Derivation)] <- ""
  params <- NULL
  if (!is.null(parameter_csv)) {
    params <- utils::read.csv(parameter_csv, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
    params$row <- seq_len(nrow(params)) + 1L
    params$Dataset <- toupper(trimws(params$Dataset))
    params$PARAMCD <- toupper(trimws(params$PARAMCD))
    params$Source[is.na(params$Source)] <- ""
    params$Derivation[is.na(params$Derivation)] <- ""
  }
  spec_of_frames(v, params)
}

spec_of_frames <- function(v, params = NULL) {
  if (is.null(params)) {
    params <- data.frame(row = integer(), Dataset = character(), PARAMCD = character(),
                         PARAM = character(), Source = character(), Derivation = character(),
                         stringsAsFactors = FALSE)
  }
  list(variables = v, parameters = params, datasets = unique(v$Dataset), problems = list(), hash = "fixture")
}

gn <- function(id, paramcd = NULL) {
  base <- sub("@.*$", "", id)
  var <- sub("^[^.]*\\.", "", base)
  list(id = id, var = var, paramcd = paramcd,
       kind = if (is.null(paramcd)) "variable" else "param_variable")
}

ge <- function(from, to, evidence = "", origin = "user") {
  list(from = from, to = to, evidence = evidence, origin = origin)
}

make_graph <- function(dataset, nodes, edges = list(), external = list(), unresolved = list(),
                       groups = list(), confirmed = TRUE) {
  list(dataset = dataset, confirmed = confirmed,
       confirmed_hash = if (isTRUE(confirmed)) "x" else NULL,
       spec_hash = "h", nodes = nodes, groups = groups, edges = edges,
       external = external, unresolved = unresolved)
}

restore_test_env <- function(name, value) {
  if (identical(name, "DUALDAG_HOME") && (is.na(value) || !nzchar(value))) {
    safe <- Sys.getenv("DUALDAG_TEST_HOME", unset = "")
    if (nzchar(safe)) { Sys.setenv(DUALDAG_HOME = safe); return(invisible(NULL)) }
  }
  if (is.na(value)) Sys.unsetenv(name) else {
    args <- list(value); names(args) <- name; do.call(Sys.setenv, args)
  }
  invisible(NULL)
}

with_env <- function(name, value, expr) {
  old <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value)) Sys.unsetenv(name) else {
    args <- list(value)
    names(args) <- name
    do.call(Sys.setenv, args)
  }
  on.exit({
    restore_test_env(name, old)
  }, add = TRUE)
  force(expr)
}

with_fake_llm <- function(expr) with_env("DUALDAG_FAKE_LLM", "1", expr)
without_fake_llm <- function(expr) with_env("DUALDAG_FAKE_LLM", NA_character_, expr)

with_qc_home <- function(expr) {
  home <- tempfile("dualdag-home-")
  dir.create(home)
  with_env("DUALDAG_HOME", home, expr)
}

issue_of <- function(check, type) {
  Filter(function(i) identical(i$type, type), check$issues)
}

edge_pairs <- function(graph) {
  if (!length(graph$edges)) return(character())
  vapply(graph$edges, function(e) paste(e$from, e$to, sep = "->"), "")
}

write_result <- function(project, dataset, node, side, df) {
  dir <- side_run_dir(project, dataset, node, side)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(as.data.frame(df, stringsAsFactors = FALSE), file.path(dir, "out.rds"))
  dir
}

temp_project <- function() {
  p <- tempfile("dualdag-proj-")
  dir.create(file.path(p, "data"), recursive = TRUE)
  p
}
