readiness_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
if (!exists("fullrun_status", mode = "function", inherits = TRUE)) {
  utils_file <- file.path(readiness_root, "R", "utils.R")
  sys.source(utils_file, envir = .GlobalEnv)
  for (f in setdiff(list.files(file.path(readiness_root, "R"), pattern = "\\.R$", full.names = TRUE), utils_file))
    sys.source(f, envir = .GlobalEnv)
}

readiness_nonlayer_project <- function() {
  p <- tempfile("fullrun-readiness-")
  dir.create(file.path(p, "spec"), recursive = TRUE)
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "X,USUBJID,Subject,Char,20,1,,", "X,A,A,Num,8,,", "X,B,B,Num,8,,"),
    file.path(p, "spec", "variable.csv"))
  proj <- open_project(p)
  spec <- load_project_spec(p)
  graph <- graph_skeleton(spec, "X")
  save_graph(proj, graph)
  list(p = p, proj = proj, spec = spec, graph = graph)
}

readiness_layered_project <- function() {
  p <- tempfile("fullrun-layer-readiness-")
  dir.create(file.path(p, "spec"), recursive = TRUE)
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "X,USUBJID,Subject,Char,20,1,,", "X,PARAMCD,Parameter,Char,8,2,,",
    "X,STUDYID,Study,Char,8,,,", "X,AVAL,Value,Num,8,,"), file.path(p, "spec", "variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation", "X,P,First,,", "X,Q,Second,,"),
    file.path(p, "spec", "parameter.csv"))
  proj <- open_project(p)
  spec <- load_project_spec(p)
  graph <- graph_skeleton(spec, "X")
  graph$generated_parameters <- list("P")
  save_graph(proj, graph)
  param_graph_write(proj, list(dataset = "X", nodes = param_defined_nodes(spec, "X"), edges = list(),
    error = NULL, warnings = list()))
  list(p = p, proj = proj, spec = spec, graph = graph)
}

readiness_api_call <- function(A, path, method = "GET", body = NULL) {
  query <- ""
  if (grepl("?", path, fixed = TRUE)) {
    query <- sub("^[^?]*\\?", "", path)
    path <- sub("\\?.*$", "", path)
  }
  raw_body <- if (is.null(body)) raw() else charToRaw(as.character(to_json(body)))
  make_handlers(A)$call(list(PATH_INFO = path, REQUEST_METHOD = method, QUERY_STRING = query,
    HTTP_HOST = sprintf("127.0.0.1:%d", A$port), HTTP_X_DUALDAG_TOKEN = A$token,
    CONTENT_TYPE = if (identical(method, "POST")) "application/json" else "",
    rook.input = list(read = function() raw_body)))
}

readiness_write_code <- function(project, dataset, id, side, code) {
  dir <- file.path(node_run_dir(project, dataset, id), side)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  write_text(code, file.path(dir, "code.R"))
}

test_that("fullrun readiness counts planned variables with code independently per side", {
  fx <- readiness_nonlayer_project()
  readiness_write_code(fx$p, "X", "X.USUBJID", "primary", "result <- 1")
  readiness_write_code(fx$p, "X", "X.A", "validation", "result <- 2")
  primary <- fullrun_status(fx$p, "X", "primary")$readiness
  validation <- fullrun_status(fx$p, "X", "validation")$readiness
  expect_equal(primary$layers_total, 0L)
  expect_equal(primary$layers_generated, 0L)
  expect_length(primary$layers_missing, 0L)
  expect_equal(primary$variables_total, 3L)
  expect_equal(primary$variables_with_code, 1L)
  expect_setequal(unlist(primary$variables_without_code), c("X.A", "X.B"))
  expect_equal(validation$variables_total, 3L)
  expect_equal(validation$variables_with_code, 1L)
  expect_setequal(unlist(validation$variables_without_code), c("X.USUBJID", "X.B"))
})

test_that("layered fullrun readiness reports missing DAGs without blocking a current run", {
  fx <- readiness_layered_project()
  plan <- fullrun_plan(fx$p, "X", "primary")
  readiness_write_code(fx$p, "X", "X.STUDYID", "primary", "result <- 1")
  readiness_write_code(fx$p, "X", "X.STUDYID", "validation", "result <- 1")
  readiness_write_code(fx$p, "X", "X.AVAL@P", "validation", "result <- 2")
  primary <- fullrun_status(fx$p, "X", "primary")$readiness
  validation <- fullrun_status(fx$p, "X", "validation")$readiness
  expect_equal(primary$layers_total, 2L)
  expect_equal(primary$layers_generated, 1L)
  expect_identical(unlist(primary$layers_missing), "Q")
  expect_equal(primary$variables_total, length(plan$ids))
  expect_equal(primary$variables_with_code, 1L)
  expect_equal(validation$variables_with_code, 2L)

  app <- create_app(readiness_root, fx$p, 18729L, "readiness-token", load_config())
  status <- readiness_api_call(app, "/api/fullrun/status?dataset=X")
  expect_equal(status$status, 200L)
  status_json <- from_json(status$body)
  expect_identical(unlist(status_json$sides$primary$readiness$layers_missing), "Q")
  accepted <- readiness_api_call(app, "/api/fullrun/run", "POST", list(dataset = "X", side = "primary"))
  expect_equal(accepted$status, 202L)
})
