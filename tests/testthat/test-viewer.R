# Data viewer API and re-run of stored code.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
for (f in c("utils.R", "deps.R", "config.R", "llm.R", "deps_extract.R", "dag.R",
            "spec.R", "project.R", "catalog.R", "sides.R", "compare.R", "qc.R",
            "export.R", "viewer.R", "server.R")) {
  sys.source(file.path(app_root, "R", f), envir = globalenv())
}

viewer_spec_csv <- function() {
  paste(c(
    "Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Unique Subject,Char,20,1,DM.USUBJID,copy",
    "ADSL,AGE,Age,Num,8,,DM.AGE,copy",
    "ADSL,AGE2,Age plus one,Num,8,,,AGE + 1"
  ), collapse = "\n")
}

isolate_viewer_home <- function() {
  old <- Sys.getenv(c("DUALDAG_HOME", "DUALDAG_LLM_API_KEY", "DUALDAG_FAKE_LLM"))
  home <- tempfile("dualdag-home-")
  dir.create(home)
  Sys.setenv(DUALDAG_HOME = home, DUALDAG_LLM_API_KEY = "")
  list(old = old, restore = function() {
    for (nm in names(old)) {
      if (identical(nm, "DUALDAG_HOME")) restore_test_env(nm, old[[nm]])
      else if (!nzchar(old[[nm]])) Sys.unsetenv(nm) else {
        args <- list(old[[nm]]); names(args) <- nm; do.call(Sys.setenv, args)
      }
    }
  })
}

call_app <- function(A, path, method = "GET", body = NULL, token = A$token) {
  q <- ""
  if (grepl("?", path, fixed = TRUE)) {
    q <- sub("^[^?]*\\?", "", path)
    path <- sub("\\?.*$", "", path)
  }
  raw <- if (is.null(body)) raw() else charToRaw(as.character(to_json(body)))
  req <- list(
    PATH_INFO = path, REQUEST_METHOD = method, QUERY_STRING = q,
    HTTP_HOST = sprintf("127.0.0.1:%d", A$port),
    HTTP_X_DUALDAG_TOKEN = if (is.null(token)) "" else token,
    CONTENT_TYPE = if (identical(method, "POST")) "application/json" else "",
    rook.input = list(read = function() raw)
  )
  make_handlers(A)$call(req)
}

resp_json <- function(res) from_json(res$body)

find_entry <- function(items, kind, node = NULL, side = NULL) {
  for (e in items) {
    if (!identical(e$kind, kind)) next
    if (!is.null(node) && !identical(e$node, node)) next
    if (!is.null(side) && !identical(e$side, side)) next
    return(e)
  }
  NULL
}

col_by_name <- function(columns, name) {
  for (c in columns) if (identical(c$name, name)) return(c)
  NULL
}

write_result <- function(project, dataset, node, side, df) {
  dir <- side_run_dir(project, dataset, node, side)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(as.data.frame(df, stringsAsFactors = FALSE), file.path(dir, "out.rds"))
  dir
}

set_level <- function(p, ds, id, level) {
  st <- get_status(p, ds, id)
  st$level <- level
  st$problem <- NULL
  st$same_source <- TRUE
  st$sides$primary <- list(ok = TRUE, error = NULL, updated = now_iso())
  st$sides$validation <- list(ok = TRUE, error = NULL, updated = now_iso())
  put_status(p, ds, id, st)
}

test_that("dataset list, paging, filter, and the mtime cache", {
  box <- isolate_viewer_home()
  on.exit(box$restore(), add = TRUE)
  proj <- tempfile("dualdag-view-")
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  dir.create(file.path(proj, "data"), recursive = TRUE)
  dir.create(file.path(proj, "output"), recursive = TRUE)
  writeLines(viewer_spec_csv(), file.path(proj, "spec", "variable.csv"))
  file.copy(file.path(app_root, "tests", "fixtures", "dm.xpt"), file.path(proj, "data", "dm.xpt"))
  file.copy(file.path(proj, "data", "dm.xpt"), file.path(proj, "output", "adsl.xpt"))

  p <- open_project(proj)
  save_graph(p, graph_skeleton(load_project_spec(proj), "ADSL"))
  write_result(proj, "ADSL", "ADSL.USUBJID", "primary", data.frame(USUBJID = c("S1", "S2", "S3")))
  write_result(proj, "ADSL", "ADSL.USUBJID", "validation", data.frame(USUBJID = c("S1", "S2", "S3")))
  set_level(p, "ADSL", "ADSL.USUBJID", "L3")
  write_result(proj, "ADSL", "ADSL.AGE", "primary",
               data.frame(USUBJID = c("S1", "S2"), AGE = c(40, 41), stringsAsFactors = FALSE))
  write_result(proj, "ADSL", "ADSL.AGE", "validation",
               data.frame(USUBJID = c("S1", "S2"), AGE = c(40, 41), stringsAsFactors = FALSE))
  write_result(proj, "ADSL", "ADSL.AGE2", "primary",
               data.frame(USUBJID = c("S2", "S3"), AGE2 = c(5, 6), stringsAsFactors = FALSE))
  write_result(proj, "ADSL", "ADSL.DT", "primary", data.frame(
    USUBJID = c("S1", "S2", "S3", "S4", "S5"),
    AGE = c(40, NA, 42, 43, 44),
    DT = as.Date(c("2020-01-02", NA, "2020-03-04", "2020-04-05", "2020-05-06")),
    NOTE = c("keep", "drop", "ZeBrA", "A.B", "AXB"),
    stringsAsFactors = FALSE
  ))
  set_level(p, "ADSL", "ADSL.AGE", "L3")
  set_level(p, "ADSL", "ADSL.AGE2", "L3")

  A <- create_app(app_root, proj, 27329L, "view-token", load_config())
  listed <- call_app(A, "/api/datasets?side=primary")
  expect_equal(listed$status, 200L)
  items <- resp_json(listed)
  expect_true(is.list(items))
  kinds <- vapply(items, function(e) e$kind, "")
  expect_true(all(c("input", "result", "generated", "output") %in% kinds))
  expect_false(any(grepl("^asm:", vapply(items, `[[`, "", "id"))))

  input <- find_entry(items, "input")
  expect_equal(input$id, "in:dm.xpt")
  expect_equal(input$dataset, "DM")
  expect_equal(input$file, "dm.xpt")
  expect_equal(input$nrow, 3)
  expect_null(input$node)
  expect_null(input$side)

  age_p <- find_entry(items, "result", "ADSL.AGE", "primary")
  age_v <- find_entry(items, "result", "ADSL.AGE", "validation")
  expect_equal(age_p$id, "res:ADSL:ADSL.AGE:primary")
  expect_equal(age_v$id, "res:ADSL:ADSL.AGE:validation")
  expect_match(age_p$file, "AGE/primary/out.rds")
  expect_equal(find_entry(items, "generated", side = "primary")$id, "frame:ADSL:primary")
  expect_equal(find_entry(items, "generated", side = "primary")$dataset, "ADSL")
  expect_equal(find_entry(items, "output")$id, "out:adsl.xpt")
  expect_equal(find_entry(items, "output")$file, "adsl.xpt")

  viewer_cache_reset()
  page1 <- call_app(A, "/api/datasets/view?id=in:dm.xpt&offset=0&limit=1")
  expect_equal(page1$status, 200L)
  expect_equal(viewer_file_reads(), 1)
  body1 <- resp_json(page1)
  expect_equal(body1$rows[[1]][[1]], "S1")
  page2 <- call_app(A, "/api/datasets/view?id=in:dm.xpt&offset=1&limit=1")
  expect_equal(viewer_file_reads(), 1)
  body2 <- resp_json(page2)
  expect_equal(body2$offset, 1)
  expect_equal(body2$limit, 1)
  expect_equal(body2$nrow, 3)
  expect_equal(length(body2$rows), 1)
  expect_equal(body2$rows[[1]][[1]], "S2")

  wide <- call_app(A, "/api/datasets/view?id=in:dm.xpt&limit=5000")
  expect_equal(resp_json(wide)$limit, 1000)

  dt <- call_app(A, "/api/datasets/view?id=res:ADSL:ADSL.DT:primary&offset=0&limit=2")
  expect_equal(dt$status, 200L)
  dt_body <- resp_json(dt)
  expect_equal(dt_body$nrow, 5)
  expect_equal(length(dt_body$rows), 2)
  expect_equal(length(dt_body$rows[[1]]), length(dt_body$columns))
  expect_equal(col_by_name(dt_body$columns, "DT")$type, "date")
  expect_equal(dt_body$rows[[1]][[3]], "2020-01-02")
  expect_null(dt_body$rows[[2]][[2]])
  expect_null(dt_body$rows[[2]][[3]])

  filtered <- resp_json(call_app(A, "/api/datasets/view?id=res:ADSL:ADSL.DT:primary&filter=zebra&cols=USUBJID,AGE"))
  expect_equal(filtered$nrow, 1)
  expect_equal(filtered$ncol, 2)
  expect_equal(vapply(filtered$columns, function(c) c$name, ""), c("USUBJID", "AGE"))
  expect_equal(filtered$rows, list(list("S3", "42")))

  dotted <- resp_json(call_app(A, "/api/datasets/view?id=res:ADSL:ADSL.DT:primary&filter=.&cols=NOTE"))
  expect_equal(dotted$nrow, 1)
  expect_equal(dotted$rows[[1]][[1]], "A.B")

  frame <- resp_json(call_app(A, "/api/datasets/view?id=frame:ADSL:primary"))
  expect_equal(vapply(frame$columns, function(c) c$name, ""), c("USUBJID", "AGE", "AGE2"))
  expect_equal(col_by_name(frame$columns, "AGE")$label, "Age")
  expect_equal(frame$nrow, 3)
  rows_by_id <- setNames(frame$rows, vapply(frame$rows, function(row) row[[1]], ""))
  expect_equal(rows_by_id[["S1"]], list("S1", "40", NULL))
  expect_equal(rows_by_id[["S2"]], list("S2", "41", "5"))
  expect_equal(rows_by_id[["S3"]], list("S3", NULL, "6"))

  before <- viewer_file_reads()
  again <- call_app(A, "/api/datasets/view?id=frame:ADSL:primary&offset=1&limit=1")
  expect_equal(again$status, 200L)
  expect_equal(viewer_file_reads(), before)
  expect_equal(resp_json(again)$rows[[1]][[1]], frame$rows[[2]][[1]])

  expect_equal(call_app(A, "/api/datasets/view?id=asm:ADSL")$status, 404L)
  missing <- call_app(A, "/api/datasets/view?id=nope")
  expect_equal(missing$status, 404L)
  expect_match(resp_json(missing)$error, "Unknown dataset")
})

test_that("re-run uses stored code, skips the LLM, and refuses when code is missing", {
  box <- isolate_viewer_home()
  on.exit(box$restore(), add = TRUE)
  Sys.unsetenv("DUALDAG_FAKE_LLM")
  calls <- 0L
  orig <- llm_chat
  llm_chat <<- function(...) {
    calls <<- calls + 1L
    stop("llm_chat must not be called", call. = FALSE)
  }
  on.exit({
    llm_chat <<- orig
    box$restore()
  }, add = TRUE)

  proj <- tempfile("dualdag-rerun-")
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  dir.create(file.path(proj, "data"), recursive = TRUE)
  writeLines(viewer_spec_csv(), file.path(proj, "spec", "variable.csv"))
  file.copy(file.path(app_root, "tests", "fixtures", "dm.xpt"), file.path(proj, "data", "dm.xpt"))
  p <- open_project(proj)
  save_graph(p, list(
    dataset = "ADSL", confirmed = TRUE, confirmed_hash = "x",
    nodes = list(
      list(id = "ADSL.AGE", var = "AGE", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.AGE2", var = "AGE2", paramcd = NULL, kind = "variable")
    ),
    edges = list(list(from = "ADSL.AGE", to = "ADSL.AGE2", evidence = "AGE + 1", origin = "user")),
    groups = list(), external = list(), unresolved = list()
  ))
  set_level(p, "ADSL", "ADSL.AGE", "L3")
  set_level(p, "ADSL", "ADSL.AGE2", "L3")
  write_result(proj, "ADSL", "ADSL.AGE", "primary",
               data.frame(USUBJID = c("S1", "S2", "S3"), AGE = c(1, 2, 3), stringsAsFactors = FALSE))
  write_result(proj, "ADSL", "ADSL.AGE", "validation",
               data.frame(USUBJID = c("S1", "S2", "S3"), AGE = c(1, 2, 3), stringsAsFactors = FALSE))
  write_result(proj, "ADSL", "ADSL.AGE2", "primary",
               data.frame(USUBJID = c("S1", "S2", "S3"), AGE2 = c(2, 3, 4), stringsAsFactors = FALSE))
  compare <- file.path(node_run_dir(p, "ADSL", "ADSL.AGE"), "compare.json")
  write_text("{}\n", compare)

  A <- create_app(app_root, proj, 27330L, "run-token", load_config())
  expect_equal(get_status(A$project, "ADSL", "ADSL.AGE")$level, "L3")
  denied <- call_app(A, "/api/node/run", "POST", list(dataset = "ADSL", node = "ADSL.AGE", side = "both"))
  expect_equal(denied$status, 409L)
  expect_match(resp_json(denied)$error, "stored code")
  expect_equal(calls, 0L)
  expect_equal(get_status(A$project, "ADSL", "ADSL.AGE")$level, "L3")
  expect_equal(get_status(A$project, "ADSL", "ADSL.AGE2")$level, "L3")
  expect_true(file.exists(compare))

  code <- paste(
    "result <- data.frame(",
    "  USUBJID = src$DM$USUBJID,",
    "  AGE = src$DM$AGE + 5,",
    "  stringsAsFactors = FALSE",
    ")",
    sep = "\n"
  )
  primary_code <- file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "primary"), "code.R")
  validation_code <- file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "validation"), "code.R")
  write_text(code, primary_code)
  partial <- call_app(A, "/api/node/run", "POST", list(dataset = "ADSL", node = "ADSL.AGE", side = "both"))
  expect_equal(partial$status, 409L)
  expect_match(resp_json(partial)$error, "validation")
  expect_equal(calls, 0L)
  expect_equal(readRDS(file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "primary"), "out.rds"))$AGE, c(1, 2, 3))

  write_text(code, validation_code)
  before <- tools::md5sum(c(primary_code, validation_code))
  ran <- call_app(A, "/api/node/run", "POST", list(dataset = "ADSL", node = "ADSL.AGE", side = "both"))
  expect_equal(ran$status, 200L)
  expect_equal(calls, 0L)
  body <- resp_json(ran)
  expect_equal(body$level, "L2")
  expect_null(body$problem)
  expect_true(isTRUE(body$same_source))
  expect_true(isTRUE(body$sides$primary$ok))
  expect_true(isTRUE(body$sides$validation$ok))
  expect_false(identical(body$level, "L3"))
  expect_equal(tools::md5sum(c(primary_code, validation_code)), before)
  got <- readRDS(file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "primary"), "out.rds"))
  expect_equal(got$AGE, c(45, 46, 47))
  expect_false(file.exists(compare))
  age2 <- get_status(A$project, "ADSL", "ADSL.AGE2")
  expect_equal(age2$level, "L3")
  expect_true("ADSL.AGE" %in% age2$upstream_changed)
  expect_true(file.exists(file.path(node_run_dir(A$project, "ADSL", "ADSL.AGE2"), "primary", "out.rds")))
})
