# Batch generate: order, one node at a time, compare only after a clean L2, continue after failure.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
rdir <- file.path(app_root, "R")
files <- list.files(rdir, pattern = "\\.R$", full.names = TRUE)
utils <- files[grepl("utils\\.R$", files)]
for (f in c(utils, files[!files %in% utils])) sys.source(f, envir = globalenv())

VAR_HEADER <- "Dataset,Variable,Label,Type,Length,Key,Source,Derivation"

isolate_batch_home <- function() {
  old <- Sys.getenv(c("DUALDAG_HOME", "DUALDAG_LLM_API_KEY", "DUALDAG_FAKE_LLM"))
  home <- tempfile("dualdag-home-")
  dir.create(home)
  Sys.setenv(DUALDAG_HOME = home, DUALDAG_LLM_API_KEY = "sk-test", DUALDAG_FAKE_LLM = "1")
  list(restore = function() {
    for (nm in names(old)) {
      if (!nzchar(old[[nm]])) Sys.unsetenv(nm) else {
        args <- list(old[[nm]])
        names(args) <- nm
        do.call(Sys.setenv, args)
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

batch_rows <- function(batch) {
  rows <- batch$results
  if (is.null(rows) || !length(rows)) return(list())
  if (is.list(rows) && !is.null(rows$node)) return(list(rows))
  unname(rows)
}

row_nodes <- function(batch) vapply(batch_rows(batch), function(r) as.character(r$node)[1], "")

pump_until <- function(pred, timeout = 180) {
  loadNamespace("later")
  t0 <- Sys.time()
  repeat {
    later::run_now()
    if (isTRUE(pred())) return(invisible(TRUE))
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) stop("Timed out waiting for the batch.", call. = FALSE)
    Sys.sleep(0.05)
  }
}

wait_batch <- function(A, timeout = 180) {
  pump_until(function() is.list(A$batch) && isTRUE(A$batch$finished), timeout)
  A$batch
}

spec_csv <- function(two_keys = FALSE) {
  rows <- c(
    VAR_HEADER,
    "ADSL,USUBJID,Unique Subject,Char,20,1,DM.USUBJID,copy",
    if (two_keys) "ADSL,STUDYID,Study,Char,20,2,DM.STUDYID,copy",
    "ADSL,AGE,Age,Num,8,,DM.AGE,copy",
    "ADSL,AGE2,Age plus one,Num,8,,,AGE + 1",
    "ADSL,NOPE,Not derivable,Char,20,,,not derivable"
  )
  paste(rows, collapse = "\n")
}

batch_project <- function(two_keys = FALSE) {
  proj <- tempfile("dualdag-batch-")
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  dir.create(file.path(proj, "data"), recursive = TRUE)
  writeLines(spec_csv(two_keys), file.path(proj, "spec", "variable.csv"))
  file.copy(file.path(app_root, "tests", "fixtures", "dm.xpt"), file.path(proj, "data", "dm.xpt"))
  p <- open_project(proj)
  nodes <- list(
    list(id = "ADSL.AGE2", var = "AGE2", paramcd = NULL, kind = "variable"),
    list(id = "ADSL.NOPE", var = "NOPE", paramcd = NULL, kind = "variable"),
    list(id = "ADSL.AGE", var = "AGE", paramcd = NULL, kind = "variable"),
    list(id = "ADSL.USUBJID", var = "USUBJID", paramcd = NULL, kind = "variable")
  )
  if (two_keys) {
    nodes <- c(nodes, list(list(id = "ADSL.STUDYID", var = "STUDYID", paramcd = NULL, kind = "variable")))
  }
  save_graph(p, list(
    dataset = "ADSL",
    nodes = nodes,
    edges = list(
      list(from = "ADSL.AGE", to = "ADSL.AGE2", evidence = "AGE2 = AGE + 1", origin = "user")
    ),
    groups = list(), external = list(), unresolved = list()
  ))
  note_graph_nodes(p, "ADSL", load_graph(p, "ADSL"))
  proj
}

test_that("batch order is keys, then topological order, then leftovers", {
  box <- isolate_batch_home()
  on.exit(box$restore(), add = TRUE)
  proj <- batch_project(two_keys = TRUE)
  p <- open_project(proj)
  asked <- c("ADSL.AGE2", "ADSL.NOPE", "ADSL.AGE", "ADSL.STUDYID", "ADSL.USUBJID", "ADSL.EXTRA")
  # EXTRA is not a graph node. The pure order still places it last.
  got <- batch_order_for(p, "ADSL", asked[asked != "ADSL.EXTRA"])
  expect_equal(got, c("ADSL.USUBJID", "ADSL.STUDYID", "ADSL.NOPE", "ADSL.AGE", "ADSL.AGE2"))
  topo <- inspect_topo(load_graph(p, "ADSL"))
  leftover <- batch_node_order(c(asked, "ADSL.AGE2"), paste0("ADSL.", dataset_keys(read_project_spec(p), "ADSL")), topo)
  expect_equal(leftover, c("ADSL.USUBJID", "ADSL.STUDYID", "ADSL.NOPE", "ADSL.AGE", "ADSL.AGE2", "ADSL.EXTRA"))
})

test_that("keep_order preserves the requested batch order", {
  box <- isolate_batch_home()
  on.exit(box$restore(), add = TRUE)
  proj <- batch_project()
  A <- create_app(app_root, proj, 7983L, "batch-token", load_config())
  asked <- c("ADSL.AGE2", "ADSL.USUBJID", "ADSL.AGE")
  for (id in asked) A$jobs[[paste0("hold-", id)]] <- list(id = paste0("hold-", id), kind = "generate", dataset = "ADSL", node = id, started = as.numeric(Sys.time()), model = NULL)
  res <- call_app(A, "/api/batch", "POST", list(dataset = "ADSL", nodes = as.list(asked), compare = TRUE, keep_order = TRUE))
  expect_equal(res$status, 200L)
  wait_batch(A, timeout = 30)
  expect_equal(row_nodes(A$batch), asked)
  A$jobs <- list()
})

test_that("a blocked queued node is reported and later nodes still run", {
  box <- isolate_batch_home()
  on.exit(box$restore(), add = TRUE)
  proj <- batch_project()
  A <- create_app(app_root, proj, 7982L, "batch-token", load_config())
  asked <- c("ADSL.AGE2", "ADSL.AGE", "ADSL.USUBJID")
  res <- call_app(A, "/api/batch", "POST", list(dataset = "ADSL", nodes = as.list(asked), compare = TRUE, keep_order = TRUE))
  expect_equal(res$status, 200L)
  wait_batch(A, timeout = 180)
  rows <- batch_rows(A$batch)
  expect_equal(row_nodes(A$batch), asked)
  expect_match(rows[[1]]$message, "upstream", ignore.case = TRUE)
  expect_true(isTRUE(rows[[2]]$ok))
  expect_true(isTRUE(rows[[3]]$ok))
})

test_that("a failed compare is recorded and batch continuation is requested", {
  box <- isolate_batch_home()
  on.exit(box$restore(), add = TRUE)
  proj <- batch_project()
  p <- open_project(proj)
  put_status(p, "ADSL", "ADSL.AGE", modifyList(blank_status(), list(level = "L2")))
  A <- new.env(parent = globalenv())
  A$project <- p
  A$batch <- list(dataset = "ADSL", current = "ADSL.AGE", results = list(), done = 0L,
    finished = FALSE, busy_step = TRUE, index = 2L, order = c("ADSL.AGE", "ADSL.AGE2"), total = 2L)
  old <- get("batch_continue", envir = .GlobalEnv)
  assign("batch_continue", function(A) { A$continued <- TRUE; invisible() }, envir = .GlobalEnv)
  on.exit(assign("batch_continue", old, envir = .GlobalEnv), add = TRUE)
  batch_after_compare(A, "ADSL.AGE", list(status = 500L))
  expect_true(isTRUE(A$continued))
  expect_false(isTRUE(A$batch$results[[1]]$ok))
  expect_equal(A$batch$results[[1]]$message, "Compare failed.")
  expect_true(isTRUE(A$batch$results[[1]]$compared))
  expect_false(isTRUE(A$batch$results[[1]]$compare_ok))
})

test_that("a completed non-matching compare is distinguishable from generate-only", {
  box <- isolate_batch_home()
  on.exit(box$restore(), add = TRUE)
  proj <- batch_project()
  p <- open_project(proj)
  st <- modifyList(blank_status(), list(level = "L2", problem = list(stage = "compare", message = "Values differ")))
  put_status(p, "ADSL", "ADSL.AGE", st)
  A <- new.env(parent = globalenv())
  A$project <- p
  A$batch <- list(dataset = "ADSL", current = "ADSL.AGE", results = list(), done = 0L,
    finished = FALSE, busy_step = TRUE, index = 1L, order = "ADSL.AGE", total = 1L)
  old <- get("batch_continue", envir = .GlobalEnv)
  assign("batch_continue", function(A) invisible(), envir = .GlobalEnv)
  on.exit(assign("batch_continue", old, envir = .GlobalEnv), add = TRUE)
  batch_after_compare(A, "ADSL.AGE", list(status = 200L))
  row <- A$batch$results[[1]]
  expect_true(isTRUE(row$compared))
  expect_true(isTRUE(row$compare_ok))
  expect_false(isTRUE(row$ok))
  expect_equal(row$level, "L2")
})

test_that("batch state shape, unknown ids, skip, and a second batch is 409", {
  box <- isolate_batch_home()
  on.exit(box$restore(), add = TRUE)
  proj <- batch_project()
  A <- create_app(app_root, proj, 7984L, "batch-token", load_config())
  st0 <- resp_json(call_app(A, "/api/state"))
  expect_null(st0$batch)

  bad <- call_app(A, "/api/batch", "POST", list(dataset = "ADSL", nodes = list("ADSL.NOPE", "ADSL.MISSING"), compare = FALSE))
  expect_equal(bad$status, 400L)
  expect_match(resp_json(bad)$error, "Unknown node", fixed = TRUE)
  empty <- call_app(A, "/api/batch", "POST", list(dataset = "ADSL", nodes = list(), compare = FALSE))
  expect_equal(empty$status, 400L)
  none <- call_app(A, "/api/batch/cancel", "POST", list())
  expect_equal(none$status, 409L)

  ids <- c("ADSL.AGE2", "ADSL.NOPE", "ADSL.USUBJID")
  for (id in ids) {
    A$jobs[[paste0("hold-", id)]] <- list(id = paste0("hold-", id), kind = "generate", dataset = "ADSL", node = id, started = as.numeric(Sys.time()), model = NULL)
  }
  started <- call_app(A, "/api/batch", "POST", list(dataset = "ADSL", nodes = as.list(ids), compare = TRUE))
  expect_equal(started$status, 200L)
  again <- call_app(A, "/api/batch", "POST", list(dataset = "ADSL", nodes = list("ADSL.NOPE"), compare = FALSE))
  expect_equal(again$status, 409L)
  expect_match(resp_json(again)$error, "already running", fixed = TRUE)
  wait_batch(A, timeout = 30)
  body <- resp_json(call_app(A, "/api/state"))
  fields <- c("dataset", "total", "done", "current", "step", "cancelled", "finished", "results")
  expect_true(all(fields %in% names(body$batch)))
  expect_true(isTRUE(body$batch$finished))
  expect_false(isTRUE(body$batch$cancelled))
  expect_equal(body$batch$dataset, "ADSL")
  expect_equal(as.integer(body$batch$total), 3L)
  expect_equal(as.integer(body$batch$done), 3L)
  expect_null(body$batch$current)
  rows <- batch_rows(body$batch)
  expect_equal(row_nodes(body$batch), c("ADSL.USUBJID", "ADSL.NOPE", "ADSL.AGE2"))
  expect_true(all(vapply(rows, function(r) identical(r$message, "skipped: running") && identical(r$ok, FALSE), logical(1))))
  A$jobs <- list()
})

test_that("a batch runs in order, compares only a clean L2, and keeps going after a failure", {
  box <- isolate_batch_home()
  on.exit(box$restore(), add = TRUE)
  proj <- batch_project()
  A <- create_app(app_root, proj, 7985L, "batch-token", load_config())
  asked <- c("ADSL.AGE2", "ADSL.NOPE", "ADSL.AGE", "ADSL.USUBJID")
  res <- call_app(A, "/api/batch", "POST", list(dataset = "ADSL", nodes = as.list(asked), compare = TRUE))
  expect_equal(res$status, 200L)
  denied <- call_app(A, "/api/batch", "POST", list(dataset = "ADSL", nodes = list("ADSL.AGE"), compare = TRUE))
  expect_equal(denied$status, 409L)

  seen_parallel <- FALSE
  order_seen <- character()
  pump_until(function() {
    jobs <- Filter(function(j) j$kind %in% c("generate", "compare"), A$jobs)
    nodes <- unique(vapply(jobs, function(j) as.character(j$node %||% ""), ""))
    nodes <- nodes[nzchar(nodes)]
    if (length(nodes) > 1L) seen_parallel <<- TRUE
    cur <- A$batch$current %||% ""
    if (nzchar(cur) && (length(order_seen) == 0L || !identical(tail(order_seen, 1), cur))) order_seen <<- c(order_seen, cur)
    isTRUE(A$batch$finished)
  }, timeout = 240)
  expect_false(seen_parallel)
  b <- A$batch
  expect_true(isTRUE(b$finished))
  expect_false(isTRUE(b$cancelled))
  expect_equal(row_nodes(b), c("ADSL.USUBJID", "ADSL.NOPE", "ADSL.AGE", "ADSL.AGE2"))
  expect_equal(order_seen, row_nodes(b))
  rows <- batch_rows(b)
  by_id <- stats::setNames(rows, row_nodes(b))
  expect_equal(by_id[["ADSL.USUBJID"]]$level, "L3")
  expect_true(isTRUE(by_id[["ADSL.USUBJID"]]$ok))
  expect_true(file.exists(file.path(node_run_dir(A$project, "ADSL", "ADSL.USUBJID"), "compare.json")))
  expect_equal(by_id[["ADSL.NOPE"]]$level, "none")
  expect_false(isTRUE(by_id[["ADSL.NOPE"]]$ok))
  expect_false(file.exists(file.path(node_run_dir(A$project, "ADSL", "ADSL.NOPE"), "compare.json")))
  expect_equal(by_id[["ADSL.AGE"]]$level, "L3")
  expect_true(isTRUE(by_id[["ADSL.AGE"]]$ok))
  expect_equal(by_id[["ADSL.AGE2"]]$level, "L3")
  expect_true(isTRUE(by_id[["ADSL.AGE2"]]$ok))
  expect_true(file.exists(file.path(node_run_dir(A$project, "ADSL", "ADSL.AGE2"), "compare.json")))
  pub <- resp_json(call_app(A, "/api/state"))$batch
  expect_equal(as.integer(pub$done), 4L)
  expect_equal(as.integer(pub$total), 4L)
  expect_true(isTRUE(pub$finished))
})

test_that("cancel stops after the current node and compare can stay off", {
  box <- isolate_batch_home()
  on.exit(box$restore(), add = TRUE)
  proj <- batch_project()
  A <- create_app(app_root, proj, 7986L, "batch-token", load_config())
  res <- call_app(A, "/api/batch", "POST", list(
    dataset = "ADSL", nodes = list("ADSL.AGE", "ADSL.USUBJID"), compare = FALSE
  ))
  expect_equal(res$status, 200L)
  expect_false(isTRUE(resp_json(res)$finished))
  pump_until(function() nzchar(A$batch$current %||% "") || isTRUE(A$batch$finished), timeout = 60)
  expect_false(isTRUE(A$batch$finished))
  stopped <- call_app(A, "/api/batch/cancel", "POST", list())
  expect_equal(stopped$status, 200L)
  expect_true(isTRUE(resp_json(stopped)$cancelled))
  wait_batch(A, timeout = 180)
  rows <- batch_rows(A$batch)
  expect_true(isTRUE(A$batch$cancelled))
  expect_true(isTRUE(A$batch$finished))
  expect_lt(length(rows), 2L)
  expect_gte(length(rows), 1L)
  expect_equal(rows[[1]]$node, "ADSL.USUBJID")
  expect_equal(rows[[1]]$level, "L2")
  expect_true(isTRUE(rows[[1]]$ok))
  expect_equal(rows[[1]]$message, "Generated")
  expect_false(file.exists(file.path(node_run_dir(A$project, "ADSL", "ADSL.USUBJID"), "compare.json")))
  expect_equal(get_status(A$project, "ADSL", "ADSL.AGE")$level, "none")
})
