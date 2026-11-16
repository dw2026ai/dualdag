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

advs_whole_project <- function() {
  proj <- tempfile("dualdag-advs-whole-")
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  dir.create(file.path(proj, "data"), recursive = TRUE)
  writeLines(c(VAR_HEADER,
    "ADVS,USUBJID,Subject,Char,20,1,VS.USUBJID,Copy subject.",
    "ADVS,PARAMCD,Parameter,Char,8,2,VS.VSTESTCD,Copy parameter.",
    "ADVS,AVAL,Value,Num,8,,VS.VSSTRESN,Copy the collected value."),
    file.path(proj, "spec", "variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation",
    "ADVS,WEIGHT,Weight,VS.VSTESTCD,Carry-over source rows.",
    "ADVS,HEIGHT,Height,VS.VSTESTCD,Carry-over source rows.",
    "ADVS,BMI,Body mass index,ADVS.AVAL@WEIGHT and ADVS.AVAL@HEIGHT,Weight divided by height squared.",
    "ADVS,OTHER,Other derived layer,ADVS.AVAL@BMI,Copy the prior derived layer."),
    file.path(proj, "spec", "parameter.csv"))
  file.copy(file.path(app_root, "tests", "fixtures", "vs.xpt"), file.path(proj, "data", "vs.xpt"))
  p <- open_project(proj); spec <- read_project_spec(p)
  g <- graph_skeleton(spec, "ADVS", carry_members = c("WEIGHT", "HEIGHT"))
  g$generated_parameters <- list("CARRY_OVER", "BMI", "OTHER")
  g$edges <- list(
    list(from = "ADVS.RECORDS@CARRY_OVER", to = "ADVS.AVAL@CARRY_OVER", evidence = "records before values", origin = "user"),
    list(from = "ADVS.RECORDS@BMI", to = "ADVS.AVAL@BMI", evidence = "records before values", origin = "user"),
    list(from = "ADVS.RECORDS@OTHER", to = "ADVS.AVAL@OTHER", evidence = "records before values", origin = "user"))
  save_graph(p, g); note_graph_nodes(p, "ADVS", g)
  pg <- list(dataset = "ADVS", nodes = param_defined_nodes(spec, "ADVS"),
    carry_members = list("WEIGHT", "HEIGHT"),
    edges = list(
      list(from = "WEIGHT", to = "BMI", uses = list(list(from_var = "AVAL", to_var = "AVAL"))),
      list(from = "HEIGHT", to = "BMI", uses = list(list(from_var = "AVAL", to_var = "AVAL"))),
      list(from = "BMI", to = "OTHER", uses = list(list(from_var = "AVAL", to_var = "AVAL")))),
    classification = list(parameters = list(
      list(paramcd = "WEIGHT", classification = "carry_over"),
      list(paramcd = "HEIGHT", classification = "carry_over"),
      list(paramcd = "BMI", classification = "derived"),
      list(paramcd = "OTHER", classification = "derived"))))
  param_graph_write(p, pg)
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

test_that("whole dataset stops on the first failed node and leaves later nodes untouched", {
  box <- isolate_batch_home(); on.exit(box$restore(), add = TRUE)
  proj <- batch_project()
  A <- create_app(app_root, proj, 7987L, "batch-token", load_config())
  res <- call_app(A, "/api/batch", "POST", list(dataset = "ADSL", whole_dataset = TRUE))
  expect_equal(res$status, 200L)
  wait_batch(A, timeout = 240)
  b <- A$batch
  expect_true(isTRUE(b$finished))
  expect_true(isTRUE(b$stop_on_error))
  expect_equal(b$stopped_at$node, "ADSL.NOPE")
  expect_match(b$stopped_at$reason, "could not derive", ignore.case = TRUE)
  expect_equal(row_nodes(b), c("ADSL.USUBJID", "ADSL.NOPE"))
  expect_false(file.exists(file.path(node_run_dir(A$project, "ADSL", "ADSL.AGE"), "compare.json")))
  expect_null(b$fullrun)
})

test_that("whole dataset generates in dependency order then runs both Full Runs", {
  box <- isolate_batch_home(); on.exit(box$restore(), add = TRUE)
  proj <- batch_project()
  p <- open_project(proj)
  spec <- read_project_spec(p)
  writeLines(spec_csv(), file.path(proj, "spec", "variable.csv"))
  # Remove the intentionally failing fixture node and retain a real dependency chain.
  writeLines(paste(c(VAR_HEADER,
    "ADSL,USUBJID,Unique Subject,Char,20,1,DM.USUBJID,copy",
    "ADSL,AGE,Age,Num,8,,DM.AGE,copy",
    "ADSL,AGE2,Age plus one,Num,8,,,AGE + 1"), collapse = "\n"), file.path(proj, "spec", "variable.csv"))
  g <- load_graph(p, "ADSL"); g$nodes <- Filter(function(n) n$id != "ADSL.NOPE", g$nodes)
  g$edges <- list(list(from = "ADSL.AGE", to = "ADSL.AGE2", evidence = "AGE2 = AGE + 1", origin = "user"))
  save_graph(p, g); note_graph_nodes(p, "ADSL", g)
  A <- create_app(app_root, proj, 7988L, "batch-token", load_config())
  res <- call_app(A, "/api/batch", "POST", list(dataset = "ADSL", whole_dataset = TRUE))
  expect_equal(res$status, 200L)
  pump_until(function() is.list(A$batch$fullrun) && identical(A$batch$fullrun$status, "running"), timeout = 240)
  during <- resp_json(call_app(A, "/api/state"))$batch
  expect_false(isTRUE(during$finished))
  expect_equal(during$step, "fullrun")
  expect_true(isTRUE(during$whole_dataset))
  pump_until(function() isTRUE(A$batch$finished) && !is.null(A$batch$fullrun) && A$batch$fullrun$status != "running", timeout = 300)
  expect_equal(row_nodes(A$batch), c("ADSL.USUBJID", "ADSL.AGE", "ADSL.AGE2"))
  expect_true(all(vapply(batch_rows(A$batch), function(r) isTRUE(r$ok), TRUE)))
  expect_equal(A$batch$fullrun$status, "complete", info = paste(capture.output(str(A$batch$fullrun)), collapse = "\n"))
  polled <- resp_json(call_app(A, "/api/state"))$batch
  expect_true(isTRUE(polled$finished))
  expect_equal(polled$fullrun$status, "complete")
  expect_true(isTRUE(polled$fullrun$primary$ok))
  expect_true(isTRUE(polled$fullrun$validation$ok))
  expect_true(file.exists(file.path(fullrun_dir(A$project, "ADSL", "primary"), "run.json")))
  expect_true(file.exists(file.path(fullrun_dir(A$project, "ADSL", "validation"), "run.json")))
})

test_that("whole dataset guard maps generated carry-over identity and selects grouped nodes once", {
  box <- isolate_batch_home(); on.exit(box$restore(), add = TRUE)
  proj <- tempfile("wei31-carry-guard-"); dir.create(file.path(proj, "spec"), recursive = TRUE)
  writeLines(c(VAR_HEADER,
    "ADVS,USUBJID,Subject,Char,20,1,,",
    "ADVS,PARAMCD,Parameter,Char,8,2,,",
    "ADVS,AVAL,Value,Num,8,,,"), file.path(proj, "spec", "variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation",
    "ADVS,WEIGHT,Weight,,", "ADVS,HEIGHT,Height,,", "ADVS,BMI,Body mass index,,"),
    file.path(proj, "spec", "parameter.csv"))
  p <- open_project(proj); spec <- read_project_spec(p)
  g <- graph_skeleton(spec, "ADVS", carry_members = c("WEIGHT", "HEIGHT"))
  g$generated_parameters <- list("CARRY_OVER")
  save_graph(p, g); note_graph_nodes(p, "ADVS", g)
  ids <- dataset_node_ids(p, spec, "ADVS")
  ordered <- batch_order_for(p, "ADVS", ids)
  expect_length(ids, length(unique(ids)))
  expect_length(ordered, length(unique(ordered)))
  expect_equal(sum(vapply(g$nodes, function(n) identical(n$paramcd, "CARRY_OVER") && n$kind == "records", TRUE)), 1L)
  expect_true("ADVS.AVAL@CARRY_OVER" %in% ordered)
  expect_false(any(grepl("@(WEIGHT|HEIGHT)$", ordered)))
  expect_equal(batch_missing_variable_dags(g, spec, "ADVS"), "BMI")
  A <- create_app(app_root, proj, 7989L, "batch-token", load_config())
  res <- call_app(A, "/api/batch", "POST", list(dataset = "ADVS", whole_dataset = TRUE))
  expect_equal(res$status, 409L)
  expect_match(resp_json(res)$error, "BMI", fixed = TRUE)
  expect_false(batch_running(A))
})

test_that("ADVS whole dataset runs carry-over and derived layers in parameter order then Full Runs both sides", {
  box <- isolate_batch_home(); on.exit(box$restore(), add = TRUE)
  proj <- advs_whole_project()
  A <- create_app(app_root, proj, 7990L, "batch-token", load_config())
  started <- call_app(A, "/api/batch", "POST", list(dataset = "ADVS", whole_dataset = TRUE))
  expect_equal(started$status, 200L)
  expected <- c("ADVS.RECORDS@CARRY_OVER", "ADVS.AVAL@CARRY_OVER",
    "ADVS.RECORDS@BMI", "ADVS.AVAL@BMI", "ADVS.RECORDS@OTHER", "ADVS.AVAL@OTHER")
  expect_equal(A$batch$order, expected)
  pump_until(function() is.list(A$batch$fullrun) && identical(A$batch$fullrun$status, "running"), timeout = 300)
  during <- resp_json(call_app(A, "/api/state"))$batch
  expect_false(isTRUE(during$finished))
  expect_equal(during$step, "fullrun")
  pump_until(function() isTRUE(A$batch$finished), timeout = 300)
  expect_equal(row_nodes(A$batch), expected)
  expect_true(all(vapply(batch_rows(A$batch), function(r) isTRUE(r$ok) && identical(r$level, "L3"), TRUE)),
    info = paste(capture.output(str(batch_rows(A$batch))), collapse = "\n"))
  expect_equal(A$batch$fullrun$status, "complete", info = paste(capture.output(str(A$batch$fullrun)), collapse = "\n"))
  final <- resp_json(call_app(A, "/api/state"))$batch
  expect_true(isTRUE(final$finished))
  expect_true(isTRUE(final$fullrun$primary$ok))
  expect_true(isTRUE(final$fullrun$validation$ok))
  expect_gt(as.integer(final$fullrun$primary$rows), 0L)
  expect_gt(as.integer(final$fullrun$validation$rows), 0L)
  expect_true(file.exists(file.path(fullrun_dir(A$project, "ADVS", "primary"), "result.rds")))
  expect_true(file.exists(file.path(fullrun_dir(A$project, "ADVS", "validation"), "result.rds")))
})

test_that("ADVS L2 compare problem in a middle derived layer stops before later layer nodes", {
  box <- isolate_batch_home(); on.exit(box$restore(), add = TRUE)
  proj <- advs_whole_project()
  real_start_compare <- start_compare
  assign("start_compare", function(A, ds, id) {
    if (!identical(id, "ADVS.AVAL@BMI")) return(real_start_compare(A, ds, id))
    st <- get_status(A$project, ds, id)
    st$level <- "L2"
    st$problem <- list(stage = "compare", message = "Fake model compare mismatch at ADVS.AVAL@BMI")
    put_status(A$project, ds, id, st)
    promises::promise(function(resolve, reject) resolve(json_resp(list(status = st, compare = list(match = FALSE)))))
  }, envir = .GlobalEnv)
  on.exit(assign("start_compare", real_start_compare, envir = .GlobalEnv), add = TRUE)
  A <- create_app(app_root, proj, 7991L, "batch-token", load_config())
  started <- call_app(A, "/api/batch", "POST", list(dataset = "ADVS", whole_dataset = TRUE))
  expect_equal(started$status, 200L)
  wait_batch(A, timeout = 300)
  expected_prefix <- c("ADVS.RECORDS@CARRY_OVER", "ADVS.AVAL@CARRY_OVER",
    "ADVS.RECORDS@BMI", "ADVS.AVAL@BMI")
  expect_equal(row_nodes(A$batch), expected_prefix)
  expect_equal(A$batch$stopped_at$node, "ADVS.AVAL@BMI")
  expect_equal(A$batch$stopped_at$reason, "Fake model compare mismatch at ADVS.AVAL@BMI")
  failed <- tail(batch_rows(A$batch), 1)[[1]]
  expect_equal(failed$level, "L2")
  expect_false(isTRUE(failed$ok))
  expect_true(isTRUE(failed$compare_ok))
  expect_equal(get_status(A$project, "ADVS", "ADVS.AVAL@BMI")$problem$message,
    "Fake model compare mismatch at ADVS.AVAL@BMI")
  for (id in c("ADVS.RECORDS@OTHER", "ADVS.AVAL@OTHER")) {
    expect_equal(get_status(A$project, "ADVS", id)$level, "none")
    expect_false(file.exists(file.path(node_run_dir(A$project, "ADVS", id), "compare.json")))
  }
  expect_null(A$batch$fullrun)
})
