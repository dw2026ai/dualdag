app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
if (!exists("open_project", mode = "function", inherits = TRUE)) {
  for (f in c("utils.R", "deps.R", "config.R", "spec.R", "project.R", "server.R"))
    sys.source(file.path(app_root, "R", f), envir = globalenv())
}

VAR_HEADER <- "Dataset,Variable,Label,Type,Length,Key,Source,Derivation"

adsl_csv <- function(deriv = "copy") {
  paste(c(VAR_HEADER, paste0("adsl,usubjid,Subject,char,20,1,DM.USUBJID,", deriv)), collapse = "\n")
}

tiny_xpt <- function(age) {
  df <- data.frame(USUBJID = "01", AGE = age, stringsAsFactors = FALSE)
  f <- tempfile(fileext = ".xpt")
  haven::write_xpt(df, f, version = 5, name = "DM")
  readBin(f, "raw", file.info(f)$size)
}

test_that("open_project creates the folder layout", {
  dir <- tempfile("dualdag-proj-")
  p <- open_project(dir)
  for (sub in c("spec", "data", "output", file.path(".dualdag", "state"), file.path(".dualdag", "runs")))
    expect_true(dir.exists(file.path(p$dir, sub)))
  expect_true(file.exists(file.path(p$state_dir, "state.json")))
})

test_that("uploading a spec and an xpt survives a new open_project", {
  dir <- tempfile("dualdag-proj-")
  p <- open_project(dir)
  sp <- project_upload_spec(p, charToRaw(adsl_csv()))
  expect_equal(sp$variables$Variable, "USUBJID")
  info <- project_upload_data(p, "DM.XPT", tiny_xpt(40))
  expect_equal(info[[1]]$domain, "DM")
  expect_true(file.exists(file.path(dir, "spec", "variable.csv")))
  expect_true(file.exists(file.path(dir, "data", "dm.xpt")))

  p2 <- open_project(dir)
  sp2 <- read_project_spec(p2)
  expect_equal(sp2$variables$Dataset, "ADSL")
  expect_equal(sp2$variables$Variable, "USUBJID")
  expect_equal(list_data_info(p2)[[1]]$domain, "DM")
  expect_equal(get_status(p2, "ADSL", "ADSL.USUBJID")$level, "none")
})

test_that("legacy L1 loads as Not generated without changing other status data or programs", {
  dir <- tempfile("dualdag-legacy-level-")
  p <- open_project(dir)
  project_upload_spec(p, charToRaw(paste0(adsl_csv(), "\n")))
  st <- blank_status()
  st$level <- "L1"
  st$problem <- list(stage = "validate", message = "old message")
  st$upstream_changed <- "ADSL.AGE"
  st$sides$primary <- list(ok = TRUE, error = NULL, updated = "p-time")
  st$sides$validation <- list(ok = FALSE, error = "kept", updated = "v-time")
  st$custom <- "preserved"
  p$state$nodes$ADSL <- list("ADSL.USUBJID" = st)
  write_state(p)
  run <- node_run_dir(p, "ADSL", "ADSL.USUBJID")
  for (side in c("primary", "validation")) {
    dir.create(file.path(run, side), recursive = TRUE, showWarnings = FALSE)
    writeLines(paste(side, "code"), file.path(run, side, "code.R"))
  }

  reopened <- open_project(dir)
  got <- get_status(reopened, "ADSL", "ADSL.USUBJID")
  expect_identical(got$level, "none")
  expect_identical(got$problem, st$problem)
  expect_identical(got$upstream_changed, st$upstream_changed)
  expect_identical(got$sides, st$sides)
  expect_identical(got$custom, "preserved")
  expect_identical(readLines(file.path(run, "primary", "code.R")), "primary code")
  expect_identical(readLines(file.path(run, "validation", "code.R")), "validation code")
})

test_that("an external spec edit resets the changed node, and a changed xpt drops L2/L3 to Not generated", {
  dir <- tempfile("dualdag-proj-")
  p <- open_project(dir)
  project_upload_spec(p, charToRaw(adsl_csv("copy")))
  project_upload_data(p, "dm.xpt", tiny_xpt(40))
  st <- get_status(p, "ADSL", "ADSL.USUBJID")
  st$level <- "L3"
  put_status(p, "ADSL", "ADSL.USUBJID", st)

  project_upload_data(p, "dm.xpt", tiny_xpt(41))
  expect_equal(get_status(p, "ADSL", "ADSL.USUBJID")$level, "none")

  st <- get_status(p, "ADSL", "ADSL.USUBJID")
  st$level <- "L3"
  put_status(p, "ADSL", "ADSL.USUBJID", st)
  writeLines(adsl_csv("renamed"), file.path(dir, "spec", "variable.csv"))
  p2 <- open_project(dir)
  expect_equal(get_status(p2, "ADSL", "ADSL.USUBJID")$level, "none")
})

test_that("a spec-row change resets that node and only flags descendants", {
  dir <- tempfile("dualdag-proj-")
  p <- open_project(dir)
  csv <- paste(c(
    VAR_HEADER,
    "adsl,usubjid,Subject,char,20,1,DM.USUBJID,copy",
    "adsl,age,Age,num,8,,DM.AGE,copy"
  ), collapse = "\n")
  project_upload_spec(p, charToRaw(csv))
  save_graph(p, list(
    dataset = "ADSL",
    nodes = list(
      list(id = "ADSL.USUBJID", var = "USUBJID", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.AGE", var = "AGE", paramcd = NULL, kind = "variable")
    ),
    edges = list(list(from = "ADSL.USUBJID", to = "ADSL.AGE", evidence = "key", origin = "user")),
    groups = list(), external = list(), unresolved = list()
  ))
  for (id in c("ADSL.USUBJID", "ADSL.AGE")) {
    st <- get_status(p, "ADSL", id)
    st$level <- "L3"
    put_status(p, "ADSL", id, st)
  }
  changed <- paste(c(
    VAR_HEADER,
    "adsl,usubjid,Subject,char,20,1,DM.USUBJID,renamed",
    "adsl,age,Age,num,8,,DM.AGE,copy"
  ), collapse = "\n")
  writeLines(changed, file.path(dir, "spec", "variable.csv"))
  p2 <- open_project(dir)
  expect_equal(get_status(p2, "ADSL", "ADSL.USUBJID")$level, "none")
  age <- get_status(p2, "ADSL", "ADSL.AGE")
  expect_equal(age$level, "L3")
  expect_true("ADSL.USUBJID" %in% age$upstream_changed)
})

test_that("editing one parameter spec row keeps other RECORDS Keys intact", {
  dir <- tempfile("dualdag-records-fingerprint-")
  p <- open_project(dir)
  dir.create(file.path(dir, "spec"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    "Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADVS,USUBJID,Subject,Char,20,1,DM.USUBJID,key",
    "ADVS,PARAMCD,Parameter,Char,8,2,VS.VSTESTCD,key",
    "ADVS,AVAL,Value,Num,8,,VS.VSSTRESN,value"
  ), file.path(dir, "spec", "variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation",
    "ADVS,P,Original P,VS.P,derive P", "ADVS,Q,Original Q,VS.Q,derive Q"),
    file.path(dir, "spec", "parameter.csv"))
  spec <- read_project_spec(p)
  p$state$files <- list(variable = file_md5(file.path(dir, "spec", "variable.csv")),
    parameter = file_md5(file.path(dir, "spec", "parameter.csv")))
  save_graph(p, list(dataset = "ADVS", nodes = list(
    list(id = "ADVS.RECORDS@P", var = "RECORDS", paramcd = "P", kind = "records"),
    list(id = "ADVS.RECORDS@Q", var = "RECORDS", paramcd = "Q", kind = "records")),
    edges = list(), groups = list(), external = list(), unresolved = list()))
  for (id in c("ADVS.RECORDS@P", "ADVS.RECORDS@Q")) {
    st <- get_status(p, "ADVS", id); st$level <- "L3"; put_status(p, "ADVS", id, st)
    run <- file.path(node_run_dir(p, "ADVS", id), "primary"); dir.create(run, recursive = TRUE)
    writeLines(paste("program", id), file.path(run, "code.R"))
  }
  rebuild_row_hash(p, spec)
  write_state(p)
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation",
    "ADVS,P,Edited P,VS.P,derive P", "ADVS,Q,Original Q,VS.Q,derive Q"),
    file.path(dir, "spec", "parameter.csv"))
  p2 <- open_project(dir)
  expect_identical(get_status(p2, "ADVS", "ADVS.RECORDS@P")$level, "none")
  expect_identical(get_status(p2, "ADVS", "ADVS.RECORDS@Q")$level, "L3")
  expect_identical(readLines(file.path(node_run_dir(p2, "ADVS", "ADVS.RECORDS@P"), "primary", "code.R")),
    "program ADVS.RECORDS@P")
  expect_identical(readLines(file.path(node_run_dir(p2, "ADVS", "ADVS.RECORDS@Q"), "primary", "code.R")),
    "program ADVS.RECORDS@Q")
})

test_that("missing legacy RECORDS fingerprint preserves its results", {
  dir <- tempfile("dualdag-records-legacy-")
  p <- open_project(dir)
  dir.create(file.path(dir, "spec"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADVS,USUBJID,Subject,Char,20,1,DM.USUBJID,key",
    "ADVS,PARAMCD,Parameter,Char,8,2,VS.VSTESTCD,key"), file.path(dir, "spec", "variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation", "ADVS,P,Parameter P,VS.P,derive"),
    file.path(dir, "spec", "parameter.csv"))
  save_graph(p, list(dataset = "ADVS", nodes = list(list(id = "ADVS.RECORDS@P", var = "RECORDS", paramcd = "P", kind = "records")),
    edges = list(), groups = list(), external = list(), unresolved = list()))
  spec <- read_project_spec(p); rebuild_row_hash(p, spec)
  p$state$files <- list(variable = file_md5(file.path(dir, "spec", "variable.csv")),
    parameter = file_md5(file.path(dir, "spec", "parameter.csv")))
  p$state$row_hash[["ADVS.RECORDS@P"]] <- NULL
  st <- get_status(p, "ADVS", "ADVS.RECORDS@P"); st$level <- "L3"; put_status(p, "ADVS", "ADVS.RECORDS@P", st)
  run <- file.path(node_run_dir(p, "ADVS", "ADVS.RECORDS@P"), "primary"); dir.create(run, recursive = TRUE)
  writeLines("legacy program", file.path(run, "code.R"))
  write_state(p)
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation", "ADVS,P,Parameter P,VS.P,derive"),
    file.path(dir, "spec", "parameter.csv"))
  p2 <- open_project(dir)
  expect_identical(get_status(p2, "ADVS", "ADVS.RECORDS@P")$level, "L3")
  expect_identical(readLines(file.path(run, "code.R")), "legacy program")
})

test_that("re-uploading the spec deletes graphs, logs, and runs", {
  dir <- tempfile("dualdag-proj-")
  p <- open_project(dir)
  project_upload_spec(p, charToRaw(adsl_csv()))
  writeLines("{}", graph_path(p, "ADSL"))
  writeLines("{}", log_path(p, "ADSL"))
  run <- node_run_dir(p, "ADSL", "ADSL.USUBJID")
  dir.create(file.path(run, "primary"), recursive = TRUE)
  writeLines("1", file.path(run, "primary", "code.R"))
  project_upload_spec(p, charToRaw(adsl_csv()))
  expect_false(file.exists(graph_path(p, "ADSL")))
  expect_false(file.exists(log_path(p, "ADSL")))
  expect_false(file.exists(file.path(run, "primary", "code.R")))
})

test_that("safe_path rejects paths that leave the project", {
  dir <- tempfile("dualdag-root-")
  dir.create(dir)
  expect_error(safe_path(dir, "../secret"), "\\.\\.")
  expect_error(safe_path(dir, "/etc/passwd"), "Absolute")
})
