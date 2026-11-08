source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
if (!exists("open_project", mode = "function") || !exists("run_side_job", mode = "function")) {
  for (f in c("utils.R", "deps.R", "config.R", "dag.R", "spec.R", "project.R", "catalog.R", "server.R")) {
    sys.source(file.path(root, "R", f), envir = globalenv())
  }
}

write_l3 <- function(proj, dataset, ids) {
  dir.create(file.path(proj, ".dualdag", "state"), recursive = TRUE, showWarnings = FALSE)
  nodes <- setNames(lapply(ids, function(id) list(level = "L3")), ids)
  payload <- list(version = 1L, nodes = setNames(list(nodes), dataset))
  writeLines(as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")),
             file.path(proj, ".dualdag", "state", "state.json"))
}

adsl_csv <- function() {
  paste(c(
    "Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Unique Subject,Char,20,1,DM.USUBJID,copy",
    "ADSL,TRTSDT,Start,Char,20,,DM.RFSTDTC,DM.RFSTDTC",
    "ADSL,AGE,Age,Num,8,,DM.AGE,copy"
  ), collapse = "\n")
}

test_that("work grows from L3 columns and a 0-row result fails", {
  proj <- temp_project()
  file.copy(fixture_path("dm.xpt"), file.path(proj, "data", "dm.xpt"))
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  writeLines(adsl_csv(), file.path(proj, "spec", "variable.csv"))
  write_result(proj, "ADSL", "ADSL.USUBJID", "primary",
               data.frame(USUBJID = c("S1", "S2"), stringsAsFactors = FALSE))
  write_l3(proj, "ADSL", "ADSL.USUBJID")
  work <- assemble_work(proj, "ADSL", "primary")
  expect_equal(nrow(work), 2L)
  expect_equal(names(work), "USUBJID")

  write_result(proj, "ADSL", "ADSL.AGE", "primary",
               data.frame(USUBJID = c("S1", "S2"), AGE = c(40, 41)))
  write_l3(proj, "ADSL", c("ADSL.USUBJID", "ADSL.AGE"))
  work2 <- assemble_work(proj, "ADSL", "primary")
  expect_equal(nrow(work2), 2L)
  expect_true(all(c("USUBJID", "AGE") %in% names(work2)))
  held <- build_upstream(proj, "ADSL", "ADSL.AGE", "primary", NULL, "USUBJID")
  expect_false("AGE" %in% names(held))
  expect_equal(nrow(held), 2L)

  empty <- '
    result <- work[0, , drop = FALSE]
  '
  ran <- run_side(proj, "ADSL", "ADSL.TRTSDT", "primary", empty, timeout = 60)
  expect_false(ran$ok)
  expect_match(ran$error, "produced 0 rows", fixed = TRUE)
  log_txt <- paste(readLines(file.path(proj, ".dualdag", "runs", "ADSL", "TRTSDT", "primary", "log.txt"), warn = FALSE), collapse = "\n")
  expect_false(grepl("(?m)^> |^\\+ ", log_txt, perl = TRUE))
  expect_false(grepl("produced 0 rows", log_txt, fixed = TRUE))
  expect_false(grepl("wrote rows=", log_txt, fixed = TRUE))

  added <- '
    result <- data.frame(work[, keys, drop = FALSE], stringsAsFactors = FALSE)
    result$TRTSDT <- "2020-01-01"
    result
  '
  ran2 <- run_side(proj, "ADSL", "ADSL.TRTSDT", "primary", added, timeout = 60)
  expect_true(ran2$ok)
  expect_equal(nrow(readRDS(ran2$out_path)), 2L)
})

test_that("work keeps Date, POSIXct, and difftime columns and repairs old work on open", {
  proj <- temp_project()
  dir.create(file.path(proj, "spec"), recursive = TRUE, showWarnings = FALSE)
  writeLines(paste(c(
    "Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Subject,Char,20,1,DM.USUBJID,copy",
    "ADSL,TRTSDT,Start,Date,8,,DM.RFSTDTC,copy",
    "ADSL,DTM,Time,Datetime,8,,DM.DTM,copy",
    "ADSL,DUR,Duration,Num,8,,DM.DUR,copy"
  ), collapse = "\n"), file.path(proj, "spec", "variable.csv"))
  ids <- c("ADSL.USUBJID", "ADSL.TRTSDT", "ADSL.DTM", "ADSL.DUR")
  key <- c("S1", "S2")
  dates <- as.Date(c("2014-01-02", NA))
  times <- as.POSIXct(c("2014-01-02 08:30:00", "2014-01-03 09:45:00"), tz = "UTC")
  duration <- as.difftime(c(30, 45), units = "mins")
  write_result(proj, "ADSL", ids[[1]], "primary", data.frame(USUBJID = key))
  write_result(proj, "ADSL", ids[[2]], "primary", data.frame(USUBJID = key, TRTSDT = dates))
  write_result(proj, "ADSL", ids[[3]], "primary", data.frame(USUBJID = key, DTM = times))
  write_result(proj, "ADSL", ids[[4]], "primary", data.frame(USUBJID = key, DUR = duration))
  write_l3(proj, "ADSL", ids)
  expect_s3_class(read_side_result(proj, "ADSL", ids[[2]], "primary", "USUBJID")$TRTSDT, "Date")
  work <- assemble_work(proj, "ADSL", "primary")
  expect_s3_class(work$TRTSDT, "Date")
  expect_s3_class(work$DTM, "POSIXct")
  expect_identical(attr(work$DTM, "tzone"), "UTC")
  expect_s3_class(work$DUR, "difftime")
  expect_identical(as.character(work$TRTSDT), as.character(dates))
  refresh_work_side(proj, "ADSL", "primary")
  old <- readRDS(work_path(proj, "ADSL", "primary"))
  old$TRTSDT <- as.numeric(old$TRTSDT)
  saveRDS(old, work_path(proj, "ADSL", "primary"))
  open_project(proj)
  repaired <- readRDS(work_path(proj, "ADSL", "primary"))
  expect_s3_class(repaired$TRTSDT, "Date")
  expect_s3_class(repaired$DTM, "POSIXct")
  expect_s3_class(repaired$DUR, "difftime")
})

test_that("plain frames strip haven metadata but keep date and time classes", {
  d <- structure(as.Date("2020-01-01"), label = "visit date", format.sas = "DATE9.")
  t <- structure(as.POSIXct("2020-01-01 12:30:00", tz = "UTC"), label = "visit time")
  labelled <- haven::labelled(42, label = "age")
  df <- plain_frame(data.frame(D = d, T = t, F = factor("A"), N = labelled))
  expect_s3_class(df$D, "Date")
  expect_s3_class(df$T, "POSIXct")
  expect_identical(attr(df$T, "tzone"), "UTC")
  expect_identical(df$F, "A")
  expect_identical(df$N, 42)
  expect_null(attr(df$D, "label"))
  expect_null(attr(df$D, "format.sas"))
  expect_null(attr(df$T, "label"))
  expect_null(attr(df$N, "label"))
})

test_that("xpt dates survive loading and Date compare distinguishes numeric storage", {
  proj <- temp_project()
  dates <- as.Date(c("2014-01-02", "2014-01-03"))
  times <- as.POSIXct(c("2014-01-02 08:30:00", "2014-01-03 09:45:00"), tz = "UTC")
  haven::write_xpt(data.frame(USUBJID = c("S1", "S2"), TRTSDT = dates, DTM = times),
                   file.path(proj, "data", "dm.xpt"))
  src <- load_src(proj)
  expect_s3_class(src$DM$TRTSDT, "Date")
  expect_s3_class(src$DM$DTM, "POSIXct")
  expect_identical(attr(src$DM$DTM, "tzone"), "UTC")
  write_result(proj, "ADSL", "ADSL.TRTSDT", "primary",
               data.frame(USUBJID = c("S1", "S2"), TRTSDT = dates))
  write_result(proj, "ADSL", "ADSL.TRTSDT", "validation",
               data.frame(USUBJID = c("S1", "S2"), TRTSDT = dates))
  matched <- compare_node(proj, "ADSL", "ADSL.TRTSDT", "USUBJID")
  expect_true(matched$match)
  write_result(proj, "ADSL", "ADSL.TRTSDT", "validation",
               data.frame(USUBJID = c("S1", "S2"), TRTSDT = as.numeric(dates)))
  different <- compare_node(proj, "ADSL", "ADSL.TRTSDT", "USUBJID")
  expect_false(different$match)
  expect_match(different$summary, "TRTSDT types differ: Date vs double", fixed = TRUE)
})

test_that("opening a project downgrades an L3 node with an empty result", {
  proj <- temp_project()
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  writeLines(adsl_csv(), file.path(proj, "spec", "variable.csv"))
  p <- open_project(proj)
  save_graph(p, list(
    dataset = "ADSL", confirmed = TRUE, confirmed_hash = "x",
    nodes = list(
      list(id = "ADSL.USUBJID", var = "USUBJID", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.AGE", var = "AGE", paramcd = NULL, kind = "variable")
    ),
    edges = list(list(from = "ADSL.USUBJID", to = "ADSL.AGE", evidence = "key", origin = "user")),
    groups = list(), external = list(), unresolved = list()
  ))
  write_result(proj, "ADSL", "ADSL.USUBJID", "primary",
               data.frame(USUBJID = character(), stringsAsFactors = FALSE))
  write_text("result <- data.frame(USUBJID = character())\n",
             file.path(side_run_dir(proj, "ADSL", "ADSL.USUBJID", "primary"), "code.R"))
  write_result(proj, "ADSL", "ADSL.AGE", "primary",
               data.frame(USUBJID = "S1", AGE = 40))
  for (id in c("ADSL.USUBJID", "ADSL.AGE")) {
    st <- get_status(p, "ADSL", id)
    st$level <- "L3"
    st$problem <- NULL
    put_status(p, "ADSL", id, st)
  }
  again <- open_project(proj)
  st_key <- get_status(again, "ADSL", "ADSL.USUBJID")
  expect_equal(st_key$level, "none")
  expect_equal(st_key$problem$message, "produced 0 rows")
  st_age <- get_status(again, "ADSL", "ADSL.AGE")
  expect_equal(st_age$level, "L3")
  expect_true("ADSL.USUBJID" %in% st_age$upstream_changed)
  expect_true(file.exists(file.path(side_run_dir(proj, "ADSL", "ADSL.USUBJID", "primary"), "out.rds")))
  expect_true(file.exists(file.path(side_run_dir(proj, "ADSL", "ADSL.USUBJID", "primary"), "code.R")))
  expect_true(file.exists(file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "primary"), "out.rds")))
})

test_that("a non-key with 0-row work builds rows from the source domain", {
  proj <- temp_project()
  file.copy(fixture_path("dm.xpt"), file.path(proj, "data", "dm.xpt"))
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  writeLines(adsl_csv(), file.path(proj, "spec", "variable.csv"))
  work <- assemble_work(proj, "ADSL", "primary")
  expect_equal(nrow(work), 0L)
  expect_equal(names(work), "USUBJID")

  empty <- "result <- work[0, , drop = FALSE]\n"
  ran <- run_side(proj, "ADSL", "ADSL.AGE", "primary", empty, timeout = 60)
  expect_true(ran$ok)
  expect_false(grepl("produced 0 rows", ran$error %||% "", fixed = TRUE))
  expect_equal(nrow(readRDS(ran$out_path)), 0L)

  old_key <- Sys.getenv("DUALDAG_LLM_API_KEY")
  old_fake <- Sys.getenv("DUALDAG_FAKE_LLM")
  Sys.setenv(DUALDAG_LLM_API_KEY = "sk-test", DUALDAG_FAKE_LLM = "1")
  on.exit({
    if (nzchar(old_key)) Sys.setenv(DUALDAG_LLM_API_KEY = old_key) else Sys.unsetenv("DUALDAG_LLM_API_KEY")
    if (nzchar(old_fake)) Sys.setenv(DUALDAG_FAKE_LLM = old_fake) else Sys.unsetenv("DUALDAG_FAKE_LLM")
  }, add = TRUE)
  res <- run_side_job(proj, "ADSL", "ADSL.AGE", "primary", NULL, 60)
  expect_true(isTRUE(res$run$ok))
  expect_equal(nrow(readRDS(res$run$out_path)), 3L)
  logged <- latest_llm_log(proj, "ADSL.AGE", "primary", task = "generate")
  expect_match(logged$user, "LINEAGE_FRAME_ROWS: 0", fixed = TRUE)
  expect_match(logged$user, "LINEAGE_ROWS: 0", fixed = TRUE)
  expect_match(logged$system, "Every non-keys step starts with this data.frame already present", fixed = TRUE)
})

test_that("notes and the current code stay on their own side and the call is logged", {
  proj <- temp_project()
  file.copy(fixture_path("dm.xpt"), file.path(proj, "data", "dm.xpt"))
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  writeLines(adsl_csv(), file.path(proj, "spec", "variable.csv"))
  p <- open_project(proj)
  write_text("VALIDATION_CODE_SENTINEL\n",
             file.path(side_run_dir(proj, "ADSL", "ADSL.TRTSDT", "validation"), "code.R"))
  write_text("PRIMARY_CODE_SENTINEL\n",
             file.path(side_run_dir(proj, "ADSL", "ADSL.TRTSDT", "primary"), "code.R"))
  append_side_note(p, "ADSL", "ADSL.TRTSDT", "primary", "note one keep the date")
  append_side_note(p, "ADSL", "ADSL.TRTSDT", "primary", "note two use RFSTDTC")
  append_side_note(p, "ADSL", "ADSL.TRTSDT", "validation", "VALIDATION_NOTE only")
  old_key <- Sys.getenv("DUALDAG_LLM_API_KEY")
  old_fake <- Sys.getenv("DUALDAG_FAKE_LLM")
  Sys.setenv(DUALDAG_LLM_API_KEY = "sk-test-secret-key-should-not-leak", DUALDAG_FAKE_LLM = "1")
  on.exit({
    if (nzchar(old_key)) Sys.setenv(DUALDAG_LLM_API_KEY = old_key) else Sys.unsetenv("DUALDAG_LLM_API_KEY")
    if (nzchar(old_fake)) Sys.setenv(DUALDAG_FAKE_LLM = old_fake) else Sys.unsetenv("DUALDAG_FAKE_LLM")
  }, add = TRUE)
  res <- run_side_job(proj, "ADSL", "ADSL.TRTSDT", "primary", NULL, 60)
  expect_true(isTRUE(res$run$ok))
  expect_match(res$code, "^# note 1: note one keep the date")
  expect_match(res$code, "# note 2: note two use RFSTDTC", fixed = TRUE)
  expect_false(grepl("VALIDATION_NOTE", res$code, fixed = TRUE))
  expect_false(grepl("VALIDATION_CODE_SENTINEL", res$code, fixed = TRUE))
  note <- read_json(file.path(side_run_dir(proj, "ADSL", "ADSL.TRTSDT", "primary"), "code_note.json"))
  expect_equal(as.numeric(note$index), 2)
  expect_equal(note$text, "note two use RFSTDTC")
  logged <- latest_llm_log(proj, "ADSL.TRTSDT", "primary", task = "generate")
  expect_false(is.null(logged))
  blob <- paste(logged$system, logged$user, logged$reply, collapse = "\n")
  expect_false(grepl("sk-test-secret-key-should-not-leak", blob, fixed = TRUE))
  expect_match(logged$user, "PRIMARY_CODE_SENTINEL", fixed = TRUE)
  expect_match(logged$user, "note one keep the date", fixed = TRUE)
  expect_match(logged$user, "note two use RFSTDTC", fixed = TRUE)
  expect_false(grepl("VALIDATION_CODE_SENTINEL", logged$user, fixed = TRUE))
  expect_false(grepl("VALIDATION_NOTE", logged$user, fixed = TRUE))
  expect_match(logged$user, "LINEAGE_ROWS:", fixed = TRUE)
  raw <- paste(list.files(file.path(proj, ".dualdag", "llm_log"), full.names = TRUE) |>
                 lapply(readLines, warn = FALSE) |> unlist(), collapse = "\n")
  expect_false(grepl("sk-test-secret-key-should-not-leak", raw, fixed = TRUE))
})

test_that("the page reads the token on every api call", {
  src <- paste(readLines(file.path(root, "www", "app.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("const TOKEN", src, fixed = TRUE))
  expect_true(grepl("token()", src, fixed = TRUE))
  expect_match(src, "X-Dualdag-Token\": token()", fixed = TRUE)
  expect_match(src, "location.reload()", fixed = TRUE)
  expect_match(src, "token-banner", fixed = TRUE)
  expect_match(src, "grok_cli", fixed = TRUE)
  expect_false(grepl('btn("dl-log"', src, fixed = TRUE))
  expect_false(grepl("Download log", src, fixed = TRUE))
})
