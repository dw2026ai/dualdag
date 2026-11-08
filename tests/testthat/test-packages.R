# Project R package list: storage, run header, guard, prompts, Settings API.

source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
for (f in c("utils.R", "deps.R", "config.R", "dag.R", "spec.R", "project.R",
            "catalog.R", "sides.R", "viewer.R", "inspect.R", "server.R")) {
  sys.source(file.path(app_root, "R", f), envir = globalenv())
}

pkg_spec <- function() {
  spec_of(V("ADSL", "USUBJID", type = "Char", length = 20, key = 1,
            source = "DM.USUBJID", derivation = "Copy DM.USUBJID", row = 2))
}

pkg_project <- function() {
  proj <- temp_project()
  file.copy(fixture_path("dm.xpt"), file.path(proj, "data", "dm.xpt"))
  dir.create(file.path(proj, "spec"), recursive = TRUE, showWarnings = FALSE)
  writeLines(paste(c(
    "Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Unique Subject,Char,20,1,DM.USUBJID,Copy DM.USUBJID"
  ), collapse = "\n"), file.path(proj, "spec", "variable.csv"))
  proj
}

plain_code <- 'result <- data.frame(USUBJID = "S1", stringsAsFactors = FALSE)\n'
filter_code <- 'result <- dplyr::filter(data.frame(USUBJID = "S1", stringsAsFactors = FALSE), USUBJID == "S1")\n'

side_log <- function(proj, side = "primary") {
  path <- file.path(proj, ".dualdag", "runs", "ADSL", "USUBJID", side, "log.txt")
  if (!file.exists(path)) return("")
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

call_app <- function(A, path, method = "GET", body = NULL, token = A$token) {
  raw <- if (is.null(body)) raw() else charToRaw(as.character(to_json(body)))
  req <- list(
    PATH_INFO = path, REQUEST_METHOD = method, QUERY_STRING = "",
    HTTP_HOST = sprintf("127.0.0.1:%d", A$port),
    HTTP_X_DUALDAG_TOKEN = if (is.null(token)) "" else token,
    CONTENT_TYPE = if (identical(method, "POST")) "application/json" else "",
    rook.input = list(read = function() raw)
  )
  make_handlers(A)$call(req)
}

resp_json <- function(res) from_json(res$body)

chr_field <- function(x) {
  if (is.null(x)) return(character())
  as.character(unlist(x, use.names = FALSE))
}

isolate_pkg_home <- function() {
  old <- Sys.getenv(c("DUALDAG_HOME", "DUALDAG_LLM_API_KEY", "DUALDAG_FAKE_LLM"))
  home <- tempfile("dualdag-home-")
  dir.create(home)
  Sys.setenv(DUALDAG_HOME = home, DUALDAG_LLM_API_KEY = "", DUALDAG_FAKE_LLM = "1")
  list(restore = function() {
    for (nm in names(old)) {
      if (identical(nm, "DUALDAG_HOME")) restore_test_env(nm, old[[nm]])
      else if (!nzchar(old[[nm]])) Sys.unsetenv(nm) else {
        args <- list(old[[nm]]); names(args) <- nm; do.call(Sys.setenv, args)
      }
    }
  })
}

test_that("a missing file defaults to dplyr and an empty list stays empty", {
  proj <- pkg_project()
  expect_identical(side_r_packages(NULL, "primary"), "dplyr")
  expect_identical(side_r_packages(proj, "validation"), "dplyr")
  saved <- r_packages_save(proj, list(r_packages = list(), override = FALSE))
  expect_identical(read_r_packages(proj)$r_packages, character())
  expect_identical(side_r_packages(proj, "primary"), character())
  expect_identical(chr_field(saved$r_packages), character())
  expect_false(isTRUE(saved$override))
  stripped <- r_packages_save(proj, list(r_packages = list("stats", "dplyr", "base", "utils", "methods")))
  expect_identical(chr_field(stripped$r_packages), "dplyr")
})

test_that("package references are checked for installation and base stays available", {
  expect_null(code_disallowed_packages("result <- mean(1:3)", character()))
  expect_null(code_disallowed_packages("result <- stats::setNames(1, \"a\")", character()))
  expect_null(code_disallowed_packages("library(dplyr)\nresult <- 1", "dplyr"))
  expect_null(code_disallowed_packages("# library(lubridate)\nresult <- 1", character()))
  expect_identical(code_required_packages(filter_code), "dplyr")
  expect_identical(code_required_packages('requireNamespace("foo", quietly = TRUE)'), "foo")
  expect_identical(code_required_packages("foo:::hidden()"), "foo")
  expect_identical(code_required_packages("require(pkg = \"jsonlite\")"), "jsonlite")
  expect_null(code_disallowed_packages("library(foo)\nresult <- 1", character()))
  expect_match(
    code_disallowed_packages("library(paste0(\"foo\"))", character()),
    "must name a package literally", fixed = TRUE
  )
  expect_identical(code_required_packages("dplyr::filter(x)\nfoo::bar()"), c("dplyr", "foo"))
  expect_equal(missing_r_packages("dualdagnosuchpkg"),
    paste0('This program needs R package dualdagnosuchpkg, which is not installed. Install it in the R at ',
           R.home(), ': install.packages("dualdagnosuchpkg"). Then press Re-run.'))
  expect_equal(missing_r_packages(c("dualdagnosuchpkg", "dualdagotherpkg")),
    paste0('This program needs R packages dualdagnosuchpkg, dualdagotherpkg, which are not installed. Install them in the R at ',
           R.home(), ': install.packages(c("dualdagnosuchpkg", "dualdagotherpkg")). Then press Re-run.'))
  expect_equal(missing_r_packages("dplyr"), NULL)
  expect_null(missing_r_packages(c("dplyr", "stats")))
  expect_equal(missing_r_package_from_error("Error: there is no package called 'dualdagnosuchpkg'"), "dualdagnosuchpkg")
})

test_that("a side run writes the package header without source echo", {
  proj <- pkg_project()
  spec <- pkg_spec()
  g <- make_graph("ADSL", list(gn("ADSL.USUBJID")))
  ran <- run_side(proj, "ADSL", "ADSL.USUBJID", "primary", plain_code, timeout = 60, graph = g, spec = spec)
  expect_true(ran$ok)
  log_txt <- side_log(proj)
  ver <- as.character(utils::packageVersion("dplyr"))
  expect_match(log_txt, paste0("# R ", as.character(getRversion()), "; packages: dplyr ", ver), fixed = TRUE)
  expect_false(grepl("(?m)^> |^\\+ ", log_txt, perl = TRUE))
  expect_false(grepl("Attaching package", log_txt, fixed = TRUE))
  expect_false(grepl("produced 0 rows", log_txt, fixed = TRUE))

  r_packages_save(proj, list(r_packages = list(), override = FALSE))
  empty <- run_side(proj, "ADSL", "ADSL.USUBJID", "primary", plain_code, timeout = 60, graph = g, spec = spec)
  expect_true(empty$ok)
  expect_match(side_log(proj), paste0("# R ", as.character(getRversion()), "; packages: none"), fixed = TRUE)
})

test_that("installed non-attached packages run on both sides", {
  proj <- pkg_project()
  spec <- pkg_spec()
  g <- make_graph("ADSL", list(gn("ADSL.USUBJID")))
  r_packages_save(proj, list(r_packages = list(), override = FALSE))
  code <- 'library(tools)\nresult <- data.frame(USUBJID = tools::file_path_sans_ext("S1.xpt"), stringsAsFactors = FALSE)\n'
  for (side in c("primary", "validation")) {
    ran <- run_side(proj, "ADSL", "ADSL.USUBJID", side, code, timeout = 60, graph = g, spec = spec)
    expect_true(ran$ok, info = ran$error %||% "")
    expect_identical(readRDS(ran$out_path)$USUBJID, "S1")
  }
})

test_that("missing code packages are stopped before execution and runtime errors are explained", {
  proj <- pkg_project()
  spec <- pkg_spec()
  g <- make_graph("ADSL", list(gn("ADSL.USUBJID")))
  r_packages_save(proj, list(r_packages = list(), override = FALSE))
  expected <- paste0('This program needs R package dualdagnosuchpkg, which is not installed. Install it in the R at ',
                    R.home(), ': install.packages("dualdagnosuchpkg"). Then press Re-run.')
  for (code in c('library(dualdagnosuchpkg)\nresult <- data.frame(USUBJID="S1")',
                 'dualdagnosuchpkg::f()\nresult <- data.frame(USUBJID="S1")')) {
    ran <- run_side(proj, "ADSL", "ADSL.USUBJID", "primary", code, timeout = 60, graph = g, spec = spec)
    expect_false(ran$ok)
    expect_identical(ran$error, expected)
    expect_match(ran$log, expected, fixed = TRUE)
    expect_null(ran$out_path)
  }
  expect_match(code_disallowed_packages('library(paste0("dualdagnosuchpkg"))\nresult <- data.frame(USUBJID="S1")'),
               "must name a package literally", fixed = TRUE)

  runtime <- run_side(proj, "ADSL", "ADSL.USUBJID", "validation",
    'loadNamespace("dualdagnosuchpkg")\nresult <- data.frame(USUBJID="S1")',
    timeout = 60, graph = g, spec = spec)
  expect_false(runtime$ok)
  expect_identical(runtime$error, expected)
  expect_match(runtime$log, expected, fixed = TRUE)
})

test_that("missing package generation keeps stored code and rerun skips model calls", {
  proj <- pkg_project()
  p <- open_project(proj)
  save_graph(proj, make_graph("ADSL", list(gn("ADSL.USUBJID"))))
  package_home <- tempfile("dualdag-packages-home-"); dir.create(package_home)
  old_home <- Sys.getenv("DUALDAG_HOME", unset = NA_character_)
  old_fake <- Sys.getenv("DUALDAG_FAKE_LLM", unset = NA_character_)
  on.exit(restore_test_env("DUALDAG_HOME", old_home), add = TRUE)
  on.exit(if (is.na(old_fake)) Sys.unsetenv("DUALDAG_FAKE_LLM") else Sys.setenv(DUALDAG_FAKE_LLM = old_fake), add = TRUE)
  Sys.setenv(DUALDAG_HOME = package_home, DUALDAG_FAKE_LLM = "1")
  generation_calls <- 0L
  review_calls <- 0L
  old_generate <- generate_side
  old_review <- if (exists("self_review", mode = "function")) self_review else NULL
  assign("generate_side", function(...) {
    generation_calls <<- generation_calls + 1L
    'library(dualdagnosuchpkg)\nresult <- data.frame(USUBJID="S1", stringsAsFactors=FALSE)'
  }, envir = .GlobalEnv)
  assign("self_review", function(...) { review_calls <<- review_calls + 1L; stop("review must not run") }, envir = .GlobalEnv)
  on.exit(assign("generate_side", old_generate, envir = .GlobalEnv), add = TRUE)
  on.exit(if (is.null(old_review)) rm("self_review", envir = .GlobalEnv) else assign("self_review", old_review, envir = .GlobalEnv), add = TRUE)

  got <- run_side_job(proj, "ADSL", "ADSL.USUBJID", "primary", NULL, 60)
  code_path <- file.path(side_run_dir(proj, "ADSL", "ADSL.USUBJID", "primary"), "code.R")
  expect_false(got$run$ok)
  expect_identical(read_text(code_path), got$code)
  expect_equal(generation_calls, 1L)
  expect_equal(review_calls, 0L)
  expect_false(file.exists(file.path(side_run_dir(proj, "ADSL", "ADSL.USUBJID", "primary"), "review.json")))
  st <- apply_generate_results(p, "ADSL", "ADSL.USUBJID",
    list(primary = list(ok = TRUE, value = list(run = got$run, same_source = FALSE))))
  expect_identical(st$problem$stage, "run")

  write_text('result <- data.frame(USUBJID="S1", stringsAsFactors=FALSE)', code_path)
  replay <- run_stored_sides(proj, "ADSL", "ADSL.USUBJID", "primary", timeout = 60,
                             graph = load_graph(proj, "ADSL"), spec = pkg_spec())
  expect_true(replay$results$primary$ok)
  rerun_state <- apply_stored_run(p, "ADSL", "ADSL.USUBJID", replay$results)
  expect_identical(rerun_state$problem, NULL)
  expect_equal(generation_calls, 1L)
  expect_equal(review_calls, 0L)
})

test_that("a per-side override uses that side's list only", {
  proj <- pkg_project()
  spec <- pkg_spec()
  g <- make_graph("ADSL", list(gn("ADSL.USUBJID")))
  r_packages_save(proj, list(
    r_packages = list("dplyr"),
    override = TRUE,
    r_packages_primary = list(),
    r_packages_validation = list("dplyr")
  ))
  expect_identical(side_r_packages(proj, "primary"), character())
  expect_identical(side_r_packages(proj, "validation"), "dplyr")
  bad <- run_side(proj, "ADSL", "ADSL.USUBJID", "primary", filter_code, timeout = 60, graph = g, spec = spec)
  expect_true(bad$ok)
  good <- run_side(proj, "ADSL", "ADSL.USUBJID", "validation", filter_code, timeout = 60, graph = g, spec = spec)
  expect_true(good$ok)
  expect_equal(readRDS(good$out_path)$USUBJID, "S1")
  expect_match(side_log(proj, "validation"), "packages: dplyr ", fixed = TRUE)
})

test_that("generate and ask prompts name the attached packages", {
  spec <- pkg_spec()
  g <- make_graph("ADSL", list(gn("ADSL.USUBJID")))
  user <- side_user_prompt("primary", spec, g, "ADSL.USUBJID", character())
  ask <- ask_user_prompt("primary", spec, g, "ADSL.USUBJID", "explain this code")
  general <- ask_user_prompt("primary", spec, g, "ADSL.USUBJID", "how many subjects are there")
  sys <- side_system_prompt("primary", spec, "ADSL", packages = "dplyr")
  ask_sys <- ask_system_prompt("primary", "ADSL", packages = c("dplyr", "tidyr"))
  for (text in list(user, ask, general, sys, ask_sys)) {
    expect_match(text, "Prefer these. If another package is really needed, call library(pkg) at the top of the program.", fixed = TRUE)
    expect_false(grepl("Do not call library()", text, fixed = TRUE))
    expect_false(grepl("DATASET_SPEC", text, fixed = TRUE))
  }
  expect_match(user, "PACKAGES: dplyr", fixed = TRUE)
  expect_match(ask, "PACKAGES: dplyr", fixed = TRUE)
  expect_match(general, "PACKAGES: dplyr", fixed = TRUE)
  expect_match(ask_sys, "dplyr, tidyr", fixed = TRUE)
  expect_match(strsplit(sys, "\n", fixed = TRUE)[[1]][[1]], "^You are the primary-side R expert")
  expect_equal(prompt_field(user, "VAR"), "USUBJID")
  expect_equal(prompt_field(user, "SOURCE"), "DM.USUBJID")
  expect_equal(prompt_field(user, "DERIVATION"), "Copy DM.USUBJID")

  proj <- pkg_project()
  r_packages_save(proj, list(
    r_packages = list("dplyr"),
    override = TRUE,
    r_packages_primary = list(),
    r_packages_validation = list("dplyr")
  ))
  primary <- side_user_prompt("primary", spec, g, "ADSL.USUBJID", character(), project = proj)
  validation <- ask_user_prompt("validation", spec, g, "ADSL.USUBJID", "explain this code", project = proj)
  expect_match(primary, "PACKAGES: none", fixed = TRUE)
  expect_match(validation, "PACKAGES: dplyr", fixed = TRUE)
  expect_false(grepl("AVAL - BASE", primary, fixed = TRUE))

  with_fake_llm({
    code <- generate_side(list(), "primary", spec, g, "ADSL.USUBJID", character(), project = proj)
    expect_match(code, "USUBJID", fixed = TRUE)
  })
})

test_that("GET and POST /api/packages keep the settings shape and do not reset state", {
  box <- isolate_pkg_home()
  on.exit(box$restore(), add = TRUE)
  proj <- pkg_project()
  A <- create_app(app_root, proj, 7983L, "pkg-token", load_config())
  A$project$state$nodes <- list(ADSL = list(ADSL.USUBJID = list(level = "L3")))
  state_path <- file.path(proj, ".dualdag", "state", "state.json")
  before <- readLines(state_path, warn = FALSE)

  got <- resp_json(call_app(A, "/api/packages"))
  expect_identical(chr_field(got$r_packages), "dplyr")
  expect_false(isTRUE(got$override))
  expect_null(got$r_packages_primary)
  expect_null(got$r_packages_validation)
  inst <- got$installed
  expect_true(is.list(inst) && length(inst) >= 1L)
  dplyr_row <- inst[[which(vapply(inst, function(x) identical(x$name, "dplyr"), logical(1)))]]
  expect_true(isTRUE(dplyr_row$installed))
  expect_equal(dplyr_row$version, as.character(utils::packageVersion("dplyr")))

  saved <- call_app(A, "/api/packages", "POST", list(
    r_packages = list("dplyr", "lubridate"),
    override = TRUE,
    r_packages_primary = list(),
    r_packages_validation = list("dplyr")
  ))
  expect_equal(saved$status, 200L)
  body <- resp_json(saved)
  expect_identical(chr_field(body$r_packages), c("dplyr", "lubridate"))
  expect_true(isTRUE(body$override))
  expect_identical(chr_field(body$r_packages_primary), character())
  expect_identical(chr_field(body$r_packages_validation), "dplyr")
  names <- vapply(body$installed, function(x) x$name, "")
  miss <- body$installed[[which(names == "lubridate")]]
  expect_false(isTRUE(miss$installed))
  expect_null(miss$version)
  expect_identical(readLines(state_path, warn = FALSE), before)
  expect_identical(A$project$state$nodes$ADSL$ADSL.USUBJID$level, "L3")

  bad <- call_app(A, "/api/packages", "POST", list(r_packages = list("1bad")))
  expect_equal(bad$status, 400L)
  expect_match(resp_json(bad)$error, "Invalid package name", fixed = TRUE)
  expect_identical(readLines(state_path, warn = FALSE), before)
  expect_identical(chr_field(read_r_packages(proj)$r_packages), c("dplyr", "lubridate"))

  cleared <- resp_json(call_app(A, "/api/packages", "POST", list(r_packages = list(), override = FALSE)))
  expect_identical(chr_field(cleared$r_packages), character())
  expect_false(isTRUE(cleared$override))
  expect_null(cleared$r_packages_primary)
  expect_null(cleared$r_packages_validation)
  again <- resp_json(call_app(A, "/api/packages"))
  expect_identical(chr_field(again$r_packages), character())
})

test_that("the primary prompt follows the spec path and validation does not", {
  sys <- side_system_prompt("primary", packages = "dplyr")
  val <- side_system_prompt("validation")
  expect_match(sys, "Follow the specification literally", fixed = TRUE)
  expect_match(sys, "step by step", fixed = TRUE)
  expect_match(sys, "in the order the spec states them", fixed = TRUE)
  expect_match(sys, "exactly the dataset and variable the spec names", fixed = TRUE)
  expect_match(sys, "Do not shortcut", fixed = TRUE)
  expect_match(sys, "say so in the top comment", fixed = TRUE)
  expect_match(sys, "only for what that note says", fixed = TRUE)
  expect_match(sys, "LINEAGE_ datasets listed in the user prompt", fixed = TRUE)
  expect_match(strsplit(sys, "\n", fixed = TRUE)[[1]][[1]], "^You are the primary-side R expert")
  expect_false(grepl("DATASET_SPEC", sys, fixed = TRUE))
  expect_false(grepl("step by step", val, fixed = TRUE))
  expect_false(grepl("orientation only", val, fixed = TRUE))
  expect_match(val, "alternative source", fixed = TRUE)
})

test_that("a primary run warns when code leaves the spec path and still succeeds", {
  spec <- spec_of(
    V("ADSL", "USUBJID", key = 1, source = "DM.USUBJID", row = 2),
    V("ADSL", "TRTSDT", source = "DM.USUBJID", derivation = "Copy DM.USUBJID", row = 3),
    V("ADSL", "AVAL", row = 4),
    V("ADSL", "BASE", row = 5),
    V("ADSL", "CHG", derivation = "AVAL - BASE", row = 6)
  )
  cols <- list(DM = c("USUBJID", "RFSTDTC", "AGE"))
  off <- "result <- data.frame(USUBJID = src$DM$USUBJID, TRTSDT = src$DM$RFSTDTC, stringsAsFactors = FALSE)\n"
  expect_equal(
    primary_spec_path_warnings(off, spec, "ADSL", "ADSL.TRTSDT", domain_cols = cols),
    "Primary uses DM.RFSTDTC, which the spec does not name for this variable"
  )
  on_path <- "result <- data.frame(USUBJID = src$DM$USUBJID, stringsAsFactors = FALSE)\n"
  expect_identical(
    primary_spec_path_warnings(on_path, spec, "ADSL", "ADSL.USUBJID", domain_cols = cols),
    character()
  )
  spec_ok <- spec_of(
    V("ADSL", "USUBJID", key = 1, source = "DM.USUBJID", row = 2),
    V("ADSL", "TRTSDT", source = "DM.RFSTDTC", derivation = "DM.RFSTDTC", row = 3)
  )
  expect_identical(
    primary_spec_path_warnings(off, spec_ok, "ADSL", "ADSL.TRTSDT", domain_cols = cols),
    character()
  )
  expect_identical(
    primary_spec_path_warnings(off, spec, "ADSL", "ADSL.TRTSDT", domain_cols = cols,
                               notes = list(list(text = "Use DM.RFSTDTC anyway"))),
    character()
  )
  expect_match(
    primary_spec_path_warnings(off, spec, "ADSL", "ADSL.TRTSDT", domain_cols = cols,
                               notes = list(list(text = "round the number"))),
    "DM.RFSTDTC", fixed = TRUE
  )
  work_off <- "result <- data.frame(USUBJID = work$USUBJID, TRTSDT = work$AGE, stringsAsFactors = FALSE)\n"
  expect_equal(
    primary_spec_path_warnings(work_off, spec, "ADSL", "ADSL.TRTSDT", domain_cols = cols),
    "Primary uses work.AGE, which the spec does not name for this variable"
  )
  work_ok <- "result <- data.frame(CHG = work$AVAL - work$BASE, stringsAsFactors = FALSE)\n"
  expect_identical(
    primary_spec_path_warnings(work_ok, spec, "ADSL", "ADSL.CHG", domain_cols = cols),
    character()
  )

  proj <- pkg_project()
  writeLines(paste(c(
    "Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Unique Subject,Char,20,1,DM.USUBJID,",
    "ADSL,TRTSDT,Start,Char,20,,DM.USUBJID,Copy DM.USUBJID"
  ), collapse = "\n"), file.path(proj, "spec", "variable.csv"))
  p <- open_project(proj)
  code_path <- file.path(side_run_dir(proj, "ADSL", "ADSL.TRTSDT", "primary"), "code.R")
  dir.create(dirname(code_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(off, code_path)
  st0 <- get_status(p, "ADSL", "ADSL.TRTSDT")
  st0$warnings <- list("Spec issue stays")
  put_status(p, "ADSL", "ADSL.TRTSDT", st0)
  st <- apply_stored_run(p, "ADSL", "ADSL.TRTSDT", list(primary = list(ok = TRUE, error = NULL)))
  expect_true(isTRUE(st$sides$primary$ok))
  expect_null(st$problem)
  expect_true("Spec issue stays" %in% as.character(st$warnings))
  expect_true("Primary uses DM.RFSTDTC, which the spec does not name for this variable" %in% as.character(st$warnings))
  kept <- apply_stored_run(p, "ADSL", "ADSL.TRTSDT", list(validation = list(ok = TRUE, error = NULL)))
  expect_true("Primary uses DM.RFSTDTC, which the spec does not name for this variable" %in% as.character(kept$warnings))
  write_json(list(list(at = "t", text = "Take DM.RFSTDTC")), file.path(dirname(code_path), "notes.json"))
  quiet <- apply_stored_run(p, "ADSL", "ADSL.TRTSDT", list(primary = list(ok = TRUE, error = NULL)))
  expect_false(any(grepl("DM.RFSTDTC", as.character(quiet$warnings %||% ""), fixed = TRUE)))
  expect_true("Spec issue stays" %in% as.character(quiet$warnings))
})

test_that("successful side rerun clears only resolved run-stage problems", {
  proj <- pkg_project(); p <- open_project(proj)
  id <- "ADSL.USUBJID"
  st <- get_status(p,"ADSL",id)
  st$problem <- list(stage="run",message="primary has a missing dependency")
  st$sides <- list(primary=list(error="missing dependency"),validation=list(error=NULL))
  put_status(p,"ADSL",id,st)
  cleared <- apply_stored_run(p,"ADSL",id,list(primary=list(ok=TRUE,error=NULL)))
  expect_null(cleared$problem)
  st <- get_status(p,"ADSL",id)
  st$problem <- list(stage="run",message="validation still failed")
  st$sides$validation <- list(error="still failed")
  put_status(p,"ADSL",id,st)
  retained <- apply_stored_run(p,"ADSL",id,list(primary=list(ok=TRUE,error=NULL)))
  expect_identical(retained$problem$stage,"run")
  st <- get_status(p,"ADSL",id)
  st$problem <- list(stage="compare",message="comparison issue")
  st$sides$validation <- list(error=NULL)
  put_status(p,"ADSL",id,st)
  other_stage <- apply_stored_run(p,"ADSL",id,list(primary=list(ok=TRUE,error=NULL)))
  expect_identical(other_stage$problem$stage,"compare")
})

test_that("the settings page omits the removed cards while package routes remain", {
  js <- paste(readLines(file.path(app_root, "www", "app.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_false(any(vapply(c('setCard("R packages"', 'setCard("Comparison"', 'setCard("Self-review"'),
                           grepl, logical(1), x = js, fixed = TRUE)))
  expect_false(grepl('api("/api/packages")|data-act="pkg-', js, perl = TRUE))
  expect_match(paste(readLines(file.path(app_root, "R", "server.R"), warn = FALSE), collapse = "\n"),
               'path == "/api/packages"', fixed = TRUE)
  expect_match(js, "specpath", fixed = TRUE)
  expect_match(js, "Primary uses ", fixed = TRUE)
})
