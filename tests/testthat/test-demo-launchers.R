demo_launcher_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
demo_launcher_rscript <- file.path(R.home("bin"), "Rscript")

demo_launcher_run <- function(script, args = character(), env = character()) {
  output <- suppressWarnings(system2(demo_launcher_rscript, c(shQuote(script), shQuote(args)),
                                     stdout = TRUE, stderr = TRUE, env = env))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(output = output, status = status)
}

demo_make_data_files <- function(root, dataset) {
  data_dir <- file.path(root, "demos", dataset, "data")
  dir.create(data_dir, recursive = TRUE)
  files <- if (dataset == "adsl") {
    paste0(c("dm", "suppdm", "ex", "ds", "ae", "vs", "lb", "sv", "cm", "mh"), ".xpt")
  } else c("vs.xpt", "adsl.xpt")
  file.create(file.path(data_dir, files))
}

test_that("demo launchers source run.R, set and restore the project, and forward CLI flags", {
  for (dataset in c("adsl", "advs")) {
    root <- tempfile(paste0("demo-launcher-", dataset, "-"))
    dir.create(root)
    dir.create(file.path(root, "demos", dataset), recursive = TRUE)
    dir.create(file.path(root, "tools"))
    expect_true(file.copy(file.path(demo_launcher_root, paste0("run_demo_", dataset, ".R")), root))
    expect_true(file.copy(file.path(demo_launcher_root, "tools", "launch_demo.R"),
                          file.path(root, "tools", "launch_demo.R")))
    demo_make_data_files(root, dataset)

    arg_log <- file.path(root, "args.txt")
    project_log <- file.path(root, "project.txt")
    pid_log <- file.path(root, "pid.txt")
    writeLines(c(
      "writeLines(commandArgs(trailingOnly = TRUE), Sys.getenv('DEMO_ARG_LOG'))",
      "writeLines(Sys.getenv('DUALDAG_PROJECT'), Sys.getenv('DEMO_PROJECT_LOG'))",
      "writeLines(as.character(Sys.getpid()), Sys.getenv('DEMO_PID_LOG'))"
    ), file.path(root, "run.R"))
    home <- file.path(root, "home")
    dualdag_home <- file.path(root, "dualdag-home")
    dir.create(home)
    dir.create(dualdag_home)
    output <- demo_launcher_run(
      file.path(root, paste0("run_demo_", dataset, ".R")),
      c("--install-deps", "--no-browser", "--port=18861"),
      c(paste0("HOME=", home), paste0("DUALDAG_HOME=", dualdag_home),
        "DUALDAG_FAKE_LLM=1", "DUALDAG_LLM_API_KEY=", paste0("DEMO_ARG_LOG=", arg_log),
        paste0("DEMO_PROJECT_LOG=", project_log), paste0("DEMO_PID_LOG=", pid_log))
    )

    expect_equal(output$status, 0L, info = paste(output$output, collapse = "\n"))
    expect_equal(readLines(arg_log), c("--install-deps", "--no-browser", "--port=18861"))
    expect_equal(readLines(project_log), normalizePath(file.path(root, "demos", dataset)))

    old_project <- Sys.getenv("DUALDAG_PROJECT", unset = NA_character_)
    old_pid_log <- Sys.getenv("DEMO_PID_LOG", unset = NA_character_)
    Sys.setenv(DUALDAG_PROJECT = "previous-project")
    Sys.setenv(DEMO_PID_LOG = pid_log)
    writeLines("writeLines(as.character(Sys.getpid()), Sys.getenv('DEMO_PID_LOG'))", file.path(root, "run.R"))
    launch_env <- new.env(parent = globalenv())
    sys.source(file.path(root, "tools", "launch_demo.R"), envir = launch_env)
    launch_env$launch_demo(dataset, root, args = "--port=18862")
    expect_equal(as.integer(readLines(pid_log)), Sys.getpid())
    expect_identical(Sys.getenv("DUALDAG_PROJECT"), "previous-project")
    if (is.na(old_project)) Sys.unsetenv("DUALDAG_PROJECT") else Sys.setenv(DUALDAG_PROJECT = old_project)
    if (is.na(old_pid_log)) Sys.unsetenv("DEMO_PID_LOG") else Sys.setenv(DEMO_PID_LOG = old_pid_log)
  }
})

test_that("missing demo data is fetched before run.R starts", {
  root <- tempfile("demo-launcher-fetch-")
  dir.create(root)
  dir.create(file.path(root, "demos", "advs", "data"), recursive = TRUE)
  dir.create(file.path(root, "tools"))
  writeLines(c(
    "get_demo_data <- function(dataset, root) {",
    "  files <- c('vs.xpt', 'adsl.xpt')",
    "  file.create(file.path(root, 'demos', dataset, 'data', files))",
    "}"
  ), file.path(root, "tools", "get_demo_data.R"))
  writeLines("writeLines(Sys.getenv('DUALDAG_PROJECT'), Sys.getenv('DEMO_PROJECT_LOG'))", file.path(root, "run.R"))
  project_log <- file.path(root, "project.txt")
  old_project_log <- Sys.getenv("DEMO_PROJECT_LOG", unset = NA_character_)
  Sys.setenv(DEMO_PROJECT_LOG = project_log)
  fetch_env <- new.env(parent = globalenv())
  sys.source(file.path(demo_launcher_root, "tools", "launch_demo.R"), envir = fetch_env)
  fetch_env$launch_demo("advs", root, args = character())
  expect_true(all(file.exists(file.path(root, "demos", "advs", "data", c("vs.xpt", "adsl.xpt")))))
  if (is.na(old_project_log)) Sys.unsetenv("DEMO_PROJECT_LOG") else Sys.setenv(DEMO_PROJECT_LOG = old_project_log)
})

test_that("partial existing demo data is preserved and requires an explicit fetch", {
  root <- tempfile("demo-launcher-partial-")
  data_dir <- file.path(root, "demos", "advs", "data")
  dir.create(data_dir, recursive = TRUE)
  vs_path <- file.path(data_dir, "vs.xpt")
  writeBin(as.raw(c(1, 2, 3, 4)), vs_path)
  original <- readBin(vs_path, "raw", n = 10)
  dir.create(file.path(root, "tools"))
  fetch_marker <- file.path(root, "fetch-was-started")
  writeLines(c(
    "get_demo_data <- function(dataset, root) {",
    "  file.create(Sys.getenv('DEMO_FETCH_MARKER'))",
    "  writeBin(as.raw(9), file.path(root, 'demos', dataset, 'data', 'vs.xpt'))",
    "}"
  ), file.path(root, "tools", "get_demo_data.R"))
  run_marker <- file.path(root, "run-was-started")
  writeLines("file.create(Sys.getenv('DEMO_RUN_MARKER'))", file.path(root, "run.R"))
  Sys.setenv(DEMO_FETCH_MARKER = fetch_marker, DEMO_RUN_MARKER = run_marker)
  launch_env <- new.env(parent = globalenv())
  sys.source(file.path(demo_launcher_root, "tools", "launch_demo.R"), envir = launch_env)

  expect_error(launch_env$launch_demo("advs", root), "Rscript tools/get_demo_data.R advs")
  expect_identical(readBin(vs_path, "raw", n = 10), original)
  expect_false(file.exists(fetch_marker))
  expect_false(file.exists(run_marker))
  Sys.unsetenv(c("DEMO_FETCH_MARKER", "DEMO_RUN_MARKER"))
})

test_that("a fetch failure names the retry command and does not launch run.R", {
  root <- tempfile("demo-launcher-fetch-failure-")
  dir.create(root)
  script <- file.path(root, "run_demo_advs.R")
  expect_true(file.copy(file.path(demo_launcher_root, "run_demo_advs.R"), root))
  dir.create(file.path(root, "tools"))
  expect_true(file.copy(file.path(demo_launcher_root, "tools", "launch_demo.R"),
                        file.path(root, "tools", "launch_demo.R")))
  dir.create(file.path(root, "demos", "advs", "data"), recursive = TRUE)
  sentinel <- file.path(root, "run-was-started")
  writeLines("file.create(Sys.getenv('DEMO_SENTINEL'))", file.path(root, "run.R"))
  home <- file.path(root, "home")
  dualdag_home <- file.path(root, "dualdag-home")
  dir.create(home)
  dir.create(dualdag_home)

  output <- demo_launcher_run(
    script, env = c(paste0("HOME=", home), paste0("DUALDAG_HOME=", dualdag_home),
                    "DUALDAG_FAKE_LLM=1", "DUALDAG_LLM_API_KEY=", paste0("DEMO_SENTINEL=", sentinel))
  )
  expect_false(identical(output$status, 0L))
  expect_true(any(grepl("Rscript tools/get_demo_data.R advs", output$output, fixed = TRUE)))
  expect_false(file.exists(sentinel))
})
