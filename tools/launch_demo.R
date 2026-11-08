launch_demo <- function(dataset, app_dir, args = commandArgs(trailingOnly = TRUE)) {
  demo_dir <- file.path(app_dir, "demos", dataset)
  if (!dir.exists(demo_dir)) stop("Demo project is missing: demos/", dataset, call. = FALSE)
  data_dir <- file.path(demo_dir, "data")
  expected <- if (dataset == "adsl") {
    paste0(c("dm", "suppdm", "ex", "ds", "ae", "vs", "lb", "sv", "cm", "mh"), ".xpt")
  } else c("vs.xpt", "adsl.xpt")
  present <- if (dir.exists(data_dir)) list.files(data_dir, pattern = "[.]xpt$", ignore.case = TRUE) else character()
  missing <- expected[!file.exists(file.path(data_dir, expected))]
  if (length(missing) && length(present)) {
    stop("Demo data is incomplete (missing ", paste(missing, collapse = ", "),
         "). To explicitly refresh the demo data, run: Rscript tools/get_demo_data.R ", dataset,
         call. = FALSE)
  }
  if (length(missing)) {
    cat("Fetching the ", toupper(dataset), " demo data from pharmaverse.\n", sep = "")
    fetch_script <- file.path(app_dir, "tools", "get_demo_data.R")
    tryCatch({
      fetch_env <- new.env(parent = globalenv())
      fetch_env$.DUALDAG_GET_DEMO_DATA_SOURCE_ONLY <- TRUE
      sys.source(fetch_script, envir = fetch_env)
      fetch_env$get_demo_data(dataset, root = app_dir)
      still_missing <- expected[!file.exists(file.path(data_dir, expected))]
      if (length(still_missing)) stop("Fetch completed without writing: ", paste(still_missing, collapse = ", "))
    }, error = function(e) {
      stop("Could not fetch demo data. Retry with: Rscript tools/get_demo_data.R ", dataset,
           " (", conditionMessage(e), ")", call. = FALSE)
    })
  }

  run_file <- normalizePath(file.path(app_dir, "run.R"), winslash = "/", mustWork = TRUE)
  old_project <- Sys.getenv("DUALDAG_PROJECT", unset = NA_character_)
  on.exit(if (is.na(old_project)) Sys.unsetenv("DUALDAG_PROJECT") else Sys.setenv(DUALDAG_PROJECT = old_project), add = TRUE)
  Sys.setenv(DUALDAG_PROJECT = normalizePath(demo_dir, winslash = "/", mustWork = TRUE))
  source(run_file, local = FALSE, chdir = FALSE)
}
