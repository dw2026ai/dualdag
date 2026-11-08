source(testthat::test_path("..", "fixtures", "helper.R"))

test_that("stored log tags mark errors, warnings, and continuation lines", {
  lines <- c("> stop('x')", "Error in stop('x') : bad", "  more detail", "Execution halted", "",
             "Warning message:", "In f() : converted", "  detail", "> result <- 1")
  expect_equal(classify_log_lines(paste(lines, collapse = "\n")),
               c("", "error", "error", "error", "", "warning", "warning", "warning", ""))
  project <- temp_project()
  code <- 'stop("DUALDAG_MISSING_DEPENDENCY: DM.MISSING")'
  run <- run_side(project, "ADSL", "ADSL.X", "primary", code, timeout = 30)
  expect_false(run$ok)
  tags <- readRDS(file.path(side_run_dir(project, "ADSL", "ADSL.X", "primary"), "log_tags.rds"))
  expect_true("error" %in% tags)
  expect_match(run$log, "DUALDAG_MISSING_DEPENDENCY", fixed = TRUE)
  expect_equal(classify_log_lines('DUALDAG_MISSING_COLUMN: Missing column: X is not in LINEAGE_SYSBP or the provided data.'), "error")
})
