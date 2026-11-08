# Run from the repository root:
#   Rscript tests/testthat.R
args <- commandArgs(FALSE)
f <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
here <- if (length(f)) dirname(normalizePath(f)) else "tests"
res <- testthat::test_dir(file.path(here, "testthat"), reporter = "summary", stop_on_failure = TRUE)
