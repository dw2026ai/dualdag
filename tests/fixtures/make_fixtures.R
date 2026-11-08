# Tiny xpt fixtures for engine tests. Run from anywhere:
#   Rscript tests/fixtures/make_fixtures.R
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script <- if (length(file_arg)) sub("^--file=", "", file_arg[1]) else "tests/fixtures/make_fixtures.R"
out_dir <- normalizePath(dirname(script), mustWork = TRUE)

dm <- data.frame(
  USUBJID = c("S1", "S2", "S3"),
  RFSTDTC = c("2020-01-01", "2020-01-02", "2020-01-03"),
  AGE = c(40, 41, 42),
  stringsAsFactors = FALSE
)
vs <- data.frame(
  USUBJID = c("S1", "S1", "S2"),
  VSTESTCD = c("WEIGHT", "HEIGHT", "WEIGHT"),
  VSSTRESN = c(70, 170, 80),
  stringsAsFactors = FALSE
)
haven::write_xpt(dm, file.path(out_dir, "dm.xpt"), version = 5, name = "DM")
haven::write_xpt(vs, file.path(out_dir, "vs.xpt"), version = 5, name = "VS")
message("wrote ", file.path(out_dir, "dm.xpt"))
message("wrote ", file.path(out_dir, "vs.xpt"))
