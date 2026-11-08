app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
if (!exists("read_spec", mode = "function", inherits = TRUE)) {
  for (f in c("utils.R", "deps.R", "config.R", "spec.R", "project.R", "server.R"))
    sys.source(file.path(app_root, "R", f), envir = globalenv())
}

VAR_HEADER <- "Dataset,Variable,Label,Type,Length,Key,Source,Derivation"
PAR_HEADER <- "Dataset,PARAMCD,PARAM,Source,Derivation"

write_csv_file <- function(text) {
  f <- tempfile(fileext = ".csv")
  writeLines(text, f, useBytes = TRUE)
  f
}

test_that("row numbers follow physical lines, and names are trimmed and uppercased", {
  path <- write_csv_file(paste(c(
    VAR_HEADER,
    "",
    "adsl, usubjid ,Subject,char,20,1,DM.USUBJID,copy"
  ), collapse = "\n"))
  sp <- read_spec(path)
  expect_equal(nrow(sp$variables), 1)
  expect_equal(sp$variables$row, 3L)
  expect_equal(sp$variables$Dataset, "ADSL")
  expect_equal(sp$variables$Variable, "USUBJID")
  expect_equal(sp$variables$Key, 1L)
  expect_equal(sp$variables$Length, 20L)
  expect_true(is.integer(sp$variables$row))
  expect_true(is.integer(sp$variables$Key))
  expect_true(is.integer(sp$variables$Length))
  expect_equal(names(sp$variables), c("row", "Dataset", "Variable", "Label", "Type", "Length", "Key", "Source", "Derivation"))
  expect_equal(sp$datasets, "ADSL")
  expect_length(sp$problems, 0)
})

test_that("a blank Key is NA and two datasets may share a variable name", {
  path <- write_csv_file(paste(c(
    VAR_HEADER,
    "ADSL,USUBJID,Subject,char,20,1,DM.USUBJID,",
    "ADSL,AGE,Age,int,8,,DM.AGE,",
    "ADAE,USUBJID,Subject,char,20,1,AE.USUBJID,"
  ), collapse = "\n"))
  sp <- read_spec(path)
  expect_equal(sp$variables$Variable, c("USUBJID", "AGE", "USUBJID"))
  expect_true(is.na(sp$variables$Key[2]))
  expect_length(sp$problems, 0)
  expect_equal(sp$datasets, c("ADAE", "ADSL"))
})

test_that("duplicate Dataset + Variable, bad Key text, gaps, duplicate keys, and no Key are issues", {
  dup <- write_csv_file(paste(c(
    VAR_HEADER,
    "ADSL,USUBJID,A,char,20,1,,",
    "ADSL,USUBJID,B,char,20,2,,"
  ), collapse = "\n"))
  sp <- read_spec(dup)
  dups <- Filter(function(x) grepl("Duplicate Dataset \\+ Variable", x$message), sp$problems)
  expect_equal(length(dups), 1)
  expect_equal(dups[[1]]$type, "structure")
  expect_equal(dups[[1]]$variables, list("USUBJID"))
  expect_true(grepl("2 and 3", dups[[1]]$message))

  gap <- write_csv_file(paste(c(VAR_HEADER, "ADSL,A,A,char,8,1,,", "ADSL,B,B,char,8,3,,"), collapse = "\n"))
  sp <- read_spec(gap)
  expect_true(any(vapply(sp$problems, function(x) x$type == "structure" && grepl("1\\.\\.n", x$message), TRUE)))

  same <- write_csv_file(paste(c(VAR_HEADER, "ADSL,A,A,char,8,1,,", "ADSL,B,B,char,8,1,,"), collapse = "\n"))
  sp <- read_spec(same)
  expect_true(any(grepl("no gaps or duplicates", vapply(sp$problems, `[[`, "", "message"))))

  none <- write_csv_file(paste(c(VAR_HEADER, "ADSL,A,Age,char,8,,,"), collapse = "\n"))
  sp <- read_spec(none)
  nk <- Filter(function(x) identical(x$type, "no_key"), sp$problems)
  expect_equal(length(nk), 1)
  expect_match(nk[[1]]$message, "ADSL has no Key")

  bad <- write_csv_file(paste(c(VAR_HEADER, "ADSL,A,A,char,8,abc,,"), collapse = "\n"))
  sp <- read_spec(bad)
  expect_true(any(grepl("positive integer", vapply(sp$problems, `[[`, "", "message"))))

  miss <- write_csv_file("Dataset,Variable,Label,Type,Length,Source,Derivation\nADSL,A,A,char,8,,\n")
  sp <- read_spec(miss)
  expect_true(any(grepl("Missing column: Key", vapply(sp$problems, `[[`, "", "message"))))
  expect_false(any(vapply(sp$problems, function(x) identical(x$type, "no_key"), TRUE)))
})

test_that("parameter PARAMCD is uppercased, and a parameter-only dataset has no Key", {
  var <- write_csv_file(paste(c(VAR_HEADER, "ADSL,USUBJID,Subject,char,20,1,,"), collapse = "\n"))
  par <- write_csv_file(paste(c(PAR_HEADER, "advs, weight ,Weight,VS.VSSTRESN,when VSTESTCD = WEIGHT"), collapse = "\n"))
  sp <- read_spec(var, par)
  expect_equal(sp$parameters$Dataset, "ADVS")
  expect_equal(sp$parameters$PARAMCD, "WEIGHT")
  expect_equal(sp$parameters$row, 2L)
  expect_equal(names(sp$parameters), c("row", "Dataset", "PARAMCD", "PARAM", "Source", "Derivation"))
  orphans <- Filter(function(x) identical(x$type, "no_key") && identical(x$dataset, "ADVS"), sp$problems)
  expect_equal(length(orphans), 1)
  expect_match(sp$hash, "^[0-9a-f]{32}$")
  writeLines(paste(readLines(var), "extra"), var)
  sp2 <- read_spec(var, par)
  expect_false(identical(sp$hash, sp2$hash))
})
