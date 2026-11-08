app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash="/")
for (f in list.files(file.path(app_root, "R"), pattern="\\.R$", full.names=TRUE))
  sys.source(f, envir=.GlobalEnv)

test_that("dataset format setup separates BDS and BDS-TTE from ADSL and OCCDS", {
  p <- open_project(tempfile("dataset-formats-"))
  variable <- paste(c(
    "Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Subject,Char,20,1,DM.USUBJID,",
    "ADLB,USUBJID,Subject,Char,20,1,DM.USUBJID,",
    "ADTTE,USUBJID,Subject,Char,20,1,DM.USUBJID,",
    "ADAE,USUBJID,Subject,Char,20,1,DM.USUBJID,"
  ), collapse="\n")
  parameter <- paste(c("Dataset,PARAMCD,PARAM,Source,Derivation", "ADLB,ALT,Alanine aminotransferase,,", "ADTTE,OS,Overall survival,,"), collapse="\n")
  project_upload_spec(p, charToRaw(variable), charToRaw(parameter))
  spec <- read_project_spec(p)
  inferred <- dataset_types_public(p, spec)
  expect_equal(inferred$ADSL, "ADSL")
  expect_equal(inferred$ADLB, "BDS")
  expect_equal(inferred$ADTTE, "BDS-TTE")
  expect_equal(inferred$ADAE, "OCCDS")
  chosen <- list(ADSL="ADSL", ADLB="BDS", ADTTE="BDS-TTE", ADAE="OCCDS")
  saved <- save_dataset_types(p, chosen, spec)
  expect_setequal(unname(unlist(saved)), unname(unlist(chosen)))
  expect_equal(dataset_type_get(p, "ADTTE", spec), "BDS-TTE")
  expect_error(save_dataset_types(p, list(ADSL="ADSL", ADLB="ADSL", ADTTE="BDS-TTE", ADAE="OCCDS"), spec), "exactly one ADSL")
})
