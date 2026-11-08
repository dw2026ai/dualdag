source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))

test_that("export merges primary results by key and keeps label, type, and length", {
  proj <- temp_project()
  spec <- spec_of(
    V("ADSL", "USUBJID", label = "Unique Subject", type = "Char", length = 10, key = 1, row = 2),
    V("ADSL", "TRTSDT", label = "Start Date", type = "Char", length = 8, row = 3),
    V("ADSL", "AGE", label = "Age", type = "Num", length = 8, row = 4)
  )
  expect_error(export_dataset(proj, "ADSL", spec, "rds"), "No primary result")
  write_result(proj, "ADSL", "ADSL.USUBJID", "primary",
               data.frame(USUBJID = c("S1", "S2"), stringsAsFactors = FALSE))
  write_result(proj, "ADSL", "ADSL.TRTSDT", "primary",
               data.frame(USUBJID = c("S1", "S2"), TRTSDT = c("2020-01-01x", "2020-02-02x"),
                          stringsAsFactors = FALSE))
  write_result(proj, "ADSL", "ADSL.AGE", "primary",
               data.frame(USUBJID = c("S2", "S1"), AGE = c("41", "40"), stringsAsFactors = FALSE))

  rds <- export_dataset(proj, "adsl", spec, "rds")
  d <- readRDS(rds)
  expect_equal(as.character(d$USUBJID), c("S1", "S2"))
  expect_equal(as.numeric(d$AGE), c(40, 41))
  expect_equal(as.character(d$TRTSDT), c("2020-01-", "2020-02-"))
  expect_equal(attr(d$USUBJID, "label"), "Unique Subject")
  expect_equal(attr(d$AGE, "label"), "Age")
  expect_equal(attr(d$TRTSDT, "label"), "Start Date")
  expect_equal(attr(d$USUBJID, "display_width"), 10L)
  expect_equal(attr(d$TRTSDT, "display_width"), 8L)
  expect_true(is.numeric(d$AGE))
  expect_true(is.character(d$USUBJID))
  expect_equal(attr(d, "label"), "ADSL")

  xpt <- export_dataset(proj, "ADSL", spec, "xpt")
  back <- haven::read_xpt(xpt)
  expect_equal(as.character(back$USUBJID), c("S1", "S2"))
  expect_equal(as.numeric(back$AGE), c(40, 41))
  expect_equal(attr(back$USUBJID, "label"), "Unique Subject")
  expect_equal(attr(back$AGE, "label"), "Age")
  expect_equal(attr(back$USUBJID, "format.sas"), "10")
  expect_equal(attr(back$TRTSDT, "format.sas"), "8")
  expect_equal(attr(back, "label"), "ADSL")
})
