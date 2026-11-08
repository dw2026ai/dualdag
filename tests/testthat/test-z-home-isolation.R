test_that("test suite keeps config writes away from HOME", {
  test_home <- Sys.getenv("DUALDAG_TEST_HOME", unset = "")
  expect_true(nzchar(test_home))
  expect_identical(normalizePath(Sys.getenv("DUALDAG_HOME")), normalizePath(test_home))
  marker <- Sys.getenv("DUALDAG_TEST_HOME_MARKER", unset = "")
  if (nzchar(marker)) {
    expect_true(file.exists(marker))
    expect_identical(readBin(marker, "raw", n = file.info(marker)$size), charToRaw("scratch-home-config-marker-v1\n"))
  }
})
