# Source variable catalog: build, reuse, lookup, prompts, missing_var, missing_value.

root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))
for (f in c("utils.R", "spec.R", "project.R", "catalog.R")) {
  sys.source(file.path(root, "R", f), envir = globalenv())
}

read_raw <- function(path) readBin(path, "raw", file.info(path)$size)

copy_fixtures <- function(proj) {
  file.copy(fixture_path("dm.xpt"), file.path(proj, "data", "dm.xpt"), overwrite = TRUE)
  file.copy(fixture_path("vs.xpt"), file.path(proj, "data", "vs.xpt"), overwrite = TRUE)
  proj
}

test_that("catalog builds from fixture xpt, captures VSTESTCD, and looks up columns", {
  proj <- temp_project()
  copy_fixtures(proj)
  cat <- build_source_catalog(proj)
  expect_true(all(c("DM", "VS") %in% names(cat$domains)))
  dm_names <- vapply(cat$domains$DM$variables, function(v) v$name, "")
  expect_equal(dm_names, c("USUBJID", "RFSTDTC", "AGE"))
  expect_true(all(vapply(cat$domains$DM$variables, function(v) "label" %in% names(v), logical(1))))
  expect_equal(catalog_lookup(cat, "DM", "USUBJID"), "ok")
  expect_equal(catalog_lookup(cat, "dm", "age"), "ok")
  expect_equal(catalog_lookup(cat, "DM", "NOTAVAR"), "missing_var")
  expect_equal(catalog_lookup(cat, "XX", "USUBJID"), "missing_domain")
  vals <- as.character(unlist(cat$domains$VS$values$VSTESTCD))
  expect_true(all(c("HEIGHT", "WEIGHT") %in% vals))
  txt <- catalog_prompt_text(cat)
  expect_match(txt, "DM \\(3 rows\\): USUBJID RFSTDTC AGE")
  expect_match(txt, "VSTESTCD: HEIGHT, WEIGHT")
  expect_false(grepl("S1", txt, fixed = TRUE))
  idx <- catalog_index(cat)
  expect_true(all(c("USUBJID", "RFSTDTC", "AGE") %in% idx$DM))
  expect_equal(data_index(proj), idx)
  pub <- catalog_public(cat)
  expect_null(pub$domains$DM$md5)
  expect_false(is.null(cat$domains$DM$md5))
  sm <- catalog_summary(cat)
  expect_equal(sm$DM$nrow, 3)
  expect_equal(sm$DM$nvars, 3L)
  expect_equal(sm$DM$file, "dm.xpt")
  expect_equal(sm$VS$nvars, 3L)
})

test_that("an unchanged catalog is reused and does not re-read the xpt", {
  proj <- temp_project()
  copy_fixtures(proj)
  build_source_catalog(proj)
  path <- catalog_file(proj)
  before_reads <- catalog_xpt_reads()
  before_mtime <- file.info(path)$mtime
  again <- build_source_catalog(proj)
  expect_equal(catalog_xpt_reads(), before_reads)
  expect_equal(file.info(path)$mtime, before_mtime)
  expect_true(all(c("DM", "VS") %in% names(again$domains)))
  forced <- build_source_catalog(proj, force = TRUE)
  expect_gt(catalog_xpt_reads(), before_reads)
  expect_true("VS" %in% names(forced$domains))
})

test_that("upload and remove rebuild the catalog", {
  proj <- tempfile("dualdag-cat-")
  p <- open_project(proj)
  before <- catalog_xpt_reads()
  project_upload_data(p, "dm.xpt", read_raw(fixture_path("dm.xpt")))
  expect_equal(catalog_xpt_reads(), before + 1L)
  cat <- build_source_catalog(p)
  expect_equal(catalog_xpt_reads(), before + 1L)
  expect_true("DM" %in% names(cat$domains))
  expect_false("VS" %in% names(cat$domains))
  project_upload_data(p, "vs.xpt", read_raw(fixture_path("vs.xpt")))
  expect_equal(catalog_xpt_reads(), before + 2L)
  cat <- build_source_catalog(p)
  expect_equal(catalog_xpt_reads(), before + 2L)
  expect_true(all(c("DM", "VS") %in% names(cat$domains)))
  project_remove_data(p, "vs.xpt")
  cat <- build_source_catalog(p)
  expect_false("VS" %in% names(cat$domains %||% list()))
  expect_true("DM" %in% names(cat$domains))
  expect_equal(catalog_xpt_reads(), before + 2L)
})

test_that("a spec ref to DM.NOTAVAR is missing_var, and an absent VSTESTCD is missing_value", {
  proj <- temp_project()
  copy_fixtures(proj)
  cat <- build_source_catalog(proj)
  idx <- catalog_index(cat)
  spec <- spec_of(
    V("ADSL", "USUBJID", key = 1L, source = "DM.USUBJID", row = 2),
    V("ADSL", "BAD", source = "DM.NOTAVAR", derivation = "DM.NOTAVAR", row = 3)
  )
  g <- make_graph("ADSL", list(gn("ADSL.USUBJID"), gn("ADSL.BAD")), external = list(
    list(ref = "DM.NOTAVAR", domain = "DM", var = "NOTAVAR", used_by = list("ADSL.BAD"), evidence = "DM.NOTAVAR")
  ))
  chk <- check_graph(g, spec, idx, catalog = cat)
  miss <- issue_of(chk, "missing_var")
  expect_true(length(miss) >= 1)
  expect_true(any(grepl("NOTAVAR", vapply(miss, function(i) paste(i$message, i$text), ""))))

  bad_param <- spec_of(
    V("ADVS", "USUBJID", key = 1L, row = 2),
    V("ADVS", "AVAL", row = 3),
    params = list(P("ADVS", "NOTAREAL", source = "VS.VSSTRESN",
                    derivation = "VS.VSSTRESN where VSTESTCD=NOTAREAL", row = 2))
  )
  g2 <- make_graph("ADVS", list(gn("ADVS.USUBJID"), gn("ADVS.AVAL")))
  chk2 <- check_graph(g2, bad_param, idx, catalog = cat)
  mv <- issue_of(chk2, "missing_value")
  expect_true(length(mv) >= 1)
  expect_equal(mv[[1]]$severity, "error")
  expect_true(grepl("NOTAREAL", mv[[1]]$message))

  ok_param <- spec_of(
    V("ADVS", "USUBJID", key = 1L, row = 2),
    V("ADVS", "AVAL", row = 3),
    params = list(P("ADVS", "WEIGHT", source = "VS.VSSTRESN",
                    derivation = "VS.VSSTRESN where VSTESTCD=WEIGHT", row = 2))
  )
  chk3 <- check_graph(g2, ok_param, idx, catalog = cat)
  expect_length(issue_of(chk3, "missing_value"), 0)

  # PARAMCD itself is the test code of the source domain.
  by_code <- spec_of(
    V("ADVS", "USUBJID", key = 1L, row = 2),
    params = list(P("ADVS", "NOTAREAL", source = "VS.VSSTRESN", derivation = "", row = 2))
  )
  chk4 <- check_graph(g2, by_code, idx, catalog = cat)
  expect_true(length(issue_of(chk4, "missing_value")) >= 1)

  prose <- spec_of(
    V("ADVS", "USUBJID", key = 1L, row = 2),
    params = list(P("ADVS", "WEIGHT", source = "", derivation = "change from baseline", row = 2))
  )
  chk5 <- check_graph(g2, prose, idx, catalog = cat)
  expect_length(issue_of(chk5, "missing_value"), 0)
})

test_that("prompts carry catalog names and the fake extractor resolves them", {
  proj <- temp_project()
  copy_fixtures(proj)
  cat <- build_source_catalog(proj)
  spec <- spec_of(
    V("ADSL", "TRTSDT", key = 1L, derivation = "RFSTDTC converted", row = 2),
    V("ADSL", "BAD", derivation = "DM.NOTAVAR", row = 3)
  )
  g0 <- make_graph("ADSL", list(gn("ADSL.TRTSDT"), gn("ADSL.BAD")), confirmed = FALSE)
  user <- paste(extract_catalog_lines(spec, "ADSL", cat), collapse = "\n")
  expect_match(user, "USUBJID")
  expect_match(user, "RFSTDTC")
  expect_match(user, "AGE")
  expect_match(user, "DM \\(3 rows\\)")
  side <- side_user_prompt("primary", spec, g0, "ADSL.TRTSDT", character(), catalog = cat)
  expect_false(grepl("USUBJID", side, fixed = TRUE))
  expect_false(grepl("SOURCE_CATALOG:", side, fixed = TRUE))
  expect_false(grepl("S1", side, fixed = TRUE))
  side_v <- side_user_prompt("validation", spec, g0, "ADSL.TRTSDT", character(), catalog = cat)
  expect_match(side_v, "RFSTDTC", fixed = TRUE)
  expect_false(grepl("S1", side_v, fixed = TRUE))

  with_fake_llm({
    g <- extract_dependencies(list(), spec, "ADSL", catalog = cat)
    refs <- if (length(g$external)) vapply(g$external, function(e) e$ref, "") else character()
    expect_true("DM.RFSTDTC" %in% refs)
    types <- if (length(g$unresolved)) vapply(g$unresolved, function(i) i$type %||% "", "") else character()
    expect_true("missing_var" %in% types)
  })
})
