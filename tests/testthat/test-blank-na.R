# SAS character missing semantics and distinct viewer payloads.
root_blank <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
for (f in c("utils.R", "deps.R", "config.R", "llm.R", "dag.R", "spec.R", "project.R",
            "catalog.R", "sides.R", "compare.R", "viewer.R", "inspect.R", "server.R")) {
  sys.source(file.path(root_blank, "R", f), envir = globalenv())
}
source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))

blank_fixture <- function() {
  dir <- tempfile("blank-na-")
  dir.create(file.path(dir, "spec"), recursive = TRUE)
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
               "ADSL,USUBJID,Subject,Char,20,1,DM.USUBJID,Copy key",
               "ADSL,TRTETMF,Time flag,Char,8,,DM.TRTETMF,Otherwise blank"),
             file.path(dir, "spec", "variable.csv"))
  p <- open_project(dir)
  save_graph(p, list(dataset = "ADSL", nodes = list(gn("ADSL.USUBJID"), gn("ADSL.TRTETMF")),
                     edges = list(), groups = list(), external = list(), unresolved = list()))
  write_result(dir, "ADSL", "ADSL.TRTETMF", "primary",
               data.frame(USUBJID = c("S1", "S2"), TRTETMF = c("", " "), stringsAsFactors = FALSE))
  write_result(dir, "ADSL", "ADSL.TRTETMF", "validation",
               data.frame(USUBJID = c("S1", "S2"), TRTETMF = c(NA_character_, NA_character_),
                          stringsAsFactors = FALSE))
  p
}

test_that("stored false blank policy is ignored and blank text still equals NA", {
  p <- blank_fixture()
  write_json(list(blank_equals_na = FALSE), file.path(p$dir, ".dualdag", "state", "compare_settings.json"))
  expect_true(compare_settings(p)$blank_equals_na)
  cmp <- compare_node(p$dir, "ADSL", "ADSL.TRTETMF", "USUBJID")
  expect_true(cmp$match)
  expect_equal(cmp$n_diff, 0L)
  expect_match(cmp$summary, "2 row(s): blank or whitespace-only text and NA treated as equal", fixed = TRUE)
  before <- readRDS(file.path(side_run_dir(p$dir, "ADSL", "ADSL.TRTETMF", "primary"), "out.rds"))
  expect_identical(before$TRTETMF, c("", " "))
  expect_true(compare_settings(open_project(p$dir))$blank_equals_na)
})

test_that("numeric NA stays numeric missing and differs from zero", {
  dir <- temp_project()
  write_result(dir, "ADSL", "ADSL.AVAL", "primary",
               data.frame(USUBJID = c("S1", "S2"), AVAL = c(NA_real_, 1)))
  write_result(dir, "ADSL", "ADSL.AVAL", "validation",
               data.frame(USUBJID = c("S1", "S2"), AVAL = c(0, 1)))
  cmp <- compare_node(dir, "ADSL", "ADSL.AVAL", "USUBJID")
  expect_false(cmp$match)
  expect_equal(cmp$n_diff, 1L)
  expect_false(grepl("blank", cmp$summary, fixed = TRUE))
  expect_true(is.na(cmp$diffs$primary[[1]]))
})

test_that("old project settings route is removed and state keeps blank equality enabled", {
  p <- blank_fixture()
  A <- create_app(root_blank, p$dir, 18988L, "blank-token", load_config())
  raw <- charToRaw(as.character(to_json(list(blank_equals_na = FALSE))))
  removed <- make_handlers(A)$call(list(PATH_INFO = "/api/project/settings", REQUEST_METHOD = "POST",
    QUERY_STRING = "", HTTP_HOST = "127.0.0.1:18988", HTTP_X_DUALDAG_TOKEN = "blank-token",
    CONTENT_TYPE = "application/json", rook.input = list(read = function() raw)))
  expect_equal(removed$status, 404L)
  expect_true(compare_node(p$dir, "ADSL", "ADSL.TRTETMF", "USUBJID")$match)
  expect_equal(state_payload(A)$project_settings$blank_equals_na, TRUE)
})

test_that("viewer and node-input payloads preserve empty string separately from null", {
  p <- blank_fixture()
  v <- view_dataset(p, "res:ADSL:ADSL.TRTETMF:primary")
  expect_identical(v$rows[[1]][[2]], "")
  expect_identical(v$rows[[2]][[2]], " ")
  write_result(p$dir, "ADSL", "ADSL.TRTETMF", "primary",
               data.frame(USUBJID = c("S1", "S2"), TRTETMF = c("", NA_character_)))
  v2 <- view_dataset(p, "res:ADSL:ADSL.TRTETMF:primary")
  expect_identical(v2$rows[[1]][[2]], "")
  expect_null(v2$rows[[2]][[2]])
  payload <- as.character(to_json(v2$rows))
  expect_match(payload, '""', fixed = TRUE)
  expect_match(payload, "null", fixed = TRUE)
  inputs <- node_inputs(p, "ADSL", "ADSL.TRTETMF", only_diff = FALSE)
  names <- vapply(inputs$columns, function(c) c$name, "")
  i <- match("primary_TRTETMF", names)
  expect_identical(inputs$rows[[1]][[i]], "")
  expect_null(inputs$rows[[2]][[i]])
  expect_false(isTRUE(tail(inputs$rows[[1]], 1)[[1]]))
})
