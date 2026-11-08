# User-visible sources stay English.
# www/ above resetToolbarHtml belongs to part A, so that scan waits until it is clean.

cjk_re <- "[㐀-鿿]"

source_root <- function() {
  normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
}

cjk_hits <- function(paths) {
  bad <- character()
  for (path in paths) {
    if (!file.exists(path) || dir.exists(path)) next
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    hit <- grep(cjk_re, lines, perl = TRUE)
    if (length(hit)) {
      bad <- c(bad, paste0(path, ":", paste(hit, collapse = ",")))
    }
  }
  bad
}

cjk_hits_lines <- function(lines, label) {
  hit <- grep(cjk_re, lines, perl = TRUE)
  if (!length(hit)) return(character())
  paste0(label, ":", paste(hit, collapse = ","))
}

test_that("R, run.R, and templates have no CJK", {
  root <- source_root()
  paths <- c(
    list.files(file.path(root, "R"), pattern = "[.][Rr]$", full.names = TRUE),
    list.files(file.path(root, "templates"), full.names = TRUE, recursive = TRUE),
    file.path(root, "run.R")
  )
  bad <- cjk_hits(paths[file.exists(paths)])
  expect_identical(bad, character())
})

test_that("app.js from resetToolbarHtml onward has no CJK", {
  app_path <- file.path(source_root(), "www", "app.js")
  app <- readLines(app_path, warn = FALSE, encoding = "UTF-8")
  marker <- grep("function resetToolbarHtml", app, fixed = TRUE)
  expect_length(marker, 1L)
  bad <- cjk_hits_lines(app[marker[1]:length(app)], app_path)
  expect_identical(bad, character())
})

test_that("client status labels use the three visible names", {
  app_path <- file.path(source_root(), "www", "app.js")
  src <- paste(readLines(app_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(src, 'none: "Not generated", L2: "Generated", L3: "Data matched"', fixed = TRUE)
  expect_match(src, '"Not generated"', fixed = TRUE)
  expect_match(src, '"Generated"', fixed = TRUE)
  expect_match(src, '"Data matched"', fixed = TRUE)
  expect_false(grepl("User confirmed|Not validated|Not L3|Validate <", src, perl = TRUE))
  expect_false(grepl('data-act="validate"|/api/node/validate', src, fixed = TRUE))
})

test_that("www has no CJK", {
  root <- source_root()
  www <- list.files(
    file.path(root, "www"),
    pattern = "[.](js|html|css)$",
    full.names = TRUE,
    recursive = TRUE
  )
  bad <- cjk_hits(www)
  expect_identical(bad, character())
})

test_that("QC is hidden behind a reversible switch and old links fall back to code", {
  src <- paste(readLines(file.path(source_root(), "www", "app.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(src, "const SHOW_QC_TAB = false;", fixed = TRUE)
  expect_match(src, '{ id: "qc", label: "QC" }', fixed = TRUE)
  expect_match(src, 'return SHOW_QC_TAB ? "qc" : "code";', fixed = TRUE)
  expect_match(src, 'tab.id !== "qc"', fixed = TRUE)
  expect_match(src, 'data-act="qc-pick"', fixed = TRUE)
  expect_match(src, "View differing rows in Mismatches", fixed = TRUE)
  expect_match(src, "SHOW_QC_TAB && S && S.last_error", fixed = TRUE)
  expect_false(grepl('qc-report-dl', src, fixed = TRUE))
  expect_false(grepl("Download report", src, fixed = TRUE))
})

test_that("the spec dag shows a key lane and does not store that connector", {
  src <- paste(readLines(file.path(source_root(), "www", "app.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(src, "Every variable is added to the key rows; keys must be Data matched first", fixed = TRUE)
  expect_match(src, ">Keys</text>", fixed = TRUE)
  expect_match(src, 'class="sw key">Key</span>', fixed = TRUE)
  expect_false(grepl("graph.edges.push", src, fixed = TRUE))
  expect_match(src, "nothing here is written back to graph.json", fixed = TRUE)
})

test_that("the Spec bottom row contains only its table", {
  src <- paste(readLines(file.path(source_root(), "www", "app.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(src, 'return pane("spec", "Spec", specExtra, specTableHtml());', fixed = TRUE)
  expect_false(grepl("Check report", src, fixed = TRUE))
  expect_false(grepl("dataset expert", src, ignore.case = TRUE))
})

test_that("Variable DAG context menus review lineage without edit actions", {
  src <- paste(readLines(file.path(source_root(), "www", "app.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(src, "Review this node", fixed = TRUE)
  expect_match(src, "Show all", fixed = TRUE)
  expect_match(src, "/api/node/lineage", fixed = TRUE)
  expect_false(grepl("Connect to…", src, fixed = TRUE))
  expect_false(grepl("Connect from…", src, fixed = TRUE))
  expect_false(grepl("Change source…", src, fixed = TRUE))
  expect_false(grepl("Change target…", src, fixed = TRUE))
  expect_false(grepl("dagPick", src, fixed = TRUE))
})
