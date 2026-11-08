ui_trim_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
ui_trim_app <- paste(readLines(file.path(ui_trim_root, "www", "app.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")

between_ui_functions <- function(text, start, end) {
  a <- regexpr(start, text, perl = TRUE)[[1]]
  b <- regexpr(end, text, perl = TRUE)[[1]]
  if (a < 1L || b < a) return("")
  substring(text, a, b - 1L)
}

test_that("settings has only the four remaining cards and fills the grid", {
  settings <- between_ui_functions(ui_trim_app, "function settingsHtml\\(\\)", "function datasetFormatSettingsHtml\\(")
  expect_match(settings, 'setCard("Spec"', fixed = TRUE)
  expect_match(settings, 'setCard("Analysis dataset formats"', fixed = TRUE)
  expect_match(settings, 'setCard("Data xpt"', fixed = TRUE)
  expect_match(settings, 'setCard("Model"', fixed = TRUE)
  expect_false(any(vapply(c('setCard("R packages"', 'setCard("Comparison"', 'setCard("Self-review"'),
                           grepl, logical(1), x = settings, fixed = TRUE)))
  css <- paste(readLines(file.path(ui_trim_root, "www", "style.css"), warn = FALSE), collapse = "\n")
  expect_match(css, ".tab-settings .model-card { grid-column: auto; }", fixed = TRUE)
  expect_match(css, ".tab-settings .settings-grid { grid-template-columns: minmax(0, 1fr); }", fixed = TRUE)
})

test_that("mismatch lists contain only compare failures and use a single empty state", {
  differs <- between_ui_functions(ui_trim_app, "function mismatchNodes\\(\\)", "function diffPaneItems\\(")
  list_html <- between_ui_functions(ui_trim_app, "function diffListHtml\\(\\)", "function qcListHtml\\(")
  expect_match(differs, 'st.problem.stage === "compare"', fixed = TRUE)
  expect_match(list_html, "const differs = mismatchNodes()", fixed = TRUE)
  expect_false(any(vapply(c('group("Match"', 'group("No compare yet"', '"No compare"'),
                           grepl, logical(1), x = list_html, fixed = TRUE)))
  expect_match(ui_trim_app, '<div class="empty">No mismatches.</div>', fixed = TRUE)
  expect_match(ui_trim_app, "if (!mismatchNodes().length) body =", fixed = TRUE)
})

test_that("a stale URL node is replaced with a listed mismatch and right-side selection is strict", {
  selection <- between_ui_functions(ui_trim_app, "function syncMismatchSelection\\(", "function diffPaneItems\\(")
  load <- between_ui_functions(ui_trim_app, "async function loadDiff\\(", "async function refresh\\(")
  view <- between_ui_functions(ui_trim_app, "function diffHtml\\(\\)", "function viewerHtml\\(")
  expect_match(selection, "ids.indexOf(candidates[i])", fixed = TRUE)
  expect_match(selection, "const oldIndex = all.indexOf(previous);", fixed = TRUE)
  expect_match(selection, "(oldIndex + offset) % all.length", fixed = TRUE)
  expect_match(selection, "if (!selected) selected = ids[0]", fixed = TRUE)
  expect_match(load, "const node = syncMismatchSelection();", fixed = TRUE)
  expect_match(view, "const selected = ui.diff.node || \"\";", fixed = TRUE)
  expect_false(grepl("ui.diff.node || ui.node", view, fixed = TRUE))
})

test_that("layered Keys labels use the full parameter-specific display name", {
  label <- between_ui_functions(ui_trim_app, "function diffVarLabel\\(", "function diffListHtml\\(")
  expect_match(label, "return displayNode(n.id);", fixed = TRUE)
  expect_false(grepl("displayNode(n.id, true)", ui_trim_app, fixed = TRUE))
})

test_that("run queue labels put variables before parameters and omit a synthetic BASE", {
  label <- between_ui_functions(ui_trim_app, "function queueNodeLabel\\(", "function queueResult\\(")
  expect_match(label, 'return layer ? code + " · " + layer : code;', fixed = TRUE)
  expect_false(grepl('"BASE"', label, fixed = TRUE))
  queue <- between_ui_functions(ui_trim_app, "function runQueueHtml\\(\\)", "function curGraph\\(\\)")
  expect_match(queue, 'result.compared && result.compare_ok && result.level !== "L3" ? "Mismatch"', fixed = TRUE)
  expect_match(queue, 'result.level === "L2" ? "Generated"', fixed = TRUE)
})
