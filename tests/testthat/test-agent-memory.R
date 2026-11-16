test_that("agent memory index and files stay complete and public-safe", {
  root <- testthat::test_path("..", "..", "agent-memory")
  index <- readLines(file.path(root, "INDEX.md"), warn = FALSE)
  expect_lt(length(index), 60L)
  listed <- sub("^- \\[.*\\]\\(([^)]+)\\) — .*$", "\\1", index)
  expect_true(all(grepl("^- \\[.+\\]\\([^)]+\\) — .+$", index)))
  files <- list.files(root, pattern = "\\.md$", full.names = FALSE)
  expect_setequal(listed, setdiff(files, "INDEX.md"))
  expect_true(all(file.exists(file.path(root, listed))))

  content <- unlist(lapply(file.path(root, files), readLines, warn = FALSE), use.names = FALSE)
  forbidden <- c("/home/", "dualdag-private", "Linear", "Orca", "api_key")
  for (term in forbidden) expect_false(any(grepl(term, content, fixed = TRUE)), info = term)
})
