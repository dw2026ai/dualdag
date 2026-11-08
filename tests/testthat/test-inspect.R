# Whole-dataset code and the mismatch input table.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
for (f in c("utils.R", "deps.R", "config.R", "dag.R", "spec.R", "project.R",
            "catalog.R", "sides.R", "viewer.R", "inspect.R", "server.R")) {
  sys.source(file.path(app_root, "R", f), envir = globalenv())
}

inspect_spec_csv <- function() {
  paste(c(
    "Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Unique Subject,Char,20,1,DM.USUBJID,copy",
    "ADSL,STUDYID,Study,Char,20,,DM.STUDYID,copy",
    "ADSL,AGE,Age,Num,8,,DM.AGE,copy",
    "ADSL,BRTHDT,Birth date,Char,20,,DM.BRTHDTC,copy"
  ), collapse = "\n")
}

isolate_inspect_home <- function() {
  old <- Sys.getenv(c("DUALDAG_HOME", "DUALDAG_LLM_API_KEY", "DUALDAG_FAKE_LLM"))
  home <- tempfile("dualdag-home-")
  dir.create(home)
  Sys.setenv(DUALDAG_HOME = home, DUALDAG_LLM_API_KEY = "", DUALDAG_FAKE_LLM = "")
  list(restore = function() {
    for (nm in names(old)) {
      if (identical(nm, "DUALDAG_HOME")) restore_test_env(nm, old[[nm]])
      else if (!nzchar(old[[nm]])) Sys.unsetenv(nm) else {
        args <- list(old[[nm]]); names(args) <- nm; do.call(Sys.setenv, args)
      }
    }
  })
}

call_app <- function(A, path, method = "GET", body = NULL, token = A$token) {
  q <- ""
  if (grepl("?", path, fixed = TRUE)) {
    q <- sub("^[^?]*\\?", "", path)
    path <- sub("\\?.*$", "", path)
  }
  raw <- if (is.null(body)) raw() else charToRaw(as.character(to_json(body)))
  req <- list(
    PATH_INFO = path, REQUEST_METHOD = method, QUERY_STRING = q,
    HTTP_HOST = sprintf("127.0.0.1:%d", A$port),
    HTTP_X_DUALDAG_TOKEN = if (is.null(token)) "" else token,
    CONTENT_TYPE = if (identical(method, "POST")) "application/json" else "",
    rook.input = list(read = function() raw)
  )
  make_handlers(A)$call(req)
}

resp_json <- function(res) from_json(res$body)

col_origin <- function(columns, origin, sides = NULL) {
  for (c in columns) {
    if (!identical(c$origin, origin)) next
    if (!is.null(sides) && !identical(c$sides, sides)) next
    return(c)
  }
  NULL
}

col_name <- function(columns, name) {
  for (c in columns) if (identical(c$name, name)) return(c)
  NULL
}

cell_of <- function(body, row, name) {
  names <- vapply(body$columns, function(c) c$name, "")
  row[[match(name, names)]]
}

write_code <- function(project, dataset, node, side, text) {
  dir <- side_run_dir(project, dataset, node, side)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  write_text(text, file.path(dir, "code.R"))
}

new_inspect_app <- function(proj) {
  create_app(app_root, proj, 7979L, "inspect-token", load_config())
}

test_that("dataset code is the stored programs in topological order", {
  box <- isolate_inspect_home()
  on.exit(box$restore(), add = TRUE)
  proj <- tempfile("dualdag-code-")
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  writeLines(inspect_spec_csv(), file.path(proj, "spec", "variable.csv"))
  p <- open_project(proj)
  save_graph(p, list(
    dataset = "ADSL", confirmed = TRUE, confirmed_hash = "x",
    nodes = list(
      list(id = "ADSL.AGE", var = "AGE", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.BRTHDT", var = "BRTHDT", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.STUDYID", var = "STUDYID", paramcd = NULL, kind = "variable")
    ),
    edges = list(
      list(from = "ADSL.STUDYID", to = "ADSL.AGE", evidence = "age after study", origin = "user"),
      list(from = "ADSL.AGE", to = "ADSL.BRTHDT", evidence = "birth after age", origin = "user")
    ),
    groups = list(), external = list(), unresolved = list()
  ))
  set_level <- function(id, level) {
    st <- get_status(p, "ADSL", id)
    st$level <- level
    put_status(p, "ADSL", id, st)
  }
  set_level("ADSL.STUDYID", "L3")
  set_level("ADSL.AGE", "L2")
  set_level("ADSL.BRTHDT", "none")
  # Written downstream-first so a directory listing would put AGE before STUDYID.
  write_code(proj, "ADSL", "ADSL.BRTHDT", "primary", "result <- data.frame(USUBJID = character(), BRTHDT = character())\n")
  write_code(proj, "ADSL", "ADSL.AGE", "primary", "result <- data.frame(USUBJID = src$DM$USUBJID, AGE = src$DM$AGE)\n")
  write_code(proj, "ADSL", "ADSL.STUDYID", "primary", "result <- data.frame(USUBJID = src$DM$USUBJID, STUDYID = src$DM$STUDYID)\n")
  write_code(proj, "ADSL", "ADSL.AGE", "validation", "result <- data.frame(USUBJID = src$DM$USUBJID, AGE = src$DM$AGEU)\n")

  A <- new_inspect_app(proj)
  res <- call_app(A, "/api/dataset/code?dataset=ADSL")
  expect_equal(res$status, 200L)
  body <- resp_json(res)
  expect_equal(body$dataset, "ADSL")
  ids <- vapply(body$primary$nodes, function(n) n$id, "")
  expect_equal(ids, c("ADSL.STUDYID", "ADSL.AGE", "ADSL.BRTHDT"))
  expect_equal(vapply(body$primary$nodes, function(n) n$status, ""), c("L3", "L2", "none"))
  expect_equal(vapply(body$primary$nodes, function(n) n$var, ""), c("STUDYID", "AGE", "BRTHDT"))
  expect_equal(body$order, as.list(ids))
  expect_match(body$primary$script, "# ==== ADSL\\.STUDYID \\(Data matched\\) ====", fixed = FALSE)
  study_at <- regexpr("# ==== ADSL.STUDYID (Data matched) ====", body$primary$script, fixed = TRUE)
  age_at <- regexpr("# ==== ADSL.AGE (Generated) ====", body$primary$script, fixed = TRUE)
  birth_at <- regexpr("# ==== ADSL.BRTHDT (Not generated) ====", body$primary$script, fixed = TRUE)
  expect_true(study_at > 0 && age_at > study_at && birth_at > age_at)
  expect_equal(vapply(body$validation$nodes, function(n) n$id, ""), "ADSL.AGE")
  expect_match(body$validation$script, "AGEU", fixed = TRUE)
  expect_false(grepl("STUDYID", body$validation$script, fixed = TRUE))

  bad <- call_app(A, "/api/dataset/code?dataset=not a dataset")
  expect_equal(bad$status, 400L)
})

test_that("inputs name AGE versus AGEU, keep multi-row domains flat, and fall back to edges", {
  box <- isolate_inspect_home()
  on.exit(box$restore(), add = TRUE)
  proj <- tempfile("dualdag-in-")
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  dir.create(file.path(proj, "data"), recursive = TRUE)
  writeLines(inspect_spec_csv(), file.path(proj, "spec", "variable.csv"))
  haven::write_xpt(
    data.frame(
      USUBJID = c("S1", "S2"),
      STUDYID = c("DEMO", "DEMO"),
      AGE = c(63, 40),
      AGEU = c("YEARS", "YEARS"),
      stringsAsFactors = FALSE
    ),
    file.path(proj, "data", "dm.xpt"), version = 5, name = "DM"
  )
  haven::write_xpt(
    data.frame(
      USUBJID = c("S1", "S1", "S1", "S2"),
      VSSTRESN = c(120, 130, 140, 90),
      stringsAsFactors = FALSE
    ),
    file.path(proj, "data", "vs.xpt"), version = 5, name = "VS"
  )
  p <- open_project(proj)
  save_graph(p, list(
    dataset = "ADSL", confirmed = TRUE, confirmed_hash = "x",
    nodes = list(
      list(id = "ADSL.STUDYID", var = "STUDYID", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.AGE", var = "AGE", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.VSSTRESN", var = "VSSTRESN", paramcd = NULL, kind = "variable")
    ),
    edges = list(list(from = "ADSL.STUDYID", to = "ADSL.AGE", evidence = "study id", origin = "user")),
    groups = list(),
    external = list(list(
      ref = "DM.AGE", domain = "DM", var = "AGE", used_by = list("ADSL.AGE"), evidence = "spec"
    )),
    unresolved = list()
  ))
  write_result <- function(node, side, df) {
    dir <- side_run_dir(proj, "ADSL", node, side)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(as.data.frame(df, stringsAsFactors = FALSE), file.path(dir, "out.rds"))
  }
  write_code(proj, "ADSL", "ADSL.AGE", "primary", paste(
    "result <- data.frame(",
    "  USUBJID = src$DM$USUBJID,",
    "  AGE = src$DM$AGE,",
    "  stringsAsFactors = FALSE",
    ")",
    sep = "\n"
  ))
  write_code(proj, "ADSL", "ADSL.AGE", "validation", paste(
    "result <- data.frame(",
    "  USUBJID = src[[\"DM\"]][[\"USUBJID\"]],",
    "  AGE = src[[\"DM\"]][[\"AGEU\"]],",
    "  stringsAsFactors = FALSE",
    ")",
    sep = "\n"
  ))
  write_result("ADSL.STUDYID", "primary", data.frame(USUBJID = c("S1", "S2"), STUDYID = c("DEMO", "OTHER"), stringsAsFactors = FALSE))
  write_result("ADSL.STUDYID", "validation", data.frame(USUBJID = c("S1", "S2"), STUDYID = c("DEMO", "OTHER"), stringsAsFactors = FALSE))
  write_result("ADSL.AGE", "primary", data.frame(USUBJID = c("S1", "S2"), AGE = c(63, 40), stringsAsFactors = FALSE))
  write_result("ADSL.AGE", "validation", data.frame(USUBJID = c("S1", "S2"), AGE = c("YEARS", "40"), stringsAsFactors = FALSE))

  A <- new_inspect_app(proj)
  viewer_cache_reset()
  res <- call_app(A, "/api/node/inputs?dataset=ADSL&node=ADSL.AGE&page=1&size=50&only_diff=0")
  expect_equal(res$status, 200L)
  body <- resp_json(res)
  expect_false(isTRUE(body$fallback$primary))
  expect_false(isTRUE(body$fallback$validation))
  expect_equal(body$var, "AGE")
  expect_equal(body$keys, list("USUBJID"))
  age <- col_origin(body$columns, "DM.AGE")
  ageu <- col_origin(body$columns, "DM.AGEU")
  expect_false(is.null(age))
  expect_equal(age$sides, "primary")
  expect_equal(age$role, "input")
  expect_equal(ageu$sides, "validation")
  expect_equal(ageu$group, "validation")
  expect_true(is.null(col_origin(body$columns, "DM.USUBJID")))
  ups <- Filter(function(c) identical(c$origin, "upstream.STUDYID"), body$columns)
  expect_true(length(ups) >= 1L)
  expect_equal(body$nrow, 2L)
  expect_equal(cell_of(body, body$rows[[1]], "USUBJID"), "S1")
  expect_equal(cell_of(body, body$rows[[1]], "DM.AGE"), "63")
  expect_equal(cell_of(body, body$rows[[1]], "DM.AGEU"), "YEARS")
  expect_equal(cell_of(body, body$rows[[1]], "primary_AGE"), "63")
  expect_equal(cell_of(body, body$rows[[1]], "validation_AGE"), "YEARS")
  expect_true(isTRUE(cell_of(body, body$rows[[1]], "diff")))
  expect_false(isTRUE(cell_of(body, body$rows[[2]], "diff")))

  diff_only <- resp_json(call_app(A, "/api/node/inputs?dataset=ADSL&node=ADSL.AGE&only_diff=1&size=10"))
  expect_true(isTRUE(diff_only$only_diff))
  expect_equal(diff_only$nrow, 1L)
  expect_equal(cell_of(diff_only, diff_only$rows[[1]], "USUBJID"), "S1")
  reads <- viewer_file_reads()
  again <- call_app(A, "/api/node/inputs?dataset=ADSL&node=ADSL.AGE&only_diff=1&page=1&size=10")
  expect_equal(again$status, 200L)
  expect_equal(viewer_file_reads(), reads)

  paged <- resp_json(call_app(A, "/api/node/inputs?dataset=ADSL&node=ADSL.AGE&only_diff=0&page=2&size=1"))
  expect_equal(paged$page, 2L)
  expect_equal(paged$size, 1L)
  expect_equal(paged$nrow, 2L)
  expect_equal(length(paged$rows), 1L)
  expect_equal(cell_of(paged, paged$rows[[1]], "USUBJID"), "S2")
  filtered <- resp_json(call_app(A, "/api/node/inputs?dataset=ADSL&node=ADSL.AGE&only_diff=0&filter=years"))
  # DM.AGEU is YEARS on every subject, so the substring hits both rows.
  expect_equal(filtered$nrow, 2L)
  narrowed <- resp_json(call_app(A, "/api/node/inputs?dataset=ADSL&node=ADSL.AGE&only_diff=0&filter=63"))
  expect_equal(narrowed$nrow, 1L)
  expect_equal(cell_of(narrowed, narrowed$rows[[1]], "USUBJID"), "S1")

  # Generated programs bind the domain first, then write the result column by
  # the same name as a source column. The output name must not count as an input.
  write_code(proj, "ADSL", "ADSL.AGE", "primary", paste(
    'dom <- src[["DM"]]',
    'if (!"AGE" %in% names(dom)) stop("Missing DM.AGE")',
    'result <- data.frame(dom[, keys, drop = FALSE], stringsAsFactors = FALSE)',
    'result[["AGE"]] <- dom[["AGE"]]',
    'result <- result[, unique(c(keys, "AGE")), drop = FALSE]',
    sep = "\n"
  ))
  write_code(proj, "ADSL", "ADSL.AGE", "validation", paste(
    'dom <- src[["DM"]]',
    'if (!"AGEU" %in% names(dom)) stop("Missing DM.AGEU")',
    'result <- data.frame(dom[, keys, drop = FALSE], stringsAsFactors = FALSE)',
    'result[["AGE"]] <- dom[["AGEU"]]',
    'result <- result[, unique(c(keys, "AGE")), drop = FALSE]',
    sep = "\n"
  ))
  aliased <- resp_json(call_app(A, "/api/node/inputs?dataset=ADSL&node=ADSL.AGE&only_diff=0"))
  expect_false(isTRUE(aliased$fallback$primary))
  expect_false(isTRUE(aliased$fallback$validation))
  expect_equal(col_origin(aliased$columns, "DM.AGE")$sides, "primary")
  expect_equal(col_origin(aliased$columns, "DM.AGE")$group, "primary")
  expect_equal(col_origin(aliased$columns, "DM.AGEU")$sides, "validation")
  expect_equal(col_origin(aliased$columns, "DM.AGEU")$group, "validation")

  write_code(proj, "ADSL", "ADSL.VSSTRESN", "primary", paste(
    "result <- data.frame(",
    "  USUBJID = src$VS$USUBJID,",
    "  VSSTRESN = src$VS$VSSTRESN,",
    "  stringsAsFactors = FALSE",
    ")",
    sep = "\n"
  ))
  write_result("ADSL.VSSTRESN", "primary", data.frame(USUBJID = c("S1", "S2"), VSSTRESN = c(120, 90), stringsAsFactors = FALSE))
  write_result("ADSL.VSSTRESN", "validation", data.frame(USUBJID = c("S1", "S2"), VSSTRESN = c(120, 90), stringsAsFactors = FALSE))
  multi <- resp_json(call_app(A, "/api/node/inputs?dataset=ADSL&node=ADSL.VSSTRESN&only_diff=0"))
  vs <- col_origin(multi$columns, "VS.VSSTRESN")
  expect_true(isTRUE(vs$multi))
  expect_equal(vs$note, "Multiple rows")
  expect_equal(cell_of(multi, multi$rows[[1]], "VS.VSSTRESN"), "120 (n=3)")
  expect_equal(cell_of(multi, multi$rows[[2]], "VS.VSSTRESN"), "90 (n=1)")
  expect_equal(length(multi$rows), 2L)

  write_text("this is not r (((\n", file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "primary"), "code.R"))
  write_text("also broken (((\n", file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "validation"), "code.R"))
  fell <- resp_json(call_app(A, "/api/node/inputs?dataset=ADSL&node=ADSL.AGE&only_diff=0"))
  expect_true(isTRUE(fell$fallback$primary))
  expect_true(isTRUE(fell$fallback$validation))
  expect_true(length(Filter(function(c) identical(c$origin, "upstream.STUDYID"), fell$columns)) >= 1L)
  expect_false(is.null(col_origin(fell$columns, "DM.AGE")))
  expect_true(is.null(col_origin(fell$columns, "DM.AGEU")))
  s1 <- fell$rows[[which(vapply(fell$rows, function(r) cell_of(fell, r, "USUBJID") == "S1", logical(1)))]]
  # Both sides have the STUDYID edge, so the shared origin is split into two columns.
  expect_equal(cell_of(fell, s1, "primary_upstream.STUDYID"), "DEMO")
  expect_equal(cell_of(fell, s1, "validation_upstream.STUDYID"), "DEMO")

  missing <- call_app(A, "/api/node/inputs?dataset=ADSL&node=ADSL.NOPE")
  expect_equal(missing$status, 404L)
  expect_match(resp_json(missing)$error, "Unknown node")
  bad_id <- call_app(A, "/api/node/inputs?dataset=ADSL&node=not-an-id")
  expect_equal(bad_id$status, 400L)
})

test_that("an empty primary out.rds stays null and does not swallow the diff flag", {
  box <- isolate_inspect_home()
  on.exit(box$restore(), add = TRUE)
  proj <- tempfile("dualdag-empty-")
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  writeLines(inspect_spec_csv(), file.path(proj, "spec", "variable.csv"))
  p <- open_project(proj)
  save_graph(p, list(
    dataset = "ADSL", confirmed = TRUE, confirmed_hash = "x",
    nodes = list(
      list(id = "ADSL.STUDYID", var = "STUDYID", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.SITEID", var = "SITEID", paramcd = NULL, kind = "variable")
    ),
    edges = list(list(from = "ADSL.STUDYID", to = "ADSL.SITEID", evidence = "site after study", origin = "user")),
    groups = list(), external = list(), unresolved = list()
  ))
  write_result <- function(node, side, df) {
    dir <- side_run_dir(proj, "ADSL", node, side)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(as.data.frame(df, stringsAsFactors = FALSE), file.path(dir, "out.rds"))
  }
  code <- "result <- data.frame(USUBJID = character(), SITEID = character(), stringsAsFactors = FALSE)\n"
  write_code(proj, "ADSL", "ADSL.SITEID", "primary", code)
  write_code(proj, "ADSL", "ADSL.SITEID", "validation", code)
  # Same shape as the demo primary: columns exist, zero rows.
  write_result("ADSL.SITEID", "primary", data.frame(USUBJID = character(), SITEID = character(), stringsAsFactors = FALSE))
  write_result("ADSL.SITEID", "validation", data.frame(
    USUBJID = c("01-701-1015", "01-701-1023"),
    SITEID = c("701", "701"),
    stringsAsFactors = FALSE
  ))

  A <- new_inspect_app(proj)
  viewer_cache_reset()
  body <- resp_json(call_app(A, "/api/node/inputs?dataset=ADSL&node=ADSL.SITEID&only_diff=0&size=10"))
  names <- vapply(body$columns, function(c) c$name, "")
  expect_equal(names, c(
    "USUBJID", "primary_upstream.STUDYID", "validation_upstream.STUDYID",
    "primary_SITEID", "validation_SITEID", "diff"
  ))
  expect_equal(body$nrow, 2L)
  row <- body$rows[[1]]
  expect_equal(length(row), length(body$columns))
  expect_equal(cell_of(body, row, "USUBJID"), "01-701-1015")
  expect_null(cell_of(body, row, "primary_upstream.STUDYID"))
  expect_null(cell_of(body, row, "validation_upstream.STUDYID"))
  expect_null(cell_of(body, row, "primary_SITEID"))
  expect_equal(cell_of(body, row, "validation_SITEID"), "701")
  expect_true(isTRUE(cell_of(body, row, "diff")))
  primary <- col_name(body$columns, "primary_SITEID")
  validation <- col_name(body$columns, "validation_SITEID")
  expect_false(isTRUE(primary$aligned))
  expect_equal(primary$note, "Empty result")
  expect_true(isTRUE(validation$aligned))
  expect_null(validation$note)
  expect_equal(cell_of(body, body$rows[[2]], "validation_SITEID"), "701")
  expect_null(cell_of(body, body$rows[[2]], "primary_SITEID"))

  unlink(file.path(side_run_dir(proj, "ADSL", "ADSL.SITEID", "primary"), "out.rds"))
  viewer_cache_reset()
  missing <- resp_json(call_app(A, "/api/node/inputs?dataset=ADSL&node=ADSL.SITEID&only_diff=0&size=10"))
  mrow <- missing$rows[[1]]
  expect_equal(length(mrow), length(missing$columns))
  expect_null(cell_of(missing, mrow, "primary_SITEID"))
  expect_equal(cell_of(missing, mrow, "validation_SITEID"), "701")
  expect_true(isTRUE(cell_of(missing, mrow, "diff")))
  expect_equal(col_name(missing$columns, "primary_SITEID")$note, "No result")
  expect_false(isTRUE(col_name(missing$columns, "primary_SITEID")$aligned))
})

test_that("a side instruction is stored only with that side", {
  box <- isolate_inspect_home()
  on.exit(box$restore(), add = TRUE)
  proj <- tempfile("dualdag-notes-")
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  writeLines(inspect_spec_csv(), file.path(proj, "spec", "variable.csv"))
  p <- open_project(proj)
  save_graph(p, list(
    dataset = "ADSL", confirmed = TRUE, confirmed_hash = "x",
    nodes = list(list(id = "ADSL.AGE", var = "AGE", paramcd = NULL, kind = "variable")),
    edges = list(), groups = list(), external = list(), unresolved = list()
  ))
  append_side_note(p, "ADSL", "ADSL.AGE", "validation", "use DM.AGE, not AGEU")
  append_side_note(p, "ADSL", "ADSL.AGE", "primary", "PRIMARY_ONLY_SENTINEL keep DM.AGE")
  vpath <- file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "validation"), "notes.json")
  ppath <- file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "primary"), "notes.json")
  expect_true(file.exists(vpath))
  expect_true(file.exists(ppath))
  expect_false(any(grepl("PRIMARY_ONLY_SENTINEL", readLines(vpath, warn = FALSE), fixed = TRUE)))
  expect_false(any(grepl("AGEU", readLines(ppath, warn = FALSE), fixed = TRUE)))
  # node_detail -> node_blocked uses flat_chr from llm.R. The running app sources it.
  if (!exists("flat_chr", mode = "function")) {
    sys.source(file.path(app_root, "R", "llm.R"), envir = globalenv())
  }
  detail <- node_detail(p, "ADSL", "ADSL.AGE")
  expect_equal(detail$notes$validation[[1]]$text, "use DM.AGE, not AGEU")
  expect_equal(detail$notes$primary[[1]]$text, "PRIMARY_ONLY_SENTINEL keep DM.AGE")
  A <- new_inspect_app(proj)
  res <- call_app(A, "/api/node/detail?dataset=ADSL&node=ADSL.AGE")
  expect_equal(res$status, 200L)
  body <- resp_json(res)
  expect_equal(body$notes$validation[[1]]$text, "use DM.AGE, not AGEU")
  expect_equal(body$notes$primary[[1]]$text, "PRIMARY_ONLY_SENTINEL keep DM.AGE")
})

wait_handler <- function(res, tries = 800L) {
  if (!inherits(res, "promise")) return(res)
  loadNamespace("promises")
  loadNamespace("later")
  out <- new.env(parent = emptyenv())
  promises::then(
    res,
    onFulfilled = function(v) { out$value <- v },
    onRejected = function(e) {
      out$value <- list(status = 500L, headers = list(), body = charToRaw(conditionMessage(e)))
    }
  )
  for (i in seq_len(tries)) {
    later::run_now()
    if (!is.null(out$value)) return(out$value)
    Sys.sleep(0.05)
  }
  stop("The handler did not finish.", call. = FALSE)
}

test_that("generation is allowed when keys and upstream are not Data matched", {
  box <- isolate_inspect_home()
  on.exit(box$restore(), add = TRUE)
  Sys.setenv(DUALDAG_FAKE_LLM = "1", DUALDAG_LLM_API_KEY = "sk-test")
  proj <- tempfile("dualdag-keys-")
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  dir.create(file.path(proj, "data"), recursive = TRUE)
  writeLines(inspect_spec_csv(), file.path(proj, "spec", "variable.csv"))
  file.copy(file.path(app_root, "tests", "fixtures", "dm.xpt"), file.path(proj, "data", "dm.xpt"))
  p <- open_project(proj)
  save_graph(p, list(
    dataset = "ADSL", confirmed = FALSE, confirmed_hash = NULL,
    nodes = list(
      list(id = "ADSL.USUBJID", var = "USUBJID", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.AGE", var = "AGE", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.BRTHDT", var = "BRTHDT", paramcd = NULL, kind = "variable")
    ),
    edges = list(
      list(from = "ADSL.USUBJID", to = "ADSL.AGE", evidence = "age after subject", origin = "user"),
      list(from = "ADSL.AGE", to = "ADSL.BRTHDT", evidence = "birth after age", origin = "user")
    ),
    groups = list(), external = list(), unresolved = list()
  ))
  st <- get_status(p, "ADSL", "ADSL.BRTHDT")
  st$level <- "L3"
  put_status(p, "ADSL", "ADSL.BRTHDT", st)
  dir.create(file.path(node_run_dir(p, "ADSL", "ADSL.BRTHDT"), "primary"), recursive = TRUE)
  writeLines("KEEP_BRTHDT\n", file.path(node_run_dir(p, "ADSL", "ADSL.BRTHDT"), "primary", "code.R"))
  pub <- node_public(p, load_graph(p, "ADSL"), "ADSL", "ADSL.AGE", list())
  expect_true("generate" %in% unlist(pub$actions))
  expect_true(isTRUE(pub$ready))
  A <- new_inspect_app(proj)
  A$jobs$busy <- list(id = "busy", kind = "generate", dataset = "ADSL", node = "ADSL.AGE")
  denied <- call_app(A, "/api/node/generate", "POST", list(dataset = "ADSL", node = "ADSL.AGE", side = "both"))
  expect_equal(denied$status, 409L)
  expect_match(resp_json(denied)$error, "already running", fixed = TRUE)
  A$jobs$busy <- NULL
  res <- wait_handler(call_app(A, "/api/node/generate", "POST", list(dataset = "ADSL", node = "ADSL.AGE", side = "both")))
  expect_equal(res$status, 200L)
  body <- resp_json(res)
  expect_equal(body$level, "L2")
  expect_null(body$problem)
  expect_null(body$upstream_changed)
  expect_equal(get_status(A$project, "ADSL", "ADSL.USUBJID")$level, "none")
  birth <- get_status(A$project, "ADSL", "ADSL.BRTHDT")
  expect_equal(birth$level, "L3")
  expect_true("ADSL.AGE" %in% birth$upstream_changed)
  expect_true(any(grepl("KEEP_BRTHDT", readLines(file.path(node_run_dir(A$project, "ADSL", "ADSL.BRTHDT"), "primary", "code.R"), warn = FALSE), fixed = TRUE)))
})

test_that("reset sets one variable back to Not generated and does not cascade", {
  box <- isolate_inspect_home()
  on.exit(box$restore(), add = TRUE)
  proj <- tempfile("dualdag-reset-")
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  writeLines(inspect_spec_csv(), file.path(proj, "spec", "variable.csv"))
  p <- open_project(proj)
  save_graph(p, list(
    dataset = "ADSL",
    nodes = list(
      list(id = "ADSL.USUBJID", var = "USUBJID", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.AGE", var = "AGE", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.BRTHDT", var = "BRTHDT", paramcd = NULL, kind = "variable")
    ),
    edges = list(
      list(from = "ADSL.USUBJID", to = "ADSL.AGE", evidence = "key", origin = "user"),
      list(from = "ADSL.AGE", to = "ADSL.BRTHDT", evidence = "birth", origin = "user")
    ),
    groups = list(), external = list(), unresolved = list()
  ))
  for (id in c("ADSL.USUBJID", "ADSL.AGE", "ADSL.BRTHDT")) {
    st <- get_status(p, "ADSL", id)
    st$level <- "L3"
    st$problem <- list(stage = "compare", message = "old")
    put_status(p, "ADSL", id, st, save = FALSE)
  }
  write_state(p)
  age_dir <- node_run_dir(p, "ADSL", "ADSL.AGE")
  dir.create(file.path(age_dir, "primary"), recursive = TRUE)
  dir.create(file.path(age_dir, "validation"), recursive = TRUE)
  dir.create(file.path(node_run_dir(p, "ADSL", "ADSL.USUBJID"), "primary"), recursive = TRUE)
  dir.create(file.path(node_run_dir(p, "ADSL", "ADSL.BRTHDT"), "primary"), recursive = TRUE)
  saveRDS(data.frame(USUBJID = c("S1", "S2"), AGE = c(40, 41)), file.path(age_dir, "primary", "out.rds"))
  saveRDS(data.frame(USUBJID = c("S1", "S2"), AGE = c(40, 41)), file.path(age_dir, "validation", "out.rds"))
  saveRDS(data.frame(USUBJID = c("S1", "S2"), stringsAsFactors = FALSE), file.path(node_run_dir(p, "ADSL", "ADSL.USUBJID"), "primary", "out.rds"))
  writeLines("AGE_CODE\n", file.path(age_dir, "primary", "code.R"))
  writeLines("AGE_CODE_V\n", file.path(age_dir, "validation", "code.R"))
  writeLines('[{"at":"t","text":"keep this note"}]', file.path(age_dir, "primary", "notes.json"))
  writeLines("LOG_STAYS\n", file.path(age_dir, "primary", "log.txt"))
  writeLines("{}\n", file.path(age_dir, "compare.json"))
  writeLines("report\n", file.path(age_dir, "qc_report.md"))
  writeLines("BIRTH_CODE\n", file.path(node_run_dir(p, "ADSL", "ADSL.BRTHDT"), "primary", "code.R"))
  saveRDS(data.frame(USUBJID = c("S1", "S2")), file.path(node_run_dir(p, "ADSL", "ADSL.USUBJID"), "primary", "out.rds"))
  refresh_work_dataset(proj, "ADSL")
  expect_true("AGE" %in% names(readRDS(work_path(proj, "ADSL", "primary"))))

  A <- new_inspect_app(proj)
  A$jobs$busy <- list(id = "busy", kind = "generate", dataset = "ADSL", node = "ADSL.AGE")
  denied <- call_app(A, "/api/node/reset", "POST", list(dataset = "ADSL", node = "ADSL.AGE"))
  expect_equal(denied$status, 409L)
  expect_match(resp_json(denied)$error, "already running", fixed = TRUE)
  A$jobs$busy <- NULL

  kept <- call_app(A, "/api/node/reset", "POST", list(dataset = "ADSL", node = "ADSL.AGE", delete_code = FALSE))
  expect_equal(kept$status, 200L)
  body <- resp_json(kept)
  expect_equal(body$level, "none")
  expect_null(body$problem)
  expect_true(file.exists(file.path(age_dir, "primary", "code.R")))
  expect_true(file.exists(file.path(age_dir, "primary", "notes.json")))
  expect_true(file.exists(file.path(age_dir, "primary", "log.txt")))
  expect_false(file.exists(file.path(age_dir, "primary", "out.rds")))
  expect_false(file.exists(file.path(age_dir, "validation", "out.rds")))
  expect_false(file.exists(file.path(age_dir, "compare.json")))
  expect_false(file.exists(file.path(age_dir, "qc_report.md")))
  expect_false("AGE" %in% names(readRDS(work_path(proj, "ADSL", "primary"))))
  birth <- get_status(A$project, "ADSL", "ADSL.BRTHDT")
  expect_equal(birth$level, "L3")
  expect_true("ADSL.AGE" %in% birth$upstream_changed)
  expect_true(any(grepl("BIRTH_CODE", readLines(file.path(node_run_dir(proj, "ADSL", "ADSL.BRTHDT"), "primary", "code.R"), warn = FALSE), fixed = TRUE)))
  expect_equal(get_status(A$project, "ADSL", "ADSL.USUBJID")$level, "L3")
  expect_null(get_status(A$project, "ADSL", "ADSL.USUBJID")$upstream_changed)

  st <- get_status(A$project, "ADSL", "ADSL.AGE")
  st$level <- "L2"
  put_status(A$project, "ADSL", "ADSL.AGE", st)
  gone <- call_app(A, "/api/node/reset", "POST", list(dataset = "ADSL", node = "ADSL.AGE", delete_code = TRUE))
  expect_equal(gone$status, 200L)
  expect_equal(resp_json(gone)$level, "none")
  expect_false(file.exists(file.path(age_dir, "primary", "code.R")))
  expect_false(file.exists(file.path(age_dir, "validation", "code.R")))
  expect_false(file.exists(file.path(age_dir, "primary", "notes.json")))
  expect_true(file.exists(file.path(age_dir, "primary", "log.txt")))
})
