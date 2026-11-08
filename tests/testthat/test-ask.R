# Ask / Explain for one side's R expert.

source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
for (f in c("utils.R", "deps.R", "config.R", "dag.R", "spec.R", "project.R",
            "catalog.R", "sides.R", "viewer.R", "inspect.R", "server.R")) {
  sys.source(file.path(app_root, "R", f), envir = globalenv())
}

explain_q <- paste(
  "Explain in plain language what this code does, what the log shows,",
  "and why it failed (if it failed). Suggest what to tell the model to fix it."
)

isolate_ask_home <- function() {
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

new_ask_app <- function(proj) create_app(app_root, proj, 18837L, "ask-token", load_config())

plant_side <- function(proj) {
  dir.create(file.path(proj, "spec"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(proj, "data"), recursive = TRUE, showWarnings = FALSE)
  writeLines(paste(c(
    "Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Unique Subject,Char,20,1,DM.USUBJID,Copy DM.USUBJID",
    "ADSL,STUDYID,Study,Char,20,,DM.STUDYID,UNRELATED_DERIVATION_SENTINEL",
    "ADSL,TRTSDT,Start,Char,20,,DM.RFSTDTC,DM.RFSTDTC"
  ), collapse = "\n"), file.path(proj, "spec", "variable.csv"))
  file.copy(file.path(app_root, "tests", "fixtures", "dm.xpt"), file.path(proj, "data", "dm.xpt"), overwrite = TRUE)
  spec <- load_project_spec(proj)
  g <- make_graph("ADSL", list(gn("ADSL.USUBJID"), gn("ADSL.STUDYID"), gn("ADSL.TRTSDT")),
                  edges = list(ge("ADSL.USUBJID", "ADSL.TRTSDT")))
  write_text("# parent of the target\nPRIMARY_OTHER_CODE\n",
             file.path(side_run_dir(proj, "ADSL", "ADSL.USUBJID", "primary"), "code.R"))
  write_text("VALIDATION_OTHER_CODE\n",
             file.path(side_run_dir(proj, "ADSL", "ADSL.USUBJID", "validation"), "code.R"))
  write_text("UNRELATED_PRIMARY_CODE\n",
             file.path(side_run_dir(proj, "ADSL", "ADSL.STUDYID", "primary"), "code.R"))
  write_text("PRIMARY_CODE_SENTINEL\nsrc[[\"DM\"]]\n",
             file.path(side_run_dir(proj, "ADSL", "ADSL.TRTSDT", "primary"), "code.R"))
  write_text("VALIDATION_CODE_SENTINEL\n",
             file.path(side_run_dir(proj, "ADSL", "ADSL.TRTSDT", "validation"), "code.R"))
  log_lines <- c(
    "Error: OLD_ERROR_SHOULD_BE_CUT",
    rep("step", 80),
    "Error: MID_ERROR_NOT_LAST",
    rep("step", 140),
    "Error: object 'AGEU' not found"
  )
  write_text(paste(log_lines, collapse = "\n"),
             file.path(side_run_dir(proj, "ADSL", "ADSL.TRTSDT", "primary"), "log.txt"))
  write_text("Error: VALIDATION_LOG_SENTINEL should not appear\n",
             file.path(side_run_dir(proj, "ADSL", "ADSL.TRTSDT", "validation"), "log.txt"))
  write_json(list(list(at = "t", text = "PRIMARY_NOTE keep RFSTDTC")),
             file.path(side_run_dir(proj, "ADSL", "ADSL.TRTSDT", "primary"), "notes.json"))
  write_json(list(list(at = "t", text = "VALIDATION_NOTE_SENTINEL")),
             file.path(side_run_dir(proj, "ADSL", "ADSL.TRTSDT", "validation"), "notes.json"))
  saveRDS(data.frame(USUBJID = "S1", TRTSDT = "VALIDATION_RESULT_SENTINEL", stringsAsFactors = FALSE),
          file.path(side_run_dir(proj, "ADSL", "ADSL.TRTSDT", "validation"), "out.rds"))
  primary_work <- work_path(proj, "ADSL", "primary")
  dir.create(dirname(primary_work), recursive = TRUE, showWarnings = FALSE)
  saveRDS(data.frame(USUBJID = c("S1", "S2"), AGE = c(40, 41), NOTE = c("PRIMARY_CELL_SENTINEL", "x"),
                     stringsAsFactors = FALSE),
          primary_work)
  saveRDS(data.frame(SECRET_COL = "hidden", stringsAsFactors = FALSE),
          work_path(proj, "ADSL", "validation"))
  dir.create(file.path(proj, ".dualdag", "state"), recursive = TRUE)
  write_json(list(
    version = 1L,
    nodes = list(ADSL = list(
      ADSL.TRTSDT = list(
        level = "L1",
        problem = list(stage = "run", message = "VALIDATION_PROBLEM_SENTINEL"),
        sides = list(
          primary = list(ok = FALSE, error = "Error: object 'AGEU' not found"),
          validation = list(ok = FALSE, error = "Error: VALIDATION_LOG_SENTINEL should not appear")
        )
      )
    ))
  ), file.path(proj, ".dualdag", "state", "state.json"))
  write_json(list(list(
    at = "2026-10-30T00:00:00Z", node = "ADSL.TRTSDT", kind = "ask",
    question = "earlier", answer = "PRIMARY_THREAD_SENTINEL", suggestion = ""
  )), expert_path(proj, "ADSL", "primary"))
  write_json(list(list(
    at = "2026-10-30T00:00:00Z", node = "ADSL.TRTSDT", kind = "ask",
    question = "earlier", answer = "VALIDATION_THREAD_SENTINEL", suggestion = ""
  )), expert_path(proj, "ADSL", "validation"))
  list(spec = spec, graph = g)
}

forbid <- function(text) {
  for (bad in c(
    "VALIDATION_CODE_SENTINEL", "VALIDATION_OTHER_CODE", "VALIDATION_LOG_SENTINEL",
    "VALIDATION_NOTE_SENTINEL", "VALIDATION_RESULT_SENTINEL", "VALIDATION_THREAD_SENTINEL",
    "VALIDATION_PROBLEM_SENTINEL", "UNRELATED_PRIMARY_CODE", "UNRELATED_DERIVATION_SENTINEL",
    "PRIMARY_CELL_SENTINEL", "SECRET_COL", "OLD_ERROR_SHOULD_BE_CUT"
  )) {
    expect_false(grepl(bad, text, fixed = TRUE))
  }
}

test_that("ask and generate prompts stay on one side's chain", {
  proj <- temp_project()
  planted <- plant_side(proj)
  user <- ask_user_prompt("primary", planted$spec, planted$graph, "ADSL.TRTSDT", explain_q, project = proj)
  sys <- ask_system_prompt("primary", "ADSL")
  expect_match(sys, "TASK: ask", fixed = TRUE)
  expect_match(sys, "You are the primary-side R expert for dataset ADSL", fixed = TRUE)
  expect_false(grepl("DATASET_SPEC", paste(sys, user), fixed = TRUE))
  expect_false(grepl("PRIMARY_OTHER_CODE", user, fixed = TRUE))
  expect_match(user, "PRIMARY_CODE_SENTINEL", fixed = TRUE)
  expect_match(user, "PRIMARY_NOTE keep RFSTDTC", fixed = TRUE)
  expect_match(user, "PRIMARY_THREAD_SENTINEL", fixed = TRUE)
  expect_match(user, "Error: object 'AGEU' not found", fixed = TRUE)
  expect_match(user, "DM.RFSTDTC", fixed = TRUE)
  expect_match(user, "USUBJID", fixed = TRUE)
  expect_match(user, "AGE", fixed = TRUE)
  forbid(paste(sys, user))
  parsed <- parse_json_reply(fake_ask_reply(sys, user))
  expect_match(parsed$answer, "Error: object 'AGEU' not found", fixed = TRUE)
  expect_match(parsed$suggestion, "Error: object 'AGEU' not found", fixed = TRUE)
  expect_false(grepl("OLD_ERROR_SHOULD_BE_CUT", parsed$answer, fixed = TRUE))
  expect_false(grepl("MID_ERROR_NOT_LAST", parsed$suggestion, fixed = TRUE))
  expect_false(grepl("VALIDATION_", parsed$answer, fixed = TRUE))

  gen_sys <- side_system_prompt("primary", planted$spec, "ADSL")
  gen_user <- side_user_prompt(
    "primary", planted$spec, planted$graph, "ADSL.TRTSDT", "USUBJID",
    project = proj,
    current_code = "PRIMARY_CODE_SENTINEL\n",
    notes = list(list(text = "PRIMARY_NOTE keep RFSTDTC"))
  )
  expect_match(gen_sys, "You are the primary-side R expert for dataset ADSL", fixed = TRUE)
  expect_false(grepl("PRIMARY_OTHER_CODE", gen_user, fixed = TRUE))
  expect_match(gen_user, "PRIMARY_THREAD_SENTINEL", fixed = TRUE)
  expect_match(gen_user, "PRIMARY_CODE_SENTINEL", fixed = TRUE)
  expect_false(grepl("DATASET_SPEC", gen_user, fixed = TRUE))
  forbid(paste(gen_sys, gen_user))
})

test_that("a 100-variable spec keeps the generate prompt on the chain and under 120k", {
  vars <- list(
    V("ADSL", "USUBJID", label = "Subject", key = 1, source = "DM.USUBJID",
      derivation = "KEY_DERIVATION_SENTINEL", row = 2),
    V("ADSL", "PARENT1", label = "Parent 1", source = "DM.AGE",
      derivation = "PARENT1_DERIVATION_SENTINEL", row = 3),
    V("ADSL", "PARENT2", label = "Parent 2", source = "ADSL.PARENT1",
      derivation = "PARENT2_DERIVATION_SENTINEL", row = 4),
    V("ADSL", "TARGET", label = "Target", source = "ADSL.PARENT2",
      derivation = "TARGET_DERIVATION_SENTINEL", row = 5)
  )
  long <- paste(rep("UNRELATED_DERIVATION_BLOCK", 40), collapse = " ")
  for (i in 5:100) {
    vars[[length(vars) + 1L]] <- V(
      "ADSL", sprintf("V%03d", i), label = sprintf("Extra %d", i),
      derivation = paste(long, i), row = i + 1L
    )
  }
  spec <- do.call(spec_of, vars)
  nodes <- lapply(spec$variables$Variable, function(v) gn(paste0("ADSL.", v)))
  g <- make_graph("ADSL", nodes, edges = list(
    ge("ADSL.PARENT1", "ADSL.PARENT2"),
    ge("ADSL.PARENT2", "ADSL.TARGET")
  ))
  sys <- side_system_prompt("primary", spec, "ADSL")
  user <- side_user_prompt("primary", spec, g, "ADSL.TARGET", "PARENT2")
  blob <- paste(sys, user)
  expect_lt(nchar(blob), 120000)
  expect_match(user, "PARENT1_DERIVATION_SENTINEL", fixed = TRUE)
  expect_match(user, "PARENT2_DERIVATION_SENTINEL", fixed = TRUE)
  expect_match(user, "TARGET_DERIVATION_SENTINEL", fixed = TRUE)
  expect_false(grepl("KEY_DERIVATION_SENTINEL", user, fixed = TRUE))
  expect_false(grepl("UNRELATED_DERIVATION_BLOCK", blob, fixed = TRUE))
  expect_false(grepl("V100", user, fixed = TRUE))
})

test_that("ask quotes the log, remembers the thread, and rejects a bad call", {
  box <- isolate_ask_home()
  on.exit(box$restore(), add = TRUE)
  Sys.setenv(DUALDAG_FAKE_LLM = "1")
  proj <- temp_project()
  plant_side(proj)
  p <- open_project(proj)
  save_graph(p, list(
    dataset = "ADSL", nodes = list(
      list(id = "ADSL.USUBJID", var = "USUBJID", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.TRTSDT", var = "TRTSDT", paramcd = NULL, kind = "variable")
    ),
    edges = list(list(from = "ADSL.USUBJID", to = "ADSL.TRTSDT", evidence = "date", origin = "user")),
    groups = list(), external = list(), unresolved = list()
  ))
  A <- new_ask_app(proj)
  bare <- call_app(A, "/api/node/ask", "POST", list(dataset = "ADSL", node = "ADSL.TRTSDT", side = "both", question = "why"))
  expect_equal(bare$status, 400L)
  expect_match(resp_json(bare)$error, "side must be primary or validation", fixed = TRUE)
  empty <- call_app(A, "/api/node/ask", "POST", list(dataset = "ADSL", node = "ADSL.TRTSDT", side = "primary", question = "  "))
  expect_equal(empty$status, 400L)
  expect_match(resp_json(empty)$error, "message is required", fixed = TRUE)
  bad_node <- call_app(A, "/api/node/ask", "POST", list(dataset = "ADSL", node = "nope", side = "primary", question = explain_q))
  expect_equal(bad_node$status, 400L)
  bad_clear <- call_app(A, "/api/expert/clear", "POST", list(dataset = "ADSL", side = "both"))
  expect_equal(bad_clear$status, 400L)

  A$jobs$busy <- list(id = "busy", kind = "generate", dataset = "ADSL", node = "ADSL.TRTSDT")
  denied <- call_app(A, "/api/node/ask", "POST", list(dataset = "ADSL", node = "ADSL.TRTSDT", side = "primary", question = explain_q))
  expect_equal(denied$status, 409L)
  expect_match(resp_json(denied)$error, "already running", fixed = TRUE)
  A$jobs$busy <- NULL

  res <- call_app(A, "/api/node/ask", "POST", list(
    dataset = "ADSL", node = "ADSL.TRTSDT", side = "primary", question = explain_q
  ))
  expect_true(inherits(res, "promise"))
  st <- resp_json(call_app(A, "/api/state"))
  busy <- st$busy
  if (is.list(busy) && !is.null(busy$kind)) busy <- list(busy)
  expect_true(any(vapply(busy, function(j) identical(j$kind, "agent"), TRUE)))
  done <- wait_handler(res)
  expect_equal(done$status, 200L)
  body <- resp_json(done)
  expect_match(body$reply, "explain this side's code", ignore.case = TRUE)
  expect_null(body$change)
  expect_equal(body$model, "fake-llm")
  logged <- latest_llm_log(proj, "ADSL.TRTSDT", "primary")
  expect_equal(logged$task, "chat")
  turns <- expert_read(proj, "ADSL", "primary")
  expect_true(any(vapply(turns, function(t) identical(t$message, explain_q), TRUE)))

  res2 <- wait_handler(call_app(A, "/api/node/ask", "POST", list(
    dataset = "ADSL", node = "ADSL.TRTSDT", side = "primary", question = "What does this code do now?"
  )))
  expect_equal(res2$status, 200L)
  logged2 <- latest_llm_log(proj, "ADSL.TRTSDT", "primary")
  expect_match(logged2$user, explain_q, fixed = TRUE)

  cleared <- call_app(A, "/api/expert/clear", "POST", list(dataset = "ADSL", side = "primary"))
  expect_equal(cleared$status, 200L)
  expect_false(file.exists(expert_path(proj, "ADSL", "primary")))
  expect_true(file.exists(expert_path(proj, "ADSL", "validation")))

  js <- paste(readLines(file.path(app_root, "www", "app.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(js, "Ask or request a code change", fixed = TRUE)
  expect_false(grepl('id="nl-text-', js, fixed = TRUE))
  expect_match(js, "How this page works", fixed = TRUE)
  expect_match(js, "Use the Coder window", fixed = TRUE)
  expect_match(js, 'Explain this code and its log.', fixed = TRUE)
  expect_false(grepl("R agent", js, fixed = TRUE))
  expect_false(grepl("View last prompt", js, fixed = TRUE))
  expect_false(grepl("ask-side", js, fixed = TRUE))
  expect_false(grepl("ask-text-", js, fixed = TRUE))
  expect_match(js, 'agent: "Coder working"', fixed = TRUE)
  expect_match(js, 'btn("explain"', fixed = TRUE)
  expect_false(grepl('btn("dl-log"', js, fixed = TRUE))
  expect_false(grepl("Download log", js, fixed = TRUE))
  expect_match(js, "expert-clear", fixed = TRUE)
  expect_match(js, "openCoderWindow", fixed = TRUE)
})

test_that("one side agent answers, changes only its side, and migrates old ask turns", {
  box <- isolate_ask_home()
  on.exit(box$restore(), add = TRUE)
  Sys.setenv(DUALDAG_FAKE_LLM = "1")
  proj <- temp_project()
  plant_side(proj)
  p <- open_project(proj)
  save_graph(p, list(
    dataset = "ADSL", nodes = list(
      list(id = "ADSL.USUBJID", var = "USUBJID", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.TRTSDT", var = "TRTSDT", paramcd = NULL, kind = "variable")
    ),
    edges = list(list(from = "ADSL.USUBJID", to = "ADSL.TRTSDT", evidence = "date", origin = "user")),
    groups = list(), external = list(), unresolved = list()
  ))
  A <- new_ask_app(proj)
  sdir <- side_run_dir(proj, "ADSL", "ADSL.TRTSDT", "primary")
  other <- side_run_dir(proj, "ADSL", "ADSL.TRTSDT", "validation")
  before_code <- read_text(file.path(sdir, "code.R"))
  other_code <- read_text(file.path(other, "code.R"))
  other_log <- read_text(file.path(other, "log.txt"))
  other_notes <- read_side_notes(proj, "ADSL", "ADSL.TRTSDT", "validation")
  old_turn <- expert_read(proj, "ADSL", "primary")[[1L]]
  expect_equal(old_turn$question, "earlier")
  ask <- wait_handler(call_app(A, "/api/node/agent", "POST", list(
    dataset = "ADSL", node = "ADSL.TRTSDT", side = "primary", message = "Explain this code and its log."
  )))
  expect_equal(ask$status, 200L)
  expect_null(resp_json(ask)$change)
  expect_identical(read_text(file.path(sdir, "code.R")), before_code)
  expect_length(read_side_notes(proj, "ADSL", "ADSL.TRTSDT", "primary"), 1L)
  prompt <- latest_llm_log(proj, "ADSL.TRTSDT", "primary")
  expect_equal(prompt$task, "chat")
  expect_match(prompt$user, "PRIMARY_CODE_SENTINEL", fixed = TRUE)
  expect_match(prompt$user, "PRIMARY_NOTE keep RFSTDTC", fixed = TRUE)
  expect_false(grepl("Error: object 'AGEU' not found", prompt$user, fixed = TRUE))
  expect_match(prompt$user, "PRIMARY_THREAD_SENTINEL", fixed = TRUE)
  forbid(prompt$user)

  changed <- wait_handler(call_app(A, "/api/node/agent", "POST", list(
    dataset = "ADSL", node = "ADSL.TRTSDT", side = "primary", message = "Change the code to use RFSTDTC"
  )))
  expect_equal(changed$status, 200L)
  body <- resp_json(changed)
  expect_true(isTRUE(body$code_updated))
  expect_equal(body$note_index, 2L)
  expect_true(body$compare_status %in% c("match", "mismatch"))
  expect_match(read_side_notes(proj, "ADSL", "ADSL.TRTSDT", "primary")[[2L]]$text, "Change the code", fixed = TRUE)
  expect_false(identical(read_text(file.path(sdir, "code.R")), before_code))
  expect_match(read_text(file.path(sdir, "code.R")), 'cat("rows:", nrow(LINEAGE_ADSL), "\\n")', fixed = TRUE)
  expect_true(file.exists(file.path(sdir, "log.txt")))
  expect_match(read_text(file.path(sdir, "log.txt")), "rows:", fixed = TRUE)
  expect_identical(read_text(file.path(other, "code.R")), other_code)
  expect_identical(read_text(file.path(other, "log.txt")), other_log)
  expect_equal(read_side_notes(proj, "ADSL", "ADSL.TRTSDT", "validation"), other_notes)
  turns <- expert_read(proj, "ADSL", "primary")
  expect_equal(length(turns), 3L)
  expect_true(isTRUE(turns[[3L]]$code_updated))
  expect_true(file.exists(file.path(node_run_dir(proj, "ADSL", "ADSL.TRTSDT"), "compare.json")))
})

test_that("ask without a key is rejected when the fake model is off", {
  box <- isolate_ask_home()
  on.exit(box$restore(), add = TRUE)
  proj <- temp_project()
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  writeLines("Dataset,Variable,Label,Type,Length,Key,Source,Derivation\nADSL,USUBJID,Subject,Char,20,1,DM.USUBJID,copy\n",
             file.path(proj, "spec", "variable.csv"))
  A <- new_ask_app(proj)
  res <- call_app(A, "/api/node/ask", "POST", list(dataset = "ADSL", node = "ADSL.USUBJID", side = "primary", question = "why"))
  expect_equal(res$status, 400L)
  expect_match(resp_json(res)$error, "Choose a model in Settings.", fixed = TRUE)
})
