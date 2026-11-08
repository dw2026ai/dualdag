source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))
source(file.path(testthat::test_path(), "..", "..", "R", "inspect.R"), local = FALSE)
source(file.path(testthat::test_path(), "..", "..", "R", "server.R"), local = FALSE)
if (!exists("save_config", mode = "function")) source(file.path(testthat::test_path(), "..", "..", "R", "config.R"), local = FALSE)
if (!exists("read_spec", mode = "function")) source(file.path(testthat::test_path(), "..", "..", "R", "spec.R"), local = FALSE)
if (!exists("project_dir_of", mode = "function")) source(file.path(testthat::test_path(), "..", "..", "R", "project.R"), local = FALSE)
if (!exists("read_side_notes", mode = "function", inherits = TRUE)) assign("read_side_notes", function(...) list(), envir = .GlobalEnv)

review_test_project <- function() {
  proj <- temp_project()
  dir.create(file.path(proj, "spec"), showWarnings = FALSE)
  writeLines(paste(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Subject,Char,20,1,DM.USUBJID,copy",
    "ADSL,AGE,Age,Num,8,,DM.AGE,copy"), collapse = "\n"),
    file.path(proj, "spec", "variable.csv"))
  haven::write_xpt(data.frame(USUBJID = c("S1", "S2"), AGE = c(20, 40)),
                   file.path(proj, "data", "DM.xpt"))
  save_graph(proj, make_graph("ADSL", list(gn("ADSL.USUBJID"), gn("ADSL.AGE"))))
  proj
}

test_that("expert conversation is isolated to the current variable in every prompt", {
  project <- review_test_project()
  spec <- spec_of(V("ADSL","USUBJID",key=1,source="DM.USUBJID"),
                  V("ADSL","AGE",source="DM.AGE"),
                  V("ADSL","WEIGHTBL",source="DM.AGE"))
  graph <- make_graph("ADSL",list(gn("ADSL.USUBJID"),gn("ADSL.AGE"),gn("ADSL.WEIGHTBL")))
  for (side in c("primary","validation")) {
    expert_append_turn(project,"ADSL","ADSL.AGE",side,list(kind="ask",question="AGE_ONLY_MARKER",answer="AGE_REPLY"))
    expert_append_turn(project,"ADSL","ADSL.WEIGHTBL",side,list(kind="ask",question="WEIGHTBL_ONLY_MARKER",answer="WEIGHTBL_REPLY"))
    path <- expert_path(project,"ADSL",side)
    turns <- expert_read(project,"ADSL",side)
    turns[[length(turns)+1L]] <- list(kind="ask",question="NO_NODE_MARKER",answer="ORPHAN_REPLY")
    write_json(turns,path)
    for (id in c("ADSL.AGE","ADSL.WEIGHTBL")) {
      node <- node_record(id)
      prompts <- list(
        generate=side_user_prompt(side,spec,graph,node,character(),project=project),
        ask=ask_user_prompt(side,spec,graph,node,"Please explain this code",project=project),
        agent=agent_user_prompt(side,spec,graph,node,"Explain this code",project),
        self_review=self_review_prompt(project,"ADSL",node,side,spec,graph))
      own <- if (id=="ADSL.AGE") "AGE_ONLY_MARKER" else "WEIGHTBL_ONLY_MARKER"
      other <- if (id=="ADSL.AGE") "WEIGHTBL_ONLY_MARKER" else "AGE_ONLY_MARKER"
      for (prompt in prompts) {
        expect_match(prompt,own,fixed=TRUE)
        expect_false(grepl(other,prompt,fixed=TRUE))
        expect_false(grepl("NO_NODE_MARKER|ORPHAN_REPLY",prompt,perl=TRUE))
      }
    }
  }
})

with_review_fake <- function(project, expr, cfg = list(self_review = TRUE, self_review_autofix = TRUE)) {
  home <- tempfile("dualdag-review-home-"); dir.create(home)
  with_env("DUALDAG_HOME", home, {
    save_config(modifyList(config_defaults(), cfg))
    with_env("DUALDAG_FAKE_LLM", "1", force(expr))
  })
}

review_log_count <- function(project) {
  files <- list.files(file.path(project, ".dualdag", "llm_log"), pattern = "_self_review_", full.names = TRUE)
  length(files)
}

test_that("review prompt is side-local and joined evidence includes missing targets", {
  proj <- temp_project()
  spec <- spec_of(V("ADSL", "USUBJID", key = 1), V("ADSL", "AGE"),
                  V("ADSL", "TRTSDT", source = "DM.RFSTDTC", derivation = "derive from AGE"))
  id <- "ADSL.TRTSDT"
  graph <- list(dataset = "ADSL", edges = list(list(from = "ADSL.AGE", to = id)))
  dir.create(side_run_dir(proj, "ADSL", id, "primary"), recursive = TRUE)
  dir.create(dirname(work_path(proj, "ADSL", "primary")), recursive = TRUE)
  writeLines("result <- 1", file.path(side_run_dir(proj, "ADSL", id, "primary"), "code.R"))
  writeLines("primary log", file.path(side_run_dir(proj, "ADSL", id, "primary"), "log.txt"))
  dir.create(side_run_dir(proj, "ADSL", id, "validation"), recursive = TRUE)
  writeLines("SECRET_OTHER_SIDE", file.path(side_run_dir(proj, "ADSL", id, "validation"), "code.R"))
  keys <- "USUBJID"
  saveRDS(data.frame(USUBJID = c("S1", "S2", "S3"), AGE = c(20, 30, 40)), work_path(proj, "ADSL", "primary"))
  saveRDS(data.frame(USUBJID = c("S1", "S2", "S3"), TRTSDT = c("2020-01-01", NA, "2020-03-01")),
          file.path(side_run_dir(proj, "ADSL", id, "primary"), "out.rds"))
  evidence <- review_joined_rows(proj, "ADSL", id, "primary", graph, spec)
  expect_match(evidence, "AGE", fixed = TRUE)
  expect_match(evidence, "S2", fixed = TRUE)
  prompt <- self_review_prompt(proj, "ADSL", id, "primary", spec, graph)
  expect_match(prompt, "primary log", fixed = TRUE)
  expect_false(grepl("SECRET_OTHER_SIDE", prompt, fixed = TRUE))
  expect_false(grepl("validation", prompt, fixed = TRUE))
})

test_that("Keys self-review judges named key columns", {
  proj <- temp_project()
  spec <- spec_of(V("ADSL","USUBJID",key=1,source="DM.USUBJID"),V("ADSL","AESEQ",key=2,source="AE.AESEQ"))
  graph <- list(dataset="ADSL",layered=FALSE,nodes=list(
    list(id="ADSL.RECORDS",var="RECORDS",kind="records",paramcd=NULL)),edges=list())
  node <- graph_node(graph,"ADSL.RECORDS")
  prompt <- self_review_prompt(proj,"ADSL",node,"primary",spec,graph)
  expect_match(prompt,"VAR: KEYS",fixed=TRUE)
  expect_match(prompt,"KEYS: USUBJID,AESEQ",fixed=TRUE)
  expect_match(prompt,"KEY_COLUMN_SPEC",fixed=TRUE)
  expect_match(prompt,"one row per record",fixed=TRUE)
  expect_false(grepl("(?:VAR: *RECORDS|\\$RECORDS|RECORDS column)",prompt,perl=TRUE))
})

test_that("review records become stale when code changes and reset removes them", {
  proj <- temp_project()
  id <- "ADSL.AGE"
  dir.create(side_run_dir(proj, "ADSL", id, "primary"), recursive = TRUE)
  code <- file.path(side_run_dir(proj, "ADSL", id, "primary"), "code.R")
  writeLines("result <- 1", code)
  review_write(proj, "ADSL", id, "primary", list(verdict = "expected", code_hash = review_code_hash(proj, "ADSL", id, "primary")))
  expect_false(review_read(proj, "ADSL", id, "primary")$stale)
  writeLines("result <- 2", code)
  expect_true(review_read(proj, "ADSL", id, "primary")$stale)
  review_remove(proj, "ADSL", id, "primary")
  expect_null(review_read(proj, "ADSL", id, "primary"))
})

test_that("review prompt includes sampled source lineage columns", {
  proj <- temp_project()
  dir.create(file.path(proj, "data"), recursive = TRUE, showWarnings = FALSE)
  haven::write_xpt(data.frame(USUBJID = c("S1", "S2"), AGE = c(20, 40)),
                   file.path(proj, "data", "DM.xpt"))
  spec <- spec_of(V("ADSL", "AGE", source = "DM.AGE"))
  prompt <- self_review_prompt(proj, "ADSL", "ADSL.AGE", "primary", spec,
                              list(dataset = "ADSL", nodes = list(), edges = list(), external = list()))
  expect_match(prompt, "SOURCE_LINEAGE_COLUMNS:", fixed = TRUE)
  expect_match(prompt, "DM.AGE", fixed = TRUE)
  expect_match(prompt, "20, 40", fixed = TRUE)
})

test_that("lineage source reads are cached by file during a prompt", {
  proj <- temp_project()
  dir.create(file.path(proj, "data"), recursive = TRUE, showWarnings = FALSE)
  haven::write_xpt(data.frame(A = 1:2, B = 3:4), file.path(proj, "data", "DM.xpt"))
  expect_equal(lineage_source_column(proj, "DM", "A"), 1:2)
  expect_equal(lineage_source_column(proj, "DM", "B"), 3:4)
})

test_that("lineage character samples preserve real spaces without format padding", {
  padded <- lineage_column_line("DM.TEST", c(">80", "100"), "ADSL")
  intentional <- lineage_column_line("DM.TEST", c(">80 ", "100"), "ADSL")
  expect_true(grepl('">80", "100"', padded, fixed = TRUE))
  expect_true(grepl('">80 ", "100"', intentional, fixed = TRUE))
})

test_that("run_side_job caps self-review at one retry and preserves retry history", {
  proj <- review_test_project()
  with_review_fake(proj, {
    got <- run_side_job(proj, "ADSL", "ADSL.AGE", "primary", "FAKE_UNEXPECTED", 60)
    expect_true(got$run$ok)
    expect_equal(review_log_count(proj), 2L)
    expect_true(isTRUE(got$review$retried))
    expect_length(got$review$history, 1L)
  })
})

test_that("run_side_job always reviews and allows one rewrite despite legacy switches", {
  p1 <- review_test_project()
  with_review_fake(p1, {
    got <- run_side_job(p1, "ADSL", "ADSL.AGE", "primary", "FAKE_UNEXPECTED", 60)
    expect_true(got$run$ok); expect_equal(review_log_count(p1), 2L)
    expect_equal(got$review$verdict, "expected"); expect_true(isTRUE(got$review$retried))
  }, cfg = list(self_review = TRUE, self_review_autofix = FALSE))
  p2 <- review_test_project()
  with_review_fake(p2, {
    got <- run_side_job(p2, "ADSL", "ADSL.AGE", "primary", NULL, 60)
    expect_true(got$run$ok); expect_equal(review_log_count(p2), 1L)
    expect_true(is.list(got$review))
  }, cfg = list(self_review = FALSE, self_review_autofix = TRUE))
})

test_that("legacy false self-review settings in config do not disable review", {
  proj <- review_test_project()
  home <- tempfile("dualdag-legacy-review-home-"); dir.create(home)
  with_env("DUALDAG_HOME", home, {
    write_json(list(self_review = FALSE, self_review_autofix = FALSE), file.path(home, "config.json"))
    with_fake_llm({
      got <- run_side_job(proj, "ADSL", "ADSL.AGE", "primary", NULL, 60)
      expect_true(got$run$ok)
      expect_equal(review_log_count(proj), 1L)
      expect_true(is.list(got$review))
    })
  })
})

test_that("config API ignores legacy self-review switches", {
  proj <- review_test_project()
  app <- create_app(normalizePath(file.path(testthat::test_path(), "..", "..")), proj,
                    18730L, "review-config-token", config_defaults())
  raw <- charToRaw(as.character(to_json(list(provider = "openai", self_review = FALSE, self_review_autofix = FALSE))))
  response <- make_handlers(app)$call(list(PATH_INFO = "/api/config", REQUEST_METHOD = "POST",
    QUERY_STRING = "", HTTP_HOST = "127.0.0.1:18730", HTTP_X_DUALDAG_TOKEN = "review-config-token",
    CONTENT_TYPE = "application/json", rook.input = list(read = function() raw)))
  expect_equal(response$status, 200L)
  expect_false("self_review" %in% names(app$cfg))
  expect_false("self_review_autofix" %in% names(app$cfg))
  expect_false("self_review" %in% names(from_json(response$body)$config))
  expect_false("self_review_autofix" %in% names(from_json(response$body)$config))
})

test_that("primary review prompt contains source lines and no other-side material", {
  proj <- review_test_project()
  dir.create(side_run_dir(proj, "ADSL", "ADSL.AGE", "validation"), recursive = TRUE)
  write_text("OTHER_SIDE_CODE_SENTINEL", file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "validation"), "code.R"))
  write_text("OTHER_SIDE_CODE_SENTINEL", file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "validation"), "code.R"))
  write_text("OTHER_SIDE_LOG_SENTINEL", file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "validation"), "log.txt"))
  append_side_note(proj, "ADSL", "ADSL.AGE", "validation", "OTHER_SIDE_NOTE_SENTINEL")
  write_text("OTHER_SIDE_CARD_SENTINEL", card_path(proj, "ADSL", "ADSL.AGE", "validation"))
  prompt <- self_review_prompt(proj, "ADSL", "ADSL.AGE", "primary", load_project_spec(proj), load_graph(proj, "ADSL"))
  expect_match(prompt, "DM.AGE", fixed = TRUE)
  expect_match(prompt, "20, 40", fixed = TRUE)
  expect_false(grepl("OTHER_SIDE_", prompt, fixed = TRUE))
})

test_that("compare remains usable after every review verdict", {
  for (verdict in c("expected", "unexpected", "uncertain")) {
    proj <- review_test_project()
    original_reply <- get("fake_self_review_reply", envir = .GlobalEnv)
    assign("fake_self_review_reply", function(system, user) json_text(list(verdict = verdict, reasoning = "test verdict", issues = list(), spec_question = if (verdict == "uncertain") "clarify" else NULL)), envir = .GlobalEnv)
    with_review_fake(proj, {
      got <- run_side_job(proj, "ADSL", "ADSL.AGE", "primary", NULL, 60)
      expect_true(got$run$ok)
      write_result(proj, "ADSL", "ADSL.AGE", "validation", readRDS(got$run$out_path))
      cmp <- compare_node(proj, "ADSL", "ADSL.AGE", "USUBJID")
      expect_true(cmp$match)
    }, cfg = list(self_review = TRUE, self_review_autofix = FALSE))
    assign("fake_self_review_reply", original_reply, envir = .GlobalEnv)
  }
})

test_that("a failed retry restores the first successful side artifacts", {
  proj <- review_test_project()
  before <- new.env(parent = emptyenv())
  original_generate <- get("generate_side", envir = .GlobalEnv)
  assign("generate_side", function(..., self_review = NULL) {
    if (!is.null(self_review)) {
      sdir <- side_run_dir(proj, "ADSL", "ADSL.AGE", "primary")
      paths <- c(file.path(sdir, "code.R"), file.path(sdir, "out.rds"),
        work_path(proj, "ADSL", "primary"), card_path(proj, "ADSL", "ADSL.AGE", "primary"))
      before$hashes <- tools::md5sum(paths)
      stop("retry generation failed")
    }
    original_generate(...)
  }, envir = .GlobalEnv)
  on.exit(assign("generate_side", original_generate, envir = .GlobalEnv), add = TRUE)
  with_review_fake(proj, {
    got <- run_side_job(proj, "ADSL", "ADSL.AGE", "primary", "FAKE_UNEXPECTED", 60)
    sdir <- side_run_dir(proj, "ADSL", "ADSL.AGE", "primary")
    expect_true(got$run$ok)
    expect_true(isTRUE(got$review$retried))
    expect_length(got$review$history, 1L)
    expect_true(file.exists(file.path(sdir, "code.R")))
    expect_true(file.exists(file.path(sdir, "out.rds")))
    expect_true(file.exists(work_path(proj, "ADSL", "primary")))
    expect_true(file.exists(card_path(proj, "ADSL", "ADSL.AGE", "primary")))
    paths <- c(file.path(sdir, "code.R"), file.path(sdir, "out.rds"),
      work_path(proj, "ADSL", "primary"), card_path(proj, "ADSL", "ADSL.AGE", "primary"))
    expect_identical(tools::md5sum(paths), before$hashes)
  })
})

test_that("a successful retry can retain an unexpected verdict", {
  proj <- review_test_project()
  original_reply <- get("fake_self_review_reply", envir = .GlobalEnv)
  original_generate <- get("generate_side", envir = .GlobalEnv)
  assign("fake_self_review_reply", function(system, user) {
    json_text(list(verdict = "unexpected", reasoning = "forced unexpected", issues = list("forced"), spec_question = NULL))
  }, envir = .GlobalEnv)
  on.exit(assign("fake_self_review_reply", original_reply, envir = .GlobalEnv), add = TRUE)
  assign("generate_side", function(..., self_review = NULL) {
    generated <- original_generate(...)
    if (!is.null(self_review)) suppressWarnings(generated$code <- 'result <- src$DM[, c("USUBJID", "AGE"), drop = FALSE]\nresult$AGE <- result$AGE + 1')
    generated
  }, envir = .GlobalEnv)
  on.exit(assign("generate_side", original_generate, envir = .GlobalEnv), add = TRUE)
  with_review_fake(proj, {
    got <- run_side_job(proj, "ADSL", "ADSL.AGE", "primary", "FAKE_UNEXPECTED", 60)
    expect_true(got$run$ok)
    expect_equal(got$review$verdict, "unexpected")
    expect_true(isTRUE(got$review$retried))
    expect_match(paste(readLines(file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "primary"), "code.R"), warn = FALSE), collapse = "\n"), "result$AGE + 1", fixed = TRUE)
  })
})

test_that("primary uncertain review preserves code and stores the spec question", {
  proj <- review_test_project()
  original_reply <- get("fake_self_review_reply", envir = .GlobalEnv)
  assign("fake_self_review_reply", function(system, user) json_text(list(verdict = "uncertain", reasoning = "unclear spec", issues = list(), spec_question = "Is AGE intentional?")), envir = .GlobalEnv)
  on.exit(assign("fake_self_review_reply", original_reply, envir = .GlobalEnv), add = TRUE)
  with_review_fake(proj, {
    got <- run_side_job(proj, "ADSL", "ADSL.AGE", "primary", NULL, 60)
    expect_true(got$run$ok)
    expect_equal(got$review$verdict, "uncertain")
    expect_equal(got$review$spec_question, "Is AGE intentional?")
    expect_true(file.exists(file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "primary"), "code.R")))
  })
})

test_that("missing expected primary columns stop without retry while validation logs and continues", {
  for (side in c("primary", "validation")) {
    proj <- review_test_project()
    writeLines(paste(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
      "ADSL,USUBJID,Subject,Char,20,1,DM.USUBJID,copy",
      "ADSL,MISSING,Missing,Num,8,,DM.AGE,copy",
      "ADSL,AGE,Age,Num,8,,DM.AGE,copy"), collapse = "\n"),
      file.path(proj, "spec", "variable.csv"))
    save_graph(proj, make_graph("ADSL", list(gn("ADSL.USUBJID"), gn("ADSL.MISSING"), gn("ADSL.AGE")),
      list(ge("ADSL.MISSING", "ADSL.AGE"))))
    original_generate <- get("generate_side", envir = .GlobalEnv)
    state <- new.env(parent = emptyenv()); state$generated <- 0L
    assign("generate_side", function(...) { state$generated <- state$generated + 1L; original_generate(...) }, envir = .GlobalEnv)
    with_review_fake(proj, {
      got <- run_side_job(proj, "ADSL", "ADSL.AGE", side, NULL, 60)
      log_path <- file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", side), "log.txt")
      log <- read_text(log_path)
      expect_equal(state$generated, 1L)
      expect_match(log, "inherited rows:", fixed = TRUE)
      expect_match(log, "inherited columns:", fixed = TRUE)
      if (identical(side, "primary")) {
        expect_false(got$run$ok)
        expect_match(got$run$error, "DUALDAG_MISSING_COLUMN: Missing column: MISSING is not in LINEAGE_ADSL or the provided data.", fixed = TRUE)
        expect_match(log, "DUALDAG_MISSING_COLUMN: Missing column: MISSING is not in LINEAGE_ADSL or the provided data.", fixed = TRUE)
        expect_null(got$review)
      } else {
        expect_true(got$run$ok)
        expect_match(log, "missing expected columns: MISSING", fixed = TRUE)
        expect_match(self_review_prompt(proj, "ADSL", "ADSL.AGE", "validation",
          load_project_spec(proj), load_graph(proj, "ADSL")), "inherited columns:", fixed = TRUE)
      }
    })
    assign("generate_side", original_generate, envir = .GlobalEnv)
  }
})

test_that("R-agent changes enter run_side_job and self-review", {
  proj <- review_test_project()
  with_review_fake(proj, {
    got <- agent_job(proj, "ADSL", "ADSL.AGE", "primary", "change the age code", 60)
    expect_true(got$run$run$ok)
    expect_equal(review_log_count(proj), 1L)
  })
})
