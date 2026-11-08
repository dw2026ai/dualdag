# DAG markdown, lookup rounds, and the graph agent.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
for (f in c("utils.R", "deps.R", "config.R", "llm.R", "dag.R", "spec.R", "project.R",
            "deps_extract.R", "sides.R", "dag_agent.R", "server.R")) {
  sys.source(file.path(app_root, "R", f), envir = globalenv())
}
dualdag_engine_loaded <- TRUE
source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))

dag_call <- function(A, path, method = "GET", body = NULL, token = A$token) {
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

dag_json <- function(res) from_json(res$body)

with_llm_log <- function(expr) {
  old <- getOption("dualdag.llm_log")
  on.exit(options(dualdag.llm_log = old), add = TRUE)
  dir <- tempfile("dag-llm-")
  dir.create(dir)
  options(dualdag.llm_log = list(dir = dir, task = "dag_ask", node = "ADSL", side = "dag"))
  logs <- function() {
    files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
    lapply(files, function(f) {
      jsonlite::fromJSON(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
                         simplifyVector = FALSE)
    })
  }
  list(dir = dir, logs = logs, value = force(expr))
}

adsl_project <- function() {
  p <- open_project(tempfile("dag-agent-"))
  writeLines(c(
    "Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Subject,Char,20,1,DM.USUBJID,Copy DM.USUBJID",
    "ADSL,AGE,Age,Num,8,,DM.AGE,Age at treatment start",
    "ADSL,TRTSDT,Treatment start,Char,20,,DM.RFSTDTC,Date of first treatment",
    "ADSL,CHG,Change,Num,8,,ADSL.TRTSDT,Change from treatment start"
  ), file.path(p$dir, "spec", "variable.csv"))
  g <- make_graph(
    "ADSL",
    nodes = list(gn("ADSL.USUBJID"), gn("ADSL.AGE"), gn("ADSL.TRTSDT"), gn("ADSL.CHG")),
    edges = list(
      ge("ADSL.USUBJID", "ADSL.AGE", "age rows follow the subject"),
      ge("ADSL.TRTSDT", "ADSL.CHG", "change uses treatment start")
    ),
    external = list(list(
      ref = "DM.AGE", domain = "DM", var = "AGE",
      used_by = list("ADSL.AGE"), evidence = "AGE from DM"
    ))
  )
  save_graph(p, g)
  secret <- file.path(p$dir, ".dualdag", "runs", "ADSL", "AGE", "primary")
  dir.create(secret, recursive = TRUE, showWarnings = FALSE)
  writeLines("SIDE_SECRET_CODE_991", file.path(secret, "code.R"))
  writeLines("SIDE_SECRET_CODE_991", file.path(secret, "log.txt"))
  writeLines('{"notes":[{"text":"SIDE_SECRET_CODE_991"}]}', file.path(secret, "notes.json"))
  p
}

graph_txt <- function(p) {
  paste(readLines(file.path(p$state_dir, "graph_ADSL.json"), warn = FALSE), collapse = "\n")
}

md_of <- function(p) {
  paste(readLines(file.path(p$state_dir, "dag_ADSL.md"), warn = FALSE), collapse = "\n")
}

test_that("save_graph writes a status-free dag markdown beside the graph", {
  p <- adsl_project()
  md <- md_of(p)
  expect_match(md, "ADSL.AGE | Age", fixed = TRUE)
  expect_match(md, "ADSL.USUBJID | Subject | key", fixed = TRUE)
  expect_match(md, "parent ADSL.USUBJID: age rows follow the subject", fixed = TRUE)
  expect_match(md, "parent DM.AGE: AGE from DM", fixed = TRUE)
  expect_match(md, "User corrections", fixed = TRUE)
  expect_match(md, "(none)", fixed = TRUE)
  expect_false(grepl("L1|L2|L3|upstream_changed", md))
  expect_true(file.exists(file.path(dirname(graph_path(p, "ADSL")), "dag_ADSL.md")))
})

test_that("lookup returns spec and graph only and ignores unknown ids", {
  p <- adsl_project()
  spec <- read_project_spec(p)
  g <- load_graph(p, "ADSL")
  ids <- c(paste0("ADSL.V", seq_len(12)), "NOT.A.NODE", "ADSL.AGE")
  block <- dag_lookup_block(spec, g, "ADSL", ids)
  expect_match(block, "Lookup capped at 10 ids.", fixed = TRUE)
  expect_match(block, "Unknown ids ignored:", fixed = TRUE)
  scoped <- dag_lookup_block(spec, g, "ADSL", c("ADSL.AGE", "ADSL.USUBJID"), allowed_ids = "ADSL.AGE")
  expect_match(scoped, "Outside this variable's lineage: ADSL.USUBJID", fixed = TRUE)
  expect_match(scoped, "NODE ADSL.AGE", fixed = TRUE)
  expect_false(grepl("NODE ADSL.USUBJID", scoped, fixed = TRUE))
  expect_false(grepl("SIDE_SECRET_CODE_991", block, fixed = TRUE))
  expect_false(grepl("code.R", block, fixed = TRUE))
  expect_false(grepl("L3", block, fixed = TRUE))
  user <- dag_user_prompt(p, "ADSL", spec, g, list(turns = list(), corrections = list()), "What depends on age?")
  expect_false(grepl("SIDE_SECRET_CODE_991", user, fixed = TRUE))
  expect_false(grepl("code.R", user, fixed = TRUE))
  sys <- dag_system_prompt("ADSL")
  expect_match(sys, "You are the dependency-graph agent for dataset ADSL.", fixed = TRUE)
  expect_match(sys, "TASK: dag_ask", fixed = TRUE)
})

test_that("lookup stops after two rounds", {
  p <- adsl_project()
  box <- with_fake_llm(with_llm_log(
    expect_error(dag_ask(list(), p, "ADSL", "lookup forever"), "2")
  ))
  logs <- box$logs()
  expect_gte(length(logs), 3L)
  expect_true(any(vapply(logs, function(x) grepl("TASK: dag_ask", x$system %||% "", fixed = TRUE), logical(1))))
})

test_that("an add-edge question does one lookup and applies the change", {
  p <- adsl_project()
  box <- with_fake_llm(with_llm_log(
    dag_ask(list(), p, "ADSL", "add edge ADSL.USUBJID -> ADSL.TRTSDT")
  ))
  res <- box$value
  logs <- box$logs()
  expect_equal(length(logs), 2L)
  expect_true(any(vapply(logs, function(x) {
    grepl("LOOKUP_ROUND", x$user %||% "", fixed = TRUE) &&
      grepl("Unknown ids ignored", x$user %||% "", fixed = TRUE)
  }, logical(1))))
  expect_equal(res$changes[[1]]$op, "add_edge")
  expect_equal(res$changes[[1]]$from, "ADSL.USUBJID")
  expect_equal(res$changes[[1]]$to, "ADSL.TRTSDT")
  expect_equal(res$usage$calls, 2L)
  expect_equal(tail(dag_agent_read(p, "ADSL")$turns, 1)[[1]]$usage$calls, 2L)
  expect_true(any(vapply(load_graph(p,"ADSL")$edges,function(e)e$from=="ADSL.USUBJID"&&e$to=="ADSL.TRTSDT",TRUE)))
  expect_true(isTRUE(tail(dag_agent_read(p,"ADSL")$turns,1)[[1]]$applied))
})

test_that("apply flags only the target and keeps its level", {
  p <- adsl_project()
  put_status(p, "ADSL", "ADSL.AGE", list(level = "L3"))
  put_status(p, "ADSL", "ADSL.TRTSDT", list(level = "L3"))
  put_status(p, "ADSL", "ADSL.CHG", list(level = "L3"))
  res <- apply_dag_changes(p, "ADSL", list(list(
    op = "add_edge", from = "ADSL.USUBJID", to = "ADSL.TRTSDT", evidence = "user test"
  )))
  expect_true(isTRUE(res$ok))
  expect_true("ADSL.TRTSDT" %in% unlist(res$affected))
  expect_false("ADSL.AGE" %in% unlist(res$affected))
  expect_false("ADSL.CHG" %in% unlist(res$affected))
  to <- get_status(p, "ADSL", "ADSL.TRTSDT")
  age <- get_status(p, "ADSL", "ADSL.AGE")
  chg <- get_status(p, "ADSL", "ADSL.CHG")
  expect_equal(to$level, "L3")
  expect_equal(age$level, "L3")
  expect_equal(chg$level, "L3")
  expect_true("ADSL.USUBJID" %in% to$upstream_changed)
  expect_null(age$upstream_changed)
  expect_null(chg$upstream_changed)
  md <- md_of(p)
  expect_match(md, "parent ADSL.USUBJID:", fixed = TRUE)
  g <- load_graph(p, "ADSL")
  pairs <- vapply(g$edges, function(e) paste(e$from, e$to, sep = "->"), "")
  expect_true("ADSL.USUBJID->ADSL.TRTSDT" %in% pairs)
})

test_that("apply rejects a cycle, an unknown node, and a self loop", {
  p <- adsl_project()
  before <- graph_txt(p)
  expect_error(
    apply_dag_changes(p, "ADSL", list(list(op = "add_edge", from = "ADSL.AGE", to = "ADSL.USUBJID"))),
    "That edge would create a cycle: ADSL.AGE -> ADSL.USUBJID"
  )
  expect_error(
    apply_dag_changes(p, "ADSL", list(list(op = "add_edge", from = "ADSL.NOPE", to = "ADSL.AGE"))),
    "Unknown node: ADSL\\.NOPE"
  )
  expect_error(
    apply_dag_changes(p, "ADSL", list(list(op = "add_edge", from = "ADSL.AGE", to = "ADSL.AGE"))),
    "An edge cannot connect a node to itself \\(ADSL.AGE\\)"
  )
  expect_identical(graph_txt(p), before)
})

test_that("DAG replies apply atomically, edit specs, and undo the complete step", {
  p <- adsl_project(); before_spec <- readBin(file.path(p$dir,"spec","variable.csv"),"raw",n=10000)
  before_graph <- graph_txt(p); put_status(p,"ADSL","ADSL.AGE",list(level="L3"))
  before_status <- get_status(p,"ADSL","ADSL.AGE")
  original <- llm_chat; replies <- list(
    list(text='{"answer":"Which change do you mean?","changes":[],"correction":null}',model="fixture"),
    list(text='{"answer":"Updated the source and dependency.","changes":[{"op":"edit_spec","file":"variable.csv","dataset":"ADSL","key":"AGE","column":"Source","value":"DM.AGE2"},{"op":"add_edge","from":"ADSL.AGE","to":"ADSL.TRTSDT","evidence":"AGE dependency"}],"correction":null}',model="fixture"),
    list(text='{"answer":"Trying invalid change.","changes":[{"op":"remove_edge","from":"ADSL.USUBJID","to":"ADSL.AGE"},{"op":"add_edge","from":"ADSL.MISSING","to":"ADSL.TRTSDT"}],"correction":null}',model="fixture"),
    list(text='{"answer":"The requested node is unknown, so neither operation was applied.","changes":[]}',model="fixture")
  ); ix <- 0L; prompts <- character()
  assign("llm_chat",function(cfg,system,user,...) { ix <<- ix+1L; prompts <<- c(prompts,user); replies[[ix]] },.GlobalEnv)
  on.exit(assign("llm_chat",original,.GlobalEnv),add=TRUE)
  question <- dag_ask(list(),p,"ADSL","Can you fix the DAG?")
  expect_length(question$changes,0L); expect_identical(graph_txt(p),before_graph)
  A <- create_app(app_root,p$dir,7984L,"agent-undo-test",load_config())
  answer <- dag_ask(list(),p,"ADSL","Change AGE source and add the dependency")
  expect_length(answer$card,2L); expect_match(answer$card[[1]]$text,"variable.csv AGE Source",fixed=TRUE)
  expect_equal(get_status(p,"ADSL","ADSL.AGE")$level,"none")
  expect_false(isTRUE(dataset_reextract_required(read_project_spec(p),"ADSL",load_graph(p,"ADSL"))))
  changed_spec <- readBin(file.path(p$dir,"spec","variable.csv"),"raw",n=10000)
  expect_false(identical(changed_spec,before_spec))
  changed_graph <- graph_txt(p)
  refused <- dag_ask(list(),p,"ADSL","do both")
  expect_length(refused$changes,0L); expect_match(refused$answer,"node is unknown",fixed=TRUE)
  expect_match(prompts[[4]],"VALIDATION_REFUSAL",fixed=TRUE); expect_match(prompts[[4]],"Unknown node",fixed=TRUE)
  expect_identical(readBin(file.path(p$dir,"spec","variable.csv"),"raw",n=10000),changed_spec)
  expect_identical(graph_txt(p),changed_graph)
  expect_true(any(vapply(load_graph(p,"ADSL")$edges,function(e)e$from=="ADSL.AGE"&&e$to=="ADSL.TRTSDT",TRUE)))
  snap <- list.dirs(dag_history_dir(p,"ADSL"),recursive=FALSE,full.names=TRUE)
  expect_length(snap,1L)
  check_manifest <- read_json(file.path(snap[[1]],"manifest.json"))
  expect_identical(file_md5(file.path(p$dir,"spec","variable.csv")),check_manifest$md5$variable)
  expect_identical(file_md5(file.path(p$dir,"spec","parameter.csv")),check_manifest$md5$parameter)
  expect_identical(file_md5(graph_path(p,"ADSL")),check_manifest$md5$graph)
  saved_state <- read_json(file.path(snap[[1]],"state.json"))
  expect_identical(saved_state$nodes$ADSL$ADSL.AGE$level,"L3")
  undone <- dag_call(A,"/api/dag/undo","POST",list(dataset="ADSL"))
  expect_equal(undone$status,200L)
  expect_identical(readBin(file.path(p$dir,"spec","variable.csv"),"raw",n=10000),before_spec)
  expect_identical(graph_txt(p),before_graph)
  expect_identical(get_status(A$project,"ADSL","ADSL.AGE")$level,before_status$level)
  applied_turns <- Filter(function(turn)length(turn$card %||% list())>0,dag_agent_read(p,"ADSL")$turns)
  expect_true(isTRUE(tail(applied_turns,1)[[1]]$undone))
})

test_that("DAG agent CLI notice is scoped and replaces the no-tools default", {
  p <- adsl_project(); original <- llm_chat; captured <- NULL
  assign("llm_chat",function(cfg,system,user,...) {
    captured <<- cli_user_prompt(user,"You have no tools and no workspace.")
    list(text='{"answer":"No changes needed.","changes":[],"correction":null}',model="fixture")
  },.GlobalEnv)
  prior <- getOption("dualdag.cli_tool_notice"); options(dualdag.cli_tool_notice="prior notice")
  on.exit({assign("llm_chat",original,.GlobalEnv);options(dualdag.cli_tool_notice=prior)},add=TRUE)
  dag_ask_job(p$dir,"ADSL","What depends on AGE?")
  expect_match(captured,"The app applies the changes listed in your JSON",fixed=TRUE)
  expect_false(grepl("You have no tools and no workspace",captured,fixed=TRUE))
  expect_identical(getOption("dualdag.cli_tool_notice"),"prior notice")
})

test_that("DAG route reloads child state after success and rejection", {
  p <- adsl_project(); put_status(p,"ADSL","ADSL.AGE",list(level="L3"))
  A <- create_app(app_root,p$dir,7984L,"dag-state-test",load_config())
  original_launch <- launch; original_chat <- llm_chat
  old_fake <- Sys.getenv("DUALDAG_FAKE_LLM",unset=NA_character_); Sys.setenv(DUALDAG_FAKE_LLM="1")
  on.exit({assign("launch",original_launch,.GlobalEnv);assign("llm_chat",original_chat,.GlobalEnv);restore_test_env("DUALDAG_FAKE_LLM",old_fake)},add=TRUE)
  assign("llm_chat",function(...) list(text='{"answer":"Updated AGE source.","changes":[{"op":"edit_spec","file":"variable.csv","dataset":"ADSL","key":"AGE","column":"Source","value":"DM.AGE2"}],"correction":null}',model="fixture"),.GlobalEnv)
  assign("launch",function(A,fn_name,args,timeout) promises::promise(function(resolve,reject) {
    tryCatch(resolve(do.call(get(fn_name,envir=.GlobalEnv,mode="function"),args)),error=reject)
  }),.GlobalEnv)
  response_promise <- dag_call(A,"/api/dag/ask","POST",list(dataset="ADSL",message="Change AGE source"))
  box <- new.env(parent=emptyenv()); promises::then(response_promise,onFulfilled=function(v)box$value<-v,onRejected=function(e)box$value<-list(status=502L))
  for(i in seq_len(200L)) { later::run_now(); if(!is.null(box$value))break; Sys.sleep(.025) }
  expect_equal(box$value$status,200L)
  expect_identical(get_status(A$project,"ADSL","ADSL.AGE")$level,"none")

  assign("launch",function(A,fn_name,args,timeout) promises::promise(function(resolve,reject) {
    child<-open_project(args$project_dir); st<-get_status(child,"ADSL","ADSL.AGE"); st$level<-"L2"; put_status(child,"ADSL","ADSL.AGE",st); reject(simpleError("child failed after writing"))
  }),.GlobalEnv)
  rejected <- dag_call(A,"/api/dag/ask","POST",list(dataset="ADSL",message="Trigger child failure"))
  box2 <- new.env(parent=emptyenv()); promises::then(rejected,onFulfilled=function(v)box2$value<-v,onRejected=function(e)box2$value<-list(status=502L))
  for(i in seq_len(200L)) { later::run_now(); if(!is.null(box2$value))break; Sys.sleep(.025) }
  expect_equal(box2$value$status,502L)
  expect_identical(get_status(A$project,"ADSL","ADSL.AGE")$level,"L2")
})

test_that("DAG undo reverses each graph edit as one stack step and preserves statuses changed later", {
  p <- adsl_project()
  original <- graph_txt(p)
  put_status(p, "ADSL", "ADSL.TRTSDT", list(level = "L3"))
  put_status(p, "ADSL", "ADSL.AGE", list(level = "L2"))
  apply_dag_changes(p, "ADSL", list(
    list(op = "remove_edge", from = "ADSL.USUBJID", to = "ADSL.AGE"),
    list(op = "add_edge", from = "ADSL.USUBJID", to = "ADSL.TRTSDT")
  ))
  expect_equal(dag_undo_public(p, "ADSL")$count, 1L)
  ran <- get_status(p, "ADSL", "ADSL.TRTSDT"); ran$level <- "L1"; ran$updated <- "2099-01-01T00:00:00+0000"
  p$state$nodes$ADSL[["ADSL.TRTSDT"]] <- ran
  undone <- apply_dag_changes(p, "ADSL", list(list(op = "set_key", node = "ADSL.AGE", value = TRUE)))
  expect_true(undone$ok)
  apply_dag_changes(p, "ADSL", list(list(op = "set_categories", node = "ADSL.CHG", categories = list(list(value = "x", rule = "test")))))
  A <- create_app(app_root, p$dir, 7984L, "undo-token", load_config())
  newest <- dag_call(A, "/api/dag/undo", "POST", list(dataset = "ADSL"))
  expect_equal(newest$status, 200L)
  expect_equal(dag_json(newest)$undo_count, 2L)
  second <- dag_call(A, "/api/dag/undo", "POST", list(dataset = "ADSL"))
  expect_equal(second$status, 200L)
  expect_equal(unlist(dag_json(second)$restored), "ADSL.AGE")
  first <- dag_call(A, "/api/dag/undo", "POST", list(dataset = "ADSL"))
  expect_equal(first$status, 200L)
  body <- dag_json(first)
  expect_equal(unlist(body$kept), "ADSL.TRTSDT")
  expect_equal(get_status(p, "ADSL", "ADSL.TRTSDT")$level, "none")
  expect_equal(get_status(p, "ADSL", "ADSL.AGE")$level, "L2")
  expect_identical(graph_txt(p), original)
  expect_equal(body$undo_count, 0L)
  expect_equal(dag_call(A, "/api/dag/undo", "POST", list(dataset = "ADSL"))$status, 409L)
})

test_that("DAG undo is rejected while busy, exposes state, caps at twenty, and extraction clears it", {
  p <- adsl_project()
  for (i in 1:21) apply_dag_changes(p, "ADSL", list(list(op = "set_key", node = "ADSL.AGE", value = i %% 2 == 0)))
  expect_equal(dag_undo_public(p, "ADSL")$count, 20L)
  A <- create_app(app_root, p$dir, 7984L, "undo-token", load_config())
  expect_equal(dag_json(dag_call(A, "/api/state"))$datasets$ADSL$dag_undo$count, 20L)
  A$batch <- list(finished = FALSE)
  expect_equal(dag_call(A, "/api/dag/undo", "POST", list(dataset = "ADSL"))$status, 409L)
  expect_equal(dag_call(A, "/api/dag/ask", "POST", list(dataset="ADSL",message="question"))$status,409L)
  expect_equal(dag_call(A,"/api/dag/clear","POST",list(dataset="ADSL"))$status,409L)
  A$batch <- NULL
  A$jobs <- list(j = list(kind = "run"))
  expect_equal(dag_call(A, "/api/dag/undo", "POST", list(dataset = "ADSL"))$status, 409L)
  A$jobs <- list()
  store_extracted(A, "ADSL", load_graph(p, "ADSL"))
  expect_equal(dag_undo_public(p, "ADSL")$count, 0L)
})

test_that("DAG agent Undo refuses later graph edits and run files", {
  make_agent_edit <- function(p) with_fake_llm(dag_ask(list(),p,"ADSL","add edge ADSL.AGE -> ADSL.TRTSDT"))
  p <- adsl_project(); A <- create_app(app_root,p$dir,7984L,"undo-later-graph",load_config()); make_agent_edit(p)
  manual <- dag_call(A,"/api/dag/apply","POST",list(dataset="ADSL",changes=list(list(op="add_edge",from="ADSL.USUBJID",to="ADSL.TRTSDT"))))
  expect_equal(manual$status,200L)
  refused_graph <- dag_call(A,"/api/dag/undo","POST",list(dataset="ADSL"))
  expect_equal(refused_graph$status,409L)
  expect_match(dag_json(refused_graph)$error,"Later changes exist, so this can't be undone.",fixed=TRUE)

  p2 <- adsl_project(); A2 <- create_app(app_root,p2$dir,7984L,"undo-later-run",load_config()); make_agent_edit(p2)
  history <- list.dirs(dag_history_dir(p2,"ADSL"),recursive=FALSE,full.names=TRUE)
  manifest <- read_json(file.path(history[[1]],"manifest.json"))
  run_file <- file.path(p2$dir,".dualdag","runs","ADSL","AGE","primary","later-result.txt")
  dir.create(dirname(run_file),recursive=TRUE,showWarnings=FALSE); writeLines("fixture result",run_file)
  Sys.setFileTime(run_file,as.POSIXct(manifest$time+5,origin="1970-01-01",tz="UTC"))
  refused_run <- dag_call(A2,"/api/dag/undo","POST",list(dataset="ADSL"))
  expect_equal(refused_run$status,409L)
  expect_match(dag_json(refused_run)$error,"Later changes exist, so this can't be undone.",fixed=TRUE)
})

test_that("DAG chat clear is refused while work is busy", {
  p <- adsl_project(); dag_agent_write(p,"ADSL",list(turns=list(list(role="user",text="keep me")),corrections=list()))
  A <- create_app(app_root,p$dir,7984L,"clear-busy",load_config())
  A$jobs <- list(active=list(kind="run"))
  expect_equal(dag_call(A,"/api/dag/clear","POST",list(dataset="ADSL"))$status,409L)
  expect_equal(dag_agent_read(p,"ADSL")$turns[[1]]$text,"keep me")
})

test_that("re-extraction prompt ignores and clears stored graph corrections", {
  p <- adsl_project()
  with_fake_llm(dag_ask(list(), p, "ADSL", "REMEMBER: Never derive AGE from TRTSDT"))
  mem <- dag_agent_read(p, "ADSL")
  texts <- vapply(mem$corrections, function(crr) crr$text %||% "", "")
  expect_true(any(grepl("Never derive AGE from TRTSDT", texts, fixed = TRUE)))
  expect_match(md_of(p), "Never derive AGE from TRTSDT", fixed = TRUE)
  logdir <- tempfile("extract-log-")
  dir.create(logdir)
  old <- getOption("dualdag.llm_log")
  on.exit(options(dualdag.llm_log = old), add = TRUE)
  options(dualdag.llm_log = list(dir = logdir, task = "extract", node = "ADSL", side = "extract"))
  spec <- read_project_spec(p)
  captured <- character()
  old_chat <- get("llm_chat", envir=.GlobalEnv)
  assign("llm_chat", function(cfg, system, user, ...) {
    captured <<- c(captured, system, user)
    list(text=if (grepl("TASK: extract_plan", system, fixed=TRUE)) '{"batches":[{"kind":"simple","rows":["V1"]}]}'
      else if (grepl("TASK: extract_check", system, fixed=TRUE)) '{"cut":[]}' else
      json_text(setNames(lapply(regmatches(user, gregexpr("ROW [A-Z][0-9]+", user))[[1]], function(x) list()), sub("ROW ", "", regmatches(user, gregexpr("ROW [A-Z][0-9]+", user))[[1]]))),
      model="test")
  }, .GlobalEnv)
  on.exit(assign("llm_chat", old_chat, envir=.GlobalEnv), add=TRUE)
  result <- extract_dependencies(list(), spec, "ADSL", project=p)
  expect_false(any(grepl("Never derive AGE from TRTSDT", captured, fixed=TRUE)))
  expect_length(Filter(function(e) !identical(e$origin, "structural"), result$edges), 0L)
  A <- list(project=p)
  store_extracted(A, "ADSL", result)
  expect_length(dag_agent_read(p,"ADSL")$corrections, 0L)
  expect_equal(state_payload(list(project=p,jobs=list()))$datasets$ADSL$dag$manual_edit_count, 0L)
})

test_that("generate and ask prompts offer lookup and still return code in one call", {
  p <- adsl_project()
  spec <- read_project_spec(p)
  g <- load_graph(p, "ADSL")
  sys <- side_system_prompt("primary", spec, "ADSL")
  ask <- ask_system_prompt("primary", "ADSL")
  expect_match(strsplit(sys, "\n", fixed = TRUE)[[1]][[1]], "^You are the primary-side R expert")
  expect_match(sys, "LOOKUP:", fixed = TRUE)
  expect_match(ask, '{"lookup":["ADSL.TRTSDT"]}', fixed = TRUE)
  logdir <- tempfile("gen-log-")
  dir.create(logdir)
  old <- getOption("dualdag.llm_log")
  on.exit(options(dualdag.llm_log = old), add = TRUE)
  options(dualdag.llm_log = list(dir = logdir, task = "generate", node = "ADSL.AGE", side = "primary"))
  code <- with_fake_llm(generate_side(list(), "primary", spec, g, "ADSL.AGE", character(), project = p$dir))
  expect_match(code, "AGE", fixed = TRUE)
  expect_equal(length(list.files(logdir, pattern = "\\.json$")), 1L)
})

test_that("HTTP apply and dag download keep the graph rules", {
  old_home <- Sys.getenv("DUALDAG_HOME", unset = NA_character_)
  home <- tempfile("dag-home-")
  dir.create(home)
  Sys.setenv(DUALDAG_HOME = home, DUALDAG_FAKE_LLM = "1")
  on.exit({
    restore_test_env("DUALDAG_HOME", old_home)
  }, add = TRUE)
  p <- adsl_project()
  put_status(p, "ADSL", "ADSL.TRTSDT", list(level = "L3"))
  A <- create_app(app_root, p$dir, 7984L, "dag-token", load_config())
  bad <- dag_call(A, "/api/dag/apply", "POST", list(
    dataset = "ADSL",
    changes = list(list(op = "add_edge", from = "ADSL.AGE", to = "ADSL.AGE"))
  ))
  expect_equal(bad$status, 400L)
  expect_match(dag_json(bad)$error, "itself", fixed = TRUE)
  ok <- dag_call(A, "/api/dag/apply", "POST", list(
    dataset = "ADSL",
    changes = list(list(op = "add_edge", from = "ADSL.USUBJID", to = "ADSL.TRTSDT", evidence = "http"))
  ))
  expect_equal(ok$status, 200L)
  body <- dag_json(ok)
  expect_true(isTRUE(body$ok))
  st <- get_status(A$project, "ADSL", "ADSL.TRTSDT")
  expect_equal(st$level, "L3")
  expect_true("ADSL.USUBJID" %in% st$upstream_changed)
  dl <- dag_call(A, "/api/graph/download?dataset=ADSL&what=dag")
  expect_equal(dl$status, 200L)
  text <- rawToChar(dl$body)
  expect_match(text, "ADSL.TRTSDT", fixed = TRUE)
  expect_match(text, "# DAG ADSL", fixed = TRUE)
})

test_that("DAG chat appears in headless Chrome", {
  chrome <- Sys.getenv("CHROME_BIN", unname(Sys.which("google-chrome")))
  skip_if_not(nzchar(chrome) && file.exists(chrome), "chrome is not installed")
  html <- normalizePath(file.path(app_root, "www", "index.html"), winslash = "/")
  url <- paste0("file://", html, "?mock=1")
  out <- tempfile("dagdom-")
  # google-chrome is a shell wrapper. system2's stdout= file redirect leaves
  # the dump empty. system() runs this line through a shell, which is what
  # captures --dump-dom. Chrome's own stderr is not a page error.
  # system2() pastes args into one shell line without quoting them, so a
  # bash -c script would be split at the first space. Call system() instead.
  cmd <- paste(
    shQuote(chrome),
    "--headless=new --disable-gpu --no-sandbox --allow-file-access-from-files",
    "--virtual-time-budget=1000 --dump-dom",
    shQuote(url),
    ">", shQuote(out),
    "2>/dev/null"
  )
  system(cmd, timeout = 60)
  dom <- paste(readLines(out, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_true(grepl("DAG agent", dom, fixed = TRUE))
  expect_true(grepl("id=\"dag-text\"", dom, fixed = TRUE))
  expect_false(grepl("Multi-select", dom, fixed = TRUE))
  expect_false(grepl("Generate selected", dom, fixed = TRUE))
  expect_false(grepl("Quick pick", dom, fixed = TRUE))
  expect_false(grepl("Compare after generating", dom, fixed = TRUE))
  expect_match(dom, 'data-boot="1"', fixed = TRUE)
  expect_match(dom, 'data-js-errors=""', fixed = TRUE)
})
