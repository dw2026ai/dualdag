source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))
source(file.path(testthat::test_path(), "..", "..", "R", "config.R"), local = FALSE)
source(file.path(testthat::test_path(), "..", "..", "R", "spec.R"), local = FALSE)
source(file.path(testthat::test_path(), "..", "..", "R", "project.R"), local = FALSE)
source(file.path(testthat::test_path(), "..", "..", "R", "coder_window.R"), local = FALSE)
source(file.path(testthat::test_path(), "..", "..", "R", "server.R"), local = FALSE)

coder_window_call <- function(A, path) {
  q <- sub("^[^?]*\\?", "", path)
  path <- sub("\\?.*$", "", path)
  make_handlers(A)$call(list(PATH_INFO = path, REQUEST_METHOD = "GET", QUERY_STRING = q,
    HTTP_HOST = sprintf("127.0.0.1:%d", A$port), HTTP_X_DUALDAG_TOKEN = A$token,
    CONTENT_TYPE = "", rook.input = list(read = function() raw())))
}

coder_window_post <- function(A, path, body) {
  raw <- charToRaw(as.character(to_json(body)))
  make_handlers(A)$call(list(PATH_INFO = path, REQUEST_METHOD = "POST", QUERY_STRING = "",
    HTTP_HOST = sprintf("127.0.0.1:%d", A$port), HTTP_X_DUALDAG_TOKEN = A$token,
    CONTENT_TYPE = "application/json", rook.input = list(read = function() raw)))
}

coder_session_project <- function() {
  p <- temp_project()
  dir.create(file.path(p, "spec"), recursive = TRUE, showWarnings = FALSE)
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Subject,Char,20,1,DM.USUBJID,copy",
    "ADSL,AGE,Age,Num,8,,DM.AGE,copy"), file.path(p, "spec", "variable.csv"))
  haven::write_xpt(data.frame(USUBJID = c("S1", "S2"), AGE = c(20, 40)), file.path(p, "data", "DM.xpt"))
  save_graph(p, make_graph("ADSL", list(gn("ADSL.USUBJID"), gn("ADSL.AGE")),
    list(list(from = "ADSL.USUBJID", to = "ADSL.AGE", evidence = "key", origin = "user"))))
  p
}

test_that("failed Re-run updates the current Coder session to failed", {
  p <- coder_session_project(); sid <- coder_session_start(p,"ADSL","ADSL.AGE","primary","fake")
  rec <- coder_session_read(p,sid); rec$status <- "success"; write_json(rec,file.path(coder_session_dir(p,sid),"session.json"))
  dir <- side_run_dir(p,"ADSL","ADSL.AGE","primary"); dir.create(dir,recursive=TRUE,showWarnings=FALSE)
  write_text("stop('rerun failed')",file.path(dir,"code.R"))
  A <- create_app(normalizePath(file.path(testthat::test_path(),"..","..")),p,8863L,"rerun-status-test",load_config())
  response <- coder_window_post(A,"/api/node/run",list(dataset="ADSL",node="ADSL.AGE",side="primary"))
  expect_equal(response$status,200L)
  expect_identical(coder_session_read(p,sid)$status,"failed")
})

test_that("SU requests are stateless and peek_data returns at most twenty rows", {
  p <- coder_session_project()
  haven::write_xpt(data.frame(USUBJID=paste0("S",1:30),AGE=seq_len(30)),file.path(p,"data","DM.xpt"))
  other <- side_run_dir(p,"ADSL","ADSL.AGE","validation"); dir.create(other,recursive=TRUE,showWarnings=FALSE)
  write_text("OTHER_SIDE_SU_MARKER",file.path(other,"code.R")); write_text("OTHER_SIDE_SU_MARKER",file.path(other,"log.txt"))
  dir.create(file.path(p,".dualdag","experts"),recursive=TRUE,showWarnings=FALSE)
  write_text("OTHER_SIDE_SU_MARKER",file.path(p,".dualdag","experts","ADSL_validation.json"))
  sid <- coder_session_start(p,"ADSL","ADSL.AGE","validation","fake")
  coder_session_append(p,sid,"turns",list(message="OTHER_SIDE_SU_MARKER",node="ADSL.AGE",side="validation"))
  A <- create_app(normalizePath(file.path(testthat::test_path(),"..","..")),p,8864L,"su-stateless-test",load_config())
  before_prompt <- side_user_prompt("primary",read_project_spec_dir(p),load_graph(p,"ADSL"),"ADSL.AGE",character(),project=p)
  state_files <- list.files(file.path(p,".dualdag"),recursive=TRUE,full.names=TRUE)
  before <- if(length(state_files)) tools::md5sum(state_files[file.info(state_files)$isdir %in% FALSE]) else character()
  old <- Sys.getenv("DUALDAG_FAKE_LLM"); Sys.setenv(DUALDAG_FAKE_LLM="1")
  original <- llm_chat; calls <- list()
  assign("llm_chat",function(cfg,system,user,...) {
    calls[[length(calls)+1L]] <<- list(system=system,user=user)
    if(length(calls)==1L) list(text='{"tool":"peek_data","args":{}}',model="fake")
    else list(text='{"tool":"reply","text":"SU reply ready."}',model="fake")
  },.GlobalEnv)
  on.exit({assign("llm_chat",original,.GlobalEnv); if(nzchar(old))Sys.setenv(DUALDAG_FAKE_LLM=old)else Sys.unsetenv("DUALDAG_FAKE_LLM")},add=TRUE)
  out <- su_tool_loop(list(),p,"ADSL","ADSL.AGE","primary",list(list(role="user",text="Inspect the source.")))
  expect_identical(out$reply,"SU reply ready.")
  expect_false(grepl("OTHER_SIDE_SU_MARKER",paste(vapply(calls,`[[`,"","user"),collapse="\n"),fixed=TRUE))
  payload <- jsonlite::fromJSON(calls[[2]]$user,simplifyVector=FALSE)
  peek <- sub("^peek_data: ","",payload$tool_results[[1]])
  rows <- jsonlite::fromJSON(peek,simplifyVector=FALSE)$DM
  expect_lte(length(rows),20L)
  after_files <- list.files(file.path(p,".dualdag"),recursive=TRUE,full.names=TRUE)
  after <- if(length(after_files)) tools::md5sum(after_files[file.info(after_files)$isdir %in% FALSE]) else character()
  expect_identical(names(after),names(before)); expect_identical(unname(after),unname(before))
  after_prompt <- side_user_prompt("primary",read_project_spec_dir(p),load_graph(p,"ADSL"),"ADSL.AGE",character(),project=p)
  expect_identical(before_prompt,after_prompt)
})

test_that("detail and Coder window GETs compute warnings without writing state", {
  p <- temp_project(); dir.create(file.path(p,"spec"),recursive=TRUE,showWarnings=FALSE)
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Subject,Char,20,1,DM.USUBJID,copy",
    "ADSL,AGE,Age,Num,8,,DM.AGE,copy",
    "ADSL,BRTHDT,Birth,Char,20,,DM.BRTHDT,copy"),file.path(p,"spec","variable.csv"))
  haven::write_xpt(data.frame(USUBJID=c("S1","S2"),AGE=c(20,40),BRTHDT=c("2000-01-01","2001-01-01")),file.path(p,"data","DM.xpt"))
  save_graph(p,make_graph("ADSL",list(gn("ADSL.USUBJID"),gn("ADSL.AGE")),list(ge("ADSL.USUBJID","ADSL.AGE"))))
  run_dir <- side_run_dir(p,"ADSL","ADSL.AGE","primary"); dir.create(run_dir,recursive=TRUE,showWarnings=FALSE)
  write_text("result <- data.frame(USUBJID=work$USUBJID, AGE=work$BRTHDT)",file.path(run_dir,"code.R"))
  A <- create_app(normalizePath(file.path(testthat::test_path(),"..","..")),p,8861L,"read-only-get-test",load_config())
  state_file <- file.path(A$project$state_dir,"state.json")
  detail_before <- file.info(state_file)$mtime
  detail <- from_json(coder_window_call(A,"/api/node/detail?dataset=ADSL&node=ADSL.AGE")$body)
  expect_true(any(grepl("Lineage warning: AGE reads BRTHDT",unlist(detail$status$warnings),fixed=TRUE)))
  expect_identical(file.info(state_file)$mtime,detail_before)
  coder_before <- file.info(state_file)$mtime
  window <- from_json(coder_window_call(A,"/api/node/coder-window?dataset=ADSL&node=ADSL.AGE&side=primary")$body)
  expect_true(any(grepl("Lineage warning: AGE reads BRTHDT",unlist(window$status$warnings),fixed=TRUE)))
  expect_identical(file.info(state_file)$mtime,coder_before)
})

test_that("SU API refreshes child-written state in the server before the next write", {
  p <- coder_session_project(); graph <- load_graph(p,"ADSL"); spec <- read_project_spec_dir(p)
  validation <- run_side(p,"ADSL",graph_node(graph,"ADSL.AGE"),"validation",
    "result <- data.frame(USUBJID=src$DM$USUBJID, AGE=src$DM$AGE + 1)",graph=graph,spec=spec)
  expect_true(validation$ok,info=validation$error %||% validation$log)
  A <- create_app(normalizePath(file.path(testthat::test_path(),"..","..")),p,8862L,"su-state-test",load_config())
  original_launch <- launch; original_chat <- llm_chat
  replies <- list(
    list(text=json_text(list(tool="write_program",args=list(code="result <- data.frame(USUBJID=src$DM$USUBJID, AGE=src$DM$AGE + 1)"))),model="scripted"),
    list(text=json_text(list(tool="reply",text="Updated.")),model="scripted")
  )
  ix <- 0L
  assign("llm_chat",function(...) { ix <<- ix+1L; replies[[ix]] },.GlobalEnv)
  assign("launch",function(A,fn_name,args,timeout) promises::promise(function(resolve,reject) {
    tryCatch(resolve(do.call(get(fn_name,envir=.GlobalEnv,mode="function"),args)),error=reject)
  }),.GlobalEnv)
  on.exit({assign("launch",original_launch,.GlobalEnv);assign("llm_chat",original_chat,.GlobalEnv)},add=TRUE)
  old_fake <- Sys.getenv("DUALDAG_FAKE_LLM"); Sys.setenv(DUALDAG_FAKE_LLM="1")
  on.exit(if(nzchar(old_fake))Sys.setenv(DUALDAG_FAKE_LLM=old_fake)else Sys.unsetenv("DUALDAG_FAKE_LLM"),add=TRUE)
  response_promise <- coder_window_post(A,"/api/node/su",list(dataset="ADSL",node="ADSL.AGE",side="primary",message="Update it.",messages=list()))
  response_box <- new.env(parent=emptyenv())
  promises::then(response_promise,onFulfilled=function(v)response_box$value<-v,onRejected=function(e)response_box$value<-list(status=502L,body=charToRaw(conditionMessage(e))))
  for(i in seq_len(200L)) { later::run_now(); if(!is.null(response_box$value))break; Sys.sleep(.025) }
  response <- response_box$value
  expect_equal(response$status,200L)
  detail <- from_json(coder_window_call(A,"/api/node/detail?dataset=ADSL&node=ADSL.AGE")$body)
  expect_identical(detail$status$level,"L3")
  unrelated <- get_status(A$project,"ADSL","ADSL.UNRELATED")
  unrelated$level <- "L2"
  put_status(A$project,"ADSL","ADSL.UNRELATED",unrelated)
  expect_identical(get_status(A$project,"ADSL","ADSL.AGE")$level,"L3")
  expect_identical(get_status(open_project(p),"ADSL","ADSL.AGE")$level,"L3")
})

test_that("SU API reloads child-written state when a later round rejects", {
  p <- coder_session_project()
  A <- create_app(normalizePath(file.path(testthat::test_path(), "..", "..")), p,
    8865L, "su-rejected-state-test", load_config())
  original_launch <- launch
  assign("launch", function(A, fn_name, args, timeout) promises::promise(function(resolve, reject) {
    child <- open_project(args$project_dir)
    st <- get_status(child, "ADSL", "ADSL.AGE"); st$level <- "L3"
    put_status(child, "ADSL", "ADSL.AGE", st)
    reject(simpleError("later SU round failed"))
  }), .GlobalEnv)
  on.exit(assign("launch", original_launch, .GlobalEnv), add = TRUE)
  response_promise <- coder_window_post(A, "/api/node/su", list(dataset = "ADSL", node = "ADSL.AGE",
    side = "primary", message = "Update it.", messages = list()))
  response_box <- new.env(parent = emptyenv())
  promises::then(response_promise, onFulfilled = function(v) response_box$value <- v,
    onRejected = function(e) response_box$value <- list(status = 502L, body = charToRaw(conditionMessage(e))))
  for (i in seq_len(200L)) { later::run_now(); if (!is.null(response_box$value)) break; Sys.sleep(.025) }
  expect_equal(response_box$value$status, 502L)
  expect_identical(get_status(A$project, "ADSL", "ADSL.AGE")$level, "L3")
  unrelated <- get_status(A$project, "ADSL", "ADSL.UNRELATED"); unrelated$level <- "L2"
  put_status(A$project, "ADSL", "ADSL.UNRELATED", unrelated)
  expect_identical(get_status(open_project(p), "ADSL", "ADSL.AGE")$level, "L3")
})

test_that("SU run_check caps output and never touches official side files", {
  p <- coder_session_project(); dir <- side_run_dir(p,"ADSL","ADSL.AGE","primary"); dir.create(dir,recursive=TRUE,showWarnings=FALSE)
  write_text("official code",file.path(dir,"code.R")); write_text("official log",file.path(dir,"log.txt")); saveRDS("official output",file.path(dir,"out.rds"))
  paths <- file.path(dir,c("code.R","log.txt","out.rds")); before <- tools::md5sum(paths)
  original_chat <- llm_chat; original_run <- run_side; calls <- 0L
  assign("llm_chat",function(cfg,system,user,...) { calls <<- calls+1L; if(calls==1L)list(text='{"tool":"run_check","args":{"code":"x <- 1"}}',model="fake") else list(text='{"tool":"reply","text":"Checked."}',model="fake") },.GlobalEnv)
  assign("run_side",function(...)list(ok=FALSE,log=strrep("x",5000)),.GlobalEnv)
  on.exit({assign("llm_chat",original_chat,.GlobalEnv);assign("run_side",original_run,.GlobalEnv)},add=TRUE)
  out <- su_tool_loop(list(),p,"ADSL","ADSL.AGE","primary",list(list(role="user",text="Check")))
  value <- jsonlite::fromJSON(sub("^run_check: ","",out$calls[[1]]))
  expect_lte(nchar(value),4000L); expect_match(value,"truncated at 4000 characters",fixed=TRUE)
  expect_identical(unname(tools::md5sum(paths)),unname(before))
})

test_that("SU refuses explicit other-side and session paths", {
  p <- coder_session_project()
  expect_match(su_forbidden_code("readLines('/tmp/project/validation/code.R')",p,"primary"),"Refused",fixed=TRUE)
  expect_match(su_forbidden_code("readRDS('/tmp/project/.dualdag/sessions/x/out.rds')",p,"primary"),"Refused",fixed=TRUE)
})

test_that("Generate clears the SU program marker", {
  p <- coder_session_project(); write_json(list(su_edited=TRUE),su_edited_path(p,"ADSL","ADSL.AGE","primary"))
  original <- generate_side; assign("generate_side",function(...)stop("test generation stop"),.GlobalEnv)
  on.exit(assign("generate_side",original,.GlobalEnv),add=TRUE)
  expect_error(run_side_job(p,"ADSL","ADSL.AGE","primary",NULL,30),"test generation stop")
  expect_false(file.exists(su_edited_path(p,"ADSL","ADSL.AGE","primary")))
})

test_that("write_program refuses cross-side paths and normal runner rule violations", {
  p <- coder_session_project(); dir <- side_run_dir(p,"ADSL","ADSL.AGE","primary"); dir.create(dir,recursive=TRUE,showWarnings=FALSE)
  write_text("keep this program",file.path(dir,"code.R"))
  original_chat <- llm_chat; on.exit(assign("llm_chat",original_chat,.GlobalEnv),add=TRUE)
  for(code in c("readLines('/tmp/project/validation/code.R')", "library(pkg)")) {
    calls <- 0L
    local_code <- code
    assign("llm_chat",function(cfg,system,user,...) { calls <<- calls+1L; if(calls==1L)list(text=json_text(list(tool="write_program",args=list(code=local_code))),model="fake") else list(text='{"tool":"reply","text":"Refused."}',model="fake") },.GlobalEnv)
    out <- su_tool_loop(list(),p,"ADSL","ADSL.AGE","primary",list(list(role="user",text="change it")))
    assign("llm_chat",original_chat,.GlobalEnv)
    expect_match(out$calls[[1]],"Refused",ignore.case=TRUE)
    expect_identical(read_text(file.path(dir,"code.R")),"keep this program")
  }
})

test_that("successful SU write records only its program event and run snapshot", {
  p <- coder_session_project(); spec <- read_project_spec_dir(p); graph <- load_graph(p,"ADSL")
  vcode <- "ADSL_TEMP <- src$DM[, c('USUBJID','AGE'), drop=FALSE]"
  vrun <- run_side(p,"ADSL",graph_node(graph,"ADSL.AGE"),"validation",vcode,graph=graph,spec=spec)
  expect_true(vrun$ok)
  sid <- coder_session_start(p,"ADSL","ADSL.AGE","primary","fake")
  pdir <- side_run_dir(p,"ADSL","ADSL.AGE","primary"); dir.create(pdir,recursive=TRUE,showWarnings=FALSE)
  write_text("old program",file.path(pdir,"code.R")); write_text("old log",file.path(pdir,"log.txt"))
  write_json(list(approach="Approach for the generated program."),file.path(pdir,"approach.json"))
  code <- "LINEAGE_ADSL <- src$DM[, c('USUBJID','AGE'), drop=FALSE]; LINEAGE_ADSL$AGE <- LINEAGE_ADSL$AGE + 1"
  original_chat <- llm_chat; calls <- 0L
  assign("llm_chat",function(cfg,system,user,...) { calls <<- calls+1L; if(calls==1L)list(text=json_text(list(tool="write_program",args=list(code=code))),model="fake") else list(text='{"tool":"reply","text":"Program updated."}',model="fake") },.GlobalEnv)
  on.exit(assign("llm_chat",original_chat,.GlobalEnv),add=TRUE)
  out <- su_tool_loop(list(),p,"ADSL","ADSL.AGE","primary",list(list(role="user",text="Change age.")))
  expect_match(out$calls[[1]],'"ok":true',fixed=TRUE)
  rec <- coder_session_full(p,sid)
  expect_length(rec$events,1L); expect_identical(rec$events[[1]]$event,"Program changed in SU mode")
  expect_null(rec$prompts); expect_null(rec$turns); expect_length(rec$runs,1L)
  expect_true(isTRUE(read_json(su_edited_path(p,"ADSL","ADSL.AGE","primary"))$su_edited))
  expect_false(file.exists(file.path(pdir,"approach.json")))
  expect_identical(get_status(open_project(p),"ADSL","ADSL.AGE")$level,"L2")
})

chat_records_project <- function(layered = FALSE) {
  p <- temp_project()
  dir.create(file.path(p,"spec"),recursive=TRUE,showWarnings=FALSE)
  if (layered) {
    writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
      "ADVS,USUBJID,Subject,Char,20,1,VS.USUBJID,Subject key",
      "ADVS,PARAMCD,Parameter,Char,8,2,VS.VSTESTCD,Parameter code",
      "ADVS,STUDYID,Study,Char,8,,DM.STUDYID,Study identifier",
      "ADVS,AVAL,Value,Num,8,,VS.VSSTRESN,Value"),file.path(p,"spec","variable.csv"))
    writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation",
      "ADVS,SYSBP,Systolic blood pressure,VS.VSTESTCD; VS.VSSTRESN,Value"),
      file.path(p,"spec","parameter.csv"))
    ds <- "ADVS"
  } else {
    writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
      "ADAE,USUBJID,Subject,Char,20,1,ADSL.USUBJID,Subject key",
      "ADAE,AESEQ,Sequence,Num,8,2,AE.AESEQ,Event sequence",
      "ADAE,AVAL,Value,Num,8,,ADAE.AESEQ,Value"),file.path(p,"spec","variable.csv"))
    ds <- "ADAE"
  }
  spec <- read_project_spec_dir(p)
  graph <- graph_skeleton(spec,ds)
  save_graph(p,graph)
  list(project=p,dataset=ds,graph=graph)
}

with_coder_fake <- function(expr) {
  old_env <- Sys.getenv(c("DUALDAG_HOME", "DUALDAG_FAKE_LLM", "DUALDAG_LLM_API_KEY"))
  home <- tempfile("coder-home-"); dir.create(home)
  Sys.setenv(DUALDAG_HOME = home, DUALDAG_FAKE_LLM = "1", DUALDAG_LLM_API_KEY = "")
  save_config(modifyList(config_defaults(), list(self_review = TRUE, self_review_autofix = FALSE)))
  original <- get("llm_chat", envir = .GlobalEnv)
  calls <- new.env(parent = emptyenv()); calls$items <- list()
  assign("llm_chat", function(cfg, system, user, ...) {
    calls$items[[length(calls$items) + 1L]] <- list(system = system, user = user)
    original(cfg, system, user, ...)
  }, envir = .GlobalEnv)
  on.exit({
    assign("llm_chat", original, envir = .GlobalEnv)
    for (nm in names(old_env)) {
      if (!nzchar(old_env[[nm]])) Sys.unsetenv(nm) else {
        values <- list(old_env[[nm]]); names(values) <- nm; do.call(Sys.setenv, values)
      }
    }
  }, add = TRUE)
  force(expr(calls, home))
}

test_that("usage metadata never changes expert prompt thread text", {
  p <- coder_session_project()
  turn <- list(kind="agent", node="ADSL.AGE", at="2026-01-01T00:00:00Z",
               message="question", reply="answer")
  before <- expert_format_turn(turn)
  turn$usage <- list(input=22400, output=10, limit=258000, calls=3L)
  expect_identical(expert_format_turn(turn), before)
  expert_append_turn(p, "ADSL", "ADSL.AGE", "primary", turn)
  text <- expert_thread_text(p, "ADSL", "primary", "ADSL.AGE")
  expect_true(grepl("Q: question", text, fixed=TRUE))
  expect_true(grepl("A: answer", text, fixed=TRUE))
  expect_false(grepl("22400|usage|258000", text, ignore.case=TRUE))
})

test_that("provider usage accumulates in the Coder session record", {
  p <- coder_session_project()
  sid <- coder_session_start(p, "ADSL", "ADSL.AGE", "primary", "fixture")
  old <- getOption("dualdag.llm_log")
  options(dualdag.llm_log=list(dir=file.path(project_dir_of(p),".dualdag","llm_log"),task="chat",
    node="ADSL.AGE",side="primary",session=sid))
  on.exit(options(dualdag.llm_log=old),add=TRUE)
  llm_log_record(list(provider="openai",api_key=""), "system", "user",
                 list(model="fixture",text="reply",usage=list(prompt_tokens=100,completion_tokens=5)))
  expect_equal(coder_session_read(p,sid)$usage_total,
               list(calls=1L,input=100,output=5))
})

test_that("Coder context usage formats the requested token counts", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "node is not installed")
  usage <- llm_usage_summary(list(input_tokens=22382L, model_context_window=258400L), "codex_cli")
  expect_equal(usage$input, 22382)
  expect_equal(usage$limit, 258400)
  js <- paste(readLines(file.path(testthat::test_path(), "..", "..", "www", "app.js"),
                        warn=FALSE, encoding="UTF-8"), collapse="\n")
  start <- regexpr("  function tokenCount(n) {", js, fixed=TRUE)[1]
  end <- regexpr("  function sessionUsageLabel(usage) {", js, fixed=TRUE)[1]
  expect_gt(start, 0L)
  expect_gt(end, start)
  helpers <- substr(js, start, end - 1L)
  script <- tempfile(fileext=".js")
  on.exit(unlink(script), add=TRUE)
  writeLines(c('function esc(s) { return String(s == null ? "" : s); }', helpers,
    paste0("const actual = contextUsageLine(", jsonlite::toJSON(
      list(input=usage$input, limit=usage$limit), auto_unbox=TRUE), ");"),
    'if (actual !== \'<div class="context-usage">Context: 22.4K of 258.4K tokens (9%)</div>\') process.exit(1);'),
    script, useBytes=TRUE)
  result <- system2(node, shQuote(script), stdout=TRUE, stderr=TRUE)
  expect_identical(attr(result, "status") %||% 0L, 0L, info=paste(result, collapse="\n"))
})

test_that("Coder window returns the exact selected side prompt and process only", {
  p <- temp_project()
  A <- create_app(normalizePath(file.path(testthat::test_path(), "..", "..")), p,
                  7992L, "coder-window-test", load_config())
  primary_system <- "exact primary system with CARD_PRIMARY_MARKER"
  primary_user <- "exact primary user with CARD_USER_MARKER"
  validation_system <- "validation only VALIDATION_SIDE_MARKER"
  coder_window_prompt(p, "ADSL", "ADSL.AGE", "primary", primary_system, primary_user)
  coder_window_event(p, "ADSL", "ADSL.AGE", "primary", "Model call started")
  coder_window_prompt(p, "ADSL", "ADSL.AGE", "validation", validation_system, "other side")
  primary <- from_json(coder_window_call(A, "/api/node/coder-window?dataset=ADSL&node=ADSL.AGE&side=primary")$body)
  expect_identical(primary$side, "primary")
  expect_identical(primary$context$system, primary_system)
  expect_identical(primary$context$user, primary_user)
  expect_length(primary$contexts, 1L)
  expect_match(primary$process[[1]]$event, "Model call started")
  encoded <- as.character(to_json(primary))
  expect_false(grepl("VALIDATION_SIDE_MARKER|other side", encoded))
  expect_false(grepl("api_key|API_KEY", encoded, ignore.case = TRUE))
  expect_false(file.exists(file.path(side_run_dir(p, "ADSL", "ADSL.AGE", "primary"), "api_key")))
})

test_that("generate_side persists the exact prompt it sends", {
  p <- temp_project()
  spec <- spec_of(V("ADSL", "USUBJID", key = 1), V("ADSL", "AGE", source = "DM.AGE"))
  graph <- make_graph("ADSL", list(gn("ADSL.USUBJID"), gn("ADSL.AGE")),
                      list(list(from = "ADSL.USUBJID", to = "ADSL.AGE")))
  with_fake_llm(generate_side(list(), "primary", spec, graph, "ADSL.AGE", "USUBJID", project = p))
  saved <- read_json(coder_window_context_path(p, "ADSL", "ADSL.AGE", "primary"))
  node <- graph_node(graph, "ADSL.AGE")
  expected_system <- side_system_prompt("primary", spec, "ADSL", packages = side_r_packages(p, "primary"), node = node, project = p)
  expected_user <- side_user_prompt("primary", spec, graph, node, "USUBJID", project = p)
  expect_identical(expected_system, side_system_prompt("primary", spec, "ADSL", packages = side_r_packages(p, "primary"), node = node, project = p, chat = FALSE))
  expect_identical(expected_user, side_user_prompt("primary", spec, graph, node, "USUBJID", project = p, chat = FALSE))
  expect_identical(saved[[1]]$system, expected_system)
  expect_identical(saved[[1]]$user, expected_user)
  expect_false("api_key" %in% names(saved[[1]]))
})

test_that("coder session labels, collision allocation, and same-session rules work", {
  p <- temp_project(); sroot <- file.path(p,".dualdag","sessions")
  dir.create(file.path(sroot,"ADSL-AGE-primary-1"),recursive=TRUE)
  a <- coder_session_start(p,"ADSL","ADSL.AGE","primary","fake")
  expect_identical(a,"ADSL-AGE-primary-2")
  expect_identical(coder_session_ensure(p,"ADSL","ADSL.AGE","primary"),a)
  b <- coder_session_start(p,"ADSL","ADSL.AGE","primary","fake")
  expect_identical(b,"ADSL-AGE-primary-3")
  expect_identical(coder_session_read(p,b)$parent_session,a)
  expect_true(dir.exists(coder_session_dir(p,a)))
  coder_session_close(p,"ADSL","ADSL.AGE","primary")
  c <- coder_session_ensure(p,"ADSL","ADSL.AGE","primary")
  expect_identical(c,"ADSL-AGE-primary-4")
  expect_identical(coder_session_start(p,"ADVS","ADVS.BASE@SYSBP","primary"),"ADVS-SYSBP-BASE-primary-1")
  expect_identical(coder_session_start(p,"ADVS","ADVS.RECORDS@SYSBP","primary"),"ADVS-SYSBP-Keys-primary-1")
  expect_identical(coder_session_start(p,"ADVS","ADVS.USUBJID","validation"),"ADVS-USUBJID-validation-1")
})

test_that("session sequence does not reuse numbers after expired sessions are pruned", {
  p <- temp_project()
  ids <- vapply(seq_len(3), function(i) coder_session_start(p,"ADSL","ADSL.AGE","primary","fake"), "")
  expect_identical(ids, c("ADSL-AGE-primary-1", "ADSL-AGE-primary-2", "ADSL-AGE-primary-3"))
  old <- format(Sys.time() - 200 * 86400, "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  for (id in ids[1:2]) {
    rec <- coder_session_read(p, id); rec$status <- "success"; rec$last_at <- old
    write_json(rec, file.path(coder_session_dir(p,id), "session.json"))
  }
  next_id <- coder_session_start(p,"ADSL","ADSL.AGE","primary","fake")
  expect_identical(next_id, "ADSL-AGE-primary-4")
  expect_false(dir.exists(coder_session_dir(p, ids[[1]])))
  expect_false(dir.exists(coder_session_dir(p, ids[[2]])))
  expect_identical(coder_session_open(p,"ADSL","ADSL.AGE","primary"), next_id)
  expect_true(coder_session_current(create_app(normalizePath(file.path(testthat::test_path(),"..","..")),p,8796L,"session-seq",load_config()),coder_session_read(p,next_id))$is_latest)
  collision <- coder_session_start(p,"AD_S","ADSL.AGE","primary","fake")
  collision_next <- coder_session_start(p,"AD.S","ADSL.AGE","primary","fake")
  expect_identical(collision,"AD.S-AGE-primary-1")
  expect_identical(collision_next,"AD.S-AGE-primary-2")
})

test_that("session records survive Coder window reset and redact secrets", {
  p <- temp_project(); Sys.setenv(DUALDAG_LLM_API_KEY="test-secret-123")
  on.exit(Sys.unsetenv("DUALDAG_LLM_API_KEY"),add=TRUE)
  sid <- coder_session_start(p,"ADSL","ADSL.AGE","primary","fake")
  coder_window_prompt(p,"ADSL","ADSL.AGE","primary","system test-secret-123","user test-secret-123")
  coder_window_event(p,"ADSL","ADSL.AGE","primary","Run log","test-secret-123")
  coder_window_reset(p,"ADSL","ADSL.AGE","primary")
  rec <- coder_session_full(p,sid)
  expect_match(rec$prompts[[1]]$system,"\\[redacted\\]")
  files <- list.files(coder_session_dir(p,sid),recursive=TRUE,full.names=TRUE)
  contents <- paste(unlist(lapply(files,function(f) readLines(f,warn=FALSE))),collapse="\n")
  expect_false(grepl("test-secret-123",contents,fixed=TRUE))
  expect_length(rec$events,1L)
  next_sid <- coder_session_start(p,"ADSL","ADSL.AGE","primary","fake")
  expect_true(dir.exists(coder_session_dir(p,sid)))
  expect_identical(coder_session_read(p,next_sid)$parent_session,sid)
})

test_that("coder LLM logs link to their session and redact the API key", {
  p <- temp_project(); sid <- coder_session_start(p,"ADSL","ADSL.AGE","primary","fake")
  logdir <- file.path(p,".dualdag","llm_log"); options(dualdag.llm_log=list(dir=logdir,task="chat",node="ADSL.AGE",side="primary",session=sid))
  on.exit(options(dualdag.llm_log=NULL),add=TRUE)
  llm_log_record(list(api_key="sample-secret-xyz"),"sample-secret-xyz","question",list(model="fake",text="sample-secret-xyz"))
  item <- read_json(list.files(logdir,full.names=TRUE)[1])
  expect_identical(item$session,sid); expect_false(grepl("sample-secret-xyz",paste(unlist(item),collapse=" "),fixed=TRUE))
})

test_that("session routes validate IDs and return metadata and full record", {
  p <- temp_project(); sid <- coder_session_start(p,"ADSL","ADSL.AGE","primary","fake")
  A <- create_app(normalizePath(file.path(testthat::test_path(),"..","..")),p,7994L,"coder-session-test",load_config())
  listing <- from_json(coder_window_call(A,"/api/sessions?dataset=ADSL&node=ADSL.AGE&side=primary")$body)
  expect_identical(listing[[1]]$id,sid)
  expect_equal(coder_window_call(A,"/api/session?id=bad%2F..")$status,400L)
  expect_equal(coder_window_call(A,"/api/session?id=ADSL-AGE-primary-999")$status,404L)
  expect_equal(coder_window_call(A,paste0("/api/session?id=",sid))$status,200L)
})

test_that("stored sessions never enter prompt construction", {
  p <- temp_project(); sid <- coder_session_start(p,"ADSL","ADSL.AGE","validation","fake")
  coder_session_append(p,sid,"prompts",list(user="OTHER_SIDE_SESSION_MARKER"))
  spec <- spec_of(V("ADSL","AGE",source="DM.AGE")); graph <- make_graph("ADSL",list(gn("ADSL.AGE")))
  before <- side_user_prompt("primary",spec,graph,"ADSL.AGE",character(),project=p)
  coder_session_append(p,sid,"prompts",list(user="OTHER_SIDE_SESSION_MARKER"))
  after <- side_user_prompt("primary",spec,graph,"ADSL.AGE",character(),project=p)
  expect_identical(before,after)
  expect_false(grepl("ADSL-AGE-primary|OTHER_SIDE_SESSION_MARKER",after))
})

test_that("session retention removes expired non-newest sessions only", {
  expect_equal(SESSION_KEEP_HOURS,168)
  p <- temp_project(); old <- format(Sys.time()-200*86400, "%Y-%m-%dT%H:%M:%S%z",tz="UTC")
  add <- function(id,node,status="success",days=200) { at <- format(Sys.time()-days*86400, "%Y-%m-%dT%H:%M:%S%z",tz="UTC"); d <- coder_session_dir(p,id); dir.create(d,recursive=TRUE); write_json(list(id=id,dataset="ADSL",node=node,side="primary",started_at=at,last_at=old,status=status),file.path(d,"session.json")) }
  add("ADSL-AGE-primary-1","ADSL.AGE",days=200); add("ADSL-AGE-primary-2","ADSL.AGE",days=190)
  add("ADSL-SEX-primary-1","ADSL.SEX","open")
  coder_session_prune(p)
  expect_false(dir.exists(coder_session_dir(p,"ADSL-AGE-primary-1")))
  expect_true(dir.exists(coder_session_dir(p,"ADSL-AGE-primary-2")))
  expect_true(dir.exists(coder_session_dir(p,"ADSL-SEX-primary-1")))
})

test_that("session route returns live current packet while idle and running", {
  p <- coder_session_project(); sid <- coder_session_start(p,"ADSL","ADSL.AGE","primary","fake")
  A <- create_app(normalizePath(file.path(testthat::test_path(),"..","..")),p,7995L,"coder-current-test",load_config())
  sdir <- side_run_dir(p,"ADSL","ADSL.AGE","primary"); write_text("x <- 1",file.path(sdir,"code.R")); write_text("ran",file.path(sdir,"log.txt"))
  idle <- from_json(coder_window_call(A,paste0("/api/session?id=",sid))$body)$current
  expect_identical(idle$code,"x <- 1"); expect_identical(idle$log,"ran"); expect_false(idle$running); expect_null(idle$step)
  expect_identical(idle$state,"Not generated"); expect_true(idle$is_latest)
  st <- get_status(A$project,"ADSL","ADSL.AGE"); st$level <- "L3"; st$problem <- list(stage="compare",message="Mismatch")
  put_status(A$project,"ADSL","ADSL.AGE",st,save=FALSE); write_json(list(match=FALSE),file.path(node_run_dir(A$project,"ADSL","ADSL.AGE"),"compare.json"))
  live <- from_json(coder_window_call(A,paste0("/api/session?id=",sid))$body)$current
  expect_identical(live$state,"Data matched"); expect_identical(live$last_problem,"Mismatch"); expect_identical(live$problem_stage,"compare"); expect_false(live$compare$match)
  st$level <- "L2"; st$problem <- NULL; put_status(A$project,"ADSL","ADSL.AGE",st,save=FALSE)
  generated <- from_json(coder_window_call(A,paste0("/api/session?id=",sid))$body)$current
  expect_identical(generated$state,"Generated")
  A$jobs[["run1"]] <- list(dataset="ADSL",node="ADSL.AGE",kind="generate")
  coder_session_append(p,sid,"events",list(event="Self-review started",at=now_iso(),detail=NULL))
  busy <- from_json(coder_window_call(A,paste0("/api/session?id=",sid))$body)$current
  expect_true(busy$running); expect_identical(busy$step,"Self-review started")
})

test_that("stored program reruns stay in sessions and snapshot their actual run logs", {
  p <- coder_session_project()
  A <- create_app(normalizePath(file.path(testthat::test_path(),"..","..")),p,8797L,"rerun-session",load_config())
  for (side in c("primary","validation")) {
    d <- side_run_dir(p,"ADSL","ADSL.AGE",side); dir.create(d,recursive=TRUE,showWarnings=FALSE)
    write_text(paste0("# ",side),file.path(d,"code.R")); write_text("OLD LOG",file.path(d,"log.txt"))
  }
  original_run <- run_stored_sides; original_apply <- apply_stored_run
  assign("run_stored_sides",function(project,dataset,node,side="both",...) {
    results <- list()
    for (s in c("primary","validation")) {
      log <- paste("fresh",s,"log")
      write_text(log,file.path(side_run_dir(project,dataset,node,s),"log.txt"))
      results[[s]] <- list(ok=identical(s,"primary"),error=if(identical(s,"primary"))NULL else "deliberate failure",log=log)
    }
    list(ok=TRUE,results=results)
  },envir=.GlobalEnv)
  assign("apply_stored_run",function(...)list(ok=TRUE),envir=.GlobalEnv)
  on.exit({assign("run_stored_sides",original_run,envir=.GlobalEnv);assign("apply_stored_run",original_apply,envir=.GlobalEnv)},add=TRUE)
  response <- coder_window_post(A,"/api/node/run",list(dataset="ADSL",node="ADSL.AGE",side="both"))
  expect_equal(response$status,200L)
  for (side in c("primary","validation")) {
    sid <- coder_session_open(p,"ADSL","ADSL.AGE",side)
    expect_false(is.null(sid))
    rec <- coder_session_full(p,sid)
    names <- vapply(rec$events,function(e)e$event,"")
    expect_true(all(c("Program run started","Program run finished","Run log") %in% names))
    if (identical(side,"validation")) expect_true("Run error" %in% names)
    runlog <- rec$events[[which(names=="Run log")[[1]]]]$detail
    expect_identical(runlog,read_text(file.path(side_run_dir(p,"ADSL","ADSL.AGE",side),"log.txt")))
    expect_length(rec$runs,1L)
    expect_identical(rec$runs[[1]]$log,runlog)
    expect_true(nzchar(rec$runs[[1]]$code))
  }
})

test_that("clearing expert history closes open sessions without changing their status", {
  p <- coder_session_project(); sid <- coder_session_start(p,"ADSL","ADSL.AGE","primary","fake")
  rec <- coder_session_read(p,sid); rec$status <- "failed"; write_json(rec,file.path(coder_session_dir(p,sid),"session.json"))
  A <- create_app(normalizePath(file.path(testthat::test_path(),"..","..")),p,8798L,"clear-session",load_config())
  cleared <- coder_window_post(A,"/api/expert/clear",list(dataset="ADSL",side="primary"))
  expect_equal(cleared$status,200L)
  expect_null(coder_session_open(p,"ADSL","ADSL.AGE","primary"))
  expect_identical(coder_session_read(p,sid)$status,"failed")
  expect_false(is.null(coder_session_read(p,sid)$last_at))
})

test_that("blocked generation records the refusal as a Run error", {
  p <- coder_session_project(); sid <- coder_session_start(p,"ADSL","ADSL.AGE","primary","fake")
  original_graph <- load_graph; original_node <- graph_node; original_upstream <- upstream_ids; original_states <- state_nodes
  assign("load_graph",function(...)list(layered=TRUE),envir=.GlobalEnv)
  assign("graph_node",function(graph,id)list(paramcd="Q"),envir=.GlobalEnv)
  assign("upstream_ids",function(...)"ADSL.PARENT",envir=.GlobalEnv)
  assign("state_nodes",function(...)empty_named(),envir=.GlobalEnv)
  on.exit({assign("load_graph",original_graph,envir=.GlobalEnv);assign("graph_node",original_node,envir=.GlobalEnv);assign("upstream_ids",original_upstream,envir=.GlobalEnv);assign("state_nodes",original_states,envir=.GlobalEnv)},add=TRUE)
  err <- tryCatch(run_side_job(p,"ADSL","ADSL.AGE","primary",NULL,30),error=function(e)e)
  expect_s3_class(err,"error")
  expect_match(conditionMessage(err),"blocked by unfinished upstream variable")
  events <- read_json(file.path(coder_session_dir(p,sid),"events.json"))
  expect_length(events,1L); expect_identical(events[[1]]$event,"Run error")
  expect_match(events[[1]]$detail,"blocked by unfinished upstream variable")
  expect_identical(coder_session_read(p,sid)$status,"failed")
})

test_that("run_side_job keeps generation and self-review attempts separate and chat appends", {
  p <- coder_session_project()
  with_coder_fake(function(calls, home) {
    expert_append_turn(p, "ADSL", "ADSL.AGE", "primary", list(kind = "ask", question = "Earlier primary question", answer = "Earlier primary answer"))
    expert_append_turn(p, "ADSL", "ADSL.AGE", "validation", list(kind = "ask", question = "VALIDATION_THREAD_SECRET", answer = "VALIDATION_REPLY_SECRET"))
    expert_append_turn(p, "ADSL", "ADSL.USUBJID", "primary", list(kind = "ask", question = "OTHER_NODE_THREAD_SECRET", answer = "OTHER_NODE_REPLY_SECRET"))
    other <- side_run_dir(p, "ADSL", "ADSL.AGE", "validation")
    dir.create(other, recursive = TRUE, showWarnings = FALSE)
    write_text("VALIDATION_CODE_SECRET", file.path(other, "code.R"))
    write_text("VALIDATION_LOG_SECRET", file.path(other, "log.txt"))

    generated <- run_side_job(p, "ADSL", "ADSL.AGE", "primary", NULL, 60)
    expect_true(generated$run$ok)
    expect_length(calls$items, 2L)
    A <- create_app(normalizePath(file.path(testthat::test_path(), "..", "..")), p, 8791L,
                    "coder-session-test", load_config())
    base_url <- "/api/node/coder-window?dataset=ADSL&node=ADSL.AGE&side=primary"
    generation <- from_json(coder_window_call(A, base_url)$body)
    self_review <- from_json(coder_window_call(A, paste0(base_url, "&attempt=2"))$body)
    expect_equal(vapply(generation$contexts, function(x) x$task, ""), c("generation", "self-review"))
    expect_identical(generation$context$task, "generation")
    expect_identical(generation$context$system, calls$items[[1]]$system)
    expect_identical(generation$context$user, calls$items[[1]]$user)
    expect_identical(self_review$context$task, "self-review")
    expect_identical(self_review$context$system, calls$items[[2]]$system)
    expect_identical(self_review$context$user, calls$items[[2]]$user)
    routed <- as.character(to_json(generation))
    for (secret in c("VALIDATION_THREAD_SECRET", "VALIDATION_REPLY_SECRET", "VALIDATION_CODE_SECRET", "VALIDATION_LOG_SECRET",
                     "OTHER_NODE_THREAD_SECRET", "OTHER_NODE_REPLY_SECRET"))
      expect_false(grepl(secret, routed, fixed = TRUE))
    expect_false(file.exists(api_key_file()))
    expect_false(grepl("api_key|API_KEY", read_text(coder_window_context_path(p, "ADSL", "ADSL.AGE", "primary")), ignore.case = TRUE))

    code_path <- file.path(side_run_dir(p, "ADSL", "ADSL.AGE", "primary"), "code.R")
    code_before <- read_text(code_path)
    process_before <- read_json(coder_window_process_path(p, "ADSL", "ADSL.AGE", "primary"))
    calls$items <- list()
    append_side_note(p, "ADSL", "ADSL.AGE", "primary", "Keep the specified age source")
    question <- "Why does this variable use the subject key?"
    decision <- agent_decide(with_cfg(), p, "ADSL", "ADSL.AGE", "primary", question)
    expect_null(decision$change)
    expect_length(calls$items, 1L)
    spec <- read_project_spec_dir(p)
    graph <- load_graph(p, "ADSL")
    node <- graph_node(graph, "ADSL.AGE")
    current <- read_text(code_path)
    notes <- read_side_notes(p, "ADSL", "ADSL.AGE", "primary")
    upstream <- agent_upstream_vars(graph, node)
    expected_system <- paste(side_generation_system_prompt("primary", spec, graph, node, p, chat = TRUE),
                             agent_chat_format_instruction(), sep = "\n")
    expected_user <- paste0(side_user_prompt("primary", spec, graph, node, upstream,
      project = p, current_code = current, notes = notes, chat = TRUE), "MESSAGE_BEGIN\n", question, "\nMESSAGE_END\n")
    generation_user <- side_user_prompt("primary", spec, graph, node, upstream,
      project = p, current_code = current, notes = notes)
    expect_identical(calls$items[[1]]$system, expected_system)
    expect_identical(calls$items[[1]]$user, expected_user)
    for (shared in c(spec_rows_text(spec, "ADSL", "AGE"),
                     paste0("CURRENT_CODE_BEGIN\n", current, "\nCURRENT_CODE_END"),
                     paste0("NOTES_BEGIN\n1. ", notes[[1]]$text, "\nNOTES_END"),
                     expert_thread_text(p, "ADSL", "primary", node))) {
      expect_match(generation_user, shared, fixed = TRUE)
      expect_match(calls$items[[1]]$user, shared, fixed = TRUE)
    }
    expect_false(grepl("CHAT_REPLY_FORMAT_BEGIN", calls$items[[1]]$user, fixed = TRUE))
    expect_false(grepl("REQUIRED_PROGRAM_OPENING|Your task is only|Return a JSON object", calls$items[[1]]$system, perl = TRUE))
    expect_false(grepl("REQUIRED_PROGRAM_OPENING|Your task is only", calls$items[[1]]$user, perl = TRUE))
    expect_match(calls$items[[1]]$system, "write no code in the reply", fixed = TRUE)
    for (secret in c("VALIDATION_THREAD_SECRET", "VALIDATION_REPLY_SECRET", "VALIDATION_CODE_SECRET", "VALIDATION_LOG_SECRET",
                     "OTHER_NODE_THREAD_SECRET", "OTHER_NODE_REPLY_SECRET", "SELF_REVIEW_BEGIN"))
      expect_false(grepl(secret, calls$items[[1]]$user, fixed = TRUE))
    latest <- coder_window_read(p, "ADSL", "ADSL.AGE", "primary")
    expect_identical(latest$context$task, "chat")
    expect_identical(latest$context$system, calls$items[[1]]$system)
    expect_identical(latest$context$user, calls$items[[1]]$user)
    expect_identical(read_text(code_path), code_before)
    expect_identical(latest$process[seq_along(process_before)], process_before)
    expect_true(any(vapply(latest$process, function(x) identical(x$event, "Chat finished"), TRUE)))
    expect_false(grepl("SELF_REVIEW_BEGIN|PREVIOUS_PROGRAM_ERROR", latest$context$user, perl = TRUE))
  })
})

test_that("Coder chat resolves Records nodes from the graph before building prompts", {
  with_coder_fake(function(calls, home) {
    for (layered in c(FALSE,TRUE)) {
      fx <- chat_records_project(layered)
      id <- if (layered) "ADVS.RECORDS@SYSBP" else "ADAE.RECORDS"
      decision <- agent_decide(with_cfg(),fx$project,fx$dataset,id,"primary","Explain this node and its existing code.")
      expect_true(nzchar(decision$reply))
      context <- coder_window_read(fx$project,fx$dataset,id,"primary")$context
      expect_identical(context$task,"chat")
      expect_match(context$user,"VAR: KEYS",fixed=TRUE)
      expect_match(context$user,if(layered)"KEYS: USUBJID,PARAMCD" else "KEYS: USUBJID,AESEQ",fixed=TRUE)
      expect_false(grepl("VAR: RECORDS",context$user,fixed=TRUE))
      expect_false(grepl("duplicate key rows",paste(context$system,context$user),fixed=TRUE))
      expect_match(context$system,"VAR: KEYS",fixed=TRUE)
      expect_match(context$system,if(layered)"KEYS: USUBJID,PARAMCD" else "KEYS: USUBJID,AESEQ",fixed=TRUE)
    }
  })
})

test_that("known dataset-level kind adds its dataset-level chat context", {
  fx <- chat_records_project(TRUE)
  id <- "ADVS.STUDYID"
  known <- graph_node(fx$graph,id)
  expect_identical(known$kind,"dataset_variable")
  with_coder_fake(function(calls, home) {
    decision <- agent_decide(with_cfg(),fx$project,fx$dataset,id,"primary","Explain this variable.")
    expect_true(nzchar(decision$reply))
    after <- coder_window_read(fx$project,fx$dataset,id,"primary")$context$user
    expect_match(after,"DATASET_LEVEL_NODE: one row per USUBJID",fixed=TRUE)
  })
})

test_that("chat retries a code-shaped reply once and never saves returned code", {
  p <- coder_session_project()
  with_coder_fake(function(calls, home) {
    original <- get("llm_chat", envir = .GlobalEnv)
    replies <- c('{"code":"result <- 1"}', '{"reply":"Hello!","change":null}')
    assign("llm_chat", function(cfg, system, user, ...) {
      calls$items[[length(calls$items) + 1L]] <- list(system = system, user = user)
      list(text = replies[[length(calls$items)]], model = "fake-llm")
    }, envir = .GlobalEnv)
    on.exit(assign("llm_chat", original, envir = .GlobalEnv), add = TRUE)
    code_path <- file.path(side_run_dir(p, "ADSL", "ADSL.AGE", "primary"), "code.R")
    before <- read_text(code_path)
    answer <- agent_decide(with_cfg(), p, "ADSL", "ADSL.AGE", "primary", "hello")
    expect_identical(answer$reply, "Hello!")
    expect_length(calls$items, 2L)
    expect_match(calls$items[[2L]]$user, "CHAT_RETRY_REMINDER", fixed = TRUE)
    expect_identical(read_text(code_path), before)
  })
})

test_that("chat reports code-shaped output after its one retry without writing it", {
  p <- coder_session_project()
  with_coder_fake(function(calls, home) {
    original <- get("llm_chat", envir = .GlobalEnv)
    assign("llm_chat", function(cfg, system, user, ...) {
      calls$items[[length(calls$items) + 1L]] <- list(system = system, user = user)
      list(text = '{"code":"result <- 2"}', model = "fake-llm")
    }, envir = .GlobalEnv)
    on.exit(assign("llm_chat", original, envir = .GlobalEnv), add = TRUE)
    code_path <- file.path(side_run_dir(p, "ADSL", "ADSL.AGE", "primary"), "code.R")
    before <- read_text(code_path)
    expect_error(agent_decide(with_cfg(), p, "ADSL", "ADSL.AGE", "primary", "hello"),
                 "code instead of a chat reply after one retry")
    expect_length(calls$items, 2L)
    expect_identical(read_text(code_path), before)
  })
})

test_that("Coder chat starts before code exists", {
  p <- coder_session_project()
  with_coder_fake(function(calls, home) {
    unlink(file.path(side_run_dir(p, "ADSL", "ADSL.AGE", "primary"), "code.R"))
    decision <- agent_decide(with_cfg(), p, "ADSL", "ADSL.AGE", "primary", "What is this variable?")
    expect_true(nzchar(decision$reply))
    expect_null(decision$change)
    expect_length(calls$items, 1L)
    expect_false(grepl("CURRENT_CODE_BEGIN", calls$items[[1]]$user, fixed = TRUE))
    expect_identical(coder_window_read(p, "ADSL", "ADSL.AGE", "primary")$context$task, "chat")
    expect_false(file.exists(api_key_file()))
  })
})

test_that("Coder window route rejects a missing or invalid side", {
  p <- temp_project()
  A <- create_app(normalizePath(file.path(testthat::test_path(), "..", "..")), p,
                  7993L, "coder-window-test", load_config())
  expect_equal(coder_window_call(A, "/api/node/coder-window?dataset=ADSL&node=ADSL.AGE&side=both")$status, 400L)
})

test_that("SU frame context is metadata only and peek_frame samples at most twenty rows", {
  p <- coder_session_project(); d <- side_run_dir(p,"ADSL","ADSL.AGE","primary"); dir.create(d,recursive=TRUE,showWarnings=FALSE)
  frame <- data.frame(USUBJID=paste0("S",seq_len(5000)),AGE=seq_len(5000),NOTE="FRAME_SECRET_MARKER")
  saveRDS(frame,file.path(d,"out.rds"))
  ctx <- su_context(p,"ADSL","ADSL.AGE","primary")
  expect_identical(ctx$frames[["ADSL/AGE/primary"]]$rows,5000L)
  expect_false(grepl("FRAME_SECRET_MARKER",to_json(ctx),fixed=TRUE))
  original <- llm_chat; prompts <- character(); n <- 0L
  assign("llm_chat",function(cfg,system,user,...) { n <<- n+1L; prompts <<- c(prompts,user); if(n==1L)list(text=json_text(list(tool="peek_frame",args=list(name="ADSL/AGE/primary"))),model="fake") else list(text='{"tool":"reply","text":"Peeked."}',model="fake") },.GlobalEnv)
  on.exit(assign("llm_chat",original,.GlobalEnv),add=TRUE)
  out <- su_tool_loop(list(),p,"ADSL","ADSL.AGE","primary",list(list(role="user",text="Inspect frame")))
  expect_identical(out$reply,"Peeked.")
  expect_lte(nchar(prompts[[1]]),100000L)
  expect_false(grepl("FRAME_SECRET_MARKER",prompts[[1]],fixed=TRUE))
  expect_true(grepl("FRAME_SECRET_MARKER",prompts[[2]],fixed=TRUE))
  tool <- sub("^peek_frame: ","",out$calls[[1]])
  expect_lte(nrow(jsonlite::fromJSON(tool)),20L)
})

test_that("SU prompt documents every real tool and warns on outside-lineage reads", {
  p <- coder_session_project()
  original <- llm_chat; seen <- NULL
  assign("llm_chat", function(cfg, system, user, ...) {
    seen <<- system
    list(text='{"tool":"reply","text":"Done."}', model="fake")
  }, .GlobalEnv)
  on.exit(assign("llm_chat", original, .GlobalEnv), add=TRUE)
  out <- su_tool_loop(list(), p, "ADSL", "ADSL.AGE", "primary", list())
  expect_identical(out$reply, "Done.")
  documented <- grep("^[a-z_]+:", strsplit(seen, "\\n")[[1]], value=TRUE)
  prompt_tools <- sub(":.*$", "", documented)
  body_text <- paste(deparse(body(su_tool_loop)), collapse="\n")
  handled <- unique(regmatches(body_text, gregexpr('tool\\s*==\\s*"[a-z_]+"', body_text, perl=TRUE))[[1]])
  handled <- sub('^tool=="|"$', "", handled)
  handled <- gsub('^tool\\s*==\\s*"|"$', "", handled)
  expect_setequal(prompt_tools, handled)
  expect_false("request_lineage_exemption" %in% prompt_tools)
  expect_match(seen, "These tools are real", fixed=TRUE)
  expect_match(seen, "the app warns", fixed=TRUE)
  expect_match(seen, "Do not print the app's warning", fixed=TRUE)
  expect_false(grepl("Lineage warning: BASE reads", seen, fixed=TRUE))
  expect_match(seen, "Missing column: X is not in LINEAGE_SYSBP or the provided data.", fixed=TRUE)
  expect_false(grepl("DUALDAG_MISSING_DEPENDENCY", seen, fixed=TRUE))
})

test_that("SU private copies include allowed files and exclude cross-side artifacts", {
  p <- coder_session_project(); own <- side_run_dir(p,"ADSL","ADSL.AGE","primary"); other <- side_run_dir(p,"ADSL","ADSL.AGE","validation")
  dir.create(own,recursive=TRUE,showWarnings=FALSE); dir.create(other,recursive=TRUE,showWarnings=FALSE)
  write_text("OWN_RUN_MARKER",file.path(own,"code.R")); write_text("OTHER_RUN_MARKER",file.path(other,"code.R"))
  write_text("SPEC_MARKER",file.path(p,"spec","marker.txt")); write_text("DATA_MARKER",file.path(p,"data","marker.txt"))
  dir.create(file.path(p,".dualdag","state"),recursive=TRUE,showWarnings=FALSE); write_text("STATE_MARKER",file.path(p,".dualdag","state","marker"))
  dir.create(file.path(p,".dualdag","llm_log"),recursive=TRUE,showWarnings=FALSE); write_text("LOG_MARKER",file.path(p,".dualdag","llm_log","marker"))
  write_text("COMPARE_MARKER",file.path(node_run_dir(p,"ADSL","ADSL.AGE"),"compare.json"))
  write_text("QC_MARKER",file.path(node_run_dir(p,"ADSL","ADSL.AGE"),"qc_report.md"))
  tmp <- su_private_copy(p,"primary"); on.exit(unlink(tmp,recursive=TRUE),add=TRUE)
  expect_true(file.exists(file.path(tmp,".dualdag","state","marker")))
  expect_true(file.exists(file.path(tmp,"spec","marker.txt")))
  expect_true(file.exists(file.path(tmp,"data","marker.txt")))
  expect_true(file.exists(file.path(tmp,".dualdag","runs","ADSL","AGE","primary","code.R")))
  expect_false(file.exists(file.path(tmp,".dualdag","runs","ADSL","AGE","validation","code.R")))
  expect_false(dir.exists(file.path(tmp,".dualdag","llm_log")))
  expect_false(file.exists(file.path(tmp,".dualdag","runs","ADSL","AGE","compare.json")))
  expect_false(file.exists(file.path(tmp,".dualdag","runs","ADSL","AGE","qc_report.md")))
})

test_that("SU run bookkeeping flags descendants and does not invent L2 without the other side", {
  p <- coder_session_project()
  g <- make_graph("ADSL",list(gn("ADSL.USUBJID"),gn("ADSL.AGE"),gn("ADSL.WEIGHT")),list(
    list(from="ADSL.USUBJID",to="ADSL.AGE",evidence="key",origin="user"),
    list(from="ADSL.AGE",to="ADSL.WEIGHT",evidence="derived",origin="user")))
  save_graph(p,g)
  st <- get_status(open_project(p),"ADSL","ADSL.WEIGHT"); st$level <- "L2"; put_status(open_project(p),"ADSL","ADSL.WEIGHT",st)
  dir <- side_run_dir(p,"ADSL","ADSL.AGE","primary"); dir.create(dir,recursive=TRUE,showWarnings=FALSE)
  saveRDS(data.frame(USUBJID="S1",AGE=21),file.path(dir,"out.rds"))
  updated <- apply_stored_run(open_project(p),"ADSL","ADSL.AGE",list(primary=list(ok=TRUE,error=NULL)))
  expect_identical(updated$level,"none")
  child <- get_status(open_project(p),"ADSL","ADSL.WEIGHT")
  expect_true("ADSL.AGE" %in% child$upstream_changed)
  expect_false(file.exists(file.path(node_run_dir(p,"ADSL","ADSL.AGE"),"compare.json")))
  expect_false(file.exists(file.path(node_run_dir(p,"ADSL","ADSL.AGE"),"qc_report.md")))
})

test_that("SU job uses the configured coder provider, model, and API key", {
  p <- coder_session_project(); home <- tempfile("su-cfg-home-"); dir.create(home)
  vars <- c("DUALDAG_HOME","DUALDAG_LLM_API_KEY","DUALDAG_FAKE_LLM")
  old <- Sys.getenv(vars,unset=NA_character_); on.exit({ for(nm in vars) { value <- old[[nm]]; if(identical(nm,"DUALDAG_HOME")) restore_test_env(nm,value) else if(is.na(value))Sys.unsetenv(nm) else do.call(Sys.setenv,setNames(list(value),nm)) } },add=TRUE)
  Sys.setenv(DUALDAG_HOME=home,DUALDAG_LLM_API_KEY="mock-coder-key"); Sys.unsetenv("DUALDAG_FAKE_LLM")
  save_config(modifyList(config_defaults(),list(provider="anthropic",model="mock-sonnet")))
  expected <- with_cfg(); captured <- NULL; original <- llm_chat
  assign("llm_chat",function(cfg,system,user,...) { captured <<- cfg; list(text='{"tool":"reply","text":"mocked"}',model="mock") },.GlobalEnv)
  on.exit(assign("llm_chat",original,.GlobalEnv),add=TRUE)
  out <- su_job(p,"ADSL","ADSL.AGE","primary",list(list(role="user",text="hello")),"hello",10)
  expect_identical(out$reply,"mocked")
  expect_true(nzchar(captured$api_key))
  expect_identical(captured$api_key,"mock-coder-key")
  expect_identical(captured$provider,expected$provider)
  expect_identical(captured$model,expected$model)
})

test_that("SU job accepts a CLI provider without an API key", {
  p <- coder_session_project(); home <- tempfile("su-cli-home-"); dir.create(home)
  vars <- c("DUALDAG_HOME","DUALDAG_LLM_API_KEY","DUALDAG_FAKE_LLM")
  old <- Sys.getenv(vars,unset=NA_character_); on.exit({ for(nm in vars) { value <- old[[nm]]; if(identical(nm,"DUALDAG_HOME")) restore_test_env(nm,value) else if(is.na(value))Sys.unsetenv(nm) else do.call(Sys.setenv,setNames(list(value),nm)) } },add=TRUE)
  Sys.setenv(DUALDAG_HOME=home); Sys.unsetenv(c("DUALDAG_LLM_API_KEY","DUALDAG_FAKE_LLM"))
  save_config(modifyList(config_defaults(),list(provider="codex_cli",model="mock-codex")))
  old_notice <- getOption("dualdag.cli_tool_notice")
  options(dualdag.cli_tool_notice="parent notice")
  on.exit(options(dualdag.cli_tool_notice=old_notice),add=TRUE)
  captured <- NULL; observed_notice <- NULL; original <- llm_chat
  assign("llm_chat",function(cfg,system,user,...) { captured <<- cfg; observed_notice <<- getOption("dualdag.cli_tool_notice"); list(text='{"tool":"reply","text":"CLI mocked"}',model="mock") },.GlobalEnv)
  on.exit(assign("llm_chat",original,.GlobalEnv),add=TRUE)
  out <- su_job(p,"ADSL","ADSL.AGE","primary",list(list(role="user",text="hello")),"hello",10)
  expect_identical(out$reply,"CLI mocked")
  expect_identical(captured$provider,"codex_cli")
  expect_identical(captured$model,"mock-codex")
  expect_false(nzchar(captured$api_key))
  expect_identical(observed_notice,"You have no tools of your own and no workspace. The app runs the tools listed above when you return their JSON. Reply now with only one JSON object.")
  expect_identical(getOption("dualdag.cli_tool_notice"),"parent notice")
})
