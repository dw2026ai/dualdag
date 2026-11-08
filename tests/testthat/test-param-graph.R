app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash="/")
for(f in c("utils.R","deps.R","config.R","llm.R","dag.R","spec.R","project.R","dataset_types.R","deps_extract.R","inspect.R","sides.R","dag_agent.R","param_graph.R","server.R")) sys.source(file.path(app_root,"R",f),envir=globalenv())
source(file.path(testthat::test_path(),"..","fixtures","helper.R"))
param_api_call <- function(A,path,method="GET",body=NULL) {
  q<-""; if(grepl("?",path,fixed=TRUE)){q<-sub("^[^?]*\\?","",path);path<-sub("\\?.*$","",path)}
  raw<-if(is.null(body))raw() else charToRaw(as.character(to_json(body)))
  req<-list(PATH_INFO=path,REQUEST_METHOD=method,QUERY_STRING=q,HTTP_HOST=sprintf("127.0.0.1:%d",A$port),HTTP_X_DUALDAG_TOKEN=A$token,CONTENT_TYPE="application/json",rook.input=list(read=function()raw))
  make_handlers(A)$call(req)
}
param_wait <- function(promise, timeout = 5) {
  done <- FALSE
  value <- NULL
  promises::then(promise, onFulfilled = function(result) {
    value <<- result
    done <<- TRUE
  }, onRejected = function(error) {
    value <<- error
    done <<- TRUE
  })
  deadline <- Sys.time() + timeout
  while (!done && Sys.time() < deadline) later::run_now(0.05)
  if (!done) stop("Timed out waiting for promise.")
  if (inherits(value, "condition")) stop(conditionMessage(value))
  value
}

test_that("ADVS parameter extraction builds independent graph", {
  p<-open_project(tempfile("param-")); file.copy(file.path(app_root,"examples/pharmaverse-demo/spec/advs/variable.csv"),file.path(p$dir,"spec/variable.csv"),overwrite=TRUE); file.copy(file.path(app_root,"examples/pharmaverse-demo/spec/advs/parameter.csv"),file.path(p$dir,"spec/parameter.csv"),overwrite=TRUE)
  sp<-read_project_spec(p); g<-with_fake_llm(param_extract(list(),sp,"ADVS",p$dir))
  expect_equal(length(g$nodes),8L); expect_true(all(vapply(g$nodes,function(n)n$defined,TRUE)))
  pairs<-vapply(g$edges,function(e)paste(e$from,e$to,sep="->"),"")
  expect_setequal(pairs,c("WEIGHT->BMI","HEIGHT->BMI","WEIGHT->WTLS15DT"))
  secret<-file.path(p$dir,"secret.R"); writeLines("SECRET_CODE",secret)
  prompt<-param_graph_prompt(sp,"ADVS"); expect_false(grepl("SECRET_CODE",prompt,fixed=TRUE)); expect_false(grepl("secret.R",prompt,fixed=TRUE))
})

test_that("layer order is stable and reports parameter cycles", {
  spec <- list(parameters=data.frame(PARAMCD=c("A","B","C","D"), PARAM=c("a","b","c","d"),
    Dataset="X", Source="", Derivation="", stringsAsFactors=FALSE))
  pg <- list(edges=list(list(from="B",to="D"),list(from="A",to="D")))
  expect_equal(param_layer_order(spec,"X",pg)$order,c("A","B","C","D"))
  cyc <- list(edges=list(list(from="B",to="D"),list(from="D",to="B")))
  expect_setequal(param_layer_order(spec,"X",cyc)$cycle,c("B","D"))
})

test_that("fake layered Records program filters parameter records and emits PARAMCD", {
  prompt <- paste(c("VAR: KEYS", "PARAMCD: WT", "KEYS: USUBJID",
    "PARAMETER_ROW_BEGIN", "PARAMCD: WT", "SOURCE: VS.VSTESTCD = 'WT'", "PARAMETER_ROW_END",
    "KEYS_NODE: return exactly the key columns for PARAMCD WT", "KEY_COLUMNS_BEGIN",
    "KEY\tUSUBJID\tVS.USUBJID\tCopy", "KEY\tPARAMCD\tVS.VSTESTCD\tCopy", "KEY_COLUMNS_END"), collapse="\n")
  reply <- jsonlite::fromJSON(fake_generate_reply("TASK: generate_primary", prompt), simplifyVector=FALSE)
  expect_no_error(parse(text=reply$code))
  env <- new.env(parent=globalenv()); env$keys <- c("USUBJID","PARAMCD"); env$src <- list(VS=data.frame(USUBJID=c("A","B"),VSTESTCD=c("WT","HT")))
  eval(parse(text=reply$code),env)
  expect_equal(env$result$USUBJID,"A")
  expect_equal(env$result$PARAMCD,"WT")
  expect_match(reply$code, "duplicate key rows", fixed=TRUE)
  expect_match(reply$code, "distinct", fixed=TRUE)
})

test_that("fake layered derived variable reads named upstream layers", {
  prompt <- paste(c("SIDE: primary","DATASET: ADVS","VAR: AVAL","PARAMCD: BMI","KEYS: USUBJID,PARAMCD,AVISITN,ATPTN",
    "SOURCE: ADVS.AVAL", "DERIVATION: WEIGHT divided by HEIGHT squared", "LAYER_VARIABLE: build BMI",
    "LAYER_KEY_ROWS_BEGIN", "LAYER_KEY\tUSUBJID\tVS.USUBJID", "RECORD_KEY_ROWS_END"),collapse="\n")
  reply <- jsonlite::fromJSON(fake_generate_reply("TASK: generate_primary",prompt),simplifyVector=FALSE)
  expect_true(grepl("layers[['WEIGHT']]",reply$code,fixed=TRUE))
  expect_no_error(parse(text=reply$code))
})

test_that("trimmed real ADVS parameter reply keeps non-key uses and drops variable endpoints", {
  advs <- file.path(app_root,"examples/pharmaverse-demo/spec/advs")
  spec <- read_fixture_spec(file.path(advs,"variable.csv"),file.path(advs,"parameter.csv"))
  parsed <- jsonlite::fromJSON(fixture_path("layers_1c_advs_param.json"),simplifyVector=FALSE)
  old <- get("llm_chat",envir=.GlobalEnv)
  assign("llm_chat",function(cfg,system,user,...) list(text=json_text(parsed),model="fixture"),envir=.GlobalEnv)
  on.exit(assign("llm_chat",old,envir=.GlobalEnv),add=TRUE)
  graph <- param_extract(list(),spec,"ADVS")
  expect_true(grepl("from and to must be PARAMCDs",param_graph_system(),fixed=TRUE))
  edge <- Filter(function(x)x$from=="WEIGHT" && x$to=="BMI",graph$edges)[[1]]
  expect_true(any(vapply(edge$uses,function(use) identical(use,list(from_var="AVAL",to_var="AVAL")),TRUE)))
  expect_false(any(vapply(edge$uses,function(use) use$from_var %in% c("USUBJID","PARAMCD") || use$to_var %in% c("USUBJID","PARAMCD"),TRUE)))
  expect_false(any(vapply(graph$edges,function(x)x$to=="AVAL" || x$from=="AVAL",TRUE)))
  expect_true(any(grepl("ADSL.TRTSDT",unlist(graph$warnings),fixed=TRUE)))
})

test_that("parameter edges extract, validate and apply variable uses", {
  p<-open_project(tempfile("param-uses-"))
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation","X,AVAL,Value,Num,8,,,","X,CHG,Change,Num,8,,,"),file.path(p$dir,"spec/variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation","X,A,Alpha,,","X,B,Beta,,"),file.path(p$dir,"spec/parameter.csv"))
  sp<-read_project_spec(p)
  old<-get("llm_chat",envir=.GlobalEnv)
  assign("llm_chat",function(cfg,system,user,...) list(text=json_text(list(edges=list(list(from="A",to="B",evidence="quoted",uses=list(list(from_var="CHG",to_var="AVAL"),list(from_var="BOGUS",to_var="AVAL")))),undefined=list())),model="test"),envir=.GlobalEnv)
  on.exit(assign("llm_chat",old,envir=.GlobalEnv),add=TRUE)
  g<-param_extract(list(),sp,"X",p)
  expect_equal(g$edges[[1]]$uses,list(list(from_var="CHG",to_var="AVAL")))
  expect_true(any(grepl("BOGUS",unlist(g$warnings))))
  param_graph_write(p,g)
  expect_error(param_graph_apply(p,"X",list(list(op="add_use",from="A",to="B",from_var="BOGUS",to_var="AVAL"))),"Unknown variable")
  param_graph_apply(p,"X",list(list(op="add_use",from="A",to="B",from_var="AVAL",to_var="CHG")))
  expect_equal(length(param_graph_read(p,"X")$edges[[1]]$uses),2L)
  expect_length(dag_agent_read(p,"X")$param_edits,1L)
})

test_that("parameter layer ordering collapses carry members to their shared group", {
  p <- open_project(tempfile("group-order-"))
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation","X,AVAL,Value,Num,8,,,"),file.path(p$dir,"spec/variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation","X,CO1,Carry one,,","X,CO2,Carry two,,","X,DER,Derived,,"),file.path(p$dir,"spec/parameter.csv"))
  spec <- read_project_spec(p)
  pg <- list(carry_members=list("CO1","CO2"),edges=list(list(from="CO1",to="DER"),list(from="CO2",to="DER")))
  expect_identical(param_layer_order(spec,"X",pg)$order,c("CARRY_OVER","DER"))
  expect_null(param_layer_blocker(NULL,"X","primary","CARRY_OVER",spec,pg))
})

test_that("layer order preserves colliding semantic parameters beside carry group", {
  p <- open_project(tempfile("carry-collision-order-"))
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation","X,AVAL,Value,Num,8,,,"),file.path(p$dir,"spec/variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation","X,CO1,Carry one,,","X,CARRY_OVER,Real carry,,","X,PARAM_CARRY_OVER,Real collision,,","X,DER,Derived,,"),file.path(p$dir,"spec/parameter.csv"))
  spec <- read_project_spec(p)
  skeleton <- graph_skeleton(spec,"X",carry_members="CO1")
  real_layer <- Filter(function(x) identical(x$members_parameters,list("CARRY_OVER")), skeleton$layers)[[1]]$paramcd
  expect_identical(real_layer,"PARAM_CARRY_OVER_2")
  pg <- list(carry_members=list("CO1"),edges=list())
  expect_identical(param_layer_order(spec,"X",pg)$order,c("CARRY_OVER","PARAM_CARRY_OVER_2","PARAM_CARRY_OVER","DER"))
  expect_true(real_layer %in% param_layer_order(spec,"X",pg)$order)
  pg$edges <- list(list(from="CO1",to="DER"),list(from="CARRY_OVER",to="DER"))
  graph <- skeleton
  save_graph(p,graph)
  write_json(list(X="BDS"),dataset_type_path(p))
  group_ids <- vapply(Filter(function(n) identical(n$paramcd,"CARRY_OVER"),graph$nodes),`[[`,"","id")
  for (id in group_ids) put_status(p,"X",id,modifyList(blank_status(),list(level="L3")))
  blocked <- param_layer_blocker(p$dir,"X","primary","DER",spec,pg)
  expect_identical(blocked$layer,real_layer)
  expect_null(param_layer_blocker(p$dir,"X","primary",real_layer,spec,pg))
})

test_that("old layered candidates load unchanged and request re-extraction without resetting status", {
  p<-open_project(tempfile("old-graph-"))
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation","X,AVAL,Value,Num,8,,,"),file.path(p$dir,"spec/variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation","X,P,Parameter,,"),file.path(p$dir,"spec/parameter.csv"))
  old<-list(dataset="X",spec_hash="old",nodes=list(list(id="X.AVAL",var="AVAL",paramcd=NULL,kind="variable",categories=list())),groups=list(),edges=list(),external=list(),unresolved=list())
  save_graph(p,old)
  bytes<-readBin(graph_path(p,"X"),"raw",n=file.info(graph_path(p,"X"))$size)
  st<-get_status(p,"X","X.AVAL"); st$level<-"L2"; put_status(p,"X","X.AVAL",st)
  payload<-state_payload(list(project=p,jobs=list()))
  expect_true(payload$datasets$X$reextract_required)
  expect_null(payload$datasets$X$graph$layered)
  expect_identical(get_status(p,"X","X.AVAL")$level,"L2")
  expect_identical(readBin(graph_path(p,"X"),"raw",n=file.info(graph_path(p,"X"))$size),bytes)
})

test_that("saved parameter edges without uses load with empty mappings", {
  p<-open_project(tempfile("legacy-uses-"))
  write_json(list(dataset="X",nodes=list(),edges=list(list(from="A",to="B",evidence="old"))),param_graph_path(p,"X"))
  expect_equal(param_graph_read(p,"X")$edges[[1]]$uses,list())
})

test_that("parameter graph rejects cycles and unknown codes, preserves corrections", {
  p<-open_project(tempfile("param-")); writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation","X,AVAL,Value,Num,8,,,"),file.path(p$dir,"spec/variable.csv")); writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation","X,A,Alpha,,","X,B,Beta,,"),file.path(p$dir,"spec/parameter.csv")); sp<-read_project_spec(p)
  param_graph_write(p,list(dataset="X",spec_hash=sp$hash,nodes=list(list(code="A",label="Alpha",defined=TRUE,mentioned_in=list()),list(code="B",label="Beta",defined=TRUE,mentioned_in=list())),edges=list(list(from="A",to="B",evidence="e",origin="llm")),error=NULL))
  expect_error(param_graph_apply(p,"X",list(list(op="add_edge",from="B",to="A"))),"cycle")
  expect_error(param_graph_apply(p,"X",list(list(op="add_edge",from="NOPE",to="A"))),"Unknown parameter")
  param_graph_apply(p,"X",list(list(op="add_undefined",code="FOO")))
  again<-param_graph_read(p,"X"); expect_false(again$nodes[[3]]$defined); expect_equal(again$nodes[[3]]$mentioned_in[[1]],"user")
  expect_true(length(param_corrections(p,"X"))>0)
})

test_that("parameter prompt excludes anything but spec rows and no-parameter extraction skips model", {
  p<-open_project(tempfile("param-")); writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation","X,AVAL,Value,Num,8,,,"),file.path(p$dir,"spec/variable.csv")); sp<-read_project_spec(p)
  g<-with_fake_llm(param_extract(list(),sp,"X",p$dir)); expect_length(g$nodes,0L); expect_length(g$edges,0L)
  expect_equal(param_graph_prompt(sp,"X"),"DATASET\tX\nPARAMETERS\nVARIABLES\nVAR\tAVAL\tValue\t\t")
})

test_that("undefined references are CSV-verified and unsafe edges are dropped", {
  p<-open_project(tempfile("param-")); writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation","X,AVAL,Value,Num,8,,AVAL@FOO,derived from FOO"),file.path(p$dir,"spec/variable.csv")); writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation","X,A,Alpha,,","X,B,Beta,,"),file.path(p$dir,"spec/parameter.csv")); sp<-read_project_spec(p)
  graph_file<-file.path(p$state_dir,"graph_X.json"); writeLines('{"untouched":true}',graph_file); before<-readBin(graph_file,"raw",n=file.info(graph_file)$size)
  old<-get("llm_chat",envir=.GlobalEnv); assign("llm_chat",function(cfg,system,user,...) list(text=json_text(list(undefined=list("FOO","GHOST"),edges=list(list(from="A",to="FOO",evidence="q"),list(from="A",to="A",evidence="self"),list(from="A",to="B",evidence="ab"),list(from="B",to="A",evidence="ba")))),model="test"),envir=.GlobalEnv)
  on.exit(assign("llm_chat",old,envir=.GlobalEnv),add=TRUE)
  g<-param_extract(list(),sp,"X",p); expect_true("FOO"%in%vapply(g$nodes,function(n)n$code,"")); expect_false("GHOST"%in%vapply(g$nodes,function(n)n$code,"")); expect_equal(g$nodes[[3]]$mentioned_in[[1]],"variable.csv: FOO"); expect_length(g$warnings,1L)
  after<-readBin(graph_file,"raw",n=file.info(graph_file)$size); expect_identical(before,after)
})

test_that("parameter re-extraction ignores corrections and keeps only model edges", {
  p <- open_project(tempfile("param-replay-"))
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
               "X,AVAL,Value,Num,8,,,"), file.path(p$dir, "spec/variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation", "X,A,Alpha,,",
               "X,B,Beta,,", "X,C,Gamma,,"), file.path(p$dir, "spec/parameter.csv"))
  spec <- read_project_spec(p)
  param_graph_write(p, list(dataset = "X", spec_hash = spec$hash,
                            nodes = param_defined_nodes(spec, "X"), edges = list(),
                            error = NULL, warnings = list()))
  param_graph_apply(p, "X", list(list(op = "add_edge", from = "B", to = "C")))
  expect_equal(state_payload(list(project=p,jobs=list()))$datasets$X$parameter_graph$manual_edit_count, 1L)
  baseline <- param_graph_prompt(spec, "X")
  old <- get("llm_chat", envir = .GlobalEnv)
  prompt_seen <- NULL
  assign("llm_chat", function(cfg, system, user, ...) {
    prompt_seen <<- user
    list(text = json_text(list(edges = list(list(from = "A", to = "B", evidence = "model")),
                           undefined = list())), model = "fake")
  }, envir = .GlobalEnv)
  on.exit(assign("llm_chat", old, envir = .GlobalEnv), add = TRUE)

  graph <- param_extract(list(), spec, "X", p)
  pairs <- vapply(graph$edges, function(edge) paste(edge$from, edge$to, sep = "->"), "")
  expect_setequal(pairs, "A->B")
  expect_equal(graph$edges[[1]]$origin, "llm")
  expect_identical(prompt_seen, baseline)
  param_clear_manual_edits(p, "X")
  expect_length(param_corrections(p,"X"), 0L)
  expect_length(dag_agent_read(p,"X")$param_edits, 0L)
  payload <- state_payload(list(project=p,jobs=list()))
  expect_equal(payload$datasets$X$parameter_graph$manual_edit_count, 0L)
})

test_that("invalid model codes do not reach the mention regex", {
  p <- open_project(tempfile("param-regex-"))
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
               "X,AVAL,Value,Num,8,,,"), file.path(p$dir, "spec/variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation", "X,A,Alpha,,"),
             file.path(p$dir, "spec/parameter.csv"))
  spec <- read_project_spec(p)
  old <- get("llm_chat", envir = .GlobalEnv)
  assign("llm_chat", function(cfg, system, user, ...) {
    list(text = json_text(list(edges = list(), undefined = list("A(B"))), model = "fake")
  }, envir = .GlobalEnv)
  on.exit(assign("llm_chat", old, envir = .GlobalEnv), add = TRUE)
  graph <- param_extract(list(), spec, "X", p)
  expect_length(graph$nodes, 1L)
})

test_that("parameter ask sends only parameter corrections", {
  p <- open_project(tempfile("param-corrections-"))
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
               "X,AVAL,Value,Num,8,,,"), file.path(p$dir, "spec/variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation", "X,A,Alpha,,"),
             file.path(p$dir, "spec/parameter.csv"))
  spec <- read_project_spec(p)
  param_graph_write(p, list(dataset = "X", spec_hash = spec$hash,
                            nodes = param_defined_nodes(spec, "X"), edges = list(),
                            error = NULL, warnings = list()))
  memory <- dag_agent_read(p, "X")
  memory$corrections <- list(list(at = "now", text = "VARIABLE_ONLY_CORRECTION"))
  memory$param_corrections <- list(list(at = "now", text = "PARAMETER_ONLY_CORRECTION"))
  dag_agent_write(p, "X", memory)
  old <- get("llm_chat", envir = .GlobalEnv)
  prompt <- NULL
  assign("llm_chat", function(cfg, system, user, ...) {
    prompt <<- user
    list(text = json_text(list(answer = "Done.", changes = list(), correction = NULL)), model = "fake")
  }, envir = .GlobalEnv)
  on.exit(assign("llm_chat", old, envir = .GlobalEnv), add = TRUE)
  result <- param_ask(list(), p, "X", "show graph")
  expect_equal(result$usage$calls, 1L)
  expect_equal(tail(dag_agent_read(p, "X")$param_turns, 1)[[1]]$usage$calls, 1L)
  expect_match(prompt, "PARAMETER_ONLY_CORRECTION", fixed = TRUE)
  expect_false(grepl("VARIABLE_ONLY_CORRECTION", prompt, fixed = TRUE))
})

test_that("parameter graph GET and apply API return graph changes and errors", {
  p<-open_project(tempfile("param-api-")); writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation","X,AVAL,Value,Num,8,,,"),file.path(p$dir,"spec/variable.csv")); writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation","X,A,Alpha,,","X,B,Beta,,"),file.path(p$dir,"spec/parameter.csv")); sp<-read_project_spec(p)
  param_graph_write(p,list(dataset="X",spec_hash=sp$hash,nodes=list(list(code="A",label="Alpha",defined=TRUE,mentioned_in=list()),list(code="B",label="Beta",defined=TRUE,mentioned_in=list())),edges=list(),error=NULL,warnings=list()))
  old_home<-Sys.getenv("DUALDAG_HOME",unset=NA_character_); home<-tempfile("param-home-");dir.create(home);Sys.setenv(DUALDAG_HOME=home);on.exit(restore_test_env("DUALDAG_HOME", old_home),add=TRUE)
  A<-create_app(app_root,p$dir,7985L,"param-token",load_config())
  state_path <- file.path(p$state_dir, "state.json")
  state_before <- readBin(state_path, "raw", n = file.info(state_path)$size)
  variable_graph_path <- graph_path(p, "X")
  writeLines('{"variable_graph":"unchanged"}', variable_graph_path)
  graph_before <- readBin(variable_graph_path, "raw", n = file.info(variable_graph_path)$size)
  get<-param_api_call(A,"/api/param-graph?dataset=X"); expect_equal(get$status,200L); expect_false(from_json(get$body)$stale)
  bad<-param_api_call(A,"/api/param-graph/apply","POST",list(dataset="X",changes=list(list(op="add_edge",from="NO",to="A"))));expect_equal(bad$status,400L)
  ok<-param_api_call(A,"/api/param-graph/apply","POST",list(dataset="X",changes=list(list(op="add_edge",from="A",to="B",evidence="API"))));expect_equal(ok$status,200L);expect_equal(from_json(ok$body)$graph$edges[[1]]$origin,"user")
  expect_identical(readBin(state_path, "raw", n = file.info(state_path)$size), state_before)
  expect_identical(readBin(variable_graph_path, "raw", n = file.info(variable_graph_path)$size), graph_before)
})

test_that("extract API accepts undefined CSV mentions and ask changes wait for apply", {
  p <- open_project(tempfile("param-api-flow-"))
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
               "X,AVAL,Value,Num,8,,,derived from FOO"), file.path(p$dir, "spec/variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation", "X,A,Alpha,,", "X,B,Beta,,"),
             file.path(p$dir, "spec/parameter.csv"))
  old_home <- Sys.getenv("DUALDAG_HOME", unset = NA_character_)
  old_fake <- Sys.getenv("DUALDAG_FAKE_LLM", unset = NA_character_)
  home <- tempfile("param-api-home-")
  dir.create(home)
  Sys.setenv(DUALDAG_HOME = home, DUALDAG_FAKE_LLM = "1")
  on.exit({
    restore_test_env("DUALDAG_HOME", old_home)
    if (is.na(old_fake)) Sys.unsetenv("DUALDAG_FAKE_LLM") else Sys.setenv(DUALDAG_FAKE_LLM = old_fake)
  }, add = TRUE)
  app <- create_app(app_root, p$dir, 7986L, "param-flow-token", load_config())
  original_launch <- get("launch", envir = .GlobalEnv)
  assign("launch", function(A, fn_name, args, timeout) {
    promises::promise(function(resolve, reject) {
      tryCatch(resolve(do.call(get(fn_name, envir = .GlobalEnv), args)), error = reject)
    })
  }, envir = .GlobalEnv)
  on.exit(assign("launch", original_launch, envir = .GlobalEnv), add = TRUE)

  extracted <- param_wait(param_api_call(app, "/api/param-graph/extract", "POST",
                                         list(dataset = "X")))
  extracted <- from_json(extracted$body)
  expect_true("FOO" %in% vapply(extracted$nodes, function(node) node$code, ""))
  expect_equal(extracted$nodes[[3]]$mentioned_in[[1]], "variable.csv: FOO")

  asked <- param_wait(param_api_call(app, "/api/param-graph/ask", "POST",
                                     list(dataset = "X", message = "add edge A -> B")))
  asked <- from_json(asked$body)
  expect_equal(asked$changes[[1]]$op, "add_edge")
  expect_length(param_graph_read(p, "X")$edges, 0L)
  applied <- param_api_call(app, "/api/param-graph/apply", "POST",
                            list(dataset = "X", changes = asked$changes))
  expect_equal(applied$status, 200L)
  expect_length(param_graph_read(p, "X")$edges, 1L)
})

test_that("successful Parameter DAG extraction seeds the grouped Variable DAG skeleton", {
  p <- open_project(tempfile("param-initial-group-"))
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
               "X,USUBJID,Subject,Char,20,1,,", "X,PARAMCD,Parameter,Char,8,2,,",
               "X,AVAL,Value,Num,8,,,", "X,VISIT,Visit,Char,20,,,"), file.path(p$dir, "spec/variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation", "X,CO1,Carry one,,",
               "X,CO2,Carry two,,", "X,DER,Derived,,"), file.path(p$dir, "spec/parameter.csv"))
  old_home <- Sys.getenv("DUALDAG_HOME", unset = NA_character_)
  home <- tempfile("param-initial-group-home-"); dir.create(home)
  Sys.setenv(DUALDAG_HOME = home)
  on.exit(restore_test_env("DUALDAG_HOME", old_home), add = TRUE)
  layer_keys_write(p, "X", list(CARRY_OVER="VISIT"))
  app <- create_app(app_root, p$dir, 7985L, "param-initial-group-token", load_config())
  original_launch <- get("launch", envir = .GlobalEnv)
  assign("launch", function(A, fn_name, args, timeout) promises::promise(function(resolve, reject) {
    resolve(list(dataset="X", nodes=list(), edges=list(), carry_members=list("CO1", "CO2"),
      classification=list(parameters=list(
        list(paramcd="CO1", classification="carry_over", reason="shared source"),
        list(paramcd="CO2", classification="carry_over", reason="shared source"),
        list(paramcd="DER", classification="derived", reason="derived")))))
  }), envir = .GlobalEnv)
  on.exit(assign("launch", original_launch, envir = .GlobalEnv), add = TRUE)

  param_wait(start_param_extract(app, "X", wait=TRUE))
  graph <- load_graph(p, "X")
  expect_identical(vapply(graph$layers, `[[`, "", "paramcd"), c("CARRY_OVER", "DER"))
  carry <- graph$layers[[1]]
  expect_identical(carry$members_parameters, list("CO1", "CO2"))
  expect_setequal(graph$layer_keys$CARRY_OVER, c("USUBJID", "PARAMCD", "VISIT"))
  expect_true("X.AVAL@CARRY_OVER" %in% carry$members)
  expect_false(any(grepl("@(CO1|CO2)$", vapply(graph$nodes, `[[`, "", "id"))))
  expect_null(graph$carry_over_regenerate_required)
  single <- graph_skeleton(read_project_spec(p), "X", carry_members="CO1")
  expect_identical(single$layers[[1]]$paramcd, "CARRY_OVER")
  expect_identical(single$layers[[1]]$members_parameters, list("CO1"))
  no_group <- graph_skeleton(read_project_spec(p), "X")
  expect_identical(vapply(no_group$layers, `[[`, "", "paramcd"), c("CO1", "CO2", "DER"))
})

test_that("parameter extraction failure leaves the variable graph ungenerated", {
  p <- open_project(tempfile("param-failure-"))
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
               "X,AVAL,Value,Num,8,,,"), file.path(p$dir, "spec/variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation", "X,A,Alpha,,", "X,B,Beta,,"),
             file.path(p$dir, "spec/parameter.csv"))
  old_home <- Sys.getenv("DUALDAG_HOME", unset = NA_character_)
  home <- tempfile("param-failure-home-")
  dir.create(home)
  Sys.setenv(DUALDAG_HOME = home)
  on.exit(restore_test_env("DUALDAG_HOME", old_home),
          add = TRUE)
  app <- create_app(app_root, p$dir, 7987L, "param-failure-token", load_config())
  original_launch <- get("launch", envir = .GlobalEnv)
  assign("launch", function(A, fn_name, args, timeout) {
    promises::promise(function(resolve, reject) {
      if (identical(fn_name, "param_extract_job")) {
        reject(simpleError("parameter extraction failed"))
      } else {
        spec <- read_project_spec(args$project_dir)
        resolve(graph_skeleton(spec, args$dataset))
      }
    })
  }, envir = .GlobalEnv)
  on.exit(assign("launch", original_launch, envir = .GlobalEnv), add = TRUE)

  param_wait(start_extract(app, "X", wait = TRUE))
  expect_false(file.exists(graph_path(p, "X")))
  failed <- param_graph_read(p, "X")
  expect_equal(failed$error, "parameter extraction failed")
  expect_equal(failed$nodes[[1]]$code, "A")
  expect_equal(failed$nodes[[2]]$code, "B")

  expect_false(file.exists(graph_path(p, "X")))
})
