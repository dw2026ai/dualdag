source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))
sys.source(file.path(testthat::test_path(),"..","..","R","project.R"),envir=.GlobalEnv)
sys.source(file.path(testthat::test_path(),"..","..","R","config.R"),envir=.GlobalEnv)
sys.source(file.path(testthat::test_path(),"..","..","R","coder_window.R"),envir=.GlobalEnv)

task48_layered_project <- function(dataset_level=FALSE) {
  p <- open_project(temp_project())
  spec <- spec_of(V("X","USUBJID",key=1,source="VS.USUBJID"),
    V("X","PARAMCD",key=2,source="VS.VSTESTCD"),
    if(dataset_level) V("X","STUDYID",source="ADSL.STUDYID") else NULL,
    V("X","AVAL",type="Num",source="VS.VSSTRESN"), params=list(P("X","P",source="VS.VSTESTCD",derivation="Keep matching rows.")))
  graph <- graph_skeleton(spec,"X")
  save_graph(p,graph)
  list(p=p,spec=spec,graph=graph)
}

test_that("handling questions are last and scoped to layered parameter variables", {
  fx <- task48_layered_project(dataset_level=TRUE)
  node <- graph_node(fx$graph, "X.AVAL@P")
  for (side in c("primary", "validation")) {
    u <- side_user_prompt(side, fx$spec, fx$graph, node, character(), project=fx$p)
    expect_true(endsWith(u, "HANDLING_QUESTION_END"))
    expect_match(u, "Current parameter: P. Current variable: AVAL.", fixed=TRUE)
    if (side == "primary") expect_match(u, "from which LINEAGE_ datasets", fixed=TRUE)
    else expect_match(u, "You may use any available source", fixed=TRUE)
    expect_match(side_generation_system_prompt(side,fx$spec,fx$graph,node,fx$p), '"approach"', fixed=TRUE)
  }
  for (id in c("X.STUDYID", "X.RECORDS@P")) {
    n <- graph_node(fx$graph,id)
    u <- side_user_prompt("primary",fx$spec,fx$graph,n,character(),project=fx$p)
    sys <- side_generation_system_prompt("primary",fx$spec,fx$graph,n,fx$p)
    expect_false(grepl("HANDLING_QUESTION_BEGIN",u,fixed=TRUE))
    expect_match(sys, 'Return a JSON object {"code":"<R script>","same_source":true|false,"description":"2-3 sentences on meaning, type/units, source/rule, and missingness"}. No prose outside the JSON.', fixed=TRUE)
  }
})

test_that("approach is generated, side local, shown after reload, and included in rewrite review", {
  fx <- task48_layered_project()
  node <- graph_node(fx$graph,"X.AVAL@P")
  with_fake_llm({
    generation_user <- side_user_prompt("primary",fx$spec,fx$graph,node,character(),project=fx$p)
    fake_reply <- parse_json_reply(fake_generate_reply(side_generation_system_prompt("primary",fx$spec,fx$graph,node,fx$p),generation_user))
    expect_true(nzchar(fake_reply$approach))
    code <- generate_side(list(),"primary",fx$spec,fx$graph,node,character(),project=fx$p)
    expect_identical(attr(code,"approach"),fake_reply$approach)
    expect_false(grepl("APPROACH",side_user_prompt("validation",fx$spec,fx$graph,node,character(),project=fx$p),fixed=TRUE))
    rew <- side_user_prompt("primary",fx$spec,fx$graph,node,character(),project=fx$p,current_code="old",
      self_review=list(code="old",approach=attr(code,"approach"),issues=list("check")))
    expect_match(rew,"Answer the question again for this rewrite. Do not copy the previous answer.",fixed=TRUE)
    expect_match(rew,attr(code,"approach"),fixed=TRUE)
    expect_equal(sum(gregexpr("PREVIOUS_APPROACH",rew,fixed=TRUE)[[1]] > 0),1L)
    saved_dir <- side_run_dir(fx$p$dir,"X",node,"primary")
    dir.create(saved_dir,recursive=TRUE,showWarnings=FALSE)
    write_json(list(approach=attr(code,"approach")),file.path(saved_dir,"approach.json"))
    chat_rewrite <- side_user_prompt("primary",fx$spec,fx$graph,node,character(),project=fx$p,
      current_code="manually kept current code",user_instruction="please revise")
    expect_match(chat_rewrite,"Answer the question again for this rewrite. Do not copy the previous answer.",fixed=TRUE)
    expect_match(chat_rewrite,attr(code,"approach"),fixed=TRUE)
    expect_equal(sum(gregexpr("PREVIOUS_APPROACH",chat_rewrite,fixed=TRUE)[[1]] > 0),1L)
    generate_side(list(),"primary",fx$spec,fx$graph,node,character(),project=fx$p,
      current_code="manually kept current code",user_instruction="please revise")
    saved_attempts <- read_json(coder_window_context_path(fx$p,"X",node$id,"primary"))
    expect_match(saved_attempts[[length(saved_attempts)]]$user,"Answer the question again for this rewrite. Do not copy the previous answer.",fixed=TRUE)
    expect_match(saved_attempts[[length(saved_attempts)]]$user,attr(code,"approach"),fixed=TRUE)
  })
  sdir <- side_run_dir(fx$p$dir,"X",node,"primary")
  dir.create(sdir,recursive=TRUE,showWarnings=FALSE)
  write_json(list(approach="Approach persisted."),file.path(sdir,"approach.json"))
  write_text("code",file.path(sdir,"code.R")); write_text("log",file.path(sdir,"log.txt"))
  coder_session_start(fx$p,"X",node$id,"primary")
  coder_window_prompt(fx$p,"X",node$id,"primary","system", "user")
  coder_window_approach(fx$p,"X",node$id,"primary","Approach persisted.")
  expect_identical(coder_window_read(fx$p,"X",node$id,"primary")$context$approach,"Approach persisted.")
  coder_session_snapshot_run(fx$p,"X",node$id,"primary")
  run_records <- coder_sessions_list(fx$p,"X",node$id,"primary")
  expect_identical(coder_session_full(fx$p,run_records[[1]]$id)$runs[[1]]$approach,"Approach persisted.")
  had_notes <- exists("read_side_notes",mode="function",inherits=FALSE)
  old_notes <- if (had_notes) get("read_side_notes",envir=.GlobalEnv) else NULL
  if (!had_notes) assign("read_side_notes",function(...)list(),envir=.GlobalEnv)
  on.exit(if(had_notes)assign("read_side_notes",old_notes,envir=.GlobalEnv)else rm("read_side_notes",envir=.GlobalEnv),add=TRUE)
  review <- self_review_prompt(fx$p$dir,"X",node,"primary",fx$spec,fx$graph)
  expect_match(review,"Does the code do what approach says?",fixed=TRUE)
  expect_match(review,"Approach persisted.",fixed=TRUE)
})

test_that("lookup attempts only show an approach notice on the final reply", {
  fx <- task48_layered_project(); node <- graph_node(fx$graph,"X.AVAL@P")
  original <- if (exists("llm_reply_with_lookup",mode="function")) get("llm_reply_with_lookup",.GlobalEnv) else NULL
  assign("llm_reply_with_lookup",function(cfg,system,user,spec,graph,dataset,max_tokens,allowed_ids,on_call) {
    on_call(system,user,"started"); on_call(system,user,"finished")
    lookup_user <- paste0(user,"\nLOOKUP_ROUND 1\nlooked up a definition")
    on_call(system,lookup_user,"started"); on_call(system,lookup_user,"finished")
    parsed <- list(approach="Final handling answer.",code="LINEAGE_P$AVAL <- 1",same_source=FALSE,description="Test result.")
    list(raw=list(text=json_text(parsed)),parsed=parsed,usage=NULL)
  },.GlobalEnv)
  on.exit(if(is.null(original))rm("llm_reply_with_lookup",envir=.GlobalEnv)else assign("llm_reply_with_lookup",original,.GlobalEnv),add=TRUE)
  generate_side(list(),"primary",fx$spec,fx$graph,node,character(),project=fx$p$dir)
  attempts <- read_json(coder_window_context_path(fx$p,"X",node$id,"primary"))
  expect_identical(vapply(attempts,function(x)x$task,""),c("generation","lookup round"))
  expect_false(isTRUE(attempts[[1]]$final_reply))
  expect_null(attempts[[1]]$approach)
  expect_true(isTRUE(attempts[[2]]$final_reply))
  expect_identical(attempts[[2]]$approach,"Final handling answer.")
  selected_first <- coder_window_read(fx$p,"X",node$id,"primary",attempt=1)$context
  selected_final <- coder_window_read(fx$p,"X",node$id,"primary",attempt=2)$context
  expect_false(isTRUE(selected_first$final_reply))
  expect_true(isTRUE(selected_final$final_reply))
  js <- paste(readLines(file.path(testthat::test_path(),"..","..","www","app.js"),warn=FALSE),collapse="\n")
  expect_match(js,"context.final_reply === true",fixed=TRUE)
})

test_that("missing and non-text approaches never block code generation", {
  fx <- task48_layered_project(); node <- graph_node(fx$graph,"X.AVAL@P")
  old <- fake_generate_reply
  on.exit(assign("fake_generate_reply",old,envir=.GlobalEnv),add=TRUE)
  assign("fake_generate_reply",function(system,user)json_text(list(code="LINEAGE_P$AVAL <- 1",same_source=FALSE,description="test")),.GlobalEnv)
  with_fake_llm({
    code <- generate_side(list(),"primary",fx$spec,fx$graph,node,character(),project=fx$p$dir)
    expect_identical(as.character(code),"LINEAGE_P$AVAL <- 1")
    expect_identical(attr(code,"approach"),"")
    expect_identical(paste(code,collapse="\n"),"LINEAGE_P$AVAL <- 1")
    assign("fake_generate_reply",function(system,user)json_text(list(approach=list("first","second"),code="LINEAGE_P$AVAL <- 2",same_source=FALSE,description="test")),.GlobalEnv)
    coerced <- generate_side(list(),"primary",fx$spec,fx$graph,node,character(),project=fx$p$dir)
    expect_identical(attr(coerced,"approach"),"first second")
  })
  js <- readLines(file.path(testthat::test_path(),"..","..","www","app.js"),warn=FALSE)
  expect_true(any(grepl("No approach was returned for this generation",js,fixed=TRUE)))
})
