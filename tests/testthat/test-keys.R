keys_test_root <- normalizePath(file.path(testthat::test_path(),"..",".."),winslash="/")
source(file.path(testthat::test_path(),"..","fixtures","helper.R"))
for(.keys_f in list.files(file.path(keys_test_root,"R"),pattern="[.]R$",full.names=TRUE))sys.source(.keys_f,envir=.GlobalEnv)

keys_test_project <- function() {
  p<-tempfile("compound-keys-");dir.create(file.path(p,"spec"),recursive=TRUE);dir.create(file.path(p,"data"))
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADAE,USUBJID,Subject,Char,20,1,ADSL.USUBJID,",
    "ADAE,AESEQ,Event sequence,Num,8,2,AE.AESEQ,",
    "ADAE,SEQPLUS,Sequence copy,Num,8,,ADAE.AESEQ,"),file.path(p,"spec","variable.csv"))
  haven::write_xpt(data.frame(USUBJID=c("A","B","A"),AESEQ=c(1,NA,2)),file.path(p,"data","ae.xpt"))
  haven::write_xpt(data.frame(USUBJID=c("A","B","D")),file.path(p,"data","adsl.xpt"))
  p
}

keys_pump_batch <- function(A,timeout=120) {
  started<-Sys.time()
  repeat {
    later::run_now()
    if(is.list(A$batch)&&isTRUE(A$batch$finished))return(A$batch)
    if(as.numeric(difftime(Sys.time(),started,units="secs"))>timeout)stop("Timed out waiting for compound-key batch.")
    Sys.sleep(.05)
  }
}

test_that("layered ADVS keeps keys only on Records and no USUBJID node", {
  advs<-file.path(keys_test_root,"examples/pharmaverse-demo/spec/advs")
  spec<-read_fixture_spec(file.path(advs,"variable.csv"),file.path(advs,"parameter.csv"))
  g<-graph_skeleton(spec,"ADVS")
  expect_equal(sum(vapply(g$nodes,function(n)identical(n$kind,"dataset_variable"),TRUE)),10L)
  expect_false("ADVS.USUBJID"%in%vapply(g$nodes,`[[`,"","id"))
  expect_true(all(vapply(g$layers,function(l)length(l$members)==22L,TRUE)))
})

test_that("single-key ADSL retains its legacy graph and prompt shape", {
  adsl<-file.path(keys_test_root,"examples/pharmaverse-demo/spec/adsl")
  spec<-read_fixture_spec(file.path(adsl,"variable.csv"),file.path(adsl,"parameter.csv"))
  g<-graph_skeleton(spec,"ADSL")
  ids<-vapply(g$nodes,`[[`,"","id")
  expect_false(isTRUE(g$layered))
  expect_false(any(vapply(g$nodes,function(n)identical(n$kind,"records"),TRUE)))
  expect_identical(ids,paste0("ADSL.",toupper(spec$variables$Variable)))
  prompt<-paste(vapply(extract_spec_rows(spec,"ADSL"),function(r)r$block,""),collapse="\n")
  expect_false(grepl("RECORD_KEY_ROWS|ADSL.RECORDS",prompt))
  expect_true(grepl("Variable: USUBJID",prompt,fixed=TRUE))
  side<-side_user_prompt("primary",spec,g,graph_node(g,"ADSL.AGE"),"USUBJID",character())
  expect_false(grepl("RECORDS_NODE|RECORD_KEY_ROWS",side))
  expect_true(grepl("LINEAGE_KEYS: USUBJID",side,fixed=TRUE))
  fake_code<-jsonlite::fromJSON(fake_generate_reply("TASK: generate_primary",side),simplifyVector=FALSE)$code
  expect_match(side,"LINEAGE_DATASET: LINEAGE_ADSL",fixed=TRUE)
  expect_match(side,"This program is one step in a chain",fixed=TRUE)
  expect_match(fake_code,"result <-",fixed=TRUE)
  save_graph(p<-open_project(tempfile("legacy-byte-")),g)
  expect_identical(unname(tools::md5sum(graph_path(p,"ADSL"))),"660d90ec5d240ed91b7b216d6870b6dd")
  p<-open_project(tempfile("legacy-plan-"))
  plan<-fullrun_plan(p,"ADSL", "primary",g,spec)
  expect_false(isTRUE(plan$multi_key_records))
  expect_false(any(grepl("RECORDS",plan$ids,fixed=TRUE)))
})

test_that("fresh multi-key extraction, batch, NA compare and full run use one Records node", {
  old<-Sys.getenv(c("DUALDAG_HOME","DUALDAG_FAKE_LLM","DUALDAG_LLM_API_KEY"));home<-tempfile("keys-home-");dir.create(home)
  Sys.setenv(DUALDAG_HOME=home,DUALDAG_FAKE_LLM="1",DUALDAG_LLM_API_KEY="sk-test")
  on.exit({for(nm in names(old))if(!nzchar(old[[nm]]))Sys.unsetenv(nm)else do.call(Sys.setenv,setNames(list(old[[nm]]),nm))},add=TRUE)
  p<-keys_test_project();spec<-load_project_spec(p)
  g<-extract_dependencies(list(),spec,"ADAE",catalog=build_source_catalog(p),project=p)
  ids<-vapply(g$nodes,`[[`,"","id")
  expect_true("ADAE.RECORDS"%in%ids);expect_false(any(c("ADAE.USUBJID","ADAE.AESEQ")%in%ids))
  expect_true(all(vapply(g$edges,function(e)!grepl("AESEQ",e$from,fixed=TRUE),TRUE)))
  expect_true(any(vapply(g$external,function(e)e$ref=="ADSL.USUBJID"&&"ADAE.RECORDS"%in%unlist(e$used_by),TRUE)))
  save_graph(p,g);note_graph_nodes(open_project(p),"ADAE",g)
  expect_match(extract_plan_prompt("ADAE"),"read the Key column",fixed=TRUE)
  expect_match(paste(vapply(extract_spec_rows(spec,"ADAE"),function(r)r$block,""),collapse="\n"),"Variable: AESEQ [^\n]*Key: 2[^\n]*\nSource: AE\\.AESEQ",perl=TRUE)
  key_node <- graph_node(g,"ADAE.RECORDS")
  for(side in c("primary","validation")) {
    generation <- side_generation_system_prompt(side,spec,g,key_node,p)
    user <- side_user_prompt(side,spec,g,key_node,character(),project=p,self_review=list(code="old",issues=list("Check key columns")))
    chat <- side_system_prompt(side,spec,"ADAE",node=key_node,project=p,chat=TRUE)
    ask <- ask_user_prompt(side,spec,g,key_node,"Why are there these rows?",project=p)
    for(prompt in c(generation,user,chat,ask)) {
      expect_match(prompt,"VAR: KEYS",fixed=TRUE)
      expect_match(prompt,"KEYS: USUBJID,AESEQ",fixed=TRUE)
      expect_false(grepl("(?:VAR: *RECORDS|\\$RECORDS)",prompt,perl=TRUE))
    }
    expect_match(generation,"duplicate key rows",fixed=TRUE)
    expect_match(generation,"distinct",fixed=TRUE)
  }
  primary_user <- side_user_prompt("primary",spec,g,key_node,character(),project=p)
  primary_code <- jsonlite::fromJSON(fake_generate_reply(side_generation_system_prompt("primary",spec,g,key_node,p),primary_user),simplifyVector=FALSE)$code
  expect_match(primary_code,'LINEAGE_ADAE <- result',fixed=TRUE)
  expect_match(primary_code,'duplicate key rows:',fixed=TRUE)
  expect_match(primary_code,'LINEAGE_ADAE[["USUBJID"]]',fixed=TRUE)
  expect_false(grepl('result[["USUBJID"]]',sub("(?s)^.*LINEAGE_ADAE <- result\\n","",primary_code,perl=TRUE),fixed=TRUE))
  primary_run <- run_side(p,"ADAE",key_node,"primary",primary_code,graph=g,spec=spec)
  expect_true(primary_run$ok,info=primary_run$error %||% primary_run$log)
  expect_match(primary_run$log,"rows:",fixed=TRUE)
  expect_match(primary_run$log,"duplicate key rows:",fixed=TRUE)
  expect_match(primary_run$log,"USUBJID type:",fixed=TRUE)
  expect_match(primary_run$log,"AESEQ type:",fixed=TRUE)
  A<-create_app(keys_test_root,p,7993L,"keys-token",load_config());on.exit(shutdown_app(A),add=TRUE)
  requested<-batch_order_for(A$project,"ADAE",ids)
  expect_identical(requested[[1]],"ADAE.RECORDS")
  start_batch(A,"ADAE",requested,TRUE);batch<-keys_pump_batch(A)
  expect_equal(batch$done,length(ids))
  expect_false(any(vapply(batch$results,function(r)!isTRUE(r$ok),TRUE)),info=paste(capture.output(str(batch$results)),collapse="\n"))
  record<-graph_node(g,"ADAE.RECORDS")
  expect_identical(get_status(A$project,"ADAE","ADAE.RECORDS")$level,"L3")
  records<-readRDS(file.path(side_run_dir(p,"ADAE",record,"primary"),"out.rds"))
  expect_true(anyNA(records$AESEQ))
  expect_true(compare_node(p,"ADAE",record,dataset_keys(spec,"ADAE"))$match)
  for(side in c("primary","validation")) {
    full<-fullrun_worker(p,"ADAE",side,timeout=90)
    expect_true(full$ok,info=full$error %||% "")
    out<-readRDS(file.path(fullrun_dir(p,"ADAE",side),"result.rds"))
    expect_identical(names(out),c("USUBJID","AESEQ","SEQPLUS"))
    expect_equal(nrow(out),3L)
    expect_equal(out$SEQPLUS,c(1,NA_real_,2))
  }
  layer_keys_write(p,"ADAE",list("*"="SEQPLUS"))
  override_prompt <- side_generation_system_prompt("primary",spec,g,key_node,p)
  expect_match(override_prompt,"KEYS: USUBJID,SEQPLUS",fixed=TRUE)
  expect_false(grepl('LINEAGE_ADAE[["AESEQ"]]',override_prompt,fixed=TRUE))
  expect_true(grepl('LINEAGE_ADAE[["SEQPLUS"]]',override_prompt,fixed=TRUE))
})

test_that("saved legacy compound-key graphs load unchanged and request re-extraction", {
  p<-keys_test_project();spec<-load_project_spec(p);proj<-open_project(p)
  legacy<-list(dataset="ADAE",layered=FALSE,nodes=list(
    list(id="ADAE.USUBJID",var="USUBJID",kind="variable",paramcd=NULL),
    list(id="ADAE.AESEQ",var="AESEQ",kind="variable",paramcd=NULL),
    list(id="ADAE.SEQPLUS",var="SEQPLUS",kind="variable",paramcd=NULL)),
    edges=list(list(from="ADAE.AESEQ",to="ADAE.SEQPLUS")))
  save_graph(proj,legacy)
  loaded<-load_graph(p,"ADAE")
  expect_identical(loaded$nodes,legacy$nodes)
  expect_true(dataset_reextract_required(spec,"ADAE",loaded))
  single<-spec_of(V("ADSL","USUBJID",key=1),V("ADSL","AGE"))
  expect_false(dataset_reextract_required(single,"ADSL",graph_skeleton(single,"ADSL")))
})
