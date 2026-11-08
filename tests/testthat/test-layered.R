layered_test_root <- normalizePath(file.path(testthat::test_path(),"..",".."),winslash="/")
for(.f in list.files(file.path(layered_test_root,"R"),pattern="\\.R$",full.names=TRUE))sys.source(.f,envir=.GlobalEnv)

layered_test_project <- function(codes=c("P","Q","BMI"),edges=NULL,dataset_level=FALSE,dataset="X") {
  p<-tempfile("layered-");dir.create(file.path(p,"spec"),recursive=TRUE)
  var_rows<-c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    paste(dataset,"USUBJID,Subject,Char,20,1,VS.USUBJID,Copy VS.USUBJID.",sep=","),
    paste(dataset,"PARAMCD,Parameter,Char,8,2,VS.VSTESTCD,Parameter code.",sep=","),
    if(dataset_level)paste(dataset,"STUDYID,Study,Char,8,,ADSL.STUDYID,Study identifier.",sep=",")else character(),
    paste(dataset,"AVAL,Value,Num,8, ,VS.VSSTRESN,Copy source measurement.",sep=","),paste(dataset,"BVAL,Other value,Num,8,,,",sep=","))
  writeLines(var_rows,file.path(p,"spec","variable.csv"))
  params<-vapply(codes,function(pc)if(pc=="BMI")paste0(dataset,",",pc,",",pc,",",dataset,".AVAL,Combine WEIGHT and HEIGHT")else paste0(dataset,",",pc,",",pc,",VS.VSTESTCD; VS.VSSTRESN,Keep VS rows with VS.VSTESTCD = '",pc,"'."),"")
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation",params),file.path(p,"spec","parameter.csv"))
  proj<-open_project(p);spec<-load_project_spec(p);g<-graph_skeleton(spec,dataset)
  g$edges<-unlist(lapply(codes,function(pc)list(list(from=paste0(dataset,".RECORDS@",pc),to=paste0(dataset,".AVAL@",pc),evidence="records"),
    list(from=paste0(dataset,".RECORDS@",pc),to=paste0(dataset,".BVAL@",pc),evidence="records"))),recursive=FALSE)
  save_graph(proj,g)
  if(is.null(edges)&&"BMI"%in%codes&&length(codes)>=3L)edges<-lapply(codes[1:2],function(pc)list(from=pc,to="BMI",uses=list(list(from_var="AVAL",to_var="AVAL"))))
  pg<-list(dataset=dataset,nodes=param_defined_nodes(spec,dataset),edges=edges%||%list(),error=NULL,warnings=list());param_graph_write(proj,pg)
  list(p=p,proj=proj,spec=spec,graph=g,param=pg,dataset=dataset)
}

layered_test_write_result <- function(fx,id,side,df) {
  path<-file.path(side_run_dir(fx$p,fx$dataset %||% "X",id,side),"out.rds");dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE);saveRDS(df,path)
}

test_that("Data viewer parameter frames stay side local and exclude failed outputs", {
  fx <- layered_test_project(c("P", "Q"), dataset_level=TRUE)
  records_p <- "X.RECORDS@P"
  records_q <- "X.RECORDS@Q"
  for (id in c(records_p, records_q)) {
    subs <- if (identical(id, records_p)) c("S1", "S2") else c("S3", "S4")
    layered_test_write_result(fx, id, "primary", data.frame(USUBJID=subs, PARAMCD=sub(".*@", "", id)))
    put_status(fx$proj, "X", id, modifyList(blank_status(), list(level="L2", sides=list(primary=list(ok=TRUE), validation=list(ok=FALSE)))))
  }
  keys_only <- viewer_generated_frame(fx$proj, "X", "primary", "P")
  expect_equal(names(keys_only), c("USUBJID", "PARAMCD"))
  expect_equal(nrow(keys_only), 2L)
  layered_test_write_result(fx, "X.AVAL@P", "primary", data.frame(USUBJID="S1", PARAMCD="P", AVAL=10))
  put_status(fx$proj, "X", "X.AVAL@P", modifyList(blank_status(), list(level="L2", sides=list(primary=list(ok=TRUE), validation=list(ok=FALSE)))))
  layered_test_write_result(fx, "X.BVAL@P", "primary", data.frame(USUBJID="S1", PARAMCD="P", BVAL=20))
  put_status(fx$proj, "X", "X.BVAL@P", modifyList(blank_status(), list(level="none", sides=list(primary=list(ok=FALSE), validation=list(ok=FALSE)))))
  layered_test_write_result(fx, "X.AVAL@Q", "primary", data.frame(USUBJID="S4", PARAMCD="Q", AVAL=99))
  put_status(fx$proj, "X", "X.AVAL@Q", modifyList(blank_status(), list(level="L2", sides=list(primary=list(ok=TRUE), validation=list(ok=FALSE)))))
  layered_test_write_result(fx, "X.STUDYID", "primary", data.frame(USUBJID=c("S1", "S2"), STUDYID="STUDY"))
  put_status(fx$proj, "X", "X.STUDYID", modifyList(blank_status(), list(level="L2", sides=list(primary=list(ok=TRUE), validation=list(ok=FALSE)))))

  p_frame <- viewer_generated_frame(fx$proj, "X", "primary", "P")
  q_frame <- viewer_generated_frame(fx$proj, "X", "primary", "Q")
  v_frame <- viewer_generated_frame(fx$proj, "X", "validation", "P")
  expect_equal(names(p_frame), c("USUBJID", "PARAMCD", "AVAL", "STUDYID"))
  expect_equal(nrow(p_frame), 2L)
  expect_equal(p_frame$AVAL, c(10, NA_real_))
  expect_false("BVAL" %in% names(p_frame))
  expect_setequal(q_frame$USUBJID, c("S3", "S4"))
  expect_equal(q_frame$AVAL[!is.na(q_frame$AVAL)], 99)
  expect_null(v_frame)
  expect_equal(layered_work_frame(fx$p, "X", "primary", graph_node(fx$graph, records_p), fx$graph, fx$spec, include_generated=TRUE), p_frame)
})

test_that("cached Data frame metadata and rows refresh when a side result changes", {
  fx <- layered_test_project(c("P"))
  records <- "X.RECORDS@P"
  layered_test_write_result(fx, records, "primary", data.frame(USUBJID="S1", PARAMCD="P"))
  put_status(fx$proj, "X", records, modifyList(blank_status(), list(level="L2", sides=list(primary=list(ok=TRUE)))))
  value_id <- "X.AVAL@P"
  layered_test_write_result(fx, value_id, "primary", data.frame(USUBJID="S1", PARAMCD="P", AVAL=1))
  put_status(fx$proj, "X", value_id, modifyList(blank_status(), list(level="L2", sides=list(primary=list(ok=TRUE)))))
  viewer_cache_reset()
  first <- viewer_generated_frame(fx$p, "X", "primary", "P")
  expect_equal(first$AVAL, 1)
  g <- fx$graph
  meta1 <- viewer_generated_dimensions(fx$p, "X", "primary", "P", g, fx$spec, state_nodes(fx$p,"X"))
  expect_equal(c(meta1$nrow, meta1$ncol), c(1L, 3L))

  path <- file.path(side_run_dir(fx$p, "X", value_id, "primary"), "out.rds")
  saveRDS(data.frame(USUBJID="S1", PARAMCD="P", AVAL=2), path)
  Sys.setFileTime(path, Sys.time() + 2)
  second <- viewer_generated_frame(fx$p, "X", "primary", "P")
  meta2 <- viewer_generated_dimensions(fx$p, "X", "primary", "P", g, fx$spec, state_nodes(fx$p,"X"))
  expect_equal(second$AVAL, 2)
  expect_equal(c(meta2$nrow, meta2$ncol), c(meta1$nrow, meta1$ncol))
  expect_false(identical(meta2$updated, meta1$updated))
})

test_that("layered primary check block and target writes pass while other columns remain blocked", {
  fx <- layered_test_project(c("P"))
  records <- "X.RECORDS@P"
  layered_test_write_result(fx, records, "primary", data.frame(USUBJID=c("A","B"),PARAMCD="P"))
  put_status(fx$proj,"X",records,modifyList(blank_status(),list(level="L3")))
  node <- graph_node(fx$graph,"X.AVAL@P")
  system <- side_generation_system_prompt("primary",fx$spec,fx$graph,node,fx$p)
  lines <- strsplit(system,"\n",fixed=TRUE)[[1]]
  first <- grep('^cat\\("rows:"',lines)[[1]]
  last <- grep("^The check prints the row count",lines)[[1]]-1L
  block <- gsub("X","AVAL",lines[first:last],fixed=TRUE)
  code <- paste(c('LINEAGE_P <- data.frame(USUBJID=c("A","B"), PARAMCD="P", AVAL=c(1,2))',block),collapse="\n")
  ran <- run_side(fx$p,"X",node,"primary",code,graph=fx$graph,spec=fx$spec)
  expect_true(ran$ok,info=ran$error %||% ran$log)
  assigned <- run_side(fx$p,"X",node,"primary",
    'LINEAGE_P <- data.frame(USUBJID="A", PARAMCD="P", AVAL=1)\nLINEAGE_P$AVAL <- LINEAGE_P$AVAL + 1',graph=fx$graph,spec=fx$spec)
  expect_true(assigned$ok,info=assigned$error %||% assigned$log)
  blocked <- run_side(fx$p,"X",node,"primary",
    'LINEAGE_P <- data.frame(USUBJID="A", PARAMCD="P", AVAL=LINEAGE_P$BVAL)',graph=fx$graph,spec=fx$spec)
  expect_false(blocked$ok)
  expect_match(blocked$error,"arguments imply differing number of rows",fixed=TRUE)
})

test_that("shared lineage selection keeps the target anchor separate from supplied upstream context", {
  fx <- layered_test_project(c("P", "Q", "BMI"), dataset_level=TRUE)
  fx$graph$edges <- list(
    list(from = "X.AVAL@P", to = "X.AVAL@BMI"),
    list(from = "X.BVAL@P", to = "X.AVAL@P"),
    list(from = "X.AVAL@Q", to = "X.AVAL@BMI")
  )
  for (side in c("primary", "validation")) for (id in c("X.AVAL@P", "X.BVAL@P", "X.AVAL@Q")) {
    col <- sub("^.*\\.", "", sub("@.*$", "", id))
    layered_test_write_result(fx, id, side, data.frame(USUBJID="S1", PARAMCD="P", setNames(list(1), col)))
    card_write(fx$proj, "X", id, side, paste(id, "card"), fx$graph, fx$spec)
  }
  target <- graph_node(fx$graph, "X.AVAL@BMI")
  for (side in c("primary", "validation")) {
    cards <- lineage_context(fx$spec, fx$graph, target, fx$p, side)
    selection <- lineage_selection(fx$graph, target, fx$p)
    expect_identical(selection$target_id, "X.AVAL@BMI")
    expect_false(selection$target_id %in% selection$upstream_ids)
    expect_setequal(selection$ids, c("X.AVAL@BMI", "X.AVAL@P", "X.AVAL@Q", "X.BVAL@P"))
    expect_setequal(selection$ids, lineage_node_ids(fx$graph, target, fx$p))
    expect_setequal(cards$prompt_ids, selection$ids)
    expect_identical(cards$input_roles[[selection$target_id]], NULL)
    expect_identical(unname(cards$roles[[selection$target_id]]), "target")
    for (id in selection$upstream_ids) expect_identical(cards$input_roles[[id]], "card")
    review_edges <- lineage_review_edges(fx$graph, selection$ids, fx$p)
    expect_true(any(vapply(review_edges, function(e) identical(e$from,"X.AVAL@P") && identical(e$to,"X.AVAL@BMI"), TRUE)))
    for (id in c("X.AVAL@P", "X.BVAL@P", "X.AVAL@Q")) expect_match(cards$text, paste0("CARD_BEGIN ",id), fixed=TRUE)
  }
  study <- graph_node(fx$graph, "X.STUDYID")
  study_cards <- lineage_context(fx$spec, fx$graph, study, fx$p, "primary")
  expect_false(grepl("CARD_BEGIN X.AVAL@P", study_cards$text, fixed=TRUE))
  expect_null(study_cards$input_roles[[study$id]])
  card_remove(fx$p, "X", "X.AVAL@Q", "primary")
  fallback <- lineage_context(fx$spec, fx$graph, target, fx$p, "primary")
  expect_identical(fallback$input_roles[["X.AVAL@Q"]], "spec")
  expect_identical(unname(fallback$roles[["X.AVAL@Q"]]), "spec")
  expect_match(fallback$text, "X.AVAL@Q\nROW:", fixed=TRUE)
})

test_that("Records compare treats NA keys as equal and reports differing record keys", {
  fx<-layered_test_project(c("P"))
  id<-"X.RECORDS@P";n<-graph_node(fx$graph,id)
  layered_test_write_result(fx,id,"primary",data.frame(USUBJID=c(NA,"A"),PARAMCD="P",stringsAsFactors=FALSE))
  layered_test_write_result(fx,id,"validation",data.frame(USUBJID=c("A",NA),PARAMCD="P",stringsAsFactors=FALSE))
  same<-compare_node(fx$p,"X",n,"USUBJID");expect_true(same$match)
  layered_test_write_result(fx,id,"validation",data.frame(USUBJID=c("A","B"),PARAMCD="P",stringsAsFactors=FALSE))
  diff<-compare_node(fx$p,"X",n,"USUBJID");expect_false(diff$match);expect_equal(diff$n_diff,2L);expect_match(diff$summary,"primary-only=1, validation-only=1")
  layered_test_write_result(fx,id,"primary",data.frame(USUBJID=c("A","A"),PARAMCD="P"))
  duplicate<-compare_node(fx$p,"X",n,"USUBJID");expect_false(duplicate$match);expect_match(duplicate$summary,"Problem: Records keys do not identify a row")
})

test_that("Records reject the wrong PARAMCD and dataset-level outputs reject duplicate USUBJID", {
  fx<-layered_test_project(c("P"))
  bad<-run_side(fx$p,"X",graph_node(fx$graph,"X.RECORDS@P"),"primary",
    "result <- data.frame(USUBJID='A', PARAMCD='WRONG')",graph=fx$graph,spec=fx$spec)
  expect_false(bad$ok);expect_match(bad$error,"PARAMCD must equal P")
  fx<-layered_test_project(c("P"),dataset_level=TRUE)
  dup<-run_side(fx$p,"X",graph_node(fx$graph,"X.STUDYID"),"primary",
    "result <- data.frame(USUBJID=c('A','A'),STUDYID=c('S1','S2'))",graph=fx$graph,spec=fx$spec)
  expect_false(dup$ok);expect_match(dup$error,"duplicate USUBJID")
})

test_that("unfinished dependency blocks generation before a model call", {
  fx<-layered_test_project(c("P","Q","BMI"))
  put_status(fx$proj,"X","X.RECORDS@P",modifyList(blank_status(),list(level="L3")))
  put_status(fx$proj,"X","X.AVAL@P",modifyList(blank_status(),list(level="L1")))
  called<-0L;old<-get("generate_side",envir=.GlobalEnv);assign("generate_side",function(...) {called<<-called+1L;stop("unexpected model call")},.GlobalEnv)
  on.exit(assign("generate_side",old,.GlobalEnv),add=TRUE)
  expect_error(run_side_job(fx$p,"X","X.RECORDS@BMI","primary",NULL,30),"P.*AVAL is not Data matched")
  expect_error(run_side_job(fx$p,"X","X.RECORDS@BMI","validation",NULL,30),"P.*AVAL is not Data matched")
  expect_equal(called,0L)
})

test_that("layered datasets bypass the legacy flat work refresh", {
  fx<-layered_test_project(c("P"))
  calls<-0L;old<-get("refresh_work_side",envir=.GlobalEnv)
  assign("refresh_work_side",function(...) {calls<<-calls+1L;stop("flat refresh must not run")},.GlobalEnv)
  on.exit(assign("refresh_work_side",old,.GlobalEnv),add=TRUE)
  refresh_work_dataset(fx$p,"X")
  expect_equal(calls,0L)
})

test_that("layer work left-joins dataset values onto Records without adding rows", {
  fx<-layered_test_project(c("P"),dataset_level=TRUE)
  rid<-"X.RECORDS@P";sid<-"X.STUDYID"
  layered_test_write_result(fx,rid,"primary",data.frame(USUBJID=c("A","B","C"),PARAMCD="P"))
  layered_test_write_result(fx,sid,"primary",data.frame(USUBJID=c("A","B","D"),STUDYID=c("S1","S2","ORPHAN")))
  for(id in c(rid,sid))put_status(fx$proj,"X",id,modifyList(blank_status(),list(level="L3")))
  node<-graph_node(fx$graph,"X.AVAL@P")
  work<-layered_work_frame(fx$p,"X","primary",node,fx$graph,fx$spec)
  expect_equal(nrow(work),3L)
  expect_equal(work$USUBJID,c("A","B","C"))
  expect_equal(work$STUDYID,c("S1","S2",NA_character_))
  expect_false("D"%in%work$USUBJID)
})

test_that("dataset-level compare uses shared subjects and reports population differences", {
  fx<-layered_test_project(c("P"),dataset_level=TRUE)
  id<-"X.STUDYID";node<-graph_node(fx$graph,id)
  layered_test_write_result(fx,id,"primary",data.frame(USUBJID=c("A","B","C"),STUDYID=c("A1","B1","C1")))
  layered_test_write_result(fx,id,"validation",data.frame(USUBJID=c("B","C","D"),STUDYID=c("B1","C1","D1")))
  same<-compare_node(fx$p,"X",node,"USUBJID")
  expect_true(same$match)
  expect_equal(same$n_diff,0L)
  expect_equal(same$n_primary,3L);expect_equal(same$n_validation,3L)
  expect_match(same$summary,"primary-only subjects=1, validation-only subjects=1")
  layered_test_write_result(fx,id,"validation",data.frame(USUBJID=c("B","C","D"),STUDYID=c("DIFF","C1","D1")))
  changed<-compare_node(fx$p,"X",node,"USUBJID")
  expect_false(changed$match);expect_equal(changed$n_diff,1L)
})

test_that("batch reports one blocked parameter layer and skips its siblings", {
  fx<-layered_test_project(c("P","Q","BMI"))
  id<-"X.RECORDS@BMI"
  msg<-"Layer BMI is blocked by unfinished layer P: AVAL is not Data matched on primary."
  st<-modifyList(blank_status(),list(level="L1",problem=list(stage="generate",message=msg)))
  put_status(fx$proj,"X",id,st)
  A<-new.env(parent=emptyenv());A$project<-fx$proj
  A$batch<-list(dataset="X",layered=TRUE,current=id,order=c(id,"X.AVAL@BMI","X.BVAL@BMI"),index=2L,
    blocked_layers=list(),reported_blocked_layers=list(),results=list(),done=0L,total=3L,finished=FALSE)
  old<-get("batch_continue",envir=.GlobalEnv);assign("batch_continue",function(A)invisible(),.GlobalEnv)
  on.exit(assign("batch_continue",old,.GlobalEnv),add=TRUE)
  batch_after_generate(A,id,list(status=400L))
  expect_equal(vapply(A$batch$results,`[[`,"","node"),"@layer:BMI")
  expect_equal(A$batch$done,3L)
  expect_true("BMI"%in%names(A$batch$blocked_layers))
})

test_that("batch preflight blocks a Records node before generation when one parent variable is not L3", {
  fx<-layered_test_project(c("P","Q","BMI"));target<-"X.RECORDS@BMI"
  for(id in layer_node_ids(fx$graph,"P"))put_status(fx$proj,"X",id,modifyList(blank_status(),list(level="L3")))
  put_status(fx$proj,"X","X.BVAL@P",modifyList(blank_status(),list(level="L2")))
  for(id in layer_node_ids(fx$graph,"Q"))put_status(fx$proj,"X",id,modifyList(blank_status(),list(level="L3")))
  A<-new.env(parent=globalenv());A$project<-fx$proj;A$jobs<-list()
  A$batch<-list(dataset="X",layered=TRUE,current=NULL,order=c(target,"X.AVAL@BMI","X.BVAL@BMI"),index=1L,
    blocked_layers=list(),reported_blocked_layers=list(),results=list(),done=0L,total=3L,finished=FALSE,busy_step=FALSE)
  generated<-0L;old_generate<-get("start_generate",envir=.GlobalEnv);old_continue<-get("batch_continue",envir=.GlobalEnv)
  assign("start_generate",function(...) {generated<<-generated+1L;stop("generation must be blocked")},.GlobalEnv)
  assign("batch_continue",function(A)invisible(),.GlobalEnv)
  on.exit({assign("start_generate",old_generate,.GlobalEnv);assign("batch_continue",old_continue,.GlobalEnv)},add=TRUE)
  batch_advance(A)
  expect_equal(generated,0L)
  expect_identical(vapply(A$batch$results,`[[`,"","node"),"@layer:BMI")
  expect_match(A$batch$results[[1]]$message,"BVAL is not Data matched")
  expect_equal(A$batch$done,3L)
})

test_that("primary receives only allowed upstream layer columns while validation gets all finished layers", {
  fx<-layered_test_project(c("P","Q","BMI"))
  for(pc in c("P","Q")) {
    rid<-paste0("X.RECORDS@",pc);vid<-paste0("X.AVAL@",pc);bid<-paste0("X.BVAL@",pc)
    for(id in c(rid,vid,bid))put_status(fx$proj,"X",id,modifyList(blank_status(),list(level="L3")))
    layered_test_write_result(fx,rid,"primary",data.frame(USUBJID="A",PARAMCD=pc))
    layered_test_write_result(fx,vid,"primary",data.frame(USUBJID="A",PARAMCD=pc,AVAL=10,SECRET="hidden"))
    layered_test_write_result(fx,bid,"primary",data.frame(USUBJID="A",PARAMCD=pc,BVAL=100))
    layered_test_write_result(fx,rid,"validation",data.frame(USUBJID="A",PARAMCD=pc))
    layered_test_write_result(fx,vid,"validation",data.frame(USUBJID="A",PARAMCD=pc,AVAL=20,SECRET="private"))
    layered_test_write_result(fx,bid,"validation",data.frame(USUBJID="A",PARAMCD=pc,BVAL=200))
  }
  primary<-finished_layers_for_side(fx$p,"X","primary",fx$graph,fx$spec,"BMI")
  expect_equal(names(primary),c("P","Q"));expect_false("SECRET"%in%names(primary$P))
  validation<-finished_layers_for_side(fx$p,"X","validation",fx$graph,fx$spec,"BMI")
  expect_true(all(c("P","Q")%in%names(validation)));expect_true("BVAL"%in%names(validation$P))
  expect_equal(primary$P$AVAL,10);expect_equal(validation$P$AVAL,20)
  node<-graph_node(fx$graph,"X.AVAL@BMI")
  layered_test_write_result(fx,"X.RECORDS@BMI","primary",data.frame(USUBJID="A",PARAMCD="BMI"))
  denied<-start_side_proc(fx$p,"X",node,"primary","result <- data.frame(work[,keys], AVAL=layers[['P']]$BVAL)",fx$graph,fx$spec)
  expect_null(denied$prep$error)
  expect_true(inherits(denied$proc,"process"))
  if (is.function(denied$proc$kill)) denied$proc$kill()
  allowed<-run_side(fx$p,"X",node,"primary","result <- data.frame(work[,keys], AVAL=layers[['P']]$AVAL)",graph=fx$graph,spec=fx$spec)
  expect_true(allowed$ok,info=allowed$error %||% allowed$log)
})

test_that("layered prompts list exactly available same-side datasets", {
  codes <- c("ODD_A", "ODD_B", "ODD_MIX")
  fx <- layered_test_project(codes, edges=list(
    list(from="ODD_A",to="ODD_MIX",uses=list(list(from_var="AVAL",to_var="AVAL"))),
    list(from="ODD_B",to="ODD_MIX",uses=list(list(from_var="AVAL",to_var="AVAL")))), dataset="QZX")
  source_frames <- list(VS=data.frame(USUBJID="SUBJECT_PRIVATE",VSTESTCD="ODD_A",VSSTRESN=777,SECRET_SOURCE="SOURCE_PRIVATE"))
  old_load_src <- get("load_src",.GlobalEnv)
  assign("load_src",function(project)source_frames,.GlobalEnv)
  on.exit(assign("load_src",old_load_src,.GlobalEnv),add=TRUE)
  for (pc in codes[1:2]) {
    for (id in layer_node_ids(fx$graph,pc)) put_status(fx$proj,"QZX",id,modifyList(blank_status(),list(level="L3")))
    for (side in c("primary","validation")) {
      layered_test_write_result(fx,paste0("QZX.RECORDS@",pc),side,data.frame(USUBJID="SUBJECT_PRIVATE",PARAMCD=pc))
      layered_test_write_result(fx,paste0("QZX.AVAL@",pc),side,data.frame(USUBJID="SUBJECT_PRIVATE",PARAMCD=pc,AVAL=42))
    }
    layered_test_write_result(fx,paste0("QZX.BVAL@",pc),"primary",data.frame(USUBJID="SUBJECT_PRIVATE",PARAMCD=pc,BVAL=99))
    layered_test_write_result(fx,paste0("QZX.BVAL@",pc),"validation",data.frame(USUBJID="SUBJECT_PRIVATE",PARAMCD=pc,BVAL=88))
  }
  primary_only <- list(id="QZX.PRIMARY_ONLY@ODD_A",var="PRIMARY_ONLY",paramcd="ODD_A",kind="param_variable")
  fx$graph$nodes <- c(fx$graph$nodes,list(primary_only))
  put_status(fx$proj,"QZX",primary_only$id,modifyList(blank_status(),list(level="L3")))
  layered_test_write_result(fx,primary_only$id,"primary",data.frame(USUBJID="SUBJECT_PRIVATE",PARAMCD="ODD_A",PRIMARY_ONLY=123))
  for (side in c("primary","validation")) {
    layered_test_write_result(fx,"QZX.RECORDS@ODD_MIX",side,data.frame(USUBJID="SUBJECT_PRIVATE",PARAMCD="ODD_MIX"))
  }
  parent_node <- graph_node(fx$graph,"QZX.AVAL@ODD_A")
  fx$graph$edges <- c(fx$graph$edges,list(list(from=parent_node$id,to="QZX.AVAL@ODD_MIX",evidence="layer input")))
  card_write(fx$p,"QZX",parent_node,"primary","PARENT_CARD_SENTINEL",fx$graph,fx$spec)

  rec_primary <- side_user_prompt("primary",fx$spec,fx$graph,graph_node(fx$graph,"QZX.RECORDS@ODD_MIX"),character(),project=fx$p)
  aval_primary <- side_user_prompt("primary",fx$spec,fx$graph,graph_node(fx$graph,"QZX.AVAL@ODD_MIX"),character(),project=fx$p)
  aval_validation <- side_user_prompt("validation",fx$spec,fx$graph,graph_node(fx$graph,"QZX.AVAL@ODD_MIX"),character(),project=fx$p)
  get_available <- function(prompt) {
    lines <- strsplit(prompt,"\\n",fixed=FALSE)[[1]]
    start <- match("AVAILABLE_DATASETS (already loaded in this program; choose what is useful):",lines)
    if (is.na(start)) return("")
    end <- start
    while (end < length(lines) && startsWith(lines[[end+1L]],"- ")) end <- end+1L
    paste(lines[start:end],collapse="\n")
  }
  primary_block <- get_available(aval_primary)
  validation_block <- get_available(aval_validation)
  records_block <- get_available(rec_primary)
  expect_match(records_block,"LINEAGE_ODD_MIX (current; 0 rows): USUBJID, PARAMCD",fixed=TRUE)
  for (block in list(primary_block,validation_block)) {
    expect_match(block,"current; 1 rows): USUBJID, PARAMCD",fixed=TRUE)
    expect_match(block,"finished; 1 rows):",fixed=TRUE)
    expect_false(grepl("PARENT_CARD_SENTINEL|LABEL_PRIVATE|SUBJECT_PRIVATE|SOURCE_PRIVATE|42|99|88|123|777",block))
  }
  expect_match(records_block,"LINEAGE_ODD_A (finished; 1 rows): USUBJID, PARAMCD, AVAL",fixed=TRUE)
  expect_match(primary_block,"LINEAGE_ODD_A (finished; 1 rows): USUBJID, PARAMCD, AVAL",fixed=TRUE)
  expect_false(grepl("BVAL|PRIMARY_ONLY|SECRET_SOURCE",primary_block))
  expect_match(primary_block,"LINEAGE_SRC_VS (source; 1 rows): USUBJID, VSTESTCD, VSSTRESN",fixed=TRUE)
  expect_match(validation_block,"QZX_ODD_A_TEMP (finished; 1 rows): USUBJID, PARAMCD, AVAL, BVAL",fixed=TRUE)
  expect_match(validation_block,"src$VS (source; 1 rows): USUBJID, VSTESTCD, VSSTRESN, SECRET_SOURCE",fixed=TRUE)
  expect_match(primary_block,"LINEAGE_ODD_MIX",fixed=TRUE)
  expect_false(grepl("_TEMP|src\\$|\\bwork\\b|\\bupstream\\b",rec_primary,ignore.case=TRUE))
  expect_false(grepl("_TEMP|src\\$|\\bwork\\b|\\bupstream\\b",aval_primary,ignore.case=TRUE))
  expect_false(grepl("LINEAGE_",aval_validation,fixed=TRUE))
  expect_match(aval_validation,"QZX_ODD_MIX_TEMP",fixed=TRUE)
  expect_false(grepl("LINEAGE_",side_system_prompt("validation",fx$spec,"QZX",node=graph_node(fx$graph,"QZX.AVAL@ODD_MIX")),fixed=TRUE))
  expect_false(grepl("PRIMARY_ONLY",validation_block))
  expect_false(grepl("BVAL|PRIMARY_ONLY|SECRET_SOURCE",records_block))
  expect_match(aval_primary,"PARENT_CARD_SENTINEL",fixed=TRUE)

  rec_sys <- side_system_prompt("primary",fx$spec,"QZX",node=graph_node(fx$graph,"QZX.RECORDS@ODD_MIX"),project=fx$p)
  expect_match(rec_sys,"with the key columns from whichever AVAILABLE_DATASETS fit",fixed=TRUE)
  expect_false(grepl("does not inherit|from source domains",rec_sys))
  expect_match(rec_primary,"Create LINEAGE_ODD_MIX with the key columns from whichever available datasets fit",fixed=TRUE)
  expect_false(grepl("does not inherit|from the source domains",rec_primary))
  run <- run_side(fx$p,"QZX",graph_node(fx$graph,"QZX.RECORDS@ODD_MIX"),"primary",
    'result <- data.frame(USUBJID=LINEAGE_ODD_A$USUBJID, PARAMCD="ODD_MIX")',graph=fx$graph,spec=fx$spec)
  expect_true(run$ok,info=run$error %||% run$log)
  expect_equal(readRDS(run$out_path)$PARAMCD,"ODD_MIX")
  legacy <- run_side(fx$p,"QZX",graph_node(fx$graph,"QZX.RECORDS@ODD_MIX"),"primary",
    'QZX_ODD_MIX_TEMP <- data.frame(USUBJID=LINEAGE_ODD_A$USUBJID, PARAMCD="ODD_MIX")',graph=fx$graph,spec=fx$spec)
  expect_true(legacy$ok,info=legacy$error %||% legacy$log)
  expect_equal(readRDS(legacy$out_path)$PARAMCD,"ODD_MIX")

  wide_names <- sprintf("COL%04d",seq_len(700L))
  wide_src <- list(VS=as.data.frame(setNames(rep(list(1L),length(wide_names)),wide_names),check.names=FALSE))
  capped <- side_available_datasets_block("QZX",graph_node(fx$graph,"QZX.AVAL@ODD_MIX"),data.frame(),list(),wide_src,"primary")
  expect_match(capped,"source column names truncated at 4000 characters",fixed=TRUE)
})

test_that("a layered parameter without parents uses the same available-datasets block", {
  fx <- layered_test_project(c("P"))
  node <- graph_node(fx$graph,"X.RECORDS@P")
  prompt <- side_user_prompt("primary",fx$spec,fx$graph,node,character(),project=fx$p)
  system <- side_system_prompt("primary",fx$spec,"X",node=node,project=fx$p)
  expect_match(prompt,"AVAILABLE_DATASETS",fixed=TRUE)
  expect_match(prompt,"LINEAGE_P (current; 0 rows)",fixed=TRUE)
  expect_match(prompt,"Create LINEAGE_P with the key columns from whichever available datasets fit",fixed=TRUE)
  expect_false(grepl("UPSTREAM_LAYER_BINDINGS|does not inherit|from the source domains",prompt))
  expect_false(grepl("does not inherit|from source domains",system))
})

test_that("layered Keys prompts use key-column wording on both sides and chat paths", {
  fx <- layered_test_project(c("P"))
  node <- graph_node(fx$graph,"X.RECORDS@P")
  for (side in c("primary","validation")) {
    generation <- side_generation_system_prompt(side,fx$spec,fx$graph,node,fx$p)
    user <- side_user_prompt(side,fx$spec,fx$graph,node,character(),project=fx$p,
      self_review=list(code="old",issues=list("Review the key columns")))
    chat <- side_system_prompt(side,fx$spec,"X",node=node,project=fx$p,chat=TRUE)
    ask <- ask_user_prompt(side,fx$spec,fx$graph,node,"Why are these rows present?",project=fx$p)
    for (prompt in c(generation,user,chat,ask)) {
      expect_match(prompt,"VAR: KEYS",fixed=TRUE)
      expect_match(prompt,"KEYS: USUBJID,PARAMCD",fixed=TRUE)
      expect_false(grepl("(?:VAR: *RECORDS|\\$RECORDS|LINEAGE_[A-Z0-9_]+\\$RECORDS)",prompt,perl=TRUE))
    }
    expect_false(grepl("duplicate key rows|key-column check block|Keys check prints",chat,ignore.case=TRUE))
    expect_equal(length(gregexpr("VAR: KEYS",chat,fixed=TRUE)[[1]]),1L)
    expect_equal(length(gregexpr("KEYS: ",chat,fixed=TRUE)[[1]]),1L)
    expect_match(user,"KEYS_NODE:",fixed=TRUE)
    expect_match(generation,"duplicate key rows",fixed=TRUE)
    expect_match(generation,"distinct",fixed=TRUE)
    gen_lines <- strsplit(generation,"\n",fixed=TRUE)[[1]]
    lead <- grep("^For the Keys step, use this key-column check block",gen_lines)[[1]]
    expect_match(gen_lines[[lead+1L]], '^cat\\("rows:"',perl=TRUE)
    expect_equal(length(gregexpr("VAR: KEYS",generation,fixed=TRUE)[[1]]),1L)
    expect_equal(length(gregexpr("KEYS: ",generation,fixed=TRUE)[[1]]),1L)
  }
  layer_keys_write(fx$p,"X",list(P="BVAL"))
  overridden <- side_generation_system_prompt("primary",fx$spec,fx$graph,node,fx$p)
  expect_match(overridden,"KEYS: USUBJID,PARAMCD,BVAL",fixed=TRUE)
  expect_false(grepl('LINEAGE_P[["AVAL"]]',overridden,fixed=TRUE))
  expect_true(grepl('LINEAGE_P[["BVAL"]]',overridden,fixed=TRUE))
})

test_that("primary explicit source refs in the node spec override lineage", {
  fx <- layered_test_project(c("ODD_A", "ODD_B", "ODD_MIX"), dataset="QZX")
  records_id <- "QZX.RECORDS@ODD_A"
  layered_test_write_result(fx,records_id,"primary",data.frame(USUBJID="S1",PARAMCD="ODD_A"))
  put_status(fx$proj,"QZX",records_id,modifyList(blank_status(),list(level="L3")))
  source_frames <- list(VS=data.frame(USUBJID="S1",VSTESTCD="ODD_A",VS_ONLY_SOURCE=31,
    VS_ONLY_DERIV=32,VS_NOT_NAMED=33,VSSTRESN=7))
  old_load_src <- get("load_src",.GlobalEnv)
  assign("load_src",function(project)source_frames,.GlobalEnv)
  on.exit(assign("load_src",old_load_src,.GlobalEnv),add=TRUE)

  node <- graph_node(fx$graph,"QZX.AVAL@ODD_A")
  var_file <- file.path(fx$p,"spec","variable.csv")
  source_spec <- fx$spec
  source_spec$variables$Source[source_spec$variables$Dataset=="QZX" & source_spec$variables$Variable=="AVAL"] <-
    "for example VS.VS_ONLY_SOURCE"
  expect_true("VS.VS_ONLY_SOURCE" %in% side_explicit_spec_source_refs(source_spec,"QZX",node,source_frames))
  param_file <- file.path(fx$p,"spec","parameter.csv")
  vars <- read.csv(var_file,stringsAsFactors=FALSE,check.names=FALSE)
  vars$Source[vars$Dataset=="QZX" & vars$Variable=="AVAL"] <- ""
  vars$Derivation[vars$Dataset=="QZX" & vars$Variable=="AVAL"] <- ""
  write.csv(vars,var_file,row.names=FALSE,na="")
  fx$spec <- load_project_spec(fx$p)
  params <- read.csv(param_file,stringsAsFactors=FALSE,check.names=FALSE)
  put_ref <- function(field,value) {
    params$Source[params$Dataset=="QZX" & params$PARAMCD=="ODD_A"] <<- ""
    params$Derivation[params$Dataset=="QZX" & params$PARAMCD=="ODD_A"] <<- ""
    params[[field]][params$Dataset=="QZX" & params$PARAMCD=="ODD_A"] <<- value
    write.csv(params,param_file,row.names=FALSE,na="")
    fx$spec <<- load_project_spec(fx$p)
  }
  for (case in list(c("parameter","Source","VS.VS_ONLY_SOURCE"),
                    c("parameter","Derivation","VS.VS_ONLY_DERIV"),
                    c("variable","Source","VS.VS_ONLY_SOURCE"),
                    c("variable","Derivation","VS.VS_ONLY_DERIV"))) {
    params$Source[params$Dataset=="QZX" & params$PARAMCD=="ODD_A"] <- ""
    params$Derivation[params$Dataset=="QZX" & params$PARAMCD=="ODD_A"] <- ""
    write.csv(params,param_file,row.names=FALSE,na="")
    vars$Source[vars$Dataset=="QZX" & vars$Variable=="AVAL"] <- ""
    vars$Derivation[vars$Dataset=="QZX" & vars$Variable=="AVAL"] <- ""
    write.csv(vars,var_file,row.names=FALSE,na="")
    if (case[[1]]=="parameter") put_ref(case[[2]],case[[3]]) else {
      vars[[case[[2]]]][vars$Dataset=="QZX" & vars$Variable=="AVAL"] <- case[[3]]
      write.csv(vars,var_file,row.names=FALSE,na="")
      fx$spec <- load_project_spec(fx$p)
    }
    col <- if(case[[3]]=="VS.VS_ONLY_SOURCE")"VS_ONLY_SOURCE"else"VS_ONLY_DERIV"
    prompt <- side_user_prompt("primary",fx$spec,fx$graph,node,character(),project=fx$p)
    expect_match(prompt,paste0("LINEAGE_SRC_VS (source; 1 rows): USUBJID, VSTESTCD, ",col),fixed=TRUE)
    run <- run_side(fx$p,"QZX",node,"primary",
      paste0("result <- LINEAGE_ODD_A; result$AVAL <- LINEAGE_SRC_VS$",col),graph=fx$graph,spec=fx$spec)
    expect_true(run$ok,info=run$error %||% run$log)
    expect_equal(readRDS(run$out_path)$AVAL,if(col=="VS_ONLY_SOURCE")31 else 32)
  }
  params$Source[params$Dataset=="QZX" & params$PARAMCD=="ODD_A"] <- ""
  params$Derivation[params$Dataset=="QZX" & params$PARAMCD=="ODD_A"] <- ""
  vars$Source[vars$Dataset=="QZX" & vars$Variable=="AVAL"] <- ""
  vars$Derivation[vars$Dataset=="QZX" & vars$Variable=="AVAL"] <- ""
  write.csv(params,param_file,row.names=FALSE,na="");write.csv(vars,var_file,row.names=FALSE,na="")
  fx$spec <- load_project_spec(fx$p)
  blocked <- run_side(fx$p,"QZX",node,"primary",
    "result <- dplyr::select(LINEAGE_SRC_VS, VS_NOT_NAMED)",graph=fx$graph,spec=fx$spec)
  expect_false(blocked$ok)
  expect_false(grepl("DUALDAG_MISSING_COLUMN|DUALDAG_MISSING_DEPENDENCY",blocked$error %||% blocked$log,perl=TRUE))
  params$Source[params$Dataset=="QZX" & params$PARAMCD=="ODD_A"] <- "VS.VS_ABSENT"
  write.csv(params,param_file,row.names=FALSE,na="")
  fx$spec <- load_project_spec(fx$p)
  missing_prompt <- side_user_prompt("primary",fx$spec,fx$graph,node,character(),project=fx$p)
  missing_lines <- strsplit(missing_prompt,"\\n")[[1]]
  missing_start <- match("AVAILABLE_DATASETS (already loaded in this program; choose what is useful):",missing_lines)
  missing_end <- missing_start
  while(missing_end < length(missing_lines) && startsWith(missing_lines[[missing_end+1L]],"- ")) missing_end <- missing_end+1L
  expect_false(grepl("VS_ABSENT",paste(missing_lines[missing_start:missing_end],collapse="\\n"),fixed=TRUE))
  missing <- run_side(fx$p,"QZX",node,"primary",
    "result <- dplyr::select(LINEAGE_SRC_VS, VS_ABSENT)",graph=fx$graph,spec=fx$spec)
  expect_false(missing$ok)
  expect_false(grepl("DUALDAG_MISSING_COLUMN|DUALDAG_MISSING_DEPENDENCY",missing$error %||% missing$log,perl=TRUE))
})

test_that("a layer variable after Records sees record rows and its same-layer columns", {
  fx<-layered_test_project(c("P","Q","BMI"))
  records_id<-"X.RECORDS@BMI"
  records<-data.frame(USUBJID=c("A","B"),PARAMCD="BMI")
  layered_test_write_result(fx,records_id,"primary",records)
  put_status(fx$proj,"X",records_id,modifyList(blank_status(),list(level="L3")))
  bval_id<-"X.BVAL@BMI"
  layered_test_write_result(fx,bval_id,"primary",data.frame(USUBJID=c("A","B"),PARAMCD="BMI",BVAL=c(10,20)))
  put_status(fx$proj,"X",bval_id,modifyList(blank_status(),list(level="L3")))
  prompt<-side_user_prompt("primary",fx$spec,fx$graph,graph_node(fx$graph,"X.AVAL@BMI"),character(),project=fx$p)
  expect_match(prompt,"LINEAGE_ROWS: 2",fixed=TRUE)
  expect_match(prompt,"LINEAGE_FRAME_ROWS: 2",fixed=TRUE)
  expect_match(prompt,"LINEAGE_FRAME_COLUMNS: USUBJID,PARAMCD,BVAL",fixed=TRUE)
  expect_false(grepl("10|20",prompt))
})

test_that("model extraction assigns different layer keys and missing layers use the Key column", {
  fx<-layered_test_project(c("P","Q","BMI"),dataset_level=TRUE)
  file<-file.path(fx$p,"spec","variable.csv")
  writeLines(c(readLines(file),"X,ATPTN,Time point,Num,8,,VS.ATPTN,Time point number."),file)
  sp<-read_project_spec(fx$p)
  old<-get("llm_chat",envir=.GlobalEnv);on.exit(assign("llm_chat",old,.GlobalEnv),add=TRUE)
  assign("llm_chat",function(cfg,system,user,...){
    if(grepl("TASK: extract_classification",system,fixed=TRUE)) {
      line<-grep("^PARAMCD_LIST: ",strsplit(user,"\n",fixed=TRUE)[[1]],value=TRUE)
      pcs<-if(length(line))strsplit(sub("^PARAMCD_LIST: ","",line[[1]]),", ",fixed=TRUE)[[1]] else character()
      return(list(text=json_text(list(parameters=lapply(pcs,function(pc)list(paramcd=pc,classification="derived",reason="Scripted description review.")))),model="fake"))
    }
    list(text=if(grepl("TASK: extract_plan",system,fixed=TRUE))json_text(list(batches=list(list(kind="simple",rows=as.list(regmatches(user,gregexpr("[VP][0-9]+(?= \\|)",user,perl=TRUE))[[1]]))),
    layer_keys=list(P=c("USUBJID","PARAMCD","BVAL"),BMI=c("USUBJID","PARAMCD","ATPTN"))))
    else if(grepl("TASK: extract_check",system,fixed=TRUE))'{"cut":[]}'
    else json_text(setNames(lapply(regmatches(user,gregexpr("(?<=ROW )[VP][0-9]+",user,perl=TRUE))[[1]],function(x)list()),regmatches(user,gregexpr("(?<=ROW )[VP][0-9]+",user,perl=TRUE))[[1]])),model="fake")},.GlobalEnv)
  g<-extract_dependencies(list(),sp,"X",project=fx$p)
  expect_setequal(g$layer_keys$P,c("USUBJID","PARAMCD","BVAL"))
  expect_setequal(g$layer_keys$BMI,c("USUBJID","PARAMCD","ATPTN"))
  expect_setequal(g$layer_keys$Q,c("USUBJID","PARAMCD"))
  ids<-vapply(g$nodes,`[[`,"","id")
  expect_false("X.BVAL@P"%in%ids);expect_true("X.BVAL@Q"%in%ids)
  expect_false("X.ATPTN@BMI"%in%ids);expect_true("X.ATPTN@P"%in%ids)
  expect_true(grepl("a derived parameter may not have a time point",extract_plan_prompt("X"),fixed=TRUE))
})

test_that("layer key edits dry run, reset the layer and descendants, and reject invalid keys", {
  fx<-layered_test_project(c("P","BMI"),edges=list(list(from="P",to="BMI",uses=list(list(from_var="AVAL",to_var="AVAL")))));fx$graph$extraction<-list(edges=list(),external=list(),unresolved=list(),categories=list());save_graph(fx$proj,fx$graph)
  p_id<-"X.AVAL@P";bmi_id<-"X.AVAL@BMI"
  for(id in c(p_id,bmi_id)){st<-get_status(fx$proj,"X",id);st$level<-"L3";put_status(fx$proj,"X",id,st)}
  code_path<-file.path(node_run_dir(fx$proj,"X",p_id),"primary","code.R");dir.create(dirname(code_path),recursive=TRUE);writeLines("result <- 1",code_path)
  desired<-c("USUBJID","PARAMCD","BVAL")
  dry<-layer_keys_set(fx$proj,"X","P",desired,TRUE)
  expect_true(dry$reset_count>0L);expect_length(layer_keys_read(fx$proj,"X"),0L)
  changed<-layer_keys_set(fx$proj,"X","P",desired,FALSE)
  expect_true(file.exists(layer_keys_path(fx$proj,"X")))
  expect_false("X.BVAL@P"%in%vapply(load_graph(fx$proj,"X")$nodes,`[[`,"","id"))
  expect_identical(get_status(fx$proj,"X",p_id)$level,"none")
  expect_identical(get_status(fx$proj,"X",bmi_id)$level,"none")
  expect_false(file.exists(code_path))
  expect_error(layer_keys_set(fx$proj,"X","P",c("PARAMCD","BVAL"),TRUE),"cannot be removed")
  fx_ds<-layered_test_project(c("P"),dataset_level=TRUE)
  expect_error(layer_keys_set(fx_ds$proj,"X","P",c("USUBJID","PARAMCD","STUDYID"),TRUE),"Dataset-level")
  model_graph<-graph_skeleton(fx$spec,"X",list(P=c("USUBJID","PARAMCD","AVAL"),BMI=c("USUBJID","PARAMCD")))
  model_graph$extraction<-list(edges=list(),external=list(),unresolved=list(),categories=list())
  layer_keys_write(fx$proj,"X",list(P="BVAL",BMI="AVISITN"))
  extract_reply_write(fx$proj,"X",list(edges=list(),external=list(),unresolved=list()))
  store_extracted(list(project=fx$proj),"X",model_graph)
  expect_setequal(layer_keys_read(fx$proj,"X")$P,"BVAL")
  expect_false(is.null(extract_reply_read(fx$proj,"X")))
  expect_setequal(load_graph(fx$proj,"X")$layer_keys$P,c("USUBJID","PARAMCD","AVAL"))
})

test_that("changing the carry group key set resets descendants through member dependencies", {
  fx <- layered_test_project(c("CO1","CO2","DER"), edges=list(list(from="CO1",to="DER",uses=list(list(from_var="AVAL",to_var="AVAL")))))
  graph <- graph_skeleton(fx$spec,"X",carry_members=c("CO1","CO2"))
  graph$extraction <- list(parameter_classification=list(parameters=list(list(paramcd="CO1",classification="carry_over"),list(paramcd="CO2",classification="carry_over"),list(paramcd="DER",classification="derived"))),edges=list(),external=list(),unresolved=list())
  save_graph(fx$proj,graph)
  param_graph_write(fx$proj,list(dataset="X",carry_members=list("CO1","CO2"),edges=list(list(from="CO1",to="DER",uses=list()))))
  dry <- layer_keys_set(fx$proj,"X","CARRY_OVER",c("USUBJID","PARAMCD","AVAL"),TRUE)
  expect_true(dry$reset_count>0L)
  expect_true("DER" %in% dry$affected_layers)
  changed <- layer_keys_set(fx$proj,"X","CARRY_OVER",c("USUBJID","PARAMCD","AVAL"),FALSE)
  expect_true("DER" %in% changed$affected_layers)
})

test_that("explicit carry group keys replace member keys and persist through API reload and removal", {
  fx <- layered_test_project(c("CO1", "CO2", "DER"), edges=list(list(from="CO1",to="DER",uses=list())))
  variable_path <- file.path(fx$p,"spec","variable.csv")
  writeLines(c(readLines(variable_path,warn=FALSE),"X,ADT,Date,Num,8,,VS.VSDTC,Date"),variable_path)
  fx$spec <- read_project_spec(fx$p)
  graph <- graph_skeleton(fx$spec,"X",list(CO1="ADT",CO2="ADT"),c("CO1","CO2"))
  graph$extraction <- list(parameter_classification=list(parameters=list(
    list(paramcd="CO1",classification="carry_over"),list(paramcd="CO2",classification="carry_over"),
    list(paramcd="DER",classification="derived"))),edges=list(),external=list(),unresolved=list())
  save_graph(fx$proj,graph)
  app <- create_app(layered_test_root, fx$p, 7996L, "carry-key-token", load_config())
  on.exit(shutdown_app(app), add=TRUE)
  call <- function(body) {
    raw <- charToRaw(as.character(to_json(body)))
    req <- list(PATH_INFO="/api/layer-keys",REQUEST_METHOD="POST",QUERY_STRING="",
      HTTP_HOST="127.0.0.1:7996",HTTP_X_DUALDAG_TOKEN=app$token,CONTENT_TYPE="application/json",
      rook.input=list(read=function()raw))
    make_handlers(app)$call(req)
  }
  keys <- c("USUBJID","PARAMCD")
  layer_keys_write(fx$p,"X",list(CO1="ADT",CO2="ADT"))
  save_graph(fx$proj,graph)
  first_response <- call(list(dataset="X",paramcd="CARRY_OVER",keys=keys,dry_run=FALSE))
  expect_equal(first_response$status,200L,info=as.character(first_response$body))
  first <- from_json(first_response$body)
  expect_false(first$dry_run)
  saved <- layer_keys_read(fx$p,"X")
  expect_true("CARRY_OVER" %in% names(saved))
  expect_setequal(saved$CARRY_OVER,character())
  reloaded <- graph_skeleton(fx$spec,"X",saved,c("CO1","CO2"))
  expect_setequal(reloaded$layer_keys$CARRY_OVER,keys)
  expect_false("AVAL" %in% reloaded$layer_keys$CARRY_OVER)
  add <- from_json(call(list(dataset="X",paramcd="CARRY_OVER",keys=c("USUBJID","PARAMCD","ADT"),dry_run=FALSE))$body)
  expect_false(add$dry_run)
  expect_true("ADT" %in% graph_skeleton(fx$spec,"X",layer_keys_read(fx$p,"X"),c("CO1","CO2"))$layer_keys$CARRY_OVER)
  removed <- from_json(call(list(dataset="X",paramcd="CARRY_OVER",keys=c("USUBJID","PARAMCD"),dry_run=FALSE))$body)
  expect_false(removed$dry_run)
  after_remove <- graph_skeleton(fx$spec,"X",layer_keys_read(fx$p,"X"),c("CO1","CO2"))
  expect_false("ADT" %in% after_remove$layer_keys$CARRY_OVER)
  expect_false("AVAL" %in% after_remove$layer_keys$CARRY_OVER)
})

test_that("single layer extraction merges only its parameter and blocks overlapping extraction", {
  fx <- layered_test_project(c("P", "Q", "C"), edges=list(
    list(from="P",to="C",uses=list(list(from_var="AVAL",to_var="AVAL"))),
    list(from="Q",to="C",uses=list(list(from_var="AVAL",to_var="AVAL")))))
  app <- create_app(layered_test_root, fx$p, 7992L, "layer-merge-token", load_config())
  first_result <- file.path(node_run_dir(app$project,"X","X.AVAL@C"),"primary","out.rds")
  dir.create(dirname(first_result),recursive=TRUE,showWarnings=FALSE); saveRDS(data.frame(AVAL=1),first_result)
  original_launch <- get("launch", envir=.GlobalEnv)
  assign("launch", function(A, fn_name, args, timeout) {
    pc <- args$paramcd
    reply <- graph_skeleton(read_project_spec(args$project_dir), args$dataset)
    reply$edges <- c(reply$edges, list(list(from=paste0("X.BVAL@",pc),to=paste0("X.AVAL@",pc),evidence=paste(pc,"edge"))))
    reply$extraction <- list(marker=pc,edges=list(),external=list(),unresolved=list(),categories=list())
    promises::promise(function(resolve, reject) resolve(reply))
  }, envir=.GlobalEnv)
  on.exit(assign("launch", original_launch, envir=.GlobalEnv), add=TRUE)
  wait <- function(pr) {
    done <- FALSE; value <- NULL
    promises::then(pr, onFulfilled=function(x) { value <<- x; done <<- TRUE }, onRejected=function(e) { value <<- e; done <<- TRUE })
    until <- Sys.time()+5
    while(!done && Sys.time()<until) later::run_now(.05)
    if(!done) stop("Timed out awaiting layer extraction")
    value
  }
  snapshots <- list()
  for (pc in c("C", "P", "Q")) {
    res <- wait(start_layer_extract(app, "X", pc, wait=TRUE))
    expect_equal(res$status, 200L)
    graph <- load_graph(fx$proj, "X")
    pairs <- vapply(graph$edges, function(e) paste(e$from,e$to,sep="->"), "")
    expect_length(pairs, length(unique(pairs)))
    expect_true(pc %in% toupper(flat_chr(graph$generated_parameters)))
    if (identical(pc,"C")) expect_true(file.exists(first_result))
    for (done_pc in names(snapshots)) expect_identical(
      Filter(function(e) grepl(paste0("@",done_pc,"$"),e$to),graph$edges), snapshots[[done_pc]])
    snapshots[[pc]] <- Filter(function(e) grepl(paste0("@",pc,"$"),e$to),graph$edges)
  }
  app$jobs[["already-running"]] <- list(kind="extract",dataset="X",node="OTHER")
  expect_equal(start_layer_extract(app,"X","P")$status,409L)
  app$jobs[["already-running"]] <- NULL
  before <- load_graph(fx$proj,"X")
  child_id <- "X.AVAL@C"
  child_status <- modifyList(blank_status(),list(level="L3")); put_status(app$project,"X",child_id,child_status)
  regen <- wait(start_layer_extract(app,"X","P",wait=TRUE))
  expect_equal(regen$status,200L)
  after <- load_graph(fx$proj,"X")
  expect_identical(Filter(function(e) grepl("@C$",e$to),after$edges),snapshots$C)
  expect_identical(get_status(app$project,"X",child_id)$level,"none")
  expect_true("C" %in% toupper(flat_chr(after$generated_parameters)))
  # Each generated layer updates the stored reply, so later key changes keep every layer.
  expect_identical(extract_reply_read(fx$proj,"X")$marker,"P")
})

test_that("parameter layer blockers run only for BDS formats", {
  fx <- layered_test_project(c("P", "Q"),edges=list(list(from="P",to="Q")))
  write_json(list(X="OCCDS"),dataset_type_path(fx$proj))
  expect_null(param_layer_block_message(fx$proj,"X","primary","Q",fx$spec,fx$param))
  write_json(list(X="BDS"),dataset_type_path(fx$proj))
  expect_match(param_layer_block_message(fx$proj,"X","primary","Q",fx$spec,fx$param),"blocked by unfinished layer")
})

test_that("DAG agent applies scoped layer key changes immediately", {
  fx<-layered_test_project(c("P","Q"),dataset_level=TRUE);fx$graph$extraction<-list(edges=list(),external=list(),unresolved=list(),categories=list());save_graph(fx$proj,fx$graph)
  old<-get("llm_chat",envir=.GlobalEnv);on.exit(assign("llm_chat",old,.GlobalEnv),add=TRUE)
  assign("llm_chat",function(cfg,system,user,...)list(text=fake_dag_reply(system,user),model="fake"),.GlobalEnv)
  res<-dag_ask(list(),fx$p,"X","these are keys",paramcd="P",selected="X.BVAL@P")
  expect_identical(res$changes[[1]]$op,"set_layer_keys")
  expect_identical(res$changes[[1]]$paramcd,"P")
  expect_true("BVAL"%in%unlist(res$changes[[1]]$added))
  expect_true("BVAL"%in%layer_key_names(fx$spec,"X","P",load_graph(fx$proj,"X"),fx$p))
  expect_false("BVAL"%in%layer_key_names(fx$spec,"X","Q",load_graph(fx$proj,"X"),fx$p))
  expect_true(any(grepl("Parameter layer keys updated",vapply(res$card,function(x)x$text,""),fixed=TRUE)))
})

test_that("POST layer-keys supports dry run and apply", {
  fx<-layered_test_project(c("P"));fx$graph$extraction<-list(edges=list(),external=list(),unresolved=list(),categories=list());save_graph(fx$proj,fx$graph)
  app<-create_app(layered_test_root,fx$p,7995L,"layer-key-token",load_config())
  on.exit(shutdown_app(app),add=TRUE)
  call<-function(body){raw<-charToRaw(as.character(to_json(body)));req<-list(PATH_INFO="/api/layer-keys",REQUEST_METHOD="POST",QUERY_STRING="",HTTP_HOST="127.0.0.1:7995",HTTP_X_DUALDAG_TOKEN=app$token,CONTENT_TYPE="application/json",rook.input=list(read=function()raw));make_handlers(app)$call(req)}
  dry<-from_json(call(list(dataset="X",paramcd="P",keys=c("USUBJID","PARAMCD","BVAL"),dry_run=TRUE))$body)
  expect_true(dry$reset_count>0L);expect_length(layer_keys_read(fx$p,"X"),0L)
  applied<-from_json(call(list(dataset="X",paramcd="P",keys=c("USUBJID","PARAMCD","BVAL"),dry_run=FALSE))$body)
  expect_false(applied$dry_run);expect_true("BVAL"%in%layer_key_names(fx$spec,"X","P",load_graph(fx$p,"X"),fx$p))
})

test_that("fake layered generation builds collected layers and a BMI-like layer through full run", {
  old_fake <- Sys.getenv("DUALDAG_FAKE_LLM", unset=NA_character_)
  Sys.setenv(DUALDAG_FAKE_LLM="1")
  on.exit(if(is.na(old_fake))Sys.unsetenv("DUALDAG_FAKE_LLM")else Sys.setenv(DUALDAG_FAKE_LLM=old_fake),add=TRUE)
  fx<-layered_test_project(c("WEIGHT","HEIGHT","BMI"))
  dir.create(file.path(fx$p,"data"),recursive=TRUE,showWarnings=FALSE)
  vs<-data.frame(USUBJID=c("A","B","A","B"),VSTESTCD=c("WEIGHT","WEIGHT","HEIGHT","HEIGHT"),
    VSSTRESN=c(60,80,170,160))
  haven::write_xpt(vs,file.path(fx$p,"data","vs.xpt"))
  ids<-vapply(fx$graph$nodes,`[[`,"","id");order<-batch_order_for(fx$proj,"X",ids)
  for(id in order) {
    n<-graph_node(fx$graph,id)
    for(side in c("primary","validation")) {
      ran<-tryCatch(run_side_job(fx$p,"X",id,side,NULL,60),error=function(e)e)
      expect_false(inherits(ran,"error"),info=if(inherits(ran,"error"))conditionMessage(ran)else"")
      if(inherits(ran,"error"))stop(conditionMessage(ran))
      expect_true(ran$run$ok,info=ran$run$error %||% "")
      if(identical(id,"X.AVAL@WEIGHT") && identical(side,"primary")) {
        fake_code <- paste(readLines(file.path(side_run_dir(fx$p,"X",id,side),"code.R"),warn=FALSE),collapse="\n")
        check <- sub("(?s)^.*LINEAGE_WEIGHT <- result\\n","",fake_code,perl=TRUE)
        expect_match(check,"nrow(LINEAGE_WEIGHT)",fixed=TRUE)
        expect_match(check,"LINEAGE_WEIGHT$AVAL",fixed=TRUE)
        expect_false(grepl("nrow(result)",check,fixed=TRUE))
      }
    }
    cmp<-compare_node(fx$p,"X",n,dataset_keys(fx$spec,"X"))
    st<-get_status(fx$proj,"X",id);st$level<-if(cmp$match)"L3" else "L2";st$problem<-if(cmp$match)NULL else list(stage="compare",message=cmp$summary);put_status(fx$proj,"X",id,st)
  }
  for(side in c("primary","validation")) {
    full<-fullrun_worker(fx$p,"X",side,timeout=90)
    expect_true(full$ok,info=full$error %||% "")
    out<-readRDS(file.path(fullrun_dir(fx$p,"X",side),"result.rds"))
    expect_equal(nrow(out),6L)
    bmi<-out[out$PARAMCD=="BMI",]
    expect_equal(bmi$AVAL,c(60/(170/100)^2,80/(160/100)^2),tolerance=1e-8)
  }
})

test_that("layered primary accepts parameter-qualified lineage and warns on outside refs", {
  fx <- layered_test_project(c("P"))
  records <- "X.RECORDS@P"; ancestor <- "X.AVAL@P"; target <- "X.BVAL@P"
  fx$graph$edges <- c(fx$graph$edges,list(list(from=ancestor,to=target,evidence="layer lineage")))
  save_graph(fx$proj,fx$graph)
  for (id in c(records,ancestor)) {
    value <- if (identical(id,records)) data.frame(USUBJID=c("S1","S2"),PARAMCD="P") else
      data.frame(USUBJID=c("S1","S2"),PARAMCD="P",AVAL=c(1,2))
    for (side in c("primary","validation")) layered_test_write_result(fx,id,side,value)
    put_status(fx$proj,"X",id,modifyList(blank_status(),list(level="L3",sides=list(primary=list(ok=TRUE),validation=list(ok=TRUE)))))
  }
  node <- graph_node(fx$graph,target)
  allowed <- lineage_context(fx$spec,fx$graph,node,fx$p,"primary")
  expect_true("X.AVAL@P" %in% allowed$ids)
  primary_code <- "LINEAGE_P$BVAL <- LINEAGE_P$AVAL + 10"
  ran <- run_side(fx$p,"X",node,"primary",primary_code,graph=fx$graph,spec=fx$spec)
  expect_true(ran$ok,info=ran$error %||% ran$log)
  expect_equal(readRDS(ran$out_path)$BVAL,c(11,12))

  outside <- start_side_proc(fx$p,"X",node,"primary",
    "LINEAGE_P$BVAL <- LINEAGE_P$AVAL + LINEAGE_P$AVISIT",fx$graph,fx$spec)
  expect_null(outside$prep$error)
  expect_true(inherits(outside$proc,"process"))
  if (is.function(outside$proc$kill)) outside$proc$kill()

  validation <- run_side(fx$p,"X",node,"validation",
    "X_P_TEMP$BVAL <- X_P_TEMP$AVAL + 1",graph=fx$graph,spec=fx$spec)
  expect_true(validation$ok,info=validation$error %||% validation$log)
  expect_equal(readRDS(validation$out_path)$BVAL,c(2,3))

  valid_warning <- lineage_code_warnings("result <- upstream$AVAL","primary","X",target,allowed,list(),c("USUBJID","PARAMCD"))
  outside_warning <- lineage_code_warnings("result <- upstream$AVISIT","primary","X",target,allowed,list(),c("USUBJID","PARAMCD"))
  expect_length(valid_warning,0L)
  expect_identical(outside_warning,"Lineage warning: BVAL reads AVISIT, which is not in its DAG lineage.")
  expect_true(lineage_upstream_ref_allowed("X.AGE","X.C",c("X.AGE")))
})

test_that("real layer jobs fill one layer at a time and a key change keeps every layer", {
  fx<-layered_test_project(c("P","Q"))
  old_fake<-Sys.getenv("DUALDAG_FAKE_LLM");Sys.setenv(DUALDAG_FAKE_LLM="1");on.exit(Sys.setenv(DUALDAG_FAKE_LLM=old_fake),add=TRUE)
  app<-create_app(layered_test_root,fx$p,7993L,"layer-real-token",load_config())
  old_launch<-get("launch",envir=.GlobalEnv);on.exit(assign("launch",old_launch,envir=.GlobalEnv),add=TRUE)
  assign("launch",function(A,fn_name,args,timeout){
    reply<-do.call(fn_name,args)
    promises::promise(function(resolve,reject)resolve(reply))
  },envir=.GlobalEnv)
  wait<-function(pr){
    done<-FALSE;value<-NULL
    promises::then(pr,onFulfilled=function(x){value<<-x;done<<-TRUE},onRejected=function(e){value<<-e;done<<-TRUE})
    until<-Sys.time()+5
    while(!done&&Sys.time()<until)later::run_now(.05)
    if(!done)stop("Timed out awaiting layer extraction")
    value
  }
  expect_equal(wait(start_layer_extract(app,"X","P",wait=TRUE))$status,200L)
  first<-load_graph(fx$proj,"X")
  expect_true(any(vapply(first$external,function(x)"X.AVAL@P"%in%flat_chr(x$used_by),TRUE)))
  expect_false(any(vapply(first$external,function(x)"X.AVAL@Q"%in%flat_chr(x$used_by),TRUE)))
  expect_equal(wait(start_layer_extract(app,"X","Q",wait=TRUE))$status,200L)
  both<-load_graph(fx$proj,"X")
  expect_setequal(toupper(flat_chr(both$generated_parameters)),c("P","Q"))
  expect_true(any(vapply(both$external,function(x)"X.AVAL@Q"%in%flat_chr(x$used_by),TRUE)))
  reply<-extract_reply_read(fx$proj,"X")
  expect_setequal(unlist(lapply(reply$external,function(x)flat_chr(x$params))),c("P","Q"))
  expect_true(isTRUE(reply$no_source_spread))
  expect_true(file.exists(extract_plan_path(fx$proj,"X")))
  layer_keys_set(fx$proj,"X","P",c("USUBJID","PARAMCD","BVAL"))
  changed<-load_graph(fx$proj,"X")
  expect_setequal(toupper(flat_chr(changed$generated_parameters)),c("P","Q"))
  expect_true(any(vapply(changed$external,function(x)"X.AVAL@Q"%in%flat_chr(x$used_by),TRUE)))
  expect_true(any(vapply(changed$external,function(x)"X.AVAL@P"%in%flat_chr(x$used_by),TRUE)))
})

test_that("regenerating a parameter drops its old issues, cuts and warnings, and the state shows the cuts", {
  fx<-layered_test_project(c("P","Q"))
  app<-create_app(layered_test_root,fx$p,7994L,"layer-regen-token",load_config())
  replies<-list()
  make<-function(pc,tag){
    reply<-graph_skeleton(read_project_spec(fx$p),"X")
    reply$unresolved<-if(nzchar(tag))list(list(type="ambiguous",dataset="X",variables=list("AVAL"),message=paste(pc,tag),paramcd=pc))else list()
    reply$warnings<-if(nzchar(tag))list(paste(pc,tag))else list()
    reply$cycle_cuts<-if(nzchar(tag))list(list(from=paste0("X.BVAL@",pc),to=paste0("X.AVAL@",pc),reason=paste(pc,tag),round=1L,paramcd=pc))else list()
    reply$extraction<-list(edges=list(),external=list(),unresolved=reply$unresolved,categories=list(),no_source_spread=TRUE)
    reply
  }
  old_launch<-get("launch",envir=.GlobalEnv);on.exit(assign("launch",old_launch,envir=.GlobalEnv),add=TRUE)
  assign("launch",function(A,fn_name,args,timeout){reply<-replies[[args$paramcd]];promises::promise(function(resolve,reject)resolve(reply))},envir=.GlobalEnv)
  wait<-function(pr){
    done<-FALSE;value<-NULL
    promises::then(pr,onFulfilled=function(x){value<<-x;done<<-TRUE},onRejected=function(e){value<<-e;done<<-TRUE})
    until<-Sys.time()+5
    while(!done&&Sys.time()<until)later::run_now(.05)
    if(!done)stop("Timed out awaiting layer extraction")
    value
  }
  messages<-function(g)vapply(g$unresolved,function(x)x$message,"")
  replies$P<-make("P","first");replies$Q<-make("Q","first")
  expect_equal(wait(start_layer_extract(app,"X","P",wait=TRUE))$status,200L)
  expect_equal(wait(start_layer_extract(app,"X","Q",wait=TRUE))$status,200L)
  g<-load_graph(fx$proj,"X")
  expect_setequal(messages(g),c("P first","Q first"))
  expect_setequal(unlist(g$warnings),c("P first","Q first"))
  expect_setequal(vapply(g$cycle_cuts,function(x)x$reason,""),c("P first","Q first"))
  shown<-state_payload(list(project=fx$proj,jobs=list()))$datasets$X$graph$cycle_cuts
  expect_setequal(vapply(shown,function(x)x$reason,""),c("P first","Q first"))
  replies$P<-make("P","")
  expect_equal(wait(start_layer_extract(app,"X","P",wait=TRUE))$status,200L)
  g<-load_graph(fx$proj,"X")
  expect_identical(messages(g),"Q first")
  expect_identical(unlist(g$warnings),"Q first")
  expect_identical(vapply(g$cycle_cuts,function(x)x$reason,""),"Q first")
  expect_identical(vapply(extract_reply_read(fx$proj,"X")$unresolved,function(x)x$message,""),"Q first")
  # A new start: nothing generated, so the older stored reply doesn't return.
  sk<-graph_skeleton(read_project_spec(fx$p),"X");sk$generated_parameters<-list();save_graph(fx$proj,sk)
  replies$Q<-make("Q","second")
  expect_equal(wait(start_layer_extract(app,"X","Q",wait=TRUE))$status,200L)
  expect_identical(vapply(extract_reply_read(fx$proj,"X")$unresolved,function(x)x$message,""),"Q second")
  expect_identical(messages(load_graph(fx$proj,"X")),"Q second")
})

