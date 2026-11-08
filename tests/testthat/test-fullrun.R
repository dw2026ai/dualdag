fullrun_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
if (!exists("fullrun_worker", mode = "function", inherits = TRUE)) {
  files <- list.files(file.path(fullrun_root, "R"), pattern = "\\.R$", full.names = TRUE)
  utils <- files[grepl("/utils\\.R$", files)]
  for (f in c(utils, setdiff(files, utils))) sys.source(f, envir = globalenv())
}

fullrun_fixture <- function() {
  p <- tempfile("fullrun-")
  dir.create(file.path(p, "spec"), recursive = TRUE)
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
               "ADSL,USUBJID,Subject,Char,20,1,,",
               "ADSL,AGE,Age,Num,8,,,"), file.path(p, "spec", "variable.csv"))
  proj <- open_project(p)
  graph <- list(dataset = "ADSL", nodes = list(list(id = "ADSL.USUBJID"), list(id = "ADSL.AGE")),
                edges = list(list(from = "ADSL.USUBJID", to = "ADSL.AGE")))
  save_graph(proj, graph)
  list(p = p, proj = proj, graph = graph)
}

layered_fullrun_fixture <- function() {
  p <- tempfile("layered-fullrun-"); dir.create(file.path(p,"spec"),recursive=TRUE)
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "X,USUBJID,Subject,Char,20,1,,","X,PARAMCD,Parameter,Char,8,2,,","X,STUDYID,Study,Char,8,,,","X,AVAL,Value,Num,8,,,"),file.path(p,"spec","variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation","X,P,Primary,,","X,Q,Secondary,,","X,R,Derived,,"),file.path(p,"spec","parameter.csv"))
  proj<-open_project(p);spec<-load_project_spec(p);g<-graph_skeleton(spec,"X")
  g$edges<-list(list(from="X.RECORDS@P",to="X.AVAL@P"),list(from="X.RECORDS@Q",to="X.AVAL@Q"),list(from="X.RECORDS@R",to="X.AVAL@R"))
  save_graph(proj,g)
  pg<-list(dataset="X",nodes=param_defined_nodes(spec,"X"),edges=list(
    list(from="P",to="R",uses=list(list(from_var="AVAL",to_var="AVAL"))),
    list(from="Q",to="R",uses=list(list(from_var="AVAL",to_var="AVAL")))),error=NULL,warnings=list())
  param_graph_write(proj,pg)
  for(side in c("primary","validation")) {
    fullrun_write_code(p,"X.STUDYID",side,"result <- data.frame(USUBJID=c('A','B'), STUDYID=c('S1','S2'))")
    for(pc in c("P","Q")) {
      fullrun_write_code(p,paste0("X.RECORDS@",pc),side,paste0("result <- data.frame(USUBJID=c('A','B'), PARAMCD='",pc,"')"))
      val<-if(pc=="P")"c(1,2)" else "c(10,20)"
      fullrun_write_code(p,paste0("X.AVAL@",pc),side,paste0("result <- data.frame(work[, keys, drop=FALSE], AVAL=",val,")"))
    }
    fullrun_write_code(p,"X.RECORDS@R",side,"result <- data.frame(USUBJID=layers[['P']]$USUBJID, PARAMCD='R')")
    fullrun_write_code(p,"X.AVAL@R",side,"result <- data.frame(work[, keys, drop=FALSE], AVAL=layers[['P']]$AVAL + layers[['Q']]$AVAL)")
  }
  list(p=p,proj=proj,graph=g,spec=spec)
}

carry_fullrun_fixture <- function() {
  p <- tempfile("carry-fullrun-"); dir.create(file.path(p,"spec"),recursive=TRUE)
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADVS,USUBJID,Subject,Char,20,1,,","ADVS,PARAMCD,Parameter,Char,8,2,,",
    "ADVS,AVISITN,Visit,Num,8,3,,","ADVS,AVAL,Value,Num,8,,,"),file.path(p,"spec","variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation","ADVS,WEIGHT,Weight,,",
    "ADVS,HEIGHT,Height,,","ADVS,BMI,Body mass index,,","ADVS,OTHER,Other derived,,"),file.path(p,"spec","parameter.csv"))
  proj <- open_project(p); spec <- load_project_spec(p)
  g <- graph_skeleton(spec,"ADVS",carry_members=c("WEIGHT","HEIGHT"))
  g$edges <- list(
    list(from="ADVS.RECORDS@CARRY_OVER",to="ADVS.AVAL@CARRY_OVER"),
    list(from="ADVS.RECORDS@BMI",to="ADVS.AVAL@BMI"),
    list(from="ADVS.RECORDS@OTHER",to="ADVS.AVAL@OTHER"))
  save_graph(proj,g)
  pg <- list(dataset="ADVS",nodes=param_defined_nodes(spec,"ADVS"),carry_members=list("WEIGHT","HEIGHT"),
    edges=list(list(from="WEIGHT",to="BMI",uses=list(list(from_var="AVAL",to_var="AVAL"))),
      list(from="HEIGHT",to="BMI",uses=list(list(from_var="AVAL",to_var="AVAL")))),
    classification=list(parameters=list(list(paramcd="WEIGHT",classification="carry_over"),
      list(paramcd="HEIGHT",classification="carry_over"),list(paramcd="BMI",classification="derived"),
      list(paramcd="OTHER",classification="derived"))))
  param_graph_write(proj,pg)
  for(side in c("primary","validation")) {
    fullrun_write_code(p,"ADVS.RECORDS@CARRY_OVER",side,
      "result <- data.frame(USUBJID=c('S1','S1','S2','S2'), PARAMCD=c('WEIGHT','HEIGHT','WEIGHT','HEIGHT'), AVISITN=1)")
    fullrun_write_code(p,"ADVS.AVAL@CARRY_OVER",side,
      "result <- data.frame(ADVS_CARRY_OVER_TEMP[, keys, drop=FALSE], AVAL=c(70,170,80,180))")
    fullrun_write_code(p,"ADVS.RECORDS@BMI",side,
      "result <- data.frame(USUBJID=c('S1','S2'), PARAMCD='BMI', AVISITN=1)")
    fullrun_write_code(p,"ADVS.AVAL@BMI",side,
      "w <- layers[['CARRY_OVER']]; w <- w[w$PARAMCD == 'WEIGHT', ]; h <- layers[['CARRY_OVER']]; h <- h[h$PARAMCD == 'HEIGHT', ]; result <- data.frame(work[, keys, drop=FALSE], AVAL=w$AVAL / (h$AVAL / 100)^2)")
    fullrun_write_code(p,"ADVS.RECORDS@OTHER",side,
      "result <- data.frame(USUBJID=c('S1','S2'), PARAMCD='OTHER', AVISITN=1)")
    fullrun_write_code(p,"ADVS.AVAL@OTHER",side,
      "result <- data.frame(work[, keys, drop=FALSE], AVAL=c(1,2))")
  }
  list(p=p,proj=proj,graph=g,spec=spec)
}

fullrun_write_code <- function(p, id, side, code) {
  d <- side_run_dir(p, parse_node_id(id)$dataset, id, side)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  write_text(code, file.path(d, "code.R"))
}

fullrun_call <- function(A, path, method = "GET", body = NULL) {
  q <- ""
  if (grepl("?", path, fixed = TRUE)) { q <- sub("^[^?]*\\?", "", path); path <- sub("\\?.*$", "", path) }
  raw <- if (is.null(body)) raw() else charToRaw(as.character(to_json(body)))
  make_handlers(A)$call(list(PATH_INFO = path, REQUEST_METHOD = method, QUERY_STRING = q,
    HTTP_HOST = sprintf("127.0.0.1:%d", A$port), HTTP_X_DUALDAG_TOKEN = A$token,
    CONTENT_TYPE = if (identical(method, "POST")) "application/json" else "",
    rook.input = list(read = function() raw)))
}

fullrun_run_program_directly <- function(program) {
  output <- system2(file.path(R.home("bin"), "Rscript"), shQuote(program), stdout = TRUE, stderr = TRUE)
  status <- attr(output, "status") %||% 0L
  list(status = as.integer(status), output = output)
}

test_that("fullrun runs a dependency ordered standalone program on each side", {
  fx <- fullrun_fixture()
  for (side in c("primary", "validation")) {
    fullrun_write_code(fx$p, "ADSL.USUBJID", side, "result <- data.frame(USUBJID=c('A','B'), EXTRA='discard')")
    fullrun_write_code(fx$p, "ADSL.AGE", side, paste(c(
      "# A multi-line variable program",
      "",
      "cat('AGE check printed\\n')",
      "summary(c(1,2))",
      "result <- data.frame(USUBJID=work$USUBJID, AGE=c(20,30))"
    ), collapse = "\n"))
    result <- fullrun_worker(fx$p, "ADSL", side)
    expect_true(result$ok)
    expect_equal(unlist(result$variables_run), c("ADSL.USUBJID", "ADSL.AGE"))
    expect_equal(result$rows, 2L)
    expect_equal(result$columns, 2L)
    expect_named(result$hashes, c("ADSL.USUBJID", "ADSL.AGE"))
    expect_equal(readRDS(file.path(fullrun_dir(fx$p, "ADSL", side), "result.rds"))$AGE, c(20, 30))
    expect_named(readRDS(file.path(fullrun_dir(fx$p, "ADSL", side), "result.rds")), c("USUBJID", "AGE"))
    expect_match(read_text(file.path(fullrun_dir(fx$p, "ADSL", side), "log.txt")), "AGE check printed")
    expect_match(read_text(file.path(fullrun_dir(fx$p, "ADSL", side), "log.txt")), "Min.")
    log <- read_text(file.path(fullrun_dir(fx$p, "ADSL", side), "log.txt"))
    expect_match(log, "# ==== ADSL.USUBJID ====")
    expect_match(log, "# ==== ADSL.AGE ====")
    program <- file.path(fullrun_dir(fx$p, "ADSL", side), "program.R")
    expect_match(read_text(program), "# ==== ADSL.AGE ====")
    direct <- fullrun_run_program_directly(program)
    expect_equal(direct$status, 0L, info = paste(direct$output, collapse = "\n"))
    expect_match(read_text(file.path(fullrun_dir(fx$p, "ADSL", side), "log.txt")), "Min.")
    expect_false(fullrun_status(fx$p, "ADSL", side)$stale)
    fullrun_write_code(fx$p, "ADSL.AGE", side, "result <- data.frame(USUBJID=work$USUBJID, AGE=c(21,31))")
    expect_true(fullrun_status(fx$p, "ADSL", side)$stale)
  }
})

test_that("primary fullrun does not reject or drop an outside-lineage source column", {
  fx <- fullrun_fixture()
  dir.create(file.path(fx$p, "data"), recursive=TRUE, showWarnings=FALSE)
  haven::write_xpt(data.frame(USUBJID=c("A","B"), OUTSIDE=c(7,9)),
                   file.path(fx$p,"data","VS.xpt"))
  fullrun_write_code(fx$p,"ADSL.USUBJID","primary","result <- data.frame(USUBJID=c('A','B'))")
  fullrun_write_code(fx$p,"ADSL.AGE","primary",
    "stopifnot('OUTSIDE' %in% names(LINEAGE_SRC_VS)); result <- data.frame(USUBJID=work$USUBJID, AGE=LINEAGE_SRC_VS$OUTSIDE)")
  ran <- fullrun_worker(fx$p,"ADSL","primary",timeout=60)
  expect_true(ran$ok,info=ran$error %||% read_text(file.path(fullrun_dir(fx$p,"ADSL","primary"),"log.txt")))
  expect_equal(readRDS(file.path(fullrun_dir(fx$p,"ADSL","primary"),"result.rds"))$AGE,c(7,9))
})

test_that("layered fullrun stacks records and variables and exposes upstream layers", {
  fx<-layered_fullrun_fixture()
  for(side in c("primary","validation")) {
    if (side == "primary") {
      fullrun_write_code(fx$p,"X.RECORDS@P",side,
        "LINEAGE_P <- data.frame(USUBJID=c('A','B'), PARAMCD='P')")
      fullrun_write_code(fx$p,"X.AVAL@P",side,
        "LINEAGE_P <- data.frame(LINEAGE_P, AVAL=c(101,202))")
    }
    expect_false("X.USUBJID"%in%fullrun_plan(fx$p,"X",side)$ids)
    expect_match(fullrun_code(fx$p,"X","X.RECORDS@P",side),"PARAMCD")
    pl<-fullrun_plan(fx$p,"X",side); pt<-fullrun_program_text(fx$p,"X",side,pl,character())
    expect_match(pt$text,"USUBJID=c('A','B')",fixed=TRUE)
    ran<-fullrun_worker(fx$p,"X",side,timeout=60)
    expect_true(ran$ok,info=paste(ran$error %||% "",paste(readLines(file.path(fullrun_dir(fx$p,"X",side),"log.txt"),warn=FALSE),collapse="\n")))
    out<-readRDS(file.path(fullrun_dir(fx$p,"X",side),"result.rds"))
    expect_equal(nrow(out),6L)
    expect_equal(out$AVAL[out$PARAMCD=="R"],if(side=="primary")c(111,222)else c(11,22))
    if (side == "primary") expect_equal(out$AVAL[out$PARAMCD=="P"],c(101,202))
    expect_identical(names(out),c("USUBJID","PARAMCD","STUDYID","AVAL"))
    expect_equal(out$STUDYID,ifelse(out$USUBJID=="A","S1","S2"))
    expect_false(any(grepl("[.]x$|[.]y$",names(out))))
  }
})

test_that("carry-over fullrun executes one shared group per side and exposes the whole group to derived layers", {
  fx <- carry_fullrun_fixture()
  blocked <- param_layer_block_message(fx$proj,"ADVS","primary","BMI",fx$spec,param_graph_read(fx$proj,"ADVS"))
  expect_match(blocked,"blocked by unfinished layer CARRY_OVER",fixed=TRUE)
  carry_ids <- vapply(Filter(function(n) identical(n$paramcd,"CARRY_OVER"),fx$graph$nodes),`[[`,"","id")
  for (id in carry_ids) put_status(fx$proj,"ADVS",id,modifyList(blank_status(),list(level="L3")))
  expect_null(param_layer_block_message(fx$proj,"ADVS","primary","BMI",fx$spec,param_graph_read(fx$proj,"ADVS")))
  expect_identical(fullrun_plan(fx$p,"ADVS","primary")$layers$CARRY_OVER$members,"ADVS.AVAL@CARRY_OVER")
  for (side in c("primary","validation")) {
    plan <- fullrun_plan(fx$p,"ADVS",side)
    program <- fullrun_program_text(fx$p,"ADVS",side,plan,character())
    expect_match(program$text,"ADVS_CARRY_OVER_TEMP",fixed=TRUE)
    expect_match(program$codes[["ADVS.RECORDS@CARRY_OVER"]],"PARAMCD=c('WEIGHT','HEIGHT'",fixed=TRUE)
    ran <- fullrun_worker(fx$p,"ADVS",side,timeout=60)
    expect_true(ran$ok,info=ran$error %||% read_text(file.path(fullrun_dir(fx$p,"ADVS",side),"log.txt")))
    out <- readRDS(file.path(fullrun_dir(fx$p,"ADVS",side),"result.rds"))
    expect_setequal(out$PARAMCD,c("WEIGHT","HEIGHT","BMI","OTHER"))
    bmi <- out[out$PARAMCD=="BMI",]
    expect_equal(bmi$AVAL[match(c("S1","S2"),bmi$USUBJID)],c(70/(1.7^2),80/(1.8^2)),tolerance=1e-10)
    expect_equal(nrow(out),8L)
  }
})

test_that("carry-over run_sides and stored runs accept all group member PARAMCDs", {
  fx <- carry_fullrun_fixture()
  node <- graph_node(fx$graph,"ADVS.RECORDS@CARRY_OVER")
  code <- "result <- data.frame(USUBJID=c('S1','S1','S2','S2'), PARAMCD=c('WEIGHT','HEIGHT','WEIGHT','HEIGHT'), AVISITN=1)"
  both <- run_sides(fx$p,"ADVS",node,list(primary=code,validation=code),timeout=30,graph=fx$graph,spec=fx$spec)
  expect_true(both$primary$ok,info=both$primary$error %||% "")
  expect_true(both$validation$ok,info=both$validation$error %||% "")
  for (side in c("primary","validation")) {
    stored <- run_stored_sides(fx$p,"ADVS",node,side=side,timeout=30,graph=fx$graph,spec=fx$spec)
    expect_true(stored$ok,info=stored$error %||% "")
    expect_true(stored$results[[side]]$ok,info=stored$results[[side]]$error %||% "")
  }
})

test_that("carry-over stored-code API reruns validate group membership, keys, and duplicates", {
  cases <- list(
    list(code="result <- data.frame(USUBJID='S1', PARAMCD='OUTSIDE', AVISITN=1)", error="PARAMCD must be one of WEIGHT, HEIGHT"),
    list(code="result <- data.frame(USUBJID='S1', PARAMCD='WEIGHT')", error="key columns: USUBJID, PARAMCD, AVISITN"),
    list(code="result <- data.frame(USUBJID=c('S1','S1'), PARAMCD='WEIGHT', AVISITN=1)", error="duplicate keys"),
    list(code="result <- data.frame(USUBJID=character(), PARAMCD=character(), AVISITN=numeric())", valid=TRUE),
    list(code="result <- data.frame(USUBJID='S1', PARAMCD='WEIGHT', AVISITN=1)", valid=TRUE)
  )
  for (i in seq_along(cases)) {
    fx <- carry_fullrun_fixture()
    fx$graph$generated_parameters <- "CARRY_OVER"
    save_graph(fx$proj, fx$graph)
    node_id <- "ADVS.RECORDS@CARRY_OVER"
    for (side in c("primary","validation")) fullrun_write_code(fx$p,node_id,side,cases[[i]]$code)
    app <- create_app(fullrun_root,fx$p,19962L,"carry-api-token",load_config())
    response <- fullrun_call(app,"/api/node/run","POST",list(dataset="ADVS",node=node_id,side="both"))
    expect_equal(response$status,200L)
    body <- jsonlite::fromJSON(response$body,simplifyVector=FALSE)
    if (isTRUE(cases[[i]]$valid)) {
      expect_true(isTRUE(body$sides$primary$ok),info=cases[[i]]$code)
      expect_true(isTRUE(body$sides$validation$ok),info=cases[[i]]$code)
    } else {
      expect_false(isTRUE(body$sides$primary$ok),info=cases[[i]]$code)
      expect_match(body$sides$primary$error,cases[[i]]$error,fixed=TRUE)
      expect_false(isTRUE(body$sides$validation$ok),info=cases[[i]]$code)
      expect_match(body$sides$validation$error,cases[[i]]$error,fixed=TRUE)
    }
  }
})

test_that("carry-over blocker checks only the variables an ancestor edge uses", {
  fx <- carry_fullrun_fixture()
  for (id in vapply(Filter(function(n) identical(n$paramcd,"CARRY_OVER") && identical(n$var,"AVAL"),fx$graph$nodes),`[[`,"","id"))
    put_status(fx$proj,"ADVS",id,modifyList(blank_status(),list(level="L3")))
  chg <- list(id="ADVS.CHG@CARRY_OVER",var="CHG",paramcd="CARRY_OVER",kind="param_variable")
  fx$graph$nodes <- c(fx$graph$nodes,list(chg)); save_graph(fx$proj,fx$graph)
  pg <- param_graph_read(fx$proj,"ADVS")
  expect_null(param_layer_block_message(fx$proj,"ADVS","primary","BMI",fx$spec,pg))
  aval_id <- "ADVS.AVAL@CARRY_OVER"
  put_status(fx$proj,"ADVS",aval_id,blank_status())
  expect_match(param_layer_block_message(fx$proj,"ADVS","primary","BMI",fx$spec,pg),
    "blocked by unfinished layer CARRY_OVER: AVAL",fixed=TRUE)
})

test_that("fullrun stops at errors, reports partial skips, and ignores _work", {
  fx <- fullrun_fixture()
  fullrun_write_code(fx$p, "ADSL.USUBJID", "primary", "result <- data.frame(USUBJID='A')")
  fullrun_write_code(fx$p, "ADSL.AGE", "primary", "stop('intentional failure')")
  old <- file.path(fx$p, ".dualdag", "runs", "ADSL", "_work")
  dir.create(old, recursive = TRUE)
  saveRDS(data.frame(USUBJID = "STALE", AGE = 999), file.path(old, "primary.rds"))
  fail <- fullrun_worker(fx$p, "ADSL", "primary")
  expect_false(fail$ok)
  expect_equal(fail$failed_variable, "ADSL.AGE")
  expect_match(fail$error, "intentional failure")

  unlink(file.path(fx$p, ".dualdag", "fullrun"), recursive = TRUE)
  fullrun_write_code(fx$p, "ADSL.USUBJID", "validation", "")
  fullrun_write_code(fx$p, "ADSL.AGE", "validation", "result <- data.frame(USUBJID=work$USUBJID, AGE=1)")
  skipped <- fullrun_worker(fx$p, "ADSL", "validation")
  expect_true(skipped$ok)
  expect_true(skipped$partial)
  expect_equal(skipped$not_run, list(list(id = "ADSL.USUBJID", reason = "no code"),
                                     list(id = "ADSL.AGE", reason = "upstream ADSL.USUBJID not run")))

  fullrun_write_code(fx$p, "ADSL.USUBJID", "validation", "result <- data.frame(USUBJID='A')")
  fullrun_write_code(fx$p, "ADSL.AGE", "validation", "result <- (")
  syntax <- fullrun_worker(fx$p, "ADSL", "validation")
  expect_false(syntax$ok)
  expect_equal(syntax$failed_variable, "ADSL.AGE")
  expect_match(syntax$error, "unexpected|incomplete|unexpected end", ignore.case = TRUE)

  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Subject,Char,20,1,,", "ADSL,AGE,Age,Num,8,,,", "ADSL,AGE2,Age 2,Num,8,,,"),
    file.path(fx$p, "spec", "variable.csv"))
  save_graph(fx$proj, list(dataset = "ADSL", nodes = lapply(c("ADSL.USUBJID", "ADSL.AGE", "ADSL.AGE2"), function(id) list(id = id)),
    edges = list(list(from = "ADSL.USUBJID", to = "ADSL.AGE"), list(from = "ADSL.AGE", to = "ADSL.AGE2"))))
  fullrun_write_code(fx$p, "ADSL.USUBJID", "validation", "result <- data.frame(USUBJID='A')")
  fullrun_write_code(fx$p, "ADSL.AGE", "validation", "stop('failed ancestor')")
  fullrun_write_code(fx$p, "ADSL.AGE2", "validation", "result <- data.frame(USUBJID='A', AGE2=2)")
  failed_ancestor <- fullrun_worker(fx$p, "ADSL", "validation")
  expect_false(failed_ancestor$ok)
  expect_true(any(vapply(failed_ancestor$not_run, function(x) identical(x$id, "ADSL.AGE2") && identical(x$reason, "upstream ADSL.AGE not run"), TRUE)))
})

test_that("current program and partial plain run are available before any prior run", {
  fx <- fullrun_fixture()
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Subject,Char,20,1,,", "ADSL,AGE,Age,Num,8,,,", "ADSL,AGE2,Age 2,Num,8,,,"),
    file.path(fx$p, "spec", "variable.csv"))
  save_graph(fx$proj, list(dataset = "ADSL", nodes = lapply(c("ADSL.USUBJID", "ADSL.AGE", "ADSL.AGE2"), function(id) list(id = id)),
    edges = list(list(from = "ADSL.USUBJID", to = "ADSL.AGE"), list(from = "ADSL.AGE", to = "ADSL.AGE2"))))
  fullrun_write_code(fx$p, "ADSL.USUBJID", "primary", "result <- data.frame(USUBJID=c('A','B'))")
  fullrun_write_code(fx$p, "ADSL.AGE2", "primary", "result <- data.frame(USUBJID=work$USUBJID, AGE2=1)")
  fullrun_write_code(fx$p, "ADSL.AGE", "validation", "result <- data.frame(USUBJID='V', AGE=9)")
  app <- create_app(fullrun_root, fx$p, 18739L, "fullrun-program-token", load_config())
  state_files <- list.files(file.path(fx$p, ".dualdag"), recursive = TRUE, full.names = TRUE)
  before_hash <- tools::md5sum(state_files[file.info(state_files)$isdir %in% FALSE])
  before <- fullrun_call(app, "/api/fullrun/program?dataset=ADSL&side=primary")
  expect_equal(before$status, 200L)
  expect_match(rawToChar(before$body), "ADSL.USUBJID", fixed = TRUE)
  expect_false(file.exists(file.path(fullrun_dir(fx$p, "ADSL", "primary"), "program.R")))
  steps <- fullrun_call(app, "/api/fullrun/steps?dataset=ADSL&side=primary")
  expect_equal(steps$status, 200L)
  expect_equal(vapply(from_json(steps$body)$steps, `[[`, "", "id"), c("ADSL.USUBJID", "ADSL.AGE2"))
  validation_steps <- fullrun_call(app, "/api/fullrun/steps?dataset=ADSL&side=validation")
  expect_equal(vapply(from_json(validation_steps$body)$steps, `[[`, "", "id"), "ADSL.AGE")
  expect_match(from_json(validation_steps$body)$steps[[1]]$code, "AGE=9")
  expect_false(grepl("AGE=9", rawToChar(before$body), fixed = TRUE))
  after_files <- list.files(file.path(fx$p, ".dualdag"), recursive = TRUE, full.names = TRUE)
  after_hash <- tools::md5sum(after_files[file.info(after_files)$isdir %in% FALSE])
  expect_identical(after_hash, before_hash)
  run <- fullrun_worker(fx$p, "ADSL", "primary")
  expect_true(run$ok)
  expect_true(run$partial)
  expect_equal(run$variables_total, 3L)
  expect_equal(run$not_run, list(list(id = "ADSL.AGE", reason = "no code"),
                                list(id = "ADSL.AGE2", reason = "upstream ADSL.AGE not run")))
  out <- readRDS(file.path(fullrun_dir(fx$p, "ADSL", "primary"), "result.rds"))
  expect_named(out, "USUBJID")
  expect_equal(out$USUBJID, c("A", "B"))
  expect_match(fullrun_code(fx$p, "ADSL", "ADSL.AGE", "validation"), "AGE=9")
})

test_that("program route is successful when a side has no code", {
  fx <- fullrun_fixture()
  app <- create_app(fullrun_root, fx$p, 18739L, "fullrun-empty-token", load_config())
  before <- fullrun_call(app, "/api/fullrun/program?dataset=ADSL&side=primary")
  steps <- fullrun_call(app, "/api/fullrun/steps?dataset=ADSL&side=primary")
  expect_equal(before$status, 200L)
  expect_match(rawToChar(before$body), "Generated Dualdag full-program run", fixed = TRUE)
  expect_equal(steps$status, 200L)
  expect_length(from_json(steps$body)$steps, 0L)
  expect_false(file.exists(file.path(fullrun_dir(fx$p, "ADSL", "primary"), "program.R")))
})

test_that("layered partial run keeps runnable layers and skips layers without records code", {
  fx <- layered_fullrun_fixture()
  for (side in c("primary", "validation")) {
    fullrun_write_code(fx$p, "X.AVAL@Q", side, "")
    fullrun_write_code(fx$p, "X.RECORDS@R", side, "")
  }
  run <- fullrun_worker(fx$p, "X", "primary", timeout = 60)
  expect_true(run$ok, info = run$error %||% "")
  expect_true(run$partial)
  expect_equal(run$variables_total, length(fullrun_plan(fx$p, "X", "primary")$ids))
  expect_true(any(vapply(run$not_run, function(x) identical(x$id, "X.RECORDS@R") && identical(x$reason, "no code"), TRUE)))
  expect_true(any(vapply(run$not_run, function(x) identical(x$id, "X.AVAL@R") && grepl("no records code", x$reason), TRUE)))
  expect_true(any(vapply(run$not_run, function(x) identical(x$id, "X.AVAL@Q") && identical(x$reason, "no code"), TRUE)))
  out <- readRDS(file.path(fullrun_dir(fx$p, "X", "primary"), "result.rds"))
  expect_true(all(c("PARAMCD", "USUBJID", "AVAL") %in% names(out)))
  expect_false("R" %in% out$PARAMCD)
  expect_false(any(is.na(out$AVAL[out$PARAMCD == "P"])))
  expect_true(all(is.na(out$AVAL[out$PARAMCD == "Q"])))
})

test_that("fullrun runs installed non-attached packages and names missing package failures", {
  fx <- fullrun_fixture()
  r_packages_save(fx$p, list(r_packages = list(), override = FALSE))
  fullrun_write_code(fx$p, "ADSL.USUBJID", "primary", "result <- data.frame(USUBJID='A')")
  fullrun_write_code(fx$p, "ADSL.AGE", "primary",
    "library(tools)\nresult <- data.frame(USUBJID=work$USUBJID, AGE=nchar(tools::file_path_sans_ext('a.R')))"
  )
  ran <- fullrun_worker(fx$p, "ADSL", "primary", timeout = 60)
  expect_true(ran$ok, info = ran$error %||% "")
  expect_equal(readRDS(file.path(fullrun_dir(fx$p, "ADSL", "primary"), "result.rds"))$AGE, 1L)

  fullrun_write_code(fx$p, "ADSL.AGE", "primary",
    "library(dualdagnosuchpkg)\nresult <- data.frame(USUBJID=work$USUBJID, AGE=1)"
  )
  missing <- fullrun_worker(fx$p, "ADSL", "primary", timeout = 60)
  expect_false(missing$ok)
  expect_equal(missing$failed_variable, "ADSL.AGE")
  expect_equal(missing$error, paste0('This program needs R package dualdagnosuchpkg, which is not installed. Install it in the R at ',
                                     R.home(), ': install.packages("dualdagnosuchpkg"). Then press Re-run.'))
  expect_match(paste(readLines(file.path(fullrun_dir(fx$p, "ADSL", "primary"), "log.txt"), warn = FALSE), collapse = "\n"),
               missing$error, fixed = TRUE)
})

test_that("fullrun keeps only the target result column and preserves key rows", {
  fx <- fullrun_fixture()
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
               "ADSL,USUBJID,Subject,Char,20,1,,",
               "ADSL,AGE,Age,Num,8,,,",
               "ADSL,AGE2,Age 2,Num,8,,,"), file.path(fx$p, "spec", "variable.csv"))
  save_graph(fx$proj, list(dataset = "ADSL", nodes = list(list(id = "ADSL.USUBJID"), list(id = "ADSL.AGE"), list(id = "ADSL.AGE2")),
    edges = list(list(from = "ADSL.USUBJID", to = "ADSL.AGE"), list(from = "ADSL.AGE", to = "ADSL.AGE2"))))
  fullrun_write_code(fx$p, "ADSL.USUBJID", "primary", "result <- data.frame(USUBJID=c('A','B'))")
  fullrun_write_code(fx$p, "ADSL.AGE", "primary", "result <- transform(work, AGE=20, EXTRA='discard')")
  fullrun_write_code(fx$p, "ADSL.AGE2", "primary", "result <- data.frame(USUBJID=work$USUBJID, AGE2=work$AGE+1)")
  ran <- fullrun_worker(fx$p, "ADSL", "primary")
  expect_true(ran$ok)
  got <- readRDS(file.path(fullrun_dir(fx$p, "ADSL", "primary"), "result.rds"))
  expect_named(got, c("USUBJID", "AGE", "AGE2"))
  expect_equal(got$AGE2, c(21, 21))

  fullrun_write_code(fx$p, "ADSL.AGE", "primary", "result <- data.frame(USUBJID=c('A','B','C'), AGE=20)")
  extra <- fullrun_worker(fx$p, "ADSL", "primary")
  expect_false(extra$ok)
  expect_equal(extra$failed_variable, "ADSL.AGE")
  expect_match(extra$error, "outside the key dataset")
})

test_that("fullrun leaves ordinary run, work, and state files byte-identical", {
  fx <- fullrun_fixture()
  fullrun_write_code(fx$p, "ADSL.USUBJID", "primary", "result <- data.frame(USUBJID='A')")
  fullrun_write_code(fx$p, "ADSL.AGE", "primary", "writeLines('marker', 'relative-marker.txt'); result <- data.frame(USUBJID=work$USUBJID, AGE=1)")
  protected <- c(file.path(fx$p, ".dualdag", "state", "state.json"), file.path(fx$p, ".dualdag", "state", "graph_ADSL.json"))
  protected <- protected[file.exists(protected)]
  before <- lapply(protected, readBin, what = "raw", n = 1e7)
  dirs <- c(file.path(fx$p, ".dualdag", "runs"), file.path(fx$p, ".dualdag", "_work"))
  snapshots <- lapply(dirs, function(d) if (dir.exists(d)) tools::md5sum(list.files(d, recursive = TRUE, full.names = TRUE)) else character())
  fullrun_worker(fx$p, "ADSL", "primary")
  expect_false(file.exists(file.path(fx$p, "relative-marker.txt")))
  expect_true(file.exists(file.path(fullrun_dir(fx$p, "ADSL", "primary"), "relative-marker.txt")))
  expect_equal(lapply(protected, readBin, what = "raw", n = 1e7), before)
  expect_equal(lapply(dirs, function(d) if (dir.exists(d)) tools::md5sum(list.files(d, recursive = TRUE, full.names = TRUE)) else character()), snapshots)
})

test_that("fullrun API rejects a duplicate side job and serves artifacts", {
  fx <- fullrun_fixture()
  A <- create_app(fullrun_root, fx$p, 7979L, "fullrun-token", load_config())
  A$jobs$held <- list(id = "held", kind = "fullrun", dataset = "ADSL", node = "primary")
  denied <- fullrun_call(A, "/api/fullrun/run", "POST", list(dataset = "ADSL", side = "primary"))
  expect_equal(denied$status, 409L)
  A$jobs$held <- NULL
  fullrun_write_code(fx$p, "ADSL.USUBJID", "primary", "result <- data.frame(USUBJID='A')")
  accepted <- fullrun_call(A, "/api/fullrun/run", "POST", list(dataset = "ADSL", side = "primary"))
  expect_equal(accepted$status, 202L)
  status <- fullrun_call(A, "/api/fullrun/status?dataset=ADSL")
  expect_named(from_json(status$body), c("dataset", "sides"))
})
