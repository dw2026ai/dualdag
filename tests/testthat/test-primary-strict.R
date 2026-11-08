source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))
source(file.path(testthat::test_path(), "..", "..", "R", "inspect.R"), local = FALSE)
root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in c("config.R", "spec.R", "project.R", "catalog.R", "server.R", "dag_agent.R")) {
  sys.source(file.path(root, "R", f), envir = globalenv())
}

strict_test_project <- function() {
  proj <- temp_project()
  dir.create(file.path(proj, "spec"), showWarnings = FALSE)
  writeLines(paste(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,STUDYID,Study,Char,20,1,DM.STUDYID,copy",
    "ADSL,USUBJID,Subject,Char,20,2,DM.USUBJID,copy",
    "ADSL,AGE,Age,Num,8,,DM.AGE,copy",
    "ADSL,HEIGHT,Height,Num,8,,VS.HEIGHT,copy"), collapse = "\n"),
    file.path(proj, "spec", "variable.csv"))
  haven::write_xpt(data.frame(STUDYID = c("S", "S"), USUBJID = c("S1", "S2"),
    AGE = c(20, 40), HEIGHT = c(150, 160), WEIGHT = c(50, 60)), file.path(proj, "data", "DM.xpt"))
  graph <- make_graph("ADSL", list(gn("ADSL.STUDYID"), gn("ADSL.USUBJID"), gn("ADSL.AGE"), gn("ADSL.HEIGHT")))
  save_graph(proj, graph)
  proj
}

outside_code <- 'result <- dplyr::select(src$VS, FOO)'

test_that("primary dependency parser catches explicit and bare outside inputs", {
  cols <- list(VS = c("HEIGHT", "WEIGHT", "VSTESTCD"))
  for (code in c("x <- src$VS$WEIGHT", 'x <- src[["VS"]][["WEIGHT"]]',
                 "result <- dplyr::mutate(src$VS, X = WEIGHT)")) {
    uses <- code_input_uses(code, cols, "USUBJID")
    expect_true(uses$ok)
    expect_true(any(vapply(uses$uses, function(u) identical(u$origin, "VS.WEIGHT"), logical(1))))
  }
})

test_that("primary dependency parser ignores local names, string literals and mutate outputs", {
  cols <- list(VS = c("WEIGHT", "VSTESTCD"))
  safe <- c("WEIGHT <- 1; result <- data.frame(X = WEIGHT)",
            'result <- dplyr::filter(src$VS, VSTESTCD == "WEIGHT")',
            'result <- dplyr::mutate(src$VS, NEWCOL = 1)')
  for (code in safe) {
    uses <- code_input_uses(code, cols, "USUBJID")
    expect_false(any(vapply(uses$uses, function(u) identical(u$origin, "VS.WEIGHT"), logical(1))))
  }
})

test_that("explicit string selectors remain dependency references", {
  cols <- list(VS = "WEIGHT")
  selectors <- c(
    'src$VS |> dplyr::select("WEIGHT")',
    'src$VS |> dplyr::select(dplyr::all_of("WEIGHT"))',
    'src$VS |> dplyr::select(dplyr::any_of(c("WEIGHT")))',
    'src$VS[, "WEIGHT", drop = FALSE]',
    'df <- src$VS; df[, "WEIGHT", drop = FALSE]'
  )
  for (code in selectors) {
    uses <- code_input_uses(code, cols, "USUBJID")
    expect_true(any(vapply(uses$uses, function(u) identical(u$origin, "VS.WEIGHT"), logical(1))))
  }
  extra <- code_input_uses('dplyr::select(src$VS, c("USUBJID"), "WEIGHT")',
                           list(VS = c("USUBJID", "WEIGHT")), "USUBJID")
  expect_true(any(vapply(extra$uses, function(u) identical(u$origin, "VS.WEIGHT"), logical(1))))
  aliases <- code_input_uses('lb <- src$LB; dm <- src$LB; dm[, "FOO"]', list(LB = "FOO"), "USUBJID")
  expect_true(any(vapply(aliases$uses, function(u) identical(u$origin, "LB.FOO"), logical(1))))
})

test_that("string literals outside source selectors are not dependencies", {
  domains <- list(VS = c("VSTESTCD", "WEIGHT", "VISIT"), LB = c("LBTESTCD", "VISIT", "FOO"), DM = c("ARM"))
  cases <- list(
    list(code = 'src$VS[src$VS$VSTESTCD == "WEIGHT", ]', absent = "VS.WEIGHT"),
    list(code = 'x <- src$VS$VSSTRESN[src$VS$VSTESTCD == "WEIGHT"]', absent = "VS.WEIGHT"),
    list(code = 'vs <- src$VS; x <- vs$VSSTRESN[vs$VSTESTCD == "WEIGHT"]', absent = "VS.WEIGHT"),
    list(code = 'x <- src$VS[src$VS$VSTESTCD == "WEIGHT"]', absent = "VS.WEIGHT"),
    list(code = 'x <- src$VS[, ifelse(TRUE, "VISIT", "WEIGHT")]', absent = "VS.WEIGHT"),
    list(code = 'x <- src$VS[, ifelse(TRUE, "VISIT", "WEIGHT")]', absent = "VS.VISIT"),
    list(code = 'src$VS |> dplyr::select("VISIT"); b <- src$LB$LBTESTCD', absent = "LB.VISIT"),
    list(code = 'x <- src$DM$AGE; work |> dplyr::select("ARM")', absent = "DM.ARM"),
    list(code = '# do not use select("FOO")\npaste("select(", "FOO")', absent = "LB.FOO")
  )
  for (case in cases) {
    uses <- code_input_uses(case$code, domains, "USUBJID")
    expect_false(any(vapply(uses$uses, function(u) identical(u$origin, case$absent), logical(1))))
  }
})

test_that("primary lineage allowlist keeps source and work variables narrow", {
  spec <- spec_of(V("ADSL", "USUBJID", key = 1), V("ADSL", "AGE", source = "DM.AGE"),
                  V("ADSL", "HEIGHT", source = "VS.HEIGHT"))
  graph <- make_graph("ADSL", list(gn("ADSL.AGE")), edges = list())
  allowed <- lineage_context(spec, graph, gn("ADSL.AGE"), NULL, "primary")
  expect_true("DM.AGE" %in% allowed$source_refs)
  expect_false("VS.HEIGHT" %in% allowed$source_refs)
  expect_true(is.character(allowed$ids))
})

test_that("a parent card changes prompt expansion but preserves full primary scope", {
  proj <- temp_project()
  spec <- spec_of(V("ADSL","USUBJID",key=1,source="DM.USUBJID"),
    V("ADSL","A",source="DM.ACOL"),V("ADSL","B",source="ADSL.A"),V("ADSL","C",source="ADSL.B"))
  graph <- make_graph("ADSL",list(gn("ADSL.USUBJID"),gn("ADSL.A"),gn("ADSL.B"),gn("ADSL.C")),
    list(ge("ADSL.A","ADSL.B"),ge("ADSL.B","ADSL.C")))
  haven::write_xpt(data.frame(USUBJID=c("S1","S2"),ACOL=c(3,4),OTHER=c(8,9)),file.path(proj,"data","DM.xpt"))
  save_graph(proj,graph)
  before <- lineage_context(spec,graph,gn("ADSL.C"),proj,"primary")
  expect_true(all(c("ADSL.C","ADSL.B","ADSL.A") %in% before$ids))
  expect_true("DM.ACOL" %in% before$source_refs)
  code <- 'result <- data.frame(USUBJID = work$USUBJID, C = work$A + work$B + nrow(src$DM), source_cols = paste(names(src$DM), collapse=","), work_cols = paste(names(work), collapse=","))'
  domain_cols <- list(DM=c("USUBJID","ACOL","OTHER"))
  expect_length(lineage_code_warnings(code,"primary","ADSL","ADSL.C",before,domain_cols,"USUBJID"),0L)
  old_refresh <- get("refresh_work_side",envir=.GlobalEnv)
  old_upstream <- get("build_upstream",envir=.GlobalEnv)
  assign("refresh_work_side",function(...) invisible(TRUE),envir=.GlobalEnv)
  assign("build_upstream",function(...) data.frame(USUBJID=c("S1","S2"),A=c(1,2),B=c(3,4)),envir=.GlobalEnv)
  on.exit(assign("refresh_work_side",old_refresh,envir=.GlobalEnv),add=TRUE)
  on.exit(assign("build_upstream",old_upstream,envir=.GlobalEnv),add=TRUE)
  run_before <- run_side(proj,"ADSL","ADSL.C","primary",code,timeout=60,graph=graph,spec=spec)
  expect_true(run_before$ok,info=run_before$error)
  out_before <- readRDS(run_before$out_path)

  bdir <- write_result(proj,"ADSL","ADSL.B","primary",data.frame(B=1:2))
  writeLines("result <- data.frame(B = 1:2)",file.path(bdir,"code.R"))
  card_write(proj,"ADSL","ADSL.B","primary","B card",graph,spec)
  after <- lineage_context(spec,graph,gn("ADSL.C"),proj,"primary")
  expect_identical(after$ids,before$ids)
  expect_identical(after$source_refs,before$source_refs)
  expect_length(lineage_code_warnings(code,"primary","ADSL","ADSL.C",after,domain_cols,"USUBJID"),0L)
  expect_match(after$text,"B card",fixed=TRUE)
  expect_match(after$text,"ADSL.A\n",fixed=TRUE)
  expect_match(before$text,"ADSL.A\n",fixed=TRUE)

  run_after <- run_side(proj,"ADSL","ADSL.C","primary",code,timeout=60,graph=graph,spec=spec)
  expect_true(run_after$ok,info=run_after$error)
  out_after <- readRDS(run_after$out_path)
  expect_true(all(out_before$source_cols=="USUBJID,ACOL"))
  expect_true(all(out_before$work_cols=="USUBJID,A,B"))
  expect_identical(out_after$source_cols,out_before$source_cols)
  expect_identical(out_after$work_cols,out_before$work_cols)
})

test_that("start_side_proc restricts primary src, work, upstream and temp frame to lineage columns and keys", {
  proj <- strict_test_project()
  spec <- load_project_spec(proj); graph <- load_graph(proj, "ADSL")
  write_result(proj, "ADSL", "ADSL.HEIGHT", "primary", data.frame(STUDYID = "S", USUBJID = "S1", HEIGHT = 170))
  p <- open_project(proj)
  status <- blank_status(); status$level <- "L3"
  put_status(p, "ADSL", "ADSL.HEIGHT", status)
  saveRDS(data.frame(STUDYID = "S", USUBJID = "S1", HEIGHT = 170), work_path(proj, "ADSL", "primary"))
  expect_true("HEIGHT" %in% names(readRDS(work_path(proj, "ADSL", "primary"))))
  old_refresh <- get("refresh_work_side", envir = .GlobalEnv)
  old_upstream <- get("build_upstream", envir = .GlobalEnv)
  assign("refresh_work_side", function(...) invisible(TRUE), envir = .GlobalEnv)
  assign("build_upstream", function(...) data.frame(STUDYID = "S", USUBJID = "S1", HEIGHT = 170), envir = .GlobalEnv)
  on.exit(assign("refresh_work_side", old_refresh, envir = .GlobalEnv), add = TRUE)
  on.exit(assign("build_upstream", old_upstream, envir = .GlobalEnv), add = TRUE)
  code <- 'stopifnot(all(c("STUDYID", "USUBJID", "AGE") %in% names(src$DM)), !"WEIGHT" %in% names(src$DM)); stopifnot(all(c("STUDYID", "USUBJID") %in% names(work)), !"HEIGHT" %in% names(work)); stopifnot(identical(names(work), names(upstream)), identical(names(work), names(ADSL_TEMP)), !"HEIGHT" %in% names(ADSL_TEMP)); result <- data.frame(USUBJID = src$DM$USUBJID, AGE = src$DM$AGE)'
  ran <- run_side(proj, "ADSL", "ADSL.AGE", "primary", code, timeout = 60, graph = graph, spec = spec)
  expect_true(ran$ok, info = ran$error)
  out <- readRDS(ran$out_path)
  expect_identical(names(out), c("USUBJID", "AGE"))
  expect_false("HEIGHT" %in% names(out))
})

test_that("outside-lineage source refs run and leave a warning beside the natural R error", {
  proj <- strict_test_project()
  haven::write_xpt(data.frame(USUBJID = c("S1", "S2"), FOO = 1:2), file.path(proj, "data", "VS.xpt"))
  spec <- load_project_spec(proj); graph <- load_graph(proj, "ADSL")
  code <- 'result <- dplyr::select(src$VS, FOO)'
  ran <- run_side(proj, "ADSL", "ADSL.AGE", "primary", code, timeout=60, graph=graph, spec=spec)
  expect_false(ran$ok)
  expect_false(grepl("DUALDAG_MISSING_COLUMN|DUALDAG_MISSING_DEPENDENCY", ran$error %||% ran$log, perl=TRUE))
  expect_true(grepl("no applicable method", ran$error %||% ran$log, ignore.case=TRUE))
  p <- open_project(proj)
  apply_generate_results(p, "ADSL", "ADSL.AGE", list(primary = list(ok = FALSE, error = ran$error)))
  st <- get_status(p, "ADSL", "ADSL.AGE")
  expect_true(any(grepl("^Lineage warning: AGE reads VS.FOO, which is not in its DAG lineage\\.$", unlist(st$warnings), perl=TRUE)))
  allowed <- lineage_context(spec, graph, gn("ADSL.AGE"), proj, "primary")
  frame_warning <- lineage_code_warnings('LINEAGE_ADSL$AGE <- LINEAGE_ADSL$USUBJID + LINEAGE_ADSL$HEIGHT',
    "primary", "ADSL", "ADSL.AGE", allowed, list(), "USUBJID")
  expect_identical(frame_warning, "Lineage warning: AGE reads HEIGHT, which is not in its DAG lineage.")
  expect_length(lineage_code_warnings('LINEAGE_ADSL$AGE <- LINEAGE_ADSL$HEIGHT',
    "validation", "ADSL", "ADSL.AGE", allowed, list(), "USUBJID"), 0L)
  code_path <- file.path(side_run_dir(proj,"ADSL","ADSL.AGE","primary"),"code.R")
  write_text('LINEAGE_ADSL$AGE <- LINEAGE_ADSL$USUBJID + LINEAGE_ADSL$HEIGHT', code_path)
  st <- attach_lineage_warnings(proj,"ADSL","ADSL.AGE",st,"primary")
  expect_true(any(grepl("Lineage warning: AGE reads HEIGHT",unlist(st$warnings),fixed=TRUE)))
  write_text('result <- data.frame(USUBJID = src$DM$USUBJID, AGE = src$DM$AGE)', code_path)
  st <- attach_lineage_warnings(proj,"ADSL","ADSL.AGE",st,"primary")
  expect_false(any(grepl("^Lineage warning:",unlist(st$warnings),perl=TRUE)))
  write_text('LINEAGE_ADSL$AGE <- LINEAGE_ADSL$USUBJID + LINEAGE_ADSL$HEIGHT', code_path)
  graph$edges <- list(ge("ADSL.HEIGHT","ADSL.AGE"))
  save_graph(proj,graph)
  st <- attach_lineage_warnings(proj,"ADSL","ADSL.AGE",st,"primary")
  expect_false(any(grepl("^Lineage warning:",unlist(st$warnings),perl=TRUE)))
})

test_that("stored and batch side runs keep filtering and allow natural source errors", {
  proj <- strict_test_project()
  haven::write_xpt(data.frame(USUBJID = c("S1", "S2"), FOO = 1:2), file.path(proj, "data", "VS.xpt"))
  spec <- load_project_spec(proj); graph <- load_graph(proj, "ADSL")
  sdir <- side_run_dir(proj, "ADSL", "ADSL.AGE", "primary")
  dir.create(sdir, recursive = TRUE, showWarnings = FALSE)
  write_text(outside_code, file.path(sdir, "code.R"))
  stored <- run_stored_sides(proj, "ADSL", "ADSL.AGE", "primary", graph = graph, spec = spec)
  expect_false(stored$results$primary$ok)
  expect_true(grepl("no applicable method", stored$results$primary$error, ignore.case=TRUE))
  batch <- run_sides(proj, "ADSL", "ADSL.AGE", list(primary = outside_code), graph = graph, spec = spec)
  expect_false(batch$primary$ok)
  expect_true(grepl("no applicable method", batch$primary$error, ignore.case=TRUE))
})

test_that("a first-time key program runs against source rows with empty work", {
  proj <- strict_test_project()
  spec <- load_project_spec(proj); graph <- load_graph(proj, "ADSL")
  ran <- run_side(proj, "ADSL", "ADSL.STUDYID", "primary",
    'result <- src$DM[, c("STUDYID", "USUBJID"), drop = FALSE]', timeout = 60, graph = graph, spec = spec)
  expect_true(ran$ok, info = ran$error)
  expect_equal(nrow(readRDS(ran$out_path)), 2L)
})

test_that("validation retains full source data and lookup answers outside ids", {
  proj <- strict_test_project()
  spec <- load_project_spec(proj); graph <- load_graph(proj, "ADSL")
  write_result(proj, "ADSL", "ADSL.HEIGHT", "primary", data.frame(STUDYID = "PRIMARY_SENTINEL", USUBJID = "P1", HEIGHT = 170))
  ran <- run_side(proj, "ADSL", "ADSL.AGE", "validation",
    'stopifnot("WEIGHT" %in% names(src$DM), !"PRIMARY_SENTINEL" %in% unlist(work, use.names = FALSE)); result <- data.frame(USUBJID = src$DM$USUBJID, AGE = src$DM$AGE)', timeout = 60, graph = graph, spec = spec)
  expect_true(ran$ok)
  expect_identical(names(readRDS(ran$out_path)), c("USUBJID", "AGE"))
  outside <- dag_lookup_block(spec, graph, "ADSL", "ADSL.HEIGHT", allowed_ids = "ADSL.AGE")
  expect_match(outside, "Outside this variable's lineage", fixed = TRUE)
  validation <- dag_lookup_block(spec, graph, "ADSL", "ADSL.HEIGHT", allowed_ids = NULL)
  expect_match(validation, "ADSL.HEIGHT", fixed = TRUE)
})

test_that("validation code does not add primary lineage warnings", {
  proj <- strict_test_project()
  p <- open_project(proj)
  write_text('result <- data.frame(USUBJID = src$DM$WEIGHT)', file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "validation"), "code.R"))
  out <- write_result(proj, "ADSL", "ADSL.AGE", "validation", data.frame(USUBJID = "S1", AGE = 50))
  result <- list(validation = list(ok = TRUE, value = list(same_source = FALSE,
    run = list(ok = TRUE, out_path = file.path(out, "out.rds")))))
  st <- apply_generate_results(p, "ADSL", "ADSL.AGE", result)
  expect_null(st$warnings)
})

test_that("validation prompt caps other-domain names and excludes primary program text", {
  proj <- strict_test_project()
  spec <- load_project_spec(proj); graph <- load_graph(proj, "ADSL")
  p <- open_project(proj)
  write_text("PRIMARY_CODE_SENTINEL", file.path(side_run_dir(proj, "ADSL", "ADSL.AGE", "primary"), "code.R"))
  append_side_note(p, "ADSL", "ADSL.AGE", "primary", "PRIMARY_NOTE_SENTINEL")
  cat <- list(domains = list(VS = list(variables = lapply(seq_len(1200), function(i) list(name = paste0("COL", i))))))
  prompt <- side_user_prompt("validation", spec, graph, gn("ADSL.AGE"), character(),
                             catalog = cat, project = proj,
                             current_code = "VALIDATION_CURRENT_CODE")
  start <- regexpr("SOURCE_DOMAINS \\(names only\\):\\n",prompt,perl=TRUE)[[1]]
  tail <- if (start>0L) substring(prompt,start) else ""
  end <- regexpr("\\nWORK_KEYS:",tail,perl=TRUE)[[1]]
  block <- if (end>0L) substring(tail,1L,end-1L) else tail
  expect_lte(nchar(block), 4040L)
  expect_match(block,"VS: COL1",fixed=TRUE)
  expect_false(grepl("label|type",block,ignore.case=TRUE))
  expect_match(prompt, "VALIDATION_CURRENT_CODE", fixed = TRUE)
  expect_false(grepl("PRIMARY", prompt, fixed = TRUE))
  expect_false(grepl("PRIMARY_CODE_SENTINEL|PRIMARY_NOTE_SENTINEL", prompt, perl = TRUE))
})

adsl_binding_project <- function(include_adsl = TRUE) {
  proj <- temp_project()
  spec <- spec_of(V("ADVS", "USUBJID", key = 1, source = "DM.USUBJID"),
                  V("ADVS", "X", type = "Num", source = "VS.HEIGHT"))
  graph <- make_graph("ADVS", list(gn("ADVS.USUBJID"), gn("ADVS.X")))
  save_graph(proj, graph)
  haven::write_xpt(data.frame(USUBJID = c("S1", "S2"), HEIGHT = c(160, 170), SECRET = c("a", "b")),
                   file.path(proj, "data", "VS.xpt"))
  if (include_adsl) haven::write_xpt(data.frame(USUBJID = c("S1", "S2"), AGE = c(30, 40), ARM = c("A", "B")),
                                     file.path(proj, "data", "ADSL.xpt"))
  list(project = proj, spec = spec, graph = graph)
}

test_that("primary AVAILABLE_DATASETS lists the whole ADSL source without values", {
  x <- adsl_binding_project()
  prompt <- side_user_prompt("primary", x$spec, x$graph, gn("ADVS.X"), "USUBJID", project = x$project)
  expect_match(prompt, "LINEAGE_SRC_ADSL (source; 2 rows): USUBJID, AGE, ARM", fixed = TRUE)
  expect_false(grepl("S1|30|40|A, B", prompt))
  expect_false(grepl("LINEAGE_SRC_ADSL", side_user_prompt("validation", x$spec, x$graph,
    gn("ADVS.X"), "USUBJID", project = x$project), fixed = TRUE))
})

test_that("primary may read any ADSL column while VS stays lineage strict", {
  x <- adsl_binding_project()
  ran <- run_side(x$project, "ADVS", "ADVS.X", "primary",
    'stopifnot(identical(names(LINEAGE_SRC_ADSL), c("USUBJID", "AGE", "ARM"))); result <- data.frame(USUBJID = LINEAGE_SRC_ADSL$USUBJID, X = LINEAGE_SRC_ADSL$AGE)',
    timeout = 60, graph = x$graph, spec = x$spec)
  expect_true(ran$ok, info = ran$error)
  expect_equal(readRDS(ran$out_path)$X, c(30, 40))
  outside <- start_side_proc(x$project, "ADVS", "ADVS.X", "primary",
    'result <- data.frame(USUBJID = src$VS$SECRET)', x$graph, x$spec)
  expect_null(outside$prep$error)
  expect_true(inherits(outside$proc, "process"))
  if (is.function(outside$proc$kill)) outside$proc$kill()
})

test_that("ADSL exception does not change lineage cards or ADSL builds", {
  x <- adsl_binding_project()
  before <- lineage_context(x$spec, x$graph, gn("ADVS.X"), x$project, "primary")
  write_result(x$project, "ADVS", "ADVS.USUBJID", "primary", data.frame(USUBJID = c("S1", "S2")))
  card_write(x$project, "ADVS", "ADVS.USUBJID", "primary", "key card", x$graph, x$spec)
  after <- lineage_context(x$spec, x$graph, gn("ADVS.X"), x$project, "primary")
  expect_identical(after$ids, before$ids)
  expect_identical(after$source_refs, before$source_refs)
  expect_false(grepl("LINEAGE_SRC_ADSL", after$text, fixed = TRUE))
  adsl_graph <- make_graph("ADSL", list(gn("ADSL.USUBJID"), gn("ADSL.AGE")))
  save_graph(x$project, adsl_graph)
  own <- run_side(x$project, "ADSL", "ADSL.AGE", "primary",
    'stopifnot(!exists("LINEAGE_SRC_ADSL", inherits = FALSE)); result <- data.frame(USUBJID = src$DM$USUBJID, AGE = src$DM$AGE)',
    timeout = 60, graph = adsl_graph,
    spec = spec_of(V("ADSL", "USUBJID", key = 1, source = "DM.USUBJID"), V("ADSL", "AGE", source = "DM.AGE")))
  expect_true(own$ok, info = own$error)
})

test_that("primary works without ADSL source data", {
  x <- adsl_binding_project(include_adsl = FALSE)
  ran <- run_side(x$project, "ADVS", "ADVS.X", "primary",
    'result <- data.frame(USUBJID = src$VS$USUBJID, X = src$VS$HEIGHT)',
    timeout = 60, graph = x$graph, spec = x$spec)
  expect_true(ran$ok, info = ran$error)
  prompt <- side_user_prompt("primary", x$spec, x$graph, gn("ADVS.X"), "USUBJID", project = x$project)
  expect_false(grepl("LINEAGE_SRC_ADSL", prompt, fixed = TRUE))
})

test_that("opening a project leaves a saved nonempty L3 result unchanged", {
  proj <- strict_test_project()
  write_result(proj, "ADSL", "ADSL.AGE", "primary", data.frame(USUBJID = "S1", AGE = 20))
  write_result(proj, "ADSL", "ADSL.AGE", "validation", data.frame(USUBJID = "S1", AGE = 20))
  p <- open_project(proj)
  st <- blank_status(); st$level <- "L3"
  put_status(p, "ADSL", "ADSL.AGE", st)
  reopened <- open_project(proj)
  expect_equal(get_status(reopened, "ADSL", "ADSL.AGE")$level, "L3")
})

