source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))
if (!exists("safe_ds", mode = "function")) source(file.path(testthat::test_path(), "..", "..", "R", "project.R"), local = FALSE)

copy_project <- function() {
  proj <- temp_project()
  file.copy(fixture_path("dm.xpt"), file.path(proj, "data", "dm.xpt"))
  proj
}

test_that("two sides run in separate processes and one error does not affect the other", {
  proj <- copy_project()
  primary <- '
    writeLines(as.character(Sys.getpid()), "started.txt")
    ok <- FALSE
    for (i in 1:160) {
      if (file.exists("../validation/started.txt")) { ok <- TRUE; break }
      Sys.sleep(0.05)
    }
    if (!ok) stop("other side did not start concurrently")
    result <- data.frame(USUBJID = src$DM$USUBJID, TRTSDT = src$DM$RFSTDTC, stringsAsFactors = FALSE)
  '
  validation <- '
    writeLines(as.character(Sys.getpid()), "started.txt")
    ok <- FALSE
    for (i in 1:160) {
      if (file.exists("../primary/started.txt")) { ok <- TRUE; break }
      Sys.sleep(0.05)
    }
    if (!ok) stop("other side did not start concurrently")
    stop("boom")
  '
  res <- run_sides(proj, "ADSL", "ADSL.TRTSDT",
                   list(primary = primary, validation = validation), timeout = 60)
  expect_true(res$primary$ok)
  expect_false(res$validation$ok)
  expect_match(res$validation$error, "boom")
  expect_false(grepl("boom", res$primary$log))
  out <- readRDS(res$primary$out_path)
  expect_equal(out$TRTSDT, c("2020-01-01", "2020-01-02", "2020-01-03"))
  expect_false(grepl("(?m)^> |^\\+ ", res$primary$log, perl = TRUE))
  expect_false(grepl("writeLines", res$primary$log, fixed = TRUE))
  expect_match(res$validation$log, "Error", fixed = TRUE)
  expect_match(res$validation$log, "Execution halted", fixed = TRUE)
  expect_false(grepl("pid:", res$primary$log, fixed = TRUE))
  expect_false(grepl("wrote rows=", res$primary$log, fixed = TRUE))
  expect_false(grepl("pid:", res$validation$log, fixed = TRUE))
  pid1 <- readLines(file.path(proj, ".dualdag", "runs", "ADSL", "TRTSDT", "primary", "started.txt"), warn = FALSE)
  pid2 <- readLines(file.path(proj, ".dualdag", "runs", "ADSL", "TRTSDT", "validation", "started.txt"), warn = FALSE)
  expect_false(identical(pid1, pid2))
  expect_false(identical(pid1, as.character(Sys.getpid())))
})

test_that("validation lineage lists the target once and omits completed descendants", {
  proj <- temp_project()
  spec <- spec_of(V("ADSL","A",type="Num",row=2),V("ADSL","B",type="Num",row=3),
                  V("ADSL","C",type="Num",row=4))
  graph <- make_graph("ADSL",list(gn("ADSL.A"),gn("ADSL.B"),gn("ADSL.C")),
                      list(ge("ADSL.A","ADSL.B"),ge("ADSL.B","ADSL.C")))
  dir.create(dirname(work_path(proj,"ADSL","validation")),recursive=TRUE,showWarnings=FALSE)
  saveRDS(data.frame(A=1,B=2,C=3),work_path(proj,"ADSL","validation"))
  prompt <- side_user_prompt("validation",spec,graph,gn("ADSL.B"),character(),project=proj)
  lineage <- sub("(?s).*LINEAGE:\\n(.*?)\\n(?:WORK_KEYS|LINEAGE_KEYS):.*","\\1",prompt,perl=TRUE)
  expect_equal(length(gregexpr("(?m)^ADSL\\.B\\nROW:",lineage,perl=TRUE)[[1]]),1L)
  expect_false(grepl("ADSL.B \\| B \\|",lineage,perl=TRUE))
  expect_false(grepl("ADSL.C \\| C \\|",lineage,perl=TRUE))
  expect_match(lineage,"ADSL.A | A | Num",fixed=TRUE)
})

test_that("run log contains only R output in execution order", {
  proj <- copy_project()
  code <- paste(
    'cat("printed output\\n")',
    'print("explicit print")',
    '1 + 1',
    'message("a message")',
    'warning("a warning")',
    'stop("an error")',
    sep = "\n"
  )
  ran <- run_side(proj, "ADSL", "ADSL.TRTSDT", "primary", code, timeout = 60)
  expect_false(ran$ok)
  lines <- strsplit(ran$log, "\n", fixed = TRUE)[[1L]]
  expect_match(lines[[1L]], "^# R [0-9.]+; packages: ")
  expect_true(any(lines == "printed output"))
  expect_true(any(lines == '[1] "explicit print"'))
  expect_true(any(lines == "[1] 2"))
  expect_true(any(lines == "a message"))
  expect_true(any(grepl("a warning", lines, fixed = TRUE)))
  expect_true(any(grepl("an error", lines, fixed = TRUE)))
  expect_true(any(lines == "Error: an error"))
  expect_identical(tail(lines, 1L), "Execution halted")
  expect_false(any(grepl("^> |^\\+ ", lines, perl = TRUE)))
  expect_false(any(grepl("NOTE|wrote rows=|pid:", lines)))
  log_path <- file.path(proj, ".dualdag", "runs", "ADSL", "TRTSDT", "primary", "log.txt")
  expect_identical(ran$log, paste(readLines(log_path, warn = FALSE), collapse = "\n"))
  positions <- vapply(c("printed output", "explicit print", "[1] 2", "a message", "a warning", "an error", "Execution halted"),
                      function(x) which(grepl(x, lines, fixed = TRUE))[[1L]], integer(1))
  expect_true(all(diff(positions) > 0))
})

test_that("primary warns for outside-lineage reads and prompt blocks only genuinely missing columns", {
  proj <- copy_project()
  spec <- spec_of(V("ADSL", "A", type="Num", row=2), V("ADSL", "B", type="Num", source="ADSL.C", row=3), V("ADSL", "C", type="Num", row=4))
  graph <- make_graph("ADSL", list(gn("ADSL.A"), gn("ADSL.B"), gn("ADSL.C")), list(ge("ADSL.A", "ADSL.B")))
  node <- graph_node(graph, "ADSL.B")
  # This is the required primary check block with the actual LINEAGE_ target.
  system <- side_generation_system_prompt("primary",spec,graph,node)
  lines <- strsplit(system,"\n",fixed=TRUE)[[1]]
  first <- grep('^cat\\("rows:"',lines)[[1]]
  last <- grep("^The check prints the row count",lines)[[1]]-1L
  block <- gsub("X","B",lines[first:last],fixed=TRUE)
  check <- paste(c('LINEAGE_ADSL <- data.frame(USUBJID=c("1","2"), B=c(1,2))',block),collapse="\n")
  ran <- run_side(proj, "ADSL", node, "primary", check, graph=graph, spec=spec)
  expect_true(ran$ok, info=ran$error %||% ran$log)
  assignment <- run_side(proj, "ADSL", node, "primary",
    'LINEAGE_ADSL <- data.frame(USUBJID="1", B=2)\nLINEAGE_ADSL$B <- LINEAGE_ADSL$B + 1', graph=graph, spec=spec)
  expect_true(assignment$ok, info=assignment$error %||% assignment$log)
  prompt <- side_generation_system_prompt("primary", spec, graph, node)
  expect_match(prompt, "Build from lineage variables first", fixed=TRUE)
  expect_match(prompt, "the app warns", fixed=TRUE)
  expect_match(prompt, "Do not print the app's warning", fixed=TRUE)
  expect_false(grepl("Lineage warning: BASE reads", prompt, fixed=TRUE))
  expect_match(prompt, "Missing column: X is not in LINEAGE_SYSBP or the provided data.", fixed=TRUE)
  expect_true(grepl("DUALDAG_MISSING_COLUMN", prompt, fixed=TRUE))
  expect_false(grepl("DUALDAG_MISSING_DEPENDENCY", prompt, fixed=TRUE))
  validation_prompt <- side_generation_system_prompt("validation", spec, graph, node)
  expect_false(grepl("Lineage warning", validation_prompt, fixed=TRUE))
  expect_false(grepl("DUALDAG_MISSING_COLUMN", validation_prompt, fixed=TRUE))
  user_prompt <- side_user_prompt("primary", spec, graph, node, character())
  expect_match(user_prompt, "missing <- setdiff(expected, names(LINEAGE_ADSL))", fixed=TRUE)
  expect_false(grepl('expected <- c("C")', user_prompt, fixed=TRUE))
  expect_true("A" %in% expected_in_temp_columns(spec, graph, node, "ADSL", "primary"))
  expect_false("C" %in% expected_in_temp_columns(spec, graph, node, "ADSL", "primary"))
  expect_true("C" %in% expected_in_temp_columns(spec, graph, node, "ADSL", "validation"))
})

test_that("fake generation runs a copy and simple arithmetic", {
  proj <- copy_project()
  with_fake_llm({
    spec <- spec_of(
      V("ADSL", "USUBJID", type = "Char", length = 10, key = 1, source = "DM.USUBJID", row = 2),
      V("ADSL", "TRTSDT", label = "Start", type = "Char", length = 20, source = "DM.RFSTDTC",
        derivation = "DM.RFSTDTC", row = 3),
      V("ADSL", "AVAL", type = "Num", length = 8, row = 4),
      V("ADSL", "BASE", type = "Num", length = 8, row = 5),
      V("ADSL", "CHG", type = "Num", length = 8, derivation = "AVAL - BASE", row = 6)
    )
    g <- extract_dependencies(list(), spec, "ADSL")
    expect_true("DM.RFSTDTC" %in% vapply(g$external, function(e) e$ref, ""))

    user_v <- side_user_prompt("validation", spec, g, "ADSL.TRTSDT", character())
    expect_false(grepl("generate_primary", user_v, fixed = TRUE))
    expect_match(side_system_prompt("primary"), "Follow the specification literally")
    expect_match(side_system_prompt("validation"), "not shown to you")
    expect_match(side_system_prompt("validation"), "alternative source")
    expect_false(grepl("Trust the user-reviewed DAG", side_system_prompt("validation"), fixed = TRUE))
    expect_false(grepl("DUALDAG_MISSING_COLUMN", side_system_prompt("validation"), fixed = TRUE))
    expect_match(side_system_prompt("primary"), "Build from lineage variables first", fixed = TRUE)
    for (side in c("primary", "validation")) {
      prompt <- side_system_prompt(side)
      out_name <- if (side=="primary") "LINEAGE_DATASET" else "DATASET_TEMP"
      expect_match(prompt, paste0('cat("rows:", nrow(',out_name,'), "\\n")'), fixed = TRUE)
      expect_match(prompt, paste0("print(summary(",out_name,"$X))"), fixed = TRUE)
      expect_match(prompt, paste0('print(table(',out_name,'$X, useNA = "ifany"))'), fixed = TRUE)
      expect_match(prompt, "more than 20 distinct values", fixed = TRUE)
    }

    code_p <- generate_side(list(), "primary", spec, g, "ADSL.TRTSDT", character())
    code_v <- generate_side(list(), "validation", spec, g, "ADSL.TRTSDT", character())
    expect_match(code_p, 'cat("rows:", nrow(LINEAGE_ADSL), "\\n")', fixed = TRUE)
    expect_match(code_p, 'cat("inherited rows:", nrow(LINEAGE_ADSL), "\\n")', fixed = TRUE)
    expect_match(code_p, 'missing <- setdiff(expected, names(LINEAGE_ADSL))', fixed = TRUE)
    expect_match(code_p, 'print(table(LINEAGE_ADSL$TRTSDT, useNA = "ifany"))', fixed = TRUE)
    expect_match(code_v, 'print(table(result$TRTSDT, useNA = "ifany"))', fixed = TRUE)
    expect_false(isTRUE(attr(code_p, "same_source")))
    expect_true(isTRUE(attr(code_v, "same_source")))
    expect_false(grepl(code_p, user_v, fixed = TRUE))
    ran <- run_side(proj, "ADSL", "ADSL.TRTSDT", "primary", code_p, timeout = 60, graph = g, spec = spec)
    expect_true(ran$ok, info = ran$error %||% "")
    expect_equal(readRDS(ran$out_path)$TRTSDT, c("2020-01-01", "2020-01-02", "2020-01-03"))
    expect_match(ran$log, "rows: 3", fixed = TRUE)
    expect_match(ran$log, "inherited rows: 0", fixed = TRUE)
    expect_match(ran$log, "inherited columns: USUBJID", fixed = TRUE)
    expect_match(ran$log, "2020-01-01", fixed = TRUE)
    expect_match(read_log_file(file.path(dirname(ran$out_path), "log.txt")), "rows: 3", fixed = TRUE)

    alt <- spec_of(
      V("ADSL", "USUBJID", key = 1, source = "DM.USUBJID", row = 2),
      V("ADSL", "TRTSDT", source = "DM.RFSTDTC", derivation = "alternative EC.ECSTDTC", row = 3)
    )
    code_alt_p <- generate_side(list(), "primary", alt, g, "ADSL.TRTSDT", character())
    code_alt_v <- generate_side(list(), "validation", alt, g, "ADSL.TRTSDT", character())
    expect_match(code_alt_p, 'src[["DM"]]', fixed = TRUE)
    expect_match(code_alt_v, 'src[["EC"]]', fixed = TRUE)
    expect_false(isTRUE(attr(code_alt_v, "same_source")))

    write_result(proj, "ADSL", "ADSL.USUBJID", "primary",
                 data.frame(USUBJID = c("S1", "S2"), stringsAsFactors = FALSE))
    write_result(proj, "ADSL", "ADSL.AVAL", "primary",
                 data.frame(USUBJID = c("S1", "S2"), AVAL = c(5, 9)))
    write_result(proj, "ADSL", "ADSL.BASE", "primary",
                 data.frame(USUBJID = c("S2", "S1"), BASE = c(4, 1)))
    dir.create(file.path(proj, ".dualdag", "state"), recursive = TRUE, showWarnings = FALSE)
    l3 <- list(
      ADSL.USUBJID = list(level = "L3"),
      ADSL.AVAL = list(level = "L3"),
      ADSL.BASE = list(level = "L3")
    )
    writeLines(as.character(jsonlite::toJSON(list(version = 1L, nodes = list(ADSL = l3)), auto_unbox = TRUE)),
               file.path(proj, ".dualdag", "state", "state.json"))
    code_chg <- generate_side(list(), "primary", spec, g, "ADSL.CHG", c("AVAL", "BASE"))
    expect_match(code_chg, "AVAL")
    expect_match(code_chg, "print(summary(LINEAGE_ADSL$CHG))", fixed = TRUE)
    ran_chg <- run_side(proj, "ADSL", "ADSL.CHG", "primary", code_chg, timeout = 60, graph = g, spec = spec)
    expect_true(ran_chg$ok)
    expect_match(ran_chg$log, "rows: 2", fixed = TRUE)
    expect_match(ran_chg$log, "Min.", fixed = TRUE)
    chg <- readRDS(ran_chg$out_path)
    chg <- chg[order(chg$USUBJID), ]
    expect_equal(chg$CHG, c(4, 5))
  })
})

test_that("expected inherited columns combine same-side ancestors and same-dataset Source for layers", {
  spec <- spec_of(
    V("ADVS", "HEIGHT", type = "Num"),
    V("ADVS", "WEIGHT", type = "Num"),
    V("ADVS", "BMI", type = "Num", source = "ADVS.WEIGHT")
  )
  graph <- list(dataset = "ADVS", layered = TRUE, nodes = list(
    list(id = "ADVS.HEIGHT@HEIGHT", var = "HEIGHT", paramcd = "HEIGHT", kind = "param_variable"),
    list(id = "ADVS.RECORDS@BMI", var = "RECORDS", paramcd = "BMI", kind = "records"),
    list(id = "ADVS.BMI@BMI", var = "BMI", paramcd = "BMI", kind = "param_variable")
  ), edges = list(
    list(from = "ADVS.HEIGHT@HEIGHT", to = "ADVS.BMI@BMI"),
    list(from = "ADVS.RECORDS@BMI", to = "ADVS.BMI@BMI")
  ))
  expected <- expected_in_temp_columns(spec, graph, "ADVS.BMI@BMI", "ADVS")
  expect_identical(expected, c("HEIGHT", "WEIGHT"))
  prompt <- side_user_prompt("validation", spec, graph, "ADVS.BMI@BMI", character())
  expect_match(prompt, "EXPECTED_IN_TEMP: HEIGHT, WEIGHT", fixed = TRUE)
  expect_match(prompt, 'expected <- c("HEIGHT", "WEIGHT")', fixed = TRUE)
  expect_match(prompt, "missing expected columns:", fixed = TRUE)
})

test_that("carry-over prompt and side dataset name describe one group with every member row", {
  spec <- spec_of(V("ADVS","USUBJID",key=1),V("ADVS","PARAMCD",key=2),V("ADVS","AVAL",type="Num"),
    params=list(P("ADVS","WEIGHT","Weight","VS.VSTESTCD","Keep weight rows."),
      P("ADVS","HEIGHT","Height","VS.VSTESTCD","Keep height rows.")))
  graph <- graph_skeleton(spec,"ADVS",carry_members=c("WEIGHT","HEIGHT"))
  node <- graph_node(graph,"ADVS.AVAL@CARRY_OVER")
  expect_identical(side_program_name("ADVS",node,"primary"),"ADVS_CARRY_OVER_TEMP")
  expect_identical(side_program_name("ADVS",node,"validation"),"ADVS_CARRY_OVER_TEMP")
  prompt <- side_user_prompt("validation",spec,graph,node,character())
  expect_match(prompt,"TEMP_DATASET: ADVS_CARRY_OVER_TEMP",fixed=TRUE)
  expect_match(prompt,"PARAMCD: WEIGHT",fixed=TRUE)
  expect_match(prompt,"PARAMCD: HEIGHT",fixed=TRUE)
  expect_match(prompt,"across the whole carry-over group",fixed=TRUE)
  records_prompt <- side_user_prompt("validation",spec,graph,graph_node(graph,"ADVS.RECORDS@CARRY_OVER"),character())
  expect_match(records_prompt,"allowed whole-group member values: WEIGHT, HEIGHT",fixed=TRUE)
  expect_match(records_prompt,"Never use the synthetic group code CARRY_OVER as a PARAMCD value",fixed=TRUE)
  expect_false(grepl("Include PARAMCD equal to that code",records_prompt,fixed=TRUE))
  review_project <- tempfile("carry-review-prompt-")
  dir.create(review_project)
  had_read_side_notes <- exists("read_side_notes", mode="function", envir=.GlobalEnv, inherits=FALSE)
  if (had_read_side_notes) old_read_side_notes <- get("read_side_notes", envir=.GlobalEnv)
  assign("read_side_notes",function(...)list(),envir=.GlobalEnv)
  on.exit(if(had_read_side_notes)assign("read_side_notes",old_read_side_notes,envir=.GlobalEnv)else rm("read_side_notes",envir=.GlobalEnv),add=TRUE)
  records <- graph_node(graph,"ADVS.RECORDS@CARRY_OVER")
  dir.create(side_run_dir(review_project,"ADVS",records,"validation"),recursive=TRUE)
  write_text("result <- data.frame()",file.path(side_run_dir(review_project,"ADVS",records,"validation"),"code.R"))
  review_prompt <- self_review_prompt(review_project,"ADVS",records,"validation",spec,graph)
  expect_match(review_prompt,"CARRY_OVER_MEMBERS: WEIGHT, HEIGHT",fixed=TRUE)
  expect_match(review_prompt,"must never use the synthetic group code CARRY_OVER",fixed=TRUE)
})

test_that("fake model creates one Records and one shared variable program for the group", {
  spec <- spec_of(V("ADVS","USUBJID",key=1,source="VS.USUBJID"),
    V("ADVS","PARAMCD",key=2,source="VS.VSTESTCD"),
    V("ADVS","AVAL",type="Num",source="VS.VSSTRESN"),
    params=list(P("ADVS","WEIGHT","Weight","VS.VSTESTCD","Keep WEIGHT rows."),
      P("ADVS","HEIGHT","Height","VS.VSTESTCD","Keep HEIGHT rows.")))
  graph <- graph_skeleton(spec,"ADVS",carry_members=c("WEIGHT","HEIGHT"))
  records <- graph_node(graph,"ADVS.RECORDS@CARRY_OVER")
  records_prompt <- side_user_prompt("validation",spec,graph,records,character())
  records_reply <- jsonlite::fromJSON(fake_generate_reply("TASK: generate_validation",records_prompt),simplifyVector=FALSE)
  expect_no_error(parse(text=records_reply$code))
  env <- new.env(parent=globalenv())
  env$keys <- c("USUBJID","PARAMCD")
  env$src <- list(VS=data.frame(USUBJID=c("S1","S1","S2"),VSTESTCD=c("WEIGHT","HEIGHT","WEIGHT"),VSSTRESN=c(70,170,80)))
  expect_match(records_reply$code,"ADVS_CARRY_OVER_TEMP",fixed=TRUE)
  eval(parse(text=records_reply$code),env)
  expect_setequal(as.character(env$ADVS_CARRY_OVER_TEMP$PARAMCD),c("WEIGHT","HEIGHT"))
  expect_equal(nrow(env$ADVS_CARRY_OVER_TEMP),3L)
  node <- graph_node(graph,"ADVS.AVAL@CARRY_OVER")
  variable_prompt <- side_user_prompt("validation",spec,graph,node,character())
  variable_reply <- jsonlite::fromJSON(fake_generate_reply("TASK: generate_validation",variable_prompt),simplifyVector=FALSE)
  expect_match(variable_prompt,"TEMP_DATASET: ADVS_CARRY_OVER_TEMP",fixed=TRUE)
  run_env <- new.env(parent=globalenv())
  run_env$keys <- c("USUBJID","PARAMCD")
  run_env$work <- env$ADVS_CARRY_OVER_TEMP
  run_env$ADVS_CARRY_OVER_TEMP <- env$ADVS_CARRY_OVER_TEMP
  run_env$src <- env$src
  eval(parse(text=variable_reply$code),run_env)
  expect_equal(run_env$ADVS_CARRY_OVER_TEMP$AVAL,c(70,80,170))
})

test_that("fake check output uses a one-line limit for more than 20 distinct values", {
  spec <- spec_of(V("ADSL", "USUBJID", key = 1, source = "DM.USUBJID"))
  graph <- make_graph("ADSL", list(gn("ADSL.USUBJID")))
  code <- with_fake_llm(generate_side(list(), "primary", spec, graph, "ADSL.USUBJID", character()))
  expect_false(grepl("inherited rows:", code, fixed = TRUE))
  expect_false(grepl("EXPECTED_IN_TEMP", side_user_prompt("primary", spec, graph, "ADSL.USUBJID", character()), fixed = TRUE))
  log_path <- tempfile("check-output-log-")
  out_path <- tempfile("check-output-result-")
  ids <- sprintf("S%02d", seq_len(21L))
  ran <- side_worker(code, list(DM = data.frame(USUBJID = ids)), NULL, "USUBJID",
                     log_path, out_path, tempdir(), packages = character())
  expect_true(ran$ok)
  log <- read_log_file(log_path)
  expect_match(log, "rows: 21", fixed = TRUE)
  expect_match(log, "USUBJID: more than 20 distinct values", fixed = TRUE)
  expect_false(grepl("S01", log, fixed = TRUE))
})

test_that("side worker saves either output binding and drops extra columns", {
  keys <- c("USUBJID")
  upstream <- data.frame(USUBJID=c("S1","S2"), BASE=c(2,4))
  run_case <- function(code, suffix) {
    log_path <- tempfile(paste0("worker-",suffix,"-log-"))
    out_path <- tempfile(paste0("worker-",suffix,"-out-"))
    ran <- side_worker(code,list(),upstream,keys,log_path,out_path,tempdir(),packages=character(),
      temp_name="ADVS_WEIGHT_TEMP",output_cols=c("USUBJID","WEIGHT"))
    expect_true(ran$ok,info=ran$error %||% "")
    readRDS(out_path)
  }
  by_temp <- run_case('ADVS_WEIGHT_TEMP$WEIGHT <- c(5, 7); ADVS_WEIGHT_TEMP$EXTRA <- "drop"',"temp")
  expect_identical(names(by_temp),c("USUBJID","WEIGHT"))
  expect_equal(by_temp$WEIGHT,c(5,7))
  by_result <- run_case('result <- data.frame(USUBJID=work$USUBJID, WEIGHT=c(5,7), EXTRA="drop")',"result")
  expect_identical(names(by_result),c("USUBJID","WEIGHT"))
  expect_equal(by_result$WEIGHT,c(5,7))
})

test_that("a BMI program can read an earlier ADVS_WEIGHT_TEMP layer", {
  log_path <- tempfile("bmi-layer-log-"); out_path <- tempfile("bmi-layer-out-")
  weight <- data.frame(USUBJID=c("S1","S2"),PARAMCD="WEIGHT",WEIGHT=c(50,80))
  code <- 'result <- data.frame(USUBJID=ADVS_WEIGHT_TEMP$USUBJID, BMI=ADVS_WEIGHT_TEMP$WEIGHT / c(1.6, 2)^2)'
  ran <- side_worker(code,list(),data.frame(USUBJID=c("S1","S2")),"USUBJID",log_path,out_path,tempdir(),
    packages=character(),layers=list(WEIGHT=weight),temp_name="ADVS_BMI_TEMP",dataset_name="ADVS",
    output_cols=c("USUBJID","BMI"))
  expect_true(ran$ok,info=ran$error %||% "")
  out<-readRDS(out_path)
  expect_identical(names(out),c("USUBJID","BMI"))
  expect_equal(out$BMI,c(50/1.6^2,80/2^2))
})

test_that("one side's instruction never enters the other side's prompt", {
  proj <- copy_project()
  with_fake_llm({
    spec <- spec_of(
      V("ADSL", "USUBJID", type = "Char", length = 10, key = 1, source = "DM.USUBJID", row = 2),
      V("ADSL", "TRTSDT", label = "Start", type = "Char", length = 20, source = "DM.RFSTDTC",
        derivation = "DM.RFSTDTC", row = 3)
    )
    g <- extract_dependencies(list(), spec, "ADSL")
    code_p <- generate_side(list(), "primary", spec, g, "ADSL.TRTSDT", character())
    primary_note <- "PRIMARY_NOTE copy RFSTDTC exactly"
    validation_note <- "VALIDATION_NOTE use the earliest dose date"
    prompt_p <- side_user_prompt(
      "primary", spec, g, "ADSL.TRTSDT", character(), user_instruction = primary_note
    )
    prompt_v <- side_user_prompt(
      "validation", spec, g, "ADSL.TRTSDT", character(), user_instruction = validation_note
    )
    expect_match(prompt_p, primary_note, fixed = TRUE)
    expect_false(grepl(validation_note, prompt_p, fixed = TRUE))
    expect_match(prompt_v, validation_note, fixed = TRUE)
    expect_false(grepl(primary_note, prompt_v, fixed = TRUE))
    expect_false(grepl(code_p, prompt_v, fixed = TRUE))
    expect_false(grepl("2020-01-01", prompt_v, fixed = TRUE))
  })
})

test_that("the dataset spec is a shared system-prompt prefix and the target stays in the user prompt", {
  with_fake_llm({
    spec <- spec_of(
      V("ADSL", "USUBJID", label = "Subject", type = "Char", length = 20, key = 1,
        source = "DM.USUBJID", row = 2),
      V("ADSL", "TRTSDT", label = "Start", type = "Char", length = 20,
        source = "DM.RFSTDTC", derivation = "DM.RFSTDTC", row = 3),
      V("ADSL", "AVAL", label = "Analysis Value", type = "Num", length = 8, row = 4),
      V("ADSL", "BASE", label = "Baseline", type = "Num", length = 8, row = 5),
      V("ADSL", "CHG", label = "Change", type = "Num", length = 8, derivation = "AVAL - BASE", row = 6),
      params = list(
        P("ADSL", "WEIGHT", "Weight", source = "VS.VSSTRESN", derivation = "VSTESTCD = WEIGHT", row = 2),
        P("ADAE", "SER", "Serious", source = "AE.AESER", row = 3)
      )
    )
    g <- extract_dependencies(list(), spec, "ADSL")
    prefix_of <- function(text) sub("\nTASK:.*", "", text)
    sys_trt <- side_system_prompt("primary", spec, g$dataset)
    sys_chg <- side_system_prompt("validation", spec, g$dataset)
    expect_match(sys_trt, "You are the primary-side R expert for dataset ADSL", fixed = TRUE)
    expect_match(sys_chg, "You are the validation-side R expert for dataset ADSL", fixed = TRUE)
    expect_match(sys_trt, "TASK: generate_primary", fixed = TRUE)
    expect_false(grepl("DATASET_SPEC", sys_trt, fixed = TRUE))
    expect_false(grepl("AVAL - BASE", sys_trt, fixed = TRUE))
    expect_false(grepl("AE.AESER", sys_trt, fixed = TRUE))
    qc <- qc_system_prompt(spec, "ADSL")
    expect_match(qc, "^DATASET_SPEC\n")
    expect_identical(prefix_of(qc), prefix_of(qc_system_prompt(spec, "ADSL")))
    for (var in c("USUBJID", "TRTSDT", "AVAL", "BASE", "CHG", "WEIGHT", "VS.VSSTRESN")) {
      expect_match(prefix_of(qc), var, fixed = TRUE)
    }
    expect_false(grepl("AE.AESER", qc, fixed = TRUE))
    expect_false(grepl("SER", prefix_of(qc), fixed = TRUE))

    user <- side_user_prompt(
      "primary", spec, g, "ADSL.TRTSDT", character(),
      qc_feedback = "three rows differ", user_instruction = "copy the date"
    )
    expect_false(grepl("DATASET_SPEC", user, fixed = TRUE))
    expect_false(grepl("AVAL - BASE", user, fixed = TRUE))
    expect_false(grepl("VAR_INDEX_BEGIN", user, fixed = TRUE))
    expect_equal(prompt_field(user, "VAR"), "TRTSDT")
    expect_equal(prompt_field(user, "SOURCE"), "DM.RFSTDTC")
    expect_equal(prompt_field(user, "DERIVATION"), "DM.RFSTDTC")
    expect_false(grepl("three rows differ", user, fixed = TRUE))
    expect_match(user, "copy the date", fixed = TRUE)

    code <- generate_side(
      list(), "primary", spec, g, "ADSL.TRTSDT", character(),
      qc_feedback = "three rows differ", user_instruction = "copy the date"
    )
    expect_match(code, 'src[["DM"]]', fixed = TRUE)
    expect_match(code, "RFSTDTC", fixed = TRUE)
    expect_false(grepl("VSSTRESN", code, fixed = TRUE))

    code_v <- generate_side(
      list(), "validation", spec, g, "ADSL.CHG", c("AVAL", "BASE"),
      user_instruction = "VALIDATION_NOTE use the change"
    )
    expect_match(code_v, "AVAL", fixed = TRUE)
    expect_false(grepl(code, code_v, fixed = TRUE))
    prompt_v <- side_user_prompt(
      "validation", spec, g, "ADSL.CHG", c("AVAL", "BASE"),
      user_instruction = "VALIDATION_NOTE use the change"
    )
    expect_false(grepl("copy the date", prompt_v, fixed = TRUE))
    expect_false(grepl("DATASET_SPEC", prompt_v, fixed = TRUE))
    expect_equal(prompt_field(prompt_v, "VAR"), "CHG")
    expect_equal(prompt_field(prompt_v, "DERIVATION"), "AVAL - BASE")
  })
})

test_that("side code can call functions from the default packages such as stats::setNames", {
  proj <- copy_project()
  with_fake_llm({
    spec <- spec_of(
      V("ADSL", "USUBJID", type = "Char", length = 10, key = 1, source = "DM.USUBJID", row = 2),
      V("ADSL", "TRTSDT", type = "Char", length = 20, source = "DM.RFSTDTC", derivation = "DM.RFSTDTC", row = 3)
    )
    g <- extract_dependencies(list(), spec, "ADSL")
    code <- paste(
      "dm <- src$DM",
      "v <- setNames(as.character(dm$RFSTDTC), dm$USUBJID)",
      "result <- data.frame(USUBJID = dm$USUBJID, TRTSDT = unname(v[dm$USUBJID]), stringsAsFactors = FALSE)",
      sep = "\n")
    ran <- run_side(proj, "ADSL", "ADSL.TRTSDT", "primary", code, timeout = 60, graph = g, spec = spec)
    expect_true(ran$ok)
    expect_null(ran$error)
  })
})


test_that("side prompt prints its upstream variable label once", {
  proj <- temp_project()
  spec <- spec_of(V("ADSL","USUBJID",key=1),V("ADSL","AGE",type="Num"))
  graph <- make_graph("ADSL",list(gn("ADSL.USUBJID"),gn("ADSL.AGE")),
    list(ge("ADSL.USUBJID","ADSL.AGE")))
  prompt <- side_user_prompt("primary",spec,graph,gn("ADSL.AGE"),"USUBJID",project=proj)
  expect_equal(length(regmatches(prompt,gregexpr("(?m)^LINEAGE_INPUT_VARS:",prompt,perl=TRUE))[[1]]),1L)
  prompt_validation <- side_user_prompt("validation",spec,graph,gn("ADSL.AGE"),"USUBJID",project=proj)
  expect_equal(length(regmatches(prompt_validation,gregexpr("(?m)^UPSTREAM_VARS:",prompt_validation,perl=TRUE))[[1]]),1L)
})
