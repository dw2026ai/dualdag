source(testthat::test_path("..", "fixtures", "helper.R"))
source(testthat::test_path("..", "..", "R", "inspect.R"))
source(testthat::test_path("..", "..", "R", "server.R"))

test_that("missing-column errors and stray source reads use current wording", {
  expect_equal(missing_dependency_message("primary", "DUALDAG_MISSING_DEPENDENCY: ADSL.X, DM.Y"),
               "Missing column: X is not in LINEAGE_ADSL or the provided data.")
  stored_old_token <- list(problem=list(stage="run",message="DUALDAG_MISSING_DEPENDENCY: ADSL.X"),
                           sides=list(primary=list(error="DUALDAG_MISSING_DEPENDENCY: ADSL.X")))
  displayed_old_token <- normalize_missing_column_status(stored_old_token)
  expect_identical(displayed_old_token$problem$message,
                   "Missing column: X is not in LINEAGE_ADSL or the provided data.")
  expect_identical(displayed_old_token$sides$primary$error,
                   "Missing column: X is not in LINEAGE_ADSL or the provided data.")
  expect_equal(missing_dependency_message("primary", "DUALDAG_MISSING_COLUMN: Missing column: X is not in LINEAGE_ADSL or the provided data."),
               "Missing column: X is not in LINEAGE_ADSL or the provided data.")
  allowed <- list(ids = c("ADSL.TARGET", "ADSL.MID"), source_refs = "DM.SEX")
  code <- 'result <- data.frame(USUBJID = work$USUBJID, TARGET = src[["DM"]][["AGE"]])'
  expect_length(lineage_code_warnings(code, "validation", "ADSL", "ADSL.TARGET", allowed,
                                     list(DM = c("USUBJID", "SEX", "AGE")), "USUBJID"), 0L)
  legacy <- list(problem=list(stage="run",message="primary says the DAG is missing a dependency: ADSL.X. Review the graph."),
                  sides=list(primary=list(error="primary says the DAG is missing a dependency: ADSL.X. Review the graph.")))
  normalized <- normalize_missing_column_status(legacy)
  expect_identical(normalized$problem$message,"Missing column: X is not in LINEAGE_ADSL or the provided data.")
  expect_identical(normalized$sides$primary$error,"Missing column: X is not in LINEAGE_ADSL or the provided data.")
})

test_that("distinct levels skip noncategorical values and bound output", {
  expect_null(distinct_levels(1:4))
  expect_null(distinct_levels(as.Date("2020-01-01") + 0:2))
  expect_null(distinct_levels(as.POSIXct("2020-01-01", tz = "UTC")))
  expect_equal(levels_line("DM.SEX", distinct_levels(factor(c("M", "F", "M", NA)))),
               "DM.SEX: F (1), M (2), NA (1)")
  expect_equal(levels_line("DM.FLAG", distinct_levels(c(TRUE, FALSE, TRUE))),
               "DM.FLAG: FALSE (1), TRUE (2)")
  expect_equal(nchar(distinct_levels(strrep("x", 70))$value), 60L)
  expect_equal(levels_line("DM.X", distinct_levels(sprintf("v%02d", 1:21))),
               "DM.X: more than 20 distinct values, not listed.")
  path <- tempfile(fileext = ".rds")
  saveRDS(data.frame(X = c("A", "A")), path)
  expect_equal(file_distinct_levels(path, "X")$n, 2L)
  Sys.sleep(1.1)
  saveRDS(data.frame(X = c("A", "B")), path)
  expect_equal(file_distinct_levels(path, "X")$n, c(1L, 1L))
})

test_that("lineage samples nonmissing values across the whole column reproducibly", {
  dates <- c(rep(as.Date(NA), 100L), as.Date("2014-01-01") + 0:39)
  set.seed(91)
  before <- .Random.seed
  first <- lineage_column_line("ADSL.TRTSDT", dates, "ADSL")
  expect_identical(.Random.seed, before)
  expect_identical(first, lineage_column_line("ADSL.TRTSDT", dates, "ADSL"))
  expect_match(first, "ADSL.TRTSDT <Date> 20 sampled:", fixed = TRUE)
  expect_match(first, "(100 NA of 140)", fixed = TRUE)
  expect_match(first, "2014-", fixed = TRUE)
  sampled <- sub("^.*20 sampled: ", "", first)
  sampled <- sub(" \\(100 NA of 140\\)$", "", sampled)
  values <- strsplit(sampled, ", ", fixed = TRUE)[[1L]]
  expect_length(values, 20L)
  expect_identical(values, sort(values))
  expect_false(any(values == "NA"))
  expect_match(lineage_column_line("ADSL.X", as.Date(rep(NA, 3)), "ADSL"),
               "all NA (3 NA of 3)", fixed = TRUE)
})

test_that("lineage profile reports actual R classes and hides key values", {
  time <- as.POSIXct(c("2014-01-02 08:30", NA), tz = "UTC")
  expect_match(lineage_column_line("VS.DTM", time, "ADSL"),
               "VS.DTM <POSIXct> 1 sampled: 2014-01-02 08:30:00 (1 NA of 2)", fixed = TRUE)
  expect_match(lineage_column_line("VS.NUM", c(1.5, NA_real_), "ADSL"),
               "VS.NUM <numeric> 1 sampled: 1.5 (1 NA of 2)", fixed = TRUE)
  expect_match(lineage_column_line("VS.INT", c(1L, NA_integer_), "ADSL"),
               "VS.INT <integer> 1 sampled: 1 (1 NA of 2)", fixed = TRUE)
  expect_match(lineage_column_line("VS.TXT", c("A", NA_character_), "ADSL"),
               'VS.TXT <character> 1 sampled: "A" (1 NA of 2)', fixed = TRUE)
  expect_match(lineage_column_line("VS.SPACE", c(" A", "B "), "ADSL"),
               '" A", "B "', fixed = TRUE)
  expect_match(lineage_column_line("VS.FLAG", c(TRUE, NA), "ADSL"),
               "VS.FLAG <logical> 1 sampled: TRUE (1 NA of 2)", fixed = TRUE)
  expect_identical(lineage_column_line("ADSL.USUBJID", c("S1", "S2"), "ADSL", key = TRUE),
                   "ADSL.USUBJID <character>")
})

test_that("assembled generate and agent prompts include work and source profiles", {
  project <- tempfile("lineage-types-")
  dir.create(file.path(project, "data"), recursive = TRUE)
  dir.create(dirname(work_path(project, "ADSL", "validation")), recursive = TRUE)
  ids <- sprintf("S%03d", seq_len(140L))
  trtsdt <- c(rep(as.Date(NA), 100L), as.Date("2014-01-01") + 0:39)
  saveRDS(data.frame(USUBJID = ids, TRTSDT = trtsdt), work_path(project, "ADSL", "validation"))
  haven::write_xpt(data.frame(USUBJID = ids, VSDTC = c(rep(NA_character_, 100L),
                                                        format(as.Date("2014-01-01") + 0:39))),
                   file.path(project, "data", "vs.xpt"))
  spec <- spec_of(
    V("ADSL", "USUBJID", key = 1L, source = "VS.USUBJID"),
    V("ADSL", "TRTSDT", source = "VS.VSDTC"),
    V("ADSL", "WEIGHTBL", type = "Num", source = "VS.VSDTC", derivation = "Use TRTSDT")
  )
  graph <- make_graph("ADSL", list(gn("ADSL.USUBJID"), gn("ADSL.TRTSDT"), gn("ADSL.WEIGHTBL")),
                      edges = list(ge("ADSL.USUBJID", "ADSL.WEIGHTBL"), ge("ADSL.TRTSDT", "ADSL.WEIGHTBL")))
  generate <- side_user_prompt("validation", spec, graph, "ADSL.WEIGHTBL", c("USUBJID", "TRTSDT"),
                               project = project)
  agent <- agent_user_prompt("validation", spec, graph, "ADSL.WEIGHTBL", "Explain this code", project)
  for (prompt in c(generate, agent)) {
    expect_false(grepl("ADSL.TRTSDT <Date> 20 sampled:", prompt, fixed = TRUE))
    expect_false(grepl("(100 NA of 140)", prompt, fixed = TRUE))
    expect_match(prompt, "ADSL.TRTSDT | TRTSDT |", fixed = TRUE)
    expect_match(prompt, "VS.VSDTC <character> 20 sampled:", fixed = TRUE)
    expect_match(prompt, "ADSL.USUBJID | USUBJID |", fixed = TRUE)
    expect_false(grepl("ADSL.USUBJID <character>", prompt, fixed = TRUE))
    expect_match(prompt, "VS.VSDTC: more than 20 distinct values, not listed.", fixed = TRUE)
  }
})

test_that("validation prompt keeps target context, done list and all source names", {
  project <- tempfile("lineage-")
  dir.create(file.path(project, "data"), recursive = TRUE)
  dir.create(dirname(work_path(project, "ADSL", "primary")), recursive = TRUE)
  saveRDS(data.frame(USUBJID = "1", MID = "M"), work_path(project, "ADSL", "primary"))
  saveRDS(data.frame(USUBJID = "1", MID = "M"), work_path(project, "ADSL", "validation"))
  haven::write_xpt(data.frame(USUBJID = "1", SEX = "F", OTHER = "hidden"),
                   file.path(project, "data", "dm.xpt"))
  spec <- spec_of(
    V("ADSL", "USUBJID", key = 1L, source = "DM.USUBJID"),
    V("ADSL", "ROOT", derivation = "DM.SEX"),
    V("ADSL", "MID", derivation = "ADSL.ROOT"),
    V("ADSL", "TARGET", source = "DM.SEX", derivation = "ADSL.MID")
  )
  g <- make_graph("ADSL", lapply(spec$variables$Variable, function(v) gn(paste0("ADSL.", v))),
                  edges = list(ge("ADSL.ROOT", "ADSL.MID"), ge("ADSL.MID", "ADSL.TARGET")))
  mid_dir <- write_result(project,"ADSL","ADSL.MID","validation",data.frame(MID="M"))
  writeLines("result <- data.frame(MID = 'M')",file.path(mid_dir,"code.R"))
  card_write(project,"ADSL","ADSL.MID","validation","MID card",g,spec)
  catalog <- list(domains = list(DM = list(variables = list(
    list(name = "SEX", label = "Sex", type = "character"),
    list(name = "OTHER", label = "Other", type = "character")))))
  primary <- side_user_prompt("primary", spec, g, "ADSL.TARGET", "MID", project = project, catalog = catalog)
  validation <- side_user_prompt("validation", spec, g, "ADSL.TARGET", "MID", project = project, catalog = catalog)
  expect_match(primary, "ADSL.MID: M (1)", fixed = TRUE)
  expect_match(primary, "ADSL.ROOT\nROW:", fixed = TRUE)
  expect_false(grepl("ADSL.ROOT\nROW:", validation, fixed = TRUE))
  expect_match(validation, "ADSL.TARGET\nROW:", fixed = TRUE)
  expect_match(validation, "ADSL.MID | MID | Char", fixed = TRUE)
  expect_match(validation,"MID card",fixed=TRUE)
  expect_false(grepl("result <- data.frame(MID = 'M')",validation,fixed=TRUE))
  expect_false(grepl("ADSL.MID: M", validation, fixed = TRUE))
  expect_match(validation, "DM.SEX: F (1)", fixed = TRUE)
  expect_match(validation, "SOURCE_DOMAINS (names only):\nDM: SEX, OTHER", fixed = TRUE)
  expect_false(grepl("SOURCE_DOMAIN_CATALOG", validation, fixed = TRUE))
  expect_false(grepl("PRIMARY",validation,fixed=TRUE))
  expect_false(grepl("DM | OTHER", primary, fixed = TRUE))
  expect_false(grepl("VAR_INDEX_BEGIN", primary, fixed = TRUE))
  expect_false(grepl("WORK_COLUMNS", primary, fixed = TRUE))
})

test_that("validation prompt includes the target parameter row", {
  spec <- spec_of(V("ADVS","AVAL",source="VS.VSSTRESN"),
                  params=list(P("ADVS","WEIGHT","Weight",source="VS.VSTESTCD=WEIGHT",derivation="Collected weight")))
  graph <- make_graph("ADVS",list(gn("ADVS.AVAL@WEIGHT")))
  prompt <- side_user_prompt("validation",spec,graph,"ADVS.AVAL@WEIGHT",character())
  expect_match(prompt,"PARAMETER_ROW_BEGIN",fixed=TRUE)
  expect_match(prompt,"PARAMCD: WEIGHT",fixed=TRUE)
  expect_match(prompt,"SOURCE: VS.VSTESTCD=WEIGHT",fixed=TRUE)
  expect_match(prompt,"DERIVATION: Collected weight",fixed=TRUE)
})
