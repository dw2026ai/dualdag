source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))
if (!exists("read_project_spec", mode="function")) {
  source(file.path(testthat::test_path(),"..","..","R","project.R"),local=FALSE)
  source(file.path(testthat::test_path(),"..","..","R","spec.R"),local=FALSE)
}

test_that("compare match becomes L3 and a mismatch stays a compare problem with diff rows", {
  proj <- temp_project()
  keys <- "USUBJID"
  g <- make_graph("ADSL", list(gn("ADSL.AVAL"), gn("ADSL.CHG")), list(ge("ADSL.AVAL", "ADSL.CHG")))
  write_result(proj, "ADSL", "ADSL.AVAL", "primary",
               data.frame(USUBJID = c("S1", "S2"), AVAL = c(1, 2)))
  write_result(proj, "ADSL", "ADSL.AVAL", "validation",
               data.frame(USUBJID = c("S2", "S1"), AVAL = c(2, 1)))
  good <- compare_node(proj, "ADSL", "ADSL.AVAL", keys)
  expect_true(good$match)
  expect_equal(good$n_diff, 0)
  expect_equal(good$n_primary, 2)
  expect_equal(good$summary, "Match")

  statuses <- init_statuses(g)
  statuses$ADSL.AVAL$level <- "L2"
  statuses$ADSL.CHG$level <- "L3"
  statuses$ADSL.CHG$sides$primary$code <- "stale"
  statuses <- advance_compare(statuses, g, "ADSL.AVAL", good$match, good$summary)
  expect_equal(statuses$ADSL.AVAL$level, "L3")
  expect_null(statuses$ADSL.AVAL$problem)
  expect_equal(statuses$ADSL.CHG$level, "L3")

  write_result(proj, "ADSL", "ADSL.AVAL", "validation",
               data.frame(USUBJID = c("S1", "S2"), AVAL = c(1, 9)))
  bad <- compare_node(proj, "ADSL", "ADSL.AVAL", keys)
  expect_false(bad$match)
  expect_equal(bad$n_diff, 1)
  expect_true(all(c("USUBJID", "primary", "validation") %in% names(bad$diffs)))
  row <- bad$diffs[bad$diffs$USUBJID == "S2", ]
  expect_equal(row$primary, 2)
  expect_equal(row$validation, 9)

  statuses$ADSL.AVAL$level <- "L3"
  statuses <- advance_compare(statuses, g, "ADSL.AVAL", bad$match, bad$summary)
  expect_equal(statuses$ADSL.AVAL$level, "L2")
  expect_equal(statuses$ADSL.AVAL$problem$stage, "compare")
  expect_match(statuses$ADSL.AVAL$problem$message, "differing")
  expect_equal(statuses$ADSL.CHG$level, "L3")
  expect_equal(statuses$ADSL.CHG$sides$primary$code, "stale")
  expect_true("ADSL.AVAL" %in% statuses$ADSL.CHG$upstream_changed)
})

test_that("carry-over comparison retains PARAMCD so a mismatch names its member", {
  proj <- temp_project(); dir.create(file.path(proj,"spec"),recursive=TRUE)
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADVS,USUBJID,Subject,Char,20,1,,","ADVS,PARAMCD,Parameter,Char,8,2,,",
    "ADVS,AVAL,Value,Num,8,,,"),file.path(proj,"spec","variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation","ADVS,WEIGHT,Weight,,",
    "ADVS,HEIGHT,Height,,","ADVS,BMI,BMI,,"),file.path(proj,"spec","parameter.csv"))
  spec <- read_project_spec(proj)
  graph <- graph_skeleton(spec,"ADVS",carry_members=c("WEIGHT","HEIGHT"))
  save_graph(proj,graph)
  id <- "ADVS.AVAL@CARRY_OVER"
  write_result(proj,"ADVS",id,"primary",data.frame(USUBJID=c("S1","S1"),PARAMCD=c("WEIGHT","HEIGHT"),AVAL=c(70,170)))
  write_result(proj,"ADVS",id,"validation",data.frame(USUBJID=c("S1","S1"),PARAMCD=c("WEIGHT","HEIGHT"),AVAL=c(71,170)))
  result <- compare_node(proj,"ADVS",id,c("USUBJID","PARAMCD"))
  expect_false(result$match)
  expect_true("PARAMCD" %in% names(result$diffs))
  expect_identical(as.character(result$diffs$PARAMCD[[1]]),"WEIGHT")
})

test_that("an empty result is not a match and key types are coerced before diffdf", {
  proj <- temp_project()
  keys <- "USUBJID"
  write_result(proj, "ADSL", "ADSL.AVAL", "primary",
               data.frame(USUBJID = character(), AVAL = numeric()))
  write_result(proj, "ADSL", "ADSL.AVAL", "validation",
               data.frame(USUBJID = c("S1", "S2"), AVAL = c(1, 2)))
  empty <- compare_node(proj, "ADSL", "ADSL.AVAL", keys)
  expect_false(empty$match)
  expect_match(empty$summary, "primary result has 0 rows", fixed = TRUE)
  expect_false(grepl("KEYS have different modes", empty$summary, fixed = TRUE))

  write_result(proj, "ADSL", "ADSL.AVAL", "primary",
               data.frame(USUBJID = c("S1", "S2"), AVAL = c(1, 2), stringsAsFactors = FALSE))
  write_result(proj, "ADSL", "ADSL.AVAL", "validation",
               data.frame(USUBJID = c(TRUE, FALSE), AVAL = c(1, 2)))
  typed <- compare_node(proj, "ADSL", "ADSL.AVAL", keys)
  expect_false(typed$match)
  expect_match(typed$summary, "key USUBJID types differ: character vs logical", fixed = TRUE)
  expect_false(grepl("KEYS have different modes", typed$summary, fixed = TRUE))
})
