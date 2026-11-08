source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))

test_that("QC report text stays out of both generation prompts", {
  spec <- spec_of(V("ADSL", "USUBJID", key = 1L, source = "DM.USUBJID"),
                  V("ADSL", "AGE", source = "DM.AGE"))
  graph <- make_graph("ADSL", list(gn("ADSL.USUBJID"), gn("ADSL.AGE")))
  for (side in c("primary", "validation")) {
    prompt <- side_user_prompt(side, spec, graph, "ADSL.AGE", character(),
                               qc_feedback = "QC_SECRET_ROW_FROM_BOTH_SIDES")
    expect_false(grepl("QC_SECRET_ROW_FROM_BOTH_SIDES", prompt, fixed = TRUE))
    expect_false(grepl("QC_FEEDBACK", prompt, fixed = TRUE))
  }
})

test_that("QC documents live under DUALDAG_HOME and replace is explicit", {
  with_qc_home({
    expect_equal(qc_list(), list())
    expect_null(qc_get("ADSL"))
    first <- qc_save("adsl", "hello methodology", replace = FALSE)
    expect_true(first$ok)
    expect_equal(qc_get("ADSL"), "hello methodology")
    blocked <- qc_save("ADSL", "new text", replace = FALSE)
    expect_false(blocked$ok)
    expect_true(blocked$exists)
    expect_equal(qc_get("adsl"), "hello methodology")
    replaced <- qc_save("adsl", "replaced doc", replace = TRUE)
    expect_true(replaced$ok)
    expect_equal(qc_get("ADSL"), "replaced doc")
    listed <- qc_list()
    expect_equal(listed[[1]]$dataset, "ADSL")
    expect_gt(listed[[1]]$size, 0)
    expect_true(nzchar(listed[[1]]$updated))
    expect_error(qc_save("../etc", "x"), "Invalid dataset")
  })
})

test_that("QC investigation quotes the dataset document and does not auto-fix", {
  with_qc_home({
    with_fake_llm({
      qc_save("ADSL", "LOOK FOR THE SUBJECT FIRST", replace = TRUE)
      diffs <- data.frame(USUBJID = "S1", primary = 1, validation = 2, stringsAsFactors = FALSE)
      report <- qc_investigate(list(), "ADSL", "ADSL.AGE", diffs, "AGE = DM.AGE")
      expect_match(report, "LOOK FOR THE SUBJECT FIRST", fixed = TRUE)
      expect_match(report, "user decides", ignore.case = TRUE)
      expect_match(report, "No auto-fix")
      expect_match(qc_system_prompt(), "The user decides")
      expect_match(qc_system_prompt(), "Do not auto-fix")
      spec <- spec_of(
        V("ADSL", "USUBJID", key = 1, source = "DM.USUBJID", row = 2),
        V("ADSL", "AGE", source = "DM.AGE", derivation = "copy AGE", row = 3)
      )
      sys <- qc_system_prompt(spec, "ADSL")
      expect_match(sys, "^DATASET_SPEC\n")
      expect_match(sys, "USUBJID", fixed = TRUE)
      expect_match(sys, "copy AGE", fixed = TRUE)
      expect_match(sys, "TASK: qc_investigate", fixed = TRUE)
      report2 <- qc_investigate(list(), "ADSL", "ADSL.AGE", diffs, "AGE = DM.AGE", spec = spec)
      expect_match(report2, "LOOK FOR THE SUBJECT FIRST", fixed = TRUE)
      expect_false(grepl("DATASET_SPEC", qc_user_prompt("ADSL", "ADSL.AGE", diffs, "AGE = DM.AGE", qc_get("ADSL")), fixed = TRUE))
      prompt <- qc_user_prompt("ADSL", "ADSL.AGE", diffs, "AGE = DM.AGE", qc_get("ADSL"))
      expect_match(prompt, "LOOK FOR THE SUBJECT FIRST", fixed = TRUE)
      expect_false(grepl("fixed code", prompt, ignore.case = TRUE))
    })
  })
})
