network_test_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
for (f in c("utils.R", "deps.R", "config.R", "llm.R", "dag.R", "spec.R", "project.R", "cards.R", "deps_extract.R", "param_graph.R"))
  sys.source(file.path(network_test_root, "R", f), envir = globalenv())

test_that("network JSON is split by parameter and contains relationships only", {
  p <- open_project(tempfile("network-json-"))
  pg <- list(dataset = "ADVS", spec_hash = "spec-hash", nodes = lapply(c("WEIGHT", "HEIGHT", "BMI"), function(code) list(code = code)),
    edges = list(list(from = "WEIGHT", to = "BMI", uses = list(list(from_var = "AVAL", to_var = "AVAL"))),
      list(from = "HEIGHT", to = "BMI", uses = list(list(from_var = "AVAL", to_var = "AVAL")))))
  param_graph_write(p, pg)
  root <- network_dir(p, "ADVS")
  expect_equal(list.files(root), "_parameters.json")
  g <- list(dataset = "ADVS", spec_hash = "spec-hash", layered = TRUE,
    nodes = list(list(id = "ADVS.AVAL@WEIGHT", var = "AVAL", paramcd = "WEIGHT"),
      list(id = "ADVS.AVAL@HEIGHT", var = "AVAL", paramcd = "HEIGHT"),
      list(id = "ADVS.AVAL@BMI", var = "AVAL", paramcd = "BMI"),
      list(id = "ADVS.ADY@BMI", var = "ADY", paramcd = "BMI"),
      list(id = "ADVS.STUDYID", var = "STUDYID", paramcd = NULL, kind = "dataset_variable")),
    edges = list(list(from = "ADVS.ADY@BMI", to = "ADVS.AVAL@BMI", evidence = "private card content"),
      list(from = "ADVS.STUDYID", to = "ADVS.ADY@BMI", evidence = "private reason")),
    external = list(list(ref = "VS.VSSTRESN", used_by = list("ADVS.AVAL@BMI"), evidence = "private code")),
    param_uses = list(
      list(from = "WEIGHT", to = "BMI", uses = list(
        list(from_var = "AVAL", to_var = "AVAL"),
        list(from_var = "ADT", to_var = "ADT"),
        list(from_var = "AVAL", to_var = "AVAL"))),
      list(from = "HEIGHT", to = "BMI", uses = list(list(from_var = "AVISIT", to_var = "AVISIT"))),
      list(from = "BMI", to = "WEIGHT", uses = list(list(from_var = "BASE", to_var = "BASE")))),
    generated_parameters = list("WEIGHT", "HEIGHT", "BMI"))
  save_graph(p, g)
  files <- sort(list.files(root))
  expect_setequal(files, c("_dataset.json", "_parameters.json", "BMI.json", "HEIGHT.json", "WEIGHT.json"))
  bmi <- read_json(file.path(root, "BMI.json"))
  expect_equal(bmi$variables, list("ADVS.AVAL@BMI", "ADVS.ADY@BMI"))
  expect_false(any(grepl("@(WEIGHT|HEIGHT)", unlist(bmi[c("variables", "edges", "inputs")]), perl = TRUE)))
  expect_true(any(vapply(bmi$inputs, function(x) identical(x$from, "VS.VSSTRESN") && identical(x$to, "ADVS.AVAL@BMI"), TRUE)))
  expect_true(any(vapply(bmi$inputs, function(x) identical(x$from, "ADVS.STUDYID") && identical(x$to, "ADVS.ADY@BMI"), TRUE)))
  param <- read_json(file.path(root, "_parameters.json"))
  weight_bmi <- Filter(function(e) identical(e$from, "WEIGHT") && identical(e$to, "BMI"), param$edges)[[1]]
  expect_equal(weight_bmi$uses, list(list(from_var = "AVAL", to_var = "AVAL")))
  height_bmi <- Filter(function(e) identical(e$from, "HEIGHT") && identical(e$to, "BMI"), param$edges)[[1]]
  expect_equal(height_bmi$uses, list(list(from_var = "AVAL", to_var = "AVAL")))
  expect_false(any(vapply(param$edges, function(e) identical(e$from, "BMI") && identical(e$to, "WEIGHT"), TRUE)))

  # A user's removal from the Parameter DAG also removes it from the export.
  pg$edges <- Filter(function(e) !(identical(e$from, "WEIGHT") && identical(e$to, "BMI")), pg$edges)
  param_graph_write(p, pg)
  param <- read_json(file.path(root, "_parameters.json"))
  expect_false(any(vapply(pg$edges, function(e) identical(e$from, "WEIGHT") && identical(e$to, "BMI"), TRUE)))
  expect_false(any(vapply(param$edges, function(e) identical(e$from, "WEIGHT") && identical(e$to, "BMI"), TRUE)))

  # Saving a regenerated parameter Variable DAG refreshes the aggregate file.
  g$param_uses[[1]]$uses <- list(list(from_var = "ADT", to_var = "AVISIT"))
  save_graph(p, g)
  param <- read_json(file.path(root, "_parameters.json"))
  expect_false(any(vapply(param$edges, function(e) identical(e$from, "WEIGHT") && identical(e$to, "BMI"), TRUE)))
  for (pc in c("WEIGHT", "HEIGHT", "BMI")) {
    item <- read_json(file.path(root, paste0(pc, ".json")))
    expect_true(all(vapply(item$variables, function(id) endsWith(id, paste0("@", pc)), TRUE)))
    expect_false(any(grepl("@(WEIGHT|HEIGHT|BMI)", unlist(item$edges), perl = TRUE) &
      !grepl(paste0("@", pc, "(?:$|\\\")"), unlist(item$edges), perl = TRUE)))
  }
  all_network <- paste(vapply(list.files(root, full.names = TRUE), read_text, ""), collapse = "\n")
  expect_false(grepl("private card content|private reason|private code|evidence|reason|approach", all_network, ignore.case = TRUE))
  before <- readBin(file.path(root, "WEIGHT.json"), "raw", n = file.info(file.path(root, "WEIGHT.json"))$size)
  g$edges[[1]]$evidence <- "changed card content"
  save_graph(p, g)
  after <- readBin(file.path(root, "WEIGHT.json"), "raw", n = file.info(file.path(root, "WEIGHT.json"))$size)
  expect_identical(after, before)
  unlink(file.path(root, "BMI.json"))
  g$generated_parameters <- list("WEIGHT", "HEIGHT")
  save_graph(p, g)
  expect_false(file.exists(file.path(root, "BMI.json")))
  reset_all_work(p)
  expect_false(dir.exists(root))
})

test_that("carry-over network JSON is one file and lists its members", {
  p <- open_project(tempfile("network-carry-json-"))
  pg <- list(dataset="ADVS", spec_hash="h", nodes=lapply(c("PULSE","TEMP","BMI"),function(code)list(code=code)),
    edges=list(list(from="PULSE",to="BMI",uses=list(list(from_var="AVAL",to_var="AVAL")))))
  param_graph_write(p,pg)
  g <- list(dataset="ADVS",spec_hash="h",layered=TRUE,
    nodes=list(list(id="ADVS.AVAL@CARRY_OVER",var="AVAL",paramcd="CARRY_OVER")),
    edges=list(),external=list(),param_uses=list(),generated_parameters=list("CARRY_OVER"),
    extraction=list(parameter_classification=list(parameters=list(
      list(paramcd="PULSE",classification="carry_over"),list(paramcd="TEMP",classification="carry_over"),
      list(paramcd="BMI",classification="derived")))))
  save_graph(p,g)
  root <- network_dir(p,"ADVS")
  expect_true(file.exists(file.path(root,"CARRY_OVER.json")))
  expect_false(any(file.exists(file.path(root,c("PULSE.json","TEMP.json")))))
  carry <- read_json(file.path(root,"CARRY_OVER.json"))
  expect_identical(carry$parameter,"CARRY_OVER")
  expect_identical(carry$members,list("PULSE","TEMP"))
  params <- read_json(file.path(root,"_parameters.json"))
  expect_identical(params$carry_over,list("PULSE","TEMP"))
})
