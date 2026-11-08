source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))

test_that("successful side runs write facts and edits survive reruns", {
  proj <- temp_project()
  file.copy(fixture_path("dm.xpt"), file.path(proj, "data", "dm.xpt"))
  code <- 'result <- data.frame(USUBJID = src$DM$USUBJID, X = as.Date(src$DM$RFSTDTC))'
  ran <- run_side(proj, "ADSL", "ADSL.X", "primary", code, description = "X is the study start date. It comes from DM.RFSTDTC; absent source dates remain missing.")
  expect_true(ran$ok)
  card <- card_read(proj, "ADSL", "ADSL.X", "primary")
  expect_equal(card$facts$r_class, "Date")
  expect_equal(card$facts$rows, 3)
  expect_equal(card$facts$na_count, 0)
  expect_equal(card$facts$min, "2020-01-01")
  expect_equal(card$facts$max, "2020-01-03")
  expect_match(card$facts$sample, "sampled")
  expect_true(nzchar(card$facts$code_hash))
  expect_true(nzchar(card$facts$out_hash))
  expect_match(card$description, "study start date")
  card_edit(proj, "ADSL", "ADSL.X", "primary", "Edited date meaning")
  run_side(proj, "ADSL", "ADSL.X", "primary", code, description = "New agent description")
  expect_equal(card_view(proj, "ADSL", "ADSL.X", "primary")$display_description, "Edited date meaning")
  card_edit(proj, "ADSL", "ADSL.X", "primary", revert = TRUE)
  expect_equal(card_view(proj, "ADSL", "ADSL.X", "primary")$display_description, "New agent description")
  run_side(proj, "ADSL", "ADSL.X", "primary", 'stop("failed")')
  expect_null(card_read(proj, "ADSL", "ADSL.X", "primary"))
})

test_that("categorical facts, parent cards, and warnings are local to a side", {
  proj <- temp_project()
  graph <- make_graph("ADSL", list(gn("ADSL.A"), gn("ADSL.B"), gn("ADSL.C"),gn("ADSL.S")),
                      list(ge("ADSL.A", "ADSL.B"), ge("ADSL.B", "ADSL.C"),ge("ADSL.A","ADSL.S")))
  spec <- spec_of(V("ADSL", "A", type = "Char"), V("ADSL", "B", type = "Char"), V("ADSL", "C", type = "Num"),V("ADSL","S",type="Char"))
  a <- write_result(proj, "ADSL", "ADSL.A", "primary", data.frame(A = c("x", NA, "y")))
  writeLines("result <- 1", file.path(a, "code.R"))
  card_write(proj, "ADSL", "ADSL.A", "primary", "Parent A", graph, spec)
  b <- write_result(proj, "ADSL", "ADSL.B", "primary", data.frame(B = c("p", "q", NA)))
  writeLines("result <- 2", file.path(b, "code.R"))
  card_write(proj, "ADSL", "ADSL.B", "primary", "Parent B", graph, spec)
  a_validation <- write_result(proj,"ADSL","ADSL.A","validation",data.frame(A=c("x","y")))
  writeLines("result <- 1",file.path(a_validation,"code.R"))
  card_write(proj,"ADSL","ADSL.A","validation","Validation A",graph,spec)
  b_validation <- write_result(proj,"ADSL","ADSL.B","validation",data.frame(B=c("p","q")))
  writeLines("result <- 2",file.path(b_validation,"code.R"))
  card_write(proj,"ADSL","ADSL.B","validation","Validation B",graph,spec)
  c <- write_result(proj, "ADSL", "ADSL.C", "primary", data.frame(C = c("a", "b", NA)))
  writeLines("result <- 3", file.path(c, "code.R"))
  card_write(proj, "ADSL", "ADSL.C", "primary", "Child", graph, spec)
  expect_equal(card_read(proj, "ADSL", "ADSL.B", "primary")$facts$na_count, 1)
  expect_equal(length(card_read(proj, "ADSL", "ADSL.B", "primary")$facts$distinct_levels$values), 3)
  prompt <- lineage_context(spec, graph, gn("ADSL.C"), proj, "primary")$text
  expect_match(prompt, "Parent B", fixed = TRUE)
  expect_match(prompt, "Parent A", fixed = TRUE)
  expect_lt(regexpr("Parent B",prompt,fixed=TRUE)[[1]],regexpr("Parent A",prompt,fixed=TRUE)[[1]])
  expect_match(prompt, "ADSL.B <character>", fixed = TRUE)
  expect_false(grepl("built_at|code_hash|out_hash|parent_code_hashes",prompt))
  disk_card <- read_text(card_path(proj, "ADSL", "ADSL.B", "primary"))
  expect_match(disk_card, "built_at", fixed = TRUE)
  expect_match(disk_card, "parent_code_hashes", fixed = TRUE)
  validation_block <- card_prompt_block(proj,"ADSL","ADSL.B","validation")
  expect_false(grepl("built_at|code_hash|out_hash|parent_code_hashes",validation_block))
  expect_match(read_text(card_path(proj,"ADSL","ADSL.B","validation")),"built_at",fixed=TRUE)
  expect_false(grepl("ADSL.A\nROW:", prompt, fixed = TRUE))
  expect_false(grepl("ADSL.B\nROW:", prompt, fixed = TRUE))
  expect_false(grepl("Parent S", prompt, fixed = TRUE))
  expect_false(grepl("validation", prompt, fixed = TRUE))
  validation_prompt <- lineage_context(spec,graph,gn("ADSL.C"),proj,"validation")$text
  expect_match(validation_prompt,"Validation B",fixed=TRUE)
  expect_match(validation_prompt,"Validation A",fixed=TRUE)
  expect_lt(regexpr("Validation B",validation_prompt,fixed=TRUE)[[1]],regexpr("Validation A",validation_prompt,fixed=TRUE)[[1]])
  expect_false(grepl("Parent A|Parent B",validation_prompt))
  unlink(card_path(proj,"ADSL","ADSL.B","primary"))
  one_card <- lineage_context(spec,graph,gn("ADSL.C"),proj,"primary")$text
  expect_match(one_card,"Parent A",fixed=TRUE)
  expect_match(one_card,"ADSL.B\nROW:",fixed=TRUE)
  expect_false(grepl("ADSL.A\nROW:",one_card,fixed=TRUE))
  own <- agent_user_prompt("primary", spec, graph, gn("ADSL.C"), "What is this?", proj)
  expect_match(own, "LABEL: C", fixed = TRUE)
  expect_match(own,"ADSL.B\nROW:",fixed=TRUE)
  expect_match(own,"Parent A",fixed=TRUE)
  expect_match(paste(unlist(card_view(proj, "ADSL", "ADSL.C", "primary", spec, graph)$warnings), collapse = " "), "spec says numeric")
  writeLines("result <- 22", file.path(b, "code.R"))
  card_write(proj, "ADSL", "ADSL.B", "primary", graph = graph, spec = spec)
  expect_match(paste(unlist(card_view(proj, "ADSL", "ADSL.C", "primary", spec, graph)$warnings), collapse = " "), "Upstream changed")
})

test_that("old L3 results gain facts-only cards on refresh", {
  proj <- temp_project()
  write_result(proj, "ADSL", "ADSL.X", "primary", data.frame(X = c(1, NA_real_)))
  dir.create(file.path(proj, ".dualdag", "state"), recursive = TRUE, showWarnings = FALSE)
  write_json(list(nodes = list(ADSL = list("ADSL.X" = list(level = "L3")))),
             file.path(proj, ".dualdag", "state", "state.json"))
  project_env <- new.env(parent = .GlobalEnv)
  sys.source(file.path(testthat::test_path(), "..", "..", "R", "project.R"), envir = project_env)
  project_env$open_project(proj)
  card <- card_read(proj, "ADSL", "ADSL.X", "primary")
  expect_equal(card$facts$r_class, "numeric")
  expect_equal(card$description, "")
  expect_null(card_read(proj, "ADSL", "ADSL.X", "validation"))
})

test_that("project refresh keeps cards for successful sides at none, including legacy L1", {
  make_project <- function(level = "none", primary_ok = TRUE, with_primary_output = TRUE) {
    proj <- temp_project()
    id <- "ADSL.X"
    if (with_primary_output) {
      write_result(proj, "ADSL", id, "primary", data.frame(X = c(1, 2)))
      card_write(proj, "ADSL", id, "primary", description = "Keep this description")
    }
    st <- list(level = level, problem = NULL, same_source = FALSE,
               sides = list(primary = list(ok = primary_ok, error = NULL, updated = NULL),
                            validation = list(ok = NULL, error = NULL, updated = NULL)))
    dir.create(file.path(proj, ".dualdag", "state"), recursive = TRUE, showWarnings = FALSE)
    write_json(list(nodes = list(ADSL = setNames(list(st), id))),
               file.path(proj, ".dualdag", "state", "state.json"))
    list(project = open_project(proj), dir = proj, id = id)
  }

  pending <- make_project()
  expect_identical(get_status(pending$project, "ADSL", pending$id)$level, "none")
  expect_true(file.exists(card_path(pending$project, "ADSL", pending$id, "primary")))
  expect_identical(card_read(pending$project, "ADSL", pending$id, "primary")$description,
                   "Keep this description")

  failed <- make_project(primary_ok = FALSE)
  expect_null(card_read(failed$project, "ADSL", failed$id, "primary"))

  fresh <- make_project(primary_ok = NULL, with_primary_output = FALSE)
  expect_null(card_read(fresh$project, "ADSL", fresh$id, "primary"))

  legacy <- make_project(level = "L1")
  expect_identical(get_status(legacy$project, "ADSL", legacy$id)$level, "none")
  expect_identical(card_read(legacy$project, "ADSL", legacy$id, "primary")$description,
                   "Keep this description")
})

test_that("reset removes a side card", {
  project_env <- new.env(parent = .GlobalEnv)
  sys.source(file.path(testthat::test_path(), "..", "..", "R", "project.R"), envir = project_env)
  proj <- project_env$open_project(temp_project())
  write_result(proj$dir, "ADSL", "ADSL.X", "primary", data.frame(X = 1))
  card_write(proj$dir, "ADSL", "ADSL.X", "primary")
  expect_true(file.exists(card_path(proj, "ADSL", "ADSL.X", "primary")))
  project_env$reset_one_node(proj, "ADSL", "ADSL.X")
  expect_false(file.exists(card_path(proj, "ADSL", "ADSL.X", "primary")))
})
