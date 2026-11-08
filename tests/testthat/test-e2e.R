source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))

# Same ADSL spec the live script posts, driven through the functions server.R calls.
# No HTTP server. DUALDAG_FAKE_LLM makes extraction and generation deterministic.

load_app <- function() {
  root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
  app <- new.env(parent = globalenv())
  files <- list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)
  utils <- files[grepl("/utils\\.R$", files)]
  for (f in c(utils, setdiff(files, utils))) sys.source(f, envir = app)
  app
}

adsl_spec_csv <- function() {
  c(
    "Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Unique Subject ID,Char,20,1,DM.USUBJID,Copy DM.USUBJID",
    "ADSL,TRTSDTC,First dose date,Char,10,,DM.RFSTDTC,Copy DM.RFSTDTC",
    "ADSL,AGE,Age,Num,8,,DM.AGE,Copy DM.AGE",
    "ADSL,AGE2,Age plus one,Num,8,,,AGE + 1"
  )
}

test_that("ADSL copies and AGE + 1 generate, compare, and export", {
  app <- load_app()
  proj <- tempfile("dualdag-e2e-")
  dir.create(file.path(proj, "spec"), recursive = TRUE)
  dir.create(file.path(proj, "data"), recursive = TRUE)
  writeLines(adsl_spec_csv(), file.path(proj, "spec", "variable.csv"))
  file.copy(fixture_path("dm.xpt"), file.path(proj, "data", "dm.xpt"))
  ids <- c("ADSL.USUBJID", "ADSL.TRTSDTC", "ADSL.AGE", "ADSL.AGE2")

  with_fake_llm({
    p <- app$open_project(proj)
    spec <- app$read_project_spec(p)
    g0 <- app$extract_dependencies(list(), spec, "ADSL")
    app$save_graph(p, g0)
    app$note_graph_nodes(p, "ADSL", app$load_graph(p, "ADSL"))
    g <- app$load_graph(p, "ADSL")
    chk <- app$check_graph(g, spec, app$data_index(proj))
    types <- if (length(chk$issues)) vapply(chk$issues, function(i) i$type, "") else character()
    expect_false("orphan" %in% types)
    expect_false(any(vapply(chk$issues, function(i) app$issue_severity(i) == "error", logical(1))))
    expect_true(all(ids %in% chk$order))

    for (id in ids) {
      expect_equal(app$actions(app$get_status(p, "ADSL", id)), "generate")
    }

    for (id in ids) {
      node <- app$graph_node(g, id)
      ups <- app$upstream_ids(g, id)
      up_vars <- if (length(ups)) vapply(ups, function(u) app$split_node_id(u)$var, "") else character()
      results <- list()
      for (side in c("primary", "validation")) {
        gen <- app$generate_side(list(), side, spec, g, node, up_vars, project = proj)
        parsed <- app$normalize_generated(gen)
        ran <- app$run_side(proj, "ADSL", node, side, parsed$code, timeout = 60)
        expect_true(isTRUE(ran$ok))
        results[[side]] <- list(
          ok = TRUE,
          value = list(code = parsed$code, same_source = isTRUE(parsed$same_source), run = ran)
        )
      }
      if (identical(id, "ADSL.AGE2")) expect_match(results$primary$value$code, "\\+ 1")
      app$apply_generate_results(p, "ADSL", id, results)
      st <- app$get_status(p, "ADSL", id)
      expect_equal(st$level, "L2")
      expect_null(st$problem)
      cmp <- app$compare_node(proj, "ADSL", node, app$dataset_keys(spec, "ADSL"))
      expect_true(isTRUE(cmp$match))
      st <- app$advance_compare(setNames(list(st), id), g, id, match = TRUE)[[id]]
      app$put_status(p, "ADSL", id, st)
      expect_equal(app$get_status(p, "ADSL", id)$level, "L3")
    }

    xpt <- app$export_dataset(proj, "ADSL", spec, "xpt")
    expect_gt(file.info(xpt)$size, 0)
    back <- haven::read_xpt(xpt)
    expect_equal(as.character(back$USUBJID), c("S1", "S2", "S3"))
    expect_equal(as.character(back$TRTSDTC), c("2020-01-01", "2020-01-02", "2020-01-03"))
    expect_equal(as.numeric(back$AGE), c(40, 41, 42))
    expect_equal(as.numeric(back$AGE2), c(41, 42, 43))
    expect_equal(attr(back$USUBJID, "label"), "Unique Subject ID")
    expect_equal(attr(back$TRTSDTC, "label"), "First dose date")
    expect_equal(attr(back$AGE, "label"), "Age")
    expect_equal(attr(back$AGE2, "label"), "Age plus one")
  })
})
