app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
for (f in c("utils.R", "deps.R", "config.R", "llm.R", "levels.R", "dag.R", "spec.R", "project.R",
            "deps_extract.R", "sides.R", "dag_agent.R", "server.R"))
  sys.source(file.path(app_root, "R", f), envir = globalenv())
dualdag_engine_loaded <- TRUE
source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))

display_project <- function() {
  p <- open_project(tempfile("dag-display-"))
  g <- make_graph("ADSL", nodes = list(
    gn("ADSL.AGEGR1"), gn("ADSL.FLAG"), gn("ADSL.AGE")))
  g$nodes[[1]]$categories <- list(list(value = "<65", rule = "AGE < 65"),
                                  list(value = ">=65", rule = "AGE >= 65"))
  save_graph(p, g)
  p
}

display_call <- function(A, path, method = "GET", body = NULL) {
  q <- if (grepl("?", path, fixed = TRUE)) sub("^[^?]*\\?", "", path) else ""
  path <- sub("\\?.*$", "", path)
  raw <- if (is.null(body)) raw() else charToRaw(as.character(to_json(body)))
  make_handlers(A)$call(list(PATH_INFO = path, REQUEST_METHOD = method, QUERY_STRING = q,
    HTTP_HOST = sprintf("127.0.0.1:%d", A$port), HTTP_X_DUALDAG_TOKEN = A$token,
    CONTENT_TYPE = if (method == "POST") "application/json" else "",
    rook.input = list(read = function() raw)))
}

test_that("layout persists, resets, and does not constrain new nodes", {
  p <- display_project()
  A <- create_app(app_root, p$dir, 7988L, "display-token", load_config())
  body <- list(dataset = "ADSL", positions = list("ADSL.AGEGR1" = list(x = 42, y = -17)))
  expect_equal(display_call(A, "/api/graph/layout", "POST", body)$status, 200L)
  expect_equal(load_layout(p, "ADSL")[["ADSL.AGEGR1"]]$x, 42)
  g <- load_graph(p, "ADSL")
  g$nodes <- c(g$nodes, list(gn("ADSL.NEW")))
  save_graph(p, g)
  state <- from_json(display_call(A, "/api/state")$body)
  expect_equal(state$datasets$ADSL$layout[["ADSL.AGEGR1"]]$y, -17)
  expect_null(state$datasets$ADSL$layout[["ADSL.NEW"]])
  expect_equal(display_call(A, "/api/graph/layout", "POST", list(dataset = "ADSL", reset = TRUE))$status, 200L)
  expect_false(file.exists(layout_path(p, "ADSL")))
})

test_that("layout accepts a display-only key node from the spec", {
  p <- display_project()
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
               "ADSL,USUBJID,Subject,Char,20,1,DM.USUBJID,Copy"),
             file.path(p$dir, "spec", "variable.csv"))
  saved <- save_layout(p, "ADSL", list("ADSL.USUBJID" = list(x = 10, y = 20)))
  expect_equal(saved[["ADSL.USUBJID"]]$x, 10)
})

test_that("category endpoint counts typed values and flags discrepancies", {
  p <- display_project()
  A <- create_app(app_root, p$dir, 7988L, "display-token", load_config())
  write_out <- function(id, side, x) {
    dir <- file.path(node_run_dir(p, "ADSL", id), side)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(setNames(data.frame(x), sub("^.*\\.", "", id)), file.path(dir, "out.rds"))
  }
  write_out("ADSL.AGEGR1", "primary", c("<65", "<65", "extra"))
  write_out("ADSL.AGEGR1", "validation", c("<65", ">=65"))
  result <- from_json(display_call(A, "/api/node/categories?dataset=ADSL&node=ADSL.AGEGR1")$body)
  expect_true(result$categorical)
  rows <- result$rows
  expect_equal(rows[[1]]$primary, 2)
  expect_true(rows[[2]]$mismatch)
  expect_false(rows[[3]]$in_spec)
  expect_true(rows[[3]]$mismatch)
  write_out("ADSL.AGE", "primary", as.Date("2020-01-01") + 0:2)
  expect_false(from_json(display_call(A, "/api/node/categories?dataset=ADSL&node=ADSL.AGE")$body)$categorical)
  write_out("ADSL.FLAG", "primary", rep(c(TRUE, FALSE), each = 2))
  expect_true(from_json(display_call(A, "/api/node/categories?dataset=ADSL&node=ADSL.FLAG")$body)$categorical)
  write_out("ADSL.FLAG", "primary", as.character(1:21))
  expect_true(from_json(display_call(A, "/api/node/categories?dataset=ADSL&node=ADSL.FLAG")$body)$overflow)
})

test_that("category counts use the shared distinct-level ordering and limits", {
  x <- factor(c("b", "a", "b", NA))
  counted <- node_value_counts(x)
  shared <- distinct_levels(x)
  expect_equal(vapply(counted$counts, function(row) row$value, ""), shared$value)
  expect_equal(vapply(counted$counts, function(row) row$n, 0L), shared$n)
  expect_true(node_value_counts(as.character(1:21))$overflow)
  expect_null(node_value_counts(as.Date("2020-01-01")))
})

test_that("mock state route keeps record IDs while the UI presents Keys labels", {
  p <- display_project()
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADSL,USUBJID,Subject,Char,20,1,DM.USUBJID,",
    "ADSL,PARAMCD,Parameter,Char,8,2,VS.VSTESTCD,",
    "ADSL,AVISITN,Visit,Num,8,3,VS.AVISITN,",
    "ADSL,ATPTN,Timepoint,Num,8,4,VS.ATPTN,"),file.path(p$dir,"spec","variable.csv"))
  g <- load_graph(p,"ADSL")
  g$layered <- TRUE
  g$nodes <- c(g$nodes,list(list(id="ADSL.RECORDS@SYSBP",var="RECORDS",paramcd="SYSBP",kind="records",key_vars=c("USUBJID","PARAMCD","AVISITN","ATPTN"))))
  save_graph(p,g)
  A <- create_app(app_root,p$dir,7988L,"display-token",load_config())
  on.exit(shutdown_app(A),add=TRUE)
  state <- from_json(display_call(A,"/api/state")$body)
  node <- Filter(function(n)identical(n$id,"ADSL.RECORDS@SYSBP"),state$datasets$ADSL$graph$nodes)[[1]]
  expect_identical(node$id,"ADSL.RECORDS@SYSBP")
  expect_identical(node$kind,"records")
  ui <- paste(readLines(file.path(app_root,"www","app.js"),warn=FALSE),collapse="\n")
  expect_match(ui,"function displayNode(id, shortForm)",fixed=TRUE)
  expect_match(ui,'"Keys · " + node.paramcd',fixed=TRUE)
  expect_match(ui,'" (" + names.join(", ") + ")"',fixed=TRUE)
  expect_false(grepl("Validate \" + displayNode(id)",ui,fixed=TRUE))
  expect_match(ui,'displayNode(w.node)',fixed=TRUE)
})

test_that("fake extraction and DAG changes carry categories", {
  expect_length(parse_spec_categories("CHG = AVAL - BASE"), 0L)
  sp <- spec_of(V("ADSL", "AGEGR1", derivation = '"<65" if AGE < 65; >=65 = AGE >= 65'))
  graph <- graph_skeleton(sp, "ADSL")
  merged <- with_fake_llm(extract_dependencies(list(), sp, "ADSL"))
  expect_equal(length(merged$nodes[[1]]$categories), 2L)
  changed <- dag_mutate(merged, "ADSL", list(list(op = "set_categories", node = "ADSL.AGEGR1",
    categories = list(list(value = "Young", rule = "AGE < 65")))))
  expect_equal(changed$graph$nodes[[1]]$categories[[1]]$value, "Young")
})
