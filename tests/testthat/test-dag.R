source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))

test_that("layered ADVS skeleton separates dataset, records and parameter variables", {
  advs <- file.path(engine_root,"examples/pharmaverse-demo/spec/advs")
  spec <- read_fixture_spec(file.path(advs,"variable.csv"),file.path(advs,"parameter.csv"))
  graph <- graph_skeleton(spec, "ADVS")
  expect_true(graph$layered)
  kinds <- table(vapply(graph$nodes, function(n) n$kind, ""))
  expect_equal(unname(kinds["records"]), 8L)
  expect_gt(unname(kinds["param_variable"]), 0L)
  expect_equal(unname(kinds["dataset_variable"]), 10L)
  expect_equal(length(graph$layers), 8L)
  expect_true(all(vapply(graph$layers,function(l)length(l$members)==22L,TRUE)))
  expect_false("ADVS.USUBJID"%in%vapply(graph$nodes,`[[`,"","id"))
  prompt <- extract_rows_prompt("ADVS",paste(vapply(extract_spec_rows(spec,"ADVS"),function(r)r$block,""),collapse="\n\n"),TRUE)
  expect_true(all(vapply(unique(spec$variables$Variable),function(v) length(gregexpr(paste0("Variable: ",v," \\|"),prompt,perl=TRUE)[[1]])==1L,TRUE)))
  expect_false(grepl("SOURCE_CATALOG|CATALOG",prompt))
  expect_true(grepl("PARAMCD: ",prompt,fixed=TRUE))
  expect_true(all(vapply(graph$layers,function(l) length(l$records)==1L,TRUE)))
})

test_that("trimmed real ADVS replies expand empty scopes and prefixed variable names", {
  advs <- file.path(engine_root,"examples/pharmaverse-demo/spec/advs")
  spec <- read_fixture_spec(file.path(advs,"variable.csv"),file.path(advs,"parameter.csv"))
  graph <- graph_skeleton(spec,"ADVS")
  parsed <- jsonlite::fromJSON(fixture_path("layers_1c_advs_extract.json"), simplifyVector=FALSE)
  merged <- merge_extract(graph,parsed)
  pairs <- edge_pairs(merged)
  base_chg <- pairs[grepl("^ADVS\\.BASE@.*->ADVS\\.CHG@",pairs)]
  expected <- vapply(graph$layers,function(layer) paste0("ADVS.BASE@",layer$paramcd,"->ADVS.CHG@",layer$paramcd),"")
  expect_setequal(base_chg,expected)
  expect_true(all(vapply(merged$external,function(ext) length(ext$used_by)>0L,TRUE)))
  expect_true(any(vapply(merged$external,function(ext) ext$ref=="VS.USUBJID" && any(grepl("ADVS.RECORDS@",unlist(ext$used_by))),TRUE)))
})

test_that("layered ADVS Source lineage fills omissions with source evidence", {
  advs <- file.path(engine_root,"examples/pharmaverse-demo/spec/advs")
  spec <- read_fixture_spec(file.path(advs,"variable.csv"),file.path(advs,"parameter.csv"))
  graph <- graph_skeleton(spec,"ADVS")
  parsed <- jsonlite::fromJSON(fixture_path("layers_1c_advs_extract.json"), simplifyVector=FALSE)
  merged <- merge_extract(graph,parsed,spec)
  second_run <- merge_extract(graph,parsed,spec)
  pairs <- edge_pairs(merged)
  expect_setequal(edge_pairs(second_run),pairs)
  for (pair in c("ADVS.ADT@SYSBP->ADVS.AVAL@SYSBP",
                 "ADVS.SRCSEQ@SYSBP->ADVS.AVAL@SYSBP",
                 "ADVS.ADT@SYSBP->ADVS.ADY@SYSBP"))
    expect_true(pair %in% pairs)
  source_edges <- Filter(function(edge) identical(edge$origin,"source"),merged$edges)
  expect_true(any(vapply(source_edges,function(edge) identical(edge$from,"ADVS.ADT@SYSBP") &&
                           identical(edge$to,"ADVS.AVAL@SYSBP") && grepl("ADVS.ADT",edge$evidence,fixed=TRUE),TRUE)))
  expect_true(any(vapply(merged$external,function(ext)ext$ref=="ADSL.TRTSDT" &&
                           any(grepl("ADVS.AVAL@",unlist(ext$used_by))),TRUE)))
})

test_that("layered Source fill preserves model scope and reports cycles", {
  scoped <- spec_of(V("X","A",source="X.A"), V("X","B",source="X.A"),
                    params=list(P("X","P1"),P("X","P2")))
  graph <- graph_skeleton(scoped,"X")
  scoped_result <- merge_extract(graph,list(edges=list(list(from="A",to="B",params="P1",evidence="model")),
    external=list(),unresolved=list(),categories=list()),scoped)
  scoped_pairs <- edge_pairs(scoped_result)
  expect_true("X.A@P1->X.B@P1" %in% scoped_pairs)
  expect_false("X.A@P2->X.B@P2" %in% scoped_pairs)

  cyclic <- spec_of(V("X","A",source="X.B"), V("X","B",source="X.A"),
                    params=list(P("X","P1")))
  cycle_result <- merge_extract(graph_skeleton(cyclic,"X"),
    list(edges=list(),external=list(),unresolved=list(),categories=list()),cyclic)
  expect_true(any(vapply(cycle_result$unresolved,function(issue) issue$type=="cycle" &&
                           all(c("A","B") %in% unlist(issue$variables)),TRUE)))
  expect_length(Filter(function(edge) edge$origin=="source",cycle_result$edges),1L)
})

test_that("layered prompt carries spec text once and fake extraction uses Source fields", {
  spec <- spec_of(V("X","USUBJID",key=1,source="DM.USUBJID"),
    V("X","AVAL",source="VS.VSSTRESN"),V("X","CHG",source="X.AVAL"),
    params=list(P("X","P1"),P("X","P2")))
  graph <- graph_skeleton(spec,"X")
  rows <- extract_spec_rows(spec,"X")
  prompt <- paste(vapply(rows,function(r)r$block,""),collapse="\n\n")
  expect_equal(length(gregexpr("(?m)^ROW V",prompt,perl=TRUE)[[1]]),3L)
  expect_match(prompt,"Variable: AVAL [^\n]*\nSource: VS\\.VSSTRESN",perl=TRUE)
  expect_match(prompt,"Variable: CHG [^\n]*\nSource: X\\.AVAL",perl=TRUE)
  fake <- parse_json_reply(fake_extract_rows_reply(prompt,prompt))
  expect_true("X.AVAL" %in% unlist(fake$V3))
  expect_true("VS.VSSTRESN" %in% unlist(fake$V2))
  merged <- merge_extract(graph,list(
    edges=list(list(from="USUBJID",to="AVAL",evidence="key to value"),
               list(from="AVAL",to="USUBJID",evidence="row selection"),
               list(from="AVAL",to="CHG",evidence="change",params="P2")),
    external=list(list(ref="DM.USUBJID",used_by="USUBJID",evidence="DM.USUBJID",params="P2"),
                  list(ref="VS.VSSTRESN",used_by="AVAL",evidence="VS.VSSTRESN",params="P1")),
    unresolved=list(),categories=list()))
  pairs <- edge_pairs(merged)
  expect_false(any(grepl("USUBJID@",pairs)))
  expect_true("X.AVAL@P1->X.RECORDS@P1" %in% pairs)
  expect_true("X.AVAL@P2->X.RECORDS@P2" %in% pairs)
  expect_true("X.AVAL@P2->X.CHG@P2" %in% pairs)
  expect_false("X.AVAL@P1->X.CHG@P1" %in% pairs)
  expect_setequal(unlist(merged$external[[1]]$used_by),"X.RECORDS@P2")
  expect_setequal(unlist(merged$external[[2]]$used_by),"X.AVAL@P1")
})

test_that("layered classification, parameter scopes and records edges are explicit", {
  spec <- spec_of(
    V("X","STUDYID",key=NA,source="DM.STUDYID"),
    V("X","SEX",source="ADSL.SEX"),
    V("X","USUBJID",key=1,source="DM.USUBJID"),
    V("X","AVAL"), V("X","ADT"),
    params=list(P("X","P1"),P("X","P2")))
  g <- graph_skeleton(spec,"X")
  ids <- vapply(g$nodes,function(n)n$id,"")
  expect_true(all(c("X.STUDYID","X.SEX") %in% ids))
  expect_false("X.USUBJID" %in% ids)
  expect_true(all(c("X.AVAL@P1","X.AVAL@P2","X.ADT@P1","X.ADT@P2") %in% ids))
  expect_true(all(vapply(g$nodes[vapply(g$nodes,function(n)n$var=="ADT",TRUE)],function(n)n$kind=="param_variable",TRUE)))
  extracted <- merge_extract(g,list(edges=list(list(from="SEX",to="AVAL",evidence="q",params="P1")),external=list(),unresolved=list()))
  expect_true("X.SEX->X.AVAL@P1" %in% edge_pairs(extracted))
  expect_false("X.SEX->X.AVAL@P2" %in% edge_pairs(extracted))
  expect_true(all(vapply(g$layers,function(l) all(vapply(unlist(l$members),function(id) paste0(l$records,"->",id) %in% edge_pairs(extracted),TRUE)),TRUE)))
})

test_that("cycle, missing ref, and ambiguous ref are reported; a clean spec is a DAG", {
  problems <- read_fixture_spec(fixture_path("problems_variable.csv"))
  clean <- read_fixture_spec(fixture_path("clean_variable.csv"))
  with_fake_llm({
    cyc <- extract_dependencies(list(), problems, "CYC")
    chk <- check_graph(cyc, problems, NULL)
    cyc_issues <- issue_of(chk, "cycle")
    expect_length(cyc_issues, 1)
    expect_true(all(c("CYC.A", "CYC.B") %in% unlist(cyc_issues[[1]]$variables)))
    expect_null(chk$order)
    miss <- issue_of(chk, "missing_ref")
    expect_true(length(miss) >= 1)
    expect_true(any(grepl("NOTREAL", vapply(miss, function(i) paste(i$message, i$text), ""))))

    bad <- extract_dependencies(list(), problems, "BAD")
    chk_bad <- check_graph(bad, problems, NULL)
    amb <- issue_of(chk_bad, "ambiguous")
    expect_true(length(amb) >= 1)
    expect_true("AVAL" %in% unlist(amb[[1]]$variables))
    expect_true("BAD.BASE->BAD.CHG" %in% edge_pairs(bad))

    prose <- extract_dependencies(list(), problems, "PROSE")
    chk_prose <- check_graph(prose, problems, NULL)
    expect_true(length(issue_of(chk_prose, "unresolvable")) >= 1)

    g <- extract_dependencies(list(), clean, "CLN")
    chk_clean <- check_graph(g, clean, NULL)
    types <- vapply(chk_clean$issues, function(i) i$type, "")
    expect_false(any(types %in% c("cycle", "missing_ref", "ambiguous", "unresolvable")))
    expect_false("orphan" %in% types)
    order <- chk_clean$order
    expect_true(is.character(order))
    expect_lt(match("CLN.ROOT", order), match("CLN.A", order))
    expect_lt(match("CLN.ROOT", order), match("CLN.B", order))
    expect_lt(match("CLN.A", order), match("CLN.A1", order))
    expect_lt(match("CLN.B", order), match("CLN.B1", order))
  })
})

test_that("check_graph finds a self-loop, a dangling edge, an orphan, and passed-through issues", {
  loop <- make_graph("ADSL", list(gn("ADSL.A")), list(ge("ADSL.A", "ADSL.A")))
  chk <- check_graph(loop, NULL, NULL)
  expect_equal(vapply(issue_of(chk, "cycle"), function(i) i$type, ""), "cycle")
  expect_equal(issue_of(chk, "cycle")[[1]]$severity, "error")
  expect_true(node_blocked("ADSL.A", "A", "ADSL", chk$issues))
  expect_null(chk$order)

  dangling <- make_graph("ADSL", list(gn("ADSL.A")), list(ge("ADSL.NOPE", "ADSL.A")))
  chk2 <- check_graph(dangling, NULL, NULL)
  expect_true(any(grepl("NOPE", vapply(issue_of(chk2, "missing_ref"), function(i) i$message, ""))))

  orphan <- make_graph("ADSL", list(gn("ADSL.A"), gn("ADSL.B"), gn("ADSL.ISO")),
                       list(ge("ADSL.A", "ADSL.B")))
  chk3 <- check_graph(orphan, NULL, NULL)
  orphans <- unlist(lapply(issue_of(chk3, "orphan"), function(i) flat_chr(i$variables)))
  expect_equal(orphans, "ADSL.ISO")
  expect_equal(issue_of(chk3, "orphan")[[1]]$severity, "warning")
  expect_false(node_blocked("ADSL.ISO", "ISO", "ADSL", chk3$issues))
  expect_true(!is.null(chk3$order))

  ext_only <- make_graph("ADSL", list(gn("ADSL.TRTSDT"), gn("ADSL.ISO")), external = list(
    list(ref = "DM.RFSTDTC", domain = "DM", var = "RFSTDTC", used_by = list("ADSL.TRTSDT"), evidence = "DM.RFSTDTC")
  ))
  chk_ext <- check_graph(ext_only, NULL, NULL)
  ext_orphans <- unlist(lapply(issue_of(chk_ext, "orphan"), function(i) flat_chr(i$variables)))
  expect_equal(ext_orphans, "ADSL.ISO")
  expect_lt(match("ADSL.A", chk3$order), match("ADSL.B", chk3$order))

  amb <- make_graph("ADSL", list(gn("ADSL.CHG")), unresolved = list(
    new_issue("ambiguous", "ADSL", c("AVAL", "ADSL.CHG"), text = "AVAL", message = "AVAL is ambiguous")))
  chk4 <- check_graph(amb, NULL, NULL)
  expect_true(length(issue_of(chk4, "ambiguous")) == 1)
})

test_that("data_index reads xpt headers and external refs become missing_data or missing_var", {
  proj <- temp_project()
  empty <- data_index(proj)
  expect_equal(empty, list())
  file.copy(fixture_path("dm.xpt"), file.path(proj, "data", "dm.xpt"))
  file.copy(fixture_path("vs.xpt"), file.path(proj, "data", "vs.xpt"))
  idx <- data_index(proj)
  expect_true(all(c("USUBJID", "RFSTDTC", "AGE") %in% idx$DM))
  expect_true(all(c("USUBJID", "VSTESTCD", "VSSTRESN") %in% idx$VS))

  g <- make_graph("ADSL", list(gn("ADSL.TRTSDT")), external = list(
    list(ref = "DM.RFSTDTC", domain = "DM", var = "RFSTDTC", used_by = list("ADSL.TRTSDT"), evidence = "DM.RFSTDTC"),
    list(ref = "XX.YY", domain = "XX", var = "YY", used_by = list("ADSL.TRTSDT"), evidence = "XX.YY")
  ))
  chk <- check_graph(g, NULL, idx)
  expect_true(length(issue_of(chk, "missing_data")) == 1)
  expect_true(length(issue_of(chk, "missing_var")) == 0)
  expect_false(any(grepl("RFSTDTC", vapply(issue_of(chk, "missing_data"), function(i) i$message, ""))))

  thin <- temp_project()
  haven::write_xpt(data.frame(USUBJID = "S1", stringsAsFactors = FALSE),
                   file.path(thin, "data", "dm.xpt"), version = 5, name = "DM")
  chk2 <- check_graph(g, NULL, data_index(thin))
  expect_true(any(grepl("RFSTDTC", vapply(issue_of(chk2, "missing_var"), function(i) i$message, ""))))
})

test_that("any node that is not Data matched can generate, and a drop only flags descendants", {
  nodes <- list(gn("DS.ROOT"), gn("DS.A"), gn("DS.B"), gn("DS.A1"), gn("DS.B1"), gn("DS.ISO"))
  edges <- list(ge("DS.ROOT", "DS.A"), ge("DS.ROOT", "DS.B"), ge("DS.A", "DS.A1"), ge("DS.B", "DS.B1"))
  g <- make_graph("DS", nodes, edges, confirmed = TRUE)
  statuses <- init_statuses(g)
  for (id in names(statuses)) statuses[[id]]$level <- "none"
  statuses$DS.ISO$level <- "L3"
  statuses$DS.ISO$sides$primary$code <- "keep"

  views <- annotate_nodes(statuses, g, list())
  expect_true(views$DS.ROOT$ready)
  expect_true("generate" %in% views$DS.ROOT$actions)
  expect_true(views$DS.A$ready)
  expect_true("generate" %in% views$DS.A$actions)
  expect_true(views$DS.A1$ready)
  expect_false(views$DS.ISO$ready)
  expect_true("generate" %in% views$DS.ISO$actions)

  statuses <- advance_generate(statuses, "DS.ROOT", ok = TRUE, graph = g)
  expect_equal(statuses$DS.ROOT$level, "L2")
  expect_true("DS.ROOT" %in% statuses$DS.A$upstream_changed)
  expect_null(statuses$DS.ISO$upstream_changed)
  views <- annotate_nodes(statuses, g, list())
  expect_true(views$DS.A$ready)

  statuses <- advance_compare(statuses, g, "DS.ROOT", match = TRUE)
  expect_equal(statuses$DS.ROOT$level, "L3")
  expect_null(statuses$DS.ROOT$problem)
  views <- annotate_nodes(statuses, g, list())
  expect_true(views$DS.A$ready)
  expect_true("generate" %in% views$DS.A$actions)
  expect_false(views$DS.ROOT$ready)

  for (id in c("DS.A", "DS.B", "DS.A1", "DS.B1")) {
    statuses[[id]]$level <- "L3"
    statuses[[id]]$sides$primary$code <- "stale"
    statuses[[id]]$sides$validation$code <- "stale"
  }
  statuses <- demote_node(statuses, g, "DS.ROOT", to = "none",
                          problem = list(stage = "validate", message = "spec changed"))
  expect_equal(statuses$DS.ROOT$level, "none")
  expect_equal(statuses$DS.ROOT$problem$stage, "validate")
  for (id in c("DS.A", "DS.B", "DS.A1", "DS.B1")) {
    expect_equal(statuses[[id]]$level, "L3")
    expect_equal(statuses[[id]]$sides$primary$code, "stale")
    expect_equal(statuses[[id]]$sides$validation$code, "stale")
    expect_true("DS.ROOT" %in% statuses[[id]]$upstream_changed)
  }
  expect_equal(statuses$DS.ISO$level, "L3")
  expect_equal(statuses$DS.ISO$sides$primary$code, "keep")
  expect_null(statuses$DS.ISO$upstream_changed)
  views <- annotate_nodes(statuses, g, list())
  expect_false(views$DS.A$ready)
  expect_true(views$DS.ROOT$ready)
  expect_false("validate" %in% views$DS.ROOT$actions)
  expect_true("generate" %in% views$DS.A$actions)
})

test_that("group status is the minimum member level", {
  expect_equal(group_status(list(list(level = "L3"), list(level = "none")))$level, "none")
  expect_equal(group_status(list("L2", "L3"))$level, "L2")
  expect_equal(group_status(list(list(level = "none"), list(level = "L3")))$level, "none")
  expect_equal(group_status(list(list(level = "L3", same_source = TRUE),
                                 list(level = "L3", same_source = TRUE)))$level, "L3")

  g <- make_graph("ADVS",
    list(gn("ADVS.AVAL@WEIGHT", "WEIGHT"), gn("ADVS.CHG@WEIGHT", "WEIGHT")),
    groups = list(list(id = "ADVS@WEIGHT", paramcd = "WEIGHT",
                       members = c("ADVS.AVAL@WEIGHT", "ADVS.CHG@WEIGHT"))))
  statuses <- init_statuses(g)
  statuses$`ADVS.AVAL@WEIGHT`$level <- "L3"
  statuses$`ADVS.CHG@WEIGHT`$level <- "L2"
  views <- annotate_nodes(statuses, g, list())
  gs <- annotate_groups(g, views)
  expect_equal(gs$`ADVS@WEIGHT`$level, "L2")
})

test_that("layered variable graph keeps parameter dependencies out of its layers", {
  spec <- read_fixture_spec(fixture_path("params_variable.csv"), fixture_path("params_parameter.csv"))
  with_fake_llm({
    g <- extract_dependencies(list(), spec, "ADVS")
    ids <- vapply(g$nodes, function(n) n$id, "")
    expect_true(all(c("ADVS.AVAL@WEIGHT", "ADVS.AVAL@HEIGHT", "ADVS.AVAL@BMI") %in% ids))
    expect_false("ADVS.USUBJID" %in% ids)
    pairs <- edge_pairs(g)
    expect_false(any(grepl("@WEIGHT.*@BMI", pairs)))
    bmi <- Filter(function(gr) identical(gr$id, "ADVS@BMI"), g$groups)[[1]]
    expect_true("ADVS.AVAL@BMI" %in% bmi$members)
    chk <- check_graph(g, spec, NULL)
    expect_length(issue_of(chk, "cycle"), 0)
    expect_true(is.character(chk$order))
    for (layer in g$layers) for (member in unlist(layer$members))
      expect_true(paste0(layer$records,"->",member) %in% pairs)
  })
})

test_that("legacy validate problems clear on Generate and failures stay Not generated", {
  g <- make_graph("DS", list(gn("DS.A"), gn("DS.B")), list(ge("DS.A", "DS.B")))
  statuses <- init_statuses(g)
  statuses$DS.A$problem <- list(stage = "validate", message = "legacy")
  expect_equal(statuses$DS.A$level, "none")
  expect_equal(statuses$DS.A$problem$stage, "validate")
  statuses <- advance_generate(statuses, "DS.A", ok = FALSE, message = "run blew up", stage = "run")
  expect_equal(statuses$DS.A$level, "none")
  expect_equal(statuses$DS.A$problem$stage, "run")
  node <- list(level = "L2", problem = list(stage = "compare", message = "diff"))
  expect_true(all(c("generate", "compare", "investigate") %in% actions(node)))
  hidden <- list(level = "none", problem = NULL)
  expect_equal(actions(hidden), "generate")
  fresh <- list(level = "none", problem = NULL)
  expect_equal(actions(fresh), "generate")
  statuses <- advance_generate(statuses, "DS.A", ok = TRUE)
  expect_equal(statuses$DS.A$level, "L2")
  expect_null(statuses$DS.A$problem)
})
