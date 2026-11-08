# Extraction: plan, one pass per parameter, check, cycles. All models here are scripted.
extract_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
for (f in c("utils", "config", "llm", "spec", "dag", "deps", "deps_extract", "project", "param_graph")) {
  if (!exists("extract_dependencies", mode = "function", inherits = TRUE) || f %in% c("deps_extract", "project", "param_graph"))
    sys.source(file.path(extract_root, "R", paste0(f, ".R")), envir = globalenv())
}

xl_spec <- function(params = TRUE) {
  dir <- tempfile("xl-spec-"); dir.create(dir)
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADX,USUBJID,Subject,Char,20,1,VS.USUBJID,Copy VS.USUBJID.",
    if (params) "ADX,PARAMCD,Code,Char,8,2,VS.VSTESTCD,Copy VS.VSTESTCD.",
    "ADX,STUDYID,Study,Char,8,,ADSL.STUDYID,Copy ADSL.STUDYID.",
    "ADX,AVAL,Value,Num,8,,VS.VSSTRESN; ADX.ADT,For P1 copy VS.VSSTRESN. For P2 use ADX.ADT.",
    "ADX,ADT,Date,Num,8,,VS.VSDTC,Convert VS.VSDTC.",
    "ADX,BASE,Base,Num,8,,ADX.AVAL,Copy ADX.AVAL.",
    "ADX,CHG,Change,Num,8,,ADX.AVAL; ADX.BASE,AVAL minus BASE."), file.path(dir, "variable.csv"))
  if (params) writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation",
    "ADX,P1,First,VS.VSTESTCD,Keep P1 rows.", "ADX,P2,Second,VS.VSTESTCD,Keep P2 rows."), file.path(dir, "parameter.csv"))
  read_spec(file.path(dir, "variable.csv"), if (params) file.path(dir, "parameter.csv"))
}

xl_task <- function(system) sub(".*TASK: ([a-z_]+).*", "\\1", substr(system, 1L, 60L))
xl_row_ids <- function(text) regmatches(text, gregexpr("(?<=ROW )[VP][0-9]+", text, perl = TRUE))[[1]]

# rows(parameter, target) gives a row's items. check and cycles get the prompt and return a reply list.
xl_script <- function(rows = NULL, check = NULL, cycles = NULL, plan = NULL, spec = NULL, classification = NULL) {
  log <- new.env(); log$calls <- list()
  targets <- function(user) {
    ids <- xl_row_ids(user)
    setNames(lapply(ids, function(id) {
      m <- regmatches(user, regexec(paste0("ROW ", id, " \\| [A-Za-z]+: ([^ ]+)"), user))[[1]]
      m[[2]]
    }), ids)
  }
  fn <- function(cfg, system, user, ...) {
    task <- xl_task(system)
    log$calls[[length(log$calls) + 1L]] <- list(task = task, model = cfg$model %||% "", system = system, user = user)
    reply <- switch(task,
      extract_classification = {
        line <- grep("^PARAMCD_LIST: ", strsplit(user,"\n",fixed=TRUE)[[1]], value=TRUE)
        pcs <- if (length(line)) strsplit(sub("^PARAMCD_LIST: ","",line[[1]]),", ",fixed=TRUE)[[1]] else character()
        list(parameters=lapply(pcs,function(pc){value<-if(is.null(classification))"derived" else classification[[pc]] %||% "derived"; list(paramcd=pc,classification=value,reason="Scripted reason.")}))
      },
      extract_plan = if (is.null(plan)) list(batches = list(list(kind = "simple", rows = as.list(xl_row_ids(user))))) else plan(user),
      extract_rows = {
        if (grepl("CARRY_OVER_PARAMETERS:",user,fixed=TRUE)) {
          lines <- strsplit(user,"\n",fixed=TRUE)[[1]]; pcs <- strsplit(sub("CARRY_OVER_PARAMETERS: ","",lines[[1]]),", ",fixed=TRUE)[[1]]
          tg <- setdiff(xl_row_ids(user), character())
          setNames(lapply(tg,function(id){m<-regmatches(user,regexec(paste0("ROW ",id," \\| [A-Za-z]+: ([^ ]+)"),user))[[1]]; unique(unlist(lapply(pcs,function(pc)(rows %||% function(p,t)character())(pc,m[[2]])),use.names=FALSE))}),tg)
        } else {
        pc <- regmatches(user, regexec("^Parameter: ([^\n]+)", user))[[1]]
        pc <- if (length(pc)) pc[[2]] else ""
        tg <- targets(user)
        lapply(tg, function(t) as.list((rows %||% function(p, t) character())(pc, t)))
        }
      },
      extract_check = (check %||% function(user) list(cut = list()))(user),
      extract_cycles = (cycles %||% function(user) list(cut = list(), conflict = list()))(user),
      stop("Unknown task: ", task))
    if (is.character(reply)) return(list(text = reply, model = "script"))
    list(text = json_text(reply), model = "script")
  }
  list(fn = fn, log = log)
}

xl_with <- function(script, code) {
  old <- get("llm_chat", envir = .GlobalEnv)
  assign("llm_chat", script$fn, envir = .GlobalEnv)
  on.exit(assign("llm_chat", old, envir = .GlobalEnv), add = TRUE)
  force(code)
}

xl_pairs <- function(g) vapply(g$edges, function(e) paste(e$from, e$to, sep = "->"), "")
xl_ext <- function(g) unlist(lapply(g$external, function(x) paste(x$ref, flat_chr(x$used_by), sep = "->")))
xl_tasks <- function(s) vapply(s$log$calls, function(x) x$task, "")

xl_items <- function(p, t) switch(t,
  AVAL = if (identical(p, "P1")) "VS.VSSTRESN" else "ADT",
  ADT = "VS.VSDTC", BASE = "AVAL", CHG = c("AVAL", "BASE"), STUDYID = "ADSL.STUDYID", PARAMCD = "VS.VSTESTCD",
  P1 = "VS.VSTESTCD", P2 = "VS.VSTESTCD", character())

test_that("each parameter gets its own edges and the Source column isn't spread over layers", {
  s <- xl_script(rows = xl_items)
  g <- xl_with(s, extract_dependencies(list(model = "normal"), xl_spec(), "ADX"))
  pairs <- xl_pairs(g)
  expect_true("ADX.ADT@P2->ADX.AVAL@P2" %in% pairs)
  expect_false("ADX.ADT@P1->ADX.AVAL@P1" %in% pairs)
  expect_true("ADX.BASE@P2->ADX.CHG@P2" %in% xl_pairs(g))
  expect_false("VS.VSSTRESN->ADX.AVAL@P2" %in% xl_ext(g))
  expect_true("ADSL.STUDYID->ADX.STUDYID" %in% xl_ext(g))
  expect_true("VS.VSTESTCD->ADX.RECORDS@P1" %in% xl_ext(g))
  expect_true("ADX.RECORDS@P1->ADX.AVAL@P1" %in% pairs)
  evidence <- Filter(function(e) identical(e$to, "ADX.CHG@P1") && identical(e$from, "ADX.BASE@P1"), g$edges)[[1]]$evidence
  expect_identical(evidence, "AVAL minus BASE.")
  expect_true(isTRUE(g$extraction$no_source_spread))
  # A plan, the rows of both layers plus dataset-level rows, and a check.
  expect_identical(table(xl_tasks(s))[["extract_plan"]], 1L)
  expect_identical(table(xl_tasks(s))[["extract_check"]], 1L)
  expect_false("extract_cycles" %in% xl_tasks(s))
})

test_that("the strong model plans and checks, the Settings model reads rows, and a blank strong model falls back", {
  s <- xl_script(rows = xl_items)
  xl_with(s, extract_dependencies(list(model = "normal", strong_model = "big"), xl_spec(), "ADX"))
  by_task <- split(vapply(s$log$calls, function(x) x$model, ""), xl_tasks(s))
  expect_setequal(by_task$extract_plan, "big")
  expect_setequal(by_task$extract_check, "big")
  expect_setequal(by_task$extract_rows, "normal")
  s2 <- xl_script(rows = xl_items, check = function(u) list(cut = list()))
  xl_with(s2, extract_dependencies(list(model = "normal", strong_model = ""), xl_spec(), "ADX"))
  expect_setequal(vapply(s2$log$calls, function(x) x$model, ""), "normal")
})

test_that("config keeps the strong model, with a blank default", {
  home <- tempfile("xl-home-"); dir.create(home)
  old <- Sys.getenv("DUALDAG_HOME"); Sys.setenv(DUALDAG_HOME = home)
  on.exit(Sys.setenv(DUALDAG_HOME = old), add = TRUE)
  expect_identical(load_config()$strong_model, "")
  cfg <- load_config(); cfg$strong_model <- "big"; save_config(cfg)
  expect_identical(load_config()$strong_model, "big")
  expect_identical(config_public(load_config())$strong_model, "big")
})

test_that("the check only cuts: ids it names go, unknown ids are reported, key edges are labelled", {
  seen <- NULL
  s <- xl_script(rows = xl_items, check = function(user) {
    seen <<- user
    lines <- strsplit(user, "\n", fixed = TRUE)[[1]]
    id <- sub(" .*", "", grep("ADX.BASE@P1 -> ADX.CHG@P1", lines, fixed = TRUE, value = TRUE))
    ext <- sub(" .*", "", grep("VS.VSDTC -> ADX.ADT@P2", lines, fixed = TRUE, value = TRUE))
    list(cut = list(id, ext, "E9999"))
  })
  g <- xl_with(s, extract_dependencies(list(), xl_spec(), "ADX"))
  expect_false("ADX.BASE@P1->ADX.CHG@P1" %in% xl_pairs(g))
  expect_true("ADX.BASE@P2->ADX.CHG@P2" %in% xl_pairs(g))
  expect_false("VS.VSDTC->ADX.ADT@P2" %in% xl_ext(g))
  expect_true("VS.VSDTC->ADX.ADT@P1" %in% xl_ext(g))
  expect_match(paste(unlist(g$warnings), collapse = " "), "E9999", fixed = TRUE)
  expect_match(seen, "VS.VSTESTCD -> ADX.RECORDS@P1 (via key variable PARAMCD)", fixed = TRUE)
  # The stored reply matches the graph, so a later key change can't bring a cut edge back.
  again <- merge_extract(graph_skeleton(xl_spec(), "ADX"), g$extraction, xl_spec())
  expect_setequal(xl_pairs(again), xl_pairs(g))
  expect_setequal(xl_ext(again), xl_ext(g))
})

cycle_items <- function(p, t) switch(t, AVAL = "BASE", BASE = "CHG", CHG = "AVAL", character())

test_that("a cycle is cut by the strong model and each cut records the edge and a reason", {
  asked <- NULL
  s <- xl_script(rows = cycle_items, cycles = function(user) {
    asked <<- user
    list(cut = list(list(from = "ADX.BASE@P1", to = "ADX.AVAL@P1", reason = "AVAL doesn't use BASE on P1."),
                    list(from = "ADX.BASE@P2", to = "ADX.AVAL@P2", reason = "AVAL doesn't use BASE on P2.")), conflict = list())
  })
  g <- xl_with(s, extract_dependencies(list(), xl_spec(), "ADX"))
  expect_length(extract_find_cycles(lapply(g$edges, function(e) c(e$from, e$to))), 0L)
  expect_false("ADX.BASE@P1->ADX.AVAL@P1" %in% xl_pairs(g))
  expect_length(g$cycle_cuts, 2L)
  expect_identical(g$cycle_cuts[[1]]$reason, "AVAL doesn't use BASE on P1.")
  expect_identical(g$cycle_cuts[[1]]$from, "ADX.BASE@P1")
  expect_identical(sum(xl_tasks(s) == "extract_cycles"), 1L)
  expect_match(asked, "ADX.CHG@P1 -> ADX.BASE@P1", fixed = TRUE)
  expect_false("ADX.BASE@P1->ADX.AVAL@P1" %in% xl_pairs(merge_extract(graph_skeleton(xl_spec(), "ADX"), g$extraction, xl_spec())))
})

test_that("cutting can't add, reverse or remove structural edges, and a cycle it can't cut stays reported", {
  s <- xl_script(rows = cycle_items, cycles = function(user) list(
    cut = list(list(from = "ADX.AVAL@P1", to = "ADX.BASE@P1", reason = "reversed"),
               list(from = "ADX.ADT@P1", to = "ADX.CHG@P1", reason = "not in the graph"),
               list(from = "ADX.RECORDS@P1", to = "ADX.AVAL@P1", reason = "structural")),
    conflict = list("AVAL and BASE define each other.")))
  sp <- xl_spec()
  before <- xl_with(xl_script(rows = cycle_items, cycles = function(u) list(cut = list(), conflict = list())), extract_dependencies(list(), sp, "ADX"))
  g <- xl_with(s, extract_dependencies(list(), sp, "ADX"))
  expect_setequal(xl_pairs(g), xl_pairs(before))
  expect_length(g$cycle_cuts, 0L)
  expect_identical(sum(xl_tasks(s) == "extract_cycles"), 1L)
  expect_match(paste(unlist(g$warnings), collapse = " "), "AVAL and BASE define each other.", fixed = TRUE)
  issues <- check_graph(g, sp)$issues
  cycle_issues <- Filter(function(i) identical(i$type, "cycle"), issues)
  expect_length(cycle_issues, 2L)
  expect_true(all(vapply(cycle_issues, function(i) identical(issue_severity(i), "error"), TRUE)))
})

test_that("cycle cutting stops after three rounds and reports what is left", {
  rounds <- 0L
  # Every variable uses every other, so one cut per round can't clear the cycles in three rounds.
  vars <- c("AVAL", "ADT", "BASE", "CHG")
  many <- function(p, t) if (t %in% vars) setdiff(vars, t) else character()
  targets <- list(c("ADX.AVAL@P1", "ADX.BASE@P1"), c("ADX.BASE@P1", "ADX.AVAL@P1"), c("ADX.ADT@P1", "ADX.CHG@P1"), c("ADX.CHG@P1", "ADX.ADT@P1"))
  s <- xl_script(rows = many, cycles = function(user) {
    rounds <<- rounds + 1L
    pair <- targets[[rounds]]
    list(cut = list(list(from = pair[[1]], to = pair[[2]], reason = paste("round", rounds))), conflict = list())
  })
  g <- xl_with(s, extract_dependencies(list(), xl_spec(), "ADX", paramcd = "P1"))
  expect_identical(rounds, 3L)
  expect_length(g$cycle_cuts, 3L)
  expect_identical(vapply(g$cycle_cuts, function(x) x$round, 1L), 1:3)
  expect_gt(length(extract_find_cycles(lapply(g$edges, function(e) c(e$from, e$to)))), 0L)
})

test_that("a failing cycle call keeps the graph and warns", {
  s <- xl_script(rows = cycle_items, cycles = function(user) "not json")
  g <- xl_with(s, extract_dependencies(list(), xl_spec(), "ADX"))
  expect_match(paste(unlist(g$warnings), collapse = " "), "The cycle check failed", fixed = TRUE)
  expect_gt(length(extract_find_cycles(lapply(g$edges, function(e) c(e$from, e$to)))), 0L)
  s2 <- xl_script(rows = cycle_items, cycles = function(user) list(conflict = list()))
  g2 <- xl_with(s2, extract_dependencies(list(), xl_spec(), "ADX"))
  expect_match(paste(unlist(g2$warnings), collapse = " "), "no cut list", fixed = TRUE)
})

test_that("failures stop the job instead of leaving a silently thin graph", {
  sp <- xl_spec()
  expect_error(xl_with(xl_script(plan = function(u) list(nothing = TRUE)), extract_dependencies(list(), sp, "ADX")), "no batches")
  expect_error(xl_with(xl_script(plan = function(u) "not json"), extract_dependencies(list(), sp, "ADX")), "valid JSON")
  expect_error(xl_with(xl_script(rows = xl_items, check = function(u) list(keep = list())), extract_dependencies(list(), sp, "ADX")), "not checked")
  expect_error(xl_with(xl_script(rows = xl_items, check = function(u) "not json"), extract_dependencies(list(), sp, "ADX")), "valid JSON")
})

test_that("a row left out or a failed batch is retried alone, and a row never answered stops the job", {
  sp <- xl_spec()
  dropped <- FALSE
  base <- xl_script(rows = xl_items)
  flaky <- list(fn = function(cfg, system, user, ...) {
    r <- base$fn(cfg, system, user, ...)
    if (xl_task(system) == "extract_rows" && !dropped && grepl("Variable: CHG", user, fixed = TRUE)) {
      dropped <<- TRUE
      parsed <- jsonlite::fromJSON(r$text, simplifyVector = FALSE)
      parsed[[grep("V[0-9]+", names(parsed), value = TRUE)[[length(parsed)]]]] <- NULL
      r$text <- json_text(parsed)
    }
    r
  }, log = base$log)
  g <- xl_with(flaky, extract_dependencies(list(), sp, "ADX"))
  expect_true(dropped)
  expect_true("ADX.BASE@P1->ADX.CHG@P1" %in% xl_pairs(g))

  failed <- FALSE
  base2 <- xl_script(rows = xl_items)
  broken <- list(fn = function(cfg, system, user, ...) {
    if (xl_task(system) == "extract_rows" && !failed) { failed <<- TRUE; stop("provider is down") }
    base2$fn(cfg, system, user, ...)
  }, log = base2$log)
  g2 <- xl_with(broken, extract_dependencies(list(), sp, "ADX"))
  expect_true(failed)
  expect_true("ADX.ADT@P2->ADX.AVAL@P2" %in% xl_pairs(g2))

  base3 <- xl_script(rows = xl_items)
  never <- list(fn = function(cfg, system, user, ...) {
    if (xl_task(system) == "extract_rows" && grepl("Variable: CHG", user, fixed = TRUE)) stop("provider is down")
    base3$fn(cfg, system, user, ...)
  }, log = base3$log)
  expect_error(xl_with(never, extract_dependencies(list(), sp, "ADX")), "no result for P1/CHG.*provider is down")
})

test_that("returned row IDs canonicalize safely across normal, grouped, and retry replies", {
  expect_identical(extract_validate_row_reply(list(" row v1 " = list("VS.VSSTRESN")), "V1")$V1, "VS.VSSTRESN")
  expect_identical(extract_validate_row_reply(list("ROW V1" = list(), V1 = list()), "V1")$V1, character())
  expect_error(extract_validate_row_reply(list(V2 = list("x")), "V1"), "Unknown row ID")
  expect_error(extract_validate_row_reply(list(V1 = list("x"), "ROW V1" = list("y")), "V1"), "Conflicting duplicate")
  expect_error(extract_validate_row_reply(list(V1 = list("Derive from baseline")), "V1"), "prose instead of reference")
  expect_error(extract_validate_row_reply(list("ROW nonsense" = list()), "V1"), "Malformed row ID")

  sp <- xl_spec()
  # Captured real reply shape: ROW-prefixed keys in a multi-row grouped reply.
  grouped <- xl_script(rows = xl_items)
  original <- grouped$fn
  grouped$fn <- function(cfg, system, user, ...) {
    r <- original(cfg, system, user, ...)
    if (xl_task(system) == "extract_rows") {
      parsed <- jsonlite::fromJSON(r$text, simplifyVector = FALSE)
      names(parsed) <- paste("ROW", names(parsed))
      r$text <- json_text(parsed)
    }
    r
  }
  g <- xl_with(grouped, extract_dependencies(list(), sp, "ADX", include_dataset_level = FALSE))
  expect_true("ADX.BASE@P1->ADX.CHG@P1" %in% xl_pairs(g))

  retry_base <- xl_script(rows = xl_items)
  first <- TRUE
  retry <- list(log = retry_base$log, fn = function(cfg, system, user, ...) {
    r <- retry_base$fn(cfg, system, user, ...)
    if (xl_task(system) == "extract_rows" && first) {
      first <<- FALSE
      parsed <- jsonlite::fromJSON(r$text, simplifyVector = FALSE)
      names(parsed)[1] <- paste("ROW", names(parsed)[1])
      parsed[[names(parsed)[1]]] <- list("derive by averaging prior values")
      r$text <- json_text(parsed)
    }
    r
  })
  g2 <- xl_with(retry, extract_dependencies(list(), sp, "ADX", paramcd = "P1", include_dataset_level = FALSE))
  expect_true("VS.VSSTRESN->ADX.AVAL@P1" %in% xl_ext(g2))

  carry_base <- xl_script(rows = function(pc, target) if (target == "AVAL") "VS.VSSTRESN" else xl_items(pc,target),
                          classification = list(P1 = "carry_over", P2 = "derived"))
  carry_first <- TRUE
  carry_retry <- list(log = carry_base$log, fn = function(cfg, system, user, ...) {
    r <- carry_base$fn(cfg, system, user, ...)
    if (xl_task(system) == "extract_rows" && grepl("CARRY_OVER_PARAMETERS:", user, fixed = TRUE) && carry_first) {
      carry_first <<- FALSE
      ids <- xl_row_ids(user)
      prose <- setNames(lapply(ids, function(id) if (grepl("Variable: AVAL", user, fixed = TRUE)) list("derive from the carry-over parameter") else list()), paste("ROW", ids))
      r$text <- json_text(prose)
    }
    r
  })
  cg <- xl_with(carry_retry, extract_dependencies(list(), sp, "ADX", include_dataset_level = FALSE))
  expect_true(isTRUE(cg$extraction$no_source_spread))
  grouped_calls <- Filter(function(x) x$task == "extract_rows" && grepl("CARRY_OVER_PARAMETERS:", x$user, fixed = TRUE), carry_retry$log$calls)
  expect_identical(length(grouped_calls), 2L)
  expect_match(grouped_calls[[2]]$user, "RETRY CORRECTION", fixed=TRUE)
  expect_match(grouped_calls[[2]]$user, "prose.*invalid", perl=TRUE)
  expect_match(grouped_calls[[2]]$user, "Resolve these row IDs: V", fixed=TRUE)
  expect_true("VS.VSSTRESN->ADX.AVAL@CARRY_OVER" %in% xl_ext(cg))
  expect_true("ADX.AVAL@CARRY_OVER->ADX.CHG@CARRY_OVER" %in% xl_pairs(cg))
})

test_that("one layer runs only that parameter, and dataset rows only when asked", {
  s <- xl_script(rows = xl_items)
  g <- xl_with(s, extract_dependencies(list(), xl_spec(), "ADX", paramcd = "P1", include_dataset_level = FALSE))
  users <- vapply(Filter(function(x) x$task == "extract_rows", s$log$calls), function(x) x$user, "")
  expect_true(all(startsWith(users, "Parameter: P1")))
  expect_false(any(grepl("Variable: STUDYID", users, fixed = TRUE)))
  model_pairs <- function(g) xl_pairs(list(edges = Filter(function(e) !identical(e$origin, "structural"), g$edges)))
  expect_false(any(grepl("@P2", model_pairs(g), fixed = TRUE)))
  expect_false(any(grepl("STUDYID", xl_ext(g), fixed = TRUE)))
  s2 <- xl_script(rows = xl_items)
  g2 <- xl_with(s2, extract_dependencies(list(), xl_spec(), "ADX", paramcd = "P2", include_dataset_level = TRUE))
  expect_true("ADSL.STUDYID->ADX.STUDYID" %in% xl_ext(g2))
  expect_error(xl_with(xl_script(), extract_dependencies(list(), xl_spec(), "ADX", paramcd = "NOPE")), "Unknown parameter layer")
})

test_that("generating layers one by one reuses the plan until the spec changes", {
  dir <- tempfile("xl-proj-"); dir.create(dir)
  sp <- xl_spec()
  s <- xl_script(rows = xl_items)
  xl_with(s, extract_dependencies(list(), sp, "ADX", project = dir, paramcd = "P1"))
  xl_with(s, extract_dependencies(list(), sp, "ADX", project = dir, paramcd = "P2"))
  expect_identical(sum(xl_tasks(s) == "extract_plan"), 1L)
  sp$hash <- "another-spec"
  xl_with(s, extract_dependencies(list(), sp, "ADX", project = dir, paramcd = "P1"))
  expect_identical(sum(xl_tasks(s) == "extract_plan"), 2L)
})

test_that("classification is validated, cached by spec hash, and cannot-tell is a derived spec issue", {
  sp <- xl_spec()
  dir <- tempfile("xl-classification-"); dir.create(dir)
  s <- xl_script(classification=list(P1="cannot_tell", P2="carry_over"))
  one <- xl_with(s, extract_classification(list(model="normal",strong_model="strong"),sp,"ADX",dir))
  expect_identical(vapply(one$parameters,`[[`,"","classification"),c("derived","carry_over"))
  expect_match(one$issues[[1]],"P1: Scripted reason",fixed=TRUE)
  xl_with(s, extract_classification(list(model="normal",strong_model="strong"),sp,"ADX",dir))
  expect_identical(sum(xl_tasks(s)=="extract_classification"),1L)
  sp$hash <- "changed"
  xl_with(s, extract_classification(list(model="normal",strong_model="strong"),sp,"ADX",dir))
  expect_identical(sum(xl_tasks(s)=="extract_classification"),2L)
  bad <- xl_script(classification=list(P1="derived"))
  # Override the reply to omit P2; incomplete coverage must stop the pass.
  old <- bad$fn
  bad$fn <- function(cfg,system,user,...) { if (xl_task(system)=="extract_classification") return(list(text=json_text(list(parameters=list(list(paramcd="P1",classification="derived",reason="one")))),model="script")); old(cfg,system,user,...) }
  expect_error(xl_with(bad,extract_classification(list(),sp,"ADX")),"exactly once")
  duplicate <- xl_script()
  duplicate$fn <- function(cfg,system,user,...) { if (xl_task(system)=="extract_classification") return(list(text=json_text(list(parameters=list(list(paramcd="P1",classification="derived",reason="one"),list(paramcd="P1",classification="derived",reason="two")))),model="script")); duplicate_old(cfg,system,user,...) }
  duplicate_old <- xl_script()$fn
  expect_error(xl_with(duplicate,extract_classification(list(),sp,"ADX")),"exactly once")
})

test_that("carry-over parameters share one normal-model extraction call and one shared layer", {
  sp <- xl_spec()
  calls <- xl_script(classification=list(P1="carry_over",P2="carry_over"), rows=function(pc,t) if(t=="AVAL") if(pc=="P1")"BASE" else "ADT" else character())
  g <- xl_with(calls,extract_dependencies(list(model="normal",strong_model="strong"),sp,"ADX",include_dataset_level=FALSE))
  row_calls <- Filter(function(x)x$task=="extract_rows",calls$log$calls)
  group_calls <- Filter(function(x)grepl("CARRY_OVER_PARAMETERS:",x$user,fixed=TRUE),row_calls)
  expect_identical(length(group_calls),1L)
  expect_identical(group_calls[[1]]$model,"normal")
  expect_identical(sum(xl_tasks(calls)=="extract_classification"),1L)
  expect_identical(sum(xl_tasks(calls)=="extract_plan"),1L)
  expect_identical(sum(xl_tasks(calls)=="extract_check"),1L)
  expect_true("ADX.BASE@CARRY_OVER->ADX.AVAL@CARRY_OVER" %in% xl_pairs(g))
  expect_true("ADX.ADT@CARRY_OVER->ADX.AVAL@CARRY_OVER" %in% xl_pairs(g))
  expect_true(grepl("P1",group_calls[[1]]$user,fixed=TRUE) && grepl("P2",group_calls[[1]]$user,fixed=TRUE))
  group_prompt <- extract_rows_prompt("ADX", "", TRUE, grouped=TRUE)
  expect_match(group_prompt, "single grouped request", fixed=TRUE)
  expect_match(group_prompt, "ONLY the union of dependency reference tokens", fixed=TRUE)
  expect_match(group_prompt, "qualify it as VAR@PARAMCD", fixed=TRUE)
  expect_match(group_prompt, "Do not return derivation logic", fixed=TRUE)
  expect_false(grepl("member-specific rules", group_prompt, fixed=TRUE))
  expect_identical(vapply(g$layers, `[[`, "", "paramcd")[1], "CARRY_OVER")
  expect_false(any(grepl("@(P1|P2)$", vapply(g$nodes, `[[`, "", "id"), perl=TRUE)))
})

test_that("classification prompt contains only the allowed spec descriptions", {
  sp <- xl_spec()
  prompt <- extract_classification_prompt("ADX",sp,"")
  expect_identical(prompt$system, paste0("TASK: extract_classification\n",
    "Classify each parameter named in PARAMCD_LIST as carry_over or derived, and give a one-sentence reason. You may answer cannot_tell. SRCDOM, SRCVAR and SRCSEQ are spec variables given as information, not items to classify. Return JSON only: {\"parameters\":[{\"paramcd\":\"\",\"classification\":\"carry_over|derived|cannot_tell\",\"reason\":\"\"}]}"))
  expect_true(nzchar(prompt$user))
  expect_true(grepl("PARAMCD_LIST: P1, P2",prompt$user,fixed=TRUE))
  expect_true(grepl("SRCDOM_SRCVAR_SRCSEQ_SPEC",prompt$user,fixed=TRUE))
  expect_true(grepl("PARAMETER_DESCRIPTIONS",prompt$user,fixed=TRUE))
  expect_false(grepl("SRCDOM_SRCVAR_SRCSEQ_SPEC",prompt$system,fixed=TRUE))
  expect_false(grepl("PARAMETER_DESCRIPTIONS",prompt$system,fixed=TRUE))
  expect_false(grepl("P1|P2|Source text|Derivation text",prompt$system,perl=TRUE))
  expect_false(grepl("decision rule|for each field|example",prompt$system,ignore.case=TRUE))
})

test_that("an incoming Parameter DAG edge to carry-over is surfaced as an issue", {
  dir <- tempfile("xl-carry-edge-"); dir.create(file.path(dir,".dualdag","state"),recursive=TRUE)
  sp <- xl_spec()
  write_json(list(dataset="ADX",nodes=list(),edges=list(list(from="P1",to="P2")),warnings=list()),param_graph_path(dir,"ADX"))
  s <- xl_script(classification=list(P1="derived",P2="carry_over"))
  g <- xl_with(s,extract_dependencies(list(),sp,"ADX",project=dir,paramcd="P2"))
  expect_match(paste(unlist(g$warnings),collapse=" "),"carry-over parameter P2 has an incoming edge from P1",fixed=TRUE)
})

test_that("cross-parameter items stay out of layer edges, and a gap with the Parameter DAG is a warning", {
  dir <- tempfile("xl-proj-"); dir.create(file.path(dir, ".dualdag", "state"), recursive = TRUE)
  sp <- xl_spec()
  write_json(list(dataset = "ADX", nodes = list(), edges = list(), warnings = list()), param_graph_path(dir, "ADX"))
  items <- function(p, t) if (identical(t, "AVAL") && identical(p, "P2")) c("AVAL@P1", "BASE@P1", "VS.VSSEQ@P1") else character()
  g <- xl_with(xl_script(rows = items), extract_dependencies(list(), sp, "ADX", project = dir, paramcd = "P2"))
  expect_false(any(grepl("@P1", xl_pairs(list(edges = Filter(function(e) !identical(e$origin, "structural"), g$edges))), fixed = TRUE)))
  expect_true("VS.VSSEQ->ADX.AVAL@P2" %in% xl_ext(g))
  expect_identical(g$param_uses[[1]]$from, "P1")
  expect_identical(g$param_uses[[1]]$uses[[2]]$from_var, "BASE")
  expect_match(paste(unlist(g$warnings), collapse = " "), "no P1 -> P2 link", fixed = TRUE)
  write_json(list(dataset = "ADX", nodes = list(), edges = list(list(from = "P1", to = "P2")), warnings = list()), param_graph_path(dir, "ADX"))
  g2 <- xl_with(xl_script(rows = items), extract_dependencies(list(), sp, "ADX", project = dir, paramcd = "P2"))
  expect_false(any(grepl("no P1 -> P2 link", unlist(g2$warnings), fixed = TRUE)))
})

test_that("stored replies keep other layers when one layer is regenerated", {
  old <- list(edges = list(list(from = "A", to = "B", params = list("P1")), list(from = "C", to = "D", params = list("P2")), list(from = "E", to = "F")),
              external = list(list(ref = "VS.X", used_by = list("A"), params = list("P1"))), param_uses = list(list(from = "P1", to = "P2")), unresolved = list())
  new <- list(edges = list(list(from = "G", to = "H", params = list("P1"))), external = list(), param_uses = list(), unresolved = list(), layer_keys = list(P1 = "K"))
  out <- extract_merge_replies(old, new, "P1", FALSE)
  expect_setequal(vapply(out$edges, function(e) e$from, ""), c("C", "E", "G"))
  expect_length(out$external, 0L)
  expect_length(out$param_uses, 1L)
  expect_setequal(vapply(extract_merge_replies(old, new, "P1", TRUE)$edges, function(e) e$from, ""), c("C", "G"))
  expect_identical(extract_merge_replies(NULL, new, "P1", TRUE), new)
})

test_that("a dataset with no parameters runs the same flow on dataset nodes", {
  sp <- xl_spec(params = FALSE)
  s <- xl_script(rows = function(p, t) switch(t, AVAL = "ADT", ADT = "VS.VSDTC", character()))
  g <- xl_with(s, extract_dependencies(list(), sp, "ADX"))
  expect_false(isTRUE(g$layered))
  expect_true("ADX.ADT->ADX.AVAL" %in% xl_pairs(g))
  expect_true("VS.VSDTC->ADX.ADT" %in% xl_ext(g))
  users <- vapply(Filter(function(x) x$task == "extract_rows", s$log$calls), function(x) x$user, "")
  expect_false(any(startsWith(users, "Parameter:")))
})

test_that("the job map works with forks and without them, as on Windows", {
  fun <- function(x) x * 2L
  expect_identical(unlist(extract_map(as.list(1:5), fun, cores = 1L, os = "unix")), c(2L, 4L, 6L, 8L, 10L))
  expect_identical(unlist(extract_map(as.list(1:5), fun, cores = 4L, os = "windows")), c(2L, 4L, 6L, 8L, 10L))
  skip_on_os("windows")
  expect_identical(unlist(extract_map(as.list(1:5), fun, cores = 2L, os = "unix")), c(2L, 4L, 6L, 8L, 10L))
  pids <- unlist(extract_map(as.list(1:3), function(x) Sys.getpid(), cores = 2L, os = "unix"))
  expect_gt(length(unique(pids)), 1L)
})

test_that("the whole flow runs on the Windows path, and with forks, giving the same graph", {
  sp <- xl_spec()
  on.exit(options(dualdag.extract_cores = 1L), add = TRUE)
  options(dualdag.extract_cores = 1L)
  one <- xl_with(xl_script(rows = xl_items), extract_dependencies(list(), sp, "ADX"))
  skip_on_os("windows")
  options(dualdag.extract_cores = 2L)
  two <- xl_with(xl_script(rows = xl_items), extract_dependencies(list(), sp, "ADX"))
  expect_setequal(xl_pairs(one), xl_pairs(two))
  expect_setequal(xl_ext(one), xl_ext(two))
})

test_that("the fake model runs the flow from start to finish", {
  old <- Sys.getenv("DUALDAG_FAKE_LLM"); Sys.setenv(DUALDAG_FAKE_LLM = "1")
  on.exit(Sys.setenv(DUALDAG_FAKE_LLM = old), add = TRUE)
  g <- extract_dependencies(list(), xl_spec(), "ADX")
  expect_true("ADX.BASE@P1->ADX.CHG@P1" %in% xl_pairs(g))
  expect_true("VS.VSDTC->ADX.ADT@P2" %in% xl_ext(g))
})

test_that("the row prompt lists other datasets' columns so a bare name can get its dataset", {
  sp <- xl_spec()
  cat <- list(domains = list(DM = list(variables = list(list(name = "RFSTDTC")))))
  s <- xl_script(rows = xl_items)
  xl_with(s, extract_dependencies(list(), sp, "ADX", catalog = cat))
  rows_call <- Filter(function(x) x$task == "extract_rows", s$log$calls)[[1]]
  expect_match(rows_call$system, "CATALOG\tDM\tRFSTDTC", fixed = TRUE)
})

test_that("issues and cycle cuts name their parameter, in one layer and in a whole-dataset run", {
  issue <- list(type = "ambiguous", variables = list("AVAL"), message = "Which value?")
  s <- xl_script(rows = xl_items, check = function(u) list(cut = list(), unresolved = list(issue)))
  one <- xl_with(s, extract_dependencies(list(), xl_spec(), "ADX", paramcd = "P1"))
  expect_identical(one$extraction$unresolved[[1]]$paramcd, "P1")
  expect_identical(one$unresolved[[1]]$paramcd, "P1")
  expect_null(xl_with(s, extract_dependencies(list(), xl_spec(), "ADX"))$unresolved[[1]]$paramcd)
  expect_identical(normalize_issue(list(type = "ambiguous", paramcd = "p1"))$paramcd, "P1")
  expect_false("paramcd" %in% names(normalize_issue(list(type = "ambiguous"))))
  cut <- xl_script(rows = cycle_items, cycles = function(user) list(cut = list(
    list(from = "ADX.BASE@P1", to = "ADX.AVAL@P1", reason = "one"), list(from = "ADX.BASE@P2", to = "ADX.AVAL@P2", reason = "two")), conflict = list()))
  whole <- xl_with(cut, extract_dependencies(list(), xl_spec(), "ADX"))
  expect_identical(vapply(whole$cycle_cuts, function(x) x$paramcd, ""), c("P1", "P2"))
})

test_that("regenerating a layer replaces its own issues, cuts and warnings and keeps the other layers'", {
  iss <- function(msg, pc = NULL) c(list(type = "ambiguous", variables = list("AVAL"), message = msg), if (!is.null(pc)) list(paramcd = pc))
  merged <- extract_merge_issues(list(iss("old P", "P"), iss("old Q", "Q"), iss("dataset"), iss("repeat")), list(iss("new P", "P"), iss("repeat", "P")), "P")
  expect_setequal(vapply(merged, function(x) x$message, ""), c("old Q", "dataset", "new P", "repeat"))
  expect_length(Filter(function(x) x$message == "repeat", merged), 1L)
  expect_length(extract_merge_issues(list(iss("old P", "P")), list(), "P"), 0L)
  expect_setequal(vapply(extract_merge_issues(list(iss("dataset")), list(), "P", TRUE), function(x) x$message, character(1)), character())

  cuts <- list(list(from = "X.A@P", to = "X.B@P", reason = "tagged", paramcd = "P"), list(from = "X.A@P", to = "X.C@P", reason = "from a whole run"),
               list(from = "X.A@Q", to = "X.B@Q", reason = "other", paramcd = "Q"), list(from = "X.D", to = "X.E", reason = "dataset"))
  kept <- extract_merge_cuts(cuts, list(list(from = "X.N@P", to = "X.M@P", reason = "new", paramcd = "P")), "P")
  expect_setequal(vapply(kept, function(x) x$reason, ""), c("other", "dataset", "new"))
  expect_setequal(vapply(extract_merge_cuts(cuts, list(), "P", TRUE), function(x) x$reason, ""), "other")

  by_layer <- extract_merge_warnings(list(P = list("fixed"), Q = list("keep")), list(), "P")
  expect_length(by_layer$P, 0L)
  expect_identical(unlist(by_layer$Q), "keep")

  reply <- extract_merge_replies(list(edges = list(), external = list(), unresolved = list(iss("old P", "P"), iss("old Q", "Q"))),
                                 list(edges = list(), external = list(), unresolved = list()), "P", FALSE)
  expect_identical(vapply(reply$unresolved, function(x) x$message, ""), "old Q")
})

test_that("carry group keys and semantic parameter layer identities remain distinct", {
  d <- tempfile("carry-identity-"); dir.create(d)
  writeLines(c("Dataset,Variable,Label,Type,Length,Key,Source,Derivation",
    "ADX,USUBJID,Subject,Char,20,1,VS.USUBJID,Copy", "ADX,PARAMCD,Code,Char,8,2,VS.VSTESTCD,Copy",
    "ADX,AVAL,Value,Num,8,,VS.VSSTRESN,Copy", "ADX,ADT,Date,Num,8,,VS.VSDTC,Convert"), file.path(d,"variable.csv"))
  writeLines(c("Dataset,PARAMCD,PARAM,Source,Derivation", "ADX,CO1,Carry,VS,Copy",
    "ADX,CARRY_OVER,Real group-like,ADX,Derived", "ADX,PARAM_CARRY_OVER,Real prefixed,ADX,Derived"), file.path(d,"parameter.csv"))
  spec <- read_spec(file.path(d,"variable.csv"),file.path(d,"parameter.csv"))
  graph <- graph_skeleton(spec,"ADX",list(CARRY_OVER=c("USUBJID","PARAMCD","ADT")),"CO1")
  expect_setequal(graph$layer_keys$CARRY_OVER,c("USUBJID","PARAMCD","ADT"))
  expect_length(unique(vapply(graph$layers, `[[`, "", "paramcd")),3L)
  expect_identical(graph$layers[[1]]$paramcd,"CARRY_OVER")
  expect_true(any(vapply(graph$layers,function(x) "CARRY_OVER" %in% x$members_parameters,TRUE)))
  expect_true(any(vapply(graph$layers,function(x) "PARAM_CARRY_OVER" %in% x$members_parameters,TRUE)))
})

