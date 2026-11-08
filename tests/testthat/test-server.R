app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
if (!exists("create_app", mode = "function", inherits = TRUE)) {
  for (f in c("utils.R", "deps.R", "config.R", "spec.R", "project.R", "server.R"))
    sys.source(file.path(app_root, "R", f), envir = globalenv())
}

VAR_HEADER <- "Dataset,Variable,Label,Type,Length,Key,Source,Derivation"

isolate_home <- function() {
  old <- Sys.getenv(c("DUALDAG_HOME", "DUALDAG_LLM_API_KEY", "DUALDAG_FAKE_LLM"))
  home <- tempfile("dualdag-home-")
  dir.create(home)
  Sys.setenv(DUALDAG_HOME = home, DUALDAG_LLM_API_KEY = "", DUALDAG_FAKE_LLM = "")
  list(home = home, restore = function() {
    restore_test_env("DUALDAG_HOME", old[["DUALDAG_HOME"]])
    Sys.setenv(DUALDAG_LLM_API_KEY = old[["DUALDAG_LLM_API_KEY"]],
               DUALDAG_FAKE_LLM = old[["DUALDAG_FAKE_LLM"]])
  })
}

new_app <- function(proj = NULL) {
  if (is.null(proj)) {
    proj <- tempfile("dualdag-proj-")
    dir.create(proj)
  }
  create_app(app_root, proj, 7979L, "test-token", load_config())
}

call_app <- function(A, path, method = "GET", body = NULL, token = A$token, host = NULL, origin = NULL) {
  q <- ""
  if (grepl("?", path, fixed = TRUE)) {
    q <- sub("^[^?]*\\?", "", path)
    path <- sub("\\?.*$", "", path)
  }
  raw <- if (is.null(body)) raw() else charToRaw(as.character(to_json(body)))
  req <- list(
    PATH_INFO = path, REQUEST_METHOD = method, QUERY_STRING = q,
    HTTP_HOST = if (is.null(host)) sprintf("127.0.0.1:%d", A$port) else host,
    HTTP_X_DUALDAG_TOKEN = if (is.null(token)) "" else token,
    CONTENT_TYPE = if (identical(method, "POST")) "application/json" else "",
    rook.input = list(read = function() raw)
  )
  if (!is.null(origin)) req$HTTP_ORIGIN <- origin
  make_handlers(A)$call(req)
}

resp_json <- function(res) from_json(res$body)

adsl_body <- function() {
  csv <- paste(c(VAR_HEADER, "adsl,usubjid,Subject,char,20,1,DM.USUBJID,copy"), collapse = "\n")
  list(variable = list(name = "variable.csv", base64 = jsonlite::base64_enc(charToRaw(csv))))
}

tiny_xpt_b64 <- function() {
  df <- data.frame(USUBJID = "01", AGE = 40, stringsAsFactors = FALSE)
  f <- tempfile(fileext = ".xpt")
  haven::write_xpt(df, f, version = 5, name = "DM")
  jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size))
}

test_that("removed dataset expert routes leave old thread files untouched", {
  project <- tempfile("retired-dataset-expert-")
  dir.create(project)
  A <- create_app(app_root, project, 7989L, "retired-token", load_config())
  legacy <- file.path(project, ".dualdag", "experts", "ADSL_dataset.json")
  write_json(list(list(question = "old question", answer = "old answer")), legacy)
  expect_equal(call_app(A, "/api/dataset/ask", "POST", list(dataset = "ADSL", question = "new"))$status, 404L)
  expect_equal(call_app(A, "/api/dataset/clear", "POST", list(dataset = "ADSL"))$status, 404L)
  expect_true(file.exists(legacy))
  expect_equal(length(read_json(legacy)), 1L)
  write_json(list(dataset = "ADSL", nodes = list(), groups = list(), edges = list(),
                  external = list(), unresolved = list()), graph_path(A$project, "ADSL"))
  expect_null(state_payload(A)$datasets$ADSL$dataset_expert)
})

test_that("/api/state requires the token and returns the state shape", {
  box <- isolate_home()
  on.exit(box$restore(), add = TRUE)
  A <- new_app()
  denied <- call_app(A, "/api/state", token = "")
  expect_equal(denied$status, 403L)
  expect_match(resp_json(denied)$error, "Forbidden")
  bad_host <- call_app(A, "/api/state", host = "evil.example")
  expect_equal(bad_host$status, 403L)

  ok <- call_app(A, "/api/state")
  expect_equal(ok$status, 200L)
  body <- resp_json(ok)
  expect_true(all(c("project", "spec", "data", "missing_data", "datasets", "issues", "config_public", "busy") %in% names(body)))
  expect_null(body$spec)
  expect_false("api_key" %in% names(body$config_public))

  page <- call_app(A, "/")
  if (file.exists(file.path(app_root, "www", "index.html"))) {
    expect_equal(page$status, 200L)
    expect_match(paste(rawToChar(page$body), collapse = ""), "html", ignore.case = TRUE)
  } else {
    expect_equal(page$status, 404L)
  }
})

test_that("QC document API routes remain available", {
  box <- isolate_home()
  on.exit(box$restore(), add = TRUE)
  sys.source(file.path(app_root, "R", "qc.R"), envir = globalenv())
  A <- new_app()
  text <- "# ADSL QC methods"
  uploaded <- call_app(A, "/api/qc/upload", "POST", list(
    dataset = "ADSL", name = "ADSL.md", base64 = jsonlite::base64_enc(charToRaw(text))
  ))
  expect_equal(uploaded$status, 200L)
  listed <- resp_json(call_app(A, "/api/qc"))
  expect_equal(listed[[1]]$dataset, "ADSL")
  viewed <- resp_json(call_app(A, "/api/qc/get?dataset=ADSL"))
  expect_equal(viewed$markdown, text)
  expect_equal(call_app(A, "/api/qc/upload", "POST", list(
    dataset = "ADSL", name = "ADSL.md", base64 = jsonlite::base64_enc(charToRaw(text))
  ))$status, 409L)
})

test_that("/api/data/upload accepts a 30 MB XPT through JSON base64", {
  box <- isolate_home()
  on.exit(box$restore(), add = TRUE)
  A <- new_app()
  df <- as.data.frame(replicate(16, rep(paste(rep("x", 1800), collapse = ""), 1100), simplify = FALSE))
  names(df) <- paste0("V", seq_len(ncol(df)))
  xpt <- tempfile(fileext = ".xpt")
  on.exit(unlink(xpt), add = TRUE)
  haven::write_xpt(df, xpt, version = 5, name = "BIG")
  bytes <- readBin(xpt, "raw", file.info(xpt)$size)
  expect_gte(length(bytes), 30 * 1024^2)
  res <- call_app(A, "/api/data/upload", "POST", list(name = "big.xpt", base64 = jsonlite::base64_enc(bytes)))
  expect_equal(res$status, 200L)
  expect_true(file.exists(file.path(A$project$dir, "data", "big.xpt")))
})

test_that("uploading a spec and an xpt through the API persists across restart", {
  box <- isolate_home()
  on.exit(box$restore(), add = TRUE)
  proj <- tempfile("dualdag-proj-")
  dir.create(proj)
  A <- new_app(proj)
  up <- call_app(A, "/api/spec/upload", "POST", adsl_body())
  expect_equal(up$status, 200L)
  expect_true(isTRUE(resp_json(up)$ok))
  dat <- call_app(A, "/api/data/upload", "POST", list(name = "dm.xpt", base64 = tiny_xpt_b64()))
  expect_equal(dat$status, 200L)
  expect_true(file.exists(file.path(proj, "spec", "variable.csv")))
  expect_true(file.exists(file.path(proj, "data", "dm.xpt")))

  A2 <- new_app(proj)
  body <- resp_json(call_app(A2, "/api/state"))
  expect_equal(body$spec$datasets, list("ADSL"))
  expect_equal(body$data[[1]]$domain, "DM")
  expect_true("USUBJID" %in% unlist(body$data[[1]]$vars))
})

test_that("uploading a spec doesn't start extraction, and the user can start it", {
  box <- isolate_home()
  on.exit(box$restore(), add = TRUE)
  Sys.setenv(DUALDAG_LLM_API_KEY = "sk-test", DUALDAG_FAKE_LLM = "1")
  proj <- tempfile("dualdag-proj-")
  dir.create(proj)
  A <- new_app(proj)
  A$cfg$provider <- "openai"; A$cfg$model <- "test-model"
  started <- 0L
  old <- get("launch", envir = .GlobalEnv)
  assign("launch", function(...) { started <<- started + 1L; promises::promise(function(resolve, reject) resolve(list())) }, envir = .GlobalEnv)
  on.exit(assign("launch", old, envir = .GlobalEnv), add = TRUE)
  up <- call_app(A, "/api/spec/upload", "POST", adsl_body())
  expect_equal(up$status, 200L)
  expect_identical(started, 0L)
  expect_length(A$jobs, 0L)
  body <- resp_json(call_app(A, "/api/state"))
  expect_length(body$busy, 0L)
  expect_null(load_graph(A$project, "ADSL"))
})

test_that("Settings saves the strong model and leaves the Settings model alone", {
  box <- isolate_home()
  on.exit(box$restore(), add = TRUE)
  A <- new_app()
  res <- call_app(A, "/api/config", "POST", list(provider = "openai", model = "small", strong_model = "big"))
  expect_equal(res$status, 200L)
  expect_identical(resp_json(res)$config$strong_model, "big")
  expect_identical(load_config()$model, "small")
  expect_identical(load_config()$strong_model, "big")
  call_app(A, "/api/config", "POST", list(strong_model = ""))
  expect_identical(load_config()$strong_model, "")
})

test_that("config save keeps the API key out of config.json and modes the key file 0600", {
  box <- isolate_home()
  on.exit(box$restore(), add = TRUE)
  A <- new_app()
  res <- call_app(A, "/api/config", "POST", list(provider = "openai", model = "demo", api_key = "sk-test-secret"))
  expect_equal(res$status, 200L)
  cfg_txt <- readLines(file.path(box$home, "config.json"), warn = FALSE)
  expect_false(any(grepl("sk-test-secret", cfg_txt)))
  expect_false(any(grepl("api_key", cfg_txt)))
  mode <- file.info(file.path(box$home, "api_key"))$mode
  expect_equal(bitwAnd(as.integer(mode), 511L), 384L)
  pub <- resp_json(res)$config
  expect_true(isTRUE(pub$api_key_set))
  expect_false(any(grepl("sk-test-secret", paste(unlist(pub), collapse = " "))))
})

test_that("grok_cli is saved without an API key and defaults the model", {
  box <- isolate_home()
  on.exit(box$restore(), add = TRUE)
  A <- new_app()
  res <- call_app(A, "/api/config", "POST", list(provider = "grok_cli", model = ""))
  expect_equal(res$status, 200L)
  pub <- resp_json(res)$config
  expect_equal(pub$provider, "grok_cli")
  expect_equal(pub$model, "grok-4.7")
  expect_false(isTRUE(pub$api_key_set))
  expect_true(isTRUE(llm_ready(A$cfg)))
  bad <- call_app(A, "/api/config", "POST", list(provider = "nope"))
  expect_equal(bad$status, 400L)
})

test_that("extract reports 501 when the extractor is not loaded", {
  skip_if(exists("extract_dependencies", mode = "function", inherits = TRUE), "extractor is loaded")
  box <- isolate_home()
  on.exit(box$restore(), add = TRUE)
  A <- new_app()
  res <- call_app(A, "/api/graph/extract", "POST", list(dataset = "ADSL"))
  expect_equal(res$status, 501L)
})

test_that("an orphan warning does not block generation and Validate route is removed", {
  box <- isolate_home()
  on.exit(box$restore(), add = TRUE)
  if (!exists("check_graph", mode = "function", inherits = TRUE)) {
    for (f in c("llm.R", "deps_extract.R", "dag.R"))
      sys.source(file.path(app_root, "R", f), envir = globalenv())
  }
  proj <- tempfile("dualdag-proj-")
  dir.create(proj)
  A <- new_app(proj)
  csv <- paste(c(
    VAR_HEADER,
    "ADSL,USUBJID,Subject,char,20,1,DM.USUBJID,copy",
    "ADSL,CONST,Constant,num,8,,,constant 1",
    "ADSL,A,A,num,8,,,from B",
    "ADSL,B,B,num,8,,,from A"
  ), collapse = "\n")
  up <- call_app(A, "/api/spec/upload", "POST", list(variable = list(name = "variable.csv", base64 = jsonlite::base64_enc(charToRaw(csv)))))
  expect_equal(up$status, 200L)
  graph <- list(
    dataset = "ADSL",
    nodes = list(
      list(id = "ADSL.USUBJID", var = "USUBJID", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.CONST", var = "CONST", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.A", var = "A", paramcd = NULL, kind = "variable"),
      list(id = "ADSL.B", var = "B", paramcd = NULL, kind = "variable")
    ),
    edges = list(
      list(from = "ADSL.A", to = "ADSL.B", evidence = "cycle", origin = "user"),
      list(from = "ADSL.B", to = "ADSL.A", evidence = "cycle", origin = "user")
    ),
    external = list(list(
      ref = "DM.USUBJID", domain = "DM", var = "USUBJID",
      used_by = list("ADSL.USUBJID"), evidence = "copy"
    )),
    groups = list(), unresolved = list(), confirmed = FALSE, confirmed_hash = NULL
  )
  save_graph(A$project, graph)

  expect_equal(call_app(A, "/api/node/validate", "POST", list(dataset = "ADSL", node = "ADSL.CONST"))$status, 404L)
  expect_identical(node_actions(get_status(A$project, "ADSL", "ADSL.CONST")), "generate")
  legacy <- get_status(A$project, "ADSL", "ADSL.CONST")
  legacy$problem <- list(stage = "validate", message = "legacy refusal")
  put_status(A$project, "ADSL", "ADSL.CONST", legacy)
  expect_identical(node_detail(A$project, "ADSL", "ADSL.CONST")$status$problem$message, "legacy refusal")
  prepared <- prepare_generate_status(A$project, "ADSL", "ADSL.CONST", graph)
  expect_identical(prepared$level, "none")
  expect_null(prepared$problem)

  st <- resp_json(call_app(A, "/api/state"))
  orphans <- Filter(function(x) identical(x$type, "orphan"), st$issues)
  expect_true(length(orphans) >= 1)
  expect_true(any(vapply(orphans, function(x) {
    identical(x$severity, "warning") && any(grepl("CONST", unlist(x$variables)))
  }, logical(1))))
  expect_true(any(vapply(st$issues, function(x) identical(x$type, "cycle") && identical(x$severity, "error"), logical(1))))
})

test_that("a generation waiting for both side outputs uses level-free wording", {
  box <- isolate_home()
  on.exit(box$restore(), add = TRUE)
  proj <- tempfile("dualdag-generate-wording-")
  dir.create(proj)
  A <- new_app(proj)
  st <- apply_generate_results(A$project, "ADSL", "ADSL.X", list())
  expect_identical(st$level, "none")
  expect_identical(st$problem$message, "Both sides must be generated and run.")
})

test_that("a live run.R answers curl for / and /api/state", {
  skip_if(Sys.which("curl") == "", "curl is not installed")
  proj <- tempfile("dualdag-live-")
  home <- tempfile("dualdag-live-home-")
  urlf <- tempfile("dualdag-url-")
  dir.create(proj)
  dir.create(home)
  err <- tempfile("dualdag-err-")
  bg <- callr::r_bg(function(app_dir, project, url_file, home) {
    Sys.setenv(DUALDAG_NO_BROWSER = "1", DUALDAG_URL_FILE = url_file,
               DUALDAG_PROJECT = project, DUALDAG_HOME = home,
               DUALDAG_LLM_API_KEY = "", DUALDAG_FAKE_LLM = "")
    setwd(app_dir)
    sys.source(file.path(app_dir, "run.R"), envir = globalenv())
  }, args = list(app_dir = app_root, project = proj, url_file = urlf, home = home),
     stderr = err, supervise = TRUE)
  on.exit(try(bg$kill(), silent = TRUE), add = TRUE)
  url <- ""
  for (i in seq_len(40)) {
    if (file.exists(urlf)) {
      url <- paste(readLines(urlf, warn = FALSE), collapse = "")
      if (nzchar(url)) break
    }
    if (!bg$is_alive()) break
    Sys.sleep(0.25)
  }
  if (!nzchar(url)) {
    msg <- if (file.exists(err)) paste(readLines(err, warn = FALSE), collapse = "\n") else ""
    fail(paste("server did not print a URL", msg))
  }
  base <- sub("#.*$", "", url)
  token <- sub("^.*#token=", "", url)
  code_of <- function(args) {
    r <- processx::run("curl", c("-sS", "-o", tempfile(), "-w", "%{http_code}", args), error_on_status = FALSE)
    r$stdout
  }
  expect_equal(code_of(base), "200")
  expect_equal(code_of(paste0(base, "api/state")), "403")
  expect_equal(code_of(c("-H", paste0("X-Dualdag-Token: ", token), paste0(base, "api/state"))), "200")
})

test_that("codex_cli keeps an empty model and cli_use_login defaults to true", {
  box <- isolate_home()
  on.exit(box$restore(), add = TRUE)
  writeLines('{"provider":"openai","model":"kept"}', file.path(box$home, "config.json"))
  loaded <- load_config()
  expect_true(isTRUE(loaded$cli_use_login))
  expect_equal(loaded$model, "kept")
  writeLines('{"provider":"claude_cli","model":"claude-sonnet-5-5","cli_use_login":false}', file.path(box$home, "config.json"))
  expect_true(isTRUE(load_config()$cli_use_login))
  A <- new_app()
  res <- call_app(A, "/api/config", "POST", list(provider = "codex_cli", model = "", cli_use_login = FALSE))
  expect_equal(res$status, 200L)
  pub <- resp_json(res)$config
  expect_equal(pub$provider, "codex_cli")
  expect_equal(pub$model, "")
  expect_true(isTRUE(pub$cli_use_login))
  expect_true(isTRUE(llm_ready(A$cfg)))
  again <- call_app(A, "/api/config", "POST", list(provider = "codex_cli", model = "gpt-5.4"))
  expect_equal(resp_json(again)$config$model, "gpt-5.4")
  expect_true(isTRUE(load_config()$cli_use_login))
  bad <- call_app(A, "/api/llm/cli_status?provider=nope")
  expect_equal(bad$status, 400L)
})

test_that("cli_status over HTTP parses fake CLIs and does not echo account fields", {
  if (!exists("cli_status", mode = "function", inherits = TRUE)) {
    sys.source(file.path(app_root, "R", "llm.R"), envir = globalenv())
  }
  box <- isolate_home()
  on.exit(box$restore(), add = TRUE)
  bin <- tempfile("cli-http-")
  dir.create(bin)
  writeLines(c(
    "#!/bin/sh",
    "if [ \"$1\" = \"--version\" ]; then echo \"2.1.316 (Claude Code)\"; exit 0; fi",
    "printf '%s\\n' '{\"loggedIn\":true,\"authMethod\":\"claude.ai\",\"email\":\"SECRETMAIL\",\"orgName\":\"ORGNAME\"}'",
    "exit 0"
  ), file.path(bin, "claude"))
  writeLines(c(
    "#!/bin/sh",
    "if [ \"$1\" = \"--version\" ]; then echo \"codex-cli 0.117.0\"; exit 0; fi",
    "echo \"Logged in using ChatGPT\"",
    "exit 0"
  ), file.path(bin, "codex"))
  writeLines(c("#!/bin/sh", "echo \"grok 9.9.9\"", "exit 0"), file.path(bin, "grok"))
  Sys.chmod(list.files(bin, full.names = TRUE), "0755")
  old_path <- Sys.getenv("PATH")
  on.exit(Sys.setenv(PATH = old_path), add = TRUE)
  Sys.setenv(PATH = paste(bin, old_path, sep = ":"))
  A <- new_app()
  cl <- call_app(A, "/api/llm/cli_status?provider=claude_cli")
  expect_equal(cl$status, 200L)
  body <- resp_json(cl)
  expect_true(isTRUE(body$logged_in))
  expect_equal(body$method, "claude.ai")
  raw <- paste(cl$body, collapse = "")
  expect_false(grepl("SECRETMAIL", raw, fixed = TRUE))
  expect_false(grepl("ORGNAME", raw, fixed = TRUE))
  cx <- resp_json(call_app(A, "/api/llm/cli_status?provider=codex_cli"))
  expect_equal(cx$method, "ChatGPT")
  expect_true(isTRUE(cx$logged_in))
  gk <- call_app(A, "/api/llm/cli_status?provider=grok_cli")
  expect_null(resp_json(gk)$logged_in)
  expect_true(grepl("\"logged_in\"", paste(gk$body, collapse = ""), fixed = TRUE))
  js <- paste(readLines(file.path(app_root, "www", "app.js"), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_match(js, "Codex CLI (ChatGPT login)", fixed = TRUE)
  expect_match(js, "Claude Code CLI (Claude subscription login)", fixed = TRUE)
  expect_match(js, "Uses your ", fixed = TRUE)
  expect_match(js, "login.</p>", fixed = TRUE)
  expect_false(grepl("Use the CLI's own login", js, fixed = TRUE))
  expect_match(js, "/api/llm/cli_status", fixed = TRUE)
  expect_match(js, "(CLI default)", fixed = TRUE)
  expect_match(js, "Check login", fixed = TRUE)
})
