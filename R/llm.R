# LLM adapters (openai / anthropic / openai_compatible / grok_cli / claude_cli / codex_cli) plus an offline fake.
# DUALDAG_FAKE_LLM=1 makes llm_chat return fake_llm_reply() and skip the network.
# Real calls send the key only to the configured provider.

if (!exists("%||%", mode = "function", inherits = TRUE)) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}
if (!exists("now_iso", mode = "function", inherits = TRUE)) {
  now_iso <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
}
if (!exists("provider_default_base_url", mode = "function", inherits = TRUE)) {
  provider_default_base_url <- function(provider) {
    switch(provider %||% "openai",
      openai = "https://api.openai.com/v1",
      anthropic = "https://api.anthropic.com",
      openai_compatible = "",
      "")
  }
}

# Flatten a JSON scalar-or-array into a character vector.
flat_chr <- function(x) {
  if (is.null(x)) return(character())
  x <- unlist(x, use.names = FALSE)
  if (!length(x)) return(character())
  x <- as.character(x)
  x[!is.na(x) & nzchar(x)]
}

write_utf8 <- function(text, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(paste(text %||% ""))), con)
  invisible(path)
}

cli_binary <- function(name, which = Sys.which) {
  path <- unname(which(name))
  if (!length(path) || is.na(path[[1]]) || !nzchar(path[[1]])) "" else as.character(path[[1]])
}

read_utf8 <- function(path) {
  if (!file.exists(path)) return("")
  n <- file.info(path)$size
  if (is.na(n) || n <= 0) return("")
  txt <- rawToChar(readBin(path, "raw", n))
  Encoding(txt) <- "UTF-8"
  txt
}

json_text <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null"))
}

llm_base_url <- function(cfg) {
  b <- cfg$base_url %||% ""
  if (!nzchar(b)) b <- provider_default_base_url(cfg$provider)
  if (!nzchar(b)) stop("Set a base URL for the OpenAI-compatible provider.", call. = FALSE)
  b <- sub("/+$", "", b)
  prov <- cfg$provider %||% "openai"
  # A pasted full endpoint would otherwise be requested twice.
  if (prov %in% c("openai", "openai_compatible")) {
    b <- sub("/chat/completions$", "", b, ignore.case = TRUE)
  } else if (identical(prov, "anthropic")) {
    b <- sub("/v1/messages$", "", b, ignore.case = TRUE)
  }
  sub("/+$", "", b)
}

# Build the httr2 request (no network). Exposed for tests.
llm_request <- function(cfg, system, user, max_tokens = 6000L, json = TRUE) {
  key <- cfg$api_key %||% ""
  if (!nzchar(key)) stop("No API key configured. Open Settings and add your key (or set DUALDAG_LLM_API_KEY).", call. = FALSE)
  if (!nzchar(cfg$model %||% "")) stop("No model name configured. Open Settings and enter a model name.", call. = FALSE)
  prov <- cfg$provider %||% "openai"
  base <- llm_base_url(cfg)
  if (identical(prov, "anthropic")) {
    req <- httr2::request(paste0(base, "/v1/messages"))
    req <- httr2::req_headers(req, `x-api-key` = key, `anthropic-version` = "2023-06-01", .redact = "x-api-key")
    body <- list(model = cfg$model, max_tokens = max_tokens, system = system,
                 messages = list(list(role = "user", content = user)))
  } else if (prov %in% c("openai", "openai_compatible")) {
    req <- httr2::request(paste0(base, "/chat/completions"))
    req <- httr2::req_headers(req, Authorization = paste("Bearer", key), .redact = "Authorization")
    body <- list(model = cfg$model, messages = list(
      list(role = "system", content = system),
      list(role = "user", content = user)))
    if (identical(prov, "openai")) {
      body$max_completion_tokens <- max_tokens
      if (json) body$response_format <- list(type = "json_object")
    } else {
      body$max_tokens <- max_tokens
    }
  } else stop("Unknown provider: ", prov, call. = FALSE)
  req <- httr2::req_body_json(req, body, auto_unbox = TRUE)
  req <- httr2::req_timeout(req, as.numeric(cfg$llm_timeout_s %||% 180))
  req <- httr2::req_user_agent(req, "dualdag")
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  req
}

llm_parse <- function(prov, j) {
  if (identical(prov, "anthropic")) {
    blocks <- Filter(function(b) identical(b$type, "text"), j$content %||% list())
    txt <- paste(vapply(blocks, function(b) b$text %||% "", ""), collapse = "")
    return(list(text = txt, model = j$model %||% NA, usage = j$usage))
  }
  ch <- j$choices %||% list()
  if (!length(ch)) stop("The provider returned no choices.", call. = FALSE)
  list(text = ch[[1]]$message$content %||% "", model = j$model %||% NA, usage = j$usage)
}

llm_redact <- function(x, key) {
  if (is.null(key) || !is.character(key) || length(key) != 1L || !nzchar(key) || nchar(key) < 4L) return(x)
  if (is.list(x)) return(lapply(x, llm_redact, key = key))
  if (!is.character(x)) return(x)
  gsub(key, "[redacted]", x, fixed = TRUE)
}

# Normalize provider accounting without assuming a model context-window table.
llm_usage_summary <- function(usage, provider = "") {
  if (!is.list(usage)) return(NULL)
  number <- function(...) {
    vals <- unlist(list(...), use.names = FALSE)
    vals <- suppressWarnings(as.numeric(vals))
    vals <- vals[is.finite(vals)]
    if (length(vals)) sum(vals) else NULL
  }
  first_number <- function(...) {
    vals <- suppressWarnings(as.numeric(unlist(list(...), use.names=FALSE)))
    vals <- vals[is.finite(vals)]
    if (length(vals)) vals[[1]] else NULL
  }
  prov <- as.character(provider %||% "")[1]
  input <- output <- limit <- NULL
  if (prov %in% c("claude_cli", "anthropic")) {
    input <- number(usage$input_tokens, usage$cache_read_input_tokens, usage$cache_creation_input_tokens)
    output <- number(usage$output_tokens)
  } else if (prov %in% c("openai", "openai_compatible")) {
    input <- number(usage$prompt_tokens); output <- number(usage$completion_tokens)
  } else if (prov == "codex_cli") {
    input <- number(usage$input_tokens); output <- number(usage$output_tokens)
    limit <- number(usage$model_context_window)
  } else if (prov == "grok_cli") {
    input <- first_number(usage$input_tokens, usage$prompt_tokens)
    output <- first_number(usage$output_tokens, usage$completion_tokens)
  } else {
    input <- number(usage$input_tokens, usage$prompt_tokens)
    output <- number(usage$output_tokens, usage$completion_tokens)
    limit <- number(usage$model_context_window)
  }
  if (is.null(input) && is.null(output)) return(NULL)
  list(input = input, output = output, limit = limit)
}

llm_usage_combine <- function(usages) {
  calls <- length(usages %||% list())
  usages <- Filter(is.list, usages %||% list())
  if (!length(usages)) return(if (calls) list(input=NULL,output=NULL,limit=NULL,calls=calls) else NULL)
  inputs <- vapply(usages, function(x) as.numeric(x$input %||% NA_real_)[1], 0.0)
  limits <- vapply(usages, function(x) as.numeric(x$limit %||% NA_real_)[1], 0.0)
  list(input = if (any(is.finite(inputs))) max(inputs, na.rm = TRUE) else NULL,
       output = sum(vapply(usages, function(x) as.numeric(x$output %||% 0)[1], 0.0), na.rm=TRUE),
       limit = if (any(is.finite(limits))) max(limits, na.rm = TRUE) else NULL,
       calls = calls)
}

llm_log_record <- function(cfg, system, user, res) {
  meta <- getOption("dualdag.llm_log")
  if (!is.list(meta) || is.null(meta$dir) || !nzchar(as.character(meta$dir)[1])) return(invisible())
  key <- if (is.list(cfg)) cfg$api_key else NULL
  fs_safe <- function(x) {
    x <- as.character(x %||% "none")[1]
    if (is.na(x) || !nzchar(x)) x <- "none"
    gsub("[^A-Za-z0-9._@-]", "_", x)
  }
  stamp <- gsub("[^0-9T]", "", format(Sys.time(), "%Y%m%dT%H%M%OS3", tz = "UTC"))
  fn <- paste0(stamp, "_", fs_safe(meta$task), "_", fs_safe(meta$node), "_", fs_safe(meta$side), ".json")
  dir.create(meta$dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(meta$dir, fn)
  if (file.exists(path)) path <- file.path(meta$dir, paste0(stamp, "_", sample.int(1000, 1), "_", fs_safe(meta$side), ".json"))
  payload <- list(
    time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    task = meta$task %||% NULL,
    node = meta$node %||% NULL,
    side = meta$side %||% NULL,
    session = meta$session %||% NULL,
    model = res$model %||% NULL,
    system = llm_redact(as.character(system %||% ""), key),
    user = llm_redact(as.character(user %||% ""), key),
    reply = llm_redact(as.character(res$text %||% ""), key),
    usage = llm_redact(res$usage %||% NULL, key)
  )
  normalized <- llm_usage_summary(res$usage %||% NULL, cfg$provider %||% "")
  if (!is.null(normalized) && !is.null(meta$session) && nzchar(as.character(meta$session)[1])) {
    session_dir <- file.path(dirname(meta$dir), "sessions", as.character(meta$session)[1])
    usage_path <- file.path(session_dir, "usage.json")
    totals <- tryCatch(jsonlite::fromJSON(read_utf8(usage_path), simplifyVector = FALSE), error=function(e) list())
    if (!is.list(totals)) totals <- list()
    totals$calls <- as.integer(totals$calls %||% 0L) + 1L
    for (field in c("input", "output")) if (!is.null(normalized[[field]]))
      totals[[field]] <- as.numeric(totals[[field]] %||% 0) + as.numeric(normalized[[field]])
    write_utf8(json_text(totals), usage_path)
    rec_path <- file.path(session_dir, "session.json")
    rec <- tryCatch(jsonlite::fromJSON(read_utf8(rec_path), simplifyVector=FALSE),error=function(e)NULL)
    if (is.list(rec)) { rec$usage_total <- totals; write_utf8(json_text(rec), rec_path) }
  }
  txt <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", pretty = TRUE, force = TRUE)
  con <- file(path, open = "wb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  writeBin(charToRaw(as.character(txt)), con)
  invisible(path)
}

latest_llm_log <- function(project, node, side, task = NULL) {
  dir <- file.path(project, ".dualdag", "llm_log")
  if (!dir.exists(dir)) return(NULL)
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  if (!length(files)) return(NULL)
  suffix <- paste0("_", as.character(node)[1], "_", as.character(side)[1], ".json")
  hit <- files[endsWith(basename(files), suffix)]
  if (!is.null(task)) hit <- hit[grepl(paste0("_", gsub("[^A-Za-z0-9._@-]", "_", as.character(task)[1]), "_"), basename(hit), fixed = TRUE)]
  if (!length(hit)) return(NULL)
  info <- file.info(hit)
  hit <- hit[order(info$mtime, decreasing = TRUE)]
  txt <- paste(readLines(hit[[1]], warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
}

# Local `grok` CLI (grok.com login). No API key. Default model grok-4.7.
grok_cli_models <- function() c("grok-4.7", "grok-4.7-build-fast", "grok-4.6", "grok-4.5")

cli_user_prompt <- function(user, default_notice) {
  notice <- getOption("dualdag.cli_tool_notice")
  if (is.null(notice)) notice <- default_notice
  paste0(as.character(user %||% ""), "\n\n", as.character(notice)[1])
}

grok_cli_chat <- function(cfg, system, user) {
  bin <- cli_binary("grok")
  if (!nzchar(bin)) {
    stop("Grok CLI is not on PATH. Install grok and log in, then try again.", call. = FALSE)
  }
  model <- cfg$model %||% ""
  if (!nzchar(model)) model <- "grok-4.7"
  wd <- tempfile("dualdag-grok-")
  dir.create(wd, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(wd, recursive = TRUE), add = TRUE)
  sys <- as.character(system %||% "")
  usr <- cli_user_prompt(user,
    "You have no tools and no workspace. Do not plan or explore. Reply now with only the requested output.")
  prompt_path <- file.path(wd, "prompt.txt")
  # One argv string must stay under MAX_ARG_STRLEN (131072 on Linux).
  args <- c(
    "--prompt-file", prompt_path,
    "--tools", "",
    "--no-subagents",
    "--disable-web-search",
    "--max-turns", "1",
    "--output-format", "json",
    "--cwd", wd,
    "-m", model
  )
  if (nchar(sys, type = "bytes") > 20000L) {
    write_utf8(paste0(sys, "\n\n", usr), prompt_path)
  } else {
    write_utf8(usr, prompt_path)
    if (nzchar(sys)) args <- c(args, "--system-prompt-override", sys)
  }
  timeout <- llm_timeout(cfg)
  launch <- cli_process_command(bin, args)
  res <- tryCatch(
    processx::run(launch$command, launch$args, wd = wd, timeout = timeout, error_on_status = FALSE, encoding = "UTF-8"),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    msg <- conditionMessage(res)
    if (grepl("timed? ?out", msg, ignore.case = TRUE)) {
      stop("Grok CLI timed out after ", timeout, " seconds.", call. = FALSE)
    }
    stop("Could not run the Grok CLI: ", msg, call. = FALSE)
  }
  out <- paste(c(res$stdout, res$stderr), collapse = "\n")
  status <- suppressWarnings(as.integer(res$status))
  if (length(status) == 1L && !is.na(status) && status < 0L) {
    stop("Grok CLI timed out after ", timeout, " seconds.", call. = FALSE)
  }
  if (!identical(status, 0L)) {
    if (grepl("log[ -]?in|sign in|not authenticated|unauthorized|credentials", out, ignore.case = TRUE)) {
      stop("Not logged in. Run grok login in a terminal, then press Check login.", call. = FALSE)
    }
    stop("Grok CLI failed: ", substr(trimws(out), 1, 400), call. = FALSE)
  }
  j <- tryCatch(jsonlite::fromJSON(res$stdout, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(j)) j <- tryCatch(parse_json_reply(res$stdout), error = function(e) NULL)
  text <- if (is.list(j)) j$text else NULL
  if (is.null(text)) stop("Grok CLI did not return JSON with a text field.", call. = FALSE)
  mu <- if (is.list(j)) j$modelUsage else NULL
  model_name <- model
  if (is.list(mu) && length(mu)) {
    nm <- names(mu)
    if (length(nm) && nzchar(nm[1])) model_name <- nm[1]
  }
  usage <- if (is.list(j)) j$usage else NULL
  if (is.null(usage) && is.list(j) && !is.null(j$total_cost_usd)) usage <- list(total_cost_usd = j$total_cost_usd)
  list(text = as.character(text)[1], model = as.character(model_name)[1], usage = usage)
}

# Local CLI providers answer slowly on big prompts; they use cli_timeout_s (default 600 s).
llm_timeout <- function(cfg) {
  cli <- (cfg$provider %||% "") %in% c("grok_cli", "claude_cli", "codex_cli")
  t <- suppressWarnings(as.numeric(if (cli) cfg$cli_timeout_s %||% 600 else cfg$llm_timeout_s %||% 180))
  if (!length(t) || !is.finite(t[1]) || t[1] < 1) t <- if (cli) 600 else 180
  t[1]
}

# config.R defines this when the app is fully sourced. Tests that load llm.R alone still work.
if (!exists("cli_prefers_login", mode = "function", inherits = TRUE)) {
  cli_prefers_login <- function(cfg) {
    if (is.list(cfg) && (cfg$provider %||% "") %in% c("claude_cli", "codex_cli", "grok_cli")) return(TRUE)
    v <- if (is.list(cfg)) cfg$cli_use_login else NULL
    if (is.null(v) || length(v) == 0L) return(TRUE)
    v <- v[[1]]
    if (is.logical(v)) return(isTRUE(v))
    if (is.numeric(v) && length(v) == 1L && !is.na(v)) return(!identical(as.numeric(v), 0))
    s <- tolower(trimws(as.character(v)[1]))
    if (!nzchar(s)) return(TRUE)
    !s %in% c("false", "0", "no")
  }
}

# NULL means inherit the parent environment. A named vector is the full child environment
# with the listed names removed (processx replaces the environment when env is set).
cli_child_env <- function(cfg, drop) {
  if (!isTRUE(cli_prefers_login(cfg))) return(NULL)
  env <- Sys.getenv()
  if (!length(env)) return(env)
  env[!names(env) %in% drop]
}

# Windows cannot launch npm's .cmd shim as a native process. Testable off-Windows.
cli_process_command <- function(bin, args, os = .Platform$OS.type, comspec = Sys.getenv("COMSPEC", unset = "cmd.exe")) {
  if (identical(tolower(os), "windows") && grepl("\\.cmd$", bin, ignore.case = TRUE)) {
    quoted <- paste(c(shQuote(bin, type = "cmd"), vapply(args, shQuote, "", type = "cmd")), collapse = " ")
    return(list(command = comspec, args = c("/d", "/s", "/c", paste0('"', quoted, '"'))))
  }
  list(command = bin, args = args)
}

# TOML basic string for `codex exec -c`. NULL means the text is not safe to put on argv.
codex_developer_instructions_arg <- function(sys) {
  sys <- paste(as.character(sys %||% ""), collapse = "\n")
  if (!nzchar(sys)) return(NULL)
  if (grepl("[\\x00-\\x08\\x0b\\x0c\\x0e-\\x1f]", sys, perl = TRUE)) return(NULL)
  esc <- gsub("\\", "\\\\", sys, fixed = TRUE)
  esc <- gsub("\"", "\\\"", esc, fixed = TRUE)
  esc <- gsub("\r", "\\r", esc, fixed = TRUE)
  esc <- gsub("\n", "\\n", esc, fixed = TRUE)
  esc <- gsub("\t", "\\t", esc, fixed = TRUE)
  arg <- paste0("developer_instructions=\"", esc, "\"")
  if (nchar(arg, type = "bytes") > 20000L) return(NULL)
  arg
}

# Local `claude` CLI (Claude Code login). No API key. Default model claude-sonnet-5-5.
claude_cli_models <- function() c("claude-sonnet-5-5", "claude-opus-5-5", "claude-haiku-5-5")

claude_cli_chat <- function(cfg, system, user) {
  bin <- cli_binary("claude")
  if (!nzchar(bin)) {
    stop("Claude Code CLI is not on PATH. Install claude and log in, then try again.", call. = FALSE)
  }
  model <- cfg$model %||% ""
  if (!nzchar(model)) model <- "claude-sonnet-5-5"
  wd <- tempfile("dualdag-claude-")
  dir.create(wd, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(wd, recursive = TRUE), add = TRUE)
  sys <- as.character(system %||% "")
  prompt_path <- file.path(wd, "prompt.txt")
  # Keep system instructions out of argv and in their own UTF-8 file. Claude's
  # help advertises --system-prompt[-file]; this avoids Windows' ~32K argv cap.
  args <- c("-p", "--output-format", "json", "--model", model, "--tools", "",
            "--no-session-persistence", "--strict-mcp-config")
  if (nzchar(sys)) {
    system_path <- file.path(wd, "system-prompt.txt")
    write_utf8(sys, system_path)
    args <- c(args, "--system-prompt-file", system_path)
  }
  write_utf8(
    cli_user_prompt(user, "You have no tools and no workspace. Reply now with only the requested output."),
    prompt_path
  )
  timeout <- llm_timeout(cfg)
  launch <- cli_process_command(bin, args)
  res <- tryCatch(
    processx::run(launch$command, launch$args, wd = wd, stdin = prompt_path, timeout = timeout,
                  error_on_status = FALSE, encoding = "UTF-8",
                  env = cli_child_env(cfg, c("ANTHROPIC_API_KEY", "ANTHROPIC_AUTH_TOKEN"))),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    msg <- conditionMessage(res)
    if (grepl("timed? ?out", msg, ignore.case = TRUE)) {
      stop("Claude Code CLI timed out after ", timeout, " seconds.", call. = FALSE)
    }
    stop("Could not run the Claude Code CLI: ", msg, call. = FALSE)
  }
  status <- suppressWarnings(as.integer(res$status))
  if (length(status) == 1L && !is.na(status) && status < 0L) {
    stop("Claude Code CLI timed out after ", timeout, " seconds.", call. = FALSE)
  }
  j <- tryCatch(jsonlite::fromJSON(res$stdout, simplifyVector = FALSE), error = function(e) NULL)
  out <- trimws(paste(c(res$stdout, res$stderr), collapse = "\n"))
  if (!is.list(j) || is.null(j$result) || isTRUE(j$is_error) || !identical(status, 0L)) {
    detail <- if (is.list(j) && !is.null(j$result)) as.character(j$result)[1] else out
    if (grepl("log[ -]?in|not authenticated|unauthorized|credentials|/login", detail, ignore.case = TRUE)) {
      stop("Not logged in. Run claude login in a terminal, then press Check login.", call. = FALSE)
    }
    stop("Claude Code CLI failed: ", substr(detail, 1, 400), call. = FALSE)
  }
  model_name <- model
  mu <- j$modelUsage
  if (is.list(mu) && length(mu) && nzchar(names(mu)[1] %||% "")) model_name <- names(mu)[1]
  usage <- j$usage
  if (is.list(usage) && !is.null(j$total_cost_usd)) usage$total_cost_usd <- j$total_cost_usd
  list(text = as.character(j$result)[1], model = as.character(model_name)[1], usage = usage)
}

# Local `codex` CLI (ChatGPT login). No API key. Empty model keeps the CLI default.
codex_cli_models <- function() c("gpt-6-luna", "gpt-5.6-terra", "gpt-5.6-luna")

# Pull a human error string out of Codex JSONL. Ignores tracing logs.
codex_error_text <- function(stdout, stderr) {
  found <- character()
  consider <- function(x, depth) {
    if (depth > 5 || is.null(x)) return()
    if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) {
      inner <- tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) NULL)
      if (is.list(inner)) consider(inner, depth + 1L) else if (!startsWith(trimws(x), "{")) found <<- c(found, x)
      return()
    }
    if (!is.list(x)) return()
    if (!is.null(x$message)) consider(x$message, depth + 1L)
    if (is.list(x$error)) consider(x$error, depth + 1L)
  }
  blob <- paste(c(stdout, stderr), collapse = "\n")
  for (ln in strsplit(blob, "\n", fixed = TRUE)[[1]]) {
    a <- regexpr("{", ln, fixed = TRUE)
    if (a < 1L) next
    j <- tryCatch(jsonlite::fromJSON(substr(ln, a, nchar(ln)), simplifyVector = FALSE), error = function(e) NULL)
    consider(j, 0L)
  }
  if (!length(found)) return("")
  paste(unique(found), collapse = " ")
}

codex_meta_from_jsonl <- function(stdout) {
  model <- ""
  usage <- NULL
  context_window <- NULL
  texts <- character()
  lines <- strsplit(paste(stdout %||% "", collapse = "\n"), "\n", fixed = TRUE)[[1]]
  for (ln in lines) {
    ln <- trimws(ln)
    if (!nzchar(ln) || !startsWith(ln, "{")) next
    j <- tryCatch(jsonlite::fromJSON(ln, simplifyVector = FALSE), error = function(e) NULL)
    if (!is.list(j)) next
    m <- j$model
    if (is.character(m) && length(m) == 1L && nzchar(m)) model <- m
    context_window <- j$model_context_window %||% j$info$model_context_window %||%
      j$turn_context_window %||% j$turn$context_window %||% j$turn$token_usage$model_context_window %||% context_window
    if (is.list(j$usage)) {
      usage <- j$usage
    }
    item <- j$item
    if (is.list(item)) {
      im <- item$model
      if (is.character(im) && length(im) == 1L && nzchar(im)) model <- im
      if (identical(as.character(item$type %||% "")[1], "agent_message") &&
          is.character(item$text) && length(item$text) == 1L && nzchar(item$text)) {
        texts <- c(texts, item$text)
      }
    }
  }
  if (is.list(usage) && !is.null(context_window)) usage$model_context_window <- context_window
  list(model = model, usage = usage, text = if (length(texts)) texts[[length(texts)]] else "")
}

codex_cli_chat <- function(cfg, system, user) {
  bin <- cli_binary("codex")
  if (!nzchar(bin)) {
    stop("Codex CLI is not on PATH. Install codex and log in, then try again.", call. = FALSE)
  }
  model <- as.character(cfg$model %||% "")[1]
  if (!nzchar(model) || identical(model, "null")) model <- ""
  wd <- tempfile("dualdag-codex-")
  dir.create(wd, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(wd, recursive = TRUE), add = TRUE)
  sys <- paste(as.character(system %||% ""), collapse = "\n")
  body <- cli_user_prompt(user,
    "You have no tools and no workspace. Reply now with only the requested output.")
  instr <- codex_developer_instructions_arg(sys)
  if (nzchar(sys) && is.null(instr)) {
    body <- paste0("System instructions\n", sys, "\n\n", body)
  }
  prompt_path <- file.path(wd, "prompt.txt")
  last_path <- file.path(wd, "last.txt")
  write_utf8(body, prompt_path)
  args <- c("exec", "--skip-git-repo-check", "--sandbox", "read-only", "--ephemeral",
            "--color", "never", "--json", "-c", "approval_policy=\"never\"")
  if (nzchar(model)) args <- c(args, "-m", model)
  if (!is.null(instr)) args <- c(args, "-c", instr)
  args <- c(args, "--output-last-message", last_path, "-")
  timeout <- llm_timeout(cfg)
  launch <- cli_process_command(bin, args)
  res <- tryCatch(
    processx::run(launch$command, launch$args, wd = wd, stdin = prompt_path, timeout = timeout,
                  error_on_status = FALSE, encoding = "UTF-8",
                  env = cli_child_env(cfg, c("OPENAI_API_KEY", "CODEX_API_KEY"))),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    msg <- conditionMessage(res)
    if (grepl("timed? ?out", msg, ignore.case = TRUE)) {
      stop("Codex CLI timed out after ", timeout, " seconds.", call. = FALSE)
    }
    stop("Could not run the Codex CLI: ", msg, call. = FALSE)
  }
  status <- suppressWarnings(as.integer(res$status))
  if (length(status) == 1L && !is.na(status) && status < 0L) {
    stop("Codex CLI timed out after ", timeout, " seconds.", call. = FALSE)
  }
  meta <- codex_meta_from_jsonl(res$stdout)
  text <- read_utf8(last_path)
  if (!nzchar(text)) text <- meta$text
  text <- sub("[ \t\r\n]+$", "", text)
  detail <- codex_error_text(res$stdout, res$stderr)
  if (!nzchar(detail)) detail <- trimws(paste(c(res$stdout, res$stderr), collapse = "\n"))
  if (!identical(status, 0L) || !nzchar(text)) {
    # Match an actual login failure. A debug log that merely contains "login" is not one.
    if (grepl("not[[:space:]]+logged[[:space:]]+in|please[[:space:]]+(log|sign)[[:space:]]+in|not authenticated|\\bunauthorized\\b",
              detail, ignore.case = TRUE, perl = TRUE)) {
      stop("Not logged in. Run codex login in a terminal, then press Check login.", call. = FALSE)
    }
    stop("Codex CLI failed: ", substr(detail, 1, 400), call. = FALSE)
  }
  model_name <- if (nzchar(meta$model)) meta$model else if (nzchar(model)) model else "codex default"
  list(text = text, model = as.character(model_name)[1], usage = meta$usage)
}

cli_probe <- function(bin, args, timeout, env = NULL) {
  launch <- cli_process_command(bin, args)
  res <- tryCatch(
    processx::run(launch$command, launch$args, timeout = timeout, error_on_status = FALSE, encoding = "UTF-8", env = env),
    error = function(e) e
  )
  if (inherits(res, "error")) {
    timed <- grepl("timed? ?out", conditionMessage(res), ignore.case = TRUE)
    return(list(ok = FALSE, timeout = timed, status = NA_integer_, stdout = "", stderr = ""))
  }
  status <- suppressWarnings(as.integer(res$status))
  timed <- length(status) == 1L && !is.na(status) && status < 0L
  list(ok = !timed && identical(status, 0L), timeout = isTRUE(timed), status = status,
       stdout = paste(res$stdout %||% "", collapse = "\n"),
       stderr = paste(res$stderr %||% "", collapse = "\n"))
}

cli_first_line <- function(txt) {
  lines <- strsplit(paste(txt %||% "", collapse = "\n"), "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (!length(lines)) return("")
  substr(lines[1], 1, 120)
}

claude_auth_from_output <- function(stdout) {
  txt <- paste(stdout %||% "", collapse = "\n")
  j <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(j)) {
    a <- regexpr("{", txt, fixed = TRUE)
    b <- max(gregexpr("}", txt, fixed = TRUE)[[1]])
    if (a > 0 && b > a) {
      j <- tryCatch(jsonlite::fromJSON(substr(txt, a, b), simplifyVector = FALSE), error = function(e) NULL)
    }
  }
  if (!is.list(j)) return(list(logged_in = FALSE, method = ""))
  method <- j$authMethod
  method <- if (is.character(method) && length(method) == 1L && nzchar(method) && !is.na(method)) method else ""
  list(logged_in = isTRUE(j$loggedIn), method = method)
}

codex_login_from_output <- function(stdout) {
  txt <- trimws(paste(stdout %||% "", collapse = "\n"))
  if (!nzchar(txt)) return(list(logged_in = FALSE, method = ""))
  if (grepl("not[[:space:]]+logged[[:space:]]+in", txt, ignore.case = TRUE, perl = TRUE)) {
    return(list(logged_in = FALSE, method = ""))
  }
  if (!grepl("logged[[:space:]]+in", txt, ignore.case = TRUE, perl = TRUE)) {
    return(list(logged_in = FALSE, method = ""))
  }
  method <- sub("(?i).*\\busing[[:space:]]+", "", txt, perl = TRUE)
  method <- trimws(strsplit(method, "\n", fixed = TRUE)[[1]][1])
  if (!nzchar(method) || grepl("logged[[:space:]]+in", method, ignore.case = TRUE, perl = TRUE)) method <- ""
  list(logged_in = TRUE, method = method)
}

# Never throws. grok logged_in stays NULL. Reply fields are installed, path, version,
# logged_in, method, hint — no account identifiers.
cli_status <- function(provider, timeout = 15, cfg = NULL) {
  provider <- as.character(provider %||% "")[1]
  hint_out <- switch(provider,
    claude_cli = "Not logged in. Run claude login in a terminal, then press Check login.",
    codex_cli = "Not logged in. Run codex login in a terminal, then press Check login.",
    ""
  )
  bin_name <- switch(provider, claude_cli = "claude", codex_cli = "codex", grok_cli = "grok", "")
  product <- switch(provider,
    claude_cli = "Claude Code CLI", codex_cli = "Codex CLI", grok_cli = "Grok CLI", "CLI")
  logged_missing <- if (identical(provider, "grok_cli")) NULL else FALSE
  blank <- function(installed, path, version, logged_in, method, hint) {
    list(installed = isTRUE(installed), path = as.character(path %||% "")[1],
         version = as.character(version %||% "")[1], logged_in = logged_in,
         method = as.character(method %||% "")[1], hint = as.character(hint %||% "")[1])
  }
  if (!nzchar(bin_name)) {
    return(blank(FALSE, "", "", FALSE, "", "Unknown provider."))
  }
  bin <- cli_binary(bin_name)
  if (!nzchar(bin)) {
    return(blank(FALSE, "", "", logged_missing, "", paste0(product, " is not on PATH.")))
  }
  if (is.null(cfg) && exists("load_config", mode = "function", inherits = TRUE)) {
    cfg <- tryCatch(load_config(), error = function(e) list())
  }
  if (is.null(cfg)) cfg <- list()
  drop <- switch(provider,
    claude_cli = c("ANTHROPIC_API_KEY", "ANTHROPIC_AUTH_TOKEN"),
    codex_cli = c("OPENAI_API_KEY", "CODEX_API_KEY"),
    character()
  )
  env <- if (length(drop)) cli_child_env(cfg, drop) else NULL
  t <- suppressWarnings(as.numeric(timeout))
  if (!length(t) || !is.finite(t[1]) || t[1] < 1) t <- 15
  ver <- cli_probe(bin, "--version", t[1], env)
  if (isTRUE(ver$timeout)) {
    return(blank(TRUE, bin, "", logged_missing, "", "The CLI check timed out."))
  }
  version <- cli_first_line(ver$stdout)
  if (!nzchar(version)) version <- cli_first_line(ver$stderr)
  if (identical(provider, "grok_cli")) {
    return(blank(TRUE, bin, version, NULL, "", ""))
  }
  auth_args <- if (identical(provider, "claude_cli")) c("auth", "status") else c("login", "status")
  auth <- cli_probe(bin, auth_args, t[1], env)
  if (isTRUE(auth$timeout)) {
    return(blank(TRUE, bin, version, FALSE, "", "The CLI check timed out."))
  }
  parsed <- if (identical(provider, "claude_cli")) {
    claude_auth_from_output(auth$stdout)
  } else {
    codex_login_from_output(paste(c(auth$stdout, auth$stderr), collapse = "\n"))
  }
  hint <- if (isTRUE(parsed$logged_in)) "" else hint_out
  blank(TRUE, bin, version, isTRUE(parsed$logged_in), parsed$method, hint)
}

llm_chat <- function(cfg, system, user, max_tokens = 6000L, json = TRUE) {
  res <- tryCatch({
    if (identical(Sys.getenv("DUALDAG_FAKE_LLM"), "1")) {
      list(text = fake_llm_reply(system, user), model = "fake-llm",
           usage = list(prompt_tokens = 0L, completion_tokens = 0L))
    } else if (identical(cfg$provider %||% "", "grok_cli")) {
      grok_cli_chat(cfg, system, user)
    } else if (identical(cfg$provider %||% "", "claude_cli")) {
      claude_cli_chat(cfg, system, user)
    } else if (identical(cfg$provider %||% "", "codex_cli")) {
      codex_cli_chat(cfg, system, user)
    } else {
      req <- llm_request(cfg, system, user, max_tokens, json)
      resp <- tryCatch(httr2::req_perform(req), error = function(e) {
        stop("Could not reach the LLM provider: ", conditionMessage(e), call. = FALSE)
      })
      st <- httr2::resp_status(resp)
      j <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE), error = function(e) {
        list(raw = tryCatch(httr2::resp_body_string(resp), error = function(e) ""))
      })
      if (st >= 400) {
        msg <- j$error$message %||% j$error %||% j$message %||% j$raw %||% ""
        if (is.list(msg)) msg <- jsonlite::toJSON(msg, auto_unbox = TRUE)
        stop(sprintf("LLM provider error %d: %s", st, substr(as.character(msg), 1, 400)), call. = FALSE)
      }
      llm_parse(cfg$provider %||% "openai", j)
    }
  }, error = function(e) e)
  logged <- if (inherits(res, "error")) list(text = conditionMessage(res), model = NULL, usage = NULL) else res
  tryCatch(llm_log_record(cfg, system, user, logged), error = function(e) NULL)
  if (inherits(res, "error")) stop(conditionMessage(res), call. = FALSE)
  res
}

# Pull the first JSON object out of a model reply (tolerates ``` fences and leading prose).
parse_json_reply <- function(txt) {
  t <- trimws(txt %||% "")
  t <- sub("^```(?:json)?\\s*", "", t, perl = TRUE)
  t <- sub("\\s*```\\s*$", "", t, perl = TRUE)
  out <- tryCatch(jsonlite::fromJSON(t, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(out)) {
    a <- regexpr("{", t, fixed = TRUE)
    b <- max(gregexpr("}", t, fixed = TRUE)[[1]])
    if (a > 0 && b > a) out <- tryCatch(jsonlite::fromJSON(substr(t, a, b), simplifyVector = FALSE), error = function(e) NULL)
  }
  if (is.null(out)) stop("The model did not return valid JSON.", call. = FALSE)
  out
}

llm_test <- function(cfg) {
  r <- llm_chat(cfg, "TASK: llm_test\nYou are a connectivity check.",
                'Reply with the JSON object {"ok": true}.', max_tokens = 400L, json = TRUE)
  list(ok = TRUE, model = r$model, reply = substr(r$text, 1, 200))
}

fake_task <- function(system) {
  m <- regexec("TASK:\\s*([a-z0-9_]+)", system %||% "", perl = TRUE)
  r <- regmatches(system %||% "", m)[[1]]
  if (length(r) < 2L) "unknown" else r[[2]]
}

# Deterministic stand-in. Task-specific builders live next to the feature they fake.
fake_llm_reply <- function(system, user) {
  task <- fake_task(system)
  if (grepl("SU_MODE", system, fixed = TRUE)) return(json_text(list(tool="reply", text="SU is ready to help with this side.")))
  if (grepl("CHAT_REPLY_FORMAT_BEGIN", system, fixed = TRUE))
    return(fake_side_agent_reply(system, user))
  if (identical(task, "llm_test")) return('{"ok":true}')
  fn <- switch(task,
    extract_classification = "fake_extract_classification_reply",
    extract_plan = "fake_extract_plan_reply",
    extract_rows = "fake_extract_rows_reply",
    extract_check = "fake_extract_check_reply",
    extract_cycles = "fake_extract_cycles_reply",
    generate_primary = "fake_generate_reply",
    generate_validation = "fake_generate_reply",
    qc_investigate = "fake_qc_reply",
    ask = "fake_ask_reply",
    side_agent = "fake_side_agent_reply",
    dag_ask = "fake_dag_reply",
    param_extract = "fake_param_reply",
    param_ask = "fake_param_reply",
    self_review = "fake_self_review_reply",
    NULL)
  if (is.null(fn) || !exists(fn, mode = "function", inherits = TRUE)) {
    return(json_text(list(ok = FALSE, message = paste("Unknown fake task:", task))))
  }
  get(fn, mode = "function", inherits = TRUE)(system, user)
}
