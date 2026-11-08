source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))

cfg_for <- function(provider, base_url = "") {
  list(provider = provider, base_url = base_url, model = "test-model", api_key = "sk-test-123", llm_timeout_s = 30)
}

su_cli_notice <- "You have no tools of your own and no workspace. The app runs the tools listed above when you return their JSON. Reply now with only one JSON object."

reply_json <- function(content, provider = "openai") {
  body <- if (provider == "anthropic") {
    list(model = "test-model", content = list(list(type = "text", text = content)), usage = list(input_tokens = 1))
  } else {
    list(model = "test-model", choices = list(list(message = list(role = "assistant", content = content))),
         usage = list(total_tokens = 1))
  }
  httr2::response_json(status_code = 200L, body = body)
}

test_that("provider token usage normalizes cache accounting and Codex context windows", {
  expect_equal(llm_usage_summary(list(input_tokens=10, cached_input_tokens=4, output_tokens=2), "codex_cli"),
               list(input=10, output=2, limit=NULL))
  expect_equal(llm_usage_summary(list(input_tokens=10, cache_read_input_tokens=3,
                                      cache_creation_input_tokens=2, output_tokens=4), "claude_cli"),
               list(input=15, output=4, limit=NULL))
  expect_equal(llm_usage_summary(list(prompt_tokens=12, completion_tokens=6), "openai"),
               list(input=12, output=6, limit=NULL))
  expect_equal(llm_usage_summary(list(input_tokens=7, output_tokens=3), "anthropic"),
               list(input=7, output=3, limit=NULL))
  expect_equal(llm_usage_summary(list(input_tokens=7, output_tokens=3), "grok_cli"),
               list(input=7, output=3, limit=NULL))
  expect_null(llm_usage_summary(list(total_cost_usd=0.01), "grok_cli"))
  meta <- codex_meta_from_jsonl('{"type":"turn.completed","usage":{"input_tokens":25,"output_tokens":4},"model_context_window":258000}')
  expect_equal(llm_usage_summary(meta$usage, "codex_cli"), list(input=25, output=4, limit=258000))
})

test_that("multi-call usage reports the largest input and actual call count", {
  usage <- llm_usage_combine(list(list(input=22000,output=50,limit=258000),
                                  list(input=22400,output=40,limit=258000),
                                  list(input=9000,output=30,limit=258000)))
  expect_equal(usage, list(input=22400,output=120,limit=258000,calls=3L))
  unknown <- llm_usage_combine(list(NULL,NULL))
  expect_equal(unknown$calls, 2L)
  expect_null(unknown$input)
})

test_that("OpenAI, Anthropic, and compatible requests match the provider contracts", {
  without_fake_llm({
    req <- llm_request(cfg_for("openai"), "sys", "user")
    expect_equal(req$url, "https://api.openai.com/v1/chat/completions")
    expect_equal(req$body$data$model, "test-model")
    expect_equal(req$body$data$response_format$type, "json_object")
    expect_equal(req$body$data$max_completion_tokens, 6000L)
    expect_equal(req$body$data$messages[[1]]$role, "system")
    expect_true(any(grepl("Authorization", names(req$headers))))

    req_a <- llm_request(cfg_for("anthropic"), "sys", "user")
    expect_equal(req_a$url, "https://api.anthropic.com/v1/messages")
    expect_equal(req_a$body$data$system, "sys")
    expect_equal(req_a$body$data$messages[[1]]$content, "user")
    expect_true(all(c("x-api-key", "anthropic-version") %in% names(req_a$headers)))

    req_c <- llm_request(cfg_for("openai_compatible", "http://localhost:8000/v1/"), "s", "u")
    expect_equal(req_c$url, "http://localhost:8000/v1/chat/completions")
    expect_equal(req_c$body$data$max_tokens, 6000L)
    expect_null(req_c$body$data$response_format)
    expect_error(llm_request(cfg_for("openai_compatible"), "s", "u"), "base URL")

    c1 <- cfg_for("openai"); c1$api_key <- ""
    expect_error(llm_request(c1, "s", "u"), "No API key")
    c2 <- cfg_for("openai"); c2$model <- ""
    expect_error(llm_request(c2, "s", "u"), "No model")
    expect_error(llm_chat(c1, "s", "u"), "No API key")
  })
})

test_that("a pasted endpoint path is not requested twice", {
  without_fake_llm({
    expect_equal(
      llm_base_url(cfg_for("openai_compatible", "https://openrouter.ai/api/v1/chat/completions")),
      "https://openrouter.ai/api/v1"
    )
    expect_equal(
      llm_base_url(cfg_for("openai", "https://openrouter.ai/api/v1/chat/completions/")),
      "https://openrouter.ai/api/v1"
    )
    req <- llm_request(cfg_for("openai_compatible", "https://openrouter.ai/api/v1/chat/completions"), "s", "u")
    expect_equal(req$url, "https://openrouter.ai/api/v1/chat/completions")
    expect_equal(
      llm_base_url(cfg_for("anthropic", "https://api.anthropic.com/v1/messages")),
      "https://api.anthropic.com"
    )
    req_a <- llm_request(cfg_for("anthropic", "https://api.anthropic.com/v1/messages/"), "s", "u")
    expect_equal(req_a$url, "https://api.anthropic.com/v1/messages")
    expect_equal(llm_base_url(cfg_for("openai_compatible", "https://openrouter.ai/api/v1")), "https://openrouter.ai/api/v1")
  })
})

test_that("llm_chat parses provider replies and hides the key", {
  without_fake_llm({
    seen <- NULL
    httr2::with_mocked_responses(function(req) { seen <<- req; reply_json('{"ok": true}') }, {
      r <- llm_chat(cfg_for("openai"), "s", "u")
    })
    expect_equal(r$text, '{"ok": true}')
    expect_equal(r$model, "test-model")
    expect_match(seen$url, "chat/completions$")
    httr2::with_mocked_responses(function(req) reply_json("hello", "anthropic"), {
      r2 <- llm_chat(cfg_for("anthropic"), "s", "u")
    })
    expect_equal(r2$text, "hello")
    httr2::with_mocked_responses(function(req) {
      httr2::response_json(status_code = 401L, body = list(error = list(message = "Incorrect API key")))
    }, {
      e <- tryCatch(llm_chat(cfg_for("openai"), "s", "u"), error = function(e) e)
    })
    expect_match(conditionMessage(e), "LLM provider error 401: Incorrect API key")
    expect_false(grepl("sk-test-123", conditionMessage(e)))
  })
})

test_that("parse_json_reply tolerates fences and prose", {
  expect_equal(parse_json_reply('```json\n{"a": 1}\n```')$a, 1)
  expect_equal(parse_json_reply('Sure! {"a": 2} hope this helps')$a, 2)
  expect_error(parse_json_reply("no json here"), "valid JSON")
})

test_that("fake LLM answers without a key and is deterministic", {
  with_fake_llm({
    r <- llm_chat(list(), "TASK: llm_test\nping", "ping")
    expect_equal(r$model, "fake-llm")
    expect_equal(parse_json_reply(r$text)$ok, TRUE)
    a <- fake_llm_reply("TASK: llm_test", "x")
    b <- fake_llm_reply("TASK: llm_test", "y")
    expect_equal(a, b)

    spec <- spec_of(
      V("ADSL", "USUBJID", key = 1, source = "DM.USUBJID", row = 2),
      V("ADSL", "TRTSDT", source = "DM.RFSTDTC", derivation = "copy DM.RFSTDTC", row = 3)
    )
    g1 <- extract_dependencies(list(), spec, "ADSL")
    g2 <- extract_dependencies(list(), spec, "ADSL")
    expect_equal(edge_pairs(g1), edge_pairs(g2))
    expect_equal(vapply(g1$external, function(e) e$ref, ""), vapply(g2$external, function(e) e$ref, ""))
    code <- generate_side(list(), "validation", spec, g1, "ADSL.TRTSDT", character())
    expect_true(isTRUE(attr(code, "same_source")))
    expect_match(code, "RFSTDTC")
  })
})

test_that("grok_cli runs a local grok binary and does not need an API key", {
  old_tool_notice <- getOption("dualdag.cli_tool_notice")
  on.exit(options(dualdag.cli_tool_notice=old_tool_notice),add=TRUE)
  bin <- tempfile("grok-bin-")
  dir.create(bin)
  capture <- tempfile("grok-cap-")
  script <- c(
    "#!/bin/sh",
    "capture=\"${DUALDAG_FAKE_GROK_CAPTURE:-}\"",
    "mode=\"${DUALDAG_FAKE_GROK_MODE:-ok}\"",
    "prompt=\"\"",
    "system=\"\"",
    "prev=\"\"",
    "for a in \"$@\"; do",
    "  if [ \"$prev\" = \"--prompt-file\" ]; then prompt=\"$a\"; fi",
    "  if [ \"$prev\" = \"--system-prompt-override\" ]; then system=\"$a\"; fi",
    "  prev=\"$a\"",
    "done",
    "if [ -n \"$capture\" ]; then",
    "  printf '%s\\n' \"$@\" > \"$capture.args\"",
    "  printf '%s' \"$system\" > \"$capture.system\"",
    "  if [ -n \"$prompt\" ] && [ -f \"$prompt\" ]; then cp \"$prompt\" \"$capture.prompt\"; fi",
    "fi",
    "if [ \"$mode\" = \"login\" ]; then echo \"Error: please log in to continue\" >&2; exit 1; fi",
    "if [ \"$mode\" = \"sleep\" ]; then sleep 30; exit 0; fi",
    "if [ \"$mode\" = \"badjson\" ]; then echo not-json; exit 0; fi",
    "printf '%s\\n' '{\"text\":\"hello from grok\",\"stopReason\":\"end_turn\",\"usage\":{\"input_tokens\":3},\"modelUsage\":{\"grok-4.7\":{\"inputTokens\":3}},\"total_cost_usd\":0}'",
    "exit 0"
  )
  writeLines(script, file.path(bin, "grok"))
  Sys.chmod(file.path(bin, "grok"), "0755")
  old_path <- Sys.getenv("PATH")
  old_mode <- Sys.getenv("DUALDAG_FAKE_GROK_MODE", unset = NA_character_)
  old_cap <- Sys.getenv("DUALDAG_FAKE_GROK_CAPTURE", unset = NA_character_)
  on.exit({
    Sys.setenv(PATH = old_path)
    if (is.na(old_mode)) Sys.unsetenv("DUALDAG_FAKE_GROK_MODE") else Sys.setenv(DUALDAG_FAKE_GROK_MODE = old_mode)
    if (is.na(old_cap)) Sys.unsetenv("DUALDAG_FAKE_GROK_CAPTURE") else Sys.setenv(DUALDAG_FAKE_GROK_CAPTURE = old_cap)
  }, add = TRUE)
  Sys.setenv(PATH = paste(bin, old_path, sep = ":"),
             DUALDAG_FAKE_GROK_CAPTURE = capture,
             DUALDAG_FAKE_GROK_MODE = "ok")
  without_fake_llm({
    cfg <- list(provider = "grok_cli", model = "", api_key = "sk-should-not-be-sent", llm_timeout_s = 30)
    r <- llm_chat(cfg, "SYS-NOTE", "USER-NOTE")
    expect_equal(r$text, "hello from grok")
    expect_equal(r$model, "grok-4.7")
    args <- paste(readLines(paste0(capture, ".args"), warn = FALSE), collapse = "\n")
    expect_match(args, "--max-turns", fixed = TRUE)
    expect_match(args, "--no-subagents", fixed = TRUE)
    expect_match(args, "--disable-web-search", fixed = TRUE)
    expect_match(args, "--output-format", fixed = TRUE)
    expect_match(args, "-m", fixed = TRUE)
    expect_false(grepl("sk-should-not-be-sent", args, fixed = TRUE))
    expect_match(paste(readLines(paste0(capture, ".system"), warn = FALSE), collapse = "\n"), "SYS-NOTE", fixed = TRUE)
    expect_match(paste(readLines(paste0(capture, ".prompt"), warn = FALSE), collapse = "\n"), "USER-NOTE", fixed = TRUE)
    normal_prompt <- paste(readLines(paste0(capture, ".prompt"), warn = FALSE), collapse = "\n")
    expect_identical(normal_prompt,"USER-NOTE\n\nYou have no tools and no workspace. Do not plan or explore. Reply now with only the requested output.")
    options(dualdag.cli_tool_notice=su_cli_notice)
    llm_chat(cfg,"SYS-NOTE","USER-NOTE")
    su_prompt <- paste(readLines(paste0(capture,".prompt"),warn=FALSE),collapse="\n")
    options(dualdag.cli_tool_notice=old_tool_notice)
    expect_identical(su_prompt,paste0("USER-NOTE\n\n",su_cli_notice))
    expect_false(grepl("You have no tools and no workspace",su_prompt,fixed=TRUE))

    long <- paste(rep("S", 100001), collapse = "")
    r2 <- llm_chat(cfg, long, "SHORT-USER")
    expect_equal(r2$text, "hello from grok")
    sys2 <- paste(readLines(paste0(capture, ".system"), warn = FALSE), collapse = "\n")
    expect_equal(sys2, "")
    prompt2 <- paste(readLines(paste0(capture, ".prompt"), warn = FALSE), collapse = "\n")
    expect_match(prompt2, "SHORT-USER", fixed = TRUE)
    expect_true(startsWith(prompt2, "S"))

    Sys.setenv(DUALDAG_FAKE_GROK_MODE = "login")
    expect_error(llm_chat(cfg, "s", "u"), "Not logged in")
    Sys.setenv(DUALDAG_FAKE_GROK_MODE = "badjson")
    expect_error(llm_chat(cfg, "s", "u"), "JSON")
    Sys.setenv(DUALDAG_FAKE_GROK_MODE = "sleep")
    cfg_t <- cfg
    cfg_t$cli_timeout_s <- 1
    expect_error(llm_chat(cfg_t, "s", "u"), "timed out")
    Sys.setenv(PATH = "/usr/bin:/bin", DUALDAG_FAKE_GROK_MODE = "ok")
    expect_error(llm_chat(cfg, "s", "u"), "not on PATH")
  })
})

test_that("claude_cli sends the prompt on stdin and reads the result field", {
  old_tool_notice <- getOption("dualdag.cli_tool_notice")
  on.exit(options(dualdag.cli_tool_notice=old_tool_notice),add=TRUE)
  bin <- tempfile("claude-bin-")
  dir.create(bin)
  capture <- tempfile("claude-cap-")
  script <- c(
    "#!/bin/sh",
    "capture=\"${DUALDAG_FAKE_CLAUDE_CAPTURE:-}\"",
    "mode=\"${DUALDAG_FAKE_CLAUDE_MODE:-ok}\"",
    "if [ -n \"$capture\" ]; then printf '%s\\n' \"$@\" > \"$capture.args\"; prev=; for arg in \"$@\"; do if [ \"$prev\" = \"--system-prompt-file\" ]; then cp \"$arg\" \"$capture.system\"; fi; prev=\"$arg\"; done; cat > \"$capture.stdin\"; fi",
    "if [ \"$mode\" = \"login\" ]; then printf '%s\\n' '{\"type\":\"result\",\"is_error\":true,\"result\":\"Not logged in. Please run /login\"}'; exit 1; fi",
    "if [ \"$mode\" = \"error\" ]; then printf '%s\\n' '{\"type\":\"result\",\"is_error\":true,\"result\":\"API Error: overloaded\"}'; exit 1; fi",
    "if [ \"$mode\" = \"sleep\" ]; then sleep 30; exit 0; fi",
    "printf '%s\\n' '{\"type\":\"result\",\"is_error\":false,\"result\":\"hello from claude\",\"stop_reason\":\"end_turn\",\"usage\":{\"input_tokens\":3},\"modelUsage\":{\"claude-sonnet-5-5\":{\"inputTokens\":3}},\"total_cost_usd\":0.001}'",
    "exit 0"
  )
  writeLines(script, file.path(bin, "claude"))
  Sys.chmod(file.path(bin, "claude"), "0755")
  old_path <- Sys.getenv("PATH")
  on.exit({
    Sys.setenv(PATH = old_path)
    Sys.unsetenv(c("DUALDAG_FAKE_CLAUDE_MODE", "DUALDAG_FAKE_CLAUDE_CAPTURE"))
  }, add = TRUE)
  Sys.setenv(PATH = paste(bin, old_path, sep = ":"), DUALDAG_FAKE_CLAUDE_CAPTURE = capture,
             DUALDAG_FAKE_CLAUDE_MODE = "ok")
  without_fake_llm({
    cfg <- list(provider = "claude_cli", model = "", api_key = "sk-should-not-be-sent")
    expect_equal(llm_timeout(cfg), 600)
    r <- llm_chat(cfg, "SYS-NOTE", "USER-NOTE")
    expect_equal(r$text, "hello from claude")
    expect_equal(r$model, "claude-sonnet-5-5")
    expect_equal(r$usage$total_cost_usd, 0.001)
    args <- readLines(paste0(capture, ".args"), warn = FALSE)
    expect_true(all(c("-p", "--output-format", "json", "--tools", "--no-session-persistence",
                      "--strict-mcp-config") %in% args))
    expect_true("--system-prompt-file" %in% args)
    expect_false("--system-prompt" %in% args)
    expect_equal(args[which(args == "--model") + 1L], "claude-sonnet-5-5")
    expect_false("--bare" %in% args)
    expect_false(any(grepl("sk-should-not-be-sent", args, fixed = TRUE)))
    expect_false(any(grepl("USER-NOTE", args, fixed = TRUE)))
    stdin <- paste(readLines(paste0(capture, ".stdin"), warn = FALSE), collapse = "\n")
    expect_match(stdin, "USER-NOTE", fixed = TRUE)
    expect_match(stdin, "no tools", fixed = TRUE)
    expect_identical(stdin,"USER-NOTE\n\nYou have no tools and no workspace. Reply now with only the requested output.")
    expect_identical(paste(readLines(paste0(capture, ".system"), warn = FALSE), collapse = "\n"), "SYS-NOTE")
    options(dualdag.cli_tool_notice=su_cli_notice)
    llm_chat(cfg,"SYS-NOTE","USER-NOTE")
    su_stdin <- paste(readLines(paste0(capture,".stdin"),warn=FALSE),collapse="\n")
    options(dualdag.cli_tool_notice=old_tool_notice)
    expect_identical(su_stdin,paste0("USER-NOTE\n\n",su_cli_notice))
    expect_identical(paste(readLines(paste0(capture, ".system"), warn = FALSE), collapse = "\n"), "SYS-NOTE")
    expect_false(grepl("You have no tools and no workspace",su_stdin,fixed=TRUE))

    Sys.setenv(DUALDAG_FAKE_CLAUDE_MODE = "login")
    expect_error(llm_chat(cfg, "s", "u"), "Not logged in")
    Sys.setenv(DUALDAG_FAKE_CLAUDE_MODE = "error")
    expect_error(llm_chat(cfg, "s", "u"), "overloaded")
    Sys.setenv(DUALDAG_FAKE_CLAUDE_MODE = "sleep")
    expect_error(llm_chat(modifyList(cfg, list(cli_timeout_s = 1)), "s", "u"), "timed out")
    Sys.setenv(PATH = "/usr/bin:/bin", DUALDAG_FAKE_CLAUDE_MODE = "ok")
    expect_error(llm_chat(cfg, "s", "u"), "not on PATH")
  })
})

test_that("codex developer instructions are a TOML string, unless the text is too large", {
  expect_equal(codex_developer_instructions_arg("SYS-NOTE"), 'developer_instructions="SYS-NOTE"')
  expect_equal(
    codex_developer_instructions_arg("say \"hi\"\nnext"),
    paste0("developer_instructions=\"", "say \\\"hi\\\"\\nnext", "\"")
  )
  expect_null(codex_developer_instructions_arg(paste(rep("S", 20001), collapse = "")))
  expect_true(isTRUE(cli_prefers_login(list())))
  expect_true(isTRUE(cli_prefers_login(list(cli_use_login = TRUE))))
  expect_false(isTRUE(cli_prefers_login(list(cli_use_login = FALSE))))
  expect_false(isTRUE(cli_prefers_login(list(cli_use_login = "false"))))
})

test_that("Claude Windows commands preserve empty tools and use cmd only for npm shims", {
  claude_args <- c("-p", "--output-format", "json", "--model", "test-model", "--tools", "",
                   "--no-session-persistence", "--strict-mcp-config")
  shim <- cli_process_command("C:/Users/wd/AppData/Local/bin/claude.cmd", claude_args,
                              os = "windows", comspec = "C:/Windows/System32/cmd.exe")
  expect_equal(shim$command, "C:/Windows/System32/cmd.exe")
  expect_equal(shim$args[1:3], c("/d", "/s", "/c"))
  expect_match(shim$args[4], "claude.cmd", fixed = TRUE)
  expect_match(shim$args[4], paste0(intToUtf8(34), "--tools", intToUtf8(34), " ", intToUtf8(34), intToUtf8(34)), fixed = TRUE)

  exe <- cli_process_command("C:/Users/wd/LOCAL~1/bin/claude.exe", claude_args, os = "windows")
  expect_equal(exe$command, "C:/Users/wd/LOCAL~1/bin/claude.exe")
  expect_identical(exe$args, claude_args)
  expect_identical(exe$args[which(exe$args == "--tools") + 1L], "")
  expect_equal(cli_binary("claude", function(nm) setNames("C:/Users/wd/LOCAL~1/bin/claude.exe", nm)),
               "C:/Users/wd/LOCAL~1/bin/claude.exe")
})

test_that("codex_cli runs codex exec and strips OpenAI key variables by default", {
  old_tool_notice <- getOption("dualdag.cli_tool_notice")
  on.exit(options(dualdag.cli_tool_notice=old_tool_notice),add=TRUE)
  bin <- tempfile("codex-bin-")
  dir.create(bin)
  capture <- tempfile("codex-cap-")
  script <- c(
    "#!/bin/sh",
    "capture=\"${DUALDAG_FAKE_CODEX_CAPTURE:-}\"",
    "mode=\"${DUALDAG_FAKE_CODEX_MODE:-ok}\"",
    "if [ -n \"$capture\" ]; then",
    "  printf '%s\\n' \"$@\" > \"$capture.args\"",
    "  cat > \"$capture.stdin\"",
    "  : > \"$capture.env\"",
    "  for n in ANTHROPIC_API_KEY ANTHROPIC_AUTH_TOKEN OPENAI_API_KEY CODEX_API_KEY; do",
    "    if printenv \"$n\" >/dev/null 2>&1; then printf '%s\\n' \"$n\" >> \"$capture.env\"; fi",
    "  done",
    "fi",
    "out=\"\"",
    "prev=\"\"",
    "for a in \"$@\"; do",
    "  if [ \"$prev\" = \"--output-last-message\" ]; then out=\"$a\"; fi",
    "  prev=\"$a\"",
    "done",
    "if [ \"$mode\" = \"login\" ]; then echo \"Not logged in. Run codex login\" >&2; exit 1; fi",
    "if [ \"$mode\" = \"model\" ]; then echo \"trace https://auth.example/login\" >&2; printf '%s\\n' '{\"type\":\"error\",\"message\":\"The model is not supported when using Codex with a ChatGPT account.\"}'; exit 1; fi",
    "if [ \"$mode\" = \"sleep\" ]; then sleep 30; exit 0; fi",
    "if [ \"$mode\" = \"longfail\" ]; then printf '%s' \"$(printf '%500s' '' | tr ' ' E)\" >&2; exit 1; fi",
    "if [ -n \"$out\" ]; then printf '%s' \"hello from codex\" > \"$out\"; fi",
    "if [ \"$mode\" = \"plain\" ]; then exit 0; fi",
    "printf '%s\\n' '{\"type\":\"thread.started\",\"model\":\"gpt-5.4\"}'",
    "printf '%s\\n' '{\"type\":\"turn.completed\",\"usage\":{\"input_tokens\":3,\"output_tokens\":1}}'",
    "exit 0"
  )
  writeLines(script, file.path(bin, "codex"))
  Sys.chmod(file.path(bin, "codex"), "0755")
  old_path <- Sys.getenv("PATH")
  keys <- c("OPENAI_API_KEY", "CODEX_API_KEY", "ANTHROPIC_API_KEY", "ANTHROPIC_AUTH_TOKEN")
  old_keys <- Sys.getenv(keys, unset = NA_character_)
  on.exit({
    Sys.setenv(PATH = old_path)
    Sys.unsetenv(c("DUALDAG_FAKE_CODEX_MODE", "DUALDAG_FAKE_CODEX_CAPTURE"))
    for (nm in keys) if (is.na(old_keys[[nm]])) Sys.unsetenv(nm) else do.call(Sys.setenv, setNames(list(old_keys[[nm]]), nm))
  }, add = TRUE)
  Sys.setenv(PATH = paste(bin, old_path, sep = ":"), DUALDAG_FAKE_CODEX_CAPTURE = capture,
             DUALDAG_FAKE_CODEX_MODE = "ok")
  for (nm in keys) do.call(Sys.setenv, setNames(list("present"), nm))
  without_fake_llm({
    cfg <- list(provider = "codex_cli", model = "", api_key = "sk-should-not-be-sent")
    expect_equal(llm_timeout(cfg), 600)
    r <- llm_chat(cfg, "SYS-NOTE", "USER-NOTE")
    expect_equal(r$text, "hello from codex")
    expect_equal(r$model, "gpt-5.4")
    expect_equal(r$usage$input_tokens, 3)
    args <- readLines(paste0(capture, ".args"), warn = FALSE)
    expect_true(all(c("exec", "--skip-git-repo-check", "--sandbox", "read-only", "--ephemeral",
                      "--color", "never", "--json", "--output-last-message", "-") %in% args))
    expect_true("approval_policy=\"never\"" %in% args)
    expect_true("developer_instructions=\"SYS-NOTE\"" %in% args)
    expect_false("-m" %in% args)
    expect_false(any(grepl("sk-should-not-be-sent", args, fixed = TRUE)))
    stdin <- paste(readLines(paste0(capture, ".stdin"), warn = FALSE), collapse = "\n")
    expect_match(stdin, "USER-NOTE", fixed = TRUE)
    expect_match(stdin, "no tools", fixed = TRUE)
    expect_identical(stdin,"USER-NOTE\n\nYou have no tools and no workspace. Reply now with only the requested output.")
    options(dualdag.cli_tool_notice=su_cli_notice)
    llm_chat(cfg,"SYS-NOTE","USER-NOTE")
    su_stdin <- paste(readLines(paste0(capture,".stdin"),warn=FALSE),collapse="\n")
    options(dualdag.cli_tool_notice=old_tool_notice)
    expect_identical(su_stdin,paste0("USER-NOTE\n\n",su_cli_notice))
    expect_false(grepl("You have no tools and no workspace",su_stdin,fixed=TRUE))
    expect_false(grepl("System instructions", stdin, fixed = TRUE))
    env_names <- readLines(paste0(capture, ".env"), warn = FALSE)
    expect_false("OPENAI_API_KEY" %in% env_names)
    expect_false("CODEX_API_KEY" %in% env_names)
    expect_true("ANTHROPIC_API_KEY" %in% env_names)

    Sys.setenv(DUALDAG_FAKE_CODEX_MODE = "plain")
    plain <- llm_chat(cfg, "SYS-NOTE", "USER-NOTE")
    expect_equal(plain$model, "codex default")
    named <- llm_chat(modifyList(cfg, list(model = "gpt-5.3-codex")), "SYS-NOTE", "USER-NOTE")
    expect_equal(named$model, "gpt-5.3-codex")
    named_args <- readLines(paste0(capture, ".args"), warn = FALSE)
    expect_equal(named_args[which(named_args == "-m") + 1L], "gpt-5.3-codex")

    Sys.setenv(DUALDAG_FAKE_CODEX_MODE = "ok")
    long <- paste(rep("S", 20001), collapse = "")
    llm_chat(cfg, long, "USER-NOTE")
    long_args <- readLines(paste0(capture, ".args"), warn = FALSE)
    expect_false(any(grepl("developer_instructions", long_args, fixed = TRUE)))
    long_in <- paste(readLines(paste0(capture, ".stdin"), warn = FALSE), collapse = "\n")
    expect_true(startsWith(long_in, "System instructions"))
    expect_match(long_in, "USER-NOTE", fixed = TRUE)

    kept <- llm_chat(modifyList(cfg, list(cli_use_login = FALSE)), "SYS-NOTE", "USER-NOTE")
    expect_equal(kept$text, "hello from codex")
    kept_names <- readLines(paste0(capture, ".env"), warn = FALSE)
    expect_false("OPENAI_API_KEY" %in% kept_names)
    expect_false("CODEX_API_KEY" %in% kept_names)

    Sys.setenv(DUALDAG_FAKE_CODEX_MODE = "model")
    model_err <- tryCatch(llm_chat(cfg, "s", "u"), error = function(e) conditionMessage(e))
    expect_match(model_err, "not supported", fixed = TRUE)
    expect_false(grepl("not logged in", model_err, ignore.case = TRUE))
    Sys.setenv(DUALDAG_FAKE_CODEX_MODE = "login")
    expect_error(
      llm_chat(cfg, "s", "u"),
      "Not logged in. Run codex login in a terminal, then press Check login.",
      fixed = TRUE
    )
    Sys.setenv(DUALDAG_FAKE_CODEX_MODE = "longfail")
    err <- tryCatch(llm_chat(cfg, "s", "u"), error = function(e) conditionMessage(e))
    expect_match(err, "^Codex CLI failed: ")
    expect_lte(nchar(err), nchar("Codex CLI failed: ") + 400L)
    Sys.setenv(DUALDAG_FAKE_CODEX_MODE = "sleep")
    expect_error(llm_chat(modifyList(cfg, list(cli_timeout_s = 1)), "s", "u"), "timed out")
    Sys.setenv(PATH = "/usr/bin:/bin")
    expect_error(llm_chat(cfg, "s", "u"), "Codex CLI is not on PATH. Install codex and log in, then try again.", fixed = TRUE)
  })
})

test_that("claude_cli strips Anthropic key variables unless cli_use_login is false", {
  bin <- tempfile("claude-env-")
  dir.create(bin)
  capture <- tempfile("claude-env-cap-")
  script <- c(
    "#!/bin/sh",
    "capture=\"${DUALDAG_FAKE_CLAUDE_CAPTURE:-}\"",
    "if [ -n \"$capture\" ]; then",
    "  printf '%s\\n' \"$@\" > \"$capture.args\"",
    "  cat > \"$capture.stdin\"",
    "  : > \"$capture.env\"",
    "  for n in ANTHROPIC_API_KEY ANTHROPIC_AUTH_TOKEN OPENAI_API_KEY CODEX_API_KEY; do",
    "    if printenv \"$n\" >/dev/null 2>&1; then printf '%s\\n' \"$n\" >> \"$capture.env\"; fi",
    "  done",
    "fi",
    "printf '%s\\n' '{\"type\":\"result\",\"is_error\":false,\"result\":\"hello from claude\",\"usage\":{\"input_tokens\":1}}'",
    "exit 0"
  )
  writeLines(script, file.path(bin, "claude"))
  Sys.chmod(file.path(bin, "claude"), "0755")
  old_path <- Sys.getenv("PATH")
  keys <- c("OPENAI_API_KEY", "CODEX_API_KEY", "ANTHROPIC_API_KEY", "ANTHROPIC_AUTH_TOKEN")
  old_keys <- Sys.getenv(keys, unset = NA_character_)
  on.exit({
    Sys.setenv(PATH = old_path)
    Sys.unsetenv(c("DUALDAG_FAKE_CLAUDE_MODE", "DUALDAG_FAKE_CLAUDE_CAPTURE"))
    for (nm in keys) if (is.na(old_keys[[nm]])) Sys.unsetenv(nm) else do.call(Sys.setenv, setNames(list(old_keys[[nm]]), nm))
  }, add = TRUE)
  Sys.setenv(PATH = paste(bin, old_path, sep = ":"), DUALDAG_FAKE_CLAUDE_CAPTURE = capture)
  for (nm in keys) do.call(Sys.setenv, setNames(list("present"), nm))
  without_fake_llm({
    cfg <- list(provider = "claude_cli", model = "claude-haiku-5-5")
    r <- llm_chat(cfg, "SYS", "USER")
    expect_equal(r$text, "hello from claude")
    stripped <- readLines(paste0(capture, ".env"), warn = FALSE)
    expect_false("ANTHROPIC_API_KEY" %in% stripped)
    expect_false("ANTHROPIC_AUTH_TOKEN" %in% stripped)
    expect_true("OPENAI_API_KEY" %in% stripped)
    llm_chat(modifyList(cfg, list(cli_use_login = FALSE)), "SYS", "USER")
    kept <- readLines(paste0(capture, ".env"), warn = FALSE)
    expect_false("ANTHROPIC_API_KEY" %in% kept)
    expect_false("ANTHROPIC_AUTH_TOKEN" %in% kept)
  })
})

test_that("cli_status parses claude, codex, and grok without returning account fields", {
  bin <- tempfile("cli-status-")
  dir.create(bin)
  writeLines(c(
    "#!/bin/sh",
    "mode=\"${DUALDAG_FAKE_STATUS_MODE:-ok}\"",
    "if [ \"$1\" = \"--version\" ]; then",
    "  if [ \"$mode\" = \"sleep\" ]; then sleep 30; fi",
    "  echo \"2.1.316 (Claude Code)\"",
    "  exit 0",
    "fi",
    "if [ \"$mode\" = \"out\" ]; then printf '%s\\n' '{\"loggedIn\":false}'; exit 1; fi",
    "printf '%s\\n' '{\"loggedIn\":true,\"authMethod\":\"claude.ai\",\"email\":\"SECRETMAIL\",\"orgId\":\"ORGSECRET\",\"orgName\":\"ORGNAME\",\"subscriptionType\":\"SUBSECRET\"}'",
    "exit 0"
  ), file.path(bin, "claude"))
  writeLines(c(
    "#!/bin/sh",
    "mode=\"${DUALDAG_FAKE_STATUS_MODE:-ok}\"",
    "if [ \"$1\" = \"--version\" ]; then echo \"codex-cli 0.117.0\"; exit 0; fi",
    "if [ \"$mode\" = \"out\" ]; then echo \"Not logged in\"; exit 1; fi",
    "echo \"Logged in using ChatGPT\"",
    "exit 0"
  ), file.path(bin, "codex"))
  writeLines(c(
    "#!/bin/sh",
    "echo \"grok 9.9.9\"",
    "exit 0"
  ), file.path(bin, "grok"))
  Sys.chmod(list.files(bin, full.names = TRUE), "0755")
  old_path <- Sys.getenv("PATH")
  on.exit({
    Sys.setenv(PATH = old_path)
    Sys.unsetenv("DUALDAG_FAKE_STATUS_MODE")
  }, add = TRUE)
  Sys.setenv(PATH = paste(bin, old_path, sep = ":"), DUALDAG_FAKE_STATUS_MODE = "ok")
  cl <- cli_status("claude_cli")
  expect_true(cl$installed)
  expect_equal(cl$version, "2.1.316 (Claude Code)")
  expect_true(isTRUE(cl$logged_in))
  expect_equal(cl$method, "claude.ai")
  expect_equal(cl$hint, "")
  blob <- paste(unlist(cl), collapse = " ")
  expect_false(grepl("SECRETMAIL", blob, fixed = TRUE))
  expect_false(grepl("ORGSECRET", blob, fixed = TRUE))
  expect_false(grepl("ORGNAME", blob, fixed = TRUE))
  expect_false(grepl("SUBSECRET", blob, fixed = TRUE))

  cx <- cli_status("codex_cli")
  expect_true(cx$installed)
  expect_equal(cx$version, "codex-cli 0.117.0")
  expect_true(isTRUE(cx$logged_in))
  expect_equal(cx$method, "ChatGPT")

  gk <- cli_status("grok_cli")
  expect_true(gk$installed)
  expect_equal(gk$version, "grok 9.9.9")
  expect_null(gk$logged_in)
  expect_equal(gk$hint, "")

  Sys.setenv(DUALDAG_FAKE_STATUS_MODE = "out")
  cl_out <- cli_status("claude_cli")
  expect_false(isTRUE(cl_out$logged_in))
  expect_equal(cl_out$hint, "Not logged in. Run claude login in a terminal, then press Check login.")
  cx_out <- cli_status("codex_cli")
  expect_false(isTRUE(cx_out$logged_in))
  expect_equal(cx_out$hint, "Not logged in. Run codex login in a terminal, then press Check login.")

  Sys.setenv(DUALDAG_FAKE_STATUS_MODE = "sleep")
  timed <- cli_status("claude_cli", timeout = 1)
  expect_true(timed$installed)
  expect_equal(timed$hint, "The CLI check timed out.")

  Sys.setenv(PATH = "/usr/bin:/bin", DUALDAG_FAKE_STATUS_MODE = "ok")
  missing <- cli_status("codex_cli")
  expect_false(missing$installed)
  expect_match(missing$hint, "not on PATH", fixed = TRUE)
  grok_missing <- cli_status("grok_cli")
  expect_null(grok_missing$logged_in)
})
