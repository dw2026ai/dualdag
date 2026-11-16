source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))
tiers_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (tiers_file in c("config.R", "admin.R")) sys.source(file.path(tiers_root, "R", tiers_file), envir=.GlobalEnv)

test_that("provider model tiers have the requested defaults", {
  expect_identical(config_model_tiers("claude_cli"), list(high="claude-opus-5-5",low="claude-sonnet-5-5"))
  expect_identical(config_model_tiers("anthropic"), list(high="claude-opus-5-5",low="claude-sonnet-5-5"))
  expect_identical(config_model_tiers("codex_cli"), list(high="gpt-6.1-terra",low="gpt-6-luna"))
  expect_identical(config_model_tiers("openai"), list(high="gpt-6.1-terra",low="gpt-6-luna"))
  expect_identical(config_model_tiers("grok_cli"), list(high="grok-4.7",low="grok-4.7"))
  expect_identical(config_model_tiers("openai_compatible"), list(high="",low=""))
})

test_that("blank tiers fill independently and old model-only configs upgrade", {
  cfg <- config_apply_model_defaults(list(provider="codex_cli",model="",strong_model=""))
  expect_identical(cfg$model,"gpt-6-luna"); expect_identical(cfg$strong_model,"gpt-6.1-terra")
  cfg <- config_apply_model_defaults(list(provider="claude_cli",model="custom-low",strong_model=""))
  expect_identical(cfg$model,"custom-low"); expect_identical(cfg$strong_model,"claude-opus-5-5")
  cfg <- config_apply_model_defaults(list(provider="openai_compatible",model="custom",strong_model=""))
  expect_identical(cfg$model,"custom"); expect_identical(cfg$strong_model,"custom")
  old <- file.path(config_dir(),"config.json")
  dir.create(dirname(old),recursive=TRUE,showWarnings=FALSE)
  writeLines('{"provider":"openai","model":"saved-low"}',old)
  loaded <- load_config()
  expect_identical(loaded$model,"saved-low"); expect_identical(loaded$strong_model,"gpt-6.1-terra")
})

test_that("OpenAI API accepts the new Codex high-end model", {
  cfg <- list(provider="openai",base_url="",model="gpt-6.1-terra",api_key="test-key",llm_timeout_s=30)
  expect_identical(llm_request(cfg,"system","user")$body$data$model,"gpt-6.1-terra")
  expect_true("gpt-6.1-terra" %in% codex_cli_models())
})

test_that("App Admin passes high-end models to fake provider CLIs", {
  base <- tempfile("admin-codex-tier-"); app <- file.path(base,"app"); project <- file.path(base,"project"); bin <- file.path(base,"bin")
  dir.create(app,recursive=TRUE); dir.create(project); dir.create(bin)
  on.exit(unlink(base,recursive=TRUE),add=TRUE)
  args_file <- file.path(base,"args.txt")
  stub <- file.path(bin,"codex")
  # Codex writes its final message to the supplied path; capture argv and derive that path.
  writeLines(c("#!/bin/sh", paste0("printf '%s\\n' \"$@\" > '",args_file,"'"),
    "prev=''", "for arg in \"$@\"; do if [ \"$prev\" = '--output-last-message' ] || [ \"$prev\" = '-o' ]; then echo ok > \"$arg\"; fi; prev=\"$arg\"; done",
    "echo '{\"type\":\"item.completed\",\"item\":{\"type\":\"agent_message\",\"text\":\"ok\"}}'"),stub)
  Sys.chmod(stub,"0755")
  old_path <- Sys.getenv("PATH"); on.exit(Sys.setenv(PATH=old_path),add=TRUE)
  Sys.setenv(PATH=paste(bin,old_path,sep=.Platform$path.sep))
  result <- admin_cli_turn(app,project,"codex","test","system",high_model="gpt-6.1-terra")
  argv <- readLines(args_file)
  expect_equal(argv[match("-m",argv)+1L],"gpt-6.1-terra")
  expect_identical(result$reply,"ok")

  claude_stub <- file.path(bin,"claude")
  writeLines(c("#!/bin/sh", paste0("printf '%s\\n' \"$@\" > '",args_file,"'"),
    "echo '{\"type\":\"result\",\"result\":\"ok\"}'"),claude_stub)
  Sys.chmod(claude_stub,"0755")
  admin_cli_turn(app,project,"claude","test","system",high_model="claude-opus-5-5")
  argv <- readLines(args_file)
  expect_equal(argv[match("--model",argv)+1L],"claude-opus-5-5")

  grok_stub <- file.path(bin,"grok")
  writeLines(c("#!/bin/sh", paste0("printf '%s\\n' \"$@\" > '",args_file,"'"),
    "echo '{\"text\":\"ok\",\"model\":\"grok-4.7\"}'"),grok_stub)
  Sys.chmod(grok_stub,"0755")
  admin_cli_turn(app,project,"grok","test","system",high_model="grok-4.7")
  argv <- readLines(args_file)
  expect_equal(argv[match("-m",argv)+1L],"grok-4.7")
})
