admin_api_test_root <- normalizePath(file.path(testthat::test_path(),"..",".."))
admin_api_rfiles <- list.files(file.path(admin_api_test_root,"R"),pattern="[.]R$",full.names=TRUE)
admin_api_utils <- admin_api_rfiles[grepl("utils[.]R$",admin_api_rfiles)]
for (admin_api_file in c(admin_api_utils,admin_api_rfiles[!admin_api_rfiles%in%admin_api_utils])) sys.source(admin_api_file,envir=.GlobalEnv)
rm(admin_api_file)
admin_api_fixture <- function() {
  base<-tempfile("admin-api-fixture-");app<-file.path(base,"app");project<-file.path(base,"project")
  dir.create(file.path(app,"R"),recursive=TRUE);dir.create(file.path(project,"programs"),recursive=TRUE)
  dir.create(file.path(project,".dualdag","state"),recursive=TRUE)
  writeLines("app v1",file.path(app,"run.R"));writeLines("program v1",file.path(project,"programs","main.R"))
  writeLines('{"nodes":{}}',file.path(project,".dualdag","state","state.json"))
  list(base=base,app=app,project=project)
}

test_that("API Admin tools round-trip for Anthropic and chat-completions", {
  f <- admin_api_fixture(); on.exit(unlink(f$base,recursive=TRUE),add=TRUE)
  old_home <- Sys.getenv("DUALDAG_HOME"); Sys.setenv(DUALDAG_HOME=tempfile("admin-api-home-")); dir.create(Sys.getenv("DUALDAG_HOME"))
  on.exit(Sys.setenv(DUALDAG_HOME=old_home),add=TRUE); save_api_key("test-only-key")
  writeLines("before",file.path(f$project,"note.txt"))
  for (provider in c("anthropic","openai")) {
    seen <- list(); round <- 0L
    options(dualdag.admin_api_request=function(cfg,system,messages,tools) {
      round <<- round+1L; seen[[round]] <<- list(messages=messages,tools=tools)
      expect_identical(system,"test boundary")
      if (provider=="anthropic") {
        if (round==1L) list(model="mock-anthropic",usage=list(input_tokens=1),content=list(list(type="tool_use",id="tool-1",name="edit_file",input=list(path="note.txt",old="before",new="after"))))
        else { expect_true(any(vapply(messages,function(m) identical(m$role,"user") && is.list(m$content) && any(vapply(m$content,function(x) is.list(x) && identical(x$type,"tool_result"),TRUE)),TRUE))); list(model="mock-anthropic",usage=list(output_tokens=1),content=list(list(type="text",text="Done."))) }
      } else {
        if (round==1L) list(model="mock-openai",usage=list(prompt_tokens=1),choices=list(list(message=list(role="assistant",content=NULL,tool_calls=list(list(id="call-1",type="function",`function`=list(name="edit_file",arguments='{"path":"note.txt","old":"before","new":"after"}')))))))
        else { expect_true(any(vapply(messages,function(m) m$role=="tool" && grepl('"ok":true',m$content,fixed=TRUE),TRUE))); list(model="mock-openai",usage=list(completion_tokens=1),choices=list(list(message=list(role="assistant",content="Done.")))) }
      }
    })
    result <- admin_api_turn(f$app,f$project,provider,"change note","test boundary",high_model="strong-test")
    expect_identical(result$reply,"Done."); expect_equal(round,2L); expect_identical(readLines(file.path(f$project,"note.txt")),"after")
    expect_true(length(result$history)>=3L); expect_equal(result$model,paste0("mock-",if(provider=="anthropic")"anthropic" else "openai"))
    expect_true(length(seen[[1]]$tools)==6L)
    tool_defs <- admin_api_tool_definitions(provider)
    expect_identical(if(provider=="anthropic")tool_defs[[1]]$name else tool_defs[[1]][["function"]]$name,"list_dir")
    writeLines("before",file.path(f$project,"note.txt"))
  }
  options(dualdag.admin_api_request=NULL)
})

test_that("OpenAI final assistant history omits empty tool_calls from the next request", {
  f <- admin_api_fixture(); on.exit(unlink(f$base,recursive=TRUE),add=TRUE)
  scratch_home <- tempfile("admin-api-openai-history-home-"); dir.create(scratch_home)
  old_home <- Sys.getenv("DUALDAG_HOME"); Sys.setenv(DUALDAG_HOME=scratch_home)
  on.exit(Sys.setenv(DUALDAG_HOME=old_home),add=TRUE); save_api_key("test-only-key")
  bodies <- list(); round <- 0L
  options(dualdag.admin_api_http=function(cfg,body) {
    bodies[[length(bodies)+1L]] <<- body; round <<- round+1L
    if (round==1L) list(model="mock-openai",choices=list(list(message=list(role="assistant",content=NULL,tool_calls=list(list(id="call-1",type="function",`function`=list(name="list_dir",arguments='{"path":"."}')))))))
    else list(model="mock-openai",choices=list(list(message=list(role="assistant",content="Done."))))
  })
  on.exit(options(dualdag.admin_api_http=NULL),add=TRUE)

  result <- admin_api_turn(f$app,f$project,"openai","list files","system",high_model="strong-test")
  second_body <- jsonlite::toJSON(bodies[[2]],auto_unbox=TRUE,null="null")
  expect_equal(round,2L)
  expect_false(grepl('"tool_calls"\\s*:\\s*\\[\\]',as.character(second_body)))
  expect_identical(result$reply,"Done.")
})

test_that("API Admin edit requires exactly one match and refuses the key file", {
  f <- admin_api_fixture(); on.exit(unlink(f$base,recursive=TRUE),add=TRUE)
  key_home <- tempfile("admin-api-key-home-"); dir.create(key_home); old_home <- Sys.getenv("DUALDAG_HOME"); Sys.setenv(DUALDAG_HOME=key_home)
  on.exit(Sys.setenv(DUALDAG_HOME=old_home),add=TRUE); save_api_key("private-test-key")
  writeLines("repeat repeat",file.path(f$project,"note.txt"))
  expect_error(admin_api_tool("edit_file",list(path="note.txt",old="repeat",new="once"),f$app,f$project),"found 2")
  expect_error(admin_api_tool("read_file",list(path=api_key_file()),f$app,f$project),"not allowed")
  expect_error(admin_api_tool("write_file",list(path=api_key_file(),content="overwrite"),f$app,f$project),"not allowed")
  expect_identical(readLines(api_key_file()),"private-test-key")
})

test_that("API Admin tool loop stops at forty provider steps", {
  f <- admin_api_fixture(); on.exit(unlink(f$base,recursive=TRUE),add=TRUE)
  key_home <- tempfile("admin-api-cap-home-"); dir.create(key_home); old_home <- Sys.getenv("DUALDAG_HOME"); Sys.setenv(DUALDAG_HOME=key_home)
  on.exit(Sys.setenv(DUALDAG_HOME=old_home),add=TRUE); save_api_key("test-only-key")
  calls <- 0L
  options(dualdag.admin_api_request=function(...) { calls <<- calls+1L; list(content=list(list(type="tool_use",id=paste0("t",calls),name="list_dir",input=list(path=".")))) })
  expect_error(admin_api_turn(f$app,f$project,"anthropic","loop","system",high_model="strong"),"40-step limit")
  expect_equal(calls,40L); options(dualdag.admin_api_request=NULL)
})

test_that("API Admin turn changes are covered by snapshot and undo", {
  f <- admin_api_fixture(); on.exit(unlink(f$base,recursive=TRUE),add=TRUE)
  key_home <- tempfile("admin-api-undo-home-"); dir.create(key_home); old_home <- Sys.getenv("DUALDAG_HOME"); Sys.setenv(DUALDAG_HOME=key_home)
  on.exit(Sys.setenv(DUALDAG_HOME=old_home),add=TRUE); save_api_key("test-only-key")
  options(dualdag.admin_api_request=function(cfg,system,messages,tools) {
    if (length(messages)==1L) list(content=list(list(type="tool_use",id="write-1",name="write_file",input=list(path="programs/main.R",content="changed"))))
    else list(content=list(list(type="text",text="Updated.")))
  })
  before <- admin_manifest(f$app,f$project); snap <- admin_snapshot(f$app,f$project)
  result <- admin_api_turn(f$app,f$project,"anthropic","write note","system",high_model="strong")
  admin_finish_snapshot(f$app,f$project,snap)
  changed <- admin_changed(before,admin_manifest(f$app,f$project))
  expect_true("project/programs/main.R" %in% changed)
  expect_identical(read_utf8(file.path(f$project,"programs","main.R")),"changed")
  undone <- admin_undo(f$app,f$project)
  expect_true(undone$ok); expect_identical(readLines(file.path(f$project,"programs","main.R")),"program v1")
  expect_identical(result$reply,"Updated."); options(dualdag.admin_api_request=NULL)
})

test_that("API Admin uses the same requested boundary language as CLI Admin", {
  f<-admin_api_fixture();on.exit(unlink(f$base,recursive=TRUE),add=TRUE)
  prompt<-admin_system_prompt(f$app,f$project)
  expect_match(prompt,"This boundary is requested and is not enforced")
  expect_match(prompt,"Do not read ~/.dualdag/api_key")
})

test_that("API provider request builders use their native tool formats", {
  captured<-list()
  options(dualdag.admin_api_http=function(cfg,body) { captured[[cfg$provider]]<<-body; list(ok=TRUE) })
  messages<-list(list(role="user",content="Hi"))
  for(provider in c("anthropic","openai","openai_compatible")) {
    cfg<-list(provider=provider,model="small",strong_model="strong",base_url="https://example.test/v1",api_key="mock-only")
    admin_api_request(cfg,"system prompt",messages,admin_api_tool_definitions(provider))
  }
  expect_identical(captured$anthropic$system,"system prompt")
  expect_identical(captured$anthropic$tools[[1]]$name,"list_dir")
  expect_identical(captured$openai$tools[[1]]$type,"function")
  expect_identical(captured$openai$tools[[1]][["function"]]$name,"list_dir")
  expect_true("max_completion_tokens" %in% names(captured$openai))
  expect_true("max_tokens" %in% names(captured$openai_compatible))
  expect_identical(captured$openai$messages[[1]]$role,"system")
  options(dualdag.admin_api_http=NULL)
})

test_that("unset DUALDAG_HOME uses only the scratch HOME for API key paths", {
  scratch_home<-tempfile("admin-api-home-blank-");dir.create(scratch_home)
  old_home<-Sys.getenv("HOME");old_dualdag<-Sys.getenv("DUALDAG_HOME",unset=NA_character_)
  Sys.setenv(HOME=scratch_home);Sys.unsetenv("DUALDAG_HOME")
  on.exit({Sys.setenv(HOME=old_home);if(is.na(old_dualdag))Sys.unsetenv("DUALDAG_HOME")else Sys.setenv(DUALDAG_HOME=old_dualdag)},add=TRUE)
  save_api_key("scratch-key")
  expect_true(startsWith(normalizePath(api_key_file()),normalizePath(scratch_home)))
  expect_error(admin_api_tool("run_command",list(command="cat $HOME/.dualdag/api_key"),"/tmp","/tmp"),"not allowed")
})

test_that("Admin state advertises API providers only when a key is configured", {
  f<-admin_api_fixture();on.exit(unlink(f$base,recursive=TRUE),add=TRUE)
  scratch<-tempfile("admin-api-status-home-");dir.create(scratch);old<-Sys.getenv("DUALDAG_HOME");Sys.setenv(DUALDAG_HOME=scratch)
  on.exit(Sys.setenv(DUALDAG_HOME=old),add=TRUE)
  A<-new.env(parent=emptyenv());A$project<-list(dir=f$project);A$app_dir<-f$app
  A$cfg<-list(provider="openai",model="base-model",strong_model="strong-model")
  A$admin_session<-list(turns=list());A$admin_active<-FALSE
  A$admin_cli_status_cache<-list(time=as.numeric(Sys.time()),values=list(claude=list(),codex=list(),grok=list()))
  status<-function() from_json(handle_api(A,NULL,"/api/admin/state","GET",list())$body)$status
  expect_false(status()$anthropic$available)
  save_api_key("scratch-key")
  ready<-status()
  expect_true(ready$anthropic$available);expect_identical(ready$anthropic$model,"claude-opus-5-5")
  expect_true(ready$openai$available);expect_identical(ready$openai$model,"strong-model")
  expect_true(ready$openai_compatible$available);expect_identical(ready$openai_compatible$model,"base-model")
})
