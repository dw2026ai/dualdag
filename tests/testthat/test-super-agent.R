app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash="/")
for (f in list.files(file.path(app_root,"R"),full.names=TRUE,pattern="[.]R$")) sys.source(f,envir=.GlobalEnv)
source(file.path(testthat::test_path(),"..","fixtures","helper.R"))

test_that("Super agent tools stay tab-scoped and settings upload copies only allowed files", {
  p<-open_project(tempfile("super-agent-")); A<-create_app(app_root,p$dir,18862L,"super-agent-test",load_config())
  writeBin(as.raw(9),file.path(p$dir,"data","prior.xpt"))
  folder<-tempfile("uploads-"); dir.create(folder); writeBin(as.raw(c(1,2,3)),file.path(folder,"dm.xpt")); writeBin(as.raw(2),file.path(folder,"prior.xpt")); writeLines("skip",file.path(folder,"notes.txt"))
  result<-super_agent_run_action(A,p,"settings","upload_folder",list(path=folder))
  expect_true(result$ok); expect_true(file.exists(file.path(p$dir,"data","dm.xpt")))
  expect_true(all(c("notes.txt","prior.xpt") %in% result$skipped))
  expect_equal(readBin(file.path(p$dir,"data","prior.xpt"),"raw",n=1),as.raw(9))
})

test_that("folder replacement preserves an existing spec file omitted from the folder", {
  p<-open_project(tempfile("super-agent-spec-")); A<-create_app(app_root,p$dir,18862L,"super-agent-test",load_config())
  project_upload_spec(p,read_file_raw(file.path(app_root,"tests/fixtures/params_variable.csv")),read_file_raw(file.path(app_root,"tests/fixtures/params_parameter.csv")))
  parameter_before<-file_md5(file.path(p$dir,"spec","parameter.csv"))
  folder<-tempfile("variable-only-"); dir.create(folder); file.copy(file.path(app_root,"tests/fixtures/clean_variable.csv"),file.path(folder,"variable.csv"))
  result<-super_agent_run_action(A,p,"settings","upload_folder",list(path=folder,replace=TRUE))
  expect_true(result$ok); expect_equal(file_md5(file.path(p$dir,"spec","parameter.csv")),parameter_before)
})

test_that("Super agent dataset reads cap sampled rows at twenty", {
  p<-open_project(tempfile("super-agent-rows-")); A<-create_app(app_root,p$dir,18862L,"super-agent-test",load_config())
  f<-tempfile(fileext=".xpt"); haven::write_xpt(data.frame(ID=seq_len(25)),f); project_upload_data(p,"large.xpt",read_file_raw(f))
  result<-super_agent_run_action(A,p,"data","dataset_view",list(id="in:large.xpt"))
  expect_length(result$rows,20L); expect_length(result$columns,1L)
})

test_that("Super agent model prompt is isolated and never includes API keys", {
  original<-llm_chat; captured<-""; assign("llm_chat",function(cfg,system,user,...) { captured<<-paste(system,user); list(text='{"tool":"reply","text":"Which folder?"}',model="scripted",usage=list(input_tokens=2,output_tokens=3)) },.GlobalEnv)
  on.exit(assign("llm_chat",original,.GlobalEnv),add=TRUE)
  old<-Sys.getenv("DUALDAG_LLM_API_KEY",unset=NA_character_); Sys.setenv(DUALDAG_LLM_API_KEY="super-agent-secret")
  on.exit(if(is.na(old))Sys.unsetenv("DUALDAG_LLM_API_KEY") else Sys.setenv(DUALDAG_LLM_API_KEY=old),add=TRUE)
  old_home<-Sys.getenv("DUALDAG_HOME",unset=NA_character_); home<-tempfile("super-home-"); dir.create(home); Sys.setenv(DUALDAG_HOME=home)
  on.exit(if(is.na(old_home))Sys.unsetenv("DUALDAG_HOME") else Sys.setenv(DUALDAG_HOME=old_home),add=TRUE)
  ans<-super_agent_chat_job("/tmp","settings",list(list(role="user",text="Upload my folder")))
  expect_equal(ans$text,"Which folder?"); expect_false(grepl("super-agent-secret",captured,fixed=TRUE))
  expect_true(grepl("Never start a new DAG",captured,fixed=TRUE))
  summary<-super_agent_chat_job("/tmp","settings",list(list(role="assistant",text="Uploaded.")),tool_result=list(copied="dm.xpt"))
  expect_equal(summary$text,"Which folder?"); expect_true(grepl("dm.xpt",captured,fixed=TRUE))
  expect_lte(nchar(super_agent_cap(paste(rep("x",5000),collapse=""))),4000)
})

test_that("browser-side tools are converted to tab-scoped replies", {
  original<-llm_chat; requested<-"run_whole_dataset"
  assign("llm_chat",function(...)list(text=jsonlite::toJSON(list(tool=requested,args=list(side="primary"),text="Starting."),auto_unbox=TRUE),model="scripted"),.GlobalEnv)
  on.exit(assign("llm_chat",original,.GlobalEnv),add=TRUE)
  old_key<-Sys.getenv("DUALDAG_LLM_API_KEY",unset=NA_character_); Sys.setenv(DUALDAG_LLM_API_KEY="fixture-key")
  on.exit(if(is.na(old_key))Sys.unsetenv("DUALDAG_LLM_API_KEY") else Sys.setenv(DUALDAG_LLM_API_KEY=old_key),add=TRUE)
  data_reply<-super_agent_chat_job("/tmp","data",list(list(role="user",text="Run this dataset")))
  expect_equal(data_reply$tool,"reply"); expect_match(data_reply$text,"not available on the data tab")
  requested<-"dag_undo"
  settings_reply<-super_agent_chat_job("/tmp","settings",list(list(role="user",text="Undo the DAG")))
  expect_equal(settings_reply$tool,"reply"); expect_match(settings_reply$text,"not available on the settings tab")
  requested<-"mystery_tool"
  unknown<-super_agent_chat_job("/tmp","data",list(list(role="user",text="Do it")))
  expect_equal(unknown$tool,"reply"); expect_match(unknown$text,"not available on the data tab")
})

test_that("set_model validates provider, uses shared defaults, and preserves invalid settings", {
  old_home<-Sys.getenv("DUALDAG_HOME",unset=NA_character_); home<-tempfile("super-model-home-"); dir.create(home); Sys.setenv(DUALDAG_HOME=home)
  on.exit(if(is.na(old_home))Sys.unsetenv("DUALDAG_HOME") else Sys.setenv(DUALDAG_HOME=old_home),add=TRUE)
  cfg<-config_defaults(); cfg$model<-"saved-model"; save_config(cfg)
  config_path<-file.path(home,"config.json"); before<-readBin(config_path,"raw",n=file.info(config_path)$size)
  p<-open_project(tempfile("super-model-project-")); A<-create_app(app_root,p$dir,18862L,"super-agent-test",load_config())
  refused<-super_agent_run_action(A,p,"settings","set_model",list(provider="made_up",model="bad"))
  expect_false(refused$ok); expect_identical(readBin(config_path,"raw",n=file.info(config_path)$size),before)
  saved<-super_agent_run_action(A,p,"settings","set_model",list(provider="claude_cli",model=""))
  expect_equal(saved$provider,"claude_cli")
  actual<-load_config(); expect_equal(actual$provider,"claude_cli"); expect_equal(actual$model,"claude-sonnet-5-5"); expect_true(actual$cli_use_login)
})

test_that("login check uses the Settings guidance when no credential is configured", {
  p<-open_project(tempfile("super-login-")); A<-create_app(app_root,p$dir,18862L,"super-agent-test",load_config())
  original<-llm_ready; assign("llm_ready",function(...)FALSE,.GlobalEnv); on.exit(assign("llm_ready",original,.GlobalEnv),add=TRUE)
  result<-super_agent_run_action(A,p,"settings","test_login",list())
  expect_match(result$error,"Choose a model in Settings")
})

test_that("cross-tab, forbidden and busy Super agent actions are refused", {
  p<-open_project(tempfile("super-agent-boundary-")); A<-create_app(app_root,p$dir,18862L,"super-agent-test",load_config())
  expect_false(super_agent_run_action(A,p,"data","set_model",list(provider="openai",model="x"))$ok)
  expect_false(super_agent_run_action(A,p,"settings","start_new_dag",list())$ok)
  A$jobs[["busy"]]<-list(kind="generate")
  expect_match(super_agent_run_action(A,p,"settings","set_model",list(provider="openai",model="x"))$error,"job or batch")
})
