# Small, tab-scoped assistant. Its transcript lives in browser localStorage; it is
# deliberately not connected to Coder, SU, or side prompt history.
super_agent_tools <- function(tab) {
  tools <- list(
    settings=c("settings","upload_folder","set_model","test_login"),
    spec=c("dag_ask","dag_undo","add_to_run_queue","remove_from_run_queue","run_queue"),
    run=c("program_status","run_whole_dataset","step_status"),
    data=c("datasets","dataset_view","open_dataset"),
    mismatches=c("mismatch_summary","mismatch_rows","open_variable"))
  tools[[tab]] %||% character()
}

super_agent_system <- function(tab) paste(
  "You are the Super agent for the current Dualdag tab. Return JSON only: {tool,args,text}.",
  "Use only a tool listed for this tab. Ask a short question and take no action if details are unclear.",
  "Never start a new DAG, delete data/spec, write R programs, use shell/general file access, or read API keys. Refuse those requests.",
  "Settings: settings, upload_folder(path), set_model(provider,model), test_login.",
  "Spec & checks: dag_ask(message), dag_undo, add_to_run_queue(nodes), remove_from_run_queue(nodes), run_queue.",
  "Run: program_status(side), run_whole_dataset(side), step_status(side,node).",
  "Data: datasets, dataset_view(id) (maximum 20 rows), open_dataset(id).",
  "Mismatches: mismatch_summary, mismatch_rows(node) (maximum 20 rows), open_variable(node).",
  paste("Current tab:",tab), sep="\n")

super_agent_chat_job <- function(project_dir, tab, messages, tool_result=NULL) {
  cfg <- with_cfg(); if (!llm_ready(cfg)) stop("Choose a model in Settings.")
  old <- getOption("dualdag.llm_log"); options(dualdag.llm_log=list(dir=file.path(project_dir,".dualdag","llm_log"),task="super-agent",node=tab,side="")); on.exit(options(dualdag.llm_log=old),add=TRUE)
  prior <- getOption("dualdag.cli_tool_notice"); options(dualdag.cli_tool_notice="The app executes only the listed tab actions when you return JSON."); on.exit(options(dualdag.cli_tool_notice=prior),add=TRUE)
  system<-super_agent_system(tab)
  if(!is.null(tool_result)) system<-paste(system,"The requested action has already run. Do not call another tool; summarize its result for the user.",sep="\n")
  raw <- llm_chat(cfg,system,to_json(list(messages=messages,tool_result=tool_result)),max_tokens=1400L,json=TRUE)
  ans <- tryCatch(parse_json_reply(raw$text),error=function(e)list(tool="reply",text=raw$text %||% ""))
  tool<-as.character(ans$tool %||% "reply")[1]; reply<-as.character(ans$text %||% "")[1]
  if(!identical(tool,"reply") && !tool %in% super_agent_tools(tab)) {
    tool<-"reply"; reply<-paste0("That action is not available on the ",tab," tab.")
  }
  list(tool=tool,args=ans$args %||% list(),text=reply,usage=llm_usage_summary(raw$usage,cfg$provider %||% ""),model=raw$model %||% "")
}

super_agent_cap <- function(x) {
  if (is.character(x)) { x[nchar(x)>4000L] <- paste0(substr(x[nchar(x)>4000L],1L,3960L),"\n[truncated at 4000 characters]"); return(x) }
  if (is.list(x)) return(lapply(x,super_agent_cap))
  x
}

super_agent_settings <- function(cfg) {
  out <- config_public(cfg)
  out$api_key_hint <- NULL
  out
}

super_agent_run_action <- function(A,p,tab,tool,args) {
  if (!tool %in% super_agent_tools(tab)) return(list(ok=FALSE,error="Refused: that action is unavailable from this tab."))
  if (tool %in% c("upload_folder","set_model","dag_ask","dag_undo","add_to_run_queue","remove_from_run_queue","run_queue","run_whole_dataset") && (length(A$jobs)>0 || batch_running(A))) return(list(ok=FALSE,error="Refused while a job or batch is running."))
  ds <- as.character(args$dataset %||% "")[1]
  tryCatch({
    if(tool=="settings") return(super_agent_settings(A$cfg))
    if(tool=="set_model") { cfg<-config_set_model(A$cfg,args$provider %||% A$cfg$provider,args$model %||% ""); save_config(cfg); A$cfg<-cfg; return(super_agent_settings(cfg)) }
    if(tool=="test_login") { if(!llm_ready(A$cfg))return(list(ok=FALSE,error="Choose a model in Settings.")); return(list(ok=TRUE,available=TRUE)) }
    if(tool=="upload_folder") {
      folder<-normalizePath(as.character(args$path %||% "")[1],mustWork=TRUE)
      if(!dir.exists(folder)) stop("Folder not found.")
      fs<-list.files(folder,all.files=FALSE,full.names=TRUE,recursive=FALSE,no..=TRUE)
      copied<-skipped<-character(); vars<-pars<-NULL
      for(f in fs) {
        nm<-basename(f); low<-tolower(nm)
        if(low %in% c("variable.csv","parameter.csv")) { if(low=="variable.csv") vars<-f else pars<-f; next }
        if(grepl("^[A-Za-z0-9][A-Za-z0-9_.-]*\\.xpt$",nm,ignore.case=TRUE)) {
          dest<-file.path(p$dir,"data",tolower(nm)); if(file.exists(dest) && !isTRUE(args$replace)) { skipped<-c(skipped,nm); next }
          project_upload_data(p,nm,read_file_raw(f)); copied<-c(copied,nm)
        } else skipped<-c(skipped,nm)
      }
      spec_dir<-file.path(p$dir,"spec"); old_var<-file.path(spec_dir,"variable.csv"); old_par<-file.path(spec_dir,"parameter.csv")
      replace<-isTRUE(args$replace); write_var<-!is.null(vars) && (replace || !file.exists(old_var))
      write_par<-!is.null(pars) && (replace || !file.exists(old_par))
      if(!is.null(vars) && !write_var) skipped<-c(skipped,"variable.csv (already exists)")
      if(!is.null(pars) && !write_par) skipped<-c(skipped,"parameter.csv (already exists)")
      if(write_var || write_par) {
        var_bytes<-if(write_var)read_file_raw(vars) else if(file.exists(old_var))read_file_raw(old_var) else NULL
        par_bytes<-if(write_par)read_file_raw(pars) else if(file.exists(old_par))read_file_raw(old_par) else NULL
        if(is.null(var_bytes)) skipped<-c(skipped,"parameter.csv (variable.csv required)") else {
          project_upload_spec(p,var_bytes,par_bytes)
          if(write_var)copied<-c(copied,"variable.csv")
          if(write_par)copied<-c(copied,"parameter.csv")
        }
      }
      return(list(ok=TRUE,copied=copied,skipped=skipped))
    }
    if(tool=="dag_undo") return(list(ok=FALSE,error="Undo is available through the existing Spec & checks Undo control."))
    if(tool=="dag_ask") return(list(ok=FALSE,error="Use the existing Spec & checks agent for this request."))
    if(tool=="datasets") { side<-as.character(args$side %||% "primary")[1]; if(!side %in% c("primary","validation"))stop("Side must be primary or validation."); return(list(datasets=list_viewer_datasets(p,side=side))) }
    if(tool=="dataset_view") { x<-view_dataset(p,id=args$id,offset=0,limit=20); x$rows<-head(x$rows,20); return(x) }
    if(tool=="mismatch_summary") {
      spec<-read_project_spec(p); ids<-dataset_node_ids(p,spec,ds)
      rows<-lapply(ids,function(id){st<-get_status(p,ds,id); cmp<-tryCatch(read_json(file.path(node_run_dir(p,ds,id),"compare.json")),error=function(e)NULL); list(node=id,status=st$level %||% "none",mismatch=is.list(cmp)&&identical(cmp$match,FALSE),summary=if(is.list(cmp))cmp$summary %||% "" else "")})
      return(list(dataset=ds,variables=rows))
    }
    if(tool=="mismatch_rows") { x<-node_inputs(p,ds,canonical_node_id(ds,args$node),page=1,size=20); x$rows<-head(x$rows,20); return(x) }
    if(tool=="program_status") {
      if(!ds %in% (load_project_spec(p)$datasets %||% character())) stop("Unknown dataset.")
      ids<-dataset_node_ids(p,read_project_spec(p),ds)
      side<-as.character(args$side %||% "primary")[1]; if(!side %in% c("primary","validation"))stop("Side must be primary or validation.")
      steps <- lapply(ids, function(id) { st <- get_status(p,ds,id); list(node=id,level=st$level %||% "none",side=st$sides[[side]] %||% list()) })
      return(list(dataset=ds,side=side,steps=steps))
    }
    if(tool=="step_status") {
      id<-canonical_node_id(ds,args$node); side<-as.character(args$side %||% "primary")[1]; if(!side %in% c("primary","validation"))stop("Side must be primary or validation.")
      st<-get_status(p,ds,id); return(list(dataset=ds,node=id,level=st$level %||% "none",problem=st$problem,side=st$sides[[side]] %||% list()))
    }
    list(ok=FALSE,error="Action is not available yet.")
  },error=function(e)list(ok=FALSE,error=conditionMessage(e)))
}
