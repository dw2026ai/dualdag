# Display-only attempt history for one Coder session. These files are never
# read by prompt builders.
coder_window_dir <- function(project, dataset, node, side) {
  side_run_dir(project_dir_of(project), dataset, node, side)
}

coder_window_context_path <- function(project, dataset, node, side) {
  file.path(coder_window_dir(project, dataset, node, side), "coder_context.json")
}

coder_window_process_path <- function(project, dataset, node, side) {
  file.path(coder_window_dir(project, dataset, node, side), "coder_process.json")
}

su_edited_path <- function(project, dataset, node, side) file.path(coder_window_dir(project,dataset,node,side), "su_edited.json")
su_tool_names <- function() c("read_code","read_history","read_spec","peek_data","peek_frame","run_check","write_program","reply")

su_cap_log <- function(x) {
  x <- as.character(x %||% "")[1]; note <- "\n[truncated at 4000 characters]"
  if(nchar(x)>4000L) paste0(substr(x,1L,4000L-nchar(note)),note) else x
}

su_context <- function(project, dataset, node, side) {
  root <- project_dir_of(project); spec <- read_project_spec_dir(root); graph <- load_graph(root,dataset)
  p <- list(state_dir=file.path(root,".dualdag","state")); p$state <- read_state_file(p)
  sdir <- side_run_dir(root,dataset,node,side)
  src <- tryCatch(load_src(root),error=function(e)list()); cols <- lapply(src,function(x) if(is.data.frame(x)) names(x) else character())
  frames <- list()
  for(dsdir in list.dirs(file.path(root,".dualdag","runs"),recursive=TRUE,full.names=TRUE)) {
    out <- file.path(dsdir,"out.rds")
    if(file.exists(out) && grepl(paste0("/",side,"$"),dsdir)) {
      frame <- tryCatch(readRDS(out),error=function(e)NULL)
      if(is.data.frame(frame)) frames[[substring(dsdir,nchar(file.path(root,".dualdag","runs"))+2L)]] <- list(rows=nrow(frame),columns=names(frame))
    }
  }
  whole_spec <- paste(c("VARIABLE_ROWS",capture.output(print(spec$variables,row.names=FALSE)),"PARAMETER_ROWS",capture.output(print(spec$parameters,row.names=FALSE))),collapse="\n")
  st <- get_status(p,dataset,node)
  own_status <- list(level=st$level,problem=st$problem,side=st$sides[[side]])
  list(code=read_text(file.path(sdir,"code.R")), log=su_cap_log(read_text(file.path(sdir,"log.txt"))),
    status=own_status, chat=Filter(function(t) identical(t$node,node), expert_read(root,dataset,side)),
    keys=list(spec=spec_key_names(spec,dataset),layer=if(exists("layer_keys_path",mode="function") && file.exists(layer_keys_path(root,dataset))) read_json(layer_keys_path(root,dataset)) else list()),
    spec=whole_spec, graph=graph, parameter_graph=if(exists("param_graph_read",mode="function")) tryCatch(param_graph_read(root,dataset),error=function(e)NULL) else NULL,
    frames=frames, source_columns=cols)
}

su_private_copy <- function(root, side) {
  tmp <- tempfile("su-private-"); dir.create(tmp)
  for (name in c("spec", "data")) { src <- file.path(root,name); if(dir.exists(src)) file.copy(src,tmp,recursive=TRUE) }
  state <- file.path(root,".dualdag","state"); if(dir.exists(state)) { dir.create(file.path(tmp,".dualdag"),showWarnings=FALSE); file.copy(state,file.path(tmp,".dualdag"),recursive=TRUE) }
  runs <- file.path(root,".dualdag","runs")
  if(dir.exists(runs)) for(d in list.dirs(runs,recursive=TRUE,full.names=TRUE)) if(identical(basename(d),side)) {
    dest <- file.path(tmp,substring(d,nchar(root)+2L)); dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE); file.copy(d,dirname(dest),recursive=TRUE)
  }
  tmp
}

su_forbidden_code <- function(code, root, side) {
  x <- paste(as.character(code %||% ""), collapse="\n")
  checks <- c("\\.dualdag[/\\\\]sessions", "[/\\\\](primary|validation)[/\\\\]", "DUALDAG_HOME")
  if (any(vapply(checks, grepl, logical(1), x=x, ignore.case=TRUE, perl=TRUE)))
    return("Refused: code may not read another side or .dualdag/sessions.")
  NULL
}

su_tool_loop <- function(cfg, root, dataset, node, side, messages, timeout=600) {
  system <- paste("TASK: SU_MODE", "You are SU mode for the selected Coder side. You may inspect the complete spec, DAGs, source data and this side's records only.",
    "Never request or expose the other side's values. Summarize data locally; do not print rows.",
    "These tools are real. Returning the JSON tool call runs it through this app. Never say tools are unavailable.",
    "Return JSON only with tool, args, and optional text. Tool reference:",
    'read_code: read this side current code; args {}; example {"tool":"read_code","args":{}}.',
    'read_history: read this coder conversation and run history; args {}; example {"tool":"read_history","args":{}}.',
    'read_spec: read the project spec; args {}; example {"tool":"read_spec","args":{}}.',
    'peek_data: inspect a small sample of source rows; args {}; example {"tool":"peek_data","args":{}}.',
    'peek_frame: inspect a sample from an own-side frame; args {"name":"ADSL.AGE"}; example {"tool":"peek_frame","args":{"name":"ADSL.AGE"}}.',
    'run_check: run code privately under normal runner rules without changing official files; args {"code":"..."}; example {"tool":"run_check","args":{"code":"result <- work"}}.',
    'write_program: run and save a complete replacement program on this side; args {"code":"..."}; example {"tool":"write_program","args":{"code":"result <- work"}}.',
    'reply: return final text; args {}; example {"tool":"reply","args":{},"text":"Done."}.',
    "For primary code, build from lineage variables first. A needed column already present in names(LINEAGE_<P>) may be used even outside DAG lineage; the app warns, and this is not an error or reason to stop. Do not print the app's warning in code or reply. Only stop when a needed column is absent from the LINEAGE_ frame and provided source data, using: Missing column: X is not in LINEAGE_SYSBP or the provided data. Source filtering remains in effect.",
    "At most 8 tool rounds. write_program args.code is the complete R program; side is fixed by the server.",sep="\n")
  ctx <- su_context(root,dataset,node,side); tool_results <- character(); spec <- read_project_spec_dir(root); graph <- load_graph(root,dataset)
  call_usages <- list()
  for (i in seq_len(8L)) {
    user <- to_json(list(context=ctx,messages=messages,tool_results=tool_results,
      instruction="Choose one tool, or reply with final text."))
    raw <- llm_chat(cfg,system,user,max_tokens=3000L,json=TRUE)
    call_usages[length(call_usages)+1L] <- list(llm_usage_summary(raw$usage,cfg$provider %||% ""))
    ans <- tryCatch(parse_json_reply(raw$text),error=function(e)list(tool="reply",text=as.character(raw$text %||% "")))
    tool <- as.character(ans$tool %||% "reply")[1]; a <- ans$args %||% list(); value <- NULL
    if (tool=="reply") return(list(reply=as.character(ans$text %||% ans$reply %||% "Done.")[1], calls=tool_results,
                                   usage=llm_usage_combine(call_usages)))
    if (tool=="read_code") value <- ctx$code
    else if (tool=="read_history") {
      history <- lapply(coder_sessions_list(root,dataset,node,side),function(rec) {
        full <- coder_session_full(root,rec$id)
        list(id=rec$id,runs=lapply(full$runs,function(r) { r$log <- su_cap_log(r$log); r }),turns=Filter(function(t) identical(t$side,side)||is.null(t$side),full$turns %||% list()))
      })
      value <- history
    }
    else if (tool=="read_spec") value <- ctx$spec
    else if (tool=="peek_frame") {
      nm <- as.character(a$name %||% "")[1]
      if(is.null(ctx$frames[[nm]])) value <- "Unknown own-side frame." else {
        frame <- tryCatch(readRDS(file.path(root,".dualdag","runs",nm,"out.rds")),error=function(e)NULL)
        if(!is.data.frame(frame)) value <- "Frame unavailable." else {
        set.seed(lineage_sample_seed(dataset,paste0("SU.FRAME.",nm)))
        ix <- sort(sample(seq_len(nrow(frame)),min(20L,nrow(frame)))); value <- frame[ix,,drop=FALSE]
        }
      }
    } else if (tool=="peek_data") {
      src <- load_src(root); loc <- do.call(rbind,lapply(names(src),function(dom) if(is.data.frame(src[[dom]]) && nrow(src[[dom]])) data.frame(domain=dom,row=seq_len(nrow(src[[dom]])),stringsAsFactors=FALSE) else NULL))
      if(is.null(loc)) value <- list() else {
        had <- exists(".Random.seed",.GlobalEnv,inherits=FALSE); oldseed <- if(had)get(".Random.seed",.GlobalEnv)else NULL
        on.exit(if(had)assign(".Random.seed",oldseed,.GlobalEnv) else if(exists(".Random.seed",.GlobalEnv,inherits=FALSE))rm(list=".Random.seed",envir=.GlobalEnv),add=TRUE)
        set.seed(lineage_sample_seed(dataset,"SU.PEEK")); pick <- sort(sample(seq_len(nrow(loc)),min(20L,nrow(loc))))
        value <- lapply(split(loc[pick,,drop=FALSE],loc$domain[pick]),function(z) src[[z$domain[[1]]]][z$row,,drop=FALSE])
      }
    } else if (tool=="run_check") {
      code <- as.character(a$code %||% ctx$code)[1]; refusal <- su_forbidden_code(code,root,side)
      if (!is.null(refusal)) value <- refusal else {
        tmp <- su_private_copy(root,side); on.exit(unlink(tmp,recursive=TRUE),add=TRUE)
        graph <- load_graph(tmp,dataset); spec <- read_project_spec_dir(tmp)
        ran <- tryCatch(run_side(tmp,dataset,graph_node(graph,node) %||% default_node(node),side,code,timeout=timeout,graph=graph,spec=spec),error=function(e)list(ok=FALSE,error=conditionMessage(e)))
        unlink(tmp,recursive=TRUE)
        full <- paste(ran$log %||% ran$error %||% "Check passed",collapse="\n")
        note <- "\n[truncated at 4000 characters]"
        value <- if(nchar(full)>4000L) paste0(substr(full,1L,4000L-nchar(note)),note) else full
      }
    } else if (tool=="write_program") {
      code <- as.character(a$code %||% "")[1]; refusal <- su_forbidden_code(code,root,side)
      if (!is.null(refusal)) value <- refusal else {
        dir <- side_run_dir(root,dataset,node,side); tmp <- su_private_copy(root,side); on.exit(unlink(tmp,recursive=TRUE),add=TRUE)
        graph <- load_graph(tmp,dataset); spec <- read_project_spec_dir(tmp)
        ran <- tryCatch(run_side(tmp,dataset,graph_node(graph,node) %||% default_node(node),side,code,timeout=timeout,graph=graph,spec=spec),error=function(e)list(ok=FALSE,error=conditionMessage(e)))
        if (!isTRUE(ran$ok)) { unlink(tmp,recursive=TRUE); value <- paste("Refused:",ran$error %||% "Program failed") } else {
          tdir <- side_run_dir(tmp,dataset,node,side)
          dir.create(dir,recursive=TRUE,showWarnings=FALSE)
          for(f in c("code.R","log.txt","out.rds","log_tags.rds","card.json")) if(file.exists(file.path(tdir,f))) file.copy(file.path(tdir,f),dir,overwrite=TRUE)
          unlink(file.path(dir,"approach.json"))
          unlink(tmp,recursive=TRUE)
          p <- open_project(root)
          apply_stored_run(p,dataset,node,setNames(list(list(ok=TRUE,error=NULL)),side))
          write_json(list(su_edited=TRUE),su_edited_path(root,dataset,node,side));
          sid <- coder_session_ensure(root,dataset,node,side)
          coder_session_append(root,sid,"events",list(event="Program changed in SU mode",at=now_iso(),detail=NULL))
          coder_session_snapshot_run(root,dataset,node,side)
          cmp <- tryCatch(compare_job(root,dataset,node),error=function(e)NULL); if(!is.null(cmp)) {
            save_compare(root,dataset,node,cmp); p <- open_project(root); st <- get_status(p,dataset,node)
            if(isTRUE(cmp$match)) { st$level <- "L3"; st$problem <- NULL }
            else { st$level <- "L2"; st$problem <- list(stage="compare",message=cmp$summary %||% "Values do not match.") }
            put_status(p,dataset,node,st)
          }
          value <- list(ok=TRUE,compare=if(isTRUE(cmp$match))"match"else"mismatch")
        }
      }
    } else value <- "Unknown tool refused."
    entry <- paste0(tool,": ",to_json(value)); tool_results <- c(tool_results,entry)
  }
  list(reply="SU stopped after 8 tool rounds. Send a follow-up to continue.",calls=tool_results,
       usage=llm_usage_combine(call_usages))
}

su_job <- function(project_dir,dataset,node_id,side,messages,message,timeout) {
  prior <- getOption("dualdag.llm_log"); options(dualdag.llm_log=NULL)
  on.exit(options(dualdag.llm_log=prior),add=TRUE)
  prior_tool_notice <- getOption("dualdag.cli_tool_notice")
  options(dualdag.cli_tool_notice="You have no tools of your own and no workspace. The app runs the tools listed above when you return their JSON. Reply now with only one JSON object.")
  on.exit(options(dualdag.cli_tool_notice=prior_tool_notice),add=TRUE)
  cfg <- with_cfg()
  if(!llm_ready(cfg)) stop("Choose a model in Settings.")
  su_tool_loop(cfg,project_dir,dataset,node_id,side,messages,timeout=timeout)
}

coder_session_root <- function(project) file.path(project_dir_of(project), ".dualdag", "sessions")
coder_session_key <- function(dataset, node, side) paste(dataset, node, side, sep = "_")
coder_session_open_path <- function(project, dataset, node, side) file.path(coder_session_root(project), paste0("open_", gsub("[^A-Za-z0-9._-]", "_", coder_session_key(dataset,node,side)), ".json"))
coder_session_dir <- function(project, id) file.path(coder_session_root(project), id)
coder_session_read <- function(project, id) read_json(file.path(coder_session_dir(project,id), "session.json"))
coder_session_open <- function(project,dataset,node,side) {
  x <- read_json(coder_session_open_path(project,dataset,node,side)); id <- x$id %||% NULL
  if (is.null(id) || !dir.exists(coder_session_dir(project,id))) NULL else id
}
coder_session_for_node <- function(project,node,side) {
  root <- coder_session_root(project); fs <- if(dir.exists(root)) list.files(root,pattern="^open_.*\\.json$",full.names=TRUE) else character()
  for(f in fs) { x <- read_json(f); if(!is.null(x$id)) { m <- coder_session_read(project,x$id); if(identical(m$node,node)&&identical(m$side,side)) return(x$id) } }
  NULL
}
coder_session_set_status <- function(project,dataset,node,side,status) {
  id <- coder_session_open(project,dataset,node,side); if(is.null(id)) return(invisible(NULL))
  m <- coder_session_read(project,id); m$status <- status; m$last_at <- now_iso(); write_json(m,file.path(coder_session_dir(project,id),"session.json")); invisible(id)
}
coder_session_parts <- function(dataset,node,side) {
  parsed <- parse_node_id(node)
  clean <- function(x) { z <- gsub("[^A-Za-z0-9.]", ".", as.character(x %||% "")[1]); gsub("\\.{2,}", ".", z) }
  variable <- if (identical(toupper(parsed$var), "RECORDS")) "Keys" else parsed$var
  c(clean(dataset), if (!is.null(parsed$paramcd) && nzchar(parsed$paramcd)) clean(parsed$paramcd), clean(variable), clean(side))
}
coder_session_prune <- function(project) {
  root <- coder_session_root(project); if (!dir.exists(root)) return(invisible(integer()))
  dirs <- list.dirs(root,recursive=FALSE,full.names=TRUE)
  recs <- lapply(dirs,function(d) tryCatch(read_json(file.path(d,"session.json")),error=function(e)NULL))
  valid <- which(vapply(recs,is.list,TRUE)); if (!length(valid)) return(invisible(integer()))
  opens <- list.files(root,pattern="^open_.*\\.json$",full.names=TRUE)
  open_ids <- vapply(opens,function(f) as.character((read_json(f)$id %||% ""))[1],"")
  ck <- function(x) paste(x$dataset %||% "",x$node %||% "",x$side %||% "",sep="|")
  keys <- vapply(recs[valid],ck,"")
  seqno <- function(x) suppressWarnings(as.integer(sub(".*-([0-9]+)$","\\1",as.character(x$id %||% ""))))
  newest <- unlist(lapply(unique(keys),function(k) { ix <- valid[keys==k]; ix[order(vapply(recs[ix],seqno,0L),vapply(recs[ix],function(x) as.character(x$started_at %||% "")[1],""),decreasing=TRUE)[1]] }))
  cutoff <- Sys.time() - as.numeric(SESSION_KEEP_HOURS %||% 168)*3600; removed <- integer()
  for (i in valid) {
    x <- recs[[i]]
    if (i %in% newest || as.character(x$id %||% "") %in% open_ids || identical(x$status,"open")) next
    at <- suppressWarnings(as.POSIXct(x$last_at %||% x$started_at,format="%Y-%m-%dT%H:%M:%S%z",tz="UTC"))
    if (!is.na(at) && at < cutoff) { unlink(dirs[[i]],recursive=TRUE); removed <- c(removed,i) }
  }
  invisible(removed)
}
coder_session_start <- function(project,dataset,node,side,model=NULL) {
  root <- coder_session_root(project); dir.create(root,recursive=TRUE,showWarnings=FALSE)
  coder_session_prune(project)
  prev <- coder_session_open(project,dataset,node,side)
  if (!is.null(prev)) {
    old <- coder_session_read(project,prev)
    if (is.list(old)) { if (identical(old$status,"open")) old$status <- "success"; old$last_at <- now_iso(); write_json(old,file.path(coder_session_dir(project,prev),"session.json")) }
    unlink(coder_session_open_path(project,dataset,node,side))
  }
  dirs <- list.dirs(root,recursive=FALSE,full.names=TRUE)
  if (is.null(prev)) {
    matches <- Filter(function(x) is.list(x) && identical(x$dataset,dataset) && identical(x$node,node) && identical(x$side,side),lapply(dirs,function(d) tryCatch(read_json(file.path(d,"session.json")),error=function(e)NULL)))
    if (length(matches)) { seqno <- function(x) suppressWarnings(as.integer(sub(".*-([0-9]+)$","\\1",as.character(x$id %||% "")))); prev <- matches[[order(vapply(matches,seqno,0L),vapply(matches,function(x) as.character(x$started_at %||% "")[1],""),decreasing=TRUE)[1]]]$id }
  }
  prefix <- paste(coder_session_parts(dataset,node,side),collapse="-")
  existing <- list.dirs(root,recursive=FALSE,full.names=FALSE)
  suffix <- substring(existing, nchar(prefix) + 2L)
  exact_prefix <- startsWith(existing, paste0(prefix, "-")) & grepl("^[0-9]+$", suffix)
  used <- suppressWarnings(as.integer(suffix[exact_prefix]))
  n <- if (length(used) && any(!is.na(used))) max(used, na.rm=TRUE) + 1L else 1L
  repeat { ids <- paste0(prefix,"-",n); dest <- file.path(root,ids); if (dir.create(dest,showWarnings=FALSE)) break; n <- n+1L }
  at <- now_iso(); rec <- list(id=ids,dataset=dataset,node=node,side=side,model=model,started_at=at,last_at=at,status="open",parent_session=prev)
  write_json(rec,file.path(dest,"session.json")); write_json(list(id=ids),coder_session_open_path(project,dataset,node,side)); ids
}
coder_session_ensure <- function(project,dataset,node,side,model=NULL) {
  id <- coder_session_open(project,dataset,node,side); if (is.null(id)) id <- coder_session_start(project,dataset,node,side,model)
  id
}
coder_session_append <- function(project,id,name,value) {
  if (is.null(id)) return(invisible(NULL)); d <- coder_session_dir(project,id); p <- file.path(d,paste0(name,".json")); a <- read_json(p); if (!is.list(a)) a <- list(); a[[length(a)+1L]] <- value; write_json(a,p)
  m <- coder_session_read(project,id); if (is.list(m)) {m$last_at <- now_iso(); write_json(m,file.path(d,"session.json"))}; invisible(value)
}
coder_session_close <- function(project,dataset,node,side) {
  id <- coder_session_open(project,dataset,node,side); if (is.null(id)) return(invisible(NULL))
  m <- coder_session_read(project,id); m$last_at <- now_iso(); write_json(m,file.path(coder_session_dir(project,id),"session.json")); unlink(coder_session_open_path(project,dataset,node,side)); invisible(id)
}
coder_session_snapshot_run <- function(project,dataset,node,side) {
  id <- coder_session_open(project,dataset,node,side); if(is.null(id)) return(invisible(NULL))
  d <- coder_session_dir(project,id); runs <- file.path(d,"runs"); dir.create(runs,recursive=TRUE,showWarnings=FALSE)
  n <- length(list.dirs(runs,recursive=FALSE,full.names=FALSE))+1L
  sdir <- file.path(node_run_dir(project,dataset,node),side); dest <- file.path(runs,sprintf("%04d",n))
  dir.create(dest,showWarnings=FALSE)
  for(f in c("code.R","log.txt")) if(file.exists(file.path(sdir,f)))
    write_text(llm_redact(read_text(file.path(sdir,f)),get_api_key()$key),file.path(dest,f))
  ap <- read_json(file.path(sdir,"approach.json"))
  if (is.list(ap)) { ap$approach <- llm_redact(as.character(ap$approach %||% "")[1],get_api_key()$key); write_json(ap,file.path(dest,"approach.json")) }
}

coder_window_reset <- function(project, dataset, node, side) {
  dir <- coder_window_dir(project, dataset, node, side)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  unlink(c(coder_window_context_path(project, dataset, node, side),
           coder_window_process_path(project, dataset, node, side)))
  invisible(NULL)
}

coder_window_prompt <- function(project, dataset, node, side, system, user, task = "generation", parent_task = NULL) {
  if (is.null(project)) return(invisible(NULL))
  path <- coder_window_context_path(project, dataset, node, side)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  attempts <- read_json(path)
  if (!is.list(attempts)) attempts <- list()
  attempts[[length(attempts) + 1L]] <- list(task = task, parent_task = parent_task,
    system = as.character(system %||% ""), user = as.character(user %||% ""), at = now_iso())
  write_json(attempts, path)
  sid <- coder_session_ensure(project,dataset,node,side,if(exists("load_config",mode="function")) load_config()$model %||% NULL else NULL)
  coder_session_append(project,sid,"prompts",list(task=task,parent_task=parent_task,system=llm_redact(as.character(system %||% ""),get_api_key()$key),user=llm_redact(as.character(user %||% ""),get_api_key()$key),time=now_iso()))
  invisible(NULL)
}

coder_window_approach <- function(project,dataset,node,side,approach) {
  if (is.null(project)) return(invisible(NULL))
  path <- coder_window_context_path(project,dataset,node,side); a <- read_json(path)
  if (!is.list(a) || !length(a)) return(invisible(NULL))
  value <- tryCatch(as.character(approach)[1],error=function(e) "")
  if (length(value)!=1L || is.na(value)) value <- ""
  mark_final <- function(rows) {
    if (!is.list(rows) || !length(rows)) return(rows)
    end <- length(rows); last <- rows[[end]]
    root_task <- if (identical(as.character(last$task %||% "")[1],"lookup round")) as.character(last$parent_task %||% "")[1] else as.character(last$task %||% "")[1]
    if (!root_task %in% c("generation","rewrite")) return(rows)
    i <- end
    while (i >= 1L) {
      entry <- rows[[i]]; task <- as.character(entry$task %||% "")[1]
      belongs <- identical(task,root_task) || (identical(task,"lookup round") && identical(as.character(entry$parent_task %||% "")[1],root_task))
      if (!belongs) break
      entry$final_reply <- identical(i,end)
      if (identical(i,end)) entry$approach <- value
      rows[[i]] <- entry
      i <- i-1L
    }
    rows
  }
  a <- mark_final(a); write_json(a,path)
  sid <- coder_session_open(project,dataset,node,side)
  if (!is.null(sid)) {
    pp <- file.path(coder_session_dir(project,sid),"prompts.json"); prompts <- read_json(pp)
    if (is.list(prompts) && length(prompts)) { prompts <- mark_final(prompts); write_json(prompts,pp) }
  }
  invisible(value)
}

coder_window_event <- function(project, dataset, node, side, event, detail = NULL) {
  if (is.null(project)) return(invisible(NULL))
  path <- coder_window_process_path(project, dataset, node, side)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  events <- read_json(path)
  if (!is.list(events)) events <- list()
  events[[length(events) + 1L]] <- list(event = as.character(event), at = now_iso(), detail = detail)
  write_json(events, path)
  sid <- coder_session_open(project,dataset,node,side)
  if (!is.null(sid)) coder_session_append(project,sid,"events",list(event=as.character(event),at=now_iso(),detail=llm_redact(detail,get_api_key()$key)))
  invisible(NULL)
}

coder_window_read <- function(project, dataset, node, side, running = FALSE, attempt = NULL) {
  thread <- if (exists("expert_read", mode = "function"))
    tryCatch(expert_read(project, dataset, side), error = function(e) list()) else list()
  thread <- Filter(function(turn) is.list(turn) && identical(as.character(turn$node %||% "")[1], as.character(node)[1]), thread)
  attempts <- read_json(coder_window_context_path(project, dataset, node, side))
  if (!is.list(attempts)) attempts <- list()
  eligible <- which(vapply(attempts, function(x) {
    task <- as.character(x$task %||% "")[1]
    parent <- as.character(x$parent_task %||% "")[1]
    task %in% c("generation", "rewrite", "chat") ||
      (identical(task, "lookup round") && parent %in% c("generation", "rewrite", "chat"))
  }, TRUE))
  default_attempt <- if (length(eligible)) tail(eligible, 1L) else NA_integer_
  selected <- suppressWarnings(as.integer(attempt)[1])
  if (is.na(selected) || selected < 1L || selected > length(attempts)) selected <- default_attempt
  sid <- coder_session_open(project,dataset,node,side)
  status_project <- if (is.character(project)) tryCatch({
    view <- new.env(parent=emptyenv())
    view$dir <- normalizePath(project, winslash="/", mustWork=TRUE)
    view$state_dir <- file.path(view$dir,".dualdag","state")
    view$state <- read_state_file(view)
    view
  }, error=function(e) NULL) else project
  status <- if (is.null(status_project)) NULL else tryCatch(get_status(status_project, dataset, node), error = function(e) NULL)
  if (is.list(status) && exists("attach_lineage_warnings", mode="function")) {
    if (exists("normalize_missing_column_status", mode="function"))
      status <- normalize_missing_column_status(status)
    status <- tryCatch(attach_lineage_warnings(status_project, dataset, node, status, "primary"), error=function(e) status)
  }
  list(dataset = dataset, node = node, side = side, session_id=sid, running = isTRUE(running),
       context = if (!is.na(selected)) attempts[[selected]] else NULL,
       contexts = attempts, context_index = if (!is.na(selected)) selected else NULL,
       default_context_index = if (!is.na(default_attempt)) default_attempt else NULL,
       process = read_json(coder_window_process_path(project, dataset, node, side)),
       usage_total = if (!is.null(sid)) read_json(file.path(coder_session_dir(project,sid),"usage.json")) else NULL,
       su_edited = isTRUE(read_json(su_edited_path(project,dataset,node,side))$su_edited),
       thread = thread,
       status = status)
}

coder_sessions_list <- function(project,dataset=NULL,node=NULL,side=NULL) {
  root <- coder_session_root(project); dirs <- if(dir.exists(root)) list.dirs(root,recursive=FALSE,full.names=TRUE) else character()
  out <- lapply(dirs,function(d) tryCatch(read_json(file.path(d,"session.json")),error=function(e)NULL)); out <- Filter(is.list,out)
  if(!is.null(dataset)&&nzchar(dataset)) out <- Filter(function(x) identical(x$dataset,dataset),out)
  if(!is.null(node)&&nzchar(node)) out <- Filter(function(x) identical(x$node,node),out)
  if(!is.null(side)&&nzchar(side)) out <- Filter(function(x) identical(x$side,side),out)
  if(length(out)) { seqno <- function(x) suppressWarnings(as.integer(sub(".*-([0-9]+)$","\\1",as.character(x$id %||% "")))); out <- out[order(vapply(out,function(x) as.character(x$started_at %||% "")[1],""),vapply(out,seqno,0L),decreasing=TRUE)] }
  out
}
coder_session_full <- function(project,id) {
  if(!grepl("^[A-Za-z0-9._-]+$",id) || grepl("\\.\\.",id)) return(NULL)
  d <- coder_session_dir(project,id); if(!dir.exists(d)) return(NULL)
  m <- read_json(file.path(d,"session.json")); if(!is.list(m)) return(NULL)
  m$prompts <- read_json(file.path(d,"prompts.json")); m$events <- read_json(file.path(d,"events.json")); m$turns <- read_json(file.path(d,"turns.json"))
  m$usage_total <- read_json(file.path(d,"usage.json"))
  run_dirs <- list.dirs(file.path(d,"runs"),recursive=FALSE,full.names=TRUE)
  m$runs <- lapply(run_dirs,function(rd) list(number=basename(rd),code=read_text(file.path(rd,"code.R")),log=read_text(file.path(rd,"log.txt")),approach=read_json(file.path(rd,"approach.json"))$approach %||% ""))
  logdir <- file.path(project_dir_of(project),".dualdag","llm_log")
  m$llm_calls <- Filter(function(x) is.list(x) && identical(x$session,id),lapply(list.files(logdir,pattern="\\.json$",full.names=TRUE),function(f) tryCatch(read_json(f),error=function(e)NULL)))
  m
}

coder_session_current <- function(A,record) {
  p <- A$project; ds <- record$dataset; node <- record$node; side <- record$side
  base <- node_run_dir(p,ds,node); st <- get_status(p,ds,node)
  jobs <- Filter(function(j) identical(j$dataset,ds) && identical(j$node,node),A$jobs)
  events <- read_json(file.path(coder_session_dir(p,record$id),"events.json"))
  step <- if (length(events)) as.character(events[[length(events)]]$event %||% "") else NULL
  latest <- coder_sessions_list(p$dir,ds,node,side)
  state <- switch(as.character(st$level %||% "none"), L3="Data matched", L2="Generated", "Not generated")
  list(code=llm_redact(read_text(file.path(base,side,"code.R")),get_api_key()$key),log=llm_redact(read_text(file.path(base,side,"log.txt")),get_api_key()$key),
       state=state,running=length(jobs)>0L,step=if(length(jobs)) step else NULL,
       last_problem=st$problem$message %||% NULL,problem_stage=st$problem$stage %||% NULL,
       compare=llm_redact(read_json(file.path(base,"compare.json")),get_api_key()$key),is_latest=length(latest)>0L && identical(latest[[1]]$id,record$id))
}
