# App Admin uses the user's logged-in coding CLI. Its two-root boundary is an
# instruction to the CLI, not a filesystem boundary or sandbox.
admin_store_dir <- function(project) file.path(project, ".dualdag", "app-admin")
admin_session_path <- function(project) file.path(admin_store_dir(project), "session.json")
admin_snapshot_root <- function(project) file.path(project, ".dualdag", "admin_snapshots")
admin_memory_dir <- function() file.path(config_dir(), "memory")
admin_memory_index <- function() {
  d <- admin_memory_dir(); dir.create(d, recursive=TRUE, showWarnings=FALSE)
  f <- file.path(d, "MEMORY.md")
  if (!file.exists(f)) write_utf8("# Customer memory\n", f)
  f
}

admin_roots <- function(app_dir, project) normalizePath(c(app_dir, project), winslash = "/", mustWork = TRUE)
admin_rel <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  if (!startsWith(path, paste0(root, "/"))) return(NA_character_)
  substring(path, nchar(root) + 2L)
}
admin_symlink_path <- function(path, root) {
  path<-gsub("\\\\","/",path);root<-gsub("\\\\","/",root)
  if(identical(path,root))return(FALSE)
  if(!startsWith(path,paste0(root,"/")))return(TRUE)
  rel<-substring(path,nchar(root)+2L)
  parts <- strsplit(rel,"/",fixed=TRUE)[[1]]
  cursor <- root
  for (part in parts) {
    cursor <- file.path(cursor,part)
    link <- Sys.readlink(cursor)
    if (length(link) && !is.na(link) && nzchar(link)) return(TRUE)
  }
  FALSE
}
admin_tree_paths <- function(root,exclude_snapshots=FALSE) {
  root<-normalizePath(root,winslash="/",mustWork=TRUE);queue<-root;out<-character()
  while(length(queue)) {
    dir<-queue[[1]];queue<-queue[-1L]
    entries<-list.files(dir,all.files=TRUE,full.names=TRUE,no..=TRUE)
    if(!length(entries))next
    out<-c(out,entries)
    info<-file.info(entries);links<-vapply(entries,function(path){x<-Sys.readlink(path);length(x)&&!is.na(x)&&nzchar(x)},TRUE)
    subdirs<-entries[info$isdir%in%TRUE&!links]
    if(isTRUE(exclude_snapshots))subdirs<-subdirs[!grepl("^[.]dualdag/admin_snapshots(/|$)",substring(gsub("\\\\","/",subdirs),nchar(root)+2L))]
    queue<-c(queue,subdirs)
  }
  out
}
admin_symlink_map <- function(root,kind) {
  paths<-admin_tree_paths(root,exclude_snapshots=identical(kind,"project"))
  if(!length(paths))return(setNames(character(),character()))
  links<-vapply(paths,function(path){x<-Sys.readlink(path);if(length(x)&&!is.na(x))x else ""},"")
  keep<-nzchar(links)&!vapply(dirname(paths),admin_symlink_path,TRUE,root=root)
  paths<-paths[keep];links<-links[keep]
  if(!length(paths))return(setNames(character(),character()))
  rel<-substring(gsub("\\\\","/",paths),nchar(gsub("\\\\","/",root))+2L)
  setNames(links,paste0(kind,"/",rel))
}
admin_restore_symlinks <- function(app_dir,project,manifest,keys,include_data=TRUE) {
  roots<-admin_roots(app_dir,project)
  for(key in unique(keys)) {
    kind<-sub("/.*$","",key);if(!kind%in%c("app","project"))next
    root<-if(kind=="app")roots[[1]]else roots[[2]]
    rel<-sub("^(app|project)/","",key);path<-file.path(root,rel)
    if(!admin_snapshot_scope(key,include_data))next
    if(admin_symlink_path(dirname(path),root))next
    expected<-manifest[[key]] %||% ""
    target<-if(startsWith(as.character(expected),"symlink:"))substring(as.character(expected),9L)else ""
    current<-Sys.readlink(path);is_link<-length(current)&&!is.na(current)&&nzchar(current)
    if(!nzchar(target)) {
      if(is_link)unlink(path,recursive=TRUE,force=TRUE)
      next
    }
    if(is_link&&identical(current,target))next
    if(file.exists(path)||dir.exists(path)||is_link)unlink(path,recursive=TRUE,force=TRUE)
    if(nzchar(target)) {
      dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
      if(!file.symlink(target,path))warning("Could not restore symlink: ",key,call.=FALSE)
    }
  }
  invisible(NULL)
}
admin_routine_project_path <- function(rel) grepl("^[.]dualdag/admin_snapshots(/|$)", rel)
admin_files <- function(app_dir, project) {
  roots <- admin_roots(app_dir, project)
  files <- c(admin_tree_paths(roots[[1]]),admin_tree_paths(roots[[2]],exclude_snapshots=TRUE))
  files <- files[file.info(files)$isdir %in% FALSE]
  rel_project <- vapply(files, admin_rel, "", root = roots[[2]])
  rel_app <- vapply(files, admin_rel, "", root = roots[[1]])
  safe_project <- !is.na(rel_project) & !vapply(files,admin_symlink_path,TRUE,root=roots[[2]])
  safe_app <- !is.na(rel_app) & !vapply(files,admin_symlink_path,TRUE,root=roots[[1]])
  in_app <- safe_app
  keep_project <- safe_project & !admin_routine_project_path(rel_project)
  files <- files[in_app | keep_project]
  sort(unique(files))
}
admin_project_size <- function(project) {
  files <- admin_tree_paths(project,exclude_snapshots=TRUE)
  if (!length(files)) return(0)
  root <- normalizePath(project,winslash="/",mustWork=TRUE)
  files <- files[!vapply(files,admin_symlink_path,TRUE,root=root)]
  files <- files[file.info(files)$isdir %in% FALSE]
  sum(file.info(files)$size,na.rm=TRUE)
}
admin_snapshot_scope <- function(key, include_data=TRUE) {
  if (startsWith(key, "app/")) return(TRUE)
  rel <- sub("^project/", "", key)
  if (admin_routine_project_path(rel)) return(FALSE)
  include_data || !grepl("(^|/)data(/|$)",rel)
}
admin_hash_only <- function(key, size, include_data=TRUE) {
  rel <- sub("^project/", "", key)
  (!include_data && grepl("(^|/)data(/|$)",rel)) || !admin_snapshot_scope(key,include_data) || is.na(size)
}
admin_snapshot_files <- function(saved, snap) {
  if (!is.null(saved$snapshot_files)) return(unlist(saved$snapshot_files, use.names = FALSE))
  # Snapshots made before Task 50b did not store this list. Infer the actual
  # copied files so those snapshots remain undoable after upgrade.
  out <- character()
  for (kind in c("app", "project")) {
    root <- file.path(snap, kind)
    files <- if (dir.exists(root)) list.files(root, recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE) else character()
    files <- files[!file.info(files)$isdir %in% TRUE]
    if (length(files)) out <- c(out, paste0(kind, "/", substring(files, nchar(root) + 2L)))
  }
  out[vapply(out, admin_snapshot_scope, TRUE)]
}
admin_history_raw <- function(project) {
  f<-file.path(project,".dualdag","admin_log.jsonl")
  if(!file.exists(f))return(NULL)
  readBin(f,"raw",n=as.integer(file.info(f)$size))
}
admin_history_restore <- function(project,raw) {
  if(is.null(raw))return(invisible(NULL))
  f<-file.path(project,".dualdag","admin_log.jsonl");dir.create(dirname(f),recursive=TRUE,showWarnings=FALSE)
  writeBin(raw,f);invisible(NULL)
}
admin_manifest <- function(app_dir, project) {
  files <- admin_files(app_dir, project)
  roots <- admin_roots(app_dir, project)
  rel <- vapply(files, function(f) {
    a <- admin_rel(f, roots[[1]]); if (!is.na(a)) paste0("app/", a) else paste0("project/", admin_rel(f, roots[[2]]))
  }, "")
  stats <- file.info(files)
  include_data <- admin_project_size(project) <= 500 * 1024^2
  hashes <- vapply(seq_along(files), function(i) {
    omitted_data <- !include_data && grepl("(^|/)data(/|$)",sub("^project/","",rel[[i]]))
    if (omitted_data) paste0("hash-only:",stats$size[[i]],":",as.numeric(stats$mtime[[i]]))
    else admin_file_hash(rel[[i]], files[[i]])
  }, "")
  names(hashes) <- rel
  only <- vapply(seq_along(hashes), function(i) admin_hash_only(rel[[i]], stats$size[[i]],include_data), TRUE)
  mark <- only & !startsWith(hashes,"hash-only:")
  hashes[mark] <- paste0("hash-only:", stats$size[mark], ":", hashes[mark])
  links<-c(admin_symlink_map(roots[[1]],"app"),admin_symlink_map(roots[[2]],"project"))
  if(length(links))hashes<-c(hashes,setNames(paste0("symlink:",links),names(links)))
  setNames(as.list(hashes), names(hashes))
}
admin_file_hash <- function(key, path) {
  if (identical(key, "project/.dualdag/state/state.json")) {
    state <- read_json(path)
    if (is.list(state)) {
      state$updated <- NULL
      stable <- as.character(jsonlite::toJSON(state, auto_unbox=TRUE, null="null", na="null", digits=NA, force=TRUE))
      return(paste(as.character(openssl::md5(charToRaw(enc2utf8(stable))))))
    }
  }
  file_md5(path)
}
admin_snapshot <- function(app_dir, project, prune=TRUE) {
  root <- admin_snapshot_root(project); dir.create(root, recursive = TRUE, showWarnings = FALSE)
  now <- as.numeric(Sys.time())
  stamp <- paste0(format(as.POSIXct(now,origin="1970-01-01",tz="UTC"),"%Y%m%d%H%M%S"),
    sprintf("%06d",as.integer((now-floor(now))*1e6)))
  id <- paste0(stamp, "-", random_token(4))
  dest <- file.path(root, id); dir.create(dest, recursive = TRUE)
  files <- admin_files(app_dir, project)
  roots <- admin_roots(app_dir, project)
  manifest <- admin_manifest(app_dir, project)
  keys <- names(manifest)
  include_data <- admin_project_size(project) <= 500 * 1024^2
  snapshot_files <- keys[vapply(seq_along(keys), function(i) admin_snapshot_scope(keys[[i]],include_data) &&
    !startsWith(manifest[[i]], "hash-only:") && !startsWith(manifest[[i]],"symlink:"), TRUE)]
  for (key in snapshot_files) {
    root_name <- sub("/.*$", "", key); rel <- sub("^(app|project)/", "", key)
    root <- if (root_name == "app") roots[[1]] else roots[[2]]
    f <- file.path(root, rel)
    target <- file.path(dest, root_name, rel)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE); file.copy(f, target, overwrite = TRUE)
  }
  write_json(list(id = id, manifest = manifest, snapshot_files = as.list(snapshot_files),
    symlink_count=sum(startsWith(unlist(manifest),"symlink:")),
    project_size_bytes=admin_project_size(project),data_excluded=!include_data,time = now_iso()), file.path(dest, "manifest.json"))
  if (isTRUE(prune)) admin_prune_snapshots(project)
  id
}
admin_valid_snapshots <- function(project) {
  root <- admin_snapshot_root(project)
  dirs <- if (dir.exists(root)) list.dirs(root, recursive = FALSE, full.names = TRUE) else character()
  dirs <- dirs[file.exists(file.path(dirs, "manifest.json"))]
  sort(dirs, decreasing = TRUE)
}
admin_prune_snapshots <- function(project,preserve=character()) {
  dirs<-admin_valid_snapshots(project)
  if(length(dirs)<=10L)return(invisible(NULL))
  preserved<-dirs[basename(dirs)%in%preserve]
  keep<-unique(c(preserved,setdiff(dirs,preserved)[seq_len(max(0L,10L-length(preserved)))]))
  unlink(setdiff(dirs,keep),recursive=TRUE)
  invisible(NULL)
}
admin_finish_snapshot <- function(app_dir, project, snapshot_id) {
  if (is.null(snapshot_id) || !length(snapshot_id) || !nzchar(snapshot_id)) return(invisible(NULL))
  write_json(admin_manifest(app_dir, project), file.path(admin_snapshot_root(project), snapshot_id, "after.json"))
  invisible(NULL)
}
admin_restore_snapshot <- function(app_dir, project, snapshot_id = NULL) {
  history_before<-admin_history_raw(project)
  dirs <- admin_valid_snapshots(project)
  if (!length(dirs)) stop("No App Admin snapshot exists in the selected project.", call. = FALSE)
  if (is.null(snapshot_id) || !nzchar(snapshot_id)) snap <- dirs[[1]]
  else {
    if (!grepl("^[A-Za-z0-9-]{1,80}$", snapshot_id)) stop("Invalid snapshot id.", call.=FALSE)
    snap <- file.path(admin_snapshot_root(project), snapshot_id)
    if (!snap %in% dirs) stop("Snapshot not found.", call.=FALSE)
  }
  saved <- read_json(file.path(snap, "manifest.json"))
  if (!is.list(saved) || !is.list(saved$manifest)) stop("The latest App Admin snapshot is invalid.", call. = FALSE)
  snapshot_files <- admin_snapshot_files(saved, snap)
  include_data<-!isTRUE(saved$data_excluded)
  current_links<-c(admin_symlink_map(admin_roots(app_dir,project)[[1]],"app"),admin_symlink_map(admin_roots(app_dir,project)[[2]],"project"))
  saved_links<-names(saved$manifest)[startsWith(vapply(saved$manifest,as.character,""),"symlink:")]
  link_keys<-union(names(current_links),saved_links)
  admin_restore_symlinks(app_dir,project,saved$manifest,union(link_keys,snapshot_files),include_data)
  roots <- admin_roots(app_dir, project)
  for (kind in c("app", "project")) {
    root <- if (kind == "app") roots[[1]] else roots[[2]]
    prefix <- paste0(kind, "/")
    for (key in snapshot_files[startsWith(snapshot_files, prefix)]) {
      rel <- sub("^(app|project)/", "", key)
      source <- file.path(snap, kind, rel)
      target <- file.path(root, rel)
      if (!file.exists(source) || dir.exists(source)) next
      if(admin_symlink_path(dirname(target),root))next
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      file.copy(source, target, overwrite = TRUE)
    }
    existing <- if (kind == "app") {
      admin_tree_paths(root)
    } else {
      admin_tree_paths(root,exclude_snapshots=TRUE)
    }
    existing <- existing[!file.info(existing)$isdir %in% TRUE]
    existing <- existing[!vapply(existing,admin_symlink_path,TRUE,root=root)]
    for (f in existing) {
      rel <- substring(normalizePath(f, winslash = "/"), nchar(root) + 2L)
      key <- paste0(prefix, rel)
      in_scope <- kind == "app" || admin_snapshot_scope(key)
      saved_value <- saved$manifest[[key]]
      size <- file.info(f)$size
      hash_only <- admin_hash_only(key, size) || (length(saved_value) > 0L && grepl("^(hash-only|large):", as.character(saved_value[[1]])))
      if (in_scope && !key %in% snapshot_files && !hash_only) unlink(f, force = TRUE)
    }
  }
  admin_restore_symlinks(app_dir,project,saved$manifest,saved_links,include_data)
  admin_history_restore(project,history_before)
  invisible(snap)
}
admin_snapshot_list <- function(project) lapply(admin_valid_snapshots(project), function(d) {
  m <- read_json(file.path(d,"manifest.json"))
  id <- basename(d)
  log_path <- file.path(project,".dualdag","admin_log.jsonl")
  records <- if(file.exists(log_path)) tryCatch(lapply(readLines(log_path,warn=FALSE),from_json),error=function(e)list()) else list()
  matched <- Filter(function(x)identical(as.character(x$snapshot_id %||% ""),id),records)
  message <- if(length(matched)) as.character(matched[[length(matched)]]$message %||% "") else "Snapshot"
  list(id=id, time=m$time %||% "", message=message, files=length(unlist(m$snapshot_files %||% list())),
    size_bytes=sum(vapply(unlist(m$snapshot_files %||% list()),function(key) {
      f<-file.path(d,key); if(file.exists(f))file.info(f)$size else 0
    },numeric(1))))
})
admin_snapshot_metadata <- function(project,id) {
  f<-file.path(admin_snapshot_root(project),id,"manifest.json");m<-read_json(f)
  entry<-Filter(function(x)identical(x$id,id),admin_snapshot_list(project))
  list(id=id,size_bytes=if(length(entry))entry[[1]]$size_bytes else 0,
    project_size_bytes=m$project_size_bytes %||% 0,data_excluded=isTRUE(m$data_excluded))
}
admin_append_log <- function(project, message, reply, files, snapshot_id, provider, model=NULL, usage=NULL) {
  path <- file.path(project,".dualdag","admin_log.jsonl")
  dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
  record <- list(time=now_iso(),message=message,reply=reply,files_changed=as.list(files),
    snapshot_id=snapshot_id,provider=provider,model=model,usage=usage)
  cat(as.character(jsonlite::toJSON(record,auto_unbox=TRUE,null="null",na="null",force=TRUE)),"\n",
    file=path,append=TRUE,sep="")
  invisible(path)
}
admin_refresh_project <- function(A) {
  project_dir <- A$project$dir
  A$project <- open_project(project_dir)
  if (exists("viewer_cache_reset",mode="function",inherits=TRUE)) viewer_cache_reset()
  if (exists("lineage_source_cache_reset",mode="function",inherits=TRUE)) lineage_source_cache_reset()
  if (exists("levels_cache_reset",mode="function",inherits=TRUE)) levels_cache_reset()
  A$project
}
admin_stop_processes <- function(A) {
  if (is.null(A$admin_procs)) return(invisible(FALSE))
  for (id in ls(A$admin_procs,all.names=TRUE)) {
    proc <- A$admin_procs[[id]]
    try(if (!is.null(proc$kill_tree)) proc$kill_tree() else proc$kill(),silent=TRUE)
  }
  invisible(TRUE)
}
admin_changed <- function(before, after) {
  keys <- union(names(before), names(after))
  comparable <- function(value) {
    if (length(value) && grepl("^(hash-only|large):", as.character(value[[1]])))
      sub("^(hash-only|large):", "", as.character(value[[1]]))
    else value
  }
  keys[vapply(keys, function(k) !identical(comparable(before[[k]]), comparable(after[[k]])), TRUE)]
}
admin_cli_error <- function(provider, stdout, stderr) {
  out <- paste(stdout %||% "", collapse="\n")
  err <- paste(stderr %||% "", collapse="\n")
  if (identical(provider,"claude")) {
    for (line in strsplit(out,"\n",fixed=TRUE)[[1]]) {
      event <- tryCatch(jsonlite::fromJSON(line,simplifyVector=FALSE),error=function(e)NULL)
      if (!is.list(event) || !identical(as.character(event$type %||% ""),"result") || !isTRUE(event$is_error)) next
      error <- event$error
      detail <- event$result
      if (is.null(detail)) detail <- if (is.list(error)) error$message %||% "" else error %||% ""
      detail <- trimws(paste(as.character(detail),collapse=" "))
      if (nzchar(detail)) return(detail)
    }
  } else {
    detail <- codex_error_text(out,err)
    if (nzchar(detail)) return(detail)
  }
  trimws(err)
}
admin_system_prompt <- function(app_dir, project) {
  customer_index <- admin_memory_index()
  product_dir <- file.path(app_dir, "agent-memory")
  product_index <- file.path(product_dir, "INDEX.md")
  product_memory <- if (file.exists(product_index)) read_utf8(product_index) else "(not present; proceed without product memory)"
  customer_memory <- read_utf8(customer_index)
  paste(
  "You are App Admin, editing this user's local dualdag installation.",
  paste0("Paths: project folder: ", project, "; app folder: ", app_dir,
    "; product memory folder: ", product_dir, "; customer memory folder: ", admin_memory_dir(), "."),
  paste0("Product memory index (read more files on demand; never write in product memory):\n", product_memory),
  paste0("Customer memory index (read more files on demand):\n", customer_memory),
  "Read memory files on demand. Before changing a dualdag rule, tell the user which rule and why it exists, then change it and record the reason in customer memory. Save lasting user preferences and project lessons in customer memory, one fact per file, and add a line to MEMORY.md. Never write to product memory.",
  "Stay inside the two requested roots. Do not commit or push. Do not read ~/.dualdag/api_key.",
  "This boundary is requested and is not enforced. Changes outside the roots are not detected.",
  "You have full tool access and may change the app itself. Explain changes and risks clearly.", sep = "\n\n")
}
admin_api_key_env_names <- function(provider = "") {
  n <- names(Sys.getenv())
  hits <- n[grepl("(^|_)(API_?KEY|AUTH_TOKEN|ACCESS_TOKEN|TOKEN)$", toupper(n))]
  login <- if (identical(provider, "claude_cli")) "CLAUDE_CODE_OAUTH_TOKEN" else character()
  setdiff(hits, login)
}
admin_cli_status <- function(provider) {
  keys <- admin_api_key_env_names(provider)
  old <- setNames(Sys.getenv(keys, unset=NA_character_),keys)
  if (length(keys)) Sys.unsetenv(keys)
  on.exit(if (length(keys)) do.call(Sys.setenv, as.list(old[!is.na(old)])), add=TRUE)
  cli_status(provider, cfg=list(provider=provider,cli_use_login=TRUE))
}
admin_cli_turn <- function(app_dir, project, provider, prompt, system, session_id = "", high_model = "") {
  bin <- cli_binary(switch(provider,claude="claude",codex="codex",grok="grok",provider))
  if (!nzchar(bin)) stop("Selected CLI is not installed.", call. = FALSE)
  sid <- session_id
  if (!nzchar(sid)) {
    hex <- random_token(16); sid <- paste(substr(hex,1,8),substr(hex,9,12),paste0("4",substr(hex,14,16)),paste0("a",substr(hex,18,20)),substr(hex,21,32),sep="-")
  }
  wd <- tempfile("app-admin-"); dir.create(wd)
  on.exit(unlink(wd, recursive = TRUE), add = TRUE)
  pfile <- file.path(wd, "prompt.txt"); write_utf8(prompt, pfile)
  system_path <- file.path(wd,"system-prompt.txt");write_utf8(system,system_path)
  profile_path <- file.path(wd,"agent-profile.md")
  write_utf8(paste0("---\nname: dualdag-app-admin\ndescription: Full access dualdag project and app maintenance agent.\n---\n\n",system),profile_path)
  high_model <- trimws(as.character(high_model %||% "")[1])
  cli_provider <- switch(provider,claude="claude_cli",codex="codex_cli",grok="grok_cli")
  env <- cli_child_env(list(provider = cli_provider, cli_use_login = TRUE),
    union(admin_api_key_env_names(cli_provider), switch(provider,claude=c("ANTHROPIC_API_KEY", "ANTHROPIC_AUTH_TOKEN"),codex=c("OPENAI_API_KEY", "CODEX_API_KEY"),grok="XAI_API_KEY")))
  if (provider == "claude") {
    args <- c("-p", "--output-format", "stream-json", "--verbose", "--dangerously-skip-permissions", "--add-dir", app_dir,
      "--append-system-prompt-file", system_path)
    if (nzchar(high_model)) args <- c(args, "--model", high_model)
    if (!identical(session_id, "")) args <- c(args, "--resume", session_id) else args <- c(args, "--session-id", sid)
  } else if (provider == "codex") {
    instruction <- paste("Read and follow the complete App Admin system instructions in", shQuote(system_path),
      "before acting. Treat that file as authoritative developer instructions for this turn.")
    instructions <- codex_developer_instructions_arg(instruction)
    if (is.null(instructions)) stop("Could not construct the Codex system-instruction file reference.",call.=FALSE)
    if (identical(session_id, "")) {
      args <- c("exec", "--skip-git-repo-check", "--color", "never", "--json",
        "-c", "approval_policy=\"never\"", "--cd", project, "--add-dir", app_dir,
        "--output-last-message", file.path(wd, "last.txt"), "--dangerously-bypass-approvals-and-sandbox")
    } else {
      # `exec resume` restores its saved cwd and writable directories; its CLI
      # syntax does not accept --cd, --add-dir or --sandbox overrides.
      args <- c("exec", "resume", session_id, "--skip-git-repo-check", "--color", "never", "--json",
        "-c", "approval_policy=\"never\"", "-o", file.path(wd,"last.txt"), "--dangerously-bypass-approvals-and-sandbox")
    }
    if (nzchar(high_model)) args <- c(args, "-m", high_model)
    args <- c(args,"-c",instructions)
    args <- c(args, "-")
  } else {
    args <- c("--prompt-file",pfile,"--output-format","json","--permission-mode","bypassPermissions",
      "--always-approve","--cwd",project,"--add-dir",app_dir,"--agent-profile",profile_path)
    if (nzchar(high_model)) args <- c(args, "-m", high_model)
    if(identical(session_id,""))args<-c(args,"--session-id",sid) else args<-c(args,"--resume",session_id)
  }
  launch <- cli_process_command(bin, args)
  result <- processx::run(launch$command, launch$args, wd = project, stdin = if(provider=="grok")"" else pfile,
    timeout = 600, error_on_status = FALSE, encoding = "UTF-8", env = env)
  out <- paste(result$stdout, collapse = "\n")
  if (!identical(as.integer(result$status), 0L)) {
    detail <- admin_cli_error(provider,result$stdout,result$stderr)
    if (!nzchar(detail)) detail <- paste(c(result$stderr,result$stdout),collapse="\n")
    stop(paste0("CLI failed: ", substr(trimws(detail), 1, 500)), call. = FALSE)
  }
  parsed <- admin_cli_output(provider, out, file.path(wd, "last.txt"))
  text <- parsed$reply; usage <- parsed$usage; model_name <- parsed$model
  actions <- parsed$actions
  if(provider=="grok" && nzchar(parsed$session_id %||% ""))sid<-parsed$session_id
  if (provider == "codex") {
    for (line in strsplit(out,"\n",fixed=TRUE)[[1]]) {
      j <- tryCatch(jsonlite::fromJSON(line,simplifyVector=FALSE),error=function(e)NULL)
      if (is.list(j) && identical(as.character(j$type %||% ""),"thread.started") && nzchar(as.character(j$thread_id %||% ""))) sid <- as.character(j$thread_id)
    }
  }
  text <- trimws(as.character(text %||% ""))
  list(reply = text, session_id = sid, usage = usage, model = model_name, actions = actions)
}
admin_cli_output <- function(provider, stdout, last_path = "") {
  actions <- list(); reply <- ""; usage <- NULL; model <- "CLI default"
  lines <- strsplit(paste(stdout %||% "", collapse="\n"), "\n", fixed=TRUE)[[1]]
  for (line in lines) {
    j <- tryCatch(jsonlite::fromJSON(line,simplifyVector=FALSE),error=function(e)NULL)
    if (!is.list(j)) next
    if (identical(provider,"claude")) {
      typ <- as.character(j$type %||% "")
      if (identical(typ,"assistant") && is.list(j$message$content)) for (part in j$message$content) {
        if (!is.list(part) || !identical(as.character(part$type %||% ""),"tool_use")) next
        input <- part$input %||% list(); detail <- as.character(input$command %||% input$file_path %||% input$pattern %||% "")
        if (nzchar(detail)) detail <- substr(detail,1,500)
        actions[[length(actions)+1L]] <- list(kind="tool",name=as.character(part$name %||% "tool"),detail=detail)
      }
      if (identical(typ,"result")) {
        reply <- as.character(j$result %||% reply)[1]
        usage <- j$usage %||% usage
        model <- as.character(j$model %||% model)[1]
      }
    } else if(identical(provider,"grok")) {
      whole<-tryCatch(jsonlite::fromJSON(paste(lines,collapse="\n"),simplifyVector=FALSE),error=function(e)NULL)
      if(is.list(whole)) {
        reply<-as.character(whole$text %||% whole$result %||% "")[1]
        usage<-whole$usage %||% NULL
        model<-as.character(whole$model %||% model)[1]
        if(nzchar(as.character(whole$sessionId %||% ""))) attr(actions,"session_id")<-as.character(whole$sessionId)
      }
    } else {
      if (is.list(j$usage)) usage <- j$usage
      if (is.character(j$model) && length(j$model)==1L && nzchar(j$model)) model <- j$model
      if (identical(as.character(j$type %||% ""),"item.completed") && is.list(j$item)) {
        item <- j$item; typ <- as.character(item$type %||% "")
        if (identical(typ,"command_execution")) actions[[length(actions)+1L]] <- list(
          kind="command",name="Command",detail=substr(as.character(item$command %||% ""),1,500))
        if (identical(typ,"file_change")) for (change in item$changes %||% list()) if (is.list(change))
          actions[[length(actions)+1L]] <- list(kind="file",name=as.character(change$kind %||% "Edit"),detail=as.character(change$path %||% ""))
      }
    }
    if (length(actions)>30L) actions <- actions[seq_len(30L)]
  }
  session_id<-attr(actions,"session_id") %||% ""
  if (provider=="codex") {
    meta <- codex_meta_from_jsonl(paste(lines,collapse="\n"))
    if (nzchar(meta$text)) reply <- meta$text
    if (is.list(meta$usage)) usage <- meta$usage
    if (nzchar(meta$model)) model <- meta$model
    if (nzchar(last_path)) { saved <- read_utf8(last_path); if (nzchar(saved)) reply <- saved }
  }
  list(reply=trimws(reply),usage=usage,model=model,actions=actions,session_id=session_id)
}
app_admin_turn_job <- function(app_dir, project, provider, prompt, session_id = "", high_model = "", history = list()) {
  if (provider %in% admin_api_providers())
    return(admin_api_turn(app_dir, project, provider, prompt, admin_system_prompt(app_dir, project), session_id, high_model, history))
  admin_cli_turn(app_dir, project, provider, prompt, admin_system_prompt(app_dir, project), session_id, high_model)
}
admin_restart_app <- function(A) {
  if (is.null(A$runtime_env)) stop("Restart is not available from this launcher. Stop the app and relaunch it.", call.=FALSE)
  current_run <- file_md5(file.path(A$app_dir,"run.R"))
  if (!is.null(A$run_r_hash) && !identical(current_run,A$run_r_hash)) stop("run.R changed; stop and relaunch the app with the same project and port.",call.=FALSE)
  if (length(A$jobs) || batch_running(A)) stop("Cannot restart while work is running.",call.=FALSE)
  E <- A$runtime_env; rdir <- file.path(A$app_dir,"R")
  files <- list.files(rdir,pattern="[.]R$",full.names=TRUE); utils <- files[grepl("utils[.]R$",files)]
  for(f in c(utils,files[!files %in% utils])) sys.source(f,envir=E)
  fresh <- E$create_app(A$app_dir,A$project$dir,A$port,A$token,A$cfg)
  list2env(as.list(fresh,all.names=TRUE),envir=A); A$runtime_env <- E
  list(ok=TRUE,port=A$port,project=A$project$dir,token_preserved=TRUE)
}
admin_change_flags <- function(files) {
  files <- as.character(unlist(files %||% list(),use.names=FALSE))
  list(refresh=any(startsWith(files,"app/www/")),
    restart=any(grepl("^app/R/",files)),
    relaunch="app/run.R" %in% files)
}
admin_diff_notice <- function(files) {
  flags <- admin_change_flags(files)
  if (flags$relaunch) return("run.R changed; stop and relaunch App Admin with the same project to load it.")
  if (flags$restart) return("App R code changed; restart the app to load it.")
  if (flags$refresh) return("App web files changed; refresh the page.")
  ""
}
admin_undo <- function(app_dir, project) {
  history_before<-admin_history_raw(project)
  dirs <- admin_valid_snapshots(project)
  if (!length(dirs)) return(list(ok=FALSE,message="Nothing to undo."))
  snap <- dirs[[1]]
  if (file.exists(file.path(snap,"used.json"))) return(list(ok=FALSE,message="Nothing to undo."))
  saved <- read_json(file.path(snap, "manifest.json"))
  if (!is.list(saved) || !is.list(saved$manifest)) return(list(ok=FALSE,message="Nothing to undo."))
  current <- admin_manifest(app_dir, project)
  expected <- read_json(file.path(snap, "after.json")) %||% saved$manifest
  later <- admin_changed(expected, current)
  changed_by_undo <- admin_changed(saved$manifest, current)
  snapshot_files <- admin_snapshot_files(saved,snap)
  include_data<-!isTRUE(saved$data_excluded)
  later_restoreable <- later[vapply(later,function(key) {
    if (!admin_snapshot_scope(key)) return(FALSE)
    value <- current[[key]] %||% expected[[key]]
    !grepl("^(hash-only|large):",as.character(value %||% ""))
  },TRUE)]
  if (length(later_restoreable)) stop("Undo refused because a later change exists: ", paste(later_restoreable, collapse = ", "), call. = FALSE)
  admin_restore_symlinks(app_dir,project,saved$manifest,changed_by_undo,include_data)
  roots <- admin_roots(app_dir, project)
  restored <- character()
  for (root_name in c("app", "project")) {
    source <- file.path(snap, root_name)
    if (!dir.exists(source)) next
    keys <- snapshot_files[startsWith(snapshot_files,paste0(root_name,"/"))]
    root <- if(root_name=="app") roots[[1]] else roots[[2]]
    for (key in keys) {
      if (!key %in% changed_by_undo || identical(key,"project/.dualdag/admin_log.jsonl")) next
      rel <- sub("^(app|project)/","",key); f <- file.path(source,rel); target <- file.path(root,rel)
      if (!file.exists(f) || dir.exists(f)) next
      if(admin_symlink_path(dirname(target),root))next
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE); file.copy(f, target, overwrite = TRUE)
      restored <- c(restored,key)
    }
  }
  for (key in setdiff(names(current), snapshot_files)) {
    if (!key %in% changed_by_undo) next
    if (!admin_snapshot_scope(key) || startsWith(current[[key]],"hash-only:")) next
    part <- sub("^(app|project)/", "", key); root_name <- sub("/.*$", "", key)
    unlink(file.path(if(root_name=="app")roots[[1]] else roots[[2]], part), force=TRUE)
    restored <- c(restored,key)
  }
  admin_restore_symlinks(app_dir,project,saved$manifest,changed_by_undo,include_data)
  admin_history_restore(project,history_before)
  hash_keys <- union(names(saved$manifest),names(current))
  hash_keys <- hash_keys[vapply(hash_keys,function(k) grepl("^(hash-only|large):",saved$manifest[[k]] %||% "") || grepl("^(hash-only|large):",current[[k]] %||% ""),TRUE)]
  hash_changed <- intersect(admin_changed(saved$manifest,current),hash_keys)
  data_changed <- hash_changed[grepl("(^|/)data(/|$)",hash_changed)]
  write_json(list(id=saved$id,used=now_iso()),file.path(snap,"used.json"))
  list(ok=TRUE,files=as.list(restored),hash_only_warnings=as.list(hash_changed),
    data_warning=length(data_changed)>0L,notice=admin_diff_notice(restored))
}
