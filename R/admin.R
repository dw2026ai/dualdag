# App Admin uses the user's logged-in coding CLI. Its two-root boundary is an
# instruction to the CLI, not a filesystem boundary or sandbox.
admin_store_dir <- function(project) file.path(project, ".dualdag", "app-admin")
admin_session_path <- function(project) file.path(admin_store_dir(project), "session.json")
admin_snapshot_root <- function(project) file.path(admin_store_dir(project), "snapshots")

admin_roots <- function(app_dir, project) normalizePath(c(app_dir, project), winslash = "/", mustWork = TRUE)
admin_rel <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  if (!startsWith(path, paste0(root, "/"))) return(NA_character_)
  substring(path, nchar(root) + 2L)
}
admin_routine_project_path <- function(rel) grepl("^[.]dualdag/(app-admin|runs|llm_log|logs)(/|$)|^[.]dualdag/state/source_catalog[.]json$", rel)
admin_files <- function(app_dir, project) {
  roots <- admin_roots(app_dir, project)
  files <- unlist(lapply(roots, function(root) list.files(root, recursive = TRUE, all.files = TRUE,
    full.names = TRUE, no.. = TRUE)), use.names = FALSE)
  files <- files[file.info(files)$isdir %in% FALSE]
  files <- files[!grepl("(^|/)([.]git|node_modules|[.]cache)(/|$)", gsub("\\\\", "/", files))]
  rel_project <- vapply(files, admin_rel, "", root = roots[[2]])
  rel_app <- vapply(files, admin_rel, "", root = roots[[1]])
  in_app <- !is.na(rel_app)
  keep_project <- !is.na(rel_project) & !admin_routine_project_path(rel_project)
  files <- files[in_app | keep_project]
  sort(unique(files))
}
admin_snapshot_scope <- function(key) {
  if (startsWith(key, "app/")) return(TRUE)
  rel <- sub("^project/", "", key)
  if (identical(rel,".dualdag/state/source_catalog.json")) return(FALSE)
  startsWith(rel, "spec/") || startsWith(rel, "programs/") || startsWith(rel, ".dualdag/state/")
}
admin_hash_only <- function(key, size) {
  rel <- sub("^project/", "", key)
  grepl("(^|/)data(/|$)", rel) || !admin_snapshot_scope(key) || is.na(size) || size > 5 * 1024^2
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
admin_manifest <- function(app_dir, project) {
  files <- admin_files(app_dir, project)
  roots <- admin_roots(app_dir, project)
  rel <- vapply(files, function(f) {
    a <- admin_rel(f, roots[[1]]); if (!is.na(a)) paste0("app/", a) else paste0("project/", admin_rel(f, roots[[2]]))
  }, "")
  hashes <- vapply(seq_along(files), function(i) admin_file_hash(rel[[i]], files[[i]]), "")
  names(hashes) <- rel
  stats <- file.info(files)
  only <- vapply(seq_along(hashes), function(i) admin_hash_only(rel[[i]], stats$size[[i]]), TRUE)
  hashes[only] <- paste0("hash-only:", stats$size[only], ":", hashes[only])
  setNames(as.list(hashes), rel)
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
admin_snapshot <- function(app_dir, project) {
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
  snapshot_files <- keys[vapply(seq_along(keys), function(i) admin_snapshot_scope(keys[[i]]) &&
    !startsWith(manifest[[i]], "hash-only:"), TRUE)]
  for (key in snapshot_files) {
    root_name <- sub("/.*$", "", key); rel <- sub("^(app|project)/", "", key)
    root <- if (root_name == "app") roots[[1]] else roots[[2]]
    f <- file.path(root, rel)
    target <- file.path(dest, root_name, rel)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE); file.copy(f, target, overwrite = TRUE)
  }
  write_json(list(id = id, manifest = manifest, snapshot_files = as.list(snapshot_files), time = now_iso()), file.path(dest, "manifest.json"))
  dirs <- sort(list.dirs(root, recursive = FALSE, full.names = TRUE), decreasing = TRUE)
  if (length(dirs) > 20L) unlink(dirs[-seq_len(20L)], recursive = TRUE)
  id
}
admin_valid_snapshots <- function(project) {
  root <- admin_snapshot_root(project)
  dirs <- if (dir.exists(root)) list.dirs(root, recursive = FALSE, full.names = TRUE) else character()
  dirs <- dirs[file.exists(file.path(dirs, "manifest.json"))]
  sort(dirs, decreasing = TRUE)
}
admin_finish_snapshot <- function(app_dir, project, snapshot_id) {
  if (is.null(snapshot_id) || !length(snapshot_id) || !nzchar(snapshot_id)) return(invisible(NULL))
  write_json(admin_manifest(app_dir, project), file.path(admin_snapshot_root(project), snapshot_id, "after.json"))
  invisible(NULL)
}
admin_restore_snapshot <- function(app_dir, project) {
  dirs <- admin_valid_snapshots(project)
  if (!length(dirs)) stop("No App Admin snapshot exists in the selected project.", call. = FALSE)
  snap <- dirs[[1]]
  saved <- read_json(file.path(snap, "manifest.json"))
  if (!is.list(saved) || !is.list(saved$manifest)) stop("The latest App Admin snapshot is invalid.", call. = FALSE)
  snapshot_files <- admin_snapshot_files(saved, snap)
  roots <- admin_roots(app_dir, project)
  for (kind in c("app", "project")) {
    root <- if (kind == "app") roots[[1]] else roots[[2]]
    prefix <- paste0(kind, "/")
    for (key in snapshot_files[startsWith(snapshot_files, prefix)]) {
      rel <- sub("^(app|project)/", "", key)
      source <- file.path(snap, kind, rel)
      target <- file.path(root, rel)
      if (!file.exists(source) || dir.exists(source)) next
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      file.copy(source, target, overwrite = TRUE)
    }
    existing <- if (kind == "app") {
      list.files(root, recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE)
    } else {
      unlist(lapply(c("spec", "programs", file.path(".dualdag", "state")), function(d)
        list.files(file.path(root, d), recursive = TRUE, all.files = TRUE, full.names = TRUE, no.. = TRUE)), use.names = FALSE)
    }
    existing <- existing[!file.info(existing)$isdir %in% TRUE]
    existing <- existing[!grepl("(^|/)([.]git|node_modules|[.]cache)(/|$)", gsub("\\\\", "/", existing))]
    for (f in existing) {
      rel <- substring(normalizePath(f, winslash = "/"), nchar(root) + 2L)
      key <- paste0(prefix, rel)
      in_scope <- kind == "app" || grepl("^(spec|programs|[.]dualdag/state)/", rel)
      saved_value <- saved$manifest[[key]]
      size <- file.info(f)$size
      hash_only <- admin_hash_only(key, size) || (length(saved_value) > 0L && grepl("^(hash-only|large):", as.character(saved_value[[1]])))
      if (kind == "project" && startsWith(rel, ".dualdag/app-admin/")) next
      if (in_scope && !key %in% snapshot_files && !hash_only) unlink(f, force = TRUE)
    }
  }
  invisible(snap)
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
admin_system_prompt <- function(app_dir, project) paste(
  "You are App Admin, editing this user's local dualdag installation.",
  paste0("The requested two roots are app install: ", app_dir, " and open project: ", project, "."),
  "Stay inside these two roots. Do not commit or push. Do not read ~/.dualdag/api_key.",
  "This boundary is requested and is not enforced. Changes outside the roots are not detected.",
  "You have full tool access and may change the app itself. Explain changes and risks clearly.", sep = "\n\n")
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
admin_cli_turn <- function(app_dir, project, provider, prompt, system, session_id = "") {
  bin <- cli_binary(if (provider == "claude") "claude" else "codex")
  if (!nzchar(bin)) stop("Selected CLI is not installed.", call. = FALSE)
  sid <- session_id
  if (!nzchar(sid)) {
    hex <- random_token(16); sid <- paste(substr(hex,1,8),substr(hex,9,12),paste0("4",substr(hex,14,16)),paste0("a",substr(hex,18,20)),substr(hex,21,32),sep="-")
  }
  wd <- tempfile("app-admin-"); dir.create(wd)
  on.exit(unlink(wd, recursive = TRUE), add = TRUE)
  pfile <- file.path(wd, "prompt.txt"); write_utf8(prompt, pfile)
  cli_provider <- if (provider == "claude") "claude_cli" else "codex_cli"
  env <- cli_child_env(list(provider = cli_provider, cli_use_login = TRUE),
    union(admin_api_key_env_names(cli_provider), if (provider == "claude") c("ANTHROPIC_API_KEY", "ANTHROPIC_AUTH_TOKEN") else c("OPENAI_API_KEY", "CODEX_API_KEY")))
  if (provider == "claude") {
    system_path <- file.path(wd, "system-prompt.txt"); write_utf8(system, system_path)
    args <- c("-p", "--output-format", "stream-json", "--verbose", "--allow-dangerously-skip-permissions", "--dangerously-skip-permissions", "--add-dir", project,
      "--append-system-prompt-file", system_path)
    if (!identical(session_id, "")) args <- c(args, "--resume", session_id) else args <- c(args, "--session-id", sid)
  } else {
    if (identical(session_id, "")) {
      args <- c("exec", "--skip-git-repo-check", "--color", "never", "--json",
        "-c", "approval_policy=\"never\"", "--cd", app_dir, "--add-dir", project,
        "--output-last-message", file.path(wd, "last.txt"), "--sandbox", "workspace-write")
    } else {
      # `exec resume` restores its saved cwd and writable directories; its CLI
      # syntax does not accept --cd, --add-dir or --sandbox overrides.
      args <- c("exec", "resume", session_id, "--skip-git-repo-check", "--color", "never", "--json",
        "-c", "approval_policy=\"never\"", "-o", file.path(wd,"last.txt"))
    }
    instructions <- codex_developer_instructions_arg(system)
    if (!is.null(instructions)) args <- c(args, "-c", instructions)
    else write_utf8(paste("System instructions:",system,"\n\n",read_utf8(pfile)),pfile)
    args <- c(args, "-")
  }
  launch <- cli_process_command(bin, args)
  result <- processx::run(launch$command, launch$args, wd = app_dir, stdin = pfile,
    timeout = 600, error_on_status = FALSE, encoding = "UTF-8", env = env)
  if (provider == "codex" && !identical(as.integer(result$status), 0L) &&
      grepl("sandbox.{0,60}(fail|error|start|unavailable|initializ)|(?:fail|error|unable).{0,60}sandbox", paste(result$stderr,result$stdout), ignore.case=TRUE)) {
    fallback <- args[!args %in% c("--sandbox", "workspace-write", "-")]
    fallback <- c(fallback, "--dangerously-bypass-approvals-and-sandbox", "-")
    launch <- cli_process_command(bin, fallback)
    result <- processx::run(launch$command, launch$args, wd=app_dir, stdin=pfile,
      timeout=600, error_on_status=FALSE, encoding="UTF-8", env=env)
  }
  out <- paste(result$stdout, collapse = "\n")
  if (!identical(as.integer(result$status), 0L)) {
    detail <- admin_cli_error(provider,result$stdout,result$stderr)
    if (!nzchar(detail)) detail <- paste(c(result$stderr,result$stdout),collapse="\n")
    stop(paste0("CLI failed: ", substr(trimws(detail), 1, 500)), call. = FALSE)
  }
  parsed <- admin_cli_output(provider, out, file.path(wd, "last.txt"))
  text <- parsed$reply; usage <- parsed$usage; model_name <- parsed$model
  actions <- parsed$actions
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
  if (provider=="codex") {
    meta <- codex_meta_from_jsonl(paste(lines,collapse="\n"))
    if (nzchar(meta$text)) reply <- meta$text
    if (is.list(meta$usage)) usage <- meta$usage
    if (nzchar(meta$model)) model <- meta$model
    if (nzchar(last_path)) { saved <- read_utf8(last_path); if (nzchar(saved)) reply <- saved }
  }
  list(reply=trimws(reply),usage=usage,model=model,actions=actions)
}
app_admin_turn_job <- function(app_dir, project, provider, prompt, session_id = "")
  admin_cli_turn(app_dir, project, provider, prompt, admin_system_prompt(app_dir, project), session_id)
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
admin_layout_key <- function(key) grepl("^project/[.]dualdag/state/layout_[^/]+[.]json$",key)
admin_undo <- function(app_dir, project) {
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
  later_restoreable <- later[vapply(later,function(key) {
    if (!admin_snapshot_scope(key) || admin_layout_key(key)) return(FALSE)
    value <- current[[key]] %||% expected[[key]]
    !grepl("^(hash-only|large):",as.character(value %||% ""))
  },TRUE)]
  if (length(later_restoreable)) stop("Undo refused because a later change exists: ", paste(later_restoreable, collapse = ", "), call. = FALSE)
  roots <- admin_roots(app_dir, project)
  restored <- character()
  for (root_name in c("app", "project")) {
    source <- file.path(snap, root_name)
    if (!dir.exists(source)) next
    keys <- snapshot_files[startsWith(snapshot_files,paste0(root_name,"/"))]
    root <- if(root_name=="app") roots[[1]] else roots[[2]]
    for (key in keys) {
      if (!key %in% changed_by_undo) next
      if (admin_layout_key(key)) next
      rel <- sub("^(app|project)/","",key); f <- file.path(source,rel); target <- file.path(root,rel)
      if (!file.exists(f) || dir.exists(f)) next
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE); file.copy(f, target, overwrite = TRUE)
      restored <- c(restored,key)
    }
  }
  for (key in setdiff(names(current), snapshot_files)) {
    if (!key %in% changed_by_undo) next
    if (admin_layout_key(key)) next
    if (!admin_snapshot_scope(key) || startsWith(current[[key]],"hash-only:")) next
    part <- sub("^(app|project)/", "", key); root_name <- sub("/.*$", "", key)
    unlink(file.path(if(root_name=="app")roots[[1]] else roots[[2]], part), force=TRUE)
    restored <- c(restored,key)
  }
  hash_keys <- union(names(saved$manifest),names(current))
  hash_keys <- hash_keys[vapply(hash_keys,function(k) grepl("^(hash-only|large):",saved$manifest[[k]] %||% "") || grepl("^(hash-only|large):",current[[k]] %||% ""),TRUE)]
  hash_changed <- intersect(admin_changed(saved$manifest,current),hash_keys)
  data_changed <- hash_changed[grepl("(^|/)data(/|$)",hash_changed)]
  write_json(list(id=saved$id,used=now_iso()),file.path(snap,"used.json"))
  list(ok=TRUE,files=as.list(restored),hash_only_warnings=as.list(hash_changed),
    data_warning=length(data_changed)>0L,notice=admin_diff_notice(restored))
}
