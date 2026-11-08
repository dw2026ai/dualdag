# dualdag launcher.
#   RStudio:      open this folder and run  source("run.R")
#   Command line: Rscript run.R [--no-browser] [--port=7979] [--project=DIR] [--install-deps] [--restore-admin-snapshot]
# Starts a local web server on 127.0.0.1 and opens the browser. Stop with Ctrl+C.

local({
  args <- commandArgs(trailingOnly = TRUE)
  flag <- function(name) any(args == paste0("--", name))
  opt <- function(name, default = NULL) {
    hit <- grep(paste0("^--", name, "="), args, value = TRUE)
    if (length(hit)) sub(paste0("^--", name, "="), "", hit[1]) else default
  }

  app_dir <- NULL
  fa <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(fa)) app_dir <- dirname(normalizePath(sub("^--file=", "", fa[1]), mustWork = FALSE))
  if (is.null(app_dir)) {
    for (i in rev(seq_len(sys.nframe()))) {
      f <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
      if (!is.null(f)) { app_dir <- dirname(normalizePath(f)); break }
    }
  }
  if (is.null(app_dir) || !file.exists(file.path(app_dir, "R", "server.R"))) app_dir <- getwd()
  if (!file.exists(file.path(app_dir, "R", "server.R"))) stop("Run this from the dualdag folder (R/server.R not found).")

  E <- new.env(parent = globalenv())
  sys.source(file.path(app_dir, "R", "deps.R"), envir = E)
  if (flag("help")) {
    cat("Usage: Rscript run.R [--no-browser] [--port=N] [--project=DIR] [--install-deps] [--restore-admin-snapshot]\n")
    return(invisible())
  }
  E$dualdag_ensure_deps(install_flag = flag("install-deps"))

  if (flag("restore-admin-snapshot")) {
    project <- opt("project", Sys.getenv("DUALDAG_PROJECT", ""))
    if (!nzchar(project)) project <- file.path(path.expand("~"), "dualdag-projects", "default")
    project <- normalizePath(project, winslash="/", mustWork=TRUE)
    snapshot_root <- file.path(project,".dualdag","app-admin","snapshots")
    dirs <- if(dir.exists(snapshot_root))list.dirs(snapshot_root,recursive=FALSE,full.names=TRUE)else character()
    dirs <- sort(dirs[file.exists(file.path(dirs,"manifest.json"))],decreasing=TRUE)
    if(!length(dirs))stop("No App Admin snapshot exists in the selected project.")
    snap <- dirs[[1]]
    saved <- jsonlite::fromJSON(file.path(snap,"manifest.json"),simplifyVector=FALSE)
    manifest <- saved$manifest
    scope <- function(key) startsWith(key,"app/") || (grepl("^project/(spec|programs|[.]dualdag/state)/",key) && !identical(key,"project/.dualdag/state/source_catalog.json"))
    hash_only <- function(key,path=NULL) {
      value<-manifest[[key]]; saved_large<-length(value)>0L&&grepl("^(hash-only|large):",as.character(value[[1]]))
      size<-if(is.null(path))NA_real_ else file.info(path)$size
      saved_large||grepl("(^|/)data(/|$)",sub("^project/","",key))||(!is.na(size)&&size>5*1024^2)
    }
    snapshot_files <- unlist(saved$snapshot_files,use.names=FALSE)
    if(is.null(saved$snapshot_files)) {
      snapshot_files<-character()
      for(kind in c("app","project")) {
        root<-file.path(snap,kind); files<-if(dir.exists(root))list.files(root,recursive=TRUE,all.files=TRUE,full.names=TRUE,no..=TRUE)else character()
        files<-files[!file.info(files)$isdir%in%TRUE]
        if(length(files))snapshot_files<-c(snapshot_files,paste0(kind,"/",substring(files,nchar(root)+2L)))
      }
      snapshot_files<-snapshot_files[vapply(snapshot_files,scope,TRUE)]
    }
    for(kind in c("app","project")) {
      root<-if(kind=="app")app_dir else project; prefix<-paste0(kind,"/")
      for(key in snapshot_files[startsWith(snapshot_files,prefix)]) {
        rel<-sub("^(app|project)/","",key); source<-file.path(snap,kind,rel); target<-file.path(root,rel)
        if(file.exists(source)&&!dir.exists(source)){dir.create(dirname(target),recursive=TRUE,showWarnings=FALSE);file.copy(source,target,overwrite=TRUE)}
      }
      existing<-if(kind=="app")list.files(root,recursive=TRUE,all.files=TRUE,full.names=TRUE,no..=TRUE) else
        unlist(lapply(c("spec","programs",file.path(".dualdag","state")),function(d)list.files(file.path(root,d),recursive=TRUE,all.files=TRUE,full.names=TRUE,no..=TRUE)),use.names=FALSE)
      existing<-existing[!file.info(existing)$isdir%in%TRUE]
      existing<-existing[!grepl("(^|/)([.]git|node_modules|[.]cache)(/|$)",gsub("\\\\","/",existing))]
      for(f in existing) {
        rel<-substring(normalizePath(f,winslash="/"),nchar(normalizePath(root,winslash="/"))+2L); key<-paste0(prefix,rel)
        if(scope(key)&&!key%in%snapshot_files&&!hash_only(key,f))unlink(f,force=TRUE)
      }
    }
    cat("Restored the latest App Admin snapshot. Restart the app normally.\n")
    return(invisible(NULL))
  }

  # utils.R first so %||% exists while the other files are sourced.
  rdir <- file.path(app_dir, "R")
  files <- list.files(rdir, pattern = "\\.R$", full.names = TRUE)
  utils <- files[grepl("utils\\.R$", files)]
  for (f in c(utils, files[!files %in% utils])) sys.source(f, envir = E)
  `%||%` <- E$`%||%`

  cfg <- E$load_config()
  port_opt <- opt("port", Sys.getenv("DUALDAG_PORT", ""))
  port <- if (nzchar(port_opt)) as.integer(port_opt) else as.integer(cfg$port %||% 7979L)
  project <- opt("project", Sys.getenv("DUALDAG_PROJECT", cfg$project %||% ""))
  if (!nzchar(project)) project <- file.path(path.expand("~"), "dualdag-projects", "default")

  headless <- .Platform$OS.type == "unix" && Sys.info()[["sysname"]] == "Linux" &&
    !nzchar(Sys.getenv("DISPLAY")) && !nzchar(Sys.getenv("WAYLAND_DISPLAY"))
  open_browser <- !flag("no-browser") && !identical(Sys.getenv("DUALDAG_NO_BROWSER"), "1") && !headless

  token <- E$random_token(24)
  A <- E$create_app(app_dir, project, port, token, cfg)
  A$runtime_env <- E
  srv <- E$start_server(A, port)
  url <- sprintf("http://127.0.0.1:%d/#token=%s", A$port, token)
  cleanup <- function() {
    try(E$shutdown_app(A), silent = TRUE)
    try(httpuv::stopServer(srv), silent = TRUE)
    cat("dualdag stopped.\n")
  }
  on.exit(cleanup(), add = TRUE)

  cat("\n  dualdag is running (R ", as.character(getRversion()), ")\n", sep = "")
  cat("  Project: ", A$project$dir, "\n", sep = "")
  cat("  Open:    ", url, "\n", sep = "")
  cat("  (local only, 127.0.0.1; the token in the link is required. Stop with Ctrl+C.)\n\n", sep = "")
  url_file <- Sys.getenv("DUALDAG_URL_FILE")
  if (nzchar(url_file)) writeLines(url, url_file)
  if (open_browser) try(utils::browseURL(url), silent = TRUE)

  tryCatch(
    repeat httpuv::service(100),
    interrupt = function(e) invisible(NULL)
  )
})
