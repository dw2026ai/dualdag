# Built-in App Admin tools for API-backed providers. Like the CLI instructions,
# the two-root boundary is requested rather than enforced.
admin_api_providers <- function() c("anthropic", "openai", "openai_compatible")

admin_api_model <- function(cfg) {
  strong <- trimws(as.character(cfg$strong_model %||% "")[1])
  if (nzchar(strong)) strong else trimws(as.character(cfg$model %||% "")[1])
}

admin_api_model_for_provider <- function(cfg, provider) {
  if (identical(as.character(cfg$provider %||% ""),provider)) return(admin_api_model(cfg))
  tier <- config_model_tiers(provider)$high
  if (nzchar(tier %||% "")) return(tier)
  trimws(as.character(cfg$model %||% "")[1])
}

admin_api_key_path <- function() normalizePath(api_key_file(), winslash="/", mustWork=FALSE)

admin_api_resolve_path <- function(path, app_dir, project) {
  path <- path.expand(as.character(path %||% "")[1])
  if (!nzchar(path)) stop("path is required.", call.=FALSE)
  if (!grepl("^(?:[A-Za-z]:[/\\\\]|/)", path, perl=TRUE)) path <- file.path(project, path)
  normalizePath(path, winslash="/", mustWork=FALSE)
}

admin_api_refuse_key_path <- function(path) {
  target <- admin_api_key_path()
  candidate <- normalizePath(path, winslash="/", mustWork=FALSE)
  if (identical(candidate, target)) stop("Access to ~/.dualdag/api_key is not allowed.", call.=FALSE)
  invisible(candidate)
}

admin_api_tool <- function(name, args, app_dir, project) {
  args <- args %||% list()
  path <- if (!is.null(args$path)) admin_api_resolve_path(args$path, app_dir, project) else ""
  if (nzchar(path)) admin_api_refuse_key_path(path)
  switch(name,
    list_dir = {
      if (!dir.exists(path)) stop("Directory does not exist.", call.=FALSE)
      entries <- list.files(path, all.files=TRUE, no..=TRUE)
      info <- file.info(file.path(path, entries))
      list(path=path, entries=lapply(seq_along(entries), function(i) list(name=entries[[i]], is_dir=isTRUE(info$isdir[[i]]))))
    },
    read_file = {
      if (!file.exists(path) || dir.exists(path)) stop("File does not exist.", call.=FALSE)
      admin_api_refuse_key_path(normalizePath(path, winslash="/", mustWork=TRUE))
      offset <- suppressWarnings(as.integer(args$offset %||% 0L)); limit <- suppressWarnings(as.integer(args$limit %||% 2000L))
      if (is.na(offset) || offset < 0L) stop("offset must be a non-negative integer.",call.=FALSE)
      if (is.na(limit) || limit < 1L) stop("limit must be a positive integer.",call.=FALSE)
      limit <- min(10000L,limit)
      lines <- readLines(path, warn=FALSE, encoding="UTF-8")
      list(path=path, offset=offset, lines=as.list(utils::head(utils::tail(lines, max(0L,length(lines)-offset)), limit)), total_lines=length(lines))
    },
    write_file = {
      admin_api_refuse_key_path(path)
      write_utf8(as.character(args$content %||% ""), path)
      list(ok=TRUE, path=path)
    },
    edit_file = {
      if (!file.exists(path) || dir.exists(path)) stop("File does not exist.", call.=FALSE)
      admin_api_refuse_key_path(normalizePath(path, winslash="/", mustWork=TRUE))
      old <- as.character(args$old %||% ""); new <- as.character(args$new %||% "")
      if (!nzchar(old)) stop("edit_file old text must not be empty.",call.=FALSE)
      content <- read_utf8(path); hits <- gregexpr(old, content, fixed=TRUE)[[1]]
      count <- if (identical(hits[[1]], -1L)) 0L else length(hits)
      if (count != 1L) stop(sprintf("edit_file requires exactly one match; found %d.", count), call.=FALSE)
      pos <- hits[[1]]; write_utf8(paste0(substr(content,1L,pos-1L),new,substr(content,pos+nchar(old),nchar(content))),path)
      list(ok=TRUE,path=path)
    },
    run_command = {
      command <- as.character(args$command %||% "")[1]
      if (!nzchar(command)) stop("command is required.", call.=FALSE)
      if (grepl("api_key",command,fixed=TRUE)) stop("Commands that reference ~/.dualdag/api_key are not allowed.",call.=FALSE)
      cwd <- if (is.null(args$cwd) || !nzchar(as.character(args$cwd)[1])) project else admin_api_resolve_path(args$cwd,app_dir,project)
      result <- processx::run("bash", c("-lc",command), wd=cwd, timeout=60, error_on_status=FALSE, encoding="UTF-8")
      list(status=result$status,stdout=substr(paste(result$stdout,collapse="\n"),1L,20000L),stderr=substr(paste(result$stderr,collapse="\n"),1L,10000L),timeout=isTRUE(result$timeout))
    },
    search = {
      pattern <- as.character(args$pattern %||% "")[1]
      if (!nzchar(pattern)) stop("pattern is required.", call.=FALSE)
      root <- if (is.null(args$path) || !nzchar(as.character(args$path)[1])) project else path
      if (!dir.exists(root) && !file.exists(root)) stop("Search path does not exist.",call.=FALSE)
      if (dir.exists(root)) {
        hits <- system2("rg", c("--line-number","--with-filename","--hidden","--glob","!.git/**","--glob","!node_modules/**","--glob","!api_key","--",pattern,root),stdout=TRUE,stderr=FALSE)
        if (length(hits)>500L) hits <- hits[seq_len(500L)]
      } else hits <- system2("rg",c("--line-number","--with-filename","--",pattern,root),stdout=TRUE,stderr=FALSE)
      list(matches=as.list(hits))
    },
    stop("Unknown tool: ", name, call.=FALSE)
  )
}

admin_api_tool_definitions <- function(provider) {
  schemas <- list(
    list(name="list_dir",description="List directory entries.",input_schema=list(type="object",properties=list(path=list(type="string")),required=list("path"))),
    list(name="read_file",description="Read a UTF-8 text file by line offset and limit.",input_schema=list(type="object",properties=list(path=list(type="string"),offset=list(type="integer"),limit=list(type="integer")),required=list("path"))),
    list(name="write_file",description="Write a UTF-8 text file.",input_schema=list(type="object",properties=list(path=list(type="string"),content=list(type="string")),required=list("path","content"))),
    list(name="edit_file",description="Replace old text only when it occurs exactly once.",input_schema=list(type="object",properties=list(path=list(type="string"),old=list(type="string"),new=list(type="string")),required=list("path","old","new"))),
    list(name="run_command",description="Run a shell command (60 second timeout, output truncated).",input_schema=list(type="object",properties=list(command=list(type="string"),cwd=list(type="string")),required=list("command"))),
    list(name="search",description="Search text using ripgrep.",input_schema=list(type="object",properties=list(pattern=list(type="string"),path=list(type="string")),required=list("pattern")))
  )
  if (identical(provider,"anthropic")) return(lapply(schemas,function(x) list(name=x$name,description=x$description,input_schema=x$input_schema)))
  lapply(schemas,function(x) list(type="function",`function`=list(name=x$name,description=x$description,parameters=x$input_schema)))
}

admin_api_http <- function(cfg, body) {
  injected <- getOption("dualdag.admin_api_http")
  if (is.function(injected)) return(injected(cfg,body))
  key <- cfg$api_key %||% ""
  if (!nzchar(key)) stop("No API key configured.",call.=FALSE)
  provider <- cfg$provider; base <- llm_base_url(cfg); model <- admin_api_model(cfg)
  if (!nzchar(model)) stop("No model name configured.",call.=FALSE)
  if (identical(provider,"anthropic")) {
    req <- httr2::request(paste0(base,"/v1/messages"))
    req <- httr2::req_headers(req, `x-api-key`=key, `anthropic-version`="2023-06-01", .redact="x-api-key")
  } else {
    req <- httr2::request(paste0(base,"/chat/completions"))
    req <- httr2::req_headers(req, Authorization=paste("Bearer",key), .redact="Authorization")
  }
  req <- httr2::req_body_json(req,body,auto_unbox=TRUE)
  req <- httr2::req_timeout(req,as.numeric(cfg$llm_timeout_s %||% 180))
  req <- httr2::req_user_agent(req,"dualdag")
  req <- httr2::req_error(req,is_error=function(resp) FALSE)
  resp <- httr2::req_perform(req); status <- httr2::resp_status(resp)
  if (status < 200L || status >= 300L) stop(sprintf("Provider request failed with HTTP %d.",status),call.=FALSE)
  httr2::resp_body_json(resp,simplifyVector=FALSE)
}

admin_api_request <- function(cfg, system, messages, tools) {
  injected <- getOption("dualdag.admin_api_request")
  if (is.function(injected)) return(injected(cfg,system,messages,tools))
  model <- admin_api_model(cfg)
  if (identical(cfg$provider,"anthropic")) {
    body <- list(model=model,max_tokens=6000L,system=system,messages=messages,tools=tools)
  } else {
    body <- list(model=model,messages=c(list(list(role="system",content=system)),messages),tools=tools,tool_choice="auto")
    if (identical(cfg$provider,"openai")) body$max_completion_tokens <- 6000L else body$max_tokens <- 6000L
  }
  admin_api_http(cfg,body)
}

admin_api_turn <- function(app_dir, project, provider, prompt, system, session_id="", high_model="", history=list()) {
  cfg <- config_public(load_config()); cfg$provider <- provider; cfg$api_key <- get_api_key()$key
  cfg$strong_model <- high_model %||% cfg$strong_model
  model <- admin_api_model(cfg)
  if (!provider %in% admin_api_providers()) stop("Unknown API provider.",call.=FALSE)
  if (!nzchar(cfg$api_key)) stop("No API key configured.",call.=FALSE)
  if (!nzchar(model)) stop("No model name configured.",call.=FALSE)
  anthropic <- identical(provider,"anthropic"); tools <- admin_api_tool_definitions(provider)
  messages <- history
  messages[[length(messages)+1L]] <- if (anthropic) list(role="user",content=prompt) else list(role="user",content=prompt)
  actions <- list(); usage <- list(); reply <- ""
  for (step in seq_len(40L)) {
    response <- admin_api_request(cfg,system,messages,tools)
    usage[[length(usage)+1L]] <- response$usage %||% NULL
    if (anthropic) {
      content <- response$content %||% list(); calls <- Filter(function(x) identical(x$type,"tool_use"),content)
      texts <- vapply(Filter(function(x) identical(x$type,"text"),content),function(x) x$text %||% "","")
      assistant <- list(role="assistant",content=content); messages[[length(messages)+1L]] <- assistant
      if (!length(calls)) { reply <- paste(texts,collapse=""); break }
      results <- lapply(calls,function(call) {
        value <- tryCatch(admin_api_tool(call$name,call$input,app_dir,project),error=function(e) list(error=conditionMessage(e)))
        actions[[length(actions)+1L]] <<- list(kind="tool",name=call$name,detail=substr(as.character(call$input$path %||% call$input$command %||% call$input$pattern %||% ""),1L,500L))
        list(type="tool_result",tool_use_id=call$id,content=json_text(value),is_error=!is.null(value$error))
      })
      messages[[length(messages)+1L]] <- list(role="user",content=results)
    } else {
      choice <- response$choices[[1]]$message; tool_calls <- choice$tool_calls %||% list()
      assistant <- list(role="assistant",content=choice$content %||% NULL)
      if (length(tool_calls)) assistant$tool_calls <- tool_calls
      messages[[length(messages)+1L]] <- assistant
      if (!length(tool_calls)) { reply <- as.character(choice$content %||% "")[1]; break }
      for (call in tool_calls) {
        fn <- call[["function"]]; args <- jsonlite::fromJSON(fn$arguments %||% "{}",simplifyVector=FALSE)
        value <- tryCatch(admin_api_tool(fn$name,args,app_dir,project),error=function(e) list(error=conditionMessage(e)))
        detail <- args$path %||% args$command %||% args$pattern %||% ""
        actions[[length(actions)+1L]] <- list(kind="tool",name=fn$name,detail=substr(as.character(detail),1L,500L))
        messages[[length(messages)+1L]] <- list(role="tool",tool_call_id=call$id,content=json_text(value))
      }
    }
    if (step == 40L) stop("App Admin API tool loop reached the 40-step limit.",call.=FALSE)
  }
  list(reply=trimws(reply),session_id=session_id,history=messages,usage=llm_usage_combine(usage),model=response$model %||% model,actions=actions)
}
