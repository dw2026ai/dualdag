# Local web server (httpuv): static www/ + JSON API.
# Listens on 127.0.0.1 only. Every /api request needs X-Dualdag-Token.
# Host and Origin are checked. Long LLM and run calls go through callr::r_bg.

MIME <- c(
  html = "text/html; charset=utf-8", js = "text/javascript; charset=utf-8",
  css = "text/css; charset=utf-8", svg = "image/svg+xml", png = "image/png",
  ico = "image/x-icon", json = "application/json", txt = "text/plain; charset=utf-8",
  csv = "text/csv; charset=utf-8", md = "text/markdown; charset=utf-8"
)

TEMPLATES <- c("variable.csv", "parameter.csv", "qc-methodology.md")

json_resp <- function(x, status = 200L) {
  list(status = as.integer(status),
       headers = list(`Content-Type` = "application/json; charset=utf-8", `Cache-Control` = "no-store"),
       body = as.character(to_json(x)))
}
err_resp <- function(msg, status = 400L, ...) json_resp(c(list(error = msg), list(...)), status)
file_resp <- function(bytes, filename, type) {
  list(status = 200L, headers = list(
    `Content-Type` = type, `Cache-Control` = "no-store",
    `Content-Disposition` = sprintf('attachment; filename="%s"', filename)
  ), body = bytes)
}

parse_query <- function(qs) {
  qs <- sub("^\\?", "", qs %||% "")
  if (!nzchar(qs)) return(list())
  kv <- strsplit(strsplit(qs, "&", fixed = TRUE)[[1]], "=", fixed = TRUE)
  out <- lapply(kv, function(x) if (length(x) > 1) utils::URLdecode(gsub("+", " ", x[2], fixed = TRUE)) else "")
  names(out) <- vapply(kv, function(x) utils::URLdecode(x[1]), "")
  out
}

read_body <- function(req, max_bytes = 120 * 1024^2) {
  inp <- req$rook.input
  if (is.null(inp)) return(list())
  raw <- inp$read()
  if (length(raw) > max_bytes) stop("Request too large.", call. = FALSE)
  if (!length(raw)) return(list())
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  from_json(txt)
}

fn_exists <- function(name) exists(name, mode = "function", inherits = TRUE)

need_fn <- function(...) {
  miss <- c(...)[!vapply(c(...), fn_exists, TRUE)]
  if (length(miss)) err_resp(paste0("Not implemented yet (", paste(miss, collapse = ", "), ")."), 501L) else NULL
}

invoke_fn <- function(fn, args) {
  fmls <- names(formals(fn))
  if (is.null(fmls) || "..." %in% fmls) return(do.call(fn, args))
  do.call(fn, args[intersect(names(args), fmls)])
}

fn_pick <- function(...) {
  for (nm in c(...)) if (fn_exists(nm)) return(get(nm, mode = "function"))
  NULL
}

later_loop <- function(fn, every = 0.2) {
  tick <- function() { if (!isTRUE(fn())) later::later(tick, every) }
  later::later(tick, 0)
}

dualdag_child_env <- function() {
  e <- callr::rcmd_safe_env()
  cur <- Sys.getenv()
  n <- grep("^(PATH|HOME|USER|LOGNAME|LANG|LC_ALL|LC_CTYPE|TMPDIR|TZ|R_LIBS|R_LIBS_USER|R_LIBS_SITE|DUALDAG_[A-Z0-9_]+)$",
            names(cur), value = TRUE)
  e[n] <- cur[n]
  e
}

# Passed to callr. Sources R/ into the child, then calls fn_name. Do not call app helpers before that source.
bg_dispatch <- function(rdir, fn_name, args) {
  files <- list.files(rdir, pattern = "\\.R$", full.names = TRUE)
  utils <- files[grepl("utils\\.R$", files)]
  files <- c(utils, files[!files %in% utils])
  for (f in files) sys.source(f, envir = .GlobalEnv)
  if (is.null(args)) args <- list()
  do.call(get(fn_name, envir = .GlobalEnv, mode = "function"), args)
}

bg_promise <- function(fun, args, timeout = 300, tracker = NULL) {
  promises::promise(function(resolve, reject) {
    proc <- tryCatch(
      callr::r_bg(fun, args = args, supervise = TRUE, package = FALSE, env = dualdag_child_env()),
      error = function(e) e
    )
    if (inherits(proc, "error")) { reject(proc); return() }
    id <- random_token(4)
    if (!is.null(tracker)) tracker[[id]] <- proc
    t0 <- Sys.time()
    later_loop(function() {
      alive <- tryCatch(proc$is_alive(), error = function(e) FALSE)
      if (isTRUE(alive)) {
        if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) {
          try(if (!is.null(proc$kill_tree)) proc$kill_tree() else proc$kill(), silent = TRUE)
          if (!is.null(tracker)) tracker[[id]] <- NULL
          reject(simpleError("Timed out."))
          return(TRUE)
        }
        return(FALSE)
      }
      if (!is.null(tracker)) tracker[[id]] <- NULL
      r <- tryCatch(list(ok = proc$get_result()), error = function(e) list(err = e))
      if (!is.null(r$err)) {
        m <- conditionMessage(r$err)
        par <- r$err$parent
        if (!is.null(par) && nzchar(conditionMessage(par) %||% "")) m <- conditionMessage(par)
        reject(simpleError(m))
      } else resolve(r$ok)
      TRUE
    }, every = 0.15)
  })
}

normalize_generated <- function(x) {
  if (is.character(x)) return(list(code = paste(x, collapse = "\n"), same_source = isTRUE(attr(x, "same_source")), description = attr(x, "description"), approach = attr(x,"approach")))
  if (is.list(x)) return(list(code = paste(as.character(x$code %||% x$text %||% ""), collapse = "\n"), same_source = isTRUE(x$same_source), description = x$description, approach = x$approach))
  list(code = "", same_source = FALSE, description = NULL, approach = NULL)
}

with_cfg <- function() {
  cfg <- load_config()
  cfg$api_key <- get_api_key()$key
  cfg
}

job_catalog <- function(project_dir) {
  if (!exists("build_source_catalog", mode = "function")) return(NULL)
  tryCatch(build_source_catalog(project_dir), error = function(e) NULL)
}

llm_log_scope <- function(project_dir, task, node, side) {
  old <- getOption("dualdag.llm_log")
  options(dualdag.llm_log = list(
    dir = file.path(project_dir, ".dualdag", "llm_log"),
    task = task, node = node, side = side,
    session = if (exists("coder_session_for_node", mode="function")) coder_session_for_node(project_dir,node,side) else NULL
  ))
  old
}

extract_job <- function(project_dir, dataset) {
  old <- llm_log_scope(project_dir, "extract", dataset, "none")
  on.exit(options(dualdag.llm_log = old), add = TRUE)
  extract_dependencies(with_cfg(), read_project_spec_dir(project_dir), dataset,
                       catalog = job_catalog(project_dir), project = project_dir)
}

run_side_job <- function(project_dir, dataset, node_id, side, instruction, timeout, description = NULL) {
  if (exists("su_edited_path",mode="function")) unlink(su_edited_path(project_dir,dataset,node_id,side))
  session_complete <- FALSE
  on.exit(if (!session_complete && exists("coder_session_set_status",mode="function"))
    coder_session_set_status(project_dir,dataset,node_id,side,"failed"), add=TRUE)
  cfg <- with_cfg()
  review_enabled <- TRUE
  autofix_enabled <- TRUE
  spec <- read_project_spec_dir(project_dir)
  graph <- load_graph(project_dir, dataset)
  node <- graph_node(graph, node_id)
  if (is.null(node)) node <- default_node(node_id)
  if (isTRUE(graph$layered) && !is.null(node$paramcd)) {
    local_parents <- Filter(function(parent) {
      pn <- graph_node(graph,parent)
      !is.null(pn) && identical(toupper(pn$paramcd %||% ""),toupper(node$paramcd))
    },upstream_ids(graph,node_id))
    states <- state_nodes(project_dir,dataset)
    unfinished <- local_parents[!vapply(local_parents,function(parent)
      identical(as.character(states[[parent]]$level %||% ""),"L3"),TRUE)]
    if(length(unfinished)) {
      refusal <- paste0("Variable ",node_id," is blocked by unfinished upstream variable ",unfinished[[1]],".")
      if (exists("coder_window_event", mode="function")) coder_window_event(project_dir,dataset,node_id,side,"Run error",refusal)
      stop(refusal,call.=FALSE)
    }
    pg <- if (exists("param_graph_read", mode="function")) param_graph_read(project_dir, dataset) else NULL
    if (!is.null(pg)) {
      order <- param_layer_order(spec, dataset, pg)
      if (length(order$cycle)) stop("Parameter dependency cycle: ", paste(order$cycle, collapse = " -> "), call. = FALSE)
      blocked <- param_layer_block_message(project_dir, dataset, side, node$paramcd, spec, pg)
      if (!is.null(blocked)) stop(blocked, call. = FALSE)
    }
  }
  ups <- vapply(upstream_ids(graph, node_id), function(u) {
    hit <- split_node_id(u)
    if (is.null(hit)) u else hit$var
  }, "")
  notes <- if (exists("read_side_notes", mode = "function")) read_side_notes(project_dir, dataset, node_id, side) else list()
  sdir <- file.path(node_run_dir(project_dir, dataset, node_id), side)
  if (exists("coder_window_reset", mode = "function")) coder_window_reset(project_dir, dataset, node_id, side)
  if (exists("coder_window_event", mode = "function")) coder_window_event(project_dir, dataset, node_id, side, "Generation started")
  current <- read_text(file.path(sdir, "code.R"))
  old <- llm_log_scope(project_dir, "generate", node_id, side)
  on.exit(options(dualdag.llm_log = old), add = TRUE)
  gen <- tryCatch(
    generate_side(cfg, side, spec, graph, node, ups, user_instruction = instruction,
                  catalog = job_catalog(project_dir), project = project_dir,
                  current_code = current, notes = notes),
    error = function(e) e
  )
  if (inherits(gen, "error")) {
    stop(gen)
  }
  parsed <- normalize_generated(gen)
  if (isTRUE(graph$layered) && identical(node$kind,"param_variable")) write_json(list(approach=handling_approach_text(parsed$approach)),file.path(sdir,"approach.json"))
  dir.create(sdir, recursive = TRUE, showWarnings = FALSE)
  write_text(parsed$code, file.path(sdir, "code.R"))
  if (length(notes)) {
    last <- notes[[length(notes)]]
    write_json(list(index = length(notes), at = last$at %||% NULL, text = last$text %||% ""),
               file.path(sdir, "code_note.json"))
  }
  if (exists("coder_window_event", mode = "function")) coder_window_event(project_dir, dataset, node_id, side, "Program run started")
  unlink(file.path(sdir,"log.txt"))
  ran <- run_side(project_dir, dataset, node, side, parsed$code, timeout,
                  graph = graph, spec = spec, description = description %||% parsed$description)
  if (!is.list(ran)) ran <- list(ok = FALSE, error = "run_side returned nothing")
  if (exists("coder_window_event", mode = "function")) {
    coder_window_event(project_dir, dataset, node_id, side, "Program run finished")
    if (!isTRUE(ran$ok)) coder_window_event(project_dir, dataset, node_id, side, "Run error", ran$error %||% "Program failed")
  }
  if (!is.null(ran$log) && !file.exists(file.path(sdir, "log.txt")))
    write_text(paste(as.character(ran$log), collapse = "\n"), file.path(sdir, "log.txt"))
  if (exists("coder_session_snapshot_run", mode="function")) coder_session_snapshot_run(project_dir,dataset,node_id,side)
  review <- NULL
  auto_rewritten <- FALSE
  if (review_enabled) {
    call_review <- function(retry = FALSE, run_error = NULL) {
      if (exists("coder_window_event", mode = "function")) coder_window_event(project_dir, dataset, node_id, side, "Self-review started", if (retry) "retry" else NULL)
      progress <- review_progress_path(project_dir, dataset, node_id, side)
      write_text("reviewing", progress)
      on.exit(unlink(progress), add = TRUE)
      old_review <- llm_log_scope(project_dir, "self_review", node_id, side)
      on.exit(options(dualdag.llm_log = old_review), add = TRUE)
      result <- tryCatch(self_review(cfg, project_dir, dataset, node, side, spec, graph, instruction, retry, run_error),
               error = function(e) list(verdict = "uncertain", reasoning = paste("Review unavailable:", conditionMessage(e)),
                                        issues = list(), spec_question = NULL,
                                        code_hash = review_code_hash(project_dir, dataset, node_id, side), time = now_iso(), retried = FALSE))
      if (exists("coder_window_event", mode = "function")) coder_window_event(project_dir, dataset, node_id, side, "Self-review finished", result$verdict %||% "complete")
      result
    }
    if (!isTRUE(ran$ok) && isTRUE(ran$process_started) && autofix_enabled &&
        !grepl("DUALDAG_MISSING_DEPENDENCY:", as.character(ran$error %||% ""), fixed = TRUE) &&
        !grepl("DUALDAG_MISSING_COLUMN:", as.character(ran$error %||% ""), fixed = TRUE) &&
        !is_missing_r_package_error(ran$error) &&
        !grepl("disallowed package|not allowed", as.character(ran$error %||% ""), ignore.case = TRUE)) {
      auto_rewritten <- TRUE
      first_error <- as.character(ran$error %||% "Side failed")[1]
      error_review <- call_review(FALSE, first_error)
      if (exists("coder_window_event", mode = "function")) coder_window_event(project_dir, dataset, node_id, side, "Rewrite started", "program error with self-review")
      retry_gen <- tryCatch(generate_side(cfg, side, spec, graph, node, ups, user_instruction = instruction,
        catalog = job_catalog(project_dir), project = project_dir, current_code = parsed$code,
        notes = notes, self_review = list(code = parsed$code, approach=parsed$approach, issues = error_review$issues, error = first_error)), error = function(e) e)
      if (!inherits(retry_gen, "error")) {
        if (exists("coder_window_event", mode = "function")) coder_window_event(project_dir, dataset, node_id, side, "Rewrite finished")
        retry_parsed <- normalize_generated(retry_gen)
        if (isTRUE(graph$layered) && identical(node$kind,"param_variable")) write_json(list(approach=handling_approach_text(retry_parsed$approach)),file.path(sdir,"approach.json"))
        write_text(retry_parsed$code, file.path(sdir, "code.R"))
        if (exists("coder_window_event", mode = "function")) coder_window_event(project_dir, dataset, node_id, side, "Program run started", "rewrite")
        unlink(file.path(sdir,"log.txt"))
        retry_run <- tryCatch(run_side(project_dir, dataset, node, side, retry_parsed$code, timeout,
          graph = graph, spec = spec, description = description %||% retry_parsed$description),
          error = function(e) list(ok = FALSE, error = conditionMessage(e)))
        if (!is.list(retry_run)) retry_run <- list(ok = FALSE, error = "run_side returned nothing")
        if (exists("coder_window_event", mode = "function")) {
          coder_window_event(project_dir, dataset, node_id, side, "Program run finished", "rewrite")
          if (!isTRUE(retry_run$ok)) coder_window_event(project_dir, dataset, node_id, side, "Run error", retry_run$error %||% "Program failed")
        }
        if (!is.null(retry_run$log) && !file.exists(file.path(sdir, "log.txt")))
          write_text(paste(as.character(retry_run$log), collapse = "\n"), file.path(sdir, "log.txt"))
        if (exists("coder_session_snapshot_run", mode="function")) coder_session_snapshot_run(project_dir,dataset,node_id,side)
        review <- error_review
        review$retried <- TRUE
        review$history <- list(list(error = first_error))
        if (isTRUE(retry_run$ok)) {
          parsed <- retry_parsed; ran <- retry_run
          review <- call_review(TRUE)
          review$retried <- TRUE
          review$history <- list(list(error = first_error))
        } else {
          second_error <- as.character(retry_run$error %||% "Side failed")[1]
          ran <- retry_run
          ran$error <- second_error
          review$verdict <- "unexpected"
          review$reasoning <- paste("The rewritten program also failed.", second_error)
          review$issues <- list()
          review$code_hash <- review_code_hash(project_dir, dataset, node_id, side)
          review$history <- list(list(error = first_error), list(error = second_error))
        }
      } else {
        review <- error_review
        review$retried <- TRUE
        review$history <- list(list(error = first_error), list(error = conditionMessage(retry_gen)))
      }
      review_write(project_dir, dataset, node_id, side, review)
    }
    if (isTRUE(ran$ok) && !auto_rewritten) {
    review <- call_review(FALSE)
    if (identical(review$verdict, "unexpected") && autofix_enabled) {
      first_review <- review
      backup <- tempfile("self-review-"); dir.create(backup)
      side_files <- list.files(sdir, all.files = TRUE, full.names = TRUE, no.. = TRUE)
      for (f in side_files) file.copy(f, backup, recursive = TRUE)
      wp <- work_path(project_dir, dataset, side); work_backup <- tempfile("self-review-work-")
      had_work <- file.exists(wp); if (had_work) file.copy(wp, work_backup)
      if (exists("coder_window_event", mode = "function")) coder_window_event(project_dir, dataset, node_id, side, "Rewrite started", "self-review")
      retry_gen <- tryCatch(generate_side(cfg, side, spec, graph, node, ups, user_instruction = instruction,
        catalog = job_catalog(project_dir), project = project_dir, current_code = parsed$code,
        notes = notes, self_review = list(code = parsed$code, approach=parsed$approach, issues = review$issues)), error = function(e) e)
      retry_run <- NULL
      if (!inherits(retry_gen, "error")) {
        if (exists("coder_window_event", mode = "function")) coder_window_event(project_dir, dataset, node_id, side, "Rewrite finished", "self-review")
        retry_parsed <- normalize_generated(retry_gen)
        if (isTRUE(graph$layered) && identical(node$kind,"param_variable")) write_json(list(approach=handling_approach_text(retry_parsed$approach)),file.path(sdir,"approach.json"))
        write_text(retry_parsed$code, file.path(sdir, "code.R"))
        if (exists("coder_window_event", mode = "function")) coder_window_event(project_dir, dataset, node_id, side, "Program run started", "rewrite")
        unlink(file.path(sdir,"log.txt"))
        retry_run <- tryCatch(run_side(project_dir, dataset, node, side, retry_parsed$code, timeout,
          graph = graph, spec = spec, description = description %||% retry_parsed$description), error = function(e) list(ok = FALSE, error = conditionMessage(e)))
        if (is.list(retry_run) && !is.null(retry_run$log) && !file.exists(file.path(sdir,"log.txt"))) write_text(paste(as.character(retry_run$log),collapse="\n"),file.path(sdir,"log.txt"))
        if (exists("coder_session_snapshot_run", mode="function")) coder_session_snapshot_run(project_dir,dataset,node_id,side)
        if (exists("coder_window_event", mode = "function")) {
          coder_window_event(project_dir, dataset, node_id, side, "Program run finished", "rewrite")
          if (!isTRUE(retry_run$ok)) coder_window_event(project_dir, dataset, node_id, side, "Run error", retry_run$error %||% "Program failed")
        }
        if (isTRUE(retry_run$ok)) {
          parsed <- retry_parsed; ran <- retry_run
          review <- call_review(TRUE)
          review$retried <- TRUE
          review$history <- list(first_review)
        }
      }
      if (!isTRUE(retry_run$ok)) {
        # Restore the first successful run if the retry failed or did not run.
        present <- list.files(sdir, all.files = TRUE, full.names = TRUE, no.. = TRUE)
        unlink(present[!basename(present) %in% c("coder_context.json", "coder_process.json")], recursive = TRUE)
        saved <- list.files(backup, all.files = TRUE, full.names = TRUE, no.. = TRUE)
        for (f in saved) if (!basename(f) %in% c("coder_context.json", "coder_process.json")) file.copy(f, sdir, recursive = TRUE)
        if (had_work) file.copy(work_backup, wp) else unlink(wp)
        review <- first_review
        review$retried <- TRUE; review$history <- list(first_review)
      }
      unlink(backup, recursive = TRUE); unlink(work_backup)
    }
    review_write(project_dir, dataset, node_id, side, review)
    }
  }
  if (exists("coder_window_event", mode = "function")) {
    coder_window_event(project_dir, dataset, node_id, side, "Final result", if (isTRUE(ran$ok)) "success" else "failed")
    coder_window_event(project_dir, dataset, node_id, side, "Run log", read_text(file.path(sdir, "log.txt")))
  }
  if (exists("coder_session_set_status", mode="function")) coder_session_set_status(project_dir,dataset,node_id,side,if(isTRUE(ran$ok)) "success" else "failed")
  session_complete <- TRUE
  list(code = parsed$code, same_source = isTRUE(parsed$same_source), run = ran, review = review)
}

agent_job <- function(project_dir, dataset, node_id, side, message, timeout) {
  if (exists("coder_session_ensure", mode="function")) coder_session_ensure(project_dir,dataset,node_id,side,load_config()$model %||% NULL)
  old <- llm_log_scope(project_dir, "chat", node_id, side)
  on.exit(options(dualdag.llm_log = old), add = TRUE)
  decision <- agent_decide(with_cfg(), project_dir, dataset, node_id, side, message,
                           catalog = job_catalog(project_dir))
  if (is.null(decision$change)) return(c(decision, list(run = NULL, note_index = NULL)))
  instruction <- decision$change$instruction
  notes <- append_side_note(project_dir, dataset, node_id, side, instruction)
  generated <- tryCatch(run_side_job(project_dir, dataset, node_id, side, instruction, timeout,
                                     description = decision$change$description),
                        error = function(e) e)
  run <- if (inherits(generated, "error")) list(ok = FALSE, error = conditionMessage(generated)) else generated
  c(decision, list(run = run, note_index = length(notes)))
}

dag_ask_job <- function(project_dir, dataset, message, paramcd=NULL, selected=NULL, parameter_graph=FALSE) {
  old <- llm_log_scope(project_dir, "dag_ask", dataset, "dag")
  on.exit(options(dualdag.llm_log = old), add = TRUE)
  prior_tool_notice <- getOption("dualdag.cli_tool_notice")
  options(dualdag.cli_tool_notice = "You have no tools of your own. The app applies the changes listed in your JSON. Return only one JSON object.")
  on.exit(options(dualdag.cli_tool_notice = prior_tool_notice), add = TRUE)
  dag_ask(with_cfg(), project_dir, dataset, message,paramcd,selected,parameter_graph)
}

compare_job <- function(project_dir, dataset, node_id) {
  spec <- read_project_spec_dir(project_dir)
  node <- graph_node(load_graph(project_dir, dataset), node_id)
  if (is.null(node)) node <- default_node(node_id)
  compare_node(project_dir, dataset, node, dataset_keys(spec, dataset))
}

qc_job <- function(project_dir, dataset, node_id) {
  old <- llm_log_scope(project_dir, "qc", node_id, "none")
  on.exit(options(dualdag.llm_log = old), add = TRUE)
  cfg <- with_cfg()
  spec <- read_project_spec_dir(project_dir)
  graph <- load_graph(project_dir, dataset)
  node <- graph_node(graph, node_id)
  if (is.null(node)) node <- default_node(node_id)
  cmp <- read_json(file.path(node_run_dir(project_dir, dataset, node_id), "compare.json"))
  diffs <- if (is.list(cmp)) cmp$diffs else NULL
  if (is.list(diffs) && !is.data.frame(diffs)) {
    diffs <- tryCatch(do.call(rbind, lapply(diffs, as.data.frame, stringsAsFactors = FALSE)), error = function(e) NULL)
  }
  rows <- NULL
  parsed <- split_node_id(node_id)
  if (!is.null(spec) && !is.null(parsed) && nrow(spec$variables)) {
    v <- spec$variables
    rows <- v[v$Dataset == dataset & v$Variable == parsed$var, , drop = FALSE]
  }
  qc_investigate(cfg, dataset = dataset, node = node, diffs = diffs, spec_rows = rows, spec = spec)
}

export_job <- function(project_dir, dataset, format) {
  export_dataset(project_dir, dataset, read_project_spec_dir(project_dir), format)
}

llm_test_job <- function() {
  cfg <- with_cfg()
  if (exists("llm_test", mode = "function")) return(llm_test(cfg))
  r <- invoke_fn(llm_chat, list(
    cfg = cfg, system = "You are dualdag connectivity check.",
    user = "Reply with the single word ok.", max_tokens = 32L, json = FALSE
  ))
  list(ok = TRUE, model = r$model %||% cfg$model, reply = r$text %||% "")
}

bg_ping <- function(rdir) {
  files <- list.files(rdir, pattern = "\\.R$", full.names = TRUE)
  utils <- files[grepl("utils\\.R$", files)]
  files <- c(utils, files[!files %in% utils])
  for (f in files) sys.source(f, envir = .GlobalEnv)
  if (!exists("load_config", envir = .GlobalEnv, mode = "function")) stop("load_config missing", call. = FALSE)
  "dualdag"
}

create_app <- function(app_dir, project_dir, port, token, cfg) {
  A <- new.env(parent = emptyenv())
  A$app_dir <- normalizePath(app_dir, winslash = "/", mustWork = TRUE)
  A$run_r_hash <- file_md5(file.path(A$app_dir,"run.R"))
  A$www <- file.path(A$app_dir, "www")
  A$rdir <- file.path(A$app_dir, "R")
  A$token <- token
  A$port <- as.integer(port)
  A$cfg <- cfg
  A$project <- open_project(project_dir)
  if (exists("coder_session_prune", mode="function")) coder_session_prune(A$project$dir)
  if (fn_exists("recompare_stored")) try(recompare_stored(A$project), silent = TRUE)
  A$jobs <- list()
  A$procs <- new.env(parent = emptyenv())
  A$inflight <- new.env(parent = emptyenv())
  A$batch <- NULL
  A$admin_active <- FALSE
  A$admin_stop <- FALSE
  A$admin_procs <- new.env(parent=emptyenv())
  A$admin_turn <- NULL
  A$admin_cli_status_cache <- NULL
  A$admin_session <- if (fn_exists("admin_session_path")) read_json(admin_session_path(A$project$dir)) %||% list(provider = "", session_id = "", turns = list()) else list(provider = "", session_id = "", turns = list())
  A$runtime_env <- NULL
  A$allowed_hosts <- c(sprintf("127.0.0.1:%d", A$port), sprintf("localhost:%d", A$port))
  A$allowed_origins <- paste0("http://", A$allowed_hosts)
  A
}

set_port <- function(A, port) {
  A$port <- as.integer(port)
  A$allowed_hosts <- c(sprintf("127.0.0.1:%d", A$port), sprintf("localhost:%d", A$port))
  A$allowed_origins <- paste0("http://", A$allowed_hosts)
  A
}

check_request <- function(A, req, token) {
  host <- req$HTTP_HOST %||% ""
  if (!host %in% A$allowed_hosts) return("bad host")
  origin <- req$HTTP_ORIGIN %||% ""
  if (nzchar(origin) && !origin %in% A$allowed_origins) return("bad origin")
  if (!is.character(token) || length(token) != 1 || !identical(token, A$token)) return("bad token")
  NULL
}

# Jobs that call the model carry the provider/model label for the header.
job_llm_kinds <- c("extract", "param_extract", "param_ask", "config-test", "generate", "investigate", "ask")

job_start <- function(A, kind, dataset = NULL, node = NULL) {
  id <- random_token(4)
  model <- if (kind %in% job_llm_kinds) {
    if (identical(Sys.getenv("DUALDAG_FAKE_LLM"), "1")) "fake-llm"
    else paste0(A$cfg$provider %||% "", " / ", A$cfg$model %||% "")
  } else NULL
  A$jobs[[id]] <- list(id = id, kind = kind, dataset = dataset, node = node,
                       started = as.numeric(Sys.time()), model = model)
  id
}
# `error` (a condition or message) is kept as last_error so the header can show it.
job_finish <- function(A, id, error = NULL) {
  j <- A$jobs[[id]]
  if (!is.null(error) && !is.null(j)) {
    A$last_error <- list(kind = j$kind, dataset = j$dataset, node = j$node, model = j$model,
                         message = if (inherits(error, "condition")) conditionMessage(error) else as.character(error)[1],
                         time = as.numeric(Sys.time()))
  }
  A$jobs[[id]] <- NULL
  if (exists(id, envir = A$inflight, inherits = FALSE)) rm(list = id, envir = A$inflight)
}

node_busy <- function(A, ds, id) {
  any(vapply(A$jobs, function(j) identical(j$dataset, ds) && identical(j$node, id), TRUE))
}

llm_limit <- function(A, extra = 30) llm_timeout(A$cfg) + extra
# Extraction makes one call per batch per parameter, so one call's limit is too short for the job.
extract_limit <- function(A, ds, layers = NULL) {
  spec <- tryCatch(read_project_spec(A$project), error = function(e) NULL)
  n_rows <- if (is.null(spec)) 100L else nrow(spec_vars(spec, ds)) + nrow(spec_params(spec, ds))
  n_layers <- layers %||% (if (is.null(spec)) 1L else max(1L, nrow(spec_params(spec, ds))))
  llm_limit(A) + llm_timeout(A$cfg) * (3L + n_layers * ceiling(n_rows / 4))
}
run_limit <- function(A) as.numeric(A$cfg$timeout_run_s %||% 600)

launch <- function(A, fn_name, args, timeout, tracker = A$procs) {
  bg_promise(bg_dispatch, list(rdir = A$rdir, fn_name = fn_name, args = args), timeout = timeout, tracker = tracker)
}

serve_static <- function(A, path) {
  if (path == "/" || path == "") path <- "/index.html"
  if (!dir.exists(A$www)) return(list(status = 404L, headers = list(`Content-Type` = "text/plain; charset=utf-8"), body = "Not found"))
  rel <- sub("^/+", "", path)
  f <- tryCatch(safe_path(A$www, rel, must_exist = TRUE), error = function(e) NULL)
  if (is.null(f) || dir.exists(f)) return(list(status = 404L, headers = list(`Content-Type` = "text/plain; charset=utf-8"), body = "Not found"))
  ext <- tolower(tools::file_ext(f))
  h <- list(
    `Content-Type` = if (ext %in% names(MIME)) unname(MIME[ext]) else "application/octet-stream",
    `Cache-Control` = "no-cache", `X-Content-Type-Options` = "nosniff", `Referrer-Policy` = "no-referrer"
  )
  if (ext == "html") {
    h$`Content-Security-Policy` <- "default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline'; img-src 'self' data:; connect-src 'self'; frame-ancestors 'none'; base-uri 'none'; form-action 'none'"
    h$`X-Frame-Options` <- "DENY"
  }
  list(status = 200L, headers = h, body = read_file_raw(f))
}

as_issue_list <- function(x) {
  if (is.null(x)) return(list())
  if (is.data.frame(x)) {
    if (!nrow(x)) return(list())
    return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  }
  if (is.list(x) && !is.null(x$type) && !is.null(x$message)) return(list(x))
  if (is.list(x)) return(unname(x))
  list()
}

# Missing severity counts as an error. An orphan with no severity is a warning and does not block generation.
issue_blocks <- function(iss) {
  if (!is.list(iss)) return(FALSE)
  sev <- tolower(trimws(as.character(iss$severity %||% "")[1]))
  if (!length(sev) || is.na(sev) || !nzchar(sev)) {
    sev <- if (identical(as.character(iss$type %||% "")[1], "orphan")) "warning" else "error"
  }
  identical(sev, "error")
}

node_is_blocked <- function(id, ds, issues) {
  if (!length(issues)) return(FALSE)
  if (fn_exists("node_blocked")) {
    parsed <- split_node_id(id)
    var <- if (is.null(parsed)) "" else parsed$var
    return(isTRUE(node_blocked(id, var, ds, issues)))
  }
  any(vapply(issues_for_node(issues, ds, id), issue_blocks, logical(1)))
}

node_actions <- function(st) {
  lvl <- st$level %||% "none"
  prob <- st$problem
  acts <- "generate"
  if (identical(lvl, "L2")) acts <- c(acts, "compare")
  if (!is.null(prob) && identical(prob$stage, "compare")) acts <- c(acts, "investigate")
  unique(acts)
}

node_ready <- function(st, acts) {
  !identical(st$level %||% "none", "L3") && length(acts) > 0
}

node_public <- function(p, graph, ds, id, check_issues) {
  st <- get_status(p, ds, id)
  acts <- node_actions(st)
  if (fn_exists("actions")) {
    probe <- list(id = id, level = st$level %||% "none", problem = st$problem)
    alt <- tryCatch(actions(probe), error = function(e) NULL)
    if (is.character(alt) || is.list(alt)) acts <- unique(as.character(unlist(alt, use.names = FALSE)))
  }
  st$actions <- as.list(acts)
  st$ready <- node_ready(st, acts)
  st$has_qc_report <- file.exists(file.path(node_run_dir(p, ds, id), "qc_report.md"))
  st
}

group_public <- function(p, ds, group) {
  members <- as.character(unlist(group$members %||% list(), use.names = FALSE))
  members <- members[nzchar(members)]
  if (!length(members)) return(list(level = "none", problem = NULL))
  sts <- lapply(members, function(id) get_status(p, ds, id))
  ranks <- vapply(sts, function(s) match(s$level %||% "none", c("none", "L2", "L3")), 1L)
  prob <- NULL
  for (s in sts) if (!is.null(s$problem)) { prob <- s$problem; break }
  list(level = c("none", "L2", "L3")[min(ranks)], problem = prob)
}

check_inputs <- function(p) {
  root <- project_dir_of(p)
  cat <- if (fn_exists("build_source_catalog")) tryCatch(build_source_catalog(root), error = function(e) NULL) else NULL
  di <- if (!is.null(cat) && fn_exists("catalog_index")) catalog_index(cat) else NULL
  if (is.null(di)) {
    di <- tryCatch(if (fn_exists("data_index")) data_index(root) else local_data_index(p),
                   error = function(e) local_data_index(p))
  }
  list(catalog = cat, index = di)
}

run_check <- function(p, spec, graph) {
  if (is.null(graph)) return(NULL)
  if (!fn_exists("check_graph")) return(list(issues = as_issue_list(graph$unresolved), order = NULL))
  inp <- check_inputs(p)
  chk <- tryCatch(check_graph(graph, spec, inp$index, catalog = inp$catalog), error = function(e) {
    list(issues = list(make_issue("structure", conditionMessage(e), dataset = graph$dataset %||% NULL)), order = NULL)
  })
  if (!is.list(chk)) chk <- list(issues = list(), order = NULL)
  list(issues = as_issue_list(chk$issues), order = chk$order)
}

state_payload <- function(A) {
  p <- A$project
  tryCatch(sync_external(p), error = function(e) NULL)
  spec <- tryCatch(read_project_spec(p), error = function(e) NULL)
  data <- tryCatch(list_data_info(p), error = function(e) list())
  uploaded <- if (length(data)) toupper(vapply(data, function(d) as.character(d$domain %||% ""), "")) else character()
  gfiles <- list.files(p$state_dir, pattern = "^graph_.*\\.json$")
  gds <- sub("^graph_", "", sub("\\.json$", "", gfiles))
  ds_names <- unique(c(if (!is.null(spec)) spec$datasets else character(), gds))
  issues <- if (!is.null(spec)) spec$problems else list()
  datasets <- empty_named()
  missing_data <- empty_named()
  for (ds in ds_names) {
    g <- tryCatch(load_graph(p, ds), error = function(e) NULL)
    if (!is.null(g)) for (i in seq_along(g$nodes)) {
      n <- g$nodes[[i]]
      if (length(n$categories %||% list())) { g$nodes[[i]]$categorical <- TRUE; next }
      id <- as.character(n$id %||% "")
      var <- as.character(n$var %||% "")
      if (!nzchar(id) || !nzchar(var)) next
      for (side in c("primary", "validation")) {
        file <- file.path(node_run_dir(p, ds, id), side, "out.rds")
        data <- if (file.exists(file)) tryCatch(readRDS(file), error = function(e) NULL) else NULL
        if (is.data.frame(data) && var %in% names(data) && !is.null(node_value_counts(data[[var]]))) {
          g$nodes[[i]]$categorical <- TRUE
          break
        }
      }
    }
    chk <- tryCatch(run_check(p, spec, g), error = function(e) list(issues = list(), order = NULL))
    ids <- dataset_node_ids(p, spec, ds)
    nodes <- empty_named()
    check_issues <- if (is.null(chk)) list() else chk$issues %||% list()
    for (id in ids) nodes[[id]] <- node_public(p, g, ds, id, check_issues)
    groups <- empty_named()
    if (!is.null(g)) for (grp in g$groups %||% list()) {
      if (!is.list(grp)) next
      gid <- as.character(grp$id %||% "")
      if (!nzchar(gid)) next
      groups[[gid]] <- group_public(p, ds, grp)
    }
    ord <- if (is.null(chk) || is.null(chk$order)) NULL else as.list(chk$order)
    dag_pub <- if (fn_exists("dag_agent_public")) {
      tryCatch(dag_agent_public(p, ds), error = function(e) list(turns = list(), corrections = list()))
    } else list(turns = list(), corrections = list())
    dag_pub$manual_edit_count <- length(dag_pub$corrections %||% list())
    param_mem <- tryCatch(dag_agent_read(p, ds), error = function(e) list())
    key_overrides <- if (exists("layer_keys_read", mode="function")) layer_keys_read(p,ds) else list()
    param_manual_edit_count <- length(param_mem$param_corrections %||% list()) + length(key_overrides)
    dag_undo <- if (fn_exists("dag_undo_public")) tryCatch(dag_undo_public(p, ds), error = function(e) list(count = 0L, label = NULL)) else list(count = 0L, label = NULL)
    if (!is.null(g) && exists("extract_reply_read", mode="function")) {
      saved_extract <- tryCatch(extract_reply_read(p,ds),error=function(e)NULL)
      if (!is.null(saved_extract$parameter_classification))
        g$extraction <- list(parameter_classification=saved_extract$parameter_classification)
    }
    if (!is.null(g) && isTRUE(g$layered)) {
      carries <- vapply(Filter(function(x) identical(x$classification,"carry_over"), as_records(g$extraction$parameter_classification$parameters)), function(x) toupper(x$paramcd %||% ""), "")
      layer_codes <- vapply(as_records(g$layers), function(x) toupper(x$paramcd %||% ""), "")
      g$carry_over_regenerate_required <- length(carries) > 0L && !"CARRY_OVER" %in% layer_codes && any(carries %in% layer_codes)
    }
    datasets[[ds]] <- list(graph = g, reextract_required = dataset_reextract_required(spec,ds,g), layout = load_layout(p, ds), check = if (is.null(g)) NULL else list(issues = check_issues, order = ord),
                           nodes = nodes, groups = groups, dag = dag_pub,
                           dag_undo = dag_undo,
                           parameter_graph = list(manual_edit_count = param_manual_edit_count))
    if (!is.null(chk)) issues <- c(issues, check_issues)
    if (!is.null(g)) {
      doms <- unique(vapply(g$external %||% list(), function(e) if (is.list(e)) toupper(as.character(e$domain %||% "")) else "", ""))
      doms <- doms[nzchar(doms) & !doms %in% uploaded]
      if (length(doms)) missing_data[[ds]] <- as.list(doms)
    }
  }
  if (!is.null(p$state$errors$spec)) {
    issues <- c(issues, list(make_issue("structure", p$state$errors$spec, file = "variable.csv")))
  }
  busy <- NULL
  if (length(A$jobs)) busy <- unname(lapply(A$jobs, function(j) list(
    kind = j$kind, dataset = j$dataset, node = j$node, model = j$model,
    elapsed = round(as.numeric(Sys.time()) - (j$started %||% as.numeric(Sys.time()))))))
  cat <- if (fn_exists("build_source_catalog")) tryCatch(build_source_catalog(p$dir), error = function(e) NULL) else NULL
  summary <- if (!is.null(cat) && fn_exists("catalog_summary")) catalog_summary(cat) else empty_named()
  list(
    project = p$dir,
    spec = spec_public(spec),
    data = data,
    missing_data = missing_data,
    datasets = datasets,
    issues = issues,
    config_public = config_public(A$cfg),
    busy = busy,
    last_error = A$last_error,
    catalog_summary = summary,
    project_settings = if (fn_exists("compare_settings")) compare_settings(p) else list(blank_equals_na = TRUE),
    dataset_types = if (fn_exists("dataset_types_public")) dataset_types_public(p, spec) else list(),
    batch = batch_public(A$batch, A$project$dir)
  )
}

# Adapt the shared lineage-level count to the categories endpoint shape.
node_value_counts <- function(x, max_levels = 20L) {
  if (!(is.character(x) || is.factor(x) || is.logical(x)) ||
      inherits(x, c("Date", "POSIXt"))) return(NULL)
  levels <- distinct_levels(x, max_levels = max_levels)
  overflow <- isTRUE(attr(levels, "too_many"))
  counts <- if (overflow) list() else lapply(seq_len(nrow(levels)), function(i)
    list(value = levels$value[[i]], n = levels$n[[i]]))
  list(overflow = overflow, counts = counts)
}

node_categories <- function(p, ds, id) {
  graph <- load_graph(p, ds)
  if (is.null(graph)) stop("No graph.", call. = FALSE)
  hit <- Filter(function(n) identical(as.character(n$id %||% ""), id), graph$nodes)
  if (!length(hit)) stop("Unknown node.", call. = FALSE)
  spec <- hit[[1]]$categories %||% list()
  if (is.list(spec) && !is.null(spec$value)) spec <- list(spec)
  if (!is.list(spec)) spec <- list()
  spec <- lapply(spec, function(c) list(value = as.character(c$value %||% ""), rule = as.character(c$rule %||% "")))
  var <- split_node_id(id)$var
  sides <- list()
  for (side in c("primary", "validation")) {
    file <- file.path(node_run_dir(p, ds, id), side, "out.rds")
    data <- if (file.exists(file)) tryCatch(readRDS(file), error = function(e) NULL) else NULL
    sides[[side]] <- if (is.data.frame(data) && var %in% names(data)) node_value_counts(data[[var]]) else NULL
  }
  side_values <- function(side) vapply(side$counts %||% list(), function(c) c$value, "")
  side_count <- function(side, value) {
    hits <- Filter(function(c) identical(c$value, value), side$counts %||% list())
    if (length(hits)) hits[[1]]$n else NULL
  }
  values <- unique(c(vapply(spec, function(c) c$value, ""),
                     side_values(sides$primary), side_values(sides$validation)))
  rows <- lapply(values, function(value) {
    rules <- vapply(Filter(function(c) identical(c$value, value), spec), function(c) c$rule, "")
    p_count <- side_count(sides$primary, value)
    v_count <- side_count(sides$validation, value)
    in_spec <- value %in% vapply(spec, function(c) c$value, "")
    list(value = value, rule = if (length(rules)) rules[[1]] else "",
         primary = p_count, validation = v_count, in_spec = in_spec,
         mismatch = !in_spec || xor(!is.null(p_count), !is.null(v_count)))
  })
  overflow <- isTRUE(sides$primary$overflow) || isTRUE(sides$validation$overflow)
  list(categorical = length(spec) > 0L || overflow || length(values) > 0L,
       overflow = overflow, rows = if (overflow) list() else rows)
}

remember <- function(A, id, pr) {
  A$inflight[[id]] <- pr
  pr
}

store_extracted <- function(A, ds, graph) {
  p <- A$project
  sp <- tryCatch(read_project_spec(p), error = function(e) NULL)
  graph <- coerce_graph(graph, ds, if (is.list(sp)) sp$hash else NULL)
  if (!is.null(graph$extraction) && exists("extract_reply_write",mode="function")) {
    extract_reply_write(p,ds,graph$extraction)
    graph$extraction <- NULL
  }
  save_graph(p, graph)
  if (exists("dag_undo_path", mode = "function")) unlink(dag_undo_path(p, ds))
  mem <- dag_agent_read(p, ds)
  mem$corrections <- list()
  dag_agent_write(p, ds, mem)
  note_graph_nodes(p, ds, graph)
  if (!is.null(p$state$errors)) p$state$errors[[ds]] <- NULL
  write_state(p)
  graph
}

start_extract <- function(A, ds, wait) {
  spec <- tryCatch(read_project_spec(A$project), error=function(e)NULL)
  if (!is.null(spec) && dataset_type_get(A$project, ds, spec) %in% c("BDS", "BDS-TTE")) {
    if (fn_exists("start_param_extract")) return(start_param_extract(A, ds, wait=wait))
  }
  id <- job_start(A, "extract", ds, NULL)
  pr <- launch(A, "extract_job", list(project_dir = A$project$dir, dataset = ds), extract_limit(A, ds))
  done <- promises::then(
    pr,
    onFulfilled = function(graph) {
      job_finish(A, id)
      out <- tryCatch(store_extracted(A, ds, graph), error = function(e) e)
      if (inherits(out, "error")) return(err_resp(conditionMessage(out), 500L))
      json_resp(out)
    },
    onRejected = function(e) {
      job_finish(A, id, e)
      A$project$state$errors[[ds]] <- conditionMessage(e)
      try(write_state(A$project), silent = TRUE)
      err_resp(conditionMessage(e), 502L)
    }
  )
  remember(A, id, done)
  if (wait) done else invisible(done)
}

start_layer_extract <- function(A, ds, paramcd, wait=TRUE) {
  ds <- safe_ds(ds); pc <- toupper(trimws(as.character(paramcd)[1]))
  if (any(vapply(A$jobs, function(j) identical(j$kind, "extract") && identical(j$dataset, ds), TRUE)))
    return(err_resp("An extraction job for this dataset is already running.", 409L))
  spec <- read_project_spec(A$project)
  pg <- param_graph_read(A$project, ds)
  if (is.null(pg) || !length(pg$nodes)) return(err_resp("Generate the Parameter DAG first.", 409L))
  current <- load_graph(A$project, ds)
  layer_record <- Filter(function(x) identical(toupper(x$paramcd %||% ""),pc), as_records(current$layers %||% list()))
  if (!identical(pc,"CARRY_OVER") && !pc %in% toupper(vapply(as_records(pg$nodes), function(n) as.character(n$code %||% ""), "")) && !length(layer_record)) return(err_resp("Unknown parameter.", 400L))
  classification <- tryCatch(extract_classification(A$cfg, spec, ds, A$project$dir), error=function(e) e)
  if (inherits(classification, "error")) return(err_resp(conditionMessage(classification), 500L))
  carry_group <- toupper(vapply(Filter(function(x) identical(x$classification,"carry_over"), classification$parameters %||% list()), function(x) x$paramcd, ""))
  if (is.null(current) || !isTRUE(current$layered) || !pc %in% vapply(current$layers %||% list(),function(x)x$paramcd,""))
    current <- graph_skeleton(spec, ds, layer_keys_read(A$project,ds), carry_group)
  layer_record <- Filter(function(x) identical(toupper(x$paramcd %||% ""),pc), as_records(current$layers %||% list()))
  is_group <- identical(pc,"CARRY_OVER")
  members <- if (is_group) carry_group else if (length(layer_record)) toupper(flat_chr(layer_record[[1]]$members_parameters)) else pc
  layer_codes <- if (is_group) c("CARRY_OVER", carry_group) else if (length(layer_record)) pc else members
  if (is.null(current) || !isTRUE(current$layered) || (identical(pc,"CARRY_OVER") && !"CARRY_OVER" %in% vapply(current$layers %||% list(),function(x)x$paramcd,""))) current <- graph_skeleton(spec, ds, carry_members=carry_group)
  descendants <- members
  queue <- members
  while (length(queue)) {
    here <- queue[[1L]]; queue <- queue[-1L]
    children <- vapply(Filter(function(e) toupper(e$from) == here, pg$edges %||% list()), function(e) toupper(e$to), "")
    fresh <- setdiff(children, descendants); descendants <- c(descendants, fresh); queue <- c(queue, fresh)
  }
  if (is_group) descendants <- unique(c("CARRY_OVER", descendants))
  was_generated <- toupper(flat_chr(current$generated_parameters))
  include_dataset_level <- !length(was_generated)
  reset_results <- if (is_group) any(descendants %in% was_generated) else pc %in% was_generated
  if (reset_results) {
    affected <- Filter(function(n) !is.null(n$paramcd) && toupper(n$paramcd) %in% descendants, current$nodes %||% list())
    for (n in affected) { clear_node_results(A$project, ds, n$id); put_status(A$project, ds, n$id, blank_status(), save=FALSE) }
    if (is_group) {
      current$generated_parameters <- as.list(setdiff(was_generated, descendants))
      save_graph(A$project, current)
    }
    write_state(A$project)
  }
  id <- job_start(A, "extract", ds, paste(members, collapse=","))
  promise <- launch(A, "layer_extract_job", list(project_dir=A$project$dir, dataset=ds, paramcd=pc), extract_limit(A, ds, 1L))
  done <- promises::then(promise,
    onFulfilled=function(graph) {
      job_finish(A,id)
      latest <- load_graph(A$project, ds)
      if (is.null(latest) || !isTRUE(latest$layered) || (identical(pc,"CARRY_OVER") && !"CARRY_OVER" %in% vapply(latest$layers %||% list(),function(x)x$paramcd,""))) latest <- graph_skeleton(spec, ds, carry_members=carry_group)
      param_node <- function(n) !is.null(n$paramcd) && toupper(n$paramcd) %in% layer_codes
      reply_nodes <- Filter(param_node, graph$nodes %||% list())
      latest$nodes <- lapply(latest$nodes %||% list(), function(n) {
        hit <- Filter(function(r) identical(r$id, n$id), reply_nodes)
        if (length(hit)) for (field in intersect(c("key", "keys", "key_vars", "key_fields"), names(hit[[1]]))) n[[field]] <- hit[[1]][[field]]
        n
      })
      latest$edges <- c(Filter(function(e) !param_node(graph_node(latest, e$to)), latest$edges %||% list()),
                        Filter(function(e) param_node(graph_node(graph, e$to)), graph$edges %||% list()))
      if (include_dataset_level) {
        dataset_ids <- vapply(Filter(function(n) identical(n$kind, "dataset_variable"), latest$nodes %||% list()), `[[`, "", "id")
        latest$edges <- c(Filter(function(e) !e$to %in% dataset_ids, latest$edges),
                          Filter(function(e) e$to %in% dataset_ids, graph$edges %||% list()))
      }
      usage_kind <- function(id, nodes) {
        n <- graph_node(list(nodes=nodes), id)
        if (is.null(n)) return("other")
        if (!is.null(n$paramcd) && toupper(n$paramcd) %in% layer_codes) return("parameter")
        if (identical(n$kind, "dataset_variable")) return("dataset")
        "other"
      }
      take_external <- function(items, nodes, wanted) {
        out <- lapply(items %||% list(), function(x) {
          used <- flat_chr(x$used_by)
          x$used_by <- as.list(used[vapply(used, function(id) usage_kind(id,nodes) %in% wanted, TRUE)])
          x
        })
        Filter(function(x) length(x$used_by) > 0L, out)
      }
      keep_kinds <- if (include_dataset_level) "other" else c("other", "dataset")
      reply_kinds <- if (include_dataset_level) c("parameter", "dataset") else "parameter"
      ext <- c(take_external(latest$external, latest$nodes, keep_kinds),
               take_external(graph$external, graph$nodes %||% list(), reply_kinds))
      latest$external <- list()
      for (x in ext) {
        ref <- toupper(x$ref %||% paste(x$domain %||% "", x$var %||% "", sep="."))
        at <- which(vapply(latest$external, function(y) identical(toupper(y$ref %||% paste(y$domain %||% "",y$var %||% "",sep=".")), ref), TRUE))
        if (length(at)) {
          i <- at[[1]]; latest$external[[i]]$used_by <- as.list(unique(c(flat_chr(latest$external[[i]]$used_by),flat_chr(x$used_by))))
        } else latest$external[[length(latest$external)+1L]] <- x
      }
      for (member in layer_codes) if (!is.null(graph$layer_keys[[member]])) latest$layer_keys[[member]] <- graph$layer_keys[[member]]
      generated_code <- if (is_group) "CARRY_OVER" else if (length(layer_record)) pc else members
      latest$generated_parameters <- unique(c(toupper(flat_chr(latest$generated_parameters)), generated_code))
      # Nothing generated yet means a new start, so an older stored reply doesn't come back.
      old_reply <- if (include_dataset_level) NULL else extract_reply_read(A$project, ds)
      latest$extraction <- extract_merge_replies(old_reply, graph$extraction, layer_codes, include_dataset_level)
      latest$unresolved <- extract_merge_issues(latest$unresolved, graph$unresolved, layer_codes, include_dataset_level)
      latest$cycle_cuts <- extract_merge_cuts(latest$cycle_cuts, graph$cycle_cuts, layer_codes, include_dataset_level)
      latest$param_uses <- c(Filter(function(x) !toupper(x$to %||% "") %in% members, latest$param_uses %||% list()), graph$param_uses %||% list())
      latest$layer_warnings <- extract_merge_warnings(latest$layer_warnings, graph$warnings, layer_codes)
      latest$warnings <- as.list(unique(unlist(latest$layer_warnings, use.names = FALSE)))
      store_extracted(A, ds, latest)
      json_resp(latest)
    },
    onRejected=function(e) { job_finish(A,id,e); err_resp(conditionMessage(e),502L) })
  remember(A,id,done)
  if (wait) done else invisible(done)
}

layer_extract_job <- function(project_dir, dataset, paramcd) {
  old <- llm_log_scope(project_dir, "extract", paste0(dataset, "@", paramcd), "none")
  on.exit(options(dualdag.llm_log = old), add = TRUE)
  spec <- read_project_spec(project_dir)
  current <- load_project_graph(project_dir, dataset)
  extract_dependencies(with_cfg(), spec, dataset, catalog=job_catalog(project_dir), project=project_dir, paramcd=paramcd,
                       include_dataset_level=!length(current$generated_parameters %||% list()))
}

side_has_output <- function(p, ds, id, side, st) {
  isTRUE(st$sides[[side]]$ok) || file.exists(file.path(node_run_dir(p, ds, id), side, "out.rds"))
}

# Spec-path warnings ride on status$warnings. They do not change level or problem.
attach_primary_path_warnings <- function(p, ds, id, st, primary_ran) {
  if (!isTRUE(primary_ran)) return(st)
  if (!fn_exists("primary_spec_path_warnings")) return(st)
  code <- tryCatch(read_text(file.path(node_run_dir(p, ds, id), "primary", "code.R")), error = function(e) "")
  spec <- tryCatch(read_project_spec(p), error = function(e) NULL)
  domain_cols <- if (fn_exists("data_index")) tryCatch(data_index(p), error = function(e) list()) else list()
  if (is.null(domain_cols)) domain_cols <- list()
  notes <- tryCatch(read_side_notes(p, ds, id, "primary"), error = function(e) list())
  fresh <- tryCatch(
    primary_spec_path_warnings(code %||% "", spec, ds, id, domain_cols = domain_cols, notes = notes),
    error = function(e) character()
  )
  old <- as.character(unlist(st$warnings, use.names = FALSE))
  old <- old[!is.na(old) & nzchar(old)]
  old <- old[!grepl("^Primary uses .+ which the spec does not name for this variable$", old)]
  msgs <- unique(c(old, fresh))
  st$warnings <- if (length(msgs)) as.list(msgs) else NULL
  st
}

missing_dependency_message <- function(side, error) {
  error <- as.character(error %||% "")[1]
  old <- "DUALDAG_MISSING_DEPENDENCY:"
  current <- "DUALDAG_MISSING_COLUMN:"
  legacy_message <- grepl("says the DAG is missing a dependency:", error, fixed = TRUE)
  marker <- if (grepl(current, error, fixed = TRUE)) current else if (grepl(old, error, fixed = TRUE)) old else if (legacy_message) "says the DAG is missing a dependency:" else return(error)
  refs <- trimws(strsplit(sub(paste0(".*", marker, "[[:space:]]*"), "", error), "[\r\n]", perl = TRUE)[[1]][1])
  if (startsWith(refs, "Missing column:")) return(refs)
  if (legacy_message) refs <- sub("\\. Review.*$", "", refs)
  ref <- strsplit(refs, ",", fixed = TRUE)[[1]][1]
  parsed <- strsplit(ref, "\\.", perl = TRUE)[[1]]
  column <- if (length(parsed) > 1L) parsed[[length(parsed)]] else ref
  frame <- if (identical(side, "primary") && length(parsed) > 1L) paste0("LINEAGE_", toupper(parsed[[1]])) else "the provided frame"
  paste0("Missing column: ", column, " is not in ", frame, " or the provided data.")
}

lineage_code_warnings <- function(code, side, ds, id, allowed, domain_cols, keys) {
  if (!identical(side, "primary")) return(character())
  parsed_node <- parse_node_id(id)
  lineage_names <- unique(c(side_lineage_name(ds, parsed_node), side_temp_name(ds, parsed_node)))
  dataset_bindings <- setNames(rep("current", length(lineage_names)), toupper(lineage_names))
  domains <- names(domain_cols) %||% character()
  source_bindings <- if (length(domains))
    setNames(toupper(domains), paste0("LINEAGE_SRC_", toupper(domains))) else character()
  used <- code_input_uses(code, domain_cols, keys, dataset_bindings = dataset_bindings,
                          source_bindings = source_bindings)
  if (!isTRUE(used$ok)) return(character())
  fresh <- character()
  for (u in used$uses) {
    ref <- if (identical(u$kind, "domain")) toupper(u$origin) else paste0(ds, ".", toupper(u$variable))
    upstream_allowed <- identical(u$kind, "upstream") && lineage_upstream_ref_allowed(ref, id, allowed$ids)
    layer_ref <- if (identical(u$kind, "layer")) paste0(ds, ".", toupper(u$variable), "@", toupper(u$domain)) else ""
    layer_allowed <- nzchar(layer_ref) && layer_ref %in% toupper(allowed$ids %||% character())
    if (upstream_allowed || layer_allowed || ref %in% c(allowed$ids, allowed$source_refs, paste0(ds, ".", keys))) next
    parsed_id <- parse_node_id(id)
    if (u$kind %in% c("upstream", "layer")) {
      fresh <- c(fresh, paste0("Lineage warning: ", parsed_id$var, " reads ", toupper(u$variable), ", which is not in its DAG lineage."))
    } else if (identical(u$kind, "layer_all")) {
      fresh <- c(fresh, paste0("Lineage warning: ", parsed_id$var, " reads LINEAGE_", toupper(u$domain), ", which is not in its DAG lineage."))
    } else {
      fresh <- c(fresh, paste0("Lineage warning: ", parsed_id$var, " reads ", ref, ", which is not in its DAG lineage."))
    }
  }
  unique(fresh)
}

normalize_missing_column_status <- function(st) {
  if (!is.list(st)) return(st)
  if (is.list(st$problem) && !is.null(st$problem$message))
    st$problem$message <- missing_dependency_message("primary", st$problem$message)
  if (is.list(st$sides)) for (side in names(st$sides)) if (is.list(st$sides[[side]]) && !is.null(st$sides[[side]]$error))
    st$sides[[side]]$error <- missing_dependency_message(side, st$sides[[side]]$error)
  st
}

attach_lineage_warnings <- function(p, ds, id, st, sides) {
  if (!fn_exists("code_input_uses") || !fn_exists("lineage_context")) return(st)
  spec <- tryCatch(read_project_spec(p), error = function(e) NULL)
  graph <- tryCatch(load_graph(p, ds), error = function(e) NULL)
  if (is.null(spec) || is.null(graph)) return(st)
  domain_cols <- if (fn_exists("data_index")) tryCatch(data_index(p), error = function(e) list()) else list()
  allowed <- lineage_context(spec, graph, id, p, "primary")
  keys <- spec_key_names(spec, ds)
  old <- as.character(unlist(st$warnings, use.names = FALSE))
  old <- old[!grepl("^(Possible missing edge:|Lineage warning:)", old)]
  fresh <- character()
  for (side in sides) {
    if (!identical(side, "primary")) next
    code <- tryCatch(read_text(file.path(node_run_dir(p, ds, id), side, "code.R")), error = function(e) "")
    fresh <- c(fresh, lineage_code_warnings(code, side, ds, id, allowed, domain_cols, keys))
  }
  msgs <- unique(c(old, fresh))
  st$warnings <- if (length(msgs)) as.list(msgs) else NULL
  st
}

apply_generate_results <- function(p, ds, id, results) {
  st <- get_status(p, ds, id)
  failed <- FALSE
  stage <- "generate"
  msg <- NULL
  for (side in names(results)) {
    res <- results[[side]]
    if (!isTRUE(res$ok)) {
      failed <- TRUE
      stage <- "generate"
      msg <- missing_dependency_message(side, res$error %||% "Generation failed.")
      st$sides[[side]] <- list(ok = FALSE, error = msg, updated = now_iso())
      next
    }
    val <- res$value
    ran <- if (is.list(val$run)) val$run else list()
    ok <- isTRUE(ran$ok) || (is.character(ran$out_path) && length(ran$out_path) == 1 && file.exists(ran$out_path))
    err <- ran$error
    st$sides[[side]] <- list(ok = ok, error = if (ok) NULL else missing_dependency_message(side, if (is.null(err)) "Run failed." else as.character(err)), updated = now_iso())
    if (identical(side, "validation")) st$same_source <- isTRUE(val$same_source)
    if (!ok) {
      failed <- TRUE
      stage <- "run"
      msg <- st$sides[[side]]$error
    }
  }
  base <- node_run_dir(p, ds, id)
  unlink(file.path(base, "compare.json"))
  unlink(file.path(base, "qc_report.md"))
  both <- side_has_output(p, ds, id, "primary", st) && side_has_output(p, ds, id, "validation", st)
  if (failed || !both) {
    st$level <- "none"
    st$problem <- list(stage = stage, message = msg %||% "Both sides must be generated and run.")
  } else {
    st$level <- "L2"
    st$problem <- NULL
  }
  st$upstream_changed <- NULL
  ran_primary <- is.list(results$primary) && isTRUE(results$primary$ok) &&
    is.list(results$primary$value) && is.list(results$primary$value$run)
  st <- attach_primary_path_warnings(p, ds, id, st, ran_primary)
  st <- attach_lineage_warnings(p, ds, id, st, names(results))
  put_status(p, ds, id, st, save = FALSE)
  flag_downstream_of(p, ds, id)
  write_state(p)
  get_status(p, ds, id)
}

# Re-run of stored code. Both sides must succeed to reach L2. Compare is cleared
# and has to be done again, so the node never stays at L3. Descendants keep their
# level and are only marked upstream_changed.
apply_stored_run <- function(p, ds, id, results) {
  st <- get_status(p, ds, id)
  failed <- FALSE
  msg <- NULL
  for (side in names(results)) {
    res <- results[[side]]
    if (!is.list(res)) res <- list(ok = FALSE, error = "Run returned nothing.")
    ok <- isTRUE(res$ok)
    err <- res$error
    if (!ok && (is.null(err) || !nzchar(as.character(err)[1] %||% ""))) err <- "Run failed."
    st$sides[[side]] <- list(
      ok = ok,
      error = if (ok) NULL else missing_dependency_message(side, as.character(err)[1]),
      updated = now_iso()
    )
    if (!ok && !failed) {
      failed <- TRUE
      msg <- st$sides[[side]]$error
    }
  }
  base <- node_run_dir(p, ds, id)
  unlink(file.path(base, "compare.json"))
  unlink(file.path(base, "qc_report.md"))
  both <- side_has_output(p, ds, id, "primary", st) && side_has_output(p, ds, id, "validation", st)
  if (both && !failed) {
    st$level <- "L2"
    st$problem <- NULL
  } else if (failed) {
    st$level <- "none"
    st$problem <- list(stage = "run", message = msg %||% "Run failed.")
  } else if (st$level %in% c("L2", "L3")) {
    st$level <- "none"
    st$problem <- NULL
  } else if (identical(st$problem$stage %||% "", "run") &&
             !any(vapply(st$sides %||% list(), function(side) {
               err <- as.character(side$error %||% "")[1]
               !is.na(err) && nzchar(err)
             }, TRUE))) {
    st$problem <- NULL
  }
  st$upstream_changed <- NULL
  st <- attach_primary_path_warnings(p, ds, id, st, "primary" %in% names(results))
  st <- attach_lineage_warnings(p, ds, id, st, names(results))
  put_status(p, ds, id, st, save = FALSE)
  flag_downstream_of(p, ds, id)
  write_state(p)
  get_status(p, ds, id)
}

settle <- function(pr) {
  promises::then(pr,
    onFulfilled = function(v) list(ok = TRUE, value = v),
    onRejected = function(e) list(ok = FALSE, error = conditionMessage(e)))
}

combine_named <- function(prs) {
  promises::promise(function(resolve, reject) {
    nms <- names(prs)
    left <- length(nms)
    out <- list()
    if (!left) { resolve(out); return() }
    for (nm in nms) local({
      key <- nm
      promises::then(prs[[key]],
        onFulfilled = function(v) {
          out[[key]] <<- v
          left <<- left - 1L
          if (left <= 0L) resolve(out)
        },
        onRejected = function(e) {
          out[[key]] <<- list(ok = FALSE, error = conditionMessage(e))
          left <<- left - 1L
          if (left <= 0L) resolve(out)
        })
    })
  })
}

save_compare <- function(p, ds, id, res) {
  d <- node_run_dir(p, ds, id)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  write_json(res, file.path(d, "compare.json"))
  invisible(res)
}

report_text <- function(x) {
  if (is.character(x)) return(paste(x, collapse = "\n"))
  if (is.list(x)) return(paste(as.character(x$report %||% x$markdown %||% x$text %||% ""), collapse = "\n"))
  ""
}

qc_text <- function(x) {
  if (is.null(x)) return("")
  if (is.character(x)) return(paste(x, collapse = "\n"))
  if (is.list(x)) return(paste(as.character(x$markdown %||% x$text %||% x$report %||% ""), collapse = "\n"))
  ""
}

export_path_of <- function(x, p, ds, format) {
  if (is.character(x) && length(x) == 1 && file.exists(x)) return(x)
  if (is.list(x) && is.character(x$path) && length(x$path) == 1 && file.exists(x$path)) return(x$path)
  if (is.raw(x)) {
    dest <- file.path(p$dir, "output", paste0(tolower(ds), ".", format))
    write_bytes(x, dest)
    return(dest)
  }
  stop("export_dataset did not return a file.", call. = FALSE)
}

pkg_versions <- function() {
  ps <- names(dualdag_required_packages())
  stats::setNames(lapply(ps, function(nm) tryCatch(as.character(utils::packageVersion(nm)), error = function(e) NA)), ps)
}

handle_api <- function(A, req, path, method, q) {
  p <- A$project
  POST <- identical(method, "POST")
  if (POST && isTRUE(A$admin_active) && !path %in% c("/api/admin/stop", "/api/admin/chat"))
    return(err_resp("Refused while an App Admin turn is running.",409L))
  if (path == "/api/admin/state" && identical(method, "GET")) {
    if (is.null(A$admin_cli_status_cache) || as.numeric(Sys.time())-A$admin_cli_status_cache$time>30) {
      A$admin_cli_status_cache <- list(time=as.numeric(Sys.time()),values=list(
        claude=tryCatch(admin_cli_status("claude_cli"),error=function(e)list(installed=FALSE,logged_in=FALSE,hint="Unavailable.")),
        codex=tryCatch(admin_cli_status("codex_cli"),error=function(e)list(installed=FALSE,logged_in=FALSE,hint="Unavailable."))))
    }
    statuses <- A$admin_cli_status_cache$values
    return(json_resp(list(session=A$admin_session, status=statuses, active=isTRUE(A$admin_active), notice="App Admin can change this app and this project without asking. It is told to stay inside them, but this is not enforced. Changes outside those roots are not detected. You are responsible for your system.")))
  }
  if (path == "/api/admin/new" && POST) {
    if (isTRUE(A$admin_active)) return(err_resp("An App Admin turn is running.",409L))
    A$admin_session <- list(provider="", session_id="", sessions=list(), turns=list()); write_json(A$admin_session,admin_session_path(p$dir))
    return(json_resp(list(ok=TRUE,session=A$admin_session)))
  }
  if (path == "/api/admin/stop" && POST) {
    if (!isTRUE(A$admin_active)) return(err_resp("No App Admin turn is running.",409L))
    A$admin_stop <- TRUE
    admin_stop_processes(A)
    return(json_resp(list(ok=TRUE)))
  }
  if (path == "/api/admin/undo" && POST) {
    if (length(A$jobs) || batch_running(A) || isTRUE(A$admin_active)) return(err_resp("Cannot undo while work is running.",409L))
    res <- tryCatch(admin_undo(A$app_dir,p$dir),error=function(e)e)
    if (inherits(res,"error")) return(err_resp(conditionMessage(res),409L))
    if (!isTRUE(res$ok)) return(json_resp(res))
    A$project <- admin_refresh_project(A)
    res$flags <- admin_change_flags(res$files)
    res$notice <- if(nzchar(res$notice %||% ""))res$notice else "Undo restored the last snapshot."
    res$warnings <- res$hash_only_warnings %||% list()
    if(length(res$warnings))res$notice<-paste(res$notice,"Not restored (hash only):",paste(unlist(res$warnings),collapse=", "))
    A$admin_session$last_notice <- res$notice
    A$admin_session$last_notice_flags <- res$flags
    write_json(A$admin_session,admin_session_path(p$dir))
    return(json_resp(res))
  }
  if (path == "/api/admin/restart" && POST) {
    res <- tryCatch(admin_restart_app(A),error=function(e)e)
    if (inherits(res,"error")) return(err_resp(conditionMessage(res),409L))
    return(json_resp(res))
  }
  if (path == "/api/admin/chat" && POST) {
    if (length(A$jobs) || batch_running(A) || isTRUE(A$admin_active)) return(err_resp("App Admin is unavailable while a job or batch is running.",409L))
    b <- read_body(req); provider <- as.character(b$provider %||% "")[1]
    if (!provider %in% c("claude","codex")) return(err_resp("Choose Claude Code or Codex.",400L))
    message <- trimws(as.character(b$message %||% "")[1]); if (!nzchar(message)) return(err_resp("message is required.",400L))
    cli_provider <- if(provider=="claude") "claude_cli" else "codex_cli"
    st <- tryCatch(admin_cli_status(cli_provider),error=function(e)list(installed=FALSE,logged_in=FALSE))
    if (!isTRUE(st$installed) || !isTRUE(st$logged_in)) return(err_resp("That CLI is not installed and logged in.",400L))
    prev <- A$admin_session
    sessions <- prev$sessions %||% list()
    sid <- as.character(sessions[[provider]] %||% if (identical(prev$provider,provider)) prev$session_id %||% "" else "")
    snapshot_id <- admin_snapshot(A$app_dir,p$dir)
    before_manifest <- admin_manifest(A$app_dir,p$dir)
    A$admin_turn <- list(snapshot_id=snapshot_id,before=before_manifest,previous=prev,provider=provider,message=message)
    A$admin_active <- TRUE; A$admin_stop <- FALSE; jid <- job_start(A,"app-admin")
    pr <- launch(A,"app_admin_turn_job",list(app_dir=A$app_dir,project=p$dir,provider=provider,prompt=message,session_id=sid),630,tracker=A$admin_procs)
    return(remember(A,jid,promises::then(pr,onFulfilled=function(ans){
      # Capture the CLI's file changes before reopening the project performs
      # normal synchronization writes to state.json.
      changed <- admin_changed(before_manifest,admin_manifest(A$app_dir,p$dir))
      admin_refresh_project(A)
      admin_finish_snapshot(A$app_dir,p$dir,snapshot_id)
      ans$changed_files <- as.list(changed)
      flags <- admin_change_flags(changed)
      ans$refresh <- flags$refresh; ans$restart <- flags$restart; ans$relaunch <- flags$relaunch
      ans$flags <- flags
      ans$notice <- admin_diff_notice(changed)
      ans$turns <- c(prev$turns %||% list(),list(list(role="user",text=message),list(role="assistant",text=ans$reply %||% "",changed_files=ans$changed_files %||% list(),actions=ans$actions %||% list(),notice=ans$notice,refresh=ans$refresh,restart=ans$restart,relaunch=ans$relaunch,flags=flags,usage=ans$usage,model=ans$model)))
      sessions[[provider]] <- ans$session_id
      A$admin_session <- list(provider=provider,session_id=ans$session_id,sessions=sessions,turns=tail(ans$turns,40L),last_notice=ans$notice,last_notice_flags=flags); write_json(A$admin_session,admin_session_path(p$dir))
      A$admin_turn <- NULL
      A$admin_active <- FALSE; job_finish(A,jid); json_resp(ans)
    },onRejected=function(e){
      changed <- admin_changed(before_manifest,admin_manifest(A$app_dir,p$dir))
      admin_refresh_project(A); try(admin_finish_snapshot(A$app_dir,p$dir,snapshot_id),silent=TRUE)
      failed <- if(isTRUE(A$admin_stop)) "Stopped." else paste("Turn failed:",conditionMessage(e))
      flags <- admin_change_flags(changed)
      turns <- c(prev$turns %||% list(),list(list(role="user",text=message),list(role="assistant",text=failed,changed_files=as.list(changed),actions=list(),failed=TRUE,notice=admin_diff_notice(changed),flags=flags)))
      A$admin_session <- list(provider=provider,session_id=sid,sessions=sessions,turns=tail(turns,40L),last_notice=admin_diff_notice(changed),last_notice_flags=flags); write_json(A$admin_session,admin_session_path(p$dir))
      A$admin_turn<-NULL; A$admin_active<-FALSE; job_finish(A,jid,e)
      err_resp(failed,502L)
    })))
  }
  if (path == "/api/state" && identical(method, "GET")) return(json_resp(state_payload(A)))
  if (path == "/api/fullrun/run" && POST) {
    b <- read_body(req)
    ds <- tryCatch(safe_ds(b$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    side <- as.character(b$side %||% "")[1]
    if (!side %in% c("primary", "validation")) return(err_resp("side must be primary or validation.", 400L))
    if (any(vapply(A$jobs, function(j) identical(j$kind, "fullrun") && identical(j$dataset, ds) && identical(j$node, side), TRUE)))
      return(err_resp("A full run is already running for this side.", 409L))
    jid <- job_start(A, "fullrun", ds, side)
    pr <- launch(A, "fullrun_worker", list(project = p$dir, dataset = ds, side = side, timeout = run_limit(A)), run_limit(A) + 30)
    promises::then(pr,
      onFulfilled = function(res) { job_finish(A, jid); invisible(res) },
      onRejected = function(e) { job_finish(A, jid, e); invisible(NULL) })
    return(json_resp(list(ok = TRUE, running = TRUE, dataset = ds, side = side), 202L))
  }
  if (path == "/api/fullrun/status" && identical(method, "GET")) {
    ds <- tryCatch(safe_ds(q$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    sides <- lapply(c("primary", "validation"), function(side) {
      running <- any(vapply(A$jobs, function(j) identical(j$kind, "fullrun") && identical(j$dataset, ds) && identical(j$node, side), TRUE))
      fullrun_status(p$dir, ds, side, running)
    })
    names(sides) <- c("primary", "validation")
    return(json_resp(list(dataset = ds, sides = sides)))
  }
  if (path == "/api/fullrun/steps" && identical(method, "GET")) {
    ds <- tryCatch(safe_ds(q$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    side <- as.character(q$side %||% "")[1]
    if (!side %in% c("primary", "validation")) return(err_resp("side must be primary or validation."))
    return(json_resp(list(dataset = ds, side = side, steps = fullrun_steps(p$dir, ds, side))))
  }
  if (path %in% c("/api/fullrun/program", "/api/fullrun/log") && identical(method, "GET")) {
    ds <- tryCatch(safe_ds(q$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    side <- as.character(q$side %||% "")[1]
    if (!side %in% c("primary", "validation")) return(err_resp("side must be primary or validation."))
    field <- if (identical(path, "/api/fullrun/program")) "program.R" else "log.txt"
    f <- file.path(fullrun_dir(p$dir, ds, side), field)
    if (identical(field, "program.R")) {
      plan <- fullrun_plan(p$dir, ds, side)
      program <- fullrun_program_text(p$dir, ds, side, plan, side_r_packages(p$dir, side))$text
      return(list(status = 200L, headers = list(`Content-Type` = "text/plain; charset=utf-8", `Cache-Control` = "no-store"), body = charToRaw(enc2utf8(program))))
    }
    if (!file.exists(f)) return(list(status = 200L, headers = list(`Content-Type` = "text/plain; charset=utf-8", `Cache-Control` = "no-store"), body = raw()))
    return(list(status = 200L, headers = list(`Content-Type` = "text/plain; charset=utf-8", `Cache-Control` = "no-store"), body = read_file_raw(f)))
  }
  if (path == "/api/graph/layout" && POST) {
    b <- read_body(req)
    ds <- tryCatch(safe_ds(b$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds)))
    if (isTRUE(b$reset)) {
      reset_layout(p, ds)
      return(json_resp(list(positions = empty_named())))
    }
    result <- tryCatch(save_layout(p, ds, b$positions), error = function(e) e)
    if (inherits(result, "error")) return(err_resp(conditionMessage(result)))
    return(json_resp(list(positions = result)))
  }
  if (path == "/api/node/categories" && identical(method, "GET")) {
    ds <- tryCatch(safe_ds(q$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds)))
    id <- tryCatch(canonical_node_id(ds, q$node), error = function(e) e)
    if (inherits(id, "error")) return(err_resp(conditionMessage(id)))
    result <- tryCatch(node_categories(p, ds, id), error = function(e) e)
    if (inherits(result, "error")) return(err_resp(conditionMessage(result), 404L))
    return(json_resp(result))
  }
  if (path == "/api/node/lineage" && identical(method, "GET")) {
    ds <- tryCatch(safe_ds(q$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds)))
    id <- tryCatch(canonical_node_id(ds, q$node), error = function(e) e)
    if (inherits(id, "error")) return(err_resp(conditionMessage(id)))
    graph <- tryCatch(load_graph(p, ds), error = function(e) e)
    if (inherits(graph, "error")) return(err_resp(conditionMessage(graph), 404L))
    if (is.null(graph)) return(err_resp("Dependency graph is not available.", 404L))
    node <- graph_node(graph, id)
    selection <- lineage_selection(graph, node, p)
    edges <- lineage_review_edges(graph, selection$ids, p)
    spec <- tryCatch(read_project_spec(p), error = function(e) NULL)
    catalog <- if (fn_exists("build_source_catalog")) tryCatch(build_source_catalog(p$dir), error = function(e) NULL) else NULL
    contexts <- setNames(lapply(c("primary", "validation"), function(side)
      lineage_context(spec, graph, node, p, side, catalog)), c("primary", "validation"))
    roles <- lapply(selection$ids, function(node_id) {
      if (identical(node_id, selection$target_id)) return(list(id = node_id, role = "target"))
      sides <- lapply(contexts, function(ctx) ctx$roles[[node_id]] %||% "dependency")
      names(sides) <- c("primary", "validation")
      list(id = node_id, role = "upstream", context = sides)
    })
    source_context <- lapply(contexts, function(ctx) list(
      requested = as.list(ctx$source_context$requested), supplied = as.list(ctx$source_context$supplied)))
    return(json_resp(list(target_id = selection$target_id, upstream_ids = as.list(selection$upstream_ids),
      ids = as.list(selection$ids), roles = roles, source_context = source_context, edges = edges)))
  }
  if (path == "/api/catalog" && identical(method, "GET")) {
    if (!fn_exists("build_source_catalog") || !fn_exists("catalog_public")) return(need_fn("build_source_catalog"))
    cat <- tryCatch(build_source_catalog(p$dir), error = function(e) e)
    if (inherits(cat, "error")) return(err_resp(conditionMessage(cat), 500L))
    return(json_resp(catalog_public(cat)))
  }
  if (path == "/api/info" && identical(method, "GET")) {
    return(json_resp(list(r_version = R.version.string, platform = R.version$platform, packages = pkg_versions(),
                          app = "dualdag", app_version = "0.1.0")))
  }
  # Package list only. Does not write state.json and does not reset any variable.
  if (path == "/api/packages" && identical(method, "GET")) {
    if (!fn_exists("r_packages_public")) return(need_fn("r_packages_public"))
    info <- tryCatch(r_packages_public(p$dir), error = function(e) e)
    if (inherits(info, "error")) return(err_resp(conditionMessage(info), 500L))
    return(json_resp(info))
  }
  if (path == "/api/packages" && POST) {
    if (!fn_exists("r_packages_save")) return(need_fn("r_packages_save"))
    saved <- tryCatch(r_packages_save(p$dir, read_body(req)), error = function(e) e)
    if (inherits(saved, "error")) return(err_resp(conditionMessage(saved), 400L))
    return(json_resp(saved))
  }
  if (path == "/api/llm/cli_status" && identical(method, "GET")) {
    if (!fn_exists("cli_status")) return(need_fn("cli_status"))
    prov <- as.character(q$provider %||% "")[1]
    if (!prov %in% c("claude_cli", "codex_cli", "grok_cli")) return(err_resp("Unknown provider."))
    info <- tryCatch(cli_status(prov), error = function(e) {
      list(installed = FALSE, path = "", version = "",
           logged_in = if (identical(prov, "grok_cli")) NULL else FALSE,
           method = "", hint = "Could not check the CLI.")
    })
    return(json_resp(info))
  }
  if (path == "/api/config" && POST) {
    b <- read_body(req)
    allowed <- config_allowed_providers()
    if (!is.null(b$provider) && !b$provider %in% allowed) return(err_resp("Unknown provider."))
    for (k in intersect(names(b), c("provider", "base_url", "model", "strong_model", "timeout_run_s", "llm_timeout_s", "cli_timeout_s"))) A$cfg[[k]] <- b[[k]]
    if (A$cfg$provider %in% c("claude_cli", "codex_cli", "grok_cli")) A$cfg$cli_use_login <- TRUE
    else if ("cli_use_login" %in% names(b)) A$cfg$cli_use_login <- cli_prefers_login(list(cli_use_login = b$cli_use_login))
    A$cfg <- config_apply_model_defaults(A$cfg)
    if (!is.null(b$api_key) && nzchar(b$api_key)) save_api_key(b$api_key)
    if (isTRUE(b$clear_api_key)) save_api_key("")
    save_config(A$cfg)
    return(json_resp(list(ok = TRUE, config = config_public(A$cfg))))
  }
  if (path == "/api/dataset/formats" && POST) {
    b <- read_body(req)
    spec <- tryCatch(read_project_spec(p), error=function(e) NULL)
    if (is.null(spec)) return(err_resp("Upload a spec before classifying datasets.", 409L))
    old <- dataset_types_public(p, spec)
    submitted <- b$formats %||% list()
    changed_request <- names(submitted)[vapply(names(submitted), function(ds)
      !identical(old[[toupper(ds)]] %||% "", toupper(as.character(submitted[[ds]])[1])), TRUE)]
    if (any(vapply(A$jobs, function(j) j$dataset %in% toupper(changed_request), TRUE)))
      return(err_resp("Wait for dataset jobs to finish before changing formats.", 409L))
    saved <- tryCatch(save_dataset_types(p, submitted, spec), error=function(e) e)
    if (inherits(saved, "error")) return(err_resp(conditionMessage(saved), 400L))
    changed <- names(saved)[vapply(names(saved), function(ds) !identical(old[[ds]] %||% "", saved[[ds]]), TRUE)]
    for (ds in changed) {
      reset_dataset_work(p, ds)
      if (exists("param_graph_path", mode="function")) unlink(param_graph_path(p, ds))
      if (exists("write_network_json", mode="function")) write_network_json(p, ds)
      if (llm_ready(A$cfg)) start_extract(A, ds, wait=FALSE)
    }
    return(json_resp(list(ok=TRUE, dataset_types=saved, restarted=as.list(changed))))
  }
  if (path == "/api/config/test" && POST) {
    if (!fn_exists("llm_test") && !fn_exists("llm_chat")) return(need_fn("llm_test"))
    if (!llm_ready(A$cfg)) return(err_resp("No API key configured.", 400L))
    id <- job_start(A, "config-test")
    pr <- launch(A, "llm_test_job", list(), llm_limit(A))
    return(remember(A, id, promises::then(pr,
      onFulfilled = function(r) { job_finish(A, id); json_resp(r) },
      onRejected = function(e) { job_finish(A, id, e); err_resp(conditionMessage(e), 502L) })))
  }
  if (path == "/api/spec/upload" && POST) {
    b <- read_body(req)
    if (is.null(b$variable) || !nzchar(b$variable$base64 %||% "")) return(err_resp("variable.csv is required."))
    var_raw <- tryCatch(decode_base64(b$variable$base64), error = function(e) e)
    if (inherits(var_raw, "error")) return(err_resp(conditionMessage(var_raw), 400L))
    par_raw <- NULL
    if (!is.null(b$parameter) && nzchar(b$parameter$base64 %||% "")) {
      par_raw <- tryCatch(decode_base64(b$parameter$base64), error = function(e) e)
      if (inherits(par_raw, "error")) return(err_resp(conditionMessage(par_raw), 400L))
    }
    sp <- tryCatch(project_upload_spec(p, var_raw, par_raw), error = function(e) e)
    if (inherits(sp, "error")) return(err_resp(conditionMessage(sp), 400L))
    # The user starts extraction with the Generate button, so a new spec costs nothing until asked.
    return(json_resp(list(ok = TRUE, issues = sp$problems)))
  }
  if (path == "/api/data/upload" && POST) {
    b <- read_body(req)
    raw <- tryCatch(decode_base64(b$base64 %||% ""), error = function(e) e)
    if (inherits(raw, "error")) return(err_resp(conditionMessage(raw), 400L))
    info <- tryCatch(project_upload_data(p, b$name %||% "", raw), error = function(e) e)
    if (inherits(info, "error")) return(err_resp(conditionMessage(info), 400L))
    return(json_resp(list(ok = TRUE, data = info)))
  }
  if (path == "/api/data/remove" && POST) {
    b <- read_body(req)
    r <- tryCatch(project_remove_data(p, b$name %||% ""), error = function(e) e)
    if (inherits(r, "error")) return(err_resp(conditionMessage(r), 400L))
    return(json_resp(list(ok = TRUE)))
  }
  if (path == "/api/graph/extract" && POST) {
    if (!fn_exists("extract_dependencies")) return(need_fn("extract_dependencies"))
    if (!llm_ready(A$cfg)) return(err_resp("Choose a model in Settings.", 400L))
    b <- read_body(req)
    ds <- safe_ds(b$dataset)
    reset_dataset_work(p, ds)
    return(start_extract(A, ds, wait = TRUE))
  }
  if (path == "/api/param-graph" && identical(method, "GET")) {
    ds<-safe_ds(q$dataset); g<-param_graph_read(p,ds); if(is.null(g)) return(json_resp(list(dataset=ds,spec_hash=NULL,nodes=list(),edges=list(),error=NULL,warnings=list(),corrections=param_corrections(p,ds),stale=FALSE)))
    sp<-tryCatch(read_project_spec(p),error=function(e)NULL); g$stale<-!is.null(sp)&&!identical(g$spec_hash,sp$hash); g$corrections<-param_corrections(p,ds); return(json_resp(g))
  }
  if (path == "/api/project/start-new" && POST) {
    b <- read_body(req)
    if (!identical(b$confirm, "CONFIRM")) return(err_resp("Type CONFIRM exactly to start a new DAG.", 400L))
    if (length(A$jobs) || batch_running(A)) return(err_resp("Wait for all jobs and batches to finish before starting a new DAG.", 409L))
    if (!llm_ready(A$cfg)) return(err_resp("Choose a model in Settings.", 400L))
    wiped <- tryCatch(project_wipe_work(p), error = function(e) e)
    if (inherits(wiped, "error")) return(err_resp(conditionMessage(wiped), 400L))
    A$project <- open_project(p$dir)
    A$jobs <- list()
    A$procs <- new.env(parent = emptyenv())
    A$inflight <- new.env(parent = emptyenv())
    A$batch <- NULL
    A$last_error <- NULL
    A$admin_session <- list(provider="",session_id="",sessions=list(),turns=list())
    A$admin_cli_status_cache <- NULL
    A$admin_turn <- NULL
    if (exists("viewer_cache_reset", mode = "function", inherits = TRUE)) viewer_cache_reset()
    if (exists("lineage_source_cache_reset", mode = "function", inherits = TRUE)) lineage_source_cache_reset()
    if (exists("levels_cache_reset", mode = "function", inherits = TRUE)) levels_cache_reset()
    return(json_resp(list(ok = TRUE)))
  }
  if (path == "/api/param-graph/extract" && POST) { if(!fn_exists("start_param_extract"))return(need_fn("start_param_extract")); b<-read_body(req); return(start_param_extract(A,safe_ds(b$dataset),wait=TRUE)) }
  if (path == "/api/graph/parameter-extract" && POST) { b<-read_body(req); if(!llm_ready(A$cfg))return(err_resp("Choose a model in Settings.",400L)); return(start_layer_extract(A,safe_ds(b$dataset),b$paramcd,wait=TRUE)) }
  if (path == "/api/param-graph/ask" && POST) { if(!llm_ready(A$cfg))return(err_resp("Choose a model in Settings.",400L)); b<-read_body(req); ds<-safe_ds(b$dataset); message<-trimws(as.character(b$message %||% "")[1]); if(!nzchar(message))return(err_resp("message is required.",400L)); if(is.null(param_graph_read(p,ds)))return(err_resp("No parameter graph yet.",404L)); jid<-job_start(A,"param_ask",ds,NULL); pr<-launch(A,"param_ask_job",list(project_dir=p$dir,dataset=ds,message=message),llm_limit(A)); return(remember(A,jid,promises::then(pr,onFulfilled=function(res){job_finish(A,jid);json_resp(res)},onRejected=function(e){job_finish(A,jid,e);err_resp(conditionMessage(e),502L)}))) }
  if (path == "/api/param-graph/apply" && POST) { b<-read_body(req); ds<-safe_ds(b$dataset); res<-tryCatch(param_graph_apply(p,ds,b$changes),error=function(e)e); if(inherits(res,"error"))return(err_resp(conditionMessage(res),400L)); return(json_resp(res)) }
  if (path == "/api/graph/download" && identical(method, "GET")) {
    ds <- safe_ds(q$dataset)
    what <- q$what %||% "graph"
    if (!what %in% c("graph", "dag", "network")) return(err_resp("what must be graph, dag, or network."))
    if (identical(what, "network")) {
      if (exists("write_network_json", mode = "function")) write_network_json(p, ds)
      root <- network_dir(p, ds)
      paramcd <- toupper(trimws(as.character(q$paramcd %||% "")[1]))
      name <- if (identical(paramcd, "_DATASET")) "_dataset.json" else if (nzchar(paramcd)) paste0(paramcd, ".json") else "_parameters.json"
      f <- file.path(root, name)
      if (!file.exists(f)) return(err_resp(if (nzchar(paramcd)) "No network file for this parameter." else "No parameter network.", 404L))
      return(file_resp(read_file_raw(f), name, "application/json"))
    }
    if (identical(what, "dag")) {
      if (!fn_exists("dag_md_path")) return(need_fn("dag_md_path"))
      f <- dag_md_path(p, ds)
      if (!file.exists(f)) {
        g <- load_graph(p, ds)
        if (is.null(g)) return(err_resp("No graph.", 404L))
        write_dag_md(p, g)
        f <- dag_md_path(p, ds)
      }
      if (!file.exists(f)) return(err_resp("No DAG file.", 404L))
      return(file_resp(read_file_raw(f), paste0("dag_", ds, ".md"), "text/markdown; charset=utf-8"))
    }
    f <- graph_path(p, ds)
    if (!file.exists(f)) return(err_resp("No graph.", 404L))
    return(file_resp(read_file_raw(f), paste0(ds, "-graph.json"), "application/json"))
  }
  if (path == "/api/dag/ask" && POST) {
    if (!fn_exists("dag_ask")) return(need_fn("dag_ask"))
    b <- read_body(req)
    if (length(A$jobs) || batch_running(A)) return(err_resp("The DAG agent is unavailable while a job or batch is running.", 409L))
    if (!llm_ready(A$cfg)) return(err_resp("Choose a model in Settings.", 400L))
    ds <- tryCatch(safe_ds(b$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    message <- trimws(as.character(b$message %||% "")[1])
    if (!nzchar(message)) return(err_resp("message is required.", 400L))
    if (is.null(load_graph(p, ds))) return(err_resp("No dependency graph yet.", 404L))
    jid <- job_start(A, "dag_ask", ds, NULL)
    pr <- launch(A, "dag_ask_job", list(project_dir = p$dir, dataset = ds, message = message,
      paramcd = b$paramcd %||% NULL, selected = b$selected %||% NULL, parameter_graph = isTRUE(b$parameter_graph)), llm_limit(A))
    return(remember(A, jid, promises::then(pr,
      onFulfilled = function(res) {
        # DAG edits run in a child process; load its spec-change state updates.
        A$project$state <- read_state_file(A$project)
        job_finish(A, jid)
        json_resp(list(
          answer = res$answer %||% "",
          changes = res$changes %||% list(), card = res$card %||% list(),
          correction = res$correction %||% NULL,
          model = res$model %||% "",
          usage = res$usage %||% NULL,
          turns = res$turns %||% list()
        ))
      },
      onRejected = function(e) {
        # The child may have written state before a later round failed.
        A$project$state <- read_state_file(A$project)
        job_finish(A, jid, e)
        err_resp(conditionMessage(e), 502L)
      })))
  }
  if (path == "/api/super-agent/chat" && POST) {
    b <- read_body(req); tab <- as.character(b$tab %||% "")[1]
    if (!tab %in% c("settings","spec","run","data","mismatches")) return(err_resp("Super agent is unavailable on this tab.",400L))
    if (length(A$jobs) || batch_running(A)) return(err_resp("The Super agent is unavailable while a job or batch is running.",409L))
    msgs <- b$messages %||% list(); if (!length(msgs)) return(err_resp("message is required.",400L)); if(length(msgs)>20L) msgs<-tail(msgs,20L)
    jid<-job_start(A,"super-agent",NULL,NULL); pr<-launch(A,"super_agent_chat_job",list(project_dir=p$dir,tab=tab,messages=msgs,tool_result=super_agent_cap(b$tool_result %||% NULL)),llm_limit(A))
    return(remember(A,jid,promises::then(pr,onFulfilled=function(ans){job_finish(A,jid);json_resp(ans)},onRejected=function(e){job_finish(A,jid,e);err_resp(conditionMessage(e),502L)})))
  }
  if (path == "/api/super-agent/action" && POST) {
    b<-read_body(req); tab<-as.character(b$tab %||% "")[1]; tool<-as.character(b$tool %||% "")[1]
    if (!tab %in% c("settings","spec","run","data","mismatches")) return(err_resp("Super agent is unavailable on this tab.",400L))
    if (!tool %in% super_agent_tools(tab)) return(json_resp(list(ok=FALSE,error="Refused: that action is unavailable from this tab.")))
    if (identical(tab,"spec") && identical(tool,"dag_ask")) {
      if(length(A$jobs)||batch_running(A)) return(err_resp("Refused while a job or batch is running.",409L))
      if(!llm_ready(A$cfg)) return(err_resp("Choose a model in Settings.",400L))
      msg<-trimws(as.character((b$args %||% list())$message %||% "")[1]); if(!nzchar(msg)) return(err_resp("A clear DAG request is required.",400L))
      ds<-tryCatch(safe_ds(b$dataset),error=function(e) ""); if(!nzchar(ds)) return(err_resp("Select a dataset first.",400L))
      jid<-job_start(A,"dag_ask",ds,NULL); pr<-launch(A,"dag_ask_job",list(project_dir=p$dir,dataset=ds,message=msg),llm_limit(A))
      return(remember(A,jid,promises::then(pr,onFulfilled=function(res){A$project$state<-read_state_file(A$project);job_finish(A,jid);json_resp(list(ok=TRUE,answer=res$answer %||% "",card=res$card %||% list(),changes=res$changes %||% list()))},onRejected=function(e){job_finish(A,jid,e);err_resp(conditionMessage(e),502L)})))
    }
    if (identical(tab,"settings") && identical(tool,"test_login")) {
      if(!llm_ready(A$cfg)) return(err_resp("Choose a model in Settings.",400L))
      jid<-job_start(A,"config-test",NULL,NULL); pr<-launch(A,"llm_test_job",list(),llm_limit(A))
      return(remember(A,jid,promises::then(pr,onFulfilled=function(r){job_finish(A,jid);json_resp(list(ok=TRUE,result=r))},onRejected=function(e){job_finish(A,jid,e);err_resp(conditionMessage(e),502L)})))
    }
    act_args<-b$args %||% list(); act_args$dataset<-b$dataset %||% NULL; act_args$side<-b$side %||% "primary"
    return(json_resp(super_agent_cap(super_agent_run_action(A,p,tab,tool,act_args))))
  }
  if (path == "/api/layer-keys" && POST) {
    b <- read_body(req)
    ds <- tryCatch(safe_ds(b$dataset), error=function(e)e)
    if(inherits(ds,"error"))return(err_resp(conditionMessage(ds),400L))
    if(is.null(b$keys))return(err_resp("keys is required.",400L))
    dry <- tryCatch(dag_parse_bool(b$dry_run %||% FALSE),error=function(e)e)
    if(inherits(dry,"error"))return(err_resp(conditionMessage(dry),400L))
    result <- tryCatch(layer_keys_set(p,ds,b$paramcd %||% if(isTRUE(load_graph(p,ds)$layered))"" else "*",b$keys,dry),error=function(e)e)
    if(inherits(result,"error"))return(err_resp(conditionMessage(result),400L))
    return(json_resp(result))
  }
  if (path == "/api/dag/apply" && POST) {
    if (!fn_exists("apply_dag_changes")) return(need_fn("apply_dag_changes"))
    b <- read_body(req)
    ds <- tryCatch(safe_ds(b$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    res <- tryCatch(apply_dag_changes(p, ds, b$changes), error = function(e) e)
    if (inherits(res, "error")) return(err_resp(conditionMessage(res), 400L))
    return(json_resp(res))
  }
  if (path == "/api/dag/undo" && POST) {
    b <- read_body(req)
    ds <- tryCatch(safe_ds(b$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    if (length(A$jobs) || batch_running(A)) return(err_resp("Cannot undo while a job or batch is running.", 409L))
    history <- dag_history_dir(p,ds)
    snapshots <- if(dir.exists(history)) sort(list.dirs(history,recursive=FALSE,full.names=TRUE)) else character()
    snapshots <- snapshots[vapply(snapshots,function(dir) {
      manifest <- read_json(file.path(dir,"manifest.json")); is.list(manifest) && identical(as.character(manifest$dataset %||% ""),ds)
    },TRUE)]
    if(length(snapshots)) {
      latest <- tail(snapshots,1L); manifest <- read_json(file.path(latest,"manifest.json"))
      current_md5 <- list(variable=file_md5(file.path(p$dir,"spec","variable.csv")),
        parameter=file_md5(file.path(p$dir,"spec","parameter.csv")),graph=file_md5(graph_path(p,ds)))
      expected_md5 <- manifest$md5
      same_file <- function(name) {
        old <- expected_md5[[name]] %||% NULL; current <- current_md5[[name]] %||% NULL
        identical(if(is.null(old))"<missing>" else as.character(old),if(is.null(current))"<missing>" else as.character(current))
      }
      files_match <- is.list(expected_md5) && all(vapply(c("variable","parameter","graph"),same_file,TRUE))
      run_root <- file.path(p$dir,".dualdag","runs")
      run_files <- if(dir.exists(run_root)) list.files(run_root,recursive=TRUE,full.names=TRUE,all.files=TRUE,no..=TRUE) else character()
      run_files <- run_files[file.exists(run_files) & !dir.exists(run_files)]
      newer_run <- length(run_files)>0L && is.finite(as.numeric(manifest$time %||% NA_real_)) &&
        any(as.numeric(file.info(run_files)$mtime)>as.numeric(manifest$time),na.rm=TRUE)
      if(!files_match || newer_run) return(err_resp("Later changes exist, so this can't be undone.",409L))
      dag_restore_snapshot(p,ds,latest); unlink(latest,recursive=TRUE)
      mem <- dag_agent_read(p,ds)
      if(length(mem$turns)) {
        indices<-rev(seq_along(mem$turns))
        for(i in indices) if(identical(mem$turns[[i]]$role,"agent") && length(mem$turns[[i]]$card %||% list()) && !isTRUE(mem$turns[[i]]$undone)) { mem$turns[[i]]$undone<-TRUE; break }
        dag_agent_write(p,ds,mem)
      }
      return(json_resp(list(ok=TRUE,label="DAG agent reply",restored=unlist(manifest$files,use.names=FALSE),undo_count=max(0L,length(snapshots)-1L))))
    }
    steps <- dag_undo_read(p, ds)$steps
    if (!length(steps)) return(err_resp("Nothing is left to undo for this dataset.", 409L))
    step <- steps[[length(steps)]]
    save_graph(p, step$graph)
    mem <- dag_agent_read(p, ds); mem$corrections <- step$corrections %||% list(); dag_agent_write(p, ds, mem)
    restored <- character(); kept <- character()
    ids <- unlist(step$ids, use.names = FALSE)
    for (i in seq_along(ids)) {
      id <- ids[[i]]
      cur <- get_status(p, ds, id)
      expected <- step$after_updated[[id]] %||% NULL
      if (identical(cur$updated %||% NULL, expected)) {
        old <- step$statuses[[i]] %||% blank_status()
        if (is.null(p$state$nodes[[ds]]) || !is.list(p$state$nodes[[ds]])) p$state$nodes[[ds]] <- empty_named()
        p$state$nodes[[ds]][[id]] <- old
        restored <- c(restored, id)
      } else kept <- c(kept, id)
    }
    steps <- head(steps, -1L); dag_undo_write(p, ds, steps)
    write_dag_md(p, step$graph); write_state(p)
    return(json_resp(list(ok = TRUE, affected = as.list(unique(c(restored, kept))), edges = list(), layer_keys = list(),
      restored = as.list(restored), kept = as.list(kept), undo_count = length(steps))))
  }
  if (path == "/api/dag/clear" && POST) {
    if (length(A$jobs) || batch_running(A)) return(err_resp("Cannot clear the DAG chat while a job or batch is running.", 409L))
    b <- read_body(req); ds <- tryCatch(safe_ds(b$dataset),error=function(e)e)
    if(inherits(ds,"error")) return(err_resp(conditionMessage(ds),400L))
    dag_agent_clear(p,ds); return(json_resp(list(ok=TRUE)))
  }
  if (path == "/api/dag/correction/remove" && POST) {
    if (!fn_exists("dag_remove_correction")) return(need_fn("dag_remove_correction"))
    b <- read_body(req)
    ds <- tryCatch(safe_ds(b$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    mem <- tryCatch(dag_remove_correction(p, ds, b$at %||% ""), error = function(e) e)
    if (inherits(mem, "error")) return(err_resp(conditionMessage(mem), 400L))
    return(json_resp(list(ok = TRUE, corrections = mem$corrections %||% list())))
  }
  if (path == "/api/node/reset" && POST) {
    b <- read_body(req)
    ds <- tryCatch(safe_ds(b$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    id <- tryCatch(canonical_node_id(ds, b$node), error = function(e) e)
    if (inherits(id, "error")) return(err_resp(conditionMessage(id), 400L))
    if (node_busy(A, ds, id)) return(err_resp("This variable is already running.", 409L))
    st <- tryCatch(reset_one_node(p, ds, id, isTRUE(b$delete_code)), error = function(e) e)
    if (inherits(st, "error")) return(err_resp(conditionMessage(st), 500L))
    return(json_resp(st))
  }
  if (path == "/api/node/generate" && POST) {
    miss <- need_fn("generate_side", "run_side")
    if (!is.null(miss)) return(miss)
    if (!llm_ready(A$cfg)) return(err_resp("Choose a model in Settings.", 400L))
    b <- read_body(req)
    ds <- safe_ds(b$dataset)
    id <- canonical_node_id(ds, b$node)
    side <- b$side %||% "both"
    if (!nzchar(side)) side <- "both"
    if (!side %in% c("primary", "validation", "both")) return(err_resp("side must be primary, validation, or both."))
    g <- load_graph(p, ds)
    if (is.null(g)) return(err_resp("No dependency graph yet.", 404L))
    if (node_busy(A, ds, id)) return(err_resp("This variable is already running.", 409L))
    spec <- tryCatch(read_project_spec(p), error=function(e)NULL)
    node <- graph_node(g,id)
    sides <- if (identical(side,"both")) c("primary","validation") else side
    if (isTRUE(g$layered) && !is.null(node$paramcd)) {
      if (!toupper(node$paramcd) %in% toupper(flat_chr(g$generated_parameters))) return(err_resp("Generate its Variable DAG first.",409L))
      pg <- tryCatch(param_graph_read(p,ds),error=function(e)NULL)
      for (s in sides) {
        blocked <- param_layer_block_message(p,ds,s,node$paramcd,spec,pg)
        if (!is.null(blocked)) return(err_resp(blocked,409L))
      }
    }
    prepare_generate_status(p, ds, id, g)
    instruction <- b$instruction %||% b$user_instruction %||% NULL
    if (is.character(instruction) && !nzchar(instruction)) instruction <- NULL
    if (is.character(instruction) && nzchar(instruction[1])) {
      for (s in sides) append_side_note(p, ds, id, s, instruction[1])
    }
    return(start_generate(A, ds, id, sides, instruction))
  }
  if (path == "/api/node/run" && POST) {
    if (!fn_exists("run_stored_sides")) return(need_fn("run_stored_sides"))
    b <- read_body(req)
    ds <- tryCatch(safe_ds(b$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    id <- tryCatch(canonical_node_id(ds, b$node), error = function(e) e)
    if (inherits(id, "error")) return(err_resp(conditionMessage(id), 400L))
    side <- b$side %||% "both"
    if (!nzchar(side)) side <- "both"
    if (!side %in% c("primary", "validation", "both")) return(err_resp("side must be primary, validation, or both."))
    if (node_busy(A, ds, id)) return(err_resp("This variable is already running.", 409L))
    graph <- tryCatch(load_graph(p, ds), error = function(e) NULL)
    if (!is.null(graph) && isTRUE(graph$layered)) {
      node <- graph_node(graph,id)
      if (!is.null(node$paramcd) && !toupper(node$paramcd) %in% toupper(flat_chr(graph$generated_parameters))) return(err_resp("Generate its Variable DAG first.",409L))
    }
    spec <- tryCatch(read_project_spec(p), error = function(e) NULL)
    sides <- if (identical(side, "both")) c("primary", "validation") else side
    missing <- sides[!vapply(sides, function(s) !is.null(read_stored_code(file.path(side_run_dir(p$dir, ds, id, s), "code.R"))), TRUE)]
    if (length(missing)) return(err_resp(paste0("No stored code for ", paste(missing, collapse = " and "), "."), 409L))
    if (exists("coder_session_ensure", mode = "function")) for (s in sides) {
      coder_session_ensure(p$dir, ds, id, s, load_config()$model %||% NULL)
      if (exists("coder_window_event", mode = "function")) coder_window_event(p$dir, ds, id, s, "Program run started")
      unlink(file.path(side_run_dir(p$dir, ds, id, s), "log.txt"))
    }
    ran <- tryCatch(
      run_stored_sides(p$dir, ds, id, side, timeout = run_limit(A), graph = graph, spec = spec),
      error = function(e) e
    )
    if (inherits(ran, "error")) {
      for (s in sides) {
        if (exists("coder_session_set_status",mode="function")) coder_session_set_status(p$dir,ds,id,s,"failed")
        if (exists("coder_window_event", mode = "function")) {
          coder_window_event(p$dir, ds, id, s, "Program run finished")
          coder_window_event(p$dir, ds, id, s, "Run error", conditionMessage(ran))
          coder_window_event(p$dir, ds, id, s, "Run log", read_text(file.path(side_run_dir(p$dir, ds, id, s), "log.txt")))
        }
        if (exists("coder_session_snapshot_run", mode = "function")) coder_session_snapshot_run(p$dir, ds, id, s)
      }
      return(err_resp(conditionMessage(ran), 500L))
    }
    if (!isTRUE(ran$ok)) {
      message <- ran$error %||% "No stored code for that side."
      for (s in sides) {
        if (exists("coder_session_set_status",mode="function")) coder_session_set_status(p$dir,ds,id,s,"failed")
        if (exists("coder_window_event", mode = "function")) {
          coder_window_event(p$dir, ds, id, s, "Program run finished")
          coder_window_event(p$dir, ds, id, s, "Run error", message)
          coder_window_event(p$dir, ds, id, s, "Run log", read_text(file.path(side_run_dir(p$dir, ds, id, s), "log.txt")))
        }
        if (exists("coder_session_snapshot_run", mode = "function")) coder_session_snapshot_run(p$dir, ds, id, s)
      }
      return(err_resp(message, 409L))
    }
    for (s in sides) {
      result <- ran$results[[s]] %||% list(ok = FALSE, error = "No run result.")
      if (exists("coder_session_set_status",mode="function"))
        coder_session_set_status(p$dir,ds,id,s,if(isTRUE(result$ok)) "success" else "failed")
      if (exists("coder_window_event", mode = "function")) {
        coder_window_event(p$dir, ds, id, s, "Program run finished")
        if (!isTRUE(result$ok)) coder_window_event(p$dir, ds, id, s, "Run error", result$error %||% "Program failed")
      }
      log_file <- file.path(side_run_dir(p$dir, ds, id, s), "log.txt")
      if (!file.exists(log_file) && !is.null(result$log)) write_text(paste(as.character(result$log), collapse = "\n"), log_file)
      log_text <- read_text(log_file)
      if (exists("coder_window_event", mode = "function")) coder_window_event(p$dir, ds, id, s, "Run log", log_text)
      if (exists("coder_session_snapshot_run", mode = "function")) coder_session_snapshot_run(p$dir, ds, id, s)
    }
    st <- tryCatch(apply_stored_run(p, ds, id, ran$results), error = function(e) e)
    if (inherits(st, "error")) return(err_resp(conditionMessage(st), 500L))
    return(json_resp(st))
  }
  if (path == "/api/node/compare" && POST) {
    if (!fn_exists("compare_node")) return(need_fn("compare_node"))
    b <- read_body(req)
    ds <- safe_ds(b$dataset)
    id <- canonical_node_id(ds, b$node)
    st <- get_status(p, ds, id)
    if (!identical(st$level, "L2")) return(err_resp("Compare is available after generation.", 409L))
    spec <- read_project_spec(p)
    if (!length(dataset_keys(spec, ds))) return(err_resp("This dataset has no Key.", 400L))
    return(start_compare(A, ds, id))
  }
  if (path == "/api/batch" && POST) {
    miss <- need_fn("generate_side", "run_side", "compare_node")
    if (!is.null(miss)) return(miss)
    if (!llm_ready(A$cfg)) return(err_resp("Choose a model in Settings.", 400L))
    if (batch_running(A)) return(err_resp("A batch is already running.", 409L))
    b <- read_body(req)
    ds <- tryCatch(safe_ds(b$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    raw_ids <- batch_id_text(b$nodes)
    if (!length(raw_ids)) return(err_resp("nodes is required.", 400L))
    if (is.null(load_graph(p, ds))) return(err_resp("No dependency graph yet.", 404L))
    ids <- character()
    for (raw in raw_ids) {
      id <- tryCatch(canonical_node_id(ds, raw), error = function(e) e)
      if (inherits(id, "error")) return(err_resp(conditionMessage(id), 400L))
      ids <- c(ids, id)
    }
    ids <- unique(ids)
    graph <- load_graph(p,ds)
    if (isTRUE(graph$layered)) {
      missing <- unique(vapply(Filter(function(id) { n<-graph_node(graph,id); !is.null(n$paramcd) && !toupper(n$paramcd) %in% toupper(flat_chr(graph$generated_parameters)) }, ids), function(id) graph_node(graph,id)$paramcd, ""))
      if (length(missing)) return(err_resp(paste0("Generate Variable DAGs first for: ",paste(missing,collapse=", ")),409L))
    }
    known <- dataset_node_ids(p, tryCatch(read_project_spec(p), error = function(e) NULL), ds)
    bad <- ids[!ids %in% known]
    if (length(bad)) return(err_resp(paste0("Unknown node: ", paste(bad, collapse = ", ")), 400L))
    compare <- if (is.null(b$compare)) TRUE else isTRUE(b$compare)
    ordered <- if (isTRUE(b$keep_order)) ids else batch_order_for(p, ds, ids)
    return(json_resp(start_batch(A, ds, ordered, compare)))
  }
  if (path == "/api/batch/cancel" && POST) {
    if (!batch_running(A)) return(err_resp("No batch is running.", 409L))
    A$batch$cancelled <- TRUE
    return(json_resp(batch_public(A$batch, A$project$dir)))
  }
  if (path == "/api/node/investigate" && POST) {
    if (!fn_exists("qc_investigate")) return(need_fn("qc_investigate"))
    if (!llm_ready(A$cfg)) return(err_resp("Choose a model in Settings.", 400L))
    b <- read_body(req)
    ds <- safe_ds(b$dataset)
    id <- canonical_node_id(ds, b$node)
    st <- get_status(p, ds, id)
    if (is.null(st$problem) || !identical(st$problem$stage, "compare"))
      return(err_resp("Investigation is available after a mismatch.", 409L))
    jid <- job_start(A, "investigate", ds, id)
    pr <- launch(A, "qc_job", list(project_dir = p$dir, dataset = ds, node_id = id), llm_limit(A))
    return(remember(A, jid, promises::then(pr,
      onFulfilled = function(res) {
        job_finish(A, jid)
        txt <- report_text(res)
        d <- node_run_dir(A$project, ds, id)
        dir.create(d, recursive = TRUE, showWarnings = FALSE)
        write_text(txt, file.path(d, "qc_report.md"))
        json_resp(list(report = txt))
      },
      onRejected = function(e) { job_finish(A, jid, e); err_resp(conditionMessage(e), 502L) })))
  }
  if (path %in% c("/api/node/agent", "/api/node/ask") && POST) {
    miss <- need_fn("agent_decide", "generate_side", "run_side")
    if (!is.null(miss)) return(miss)
    if (!llm_ready(A$cfg)) return(err_resp("Choose a model in Settings.", 400L))
    b <- read_body(req)
    ds <- tryCatch(safe_ds(b$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    id <- tryCatch(canonical_node_id(ds, b$node), error = function(e) e)
    if (inherits(id, "error")) return(err_resp(conditionMessage(id), 400L))
    side <- b$side %||% ""
    if (!side %in% c("primary", "validation")) return(err_resp("side must be primary or validation.", 400L))
    message <- trimws(as.character(b$message %||% b$question %||% "")[1])
    if (!nzchar(message)) return(err_resp("message is required.", 400L))
    graph <- load_graph(p, ds)
    if (is.null(graph)) return(err_resp("No dependency graph yet.", 404L))
    if (node_busy(A, ds, id)) return(err_resp("This variable is already running.", 409L))
    jid <- job_start(A, "agent", ds, id)
    finish_agent <- function(res, status = NULL, compare = NULL, error = NULL) {
      updated <- !is.null(res$change) && is.list(res$run) && !is.null(res$run$code)
      label <- if (is.null(status)) "not run" else if (identical(status$level, "L3")) "match" else if (!is.null(compare) && !isTRUE(compare$match)) "mismatch" else if (!is.null(error)) "failed" else status$level %||% "run"
      sid <- if (exists("coder_session_ensure", mode="function")) coder_session_ensure(p$dir,ds,id,side,load_config()$model %||% NULL) else NULL
      turn <- list(kind = "agent", session=sid, message = message, reply = res$reply,
                   change = res$change, code_updated = updated,
                   note_index = res$note_index, compare_status = if (updated) label else NULL,
                   usage = res$usage)
      expert_append_turn(p$dir, ds, id, side, turn)
      job_finish(A, jid, error)
      json_resp(list(reply = res$reply, change = res$change, model = res$model, usage = res$usage,
                     code_updated = updated, note_index = res$note_index,
                     compare_status = if (updated) label else NULL,
                     status = status, compare = compare))
    }
    pr <- launch(A, "agent_job", list(project_dir = p$dir, dataset = ds, node_id = id,
                                      side = side, message = message, timeout = run_limit(A)),
                 llm_limit(A, extra = run_limit(A) + 30))
    done <- promises::then(pr, onFulfilled = function(res) {
      if (is.null(res$change)) return(finish_agent(res))
      prepare_generate_status(p, ds, id, graph)
      outcome <- res$run
      wrapped <- if (is.list(outcome) && !is.null(outcome$run)) list(ok = TRUE, value = outcome) else
        list(ok = FALSE, error = outcome$error %||% "Generation failed.")
      status <- apply_generate_results(p, ds, id, setNames(list(wrapped), side))
      if (!identical(status$level, "L2")) {
        run_error <- if (isTRUE(wrapped$ok) && isTRUE(wrapped$value$run$ok)) NULL else status$problem$message %||% "Run failed."
        return(finish_agent(res, status, error = run_error))
      }
      cmp <- launch(A, "compare_job", list(project_dir = p$dir, dataset = ds, node_id = id), run_limit(A))
      promises::then(cmp, onFulfilled = function(value) {
        save_compare(p, ds, id, value)
        st <- get_status(p, ds, id)
        if (isTRUE(value$match)) { st$level <- "L3"; st$problem <- NULL }
        else { st$level <- "L2"; st$problem <- list(stage = "compare", message = value$summary %||% "Values do not match.") }
        put_status(p, ds, id, st)
        finish_agent(res, st, value)
      }, onRejected = function(e) {
        st <- get_status(p, ds, id)
        st$problem <- list(stage = "compare", message = conditionMessage(e))
        put_status(p, ds, id, st)
        finish_agent(res, st, error = conditionMessage(e))
      })
    }, onRejected = function(e) { job_finish(A, jid, e); err_resp(conditionMessage(e), 502L) })
    done <- promises::catch(done, function(e) { job_finish(A, jid, e); err_resp(conditionMessage(e), 502L) })
    return(remember(A, jid, done))
  }
  if (path == "/api/expert/clear" && POST) {
    if (!fn_exists("expert_clear")) return(need_fn("expert_clear"))
    b <- read_body(req)
    ds <- tryCatch(safe_ds(b$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    side <- b$side %||% ""
    if (!side %in% c("primary", "validation")) return(err_resp("side must be primary or validation.", 400L))
    expert_clear(p$dir, ds, side)
    if (exists("coder_session_root",mode="function")) {
      root_sessions <- coder_session_root(p$dir)
      if (dir.exists(root_sessions)) for (f in list.files(root_sessions,pattern="^open_.*\\.json$",full.names=TRUE)) {
        x <- read_json(f); m <- if (!is.null(x$id)) coder_session_read(p$dir,x$id) else NULL
        if (is.list(m) && identical(m$dataset,ds) && identical(m$side,side)) coder_session_close(p$dir, m$dataset, m$node, m$side)
      }
    }
    return(json_resp(list(ok = TRUE)))
  }
  if (path == "/api/node/detail" && identical(method, "GET")) {
    ds <- safe_ds(q$dataset)
    id <- canonical_node_id(ds, q$node)
    return(json_resp(node_detail(p, ds, id)))
  }
  if (path == "/api/node/coder-window" && identical(method, "GET")) {
    ds <- tryCatch(safe_ds(q$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    id <- tryCatch(canonical_node_id(ds, q$node), error = function(e) e)
    if (inherits(id, "error")) return(err_resp(conditionMessage(id), 400L))
    side <- q$side %||% ""
    if (!side %in% c("primary", "validation")) return(err_resp("side must be primary or validation.", 400L))
    if (!exists("coder_window_read", mode = "function")) return(need_fn("coder_window_read"))
    return(json_resp(coder_window_read(p, ds, id, side, node_busy(A, ds, id), q$attempt)))
  }
  if (path == "/api/node/su" && POST) {
    b <- read_body(req); ds <- tryCatch(safe_ds(b$dataset),error=function(e)e)
    if(inherits(ds,"error")) return(err_resp(conditionMessage(ds),400L))
    id <- tryCatch(canonical_node_id(ds,b$node),error=function(e)e); if(inherits(id,"error")) return(err_resp(conditionMessage(id),400L))
    side <- as.character(b$side %||% "")[1]; if(!side %in% c("primary","validation")) return(err_resp("side must be primary or validation.",400L))
    if(node_busy(A,ds,id)) return(err_resp("This variable is already running.",409L))
    message <- trimws(as.character(b$message %||% "")[1]); if(!nzchar(message)) return(err_resp("message is required.",400L))
    messages <- b$messages %||% list()
    last_message <- if(length(messages)) as.character(messages[[length(messages)]]$text %||% messages[[length(messages)]]$message %||% "")[1] else ""
    if(!identical(last_message,message)) messages <- c(messages,list(list(role="user",text=message)))
    if(length(messages)>40L) messages <- tail(messages,40L)
    jid <- job_start(A,"su",ds,id)
    pr <- launch(A,"su_job",list(project_dir=p$dir,dataset=ds,node_id=id,side=side,messages=messages,message=message,timeout=run_limit(A)),llm_limit(A,extra=run_limit(A)+30))
    done <- promises::then(pr,onFulfilled=function(ans){
      # SU runs in a child process; refresh the server's in-memory state after its writes.
      A$project$state <- read_state_file(A$project)
      job_finish(A,jid)
      json_resp(ans)
    },onRejected=function(e){
      # The child may have written state before a later round failed or timed out.
      A$project$state <- read_state_file(A$project)
      job_finish(A,jid,e)
      err_resp(conditionMessage(e),502L)
    })
    return(remember(A,jid,done))
  }
  if (path == "/api/sessions" && identical(method,"GET")) return(json_resp(coder_sessions_list(p$dir,q$dataset,q$node,q$side)))
  if (path == "/api/session" && identical(method,"GET")) {
    sid <- as.character(q$id %||% "")[1]
    if (!grepl("^[A-Za-z0-9._-]+$",sid) || grepl("\\.\\.",sid)) return(err_resp("Invalid session id.",400L))
    rec <- coder_session_full(p$dir,sid); if(is.null(rec)) return(err_resp(paste("Unknown session",sid),404L))
    rec$current <- coder_session_current(A,rec)
    return(json_resp(rec))
  }
  if (path == "/api/node/card" && POST) {
    b <- read_body(req)
    ds <- tryCatch(safe_ds(b$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    id <- tryCatch(canonical_node_id(ds, b$node), error = function(e) e)
    if (inherits(id, "error")) return(err_resp(conditionMessage(id), 400L))
    side <- b$side %||% ""
    if (!side %in% c("primary", "validation")) return(err_resp("Invalid side.", 400L))
    edited <- tryCatch(card_edit(p, ds, id, side, b$description, isTRUE(b$revert)), error = function(e) e)
    if (inherits(edited, "error")) return(err_resp(conditionMessage(edited), 400L))
    return(json_resp(card_view(p, ds, id, side)))
  }
  if (path == "/api/node/llm-log" && identical(method, "GET")) {
    ds <- safe_ds(q$dataset)
    id <- canonical_node_id(ds, q$node)
    side <- q$side %||% "primary"
    if (!side %in% c("primary", "validation")) return(err_resp("side must be primary or validation."))
    if (!exists("latest_llm_log", mode = "function")) return(need_fn("latest_llm_log"))
    rec <- latest_llm_log(p$dir, id, side)
    if (is.null(rec)) return(err_resp("No LLM log for this side.", 404L))
    return(json_resp(rec))
  }
  if (path == "/api/qc" && identical(method, "GET")) {
    fn <- fn_pick("qc_list", "list_qc")
    if (is.null(fn)) return(need_fn("qc_list"))
    return(json_resp(tryCatch(fn(), error = function(e) e)))
  }
  if (path == "/api/qc/get" && identical(method, "GET")) {
    fn <- fn_pick("qc_get", "qc_read")
    if (is.null(fn)) return(need_fn("qc_get"))
    ds <- safe_ds(q$dataset)
    cur <- tryCatch(invoke_fn(fn, list(dataset = ds)), error = function(e) e)
    if (inherits(cur, "error")) return(err_resp(conditionMessage(cur), 500L))
    txt <- qc_text(cur)
    if (!nzchar(txt)) return(err_resp("No QC document for this dataset.", 404L))
    return(json_resp(list(markdown = txt)))
  }
  if (path == "/api/qc/upload" && POST) {
    getter <- fn_pick("qc_get", "qc_read")
    putter <- fn_pick("qc_put", "qc_save", "qc_write", "qc_upload")
    if (is.null(putter)) return(need_fn("qc_put"))
    b <- read_body(req)
    ds <- safe_ds(b$dataset)
    raw <- tryCatch(decode_base64(b$base64 %||% ""), error = function(e) e)
    if (inherits(raw, "error")) return(err_resp(conditionMessage(raw), 400L))
    txt <- rawToChar(raw); Encoding(txt) <- "UTF-8"
    if (!is.null(getter) && !isTRUE(b$replace)) {
      cur <- tryCatch(qc_text(invoke_fn(getter, list(dataset = ds))), error = function(e) "")
      if (nzchar(cur)) return(err_resp("QC document already exists.", 409L, exists = TRUE))
    }
    r <- tryCatch(invoke_fn(putter, list(dataset = ds, markdown = txt, text = txt, content = txt,
                                        replace = isTRUE(b$replace), name = b$name %||% paste0(ds, ".md"))),
                  error = function(e) e)
    if (inherits(r, "error")) return(err_resp(conditionMessage(r), 500L))
    if (is.list(r) && isTRUE(r$exists)) return(err_resp("QC document already exists.", 409L, exists = TRUE))
    return(json_resp(list(ok = TRUE)))
  }
  if (path == "/api/template" && identical(method, "GET")) {
    name <- q$name %||% ""
    if (!name %in% TEMPLATES) return(err_resp("Unknown template.", 404L))
    f <- file.path(A$app_dir, "templates", name)
    if (!file.exists(f)) return(err_resp("Template is missing.", 404L))
    ext <- tolower(tools::file_ext(name))
    return(file_resp(read_file_raw(f), name, if (ext %in% names(MIME)) unname(MIME[ext]) else "application/octet-stream"))
  }
  if (path == "/api/export" && POST) {
    if (!fn_exists("export_dataset")) return(need_fn("export_dataset"))
    b <- read_body(req)
    ds <- safe_ds(b$dataset)
    fmt <- b$format %||% ""
    if (!fmt %in% c("xpt", "rds")) return(err_resp("format must be xpt or rds."))
    spec <- read_project_spec(p)
    ids <- dataset_node_ids(p, spec, ds)
    if (!length(ids) || !all(vapply(ids, function(id) identical(get_status(p, ds, id)$level, "L3"), TRUE)))
      return(err_resp("Every variable must be Data matched before export.", 409L))
    jid <- job_start(A, "export", ds, NULL)
    pr <- launch(A, "export_job", list(project_dir = p$dir, dataset = ds, format = fmt), run_limit(A))
    return(remember(A, jid, promises::then(pr,
      onFulfilled = function(res) {
        job_finish(A, jid)
        dest <- tryCatch(export_path_of(res, A$project, ds, fmt), error = function(e) e)
        if (inherits(dest, "error")) return(err_resp(conditionMessage(dest), 500L))
        type <- if (identical(fmt, "rds")) "application/octet-stream" else "application/octet-stream"
        file_resp(read_file_raw(dest), paste0(tolower(ds), ".", fmt), type)
      },
      onRejected = function(e) { job_finish(A, jid, e); err_resp(conditionMessage(e), 502L) })))
  }
  if (path == "/api/datasets" && identical(method, "GET")) {
    if (!fn_exists("list_viewer_datasets")) return(need_fn("list_viewer_datasets"))
    side <- as.character(q$side %||% "primary")[1]
    if (!side %in% c("primary", "validation")) return(err_resp("side must be primary or validation."))
    got <- tryCatch(list_viewer_datasets(p, side = side), error = function(e) e)
    if (inherits(got, "error")) return(err_resp(conditionMessage(got), 500L))
    return(json_resp(got))
  }
  if (path == "/api/datasets/view" && identical(method, "GET")) {
    if (!fn_exists("view_dataset")) return(need_fn("view_dataset"))
    if (is.null(q$id) || !nzchar(as.character(q$id)[1])) return(err_resp("id is required.", 400L))
    got <- tryCatch(
      view_dataset(p, id = q$id, offset = q$offset, limit = q$limit, filter = q$filter, cols = q$cols),
      viewer_error = function(e) e,
      error = function(e) e
    )
    if (inherits(got, "viewer_error")) return(err_resp(conditionMessage(got), got$status %||% 400L))
    if (inherits(got, "error")) return(err_resp(conditionMessage(got), 500L))
    return(json_resp(got))
  }
  if (path == "/api/dataset/code" && identical(method, "GET")) {
    if (!fn_exists("dataset_program")) return(need_fn("dataset_program"))
    ds <- tryCatch(safe_ds(q$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    got <- tryCatch(dataset_program(p, ds), error = function(e) e)
    if (inherits(got, "error")) return(err_resp(conditionMessage(got), 500L))
    return(json_resp(got))
  }
  if (path == "/api/node/inputs" && identical(method, "GET")) {
    if (!fn_exists("node_inputs")) return(need_fn("node_inputs"))
    ds <- tryCatch(safe_ds(q$dataset), error = function(e) e)
    if (inherits(ds, "error")) return(err_resp(conditionMessage(ds), 400L))
    id <- tryCatch(canonical_node_id(ds, q$node), error = function(e) e)
    if (inherits(id, "error")) return(err_resp(conditionMessage(id), 400L))
    got <- tryCatch(
      node_inputs(p, ds, id, page = q$page, size = q$size, only_diff = q$only_diff, filter = q$filter),
      viewer_error = function(e) e,
      error = function(e) e
    )
    if (inherits(got, "viewer_error")) return(err_resp(conditionMessage(got), got$status %||% 400L))
    if (inherits(got, "error")) return(err_resp(conditionMessage(got), 500L))
    return(json_resp(got))
  }
  err_resp("Not found.", 404L)
}

side_notes_path <- function(p, ds, id, side) {
  file.path(node_run_dir(p, ds, id), side, "notes.json")
}

read_side_notes <- function(p, ds, id, side) {
  raw <- read_json(side_notes_path(p, ds, id, side))
  if (is.null(raw) || !length(raw) || !is.list(raw)) return(list())
  # A length-1 JSON array stays a list of notes. A bare object has $text.
  if (!is.null(raw$text) && (is.null(raw[[1]]) || !is.list(raw[[1]]) || is.null(raw[[1]]$text))) {
    return(list(raw))
  }
  Filter(function(n) is.list(n) && !is.null(n$text), raw)
}

append_side_note <- function(p, ds, id, side, text) {
  side <- match.arg(side, c("primary", "validation"))
  text <- trimws(as.character(text %||% "")[1])
  if (!nzchar(text)) return(invisible(list()))
  notes <- read_side_notes(p, ds, id, side)
  notes[[length(notes) + 1L]] <- list(at = now_iso(), text = text)
  if (length(notes) > 20L) notes <- notes[(length(notes) - 19L):length(notes)]
  write_json(notes, side_notes_path(p, ds, id, side))
  invisible(notes)
}

read_code_note <- function(p, ds, id, side) {
  note <- read_json(file.path(node_run_dir(p, ds, id), side, "code_note.json"))
  if (!is.list(note) || is.null(note$text) || !nzchar(as.character(note$text)[1])) return(NULL)
  list(index = note$index %||% NULL, at = note$at %||% NULL, text = as.character(note$text)[1])
}

generate_warnings <- function(p, ds, id, graph = NULL) {
  if (is.null(graph)) graph <- tryCatch(load_graph(p, ds), error = function(e) NULL)
  chk <- tryCatch(run_check(p, read_project_spec(p), graph), error = function(e) list(issues = list()))
  issues <- if (is.null(chk)) list() else as_issue_list(chk$issues %||% list())
  if (!node_is_blocked(id, ds, issues)) return(character())
  msgs <- character()
  for (iss in issues_for_node(issues, ds, id)) {
    if (!isTRUE(issue_blocks(iss))) next
    msg <- as.character(iss$message %||% "")[1]
    if (length(msg) == 1L && !is.na(msg) && nzchar(msg)) msgs <- c(msgs, msg)
  }
  unique(msgs)
}

# none, or a legacy validate-stage problem, stays Not generated. Blocking spec issues are warnings.
# They do not refuse generation and they do not demote descendants.
prepare_generate_status <- function(p, ds, id, graph = NULL) {
  st <- get_status(p, ds, id)
  lvl <- st$level %||% "none"
  if (identical(lvl, "none") || (!is.null(st$problem) && identical(st$problem$stage, "validate"))) {
    st$level <- "none"
    st$problem <- NULL
  }
  msgs <- generate_warnings(p, ds, id, graph)
  st$warnings <- if (length(msgs)) as.list(msgs) else NULL
  st$upstream_changed <- NULL
  put_status(p, ds, id, st, save = FALSE)
  write_state(p)
  get_status(p, ds, id)
}

start_generate <- function(A, ds, id, sides, instruction) {
  p <- A$project
  if (exists("coder_session_start", mode="function")) for (s in sides) coder_session_start(p$dir,ds,id,s,load_config()$model %||% NULL)
  jid <- job_start(A, "generate", ds, id)
  timeout <- llm_limit(A, extra = run_limit(A) + 30)
  prs <- list()
  for (s in sides) {
    prs[[s]] <- settle(launch(A, "run_side_job", list(
      project_dir = p$dir, dataset = ds, node_id = id, side = s,
      instruction = instruction, timeout = run_limit(A)
    ), timeout))
  }
  pr <- promises::then(combine_named(prs), onFulfilled = function(results) {
    failed <- Filter(function(r) is.list(r) && identical(r$ok, FALSE), results)
    job_finish(A, jid, if (length(failed)) paste0(names(failed), ": ",
      vapply(failed, function(r) as.character(r$error %||% "")[1], ""), collapse = " | "))
    tryCatch(json_resp(apply_generate_results(A$project, ds, id, results)), error = function(e) err_resp(conditionMessage(e), 500L))
  }, onRejected = function(e) { job_finish(A, jid, e); err_resp(conditionMessage(e), 502L) })
  remember(A, jid, pr)
}

# Shared by POST /api/node/compare and a batch step. Resolves to an HTTP response.
# A mismatch is 200 with the node left at L2. A thrown compare is 502 and stores the problem.
start_compare <- function(A, ds, id) {
  jid <- job_start(A, "compare", ds, id)
  pr <- launch(A, "compare_job", list(project_dir = A$project$dir, dataset = ds, node_id = id), run_limit(A))
  done <- promises::then(pr,
    onFulfilled = function(res) {
      job_finish(A, jid)
      tryCatch({
        save_compare(A$project, ds, id, res)
        st2 <- get_status(A$project, ds, id)
        if (isTRUE(res$match)) { st2$level <- "L3"; st2$problem <- NULL }
        else { st2$level <- "L2"; st2$problem <- list(stage = "compare", message = res$summary %||% "Values do not match.") }
        put_status(A$project, ds, id, st2)
        json_resp(list(status = st2, compare = res))
      }, error = function(e) err_resp(conditionMessage(e), 500L))
    },
    onRejected = function(e) {
      job_finish(A, jid, e)
      st2 <- tryCatch(get_status(A$project, ds, id), error = function(err) blank_status())
      st2$problem <- list(stage = "compare", message = conditionMessage(e))
      try(put_status(A$project, ds, id, st2), silent = TRUE)
      err_resp(conditionMessage(e), 502L)
    })
  remember(A, jid, done)
}

batch_running <- function(A) is.list(A$batch) && !isTRUE(A$batch$finished)

batch_public <- function(b, project = NULL) {
  if (!is.list(b)) return(NULL)
  step <- b$step
  if (identical(step, "generate") && !is.null(project) && !is.null(b$current)) {
    paths <- vapply(c("primary", "validation"), function(side)
      review_progress_path(project, b$dataset, b$current, side), "")
    if (any(file.exists(paths))) step <- "reviewing"
  }
  list(
    dataset = b$dataset %||% NULL,
    total = as.integer(b$total %||% 0L),
    done = as.integer(b$done %||% 0L),
    current = if (is.null(b$current) || !nzchar(as.character(b$current)[1])) NULL else as.character(b$current)[1],
    step = if (is.null(step) || !nzchar(as.character(step)[1])) NULL else as.character(step)[1],
    cancelled = isTRUE(b$cancelled),
    finished = isTRUE(b$finished),
    results = b$results %||% list()
  )
}

batch_id_text <- function(nodes) {
  if (is.character(nodes)) return(nodes[!is.na(nodes) & nzchar(nodes)])
  if (!is.list(nodes) || !length(nodes)) return(character())
  out <- vapply(nodes, function(x) {
    if (is.null(x) || (length(x) == 1L && is.na(x))) return("")
    if (is.character(x) || is.numeric(x)) return(as.character(x)[1])
    ""
  }, "")
  out[nzchar(out)]
}

# Keys first (spec Key order), then inspect_topo, then any requested id still left.
batch_node_order <- function(ids, key_ids, topo) {
  ids <- unique(as.character(ids %||% character()))
  ids <- ids[!is.na(ids) & nzchar(ids)]
  key_ids <- as.character(key_ids %||% character())
  topo <- as.character(topo %||% character())
  keys <- key_ids[key_ids %in% ids]
  mid <- topo[topo %in% ids & !topo %in% keys]
  rest <- ids[!ids %in% c(keys, mid)]
  c(keys, mid, rest)
}

batch_order_for <- function(p, ds, ids) {
  spec <- tryCatch(read_project_spec(p), error = function(e) NULL)
  keys <- if (is.list(spec)) paste0(ds, ".", dataset_keys(spec, ds)) else character()
  g <- tryCatch(load_graph(p, ds), error = function(e) NULL)
  topo <- if (!is.null(g) && fn_exists("inspect_topo")) inspect_topo(g) else character()
  if(!is.null(g) && any(vapply(g$nodes,function(n)identical(n$kind,"records")&&is.null(n$paramcd),TRUE)))
    keys<-paste0(ds,".RECORDS")
  if (!is.null(g) && isTRUE(g$layered) && exists("param_graph_read", mode="function")) {
    pg <- tryCatch(param_graph_read(p, ds), error=function(e) NULL)
    po_result <- if (!is.null(pg)) param_layer_order(spec, ds, pg) else list(order=character(),cycle=character())
    if (length(po_result$cycle)) stop("Parameter dependency cycle: ", paste(po_result$cycle,collapse=" -> "),call.=FALSE)
    po <- po_result$order
    layered <- unlist(lapply(po, function(pc) {
      members <- vapply(Filter(function(n) identical(n$paramcd, pc) && n$kind == "param_variable", g$nodes), `[[`, "", "id")
      c(paste0(ds, ".RECORDS@", pc), topo[topo %in% members], setdiff(members, topo))
    }), use.names=FALSE)
    dataset_nodes <- vapply(Filter(function(n) identical(n$kind, "dataset_variable"), g$nodes), `[[`, "", "id")
    dataset <- c(topo[topo %in% dataset_nodes],setdiff(dataset_nodes,topo))
    topo <- unique(c(dataset, layered, topo))
  }
  batch_node_order(ids, keys, topo)
}

batch_record <- function(A, id, level, ok, message, compared = FALSE, compare_ok = NULL) {
  b <- A$batch
  if (!is.list(b)) return(invisible())
  b$results[[length(b$results %||% list()) + 1L]] <- list(
    node = as.character(id)[1],
    level = as.character(level %||% "none")[1],
    ok = isTRUE(ok),
    message = as.character(message %||% "")[1],
    compared = isTRUE(compared),
    compare_ok = if (is.null(compare_ok)) NULL else isTRUE(compare_ok)
  )
  b$done <- length(b$results)
  b$current <- NULL
  b$step <- NULL
  b$busy_step <- FALSE
  A$batch <- b
  invisible()
}

batch_blocked_ancestor <- function(p, ds, code, blocked) {
  pg <- tryCatch(param_graph_read(p, ds), error=function(e) NULL)
  if (is.null(pg) || !length(blocked)) return(NULL)
  queue <- toupper(code); seen <- character()
  while (length(queue)) {
    cur <- queue[[1]]; queue <- queue[-1L]
    if (cur %in% names(blocked)) return(blocked[[cur]])
    if (cur %in% seen) next
    seen <- c(seen,cur)
    queue <- c(queue,vapply(Filter(function(e) toupper(e$to)==cur,pg$edges %||% list()),function(e)toupper(e$from),""))
  }
  NULL
}

batch_abort <- function(A, e) {
  b <- A$batch
  if (!is.list(b) || isTRUE(b$finished)) return(invisible())
  msg <- tryCatch(conditionMessage(e), error = function(err) "Batch failed.")
  if (isTRUE(b$busy_step) && !is.null(b$current) && nzchar(as.character(b$current)[1])) {
    batch_record(A, b$current, "none", FALSE, msg)
    b <- A$batch
  }
  b$finished <- TRUE
  b$current <- NULL
  b$step <- NULL
  b$busy_step <- FALSE
  A$batch <- b
  invisible()
}

batch_continue <- function(A) {
  b <- A$batch
  if (!is.list(b) || isTRUE(b$finished)) return(invisible())
  if (isTRUE(b$cancelled) || b$index > length(b$order)) {
    b$finished <- TRUE
    b$current <- NULL
    b$step <- NULL
    b$busy_step <- FALSE
    A$batch <- b
    return(invisible())
  }
  b$step_token <- (b$step_token %||% 0L) + 1L
  token <- b$step_token
  A$batch <- b
  later::later(function() {
    if (!is.list(A$batch) || !identical(A$batch$step_token, token)) return()
    tryCatch(batch_advance(A), error = function(e) batch_abort(A, e))
  }, 0)
  invisible()
}

batch_after_compare <- function(A, id, resp) {
  b <- A$batch
  if (!is.list(b) || isTRUE(b$finished) || !identical(b$current, id)) return(invisible())
  st <- tryCatch(get_status(A$project, b$dataset, id), error = function(e) NULL)
  level <- if (is.list(st)) as.character(st$level %||% "none")[1] else "none"
  http_ok <- is.list(resp) && identical(as.integer(resp$status %||% 0L), 200L)
  msg <- "Compared"
  if (is.list(st) && is.list(st$problem) && nzchar(as.character(st$problem$message %||% "")[1])) {
    msg <- as.character(st$problem$message)[1]
  } else if (identical(level, "L3")) msg <- "Exact match"
  if (!http_ok && identical(msg, "Compared")) msg <- "Compare failed."
  ok <- http_ok && identical(level, "L3")
  batch_record(A, id, level, ok, msg, compared = TRUE, compare_ok = http_ok)
  batch_continue(A)
}

batch_after_generate <- function(A, id, resp) {
  b <- A$batch
  if (!is.list(b) || isTRUE(b$finished) || !identical(b$current, id)) return(invisible())
  st <- tryCatch(get_status(A$project, b$dataset, id), error = function(e) NULL)
  level <- if (is.list(st)) as.character(st$level %||% "none")[1] else "none"
  problem <- if (is.list(st)) st$problem else list(message = "Generate failed.")
  http_ok <- is.list(resp) && identical(as.integer(resp$status %||% 0L), 200L)
  clean_l2 <- http_ok && identical(level, "L2") && is.null(problem)
  if (isTRUE(b$compare) && clean_l2) {
    spec <- tryCatch(read_project_spec(A$project), error = function(e) NULL)
    if (!length(dataset_keys(spec, b$dataset))) {
      batch_record(A, id, level, FALSE, "This dataset has no Key.")
      batch_continue(A)
      return(invisible())
    }
    b$step <- "compare"
    b$current <- id
    A$batch <- b
    pr <- tryCatch(start_compare(A, b$dataset, id), error = function(e) e)
    if (inherits(pr, "error") || !inherits(pr, "promise")) {
      msg <- if (inherits(pr, "error")) conditionMessage(pr) else "Compare did not start."
      batch_record(A, id, level, FALSE, msg)
      batch_continue(A)
      return(invisible())
    }
    promises::then(pr,
      onFulfilled = function(cmp) {
        tryCatch(batch_after_compare(A, id, cmp), error = function(e) batch_abort(A, e))
      },
      onRejected = function(e) {
        tryCatch({
          st2 <- tryCatch(get_status(A$project, A$batch$dataset, id), error = function(err) NULL)
          lvl <- if (is.list(st2)) as.character(st2$level %||% level)[1] else level
          batch_record(A, id, lvl, FALSE, conditionMessage(e))
          batch_continue(A)
        }, error = function(e2) batch_abort(A, e2))
      })
    return(invisible())
  }
  ok <- http_ok && is.null(problem) && level %in% c("L2", "L3")
  msg <- if (is.list(problem) && nzchar(as.character(problem$message %||% "")[1])) as.character(problem$message)[1] else "Generated"
  if (!http_ok && !nzchar(msg %||% "")) msg <- "Generate failed."
  if (!http_ok && identical(msg, "Generated")) msg <- "Generate failed."
  if (isTRUE(A$batch$layered) && grepl(" is blocked by unfinished layer ",msg,fixed=TRUE)) {
    pc <- toupper(parse_node_id(id)$paramcd %||% "")
    if (nzchar(pc)) A$batch$blocked_layers[[pc]] <- msg
    batch_record(A,paste0("@layer:",pc),"none",FALSE,msg)
    # Suppress the rest of this layer's nodes in batch results while keeping progress accurate.
    remaining_ids <- if (A$batch$index <= length(A$batch$order)) A$batch$order[A$batch$index:length(A$batch$order)] else character()
    siblings <- if (length(remaining_ids)) sum(vapply(remaining_ids, function(next_id)
      identical(toupper(parse_node_id(next_id)$paramcd %||% ""),pc), TRUE)) else 0L
    A$batch$done <- A$batch$done + siblings
    A$batch$index <- A$batch$index + siblings
    batch_continue(A)
    return(invisible())
  }
  batch_record(A, id, level, ok, msg)
  batch_continue(A)
}

batch_advance <- function(A) {
  b <- A$batch
  if (!is.list(b) || isTRUE(b$finished) || isTRUE(b$busy_step)) return(invisible())
  if (isTRUE(b$cancelled) || b$index > length(b$order)) {
    b$finished <- TRUE
    b$current <- NULL
    b$step <- NULL
    A$batch <- b
    return(invisible())
  }
  id <- b$order[[b$index]]
  if (isTRUE(b$layered) && !is.null(parse_node_id(id)$paramcd)) {
    pc <- toupper(parse_node_id(id)$paramcd)
    node <- tryCatch(graph_node(load_graph(A$project,b$dataset),id),error=function(e)NULL)
    spec <- tryCatch(load_project_spec(A$project),error=function(e)NULL)
    if (!is.null(node)) {
      pg <- tryCatch(param_graph_read(A$project,b$dataset),error=function(e)NULL)
      blocked <- NULL
      for (side in c("primary","validation")) {
        blocked <- param_layer_block_message(A$project,b$dataset,side,pc,spec,pg)
        if (!is.null(blocked)) break
      }
      if (!is.null(blocked)) {
        b$blocked_layers[[pc]] <- blocked
        remaining_ids <- b$order[b$index:length(b$order)]
        layer_count <- sum(vapply(remaining_ids,function(next_id)
          identical(toupper(parse_node_id(next_id)$paramcd %||% ""),pc),TRUE))
        batch_record(A,paste0("@layer:",pc),"none",FALSE,blocked)
        b <- A$batch
        b$done <- b$done + max(0L,layer_count-1L)
        b$index <- b$index + layer_count
        A$batch <- b
        batch_continue(A)
        return(invisible())
      }
    }
    cause <- batch_blocked_ancestor(A$project, b$dataset, pc, b$blocked_layers %||% list())
    if (!is.null(cause)) {
      b$index <- b$index + 1L
      A$batch <- b
      if (!pc %in% names(b$reported_blocked_layers %||% list())) {
        b$reported_blocked_layers[[pc]] <- TRUE
        A$batch <- b
        batch_record(A,paste0("@layer:",pc),"none",FALSE,cause)
      } else {
        b <- A$batch; b$done <- b$done + 1L; A$batch <- b
      }
      batch_continue(A)
      return(invisible())
    }
  }
  b$index <- b$index + 1L
  b$busy_step <- TRUE
  b$current <- id
  b$step <- "generate"
  A$batch <- b
  if (node_busy(A, b$dataset, id)) {
    st <- tryCatch(get_status(A$project, b$dataset, id), error = function(e) NULL)
    lvl <- if (is.list(st)) st$level %||% "none" else "none"
    batch_record(A, id, lvl, FALSE, "skipped: running")
    batch_continue(A)
    return(invisible())
  }
  prep <- tryCatch(prepare_generate_status(A$project, b$dataset, id), error = function(e) e)
  if (inherits(prep, "error")) {
    batch_record(A, id, "none", FALSE, conditionMessage(prep))
    batch_continue(A)
    return(invisible())
  }
  pr <- tryCatch(start_generate(A, b$dataset, id, c("primary", "validation"), NULL), error = function(e) e)
  if (inherits(pr, "error") || !inherits(pr, "promise")) {
    msg <- if (inherits(pr, "error")) conditionMessage(pr) else "Generate did not start."
    batch_record(A, id, "none", FALSE, msg)
    batch_continue(A)
    return(invisible())
  }
  promises::then(pr,
    onFulfilled = function(resp) {
      tryCatch(batch_after_generate(A, id, resp), error = function(e) batch_abort(A, e))
    },
    onRejected = function(e) {
      tryCatch({
        st <- tryCatch(get_status(A$project, A$batch$dataset, id), error = function(err) NULL)
        lvl <- if (is.list(st)) st$level %||% "none" else "none"
        batch_record(A, id, lvl, FALSE, conditionMessage(e))
        batch_continue(A)
      }, error = function(e2) batch_abort(A, e2))
    })
  invisible()
}

start_batch <- function(A, ds, ids, compare) {
  graph <- tryCatch(load_graph(A$project,ds),error=function(e)NULL)
  A$batch <- list(
    dataset = ds,
    total = length(ids),
    done = 0L,
    current = NULL,
    step = NULL,
    cancelled = FALSE,
    finished = FALSE,
    results = list(),
    compare = isTRUE(compare),
    order = ids,
    layered = isTRUE(graph$layered),
    blocked_layers = list(),
    reported_blocked_layers = list(),
    index = 1L,
    busy_step = FALSE,
    step_token = 0L
  )
  later::later(function() {
    tryCatch(batch_advance(A), error = function(e) batch_abort(A, e))
  }, 0)
  batch_public(A$batch, A$project$dir)
}

node_detail <- function(p, ds, id) {
  spec <- tryCatch(read_project_spec(p), error = function(e) NULL)
  st <- normalize_missing_column_status(get_status(p, ds, id))
  st <- attach_lineage_warnings(p, ds, id, st, "primary")
  acts <- node_actions(st)
  if (fn_exists("actions")) {
    probe <- list(id = id, level = st$level %||% "none", problem = st$problem)
    alt <- tryCatch(actions(probe), error = function(e) NULL)
    if (is.character(alt) || is.list(alt)) acts <- unique(as.character(unlist(alt, use.names = FALSE)))
  }
  base <- node_run_dir(p, ds, id)
  read_side <- function(side) read_text(file.path(base, side, "code.R"))
  read_log <- function(side) read_text(file.path(base, side, "log.txt"))
  rows <- list()
  parsed <- split_node_id(id)
  if (!is.null(spec) && !is.null(parsed) && nrow(spec$variables)) {
    v <- spec$variables
    hit <- which(v$Dataset == ds & v$Variable == parsed$var)
    for (i in hit) rows[[length(rows) + 1L]] <- as.list(v[i, , drop = FALSE])
    if (!is.null(parsed$paramcd) && nrow(spec$parameters)) {
      a <- spec$parameters
      ph <- which(a$Dataset == ds & a$PARAMCD == parsed$paramcd)
      for (i in ph) rows[[length(rows) + 1L]] <- as.list(a[i, , drop = FALSE])
    }
  }
  list(
    status = st, actions = as.list(acts), ready = node_ready(st, acts),
    code = list(primary = read_side("primary"), validation = read_side("validation")),
    su_edited = list(primary = isTRUE(read_json(su_edited_path(p$dir,ds,id,"primary"))$su_edited), validation = isTRUE(read_json(su_edited_path(p$dir,ds,id,"validation"))$su_edited)),
    log = list(primary = read_log("primary"), validation = read_log("validation")),
    compare = read_json(file.path(base, "compare.json")),
    report = read_text(file.path(base, "qc_report.md")),
    notes = list(
      primary = read_side_notes(p, ds, id, "primary"),
      validation = read_side_notes(p, ds, id, "validation")
    ),
    code_note = list(
      primary = read_code_note(p, ds, id, "primary"),
      validation = read_code_note(p, ds, id, "validation")
    ),
    experts = list(
      primary = if (exists("expert_read", mode = "function")) tryCatch(expert_read(p, ds, "primary"), error = function(e) list()) else list(),
      validation = if (exists("expert_read", mode = "function")) tryCatch(expert_read(p, ds, "validation"), error = function(e) list()) else list()
    ),
    spec_rows = rows,
    cards = list(primary = card_view(p, ds, id, "primary", spec, status = st),
                 validation = card_view(p, ds, id, "validation", spec, status = st)),
    reviews = list(primary = review_read(p, ds, id, "primary"),
                   validation = review_read(p, ds, id, "validation"))
  )
}

make_handlers <- function(A) {
  list(
    call = function(req) {
      path <- req$PATH_INFO %||% "/"
      method <- req$REQUEST_METHOD %||% "GET"
      if (!startsWith(path, "/api/")) {
        if (!(req$HTTP_HOST %||% "") %in% A$allowed_hosts)
          return(list(status = 403L, headers = list(`Content-Type` = "text/plain; charset=utf-8"), body = "Forbidden host"))
        if (!method %in% c("GET", "HEAD"))
          return(list(status = 405L, headers = list(`Content-Type` = "text/plain; charset=utf-8"), body = "Method not allowed"))
        return(serve_static(A, path))
      }
      q <- parse_query(req$QUERY_STRING)
      bad <- check_request(A, req, req$HTTP_X_DUALDAG_TOKEN %||% "")
      if (!is.null(bad)) return(err_resp(paste0("Forbidden (", bad, "). Open the URL printed by run.R."), 403L))
      if (method == "POST" && !grepl("^application/json", req$CONTENT_TYPE %||% ""))
        return(err_resp("Expected application/json.", 415L))
      if (!method %in% c("GET", "POST", "HEAD")) return(err_resp("Method not allowed.", 405L))
      tryCatch(handle_api(A, req, path, method, q), error = function(e) err_resp(conditionMessage(e), 500L))
    }
  )
}

start_server <- function(A, port) {
  loadNamespace("promises")
  loadNamespace("later")
  loadNamespace("httpuv")
  tries <- c(as.integer(port) + 0:19, httpuv::randomPort())
  for (pt in tries) {
    set_port(A, pt)
    s <- tryCatch(httpuv::startServer("127.0.0.1", pt, make_handlers(A)), error = function(e) NULL)
    if (!is.null(s)) return(s)
  }
  stop("Could not find a free port.", call. = FALSE)
}

shutdown_app <- function(A) {
  if (is.null(A)) return(invisible())
  for (id in ls(A$procs)) try(A$procs[[id]]$kill_tree(), silent = TRUE)
  if (!is.null(A$admin_procs)) for (id in ls(A$admin_procs)) try(A$admin_procs[[id]]$kill_tree(), silent = TRUE)
  invisible(TRUE)
}
