# Extract a dependency DAG from one dataset's spec without opening the app.
#   Rscript tools/extract_dag.R --spec <dir> --out <new-empty-dir> [--dataset ADVS]
#     [--provider claude_cli] [--model <normal model>] [--strong <strong model>]
# Reads variable.csv and parameter.csv from <dir>. Writes only into the out folder, never into a project.
# Without --provider and --model it uses the model chosen in Settings.

parse_args <- function(argv) {
  out <- list()
  i <- 1L
  while (i <= length(argv)) {
    if (!startsWith(argv[[i]], "--") || i == length(argv)) stop("Bad argument: ", argv[[i]], call. = FALSE)
    out[[sub("^--", "", argv[[i]])]] <- argv[[i + 1L]]
    i <- i + 2L
  }
  if (is.null(out$spec) || is.null(out$out)) stop("Pass --spec and --out.", call. = FALSE)
  out
}

main <- function() {
  a <- parse_args(commandArgs(TRUE))
  flag <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  repo <- dirname(dirname(normalizePath(sub("^--file=", "", flag[[1]]))))
  for (f in c("utils", "config", "llm", "spec", "dag", "deps", "deps_extract")) sys.source(file.path(repo, "R", paste0(f, ".R")), envir = globalenv())
  if (dir.exists(a$out) && length(list.files(a$out, all.files = TRUE, no.. = TRUE))) stop("Output folder is not empty: ", a$out, call. = FALSE)
  variable_path <- file.path(a$spec, "variable.csv")
  if (!file.exists(variable_path)) stop("No variable.csv in ", a$spec, call. = FALSE)
  parameter_path <- file.path(a$spec, "parameter.csv")
  sp <- read_spec(variable_path, if (file.exists(parameter_path)) parameter_path else NULL)
  ds <- toupper(a$dataset %||% "")
  if (!nzchar(ds)) {
    if (length(sp$datasets) != 1L) stop("Pass --dataset. Found: ", paste(sp$datasets, collapse = ", "), call. = FALSE)
    ds <- toupper(sp$datasets[[1]])
  } else if (!ds %in% toupper(sp$datasets)) stop("Dataset ", ds, " is not in the spec. Found: ", paste(sp$datasets, collapse = ", "), call. = FALSE)
  cfg <- load_config()
  if (!is.null(a$provider)) cfg <- config_set_model(cfg, a$provider, a$model %||% "") else if (!is.null(a$model)) cfg$model <- a$model
  if (!is.null(a$strong)) cfg$strong_model <- a$strong
  if (!cfg$provider %in% c("claude_cli", "codex_cli", "grok_cli")) cfg$api_key <- get_api_key()$key
  start <- Sys.time()
  g <- extract_dependencies(cfg, sp, ds)
  dir.create(a$out, recursive = TRUE, showWarnings = FALSE)
  g$spec_hash <- sp$hash
  extraction <- g$extraction; g$extraction <- NULL
  write_json(g, file.path(a$out, paste0("graph_", ds, ".json")))
  write_json(extraction, file.path(a$out, paste0("extract_", ds, ".json")))
  cycles <- Filter(function(i) identical(i$type, "cycle"), check_graph(g, sp)$issues)
  cat("dataset", ds, "| seconds", round(as.numeric(difftime(Sys.time(), start, units = "secs"))),
      "| nodes", length(g$nodes), "| edges", length(g$edges), "| cycle cuts", length(g$cycle_cuts), "| cycles left", length(cycles), "\n")
  for (w in g$warnings) message(w)
  if (length(cycles)) quit(status = 1L, save = "no")
}

if (sys.nframe() == 0L) main()
