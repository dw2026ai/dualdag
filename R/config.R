# Local configuration: ~/.dualdag/config.json (no secrets) and ~/.dualdag/api_key (mode 600).
# DUALDAG_HOME overrides the folder (used by tests). DUALDAG_LLM_API_KEY overrides the stored key.

SESSION_KEEP_HOURS <- 168

config_dir <- function() {
  d <- Sys.getenv("DUALDAG_HOME")
  if (!nzchar(d)) d <- file.path(path.expand("~"), ".dualdag")
  d
}

config_defaults <- function() {
  list(
    provider = "openai", base_url = "", model = "",
    # Plans and checks the DAG extraction. Blank uses `model`.
    strong_model = "",
    timeout_run_s = 600, llm_timeout_s = 180,
    port = 7979L, project = "",
    # TRUE: claude_cli / codex_cli drop API-key env vars so the CLI login is used.
    cli_use_login = TRUE
  )
}

config_allowed_providers <- function() c("openai", "anthropic", "openai_compatible", "grok_cli", "claude_cli", "codex_cli")

config_default_model <- function(provider) switch(as.character(provider %||% "")[1],
  grok_cli = "grok-4.7", claude_cli = "claude-sonnet-5-5", "")

config_apply_model_defaults <- function(cfg) {
  if (!nzchar(as.character(cfg$model %||% "")[1])) {
    default <- config_default_model(cfg$provider)
    if (nzchar(default)) cfg$model <- default
  }
  if (cfg$provider %in% c("grok_cli", "claude_cli", "codex_cli")) cfg$cli_use_login <- TRUE
  cfg
}

config_set_model <- function(cfg, provider, model = "") {
  provider <- as.character(provider %||% "")[1]
  if (!provider %in% config_allowed_providers()) stop("Unknown provider.", call. = FALSE)
  cfg$provider <- provider
  model <- trimws(as.character(model %||% "")[1])
  cfg$model <- if (nzchar(model)) model else config_default_model(provider)
  config_apply_model_defaults(cfg)
}

# Missing or blank stays TRUE. Only an explicit false turns the login preference off.
cli_prefers_login <- function(cfg) {
  if (is.list(cfg) && (cfg$provider %||% "") %in% c("claude_cli", "codex_cli", "grok_cli")) return(TRUE)
  v <- if (is.list(cfg)) cfg$cli_use_login else NULL
  if (is.null(v) || length(v) == 0L) return(TRUE)
  v <- v[[1]]
  if (is.logical(v)) return(isTRUE(v))
  if (is.numeric(v) && length(v) == 1L && !is.na(v)) return(!identical(as.numeric(v), 0))
  s <- tolower(trimws(as.character(v)[1]))
  if (!nzchar(s)) return(TRUE)
  !s %in% c("false", "0", "no")
}

provider_default_base_url <- function(provider) {
  switch(provider %||% "openai",
    openai = "https://api.openai.com/v1",
    anthropic = "https://api.anthropic.com",
    openai_compatible = "",
    "")
}

load_config <- function() {
  cfg <- config_defaults()
  f <- file.path(config_dir(), "config.json")
  if (file.exists(f)) {
    saved <- tryCatch(jsonlite::fromJSON(f, simplifyVector = FALSE), error = function(e) list())
    for (k in intersect(names(saved), names(cfg))) cfg[[k]] <- saved[[k]]
    if (cfg$provider %in% c("claude_cli", "codex_cli", "grok_cli")) cfg$cli_use_login <- TRUE
  }
  cfg$api_key <- NULL
  cfg
}

save_config <- function(cfg) {
  d <- config_dir()
  dir.create(d, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  Sys.chmod(d, mode = "0700", use_umask = FALSE)
  keep <- cfg[intersect(names(cfg), names(config_defaults()))]
  if (keep$provider %in% c("claude_cli", "codex_cli", "grok_cli")) keep$cli_use_login <- TRUE
  keep$api_key <- NULL
  write_text(as.character(jsonlite::toJSON(keep, auto_unbox = TRUE, pretty = TRUE, null = "null")), file.path(d, "config.json"))
  invisible(keep)
}

api_key_file <- function() file.path(config_dir(), "api_key")

get_api_key <- function() {
  k <- Sys.getenv("DUALDAG_LLM_API_KEY")
  if (nzchar(k)) return(list(key = k, source = "env"))
  f <- api_key_file()
  if (file.exists(f)) {
    k <- trimws(paste(readLines(f, warn = FALSE), collapse = ""))
    if (nzchar(k)) return(list(key = k, source = "file"))
  }
  list(key = "", source = "none")
}

save_api_key <- function(key) {
  d <- config_dir()
  dir.create(d, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  Sys.chmod(d, mode = "0700", use_umask = FALSE)
  f <- api_key_file()
  if (!nzchar(key %||% "")) { unlink(f); return(invisible(FALSE)) }
  old <- Sys.umask("077"); on.exit(Sys.umask(old), add = TRUE)
  file.create(f, showWarnings = FALSE)
  Sys.chmod(f, mode = "0600", use_umask = FALSE)
  writeLines(key, f)
  Sys.chmod(f, mode = "0600", use_umask = FALSE)
  invisible(TRUE)
}

config_public <- function(cfg) {
  k <- get_api_key()
  pub <- cfg[intersect(names(cfg), names(config_defaults()))]
  pub$api_key <- NULL
  c(pub, list(
    provider_saved = file.exists(file.path(config_dir(), "config.json")),
    api_key_set = nzchar(k$key),
    api_key_source = k$source,
    api_key_hint = if (nzchar(k$key)) paste0("\u2026", substr(k$key, nchar(k$key) - 3, nchar(k$key))) else ""
  ))
}

llm_ready <- function(cfg = NULL) {
  if (identical(Sys.getenv("DUALDAG_FAKE_LLM"), "1")) return(TRUE)
  if (is.null(cfg)) cfg <- tryCatch(load_config(), error = function(e) list())
  prov <- if (is.list(cfg)) as.character(cfg$provider %||% "")[1] else ""
  if (prov %in% c("grok_cli", "claude_cli", "codex_cli")) return(TRUE)
  nzchar(get_api_key()$key)
}
