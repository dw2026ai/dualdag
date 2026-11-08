# Small helpers shared by the server modules.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

empty_named <- function() setNames(list(), character())

to_json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA, force = TRUE)

from_json <- function(txt) {
  if (is.raw(txt)) txt <- rawToChar(txt)
  if (!length(txt) || !nzchar(txt)) return(list())
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}

read_text <- function(path) {
  if (!file.exists(path)) return(NULL)
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(x)) x <- sub("\r$", "", x)
  paste(x, collapse = "\n")
}

write_text <- function(text, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(paste0(text %||% "", if (endsWith(text %||% "", "\n")) "" else "\n"))), con)
  invisible(path)
}

write_bytes <- function(bytes, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, open = "wb")
  on.exit(close(con))
  if (length(bytes)) writeBin(as.raw(bytes), con)
  invisible(path)
}

write_json <- function(x, path) {
  txt <- jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE, digits = NA, force = TRUE)
  write_text(as.character(txt), path)
}

read_json <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(from_json(read_text(path)), error = function(e) NULL)
}

now_iso <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

random_token <- function(n = 32) {
  bytes <- tryCatch(openssl::rand_bytes(n), error = function(e) as.raw(sample.int(256, n, replace = TRUE) - 1L))
  paste(sprintf("%02x", as.integer(bytes)), collapse = "")
}

file_md5 <- function(path) {
  if (!is.character(path) || length(path) != 1 || !file.exists(path) || dir.exists(path)) return(NULL)
  con <- file(path, open = "rb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  paste(as.character(openssl::md5(con)))
}

decode_base64 <- function(x) {
  if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(x)) stop("Expected base64 content.", call. = FALSE)
  x <- gsub("\\s+", "", x)
  x <- sub("^data:[^,]*,", "", x)
  jsonlite::base64_dec(x)
}

# Issue object used by spec checks and the report table (F8).
make_issue <- function(type, message, dataset = NULL, variables = character(),
                       file = NULL, row = NULL, text = NULL, suggestion = NULL,
                       severity = NULL) {
  if (is.null(variables)) variables <- character()
  # A missing severity is an error, except an orphan, which is only a warning.
  if (is.null(severity) || !length(severity) || !nzchar(as.character(severity)[1])) {
    severity <- if (identical(as.character(type)[1], "orphan")) "warning" else "error"
  }
  list(
    type = type, dataset = dataset, variables = as.list(variables),
    file = file, row = if (is.null(row) || (length(row) == 1 && is.na(row))) NULL else as.integer(row),
    text = text, message = message, suggestion = suggestion,
    severity = as.character(severity)[1]
  )
}

# ---- path validation (project and www file access goes through this) ----
safe_path <- function(root, rel, must_exist = FALSE) {
  if (!is.character(rel) || length(rel) != 1L || is.na(rel) || !nzchar(rel)) stop("Invalid path.", call. = FALSE)
  if (grepl("[\001-\037]", rel)) stop("Invalid characters in path.", call. = FALSE)
  r <- gsub("\\\\", "/", rel)
  if (grepl("^(/|~|[A-Za-z]:)", r)) stop("Absolute paths are not allowed: ", rel, call. = FALSE)
  parts <- strsplit(r, "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts) & parts != "."]
  if (any(parts == "..")) stop("'..' is not allowed in paths: ", rel, call. = FALSE)
  if (!length(parts)) stop("Invalid path.", call. = FALSE)
  root_n <- normalizePath(root, winslash = "/", mustWork = TRUE)
  cur <- root_n
  i <- 0L
  while (i < length(parts)) {
    nxt <- file.path(cur, parts[i + 1L])
    lk <- Sys.readlink(nxt)
    is_link <- !is.na(lk) && nzchar(lk)
    if (is_link && !file.exists(nxt)) stop("Broken symbolic link in path: ", rel, call. = FALSE)
    if (!file.exists(nxt)) break
    cur <- normalizePath(nxt, winslash = "/", mustWork = FALSE)
    i <- i + 1L
    if (!path_within(cur, root_n)) stop("Path escapes the project folder: ", rel, call. = FALSE)
  }
  if (must_exist && i < length(parts)) stop("File not found: ", rel, call. = FALSE)
  if (i < length(parts)) do.call(file.path, as.list(c(cur, parts[(i + 1L):length(parts)]))) else cur
}

path_within <- function(p, root) {
  if (.Platform$OS.type == "windows") { p <- tolower(p); root <- tolower(root) }
  identical(p, root) || startsWith(p, paste0(sub("/$", "", root), "/"))
}
