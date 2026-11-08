# Source variable catalog. One JSON file of every uploaded xpt's names, labels,
# and distinct code values. Later checks and prompts read this file instead of
# scanning the xpt again.
#
# <project>/.dualdag/state/source_catalog.json
# Rebuilt only for files whose name, size, mtime, or md5 changed.

.catalog_state <- new.env(parent = emptyenv())
.catalog_state$reads <- 0L

# How many xpt files have been read into a data.frame during this process.
# Unchanged files do not increment it.
catalog_xpt_reads <- function() .catalog_state$reads

catalog_file <- function(project) {
  file.path(catalog_project_dir(project), ".dualdag", "state", "source_catalog.json")
}

catalog_project_dir <- function(project) {
  if (is.character(project) && length(project) == 1L && !is.na(project)) return(project)
  if (is.environment(project) && is.character(project$dir) && length(project$dir) == 1L) return(project$dir)
  if (is.list(project) && is.character(project$dir %||% NULL) && length(project$dir) == 1L) return(project$dir)
  stop("A project directory is required.", call. = FALSE)
}

# Code columns whose distinct values are stored (max 500).
catalog_code_column <- function(name) {
  n <- toupper(as.character(name)[1] %||% "")
  if (!nzchar(n)) return(FALSE)
  if (n %in% c("VISIT", "VISITNUM", "AVISIT", "AVISITN", "EPOCH", "DSDECOD", "DSTERM")) return(TRUE)
  grepl("(TESTCD|PARAMCD|QNAM|SCAT|CAT)$", n)
}

catalog_col_type <- function(x) {
  if (inherits(x, "POSIXt")) return("datetime")
  if (inherits(x, "Date")) return("date")
  if (is.character(x) || is.factor(x)) return("character")
  if (is.integer(x)) return("integer")
  if (is.numeric(x)) return("numeric")
  typeof(x)
}

catalog_col_length <- function(x) {
  w <- attr(x, "width", exact = TRUE)
  if (is.numeric(w) && length(w) == 1L && !is.na(w)) return(as.integer(w))
  fmt <- attr(x, "format.sas", exact = TRUE)
  if (is.character(fmt) && length(fmt) == 1L && grepl("^[0-9]+$", fmt)) return(as.integer(fmt))
  if (is.character(x) || is.factor(x)) {
    n <- nchar(as.character(x), keepNA = TRUE)
    n <- n[!is.na(n)]
    if (length(n)) return(as.integer(max(n)))
    return(NA_integer_)
  }
  8L
}

catalog_file_sig <- function(path) {
  info <- file.info(path)
  md5 <- unname(tools::md5sum(path))
  list(
    file = basename(path),
    size = as.numeric(info$size),
    mtime = format(info$mtime, "%Y-%m-%dT%H:%M:%S%z"),
    md5 = as.character(md5)[1]
  )
}

catalog_sig_match <- function(sig, stored) {
  if (is.null(stored)) return(FALSE)
  identical(as.character(stored$file %||% ""), sig$file) &&
    identical(as.numeric(stored$size %||% NA), sig$size) &&
    identical(as.character(stored$mtime %||% ""), sig$mtime) &&
    identical(tolower(as.character(stored$md5 %||% "")), tolower(sig$md5))
}

catalog_read_domain <- function(path, sig) {
  df <- haven::read_xpt(path)
  .catalog_state$reads <- .catalog_state$reads + 1L
  variables <- list()
  values <- empty_named()
  for (nm in names(df)) {
    col <- df[[nm]]
    lab <- attr(col, "label", exact = TRUE)
    lab <- if (is.null(lab) || !length(lab) || is.na(lab[[1]]) || !nzchar(as.character(lab[[1]]))) "" else as.character(lab[[1]])
    variables[[length(variables) + 1L]] <- list(
      name = nm,
      label = lab,
      type = catalog_col_type(col),
      length = catalog_col_length(col)
    )
    if (!catalog_code_column(nm)) next
    vv <- unique(as.character(col))
    vv <- vv[!is.na(vv)]
    vv <- sort(vv)
    if (length(vv) > 500L) vv <- vv[seq_len(500L)]
    # A length-1 vector would collapse to a JSON string under auto_unbox.
    values[[nm]] <- as.list(vv)
  }
  lab <- attr(df, "label", exact = TRUE)
  lab <- if (is.null(lab) || !length(lab) || is.na(lab[[1]])) "" else as.character(lab[[1]])
  c(sig, list(nrow = nrow(df), label = lab, variables = variables, values = values))
}

catalog_load <- function(project) {
  path <- catalog_file(project)
  if (!file.exists(path)) return(NULL)
  got <- read_json(path)
  if (!is.list(got)) return(NULL)
  if (is.null(got$domains) || !is.list(got$domains)) got$domains <- empty_named()
  got
}

catalog_save <- function(project, catalog) {
  path <- catalog_file(project)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_json(catalog, path)
  invisible(path)
}

# Build or reuse the catalog. `force = TRUE` re-reads every xpt.
build_source_catalog <- function(project, force = FALSE) {
  root <- catalog_project_dir(project)
  data_dir <- file.path(root, "data")
  old <- catalog_load(root)
  old_domains <- if (is.null(old)) empty_named() else (old$domains %||% empty_named())
  files <- if (dir.exists(data_dir)) list.files(data_dir, pattern = "\\.[Xx][Pp][Tt]$", full.names = TRUE) else character()
  files <- sort(files)
  domains <- empty_named()
  changed <- isTRUE(force) || is.null(old)
  seen <- character()
  for (f in files) {
    dom <- toupper(tools::file_path_sans_ext(basename(f)))
    if (!nzchar(dom) || dom %in% seen) next
    seen <- c(seen, dom)
    sig <- catalog_file_sig(f)
    prev <- old_domains[[dom]]
    if (!isTRUE(force) && catalog_sig_match(sig, prev)) {
      domains[[dom]] <- prev
    } else {
      domains[[dom]] <- tryCatch(catalog_read_domain(f, sig), error = function(e) {
        c(sig, list(nrow = 0L, label = "", variables = list(), values = empty_named(), error = conditionMessage(e)))
      })
      changed <- TRUE
    }
  }
  if (length(setdiff(names(old_domains), names(domains)))) changed <- TRUE
  catalog <- list(built = if (!changed && !is.null(old$built)) old$built else now_iso(), domains = domains)
  if (changed) catalog_save(root, catalog)
  catalog
}

# Same shape as the historical data_index(): DOMAIN -> column names.
catalog_index <- function(catalog) {
  doms <- if (is.null(catalog)) NULL else catalog$domains
  if (!length(doms)) return(list())
  out <- list()
  for (nm in names(doms)) {
    vars <- doms[[nm]]$variables %||% list()
    out[[nm]] <- if (!length(vars)) character() else vapply(vars, function(v) as.character(v$name %||% "")[1], "")
  }
  out
}

catalog_lookup <- function(catalog, domain, var) {
  domain <- toupper(as.character(domain %||% "")[1])
  var <- toupper(as.character(var %||% "")[1])
  dom <- if (is.null(catalog) || is.null(catalog$domains)) NULL else catalog$domains[[domain]]
  if (is.null(dom)) return("missing_domain")
  names <- toupper(vapply(dom$variables %||% list(), function(v) as.character(v$name %||% "")[1], ""))
  if (!nzchar(var) || !(var %in% names)) return("missing_var")
  "ok"
}

# One line per domain, then that domain's code values. No data rows.
catalog_prompt_text <- function(catalog, domains = NULL, max_chars = 15000, labels = FALSE) {
  if (is.null(catalog) || !length(catalog$domains)) return("")
  doms <- catalog$domains
  if (!is.null(domains)) {
    want <- toupper(as.character(domains))
    doms <- doms[names(doms) %in% want]
  }
  if (!length(doms)) return("")
  lines <- character()
  for (nm in names(doms)) {
    d <- doms[[nm]]
    vars <- d$variables %||% list()
    bits <- vapply(vars, function(v) {
      name <- as.character(v$name %||% "")[1]
      if (!isTRUE(labels)) return(name)
      lab <- as.character(v$label %||% "")[1]
      if (!nzchar(lab)) name else paste0(name, " [", lab, "]")
    }, "")
    bits <- bits[nzchar(bits)]
    nrow <- d$nrow %||% 0L
    lines <- c(lines, paste0(nm, " (", nrow, " rows): ", paste(bits, collapse = " ")))
    vals <- d$values %||% list()
    for (cn in names(vals)) {
      vv <- as.character(unlist(vals[[cn]], use.names = FALSE))
      vv <- vv[!is.na(vv)]
      lines <- c(lines, paste0("  ", cn, ": ", paste(vv, collapse = ", ")))
    }
  }
  text <- paste(lines, collapse = "\n")
  max_chars <- as.integer(max_chars %||% 15000L)[1]
  if (length(max_chars) == 1L && !is.na(max_chars) && nchar(text) > max_chars) {
    text <- paste0(substr(text, 1L, max(0L, max_chars - 3L)), "...")
  }
  text
}

# API view: the stored catalog without per-file md5.
catalog_public <- function(catalog) {
  if (is.null(catalog)) return(list(built = NULL, domains = empty_named()))
  doms <- catalog$domains %||% empty_named()
  for (nm in names(doms)) doms[[nm]]$md5 <- NULL
  list(built = catalog$built %||% NULL, domains = doms)
}

catalog_summary <- function(catalog) {
  out <- empty_named()
  doms <- if (is.null(catalog)) NULL else catalog$domains
  if (!length(doms)) return(out)
  for (nm in names(doms)) {
    d <- doms[[nm]]
    out[[nm]] <- list(
      nrow = d$nrow %||% 0L,
      nvars = length(d$variables %||% list()),
      file = as.character(d$file %||% "")[1]
    )
  }
  out
}
