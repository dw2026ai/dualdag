# Export one ADaM dataset from primary results merged by key (F11).
# xpt is written with haven and carries label, type, and length from the spec.

is_char_type <- function(type) {
  t <- tolower(trimws(type %||% ""))
  t %in% c("char", "character", "text", "c", "string")
}

is_int_type <- function(type) {
  t <- tolower(trimws(type %||% ""))
  t %in% c("int", "integer")
}

apply_spec_column <- function(x, type, length, label) {
  len <- suppressWarnings(as.integer(length))
  if (!length(len)) len <- NA_integer_
  if (is_char_type(type)) {
    x <- as.character(x)
    if (!is.na(len) && len > 0L && len < 100000L) {
      x <- substr(x, 1L, len)
      attr(x, "display_width") <- len
    }
  } else if (is_int_type(type)) {
    x <- suppressWarnings(as.integer(round(as.numeric(x))))
    if (!is.na(len) && len > 0L) attr(x, "display_width") <- len
  } else {
    x <- suppressWarnings(as.numeric(x))
    if (!is.na(len) && len > 0L) attr(x, "display_width") <- len
  }
  lab <- paste(label %||% "")
  if (nzchar(lab) && !is.na(lab)) attr(x, "label") <- lab
  x
}

primary_result_paths <- function(project, dataset, var) {
  base <- file.path(project, ".dualdag", "runs", toupper(dataset))
  if (!dir.exists(base)) return(character())
  dirs <- list.dirs(base, recursive = FALSE, full.names = FALSE)
  keep <- dirs[dirs == var | startsWith(dirs, paste0(var, "@"))]
  paths <- file.path(base, keep, "primary", "out.rds")
  sort(paths[file.exists(paths)])
}

read_primary_var <- function(project, dataset, var, keys) {
  paths <- primary_result_paths(project, dataset, var)
  if (!length(paths)) stop("No primary result for ", toupper(dataset), ".", var, call. = FALSE)
  frames <- lapply(paths, function(p) plain_frame(readRDS(p)))
  for (df in frames) {
    miss <- setdiff(keys, names(df))
    if (length(miss)) stop("Primary result for ", var, " is missing keys: ", paste(miss, collapse = ", "), call. = FALSE)
    if (!var %in% names(df)) stop("Primary result for ", var, " has no ", var, " column.", call. = FALSE)
  }
  frames <- lapply(frames, function(df) df[, unique(c(keys, var)), drop = FALSE])
  out <- do.call(rbind, frames)
  rownames(out) <- NULL
  out
}

export_dataset <- function(project, dataset, spec, format) {
  format <- tolower(format %||% "")
  if (!format %in% c("xpt", "rds")) stop("format must be xpt or rds.", call. = FALSE)
  dataset <- toupper(trimws(dataset))
  vars <- spec_vars(spec, dataset)
  if (!nrow(vars)) stop("No variables for dataset ", dataset, ".", call. = FALSE)
  keys_df <- vars
  k <- suppressWarnings(as.integer(vars$Key))
  keys_df <- vars[!is.na(k), , drop = FALSE]
  if (!nrow(keys_df)) stop("Dataset ", dataset, " has no key.", call. = FALSE)
  keys <- keys_df$Variable[order(suppressWarnings(as.integer(keys_df$Key)))]
  order_vars <- vars$Variable
  pieces <- lapply(order_vars, function(v) read_primary_var(project, dataset, v, keys))
  names(pieces) <- order_vars
  out <- pieces[[1]]
  if (length(pieces) > 1L) {
    for (i in seq.int(2L, length(pieces))) {
      out <- merge(out, pieces[[i]], by = keys, all = TRUE, sort = FALSE)
    }
  }
  out <- out[, order_vars, drop = FALSE]
  if (length(keys)) {
    out <- out[do.call(order, out[keys]), , drop = FALSE]
  }
  rownames(out) <- NULL
  for (i in seq_len(nrow(vars))) {
    v <- vars$Variable[[i]]
    out[[v]] <- apply_spec_column(out[[v]], vars$Type[[i]], vars$Length[[i]], vars$Label[[i]])
  }
  attr(out, "label") <- dataset
  dest_dir <- file.path(project, "output")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  if (identical(format, "rds")) {
    dest <- file.path(dest_dir, paste0(tolower(dataset), ".rds"))
    saveRDS(out, dest)
  } else {
    dest <- file.path(dest_dir, paste0(tolower(dataset), ".xpt"))
    ver <- if (nchar(dataset) <= 8L) 5L else 8L
    nm <- substr(dataset, 1L, if (ver == 5L) 8L else 32L)
    haven::write_xpt(out, dest, version = ver, name = nm, label = dataset)
  }
  normalizePath(dest, winslash = "/", mustWork = TRUE)
}
