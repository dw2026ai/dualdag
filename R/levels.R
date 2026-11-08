# Distinct categorical levels sent with a lineage variable.
distinct_levels <- function(x, max_levels = 20L) {
  if (!(is.character(x) || is.factor(x) || is.logical(x)) ||
      inherits(x, c("Date", "POSIXt"))) return(NULL)
  max_levels <- as.integer(max_levels)[1]
  if (is.na(max_levels) || max_levels < 1L) stop("max_levels must be positive")
  value <- as.character(x)
  value[is.na(x)] <- NA_character_
  distinct <- unique(value)
  if (length(distinct) > max_levels)
    return(structure(data.frame(value = character(), n = integer()), too_many = TRUE))
  distinct <- sort(distinct, na.last = TRUE)
  data.frame(value = substr(ifelse(is.na(distinct), "NA", distinct), 1L, 60L),
             n = vapply(distinct, function(v) if (is.na(v)) sum(is.na(value)) else sum(!is.na(value) & value == v), 0L,
                        USE.NAMES = FALSE),
             stringsAsFactors = FALSE)
}

.levels_cache <- new.env(parent = emptyenv())
levels_cache_reset <- function() {
  rm(list = ls(.levels_cache, all.names = TRUE), envir = .levels_cache)
  invisible(NULL)
}

file_distinct_levels <- function(path, column, max_levels = 20L) {
  if (!file.exists(path)) return(NULL)
  info <- file.info(path)
  key <- paste(normalizePath(path), column, as.numeric(info$mtime), info$size, max_levels, sep = "\r")
  if (exists(key, envir = .levels_cache, inherits = FALSE)) return(get(key, envir = .levels_cache))
  df <- tryCatch(if (grepl("\\.[Xx][Pp][Tt]$", path)) haven::read_xpt(path) else readRDS(path), error = function(e) NULL)
  got <- if (is.data.frame(df) && column %in% names(df)) distinct_levels(df[[column]], max_levels) else NULL
  assign(key, got, envir = .levels_cache)
  got
}

levels_line <- function(ref, levels, max_levels = 20L) {
  if (is.null(levels)) return("")
  if (isTRUE(attr(levels, "too_many")))
    return(sprintf("%s: more than %d distinct values, not listed.", ref, max_levels))
  if (!nrow(levels)) return("")
  paste0(ref, ": ", paste0(levels$value, " (", levels$n, ")", collapse = ", "))
}
