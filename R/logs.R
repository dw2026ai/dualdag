# Classify the stored R console transcript without interpreting its contents as HTML.
classify_log_lines <- function(log) {
  lines <- strsplit(as.character(log %||% ""), "\n", fixed = TRUE)[[1]]
  if (!length(lines)) return(character())
  tags <- rep("", length(lines))
  state <- ""
  for (i in seq_along(lines)) {
    line <- lines[[i]]
    if (grepl("^[[:space:]]*(Error( in\\b|\\b)|Execution halted\\b|DUALDAG_MISSING_(DEPENDENCY|COLUMN)\\b)", line, perl = TRUE)) {
      state <- "error"
    } else if (grepl("^[[:space:]]*(Warning( messages?:)?\\b|In .+[[:space:]]:[[:space:]]*)", line, perl = TRUE)) {
      state <- "warning"
    } else if (grepl("^[[:space:]]*([>+#][[:space:]]|$)", line, perl = TRUE)) {
      state <- ""
    }
    tags[[i]] <- state
  }
  tags
}

save_log_tags <- function(path, log) {
  if (!is.character(path) || !length(path) || !nzchar(path[[1]])) return(invisible(NULL))
  tags <- classify_log_lines(log)
  saveRDS(tags, file.path(dirname(path[[1]]), "log_tags.rds"))
  invisible(tags)
}
