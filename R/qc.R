# Per-dataset QC methodology documents (F9g).
# One file per dataset under ~/.dualdag/qc/<DATASET>.md.
# DUALDAG_HOME overrides the config directory (tests use a temp folder).

qc_home <- function() {
  d <- Sys.getenv("DUALDAG_HOME", unset = "")
  if (!nzchar(d)) d <- file.path(path.expand("~"), ".dualdag")
  d
}

qc_dir <- function() file.path(qc_home(), "qc")

qc_dataset_name <- function(dataset) {
  ds <- toupper(trimws(dataset %||% ""))
  if (!grepl("^[A-Z][A-Z0-9_]*$", ds)) stop("Invalid dataset name.", call. = FALSE)
  ds
}

qc_path <- function(dataset) file.path(qc_dir(), paste0(qc_dataset_name(dataset), ".md"))

qc_list <- function() {
  dir <- qc_dir()
  if (!dir.exists(dir)) return(list())
  files <- list.files(dir, pattern = "\\.md$", full.names = TRUE)
  out <- list()
  for (f in files) {
    ds <- toupper(sub("\\.md$", "", basename(f)))
    if (!grepl("^[A-Z][A-Z0-9_]*$", ds)) next
    info <- file.info(f)
    out[[length(out) + 1L]] <- list(
      dataset = ds,
      size = as.integer(info$size),
      updated = format(info$mtime, "%Y-%m-%dT%H:%M:%S%z")
    )
  }
  out
}

qc_get <- function(dataset) {
  p <- qc_path(dataset)
  if (!file.exists(p)) return(NULL)
  read_utf8(p)
}

# replace = FALSE leaves an existing document untouched and reports exists.
qc_save <- function(dataset, markdown, replace = FALSE) {
  p <- qc_path(dataset)
  if (file.exists(p) && !isTRUE(replace)) return(list(ok = FALSE, exists = TRUE))
  write_utf8(paste(markdown %||% ""), p)
  list(ok = TRUE, exists = FALSE)
}
