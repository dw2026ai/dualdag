# Project-level analysis dataset format selection.
dataset_type_path <- function(project) {
  root <- if (is.character(project)) project else project_dir_of(project)
  file.path(root, ".dualdag", "state", "dataset_types.json")
}

dataset_type_infer <- function(spec, dataset) {
  ds <- toupper(as.character(dataset)[1])
  if (identical(ds, "ADSL")) return("ADSL")
  if (identical(ds, "ADTTE")) return("BDS-TTE")
  if (ds %in% c("ADAE", "ADCM", "ADMH")) return("OCCDS")
  if (!is.null(spec) && nrow(spec_params(spec, ds)) > 0L) return("BDS")
  ""
}

dataset_types_read <- function(project) {
  raw <- tryCatch(read_json(dataset_type_path(project)), error=function(e) NULL)
  out <- list()
  if (is.list(raw)) for (nm in names(raw)) {
    type <- toupper(as.character(raw[[nm]])[1])
    if (type %in% c("ADSL", "BDS", "BDS-TTE", "OCCDS")) out[[toupper(nm)]] <- type
  }
  out
}

dataset_type_get <- function(project, dataset, spec=NULL) {
  ds <- toupper(as.character(dataset)[1])
  saved <- dataset_types_read(project)[[ds]]
  if (!is.null(saved)) return(saved)
  if (is.null(spec)) spec <- tryCatch(read_project_spec(project), error=function(e) NULL)
  dataset_type_infer(spec, ds)
}

dataset_types_public <- function(project, spec=NULL) {
  if (is.null(spec)) spec <- tryCatch(read_project_spec(project), error=function(e) NULL)
  datasets <- if (is.null(spec)) character() else as.character(spec$datasets %||% character())
  out <- lapply(datasets, function(ds) dataset_type_get(project, ds, spec))
  names(out) <- datasets
  out
}

save_dataset_types <- function(project, formats, spec=NULL) {
  if (is.null(spec)) spec <- read_project_spec(project)
  datasets <- unique(toupper(as.character(spec$datasets %||% character())))
  if (!is.list(formats) || is.null(names(formats))) stop("formats must map dataset names to formats.", call.=FALSE)
  submitted <- toupper(names(formats))
  if (any(!submitted %in% datasets)) stop("A format was supplied for a dataset that is not in this spec.", call.=FALSE)
  allowed <- c("ADSL", "BDS", "BDS-TTE", "OCCDS")
  values <- vapply(formats, function(x) toupper(as.character(x)[1]), "")
  if (any(!values %in% allowed)) stop("Choose ADSL, BDS, BDS-TTE, or OCCDS for every dataset.", call.=FALSE)
  complete <- setNames(rep("", length(datasets)), datasets)
  current <- dataset_types_public(project, spec)
  complete[names(current)] <- unlist(current, use.names=FALSE)
  complete[names(values)] <- values
  if (any(!nzchar(complete))) stop("Choose a format for every analysis dataset.", call.=FALSE)
  adsl <- names(complete)[complete == "ADSL"]
  if (length(adsl) != 1L) stop("Choose exactly one ADSL dataset per study.", call.=FALSE)
  path <- dataset_type_path(project)
  dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)
  write_json(as.list(complete), path)
  as.list(complete)
}
