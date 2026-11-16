# Fetch the SDTM transports needed by a bundled demo project.

demo_fetch_root <- function() {
  frames <- sys.frames()
  for (i in rev(seq_along(frames))) {
    f <- frames[[i]]$ofile
    if (!is.null(f)) {
      x <- normalizePath(dirname(f), winslash = "/", mustWork = FALSE)
      for (j in seq_len(6L)) {
        if (file.exists(file.path(x, "tools", "get_demo_data.R"))) return(x)
        parent <- dirname(x)
        if (parent == x) break
        x <- parent
      }
    }
  }
  x <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (j in seq_len(6L)) {
    if (file.exists(file.path(x, "tools", "get_demo_data.R"))) return(x)
    parent <- dirname(x)
    if (parent == x) break
    x <- parent
  }
  stop("Cannot find dualdag root.", call. = FALSE)
}

demo_ensure_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(missing)) return(invisible(NULL))
  cat("Installing required R package(s): ", paste(missing, collapse = ", "), " from CRAN.\n", sep = "")
  utils::install.packages(missing, repos = "https://cloud.r-project.org")
  still_missing <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still_missing)) stop("Could not install required package(s): ", paste(still_missing, collapse = ", "), call. = FALSE)
}

get_demo_data <- function(dataset = c("adsl", "advs", "adppk", "all"), root = demo_fetch_root()) {
  dataset <- match.arg(tolower(dataset), c("adsl", "advs", "adppk", "all"))
  demo_ensure_packages(c("pharmaversesdtm", "haven"))
  version <- as.character(utils::packageVersion("pharmaversesdtm"))
  if (!identical(version, "1.5.0")) {
    warning("pharmaversesdtm ", version, " is installed; demo specs were written against 1.5.0. Continuing.", call. = FALSE)
  }

  source(file.path(root, "tools", "adapt_to_pharmaverse.R"), local = environment())
  source(file.path(root, "tools", "pharmaverse_demo_ref.R"), local = environment())
  sets <- if (dataset == "all") c("adsl", "advs", "adppk") else dataset
  adsl_domains <- c("dm", "suppdm", "ex", "ds", "ae", "vs", "lb", "sv", "cm", "mh")
  for (ds in sets) {
    data_dir <- file.path(root, "demos", ds, "data")
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    if (ds == "adsl") {
      export_pharmaversesdtm(data_dir, domains = adsl_domains)
    } else if (ds == "advs") {
      build_input <- tempfile("dualdag-advs-input-")
      dir.create(build_input)
      on.exit(unlink(build_input, recursive = TRUE), add = TRUE)
      export_pharmaversesdtm(build_input, domains = adsl_domains)
      file.copy(file.path(build_input, "vs.xpt"), file.path(data_dir, "vs.xpt"), overwrite = TRUE)
      demo_ensure_packages("dplyr")
      build_pharmaverse_demo_adsl(
        dir = build_input,
        spec_dir = file.path(root, "demos", "adsl", "spec"),
        out_dir = data_dir
      )
      unlink(build_input, recursive = TRUE)
    } else {
      build_input <- tempfile("dualdag-adppk-input-")
      dir.create(build_input)
      on.exit(unlink(build_input, recursive = TRUE), add = TRUE)
      export_pharmaversesdtm(build_input, domains = unique(c(adsl_domains, "pc")))
      for (domain in c("ex", "pc", "vs", "lb")) {
        file.copy(file.path(build_input, paste0(domain, ".xpt")),
                  file.path(data_dir, paste0(domain, ".xpt")), overwrite = TRUE)
      }
      build_pharmaverse_demo_adsl(
        dir = build_input,
        spec_dir = file.path(root, "demos", "adsl", "spec"),
        out_dir = data_dir
      )
      unlink(build_input, recursive = TRUE)
    }
  }
  invisible(sets)
}

if (!exists(".DUALDAG_GET_DEMO_DATA_SOURCE_ONLY", inherits = FALSE)) {
  args <- commandArgs(trailingOnly = TRUE)
  get_demo_data(if (length(args)) args[[1L]] else "all")
}
