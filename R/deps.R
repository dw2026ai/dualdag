# Dependency check. Base R only: this file is sourced before any package is loaded.

dualdag_required_packages <- function() {
  c(
    httpuv = "1.6.0", jsonlite = "1.8.0", httr2 = "1.0.0", callr = "3.7.0",
    later = "1.3.0", promises = "1.2.0", openssl = "1.4.0", haven = "2.5.0",
    igraph = "1.3.0", diffdf = "1.0.0", dplyr = "1.1.0"
  )
}

dualdag_check_deps <- function(req = dualdag_required_packages(), lib = NULL) {
  rows <- lapply(names(req), function(p) {
    have <- tryCatch(as.character(utils::packageVersion(p, lib.loc = lib)), error = function(e) NA_character_)
    ok <- !is.na(have) && utils::compareVersion(have, req[[p]]) >= 0
    data.frame(package = p, required = req[[p]], installed = have, ok = ok, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out[!out$ok, c("package", "required", "installed"), drop = FALSE]
}

# Interactive R asks. Rscript installs only with --install-deps or DUALDAG_INSTALL_DEPS=1.
dualdag_ensure_deps <- function(install_flag = FALSE) {
  if (getRversion() < "4.2.0") stop("dualdag needs R >= 4.2.0 (this is R ", getRversion(), ").", call. = FALSE)
  miss <- dualdag_check_deps()
  if (!nrow(miss)) return(invisible(TRUE))
  cat("dualdag needs these R packages (missing or too old):\n")
  for (i in seq_len(nrow(miss))) {
    cat(sprintf("  - %-10s >= %-8s (installed: %s)\n", miss$package[i], miss$required[i],
                ifelse(is.na(miss$installed[i]), "none", miss$installed[i])))
  }
  repos <- getOption("repos")
  if (is.null(repos) || identical(unname(repos["CRAN"]), "@CRAN@")) repos <- c(CRAN = "https://cloud.r-project.org")
  cat("CRAN mirror: ", paste(repos, collapse = ", "), "  (set options(repos = ...) to use your company mirror)\n", sep = "")
  go <- FALSE
  if (interactive()) {
    ans <- utils::askYesNo("Install them now with install.packages()?", default = TRUE)
    go <- isTRUE(ans)
  } else if (isTRUE(install_flag) || identical(Sys.getenv("DUALDAG_INSTALL_DEPS"), "1")) {
    go <- TRUE
  } else {
    cat("\nNot installing automatically. Re-run with:  Rscript run.R --install-deps\n",
        "or set DUALDAG_INSTALL_DEPS=1, or install them yourself:\n  install.packages(c(",
        paste0('"', miss$package, '"', collapse = ", "), "))\n", sep = "")
  }
  if (!go) stop("Missing R packages: ", paste(miss$package, collapse = ", "), call. = FALSE)
  utils::install.packages(miss$package, repos = repos)
  still <- dualdag_check_deps()
  if (nrow(still)) stop("Still missing after install: ", paste(still$package, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}
