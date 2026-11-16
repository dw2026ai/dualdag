# Compare the independent committed-spec reference build with pharmaverse's
# own ADPPK program. admiral is needed only to execute the upstream program.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Usage: Rscript tools/compare_pharmaverse_adppk.R /path/to/pharmaverse/examples", call. = FALSE)
}
source_root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
source_file <- file.path(source_root, "adam", "adppk.R")
if (!file.exists(source_file) || !file.exists(file.path(source_root, "metadata", "pk_spec.xlsx"))) {
  stop("Expected adam/adppk.R and metadata/pk_spec.xlsx under ", source_root, call. = FALSE)
}

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
while (!file.exists(file.path(root, "tools", "pharmaverse_adppk_ref.R"))) {
  parent <- dirname(root)
  if (parent == root) stop("Run this command from the dualdag repository.", call. = FALSE)
  root <- parent
}
ref_env <- new.env(parent = globalenv())
ref_env$.DUALDAG_ADPPK_REF_SOURCE_ONLY <- TRUE
sys.source(file.path(root, "tools", "pharmaverse_adppk_ref.R"), envir = ref_env)

upstream_env <- new.env(parent = globalenv())
old_wd <- getwd()
setwd(source_root)
sys.source(source_file, envir = upstream_env)
setwd(old_wd)
upstream <- upstream_env$adppk
reference <- ref_env$build_pharmaverse_adppk_ref(
  data_dir = file.path(root, "demos", "adppk", "data"),
  spec_file = file.path(root, "demos", "adppk", "spec", "variable.csv")
)

spec <- utils::read.csv(file.path(root, "demos", "adppk", "spec", "variable.csv"),
                        stringsAsFactors = FALSE, check.names = FALSE)
if (!identical(names(upstream), spec$Variable)) {
  stop("Upstream output columns do not match the committed spec in name and order.", call. = FALSE)
}
if (!identical(names(reference), spec$Variable)) {
  stop("Reference output columns do not match the committed spec in name and order.", call. = FALSE)
}
# RECSEQ is a generated sequence used here to align corresponding rows from
# the two builds. It is not an ADPPK metadata key (the keys are in the spec).
if (!"RECSEQ" %in% names(upstream) || anyDuplicated(upstream$RECSEQ) || anyDuplicated(reference$RECSEQ)) {
  stop("RECSEQ must uniquely align records in both outputs.", call. = FALSE)
}
reference <- reference[match(upstream$RECSEQ, reference$RECSEQ), , drop = FALSE]
if (nrow(upstream) != nrow(reference) || anyNA(reference$RECSEQ)) {
  stop("Outputs have different record counts or RECSEQ row-alignment values.", call. = FALSE)
}

same_value <- function(x, y) {
  x_num <- is.numeric(x) || inherits(x, "Date")
  y_num <- is.numeric(y) || inherits(y, "Date")
  same <- is.na(x) & is.na(y)
  if (x_num && y_num) {
    xn <- as.numeric(x); yn <- as.numeric(y)
    same <- same | (!is.na(xn) & !is.na(yn) & abs(xn - yn) <= 1e-8)
  } else {
    xc <- as.character(x); yc <- as.character(y)
    same <- same | (!is.na(xc) & !is.na(yc) & xc == yc)
  }
  same
}

different <- list()
for (nm in spec$Variable) {
  equal <- same_value(upstream[[nm]], reference[[nm]])
  if (any(!equal)) {
    r <- spec[match(nm, spec$Variable), , drop = FALSE]
    different[[nm]] <- list(count = sum(!equal), reason = r$Derivation[[1L]])
  }
}
cat("Upstream pharmaverse output:", nrow(upstream), "records x", ncol(upstream), "columns\n")
cat("Independent committed-spec output:", nrow(reference), "records x", ncol(reference), "columns\n")
if (!length(different)) {
  cat("Value comparison by RECSEQ: all", nrow(upstream), "records match in all", ncol(upstream), "columns.\n")
} else {
  cat("Columns with value differences:\n")
  for (nm in names(different)) {
    cat("- ", nm, ": ", different[[nm]]$count, " record(s); spec derivation: ",
        different[[nm]]$reason, "\n", sep = "")
  }
  quit(status = 1L)
}
