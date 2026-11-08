# Pharmaverse demo specs: structure, references, the offline checker, and the reference xpt.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
sys.source(file.path(app_root, "R", "utils.R"), envir = globalenv())
source(file.path(testthat::test_path(), "..", "fixtures", "helper.R"))
for (f in c("spec.R", "catalog.R")) {
  sys.source(file.path(app_root, "R", f), envir = globalenv())
}

sdtm_dir <- path.expand("~/dualdag-demo/pharmaversesdtm")
adam_dir <- path.expand("~/dualdag-demo/pharmaverse-demo")
spec_root <- file.path(app_root, "examples", "pharmaverse-demo", "spec")
datasets <- c("adsl", "adae", "advs")
adam_names <- c("ADSL", "ADAE", "ADVS")
adsl_domains <- c("dm", "suppdm", "ex", "ds", "ae", "vs", "lb", "sv", "cm", "mh")

read_chr_csv <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                        na.strings = character(), check.names = FALSE, comment.char = "")
  rownames(df) <- NULL
  df
}

sheet_frame <- function(path, sheet) {
  df <- as.data.frame(readxl::read_excel(path, sheet, col_types = "text"), stringsAsFactors = FALSE)
  for (nm in names(df)) df[[nm]][is.na(df[[nm]])] <- ""
  rownames(df) <- NULL
  df
}

spec_paths <- function(ds) {
  list(variable = file.path(spec_root, ds, "variable.csv"),
       parameter = file.path(spec_root, ds, "parameter.csv"),
       xlsx = file.path(spec_root, ds, paste0(ds, "_spec.xlsx")))
}

load_demo_spec <- function(ds) {
  p <- spec_paths(ds)
  read_spec(p$variable, p$parameter)
}

issue_lines <- function(issues) {
  if (!length(issues)) return(character())
  vapply(issues, function(i) paste(i$type %||% "", i$message %||% "", sep = ": "), "")
}

ref_tokens <- function(text) {
  m <- unlist(regmatches(text, gregexpr("\\b[A-Za-z][A-Za-z0-9]*\\.[A-Za-z][A-Za-z0-9]*\\b", text, perl = TRUE)))
  if (!length(m)) return(character())
  unique(toupper(m))
}

column_cache <- new.env(parent = emptyenv())
columns_of <- function(dom, specs) {
  dom <- toupper(dom)
  if (exists(dom, envir = column_cache, inherits = FALSE)) return(get(dom, envir = column_cache))
  cols <- if (dom %in% adam_names) {
    specs[[tolower(dom)]]$variables$Variable
  } else {
    path <- file.path(sdtm_dir, paste0(tolower(dom), ".xpt"))
    if (!file.exists(path)) NULL else names(haven::read_xpt(path, n_max = 0L))
  }
  assign(dom, cols, envir = column_cache)
  cols
}

link_xpt <- function(proj, paths) {
  dir.create(file.path(proj, "data"), recursive = TRUE, showWarnings = FALSE)
  for (f in paths) file.symlink(f, file.path(proj, "data", basename(f)))
  proj
}

test_that("pharmaverse demo specs load and the workbook matches the CSV", {
  for (ds in datasets) {
    p <- spec_paths(ds)
    expect_true(file.exists(p$variable))
    expect_true(file.exists(p$parameter))
    expect_true(file.exists(p$xlsx))
    sp <- load_demo_spec(ds)
    bad <- Filter(function(prob) (prob$type %||% "") %in% c("structure", "no_key"), sp$problems)
    expect_identical(issue_lines(bad), character(), info = ds)
    csv_v <- read_chr_csv(p$variable)
    csv_p <- read_chr_csv(p$parameter)
    expect_equal(sheet_frame(p$xlsx, "Variable"), csv_v, info = ds)
    expect_equal(sheet_frame(p$xlsx, "Parameter"), csv_p, info = ds)
    sheets <- readxl::excel_sheets(p$xlsx)
    expect_equal(sheets, c("Variable", "Parameter", "Datasets", "Notes"), info = ds)
  }
  counts <- vapply(datasets, function(ds) nrow(load_demo_spec(ds)$variables), 0L)
  expect_equal(counts, c(adsl = 44L, adae = 41L, advs = 36L))
  params <- vapply(datasets, function(ds) nrow(load_demo_spec(ds)$parameters), 0L)
  expect_equal(params, c(adsl = 0L, adae = 0L, advs = 8L))
})

test_that("every DOMAIN.VAR reference exists and the text has no i.e. or e.g.", {
  skip_if_not(dir.exists(sdtm_dir) && file.exists(file.path(sdtm_dir, "dm.xpt")),
              "pharmaversesdtm transports are not installed")
  specs <- setNames(lapply(datasets, load_demo_spec), datasets)
  for (ds in datasets) {
    sp <- specs[[ds]]
    texts <- c(sp$variables$Source, sp$variables$Derivation, sp$parameters$Source, sp$parameters$Derivation)
    blob <- paste(texts, collapse = "\n")
    expect_false(grepl("\\bi\\.e\\.|\\be\\.g\\.", blob, perl = TRUE, ignore.case = TRUE), info = ds)
    missing <- character()
    for (tok in ref_tokens(blob)) {
      dom <- sub("\\..*$", "", tok)
      var <- sub("^[^.]*\\.", "", tok)
      cols <- columns_of(dom, specs)
      if (is.null(cols) || !(var %in% toupper(cols))) missing <- c(missing, tok)
    }
    expect_identical(unique(missing), character(), info = ds)
  }
})

test_that("the offline checker reports no error-severity issues", {
  skip_if_not(dir.exists(sdtm_dir) && file.exists(file.path(sdtm_dir, "dm.xpt")),
              "pharmaversesdtm transports are not installed")
  skip_if_not(file.exists(file.path(adam_dir, "adsl.xpt")),
              "reference adsl.xpt has not been built")
  uploads <- list(
    ADSL = file.path(sdtm_dir, paste0(adsl_domains, ".xpt")),
    ADAE = c(file.path(sdtm_dir, "ae.xpt"), file.path(adam_dir, "adsl.xpt")),
    ADVS = c(file.path(sdtm_dir, "vs.xpt"), file.path(adam_dir, "adsl.xpt"))
  )
  with_fake_llm({
    for (ds in names(uploads)) {
      proj <- link_xpt(tempfile(paste0("pharma-", ds, "-")), uploads[[ds]])
      catg <- build_source_catalog(proj)
      sp <- load_demo_spec(tolower(ds))
      g <- extract_dependencies(list(), sp, ds, catalog = catg)
      chk <- check_graph(g, sp, catalog_index(catg), catalog = catg)
      errs <- Filter(function(i) !identical(issue_severity(i), "warning"), chk$issues)
      expect_identical(issue_lines(errs), character(), info = ds)
    }
  })
})

test_that("reference datasets keep unique keys and the spec columns", {
  skip_if_not(file.exists(file.path(adam_dir, "adsl.xpt")) &&
                file.exists(file.path(adam_dir, "adae.xpt")) &&
                file.exists(file.path(adam_dir, "advs.xpt")),
              "reference transports have not been built")
  frames <- list()
  for (ds in datasets) {
    sp <- load_demo_spec(ds)
    df <- haven::read_xpt(file.path(adam_dir, paste0(ds, ".xpt")))
    expect_true(setequal(names(df), sp$variables$Variable), info = ds)
    keyed <- sp$variables[!is.na(sp$variables$Key), , drop = FALSE]
    keyed <- keyed[order(keyed$Key), , drop = FALSE]
    expect_equal(anyDuplicated(df[, keyed$Variable, drop = FALSE]), 0L, info = ds)
    frames[[ds]] <- df
  }
  adsl <- frames$adsl
  adae <- frames$adae
  advs <- frames$advs
  expect_equal(nrow(adsl), 306L)
  expect_equal(nrow(adae), 1191L)
  expect_equal(sum(adsl$SAFFL == "Y"), 254L)
  ok <- !is.na(adsl$TRTSDT) & !is.na(adsl$TRTEDT)
  expect_true(all(adsl$TRTDURD[ok] == as.integer(adsl$TRTEDT[ok] - adsl$TRTSDT[ok]) + 1L))
  flags <- as.character(adae$ASTDTF)
  flags[is.na(flags)] <- ""
  expect_equal(sum(flags == "D"), 15L)
  expect_equal(sum(flags == "M"), 11L)
  te <- adae$TRTEMFL == "Y" & !is.na(adae$ASTDT)
  y <- !is.na(adae$AOCCIFL) & adae$AOCCIFL == "Y"
  per_subj <- tapply(y, adae$USUBJID, sum)
  expect_true(all(per_subj <= 1L))
  expect_true(all(per_subj[unique(adae$USUBJID[te])] == 1L))
  ind <- as.character(advs$ANRIND)
  ind[is.na(ind)] <- ""
  expect_true(all(ind %in% c("", "LOW", "NORMAL", "HIGH")))
  ht <- advs[advs$PARAMCD == "HEIGHT" & !is.na(advs$AVAL), c("USUBJID", "AVAL")]
  wt <- advs[advs$PARAMCD == "WEIGHT" & !is.na(advs$AVAL), c("USUBJID", "AVISITN", "AVAL")]
  bmi <- advs[advs$PARAMCD == "BMI", c("USUBJID", "AVISITN", "AVAL")]
  h <- ht$AVAL[match(bmi$USUBJID, ht$USUBJID)]
  w <- wt$AVAL[match(paste(bmi$USUBJID, bmi$AVISITN), paste(wt$USUBJID, wt$AVISITN))]
  expect_true(length(bmi$AVAL) > 0L)
  expect_true(all(abs(bmi$AVAL - w / (h / 100) ^ 2) < 1e-6))
  group <- paste(advs$USUBJID, advs$PARAMCD, ifelse(is.na(advs$ATPTN), "*", advs$ATPTN))
  expect_true(all(tapply(advs$ABLFL == "Y", group, sum) <= 1L))
})
