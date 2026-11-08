# Convert the CDISC pilot ADaM define.xml and check the spec the app can load.
# The pilot transports stay outside the repo. Skip when that tree is absent.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
pilot_define <- Sys.getenv("DUALDAG_PILOT_DEFINE", "")

sys.source(file.path(app_root, "tools", "define_to_spec.R"), envir = environment())
if (!exists("read_spec", mode = "function", inherits = TRUE)) {
  sys.source(file.path(app_root, "R", "utils.R"), envir = globalenv())
  sys.source(file.path(app_root, "R", "spec.R"), envir = globalenv())
}

read_chr_csv <- function(path) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                        na.strings = character(), check.names = FALSE, comment.char = "")
  rownames(df) <- NULL
  df
}

itemref_counts <- function(path) {
  doc <- xml2::read_xml(path)
  groups <- xml2::xml_find_all(doc, ".//*[local-name()='ItemGroupDef']")
  n_group <- sum(vapply(groups, function(g) {
    length(xml2::xml_find_all(g, "./*[local-name()='ItemRef']"))
  }, 0L))
  n_vl <- length(xml2::xml_find_all(doc, ".//*[local-name()='ValueListDef']/*[local-name()='ItemRef']"))
  n_item <- length(xml2::xml_find_all(doc, ".//*[local-name()='ItemDef']"))
  list(group = n_group, value = n_vl, item = n_item)
}

note_vars <- function(notes, dataset, topic) {
  hit <- notes$Dataset == dataset & notes$Topic == topic
  sort(unique(notes$Variable[hit]))
}

test_that("sourcing the converter does not launch the command-line entry", {
  expect_false(launched())
})

test_that("pilot define.xml becomes a Variable sheet of dataset columns the app accepts", {
  skip_if_not(file.exists(pilot_define), "CDISC pilot define.xml is not on this machine")
  counts <- itemref_counts(pilot_define)
  # Value-level ItemRefs repeat a name inside one dataset, so they are not Variable rows.
  expect_equal(counts$item, 1318L)
  expect_equal(counts$group + counts$value, 1318L)

  out <- tempfile("cdisc-spec-")
  dir.create(out)
  res <- define_to_spec(pilot_define, out)
  on.exit(unlink(out, recursive = TRUE), add = TRUE)

  expect_equal(names(res$variable), c(
    "Dataset", "Variable", "Label", "Type", "Length", "Key", "Source", "Derivation"))
  expect_equal(names(res$parameter), c("Dataset", "PARAMCD", "PARAM", "Source", "Derivation"))
  expect_equal(nrow(res$variable), counts$group)
  expect_equal(res$n_group_ref, counts$group)
  expect_equal(nrow(res$value_level), counts$value)
  expect_equal(res$n_vl_ref, counts$value)
  expect_equal(length(unique(res$variable$Dataset)), 10L)
  expect_false(any(duplicated(paste(res$variable$Dataset, res$variable$Variable))))
  expect_true(all(nzchar(res$variable$Type)))
  expect_false(any(tolower(res$variable$Type) %in% c("char", "num", "character", "numeric")))
  filled_src <- res$variable$Source[nzchar(res$variable$Source)]
  expect_true(all(grepl("^[A-Z][A-Z0-9]*\\.[A-Z][A-Z0-9_]*$", filled_src)))

  key_bad <- character()
  for (ds in unique(res$variable$Dataset)) {
    ks <- res$variable$Key[res$variable$Dataset == ds]
    ks <- ks[nzchar(ks)]
    if (!length(ks) || !identical(sort(as.integer(ks)), seq_along(ks))) key_bad <- c(key_bad, ds)
  }
  expect_equal(key_bad, character())
  bds <- res$datasets$Dataset[res$datasets$Class == "BDS"]
  expect_true(all(c("ADSL", "ADAE") %in% res$datasets$Dataset))
  expect_gte(length(bds), 1L)
  bds_empty <- bds[vapply(bds, function(ds) sum(res$parameter$Dataset == ds) < 1L, TRUE)]
  expect_equal(bds_empty, character())

  sheets <- readxl::excel_sheets(res$workbook)
  expect_equal(sheets, c("Variable", "Parameter", "Datasets", "ValueLevel", "Codelist", "Methods", "Notes"))
  vx <- readxl::read_excel(res$workbook, "Variable", col_types = "text")
  expect_equal(nrow(vx), counts$group)
  expect_equal(names(vx), names(res$variable))

  sheet_xml <- unzip(res$workbook, "xl/worksheets/sheet1.xml", exdir = tempdir())
  style_xml <- unzip(res$workbook, "xl/styles.xml", exdir = tempdir())
  sheet_txt <- paste(readLines(sheet_xml, warn = FALSE), collapse = "")
  style_txt <- paste(readLines(style_xml, warn = FALSE), collapse = "")
  expect_match(sheet_txt, "autoFilter")
  expect_match(sheet_txt, 'ySplit="1"')
  expect_match(sheet_txt, "frozen")
  expect_match(style_txt, 'b val="1"')
  expect_match(style_txt, "wrapText")

  adam_dir <- dirname(pilot_define)
  label_bad <- character()
  length_bad <- character()
  for (i in seq_len(nrow(res$datasets))) {
    ds <- res$datasets$Dataset[[i]]
    path <- file.path(adam_dir, res$datasets$XptFile[[i]])
    expect_true(file.exists(path))
    df <- haven::read_xpt(path)
    spec_vars <- res$variable$Variable[res$variable$Dataset == ds]
    expect_equal(sort(setdiff(spec_vars, names(df))), note_vars(res$notes, ds, "not_in_xpt"))
    expect_equal(sort(setdiff(names(df), spec_vars)), note_vars(res$notes, ds, "not_in_define"))
    for (v in intersect(spec_vars, names(df))) {
      lab <- attr(df[[v]], "label", exact = TRUE)
      if (!is.null(lab) && length(lab) && !is.na(lab[[1]]) && nzchar(norm_ws(lab[[1]]))) {
        spec_lab <- res$variable$Label[res$variable$Dataset == ds & res$variable$Variable == v]
        if (!identical(norm_ws(spec_lab), norm_ws(lab[[1]]))) {
          label_bad <- c(label_bad, paste0(ds, ".", v))
        }
      }
      for (a in c("display_width", "width", "length")) {
        lv <- attr(df[[v]], a, exact = TRUE)
        if (is.null(lv) || !length(lv) || is.na(lv[[1]]) || !nzchar(as.character(lv[[1]]))) next
        spec_len <- res$variable$Length[res$variable$Dataset == ds & res$variable$Variable == v]
        got <- as.character(suppressWarnings(as.integer(lv[[1]])))
        if (!identical(spec_len, got)) length_bad <- c(length_bad, paste0(ds, ".", v, " ", a))
      }
    }
  }
  expect_equal(label_bad, character())
  expect_equal(length_bad, character())

  spec <- read_spec(file.path(out, "variable.csv"), file.path(out, "parameter.csv"))
  expect_equal(spec$datasets, sort(unique(res$variable$Dataset)))
  expect_length(spec$datasets, 10L)
  blocked <- Filter(function(issue) issue$type %in% c("structure", "no_key"), spec$problems)
  expect_length(blocked, 0L)

  committed <- file.path(app_root, "examples", "cdiscpilot01", "spec")
  expect_equal(read_chr_csv(file.path(committed, "variable.csv")), res$variable)
  expect_equal(read_chr_csv(file.path(committed, "parameter.csv")), res$parameter)
  committed_spec <- read_spec(
    file.path(committed, "variable.csv"),
    file.path(committed, "parameter.csv"))
  expect_length(committed_spec$datasets, 10L)
  blocked_committed <- Filter(function(issue) issue$type %in% c("structure", "no_key"), committed_spec$problems)
  expect_length(blocked_committed, 0L)

  expect_equal(nrow(res$splits), 10L)
  expect_equal(sort(res$splits$Dataset), sort(unique(res$variable$Dataset)))
  sheet_frame <- function(path, sheet) {
    df <- as.data.frame(readxl::read_excel(path, sheet, col_types = "text"), stringsAsFactors = FALSE)
    rownames(df) <- NULL
    df
  }
  combined_var <- sheet_frame(res$workbook, "Variable")
  var_sum <- 0L
  par_sum <- 0L
  for (i in seq_len(nrow(res$splits))) {
    ds <- res$splits$Dataset[[i]]
    dest <- file.path(out, tolower(ds))
    expect_true(dir.exists(dest))
    vpath <- file.path(dest, "variable.csv")
    ppath <- file.path(dest, "parameter.csv")
    xpath <- file.path(dest, paste0(tolower(ds), "_spec.xlsx"))
    expect_true(file.exists(xpath))
    one <- read_spec(vpath, ppath)
    expect_equal(one$datasets, ds)
    blocked_one <- Filter(function(issue) issue$type %in% c("structure", "no_key"), one$problems)
    expect_length(blocked_one, 0L)
    vrows <- read_chr_csv(vpath)
    prows <- read_chr_csv(ppath)
    var_sum <- var_sum + nrow(vrows)
    par_sum <- par_sum + nrow(prows)
    expect_equal(nrow(vrows), res$splits$Variables[[i]])
    expect_equal(nrow(prows), res$splits$Parameters[[i]])
    expect_equal(names(prows), names(res$parameter))
    got <- sheet_frame(xpath, "Variable")
    want <- combined_var[combined_var$Dataset == ds, , drop = FALSE]
    rownames(want) <- NULL
    expect_equal(got, want)
    expect_equal(readxl::excel_sheets(xpath), sheets)
    committed_dir <- file.path(committed, tolower(ds))
    expect_equal(read_chr_csv(file.path(committed_dir, "variable.csv")), vrows)
    expect_equal(read_chr_csv(file.path(committed_dir, "parameter.csv")), prows)
  }
  expect_equal(var_sum, 415L)
  expect_equal(par_sum, 113L)
  expect_equal(var_sum, nrow(res$variable))
  expect_equal(par_sum, nrow(res$parameter))
})
