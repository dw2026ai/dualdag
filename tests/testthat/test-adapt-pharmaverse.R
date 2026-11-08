# Pharmaverse adaptation of the per-dataset CDISC pilot specs.
# The transports live outside the repo. Skip the xpt checks when that tree is absent.

app_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/")
xpt_dir <- path.expand("~/dualdag-demo/pharmaversesdtm")
spec_root <- file.path(app_root, "examples", "pharmaverse", "spec")
pilot_root <- file.path(app_root, "examples", "cdiscpilot01", "spec")
datasets <- c("adsl", "adae", "adlbc", "adlbh", "adlbhy",
              "adqsadas", "adqscibc", "adqsnpix", "adtte", "advs")
adam <- toupper(datasets)

sys.source(file.path(app_root, "tools", "adapt_to_pharmaverse.R"), envir = environment())
if (!exists("read_spec", mode = "function", inherits = TRUE)) {
  sys.source(file.path(app_root, "R", "utils.R"), envir = environment())
  sys.source(file.path(app_root, "R", "spec.R"), envir = environment())
}

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

# Same reference rule as the adapter: ignore "i.e." / "e.g.", and only
# two-letter domains, SUPPxx, and the ten ADaM datasets are references.
ref_tokens <- function(text) {
  m <- unlist(regmatches(text, gregexpr("[A-Za-z][A-Za-z0-9]*\\.[A-Za-z][A-Za-z0-9_]*", text, perl = TRUE)))
  if (!length(m)) return(character())
  m <- unique(toupper(m))
  m <- m[!m %in% c("I.E", "E.G")]
  dom <- sub("\\..*$", "", m)
  m[nchar(dom) == 2L | startsWith(dom, "SUPP") | dom %in% adam]
}

token_domain <- function(tok) sub("\\..*$", "", tok)
token_var <- function(tok) sub("^[^.]*\\.", "", tok)

test_that("sourcing the adapter does not launch the command-line entry", {
  expect_false(adapt_launched())
})

test_that("adapted specs keep resolvable references, keys, and a loadable shape", {
  expect_true(dir.exists(spec_root))
  expect_true(file.exists(file.path(app_root, "examples", "pharmaverse", "README.md")))
  # The pilot specs are the input and stay intact.
  pilot_adsl <- read_chr_csv(file.path(pilot_root, "adsl", "variable.csv"))
  expect_equal(nrow(pilot_adsl), 48L)
  expect_true("EDUCLVL" %in% pilot_adsl$Variable)
  expect_true("MMSETOT" %in% pilot_adsl$Variable)

  var_by <- list()
  par_by <- list()
  blobs <- character()
  for (ds in datasets) {
    dest <- file.path(spec_root, ds)
    vpath <- file.path(dest, "variable.csv")
    ppath <- file.path(dest, "parameter.csv")
    xpath <- file.path(dest, paste0(ds, "_spec.xlsx"))
    expect_true(file.exists(vpath))
    expect_true(file.exists(ppath))
    expect_true(file.exists(xpath))
    sheets <- readxl::excel_sheets(xpath)
    expect_equal(sheets, c("Variable", "Parameter", "Datasets", "ValueLevel",
                           "Codelist", "Methods", "Notes", "Changes"))
    v <- read_chr_csv(vpath)
    p <- read_chr_csv(ppath)
    expect_equal(names(v), c("Dataset", "Variable", "Label", "Type", "Length", "Key", "Source", "Derivation"))
    expect_equal(names(p), c("Dataset", "PARAMCD", "PARAM", "Source", "Derivation"))
    expect_true(nrow(v) >= 1L)
    expect_true(all(v$Dataset == toupper(ds)))
    if (nrow(p)) expect_true(all(p$Dataset == toupper(ds)))
    ks <- v$Key[nzchar(v$Key)]
    expect_gte(length(ks), 1L)
    expect_equal(sort(as.integer(ks)), seq_along(ks))
    protected <- c("USUBJID", "STUDYID", "PARAMCD", "PARAM", "AVAL")
    # Per-dataset protected names that the pilot had must still be present.
    pilot_v <- read_chr_csv(file.path(pilot_root, ds, "variable.csv"))
    must <- intersect(protected, pilot_v$Variable)
    expect_true(all(must %in% v$Variable))
    # Keys from the pilot are never removed.
    pilot_keys <- pilot_v$Variable[nzchar(pilot_v$Key)]
    expect_true(all(pilot_keys %in% v$Variable))

    spec <- read_spec(vpath, ppath)
    expect_equal(spec$datasets, toupper(ds))
    blocked <- Filter(function(issue) issue$type %in% c("structure", "no_key"), spec$problems)
    expect_length(blocked, 0L)

    ch <- sheet_frame(xpath, "Changes")
    expect_equal(names(ch), c("Dataset", "Variable", "Action", "Reason", "EmptyReference", "CascadeFrom"))
    if (nrow(ch)) expect_true(all(ch$Action %in% c("deleted", "renamed", "text-updated")))
    nt <- sheet_frame(xpath, "Notes")
    blockers <- nt[nt$Topic == "blocker", , drop = FALSE]
    if (nrow(blockers)) {
      expect_true(all(startsWith(blockers$Message, "BLOCKER:")))
      expect_true(all(blockers$Variable %in% v$Variable))
    }
    # A text-updated row recorded an empty reference, and that token is gone
    # from the variable's Source and Derivation.
    updated <- ch[ch$Action == "text-updated", , drop = FALSE]
    for (i in seq_len(nrow(updated))) {
      row <- v[v$Variable == updated$Variable[i], , drop = FALSE]
      expect_equal(nrow(row), 1L)
      refs <- strsplit(updated$EmptyReference[i], " | ", fixed = TRUE)[[1]]
      for (ref in refs[nzchar(refs)]) {
        expect_false(grepl(ref, paste(row$Source, row$Derivation), fixed = TRUE))
      }
    }

    cl <- sheet_frame(xpath, "Codelist")
    if (nrow(cl) && "OID" %in% names(cl)) {
      pc <- cl[cl$OID == paste0("PARAMCD_", toupper(ds)), , drop = FALSE]
      if (nrow(pc)) expect_true(all(pc$Code %in% p$PARAMCD))
    }
    vl <- sheet_frame(xpath, "ValueLevel")
    if (nrow(vl)) {
      expect_true(all(vl$Variable %in% v$Variable))
      if ("PARAMCD" %in% names(vl)) {
        coded <- vl$PARAMCD[nzchar(vl$PARAMCD)]
        if (length(coded)) expect_true(all(coded %in% p$PARAMCD))
      }
    }

    var_by[[toupper(ds)]] <- v$Variable
    par_by[[toupper(ds)]] <- if (nrow(p)) p$PARAMCD else character()
    blobs <- c(blobs, v$Source, v$Derivation, p$Source, p$Derivation,
               if (nrow(vl) && "Comment" %in% names(vl)) vl$Comment else character())
  }

  # Anchors for the pharmaverse gaps: SC and QS are not in the package.
  adsl <- read_chr_csv(file.path(spec_root, "adsl", "variable.csv"))
  expect_false(any(adsl$Variable %in% c("EDUCLVL", "MMSETOT")))
  expect_equal(nrow(read_chr_csv(file.path(spec_root, "adqsadas", "parameter.csv"))), 0L)
  expect_equal(nrow(read_chr_csv(file.path(spec_root, "adqscibc", "parameter.csv"))), 0L)
  expect_equal(nrow(read_chr_csv(file.path(spec_root, "adqsnpix", "parameter.csv"))), 0L)
  for (ds in c("adqsadas", "adqscibc", "adqsnpix")) {
    v <- read_chr_csv(file.path(spec_root, ds, "variable.csv"))
    expect_equal(v$Source[v$Variable == "EFFFL"], "ADSL.EFFFL")
    notes <- sheet_frame(file.path(spec_root, ds, paste0(ds, "_spec.xlsx")), "Notes")
    expect_true(any(notes$Topic == "blocker" & notes$Variable == "ADT"))
    expect_true(any(notes$Topic == "blocker" & notes$Variable == "AVAL"))
  }

  skip_if_not(dir.exists(xpt_dir) && file.exists(file.path(xpt_dir, "dm.xpt")),
              "pharmaversesdtm transports are not on this machine")
  ver <- readLines(file.path(xpt_dir, "VERSION.txt"), n = 1L, warn = FALSE)
  expect_match(ver, "^pharmaversesdtm ")
  expect_false(file.exists(file.path(xpt_dir, "sdg_db.xpt")))
  expect_false(file.exists(file.path(xpt_dir, "smq_db.xpt")))
  expect_false(file.exists(file.path(xpt_dir, "qs.xpt")))
  expect_false(file.exists(file.path(xpt_dir, "sc.xpt")))
  expect_true(file.exists(file.path(xpt_dir, "ae_ophtha.xpt")))
  dm <- haven::read_xpt(file.path(xpt_dir, "dm.xpt"))
  study_label <- attr(dm$STUDYID, "label")
  expect_true(!is.null(study_label) && length(study_label) == 1L && !is.na(study_label) && nzchar(study_label))

  files <- list.files(xpt_dir, pattern = "[.]xpt$", ignore.case = TRUE)
  stems <- toupper(sub("[.]xpt$", "", files, ignore.case = TRUE))
  cols <- new.env(parent = emptyenv(), hash = TRUE)
  qnam <- new.env(parent = emptyenv(), hash = TRUE)
  load_dom <- function(dom) {
    if (!is.null(cols[[dom]])) return()
    hit <- files[stems == dom]
    expect_equal(length(hit), 1L)
    df <- haven::read_xpt(file.path(xpt_dir, hit))
    cols[[dom]] <- toupper(names(df))
    if ("QNAM" %in% toupper(names(df))) {
      values <- unique(toupper(trimws(as.character(df[[match("QNAM", toupper(names(df)))]]))))
      qnam[[dom]] <- values[!is.na(values) & nzchar(values)]
    }
  }

  bad <- character()
  for (text in blobs) {
    for (tk in ref_tokens(text)) {
      dom <- token_domain(tk)
      var <- token_var(tk)
      if (dom %in% adam) {
        if (!(var %in% var_by[[dom]])) bad <- c(bad, tk)
      } else {
        load_dom(dom)
        known <- cols[[dom]]
        qn <- qnam[[dom]]
        if (is.null(known) || !(var %in% known || (!is.null(qn) && var %in% qn))) bad <- c(bad, tk)
      }
    }
  }
  expect_equal(unique(bad), character())
})
