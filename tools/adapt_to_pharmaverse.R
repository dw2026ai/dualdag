# Adapt the CDISC pilot per-dataset ADaM specs to pharmaversesdtm.
#   Rscript tools/adapt_to_pharmaverse.R export [xpt_dir]
#   Rscript tools/adapt_to_pharmaverse.R [pilot_spec_dir] [xpt_dir] [out_spec_dir]
#
# pharmaversesdtm is data only. sdg_db and smq_db are MedDRA lookup tables, not SDTM.
# SAS V5 transport cannot store a member name longer than 8 characters, so those
# datasets are written as V8. Every other dataset is V5. Column labels are kept.
# haven also rejects a dataset label longer than 40 characters, so those labels
# are truncated and the original is recorded in VERSION.txt.

dualdag_root <- function() {
  x <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in seq_len(6L)) {
    if (file.exists(file.path(x, "tools", "define_to_spec.R"))) return(x)
    parent <- dirname(x)
    if (parent == x) break
    x <- parent
  }
  stop("Cannot find the dualdag root from ", getwd(), call. = FALSE)
}

if (!exists("style_sheet", mode = "function")) {
  source(file.path(dualdag_root(), "tools", "define_to_spec.R"), local = FALSE)
}

ADAM_DATASETS <- c("ADSL", "ADAE", "ADLBC", "ADLBH", "ADLBHY",
                   "ADQSADAS", "ADQSCIBC", "ADQSNPIX", "ADTTE", "ADVS")
PROTECTED <- c("USUBJID", "STUDYID", "PARAMCD", "PARAM", "AVAL")
# BDS families whose parameters are test codes in one SDTM domain.
PARAM_DOMAIN <- c(ADVS = "VS", ADLBC = "LB", ADLBH = "LB", ADLBHY = "LB",
                  ADQSADAS = "QS", ADQSCIBC = "QS", ADQSNPIX = "QS")
SKIP_OBJECTS <- c("sdg_db", "smq_db")
TOKEN_RE <- "[A-Za-z][A-Za-z0-9]*\\.[A-Za-z][A-Za-z0-9_]*"

re_escape <- function(x) gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x, perl = TRUE)
BLOCKER_TEXT <- "Kept because this variable is a key or a required analysis variable. The source reference is not in the pharmaverse SDTM data. See the Changes sheet."

blank_na <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    v <- df[[nm]]
    if (is.list(v)) v <- as.character(v)
    v <- as.character(v)
    v[is.na(v)] <- ""
    df[[nm]] <- v
  }
  rownames(df) <- NULL
  df
}

read_sheet <- function(path, sheet) {
  df <- readxl::read_excel(path, sheet = sheet, col_types = "text")
  blank_na(df)
}

empty_sheet <- function(df) {
  out <- df[0, , drop = FALSE]
  rownames(out) <- NULL
  out
}

export_pharmaversesdtm <- function(xpt_dir = path.expand("~/dualdag-demo/pharmaversesdtm"), domains = NULL) {
  xpt_dir <- path.expand(xpt_dir)
  dir.create(xpt_dir, recursive = TRUE, showWarnings = FALSE)
  suppressPackageStartupMessages(library(pharmaversesdtm))
  ver <- as.character(utils::packageVersion("pharmaversesdtm"))
  objs <- data(package = "pharmaversesdtm")$results[, "Item"]
  objs <- objs[!objs %in% SKIP_OBJECTS]
  if (!is.null(domains)) objs <- intersect(objs, tolower(domains))
  lines <- c(paste("pharmaversesdtm", ver), "skipped: sdg_db, smq_db (not SDTM domains)")
  for (nm in objs) {
    df <- get(nm, envir = as.environment("package:pharmaversesdtm"), inherits = FALSE)
    if (!is.data.frame(df)) {
      lines <- c(lines, paste(nm, "skipped-not-dataframe"))
      next
    }
    path <- file.path(xpt_dir, paste0(nm, ".xpt"))
    # haven rejects a dataset label longer than 40 characters on both V5 and V8.
    # Column labels are left unchanged.
    lab <- attr(df, "label", exact = TRUE)
    label_note <- ""
    if (is.character(lab) && length(lab) == 1L && !is.na(lab) && nchar(lab) > 40L) {
      original <- lab
      lab <- substr(lab, 1L, 40L)
      attr(df, "label") <- lab
      label_note <- paste0(" dataset-label-truncated from: ", original)
    }
    version <- if (nchar(nm) <= 8L) 5L else 8L
    write_one <- function(ver) {
      haven::write_xpt(df, path, version = ver, name = nm,
                       label = if (is.null(lab)) NULL else lab)
    }
    wrote <- tryCatch(write_one(version), error = function(e) e)
    if (inherits(wrote, "error") && version == 5L) {
      version <- 8L
      wrote <- tryCatch(write_one(version), error = function(e) e)
    }
    if (inherits(wrote, "error")) stop(wrote)
    lines <- c(lines, paste0(nm, " version ", version, " rows ", nrow(df), " cols ", ncol(df), label_note))
    cat(nm, " v", version, " ", file.info(path)$size, "\n", sep = "")
  }
  writeLines(lines, file.path(xpt_dir, "VERSION.txt"), useBytes = TRUE)
  invisible(ver)
}

load_xpt_meta <- function(xpt_dir) {
  files <- list.files(xpt_dir, pattern = "[.]xpt$", full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) stop("No xpt files in ", xpt_dir, call. = FALSE)
  cols <- new.env(parent = emptyenv(), hash = TRUE)
  testcd <- new.env(parent = emptyenv(), hash = TRUE)
  qnam <- new.env(parent = emptyenv(), hash = TRUE)
  for (path in files) {
    dom <- toupper(sub("[.]xpt$", "", basename(path), ignore.case = TRUE))
    df <- haven::read_xpt(path)
    nm <- toupper(names(df))
    cols[[dom]] <- nm
    hit <- grep("TESTCD$", nm, value = TRUE)
    if (length(hit)) {
      values <- unique(toupper(trimws(as.character(df[[match(hit[1], nm)]]))))
      testcd[[dom]] <- values[!is.na(values) & nzchar(values)]
    }
    if ("QNAM" %in% nm) {
      values <- unique(toupper(trimws(as.character(df[[match("QNAM", nm)]]))))
      qnam[[dom]] <- values[!is.na(values) & nzchar(values)]
    }
  }
  list(cols = cols, testcd = testcd, qnam = qnam, dir = xpt_dir)
}

ref_tokens <- function(text) {
  m <- unlist(regmatches(text, gregexpr(TOKEN_RE, text, perl = TRUE)))
  if (!length(m)) return(character())
  unique(toupper(m))
}

# I.E and E.G are the words "i.e." and "e.g.", not datasets.
real_tokens <- function(text) {
  toks <- ref_tokens(text)
  toks <- toks[!toks %in% c("I.E", "E.G")]
  if (!length(toks)) return(character())
  dom <- sub("\\..*$", "", toks)
  keep <- nchar(dom) == 2L | startsWith(dom, "SUPP") | dom %in% ADAM_DATASETS
  toks[keep]
}

token_domain <- function(tok) sub("\\..*$", "", tok)
token_var <- function(tok) sub("^[^.]*\\.", "", tok)

sdtm_ok <- function(tok, meta) {
  dom <- token_domain(tok)
  var <- token_var(tok)
  if (dom %in% ADAM_DATASETS) return(TRUE)
  known <- meta$cols[[dom]]
  if (is.null(known)) return(FALSE)
  if (var %in% known) return(TRUE)
  qn <- meta$qnam[[dom]]
  !is.null(qn) && var %in% qn
}

mentions_name <- function(text, var) {
  grepl(paste0("(^|[^A-Za-z0-9_])", var, "([^A-Za-z0-9_]|$)", collapse = ""), text, perl = TRUE)
}

strip_tokens <- function(text, toks) {
  out <- text
  for (tk in toks) {
    out <- gsub(paste0("(^|[^A-Za-z0-9_])", re_escape(tk), "([^A-Za-z0-9_]|$)"), "\\1 \\2",
                out, ignore.case = TRUE, perl = TRUE)
  }
  out <- gsub("[[:space:]]+", " ", out, perl = TRUE)
  out <- gsub(" ([,;:|])", "\\1", out, perl = TRUE)
  trimws(out)
}

retarget_text <- function(text, child_var, child_label, catalog) {
  toks <- real_tokens(text)
  toks <- toks[token_domain(toks) %in% ADAM_DATASETS]
  out <- text
  notes <- character()
  for (tk in toks) {
    dom <- token_domain(tk)
    var <- token_var(tk)
    have <- catalog$Variable[catalog$Dataset == dom]
    labels <- catalog$Label[catalog$Dataset == dom]
    if (var %in% have) next
    repl <- ""
    if (child_var %in% have) repl <- child_var
    if (!nzchar(repl) && nzchar(child_label)) {
      hits <- unique(have[labels == child_label])
      if (length(hits) == 1L) repl <- hits
    }
    if (!nzchar(repl)) next
    new_tk <- paste(dom, repl, sep = ".")
    out <- gsub(re_escape(tk), new_tk, out, ignore.case = TRUE, perl = TRUE)
    notes <- c(notes, paste(tk, "->", new_tk))
  }
  list(text = out, notes = unique(notes))
}

param_test_problem <- function(ds, paramcd, source, derivation, meta) {
  # This R errors on a missing name with `[[`, so match instead of subsetting.
  hit <- match(ds, names(PARAM_DOMAIN))
  if (is.na(hit)) return("")
  dom <- PARAM_DOMAIN[[hit]]
  if (is.null(dom) || is.na(dom) || !nzchar(dom)) return("")
  if (is.null(meta$cols[[dom]])) {
    return(paste0(dom, " is not a dataset in pharmaversesdtm, so its test codes do not occur."))
  }
  values <- meta$testcd[[dom]]
  if (is.null(values)) values <- character()
  blob <- toupper(paste(source, derivation))
  asks_paramcd <- grepl(paste0(dom, "TESTCD[[:space:]]*=[[:space:]]*PARAMCD"), blob, perl = TRUE) ||
    grepl("TESTCD[[:space:]]*=[[:space:]]*PARAMCD", blob, perl = TRUE)
  copies_result <- grepl(paste0(dom, "\\.", dom, "STRESN"), blob, perl = TRUE)
  if (asks_paramcd || (copies_result && grepl("^[A-Z][A-Z0-9]*$", paramcd))) {
    if (!paramcd %in% values) {
      return(paste0(paramcd, " never occurs as ", dom, "TESTCD."))
    }
  }
  if (startsWith(paramcd, "_")) {
    base <- sub("^_", "", paramcd)
    if (!base %in% values) {
      return(paste0("Base test ", base, " never occurs as ", dom, "TESTCD."))
    }
  }
  ""
}

bad_sdtm <- function(text, meta) {
  toks <- real_tokens(text)
  toks <- toks[!token_domain(toks) %in% ADAM_DATASETS]
  toks[!vapply(toks, sdtm_ok, FALSE, meta = meta)]
}

bad_adam <- function(text, alive) {
  toks <- real_tokens(text)
  toks <- toks[token_domain(toks) %in% ADAM_DATASETS]
  toks[!vapply(toks, function(tk) paste(token_domain(tk), token_var(tk), sep = "\r") %in% alive, FALSE)]
}

dead_names_in <- function(text, ds, dead) {
  hits <- dead$Variable[dead$Dataset == ds]
  if (!length(hits)) return(character())
  hits[vapply(hits, function(v) mentions_name(text, v), FALSE)]
}

adapt_pharmaverse <- function(pilot_root = file.path(dualdag_root(), "examples", "cdiscpilot01", "spec"),
                              xpt_dir = path.expand("~/dualdag-demo/pharmaversesdtm"),
                              out_root = file.path(dualdag_root(), "examples", "pharmaverse", "spec"),
                              readme_path = file.path(dualdag_root(), "examples", "pharmaverse", "README.md")) {
  meta <- load_xpt_meta(xpt_dir)
  folders <- intersect(tolower(ADAM_DATASETS), list.dirs(pilot_root, full.names = FALSE, recursive = FALSE))
  if (!length(folders)) stop("No per-dataset specs under ", pilot_root, call. = FALSE)

  pieces <- lapply(folders, function(folder) {
    ds <- toupper(folder)
    base <- file.path(pilot_root, folder, paste0(folder, "_spec.xlsx"))
    variable <- read_sheet(base, "Variable")
    parameter <- read_sheet(base, "Parameter")
    list(
      dataset = ds,
      variable = variable,
      parameter = parameter,
      datasets = read_sheet(base, "Datasets"),
      value_level = read_sheet(base, "ValueLevel"),
      codelist = read_sheet(base, "Codelist"),
      methods = read_sheet(base, "Methods"),
      notes = read_sheet(base, "Notes")
    )
  })
  names(pieces) <- vapply(pieces, function(p) p$dataset, "")

  catalog <- do.call(rbind, lapply(pieces, function(p) {
    data.frame(Dataset = p$dataset, Variable = p$variable$Variable, Label = p$variable$Label,
               stringsAsFactors = FALSE)
  }))
  rownames(catalog) <- NULL

  changes <- list()
  add_change <- function(ds, variable, action, reason, empty_ref, cascade) {
    changes[[length(changes) + 1L]] <<- data.frame(
      Dataset = ds, Variable = variable, Action = action, Reason = reason,
      EmptyReference = empty_ref, CascadeFrom = cascade, stringsAsFactors = FALSE
    )
  }

  for (ds in names(pieces)) {
    v <- pieces[[ds]]$variable
    for (i in seq_len(nrow(v))) {
      src <- retarget_text(v$Source[i], v$Variable[i], v$Label[i], catalog)
      der <- retarget_text(v$Derivation[i], v$Variable[i], v$Label[i], catalog)
      moved <- unique(c(src$notes, der$notes))
      if (length(moved)) {
        add_change(ds, v$Variable[i], "renamed",
                   paste("Retargeted to the variable that is actually in the spec:", paste(moved, collapse = "; ")),
                   paste(sub(" ->.*", "", moved), collapse = " | "), "")
      }
      v$Source[i] <- src$text
      v$Derivation[i] <- der$text
    }
    p <- pieces[[ds]]$parameter
    if (nrow(p)) {
      for (i in seq_len(nrow(p))) {
        src <- retarget_text(p$Source[i], p$PARAMCD[i], p$PARAM[i], catalog)
        der <- retarget_text(p$Derivation[i], p$PARAMCD[i], p$PARAM[i], catalog)
        moved <- unique(c(src$notes, der$notes))
        if (length(moved)) {
          add_change(ds, p$PARAMCD[i], "renamed",
                     paste("Parameter retargeted:", paste(moved, collapse = "; ")),
                     paste(sub(" ->.*", "", moved), collapse = " | "), "")
        }
        p$Source[i] <- src$text
        p$Derivation[i] <- der$text
      }
    }
    pieces[[ds]]$variable <- v
    pieces[[ds]]$parameter <- p
  }

  alive <- paste(catalog$Dataset, catalog$Variable, sep = "\r")
  protected <- function(ds, variable, key) {
    nzchar(key) || variable %in% PROTECTED
  }

  # Direct empties: missing SDTM, missing ADaM targets, parameter test codes.
  for (ds in names(pieces)) {
    v <- pieces[[ds]]$variable
    drop <- rep(FALSE, nrow(v))
    for (i in seq_len(nrow(v))) {
      text <- paste(v$Source[i], v$Derivation[i])
      miss <- c(bad_sdtm(text, meta), bad_adam(text, alive))
      if (!length(miss)) next
      if (protected(ds, v$Variable[i], v$Key[i])) next
      drop[i] <- TRUE
      add_change(ds, v$Variable[i], "deleted",
                 "Source or derivation cites a domain or variable that pharmaversesdtm and the remaining specs do not have.",
                 paste(unique(miss), collapse = " | "), "")
    }
    if (any(drop)) alive <- setdiff(alive, paste(ds, v$Variable[drop], sep = "\r"))
    pieces[[ds]]$drop_var <- drop

    p <- pieces[[ds]]$parameter
    drop_p <- rep(FALSE, nrow(p))
    if (nrow(p)) {
      for (i in seq_len(nrow(p))) {
        text <- paste(p$Source[i], p$Derivation[i])
        reason <- param_test_problem(ds, p$PARAMCD[i], p$Source[i], p$Derivation[i], meta)
        miss <- c(bad_sdtm(text, meta), bad_adam(text, alive))
        if (!nzchar(reason) && !length(miss)) next
        drop_p[i] <- TRUE
        if (!nzchar(reason)) reason <- "Parameter cites a missing domain or variable."
        add_change(ds, p$PARAMCD[i], "deleted", paste0("Parameter: ", reason),
                   paste(unique(miss), collapse = " | "), "")
      }
    }
    pieces[[ds]]$drop_par <- drop_p
  }

  # Cascade until nothing newly depends on a deleted variable.
  repeat {
    dead <- catalog[!paste(catalog$Dataset, catalog$Variable, sep = "\r") %in% alive, , drop = FALSE]
    changed <- FALSE
    for (ds in names(pieces)) {
      v <- pieces[[ds]]$variable
      for (i in which(!pieces[[ds]]$drop_var)) {
        text <- paste(v$Source[i], v$Derivation[i])
        miss_adam <- bad_adam(text, alive)
        miss_bare <- dead_names_in(text, ds, dead)
        # A bare name inside a still-valid DOMAIN.VAR of another field is already in miss_adam.
        miss <- unique(c(miss_adam, miss_bare))
        if (!length(miss)) next
        if (protected(ds, v$Variable[i], v$Key[i])) next
        pieces[[ds]]$drop_var[i] <- TRUE
        alive <- setdiff(alive, paste(ds, v$Variable[i], sep = "\r"))
        changed <- TRUE
        add_change(ds, v$Variable[i], "deleted",
                   "Depends on a variable that was removed.",
                   paste(miss, collapse = " | "), paste(miss, collapse = " | "))
      }
      p <- pieces[[ds]]$parameter
      if (!nrow(p)) next
      for (i in which(!pieces[[ds]]$drop_par)) {
        text <- paste(p$Source[i], p$Derivation[i])
        miss <- unique(c(bad_adam(text, alive), dead_names_in(text, ds, dead)))
        if (!length(miss)) next
        pieces[[ds]]$drop_par[i] <- TRUE
        changed <- TRUE
        add_change(ds, p$PARAMCD[i], "deleted",
                   "Parameter: depends on a variable that was removed.",
                   paste(miss, collapse = " | "), paste(miss, collapse = " | "))
      }
    }
    if (!changed) break
  }

  # Protected variables keep their row. Clear references that still do not resolve.
  blockers <- list()
  for (ds in names(pieces)) {
    v <- pieces[[ds]]$variable
    for (i in which(!pieces[[ds]]$drop_var)) {
      text <- paste(v$Source[i], v$Derivation[i])
      dead <- catalog[!paste(catalog$Dataset, catalog$Variable, sep = "\r") %in% alive, , drop = FALSE]
      miss <- unique(c(bad_sdtm(text, meta), bad_adam(text, alive), dead_names_in(v$Derivation[i], ds, dead)))
      # Bare names that are the variable itself are not missing sources.
      miss <- miss[miss != v$Variable[i]]
      if (!length(miss)) next
      new_src <- strip_tokens(v$Source[i], miss)
      new_der <- strip_tokens(v$Derivation[i], miss)
      if (!length(real_tokens(new_der)) && nchar(new_der) < 40L) new_der <- BLOCKER_TEXT
      if (any(miss %in% real_tokens(v$Source[i])) && !length(real_tokens(new_src))) new_src <- ""
      # If a remaining token is still bad, drop the whole derivation rather than fail the check.
      still <- c(bad_sdtm(paste(new_src, new_der), meta), bad_adam(paste(new_src, new_der), alive))
      if (length(still)) {
        new_src <- ""
        new_der <- BLOCKER_TEXT
      }
      v$Source[i] <- new_src
      v$Derivation[i] <- new_der
      pieces[[ds]]$variable <- v
      why <- if (nzchar(v$Key[i])) "key" else "required variable"
      msg <- paste0("BLOCKER: ", v$Variable[i], " was kept because it is a ", why,
                    ". Empty reference: ", paste(miss, collapse = " | "), ".")
      blockers[[length(blockers) + 1L]] <- data.frame(
        Dataset = ds, Variable = v$Variable[i], Topic = "blocker", Message = msg,
        stringsAsFactors = FALSE
      )
      add_change(ds, v$Variable[i], "text-updated", msg, paste(miss, collapse = " | "), "")
    }
  }

  change_df <- if (length(changes)) do.call(rbind, changes) else data.frame(
    Dataset = character(), Variable = character(), Action = character(), Reason = character(),
    EmptyReference = character(), CascadeFrom = character(), stringsAsFactors = FALSE
  )
  rownames(change_df) <- NULL

  dir.create(out_root, recursive = TRUE, showWarnings = FALSE)
  summary_rows <- list()
  for (ds in names(pieces)) {
    pce <- pieces[[ds]]
    v <- pce$variable[!pce$drop_var, , drop = FALSE]
    par <- if (nrow(pce$parameter)) pce$parameter[!pce$drop_par, , drop = FALSE] else pce$parameter
    rownames(v) <- NULL
    rownames(par) <- NULL
    dead_vars <- pce$variable$Variable[pce$drop_var]
    dead_pars <- if (nrow(pce$parameter)) pce$parameter$PARAMCD[pce$drop_par] else character()

    vl <- pce$value_level
    if (nrow(vl)) {
      keep <- vl$Variable %in% v$Variable
      if ("PARAMCD" %in% names(vl) && nrow(pce$parameter)) {
        # Value-level rows belong to a parameter. Drop rows for deleted parameters.
        keep <- keep & (vl$PARAMCD %in% par$PARAMCD | !nzchar(vl$PARAMCD))
      }
      vl <- vl[keep, , drop = FALSE]
      rownames(vl) <- NULL
    }

    cl <- pce$codelist
    if (nrow(cl) && "OID" %in% names(cl)) {
      pc_oid <- paste0("PARAMCD_", ds)
      pm_oid <- paste0("PARAM_", ds)
      pc <- cl$OID == pc_oid
      pm <- cl$OID == pm_oid
      if (any(pc) && any(pm) && sum(pc) == sum(pm)) {
        # PARAM and PARAMCD codelists were paired in define order.
        keep_code <- cl$Code[pc] %in% par$PARAMCD
        pc_idx <- which(pc)
        pm_idx <- which(pm)
        drop_idx <- c(pc_idx[!keep_code], pm_idx[!keep_code])
        if (length(drop_idx)) cl <- cl[-drop_idx, , drop = FALSE]
      } else if (any(pc)) {
        cl <- cl[!(cl$OID == pc_oid & !cl$Code %in% par$PARAMCD), , drop = FALSE]
      }
      rownames(cl) <- NULL
    }

    meth <- pce$methods
    if (nrow(meth) && "Text" %in% names(meth)) {
      gone <- c(dead_vars, dead_pars)
      if (length(gone)) {
        blob <- paste(meth$OID, meth$Name, meth$Text)
        hit_names <- lapply(blob, function(t) gone[vapply(gone, function(g) mentions_name(t, g), FALSE)])
        hit <- lengths(hit_names) > 0L
        if (any(hit)) {
          for (i in which(hit)) {
            add_change(ds, meth$OID[i], "deleted",
                       "Method: text names a variable or parameter that was removed.",
                       paste(hit_names[[i]], collapse = " | "),
                       paste(hit_names[[i]], collapse = " | "))
          }
          meth <- meth[!hit, , drop = FALSE]
          rownames(meth) <- NULL
        }
      }
    }

    nt <- pce$notes
    if (nrow(nt)) {
      study <- !nzchar(nt$Dataset)
      keep_var <- !nzchar(nt$Variable) | nt$Variable %in% v$Variable | nt$Variable %in% par$PARAMCD
      nt <- nt[study | keep_var, , drop = FALSE]
      rownames(nt) <- NULL
    }
    extra <- Filter(function(b) b$Dataset == ds, blockers)
    if (length(extra)) {
      extra_df <- do.call(rbind, extra)
      # Notes columns are Dataset, Variable, Topic, Message. Match the pilot sheet.
      extra_df <- extra_df[, c("Dataset", "Variable", "Topic", "Message"), drop = FALSE]
      nt <- rbind(extra_df, nt)
    }

    ch <- change_df[change_df$Dataset == ds, , drop = FALSE]
    # Method deletions were appended after change_df was snapshotted. Rebuild from the closure list.
    ch <- if (length(changes)) do.call(rbind, changes) else change_df
    ch <- ch[ch$Dataset == ds, , drop = FALSE]
    rownames(ch) <- NULL

    dest <- file.path(out_root, tolower(ds))
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    xlsx <- file.path(dest, paste0(tolower(ds), "_spec.xlsx"))
    wb <- openxlsx2::wb_workbook()
    sheets <- list(
      list("Variable", v, c(Dataset = 14, Variable = 16, Label = 36, Type = 12, Length = 10, Key = 8, Source = 22, Derivation = 80), "Derivation"),
      list("Parameter", par, c(Dataset = 14, PARAMCD = 16, PARAM = 42, Source = 22, Derivation = 80), "Derivation"),
      list("Datasets", pce$datasets, c(Dataset = 14, Label = 42, Structure = 55, Class = 12, Keys = 36, XptFile = 18, RowCount = 12), "Structure"),
      list("ValueLevel", vl, c(Dataset = 14, Variable = 14, PARAMCD = 14, OID = 28, Comment = 80), "Comment"),
      list("Codelist", cl, c(OID = 28, Name = 28, DataType = 12, Rank = 8, Code = 18, Decode = 55), "Decode"),
      list("Methods", meth, c(OID = 28, Name = 28, Text = 100), "Text"),
      list("Notes", nt, c(Dataset = 14, Variable = 18, Topic = 24, Message = 100), "Message"),
      list("Changes", ch, c(Dataset = 14, Variable = 18, Action = 16, Reason = 55, EmptyReference = 40, CascadeFrom = 28), c("Reason", "EmptyReference"))
    )
    for (s in sheets) {
      wb <- openxlsx2::wb_add_worksheet(wb, s[[1]])
      wb <- style_sheet(wb, s[[1]], blank_na(s[[2]]), s[[3]], s[[4]])
    }
    openxlsx2::wb_save(wb, xlsx, overwrite = TRUE)
    write_utf8_csv(blank_na(v[, c("Dataset", "Variable", "Label", "Type", "Length", "Key", "Source", "Derivation")]),
                   file.path(dest, "variable.csv"))
    write_utf8_csv(blank_na(par[, c("Dataset", "PARAMCD", "PARAM", "Source", "Derivation")]),
                   file.path(dest, "parameter.csv"))

    summary_rows[[ds]] <- data.frame(
      Dataset = ds,
      VariablesBefore = nrow(pce$variable),
      VariablesAfter = nrow(v),
      ParametersBefore = nrow(pce$parameter),
      ParametersAfter = nrow(par),
      Renamed = sum(ch$Action == "renamed"),
      Deleted = sum(ch$Action == "deleted"),
      TextUpdated = sum(ch$Action == "text-updated"),
      stringsAsFactors = FALSE
    )
    cat(ds, " ", nrow(pce$variable), "->", nrow(v), " vars  ",
        nrow(pce$parameter), "->", nrow(par), " params  renamed ",
        sum(ch$Action == "renamed"), "\n", sep = "")
  }

  summary_df <- do.call(rbind, summary_rows)
  rownames(summary_df) <- NULL
  all_changes <- if (length(changes)) do.call(rbind, changes) else change_df
  rownames(all_changes) <- NULL
  if (nzchar(readme_path %||% "")) {
    write_pharmaverse_readme(readme_path, xpt_dir, out_root, pieces, summary_df, all_changes, meta)
  }
  invisible(list(summary = summary_df, changes = all_changes, blockers = if (length(blockers)) do.call(rbind, blockers) else NULL))
}

referenced_domains <- function(text) {
  toks <- real_tokens(text)
  if (!length(toks)) return(character())
  unique(token_domain(toks))
}

write_pharmaverse_readme <- function(path, xpt_dir, out_root, pieces, summary_df, changes, meta) {
  ver <- "unknown"
  ver_file <- file.path(xpt_dir, "VERSION.txt")
  if (file.exists(ver_file)) {
    first <- readLines(ver_file, n = 1L, warn = FALSE)
    ver <- sub("^pharmaversesdtm ", "", first)
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  lines <- c(
    "# Pharmaverse adaptation of the CDISC pilot ADaM specs",
    "",
    "These specs are derived from the CDISC SDTM/ADaM pilot (`cdiscpilot01`) specs in `examples/cdiscpilot01`. The pilot files are unchanged. Each spec was edited so the references that remain point at [pharmaversesdtm](https://cran.r-project.org/package=pharmaversesdtm) " %+% ver %+% " (Apache License 2.0). CDISC's Terms of Use require attribution when the pilot material is distributed.",
    "",
    "## Where the SDTM data lives",
    "",
    paste0("The exported transports are outside this repository, in `", xpt_dir, "`."),
    "Regenerate them with `Rscript tools/adapt_to_pharmaverse.R export`.",
    "Member names of 8 characters or fewer are SAS V5 transport files. Longer names are SAS V8, because V5 rejects them. Column labels are kept.",
    "`sdg_db` and `smq_db` are MedDRA lookup tables and were not exported.",
    "pharmaversesdtm does not ship the pilot `qs` or `sc` domains. It ships `qs_metabolic`, `qs_ophtha`, and `sc_ophtha`, which are different studies, so those names were not substituted.",
    "",
    "## Regenerate the specs",
    "",
    "```sh",
    "Rscript tools/adapt_to_pharmaverse.R export",
    "Rscript tools/adapt_to_pharmaverse.R",
    "```",
    "",
    "Each `spec/<dataset>/` folder has `<dataset>_spec.xlsx` (the pilot sheets plus `Changes`) and `variable.csv` / `parameter.csv` for dualdag.",
    "",
    "## Counts",
    "",
    "| Dataset | Variables before | Variables after | Parameters before | Parameters after | Renamed |",
    "| --- | ---: | ---: | ---: | ---: | ---: |"
  )
  for (i in seq_len(nrow(summary_df))) {
    r <- summary_df[i, ]
    lines <- c(lines, paste0("| ", r$Dataset, " | ", r$VariablesBefore, " | ", r$VariablesAfter,
                             " | ", r$ParametersBefore, " | ", r$ParametersAfter, " | ", r$Renamed, " |"))
  }
  lines <- c(lines, "", "## Transports to upload with each dataset", "",
             "SDTM files are in the pharmaverse export directory. Another ADaM dataset is this adapted spec, not a pharmaverse transport. A dataset is listed only when a remaining Source or Derivation still names it.",
             "",
             "| ADaM dataset | Upload |",
             "| --- | --- |")
  for (ds in summary_df$Dataset) {
    vpath <- file.path(out_root, tolower(ds), "variable.csv")
    ppath <- file.path(out_root, tolower(ds), "parameter.csv")
    vv <- utils::read.csv(vpath, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(), check.names = FALSE)
    pp <- utils::read.csv(ppath, stringsAsFactors = FALSE, colClasses = "character", na.strings = character(), check.names = FALSE)
    blob <- paste(c(vv$Source, vv$Derivation, pp$Source, pp$Derivation), collapse = "\n")
    doms <- referenced_domains(blob)
    doms <- doms[nzchar(doms) & doms != ds]
    sdtm <- doms[!doms %in% ADAM_DATASETS]
    adam <- doms[doms %in% ADAM_DATASETS]
    if (length(sdtm)) {
      sdtm <- sdtm[vapply(sdtm, function(d) !is.null(meta$cols[[d]]), FALSE)]
    }
    # paste0(character(0), x) is " x" in this R, so only paste non-empty vectors.
    files <- character()
    if (length(adam)) files <- c(files, paste0(tolower(adam), " spec"))
    if (length(sdtm)) files <- c(files, paste0(tolower(sdtm), ".xpt"))
    if (!length(files)) files <- "(none)"
    lines <- c(lines, paste0("| ", ds, " | ", paste(files, collapse = ", "), " |"))
  }
  blockers <- changes[changes$Action == "text-updated", , drop = FALSE]
  lines <- c(lines, "", "## Blockers", "")
  if (!nrow(blockers)) {
    lines <- c(lines, "No key or required variable had to be kept against a missing reference.")
  } else {
    lines <- c(lines, "These rows were kept because they are keys or `USUBJID` / `STUDYID` / `PARAMCD` / `PARAM` / `AVAL`. The missing reference was removed from Source and Derivation and recorded on `Changes` and `Notes`.", "")
    for (i in seq_len(nrow(blockers))) {
      lines <- c(lines, paste0("- ", blockers$Dataset[i], ".", blockers$Variable[i], ": ", blockers$EmptyReference[i]))
    }
  }
  lines <- c(lines, "",
             paste0("Changes in total: ", sum(changes$Action == "deleted"), " deleted, ",
                    sum(changes$Action == "renamed"), " renamed, ",
                    sum(changes$Action == "text-updated"), " text-updated."),
             "")
  writeLines(lines, path, useBytes = TRUE)
}

`%+%` <- function(a, b) paste0(a, b)

adapt_launched <- function() {
  f <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(f) == 1L && grepl("adapt_to_pharmaverse\\.R$", sub("^--file=", "", f))
}

if (adapt_launched()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) && identical(args[[1]], "export")) {
    export_pharmaversesdtm(if (length(args) >= 2L) args[[2]] else path.expand("~/dualdag-demo/pharmaversesdtm"))
  } else if (length(args) >= 3L) {
    adapt_pharmaverse(args[[1]], args[[2]], args[[3]])
  } else {
    adapt_pharmaverse()
  }
}
