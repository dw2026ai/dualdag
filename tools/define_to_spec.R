# Convert a Define-XML 1.0 ADaM define.xml into a dualdag spec workbook.
#   Rscript tools/define_to_spec.R <define.xml> <outdir>
# Writes <outdir>/cdiscpilot01_adam_spec.xlsx plus variable.csv and parameter.csv,
# and one folder per dataset: <outdir>/<dataset>/<dataset>_spec.xlsx with the same two CSVs.

# xml2 / haven / openxlsx2 are called by namespace. openxlsx2 masks xml2::read_xml.

norm_ws <- function(x) {
  x <- paste(x %||% "")
  x <- gsub("[\r\n\t]+", " ", x, perl = TRUE)
  x <- gsub(" +", " ", x, perl = TRUE)
  trimws(x)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# A comment that is only DATASET.VARIABLE is a copy. Anything longer stays a derivation.
ref_token <- function(text) {
  t <- toupper(norm_ws(text))
  if (grepl("^[A-Z][A-Z0-9]*\\.[A-Z][A-Z0-9_]*$", t)) t else ""
}

at <- function(node, name) {
  if (inherits(node, "xml_missing") || is.null(node)) return("")
  v <- xml2::xml_attr(node, name)
  if (length(v) != 1L || is.na(v)) "" else v
}

kids <- function(node, name) {
  xml2::xml_find_all(node, sprintf("./*[local-name()='%s']", name))
}

blank_df <- function(cols) {
  df <- as.data.frame(replicate(length(cols), character(), simplify = FALSE), stringsAsFactors = FALSE)
  names(df) <- cols
  df
}

as_chr_df <- function(df) {
  for (nm in names(df)) {
    v <- df[[nm]]
    v[is.na(v)] <- ""
    df[[nm]] <- as.character(v)
  }
  df
}

write_utf8_csv <- function(df, path) {
  df <- as_chr_df(df)
  utils::write.csv(df, path, row.names = FALSE, na = "", fileEncoding = "UTF-8", quote = TRUE)
}

# Identical value-level comments collapse to one sentence. Differing ones stay in full.
vl_text <- function(paramcd, comment) {
  comment <- norm_ws(comment)
  keep <- nzchar(comment)
  paramcd <- paramcd[keep]
  comment <- comment[keep]
  if (!length(comment)) return("")
  uniq <- unique(comment)
  if (length(uniq) == 1L) return(uniq)
  paste(vapply(uniq, function(c) {
    paste0(paste(paramcd[comment == c], collapse = ", "), ": ", c)
  }, ""), collapse = " | ")
}

xpt_meta <- function(path) {
  if (!file.exists(path)) return(NULL)
  df <- haven::read_xpt(path)
  labels <- vapply(df, function(x) {
    lb <- attr(x, "label")
    if (is.null(lb) || !length(lb) || is.na(lb[[1]])) "" else norm_ws(lb[[1]])
  }, "")
  lengths <- vapply(df, function(x) {
    for (a in c("display_width", "width", "length")) {
      v <- attr(x, a)
      if (!is.null(v) && length(v) == 1L && !is.na(v) && nzchar(as.character(v))) return(as.character(v))
    }
    ""
  }, "")
  pc <- if ("PARAMCD" %in% names(df)) trimws(as.character(df$PARAMCD)) else character()
  pm <- if ("PARAM" %in% names(df)) trimws(as.character(df$PARAM)) else character()
  ok <- nzchar(pc) & !is.na(pc)
  pairs <- if (length(ok) && any(ok)) {
    unique(data.frame(PARAMCD = pc[ok], PARAM = pm[ok], stringsAsFactors = FALSE))
  } else {
    data.frame(PARAMCD = character(), PARAM = character(), stringsAsFactors = FALSE)
  }
  list(nrow = nrow(df), names = names(df), labels = labels, lengths = lengths, pairs = pairs)
}

codelist_items <- function(node) {
  ch <- kids(node, "CodeListItem")
  if (!length(ch)) ch <- kids(node, "EnumeratedItem")
  if (!length(ch)) {
    return(data.frame(Rank = character(), Code = character(), Decode = character(), stringsAsFactors = FALSE))
  }
  data.frame(
    Rank = vapply(ch, function(n) { r <- at(n, "Rank"); if (nzchar(r)) r else as.character(at(n, "OrderNumber")) }, ""),
    Code = vapply(ch, function(n) at(n, "CodedValue"), ""),
    Decode = vapply(ch, function(n) {
      d <- xml2::xml_find_first(n, ".//*[local-name()='Decode']")
      txt <- if (inherits(d, "xml_missing")) "" else norm_ws(xml2::xml_text(d))
      if (nzchar(txt)) txt else at(n, "CodedValue")
    }, ""),
    stringsAsFactors = FALSE
  )
}

style_sheet <- function(wb, sheet, df, widths, wrap) {
  df <- as_chr_df(df)
  wb <- openxlsx2::wb_add_data(wb, sheet, df, na = "", with_filter = TRUE)
  hdr <- openxlsx2::wb_dims(rows = 1, cols = seq_len(max(1L, ncol(df))))
  wb <- openxlsx2::wb_add_font(wb, sheet, dims = hdr, bold = TRUE)
  wb <- openxlsx2::wb_freeze_pane(wb, sheet, first_row = TRUE)
  use_w <- rep(18, ncol(df))
  for (nm in names(widths)) {
    i <- match(nm, names(df))
    if (!is.na(i)) use_w[[i]] <- widths[[nm]]
  }
  wb <- openxlsx2::wb_set_col_widths(wb, sheet, cols = seq_len(ncol(df)), widths = use_w)
  wrap <- intersect(wrap, names(df))
  if (length(wrap) && nrow(df)) {
    dims <- openxlsx2::wb_dims(rows = seq_len(nrow(df) + 1L), cols = match(wrap, names(df)))
    wb <- openxlsx2::wb_add_cell_style(wb, sheet, dims = dims, wrap_text = TRUE, vertical = "top")
  }
  wb
}

codelist_oid <- function(node) {
  if (is.null(node)) return("")
  cl_node <- xml2::xml_find_first(node, "./*[local-name()='CodeListRef']")
  at(cl_node, "CodeListOID")
}

# Analysis-display ComputationMethods name the datasets they use via ItemGroupRef.
method_datasets <- function(doc) {
  empty <- data.frame(OID = character(), Dataset = character(), stringsAsFactors = FALSE)
  ars <- xml2::xml_find_all(doc, ".//*[local-name()='AnalysisResults']")
  rows <- list()
  for (ar in ars) {
    groups <- xml2::xml_find_all(ar, ".//*[local-name()='ItemGroupRef']")
    ds <- unique(vapply(groups, function(n) toupper(at(n, "ItemGroupOID")), ""))
    ds <- ds[nzchar(ds)]
    cms <- xml2::xml_find_all(ar, ".//*[local-name()='ComputationMethod']")
    oids <- vapply(cms, function(n) at(n, "OID"), "")
    oids <- unique(oids[nzchar(oids)])
    if (!length(ds) || !length(oids)) next
    rows[[length(rows) + 1L]] <- data.frame(
      OID = rep(oids, each = length(ds)),
      Dataset = rep(ds, times = length(oids)),
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(empty)
  out <- unique(do.call(rbind, rows))
  rownames(out) <- NULL
  out
}

slice_df <- function(df, keep) {
  out <- if (!length(keep) || !any(keep)) df[0, , drop = FALSE] else df[keep, , drop = FALSE]
  rownames(out) <- NULL
  out
}

write_spec_workbook <- function(path, variable, parameter, datasets, value_level, codelist, methods, notes) {
  wb <- openxlsx2::wb_workbook()
  sheets <- list(
    list("Variable", variable, c(Dataset = 14, Variable = 16, Label = 36, Type = 12, Length = 10, Key = 8, Source = 22, Derivation = 80), "Derivation"),
    list("Parameter", parameter, c(Dataset = 14, PARAMCD = 16, PARAM = 42, Source = 22, Derivation = 80), "Derivation"),
    list("Datasets", datasets, c(Dataset = 14, Label = 42, Structure = 55, Class = 12, Keys = 36, XptFile = 18, RowCount = 12), "Structure"),
    list("ValueLevel", value_level, c(Dataset = 14, Variable = 14, PARAMCD = 14, OID = 28, Type = 12, Length = 10, Label = 28, Origin = 14, Comment = 80), "Comment"),
    list("Codelist", codelist, c(OID = 28, Name = 28, DataType = 12, Rank = 8, Code = 18, Decode = 55), "Decode"),
    list("Methods", methods, c(OID = 28, Name = 28, Text = 100), "Text"),
    list("Notes", notes, c(Dataset = 14, Variable = 18, Topic = 24, Message = 100), "Message")
  )
  for (s in sheets) {
    wb <- openxlsx2::wb_add_worksheet(wb, s[[1]])
    wb <- style_sheet(wb, s[[1]], s[[2]], s[[3]], s[[4]])
  }
  openxlsx2::wb_save(wb, path, overwrite = TRUE)
  invisible(path)
}

define_to_spec <- function(define_path, outdir, adam_dir = NULL,
                           workbook = "cdiscpilot01_adam_spec.xlsx") {
  if (!file.exists(define_path)) stop("define.xml not found: ", define_path, call. = FALSE)
  if (is.null(adam_dir) || !nzchar(adam_dir)) adam_dir <- dirname(normalizePath(define_path))
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  doc <- xml2::read_xml(define_path)

  item_nodes <- xml2::xml_find_all(doc, ".//*[local-name()='ItemDef']")
  item_oid <- vapply(item_nodes, function(n) at(n, "OID"), "")
  items <- new.env(parent = emptyenv(), hash = TRUE)
  for (i in seq_along(item_nodes)) items[[item_oid[[i]]]] <- item_nodes[[i]]

  groups <- xml2::xml_find_all(doc, ".//*[local-name()='ItemGroupDef']")
  ds_names <- vapply(groups, function(n) toupper(at(n, "Name")), "")

  # Value-level ItemRefs, keyed by parent dataset variable.
  cl_env <- new.env(parent = emptyenv(), hash = TRUE)
  remember_cl <- function(ds, oid) {
    if (!nzchar(ds) || !nzchar(oid)) return()
    cur <- cl_env[[ds]]
    if (is.null(cur)) cl_env[[ds]] <- oid
    else if (!oid %in% cur) cl_env[[ds]] <- c(cur, oid)
  }

  vl_defs <- xml2::xml_find_all(doc, ".//*[local-name()='ValueListDef']")
  vl_rows <- list()
  vl_for <- list()
  for (vl in vl_defs) {
    vl_oid <- at(vl, "OID")
    rest <- sub("^ValueList\\.", "", vl_oid)
    ds <- ""
    for (d in ds_names) if (startsWith(rest, paste0(d, "."))) ds <- d
    if (!nzchar(ds)) next
    parent <- sub(paste0("^", ds, "\\."), "", rest)
    refs <- kids(vl, "ItemRef")
    if (!length(refs)) next
    ord <- suppressWarnings(as.integer(vapply(refs, function(n) at(n, "OrderNumber"), "")))
    ord[is.na(ord)] <- 0L
    refs <- refs[order(ord)]
    block <- lapply(refs, function(ref) {
      oid <- at(ref, "ItemOID")
      node <- items[[oid]]
      if (is.null(node)) return(NULL)
      remember_cl(ds, codelist_oid(node))
      data.frame(
        Dataset = ds, Variable = parent, PARAMCD = at(node, "Name"), OID = oid,
        Type = at(node, "DataType"), Length = at(node, "Length"),
        Label = norm_ws(at(node, "Label")), Origin = norm_ws(at(node, "Origin")),
        Comment = norm_ws(at(node, "Comment")), stringsAsFactors = FALSE
      )
    })
    block <- do.call(rbind, Filter(Negate(is.null), block))
    if (is.null(block) || !nrow(block)) next
    key <- paste(ds, parent, sep = ".")
    vl_for[[key]] <- block
    vl_rows[[length(vl_rows) + 1L]] <- block
  }
  value_level <- if (length(vl_rows)) do.call(rbind, vl_rows) else blank_df(
    c("Dataset", "Variable", "PARAMCD", "OID", "Type", "Length", "Label", "Origin", "Comment"))
  rownames(value_level) <- NULL

  lists <- xml2::xml_find_all(doc, ".//*[local-name()='CodeList']")
  cl_by_oid <- new.env(parent = emptyenv(), hash = TRUE)
  codelist_rows <- list()
  for (cl in lists) {
    oid <- at(cl, "OID")
    parsed <- codelist_items(cl)
    cl_by_oid[[oid]] <- parsed
    if (!nrow(parsed)) next
    codelist_rows[[length(codelist_rows) + 1L]] <- data.frame(
      OID = oid, Name = at(cl, "Name"), DataType = at(cl, "DataType"),
      Rank = parsed$Rank, Code = parsed$Code, Decode = parsed$Decode,
      stringsAsFactors = FALSE
    )
  }
  codelist <- if (length(codelist_rows)) do.call(rbind, codelist_rows) else blank_df(
    c("OID", "Name", "DataType", "Rank", "Code", "Decode"))
  rownames(codelist) <- NULL

  methods_nodes <- xml2::xml_find_all(doc, ".//*[local-name()='ComputationMethod']")
  methods <- if (length(methods_nodes)) {
    data.frame(
      OID = vapply(methods_nodes, function(n) at(n, "OID"), ""),
      Name = vapply(methods_nodes, function(n) at(n, "Name"), ""),
      Text = vapply(methods_nodes, function(n) norm_ws(xml2::xml_text(n)), ""),
      stringsAsFactors = FALSE
    )
  } else blank_df(c("OID", "Name", "Text"))
  method_use <- method_datasets(doc)

  notes <- list()
  add_note <- function(dataset, variable, topic, message) {
    notes[[length(notes) + 1L]] <<- data.frame(
      Dataset = dataset, Variable = variable, Topic = topic, Message = message,
      stringsAsFactors = FALSE
    )
  }

  n_itemdef <- length(item_nodes)
  n_group_ref <- sum(vapply(groups, function(g) length(kids(g, "ItemRef")), 0L))
  n_vl_ref <- nrow(value_level)
  add_note("", "", "sheet_scope", paste(
    "Variable has one row per ItemGroupDef ItemRef (", n_group_ref,
    "), the dataset columns. Define-XML also has ", n_itemdef,
    " ItemDef and ", n_group_ref + n_vl_ref,
    " ItemRef in total; the ", n_vl_ref,
    " value-level ItemRefs repeat names inside a dataset and are not xpt columns, so they are on ValueLevel.",
    sep = ""
  ))
  linked_cm <- length(xml2::xml_find_all(doc, ".//*[local-name()='ItemDef'][@*[local-name()='ComputationMethodOID']]"))
  add_note("", "", "computation_method", paste(
    "No ItemDef points at def:ComputationMethodOID (", linked_cm, " links). The ",
    nrow(methods), " ComputationMethod entries are analysis-display text and are stored on Methods, not in Derivation.",
    sep = ""
  ))
  add_note("", "", "origin", "ItemDef Origin on dataset variables is the single word Derived, so it is not repeated in Derivation.")

  var_rows <- list()
  ds_rows <- list()
  for (gi in seq_along(groups)) {
    g <- groups[[gi]]
    ds <- ds_names[[gi]]
    refs <- kids(g, "ItemRef")
    ord <- suppressWarnings(as.integer(vapply(refs, function(n) at(n, "OrderNumber"), "")))
    ord[is.na(ord)] <- 0L
    refs <- refs[order(ord)]
    leaf <- xml2::xml_find_first(g, "./*[local-name()='leaf']")
    href <- at(leaf, "href")
    if (!nzchar(href)) href <- basename(at(leaf, "xlink:href"))
    if (!nzchar(href)) href <- paste0(tolower(ds), ".xpt")
    href <- basename(href)

    built <- lapply(refs, function(ref) {
      oid <- at(ref, "ItemOID")
      node <- items[[oid]]
      if (is.null(node)) {
        add_note(ds, oid, "missing_itemdef", paste("ItemRef", oid, "has no ItemDef."))
        return(NULL)
      }
      var <- toupper(at(node, "Name"))
      comment <- norm_ws(at(node, "Comment"))
      origin <- norm_ws(at(node, "Origin"))
      cl_oid <- codelist_oid(node)
      remember_cl(ds, cl_oid)
      vl <- vl_for[[paste(ds, var, sep = ".")]]
      src <- ref_token(comment)
      extra <- ""
      if (!is.null(vl) && nrow(vl)) {
        extra <- vl_text(vl$PARAMCD, vl$Comment)
        if (!nzchar(src)) {
          u <- unique(norm_ws(vl$Comment))
          u <- u[nzchar(u)]
          if (length(u) == 1L) src <- ref_token(u)
        }
        if (!nzchar(comment)) {
          add_note(ds, var, "value_level_text",
                   "ItemDef Comment is empty. Derivation is the value-level Comment text for this variable.")
        }
      }
      parts <- unique(c(comment, extra))
      parts <- parts[nzchar(parts)]
      # Drop a value-level block that only repeats the variable comment.
      if (length(parts) > 1L) parts <- parts[!parts %in% comment | parts == comment]
      parts <- unique(parts[nzchar(parts)])
      if (nzchar(origin) && !origin %in% c("Derived", "Assigned", "Predecessor")) {
        parts <- c(parts, paste0("Origin: ", origin))
      }
      if (!length(parts) && nzchar(cl_oid)) {
        parts <- paste0("Controlled terminology ", cl_oid, " (see Codelist).")
        add_note(ds, var, "codelist_only",
                 paste("ItemDef Comment is empty. Terms are codelist", cl_oid, "on the Codelist sheet."))
      }
      data.frame(
        Dataset = ds, Variable = var, Label = norm_ws(at(node, "Label")),
        Type = at(node, "DataType"), Length = at(node, "Length"),
        Key = "", Source = src, Derivation = paste(parts, collapse = " | "),
        stringsAsFactors = FALSE
      )
    })
    built <- do.call(rbind, Filter(Negate(is.null), built))
    if (is.null(built)) built <- blank_df(c("Dataset", "Variable", "Label", "Type", "Length", "Key", "Source", "Derivation"))
    key_raw <- trimws(strsplit(at(g, "DomainKeys"), ",", fixed = FALSE)[[1]])
    key_raw <- unique(key_raw[nzchar(key_raw)])
    present <- key_raw[key_raw %in% built$Variable]
    missing_k <- setdiff(key_raw, built$Variable)
    if (length(missing_k)) {
      add_note(ds, paste(missing_k, collapse = ","), "missing_key_var",
               paste("DomainKeys lists", paste(missing_k, collapse = ", "),
                     "but that variable is not in the dataset. Key numbers use the remaining keys in define order."))
    }
    if (!length(present)) {
      present <- if ("USUBJID" %in% built$Variable) "USUBJID" else built$Variable[[1]]
      add_note(ds, present, "derived_key",
               "Define DomainKeys gave no variable that exists here. This variable was marked Key 1.")
    }
    built$Key <- ifelse(built$Variable %in% present, as.character(match(built$Variable, present)), "")
    var_rows[[length(var_rows) + 1L]] <- built

    xpt_path <- file.path(adam_dir, href)
    meta <- xpt_meta(xpt_path)
    if (is.null(meta)) {
      add_note(ds, "", "missing_xpt", paste("xpt not found:", xpt_path))
      n_row <- ""
      xpt_names <- character()
      xpt_labels <- character()
      xpt_lengths <- character()
      xpt_pairs <- data.frame(PARAMCD = character(), PARAM = character())
    } else {
      n_row <- as.character(meta$nrow)
      xpt_names <- meta$names
      xpt_labels <- meta$labels
      xpt_lengths <- meta$lengths
      xpt_pairs <- meta$pairs
      for (v in setdiff(built$Variable, xpt_names)) {
        add_note(ds, v, "not_in_xpt", "Variable is in the define ItemGroup but not in the xpt.")
      }
      for (v in setdiff(xpt_names, built$Variable)) {
        add_note(ds, v, "not_in_define", "Variable is in the xpt but not in the define ItemGroup.")
      }
      for (v in intersect(built$Variable, xpt_names)) {
        lab <- xpt_labels[[v]]
        spec_lab <- built$Label[match(v, built$Variable)]
        if (nzchar(lab) && !identical(norm_ws(lab), norm_ws(spec_lab))) {
          add_note(ds, v, "label_mismatch", paste0("define label [", spec_lab, "] xpt label [", lab, "]"))
        }
        ln <- xpt_lengths[[v]]
        if (nzchar(ln) && !identical(as.character(suppressWarnings(as.integer(ln))), built$Length[match(v, built$Variable)])) {
          add_note(ds, v, "length_mismatch", paste0("define length ", built$Length[match(v, built$Variable)], " xpt attribute ", ln))
        }
      }
    }
    ds_rows[[length(ds_rows) + 1L]] <- data.frame(
      Dataset = ds, Label = norm_ws(at(g, "Label")), Structure = norm_ws(at(g, "Structure")),
      Class = at(g, "Class"), Keys = paste(present, collapse = ", "),
      XptFile = href, RowCount = n_row, stringsAsFactors = FALSE
    )
    attr(ds_rows[[length(ds_rows)]], "pairs") <- xpt_pairs
    attr(ds_rows[[length(ds_rows)]], "class_name") <- at(g, "Class")
  }

  variable <- do.call(rbind, var_rows)
  rownames(variable) <- NULL
  pair_env <- new.env(parent = emptyenv())
  for (i in seq_along(ds_rows)) pair_env[[ds_rows[[i]]$Dataset]] <- attr(ds_rows[[i]], "pairs")
  datasets <- do.call(rbind, lapply(ds_rows, function(d) {
    d$Dataset <- d$Dataset
    data.frame(Dataset = d$Dataset, Label = d$Label, Structure = d$Structure, Class = d$Class,
               Keys = d$Keys, XptFile = d$XptFile, RowCount = d$RowCount, stringsAsFactors = FALSE)
  }))
  rownames(datasets) <- NULL

  # Parameters from PARAMCD_* / PARAM_* codelists. adamref:Parameter is analysis-result metadata.
  adam_par <- xml2::xml_find_all(doc, ".//*[local-name()='Parameter']")
  adam_cd <- vapply(adam_par, function(n) at(n, "ParamCD"), "")
  add_note("", "", "parameter_source", paste(
    "Parameter rows come from CodeList PARAMCD_* paired in order with PARAM_*. adamref:Parameter has ",
    length(adam_par), " analysis-result entries and is not the BDS parameter catalog.",
    sep = ""
  ))
  par_rows <- list()
  for (i in seq_along(ds_rows)) {
    ds <- ds_rows[[i]]$Dataset
    cls <- at(groups[[i]], "Class")
    if (!identical(cls, "BDS")) next
    cd <- cl_by_oid[[paste0("PARAMCD_", ds)]]
    pm <- cl_by_oid[[paste0("PARAM_", ds)]]
    if (is.null(cd) || !nrow(cd)) {
      add_note(ds, "", "no_param_codelist", "BDS dataset has no PARAMCD codelist.")
      next
    }
    if (is.null(pm)) pm <- data.frame(Code = character(), Decode = character())
    n <- nrow(cd)
    if (nrow(pm) != n) {
      add_note(ds, "", "param_codelist_length",
               paste("PARAMCD codelist has", n, "items and PARAM codelist has", nrow(pm), "."))
    }
    for (k in seq_len(n)) {
      code <- cd$Code[[k]]
      param <- if (k <= nrow(pm)) {
        dec <- pm$Decode[[k]]
        if (nzchar(dec)) dec else pm$Code[[k]]
      } else ""
      hits <- value_level[value_level$Dataset == ds & value_level$PARAMCD == code, , drop = FALSE]
      src <- ""
      der <- ""
      if (nrow(hits)) {
        aval <- hits[hits$Variable == "AVAL", , drop = FALSE]
        src <- if (nrow(aval)) ref_token(aval$Comment[[1]]) else ""
        if (!nzchar(src)) {
          u <- unique(norm_ws(hits$Comment))
          u <- u[nzchar(u)]
          if (length(u) == 1L) src <- ref_token(u)
        }
        der <- vl_text(paste0(hits$Variable, "/", hits$PARAMCD), hits$Comment)
        # vl_text labels by PARAMCD; prefer parent-variable labels.
        der <- paste(unique(paste0(hits$Variable, ": ", norm_ws(hits$Comment))[nzchar(norm_ws(hits$Comment))]),
                     collapse = " | ")
      }
      if (!nzchar(der)) {
        der <- norm_ws(cd$Decode[[k]])
        if (!nzchar(der)) der <- param
      }
      par_rows[[length(par_rows) + 1L]] <- data.frame(
        Dataset = ds, PARAMCD = code, PARAM = param, Source = src, Derivation = der,
        stringsAsFactors = FALSE
      )
    }
    pairs <- pair_env[[ds]]
    sheet_cd <- cd$Code
    if (!is.null(pairs) && nrow(pairs)) {
      for (code in setdiff(unique(pairs$PARAMCD), sheet_cd)) {
        add_note(ds, code, "param_not_in_spec", "PARAMCD is in the xpt but not in the PARAMCD codelist.")
      }
      for (code in setdiff(sheet_cd, unique(pairs$PARAMCD))) {
        add_note(ds, code, "param_not_in_xpt", "PARAMCD is in the codelist but not in the xpt.")
      }
      for (code in intersect(sheet_cd, unique(pairs$PARAMCD))) {
        got <- unique(pairs$PARAM[pairs$PARAMCD == code])
        want <- param_of(par_rows, ds, code)
        if (length(got) == 1L && nzchar(want) && !identical(norm_ws(got), norm_ws(want))) {
          add_note(ds, code, "param_text_mismatch", paste0("codelist PARAM [", want, "] xpt PARAM [", got, "]"))
        } else if (length(got) > 1L) {
          add_note(ds, code, "param_text_mismatch",
                   paste("xpt has more than one PARAM for this PARAMCD:", paste(got, collapse = " | ")))
        }
      }
    }
  }
  parameter <- if (length(par_rows)) do.call(rbind, par_rows) else blank_df(
    c("Dataset", "PARAMCD", "PARAM", "Source", "Derivation"))
  rownames(parameter) <- NULL

  # adamref PARAMCDs that never appear in a dataset codelist.
  known <- unique(parameter$PARAMCD)
  extra_cd <- unique(adam_cd[nzchar(adam_cd) & !adam_cd %in% known])
  if (length(extra_cd)) {
    add_note("", paste(extra_cd, collapse = ","), "adamref_only",
             "These adamref:Parameter ParamCD values are not in any dataset PARAMCD codelist.")
  }

  note_df <- if (length(notes)) do.call(rbind, notes) else blank_df(c("Dataset", "Variable", "Topic", "Message"))
  rownames(note_df) <- NULL

  variable <- as_chr_df(variable[, c("Dataset", "Variable", "Label", "Type", "Length", "Key", "Source", "Derivation")])
  parameter <- as_chr_df(parameter[, c("Dataset", "PARAMCD", "PARAM", "Source", "Derivation")])
  datasets <- as_chr_df(datasets)
  value_level <- as_chr_df(value_level)
  codelist <- as_chr_df(codelist)
  methods <- as_chr_df(methods)
  note_df <- as_chr_df(note_df)

  xlsx <- file.path(outdir, workbook)
  write_spec_workbook(xlsx, variable, parameter, datasets, value_level, codelist, methods, note_df)
  write_utf8_csv(variable, file.path(outdir, "variable.csv"))
  write_utf8_csv(parameter, file.path(outdir, "parameter.csv"))

  splits <- lapply(datasets$Dataset, function(ds) {
    dest <- file.path(outdir, tolower(ds))
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    v <- slice_df(variable, variable$Dataset == ds)
    p <- slice_df(parameter, parameter$Dataset == ds)
    drow <- slice_df(datasets, datasets$Dataset == ds)
    vl <- slice_df(value_level, value_level$Dataset == ds)
    oids <- cl_env[[ds]]
    if (is.null(oids)) oids <- character()
    cl <- slice_df(codelist, codelist$OID %in% oids)
    m <- slice_df(methods, methods$OID %in% method_use$OID[method_use$Dataset == ds])
    n <- slice_df(note_df, note_df$Dataset == ds | !nzchar(note_df$Dataset))
    write_spec_workbook(
      file.path(dest, paste0(tolower(ds), "_spec.xlsx")),
      v, p, drow, vl, cl, m, n
    )
    write_utf8_csv(v, file.path(dest, "variable.csv"))
    write_utf8_csv(p, file.path(dest, "parameter.csv"))
    data.frame(Dataset = ds, Dir = dest, Variables = nrow(v), Parameters = nrow(p),
               stringsAsFactors = FALSE)
  })
  splits <- do.call(rbind, splits)
  rownames(splits) <- NULL

  cat("Wrote ", xlsx, "\n", sep = "")
  cat("Variables ", nrow(variable), " parameters ", nrow(parameter),
      " value-level ", nrow(value_level), " notes ", nrow(note_df), "\n", sep = "")
  for (i in seq_len(nrow(splits))) {
    cat(splits$Dataset[[i]], " ", splits$Variables[[i]], " variables, ",
        splits$Parameters[[i]], " parameters -> ", splits$Dir[[i]], "\n", sep = "")
  }
  invisible(list(
    variable = variable, parameter = parameter, datasets = datasets,
    value_level = value_level, codelist = codelist, methods = methods, notes = note_df,
    n_itemdef = n_itemdef, n_group_ref = n_group_ref, n_vl_ref = n_vl_ref,
    workbook = xlsx, splits = splits
  ))
}

# param text just appended; look it up from the rows collected so far.
param_of <- function(rows, ds, code) {
  if (!length(rows)) return("")
  hit <- Filter(function(r) identical(r$Dataset, ds) && identical(r$PARAMCD, code), rows)
  if (!length(hit)) "" else hit[[length(hit)]]$PARAM
}

launched <- function() {
  f <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(f) == 1L && grepl("define_to_spec\\.R$", sub("^--file=", "", f))
}

if (launched()) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 2L) {
    cat("Usage: Rscript tools/define_to_spec.R <define.xml> <outdir>\n")
    quit(status = 1L)
  }
  define_to_spec(args[[1]], args[[2]])
}
