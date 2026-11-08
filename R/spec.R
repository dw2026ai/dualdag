# Read and validate variable.csv + parameter.csv (F2).
# row = physical CSV line of the record (header is line 1 when the file starts with the header).

.VAR_COLS <- c("Dataset", "Variable", "Label", "Type", "Length", "Key", "Source", "Derivation")
.PAR_COLS <- c("Dataset", "PARAMCD", "PARAM", "Source", "Derivation")

empty_variables <- function() {
  data.frame(row = integer(), Dataset = character(), Variable = character(), Label = character(),
             Type = character(), Length = integer(), Key = integer(), Source = character(),
             Derivation = character(), stringsAsFactors = FALSE)
}

empty_parameters <- function() {
  data.frame(row = integer(), Dataset = character(), PARAMCD = character(), PARAM = character(),
             Source = character(), Derivation = character(), stringsAsFactors = FALSE)
}

# Group physical lines into CSV records so a quoted newline keeps the starting line number.
csv_records <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  if (length(lines)) lines <- sub("\r$", "", lines)
  if (length(lines) && nzchar(lines[1])) lines[1] <- sub("^\ufeff", "", lines[1])
  if (!length(lines) || !any(nzchar(trimws(lines)))) stop("Empty CSV: ", path, call. = FALSE)
  recs <- list()
  buf <- character()
  start <- 1L
  qcount <- 0L
  for (i in seq_along(lines)) {
    if (!length(buf)) start <- i
    buf <- c(buf, lines[i])
    stripped <- gsub("\"\"", "", lines[i])
    qcount <- qcount + nchar(gsub("[^\"]", "", stripped))
    if (qcount %% 2L == 0L) {
      recs[[length(recs) + 1L]] <- list(line = start, text = paste(buf, collapse = "\n"))
      buf <- character()
      qcount <- 0L
    }
  }
  if (length(buf)) stop("Unbalanced quotes in ", basename(path), ".", call. = FALSE)
  Filter(function(r) nzchar(trimws(r$text)), recs)
}

read_csv_table <- function(path) {
  recs <- csv_records(path)
  txt <- paste(vapply(recs, `[[`, "", "text"), collapse = "\n")
  # row.names = NULL: a trailing comma must not steal the first column as a row name.
  df <- utils::read.csv(text = txt, check.names = FALSE, stringsAsFactors = FALSE,
                        colClasses = "character", na.strings = character(),
                        comment.char = "", quote = "\"", fill = TRUE, row.names = NULL)
  names(df) <- trimws(names(df))
  list(df = df, lines = if (length(recs) > 1) vapply(recs[-1], `[[`, 0L, "line") else integer())
}

col_pick <- function(df, name) {
  hit <- which(tolower(names(df)) == tolower(name))
  if (!length(hit)) NULL else df[[hit[1]]]
}

up <- function(x) {
  if (is.null(x)) return("")
  toupper(trimws(x))
}

parse_key_cell <- function(x) {
  x <- trimws(x %||% "")
  if (!nzchar(x)) return(list(key = NA_integer_, bad = FALSE))
  if (grepl("^[1-9][0-9]*$", x)) return(list(key = as.integer(x), bad = FALSE))
  list(key = NA_integer_, bad = TRUE)
}

row_text <- function(df, i) paste(vapply(df[i, , drop = TRUE], as.character, ""), collapse = ",")

read_spec <- function(variable_path, parameter_path = NULL) {
  problems <- list()
  vt <- tryCatch(read_csv_table(variable_path), error = function(e) e)
  if (inherits(vt, "error")) stop(conditionMessage(vt), call. = FALSE)
  vdf <- vt$df
  missing_v <- .VAR_COLS[vapply(.VAR_COLS, function(nm) is.null(col_pick(vdf, nm)), TRUE)]
  for (nm in missing_v) {
    problems[[length(problems) + 1L]] <- make_issue(
      "structure", paste0("Missing column: ", nm), file = "variable.csv", row = 1L,
      suggestion = "Add the column to the header row of variable.csv.")
  }
  variables <- empty_variables()
  if (!("Dataset" %in% missing_v) && !("Variable" %in% missing_v) && nrow(vdf)) {
    n <- nrow(vdf)
    keys <- rep(NA_integer_, n)
    bad_key <- rep(FALSE, n)
    key_raw <- col_pick(vdf, "Key")
    if (!is.null(key_raw)) {
      parsed <- lapply(key_raw, parse_key_cell)
      keys <- vapply(parsed, function(z) z$key, 0L)
      bad_key <- vapply(parsed, function(z) z$bad, FALSE)
    }
    len_raw <- col_pick(vdf, "Length")
    src <- col_pick(vdf, "Source")
    der <- col_pick(vdf, "Derivation")
    variables <- data.frame(
      row = as.integer(vt$lines),
      Dataset = up(col_pick(vdf, "Dataset")),
      Variable = up(col_pick(vdf, "Variable")),
      Label = trimws(col_pick(vdf, "Label") %||% rep("", n)),
      Type = trimws(col_pick(vdf, "Type") %||% rep("", n)),
      Length = if (is.null(len_raw)) rep(NA_integer_, n) else suppressWarnings(as.integer(ifelse(nzchar(trimws(len_raw)), len_raw, NA))),
      Key = keys,
      Source = if (is.null(src)) rep("", n) else trimws(src),
      Derivation = if (is.null(der)) rep("", n) else trimws(der),
      stringsAsFactors = FALSE
    )
    keep <- nzchar(variables$Dataset) | nzchar(variables$Variable)
    # blank padding rows from a trailing newline are already dropped; drop fully empty data rows
    empty_row <- !nzchar(variables$Dataset) & !nzchar(variables$Variable) &
      !nzchar(variables$Label) & !nzchar(variables$Source) & !nzchar(variables$Derivation)
    keep <- !empty_row
    variables <- variables[keep, , drop = FALSE]
    bad_key <- bad_key[keep]
    rownames(variables) <- NULL
    for (i in seq_len(nrow(variables))) {
      if (!nzchar(variables$Dataset[i]) || !nzchar(variables$Variable[i])) {
        problems[[length(problems) + 1L]] <- make_issue(
          "structure", "Dataset and Variable are required.", dataset = variables$Dataset[i],
          file = "variable.csv", row = variables$row[i], text = row_text(vdf, match(variables$row[i], vt$lines)),
          suggestion = "Fill both Dataset and Variable.")
      }
      if (isTRUE(bad_key[i])) {
        problems[[length(problems) + 1L]] <- make_issue(
          "structure", "Key must be a positive integer (1, 2, 3, ...) or blank.",
          dataset = variables$Dataset[i], variables = variables$Variable[i],
          file = "variable.csv", row = variables$row[i],
          suggestion = "Use 1..n with no gaps, or leave Key blank.")
      }
    }
    if (nrow(variables)) {
      id <- paste(variables$Dataset, variables$Variable, sep = "\r")
      dup <- nzchar(variables$Dataset) & nzchar(variables$Variable) & (duplicated(id) | duplicated(id, fromLast = TRUE))
      seen <- character()
      for (i in which(dup)) {
        k <- id[i]
        if (k %in% seen) next
        seen <- c(seen, k)
        rows <- variables$row[id == k]
        problems[[length(problems) + 1L]] <- make_issue(
          "structure", paste0("Duplicate Dataset + Variable ", variables$Dataset[i], ".", variables$Variable[i],
                              " at rows ", paste(rows, collapse = " and "), "."),
          dataset = variables$Dataset[i], variables = variables$Variable[i],
          file = "variable.csv", row = rows[length(rows)],
          suggestion = "Keep one row per Dataset + Variable.")
      }
    }
  }

  parameters <- empty_parameters()
  if (!is.null(parameter_path) && nzchar(parameter_path) && file.exists(parameter_path)) {
    pt <- tryCatch(read_csv_table(parameter_path), error = function(e) e)
    if (inherits(pt, "error")) stop(conditionMessage(pt), call. = FALSE)
    pdf <- pt$df
    missing_p <- .PAR_COLS[vapply(.PAR_COLS, function(nm) is.null(col_pick(pdf, nm)), TRUE)]
    for (nm in missing_p) {
      problems[[length(problems) + 1L]] <- make_issue(
        "structure", paste0("Missing column: ", nm), file = "parameter.csv", row = 1L,
        suggestion = "Add the column to the header row of parameter.csv.")
    }
    if (!("Dataset" %in% missing_p) && !("PARAMCD" %in% missing_p) && nrow(pdf)) {
      n <- nrow(pdf)
      parameters <- data.frame(
        row = as.integer(pt$lines),
        Dataset = up(col_pick(pdf, "Dataset")),
        PARAMCD = up(col_pick(pdf, "PARAMCD")),
        PARAM = trimws(col_pick(pdf, "PARAM") %||% rep("", n)),
        Source = trimws(col_pick(pdf, "Source") %||% rep("", n)),
        Derivation = trimws(col_pick(pdf, "Derivation") %||% rep("", n)),
        stringsAsFactors = FALSE
      )
      empty_row <- !nzchar(parameters$Dataset) & !nzchar(parameters$PARAMCD) & !nzchar(parameters$PARAM)
      parameters <- parameters[!empty_row, , drop = FALSE]
      rownames(parameters) <- NULL
      if (nrow(parameters)) {
        id <- paste(parameters$Dataset, parameters$PARAMCD, sep = "\r")
        dup <- nzchar(parameters$Dataset) & nzchar(parameters$PARAMCD) & (duplicated(id) | duplicated(id, fromLast = TRUE))
        seen <- character()
        for (i in which(dup)) {
          k <- id[i]
          if (k %in% seen) next
          seen <- c(seen, k)
          rows <- parameters$row[id == k]
          problems[[length(problems) + 1L]] <- make_issue(
            "structure", paste0("Duplicate Dataset + PARAMCD ", parameters$Dataset[i], ".", parameters$PARAMCD[i],
                                " at rows ", paste(rows, collapse = " and "), "."),
            dataset = parameters$Dataset[i], variables = parameters$PARAMCD[i],
            file = "parameter.csv", row = rows[length(rows)],
            suggestion = "Keep one row per Dataset + PARAMCD.")
        }
      }
    }
  }

  if (!("Key" %in% missing_v) && nrow(variables)) {
    for (ds in unique(variables$Dataset)) {
      if (!nzchar(ds)) next
      sub <- variables[variables$Dataset == ds, , drop = FALSE]
      ks <- sub$Key[!is.na(sub$Key)]
      if (!length(ks)) {
        problems[[length(problems) + 1L]] <- make_issue(
          "no_key", paste0(ds, " has no Key."), dataset = ds, file = "variable.csv",
          row = sub$row[1], suggestion = "Mark at least one variable with Key 1, 2, 3, ... in order.")
      } else if (!identical(sort(as.integer(ks)), seq_along(ks))) {
        problems[[length(problems) + 1L]] <- make_issue(
          "structure", paste0(ds, " Key values must be exactly 1..n with no gaps or duplicates."),
          dataset = ds, variables = sub$Variable[!is.na(sub$Key)], file = "variable.csv",
          row = sub$row[which(!is.na(sub$Key))[1]],
          suggestion = "Number keys 1, 2, 3, ... once each, and leave non-keys blank.")
      }
    }
  }

  if (nrow(parameters)) {
    orphan <- setdiff(unique(parameters$Dataset), unique(variables$Dataset))
    orphan <- orphan[nzchar(orphan)]
    for (ds in orphan) {
      problems[[length(problems) + 1L]] <- make_issue(
        "no_key", paste0(ds, " has parameters but no variables, so it has no Key."),
        dataset = ds, file = "parameter.csv",
        suggestion = "Add the dataset's variables, including Key 1..n, to variable.csv.")
    }
  }

  datasets <- sort(unique(c(variables$Dataset, parameters$Dataset)))
  datasets <- datasets[nzchar(datasets)]
  hash <- spec_file_hash(variable_path, if (!is.null(parameter_path) && file.exists(parameter_path)) parameter_path else NULL)
  list(variables = variables, parameters = parameters, datasets = datasets, problems = problems, hash = hash)
}

spec_file_hash <- function(variable_path, parameter_path = NULL) {
  read_raw <- function(path) {
    sz <- file.info(path)$size
    if (is.na(sz) || sz <= 0) raw() else readBin(path, "raw", sz)
  }
  b <- read_raw(variable_path)
  if (!is.null(parameter_path) && file.exists(parameter_path)) b <- c(b, charToRaw("\n--\n"), read_raw(parameter_path))
  if (!length(b)) b <- raw(0)
  paste(as.character(openssl::md5(b)))
}

spec_public <- function(sp) {
  if (is.null(sp)) return(NULL)
  list(variables = sp$variables, parameters = sp$parameters, datasets = as.list(sp$datasets), hash = sp$hash)
}
