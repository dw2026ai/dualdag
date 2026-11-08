# Data viewer: list uploaded xpt, side results, generated frames, and exports.
# xpt/rds bytes are cached by path + mtime + size so paging does not read them again.

.viewer_state <- new.env(parent = emptyenv())
.viewer_state$files <- new.env(parent = emptyenv())
.viewer_state$file_meta <- new.env(parent = emptyenv())
.viewer_state$assembled <- new.env(parent = emptyenv())
.viewer_state$generated <- new.env(parent = emptyenv())
.viewer_state$dimensions <- new.env(parent = emptyenv())
.viewer_state$reads <- 0L

viewer_cache_reset <- function() {
  .viewer_state$files <- new.env(parent = emptyenv())
  .viewer_state$file_meta <- new.env(parent = emptyenv())
  .viewer_state$assembled <- new.env(parent = emptyenv())
  .viewer_state$generated <- new.env(parent = emptyenv())
  .viewer_state$dimensions <- new.env(parent = emptyenv())
  .viewer_state$reads <- 0L
  invisible(0L)
}

# Disk reads of an xpt or rds. A cache hit does not increment this.
viewer_file_reads <- function() .viewer_state$reads

viewer_error <- function(message, status = 400L) {
  structure(
    list(message = message, status = as.integer(status), call = NULL),
    class = c("viewer_error", "error", "condition")
  )
}

viewer_stop <- function(message, status = 400L) stop(viewer_error(message, status))

viewer_project <- function(project) {
  if (is.environment(project) && is.character(project$dir) && length(project$dir) == 1L) return(project)
  if (is.character(project) && length(project) == 1L && !is.na(project) && nzchar(project)) {
    return(open_project(project))
  }
  stop("A project is required.", call. = FALSE)
}

viewer_iso <- function(mtime) {
  if (is.null(mtime) || !length(mtime) || all(is.na(mtime))) return(NULL)
  # c() drops the POSIXt class, so a collected vector arrives as seconds.
  if (!inherits(mtime, "POSIXt")) mtime <- as.POSIXct(as.numeric(mtime), origin = "1970-01-01")
  format(max(mtime, na.rm = TRUE), "%Y-%m-%dT%H:%M:%S%z")
}

viewer_rel <- function(root, path) {
  root_n <- normalizePath(root, winslash = "/", mustWork = TRUE)
  path_n <- normalizePath(path, winslash = "/", mustWork = TRUE)
  prefix <- paste0(sub("/$", "", root_n), "/")
  if (startsWith(path_n, prefix)) substring(path_n, nchar(prefix) + 1L) else basename(path_n)
}

viewer_safe_name <- function(name, exts) {
  name <- basename(as.character(name %||% "")[1])
  if (!nzchar(name) || grepl("\\.\\.", name, fixed = TRUE)) return(NULL)
  pat <- paste0("^[A-Za-z0-9][A-Za-z0-9_.-]*\\.(", paste(exts, collapse = "|"), ")$")
  if (!grepl(pat, name, ignore.case = TRUE)) return(NULL)
  name
}

viewer_find_file <- function(dir, name) {
  if (!dir.exists(dir)) return(NULL)
  path <- file.path(dir, name)
  if (file.exists(path) && !dir.exists(path)) return(path)
  hits <- list.files(dir, full.names = FALSE)
  hits <- hits[tolower(hits) == tolower(name)]
  if (!length(hits)) return(NULL)
  file.path(dir, hits[1])
}

viewer_col_type <- function(x) {
  if (inherits(x, "POSIXt")) return("datetime")
  if (inherits(x, "Date")) return("date")
  if (is.character(x) || is.factor(x)) return("character")
  if (is.integer(x)) return("integer")
  if (is.numeric(x)) return("numeric")
  if (is.logical(x)) return("logical")
  typeof(x)
}

viewer_prepare <- function(df) {
  if (is.null(df)) return(NULL)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!ncol(df) && !nrow(df)) {
    rownames(df) <- NULL
    return(df)
  }
  # Subset copies the data.frame so later column edits cannot touch the cache.
  df <- df[seq_len(nrow(df)), , drop = FALSE]
  for (nm in names(df)) {
    col <- df[[nm]]
    lab <- attr(col, "label", exact = TRUE)
    if (inherits(col, "haven_labelled") || inherits(col, "haven_labelled_spss")) col <- haven::zap_labels(col)
    if (is.factor(col)) col <- as.character(col)
    if (is.character(lab) && length(lab) == 1L && !is.na(lab) && nzchar(lab)) attr(col, "label") <- as.character(lab)
    df[[nm]] <- col
  }
  rownames(df) <- NULL
  df
}

viewer_spec_labels <- function(spec, dataset) {
  out <- empty_named()
  if (is.null(spec) || is.null(spec$variables) || !nrow(spec$variables)) return(out)
  v <- spec$variables
  hit <- which(v$Dataset == dataset)
  for (i in hit) {
    lab <- v$Label[i]
    out[[as.character(v$Variable[i])]] <- if (is.na(lab)) "" else as.character(lab)
  }
  out
}

viewer_apply_spec_labels <- function(df, spec, dataset) {
  labs <- viewer_spec_labels(spec, dataset)
  if (!length(labs) || !ncol(df)) return(df)
  for (nm in intersect(names(df), names(labs))) {
    cur <- attr(df[[nm]], "label", exact = TRUE)
    if ((is.null(cur) || !nzchar(as.character(cur)[1] %||% "")) && nzchar(labs[[nm]] %||% "")) {
      attr(df[[nm]], "label") <- labs[[nm]]
    }
  }
  df
}

viewer_display <- function(col) {
  n <- length(col)
  if (inherits(col, "POSIXt")) return(format(col, "%Y-%m-%dT%H:%M:%S%z"))
  if (inherits(col, "Date")) return(format(col, "%Y-%m-%d"))
  if (is.numeric(col)) {
    txt <- rep(NA_character_, n)
    ok <- !is.na(col)
    if (any(ok)) txt[ok] <- sprintf("%.15g", col[ok])
    return(txt)
  }
  txt <- as.character(col)
  txt[is.na(col)] <- NA_character_
  txt
}

viewer_match <- function(df, filter) {
  n <- nrow(df)
  if (!nzchar(filter) || !n || !ncol(df)) return(rep(TRUE, n))
  needle <- tolower(filter)
  hit <- rep(FALSE, n)
  for (nm in names(df)) {
    txt <- viewer_display(df[[nm]])
    txt[is.na(txt)] <- ""
    part <- grepl(needle, tolower(txt), fixed = TRUE)
    part[is.na(part)] <- FALSE
    hit <- hit | part
  }
  hit
}

viewer_columns <- function(df) {
  lapply(names(df), function(nm) {
    col <- df[[nm]]
    lab <- attr(col, "label", exact = TRUE)
    if (is.null(lab) || !length(lab) || is.na(lab[[1]]) || !nzchar(as.character(lab[[1]]))) lab <- ""
    list(name = nm, label = as.character(lab)[1], type = viewer_col_type(col))
  })
}

viewer_rows <- function(df) {
  if (!nrow(df)) return(list())
  cells <- lapply(df, function(col) {
    txt <- viewer_display(col)
    out <- vector("list", length(txt))
    for (i in seq_along(txt)) if (!is.na(txt[[i]])) out[[i]] <- txt[[i]]
    out
  })
  lapply(seq_len(nrow(df)), function(i) unname(lapply(cells, function(col) col[[i]])))
}

viewer_read_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "rds")) return(readRDS(path))
  if (identical(ext, "xpt")) return(haven::read_xpt(path))
  stop("Unsupported data file.", call. = FALSE)
}

# Cache key is the normalized path plus mtime and size. Content is not hashed:
# hashing would read the file on every page.
viewer_cached_frame <- function(path) {
  info <- file.info(path)
  if (is.na(info$size) || is.na(info$mtime)) stop("File not found.", call. = FALSE)
  key <- as.character(path)[1]
  sig <- paste0(sprintf("%.6f", as.numeric(info$mtime)), ":", sprintf("%.0f", info$size))
  hit <- .viewer_state$files[[key]]
  if (!is.null(hit) && identical(hit$sig, sig)) return(hit$df)
  df <- viewer_read_file(path)
  .viewer_state$reads <- .viewer_state$reads + 1L
  .viewer_state$files[[key]] <- list(sig = sig, df = df)
  df
}

viewer_cached_metadata <- function(path, info = NULL) {
  if (is.null(info)) info <- file.info(path)
  if (is.na(info$size) || is.na(info$mtime)) return(NULL)
  key <- as.character(path)[1]
  sig <- paste0(sprintf("%.6f", as.numeric(info$mtime)), ":", sprintf("%.0f", info$size))
  hit <- .viewer_state$file_meta[[key]]
  if (!is.null(hit) && identical(hit$sig, sig)) return(hit$meta)
  df <- tryCatch(viewer_cached_frame(path), error = function(e) NULL)
  if (!is.data.frame(df)) return(NULL)
  meta <- list(nrow = as.integer(nrow(df)), columns = names(df), ncol = as.integer(ncol(df)),
               updated = viewer_iso(info$mtime))
  .viewer_state$file_meta[[key]] <- list(sig = sig, meta = meta)
  meta
}

viewer_entry <- function(id, kind, dataset, node, side, file, df, updated) {
  list(
    id = id, kind = kind, dataset = dataset, node = node, side = side, file = file,
    nrow = as.integer(nrow(df)), ncol = as.integer(ncol(df)), updated = updated
  )
}

viewer_entry_dims <- function(id, kind, dataset, node, side, file, nrow, ncol, updated) {
  list(id = id, kind = kind, dataset = dataset, node = node, side = side, file = file,
       nrow = as.integer(nrow), ncol = as.integer(ncol), updated = updated)
}

viewer_l3_ids <- function(p, dataset) {
  ids <- names(p$state$nodes[[dataset]])
  if (!length(ids)) return(character())
  keep <- vapply(ids, function(id) identical(get_status(p, dataset, id)$level, "L3"), logical(1))
  ids[keep]
}

viewer_side_ids <- function(p, dataset, side, graph = NULL, states = NULL) {
  p <- project_dir_of(p)
  graph <- graph %||% tryCatch(load_graph(p, dataset), error = function(e) NULL)
  ids <- vapply(graph$nodes %||% list(), function(n) as.character(n$id %||% ""), "")
  ids <- ids[nzchar(ids)]
  if (is.null(states)) states <- state_nodes(p, dataset)
  if (!length(ids)) ids <- names(states)
  ids[vapply(ids, function(id) node_side_result_available(p, dataset, id, side, TRUE, states), TRUE)]
}

viewer_generated_signature_info <- function(p, dataset, side, paramcd = NULL, graph = NULL, spec = NULL, states = NULL) {
  p <- project_dir_of(p)
  graph <- graph %||% tryCatch(load_graph(p, dataset), error = function(e) NULL)
  spec <- spec %||% tryCatch(read_project_spec(p), error = function(e) NULL)
  if (is.null(states)) states <- state_nodes(p, dataset)
  ids <- if (!is.null(paramcd) && isTRUE(graph$layered))
    unique(c(layer_node_ids(graph, paramcd), vapply(Filter(function(n) identical(n$kind, "dataset_variable"), graph$nodes %||% list()), `[[`, "", "id"))) else
    vapply(graph$nodes %||% list(), function(n) as.character(n$id %||% ""), "")
  ids <- ids[nzchar(ids)]
  paths <- file.path(vapply(ids, function(id) side_run_dir(p, dataset, id, side), ""), "out.rds")
  fi <- if (length(paths)) file.info(paths) else data.frame(size=numeric(),mtime=as.POSIXct(character()))
  good <- vapply(ids, function(id) {
    st <- states[[id]] %||% list()
    isTRUE(st$sides[[side]]$ok) || identical(as.character(st$level %||% ""), "L3")
  }, TRUE)
  mtime <- as.numeric(fi$mtime)
  size <- fi$size
  exists <- !is.na(size) & !is.na(mtime)
  spec_keys <- if (is.null(spec)) character() else if (!is.null(paramcd))
    layer_key_names(spec,dataset,paramcd,graph,p) else spec_key_names(spec,dataset)
  header <- paste(p, dataset, side, toupper(paramcd %||% ""), paste(ids, collapse=","), paste(spec_keys, collapse=","), sep="|")
  rows <- if (length(ids)) paste(ids, good, ifelse(exists, sprintf("%.6f:%.0f", mtime, size), "missing"), sep="|") else character()
  updated <- if (any(exists)) viewer_iso(max(mtime[exists])) else NULL
  list(signature=paste(c(header,rows),collapse="\n"), updated=updated, ids=ids, paths=paths, file_info=fi,
       exists=exists, size=size, mtime=mtime, good=good, states=states)
}

viewer_generated_frame <- function(p, dataset, side = "primary", paramcd = NULL, states = NULL) {
  p <- viewer_project(p)
  p <- project_dir_of(p)
  dataset <- toupper(dataset)
  side <- match.arg(side, c("primary", "validation"))
  graph <- tryCatch(load_graph(p, dataset), error = function(e) NULL)
  spec <- tryCatch(read_project_spec(p), error = function(e) NULL)
  if (is.null(spec)) return(NULL)
  if (is.null(graph)) graph <- tryCatch(graph_skeleton(spec, dataset), error = function(e) NULL)
  if (is.null(graph)) return(NULL)
  states <- states %||% state_nodes(p, dataset)
  if (!length(viewer_side_ids(p, dataset, side, graph, states))) return(NULL)
  if (!is.null(paramcd) && isTRUE(graph$layered)) {
    records_id <- paste0(dataset, ".RECORDS@", toupper(paramcd))
    # A parameter frame is keyed by its own successful Keys/Records result.
    if (!node_side_result_available(p, dataset, records_id, side, TRUE, states)) return(NULL)
  }
  key <- paste(p, dataset, toupper(paramcd %||% ""), side, sep = "|")
  info <- viewer_generated_signature_info(p, dataset, side, paramcd, graph, spec, states)
  sig <- info$signature
  hit <- .viewer_state$generated[[key]]
  if (!is.null(hit) && identical(hit$sig, sig)) return(hit$df)
  if (!is.null(paramcd) && isTRUE(graph$layered)) {
    records_id <- paste0(dataset, ".RECORDS@", toupper(paramcd))
    node <- graph_node(graph, records_id) %||% default_node(records_id)
    df <- layered_work_frame(p, dataset, side, node, graph, spec, include_generated = TRUE, states=states)
  } else {
    df <- assemble_work(p, dataset, side, spec, graph, include_generated = TRUE, states=states)
  }
  if (!is.data.frame(df) || !nrow(df) || !ncol(df)) return(NULL)
  .viewer_state$generated[[key]] <- list(sig = sig, df = df, updated=info$updated)
  df
}

viewer_generated_updated <- function(p, dataset, side, paramcd = NULL) {
  p <- project_dir_of(p)
  key <- paste(p, toupper(dataset), toupper(paramcd %||% ""), side, sep="|")
  cached <- .viewer_state$generated[[key]]
  if (!is.null(cached) && !is.null(cached$updated)) return(cached$updated)
  graph <- tryCatch(load_graph(p,dataset),error=function(e)NULL)
  ids <- if (!is.null(paramcd)) unique(c(layer_node_ids(graph,paramcd), vapply(Filter(function(n) identical(n$kind, "dataset_variable"), graph$nodes %||% list()), `[[`, "", "id"))) else
    viewer_side_ids(p,dataset,side)
  secs <- numeric()
  for (id in ids) {
    path <- file.path(side_run_dir(p,dataset,id,side),"out.rds")
    if (file.exists(path)) secs <- c(secs,as.numeric(file.info(path)$mtime))
  }
  viewer_iso(secs)
}

viewer_generated_dimensions <- function(p, dataset, side, paramcd, graph, spec, states) {
  p <- project_dir_of(p)
  key <- paste(p, toupper(dataset), toupper(paramcd %||% ""), side, sep="|")
  info <- viewer_generated_signature_info(p, dataset, side, paramcd, graph, spec, states)
  hit <- .viewer_state$dimensions[[key]]
  if (!is.null(hit) && identical(hit$sig, info$signature)) return(hit$meta)
  if (isTRUE(graph$layered) && !is.null(paramcd)) {
    records_id <- paste0(toupper(dataset), ".RECORDS@", toupper(paramcd))
    records_at <- match(records_id, info$ids)
    if (is.na(records_at) || !isTRUE(info$good[[records_at]]) || !isTRUE(info$exists[[records_at]])) return(NULL)
    records <- viewer_cached_metadata(info$paths[[records_at]], info$file_info[records_at,,drop=FALSE])
    if (is.null(records) || !records$nrow) return(NULL)
    columns <- records$columns
    keys <- layer_key_names(spec, dataset, paramcd, graph, p)
    for (id in setdiff(layer_node_ids(graph, paramcd), records_id)) {
      at <- match(id, info$ids)
      if (is.na(at) || !isTRUE(info$good[[at]]) || !isTRUE(info$exists[[at]])) next
      var <- parse_node_id(id)$var %||% ""
      if (!nzchar(var)) next
      piece <- viewer_cached_metadata(info$paths[[at]], info$file_info[at,,drop=FALSE])
      if (is.null(piece) || !var %in% piece$columns) next
      if (length(intersect(keys, intersect(columns, piece$columns)))) columns <- c(columns, setdiff(var, columns))
    }
    dsids <- vapply(Filter(function(n) identical(n$kind, "dataset_variable"), graph$nodes %||% list()), `[[`, "", "id")
    for (id in dsids) {
      at <- match(id, info$ids)
      if (is.na(at) || !isTRUE(info$good[[at]]) || !isTRUE(info$exists[[at]])) next
      var <- parse_node_id(id)$var %||% ""
      if (!nzchar(var) || !"USUBJID" %in% columns) next
      piece <- viewer_cached_metadata(info$paths[[at]], info$file_info[at,,drop=FALSE])
      if (!is.null(piece) && "USUBJID" %in% piece$columns && var %in% piece$columns)
        columns <- c(columns, setdiff(var, columns))
    }
    for (k in keys) if (!k %in% columns) columns <- c(columns, k)
    columns <- c(keys, setdiff(columns, keys))
    meta <- list(nrow=records$nrow,ncol=length(columns))
  } else {
    df <- viewer_generated_frame(p, dataset, side, paramcd, states)
    if (is.null(df)) return(NULL)
    meta <- list(nrow=nrow(df),ncol=ncol(df))
  }
  meta$updated <- info$updated
  .viewer_state$dimensions[[key]] <- list(sig=info$signature,meta=meta)
  meta
}

viewer_spec_order <- function(spec, dataset) {
  if (is.null(spec) || is.null(spec$variables) || !nrow(spec$variables)) return(character())
  v <- spec$variables
  v <- v[v$Dataset == dataset & !is.na(v$Variable) & nzchar(v$Variable), , drop = FALSE]
  unique(as.character(v$Variable))
}

viewer_primary_path <- function(p, dataset, id) {
  file.path(node_run_dir(p, dataset, id), "primary", "out.rds")
}

assembled_signature <- function(p, dataset) {
  ids <- sort(viewer_l3_ids(p, dataset))
  if (!length(ids)) return(NULL)
  spec <- tryCatch(read_project_spec(p), error = function(e) NULL)
  keys <- if (is.null(spec)) "" else paste(dataset_keys(spec, dataset), collapse = ",")
  order_vars <- paste(viewer_spec_order(spec, dataset), collapse = ",")
  bits <- c(paste0("keys:", keys), paste0("order:", order_vars))
  for (id in ids) {
    path <- viewer_primary_path(p, dataset, id)
    if (!file.exists(path)) {
      bits <- c(bits, paste0(id, ":missing"))
      next
    }
    info <- file.info(path)
    bits <- c(bits, paste(id, sprintf("%.6f", as.numeric(info$mtime)), sprintf("%.0f", info$size), sep = "|"))
  }
  paste(bits, collapse = "\n")
}

assemble_dataset <- function(p, dataset) {
  dataset <- toupper(dataset)
  ids <- viewer_l3_ids(p, dataset)
  if (!length(ids)) return(NULL)
  spec <- tryCatch(read_project_spec(p), error = function(e) NULL)
  keys <- if (is.null(spec)) character() else dataset_keys(spec, dataset)
  order_vars <- viewer_spec_order(spec, dataset)
  vars <- vapply(ids, function(id) {
    pt <- split_node_id(id)
    if (is.null(pt)) "" else pt$var
  }, "")
  ok <- nzchar(vars)
  ids <- ids[ok]
  vars <- vars[ok]
  if (!length(ids)) return(NULL)
  uniq <- unique(vars)
  var_order <- c(intersect(order_vars, uniq), sort(setdiff(uniq, order_vars)))
  pieces <- list()
  for (v in var_order) {
    frames <- list()
    for (id in sort(ids[vars == v])) {
      path <- viewer_primary_path(p, dataset, id)
      if (!file.exists(path)) next
      df <- viewer_prepare(viewer_cached_frame(path))
      if (is.null(df) || !v %in% names(df)) next
      keep <- unique(c(intersect(keys, names(df)), v))
      frames[[length(frames) + 1L]] <- df[, keep, drop = FALSE]
    }
    if (!length(frames)) next
    piece <- frames[[1]]
    if (length(frames) > 1L) {
      piece <- do.call(rbind, frames)
      rownames(piece) <- NULL
    }
    pieces[[v]] <- piece
  }
  if (!length(pieces)) return(NULL)
  out <- pieces[[1]]
  if (length(pieces) > 1L) {
    for (i in seq.int(2L, length(pieces))) {
      by <- intersect(keys, intersect(names(out), names(pieces[[i]])))
      nxt <- pieces[[i]]
      if (length(by)) {
        out <- merge(out, nxt, by = by, all = TRUE, sort = FALSE)
      } else if (identical(nrow(out), nrow(nxt))) {
        extra <- setdiff(names(nxt), names(out))
        if (length(extra)) out <- cbind(out, nxt[, extra, drop = FALSE])
      } else {
        stop("Cannot assemble ", dataset, ": results do not share key columns.", call. = FALSE)
      }
    }
  }
  if (length(order_vars)) {
    want <- intersect(order_vars, names(out))
    if (length(want)) out <- out[, want, drop = FALSE]
  }
  if (length(keys)) {
    use <- intersect(keys, names(out))
    if (length(use)) out <- out[do.call(order, out[use]), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

load_assembled <- function(p, dataset) {
  dataset <- toupper(dataset)
  sig <- assembled_signature(p, dataset)
  if (is.null(sig)) return(NULL)
  hit <- .viewer_state$assembled[[dataset]]
  if (!is.null(hit) && identical(hit$sig, sig)) return(hit$df)
  df <- assemble_dataset(p, dataset)
  if (is.null(df)) return(NULL)
  .viewer_state$assembled[[dataset]] <- list(sig = sig, df = df)
  df
}

assembled_updated <- function(p, dataset) {
  ids <- viewer_l3_ids(p, dataset)
  secs <- numeric()
  for (id in ids) {
    path <- viewer_primary_path(p, dataset, id)
    if (!file.exists(path)) next
    secs <- c(secs, as.numeric(file.info(path)$mtime))
  }
  viewer_iso(secs)
}

list_viewer_datasets <- function(project, side = "primary") {
  p <- viewer_project(project)
  side <- match.arg(as.character(side %||% "primary")[1], c("primary", "validation"))
  root <- normalizePath(project_dir_of(p), winslash="/", mustWork=TRUE)
  out <- list()

  data_dir <- file.path(root, "data")
  data_names <- sort(list.files(data_dir))
  data_names <- data_names[grepl("\\.xpt$", data_names, ignore.case = TRUE)]
  for (nm in data_names) {
    path <- file.path(data_dir, nm)
    meta <- viewer_cached_metadata(path)
    if (is.null(meta)) next
    domain <- toupper(tools::file_path_sans_ext(nm))
    out[[length(out) + 1L]] <- viewer_entry_dims(
      paste0("in:", nm), "input", domain, NULL, NULL, nm, meta$nrow, meta$ncol, meta$updated
    )
  }

  runs <- file.path(root, ".dualdag", "runs")
  datasets <- character()
  if (dir.exists(runs)) {
    datasets <- list.files(runs, full.names = FALSE)
    datasets <- datasets[dir.exists(file.path(runs, datasets))]
  }
  results <- list()
  for (ds in datasets) {
    base <- file.path(runs, ds)
    folders <- list.files(base, full.names = FALSE)
    folders <- folders[dir.exists(file.path(base, folders))]
    for (folder in folders) {
      node <- paste0(ds, ".", folder)
      if (is.null(split_node_id(node))) next
      for (result_side in c("primary", "validation")) {
        path <- file.path(base, folder, result_side, "out.rds")
        if (!file.exists(path)) next
        meta <- viewer_cached_metadata(path)
        if (is.null(meta)) next
        results[[length(results) + 1L]] <- viewer_entry_dims(
          paste0("res:", ds, ":", node, ":", result_side), "result", ds, node, result_side,
          viewer_rel(root, path), meta$nrow, meta$ncol, meta$updated
        )
      }
    }
  }
  if (length(results)) {
    ord <- order(
      vapply(results, function(e) e$dataset, ""),
      vapply(results, function(e) e$node %||% "", ""),
      vapply(results, function(e) e$side %||% "", "")
    )
    out <- c(out, results[ord])
  }

  spec <- tryCatch(read_project_spec(root), error = function(e) NULL)
  generated_ds <- sort(unique(c(if (!is.null(spec)) spec$datasets else character(),
                                names(p$state$nodes) %||% character(), datasets)))
  for (ds in generated_ds[nzchar(generated_ds)]) {
    graph <- tryCatch(load_graph(root, ds), error = function(e) NULL)
    if (is.null(graph) && !is.null(spec)) graph <- tryCatch(graph_skeleton(spec, ds), error = function(e) NULL)
    if (is.null(graph)) next
    states <- state_nodes(root, ds)
    if (isTRUE(graph$layered)) {
      pg <- tryCatch(param_graph_read(root, ds), error = function(e) NULL)
      order <- if (is.null(pg)) character() else param_layer_order(spec, ds, pg)$order
      for (pc in order) {
        meta <- tryCatch(viewer_generated_dimensions(root, ds, side, pc, graph, spec, states), error = function(e) NULL)
        if (is.null(meta)) next
        entry <- viewer_entry_dims(paste0("layer:", ds, ":", pc, ":", side), "parameter", ds, NULL, side, NULL,
                                   meta$nrow, meta$ncol, meta$updated)
        entry$paramcd <- pc
        out[[length(out) + 1L]] <- entry
      }
    } else {
      df <- tryCatch(viewer_generated_frame(p, ds, side, states=states), error = function(e) NULL)
      if (is.null(df)) next
      out[[length(out) + 1L]] <- viewer_entry_dims(paste0("frame:", ds, ":", side), "generated", ds, NULL, side,
                                                    NULL, nrow(df), ncol(df), viewer_generated_updated(p, ds, side))
    }
  }

  out_dir <- file.path(root, "output")
  out_names <- sort(list.files(out_dir))
  out_names <- out_names[grepl("\\.(xpt|rds)$", out_names, ignore.case = TRUE)]
  for (nm in out_names) {
    path <- file.path(out_dir, nm)
    meta <- viewer_cached_metadata(path)
    if (is.null(meta)) next
    ds <- toupper(tools::file_path_sans_ext(nm))
    out[[length(out) + 1L]] <- viewer_entry_dims(
      paste0("out:", nm), "output", ds, NULL, NULL, nm, meta$nrow, meta$ncol, meta$updated
    )
  }
  unname(out)
}

parse_viewer_id <- function(id) {
  id <- as.character(id %||% "")[1]
  if (!nzchar(id)) return(NULL)
  parts <- strsplit(id, ":", fixed = TRUE)[[1]]
  kind <- parts[1]
  if (identical(kind, "in") && length(parts) == 2L) return(list(kind = "input", file = parts[2]))
  if (identical(kind, "out") && length(parts) == 2L) return(list(kind = "output", file = parts[2]))
  if (identical(kind, "frame") && length(parts) == 3L) return(list(kind = "generated", dataset = toupper(parts[2]), side = parts[3]))
  if (identical(kind, "layer") && length(parts) == 4L) return(list(kind = "parameter", dataset = toupper(parts[2]), paramcd = toupper(parts[3]), side = parts[4]))
  if (identical(kind, "res") && length(parts) == 4L) {
    return(list(kind = "result", dataset = toupper(parts[2]), node = parts[3], side = parts[4]))
  }
  NULL
}

resolve_viewer_id <- function(p, id) {
  parsed <- parse_viewer_id(id)
  if (is.null(parsed)) return(NULL)
  root <- project_dir_of(p)
  if (identical(parsed$kind, "input")) {
    fn <- viewer_safe_name(parsed$file, "xpt")
    if (is.null(fn)) return(NULL)
    path <- viewer_find_file(file.path(root, "data"), fn)
    if (is.null(path)) return(NULL)
    fn <- basename(path)
    return(list(
      id = paste0("in:", fn), kind = "input", dataset = toupper(tools::file_path_sans_ext(fn)),
      path = path, node = NULL, side = NULL
    ))
  }
  if (identical(parsed$kind, "output")) {
    fn <- viewer_safe_name(parsed$file, c("xpt", "rds"))
    if (is.null(fn)) return(NULL)
    path <- viewer_find_file(file.path(root, "output"), fn)
    if (is.null(path)) return(NULL)
    fn <- basename(path)
    return(list(
      id = paste0("out:", fn), kind = "output", dataset = toupper(tools::file_path_sans_ext(fn)),
      path = path, node = NULL, side = NULL
    ))
  }
  if (parsed$kind %in% c("generated", "parameter")) {
    ds <- parsed$dataset
    if (!grepl("^[A-Z][A-Z0-9_]*$", ds) || !parsed$side %in% c("primary", "validation")) return(NULL)
    graph <- tryCatch(load_graph(root, ds), error = function(e) NULL)
    if (is.null(graph)) {
      spec <- tryCatch(read_project_spec(root), error = function(e) NULL)
      if (!is.null(spec)) graph <- tryCatch(graph_skeleton(spec, ds), error = function(e) NULL)
    }
    if (is.null(graph) || identical(parsed$kind, "parameter") && !isTRUE(graph$layered)) return(NULL)
    id <- if (identical(parsed$kind, "parameter")) paste0("layer:", ds, ":", parsed$paramcd, ":", parsed$side) else paste0("frame:", ds, ":", parsed$side)
    return(list(id = id, kind = parsed$kind, dataset = ds, paramcd = parsed$paramcd %||% NULL,
                side = parsed$side, path = NULL, node = NULL))
  }
  ds <- parsed$dataset
  if (!grepl("^[A-Z][A-Z0-9_]*$", ds)) return(NULL)
  if (!parsed$side %in% c("primary", "validation")) return(NULL)
  node <- tryCatch(canonical_node_id(ds, parsed$node), error = function(e) NULL)
  if (is.null(node)) return(NULL)
  path <- file.path(node_run_dir(p, ds, node), parsed$side, "out.rds")
  if (!file.exists(path)) return(NULL)
  list(
    id = paste0("res:", ds, ":", node, ":", parsed$side), kind = "result", dataset = ds,
    path = path, node = node, side = parsed$side
  )
}

viewer_page_args <- function(offset, limit) {
  offset <- suppressWarnings(as.integer(offset %||% 0L)[1])
  limit <- suppressWarnings(as.integer(if (is.null(limit)) 100L else limit)[1])
  if (!length(offset) || is.na(offset) || offset < 0L) offset <- 0L
  if (!length(limit) || is.na(limit)) limit <- 100L
  if (limit < 0L) limit <- 0L
  if (limit > 1000L) limit <- 1000L
  list(offset = offset, limit = limit)
}

viewer_select_cols <- function(df, cols) {
  if (is.null(cols)) return(names(df))
  if (length(cols) == 1L) {
    raw <- as.character(cols)[1]
    if (!nzchar(raw)) return(names(df))
    wanted <- trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
  } else {
    wanted <- trimws(as.character(cols))
  }
  wanted <- wanted[nzchar(wanted)]
  if (!length(wanted)) return(names(df))
  keep <- character()
  have <- names(df)
  lower <- tolower(have)
  for (w in wanted) {
    hit <- which(have == w)
    if (!length(hit)) hit <- which(lower == tolower(w))
    if (length(hit)) keep <- c(keep, have[hit[1]])
  }
  if (!length(keep)) viewer_stop("None of the requested columns exist.", 400L)
  unique(keep)
}

view_dataset <- function(project, id, offset = 0L, limit = 100L, filter = "", cols = "") {
  p <- viewer_project(project)
  target <- resolve_viewer_id(p, id)
  if (is.null(target)) viewer_stop("Unknown dataset.", 404L)
  if (target$kind %in% c("generated", "parameter")) {
    df <- viewer_generated_frame(p, target$dataset, target$side, target$paramcd)
  } else {
    df <- viewer_cached_frame(target$path)
  }
  if (is.null(df)) {
    if (target$kind %in% c("generated", "parameter")) return(list(
      id = target$id, nrow = 0L, ncol = 0L, columns = list(), rows = list(), offset = 0L,
      limit = viewer_page_args(offset, limit)$limit, empty = TRUE,
      message = "No data generated yet. Generate Keys first."
    ))
    viewer_stop("Dataset is not available.", 404L)
  }
  df <- viewer_prepare(df)
  spec <- tryCatch(read_project_spec(p), error = function(e) NULL)
  page <- viewer_page_args(offset, limit)
  filt <- as.character(filter %||% "")[1]
  if (!nzchar(filt) || is.na(filt)) filt <- ""
  # Filter searches every cell, including columns the caller did not ask to see.
  mask <- viewer_match(df, filt)
  keep_cols <- viewer_select_cols(df, cols)
  df <- df[, keep_cols, drop = FALSE]
  df <- viewer_apply_spec_labels(df, spec, target$dataset)
  idx <- which(mask)
  nkeep <- length(idx)
  if (!page$limit || !nkeep || page$offset >= nkeep) {
    page_idx <- integer()
  } else {
    end <- min(nkeep, page$offset + page$limit)
    page_idx <- idx[seq.int(page$offset + 1L, end)]
  }
  shown <- if (length(page_idx)) df[page_idx, , drop = FALSE] else df[integer(0), , drop = FALSE]
  list(
    id = target$id,
    nrow = as.integer(nkeep),
    ncol = as.integer(ncol(df)),
    columns = viewer_columns(df),
    rows = viewer_rows(shown),
    offset = as.integer(page$offset),
    limit = as.integer(page$limit)
  )
}
