# Per-side variable cards. Facts come only from saved R results.

card_path <- function(project, dataset, node, side) {
  root <- if (is.character(project)) project else project$dir
  file.path(side_run_dir(root, dataset, node, side), "card.json")
}

card_read <- function(project, dataset, node, side) {
  path <- card_path(project, dataset, node, side)
  if (!file.exists(path)) return(NULL)
  tryCatch(read_json(path), error = function(e) NULL)
}

card_remove <- function(project, dataset, node, side) {
  unlink(card_path(project, dataset, node, side))
  invisible()
}

card_parents <- function(graph, node) {
  id <- if (is.list(node)) node$id else as.character(node)[1]
  if (is.null(graph)) return(character())
  unique(vapply(Filter(function(e) identical(e$to %||% "", id), graph$edges %||% list()),
                function(e) as.character(e$from %||% "")[1], ""))
}

card_file_hash <- function(path) {
  if (!file.exists(path)) return("")
  unname(as.character(tools::md5sum(path))[[1]])
}

card_parent_hashes <- function(project, dataset, node, side, graph = NULL) {
  root <- if (is.character(project)) project else project$dir
  if (is.null(graph)) graph <- tryCatch(load_project_graph(root, dataset), error = function(e) NULL)
  parents <- card_parents(graph, node)
  out <- setNames(vector("list", length(parents)), parents)
  for (id in parents) {
    parent <- card_read(root, dataset, id, side)
    out[[id]] <- as.character(parent$facts$code_hash %||% "")[1]
  }
  out
}

card_write <- function(project, dataset, node, side, description = NULL, graph = NULL, spec = NULL) {
  root <- if (is.character(project)) project else project$dir
  id <- if (is.list(node)) node$id else as.character(node)[1]
  var <- parse_node_id(id)$var
  dir <- side_run_dir(root, dataset, id, side)
  out_path <- file.path(dir, "out.rds")
  code_path <- file.path(dir, "code.R")
  df <- tryCatch(readRDS(out_path), error = function(e) NULL)
  if (!is.data.frame(df) || !var %in% names(df)) return(invisible(NULL))
  if (is.null(spec)) spec <- tryCatch(load_project_spec(root), error = function(e) NULL)
  x <- plain_frame(df)[[var]]
  levels <- distinct_levels(x)
  distinct <- if (is.null(levels)) NULL else if (isTRUE(attr(levels, "too_many"))) {
    list(overflow = levels_line(var, levels))
  } else {
    list(values = lapply(seq_len(nrow(levels)), function(i) list(value = levels$value[[i]], n = levels$n[[i]])))
  }
  values <- x[!is.na(x)]
  bounds <- if ((is.numeric(x) || inherits(x, "Date")) && length(values)) {
    list(min = as.character(min(values)), max = as.character(max(values)))
  } else list(min = NULL, max = NULL)
  previous <- card_read(root, dataset, id, side)
  if (is.null(description)) description <- previous$description %||% ""
  facts <- list(
    r_class = class(x)[[1L]], rows = nrow(df), na_count = sum(is.na(x)),
    sample = lineage_column_line(id, x, dataset),
    distinct_levels = distinct, min = bounds$min, max = bounds$max,
    built_at = now_iso(), code_hash = card_file_hash(code_path), out_hash = card_file_hash(out_path),
    parent_code_hashes = card_parent_hashes(root, dataset, id, side, graph)
  )
  card <- list(facts = facts, description = trimws(as.character(description %||% "")[1]),
               user_description = previous$user_description %||% NULL)
  write_json(card, card_path(root, dataset, id, side))
  invisible(card)
}

card_edit <- function(project, dataset, node, side, text = NULL, revert = FALSE) {
  card <- card_read(project, dataset, node, side)
  if (is.null(card)) stop("No card for this side yet.", call. = FALSE)
  if (isTRUE(revert)) card$user_description <- NULL else {
    text <- trimws(as.character(text %||% "")[1])
    if (!nzchar(text)) stop("Description is required.", call. = FALSE)
    card$user_description <- text
  }
  write_json(card, card_path(project, dataset, node, side))
  card
}

card_type_warning <- function(card, spec, dataset, node) {
  if (is.null(card) || is.null(spec)) return(character())
  var <- parse_node_id(if (is.list(node)) node$id else node)$var
  rows <- spec_vars(spec, dataset)
  hit <- which(toupper(rows$Variable) == var)
  if (!length(hit)) return(character())
  row <- rows[hit[[1]], , drop = FALSE]
  declared <- tolower(trimws(paste(c(row$Type %||% "", row$Format %||% ""), collapse = " ")))
  want <- if (grepl("datetime|date.?time|posix", declared)) "datetime" else if (grepl("date", declared)) "date" else
    if (grepl("num|integer|int|float|double", declared)) "numeric" else if (grepl("char|text|string", declared)) "character" else ""
  actual <- card$facts$r_class %||% ""
  okay <- switch(want, date = identical(actual, "Date"), datetime = identical(actual, "POSIXct"),
                 numeric = actual %in% c("numeric", "integer"), character = actual %in% c("character", "factor"), TRUE)
  if (!nzchar(want) || isTRUE(okay)) character() else
    paste0(var, ": spec says ", want, ", result is ", actual)
}

card_view <- function(project, dataset, node, side, spec = NULL, graph = NULL, status = NULL) {
  card <- card_read(project, dataset, node, side)
  if (is.null(card)) return(NULL)
  if (is.null(spec)) spec <- tryCatch(load_project_spec(if (is.character(project)) project else project$dir), error = function(e) NULL)
  current <- card_parent_hashes(project, dataset, node, side, graph)
  old <- card$facts$parent_code_hashes %||% list()
  stale <- !identical(old, current) || length(status$upstream_changed %||% character()) > 0L
  card$warnings <- as.list(c(card_type_warning(card, spec, dataset, node),
                             if (stale) "Upstream changed after this card was built" else character()))
  card$edited_by_user <- !is.null(card$user_description) && nzchar(card$user_description)
  card$display_description <- if (card$edited_by_user) card$user_description else card$description %||% ""
  card
}

card_prompt_block <- function(project, dataset, node, side) {
  path <- card_path(project, dataset, node, side)
  if (!file.exists(path)) return("")
  card <- tryCatch(read_json(path), error = function(e) NULL)
  if (is.null(card)) return("")
  internal <- c("built_at", "code_hash", "out_hash", "parent_code_hashes")
  card$facts[intersect(internal, names(card$facts %||% list()))] <- NULL
  paste0("CARD_BEGIN ", if (is.list(node)) node$id else node, "\n",
         "When user_description is present, use it instead of description.\n",
         to_json(card), "\nCARD_END\n")
}

card_refresh_project <- function(p) {
  root <- if (is.character(p)) p else p$dir
  state <- if (is.environment(p)) p$state else read_project_state(root)
  for (ds in names(state$nodes %||% list())) {
    graph <- tryCatch(load_project_graph(root, ds), error = function(e) NULL)
    spec <- tryCatch(load_project_spec(root), error = function(e) NULL)
    for (id in names(state$nodes[[ds]])) for (side in c("primary", "validation")) {
      st <- state$nodes[[ds]][[id]]
      result <- file.path(side_run_dir(root, ds, id, side), "out.rds")
      if (!file.exists(result) || (identical(st$level %||% "", "none") &&
          !isTRUE(st$sides[[side]]$ok)) ||
          identical(st$problem$message %||% "", "produced 0 rows")) {
        card_remove(root, ds, id, side)
      } else if (identical(st$level %||% "", "L3") && is.null(card_read(root, ds, id, side))) {
        card_write(root, ds, id, side, description = "", graph = graph, spec = spec)
      }
    }
  }
  invisible()
}
