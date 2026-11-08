# Dependency extraction (F5).
# The graph is built from the spec; the LLM (or the fake) only fills edges,
# external refs, and unresolved issues. The graph never writes back to the spec.

structural_var_names <- function() {
  c("STUDYID", "USUBJID", "SUBJID", "SITEID", "SITEGR1", "PARAM", "PARAMCD", "PARAMN",
    "PARCAT1", "PARCAT1N", "PARCAT2", "PARCAT2N", "ASEQ", "AVISIT", "AVISITN",
    "ADT", "ADY", "ATM", "ATPT", "ATPTN", "ADTM", "SRCDOM", "SRCVAR", "SRCSEQ",
    "VISIT", "VISITNUM", "DTYPE", "ABLFL", "ANL01FL")
}

spec_vars <- function(spec, dataset = NULL) {
  v <- if (is.null(spec)) NULL else spec$variables
  if (is.null(v) || !nrow(v)) {
    return(data.frame(row = integer(), Dataset = character(), Variable = character(),
                      Label = character(), Type = character(), Length = integer(),
                      Key = integer(), Source = character(), Derivation = character(),
                      stringsAsFactors = FALSE))
  }
  if (!is.null(dataset)) v <- v[toupper(v$Dataset) == toupper(dataset), , drop = FALSE]
  v
}

spec_params <- function(spec, dataset = NULL) {
  p <- if (is.null(spec)) NULL else spec$parameters
  if (is.null(p) || !nrow(p)) {
    return(data.frame(row = integer(), Dataset = character(), PARAMCD = character(),
                      PARAM = character(), Source = character(), Derivation = character(),
                      stringsAsFactors = FALSE))
  }
  if (!is.null(dataset)) p <- p[toupper(p$Dataset) == toupper(dataset), , drop = FALSE]
  p
}

field_txt <- function(x) {
  x <- x %||% ""
  if (!length(x) || is.na(x[[1]])) return("")
  gsub("[\t\r\n]+", " ", paste(as.character(x[[1]])))
}

parse_spec_categories <- function(text) {
  text <- as.character(text %||% "")
  if (!length(text) || !nzchar(text[[1]])) return(list())
  parts <- unlist(strsplit(text[[1]], "[;\\n]+", perl = TRUE))
  out <- list()
  for (part in parts) {
    part <- trimws(part)
    m <- regexec('^(.+?)\\s+(?:if|=)\\s+(.+)$', part, ignore.case = TRUE, perl = TRUE)
    hit <- regmatches(part, m)[[1]]
    if (length(hit) < 3L) next
    raw_value <- trimws(hit[[2]])
    if (!grepl('^["\']', raw_value) && !grepl('^[<>]=?|^[0-9]', raw_value)) next
    value <- trimws(gsub('^["\']|["\']$', '', raw_value))
    rule <- trimws(hit[[3]])
    if (nzchar(value) && nzchar(rule)) out[[length(out) + 1L]] <- list(value = value, rule = rule)
  }
  out
}

# One node per variable. With parameter rows, non-key analysis variables become
# one param_variable node per PARAMCD (F9i). Keys and structural ids stay single.
graph_skeleton <- function(spec, dataset, layer_keys = NULL, carry_members = NULL) {
  dataset <- toupper(dataset)
  vars <- spec_vars(spec, dataset)
  params <- spec_params(spec, dataset)
  graph <- empty_graph(dataset, spec$hash %||% NULL)
  if (!nrow(vars)) return(graph)
  paramcds <- if (nrow(params)) toupper(trimws(params$PARAMCD)) else character()
  paramcds <- paramcds[nzchar(paramcds)]
  has_params <- length(paramcds) > 0L
  key_order <- suppressWarnings(as.integer(vars$Key))
  key_rows <- which(!is.na(key_order) & key_order != 0L)
  requested_dataset_keys <- if (!is.null(layer_keys) && !is.null(layer_keys[["*"]])) toupper(flat_chr(layer_keys[["*"]])) else toupper(vars$Variable[key_rows])
  dataset_dropped <- setdiff(requested_dataset_keys,c(toupper(vars$Variable),"USUBJID"))
  if(length(dataset_dropped))graph$warnings <- list(paste0("Dropped unknown dataset key(s): ",paste(dataset_dropped,collapse=", "),"."))
  dataset_keys <- intersect(requested_dataset_keys,toupper(vars$Variable))
  multi_key_records <- !has_params && length(dataset_keys) >= 2L
  if (multi_key_records || (!is.null(layer_keys) && !is.null(layer_keys[["*"]]))) graph$key_vars <- unique(dataset_keys)
  if (has_params) {
    carry_members <- unique(toupper(trimws(flat_chr(carry_members))))
    carry_members <- intersect(carry_members, paramcds)
    # Reserve CARRY_OVER for the semantic group. Real parameters keep unique
    # internal layer identities even when their PARAMCD resembles that name.
    layer_of <- function(pc) {
      if (pc %in% carry_members) return("CARRY_OVER")
      if (identical(pc,"CARRY_OVER") && length(carry_members)) {
        candidate <- "PARAM_CARRY_OVER"; suffix <- 2L
        while (candidate %in% setdiff(paramcds, pc) || candidate %in% c("CARRY_OVER", carry_members)) {
          candidate <- paste0("PARAM_CARRY_OVER_", suffix); suffix <- suffix + 1L
        }
        return(candidate)
      }
      pc
    }
    layer_codes <- unique(vapply(paramcds, layer_of, ""))
    if ("CARRY_OVER" %in% layer_codes) layer_codes <- c("CARRY_OVER", setdiff(layer_codes, "CARRY_OVER"))
    graph$layered <- TRUE
    key <- suppressWarnings(as.integer(vars$Key))
    spec_keys <- toupper(vars$Variable[!is.na(key) & key != 0L])
    valid_vars <- toupper(trimws(vars$Variable))
    normalize_keys <- function(pc) {
      requested <- if (!is.null(layer_keys) && !is.null(layer_keys[[pc]])) toupper(flat_chr(layer_keys[[pc]])) else spec_keys
      dropped <- setdiff(requested, c(valid_vars,"USUBJID","PARAMCD"))
      if (length(dropped)) graph$warnings <<- c(graph$warnings %||% list(), list(paste0("Dropped unknown layer key(s) for ", pc, ": ", paste(dropped, collapse=", "), ".")))
      unique(c("USUBJID", "PARAMCD", intersect(requested, valid_vars)))
    }
    member_keys <- setNames(lapply(paramcds, normalize_keys), paramcds)
    graph$layer_keys <- setNames(lapply(layer_codes, function(layer) {
      codes <- paramcds[vapply(paramcds, function(pc) identical(layer_of(pc), layer), TRUE)]
      explicit <- !is.null(layer_keys) && !is.null(layer_keys[[layer]])
      if (explicit && identical(layer,"CARRY_OVER")) {
        unique(c("USUBJID", "PARAMCD", intersect(toupper(flat_chr(layer_keys[[layer]])), valid_vars)))
      } else unique(unlist(member_keys[codes], use.names=FALSE))
    }), layer_codes)
    graph$key_vars <- unique(unlist(graph$layer_keys, use.names=FALSE))
    dataset_level <- function(i, var) {
      if (var == "STUDYID") return(TRUE)
      if (var == "USUBJID") return(FALSE)
      key <- suppressWarnings(as.integer(vars$Key[[i]]))
      if (length(key) == 1L && !is.na(key) && key != 0L) return(FALSE)
      refs <- domain_var_refs(field_txt(vars$Source[[i]]))
      length(refs) > 0L && all(vapply(refs, function(r) r$dom %in% c("ADSL", "DM"), TRUE))
    }
    ds_level_rows <- which(vapply(seq_len(nrow(vars)),function(i)dataset_level(i,toupper(trimws(vars$Variable[[i]]))),TRUE))
    ds_level_vars <- toupper(trimws(vars$Variable[ds_level_rows]))
    for(pc in layer_codes) {
      bad <- intersect(graph$layer_keys[[pc]],ds_level_vars)
      if(length(bad)) graph$warnings <- c(graph$warnings %||% list(),list(paste0("Dropped dataset-level layer key(s) for ",pc,": ",paste(bad,collapse=", "),".")))
      graph$layer_keys[[pc]] <- setdiff(graph$layer_keys[[pc]],bad)
    }
    graph$key_vars <- unique(unlist(graph$layer_keys,use.names=FALSE))
    nodes <- list(); groups <- list(); layers <- list()
    for (i in seq_len(nrow(vars))) {
      var <- toupper(trimws(vars$Variable[[i]])); if (!nzchar(var)) next
      if (dataset_level(i, var)) {
        nodes[[length(nodes)+1L]] <- list(id=paste0(dataset,".",var),var=var,paramcd=NULL,kind="dataset_variable",categories=parse_spec_categories(vars$Derivation[[i]]))
      } else if (identical(var,"USUBJID")) {
        next
      } else for (pc in layer_codes) {
        if (var %in% graph$layer_keys[[pc]]) next
        id <- paste0(dataset,".",var,"@",pc)
        nodes[[length(nodes)+1L]] <- list(id=id,var=var,paramcd=pc,kind="param_variable",categories=parse_spec_categories(vars$Derivation[[i]]))
      }
    }
    for (pc in layer_codes) {
      rid <- paste0(dataset,".RECORDS@",pc)
      nodes[[length(nodes)+1L]] <- list(id=rid,var="RECORDS",paramcd=pc,kind="records",categories=list())
      members <- vapply(Filter(function(n) identical(n$paramcd,pc) && n$kind == "param_variable",nodes), function(n)n$id,"")
      codes <- paramcds[vapply(paramcds, function(code) identical(layer_of(code), pc), TRUE)]
      layers[[length(layers)+1L]] <- list(paramcd=pc,records=rid,members=as.list(members),members_parameters=as.list(codes))
      groups[[length(groups)+1L]] <- list(id=paste0(dataset,"@",pc),paramcd=pc,members=as.list(c(rid,members)),members_parameters=as.list(codes))
    }
    graph$nodes <- nodes; graph$groups <- groups; graph$layers <- layers
    return(graph)
  }
  struct <- structural_var_names()
  nodes <- list()
  if (multi_key_records) nodes[[1L]] <- list(id=paste0(dataset,".RECORDS"),var="RECORDS",paramcd=NULL,kind="records",categories=list())
  groups <- list()
  members <- setNames(vector("list", length(paramcds)), paramcds)
  for (i in seq_len(nrow(vars))) {
    var <- toupper(trimws(vars$Variable[[i]]))
    if (!nzchar(var)) next
    key <- suppressWarnings(as.integer(vars$Key[[i]]))
    is_key <- length(key) == 1L && !is.na(key)
    if (multi_key_records && var %in% graph$key_vars) next
    split <- has_params && !is_key && !(var %in% struct)
    if (!split) {
      nodes[[length(nodes) + 1L]] <- list(
        id = paste0(dataset, ".", var), var = var, paramcd = NULL, kind = "variable",
        categories = parse_spec_categories(vars$Derivation[[i]]))
    } else {
      for (pc in paramcds) {
        id <- paste0(dataset, ".", var, "@", pc)
        nodes[[length(nodes) + 1L]] <- list(
          id = id, var = var, paramcd = pc, kind = "param_variable",
          categories = parse_spec_categories(vars$Derivation[[i]]))
        members[[pc]] <- c(members[[pc]], id)
      }
    }
  }
  for (pc in paramcds) {
    groups[[length(groups) + 1L]] <- list(
      id = paste0(dataset, "@", pc), paramcd = pc, members = members[[pc]] %||% character())
  }
  if (nrow(params)) for (i in seq_len(nrow(params))) {
    pc <- toupper(trimws(params$PARAMCD[[i]]))
    cats <- parse_spec_categories(params$Derivation[[i]])
    if (!length(cats)) next
    for (j in seq_along(nodes)) if (identical(nodes[[j]]$paramcd, pc)) {
      existing <- nodes[[j]]$categories %||% list()
      nodes[[j]]$categories <- c(existing, cats)
    }
  }
  graph$nodes <- nodes
  graph$groups <- groups
  graph
}

# Structured lines the fake parser reads. A trailing '#' keeps empty fields.
prompt_line <- function(...) {
  bits <- vapply(list(...), field_txt, "")
  paste0(paste(bits, collapse = "\t"), "\t#")
}

extract_catalog_lines <- function(spec, dataset, catalog = NULL) {
  lines <- character()
  others <- spec_vars(spec)
  if (nrow(others)) {
    others <- others[toupper(others$Dataset) != dataset, , drop = FALSE]
    for (ds in unique(toupper(others$Dataset))) {
      vv <- unique(toupper(others$Variable[toupper(others$Dataset) == ds]))
      lines <- c(lines, prompt_line("CATALOG", ds, paste(vv, collapse = ",")))
    }
  }
  # Source xpt columns override a same-named spec dataset: the fake parser
  # keeps the last CATALOG line for a domain.
  if (!is.null(catalog) && length(catalog$domains)) {
    for (ds in names(catalog$domains)) {
      vars <- catalog$domains[[ds]]$variables %||% list()
      vv <- unique(toupper(vapply(vars, function(v) as.character(v$name %||% "")[1], "")))
      vv <- vv[nzchar(vv)]
      lines <- c(lines, prompt_line("CATALOG", ds, paste(vv, collapse = ",")))
    }
    if (exists("catalog_prompt_text", mode = "function")) {
      lines <- c(lines, "SOURCE_CATALOG_BEGIN", catalog_prompt_text(catalog), "SOURCE_CATALOG_END")
    }
  }
  lines
}

merge_extract <- function(graph, parsed, spec = NULL) {
  for (cat in as_records(parsed$categories)) {
    id <- field_txt(cat$node)
    value <- field_txt(cat$value)
    rule <- field_txt(cat$rule)
    if (!nzchar(id) || !nzchar(value)) next
    for (i in seq_along(graph$nodes)) if (identical(graph$nodes[[i]]$id, id)) {
      existing <- graph$nodes[[i]]$categories %||% list()
      if (!any(vapply(existing, function(e) identical(e$value, value), logical(1))))
        graph$nodes[[i]]$categories <- c(existing, list(list(value = value, rule = rule)))
    }
  }
  edges <- list()
  ext <- list()
  ext_at <- list()
  compound_keys <- FALSE
  compound_key_vars <- character()
  compound_records_id <- paste0(graph$dataset,".RECORDS")
  if (!isTRUE(graph$layered) && !is.null(spec)) {
    key_spec <- spec_vars(spec,graph$dataset)
    key_order <- if(nrow(key_spec))suppressWarnings(as.integer(key_spec$Key))else integer()
    key_rows <- which(!is.na(key_order) & key_order!=0L)
    compound_keys <- length(flat_chr(graph$key_vars))>=2L && any(vapply(graph$nodes,function(n)identical(n$id,compound_records_id)&&identical(n$kind,"records"),TRUE))
    if(compound_keys)compound_key_vars<-toupper(flat_chr(graph$key_vars %||% key_spec$Variable[key_rows]))
  }
  if (isTRUE(graph$layered)) {
    vars <- unique(vapply(graph$nodes, function(n)n$var, ""))
    params <- unique(vapply(graph$layers, function(x)x$paramcd, ""))
    key_vars <- toupper(flat_chr(graph$key_vars))
    expanded_seen <- character()
    graph$warnings <- graph$warnings %||% list()
    normalize_var <- function(value) {
      value <- toupper(trimws(as.character(value %||% "")[1]))
      prefix <- paste0(toupper(graph$dataset), ".")
      if (startsWith(value, prefix)) substring(value, nchar(prefix) + 1L) else value
    }
    add_expanded <- function(from,to,evidence,origin="llm") {
      key <- paste(from,to,sep="\t")
      if (!nzchar(from) || !nzchar(to) || from==to || key %in% expanded_seen) return()
      expanded_seen <<- c(expanded_seen,key)
      edges[[length(edges)+1L]] <<- list(from=from,to=to,evidence=evidence,origin=origin)
    }
    applicable_params <- function(record) {
      requested <- unique(toupper(flat_chr(record$params)))
      requested <- requested[nzchar(requested)]
      if (!length(requested)) return(params)
      matched <- intersect(params, requested)
      if (!length(matched)) graph$warnings <<- c(graph$warnings, list(
        paste0("Dropped layered dependency with unmatched params: ", paste(requested, collapse = ", "), ".")))
      matched
    }
    node_for <- function(var, pc, target=FALSE) {
      var <- toupper(var)
      pc_keys <- toupper(flat_chr(graph$layer_keys[[pc]] %||% graph$key_vars))
      if (target && var %in% pc_keys) return(paste0(graph$dataset,".RECORDS@",pc))
      hit <- Filter(function(n) identical(n$var,var) && is.null(n$paramcd) && n$kind != "records",graph$nodes)
      if (length(hit)) return(hit[[1]]$id)
      hit <- Filter(function(n) identical(n$var,var) && identical(n$paramcd,pc) && n$kind != "records",graph$nodes)
      if (length(hit)) hit[[1]]$id else ""
    }
    for (e in as_records(parsed$edges)) {
      from <- normalize_var(e$from); to <- normalize_var(e$to)
      if (!from %in% c(vars,key_vars) || !to %in% c(vars,key_vars) || identical(from,to)) next
      pcs <- applicable_params(e)
      for (pc in pcs) {
        pc_keys <- toupper(flat_chr(graph$layer_keys[[pc]] %||% graph$key_vars))
        if (from %in% pc_keys) next
        fid <- node_for(from,pc); tid <- node_for(to,pc,target=TRUE)
        add_expanded(fid,tid,field_txt(e$evidence),e$origin %||% "llm")
      }
    }
    # A layer-by-layer extraction keeps each edge in the layer that named it, so the Source column isn't spread.
    source_vars <- spec_vars(if (isTRUE(parsed$no_source_spread)) NULL else spec, graph$dataset)
    source_ref <- function(value) domain_var_refs(field_txt(value))
    reaches <- function(start, target, edge_list = edges) {
      queue <- start; seen <- character()
      while (length(queue)) {
        id <- queue[[1L]]; queue <- queue[-1L]
        if (identical(id, target)) return(TRUE)
        if (id %in% seen) next
        seen <- c(seen, id)
        queue <- c(queue, vapply(Filter(function(edge) identical(edge$from, id), edge_list), function(edge) edge$to, ""))
      }
      FALSE
    }
    if (nrow(source_vars)) for (i in seq_len(nrow(source_vars))) {
      target <- toupper(trimws(source_vars$Variable[[i]]))
        if (!nzchar(target)) next
      target_nodes <- vapply(params, function(pc) node_for(target, pc, target=TRUE), "")
      target_nodes <- unique(target_nodes[nzchar(target_nodes)])
      refs <- source_ref(source_vars$Source[[i]])
      for (ref_item in refs) {
        source_domain <- toupper(ref_item$dom)
        source_name <- toupper(ref_item$var)
        if (identical(source_domain, toupper(graph$dataset))) {
          if (identical(source_name, target)) next
          model_pair <- any(vapply(edges, function(edge) {
            parse_node_id(edge$from)$var == source_name && parse_node_id(edge$to)$var == target
          }, TRUE))
          if (!model_pair) for (pc in params) {
            if (target %in% graph$layer_keys[[pc]] || source_name %in% graph$layer_keys[[pc]]) next
            from_id <- node_for(source_name, pc)
            to_id <- node_for(target, pc, target=TRUE)
            if (!nzchar(from_id) || !nzchar(to_id)) next
            if (reaches(to_id, from_id)) {
              graph$unresolved <- c(graph$unresolved %||% list(), list(new_issue(
                "cycle", graph$dataset, c(source_name, target), text=field_txt(source_vars$Source[[i]]),
                message=paste0("Skipped Source dependency ", source_name, " -> ", target, " because it would create a cycle."))))
              next
            }
            add_expanded(from_id, to_id, field_txt(source_vars$Source[[i]]), "source")
          }
        } else {
          ref <- paste0(source_domain, ".", source_name)
          used <- vapply(params, function(pc) node_for(target, pc, target=TRUE), "")
          used <- unique(used[nzchar(used)])
          if (!length(used)) next
          if (!is.null(ext_at[[ref]])) {
            idx <- ext_at[[ref]]
            ext[[idx]]$used_by <- unique(c(flat_chr(ext[[idx]]$used_by), used))
          } else {
            ext[[length(ext)+1L]] <- list(ref=ref,domain=source_domain,var=source_name,
              used_by=used,evidence=field_txt(source_vars$Source[[i]]))
            ext_at[[ref]] <- length(ext)
          }
        }
      }
    }
    for (layer in graph$layers) for (id in unlist(layer$members,use.names=FALSE))
      add_expanded(layer$records,id,"All variables are defined on this parameter's records.","structural")
  }
  seen <- character()
  if (!isTRUE(graph$layered)) for (e in as_records(parsed$edges)) {
    from <- field_txt(e$from)
    to <- field_txt(e$to)
    if (!nzchar(from) || !nzchar(to) || identical(from, to)) next
    if(compound_keys) {
      from_var<-split_node_id(from)$var %||% toupper(from)
      to_var<-split_node_id(to)$var %||% toupper(to)
      if(from_var%in%compound_key_vars || to_var%in%compound_key_vars)next
    }
    key <- paste(from, to, sep = "\t")
    if (key %in% seen) next
    seen <- c(seen, key)
    edges[[length(edges) + 1L]] <- list(
      from = from, to = to, evidence = field_txt(e$evidence), origin = e$origin %||% "llm")
  }
  if(compound_keys) for(n in graph$nodes) {
    if(identical(n$id,compound_records_id) || identical(n$kind,"records"))next
    edges[[length(edges)+1L]]<-list(from=compound_records_id,to=n$id,
      evidence="All dataset variables are defined on the compound key rows.",origin="structural")
  }
  for (e in as_records(parsed$external)) {
    ref <- toupper(field_txt(e$ref))
    dom <- toupper(field_txt(e$domain))
    var <- toupper(field_txt(e$var))
    if (!nzchar(dom) && grepl(".", ref, fixed = TRUE)) {
      dom <- sub("\\..*$", "", ref)
      var <- sub("^[^.]*\\.", "", ref)
    }
    if (nzchar(dom) && nzchar(var) && !grepl("\\.", ref)) ref <- paste0(dom, ".", var)
    if (!nzchar(ref)) next
    if (isTRUE(graph$layered) && identical(dom,graph$dataset)) next
    used <- unique(flat_chr(e$used_by))
    if(compound_keys)used<-unique(vapply(used,function(u) {
      parsed<-split_node_id(u);vu<-if(is.null(parsed))toupper(u)else parsed$var
      if(vu%in%compound_key_vars)compound_records_id else u
    },""))
    if (isTRUE(graph$layered)) {
      params <- unique(vapply(graph$layers,function(x)x$paramcd,""))
      pcs <- applicable_params(e)
      expanded <- character()
      for (name in used) for (pc in pcs) {
        name <- normalize_var(name)
        if (name %in% toupper(flat_chr(graph$layer_keys[[pc]] %||% graph$key_vars))) {
          expanded <- c(expanded,paste0(graph$dataset,".RECORDS@",pc))
          next
        }
        if (identical(name, "RECORDS")) { expanded <- c(expanded,paste0(graph$dataset,".RECORDS@",pc)); next }
        hit <- Filter(function(n) identical(n$var,name) && (is.null(n$paramcd) || identical(n$paramcd,pc)) && n$kind != "records",graph$nodes)
        expanded <- c(expanded,vapply(hit,function(n)n$id,""))
      }
      used <- unique(expanded)
    }
    if (!is.null(ext_at[[ref]])) {
      i <- ext_at[[ref]]
      ext[[i]]$used_by <- unique(c(flat_chr(ext[[i]]$used_by), used))
    } else {
      ext[[length(ext) + 1L]] <- list(
        ref = ref, domain = dom, var = var, used_by = used, evidence = field_txt(e$evidence))
      ext_at[[ref]] <- length(ext)
    }
  }
  if(compound_keys) {
    key_spec<-spec_vars(spec,graph$dataset);key_order<-suppressWarnings(as.integer(key_spec$Key))
    for(i in which(!is.na(key_order)&key_order!=0L))for(ref in domain_var_refs(field_txt(key_spec$Source[[i]]))) {
      if(identical(toupper(ref$dom),toupper(graph$dataset)))next
      ref_id<-toupper(ref$ref);at<-which(vapply(ext,function(x)identical(toupper(x$ref %||% ""),ref_id),TRUE))
      if(length(at))ext[[at[[1]]]]$used_by<-unique(c(flat_chr(ext[[at[[1]]]]$used_by),compound_records_id)) else {
        ext[[length(ext)+1L]]<-list(ref=ref_id,domain=toupper(ref$dom),var=toupper(ref$var),
          used_by=compound_records_id,evidence=field_txt(key_spec$Source[[i]]))
      }
    }
  }
  issues <- c(lapply(as_records(parsed$unresolved), function(iss) normalize_issue(iss, graph$dataset)),
              graph$unresolved %||% list())
  graph$edges <- edges
  graph$external <- ext
  graph$unresolved <- Filter(Negate(is.null), issues)
  graph
}

layer_keys_path <- function(project, dataset) {
  root <- if(is.character(project))as.character(project)[1] else if(exists("project_dir_of",mode="function"))project_dir_of(project)else project$dir
  file.path(root, ".dualdag", "state", paste0("layer_keys_", toupper(as.character(dataset)[1]), ".json"))
}
extract_reply_path <- function(project,dataset) {
  root <- if(is.character(project))as.character(project)[1] else if(exists("project_dir_of",mode="function"))project_dir_of(project)else project$dir
  file.path(root, ".dualdag", "state", paste0("extract_", toupper(as.character(dataset)[1]), ".json"))
}

extract_reply_read <- function(project,dataset) tryCatch(read_json(extract_reply_path(project,dataset)),error=function(e)NULL)
extract_reply_write <- function(project,dataset,reply) {
  path<-extract_reply_path(project,dataset);dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE);write_json(reply,path);invisible(reply)
}

layer_keys_read <- function(project, dataset) {
  raw <- tryCatch(read_json(layer_keys_path(project, dataset)), error=function(e) NULL)
  if (is.null(raw) || !is.list(raw)) list() else lapply(raw, function(x) unique(toupper(flat_chr(x))))
}

layer_keys_write <- function(project, dataset, keys) {
  path <- layer_keys_path(project, dataset)
  dir.create(dirname(path), recursive=TRUE, showWarnings=FALSE)
  write_json(keys, path)
  invisible(keys)
}

layer_key_names <- function(spec, dataset, paramcd, graph=NULL, project=NULL) {
  ds <- safe_ds(dataset); pc <- toupper(paramcd %||% "*")
  overrides <- if (!is.null(project)) layer_keys_read(project, ds) else list()
  if (!is.null(overrides[[pc]])) return(unique(c("USUBJID", if (pc != "*") "PARAMCD", overrides[[pc]])))
  if (!is.null(graph$layer_keys[[pc]])) return(unique(c("USUBJID", if (pc != "*") "PARAMCD", toupper(flat_chr(graph$layer_keys[[pc]])))))
  vars <- spec_vars(spec, ds); key <- suppressWarnings(as.integer(vars$Key))
  unique(c("USUBJID", if (pc != "*") "PARAMCD", toupper(vars$Variable[!is.na(key) & key != 0L])))
}

layer_keys_set <- function(project, dataset, paramcd, keys, dry_run=FALSE) {
  ds <- safe_ds(dataset); pc <- toupper(trimws(as.character(paramcd %||% "*")[1]))
  state_project <- if(is.environment(project))project else open_project(project)
  graph <- load_graph(state_project,ds); spec <- read_project_spec(state_project)
  if (is.null(graph)) stop("No dependency graph yet.",call.=FALSE)
  layered <- isTRUE(graph$layered)
  if (layered && !pc %in% vapply(graph$layers, function(x)x$paramcd,"")) stop("Unknown parameter layer: ",pc,".",call.=FALSE)
  if (!layered && pc != "*") stop("Use paramcd '*' for a dataset-level key set.",call.=FALSE)
  requested <- unique(toupper(trimws(flat_chr(keys))))
  requested <- requested[nzchar(requested)]
  required <- c("USUBJID",if(layered)"PARAMCD")
  missing_required <- setdiff(required,requested)
  if (length(missing_required)) stop("Required key column(s) cannot be removed: ",paste(missing_required,collapse=", "),".",call.=FALSE)
  vars <- toupper(trimws(spec_vars(spec,ds)$Variable))
  fixed <- c("USUBJID",if(layered)"PARAMCD")
  optional <- setdiff(requested,fixed)
  unknown <- setdiff(optional,vars)
  if(length(unknown))stop("Unknown key variable(s): ",paste(unknown,collapse=", "),".",call.=FALSE)
  if(layered) {
    ds_vars <- vapply(Filter(function(n)identical(n$kind,"dataset_variable"),graph$nodes),`[[`,"","var")
    bad <- intersect(optional,ds_vars)
    if(length(bad))stop("Dataset-level variable(s) cannot be layer keys: ",paste(bad,collapse=", "),".",call.=FALSE)
  }
  old_keys <- layer_key_names(spec,ds,pc,graph,state_project)
  if(setequal(old_keys,requested)) return(list(ok=TRUE,keys=as.list(old_keys),reset_count=0L,dry_run=isTRUE(dry_run)))
  extraction <- graph$extraction %||% extract_reply_read(state_project,ds)
  if(is.null(extraction)) stop("This graph has no stored extraction reply; re-extract it before changing keys.",call.=FALSE)
  key_map <- graph$layer_keys %||% list("*"=graph$key_vars %||% character())
  key_map[[pc]] <- requested
  carry_members <- vapply(Filter(function(x) identical(x$classification,"carry_over"), as_records(extraction$parameter_classification$parameters)),function(x)toupper(x$paramcd %||% ""),"")
  fresh <- graph_skeleton(spec,ds,key_map,carry_members)
  fresh$extraction <- extraction
  fresh <- merge_extract(fresh,extraction,spec)
  fresh$generated_parameters <- graph$generated_parameters
  fresh$cycle_cuts <- graph$cycle_cuts
  fresh$param_uses <- graph$param_uses
  fresh$layer_warnings <- graph$layer_warnings
  affected <- pc
  if(layered) {
    pg <- tryCatch(param_graph_read(state_project,ds),error=function(e)NULL)
    seeds <- if (identical(pc,"CARRY_OVER")) unique(c(pc,carry_members)) else pc
    affected <- seeds
    repeat {
      children <- unique(vapply(Filter(function(e)toupper(e$from)%in%affected,pg$edges %||% list()),function(e)toupper(e$to),""))
      more <- setdiff(children,affected); if(!length(more))break
      affected <- c(affected,more)
    }
    affected <- unique(c(if(identical(pc,"CARRY_OVER")) "CARRY_OVER", affected))
  }
  ids <- if(layered)unique(unlist(lapply(affected,function(code)c(layer_node_ids(graph,code),layer_node_ids(fresh,code))),use.names=FALSE)) else
    unique(c(vapply(graph$nodes,`[[`,"","id"),vapply(fresh$nodes,`[[`,"","id")))
  reset_count <- length(ids)
  if(isTRUE(dry_run))return(list(ok=TRUE,keys=as.list(requested),reset_count=reset_count,dry_run=TRUE,affected_layers=as.list(affected)))
  stored <- layer_keys_read(state_project,ds); stored[[pc]] <- setdiff(requested,fixed)
  layer_keys_write(state_project,ds,stored)
  extract_reply_write(state_project,ds,extraction)
  fresh$extraction <- NULL
  save_graph(state_project,fresh)
  for(id in ids) reset_one_node(state_project,ds,id,delete_code=TRUE)
  for(side in c("primary","validation")) if(exists("work_path",mode="function")) unlink(work_path(project_dir_of(state_project),ds,side))
  write_state(state_project)
  list(ok=TRUE,keys=as.list(requested),reset_count=reset_count,dry_run=FALSE,affected_layers=as.list(affected))
}

# ---- extraction: plan, one pass per parameter, check, cycles ----

EXTRACT_MAX_CYCLE_ROUNDS <- 3L

extract_prompt_header <- function(task, dataset) paste0("TASK: ", task, "\nDATASET: ", dataset, "\n")

extract_plan_prompt <- function(dataset) paste0(extract_prompt_header("extract_plan", dataset),
  "You are a senior CDISC ADaM reviewer. Read this whole specification and split all its rows into batches for another model, which will list what each row uses.\n",
  "Simple, alike rows can share a batch of 10 to 20 rows. Long or complex rows go alone or in a batch of 2 to 3. Use as few batches as quality allows. Every row id must appear exactly once.\n",
  "Also read the Key column and return the row keys. For a dataset with parameters, give the keys of each PARAMCD: a derived parameter may not have a time point. With no parameters, use '*'. A parameter you leave out uses the Key column.\n",
  'Return only JSON: {"batches":[{"kind":"simple|complex","rows":["V1"]}],"layer_keys":{"PARAMCD":["VAR"]}}')

extract_rows_prompt <- function(dataset, spec_text, layered, catalog_lines = character(), grouped = FALSE) paste0(extract_prompt_header("extract_rows", dataset),
  "You are a CDISC ADaM expert. This is the whole specification:\n\n", spec_text, "\n\n",
  if (length(catalog_lines)) paste0("Columns of other datasets. Use them to name the dataset of a bare column name:\n", paste(catalog_lines, collapse = "\n"), "\n\n") else "",
  if (layered && grouped) paste0(
    "You will be given all members of one CARRY_OVER layer and its specification rows in a single grouped request. Return one result for each exact requested row ID. For each row, return ONLY the union of dependency reference tokens needed across the shared-layer members. If a dependency applies only to particular members, qualify it as VAR@PARAMCD; use VAR for a same-dataset dependency shared by the members, and DATASET.VAR for another dataset. Include every requested variable and parameter row, including rows with an empty dependency list. Use only references supported by the specification; do not infer edges. Do not return derivation logic, member rules, explanations, or sentences. The value for every row must be an array of reference tokens only.\n") else if (layered) paste0(
    "You work on one parameter at a time. You will be shown a parameter code and some rows. For each row, list only what that row uses when its value is computed, or its records are selected, for THIS parameter's records. Read the row's clauses: ignore any clause written for other parameters. If the row leaves its value missing for this parameter, or does not need anything for it, return an empty list. A rule counts for this parameter unless the text excludes it: list every variable a rule names, even if that variable's value happens to be missing on this parameter's records.\n",
    "Write a variable in the same parameter as VAR, keeping a dataset prefix such as ADSL.TRTSDT or VS.VSSEQ when the source is another dataset. When a value comes from another parameter's records, write VAR@PARAMCD, such as AVAL@WEIGHT. Do not list the row's own variable unless it comes from another parameter. Names from the specification only, no sentences.\n")
  else "You will be shown some rows. For each row, list what it uses when its value is computed. Write a variable in this dataset as VAR, keeping a dataset prefix such as ADSL.TRTSDT when the source is another dataset. Do not list the row's own variable. Names from the specification only, no sentences.\n",
  'Return only JSON: {"ROW_ID":["item"]}')

extract_check_prompt <- function(dataset, spec_text, layered) paste0(extract_prompt_header("extract_check", dataset),
  "You are a senior CDISC ADaM reviewer. This is the whole specification:\n\n", spec_text, "\n\n",
  "You will get draft dependency edges, written FROM -> TO, where TO is the row being derived. An edge is correct only if this specification shows that the FROM value is used to compute TO or to select its records, in that direction.\n",
  "List the id of every edge that is wrong: not supported by the text, or pointing the wrong way. You can only remove edges. You cannot add or reverse any. Give no reasons.\n",
  if (layered) "Nodes are written VARIABLE@PARAMETER, so judge each edge only for that parameter. RECORDS@X is the row set of parameter X. A row-key variable has no node of its own, so an edge marked (via key variable K) means the source is used to derive K for parameter X: judge it against K's row, and keep it if K's row uses that source for X. A rule counts for a parameter unless the text excludes it, so keep an edge the rule names even if the variable's value happens to be missing on that parameter's records.\n" else "",
  "Also report any row whose reference is ambiguous, names a variable that is not in the specification, or is only prose with no resolvable source. Issue types: ambiguous, unresolvable, missing_ref.\n",
  if (layered) "" else "Also list category values and rules that a Derivation states.\n",
  'Return only JSON: {"cut":["E1"],"unresolved":[{"type":"","dataset":"","variables":[],"file":"variable.csv","row":null,"text":"","message":"","suggestion":""}],"categories":[{"node":"","value":"","rule":""}]}')

extract_cycle_prompt <- function(dataset, spec_text) paste0(extract_prompt_header("extract_cycles", dataset),
  "You are a senior CDISC ADaM reviewer. This is the whole specification:\n\n", spec_text, "\n\n",
  "A draft dependency graph has cycles. Nodes are written VARIABLE@PARAMETER. A valid ADaM dataset must be computable in order, so a cycle usually means an edge was over-extracted or spread to parameters where it does not apply.\n",
  "Read the specification carefully and decide which edges are wrong. You can only remove edges. You cannot add or reverse any. An edge marked (structural) cannot be removed. Remove as few as the text justifies, and do not remove an edge the text supports for that parameter.\n",
  "If the specification itself truly contradicts, list it under conflict instead of cutting.\n",
  'Return only JSON: {"cut":[{"from":"","to":"","reason":"one sentence with a short exact quote"}],"conflict":[""]}')

# The normal model is the one chosen in Settings. The strong model falls back to it.
extract_strong_cfg <- function(cfg) {
  strong <- trimws(as.character(cfg$strong_model %||% "")[1])
  if (!is.na(strong) && nzchar(strong)) cfg$model <- strong
  cfg
}

extract_call <- function(cfg, system, user, max_tokens = 8000L) {
  parse_json_reply(llm_chat(cfg, system, user, max_tokens = max_tokens, json = TRUE)$text)
}

extract_spec_rows <- function(spec, dataset) {
  rows <- list()
  add <- function(id, kind, target, label_name, label, key, source, derivation, file, row) {
    rows[[id]] <<- list(id = id, kind = kind, target = target, source = source, derivation = derivation,
      block = paste0("ROW ", id, " | ", if (kind == "var") "Variable" else "PARAMCD", ": ", target, " | ", label_name, ": ", label,
                     if (nzchar(key)) paste0(" | Key: ", key) else "", " | Row: ", row,
                     "\nSource: ", source, "\nDerivation: ", derivation))
  }
  v <- spec_vars(spec, dataset)
  for (i in seq_len(nrow(v))) {
    name <- toupper(trimws(v$Variable[[i]])); if (!nzchar(name)) next
    key <- suppressWarnings(as.integer(v$Key[[i]]))
    add(paste0("V", i), "var", name, "Label", field_txt(v$Label[[i]]), if (length(key) == 1L && !is.na(key)) as.character(key) else "",
        field_txt(v$Source[[i]]), field_txt(v$Derivation[[i]]), "variable.csv", if (!is.null(v$row)) as.character(v$row[[i]]) else as.character(i))
  }
  p <- spec_params(spec, dataset)
  for (i in seq_len(nrow(p))) {
    name <- toupper(trimws(p$PARAMCD[[i]])); if (!nzchar(name)) next
    add(paste0("P", i), "param", name, "PARAM", field_txt(p$PARAM[[i]]), "",
        field_txt(p$Source[[i]]), field_txt(p$Derivation[[i]]), "parameter.csv", if (!is.null(p$row)) as.character(p$row[[i]]) else as.character(i))
  }
  rows
}

extract_plan_path <- function(project, dataset) {
  root <- if (is.character(project)) as.character(project)[1] else if (exists("project_dir_of", mode = "function")) project_dir_of(project) else project$dir
  file.path(root, ".dualdag", "state", paste0("extract_plan_", toupper(as.character(dataset)[1]), ".json"))
}

extract_classification_prompt <- function(dataset, spec, spec_text) {
  vars <- spec_vars(spec, dataset)
  src <- vars[toupper(trimws(vars$Variable)) %in% c("SRCDOM", "SRCVAR", "SRCSEQ"), , drop=FALSE]
  src_text <- if (nrow(src)) paste(vapply(seq_len(nrow(src)), function(i)
    paste("SPEC", field_txt(src$Variable[[i]]), field_txt(src$Label[[i]]),
          field_txt(src$Type[[i]]), field_txt(src$Length[[i]]),
          field_txt(src$Source[[i]]), field_txt(src$Derivation[[i]]), sep="\t"), ""), collapse="\n") else ""
  params <- spec_params(spec, dataset)
  ptext <- if (nrow(params)) paste(vapply(seq_len(nrow(params)), function(i)
    paste("PARAM", field_txt(params$PARAMCD[[i]]), field_txt(params$PARAM[[i]]),
          field_txt(params$Source[[i]]), field_txt(params$Derivation[[i]]), sep="\t"), ""), collapse="\n") else ""
  system <- paste0("TASK: extract_classification\n",
    "Classify each parameter named in PARAMCD_LIST as carry_over or derived, and give a one-sentence reason. You may answer cannot_tell. SRCDOM, SRCVAR and SRCSEQ are spec variables given as information, not items to classify. Return JSON only: {\"parameters\":[{\"paramcd\":\"\",\"classification\":\"carry_over|derived|cannot_tell\",\"reason\":\"\"}]}")
  user <- paste0("PARAMCD_LIST: ", paste(unique(toupper(trimws(params$PARAMCD))), collapse=", "),
    "\n\nSRCDOM_SRCVAR_SRCSEQ_SPEC\n", src_text, "\nPARAMETER_DESCRIPTIONS\n", ptext)
  list(system=system, user=user)
}

extract_classification_path <- function(project, dataset) {
  root <- if (is.character(project)) as.character(project)[1] else if (exists("project_dir_of", mode="function")) project_dir_of(project) else project$dir
  file.path(root, ".dualdag", "state", paste0("extract_classification_", toupper(dataset), ".json"))
}

extract_classification <- function(cfg, spec, dataset, project=NULL) {
  params <- spec_params(spec, dataset)
  codes <- unique(toupper(trimws(params$PARAMCD))); codes <- codes[nzchar(codes)]
  path <- if (!is.null(project)) extract_classification_path(project, dataset)
  saved <- if (!is.null(path)) tryCatch(read_json(path), error=function(e) NULL)
  if (is.list(saved) && identical(saved$spec_hash, spec$hash %||% NULL) && length(saved$parameters)) return(saved)
  prompt <- extract_classification_prompt(dataset, spec, "")
  reply <- tryCatch(extract_call(extract_strong_cfg(cfg), prompt$system, prompt$user), error=function(e) stop("Parameter classification failed: ", conditionMessage(e), call.=FALSE))
  records <- if (is.list(reply$parameters) && !is.null(reply$parameters$paramcd)) list(reply$parameters) else as_records(reply$parameters)
  got <- vapply(records, function(x) toupper(field_txt(x$paramcd)), "")
  if (length(records) != length(codes) || anyDuplicated(got) || !setequal(got, codes) ||
      any(!vapply(records, function(x) field_txt(x$classification) %in% c("carry_over","derived","cannot_tell") && nzchar(field_txt(x$reason)), TRUE)))
    stop("Parameter classification failed: reply must cover every PARAMCD exactly once with a classification and reason (expected ", paste(codes,collapse=","), "; received ", paste(got,collapse=","), ").", call.=FALSE)
  uncertain <- character()
  for (i in seq_along(records)) {
    records[[i]]$paramcd <- got[[i]]
    if (identical(field_txt(records[[i]]$classification), "cannot_tell")) {
      uncertain <- c(uncertain, paste0(got[[i]], ": ", field_txt(records[[i]]$reason)))
      records[[i]]$classification <- "derived"
    }
  }
  result <- list(spec_hash=spec$hash %||% NULL, parameters=records, issues=as.list(uncertain))
  if (!is.null(path)) { dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE); write_json(result,path) }
  result
}

# All layers of one spec share a plan, so generating them one by one doesn't repeat the strong-model call.
extract_plan <- function(cfg, spec, dataset, spec_text, project = NULL) {
  path <- if (!is.null(project)) extract_plan_path(project, dataset)
  saved <- if (!is.null(path)) tryCatch(read_json(path), error = function(e) NULL)
  if (is.list(saved) && identical(saved$spec_hash, spec$hash %||% NULL) && length(saved$plan$batches)) return(saved$plan)
  plan <- extract_call(extract_strong_cfg(cfg), extract_plan_prompt(dataset), spec_text)
  if (!is.list(plan) || !length(as_records(plan$batches))) stop("The plan reply has no batches.", call. = FALSE)
  if (!is.null(path)) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    write_json(list(spec_hash = spec$hash %||% NULL, plan = plan), path)
  }
  plan
}

extract_batches <- function(plan, rows) {
  seen <- character(); batches <- list()
  for (b in as_records(plan$batches)) {
    ids <- setdiff(intersect(flat_chr(b$rows), names(rows)), seen)
    seen <- c(seen, ids)
    if (length(ids)) batches[[length(batches) + 1L]] <- ids
  }
  c(batches, as.list(setdiff(names(rows), seen)))
}

# The first call runs alone so it can write the provider's prompt cache for the rest.
# Forking isn't available on Windows, so there the jobs run one after another.
extract_map <- function(jobs, fun, cores = getOption("dualdag.extract_cores", 4L), os = .Platform$OS.type) {
  if (length(jobs) < 2L) return(lapply(jobs, fun))
  rest <- jobs[-1L]
  first <- fun(jobs[[1L]])
  more <- if (identical(os, "unix") && as.integer(cores) > 1L) {
    parallel::mclapply(rest, fun, mc.cores = as.integer(cores), mc.preschedule = FALSE)
  } else lapply(rest, fun)
  c(list(first), more)
}

extract_canonical_row_id <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) return(NA_character_)
  value <- toupper(x)
  match <- regmatches(value, regexec("^[[:space:]]{0,2}(?:ROW[[:space:]]{1,2})?([VP][0-9]+)[[:space:]]{0,2}$", value, perl=TRUE))[[1]]
  if (length(match) != 2L) return(NA_character_)
  match[[2]]
}

# Validate reply keys before using them. Return only requested rows and surface
# malformed, unknown, and conflicting entries as errors for targeted retry.
extract_validate_row_reply <- function(reply, requested) {
  requested_ids <- vapply(requested, extract_canonical_row_id, "")
  if (anyNA(requested_ids) || anyDuplicated(requested_ids))
    stop("Malformed requested row IDs.", call. = FALSE)
  if (!is.list(reply) || is.null(names(reply)) || !length(names(reply)))
    stop("Malformed row reply: expected an object keyed by requested row IDs.", call. = FALSE)
  out <- list()
  for (i in seq_along(reply)) {
    raw <- names(reply)[[i]]
    id <- extract_canonical_row_id(raw)
    if (is.na(id)) stop("Malformed row ID in reply: '", raw, "'.", call. = FALSE)
    if (!id %in% requested_ids) stop("Unknown row ID in reply: '", raw, "' (", id, ").", call. = FALSE)
    value <- reply[[i]]
    if (!is.list(value) && !is.character(value)) stop("Malformed reply for ", id, ": expected an array of references.", call. = FALSE)
    vals <- flat_chr(value)
    if (length(vals) && any(!grepl("^(?:[A-Za-z][A-Za-z0-9_]*\\.)?[A-Za-z][A-Za-z0-9_]*(?:@[A-Za-z][A-Za-z0-9_]*)?$", trimws(vals))))
      stop("Malformed reply for ", id, ": dependency array contains prose instead of reference tokens.", call. = FALSE)
    if (id %in% names(out)) {
      if (!identical(out[[id]], vals)) stop("Conflicting duplicate row IDs in reply for ", id, ".", call. = FALSE)
    } else out[[id]] <- vals
  }
  out
}

extract_row_job <- function(job, cfg, rows, system) {
  user <- paste0(if (nzchar(job$label)) paste0("Parameter: ", job$label, "\n\n") else "",
                 paste(vapply(rows[job$ids], function(r) r$block, ""), collapse = "\n\n"))
  tryCatch({
    reply <- extract_call(cfg, system, user, max_tokens = 4000L)
    validated <- extract_validate_row_reply(reply, job$ids)
    list(layer = job$layer, items = validated, error = NULL)
  }, error = function(e) list(layer = job$layer, items = list(), error = paste0(job$label, ": ", conditionMessage(e))))
}

extract_carry_job <- function(job, cfg, rows, system) {
  request_ids <- job$request_ids %||% job$ids
  user <- paste0("CARRY_OVER_PARAMETERS: ", paste(job$parameters, collapse=", "), "\n\n",
      paste(vapply(rows[request_ids], function(r) r$block, ""), collapse="\n\n"),
    if (length(job$retry_feedback %||% character())) paste0("\n\nRETRY CORRECTION: The previous reply was rejected: ",
      paste(job$retry_feedback, collapse="; "), ". Expected JSON schema: {\"ROW_ID\":[\"REFERENCE_TOKEN\"]}. Return each requested row ID exactly once (use an empty array when it has no dependencies). Array items must be reference tokens only, such as VAR, DATASET.VAR, or VAR@PARAMCD. Prose, derivation sentences, and explanations are invalid. Resolve these row IDs: ", paste(request_ids, collapse=", "), ". Do not add unsupported dependencies.") else "")
  tryCatch({
    reply <- extract_call(cfg, system, user, max_tokens=8000L)
    if (!is.list(reply)) stop("Missing grouped carry-over result.")
    validated <- extract_validate_row_reply(reply, request_ids)
    list(layer="CARRY_OVER", items=validated, error=NULL)
  }, error=function(e) list(layer="CARRY_OVER", items=list(), error=paste0("Carry-over group: ", conditionMessage(e))))
}

extract_find_cycles <- function(pairs) {
  if (!length(pairs)) return(list())
  el <- data.frame(from = vapply(pairs, `[[`, "", 1L), to = vapply(pairs, `[[`, "", 2L), stringsAsFactors = FALSE)
  g <- igraph::graph_from_data_frame(el, directed = TRUE)
  comp <- igraph::components(g, mode = "strong")
  out <- lapply(which(comp$csize > 1L), function(k) sort(names(comp$membership)[comp$membership == k]))
  c(out, as.list(unique(el$from[el$from == el$to])))
}

# Turn each row's list into parsed edges and external references, the shape merge_extract reads.
extract_parse_items <- function(found, rows, dataset, layers, row_vars, carry_members=character()) {
  edges <- list(); external <- list(); uses <- list(); context <- character(); not_placed <- character()
  for (k in names(found)) {
    parts <- strsplit(k, "|", fixed = TRUE)[[1]]
    pc <- parts[[1]]; r <- rows[[parts[[2]]]]
    to <- if (r$kind == "param") "RECORDS" else r$target
    evidence <- substr(if (nzchar(r$derivation)) r$derivation else r$source, 1L, 2000L)
    for (raw in found[[k]]) {
      item <- toupper(trimws(raw))
      m <- regmatches(item, regexec(paste0("^(?:", dataset, "\\.)?([A-Z][A-Z0-9_]*)@([A-Z][A-Z0-9_]*)$"), item, perl = TRUE))[[1]]
      if (length(m) && m[[3]] %in% layers) {
        if (identical(m[[3]], pc) || (identical(pc,"CARRY_OVER") && m[[3]] %in% carry_members)) item <- m[[2]]
        else if (nzchar(pc)) {
          uses[[paste(m[[3]], pc, sep = "|")]] <- c(uses[[paste(m[[3]], pc, sep = "|")]], list(list(from_var = m[[2]], to_var = to)))
          next
        }
      }
      # A source dataset value reached through another parameter's record is still that source.
      via <- regmatches(item, regexec("^([A-Z][A-Z0-9_]*\\.[A-Z][A-Z0-9_]*)@([A-Z][A-Z0-9_]*)$", item))[[1]]
      if (length(via) && via[[3]] %in% layers) item <- via[[2]]
      dot <- regmatches(item, regexec("^([A-Z][A-Z0-9_]*)\\.([A-Z][A-Z0-9_]*)$", item))[[1]]
      dom <- if (length(dot)) dot[[2]] else ""
      name <- if (length(dot)) dot[[3]] else item
      if (nzchar(dom) && !identical(dom, dataset)) {
        ref <- paste0(dom, ".", name)
        external[[paste(ref, to, pc, sep = "|")]] <- list(ref = ref, domain = dom, var = name, used_by = to, evidence = evidence, pc = pc)
      } else if (!nzchar(dom) && name %in% layers) {
        if (nzchar(pc) && !identical(name, pc)) uses[[paste(name, pc, sep = "|")]] <- c(uses[[paste(name, pc, sep = "|")]], list(list(from_var = "RECORDS", to_var = to)))
      } else if (name %in% row_vars) {
        if (identical(name, to)) next
        if (identical(to, "RECORDS")) { not_placed <- c(not_placed, paste0(name, " -> RECORDS@", pc)); next }
        edges[[paste(name, to, pc, sep = "|")]] <- list(from = name, to = to, evidence = evidence, origin = "llm", pc = pc)
      } else context <- c(context, item)
    }
  }
  list(edges = unname(edges), external = unname(external), uses = uses, context = sort(unique(context)), not_placed = sort(unique(not_placed)))
}

# Node ids of a parsed record, so a cut on the graph can be applied back to the parsed list.
extract_record_ids <- function(graph, from, to, pc) {
  ds <- graph$dataset
  if (!isTRUE(graph$layered)) return(c(paste0(ds, ".", from), paste0(ds, ".", to)))
  ids <- vapply(graph$nodes, function(n) n$id, "")
  keys <- if (nzchar(pc)) toupper(flat_chr(graph$layer_keys[[pc]])) else character()
  node <- function(var) {
    if (identical(var, "RECORDS") || var %in% keys) return(paste0(ds, ".RECORDS@", pc))
    plain <- paste0(ds, ".", var)
    if (plain %in% ids) plain else paste0(plain, "@", pc)
  }
  c(if (grepl(".", from, fixed = TRUE)) from else node(from), node(to))
}

extract_to_parsed <- function(items, graph, unresolved = list(), categories = list()) {
  layered <- isTRUE(graph$layered)
  name <- function(v) if (layered) v else paste0(graph$dataset, ".", v)
  with_params <- function(x, pc) { x$pc <- NULL; if (nzchar(pc)) x$params <- list(pc); x }
  list(
    edges = lapply(items$edges, function(e) with_params(c(list(from = name(e$from), to = name(e$to)), e[c("evidence", "origin", "pc")]), e$pc)),
    external = lapply(items$external, function(x) { pc <- x$pc; x$used_by <- list(name(x$used_by)); with_params(x, pc) }),
    unresolved = unresolved, categories = categories, no_source_spread = TRUE)
}

extract_remove <- function(items, graph, cut_pairs) {
  keep <- function(from, to, pc) {
    ids <- extract_record_ids(graph, from, to, pc)
    !paste(ids, collapse = "|") %in% cut_pairs
  }
  items$edges <- Filter(function(e) keep(e$from, e$to, e$pc), items$edges)
  items$external <- Filter(function(x) keep(x$ref, x$used_by, x$pc), items$external)
  items
}

extract_dependencies <- function(cfg, spec, dataset, catalog = NULL, project = NULL, paramcd = NULL,
                                 include_dataset_level = TRUE) {
  dataset <- toupper(trimws(dataset))
  rows <- extract_spec_rows(spec, dataset)
  spec_text <- paste(vapply(rows, function(r) r$block, ""), collapse = "\n\n")
  classification <- if (nrow(spec_params(spec, dataset))) extract_classification(cfg, spec, dataset, project) else NULL
  plan <- extract_plan(cfg, spec, dataset, spec_text, project)
  keys <- plan$layer_keys
  overrides <- if (!is.null(project)) layer_keys_read(project, dataset) else list()
  for (pc in names(overrides)) { if (!is.list(keys)) keys <- list(); keys[[pc]] <- overrides[[pc]] }
  classes <- classification$parameters %||% list()
  carry_codes <- vapply(Filter(function(x) identical(x$classification,"carry_over"), classes), function(x) toupper(x$paramcd), "")
  graph <- graph_skeleton(spec, dataset, carry_members=carry_codes)
  if (is.list(keys) && length(keys) && (isTRUE(graph$layered) || !is.null(keys[["*"]]))) graph <- graph_skeleton(spec, dataset, keys, carry_codes)
  layered <- isTRUE(graph$layered)
  all_layers <- if (layered) vapply(graph$layers, function(l) l$paramcd, "") else character()
  layers <- all_layers
  if (layered && !is.null(paramcd)) {
    requested <- toupper(trimws(paramcd))
    if (length(requested) != 1L || !(requested %in% c(all_layers, carry_codes))) stop("Unknown parameter layer: ", paste(requested, collapse=","), ".", call. = FALSE)
    layers <- if (requested %in% c(carry_codes,"CARRY_OVER")) "CARRY_OVER" else requested
  }
  ds_vars <- unlist(lapply(graph$nodes, function(n) if (identical(n$kind, "dataset_variable")) n$var))
  row_vars <- unique(unlist(lapply(rows, function(r) if (r$kind == "var") r$target)))
  batches <- extract_batches(plan, rows)

  jobs <- list()
  add_job <- function(layer, label, ids) if (length(ids)) jobs[[length(jobs) + 1L]] <<- list(layer = layer, label = label, ids = ids)
  if (layered) {
    in_layer <- function(i) rows[[i]]$kind == "var" && !rows[[i]]$target %in% c(ds_vars, "USUBJID")
    carry <- if ("CARRY_OVER" %in% layers) carry_codes else character()
    derived <- setdiff(layers, "CARRY_OVER")
    if (length(carry)) {
      variable_ids <- unique(unlist(lapply(batches, function(b) unlist(Filter(in_layer,b))), use.names=FALSE))
      parameter_ids <- names(Filter(function(r) r$kind=="param" && r$target %in% carry, rows))
      group_ids <- unique(c(variable_ids, parameter_ids))
      jobs[[length(jobs)+1L]] <- list(layer="CARRY_OVER", label="carry-over", ids=group_ids, parameters=carry, grouped=TRUE)
    }
    for (pc in derived) {
      for (b in batches) add_job(pc, pc, unlist(Filter(in_layer, b)))
      add_job(pc, pc, unlist(Filter(function(i) rows[[i]]$kind == "param" && identical(rows[[i]]$target, pc), names(rows))))
    }
    if (isTRUE(include_dataset_level)) add_job("", "(all parameters: this is a dataset-level variable)",
      unlist(Filter(function(i) rows[[i]]$kind == "var" && rows[[i]]$target %in% ds_vars, names(rows))))
  } else for (b in batches) add_job("", "", b)

  catalog_lines <- extract_catalog_lines(spec, dataset, catalog)
  system <- extract_rows_prompt(dataset, spec_text, layered, grep("^CATALOG\t", catalog_lines, value = TRUE))
  group_system <- extract_rows_prompt(dataset, spec_text, layered, grep("^CATALOG\t", catalog_lines, value = TRUE), grouped=TRUE)
  found <- list(); errors <- character()
  take <- function(results) for (r in results) {
    if (inherits(r, "try-error")) { errors <<- c(errors, as.character(r)); next }
    if (!is.null(r$error)) errors <<- c(errors, r$error)
    for (i in names(r$items)) {
      key <- if (grepl("|", i, fixed=TRUE)) i else paste(r$layer, i, sep = "|")
      if (is.null(found[[key]])) found[[key]] <<- flat_chr(r$items[[i]])
    }
  }
  take(extract_map(jobs, function(j) if (isTRUE(j$grouped)) extract_carry_job(j,cfg,rows,group_system) else extract_row_job(j, cfg, rows,system)))
  lacking <- function() unlist(lapply(jobs, function(j) {
    if (isTRUE(j$grouped)) {
      missing_ids <- Filter(function(id) is.null(found[[paste(j$layer,id,sep="|")]]), j$ids)
      if (length(missing_ids)) list(c(j, list(request_ids=missing_ids, retry_feedback=tail(errors, 1L)))) else list()
    } else lapply(Filter(function(i) is.null(found[[paste(j$layer, i, sep = "|")]]), j$ids),
      function(i) list(layer = j$layer, label = j$label, ids = i))
  }), recursive = FALSE)
  for (j in lacking()) take(list(if (isTRUE(j$grouped)) extract_carry_job(j,cfg,rows,group_system) else extract_row_job(j, cfg, rows, system)))
  still <- lacking()
  if (length(still)) stop("The model returned no result for ", paste(vapply(still, function(j) if (isTRUE(j$grouped)) "carry-over group" else paste0(j$layer, if (nzchar(j$layer)) "/", rows[[j$ids]]$target), ""), collapse = ", "),
                          if (length(errors)) paste0(". Last error: ", errors[[length(errors)]]) else ".", call. = FALSE)

  items <- extract_parse_items(found, rows, dataset, unique(c(all_layers, carry_codes, toupper(trimws(spec_params(spec,dataset)$PARAMCD)))), row_vars, carry_codes)
  merged <- function(unresolved = list(), categories = list()) merge_extract(graph, extract_to_parsed(items, graph, unresolved, categories), if (layered) NULL else spec)
  g <- merged()

  # Strong check: every model edge, cut only.
  via <- list()
  if (layered) for (e in c(items$edges, lapply(items$external, function(x) { x$to <- x$used_by; x$from <- x$ref; x }))) {
    if (nzchar(e$pc) && e$to %in% toupper(flat_chr(graph$layer_keys[[e$pc]]))) {
      ids <- extract_record_ids(graph, e$from, e$to, e$pc)
      via[[paste(ids, collapse = "|")]] <- unique(c(via[[paste(ids, collapse = "|")]], e$to))
    }
  }
  note <- function(from, to) { v <- via[[paste(from, to, sep = "|")]]; if (is.null(v)) "" else paste0(" (via key variable ", paste(sort(v), collapse = ", "), ")") }
  checks <- list(); lines <- character()
  add_check <- function(from, to) {
    id <- paste0("E", length(checks) + 1L)
    checks[[id]] <<- paste(from, to, sep = "|")
    lines <<- c(lines, paste0(id, " ", from, " -> ", to, note(from, to)))
  }
  for (e in g$edges) if (!identical(e$origin, "structural")) add_check(e$from, e$to)
  for (x in g$external) for (u in flat_chr(x$used_by)) add_check(x$ref, u)
  check <- extract_call(extract_strong_cfg(cfg), extract_check_prompt(dataset, spec_text, layered),
                        paste(c(if (length(lines)) lines else "(no edges)", catalog_lines), collapse = "\n"))
  if (is.null(check$cut)) stop("The check reply has no cut list, so the edges were not checked.", call. = FALSE)
  cut_ids <- flat_chr(check$cut)
  unknown_cuts <- setdiff(cut_ids, names(checks))
  items <- extract_remove(items, graph, unlist(checks[intersect(cut_ids, names(checks))], use.names = FALSE))
  categories <- as_records(check$categories)
  unresolved <- lapply(as_records(check$unresolved), function(iss) {
    layer <- if (length(layers) == 1L && !is.null(paramcd)) layers else extract_issue_layer(iss)
    if (nzchar(layer)) iss$paramcd <- layer
    iss
  })
  g <- merged(unresolved, categories)

  # Cycles: only cut, never add or reverse. What can't be cut stays in the graph and check_graph reports it.
  cycle_cuts <- list(); warnings <- list(); rounds <- 0L
  pairs_of <- function(graph_now) lapply(graph_now$edges, function(e) c(e$from, e$to))
  repeat {
    cycles <- extract_find_cycles(pairs_of(g))
    if (!length(cycles) || rounds >= EXTRACT_MAX_CYCLE_ROUNDS) break
    rounds <- rounds + 1L
    members <- unique(unlist(cycles))
    inside <- Filter(function(e) e$from %in% members && e$to %in% members, g$edges)
    text <- paste0("Cycle edges:\n", paste(vapply(inside, function(e) paste0(e$from, " -> ", e$to, if (identical(e$origin, "structural")) " (structural)" else ""), ""), collapse = "\n"))
    reply <- tryCatch(extract_call(extract_strong_cfg(cfg), extract_cycle_prompt(dataset, spec_text), text), error = function(e) e)
    if (inherits(reply, "error") || is.null(reply$cut)) {
      warnings <- c(warnings, list(paste0("The cycle check failed: ", if (inherits(reply, "error")) conditionMessage(reply) else "the reply has no cut list.")))
      break
    }
    allowed <- vapply(Filter(function(e) !identical(e$origin, "structural"), inside), function(e) paste(e$from, e$to, sep = "|"), "")
    removed <- character()
    for (c1 in as_records(reply$cut)) {
      key <- paste(c1$from %||% "", c1$to %||% "", sep = "|")
      if (key %in% allowed && !key %in% removed) {
        removed <- c(removed, key)
        cycle_cuts[[length(cycle_cuts) + 1L]] <- list(from = c1$from, to = c1$to, reason = field_txt(c1$reason %||% c1$quote), round = rounds)
      }
    }
    for (conflict in flat_chr(reply$conflict)) warnings <- c(warnings, list(paste0("The spec may contradict itself: ", conflict)))
    if (!length(removed)) break
    items <- extract_remove(items, graph, removed)
    g <- merged(unresolved, categories)
  }

  # The parameter graph stays the authority on links between parameters. Only report a gap here.
  param_uses <- lapply(names(items$uses), function(key) {
    parts <- strsplit(key, "|", fixed = TRUE)[[1]]
    list(from = parts[[1]], to = parts[[2]], uses = unique(items$uses[[key]]))
  })
  if (layered && !is.null(project) && exists("param_graph_read", mode = "function")) {
    pg <- tryCatch(param_graph_read(project, dataset), error = function(e) NULL)
    carry_codes <- if (is.null(classification)) character() else vapply(Filter(function(x) identical(x$classification,"carry_over"),classification$parameters),function(x)x$paramcd,"")
    if (!is.null(pg) && length(carry_codes)) for (edge in pg$edges %||% list()) {
      if (toupper(edge$to) %in% carry_codes) warnings <- c(warnings, list(paste0(
        "Parameter DAG issue: carry-over parameter ", toupper(edge$to), " has an incoming edge from ", toupper(edge$from), ".")))
    }
    if (!is.null(pg)) for (u in param_uses) {
      linked <- any(vapply(pg$edges %||% list(), function(e) identical(toupper(e$from), u$from) && identical(toupper(e$to), u$to), TRUE))
      if (!linked) warnings <- c(warnings, list(paste0("The ", u$to, " layer uses ", u$from, ", but the Parameter DAG has no ", u$from, " -> ", u$to, " link.")))
    }
  }
  if (length(unknown_cuts)) warnings <- c(warnings, list(paste0("The check named unknown edges, ignored: ", paste(unknown_cuts, collapse = ", "), ".")))
  extraction <- extract_to_parsed(items, graph, unresolved, categories)
  extraction$layer_keys <- graph$layer_keys %||% (if (!is.null(keys[["*"]])) keys)
  extraction$param_uses <- param_uses; extraction$context <- as.list(items$context); extraction$not_placed <- as.list(items$not_placed)
  extraction$parameter_classification <- classification
  g$extraction <- extraction
  g$param_uses <- param_uses
  g$warnings <- c(g$warnings %||% list(), warnings)
  g$cycle_cuts <- lapply(cycle_cuts, function(x) {
    layer <- extract_cut_layer(x)
    if (nzchar(layer)) x$paramcd <- layer
    x
  })
  g
}

# The parameter in a node id like ADVS.AVAL@SYSBP. A dataset-level node has none.
extract_node_layer <- function(id) {
  id <- as.character(id %||% "")[1]
  if (grepl("@", id, fixed = TRUE)) toupper(sub("^.*@", "", id)) else ""
}

extract_cut_layer <- function(cut) {
  tag <- toupper(as.character(cut$paramcd %||% "")[1])
  if (!is.na(tag) && nzchar(tag)) tag else extract_node_layer(cut$to)
}

extract_issue_layer <- function(iss) {
  layers <- unique(vapply(flat_chr(iss$variables), extract_node_layer, ""))
  if (length(layers) == 1L) layers else ""
}

# A regenerated layer replaces its own issues. Another layer's stay until that layer is regenerated.
# An issue with no layer stays too, unless the new run reports the same one.
extract_merge_issues <- function(old, new, paramcd, include_dataset_level = FALSE) {
  paramcd <- toupper(flat_chr(paramcd))
  new <- as_records(new)
  same <- function(iss) paste(iss$type %||% "", paste(sort(flat_chr(iss$variables)), collapse = ","), iss$message %||% "", sep = "|")
  seen <- vapply(new, same, "")
  keep <- Filter(function(iss) {
    layer <- toupper(as.character(iss$paramcd %||% "")[1])
    if (!is.na(layer) && nzchar(layer)) return(!layer %in% paramcd)
    !isTRUE(include_dataset_level) && !same(iss) %in% seen
  }, as_records(old))
  c(keep, new)
}

extract_merge_cuts <- function(old, new, paramcd, include_dataset_level = FALSE) {
  paramcd <- toupper(flat_chr(paramcd))
  keep <- Filter(function(cut) {
    layer <- extract_cut_layer(cut)
    if (nzchar(layer)) !layer %in% paramcd else !isTRUE(include_dataset_level)
  }, as_records(old))
  c(keep, as_records(new))
}

# Warnings are kept per layer. The graph's list is rebuilt from them, so a fixed one goes away.
extract_merge_warnings <- function(by_layer, new, paramcd) {
  by_layer <- if (is.list(by_layer)) by_layer else list()
  for (pc in toupper(flat_chr(paramcd))) by_layer[[pc]] <- as.list(unique(flat_chr(new)))
  by_layer
}

# Replace one layer's records in a stored extraction, so later key changes keep every generated layer.
extract_merge_replies <- function(old, new, paramcd, include_dataset_level) {
  if (is.null(old)) return(new)
  paramcd <- toupper(flat_chr(paramcd))
  stale <- function(rec) {
    params <- toupper(flat_chr(rec$params))
    if (length(params)) any(params %in% paramcd) else isTRUE(include_dataset_level)
  }
  out <- new
  out$edges <- c(Filter(Negate(stale), as_records(old$edges)), as_records(new$edges))
  out$external <- c(Filter(Negate(stale), as_records(old$external)), as_records(new$external))
  out$param_uses <- c(Filter(function(u) !toupper(u$to) %in% paramcd, as_records(old$param_uses)), as_records(new$param_uses))
  out$unresolved <- extract_merge_issues(old$unresolved, new$unresolved, paramcd, include_dataset_level)
  out$parameter_classification <- new$parameter_classification %||% old$parameter_classification
  out
}

# ---- fake extraction (DUALDAG_FAKE_LLM) ----

split_prompt_line <- function(line) {
  parts <- strsplit(line, "\t", fixed = TRUE)[[1]]
  if (length(parts) && identical(parts[[length(parts)]], "#")) parts <- parts[-length(parts)]
  parts
}

blank_field <- function(x) {
  x <- x %||% ""
  if (!nzchar(x) || identical(x, "-")) "" else x
}

extract_tokens <- function(text, pattern) {
  out <- list()
  pos <- 1L
  n <- nchar(text)
  while (pos <= n) {
    subtxt <- substr(text, pos, n)
    m <- regexec(pattern, subtxt, perl = TRUE)
    r <- regmatches(subtxt, m)[[1]]
    if (!length(r)) break
    start <- m[[1]][1]
    out[[length(out) + 1L]] <- r
    pos <- pos + start + nchar(r[[1]]) - 1L
    if (start < 1L) break
  }
  out
}

fake_extract_rows_of <- function(text) {
  m <- gregexpr("ROW ([A-Z][0-9]+) \\| (Variable|PARAMCD): ([^ ]+) \\|[^\n]*\nSource: [^\n]*\nDerivation: [^\n]*", text %||% "", perl = TRUE)
  blocks <- regmatches(text %||% "", m)[[1]]
  lapply(blocks, function(b) {
    lines <- strsplit(b, "\n", fixed = TRUE)[[1]]
    head <- regmatches(lines[[1]], regexec("^ROW ([A-Z][0-9]+) \\| (Variable|PARAMCD): ([^ ]+) \\|(.*)$", lines[[1]]))[[1]]
    key <- regmatches(head[[5]], regexec("Key: ([0-9]+)", head[[5]]))[[1]]
    row <- regmatches(head[[5]], regexec("Row: ([0-9]+)", head[[5]]))[[1]]
    list(id = head[[2]], kind = if (head[[3]] == "Variable") "var" else "param", target = head[[4]],
         key = if (length(key)) key[[2]] else "", row = if (length(row)) row[[2]] else "",
         source = sub("^Source: ", "", lines[[2]]), derivation = sub("^Derivation: ", "", lines[[3]]))
  })
}

fake_extract_classification_reply <- function(system, user) {
  line <- grep("^PARAMCD_LIST: ", strsplit(user %||% "", "\n", fixed=TRUE)[[1]], value=TRUE)
  codes <- if (length(line)) strsplit(sub("^PARAMCD_LIST: ", "", line[[1]]), ", ", fixed=TRUE)[[1]] else character()
  descriptions <- strsplit(user %||% "", "\n", fixed=TRUE)[[1]]
  rows <- lapply(codes, function(code) {
    description <- grep(paste0("^PARAM\\t", code, "\\t"), descriptions, value=TRUE)
    carry <- length(description) && grepl("CARRY_OVER", description[[1]], fixed=TRUE)
    list(paramcd=code, classification=if (carry) "carry_over" else "derived",
         reason=if (carry) "The supplied parameter description marks source carry-over." else "The supplied description does not establish carry-over status.")
  })
  json_text(list(parameters=rows))
}

fake_extract_plan_reply <- function(system, user) {
  rows <- fake_extract_rows_of(user)
  ids <- vapply(rows, function(r) r$id, "")
  groups <- split(ids, ceiling(seq_along(ids) / 15))
  json_text(list(batches = unname(lapply(groups, function(g) list(kind = "simple", rows = as.list(g))))))
}

fake_extract_rows_reply <- function(system, user) {
  if (grepl("CARRY_OVER_PARAMETERS:", user %||% "", fixed=TRUE)) {
    clean_user <- sub("^CARRY_OVER_PARAMETERS: [^\\n]*\\n\\n", "", user)
    one <- jsonlite::fromJSON(fake_extract_rows_reply(system, clean_user), simplifyVector=FALSE)
    if (identical(Sys.getenv("DUALDAG_FAKE_GROUP_PROSE_ONCE"), "1") &&
        !grepl("RETRY CORRECTION", user %||% "", fixed=TRUE)) {
      first <- names(one)[[1L]]
      one[[first]] <- list("derive using the carry-over rules")
    }
    return(json_text(one))
  }
  all_rows <- fake_extract_rows_of(system)
  var_names <- vapply(Filter(function(r) r$kind == "var", all_rows), function(r) r$target, "")
  param_names <- vapply(Filter(function(r) r$kind == "param", all_rows), function(r) r$target, "")
  owners <- list()
  for (line in grep("^CATALOG\t", strsplit(system %||% "", "\n", fixed = TRUE)[[1]], value = TRUE)) {
    p <- split_prompt_line(line)
    if (length(p) >= 3L) owners[[toupper(p[[2]])]] <- strsplit(toupper(p[[3]]), ",", fixed = TRUE)[[1]]
  }
  out <- list()
  for (r in fake_extract_rows_of(user)) {
    text <- toupper(paste(r$source, r$derivation))
    refs <- domain_var_refs(text)
    items <- vapply(refs, function(x) toupper(x$ref), "")
    if (r$kind == "param") items <- c(items, "PARAMCD")
    stripped <- text
    for (ref in items) stripped <- gsub(ref, " ", stripped, fixed = TRUE)
    for (name in c(var_names, if (r$kind == "param") param_names)) {
      if (!identical(name, r$target) && grepl(paste0("\\b", name, "\\b"), stripped, perl = TRUE)) items <- c(items, name)
    }
    for (word in unique(regmatches(stripped, gregexpr("\\b[A-Z][A-Z0-9_]*\\b", stripped, perl = TRUE))[[1]])) {
      domains <- names(owners)[vapply(owners, function(v) word %in% v, TRUE)]
      if (length(domains) == 1L && !word %in% var_names) items <- c(items, paste0(domains, ".", word))
    }
    out[[r$id]] <- as.list(unique(unname(items)))
  }
  # Private fake-server regressions can exercise the provider's observed
  # ROW-prefixed response shape without changing fake behavior for other runs.
  if (identical(Sys.getenv("DUALDAG_FAKE_ROW_PREFIX"), "1"))
    names(out) <- paste("ROW", names(out))
  json_text(out)
}

fake_extract_check_reply <- function(system, user) {
  dataset <- regmatches(system, regexec("DATASET: ([^\n]+)", system))[[1]][[2]]
  rows <- fake_extract_rows_of(system)
  lines <- paste0("DATASET\t", dataset, "\t#")
  for (r in Filter(function(x) x$kind == "var", rows)) lines <- c(lines, prompt_line("NODE", paste0(dataset, ".", r$target), r$target, "-", r$row, if (nzchar(r$key)) r$key else "-", r$source, r$derivation))
  for (r in Filter(function(x) x$kind == "param", rows)) lines <- c(lines, prompt_line("PARAM", r$target, r$row, r$source, r$derivation))
  catalog <- grep("^CATALOG\t", strsplit(user %||% "", "\n", fixed = TRUE)[[1]], value = TRUE)
  old <- jsonlite::fromJSON(fake_extract_reply(system, paste(c(lines, catalog), collapse = "\n")), simplifyVector = FALSE)
  json_text(list(cut = list(), unresolved = old$unresolved %||% list(), categories = old$categories %||% list()))
}

fake_extract_cycles_reply <- function(system, user) json_text(list(cut = list(), conflict = list()))

fake_extract_reply <- function(system, user) {
  if (grepl("VARIABLES\n",user %||% "",fixed=TRUE) && grepl("\nVAR\t",user %||% "",fixed=TRUE)) {
    dataset <- ""; vars <- list()
    for (line in strsplit(user %||% "","\n",fixed=TRUE)[[1]]) {
      if (!grepl("\t",line,fixed=TRUE)) next
      p <- split_prompt_line(line); type <- p[[1]]
      if (type=="DATASET" && length(p)>=2L) dataset <- toupper(p[[2]])
      if (type=="VAR" && length(p)>=5L) vars[[length(vars)+1L]] <- list(
        var=toupper(p[[2]]), key=blank_field(p[[3]]), source=p[[4]], derivation=p[[5]])
    }
    edges <- list(); edge_keys <- character(); external <- list(); ext_at <- list()
    add_edge <- function(from,to,evidence) {
      if (!nzchar(from) || !nzchar(to) || identical(from,to)) return()
      key <- paste(from,to,sep="\t"); if (key %in% edge_keys) return()
      edge_keys <<- c(edge_keys,key); edges[[length(edges)+1L]] <<- list(from=from,to=to,evidence=evidence)
    }
    add_external <- function(ref,target,evidence) {
      dom <- sub("\\..*$","",ref); v <- sub("^[^.]*\\.","",ref)
      if (identical(dom,dataset)) return()
      if (is.null(ext_at[[ref]])) {
        external[[length(external)+1L]] <<- list(ref=ref,domain=dom,var=v,used_by=list(target),evidence=evidence)
        ext_at[[ref]] <<- length(external)
      } else {
        i <- ext_at[[ref]]; external[[i]]$used_by <<- unique(c(flat_chr(external[[i]]$used_by),target))
      }
    }
    for (target in vars) for (field in c("source","derivation")) {
      evidence <- target[[field]] %||% ""; if (!nzchar(trimws(evidence))) next
      refs <- domain_var_refs(evidence)
      for (ref in refs) {
        if (identical(ref$dom,dataset)) add_edge(ref$var,target$var,evidence)
        else add_external(ref$ref,target$var,evidence)
      }
      bare <- toupper(evidence)
      for (other in vars) if (other$var != target$var && grepl(paste0("\\b",other$var,"\\b"),bare,perl=TRUE) &&
          !any(vapply(refs,function(ref) ref$var==other$var,TRUE))) add_edge(other$var,target$var,evidence)
    }
    return(json_text(list(edges=edges,external=external,unresolved=list(),categories=list())))
  }
  nodes <- list()
  params <- list()
  catalog <- list()
  key_rows <- list()
  dataset <- ""
  for (line in strsplit(user %||% "", "\n", fixed = TRUE)[[1]]) {
    if (!nzchar(line) || !grepl("\t", line, fixed = TRUE)) next
    p <- split_prompt_line(line)
    kind <- p[[1]]
    if (identical(kind, "DATASET") && length(p) >= 2L) dataset <- toupper(p[[2]])
    if (identical(kind, "NODE") && length(p) >= 3L) {
      nodes[[length(nodes) + 1L]] <- list(
        id = p[[2]],
        var = toupper(p[[3]]),
        paramcd = blank_field(if (length(p) >= 4L) p[[4]] else ""),
        row = if (length(p) >= 5L) blank_field(p[[5]]) else "",
        key = if (length(p) >= 6L) blank_field(p[[6]]) else "",
        source = if (length(p) >= 7L) p[[7]] else "",
        derivation = if (length(p) >= 8L) p[[8]] else ""
      )
    }
    if (identical(kind,"KEY") && length(p)>=5L) key_rows[[length(key_rows)+1L]]<-list(
      var=toupper(p[[2]]),key=blank_field(p[[3]]),source=p[[4]],derivation=p[[5]])
    if (identical(kind, "PARAM") && length(p) >= 2L) {
      params[[length(params) + 1L]] <- list(
        paramcd = toupper(p[[2]]),
        row = if (length(p) >= 3L) blank_field(p[[3]]) else "",
        source = if (length(p) >= 4L) p[[4]] else "",
        derivation = if (length(p) >= 5L) p[[5]] else ""
      )
    }
    if (identical(kind, "CATALOG") && length(p) >= 2L) {
      vars <- strsplit(if (length(p) >= 3L) toupper(p[[3]]) else "", ",", fixed = TRUE)[[1]]
      catalog[[toupper(p[[2]])]] <- unique(vars[nzchar(vars)])
    }
  }
  keys <- unique(c(vapply(Filter(function(n) nzchar(n$key), nodes), function(n) n$var, ""),
    vapply(key_rows,function(n)n$var,"")))
  node_var <- function(id) {
    for (n in nodes) if (identical(n$id, id)) return(n$var)
    ""
  }
  resolve_ids <- function(var, paramcd) {
    hits <- Filter(function(n) identical(n$var, var), nodes)
    if (!length(hits)) return(character())
    if (nzchar(paramcd %||% "")) {
      same <- Filter(function(n) identical(n$paramcd, paramcd), hits)
      if (length(same)) return(vapply(same, function(n) n$id, ""))
    }
    plain <- Filter(function(n) !nzchar(n$paramcd %||% ""), hits)
    if (length(plain)) return(vapply(plain, function(n) n$id, ""))
    vapply(hits, function(n) n$id, "")
  }
  owners_of <- function(var) {
    ds <- character()
    if (any(vapply(nodes, function(n) identical(n$var, var), logical(1)))) ds <- c(ds, dataset)
    for (nm in names(catalog)) if (var %in% catalog[[nm]]) ds <- c(ds, nm)
    unique(ds)
  }
  edges <- list()
  edge_key <- character()
  add_edge <- function(from, to, evidence) {
    if (!nzchar(from) || !nzchar(to) || identical(from, to)) return()
    key <- paste(from, to, sep = "\t")
    if (key %in% edge_key) return()
    edge_key <<- c(edge_key, key)
    edges[[length(edges) + 1L]] <<- list(from = from, to = to, evidence = substr(evidence, 1L, 180L))
  }
  external <- list()
  ext_i <- list()
  add_ext <- function(dom, var, used_by, evidence) {
    ref <- paste0(dom, ".", var)
    if (is.null(ext_i[[ref]])) {
      external[[length(external) + 1L]] <<- list(
        ref = ref, domain = dom, var = var, used_by = used_by, evidence = substr(evidence, 1L, 180L))
      ext_i[[ref]] <<- length(external)
    } else {
      i <- ext_i[[ref]]
      external[[i]]$used_by <<- unique(c(external[[i]]$used_by, used_by))
    }
  }
  issues <- list()
  add_issue <- function(type, variables, row, text, message, suggestion, file = "variable.csv") {
    issues[[length(issues) + 1L]] <<- list(
      type = type, dataset = dataset, variables = as.list(unique(variables)),
      file = file, row = if (nzchar(row)) as.integer(row) else NA,
      text = text, message = message, suggestion = suggestion)
  }
  seen_ambig <- character()

  scan_text <- function(text, target_ids, row, file) {
    raw <- text %||% ""
    up <- toupper(raw)
    if (!nzchar(trimws(up))) return(list(linked = FALSE))
    linked <- FALSE
    doms <- extract_tokens(up, "\\b([A-Z][A-Z0-9]*)\\.([A-Z][A-Z0-9]*)\\b")
    stripped <- up
    for (m in doms) {
      stripped <- sub(m[[1]], " ", stripped, fixed = TRUE)
      dom <- m[[2]]
      var <- m[[3]]
      if (identical(dom, dataset)) {
        if (var %in% keys) { linked <- TRUE; next }
        ids <- resolve_ids(var, "")
        # Prefer each target's own paramcd when the name is split by parameter.
        if (!length(ids)) {
          add_issue("missing_ref", c(paste0(dom, ".", var), target_ids), row, raw,
                    paste(dom, var, "is not a variable in", dataset, sep = "."),
                    "Check the variable name in the specification.", file)
          linked <- TRUE
        } else {
          for (tid in target_ids) {
            pc <- ""
            for (n in nodes) if (identical(n$id, tid)) pc <- n$paramcd %||% ""
            use <- resolve_ids(var, pc)
            if (!length(use)) use <- ids
            for (from in use) add_edge(from, tid, raw)
          }
          linked <- TRUE
        }
      } else {
        known <- catalog[[dom]]
        if (!is.null(known) && !(var %in% known)) {
          add_issue("missing_var", c(paste0(dom, ".", var), target_ids), row, raw,
                    paste(paste0(dom, ".", var), "is not a column in the uploaded", dom, "data"),
                    "Check the variable name against the xpt.", paste0(tolower(dom), ".xpt"))
        } else {
          for (tid in target_ids) add_ext(dom, var, tid, raw)
        }
        linked <- TRUE
      }
    }
    bare <- unique(extract_tokens(stripped, "\\b([A-Z][A-Z0-9_]*)\\b"))
    for (m in bare) {
      var <- m[[1]]
      if (!nzchar(var)) next
      if (!length(target_ids)) next
      if (all(vapply(target_ids, function(id) identical(node_var(id), var), logical(1)))) next
      owners <- owners_of(var)
      if (!length(owners)) next
      # owners is a character vector; compare the single element, not the vector object.
      internal <- var %in% keys || (length(owners) == 1L && identical(owners[[1]], dataset))
      outside <- setdiff(owners, dataset)
      if (internal) {
        for (tid in target_ids) {
          pc <- ""
          for (n in nodes) if (identical(n$id, tid)) pc <- n$paramcd %||% ""
          for (from in resolve_ids(var, pc)) add_edge(from, tid, raw)
        }
        linked <- TRUE
      } else if (length(outside) == 1L && !(dataset %in% owners)) {
        # A bare name that exists in exactly one uploaded domain is that column.
        for (tid in target_ids) add_ext(outside[[1]], var, tid, raw)
        linked <- TRUE
      } else if (!(var %in% seen_ambig)) {
        seen_ambig <<- c(seen_ambig, var)
        add_issue("ambiguous", c(var, target_ids), row, raw,
                  paste(var, "does not name a dataset, and it exists in", paste(owners, collapse = ", ")),
                  paste0("Qualify the dataset, for example ", owners[[1]], ".", var), file)
        linked <- TRUE
      } else {
        linked <- TRUE
      }
    }
    list(linked = linked, text = raw)
  }

  # Group node ids by variable so unresolvable is one issue per variable.
  by_var <- if (length(nodes)) {
    split(vapply(nodes, function(n) n$id, ""), vapply(nodes, function(n) n$var, ""))
  } else list()
  touched <- character()
  for (n in nodes) {
    text <- paste(n$source, n$derivation)
    # Scan each node with its own id so param copies bind to the same PARAMCD.
    res <- scan_text(text, n$id, n$row, "variable.csv")
    if (isTRUE(res$linked)) touched <- c(touched, n$var)
  }
  for (var in names(by_var)) {
    sample <- Filter(function(n) identical(n$var, var), nodes)[[1]]
    text <- paste(sample$source, sample$derivation)
    if (!grepl("[A-Za-z]", text)) next
    if (var %in% touched) next
    add_issue("unresolvable", c(var, by_var[[var]]), sample$row, trimws(text),
              paste("The derivation for", var, "does not name a variable that can be resolved."),
              "Name the source as DATASET.VARIABLE or a variable in this dataset.",
              "variable.csv")
  }
  for (pr in params) {
    text <- paste(pr$source, pr$derivation)
    up <- toupper(text)
    member_ids <- vapply(Filter(function(n) identical(n$paramcd, pr$paramcd), nodes), function(n) n$id, "")
    if (!length(member_ids)) next
    res <- scan_text(text, member_ids, pr$row, "parameter.csv")
    # Parameter group depends on another parameter through the shared variables.
    for (other in setdiff(vapply(params, function(x) x$paramcd, ""), pr$paramcd)) {
      if (!grepl(paste0("\\b", other, "\\b"), up, perl = TRUE)) next
      mine <- Filter(function(n) identical(n$paramcd, pr$paramcd), nodes)
      for (n in mine) {
        froms <- resolve_ids(n$var, other)
        for (from in froms) add_edge(from, n$id, text)
      }
    }
  }
  records_nodes<-Filter(function(n)identical(n$var,"RECORDS")&&!nzchar(n$paramcd %||% ""),nodes)
  if(length(records_nodes)) {
    rid<-records_nodes[[1]]$id
    for(n in nodes)if(!identical(n$id,rid))add_edge(rid,n$id,"All variables are defined on the compound key rows.")
    for(k in key_rows)for(ref in domain_var_refs(k$source))if(!identical(toupper(ref$dom),dataset))
      add_ext(toupper(ref$dom),toupper(ref$var),rid,k$source)
  }
  categories <- list()
  for (n in nodes) for (cat in parse_spec_categories(n$derivation))
    categories[[length(categories) + 1L]] <- list(node = n$id, value = cat$value, rule = cat$rule)
  for (pr in params) for (cat in parse_spec_categories(pr$derivation))
    for (n in nodes) if (identical(n$paramcd, pr$paramcd))
      categories[[length(categories) + 1L]] <- list(node = n$id, value = cat$value, rule = cat$rule)
  json_text(list(edges = edges, external = external, unresolved = issues, categories = categories))
}

text_block <- function(text, begin, end) {
  m <- regexec(paste0(begin, "\\n([\\s\\S]*)\\n", end), text %||% "", perl = TRUE)
  r <- regmatches(text %||% "", m)[[1]]
  if (length(r) < 2L) "" else r[[2]]
}
