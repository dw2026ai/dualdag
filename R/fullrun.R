# Whole-dataset execution, deliberately isolated from normal node runs.

fullrun_dir <- function(project, dataset, side) {
  file.path(project_dir_of(project), ".dualdag", "fullrun", safe_ds(dataset), side)
}

fullrun_code <- function(project, dataset, id, side) {
  read_text(file.path(node_run_dir(project, dataset, id), side, "code.R"))
}

fullrun_steps <- function(project, dataset, side, plan = NULL) {
  plan <- plan %||% fullrun_plan(project, dataset, side)
  codes <- setNames(lapply(plan$ids, function(id) fullrun_code(project, dataset, id, side)), plan$ids)
  keep <- vapply(codes, function(code) length(code) == 1L && !is.na(code) && nzchar(trimws(code)), TRUE)
  lapply(plan$ids[keep], function(id) list(id = id, code = codes[[id]]))
}

fullrun_plan <- function(project, dataset, side, graph = NULL, spec = NULL) {
  graph <- graph %||% load_project_graph(project, dataset)
  spec <- spec %||% load_project_spec(project)
  ids <- dataset_node_ids(project, spec, dataset)
  topo <- if (!is.null(graph)) inspect_topo(graph) else character()
  key_ids <- paste0(toupper(dataset), ".", dataset_keys(spec, dataset))
  records_id<-paste0(toupper(dataset),".RECORDS")
  multi_key_records<-!is.null(graph)&&!isTRUE(graph$layered)&&any(vapply(graph$nodes %||% list(),function(n)
    identical(n$id,records_id)&&identical(n$kind,"records")&&is.null(n$paramcd),TRUE))
  if(multi_key_records)key_ids<-records_id
  if (!is.null(graph) && isTRUE(graph$layered) && exists("param_graph_read", mode="function")) {
    pg <- tryCatch(param_graph_read(project, dataset), error=function(e) NULL)
    po <- if (!is.null(pg)) param_layer_order(spec, dataset, pg) else list(order=character(),cycle=character())
    if (length(po$cycle)) stop("Parameter dependency cycle: ", paste(po$cycle, collapse=" -> "), call.=FALSE)
    dataset_ids <- vapply(Filter(function(n) identical(n$kind,"dataset_variable"),graph$nodes), `[[`, "", "id")
    layers <- unlist(lapply(po$order,function(pc) {
      members <- vapply(Filter(function(n) identical(n$paramcd,pc) && identical(n$kind,"param_variable"),graph$nodes), `[[`, "", "id")
      c(paste0(toupper(dataset),".RECORDS@",pc), topo[topo %in% members], setdiff(members,topo))
    }),use.names=FALSE)
    ids <- unique(c(key_ids, dataset_ids, layers, topo, ids))
  } else ids <- unique(c(key_ids, topo, ids))
  ids <- ids[ids %in% dataset_node_ids(project, spec, dataset)]
  layered <- !is.null(graph) && isTRUE(graph$layered)
  plan <- list(ids = ids, keys = spec_key_names(spec, dataset), graph = graph, spec = spec, layered=layered)
  if(multi_key_records){plan$multi_key_records<-TRUE;plan$records_id<-records_id;plan$spec_order<-toupper(spec_vars(spec,dataset)$Variable)}
  if (layered) {
    pg <- tryCatch(param_graph_read(project,dataset),error=function(e)NULL)
    po <- if(!is.null(pg))param_layer_order(spec,dataset,pg)else list(order=character(),cycle=character())
    if(length(po$cycle))stop("Parameter dependency cycle: ",paste(po$cycle,collapse=" -> "),call.=FALSE)
    topo <- inspect_topo(graph)
    dataset_nodes <- vapply(Filter(function(n)identical(n$kind,"dataset_variable"),graph$nodes),`[[`,"","id")
    plan$dataset_ids <- c(topo[topo%in%dataset_nodes],setdiff(dataset_nodes,topo))
    plan$layer_order <- po$order
    plan$layers <- lapply(po$order,function(pc){
      members <- vapply(Filter(function(n)identical(n$paramcd,pc)&&identical(n$kind,"param_variable"),graph$nodes),`[[`,"","id")
      layer_record <- Filter(function(x) identical(toupper(x$paramcd %||% ""),toupper(pc)), graph$layers %||% list())
      allowed <- if(!is.null(pg)) param_layer_visible_columns(spec,dataset,pg,pc) else list()
      carries <- toupper(flat_chr(pg$carry_members %||% character()))
      carry_reads <- unlist(allowed[intersect(names(allowed),carries)],use.names=FALSE)
      allowed[intersect(names(allowed),carries)] <- NULL
      if(length(carry_reads)) allowed$CARRY_OVER <- unique(toupper(carry_reads))
      parents <- unique(vapply(Filter(function(e)toupper(e$to)==pc,pg$edges%||%list()),function(e)toupper(e$from),""))
      parents[parents %in% carries] <- "CARRY_OVER"
      list(paramcd=pc,records=paste0(toupper(dataset),".RECORDS@",pc),members=c(topo[topo%in%members],setdiff(members,topo)),
           parents=unique(parents),
           parameters=if(length(layer_record)) flat_chr(layer_record[[1]]$members_parameters) else pc,
           allowed=allowed)
    })
    names(plan$layers)<-po$order
    plan$spec_order <- toupper(spec_vars(spec,dataset)$Variable)
  }
  plan
}

fullrun_layered_program_text <- function(project,dataset,side,plan,packages,codes,hashes) {
  data_dir <- normalizePath(file.path(project_dir_of(project),"data"),winslash="/",mustWork=FALSE)
  literal <- function(x) paste(deparse(x,control="all",width.cutoff=500L),collapse="\n")
  code_literal <- function(code) {
    lines <- strsplit(code %||% "","\n",fixed=TRUE)[[1]]; if(!length(lines))lines<-""
    paste0("c(",paste(vapply(lines,literal,""),collapse=","),")")
  }
  ids <- plan$ids; names(codes)<-ids
  subject_keys <- intersect("USUBJID",plan$keys)
  if(!length(subject_keys))subject_keys<-setdiff(plan$keys,"PARAMCD")
  layer_keys <- plan$keys
  meta <- lapply(plan$layers,function(x)list(records=x$records,members=as.list(x$members),parents=as.list(x$parents),parameters=as.list(x$parameters %||% x$paramcd),allowed=x$allowed))
  names(meta)<-names(plan$layers)
  dataset_steps <- unlist(lapply(plan$dataset_ids,function(id)c(
    paste0(".fullrun_dataset_step(",literal(id),",",code_literal(codes[[id]]),")"))),use.names=FALSE)
  layer_steps <- character()
  for(pc in names(plan$layers)) {
    layer <- plan$layers[[pc]]
    layer_steps <- c(layer_steps,paste0(".fullrun_layer_step(",literal(pc),")"))
  }
  result_path<-file.path(fullrun_dir(project_dir_of(project),dataset,side),"result.rds")
  runner_path<-file.path(fullrun_dir(project_dir_of(project),dataset,side),"runner.json")
  log_path<-file.path(fullrun_dir(project_dir_of(project),dataset,side),"log.txt")
  lines<-c(
    "# Generated Dualdag layered full-program run. This script is standalone.",
    paste0(".fullrun_dataset <- ",literal(dataset)),paste0(".fullrun_side <- ",literal(side)),
    paste0(".fullrun_keys <- ",literal(layer_keys)),paste0(".fullrun_subject_keys <- ",literal(subject_keys)),
    paste0(".fullrun_hashes <- ",literal(as.list(hashes))),paste0(".fullrun_codes <- ",literal(codes)),
    paste0(".fullrun_dataset_ids <- ",literal(as.list(plan$dataset_ids))),paste0(".fullrun_layers_meta <- ",literal(meta)),
    paste0(".fullrun_spec_order <- ",literal(plan$spec_order)),paste0(".fullrun_data_dir <- ",literal(data_dir)),
    paste0(".fullrun_result_path <- ",literal(result_path)),paste0(".fullrun_runner_path <- ",literal(runner_path)),
    paste0(".fullrun_log_path <- ",literal(log_path)),paste0(".fullrun_packages <- ",literal(as.character(packages))),
    ".file_arg <- grep('^--file=',commandArgs(FALSE),value=TRUE)",
    "if(length(.file_arg))setwd(dirname(normalizePath(sub('^--file=','',.file_arg[[1]]),winslash='/',mustWork=TRUE)))",
    ".logcon<-file(.fullrun_log_path,open='wt',encoding='UTF-8');sink(.logcon);sink(.logcon,type='message')",
    "for(.pkg in .fullrun_packages)suppressPackageStartupMessages(library(.pkg,character.only=TRUE))",
    ".src<-list(); if(dir.exists(.fullrun_data_dir))for(.f in list.files(.fullrun_data_dir,pattern='\\\\.[Xx][Pp][Tt]$',full.names=TRUE)){.nm<-toupper(tools::file_path_sans_ext(basename(.f)));if(!(.nm%in%names(.src))){.d<-as.data.frame(haven::read_xpt(.f),stringsAsFactors=FALSE);rownames(.d)<-NULL;.src[[.nm]]<-.d}}",
    ".completed<-character();.skipped<-character();.not_run<-list();.failed_variable<-NULL;.err<-NULL;.failed_layers<-character();.dataset_work<-NULL;.layers<-list();.final_layers<-list();.out<-data.frame()",
    ".fullrun_ids<-names(.fullrun_codes);.fullrun_targets<-setNames(as.list(vapply(.fullrun_ids,function(.id)sub('^.*\\\\.','',sub('@.*$','',.id)),'')),.fullrun_ids)",
    ".fullrun_pick_output <- function(.env,.candidates,.target,.inherited) { if (exists('result',.env,inherits=FALSE)) return('result'); .candidates <- unique(.candidates); .frames <- .candidates[vapply(.candidates,function(.n) exists(.n,.env,inherits=FALSE) && is.data.frame(get(.n,.env,inherits=FALSE)),TRUE)]; .matching <- .frames[vapply(.frames,function(.n) .target %in% names(get(.n,.env,inherits=FALSE)),TRUE)]; if(length(.matching)) return(.matching[[1L]]); .changed <- .frames[!vapply(.frames,function(.n) identical(get(.n,.env,inherits=FALSE),.inherited),TRUE)]; if(length(.changed)) return(.changed[[1L]]); if(length(.frames)) return(.frames[[1L]]); .candidates[[1L]] }",
    ".fullrun_exec<-function(.id,.code,.work,.layer_list,.keys){.code<-paste(as.character(.code),collapse='\\n');if(!nzchar(trimws(.code)))return(list(skipped='no code'));.pc<-if(grepl('@',.id,fixed=TRUE))gsub('[^A-Za-z0-9_.]','_',toupper(sub('^.*@','',.id)))else NULL;.temp_name<-if(is.null(.pc))paste0(toupper(.fullrun_dataset),'_TEMP')else paste0(toupper(.fullrun_dataset),'_',.pc,'_TEMP');.lineage_name<-if(is.null(.pc))paste0('LINEAGE_',toupper(.fullrun_dataset))else paste0('LINEAGE_',.pc);.env<-new.env(parent=globalenv());.env$src<-.src;.env$work<-.work;.env$upstream<-.work;.env$keys<-.keys;.env$layers<-.layer_list;.env[[.temp_name]]<-.work;if(identical(.fullrun_side,'primary')){.env[[.lineage_name]]<-.work;for(.dom in names(.src)).env[[paste0('LINEAGE_SRC_',toupper(.dom))]]<-.src[[.dom]]};if(length(.layer_list))for(.up in names(.layer_list)){.safe<-gsub('[^A-Za-z0-9_.]','_',toupper(.up));.env[[paste0(toupper(.fullrun_dataset),'_',.safe,'_TEMP')]]<-.layer_list[[.up]];if(identical(.fullrun_side,'primary')).env[[paste0('LINEAGE_',.safe)]]<-.layer_list[[.up]]};.failure<-tryCatch({.exprs<-parse(text=.code,keep.source=TRUE);source(exprs=.exprs,local=.env,echo=FALSE,print.eval=TRUE,keep.source=TRUE);.name<-.fullrun_pick_output(.env,c(.lineage_name,.temp_name),if(identical(.fullrun_targets[[.id]],'RECORDS'))'PARAMCD'else .fullrun_targets[[.id]],.work);if(!exists(.name,.env,inherits=FALSE))stop(paste('Code must update',.lineage_name));.r<-get(.name,.env,inherits=FALSE);if(!is.data.frame(.r))stop(paste(.name,'must be a data.frame.'));as.data.frame(.r,stringsAsFactors=FALSE)},error=function(e)e);if(inherits(.failure,'error'))list(error=conditionMessage(.failure))else list(value=.failure)}",
    ".fullrun_merge<-function(.base,.piece,.by){if(is.null(.base))return(.piece);if(!all(.by%in%names(.piece)))stop('Result is missing merge keys.');.m<-merge(.base,.piece,by=.by,all.x=TRUE,sort=FALSE);if(nrow(.m)!=nrow(.base))stop('Result would change the dataset row count.');.m}",
    ".fullrun_dataset_step<-function(.id,.code){cat('# ==== ',.id,' ====\\n',sep='');.deps<-.fullrun_deps[[.id]];.bad<-intersect(.deps,.skipped);if(length(.bad)){.skipped<<-c(.skipped,.id);.not_run[[.id]]<<-paste0('upstream ',.bad[[1]],' not run');return(invisible(FALSE))};.r<-.fullrun_exec(.id,.code,.dataset_work,list(),.fullrun_subject_keys);if(!is.null(.r$skipped)){.skipped<<-c(.skipped,.id);.not_run[[.id]]<<-'no code';return(invisible(FALSE))};if(!is.null(.r$error)){.failed_variable<<-.id;.err<<-.r$error;.skipped<<-c(.skipped,.id);.not_run[[.id]]<<-'failed';return(invisible(FALSE))};.d<-.r$value;if(!all(c(.fullrun_subject_keys,.fullrun_targets[[.id]])%in%names(.d))){.failed_variable<<-.id;.err<<-'Dataset-level result is missing USUBJID or target.';return(invisible(FALSE))};.piece<-.d[,unique(c(.fullrun_subject_keys,.fullrun_targets[[.id]])),drop=FALSE];if(identical(.fullrun_targets[[.id]],'USUBJID')&&anyDuplicated(.piece$USUBJID)){.failed_variable<<-.id;.err<<-'Dataset-level result contains duplicate USUBJID values.';return(invisible(FALSE))};.dataset_work<<-.fullrun_merge(.dataset_work,.piece,.fullrun_subject_keys);.completed<<-c(.completed,.id);invisible(TRUE)}",
    ".fullrun_layer_step<-function(.pc){.meta<-.fullrun_layers_meta[[.pc]];if(any(unlist(.meta$parents)%in%.failed_layers)){.parent<-intersect(unlist(.meta$parents),.failed_layers)[[1]];.failed_layers<<-c(.failed_layers,.pc);.skipped<<-c(.skipped,paste0('layer:',.pc));.not_run[[paste0('layer:',.pc)]]<<-paste0('upstream layer ',.parent,' not run');for(.miss in c(.meta$records,unlist(.meta$members))).not_run[[.miss]]<<-paste0('upstream layer ',.parent,' not run');cat('Skipped layer ',.pc,': upstream layer ',.parent,' not run.\\n',sep='');return(invisible(FALSE))};.rid<-.meta$records;.record_layers<-.layers;if(identical(.fullrun_side,'primary')){.record_layers<-list();for(.up in names(.meta$allowed))if(!is.null(.layers[[.up]])){.cols<-unique(c(.fullrun_keys,unlist(.meta$allowed[[.up]],use.names=FALSE)));.record_layers[[.up]]<-if(identical(.up,'CARRY_OVER')) .layers[[.up]] else .layers[[.up]][,intersect(.cols,names(.layers[[.up]])),drop=FALSE]}};.rdeps<-.fullrun_deps[[.rid]];.bad<-intersect(.rdeps,.skipped);if(length(.bad)){.skipped<<-c(.skipped,.rid,paste0('layer:',.pc));.not_run[[.rid]]<<-paste0('upstream ',.bad[[1]],' not run');.not_run[[paste0('layer:',.pc)]]<<-paste0('upstream ',.bad[[1]],' not run');for(.miss in unlist(.meta$members)).not_run[[.miss]]<<-paste0('upstream ',.bad[[1]],' not run');.failed_layers<<-c(.failed_layers,.pc);return(invisible(FALSE))};.rr<-.fullrun_exec(.rid,.fullrun_codes[[.rid]],data.frame(),.record_layers,.fullrun_keys);if(!is.null(.rr$skipped)){.skipped<<-c(.skipped,.rid,paste0('layer:',.pc));.not_run[[.rid]]<<-'no code';.not_run[[paste0('layer:',.pc)]]<<-paste0('no records code for layer ',.pc);for(.miss in unlist(.meta$members)).not_run[[.miss]]<<-paste0('no records code for layer ',.pc);.failed_layers<<-c(.failed_layers,.pc);return(invisible(FALSE))};if(!is.null(.rr$error)){.failed_variable<<-.rid;.err<<-.rr$error;.not_run[[paste0('layer:',.pc)]]<<-paste0('records program failed for layer ',.pc);for(.miss in unlist(.meta$members)).not_run[[.miss]]<<-paste0('upstream ',.rid,' not run');.failed_layers<<-c(.failed_layers,.pc);return(invisible(FALSE))};.records<-.rr$value;if(!all(c(.fullrun_keys,'PARAMCD')%in%names(.records))){.failed_variable<<-.rid;.err<<-'Records result is missing key columns.';.failed_layers<<-c(.failed_layers,.pc);return(invisible(FALSE))};.allowed_paramcd<-.meta$parameters;if(is.null(.allowed_paramcd)||!length(unlist(.allowed_paramcd))).allowed_paramcd<-.pc;if(any(is.na(.records$PARAMCD)|!toupper(as.character(.records$PARAMCD))%in%toupper(unlist(.allowed_paramcd)))){.failed_variable<<-.rid;.err<<-paste('Records PARAMCD must be one of',paste(unlist(.allowed_paramcd),collapse=', '));.failed_layers<<-c(.failed_layers,.pc);return(invisible(FALSE))};if(anyDuplicated(.records[,unique(c(.fullrun_keys,'PARAMCD')),drop=FALSE])){.failed_variable<<-.rid;.err<<-'Records result contains duplicate keys.';.failed_layers<<-c(.failed_layers,.pc);return(invisible(FALSE))};.work<-.records;if(!is.null(.dataset_work)).work<-.fullrun_merge(.work,.dataset_work,.fullrun_subject_keys);.completed<<-c(.completed,.rid);.local_failed<-character();for(.id in unlist(.meta$members)){.deps<-.fullrun_deps[[.id]];.bad<-intersect(.deps,c(.local_failed,.skipped));if(length(.bad)){.skipped<<-c(.skipped,.id);.not_run[[.id]]<<-paste0('upstream ',.bad[[1]],' not run');.local_failed<-c(.local_failed,.id);next};.visible<-.layers;if(identical(.fullrun_side,'primary')){.allow<-.meta$allowed;.visible<-list();for(.up in names(.allow))if(!is.null(.layers[[.up]])){.cols<-unique(c(.fullrun_keys,unlist(.allow[[.up]],use.names=FALSE)));.visible[[.up]]<-if(identical(.up,'CARRY_OVER')) .layers[[.up]] else .layers[[.up]][,intersect(.cols,names(.layers[[.up]])),drop=FALSE]}};.res<-.fullrun_exec(.id,.fullrun_codes[[.id]],.work,.visible,.fullrun_keys);if(!is.null(.res$skipped)){.skipped<<-c(.skipped,.id);.not_run[[.id]]<<-'no code';.local_failed<-c(.local_failed,.id);next};if(!is.null(.res$error)){.failed_variable<<-.id;.err<<-.res$error;.local_failed<-c(.local_failed,.id);next};.v<-sub('^.*\\\\.','',sub('@.*$','',.id));.piece<-.res$value;if(!all(.fullrun_keys%in%names(.piece))||!.v%in%names(.piece)){.failed_variable<<-.id;.err<<-'Layer variable result is missing keys or target.';.local_failed<-c(.local_failed,.id);next};.work<-.fullrun_merge(.work,.piece[,unique(c(.fullrun_keys,.v)),drop=FALSE],.fullrun_keys);.completed<<-c(.completed,.id)};if(length(setdiff(.local_failed,names(.not_run)))){.badid<-setdiff(.local_failed,names(.not_run))[[1]];.not_run[[paste0('layer:',.pc)]]<<-paste0('layer failed at ',.badid);for(.miss in unlist(.meta$members))if(!.miss%in%names(.not_run)).not_run[[.miss]]<<-if(identical(.miss,.badid))'failed'else paste0('upstream ',.badid,' not run');.completed<<-setdiff(.completed,c(.rid,unlist(.meta$members)));.failed_layers<<-c(.failed_layers,.pc);return(invisible(FALSE))};.layers[[.pc]]<<-.work;.final_layers[[.pc]]<<-.work;invisible(TRUE)}",
    ".fullrun_deps<-",literal(setNames(lapply(ids,function(id)upstream_ids(plan$graph,id)),ids)),
    dataset_steps,layer_steps,
    ".fullrun_bind<-function(.xs){.all<-unique(unlist(lapply(.xs,names),use.names=FALSE));.xs<-lapply(.xs,function(.x){for(.n in setdiff(.all,names(.x))).x[[.n]]<-NA;.x[,.all,drop=FALSE]});do.call(rbind,.xs)}",
    "if(length(.final_layers)||!is.null(.dataset_work)){.out<-.fullrun_bind(.final_layers);if(is.null(.out)).out<-data.frame();.missing_dataset<-setdiff(names(.dataset_work),names(.out));if(length(.missing_dataset)){.join_cols<-unique(c(.fullrun_subject_keys,.missing_dataset));.out<-.fullrun_merge(.out,.dataset_work[,intersect(.join_cols,names(.dataset_work)),drop=FALSE],.fullrun_subject_keys)};.missing_spec<-setdiff(.fullrun_spec_order,names(.out));if(length(.missing_spec)&&!length(.not_run)){.failed_variable<-'final stack';.err<-paste('Stack is missing specification variables:',paste(.missing_spec,collapse=', '))}else{.out<-.out[,intersect(.fullrun_spec_order,names(.out)),drop=FALSE];saveRDS(.out,.fullrun_result_path)}}",
    "sink(type='message');sink();close(.logcon)",
    "jsonlite::write_json(list(completed=as.list(.completed),skipped=as.list(.skipped),not_run=.not_run,failed_variable=.failed_variable,error=.err,blocked_layers=as.list(.failed_layers),rows=if(is.null(.failed_variable))nrow(.out)else NULL,columns=if(is.null(.failed_variable))ncol(.out)else NULL,hashes=.fullrun_hashes),.fullrun_runner_path,auto_unbox=TRUE,null='null',pretty=TRUE)"
  )
  list(text=paste(lines,collapse="\n"),codes=codes,hashes=hashes)
}

fullrun_program_text <- function(project, dataset, side, plan, packages) {
  data_dir <- normalizePath(file.path(project_dir_of(project), "data"), winslash = "/", mustWork = FALSE)
  codes <- lapply(plan$ids, function(id) fullrun_code(project, dataset, id, side))
  names(codes) <- plan$ids
  has_code <- vapply(codes, function(code) length(code) == 1L && !is.na(code) && nzchar(trimws(code)), TRUE)
  execution_plan <- plan
  hash_code <- function(x) paste(as.character(openssl::sha256(charToRaw(enc2utf8(x %||% "")))), collapse = "")
  hashes <- vapply(codes, hash_code, "")
  if (isTRUE(plan$layered)) return(fullrun_layered_program_text(project,dataset,side,execution_plan,packages,codes,hashes))
  literal <- function(x) paste(deparse(x, control = "all", width.cutoff = 500L), collapse = "\n")
  code_literal <- function(code) {
    lines <- strsplit(code %||% "", "\n", fixed = TRUE)[[1]]
    if (!length(lines)) lines <- ""
    paste0("c(", paste(vapply(lines, literal, ""), collapse = ", "), ")")
  }
  deps <- lapply(plan$ids, function(id) upstream_ids(plan$graph, id))
  names(deps) <- plan$ids
  target_vars <- vapply(plan$ids, function(id) split_node_id(id)$var %||% "", "")
  step_blocks <- unlist(lapply(seq_along(execution_plan$ids), function(i) {
    id <- execution_plan$ids[[i]]
    preview <- strsplit(codes[[i]] %||% "", "\n", fixed = TRUE)[[1]]
    if (!length(preview)) preview <- ""
    c(paste0("# ==== ", id, " ===="),
      paste0("#   ", preview),
      "if (is.null(.failed)) {",
      " .ok <- local({",
      paste0("   .id <- ", literal(id)),
      paste0("   .code <- ", code_literal(codes[[i]])),
      "   .fullrun_execute(.id, .code)",
      " })",
      "}")
  }), use.names = FALSE)
  multi_key_records<-isTRUE(plan$multi_key_records)
  records_code<-if(multi_key_records)paste0(".fullrun_records_id <- ",literal(plan$records_id),"\n.fullrun_spec_order <- ",literal(plan$spec_order))else character()
  execute_code<-if(multi_key_records) {
    "  .failure <- tryCatch({ .exprs <- parse(text=.code, keep.source=TRUE); source(exprs=.exprs, local=.env, echo=FALSE, print.eval=TRUE, keep.source=TRUE); .result_name <- .fullrun_pick_output(.env,c(paste0('LINEAGE_',toupper(.fullrun_dataset)),paste0(toupper(.fullrun_dataset),'_TEMP')),if(identical(.fullrun_targets[[.id]],'RECORDS')) 'PARAMCD' else .fullrun_targets[[.id]],.work); if (!exists(.result_name,.env,inherits=FALSE)) stop(paste('Code must update',.result_name)); .result <- get(.result_name, .env, inherits=FALSE); if (!is.data.frame(.result)) stop('`result` must be a data.frame.'); .res <- as.data.frame(.result, stringsAsFactors=FALSE); if (!all(.fullrun_keys %in% names(.res))) stop('result is missing dataset keys.'); if (identical(.id,.fullrun_records_id)) { .piece <- .res[, .fullrun_keys, drop=FALSE]; if (anyDuplicated(.piece)) stop('Records result contains duplicate keys.'); .work <<- .piece } else { .target <- .fullrun_targets[[.id]]; .piece <- .res[, intersect(unique(c(.fullrun_keys, .target)), names(.res)), drop=FALSE]; if (!all(.fullrun_keys %in% names(.piece))) stop('result is missing dataset keys.'); if (nrow(.work)==0L) .work <<- .piece else { .newkeys <- merge(.piece[, .fullrun_keys, drop=FALSE], .work[, .fullrun_keys, drop=FALSE], by=.fullrun_keys, all=FALSE); if (nrow(.newkeys) != nrow(.piece)) stop('result contains keys outside the key dataset.'); .merged <- merge(.work, .piece, by=.fullrun_keys, all.x=TRUE, sort=FALSE); if (nrow(.merged) != nrow(.work)) stop('result would change the dataset row count.'); .work <<- .merged } }; NULL }, error=function(e) conditionMessage(e))"
  } else {
    "  .failure <- tryCatch({ .exprs <- parse(text=.code, keep.source=TRUE); source(exprs=.exprs, local=.env, echo=FALSE, print.eval=TRUE, keep.source=TRUE); .result_name <- .fullrun_pick_output(.env,c(paste0('LINEAGE_',toupper(.fullrun_dataset)),paste0(toupper(.fullrun_dataset),'_TEMP')),if(identical(.fullrun_targets[[.id]],'RECORDS')) 'PARAMCD' else .fullrun_targets[[.id]],.work); if (!exists(.result_name,.env,inherits=FALSE)) stop(paste('Code must update',.result_name)); .result <- get(.result_name, .env, inherits=FALSE); if (!is.data.frame(.result)) stop('`result` must be a data.frame.'); .res <- as.data.frame(.result, stringsAsFactors=FALSE); if (length(.fullrun_keys) && !all(.fullrun_keys %in% names(.res))) stop('result is missing dataset keys.'); .target <- .fullrun_targets[[.id]]; .piece <- .res[, intersect(unique(c(.fullrun_keys, .target)), names(.res)), drop=FALSE]; if (length(.fullrun_keys) && !all(.fullrun_keys %in% names(.piece))) stop('result is missing dataset keys.'); if (nrow(.work)==0L) .work <<- .piece else if (length(.fullrun_keys)) { .newkeys <- merge(.piece[, .fullrun_keys, drop=FALSE], .work[, .fullrun_keys, drop=FALSE], by=.fullrun_keys, all=FALSE); if (nrow(.newkeys) != nrow(.piece)) stop('result contains keys outside the key dataset.'); .merged <- merge(.work, .piece, by=.fullrun_keys, all.x=TRUE, sort=FALSE); if (nrow(.merged) != nrow(.work)) stop('result would change the dataset row count.'); .work <<- .merged } else stop('Dataset keys are required to merge variable results.'); NULL }, error=function(e) conditionMessage(e))"
  }
  final_run_code<-if(multi_key_records)"if (is.null(.failed)) { .missing <- setdiff(.fullrun_spec_order,names(.work)); if(length(.missing)){.failed <- 'final stack';.err <- paste('Full run is missing specification variables:',paste(.missing,collapse=', '))}else{.work <- .work[,.fullrun_spec_order,drop=FALSE];saveRDS(.work,.fullrun_result_path)} }" else "if (is.null(.failed)) saveRDS(.work, .fullrun_result_path)"
  lines <- c(
    "# Generated Dualdag full-program run. This script is standalone.",
    paste0(".fullrun_dataset <- ", literal(dataset)),
    paste0(".fullrun_side <- ", literal(side)),
    paste0(".fullrun_keys <- ", literal(plan$keys)),
    records_code,
    paste0(".fullrun_hashes <- ", literal(as.list(hashes))),
    paste0(".fullrun_codes <- ", literal(as.list(codes))),
    paste0(".fullrun_deps <- ", literal(deps)),
    paste0(".fullrun_targets <- ", literal(as.list(target_vars))),
    paste0(".fullrun_data_dir <- ", literal(data_dir)),
    paste0(".fullrun_result_path <- ", literal(file.path(fullrun_dir(project_dir_of(project), dataset, side), "result.rds"))),
    paste0(".fullrun_runner_path <- ", literal(file.path(fullrun_dir(project_dir_of(project), dataset, side), "runner.json"))),
    paste0(".fullrun_log_path <- ", literal(file.path(fullrun_dir(project_dir_of(project), dataset, side), "log.txt"))),
    paste0(".fullrun_packages <- ", literal(as.character(packages))),
    ".file_arg <- grep('^--file=', commandArgs(FALSE), value=TRUE)",
    "if (length(.file_arg)) setwd(dirname(normalizePath(sub('^--file=', '', .file_arg[[1]]), winslash='/', mustWork=TRUE)))",
    ".logcon <- file(.fullrun_log_path, open='wt', encoding='UTF-8'); sink(.logcon); sink(.logcon, type='message')",
    "for (.pkg in .fullrun_packages) suppressPackageStartupMessages(library(.pkg, character.only=TRUE))",
    ".src <- list()",
    "if (dir.exists(.fullrun_data_dir)) for (.f in list.files(.fullrun_data_dir, pattern='\\\\.[Xx][Pp][Tt]$', full.names=TRUE)) { .nm <- toupper(tools::file_path_sans_ext(basename(.f))); if (!(.nm %in% names(.src))) { .df <- as.data.frame(haven::read_xpt(.f), stringsAsFactors=FALSE); for (.j in seq_along(.df)) attributes(.df[[.j]]) <- attributes(.df[[.j]])[intersect(names(attributes(.df[[.j]])), c('class','levels','tzone','units'))]; rownames(.df) <- NULL; .src[[.nm]] <- .df } }",
    ".work <- if (length(.fullrun_keys)) as.data.frame(setNames(replicate(length(.fullrun_keys), logical(0), simplify=FALSE), .fullrun_keys), stringsAsFactors=FALSE) else data.frame()",
    ".completed <- character(); .skipped <- character(); .not_run <- list(); .failed <- NULL; .err <- NULL",
    ".fullrun_pick_output <- function(.env,.candidates,.target,.inherited) { if (exists('result',.env,inherits=FALSE)) return('result'); .candidates <- unique(.candidates); .frames <- .candidates[vapply(.candidates,function(.n) exists(.n,.env,inherits=FALSE) && is.data.frame(get(.n,.env,inherits=FALSE)),TRUE)]; .matching <- .frames[vapply(.frames,function(.n) .target %in% names(get(.n,.env,inherits=FALSE)),TRUE)]; if(length(.matching)) return(.matching[[1L]]); .changed <- .frames[!vapply(.frames,function(.n) identical(get(.n,.env,inherits=FALSE),.inherited),TRUE)]; if(length(.changed)) return(.changed[[1L]]); if(length(.frames)) return(.frames[[1L]]); .candidates[[1L]] }",
    ".fullrun_execute <- function(.id, .code) {",
    "  cat('# ==== ', .id, ' ====\\n', sep='')",
    "  .code <- paste(as.character(.code), collapse='\\n')",
    "  if (!nzchar(trimws(.code))) { .skipped <<- c(.skipped, .id); .not_run[[.id]] <<- 'no code'; cat('Skipped ', .id, ': no code\\n', sep=''); return(TRUE) }",
    "  .deps <- .fullrun_deps[[.id]]; if (is.null(.deps)) .deps <- character(); .baddeps <- intersect(.deps, .skipped)",
    "  if (length(.baddeps)) { .skipped <<- c(.skipped, .id); .not_run[[.id]] <<- paste0('upstream ', .baddeps[[1]], ' not run'); cat('Skipped ', .id, ': upstream ', .baddeps[[1]], ' not run\\n', sep=''); return(TRUE) }",
    "  .env <- new.env(parent=globalenv()); .env$src <- .src; .env$work <- .work; .env$upstream <- .work; .env$keys <- .fullrun_keys; .env[[paste0(toupper(.fullrun_dataset), '_TEMP')]] <- .work; if (identical(.fullrun_side,'primary')) { .env[[paste0('LINEAGE_',toupper(.fullrun_dataset))]] <- .work; for (.dom in names(.src)) .env[[paste0('LINEAGE_SRC_',toupper(.dom))]] <- .src[[.dom]] }",
    execute_code,
    "  if (!is.null(.failure)) { .failed <<- .id; .err <<- .failure; cat('Full run failed at ', .id, ': ', .failure, '\\nCompleted: ', paste(.completed, collapse=', '), '\\n', sep=''); return(FALSE) }",
    "  .completed <<- c(.completed, .id); TRUE",
    "}",
    step_blocks,
    "if (!is.null(.failed)) { .pending <- setdiff(names(.fullrun_deps), c(.completed, .skipped)); for (.id in .pending) { .bad <- intersect(.fullrun_deps[[.id]], c(.skipped, .failed)); .code <- paste(as.character(.fullrun_codes[[.id]] %||% ''), collapse='\\n'); .reason <- if (length(.bad)) paste0('upstream ', .bad[[1]], ' not run') else if (!nzchar(trimws(.code))) 'no code' else paste0('not run after ', .failed, ' failed'); .skipped <- c(.skipped, .id); .not_run[[.id]] <- .reason } }",
    final_run_code,
    "sink(type='message'); sink(); close(.logcon)",
    "jsonlite::write_json(list(completed=as.list(.completed), skipped=as.list(.skipped), not_run=.not_run, failed_variable=.failed, error=.err, rows=if(is.null(.failed)) nrow(.work) else NULL, columns=if(is.null(.failed)) ncol(.work) else NULL, hashes=.fullrun_hashes), .fullrun_runner_path, auto_unbox=TRUE, null='null', pretty=TRUE)"
  )
  list(text = paste(lines, collapse = "\n"), codes = codes, hashes = hashes)
}

fullrun_worker <- function(project, dataset, side, timeout = 600) {
  started <- as.POSIXct(Sys.time(), tz = "UTC")
  dir <- fullrun_dir(project, dataset, side)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  plan <- fullrun_plan(project, dataset, side)
  pkgs <- side_r_packages(project, side)
  program <- fullrun_program_text(project, dataset, side, plan, pkgs)
  write_utf8(program$text, file.path(dir, "program.R"))
  if (file.exists(file.path(dir, "result.rds"))) unlink(file.path(dir, "result.rds"))
  # Dependency relationships are preserved in graph order and checked below.
  log_path <- file.path(dir, "log.txt")
  runner_path <- file.path(dir, "runner.json")
  if (file.exists(runner_path)) unlink(runner_path)
  nonliteral <- NULL
  required_by_id <- setNames(lapply(names(program$codes), function(id) code_required_packages(program$codes[[id]])), names(program$codes))
  for (id in names(program$codes)) {
    nonliteral <- code_disallowed_packages(program$codes[[id]], pkgs)
    if (!is.null(nonliteral)) break
  }
  missing <- missing_r_package_names(c(pkgs, unlist(required_by_id, use.names = FALSE)))
  code_ids <- names(program$codes) %||% character()
  failing_id <- if (length(code_ids)) code_ids[[1]] else if (length(plan$ids)) plan$ids[[1]] else NULL
  if (length(missing)) {
    for (id in names(required_by_id)) {
      if (length(intersect(required_by_id[[id]], missing))) { failing_id <- id; break }
    }
  }
  if (!is.null(nonliteral) || length(missing)) {
    bad <- nonliteral %||% package_install_message(missing)
    write_utf8(paste0("Error: ", bad, "\n"), log_path)
    rr <- list(completed = list(), skipped = list(), failed_variable = failing_id, error = bad, rows = NULL, columns = NULL, hashes = as.list(program$hashes))
  } else {
    # The process executing this worker is already a fresh callr child.
    out <- tryCatch(callr::rscript(file.path(dir, "program.R"), show = FALSE, wd = dir, timeout = timeout), error = function(e) e)
    if (inherits(out, "error")) write_utf8(paste0(read_log_file(log_path), "\n", conditionMessage(out)), log_path)
    rr <- tryCatch(jsonlite::fromJSON(runner_path, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(rr)) rr <- list(completed = list(), skipped = list(), failed_variable = NULL, error = if (inherits(out, "error")) conditionMessage(out) else "Program did not produce a run result.", rows = NULL, columns = NULL, hashes = as.list(program$hashes))
  }
  missing_pkg <- missing_r_package_from_error(paste(rr$error %||% "", read_log_file(log_path)))
  if (nzchar(missing_pkg)) {
    rr$error <- package_install_message(missing_pkg)
    if (is.null(rr$failed_variable) || !nzchar(as.character(rr$failed_variable)[1] %||% ""))
      rr$failed_variable <- plan$ids[[1]] %||% NULL
    log <- read_log_file(log_path)
    if (!grepl(rr$error, log, fixed = TRUE)) write_utf8(paste(log, rr$error, sep = "\n"), log_path)
  }
  finished <- as.POSIXct(Sys.time(), tz = "UTC")
  not_run <- fullrun_not_run_entries(rr$not_run %||% list(), rr$failed_variable)
  variables_total <- length(plan$ids)
  variables_run <- rr$completed %||% list()
  meta <- list(ok = is.null(rr$failed_variable) && is.null(rr$error), started = format(started, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    finished = format(finished, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), seconds = as.numeric(difftime(finished, started, units = "secs")),
    variables_run = variables_run, skipped = rr$skipped %||% list(), failed_variable = rr$failed_variable,
    error = rr$error, rows = rr$rows, columns = rr$columns, hashes = rr$hashes %||% as.list(program$hashes),
    partial = length(not_run) > 0L || length(variables_run) < variables_total,
    variables_total = variables_total, not_run = not_run)
  write_json(meta, file.path(dir, "run.json"))
  if (file.exists(runner_path)) unlink(runner_path)
  meta
}

fullrun_code_hashes <- function(codes) {
  vapply(codes, function(code) paste(as.character(openssl::sha256(charToRaw(enc2utf8(code %||% "")))), collapse = ""), "")
}

fullrun_not_run_entries <- function(x, failed_variable = NULL) {
  ids <- names(x) %||% character()
  entries <- lapply(ids, function(id) list(id = id, reason = as.character(x[[id]] %||% "not run")[1]))
  if (!is.null(failed_variable) && length(failed_variable) && nzchar(as.character(failed_variable)[1])) {
    id <- as.character(failed_variable)[1]
    if (!id %in% ids) entries[[length(entries) + 1L]] <- list(id = id, reason = "failed")
  }
  entries
}

fullrun_current_hashes <- function(project, dataset, side, plan = NULL, codes = NULL) {
  plan <- plan %||% fullrun_plan(project, dataset, side)
  codes <- codes %||% setNames(lapply(plan$ids, function(id) fullrun_code(project, dataset, id, side)), plan$ids)
  fullrun_code_hashes(codes)
}

fullrun_readiness <- function(project, dataset, side, plan = NULL, codes = NULL) {
  plan <- plan %||% fullrun_plan(project, dataset, side)
  codes <- codes %||% setNames(lapply(plan$ids, function(id) fullrun_code(project, dataset, id, side)), plan$ids)
  ids <- plan$ids
  has_code <- vapply(ids, function(id) {
    code <- codes[[id]] %||% ""
    length(code) == 1L && !is.na(code) && nzchar(trimws(code))
  }, TRUE)
  graph <- plan$graph
  expected_layers <- if (isTRUE(plan$layered)) unique(vapply(graph$layers %||% list(), function(layer) {
    toupper(as.character(layer$paramcd %||% ""))
  }, "")) else character()
  expected_layers <- expected_layers[nzchar(expected_layers)]
  generated_layers <- intersect(expected_layers, toupper(flat_chr(graph$generated_parameters)))
  list(layers_total = length(expected_layers),
       layers_generated = length(generated_layers),
       layers_missing = as.list(setdiff(expected_layers, generated_layers)),
       variables_total = length(ids),
       variables_with_code = sum(has_code),
       variables_without_code = as.list(ids[!has_code]))
}

fullrun_status <- function(project, dataset, side, running = FALSE) {
  plan <- fullrun_plan(project, dataset, side)
  codes <- setNames(lapply(plan$ids, function(id) fullrun_code(project, dataset, id, side)), plan$ids)
  dir <- fullrun_dir(project, dataset, side)
  path <- file.path(dir, "run.json")
  meta <- if (file.exists(path)) tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL) else NULL
  stale <- FALSE
  if (!is.null(meta$hashes)) {
    old <- unlist(meta$hashes, use.names = TRUE)
    now <- fullrun_current_hashes(project, dataset, side, plan, codes)
    stale <- !identical(sort(names(old)), sort(names(now))) || !identical(unname(old[sort(names(old))]), unname(now[sort(names(now))]))
  }
  log_path <- file.path(dir, "log.txt")
  log_tail <- if (file.exists(log_path)) tail(readLines(log_path, warn = FALSE), 80L) else character()
  list(run = meta, running = isTRUE(running), stale = stale, log_tail = as.list(log_tail),
       readiness = fullrun_readiness(project, dataset, side, plan, codes))
}
