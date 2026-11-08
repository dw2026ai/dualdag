# Reference ADaM build for examples/pharmaverse-demo.
#
# The specs in examples/pharmaverse-demo/spec are the authority. This script
# implements them against pharmaversesdtm 1.5.0 and writes
#   ~/dualdag-demo/pharmaverse-demo/adsl.xpt
#   ~/dualdag-demo/pharmaverse-demo/adae.xpt
#   ~/dualdag-demo/pharmaverse-demo/advs.xpt
#
#   Rscript tools/pharmaverse_demo_ref.R
#   Rscript tools/pharmaverse_demo_ref.R --write-spec
#
# --write-spec rewrites the CSV and xlsx from pharmaverse_demo_spec_frames().
# A normal run only refreshes the three transports.

pharmaverse_demo_root <- function() {
  x <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in seq_len(6L)) {
    if (file.exists(file.path(x, "tools", "pharmaverse_demo_ref.R"))) return(x)
    parent <- dirname(x)
    if (parent == x) break
    x <- parent
  }
  stop("Cannot find the dualdag root from ", getwd(), call. = FALSE)
}

pharmaverse_sdtm_dir <- function() {
  path.expand("~/dualdag-demo/pharmaversesdtm")
}

pharmaverse_demo_out_dir <- function() {
  path.expand("~/dualdag-demo/pharmaverse-demo")
}

pharmaverse_demo_spec_dir <- function() {
  file.path(pharmaverse_demo_root(), "examples", "pharmaverse-demo", "spec")
}

.demo_pkgs <- function() {
  suppressPackageStartupMessages({
    library(dplyr)
    library(haven)
  })
}

as_chr <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) x <- format(x)
  x <- as.character(haven::zap_labels(x))
  x[is.na(x)] <- ""
  x
}

as_num <- function(x) {
  x <- haven::zap_labels(x)
  suppressWarnings(as.numeric(x))
}

as_int <- function(x) {
  x <- haven::zap_labels(x)
  if (inherits(x, "Date")) return(as.integer(x))
  suppressWarnings(as.integer(x))
}

complete_day <- function(x) {
  x <- as_chr(x)
  grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x)
}

# Complete calendar day, or that day followed by T and a clock time.
parse_used_date <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  ok <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}($|T)", x, perl = TRUE)
  as.Date(ifelse(ok, substr(x, 1L, 10L), NA_character_))
}

parse_complete_date <- function(x) {
  x <- as_chr(x)
  as.Date(ifelse(complete_day(x), x, NA_character_))
}

read_sdtm <- function(dom, dir = pharmaverse_sdtm_dir()) {
  path <- file.path(dir, paste0(tolower(dom), ".xpt"))
  if (!file.exists(path)) stop("Missing SDTM transport: ", path, call. = FALSE)
  haven::read_xpt(path)
}

blank_to_na <- function(x) {
  x <- as_chr(x)
  x[!nzchar(x)] <- NA_character_
  x
}

group_key3 <- function(usubjid, paramcd, atptn) {
  paste(usubjid, paramcd, ifelse(is.na(atptn), "*", sprintf("%.0f", atptn)), sep = "\r")
}

# Relative day: analysis date minus reference date, plus 1 when analysis is on or after reference.
rel_day <- function(adt, ref) {
  out <- rep(NA_integer_, length(adt))
  ok <- !is.na(adt) & !is.na(ref)
  on_or_after <- ok & adt >= ref
  before <- ok & adt < ref
  out[on_or_after] <- as.integer(adt[on_or_after] - ref[on_or_after]) + 1L
  out[before] <- as.integer(adt[before] - ref[before])
  out
}

# Conservative start-date imputation. Day-missing dates start at day 01 and move
# forward to TRTSDT when that date falls in the same month. Year-only dates start
# at 01 January and move forward to TRTSDT when that date falls in the same year.
impute_start <- function(dtc, trtsdt) {
  dtc <- as_chr(dtc)
  n <- length(dtc)
  out <- as.Date(rep(NA_character_, n))
  flag <- rep(NA_character_, n)
  trt_chr <- ifelse(is.na(trtsdt), "", format(trtsdt, "%Y-%m-%d"))
  for (i in seq_len(n)) {
    x <- dtc[[i]]
    t <- trt_chr[[i]]
    if (!nzchar(x)) next
    if (complete_day(x)) {
      out[[i]] <- as.Date(x)
      next
    }
    if (grepl("^[0-9]{4}-[0-9]{2}$", x)) {
      cand <- as.Date(paste0(x, "-01"))
      flag[[i]] <- "D"
      if (nzchar(t) && substr(t, 1L, 7L) == x && cand < as.Date(t)) cand <- as.Date(t)
      out[[i]] <- cand
      next
    }
    if (grepl("^[0-9]{4}$", x)) {
      cand <- as.Date(paste0(x, "-01-01"))
      flag[[i]] <- "M"
      if (nzchar(t) && substr(t, 1L, 4L) == x && cand < as.Date(t)) cand <- as.Date(t)
      out[[i]] <- cand
    }
  }
  list(date = out, flag = flag)
}

# End dates in this study are complete or blank. The rule still imputes a partial
# end to the last day of the month or to 31 December, then caps it at TRTEDT when
# the partial value could be that treatment end date.
impute_end <- function(dtc, trtedt) {
  dtc <- as_chr(dtc)
  n <- length(dtc)
  out <- as.Date(rep(NA_character_, n))
  flag <- rep(NA_character_, n)
  trt_chr <- ifelse(is.na(trtedt), "", format(trtedt, "%Y-%m-%d"))
  for (i in seq_len(n)) {
    x <- dtc[[i]]
    t <- trt_chr[[i]]
    if (!nzchar(x)) next
    if (complete_day(x)) {
      out[[i]] <- as.Date(x)
      next
    }
    if (grepl("^[0-9]{4}-[0-9]{2}$", x)) {
      first <- as.Date(paste0(x, "-01"))
      cand <- seq(first, length.out = 2L, by = "month")[2] - 1L
      flag[[i]] <- "D"
      if (nzchar(t) && substr(t, 1L, 7L) == x && cand > as.Date(t)) cand <- as.Date(t)
      out[[i]] <- cand
      next
    }
    if (grepl("^[0-9]{4}$", x)) {
      cand <- as.Date(paste0(x, "-12-31"))
      flag[[i]] <- "M"
      if (nzchar(t) && substr(t, 1L, 4L) == x && cand > as.Date(t)) cand <- as.Date(t)
      out[[i]] <- cand
    }
  }
  list(date = out, flag = flag)
}

stamp_start <- function(day) {
  out <- as.POSIXct(rep(NA_real_, length(day)), origin = "1970-01-01", tz = "UTC")
  ok <- !is.na(day)
  out[ok] <- as.POSIXct(paste(format(day[ok], "%Y-%m-%d"), "00:00:00"), tz = "UTC")
  out
}

stamp_end <- function(day) {
  out <- as.POSIXct(rep(NA_real_, length(day)), origin = "1970-01-01", tz = "UTC")
  ok <- !is.na(day)
  out[ok] <- as.POSIXct(paste(format(day[ok], "%Y-%m-%d"), "23:59:59"), tz = "UTC")
  out
}

trt_code <- function(x) {
  x <- as_chr(x)
  out <- rep(NA_integer_, length(x))
  out[x == "Placebo"] <- 0L
  out[x == "Xanomeline Low Dose"] <- 1L
  out[x == "Xanomeline High Dose"] <- 2L
  out[x == "Screen Failure"] <- 3L
  out
}

supp_flag <- function(suppdm, qnam, usubjid) {
  q <- as_chr(suppdm$QNAM)
  v <- as_chr(suppdm$QVAL)
  id <- as_chr(suppdm$USUBJID)
  hit <- unique(id[q == qnam & v == "Y"])
  ifelse(usubjid %in% hit, "Y", "N")
}

pick_row <- function(df, decreasing_date, date_col, seq_col) {
  if (!nrow(df)) return(df)
  date_val <- df[[date_col]]
  seq_val <- df[[seq_col]]
  ord <- if (isTRUE(decreasing_date)) {
    order(df$USUBJID, date_val, seq_val, decreasing = c(FALSE, TRUE, TRUE), method = "radix")
  } else {
    order(df$USUBJID, date_val, seq_val, method = "radix")
  }
  df <- df[ord, , drop = FALSE]
  df[!duplicated(df$USUBJID), , drop = FALSE]
}

exposure_bounds <- function(ex) {
  ex <- data.frame(
    USUBJID = as_chr(ex$USUBJID),
    EXSEQ = as_int(ex$EXSEQ),
    EXTRT = as_chr(ex$EXTRT),
    EXDOSE = as_num(ex$EXDOSE),
    EXSTDTC = as_chr(ex$EXSTDTC),
    EXENDTC = as_chr(ex$EXENDTC),
    stringsAsFactors = FALSE
  )
  qual <- !is.na(ex$EXDOSE) & (ex$EXDOSE > 0 | ex$EXTRT == "PLACEBO")
  ex <- ex[qual, , drop = FALSE]
  start <- ex[complete_day(ex$EXSTDTC), , drop = FALSE]
  first <- pick_row(start, FALSE, "EXSTDTC", "EXSEQ")
  end_day <- ifelse(complete_day(ex$EXENDTC), ex$EXENDTC,
                    ifelse(complete_day(ex$EXSTDTC), ex$EXSTDTC, NA_character_))
  last_src <- ex[!is.na(end_day), , drop = FALSE]
  last_src$ENDDAY <- end_day[!is.na(end_day)]
  last <- pick_row(last_src, TRUE, "ENDDAY", "EXSEQ")
  list(
    first = data.frame(USUBJID = first$USUBJID, TRTSDT = as.Date(first$EXSTDTC), stringsAsFactors = FALSE),
    last = data.frame(USUBJID = last$USUBJID, TRTEDT = as.Date(last$ENDDAY), stringsAsFactors = FALSE),
    exposed = unique(ex$USUBJID)
  )
}

one_ds_row <- function(ds, cat, decod = NULL) {
  ds <- data.frame(
    USUBJID = as_chr(ds$USUBJID),
    DSSEQ = as_int(ds$DSSEQ),
    DSCAT = as_chr(ds$DSCAT),
    DSDECOD = as_chr(ds$DSDECOD),
    DSSTDTC = as_chr(ds$DSSTDTC),
    stringsAsFactors = FALSE
  )
  keep <- ds$DSCAT == cat
  if (!is.null(decod)) keep <- keep & ds$DSDECOD == decod
  ds <- ds[keep, , drop = FALSE]
  if (!nrow(ds)) {
    return(data.frame(USUBJID = character(), DSDECOD = character(), DSSTDTC = character(),
                      stringsAsFactors = FALSE))
  }
  ds$DAY <- parse_complete_date(ds$DSSTDTC)
  # Earliest complete day, then smallest sequence. A missing day sorts last.
  ds$DAYKEY <- ifelse(is.na(ds$DAY), "9999-99-99", format(ds$DAY, "%Y-%m-%d"))
  ds <- ds[order(ds$USUBJID, ds$DAYKEY, ds$DSSEQ, method = "radix"), , drop = FALSE]
  ds <- ds[!duplicated(ds$USUBJID), , drop = FALSE]
  ds[, c("USUBJID", "DSDECOD", "DSSTDTC")]
}

collect_last_alive <- function(tables, usubjid) {
  pieces <- list()
  add <- function(df, cols) {
    id <- as_chr(df$USUBJID)
    for (col in cols) {
      d <- parse_used_date(df[[col]])
      ok <- !is.na(d)
      if (!any(ok)) next
      pieces[[length(pieces) + 1L]] <<- data.frame(USUBJID = id[ok], DT = d[ok], stringsAsFactors = FALSE)
    }
  }
  add(tables$dm, c("RFPENDTC", "RFENDTC", "DTHDTC"))
  add(tables$ds, "DSSTDTC")
  add(tables$ex, c("EXSTDTC", "EXENDTC"))
  add(tables$ae, c("AESTDTC", "AEENDTC"))
  add(tables$vs, "VSDTC")
  add(tables$lb, "LBDTC")
  add(tables$sv, c("SVSTDTC", "SVENDTC"))
  add(tables$cm, c("CMSTDTC", "CMENDTC"))
  add(tables$mh, c("MHSTDTC", "MHENDTC"))
  if (!length(pieces)) return(as.Date(rep(NA_character_, length(usubjid))))
  all <- do.call(rbind, pieces)
  mx <- aggregate(DT ~ USUBJID, data = all, FUN = max)
  out <- as.Date(rep(NA_character_, length(usubjid)))
  hit <- match(usubjid, mx$USUBJID)
  out[!is.na(hit)] <- mx$DT[hit[!is.na(hit)]]
  out
}

baseline_vs <- function(vs, testcd, trt) {
  vs <- data.frame(
    USUBJID = as_chr(vs$USUBJID),
    VSTESTCD = as_chr(vs$VSTESTCD),
    VSSTRESN = as_num(vs$VSSTRESN),
    VSDTC = parse_used_date(vs$VSDTC),
    VSSEQ = as_int(vs$VSSEQ),
    stringsAsFactors = FALSE
  )
  vs <- vs[vs$VSTESTCD == testcd & !is.na(vs$VSSTRESN) & !is.na(vs$VSDTC), , drop = FALSE]
  vs <- merge(vs, trt, by = "USUBJID", all.x = FALSE, sort = FALSE)
  vs <- vs[!is.na(vs$TRTSDT) & vs$VSDTC <= vs$TRTSDT, , drop = FALSE]
  if (!nrow(vs)) {
    return(data.frame(USUBJID = character(), VALUE = numeric(), stringsAsFactors = FALSE))
  }
  vs$DAYKEY <- format(vs$VSDTC, "%Y-%m-%d")
  vs <- vs[order(vs$USUBJID, vs$DAYKEY, vs$VSSEQ, decreasing = c(FALSE, TRUE, TRUE), method = "radix"), , drop = FALSE]
  vs <- vs[!duplicated(vs$USUBJID), , drop = FALSE]
  data.frame(USUBJID = vs$USUBJID, VALUE = vs$VSSTRESN, stringsAsFactors = FALSE)
}

build_pharmaverse_adsl <- function(dir = pharmaverse_sdtm_dir()) {
  .demo_pkgs()
  dm <- read_sdtm("dm", dir)
  suppdm <- read_sdtm("suppdm", dir)
  ex <- read_sdtm("ex", dir)
  ds <- read_sdtm("ds", dir)
  ae <- read_sdtm("ae", dir)
  vs <- read_sdtm("vs", dir)
  lb <- read_sdtm("lb", dir)
  sv <- read_sdtm("sv", dir)
  cm <- read_sdtm("cm", dir)
  mh <- read_sdtm("mh", dir)

  id <- as_chr(dm$USUBJID)
  bounds <- exposure_bounds(ex)
  first <- bounds$first
  last <- bounds$last
  trtsdt <- first$TRTSDT[match(id, first$USUBJID)]
  trtedt <- last$TRTEDT[match(id, last$USUBJID)]
  age <- as_int(dm$AGE)
  agegr1 <- ifelse(age < 65, "<65", ifelse(age <= 80, "65-80", ">80"))
  agegr1n <- ifelse(age < 65, 1L, ifelse(age <= 80, 2L, 3L))
  race <- as_chr(dm$RACE)
  racen <- rep(NA_integer_, length(id))
  racen[race == "WHITE"] <- 1L
  racen[race == "BLACK OR AFRICAN AMERICAN"] <- 2L
  racen[race == "ASIAN"] <- 3L
  racen[race == "AMERICAN INDIAN OR ALASKA NATIVE"] <- 4L
  trt01p <- as_chr(dm$ARM)
  trt01a <- as_chr(dm$ACTARM)
  disp <- one_ds_row(ds, "DISPOSITION EVENT")
  eos <- ifelse(id %in% disp$USUBJID[disp$DSDECOD == "COMPLETED"], "COMPLETED", "DISCONTINUED")
  # Every subject in this study has a disposition row. A subject with none would
  # stay DISCONTINUED only when a row exists and is not COMPLETED.
  known <- id %in% disp$USUBJID
  eos[!known] <- NA_character_
  dcs <- rep(NA_character_, length(id))
  dcs_decod <- disp$DSDECOD[match(id, disp$USUBJID)]
  dcs[!is.na(eos) & eos == "DISCONTINUED"] <- dcs_decod[!is.na(eos) & eos == "DISCONTINUED"]
  rand <- one_ds_row(ds, "PROTOCOL MILESTONE", "RANDOMIZED")
  randdt <- parse_complete_date(rand$DSSTDTC[match(id, rand$USUBJID)])
  dthfl_src <- as_chr(dm$DTHFL)
  height <- baseline_vs(vs, "HEIGHT", data.frame(USUBJID = id, TRTSDT = trtsdt, stringsAsFactors = FALSE))
  weight <- baseline_vs(vs, "WEIGHT", data.frame(USUBJID = id, TRTSDT = trtsdt, stringsAsFactors = FALSE))
  heightbl <- height$VALUE[match(id, height$USUBJID)]
  weightbl <- weight$VALUE[match(id, weight$USUBJID)]
  bmibl <- ifelse(!is.na(heightbl) & !is.na(weightbl) & heightbl > 0,
                  weightbl / (heightbl / 100) ^ 2, NA_real_)
  tables <- list(dm = dm, ds = ds, ex = ex, ae = ae, vs = vs, lb = lb, sv = sv, cm = cm, mh = mh)
  out <- data.frame(
    STUDYID = as_chr(dm$STUDYID),
    USUBJID = id,
    SUBJID = as_chr(dm$SUBJID),
    SITEID = as_chr(dm$SITEID),
    AGE = age,
    AGEU = as_chr(dm$AGEU),
    AGEGR1 = agegr1,
    AGEGR1N = as.integer(agegr1n),
    SEX = as_chr(dm$SEX),
    RACE = race,
    RACEN = racen,
    ETHNIC = as_chr(dm$ETHNIC),
    COUNTRY = as_chr(dm$COUNTRY),
    ARM = trt01p,
    ACTARM = trt01a,
    TRT01P = trt01p,
    TRT01PN = trt_code(trt01p),
    TRT01A = trt01a,
    TRT01AN = trt_code(trt01a),
    TRTSDTM = stamp_start(trtsdt),
    TRTSDT = trtsdt,
    TRTSDTF = NA_character_,
    TRTSTMF = ifelse(is.na(trtsdt), NA_character_, "H"),
    TRTEDTM = stamp_end(trtedt),
    TRTEDT = trtedt,
    TRTEDTF = NA_character_,
    TRTETMF = ifelse(is.na(trtedt), NA_character_, "H"),
    TRTDURD = ifelse(is.na(trtsdt) | is.na(trtedt), NA_integer_, as.integer(trtedt - trtsdt) + 1L),
    RANDDT = randdt,
    SAFFL = ifelse(id %in% bounds$exposed, "Y", "N"),
    ITTFL = supp_flag(suppdm, "ITT", id),
    EFFFL = supp_flag(suppdm, "EFFICACY", id),
    COMP8FL = supp_flag(suppdm, "COMPLT8", id),
    COMP16FL = supp_flag(suppdm, "COMPLT16", id),
    COMP24FL = supp_flag(suppdm, "COMPLT24", id),
    DTHFL = ifelse(dthfl_src == "Y", "Y", "N"),
    DTHDT = parse_complete_date(dm$DTHDTC),
    EOSSTT = eos,
    DCSREAS = dcs,
    RFENDT = parse_complete_date(dm$RFENDTC),
    LSTALVDT = collect_last_alive(tables, id),
    HEIGHTBL = heightbl,
    WEIGHTBL = weightbl,
    BMIBL = bmibl,
    stringsAsFactors = FALSE
  )
  out[order(out$USUBJID), , drop = FALSE]
}

build_pharmaverse_adae <- function(adsl, dir = pharmaverse_sdtm_dir()) {
  .demo_pkgs()
  ae <- read_sdtm("ae", dir)
  id <- as_chr(ae$USUBJID)
  sub <- adsl[match(id, adsl$USUBJID), , drop = FALSE]
  if (any(is.na(sub$USUBJID))) stop("AE subject missing from ADSL.", call. = FALSE)
  start <- impute_start(ae$AESTDTC, sub$TRTSDT)
  end <- impute_end(ae$AEENDTC, sub$TRTEDT)
  astdt <- start$date
  aendt <- end$date
  trtem <- ifelse(!is.na(astdt) & !is.na(sub$TRTSDT) & astdt >= sub$TRTSDT &
                    (is.na(sub$TRTEDT) | astdt <= sub$TRTEDT + 30L), "Y", "N")
  pre <- ifelse(!is.na(astdt) & !is.na(sub$TRTSDT) & astdt < sub$TRTSDT, "Y", "N")
  aesev <- as_chr(ae$AESEV)
  aesevn <- rep(NA_integer_, nrow(ae))
  aesevn[aesev == "MILD"] <- 1L
  aesevn[aesev == "MODERATE"] <- 2L
  aesevn[aesev == "SEVERE"] <- 3L
  aerel <- blank_to_na(ae$AEREL)
  areln <- rep(NA_integer_, nrow(ae))
  areln[!is.na(aerel) & aerel == "NONE"] <- 1L
  areln[!is.na(aerel) & aerel == "REMOTE"] <- 2L
  areln[!is.na(aerel) & aerel == "POSSIBLE"] <- 3L
  areln[!is.na(aerel) & aerel == "PROBABLE"] <- 4L
  adurn <- ifelse(!is.na(astdt) & !is.na(aendt) & aendt >= astdt, as.integer(aendt - astdt) + 1L, NA_integer_)
  out <- data.frame(
    STUDYID = as_chr(ae$STUDYID),
    USUBJID = id,
    SUBJID = sub$SUBJID,
    SITEID = sub$SITEID,
    AESEQ = as_int(ae$AESEQ),
    AETERM = as_chr(ae$AETERM),
    AEDECOD = as_chr(ae$AEDECOD),
    AEBODSYS = as_chr(ae$AEBODSYS),
    AELLT = as_chr(ae$AELLT),
    AESEV = aesev,
    ASEV = aesev,
    ASEVN = aesevn,
    AEREL = as_chr(ae$AEREL),
    AREL = aerel,
    ARELN = areln,
    AESER = as_chr(ae$AESER),
    AEOUT = as_chr(ae$AEOUT),
    AESDTH = as_chr(ae$AESDTH),
    TRT01A = sub$TRT01A,
    TRT01AN = sub$TRT01AN,
    SAFFL = sub$SAFFL,
    AGE = sub$AGE,
    SEX = sub$SEX,
    RACE = sub$RACE,
    TRTSDT = sub$TRTSDT,
    TRTEDT = sub$TRTEDT,
    ASTDT = astdt,
    ASTDTF = start$flag,
    AENDT = aendt,
    AENDTF = end$flag,
    ASTDY = rel_day(astdt, sub$TRTSDT),
    AENDY = rel_day(aendt, sub$TRTSDT),
    ADURN = adurn,
    ADURU = ifelse(is.na(adurn), NA_character_, "DAYS"),
    TRTEMFL = trtem,
    PREFL = pre,
    AOCCIFL = NA_character_,
    AOCCPFL = NA_character_,
    AOCCSFL = NA_character_,
    SRCDOM = "AE",
    SRCSEQ = as_int(ae$AESEQ),
    stringsAsFactors = FALSE
  )
  # AEREL is kept as collected, including blank, while AREL is missing when blank.
  out$AEREL <- as_chr(ae$AEREL)
  out$AEREL[!nzchar(out$AEREL)] <- NA_character_
  flag_first <- function(group_cols) {
    ok <- which(out$TRTEMFL == "Y" & !is.na(out$ASTDT))
    if (!length(ok)) return(rep(NA_character_, nrow(out)))
    ord <- ok[do.call(order, c(lapply(group_cols, function(nm) out[[nm]][ok]),
                               list(out$ASTDT[ok], out$AESEQ[ok])))]
    key <- if (length(group_cols) == 1L) out[[group_cols]][ord] else do.call(paste, c(out[ord, group_cols, drop = FALSE], sep = "\r"))
    y <- rep(NA_character_, nrow(out))
    y[ord[!duplicated(key)]] <- "Y"
    y
  }
  out$AOCCIFL <- flag_first("USUBJID")
  out$AOCCPFL <- flag_first(c("USUBJID", "AEDECOD"))
  out$AOCCSFL <- flag_first(c("USUBJID", "AEBODSYS"))
  out[order(out$USUBJID, out$AESEQ), , drop = FALSE]
}

vs_param_map <- function() {
  data.frame(
    PARAMCD = c("SYSBP", "DIABP", "PULSE", "TEMP", "WEIGHT", "HEIGHT", "BMI"),
    PARAM = c(
      "Systolic Blood Pressure (mmHg)",
      "Diastolic Blood Pressure (mmHg)",
      "Pulse Rate (beats/min)",
      "Temperature (C)",
      "Weight (kg)",
      "Height (cm)",
      "Body Mass Index (kg/m2)"
    ),
    PARAMN = 1:7,
    AVALU = c("mmHg", "mmHg", "beats/min", "C", "kg", "cm", "kg/m2"),
    ANRLO = c(90, 60, 60, 36, NA, NA, 18.5),
    ANRHI = c(140, 90, 100, 37.5, NA, NA, 25),
    stringsAsFactors = FALSE
  )
}

anrind_of <- function(paramcd, aval, lo, hi) {
  out <- rep(NA_character_, length(aval))
  bmi <- paramcd == "BMI" & !is.na(aval)
  out[bmi & aval < 18.5] <- "LOW"
  out[bmi & aval >= 18.5 & aval < 25] <- "NORMAL"
  out[bmi & aval >= 25] <- "HIGH"
  use <- paramcd %in% c("SYSBP", "DIABP", "PULSE", "TEMP") & !is.na(aval) & !is.na(lo) & !is.na(hi)
  out[use & aval < lo] <- "LOW"
  out[use & aval > hi] <- "HIGH"
  out[use & aval >= lo & aval <= hi] <- "NORMAL"
  out
}

build_pharmaverse_advs <- function(adsl, dir = pharmaverse_sdtm_dir()) {
  .demo_pkgs()
  vs <- read_sdtm("vs", dir)
  map <- vs_param_map()
  collected_cd <- setdiff(map$PARAMCD, "BMI")
  raw <- data.frame(
    STUDYID = as_chr(vs$STUDYID),
    USUBJID = as_chr(vs$USUBJID),
    PARAMCD = as_chr(vs$VSTESTCD),
    AVAL = as_num(vs$VSSTRESN),
    ADT = parse_used_date(vs$VSDTC),
    AVISIT = blank_to_na(vs$VISIT),
    AVISITN = as_num(vs$VISITNUM),
    ATPT = blank_to_na(vs$VSTPT),
    ATPTN = as_num(vs$VSTPTNUM),
    VISIT = blank_to_na(vs$VISIT),
    VISITNUM = as_num(vs$VISITNUM),
    SRCSEQ = as_int(vs$VSSEQ),
    stringsAsFactors = FALSE
  )
  raw <- raw[raw$PARAMCD %in% collected_cd, , drop = FALSE]
  meta <- map[match(raw$PARAMCD, map$PARAMCD), , drop = FALSE]
  sub <- adsl[match(raw$USUBJID, adsl$USUBJID), , drop = FALSE]
  if (any(is.na(sub$USUBJID))) stop("VS subject missing from ADSL.", call. = FALSE)
  base <- data.frame(
    STUDYID = raw$STUDYID,
    USUBJID = raw$USUBJID,
    SUBJID = sub$SUBJID,
    SITEID = sub$SITEID,
    AGE = sub$AGE,
    SEX = sub$SEX,
    TRT01A = sub$TRT01A,
    TRT01AN = sub$TRT01AN,
    SAFFL = sub$SAFFL,
    TRTSDT = sub$TRTSDT,
    TRTEDT = sub$TRTEDT,
    PARAMN = meta$PARAMN,
    PARAMCD = raw$PARAMCD,
    PARAM = meta$PARAM,
    AVAL = raw$AVAL,
    AVALU = meta$AVALU,
    ADT = raw$ADT,
    ADY = NA_integer_,
    AVISIT = raw$AVISIT,
    AVISITN = raw$AVISITN,
    ATPT = raw$ATPT,
    ATPTN = raw$ATPTN,
    VISIT = raw$VISIT,
    VISITNUM = raw$VISITNUM,
    ABLFL = NA_character_,
    BASE = NA_real_,
    CHG = NA_real_,
    PCHG = NA_real_,
    ONTRTFL = NA_character_,
    ANRLO = meta$ANRLO,
    ANRHI = meta$ANRHI,
    ANRIND = NA_character_,
    DTYPE = NA_character_,
    SRCDOM = "VS",
    SRCVAR = "VSSTRESN",
    SRCSEQ = raw$SRCSEQ,
    stringsAsFactors = FALSE
  )
  bmi <- derive_bmi_rows(base)
  out <- rbind(base, bmi)
  out$ADY <- rel_day(out$ADT, out$TRTSDT)
  out$ABLFL <- flag_ablfl(out)
  out$BASE <- lookup_base(out)
  both <- !is.na(out$AVAL) & !is.na(out$BASE)
  out$CHG <- ifelse(both, out$AVAL - out$BASE, NA_real_)
  out$PCHG <- ifelse(both & out$BASE != 0, 100 * out$CHG / out$BASE, NA_real_)
  out$ONTRTFL <- flag_ontrt(out$ADT, out$TRTSDT, out$TRTEDT)
  out$ANRIND <- anrind_of(out$PARAMCD, out$AVAL, out$ANRLO, out$ANRHI)
  out <- out[order(out$USUBJID, out$PARAMN, out$AVISITN, out$ATPTN, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

derive_bmi_rows <- function(base) {
  wt <- base[base$PARAMCD == "WEIGHT" & !is.na(base$AVAL), , drop = FALSE]
  ht <- base[base$PARAMCD == "HEIGHT" & !is.na(base$AVAL) & base$AVAL > 0 & !is.na(base$ADT), , drop = FALSE]
  empty <- base[0, , drop = FALSE]
  if (!nrow(wt) || !nrow(ht)) return(empty)
  ht_same <- ht[, c("USUBJID", "AVISITN", "AVAL", "ADT", "SRCSEQ")]
  names(ht_same) <- c("USUBJID", "AVISITN", "HVAL", "HDT", "HSEQ")
  same <- merge(wt, ht_same, by = c("USUBJID", "AVISITN"), all = FALSE, sort = FALSE)
  if (nrow(same)) {
    same <- same[order(same$USUBJID, same$AVISITN, as.integer(same$HDT), same$HSEQ,
                       decreasing = c(FALSE, FALSE, TRUE, TRUE), method = "radix"), , drop = FALSE]
    same <- same[!duplicated(paste(same$USUBJID, same$AVISITN)), , drop = FALSE]
  }
  matched <- if (nrow(same)) paste(same$USUBJID, same$AVISITN) else character()
  left <- wt[!paste(wt$USUBJID, wt$AVISITN) %in% matched & !is.na(wt$ADT), , drop = FALSE]
  alt <- same[0, , drop = FALSE]
  if (nrow(left)) {
    hall <- ht_same[, c("USUBJID", "HVAL", "HDT", "HSEQ"), drop = FALSE]
    alt <- merge(left, hall, by = "USUBJID", all = FALSE, sort = FALSE)
    alt <- alt[!is.na(alt$HDT) & alt$HDT <= alt$ADT, , drop = FALSE]
    if (nrow(alt)) {
      alt <- alt[order(alt$USUBJID, as.integer(alt$ADT), alt$SRCSEQ, as.integer(alt$HDT), alt$HSEQ,
                       decreasing = c(FALSE, FALSE, FALSE, TRUE, TRUE), method = "radix"), , drop = FALSE]
      alt <- alt[!duplicated(paste(alt$USUBJID, alt$SRCSEQ)), , drop = FALSE]
    }
  }
  parts <- Filter(nrow, list(same, alt))
  if (!length(parts)) return(empty)
  paired <- do.call(rbind, parts)
  bmi_map <- vs_param_map()
  bmi_map <- bmi_map[bmi_map$PARAMCD == "BMI", , drop = FALSE]
  paired$PARAMCD <- "BMI"
  paired$PARAM <- bmi_map$PARAM
  paired$PARAMN <- bmi_map$PARAMN
  paired$AVALU <- bmi_map$AVALU
  paired$AVAL <- paired$AVAL / (paired$HVAL / 100) ^ 2
  paired$ATPT <- NA_character_
  paired$ATPTN <- NA_real_
  paired$ANRLO <- bmi_map$ANRLO
  paired$ANRHI <- bmi_map$ANRHI
  paired$DTYPE <- "DERIVED"
  paired$SRCDOM <- "ADVS"
  paired$SRCVAR <- "AVAL"
  paired$HVAL <- NULL
  paired$HDT <- NULL
  paired$HSEQ <- NULL
  paired[, names(base), drop = FALSE]
}

flag_ablfl <- function(df) {
  out <- rep("N", nrow(df))
  ok <- !is.na(df$AVAL) & !is.na(df$ADT) & !is.na(df$TRTSDT) & df$ADT <= df$TRTSDT
  if (!any(ok)) return(out)
  idx <- which(ok)
  key <- group_key3(df$USUBJID[idx], df$PARAMCD[idx], df$ATPTN[idx])
  ord <- idx[order(key, as.integer(df$ADT[idx]), df$AVISITN[idx], df$SRCSEQ[idx], method = "radix")]
  key_ord <- group_key3(df$USUBJID[ord], df$PARAMCD[ord], df$ATPTN[ord])
  win <- ord[!duplicated(key_ord, fromLast = TRUE)]
  out[win] <- "Y"
  out
}

lookup_base <- function(df) {
  y <- df$ABLFL == "Y"
  if (!any(y)) return(rep(NA_real_, nrow(df)))
  key_y <- group_key3(df$USUBJID[y], df$PARAMCD[y], df$ATPTN[y])
  if (anyDuplicated(key_y)) stop("More than one baseline row in a parameter time point.", call. = FALSE)
  val <- df$AVAL[y]
  names(val) <- key_y
  key <- group_key3(df$USUBJID, df$PARAMCD, df$ATPTN)
  as.numeric(unname(val[key]))
}

flag_ontrt <- function(adt, trtsdt, trtedt) {
  out <- rep(NA_character_, length(adt))
  miss_adt <- is.na(adt)
  miss_start <- !miss_adt & is.na(trtsdt)
  no_end <- !miss_adt & !is.na(trtsdt) & is.na(trtedt)
  both <- !miss_adt & !is.na(trtsdt) & !is.na(trtedt)
  out[miss_start] <- "N"
  out[no_end & adt >= trtsdt] <- "Y"
  out[no_end & adt < trtsdt] <- "N"
  out[both & adt >= trtsdt & adt <= trtedt] <- "Y"
  out[both & (adt < trtsdt | adt > trtedt)] <- "N"
  out
}

apply_spec_metadata <- function(df, var_df, dataset_label) {
  need <- var_df$Variable
  missing <- setdiff(need, names(df))
  extra <- setdiff(names(df), need)
  if (length(missing)) stop("Implementation is missing ", paste(missing, collapse = ", "), call. = FALSE)
  if (length(extra)) stop("Implementation has extra columns ", paste(extra, collapse = ", "), call. = FALSE)
  df <- df[, need, drop = FALSE]
  for (i in seq_len(nrow(var_df))) {
    nm <- var_df$Variable[[i]]
    type <- tolower(var_df$Type[[i]])
    len <- suppressWarnings(as.integer(var_df$Length[[i]]))
    x <- df[[nm]]
    if (identical(type, "char")) {
      x <- as.character(x)
      too <- !is.na(x) & nchar(x) > len
      if (any(too)) stop(nm, " exceeds length ", len, call. = FALSE)
      attr(x, "width") <- len
      attr(x, "display_width") <- len
    } else if (identical(type, "integer")) {
      if (inherits(x, "Date") || inherits(x, "POSIXt")) stop(nm, " is a date but the spec says integer.", call. = FALSE)
      x <- suppressWarnings(as.integer(x))
    } else if (identical(type, "float")) {
      x <- suppressWarnings(as.numeric(x))
    } else if (identical(type, "date")) {
      if (!inherits(x, "Date")) x <- as.Date(x)
    } else if (identical(type, "datetime")) {
      if (!inherits(x, "POSIXt")) stop(nm, " is not a datetime.", call. = FALSE)
      x <- as.POSIXct(x, tz = "UTC")
    } else {
      stop("Unknown spec type for ", nm, ": ", type, call. = FALSE)
    }
    lab <- var_df$Label[[i]]
    if (nchar(lab) > 40L) stop("Label longer than 40 for ", nm, ": ", lab, call. = FALSE)
    attr(x, "label") <- lab
    df[[nm]] <- x
  }
  attr(df, "label") <- dataset_label
  df
}

read_demo_variable_csv <- function(ds, spec_dir = pharmaverse_demo_spec_dir()) {
  nested <- file.path(spec_dir, tolower(ds), "variable.csv")
  path <- if (file.exists(nested)) nested else file.path(spec_dir, "variable.csv")
  df <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
                        na.strings = character(), check.names = FALSE, comment.char = "")
  df$Variable <- toupper(trimws(df$Variable))
  df
}

write_demo_xpt <- function(df, dataset, label, out_dir = pharmaverse_demo_out_dir()) {
  dest_dir <- out_dir
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dest_dir, paste0(tolower(dataset), ".xpt"))
  haven::write_xpt(df, path, version = 5, name = dataset, label = label)
  path
}

assert_unique_keys <- function(df, keys, label) {
  if (anyDuplicated(df[keys])) stop(label, " keys are not unique: ", paste(keys, collapse = ", "), call. = FALSE)
}

build_pharmaverse_demo_adsl <- function(dir = pharmaverse_sdtm_dir(),
                                        spec_dir = pharmaverse_demo_spec_dir(),
                                        out_dir = pharmaverse_demo_out_dir()) {
  adsl_var <- read_demo_variable_csv("ADSL", spec_dir)
  adsl <- apply_spec_metadata(build_pharmaverse_adsl(dir), adsl_var, "Subject-Level Analysis Dataset")
  assert_unique_keys(adsl, "USUBJID", "ADSL")
  write_demo_xpt(adsl, "ADSL", "Subject-Level Analysis Dataset", out_dir)
}

run_pharmaverse_demo_ref <- function(dir = pharmaverse_sdtm_dir(),
                                    spec_dir = pharmaverse_demo_spec_dir(),
                                    out_dir = pharmaverse_demo_out_dir()) {
  adsl_var <- read_demo_variable_csv("ADSL", spec_dir)
  adae_var <- read_demo_variable_csv("ADAE", spec_dir)
  advs_var <- read_demo_variable_csv("ADVS", spec_dir)
  adsl <- apply_spec_metadata(build_pharmaverse_adsl(dir), adsl_var, "Subject-Level Analysis Dataset")
  adae <- apply_spec_metadata(build_pharmaverse_adae(adsl, dir), adae_var, "Adverse Event Analysis Dataset")
  advs <- apply_spec_metadata(build_pharmaverse_advs(adsl, dir), advs_var, "Vital Signs Analysis Dataset")
  assert_unique_keys(adsl, "USUBJID", "ADSL")
  assert_unique_keys(adae, c("USUBJID", "AESEQ"), "ADAE")
  assert_unique_keys(advs, c("USUBJID", "PARAMCD", "AVISITN", "ATPTN"), "ADVS")
  paths <- c(
    write_demo_xpt(adsl, "ADSL", "Subject-Level Analysis Dataset", out_dir),
    write_demo_xpt(adae, "ADAE", "Adverse Event Analysis Dataset", out_dir),
    write_demo_xpt(advs, "ADVS", "Vital Signs Analysis Dataset", out_dir)
  )
  message("ADSL ", nrow(adsl), " rows, ADAE ", nrow(adae), " rows, ADVS ", nrow(advs), " rows")
  message(paste(paths, collapse = "\n"))
  invisible(list(adsl = adsl, adae = adae, advs = advs, paths = paths))
}
# Spec tables for examples/pharmaverse-demo. Sourced only as part of
# tools/pharmaverse_demo_ref.R (appended below the reference builder).

demo_spec_row <- function(variable, label, type, length, key = "", source = "", derivation = "") {
  data.frame(
    Variable = variable, Label = label, Type = type, Length = as.character(length),
    Key = if (!nzchar(as.character(key))) "" else as.character(key),
    Source = source, Derivation = derivation, stringsAsFactors = FALSE
  )
}

demo_bind_vars <- function(ds, rows) {
  df <- do.call(rbind, rows)
  df <- cbind(Dataset = ds, df, stringsAsFactors = FALSE)
  df <- df[, c("Dataset", "Variable", "Label", "Type", "Length", "Key", "Source", "Derivation")]
  rownames(df) <- NULL
  dup <- df$Variable[duplicated(df$Variable)]
  if (length(dup)) stop(ds, " duplicate variables: ", paste(dup, collapse = ", "), call. = FALSE)
  if (any(nchar(df$Label) > 40L)) {
    bad <- df$Variable[nchar(df$Label) > 40L]
    stop(ds, " label longer than 40: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  df
}

demo_bind_params <- function(ds, rows) {
  if (!length(rows)) {
    return(data.frame(Dataset = character(), PARAMCD = character(), PARAM = character(),
                      Source = character(), Derivation = character(), stringsAsFactors = FALSE))
  }
  df <- do.call(rbind, rows)
  df <- cbind(Dataset = ds, df, stringsAsFactors = FALSE)
  df <- df[, c("Dataset", "PARAMCD", "PARAM", "Source", "Derivation")]
  rownames(df) <- NULL
  df
}

demo_param_row <- function(paramcd, param, source, derivation) {
  data.frame(PARAMCD = paramcd, PARAM = param, Source = source, Derivation = derivation,
             stringsAsFactors = FALSE)
}

pharmaverse_adsl_variables <- function() {
  demo_bind_vars("ADSL", list(
    demo_spec_row("STUDYID", "Study Identifier", "Char", 20, "", "DM.STUDYID",
                  "Copy DM.STUDYID."),
    demo_spec_row("USUBJID", "Unique Subject Identifier", "Char", 20, "1", "DM.USUBJID",
                  "Copy DM.USUBJID. One row per DM.USUBJID."),
    demo_spec_row("SUBJID", "Subject Identifier for the Study", "Char", 8, "", "DM.SUBJID",
                  "Copy DM.SUBJID."),
    demo_spec_row("SITEID", "Study Site Identifier", "Char", 8, "", "DM.SITEID",
                  "Copy DM.SITEID."),
    demo_spec_row("AGE", "Age", "Integer", 8, "", "DM.AGE",
                  "Copy DM.AGE. DM.AGEU is years for every subject."),
    demo_spec_row("AGEU", "Age Units", "Char", 10, "", "DM.AGEU",
                  "Copy DM.AGEU."),
    demo_spec_row("AGEGR1", "Pooled Age Group 1", "Char", 10, "", "ADSL.AGE",
                  "Set AGEGR1 from ADSL.AGE. Use <65 when ADSL.AGE is below 65. Use 65-80 when ADSL.AGE is from 65 through 80 inclusive. Use >80 when ADSL.AGE is above 80."),
    demo_spec_row("AGEGR1N", "Pooled Age Group 1 (N)", "Integer", 8, "", "ADSL.AGEGR1",
                  "Set AGEGR1N from ADSL.AGEGR1. The group <65 has code 1, the group 65-80 has code 2, and the group >80 has code 3."),
    demo_spec_row("SEX", "Sex", "Char", 1, "", "DM.SEX",
                  "Copy DM.SEX."),
    demo_spec_row("RACE", "Race", "Char", 40, "", "DM.RACE",
                  "Copy DM.RACE."),
    demo_spec_row("RACEN", "Race (N)", "Integer", 8, "", "ADSL.RACE",
                  "Set RACEN from ADSL.RACE. WHITE has code 1. BLACK OR AFRICAN AMERICAN has code 2. ASIAN has code 3. AMERICAN INDIAN OR ALASKA NATIVE has code 4. Leave RACEN missing for any other value."),
    demo_spec_row("ETHNIC", "Ethnicity", "Char", 40, "", "DM.ETHNIC",
                  "Copy DM.ETHNIC."),
    demo_spec_row("COUNTRY", "Country", "Char", 3, "", "DM.COUNTRY",
                  "Copy DM.COUNTRY."),
    demo_spec_row("ARM", "Description of Planned Arm", "Char", 40, "", "DM.ARM",
                  "Copy DM.ARM."),
    demo_spec_row("ACTARM", "Description of Actual Arm", "Char", 40, "", "DM.ACTARM",
                  "Copy DM.ACTARM."),
    demo_spec_row("TRT01P", "Planned Treatment for Period 01", "Char", 40, "", "DM.ARM",
                  "Copy DM.ARM. Screen Failure is retained as its own planned value."),
    demo_spec_row("TRT01PN", "Planned Treatment for Period 01 (N)", "Integer", 8, "", "ADSL.TRT01P",
                  "Set TRT01PN from ADSL.TRT01P. Placebo has code 0. Xanomeline Low Dose has code 1. Xanomeline High Dose has code 2. Screen Failure has code 3. Leave TRT01PN missing for any other value."),
    demo_spec_row("TRT01A", "Actual Treatment for Period 01", "Char", 40, "", "DM.ACTARM",
                  "Copy DM.ACTARM. This differs from ADSL.TRT01P when the subject was treated with a different regimen from the planned regimen."),
    demo_spec_row("TRT01AN", "Actual Treatment for Period 01 (N)", "Integer", 8, "", "ADSL.TRT01A",
                  "Set TRT01AN from ADSL.TRT01A using the same codes as ADSL.TRT01PN. Placebo has code 0. Xanomeline Low Dose has code 1. Xanomeline High Dose has code 2. Screen Failure has code 3."),
    demo_spec_row("TRTSDTM", "Datetime of First Exposure to Treatment", "Datetime", 8, "",
                  "EX.EXSTDTC; EX.EXDOSE; EX.EXTRT; EX.EXSEQ",
                  "Keep EX rows for the subject where EX.EXDOSE is greater than zero or EX.EXTRT equals PLACEBO, and EX.EXSTDTC is a complete calendar day. A complete calendar day is four digits, a hyphen, two digits, a hyphen, and two digits, with nothing after the day. Sort qualifying rows by EX.EXSTDTC ascending and then by EX.EXSEQ ascending. The first row supplies the calendar day. EX.EXSTDTC contains no clock time, so set the hour, minute, and second to 00:00:00. Leave TRTSDTM missing when the subject has no qualifying row."),
    demo_spec_row("TRTSDT", "Date of First Exposure to Treatment", "Date", 8, "", "ADSL.TRTSDTM",
                  "Set TRTSDT to the calendar day of ADSL.TRTSDTM. Leave TRTSDT missing when ADSL.TRTSDTM is missing."),
    demo_spec_row("TRTSDTF", "Date Imputation Flag for TRTSDT", "Char", 1, "", "EX.EXSTDTC; ADSL.TRTSDTM",
                  "Leave TRTSDTF blank. A qualifying EX.EXSTDTC is a complete calendar day, so the day, month, and year on ADSL.TRTSDTM are not imputed. The flag would be D when only the day is imputed, M when the month is imputed, and Y when the year is imputed."),
    demo_spec_row("TRTSTMF", "Time Imputation Flag for TRTSDTM", "Char", 1, "", "ADSL.TRTSDTM",
                  "Set TRTSTMF to H when ADSL.TRTSDTM is not missing, because the clock time was imputed to 00:00:00. Leave TRTSTMF blank when ADSL.TRTSDTM is missing. H means the hour and the smaller time parts were imputed together."),
    demo_spec_row("TRTEDTM", "Datetime of Last Exposure to Treatment", "Datetime", 8, "",
                  "EX.EXENDTC; EX.EXSTDTC; EX.EXDOSE; EX.EXTRT; EX.EXSEQ",
                  "Keep EX rows for the subject where EX.EXDOSE is greater than zero or EX.EXTRT equals PLACEBO. For each such row the candidate calendar day is EX.EXENDTC when EX.EXENDTC is a complete calendar day. When EX.EXENDTC is blank, the candidate is EX.EXSTDTC when EX.EXSTDTC is a complete calendar day. Ignore a row with no candidate. Choose the latest candidate day and break a tie with the greatest EX.EXSEQ. The source has no clock time, so set the hour, minute, and second to 23:59:59. Leave TRTEDTM missing when the subject has no candidate day."),
    demo_spec_row("TRTEDT", "Date of Last Exposure to Treatment", "Date", 8, "", "ADSL.TRTEDTM",
                  "Set TRTEDT to the calendar day of ADSL.TRTEDTM. Leave TRTEDT missing when ADSL.TRTEDTM is missing."),
    demo_spec_row("TRTEDTF", "Date Imputation Flag for TRTEDT", "Char", 1, "", "EX.EXENDTC; ADSL.TRTEDTM",
                  "Leave TRTEDTF blank. The candidate day taken from EX.EXENDTC or EX.EXSTDTC is a complete calendar day, so the day, month, and year on ADSL.TRTEDTM are not imputed. The flag would be D when only the day is imputed, M when the month is imputed, and Y when the year is imputed."),
    demo_spec_row("TRTETMF", "Time Imputation Flag for TRTEDTM", "Char", 1, "", "ADSL.TRTEDTM",
                  "Set TRTETMF to H when ADSL.TRTEDTM is not missing, because the clock time was imputed to 23:59:59. Leave TRTETMF blank when ADSL.TRTEDTM is missing."),
    demo_spec_row("TRTDURD", "Total Treatment Duration (Days)", "Integer", 8, "", "ADSL.TRTSDT; ADSL.TRTEDT",
                  "When ADSL.TRTSDT and ADSL.TRTEDT are both non-missing, set TRTDURD to the number of calendar days from ADSL.TRTSDT through ADSL.TRTEDT, which is ADSL.TRTEDT minus ADSL.TRTSDT plus 1. Otherwise leave TRTDURD missing."),
    demo_spec_row("RANDDT", "Date of Randomization", "Date", 8, "", "DS.DSCAT; DS.DSDECOD; DS.DSSTDTC; DS.DSSEQ",
                  "Use DS rows where DS.DSCAT equals PROTOCOL MILESTONE and DS.DSDECOD equals RANDOMIZED. If more than one row matches the subject, keep the earliest DS.DSSTDTC and then the smallest DS.DSSEQ. Set RANDDT to DS.DSSTDTC when that value is a complete calendar day. Leave RANDDT missing when the subject has no such row."),
    demo_spec_row("SAFFL", "Safety Population Flag", "Char", 1, "", "EX.EXDOSE; EX.EXTRT; EX.USUBJID",
                  "Set SAFFL to Y when the subject has at least one EX row where EX.EXDOSE is greater than zero or EX.EXTRT equals PLACEBO. Otherwise set SAFFL to N."),
    demo_spec_row("ITTFL", "Intent-to-Treat Population Flag", "Char", 1, "", "SUPPDM.QNAM; SUPPDM.QVAL; SUPPDM.USUBJID",
                  "Set ITTFL to Y when a SUPPDM row for SUPPDM.USUBJID has SUPPDM.QNAM equal to ITT and SUPPDM.QVAL equal to Y. Otherwise set ITTFL to N."),
    demo_spec_row("EFFFL", "Efficacy Population Flag", "Char", 1, "", "SUPPDM.QNAM; SUPPDM.QVAL; SUPPDM.USUBJID",
                  "Set EFFFL to Y when a SUPPDM row for SUPPDM.USUBJID has SUPPDM.QNAM equal to EFFICACY and SUPPDM.QVAL equal to Y. Otherwise set EFFFL to N."),
    demo_spec_row("COMP8FL", "Completers of Week 8 Population Flag", "Char", 1, "", "SUPPDM.QNAM; SUPPDM.QVAL; SUPPDM.USUBJID",
                  "Set COMP8FL to Y when a SUPPDM row for SUPPDM.USUBJID has SUPPDM.QNAM equal to COMPLT8 and SUPPDM.QVAL equal to Y. Otherwise set COMP8FL to N."),
    demo_spec_row("COMP16FL", "Completers of Week 16 Population Flag", "Char", 1, "", "SUPPDM.QNAM; SUPPDM.QVAL; SUPPDM.USUBJID",
                  "Set COMP16FL to Y when a SUPPDM row for SUPPDM.USUBJID has SUPPDM.QNAM equal to COMPLT16 and SUPPDM.QVAL equal to Y. Otherwise set COMP16FL to N."),
    demo_spec_row("COMP24FL", "Completers of Week 24 Population Flag", "Char", 1, "", "SUPPDM.QNAM; SUPPDM.QVAL; SUPPDM.USUBJID",
                  "Set COMP24FL to Y when a SUPPDM row for SUPPDM.USUBJID has SUPPDM.QNAM equal to COMPLT24 and SUPPDM.QVAL equal to Y. Otherwise set COMP24FL to N."),
    demo_spec_row("DTHFL", "Subject Death Flag", "Char", 1, "", "DM.DTHFL",
                  "Set DTHFL to Y when DM.DTHFL equals Y. Otherwise set DTHFL to N."),
    demo_spec_row("DTHDT", "Date of Death", "Date", 8, "", "DM.DTHDTC",
                  "Set DTHDT to DM.DTHDTC when DM.DTHDTC is a complete calendar day. Otherwise leave DTHDT missing."),
    demo_spec_row("EOSSTT", "End of Study Status", "Char", 20, "", "DS.DSCAT; DS.DSDECOD; DS.DSSTDTC; DS.DSSEQ",
                  "Use the DS row where DS.DSCAT equals DISPOSITION EVENT. If more than one row matches the subject, keep the earliest DS.DSSTDTC and then the smallest DS.DSSEQ. Set EOSSTT to COMPLETED when DS.DSDECOD equals COMPLETED. Otherwise set EOSSTT to DISCONTINUED. Leave EOSSTT missing when the subject has no disposition row."),
    demo_spec_row("DCSREAS", "Reason for Discontinuation from Study", "Char", 40, "", "ADSL.EOSSTT; DS.DSCAT; DS.DSDECOD; DS.DSSTDTC; DS.DSSEQ",
                  "Use the same DS row as ADSL.EOSSTT, where DS.DSCAT equals DISPOSITION EVENT, choosing the earliest DS.DSSTDTC and then the smallest DS.DSSEQ when more than one row matches. When ADSL.EOSSTT equals DISCONTINUED, set DCSREAS to DS.DSDECOD. Otherwise leave DCSREAS blank."),
    demo_spec_row("RFENDT", "Subject Reference End Date", "Date", 8, "", "DM.RFENDTC",
                  "Set RFENDT to DM.RFENDTC when DM.RFENDTC is a complete calendar day. Otherwise leave RFENDT missing."),
    demo_spec_row("LSTALVDT", "Date Last Known Alive", "Date", 8, "",
                  "DM.RFPENDTC; DM.RFENDTC; DM.DTHDTC; DS.DSSTDTC; EX.EXSTDTC; EX.EXENDTC; AE.AESTDTC; AE.AEENDTC; VS.VSDTC; LB.LBDTC; SV.SVSTDTC; SV.SVENDTC; CM.CMSTDTC; CM.CMENDTC; MH.MHSTDTC; MH.MHENDTC",
                  "Set LSTALVDT to the latest usable calendar day across DM.RFPENDTC, DM.RFENDTC, DM.DTHDTC, DS.DSSTDTC, EX.EXSTDTC, EX.EXENDTC, AE.AESTDTC, AE.AEENDTC, VS.VSDTC, LB.LBDTC, SV.SVSTDTC, SV.SVENDTC, CM.CMSTDTC, CM.CMENDTC, MH.MHSTDTC, and MH.MHENDTC. A value is usable when it is a complete calendar day, or a complete calendar day followed by the letter T and a clock time. For a value with a clock time, keep only the calendar day. Ignore blanks and partial values that do not match those two patterns. Leave LSTALVDT missing when the subject has no usable day."),
    demo_spec_row("HEIGHTBL", "Baseline Height (cm)", "Float", 8, "", "VS.VSTESTCD; VS.VSSTRESN; VS.VSDTC; VS.VSSEQ; ADSL.TRTSDT",
                  "Consider VS rows with VS.VSTESTCD equal to HEIGHT and VS.VSSTRESN not missing. Keep a row only when VS.VSDTC is a usable calendar day on or before ADSL.TRTSDT. Choose the latest VS.VSDTC and break a tie with the greatest VS.VSSEQ. HEIGHTBL is the VS.VSSTRESN from that row, in centimetres. Leave HEIGHTBL missing when ADSL.TRTSDT is missing or no row qualifies."),
    demo_spec_row("WEIGHTBL", "Baseline Weight (kg)", "Float", 8, "", "VS.VSTESTCD; VS.VSSTRESN; VS.VSDTC; VS.VSSEQ; ADSL.TRTSDT",
                  "Consider VS rows with VS.VSTESTCD equal to WEIGHT and VS.VSSTRESN not missing. Keep a row only when VS.VSDTC is a usable calendar day on or before ADSL.TRTSDT. Choose the latest VS.VSDTC and break a tie with the greatest VS.VSSEQ. WEIGHTBL is the VS.VSSTRESN from that row, in kilograms. Leave WEIGHTBL missing when ADSL.TRTSDT is missing or no row qualifies."),
    demo_spec_row("BMIBL", "Baseline BMI (kg/m2)", "Float", 8, "", "ADSL.WEIGHTBL; ADSL.HEIGHTBL",
                  "When ADSL.WEIGHTBL and ADSL.HEIGHTBL are both non-missing and ADSL.HEIGHTBL is greater than zero, set BMIBL to ADSL.WEIGHTBL divided by the square of (ADSL.HEIGHTBL divided by 100). Do not round. Otherwise leave BMIBL missing. The unit is kilograms per metre squared.")
  ))
}

pharmaverse_adae_variables <- function() {
  demo_bind_vars("ADAE", list(
    demo_spec_row("STUDYID", "Study Identifier", "Char", 20, "", "AE.STUDYID", "Copy AE.STUDYID."),
    demo_spec_row("USUBJID", "Unique Subject Identifier", "Char", 20, "1", "AE.USUBJID", "Copy AE.USUBJID."),
    demo_spec_row("SUBJID", "Subject Identifier for the Study", "Char", 8, "", "ADSL.SUBJID; ADSL.USUBJID",
                  "Copy ADSL.SUBJID for the same ADSL.USUBJID."),
    demo_spec_row("SITEID", "Study Site Identifier", "Char", 8, "", "ADSL.SITEID; ADSL.USUBJID",
                  "Copy ADSL.SITEID for the same ADSL.USUBJID."),
    demo_spec_row("AESEQ", "Sequence Number", "Integer", 8, "2", "AE.AESEQ", "Copy AE.AESEQ."),
    demo_spec_row("AETERM", "Reported Term for the Adverse Event", "Char", 200, "", "AE.AETERM", "Copy AE.AETERM."),
    demo_spec_row("AEDECOD", "Dictionary-Derived Term", "Char", 200, "", "AE.AEDECOD", "Copy AE.AEDECOD."),
    demo_spec_row("AEBODSYS", "Body System or Organ Class", "Char", 200, "", "AE.AEBODSYS", "Copy AE.AEBODSYS."),
    demo_spec_row("AELLT", "Lowest Level Term", "Char", 200, "", "AE.AELLT", "Copy AE.AELLT."),
    demo_spec_row("AESEV", "Severity/Intensity", "Char", 8, "", "AE.AESEV", "Copy AE.AESEV."),
    demo_spec_row("ASEV", "Analysis Severity/Intensity", "Char", 8, "", "AE.AESEV",
                  "Copy AE.AESEV. The collected values are MILD, MODERATE, and SEVERE."),
    demo_spec_row("ASEVN", "Analysis Severity/Intensity (N)", "Integer", 8, "", "ADAE.ASEV",
                  "Set ASEVN from ADAE.ASEV. MILD has code 1, MODERATE has code 2, and SEVERE has code 3. Leave ASEVN missing for any other value."),
    demo_spec_row("AEREL", "Causality", "Char", 20, "", "AE.AEREL",
                  "Copy AE.AEREL. Leave AEREL missing when AE.AEREL is blank."),
    demo_spec_row("AREL", "Analysis Causality", "Char", 20, "", "AE.AEREL",
                  "Copy AE.AEREL. Leave AREL missing when AE.AEREL is blank. Non-missing values stay NONE, REMOTE, POSSIBLE, or PROBABLE."),
    demo_spec_row("ARELN", "Analysis Causality (N)", "Integer", 8, "", "ADAE.AREL",
                  "Set ARELN from ADAE.AREL. NONE has code 1, REMOTE has code 2, POSSIBLE has code 3, and PROBABLE has code 4. Leave ARELN missing when ADAE.AREL is missing or has any other value."),
    demo_spec_row("AESER", "Serious Event", "Char", 1, "", "AE.AESER", "Copy AE.AESER."),
    demo_spec_row("AEOUT", "Outcome of Adverse Event", "Char", 40, "", "AE.AEOUT", "Copy AE.AEOUT."),
    demo_spec_row("AESDTH", "Results in Death", "Char", 1, "", "AE.AESDTH", "Copy AE.AESDTH."),
    demo_spec_row("TRT01A", "Actual Treatment for Period 01", "Char", 40, "", "ADSL.TRT01A; ADSL.USUBJID",
                  "Copy ADSL.TRT01A for the same ADSL.USUBJID."),
    demo_spec_row("TRT01AN", "Actual Treatment for Period 01 (N)", "Integer", 8, "", "ADSL.TRT01AN; ADSL.USUBJID",
                  "Copy ADSL.TRT01AN for the same ADSL.USUBJID."),
    demo_spec_row("SAFFL", "Safety Population Flag", "Char", 1, "", "ADSL.SAFFL; ADSL.USUBJID",
                  "Copy ADSL.SAFFL for the same ADSL.USUBJID."),
    demo_spec_row("AGE", "Age", "Integer", 8, "", "ADSL.AGE; ADSL.USUBJID",
                  "Copy ADSL.AGE for the same ADSL.USUBJID."),
    demo_spec_row("SEX", "Sex", "Char", 1, "", "ADSL.SEX; ADSL.USUBJID",
                  "Copy ADSL.SEX for the same ADSL.USUBJID."),
    demo_spec_row("RACE", "Race", "Char", 40, "", "ADSL.RACE; ADSL.USUBJID",
                  "Copy ADSL.RACE for the same ADSL.USUBJID."),
    demo_spec_row("TRTSDT", "Date of First Exposure to Treatment", "Date", 8, "", "ADSL.TRTSDT; ADSL.USUBJID",
                  "Copy ADSL.TRTSDT for the same ADSL.USUBJID."),
    demo_spec_row("TRTEDT", "Date of Last Exposure to Treatment", "Date", 8, "", "ADSL.TRTEDT; ADSL.USUBJID",
                  "Copy ADSL.TRTEDT for the same ADSL.USUBJID."),
    demo_spec_row("ASTDT", "Analysis Start Date", "Date", 8, "", "AE.AESTDTC; ADSL.TRTSDT",
                  "Impute ADAE.ASTDT from AE.AESTDTC. When AE.AESTDTC is a complete calendar day, use that day and do not impute. When AE.AESTDTC is a four digit year, a hyphen, and a two digit month, start at day 01 of that month. If ADSL.TRTSDT falls in that same month and day 01 is before ADSL.TRTSDT, move the imputed day forward to ADSL.TRTSDT. When AE.AESTDTC is a four digit year only, start at 01 January of that year. If ADSL.TRTSDT falls in that same year and 01 January is before ADSL.TRTSDT, move the imputed day forward to ADSL.TRTSDT. Leave ASTDT missing when AE.AESTDTC is blank or does not match one of those patterns. Do not impute a missing year."),
    demo_spec_row("ASTDTF", "Analysis Start Date Imput. Flag", "Char", 1, "", "AE.AESTDTC; ADSL.TRTSDT",
                  "Set ASTDTF from the same imputation as ADAE.ASTDT. Use D when the day was missing and a day was imputed. Use M when the month was missing and a month was imputed. Leave ASTDTF blank when AE.AESTDTC was a complete calendar day or when ADAE.ASTDT is missing. Moving an imputed day forward to ADSL.TRTSDT does not change D or M."),
    demo_spec_row("AENDT", "Analysis End Date", "Date", 8, "", "AE.AEENDTC; ADSL.TRTEDT",
                  "Impute ADAE.AENDT from AE.AEENDTC. When AE.AEENDTC is a complete calendar day, use that day. When AE.AEENDTC is a year and month, use the last calendar day of that month. If ADSL.TRTEDT falls in that same month and the last day is after ADSL.TRTEDT, move the imputed day back to ADSL.TRTEDT. When AE.AEENDTC is a year only, use 31 December of that year. If ADSL.TRTEDT falls in that same year and 31 December is after ADSL.TRTEDT, move the imputed day back to ADSL.TRTEDT. Leave AENDT missing when AE.AEENDTC is blank or does not match one of those patterns."),
    demo_spec_row("AENDTF", "Analysis End Date Imputation Flag", "Char", 1, "", "AE.AEENDTC; ADSL.TRTEDT",
                  "Set AENDTF from the same imputation as ADAE.AENDT. Use D when the day was missing and a day was imputed. Use M when the month was missing and a month was imputed. Leave AENDTF blank when AE.AEENDTC was a complete calendar day or when ADAE.AENDT is missing."),
    demo_spec_row("ASTDY", "Analysis Start Relative Day", "Integer", 8, "", "ADAE.ASTDT; ADSL.TRTSDT",
                  "Leave ASTDY missing when ADAE.ASTDT or ADSL.TRTSDT is missing. When ADAE.ASTDT is on or after ADSL.TRTSDT, subtract ADSL.TRTSDT from ADAE.ASTDT and add 1. When ADAE.ASTDT is before ADSL.TRTSDT, subtract ADSL.TRTSDT from ADAE.ASTDT and do not add 1."),
    demo_spec_row("AENDY", "Analysis End Relative Day", "Integer", 8, "", "ADAE.AENDT; ADSL.TRTSDT",
                  "Leave AENDY missing when ADAE.AENDT or ADSL.TRTSDT is missing. When ADAE.AENDT is on or after ADSL.TRTSDT, subtract ADSL.TRTSDT from ADAE.AENDT and add 1. When ADAE.AENDT is before ADSL.TRTSDT, subtract ADSL.TRTSDT from ADAE.AENDT and do not add 1."),
    demo_spec_row("ADURN", "AE Duration (N)", "Integer", 8, "", "ADAE.AENDT; ADAE.ASTDT",
                  "When ADAE.ASTDT and ADAE.AENDT are both non-missing and ADAE.AENDT is on or after ADAE.ASTDT, set ADURN to ADAE.AENDT minus ADAE.ASTDT plus 1. Otherwise leave ADURN missing."),
    demo_spec_row("ADURU", "AE Duration Units", "Char", 4, "", "ADAE.ADURN",
                  "Set ADURU to DAYS when ADAE.ADURN is not missing. Otherwise leave ADURU blank."),
    demo_spec_row("TRTEMFL", "Treatment Emergent Analysis Flag", "Char", 1, "", "ADAE.ASTDT; ADSL.TRTSDT; ADSL.TRTEDT",
                  "Set TRTEMFL to Y when ADAE.ASTDT is not missing, ADSL.TRTSDT is not missing, ADAE.ASTDT is on or after ADSL.TRTSDT, and either ADSL.TRTEDT is missing or ADAE.ASTDT is on or before ADSL.TRTEDT plus 30 calendar days. Otherwise set TRTEMFL to N."),
    demo_spec_row("PREFL", "Pre-treatment Flag", "Char", 1, "", "ADAE.ASTDT; ADSL.TRTSDT",
                  "Set PREFL to Y when ADAE.ASTDT and ADSL.TRTSDT are both non-missing and ADAE.ASTDT is before ADSL.TRTSDT. Otherwise set PREFL to N."),
    demo_spec_row("AOCCIFL", "1st Occurrence of Any AE Flag", "Char", 1, "", "ADAE.TRTEMFL; ADAE.ASTDT; ADAE.AESEQ; ADAE.USUBJID",
                  "Consider rows with ADAE.TRTEMFL equal to Y and ADAE.ASTDT not missing. Within each ADAE.USUBJID, sort by ADAE.ASTDT ascending and then by ADAE.AESEQ ascending. Set AOCCIFL to Y on the first row. Leave AOCCIFL blank on every other row, including rows that are not treatment emergent."),
    demo_spec_row("AOCCPFL", "1st Occurrence of Preferred Term Flag", "Char", 1, "", "ADAE.TRTEMFL; ADAE.ASTDT; ADAE.AESEQ; ADAE.USUBJID; ADAE.AEDECOD",
                  "Consider rows with ADAE.TRTEMFL equal to Y and ADAE.ASTDT not missing. Within each pair of ADAE.USUBJID and ADAE.AEDECOD, sort by ADAE.ASTDT ascending and then by ADAE.AESEQ ascending. Set AOCCPFL to Y on the first row. Leave AOCCPFL blank on every other row."),
    demo_spec_row("AOCCSFL", "1st Occurrence of SOC Flag", "Char", 1, "", "ADAE.TRTEMFL; ADAE.ASTDT; ADAE.AESEQ; ADAE.USUBJID; ADAE.AEBODSYS",
                  "Consider rows with ADAE.TRTEMFL equal to Y and ADAE.ASTDT not missing. Within each pair of ADAE.USUBJID and ADAE.AEBODSYS, sort by ADAE.ASTDT ascending and then by ADAE.AESEQ ascending. Set AOCCSFL to Y on the first row. Leave AOCCSFL blank on every other row."),
    demo_spec_row("SRCDOM", "Source Data", "Char", 8, "", "AE.DOMAIN; AE.AESEQ",
                  "Set SRCDOM to AE. Each row comes from AE and is traced by AE.AESEQ."),
    demo_spec_row("SRCSEQ", "Source Sequence Number", "Integer", 8, "", "AE.AESEQ",
                  "Copy AE.AESEQ.")
  ))
}

pharmaverse_advs_variables <- function() {
  demo_bind_vars("ADVS", list(
    demo_spec_row("STUDYID", "Study Identifier", "Char", 20, "", "VS.STUDYID", "Copy VS.STUDYID."),
    demo_spec_row("USUBJID", "Unique Subject Identifier", "Char", 20, "1", "VS.USUBJID", "Copy VS.USUBJID."),
    demo_spec_row("SUBJID", "Subject Identifier for the Study", "Char", 8, "", "ADSL.SUBJID; ADSL.USUBJID",
                  "Copy ADSL.SUBJID for the same ADSL.USUBJID."),
    demo_spec_row("SITEID", "Study Site Identifier", "Char", 8, "", "ADSL.SITEID; ADSL.USUBJID",
                  "Copy ADSL.SITEID for the same ADSL.USUBJID."),
    demo_spec_row("AGE", "Age", "Integer", 8, "", "ADSL.AGE; ADSL.USUBJID",
                  "Copy ADSL.AGE for the same ADSL.USUBJID."),
    demo_spec_row("SEX", "Sex", "Char", 1, "", "ADSL.SEX; ADSL.USUBJID",
                  "Copy ADSL.SEX for the same ADSL.USUBJID."),
    demo_spec_row("TRT01A", "Actual Treatment for Period 01", "Char", 40, "", "ADSL.TRT01A; ADSL.USUBJID",
                  "Copy ADSL.TRT01A for the same ADSL.USUBJID."),
    demo_spec_row("TRT01AN", "Actual Treatment for Period 01 (N)", "Integer", 8, "", "ADSL.TRT01AN; ADSL.USUBJID",
                  "Copy ADSL.TRT01AN for the same ADSL.USUBJID."),
    demo_spec_row("SAFFL", "Safety Population Flag", "Char", 1, "", "ADSL.SAFFL; ADSL.USUBJID",
                  "Copy ADSL.SAFFL for the same ADSL.USUBJID."),
    demo_spec_row("TRTSDT", "Date of First Exposure to Treatment", "Date", 8, "", "ADSL.TRTSDT; ADSL.USUBJID",
                  "Copy ADSL.TRTSDT for the same ADSL.USUBJID."),
    demo_spec_row("TRTEDT", "Date of Last Exposure to Treatment", "Date", 8, "", "ADSL.TRTEDT; ADSL.USUBJID",
                  "Copy ADSL.TRTEDT for the same ADSL.USUBJID."),
    demo_spec_row("PARAMN", "Parameter (N)", "Integer", 8, "", "ADVS.PARAMCD",
                  "Set PARAMN from ADVS.PARAMCD. SYSBP is 1, DIABP is 2, PULSE is 3, TEMP is 4, WEIGHT is 5, HEIGHT is 6, and BMI is 7."),
    demo_spec_row("PARAMCD", "Parameter Code", "Char", 8, "2", "VS.VSTESTCD",
                  "For a collected row, copy VS.VSTESTCD. The collected codes are SYSBP, DIABP, PULSE, TEMP, WEIGHT, and HEIGHT. For a derived row, set PARAMCD to BMI."),
    demo_spec_row("PARAM", "Parameter", "Char", 40, "", "ADVS.PARAMCD",
                  "Set PARAM from ADVS.PARAMCD. SYSBP is Systolic Blood Pressure (mmHg). DIABP is Diastolic Blood Pressure (mmHg). PULSE is Pulse Rate (beats/min). TEMP is Temperature (C). WEIGHT is Weight (kg). HEIGHT is Height (cm). BMI is Body Mass Index (kg/m2)."),
    demo_spec_row("AVAL", "Analysis Value", "Float", 8, "", "VS.VSSTRESN; VS.VSTESTCD; ADVS.PARAMCD",
                  "For a collected parameter, copy VS.VSSTRESN from the VS row whose VS.VSTESTCD equals ADVS.PARAMCD. Keep the row when VS.VSSTRESN is missing. For BMI, set AVAL to the numeric WEIGHT result in kilograms divided by the square of the numeric HEIGHT result after dividing that height by 100. Do not round. Do not create a BMI row when either numeric result is missing or the height result is not greater than zero."),
    demo_spec_row("AVALU", "Analysis Unit", "Char", 20, "", "ADVS.PARAMCD",
                  "Set AVALU from ADVS.PARAMCD. SYSBP and DIABP use mmHg. PULSE uses beats/min. TEMP uses C. WEIGHT uses kg. HEIGHT uses cm. BMI uses kg/m2."),
    demo_spec_row("ADT", "Analysis Date", "Date", 8, "", "VS.VSDTC",
                  "For a collected parameter, convert VS.VSDTC to a calendar day. In this study VS.VSDTC is a complete calendar day. For a BMI row, use the calendar day of the paired WEIGHT record."),
    demo_spec_row("ADY", "Analysis Relative Day", "Integer", 8, "", "ADVS.ADT; ADSL.TRTSDT",
                  "Leave ADY missing when ADVS.ADT or ADSL.TRTSDT is missing. When ADVS.ADT is on or after ADSL.TRTSDT, subtract ADSL.TRTSDT from ADVS.ADT and add 1. When ADVS.ADT is before ADSL.TRTSDT, subtract ADSL.TRTSDT from ADVS.ADT and do not add 1."),
    demo_spec_row("AVISIT", "Analysis Visit", "Char", 40, "", "VS.VISIT",
                  "Copy VS.VISIT. For a BMI row, copy VS.VISIT from the paired WEIGHT record."),
    demo_spec_row("AVISITN", "Analysis Visit (N)", "Float", 8, "3", "VS.VISITNUM",
                  "Copy VS.VISITNUM. For a BMI row, copy VS.VISITNUM from the paired WEIGHT record."),
    demo_spec_row("ATPT", "Analysis Timepoint", "Char", 40, "", "VS.VSTPT",
                  "Copy VS.VSTPT. Leave ATPT missing when VS.VSTPT is blank. BMI, HEIGHT, WEIGHT, and TEMP have no time point, so ATPT is missing on those rows."),
    demo_spec_row("ATPTN", "Analysis Timepoint (N)", "Float", 8, "4", "VS.VSTPTNUM",
                  "Copy VS.VSTPTNUM. Leave ATPTN missing when VS.VSTPTNUM is missing. BMI, HEIGHT, WEIGHT, and TEMP have no time point, so ATPTN is missing on those rows."),
    demo_spec_row("VISIT", "Visit Name", "Char", 40, "", "VS.VISIT",
                  "Copy VS.VISIT. For a BMI row, copy VS.VISIT from the paired WEIGHT record."),
    demo_spec_row("VISITNUM", "Visit Number", "Float", 8, "", "VS.VISITNUM",
                  "Copy VS.VISITNUM. For a BMI row, copy VS.VISITNUM from the paired WEIGHT record."),
    demo_spec_row("ABLFL", "Baseline Record Flag", "Char", 1, "", "ADVS.AVAL; ADVS.ADT; ADVS.AVISITN; ADVS.SRCSEQ; ADVS.USUBJID; ADVS.PARAMCD; ADVS.ATPTN; ADSL.TRTSDT",
                  "Group rows by ADVS.USUBJID, ADVS.PARAMCD, and ADVS.ATPTN. A missing ADVS.ATPTN is its own group. A row can be the baseline only when ADVS.AVAL is not missing, ADVS.ADT is not missing, ADSL.TRTSDT is not missing, and ADVS.ADT is on or before ADSL.TRTSDT. Set ABLFL to Y on the row with the latest ADVS.ADT, then the greatest ADVS.AVISITN, then the greatest ADVS.SRCSEQ. Set ABLFL to N on every other row in the group. When ADSL.TRTSDT is missing or no row qualifies, set ABLFL to N on every row in the group."),
    demo_spec_row("BASE", "Baseline Value", "Float", 8, "", "ADVS.AVAL; ADVS.ABLFL; ADVS.USUBJID; ADVS.PARAMCD; ADVS.ATPTN",
                  "Within ADVS.USUBJID, ADVS.PARAMCD, and ADVS.ATPTN, copy ADVS.AVAL from the row where ADVS.ABLFL equals Y onto every row in that group. A missing ADVS.ATPTN is its own group. Leave BASE missing when the group has no row with ADVS.ABLFL equal to Y."),
    demo_spec_row("CHG", "Change from Baseline", "Float", 8, "", "ADVS.AVAL; ADVS.BASE",
                  "When ADVS.AVAL and ADVS.BASE are both non-missing, set CHG to ADVS.AVAL minus ADVS.BASE. The baseline row is included and is zero when the two values are equal. Otherwise leave CHG missing."),
    demo_spec_row("PCHG", "Percent Change from Baseline", "Float", 8, "", "ADVS.CHG; ADVS.BASE",
                  "When ADVS.CHG and ADVS.BASE are both non-missing and ADVS.BASE is not zero, set PCHG to 100 times ADVS.CHG divided by ADVS.BASE. Do not round. Otherwise leave PCHG missing."),
    demo_spec_row("ONTRTFL", "On Treatment Record Flag", "Char", 1, "", "ADVS.ADT; ADSL.TRTSDT; ADSL.TRTEDT",
                  "Leave ONTRTFL missing when ADVS.ADT is missing. Set ONTRTFL to N when ADSL.TRTSDT is missing. When ADSL.TRTEDT is missing and ADSL.TRTSDT is not missing, set ONTRTFL to Y when ADVS.ADT is on or after ADSL.TRTSDT and to N otherwise. When both treatment days are present, set ONTRTFL to Y when ADVS.ADT is on or after ADSL.TRTSDT and on or before ADSL.TRTEDT, and to N otherwise."),
    demo_spec_row("ANRLO", "Analysis Normal Range Lower Limit", "Float", 8, "", "ADVS.PARAMCD",
                  "Set ANRLO from ADVS.PARAMCD. SYSBP is 90, DIABP is 60, PULSE is 60, TEMP is 36, and BMI is 18.5. WEIGHT and HEIGHT have no lower bound, so ANRLO is missing."),
    demo_spec_row("ANRHI", "Analysis Normal Range Upper Limit", "Float", 8, "", "ADVS.PARAMCD",
                  "Set ANRHI from ADVS.PARAMCD. SYSBP is 140, DIABP is 90, PULSE is 100, TEMP is 37.5, and BMI is 25. WEIGHT and HEIGHT have no upper bound, so ANRHI is missing."),
    demo_spec_row("ANRIND", "Analysis Reference Range Indicator", "Char", 8, "", "ADVS.AVAL; ADVS.PARAMCD; ADVS.ANRLO; ADVS.ANRHI",
                  "Leave ANRIND missing when ADVS.AVAL is missing. WEIGHT and HEIGHT have no reference range, so leave ANRIND missing for those parameters. For BMI, set LOW when ADVS.AVAL is below 18.5, NORMAL when ADVS.AVAL is from 18.5 inclusive to below 25, and HIGH when ADVS.AVAL is greater than or equal to 25. For SYSBP, DIABP, PULSE, and TEMP, set LOW when ADVS.AVAL is below ADVS.ANRLO, HIGH when ADVS.AVAL is above ADVS.ANRHI, and NORMAL when ADVS.AVAL is from ADVS.ANRLO through ADVS.ANRHI inclusive."),
    demo_spec_row("DTYPE", "Derivation Type", "Char", 8, "", "ADVS.PARAMCD",
                  "Set DTYPE to DERIVED when ADVS.PARAMCD equals BMI. Otherwise leave DTYPE blank."),
    demo_spec_row("SRCDOM", "Source Data", "Char", 8, "", "ADVS.PARAMCD; VS.DOMAIN",
                  "Set SRCDOM to VS for SYSBP, DIABP, PULSE, TEMP, WEIGHT, and HEIGHT. Set SRCDOM to ADVS when ADVS.PARAMCD equals BMI."),
    demo_spec_row("SRCVAR", "Source Variable", "Char", 8, "", "ADVS.PARAMCD; VS.VSSTRESN",
                  "Set SRCVAR to VSSTRESN for a collected parameter. Set SRCVAR to AVAL when ADVS.PARAMCD equals BMI."),
    demo_spec_row("SRCSEQ", "Source Sequence Number", "Integer", 8, "", "VS.VSSEQ",
                  "For a collected row, copy VS.VSSEQ. For a BMI row, copy VS.VSSEQ from the paired WEIGHT record.")
  ))
}

pharmaverse_advs_parameters <- function() {
  demo_bind_params("ADVS", list(
    demo_param_row("SYSBP", "Systolic Blood Pressure (mmHg)", "VS.VSTESTCD; VS.VSSTRESN",
                   "Collected measurement. Keep VS rows with VS.VSTESTCD = 'SYSBP'. The numeric result is VS.VSSTRESN in mmHg."),
    demo_param_row("DIABP", "Diastolic Blood Pressure (mmHg)", "VS.VSTESTCD; VS.VSSTRESN",
                   "Collected measurement. Keep VS rows with VS.VSTESTCD = 'DIABP'. The numeric result is VS.VSSTRESN in mmHg."),
    demo_param_row("PULSE", "Pulse Rate (beats/min)", "VS.VSTESTCD; VS.VSSTRESN",
                   "Collected measurement. Keep VS rows with VS.VSTESTCD = 'PULSE'. The numeric result is VS.VSSTRESN in beats/min."),
    demo_param_row("TEMP", "Temperature (C)", "VS.VSTESTCD; VS.VSSTRESN",
                   "Collected measurement. Keep VS rows with VS.VSTESTCD = 'TEMP'. The numeric result is VS.VSSTRESN in degrees C."),
    demo_param_row("WEIGHT", "Weight (kg)", "VS.VSTESTCD; VS.VSSTRESN",
                   "Collected measurement. Keep VS rows with VS.VSTESTCD = 'WEIGHT'. The numeric result is VS.VSSTRESN in kilograms."),
    demo_param_row("HEIGHT", "Height (cm)", "VS.VSTESTCD; VS.VSSTRESN",
                   "Collected measurement. Keep VS rows with VS.VSTESTCD = 'HEIGHT'. The numeric result is VS.VSSTRESN in centimetres."),
    demo_param_row("BMI", "Body Mass Index (kg/m2)", "ADVS.PARAMCD; ADVS.AVAL; ADVS.USUBJID; ADVS.AVISITN; ADVS.ADT",
                   "Derived parameter. Create one row for each WEIGHT record whose numeric result is not missing. Pair it with the HEIGHT record for the same ADVS.USUBJID and the same ADVS.AVISITN when that HEIGHT record exists. Otherwise pair it with the HEIGHT record for the same ADVS.USUBJID whose ADVS.ADT is the latest ADVS.ADT on or before the WEIGHT record ADVS.ADT. Break a tie on the height record with the greatest source sequence. The numeric result is the WEIGHT result in kilograms divided by the square of the HEIGHT result in metres. Convert centimetres to metres by dividing by 100. Skip the row when the height result is missing or not greater than zero.")
  ))
}

pharmaverse_demo_datasets_sheet <- function(ds) {
  rows <- list(
    ADSL = c("Subject-Level Analysis Dataset", "ADSL", "One record per subject", "USUBJID",
             "dm.xpt, suppdm.xpt, ex.xpt, ds.xpt, ae.xpt, vs.xpt, lb.xpt, sv.xpt, cm.xpt, mh.xpt"),
    ADAE = c("Adverse Event Analysis Dataset", "OCCDS", "One record per subject per adverse event", "USUBJID, AESEQ",
             "ae.xpt, adsl.xpt"),
    ADVS = c("Vital Signs Analysis Dataset", "BDS",
             "One record per subject per parameter per analysis visit per analysis time point",
             "USUBJID, PARAMCD, AVISITN, ATPTN",
             "vs.xpt, adsl.xpt")
  )
  x <- rows[[ds]]
  data.frame(Dataset = ds, Label = x[[1]], Class = x[[2]], Structure = x[[3]], Keys = x[[4]],
             SourceData = x[[5]], stringsAsFactors = FALSE)
}

pharmaverse_demo_notes <- function(ds) {
  common <- list(
    ADSL = list(
      c("", "purpose", "Subject-level analysis dataset derived from pharmaversesdtm 1.5.0 core domains. One row per DM.USUBJID."),
      c("TRTSDTM", "complex", "First qualifying exposure. Qualifying rows have EX.EXDOSE greater than zero or EX.EXTRT equal to PLACEBO. Clock time is imputed to 00:00:00 and TRTSTMF is H."),
      c("TRTEDTM", "complex", "Last qualifying exposure. A blank EX.EXENDTC falls back to EX.EXSTDTC for that row. Clock time is imputed to 23:59:59 and TRTETMF is H."),
      c("TRT01P", "complex", "Planned regimen from DM.ARM, with TRT01PN codes 0 Placebo, 1 Xanomeline Low Dose, 2 Xanomeline High Dose, 3 Screen Failure. TRT01A and TRT01AN use DM.ACTARM and the same codes."),
      c("SAFFL", "complex", "Y when the subject has a qualifying EX row. Population flags ITTFL, EFFFL, COMP8FL, COMP16FL, and COMP24FL come from SUPPDM.QVAL for QNAM ITT, EFFICACY, COMPLT8, COMPLT16, and COMPLT24."),
      c("EOSSTT", "complex", "COMPLETED or DISCONTINUED from the DS disposition event. DCSREAS is DS.DSDECOD when EOSSTT is DISCONTINUED."),
      c("RANDDT", "complex", "DS.DSSTDTC where DS.DSCAT is PROTOCOL MILESTONE and DS.DSDECOD is RANDOMIZED."),
      c("LSTALVDT", "complex", "Latest usable calendar day across DM, DS, EX, AE, VS, LB, SV, CM, and MH. Partial days are ignored."),
      c("BMIBL", "complex", "WEIGHTBL divided by the square of HEIGHTBL in metres. Both baseline values are the latest non-missing VS result on or before TRTSDT.")
    ),
    ADAE = list(
      c("", "purpose", "Occurrence dataset, one row per AE.AESEQ. Upload ae.xpt and the reference adsl.xpt."),
      c("ASTDT", "complex", "Partial AE.AESTDTC is imputed to the first possible day, then moved forward to ADSL.TRTSDT when the partial value could contain that day. ASTDTF is D or M when a part was imputed."),
      c("AENDT", "complex", "A partial end is imputed to the last possible day, then moved back to ADSL.TRTEDT when the partial value could contain that day. A blank end stays missing."),
      c("TRTEMFL", "complex", "Y when ASTDT is on or after ADSL.TRTSDT and on or before ADSL.TRTEDT plus 30 calendar days."),
      c("AOCCIFL", "complex", "First treatment-emergent row within the subject, sorted by ASTDT and AESEQ. AOCCPFL is the first within preferred term. AOCCSFL is the first within body system."),
      c("ASEVN", "complex", "MILD 1, MODERATE 2, SEVERE 3. ARELN is NONE 1, REMOTE 2, POSSIBLE 3, PROBABLE 4.")
    ),
    ADVS = list(
      c("", "purpose", "BDS dataset for SYSBP, DIABP, PULSE, TEMP, WEIGHT, HEIGHT, and derived BMI. Upload vs.xpt and the reference adsl.xpt."),
      c("ATPTN", "keys", "The key is USUBJID, PARAMCD, AVISITN, ATPTN. ATPTN is missing for HEIGHT, WEIGHT, TEMP, and BMI. That combination is unique because each subject, parameter, and analysis visit has one such row."),
      c("BMI", "complex", "Derived from WEIGHT and HEIGHT. Prefer the height at the same AVISITN. Otherwise use the latest height on or before the weight date. Value is kilograms divided by metres squared."),
      c("ABLFL", "complex", "Last non-missing AVAL on or before ADSL.TRTSDT within USUBJID, PARAMCD, and ATPTN. Tie-break is the greatest AVISITN and then the greatest SRCSEQ."),
      c("ANRIND", "complex", "VS has no normal ranges. SYSBP 90 to 140, DIABP 60 to 90, PULSE 60 to 100, and TEMP 36 to 37.5 are NORMAL, inclusive. BMI below 18.5 is LOW, 18.5 up to but not including 25 is NORMAL, and 25 or more is HIGH. WEIGHT and HEIGHT have no range.")
    )
  )
  items <- common[[ds]]
  df <- do.call(rbind, lapply(items, function(x) {
    data.frame(Dataset = ds, Variable = x[[1]], Topic = x[[2]], Message = x[[3]], stringsAsFactors = FALSE)
  }))
  rownames(df) <- NULL
  df
}

pharmaverse_demo_spec_frames <- function() {
  list(
    ADSL = list(variable = pharmaverse_adsl_variables(),
                parameter = demo_bind_params("ADSL", list()),
                datasets = pharmaverse_demo_datasets_sheet("ADSL"),
                notes = pharmaverse_demo_notes("ADSL")),
    ADAE = list(variable = pharmaverse_adae_variables(),
                parameter = demo_bind_params("ADAE", list()),
                datasets = pharmaverse_demo_datasets_sheet("ADAE"),
                notes = pharmaverse_demo_notes("ADAE")),
    ADVS = list(variable = pharmaverse_advs_variables(),
                parameter = pharmaverse_advs_parameters(),
                datasets = pharmaverse_demo_datasets_sheet("ADVS"),
                notes = pharmaverse_demo_notes("ADVS"))
  )
}

demo_style_sheet <- function(wb, sheet, df, widths, wrap) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    df[[nm]] <- as.character(df[[nm]])
    df[[nm]][is.na(df[[nm]])] <- ""
  }
  wb <- openxlsx2::wb_add_data(wb, sheet, df, na.strings = "", with_filter = TRUE)
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

demo_write_csv <- function(df, path) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    df[[nm]] <- as.character(df[[nm]])
    df[[nm]][is.na(df[[nm]])] <- ""
  }
  utils::write.csv(df, path, row.names = FALSE, na = "", quote = TRUE, fileEncoding = "UTF-8")
}

demo_audit_spec_text <- function(frames, sdtm_dir = pharmaverse_sdtm_dir()) {
  doms <- c("DM", "SUPPDM", "EX", "DS", "AE", "VS", "MH", "SV", "LB", "CM", "ADSL")
  cols <- lapply(doms, function(dom) {
    path <- if (identical(dom, "ADSL")) {
      file.path(pharmaverse_demo_out_dir(), "adsl.xpt")
    } else {
      file.path(sdtm_dir, paste0(tolower(dom), ".xpt"))
    }
    if (!file.exists(path)) return(character())
    names(haven::read_xpt(path, n_max = 0L))
  })
  names(cols) <- doms
  # ADAE and ADVS are checked with adsl.xpt. Use the ADSL spec when that file
  # has not been written yet, so a bare ADSL column name is still visible.
  if (!length(cols$ADSL)) cols$ADSL <- unique(toupper(frames$ADSL$variable$Variable))
  bad <- character()
  scan_one <- function(ds, text, target, keys, own) {
    if (grepl("\\bi\\.e\\.|\\be\\.g\\.", text, perl = TRUE, ignore.case = TRUE)) {
      bad <<- c(bad, paste(ds, "contains i.e. or e.g."))
    }
    if (grepl("\t", text, fixed = TRUE)) bad <<- c(bad, paste(ds, "contains a tab"))
    up <- toupper(text)
    stripped <- gsub("\\b[A-Z][A-Z0-9]*\\.[A-Z][A-Z0-9]*\\b", " ", up, perl = TRUE)
    words <- unique(unlist(regmatches(stripped, gregexpr("\\b[A-Z][A-Z0-9_]*\\b", stripped, perl = TRUE))))
    for (word in words) {
      # The fake extractor skips a bare name that is the variable being defined,
      # and it treats a key as internal even when source domains share that name.
      if (nzchar(target) && identical(word, target)) next
      if (word %in% keys) next
      owners <- names(cols)[vapply(cols, function(v) word %in% toupper(v), logical(1))]
      if (identical(ds, "ADSL")) owners <- setdiff(owners, "ADSL")
      if (word %in% own) owners <- unique(c(ds, owners))
      external <- setdiff(owners, ds)
      ambiguous <- length(owners) >= 2L && (length(external) >= 2L || (ds %in% owners && length(external) >= 1L))
      if (isTRUE(ambiguous)) bad <<- c(bad, paste(ds, word, "owners:", paste(owners, collapse = "+")))
    }
  }
  for (ds in names(frames)) {
    vars <- frames[[ds]]$variable
    params <- frames[[ds]]$parameter
    own <- unique(toupper(vars$Variable))
    keys <- unique(toupper(vars$Variable[nzchar(vars$Key)]))
    for (i in seq_len(nrow(vars))) {
      scan_one(ds, paste(vars$Source[[i]], vars$Derivation[[i]]), toupper(vars$Variable[[i]]), keys, own)
    }
    if (nrow(params)) {
      for (i in seq_len(nrow(params))) {
        # A parameter row is attached to every member variable, so its own
        # PARAMCD is not a single target name the extractor would skip.
        scan_one(ds, paste(params$Source[[i]], params$Derivation[[i]]), "", keys, own)
      }
    }
  }
  unique(bad)
}

write_pharmaverse_demo_specs <- function(spec_dir = pharmaverse_demo_spec_dir()) {
  frames <- pharmaverse_demo_spec_frames()
  # Audit against SDTM columns. ADSL transports are optional at spec-write time.
  problems <- demo_audit_spec_text(frames)
  if (length(problems)) stop(paste(problems, collapse = "\n"), call. = FALSE)
  for (ds in names(frames)) {
    dest <- file.path(spec_dir, tolower(ds))
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    part <- frames[[ds]]
    demo_write_csv(part$variable, file.path(dest, "variable.csv"))
    demo_write_csv(part$parameter, file.path(dest, "parameter.csv"))
    wb <- openxlsx2::wb_workbook()
    sheets <- list(
      list("Variable", part$variable, c(Dataset = 12, Variable = 14, Label = 42, Type = 12, Length = 10, Key = 8, Source = 36, Derivation = 80), "Derivation"),
      list("Parameter", part$parameter, c(Dataset = 12, PARAMCD = 14, PARAM = 42, Source = 28, Derivation = 80), "Derivation"),
      list("Datasets", part$datasets, c(Dataset = 12, Label = 42, Class = 12, Structure = 55, Keys = 36, SourceData = 55), "Structure"),
      list("Notes", part$notes, c(Dataset = 12, Variable = 14, Topic = 14, Message = 100), "Message")
    )
    for (s in sheets) {
      wb <- openxlsx2::wb_add_worksheet(wb, s[[1]])
      wb <- demo_style_sheet(wb, s[[1]], s[[2]], s[[3]], s[[4]])
    }
    openxlsx2::wb_save(wb, file.path(dest, paste0(tolower(ds), "_spec.xlsx")), overwrite = TRUE)
  }
  invisible(frames)
}

pharmaverse_demo_ref_launched <- function() {
  f <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(f) == 1L && grepl("pharmaverse_demo_ref\\.R$", sub("^--file=", "", f))
}

if (pharmaverse_demo_ref_launched()) {
  args <- commandArgs(trailingOnly = TRUE)
  if ("--write-spec" %in% args || "--write-spec-only" %in% args) write_pharmaverse_demo_specs()
  if (!"--write-spec-only" %in% args) run_pharmaverse_demo_ref()
}
