# Independent ADPPK reference build following demos/adppk/spec/variable.csv.
# Uses base R, dplyr, and haven only. It intentionally does not call admiral.

adppk_ref_root <- function() {
  x <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (i in seq_len(8L)) {
    if (file.exists(file.path(x, "demos", "adppk", "spec", "variable.csv"))) return(x)
    parent <- dirname(x)
    if (parent == x) break
    x <- parent
  }
  stop("Cannot find the dualdag root.", call. = FALSE)
}

adppk_ref_blank_na <- function(df) {
  for (nm in names(df)) {
    if (is.character(df[[nm]])) {
      df[[nm]][!is.na(df[[nm]]) & !nzchar(trimws(df[[nm]]))] <- NA_character_
    }
  }
  df
}

adppk_ref_time <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- NA_character_
  x[!is.na(x) & nchar(x) == 10L] <- paste0(x[!is.na(x) & nchar(x) == 10L], "T00:00:00")
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
}

adppk_ref_pt_hours <- function(x) {
  x <- trimws(as.character(x))
  out <- rep(NA_real_, length(x))
  out[!is.na(x) & x == "Pre-dose"] <- 0
  r <- regexec("^([0-9.]+)-([0-9.]+)h Post-dose$", x)
  m <- regmatches(x, r)
  for (i in seq_along(m)) if (length(m[[i]]) == 3L) out[[i]] <- (as.numeric(m[[i]][2]) + as.numeric(m[[i]][3])) / 2
  r <- regexec("^([0-9.]+)(h| Min) Post-dose$", x)
  m <- regmatches(x, r)
  for (i in seq_along(m)) if (length(m[[i]]) == 3L) {
    val <- as.numeric(m[[i]][2])
    out[[i]] <- if (m[[i]][3] == " Min") val / 60 else val
  }
  out
}

adppk_ref_rel_hours <- function(start, end) {
  as.numeric(difftime(end, start, units = "hours"))
}

build_pharmaverse_adppk_ref <- function(data_dir = file.path(adppk_ref_root(), "demos", "adppk", "data"),
                                        spec_file = file.path(adppk_ref_root(), "demos", "adppk", "spec", "variable.csv")) {
  if (!requireNamespace("haven", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) {
    stop("The ADPPK reference build needs haven and dplyr.", call. = FALSE)
  }
  read <- function(name) adppk_ref_blank_na(haven::read_xpt(file.path(data_dir, paste0(name, ".xpt"))))
  ex <- read("ex"); pc <- read("pc"); vs <- read("vs"); lb <- read("lb"); adsl <- read("adsl")
  required <- c("ex", "pc", "vs", "lb", "adsl")
  absent <- required[!file.exists(file.path(data_dir, paste0(required, ".xpt")))]
  if (length(absent)) stop("Missing ADPPK inputs: ", paste(absent, collapse = ", "), call. = FALSE)

  ex <- ex[!is.na(ex$EXDOSE) & ex$EXDOSE > 0, , drop = FALSE]
  ex$EXSEQ <- as.integer(ex$EXSEQ)
  ex$EXDOSE <- as.numeric(ex$EXDOSE)
  ex$ASTDTM <- adppk_ref_time(ex$EXSTDTC)
  ex$AENDTM <- adppk_ref_time(ex$EXENDTC)
  ex$AENDTM[is.na(ex$AENDTM)] <- ex$ASTDTM[is.na(ex$AENDTM)]
  ex$ASTDT <- as.Date(ex$ASTDTM, tz = "UTC")
  ex$AENDT <- as.Date(ex$AENDTM, tz = "UTC")
  ex$NFRLT0 <- (as.numeric(ex$VISITDY) - 1) * 24
  ex_parts <- lapply(seq_len(nrow(ex)), function(i) {
    days <- seq(ex$ASTDT[[i]], ex$AENDT[[i]], by = "day")
    data.frame(
      STUDYID = as.character(ex$STUDYID[[i]]), USUBJID = as.character(ex$USUBJID[[i]]),
      EXSEQ = ex$EXSEQ[[i]], EXDOSE = ex$EXDOSE[[i]], EXTRT = as.character(ex$EXTRT[[i]]),
      EXDOSU = as.character(ex$EXDOSU[[i]]), EXDOSFRM = as.character(ex$EXDOSFRM[[i]]),
      EXROUTE = as.character(ex$EXROUTE[[i]]), VISITDY = ex$VISITDY[[i]],
      ADTM = as.POSIXct(paste(days, "00:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
      NFRLT = ex$NFRLT0[[i]] + (seq_along(days) - 1L) * 24,
      EVID = 1L, DRUG = as.character(ex$EXTRT[[i]]), stringsAsFactors = FALSE
    )
  })
  doses <- do.call(rbind, ex_parts)
  doses$ADT <- as.Date(doses$ADTM, tz = "UTC")
  doses$EXDOSE_first <- NA_real_; doses$EXDOSE_prev <- NA_real_
  doses$ADTM_prev <- as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
  doses$NFRLT_prev <- NA_real_; doses$FANLDTM <- as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")

  pc$ADTM <- adppk_ref_time(pc$PCDTC)
  pc$ADT <- as.Date(pc$ADTM, tz = "UTC")
  pc$NFRLT <- (as.numeric(pc$VISITDY) - 1) * 24 + adppk_ref_pt_hours(pc$PCTPT)
  pc$EVID <- 0L; pc$DRUG <- as.character(pc$PCTEST)
  pc$EXDOSE <- NA_real_
  pc$EXDOSE_first <- NA_real_; pc$EXDOSE_prev <- NA_real_
  pc$ADTM_prev <- as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
  pc$NFRLT_prev <- NA_real_; pc$FANLDTM <- as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
  first_dose <- doses[order(doses$USUBJID, doses$DRUG, doses$ADTM, doses$EXSEQ), , drop = FALSE]
  first_dose <- first_dose[!duplicated(paste(first_dose$USUBJID, first_dose$DRUG)), , drop = FALSE]
  for (i in seq_len(nrow(pc))) {
    hit <- which(first_dose$USUBJID == pc$USUBJID[[i]] & first_dose$DRUG == pc$DRUG[[i]])
    if (!length(hit)) next
    pc$FANLDTM[[i]] <- first_dose$ADTM[hit[[1]]]
    pc$EXDOSE_first[[i]] <- first_dose$EXDOSE[hit[[1]]]
    prev_actual <- which(doses$USUBJID == pc$USUBJID[[i]] & doses$ADTM < pc$ADTM[[i]])
    if (length(prev_actual)) {
      prev_actual <- prev_actual[order(doses$ADTM[prev_actual], doses$EXSEQ[prev_actual])]
      j <- prev_actual[[length(prev_actual)]]
      pc$ADTM_prev[[i]] <- doses$ADTM[[j]]
      pc$EXDOSE_prev[[i]] <- doses$EXDOSE[[j]]
    }
    prev_nominal <- which(doses$USUBJID == pc$USUBJID[[i]] & doses$NFRLT < pc$NFRLT[[i]])
    if (length(prev_nominal)) {
      prev_nominal <- prev_nominal[order(doses$NFRLT[prev_nominal], doses$ADTM[prev_nominal], doses$EXSEQ[prev_nominal])]
      pc$NFRLT_prev[[i]] <- doses$NFRLT[prev_nominal[[length(prev_nominal)]]]
    }
  }
  pc <- pc[!is.na(pc$FANLDTM), , drop = FALSE]
  common <- c("STUDYID", "USUBJID", "EXSEQ", "EXDOSE", "ADTM", "ADT", "NFRLT", "EVID", "DRUG",
              "EXDOSE_first", "EXDOSE_prev", "ADTM_prev", "NFRLT_prev", "FANLDTM", "PARAMCD", "PC_TEST",
              "PCSTRESC", "PCSTRESN", "PCSTRESU", "PCSPEC", "PCLLOQ")
  d <- doses[, c("STUDYID", "USUBJID", "EXSEQ", "EXDOSE", "ADTM", "ADT", "NFRLT", "EVID", "DRUG",
                 "EXDOSE_first", "EXDOSE_prev", "ADTM_prev", "NFRLT_prev", "FANLDTM"), drop = FALSE]
  d$PARAMCD <- NA_character_; d$PC_TEST <- as.character(doses$EXTRT)
  d$PCSTRESC <- NA_character_; d$PCSTRESN <- NA_real_; d$PCSTRESU <- NA_character_; d$PCSPEC <- NA_character_; d$PCLLOQ <- NA_real_
  p <- pc[, c("STUDYID", "USUBJID", "PCSEQ", "EXDOSE", "ADTM", "ADT", "NFRLT", "EVID", "DRUG",
              "EXDOSE_first", "EXDOSE_prev", "ADTM_prev", "NFRLT_prev", "FANLDTM", "PCTESTCD", "PCTEST",
              "PCSTRESC", "PCSTRESN", "PCSTRESU", "PCSPEC", "PCLLOQ"), drop = FALSE]
  names(p)[names(p) == "PCSEQ"] <- "EXSEQ"; names(p)[names(p) == "PCTESTCD"] <- "PARAMCD"; names(p)[names(p) == "PCTEST"] <- "PC_TEST"
  d <- d[, common, drop = FALSE]; p <- p[, common, drop = FALSE]
  z <- rbind(d, p)
  z$PARAMCD <- ifelse(z$EVID == 1L, "DOSE", as.character(z$PARAMCD))
  z$TRT01P <- adsl$TRT01P[match(z$USUBJID, adsl$USUBJID)]
  z$maxdate <- as.Date(NA)
  z$min_NFRLT <- NA_real_
  key <- paste(z$USUBJID, z$DRUG, sep = "\r")
  for (k in unique(key)) {
    ix <- which(key == k)
    conc <- ix[z$EVID[ix] == 0L]
    if (!length(conc)) next
    z$maxdate[ix] <- max(z$ADT[conc], na.rm = TRUE)
    z$min_NFRLT[ix] <- min(z$NFRLT[ix], na.rm = TRUE)
  }
  z <- z[!is.na(z$maxdate) & z$ADT <= z$maxdate, , drop = FALSE]
  z$FANLDTM <- as.POSIXct(NA, origin = "1970-01-01", tz = "UTC")
  z$min_NFRLT <- NA_real_
  key <- paste(z$USUBJID, z$DRUG, sep = "\r")
  for (k in unique(key)) {
    ix <- which(key == k)
    z$FANLDTM[ix] <- min(pc$FANLDTM[paste(pc$USUBJID, pc$DRUG, sep = "\r") == k], na.rm = TRUE)
    z$min_NFRLT[ix] <- min(z$NFRLT[ix], na.rm = TRUE)
  }
  z$AFRLT <- adppk_ref_rel_hours(z$FANLDTM, z$ADTM)
  z$APRLT <- adppk_ref_rel_hours(z$ADTM_prev, z$ADTM)
  z$APRLT[z$EVID == 1L] <- 0
  z$APRLT[is.na(z$APRLT)] <- z$AFRLT[is.na(z$APRLT)]
  z$NPRLT <- z$NFRLT - ifelse(is.na(z$NFRLT_prev), z$min_NFRLT, z$NFRLT_prev)
  z$NPRLT[z$EVID == 1L] <- 0
  z$DOSEA <- ifelse(z$EVID == 1L, z$EXDOSE, ifelse(is.na(z$EXDOSE_prev), z$EXDOSE_first, z$EXDOSE_prev))
  z$DOSEP <- ifelse(z$TRT01P == "Xanomeline High Dose", 81,
                    ifelse(z$TRT01P == "Xanomeline Low Dose", 54, ifelse(z$TRT01P == "Placebo", 0, NA_real_)))
  z$DVID <- ifelse(z$EVID == 1L, NA_character_, as.character(z$PARAMCD))
  z$DV <- ifelse(z$EVID == 1L, NA_real_, z$PCSTRESN)
  z$CMT <- ifelse(z$EVID == 1L, 1L, ifelse(z$PCSPEC == "PLASMA", 2L, 3L))
  z$MDV <- ifelse(z$EVID == 1L | is.na(z$DV), 1L, 0L)
  z$BLQFL <- ifelse(!is.na(z$PCSTRESC) & z$PCSTRESC == "<BLQ", "Y", "N")
  z$BLQFN <- ifelse(!is.na(z$PCSTRESC) & z$PCSTRESC == "<BLQ", 1L, 0L)
  z$AMT <- ifelse(z$EVID == 1L, z$EXDOSE, NA_real_)
  z$AVAL <- z$DV
  z$AVALU <- ifelse(z$EVID == 1L, NA_character_, as.character(z$PCSTRESU))
  z$USTRESC <- ifelse(z$EVID == 1L, NA_character_, as.character(z$PCSTRESC))
  z$RLTU <- "h"; z$II <- as.integer(z$EVID == 1L); z$SS <- as.integer(z$EVID == 1L); z$ADDL <- 0L; z$OCC <- 1L
  z$UDTC <- ifelse(is.na(z$ADTM), NA_character_, format(z$ADTM, "%Y-%m-%dT%H:%M:%S", tz = "UTC"))

  adsl$STUDYIDN <- suppressWarnings(as.numeric(sub("-.*$", "", adsl$USUBJID)))
  adsl$USUBJIDN <- suppressWarnings(as.numeric(sub("^[^-]*-[^-]*-", "", adsl$USUBJID)))
  adsl$SITEIDN <- suppressWarnings(as.numeric(sub("^[^-]*-([^-]*)-.*$", "\\1", adsl$USUBJID)))
  adsl$SUBJIDN <- suppressWarnings(as.numeric(adsl$SUBJID))
  adsl$SEXN <- ifelse(adsl$SEX == "M", 1L, ifelse(adsl$SEX == "F", 2L, NA_integer_))
  adsl$RACEN <- ifelse(adsl$RACE == "WHITE", 1L, ifelse(adsl$RACE == "BLACK OR AFRICAN AMERICAN", 2L,
                  ifelse(adsl$RACE == "AMERICAN INDIAN OR ALASKA NATIVE", 3L, ifelse(adsl$RACE == "ASIAN", 4L, NA_integer_))))
  adsl$AETHNIC <- adsl$ETHNIC
  adsl$AETHNICN <- ifelse(adsl$ETHNIC == "NOT HISPANIC OR LATINO", 0L,
                           ifelse(adsl$ETHNIC == "HISPANIC OR LATINO", 1L, NA_integer_))
  adsl$REGION1 <- adsl$COUNTRY; adsl$REGION1N <- 1L
  adsl$COUNTRYN <- ifelse(adsl$COUNTRY == "USA", 1L, NA_integer_)
  adsl$COUNTRYL <- ifelse(adsl$COUNTRY == "USA", "United States", NA_character_)
  adsl$SUBJTYPC <- "Volunteer"; adsl$SUBJTYP <- 1L
  adsl$ARMCD <- c("Pbo", "Xan_Hi", "Xan_Lo", "Scrnfail")[match(adsl$ARM, c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose", "Screen Failure"))]
  adsl$COHORT <- ifelse(adsl$ARMCD == "Scrnfail", 9L, ifelse(adsl$ARMCD == "Pbo", 0L,
                       ifelse(adsl$ARMCD == "Xan_Lo", 1L, ifelse(adsl$ARMCD == "Xan_Hi", 2L, NA_integer_))))
  adsl$COHORTC <- ifelse(adsl$ARMCD == "Scrnfail", "Screen Failure", adsl$ARM)
  adsl$FORM <- unique(ex$EXDOSFRM)[[1]]; adsl$FORMN <- 3L
  adsl$ROUTE <- unique(ex$EXROUTE)[[1]]; adsl$ROUTEN <- 3L
  for (nm in c("WTBL", "HTBL", "CREATBL", "ALTBL", "ASTBL", "TBILBL")) adsl[[nm]] <- NA_real_
  get_first <- function(df, filter, value) {
    q <- df[filter & !is.na(df[[value]]), c("USUBJID", value), drop = FALSE]
    q <- q[!duplicated(q$USUBJID), , drop = FALSE]
    q[[value]][match(adsl$USUBJID, q$USUBJID)]
  }
  adsl$HTBL <- get_first(vs, vs$VSTESTCD == "HEIGHT", "VSSTRESN")
  adsl$WTBL <- get_first(vs, vs$VSTESTCD == "WEIGHT" & vs$VSBLFL == "Y", "VSSTRESN")
  adsl$CREATBL <- get_first(lb, lb$LBTESTCD == "CREAT" & lb$LBBLFL == "Y", "LBSTRESN")
  adsl$ALTBL <- get_first(lb, lb$LBTESTCD == "ALT" & lb$LBBLFL == "Y", "LBSTRESN")
  adsl$ASTBL <- get_first(lb, lb$LBTESTCD == "AST" & lb$LBBLFL == "Y", "LBSTRESN")
  adsl$TBILBL <- get_first(lb, lb$LBTESTCD == "BILI" & lb$LBBLFL == "Y", "LBSTRESN")
  adsl$BMIBL <- adsl$WTBL / (adsl$HTBL / 100)^2
  adsl$BSABL <- sqrt(adsl$HTBL * adsl$WTBL / 3600)
  female <- adsl$SEX == "F"
  adsl$CRCLBL <- ((140 - adsl$AGE) * adsl$WTBL * ifelse(female, 1.04, 1.23)) / adsl$CREATBL
  scr <- adsl$CREATBL / 88.42; kap <- ifelse(female, 0.7, 0.9); alpha <- ifelse(female, -0.241, -0.302)
  adsl$EGFRBL <- 142 * pmin(scr / kap, 1)^alpha * pmax(scr / kap, 1)^(-1.2) * 0.9938^adsl$AGE * ifelse(female, 1.012, 1)
  covars <- adsl[, c("STUDYID", "USUBJID", "SUBJID", "SITEID", "STUDYIDN", "USUBJIDN", "SUBJIDN", "SITEIDN",
                     "AGE", "SEX", "SEXN", "RACE", "RACEN", "AETHNIC", "AETHNICN", "COUNTRY", "COUNTRYL", "COUNTRYN",
                     "REGION1", "REGION1N", "SUBJTYP", "SUBJTYPC", "COHORT", "COHORTC", "FORM", "FORMN", "ROUTE", "ROUTEN",
                     "WTBL", "HTBL", "BMIBL", "BSABL", "CREATBL", "CRCLBL", "EGFRBL", "TBILBL", "ASTBL", "ALTBL"), drop = FALSE]
  z <- merge(z, covars, by = c("STUDYID", "USUBJID"), all.x = TRUE, sort = FALSE)
  z <- z[order(z$STUDYIDN, z$USUBJIDN, z$AFRLT, z$EVID, z$EXSEQ), , drop = FALSE]
  z$RECSEQ <- seq_len(nrow(z))
  z <- z[order(z$STUDYID, z$USUBJID, z$AFRLT, z$EVID, z$CMT, z$RECSEQ), , drop = FALSE]
  z$PROJID <- z$DRUG; z$PROJIDN <- 1L; z$PART <- 1L
  z$EXCLFCOM <- "None"; z$EXCLF <- 0L
  z$DVIDN <- ifelse(z$DVID == "XAN", 1L, NA_integer_)
  spec <- utils::read.csv(spec_file, stringsAsFactors = FALSE, check.names = FALSE)
  unknown <- setdiff(spec$Variable, names(z))
  if (length(unknown)) stop("Reference build is missing spec variables: ", paste(unknown, collapse = ", "), call. = FALSE)
  z[, spec$Variable, drop = FALSE]
}

if (!exists(".DUALDAG_ADPPK_REF_SOURCE_ONLY", inherits = FALSE)) {
  actual <- build_pharmaverse_adppk_ref()
  cat("Independent ADPPK reference build:", nrow(actual), "rows x", ncol(actual), "columns\n")
}
