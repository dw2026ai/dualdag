# Pharmaverse ADaM demo specs

ADSL, ADAE, and ADVS specifications written in English for the core domains in pharmaversesdtm 1.5.0. The text is original. It is not a conversion of the CDISC pilot define.

Each `spec/<dataset>/` folder contains `variable.csv`, `parameter.csv`, and `<dataset>_spec.xlsx` (sheets Variable, Parameter, Datasets, and Notes). The workbook header is bold and frozen, the columns have an autofilter, and the Derivation column wraps.

## Data and licence

The source transports are [pharmaversesdtm](https://cran.r-project.org/package=pharmaversesdtm) 1.5.0, Apache License 2.0. This demo uses the core domains only. It does not use the ophthalmology, vaccine, oncology, metabolic, neurology, or pediatric variants.

The tools look for those transports in `~/dualdag-demo/pharmaversesdtm/` by default.

## Transports to upload

Upload these files with each dataset. ADAE and ADVS also need the subject-level file produced by the reference build, because their derivations read ADSL columns.

| Dataset | Upload |
| --- | --- |
| ADSL | `dm.xpt`, `suppdm.xpt`, `ex.xpt`, `ds.xpt`, `ae.xpt`, `vs.xpt`, `lb.xpt`, `sv.xpt`, `cm.xpt`, `mh.xpt` |
| ADAE | `ae.xpt` and `~/dualdag-demo/pharmaverse-demo/adsl.xpt` |
| ADVS | `vs.xpt` and `~/dualdag-demo/pharmaverse-demo/adsl.xpt` |

## Reference build

`tools/pharmaverse_demo_ref.R` implements the three datasets from those transports and from the spec text in that same file.

```sh
Rscript tools/pharmaverse_demo_ref.R --write-spec
```

`--write-spec` rewrites the CSV and workbook files, then writes `adsl.xpt`, `adae.xpt`, and `advs.xpt` under `~/dualdag-demo/pharmaverse-demo/`. A run without that flag only refreshes the three transports from the CSV already on disk. `--write-spec-only` rewrites the spec files and does not rebuild the transports.

## Counts

| Dataset | Variables | Parameters | Keys |
| --- | ---: | ---: | --- |
| ADSL | 44 | 0 | USUBJID |
| ADAE | 41 | 0 | USUBJID, AESEQ |
| ADVS | 36 | 7 | USUBJID, PARAMCD, AVISITN, ATPTN |

ADVS parameters are SYSBP, DIABP, PULSE, TEMP, WEIGHT, HEIGHT, and a derived BMI parameter.

## Complex variables

ADSL

- TRTSDTM and TRTEDTM are the first and last qualifying exposure. A row qualifies when EXDOSE is greater than zero or EXTRT is placebo. The source has no clock time, so the start is imputed to 00:00:00 and the end to 23:59:59. A blank EXENDTC falls back to EXSTDTC.
- TRTSDT, TRTEDT, and TRTDURD are the calendar days of those datetimes and the inclusive day count.
- TRT01P, TRT01PN, TRT01A, and TRT01AN copy the planned and actual arms, with numeric codes for placebo, low dose, high dose, and screen failure.
- AGEGR1, AGEGR1N, and RACEN are the pooled age group and the race code.
- SAFFL is Y when the subject has a qualifying exposure row. ITTFL, EFFFL, COMP8FL, COMP16FL, and COMP24FL come from SUPPDM QNAM values ITT, EFFICACY, COMPLT8, COMPLT16, and COMPLT24.
- DTHFL and DTHDT come from the death flag and the death date.
- EOSSTT and DCSREAS come from the disposition event. EOSSTT is COMPLETED or DISCONTINUED, and DCSREAS is the decoded term when the subject discontinued.
- RANDDT is the protocol-milestone randomization date.
- LSTALVDT is the latest usable calendar day across DM, DS, EX, AE, VS, LB, SV, CM, and MH. Partial dates are ignored.
- HEIGHTBL and WEIGHTBL are the latest non-missing height and weight on or before TRTSDT. BMIBL is weight divided by height in metres squared.

ADAE

- ASTDT and AENDT impute a partial AE start or end. A missing day starts on day 01 and a missing month starts on 01 January, then moves forward to TRTSDT when that treatment day could fall inside the partial value. An end moves to the last day of the month or to 31 December, then back to TRTEDT when that day could fall inside the partial value. ASTDTF and AENDTF are D or M when a part was imputed.
- ASTDY and AENDY are relative days against TRTSDT. ADURN is the inclusive duration in days and ADURU is DAYS.
- TRTEMFL is Y when the start is on or after TRTSDT and on or before TRTEDT plus 30 days.
- AOCCIFL, AOCCPFL, and AOCCSFL mark the first treatment-emergent row within the subject, within preferred term, and within body system. The sort is analysis start date, then AESEQ.
- ASEV and ASEVN keep severity, with MILD 1, MODERATE 2, and SEVERE 3. AREL and ARELN keep causality, with NONE 1, REMOTE 2, POSSIBLE 3, and PROBABLE 4.
- TRT01A, TRT01AN, SAFFL, AGE, SEX, and RACE are copied from ADSL.

ADVS

- Collected parameters are SYSBP, DIABP, PULSE, WEIGHT, HEIGHT, and TEMP. BMI is derived at each non-missing weight: use the height at the same analysis visit when it exists, otherwise the latest height on or before the weight date. The value is kilograms divided by metres squared.
- ADT, ADY, AVISIT, AVISITN, and AVAL are the analysis date, relative day, visit, visit number, and result. A collected visit is copied from VS. A BMI row keeps the visit of the paired weight.
- ABLFL marks the last non-missing result on or before TRTSDT within USUBJID, PARAMCD, and ATPTN. Ties break on the greatest AVISITN and then the greatest source sequence. BASE, CHG, and PCHG follow that row.
- ONTRTFL is Y when the analysis date falls on TRTSDT through TRTEDT.
- ANRIND uses thresholds stated in the spec, because VS has no normal ranges. SYSBP 90 to 140, DIABP 60 to 90, PULSE 60 to 100, and TEMP 36 to 37.5 are normal, inclusive. BMI below 18.5 is low, 18.5 up to but not including 25 is normal, and 25 or above is high. WEIGHT and HEIGHT have no range.
