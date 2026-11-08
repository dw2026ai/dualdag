# Pharmaverse adaptation of the CDISC pilot ADaM specs

These specs are derived from the CDISC SDTM/ADaM pilot (`cdiscpilot01`) specs in `examples/cdiscpilot01`. The pilot files are unchanged. Each spec was edited so the references that remain point at [pharmaversesdtm](https://cran.r-project.org/package=pharmaversesdtm) 1.5.0 (Apache License 2.0). CDISC's Terms of Use require attribution when the pilot material is distributed.

## Where the SDTM data lives

The exported transports are outside this repository, in `~/dualdag-demo/pharmaversesdtm` by default.
Regenerate them with `Rscript tools/adapt_to_pharmaverse.R export`.
Member names of 8 characters or fewer are SAS V5 transport files. Longer names are SAS V8, because V5 rejects them. Column labels are kept.
`sdg_db` and `smq_db` are MedDRA lookup tables and were not exported.
pharmaversesdtm does not ship the pilot `qs` or `sc` domains. It ships `qs_metabolic`, `qs_ophtha`, and `sc_ophtha`, which are different studies, so those names were not substituted.

## Regenerate the specs

```sh
Rscript tools/adapt_to_pharmaverse.R export
Rscript tools/adapt_to_pharmaverse.R
```

Each `spec/<dataset>/` folder has `<dataset>_spec.xlsx` (the pilot sheets plus `Changes`) and `variable.csv` / `parameter.csv` for dualdag.

## Counts

| Dataset | Variables before | Variables after | Parameters before | Parameters after | Renamed |
| --- | ---: | ---: | ---: | ---: | ---: |
| ADSL | 48 | 46 | 0 | 0 | 0 |
| ADAE | 55 | 55 | 0 | 0 | 0 |
| ADLBC | 46 | 46 | 36 | 36 | 0 |
| ADLBH | 46 | 46 | 34 | 34 | 0 |
| ADLBHY | 43 | 43 | 6 | 6 | 0 |
| ADQSADAS | 40 | 33 | 15 | 0 | 1 |
| ADQSCIBC | 36 | 33 | 1 | 0 | 1 |
| ADQSNPIX | 41 | 34 | 14 | 0 | 1 |
| ADTTE | 26 | 26 | 1 | 1 | 0 |
| ADVS | 34 | 34 | 6 | 6 | 0 |

## Transports to upload with each dataset

SDTM files are in the pharmaverse export directory. Another ADaM dataset is this adapted spec, not a pharmaverse transport. A dataset is listed only when a remaining Source or Derivation still names it.

| ADaM dataset | Upload |
| --- | --- |
| ADSL | dm.xpt, sv.xpt, ex.xpt, vs.xpt, mh.xpt, ds.xpt |
| ADAE | adsl spec, ae.xpt |
| ADLBC | adsl spec, lb.xpt |
| ADLBH | adsl spec, adlbc spec, lb.xpt |
| ADLBHY | adsl spec, adlbc spec, lb.xpt |
| ADQSADAS | adsl spec |
| ADQSCIBC | adsl spec |
| ADQSNPIX | adsl spec |
| ADTTE | adsl spec, adae spec |
| ADVS | adsl spec, vs.xpt |

## Blockers

These rows were kept because they are keys or `USUBJID` / `STUDYID` / `PARAMCD` / `PARAM` / `AVAL`. The missing reference was removed from Source and Derivation and recorded on `Changes` and `Notes`.

- ADQSADAS.ADT: QS.QSDTC
- ADQSADAS.AVAL: QS.QSSTRESN
- ADQSCIBC.ADT: QS.QSDTC
- ADQSCIBC.AVAL: QS.QSSTRESN | QS.QSTESTCD
- ADQSNPIX.ADT: QS.QSDTC
- ADQSNPIX.AVAL: QS.QSSTRESN

Changes in total: 59 deleted, 3 renamed, 6 text-updated.

