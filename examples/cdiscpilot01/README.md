# CDISC pilot ADaM spec (cdiscpilot01)

This example is derived from the CDISC SDTM/ADaM Pilot Project (`cdiscpilot01`). The source repository is [https://github.com/cdisc-org/sdtm-adam-pilot-project](https://github.com/cdisc-org/sdtm-adam-pilot-project). CDISC's Terms of Use require attribution back to CDISC when the pilot data is distributed. This directory does not contain the pilot transport files. It contains a dualdag variable and parameter spec built from the pilot ADaM Define-XML.

## Where the data lives

This spec was built from the CDISC pilot submission package. Inside that package, the study folder is:

```text
updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01
```

- ADaM Define-XML and ADaM transports: `analysis/adam/datasets/` (`define.xml`, `adsl.xpt`, and the other `ad*.xpt` files).
- SDTM transports and the SDTM define: `tabulations/sdtm/`.
- ADaM programs: `analysis/adam/programs/`.

Do not copy those `.xpt` files into this repository.

## Regenerate

From the dualdag repository root, with an R that has `xml2`, `haven`, and `openxlsx2`:

```sh
PILOT=/path/to/updated-pilot-submission-package/900172/m5/datasets/cdiscpilot01
Rscript tools/define_to_spec.R "$PILOT/analysis/adam/datasets/define.xml" examples/cdiscpilot01/spec
```

That writes the combined spec and one folder per ADaM dataset:

- `spec/cdiscpilot01_adam_spec.xlsx` — all ten datasets. Sheets: `Variable`, `Parameter`, `Datasets`, `ValueLevel`, `Codelist`, `Methods`, `Notes`.
- `spec/variable.csv` and `spec/parameter.csv` — the same two tables, UTF-8.
- `spec/<dataset>/` — one ADaM dataset. `<dataset>_spec.xlsx` has the same seven sheets filtered to that dataset, plus `variable.csv` and `parameter.csv`. `ADSL` and `ADAE` have a header-only `parameter.csv`. dualdag reads the two CSVs. Upload one folder when you want a single dataset.

`Variable` has one row per `ItemGroupDef` item (the dataset columns), in define order. The 903 value-level items repeat names inside a dataset, so they are on `ValueLevel`, not on `Variable`. A per-dataset `Codelist` sheet keeps only codelists referenced by that dataset's variables or value-level items. `Methods` keeps computation methods whose analysis result names that dataset; `ADVS` has none. `Notes` keeps that dataset's notes plus the study-wide notes.

## Transports to upload with each dataset

`Source` is filled only when a define comment is exactly one `DOMAIN.VARIABLE` token, such as `DM.AGE` or `ADSL.TRT01P`. A longer derivation stays in `Derivation` with `Source` blank. The files below are the union of those tokens, excluding the dataset itself. Words such as "i.e." and "e.g." are not dataset references. SDTM files are `$PILOT/tabulations/sdtm/<name>`. Other ADaM files are `$PILOT/analysis/adam/datasets/<name>`.

| ADaM dataset | Upload with the spec |
| --- | --- |
| ADSL | `dm.xpt`, `ds.xpt`, `ex.xpt`, `mh.xpt`, `qs.xpt`, `sc.xpt`, `sv.xpt`, `vs.xpt` |
| ADAE | `adsl.xpt`, `ae.xpt` |
| ADLBC | `adsl.xpt`, `lb.xpt` |
| ADLBH | `adsl.xpt`, `adlbc.xpt`, `lb.xpt` |
| ADLBHY | `adsl.xpt`, `adlbc.xpt`, `lb.xpt` |
| ADQSADAS | `adsl.xpt`, `qs.xpt` |
| ADQSCIBC | `adsl.xpt`, `qs.xpt` |
| ADQSNPIX | `adsl.xpt`, `qs.xpt` |
| ADTTE | `adsl.xpt`, `adae.xpt` |
| ADVS | `adsl.xpt`, `vs.xpt` |

## What the converter decided

These calls are one row each on the `Notes` sheet (plus 40 variables whose empty comment was filled from value-level text, and 6 whose empty comment points at a codelist).

- Variable rows are the 415 dataset columns. Define-XML has 1318 `ItemDef` and 1318 `ItemRef`; 903 of the item references are value-level metadata.
- No item points at `def:ComputationMethodOID`. The 42 computation methods are analysis-display text on the `Methods` sheet. They are not pasted into `Derivation`.
- Dataset-variable `Origin` is the word `Derived`, so it is not repeated in `Derivation`.
- Keys come from `ItemGroupDef` `DomainKeys` (this define has no `KeySequence`). `ADLBHY` lists `LBSEQ`, which is not a column. The key used is `USUBJID`, `PARAMCD`, `AVISIT`.
- Parameter rows are the `PARAMCD_*` / `PARAM_*` codelists (113 parameters on the eight BDS datasets). `adamref:Parameter` has 184 analysis-result entries and is not that catalog. `POLICHR` and `_POLICHR` are the only ParamCD values from that list that do not appear in any dataset codelist.
- `ADTTE` `TTDE` codelist text is "Time to Derm. Event or End of Study". The transport's `PARAM` is "Time to First Dermatologic Event".
- Labels matched the transport label attributes. The transports exposed no length attribute to compare with define `Length`.
