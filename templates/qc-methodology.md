# QC Methodology — <DATASET> (example: ADSL)

> One file per dataset. Write it, then upload it. Each dataset keeps a single copy. You maintain the version.
> When the two sides do not match, the model investigates using this document and reports back. You decide what to do.
> Delete the prompt text in each section after you fill it in.

## 1. Scope
- Dataset:
- Products or study types this covers:
- Last updated:

## 2. Compare settings
- Merge keys (example: `USUBJID`; for BDS also `PARAMCD` + `AVISITN`):
- Compare rules (numeric precision, case, leading and trailing spaces, date format, how missing values count):

## 3. What to look at first after a mismatch
- Order of checks (example: count and share of differences, then whether they cluster on certain subjects or visits, then a few records traced back to source data):
- How many records to spot-check, and how to pick them:

## 4. Common causes and how to judge them
| Cause | Typical sign | How to confirm |
|---|---|---|
| Primary code does not follow the spec | | |
| Validation misread the spec | | |
| Spec is ambiguous or wrong | | |
| Source data (missing, duplicate, or inconsistent) | | |
| An upstream variable cascaded the difference | | |

## 5. Data-check habits
- Source-data points that must be checked (example: duplicate records, partial dates, mismatches across domains):
- Pitfalls specific to this dataset:

## 6. Report format
- What the report must include (example: difference count, example records, cause, evidence, suggested handling):
- How to write a point you are not sure about:

## 7. Not allowed
- Example: do not change the validation derivation so it matches primary. Do not state a cause when you are not sure.
