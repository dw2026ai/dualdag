Demo data attribution

The demo SDTM XPT files are fetched from pharmaversesdtm 1.5.0 on CRAN:
https://cran.r-project.org/package=pharmaversesdtm
pharmaversesdtm is distributed under the Apache License 2.0.

The ADVS project's adsl.xpt is derived from the pharmaverse ADaM reference build
in tools/pharmaverse_demo_ref.R, using the committed ADSL demo spec.

The ADPPK project fetches EX, PC, VS, and LB from pharmaversesdtm 1.5.0. Its
adsl.xpt is built from the same committed ADSL demo spec. ADPPK derivations are
documented from the public pharmaverse example program and its metadata workbook:
https://pharmaverse.github.io/examples/adam/adppk.html
https://github.com/pharmaverse/examples/blob/main/adam/adppk.R
https://github.com/pharmaverse/examples/blob/main/metadata/pk_spec.xlsx
The workbook provides labels, types, lengths, and controlled terminology, but
has no derivation methods or parameter-level specification for ADPPK.
The independent reference build is tools/pharmaverse_adppk_ref.R and uses only
the bundled spec and EX, PC, VS, LB, and ADSL inputs; it does not call admiral.
To compare it with the public program, use a checkout of pharmaverse/examples:
Rscript tools/compare_pharmaverse_adppk.R /path/to/pharmaverse/examples
That comparison runs the upstream program separately, aligns records by RECSEQ,
and reports each differing column with its bundled derivation text. admiral,
metacore, metatools, xportr, pharmaverseadam, and their dependencies are needed
only for this development comparison, not by dualdag or the demo launcher.
