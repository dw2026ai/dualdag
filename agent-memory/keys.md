---
name: Keys
description: Dataset keys are defined only by the spec Key column.
---

The variable.csv Key column is the only source of keys. Positive integer Key values are ordered numerically; blank means the variable is not a key. Key resolution always puts USUBJID first, even when absent from the spec keys or assigned another number. The DAG panel omits its USUBJID ordering notice for the conventional CDISC assignment STUDYID Key 1 and USUBJID Key 2; other assignments still show the dismissible notice. For layered datasets, use the dataset keys and PARAMCD: USUBJID first, then PARAMCD unless it already has a Key number, then remaining spec keys by Key number. Key changes are edits to the spec and can be undone with other DAG agent edits. Keys cannot vary by parameter.
