---
name: DAG agent edits
description: Structured changes, rather than answer prose, alter the graph.
---

Only recognized operations in the reply's JSON `changes` list are applied; explanatory answer text does not mutate the graph. Invalid graph edits roll back the complete change set and are returned as a refusal; cyclic edges are rejected. `dag_normalize_changes`, `dag_mutate`, `apply_dag_changes`, and `dag_ask` in `R/dag_agent.R` implement parsing, validation, and application. Entries lacking an operation are filtered during normalization, so not every malformed or dropped proposal is reported individually.

Key changes use `edit_spec` on the `Key` column in `variable.csv`. Keys are dataset-wide; per-parameter key edits are unsupported.
