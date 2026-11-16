---
name: Primary inputs
description: Which source and work columns the primary side can use.
---

Primary source frames are filtered to source references from DAG lineage, key-variable specs, and literal domain-variable references in the current variable's Source or Derivation; the code does not infer columns from prose such as “drug.” This keeps source inputs auditable. `side_primary_source_refs` and `side_explicit_spec_source_refs` in `R/sides.R` build that set; `side_filter_source_frames` applies it. For example, ADPPK `PC.PCTEST` is absent from Keys inputs unless one of those references includes it.

For non-ADSL targets, primary receives the complete ADSL frame when available, regardless of lineage. This is a deliberate shared covariate source. `side_filter_source_frames` in `R/sides.R` implements the exception.

Primary code is encouraged to start from lineage, but an outside-lineage column already present in its inherited work frame is allowed and produces a warning, not a stop. Source filtering remains in effect. `side_system_prompt` and `side_user_prompt` in `R/sides.R` state this; `lineage_code_warnings` in `R/server.R` reports uses.
