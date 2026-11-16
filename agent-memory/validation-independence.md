---
name: Validation independence
description: Validation is independently programmed with broad source access.
---

Validation source frames are not lineage-filtered, and the validation prompt permits any available source domain or same-side built variable. The spec gives context rather than limiting the independent derivation, supporting double programming. `side_filter_source_frames` in `R/sides.R` returns all sources for validation; `side_system_prompt` and `side_user_prompt` describe its role.

Validation receives only validation-side code, cards, and work; it never receives primary code. It gets its target row, its own completed work list, and names-only catalogs for other domains. `lineage_context` in `R/sides.R` builds this bounded context.
