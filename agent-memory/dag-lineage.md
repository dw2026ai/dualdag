---
name: DAG and lineage
description: The graph records dependencies and determines lineage context.
---

The graph JSON is the source of truth; the adjacent readable DAG Markdown is a view. Edges represent upstream dependencies. Lineage selection follows those edges (including parameter-layer relationships) and supplies the context used for generation. `save_graph` in `R/dag.R` writes both graph and view; `lineage_selection` and `lineage_context` in `R/sides.R` assemble prompt context.

The displayed lineage endpoint and generation cards are based on lineage selection, but presentation can differ: cards summarize completed ancestors and may replace expanded spec/work details. This keeps prompts compact while retaining the allowed lineage scope. `lineage_context` in `R/sides.R` handles cards; the lineage API is served by the `/api/node/lineage` branch in `handle_api` in `R/server.R`.
