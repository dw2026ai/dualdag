---
name: Layered datasets
description: How parameterized BDS work is grouped and ordered.
---

BDS execution is planned one parameter layer at a time in parameter dependency order. Each layer runs its Records step and then its member variables. This preserves parameter dependencies and makes layer failures isolatable. `fullrun_plan` in `R/fullrun.R` and `batch_order_for` in `R/server.R` construct that order.

Parameters classified as carry-over are combined in one shared CARRY_OVER layer; extraction treats their rows as one grouped model request. This supports shared row handling and cross-member dependencies. `extract_carry_job` and `extract_dependencies` in `R/deps_extract.R` implement grouped extraction; `graph_skeleton` maps group members to the shared layer.
