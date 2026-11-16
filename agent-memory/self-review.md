---
name: Self-review
description: Generated programs receive an isolated review and at most one rewrite.
---

After a successful side run, self-review is enabled and a verdict of unexpected can trigger one regeneration. The review sees only that side's inputs and results; the first successful run is restored if the retry fails. This catches derivation mismatches without comparing sides during review. `run_side_job` in `R/server.R` and `self_review_run` in `R/self_review.R` implement the flow; legacy config switches do not disable it.
