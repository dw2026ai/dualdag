---
name: Agent context
description: Coder context is isolated by variable and side.
---

Starting a side's coder session resets that variable's saved context, so prior variable conversations do not silently leak into a new task. `coder_window_reset` in `R/coder_window.R` clears the context file.

Validation context includes its own completed work list while excluding the target and its downstream descendants. It does not receive the primary side's work or conversation. `lineage_context` in `R/sides.R` builds the validation Done list and separate source-domain names.
