# Model tiers

DualDag uses two model tiers per provider. The stored `model` key is always the low-end model; `strong_model` is always the high-end model. Blank values are filled from the provider defaults. Existing configurations keep their saved `model` as the low-end choice and receive the high-end default when one exists. A provider without defaults may use its saved low-end model as the high-end fallback.

| Provider | High-end (`strong_model`) | Low-end (`model`) |
| --- | --- | --- |
| `claude_cli` | `claude-opus-5-5` | `claude-sonnet-5-5` |
| `anthropic` | `claude-opus-5-5` | `claude-sonnet-5-5` |
| `codex_cli` | `gpt-6.1-terra` | `gpt-6-luna` |
| `openai` | `gpt-6.1-terra` | `gpt-6-luna` |
| `grok_cli` | `grok-4.7` | `grok-4.7` |
| `openai_compatible` | User enters a model | User enters a model |

High-end models are used for DAG extraction and the App Admin CLI. Low-end models remain the default for other agents and per-variable work.
