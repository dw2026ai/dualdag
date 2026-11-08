# dualdag — architecture & contracts (v0.1)

Hard constraint: user has only R (>= 4.2) + a browser. No Node/Python/build tools.
Frontend = hand-written static `www/index.html`, `www/app.js`, `www/style.css` (no frameworks, no CDN).
DAG layout/drawing is done in plain JS + SVG. Graph algorithms in R with **igraph**; comparison with **diffdf**.

Test R on this dev machine: `~/.local/mamba/root/envs/r/bin/Rscript` (packages httpuv jsonlite httr2
callr later promises openssl haven igraph diffdf dplyr testthat already installed).

## File layout & ownership

```
run.R                 launcher (Worker A)
DESCRIPTION, README.md, README_zh.md, LICENSE (MIT)   (Worker A)
R/utils.R             %||%, to_json, from_json, read_text, write_text, now_iso, random_token, safe_path (A)
R/deps.R              dualdag_required_packages(), dualdag_ensure_deps() (A)
R/config.R            ~/.dualdag/config.json, ~/.dualdag/api_key (600), env DUALDAG_LLM_API_KEY (A)
R/server.R            httpuv server, token/Host/Origin checks, CSP, static, API routing, bg_promise (A)
R/project.R           project folder, uploads, persistence (.dualdag/state/), external-change detection (A)
R/spec.R              read/validate variable.csv + parameter.csv, templates (A)
R/llm.R               provider adapters (openai / anthropic / openai_compatible / grok_cli / claude_cli / codex_cli), parse_json_reply (B)
R/deps_extract.R      LLM dependency extraction (F5) (B)
R/dag.R               igraph build, checks (F6, F7), topo order, node status machine (F9a-c, F9i) (B)
R/dag_agent.R         readable dag_<DS>.md, on-demand node lookup, DAG agent ask/apply
R/sides.R             primary/validation code generation prompts + isolated callr runs (F9e, F9f) (B)
R/cards.R             per-side variable cards: computed facts, descriptions, parent hashes, warnings, edits, backfill
R/compare.R           diffdf per-variable compare by Key, mismatch QC report (F9b③, F9g) (B)
R/inspect.R           whole-dataset code assembly and local mismatch-input table (no LLM)
R/qc.R                ~/.dualdag/qc/<DATASET>.md store (F9g) (B)
R/export.R            export dataset xpt/rds with labels/types/lengths (F11) (B)
www/index.html, www/app.js, www/style.css   (Worker C)
templates/variable.csv, templates/parameter.csv, templates/qc-methodology.md
tests/testthat/*.R    each worker writes tests for its own files
```

Layered dependency graphs apply when a dataset has parameter.csv rows. They carry `layered: true`, one
`records` node per PARAMCD, per-parameter variable nodes (including structural variables), and
dataset-level variables from ADSL/DM or STUDYID. `graph$layers` records each records node and its
members; `groups` remains available to existing clients. Parameter graph edges carry `uses` pairs
(`from_var`, `to_var`). A pre-layered saved graph is exposed by `/api/state` with
`datasets[DS].reextract_required: true`; merely opening a project does not alter statuses.

All R files are `sys.source()`d into one environment `E`. No package install of dualdag itself.
Every function must be callable without the server for unit tests. Use base R + listed packages only.

## Project folder (per study)

```
<project>/
  spec/variable.csv, spec/parameter.csv      uploaded spec (latest only)
  data/<name>.xpt                            uploaded SDTM / external ADaM, lowercase file name = domain (dm.xpt -> DM)
  .dualdag/state/state.json                  persisted app state (see below)
  .dualdag/state/graph_<DATASET>.json        dependency graph per dataset (source of truth)
  .dualdag/state/dag_<DATASET>.md            readable DAG rewritten on every save_graph
  .dualdag/experts/<DATASET>_dag.json        DAG agent turns and user corrections
  .dualdag/runs/<DATASET>/<VAR>/<side>/      side = primary|validation: code.R, log.txt, out.rds, card.json, notes.json, code_note.json
  .dualdag/runs/<DATASET>/_work/<side>.rds   key rows plus every Data matched column on that side
  .dualdag/llm_log/                          one JSON file per LLM call (no API key)
  .dualdag/runs/<DATASET>/<VAR>/compare.json, qc_report.md
  output/<dataset>.xpt|.rds                   exports
```

Default project dir: `~/dualdag-projects/default` (created if missing); `--project=DIR` / `DUALDAG_PROJECT`.
Config dir `~/.dualdag/` (override `DUALDAG_HOME` for tests). QC docs in `~/.dualdag/qc/<DATASET>.md`.
Env vars: `DUALDAG_PORT` (default 7979), `DUALDAG_PROJECT`, `DUALDAG_NO_BROWSER=1`, `DUALDAG_INSTALL_DEPS=1`,
`DUALDAG_URL_FILE`, `DUALDAG_HOME`, `DUALDAG_LLM_API_KEY`, `DUALDAG_FAKE_LLM=1` (see LLM below).

## Variable cards

Each successful side run writes `card.json` beside `out.rds`. Its `facts` are computed from the saved result and code: R class, row and NA counts, a stable sample of non-missing values, categorical levels, numeric or Date bounds, build time, hashes, and direct parent code hashes. The coder supplies `description`; a local `user_description` overrides it for display and prompts and persists through regeneration. Reset or removed results delete cards, while opening an older Data matched project backfills facts-only cards. Generation, self-review, and R-agent prompts show cards for all lineal ancestors that have one on the requested side, nearest first, walking through carded nodes. Ancestors without a card contribute their spec row or available work lines. A card replaces that ancestor's expanded context, while the allowed lineage scope remains unchanged. Validation uses validation-side cards only; its Done list and target spec row remain. Type and stale-parent warnings never block a run.

## Spec model (R/spec.R)

`read_spec(variable_path, parameter_path = NULL)` returns
```
list(variables = data.frame(row, Dataset, Variable, Label, Type, Length, Key(int|NA), Source, Derivation),
     parameters = data.frame(row, Dataset, PARAMCD, PARAM, Source, Derivation),
     datasets = character(), problems = list(<issue>...), hash = "<md5 of both files>")
```
`row` = CSV line number (header = line 1, first data row = 2). Dataset/Variable uppercased & trimmed.
Structural problems (missing columns, duplicate Dataset+Variable, dataset with no Key, Key not 1..n) become issues.

## Issue object (used everywhere, F8)

```json
{"type": "cycle|missing_ref|ambiguous|unresolvable|orphan|no_key|structure|missing_data|missing_var",
 "dataset": "ADSL", "variables": ["A","B"], "file": "variable.csv", "row": 5,
 "text": "<original spec text>", "message": "...", "suggestion": "..."}
```

## Dependency graph (R/deps_extract.R, R/dag.R)

Graph JSON per dataset (`graph_<DS>.json`):
```json
{"dataset":"ADVS","spec_hash":"...",
 "nodes":[{"id":"ADVS.AVAL","var":"AVAL","paramcd":null,"kind":"variable"},
          {"id":"ADVS.AVAL@WEIGHT","var":"AVAL","paramcd":"WEIGHT","kind":"param_variable"}],
 "groups":[{"id":"ADVS@WEIGHT","paramcd":"WEIGHT","members":["ADVS.AVAL@WEIGHT", "..."]}],
 "edges":[{"from":"ADVS.BASE","to":"ADVS.CHG","evidence":"CHG = AVAL - BASE","origin":"llm"}],
 "external":[{"ref":"VS.VSSTRESN","domain":"VS","var":"VSSTRESN","used_by":["ADVS.AVAL"],"evidence":"..."}],
 "unresolved":[<issue>]}
```
Older `graph_*.json` files may still contain `confirmed`, `confirmed_hash`, or `origin: "user"`.
Those fields are ignored on load and dropped the next time the graph is saved. There is no confirm step.
Edge direction: `from` = upstream dependency, `to` = the variable that depends on it. External refs
(SDTM domains and other ADaM datasets) are NOT nodes (F7a); they are listed under `external`.
Node ids: `<DS>.<VAR>` or `<DS>.<VAR>@<PARAMCD>` for parameter-level members (F9i). A parameter group may
depend on another (e.g. BMI on WEIGHT, HEIGHT) through member edges.
A node may carry `key` (true or false) and `label`. Those fields survive load and save. `key` overrides the
spec Key column for the DAG file and the display lane. The lane is still not written back from layout.
`save_graph` also writes `dag_<DS>.md` beside the JSON: one line per node (id, label, key, no level), direct
parents with one-line evidence, and a User corrections section. The markdown is a view. The JSON stays the
source of truth. Re-extract receives the same corrections so it does not undo them.

Functions:
- `extract_dependencies(cfg, spec, dataset, catalog = NULL, project = NULL)` -> graph (calls LLM, returns structured list with evidence; LLM
  marks things it cannot resolve as `ambiguous`/`unresolvable` issues). When `project` is set and the dataset
  has DAG-agent corrections, those sentences are appended to the extraction user prompt.
- `check_graph(graph, spec, data_index)` -> `list(issues, order = topo order or NULL)`: igraph cycles
  (`igraph::is_dag`, find cycles via SCCs), edges to missing nodes, orphans (no in/out edges), external refs
  missing in uploaded xpt (`missing_data` = whole file missing, `missing_var` = variable absent).
- `data_index(project)` -> `list(<DOMAIN> = character(colnames))` from uploaded xpt (read header only via haven).

## Node status machine (R/dag.R) — F9a/b/c

Per node `status` in state.json:
```json
{"level":"none|L2|L3", "problem": null | {"stage":"validate|generate|run|compare","message":"..."},
 "warnings": null | ["..."], "upstream_changed": null | ["<node id>", "..."],
 "same_source": false, "updated": "<iso>", "sides": {"primary": {...}, "validation": {...}}}
```
Rules: Generate is allowed in every state unless that variable is already running (HTTP 409). A variable at `none`, or with a legacy validate-stage problem, stays Not generated while generation starts; Generate clears that legacy problem. Blocking spec issues are stored on `warnings` and do not block generation or demote anyone. After a primary run, a source column the code reads that this variable's Source and Derivation do not name is also a warning, not a failure: `Primary uses DM.RFSTDTC, which the spec does not name for this variable`. Keys are allowed. A work column is allowed when the spec names that variable. A primary note that mentions that source suppresses the warning. The code tab shows these next to the primary pane. The state stays Not generated until both sides generate and run successfully, which moves it to Generated. Compare moves Generated to Data matched on an exact match. A failed generate or run leaves the variable Not generated with `problem`; a failed compare leaves it Generated with `problem`. `actions(node)` always includes `generate`, plus `compare` while Generated and `investigate` when the problem stage is compare. `ready` is true when an action is available and the state is not Data matched. Legacy stored `L1` values are read as `none` without changing other status fields.

Dependencies are advisory. Generating, re-running, resetting, or dropping a variable below Data matched does not reset downstream variables. Each descendant gets `upstream_changed` (the upstream node ids) and keeps its level and results. That flag is cleared the next time the descendant is generated or re-run. A spec-row hash change still resets that variable itself to `none`. A changed or removed source xpt still resets each Generated or Data matched variable itself to `none`, not by walking the graph. Group status = minimum member state.

## Isolated sides (R/sides.R) — F9e/F9f

`generate_side(cfg, side, spec, graph, node, upstream_vars, qc_feedback=NULL, user_instruction=NULL)` -> code string.
The legacy `qc_feedback` argument is ignored. The saved QC report is never added to either generation prompt;
a user may copy its advice into a side note.
- primary prompt: follow the spec's Source and Derivation step by step, in the order written, taking each value from exactly the dataset and variable named there. If the spec is ambiguous, follow its wording and say so in the top comment. A note overrides the spec only for what the note says. Validation prompts support an independent derivation and prefer another source,
  never sees primary code; must report `same_source: true|false` in JSON reply. The validation prompt is not held to the primary spec path.
- Each side has one R expert. The system prompt starts with `You are the primary-side R expert for dataset X`
  (validation likewise) and does **not** include the whole-spec `DATASET_SPEC` prefix. That prefix remains on
  `qc_investigate` and on dependency extraction. The user prompt keeps the target contract (`VAR` / `SOURCE` /
  `DERIVATION`, work, src, keys, notes override the spec, top comment). Generation and node-specific Ask send only
  the target's lineage frontier. The target has its full spec row. A parent already in that side's work stops the walk
  and contributes its name, label, type, actual R class, sampled values, missing count, and categorical distinct values.
  A source-domain variable also stops the walk and contributes its catalog row, class, sample, and categorical values.
  A not-yet-built dataset parent contributes its full
  spec row and the walk continues. Work keys and dataset rules are included. Validation alone receives the full
  column catalog for source domains in that lineage; unrelated domains and the variable index are excluded.
  Each non-key lineage column contributes up to 20 nonmissing values sampled across the entire column with a stable
  seed per dataset and column, displayed in row order using R formatting, plus the total NA count. An all-NA column
  says so; keys disclose their class only. `distinct_levels()` separately counts character, factor, and logical
  values only, with NA as a level; at most 20 levels are listed with counts, otherwise a single high-cardinality
  line is sent. Source domains are read once per prompt.
  Primary runtime inputs are limited to source domains and variables in the target lineage plus join columns, and
  work/upstream contains only keys and lineage variables. Before a primary run, known source or dataset references
  outside that lineage fail with `DUALDAG_MISSING_DEPENDENCY: ...` and an actionable graph message. Validation uses
  full source data and work as before; its prompt uses the spec and DAG as context and may choose any built same-side
  variable or source domain. Validation has no lineage stop or possible-missing-edge warning. Its prompt includes
  full catalogs for lineage domains and a names-only catalog for other domains, capped at 4000 characters.
  Primary lookup is restricted to ids in the target lineage; explanations and validation lookup remain unrestricted.
  The other side's code, logs, notes, results, and thread never enter the prompt.
- The expert's memory is `<project>/.dualdag/experts/<DATASET>_<side>.json`. Only user messages and agent replies
  are kept; older ask turns are read into the same conversation. Generate and the agent prompt use up to 10 recent turns.
- Code contract: the code runs in a fresh R process (`callr::r`) whose working dir is that side's run folder;
  pre-defined objects: `src` = named list of data.frames (all uploaded xpt, names uppercase domain),
  `work` = the cumulative dataset for this side (key rows plus every variable already Data matched on this side),
  `upstream` = the same data.frame as `work`,
  `keys` = character vector of key var names. The user prompt states `work has N rows and columns: ...`.
  Both prompts name the packages already attached for that side (`Packages already attached on this side: ...`)
  and say not to call `library()` or use any other package. base, stats, utils, and methods are always available
  and are not listed. The project default is `dplyr`.
  Code must create `result`: the key columns plus the target variable. When `work` has rows, keep those rows.
  Every generated program ends with ordinary R that prints `rows: N`, then `summary(result$X)` for numeric or Date
  targets. Character, factor, or logical targets print `table(result$X, useNA = "ifany")` when there are at most
  20 distinct values, or `X: more than 20 distinct values` otherwise. This is generated source code; the runner
  does not add output lines to the Run log.
  When no key variable is Data matched on this side, `work` has 0 rows and the program builds the row frame from the
  source domain named in the specification, including for a variable that is not a key. Upstream variables
  that are not in `work` are simply absent; the program may fail with an R error, and that error is the result.
  A result with 0 rows fails the run with `produced 0 rows` only when `work` has rows. User notes override the spec and the side's default strategy; the
  code starts with a short comment for each note (latest wins). Regeneration sends that side's current
  code and every note, never the other side. Before the process starts, the runner checks packages named by
  `library()`, `require()`, `requireNamespace()`, `pkg::`, and `pkg:::` as well as the side's attached package
  list. If a package is missing, the program stays stored and the run log asks the user to install it in
  `R.home()`; Dualdag never installs packages. A non-literal `library()` call is rejected. Installed packages
  may be used directly, and `library()` in a program attaches a package for that program. Changing the stored
  attached list does not reset any variable.
  The list is `<project>/.dualdag/state/r_packages.json` (`r_packages`, plus `r_packages_primary` and
  `r_packages_validation` when `override` is true). The child process attaches each listed package with
  `library()` and suppresses startup messages. Runner saves `out.rds`, `log.txt` (one comment header
  `# R x.y.z; packages: dplyr 1.1.4, ...`, then the program's R output without
  source echo: printed values, messages, warnings, and R's error text),
  and `code_note.json` (which note produced the current code). A post-run check such as `produced 0 rows`
  is the run error, not a line in `log.txt`. On Data matched the column is merged into
  `_work/<side>.rds`; a downgrade removes it. Source and result frames drop haven labels and formats while
  retaining Date, POSIXct (including timezone), and difftime classes; factors become character. Opening a
  project rebuilds both work files from saved side results, repairing work files created before this fix.
  Compare treats a Date and its underlying numeric value as different types. Descendants are marked
  `upstream_changed`, not demoted.
- `run_side(project, dataset, node, side, code, timeout)` -> `list(ok, log, out_path, error)`; two sides run in
  two separate `callr::r_bg` processes concurrently; one failing does not affect the other.

## Compare (R/compare.R)

For character target variables, `blank_equals_na` is always true: `""` and whitespace-only text are
normalized to `NA` in comparison copies before diffdf. Stored side `out.rds` files remain unchanged;
numeric values retain exact NA behavior. The summary reports how many aligned rows this reconciled.
Stored legacy comparison settings are ignored. Stored mismatches are re-compared locally when a project opens.

`compare_node(project, dataset, node, keys)` -> `list(match = TRUE|FALSE, n_primary, n_validation,
n_diff, diffs = data.frame(first 200 rows: keys, primary, validation), summary)` using `diffdf::diffdf(base=primary,
compare=validation, keys=keys)`. No tolerance; exact. A side with 0 rows is not a match (`primary result has 0 rows`).
A row count different from that side's work dataset is not a match, and that check runs only when that side's work has rows. Both sides must still have more than 0 rows and equal key sets to match. Key columns are coerced to character before
`diffdf`. A key storage difference is reported as `key USUBJID types differ: character vs logical` rather than the
raw diffdf error. Opening a project drops a Data matched node whose `out.rds` has 0 rows to Not generated with problem
`produced 0 rows` and marks descendants `upstream_changed`; a missing `out.rds` is left as it is. On mismatch, `qc_investigate(cfg, ...)` sends the QC doc of the
dataset + diff rows + relevant source rows to LLM and returns a markdown report (no auto-fix). Its system
prompt starts with the whole-spec `DATASET_SPEC` prefix. Side generation does not.

## LLM (R/llm.R)

Providers: `openai`, `anthropic`, `openai_compatible`, `claude_cli`, `codex_cli`, and `grok_cli` (the local `grok` command, already logged in; no API key). `grok_cli` defaults to model `grok-4.7` when the model field is empty (`grok-4.7`, `grok-4.7-build-fast`, `grok-4.6`, `grok-4.5`). The call is `grok --prompt-file … --system-prompt-override … --tools "" --no-subagents --disable-web-search --max-turns 1 --output-format json`. A system prompt longer than 100000 bytes is prepended to the prompt file instead of the argv. `claude_cli` runs the local `claude` command (Claude Code login; no API key; never `--bare`): `claude -p --output-format json --model <m> --tools "" --no-session-persistence --strict-mcp-config --system-prompt <system>`, with the user prompt on stdin; the answer is the `result` field, `is_error` or a non-zero exit is an error. Default model `claude-sonnet-5-5` (`claude-opus-5-5`, `claude-haiku-5-5` also offered). `codex_cli` runs the local `codex` command (ChatGPT login; no API key). An empty model omits `-m` so Codex keeps its own default; settings offer `(CLI default)`, `gpt-6-luna`, `gpt-5.6-terra`, `gpt-5.6-luna`, and a free-text model. The call is `codex exec --skip-git-repo-check --sandbox read-only --ephemeral --color never --json -c approval_policy="never" [-m <model>] [-c developer_instructions="<system>"] --output-last-message <file> -`, with the user prompt on stdin. The system text is a TOML `developer_instructions` value when that value is at most 100000 bytes; otherwise it is prepended under a `System instructions` header. The answer is the last-message file. Model and usage come from the JSONL events when those events report them; otherwise the model is `cfg$model` or `codex default`. When `cli_use_login` is true (the default), `claude_cli` removes `ANTHROPIC_API_KEY` and `ANTHROPIC_AUTH_TOKEN` from the child environment and `codex_cli` removes `OPENAI_API_KEY` and `CODEX_API_KEY`, so each CLI uses its own login. `GET /api/llm/cli_status?provider=claude_cli|codex_cli|grok_cli` returns `{installed, path, version, logged_in, method, hint}` from a 15 s probe that never fails the request. Claude reads `loggedIn` and `authMethod` from `claude auth status`. Codex reads `codex login status`. Grok returns the version only and `logged_in` is null. The reply contains no account identifiers. All three CLI providers append a no-tools line to the prompt and use `cli_timeout_s` (default 600 s) instead of `llm_timeout_s`. A missing binary or a login failure is a clear error. `llm_base_url()` drops a trailing `/chat/completions` (OpenAI and compatible) or `/v1/messages` (Anthropic) so a pasted endpoint is not requested twice. The settings hint shows a base such as `https://openrouter.ai/api/v1`. `llm_chat(cfg, system, user, max_tokens, json=TRUE)` -> `list(text, model, usage)`.
When option `dualdag.llm_log` is `list(dir, task, node, side)`, every call (including the fake LLM) is written to
`<project>/.dualdag/llm_log/<timestamp>_<task>_<node>_<side>.json` with the system prompt, user prompt, reply,
model, time, and usage. The API key is redacted and is not a field of the file.
If `Sys.getenv("DUALDAG_FAKE_LLM") == "1"`, `llm_chat` returns deterministic canned replies from
`fake_llm_reply(system, user)` (used by tests and offline demo): dependency extraction via regex over
Source/Derivation looking for `DOMAIN.VAR` and same-dataset variable names; code generation returning working
simple code where possible; `TASK: side_agent` answers questions without changing code and treats messages starting
with `change`, `use`, or `make` as change requests.
`TASK: dag_ask` is `fake_dag_reply` in R/dag_agent.R: one lookup round, then an answer,
and a graph change when the message says `add edge A -> B` or `remove edge A -> B`. The ask prompt is one side only (that side's dependency chain, index, work columns, and
thread). Generate, ask, and the DAG agent may answer with only `{"lookup":["ADSL.TRTSDT"]}` (at most 10 ids).
The server appends those nodes' parents, children, one-line evidence, and spec Source and Derivation, then
asks again. At most 2 lookup rounds, then the final JSON is required. A lookup is spec and graph only. It
never includes either side's code, logs, notes, or results. The inline ancestor slice stays. Long calls run in background via `bg_promise` in server.R.

## HTTP API (R/server.R, consumed by www/app.js)

All `/api/*` need header `X-Dualdag-Token: <token>` (token from URL `#token=`). The page reads that token on every call, `decodeURIComponent`s it, and strips whitespace so a line-break space in a pasted link does not change the token. Pasting a new `#token=` into the same tab stores it and reloads. A 403 bad token shows one banner: the link token is wrong or expired, and the user should open the URL `run.R` printed. JSON in/out. Errors: `{"error": "..."}` + 4xx/5xx.
Uploads: POST JSON `{"name": "dm.xpt", "base64": "..."}` (no multipart).

| Method | Path | Body / query | Returns |
|---|---|---|---|
| GET | /api/state | | `{project, spec:{variables,parameters,datasets,hash}|null, data:[{name,domain,nrow,ncol,vars}], missing_data:{DS:[domains]}, datasets:{DS:{graph, check:{issues,order}, nodes:{id:status+actions+ready}, groups:{id:status}, dag:{turns,corrections}}}, issues:[...], config_public, busy, batch}` |
| GET | /api/info | | R version, package versions |
| GET | /api/packages | | `{r_packages, override, r_packages_primary, r_packages_validation, installed:[{name, version, installed}]}`. Missing file means `r_packages: ["dplyr"]` and `override: false`. `installed` is false and `version` is null when the package is not installed |
| POST | /api/packages | `{r_packages, override, r_packages_primary?, r_packages_validation?}` | The same object. Does not reset any variable. 400 on a package name that is not a letter followed by letters, digits, or dots |
| GET | /api/llm/cli_status | `?provider=claude_cli\|codex_cli\|grok_cli` | `{installed, path, version, logged_in, method, hint}`. 15 s probe. Grok `logged_in` is null. No account identifiers. The request still returns 200 when the CLI is missing or the probe fails |
| POST | /api/config | provider, base_url, model, cli_use_login?, api_key?, clear_api_key? | `{ok, config}`. `codex_cli` keeps an empty model (CLI default). `cli_use_login` defaults to true |
| POST | /api/config/test | | `{ok, model, reply}` |
| POST | /api/spec/upload | `{variable:{name,base64}, parameter?:{name,base64}}` | `{ok, issues}` (rebuilds graphs: extraction starts per dataset) |
| POST | /api/data/upload | `{name, base64}` | `{ok, data}` |
| DELETE-like POST | /api/data/remove | `{name}` | `{ok}` |
| POST | /api/graph/extract | `{dataset}` | graph (LLM) |
| POST | /api/graph/layout | `{dataset, positions:{nodeId:{x,y}}}` or `{dataset, reset:true}` | Saved coordinates or empty coordinates after reset. Stored in `.dualdag/state/layout_<DS>.json`; `/api/state` includes each dataset's `layout` |
| GET | /api/graph/download | `?dataset=&what=graph\|dag` | `what=graph` returns `graph_<DS>.json`. `what=dag` returns `dag_<DS>.md` (written first if missing). Anything else is rejected |
| GET | /api/node/categories | `?dataset=&node=` | `{categorical,overflow,rows:[{value,rule,primary,validation,in_spec,mismatch}]}`. Counts are computed locally from each side's latest `out.rds` for character, factor, and logical columns only; more than 20 distinct values returns `overflow` |
| POST | /api/dag/ask | `{dataset, message}` | `{answer, changes, correction, model, turns}` (async, job kind `dag_ask`). 400 when the message is empty or no API key is configured. 404 when the dataset has no graph |
| POST | /api/dag/apply | `{dataset, changes}` | `{ok, affected, edges:[{from,to,kind}]}`. Validates the batch, then `save_graph`. Unknown node, self-loop, and cycle are 400 and the file is unchanged. Sets advisory `upstream_changed` on the edge's `to` node only (no descendant cascade, level unchanged) |
| POST | /api/dag/correction/remove | `{dataset, at}` | `{ok, corrections}`. Drops the correction whose `at` matches |
| GET | /api/param-graph | `?dataset=` | `{dataset,spec_hash,nodes:[{code,label,defined,mentioned_in}],edges:[{from,to,evidence,origin}],error,warnings,stale,corrections}`. A missing graph returns an empty graph. `stale` is true when its spec hash differs from the current spec |
| POST | /api/param-graph/extract | `{dataset}` | Parameter graph object. Runs one parameter-only extraction; failure returns and stores the graph with `error`, without affecting variable extraction. No parameter rows returns an empty graph without an LLM call |
| POST | /api/param-graph/ask | `{dataset,message}` | `{answer,changes,correction,model,turns}` (async, job kind `param_ask`). The parameter thread and corrections share the dataset DAG agent file |
| POST | /api/param-graph/apply | `{dataset,changes}` | `{ok,graph}`. Supported ops: `add_edge`, `remove_edge`, `add_undefined`, `remove_undefined`; applied edges have `origin:"user"`. Invalid codes and cycles return 400 |
| POST | /api/node/reset | `{dataset, node, delete_code?:bool}` | node status. Sets that variable to `none` with no problem, removes its column from both `_work/<side>.rds`, and deletes its `out.rds` and compare result. Keeps `code.R` and `notes.json` unless `delete_code` is true, in which case both sides' code and notes are deleted and the logs stay. Descendants only get `upstream_changed`. 409 if the variable is already running |
| POST | /api/node/generate | `{dataset, node, side?:"primary|validation|both", instruction?}` | node status (async). Allowed in every state when the variable is not already running. `none` and legacy validate-stage problems stay Not generated while Generate starts; blocking spec issues become `warnings` and do not block. `side` regenerates only that side. A non-empty `instruction` is appended to that side's `notes.json`. The model receives that side's current code and every note in order. 409 only while that variable is already running |
| POST | /api/node/compare | `{dataset, node}` | `{status, compare}` |
| POST | /api/batch | `{dataset, nodes:[ids], compare?:bool}` | Batch state, returned at once. 409 if a batch is already running. Unknown ids are 400. Nodes run one after another (never in parallel): dataset key nodes first, then `inspect_topo` order, then any remaining ids. Each node is generated on both sides. When `compare` is true (the default) and the node is Generated with no problem, compare runs. One node's failure is recorded and the batch continues. A node that is already running is recorded as `skipped: running` |
| POST | /api/batch/cancel | | Batch state. Stops after the current node finishes. 409 if none is running |
| POST | /api/node/investigate | `{dataset, node}` | `{report}` (mismatch QC, sends data rows to LLM) |
| POST | /api/node/ask | `{dataset, node, side:"primary"\|"validation", question}` | Compatibility alias for `/api/node/agent`; `question` maps to `message` and the response has the agent shape |
| POST | /api/node/agent | `{dataset, node, side:"primary"\|"validation", message}` | `{reply, change, model, code_updated, note_index, compare_status, status, compare}` (async). One side's agent answers or stores a note, regenerates only that side, runs it, and compares when both results exist. One `agent` job covers the entire sequence; 400 for an empty message or invalid side, 409 when busy |
| POST | /api/expert/clear | `{dataset, side}` | `{ok:true}`. Deletes that side's expert thread only. 400 on a bad side |
| GET | /api/node/detail | `?dataset=&node=` | `{status, actions, code:{primary,validation}, log:{...}, compare, report, notes:{primary,validation}, code_note:{primary,validation}, experts:{primary,validation}, spec_rows}` |
| GET | /api/node/llm-log | `?dataset=&node=&side=` | last logged prompt and reply for that side, or 404 |
| POST | /api/fullrun/run | `{dataset, side:"primary"|"validation"}` | `{ok:true, running:true, dataset, side}` (202); 409 if that side already has a full run |
| GET | /api/fullrun/status | `?dataset=` | `{dataset, sides:{primary:{run, running, stale, log_tail}, validation:{run, running, stale, log_tail}}}`. `run` is `null` before the first run; otherwise `{ok, started, finished, seconds, variables_run, skipped, failed_variable, error, rows, columns, hashes}` (`rows` and `columns` are counts; `hashes` maps node ids to SHA-256 strings). `stale` compares the saved code hashes with current code |
| GET | /api/fullrun/program | `?dataset=&side=` | Standalone R program text, or 404 before a program exists |
| GET | /api/fullrun/log | `?dataset=&side=` | Full raw program log text, or 404 before a log exists |
| GET | /api/dataset/code | `?dataset=` | `{dataset, order, primary:{nodes,script}, validation:{nodes,script}}`. Nodes that have `code.R`, DAG order (upstream first). Each node is `{id, var, status, code}`. `script` concatenates them with a header `# ==== ADSL.STUDYID (Data matched) ====` |
| GET | /api/node/inputs | `?dataset=&node=&page=&size=&only_diff=&filter=` | Local mismatch table. See below. Nothing from this response is sent to the LLM |
| GET | /api/qc | | `[{dataset, size, updated}]` |
| GET | /api/qc/get | `?dataset=` | `{markdown}` |
| POST | /api/qc/upload | `{dataset, name, base64, replace:bool}` | `{ok}` or 409 `{exists:true}` |
| GET | /api/template | `?name=variable.csv|parameter.csv|qc-methodology.md` | file |
| POST | /api/export | `{dataset, format:"xpt|rds"}` | file (only when all nodes are Data matched) |

`/api/state` includes `batch`. It is null until the first batch. A finished batch stays until the next one starts:
`{dataset, total, done, current, step, cancelled, finished, results:[{node, level, ok, message}]}`.
`step` is `"generate"` or `"compare"` while that step is running, otherwise null. `done` is how many nodes are recorded.
`current` is the node in progress. Job pills (`busy`, `last_error`) still track each generate and compare.

## Frontend (www/) — F2-F11 UI

Single page, English UI labels. Panels:
1. Top bar: project path, busy state, data-outbound notice.
2. Settings tab: upload spec (variable.csv + parameter.csv; warn once "Changes you have not downloaded will be lost" before re-upload),
   template downloads, xpt uploads list with missing-file warnings,
   and Model (provider, base URL, model, API key, test). The key field is never filled from the saved key;
   leaving it blank keeps the saved key.
3. Spec tab: dataset tabs, then a toolbar: `Reset` (enabled when the selected variable is Generated, Data matched, has a problem, or has code; confirm dialog includes "Also delete the generated code and notes"), Re-extract, zoom, Download, and Download DAG. Per dataset an SVG DAG, left-to-right
   layered layout (longest-path layering, barycenter ordering), parameter groups drawn as boxes, colors by
   state (Not generated grey, Generated amber, Data matched green, problem red, ready = pulsing outline), "Same source" badge,
   a small marker when `upstream_changed` is set (tooltip `Upstream X changed after this was built`),
   cycles as red edges. Key variables from the spec Key column sit in a leftmost lane labelled Keys, each with a Key badge.
   One dashed connector runs from that lane into the rest of the dataset. Its tooltip is
   `Every variable is added to the key rows; keys must be matched first`. The connector is derived for display and is not
   stored in graph.json. It does not block generation. The graph sits beside a collapsible "Ask the DAG agent"
   panel (message list, input, Send). A reply that proposes changes lists them as `+ edge` / `- edge` lines with
   Apply changes and a Discard link. Apply highlights those edges for a few seconds. Corrections are listed with
   a remove control. A graph click pre-fills `About <id>: ` when the draft is empty or already has that prefix.
   `?mock=1&autodag=1` asks to add one edge and applies it. Dragging a node moves it; dragging a parameter group moves its members. Saved coordinates override automatic placement, while new nodes retain automatic placement. Reset layout clears saved coordinates. Category buttons open local value, rule, and side-count tables; missing or one-sided values are highlighted. The graph extractor stores optional node `categories` parsed from Derivation, and the DAG agent can edit them with `set_categories`. Pan/zoom. Clicking a node selects it (shared with the
   other tabs) and scrolls its spec row into view. Ctrl/Cmd+click or Shift+click toggles that node in a multi-selection
   (a purple outline). Ctrl/Cmd+click on a parameter group header selects every member. The Spec toolbar has a Multi-select
   toggle: while it is on, a plain click on a node, group header, or spec row adds or removes it (the cursor shows a
   copy sign) and a Quick pick menu (All, Not data matched, Problem, Not generated yet, Clear) appears; Done selecting turns it off
   and keeps the selection. When the selection is not empty the toolbar shows `N selected`,
   a Compare after generating checkbox (on by default), Generate selected, and Clear. The same selection is toggled from
   spec-table rows with Ctrl/Shift+click. Generate selected posts `/api/batch`. While it runs the header shows
   `Batch 3/10 · Generating ADSL.AGE` with Stop. When it finishes the header shows `Batch done: 8 Data matched, 1 Generated, 1 failed`
   and Results lists each node, state, and message; choosing a row opens that variable. The batch object is part of
   `/api/state`, so a reload still shows a running batch. `?mock=1` selects and runs a fake batch the same way.
   Generate is offered for any variable that is not already running. Blocking spec issues are warnings. The graph does not force an order.
4. Code tab: the selected node (level, stuck stage, buttons Generate & run / Compare / Re-run, code view of
   both sides read-only, logs, comparison table). A primary spec-path warning is shown above the primary code. R run logs mark error messages and continuations red, and warnings amber; the header counts each and its badge jumps to the first line. The saved `log_tags.rds` records the R-side classification of each transcript line. Generate & run and Regenerate are enabled when a variable is selected and nothing is running. Re-run also needs saved code. Compare needs both sides to have run. A disabled button says why. An HTTP error shows the server message in the status line. View in Mismatch inspector opens the Mismatches tab.
5. Bottom of the Spec tab: a full-width spec table (variable/parameter tabs) with linking; clicking a variable highlights its node.

## Source variable catalog (added 2026-10-29)

Purpose: when a spec is checked, first extract the variable names (and a little metadata) of every uploaded
input xpt (SDTM domains and external ADaM such as adsl.xpt), so all later checks use one stored catalog.

File: `<project>/.dualdag/state/source_catalog.json`, rebuilt when any file in `<project>/data/` is added,
removed or changed (compare name + size + mtime + md5 stored per file); otherwise reused.
```json
{"built": "<iso>",
 "domains": {"DM": {"file": "dm.xpt", "md5": "...", "size": 1234, "mtime": "<iso>", "nrow": 306,
                    "label": "Demographics",
                    "variables": [{"name": "USUBJID", "label": "Unique Subject Identifier", "type": "character", "length": 11}],
                    "values": {"VSTESTCD": ["SYSBP", "DIABP"]}}}}
```
`values` holds the distinct values (max 500 each) of code columns: any column ending in TESTCD, PARAMCD,
QNAM, CAT, SCAT, plus VISIT/VISITNUM/AVISIT/AVISITN, EPOCH, and DSDECOD/DSTERM if present.

Functions (R/catalog.R): `build_source_catalog(project, force = FALSE)` -> catalog list (reads xpt fully once
to get nrow/values; header-only is not enough for values); `catalog_index(catalog)` -> `list(DOMAIN = colnames)`
(same shape as `data_index()`, which now delegates to it); `catalog_lookup(catalog, domain, var)` ->
`"ok" | "missing_domain" | "missing_var"`; `catalog_prompt_text(catalog, domains = NULL, max_chars = 15000)` ->
compact text (one line per domain: `DM (306 rows): STUDYID USUBJID AGE ...` + code values) for LLM prompts.

Used by: check_graph (missing_data / missing_var issues), parameter checks (a parameter whose defining test code
value never occurs in the data -> issue type `missing_value`, severity error), dependency extraction
prompts (LLM sees the real variable names so it can resolve refs and flag unresolvable ones), code generation
prompts for both sides. A side prompt includes only source variables named by the target lineage; validation also
gets names, labels, and types for the source domains in that lineage. No unrelated domain is sent.

API: `GET /api/catalog` -> catalog (without md5); `/api/state` gains `catalog_summary: {DOMAIN: {nrow, nvars, file}}`.
Spec check flow: upload spec or press Check → build/reuse catalog (step shown in UI as "Reading input columns") → extraction/check.

## UI layout v2 + data viewer (added 2026-10-29)

The page is split into six top-level tabs (one visible at a time; the header bar stays global).
The URL hash keeps `tab=spec|code|data|mismatches|qc|settings` (numeric aliases `1`–`4` as before, and `5` for Mismatches).
An old link `tab=data&diff=1` or `tab=data&view=diff` opens Mismatches.
1. **Spec & checks** — spec table (variable/parameter), dependency DAG, issues report. The toolbar is
   dataset tabs, Select (All / Not data matched / Problem / Not generated yet / Clear), the batch controls when the selection
   is not empty, `Reset`, Re-extract, zoom, Download, and Download DAG. A plain click selects
   one node and scrolls that spec row. Ctrl/Cmd+click or Shift+click adds it to the batch selection. The code and
   data tabs cover the rest. With no spec, or when the graph names an
   xpt that is not uploaded, a short hint links to Settings. A first visit with no spec and no tab in the
   URL opens on Settings.
2. **R code & runs** — the variable chosen on the Spec tab, then primary and validation code side by side
   (read-only), their run logs (live while running), actions Generate & run / Re-run / Compare. Above the panes, a
   Current variable / Whole dataset toggle (hash `view=all`). Whole-dataset mode shows each side's concatenated script;
   a node header jumps back to that variable. A collapsed "How this page works" strip sits at the top of the tab
   (remembered in `localStorage` as `dualdag.codeGuide`). Each pane has its own Download button, and in current-variable
   mode an Explain button next to Download log. Each side flows from code to an independently scrollable, vertically
   resizable run log (about 18 lines), then one "<side> R agent" thread and input with Send and Clear.
   Explain sends "Explain this code and its log." into the same thread. Only user messages and agent replies
   appear there; earlier ask turns are retained, while generate and rerun turns are dropped from memory.
   A clear change request becomes a side note, then regenerates and runs only that side and compares when both
   results exist. Its reply links to the code and reports the note number and compare status. The agent prompt
   includes that side's role, lineage, current code, log tail, notes, and recent conversation without the other
   side's code, log, notes, results, or thread. Generate also receives that side's recent conversation.
   Long code, logs, threads, compare tables, reports, graph side
   threads, settings cards, and data viewers scroll within their own panels so controls remain reachable.
   Compare panel links to View in Mismatch inspector. Generate & run stays enabled while a variable is selected
   and nothing is running. Re-run also needs saved code, and Compare needs both sides to have run. A disabled button says why.
3. **Data** — browse every dataset that exists now and open it in a table viewer:
   input xpt (uploaded), per-node side results (primary / validation `out.rds`: keys + variable), the assembled
   dataset so far (primary results of all Data matched variables merged by key), and exported files in `output/`.
   The mismatch inspector is not on this tab.
4. **Mismatches** — the mismatch inspector at full height. The left list groups variables that differ,
   only variables that differ. With none, it shows `No mismatches.`. Only differing rows, filter, and paging
   stay above the two synced panes. View in Mismatch inspector
   on the code tab opens this tab with that variable selected.
5. **QC (hidden)** — the `SHOW_QC_TAB` switch in `www/app.js` is false by default. Old `tab=qc` links open
   R code & runs. The retained QC view groups compared variables as Differs then Match, with markers for saved QC reports. Selection syncs
   with `tab=qc&node=...`. A selected node shows Investigate when it differs, its markdown QC report with download,
   a link to differing rows in Mismatches, and the investigation outbound notice. The dataset's QC document template,
   upload, list, and view controls live below the report. Investigation remains a separate job and its report never
   enters either side's generation prompt.
6. **Settings** — Spec upload and templates, analysis dataset formats, the input xpt list and its variable catalog,
   and model settings (the old header button). The four cards sit two-by-two on a wide screen and stack on phones.

Data viewer API (Worker A):
- `GET /api/datasets` -> `[{id, kind:"input"|"result"|"assembled"|"output", dataset, node?, side?, file, nrow, ncol, updated}]`
  (`id` is an opaque string usable in the next call).
- `GET /api/datasets/view?id=&offset=0&limit=100&filter=&cols=` -> `{id, nrow, ncol, columns:[{name,label,type}],
  rows:[[...]], offset, limit}`. `filter` is a plain substring matched case-insensitively against all cells
  (server side); `cols` comma list to restrict columns. limit max 1000. Values serialized as strings (dates ISO,
  NA -> null, empty string -> `""`). The Data and Mismatches views render those values differently.
  Data stays local (browser only); no LLM involvement.
- `POST /api/node/run` `{dataset, node, side:"primary"|"validation"|"both"}` re-runs the STORED code (no LLM call,
  code unchanged) and updates logs/results; state rules unchanged (both sides must succeed for Generated; compare must be
  redone). Allowed only when stored code exists for that side.

## Whole program and mismatch inputs (added 2026-10-30)

`R/inspect.R`. Both endpoints stay on the machine. The mismatch table is not an investigate call: no rows,
code, or column values are sent to the model.

`GET /api/dataset/code?dataset=ADSL` reads each node's stored `code.R` and orders nodes with a stable
topological sort (ready nodes keep their graph order; a cycle is appended after the sorted prefix, in
graph order). A node with code on disk but missing from the graph is appended. Per side, `nodes` keeps
only sides that have non-empty code. `order` is the ids that have code on at least one side. An unknown
dataset name is 400; a valid dataset with no code returns empty sides.

`GET /api/node/inputs?dataset=ADSL&node=ADSL.AGE&page=1&size=100&only_diff=0&filter=` builds one table:
key columns, then the source columns each side's program used, then `primary_<VAR>`, `validation_<VAR>`,
and `diff`. Column metadata carries `role` (`key|input|result|diff`), `group`
(`key|primary|both|validation|result`), `sides` (`primary|validation|both`), `origin` (`DM.AGE` or
`upstream.STUDYID`), and `multi` / `note: "Multiple rows"` when a domain has more than one row for any key.
Every row is the same length as `columns`. A missing cell is JSON `null` and is never omitted:
dropping a null would slide the diff flag into `primary_<VAR>`. A side with no `out.rds` (or a file
that cannot be read) sets that result column to `aligned: false` and `note: "No result"`, and every
result cell is null. A stored `out.rds` with 0 rows sets `note: "Empty result"` and `aligned: false`
(the cell stays null, not the diff flag). Rows that exist but have no key or no result column set
`note: "Cannot align"`. A non-empty aligned result keeps `aligned: true`; its `note` is `"Multiple rows"` only when
a key has more than one result row.
Used variables come from `getParseData` on that side's `code.R`: a domain is opened only by
`src$DOMAIN` / `src[["DOMAIN"]]` / `src["DOMAIN"]`, and a later name on that chain binds to that domain.
`dom <- src[["DM"]]` makes a later `dom$VAR` / `dom[["VAR"]]` a use of that domain. A bare token that
matches a column of an opened domain is also included, except a name that the same program writes into
`result` (`result[["AGE"]] <- ...`), which is the output column rather than another input. Key columns
are always the row key and are not listed again as inputs. Graph edges always add upstream columns. If the code is missing
or will not parse, that side falls back to graph edges plus external refs whose `used_by` contains the
node. The same upstream origin on both sides is returned as `primary_upstream.X` and
`validation_upstream.X` (the `origin` stays `upstream.X`) because the two sides can disagree.
Multi-row domains are not exploded: the cell is `"<first> (n=<count>)"`. `only_diff` defaults to false
when the query omits it. `filter` is a case-insensitive substring. `page` is 1-based, `size` defaults
to 100 and caps at 1000. Unknown node (not in the graph and no run directory) is 404. No key on the
dataset is 400.

The Mismatches tab shows the inspector as two panes at full height. The left list replaces the variable
dropdown. Only differing rows (default on), filter, and paging stay shared above both panes. The left pane is primary: key columns,
then the inputs that side used (including columns both sides used), then `primary_<VAR>`. The right
pane is validation: key columns, then the inputs that side used (including the shared ones), then
`validation_<VAR>`. The `diff` column is not drawn; a differing row and its result cells are
highlighted on both sides. Each pane has its own horizontal and vertical scrollbars, and scrolling
one pane moves the other by the same amount. A divider between the panes can be dragged. Rows stay
in the same key order with equal heights. An empty or missing result shows that column's note
(`Empty result`, `No result`, or `Cannot align`) instead of a shifted flag. A side colour marks columns used by
only one side. Hash `tab=mismatches` records the tab. Older `diff=1` and `tab=data&view=diff` links open it too.
`?mock=1` serves the same layout for Data and Mismatches.

## Layered parameter run model

Layered graphs (`layered: true`) use `parameter.csv` order as the stable tie-breaker for a topological parameter order. A parameter cycle is reported as an error for the user to resolve. Each parameter has a Records node followed by its own parameter-variable nodes; dataset-level variables remain separate. Parameter edges carry `uses` mappings. `param_layer_read_vars()` is the common source of readable upstream variables (edge uses plus same-dataset Source references).

Generation of a parameter layer is blocked while an earlier dependency layer has any node that is not Data matched on that side. Records output includes the dataset keys and PARAMCD and is checked for the layer code and duplicate key rows. Records comparison checks the key-row set with NA-aware key signatures and reports sample rows. Non-layered graphs retain the ordinary variable path.

## Per-side self-review

After a generated side runs successfully, `R/self_review.R` makes one additional model call before compare. The call receives only that side's role, target spec row, rules, notes, instruction, code, R log, card facts, joined result/parent work rows (seeded sample, including missing targets when present), and source-column summaries. It has no tools and no compare/QC or other-side data. A valid JSON reply has `verdict` (`expected`, `unexpected`, or `uncertain`), `reasoning`, `issues`, and nullable `spec_question`. A literal but suspect spec is `uncertain`; review warnings never block compare.

An `unexpected` verdict always triggers at most one regeneration. The retry prompt includes `SELF_REVIEW_BEGIN` / `SELF_REVIEW_END` with the prior code and issues. A failed retry restores the first successful run; a successful retry is reviewed once and the first review is retained in history. Reviews are stored per side at `runs/<DS>/<VAR>/<side>/review.json`, with `code_hash`, `time`, `retried`, and optional `history`; API node detail exposes them as `reviews.primary` and `reviews.validation`, with `stale` derived from the current code hash. Review and the single rewrite are always on; legacy config values are ignored.
