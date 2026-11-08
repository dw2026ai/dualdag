# dualdag

dualdag is a local web app for statistical programmers. It uses an ADaM spec and SDTM or external ADaM XPT data to build dependency graphs, generate two independent R programs per variable, run them, and compare their results. You need R and an LLM provider or a logged-in supported coding CLI.

Review generated code and results yourself. dualdag has not been validated for regulatory use.

## Download

Download a ZIP from GitHub Releases, or choose **Code > Download ZIP** and unpack it. You can also clone the repository.

## Quick start

Requires R 4.2 or later. No Node.js or Python is required. On first start, dualdag checks for its R dependencies and may need internet access or a company CRAN mirror to install them.

In RStudio, open the unpacked folder and run:

```r
source("run.R")
```

Or use a terminal:

```sh
Rscript run.R
Rscript run.R --no-browser
Rscript run.R --install-deps
Rscript run.R --project=~/studies/study-01 --port=18790
Rscript run.R --restore-admin-snapshot --project=~/studies/study-01
```

`--restore-admin-snapshot` restores the latest App Admin snapshot for that project; start the app normally afterward. The app listens on `127.0.0.1` and prints a tokenized link. Keep the link open for the session and stop the app with Ctrl+C. Options also include `--port=N`, `--project=DIR`, and `--no-browser`; matching environment variables are `DUALDAG_PORT`, `DUALDAG_PROJECT`, and `DUALDAG_NO_BROWSER=1`. Linux without a display does not open a browser. `DUALDAG_URL_FILE` writes the link to a file.

The default project is `~/dualdag-projects/default`. Settings are stored in `~/.dualdag/`; `DUALDAG_HOME` changes that settings folder.

## Try the demos

Run either bundled example from the app folder:

```sh
Rscript run_demo_adsl.R
Rscript run_demo_advs.R
```

The ADSL and ADVS demo specs are included. On the first run, the launcher fetches demo source data from `pharmaversesdtm` and builds the ADVS example data. This step needs internet access and the `pharmaversesdtm` and `haven` R packages (plus `dplyr` for ADVS). The data package is distributed under Apache 2.0; see [demos/README.txt](demos/README.txt).

To fetch or refresh data explicitly, run `Rscript tools/get_demo_data.R adsl`, `Rscript tools/get_demo_data.R advs`, or `Rscript tools/get_demo_data.R all`.

## What you can do

- Upload `variable.csv`, optional `parameter.csv`, and domain-named SDTM or external ADaM `.xpt` files (for example, `dm.xpt`). Download the spec templates from **Spec & checks**. Dataset, Variable, and PARAMCD values are uppercased; key numbers must be consecutive from 1.
- Build the variable dependency DAG. For BDS specs, first build a Parameter DAG and then generate variable DAGs parameter by parameter. Parameters are classified as carry-over or derived; carry-over parameters are generated together as one group.
- Use the **DAG agent** to discuss graph changes and the **Super agent** on supported tabs (Settings, Spec & checks, R code & runs, Data, and Mismatches). **App Admin** can edit the app and open project through a logged-in Claude Code or Codex CLI; it creates snapshots that can be undone in the app or restored from the command line.
- In **R code & runs**, generate and run the Primary and Validation programs in separate R processes, then compare their outputs. Primary is restricted to the variable's declared lineage; Validation can access all source data and uses the spec as guidance. Each side is isolated from the other side's code, logs, notes, and results.
- Inspect mismatching rows, download network JSON from the DAG view, and use **Reset** to clear project work. Reset removes DAGs, programs, intermediate data, sessions, logs, and exports while preserving the spec and raw data.
- Open a coder window's **SU mode** to ask for a side-specific review with access to all spec and data. It can edit that side only.

## Project files

```text
<project>/
  spec/variable.csv
  spec/parameter.csv
  data/<domain>.xpt
  .dualdag/state/                 saved state, DAGs, layouts, and network JSON
  .dualdag/runs/<DATASET>/<VAR>/  generated code, logs, results, and comparisons
  .dualdag/app-admin/snapshots/   App Admin recovery snapshots
  output/                         exported datasets
```

The downloadable package also includes `demos/` (demo specs and launchers) and `tools/get_demo_data.R` (demo data fetch/build script).

## Security and privacy

- The app binds to `127.0.0.1`. Each launch creates a session token required for API requests; it also checks `Host` and `Origin`. HTML responses include a Content-Security-Policy.
- Generated programs run as R code on your computer with your account's permissions, in a separate process. Review code before running it. The run timeout defaults to 600 seconds and can be changed in Settings.
- API keys are sent from R to the provider you select, never from the browser. They are stored in `~/.dualdag/api_key` with mode 600 or read from `DUALDAG_LLM_API_KEY`; they are not written to `config.json`.
- **Your data may be sent to your LLM provider.** Dependency extraction, code generation, and self-review send spec text. Code generation and self-review also send source column types, missing-value counts, and up to 20 sample values per lineage column. Self-review includes generated code and that side's run results. A mismatch investigation sends up to 200 differing rows. With a coding CLI provider, this information passes through that tool.
- dualdag has no telemetry. It connects to your selected LLM provider and, when installing R packages or fetching demo data, to CRAN.
- Set `DUALDAG_FAKE_LLM=1` to use deterministic fake LLM replies without network calls to a model provider.

## Contact

Questions, bug reports and feedback: [wayne.ding26@gmail.com](<mailto:wayne.ding26@gmail.com>). Once this repository is public, you can also use GitHub Issues.

## License

MIT. See [LICENSE](LICENSE). The app's `DESCRIPTION` declares `MIT + file LICENSE`.
