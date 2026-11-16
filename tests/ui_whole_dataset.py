"""Chromium and Firefox regressions for whole-dataset progress, cancel, and stop reporting."""
import os
import socket
import subprocess
import tempfile
import time
from pathlib import Path

from playwright.sync_api import sync_playwright

ROOT = Path(__file__).resolve().parents[1]
RSCRIPT = os.environ["DUALDAG_TEST_RSCRIPT"]
CHROME = os.environ["DUALDAG_TEST_CHROMIUM"]
FIREFOX = os.environ["DUALDAG_TEST_FIREFOX"]

VARIABLE = """Dataset,Variable,Label,Type,Length,Key,Source,Derivation
ADSL,USUBJID,Unique Subject,Char,20,1,DM.USUBJID,copy
ADSL,AGE,Age,Num,8,,DM.AGE,copy
ADSL,NOPE,Not derivable,Char,20,,,not derivable
ADSL,AGE2,Age plus one,Num,8,,,AGE + 1
"""


def free_port():
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


def run_browser(browser_type, work, label, executable):
    home, project = work / f"{label}-home", work / f"{label}-project"
    home.mkdir(); (project / "spec").mkdir(parents=True); (project / "data").mkdir()
    (project / "spec" / "variable.csv").write_text(VARIABLE)
    (project / "data" / "dm.xpt").write_bytes((ROOT / "tests/fixtures/dm.xpt").read_bytes())
    bootstrap = r'''
files <- list.files("R", pattern="[.]R$", full.names=TRUE)
utils <- files[grepl("utils[.]R$", files)]
for (f in c(utils, setdiff(files, utils))) sys.source(f, envir=globalenv())
p <- open_project(commandArgs(TRUE)[1])
g <- list(dataset="ADSL", nodes=list(
  list(id="ADSL.AGE2",var="AGE2",paramcd=NULL,kind="variable"),
  list(id="ADSL.NOPE",var="NOPE",paramcd=NULL,kind="variable"),
  list(id="ADSL.AGE",var="AGE",paramcd=NULL,kind="variable"),
  list(id="ADSL.USUBJID",var="USUBJID",paramcd=NULL,kind="variable")),
  edges=list(list(from="ADSL.AGE",to="ADSL.AGE2",evidence="AGE2 = AGE + 1",origin="user")),
  groups=list(),external=list(),unresolved=list())
save_graph(p,g); note_graph_nodes(p,"ADSL",g)
'''
    subprocess.run([RSCRIPT, "-e", bootstrap, str(project)], cwd=ROOT, check=True,
                   env=dict(os.environ, HOME=str(home), DUALDAG_HOME=str(home / "dualdag"), DUALDAG_FAKE_LLM="1"))
    port = free_port(); log = work / f"{label}.log"
    env = dict(os.environ, HOME=str(home), DUALDAG_HOME=str(home / "dualdag"),
               DUALDAG_FAKE_LLM="1", DUALDAG_LLM_API_KEY="")
    with log.open("w") as output:
        server = subprocess.Popen([RSCRIPT, "run.R", f"--project={project}", f"--port={port}", "--no-browser"],
                                  cwd=ROOT, env=env, stdout=output, stderr=subprocess.STDOUT)
    browser = browser_type.launch(headless=True, executable_path=executable)
    try:
        link = None
        for _ in range(120):
            link = next((line.split("Open:", 1)[1].strip() for line in log.read_text(errors="replace").splitlines()
                         if "Open:" in line and "#token=" in line), None)
            if link: break
            if server.poll() is not None: raise RuntimeError(log.read_text(errors="replace")[-2000:])
            time.sleep(.25)
        assert link, "private app did not start"
        page = browser.new_page()
        page.goto(link + "&tab=spec")
        page.locator('[data-act="tab"][data-tab="spec"]').click()
        page.get_by_role("tab", name="Variable DAG").click()
        run = page.get_by_role("button", name="Run whole dataset", exact=True)
        assert run.is_visible() and run.is_enabled()

        run.click()
        progress = page.locator("header .pill.job").filter(has_text="Whole dataset")
        progress.wait_for(timeout=30000)
        assert "Whole dataset" in progress.inner_text()
        assert page.get_by_role("button", name="Cancel", exact=True).is_visible()
        page.get_by_role("button", name="Cancel", exact=True).click()
        page.get_by_text("Whole dataset run will cancel after the current step").wait_for(timeout=10000)
        page.wait_for_function("() => { const x=document.querySelector('.pill.job'); return !x; }", timeout=240000)

        run = page.get_by_role("button", name="Run whole dataset", exact=True)
        run.click()
        page.get_by_text("Stopped at ADSL.NOPE:", exact=False).wait_for(timeout=300000)
        stopped = page.locator("header").inner_text()
        assert "could not derive NOPE" in stopped, stopped
        assert "AGE2" not in stopped or "AGE2 +" not in stopped
        print(f"{label}: whole-dataset progress, Cancel, and first-error node/reason passed")
    finally:
        browser.close(); server.terminate(); server.wait(timeout=15)


def main():
    work = Path(tempfile.mkdtemp(prefix="wei31-whole-dataset-private-"))
    with sync_playwright() as playwright:
        run_browser(playwright.chromium, work, "chromium", CHROME)
        run_browser(playwright.firefox, work, "firefox", FIREFOX)


if __name__ == "__main__":
    main()
