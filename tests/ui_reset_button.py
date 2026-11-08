"""Private fake project browser check for the Reset toolbar button."""
import base64
import json
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
ADVS,USUBJID,Subject,Char,20,1,VS.USUBJID,Copy subject.
ADVS,PARAMCD,Parameter,Char,8,2,VS.VSTESTCD,Copy parameter.
ADVS,AVAL,Value,Num,8,,VS.VSSTRESN,Copy source value.
"""
PARAMETER = """Dataset,PARAMCD,PARAM,Source,Derivation
ADVS,P1,Parameter one,VS.VSTESTCD,Retain parameter records.
"""


def free_port():
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


def run_browser(browser_type, work, label, executable):
    home, project = work / f"{label}-home", work / f"{label}-project-copy"
    home.mkdir()
    project.mkdir()
    port = free_port()
    env = dict(os.environ, HOME=str(home), DUALDAG_HOME=str(home / "dualdag"),
               DUALDAG_FAKE_LLM="1", DUALDAG_LLM_API_KEY="")
    log = work / f"{label}.log"
    with log.open("w") as output:
        server = subprocess.Popen(
            [RSCRIPT, "run.R", f"--project={project}", f"--port={port}", "--no-browser"],
            cwd=ROOT, env=env, stdout=output, stderr=subprocess.STDOUT,
        )
    browser = browser_type.launch(headless=True, executable_path=executable)
    try:
        link = None
        for _ in range(120):
            text = log.read_text(errors="replace")
            link = next((line.split("Open:", 1)[1].strip() for line in text.splitlines()
                         if "Open:" in line and "#token=" in line), None)
            if link:
                break
            if server.poll() is not None:
                raise RuntimeError(f"{label} server exited: {text[-2000:]}")
            time.sleep(.25)
        assert link, f"{label} server did not start: {log.read_text(errors='replace')[-2000:]}"
        page = browser.new_page()
        page.goto(link + "&tab=settings")
        base, token = link.split("/#token=")
        headers = {"X-Dualdag-Token": token}
        uploaded = page.request.post(base + "/api/spec/upload", headers=headers, data={
            "variable": {"name": "variable.csv", "base64": base64.b64encode(VARIABLE.encode()).decode()},
            "parameter": {"name": "parameter.csv", "base64": base64.b64encode(PARAMETER.encode()).decode()},
        })
        assert uploaded.status == 200, uploaded.text()
        extracted = page.request.post(base + "/api/param-graph/extract", headers=headers,
                                      data={"dataset": "ADVS"})
        assert extracted.status == 200, extracted.text()

        page.reload()
        page.locator('[data-act="tab"][data-tab="spec"]').click()
        page.get_by_role("tab", name="Variable DAG").click()
        assert page.locator('[data-act="zoom-reset"]').count() == 0
        assert page.get_by_role("button", name="Reset", exact=True).count() == 0

        page.get_by_role("tab", name="Parameter DAG").click()
        reset = page.locator('[data-act="start-new-dag"]')
        reset.wait_for()
        assert reset.inner_text() == "Reset"
        assert "danger" in (reset.get_attribute("class") or "")
        assert page.get_by_role("button", name="Reset view", exact=True).count() == 0
        page.get_by_role("button", name="Zoom in").wait_for()
        page.get_by_role("button", name="Zoom out").wait_for()
        reset.click()
        assert page.locator(".mbody h3").inner_text() == "Reset"
        assert "This deletes everything stored for this project" in page.locator(".mbody").inner_text()
        assert "Type CONFIRM" in page.locator(".mbody").inner_text()
        confirm = page.locator('[data-act="start-new-dag-confirm-action"]')
        assert confirm.inner_text() == "Reset"
        assert "danger-filled" in (confirm.get_attribute("class") or "")
        page.locator('[data-act="modal-close"]').click()
        assert page.locator(".modal").is_hidden()
        print(f"{label}: Reset toolbar and cancel modal passed")
    finally:
        browser.close()
        server.terminate()
        server.wait(timeout=15)


def main():
    work = Path(tempfile.mkdtemp(prefix="dualdag-reset-private-"))
    with sync_playwright() as playwright:
        run_browser(playwright.chromium, work, "chromium", CHROME)
        run_browser(playwright.firefox, work, "firefox", FIREFOX)


if __name__ == "__main__":
    main()
