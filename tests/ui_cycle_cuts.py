"""Headless mock browser check: the DAG shows each cycle cut and its reason, read-only.

Run from the repository root with: python tests/ui_cycle_cuts.py
"""

from pathlib import Path
from shutil import which

from playwright.sync_api import sync_playwright


ROOT = Path(__file__).resolve().parents[1]
CHROME = Path.home() / ".local/bin/google-chrome"
CHROME = str(CHROME if CHROME.exists() else which("google-chrome"))


def main():
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True, executable_path=CHROME)
        page = browser.new_page(viewport={"width": 1300, "height": 900})
        errors = []
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.goto((ROOT / "www/index.html").as_uri() + "?mock=1#tab=spec")
        page.locator('[data-act="graph-mode"][data-mode="parameter"]').click()
        page.locator('[data-act="param-extract"]').click()
        page.locator("#dagwrap svg").wait_for()
        assert page.locator(".cycle-cuts").count() == 0, "Cuts belong to the variable view"
        page.locator('[data-act="graph-mode"][data-mode="variable"]').click()
        panel = page.locator(".cycle-cuts")
        panel.wait_for()
        text = panel.inner_text()
        assert "Edges cut to break cycles" in text, text
        assert "ADVS.SRC → ADVS.ABLFL" in text, text
        assert 'The ABLFL rule does not read SRC: "Set ABLFL to Y on the latest row" <mock>.' in text, text
        assert panel.locator("mock").count() == 0, "The reason must be escaped"
        assert panel.locator("button, input, select, textarea, a, [data-act]").count() == 0, "The list must be read-only"
        assert not errors, errors
        browser.close()
    print("ok")


if __name__ == "__main__":
    main()
