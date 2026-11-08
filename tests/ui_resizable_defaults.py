"""Measure the unmodified pane defaults against a running private server.

Usage: python tests/ui_resizable_defaults.py http://127.0.0.1:18861/#token=...
"""

import sys

from playwright.sync_api import sync_playwright


def pane_size(locator, axis):
    box = locator.bounding_box()
    return box["width"] if axis == "x" else box["height"]


def close_enough(actual, expected, tolerance=5):
    assert abs(actual - expected) <= tolerance, (actual, expected)


def main():
    if len(sys.argv) != 2:
        raise SystemExit("Pass the URL printed by run.R")
    with sync_playwright() as playwright:
        browser = playwright.chromium.launch(headless=True)
        page = browser.new_page(viewport={"width": 1500, "height": 950})
        page.goto(sys.argv[1])
        page.locator(".tab-spec .dagrow").wait_for()
        assert page.evaluate("Object.keys(localStorage).filter(k => k.indexOf('dualdag.panes.') === 0).length") == 0

        queue = page.locator(".run-queue")
        agent = page.locator("#dag-agent")
        close_enough(pane_size(queue, "x"), 200, 5)
        close_enough(pane_size(agent, "x"), 300, 5)
        for selector, expected in [
            (".tab-spec .dag-canvas-row > .pane-resize-bar", 200),
            (".tab-spec .dagrow > .pane-resize-bar", 300),
        ]:
            bar = page.locator(selector)
            bar.dblclick()
            page.wait_for_timeout(80)
            pane = queue if expected == 200 else agent
            close_enough(pane_size(pane, "x"), expected, 5)

        page.locator(".tab-spec table tbody tr.click").first.click(force=True)
        page.locator('.apptabs [data-tab="code"]').click(force=True)
        splits = page.locator(".code-body-split")
        assert splits.count() == 2
        for i in range(splits.count()):
            code = pane_size(splits.nth(i).locator(".code-area"), "y")
            log = pane_size(splits.nth(i).locator(".log-area"), "y")
            ratio = code / (code + log)
            assert 0.58 <= ratio <= 0.62, (i, code, log, ratio)
            splits.nth(i).locator(":scope > .pane-resize-bar").dblclick()
            page.wait_for_timeout(80)
            code = pane_size(page.locator(".code-body-split").nth(i).locator(".code-area"), "y")
            log = pane_size(page.locator(".code-body-split").nth(i).locator(".log-area"), "y")
            assert 0.58 <= code / (code + log) <= 0.62, (i, code, log)

        page.locator('.apptabs [data-tab="run"]').click(force=True)
        log = page.locator(".tab-run .run-log-area").first
        assert not log.inner_text().strip(), log.inner_text()
        assert pane_size(log, "y") <= 90, pane_size(log, "y")
        program = page.locator(".tab-run .run-program-area").first
        controls = page.locator(".tab-run .run-controls").first
        assert pane_size(program, "y") > pane_size(log, "y"), (pane_size(program, "y"), pane_size(log, "y"))
        button = controls.locator("button").first.bounding_box()
        pane_bottom = page.locator(".tab-run .pane").first.bounding_box()["y"] + page.locator(".tab-run .pane").first.bounding_box()["height"]
        assert button["y"] + button["height"] <= pane_bottom, (button, pane_bottom)
        assert page.evaluate("document.documentElement.scrollHeight <= innerHeight")
        browser.close()
        print("Resizable pane defaults at 1500x950: passed")


if __name__ == "__main__":
    main()
