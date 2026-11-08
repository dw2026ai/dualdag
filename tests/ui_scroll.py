"""Headless mock browser check for scroll and caret preservation.

Run from the repository root with: python tests/ui_scroll.py
"""

from pathlib import Path
from shutil import which

from playwright.sync_api import sync_playwright


ROOT = Path(__file__).resolve().parents[1]
CHROME = Path.home() / ".local/bin/google-chrome"
CHROME = str(CHROME if CHROME.exists() else which("google-chrome"))
URL = (ROOT / "www/index.html").as_uri() + "?mock=1&scrolltest=1#tab=code&node=ADVS.AVALC"


def position(page):
    return page.evaluate("""() => ({page: window.scrollY,
        main: document.querySelector('.codemain').scrollTop})""")


def close_enough(before, after):
    assert abs(before["page"] - after["page"]) <= 3, (before, after)
    assert abs(before["main"] - after["main"]) <= 3, (before, after)


def test_dag_context_menu(browser_type):
    browser = browser_type.launch(headless=True)
    page = browser.new_page(viewport={"width": 1200, "height": 1000})
    errors = []
    page.on("pageerror", lambda error: errors.append(str(error)))
    page.goto((ROOT / "www/index.html").as_uri() + "?mock=1#tab=spec")
    # The mock spec has parameter layers, so build its mock parameter graph
    # before the variable DAG becomes available.
    page.locator('[data-act="graph-mode"][data-mode="parameter"]').click()
    page.locator('[data-act="param-extract"]').click()
    page.locator("#dagwrap svg").wait_for()
    page.locator('[data-act="graph-mode"][data-mode="variable"]').click()
    page.locator("#dagwrap .node").first.wait_for()
    nodes = page.locator("#dagwrap .node")
    nodes.nth(0).click(button="left")
    nodes.nth(0).click(button="right")
    menu = page.locator(".dag-context")
    menu.wait_for()
    assert menu.get_by_role("menuitem").count() == 1
    assert menu.get_by_role("menuitem", name="Super agent").count() == 0
    page.wait_for_timeout(1500)
    assert menu.is_visible(), f"{browser_type.name}: right-click menu closed without graph movement"
    menu.get_by_role("menuitem", name="Review this node").click()
    page.get_by_text("Reviewing ", exact=False).wait_for()

    # Open the menu again while a review is active, then verify real page
    # movement still dismisses it.
    page.locator("#dagwrap .node").nth(0).click(button="right")
    page.locator(".dag-context").wait_for()
    page.evaluate("""() => {
        const spacer = document.createElement('div');
        spacer.style.height = '1800px';
        document.body.appendChild(spacer);
        window.scrollTo(0, 300);
    }""")
    page.wait_for_function("!document.querySelector('.dag-context')")
    assert errors == [], (browser_type.name, errors)
    browser.close()
    print(f"{browser_type.name} DAG context menu mouse regression: pass")


def main():
    with sync_playwright() as playwright:
        browser = playwright.chromium.launch(
            headless=True, executable_path=CHROME,
            args=["--no-sandbox", "--allow-file-access-from-files"],
        )
        page = browser.new_page(viewport={"width": 900, "height": 480})
        errors = []
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.on("console", lambda message: errors.append(message.text) if message.type == "error" else None)
        page.goto(URL)
        page.locator('[data-act="coder-window"][data-side="primary"]').click()
        page.locator("#coder-window-message").wait_for()

        # Include the document scroll as well as the fixed-height code pane.
        page.evaluate("""() => {
            const spacer = document.createElement('div');
            spacer.style.height = '1200px';
            document.body.appendChild(spacer);
        }""")
        draft = page.locator("#coder-window-message")
        draft.fill("Why did this happen?")
        page.evaluate("""() => {
            const input = document.querySelector('#coder-window-message');
            input.focus(); input.setSelectionRange(4, 4);
            document.querySelector('.codemain').scrollTop = 800;
            window.scrollTo(0, 100);
        }""")
        before = position(page)
        page.evaluate("() => window.__dualdagMockPoll()")
        close_enough(before, position(page))
        assert page.evaluate("document.activeElement.id") == "coder-window-message"
        assert draft.input_value() == "Why did this happen?"
        assert page.evaluate("document.activeElement.selectionStart") == 4

        # The send path renders immediately, then refreshes with the reply.
        before = position(page)
        page.evaluate("document.querySelector('[data-act=coder-send]').click()")
        page.locator(".coder-conversation .coder-turn").last.wait_for()
        close_enough(before, position(page))
        assert draft.input_value() == ""

        # Build a long conversation, then check that the newest reply is in view.
        for index in range(8):
            draft.fill(f"Question {index}")
            before = position(page)
            page.evaluate("document.querySelector('[data-act=coder-send]').click()")
            page.wait_for_function("n => document.querySelectorAll('.coder-conversation .coder-turn').length >= n", arg=index + 2)
            close_enough(before, position(page))
        thread = page.locator(".coder-conversation").evaluate("e => [e.scrollTop, e.scrollHeight - e.clientHeight]")
        assert thread[1] > 0 and abs(thread[0] - thread[1]) <= 3, thread

        page.evaluate("document.querySelector('[data-act=coder-window-close]').click()")
        assert errors == [], errors
        browser.close()
        test_dag_context_menu(playwright.chromium)
        test_dag_context_menu(playwright.firefox)
    print("mock Coder window scroll, focus, caret, send, poll, and console: pass")


if __name__ == "__main__":
    main()
