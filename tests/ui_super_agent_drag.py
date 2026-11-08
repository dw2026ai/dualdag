"""Regression check for Super agent drag/draft state across app renders."""
from pathlib import Path
from playwright.sync_api import sync_playwright

ROOT = Path(__file__).resolve().parents[1]
URL = (ROOT / "www/index.html").as_uri() + "?mock=1#tab=settings"
CHROME = Path.home() / ".local/bin/google-chrome"

with sync_playwright() as p:
    browser = p.chromium.launch(headless=True, executable_path=str(CHROME))
    page = browser.new_page(viewport={"width": 1440, "height": 1000})
    page.goto(URL)
    page.wait_for_timeout(700)
    page.get_by_role("button", name="Super agent").click()
    window = page.locator(".super-agent-overlay")
    window.wait_for()
    before = window.bounding_box()
    head = page.locator(".super-agent-head")
    start = head.bounding_box()
    sx, sy = start["x"] + start["width"] * 0.35, start["y"] + start["height"] / 2
    page.mouse.move(sx, sy)
    page.mouse.down()
    target_x = 330 + sx - before["x"]
    target_y = 210 + sy - before["y"]
    page.mouse.move(target_x, target_y, steps=12)
    page.mouse.up()
    dragged = window.bounding_box()
    assert abs(dragged["x"] - 330) < 3 and abs(dragged["y"] - 210) < 3, (before, dragged)

    composer = page.locator("#super-agent-message")
    composer.fill("draft survives rerender")
    page.evaluate("window.__superBeforeRender = document.querySelector('.super-agent-overlay')")
    page.locator('[data-act="tab"][data-tab="run"]').click(force=True)
    page.wait_for_function("window.__superBeforeRender && !window.__superBeforeRender.isConnected")
    page.wait_for_timeout(2800)  # also allow the normal refresh poll to render again
    after = page.locator(".super-agent-overlay").bounding_box()
    assert abs(after["x"] - dragged["x"]) < 2 and abs(after["y"] - dragged["y"]) < 2, (dragged, after)
    assert page.locator("#super-agent-message").input_value() == "draft survives rerender"
    print("Super agent real-mouse drag, polling render, and draft persistence: pass")
    browser.close()
