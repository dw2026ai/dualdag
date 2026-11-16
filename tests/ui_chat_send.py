"""Private Chromium and Firefox check for clearing chat composers after send."""
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


def free_port():
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


def run_browser(browser_type, work, label, executable):
    home, project = work / f"{label}-home", work / f"{label}-project"
    home.mkdir()
    project.mkdir()
    port = free_port()
    env = dict(os.environ, HOME=str(home), DUALDAG_HOME=str(home / "dualdag"),
               DUALDAG_FAKE_LLM="1", DUALDAG_LLM_API_KEY="", DUALDAG_PORT=str(port))
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
            content = log.read_text(errors="replace")
            link = next((line.split("Open:", 1)[1].strip() for line in content.splitlines()
                         if "Open:" in line and "#token=" in line), None)
            if link:
                break
            if server.poll() is not None:
                raise RuntimeError(f"{label} app exited: {content[-2000:]}")
            time.sleep(.25)
        assert link, f"{label} app did not start: {log.read_text(errors='replace')[-2000:]}"
        page = browser.new_page()
        page.goto(link)
        page.get_by_role("button", name="Super agent").click()
        composer = page.locator("#super-agent-message")

        composer.fill("send with button")
        page.locator('[data-act="super-send"]').click()
        page.get_by_text("send with button", exact=True).wait_for()
        page.wait_for_function("() => document.querySelector('#super-agent-message')?.value === ''")
        assert composer.evaluate("el => document.activeElement === el")
        page.locator(".super-agent-overlay .coder-working").wait_for(state="detached")

        composer.fill("send with enter")
        composer.press("Enter")
        page.get_by_text("send with enter", exact=True).wait_for()
        page.wait_for_function("() => document.querySelector('#super-agent-message')?.value === ''")
        assert composer.evaluate("el => document.activeElement === el")
        page.locator(".super-agent-overlay .coder-working").wait_for(state="detached")
        print(f"{label}: button and Enter send clear the focused composer; messages appear")
    finally:
        browser.close()
        server.terminate()
        server.wait(timeout=15)


def main():
    work = Path(tempfile.mkdtemp(prefix="wei41-chat-send-private-"))
    with sync_playwright() as playwright:
        run_browser(playwright.chromium, work, "chromium", CHROME)
        run_browser(playwright.firefox, work, "firefox", FIREFOX)


if __name__ == "__main__":
    main()
