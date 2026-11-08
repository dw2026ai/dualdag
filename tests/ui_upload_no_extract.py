"""Real app check: a spec upload starts no extraction, and generating works, by button and by parameter.

Run from the repository root with: python tests/ui_upload_no_extract.py
Starts the app with the fake model and a scratch home, so it makes no model calls.
"""

import json
import os
import re
import shutil
import subprocess
import tempfile
import time
import urllib.request
from pathlib import Path
from shutil import which

from playwright.sync_api import sync_playwright


ROOT = Path(__file__).resolve().parents[1]
CHROME = Path.home() / ".local/bin/google-chrome"
CHROME = str(CHROME if CHROME.exists() else which("google-chrome"))
PORT = int(os.environ.get("DUALDAG_PORT", "7941"))


def wait_for_link(log):
    for _ in range(120):
        found = re.search(r"http://127\.0\.0\.1:\d+/#token=\S+", log.read_text() if log.exists() else "")
        if found:
            return found.group(0)
        time.sleep(0.5)
    raise RuntimeError("The app did not start: " + log.read_text()[-500:])


def post(link, path, body):
    base, token = link.split("/#token=")
    req = urllib.request.Request(base + path, data=json.dumps(body).encode(), method="POST",
                                 headers={"Content-Type": "application/json", "X-Dualdag-Token": token})
    with urllib.request.urlopen(req, timeout=240) as res:
        return json.loads(res.read().decode())


def start_app(work, name, port):
    home, proj = work / (name + "-home"), work / (name + "-project")
    home.mkdir()
    proj.mkdir()
    (home / "config.json").write_text(json.dumps({"provider": "openai", "model": "test-model"}))
    env = dict(os.environ, DUALDAG_HOME=str(home), DUALDAG_FAKE_LLM="1", DUALDAG_LLM_API_KEY="sk-test")
    rscript = os.environ.get("DUALDAG_TEST_RSCRIPT") or shutil.which("Rscript")
    log = work / (name + ".log")
    server = subprocess.Popen([rscript, "run.R", f"--project={proj}", f"--port={port}", "--no-browser"],
                              cwd=ROOT, env=env, stdout=log.open("w"), stderr=subprocess.STDOUT, text=True)
    return server, proj, wait_for_link(log)


def upload(page, link, variable, parameter=None):
    page.goto(link + "&tab=settings")
    page.wait_for_selector("#f-var", state="attached")
    page.locator("#f-var").set_input_files(str(variable))
    if parameter:
        page.locator("#f-param").set_input_files(str(parameter))
    page.locator("[data-act='spec-up']").first.click()
    page.wait_for_function("() => /domains/.test(document.body.innerText)", timeout=20000)
    time.sleep(2)


def graphs(proj):
    return list((proj / ".dualdag" / "state").glob("graph_*.json"))


def generated_parameters(graph):
    value = graph.get("generated_parameters", [])
    return [value] if isinstance(value, str) else value


def check_flat(browser, work):
    (work / "variable.csv").write_text(
        "Dataset,Variable,Label,Type,Length,Key,Source,Derivation\n"
        "ADSL,USUBJID,Subject,Char,20,1,DM.USUBJID,Copy DM.USUBJID.\n"
        "ADSL,AGE,Age,Num,8,,DM.AGE,Copy DM.AGE.\n")
    server, proj, link = start_app(work, "flat", PORT)
    try:
        page = browser.new_page(viewport={"width": 1300, "height": 900})
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        page.goto(link + "&tab=settings")
        page.wait_for_selector("#cfg-strong-model", state="attached")
        if os.environ.get("UI_SHOT_DIR"):
            page.screenshot(path=str(Path(os.environ["UI_SHOT_DIR"]) / "settings.png"), full_page=True)
        upload(page, link, work / "variable.csv")
        assert not graphs(proj), "A graph appeared without a click"
        page.goto(link + "&tab=spec")
        page.reload()
        page.wait_for_selector("[data-act='extract']", timeout=20000)
        assert "No dependency graph yet" in page.inner_text("body")
        page.locator("[data-act='extract']").first.click()
        for _ in range(120):
            if graphs(proj):
                break
            time.sleep(0.5)
        assert graphs(proj), "Generate didn't write a graph"
        assert not errors, errors
        page.close()
    finally:
        server.terminate()


def check_layers(browser, work):
    (work / "layered-variable.csv").write_text(
        "Dataset,Variable,Label,Type,Length,Key,Source,Derivation\n"
        "ADVS,USUBJID,Subject,Char,20,1,VS.USUBJID,Copy VS.USUBJID.\n"
        "ADVS,PARAMCD,Code,Char,8,2,VS.VSTESTCD,Copy VS.VSTESTCD.\n"
        "ADVS,AVAL,Value,Num,8,,VS.VSSTRESN,Copy VS.VSSTRESN.\n"
        "ADVS,CHG,Change,Num,8,,ADVS.AVAL,AVAL minus baseline.\n")
    (work / "layered-parameter.csv").write_text(
        "Dataset,PARAMCD,PARAM,Source,Derivation\n"
        "ADVS,SYSBP,Systolic,VS.VSTESTCD,Keep SYSBP rows.\n"
        "ADVS,DIABP,Diastolic,VS.VSTESTCD,Keep DIABP rows.\n")
    server, proj, link = start_app(work, "layers", PORT + 1)
    try:
        page = browser.new_page(viewport={"width": 1300, "height": 900})
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        upload(page, link, work / "layered-variable.csv", work / "layered-parameter.csv")
        assert not graphs(proj), "A graph appeared without a click"
        page.goto(link + "&tab=spec")
        page.reload()
        page.wait_for_selector("text=Parameter DAG", timeout=30000)
        page.get_by_text("Parameter DAG", exact=False).first.click()
        page.wait_for_selector("[data-act='param-extract']", timeout=30000)
        assert "No parameter graph yet" in page.inner_text("body")
        post(link, "/api/param-graph/extract", {"dataset": "ADVS"})
        for pc in ("SYSBP", "DIABP"):
            post(link, "/api/graph/parameter-extract", {"dataset": "ADVS", "paramcd": pc})
        page.goto(link + "&tab=spec")
        page.reload()
        page.wait_for_function("() => document.body.innerText.includes('Carry-over') && document.body.innerText.includes('Derived')", timeout=20000)
        assert "SYSBP" in page.inner_text("body") and "DIABP" in page.inner_text("body")
        graph = json.loads((proj / ".dualdag" / "state" / "graph_ADVS.json").read_text())
        pairs = {(e["from"], e["to"]) for e in graph["edges"]}
        assert set(graph["generated_parameters"]) == {"SYSBP", "DIABP"}, graph["generated_parameters"]
        assert ("ADVS.AVAL@SYSBP", "ADVS.CHG@SYSBP") in pairs and ("ADVS.AVAL@DIABP", "ADVS.CHG@DIABP") in pairs
        assert (proj / ".dualdag" / "state" / "extract_plan_ADVS.json").exists()
        stored = json.loads((proj / ".dualdag" / "state" / "extract_ADVS.json").read_text())
        layers = {p for e in stored["edges"] for p in e.get("params", [])}
        assert layers == {"SYSBP", "DIABP"}, layers
        assert not errors, errors
        page.close()
    finally:
        server.terminate()


def check_carry_group(browser, work, suffix="chromium"):
    (work / "group-variable.csv").write_text(
        "Dataset,Variable,Label,Type,Length,Key,Source,Derivation\n"
        "ADVS,USUBJID,Subject,Char,20,1,VS.USUBJID,Copy VS.USUBJID.\n"
        "ADVS,PARAMCD,Code,Char,8,2,VS.VSTESTCD,Copy VS.VSTESTCD.\n"
        "ADVS,AVAL,Value,Num,8,,VS.VSSTRESN,Copy VS.VSSTRESN.\n"
        "ADVS,CHG,Change,Num,8,,ADVS.AVAL,Derived from AVAL for each parameter.\n")
    (work / "group-parameter.csv").write_text(
        "Dataset,PARAMCD,PARAM,Source,Derivation\n"
        "ADVS,CO1,Carry One,CARRY_OVER,Carry-over source rows.\n"
        "ADVS,CO2,Carry Two,CARRY_OVER,Carry-over source rows.\n"
        "ADVS,DER,Derived One,ADVS.AVAL@CO1,Derived from CO1 using its AVAL.\n"
        "ADVS,DER2,Derived Two,ADVS.AVAL@CO2,Derived from CO2 using its AVAL.\n")
    server, proj, link = start_app(work, "carry-group-" + suffix, PORT + (2 if suffix == "chromium" else 3))
    try:
        page = browser.new_page(viewport={"width": 1300, "height": 900})
        errors = []
        page.on("pageerror", lambda e: errors.append(str(e)))
        upload(page, link, work / "group-variable.csv", work / "group-parameter.csv")
        post(link, "/api/param-graph/extract", {"dataset": "ADVS"})
        param_path = proj / ".dualdag" / "state" / "param_graph_ADVS.json"
        variable_path = proj / ".dualdag" / "state" / "graph_ADVS.json"
        param_graph = json.loads(param_path.read_text())
        assert any(e["from"] == "CO1" and e["to"] == "DER" for e in param_graph.get("edges", [])), param_graph.get("edges")
        post(link, "/api/graph/parameter-extract", {"dataset": "ADVS", "paramcd": "CARRY_OVER"})
        post(link, "/api/graph/parameter-extract", {"dataset": "ADVS", "paramcd": "DER"})
        post(link, "/api/graph/parameter-extract", {"dataset": "ADVS", "paramcd": "DER2"})
        graph = json.loads(variable_path.read_text())
        assert [x["paramcd"] for x in graph.get("layers", [])].count("CARRY_OVER") == 1, graph.get("layers")
        ids = {n["id"] for n in graph.get("nodes", [])}
        assert "ADVS.AVAL@CARRY_OVER" in ids and not any(re.search(r"@(CO1|CO2)$", n) for n in ids), ids
        assert set(generated_parameters(graph)) == {"CARRY_OVER", "DER", "DER2"}, graph.get("generated_parameters")
        assert any(e.get("from") == "CO1" and e.get("to") == "DER" and e.get("uses") for e in graph.get("param_uses", [])), graph.get("param_uses")

        page.goto(link + "&tab=spec")
        page.reload()
        page.wait_for_selector(".param-group-card", timeout=20000)
        panel = page.locator(".param-group-card")
        assert "CO1" in panel.inner_text() and "CO2" in panel.inner_text()
        panel.click()
        assert panel.get_attribute("aria-pressed") == "true"
        options = page.locator("#graph-layer-select option").all_text_contents()
        assert "Carry-over (CO1, CO2)" in options and "CO1" not in options and "CO2" not in options, options
        page.locator("#graph-layer-select").select_option("CARRY_OVER")
        visible_ids = set(page.locator("#dag g.node").evaluate_all("els => els.map(e => e.getAttribute('data-id'))"))
        assert "ADVS.AVAL@CARRY_OVER" in visible_ids
        assert not any(re.search(r"@(CO1|CO2)$", n or "") for n in visible_ids), visible_ids
        page.locator("[data-act='graph-mode'][data-mode='parameter']").click()
        page.wait_for_selector("#dag g.node[data-id='CARRY_OVER']", timeout=10000)
        assert page.locator("#dag g.node[data-id='CARRY_OVER']").get_attribute("class").find("param-carry-over") >= 0
        assert page.locator(".legend .param-carry-over").count() == 1
        panel.click()
        assert page.locator("[data-act='graph-mode'][data-mode='parameter']").get_attribute("aria-selected") == "true"

        logs = [json.loads(f.read_text()) for f in (proj / ".dualdag" / "llm_log").glob("*.json")]
        group_calls = [r for r in logs if "CARRY_OVER_PARAMETERS: CO1, CO2" in r.get("user", "")]
        assert len(group_calls) == 1, len(group_calls)
        root = proj / ".dualdag" / "state" / "network_ADVS"
        assert (root / "CARRY_OVER.json").exists(), list(root.glob("*.json"))
        carry = json.loads((root / "CARRY_OVER.json").read_text())
        assert carry["parameter"] == "CARRY_OVER" and carry["members"] == ["CO1", "CO2"], carry
        assert (root / "_parameters.json").exists()

        # Seed generated outputs, then exercise the visible Regenerate button.
        run_root = proj / ".dualdag" / "runs" / "ADVS"
        seeded = [run_root / "AVAL@CARRY_OVER" / side / "out.rds" for side in ("primary", "validation")]
        seeded += [run_root / "AVAL@DER" / side / "out.rds" for side in ("primary", "validation")]
        for output in seeded:
            output.parent.mkdir(parents=True, exist_ok=True)
            output.write_bytes(b"seeded-output")
        page.locator("[data-act='graph-mode'][data-mode='variable']").click()
        panel.click()
        regenerate = page.locator("[data-act='layer-generate']")
        assert regenerate.is_visible(), page.locator("body").inner_text()
        assert regenerate.inner_text() == "Regenerate Carry-over Group (CO1, CO2)", regenerate.inner_text()
        dialogs = []
        page.once("dialog", lambda dialog: (dialogs.append(dialog.message), dialog.dismiss()))
        regenerate.click()
        assert dialogs == ["Regenerate the carry-over group (CO1, CO2) and clear its results and descendants?"], dialogs
        graph = json.loads(variable_path.read_text())
        assert set(generated_parameters(graph)) == {"CARRY_OVER", "DER", "DER2"}, graph.get("generated_parameters")
        assert all(output.exists() for output in seeded), [str(x) for x in seeded if not x.exists()]

        dialogs.clear()
        page.once("dialog", lambda dialog: (dialogs.append(dialog.message), dialog.accept()))
        regenerate.click()
        assert dialogs == ["Regenerate the carry-over group (CO1, CO2) and clear its results and descendants?"], dialogs
        for _ in range(60):
            current = json.loads(variable_path.read_text())
            if generated_parameters(current) == ["CARRY_OVER"] and not any(output.exists() for output in seeded):
                break
            time.sleep(0.25)
        graph = json.loads(variable_path.read_text())
        assert generated_parameters(graph) == ["CARRY_OVER"], graph.get("generated_parameters")
        assert not any(output.exists() for output in seeded), [str(x) for x in seeded if x.exists()]

        # Generate descendants again to prove the group remains buildable after regeneration.
        post(link, "/api/graph/parameter-extract", {"dataset": "ADVS", "paramcd": "DER"})
        post(link, "/api/graph/parameter-extract", {"dataset": "ADVS", "paramcd": "DER2"})
        graph = json.loads(variable_path.read_text())
        assert set(generated_parameters(graph)) == {"CARRY_OVER", "DER", "DER2"}, graph.get("generated_parameters")

        # Remove a parameter edge; export must not re-add it from extraction uses.
        post(link, "/api/param-graph/apply", {"dataset": "ADVS", "changes": [{"op": "remove_edge", "from": "CO1", "to": "DER"}]})
        params = json.loads((root / "_parameters.json").read_text())
        assert not any(e["from"] == "CO1" and e["to"] == "DER" for e in params.get("edges", [])), params

        # Recreate a saved graph with the former per-member layer shape.
        graph["layers"] = [dict(layer, paramcd=member) for member in ("CO1", "CO2") for layer in graph["layers"] if layer["paramcd"] == "CARRY_OVER"]
        graph["nodes"] = [dict(n, id=n["id"].replace("@CARRY_OVER", "@CO1"), paramcd="CO1") if n.get("paramcd") == "CARRY_OVER" else n for n in graph["nodes"]]
        variable_path.write_text(json.dumps(graph))
        page.goto(link + "&tab=spec")
        page.reload()
        page.wait_for_function("() => document.body.innerText.includes('This saved DAG uses the old per-member carry-over format')", timeout=10000)
        legacy_options = page.locator("#graph-layer-select option")
        legacy_actual = [(legacy_options.nth(i).get_attribute("value"), legacy_options.nth(i).inner_text()) for i in range(legacy_options.count())]
        assert ("CARRY_OVER", "Carry-over (CO1, CO2)") in legacy_actual, legacy_actual
        assert not any(value in ("CO1", "CO2") for value, _ in legacy_actual), legacy_actual
        assert page.locator("#graph-layer-select").input_value() == "CARRY_OVER"
        legacy_generate = page.locator("[data-act='layer-generate']")
        assert legacy_generate.inner_text() == "Regenerate Carry-over Group (CO1, CO2)", legacy_generate.inner_text()
        dialogs.clear()
        page.once("dialog", lambda dialog: (dialogs.append(dialog.message), dialog.dismiss()))
        legacy_generate.click()
        assert dialogs == ["Regenerate the carry-over group (CO1, CO2) and clear its results and descendants?"], dialogs
        assert not errors, errors
        page.close()
    finally:
        server.terminate()
        server.wait(timeout=10)

def main():
    work = Path(tempfile.mkdtemp(prefix="dualdag-ui-"))
    try:
        with sync_playwright() as p:
            browser = p.chromium.launch(headless=True, executable_path=CHROME)
            check_flat(browser, work)
            check_layers(browser, work)
            check_carry_group(browser, work, "chromium")
            browser.close()
            browser = p.firefox.launch(headless=True)
            check_carry_group(browser, work, "firefox")
            browser.close()
        print("ok")
    finally:
        shutil.rmtree(work, ignore_errors=True)


if __name__ == "__main__":
    main()
