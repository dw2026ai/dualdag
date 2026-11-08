"""Private fake-model browser coverage for carry-over generation and Full Run."""
import base64
import json
import os
import shutil
import subprocess
import tempfile
import time
import urllib.request
from pathlib import Path
from shutil import which

from playwright.sync_api import sync_playwright

ROOT = Path(__file__).resolve().parents[1]
RSCRIPT = os.environ.get("DUALDAG_TEST_RSCRIPT") or shutil.which("Rscript")
CHROME = Path.home() / ".local/bin/google-chrome"
CHROME = str(CHROME if CHROME.exists() else which("google-chrome"))
BASE_PORT = int(os.environ["DUALDAG_PORT"])
SHOTS = Path(os.environ.get("UI_SHOT_DIR", "/tmp/wei21-browser-shots"))
SHOTS.mkdir(parents=True, exist_ok=True)

VARIABLE = """Dataset,Variable,Label,Type,Length,Key,Source,Derivation
ADVS,USUBJID,Subject,Char,20,1,VS.USUBJID,Copy subject.
ADVS,PARAMCD,Parameter,Char,8,2,VS.VSTESTCD,Copy parameter.
ADVS,AVISITN,Visit,Num,8,3,VS.AVISITN,Copy visit.
ADVS,AVAL,Value,Num,8,,VS.VSSTRESN,Copy source value.
ADVS,CHG,Change,Num,8,,ADVS.AVAL,AVAL minus zero.
"""
PARAMETER = """Dataset,PARAMCD,PARAM,Source,Derivation
ADVS,WEIGHT,Weight,CARRY_OVER,Carry-over source rows.
ADVS,HEIGHT,Height,CARRY_OVER,Carry-over source rows.
ADVS,BMI,Body mass index,"ADVS.AVAL@WEIGHT,ADVS.AVAL@HEIGHT",Weight divided by squared height in meters.
ADVS,OTHER,Other derived,ADVS.AVAL@WEIGHT,Copy WEIGHT values.
"""


def wait_link(log):
    for _ in range(120):
        if log.exists():
            import re
            match = re.search(r"http://127\.0\.0\.1:\d+/#token=\S+", log.read_text())
            if match:
                return match.group(0)
        time.sleep(.25)
    raise RuntimeError(log.read_text()[-1000:] if log.exists() else "server did not start")


def request(link, path, body=None, method=None):
    base, token = link.split("/#token=")
    data = None if body is None else json.dumps(body).encode()
    req = urllib.request.Request(base + path, data=data, method=method or ("POST" if body is not None else "GET"),
        headers={"Content-Type": "application/json", "X-Dualdag-Token": token})
    try:
        with urllib.request.urlopen(req, timeout=240) as response:
            raw = response.read()
            content_type = response.headers.get("Content-Type", "")
            return response.status, (json.loads(raw) if raw and "json" in content_type else raw.decode(errors="replace")) if raw else None
    except urllib.error.HTTPError as error:
        raw = error.read()
        try:
            payload = json.loads(raw)
        except Exception:
            payload = raw.decode(errors="replace")
        return error.code, payload


def check(browser, work, label, port):
    home = work / f"{label}-home"
    project = work / f"{label}-project"
    home.mkdir(); project.mkdir(); (project / "data").mkdir()
    (home / "config.json").write_text(json.dumps({"provider": "openai", "model": "test-model"}))
    env = dict(os.environ, DUALDAG_HOME=str(home), DUALDAG_FAKE_LLM="1", DUALDAG_LLM_API_KEY="sk-test")
    log = work / f"{label}.log"
    server = subprocess.Popen([RSCRIPT, "run.R", f"--project={project}", f"--port={port}", "--no-browser"],
        cwd=ROOT, env=env, stdout=log.open("w"), stderr=subprocess.STDOUT, text=True)
    page = None
    try:
        link = wait_link(log)
        xpt = project / "data" / "vs.xpt"
        xpt_code = "haven::write_xpt(data.frame(USUBJID=c('S1','S1','S2','S2'), VSTESTCD=c('WEIGHT','HEIGHT','WEIGHT','HEIGHT'), AVISITN=1, VSSTRESN=c(70,170,80,180)), commandArgs(TRUE)[1])"
        subprocess.run([RSCRIPT, "-e", xpt_code, str(xpt)], check=True, env=env,
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        page = browser.new_page(viewport={"width": 1360, "height": 900})
        errors = []
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.goto(link + "&tab=settings")
        page.wait_for_selector("#f-var", state="attached")
        (work / f"{label}-variable.csv").write_text(VARIABLE)
        (work / f"{label}-parameter.csv").write_text(PARAMETER)
        page.locator("#f-var").set_input_files(str(work / f"{label}-variable.csv"))
        page.locator("#f-param").set_input_files(str(work / f"{label}-parameter.csv"))
        page.locator("[data-act='spec-up']").first.click()
        page.wait_for_function("() => document.body.innerText.includes('ADVS')", timeout=20000)
        page.screenshot(path=str(SHOTS / f"{label}-settings.png"), full_page=True)

        code, result = request(link, "/api/param-graph/extract", {"dataset": "ADVS"})
        assert code == 200 and not result.get("error"), (code, result)

        # Stop at the Parameter DAG boundary: this assertion must run before
        # any request to /api/graph/parameter-extract can repair the graph.
        def assert_initial_group(stage):
            graph_path = project / ".dualdag" / "state" / "graph_ADVS.json"
            initial = json.loads(graph_path.read_text())
            assert initial.get("layered") is True, (stage, initial)
            layers = initial.get("layers", [])
            carry_layers = [layer for layer in layers if layer.get("paramcd") == "CARRY_OVER"]
            assert len(carry_layers) == 1, (stage, [x.get("paramcd") for x in layers])
            carry = carry_layers[0]
            assert carry.get("members_parameters") == ["WEIGHT", "HEIGHT"], (stage, carry)
            assert "ADVS.AVAL@CARRY_OVER" in carry.get("members", []), (stage, carry)
            assert not any(n.get("id", "").endswith(("@WEIGHT", "@HEIGHT")) for n in initial.get("nodes", [])), stage
            page.goto(link + "&tab=spec"); page.reload()
            page.wait_for_selector("#graph-layer-select", timeout=20000)
            options = page.locator("#graph-layer-select option")
            actual = [(options.nth(i).get_attribute("value"), options.nth(i).inner_text()) for i in range(options.count())]
            assert actual.count(("CARRY_OVER", "Carry-over (WEIGHT, HEIGHT)")) == 1, (stage, actual)
            assert not any(value in ("WEIGHT", "HEIGHT") for value, _ in actual), (stage, actual)
            assert page.locator("#graph-layer-select").input_value() == "CARRY_OVER", stage
            assert "Generate Carry-over Group (WEIGHT, HEIGHT)" in page.locator("[data-act='layer-generate']").inner_text(), stage
            card = page.locator("[data-act='layer-select'][data-layer='CARRY_OVER']")
            card.click()
            assert card.get_attribute("aria-pressed") == "true", stage
            assert "Generate Carry-over Group (WEIGHT, HEIGHT)" in page.locator("[data-act='layer-generate']").inner_text(), stage
            page.screenshot(path=str(SHOTS / f"{label}-{stage}-initial-group.png"), full_page=True)

        assert_initial_group("after-param-extract")
        for pc in ("CARRY_OVER", "BMI", "OTHER"):
            code, result = request(link, "/api/graph/parameter-extract", {"dataset": "ADVS", "paramcd": pc})
            assert code == 200 and not result.get("error"), (pc, code, result)

        # Regenerate Parameter DAG after Variable DAG group extraction and
        # verify the same pre-variable-extraction invariant again.
        code, result = request(link, "/api/param-graph/extract", {"dataset": "ADVS"})
        assert code == 200 and not result.get("error"), ("parameter regeneration", code, result)
        assert_initial_group("after-param-regeneration")
        for pc in ("CARRY_OVER", "BMI", "OTHER"):
            code, result = request(link, "/api/graph/parameter-extract", {"dataset": "ADVS", "paramcd": pc})
            assert code == 200 and not result.get("error"), ("post-regeneration", pc, code, result)

        graph_path = project / ".dualdag" / "state" / "graph_ADVS.json"
        graph = json.loads(graph_path.read_text())
        assert graph.get("layered") is True
        assert [layer["paramcd"] for layer in graph["layers"]].count("CARRY_OVER") == 1
        page.goto(link + "&tab=spec"); page.reload()
        page.wait_for_selector(".param-group-card", timeout=20000)
        group_card = page.locator(".param-group-card")
        assert "WEIGHT" in group_card.inner_text() and "HEIGHT" in group_card.inner_text()
        group_card.click()
        assert group_card.get_attribute("aria-pressed") == "true"
        page.screenshot(path=str(SHOTS / f"{label}-group-lineage.png"), full_page=True)

        # Real Generate endpoint, independent primary and validation jobs, then both-side Run/Compare.
        def generate(node):
            status, payload = request(link, "/api/node/generate", {"dataset": "ADVS", "node": node, "side": "both"})
            assert status == 200, (node, status, payload)
            return payload
        def run(node):
            status, payload = request(link, "/api/node/run", {"dataset": "ADVS", "node": node, "side": "both"})
            assert status == 200, (node, status, payload)
            return payload
        def compare(node):
            status, payload = request(link, "/api/node/compare", {"dataset": "ADVS", "node": node})
            assert status == 200, (node, status, payload)
            return payload

        records = "ADVS.RECORDS@CARRY_OVER"
        generate(records); run(records); compare(records)
        group_av = "ADVS.AVAL@CARRY_OVER"
        generate(group_av)
        group_paths = [project / ".dualdag" / "runs" / "ADVS" / "AVAL@CARRY_OVER" / side / "code.R" for side in ("primary", "validation")]
        for side, path in zip(("primary", "validation"), group_paths):
            stored = path.read_text()
            assert "ADVS_CARRY_OVER_TEMP" in stored, (side, stored)
            assert "PARAMCD" in stored
        llm_logs = [json.loads(p.read_text()) for p in (project / ".dualdag" / "llm_log").glob("*.json")]
        av_calls = [row for row in llm_logs if "Current carry-over group" in row.get("user", "") and "Current variable: AVAL" in row.get("user", "")]
        assert len(av_calls) == 2, len(av_calls)
        assert {row.get("side") for row in av_calls} == {"primary", "validation"}
        for row in av_calls:
            assert "ADVS_CARRY_OVER_TEMP" in row["user"]
            assert "PARAMETER_ROWS_BEGIN" in row["user"]
            assert "CHECK_OUTPUT" in row["user"] or "check" in row.get("system", "").lower()
        run(group_av); compare(group_av)
        group_status = request(link, f"/api/node/detail?dataset=ADVS&node={group_av}")[1]["status"]
        assert group_status["level"] == "L3" and all(group_status["sides"][side]["ok"] for side in ("primary", "validation")), group_status
        # Force one genuine spec fallback in the later BMI prompt; the target anchor remains intact.
        (project / ".dualdag" / "runs" / "ADVS" / "AVAL@CARRY_OVER" / "primary" / "card.json").unlink()

        # AVAL is matched while unrelated CHG is still unfinished: BMI must generate.
        detail_id = "ADVS.CHG@CARRY_OVER"
        detail = request(link, f"/api/node/detail?dataset=ADVS&node={detail_id}")[1]
        assert detail["status"]["level"] != "L3"
        bmi_records = "ADVS.RECORDS@BMI"
        status, payload = request(link, "/api/node/generate", {"dataset": "ADVS", "node": bmi_records, "side": "both"})
        assert status == 200, (status, payload)
        run(bmi_records); compare(bmi_records)
        generate("ADVS.AVAL@BMI"); run("ADVS.AVAL@BMI"); compare("ADVS.AVAL@BMI")
        generate("ADVS.RECORDS@OTHER"); run("ADVS.RECORDS@OTHER"); compare("ADVS.RECORDS@OTHER")
        generate("ADVS.AVAL@OTHER"); run("ADVS.AVAL@OTHER"); compare("ADVS.AVAL@OTHER")

        # The app's run queue (batch route) also generates, runs, and compares a group variable.
        status, queued = request(link, "/api/batch", {"dataset": "ADVS", "nodes": ["ADVS.CHG@CARRY_OVER"], "compare": True})
        assert status == 200, (status, queued)
        for _ in range(180):
            state = request(link, "/api/state")[1]
            batch = state.get("batch") or {}
            if batch.get("finished"):
                break
            time.sleep(.25)
        assert batch.get("finished") and all(row["ok"] for row in batch.get("results", [])), batch
        llm_logs = [json.loads(p.read_text()) for p in (project / ".dualdag" / "llm_log").glob("*.json")]
        chg_calls = [row for row in llm_logs if "Current carry-over group" in row.get("user", "") and "Current variable: CHG" in row.get("user", "")]
        assert len(chg_calls) == 2 and {row.get("side") for row in chg_calls} == {"primary", "validation"}, len(chg_calls)

        # Compare both sides' actual prompt cards with the backend roles and review DOM.
        import re
        for target in (group_av, "ADVS.AVAL@BMI", "ADVS.AVAL@OTHER"):
            lineage_status, lineage = request(link, f"/api/node/lineage?dataset=ADVS&node={target}")
            assert lineage_status == 200
            assert lineage["target_id"] == target
            assert target in lineage["ids"] and target not in lineage["upstream_ids"]
            assert set(lineage["ids"]) == {target, *lineage["upstream_ids"]}, (target, lineage)
            roles = {item["id"]: item for item in lineage["roles"]}
            assert roles[target]["role"] == "target"
            assert set(roles) == set(lineage["ids"])
            if target != group_av:
                assert "ADVS.AVAL@CARRY_OVER" in lineage["upstream_ids"]
            assert not any("@WEIGHT" in node or "@HEIGHT" in node for node in lineage["upstream_ids"])
            for side in ("primary", "validation"):
                prompt = next(row["user"] for row in llm_logs if f"NODE: {target}" in row.get("user", "") and row.get("side") == side)
                cards_sent = set(re.findall(r"(?m)^CARD_BEGIN ([^\n]+)$", prompt))
                expected_cards = {node for node in lineage["upstream_ids"] if roles[node]["context"].get(side) == "card"}
                assert cards_sent == expected_cards, (target, side, cards_sent, expected_cards)
                assert target not in cards_sent, (target, side, cards_sent)
                assert all(roles[node]["context"].get(side) in ("card", "spec", "work", "dependency") for node in lineage["upstream_ids"]), (target, side, roles)
                for node_id in lineage["upstream_ids"]:
                    if roles[node_id]["context"].get(side) == "spec":
                        assert f"{node_id}\nROW:" in prompt, (target, side, node_id)

            page.goto(link + "&tab=spec")
            page.reload()
            layer = target.rsplit("@", 1)[1]
            if layer == "CARRY_OVER":
                page.locator('[data-act="layer-select"][data-layer="CARRY_OVER"]').click()
            else:
                page.locator(f'.param-derived-row[data-layer="{layer}"]').click()
            node = page.locator(f'.node[data-id="{target}"]')
            node.wait_for(state="attached", timeout=10000)
            node.evaluate("el => el.dispatchEvent(new MouseEvent('contextmenu', {bubbles:true, cancelable:true, clientX:100, clientY:100}))")
            page.get_by_role("menuitem", name="Review this node").click()
            page.locator(f'.node[data-id="{target}"][data-lineage-role="target"]').wait_for(state="attached", timeout=10000)
            dom_inputs = set(page.locator('.node[data-lineage-role="upstream"]').evaluate_all("els => els.map(el => el.getAttribute('data-id'))"))
            assert dom_inputs == set(lineage["upstream_ids"]), (target, dom_inputs, lineage["upstream_ids"])
            for node_id in lineage["upstream_ids"]:
                rendered = page.locator(f'.node[data-id="{node_id}"]')
                assert json.loads(rendered.get_attribute("data-lineage-context")) == roles[node_id]["context"], (target, node_id)
            dom_edges = set(page.locator("#dag path.edge[data-from][data-to]").evaluate_all("els => els.map(el => el.getAttribute('data-from') + '\\t' + el.getAttribute('data-to'))"))
            required_edges = {edge["from"] + "\t" + edge["to"] for edge in lineage["edges"]}
            assert required_edges.issubset(dom_edges), (target, required_edges, dom_edges)
            assert page.locator(f'[data-lineage-target="{target}"]').count() == 1
            for side in ("primary", "validation"):
                context = lineage["source_context"][side]
                for ref in context["requested"]:
                    supplied = ref in context["supplied"]
                    row = page.locator(f'[data-lineage-context-source="{ref}"][data-context-side="{side}"]')
                    assert row.get_attribute("data-supplied") == str(supplied).lower(), (target, side, ref, context)
        page.goto(link + "&tab=run")
        page.wait_for_timeout(500)
        page.screenshot(path=str(SHOTS / f"{label}-run-queue.png"), full_page=True)

        # SU is real app traffic but remains on the selected group node and fake model.
        status, su = request(link, "/api/node/su", {"dataset": "ADVS", "node": group_av, "side": "primary", "message": "Summarize this group variable program."})
        assert status == 200 and su.get("ok", True), (status, su)

        # Full Run through the app route, independently for primary and validation.
        for side in ("primary", "validation"):
            status, payload = request(link, "/api/fullrun/run", {"dataset": "ADVS", "side": side})
            assert status == 202 and payload.get("running"), (side, status, payload)
        for _ in range(180):
            status, fr = request(link, "/api/fullrun/status?dataset=ADVS")
            assert status == 200
            if all(not s.get("running") for s in fr["sides"].values()):
                break
            time.sleep(.25)
        assert all(s.get("run", {}).get("ok") for s in fr["sides"].values()), fr
        for side in ("primary", "validation"):
            program_status, program = request(link, f"/api/fullrun/program?dataset=ADVS&side={side}")
            assert program_status == 200 and "ADVS_CARRY_OVER_TEMP" in program, side
            output = project / ".dualdag" / "fullrun" / "ADVS" / side / "result.rds"
            assert output.exists(), output
            check_code = "x<-readRDS(commandArgs(TRUE)[1]);stopifnot(setequal(unique(x$PARAMCD),c('WEIGHT','HEIGHT','BMI','OTHER')),nrow(x)==8L);b<-x[x$PARAMCD=='BMI',];stopifnot(isTRUE(all.equal(b$AVAL[match(c('S1','S2'),b$USUBJID)],c(70/(1.7^2),80/(1.8^2)),tolerance=1e-10)));cat('rows=',nrow(x),' members=',paste(sort(unique(x$PARAMCD)),collapse=','),' BMI=',paste(signif(b$AVAL,6),collapse=','),'\\n',sep='')"
            checked = subprocess.run([RSCRIPT, "-e", check_code, str(output)], check=True, env=env,
                stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True).stdout
            assert "rows=8 members=BMI,HEIGHT,OTHER,WEIGHT" in checked, checked
        # A fixture-only value perturbation exercises mismatch key retention.
        run(group_av)
        validation_out = project / ".dualdag" / "runs" / "ADVS" / "AVAL@CARRY_OVER" / "validation" / "out.rds"
        mismatch_code = "x<-readRDS(commandArgs(TRUE)[1]);i<-x$PARAMCD=='WEIGHT';x$AVAL[i]<-x$AVAL[i]+1;saveRDS(x,commandArgs(TRUE)[1])"
        subprocess.run([RSCRIPT, "-e", mismatch_code, str(validation_out)], check=True, env=env,
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        mismatch = compare(group_av)["compare"]
        assert not mismatch["match"] and any(row.get("PARAMCD") == "WEIGHT" for row in mismatch.get("diffs", [])), mismatch
        page.goto(link + "&tab=fullrun"); page.wait_for_timeout(500)
        page.screenshot(path=str(SHOTS / f"{label}-fullrun.png"), full_page=True)
        assert not errors, errors
    finally:
        if page:
            page.close()
        server.terminate(); server.wait(timeout=10)


def main():
    work = Path(tempfile.mkdtemp(prefix="wei21-fullrun-ui-"))
    try:
        with sync_playwright() as playwright:
            chrome = playwright.chromium.launch(headless=True, executable_path=CHROME)
            check(chrome, work, "chromium", BASE_PORT)
            chrome.close()
            firefox = playwright.firefox.launch(headless=True)
            check(firefox, work, "firefox", BASE_PORT + 1)
            firefox.close()
        print(f"Chromium/Firefox carry-over Generate, queue, SU, compare, and Full Run passed; screenshots: {SHOTS}")
    finally:
        # Keep the isolated server projects and screenshots available for inspection.
        pass


if __name__ == "__main__":
    main()
