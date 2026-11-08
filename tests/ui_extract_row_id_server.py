"""Private fake-model HTTP regression for ROW-prefixed extraction replies."""
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
ADVS,CHG,Change,Num,8,,ADVS.AVAL,AVAL minus zero.
"""
PARAMETER = """Dataset,PARAMCD,PARAM,Source,Derivation
ADVS,P1,Carry-over source group CARRY_OVER,VS.VSTESTCD,Retain carry-over records.
"""


def free_port():
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


def run_browser(browser_type, work, label):
    home, project = work / f"{label}-home", work / f"{label}-project"
    home.mkdir(); project.mkdir()
    (home / ".dualdag").mkdir()
    (home / ".dualdag" / "config.json").write_text('{"marker":"scratch-only"}\n')
    (home / "config.json").write_text(json.dumps({"provider": "openai", "model": "fake-test"}))
    port = free_port()
    env = dict(os.environ, HOME=str(home), DUALDAG_HOME=str(home / "dualdag"),
               DUALDAG_FAKE_LLM="1", DUALDAG_FAKE_ROW_PREFIX="1",
               DUALDAG_FAKE_GROUP_PROSE_ONCE="1", DUALDAG_LLM_API_KEY="")
    log = work / f"{label}.log"
    server = subprocess.Popen([RSCRIPT, "run.R", f"--project={project}", f"--port={port}", "--no-browser"],
                              cwd=ROOT, env=env, stdout=log.open("w"), stderr=subprocess.STDOUT)
    browser = browser_type.launch(headless=True, executable_path=CHROME if label == "chromium" else FIREFOX)
    try:
        link = None
        for _ in range(120):
            text = log.read_text(errors="replace") if log.exists() else ""
            for line in text.splitlines():
                if "Open:" in line and "#token=" in line:
                    link = line.split("Open:", 1)[1].strip(); break
            if link: break
            if server.poll() is not None:
                raise RuntimeError(f"{label} fake server exited: {text[-2000:]}")
            time.sleep(.25)
        assert link, f"{label} fake server did not start: {log.read_text(errors='replace')[-2000:]}"
        page = browser.new_page()
        page.goto(link + "&tab=settings")
        base, token = link.split("/#token=")
        headers = {"X-Dualdag-Token": token}
        upload = page.request.post(base + "/api/spec/upload", headers=headers, data={
            "variable": {"name": "variable.csv", "base64": base64.b64encode(VARIABLE.encode()).decode()},
            "parameter": {"name": "parameter.csv", "base64": base64.b64encode(PARAMETER.encode()).decode()},
        })
        assert upload.status == 200, upload.text()
        response = page.request.post(base + "/api/param-graph/extract", headers=headers, data={"dataset": "ADVS"})
        assert response.status == 200, response.text()
        pg = response.json()
        assert not pg.get("error"), pg
        response = page.request.post(base + "/api/graph/parameter-extract", headers=headers,
                                     data={"dataset": "ADVS", "paramcd": "CARRY_OVER"})
        assert response.status == 200, response.text()
        extracted = response.json()
        assert "CARRY_OVER" in extracted.get("generated_parameters", []), extracted.get("generated_parameters")
        stored = json.loads((project / ".dualdag" / "state" / "graph_ADVS.json").read_text())
        assert "CARRY_OVER" in stored.get("generated_parameters", []), stored.get("generated_parameters")
        edges = [e for e in stored.get("edges", []) if e.get("origin") != "structural"]
        assert any(e.get("from") == "ADVS.AVAL@CARRY_OVER" and e.get("to") == "ADVS.CHG@CARRY_OVER" for e in edges), edges
        extraction = json.loads((project / ".dualdag" / "state" / "extract_ADVS.json").read_text())
        external = extraction.get("external", [])
        assert any(e.get("ref") == "VS.VSSTRESN"
                   and "AVAL" in e.get("used_by", [])
                   and "CARRY_OVER" in e.get("params", []) for e in external), external
        network_path = project / ".dualdag" / "state" / "network_ADVS" / "CARRY_OVER.json"
        network = json.loads(network_path.read_text())
        assert any(i.get("from") == "VS.VSSTRESN"
                   and i.get("to") == "ADVS.AVAL@CARRY_OVER" for i in network.get("inputs", [])), network

        # Simulate a saved graph before regeneration: stale group-only input plus
        # unrelated derived-parameter and dataset-level references must be handled
        # by the final save path, not merely by the intermediate extraction reply.
        graph_path = project / ".dualdag" / "state" / "graph_ADVS.json"
        stored["nodes"].extend([
            {"id": "ADVS.DERIVED@P2", "var": "DERIVED", "paramcd": "P2", "kind": "parameter_variable"},
            {"id": "ADVS.SEX", "var": "SEX", "kind": "dataset_variable"},
        ])
        generated_parameters = stored.get("generated_parameters", [])
        if isinstance(generated_parameters, str):
            generated_parameters = [generated_parameters]
        stored["generated_parameters"] = generated_parameters + ["P2"]
        stored["external"].extend([
            {"ref": "VS.STALE", "used_by": ["ADVS.AVAL@CARRY_OVER"], "params": ["CARRY_OVER"]},
            {"ref": "VS.DERIVED", "used_by": ["ADVS.DERIVED@P2"], "params": ["P2"]},
            {"ref": "ADSL.SEX", "used_by": ["ADVS.SEX"], "evidence": "dataset level"},
        ])
        graph_path.write_text(json.dumps(stored))
        response = page.request.post(base + "/api/graph/parameter-extract", headers=headers,
                                     data={"dataset": "ADVS", "paramcd": "CARRY_OVER"})
        assert response.status == 200, response.text()
        regenerated = json.loads(graph_path.read_text())
        saved_external = regenerated.get("external", [])
        assert any(e.get("ref") == "VS.VSSTRESN" and "ADVS.AVAL@CARRY_OVER" in e.get("used_by", [])
                   for e in saved_external), saved_external
        assert not any(e.get("ref") == "VS.STALE" for e in saved_external), saved_external
        assert any(e.get("ref") == "VS.DERIVED" and "ADVS.DERIVED@P2" in e.get("used_by", [])
                   for e in saved_external), saved_external
        assert any(e.get("ref") == "ADSL.SEX" and "ADVS.SEX" in e.get("used_by", [])
                   for e in saved_external), saved_external
        regenerated_network = json.loads(network_path.read_text())
        assert any(i.get("from") == "VS.VSSTRESN" and i.get("to") == "ADVS.AVAL@CARRY_OVER"
                   for i in regenerated_network.get("inputs", [])), regenerated_network
        assert not any(i.get("from") == "VS.STALE" for i in regenerated_network.get("inputs", [])), regenerated_network
        llm_log = project / ".dualdag" / "llm_log"
        logged = [json.loads(path.read_text()) for path in llm_log.glob("*.json")]
        group_requests = [x for x in logged if "CARRY_OVER_PARAMETERS:" in x.get("user", "")]
        assert len(group_requests) == 4, group_requests
        retry_users = [x["user"] for x in group_requests if "RETRY CORRECTION" in x["user"]]
        assert len(retry_users) == 2, group_requests
        assert "prose" in retry_users[0] and "invalid" in retry_users[0]
        print(f"{label}: grouped retries and regeneration retained graph/network inputs; stale group input removed")
    finally:
        browser.close()
        server.terminate()
        server.wait(timeout=15)


def main():
    work = Path(tempfile.mkdtemp(prefix="dualdag-row-id-private-server-"))
    with sync_playwright() as playwright:
        run_browser(playwright.chromium, work, "chromium")
        run_browser(playwright.firefox, work, "firefox")


if __name__ == "__main__":
    main()
