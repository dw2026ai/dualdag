// dualdag front end. ES2017, no frameworks, no build step.
// Session token comes from location.hash (#token=...) and is sent as X-Dualdag-Token.
// Dev only: ?mock=1 serves a canned study so the page can be opened without R.
"use strict";
(function () {
  const MOCK = /(?:\?|&)mock=1(?:&|$)/.test(location.search);
  const AUTODAG = /(?:\?|&)autodag=1(?:&|$)/.test(location.search);
  const SHOW_QC_TAB = false;
  const SHOW_DAG_MULTI_SELECT = false;
  window.__dualdagErrors = window.__dualdagErrors || [];
  const ACT_EN = {
    "Generate": "generate", "Generate & run": "generate",
    "Compare": "compare", "Investigate": "investigate"
  };
  function readDagEditNotes() {
    try {
      const notes = JSON.parse(localStorage.getItem("dualdag_manual_graph_edits") || "{}");
      Object.keys(notes || {}).forEach(function (ds) {
        notes[ds] = asArray(notes[ds]).map(function (note) {
          if (note && typeof note === "object") return note;
          const text = String(note || ""), match = text.match(/^(Disconnected|Connected) (.+) → (.+)$/);
          return { text: text, undoLabel: match ? (match[1] === "Disconnected" ? "- edge " : "+ edge ") + match[2] + " -> " + match[3] : "" };
        });
      });
      return notes;
    }
    catch (err) { return {}; }
  }
  function saveDagEditNotes() {
    try { localStorage.setItem("dualdag_manual_graph_edits", JSON.stringify(ui.dagEditNotes || {})); }
    catch (err) { /* Keep graph edits usable when storage is unavailable. */ }
  }
  function reconcileDagEditNotes(state) {
    if (!state || !state.datasets || !ui.dagEditNotes) return;
    let changed = false;
    Object.keys(ui.dagEditNotes).forEach(function (ds) {
      const pack = state.datasets[ds], graph = pack && pack.graph;
      if (!graph || !Array.isArray(graph.edges)) return;
      const edges = graph.edges;
      const notes = asArray(ui.dagEditNotes[ds]);
      const kept = notes.filter(function (note) {
        const text = String(note && typeof note === "object" ? note.text || "" : note || "");
        const parts = text.split(/;\s*/).map(function (part) {
          const match = part.match(/^(Disconnected|Connected) (.+) → (.+)$/);
          return match ? { connected: match[1] === "Connected", from: match[2], to: match[3] } : null;
        });
        if (!parts.length || parts.some(function (part) { return !part; })) return true;
        return !parts.some(function (part) {
          const exists = edges.some(function (edge) { return edge.from === part.from && edge.to === part.to; });
          return part.connected ? !exists : exists;
        });
      });
      if (kept.length !== notes.length) {
        ui.dagEditNotes[ds] = kept;
        changed = true;
      }
    });
    if (changed) saveDagEditNotes();
  }

  let S = null;
  const ui = {
    tab: "spec",
    dataset: null, node: null, detail: null, specTab: "variable", specFilter: "", specAll: false,
    pin: {}, sel: [], selRows: [], fromSpec: false, paramFilter: {}, layerChoice: {}, layerLayouts: {},
    compareAfter: true, selectOpen: false,
    codeGuide: false,
    codeScope: "var", program: null, programDs: "", programErr: "",
    qcList: [], qcDs: "", files: {}, msg: "", inflight: false,
    cfgDraft: null, cfgTest: null, cfgOther: false, cliStatus: null, cliAutoDetectDone: false,
    fullRun: { dataset: "", status: null, steps: {}, errors: {}, statusError: "", timer: null, busy: false, started: {}, loadingDataset: "" },
    extractJobs: {}, extractErrors: {}, extractTimer: null,
    coderWindow: null, coderWindowTimer: null, superAgent: null, appAdmin: null, appAdminTimer: null,
    catalog: null, catalogStatus: "", catOpen: {}, catFilter: {},
    view: { x: 16, y: 16, k: 1 }, didFit: false, userView: false, layout: null, drag: null, poll: null,
    layoutSave: null, category: null,
    fold: {}, runSide: "both", running: false, runPoll: null, wantNode: "",
    dagOpen: true, dagDraft: "", dagPending: null, dagHideAt: "", dagFlash: {},
    dagContext: null, dagReview: null, dagEditBusy: false, dagUndoBusy: false, dagEditNotes: readDagEditNotes(),
    dagAsking: false, autodagDone: "", runQueues: {}, runQueueHiddenResults: {}, runQueueDragging: "", runQueueOver: false,
    graphMode: {}, paramGraphs: {}, paramTurns: {}, paramDraft: "", paramPending: null, paramAsking: false, paramErrors: {}, dagActionWait: null,
    diff: {
      node: "", onlyDiff: true, filter: "", page: 1, size: 100,
      view: null, err: "", loading: false, focus: false, split: 50
    },
    data: {
      list: null, missing: false, err: "", filterNode: "", openId: null, view: null,
      side: "primary", offset: 0, limit: 100, filter: "", colsOn: null, allCols: null, loading: false, viewErr: ""
    }
  };

  try { ui.codeGuide = localStorage.getItem("dualdag.codeGuide") === "1"; } catch (err) { ui.codeGuide = false; }
  try { ui.data.side = localStorage.getItem("dualdag.dataSide") === "validation" ? "validation" : "primary"; } catch (err) { ui.data.side = "primary"; }
  try { ui.parameterClassificationCollapsed = localStorage.getItem("dualdag.parameterClassificationCollapsed") === "1"; } catch (err) { ui.parameterClassificationCollapsed = false; }
  ui.fold["g:input"] = true;
  ui.fold["g:result"] = true;
  const EXPLAIN_Q = "Explain this code and its log.";

  function esc(s) {
    return String(s == null ? "" : s).replace(/[&<>"']/g, function (c) {
      return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" }[c];
    });
  }
  function tokenCount(n) {
    const value = Number(n);
    if (!Number.isFinite(value)) return "";
    if (value < 1000) return String(Math.round(value));
    return (value / 1000).toFixed(1).replace(/\.0$/, "") + "K";
  }
  function contextUsageLine(usage) {
    if (!usage) return "";
    const input = usage.input == null ? NaN : Number(usage.input);
    const calls = Number(usage.calls || 1);
    let text;
    if (!Number.isFinite(input)) text = "Context: not reported";
    else {
      const used = tokenCount(input);
      const limit = Number(usage.limit);
      text = Number.isFinite(limit) && limit > 0
        ? "Context: " + used + " of " + tokenCount(limit) + " tokens (" + Math.round(input / limit * 100) + "%)"
        : "Context: " + used + " tokens";
    }
    if (calls > 1) text += " · " + calls + " model calls";
    return '<div class="context-usage">' + esc(text) + "</div>";
  }
  function sessionUsageLabel(usage) {
    if (!usage || usage.input == null && usage.output == null) return "";
    const total = (Number(usage.input) || 0) + (Number(usage.output) || 0);
    return "Session: " + tokenCount(total) + " tokens used";
  }
  function renderConsoleLog(text) {
    const lines = String(text == null ? "" : text).split("\n");
    let state = "", errors = 0, warnings = 0, firstError = -1, firstWarning = -1;
    const html = lines.map(function (line, i) {
      if (/^\s*(?:Error(?: in\b|\b)|Execution halted\b|DUALDAG_MISSING_(?:DEPENDENCY|COLUMN)\b)/.test(line)) {
        if (state !== "error") { errors++; if (firstError < 0) firstError = i; }
        state = "error";
      } else if (/^\s*(?:Warning(?: messages?:)?\b|In .+\s:\s*)/.test(line)) {
        if (state !== "warning") { warnings++; if (firstWarning < 0) firstWarning = i; }
        state = "warning";
      } else if (/^\s*(?:>|\+|#)\s|^\s*$/.test(line)) {
        state = "";
      }
      const missingToken = line.match(/DUALDAG_MISSING_(DEPENDENCY|COLUMN):\s*(.*)$/);
      let displayLine = line;
      if (missingToken) {
        const detail = missingToken[2];
        if (/^Missing column:/.test(detail)) displayLine = detail;
        else {
          const ref = detail.split(/[\s,]/)[0];
          const pieces = ref.split(".");
          const column = pieces[pieces.length - 1] || ref;
          const frame = pieces.length > 1 ? "LINEAGE_" + pieces[0] : "LINEAGE_SYSBP";
          displayLine = "Missing column: " + column + " is not in " + frame + " or the provided data.";
        }
      }
      return '<span class="logline' + (state ? ' log-' + state : "") + '" data-log-line="' + i + '">' + esc(displayLine) + '</span>';
    }).join("\n");
    return { html: html, errors: errors, warnings: warnings,
             firstError: firstError, firstWarning: firstWarning };
  }
  function logBadges(side, info) {
    return (info.errors ? '<button type="button" class="logbadge error" data-act="log-jump" data-side="' + side + '" data-kind="error" data-line="' + info.firstError + '">' + info.errors + ' error' + (info.errors === 1 ? '' : 's') + '</button>' : '')
      + (info.warnings ? '<button type="button" class="logbadge warning" data-act="log-jump" data-side="' + side + '" data-kind="warning" data-line="' + info.firstWarning + '">' + info.warnings + ' warning' + (info.warnings === 1 ? '' : 's') + '</button>' : '');
  }
  function trunc(s, n) {
    s = String(s == null ? "" : s);
    return s.length > n ? s.slice(0, n - 1) + "…" : s;
  }
  function clamp(v, a, b) { return Math.max(a, Math.min(b, v)); }
  let paneResizeDrag = null, paneResizePending = false, coderResizeActive = false, coderResizeTarget = null;
  function paneSize(tab, name, index, fallback) {
    try {
      const saved = JSON.parse(localStorage.getItem("dualdag.panes." + tab) || "{}");
      return Number.isFinite(saved[name + ":" + index]) && saved[name + ":" + index] >= 0
        ? saved[name + ":" + index] : fallback;
    } catch (err) { return fallback; }
  }
  function savePaneSize(tab, name, index, value) {
    try {
      const key = "dualdag.panes." + tab, saved = JSON.parse(localStorage.getItem(key) || "{}");
      saved[name + ":" + index] = value; localStorage.setItem(key, JSON.stringify(saved));
    } catch (err) { /* Resizing remains available for this session. */ }
  }
  function bindResizablePanes(root) {
    const specs = [
      ["spec", ".tab-spec .maincol", [".dagrow", ".split"], "y"],
      ["spec", ".tab-spec .dagrow", [".dagmain", ".dagagent"], "x"],
      ["spec", ".tab-spec .dag-canvas-row", function (parent) {
        const panes = [];
        if (parent.querySelector(".run-queue")) panes.push(".run-queue");
        panes.push(".dag-canvas-main");
        if (parent.querySelector(".param-classification")) panes.push(".param-classification");
        return panes;
      }, "x"],
      ["code", ".tab-code .codesplit", [".codeside:nth-of-type(1)", ".codeside:nth-of-type(2)"], "x"],
      ["code", ".tab-code .code-compare-layout", [".codesplit", ".code-compare"], "y"],
      ["code", ".tab-code .codeside .code-body-split", [".code-area", ".log-area"], "y"],
      ["code", ".tab-code .codeside:nth-of-type(2) .code-body-split", [".code-area", ".log-area"], "y"],
      ["data", ".tab-data", [".datalist", ".viewer"], "x"],
      ["mismatches", ".tab-mismatches", [".datalist", ".viewer"], "x"],
      ["mismatches", ".tab-mismatches .diffpanes", [".diffpane:nth-child(1)", ".diffpane:nth-child(2)"], "x"],
      ["run", ".tab-run .codesplit", [".pane:nth-of-type(1)", ".pane:nth-of-type(2)"], "x"],
      ["run", ".tab-run .pane:nth-of-type(1) .scroll", [".run-program-area", ".run-log-area", ".run-controls"], "y"],
      ["run", ".tab-run .pane:nth-of-type(2) .scroll", [".run-program-area", ".run-log-area", ".run-controls"], "y"]
    ];
    specs.forEach(function (spec) {
      const tab = spec[0], selector = spec[1], children = spec[2], axis = spec[3];
      const parent = root.querySelector(selector); if (!parent) return;
      const panes = (typeof children === "function" ? children(parent) : children).map(function (s) { return parent.querySelector(s); });
      if (panes.some(function (p) { return !p || p.parentElement !== parent; })) return;
      const name = selector === ".tab-spec .dag-canvas-row" && !parent.querySelector(".run-queue") ? selector + ":parameter" : selector;
      const initial = panes.map(function (pane) { const r = pane.getBoundingClientRect(); return axis === "x" ? r.width : r.height; });
      const initialTotal = initial.reduce(function (a, b) { return a + b; }, 0);
      const css = getComputedStyle(parent);
      const before = axis === "x" ? parent.clientWidth - parseFloat(css.paddingLeft) - parseFloat(css.paddingRight)
        : parent.clientHeight - parseFloat(css.paddingTop) - parseFloat(css.paddingBottom);
      const gap = parseFloat(axis === "x" ? css.columnGap : css.rowGap) || 0;
      const trackSpace = Math.max(80 * panes.length, before - gap * (panes.length - 1) - 7 * (panes.length - 1));
      let defaults = initialTotal > 0 ? initial.map(function (size) { return size / initialTotal; })
        : panes.map(function () { return 1 / panes.length; });
      if (name === ".tab-spec .dagrow") defaults = [(trackSpace - 300) / trackSpace, 300 / trackSpace];
      else if (name.indexOf(".tab-spec .dag-canvas-row") === 0) defaults = panes.length === 3
        ? [200 / trackSpace, (trackSpace - 540) / trackSpace, 340 / trackSpace]
        : panes.length === 2 ? [panes[0].classList.contains("run-queue") ? 200 / trackSpace : (trackSpace - 340) / trackSpace,
          panes[0].classList.contains("run-queue") ? (trackSpace - 200) / trackSpace : 340 / trackSpace] : [1];
      else if (name.indexOf(".code-body-split") >= 0) defaults = [0.6, 0.4];
      else if (tab === "run" && panes.length === 3 && !String(panes[1].textContent || "").trim()) defaults = [1, 0, 0];
      const sizes = panes.map(function (_, i) { return paneSize(tab, name, i, defaults[i]); });
      if (name.indexOf(".tab-spec .dag-canvas-row") === 0 && panes.length >= 2 && panes[panes.length - 1].classList.contains("is-collapsed")) {
        const stripIndex = panes.length - 1, strip = Math.min(180, trackSpace * 0.35) / trackSpace;
        const others = sizes.slice(0, stripIndex).reduce(function (a, b) { return a + b; }, 0);
        for (let i = 0; i < stripIndex; i++) sizes[i] = others > 0 ? sizes[i] * (1 - strip) / others : (1 - strip) / stripIndex;
        sizes[stripIndex] = strip;
      }
      parent.classList.add("resize-grid", axis === "x" ? "resize-x" : "resize-y");
      const tracks = [];
      panes.forEach(function (pane, i) {
        pane.style.minWidth = "0"; pane.style.minHeight = "0";
        pane.style.flex = "none";
        if (axis === "x") { pane.style.width = "auto"; pane.style.maxWidth = "none"; }
        else { pane.style.height = "auto"; pane.style.maxHeight = "none"; }
        tracks.push("minmax(80px, " + sizes[i] + "fr)");
        if (i < panes.length - 1) {
          const bar = document.createElement("div");
          bar.className = "pane-resize-bar"; bar.dataset.resizeTab = tab; bar.dataset.resizeName = name;
          bar.dataset.resizeIndex = i; bar.dataset.resizeAxis = axis; bar.setAttribute("role", "separator");
          bar.setAttribute("aria-orientation", axis === "x" ? "vertical" : "horizontal"); bar.tabIndex = 0;
          pane.after(bar); tracks.push("7px");
          bar.addEventListener("dblclick", function () {
            panes.forEach(function (p, j) { p.style.flex = ""; savePaneSize(tab, name, j, defaults[j]); }); render();
          });
          bar.addEventListener("pointerdown", function (e) {
            if (e.button != null && e.button !== 0) return;
            e.preventDefault(); paneResizeDrag = { bar: bar, parent: parent, panes: panes, tab: tab, name: name, index: i, axis: axis };
            try { bar.setPointerCapture(e.pointerId); } catch (err) { /* window listeners also track the drag */ }
          });
        }
      });
      parent.style.gridTemplateColumns = axis === "x" ? tracks.join(" ") : "minmax(0, 1fr)";
      parent.style.gridTemplateRows = axis === "y" ? tracks.join(" ") : "minmax(0, 1fr)";
      parent.querySelectorAll(":scope > .pane-resize-bar").forEach(function (bar, i) {
        bar.addEventListener("pointerdown", function () { bar.dataset.resizeIndex = i; });
      });
    });
  }
  window.addEventListener("pointermove", function (e) {
    const d = paneResizeDrag; if (!d) return;
    const rect = d.panes[d.index].getBoundingClientRect();
    const origin = d.axis === "x" ? rect.left : rect.top, pos = (d.axis === "x" ? e.clientX : e.clientY) - origin;
    const sizes = d.panes.map(function (pane) { const r = pane.getBoundingClientRect(); return d.axis === "x" ? r.width : r.height; });
    const usable = Math.max(1, sizes[d.index] + sizes[d.index + 1]), min = 80, left = clamp(pos, min, usable - min);
    sizes[d.index] = left; sizes[d.index + 1] = usable - left;
    const sum = sizes.reduce(function (a, b) { return a + b; }, 0);
    const tracks = [];
    d.panes.forEach(function (_, i) {
      tracks.push("minmax(80px, " + (sizes[i] / sum) + "fr)");
      if (i < d.panes.length - 1) tracks.push("7px");
    });
    if (d.axis === "x") d.parent.style.gridTemplateColumns = tracks.join(" "); else d.parent.style.gridTemplateRows = tracks.join(" ");
    sizes.forEach(function (size, i) { savePaneSize(d.tab, d.name, i, size / sum); });
  });
  window.addEventListener("pointerup", function () {
    if (!paneResizeDrag) return; paneResizeDrag = null;
    if (paneResizePending) { paneResizePending = false; render(); }
  });
  function asArray(v) {
    if (!v) return [];
    if (Array.isArray(v)) return v;
    if (typeof v === "string") return [v];
    return [];
  }
  function asRows(df) {
    if (!df) return [];
    if (Array.isArray(df)) return df;
    if (typeof df !== "object") return [];
    const keys = Object.keys(df);
    if (!keys.length || !Array.isArray(df[keys[0]])) return [];
    const n = df[keys[0]].length;
    const rows = [];
    for (let i = 0; i < n; i++) {
      const r = {};
      keys.forEach(function (k) { r[k] = df[k][i]; });
      rows.push(r);
    }
    return rows;
  }
  function httpErr(status, msg, data) {
    const e = new Error(msg);
    e.status = status;
    e.data = data || { error: msg };
    return e;
  }
  function tokenBannerText() {
    return "This link's token is wrong or expired. Open the address printed by run.R.";
  }
  function showTokenBanner() {
    if (document.getElementById("token-banner")) return;
    const el = document.createElement("div");
    el.id = "token-banner";
    el.className = "banner";
    el.innerHTML = "<b>" + tokenBannerText() + "</b>";
    const app = document.getElementById("app");
    (app || document.body).insertBefore(el, (app || document.body).firstChild);
  }
  function isBadToken(status, msg) {
    return status === 403 && /bad token/i.test(String(msg || ""));
  }
  function asHttpErr(status, msg, data) {
    if (isBadToken(status, msg)) {
      showTokenBanner();
      const e = httpErr(status, tokenBannerText(), data);
      e.token = true;
      return e;
    }
    return httpErr(status, msg, data);
  }
  function showErr(e) {
    if (e && e.token) { showTokenBanner(); return; }
    ui.msg = e && e.message ? e.message : String(e);
    const el = document.getElementById("st");
    if (el) el.textContent = ui.msg;
  }
  function setMsg(m) {
    ui.msg = m || "";
    const el = document.getElementById("st");
    if (el) el.textContent = ui.msg;
  }

  function readHash() {
    const out = {};
    const h = String(location.hash || "").replace(/^#/, "");
    if (!h) return out;
    h.split("&").forEach(function (pair) {
      if (!pair) return;
      const i = pair.indexOf("=");
      const k = decodeURIComponent(i >= 0 ? pair.slice(0, i) : pair);
      let v = decodeURIComponent(i >= 0 ? pair.slice(i + 1) : "");
      // A wrapped URL can put %20 or a literal break inside the token.
      if (k === "token") v = v.replace(/\s+/g, "");
      out[k] = v;
    });
    return out;
  }
  function syncHash() {
    const node = ui.tab === "mismatches" ? (ui.diff.node || "") : (ui.node || "");
    writeHash({
      tab: ui.tab,
      node: node,
      view: ui.codeScope === "all" ? "all" : "",
      diff: ""
    });
  }
  function writeHash(updates) {
    const cur = readHash();
    Object.keys(updates || {}).forEach(function (k) {
      if (updates[k] == null || updates[k] === "") delete cur[k];
      else cur[k] = String(updates[k]);
    });
    const prefer = { token: 0, tab: 1 };
    const keys = Object.keys(cur).sort(function (a, b) {
      const ra = a in prefer ? prefer[a] : 50;
      const rb = b in prefer ? prefer[b] : 50;
      if (ra !== rb) return ra - rb;
      return a < b ? -1 : a > b ? 1 : 0;
    });
    const s = keys.map(function (k) { return encodeURIComponent(k) + "=" + encodeURIComponent(cur[k]); }).join("&");
    const next = location.pathname + location.search + (s ? "#" + s : "");
    const now = location.pathname + location.search + location.hash;
    if (next !== now) history.replaceState(null, "", next);
  }
  let tabPinned = false;
  function normTab(v) {
    if (v === "1" || v === "spec" || v === "dag") return "spec";
    if (v === "2" || v === "code") return "code";
    // The standalone Run page is temporarily hidden. Keep older bookmarks usable.
    if (v === "run") return "spec";
    if (v === "3" || v === "data") return "data";
    if (v === "5" || v === "mismatches" || v === "mismatch" || v === "diff") return "mismatches";
    if (v === "qc") return SHOW_QC_TAB ? "qc" : "code";
    if (v === "4" || v === "settings") return "settings";
    return "spec";
  }
  function hashTab(hp) {
    const raw = hp && hp.tab ? String(hp.tab) : "";
    const tab = raw ? normTab(raw) : "";
    if ((hp && (hp.diff === "1" || hp.view === "diff")) && (!raw || tab === "data")) return "mismatches";
    return tab;
  }
  (function initHash() {
    try {
      const hp = readHash();
      if (hp.token) sessionStorage.setItem("dualdag_token", hp.token);
      const tab = hashTab(hp);
      if (tab) {
        ui.tab = tab;
        tabPinned = true;
        if (String(hp.tab || "") === "dag") syncHash();
      }
      if (hp.view === "all") ui.codeScope = "all";
      if (hp.diff === "1" || hp.view === "diff") {
        ui.diff.focus = true;
        ui.fold.diff = false;
      }
      if (hp.node) {
        ui.wantNode = hp.node;
        ui.diff.node = hp.node;
      }
    } catch (e) { /* private mode: keep the hash, including #token= */ }
  })();
  function token() {
    try { return sessionStorage.getItem("dualdag_token") || ""; }
    catch (e) { return ""; }
  }
  async function api(path, body) {
    if (MOCK) return mockApi(path, body);
    const headers = { "X-Dualdag-Token": token() };
    const opt = { cache: "no-store", headers: headers };
    if (body !== undefined) {
      opt.method = "POST";
      headers["content-type"] = "application/json";
      opt.body = JSON.stringify(body);
    }
    const r = await fetch(path, opt);
    const t = await r.text();
    let j = null;
    try { j = t ? JSON.parse(t) : null; } catch (e) { j = null; }
    if (!r.ok) throw asHttpErr(r.status, (j && j.error) || (r.status + " " + t.slice(0, 180)), j || { error: t.slice(0, 180) });
    return j;
  }
  function saveBlob(blob, name) {
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = name || "download";
    document.body.appendChild(a);
    a.click();
    a.remove();
    setTimeout(function () { URL.revokeObjectURL(url); }, 4000);
  }
  async function dl(path, body, fallback) {
    if (MOCK) {
      const j = mockApi(path, body);
      const text = typeof j === "string" ? j : JSON.stringify(j, null, 2);
      saveBlob(new Blob([text], { type: "text/plain;charset=utf-8" }), fallback);
      return;
    }
    const headers = { "X-Dualdag-Token": token() };
    const opt = { cache: "no-store", headers: headers };
    if (body !== undefined) {
      opt.method = "POST";
      headers["content-type"] = "application/json";
      opt.body = JSON.stringify(body);
    }
    const r = await fetch(path, opt);
    const ct = r.headers.get("content-type") || "";
    if (!r.ok) {
      let msg = String(r.status);
      try {
        if (ct.indexOf("json") >= 0) { const j = await r.json(); if (j && j.error) msg = j.error; }
        else msg = (await r.text()).slice(0, 200);
      } catch (e) { /* keep status */ }
      throw asHttpErr(r.status, msg, { error: msg });
    }
    const blob = await r.blob();
    let name = fallback;
    const cd = r.headers.get("content-disposition") || "";
    const m = /filename="([^"]+)"/.exec(cd) || /filename=([^;]+)/.exec(cd);
    if (m) name = m[1].replace(/"/g, "").trim();
    saveBlob(blob, name);
  }
  function fileB64(file) {
    return new Promise(function (resolve, reject) {
      const fr = new FileReader();
      fr.onload = function () {
        const s = String(fr.result || "");
        const i = s.indexOf(",");
        resolve(i >= 0 ? s.slice(i + 1) : s);
      };
      fr.onerror = function () { reject(new Error("Could not read the file.")); };
      fr.readAsDataURL(file);
    });
  }

  // Markdown -> HTML. Escape the whole source first, then only add tags we generate.
  function mdInline(s) {
    return s.replace(/`([^`]+)`/g, "<code>$1</code>").replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>");
  }
  function mdBlocks(s) {
    const lines = s.split("\n");
    let html = "", list = null, para = [];
    function closeList() { if (list) { html += "</" + list + ">"; list = null; } }
    function closePara() { if (para.length) { html += "<p>" + para.join("<br>") + "</p>"; para = []; } }
    for (let i = 0; i < lines.length; i++) {
      const line = lines[i].trim();
      let m;
      if (!line) { closeList(); closePara(); continue; }
      if ((m = /^(#{1,6}) (.+)$/.exec(line))) {
        closeList(); closePara();
        html += "<h" + m[1].length + ">" + mdInline(m[2]) + "</h" + m[1].length + ">";
        continue;
      }
      if (/^(-{3,}|\*{3,})$/.test(line)) { closeList(); closePara(); html += "<hr>"; continue; }
      if ((m = /^[-*] (.+)$/.exec(line))) {
        closePara();
        if (list !== "ul") { closeList(); html += "<ul>"; list = "ul"; }
        html += "<li>" + mdInline(m[1]) + "</li>";
        continue;
      }
      if ((m = /^\d+\. (.+)$/.exec(line))) {
        closePara();
        if (list !== "ol") { closeList(); html += "<ol>"; list = "ol"; }
        html += "<li>" + mdInline(m[1]) + "</li>";
        continue;
      }
      closeList();
      para.push(mdInline(line));
    }
    closeList(); closePara();
    return html;
  }
  function mdToHtml(src) {
    const escaped = esc(String(src == null ? "" : src).replace(/\r\n/g, "\n"));
    const parts = escaped.split("```");
    let html = "";
    for (let i = 0; i < parts.length; i++) {
      if (i % 2 === 1) html += '<pre class="mdcode">' + parts[i].replace(/^\n/, "") + "</pre>";
      else html += mdBlocks(parts[i]);
    }
    return html;
  }

  // --- graph layout: longest-path layers, then barycenter sweeps -----------------
  function tarjan(ids, edges) {
    const succ = {};
    ids.forEach(function (id) { succ[id] = []; });
    edges.forEach(function (e) { if (succ[e.from] && succ[e.to]) succ[e.from].push(e.to); });
    let index = 0, ci = 0;
    const stack = [], on = {}, idx = {}, low = {}, comp = {}, sizes = {};
    function strong(v) {
      idx[v] = low[v] = ++index;
      stack.push(v);
      on[v] = true;
      succ[v].forEach(function (w) {
        if (!(w in idx)) { strong(w); low[v] = Math.min(low[v], low[w]); }
        else if (on[w]) low[v] = Math.min(low[v], idx[w]);
      });
      if (low[v] === idx[v]) {
        let w, n = 0;
        do { w = stack.pop(); on[w] = false; comp[w] = ci; n++; } while (w !== v);
        sizes[ci] = n;
        ci++;
      }
    }
    ids.forEach(function (id) { if (!(id in idx)) strong(id); });
    return { comp: comp, sizes: sizes };
  }
  function isCycleEdge(e, info) {
    if (!e || e.from === e.to) return true;
    const c = info.comp[e.from];
    return c != null && c === info.comp[e.to] && info.sizes[c] > 1;
  }
  // Layer = length of the longest path from a source, after cycle edges are removed.
  function longestPathLayers(ids, dagEdges) {
    const indeg = {}, layer = {}, succ = {};
    ids.forEach(function (id) { indeg[id] = 0; layer[id] = 0; succ[id] = []; });
    dagEdges.forEach(function (e) { indeg[e.to]++; succ[e.from].push(e.to); });
    const q = [];
    ids.forEach(function (id) { if (indeg[id] === 0) q.push(id); });
    for (let qi = 0; qi < q.length; qi++) {
      const u = q[qi];
      succ[u].forEach(function (v) {
        if (layer[u] + 1 > layer[v]) layer[v] = layer[u] + 1;
        indeg[v]--;
        if (indeg[v] === 0) q.push(v);
      });
    }
    return layer;
  }
  function alignCycles(ids, layer, info) {
    const maxL = {};
    ids.forEach(function (id) {
      const c = info.comp[id];
      if (info.sizes[c] > 1 && (!(c in maxL) || layer[id] > maxL[c])) maxL[c] = layer[id];
    });
    ids.forEach(function (id) {
      const c = info.comp[id];
      if (info.sizes[c] > 1) layer[id] = maxL[c];
    });
  }
  function clusterOf(id, graph, info) {
    const groups = asRecords(graph && graph.groups);
    for (let i = 0; i < groups.length; i++) {
      if (asArray(groups[i].members).indexOf(id) >= 0) return "g:" + groups[i].id;
    }
    const c = info.comp[id];
    if (info.sizes[c] > 1) return "c:" + c;
    return "main";
  }
  // Barycenter crossing reduction inside one band. Neighbor order is the mean
  // position of adjacent-layer endpoints; sweeps alternate left-to-right.
  function barycenterCols(members, layer, dagEdges) {
    let hi = 0;
    members.forEach(function (id) { if (layer[id] > hi) hi = layer[id]; });
    const cols = [];
    for (let i = 0; i <= hi; i++) cols.push([]);
    members.forEach(function (id) { cols[layer[id]].push(id); });
    const pos = {}, mine = {};
    members.forEach(function (id) { mine[id] = true; });
    function reindex() { cols.forEach(function (col) { col.forEach(function (id, i) { pos[id] = i; }); }); }
    reindex();
    function bary(id, dir) {
      const want = layer[id] + (dir === "left" ? -1 : 1);
      const acc = [];
      dagEdges.forEach(function (e) {
        if (dir === "left" && e.to === id && layer[e.from] === want && mine[e.from]) acc.push(pos[e.from]);
        if (dir === "right" && e.from === id && layer[e.to] === want && mine[e.to]) acc.push(pos[e.to]);
      });
      if (!acc.length) return pos[id];
      let s = 0;
      for (let i = 0; i < acc.length; i++) s += acc[i];
      return s / acc.length;
    }
    for (let sweep = 0; sweep < 4; sweep++) {
      for (let i = 1; i < cols.length; i++) {
        cols[i].sort(function (a, b) { return (bary(a, "left") - bary(b, "left")) || (pos[a] - pos[b]); });
        reindex();
      }
      for (let i = cols.length - 2; i >= 0; i--) {
        cols[i].sort(function (a, b) { return (bary(a, "right") - bary(b, "right")) || (pos[a] - pos[b]); });
        reindex();
      }
    }
    return cols;
  }
  // Spec Key column, in Key order. Missing graph nodes are shown anyway.
  // The lane is display-only: nothing here is written back to graph.json.
  function datasetKeyNodes(graph) {
    if (graph && (graph._viewFilter || graph._paramDag)) return [];
    const ds = (graph && graph.dataset) || ui.dataset || "";
    if (!ds) return [];
    const out = [];
    const seen = {};
    asRows(S && S.spec && S.spec.variables).forEach(function (r) {
      if (!r || String(r.Dataset || "") !== ds) return;
      if (r.Key == null || r.Key === "") return;
      const n = Number(r.Key);
      if (!isFinite(n) || n <= 0) return;
      const variable = String(r.Variable || "");
      if (!variable) return;
      const id = ds + "." + variable;
      if (seen[id]) return;
      seen[id] = true;
      out.push({ id: id, var: variable, order: n });
    });
    const byId = {};
    out.forEach(function (k) { byId[k.id] = k; });
    asRecords(graph && graph.nodes).forEach(function (n) {
      if (!n || !n.id || n.key == null || n.key === "") return;
      const on = n.key === true || n.key === 1 || n.key === "1" || n.key === "true";
      const off = n.key === false || n.key === 0 || n.key === "0" || n.key === "false";
      if (off) delete byId[n.id];
      else if (on && !byId[n.id]) byId[n.id] = { id: n.id, var: n.var || String(n.id).split(".").pop(), order: 100000 };
    });
    const merged = Object.keys(byId).map(function (id) { return byId[id]; });
    merged.sort(function (a, b) { return a.order - b.order || (a.id < b.id ? -1 : 1); });
    return merged;
  }
  function keyNamesForDataset(ds) {
    const byName = {};
    asRows(S && S.spec && S.spec.variables).forEach(function (r) {
      if (!r || String(r.Dataset || "") !== String(ds || "") || r.Key == null || r.Key === "") return;
      const order = Number(r.Key), name = String(r.Variable || "");
      if (name && isFinite(order) && order > 0) byName[name] = order;
    });
    return Object.keys(byName).sort(function (a, b) { return byName[a] - byName[b] || (a < b ? -1 : 1); });
  }
  function displayNode(id, shortForm) {
    const graph = S && S.datasets && S.datasets[ui.dataset] && S.datasets[ui.dataset].graph;
    const node = asRecords(graph && graph.nodes).find(function (n) { return n.id === id; });
    if (!node || node.kind !== "records") return String(id || "");
    const keys = asArray(node.keyLines).map(function (x) { return String(x).split(/[\t ]/)[0]; }).filter(Boolean);
    const names = keys.length ? keys : keyNamesForDataset(ui.dataset);
    const keyText = names.length ? " (" + names.join(", ") + ")" : "";
    if (!graph.layered || !node.paramcd) return "Keys" + keyText;
    return (shortForm ? "Keys" : "Keys · " + node.paramcd) + keyText;
  }
  function keyNameLines(names, maxChars) {
    const lines = [];
    let line = "";
    (names || []).forEach(function (name) {
      const next = line ? line + ", " + name : name;
      if (line && next.length > maxChars) { lines.push(line); line = name; }
      else line = next;
    });
    if (line) lines.push(line);
    return lines.length ? lines : ["Record rows"];
  }
  function recordsNodeForKey(ds, id) {
    if (!ds || !id) return null;
    const fullId = String(id);
    if (fullId.indexOf(String(ds) + ".") !== 0) return null;
    const variable = fullId.split(".").pop().split("@")[0];
    if (keyNamesForDataset(ds).indexOf(variable) < 0) return null;
    const graph = S && S.datasets && S.datasets[ds] && S.datasets[ds].graph;
    if (!graph) return null;
    const records = asRecords(graph.nodes).filter(function (n) { return n.kind === "records"; });
    if (!records.length) return null;
    if (graph.layered === true) {
      const layers = asRecords(graph.layers);
      let selected = ui.layerChoice[ds];
      if (!selected || selected === "__dataset__") selected = layers.length ? String(layers[0].paramcd || "") : "";
      const layer = layers.find(function (l) { return String(l.paramcd || "") === selected; });
      const layerRecordId = layer && layer.records;
      const exact = records.find(function (n) { return n.id === layerRecordId; });
      if (exact) return exact.id;
      const byParam = records.find(function (n) { return String(n.paramcd || "") === selected; });
      return byParam ? byParam.id : null;
    }
    const datasetRecord = records.find(function (n) { return !n.paramcd && n.id === ds + ".RECORDS"; })
      || records.find(function (n) { return !n.paramcd; });
    return datasetRecord ? datasetRecord.id : null;
  }
  function edgePath(a, b, slot, laneOffset) {
    const sameLayer = Math.abs(a.x - b.x) < 2;
    if (sameLayer) {
      // Two cycle edges share a rank: bulge to the right by different amounts.
      const x1 = a.x + a.w, y1 = a.y + a.h / 2;
      const x2 = b.x + b.w, y2 = b.y + b.h / 2;
      const bulge = 26 + (slot % 2) * 22;
      const mx = Math.max(x1, x2) + bulge;
      return "M " + x1 + " " + y1 + " C " + mx + " " + y1 + ", " + mx + " " + y2 + ", " + x2 + " " + y2;
    }
    const x1 = a.x + a.w, y1 = a.y + a.h / 2, x2 = b.x, y2 = b.y + b.h / 2;
    const dx = Math.max(28, (x2 - x1) / 2);
    const lane = Number(laneOffset) || 0;
    return "M " + x1 + " " + y1 + " C " + (x1 + dx) + " " + (y1 + lane) + ", " + (x2 - dx) + " " + (y2 + lane) + ", " + x2 + " " + y2;
  }
  function parameterUses(edge) {
    if (edge && asArray(edge.member_edges).length) {
      return asArray(edge.member_edges).map(function (item) {
        return String(item.member || "") + ": " + asArray(item.uses).map(function (u) {
          if (typeof u === "string") return u;
          return String(u.from_var || "") + " → " + String(u.to_var || "");
        }).filter(Boolean).join(", ");
      }).filter(function (x) { return x !== ": "; });
    }
    return asArray(edge && edge.uses).map(function (u) {
      if (typeof u === "string") return u;
      const from = String(u && u.from_var || "");
      const to = String(u && u.to_var || "");
      if (/^RECORDS$/i.test(from) || /^RECORDS$/i.test(to)) return "rows";
      return from && to ? from + " → " + to : (from || to);
    }).filter(Boolean);
  }
  function keyBusPath(coords, keyIds, nonKeys) {
    if (!keyIds.length || !nonKeys.length) return "";
    let kRight = -Infinity, kTop = Infinity, kBot = -Infinity;
    keyIds.forEach(function (id) {
      const c = coords[id];
      kRight = Math.max(kRight, c.x + c.w);
      kTop = Math.min(kTop, c.y + c.h / 2);
      kBot = Math.max(kBot, c.y + c.h / 2);
    });
    let rLeft = Infinity, rTop = Infinity, rBot = -Infinity;
    nonKeys.forEach(function (id) {
      const c = coords[id];
      rLeft = Math.min(rLeft, c.x);
      rTop = Math.min(rTop, c.y);
      rBot = Math.max(rBot, c.y + c.h);
    });
    const xBus = kRight + 16, yKey = (kTop + kBot) / 2, yMid = (rTop + rBot) / 2;
    return "M " + kRight + " " + kTop + " L " + xBus + " " + kTop + " L " + xBus + " " + kBot
      + " M " + xBus + " " + yKey + " C " + (xBus + 40) + " " + yKey + ", "
      + (rLeft - 36) + " " + yMid + ", " + (rLeft - 2) + " " + yMid;
  }
  function layoutGraph(graph) {
    const rawNodes = asRecords(graph && graph.nodes);
    const graphDs = (graph && graph.dataset) || ui.dataset;
    const keyNames = keyNamesForDataset(graphDs);
    const hasRecords = rawNodes.some(function (n) { return n.kind === "records"; });
    const NW = hasRecords ? 248 : 176, NH = 64, GAPX = 70, GAPY = 28, PAD = 36, BAND = 34;
    const specKeyNames = {};
    if (hasRecords) keyNames.forEach(function (name) { specKeyNames[name] = true; });
    const nodes = rawNodes.filter(function (n) { return !(hasRecords && specKeyNames[n.var || String(n.id || "").split(".").pop().split("@")[0]] && n.kind !== "records"); }).map(function (n) {
      if (n.kind !== "records") return n;
      return Object.assign({}, n, { keyNames: keyNames, keyLines: keyNameLines(keyNames, 24) });
    });
    const keyMeta = graph && (graph._layered || hasRecords) ? [] : datasetKeyNodes(graph);
    const idSet = {};
    nodes.forEach(function (n) { idSet[n.id] = true; });
    keyMeta.forEach(function (k) {
      if (idSet[k.id]) return;
      nodes.push({ id: k.id, var: k.var, paramcd: null, kind: "variable" });
      idSet[k.id] = true;
    });
    const keySet = {};
    keyMeta.forEach(function (k) { if (idSet[k.id]) keySet[k.id] = true; });
    const rawEdges = asRecords(graph && graph.edges);
    const ids = nodes.map(function (n) { return n.id; });
    const edges = rawEdges.filter(function (e) { return idSet[e.from] && idSet[e.to]; });
    if (!ids.length) return { coords: {}, groups: [], edges: [], nodes: [], width: 240, height: 80, layerOf: {}, keyLane: null };
    const info = tarjan(ids, edges);
    const dag = edges.filter(function (e) { return !isCycleEdge(e, info); });
    const layer = longestPathLayers(ids, dag);
    alignCycles(ids, layer, info);
    const bands = [];
    function addBand(k) { if (bands.indexOf(k) < 0) bands.push(k); }
    addBand("main");
    asRecords(graph.groups).forEach(function (g) { addBand("g:" + g.id); });
    // members may arrive as a length-1 string if the server unboxes the vector
    ids.forEach(function (id) { const c = info.comp[id]; if (info.sizes[c] > 1) addBand("c:" + c); });
    const byBand = {};
    ids.forEach(function (id) {
      if (keySet[id]) return;
      const k = clusterOf(id, graph, info);
      if (!byBand[k]) byBand[k] = [];
      byBand[k].push(id);
    });
    const coords = {};
    let y0 = PAD;
    bands.forEach(function (k) {
      const members = byBand[k] || [];
      if (!members.length) return;
      const cols = barycenterCols(members, layer, dag);
      let rows = 1;
      cols.forEach(function (col) { if (col.length > rows) rows = col.length; });
      const maxNodeH = members.reduce(function (max, id) { const n = nodes.find(function (x) { return x.id === id; }); return Math.max(max, n && n.kind === "records" ? 58 + n.keyLines.length * 18 : NH); }, NH);
      const stride = maxNodeH + GAPY;
      cols.forEach(function (col) {
        const extra = (rows - col.length) * stride / 2;
        col.forEach(function (id, ri) {
          const n = nodes.find(function (x) { return x.id === id; });
          const h = n && n.kind === "records" ? 58 + n.keyLines.length * 18 : NH;
          coords[id] = { x: PAD + layer[id] * (NW + GAPX), y: y0 + extra + ri * stride, w: NW, h: h };
        });
      });
      y0 += rows * stride - GAPY + BAND;
    });
    const keyIds = keyMeta.map(function (k) { return k.id; }).filter(function (id) { return keySet[id]; });
    const nonKeys = ids.filter(function (id) { return !keySet[id] && coords[id]; });
    let keyLane = null;
    if (keyIds.length) {
      const colW = NW + GAPX;
      nonKeys.forEach(function (id) { coords[id].x += colW; });
      let top = PAD;
      nonKeys.forEach(function (id) { if (coords[id].y < top) top = coords[id].y; });
      keyIds.forEach(function (id, i) {
        coords[id] = { x: PAD, y: top + i * (NH + GAPY), w: NW, h: NH, key: true };
        layer[id] = 0;
      });
      keyLane = { x: PAD, y: top - 8 };
    }
    const layerLayouts = graph && graph._layerKey && ui.layerLayouts[graph._layerKey];
    const saved = layerLayouts || ((curPack() && curPack().layout) || {});
    Object.keys(saved).forEach(function (id) {
      if (!coords[id] || !saved[id]) return;
      const x = Number(saved[id].x), y = Number(saved[id].y);
      if (isFinite(x) && isFinite(y)) { coords[id].x = x; coords[id].y = y; }
    });
    const groups = [];
    asRecords(graph.groups).forEach(function (g) {
      const ms = asArray(g.members).filter(function (id) { return coords[id] && !keySet[id]; });
      if (!ms.length) return;
      let x0 = Infinity, y1 = Infinity, x1 = -Infinity, y2 = -Infinity;
      ms.forEach(function (id) {
        const c = coords[id];
        x0 = Math.min(x0, c.x); y1 = Math.min(y1, c.y);
        x1 = Math.max(x1, c.x + c.w); y2 = Math.max(y2, c.y + c.h);
      });
      groups.push({ id: g.id, paramcd: g.paramcd || g.id, members: ms.slice(), x: x0 - 12, y: y1 - 28, w: (x1 - x0) + 24, h: (y2 - y1) + 40 });
    });
    let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
    function bound(x, y, w, h) {
      minX = Math.min(minX, x); minY = Math.min(minY, y);
      maxX = Math.max(maxX, x + w); maxY = Math.max(maxY, y + h);
    }
    Object.keys(coords).forEach(function (id) { const c = coords[id]; bound(c.x, c.y, c.w, c.h); });
    groups.forEach(function (g) { bound(g.x, g.y, g.w, g.h); });
    if (keyLane) bound(keyLane.x, keyLane.y - 14, 48, 14);
    const drawn = [];
    const slot = {};
    const incomingLane = {};
    const outgoingLabelCounts = {};
    edges.forEach(function (e) {
      const a = coords[e.from], b = coords[e.to];
      if (!a || !b) return;
      const cyc = isCycleEdge(e, info);
      const key = [e.from, e.to].sort().join("|");
      slot[key] = (slot[key] || 0) + 1;
      const laneIndex = incomingLane[e.to] || 0;
      incomingLane[e.to] = laneIndex + 1;
      const laneCount = edges.filter(function (other) { return other.to === e.to; }).length;
      const laneOffset = laneCount > 1 ? (laneIndex - (laneCount - 1) / 2) * 14 : 0;
      const hasLabel = parameterUses(e).length > 0;
      const labelIndex = hasLabel ? (outgoingLabelCounts[e.from] || 0) : 0;
      if (hasLabel) outgoingLabelCounts[e.from] = labelIndex + 1;
      const labelCount = hasLabel ? edges.filter(function (other) { return other.from === e.from && parameterUses(other).length; }).length : 0;
      drawn.push({
        from: e.from, to: e.to, evidence: e.evidence || "", origin: e.origin || "",
        cycle: cyc, d: edgePath(a, b, slot[key], laneOffset), laneOffset: laneOffset,
        usesTip: graph && graph._paramDag && parameterUses(e).join(", "),
        usesLabel: graph && graph._paramDag && (parameterUses(e).slice(0, 2).join(", ") + (parameterUses(e).length > 2 ? " +" + (parameterUses(e).length - 2) : "")),
        labelOffset: labelCount > 1 ? (labelIndex - (labelCount - 1) / 2) * 14 : 0
      });
    });
    if (keyIds.length && nonKeys.length) {
      drawn.push({
        bundle: true,
        evidence: "Every variable is added to the key rows; keys must be Data matched first",
        d: keyBusPath(coords, keyIds, nonKeys)
      });
    }
    return {
      coords: coords, groups: groups, edges: drawn, nodes: nodes, layerOf: layer, keyLane: keyLane,
      minX: minX - PAD, minY: minY - PAD,
      width: maxX - minX + PAD * 2 + 64,
      height: maxY - minY + PAD * 2 + 24
    };
  }

  // --- canned study (?mock=1): root -> 2 children -> 2 grandchildren, ---------------
  // WEIGHT/HEIGHT/BMI groups, one cycle, one problem node -----------------------------
  const M = MOCK ? buildMock() : null;
  function buildMock() {
    const specRows = {
      "ADVS.SRC": [{ file: "variable.csv", row: 2 }],
      "ADVS.BASE": [{ file: "variable.csv", row: 3 }],
      "ADVS.CHG": [{ file: "variable.csv", row: 4 }],
      "ADVS.PCHG": [{ file: "variable.csv", row: 5 }],
      "ADVS.AVALC": [{ file: "variable.csv", row: 6 }],
      "ADVS.X": [{ file: "variable.csv", row: 7 }],
      "ADVS.Y": [{ file: "variable.csv", row: 8 }],
      "ADVS.AVAL@WEIGHT": [{ file: "variable.csv", row: 9 }, { file: "parameter.csv", row: 2 }],
      "ADVS.BASE@WEIGHT": [{ file: "parameter.csv", row: 2 }, { file: "variable.csv", row: 3 }],
      "ADVS.AVAL@HEIGHT": [{ file: "variable.csv", row: 9 }, { file: "parameter.csv", row: 3 }],
      "ADVS.AVAL@BMI": [{ file: "variable.csv", row: 9 }, { file: "parameter.csv", row: 4 }],
      "ADVS.SEX@HEIGHT": [{ file: "variable.csv", row: 11 }, { file: "parameter.csv", row: 3 }],
      "ADVS.USUBJID": [{ file: "variable.csv", row: 10 }]
    };
    const model = {
      project: "~/dualdag-projects/default",
      spec: {
        hash: "mockhash001",
        datasets: ["ADVS"],
        variables: [
          { row: 2, Dataset: "ADVS", Variable: "SRC", Label: "Source value", Type: "num", Length: 8, Key: null, Source: "DM.RFSTDTC", Derivation: "Copy DM.RFSTDTC" },
          { row: 3, Dataset: "ADVS", Variable: "BASE", Label: "Baseline", Type: "num", Length: 8, Key: null, Source: "ADVS.SRC", Derivation: "BASE = SRC" },
          { row: 4, Dataset: "ADVS", Variable: "CHG", Label: "Change", Type: "num", Length: 8, Key: null, Source: "ADVS.SRC", Derivation: "CHG = AVAL - BASE" },
          { row: 5, Dataset: "ADVS", Variable: "PCHG", Label: "Percent change", Type: "num", Length: 8, Key: null, Source: "ADVS.BASE", Derivation: "PCHG = CHG / BASE * 100" },
          { row: 6, Dataset: "ADVS", Variable: "AVALC", Label: "Analysis value (char)", Type: "text", Length: 20, Key: null, Source: "ADVS.CHG", Derivation: "Character form of CHG" },
          { row: 7, Dataset: "ADVS", Variable: "X", Label: "Cycle X", Type: "num", Length: 8, Key: null, Source: "ADVS.Y", Derivation: "X = Y + 1" },
          { row: 8, Dataset: "ADVS", Variable: "Y", Label: "Cycle Y", Type: "num", Length: 8, Key: null, Source: "ADVS.X", Derivation: "Y = X + 1" },
          { row: 9, Dataset: "ADVS", Variable: "AVAL", Label: "Analysis value", Type: "num", Length: 8, Key: null, Source: "VS.VSSTRESN", Derivation: "VSSTRESN for the PARAMCD" },
          { row: 10, Dataset: "ADVS", Variable: "USUBJID", Label: "Subject", Type: "text", Length: 20, Key: 1, Source: "DM.USUBJID", Derivation: "" }
        ],
        parameters: [
          { row: 2, Dataset: "ADVS", PARAMCD: "WEIGHT", PARAM: "Weight", Source: "VS.VSSTRESN", Derivation: "VSTESTCD = WEIGHT" },
          { row: 3, Dataset: "ADVS", PARAMCD: "HEIGHT", PARAM: "Height", Source: "VS.VSSTRESN", Derivation: "VSTESTCD = HEIGHT" },
          { row: 4, Dataset: "ADVS", PARAMCD: "BMI", PARAM: "Body mass index", Source: "WEIGHT / HEIGHT", Derivation: "BMI = WEIGHT / HEIGHT^2" }
        ]
      },
      data: [
        { name: "dm.xpt", domain: "DM", nrow: 20, ncol: 4, vars: ["USUBJID", "RFSTDTC", "AGE", "ARM"] },
        { name: "vs.xpt", domain: "VS", nrow: 48, ncol: 5, vars: ["USUBJID", "VSTESTCD", "VSSTRESN", "VSSTRESU", "VISIT"] }
      ],
      missing: {},
      catalog_summary: {
        DM: { nrow: 20, nvars: 4, file: "dm.xpt" },
        VS: { nrow: 48, nvars: 5, file: "vs.xpt" }
      },
      catalog: {
        built: "2026-10-29T00:00:00Z",
        domains: {
          DM: {
            file: "dm.xpt", nrow: 20, label: "Demographics",
            variables: [
              { name: "USUBJID", label: "Unique Subject Identifier", type: "character", length: 20 },
              { name: "RFSTDTC", label: "Subject Reference Start Date/Time", type: "character", length: 19 },
              { name: "AGE", label: "Age", type: "numeric", length: 8 },
              { name: "ARM", label: "Description of Planned Arm", type: "character", length: 20 }
            ],
            values: {}
          },
          VS: {
            file: "vs.xpt", nrow: 48, label: "Vital Signs",
            variables: [
              { name: "USUBJID", label: "Unique Subject Identifier", type: "character", length: 20 },
              { name: "VSTESTCD", label: "Vital Signs Test Short Name", type: "character", length: 8 },
              { name: "VSSTRESN", label: "Numeric Result in Standard Units", type: "numeric", length: 8 },
              { name: "VSSTRESU", label: "Standard Units", type: "character", length: 8 },
              { name: "VISIT", label: "Visit Name", type: "character", length: 20 }
            ],
            values: { VSTESTCD: ["SYSBP", "DIABP"], VISIT: ["SCREENING", "WEEK 2"] }
          }
        }
      },
      layout: {},
      graph: {
        dataset: "ADVS", spec_hash: "mockhash001",
        cycle_cuts: [{ from: "ADVS.SRC", to: "ADVS.ABLFL", reason: "The ABLFL rule does not read SRC: \"Set ABLFL to Y on the latest row\" <mock>.", round: 1 }],
        nodes: [
          { id: "ADVS.USUBJID", var: "USUBJID", paramcd: null, kind: "variable" },
          { id: "ADVS.ABLFL", var: "ABLFL", paramcd: null, kind: "variable" },
          { id: "ADVS.SRC", var: "SRC", paramcd: null, kind: "variable" },
          { id: "ADVS.BASE", var: "BASE", paramcd: null, kind: "variable" },
          { id: "ADVS.CHG", var: "CHG", paramcd: null, kind: "variable" },
          { id: "ADVS.AVALC", var: "AVALC", paramcd: null, kind: "variable", categories: [{ value: "Low", rule: "CHG < 0" }, { value: "High", rule: "CHG >= 0" }] },
          { id: "ADVS.PCHG", var: "PCHG", paramcd: null, kind: "variable" },
          { id: "ADVS.AVAL@WEIGHT", var: "AVAL", paramcd: "WEIGHT", kind: "param_variable" },
          { id: "ADVS.BASE@WEIGHT", var: "BASE", paramcd: "WEIGHT", kind: "param_variable" },
          { id: "ADVS.AVAL@HEIGHT", var: "AVAL", paramcd: "HEIGHT", kind: "param_variable" },
          { id: "ADVS.AVAL@BMI", var: "AVAL", paramcd: "BMI", kind: "param_variable" },
          { id: "ADVS.SEX@HEIGHT", var: "SEX", paramcd: "HEIGHT", kind: "param_variable" },
          { id: "ADVS.X", var: "X", paramcd: null, kind: "variable" },
          { id: "ADVS.Y", var: "Y", paramcd: null, kind: "variable" }
        ],
        groups: [
          { id: "ADVS@WEIGHT", paramcd: "WEIGHT", members: ["ADVS.AVAL@WEIGHT", "ADVS.BASE@WEIGHT"] },
          { id: "ADVS@HEIGHT", paramcd: "HEIGHT", members: ["ADVS.AVAL@HEIGHT"] },
          { id: "ADVS@BMI", paramcd: "BMI", members: ["ADVS.AVAL@BMI"] }
        ],
        edges: [
          { from: "ADVS.ABLFL", to: "ADVS.AVAL@WEIGHT", evidence: "Parameter baseline flag", origin: "llm" },
          { from: "ADVS.ABLFL", to: "ADVS.AVAL@HEIGHT", evidence: "Parameter baseline flag", origin: "llm" },
          { from: "ADVS.ABLFL", to: "ADVS.AVAL@BMI", evidence: "Parameter baseline flag", origin: "llm" },
          { from: "ADVS.SRC", to: "ADVS.BASE", evidence: "BASE = SRC", origin: "llm" },
          { from: "ADVS.SRC", to: "ADVS.CHG", evidence: "CHG comes from SRC", origin: "llm" },
          { from: "ADVS.BASE", to: "ADVS.PCHG", evidence: "PCHG = CHG / BASE * 100", origin: "llm" },
          { from: "ADVS.CHG", to: "ADVS.AVALC", evidence: "AVALC is the character form of CHG", origin: "llm" },
          { from: "ADVS.AVAL@WEIGHT", to: "ADVS.BASE@WEIGHT", evidence: "BASE is this parameter's AVAL", origin: "llm" },
          { from: "ADVS.AVAL@WEIGHT", to: "ADVS.AVAL@BMI", evidence: "BMI uses weight", origin: "llm" },
          { from: "ADVS.AVAL@HEIGHT", to: "ADVS.AVAL@BMI", evidence: "BMI uses height", origin: "llm" },
          { from: "ADVS.Y", to: "ADVS.X", evidence: "X = Y + 1", origin: "llm" },
          { from: "ADVS.X", to: "ADVS.Y", evidence: "Y = X + 1", origin: "llm" }
        ],
        external: [
          { ref: "DM.RFSTDTC", domain: "DM", var: "RFSTDTC", used_by: ["ADVS.SRC"], evidence: "SRC is taken from DM.RFSTDTC" },
          { ref: "VS.VSSTRESN", domain: "VS", var: "VSSTRESN", used_by: ["ADVS.AVAL@WEIGHT", "ADVS.AVAL@HEIGHT"], evidence: "AVAL = VSSTRESN" }
        ],
        unresolved: []
      },
      nodes: {},
      dag: { turns: [], corrections: [] },
      dagUndo: [],
      groupStatus: {},
      bad: {},
      issues: [
        { type: "cycle", dataset: "ADVS", variables: ["X", "Y"], file: "variable.csv", row: 7, text: "X = Y + 1", message: "X and Y depend on each other", suggestion: "Fix the cycle in the spec, then re-extract." },
        { type: "orphan", dataset: "ADVS", variables: ["USUBJID"], file: "variable.csv", row: 10, text: "DM.USUBJID", message: "USUBJID has no variable-to-variable edge", suggestion: "It is a key. Other variables are still added to its rows." },
        { type: "missing_value", dataset: "ADVS", variables: ["HEIGHT"], file: "parameter.csv", row: 3, text: "VSTESTCD = HEIGHT", message: "HEIGHT is not in VS.VSTESTCD", suggestion: "Use a test code that is in the data, such as SYSBP or DIABP." }
      ],
      config: { provider: "openai", base_url: "", model: "", strong_model: "", cli_use_login: true, api_key_set: false, api_key_source: "none", api_key_hint: "" },
      qc: {
        ADVS: {
          updated: "2026-10-28T01:02:03Z",
          markdown: "# ADVS investigation\n\n- Align on USUBJID first\n- **Units** must match. Weight is in kilograms.\n\n```\nUSUBJID + PARAMCD\n```\n\nReport differences. Do not change code automatically.\n"
        }
      },
      specRows: specRows
    };
    function put(id, level, extra) {
      model.nodes[id] = {
        level: level, problem: null, same_source: false, updated: "2026-10-28T00:00:00Z",
        sides: { primary: {}, validation: {} }, actions: [], ready: false
      };
      if (extra) Object.keys(extra).forEach(function (k) { model.nodes[id][k] = extra[k]; });
    }
    put("ADVS.USUBJID", "none");
    put("ADVS.ABLFL", "none");
    put("ADVS.SRC", "L3");
    put("ADVS.BASE", "none", { upstream_changed: ["ADVS.SRC"] });
    put("ADVS.CHG", "none");
    put("ADVS.PCHG", "none");
    put("ADVS.AVALC", "L2", { problem: { stage: "compare", message: "1 row differs" }, has_qc_report: true });
    put("ADVS.AVAL@WEIGHT", "L3", { same_source: true });
    put("ADVS.BASE@WEIGHT", "none");
    put("ADVS.AVAL@HEIGHT", "L2");
    put("ADVS.SEX@HEIGHT", "none");
    put("ADVS.AVAL@BMI", "none");
    put("ADVS.X", "none");
    put("ADVS.Y", "none");
    model.runLog = { "ADVS.BASE@WEIGHT": {
      primary: Array.from({ length: 42 }, function (_, i) { return "> step " + (i + 1) + " completed"; }).join("\n")
        + "\n> result <- missing_value\nError: object '<missing_value>' & not found\nExecution halted\n",
      validation: "Warning message:\nIn result <- as.numeric(value) : NAs introduced by coercion\n"
    } };
    model.experts = {
      ADVS_primary: Array.from({ length: 24 }, function (_, i) {
        return { kind: i % 2 ? "rerun" : "generate", node: "ADVS.SRC", summary: "stored run " + i, result: "ok" };
      }).concat([{ kind: "ask", question: "What happened?", answer: "Review the error.", suggestion: "Fix the source." }]),
      ADVS_validation: Array.from({ length: 24 }, function (_, i) {
        return { kind: "generate", node: "ADVS.SRC", summary: "stored run " + i, result: "ok" };
      })
    };
    return model;
  }
  function mockFullRunStatus(dataset) {
    if (!M.fullRun) M.fullRun = {
      primary: { running: false, run: { ok: true, started: "2026-11-01T12:00:00Z", finished: "2026-11-01T12:00:03Z", seconds: 3, variables_run: ["ADVS.SRC", "ADVS.BASE", "ADVS.CHG"], skipped: [], failed_variable: null, error: "", rows: 48, columns: 5, hashes: {} }, log_tail: ["Starting primary run", "48 rows × 5 columns", "Run completed"], readiness: { layers_total: 0, layers_generated: 0, layers_missing: [], variables_total: 8, variables_with_code: 3, variables_without_code: ["ADVS.BASE", "ADVS.CHG", "ADVS.X", "ADVS.Y", "ADVS.PCHG"] } },
      validation: { running: false, run: { ok: false, started: "2026-11-01T12:01:00Z", finished: "2026-11-01T12:01:01Z", seconds: 1.4, variables_run: ["ADVS.SRC"], skipped: [], failed_variable: "ADVS.BASE", error: "object 'SOURCE_MISSING' not found", rows: null, columns: null, hashes: {} }, log_tail: ["Running ADVS.SRC", "Error in ADVS.BASE: object 'SOURCE_MISSING' not found"], readiness: { layers_total: 0, layers_generated: 0, layers_missing: [], variables_total: 8, variables_with_code: 5, variables_without_code: ["ADVS.BASE", "ADVS.CHG", "ADVS.X"] } }
    };
    return { dataset: dataset, sides: { primary: mockClone(M.fullRun.primary || { running: false, run: null, log_tail: [] }), validation: mockClone(M.fullRun.validation || { running: false, run: null, log_tail: [] }) } };
  }
  const fullRunErrorsStorageKey = "dualdag.fullRun.errors";
  function fullRunErrorMap() {
    if (!ui.fullRun.errors || typeof ui.fullRun.errors !== "object") ui.fullRun.errors = {};
    return ui.fullRun.errors;
  }
  function fullRunSideError(dataset, side) {
    const errors = fullRunErrorMap();
    if (!errors[dataset]) {
      try {
        const saved = JSON.parse(localStorage.getItem(fullRunErrorsStorageKey) || "{}")[dataset];
        errors[dataset] = saved && typeof saved === "object" ? saved : {};
      }
      catch (e) { errors[dataset] = {}; }
    }
    return errors[dataset][side] || "";
  }
  function setFullRunSideError(dataset, side, message) {
    const errors = fullRunErrorMap();
    if (!errors[dataset]) fullRunSideError(dataset, side);
    errors[dataset][side] = String(message || "");
    try {
      const saved = JSON.parse(localStorage.getItem(fullRunErrorsStorageKey) || "{}");
      saved[dataset] = errors[dataset];
      localStorage.setItem(fullRunErrorsStorageKey, JSON.stringify(saved));
    } catch (e) { /* storage may be unavailable */ }
  }
  function mockClone(o) { return JSON.parse(JSON.stringify(o)); }
  function mockDagChangeLabel(changes) {
    return changes.map(function (ch) {
      if (ch.op === "add_edge") return "+ edge " + ch.from + " -> " + ch.to;
      if (ch.op === "remove_edge") return "- edge " + ch.from + " -> " + ch.to;
      if (ch.op === "set_key") return (ch.value ? "+ key " : "- key ") + (ch.node || ch.id || "") + " = " + ch.value;
      if (ch.op === "set_categories") return "categories " + (ch.node || ch.id || "");
      if (ch.op === "add_node") return "+ node " + (ch.id || ch.node || "");
      return "Updated the variable graph";
    }).join("; ");
  }
  function mockDagUndoStatus() {
    const stack = asArray(M && M.dagUndo);
    return { count: stack.length, label: stack.length ? String(stack[stack.length - 1].label || "") : "" };
  }
  function mockDagUndo() {
    const stack = asArray(M && M.dagUndo);
    if (!stack.length) throw httpErr(400, "There are no graph edits to undo.");
    const snapshot = stack.pop();
    const oldGraph = M.graph, oldNodes = M.nodes;
    const restored = Object.keys(snapshot.nodes || {}).filter(function (id) {
      return JSON.stringify(oldNodes && oldNodes[id]) !== JSON.stringify(snapshot.nodes[id]);
    });
    const oldEdges = asRecords(oldGraph && oldGraph.edges), restoredEdges = asRecords(snapshot.graph && snapshot.graph.edges);
    const edges = [];
    oldEdges.forEach(function (edge) {
      if (!restoredEdges.some(function (other) { return other.from === edge.from && other.to === edge.to; }))
        edges.push({ from: edge.from, to: edge.to, kind: "remove" });
    });
    restoredEdges.forEach(function (edge) {
      if (!oldEdges.some(function (other) { return other.from === edge.from && other.to === edge.to; }))
        edges.push({ from: edge.from, to: edge.to, kind: "add" });
    });
    M.graph = snapshot.graph;
    M.nodes = snapshot.nodes;
    if (M.dag && M.dag.turns) for (let i = M.dag.turns.length - 1; i >= 0; i--) { const turn = M.dag.turns[i]; if (turn.role === "agent" && asRecords(turn.card).length && !turn.undone) { turn.undone = true; break; } }
    mockRecompute();
    return { ok: true, restored: restored, kept: [], undo_count: stack.length,
      label: snapshot.label, affected: restored, edges: edges };
  }
  function mockDescendants(id) {
    const down = {};
    (M.graph.edges || []).forEach(function (e) {
      down[e.from] = down[e.from] || [];
      down[e.from].push(e.to);
    });
    const seen = {};
    const stack = (down[id] || []).slice();
    while (stack.length) {
      const cur = stack.pop();
      if (!cur || seen[cur] || cur === id) continue;
      seen[cur] = true;
      (down[cur] || []).forEach(function (n) { stack.push(n); });
    }
    return Object.keys(seen);
  }
  function mockFlagDownstream(id) {
    mockDescendants(id).forEach(function (d) {
      const st = M.nodes[d];
      if (!st) return;
      let cur = st.upstream_changed;
      if (cur == null || cur === "") cur = [];
      else if (typeof cur === "string") cur = [cur];
      else cur = cur.slice();
      if (cur.indexOf(id) < 0) cur.push(id);
      st.upstream_changed = cur;
    });
  }
  function mockRecompute() {
    if (!M) return;
    Object.keys(M.nodes).forEach(function (id) {
      const st = M.nodes[id];
      const acts = [];
      acts.push("generate");
      if (st.level === "L2") acts.push("compare");
      if (st.problem && st.problem.stage === "compare") acts.push("investigate");
      st.actions = acts;
      st.ready = st.level !== "L3" && acts.length > 0;
    });
    M.groupStatus = {};
    (M.graph.groups || []).forEach(function (g) {
      let level = "L3", problem = null, ready = false, any = false;
      (g.members || []).forEach(function (id) {
        const st = M.nodes[id];
        if (!st) return;
        any = true;
        if (st.problem && !problem) problem = st.problem;
        const rank = { none: 0, L2: 1, L3: 2 };
        if ((rank[st.level] || 0) < (rank[level] || 0)) level = st.level || "none";
        if (st.ready) ready = true;
      });
      M.groupStatus[g.id] = { level: any ? level : "none", problem: problem, ready: ready };
    });
  }
  function mockDetail(id) {
    const st = M.nodes[id];
    if (!st) throw httpErr(404, "No such node");
    const v = (id.split(".")[1] || id).split("@")[0];
    const primary = [
      "# primary — follow the spec",
      "result <- data.frame(",
      "  USUBJID = src$DM$USUBJID,",
      "  " + v + " = upstream[[" + JSON.stringify(v) + "]],",
      "  stringsAsFactors = FALSE",
      ")"
    ].join("\n");
    const validation = [
      "# validation — derive independently, without the primary code",
      "result <- data.frame(",
      "  USUBJID = src$DM$USUBJID,",
      "  " + v + " = NA_real_,",
      "  stringsAsFactors = FALSE",
      ")"
    ].join("\n");
    let compare = null;
    if (id === "ADVS.AVALC" || M.bad[id]) {
      compare = {
        match: false, n_primary: 3, n_validation: 3, n_diff: 1,
        summary: "1 row differs",
        diffs: [{ USUBJID: "SUBJ-01", primary: "70.1", validation: "154.5" }]
      };
    } else if (st.level === "L3") {
      compare = { match: true, n_primary: 3, n_validation: 3, n_diff: 0, summary: "Exact match", diffs: [] };
    }
    const report = (id === "ADVS.AVALC" || (st.problem && st.problem.stage === "compare"))
      ? "# Investigation\n\n**Possible cause**: validation treated weight as pounds.\n\n- Look at USUBJID = SUBJ-01\n- Spec text: AVAL = VSSTRESN\n\nAsk in plain language to rewrite validation in kilograms. Report only. Do not change code.\n"
      : "";
    const wiped = M.deletedCode && M.deletedCode[id];
    const stored = !wiped && (st.level !== "none" || !!st.problem);
    const extra = (M.runLog && M.runLog[id]) || {};
    const logP = stored ? ("primary finished, exit " + (String(extra.primary || "").includes("Error:") ? "1" : "0") + "\n" + (extra.primary || "")) : "";
    const logV = stored ? ("validation finished, exit 0\n" + (extra.validation || "")) : "";
    return {
      status: mockClone(st), actions: st.actions.slice(),
      code: {
        primary: stored ? ((M.agentCode && M.agentCode[id] && M.agentCode[id].primary) || primary) : "",
        validation: stored ? ((M.agentCode && M.agentCode[id] && M.agentCode[id].validation) || validation) : ""
      },
      log: { primary: logP, validation: logV },
      compare: compare, report: report, spec_rows: M.specRows[id] || [],
      reviews: stored ? {
        primary: { verdict: "clear", reasoning: "The generated program follows the variable specification.", issues: [], retried: false },
        validation: { verdict: "uncertain", reasoning: "Confirm the source column is available for all subjects.", issues: ["One source assumption needs confirmation."], retried: false }
      } : {},
      notes: (M.notes && M.notes[id]) ? {
        primary: (M.notes[id].primary || []).slice(),
        validation: (M.notes[id].validation || []).slice()
      } : { primary: [], validation: [] },
      code_note: codeNoteOf(id),
      experts: {
        primary: (M.experts && M.experts[(M.dataset || "ADVS") + "_primary"]) || [],
        validation: (M.experts && M.experts[(M.dataset || "ADVS") + "_validation"]) || []
      }
    };
  }
  function codeNoteOf(id) {
    const bag = M.notes && M.notes[id];
    function one(side) {
      const rows = bag && bag[side];
      if (!rows || !rows.length) return null;
      const last = rows[rows.length - 1];
      return { index: rows.length, at: last.at || "", text: last.text || "" };
    }
    if (bag) return { primary: one("primary"), validation: one("validation") };
    if (id === "ADVS.SRC") return { primary: { index: 1, at: "12:44", text: "carry SITEID from DM" }, validation: null };
    return { primary: null, validation: null };
  }
  function mockTopo(nodes, edges) {
    const ids = nodes.map(function (n) { return n.id; });
    const incoming = {};
    const outs = {};
    ids.forEach(function (id) { incoming[id] = 0; outs[id] = []; });
    const seen = {};
    (edges || []).forEach(function (e) {
      if (!e || !e.from || !e.to || e.from === e.to) return;
      if (incoming[e.from] == null || incoming[e.to] == null) return;
      const key = e.from + "\t" + e.to;
      if (seen[key]) return;
      seen[key] = true;
      outs[e.from].push(e.to);
      incoming[e.to] += 1;
    });
    const ready = ids.filter(function (id) { return incoming[id] === 0; });
    const out = [];
    while (ready.length) {
      const n = ready.shift();
      out.push(n);
      outs[n].forEach(function (m) {
        incoming[m] -= 1;
        if (incoming[m] === 0) ready.push(m);
      });
    }
    ids.forEach(function (id) { if (out.indexOf(id) < 0) out.push(id); });
    return out;
  }
  function mockNodeCode(id, side) {
    const v = ((id.split(".")[1] || "X").split("@")[0]);
    if (id === "ADVS.AVALC" && side === "validation") {
      return "result <- data.frame(\n  USUBJID = src$VS$USUBJID,\n  AVALC = src$VS$VSSTRESU,\n  stringsAsFactors = FALSE\n)\n";
    }
    if (id === "ADVS.AVALC") {
      return "result <- data.frame(\n  USUBJID = src$VS$USUBJID,\n  AVALC = src$VS$VSSTRESN,\n  stringsAsFactors = FALSE\n)\n";
    }
    return "result <- data.frame(\n  USUBJID = src$DM$USUBJID,\n  " + v + " = src$DM$USUBJID,\n  stringsAsFactors = FALSE\n)\n";
  }
  function mockProgram(dataset) {
    const ds = dataset || "ADVS";
    const order = mockTopo(M.graph.nodes, M.graph.edges);
    function one(side) {
      const nodes = [];
      order.forEach(function (id) {
        const st = M.nodes[id];
        if (!st || (st.level === "none" && !st.problem)) return;
        const code = mockNodeCode(id, side);
        nodes.push({
          id: id,
          var: (id.split(".")[1] || id).split("@")[0],
          status: st.level || "none",
          code: code
        });
      });
      const script = nodes.map(function (n) {
        return "# ==== " + n.id + " (" + n.status + ") ====\n" + String(n.code).replace(/\s+$/, "") + "\n";
      }).join("\n");
      return { nodes: nodes, script: script };
    }
    const primary = one("primary");
    const validation = one("validation");
    const shown = [];
    primary.nodes.concat(validation.nodes).forEach(function (n) {
      if (shown.indexOf(n.id) < 0) shown.push(n.id);
    });
    return {
      dataset: ds,
      order: order.filter(function (id) { return shown.indexOf(id) >= 0; }),
      primary: primary,
      validation: validation
    };
  }
  function mockColMeta(partial) {
    return {
      name: partial.name, label: partial.label || partial.name, role: partial.role,
      group: partial.group, sides: partial.sides, origin: partial.origin == null ? null : partial.origin,
      domain: partial.domain || null, variable: partial.variable || null,
      multi: !!partial.multi, note: partial.note || null, aligned: partial.aligned !== false
    };
  }
  function mockInputs(q) {
    const id = q.node || "";
    const st = M.nodes[id];
    if (!st) throw httpErr(404, "Unknown node.");
    const only = q.only_diff === "1" || q.only_diff === "true" || q.only_diff === "yes";
    let page = parseInt(q.page || "1", 10);
    let size = parseInt(q.size || "100", 10);
    if (!page || page < 1) page = 1;
    if (!size || size < 1) size = 100;
    if (size > 1000) size = 1000;
    const filter = String(q.filter || "").toLowerCase();
    const variable = (id.split(".")[1] || id).split("@")[0];
    const columns = [
      mockColMeta({ name: "USUBJID", role: "key", group: "key", sides: "both" })
    ];
    let rows;
    if (id === "ADVS.AVALC") {
      columns.push(mockColMeta({
        name: "VS.VSSTRESN", role: "input", group: "primary", sides: "primary",
        origin: "VS.VSSTRESN", domain: "VS", variable: "VSSTRESN", multi: true, note: "multiple rows"
      }));
      columns.push(mockColMeta({
        name: "VS.VSSTRESU", role: "input", group: "validation", sides: "validation",
        origin: "VS.VSSTRESU", domain: "VS", variable: "VSSTRESU", multi: true, note: "multiple rows"
      }));
      rows = [
        ["SUBJ-01", "70.1 (n=2)", "lb (n=2)", "70.1", "154.5", true],
        ["SUBJ-02", "80 (n=1)", "kg (n=1)", "80", "80", false],
        ["SUBJ-03", "", null, "", null, M.blankEqualsNA === false]
      ];
    } else {
      const ups = [];
      (M.graph.edges || []).forEach(function (e) {
        if (e.to !== id) return;
        const folder = (e.from.split(".")[1] || e.from);
        ups.push(folder);
      });
      const upCols = { primary: [], validation: [] };
      ups.forEach(function (folder) {
        const origin = "upstream." + folder;
        ["primary", "validation"].forEach(function (side) {
          upCols[side].push(mockColMeta({
            name: side + "_" + origin, role: "input", group: side, sides: side,
            origin: origin, variable: folder.split("@")[0]
          }));
        });
      });
      upCols.primary.forEach(function (c) { columns.push(c); });
      (M.graph.external || []).forEach(function (ext) {
        const used = ext.used_by || [];
        const hit = Array.isArray(used) ? used.indexOf(id) >= 0 : used === id;
        if (!hit) return;
        const origin = ext.ref || ((ext.domain || "") + "." + (ext.var || ""));
        const multi = origin === "VS.VSSTRESN";
        columns.push(mockColMeta({
          name: origin, role: "input", group: "both", sides: "both",
          origin: origin, domain: ext.domain || null, variable: ext.var || null,
          multi: multi, note: multi ? "multiple rows" : null
        }));
      });
      upCols.validation.forEach(function (c) { columns.push(c); });
      const bad = !!M.bad[id] || !!(st.problem && st.problem.stage === "compare");
      const nIn = columns.length - 1;
      function fill(subj, differ) {
        const row = [subj];
        for (let i = 0; i < nIn; i++) {
          const c = columns[i + 1];
          row.push(c.multi ? "120 (n=2)" : "1");
        }
        row.push(differ ? "70.1" : "80");
        row.push(differ ? "154.5" : "80");
        row.push(!!differ);
        return row;
      }
      rows = [fill("SUBJ-01", bad), fill("SUBJ-02", false)];
    }
    columns.push(mockColMeta({
      name: "primary_" + variable, role: "result", group: "result", sides: "primary", variable: variable
    }));
    columns.push(mockColMeta({
      name: "validation_" + variable, role: "result", group: "result", sides: "validation", variable: variable
    }));
    columns.push(mockColMeta({ name: "diff", role: "diff", group: "result", sides: "both" }));
    if (id === "ADVS.AVAL@HEIGHT") {
      const pi = columns.findIndex(function (c) { return c.role === "result" && c.sides === "primary"; });
      if (pi >= 0) {
        columns[pi].aligned = false;
        columns[pi].note = "empty result";
        rows.forEach(function (r) {
          r[pi] = null;
          r[r.length - 1] = true;
        });
      }
    }
    let shown = rows.slice();
    if (only) shown = shown.filter(function (r) { return r[r.length - 1] === true; });
    if (filter) {
      shown = shown.filter(function (r) {
        return r.some(function (cell) {
          return String(cell == null ? "" : cell).toLowerCase().indexOf(filter) >= 0;
        });
      });
    }
    const nrow = shown.length;
    const start = (page - 1) * size;
    shown = start >= nrow ? [] : shown.slice(start, start + size);
    return {
      dataset: q.dataset || "ADVS", node: id, var: variable, keys: ["USUBJID"],
      page: page, size: size, nrow: nrow, only_diff: only,
      fallback: { primary: false, validation: false },
      columns: columns, rows: shown
    };
  }
  function mockPeople(n) {
    const rows = [];
    for (let i = 1; i <= n; i++) {
      const id = "SUBJ-" + ("000" + i).slice(-3);
      const age = i === 5 ? null : (30 + (i % 40));
      const arm = i % 7 === 0 ? null : (i % 2 ? "Drug" : "Placebo");
      const day = ("0" + ((i % 28) + 1)).slice(-2);
      rows.push({
        USUBJID: id, RFSTDTC: "2020-01-" + day, AGE: age,
        ARM: i === 1 ? "" : i === 2 ? null : i === 3 ? " " : arm,
        SRC: age == null ? null : String(age), BASE: age, AVAL: age == null ? null : age + 0.5,
        VSTESTCD: i % 2 ? "SYSBP" : "DIABP", VSSTRESN: age, VISIT: i % 2 ? "SCREENING" : "WEEK 2"
      });
    }
    return rows;
  }
  function mockCol(name, label, type) { return { name: name, label: label, type: type }; }
  function mockTableFor(id) {
    const people = mockPeople(id === "in:vs.xpt" ? 48 : 20);
    const C = {
      USUBJID: mockCol("USUBJID", "Unique Subject Identifier", "character"),
      PARAMCD: mockCol("PARAMCD", "Parameter Code", "character"),
      STUDYID: mockCol("STUDYID", "Study Identifier", "character"),
      RFSTDTC: mockCol("RFSTDTC", "Subject Reference Start Date/Time", "character"),
      AGE: mockCol("AGE", "Age", "numeric"),
      ARM: mockCol("ARM", "Description of Planned Arm", "character"),
      SRC: mockCol("SRC", "Source value", "numeric"),
      BASE: mockCol("BASE", "Baseline", "numeric"),
      AVAL: mockCol("AVAL", "Analysis value", "numeric"),
      VSTESTCD: mockCol("VSTESTCD", "Vital Signs Test Short Name", "character"),
      VSSTRESN: mockCol("VSSTRESN", "Numeric Result in Standard Units", "numeric"),
      VISIT: mockCol("VISIT", "Visit Name", "character")
    };
    let names = ["USUBJID", "SRC"];
    if (id === "in:dm.xpt") names = ["USUBJID", "RFSTDTC", "AGE", "ARM"];
    else if (id === "in:vs.xpt") names = ["USUBJID", "VSTESTCD", "VSSTRESN", "VISIT"];
    else if (id.indexOf("layer:ADVS:") === 0) names = ["USUBJID", "PARAMCD", "STUDYID", "AVAL"];
    else if (id.indexOf("frame:") === 0 || id === "out:advs.xpt") names = ["USUBJID", "SRC", "BASE", "AVAL"];
    else if (id.indexOf("@WEIGHT") >= 0 || id.indexOf("AVAL") >= 0) names = ["USUBJID", "AVAL"];
    else if (id.indexOf("BASE") >= 0) names = ["USUBJID", "BASE"];
    return { columns: names.map(function (n) { return C[n]; }), rows: people };
  }
  function mockDatasets() {
    const rows = [
      { id: "in:dm.xpt", kind: "input", dataset: "DM", file: "dm.xpt", nrow: 20, ncol: 4, updated: "2026-10-29T00:00:00Z" },
      { id: "in:vs.xpt", kind: "input", dataset: "VS", file: "vs.xpt", nrow: 48, ncol: 4, updated: "2026-10-29T00:00:00Z" }
    ];
    Object.keys(M.nodes).forEach(function (id) {
      const st = M.nodes[id];
      if (st.level === "none" && !st.problem) return;
      ["primary", "validation"].forEach(function (side) {
        rows.push({
          id: "res:" + id + ":" + side, kind: "result", dataset: "ADVS", node: id, side: side,
          file: side + "/out.rds", nrow: 20, ncol: 2, updated: st.updated || ""
        });
      });
    });
    ["WEIGHT", "HEIGHT", "BMI"].forEach(function (paramcd) {
      rows.push({ id: "layer:ADVS:" + paramcd + ":" + ui.data.side, kind: "parameter", dataset: "ADVS", paramcd: paramcd,
        side: ui.data.side, nrow: paramcd === "BMI" ? 20 : 40, ncol: 4, updated: "2026-10-29T01:00:00Z" });
    });
    rows.push({ id: "frame:ADSL:" + ui.data.side, kind: "generated", dataset: "ADSL", side: ui.data.side, nrow: 20, ncol: 4, updated: "2026-10-29T01:00:00Z" });
    rows.push({ id: "out:advs.xpt", kind: "output", dataset: "ADVS", file: "advs.xpt", nrow: 20, ncol: 4, updated: "2026-10-29T02:00:00Z" });
    return rows;
  }
  function mockView(q) {
    const built = mockTableFor(q.id || "");
    if (!built.columns.length) throw httpErr(404, "No such dataset");
    let cols = built.columns;
    if (q.cols) {
      const want = {};
      q.cols.split(",").forEach(function (c) { if (c) want[c] = true; });
      const picked = cols.filter(function (c) { return want[c.name]; });
      if (picked.length) cols = picked;
    }
    const filter = String(q.filter || "").toLowerCase();
    let records = built.rows;
    if (filter) {
      records = records.filter(function (r) {
        return cols.some(function (c) {
          const v = r[c.name];
          return v != null && String(v).toLowerCase().indexOf(filter) >= 0;
        });
      });
    }
    const limit = Math.min(1000, Math.max(1, parseInt(q.limit || "100", 10) || 100));
    const offset = Math.max(0, parseInt(q.offset || "0", 10) || 0);
    const slice = records.slice(offset, offset + limit);
    const names = cols.map(function (c) { return c.name; });
    return {
      id: q.id, nrow: records.length, ncol: cols.length, columns: cols,
      rows: slice.map(function (r) { return names.map(function (n) { return r[n] == null ? null : String(r[n]); }); }),
      offset: offset, limit: limit
    };
  }
  function mockState() {
    mockRecompute();
    return {
      project: M.project,
      spec: mockClone(M.spec),
      data: mockClone(M.data),
      missing_data: mockClone(M.missing),
      datasets: {
        ADVS: {
          graph: mockClone(M.graph), layout: mockClone(M.layout),
          check: { issues: mockClone(M.issues), order: null },
          nodes: mockClone(M.nodes),
          groups: mockClone(M.groupStatus),
          dag: mockClone(M.dag || { turns: [], corrections: [] }),
          dag_undo: mockDagUndoStatus(),
        }
      },
      issues: mockClone(M.issues),
      config_public: mockClone(M.config),
      project_settings: { blank_equals_na: M.blankEqualsNA !== false },
      catalog_summary: mockClone(M.catalog_summary),
      busy: mockExtractBusy() || M.busy || false,
      last_error: M.last_error || null,
      batch: mockBatchPublic()
    };
  }
  function mockBatchPublic() {
    const b = M && M.batch;
    if (!b) return null;
    return {
      dataset: b.dataset, total: b.total, done: b.done,
      current: b.current, step: b.step,
      cancelled: !!b.cancelled, finished: !!b.finished,
      results: (b.results || []).map(function (r) {
        return { node: r.node, level: r.level, ok: !!r.ok, message: r.message || "" };
      })
    };
  }
  function mockBatchOrder(ids) {
    const keyed = asRows(M.spec.variables).filter(function (r) {
      return r && r.Key != null && r.Key !== "" && !isNaN(Number(r.Key));
    }).sort(function (a, b) { return Number(a.Key) - Number(b.Key); });
    const keyIds = keyed.map(function (r) { return r.Dataset + "." + r.Variable; });
    const want = {};
    ids.forEach(function (id) { want[id] = true; });
    const keys = keyIds.filter(function (id) { return want[id]; });
    const topo = mockTopo(M.graph.nodes, M.graph.edges);
    const seen = {};
    const out = [];
    function push(id) {
      if (!id || seen[id] || !want[id]) return;
      seen[id] = true;
      out.push(id);
    }
    keys.forEach(push);
    topo.forEach(push);
    ids.forEach(push);
    return out;
  }
  function mockBatchFinish(b) {
    b.finished = true;
    b.current = null;
    b.step = null;
  }
  function mockBatchStep() {
    const b = M.batch;
    if (!b || b.finished) return;
    if (b.cancelled || !b.queue || !b.queue.length) { mockBatchFinish(b); return; }
    const id = b.queue.shift();
    b.current = id;
    b.step = "generate";
    setTimeout(function () {
      if (!M.batch || M.batch !== b || b.finished) return;
      const st = M.nodes[id];
      function record(level, ok, message) {
        b.results.push({ node: id, level: level, ok: ok, message: message });
        b.done = b.results.length;
        b.current = null;
        b.step = null;
        mockRecompute();
        if (b.cancelled) { mockBatchFinish(b); return; }
        mockBatchStep();
      }
      if (!st) { record("none", false, "Unknown node"); return; }
      if (id === "ADVS.X") {
        st.level = "none";
        st.problem = { stage: "run", message: "fake failure" };
        record("none", false, "fake failure");
        return;
      }
      st.level = "L2";
      st.problem = null;
      st.upstream_changed = null;
      mockFlagDownstream(id);
      if (!b.compare) { record("L2", true, "Generated"); return; }
      b.step = "compare";
      b.current = id;
      setTimeout(function () {
        if (!M.batch || M.batch !== b || b.finished) return;
        if (id === "ADVS.AVALC") {
          st.level = "L2";
          st.problem = { stage: "compare", message: "1 row differs" };
          record("L2", true, "1 row differs");
          return;
        }
        st.level = "L3";
        st.problem = null;
        record("L3", true, "Exact match");
      }, 40);
    }, 40);
  }
  function mockQuery(path) {
    const qpos = path.indexOf("?");
    const p = qpos >= 0 ? path.slice(0, qpos) : path;
    const query = {};
    if (qpos >= 0) {
      path.slice(qpos + 1).split("&").forEach(function (pair) {
        if (!pair) return;
        const i = pair.indexOf("=");
        const k = decodeURIComponent(i >= 0 ? pair.slice(0, i) : pair);
        query[k] = decodeURIComponent(i >= 0 ? pair.slice(i + 1) : "");
      });
    }
    return { p: p, query: query };
  }
  function mockExtractBusy() {
    try {
      const raw = sessionStorage.getItem("dualdag.mockExtractWait");
      if (!raw) return null;
      const wait = JSON.parse(raw);
      if (!wait || Number(wait.completeAt) <= Date.now()) {
        sessionStorage.removeItem("dualdag.mockExtractWait");
        return null;
      }
      return [{ kind: "extract", dataset: wait.dataset || "ADVS", elapsed: Math.round((Date.now() - Number(wait.startedAt)) / 1000), model: "mock-model" }];
    } catch (e) { return null; }
  }
  function mockApi(path, body) {
    body = body || {};
    const parsed = mockQuery(path);
    const p = parsed.p, q = parsed.query;
    if (p === "/api/state") return mockState();
    if (p === "/api/catalog") return mockClone(M.catalog);
    if (p === "/api/info") return { r: "4.x (mock)", packages: { igraph: "mock", diffdf: "mock" }, config: mockClone(M.config) };
    if (p === "/api/llm/cli_status") {
      const prov = q.provider || "";
      if (prov === "grok_cli") {
        return { installed: true, path: "/usr/bin/grok", version: "grok 0.1.0", logged_in: null, method: "", hint: "" };
      }
      if (prov === "claude_cli") {
        return {
          installed: true, path: "/usr/bin/claude", version: "2.1.316 (Claude Code)",
          logged_in: true, method: "claude.ai", hint: ""
        };
      }
      if (prov === "codex_cli") {
        return {
          installed: true, path: "/usr/bin/codex", version: "codex-cli 0.117.0",
          logged_in: false, method: "",
          hint: "Not logged in. Run codex login in a terminal, then press Check login."
        };
      }
      throw httpErr(400, "Unknown provider.");
    }
    if (p === "/api/config") {
      ["provider", "base_url", "model", "strong_model"].forEach(function (k) { if (body[k] != null) M.config[k] = body[k]; });
      M.config.cli_use_login = CLI_PROVIDERS[body.provider] ? true : (body.cli_use_login == null ? true : !!body.cli_use_login);
      if (body.api_key) {
        M.config.api_key_set = true;
        M.config.api_key_source = "file";
        const key = String(body.api_key);
        M.config.api_key_hint = "…" + key.slice(-4);
      }
      if (body.clear_api_key) {
        M.config.api_key_set = false;
        M.config.api_key_source = "none";
        M.config.api_key_hint = "";
      }
      return { ok: true, config: mockClone(M.config) };
    }
    if (p === "/api/fullrun/status") return mockFullRunStatus(q.dataset || "ADVS");
    if (p === "/api/fullrun/run") {
      const side = body.side || "primary", ds = body.dataset || "ADVS";
      if (!M.fullRun) M.fullRun = {};
      if (M.fullRun[side] && M.fullRun[side].running) throw httpErr(409, "A full run is already running on this side.");
      M.fullRun[side] = { running: true, run: { ok: false, started: new Date().toISOString(), finished: null, seconds: 0, variables_run: [], skipped: [], failed_variable: null, error: "", rows: null, columns: null, hashes: {} }, log_tail: ["Starting full dataset run on " + side + "…"] };
      setTimeout(function () {
        M.fullRun[side] = side === "primary" ? { running: false, run: { ok: true, started: new Date().toISOString(), finished: new Date().toISOString(), seconds: 3.2, variables_run: ["ADVS.SRC", "ADVS.BASE", "ADVS.CHG"], skipped: [], failed_variable: null, error: "", rows: 48, columns: 5, hashes: {} }, log_tail: ["Full run completed.", "48 rows × 5 columns"] }
          : { running: false, run: { ok: false, started: new Date().toISOString(), finished: new Date().toISOString(), seconds: 1.4, variables_run: ["ADVS.SRC"], skipped: [], failed_variable: "ADVS.BASE", error: "object 'SOURCE_MISSING' not found", rows: null, columns: null, hashes: {} }, log_tail: ["Running ADVS.SRC", "Error in ADVS.BASE: object 'SOURCE_MISSING' not found"] };
      }, 4000);
      return { ok: true, running: true, dataset: ds, side: side };
    }
    if (p === "/api/fullrun/program" || p === "/api/fullrun/log") {
      const side = q.side || "primary";
      if (p.endsWith("/program")) return side === "primary" ? "# Mock full dataset program\nrun_primary()\n" : "# Mock validation program\nrun_validation()\n";
      const item = M.fullRun && M.fullRun[side];
      if (!item) throw httpErr(404, "No full run log is available.");
      return (item.log_tail || []).join("\n") + "\n";
    }
    if (p === "/api/config/test") return { ok: true, model: M.config.model || "mock-model", reply: "pong" };
    if (p === "/api/spec/upload") {
      M.spec.hash = "up" + Date.now();
      M.graph.spec_hash = M.spec.hash;
      mockRecompute();
      return { ok: true, issues: mockClone(M.issues) };
    }
    if (p === "/api/data/upload") {
      const name = body.name || "data.xpt";
      const domain = name.replace(/\.xpt$/i, "").toUpperCase();
      M.data = M.data.filter(function (d) { return d.name !== name; });
      M.data.push({ name: name, domain: domain, nrow: 4, ncol: 2, vars: ["USUBJID"] });
      Object.keys(M.missing).forEach(function (ds) {
        M.missing[ds] = (M.missing[ds] || []).filter(function (dom) { return String(dom).toUpperCase() !== domain; });
      });
      return { ok: true, data: mockClone(M.data) };
    }
    if (p === "/api/data/remove") {
      M.data = M.data.filter(function (d) { return d.name !== body.name; });
      return { ok: true };
    }
    if (p === "/api/graph/extract") {
      const ds = body.dataset || "ADVS";
      const startedAt = Date.now();
      try { sessionStorage.setItem("dualdag.mockExtractWait", JSON.stringify({ dataset: ds, startedAt: startedAt, completeAt: startedAt + 4500 })); } catch (e) { /* unavailable storage */ }
      M.busy = [{ kind: "extract", dataset: ds, elapsed: 0, model: "mock-model" }];
      return new Promise(function (resolve) {
        setTimeout(function () {
          M.busy = false;
          try { sessionStorage.removeItem("dualdag.mockExtractWait"); } catch (e) { /* unavailable storage */ }
          resolve(mockClone(M.graph));
        }, 4500);
      });
    }
    if (p === "/api/graph/layout") {
      M.layout = body.reset ? {} : mockClone(body.positions || {});
      return { positions: mockClone(M.layout) };
    }
    if (p === "/api/node/categories") {
      if (q.node === "ADVS.AVALC") return { categorical: true, overflow: false, rows: [
        { value: "Low", rule: "CHG < 0", primary: 11, validation: 11, in_spec: true, mismatch: false },
        { value: "High", rule: "CHG >= 0", primary: 9, validation: 8, in_spec: true, mismatch: false },
        { value: "Unexpected", rule: "", primary: 1, validation: null, in_spec: false, mismatch: true }
      ] };
      return { categorical: false, overflow: false, rows: [] };
    }
    if (p === "/api/node/lineage") {
      const id = q.node || "", seen = {}, queue = [id], edges = asRecords(M.graph.edges);
      while (queue.length) { const cur = queue.shift(); if (!cur || seen[cur]) continue; seen[cur] = true; edges.forEach(function (e) { if (e.to === cur) queue.push(e.from); }); }
      return { ids: Object.keys(seen) };
    }
    if (p === "/api/graph/download") {
      if (q.what === "dag") {
        return "# DAG ADVS\n\n## Nodes\n\n- ADVS.USUBJID | Subject | key\n  - (no parents)\n\n## User corrections\n\n(none)\n";
      }
      if (q.what && q.what !== "graph") throw httpErr(400, "what must be graph or dag");
      return mockClone(M.graph);
    }
    if (p === "/api/node/detail") return mockDetail(q.node);
    if (p === "/api/node/generate") {
      const st = M.nodes[body.node];
      if (!st) throw httpErr(404, "No such node");
      const side = body.side || "both";
      if (["primary", "validation", "both"].indexOf(side) < 0) throw httpErr(400, "side must be primary, validation, or both.");
      const sides = side === "both" ? ["primary", "validation"] : [side];
      if (body.instruction) {
        M.notes = M.notes || {};
        M.notes[body.node] = M.notes[body.node] || { primary: [], validation: [] };
        sides.forEach(function (s) {
          M.notes[body.node][s].push({ at: new Date().toISOString(), text: String(body.instruction) });
        });
        if (/wrong|error|mistake|incorrect/i.test(body.instruction)) M.bad[body.node] = true;
      }
      if (st.level === "none" || (st.problem && st.problem.stage === "validate")) {
        st.level = "none";
        st.problem = null;
      }
      st.upstream_changed = null;
      st.level = "L2";
      st.problem = null;
      mockFlagDownstream(body.node);
      mockRecompute();
      return mockClone(st);
    }
    if (p === "/api/node/reset") {
      const st = M.nodes[body.node];
      if (!st) throw httpErr(404, "No such node");
      st.level = "none";
      st.problem = null;
      st.same_source = false;
      st.upstream_changed = null;
      if (body.delete_code) {
        M.deletedCode = M.deletedCode || {};
        M.deletedCode[body.node] = true;
      }
      mockFlagDownstream(body.node);
      mockRecompute();
      return mockClone(st);
    }
    if (p === "/api/node/compare") {
      const st = M.nodes[body.node];
      if (!st) throw httpErr(404, "No such node");
      if (st.level !== "L2" && !(st.problem && st.problem.stage === "compare")) throw httpErr(400, "Generate and run first.");
      const bad = body.node === "ADVS.AVALC" || M.bad[body.node];
      if (bad) st.problem = { stage: "compare", message: "Compare mismatch" };
      else { st.level = "L3"; st.problem = null; }
      mockRecompute();
      const detail = mockDetail(body.node);
      return { status: mockClone(st), compare: detail.compare };
    }
    if (p === "/api/node/investigate") {
      return { report: mockDetail(body.node).report || "# Investigation\n\nNo differences to inspect.\n" };
    }
    if (p === "/api/node/run") {
      const id = body.node;
      const st = M.nodes[id];
      if (!st) throw httpErr(404, "No such node");
      const side = body.side || "both";
      if (["primary", "validation", "both"].indexOf(side) < 0) throw httpErr(400, "side must be primary, validation, or both.");
      const detail = mockDetail(id);
      const need = side === "both" ? ["primary", "validation"] : [side];
      for (let i = 0; i < need.length; i++) {
        if (!detail.code[need[i]]) throw httpErr(400, "No saved code.");
      }
      M.runLog = M.runLog || {};
      M.runLog[id] = M.runLog[id] || { primary: "", validation: "" };
      need.forEach(function (s) { M.runLog[id][s] += "Re-run " + s + ", exit 0\n"; });
      st.upstream_changed = null;
      mockFlagDownstream(id);
      mockRecompute();
      return { ok: true, status: mockClone(st) };
    }
    if (p === "/api/datasets") return mockDatasets();
    if (p === "/api/datasets/view") return mockView(q);
    if (p === "/api/qc") {
      return Object.keys(M.qc).map(function (ds) {
        return { dataset: ds, size: (M.qc[ds].markdown || "").length, updated: M.qc[ds].updated };
      });
    }
    if (p === "/api/qc/get") {
      const doc = M.qc[q.dataset];
      if (!doc) throw httpErr(404, "No QC document for this dataset.");
      return { markdown: doc.markdown };
    }
    if (p === "/api/qc/upload") {
      const ds = body.dataset;
      if (!ds) throw httpErr(400, "Dataset name is required.");
      if (M.qc[ds] && !body.replace) throw httpErr(409, "A QC document for this dataset already exists.", { exists: true, error: "Already exists" });
      M.qc[ds] = { markdown: "# " + ds + "\n\n(mock received " + (body.name || "qc.md") + ")\n", updated: new Date().toISOString() };
      return { ok: true };
    }
    if (p === "/api/template") {
      if (q.name === "parameter.csv") return "Dataset,PARAMCD,PARAM,Source,Derivation\n";
      if (q.name === "qc-methodology.md") return "# QC\n\n## Which records to check\n\n## How to classify differences\n";
      return "Dataset,Variable,Label,Type,Length,Key,Source,Derivation\n";
    }
    if (p === "/api/export") {
      const nodes = M.graph.nodes;
      for (let i = 0; i < nodes.length; i++) {
        const st = M.nodes[nodes[i].id];
        if (!st || st.level !== "L3" || st.problem) throw httpErr(400, "Every variable must be Data matched before export.");
      }
      return "mock " + (body.format || "xpt") + " " + (body.dataset || "");
    }
    if (p === "/api/dataset/code") return mockProgram(q.dataset);
    if (p === "/api/node/inputs") return mockInputs(q);
    if (p === "/api/batch") {
      if (M.batch && !M.batch.finished) throw httpErr(409, "A batch is already running.");
      const nodes = Array.isArray(body.nodes) ? body.nodes.map(function (id) { return String(id); }) : [];
      if (!nodes.length) throw httpErr(400, "nodes is required.");
      const known = {};
      M.graph.nodes.forEach(function (n) { known[n.id] = true; });
      const bad = nodes.filter(function (id) { return !known[id]; });
      if (bad.length) throw httpErr(400, "Unknown node: " + bad.join(", "));
      const order = mockBatchOrder(nodes);
      M.batch = {
        dataset: body.dataset || "ADVS", total: order.length, done: 0,
        current: null, step: null, cancelled: false, finished: false,
        results: [], compare: body.compare !== false, queue: order.slice()
      };
      mockBatchStep();
      return mockBatchPublic();
    }
    if (p === "/api/batch/cancel") {
      if (!M.batch || M.batch.finished) throw httpErr(409, "No batch is running.");
      M.batch.cancelled = true;
      return mockBatchPublic();
    }
    if (p === "/api/node/agent") {
      const side = body.side === "validation" ? "validation" : (body.side === "primary" ? "primary" : "");
      if (!side) throw httpErr(400, "side must be primary or validation.");
      if (!body.message || !String(body.message).trim()) throw httpErr(400, "message is required.");
      const key = (body.dataset || "ADVS") + "_" + side;
      M.experts = M.experts || {};
      const changed = /^(change|use|make)\b/i.test(String(body.message).trim());
      const notes = M.notes = M.notes || {};
      notes[body.node] = notes[body.node] || { primary: [], validation: [] };
      if (changed) {
        notes[body.node][side].push({ at: "2026-10-30T00:00:00Z", text: String(body.message) });
        M.agentCode = M.agentCode || {};
        M.agentCode[body.node] = M.agentCode[body.node] || {};
        M.agentCode[body.node][side] = "# note " + notes[body.node][side].length + ": " + String(body.message) + "\n" + mockDetail(body.node).code[side];
        if (M.nodes[body.node]) { M.nodes[body.node].level = "L3"; M.nodes[body.node].problem = null; }
        mockRecompute();
      }
      const turn = {
        at: "2026-10-30T00:00:00Z", node: body.node || "", kind: "agent",
        message: String(body.message), reply: "Mock reply from the " + side + " Coder.",
        code_updated: changed, note_index: changed ? notes[body.node][side].length : null,
        compare_status: changed ? "match" : null
      };
      M.experts[key] = (M.experts[key] || []).concat([turn]);
      const windowKey = (body.dataset || "ADVS") + "|" + (body.node || "") + "|" + side;
      M.coderContexts = M.coderContexts || {};
      M.coderContexts[windowKey] = M.coderContexts[windowKey] || [];
      M.coderContexts[windowKey].push({ task: "chat", system: "Mock generation system for " + side + ".",
        user: "Mock generation context for " + (body.node || "") + "\nMESSAGE_BEGIN\n" + String(body.message) + "\nMESSAGE_END\n",
        at: "2026-10-30T00:00:00Z" });
      M.coderProcesses = M.coderProcesses || {};
      M.coderProcesses[windowKey] = M.coderProcesses[windowKey] || [];
      M.coderProcesses[windowKey].push({ event: "Chat started", at: "2026-10-30T00:00:00Z", detail: null },
        { event: "Chat finished", at: "2026-10-30T00:00:01Z", detail: null });
      return { reply: turn.reply, change: changed ? { instruction: String(body.message) } : null,
        code_updated: changed, note_index: turn.note_index, compare_status: turn.compare_status, model: "mock" };
    }
    if (p === "/api/expert/clear") {
      const side = body.side === "validation" ? "validation" : (body.side === "primary" ? "primary" : "");
      if (!side) throw httpErr(400, "side must be primary or validation.");
      const key = (body.dataset || "ADVS") + "_" + side;
      if (M.experts) delete M.experts[key];
      return { ok: true };
    }
    if (p === "/api/node/llm-log") {
      const side = q.side === "validation" ? "validation" : "primary";
      return {
        time: "2026-10-30T00:00:00Z",
        task: "generate",
        node: q.node || "",
        side: side,
        model: "mock",
        system: "TASK: generate_" + side,
        user: "VAR: " + (q.node || "") + "\nwork has 3 rows and columns: USUBJID",
        reply: "{\"code\":\"result <- work\"}",
        usage: { prompt_tokens: 1, completion_tokens: 1 }
      };
    }
    if (p === "/api/node/coder-window") {
      const side = q.side === "validation" ? "validation" : (q.side === "primary" ? "primary" : "");
      if (!side) throw httpErr(400, "side must be primary or validation.");
      const node = q.node || "ADVS.AVAL";
      const key = (q.dataset || "ADVS") + "|" + node + "|" + side;
      const threadKey = (q.dataset || "ADVS") + "_" + side;
      const contexts = M.coderContexts && M.coderContexts[key] ? mockClone(M.coderContexts[key]) : [
        { task: "generation", system: "TASK: generate_" + side + "\nBuild only this side.",
          user: "VAR: " + node + "\nMock prompt for " + side + ".", at: "2026-10-30T00:00:00Z" }
      ];
      const eligible = contexts.map(function (c, i) { return ["generation", "rewrite", "chat", "lookup round"].indexOf(c.task) >= 0 ? i + 1 : 0; }).filter(Boolean);
      const def = eligible.length ? eligible[eligible.length - 1] : null;
      const requested = Number(q.attempt);
      const idx = requested >= 1 && requested <= contexts.length ? requested : def;
      const allTurns = M.experts && M.experts[threadKey] || [];
      return { dataset: q.dataset || "ADVS", node: node, side: side, running: false,
        context: idx ? contexts[idx - 1] : null, contexts: contexts, context_index: idx, default_context_index: def,
        thread: allTurns.filter(function (turn) { return turn.node === node; }),
        process: (M.coderProcesses && M.coderProcesses[key]) || [{ event: "Model call started", at: "2026-10-30T00:00:01Z", detail: "generation" },
          { event: "Model call finished", at: "2026-10-30T00:00:02Z", detail: "generation" },
          { event: "Final result", at: "2026-10-30T00:00:03Z", detail: "success" }] };
    }
    if (p === "/api/dag/ask") return mockDagAsk(body);
    if (p === "/api/dag/clear") { if (M.dag) M.dag.turns = []; return { ok: true }; }
    if (p === "/api/dag/apply") return mockDagApply(body);
    if (p === "/api/dag/undo") return mockDagUndo(body);
    if (p === "/api/dag/correction/remove") return mockDagRemoveCorrection(body);
    if (p === "/api/param-graph") return mockClone(M.paramGraph || { dataset: q.dataset || "ADVS", nodes: [], edges: [], corrections: [], turns: [] });
    if (p === "/api/param-graph/extract") { M.paramGraph = mockParamGraph(); return mockClone(M.paramGraph); }
    if (p === "/api/param-graph/ask") { M.paramTurns = M.paramTurns || []; M.paramTurns.push({ role: "user", text: body.message }); M.paramTurns.push({ role: "agent", text: "I propose adding the dependency.", changes: [{ op: "add_edge", from: "WEIGHT", to: "BMI" }] }); return { answer: "I propose adding the dependency.", changes: [{ op: "add_edge", from: "WEIGHT", to: "BMI" }], model: "mock", turns: mockClone(M.paramTurns) }; }
    if (p === "/api/param-graph/apply") { M.paramGraph = M.paramGraph || mockParamGraph(); asRecords(body.changes).forEach(function (c) { if (c.op === "add_edge") M.paramGraph.edges.push({ from: c.from, to: c.to, evidence: "Applied by user", origin: "user" }); else if (c.op === "remove_edge") M.paramGraph.edges = M.paramGraph.edges.filter(function (e) { return !(e.from === c.from && e.to === c.to); }); else if (c.op === "add_undefined") M.paramGraph.nodes.push({ code: c.code, label: c.code, defined: false, mentioned_in: [] }); else if (c.op === "remove_undefined") M.paramGraph.nodes = M.paramGraph.nodes.filter(function (n) { return n.code !== c.code; }); }); return { ok: true, graph: mockClone(M.paramGraph) }; }
    throw httpErr(404, "mock has no handler for " + p);
  }
  function mockParamGraph() {
    return { dataset: "ADVS", spec_hash: "mockhash001", error: null, warnings: [], stale: false,
      nodes: [
        { code: "WEIGHT", label: "Weight", defined: true, mentioned_in: ["parameter.csv:2"] },
        { code: "HEIGHT", label: "Height", defined: true, mentioned_in: ["parameter.csv:3"] },
        { code: "BMI", label: "Body mass index", defined: true, mentioned_in: ["parameter.csv:4"] },
        { code: "WTLS15DT", label: "", defined: false, mentioned_in: ["parameter.csv:5"] }
      ], edges: [
        { from: "WEIGHT", to: "BMI", evidence: "BMI = WEIGHT / HEIGHT^2", origin: "llm" },
        { from: "HEIGHT", to: "BMI", evidence: "BMI = WEIGHT / HEIGHT^2", origin: "llm" },
        { from: "WEIGHT", to: "WTLS15DT", evidence: "Source mentions WTLS15DT", origin: "llm" }
      ], corrections: [], turns: mockClone(M && M.paramTurns || []) };
  }
  function mockDagKnown(id) {
    return (M.graph.nodes || []).some(function (n) { return n.id === id; });
  }
  function mockDownstreamReaches(start, goal) {
    const seen = {};
    const stack = [];
    (M.graph.edges || []).forEach(function (e) { if (e.from === start) stack.push(e.to); });
    while (stack.length) {
      const id = stack.pop();
      if (!id || seen[id]) continue;
      if (id === goal) return true;
      seen[id] = true;
      (M.graph.edges || []).forEach(function (e) { if (e.from === id) stack.push(e.to); });
    }
    return false;
  }
  function mockDagAsk(body) {
    const message = String((body && body.message) || "").trim();
    if (!message) throw httpErr(400, "message is required.");
    if (!M.graph) throw httpErr(404, "No dependency graph yet.");
    M.dag = M.dag || { turns: [], corrections: [] };
    const at = new Date().toISOString();
    let answer = "The graph is unchanged.";
    let changes = [];
    let correction = null;
    const add = message.match(/add edge\s+([A-Za-z0-9_.@]+)\s*(?:->|→)\s*([A-Za-z0-9_.@]+)/i);
    const rem = message.match(/remove edge\s+([A-Za-z0-9_.@]+)\s*(?:->|→)\s*([A-Za-z0-9_.@]+)/i);
    if (add) {
      answer = "Proposed a graph change.";
      changes = [{ op: "add_edge", from: add[1], to: add[2], evidence: "User asked to add this edge" }];
    } else if (rem) {
      answer = "Proposed a graph change.";
      changes = [{ op: "remove_edge", from: rem[1], to: rem[2] }];
    }
    const remembered = message.match(/REMEMBER:\s*(.+)/i);
    if (remembered) correction = String(remembered[1] || "").trim();
    M.dag.turns.push({ at: at, role: "user", text: message });
    const card = changes.map(function (change) { return { text: change.op === "add_edge" ? "Added edge " + change.from + " → " + change.to : "Removed edge " + change.from + " → " + change.to }; });
    M.dag.turns.push({ at: at, role: "agent", text: changes.length ? "Done." : answer, changes: changes, card: card, correction: correction, applied: !changes.length });
    if (changes.length) mockDagApply({ changes: changes });
    if (correction) M.dag.corrections.push({ at: at, text: correction });
    return { answer: changes.length ? "Done." : answer, changes: changes, card: card, correction: correction, model: "mock", turns: M.dag.turns.slice(-10) };
  }
  function mockDagApply(body) {
    const changes = asRecords(body && body.changes);
    if (!changes.length) throw httpErr(400, "No changes to apply.");
    const edgesOut = [];
    const affected = [];
    const graphBefore = mockClone(M.graph), nodesBefore = mockClone(M.nodes);
    try { changes.forEach(function (ch) {
      const op = String((ch && ch.op) || "").toLowerCase();
      if (!op) throw httpErr(400, "A graph change is missing its op.");
      if (op === "add_edge" || op === "remove_edge") {
        const from = String(ch.from || "");
        const to = String(ch.to || "");
        if (!mockDagKnown(from)) throw httpErr(400, "Unknown node: " + (from || ".") + ".");
        if (!mockDagKnown(to)) throw httpErr(400, "Unknown node: " + (to || ".") + ".");
        if (from === to) throw httpErr(400, "An edge cannot connect a node to itself (" + from + ").");
        if (op === "add_edge") {
          if (mockDownstreamReaches(to, from)) throw httpErr(400, "That edge would create a cycle: " + from + " -> " + to + ".");
          const exists = (M.graph.edges || []).some(function (e) { return e.from === from && e.to === to; });
          if (!exists) {
            const added = {
              from: from, to: to, evidence: ch.evidence || "Added by the DAG agent", origin: "dag_agent"
            };
            M.graph.edges[M.graph.edges.length] = added;
          }
          edgesOut.push({ from: from, to: to, kind: "add" });
        } else {
          const idx = (M.graph.edges || []).findIndex(function (e) { return e.from === from && e.to === to; });
          if (idx < 0) throw httpErr(400, "No edge from " + from + " to " + to + ".");
          M.graph.edges.splice(idx, 1);
          edgesOut.push({ from: from, to: to, kind: "remove" });
        }
        const st = M.nodes[to];
        if (st) {
          let cur = st.upstream_changed;
          if (cur == null || cur === "") cur = [];
          else if (typeof cur === "string") cur = [cur];
          else cur = cur.slice();
          if (cur.indexOf(from) < 0) cur.push(from);
          st.upstream_changed = cur;
        }
        if (affected.indexOf(to) < 0) affected.push(to);
      } else if (op === "set_key") {
        const id = String(ch.node || ch.id || "");
        if (!mockDagKnown(id)) throw httpErr(400, "Unknown node: " + (id || ".") + ".");
        const on = ch.value === true || ch.value === 1 || ch.value === "1" || ch.value === "true";
        const off = ch.value === false || ch.value === 0 || ch.value === "0" || ch.value === "false";
        if (!on && !off) throw httpErr(400, "set_key value must be true or false.");
        M.graph.nodes.forEach(function (n) { if (n.id === id) n.key = on; });
        const st = M.nodes[id];
        if (st) {
          let cur = st.upstream_changed;
          if (cur == null || cur === "") cur = [];
          else if (typeof cur === "string") cur = [cur];
          else cur = cur.slice();
          if (cur.indexOf(id) < 0) cur.push(id);
          st.upstream_changed = cur;
        }
        if (affected.indexOf(id) < 0) affected.push(id);
      } else if (op === "set_categories") {
        const id = String(ch.node || ch.id || "");
        if (!mockDagKnown(id)) throw httpErr(400, "Unknown node: " + (id || ".") + ".");
        M.graph.nodes.forEach(function (n) { if (n.id === id) { n.categories = asArray(ch.categories); n.categorical = !!n.categories.length; } });
      } else if (op === "add_node") {
        const id = String(ch.id || ch.node || "");
        if (!id || id.indexOf(".") < 0) throw httpErr(400, "Unknown node: " + (id || ".") + ".");
        if (mockDagKnown(id)) throw httpErr(400, "Node " + id + " already exists.");
        const parts = id.split(".");
        M.graph.nodes.push({ id: id, var: parts[1].split("@")[0], paramcd: null, kind: "variable", label: ch.label || "" });
        M.nodes[id] = { level: "none", problem: null, same_source: false, updated: "", sides: {}, actions: [], ready: false };
      } else {
        throw httpErr(400, "Unknown graph change: " + op + ".");
      }
    }); } catch (err) { M.graph = graphBefore; M.nodes = nodesBefore; throw err; }
    M.dagUndo = M.dagUndo || [];
    M.dagUndo.push({ graph: graphBefore, nodes: nodesBefore, label: mockDagChangeLabel(changes) });
    if (M.dag && M.dag.turns && M.dag.turns.length) {
      const last = M.dag.turns[M.dag.turns.length - 1];
      if (last && last.role === "agent") last.applied = true;
    }
    return { ok: true, affected: affected, edges: edgesOut };
  }
  function mockDagRemoveCorrection(body) {
    const at = String((body && body.at) || "");
    M.dag = M.dag || { turns: [], corrections: [] };
    M.dag.corrections = (M.dag.corrections || []).filter(function (c) { return String(c.at || "") !== at; });
    return { ok: true, corrections: M.dag.corrections.slice() };
  }

  // --- view model -----------------------------------------------------------------
  function projectText() {
    const p = S && S.project;
    if (p == null) return "";
    if (typeof p === "string") return p;
    return p.path || p.dir || p.name || "";
  }
  function dsNames() {
    const names = [];
    asArray(S && S.spec && S.spec.datasets).forEach(function (d) { if (names.indexOf(d) < 0) names.push(d); });
    Object.keys((S && S.datasets) || {}).forEach(function (d) { if (names.indexOf(d) < 0) names.push(d); });
    return names;
  }
  function ensureDataset() {
    const names = dsNames();
    if (!ui.dataset || names.indexOf(ui.dataset) < 0) {
      ui.dataset = names.length ? names[0] : null;
      ui.didFit = false;
      ui.userView = false;
      ui.node = null;
      ui.detail = null;
    }
  }
  function curPack() {
    if (!S || !S.datasets || !ui.dataset) return null;
    return S.datasets[ui.dataset] || null;
  }
  function queueFor(ds) { return asArray(ui.runQueues[ds]); }
  function saveRunQueues() {
    try { localStorage.setItem("dualdag.runQueue", JSON.stringify(ui.runQueues || {})); } catch (err) { /* Queue remains usable for this session. */ }
  }
  try { ui.runQueues = JSON.parse(localStorage.getItem("dualdag.runQueue") || "{}") || {}; } catch (err) { ui.runQueues = {}; }
  try { ui.runQueueHiddenResults = JSON.parse(localStorage.getItem("dualdag.runQueueHiddenResults") || "{}") || {}; } catch (err) { ui.runQueueHiddenResults = {}; }
  function hideQueueResult(ds, id) {
    if (!ui.runQueueHiddenResults[ds]) ui.runQueueHiddenResults[ds] = {};
    ui.runQueueHiddenResults[ds][id] = true;
    try { localStorage.setItem("dualdag.runQueueHiddenResults", JSON.stringify(ui.runQueueHiddenResults)); } catch (err) { /* Session state still applies. */ }
  }
  function clearQueueResultHides(ds) {
    ui.runQueueHiddenResults[ds] = {};
    try { localStorage.setItem("dualdag.runQueueHiddenResults", JSON.stringify(ui.runQueueHiddenResults)); } catch (err) { /* Session state still applies. */ }
  }
  function reconcileRunQueues(state) {
    if (!state || !state.datasets) return;
    let changed = false;
    Object.keys(ui.runQueues || {}).forEach(function (ds) {
      const ids = new Set(asRecords(state.datasets[ds] && state.datasets[ds].graph && state.datasets[ds].graph.nodes).map(function (n) { return n.id; }));
      const next = queueFor(ds).filter(function (id) { return ids.has(id); });
      if (next.length !== queueFor(ds).length) { ui.runQueues[ds] = next; changed = true; }
    });
    if (changed) saveRunQueues();
  }
  function queueNodeLabel(ds, id) {
    const graph = S && S.datasets && S.datasets[ds] && S.datasets[ds].graph;
    const n = asRecords(graph && graph.nodes).find(function (x) { return x.id === id; });
    if (!n) return id;
    const code = n.kind === "records" ? "Keys" : (n.var || String(id).split(".").pop());
    const layer = n.paramcd || "";
    return layer ? code + " · " + layer : code;
  }
  function queueResult(ds, id) {
    if (ui.runQueueHiddenResults[ds] && ui.runQueueHiddenResults[ds][id]) return null;
    const b = S && S.batch;
    if (!b || b.dataset !== ds) return null;
    return batchRows(b).find(function (r) { return r.node === id; }) || null;
  }
  function runQueueHtml() {
    if (!ui.dataset || parameterMode()) return "";
    const ids = queueFor(ui.dataset), b = S && S.batch, running = batchRunning();
    const current = running && b.dataset === ui.dataset ? b.current : null;
    const rows = ids.map(function (id, i) {
      const label = queueNodeLabel(ui.dataset, id), result = queueResult(ui.dataset, id);
      const status = result ? (result.compared && result.compare_ok && result.level !== "L3" ? "Mismatch" : !result.ok ? "Failed" : result.level === "L3" ? "Data matched" : result.level === "L2" ? "Generated" : "Skipped")
        : (b && b.dataset === ui.dataset && b.finished && b.cancelled && i >= batchRows(b).length ? "Stopped" : "");
      let upstream = "";
      const graph = curGraph(), edges = asRecords(graph && graph.edges);
      const parents = edges.filter(function (e) { return e.to === id; }).map(function (e) { return e.from; });
      const later = parents.find(function (p) { return ids.indexOf(p) > i; });
      const unfinished = parents.find(function (p) { const st = curPack() && curPack().nodes && curPack().nodes[p]; return !st || st.level !== "L3"; });
      if (later) upstream = "Upstream " + queueNodeLabel(ui.dataset, later) + " runs later.";
      else if (unfinished) upstream = "Upstream " + queueNodeLabel(ui.dataset, unfinished) + " is not finished.";
      return '<div class="run-queue-row' + (id === ui.runQueueDragging ? " dragging" : "") + (running ? " locked" : "") + '" data-queue-node="' + esc(id) + '" title="' + esc(result && result.message || "") + '">'
        + '<span class="run-queue-number">' + (i + 1) + '</span><button class="run-queue-label" data-act="queue-open" data-node="' + esc(id) + '">' + (id === current ? '<span class="spin"></span>' : "") + esc(label) + '</button>'
        + (status ? '<span class="run-queue-status" title="' + esc(result && result.message || "") + '">' + esc(status + (result && result.message && (!result.ok || status === "Skipped") ? ": " + result.message : "")) + '</span>' : "")
        + (upstream && !running ? '<small class="run-queue-upstream">' + esc(upstream) + '</small>' : "")
        + (id === ui.runQueueDragging && ui.runQueueDragOutside ? '<small class="run-queue-drag-hint">Release to remove</small>' : "")
        + '<button class="run-queue-remove" data-act="queue-remove" data-node="' + esc(id) + '" aria-label="Remove ' + esc(label) + '"' + (running ? " disabled" : "") + '>×</button></div>';
    }).join("");
    const otherBatch = running && b.dataset !== ui.dataset;
    return '<aside class="run-queue' + (ui.runQueueOver ? " drop-over" : "") + '" id="run-queue"><h3>Run queue</h3><div class="run-queue-items">' + (rows || '<p class="muted">Drag variables here.</p>') + '</div>'
      + '<div class="run-queue-actions"><button data-act="queue-clear"' + (!ids.length || running ? " disabled" : "") + '>Clear</button><button class="pri" data-act="queue-run"' + (otherBatch || (!ids.length && !running) ? " disabled" : "") + '>' + (running ? (otherBatch ? "Batch running" : "Stop") : "Run all (" + ids.length + ")") + '</button></div></aside>';
  }
  function curGraph() {
    const p = curPack();
    return p && p.graph ? p.graph : null;
  }
  function parameterMode() {
    return !!(ui.dataset && ui.graphMode[ui.dataset] === "parameter"
      && asRows(S && S.spec && S.spec.parameters).some(function (r) { return String(r.Dataset || "") === String(ui.dataset || ""); }));
  }
  function curParamGraph() { return ui.paramGraphs[ui.dataset] || null; }
  function graphSwitchHtml(hasParams) {
    const variableOn = !parameterMode();
    return '<div class="graph-tabs" role="tablist" aria-label="Graph view">'
      + '<button type="button" role="tab" data-act="graph-mode" data-mode="variable" aria-selected="' + (variableOn ? "true" : "false") + '" class="' + (variableOn ? "on" : "") + '">Variable DAG</button>'
      + (hasParams ? '<button type="button" role="tab" data-act="graph-mode" data-mode="parameter" aria-selected="' + (!variableOn ? "true" : "false") + '" class="' + (!variableOn ? "on" : "") + '">Parameter DAG</button>' : "")
      + (hasVariableViewFilter() ? btn("clear-out", "Clear out", { cls: "sm graph-clear" }) : "") + "</div>";
  }
  function normalizeParamGraph(pg) {
    if (!pg) return null;
    const source = asRecords(pg.nodes);
    const carry = carryOverParameters(curGraph());
    const group = carry.length > 0;
    const identities = {};
    asRecords(curGraph() && curGraph().layers).forEach(function (layer) {
      const identity = String(layer.paramcd || "").toUpperCase();
      asArray(layer.members_parameters).forEach(function (member) {
        member = String(member || "").toUpperCase();
        if (identity && member && identity !== "CARRY_OVER" && identity !== member) identities[member] = identity;
      });
    });
    const displayCode = function (code) { return identities[String(code || "").toUpperCase()] || code; };
    const nodes = source.filter(function (n) { return !group || carry.indexOf(String(n.code || "").toUpperCase()) < 0; })
      .map(function (n) { const identity = displayCode(n.code); return { id: identity, code: identity, label: n.label || "", var: identity, paramcd: null, kind: "parameter", defined: n.defined !== false, mentioned_in: n.mentioned_in || [] }; });
    if (group) nodes.unshift({ id: "CARRY_OVER", code: "CARRY_OVER", label: "Carry-over (" + carry.join(", ") + ")", var: "CARRY_OVER", kind: "parameter", defined: true, classification: "carry_over", members: carry });
    const edges = [];
    asRecords(pg.edges).forEach(function (e) {
      const fromCode = String(e.from || "").toUpperCase(), toCode = String(e.to || "").toUpperCase();
      const fromCarry = carry.indexOf(fromCode) >= 0, toCarry = carry.indexOf(toCode) >= 0;
      const edge = Object.assign({}, e, { from: fromCarry ? "CARRY_OVER" : displayCode(e.from), to: toCarry ? "CARRY_OVER" : displayCode(e.to) });
      if (fromCarry && !toCarry) edge.member = fromCode;
      if (toCarry && !fromCarry) edge.member = toCode;
      if (fromCarry || toCarry) edge.member_edges = [{ member: fromCarry ? fromCode : toCode, uses: e.uses || [] }];
      const at = edges.findIndex(function (x) { return x.from === edge.from && x.to === edge.to; });
      if (at < 0) edges.push(edge);
      else edges[at].member_edges = asArray(edges[at].member_edges).concat(asArray(edge.member_edges));
    });
    return { nodes: nodes, edges: edges, groups: [], dataset: ui.dataset, _paramDag: true };
  }
  function cycleCutsHtml(graph) {
    const cuts = asRecords(graph && graph.cycle_cuts);
    if (!cuts.length) return "";
    return '<div class="cycle-cuts"><b>Edges cut to break cycles</b><ul>' + cuts.map(function (cut) {
      return "<li>" + esc(displayNode(cut.from)) + " \u2192 " + esc(displayNode(cut.to)) + (cut.reason ? ": " + esc(cut.reason) : "") + "</li>";
    }).join("") + "</ul></div>";
  }
  function graphForView() {
    const rawGraph = curGraph();
    const review = ui.dagReview;
    if (review && review.dataset === ui.dataset && review.ids && rawGraph) {
      const wanted = {}; review.ids.forEach(function (id) { wanted[id] = true; });
      const roleById = {};
      asArray(review.roles).forEach(function (item) { if (item && item.id) roleById[item.id] = item; });
      const nodes = asRecords(rawGraph.nodes), edges = asRecords(rawGraph.edges);
      const sourceIds = {};
      edges.forEach(function (e) { if (wanted[e.to]) { const n = nodes.find(function (x) { return x.id === e.from; }); if (n && (n.kind === "dataset_variable" || n.kind === "source")) sourceIds[e.from] = true; } });
      const keep = Object.assign({}, wanted, sourceIds);
      return Object.assign({}, rawGraph, { nodes: nodes.filter(function (n) { return !!keep[n.id]; }).map(function (n) {
          const role = roleById[n.id];
          return role ? Object.assign({}, n, { lineageRole: role.role, lineageContext: role.context || {} }) : n;
        }),
        edges: edges.filter(function (e) { return !!keep[e.from] && !!keep[e.to]; }).concat(asRecords(review.edges)), groups: [], _viewFilter: "review:" + review.node, _review: true });
    }
    const graph = layeredGraphView(rawGraph);
    const paramcd = ui.dataset && ui.paramFilter[ui.dataset];
    if (!graph || graph.layered || !paramcd) return graph;
    const nodes = asRecords(graph.nodes), groups = asRecords(graph.groups), edges = asRecords(graph.edges);
    const groupMembers = {}, nodeById = {}, groupParamcd = {};
    nodes.forEach(function (n) { nodeById[n.id] = n; });
    groups.forEach(function (g) { groupMembers[g.id] = asArray(g.members); });
    groups.forEach(function (g) { groupParamcd[g.id] = g.paramcd; });
    function expand(id) { return groupMembers[id] || [id]; }
    function allowed(id) {
      if (!paramcd) return true;
      const groupCode = groupParamcd[id];
      const node = nodeById[id];
      const code = groupCode != null && groupCode !== "" ? groupCode : (node && node.paramcd);
      return code == null || code === "" || String(code) === String(paramcd);
    }
    const inbound = {}, outbound = {};
    edges.forEach(function (e) {
      expand(e.from).forEach(function (a) { expand(e.to).forEach(function (b) {
        if (!allowed(a) || !allowed(b)) return;
        if (!inbound[b]) inbound[b] = {}; inbound[b][a] = true;
        if (!outbound[a]) outbound[a] = {}; outbound[a][b] = true;
      }); });
    });
    function reach(seed) {
      const keep = {};
      function walk(adj) {
        const todo = Object.keys(seed).filter(allowed), seen = {};
        while (todo.length) { const id = todo.pop(); if (seen[id]) continue; seen[id] = true; keep[id] = true;
          Object.keys(adj[id] || {}).forEach(function (next) { if (!seen[next]) todo.push(next); });
        }
      }
      walk(inbound); walk(outbound);
      return keep;
    }
    let paramKeep = null;
    if (paramcd) {
      const starts = {};
      nodes.forEach(function (n) { if (String(n.paramcd || "") === paramcd) starts[n.id] = true; });
      groups.forEach(function (g) { if (String(g.paramcd || "") === paramcd) expand(g.id).forEach(function (id) { starts[id] = true; }); });
      paramKeep = reach(starts);
      if (!Object.keys(starts).length) paramKeep = {};
    }
    const keep = {};
    nodes.forEach(function (n) {
      if (!paramKeep || paramKeep[n.id]) keep[n.id] = true;
    });
    const keptNodes = nodes.filter(function (n) { return !!keep[n.id]; });
    const keptGroups = groups.map(function (g) { return Object.assign({}, g, { members: asArray(g.members).filter(function (id) { return !!keep[id]; }) }); })
      .filter(function (g) { return g.members.length > 0; });
    const keptEdges = [];
    edges.forEach(function (e) {
      expand(e.from).forEach(function (from) { expand(e.to).forEach(function (to) {
        if (keep[from] && keep[to]) keptEdges.push(Object.assign({}, e, { from: from, to: to }));
      }); });
    });
    if (paramcd) ui.sel = (ui.sel || []).filter(function (id) { return !!keep[id]; });
    return Object.assign({}, graph, {
      nodes: keptNodes, groups: keptGroups, edges: keptEdges,
      _viewFilter: paramcd
    });
  }
  function layeredGraphView(graph) {
    if (!graph || graph.layered !== true) return graph;
    const layers = asRecords(graph.layers);
    if (!layers.length) return Object.assign({}, graph, { nodes: [], edges: [], groups: [], _layered: true });
    let choice = ui.dataset && ui.layerChoice[ui.dataset];
    if (!choice || !layers.some(function (l) { return String(l.paramcd) === choice; })) {
      choice = String(layers[0].paramcd || "");
      if (ui.dataset) ui.layerChoice[ui.dataset] = choice;
    }
    const nodes = asRecords(graph.nodes), edges = asRecords(graph.edges);
    const datasetNodes = nodes.filter(function (n) { return n.kind === "dataset_variable"; });
    const layer = layers.find(function (l) { return String(l.paramcd || "") === choice; }) || layers[0];
    const keySet = {}; keyNamesForDataset(graph.dataset || ui.dataset).forEach(function (name) { keySet[name] = true; });
    const nodeById = {}; nodes.forEach(function (n) { nodeById[n.id] = n; });
    const memberIds = asArray(layer.members).filter(function (id) { const n = nodeById[id]; return !n || !keySet[n.var || ""]; });
    const own = {}; memberIds.forEach(function (id) { own[id] = true; });
    if (layer.records) own[layer.records] = true;
    const inputIds = {};
    edges.forEach(function (e) { if (own[e.to] && datasetNodes.some(function (n) { return n.id === e.from && !keySet[n.var || ""]; })) inputIds[e.from] = true; });
    const visible = Object.assign({}, own, inputIds);
    const shownNodes = nodes.filter(function (n) { return !!visible[n.id] && !(n.kind === "dataset_variable" && keySet[n.var || ""]); });
    const shownEdges = edges.filter(function (e) { return visible[e.from] && visible[e.to]; });
    return Object.assign({}, graph, { nodes: shownNodes, edges: shownEdges, groups: [], _layered: true, _activeLayer: layer, _layerKey: ui.dataset + ":" + choice });
  }
  function hasVariableViewFilter() {
    return !!(ui.dataset && ui.paramFilter[ui.dataset]);
  }
  function paramFilterHtml() {
    const graph = curGraph();
    if (graph && graph.layered === true) {
      const layers = asRecords(graph.layers);
      const carries = carryOverParameters(graph);
      const carryLayer = carries.length ? layers.find(function (l) {
        return asArray(l.members_parameters).some(function (pc) { return carries.indexOf(String(pc || "").toUpperCase()) >= 0; });
      }) : null;
      const options = layers.filter(function (l) {
        const code = String(l.paramcd || "").toUpperCase();
        return !(carries.indexOf(code) >= 0 || (carryLayer && code === String(carryLayer.paramcd || "").toUpperCase()));
      });
      if (carries.length) options.unshift({ paramcd: "CARRY_OVER", _carryLabel: "Carry-over (" + carries.join(", ") + ")" });
      let selected = ui.layerChoice[ui.dataset];
      if (carries.indexOf(String(selected || "").toUpperCase()) >= 0) selected = "CARRY_OVER";
      if (!selected || !options.some(function (l) { return String(l.paramcd || "") === selected; })) selected = options.length ? String(options[0].paramcd || "") : "";
      if (ui.dataset) ui.layerChoice[ui.dataset] = selected;
      return '<label class="layer-select-label">Parameter <select id="graph-layer-select" aria-label="Variable DAG parameter">'
        + options.map(function (l) { const code = String(l.paramcd || ""); const label = l._carryLabel || code; return '<option value="' + esc(code) + '"' + (selected === code ? ' selected' : '') + '>' + esc(label) + '</option>'; }).join("")
        + '</select></label>';
    }
    const params = asRows(S && S.spec && S.spec.parameters).filter(function (r) { return String(r.Dataset || "") === String(ui.dataset || ""); });
    if (!params.length) return "";
    const selected = ui.paramFilter[ui.dataset] || "";
    return '<select id="graph-param-filter" aria-label="Filter graph by parameter"><option value="">All parameters</option>'
      + params.map(function (r) { const code = String(r.PARAMCD || ""); return '<option value="' + esc(code) + '"' + (selected === code ? ' selected' : '') + '>' + esc(code + " - " + String(r.PARAM || "")) + "</option>"; }).join("")
      + '</select>';
  }
  function nodeState(id) {
    const p = curPack();
    const base = { level: "none", problem: null, same_source: false, actions: [], ready: false };
    if (!p || !p.nodes || !p.nodes[id]) return base;
    return p.nodes[id];
  }
  function asRecords(v) {
    if (v == null) return [];
    if (Array.isArray(v)) return v;
    if (typeof v === "object") return [v];
    return [];
  }
  function specFileOf(r) {
    if (!r) return "";
    if (r.file) return r.file;
    if (r.PARAMCD && !r.Variable) return "parameter.csv";
    return (r.Variable || r.Dataset) ? "variable.csv" : "";
  }
  function normalizeSpecRows(rows) {
    return asRecords(rows).map(function (r) {
      const copy = {};
      Object.keys(r || {}).forEach(function (k) { copy[k] = r[k]; });
      if (!copy.file) copy.file = specFileOf(r);
      return copy;
    });
  }
  function actionList(st) {
    let a = st && st.actions ? st.actions : [];
    if (typeof a === "string") a = [a];
    if (!Array.isArray(a)) return [];
    const out = [];
    a.forEach(function (x) {
      const k = ACT_EN[x] || x;
      if (out.indexOf(k) < 0 && ["generate", "compare", "investigate"].indexOf(k) >= 0) out.push(k);
    });
    return out;
  }
  function stageZh(stage) {
    const m = { validate: "Preflight", generate: "Generate", run: "Run", compare: "Compare" };
    return m[stage] || stage || "";
  }
  function levelText(st) {
    if (!st) return "Not generated";
    if (st.problem) {
      const s = stageZh(st.problem.stage);
      return "Problem" + (s ? " · " + s : "");
    }
    const m = { none: "Not generated", L2: "Generated", L3: "Data matched" };
    return m[st.level] || "Not generated";
  }
  function groupState(g) {
    const p = curPack();
    if (p && p.groups && p.groups[g.id]) return p.groups[g.id];
    let level = "L3", problem = null, any = false;
    asArray(g.members).forEach(function (id) {
      const st = nodeState(id);
      any = true;
      if (st.problem && !problem) problem = st.problem;
      const rank = { none: 0, L2: 1, L3: 2 };
      if ((rank[st.level] || 0) < (rank[level] || 0)) level = st.level || "none";
    });
    return { level: any ? level : "none", problem: problem, ready: false };
  }
  function nodesFor(ds, variable, paramcd) {
    const pack = S && S.datasets && S.datasets[ds];
    const g = pack && pack.graph;
    if (!g) return [];
    if (paramcd) {
      const grp = asRecords(g.groups).filter(function (x) { return x.paramcd === paramcd || x.id === ds + "@" + paramcd; })[0];
      return grp ? asArray(grp.members).slice() : [];
    }
    if (!variable) return [];
    const exact = ds + "." + variable;
    const ids = [];
    asRecords(g.nodes).forEach(function (n) { if (n.id === exact || n.var === variable) ids.push(n.id); });
    ids.sort(function (a, b) { return (a === exact ? 0 : 1) - (b === exact ? 0 : 1); });
    return ids;
  }
  function isSel(file, row) {
    const rows = ui.selRows || [];
    for (let i = 0; i < rows.length; i++) {
      if (String(rows[i].file) === String(file) && String(rows[i].row) === String(row)) return true;
    }
    return false;
  }
  function idsForSpecRow(ds, variable, param) {
    if (param) return nodesFor(ds, "", param);
    if (!ds || !variable) return [];
    const exact = ds + "." + variable;
    const ids = nodesFor(ds, variable, "");
    if (ids.indexOf(exact) >= 0) return [exact];
    return ids.length ? [ids[0]] : [];
  }
  function rowInSelection(r, kind) {
    const ids = ui.sel || [];
    if (!ids.length || !r) return false;
    const ds = r.Dataset || "";
    if (kind === "parameters") {
      return nodesFor(ds, "", r.PARAMCD || "").some(function (id) { return ids.indexOf(id) >= 0; });
    }
    const exact = ds + "." + (r.Variable || "");
    if (ids.indexOf(exact) >= 0) return true;
    return ids.some(function (id) { return id.indexOf(exact + "@") === 0; });
  }
  function specRowsOf(kind) {
    let rows = asRows(S && S.spec && S.spec[kind]);
    if (!ui.specAll && ui.dataset) {
      rows = rows.filter(function (r) { return !r.Dataset || r.Dataset === ui.dataset; });
    }
    const f = (ui.specFilter || "").toLowerCase();
    if (!f) return rows;
    return rows.filter(function (r) {
      return Object.keys(r).some(function (k) {
        return String(r[k] == null ? "" : r[k]).toLowerCase().indexOf(f) >= 0;
      });
    });
  }
  function exportReady() {
    const g = curGraph();
    const exportNodes = asRecords(g && g.nodes);
    if (!g || !exportNodes.length) return false;
    for (let i = 0; i < exportNodes.length; i++) {
      const st = nodeState(exportNodes[i].id);
      if (!st || st.problem || st.level !== "L3") return false;
    }
    return true;
  }
  function btn(act, label, opt) {
    opt = opt || {};
    let attrs = ' type="button" data-act="' + esc(act) + '"';
    if (opt.cls) attrs += ' class="' + opt.cls + '"';
    if (opt.title) attrs += ' title="' + esc(opt.title) + '"';
    if (opt.ariaLabel) attrs += ' aria-label="' + esc(opt.ariaLabel) + '"';
    if (opt.data) {
      Object.keys(opt.data).forEach(function (k) { attrs += " data-" + k + '="' + esc(opt.data[k]) + '"'; });
    }
    if (opt.disabled) attrs += " disabled";
    return "<button" + attrs + ">" + esc(label) + "</button>";
  }
  function sideText(obj, side) {
    if (obj == null) return "";
    if (typeof obj === "string") return obj;
    const v = obj[side];
    if (v == null) return "";
    if (typeof v === "string") return v;
    return v.code || v.text || v.log || "";
  }
  function diffTable(compare) {
    if (!compare) return "";
    const d = compare.diffs;
    let cols = [], rows = [];
    if (Array.isArray(d)) {
      d.forEach(function (r) { Object.keys(r || {}).forEach(function (k) { if (cols.indexOf(k) < 0) cols.push(k); }); });
      rows = d;
    } else if (d && typeof d === "object") {
      cols = Object.keys(d);
      const n = cols.length && Array.isArray(d[cols[0]]) ? d[cols[0]].length : 0;
      for (let i = 0; i < n; i++) {
        const r = {};
        cols.forEach(function (c) { r[c] = d[c][i]; });
        rows.push(r);
      }
    }
    const head = '<p class="' + (compare.match ? "good" : "bad") + '">' + esc(compare.summary || (compare.match ? "Exact match" : "Mismatch"))
      + " · primary " + esc(compare.n_primary) + " · validation " + esc(compare.n_validation) + " · differing " + esc(compare.n_diff) + "</p>";
    if (!rows.length) return head;
    let html = head + "<table><thead><tr>" + cols.map(function (c) { return "<th>" + esc(c) + "</th>"; }).join("") + "</tr></thead><tbody>";
    rows.forEach(function (r) {
      html += "<tr>" + cols.map(function (c) {
        return "<td>" + (missingCellHtml(r[c]) || esc(r[c])) + "</td>";
      }).join("") + "</tr>";
    });
    return html + "</tbody></table>";
  }

  const JOB_LABELS = {
    extract: "Extracting dependencies", generate: "Generating", compare: "Comparing",
    investigate: "Investigating", export: "Exporting", "config-test": "Testing connection",
    ask: "Explaining", agent: "Coder working", dag_ask: "Asking the DAG agent"
  };
  function jobsHtml() {
    const jobs = asRecords(S && S.busy);
    if (!jobs.length) return ui.running ? '<span class="pill job"><span class="spin"></span>Working…</span>' : "";
    return jobs.map(function (j) {
      const what = (JOB_LABELS[j.kind] || j.kind || "Working") + " " + (j.node || j.dataset || "");
      const secs = Number(j.elapsed);
      return '<span class="pill job" title="' + esc(what + (j.model ? " · " + j.model : "")) + '"><span class="spin"></span>'
        + esc(what) + (j.model ? ' · <span class="muted">' + esc(j.model) + "</span>" : "")
        + (isFinite(secs) ? " · " + secs + " s" : "") + "</span>";
    }).join("");
  }
  function lastErrorHtml() {
    const e = S && S.last_error;
    if (!e || !e.message || ui.errSeen === e.time) return "";
    if (ui.tab === "spec" && e.kind === "extract" && String(e.dataset || "") === String(ui.dataset || "")) return "";
    const what = (JOB_LABELS[e.kind] || e.kind || "Job") + " " + (e.node || e.dataset || "");
    return '<div class="jobfail" role="alert"><b>' + esc(what) + " failed</b>"
      + (e.model ? ' <span class="muted">(' + esc(e.model) + ")</span>" : "") + ": " + esc(e.message)
      + (e.node ? " " + btn("err-open", "Open " + e.node, { cls: "sm", data: { node: e.node, dataset: e.dataset || "" } }) : "")
      + " " + btn("err-dismiss", "Dismiss", { cls: "sm" }) + "</div>";
  }
  function batchRows(b) {
    return asRecords(b && b.results).filter(function (r) { return r && r.node; });
  }
  function batchRunning() {
    const b = S && S.batch;
    return !!(b && b.total && !b.finished);
  }
  function batchCounts(b) {
    const counts = { l3: 0, l2: 0, failed: 0 };
    batchRows(b).forEach(function (r) {
      if (!r.ok) counts.failed += 1;
      else if (r.level === "L3") counts.l3 += 1;
      else if (r.level === "L2") counts.l2 += 1;
      else counts.failed += 1;
    });
    return counts;
  }
  function batchHtml() {
    const b = S && S.batch;
    if (!b || !b.total) return "";
    if (!b.finished) {
      const n = (Number(b.done) || 0) + (b.current ? 1 : 0);
      const step = b.step === "compare" ? "Comparing" : "Generating";
      const label = "Batch " + n + "/" + b.total + " · " + step + (b.current ? " " + b.current : "");
      return '<span class="pill job"><span class="spin"></span>' + esc(label) + " "
        + btn("batch-stop", "Stop", { cls: "sm" }) + "</span>";
    }
    const c = batchCounts(b);
    const label = "Batch done: " + c.l3 + " Data matched, " + c.l2 + " Generated, " + c.failed + " failed";
    return '<span class="pill">' + esc(label) + " " + btn("batch-results", "Results", { cls: "sm" }) + "</span>";
  }
  function headerHtml() {
    const busy = jobsHtml();
    const mock = MOCK ? '<span class="pill">Sample data</span>' : "";
    return '<header><h1 class="brand"><svg viewBox="0 0 24 24" aria-hidden="true"><path d="m12 2 9 5v10l-9 5-9-5V7l9-5Z"/><path d="m3.5 7.2 8.5 4.9 8.5-4.9M12 12.1V22"/></svg><span>dualdag</span></h1><span class="path muted" title="' + esc(projectText()) + '">' + esc(projectText()) + "</span>"
      + mock + batchHtml() + busy
      + '<span class="outbound">' + esc(outboundNote()) + "</span>"
      + '<button type="button" class="sm" data-act="admin-open">App Admin</button>'
      + '<span class="spacer"></span><span id="st" class="muted">' + esc(ui.msg) + "</span></header>" + lastErrorHtml();
  }
  function tabBarHtml() {
    const icons = {
      spec: '<path d="M4 5h16v14H4zM8 9h8M8 13h8M8 17h5"/>',
      code: '<path d="m8 6-5 6 5 6M16 6l5 6-5 6M14 4l-4 16"/>',
      data: '<ellipse cx="12" cy="5" rx="8" ry="3"/><path d="M4 5v7c0 1.7 3.6 3 8 3s8-1.3 8-3V5M4 12v7c0 1.7 3.6 3 8 3s8-1.3 8-3v-7"/>',
      mismatches: '<path d="m12 3 9 17H3L12 3Z"/><path d="M12 9v5M12 17h.01"/>',
      settings: '<circle cx="12" cy="12" r="3"/><path d="m19.4 15 .1.1 1.4 1.1-1.4 2.4-1.7-.7a8 8 0 0 1-1.6.9l-.3 1.8h-2.8l-.3-1.8a8 8 0 0 1-1.6-.9l-1.7.7-1.4-2.4 1.4-1.1a7 7 0 0 1 0-1.9l-1.4-1.1 1.4-2.4 1.7.7a8 8 0 0 1 1.6-.9l.3-1.8h2.8l.3 1.8a8 8 0 0 1 1.6.9l1.7-.7 1.4 2.4-1.4 1.1a7 7 0 0 1-.1 1.9Z"/>'
    };
    const tabs = [
      { id: "spec", label: "Spec & checks" },
      { id: "code", label: "R code & runs" },
      { id: "data", label: "Data" },
      { id: "mismatches", label: "Mismatches" },
      { id: "qc", label: "QC" },
      { id: "settings", label: "Settings" }
    ].filter(function (tab) { return SHOW_QC_TAB || tab.id !== "qc"; });
    return '<nav class="apptabs" role="tablist">' + tabs.map(function (t) {
      const on = ui.tab === t.id;
      return '<button type="button" role="tab" data-act="tab" data-tab="' + t.id + '" aria-selected="' + (on ? "true" : "false") + '"'
        + (on ? ' class="on"' : "") + '><svg class="tab-icon" viewBox="0 0 24 24" aria-hidden="true">' + icons[t.id] + '</svg><span>' + esc(t.label) + "</span></button>";
    }).join("") + (SUPER_TABS[ui.tab] ? '<button type="button" class="sm" data-act="super-agent">Super agent</button>' : "") + "</nav>";
  }
  function fullRunSideHtml(side) {
    const s = ui.fullRun.status && ui.fullRun.status.dataset === ui.dataset && ui.fullRun.status.sides
      ? ui.fullRun.status.sides[side] : null;
    const run = s && s.run;
    const variablesRun = asArray(run && run.variables_run);
    const skipped = asArray(run && run.skipped);
    const notRun = asArray(run && run.not_run);
    const readiness = s && s.readiness || {};
    const hasReadiness = !!(s && s.readiness && readiness.variables_total != null);
    const missingLayers = asArray(readiness.layers_missing);
    const withoutCode = asArray(readiness.variables_without_code);
    const allCode = Number(readiness.variables_with_code || 0) === Number(readiness.variables_total || 0);
    const layerBlock = false;
    const steps = asArray(ui.fullRun.steps && ui.fullRun.steps[side]);
    const missingLayerLine = missingLayers.length ? '<p>No Variable DAG yet: ' + esc(missingLayers.join(", ")) + '.</p>' : "";
    const nothingRun = !!(run && run.ok && !variablesRun.length && skipped.length);
    let state = "Not run", cls = "muted", elapsed = "";
    if (s && s.running) { state = "Running"; cls = "job"; }
    else if (run) { state = nothingRun ? "Nothing run" : (run.ok ? (skipped.length ? "Passed with " + skipped.length + " skipped" : "Passed") : "Failed"); cls = run.ok && !skipped.length ? "good" : (run.ok ? "warn" : "bad"); }
    if (run && run.started) elapsed = " · Started " + run.started + (run.seconds != null ? " · " + run.seconds + " seconds" : "");
    else if (s && s.running) {
      const started = ui.fullRun.started[side] || Date.now();
      elapsed = " · Started " + new Date(started).toLocaleString() + " · " + Math.max(0, Math.floor((Date.now() - started) / 1000)) + " seconds";
    }
    let details = "";
    if (run && run.ok) {
      if (nothingRun) details += '<p>No variable has code on this side yet.</p>';
      else details += '<p>Rows: ' + esc(run.rows == null ? "—" : run.rows) + ' · Columns: ' + esc(run.columns == null ? "—" : run.columns) + '</p><p>Variables run: ' + esc(variablesRun.join(", ") || "none") + '</p>';
    } else if (run && !run.ok) {
      if (run.failed_variable) details += '<p>Failed variable: <b>' + esc(displayNode(run.failed_variable)) + '</b></p>';
      details += '<pre class="log">' + esc(run.error || "Run failed without an error message.") + '</pre>';
      details += '<p>Variables completed: ' + esc(asArray(run.variables_run).map(function (id) { return displayNode(id); }).join(", ") || "none") + '</p>';
    }
    if (run && run.partial) details += '<p class="warn">Partial result: ' + esc(variablesRun.length) + ' of ' + esc(run.variables_total || readiness.variables_total || 0) + ' variables</p>';
    if (run && (notRun.length || skipped.length)) details += '<details class="run-missing"><summary>Not run (' + (notRun.length || skipped.length) + ')</summary><ul>'
      + (notRun.length ? notRun.map(function (entry) { return '<li>' + esc(entry.id || "") + ': ' + esc(entry.reason || "not run") + '</li>'; })
        : skipped.map(function (id) { return '<li>' + esc(id) + '</li>'; })).join("") + '</ul></details>';
    if (s && s.stale) details += '<p class="warn">Programs changed since this run.</p>';
    const log = asArray(s && s.log_tail);
    if (log.length) details += '<pre class="log">' + esc(log.join("\n")) + '</pre>';
    const sideError = fullRunSideError(ui.dataset, side);
    if (sideError) details += '<p class="bad">' + esc(sideError) + '</p>';
    let readyHtml = "";
    if (hasReadiness) {
      if (Number(readiness.layers_total || 0) > 0) readyHtml += '<p>Variable DAGs: ' + esc(readiness.layers_generated || 0) + ' of ' + esc(readiness.layers_total) + ' generated.'
        + (missingLayers.length ? ' Missing: ' + esc(missingLayers.join(", ")) + '.' : "") + '</p>';
      readyHtml += '<p>Programs: ' + esc(readiness.variables_with_code || 0) + ' of ' + esc(readiness.variables_total || 0) + ' variables have code on this side.</p>';
      if (withoutCode.length) readyHtml += '<details class="run-missing"><summary>Without code (' + withoutCode.length + ')</summary><ul>'
        + withoutCode.map(function (id) { return '<li><button type="button" class="sm quiet" data-act="fullrun-open-node" data-side="' + esc(side) + '" data-id="' + esc(id) + '">' + esc(displayNode(id)) + '</button></li>'; }).join("")
        + '</ul></details>';
      readyHtml += missingLayerLine;
      if (steps.length) readyHtml += '<h3>Program</h3>' + steps.map(function (step) {
        return '<div class="run-program-step"><button type="button" class="sm quiet" data-act="fullrun-open-node" data-side="' + esc(side) + '" data-id="' + esc(step.id) + '">' + esc(displayNode(step.id)) + '</button><pre class="log">' + esc(step.code) + '</pre></div>';
      }).join("");
      else readyHtml += '<p class="muted">No program yet on this side.</p>';
      if (steps.length) readyHtml += '<p class="muted">Runs ' + steps.length + ' programs. ' + Math.max(0, Number(readiness.variables_total || 0) - steps.length) + ' variables are not included.</p>';
    }
    return '<section class="pane"><div class="ph"><b>' + (side === "primary" ? "Primary" : "Validation") + '</b><span class="pill ' + cls + '">' + esc(state) + '</span></div>'
      + '<div class="scroll"><div class="run-program-area">' + readyHtml + '<p>' + esc(elapsed || "No run recorded.") + '</p></div>'
      + '<div class="run-log-area">' + details + '</div><div class="run-controls"><div class="rowbtns">' + btn("fullrun", "Run " + side, { cls: "pri sm", disabled: !steps.length || !!(s && s.running), data: { side: side } })
      + btn("fullrun-text", "Full log", { cls: "sm", data: { side: side, what: "log" }, disabled: !run })
      + btn("fullrun-download-program", "Download program", { cls: "sm", data: { side: side } }) + '</div></div></div></section>';
  }
  function runTabHtml() {
    if (!ui.dataset) return '<div class="empty">Select a dataset first.</div>';
    if (ui.fullRun.status && ui.fullRun.status.noGraph) return '<div class="empty">No dependency graph is available for this dataset.'
      + (ui.fullRun.statusError ? ' <p class="bad">' + esc(ui.fullRun.statusError) + '</p>' : "") + '</div>';
    return '<div class="ph"><p>Runs all of this side\'s generated code, in DAG order, on the full data. It does not change any variable\'s status.</p></div>'
      + (ui.fullRun.busy && !ui.fullRun.status ? '<p class="muted">Loading run status…</p>' : '')
      + (ui.fullRun.statusError ? '<p class="bad">' + esc(ui.fullRun.statusError) + '</p>' : '')
      + '<div class="codesplit">' + fullRunSideHtml("primary") + fullRunSideHtml("validation") + '</div>';
  }
  function stopFullRunPoll() {
    if (ui.fullRun.timer) clearTimeout(ui.fullRun.timer);
    ui.fullRun.timer = null;
  }
  function scheduleFullRunPoll() {
    stopFullRunPoll();
    if (ui.tab !== "run" || !ui.dataset || !ui.fullRun.status) return;
    const sides = ui.fullRun.status.sides || {};
    if (!(sides.primary && sides.primary.running) && !(sides.validation && sides.validation.running)) return;
    ui.fullRun.timer = setTimeout(function () { loadFullRunStatus(true); }, 2000);
  }
  async function loadFullRunStatus(silent, refreshStepsIfIdle) {
    if (!ui.dataset || ui.tab !== "run") return;
    const ds = ui.dataset;
    if (ui.fullRun.loadingDataset === ds) return;
    ui.fullRun.loadingDataset = ds;
    ui.fullRun.busy = !silent;
    if (!silent) { ui.fullRun.statusError = ""; render(); }
    try {
      const result = await api("/api/fullrun/status?dataset=" + encodeURIComponent(ds));
      if (ui.tab !== "run" || ui.dataset !== ds) return;
      const wasRunning = !!(ui.fullRun.status && ui.fullRun.status.dataset === ds && ui.fullRun.status.sides
        && (ui.fullRun.status.sides.primary.running || ui.fullRun.status.sides.validation.running));
      const isRunning = !!(result.sides && (result.sides.primary.running || result.sides.validation.running));
      ui.fullRun.status = result;
      if (!silent || (wasRunning && !isRunning) || (refreshStepsIfIdle && !isRunning)) await loadFullRunSteps(ds);
      ["primary", "validation"].forEach(function (side) {
        if (result.sides && result.sides[side] && result.sides[side].running && !ui.fullRun.started[side]) ui.fullRun.started[side] = Date.now();
        else if (!(result.sides && result.sides[side] && result.sides[side].running)) delete ui.fullRun.started[side];
        const oldError = fullRunSideError(ds, side);
        const readiness = result.sides && result.sides[side] && result.sides[side].readiness;
        if (oldError.indexOf("Generate Variable DAGs first") === 0 && readiness && !asArray(readiness.layers_missing).length)
          setFullRunSideError(ds, side, "");
      });
    } catch (e) {
      if (ui.tab !== "run" || ui.dataset !== ds) return;
      ui.fullRun.status = e && e.status === 404 ? { dataset: ds, sides: {}, noGraph: true } : null;
      ui.fullRun.statusError = e && e.status === 404 ? "" : ((e && e.message) || "Could not load full run status");
    } finally {
      if (ui.fullRun.loadingDataset === ds) ui.fullRun.loadingDataset = "";
      if (ui.dataset === ds) ui.fullRun.busy = false;
      if (ui.tab === "run" && ui.dataset === ds) { render(); scheduleFullRunPoll(); }
    }
  }
  async function loadFullRunSteps(ds) {
    ui.fullRun.steps = ui.fullRun.steps || {};
    const packs = await Promise.all(["primary", "validation"].map(function (side) {
      return api("/api/fullrun/steps?dataset=" + encodeURIComponent(ds) + "&side=" + side)
        .then(function (pack) { return [side, asArray(pack.steps)]; })
        .catch(function () { return [side, []]; });
    }));
    if (ui.dataset === ds) packs.forEach(function (entry) { ui.fullRun.steps[entry[0]] = entry[1]; });
  }
  async function viewFullRunText(side, what) {
    const url = "/api/fullrun/" + what + "?dataset=" + encodeURIComponent(ui.dataset) + "&side=" + encodeURIComponent(side);
    let text;
    if (MOCK) text = mockApi(url, {});
    else {
      const r = await fetch(url, { cache: "no-store", headers: { "X-Dualdag-Token": token() } });
      text = await r.text();
      if (!r.ok) throw asHttpErr(r.status, text || (r.status + ""), { error: text });
    }
    openModal('<h3>' + esc(side + " · " + (what === "log" ? "Full log" : "Program")) + '</h3><pre class="log">' + esc(text) + '</pre><div class="rowbtns">' + btn("modal-close", "Close", { cls: "sm" }) + '</div>');
  }
  async function downloadFullRunProgram(side) {
    const url = "/api/fullrun/program?dataset=" + encodeURIComponent(ui.dataset) + "&side=" + encodeURIComponent(side);
    const r = await fetch(url, { cache: "no-store", headers: { "X-Dualdag-Token": token() } });
    if (!r.ok) throw asHttpErr(r.status, await r.text());
    const blob = await r.blob();
    const link = document.createElement("a");
    link.href = URL.createObjectURL(blob);
    link.download = String(ui.dataset || "dataset").toLowerCase() + "-" + side + "-program.R";
    document.body.appendChild(link); link.click(); link.remove(); URL.revokeObjectURL(link.href);
  }
  async function downloadNetwork(parameter) {
    const parameters = Array.isArray(parameter) ? parameter : [parameter];
    for (const selected of parameters) {
      const query = "dataset=" + encodeURIComponent(ui.dataset || "") + (selected ? "&paramcd=" + encodeURIComponent(selected) : "");
      if (MOCK) {
        const network = selected ? { dataset: "ADVS", parameter: selected, spec_hash: "mock", variables: [], edges: [], inputs: [] }
          : { dataset: "ADVS", spec_hash: "mock", parameters: [], edges: [] };
        saveBlob(new Blob([JSON.stringify(network, null, 2)], { type: "application/json" }), selected ? String(selected).toUpperCase() + ".json" : "_parameters.json");
        continue;
      }
      const r = await fetch("/api/graph/download?what=network&" + query, { cache: "no-store", headers: { "X-Dualdag-Token": token() } });
      if (!r.ok) throw asHttpErr(r.status, await r.text());
      const blob = await r.blob();
      const name = selected ? String(selected).toUpperCase() + ".json" : "_parameters.json";
      const link = document.createElement("a"); link.href = URL.createObjectURL(blob); link.download = name;
      document.body.appendChild(link); link.click(); link.remove(); URL.revokeObjectURL(link.href);
    }
  }
  function folded(key) { return !!ui.fold[key]; }
  function missingHtml() {
    const md = (S && S.missing_data) || {};
    const items = [];
    Object.keys(md).forEach(function (ds) {
      asArray(md[ds]).forEach(function (dom) {
        items.push(ds + " references " + dom + ", but " + String(dom).toLowerCase() + ".xpt is not uploaded.");
      });
    });
    if (!items.length) return "";
    return '<div class="warn"><b>Missing xpt</b><ul>' + items.map(function (t) { return "<li>" + esc(t) + "</li>"; }).join("") + "</ul></div>";
  }
  function fileLabel(key) {
    const f = ui.files[key];
    return f ? esc(Array.isArray(f) ? (f.length + " files selected") : f.name) : '<span class="muted">None selected</span>';
  }
  function domainKey(d) {
    const dom = String((d && d.domain) || "").toUpperCase();
    if (dom) return dom;
    return String((d && d.name) || "").replace(/\.xpt$/i, "").toUpperCase();
  }
  function domainCount() {
    const sum = (S && S.catalog_summary) || {};
    const sk = Object.keys(sum);
    if (sk.length) return sk.length;
    const cat = ui.catalog && ui.catalog.domains;
    if (cat && Object.keys(cat).length) return Object.keys(cat).length;
    return ((S && S.data) || []).length;
  }
  function flowHtml() {
    if (!S || !S.spec) return "";
    const n = domainCount();
    const g = curGraph();
    let extracting = false;
    asRecords(S.busy).forEach(function (b) { if (b && b.kind === "extract") extracting = true; });
    const step1 = ui.catalogStatus === "ok" ? "done" : "on";
    const step2 = extracting ? "on" : (g ? "done" : "");
    return '<ol class="flow settings-flow"><li class="' + step1 + '">' + settingsIcon("check") + 'Read input variables · ' + n + " domains</li>"
      + '<li class="' + step2 + '">' + settingsIcon("check") + 'Extract dependencies</li></ol>';
  }
  function catVars(rec) {
    const rows = asRows(rec && rec.variables);
    if (rows.length) return rows;
    return asRecords(rec && rec.variables);
  }
  function catDomain(d) {
    const domains = ui.catalog && ui.catalog.domains;
    if (!domains) return null;
    const dom = domainKey(d);
    const keys = Object.keys(domains);
    for (let i = 0; i < keys.length; i++) {
      const rec = domains[keys[i]];
      if (String(keys[i]).toUpperCase() === dom) return rec;
      if (rec && String(rec.file || "").toLowerCase() === String((d && d.name) || "").toLowerCase()) return rec;
    }
    return null;
  }
  function catBody(d) {
    const rec = catDomain(d);
    const dom = domainKey(d);
    if (!rec) {
      if (ui.catalogStatus === "missing" || ui.catalogStatus === "error") return '<p class="muted">The variable catalog is not back yet.</p>';
      return '<p class="muted">This file has no variable catalog yet.</p>';
    }
    const q = (ui.catFilter[dom] || "").toLowerCase();
    let vars = catVars(rec);
    if (q) {
      vars = vars.filter(function (v) {
        return String(v.name || "").toLowerCase().indexOf(q) >= 0
          || String(v.label || "").toLowerCase().indexOf(q) >= 0;
      });
    }
    const rows = vars.map(function (v) {
      const len = v.length == null ? "" : v.length;
      return "<tr><td>" + esc(v.name) + "</td><td>" + esc(v.label || "") + "</td><td>" + esc(v.type || "") + "</td><td>" + esc(len) + "</td></tr>";
    }).join("");
    const values = (rec && rec.values) || {};
    let vals = "";
    Object.keys(values).forEach(function (k) {
      const list = asArray(values[k]);
      let shown = list;
      if (q) {
        const byVal = list.filter(function (x) { return String(x).toLowerCase().indexOf(q) >= 0; });
        const byCol = k.toLowerCase().indexOf(q) >= 0;
        if (!byVal.length && !byCol) return;
        shown = byVal.length ? byVal : list;
      }
      vals += "<p><b>" + esc(k) + "</b>: " + (shown.length ? shown.map(esc).join(", ") : '<span class="muted">none</span>') + "</p>";
    });
    return (rows
      ? "<table><thead><tr><th>Variable</th><th>Label</th><th>Type</th><th>Length</th></tr></thead><tbody>" + rows + "</tbody></table>"
      : '<p class="muted">No matching variables.</p>')
      + (vals ? '<div class="vals">' + vals + "</div>" : "");
  }
  function lookupTok(dom, varName) {
    const domains = ui.catalog && ui.catalog.domains;
    if (!domains) return "unknown";
    let rec = null;
    const keys = Object.keys(domains);
    const want = String(dom || "").toUpperCase();
    for (let i = 0; i < keys.length; i++) {
      if (String(keys[i]).toUpperCase() === want) rec = domains[keys[i]];
    }
    if (!rec) return "miss";
    const hit = catVars(rec).some(function (v) {
      return String(v.name || "").toUpperCase() === String(varName || "").toUpperCase();
    });
    return hit ? "ok" : "miss";
  }
  function specDatasetSet() {
    const set = {};
    const spec = S && S.spec;
    if (!spec) return set;
    asArray(spec.datasets).forEach(function (d) {
      const k = String(d || "").toUpperCase();
      if (k) set[k] = true;
    });
    asRows(spec.variables).forEach(function (r) {
      const k = String(r.Dataset || "").toUpperCase();
      if (k) set[k] = true;
    });
    asRows(spec.parameters).forEach(function (r) {
      const k = String(r.Dataset || "").toUpperCase();
      if (k) set[k] = true;
    });
    return set;
  }
  function uploadedDomainSet() {
    const set = {};
    ((S && S.data) || []).forEach(function (d) {
      const k = domainKey(d);
      if (k) set[k] = true;
    });
    const sum = (S && S.catalog_summary) || {};
    Object.keys(sum).forEach(function (k) {
      if (sum[k] && typeof sum[k] === "object") set[String(k).toUpperCase()] = true;
    });
    const domains = ui.catalog && ui.catalog.domains;
    if (domains) Object.keys(domains).forEach(function (k) { set[String(k).toUpperCase()] = true; });
    return set;
  }
  function specHasVar(dataset, varName) {
    const ds = String(dataset || "").toUpperCase();
    const v = String(varName || "").toUpperCase();
    if (!ds || !v || !S || !S.spec) return false;
    const rows = asRows(S.spec.variables);
    for (let i = 0; i < rows.length; i++) {
      if (String(rows[i].Dataset || "").toUpperCase() === ds && String(rows[i].Variable || "").toUpperCase() === v) return true;
    }
    return false;
  }
  // Own-dataset and other spec-only datasets are spec variables, not input xpt columns.
  function classifyTok(dom, varName, rowDataset) {
    const d = String(dom || "").toUpperCase();
    const row = String(rowDataset || "").toUpperCase();
    const own = !!(row && d === row);
    const otherSpec = !own && specDatasetSet()[d] && !uploadedDomainSet()[d];
    if (own || otherSpec) return specHasVar(d, varName) ? "self" : "specmiss";
    return lookupTok(d, varName);
  }
  function markRefs(text, rowDataset) {
    const s = String(text == null ? "" : text);
    const re = /\b([A-Za-z][A-Za-z0-9_]*)\.([A-Za-z][A-Za-z0-9_]*)\b/g;
    let html = "";
    let last = 0;
    let m;
    while ((m = re.exec(s))) {
      html += esc(s.slice(last, m.index));
      const kind = classifyTok(m[1], m[2], rowDataset);
      let cls = "tok";
      let title = "";
      if (kind === "ok") cls = "tok ok";
      else if (kind === "self") { cls = "tok self"; title = ' title="Variable in this dataset"'; }
      else if (kind === "specmiss") { cls = "tok miss"; title = ' title="Not in the spec"'; }
      else if (kind === "miss") { cls = "tok miss"; title = ' title="Variable or dataset is not in the data"'; }
      html += '<span class="' + cls + '"' + title + ">" + esc(m[0]) + "</span>";
      last = m.index + m[0].length;
    }
    html += esc(s.slice(last));
    return html;
  }
  function setIcon(kind) {
    const paths = {
      spec: '<path d="M4 6h16M4 12h16M4 18h16"/><circle cx="8" cy="6" r="2"/><circle cx="16" cy="12" r="2"/><circle cx="10" cy="18" r="2"/>',
      data: '<path d="m12 3 9 5-9 5-9-5 9-5Z"/><path d="m3 12 9 5 9-5M3 16l9 5 9-5"/>',
      comparison: '<path d="M12 3v18M5 6h14M5 6l-3 7h6L5 6Zm14 0-3 7h6l-3-7ZM8 21h8"/>',
      model: '<path d="m12 2 2.2 6.8L21 11l-6.8 2.2L12 20l-2.2-6.8L3 11l6.8-2.2L12 2Z"/><path d="m19 14 1.1 3.1L23 18l-2.9 1-1.1 3-1-3-3-.9 3-.9 1-3.2Z"/>'
    };
    return '<svg class="seticon" viewBox="0 0 24 24" aria-hidden="true" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round">' + paths[kind] + '</svg>';
  }
  function settingsIcon(kind) {
    const paths = {
      upload: '<path d="M12 16V4m0 0L7 9m5-5 5 5"/><path d="M5 14v5h14v-5"/>',
      check: '<path d="m5 12 4 4L19 6"/>',
      folder: '<path d="M3 6h7l2 2h9v10H3z"/><path d="M12 10v6m-3-3h6"/>',
      chevron: '<path d="m9 18 6-6-6-6"/>',
      trash: '<path d="M4 7h16M10 11v6m4-6v6M6 7l1 14h10l1-14M9 7V4h6v3"/>',
      file: '<path d="M6 3h8l4 4v14H6z"/><path d="M14 3v5h5M9 13h6m-6 4h6"/>',
      x: '<path d="m6 6 12 12M18 6 6 18"/>'
    };
    return '<svg class="set-action-icon" viewBox="0 0 24 24" aria-hidden="true" fill="none" stroke="currentColor" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round">' + paths[kind] + '</svg>';
  }
  function settingsBtn(act, label, icon, opt) {
    const html = btn(act, label, opt);
    return html.replace('>' + esc(label) + '</button>', '>' + settingsIcon(icon) + '<span>' + esc(label) + '</span></button>');
  }
  function setCard(title, subtitle, kind, body, cls) {
    return '<section class="setcard ' + (cls || '') + '"><div class="sethead">' + setIcon(kind)
      + '<div><h2>' + esc(title) + '</h2><p>' + esc(subtitle) + '</p></div></div><div class="setscroll">' + body + "</div></section>";
  }
  function specSettingsHtml() {
    const hash = S && S.spec && S.spec.hash ? String(S.spec.hash).slice(0, 12) : "";
    return '<p class="muted">' + (hash ? "hash " + esc(hash) : "Not uploaded yet") + "</p>"
      + '<div class="template-tabs">' + btn("tpl", "Variable template", { cls: "sm active", data: { name: "variable.csv" } })
      + btn("tpl", "Parameter template", { cls: "sm", data: { name: "parameter.csv" } }) + "</div>"
      + '<div class="fileline spec-file">' + settingsBtn("browse", "variable.csv", "file", { cls: "sm", data: { target: "f-var" } }) + "<span>" + fileLabel("var") + "</span></div>"
      + '<div class="fileline spec-file">' + settingsBtn("browse", "parameter.csv", "file", { cls: "sm", data: { target: "f-param" } }) + "<span>" + fileLabel("param") + "</span></div>"
      + '<input id="f-var" type="file" accept=".csv,text/csv" hidden>'
      + '<input id="f-param" type="file" accept=".csv,text/csv" hidden>'
      + '<div class="rowbtns">' + settingsBtn("spec-up", "Upload spec", "upload", { cls: "pri sm", title: "Asks once before replacing the spec" })
      + (S && S.spec ? settingsBtn("recheck", "Check", "check", { cls: "sm" }) : "") + "</div>"
      + flowHtml();
  }
  function xptSettingsHtml() {
    const dataRows = ((S && S.data) || []).map(function (d) {
      const dom = domainKey(d);
      const open = !!ui.catOpen[dom];
      let row = "<tr><td>" + esc(d.name) + "</td><td>" + esc(d.domain) + "</td><td>"
        + settingsBtn("cat-toggle", open ? "Collapse" : "Expand", "chevron", { cls: "sm", data: { domain: dom } })
        + settingsBtn("rm-data", "Remove", "trash", { cls: "sm", data: { name: d.name || "" } }) + "</td></tr>";
      if (open) {
        row += '<tr class="catrow"><td colspan="3">'
          + '<input id="cat-filter-' + esc(dom) + '" placeholder="Filter variables or values" value="' + esc(ui.catFilter[dom] || "") + '">'
          + '<div id="cat-body-' + esc(dom) + '">' + catBody(d) + "</div></td></tr>";
      }
      return row;
    }).join("");
    return missingHtml()
      + '<div class="fileline">' + settingsBtn("browse", "Choose xpt", "folder", { cls: "sm", data: { target: "f-xpt" } })
      + settingsBtn("browse-folder", "Choose folder", "folder", { cls: "sm", data: { target: "f-xpt-folder" } }) + "<span>" + fileLabel("xpt") + "</span>"
      + settingsBtn("data-up", "Upload", "upload", { cls: "sm" }) + "</div>"
      + '<input id="f-xpt" type="file" accept=".xpt" multiple hidden>'
      + '<input id="f-xpt-folder" type="file" accept=".xpt" webkitdirectory multiple hidden>'
      + (dataRows ? "<table><thead><tr><th>File</th><th>Domain</th><th></th></tr></thead><tbody>" + dataRows + "</tbody></table>" : '<p class="muted">No xpt files yet.</p>');
  }
  function qcSettingsHtml() {
    const qcRows = (ui.qcList || []).filter(function (d) { return d.dataset === ui.dataset; }).map(function (d) {
      return "<tr><td>" + esc(d.dataset) + "</td><td>" + esc(d.updated || "") + "</td><td>"
        + btn("qc-view", "View", { cls: "sm", data: { name: d.dataset } }) + "</td></tr>";
    }).join("");
    return btn("tpl", "QC template", { cls: "sm", data: { name: "qc-methodology.md" } })
      + '<div class="field"><label for="qc-ds">Dataset</label><input id="qc-ds" value="' + esc(ui.dataset || "") + '" readonly></div>'
      + '<div class="fileline">' + btn("browse", "Choose md", { data: { target: "f-qc" } }) + "<span>" + fileLabel("qc") + "</span>"
      + btn("qc-up", "Upload QC", { cls: "sm", title: "Asks before replacing" }) + "</div>"
      + '<input id="f-qc" type="file" accept=".md,text/markdown" hidden>'
      + (qcRows ? "<table><thead><tr><th>Dataset</th><th>Updated</th><th></th></tr></thead><tbody>" + qcRows + "</tbody></table>" : '<p class="muted">No QC documents yet.</p>');
  }
  function cfgValues() {
    const c = (S && S.config_public) || {};
    const d = ui.cfgDraft;
    return {
      provider: d ? d.provider : (c.provider || "openai"),
      base_url: d ? d.base_url : (c.base_url || ""),
      model: d ? d.model : (c.model || ""),
      key: d ? d.key : "",
      clear: d ? !!d.clear : false,
      hint: c.api_key_set ? ("Saved " + (c.api_key_hint || "") + (c.api_key_source ? " (" + c.api_key_source + ")" : "")) : "Not saved yet",
      cli_use_login: CLI_PROVIDERS[d ? d.provider : (c.provider || "openai")] ? true : c.cli_use_login !== false,
    };
  }
  function outboundNote() {
    const prov = cfgValues().provider || "";
    if (CLI_PROVIDERS[prov]) {
      const label = prov === "claude_cli" ? "Claude Code" : prov === "codex_cli" ? "Codex" : "Grok";
      return "Outbound: up to 20 randomly sampled non-missing values from lineage columns, plus categorical levels, are sent to your " + label + " login.";
    }
    const base = "Outbound: up to 20 randomly sampled non-missing values from each non-key lineage column, plus categorical levels, are sent to the model.";
    return base;
  }
  function cfgMsgHtml() {
    const t = ui.cfgTest;
    if (!t) return '<p id="cfg-msg" class="muted"></p>';
    if (t.pending) return '<p id="cfg-msg" class="muted">' + esc(t.text) + "</p>";
    if (t.ok) return '<p id="cfg-msg" class="muted"><span class="good">OK</span> · ' + esc(t.text) + "</p>";
    return '<p id="cfg-msg" class="bad">' + esc(t.text) + "</p>";
  }
  const CLI_PROVIDERS = {
    grok_cli: { bin: "grok", label: "Grok CLI", models: ["grok-4.7", "grok-4.7-build-fast", "grok-4.6", "grok-4.5"] },
    claude_cli: {
      bin: "claude", label: "Claude Code CLI (Claude subscription login)",
      models: ["claude-sonnet-5-5", "claude-opus-5-5", "claude-haiku-5-5"]
    },
    codex_cli: {
      bin: "codex", label: "Codex CLI (ChatGPT login)", allowOther: true,
      models: ["", "gpt-6-luna", "gpt-5.6-terra", "gpt-5.6-luna"]
    }
  };
  function cliModelLabel(id) {
    if (id === "") return "(CLI default)";
    return id;
  }
  function cliLoginHtml(provider) {
    const mine = ui.cliStatus && ui.cliStatus.provider === provider ? ui.cliStatus : null;
    let body = "";
    if (!mine || mine.pending || !mine.data) {
      body += '<p class="muted">Checking login…</p>';
    } else {
      const d = mine.data;
      body += "<p>Installed: " + (d.installed ? "yes" : "no");
      if (d.version) body += " · " + esc(d.version);
      body += "</p>";
      if (d.path) body += '<p class="muted">' + esc(d.path) + "</p>";
      if (d.logged_in === true) body += "<p>Logged in" + (d.method ? " · " + esc(d.method) : "") + "</p>";
      else if (d.logged_in === false) body += '<p class="bad">Not logged in</p>';
      if (d.hint) body += '<p class="muted">' + esc(d.hint) + "</p>";
    }
    return '<div class="cli-status">' + body + '<div class="rowbtns">' + settingsBtn("cfg-cli-check", "Check login", "check", { cls: "sm" }) + "</div></div>";
  }
  function modelSettingsHtml() {
    const v = cfgValues();
    const cli = CLI_PROVIDERS[v.provider];
    function opt(val, lab) {
      return '<option value="' + val + '"' + (v.provider === val ? " selected" : "") + ">" + esc(lab) + "</option>";
    }
    let useOther = false;
    let cliSel = cli ? cli.models[0] : "";
    if (cli && cli.allowOther && ui.cfgOther) {
      useOther = true;
      cliSel = "__other__";
    } else if (cli && cli.models.indexOf(v.model) >= 0) {
      cliSel = v.model;
    } else if (cli && cli.allowOther) {
      useOther = true;
      cliSel = "__other__";
    }
    const cliOpts = cli ? cli.models.map(function (m) {
      return '<option value="' + m + '"' + (m === cliSel ? " selected" : "") + ">" + esc(cliModelLabel(m)) + "</option>";
    }).join("") : "";
    const otherOpt = cli && cli.allowOther
      ? '<option value="__other__"' + (useOther ? " selected" : "") + ">Other…</option>" : "";
    const apiLabel = cli ? "" : " (API key)";
    const head = '<div class="field"><label for="cfg-provider">Provider</label><select id="cfg-provider">'
      + (ui.loggedCliProviders || ["grok_cli", "claude_cli", "codex_cli"]).map(function (p) { return opt(p, CLI_PROVIDERS[p].label); }).join("")
      + opt("openai", "OpenAI" + apiLabel) + opt("anthropic", "Anthropic" + apiLabel) + opt("openai_compatible", "OpenAI-compatible" + apiLabel) + "</select></div>";
    const strongField = '<div class="field"><label for="cfg-strong-model">Strong model for the DAG (optional)</label><input id="cfg-strong-model" value="'
      + esc(ui.cfgDraft && ui.cfgDraft.strong_model != null ? ui.cfgDraft.strong_model : (S && S.config_public && S.config_public.strong_model) || "") + '" placeholder="Blank uses the model above"></div>';
    const left = cli
      ? head + '<div class="field"><label for="cfg-model-cli">Model</label><select id="cfg-model-cli">' + cliOpts + otherOpt + "</select></div>"
        + (useOther ? '<div class="field"><label for="cfg-model-other">Other model</label><input id="cfg-model-other" value="'
          + esc(v.model) + '" placeholder="Model id"></div>' : "") + strongField
      : head + '<div class="field"><label for="cfg-base">Base URL</label><input id="cfg-base" value="' + esc(v.base_url) + '" placeholder="for example https://openrouter.ai/api/v1"></div>'
        + '<div class="field"><label for="cfg-model">Model</label><input id="cfg-model" value="' + esc(v.model) + '" placeholder="for example gpt-4.1"></div>' + strongField
        + '<div class="field"><label for="cfg-key">API key</label><input id="cfg-key" type="password" value="' + esc(v.key) + '" placeholder="' + esc(v.hint) + '" autocomplete="off"></div>'
        + '<label><input id="cfg-clear" type="checkbox"' + (v.clear ? " checked" : "") + "> Clear the saved key</label>";
    const right = cli
      ? cliLoginHtml(v.provider) + '<p class="muted model-help">Uses your ' + esc(v.provider === "claude_cli" ? "Claude Code" : v.provider === "codex_cli" ? "Codex" : "Grok") + " login.</p>"
      : '<p class="muted model-help">For example https://openrouter.ai/api/v1. Do not include /chat/completions. Leave blank to use the provider default.</p>'
        + '<p class="muted model-help">The key is sent only to the provider you choose, and it stays on this machine. Leave it blank to keep the saved key.</p>';
    return '<div class="model-layout"><div class="model-fields">' + left + '</div><div class="model-status">' + right + cfgMsgHtml()
      + '<div class="rowbtns">' + btn("cfg-save", "Save", { cls: "pri sm" }) + btn("cfg-test", "Test connection", { cls: "sm" }) + "</div></div></div>";
  }
  function settingsHtml() {
    const cfg = cfgValues();
    const model = cfg.model || (CLI_PROVIDERS[cfg.provider] ? "(CLI default)" : "No model selected");
    const configured = !!String(cfg.model || "").trim();
    return '<div class="settings"><div class="settings-top"><div><h1>Analysis Setup</h1>'
      + '<p>Configure data and model for your analysis</p></div><div class="settings-actions"><span class="current-model' + (configured ? " configured" : "") + '"><i></i>' + esc(configured ? model : "No model selected")
      + '</span>' + settingsBtn("cfg-test", "Check connection", "check", { cls: "sm" })
      + '<button type="button" class="help-button" title="Configure data and model for your analysis" aria-label="Help">?</button></div></div>'
      + '<div class="settings-grid">'
      + setCard("Spec", "Load variable and parameter templates", "spec", specSettingsHtml())
      + setCard("Analysis dataset formats", "Classify datasets for the correct extraction flow", "spec", datasetFormatSettingsHtml())
      + setCard("Data xpt", "Manage XPT data files", "data", xptSettingsHtml())
      + setCard("Model", "Configure CLI provider and model", "model", modelSettingsHtml(), "model-card")
      + '</div>'
      + "</div>";
  }
  function datasetFormatSettingsHtml() {
    const datasets = asArray(S && S.spec && S.spec.datasets);
    if (!datasets.length) return '<p class="muted">Upload a spec to classify its analysis datasets.</p>';
    const formats = (S && S.dataset_types) || {};
    const descriptions = {
      "ADSL": "Subject-level analysis dataset, one record per subject, with demographics, treatment groups, key dates, and population flags such as SAFFL and ITTFL.",
      "BDS": "Basic Data Structure: one or more records per subject, analysis parameter (PARAMCD), and analysis time point. Core variables include PARAM/PARAMCD and AVAL/AVALC. Examples: laboratory (ADLB), vital signs (ADVS), and efficacy endpoints.",
      "BDS-TTE": "Time-to-event subtype of BDS (for example, ADTTE).",
      "OCCDS": "Occurrence Data Structure for counts and frequency analyses such as adverse events (ADAE), concomitant medications (ADCM), and medical history (ADMH). ADAE used to have a separate structure file and is now included in OCCDS."
    };
    let html = '<div class="dataset-formats">';
    datasets.forEach(function (ds) {
      const val = String(formats[ds] || "");
      html += '<label>' + esc(ds) + ' <select data-dataset-format="' + esc(ds) + '"><option value=""' + (!val ? ' selected' : '') + '>Choose format</option>'
        + ["ADSL", "BDS", "BDS-TTE", "OCCDS"].map(function (format) { return '<option value="' + format + '"' + (val === format ? ' selected' : '') + '>' + format + '</option>'; }).join("")
        + '</select></label>';
    });
    html += '<div class="muted dataset-format-help">' + ["ADSL", "BDS", "BDS-TTE", "OCCDS"].map(function (format) {
      return '<p><b>' + format + ':</b> ' + esc(descriptions[format]) + '</p>';
    }).join("") + '</div><button type="button" class="pri sm" data-act="dataset-formats-save">Save dataset formats</button></div>';
    return html;
  }
  function missingNames() {
    const md = (S && S.missing_data) || {};
    const names = [];
    Object.keys(md).forEach(function (ds) {
      asArray(md[ds]).forEach(function (dom) { names.push(String(dom)); });
    });
    return names;
  }
  function legendHtml() {
    if (parameterMode()) return '<div class="legend"><span class="sw">Defined parameter</span><span class="sw param-carry-over">Carry-over</span><span class="sw param-derived">Derived</span><span class="sw param-missing">Mentioned, not defined</span><span class="sw param-user">User added edge</span></div>';
    return '<div class="legend"><span class="sw lv-none">Not generated</span><span class="sw lv-L2">Generated</span>'
      + '<span class="sw lv-L3">Data matched</span><span class="sw problem">Problem</span><span class="sw ready">Ready</span><span class="sw cyc">Cycle</span>'
      + '<span class="sw key">Key</span></div>';
  }
  function changedIds(st) {
    const v = st && st.upstream_changed;
    if (v == null || v === false || v === "") return [];
    if (typeof v === "string") return [v];
    if (Array.isArray(v)) {
      const out = [];
      v.forEach(function (x) { if (x) out.push(String(x)); });
      return out;
    }
    return [];
  }
  function resetAllowed(id) {
    if (!id) return false;
    const detail = ui.detail && ui.node === id ? ui.detail : null;
    const st = (detail && detail.status) || nodeState(id);
    if (st && (st.level === "L2" || st.level === "L3" || st.problem)) return true;
    const code = detail && detail.code;
    if (!code) return false;
    return !!(sideText(code, "primary").trim() || sideText(code, "validation").trim());
  }
  function resetToolbarHtml(id) {
    const busy = busyNow();
    const ok = !!(id && !busy && resetAllowed(id));
    let title = "";
    if (!ok) {
      if (!id) title = "Select a variable";
      else if (busy) title = "This variable is already running.";
      else title = "Nothing to reset";
    }
    return btn("reset", "Reset", { cls: "sm", disabled: !ok, title: title });
  }
  function lineageReviewContextHtml() {
    const review = ui.dagReview;
    if (!review || review.dataset !== ui.dataset) return "";
    const sourceRows = [];
    ["primary", "validation"].forEach(function (side) {
      const ctx = review.sourceContext && review.sourceContext[side] || {};
      asArray(ctx.requested).forEach(function (ref) {
        const supplied = asArray(ctx.supplied).indexOf(ref) >= 0;
        sourceRows.push('<li data-lineage-context-source="' + esc(ref) + '" data-context-side="' + side + '" data-supplied="' + supplied + '">' + esc(side) + ' source context · ' + esc(ref) + ' · ' + (supplied ? 'supplied' : 'not supplied') + '</li>');
      });
    });
    const target = review.targetId || review.node;
    return '<div class="lineage-review-context" aria-label="Lineage context"><span data-lineage-target="' + esc(target) + '">Target anchor · ' + esc(target) + '</span>'
      + (sourceRows.length ? '<ul>' + sourceRows.join("") + '</ul>' : '') + '</div>';
  }
  function dagSvg(graph) {
    const lay = layoutGraph(graph);
    lay._layerKey = graph && graph._layerKey || "";
    if (graph && graph._viewFilter) lay.viewFilter = graph._viewFilter;
    ui.layout = lay;
    const nodeById = {};
    asRecords(graph.nodes).forEach(function (n) { nodeById[n.id] = n; });
    const parameterClasses = {};
    if (graph && graph._paramDag) {
      const sourceGraph = curGraph();
      const classification = (graph.extraction && graph.extraction.parameter_classification)
        || (sourceGraph && sourceGraph.extraction && sourceGraph.extraction.parameter_classification);
      asRecords(classification && classification.parameters).forEach(function (item) {
        const code = String(item.paramcd || "").toUpperCase();
        const kind = String(item.classification || "");
        if (code && (kind === "carry_over" || kind === "derived")) parameterClasses[code] = kind === "carry_over" ? "param-carry-over" : "param-derived";
      });
      if (carryOverParameters(curGraph()).length) parameterClasses.CARRY_OVER = "param-carry-over";
    }
    let svg = '<svg id="dag" role="img" aria-label="Dependency graph"><defs>'
      + '<marker id="mk-n" viewBox="0 0 8 6" markerWidth="8" markerHeight="6" refX="7" refY="3" orient="auto"><path fill="#64748b" d="M0,0 L8,3 L0,6 Z"></path></marker>'
      + '<marker id="mk-c" viewBox="0 0 8 6" markerWidth="8" markerHeight="6" refX="7" refY="3" orient="auto"><path fill="#dc2626" d="M0,0 L8,3 L0,6 Z"></path></marker>'
      + '<marker id="mk-k" viewBox="0 0 8 6" markerWidth="8" markerHeight="6" refX="7" refY="3" orient="auto"><path fill="#1d4ed8" d="M0,0 L8,3 L0,6 Z"></path></marker>'
      + '</defs><g id="viewport">';
    if (lay.keyLane) {
      svg += '<text class="lane" x="' + lay.keyLane.x + '" y="' + lay.keyLane.y + '">Keys</text>';
    }
    lay.groups.forEach(function (g) {
      const st = groupState(g);
      const members = (g.members || []).join(",");
      svg += '<g class="gbox" data-group="' + esc(g.id) + '" data-members="' + esc(members) + '">'
        + '<rect x="' + g.x + '" y="' + g.y + '" width="' + g.w + '" height="' + g.h + '" rx="12"></rect>'
        + '<text x="' + (g.x + 10) + '" y="' + (g.y + 16) + '">' + esc((g.paramcd || g.id) + " · " + levelText(st)) + "</text></g>";
    });
    lay.edges.forEach(function (e) {
      if (e.bundle) {
        svg += '<path class="edge keybus" d="' + e.d + '" marker-end="url(#mk-k)"><title>'
          + esc(e.evidence) + "</title></path>";
        return;
      }
      const cls = ["edge"];
      if (e.origin === "user") cls.push("user-edge");
      if (e.cycle) cls.push("cycle");
      else if (ui.dagFlash && ui.dagFlash[e.from + "\t" + e.to]) cls.push("flash");
      else if (ui.node && (e.from === ui.node || e.to === ui.node)) cls.push("hot");
      const marker = e.cycle ? "mk-c" : "mk-n";
      svg += '<path class="' + cls.join(" ") + '" data-from="' + esc(e.from) + '" data-to="' + esc(e.to) + '" data-lane-offset="' + (e.laneOffset || 0) + '" d="' + e.d + '" marker-end="url(#' + marker + ')"><title>'
        + esc((e.cycle ? "Cycle " : "") + displayNode(e.from, true) + " → " + displayNode(e.to, true) + (e.usesTip ? " · " + e.usesTip : "") + (e.evidence ? " · " + e.evidence : "")) + "</title></path>";
      svg += '<path class="edge-hit" data-from="' + esc(e.from) + '" data-to="' + esc(e.to) + '" data-lane-offset="' + (e.laneOffset || 0) + '" d="' + e.d + '"></path>';
      if (e.usesLabel) {
        const a = lay.coords[e.from], b = lay.coords[e.to];
        svg += '<text class="edge-label" x="' + ((a.x + a.w + b.x) / 2) + '" y="' + (((a.y + b.y + 52) / 2) + (e.labelOffset || 0)) + '">' + esc(e.usesLabel) + '</text>';
      }
    });
    (lay.nodes || asRecords(graph.nodes)).forEach(function (n) {
      const c = lay.coords[n.id];
      if (!c) return;
      const st = nodeState(n.id);
      const cls = ["node"];
      if (graph._paramDag && n.defined === false) cls.push("undefined-param");
      if (graph._paramDag && parameterClasses[String(n.code || "").toUpperCase()]) cls.push(parameterClasses[String(n.code || "").toUpperCase()]);
      if (st.problem) cls.push("problem");
      else cls.push("lv-" + (st.level || "none"));
      if (st.ready) cls.push("ready");
      if ((ui.sel || []).indexOf(n.id) >= 0) cls.push("multi");
      if (ui.node === n.id) cls.push("sel");
      else if (ui.pin[n.id]) cls.push("pin");
      const label = graph._paramDag ? (asArray(n.members).length ? n.code + " (" + asArray(n.members).join(", ") + ")" : n.code) : (n.kind === "records" ? "Keys" : (graph._layered ? (n.var || n.id) : (n.paramcd ? n.var + "@" + n.paramcd : (n.var || n.id))));
      const changed = changedIds(st);
      const changedTip = changed.length ? ("Upstream " + changed.join(", ") + " changed after this was built") : "";
      const locked = graph._paramDag && n.defined === false;
      const lineageContext = n.lineageContext || {};
      const lineageType = n.lineageRole === "target" ? "Target anchor" : (n.lineageRole === "upstream"
        ? "Upstream input · " + ["primary", "validation"].map(function (side) {
          const role = lineageContext[side]; return side + " " + (role || "dependency only");
        }).join(" · ") : "");
      const mentionTip = locked ? " · Mentioned in " + asArray(n.mentioned_in).join(", ") : "";
      if (n.kind === "records") cls.push("records-node");
      if (n.kind === "dataset_variable") cls.push("dataset-input-node");
      svg += '<g class="' + cls.join(" ") + '"' + (locked ? ' aria-disabled="true" tabindex="-1"' : ' tabindex="0" role="button"') + ' data-id="' + esc(n.id) + '"' + (n.lineageRole ? ' data-lineage-role="' + esc(n.lineageRole) + '" data-lineage-context="' + esc(JSON.stringify(lineageContext)) + '" aria-label="' + esc(lineageType + ' · ' + n.id) + '"' : '') + ' data-defined="' + (!locked) + '" data-layer="' + (lay.layerOf[n.id] || 0)
        + '" data-x="' + Math.round(c.x) + '" data-y="' + Math.round(c.y) + '" transform="translate(' + c.x + " " + c.y + ')">'
        + "<title>" + esc((graph._paramDag ? (n.code + (n.label ? " · " + n.label : "")) : displayNode(n.id) + " · " + levelText(st)) + (lineageType ? " · " + lineageType : "") + (locked ? mentionTip : "") + (c.key ? " · Key" : "") + (st.ready ? " · Ready" : "") + (st.same_source ? " · Same source" : "") + (changedTip ? " · " + changedTip : "")) + "</title>"
        + '<rect class="shape" width="' + c.w + '" height="' + c.h + '" rx="8"></rect>'
        + '<text class="main" x="10" y="24">' + esc(trunc(label, st.same_source || c.key ? 12 : 18)) + "</text>";
      if (n.kind === "records") {
        asArray(n.keyLines).forEach(function (line, i) {
          svg += '<text class="key-list" x="10" y="' + (42 + i * 18) + '">' + esc(line) + '</text>';
        });
        svg += '<text class="sub record-status" x="10" y="' + (c.h - 6) + '">' + esc(levelText(st)) + '</text>';
      } else {
        svg += '<text class="sub" x="10" y="44">' + esc(graph._paramDag ? trunc(n.label || (locked ? "Undefined parameter" : ""), 25) : levelText(st)) + "</text>";
      }
      if (c.key) {
        const ky = st.same_source ? 44 : 8;
        svg += '<rect class="badge key" x="132" y="' + ky + '" width="36" height="15" rx="7"></rect>'
          + '<text class="badge key" x="150" y="' + (ky + 11) + '" text-anchor="middle">Key</text>';
      }
      if (st.same_source) {
        svg += '<rect class="badge" x="96" y="8" width="74" height="15" rx="7"></rect>'
          + '<text class="badge" x="133" y="19" text-anchor="middle">Same source</text>';
      }
      if (changedTip) {
        const cx = st.same_source ? 86 : (c.key ? 118 : (c.w - 12));
        svg += '<circle cx="' + cx + '" cy="12" r="5" fill="#d97706"><title>' + esc(changedTip) + "</title></circle>";
      }
      const specCats = asArray(n.categories);
      const specType = asRows(S && S.spec && S.spec.variables).find(function (r) {
        return r.Dataset === ui.dataset && r.Variable === n.var;
      });
      const categorical = n.categorical || specCats.length || (specType && /char|text|factor|logical|bool/i.test(String(specType.Type || "")));
      if (categorical) svg += '<g class="cat-toggle" data-category="' + esc(n.id) + '" role="button" aria-label="Show categories for ' + esc(n.id) + '">'
        + '<rect x="151" y="39" width="19" height="19" rx="4"></rect><text x="160.5" y="53" text-anchor="middle">▼</text></g>';
      svg += "</g>";
    });
    svg += "</g></svg>";
    return svg;
  }
  function categoryHtml() {
    const c = ui.category;
    if (!c || c.dataset !== ui.dataset || !ui.layout || !ui.layout.coords[c.node]) return "";
    const p = ui.layout.coords[c.node];
    let content = "Loading categories…";
    if (c.error) content = esc(c.error);
    else if (c.data) {
      if (c.data.overflow) content = "more than 20 distinct values, not listed.";
      else if (!asArray(c.data.rows).length) content = "No category values yet.";
      else content = '<table><thead><tr><th>Value</th><th>Rule</th><th>Primary n</th><th>Validation n</th></tr></thead><tbody>'
        + asArray(c.data.rows).map(function (r) {
          return '<tr' + (r.mismatch ? ' class="mismatch"' : "") + '><td>' + esc(r.value) + '</td><td>'
            + esc(r.rule || "") + '</td><td>' + (r.primary == null ? "" : esc(r.primary)) + '</td><td>'
            + (r.validation == null ? "" : esc(r.validation)) + "</td></tr>";
        }).join("") + "</tbody></table>";
    }
    return '<div class="category-pop" id="category-pop"><div class="category-title">' + esc(displayNode(c.node))
      + ' <button type="button" class="sm" data-act="category-close">Close</button></div>' + content + "</div>";
  }
  function positionCategory() {
    const pop = document.getElementById("category-pop");
    const c = ui.category && ui.layout && ui.layout.coords[ui.category.node];
    if (!pop || !c) return;
    const wrap = document.getElementById("dagwrap");
    pop.style.left = Math.max(4, Math.min(wrap.clientWidth - pop.offsetWidth - 4, ui.view.x + c.x * ui.view.k)) + "px";
    pop.style.top = Math.max(4, Math.min(wrap.clientHeight - pop.offsetHeight - 4, ui.view.y + (c.y + c.h) * ui.view.k + 5)) + "px";
  }
  function setSelection(ids) {
    const seen = {};
    ui.sel = [];
    (ids || []).forEach(function (id) {
      if (!id || seen[id]) return;
      seen[id] = true;
      ui.sel.push(id);
    });
  }
  function toggleSelection(id) {
    if (!id) return;
    const i = (ui.sel || []).indexOf(id);
    if (i >= 0) ui.sel.splice(i, 1);
    else ui.sel.push(id);
  }
  function addSelection(ids) {
    (ids || []).forEach(function (id) {
      if (id && (ui.sel || []).indexOf(id) < 0) ui.sel.push(id);
    });
  }
  function graphNodeIds() {
    return asRecords(curGraph() && curGraph().nodes).map(function (n) { return n.id; });
  }
  function applyQuick(which) {
    const ids = graphNodeIds();
    let next = [];
    if (which === "all") next = ids.slice();
    else if (which === "not-matched") {
      next = ids.filter(function (id) { return nodeState(id).level !== "L3"; });
    } else if (which === "problem") {
      next = ids.filter(function (id) { return !!nodeState(id).problem; });
    } else if (which === "new") {
      next = ids.filter(function (id) {
        const level = nodeState(id).level;
        return level !== "L2" && level !== "L3";
      });
    }
    setSelection(next);
    ui.selectOpen = false;
    render();
  }
  function selectMenuHtml() {
    const picks = ui.selectOpen
      ? '<span class="picks">'
        + btn("select-quick", "All", { cls: "sm", data: { which: "all" } })
        + btn("select-quick", "Not data matched", { cls: "sm", data: { which: "not-matched" } })
        + btn("select-quick", "Problem", { cls: "sm", data: { which: "problem" } })
        + btn("select-quick", "Not generated yet", { cls: "sm", data: { which: "new" } })
        + btn("select-quick", "Clear", { cls: "sm", data: { which: "clear" } })
        + "</span>"
      : "";
    const mode = btn("multi-mode", ui.multiMode ? "Done selecting" : "Multi-select", {
      cls: ui.multiMode ? "sm on" : "sm",
      title: ui.multiMode ? "Clicks go back to opening one variable" : "Then click variables one by one to add or remove them"
    });
    if (!ui.multiMode) return mode;
    return mode + '<span class="muted hint-inline">Click variables to add or remove them.</span>'
      + btn("select-menu", "Quick pick", { cls: ui.selectOpen ? "sm on" : "sm" }) + picks;
  }
  function selectionBarHtml() {
    const n = (ui.sel || []).length;
    if (!n) return "";
    const running = batchRunning();
    const on = ui.compareAfter !== false;
    return '<span class="pill">' + n + " selected</span>"
      + '<label class="chk"><input id="batch-compare" type="checkbox"' + (on ? " checked" : "") + "> Compare after generating</label>"
      + btn("batch-go", "Generate selected", { cls: "pri", disabled: running || !ui.dataset || isDatasetExtracting(ui.dataset), title: isDatasetExtracting(ui.dataset) ? "Dependencies are being extracted for this dataset." : (running ? "A batch is already running." : "") })
      + btn("select-quick", "Clear", { cls: "sm", data: { which: "clear" } });
  }
  function extractionJob(dataset) {
    return asRecords(S && S.busy).filter(function (job) {
      return job && (job.kind === "extract" || job.kind === "param_extract") && String(job.dataset || "") === String(dataset || "");
    })[0] || null;
  }
  function paramExtractionJob(dataset) { return asRecords(S && S.busy).filter(function (j) { return j && j.kind === "param_extract" && String(j.dataset || "") === String(dataset || ""); })[0] || null; }
  function isDatasetExtracting(dataset) { return !!extractionJob(dataset); }
  function extractionStartTime(job) {
    const elapsed = Number(job && job.elapsed);
    if (isFinite(elapsed) && elapsed >= 0) return Date.now() - elapsed * 1000;
    if (!job || job.started == null || job.started === "") return Date.now();
    const raw = job.started;
    const numeric = Number(raw);
    const parsed = isFinite(numeric) ? (numeric < 1000000000000 ? numeric * 1000 : numeric) : Date.parse(raw);
    return isFinite(parsed) ? parsed : Date.now();
  }
  function extractError(dataset) {
    const err = S && S.last_error;
    if (!err || err.kind !== "extract" || String(err.dataset || "") !== String(dataset || "")) return null;
    const seen = ui.extractJobs[dataset];
    if (seen && seen.ignoreErrorTime != null && Number(err.time) === Number(seen.ignoreErrorTime)) return null;
    return err;
  }
  function extractionElapsed() {
    const job = extractionJob(ui.dataset);
    if (!job) return null;
    if (!ui.extractJobs[ui.dataset] || !ui.extractJobs[ui.dataset].active) {
      ui.extractJobs[ui.dataset] = { seenAt: extractionStartTime(job), active: true, ignoreErrorTime: null };
    }
    return Math.max(0, Math.floor((Date.now() - ui.extractJobs[ui.dataset].seenAt) / 1000));
  }
  function formatElapsed(seconds) {
    const whole = Math.max(0, Math.floor(Number(seconds) || 0));
    return Math.floor(whole / 60) + ":" + String(whole % 60).padStart(2, "0");
  }
  function updateExtractElapsed() {
    const el = document.getElementById("extract-elapsed");
    if (!el || !isDatasetExtracting(ui.dataset)) return;
    el.textContent = "Elapsed: " + formatElapsed(extractionElapsed());
  }
  function syncExtractTimer() {
    const job = extractionJob(ui.dataset);
    if (job) {
      extractionElapsed();
      if (!ui.extractTimer) ui.extractTimer = setInterval(updateExtractElapsed, 1000);
      return;
    }
    const previous = ui.extractJobs[ui.dataset];
    if (previous && previous.active) {
      previous.active = false;
      const err = S && S.last_error;
      if (err && err.kind === "extract" && String(err.dataset || "") === String(ui.dataset || "")
          && Number(err.time) * 1000 <= previous.seenAt) previous.ignoreErrorTime = err.time;
    }
    if (ui.extractTimer) { clearInterval(ui.extractTimer); ui.extractTimer = null; }
  }
  function extractionPanelHtml(error) {
    const running = isDatasetExtracting(ui.dataset);
    const elapsed = extractionElapsed();
    const job = extractionJob(ui.dataset);
    const layer = job && job.kind === "extract" && String(job.node || "").trim();
    const heading = layer ? "Generating " + (String(job.node).indexOf(",") >= 0 ? "Carry-over Group (" + job.node + ")" : "Variable DAG for " + job.node)
      : job && job.kind === "param_extract" ? "Extracting parameter dependencies for " + (ui.dataset || "this dataset")
      : "Extracting dependencies for " + (ui.dataset || "this dataset");
    if (running) return '<section class="extract-wait" aria-live="polite"><span class="extract-spinner" aria-hidden="true"></span>'
      + '<h2>' + esc(heading) + '</h2>'
      + '<p>Still working, not stuck.</p><p id="extract-elapsed">Elapsed: ' + formatElapsed(elapsed) + '</p></section>';
    if (error) return '<section class="extract-wait extract-failed" role="alert"><h2>Dependency extraction failed for ' + esc(ui.dataset || "this dataset") + '</h2>'
      + '<p>' + esc(error.message || "The dependency extraction failed.") + '</p>'
      + btn("extract", "Extract", { cls: "pri sm" }) + '</section>';
    return "";
  }
  function parameterClassificationHtml(graph) {
    const classification = graph && graph.extraction && graph.extraction.parameter_classification;
    if (!classification || !asRecords(classification.parameters).length) return "";
    const records = asRecords(classification.parameters);
    const issues = asArray(classification.issues).map(function (issue) { return '<p class="warn">Spec issue: ' + esc(issue) + '</p>'; }).join("");
    const carryCount = records.filter(function (r) { return String(r.classification || "") === "carry_over"; }).length;
    const derivedCount = records.filter(function (r) { return String(r.classification || "") === "derived"; }).length;
    const selected = String(ui.layerChoice[ui.dataset] || (asRecords(graph.layers)[0] || {}).paramcd || "").toUpperCase();
    const carryCodes = records.filter(function (r) { return String(r.classification || "") === "carry_over"; })
      .map(function (r) { return String(r.paramcd || "").toUpperCase(); });
    const carrySelected = carryCodes.length > 0 && (selected === "CARRY_OVER" || carryCodes.indexOf(selected) >= 0);
    const readOnly = parameterMode();
    function group(name, title) {
      const rows = records.filter(function (r) { return String(r.classification || "") === name; });
      if (!rows.length) return "";
      const className = name === "carry_over" ? "param-carry-over" : "param-derived";
      if (name === "carry_over") {
        const codes = rows.map(function (r) { return String(r.paramcd || "").toUpperCase(); });
        const active = carrySelected;
        const contents = '<b>Carry-over (' + esc(codes.join(", ")) + ')</b><ul>' + rows.map(function (r) {
          return '<li><b>' + esc(r.paramcd || "") + '</b>: ' + esc(r.reason || "") + '</li>';
        }).join("") + '</ul>';
        return '<section class="param-classification-group ' + className + '"><h3>Carry-over</h3>'
          + (readOnly ? '<div class="param-group-card is-readonly">' + contents + '</div>'
            : '<button type="button" class="param-group-card' + (active ? ' selected' : '') + '" data-act="layer-select" data-layer="CARRY_OVER" aria-pressed="' + active + '">' + contents + '</button>')
          + '</section>';
      }
      return '<section class="param-classification-group ' + className + '"><h3>' + title + '</h3><ul>' + rows.map(function (r) {
        const code = String(r.paramcd || "").toUpperCase(), active = selected === code;
        return '<li>' + (readOnly ? '<div class="param-derived-row is-readonly"><b>' + esc(code) + '</b>: ' + esc(r.reason || "") + '</div>'
          : '<button type="button" class="param-derived-row' + (active ? ' selected' : '') + '" data-act="layer-select" data-layer="' + esc(code) + '" aria-pressed="' + active + '"><b>' + esc(code) + '</b>: ' + esc(r.reason || "") + '</button>') + '</li>';
      }).join("") + '</ul></section>';
    }
    const open = !ui.parameterClassificationCollapsed;
    const selectedLabel = carrySelected ? "Carry-over (" + carryCodes.join(", ") + ")" : (selected || "None");
    return '<section class="param-classification' + (open ? ' is-open' : ' is-collapsed') + '" aria-label="Parameter classification">'
      + '<div class="param-classification-summary"><span>Carry-over: ' + carryCount + ' · Derived: ' + derivedCount + ' · Selected: ' + esc(selectedLabel) + '</span>'
      + '<button type="button" class="sm" data-act="classification-toggle" aria-expanded="' + (open ? 'true' : 'false') + '">' + (open ? 'Collapse' : 'Expand') + '</button></div>'
      + (open ? '<div class="param-classification-details">' + group("carry_over", "Carry-over") + group("derived", "Derived") + issues + '</div>' : '<div class="param-classification-issues">' + issues + '</div>')
      + '</section>';
  }
  function carryOverParameters(graph) {
    const classification = graph && graph.extraction && graph.extraction.parameter_classification;
    return asRecords(classification && classification.parameters)
      .filter(function (item) { return String(item.classification || "") === "carry_over"; })
      .map(function (item) { return String(item.paramcd || "").toUpperCase(); }).filter(Boolean);
  }
  function centerHtml() {
    const names = dsNames();
    const tabs = names.map(function (d) {
      return btn("dataset", d, { cls: d === ui.dataset ? "sm on" : "sm", data: { ds: d } });
    }).join("");
    const g = curGraph();
    const parameter = parameterMode();
    const pg = curParamGraph();
    const variableViewGraph = !parameter && g ? graphForView() : g;
    const hasParams = asRows(S && S.spec && S.spec.parameters).some(function (r) { return String(r.Dataset || "") === String(ui.dataset || ""); });
    const serverParamBusy = !!paramExtractionJob(ui.dataset);
    const actionWait = ui.dagActionWait && String(ui.dagActionWait.dataset || "") === String(ui.dataset || "") ? ui.dagActionWait : null;
    const paramBusy = serverParamBusy || !!(actionWait && actionWait.kind === "param-extract");
    const extracting = isDatasetExtracting(ui.dataset);
    const undoInfo = !parameter ? dagUndoInfo() : { count: 0, label: "" };
    const extractionFailure = !extracting && (ui.extractErrors[ui.dataset]
      ? { message: ui.extractErrors[ui.dataset] } : extractError(ui.dataset));
    const miss = missingNames();
    const missHint = miss.length
      ? '<p class="hint">Missing xpt: ' + esc(miss.slice(0, 4).join(", ")) + (miss.length > 4 ? ", and more" : "") + ". Upload data in Settings.</p>"
      : "";
    let body;
    if (parameter) {
      if (paramBusy) body = '<section class="extract-wait" aria-live="polite"><span class="extract-spinner" aria-hidden="true"></span><h2>Extracting parameter dependencies for ' + esc(ui.dataset || "this dataset") + '</h2><p>Still working, not stuck.</p><button type="button" class="pri" data-act="param-extract" disabled>Extract parameter dependencies</button>' + (serverParamBusy ? '<p id="extract-elapsed">Elapsed: ' + formatElapsed(extractionElapsed()) + '</p>' : '') + '</section>';
      else if (!pg || !(pg.nodes || []).length) body = '<div class="empty">No parameter graph yet. ' + btn("param-extract", "Extract parameter dependencies", { cls: "pri" }) + (pg && pg.error ? '<p class="bad">Error: ' + esc(pg.error) + '</p>' : "") + (pg && pg.stale ? '<p class="warn">This parameter graph is stale.</p>' : "") + (pg && asArray(pg.warnings).length ? '<p class="warn">Warnings: ' + esc(asArray(pg.warnings).join("; ")) + '</p>' : "") + (ui.paramErrors[ui.dataset] ? '<p class="bad">' + esc(ui.paramErrors[ui.dataset]) + '</p>' : "") + '</div>';
      else body = '<div class="param-status">' + (pg.error ? '<p class="bad">Error: ' + esc(pg.error) + '</p>' : "") + (pg.stale ? '<p class="warn">This parameter graph is stale.</p>' : "") + (asArray(pg.warnings).length ? '<p class="warn">Warnings: ' + esc(asArray(pg.warnings).join("; ")) + '</p>' : "") + '</div><div id="dagwrap">' + dagSvg(normalizeParamGraph(pg)) + '</div>';
    } else if (actionWait && actionWait.kind === "layer-generate") {
      body = '<section class="extract-wait" aria-live="polite"><span class="extract-spinner" aria-hidden="true"></span><h2>' + esc(actionWait.label) + '</h2><p>Still working, not stuck.</p></section>';
    } else if (extracting || extractionFailure) {
      body = extractionPanelHtml(extractionFailure);
    } else if (!S.spec && !names.length) {
      body = '<div class="empty">No spec yet. Upload variable.csv in Settings. '
        + btn("tab", "Settings", { cls: "pri", data: { tab: "settings" } }) + "</div>";
    } else if (!g) body = '<div class="empty">No dependency graph yet. ' + btn("extract", "Extract dependencies", { cls: "pri" }) + "</div>";
    else {
      body = variableViewGraph._viewFilter && !asRecords(variableViewGraph.nodes).length
        ? '<div class="empty">No graph variables for this view.</div>'
        : lineageReviewContextHtml() + '<div id="dagwrap">' + dagSvg(variableViewGraph) + categoryHtml() + dagContextMenuHtml() + '</div>';
    }
    const tools = '<div class="ph"><div class="tabs">' + (tabs || '<span class="muted">No dataset</span>') + "</div>"
      + (SHOW_DAG_MULTI_SELECT && !parameter && g ? selectMenuHtml() : "")
      + (SHOW_DAG_MULTI_SELECT && !parameter ? selectionBarHtml() : "")
      + (ui.dataset && parameter && pg && (pg.nodes || []).length ? btn("start-new-dag", "Reset", { cls: "sm danger", disabled: paramBusy }) : "")
      + (ui.dataset && parameter && pg && (pg.nodes || []).length ? btn("network-download", "Download network JSON", { cls: "sm" }) : "")
      + (!hasParams && ui.dataset && g ? btn("extract", "Regenerate DAG", { cls: "sm", disabled: extracting }) : "")
      + btn("zoom", "Zoom in", { cls: "sm", data: { k: "1.12" } })
      + btn("zoom", "Zoom out", { cls: "sm", data: { k: "0.89" } })
      + (!parameter && g ? paramFilterHtml() : "")
      + (ui.dataset && !parameter && g ? btn("network-download", "Download network JSON", { cls: "sm" }) : "")
      + (ui.dagReview && ui.dagReview.dataset === ui.dataset ? '<span class="muted">Reviewing ' + esc(displayNode(ui.dagReview.node)) + '</span>' : "")
      + (hasParams && !parameter && pg && (pg.nodes || []).length ? (function () {
        const selected = String(ui.layerChoice[ui.dataset] || (asRecords(g && g.layers)[0] || {}).paramcd || "").toUpperCase();
        const carry = carryOverParameters(g);
        const group = selected === "CARRY_OVER" || carry.indexOf(selected) >= 0;
        const generated = asArray(g && g.generated_parameters);
        const groupGenerated = generated.indexOf("CARRY_OVER") >= 0 || carry.some(function (pc) { return generated.indexOf(pc) >= 0; });
        const label = group ? (groupGenerated ? "Regenerate Carry-over Group (" : "Generate Carry-over Group (") + carry.join(", ") + ")" : (generated.indexOf(selected) >= 0 ? "Regenerate " : "Generate ") + selected + " Variable DAG";
        const running = !!(actionWait && actionWait.kind === "layer-generate");
        return btn("layer-generate", label, { cls: "pri sm", disabled: running || serverParamBusy });
      })() : "")
      + "</div>";
    if (hasParams && !parameter && (!pg || !(pg.nodes || []).length) && !extracting && !actionWait) body = '<div class="empty">Generate the Parameter DAG first.</div>';
    body = '<div class="dag-canvas-row">' + (!parameter ? runQueueHtml() : "") + '<div class="dag-canvas-main">' + body + '</div>' + (hasParams ? parameterClassificationHtml(g) : "") + '</div>';
    if (g && g.carry_over_regenerate_required && !parameter) body = '<p class="warn carry-over-regenerate" role="status">This saved DAG uses the old per-member carry-over format. Regenerate the CARRY_OVER group to continue.</p>' + body;
    return graphSwitchHtml(hasParams) + tools + '<div class="ph legend-ph">' + legendHtml() + "</div>" + missHint
      + (!parameter ? cycleCutsHtml(g) : "") + body;
  }
  function busyNow() {
    return !!(ui.running || ui.inflight || asRecords(S && S.busy).some(function (job) { return job && job.kind !== "param_extract" && job.kind !== "param_ask"; }));
  }
  function hasStored(side) {
    return !!sideText(ui.detail && ui.detail.code, side).trim();
  }
  function pane(key, title, extra, body) {
    const open = !folded(key);
    const sid = key === "spec" ? ' id="spec-scroll"' : "";
    return '<section class="pane' + (open ? "" : " fold") + '"><div class="ph"><b>' + esc(title) + "</b>"
      + (open ? (extra || "") : "")
      + btn("fold", open ? "Collapse" : "Expand", { cls: "sm", data: { pane: key } })
      + "</div>" + (open ? '<div class="scroll" data-scroll-key="pane-' + esc(key) + '"' + sid + ">" + body + "</div>" : "") + "</section>";
  }
  function scriptJumps(script) {
    const jumps = [];
    const re = /^# ==== (\S+) \(([^)]*)\) ====$/;
    String(script || "").split("\n").forEach(function (line, i) {
      const m = re.exec(line);
      if (m) jumps.push({ line: i + 1, id: m[1], status: m[2] });
    });
    return jumps;
  }
  function lineView(text, jumps) {
    const lines = String(text || "").replace(/\n$/, "").split("\n");
    if (!lines.length) lines.push("");
    const at = {};
    (jumps || []).forEach(function (j) { at[j.line] = j; });
    return '<div class="codeview lines">' + lines.map(function (line, i) {
      const n = i + 1;
      const jump = at[n];
      const body = jump
        ? '<button type="button" class="codejump" data-act="code-jump" data-id="' + esc(jump.id) + '">' + esc(line) + "</button>"
        : '<span class="codeline">' + (line === "" ? " " : esc(line)) + "</span>";
      return '<span class="ln">' + n + "</span>" + body;
    }).join("") + "</div>";
  }
  function codeGuideHtml() {
    return '<details id="code-guide" class="guide"' + (ui.codeGuide ? " open" : "") + '><summary>How this page works</summary>'
      + "<p>Each side is generated independently. The other side's code and results are not sent to the model.</p>"
      + "<p>Generate &amp; run writes and runs both programs. Re-run executes the saved code and does not call the model. Compare checks that the two results match.</p>"
      + "<p>Nodes are Not generated until both programs have run. They are Generated after both runs succeed, and Data matched when the results agree.</p>"
      + "<p>Use the Coder window to ask about the code or request a change.</p>"
      + "</details>";
  }
  function bindCodeGuide() {
    const el = document.getElementById("code-guide");
    if (!el) return;
    el.addEventListener("toggle", function () {
      ui.codeGuide = !!el.open;
      try { localStorage.setItem("dualdag.codeGuide", el.open ? "1" : "0"); } catch (err) { /* private mode */ }
    });
  }
  function codeView(side) {
    const whole = ui.codeScope === "all";
    let text = "";
    let jumps = [];
    if (whole) {
      const bucket = ui.program && ui.program[side];
      text = (bucket && bucket.script) || "";
      jumps = scriptJumps(text);
    } else {
      text = sideText(ui.detail && ui.detail.code, side);
    }
    const trimmed = String(text || "").trim();
    const busy = busyNow();
    const why = !ui.node ? "Select a variable first" : (busy ? "Busy." : (!trimmed ? "No saved code" : ""));
    const can = !whole && !!ui.node && !!trimmed && !busy;
    const head = '<div class="ph"><b>' + side + " · " + esc(displayNode(ui.node)) + ((ui.detail && ui.detail.su_edited && ui.detail.su_edited[side]) ? ' <span class="su-pill">SU</span>' : '') + "</b>"
      + (whole ? "" : btn("rerun", "Re-run", { cls: "sm", disabled: !can, data: { side: side }, title: can ? "Runs the saved code" : why }))
      + (!whole && why ? '<p class="hint">' + esc(why) + "</p>" : "")
      + (whole ? "" : btn("coder-window", "Coder window", { cls: "sm", disabled: !ui.node, data: { side: side }, title: "View this side's latest context and process" }))
      + "</div>";
    const body = !trimmed
      ? '<p class="muted empty">' + (whole ? (ui.programErr || "No saved code on this side") : "Not generated") + "</p>"
      : lineView(text, jumps);
    const log = sideText(ui.detail && ui.detail.log, side);
    const explainBtn = btn("explain", "Explain", {
      cls: "sm", disabled: busy || !ui.node, data: { side: side },
      title: busy ? "Busy." : "Explain this side's code and log"
    });
    const renderedLog = renderConsoleLog(log || (ui.running ? "Running…" : "No log yet"));
    const logActs = '<span class="logacts">' + explainBtn + "</span>";
    const hit = !whole && ui.fromSpec ? " hit" : "";
    const pathWarn = specPathWarningHtml(side);
    return '<section class="codeside" data-scroll-key="code-side-' + side + '">' + head + '<div class="code-body-split">'
      + '<div class="code-area">' + pathWarn + (whole ? "" : reviewHtml(side))
      + (trimmed ? body.replace('class="codeview lines"', 'class="codeview lines' + hit + '" data-scroll-key="code-view-' + side + '"') : body) + '</div>'
      + (whole ? "" : '<div class="log-area"><div class="loghead"><span>Run log</span><span class="logbadges" data-side="' + side + '">' + logBadges(side, renderedLog) + '</span>' + logActs + "</div>"
        + '<pre class="log" data-scroll-key="log-' + side + '" data-side="' + side + '">' + renderedLog.html + "</pre></div>")
      + "</div></section>";
  }
  function coderWindowHtml() {
    const w = ui.coderWindow;
    if (!w) return "";
    const d = w.data || {};
    const context = d.context;
    const contexts = asArray(d.contexts);
    const selectedAttemptIndex = Number(w.selectedAttempt || d.context_index || 0) - 1;
    const followingAttempt = contexts[selectedAttemptIndex + 1];
    const lookupContinues = !!(context && followingAttempt && followingAttempt.task === "lookup round"
      && followingAttempt.parent_task === context.task);
    const contextFinalReply = !!(context && (context.final_reply === true
      || (context.final_reply !== false && context.task === "lookup round" && String(context.approach || "").trim())
      || (context.final_reply !== false && context.task !== "lookup round" && !lookupContinues)));
    const events = asArray(d.process);
    const thread = asArray(d.thread);
    const busy = !!d.running || !!w.sending;
    const fmtTime = function (value) {
      if (!value) return "";
      const date = new Date(value);
      if (isNaN(date.getTime())) return "";
      return [date.getHours(), date.getMinutes(), date.getSeconds()].map(function (n) {
        return String(n).padStart(2, "0");
      }).join(":");
    };
    const detailText = function (value) {
      if (value == null || value === "") return "";
      return typeof value === "string" ? value : JSON.stringify(value, null, 2);
    };
    const timeline = [];
    const starts = {};
    let runLog = "";
    const hasReviewPair = events.some(function (e) { return e.event === "Self-review started"; })
      && events.some(function (e) { return e.event === "Self-review finished"; });
    const hasRewritePair = events.some(function (e) { return e.event === "Rewrite started"; })
      && events.some(function (e) { return e.event === "Rewrite finished"; });
    const hasChatPair = events.some(function (e) { return e.event === "Chat started"; })
      && events.some(function (e) { return e.event === "Chat finished"; });
    const startKind = function (event) {
      return ({ "Model call started": "model", "Program run started": "run",
        "Self-review started": "review", "Rewrite started": "rewrite",
        "Chat started": "chat", "Chat retry started": "chat-retry" })[event] || "";
    };
    const finishKind = function (event) {
      return ({ "Model call finished": "model", "Program run finished": "run",
        "Self-review finished": "review", "Rewrite finished": "rewrite",
        "Chat finished": "chat", "Chat retry finished": "chat-retry" })[event] || "";
    };
    const addTimeline = function (event, text, start, end, duration) {
      timeline.push({ at: (start && start.at) || (end && end.at) || "", text: text,
        duration: duration == null ? null : duration, order: timeline.length, event: event });
    };
    events.forEach(function (event) {
      const name = String(event.event || "");
      const detail = detailText(event.detail);
      if (name === "Run log") { runLog = detail; return; }
      const kind = startKind(name);
      if (kind) {
        (starts[kind] = starts[kind] || []).push(event);
        return;
      }
      const finished = finishKind(name);
      if (finished) {
        const queue = starts[finished] || [];
        let startIndex = queue.length - 1;
        if (finished === "model" && detail) {
          for (let i = 0; i < queue.length; i++) {
            if (detailText(queue[i].detail) === detail) { startIndex = i; break; }
          }
        }
        const start = startIndex >= 0 ? queue.splice(startIndex, 1)[0] : null;
        const a = start ? Date.parse(start.at) : NaN, b = Date.parse(event.at);
        const duration = isFinite(a) && isFinite(b) ? Math.max(0, Math.round((b - a) / 1000)) : null;
        let text = "";
        if (finished === "model") {
          const task = detail || detailText(start && start.detail);
          if ((task === "self-review" && hasReviewPair) || (task === "rewrite" && hasRewritePair)
              || (task === "chat" && hasChatPair)) return;
          if (task === "generation") text = "Wrote the program";
          else if (task === "rewrite") text = "Revised the program";
          else if (task === "self-review") text = "Reviewed the result";
          else if (task === "chat") text = "Replied to your message";
          else if (task === "lookup round") text = "Looked up additional context";
          else text = "Model call finished" + (task ? ": " + task : "");
        } else if (finished === "run") text = "Ran the program";
        else if (finished === "review") text = "Reviewed the result" + (detail ? ": " + detail : "");
        else if (finished === "rewrite") text = "Revised the program";
        else if (finished === "chat") text = "Replied to your message";
        else if (finished === "chat-retry") text = "Retried the reply";
        addTimeline(name, text, start, event, duration);
        return;
      }
      if (name === "Generation started") return;
      if (name === "Run error") {
        const lastRun = timeline.slice().reverse().find(function (row) { return row.event === "Program run finished"; });
        if (lastRun) lastRun.text = "Run failed: " + (detail || "Program failed.");
        else addTimeline(name, "Run failed: " + (detail || "Program failed."), event, event, null);
        return;
      }
      if (name === "Final result") {
        addTimeline(name, "Finished: " + (detail || "complete"), event, event, null);
        return;
      }
      addTimeline(name, name + (detail ? ": " + detail : ""), event, event, null);
    });
    const process = timeline.map(function (row) {
      const time = fmtTime(row.at);
      const suffix = row.duration == null ? "" : " (" + row.duration + "s)";
      return '<li>' + (time ? '<time>' + esc(time) + '</time>' : "")
        + '<span>' + esc(row.text + suffix) + '</span></li>';
    }).join("");
    const runLogText = runLog || sideText(ui.detail && ui.detail.log, w.side);
    const attemptSelect = contexts.length ? '<label class="coder-attempt-label">Attempt <select id="coder-context-attempt">'
      + contexts.map(function (a, i) { return '<option value="' + (i + 1) + '"' + ((w.selectedAttempt || d.context_index) === i + 1 ? ' selected' : '') + '>'
        + esc(a.task || "model call") + (a.at ? ' · ' + esc(a.at) : "") + '</option>'; }).join("")
      + '</select></label>' : "";
    const displayThread = thread.slice();
    const lastTurn = displayThread.length ? displayThread[displayThread.length - 1] : null;
    const reflected = lastTurn && thread.length > Number(w.optimisticThreadLength || 0)
      && String(lastTurn.message || lastTurn.question || "") === String(w.optimisticMessage || "");
    if (w.optimisticMessage && !reflected) displayThread.push({ message: w.optimisticMessage, pending: true });
    const replyHtml = function (reply) {
      const text = String(reply || "");
      const fence = /```[^\n`]*\n?([\s\S]*?)```/g;
      let out = "", at = 0, match;
      while ((match = fence.exec(text))) {
        if (match.index > at) out += '<span class="coder-reply-text">' + esc(text.slice(at, match.index)) + '</span>';
        out += '<pre class="coder-code"><code>' + esc(match[1].replace(/\n$/, "")) + '</code></pre>';
        at = fence.lastIndex;
      }
      if (at < text.length || !out) out += '<span class="coder-reply-text">' + esc(text.slice(at)) + '</span>';
      return out;
    };
    const conversation = displayThread.map(function (turn, i) {
      const userText = turn.message || turn.question || "";
      const reply = turn.reply || turn.answer || "";
      const update = turn.code_updated ? '<div class="coder-code-updated">Code updated'
        + (turn.compare_status ? ' · ' + esc(turn.compare_status) : "") + '</div>' : "";
      return '<article class="coder-turn" data-scroll-key="coder-turn-' + i + '">'
        + '<div class="coder-message coder-user"><b>You</b><div class="coder-bubble coder-user-bubble">' + esc(userText) + '</div></div>'
        + (reply ? '<div class="coder-message coder-assistant"><b>Coder</b><div class="coder-bubble coder-reply">'
          + replyHtml(reply) + update + '</div>' + contextUsageLine(turn.usage) + '</div>' : "")
        + '</article>';
    }).join("");
    const chatBody = conversation + (busy ? '<div class="coder-working"><span class="coder-working-dot"></span>Coder is working…</div>' : "");
    const details = '<div class="coder-details-scroll" data-scroll-key="coder-details-scroll"><section><h3>Context</h3>' + attemptSelect
      + (context ? ((contextFinalReply && String(context.system || "").indexOf('"approach"') >= 0) ? (String(context.approach || "").trim()
          ? '<h4>Approach</h4><p>' + esc(context.approach) + '</p>'
          : '<p class="muted">No approach was returned for this generation.</p>') : '')
        + '<h4>Instructions sent to the coder</h4><pre data-scroll-key="coder-prompt-system">' + esc(context.system || "")
        + '</pre><h4>Data and task sent to the coder</h4><pre data-scroll-key="coder-prompt-user">' + esc(context.user || "") + '</pre>'
        : '<p class="muted">No context stored yet.</p>')
      + '</section><section><h3>What happened</h3>'
      + (process ? '<ul class="coder-timeline">' + process + '</ul>' : '<p class="muted">No process stored yet.</p>')
      + '<details class="coder-run-log"><summary>Run log</summary><pre>' + esc(runLogText || "No run log yet.") + '</pre></details>'
      + '</section></div>';
    const textarea = '<textarea id="coder-window-message" rows="1" placeholder="Ask or request a code change">' + esc(w.draft || "") + '</textarea>';
    const sendButton = btn("coder-send", "Send", { cls: "pri sm", disabled: busy || !String(w.draft || "").trim() });
    const suControls = '<label class="su-switch"><input type="checkbox" id="coder-su-switch"' + (w.suMode ? " checked" : "") + (busy || d.running ? " disabled" : "") + '> SU mode</label>';
    const suBanner = w.suMode ? '<div class="su-banner"><b>!</b> SU mode: sees all spec and data. Edits this side only.</div>' : "";
    const coderLineageWarnings = asArray(d.status && d.status.warnings).filter(function (m) { return /^Lineage warning: /.test(String(m)); });
    const coderLineageWarningHtml = coderLineageWarnings.length ? '<div class="warn specpath"><ul>' + coderLineageWarnings.map(function (m) { return '<li>' + esc(m) + '</li>'; }).join("") + '</ul></div>' : "";
    const visibleThread = w.suMode ? asArray(w.suMessages).map(function (m) {
      if (m.kind === "tool") return '<div class="su-tool-line">' + esc(m.text) + '</div>';
      return '<article class="coder-turn"><div class="coder-message coder-' + (m.role === "assistant" ? 'assistant' : 'user') + '"><b>' + (m.role === "assistant" ? 'SU' : 'You') + '</b><div class="coder-bubble ' + (m.role === "assistant" ? 'coder-reply' : 'coder-user-bubble') + '">' + esc(m.text) + '</div>' + (m.role === "assistant" ? contextUsageLine(m.usage) : '') + '</div></article>';
    }).join("") : conversation;
    return '<div class="coder-overlay" role="dialog" aria-modal="false" aria-label="Coder window">'
      + '<div class="coder-overlay-head"><b>Coder window · ' + esc(w.side) + ' · ' + esc(displayNode(w.node)) + (d.su_edited ? ' <span class="su-pill">SU</span>' : '') + '</b>'
      + ((w.suMode && w.suUsage) || d.usage_total ? '<span class="muted session-usage">' + esc(sessionUsageLabel(w.suMode ? w.suUsage : d.usage_total)) + '</span>' : '')
      + (d.session_id ? '<button type="button" class="sm quiet" data-act="copy-session" data-session="' + esc(d.session_id) + '" title="Copy session ID">Session ' + esc(d.session_id) + '</button>' : '')
      + '<div class="coder-head-actions">' + suControls + btn("expert-clear", "Clear", { cls: "sm quiet", disabled: busy || !thread.length,
        data: { side: w.side }, title: "Clears this side's conversation memory" })
      + '<button type="button" class="sm quiet" data-act="coder-details-toggle" aria-expanded="' + (w.detailsOpen ? "true" : "false") + '">'
      + (w.detailsOpen ? "Conversation" : "Details") + '</button>'
      + '<button type="button" class="coder-close" data-act="coder-window-close" aria-label="Close">×</button></div></div>'
      + (w.error ? '<p class="bad">' + esc(w.error) + '</p>' : "")
      + suBanner + coderLineageWarningHtml + '<div class="coder-overlay-content" data-scroll-key="coder-overlay-content">'
      + (w.suMode ? '<div class="coder-conversation">' + (visibleThread || '<p class="coder-empty">Ask SU about this side.</p>') + (busy ? '<div class="coder-working">SU is working…</div>' : '') + '</div>' : (w.detailsOpen ? details : '<div class="coder-conversation" data-scroll-key="coder-conversation">'
        + (chatBody || '<p class="coder-empty">Ask about this variable\'s code or request a change.</p>') + '</div>'))
      + '</div><div class="coder-composer">' + (w.suMode ? '<b class="su-mark">!</b>' : '') + textarea + sendButton + '</div></div>';
  }
  const SUPER_TABS = { spec: "Spec & checks", run: "Run", data: "Data", mismatches: "Mismatches", settings: "Settings" };
  function superKey(tab) { return "dualdag.super-agent." + tab; }
  function superLoad(tab) { try { return JSON.parse(localStorage.getItem(superKey(tab)) || "[]"); } catch (_) { return []; } }
  function openSuperAgent(tab) {
    if (!SUPER_TABS[tab]) return;
    ui.superAgent = { tab: tab, turns: superLoad(tab), draft: "", busy: false, error: "", x: null, y: null }; render();
  }
  function superAgentHtml() {
    const w = ui.superAgent; if (!w) return "";
    const position = Number.isFinite(w.x) && Number.isFinite(w.y) ? ' style="left:' + w.x + 'px;top:' + w.y + 'px;right:auto"' : "";
    const turns = w.turns.map(function (m) { return '<article class="coder-turn"><div class="coder-message coder-' + (m.role === "assistant" ? "assistant" : "user") + '"><b>' + (m.role === "assistant" ? "Agent" : "You") + '</b><div class="coder-bubble ' + (m.role === "assistant" ? "coder-reply" : "coder-user-bubble") + '">' + esc(m.text || "") + '</div>' + (m.card ? '<small>' + esc(m.card) + '</small>' : "") + (m.usage ? contextUsageLine(m.usage) : "") + '</div></article>'; }).join("");
    return '<div class="coder-overlay super-agent-overlay" role="dialog"' + position + '><div class="coder-overlay-head super-agent-head"><b>Super agent · ' + esc(SUPER_TABS[w.tab]) + '</b><div class="coder-head-actions"><button type="button" class="sm quiet" data-act="super-clear">Clear</button><button type="button" class="coder-close" data-act="super-close">×</button></div></div>' + (w.error ? '<p class="bad">' + esc(w.error) + '</p>' : '') + '<div class="coder-overlay-content"><div class="coder-conversation">' + (turns || '<p class="coder-empty">Ask the agent to work in this tab.</p>') + (w.busy ? '<div class="coder-working">Agent is working…</div>' : '') + '</div></div><div class="coder-composer"><textarea id="super-agent-message" rows="1">' + esc(w.draft) + '</textarea><button class="pri sm" data-act="super-send" ' + (w.busy || !w.draft.trim() ? 'disabled' : '') + '>Send</button></div></div>';
  }
  function appAdminHtml() {
    const w = ui.appAdmin; if (!w) return "";
    const turns = (w.session && w.session.turns || []).map(function (m) {
      const changed = Array.isArray(m.changed_files) ? (m.changed_files.length ? '<small>Changed: ' + esc(m.changed_files.join(", ")) + '</small>' : '<small>No files changed.</small>') : '';
      const did = Array.isArray(m.actions) && m.actions.length ? '<div class="admin-did"><b>What it did</b><ul>' + m.actions.map(function(a){return '<li><b>'+esc(a.name||a.kind||"Action")+'</b>'+(a.detail?' · '+esc(a.detail):'')+'</li>';}).join("") + '</ul></div>' : (m.role === "assistant" && !m.failed ? '<small>What it did: no tool actions reported.</small>' : '');
      const restart = m.restart ? '<button class="sm" data-act="admin-restart">Restart app</button>' : '';
      const relaunch = m.relaunch ? '<button class="sm" data-act="admin-relaunch-note">Relaunch app</button>' : '';
      const usage = m.usage ? (m.usage.model_context_window
        ? contextUsageLine({input:m.usage.input_tokens,limit:m.usage.model_context_window,calls:1})
        : '<small>Usage · ' + esc(m.usage.input_tokens == null ? 'not reported' : String(m.usage.input_tokens) + ' input / ' + String(m.usage.output_tokens || 0) + ' output tokens') + '</small>') : '';
      const notice = m.notice ? '<div class="admin-status">'+esc(m.notice)+'</div>' : '';
      return '<article class="coder-turn"><div class="coder-message coder-' + (m.role === "assistant" ? "assistant" : "user") + '"><b>' + (m.role === "assistant" ? "App Admin" : "You") + '</b><div class="coder-bubble">' + esc(m.text || "") + '</div>' + did + usage + changed + notice + restart + relaunch + '</div></article>';
    }).join("");
    const statuses = w.status || {};
    const options = ["claude", "codex"].filter(function (p) { const s = statuses[p] || {}; return s.installed && s.logged_in; });
    const picker = options.length ? '<select id="admin-provider">' + options.map(function(p){return '<option value="'+p+'"'+(w.provider===p?' selected':'')+'>'+(p==='claude'?'Claude Code':'Codex')+'</option>';}).join("") + '</select>' : '<span class="muted">Install and log in to Claude Code or Codex to use App Admin.</span>';
    const statusInfo=w.statusMessage?{text:w.statusMessage,restart:w.statusFlags&&w.statusFlags.restart,relaunch:w.statusFlags&&w.statusFlags.relaunch,refresh:w.statusFlags&&w.statusFlags.refresh}:null;
    const status = statusInfo && statusInfo.text ? '<div class="admin-status">'+esc(statusInfo.text)+(statusInfo.restart?' <button class="sm" data-act="admin-restart">Restart app</button>':'')+(statusInfo.relaunch?' <button class="sm" data-act="admin-relaunch-note">Relaunch app</button>':'')+'</div>' : '';
    return '<div class="coder-overlay app-admin-overlay" role="dialog"><div class="coder-overlay-head"><b>App Admin</b><div class="coder-head-actions">' + picker + '<button class="sm quiet" data-act="admin-new">New conversation</button><button class="coder-close" data-act="admin-close">×</button></div></div><p class="admin-notice">' + esc(w.notice || "") + '</p><div class="coder-overlay-content"><div class="coder-conversation">' + (turns || '<p class="coder-empty">Start a conversation to edit this app and the open project.</p>') + '</div></div><div class="coder-composer"><textarea id="app-admin-message" rows="2" placeholder="Ask App Admin to change this app or project">' + esc(w.draft || "") + '</textarea>' + (w.busy ? '<button class="sm" data-act="admin-stop">Stop</button>' : '<button class="pri sm" data-act="admin-send" ' + (!options.length || !(w.draft||"").trim() ? 'disabled' : '') + '>Send</button>') + '<button class="sm" data-act="admin-undo">Undo</button></div>' + status + (w.error ? '<p class="bad">'+esc(w.error)+'</p>' : '') + '</div>';
  }
  async function openAppAdmin() {
    if (ui.appAdminTimer) clearTimeout(ui.appAdminTimer);
    try { const data = await api("/api/admin/state"); const session=data.session||{}, status=data.status||{}; let provider=session.provider||"claude"; if(!(status[provider]&&status[provider].installed&&status[provider].logged_in)) provider=status.claude&&status.claude.installed&&status.claude.logged_in?"claude":status.codex&&status.codex.installed&&status.codex.logged_in?"codex":"claude"; ui.appAdmin = { session:session, status:status, notice:data.notice || "", provider:provider, draft:"", busy:data.active, error:"", statusMessage:session.last_notice||"", statusFlags:session.last_notice_flags||{} }; }
    catch (e) { ui.appAdmin = {session:{},status:{},notice:"",provider:"claude",draft:"",busy:false,error:e.message||String(e)}; }
    render(); scheduleAppAdminPoll();
  }
  function scheduleAppAdminPoll() {
    if (ui.appAdminTimer) clearTimeout(ui.appAdminTimer);
    if (!ui.appAdmin || !ui.appAdmin.busy) return;
    ui.appAdminTimer=setTimeout(function(){pollAppAdminState();},900);
  }
  async function pollAppAdminState() {
    const w=ui.appAdmin; if(!w)return;
    try {
      const data=await api("/api/admin/state"); if(ui.appAdmin!==w)return;
      w.session=data.session||{}; w.busy=!!data.active; w.statusMessage=w.session.last_notice||""; w.statusFlags=w.session.last_notice_flags||{};
      render(); scheduleAppAdminPoll();
    } catch(e) { if(ui.appAdmin===w){w.error=e.message||String(e);w.busy=false;render();} }
  }
  async function sendAppAdmin() {
    const w=ui.appAdmin; if(!w||w.busy||!w.draft.trim()) return;
    const msg=w.draft.trim(); w.draft=""; w.busy=true; w.error=""; render(); scheduleAppAdminPoll();
    try { await api("/api/admin/chat",{provider:w.provider,message:msg}); }
    catch(e){ if(ui.appAdmin===w)w.error=e.message||String(e); }
    if(ui.appAdmin===w) {
      await pollAppAdminState();
      try { await refresh(); } catch(e) { w.error=w.error||e.message||String(e); }
      if(ui.appAdmin===w) render();
    }
  }
  async function sendSuperAgent() {
    const w = ui.superAgent; if (!w || w.busy || !w.draft.trim()) return;
    const message = w.draft.trim(); w.draft = ""; w.busy = true; w.error = ""; w.turns.push({ role: "user", text: message }); render();
    try {
      const result = await api("/api/super-agent/chat", { tab: w.tab, dataset: ui.dataset || "", side: ui.data.side, messages: w.turns.map(function (m) { return { role: m.role, text: m.text }; }) });
      let reply = result.text || "Done.", card = "";
      if (result.tool && result.tool !== "reply") {
        const args = result.args || {}; let acted;
        if (result.tool === "dag_undo") acted = await api("/api/dag/undo", { dataset: ui.dataset });
        else if (result.tool === "add_to_run_queue" || result.tool === "remove_from_run_queue") {
          if (busyNow() || batchRunning()) { acted={error:"Refused while a job or batch is running."}; }
          else {
          const graphIds = asRecords(S && S.datasets && S.datasets[ui.dataset] && S.datasets[ui.dataset].graph && S.datasets[ui.dataset].graph.nodes).map(function(n){return n.id;});
          const asked = asArray(args.nodes).map(String).filter(function(id){return graphIds.indexOf(id)>=0;});
          let q = queueFor(ui.dataset).slice();
          q = result.tool === "add_to_run_queue" ? q.concat(asked.filter(function(id){return q.indexOf(id)<0;})) : q.filter(function(id){return asked.indexOf(id)<0;});
          ui.runQueues[ui.dataset] = q; saveRunQueues(); render(); acted = { ok:true, nodes:asked };
          }
        } else if (result.tool === "run_queue") { await runQueue(); acted = { ok:true }; }
        else if (result.tool === "run_whole_dataset") { acted = await api("/api/fullrun/run", { dataset:ui.dataset, side:args.side }); }
        else if (result.tool === "open_dataset") { ui.data.openId=String(args.id || ""); ui.tab="data"; await loadDatasets(); await openView(ui.data.openId); render(); acted={ok:true,id:ui.data.openId}; }
        else if (result.tool === "open_variable") { const id=String(args.node || ""); ui.diff.node=id; ui.tab="mismatches"; await loadDiff({render:false}); render(); acted={ok:true,node:id}; }
        else acted = await api("/api/super-agent/action", { tab: w.tab, dataset: ui.dataset || "", side: ui.data.side, tool: result.tool, args: args });
        if (acted && acted.error) card = "Refused: " + acted.error;
        else if (result.tool === "upload_folder" && acted) card = "Uploaded " + (acted.copied || []).length + " files" + ((acted.skipped || []).length ? "; skipped " + acted.skipped.length + ": " + acted.skipped.join(", ") : "") + ".";
        else if (result.tool === "set_model") card = "Model settings updated.";
        else if (result.tool === "dag_ask" && acted) card = ((acted.changes || []).length) + " DAG/spec change(s) applied.";
        else card = "Completed " + result.tool + ".";
        if (!(acted && acted.error)) {
          try {
            const summary = await api("/api/super-agent/chat", { tab:w.tab, dataset:ui.dataset || "", side:ui.data.side,
              messages:w.turns.concat([{role:"assistant",text:reply}]).map(function(m){return {role:m.role,text:m.text};}), tool_result:acted });
            if (summary.tool === "reply" && summary.text) reply = summary.text;
          } catch (_) { /* Busy run actions keep their immediate result card. */ }
        }
      }
      w.turns.push({ role: "assistant", text: reply, card: card, usage: result.usage });
      localStorage.setItem(superKey(w.tab), JSON.stringify(w.turns));
    } catch (e) { w.error = e.message || String(e); }
    w.busy = false; render();
  }
  function nodeStatus() {
    return ui.node ? ((ui.detail && ui.detail.status) || nodeState(ui.node)) : null;
  }
  function specPathWarningHtml(side) {
    const st = nodeStatus();
    let w = st && st.warnings;
    if (!w) return "";
    if (typeof w === "string") w = [w];
    else if (!Array.isArray(w)) w = Object.keys(w).map(function (k) { return w[k]; });
    const msgs = w.filter(function (m) {
      return typeof m === "string" && (side === "primary" && m.indexOf("Lineage warning: ") === 0 || (side === "primary" && m.indexOf("Primary uses ") === 0));
    });
    if (!msgs.length) return "";
    return '<div class="warn specpath"><ul>' + msgs.map(function (m) {
      return "<li>" + esc(m) + "</li>";
    }).join("") + "</ul></div>";
  }
  function sideRan(side) {
    if (sideText(ui.detail && ui.detail.log, side).trim()) return true;
    const st = nodeStatus();
    const s = st && st.sides && st.sides[side];
    if (!s) return false;
    if (typeof s.ok === "boolean") return true;
    if (s.out_path || s.error || s.log) return true;
    return false;
  }
  function generateWhy(busy) {
    if (!ui.node) return "Select a variable first";
    if (busy) return "Busy.";
    return "";
  }
  function compareWhy(busy) {
    if (!ui.node) return "Select a variable first";
    if (busy) return "Busy.";
    const miss = [];
    if (!sideRan("primary")) miss.push("primary");
    if (!sideRan("validation")) miss.push("validation");
    if (miss.length === 1) return "The " + miss[0] + " side has not run yet.";
    if (miss.length > 1) return "Primary and validation have not run yet.";
    return "";
  }
  function rerunWhy(canRun, busy) {
    if (canRun) return "";
    if (!ui.node) return "Select a variable first";
    if (busy) return "Busy.";
    const side = ui.runSide || "both";
    if (side === "both") {
      const miss = [];
      if (!hasStored("primary")) miss.push("primary");
      if (!hasStored("validation")) miss.push("validation");
      if (miss.length === 1) return "No saved code on the " + miss[0] + " side.";
      if (miss.length > 1) return "No saved code on the primary and validation sides.";
    } else if (!hasStored(side)) {
      return "No saved code on the " + side + " side.";
    }
    return "No saved code on the selected side.";
  }
  function codeActionsHtml() {
    const st = ui.node ? ((ui.detail && ui.detail.status) || nodeState(ui.node)) : null;
    const busy = busyNow();
    const canGen = !!ui.node && !busy;
    const canCmp = !!ui.node && !busy && sideRan("primary") && sideRan("validation");
    const side = ui.runSide || "both";
    const canRun = !busy && !!ui.node && (side === "both" ? (hasStored("primary") && hasStored("validation")) : hasStored(side));
    const genWhy = generateWhy(busy);
    const cmpWhy = compareWhy(busy);
    const runWhy = rerunWhy(canRun, busy);
    const reasons = [];
    if (busy) reasons.push(esc("Busy."));
    else {
      if (!canGen && genWhy) reasons.push(esc("Generate & run: " + genWhy));
      if (!canCmp && cmpWhy) reasons.push(esc("Compare: " + cmpWhy));
      if (!canRun && runWhy) reasons.push(esc("Re-run: " + runWhy));
    }
    function opt(v, lab) {
      return '<option value="' + v + '"' + (side === v ? " selected" : "") + ">" + lab + "</option>";
    }
    return '<div class="acts">'
      + btn("generate", "Generate & run", { cls: "pri", disabled: !canGen, title: canGen ? "" : genWhy })
      + '<label class="inline">Re-run <select id="run-side">' + opt("both", "Both") + opt("primary", "primary") + opt("validation", "validation") + "</select></label>"
      + btn("rerun", "Re-run", { disabled: !canRun, title: canRun ? "Runs the saved code. Does not call the model." : runWhy })
      + btn("compare", "Compare", { cls: "pri", disabled: !canCmp, title: canCmp ? "" : cmpWhy })
      + (ui.running ? '<span class="pill" id="run-state">Running…</span>' : "")
      + "</div>"
      + (reasons.length ? '<p class="hint">' + reasons.join("<br>") + "</p>" : "")
      + (st && st.problem && String(st.problem.message || "").indexOf("Missing column:") === 0
        ? '<p class="bad">' + esc(st.problem.message) + '</p>' : "");
  }
  function scopeToggleHtml() {
    const all = ui.codeScope === "all";
    return '<div class="seg" role="group" aria-label="Code scope">'
      + '<button type="button" data-act="code-scope" data-scope="var"' + (all ? "" : ' class="on"') + ">Current variable</button>"
      + '<button type="button" data-act="code-scope" data-scope="all"' + (all ? ' class="on"' : "") + ">Whole dataset</button>"
      + "</div>";
  }
  function codeBodyHtml() {
    const whole = ui.codeScope === "all";
    const st = ui.node ? ((ui.detail && ui.detail.status) || nodeState(ui.node)) : null;
    const head = ui.node && st
      ? '<div class="ph"><b>' + esc(displayNode(ui.node)) + '</b> <span class="' + (st.problem ? "lv lv-bad" : "lv lv-" + (st.level || "none")) + '">'
        + esc(levelText(st)) + "</span>" + (st.same_source ? '<span class="badge">Same source</span>' : "")
        + (st.problem ? ' <span class="bad">Stuck at ' + esc(stageZh(st.problem.stage) || "this step") + "</span>" : "")
        + "</div>" + '<div class="ph variable-detail-actions">' + resetToolbarHtml(ui.node) + "</div>" + codeActionsHtml()
      : '<div class="ph"><b>Whole dataset</b> <span class="muted">' + esc(ui.dataset || "") + "</span></div>";
    const compareBody = (ui.detail && ui.detail.compare ? diffTable(ui.detail.compare) : '<p class="muted">No compare result yet.</p>')
      + (ui.node ? '<div class="rowbtns">' + btn("goto-diff", "View in Mismatch inspector", { cls: "sm" }) + "</div>" : "");
    const lower = ui.node
      ? '<div class="code-compare">' + pane("compare", "Compare", "", compareBody) + "</div>"
      : "";
    return head
      + '<div class="ph">' + scopeToggleHtml()
      + (whole ? '<span class="muted">In dependency order. Click a node heading to open that variable.</span>' : "")
      + "</div>"
      + '<div class="code-compare-layout"><div class="codesplit">' + codeView("primary") + codeView("validation") + "</div>"
      + lower + "</div>";
  }
  function exportDataset() {
    const list = ui.data.list || [];
    if (ui.tab === "data" && ui.data.openId) {
      for (let i = 0; i < list.length; i++) {
        if (list[i] && list[i].id === ui.data.openId && list[i].dataset) return list[i].dataset;
      }
    }
    return ui.dataset;
  }
  function exportReadyFor(ds) {
    const pack = S && S.datasets && ds && S.datasets[ds];
    const g = pack && pack.graph;
    if (!g) return false;
    const nodes = asRecords(g.nodes);
    if (!nodes.length) return false;
    for (let i = 0; i < nodes.length; i++) {
      const st = (pack.nodes && pack.nodes[nodes[i].id]) || {};
      if (st.problem || st.level !== "L3") return false;
    }
    return true;
  }
  function dataRowsShown() {
    const all = ui.data.list || [];
    const n = ui.data.filterNode;
    if (!n) return all;
    return all.filter(function (r) {
      if (!r) return false;
      if (r.node === n) return true;
      return false;
    });
  }
  function dataListHtml() {
    if (ui.data.missing) return '<p class="muted">The data API is not ready yet.</p>';
    if (ui.data.err) return '<p class="bad">' + esc(ui.data.err) + "</p>";
    if (ui.data.list == null) return '<p class="muted">Loading…</p>';
    const rows = dataRowsShown();
    const kinds = [
      { id: "parameter", title: "Parameter datasets" },
      { id: "generated", title: "Dataset" },
      { id: "input", title: "Inputs" },
      { id: "result", title: "Variable results" },
      { id: "output", title: "Exported" }
    ];
    let html = '<div class="ph"><b>View side</b><button type="button" class="' + (ui.data.side === "primary" ? "on" : "") + '" data-act="data-side" data-side="primary">Primary</button><button type="button" class="' + (ui.data.side === "validation" ? "on" : "") + '" data-act="data-side" data-side="validation">Validation</button></div>';
    if (!rows.length) html += '<p class="muted">No datasets.</p>';
    const known = {};
    kinds.forEach(function (g) {
      known[g.id] = true;
      const items = rows.filter(function (r) { return r.kind === g.id; });
      if (!items.length) return;
      const open = !folded("g:" + g.id);
      html += '<section class="sec"><div class="ph"><b>' + esc(g.title) + "</b>"
        + btn("fold", open ? "Collapse" : "Expand", { cls: "sm", data: { pane: "g:" + g.id } }) + "</div>";
      if (!open) { html += "</section>"; return; }
      html += items.map(function (r) {
        let name = r.file || r.dataset || r.id;
        if (r.kind === "result") name = (r.node || r.dataset || "") + (r.side ? " · " + r.side : "");
        else if (r.kind === "parameter") name = r.paramcd || r.dataset || "";
        else if (r.kind === "generated") name = r.dataset || "Dataset";
        const dim = (r.nrow == null ? "?" : r.nrow) + "×" + (r.ncol == null ? "?" : r.ncol);
        return '<button type="button" class="dsrow' + (r.id === ui.data.openId ? " on" : "") + '" data-act="open-ds" data-id="' + esc(r.id) + '" title="' + esc((r.file || "") + (r.updated ? " · " + r.updated : "")) + '"><span>'
          + esc(name) + '</span><span class="muted">' + esc(dim) + "</span></button>";
      }).join("") + "</section>";
    });
    const other = rows.filter(function (r) { return !known[r.kind]; });
    if (other.length) {
      html += '<section class="sec"><div class="ph"><b>Other</b></div>' + other.map(function (r) {
        return '<button type="button" class="dsrow" data-act="open-ds" data-id="' + esc(r.id) + '"><span>' + esc(r.file || r.id) + "</span></button>";
      }).join("") + "</section>";
    }
    return html;
  }
  function missingCellHtml(v) {
    if (v == null) return '<span class="na">NA</span>';
    if (typeof v === "string" && /^\s*$/.test(v)) return '<span class="blank">"' + esc(v) + '"</span>';
    return null;
  }
  function viewCell(v) {
    const missing = missingCellHtml(v);
    if (missing) return '<td class="' + (v == null ? "na" : "blank") + '">' + missing + "</td>";
    return '<td title="' + esc(v) + '">' + esc(v) + "</td>";
  }
  function colsQuery() {
    const cols = ui.data.allCols;
    const on = ui.data.colsOn;
    if (!cols || !on) return "";
    const names = cols.filter(function (c) { return on[c.name] !== false; }).map(function (c) { return c.name; });
    if (!names.length || names.length === cols.length) return "";
    return names.join(",");
  }
  function diffNodes() {
    const g = curGraph();
    return g ? asRecords(g.nodes) : [];
  }
  function mismatchNodes() {
    return diffNodes().filter(function (node) {
      const st = nodeState(node.id);
      return !!(st && st.problem && st.problem.stage === "compare");
    });
  }
  function syncMismatchSelection(preferred) {
    const mismatches = mismatchNodes();
    const ids = mismatches.map(function (node) { return node.id; });
    const candidates = [preferred, ui.diff.node, ui.node].filter(function (id, i, all) { return !!id && all.indexOf(id) === i; });
    let selected = "";
    for (let i = 0; i < candidates.length; i++) {
      if (ids.indexOf(candidates[i]) >= 0) { selected = candidates[i]; break; }
    }
    if (!selected && ids.length) {
      const all = diffNodes().map(function (node) { return node.id; });
      const previous = ui.diff.node || preferred || "";
      const oldIndex = all.indexOf(previous);
      if (oldIndex >= 0) {
        for (let offset = 1; offset <= all.length; offset++) {
          const next = all[(oldIndex + offset) % all.length];
          if (ids.indexOf(next) >= 0) { selected = next; break; }
        }
      }
      if (!selected) selected = ids[0];
    }
    if (ui.diff.node !== selected) {
      ui.diff.node = selected;
      ui.diff.page = 1;
      ui.diff.view = null;
      ui.diff.err = "";
    }
    if (!selected) {
      ui.diff.view = null;
      ui.diff.err = "";
      ui.diff.loading = false;
    }
    return selected;
  }
  function diffPaneItems(columns, side) {
    const out = [];
    columns.forEach(function (c, i) {
      if (c.role === "diff" || c.name === "diff") return;
      if (c.role === "key" || c.group === "key") { out.push({ c: c, i: i }); return; }
      if (c.role === "result") {
        if (c.sides === side) out.push({ c: c, i: i });
        return;
      }
      if (c.role === "input" && (c.sides === side || c.sides === "both" || c.group === side || c.group === "both")) {
        out.push({ c: c, i: i });
      }
    });
    return out;
  }
  function diffHot(row, columns) {
    const at = columns.findIndex(function (c) { return c.role === "diff" || c.name === "diff"; });
    if (at < 0 || !row) return false;
    const flag = Array.isArray(row) ? row[at] : row[columns[at].name];
    return flag === true || flag === "true" || flag === 1;
  }
  function diffCellValue(row, col, index) {
    if (!row) return null;
    if (Array.isArray(row)) return index < row.length ? row[index] : null;
    return row[col.name];
  }
  function diffCellHtml(col, cell, hot) {
    if (col.role === "result" && col.aligned === false && col.note) {
      return '<td class="na empty-result" title="' + esc(col.note) + '">' + esc(col.note) + "</td>";
    }
    const missing = missingCellHtml(cell);
    if (missing) return '<td class="' + (cell == null ? "na" : "blank")
      + (hot && col.role === "result" ? " diffcell" : "") + '">' + missing + "</td>";
    const cls = hot && col.role === "result" ? ' class="diffcell"' : "";
    return "<td" + cls + ' title="' + esc(cell) + '">' + esc(cell) + "</td>";
  }
  function diffTableHtml(items, rows, columns) {
    const groupLabel = { key: "Key", primary: "Primary inputs", validation: "Validation inputs", both: "Used by both", result: "Result" };
    const groups = [];
    items.forEach(function (item) {
      const id = item.c.group || "result";
      const last = groups[groups.length - 1];
      if (!last || last.id !== id) groups.push({ id: id, n: 1 });
      else last.n += 1;
    });
    const groupRow = groups.map(function (g) {
      return '<th colspan="' + g.n + '">' + esc(groupLabel[g.id] || g.id) + "</th>";
    }).join("");
    const colRow = items.map(function (item) {
      const c = item.c;
      let tag = "";
      if (c.role === "input" && (c.sides === "primary" || c.sides === "validation")) {
        tag = ' <span class="sidetag tag-' + c.sides + '">' + esc(c.sides) + "</span>";
      }
      if (c.multi) tag += ' <span class="sidetag tag-multi">Multiple rows</span>';
      if (c.note && c.aligned === false) tag += ' <span class="sidetag tag-empty">' + esc(c.note) + "</span>";
      const title = c.note ? ((c.origin || c.label || c.name || "") + " — " + c.note) : (c.origin || c.label || c.name || "");
      return '<th title="' + esc(title) + '">' + esc(c.label || c.name || "") + tag + "</th>";
    }).join("");
    let body = "";
    if (!rows.length) body = '<tr><td colspan="' + Math.max(1, items.length) + '">No rows</td></tr>';
    rows.forEach(function (r) {
      const hot = diffHot(r, columns);
      body += "<tr" + (hot ? ' class="diffrow"' : "") + ">" + items.map(function (item) {
        return diffCellHtml(item.c, diffCellValue(r, item.c, item.i), hot);
      }).join("") + "</tr>";
    });
    return '<table class="grid diffgrid"><thead><tr>' + groupRow + "</tr><tr>" + colRow
      + "</tr></thead><tbody>" + body + "</tbody></table>";
  }
  function diffPaneHtml(side, items, rows, columns, style) {
    const result = items.filter(function (item) { return item.c.role === "result"; })[0];
    const note = result && result.c.aligned === false && result.c.note
      ? '<span class="sidetag tag-empty">' + esc(result.c.note) + "</span>" : "";
    return '<div class="diffpane" id="diff-pane-' + side + '"' + (style ? ' style="' + style + '"' : "") + ">"
      + '<div class="diffpane-cap"><b>' + esc(side) + "</b>" + note + "</div>"
      + '<div class="diffscroll" id="diff-scroll-' + side + '" tabindex="0">'
      + diffTableHtml(items, rows, columns) + "</div></div>";
  }
  function equalizeDiffRows() {
    const leftScroll = document.getElementById("diff-scroll-primary");
    const rightScroll = document.getElementById("diff-scroll-validation");
    const leftTable = leftScroll && leftScroll.querySelector("table");
    const rightTable = rightScroll && rightScroll.querySelector("table");
    if (leftTable && rightTable) {
      leftTable.style.width = "";
      rightTable.style.width = "";
      const commonRange = Math.max(
        leftTable.scrollWidth - leftScroll.clientWidth,
        rightTable.scrollWidth - rightScroll.clientWidth,
        0
      );
      leftTable.style.width = Math.ceil(leftScroll.clientWidth + commonRange) + "px";
      rightTable.style.width = Math.ceil(rightScroll.clientWidth + commonRange) + "px";
    }
    const leftHead = document.querySelectorAll("#diff-scroll-primary thead tr");
    const rightHead = document.querySelectorAll("#diff-scroll-validation thead tr");
    const leftBody = document.querySelectorAll("#diff-scroll-primary tbody tr");
    const rightBody = document.querySelectorAll("#diff-scroll-validation tbody tr");
    function pair(a, b) {
      const n = Math.min(a.length, b.length);
      let i;
      for (i = 0; i < n; i++) {
        a[i].style.height = "";
        b[i].style.height = "";
      }
      if (n && a[0]) a[0].getBoundingClientRect();
      for (i = 0; i < n; i++) {
        const h = Math.max(a[i].getBoundingClientRect().height, b[i].getBoundingClientRect().height);
        const px = Math.ceil(h) + "px";
        a[i].style.height = px;
        b[i].style.height = px;
      }
    }
    pair(leftHead, rightHead);
    pair(leftBody, rightBody);
    const headH = leftHead.length ? leftHead[0].getBoundingClientRect().height : 0;
    if (headH) {
      document.querySelectorAll(".diffgrid thead tr:nth-child(2) th").forEach(function (th) {
        th.style.top = Math.ceil(headH) + "px";
      });
    }
  }
  let diffResizeBound = false;
  function bindDiffPanes() {
    const left = document.getElementById("diff-scroll-primary");
    const right = document.getElementById("diff-scroll-validation");
    if (!left || !right) return;
    let syncing = false;
    function follow(from, to) {
      if (syncing) return;
      syncing = true;
      if (to.scrollTop !== from.scrollTop) to.scrollTop = from.scrollTop;
      if (to.scrollLeft !== from.scrollLeft) to.scrollLeft = from.scrollLeft;
      requestAnimationFrame(function () { syncing = false; });
    }
    left.addEventListener("scroll", function () { follow(left, right); }, { passive: true });
    right.addEventListener("scroll", function () { follow(right, left); }, { passive: true });
    equalizeDiffRows();
    if (!diffResizeBound) {
      diffResizeBound = true;
      window.addEventListener("resize", function () { equalizeDiffRows(); });
    }
  }
  function compareMark(id) {
    const st = nodeState(id);
    if (st && st.problem && st.problem.stage === "compare") return st.problem.message || "Differs";
    if (st && st.level === "L3") return "Match";
    return "";
  }
  function diffVarLabel(n) {
    if (n && n.kind === "records") return displayNode(n.id);
    return n.paramcd ? n.var + "@" + n.paramcd : (n.var || n.id);
  }
  function diffListHtml() {
    const selected = ui.diff.node || "";
    const differs = mismatchNodes().map(function (n) { return { n: n, mark: compareMark(n.id) }; });
    if (!differs.length) return "";
    function row(item) {
      const n = item.n;
      const on = n.id === selected ? " on" : "";
      const sub = item.mark || "Differs";
      return '<button type="button" class="dsrow' + on + '" data-act="diff-pick" data-id="' + esc(n.id) + '" title="' + esc(sub) + '"><span>'
        + esc(diffVarLabel(n)) + '</span><span class="muted">' + esc(trunc(sub, 28)) + "</span></button>";
    }
    function group(title, items) {
      if (!items.length) return "";
      return '<section class="sec"><div class="ph"><b>' + esc(title) + "</b></div>" + items.map(row).join("") + "</section>";
    }
    return group("Differs", differs);
  }
  function qcListHtml() {
    const compared = diffNodes().map(function (n) {
      return { node: n, state: nodeState(n.id), mark: compareMark(n.id) };
    }).filter(function (x) { return !!x.mark; });
    const differs = compared.filter(function (x) { return x.state && x.state.problem && x.state.problem.stage === "compare"; });
    const matches = compared.filter(function (x) { return !x.state || !x.state.problem || x.state.problem.stage !== "compare"; });
    function group(title, rows) {
      if (!rows.length) return "";
      return '<section class="sec"><div class="ph"><b>' + title + '</b></div>' + rows.map(function (x) {
        const id = x.node.id;
        return '<button type="button" class="dsrow' + (ui.node === id ? " on" : "")
          + '" data-act="qc-pick" data-id="' + esc(id) + '"><span>' + esc(diffVarLabel(x.node))
          + (x.state && x.state.has_qc_report ? ' <span class="badge">QC report</span>' : "")
          + '</span><span class="muted">' + esc(title) + '</span></button>';
      }).join("") + '</section>';
    }
    return group("Differs", differs) + group("Match", matches)
      || '<p class="muted">No compared variables yet.</p>';
  }
  function qcBodyHtml() {
    const st = ui.node ? nodeState(ui.node) : null;
    const differs = !!(st && st.problem && st.problem.stage === "compare");
    const report = ui.detail && ui.detail.report || "";
    const selected = ui.node && compareMark(ui.node);
    const detail = selected
      ? '<section class="qcnode"><div class="ph"><b>' + esc(displayNode(ui.node)) + '</b><span class="muted">' + esc(compareMark(ui.node)) + '</span>'
        + btn("investigate", "Investigate", { cls: "pri sm", disabled: !differs || busyNow(),
             title: differs ? "Investigate this mismatch" : "Available after a mismatch" })
        + (differs ? btn("qc-goto-diff", "View differing rows in Mismatches", { cls: "sm" }) : "")
        + '</div><div class="qcreport md" data-scroll-key="qc-report">' + (report ? mdToHtml(report) : '<p class="muted">No QC report yet.</p>') + '</div></section>'
      : '<section class="qcnode"><div class="ph"><b>QC report</b></div><div class="qcreport" data-scroll-key="qc-report"><p class="muted">Select a compared variable.</p></div></section>';
    return '<div class="workspace tab-qc"><aside class="qclist"><div class="ph"><b>Compared variables</b></div>'
      + '<div class="scroll" data-scroll-key="qc-list">' + qcListHtml() + '</div></aside><div class="qcmain" data-scroll-key="qc-main">'
      + detail
      + '<p class="outbound qc-outbound">Outbound: investigation sends differing rows to the model.</p>'
      + '<section class="qcdocs"><div class="ph"><b>QC documents</b></div><div class="scroll" data-scroll-key="qc-docs">' + qcSettingsHtml() + '</div></section>'
      + '</div></div>';
  }
  function diffHtml() {
    const selected = ui.diff.node || "";
    const head = '<div class="ph"><b>Mismatches</b>'
      + '<span class="muted">Computed on this machine. Nothing is sent to the model.</span>'
      + "</div>";
    const tools = '<div class="ph vtools">'
      + '<label class="inline"><input id="diff-only" type="checkbox"' + (ui.diff.onlyDiff ? " checked" : "") + "> Only differing rows</label>"
      + '<input id="diff-filter" placeholder="Filter" value="' + esc(ui.diff.filter) + '">'
      + "</div>";
    if (!mismatchNodes().length) return '<div class="empty">No mismatches.</div>';
    if (!selected || !mismatchNodes().some(function (node) { return node.id === selected; }))
      return '<section id="diff-sec" class="diffsec">' + head + tools + '<p class="muted">Opening…</p></section>';
    if (ui.diff.err) {
      return '<section id="diff-sec" class="diffsec">' + head + tools + '<p class="bad">' + esc(ui.diff.err) + "</p></section>";
    }
    const v = ui.diff.view;
    if (!v) {
      return '<section id="diff-sec" class="diffsec">' + head + tools
        + '<p class="muted">' + (ui.diff.loading ? "Loading…" : "Opening…") + "</p></section>";
    }
    const columns = asRecords(v.columns);
    const rows = Array.isArray(v.rows) ? v.rows : [];
    const leftPane = diffPaneHtml("primary", diffPaneItems(columns, "primary"), rows, columns, "");
    const rightPane = diffPaneHtml("validation", diffPaneItems(columns, "validation"), rows, columns, "");
    const page = v.page || ui.diff.page || 1;
    const size = v.size || ui.diff.size || 100;
    const pages = Math.max(1, Math.ceil((v.nrow || 0) / size));
    const nShown = Number(v.nrow);
    const pager = '<div class="ph vtools"><span class="muted">' + esc(v.nrow) + (nShown === 1 ? " row" : " rows") + "</span>"
      + btn("diff-page", "Previous", { cls: "sm", disabled: page <= 1, data: { delta: "-1" } })
      + '<span class="muted">' + page + " / " + pages + "</span>"
      + btn("diff-page", "Next", { cls: "sm", disabled: page >= pages, data: { delta: "1" } })
      + "</div>";
    return '<section id="diff-sec" class="diffsec">' + head + tools + pager
      + '<div class="diffpanes" id="diff-panes">' + leftPane + rightPane + "</div></section>";
  }
  function viewerHtml() {
    const expDs = exportDataset();
    const ready = exportReadyFor(expDs);
    const expTitle = ready ? "" : "Export after every variable is Data matched";
    const chip = ui.data.filterNode
      ? '<span class="pill">' + esc(displayNode(ui.data.filterNode)) + "</span>" + btn("clear-node-filter", "Clear filter", { cls: "sm" })
      : "";
    const bar = '<div class="ph"><b>Data</b>' + chip + '<span class="muted">' + esc(expDs || "") + "</span>"
      + btn("export", "Export xpt", { cls: "sm", disabled: !ready, data: { fmt: "xpt" }, title: expTitle })
      + btn("export", "Export rds", { cls: "sm", disabled: !ready, data: { fmt: "rds" }, title: expTitle })
      + "</div>";
    if (!ui.data.openId) return bar + '<div class="vbody"><div class="empty">Open a dataset on the left.</div></div>';
    if (ui.data.viewErr) return bar + '<div class="vbody"><p class="bad">' + esc(ui.data.viewErr) + "</p></div>";
    const v = ui.data.view;
    if (!v) return bar + '<div class="vbody"><p class="muted">' + (ui.data.loading ? "Loading…" : "Opening…") + "</p></div>";
    if (v.empty) return bar + '<div class="vbody"><div class="empty">' + esc(v.message || "No data generated yet. Generate Keys first.") + "</div></div>";
    const columns = asRecords(v.columns);
    const allCols = ui.data.allCols || columns;
    const limit = (v.limit || ui.data.limit || 100);
    const page = Math.floor((v.offset || 0) / limit) + 1;
    const pages = Math.max(1, Math.ceil((v.nrow || 0) / limit));
    let chooser = "";
    if (ui.data.colsOpen) {
      chooser = '<div class="chooser">' + allCols.map(function (c) {
        const on = !ui.data.colsOn || ui.data.colsOn[c.name] !== false;
        return '<label><input type="checkbox" data-act="col" data-col="' + esc(c.name) + '"' + (on ? " checked" : "") + "> " + esc(c.name) + "</label>";
      }).join("") + "</div>";
    }
    const rows = Array.isArray(v.rows) ? v.rows : [];
    const head = columns.map(function (c) {
      return '<th title="' + esc(c.label || c.name || "") + '">' + esc(c.name || "") + "</th>";
    }).join("");
    let body = "";
    if (!rows.length) body = '<tr><td colspan="' + Math.max(1, columns.length) + '">No rows</td></tr>';
    rows.forEach(function (r) {
      body += "<tr>" + columns.map(function (c, i) {
        let cell = null;
        if (Array.isArray(r)) cell = r[i];
        else if (r && typeof r === "object") cell = r[c.name];
        return viewCell(cell);
      }).join("") + "</tr>";
    });
    return bar + '<div class="ph vtools">'
      + '<input id="data-filter" placeholder="Filter" value="' + esc(ui.data.filter) + '">'
      + btn("cols", ui.data.colsOpen ? "Hide columns" : "Columns", { cls: "sm" })
      + '<span class="muted">' + esc(v.nrow) + "×" + esc(v.ncol) + "</span>"
      + btn("page", "Previous", { cls: "sm", disabled: page <= 1, data: { delta: "-1" } })
      + '<span class="muted">' + page + " / " + pages + "</span>"
      + btn("page", "Next", { cls: "sm", disabled: page >= pages, data: { delta: "1" } })
      + '<span class="muted">100 rows per page</span></div>'
      + chooser
      + '<div class="vscroll" data-scroll-key="data-viewer"><table class="grid"><thead><tr>' + head + "</tr></thead><tbody>" + body + "</tbody></table></div>";
  }
  function specTableHtml() {
    const kind = ui.specTab === "parameter" ? "parameters" : "variables";
    const rows = specRowsOf(kind);
    const cols = kind === "parameters"
      ? ["row", "Dataset", "PARAMCD", "PARAM", "Source", "Derivation"]
      : ["row", "Dataset", "Variable", "Label", "Type", "Length", "Key", "Source", "Derivation"];
    let html = "<table><thead><tr>" + cols.map(function (c) { return "<th>" + esc(c) + "</th>"; }).join("") + "</tr></thead><tbody>";
    if (!rows.length) return html + '<tr><td colspan="' + cols.length + '">No rows</td></tr></tbody></table>';
    rows.forEach(function (r) {
      const file = kind === "parameters" ? "parameter.csv" : "variable.csv";
      const sel = (isSel(file, r.row) || rowInSelection(r, kind)) ? " sel" : "";
      html += '<tr class="click' + sel + '" data-act="spec-row" data-file="' + file + '" data-row="' + esc(r.row) + '" data-ds="' + esc(r.Dataset || "") + '" data-var="' + esc(r.Variable || "") + '" data-param="' + esc(r.PARAMCD || "") + '">';
      cols.forEach(function (c) {
        const raw = r[c] == null ? "" : r[c];
        const inner = (c === "Source" || c === "Derivation") ? markRefs(raw, r.Dataset) : esc(raw);
        html += '<td class="clip" title="' + esc(raw) + '">' + inner + "</td>";
      });
      html += "</tr>";
    });
    return html + "</tbody></table>";
  }
  function bottomHtml() {
    const specExtra = '<div class="tabs">'
      + btn("spec-tab", "Variables", { cls: ui.specTab === "variable" ? "sm on" : "sm", data: { tab: "variable" } })
      + btn("spec-tab", "Parameters", { cls: ui.specTab === "parameter" ? "sm on" : "sm", data: { tab: "parameter" } })
      + "</div>" + btn("spec-all", ui.specAll ? "Current" : "All", { cls: "sm", title: ui.specAll ? "Only this dataset" : "All datasets" })
      + '<input id="spec-filter" placeholder="Filter" value="' + esc(ui.specFilter) + '">';
    return pane("spec", "Spec", specExtra, specTableHtml());
  }
  function reviewHtml(side) {
    const review = ui.detail && ui.detail.reviews && ui.detail.reviews[side];
    if (!review) return "";
    const issues = asArray(review.issues);
    const verdict = String(review.verdict || "uncertain");
    return '<section class="self-review"><b>Self-review · ' + esc(verdict) + '</b>'
      + (review.reasoning ? '<p>' + esc(review.reasoning) + '</p>' : "")
      + (issues.length ? '<p>Issues: ' + esc(issues.map(function (x) { return typeof x === "string" ? x : JSON.stringify(x); }).join("; ")) + '</p>' : "")
      + (review.spec_question ? '<p>Spec question: ' + esc(review.spec_question) + '</p>' : "")
      + '<p class="muted">' + (review.retried ? "Retried once" : "Not retried") + '</p></section>';
  }
  function dagMem() {
    const pack = curPack();
    return (pack && pack.dag) || { turns: [], corrections: [] };
  }
  function dagContextMenuHtml() {
    const c = ui.dagContext;
    if (!c || parameterMode()) return "";
    const items = [];
    if (ui.dagReview && ui.dagReview.dataset === ui.dataset) items.push(["Show all", "dag-show-all"]);
    if (c.node) items.push(["Review this node", "dag-review"]);
    if (!items.length) return "";
    const left = Math.max(4, Math.min(c.x, (document.getElementById('dagwrap') || {}).clientWidth - 210));
    const top = Math.max(4, Math.min(c.y, (document.getElementById('dagwrap') || {}).clientHeight - 220));
    return '<div class="dag-context" role="menu" style="left:' + left + 'px;top:' + top + 'px">' + items.map(function (it) {
      return '<button type="button" role="menuitem" data-act="dag-context-action" data-action="' + it[1] + '"'
        + (it[2] ? ' data-from="' + esc(it[2].from) + '" data-to="' + esc(it[2].to) + '"' : '') + '>' + esc(it[0]) + '</button>';
    }).join("") + '</div>';
  }
  function reviewDagNode(id) {
    const ds = ui.dataset;
    if (!id || !ds) return;
    const previous = ui.dagReview && ui.dagReview.dataset === ds ? ui.dagReview.previous : { view: Object.assign({}, ui.view) };
    ui.dagContext = null;
    api("/api/node/lineage?dataset=" + encodeURIComponent(ds) + "&node=" + encodeURIComponent(id)).then(function (result) {
      if (ui.dataset !== ds) return;
      ui.dagReview = { dataset: ds, node: id, targetId: result && result.target_id || id,
        ids: asArray(result && result.ids), roles: asArray(result && result.roles),
        sourceContext: result && result.source_context || {}, edges: asArray(result && result.edges), previous: previous };
      ui.didFit = false; ui.userView = false; ui.category = null;
      render();
    }).catch(showErr);
  }
  function showAllDag() {
    const review = ui.dagReview;
    if (!review || review.dataset !== ui.dataset) return;
    ui.dagReview = null; ui.dagContext = null;
    ui.view = Object.assign({}, review.previous && review.previous.view || ui.view);
    ui.didFit = true; ui.userView = true; render();
  }
  function confirmManualEdits(count) {
    const n = Number(count);
    if (!(n > 0)) return true;
    return confirm("Re-extract replaces this graph with the model's own reading. " + n + " manual edits will be dropped.");
  }
  function graphManualEditCount(ds) {
    const pack = S && S.datasets && S.datasets[ds];
    const saved = pack && pack.dag ? asRecords(pack.dag.corrections).length : 0;
    return saved + asArray(ui.dagEditNotes && ui.dagEditNotes[ds]).length;
  }
  function parameterManualEditCount(pg, ds) {
    if (pg && typeof pg === 'object') {
      const key = Object.keys(pg).find(function (k) { return /manual.*edit.*count|edit.*count/i.test(k); });
      if (key) return Number(pg[key]);
    }
    const pack = S && S.datasets && S.datasets[ds || ui.dataset];
    const count = pack && pack.parameter_graph && pack.parameter_graph.manual_edit_count;
    return count == null ? null : Number(count);
  }
  function dagUndoInfo() {
    const turns = asRecords(dagMem().turns);
    for (let i = turns.length - 1; i >= 0; i--) if (turns[i] && turns[i].role === "agent" && asRecords(turns[i].card).length && !turns[i].undone)
      return { count: !ui.dagUndoBusy ? 1 : 0, label: "DAG agent reply" };
    const state = curPack() && curPack().dag_undo;
    const raw = state && state.count;
    const count = Number(raw);
    return { count: !ui.dagEditBusy && !ui.dagUndoBusy && isFinite(count) && count > 0 ? count : 0, label: String(state && state.label || "") };
  }
  async function undoDagEdit() {
    if (!ui.dataset || ui.dagEditBusy || ui.dagUndoBusy) return;
    const before = dagUndoInfo();
    if (!before.count) return;
    ui.dagUndoBusy = true;
    setMsg("Undoing graph edit…");
    render();
    try {
      const r = await api("/api/dag/undo", { dataset: ui.dataset });
      await refresh();
      const notes = asArray(ui.dagEditNotes && ui.dagEditNotes[ui.dataset]);
      const restoredLabel = String(r && r.label || before.label || "");
      const noteIndex = notes.map(function (note) { return note && typeof note === "object" ? note.undoLabel : ""; }).lastIndexOf(restoredLabel);
      if (noteIndex >= 0) { notes.splice(noteIndex, 1); saveDagEditNotes(); }
      if (r && r.edges) armDagFlash(asRecords(r.edges));
      const label = String(r && r.label || before.label || "latest graph edit");
      const kept = r && r.kept;
      const keptNumber = typeof kept === "number" ? kept : (typeof kept === "string" && kept.trim() ? Number(kept) : NaN);
      const keptCount = Array.isArray(kept) ? kept.length : (isFinite(keptNumber) ? keptNumber : (kept && typeof kept === "object" ? Object.keys(kept).length : (kept ? 1 : 0)));
      setMsg("Undid: " + label + (keptCount > 0 ? ". " + keptCount + " variables keep their newer results." : ""));
    } catch (e) {
      setMsg(serverError(e, "Could not undo graph edit"));
    } finally {
      ui.dagUndoBusy = false;
      render();
    }
  }
  function applyGraphEdit(changes) {
    if (!ui.dataset || ui.dagEditBusy) return;
    const ds = ui.dataset;
    const description = changes.map(function (ch) { return ch.op === 'remove_edge' ? 'Disconnected ' + ch.from + ' → ' + ch.to : 'Connected ' + ch.from + ' → ' + ch.to; }).join('; ');
    ui.dagEditBusy = true; setMsg('Applying graph edit…'); render();
    api('/api/dag/apply', { dataset: ds, changes: changes }).then(async function (r) {
      const notes = ui.dagEditNotes[ds] || (ui.dagEditNotes[ds] = []);
      notes.push({ text: description, undoLabel: changes.map(dagChangeLine).join(", ") });
      saveDagEditNotes();
      await refresh();
      if (r && r.edges) armDagFlash(asRecords(r.edges));
      const n = Array.isArray(r && r.affected) ? r.affected.length : Number(r && r.affected);
      setMsg(r && r.affected != null ? description + ' · ' + (isFinite(n) ? n : r.affected) + ' variables reset' : description);
    }).catch(function (e) { setMsg(serverError(e, 'Could not apply graph edit')); }).finally(function () { ui.dagEditBusy = false; render(); });
  }
  function pendingDagChanges() {
    if (ui.dagPending && ui.dagPending.length) return ui.dagPending;
    const turns = asRecords(dagMem().turns);
    for (let i = turns.length - 1; i >= 0; i--) {
      const t = turns[i];
      if (!t || t.role !== "agent") continue;
      if (t.applied) return [];
      if (ui.dagHideAt && t.at && ui.dagHideAt === t.at) return [];
      return asRecords(t.changes);
    }
    return [];
  }
  function dagChangeLine(ch) {
    const op = ch && ch.op;
    if (op === "add_edge") return "+ edge " + (ch.from || "") + " -> " + (ch.to || "");
    if (op === "remove_edge") return "- edge " + (ch.from || "") + " -> " + (ch.to || "");
    if (op === "set_key") {
      const off = ch.value === false || ch.value === 0 || ch.value === "0" || ch.value === "false";
      return (off ? "- key " : "+ key ") + (ch.node || ch.id || "");
    }
    if (op === "set_categories") return "categories " + (ch.node || ch.id || "") + " (" + asArray(ch.categories).length + ")";
    if (op === "add_node") return "+ node " + (ch.id || ch.node || "");
    return String(op || "change");
  }
  function dagAgentHtml() {
    const open = ui.dagOpen !== false, mem = dagMem();
    let body = "";
    if (open) {
      const turns = asRecords(mem.turns);
      let latestCard = -1;
      for (let i = turns.length - 1; i >= 0; i--) if (turns[i] && turns[i].role === "agent" && asRecords(turns[i].card).length && !turns[i].undone) { latestCard = i; break; }
      let msgs = turns.length ? "" : '<p class="muted">Ask about dependencies or request a change to this DAG or spec.</p>';
      turns.forEach(function (t, turnIndex) {
        const isAgent = t.role === "agent";
        msgs += '<div class="dagmsg ' + (isAgent ? 'agent' : 'you') + '"><b>' + (isAgent ? 'Agent' : 'You') + '</b><div>' + esc(t.text || '') + '</div>'
          + (isAgent ? contextUsageLine(t.usage) : '') + '</div>';
        asRecords(t.card).forEach(function (item) { msgs += '<div class="dagchange-card"><ul><li>' + esc(item.text || '') + '</li></ul>' + (t.undone ? '<span class="muted">Undone</span>' : (turnIndex === latestCard ? btn('dag-undo', 'Undo', { cls: 'sm', disabled: !!ui.dagUndoBusy }) : '<span class="muted">Undo the newer change first</span>')) + '</div>'; });
      });
      if (ui.dagAsking) msgs += '<div class="dagmsg agent"><b>Agent</b><div>Agent is working…</div></div>';
      const corrs = asRecords(mem.corrections);
      if (corrs.length) msgs += '<details class="dagcorr"><summary>Corrections</summary>' + corrs.map(function (c) { return '<div class="dagcorrline">' + esc(c.text || '') + '</div>'; }).join('') + '</details>';
      body = '<div class="scroll dagscroll" data-scroll-key="dag-thread">' + msgs + '</div><div class="dagcompose"><textarea id="dag-text" placeholder="Message the DAG agent…"' + (ui.dagAsking ? ' disabled' : '') + '>' + esc(ui.dagDraft || '') + '</textarea>'
        + '<div class="dag-actions">' + btn('dag-send', 'Send', { cls: 'pri sm', disabled: !!ui.dagAsking }) + btn('dag-clear', 'Clear', { cls: 'sm', disabled: !!ui.dagAsking }) + '</div></div>';
    }
    return '<aside class="dagagent' + (open ? '' : ' fold') + '" id="dag-agent"><div class="ph"><b>DAG agent</b>' + btn('dag-fold', open ? 'Collapse' : 'Expand', { cls: 'sm' }) + '</div>' + body + '</aside>';
  }
  function paramAgentHtml() { return dagAgentHtml(); }
  function prefillDag(id) {
    if (!id) return;
    const cur = ui.dagDraft || "";
    if (!String(cur).trim() || /^About [^:]+: /.test(cur)) { const parts = String(id).split("."); ui.dagDraft = "About " + (parts.length > 1 ? parts[0] + " · " + parts[1].split("@")[0] : id) + ": "; }
  }
  function publishBoot() {
    const html = document.documentElement;
    const errs = (window.__dualdagErrors || []).join(" | ");
    html.dataset.boot = "1";
    html.dataset.jsErrors = errs;
    if (ui.autodagDone) html.dataset.autodag = ui.autodagDone;
    if (ui.dagFlashed) html.dataset.flashed = ui.dagFlashed;
    let boot = document.getElementById("dag-boot");
    if (!boot) {
      boot = document.createElement("div");
      boot.id = "dag-boot";
      boot.hidden = true;
      document.body.appendChild(boot);
    }
    boot.dataset.boot = "1";
    boot.dataset.jsErrors = errs;
    boot.dataset.autodag = html.dataset.autodag || "";
    boot.dataset.flashed = html.dataset.flashed || "";
  }
  function armDagFlash(edges) {
    ui.dagFlash = {};
    const ids = [];
    asRecords(edges).forEach(function (e) {
      if (!e || !e.from || !e.to) return;
      ui.dagFlash[e.from + "\t" + e.to] = e.kind || "add";
      ids.push(e.from + "->" + e.to);
    });
    if (ids.length) ui.dagFlashed = ids.join(",");
    if (ui.dagFlashTimer) clearTimeout(ui.dagFlashTimer);
    ui.dagFlashTimer = setTimeout(function () {
      ui.dagFlash = {};
      ui.dagFlashTimer = null;
      if (S) render();
    }, 4000);
  }
  function scrollKey(el, root) {
    if (el.dataset && el.dataset.scrollKey) return "key:" + el.dataset.scrollKey;
    if (el.id) return "id:" + el.id;
    const parts = [];
    for (let node = el; node && node !== root; node = node.parentElement) {
      const parent = node.parentElement;
      if (!parent) break;
      const siblings = Array.from(parent.children).filter(function (other) {
        return other.tagName === node.tagName && other.className === node.className;
      });
      parts.push(node.tagName + "." + String(node.className || "") + ":" + siblings.indexOf(node));
    }
    return "path:" + parts.reverse().join("/");
  }
  function scrollBoxes(root) {
    return Array.from(root.querySelectorAll('[data-scroll-key], .maincol, .dagmain, .codemain, .codeside, .codeview, .codeblock, .log, .askthread, .vscroll, .qcmain, .qcreport, .setscroll, .diffscroll, .dagcompose, .scroll, .category-pop, .mbody, .settings, .md pre, .coder-overlay-content, .coder-conversation')).filter(function (el) {
      const style = getComputedStyle(el);
      return /(auto|scroll)/.test(style.overflowX + " " + style.overflowY)
        && (el.scrollHeight > el.clientHeight || el.scrollWidth > el.clientWidth);
    });
  }
  function renderContext() {
    return [ui.tab, ui.dataset || "", ui.node || "", ui.codeScope || ""].join("|");
  }
  function render() {
    if (paneResizeDrag || coderResizeActive) { paneResizePending = true; return; }
    const app = document.getElementById("app");
    if (!S) { app.textContent = "Loading…"; return; }
    const context = renderContext();
    const sameView = context === ui.renderedContext;
    const oldScroll = {};
    const pageScroll = { x: window.scrollX, y: window.scrollY,
      top: document.scrollingElement ? document.scrollingElement.scrollTop : 0 };
    if (sameView) scrollBoxes(app).forEach(function (el) {
      oldScroll[scrollKey(el, app)] = { top: el.scrollTop, left: el.scrollLeft,
        followEnd: el.matches("pre.log") && el.scrollHeight - el.clientHeight - el.scrollTop < 40 };
    });
    const active = app.contains(document.activeElement) ? document.activeElement : null;
    const focusId = ui.keepFocus || (sameView && active && active.id) || "";
    const activeValue = active && typeof active.value === "string" ? active.value : null;
    const selection = active && typeof active.selectionStart === "number"
      ? { start: active.selectionStart, end: active.selectionEnd, direction: active.selectionDirection } : null;
    const restoreValue = !ui.skipValueRestore;
    ui.skipValueRestore = false;
    ui.keepFocus = "";
    let body;
    if (ui.tab === "code") {
      const showCode = !!ui.node || ui.codeScope === "all";
      body = '<div class="workspace tab-code"><div class="codemain" data-scroll-key="code-main">'
        + codeGuideHtml()
        + (showCode ? codeBodyHtml() : '<div class="empty">Select a variable under Spec & checks. '
          + btn("tab", "Spec & checks", { cls: "pri", data: { tab: "spec" } }) + "</div>")
        + "</div></div>";
    } else if (ui.tab === "run") {
      body = '<div class="workspace tab-run"><div class="maincol" data-scroll-key="run-main">' + runTabHtml() + '</div></div>';
    } else if (ui.tab === "data") {
      body = '<div class="workspace tab-data"><aside class="datalist"><div class="scroll" id="data-list">'
        + dataListHtml() + '</div></aside><section class="viewer"><div class="dataview">' + viewerHtml() + "</div></section></div>";
    } else if (ui.tab === "mismatches") {
      if (!mismatchNodes().length) body = '<div class="empty">No mismatches.</div>';
      else body = '<div class="workspace tab-mismatches"><aside class="datalist"><div class="scroll" id="diff-list">'
        + diffListHtml() + '</div></aside><section class="viewer">' + diffHtml() + "</section></div>";
    } else if (ui.tab === "qc") {
      body = qcBodyHtml();
    } else if (ui.tab === "settings") {
      body = '<div class="workspace tab-settings">' + settingsHtml() + "</div>";
    } else {
      body = '<div class="workspace tab-spec' + (ui.multiMode ? " multimode" : "") + '"><div class="maincol" data-scroll-key="spec-main"><div class="dagrow"><div class="dagmain" data-scroll-key="dag-main">' + centerHtml()
        + '</div>' + paramAgentHtml() + '</div><div class="split">' + bottomHtml() + "</div></div></div>";
    }
    app.innerHTML = headerHtml() + tabBarHtml() + body + coderWindowHtml() + superAgentHtml() + appAdminHtml();
    if (ui.tab !== "settings") bindResizablePanes(app);
    const coderOverlay = app.querySelector(".coder-overlay:not(.super-agent-overlay):not(.app-admin-overlay)");
    const adminOverlay = app.querySelector(".app-admin-overlay");
    try {
      const coderSize=JSON.parse(localStorage.getItem("dualdag.coderWindowSize")||"null");
      if(coderOverlay&&coderSize&&coderSize.width>0&&coderSize.height>0){coderOverlay.style.width=coderSize.width+"px";coderOverlay.style.height=coderSize.height+"px";}
      const adminSize=JSON.parse(localStorage.getItem("dualdag.appAdminWindowSize")||"null");
      if(adminOverlay&&adminSize&&adminSize.width>0&&adminSize.height>0){adminOverlay.style.width=adminSize.width+"px";adminOverlay.style.height=adminSize.height+"px";}
    } catch(err) { /* Default overlay sizes remain available. */ }
    if ((coderOverlay||adminOverlay) && !window.dualdagOverlayResizeBound) {
      window.dualdagOverlayResizeBound=true;
      window.addEventListener("pointerdown",function(e){
        const el=e.target&&e.target.closest?e.target.closest(".coder-overlay:not(.super-agent-overlay)"):null;
        if(!el)return; const rect=el.getBoundingClientRect();
        coderResizeActive=e.clientX>=rect.right-22&&e.clientY>=rect.bottom-22;
        coderResizeTarget=coderResizeActive?el:null;
      },true);
      window.addEventListener("pointerup",function(){
        const el=coderResizeTarget;
        if(el)try{const key=el.classList.contains("app-admin-overlay")?"dualdag.appAdminWindowSize":"dualdag.coderWindowSize";localStorage.setItem(key,JSON.stringify({width:el.offsetWidth,height:el.offsetHeight}));}catch(err){/* Keep default size. */}
        if(coderResizeActive){coderResizeActive=false;coderResizeTarget=null;if(paneResizePending){paneResizePending=false;render();}}
      });
    }
    const coderComposer = document.getElementById("coder-window-message");
    if (coderComposer) {
      coderComposer.style.height = "auto";
      coderComposer.style.height = Math.min(coderComposer.scrollHeight, 120) + "px";
    }
    ui.renderedContext = context;
    if (sameView) scrollBoxes(app).forEach(function (el) {
      const saved = oldScroll[scrollKey(el, app)];
      if (saved) { el.scrollTop = saved.followEnd ? el.scrollHeight : saved.top; el.scrollLeft = saved.left; }
    });
    if (ui.coderWindow && !ui.coderWindow.detailsOpen && ui.coderWindow.followNewest) {
      const conversation = app.querySelector(".coder-conversation");
      if (conversation) conversation.scrollTop = conversation.scrollHeight;
    }
    app.querySelectorAll("pre.log[data-side]").forEach(function (el) {
      if (!sameView || !oldScroll[scrollKey(el, app)]) el.scrollTop = el.scrollHeight;
    });
    if (ui.threadToEnd) {
      const thread = app.querySelector('[data-scroll-key="' + ui.threadToEnd + '"]');
      if (thread) thread.scrollTop = thread.scrollHeight;
    }
    if (ui.tab === "settings") { autoDetectCliLogin(); loadCliStatus(false); }
    app.dataset.ready = "1";
    app.dataset.tab = ui.tab;
    publishBoot();
    if (ui.tab === "mismatches") bindDiffPanes();
    if (ui.tab === "code") bindCodeGuide();
    if (ui.tab === "spec") { bindDag(); bindRunQueue(); }
    if (ui.scrollSpec && ui.tab === "spec") {
      const target = ui.scrollSpec;
      ui.scrollSpec = null;
      const q = document.querySelector('#spec-scroll tr[data-file="' + target.file + '"][data-row="' + target.row + '"]');
      if (q) q.scrollIntoView({ block: "center" });
    }
    if (focusId) {
      const n = document.getElementById(focusId);
      if (n && n.focus) {
        if (restoreValue && active && active.id === focusId && active.id !== "coder-window-message"
            && activeValue != null && typeof n.value === "string")
          n.value = activeValue;
        n.focus({ preventScroll: true });
        if (typeof n.value === "string" && n.setSelectionRange) {
          const start = selection && active && active.id === focusId ? selection.start : n.value.length;
          const end = selection && active && active.id === focusId ? selection.end : n.value.length;
          try { n.setSelectionRange(start, end, selection ? selection.direction : "none"); } catch (e) { /* number inputs */ }
        }
      }
    }
    if (sameView) {
      window.scrollTo(pageScroll.x, pageScroll.y);
      if (document.scrollingElement) document.scrollingElement.scrollTop = pageScroll.top;
    }
  }
  function applyView() {
    const g = document.getElementById("viewport");
    if (!g || !ui.view) return;
    g.setAttribute("transform", "translate(" + ui.view.x + " " + ui.view.y + ") scale(" + ui.view.k + ")");
    positionCategory();
  }
  function panNodeIntoView(id) {
    const wrap = document.getElementById("dagwrap");
    const node = Array.from(document.querySelectorAll("#dag .node[data-id]")).find(function (el) { return el.getAttribute("data-id") === id; });
    if (!wrap || !node) return;
    const bounds = wrap.getBoundingClientRect(), box = node.getBoundingClientRect(), margin = 0;
    let dx = 0, dy = 0;
    if (box.left < bounds.left + margin) dx = bounds.left + margin - box.left;
    else if (box.right > bounds.right - margin) dx = bounds.right - margin - box.right;
    if (box.top < bounds.top + margin) dy = bounds.top + margin - box.top;
    else if (box.bottom > bounds.bottom - margin) dy = bounds.bottom - margin - box.bottom;
    if (dx || dy) {
      ui.view.x += dx;
      ui.view.y += dy;
      ui.userView = true;
      applyView();
    }
  }
  function fitView() {
    const wrap = document.getElementById("dagwrap");
    const lay = ui.layout;
    if (!wrap || !lay || !lay.width) return;
    const pad = 24;
    const kw = (wrap.clientWidth - pad) / lay.width;
    const kh = (wrap.clientHeight - pad) / lay.height;
    if (!isFinite(kw) || !isFinite(kh) || wrap.clientWidth < 40) return;
    ui.view.k = clamp(Math.min(kw, kh), 0.28, 1.15);
    ui.view.x = (wrap.clientWidth - lay.width * ui.view.k) / 2 - lay.minX * ui.view.k;
    ui.view.y = (wrap.clientHeight - lay.height * ui.view.k) / 2 - lay.minY * ui.view.k;
    applyView();
  }
  function zoomBy(factor) {
    const svg = document.getElementById("dag");
    if (!svg) return;
    ui.userView = true;
    const rect = svg.getBoundingClientRect();
    const mx = rect.width / 2, my = rect.height / 2;
    const nk = clamp(ui.view.k * factor, 0.28, 2.6);
    ui.view.x = mx - (mx - ui.view.x) * (nk / ui.view.k);
    ui.view.y = my - (my - ui.view.y) * (nk / ui.view.k);
    ui.view.k = nk;
    applyView();
  }
  function updateLiveGraph() {
    const lay = ui.layout;
    if (!lay) return;
    document.querySelectorAll("#dag .node[data-id]").forEach(function (el) {
      const c = lay.coords[el.getAttribute("data-id")];
      if (c) el.setAttribute("transform", "translate(" + c.x + " " + c.y + ")");
    });
    lay.groups.forEach(function (g) {
      const ms = g.members.map(function (id) { return lay.coords[id]; }).filter(Boolean);
      if (!ms.length) return;
      g.x = Math.min.apply(null, ms.map(function (c) { return c.x; })) - 12;
      g.y = Math.min.apply(null, ms.map(function (c) { return c.y; })) - 28;
      g.w = Math.max.apply(null, ms.map(function (c) { return c.x + c.w; })) - g.x + 12;
      g.h = Math.max.apply(null, ms.map(function (c) { return c.y + c.h; })) - g.y + 12;
      const el = Array.from(document.querySelectorAll("#dag .gbox")).find(function (e) { return e.getAttribute("data-group") === g.id; });
      if (!el) return;
      const rect = el.querySelector("rect"), label = el.querySelector("text");
      if (rect) { rect.setAttribute("x", g.x); rect.setAttribute("y", g.y); rect.setAttribute("width", g.w); rect.setAttribute("height", g.h); }
      if (label) { label.setAttribute("x", g.x + 10); label.setAttribute("y", g.y + 16); }
    });
    document.querySelectorAll("#dag .edge[data-from]").forEach(function (el) {
      const a = lay.coords[el.getAttribute("data-from")], b = lay.coords[el.getAttribute("data-to")];
      if (a && b) el.setAttribute("d", edgePath(a, b, 1, Number(el.getAttribute("data-lane-offset")) || 0));
    });
    document.querySelectorAll("#dag .edge-hit[data-from]").forEach(function (el) {
      const a = lay.coords[el.getAttribute("data-from")], b = lay.coords[el.getAttribute("data-to")];
      if (a && b) el.setAttribute("d", edgePath(a, b, 1, Number(el.getAttribute("data-lane-offset")) || 0));
    });
    const keyBus = document.querySelector("#dag .keybus");
    if (keyBus) {
      const keys = Object.keys(lay.coords).filter(function (id) { return lay.coords[id].key; });
      const rest = Object.keys(lay.coords).filter(function (id) { return !lay.coords[id].key; });
      keyBus.setAttribute("d", keyBusPath(lay.coords, keys, rest));
    }
    positionCategory();
  }
  function saveMovedLayout() {
    if (!ui.dataset || !S || !S.datasets[ui.dataset] || ui.dagReview || (ui.layout && (ui.layout.viewFilter || ui.layout._layerKey))) return;
    const ds = ui.dataset;
    const positions = S.datasets[ds].layout || {};
    if (ui.layoutSave) clearTimeout(ui.layoutSave);
    ui.layoutSave = setTimeout(function () {
      api("/api/graph/layout", { dataset: ds, positions: positions }).catch(showErr);
      ui.layoutSave = null;
    }, 250);
  }
  function bindDag() {
    const svg = document.getElementById("dag");
    applyView();
    const wrap = document.getElementById("dagwrap");
    if (wrap && window.ResizeObserver) {
      if (window.dualdagGraphResizeObserver) window.dualdagGraphResizeObserver.disconnect();
      window.dualdagGraphResizeObserver = new ResizeObserver(function () { if (!ui.userView && ui.tab === "spec") fitView(); });
      window.dualdagGraphResizeObserver.observe(wrap);
    }
    if (!ui.didFit) {
      ui.didFit = true;
      requestAnimationFrame(function () { if (!ui.userView) fitView(); });
    }
    if (!svg) return;
    svg.addEventListener("contextmenu", function (e) {
      const node = e.target && e.target.closest ? e.target.closest(".node") : null;
      if (parameterMode() || !node) return;
      e.preventDefault(); e.stopPropagation();
      const r = wrap.getBoundingClientRect();
      ui.dagContext = { x: e.clientX - r.left, y: e.clientY - r.top,
        node: node ? node.getAttribute("data-id") : "", wrapLeft: r.left, wrapTop: r.top };
      render();
    });
    svg.addEventListener("wheel", function () { if (ui.dagContext) { ui.dagContext = null; render(); } }, { passive: true });
    svg.querySelectorAll(".cat-toggle").forEach(function (el) {
      el.addEventListener("pointerdown", function (e) { e.stopPropagation(); });
      el.addEventListener("click", function (e) {
        e.stopPropagation();
        const id = el.getAttribute("data-category");
        ui.category = { dataset: ui.dataset, node: id, data: null, error: "" };
        render();
        api("/api/node/categories?dataset=" + encodeURIComponent(ui.dataset) + "&node=" + encodeURIComponent(id))
          .then(function (data) { if (ui.category && ui.category.node === id) { ui.category.data = data; render(); } })
          .catch(function (err) { if (ui.category && ui.category.node === id) { ui.category.error = err.message || String(err); render(); } });
      });
    });
    svg.addEventListener("wheel", function (e) {
      e.preventDefault();
      ui.userView = true;
      const rect = svg.getBoundingClientRect();
      const mx = e.clientX - rect.left, my = e.clientY - rect.top;
      const nk = clamp(ui.view.k * (e.deltaY < 0 ? 1.08 : 1 / 1.08), 0.28, 2.6);
      ui.view.x = mx - (mx - ui.view.x) * (nk / ui.view.k);
      ui.view.y = my - (my - ui.view.y) * (nk / ui.view.k);
      ui.view.k = nk;
      applyView();
    }, { passive: false });
    svg.addEventListener("pointerdown", function (e) {
      if (e.button != null && e.button !== 0) return;
      const hit = e.target && e.target.closest ? e.target.closest(".node") : null;
      if (hit && hit.getAttribute("data-defined") === "false") { e.preventDefault(); e.stopPropagation(); return; }
      const grp = !hit && e.target && e.target.closest ? e.target.closest(".gbox") : null;
      const members = grp ? (grp.getAttribute("data-members") || "").split(",").filter(Boolean)
        : hit ? [hit.getAttribute("data-id")] : [];
      const starts = {};
      members.forEach(function (id) { const c = ui.layout && ui.layout.coords[id]; if (c) starts[id] = { x: c.x, y: c.y }; });
      ui.drag = {
        x: e.clientX, y: e.clientY, vx: ui.view.x, vy: ui.view.y, moved: false, id: e.pointerId,
        node: hit ? hit.getAttribute("data-id") : "",
        group: grp ? (grp.getAttribute("data-group") || "") : "",
        members: grp ? (grp.getAttribute("data-members") || "") : "", starts: starts
      };
      if (wrap) wrap.classList.add("drag");
      try { svg.setPointerCapture(e.pointerId); } catch (err) { /* ignore */ }
    });
    svg.addEventListener("pointermove", function (e) {
      if (!ui.drag || ui.drag.id !== e.pointerId) return;
      const dx = e.clientX - ui.drag.x, dy = e.clientY - ui.drag.y;
      if (Math.abs(dx) + Math.abs(dy) > 4) ui.drag.moved = true;
      if (!ui.drag.moved) return;
      const queue = document.getElementById("run-queue");
      const overQueue = !!(ui.drag.node && !batchRunning() && queue && (function () { const r = queue.getBoundingClientRect(); return e.clientX >= r.left && e.clientX <= r.right && e.clientY >= r.top && e.clientY <= r.bottom; })());
      if (ui.runQueueOver !== overQueue) { ui.runQueueOver = overQueue; if (queue) queue.classList.toggle("drop-over", overQueue); }
      ui.userView = true;
      if (ui.drag.node || ui.drag.group) {
        Object.keys(ui.drag.starts).forEach(function (id) {
          const start = ui.drag.starts[id], c = ui.layout.coords[id];
          c.x = start.x + dx / ui.view.k; c.y = start.y + dy / ui.view.k;
        });
        updateLiveGraph();
      } else {
        ui.view.x = ui.drag.vx + dx;
        ui.view.y = ui.drag.vy + dy;
        applyView();
      }
    });
    svg.addEventListener("pointerup", function (e) {
      if (!ui.drag || ui.drag.id !== e.pointerId) return;
      const moved = ui.drag.moved;
      const starts = ui.drag.starts;
      const nodeId = ui.drag.node;
      const groupId = ui.drag.group;
      const members = ui.drag.members ? ui.drag.members.split(",").filter(Boolean) : [];
      const qel = document.getElementById("run-queue"), qr = qel && qel.getBoundingClientRect();
      const queueDrop = !!(nodeId && moved && !batchRunning() && qr && e.clientX >= qr.left && e.clientX <= qr.right && e.clientY >= qr.top && e.clientY <= qr.bottom);
      ui.runQueueOver = false;
      ui.drag = null;
      if (wrap) wrap.classList.remove("drag");
      if (queueDrop) {
        Object.keys(starts).forEach(function (id) { if (ui.layout && ui.layout.coords[id]) { ui.layout.coords[id].x = starts[id].x; ui.layout.coords[id].y = starts[id].y; } });
        const rawGraph = curGraph(), node = asRecords(rawGraph && rawGraph.nodes).find(function (n) { return n.id === nodeId; });
        if (node && node.defined !== false && node.defined !== "false") {
          const ids = queueFor(ui.dataset);
          if (ids.indexOf(nodeId) >= 0) setMsg(queueNodeLabel(ui.dataset, nodeId) + " is already in the queue.");
          else { ids.push(nodeId); ui.runQueues[ui.dataset] = ids; saveRunQueues(); }
        }
        updateLiveGraph(); render(); return;
      }
      if (qel) qel.classList.remove("drop-over");
      if (moved) {
        if (nodeId || groupId) {
          if (ui.layout && ui.layout.viewFilter) { render(); return; }
          const positions = ui.layout && ui.layout._layerKey
            ? (ui.layerLayouts[ui.layout._layerKey] || (ui.layerLayouts[ui.layout._layerKey] = {}))
            : (curPack().layout || (curPack().layout = {}));
          Object.keys(starts).forEach(function (id) {
            const c = ui.layout.coords[id];
            positions[id] = { x: c.x, y: c.y };
          });
          saveMovedLayout();
          render();
        }
        return;
      }
      const multi = !!(ui.multiMode || e.ctrlKey || e.metaKey || e.shiftKey);
      if (nodeId) {
        const keyRecord = recordsNodeForKey(ui.dataset, nodeId);
        if (keyRecord) {
          if (multi) { toggleSelection(keyRecord); render(); }
          else { ui.category = null; setSelection([keyRecord]); pickNode(keyRecord, false).catch(showErr); }
          return;
        }
        if (parameterMode()) {
          const hitNode = (curParamGraph() && asRecords(curParamGraph().nodes).find(function (n) { return n.code === nodeId; }));
          if (!hitNode || hitNode.defined === false) return;
          ui.graphMode[ui.dataset] = "variable";
          if (curGraph() && curGraph().layered === true) ui.layerChoice[ui.dataset] = nodeId;
          else ui.paramFilter[ui.dataset] = nodeId;
          ui.didFit = false; ui.userView = false; ui.sel = []; ui.category = null; render();
          return;
        }
        if (multi) {
          toggleSelection(nodeId);
          render();
        } else {
          ui.category = null;
          setSelection([nodeId]);
          prefillDag(nodeId);
          pickNode(nodeId, false).catch(showErr);
        }
        return;
      }
      if (groupId && (ui.multiMode || e.ctrlKey || e.metaKey)) {
        addSelection(members);
        render();
      }
    });
    svg.addEventListener("pointercancel", function (e) {
      if (ui.drag && ui.drag.id === e.pointerId) ui.drag = null;
      if (wrap) wrap.classList.remove("drag");
      ui.runQueueOver = false;
    });
  }

  function schedulePoll() {
    if (ui.poll) clearTimeout(ui.poll);
    ui.poll = null;
    if ((S && S.busy) || batchRunning()) ui.poll = setTimeout(function () { refresh().catch(showErr); }, 2000);
  }
  async function loadCatalog() {
    try {
      const cat = await api("/api/catalog");
      ui.catalog = cat && typeof cat === "object" ? cat : null;
      ui.catalogStatus = ui.catalog ? "ok" : "missing";
    } catch (e) {
      ui.catalog = null;
      ui.catalogStatus = (e && (e.token || e.status === 404)) ? "missing" : "error";
    }
  }
  async function ensureProgram() {
    if (!ui.dataset) return;
    if (ui.program && ui.programDs === ui.dataset) return;
    ui.programErr = "";
    ui.program = await api("/api/dataset/code?dataset=" + encodeURIComponent(ui.dataset));
    ui.programDs = ui.dataset;
  }
  async function loadDiff(opts) {
    opts = opts || {};
    const node = syncMismatchSelection();
    if (!ui.dataset || !node) {
      ui.diff.view = null;
      ui.diff.err = "";
      ui.diff.loading = false;
      if (opts.render) render();
      return;
    }
    ui.diff.node = node;
    ui.diff.loading = true;
    const q = "/api/node/inputs?dataset=" + encodeURIComponent(ui.dataset)
      + "&node=" + encodeURIComponent(node)
      + "&page=" + (ui.diff.page || 1)
      + "&size=" + (ui.diff.size || 100)
      + "&only_diff=" + (ui.diff.onlyDiff ? "1" : "0")
      + "&filter=" + encodeURIComponent(ui.diff.filter || "");
    try {
      ui.diff.view = await api(q);
      ui.diff.err = "";
    } catch (e) {
      ui.diff.view = null;
      ui.diff.err = (e && e.token) ? "" : ((e && e.message) || "Could not load the mismatch inspector");
    }
    ui.diff.loading = false;
    if (ui.diff.filter) ui.keepFocus = "diff-filter";
    if (opts.render !== false) render();
  }
  async function refresh() {
    const previousDataset = ui.dataset;
    S = await api("/api/state");
    ensureDataset();
    if (ui.dataset !== previousDataset) {
      ui.fullRun.status = null;
      ui.fullRun.steps = {};
      ui.fullRun.statusError = "";
      if (ui.fullRun.loadingDataset !== ui.dataset) ui.fullRun.loadingDataset = "";
    }
    if (ui.tab === "run" && ui.dataset && (!ui.fullRun.status || ui.fullRun.status.dataset !== ui.dataset)
        && ui.fullRun.loadingDataset !== ui.dataset) loadFullRunStatus(false);
    reconcileDagEditNotes(S);
    reconcileRunQueues(S);
    if (ui.dataset) {
      try {
        const pg = await api("/api/param-graph?dataset=" + encodeURIComponent(ui.dataset));
        ui.paramGraphs[ui.dataset] = pg;
        if (pg && pg.turns) ui.paramTurns[ui.dataset] = asRecords(pg.turns);
        delete ui.paramErrors[ui.dataset];
      } catch (e) { ui.paramGraphs[ui.dataset] = ui.paramGraphs[ui.dataset] || null; }
    }
    await loadCatalog();
    try {
      const qc = await api("/api/qc");
      ui.qcList = asRecords(qc);
    } catch (e) { ui.qcList = ui.qcList || []; }
    ensureDataset();
    syncExtractTimer();
    if (ui.programDs && ui.programDs !== ui.dataset) {
      ui.program = null;
      ui.programDs = "";
    }
    if (!ui.node && ui.wantNode) {
      const g0 = curGraph();
      const want = ui.wantNode;
      if (g0 && asRecords(g0.nodes).some(function (n) { return n.id === want; })) {
        ui.node = want;
        setSelection([want]);
        ui.wantNode = "";
      }
    }
    if (ui.sel && ui.sel.length) {
      const have = {};
      asRecords(curGraph() && curGraph().nodes).forEach(function (n) { have[n.id] = true; });
      ui.sel = ui.sel.filter(function (id) { return have[id]; });
    }
    if (ui.tab === "data") {
      await loadDatasets();
      if (ui.data.openId) await openView();
    }
    if (ui.node) {
      const g = curGraph();
      const ok = g && asRecords(g.nodes).some(function (n) { return n.id === ui.node; });
      if (!ok) { ui.node = null; ui.detail = null; }
      else {
        try {
          ui.detail = await api("/api/node/detail?dataset=" + encodeURIComponent(ui.dataset) + "&node=" + encodeURIComponent(ui.node));
        } catch (e2) { /* keep previous detail */ }
      }
    }
    if (ui.tab === "code" && ui.codeScope === "all") {
      try { await ensureProgram(); }
      catch (e) { ui.program = null; ui.programErr = (e && e.token) ? "" : ((e && e.message) || "Could not load the dataset program"); }
    }
    if (ui.tab === "mismatches") {
      syncMismatchSelection();
      await loadDiff({ render: false });
    }
    if (!tabPinned) {
      ui.tab = (S && S.spec) ? "spec" : "settings";
      tabPinned = true;
    }
    render();
    syncHash();
    schedulePoll();
    maybeAutodag();
  }
  async function loadDatasets() {
    try {
      const rows = await api("/api/datasets?side=" + encodeURIComponent(ui.data.side));
      ui.data.list = asRecords(rows);
      ui.data.missing = false;
      ui.data.err = "";
      const list = ui.data.list;
      const node = ui.node && asRecords(S && S.datasets && S.datasets[ui.dataset] && S.datasets[ui.dataset].graph && S.datasets[ui.dataset].graph.nodes).find(function (n) { return n.id === ui.node; });
      let wanted = ui.data.openId && list.some(function (r) { return r.id === ui.data.openId; }) ? ui.data.openId : null;
      if (!wanted && node && node.paramcd) wanted = "layer:" + ui.dataset + ":" + node.paramcd + ":" + ui.data.side;
      const inDataset = list.filter(function (r) { return !ui.dataset || r.dataset === ui.dataset; });
      if (!wanted) wanted = (inDataset.find(function (r) { return r.kind === "parameter"; }) || inDataset.find(function (r) { return r.kind === "generated"; }) || list.find(function (r) { return r.kind === "parameter"; }) || list.find(function (r) { return r.kind === "generated"; }) || {}).id || null;
      if (!wanted && ui.dataset) wanted = "frame:" + ui.dataset + ":" + ui.data.side;
      ui.data.openId = wanted;
    } catch (e) {
      ui.data.list = [];
      if (e && e.status === 404) { ui.data.missing = true; ui.data.err = ""; }
      else { ui.data.missing = false; ui.data.err = (e && e.token) ? "" : ((e && e.message) || "Could not list datasets"); }
    }
  }
  let viewSeq = 0;
  async function openView(id) {
    const seq = ++viewSeq;
    if (id && id !== ui.data.openId) {
      ui.data.offset = 0;
      ui.data.filter = "";
      ui.data.colsOn = null;
      ui.data.allCols = null;
    }
    if (id) ui.data.openId = id;
    if (!ui.data.openId) { render(); return; }
    const cols = colsQuery();
    ui.data.loading = true;
    render();
    const filter = ui.data.filter || "";
    const offset = ui.data.offset || 0;
    try {
      const v = await api("/api/datasets/view?id=" + encodeURIComponent(ui.data.openId)
        + "&offset=" + offset + "&limit=" + (ui.data.limit || 100)
        + "&filter=" + encodeURIComponent(filter)
        + (cols ? "&cols=" + encodeURIComponent(cols) : ""));
      if (seq !== viewSeq) return;
      ui.data.view = v;
      ui.data.viewErr = "";
      if (!cols && v && v.columns) ui.data.allCols = asRecords(v.columns);
    } catch (e) {
      if (seq !== viewSeq) return;
      ui.data.view = null;
      ui.data.viewErr = (e && e.token) ? "" : ((e && e.status === 404) ? "The data API is not ready yet." : ((e && e.message) || "Could not open"));
    }
    if (seq !== viewSeq) return;
    ui.data.loading = false;
    if (filter) ui.keepFocus = "data-filter";
    render();
  }
  function setTab(tab) {
    const previousTab = ui.tab;
    if (ui.tab === "run" && normTab(tab) !== "run") stopFullRunPoll();
    ui.tab = normTab(tab);
    if (ui.tab === "mismatches") syncMismatchSelection();
    if (ui.tab === "spec") ui.didFit = false;
    syncHash();
    render();
    if (ui.tab === "run" && previousTab !== "run") loadFullRunStatus(false);
    if (ui.tab === "code" && ui.codeScope === "all") {
      ensureProgram().then(function () { render(); }).catch(function (e) {
        ui.programErr = (e && e.token) ? "" : ((e && e.message) || "Could not load the dataset program");
        render();
      });
    }
    if (ui.tab === "data") {
      loadDatasets().then(function () {
        render();
        if (ui.data.openId) return openView();
      }).catch(showErr);
    }
    if (ui.tab === "mismatches") {
      syncMismatchSelection();
      loadDiff().catch(showErr);
    }
  }
  function stopRunPoll() {
    if (ui.runPoll) { clearTimeout(ui.runPoll); ui.runPoll = null; }
  }
  function syncCoderWindowPoll() {
    const w = ui.coderWindow;
    const variableRunning = w && ui.running && ui.dataset === w.dataset && ui.node === w.node
      && !ui.dagAsking && !ui.paramAsking;
    const needed = !!(w && (variableRunning || w.sending || (w.data && w.data.running)));
    if (!needed) {
      if (ui.coderWindowTimer) clearInterval(ui.coderWindowTimer);
      ui.coderWindowTimer = null;
    } else if (!ui.coderWindowTimer) {
      ui.coderWindowTimer = setInterval(function () {
        if (ui.coderWindowLoad) ui.coderWindowLoad().catch(function () {});
      }, 2000);
    }
  }
  function patchLogs() {
    ["primary", "validation"].forEach(function (side) {
      const el = document.querySelector('pre.log[data-side="' + side + '"]');
      if (!el) return;
      const text = sideText(ui.detail && ui.detail.log, side) || (ui.running ? "Running…" : "No log yet");
      if (el.textContent !== text) {
        const followEnd = el.scrollHeight - el.clientHeight - el.scrollTop < 40;
        const info = renderConsoleLog(text);
        el.innerHTML = info.html;
        if (followEnd) el.scrollTop = el.scrollHeight;
        const badges = document.querySelector('.logbadges[data-side="' + side + '"]');
        if (badges) badges.innerHTML = logBadges(side, info);
      }
    });
  }
  function startRunPoll() {
    stopRunPoll();
    const node = ui.node, ds = ui.dataset, gen = ui.runGen;
    async function tick() {
      if (!ui.running || ui.runGen !== gen || ui.node !== node) { ui.runPoll = null; return; }
      try {
        const d = await api("/api/node/detail?dataset=" + encodeURIComponent(ds) + "&node=" + encodeURIComponent(node));
        if (ui.running && ui.runGen === gen && ui.node === node && d) ui.detail = d;
        try {
          const st = await api("/api/state");
          if (ui.running && ui.runGen === gen && st) { S = st; ensureDataset(); reconcileDagEditNotes(S); }
        } catch (e2) { /* state can lag a running job */ }
        if (ui.running && ui.runGen === gen && ui.tab === "code") {
          if (ui.asking) render();
          else patchLogs();
        }
      } catch (e) { /* keep waiting on the run */ }
      if (ui.running && ui.runGen === gen && ui.node === node) ui.runPoll = setTimeout(tick, 1500);
    }
    ui.runPoll = setTimeout(tick, 700);
  }
  async function pickNode(id, fromSpec) {
    if (ui.coderWindow && (ui.coderWindow.node !== id || ui.coderWindow.dataset !== ui.dataset)) {
      if (ui.coderWindowTimer) clearInterval(ui.coderWindowTimer);
      ui.coderWindowTimer = null; ui.coderWindow = null; ui.coderWindowLoad = null;
    }
    const recordsId = recordsNodeForKey(ui.dataset, id);
    if (recordsId) {
      id = recordsId;
      setSelection([id]);
    }
    ui.node = id;
    ui.fromSpec = !!fromSpec;
    if (!fromSpec) {
      ui.pin = {};
      ui.pin[id] = true;
    }
    ui.detail = null;
    render();
    panNodeIntoView(id);
    const d = await api("/api/node/detail?dataset=" + encodeURIComponent(ui.dataset) + "&node=" + encodeURIComponent(id));
    if (ui.node !== id) return;
    ui.detail = d;
    const specRows = normalizeSpecRows(d && d.spec_rows);
    if (!fromSpec && specRows.length) ui.selRows = specRows;
    render();
    panNodeIntoView(id);
    if (ui.selRows && ui.selRows.length) {
      const r0 = ui.selRows[0];
      ui.scrollSpec = { file: r0.file, row: r0.row };
      const q = document.querySelector('#spec-scroll tr[data-file="' + r0.file + '"][data-row="' + r0.row + '"]');
      if (q) q.scrollIntoView({ block: "center" });
    }
    if (!ui.diff.node) ui.diff.node = id;
    syncHash();
  }
  function serverError(e, fallback) {
    if (!e || e.token) return "";
    let msg = "";
    const raw = e.data && e.data.error;
    if (Array.isArray(raw)) msg = raw.filter(Boolean).join(" ");
    else if (raw) msg = String(raw);
    if (!msg && e.message) msg = String(e.message);
    msg = String(msg || "").trim();
    if (!msg || /^\d{3}$/.test(msg)) return fallback || "Request failed";
    return msg;
  }
  async function runNode(kind, extra) {
    if (!ui.node || !ui.dataset) return;
    const body = { dataset: ui.dataset, node: ui.node };
    if (extra) Object.keys(extra).forEach(function (k) { body[k] = extra[k]; });
    if (kind === "generate" && !body.side) body.side = "both";
    const labels = { generate: "Generate & run", compare: "Compare", investigate: "Investigate" };
    const label = labels[kind] || kind;
    ui.runGen = (ui.runGen || 0) + 1;
    ui.running = true;
    syncCoderWindowPoll();
    setMsg(label + "…");
    render();
    startRunPoll();
    let msg = label + " finished";
    try {
      const r = await api("/api/node/" + kind, body);
      if (kind === "investigate" && r && r.report) {
        ui.detail = ui.detail || {};
        ui.detail.report = r.report;
      }
    } catch (e) {
      msg = serverError(e, label + " failed");
    } finally {
      ui.running = false;
      syncCoderWindowPoll();
      stopRunPoll();
    }
    await refresh();
    if (ui.coderWindowLoad) await ui.coderWindowLoad();
    setMsg(msg);
  }
  async function rerunStored(side) {
    if (!ui.node || !ui.dataset) return;
    const which = side || ui.runSide || "both";
    const need = which === "both" ? ["primary", "validation"] : [which];
    for (let i = 0; i < need.length; i++) {
      if (!hasStored(need[i])) { setMsg("No saved " + need[i] + " code"); render(); return; }
    }
    ui.runGen = (ui.runGen || 0) + 1;
    ui.running = true;
    syncCoderWindowPoll();
    setMsg("Re-running…");
    render();
    startRunPoll();
    let msg = "Re-run finished";
    try {
      await api("/api/node/run", { dataset: ui.dataset, node: ui.node, side: which });
    } catch (e) {
      msg = serverError(e, e && e.status === 404 ? "Re-run is not available yet" : "Re-run failed");
    } finally {
      ui.running = false;
      syncCoderWindowPoll();
      stopRunPoll();
    }
    await refresh();
    if (ui.coderWindowLoad) await ui.coderWindowLoad();
    setMsg(msg);
  }
  function openModal(html) {
    const m = document.getElementById("modal");
    document.getElementById("mbody").innerHTML = html;
    m.hidden = false;
    m.classList.add("on");
  }
  function closeModal() {
    const m = document.getElementById("modal");
    if (!m) return;
    m.classList.remove("on");
    m.hidden = true;
    document.getElementById("mbody").innerHTML = "";
  }
  function scrollToSpec(file, row) {
    ui.tab = "spec";
    ui.fold.spec = false;
    ui.specTab = /parameter/i.test(file || "") ? "parameter" : "variable";
    ui.selRows = [{ file: file, row: row }];
    ui.scrollSpec = { file: file, row: row };
    syncHash();
    render();
  }

  function batchResultsHtml() {
    const rows = batchRows(S && S.batch);
    let body = "<table><thead><tr><th>Variable</th><th>Level</th><th>Message</th></tr></thead><tbody>";
    if (!rows.length) body += '<tr><td colspan="3">No results</td></tr>';
    rows.forEach(function (r) {
      body += '<tr class="click" data-act="batch-open" data-node="' + esc(r.node) + '"><td>' + esc(displayNode(r.node))
        + "</td><td>" + esc(r.level || "") + "</td><td>" + esc(r.message || "") + "</td></tr>";
    });
    return "<h3>Batch results</h3>" + body + "</tbody></table>"
      + '<div class="rowbtns">' + btn("modal-close", "Close") + "</div>";
  }
  async function startBatch() {
    const ids = (ui.sel || []).slice();
    if (!ids.length || !ui.dataset) { setMsg("Select variables first"); return; }
    if (batchRunning()) { setMsg("A batch is already running."); return; }
    await api("/api/batch", {
      dataset: ui.dataset,
      nodes: ids,
      compare: ui.compareAfter !== false
    });
    await refresh();
    setMsg("Batch started");
  }
  async function runQueue() {
    if (!ui.dataset) return;
    if (batchRunning()) {
      if (!S.batch || S.batch.dataset !== ui.dataset) { setMsg("A batch is already running for another dataset."); return; }
      await api("/api/batch/cancel", {});
      await refresh();
      setMsg("Queue will stop after the current step");
      return;
    }
    const ids = queueFor(ui.dataset).slice();
    if (!ids.length) return;
    clearQueueResultHides(ui.dataset);
    await api("/api/batch", { dataset: ui.dataset, nodes: ids, compare: true, keep_order: true });
    await refresh();
    setMsg("Run queue started");
  }
  function bindRunQueue() {
    const panel = document.getElementById("run-queue");
    if (!panel) return;
    panel.querySelectorAll(".run-queue-row").forEach(function (row) {
      row.addEventListener("pointerdown", function (e) {
        if (batchRunning() || e.button !== 0 || (e.target.closest && e.target.closest(".run-queue-remove"))) return;
        const startX = e.clientX, startY = e.clientY;
        let started = false;
        const nodeId = row.getAttribute("data-queue-node");
        const update = function (ev) {
          if (!started && Math.hypot(ev.clientX - startX, ev.clientY - startY) < 4) return;
          if (!started) {
            started = true;
            ui.runQueueDragging = nodeId;
            ui.runQueueDragOutside = false;
            try { row.setPointerCapture(e.pointerId); } catch (err) { /* Document listeners provide fallback tracking. */ }
            row.classList.add("dragging");
          }
          const currentPanel = document.getElementById("run-queue");
          const r = currentPanel && currentPanel.getBoundingClientRect();
          if (!r) return;
          const outside = ev.clientX < r.left || ev.clientX > r.right || ev.clientY < r.top || ev.clientY > r.bottom;
          if (outside !== ui.runQueueDragOutside) {
            ui.runQueueDragOutside = outside;
            row.classList.toggle("drag-outside", outside);
            const hint = row.querySelector(".run-queue-drag-hint");
            if (outside && !hint) { const h = document.createElement("small"); h.className = "run-queue-drag-hint"; h.textContent = "Release to remove"; row.appendChild(h); }
            else if (!outside && hint) hint.remove();
          }
        };
        let finish, cancel;
        const keydown = function (ev) { if (ev.key === "Escape") { ev.preventDefault(); cancel(); } };
        const cleanup = function () {
          document.removeEventListener("pointermove", update, true);
          document.removeEventListener("pointerup", finish, true);
          document.removeEventListener("pointercancel", cancel, true);
          document.removeEventListener("keydown", keydown, true);
        };
        const reset = function () { cleanup(); if (started) { ui.runQueueDragging = ""; ui.runQueueDragOutside = false; render(); } };
        finish = function (ev) {
          update(ev);
          const from = started ? nodeId : "";
          const currentPanel = document.getElementById("run-queue");
          if (started && !batchRunning() && from && currentPanel) {
            if (ui.runQueueDragOutside) {
              hideQueueResult(ui.dataset, from);
              ui.runQueues[ui.dataset] = queueFor(ui.dataset).filter(function (id) { return id !== from; });
              saveRunQueues();
            } else {
              const target = document.elementFromPoint(ev.clientX, ev.clientY);
              const targetRow = target && target.closest ? target.closest(".run-queue-row") : null;
              if (targetRow && currentPanel.contains(targetRow)) {
                const to = targetRow.getAttribute("data-queue-node"), ids = queueFor(ui.dataset).slice();
                const a = ids.indexOf(from), b = ids.indexOf(to);
                if (a >= 0 && b >= 0 && a !== b) { ids.splice(a, 1); ids.splice(b, 0, from); ui.runQueues[ui.dataset] = ids; saveRunQueues(); }
              }
            }
          }
          reset();
        };
        cancel = function () { reset(); };
        document.addEventListener("pointermove", update, true);
        document.addEventListener("pointerup", finish, true);
        document.addEventListener("pointercancel", cancel, true);
        document.addEventListener("keydown", keydown, true);
      });
    });
  }
  async function openCoderWindow(side, initialMessage) {
    if (ui.coderWindowTimer) clearInterval(ui.coderWindowTimer);
    side = side === "validation" ? "validation" : "primary";
    const dataset = ui.dataset, node = ui.node;
    ui.coderWindow = { side: side, dataset: dataset, node: node, data: null, error: "",
      selectedAttempt: null, manualAttempt: false, draft: "", sending: false, detailsOpen: false,
      followNewest: true, optimisticMessage: "", optimisticThreadLength: 0 };
    const load = async function () {
      const w = ui.coderWindow;
      if (!w || w.side !== side) return;
      try {
        const attempt = w.manualAttempt && w.selectedAttempt ? "&attempt=" + encodeURIComponent(w.selectedAttempt) : "";
        const d = await api("/api/node/coder-window?dataset=" + encodeURIComponent(dataset)
          + "&node=" + encodeURIComponent(node) + "&side=" + encodeURIComponent(side) + attempt);
        if (ui.coderWindow === w) {
          const changed = JSON.stringify(w.data) !== JSON.stringify(d) || !!w.error;
          if (!w.manualAttempt) w.selectedAttempt = d.context_index;
          w.data = d; w.error = "";
          syncCoderWindowPoll();
          if (changed) render();
        }
      } catch (err) {
        if (ui.coderWindow === w) {
          const message = err.message || String(err);
          if (w.error !== message) { w.error = message; render(); }
        }
      }
    };
    ui.coderWindowLoad = load;
    render(); await load();
    syncCoderWindowPoll();
    if (initialMessage) {
      ui.coderWindow.draft = initialMessage;
      await sendCoderMessage();
    }
  }

  async function dispatch(act, t, ev) {
    if (act === "queue-run") { await runQueue(); return; }
    if (act === "queue-clear") { if (!batchRunning()) { queueFor(ui.dataset).forEach(function (id) { hideQueueResult(ui.dataset, id); }); ui.runQueues[ui.dataset] = []; saveRunQueues(); render(); } return; }
    if (act === "queue-remove") { if (!batchRunning()) { hideQueueResult(ui.dataset, t.getAttribute("data-node")); ui.runQueues[ui.dataset] = queueFor(ui.dataset).filter(function (id) { return id !== t.getAttribute("data-node"); }); saveRunQueues(); render(); } return; }
    if (act === "queue-open") {
      const id = t.getAttribute("data-node") || "";
      if (!id) return;
      ui.graphMode[ui.dataset] = "variable"; ui.layerChoice[ui.dataset] = (asRecords(curGraph() && curGraph().nodes).find(function (n) { return n.id === id; }) || {}).paramcd || ui.layerChoice[ui.dataset];
      ui.tab = "spec"; ui.didFit = false; render(); setSelection([id]); await pickNode(id, false); return;
    }
    if (act === "copy-session") {
      const value = t.getAttribute("data-session") || "";
      if (!value) return;
      try {
        if (!navigator.clipboard || !navigator.clipboard.writeText) throw new Error("Clipboard unavailable");
        await navigator.clipboard.writeText(value);
        setMsg("Copied " + value);
      } catch (err) { setMsg(value); }
      return;
    }
    if (act === "coder-window") {
      const side = t.getAttribute("data-side") === "validation" ? "validation" : "primary";
      await openCoderWindow(side);
      return;
    }
    if (act === "coder-window-close") {
      if (ui.coderWindowTimer) clearInterval(ui.coderWindowTimer);
      ui.coderWindowTimer = null; ui.coderWindow = null; ui.coderWindowLoad = null; render(); return;
    }
    if (act === "coder-details-toggle" && ui.coderWindow) {
      ui.coderWindow.detailsOpen = !ui.coderWindow.detailsOpen;
      render(); return;
    }
    if (act === "coder-send") { await sendCoderMessage(); return; }
    if (act === "log-jump") {
      const side = t.getAttribute("data-side");
      const line = t.getAttribute("data-line");
      const log = document.querySelector('pre.log[data-side="' + side + '"]');
      const hit = log && log.querySelector('[data-log-line="' + line + '"]');
      if (hit) hit.scrollIntoView({ block: "center" });
      return;
    }
    if (act === "modal-close") { closeModal(); return; }
    if (act === "start-new-dag") {
      openModal('<h3>Reset</h3><p>This deletes everything stored for this project: all DAGs, programs, intermediate data, coder sessions, logs and exports. Only the spec and the raw data stay. This cannot be undone. Type CONFIRM to continue.</p><label for="start-new-dag-confirm">Type CONFIRM</label><input id="start-new-dag-confirm" type="text" autocomplete="off"><div class="rowbtns">' + btn("start-new-dag-confirm-action", "Reset", { cls: "pri danger-filled", disabled: true }) + btn("modal-close", "Cancel") + "</div>");
      return;
    }
    if (act === "start-new-dag-confirm-action") {
      if (document.getElementById("start-new-dag-confirm")?.value !== "CONFIRM") return;
      const ds = ui.dataset;
      t.disabled = true;
      const row = t.parentNode;
      if (row) row.insertAdjacentHTML("afterbegin", '<span class="extract-spinner" aria-hidden="true"></span><span class="start-new-dag-wait" aria-live="polite">Starting a new DAG for ' + esc(ds || "this project") + '…</span>');
      try {
        await api("/api/project/start-new", { confirm: "CONFIRM" });
        closeModal();
        if (ds) {
          try { sessionStorage.setItem("dualdag_start_new_dataset", ds); } catch (ignore) {}
        }
        location.reload();
      } catch (e) {
        t.disabled = false;
        const waitLabel = row && row.querySelector(".start-new-dag-wait");
        const spinner = row && row.querySelector(".extract-spinner");
        if (waitLabel) waitLabel.remove();
        if (spinner) spinner.remove();
        showErr(e);
      }
      return;
    }
    if (act === "reset") {
      if (!ui.node || !ui.dataset || !resetAllowed(ui.node) || busyNow()) return;
      openModal(
        "<h3>Reset " + esc(displayNode(ui.node)) + "</h3>"
        + "<p>Sets this variable back to Not generated and deletes its results. Downstream variables keep their state and are only marked that this one changed.</p>"
        + '<label><input id="reset-delete-code" type="checkbox"> Also delete the generated code and notes</label>'
        + '<div class="rowbtns">' + btn("reset-confirm", "Reset", { cls: "pri" }) + btn("modal-close", "Cancel") + "</div>"
      );
      return;
    }
    if (act === "browse") {
      const el = document.getElementById(t.getAttribute("data-target"));
      if (el) el.click();
      return;
    }
    if (act === "dataset") {
      stopFullRunPoll();
      ui.dataset = t.getAttribute("data-ds");
      ui.fullRun.status = null;
      ui.fullRun.steps = {};
      ui.fullRun.statusError = "";
      ui.dagReview = null; ui.dagContext = null;
      const selectedDataset = ui.dataset;
      ui.paramGraphs[selectedDataset] = null;
      api("/api/param-graph?dataset=" + encodeURIComponent(selectedDataset)).then(function (pg) { ui.paramGraphs[selectedDataset] = pg; if (ui.dataset === selectedDataset) render(); }).catch(function () {});
      syncExtractTimer();
      ui.node = null;
      ui.sel = [];
      ui.selectOpen = false;
      ui.detail = null;
      ui.program = null;
      ui.programDs = "";
      ui.diff.node = "";
      ui.diff.view = null;
      ui.pin = {};
      ui.category = null;
      ui.selRows = [];
      ui.didFit = false;
      ui.userView = false;
      render();
      if (ui.tab === "run") loadFullRunStatus(false);
      return;
    }
    if (act === "fullrun") {
      const side = t.getAttribute("data-side"), ds = ui.dataset;
      setFullRunSideError(ds, side, "");
      ui.fullRun.started[side] = Date.now();
      try { await api("/api/fullrun/run", { dataset: ds, side: side }); await loadFullRunStatus(true, true); }
      catch (e) {
        setFullRunSideError(ds, side, (e && e.message) || "Could not start full run");
        if (e && e.status === 404) ui.fullRun.status = { dataset: ds, sides: {}, noGraph: true };
        render();
      }
      return;
    }
    if (act === "fullrun-open-node") {
      const side = t.getAttribute("data-side"), id = t.getAttribute("data-id");
      ui.codeScope = "node";
      ui.runSide = side;
      setSelection([id]);
      setTab("code");
      try {
        await pickNode(id, false);
      } catch (e) { showErr(e); }
      return;
    }
    if (act === "fullrun-download-program") {
      try { await downloadFullRunProgram(t.getAttribute("data-side")); }
      catch (e) { showErr(e); }
      return;
    }
    if (act === "network-download") {
      const selected = String(ui.layerChoice[ui.dataset] || (asRecords(curGraph() && curGraph().layers)[0] || {}).paramcd || "").toUpperCase();
      try { await downloadNetwork(parameterMode() ? "" : selected); }
      catch (e) { setMsg(serverError(e, "Could not download network JSON")); }
      return;
    }
    if (act === "fullrun-text") {
      const side = t.getAttribute("data-side");
      viewFullRunText(side, t.getAttribute("data-what")).catch(function (e) {
        setFullRunSideError(ui.dataset, side, (e && e.message) || "Could not open full run text"); render();
      });
      return;
    }
    if (act === "clear-out") {
      if (ui.dataset) {
        delete ui.paramFilter[ui.dataset];
        ui.graphMode[ui.dataset] = "variable";
      }
      ui.sel = [];
      ui.node = null;
      ui.detail = null;
      ui.pin = {};
      ui.category = null;
      ui.didFit = false;
      ui.userView = false;
      render();
      return;
    }
    if (act === "spec-tab") { ui.specTab = t.getAttribute("data-tab") || "variable"; render(); return; }
    if (act === "spec-all") { ui.specAll = !ui.specAll; render(); return; }
    if (act === "zoom") { zoomBy(parseFloat(t.getAttribute("data-k") || "1")); return; }
    if (act === "category-close") { ui.category = null; render(); return; }
    if (act === "tab") { setTab(t.getAttribute("data-tab")); return; }
    if (act === "super-agent") { openSuperAgent(ui.tab); return; }
    if (act === "qc-pick") { await pickNode(t.getAttribute("data-id"), false); return; }
    if (act === "qc-goto-diff") { ui.diff.node = ui.node; setTab("mismatches"); return; }
    if (act === "err-dismiss" || act === "err-open") {
      ui.errSeen = S && S.last_error ? S.last_error.time : null;
      if (act === "err-open") {
        const ds = t.getAttribute("data-dataset");
        if (ds) ui.dataset = ds;
        const opened = t.getAttribute("data-node");
        setTab(SHOW_QC_TAB && S && S.last_error && S.last_error.kind === "investigate" ? "qc" : "code");
        setSelection([opened]);
        pickNode(opened, false).catch(showErr);
        return;
      }
      render();
      return;
    }
    if (act === "fold") {
      const key = t.getAttribute("data-pane") || "";
      ui.fold[key] = !ui.fold[key];
      render();
      return;
    }
    if (act === "classification-toggle") {
      ui.parameterClassificationCollapsed = !ui.parameterClassificationCollapsed;
      try { localStorage.setItem("dualdag.parameterClassificationCollapsed", ui.parameterClassificationCollapsed ? "1" : "0"); } catch (err) { /* The panel remains usable for this session. */ }
      render();
      return;
    }
    if (act === "layer-select") {
      if (ui.dataset && (ui.graphMode[ui.dataset] || "variable") === "variable") {
        let layer = String(t.getAttribute("data-layer") || "").toUpperCase();
        const graph = curGraph(), layers = asRecords(graph && graph.layers);
        if (layer === "CARRY_OVER" && graph && graph.layered === true && !layers.some(function (item) { return String(item.paramcd || "").toUpperCase() === layer; })) {
          layer = carryOverParameters(graph)[0] || layer;
        }
        ui.layerChoice[ui.dataset] = layer; ui.didFit = false; ui.userView = false; ui.sel = []; ui.node = null; ui.detail = null;
      }
      render();
      return;
    }
    if (act === "goto-diff") {
      ui.diff.focus = true;
      ui.fold.diff = false;
      if (ui.node) ui.diff.node = ui.node;
      ui.diff.page = 1;
      ui.data.filterNode = "";
      setTab("mismatches");
      return;
    }
    if (act === "diff-pick") {
      const id = t.getAttribute("data-id") || "";
      if (!id) return;
      ui.diff.node = id;
      ui.diff.page = 1;
      ui.diff.focus = true;
      if (ui.node !== id) {
        setSelection([id]);
        await pickNode(id, false);
      }
      await loadDiff();
      return;
    }
    if (act === "code-scope") {
      ui.codeScope = t.getAttribute("data-scope") === "all" ? "all" : "var";
      syncHash();
      if (ui.codeScope === "all") {
        try { await ensureProgram(); ui.programErr = ""; }
        catch (e) { ui.program = null; ui.programErr = (e && e.token) ? "" : ((e && e.message) || "Could not load the dataset program"); }
      }
      render();
      return;
    }
    if (act === "code-jump") {
      const id = t.getAttribute("data-id");
      ui.codeScope = "var";
      syncHash();
      if (id) { setSelection([id]); await pickNode(id, false); }
      else render();
      return;
    }
    if (act === "diff-page") {
      const delta = parseInt(t.getAttribute("data-delta") || "0", 10) || 0;
      const v = ui.diff.view || {};
      const page = v.page || ui.diff.page || 1;
      const size = v.size || ui.diff.size || 100;
      const pages = Math.max(1, Math.ceil((v.nrow || 0) / size));
      let next = page + delta;
      if (next < 1 || next > pages) return;
      ui.diff.page = next;
      await loadDiff();
      return;
    }
    if (act === "clear-node-filter") { ui.data.filterNode = ""; render(); return; }
    if (act === "data-side") {
      const next = t.getAttribute("data-side") === "validation" ? "validation" : "primary";
      if (next !== ui.data.side) {
        ui.data.side = next;
        try { localStorage.setItem("dualdag.dataSide", next); } catch (err) {}
        ui.data.list = null;
        ui.data.openId = null;
        ui.data.view = null;
        ui.data.colsOn = null;
        ui.data.allCols = null;
        loadDatasets().then(function () { render(); if (ui.data.openId) openView(); }).catch(showErr);
      }
      return;
    }
    if (act === "cols") { ui.data.colsOpen = !ui.data.colsOpen; render(); return; }
    if (act === "open-ds") { await openView(t.getAttribute("data-id")); return; }
    if (act === "page") {
      const delta = parseInt(t.getAttribute("data-delta") || "0", 10) || 0;
      const v = ui.data.view || {};
      const limit = v.limit || ui.data.limit || 100;
      const nrow = v.nrow || 0;
      let next = (ui.data.offset || 0) + delta * limit;
      if (next < 0) next = 0;
      if (delta > 0 && next >= nrow) return;
      ui.data.offset = next;
      await openView();
      return;
    }
    if (act === "col") {
      const name = t.getAttribute("data-col");
      const cols = ui.data.allCols || [];
      if (!ui.data.colsOn) {
        ui.data.colsOn = {};
        cols.forEach(function (c) { ui.data.colsOn[c.name] = true; });
      }
      ui.data.colsOn[name] = !!t.checked;
      if (!cols.some(function (c) { return ui.data.colsOn[c.name] !== false; })) ui.data.colsOn[name] = true;
      await openView();
      return;
    }
    if (act === "cat-toggle") {
      const dom = t.getAttribute("data-domain") || "";
      ui.catOpen[dom] = !ui.catOpen[dom];
      render();
      return;
    }
    if (act === "spec-row") {
      const file = t.getAttribute("data-file");
      const row = t.getAttribute("data-row");
      const ds = t.getAttribute("data-ds") || ui.dataset;
      const variable = t.getAttribute("data-var") || "";
      const param = t.getAttribute("data-param") || "";
      const multi = !!(ui.multiMode || (ev && (ev.ctrlKey || ev.metaKey || ev.shiftKey)));
      if (ds && ds !== ui.dataset && S.datasets && S.datasets[ds]) { ui.dataset = ds; ui.sel = []; ui.didFit = false; ui.userView = false; }
      const keyRecordId = variable ? recordsNodeForKey(ds, ds + "." + variable) : null;
      const picked = keyRecordId ? [keyRecordId] : idsForSpecRow(ds, variable, param);
      if (multi) {
        if (picked.length > 1) {
          const allOn = picked.every(function (id) { return (ui.sel || []).indexOf(id) >= 0; });
          if (allOn) picked.forEach(function (id) { toggleSelection(id); });
          else addSelection(picked);
        } else if (picked.length === 1) toggleSelection(picked[0]);
        render();
        return;
      }
      ui.selRows = [{ file: file, row: row }];
      ui.specTab = /parameter/i.test(file) ? "parameter" : "variable";
      if (ds && ds !== ui.dataset && S.datasets && S.datasets[ds]) {
        ui.didFit = false;
        ui.userView = false;
      }
      const ids = keyRecordId ? [keyRecordId] : nodesFor(ds, param ? "" : variable, param);
      ui.pin = {};
      ids.forEach(function (id) { ui.pin[id] = true; });
      if (ids.length) {
        setSelection(picked.length ? picked : [ids[0]]);
        await pickNode(ids[0], true);
      } else { ui.node = null; ui.detail = null; scrollToSpec(file, row); }
      return;
    }
    if (act === "multi-mode") {
      ui.multiMode = !ui.multiMode;
      ui.selectOpen = false;
      render();
      return;
    }
    if (act === "select-menu") {
      ui.selectOpen = !ui.selectOpen;
      render();
      return;
    }
    if (act === "select-quick") {
      applyQuick(t.getAttribute("data-which") || "clear");
      return;
    }
    if (act === "batch-go") { await startBatch(); return; }
    if (act === "batch-stop") {
      await api("/api/batch/cancel", {});
      await refresh();
      setMsg("Batch will stop after the current variable");
      return;
    }
    if (act === "batch-results") { openModal(batchResultsHtml()); return; }
    if (act === "batch-open") {
      const id = t.getAttribute("data-node") || "";
      closeModal();
      if (!id) return;
      setSelection([id]);
      setTab("code");
      await pickNode(id, false);
      return;
    }
    if (act === "explain") {
      await openCoderWindow(t.getAttribute("data-side") === "validation" ? "validation" : "primary", EXPLAIN_Q);
      return;
    }
    if (act === "dag-fold") {
      ui.dagOpen = ui.dagOpen === false;
      render();
      return;
    }
    if (act === "dag-context-action") {
      const action = t.getAttribute("data-action"), c = ui.dagContext;
      if (!c) return;
      ui.dagContext = null;
      if (action === "dag-review") reviewDagNode(c.node);
      else if (action === "dag-show-all") showAllDag();
      return;
    }
    if (act === "super-close") { ui.superAgent = null; render(); return; }
    if (act === "admin-open") { await openAppAdmin(); return; }
    if (act === "admin-close") { if(ui.appAdminTimer)clearTimeout(ui.appAdminTimer); ui.appAdmin = null; render(); return; }
    if (act === "admin-send") { sendAppAdmin(); return; }
    if (act === "admin-stop") { try { await api("/api/admin/stop", {}); } catch(e) { if(ui.appAdmin)ui.appAdmin.error=e.message||String(e); } return; }
    if (act === "admin-new") { try { await api("/api/admin/new", {}); if(ui.appAdmin){ui.appAdmin.statusMessage="";ui.appAdmin.statusFlags={};ui.appAdmin.error="";} await openAppAdmin(); } catch(e) { if(ui.appAdmin)ui.appAdmin.error=e.message||String(e); render(); } return; }
    if (act === "admin-undo") { try { const u=await api("/api/admin/undo", {}); await refresh(); if(ui.appAdmin){await pollAppAdminState();ui.appAdmin.statusMessage=u.ok?(u.notice||"Undo restored the last snapshot."):(u.message||"Nothing to undo.");ui.appAdmin.statusFlags=u.flags||{};ui.appAdmin.error="";render();} } catch(e) { if(ui.appAdmin)ui.appAdmin.error=e.message||String(e); render(); } return; }
    if (act === "admin-restart") { try { await api("/api/admin/restart",{}); location.reload(); } catch(e) { if(ui.appAdmin){const message=e.message||String(e);if(message.indexOf("run.R changed")>=0){ui.appAdmin.statusMessage=message;ui.appAdmin.statusFlags={relaunch:true};}else ui.appAdmin.error=message+" Restart could not be completed; relaunch with the same project and port.";} render(); } return; }
    if (act === "admin-relaunch-note") { if(ui.appAdmin){ui.appAdmin.statusMessage="Stop the app and relaunch run.R with the same project and port; this window cannot reload the launcher.";ui.appAdmin.statusFlags={};render();} return; }
    if (act === "super-clear") { if (ui.superAgent) { localStorage.removeItem(superKey(ui.superAgent.tab)); ui.superAgent.turns = []; render(); } return; }
    if (act === "super-send") { sendSuperAgent(); return; }
    if (act === "dag-undo") { await undoDagEdit(); return; }
    if (act === "graph-mode") { if (ui.dataset) ui.graphMode[ui.dataset] = t.getAttribute("data-mode") === "parameter" ? "parameter" : "variable"; ui.didFit = false; ui.userView = false; ui.drag = null; render(); return; }
    if (act === "param-extract") {
      if (!ui.dataset || ui.dagActionWait || paramExtractionJob(ui.dataset)) return;
      if (!confirmManualEdits(parameterManualEditCount(curParamGraph(), ui.dataset))) return;
      ui.dagActionWait = { kind: "param-extract", dataset: ui.dataset }; render();
      try { const pg = await api("/api/param-graph/extract", { dataset: ui.dataset }); ui.paramGraphs[ui.dataset] = pg; delete ui.paramErrors[ui.dataset]; setMsg("Parameter dependencies extracted"); }
      catch (e) { ui.paramErrors[ui.dataset] = serverError(e, "Parameter extraction failed"); setMsg(ui.paramErrors[ui.dataset]); }
      finally { ui.dagActionWait = null; await refresh(); }
      return;
    }
    if (act === "layer-generate") {
      if (!ui.dataset || ui.dagActionWait || paramExtractionJob(ui.dataset) || isDatasetExtracting(ui.dataset)) return;
      const layers = asRecords(curGraph() && curGraph().layers);
      const paramcd = ui.layerChoice[ui.dataset] || (layers.length ? String(layers[0].paramcd || "") : "");
      const graph = curGraph();
      const carry = carryOverParameters(graph);
      const group = String(paramcd).toUpperCase() === "CARRY_OVER" || carry.indexOf(String(paramcd).toUpperCase()) >= 0;
      const members = group ? carry : [String(paramcd).toUpperCase()];
      const generated = asArray(graph && graph.generated_parameters);
      const groupGenerated = generated.indexOf("CARRY_OVER") >= 0 || members.some(function (pc) { return generated.indexOf(pc) >= 0; });
      if ((group ? groupGenerated : members.some(function (pc) { return generated.indexOf(pc) >= 0; })) &&
          !confirm(group ? "Regenerate the carry-over group (" + members.join(", ") + ") and clear its results and descendants?" : "Regenerate " + paramcd + " Variable DAG and clear its results?")) return;
      ui.layerChoice[ui.dataset] = paramcd;
      const label = group ? ((groupGenerated ? "Regenerating" : "Generating") + " Carry-over Group (" + members.join(", ") + ")") : ((generated.indexOf(String(paramcd).toUpperCase()) >= 0 ? "Regenerating" : "Generating") + " Variable DAG for " + String(paramcd).toUpperCase());
      ui.dagActionWait = { kind: "layer-generate", dataset: ui.dataset, label: label };
      render();
      try { await api("/api/graph/parameter-extract", { dataset: ui.dataset, paramcd: paramcd }); await refresh(); setMsg(group ? "Carry-over group generated: " + members.join(", ") : "Variable DAG generated for " + paramcd); }
      catch (e) { showErr(e); }
      finally { ui.dagActionWait = null; await refresh(); }
      return;
    }
    if (act === "param-send") { await sendParamDag(); return; }
    if (act === "param-apply") { await applyParamDag(); return; }
    if (act === "dag-send") {
      await sendDag();
      return;
    }
    if (act === "dag-undo") { await undoDagEdit(); return; }
    if (act === "dag-clear") { if (!ui.dataset) return; await api("/api/dag/clear", { dataset: ui.dataset }); await refresh(); setMsg("Conversation cleared"); return; }
    if (act === "dag-apply") { await applyDag(); return; }
    if (act === "dag-discard") {
      discardDag();
      return;
    }
    if (act === "dag-corr-x") {
      if (!ui.dataset) return;
      await api("/api/dag/correction/remove", { dataset: ui.dataset, at: t.getAttribute("data-at") || "" });
      await refresh();
      setMsg("Correction removed");
      return;
    }
    if (act === "expert-clear") {
      const side = t.getAttribute("data-side") === "validation" ? "validation" : "primary";
      if (!ui.dataset) return;
      await api("/api/expert/clear", { dataset: ui.dataset, side: side });
      await refresh();
      if (ui.coderWindowLoad) await ui.coderWindowLoad();
      setMsg("Cleared the " + side + " expert memory");
      return;
    }
    if (ui.inflight) return;
    ui.inflight = true;
    try {
      if (act === "spec-up") await uploadSpec();
      else if (act === "recheck") {
        if (!S || !S.spec) setMsg("Upload a spec first");
        else {
          setMsg("Reading input columns…");
          await refresh();
          setMsg("Reading input columns · " + domainCount() + " domains");
        }
      }
      else if (act === "data-up") await uploadData();
      else if (act === "rm-data") { await api("/api/data/remove", { name: t.getAttribute("data-name") }); await refresh(); setMsg("Removed"); }
      else if (act === "qc-up") await uploadQc();
      else if (act === "qc-view") {
        const ds = t.getAttribute("data-name");
        const r = await api("/api/qc/get?dataset=" + encodeURIComponent(ds));
        openModal("<h3>" + esc(ds) + ' QC document</h3><div class="md">' + mdToHtml(r.markdown || "") + "</div>" + btn("modal-close", "Close"));
      }
      else if (act === "tpl") {
        const name = t.getAttribute("data-name");
        await dl("/api/template?name=" + encodeURIComponent(name), undefined, name);
      }
      else if (act === "extract") await extractGraph();
      else if (act === "export") {
        const fmt = t.getAttribute("data-fmt") || "xpt";
        const ds = exportDataset();
        await dl("/api/export", { dataset: ds, format: fmt }, (ds || "dataset") + "." + fmt);
      }
      else if (act === "rerun") await rerunStored(t.getAttribute("data-side") || ui.runSide || "both");
      else if (act === "generate" || act === "compare" || act === "investigate") await runNode(act);
      else if (act === "reset-confirm") {
        const box = document.getElementById("reset-delete-code");
        const deleteCode = !!(box && box.checked);
        closeModal();
        await api("/api/node/reset", { dataset: ui.dataset, node: ui.node, delete_code: deleteCode });
        await refresh();
        setMsg("Reset " + displayNode(ui.node));
      }
      else if (act === "llm-log") {
        const side = t.getAttribute("data-side") || "primary";
        const r = await api("/api/node/llm-log?dataset=" + encodeURIComponent(ui.dataset) + "&node=" + encodeURIComponent(ui.node) + "&side=" + encodeURIComponent(side));
        const body = [r && r.system, r && r.user, r && r.reply].filter(Boolean).join("\n\n");
        openModal("<h3>" + esc(side) + " last prompt</h3><pre class=\"log\">" + esc(body) + "</pre>" + btn("modal-close", "Close"));
      }
      else if (act === "dataset-formats-save") {
        const formats = {};
        document.querySelectorAll("[data-dataset-format]").forEach(function (el) { formats[el.getAttribute("data-dataset-format")] = el.value; });
        try { await api("/api/dataset/formats", { formats: formats }); await refresh(); setMsg("Dataset formats saved"); }
        catch (e) { showErr(e); }
      }
      else if (act === "cfg-save") await saveConfig();
      else if (act === "cfg-test") await testConfig();
      else if (act === "cfg-cli-check") await loadCliStatus(true);
    } finally {
      // Render while inflight is true disables every gated button. Paint once
      // more after the action settles so the next allowed action is clickable.
      ui.inflight = false;
      if (S) render();
    }
  }
  async function uploadSpec() {
    const fv = ui.files.var;
    if (!fv) { setMsg("Choose a variable.csv"); return; }
    if (S && S.spec && !confirm("Changes you have not downloaded will be lost")) return;
    const body = { variable: { name: fv.name, base64: await fileB64(fv) } };
    if (ui.files.param) body.parameter = { name: ui.files.param.name, base64: await fileB64(ui.files.param) };
    setMsg("Reading input columns…");
    const r = await api("/api/spec/upload", body);
    ui.files.var = null;
    ui.files.param = null;
    await refresh();
    const nIss = asRecords(r && r.issues).length;
    setMsg("Reading input columns · " + domainCount() + " domains" + (nIss ? ", " + nIss + " spec issues" : ""));
  }
  async function uploadData(files, folderMode) {
    const selected = Array.from(files || ui.files.xpt || []);
    const xpts = selected.filter(function (f) { return /\.xpt$/i.test(f.name); });
    if (!xpts.length) { setMsg(folderMode ? "No .xpt files in this folder." : "Choose an xpt file"); return; }
    const names = {};
    const clashes = [];
    xpts.forEach(function (f) { const n = f.name.toLowerCase(); if (names[n]) clashes.push(f.name); names[n] = true; });
    if (clashes.length) { setMsg("Duplicate XPT filenames: " + clashes.join(", ") + ". Nothing uploaded."); return; }
    const failed = [];
    for (let i = 0; i < xpts.length; i++) {
      const f = xpts[i];
      setMsg("Uploading " + (i + 1) + " of " + xpts.length + ": " + f.name);
      try { await api("/api/data/upload", { name: f.name, base64: await fileB64(f) }); }
      catch (e) { failed.push(f.name + ": " + ((e && e.message) || "upload failed")); }
    }
    ui.files.xpt = null;
    await refresh();
    setMsg("Uploaded " + (xpts.length - failed.length) + " of " + xpts.length + "." + (failed.length ? " Failed: " + failed.join("; ") : ""));
  }
  async function uploadQc() {
    const f = ui.files.qc;
    const dsEl = document.getElementById("qc-ds");
    const ds = ((dsEl && dsEl.value) || ui.qcDs || ui.dataset || "").trim();
    if (!f || !ds) { setMsg("Choose a QC document and enter the dataset name"); return; }
    const b64 = await fileB64(f);
    try {
      await api("/api/qc/upload", { dataset: ds, name: f.name, base64: b64, replace: false });
    } catch (e) {
      if (e.status === 409 && e.data && e.data.exists) {
        if (!confirm("This dataset already has a QC document. Replace it?")) return;
        await api("/api/qc/upload", { dataset: ds, name: f.name, base64: b64, replace: true });
      } else throw e;
    }
    ui.files.qc = null;
    await refresh();
    setMsg("QC document saved");
  }
  async function extractGraph() {
    if (!ui.dataset) return;
    if (isDatasetExtracting(ui.dataset)) return;
    if (!confirmManualEdits(graphManualEditCount(ui.dataset))) return;
    setMsg("Reading input columns…");
    await loadCatalog();
    render();
    setMsg("Extracting dependencies…");
    const ds = ui.dataset;
    ui.dagReview = null; ui.dagContext = null;
    delete ui.extractErrors[ds];
    const pending = api("/api/graph/extract", { dataset: ds });
    await refresh();
    try { await pending; }
    catch (e) {
      ui.extractErrors[ds] = (e && e.message) || "The dependency extraction failed.";
      await refresh();
      throw e;
    }
    await refresh();
    delete ui.dagEditNotes[ds];
    saveDagEditNotes();
    setMsg("Graph updated");
  }
  async function sendCoderMessage() {
    const w = ui.coderWindow;
    if (!w || w.sending || (w.data && w.data.running)) return;
    const message = String(w.draft || "").trim();
    if (!message) return;
    if (w.suMode) {
      w.suMessages = asArray(w.suMessages);
      const last = w.suMessages[w.suMessages.length - 1];
      if (!(last && last.role === "user" && String(last.text || "") === message)) w.suMessages.push({ role: "user", text: message });
      w.sending = true; w.draft = ""; w.error = ""; render();
      try {
        const out = await api("/api/node/su", { dataset: w.dataset, node: w.node, side: w.side, message: message, messages: w.suMessages });
        asArray(out.calls).forEach(function (line) {
          const call = String(line).split(":")[0];
          const result = call === "write_program" ? (String(line).indexOf('"ok":true') >= 0 ? "program written" : "refused") : "finished";
          w.suMessages.push({ kind: "tool", text: call + ": " + result });
        });
        if (asArray(out.calls).some(function (line) { return String(line).indexOf("write_program:") === 0 && String(line).indexOf('"ok":true') >= 0; })) await refresh();
        w.suMessages.push({ role: "assistant", text: out.reply || "Done.", usage: out.usage || null });
        if (out.usage) {
          const prior = w.suUsage || { input: 0, output: 0 };
          if (Number.isFinite(Number(out.usage.input))) prior.input += Number(out.usage.input);
          if (Number.isFinite(Number(out.usage.output))) prior.output += Number(out.usage.output);
          w.suUsage = prior;
        }
        if (ui.coderWindowLoad) await ui.coderWindowLoad();
      } catch (err) { w.error = serverError(err, "SU request failed"); w.draft = message; }
      finally { w.sending = false; render(); }
      return;
    }
    w.optimisticMessage = message;
    w.optimisticThreadLength = asArray(w.data && w.data.thread).length;
    w.sending = true; w.draft = ""; w.error = ""; syncCoderWindowPoll(); render();
    try {
      await api("/api/node/agent", { dataset: w.dataset, node: w.node, side: w.side, message: message });
    } catch (err) {
      if (ui.coderWindow === w) {
        w.error = serverError(err, "Chat failed"); w.draft = message; w.optimisticMessage = "";
      }
    } finally {
      if (ui.coderWindow === w) {
        w.sending = false;
        w.manualAttempt = false; w.selectedAttempt = null;
        await refresh();
        if (ui.coderWindowLoad) await ui.coderWindowLoad();
        else render();
        w.optimisticMessage = ""; w.optimisticThreadLength = 0;
        render();
        syncCoderWindowPoll();
      }
    }
  }
  async function sendDag() {
    if (!ui.dataset) { setMsg("Select a dataset first"); return; }
    if (ui.dagAsking) return;
    const el = document.getElementById("dag-text");
    const message = String((el && el.value) || ui.dagDraft || "").trim();
    if (!message) { setMsg("Enter a question"); return; }
    ui.dagDraft = "";
    ui.skipValueRestore = true;
    ui.threadToEnd = "dag-thread";
    ui.dagAsking = true;
    ui.running = true;
    setMsg("Agent is working…");
    render();
    let msg = "DAG agent answered";
    try {
      const r = await api("/api/dag/ask", { dataset: ui.dataset, message: message, paramcd: parameterMode() ? (ui.layerChoice[ui.dataset] || null) : null, selected: ui.sel || [], parameter_graph: parameterMode() });
      ui.dagPending = null; ui.dagHideAt = "";
      if (r && r.model) msg = "DAG agent answered · " + r.model;
    } catch (e) {
      msg = serverError(e, "DAG agent failed");
      ui.dagPending = null;
    } finally {
      ui.dagAsking = false;
      ui.running = false;
    }
    await refresh();
    setMsg(msg);
    ui.threadToEnd = null;
  }
  async function sendParamDag() {
    if (!ui.dataset || ui.paramAsking) return;
    const el = document.getElementById("param-dag-text");
    const message = String((el && el.value) || ui.paramDraft || "").trim();
    if (!message) { setMsg("Enter a question"); return; }
    ui.paramDraft = ""; ui.paramAsking = true; render();
    try {
      const r = await api("/api/param-graph/ask", { dataset: ui.dataset, message: message });
      ui.paramPending = asRecords(r && r.changes);
      ui.paramTurns[ui.dataset] = asRecords(r && r.turns);
      if (!ui.paramTurns[ui.dataset].length) ui.paramTurns[ui.dataset] = [{ role: "user", text: message }, { role: "agent", text: r.answer || "Proposal ready", changes: ui.paramPending }];
      setMsg("Parameter agent answered" + (r.model ? " · " + r.model : ""));
    } catch (e) { setMsg(serverError(e, "Parameter agent failed")); }
    finally { ui.paramAsking = false; render(); }
  }
  async function applyParamDag() {
    if (!ui.dataset || !ui.paramPending || !ui.paramPending.length || ui.paramAsking) return;
    ui.paramAsking = true; render();
    try {
      const r = await api("/api/param-graph/apply", { dataset: ui.dataset, changes: ui.paramPending });
      if (!r || !r.ok) throw new Error((r && r.error) || "Could not apply parameter graph changes");
      ui.paramGraphs[ui.dataset] = r.graph || ui.paramGraphs[ui.dataset]; ui.paramPending = null; setMsg("Parameter graph updated");
    } catch (e) { setMsg(serverError(e, "Apply failed")); }
    finally { ui.paramAsking = false; await refresh(); }
  }
  async function applyDag() {
    const changes = pendingDagChanges();
    if (!ui.dataset || !changes.length || ui.dagAsking) return;
    ui.dagAsking = true;
    setMsg("Applying graph changes…");
    render();
    let edges = [];
    let msg = "Graph updated";
    try {
      const r = await api("/api/dag/apply", { dataset: ui.dataset, changes: changes });
      edges = asRecords(r && r.edges);
      ui.dagPending = null;
      ui.dagHideAt = "";
    } catch (e) {
      msg = serverError(e, "Apply failed");
    } finally {
      ui.dagAsking = false;
    }
    await refresh();
    if (edges.length) {
      armDagFlash(edges);
      render();
    }
    setMsg(msg);
  }
  function discardDag() {
    const turns = asRecords(dagMem().turns);
    for (let i = turns.length - 1; i >= 0; i--) {
      if (turns[i] && turns[i].role === "agent") {
        ui.dagHideAt = turns[i].at || "hidden";
        break;
      }
    }
    ui.dagPending = null;
    render();
    setMsg("Changes discarded");
  }
  let autodagStarted = false;
  function maybeAutodag() {
    if (!MOCK || !AUTODAG || autodagStarted || ui.autodagDone) return;
    autodagStarted = true;
    ui.autodagDone = "running";
    runAutodag().catch(function (e) {
      window.__dualdagErrors.push("autodag: " + ((e && e.message) || e));
      ui.autodagDone = "fail";
      publishBoot();
    });
  }
  async function runAutodag() {
    ui.tab = "spec";
    ui.dataset = "ADVS";
    prefillDag("ADVS.CHG");
    ui.dagDraft = "add edge ADVS.USUBJID -> ADVS.CHG";
    await sendDag();
    if (!pendingDagChanges().length) throw new Error("autodag: no proposed changes");
    const applyBtn = document.getElementById("dag-pending");
    if (!applyBtn) throw new Error("autodag: Apply changes was not shown");
    await applyDag();
    if (!ui.dagFlashed || ui.dagFlashed.indexOf("ADVS.USUBJID->ADVS.CHG") < 0) {
      throw new Error("autodag: changed edge was not highlighted");
    }
    ui.autodagDone = "ok";
    publishBoot();
  }
  async function loadCliStatus(force) {
    const prov = cfgValues().provider;
    if (!CLI_PROVIDERS[prov]) return;
    if (!force && ui.cliStatus && ui.cliStatus.provider === prov && (ui.cliStatus.pending || ui.cliStatus.data)) return;
    if (force) readCfgForm();
    ui.cliStatus = { provider: prov, pending: true, data: null };
    if (force && ui.tab === "settings") render();
    let data;
    try {
      data = await api("/api/llm/cli_status?provider=" + encodeURIComponent(prov));
    } catch (e) {
      data = {
        installed: false, path: "", version: "", logged_in: false, method: "",
        hint: (e && e.message) || "Could not check the CLI."
      };
    }
    if (!ui.cliStatus || ui.cliStatus.provider !== prov) return;
    ui.cliStatus = { provider: prov, pending: false, data: data };
    if (ui.tab === "settings") render();
    if (data.installed && data.logged_in === true && prov === cfgValues().provider &&
        (!S.config_public || S.config_public.provider !== prov)) {
      try {
        ui.cfgDraft = { provider: prov, model: cfgValues().model, base_url: "", key: "", clear: false, cli_use_login: true };
        await saveConfig();
        ui.cfgTest = { pending: false, ok: true, text: "Using " + (prov === "claude_cli" ? "Claude Code" : prov === "codex_cli" ? "Codex" : "Grok") + " login." };
        if (ui.tab === "settings") render();
      } catch (e) { /* explicit Save/Test continues to show errors */ }
    }
  }
  async function autoDetectCliLogin() {
    if (ui.cliAutoDetectDone || !S || !S.config_public || S.config_public.provider_saved || S.config_public.api_key_set) return;
    ui.cliAutoDetectDone = true;
    const providers = ["claude_cli", "codex_cli", "grok_cli"];
    const statuses = await Promise.all(providers.map(function (provider) {
      return api("/api/llm/cli_status?provider=" + encodeURIComponent(provider)).then(function (data) {
        return { provider: provider, data: data };
      }).catch(function () { return { provider: provider, data: { installed: false, logged_in: false } }; });
    }));
    const logged = statuses.filter(function (x) { return x.data.installed && x.data.logged_in === true; });
    ui.loggedCliProviders = logged.map(function (x) { return x.provider; }).concat(providers.filter(function (p) { return !logged.some(function (x) { return x.provider === p; }); }));
    if (logged.length !== 1) { if (ui.tab === "settings") render(); return; }
    const chosen = logged[0];
    ui.cfgDraft = { provider: chosen.provider, base_url: "", model: CLI_PROVIDERS[chosen.provider].models[0] || "", key: "", clear: false, cli_use_login: true };
    ui.cliStatus = { provider: chosen.provider, pending: false, data: chosen.data };
    if (ui.tab === "settings") render();
    try {
      await saveConfig();
      ui.cfgTest = { pending: false, ok: true, text: "Using " + (chosen.provider === "claude_cli" ? "Claude Code" : chosen.provider === "codex_cli" ? "Codex" : "Grok") + " login." };
    } catch (e) { ui.cfgTest = { pending: false, ok: false, text: (e && e.message) || "Could not save CLI settings." }; }
    if (ui.tab === "settings") render();
  }
  function cliModelFromForm() {
    const sel = document.getElementById("cfg-model-cli");
    if (!sel) return "";
    if (sel.value === "__other__") return (document.getElementById("cfg-model-other") || {}).value || "";
    return sel.value || "";
  }
  function readCfgForm() {
    const prov = document.getElementById("cfg-provider");
    if (!prov) return;
    const login = document.getElementById("cfg-cli-login");
    ui.cfgDraft = {
      provider: prov.value,
      base_url: (document.getElementById("cfg-base") || {}).value || "",
      model: CLI_PROVIDERS[prov.value]
        ? cliModelFromForm()
        : ((document.getElementById("cfg-model") || {}).value || ""),
      strong_model: (document.getElementById("cfg-strong-model") || {}).value || "",
      key: (document.getElementById("cfg-key") || {}).value || "",
      clear: !!(document.getElementById("cfg-clear") && document.getElementById("cfg-clear").checked),
      cli_use_login: CLI_PROVIDERS[prov.value] ? true : (login ? !!login.checked : (ui.cfgDraft && ui.cfgDraft.cli_use_login != null
        ? !!ui.cfgDraft.cli_use_login
        : ((S && S.config_public && S.config_public.cli_use_login) !== false))),
    };
  }
  async function saveConfig(preserveTest) {
    if (!preserveTest) ui.cfgTest = null;
    const prov = document.getElementById("cfg-provider");
    if (!prov) { setMsg("Model settings are not open"); return; }
    const cli = !!CLI_PROVIDERS[prov.value];
    const login = document.getElementById("cfg-cli-login");
    const body = {
      provider: prov.value,
      base_url: cli ? "" : document.getElementById("cfg-base").value,
      model: cli ? cliModelFromForm().trim() : document.getElementById("cfg-model").value,
      strong_model: ((document.getElementById("cfg-strong-model") || {}).value || "").trim(),
    };
    if (cli) body.cli_use_login = true;
    else if (login) body.cli_use_login = !!login.checked;
    else if (ui.cfgDraft && ui.cfgDraft.cli_use_login != null) body.cli_use_login = !!ui.cfgDraft.cli_use_login;
    else if (S && S.config_public && S.config_public.cli_use_login != null) body.cli_use_login = !!S.config_public.cli_use_login;
    if (!cli) {
      const key = document.getElementById("cfg-key").value;
      if (key) body.api_key = key;
      if (document.getElementById("cfg-clear") && document.getElementById("cfg-clear").checked) body.clear_api_key = true;
    }
    const r = await api("/api/config", body);
    if (r && r.config && S) S.config_public = r.config;
    ui.cfgDraft = null;
    ui.cfgOther = false;
    await refresh();
    setMsg("Settings saved");
  }
  async function testConfig() {
    const testedProvider = (document.getElementById("cfg-provider") || {}).value || "";
    ui.cfgTest = { pending: true, ok: false, text: "Testing connection…" };
    readCfgForm();
    render();
    try {
      await saveConfig(true);
      const r = await api("/api/config/test", {});
      ui.cfgTest = { pending: false, ok: true, text: (testedProvider === "claude_cli" ? "Using Claude Code login. " : "") + (r.model || "") + " · " + (r.reply || "") };
    } catch (e) {
      ui.cfgTest = { pending: false, ok: false, text: (e && e.message) || "Test failed" };
    }
    if (S) render();
  }

  function onClick(e) {
    const t = e.target && e.target.closest ? e.target.closest("[data-act]") : null;
    if (ui.dagContext && !(e.target && e.target.closest && e.target.closest(".dag-context"))) {
      ui.dagContext = null;
      if (!t) { render(); return; }
    }
    if (!t) {
      if (ui.selectOpen && !(e.target && e.target.closest && e.target.closest(".picks, .chk"))) {
        ui.selectOpen = false;
        render();
      }
      return;
    }
    dispatch(t.getAttribute("data-act"), t, e).catch(showErr);
  }
  function onChange(e) {
    const t = e.target;
    if (!t || !t.id) return;
    if (t.id === "coder-su-switch" && ui.coderWindow) {
      ui.coderWindow.suMode = !!t.checked;
      if (!t.checked) ui.coderWindow.suMessages = [];
      render(); return;
    }
    if (t.id === "coder-context-attempt" && ui.coderWindow) {
      ui.coderWindow.selectedAttempt = Number(t.value);
      ui.coderWindow.manualAttempt = true;
      if (ui.coderWindowLoad) ui.coderWindowLoad().catch(showErr);
      return;
    }
    if (t.id === "graph-param-filter") {
      if (ui.dataset) {
        if (t.value) ui.paramFilter[ui.dataset] = t.value;
        else delete ui.paramFilter[ui.dataset];
      }
      ui.category = null;
      ui.didFit = false;
      ui.userView = false;
      render();
      return;
    }
    if (t.id === "graph-layer-select") {
      if (ui.dataset) ui.layerChoice[ui.dataset] = t.value;
      ui.sel = []; ui.node = null; ui.detail = null; ui.pin = {}; ui.category = null;
      ui.didFit = false; ui.userView = false; render();
      return;
    }
    const map = { "f-var": "var", "f-param": "param", "f-qc": "qc" };
    if (map[t.id] && t.files && t.files[0]) {
      ui.files[map[t.id]] = t.files[0];
      render();
    }
    if (t.id === "f-xpt-folder" && t.files) {
      ui.files.xpt = Array.from(t.files);
      ui.inflight = true;
      uploadData(ui.files.xpt, true).catch(showErr).finally(function () { ui.inflight = false; if (S) render(); });
    } else if (t.id === "f-xpt" && t.files && t.files.length) {
      ui.files.xpt = Array.from(t.files);
      render();
    }
    if (t.id === "diff-node") {
      ui.diff.node = t.value;
      ui.diff.page = 1;
      ui.diff.focus = true;
      loadDiff().then(function () { syncHash(); }).catch(showErr);
    } else if (t.id === "diff-only") {
      ui.diff.onlyDiff = !!t.checked;
      ui.diff.page = 1;
      loadDiff().catch(showErr);
    } else if (t.id === "run-side") {
      ui.runSide = t.value;
      render();
    } else if (t.id === "batch-compare") {
      ui.compareAfter = !!t.checked;
    } else if (t.id === "cfg-provider") {
      ui.cfgTest = null;
      readCfgForm();
      const next = CLI_PROVIDERS[ui.cfgDraft.provider];
      if (next && next.models.indexOf(ui.cfgDraft.model) < 0) {
        ui.cfgDraft.model = next.allowOther ? "" : (next.models[0] || "");
        ui.cfgOther = false;
      } else {
        ui.cfgOther = false;
      }
      ui.cliStatus = null;
      ui.cfgTest = null;
      render();
    } else if (t.id === "cfg-clear" || t.id === "cfg-cli-login") {
      if (t.id === "cfg-cli-login") ui.cfgTest = null;
      readCfgForm();
    } else if (t.id === "cfg-model-cli") {
      ui.cfgTest = null;
      ui.cfgOther = t.value === "__other__";
      readCfgForm();
      render();
    }
  }
  function onInput(e) {
    const t = e.target;
    if (!t) return;
    if (t.id === "start-new-dag-confirm") {
      const button = document.querySelector('[data-act="start-new-dag-confirm-action"]');
      if (button) button.disabled = t.value !== "CONFIRM";
    }
    if (t.id === "coder-window-message" && ui.coderWindow) {
      ui.coderWindow.draft = t.value;
      t.style.height = "auto";
      t.style.height = Math.min(t.scrollHeight, 120) + "px";
      const send = document.querySelector('[data-act="coder-send"]');
      if (send) send.disabled = !!(ui.coderWindow.sending || (ui.coderWindow.data && ui.coderWindow.data.running) || !t.value.trim());
    }
    if (t.id === "spec-filter") {
      ui.specFilter = t.value;
      const el = document.getElementById("spec-scroll");
      if (el) el.innerHTML = specTableHtml();
    } else if (t.id && t.id.indexOf("cat-filter-") === 0) {
      const dom = t.id.slice("cat-filter-".length);
      ui.catFilter[dom] = t.value;
      const body = document.getElementById("cat-body-" + dom);
      const row = ((S && S.data) || []).filter(function (x) { return domainKey(x) === dom; })[0];
      if (body && row) body.innerHTML = catBody(row);
    } else if (t.id === "dag-text") {
      ui.dagDraft = t.value;
    } else if (t.id === "param-dag-text") ui.paramDraft = t.value;
    else if (t.id === "qc-ds") ui.qcDs = t.value;
    else if (t.id === "cfg-model-other") {
      ui.cfgTest = null;
      if (!ui.cfgDraft) readCfgForm();
      if (ui.cfgDraft) ui.cfgDraft.model = t.value;
    }
    else if (t.id === "cfg-base" || t.id === "cfg-model" || t.id === "cfg-key") { ui.cfgTest = null; readCfgForm(); }
    else if (t.id === "data-filter") {
      ui.data.filter = t.value;
      ui.data.offset = 0;
      if (ui.data.filterTimer) clearTimeout(ui.data.filterTimer);
      ui.data.filterTimer = setTimeout(function () { openView().catch(showErr); }, 280);
    } else if (t.id === "diff-filter") {
      ui.diff.filter = t.value;
      ui.diff.page = 1;
      if (ui.diff.filterTimer) clearTimeout(ui.diff.filterTimer);
      ui.diff.filterTimer = setTimeout(function () { loadDiff().catch(showErr); }, 280);
    }
  }
  function onKey(e) {
    if (e.key === "Enter" && !e.shiftKey && e.target && e.target.id === "coder-window-message") {
      e.preventDefault(); sendCoderMessage().catch(showErr); return;
    }
    if (e.key === "Escape" && ui.coderWindow) {
      if (ui.coderWindowTimer) clearInterval(ui.coderWindowTimer);
      ui.coderWindowTimer = null; ui.coderWindow = null; ui.coderWindowLoad = null; render(); return;
    }
    if (e.key === "Escape" && ui.dagContext) {
      ui.dagContext = null; render(); return;
    }
    if (e.key === "Enter" && !e.shiftKey && e.target && e.target.id === "dag-text") {
      e.preventDefault();
      sendDag().catch(showErr);
      return;
    }
    if (e.key === "Enter" && !e.shiftKey && e.target && e.target.id === "param-dag-text") { e.preventDefault(); sendParamDag().catch(showErr); return; }
    if (e.key !== "Enter" && e.key !== " ") return;
    const t = e.target && e.target.closest ? e.target.closest(".node") : null;
    if (!t) return;
    if (t.getAttribute("data-defined") === "false") { e.preventDefault(); return; }
    e.preventDefault();
    const kid = t.getAttribute("data-id");
    if (parameterMode()) {
      ui.graphMode[ui.dataset] = "variable";
      if (curGraph() && curGraph().layered === true) ui.layerChoice[ui.dataset] = kid;
      else ui.paramFilter[ui.dataset] = kid;
      ui.didFit = false; ui.userView = false; ui.sel = []; render(); return;
    }
    prefillDag(kid);
    setSelection([kid]);
    pickNode(kid, false).catch(showErr);
  }
  window.addEventListener("error", function (ev) {
    window.__dualdagErrors = window.__dualdagErrors || [];
    window.__dualdagErrors.push(String((ev && ev.message) || "error"));
    const app = document.getElementById("app");
    if (app && app.dataset.ready !== "1") app.textContent = "Script error: " + (ev.message || "");
    publishBoot();
  });
  window.addEventListener("unhandledrejection", function (ev) {
    const reason = ev && ev.reason;
    const msg = (reason && (reason.message || String(reason))) || "unhandledrejection";
    window.__dualdagErrors = window.__dualdagErrors || [];
    window.__dualdagErrors.push(String(msg));
    publishBoot();
  });

  function boot() {
    if (!MOCK && !token()) {
      document.getElementById("app").innerHTML = '<div class="banner"><b>Missing session token.</b> Open the full link printed by <code>run.R</code> (it ends with <code>#token=…</code>).</div>';
      return;
    }
    refresh().then(async function () {
      let ds = "";
      try { ds = sessionStorage.getItem("dualdag_start_new_dataset") || ""; } catch (ignore) {}
      if (!ds) return;
      try { sessionStorage.removeItem("dualdag_start_new_dataset"); } catch (ignore) {}
      ui.dataset = ds;
      ui.tab = "spec";
      ui.graphMode[ds] = "parameter";
      const pg = await api("/api/param-graph?dataset=" + encodeURIComponent(ds));
      ui.paramGraphs[ds] = pg;
      render();
      await dispatch("param-extract", null, null);
    }).catch(function (e) {
      if (e && e.token) {
        document.getElementById("app").innerHTML = '<div class="banner" id="token-banner"><b>' + tokenBannerText() + "</b></div>";
        return;
      }
      const why = e.status === 403 ? "<br>" + tokenBannerText() : "";
      document.getElementById("app").innerHTML = '<div class="banner" id="token-banner">Failed to load: ' + esc(e.message) + why + "</div>";
    });
  }
  window.addEventListener("hashchange", function () {
    const hp = readHash();
    if (hp.token) {
      let cur = "";
      try { cur = sessionStorage.getItem("dualdag_token") || ""; } catch (err) { cur = ""; }
      if (hp.token !== cur) {
        try { sessionStorage.setItem("dualdag_token", hp.token); } catch (err) { /* keep going */ }
        location.reload();
        return;
      }
    }
    const tab = hashTab(hp);
    if (tab && tab !== ui.tab) setTab(tab);
  });
  document.addEventListener("click", onClick);
  document.addEventListener("input", function (e) {
    if (e.target && e.target.id === "super-agent-message" && ui.superAgent) { ui.superAgent.draft = e.target.value; const b=document.querySelector('[data-act="super-send"]'); if(b)b.disabled=!e.target.value.trim()||ui.superAgent.busy; }
    if (e.target && e.target.id === "app-admin-message" && ui.appAdmin) { ui.appAdmin.draft=e.target.value; const b=document.querySelector('[data-act="admin-send"]'); if(b)b.disabled=!e.target.value.trim()||ui.appAdmin.busy; }
    if (e.target && e.target.id === "admin-provider" && ui.appAdmin) ui.appAdmin.provider=e.target.value;
  });
  document.addEventListener("keydown", function(e){ if(e.target&&e.target.id==="app-admin-message"&&e.key==="Enter"&&!e.shiftKey){e.preventDefault();sendAppAdmin();} });
  document.addEventListener("keydown", function (e) { if (e.target && e.target.id === "super-agent-message" && e.key === "Enter" && !e.shiftKey) { e.preventDefault(); sendSuperAgent(); } });
  document.addEventListener("pointerdown", function(e) { if (ui.superAgent && e.target && e.target.closest && e.target.closest(".super-agent-head") && !e.target.closest("button")) { const box=document.querySelector(".super-agent-overlay").getBoundingClientRect(); ui.superAgent.drag={x:e.clientX-box.left,y:e.clientY-box.top}; e.preventDefault(); } });
  document.addEventListener("pointermove", function(e) { if(!ui.superAgent || !ui.superAgent.drag)return; const el=document.querySelector(".super-agent-overlay"); if(!el)return; ui.superAgent.x=Math.max(0,Math.min(innerWidth-el.offsetWidth,e.clientX-ui.superAgent.drag.x)); ui.superAgent.y=Math.max(0,Math.min(innerHeight-80,e.clientY-ui.superAgent.drag.y)); el.style.left=ui.superAgent.x+"px"; el.style.top=ui.superAgent.y+"px"; el.style.right="auto"; });
  document.addEventListener("pointerup", function(){ if(ui.superAgent)ui.superAgent.drag=null; });
  document.addEventListener("change", onChange);
  document.addEventListener("input", onInput);
  document.addEventListener("keydown", onKey);
  document.addEventListener("scroll", function (e) {
    if (ui.coderWindow && !ui.coderWindow.detailsOpen && e.target && e.target.classList
        && e.target.classList.contains("coder-conversation")) {
      ui.coderWindow.followNewest = e.target.scrollHeight - e.target.clientHeight - e.target.scrollTop < 48;
    }
    if (!ui.dagContext) return;
    const wrap = document.getElementById("dagwrap");
    if (!wrap) return;
    const r = wrap.getBoundingClientRect();
    if (r.left !== ui.dagContext.wrapLeft || r.top !== ui.dagContext.wrapTop) {
      ui.dagContext = null;
      render();
    }
  }, true);
  if (MOCK && /(?:\?|&)scrolltest=1(?:&|$)/.test(location.search)) window.__dualdagMockPoll = refresh;
  const modal = document.getElementById("modal");
  if (modal) modal.addEventListener("click", function (e) { if (e.target.id === "modal") closeModal(); });
  boot();
})();
