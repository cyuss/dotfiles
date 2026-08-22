// Rend un HTML local en PDF via Chrome DevTools Protocol.
// Le CDP est necessaire (et pas --print-to-pdf) pour avoir un pied de page
// avec numerotation : la CLI ne sait pas passer footerTemplate.
import { spawn } from "node:child_process";
import { writeFileSync } from "node:fs";
import { resolve } from "node:path";

const [htmlPath, pdfPath, docTitle] = process.argv.slice(2);
const NOFOOT = process.argv.includes("--no-footer");
const CHROME = "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome";
const PORT = 9333 + (process.pid % 500);

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const chrome = spawn(CHROME, [
  "--headless=new", `--remote-debugging-port=${PORT}`,
  "--no-first-run", "--no-default-browser-check", "--disable-gpu",
  "--hide-scrollbars", "--run-all-compositor-stages-before-draw",
  "--virtual-time-budget=8000", "--allow-file-access-from-files",
  "about:blank",
], { stdio: "ignore" });

async function version() {
  for (let i = 0; i < 100; i++) {
    try {
      const r = await fetch(`http://127.0.0.1:${PORT}/json/version`);
      if (r.ok) return await r.json();
    } catch {}
    await sleep(120);
  }
  throw new Error("Chrome n'a pas ouvert le port de debug");
}

const { webSocketDebuggerUrl } = await version();
const ws = new WebSocket(webSocketDebuggerUrl);
await new Promise((res, rej) => { ws.onopen = res; ws.onerror = rej; });

let id = 0;
const pending = new Map();
const events = [];
const waiters = [];
ws.onmessage = (m) => {
  const msg = JSON.parse(m.data);
  if (msg.id !== undefined) {
    const p = pending.get(msg.id); pending.delete(msg.id);
    msg.error ? p.rej(new Error(JSON.stringify(msg.error))) : p.res(msg.result);
  } else {
    events.push(msg);
    for (const w of waiters.splice(0)) w(msg);
  }
};
const send = (method, params = {}, sessionId) =>
  new Promise((res, rej) => {
    const i = ++id;
    pending.set(i, { res, rej });
    ws.send(JSON.stringify({ id: i, method, params, ...(sessionId ? { sessionId } : {}) }));
  });
const waitEvent = (name, timeout = 30000) =>
  new Promise((res, rej) => {
    const hit = events.find((e) => e.method === name);
    if (hit) return res(hit);
    const t = setTimeout(() => rej(new Error("timeout " + name)), timeout);
    const on = (msg) => {
      if (msg.method === name) { clearTimeout(t); res(msg); }
      else waiters.push(on);
    };
    waiters.push(on);
  });

const { targetId } = await send("Target.createTarget", { url: "about:blank" });
const { sessionId } = await send("Target.attachToTarget", { targetId, flatten: true });
await send("Page.enable", {}, sessionId);
await send("Page.navigate", { url: "file://" + resolve(htmlPath) }, sessionId);
await waitEvent("Page.loadEventFired");
await sleep(1400); // laisse les polices locales se resoudre

const foot = `
<style>
  #f { font-family: "Helvetica Neue", Helvetica, sans-serif; font-size: 6.6pt;
       color: #8b93a3; width: 100%; padding: 0 14mm; display: flex;
       justify-content: space-between; align-items: center; }
  #f b { color: #14171c; font-weight: 600; }
  .pageNumber, .totalPages { font-family: monospace; }
</style>
<div id="f"><span>${docTitle}</span>
<span><span class="pageNumber"></span> / <span class="totalPages"></span></span></div>`;

const { data } = await send("Page.printToPDF", {
  printBackground: true,
  preferCSSPageSize: true,
  displayHeaderFooter: !NOFOOT,
  headerTemplate: "<span></span>",
  footerTemplate: foot,
  marginTop: 0.63, marginBottom: 0.63, marginLeft: 0.55, marginRight: 0.55,
}, sessionId);

writeFileSync(pdfPath, Buffer.from(data, "base64"));
ws.close();
chrome.kill();
console.log(`${pdfPath} ecrit`);
process.exit(0);
