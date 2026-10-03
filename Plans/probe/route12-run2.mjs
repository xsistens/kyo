// An example the reader changed still compiles and runs: a fourth tag is typed into the
// editor and the result frame has to show it.
import { attach } from "./cdp.mjs";
const base = process.argv[2] ?? "http://localhost:5179";
const c = await attach();
await c.send("Emulation.setDeviceMetricsOverride", {width: 1600, height: 1000, deviceScaleFactor: 1, mobile: false});
await c.go(`${base}/c/tag`, 8000);

const frameText = () => c.ev(`(() => {
  const f = document.querySelector("iframe");
  if (!f) return null;
  try { return (f.contentDocument?.body?.innerText || "").trim().replace(/\\n/g, " ").slice(0, 160); } catch (e) { return "cross-origin"; }
})()`);
const hint = () => c.ev(`(document.querySelector(".demoCodeHint")?.textContent || "").trim()`);

const lineBox = await c.ev(`(() => {
  const cm = document.querySelector(".cm-content");
  const line = Array.from(cm.querySelectorAll(".cm-line")).find(l => l.textContent.includes("Primary"));
  if (!line) return null;
  line.scrollIntoView({block: "center"});
  const r = line.getBoundingClientRect();
  return {x: Math.round(r.x + r.width - 4), y: Math.round(r.y + r.height / 2), text: line.textContent};
})()`);

for (const type of ["mousePressed", "mouseReleased"])
  await c.send("Input.dispatchMouseEvent", {type, x: lineBox.x, y: lineBox.y, button: "left", clickCount: 1});
await c.sleep(500);
await c.key("End");
await c.send("Input.insertText", {text: ` uic.Tag("Edited"),`});
await c.sleep(500);

const edited = await c.ev(`(document.querySelector(".cm-content")?.innerText || "").slice(0, 140).split(String.fromCharCode(10)).join(" | ")`);
const before = await frameText();

const box = await c.ev(`(() => {
  const b = Array.from(document.querySelectorAll("button")).find(x => (x.textContent||"").trim() === "Run");
  if (!b) return null; b.scrollIntoView({block:"center"});
  const r = b.getBoundingClientRect(); return {x: Math.round(r.x+r.width/2), y: Math.round(r.y+r.height/2)};
})()`);
for (const type of ["mousePressed","mouseReleased"])
  await c.send("Input.dispatchMouseEvent", {type, x: box.x, y: box.y, button: "left", clickCount: 1});
await c.sleep(10000);

console.log(JSON.stringify({line: lineBox?.text, edited, before, hint: await hint(), after: await frameText()}, null, 2));
c.close();
