// The SPA shows the excerpt first: the editor holds the whole example with the section's
// irrelevant lines folded away, and the toggle unfolds them and puts the preamble on top.
import { attach } from "./cdp.mjs";
const base = process.argv[2] ?? "http://localhost:5179";
const c = await attach();
await c.send("Emulation.setDeviceMetricsOverride", {width: 1600, height: 1000, deviceScaleFactor: 1, mobile: false});
await c.go(`${base}/c/datatable`, 8000);

const state = (n) => c.ev(`(() => {
  const cm = Array.from(document.querySelectorAll(".cm-content"))[${n}];
  const t = cm ? cm.innerText : "";
  return {
    chars: t.length,
    folds: cm ? cm.querySelectorAll(".cm-foldPlaceholder").length : -1,
    preambles: document.querySelectorAll(".demoPreamble").length,
    head: t.slice(0,40).split(String.fromCharCode(10)).join(" | ")
  };
})()`);

const foldedEditor = await c.ev(`(() => {
  const cs = Array.from(document.querySelectorAll(".cm-content"));
  return cs.findIndex(e => e.querySelectorAll(".cm-foldPlaceholder").length > 0);
})()`);

const clickNth = async (n) => {
  const box = await c.ev(`(() => {
    const bs = Array.from(document.querySelectorAll("button")).filter(x => (x.textContent||"").trim().startsWith("Show the"));
    const b = bs[${n}]; if (!b) return null; b.scrollIntoView({block:"center"});
    const r = b.getBoundingClientRect(); return {x: Math.round(r.x+r.width/2), y: Math.round(r.y+r.height/2)};
  })()`);
  if (!box) return false;
  for (const type of ["mousePressed","mouseReleased"])
    await c.send("Input.dispatchMouseEvent", {type, x: box.x, y: box.y, button: "left", clickCount: 1});
  await c.sleep(1500); return true;
};

const out = {foldedEditor};
out.before = await state(foldedEditor);
out.clicked = await clickNth(foldedEditor);
out.after = await state(foldedEditor);
out.clickedBack = await clickNth(foldedEditor);
out.again = await state(foldedEditor);
console.log(JSON.stringify(out, null, 2));
c.close();
