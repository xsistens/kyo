// The routing half of the demo plan, walked in a real browser over one transport.
//   bun route12.mjs http://localhost:8080     (server-push)
//   bun route12.mjs http://localhost:5179     (SPA)
//
// Five questions, in order: does a deep link land where it says; does a nav click move the URL
// (and does the SPA do it without a reload); do back and forward walk the pages just visited;
// does an example link in a settings table jump to that section; does the code bar hand over
// the complete source and back.
import { attach } from "./cdp.mjs";

const base = process.argv[2] ?? "http://localhost:8080";
const spa = base.includes("5179");
const c = await attach();
// The table of contents is hidden below 1200px, and the walk has to see it.
await c.send("Emulation.setDeviceMetricsOverride", { width: 1600, height: 1000, deviceScaleFactor: 1, mobile: false });
const out = {};

const where = () => c.ev(`({
  path: location.pathname,
  hash: location.hash,
  title: (document.querySelector(".demoPageHead")?.textContent || "").trim().slice(0, 30),
  nav: (document.querySelector(".p-listbox-option-selected")?.textContent || "").trim(),
  marked: window.__routeProbe === 1,
  toc: (document.querySelector(".demoTocActive")?.textContent || "").trim()
})`);

const seen = (sel) => c.ev(`(() => {
  const e = document.querySelector(${JSON.stringify(sel)});
  if (!e) return null;
  const b = e.getBoundingClientRect();
  return { top: Math.round(b.top), inView: b.top > -40 && b.top < innerHeight };
})()`);

// 1. a deep link lands on the page AND at the section
await c.go(`${base}/c/datatable#lazily-loaded-rows`, 5000);
out.deepLink = { ...(await where()), section: await seen("#lazily-loaded-rows") };

// 2. a nav click moves the URL. The marker says whether the document survived it: the SPA
// patches in place, the server-push transport navigates and loses it.
await c.ev(`window.__routeProbe = 1`);
await c.click(`a[href="/c/tag"]`);
await c.sleep(1200);
out.navClick = await where();

await c.ev(`window.__routeProbe = 1`);
await c.click(`a[href="/c/chip"]`);
await c.sleep(1200);
out.navClick2 = await where();

// 3. back and forward walk the pages just visited
await c.ev(`history.back()`);
await c.sleep(1800);
out.back = await where();
await c.ev(`history.back()`);
await c.sleep(1800);
out.backAgain = await where();
await c.ev(`history.forward()`);
await c.sleep(1800);
out.forward = await where();

// 4. an example link in a settings table jumps to the section it names
await c.go(`${base}/c/datatable`, 5000);
const target = await c.ev(`document.querySelector("table a[href^='#']")?.getAttribute("href")`);
await c.click(`table a[href^='#']`);
await c.sleep(1200);
out.tableLink = { target, ...(await where()), section: await seen(target) };

// 5. the code bar hands over the complete source and takes it back
const codeOf = () => c.ev(`(() => {
  const pre = document.querySelector("pre");
  const t = pre ? pre.textContent : "";
  return { chars: t.length, imports: t.includes("import kyo"), fixture: t.includes("val products"), head: t.slice(0, 60).replace(/\\n/g, " | ") };
})()`);
out.excerpt = await codeOf();
await c.click(`button`);
await c.sleep(1000);
out.full = await codeOf();
const label = await c.ev(`(document.querySelector("button")?.textContent || "").trim()`);
await c.click(`button`);
await c.sleep(1000);
out.backToExcerpt = { label, ...(await codeOf()) };

console.log(JSON.stringify({ base, spa, ...out }, null, 2));
c.close();
