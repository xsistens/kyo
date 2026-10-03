import { attach } from "./cdp.mjs";
const c = await attach();
await c.go((process.argv[2]||"http://localhost:8080") + "/c/menu");
const dump = (n) => `(() => {
  const m = document.querySelectorAll('.p-menu')[${n}];
  const rows = [...m.querySelectorAll('li')];
  return {hi: rows.map((r,i) => r.classList.contains('p-focus') ? r.textContent.trim().slice(0,8) : null).filter(Boolean).join(','),
          ad: m.querySelector('ul').getAttribute('aria-activedescendant')||'-',
          act: document.activeElement.tagName};
})()`;
const show = async (t, n) => console.log(t.padEnd(14), JSON.stringify(await c.ev(dump(n))));
console.log("=== focus seeds the highlight (waiting for the round trip) ===");
await c.ev(`document.querySelectorAll('.p-menu')[0].querySelector('ul').focus()`);
await c.sleep(600); await show("after focus", 0);
await c.key("ArrowDown"); await show("ArrowDown", 0);
console.log("=== blur drops it ===");
await c.ev(`document.querySelectorAll('.p-menu')[0].querySelector('ul').blur()`);
await c.sleep(600); await show("after blur", 0);
console.log("=== grouped: focus seeds the first ENABLED row ===");
await c.ev(`document.querySelectorAll('.p-menu')[1].querySelector('ul').focus()`);
await c.sleep(600); await show("after focus", 1);
console.log("=== a click on a row: does it seed the highlight? ===");
await c.click(".p-menu:nth-of-type(1) .p-menu-item:last-child a, .p-menu .p-menu-item:last-child a");
await c.sleep(400); await show("after click", 0);
c.close();
