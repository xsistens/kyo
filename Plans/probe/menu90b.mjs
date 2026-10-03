import { attach } from "./cdp.mjs";
const c = await attach();
await c.go((process.argv[2]||"http://localhost:8080") + "/c/menu");
const dump = (n) => `(() => {
  const m = document.querySelectorAll('.p-menu')[${n}];
  const rows = [...m.querySelectorAll('li')];
  return {roles: rows.map(r => (r.getAttribute('role')||'-')[0]).join(''),
          text: rows.map(r => r.textContent.trim().slice(0,8)).join('|'),
          hi: rows.map((r,i) => r.classList.contains('p-focus') ? i : -1).filter(i=>i>=0).join(',')};
})()`;
const show = async (t, n) => console.log(t.padEnd(12), JSON.stringify(await c.ev(dump(n))));
for (const n of [1]) {
  console.log("=== grouped menu, index " + n + " ===");
  await c.ev(`document.querySelectorAll('.p-menu')[${n}].querySelector('ul').focus()`);
  await show("start", n);
  await c.key("ArrowDown"); await show("Down(1st)", n);
  await c.ev(`document.querySelectorAll('.p-menu')[${n}].querySelector('ul').blur(); document.querySelectorAll('.p-menu')[${n}].querySelector('ul').focus()`);
  await c.key("Home"); await show("Home", n);
  await c.key("End"); await show("End", n);
  await c.key("ArrowDown"); await show("Down(wrap)", n);
  await c.key("ArrowUp"); await show("Up", n);
}
console.log("=== flat menu: does the highlight survive an activation? ===");
await c.ev(`document.querySelectorAll('.p-menu')[0].querySelector('ul').focus()`);
await c.key("ArrowDown"); await show("Down", 0);
await c.key("ArrowDown"); await show("Down", 0);
await c.key("Enter");     await show("Enter", 0);
await c.key("ArrowDown"); await show("Down after", 0);
console.log("=== ids and announcement ===");
console.log(await c.ev(`(() => {
  const ms = [...document.querySelectorAll('.p-menu')];
  return ms.map(m => { const l = m.querySelector('ul'); return {ad: l.getAttribute('aria-activedescendant')||'-', rowIds: [...m.querySelectorAll('li')].map(r=>r.id||'.').join(',')}; });
})()`));
c.close();
