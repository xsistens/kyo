import { attach } from "./cdp.mjs";
const base = process.argv[2] || "http://localhost:8080";
const c = await attach();
await c.go(base + "/c/treetable");
const dump = (n=0) => `(() => {
  const a = document.activeElement;
  const t = document.querySelectorAll('.p-treetable-table')[${n}];
  const rows = [...t.querySelectorAll('tbody tr')];
  const name = r => (r.querySelector('td')?.textContent||'').trim().replace(/[0-9]+[kmg]b.*/i,'').slice(0,11);
  return {act: ((a.textContent||a.tagName)+'').trim().replace(/[0-9]+[kmg]b.*/i,'').slice(0,11)+'/'+a.tagName,
          rows: rows.map(name).join('|'),
          ti: rows.map(r => r.tabIndex).join(','),
          multi: t.getAttribute('aria-multiselectable') ?? '-'};
})()`;
const show = async (t, n=0) => console.log(t.padEnd(20), JSON.stringify(await c.ev(dump(n))));

console.log("=== the tab stop follows the cursor ===");
await c.ev(`document.querySelectorAll('.p-treetable-table')[0].querySelector('tbody tr').focus()`);
await show("focus row 0");
await c.key("ArrowDown"); await show("ArrowDown");
await c.key("ArrowDown"); await show("ArrowDown");
await c.key("Tab");       await show("Tab out");
await c.key("Tab", ["shift"]); await show("Shift+Tab back");
console.log("=== and a click seeds it ===");
await c.click(".p-treetable-table tbody tr:nth-child(4)");
await show("click row 3");
await c.key("ArrowDown"); await show("ArrowDown");
console.log("=== the chosen row is where the second table picks up ===");
await show("table 2 (editor.app chosen)", 1);
c.close();
