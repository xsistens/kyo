import { attach } from "./cdp.mjs";
const base = process.argv[2] || "http://localhost:8080";
const c = await attach();
await c.go(base + "/c/treetable");
const dump = (n=0) => `(() => {
  const a = document.activeElement;
  const t = document.querySelectorAll('.p-treetable-table')[${n}];
  const rows = t ? [...t.querySelectorAll('tbody tr')] : [];
  const name = r => (r.querySelector('td')?.textContent||'').trim().slice(0,14);
  return {
    act: ((a.getAttribute('aria-label')||a.textContent||a.tagName)+'').trim().slice(0,16)+' ti='+a.tabIndex+' '+(a.tagName),
    role: t ? t.getAttribute('role') : null,
    rows: rows.map(name).join('|'),
    ti: rows.map(r => r.tabIndex).join(','),
    exp: rows.map(r => r.getAttribute('aria-expanded') ?? '-').join(','),
    lvl: rows.map(r => r.getAttribute('aria-level')).join(','),
    sel: rows.map(r => r.getAttribute('aria-selected') ?? '-').join(',')
  };
})()`;
const show = async (t, n=0) => console.log(t.padEnd(20), JSON.stringify(await c.ev(dump(n))));

await show("initial");
await c.ev(`document.querySelectorAll('.p-treetable-table')[0].querySelector('tbody tr').focus()`);
await show("focus row 0");
for (const k of ["ArrowDown","ArrowDown","ArrowRight","ArrowDown","ArrowLeft","ArrowLeft","End","Home","Enter"]) {
  await c.key(k); await show(k);
}
console.log("--- Tab out and back ---");
await c.key("ArrowDown"); await show("ArrowDown");
await c.key("Tab"); await show("Tab");
await c.key("Tab", ["shift"]); await show("Shift+Tab");
c.close();
