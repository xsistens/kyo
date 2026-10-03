import { attach } from "./cdp.mjs";
const base = process.argv[2] || "http://localhost:8080";
const c = await attach();
await c.go(base + "/c/treetable");
const dump = (n=0) => `(() => {
  const a = document.activeElement;
  const t = document.querySelectorAll('.p-treetable-table')[${n}];
  const rows = t ? [...t.querySelectorAll('tbody tr')] : [];
  const name = r => (r.querySelector('td')?.textContent||'').trim().replace(/[0-9]+[kmg]b.*/i,'').slice(0,12);
  return {
    act: ((a.textContent||a.tagName)+'').trim().replace(/[0-9]+[kmg]b.*/i,'').slice(0,12)+'/'+a.tagName+' ti='+a.tabIndex,
    rows: rows.map(name).join('|'),
    ti: rows.map(r => r.tabIndex).join(','),
    exp: rows.map(r => r.getAttribute('aria-expanded') ?? '-').join(','),
    sel: rows.map(r => r.getAttribute('aria-selected') ?? '-').join(',')
  };
})()`;
const show = async (t, n=0) => console.log(t.padEnd(18), JSON.stringify(await c.ev(dump(n))));

console.log("=== table 1: expand and collapse by key ===");
await c.ev(`document.querySelectorAll('.p-treetable-table')[0].querySelector('tbody tr').focus()`);
await show("focus row 0");
for (const k of ["ArrowDown","ArrowRight","ArrowRight","ArrowLeft","ArrowLeft","ArrowLeft"]) { await c.key(k); await show(k); }

console.log("=== table 2: selection ===");
await c.ev(`document.querySelectorAll('.p-treetable-table')[1].querySelectorAll('tbody tr')[0].focus()`);
await show("focus row 0", 1);
for (const k of ["ArrowDown"," ","ArrowDown","Enter"]) { await c.key(k); await show(k === " " ? "Space" : k, 1); }

console.log("=== table 3: the sort headers ===");
console.log(await c.ev(`(() => {
  const t = document.querySelectorAll('.p-treetable-table')[2];
  return [...t.querySelectorAll('thead th')].map(h => (h.textContent||'').trim().slice(0,10)+':ti='+h.tabIndex+':'+(h.getAttribute('aria-sort')||'-'));
})()`));
await c.ev(`document.querySelectorAll('.p-treetable-table')[2].querySelector('thead th').focus()`);
await show("focus header", 2);
await c.key("Enter"); await show("Enter", 2);
console.log(await c.ev(`(() => { const t = document.querySelectorAll('.p-treetable-table')[2]; return [...t.querySelectorAll('thead th')].map(h => (h.getAttribute('aria-sort')||'-')); })()`));
c.close();
