import { attach } from "./cdp.mjs";
const c = await attach();
await c.go((process.argv[2]||"http://localhost:8080") + "/c/treetable");
const st = `(() => {
  const t = document.querySelectorAll('.p-treetable-table')[1];
  const rows = [...t.querySelectorAll('tbody tr')];
  const name = r => (r.querySelector('td')?.textContent||'').trim().replace(/[0-9]+[kmg]b.*/i,'').slice(0,11);
  return rows.map(r => name(r) + (r.getAttribute('aria-expanded')==='true'?'[v]':(r.getAttribute('aria-expanded')==='false'?'[>]':'')) + (r.getAttribute('aria-selected')==='true'?'*':'')).join(' ')
       + ' | act=' + ((document.activeElement.textContent||'').trim().replace(/[0-9]+[kmg]b.*/i,'').slice(0,11));
})()`;
const show = async t => console.log(t.padEnd(14), await c.ev(st));
await c.ev(`document.querySelectorAll('.p-treetable-table')[1].querySelectorAll('tbody tr')[0].focus()`);
await show("start");
for (let i = 0; i < 6; i++) { await c.key(" "); await show("Space " + i); }
console.log("--- move then toggle repeatedly ---");
for (const k of ["ArrowDown"," ","ArrowUp"," ","ArrowDown"," "]) { await c.key(k); await show(k === " " ? "Space" : k); }
c.close();
