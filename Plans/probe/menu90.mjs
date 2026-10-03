import { attach } from "./cdp.mjs";
const base = process.argv[2] || "http://localhost:8080";
const c = await attach();
await c.go(base + "/c/menu");
const dump = (n=0) => `(() => {
  const a = document.activeElement;
  const m = document.querySelectorAll('.p-menu')[${n}];
  const rows = m ? [...m.querySelectorAll('li')] : [];
  const list = m ? m.querySelector('ul') : null;
  const last = [...document.querySelectorAll('span,p,div')].filter(e => e.children.length===0 && e.textContent.startsWith('Last action:')).map(e=>e.textContent)[0]||'';
  return {act: ((a.getAttribute('aria-label')||a.textContent||a.tagName)+'').trim().slice(0,14)+'/'+a.tagName+' ti='+a.tabIndex,
          ad: list ? (list.getAttribute('aria-activedescendant')||'-') : null,
          lrole: list ? list.getAttribute('role') : null,
          roles: rows.map(r => (r.getAttribute('role')||'-')[0]).join(''),
          focusRow: rows.map((r,i) => r.classList.contains('p-focus') ? i : -1).filter(i => i>=0).join(','),
          ids: rows.map(r => r.id ? 'id' : '.').join(''),
          last};
})()`;
const show = async (t, n=0) => console.log(t.padEnd(16), JSON.stringify(await c.ev(dump(n))));
console.log("=== the flat menu in the page ===");
await show("initial");
await c.ev(`document.querySelectorAll('.p-menu')[0].querySelector('ul').focus()`);
await show("focus list");
for (const k of ["ArrowDown","ArrowDown","ArrowUp","Home","End","ArrowDown","Enter"]) { await c.key(k); await show(k); }
console.log("=== groups and separators ===");
await c.ev(`document.querySelectorAll('.p-menu')[1].querySelector('ul').focus()`);
await show("focus list", 1);
for (const k of ["ArrowDown","ArrowDown","ArrowDown"]) { await c.key(k); await show(k, 1); }
console.log("=== Tab out ===");
await c.key("Tab"); await show("Tab", 1);
c.close();
