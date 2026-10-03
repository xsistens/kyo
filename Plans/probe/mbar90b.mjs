import { attach } from "./cdp.mjs";
const c = await attach();
await c.go((process.argv[2]||"http://localhost:8080") + "/c/menubar");
const dump = (n=0) => `(() => {
  const a = document.activeElement;
  const root = document.querySelectorAll('.p-menubar')[${n}];
  const rootList = root.querySelector('.p-menubar-root-list');
  const items = [...root.querySelectorAll('li')];
  const last = [...document.querySelectorAll('span,p,div')].filter(e => e.children.length===0 && e.textContent.startsWith('Last action:')).map(e=>e.textContent)[0]||'';
  return {act: ((a.textContent||a.tagName)+'').trim().slice(0,10)+'/'+a.tagName,
          ad: rootList.getAttribute('aria-activedescendant')||'-',
          hi: items.filter(i => i.classList.contains('p-focus')).map(i => i.textContent.trim().slice(0,9)).join(' '),
          panels: root.querySelectorAll('.p-menubar-submenu').length,
          last: last.replace('Last action: ','')};
})()`;
const show = async (t, n=0) => console.log(t.padEnd(15), JSON.stringify(await c.ev(dump(n))));

console.log("=== into a nested submenu and back out ===");
await c.ev(`document.querySelectorAll('.p-menubar-root-list')[0].focus()`); await c.sleep(400);
for (const k of ["ArrowRight","ArrowDown","ArrowDown","ArrowDown","ArrowRight","ArrowDown","Escape","Escape","Escape"]) { await c.key(k); await show(k); }
console.log("=== Enter on a leaf ===");
for (const k of ["ArrowDown","ArrowDown","Enter"]) { await c.key(k); await show(k); }
console.log("=== Tab while a submenu is open ===");
for (const k of ["ArrowDown","ArrowDown"]) { await c.key(k); await show(k); }
await c.key("Tab"); await show("Tab");
console.log("=== the third bar carries an id ===");
await c.ev(`document.querySelectorAll('.p-menubar-root-list')[2].focus()`); await c.sleep(400);
await c.key("ArrowRight"); await show("ArrowRight", 2);
c.close();
