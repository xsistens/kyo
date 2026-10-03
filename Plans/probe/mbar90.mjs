import { attach } from "./cdp.mjs";
const base = process.argv[2] || "http://localhost:8080";
const c = await attach();
await c.go(base + "/c/menubar");
const dump = `(() => {
  const a = document.activeElement;
  const root = document.querySelector('.p-menubar');
  const rootList = root.querySelector('.p-menubar-root-list');
  const items = [...root.querySelectorAll('li')];
  return {act: ((a.getAttribute('aria-label')||a.textContent||a.tagName)+'').trim().slice(0,12)+'/'+a.tagName+' ti='+a.tabIndex,
          role: rootList.getAttribute('role'),
          ad: rootList.getAttribute('aria-activedescendant')||'-',
          rootTi: rootList.tabIndex,
          hi: items.filter(i => i.classList.contains('p-focus')).map(i => i.textContent.trim().slice(0,10)).join(','),
          open: items.filter(i => i.getAttribute('aria-expanded')==='true').map(i => i.textContent.trim().slice(0,8)).join(','),
          submenus: root.querySelectorAll('.p-menubar-submenu').length};
})()`;
const show = async t => console.log(t.padEnd(16), JSON.stringify(await c.ev(dump)));
await show("initial");
await c.ev(`document.querySelector('.p-menubar-root-list').focus()`); await c.sleep(500);
await show("focus root");
for (const k of ["ArrowRight","ArrowRight","ArrowDown","ArrowDown","ArrowRight","ArrowLeft","Escape","ArrowLeft","Home","End","Enter"]) {
  await c.key(k); await show(k);
}
console.log("--- Tab out ---");
await c.key("Tab"); await show("Tab");
c.close();
