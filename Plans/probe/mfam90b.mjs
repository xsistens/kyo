import { attach } from "./cdp.mjs";
const base = process.argv[2] || "http://localhost:8080";
const c = await attach();
const st = (rootSel, listSel) => `(() => {
  const a = document.activeElement;
  const root = document.querySelector(${JSON.stringify(rootSel)});
  if (!root) return {gone: true};
  const list = root.querySelector(${JSON.stringify(listSel)});
  const items = [...root.querySelectorAll('li')];
  return {act: ((a.textContent||a.tagName)+'').trim().slice(0,9)+'/'+a.tagName,
          ad: list ? (list.getAttribute('aria-activedescendant')||'-') : 'no-list',
          hi: items.filter(i => i.classList.contains('p-focus')).map(i => i.textContent.trim().slice(0,8)).join(' '),
          panels: root.querySelectorAll('[class*=submenu]').length};
})()`;

console.log("###### Menu (inline + popup)");
await c.go(base + "/c/menu");
let show = async t => console.log(t.padEnd(13), JSON.stringify(await c.ev(st('.p-menu', '.p-menu-list'))));
await c.ev(`document.querySelector('.p-menu-list').focus()`); await c.sleep(500);
await show("focus"); await c.key("ArrowDown"); await show("ArrowDown");

console.log("###### Menubar: Tab closes every level");
await c.go(base + "/c/menubar");
show = async t => console.log(t.padEnd(13), JSON.stringify(await c.ev(st('.p-menubar', '.p-menubar-root-list'))));
await c.ev(`document.querySelector('.p-menubar-root-list').focus()`); await c.sleep(400);
for (const k of ["ArrowRight","ArrowDown","ArrowDown","ArrowDown","ArrowRight"]) { await c.key(k); await show(k); }
await c.key("Tab"); await show("Tab");

console.log("###### TieredMenu");
await c.go(base + "/c/tieredmenu");
show = async t => console.log(t.padEnd(13), JSON.stringify(await c.ev(st('.p-tieredmenu', '.p-tieredmenu-root-list'))));
await c.ev(`document.querySelector('.p-tieredmenu-root-list').focus()`); await c.sleep(400);
for (const k of ["ArrowDown","ArrowDown","ArrowRight"]) { await c.key(k); await show(k); }
await c.key("Tab"); await show("Tab");

console.log("###### MegaMenu");
await c.go(base + "/c/megamenu");
show = async t => console.log(t.padEnd(13), JSON.stringify(await c.ev(st('.p-megamenu', '.p-megamenu-root-list'))));
await c.ev(`document.querySelector('.p-megamenu-root-list').focus()`); await c.sleep(400);
for (const k of ["ArrowRight","ArrowDown","ArrowDown"]) { await c.key(k); await show(k); }
await c.key("Tab"); await show("Tab");

console.log("###### ContextMenu");
await c.go(base + "/c/contextmenu");
show = async t => console.log(t.padEnd(13), JSON.stringify(await c.ev(st('.p-contextmenu', '.p-contextmenu-root-list'))));
const box = await c.at('.demoExample');
for (const type of ["mousePressed","mouseReleased"])
  await c.send("Input.dispatchMouseEvent", {type, x: box.x, y: box.y, button: "right", clickCount: 1});
await c.sleep(800); await show("right click");
await c.key("ArrowDown"); await show("ArrowDown");
await c.key("Tab"); await show("Tab");
c.close();
