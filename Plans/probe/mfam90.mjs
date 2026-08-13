import { attach } from "./cdp.mjs";
const base = process.argv[2] || "http://localhost:8080";
const c = await attach();
const state = (rootSel, listSel) => `(() => {
  const a = document.activeElement;
  const root = document.querySelector(${JSON.stringify(rootSel)});
  if (!root) return {missing: true};
  const list = root.querySelector(${JSON.stringify(listSel)});
  const items = [...root.querySelectorAll('li')];
  return {act: ((a.textContent||a.tagName)+'').trim().slice(0,10)+'/'+a.tagName,
          ad: list ? (list.getAttribute('aria-activedescendant')||'-') : 'no-list',
          hi: items.filter(i => i.classList.contains('p-focus')).map(i => i.textContent.trim().slice(0,9)).join(' '),
          panels: root.querySelectorAll('[class*=submenu]').length};
})()`;

console.log("############ TieredMenu");
await c.go(base + "/c/tieredmenu");
let show = async t => console.log(t.padEnd(14), JSON.stringify(await c.ev(state('.p-tieredmenu', '.p-tieredmenu-root-list'))));
await c.ev(`document.querySelector('.p-tieredmenu-root-list').focus()`); await c.sleep(400);
await show("focus");
for (const k of ["ArrowDown","ArrowDown","ArrowRight","ArrowDown","Escape"]) { await c.key(k); await show(k); }
await c.key("ArrowRight"); await show("ArrowRight");
await c.key("Tab"); await show("Tab");

console.log("############ MegaMenu");
await c.go(base + "/c/megamenu");
show = async t => console.log(t.padEnd(14), JSON.stringify(await c.ev(state('.p-megamenu', '.p-megamenu-root-list'))));
await c.ev(`document.querySelector('.p-megamenu-root-list').focus()`); await c.sleep(400);
await show("focus");
for (const k of ["ArrowRight","ArrowDown","ArrowDown","ArrowRight"]) { await c.key(k); await show(k); }
await c.key("Tab"); await show("Tab");

console.log("############ ContextMenu");
await c.go(base + "/c/contextmenu");
show = async t => console.log(t.padEnd(14), JSON.stringify(await c.ev(state('.p-contextmenu', '.p-contextmenu-root-list'))));
const box = await c.at('.p-contextmenu-target, [class*=contextmenu-target], .demoExample');
if (box) {
  for (const type of ["mousePressed","mouseReleased"])
    await c.send("Input.dispatchMouseEvent", {type, x: box.x, y: box.y, button: "right", clickCount: 1});
  await c.sleep(800);
}
await show("right click");
for (const k of ["ArrowDown","ArrowDown","ArrowRight","Escape"]) { await c.key(k); await show(k); }
await c.key("Tab"); await show("Tab");
c.close();
