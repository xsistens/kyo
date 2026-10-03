import { attach } from "./cdp.mjs";
const c = await attach();
await c.go((process.argv[2]||"http://localhost:8080") + "/c/select");
const box = await c.ev(`(() => {
  const t = [...document.querySelectorAll('.p-select')].find(t => (t.textContent||'').trim() === '1994');
  if (!t) return null;
  t.scrollIntoView({block:'center'});
  const b = t.getBoundingClientRect();
  return {x: Math.round(b.x + b.width/2), y: Math.round(b.y + b.height/2)};
})()`);
console.log("trigger", box);
for (const type of ["mousePressed","mouseReleased"])
  await c.send("Input.dispatchMouseEvent", {type, x: box.x, y: box.y, button: "left", clickCount: 1});
await c.sleep(700);
const st = `(() => {
  const p = document.querySelector('.p-select-overlay');
  if (!p) return {open:false};
  const bx = p.querySelector('.p-select-list-container') || p.querySelector('.p-select-list');
  const hi = p.querySelector('li.p-focus');
  const br = bx.getBoundingClientRect(), hr = hi ? hi.getBoundingClientRect() : null;
  return {n: p.querySelectorAll('li').length, hi: hi ? hi.textContent.trim() : '',
          scrollTop: Math.round(bx.scrollTop), clientH: Math.round(bx.clientHeight), scrollH: Math.round(bx.scrollHeight),
          visible: hr ? (hr.top >= br.top - 1 && hr.bottom <= br.bottom + 1) : null};
})()`;
const show = async t => console.log(t.padEnd(14), JSON.stringify(await c.ev(st)));
await show("opened");
for (let i = 0; i < 10; i++) await c.key("ArrowDown");
await show("Down x10");
await c.key("End");  await show("End");
await c.key("Home"); await show("Home");
await c.key("ArrowUp"); await show("Up (wrap?)");
c.close();
