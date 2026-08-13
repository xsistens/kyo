import { attach } from "./cdp.mjs";
const base = process.argv[2] || "http://localhost:8080";
const c = await attach();

console.log("###### Select: a list longer than the panel");
await c.go(base + "/c/select");
// find the select whose panel has many options: the new example binds 1994
const idx = await c.ev(`(() => {
  const ts = [...document.querySelectorAll('.p-select')];
  return ts.findIndex(t => (t.textContent||'').trim() === '1994');
})()`);
console.log("select index", idx);
const st = `(() => {
  const p = document.querySelector('.p-select-overlay');
  if (!p) return {open:false};
  const box = p.querySelector('.p-select-list-container') || p.querySelector('.p-select-list');
  const hi = p.querySelector('li.p-focus');
  const br = box.getBoundingClientRect();
  const hr = hi ? hi.getBoundingClientRect() : null;
  return {open:true, n: p.querySelectorAll('li').length, hi: hi ? hi.textContent.trim() : '',
          scrollTop: Math.round(box.scrollTop), scrollH: Math.round(box.scrollHeight), clientH: Math.round(box.clientHeight),
          visible: hr ? (hr.top >= br.top - 1 && hr.bottom <= br.bottom + 1) : null};
})()`;
const show = async t => console.log(t.padEnd(14), JSON.stringify(await c.ev(st)));
await c.ev(`[...document.querySelectorAll('.p-select')][${idx}].scrollIntoView({block:'center'}); 1`);
await c.sleep(300);
await c.click(`.p-select:nth-of-type(${idx+1})`);
await c.sleep(300);
await show("opened");
for (let i = 0; i < 10; i++) await c.key("ArrowDown");
await show("Down x10");
await c.key("End"); await show("End");
await c.key("Home"); await show("Home");
await c.key("Escape");

console.log("###### Listbox: a list longer than its box");
await c.go(base + "/c/listbox");
const lst = `(() => {
  const lb = [...document.querySelectorAll('.p-listbox')].find(l => l.querySelectorAll('li').length > 50);
  if (!lb) return {missing:true};
  const box = lb.closest('div[style*="max-height"]') || lb.parentElement;
  const hi = lb.querySelector('li.p-focus');
  const br = box.getBoundingClientRect();
  const hr = hi ? hi.getBoundingClientRect() : null;
  return {rows: lb.querySelectorAll('li').length, hi: hi ? hi.textContent.trim() : '',
          scrollTop: Math.round(box.scrollTop), clientH: Math.round(box.clientHeight),
          visible: hr ? (hr.top >= br.top - 1 && hr.bottom <= br.bottom + 1) : null};
})()`;
const show2 = async t => console.log(t.padEnd(14), JSON.stringify(await c.ev(lst)));
await c.ev(`(() => { const lb = [...document.querySelectorAll('.p-listbox')].find(l => l.querySelectorAll('li').length > 50); lb.scrollIntoView({block:'center'}); lb.querySelector('.p-listbox-list').focus(); return 1; })()`);
await c.sleep(500);
await show2("focus");
for (let i = 0; i < 12; i++) await c.key("ArrowDown");
await show2("Down x12");
await c.key("End");  await show2("End");
await c.key("Home"); await show2("Home");
c.close();
