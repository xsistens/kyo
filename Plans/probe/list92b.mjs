import { attach } from "./cdp.mjs";
const c = await attach();
await c.go((process.argv[2]||"http://localhost:8080") + "/c/listbox");
const st = `(() => {
  const lb = document.querySelectorAll('.p-listbox')[3];
  const rows = [...lb.querySelectorAll('li')];
  const hi = rows.find(r => r.classList.contains('p-focus'));
  const r = hi ? hi.getBoundingClientRect() : null;
  return {rows: rows.length, hi: hi ? hi.textContent.trim().slice(0,10) : '',
          top: r ? Math.round(r.top) : null, vh: window.innerHeight,
          inView: r ? (r.top >= 0 && r.bottom <= window.innerHeight) : null,
          pageY: Math.round(window.scrollY)};
})()`;
const show = async t => console.log(t.padEnd(12), JSON.stringify(await c.ev(st)));
await c.ev(`document.querySelectorAll('.p-listbox')[3].scrollIntoView({block:'start'}); window.scrollBy(0, -20); 1`);
await c.sleep(300);
await c.ev(`document.querySelectorAll('.p-listbox')[3].querySelector('.p-listbox-list').focus()`);
await c.sleep(400);
await show("focus");
for (let i = 0; i < 6; i++) { await c.key("ArrowDown"); }
await show("Down x6");
for (let i = 0; i < 8; i++) { await c.key("ArrowDown"); }
await show("Down x14");
await c.key("End");
await show("End");
c.close();
