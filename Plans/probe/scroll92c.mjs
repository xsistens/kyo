import { attach } from "./cdp.mjs";
const c = await attach();
await c.go((process.argv[2]||"http://localhost:8080") + "/c/listbox");
// The counterfactual: with scrollIntoView disabled the page behaves as it did before mounted92.
await c.ev(`window.__si = Element.prototype.scrollIntoView; Element.prototype.scrollIntoView = function(){}; 'off'`);
const lst = `(() => {
  const lb = [...document.querySelectorAll('.p-listbox')].find(l => l.querySelectorAll('li').length > 50);
  const box = lb.closest('div[style*="max-height"]') || lb.parentElement;
  const hi = lb.querySelector('li.p-focus');
  const br = box.getBoundingClientRect(), hr = hi ? hi.getBoundingClientRect() : null;
  return {hi: hi ? hi.textContent.trim() : '', scrollTop: Math.round(box.scrollTop),
          visible: hr ? (hr.top >= br.top - 1 && hr.bottom <= br.bottom + 1) : null};
})()`;
const show = async t => console.log(t.padEnd(16), JSON.stringify(await c.ev(lst)));
await c.ev(`(() => { const lb = [...document.querySelectorAll('.p-listbox')].find(l => l.querySelectorAll('li').length > 50); lb.scrollIntoView({block:'center'}); lb.querySelector('.p-listbox-list').focus(); return 1; })()`);
await c.sleep(500); await show("focus (no scroll)");
for (let i = 0; i < 12; i++) await c.key("ArrowDown");
await show("Down x12");
await c.key("End"); await show("End");
console.log("--- restore and repeat ---");
await c.ev(`Element.prototype.scrollIntoView = window.__si; 'on'`);
await c.key("Home"); await show("Home");
await c.key("End");  await show("End");
c.close();
