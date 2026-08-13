import { attach } from "./cdp.mjs";
const c = await attach();
await c.go((process.argv[2]||"http://localhost:8080") + "/c/select");
console.log(await c.ev(`[...document.querySelectorAll('.p-select')].length + ' selects'`));
// open each select in turn is slow; instead click the first few and report panel scrollability
const r = [];
for (let i = 0; i < 6; i++) {
  await c.ev(`(() => { const s = document.querySelectorAll('.p-select')[${i}]; if (s) s.scrollIntoView({block:'center'}); return 1; })()`);
  const box = await c.at(`.p-select:nth-of-type(${i+1})`);
  await c.click(`.p-select:nth-of-type(${i+1})`);
  const info = await c.ev(`(() => {
    const p = document.querySelector('.p-select-overlay, .p-select-panel');
    if (!p) return null;
    const box = p.querySelector('.p-select-list-container') || p.querySelector('.p-select-list');
    return {n: p.querySelectorAll('li').length, scroll: box.scrollHeight > box.clientHeight + 2, h: Math.round(box.clientHeight), sh: Math.round(box.scrollHeight)};
  })()`);
  r.push(i + ':' + JSON.stringify(info));
  await c.key("Escape");
}
console.log(r.join('\n'));
c.close();
