import { attach } from "./cdp.mjs";
const c = await attach();
await c.go((process.argv[2]||"http://localhost:8080") + "/c/splitbutton");
const st = `(() => {
  const a = document.activeElement;
  const root = document.querySelector('.p-splitbutton');
  const list = document.querySelector('.p-menu-list');
  const rows = list ? [...list.querySelectorAll('li')] : [];
  return {act: ((a.getAttribute('aria-label')||a.textContent||a.tagName)+'').trim().slice(0,12)+'/'+a.tagName,
          panel: !!list,
          ad: list ? (list.getAttribute('aria-activedescendant')||'-') : '-',
          hi: rows.filter(r => r.classList.contains('p-focus')).map(r => r.textContent.trim().slice(0,8)).join(',')};
})()`;
const show = async t => console.log(t.padEnd(14), JSON.stringify(await c.ev(st)));
await c.click(".p-splitbutton-dropdown");
await c.sleep(500); await show("click chevron");
await c.key("ArrowDown"); await show("ArrowDown");
await c.key("ArrowDown"); await show("ArrowDown");
await c.key("Tab");       await show("Tab");
c.close();
