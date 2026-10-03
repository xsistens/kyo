import { attach } from "./cdp.mjs";
const c = await attach();
const base = process.argv[2]||"http://localhost:8080";
await c.go(base + "/c/password");
console.log(await c.ev(`(() => {
  const ps = [...document.querySelectorAll('.p-password')];
  const withToggle = ps.find(p => p.querySelector('[role=button], button, .p-password-toggle-mask'));
  if (!withToggle) return {none: true, n: ps.length, first: ps[0].outerHTML.slice(0,200)};
  const t = withToggle.querySelector('[role=button], button, .p-password-toggle-mask');
  return {n: ps.length, toggleTag: t.tagName, role: t.getAttribute('role'), ti: t.tabIndex, cls: t.className.slice(0,40),
          inputType: withToggle.querySelector('input').type};
})()`));
await c.ev(`(() => { const p = [...document.querySelectorAll('.p-password')].find(p => p.querySelector('[role=button], button, .p-password-toggle-mask'));
  const t = p.querySelector('[role=button], button, .p-password-toggle-mask'); t.scrollIntoView({block:'center'}); t.focus(); window.__p = p; return document.activeElement === t; })()`).then(r => console.log("focused:", r));
const typ = `window.__p.querySelector('input').type`;
console.log("type:", await c.ev(typ));
await c.key("Enter"); await c.sleep(300); console.log("after Enter:", await c.ev(typ));
await c.key(" "); await c.sleep(300); console.log("after Space:", await c.ev(typ));
console.log("### VirtualScroller keys");
await c.go(base + "/c/virtualscroller");
const sc = `[...document.querySelectorAll('.p-virtualscroller, .p-virtualscroller *')].find(e => e.scrollHeight > e.clientHeight + 2)`;
await c.ev(`(() => { const s = ${sc}; s.scrollIntoView({block:'center'}); s.focus(); return 1; })()`);
console.log("focused:", await c.ev(`document.activeElement === (${sc})`), "top:", await c.ev(`Math.round((${sc}).scrollTop)`));
await c.key("PageDown"); console.log("after PageDown:", await c.ev(`Math.round((${sc}).scrollTop)`));
c.close();
