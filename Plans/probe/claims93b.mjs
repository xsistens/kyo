import { attach } from "./cdp.mjs";
const base = process.argv[2] || "http://localhost:8080";
const c = await attach();
console.log("### Password: the reveal is a control");
await c.go(base + "/c/password");
console.log(await c.ev(`(() => { const t = document.querySelector('.p-password-toggle-mask, .p-password [role=button]');
  return t ? {role: t.getAttribute('role'), ti: t.tabIndex, tag: t.tagName} : 'none'; })()`));
await c.ev(`(() => { const t = document.querySelector('.p-password-toggle-mask, .p-password [role=button]'); t.scrollIntoView({block:'center'}); t.focus(); return 1; })()`);
const typ = `document.querySelector('.p-password input').type`;
const t0 = await c.ev(typ); await c.key("Enter"); const t1 = await c.ev(typ); await c.key(" "); const t2 = await c.ev(typ);
console.log("password input type:", t0, "-enter->", t1, "-space->", t2);

console.log("### Chip: the remove button takes Backspace");
await c.go(base + "/c/chip");
const chips = `document.querySelectorAll('.p-chip').length`;
console.log("chips", await c.ev(chips), "remove buttons", await c.ev(`document.querySelectorAll('.p-chip-remove-icon').length`));
await c.ev(`(() => { const b = document.querySelector('.p-chip-remove-icon'); b.scrollIntoView({block:'center'}); b.focus(); return b.tagName + ':' + b.tabIndex; })()`).then(r => console.log("remove:", r));
const n0 = await c.ev(`[...document.querySelectorAll('span,p,div')].map(e=>e.textContent).find(t=>t&&t.startsWith('Removed'))`);
await c.key("Backspace"); await c.sleep(400);
const n1 = await c.ev(`[...document.querySelectorAll('span,p,div')].map(e=>e.textContent).find(t=>t&&t.startsWith('Removed'))`);
console.log("removed marker:", n0, "->", n1);

console.log("### Panel and Fieldset: the header is a real button");
await c.go(base + "/c/panel");
console.log(await c.ev(`(() => { const b = document.querySelector('.p-panel-header button, .p-panel-toggle-button');
  return b ? {tag: b.tagName, ti: b.tabIndex, exp: b.getAttribute('aria-expanded')} : 'none'; })()`));
await c.ev(`(() => { const b = document.querySelector('.p-panel-header button, .p-panel-toggle-button'); b.scrollIntoView({block:'center'}); b.focus(); return 1; })()`);
const ex = `document.querySelector('.p-panel-header button, .p-panel-toggle-button').getAttribute('aria-expanded')`;
const e0 = await c.ev(ex); await c.key("Enter"); const e1 = await c.ev(ex); await c.key(" "); const e2 = await c.ev(ex);
console.log("panel expanded:", e0, "-enter->", e1, "-space->", e2);

console.log("### Breadcrumb: crumbs are anchors");
await c.go(base + "/c/breadcrumb");
console.log(await c.ev(`[...document.querySelectorAll('.p-breadcrumb li')].slice(0,4).map(li => { const a = li.querySelector('a'); return a ? 'a:ti'+a.tabIndex : 'text'; })`));

console.log("### ScrollPanel: the browser gives a scroller its own tab stop");
await c.go(base + "/c/scrollpanel");
console.log(await c.ev(`(() => { const p = document.querySelector('.p-scrollpanel-content, .p-scrollpanel');
  const before = Math.round(p.scrollTop); p.focus(); return {tag: p.tagName, ti: p.tabIndex, before, focused: document.activeElement === p}; })()`));
await c.key("ArrowDown"); await c.key("PageDown");
console.log("after keys:", await c.ev(`Math.round((document.querySelector('.p-scrollpanel-content, .p-scrollpanel')).scrollTop)`));
c.close();
