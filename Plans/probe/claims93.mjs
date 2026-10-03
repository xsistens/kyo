import { attach } from "./cdp.mjs";
const base = process.argv[2] || "http://localhost:8080";
const c = await attach();
const focusFirst = sel => c.ev(`(() => { const e = document.querySelector(${JSON.stringify(sel)}); if(!e) return 'missing'; e.scrollIntoView({block:'center'}); e.focus(); return e.tagName + ':' + (e.type||''); })()`);

console.log("### Tag: a clickable tag is a control");
await c.go(base + "/c/tag");
console.log(await c.ev(`(() => {
  const tags = [...document.querySelectorAll('.p-tag')];
  const click = tags.find(t => t.classList.contains('p-uic-clickable'));
  return {clickable: !!click, role: click && click.getAttribute('role'), ti: click && click.tabIndex,
          plainTi: tags.find(t => !t.classList.contains('p-uic-clickable'))?.tabIndex};
})()`));
await c.ev(`document.querySelector('.p-tag.p-uic-clickable').focus()`);
const before = await c.ev(`[...document.querySelectorAll('span')].map(s=>s.textContent).find(t => t && t.startsWith('Clicked'))`);
await c.key("Enter"); await c.sleep(300);
await c.key(" "); await c.sleep(300);
const after = await c.ev(`[...document.querySelectorAll('span')].map(s=>s.textContent).find(t => t && t.startsWith('Clicked'))`);
console.log("tag counter:", before, "->", after);

console.log("### CheckBox: Space and Enter both toggle");
await c.go(base + "/c/checkbox");
console.log(await focusFirst(".p-checkbox-input"));
const st = `document.querySelector('.p-checkbox-input').checked`;
const c0 = await c.ev(st); await c.key(" "); const c1 = await c.ev(st); await c.key("Enter"); const c2 = await c.ev(st);
console.log("checkbox:", c0, "-space->", c1, "-enter->", c2);

console.log("### RadioButton: arrows move and choose inside a name group");
await c.go(base + "/c/radiobutton");
console.log(await focusFirst(".p-radiobutton-input"));
const rs = `[...document.querySelectorAll('.p-radiobutton-input')].slice(0,4).map(r => (r.checked?'x':'.')).join('')`;
console.log("before", await c.ev(rs));
await c.key("ArrowDown"); console.log("ArrowDown", await c.ev(rs), await c.ev(`document.activeElement.className`));

console.log("### Slider: arrows and Home/End move a native range");
await c.go(base + "/c/slider");
console.log(await focusFirst("input[type=range]"));
const sv = `document.querySelector('input[type=range]').value`;
const s0 = await c.ev(sv); await c.key("ArrowRight"); const s1 = await c.ev(sv); await c.key("End"); const s2 = await c.ev(sv);
console.log("slider:", s0, "->", s1, "-end->", s2);

console.log("### Paginator: every button is a tab stop");
await c.go(base + "/c/paginator");
console.log(await c.ev(`(() => { const p = document.querySelector('.p-paginator'); const bs = [...p.querySelectorAll('button')];
  return {n: bs.length, tabbable: bs.filter(b => b.tabIndex >= 0 && !b.disabled).length}; })()`));

console.log("### InputNumber: the spin buttons are out of the tab order");
await c.go(base + "/c/inputnumber");
console.log(await c.ev(`[...document.querySelectorAll('.p-inputnumber-button')].slice(0,3).map(b => b.tabIndex)`));
c.close();
