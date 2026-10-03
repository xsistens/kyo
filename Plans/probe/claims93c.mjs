import { attach } from "./cdp.mjs";
const base = process.argv[2] || "http://localhost:8080";
const c = await attach();
console.log("### Password anatomy");
await c.go(base + "/c/password");
console.log(await c.ev(`(() => {
  const p = document.querySelector('.p-password');
  return {html: p.outerHTML.slice(0, 400)};
})()`));

console.log("### ScrollPanel anatomy: which element scrolls, and can Tab reach it?");
await c.go(base + "/c/scrollpanel");
console.log(await c.ev(`(() => {
  const sp = document.querySelector('.p-scrollpanel');
  const all = [sp, ...sp.querySelectorAll('*')].filter(e => e.scrollHeight > e.clientHeight + 2);
  return all.slice(0,3).map(e => ({cls: e.className.slice(0,40), ti: e.getAttribute('tabindex'), sh: e.scrollHeight, ch: e.clientHeight}));
})()`));
console.log(await c.ev(`(() => {
  const sc = [...document.querySelectorAll('.p-scrollpanel *')].find(e => e.scrollHeight > e.clientHeight + 2);
  sc.focus(); const got = document.activeElement === sc;
  return {focusable: got, activeTag: document.activeElement.tagName};
})()`));
c.close();
