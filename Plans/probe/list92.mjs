import { attach } from "./cdp.mjs";
const base = process.argv[2] || "http://localhost:8080";
const c = await attach();
await c.go(base + "/c/listbox");
const st = (n=0) => `(() => {
  const lb = document.querySelectorAll('.p-listbox')[${n}];
  if (!lb) return {gone:true};
  const list = lb.querySelector('.p-listbox-list');
  const box = lb.querySelector('.p-listbox-list-container') || list;
  const rows = [...lb.querySelectorAll('li')];
  const hi = rows.find(r => r.classList.contains('p-focus'));
  const br = box.getBoundingClientRect();
  const hr = hi ? hi.getBoundingClientRect() : null;
  return {ad: (list.getAttribute('aria-activedescendant')||'-'),
          hi: hi ? hi.textContent.trim().slice(0,12) : '',
          rows: rows.length,
          scrollTop: Math.round(box.scrollTop),
          scrollH: Math.round(box.scrollHeight), clientH: Math.round(box.clientHeight),
          visible: hr ? (hr.top >= br.top - 1 && hr.bottom <= br.bottom + 1) : null};
})()`;
const show = async (t, n=0) => console.log(t.padEnd(14), JSON.stringify(await c.ev(st(n))));
console.log("=== which listbox scrolls? ===");
console.log(await c.ev(`[...document.querySelectorAll('.p-listbox')].map((lb,i) => {
  const box = lb.querySelector('.p-listbox-list-container') || lb.querySelector('.p-listbox-list');
  return i + ':rows=' + lb.querySelectorAll('li').length + ':scroll=' + (box.scrollHeight > box.clientHeight + 2);
})`));
c.close();
