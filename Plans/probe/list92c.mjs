import { attach } from "./cdp.mjs";
const c = await attach();
await c.go((process.argv[2]||"http://localhost:8080") + "/c/listbox");
console.log(await c.ev(`[...document.querySelectorAll('.p-listbox')].map((lb,i) => {
  const l = lb.querySelector('.p-listbox-list');
  return i + ' rows=' + lb.querySelectorAll('li').length + ' listTi=' + (l?l.tabIndex:'-') + ' role=' + (l?l.getAttribute('role'):'-') + ' first=' + (lb.querySelector('li')?.textContent.trim().slice(0,10));
})`));
c.close();
