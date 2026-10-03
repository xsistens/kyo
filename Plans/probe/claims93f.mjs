import { attach } from "./cdp.mjs";
const c = await attach();
await c.go((process.argv[2]||"http://localhost:8080") + "/c/chip");
const gone = `document.body.textContent.includes('Chip removed')`;
await c.ev(`(() => { const b = [...document.querySelectorAll('.p-chip-remove-icon')].pop(); b.scrollIntoView({block:'center'}); b.focus(); return b.tagName; })()`).then(r=>console.log("focused", r));
console.log("removed before:", await c.ev(gone));
await c.key("Backspace"); await c.sleep(500);
console.log("after Backspace:", await c.ev(gone));
c.close();
