// CDP helper for the keyboard walks. Lives here rather than in a scratchpad because /tmp is
// cleaned between sessions and this file is the walk itself.
export async function attach(port = 9448) {
  const tabs = await (await fetch(`http://localhost:${port}/json`)).json();
  const page = tabs.find(t => t.type === "page");
  const ws = new WebSocket(page.webSocketDebuggerUrl);
  await new Promise(r => ws.onopen = r);
  let id = 0; const pending = new Map();
  ws.onmessage = e => { const m = JSON.parse(e.data); if (pending.has(m.id)) { pending.get(m.id)(m); pending.delete(m.id); } };
  const send = (m, p = {}) => new Promise(r => { const i = ++id; pending.set(i, r); ws.send(JSON.stringify({id: i, method: m, params: p})); });
  const ev = async e => (await send("Runtime.evaluate", {expression: e, returnByValue: true, awaitPromise: true})).result?.result?.value;
  const sleep = ms => new Promise(r => setTimeout(r, ms));
  const CODES = {ArrowDown: 40, ArrowUp: 38, ArrowLeft: 37, ArrowRight: 39, Enter: 13, Escape: 27, " ": 32,
                 Tab: 9, Home: 36, End: 35, PageDown: 34, PageUp: 33, Backspace: 8};
  const MODS = {alt: 1, ctrl: 2, meta: 4, shift: 8};
  async function key(k, mods = []) {
    const printable = k.length === 1 && k !== " ";
    const vk = CODES[k] ?? (printable ? k.toUpperCase().charCodeAt(0) : 0);
    const code = k === " " ? "Space" : (printable ? "Key" + k.toUpperCase() : k);
    const m = mods.reduce((n, x) => n | MODS[x], 0);
    const base = {key: k, code, modifiers: m, windowsVirtualKeyCode: vk, nativeVirtualKeyCode: vk};
    // Enter carries a char event or Chrome never synthesises the native click on a <button>:
    // without it only the framework's own activation emulation runs, which hides a double one.
    const text = k === " " ? " " : (k === "Enter" ? "\r" : (printable ? k : null));
    if (text !== null) {
      await send("Input.dispatchKeyEvent", {...base, type: "keyDown", text, unmodifiedText: text});
    } else {
      await send("Input.dispatchKeyEvent", {...base, type: "rawKeyDown"});
    }
    await send("Input.dispatchKeyEvent", {...base, type: "keyUp"});
    await sleep(400);
  }
  const at = async sel => ev(`(() => { const e = document.querySelector(${JSON.stringify(sel)}); if (!e) return null; e.scrollIntoView({block:'center'}); const b = e.getBoundingClientRect(); return {x: Math.round(b.x + b.width/2), y: Math.round(b.y + b.height/2)}; })()`);
  const click = async sel => {
    const box = await at(sel); if (!box) return null;
    for (const type of ["mousePressed", "mouseReleased"])
      await send("Input.dispatchMouseEvent", {type, x: box.x, y: box.y, button: "left", clickCount: 1});
    await sleep(600); return box;
  };
  // Without focus emulation the page sees no key at all when the window lacks OS focus.
  await send("Emulation.setFocusEmulationEnabled", {enabled: true});
  await send("Network.enable");
  await send("Network.setCacheDisabled", {cacheDisabled: true});
  const go = async (url, wait = 3500) => { await send("Page.bringToFront"); await send("Page.navigate", {url}); await sleep(wait); };
  return {ws, send, ev, sleep, key, click, at, go, close: () => ws.close()};
}
