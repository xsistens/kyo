// Build-time extraction of the PrimeOne design system (all inputs MIT):
//   @primeuix/themes  — token presets (aura/material/lara/nora, light+dark)
//   @primeuix/styles  — per-component CSS with dt('token.path') references
//   @primeuix/styled  — the resolution engine (Theme, dt, evaluateDtExpressions)
//
// Run from gen/: `bun extract.mjs` (deps live in gen/npm, installed by fetch.sh).
// Emits into gen/work/ (gitignored):
//   tokens/<preset>.json  — { light: [[name, value]...], dark: [[...]] }
//   css/base.css          — shared/base component style (var(--p-*) consumers)
//   css/<component>.css   — per-component style, dt() resolved to var(--p-*)
//
// Token names are taken verbatim from the engine's own output (never re-derived);
// the dark bucket is whatever the engine emits under the darkModeSelector.

import { mkdirSync, writeFileSync } from "node:fs";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const genDir = dirname(fileURLToPath(import.meta.url));
const req = (p) => import(join(genDir, "npm", "node_modules", p, "dist/index.mjs"));

const { Theme, dt, evaluateDtExpressions } = await req("@primeuix/styled");

const PRESETS = ["aura", "material", "lara", "nora"];
const DARK = '[data-scheme="dark"]';

// The components kyo-ui-components implements (phase 3 set + base). Extend as
// new components are ported; every name must exist in @primeuix/styles/dist.
const COMPONENTS = [
  "accordion", "autocomplete", "avatar", "badge", "breadcrumb", "button",
  "card", "carousel", "cascadeselect", "checkbox", "chip", "colorpicker",
  "confirmdialog",
  "contextmenu", "datatable", "dataview", "datepicker", "dialog", "divider",
  "drawer", "fieldset", "fileupload", "floatlabel", "galleria", "iconfield",
  "iftalabel", "inplace", "inputgroup", "inputnumber", "inputotp",
  "inputtext", "knob", "listbox", "megamenu", "menu", "menubar", "message",
  "metergroup", "multiselect", "orderlist", "organizationchart",
  "overlaybadge", "paginator", "panel", "password", "picklist", "popover",
  "progressbar", "progressspinner", "radiobutton", "rating", "scrollpanel",
  "select", "selectbutton", "skeleton", "slider", "speeddial", "splitbutton",
  "stepper", "tabs", "tag", "terminal", "textarea", "tieredmenu", "timeline",
  "toast", "togglebutton", "toggleswitch", "toolbar", "tooltip", "tree",
  "treeselect", "treetable", "virtualscroller",
];

const work = join(genDir, "work");
mkdirSync(join(work, "tokens"), { recursive: true });
mkdirSync(join(work, "css"), { recursive: true });

// Parse `selector{--name:value;...}` blocks into [light|dark] -> [[name, value]].
// CSS custom-property values never contain unbraced ';'; split declarations on
// ';' and each declaration on the FIRST ':'.
function collectVars(raw, into) {
  const css = typeof raw === "string" ? raw : (raw?.css ?? "");
  const blocks = css.matchAll(/([^{}]+)\{([^{}]*)\}/g);
  for (const [, selector, body] of blocks) {
    const bucket = selector.includes('data-scheme="dark"') ? into.dark : into.light;
    for (const decl of body.split(";")) {
      const i = decl.indexOf(":");
      if (i < 0) continue;
      const name = decl.slice(0, i).trim();
      if (!name.startsWith("--")) continue;
      bucket.set(name, decl.slice(i + 1).trim());
    }
  }
}

let baseCss = null;

for (const presetName of PRESETS) {
  const preset = (await import(join(genDir, "npm", "node_modules",
    "@primeuix/themes/dist", presetName, "index.mjs"))).default;
  Theme.setTheme({ preset, options: { prefix: "p", darkModeSelector: DARK, cssLayer: false } });

  const vars = { light: new Map(), dark: new Map() };
  const common = Theme.getCommon("common");
  collectVars(common.primitive, vars);
  collectVars(common.semantic, vars);
  for (const c of COMPONENTS) collectVars(Theme.getComponent(c).css ?? "", vars);

  writeFileSync(join(work, "tokens", `${presetName}.json`), JSON.stringify({
    light: [...vars.light.entries()],
    dark: [...vars.dark.entries()],
  }, null, 1));
  console.log(`${presetName}: ${vars.light.size} light + ${vars.dark.size} dark tokens`);

  // Component CSS is preset-independent (pure var(--p-*) consumers) — extract once.
  if (baseCss === null) {
    const { style: base } = await req2("base");
    baseCss = evaluateDtExpressions(String(base ?? ""), dt) + "\n" + (common.style ?? "");
    writeFileSync(join(work, "css", "base.css"), baseCss);
    for (const c of COMPONENTS) {
      const { style } = await req2(c);
      const resolved = evaluateDtExpressions(String(style), dt);
      if (resolved.includes("dt(")) throw new Error(`unresolved dt() in ${c}`);
      writeFileSync(join(work, "css", `${c}.css`), resolved);
    }
    console.log(`css: base + ${COMPONENTS.length} components extracted`);
  }
}

async function req2(name) {
  return import(join(genDir, "npm", "node_modules", "@primeuix/styles/dist", name, "index.mjs"));
}
