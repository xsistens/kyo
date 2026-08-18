# kyo-ui-components-gen: the extraction and codegen pipeline

Paths below are relative to this directory (`kyo-ui/components/gen`). Inputs are pinned
and gitignored; the generated Scala is committed under
`kyo-ui/components/shared/src/main/scala/kyo/uic/generated`. Rerun:

```
./fetch.sh                       # bun install (npm/) + extract.mjs + fiori icons download
sbt kyo-ui-components-gen/run    # FioriIconsGen, PrimeIconsGen, TokensGen, ComponentCssGen
```

## extract.mjs (the load-bearing step)

Drives the MIT `@primeuix/styled` engine outside any framework, which is exactly what
PrimeVue's styled mode does at runtime, frozen to static output at build time:

1. Per preset (aura/material/lara/nora): `Theme.setTheme({preset, options})`,
   then `Theme.getCommon()` + `Theme.getComponent(name)` and parse the emitted
   `:root`/dark-selector blocks into `work/tokens/<preset>.json`
   (`{light: [[name,value]], dark: [[...]]}`). Token names are taken verbatim
   from the engine's output, never re-derived.
2. Once: each `@primeuix/styles` template through
   `evaluateDtExpressions(style, dt)` into `work/css/<component>.css` with every
   `dt('token.path')` resolved to `var(--p-token-path)`; fails the build if any
   `dt(` survives.

## Generators (sbt kyo-ui-components-gen/run)

- **TokensGen** into `generated/Tokens.scala`: per preset a full light set plus the
  dark override set as `Seq[(String, String)]` (names without the leading dashes),
  chunked 250/def against JVM method limits.
- **ComponentCssGen** into `generated/ComponentCss.scala`: one `def` per
  extracted sheet plus `base` and `all`, triple-quoted, 40K-chunked against the
  64K string-constant limit.
- **PrimeIconsGen** into `generated/Icons.scala`: the primeicons raw SVGs, one
  `def` per glyph (Scala.js DCE strips unused path data); multi-`<path>` icons
  merge their `d` strings (all subpaths start with absolute `M`);
  `java.lang.Object` member collisions get an `Icon` suffix (`cloneIcon`).
- **FioriIconsGen** into `generated/FioriIcons.scala`: the legacy SAP-icons-v5
  set, kept as a second icon set by explicit decision (the ONLY remaining ui5
  input; downloaded by fetch.sh).

Neither icon count is restated here. Each generated file stamps its own count in
its header, and `IconsTest` asserts both off the compiled objects, so prose that
repeats a number can only drift away from it.

## Version bumps

Pin `@primeuix/styles` and `@primeuix/themes` to MATCHING majors (the `dt()`
paths must align with the preset token paths) in `npm/package.json`;
`primeicons` and the ui5-icons version live in `npm/package.json` and
`fetch.sh`. License boundary: only `@primeuix/*` 2.x (MIT), `primeicons`
(MIT), `primevue` 4 (MIT, demo devDep), never `@primereact/*@11` (commercial).
