# kyo-ui-components: component conventions (load-bearing)

Rules that bite repeatedly when building or extending components in
`kyo-ui/components/shared/src/main/scala/kyo/uic/`. Read before touching a component.
The component catalog, the composition story and the parity notes are in this
module's `README.md`.

## kyo-ui is in-tree

- Components build against `kyo-ui` from the same build, not a published artifact.
  Its public API is `kyo-ui/shared/src/main/scala/kyo/UI.scala`; the JS client
  (pointer, measure and command transport) is
  `kyo-ui/js-wasm/src/main/scala/kyo/internal/DomBackend.scala`.
- Do not assume a kyo-ui capability exists: grep the source. A capability this module
  needs and kyo-ui lacks is a kyo-ui change, landed as its own commit, not a shim here.

## Anatomy: `.p-*` verbatim, the extracted sheet is the truth

- Every component mirrors PrimeReact/PrimeVue anatomy so the extracted `@primeuix`
  sheets apply verbatim. Class names are Prime's exact `.p-*`, never an invented
  `.p-*` class. The generated sheets are `generated/ComponentCss.scala`
  (`Theme.primeCss`); kyo-specific glue lives in `Theme.primeExtraCss` under
  `.p-uic-*` names.
- **The extracted sheet is anatomy ground truth, NOT the v10 React docs.** When
  the API reference (PrimeReact v10 docs) and the extracted PrimeVue-4 sheet
  disagree, the sheet wins; otherwise you build dead anatomy (styling classes
  the sheet never targets). Read the sheet (`grep -o "p-<comp>[a-z-]*" generated/ComponentCss.scala`)
  BEFORE deciding a component's structure. Behavior DEFAULTS (e.g. select-all is
  binary, highlightOnSelect is off) come from the live PrimeVue component, not
  the sheet: verify those against the running reference.
- Where kyo-ui cannot render the target DOM, re-scope the CSS rules onto stamped
  classes rather than faking anatomy. Check first whether kyo-ui really cannot:
  DataTable and TreeTable carried a `tr.p-uic-*-row` re-scope long after `UI.thead`
  and `UI.tbody` made the real row groups available, and every rule it hand-copied
  was already in the extracted sheet, scoped to the anatomy it was avoiding.

## Self-addressing: the Commands channel

`UI.commands` (`Env.get[UI.Commands]`) is the escape hatch for the two things a
render diff cannot express: imperative DOM commands (`focus`, `scrollIntoView`)
and measuring an element (`requestMeasure*`). Load-bearing rules:

- **Commands resolve ONLY inside a `UI.mounted` effect or an event handler**,
  never in pure build/SSR render. In pure render there is no `Env[Commands]`.
- **A reusable component has no way to learn its own render path.** Use the
  id-addressed twins: in a `UI.mounted` effect, `id <- cmds.freshId`, stamp it
  with `.id(id)` on your own element, then drive `focusId(id)` /
  `scrollIntoViewId(id)` / `requestMeasureById(id)`. `freshId` mints
  session-unique `kyo-uic-N` deterministically.
- **`uic.Button.onClick` accepts only `Abort[Throwable] & Async`, NOT
  `Env[Commands]`.** For a handler that uses `UI.commands`, drop to the raw kyo
  `button`/element (effect-polymorphic `onClick`). Capture `cmds` once in the
  outer mount and pass it down so inner effects need no `Env`.
- **A self-command MUST fire AFTER the element is inserted.** The client resolves
  `getElementById` immediately (no rAF, no retry) and the command op can reach it
  BEFORE the render/insert op. So never `focusId`/`scrollIntoViewId`/`requestMeasureById`
  in the same effect that also yields the element: it races the publish and
  misses. The robust pattern is a nested `UI.mounted` whose effect runs after the
  enclosing content is published:
  - Overlay flip: a `measureTrigger` sibling `UI.mounted` requests the measure
    once the panel (with its stamped id) is in the DOM (`Overlay.scala`).
  - Terminal auto-scroll: the newest row carries an invisible on-mount child that
    scrolls ITSELF into view after insertion, not a scroll from the append
    handler (that races the re-render; `scrollTop` stays 0 even though the
    container is scrollable). See `Terminal.scala`.
  - OTP auto-advance advances from the input handler (the target cell already
    exists from the prior render), so no nested mount is needed there.

## Pointer and drag: the stable-surface idiom

`onPointerDown/Move/Up(PointerEvent => ...)` open a `setPointerCapture` drag
session; `PointerEvent` carries `x/y` relative to the element under the cursor
plus its `rectX/Y/W/H` (normalize to 0..1 without a second round-trip).

- **The element carrying the pointer handler must NOT be replaced during the
  drag.** A per-frame re-render that re-emits the dragged element detaches the
  captured node (`rect` goes to 0, the drag dies). Put the handler on a STABLE
  surface built ONCE, outside the reactive region; the moving visual is a
  `pointer-events:none` reactive child that reads the ref LIVE (`ref.get`), never
  a closed-over render-time value. Reference: `ColorPicker.scala` (plane and hue
  surfaces), `Knob.scala` (drag on the outer `.p-knob` div, svg dial is
  `pointer-events:none`).
- **`PointerEvent` rect = the element UNDER the cursor.** Children of the drag
  surface must be `pointer-events:none` so the pointer lands on the surface whose
  rect defines the coordinate space.
- For a box that MOVES as a whole (Dialog draggable): keep the header/handle as
  STABLE children reused by the positional diff; the reactive region only
  re-styles the box element (translate/size). Drop the enter animation on the
  moving box, since a from-state class replays every frame and flickers. A
  `transition` on `transform` also smears the live drag, so add a
  `transition:none` class (`.p-uic-dialog-movable`). See `Dialog.scala`.
- On touch, the drag surface needs `touch-action:none` (glue in `Theme.primeExtraCss`).

## The wired golden seam

Golden tests (`GoldenRenderTest`, all four platforms) render through the real `HtmlRenderer`,
which shows a `UI.mounted` region as its PLACEHOLDER (the mount effect does not
run). So a component whose live behavior lives in a mount exposes a
`private[uic] def wired(...)` that returns the published subtree directly; the
golden test calls it to assert the anatomy the placeholder cannot show (stamped
ids, pointer handlers, focus wiring).

- **Keep the wired seam Commands-free** so `kyo.uic.test` can drive it: pass id
  lists plus a plain `String => Any < Async` scroll/focus function, not a
  `UI.Commands` (`Commands.init` is `private[kyo]`, unreachable from the test).
  Production wires `id => cmds.focusId(id)`; the test wires `_ => ()`. See
  `InputOtp.wired`, `Terminal.wired`, `Carousel.wired`.
- `private[uic]` members (including `wired` and pure helpers like
  `Knob.valueFromPointer`, `Overlay.flipAnchor`) ARE reachable from `kyo.uic.test`,
  because package-private includes subpackages. Unit-test pure geometry directly there.
- Discoverability tests (`DiscoverabilityTest`, all four platforms) gate each setter by
  concrete return type: inapplicable options must not compile. When you change a
  setter's signature (e.g. `FileUpload.onSelect` String to `Seq[FilePayload]`),
  update the discoverability property that pinned the old shape.

## Overlay and consumers

- `Overlay(open)(...).render` (the standard path) gets `autoFlip` (default true):
  on open it self-measures and flips Bottom to Top, or shifts horizontally. The
  host-gated `renderOpen` (menu-family nested submenus) keeps the declared anchor,
  because nesting a mount into an already-subscribed region would double-subscribe
  and duplicate the panel DOM (one subscription per ref chain; `renderOpen` is the
  single-subscription form).
- `autoFlip` only flips DECISIVELY for `seedFocus(false)` overlays. kyo's
  focus-seed calls `element.focus()` without `preventScroll`, so a seedFocus-on
  panel (Select/MultiSelect/Menu) gets scrolled into view by the browser before
  the measure lands: it stays on-screen but does not visibly flip. Verify flip on
  a seedFocus(false) panel near the viewport bottom.

## Enter vs leave animations

- **Enter = a transient FROM-STATE class plus a `transition` on the base element**
  (`.p-uic-enter-fade { opacity:0 }` released next frame). NEVER apply Prime's
  own `-enter-active` keyframe permanently: its fill-forwards animation paints
  the element transparent (a computed-style probe misses it; only a screenshot
  catches it). Golden tests assert the ABSENCE of `-enter-active`.
- **Leave = Prime's own `-leave-active` keyframe** via kyo-ui's leave ghost
  (`.leaveTransition("p-...-leave-active")`), which holds the class until
  animationend.

## Reactive-wrapper DOM signature

kyo renders a reactive child in a classless `span[data-kyo-reactive]`, which
shares its `data-kyo-path` with its first child and breaks Prime's structural
child selectors (`.p-iconfield > .p-inputtext`). Composition CSS must address the
wrapper (mirror Prime's `.p-inputwrapper` pattern); focus-restore prefers
`:not([data-kyo-reactive])`. `<li>` rows need a real `<ul>` scope
(`display:contents` for layout transparency), since a `<div>` does not stop the
parser from hoisting a nested `<li>` into the outer list.

## The demo lives outside this repository

The interactive demo (one page per component, one section per feature) is the
separate `kyo-ui-components` repository, which consumes this module as a published
artifact. Its contract, when you change an example there: each section is
`Doc.section(title, description, codeString)(liveUI)` and the `codeString` MUST
byte-match the `liveUI` it renders beside (layout scaffolding excepted), because it
is shown to the reader as the copy-paste source. Change both or neither.

An API change here is not finished until that demo compiles against the new
version. A rename with no counterpart in the demo is a rename that has not been
carried through.

## Effect-as-value trap

An untyped `match` with `Unit < Async` and `Unit` branches infers `Any`: the
effect lands inert in a by-name handler and silently does nothing (a "dead"
button). Type handler-effect branches explicitly (`val eff: Any < Async = ...`).
The build runs `-Werror`, so a stray `@nowarn` that no longer suppresses anything
fails it.
