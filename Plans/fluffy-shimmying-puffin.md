# Next: kyo-ui-components-base and kyo-ui-components-prime (T27, updated 2026-11-09)

## Context

What a component is built from now: `final class ToggleButton(val p: Props) extends Component(Setters[Props, ToggleButton])`.
`Setters` (kyo-ui) generates a setter per field: a plain field takes its type, an `Opt` field the value itself, a `UI.Ownable`
field a binding setter and its event (`on<Field>Change` or `@eventName`), a callback field (`Maybe[A => UI.Action]`) the function,
every handed function under `UI.attempt`; a member the class declares wins, `@noSetter` opts out. Generic code writes a setter it
cannot see through `HasSetter[C, N, A]` (uic), found in `Fields` or given by a trait for a hand-written member
(`BooleanFormControl`), so the form's Boolean `bind` serves both styles. All of it is committed and green (T22, T30, T31).

Chris (2026-11-09): what is needed to build components belongs in a module of its own, `kyo-ui-components-base`; the ready-made
components become `kyo-ui-components-prime`. kyo-ui keeps only the foundation the base needs without workarounds or performance
loss. kyo-ui never depends on either module, and behaviour does not change. Flavio decides later whether base stays separate or
joins kyo-ui.

Dependency direction: `kyo-ui` <- `kyo-ui-components-base` <- `kyo-ui-components-prime` (package `kyo.uic` kept for prime).

## What lives where

**Stays in kyo-ui (foundation the engine or kyo-ui itself uses):**

- `UI.Prop` with `signal`, `map`, `current`; the element setters on `Prop` (`disabled`, `readOnly`, `hidden`, `title`,
  `aria(name, Prop)`, `cssClass(name, Prop)`, `cssClass(Signal[Maybe[String]])`, `ariaFlag`, `silent`) and the child lifts.
- All reactive channels, silent handlers, `UI.Children`/`UI.children(parts*)`.
- `mountedWith`, `Live`, `UI.Ownable` and `internal/Owning` (the engine resolves `Ownable` fields), `UI.attempt`.

**Moves to base (package `kyo`):**

- From kyo-ui: `Setters`, `noSetter`, `eventName` (`Setters.scala`) with `SettersTest`; `UI.reactive` with `ReactiveScope`, the
  `.read` extensions and their stray guard (`UI.scala` ~288 to 335), `internal/ReactiveMacro.scala` with `ReactiveMacroTest`;
  `Prop.render`, `Prop.zip`, `Prop.writable` with their `PropTest` cases. Call sites stay as they are: `reactive` becomes an
  `extension (u: UI.type)`, the three ops an `extension (p: UI.Prop.type)`, both found through `import kyo.*`.
- From uic (`Component.scala`): `Opt`, `Component`, `HasSetter` with `ComponentTest`. `private[uic]` on `Component.setters` and
  `p` becomes `private[kyo]` (prime is inside `kyo`). No name clashes in kyo (checked: no other `Opt`, `Component`, `HasSetter`,
  `Setters`).

**Stays in prime (uic):** every component, `Controlled` (the former `Slot`, which needs `Callback`, `Diagnostics`,
`ReactiveValue`), `Callback`, the form layer, the shared traits and their `HasSetter` bridges (`BooleanFormControl`'s companion),
`RULEBOOK.md`, `CLAUDE.md`, `README.md`.

## Steps (each ends green, its own commits, ISA in its own commit, nothing pushed)

1. **Two modules.** `git mv kyo-ui/components/{shared,jvm,js,native,wasm,...}` to `kyo-ui/components/prime/`, generator to
   `kyo-ui/components/prime/gen`; new empty crossProject `kyo-ui-components-base` in `kyo-ui/components/base` (same settings as
   today's `kyo-ui-components`: four platforms, `withKyoTest`, `openssl-native-settings`, CommonJS on JS), depending on `kyo-ui`;
   `kyo-ui-components` renamed `kyo-ui-components-prime`, depending on base. Aggregates in `build.sbt` (the jvm/js/native/wasm
   lists near lines 521/629/723/814), `README.md` row, `stage1/tools/measure.sh`. Docs stay in `kyo-ui/components/`.
2. **Opt, Component, HasSetter into base**, with `ComponentTest`; prime imports nothing new.
3. **Setters, noSetter, eventName into base**, with `SettersTest`. kyo-ui then has no user of them (checked).
4. **reactive and the Prop ops into base**, as extensions; `ReactiveMacroTest` and the moved `PropTest` cases go to base tests.
   Whatever `private[kyo]` the macro reaches stays reachable (base is in package `kyo`).
5. **Demo and docs.** Demo `~/Programming/kyo-ui-components`: dependency on `kyo-ui-components-prime`, `ApiMacro` reads a
   generated setter's default from the `Props` parameter default (it reads setter bodies today, and generated setters have
   none); compile it. `RULEBOOK.md` gains the rule for `Component`, `@noSetter`, `@eventName` and `HasSetter`;
   `components/CLAUDE.md` names the modules.

## Critical files

- `build.sbt` (`kyo-ui-components` at ~4318, `-gen` at ~4366, aggregates)
- `kyo-ui/shared/src/main/scala/kyo/UI.scala` (`Prop` ~142, `ReactiveScope`/`reactive` ~288 to 335, `Ownable` ~271 stays)
- `kyo-ui/shared/src/main/scala/kyo/{Setters.scala, internal/ReactiveMacro.scala}`
- `kyo-ui/components/shared/src/main/scala/kyo/uic/{Component,FormControl,form/Field}.scala`
- tests: `kyo-ui/shared/src/test/scala/kyo/{SettersTest,ReactiveMacroTest,PropTest}.scala`, uic `ComponentTest.scala`

## Verification

- Each step one background run, one heavy build at a time. Steps 1 to 3: base tests, `kyo-ui-components-primeJVM/test`
  (2733 today) and kyo-ui's moved-from suites; step 4 adds `kyo-uiJVM/test` (3122 today).
- Behaviour unchanged: prime 2733/2733 and goldens byte-equal after every step; Discoverability and NodeSurface green (setters
  compile from outside, internals hidden).
- Direction: `kyo-uiJVM/compile` with neither module on its classpath, checked in `build.sbt`.
- JS at the end: kyo-ui, base and prime JS tests. A JS test compile from nothing needs about 8 GB in one batch (T34), so a
  rebuild compiles kyo-ui's shared tests first, then the rest incrementally, and runs with the 2 GB-reserve watchdog copy.
- The demo compiles; presentation-compiler completion probe at `uic.ToggleButton().` once more against the tree.
# Done: silent handlers in kyo-ui, then ToggleButton (T22)

## Context

ToggleButton was rebuilt to Button's standard (uncommitted, `stage1/t22-tb.log`: 6 of 2729 red). `disabled` and `readonly` became
channels, so a locked button kept a click handler that checks the flags and does nothing. Chris (2026-11-09): a locked element's
handler should really disappear and come back by itself, so a handler never carries locking logic; no boilerplate in app code,
no rerender or rebuild of the component. Five red tests asserted "no handler" (`attrs.onClick.isEmpty`, `presses`), one timed out
waiting for a repaint that a channel no longer makes.

## Design (kyo-ui foundation)

- Every element gets `silent(v: Boolean | Signal[Boolean])`; an element is silent while `disabled || silent`. `disabled` silences
  by itself, as a native disabled button gets no click from the browser anyway.
- `data-kyo-ev` (written by `HtmlRenderer.renderEventAttr`, ~1335) lists no event of a silent element. A constant: the
  attribute is not written, so the bytes are those of an element with no handler. A signal: the render writes the current
  state and binds the attribute to the existing maybe-attribute channel (`reactiveMaybeAttrs`: Present sets, Absent removes),
  so a change patches in place and draws nothing.
- Client unchanged: both transports read `data-kyo-ev` at event time (`DomBackend.scala` ~1398, clientJs ~2226); an event
  passes on to the nearest ancestor that lists it, as with no handler.
- Server: `EventDispatch` checks the element's current silent state before calling its handler, so a click sent before the
  patch arrived reaches nothing.
- Decided (Chris, 2026-11-09), from the agent's build plan `Plans/fluffy-shimmying-puffin-agent-a681cf7b1624dcda4.md`, which is
  the detailed blueprint (Attrs `silent`/`silentSignal`, one silence decision in `Channels.of` feeding `data-kyo-ev` through
  `maybeAttrs`, `renderEventAttr` split into `declaredEvents`/`eventList`, a new `kyo/internal/EventDispatchTest.scala`):
  - Dropdown: the clean version, clients included. Both transports read the Dropdown's `data-kyo-ev` before posting an
    option pick's `Change` (`HtmlRenderer.scala` ~813 and ~2553-2560, the DomBackend twin), and the wrapper's
    `data-kyo-ev` follows silence like every element's. The server skip stays as the second guard.
  - Bubble path: a silent element never runs its handlers, as target or as an ancestor an event bubbles through.
  - Taken as proposed: `disabled` counts on `HasDisabled` elements only, not their descendants; `onAttached` still runs on a
    silent element; a signal-silent element gets `data-kyo-path`.
  - A full `kyo-uiJVM/test` run after the targeted suites, since disabled ancestors change behaviour.
- Red first: render (constant silent and disabled write no `data-kyo-ev`; signal writes current and binds the channel),
  channel binding (off/on removes and restores the attribute on both sinks), dispatch (a silent element's handler is not
  called). Files: `UI.scala` (Attrs, `silent`), `HtmlRenderer.scala`, `Channels.scala`, `ReactiveUI.scala`,
  `EventDispatch.scala`; tests `AttrsTest`, `ReactiveChannelBindingTest`, an `EventDispatch` test. Own kyo-ui commit.

## ToggleButton on it

- `.onClick(toggle)`, radio `.onKeyDown(r.onKey)` with no flag checks, `.disabled(off)`, `.silent(inert)`; `ariaFlag` stays.
- The six red tests then judge the drawn element: no `click` in `data-kyo-ev` while locked, back when unlocked, and the
  readonly-from-signal case waits for the attribute patch instead of a repaint. Goldens with a disabled element and a handler
  lose `click` from `data-kyo-ev`: shown to Chris one by one.
- Rulebook: rule 6 says a locked element is silent, never a handler that checks the flag. Button: its `if off then Kyo.unit`
  goes, `disabled(off)` silences it (its `ariaDisabled` click stays, deliberately).
- Verify: kyo-ui suites above, the whole uic suite, goldens reviewed, regions equal or fewer.

---

