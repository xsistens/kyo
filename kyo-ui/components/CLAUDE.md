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

## One setter per slot: the `A | Signal[A]` union

Every value-bearing slot is ONE setter taking `A | Signal[A]`, never a constant/`Signal` overload
pair. `ReactiveValue(v)` is the smart constructor that stores the right case, and it dispatches on
the argument's RUNTIME class: `SignalRef` first (two-way `ReactiveVariable`), then `Signal`
(one-way `Dyn`), then the plain value (`Const`). `Signal` is sealed and `SignalRef` is its only
named subclass, so the two reactive cases are exact rather than a heuristic.

- **Do not reintroduce the pair.** A `def x(v: A)` beside a `def x(sig: Signal[A])` is the shape
  this module removed; the union covers both plus `if c then a else sig`, which the overloads
  could not type at all.
- **Two-way is decided at runtime, so a type ascription cannot opt out.** `slider.value(ref:
  Signal[Double])` still binds two way. `ref.readOnly` (kyo-core `SignalRef.readOnly`) is the
  opt-out, and it returns a `Signal` that is deliberately not a `SignalRef`.
- **Read-only hosts need no branch for it.** `ReactiveVariable` IS-A `ReactiveValue.Dyn`, so an
  existing `case Dyn(sig)` keeps matching. Match `ReactiveVariable` BEFORE `Dyn` wherever the
  write-back ref is actually wanted, or the two-way case is silently swallowed.
- **`invalidMessage` is the one deliberate exception.** Its two forms carry different element
  types (`String` always shown vs `Signal[Maybe[String]]` that can clear the row), so
  `String | Signal[Maybe[String]]` would be a heterogeneous union, not this pattern.

## Shared slots live in traits, not in every component

A slot several components carry is defined ONCE, in `FormControl.scala` (validity: `invalid`,
`invalidMessage`) or `Capabilities.scala` (`HasAccessibleName`, `HasAccessibleNameRef`,
`HasAccessibleDescription`, `HasPlaceholder`, `HasTooltip`, `HasEmptyContent`). The trait carries
the setter and its scaladoc; a component supplies only the one-line `withX` writer, because the
field names differ (`accNameV` in the field-shaped controls, `accessibleNameV` in the containers).

- **Adding a slot to a component that already has a trait for it means mixing the trait in**, not
  writing another setter. A second spelling of an existing slot is the drift these traits exist to
  stop.
- **A trait requires the storage to be genuinely uniform.** `disabled` is NOT one of these: 14
  components store `Maybe[BoolValue]` and honour a reactive value, 19 store a plain `Boolean` with
  no reactive path in their render. `severity` is not either: its storage disagrees on whether the
  slot is optional. Check the storage before lifting; forcing a trait onto the second group is a
  behaviour change per component, not a refactor.
- **A slot only some implementors have gets its own trait** (`HasAccessibleNameRef` extends
  `HasAccessibleName` for the 21 of 38 that carry the reference), never an abstract member the
  others cannot answer.

## Typed builders: where inference dies

A setter that takes a projection of the component's own element type (`Column`'s
`_.name`) infers only while the expected type is known. `columns(Column("Name")(_.name))`
works, because the varargs element type fixes `A`; chaining anything onto it does not,
because the receiver is then typed alone and `A` widens to `Any`. The fix that works here
is a context function over a marker scope (`ColumnScope[A]`), which fixes `A` before the
argument is typed. Two things that do NOT work, both tried:

- Keeping the old signature as an OVERLOAD beside the scoped one. With two alternatives
  the arguments are typed without a definite expected type, the scope is never
  established, and every scoped call fails with `No given instance`.
- Relying on the scoped form to cover a splat of prepared values. A splat applies no
  per-element conversion; the lift has to be a `Conversion` on the sequence as a whole.
  Put it in the SCOPE's companion (`object ColumnScope`), generic in the element type, not
  in each element type's own. Once the element types are related (`Column <: ColumnTree`)
  two element-specific givens both match a `Seq[Column]` argument whose expected element
  is the supertype, and the call fails as ambiguous. The scope is the one type every such
  query mentions, so one given there serves every element type without overlap.

The same shape carries `groupBy(uic.group(_.category))` (`GroupScope[A]`), with one
difference that matters: `column` takes the header first and reads `A` from the `using`
clause between its two lists, but a grouping level's first real parameter IS the
projection, so its `using` clause has to come FIRST (`def group[A](using GroupScope[A])(key:
A => String)`). Put it after and `A` is still undetermined when `_.category` is typed.

A carrier shared by two components (`Column`, by `DataTable` and `TreeTable`) offers its
whole setter surface to both. Gate the ones a host cannot honor with a covariant PHANTOM
kind on the carrier (`Column[A, +K <: FlatOnly]`): the flat-only setters return
`Column[A, FlatOnly]`, and the narrow host asks for evidence (`AnyTableColumn[K]`). Two
things this buys that a plain subtype bound does not:

- The error message. With `columns(cs: ColumnOf[A, AnyTable]*)` the mismatch makes the
  compiler retype the argument without an expected type, the scope is never established,
  and the failure surfaces as `No given instance of ColumnScope[A]`. With the evidence as
  a separate `using` after the varargs, the arguments type fine, `K` is solved, and the
  failure is the evidence's own `@implicitNotFound` message.
- A cast-free store. Let the evidence carry the coercion (`private[uic] def widen`), since
  `K = AnyTable` is proven at the call site but not at the copy site.

More than one fact fits in that ONE phantom slot, as a subtyping lattice rather than as
extra parameters. `Column` carries both "which tables take it" and "does it have a text
projection" in four traits (`TextAnyTable <: AnyTable <: FlatOnly`, `TextAnyTable <:
TextFlatOnly <: FlatOnly`), which is what lets the bare `rowSpan` require `K <:<
TextFlatOnly`. Two consequences to know:

- Because `Column` is covariant in the kind, a caller can always FORGET a fact: a prepared
  list still annotates `Seq[Column[R, AnyTable]]` even though its elements are
  `TextAnyTable`. That is what keeps the extra fact free.
- A setter cannot REMOVE one component of the kind and keep another (`footer` has to drop
  `AnyTable`, so it drops the text fact with it). A match type would express it and does
  not compile: it puts `K` in an invariant position, which a covariant parameter forbids.
  So flat-only setters reset the kind, and the ORDER matters, which the message says.

Also: ANY failure INSIDE a `columns(...)` argument is reported against the `ColumnScope`,
not against the constraint that actually failed, for the retype reason above. `HasText`'s
own `@implicitNotFound` only shows for a standalone column, and a column setter reached for
on a `HeaderGroup` (which has none) surfaces there too, so `ColumnScope`'s message has to
name every cause it stands in front of.

The header is a sealed ADT over the columns (`ColumnTree` = `Column` | `HeaderGroup`), and
Scala 3 wants a sealed trait's direct children in ITS file. That is what moved the whole
column vocabulary out of `DataTable.scala` into `Column.scala`: a sealed family cannot be
split across files, so the split has to be drawn around the family, not around the
component that consumes it.

`typeCheck`/`typeCheckFailure` results are baked at TEST-compile time. A change to the
main sources alone may leave them stale: a snippet the real compiler now rejects can still
report green (observed while adding this gate, `touch` on the test file does not help
either, since bloop hashes content). After changing a gated signature, edit the test file
or clean the test module before trusting the suite.

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
- `autoFlip` only flips DECISIVELY where nothing inside the panel is focus-seeded.
  kyo's focus-seed calls `element.focus()` without `preventScroll`, so a seeded
  element gets scrolled into view by the browser before the measure lands: the
  panel stays on-screen but does not visibly flip. That is now the menu family
  (whose LIST is seeded) and a filterable Select/MultiSelect (whose filter input
  is); the option panels themselves seed nothing. Verify flip on one of those
  near the viewport bottom.

- **Focus is a single-owner property, and `aria-activedescendant` follows it.** The
  attribute is read off the element with DOM focus or off nothing at all, so it
  belongs on the one element that holds focus while the popup is open: the trigger
  for Select/MultiSelect/CascadeSelect/TreeSelect, the input for AutoComplete and
  for a filterable Select/MultiSelect, the LIST for the popup menus. A panel that
  seeds focus takes the announcement away from whatever was carrying it, which is
  how the whole family ended up announcing nothing. `GoldenRenderTest`'s "a
  highlight names an element that says what it is" holds both halves: the target
  must have a real role, and the carrier must be a tab stop, focus-seeded, or a
  text box.

## One activation, one handler

A key press on a native control is an ACTIVATION, and the browser reports it more than once: an
arrow that selects a radio fires `change` on the input AND dispatches a `click` that bubbles out
of it. Wire both and the second one runs against a tree the first one has already re-rendered,
with the new value in hand. Rating did, and read the click as a second pick of the same star:
cancel-on-same-value fired and every arrow press ended at zero.

- **Put the handler where BOTH inputs arrive.** For a hidden native control inside a visible box,
  that is the box's click: a pointer press reaches only the box (the input is clipped away), and a
  key press reaches it through the input. `RatingTest` drives change-then-click, which is what the
  browser does, and re-reads the tree between them, which is what makes the trap visible.
- The framework-level twin of this is `doubleActivation` in the two clients, which suppresses the
  emulated activation on elements the browser already activates. It covers `button` and `a[href]`,
  not a handler pair a component wires itself.

## A panel whose keyboard lives outside it holds no tab stop

The combobox family keeps focus on the trigger (or, with a filter header, on that one input) and
steers the panel from there through `aria-activedescendant`. Everything else in the panel is then
a place the reader must not be able to put focus, for two reasons that both bit MultiSelect:

- **A key pressed inside the panel reaches the outside handler too.** Dispatch bubbles to every
  ancestor that declared the event, and the panel is rendered inside the trigger, so a Space on
  MultiSelect's header select-all toggled everything AND was read by the trigger as an activation
  of the highlighted option, deselecting it. The box lost its tab stop (`CheckBox.tabbable(false)`,
  package-internal) and gained a stamped id, so a key that does arrive from it (a pointer can still
  focus it) is recognised and ignored. Its keyboard route is a chord on the element that does hold
  focus: Ctrl or Cmd with A.
- **Tab then leaves the widget rather than walking into it**, so every panel in the family closes
  on Tab in both directions. Handling it in the panel's key function rather than by watching focus
  is what makes it work in both transports: a close is a ref write like any other, and the browser
  has already moved focus by the time it lands, which is exactly right when nothing inside the
  panel could have taken it.

The same Tab rule holds where the popup's rows ARE real controls and hold real focus
(`SpeedDial`, whose actions are Buttons): the trigger is the widget's one tab stop, no row is in
the tab sequence, focus enters by opening and leaves by Tab, which closes. Giving the active row a
tab stop as well makes the open widget two of them, and Tab then walks the reader between the
trigger and its own popup instead of past the widget. Nothing is lost by it, because the way back
into an open popup is the same key that opened it.

## An opening key has to land on a row

`ArrowDown` on a closed combobox opens it AND puts the highlight on a row: the selected one, else
the first the highlight may sit on. Opening without landing looks like a dropped keystroke, because
the reader's next arrow moves from nowhere to the first row, which is where they thought the first
press had already put them. AutoComplete had this right and its four siblings did not; the seed
belongs in the component's own open path (`openPanel`), which is also the path a pointer takes.

## A control that reads several refs is ONE region

Nesting `ref.render` inside `ref.render` leaves the inner region subscribed against the value the
outer one held when it created it. That is invisible until one effect writes both refs: the outer
region re-renders, the inner one is still live against the older values, and its next emission
repaints them over the finished write. PickList read four refs through four nested renders and a
transfer writes three of them, so a plain click on a row put a transferred item back while the
refs themselves held the correct result.

- **Combine the signals instead**: `a.combineLatest(b).render { case (x, y) => ... }` reads every
  value on every emission, so there is one region and nothing to be stale. `Signal.initConst`
  stands in for an unbound slot, and is built for exactly this pairing.
- `UicTest.regionsAbove` counts the subscriptions a control wraps its markup in, so the rule is
  checked rather than remembered.
- The roving highlight of an embedded list belongs in the same combined signal, not in a region of
  its own: the columns and their cursors are one state.

## An unbound ref is not a reason to disable a control

Optional bindings (`month`, `currentView`, `selected`) exist so a page CAN drive a component from
outside, not so the component only works when it is driven. A control wired straight to such a ref
and rendered `disabled` without one is a control the reader can neither click nor tab to, and
whatever it leads to goes with it: DatePicker's month and year title buttons rendered disabled by
default, which left the month and year grids, and the keyboard those grids had just been given,
unreachable in every picker that bound no `currentView`.

- **The mount mints what the caller did not bind**, next to the ids it already mints
  (`DatePicker.render`: the open, month, view and cursor refs), and hands them to the `wired`
  seam. The static projection is the only place a control renders disabled for want of a ref, and
  that projection is inert throughout.
- **The seam must mint the way the mount does**, from the component's own fields
  (`dp.currentViewRefV` else `Signal.initRef(dp.viewV)`), or the golden documents a control the
  reader never meets and a caller's own binding stops working through the seam.
- `aria-disabled` is the right answer where the control is genuinely inapplicable right now: it
  keeps the tab stop and says so. Native `disabled` removes it from the keyboard entirely, which
  is also how OrderList's move button lost the focus mid-press.

## A cancel remembers at the first write, not at the open

Escape on a panel that can be cancelled has to put back what the reader started with, and the
obvious place to record that is the open. It is the wrong place: the open ref is a caller's to
write, so a panel opened from application code never passes through the component's open path and
the memory is either stale or missing. Record at the FIRST write inside the panel instead, whoever
opened it and however: before that moment there is nothing to undo, and after it the memory is
exactly the state the reader is about to leave.

- **A close that commits forgets.** Clear the memory wherever the panel closes on a pick, so the
  next Escape after a reopen has nothing old to put back.
- **The undo writes refs and stays silent on the change callback.** Over the whole open nothing
  changed, so an `onChange` for the undo would report a change that did not happen; a ref-observing
  page still sees the restored value, because that is where the value lives.
- **An in-flow variant has nothing to cancel** (`DatePicker.inline`): it does not close, and what
  is picked in the page is picked. Guard the cancel on the floating form, not just its close.

## `readonly` is one thing

A readonly control is one a reader can REACH and read the value of and cannot change:
focusable, saying so in the vocabulary its own role has (`aria-readonly` where the role
supports it, `aria-disabled` on a `role="button"`, which has no readonly state), and inert
to every key and click that would change it. `disabled` is the different thing, and stays
native. Select, CheckBox, ToggleSwitch, Rating and ToggleButton all answer this way;
`ReadonlyTest` holds them to it.

- **Do not implement readonly by disabling the native input.** It drops the tab stop, so a
  keyboard reader cannot reach the value at all, and it makes readonly and disabled the same
  thing to a screen reader (which reports the disabled state and ignores `aria-readonly`
  beside it).
- **A handler cannot decline the browser's default**, since it runs asynchronously and
  remotely: by the time it is asked, the checkbox has already toggled. `UI.preventActivation`
  is the kyo-ui primitive that declines it in the client, the same way `preventScrollKeys`
  declines the page scroll. Both read from `kyo.internal.KeyPolicy`, and `KeyPolicyTest`
  holds the two transports against it.

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
