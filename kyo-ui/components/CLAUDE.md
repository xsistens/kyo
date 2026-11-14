# kyo-ui-components: component conventions (load-bearing)

Rules that bite repeatedly when building or extending components in
`kyo-ui/components/prime/shared/src/main/scala/kyo/uic/` (module `kyo-ui-components-prime`; what a component is built from, `Setters`, `Component` and the rest, lives in `kyo-ui-components-base`, the same package). Read before touching a component.
How a component's code is written (names, the one draw, chains, channels) is `RULEBOOK.md`; read it too.
`aria` and `data` take `name -> value` entries, several in one call: `.aria(Aria.label -> p.access.name, Aria.pressed -> checked)`.
A Boolean or a number writes its own text (no `.toString`), `Absent` writes nothing, a signal is the attribute's channel
(RULEBOOK rules 4 and 6).
The component catalog, the composition story and the parity notes are in this
module's `README.md`.

## A component is a class over its props

A component the rulebook has reached (`Button`, `ToggleButton`, `Input`, `Password`, `TextArea`, `Select`) also
`extends Component(Setters[X.Props, X])` and has its setters
generated from its props (`RULEBOOK.md` rule 11). The rest still write each setter as below, until their wave reaches them.

```scala
final class InputMask private[uic] (private[uic] val p: InputMask.Props) extends Node, FormControl[String], HasProps:
    import p.*   // reads in the body stay as they are
    type Self = InputMask

    def size(v: Size): InputMask = new InputMask(p.copy(sizeV = v))

    private[uic] def validitySlots: FieldValidity.Slots         = validityV
    private[uic] def withValidity(v: FieldValidity.Slots): Self = new InputMask(p.copy(validityV = v))
    private[uic] def accessibility: Accessibility               = access
    private[uic] def withAccessibility(v: Accessibility): Self  = new InputMask(p.copy(access = v))
    private[uic] def bindRef(ref: SignalRef[String])(using Frame): InputMask    = value(ref)
    private[uic] def hearBlur(f: String => UI.Action)(using Frame): InputMask   = onBlur(f)
end InputMask

object InputMask:
    final private[uic] case class Props(
        sizeV: Size = Size.Normal,
        validityV: FieldValidity.Slots = FieldValidity.Slots.empty,
        access: Accessibility = Accessibility.none
    )

    def apply(): InputMask = new InputMask(Props())
end InputMask
```

So a reader typing `uic.InputMask().` outside this package is offered the setters and what every object has: no field, no `copy`,
no `_1`, no `product*`. `NodeSurfaceTest` compiles from outside `kyo.uic` and sweeps every component; a new one belongs in it.

- **Every field lives in `Props`**, the configuration the setters build. A setter is `new X(p.copy(...))`. A component reads
  another's field through its props (`button.p.labelV`), and so does a test, since `private[uic]` reaches subpackages. What a
  component holds while it is shown (an open panel, a highlight, an id) is state, and it is made in the component's mount.
- **`HasProps` gives back what a case class gave**: equality over the props and a `toString` (`Anchored` finds a menu item again
  by equality). A wrapper over one component (`MultipleAccordion`, `MultiSelectButton`, `MultiDatePicker`, `RangeDatePicker`) has
  no props of its own: it holds that component and is `HasProps` over it.
- **Four things `import p.*` does not settle.** A local named `p` hides the field, so a parameter `p` reads
  `this.p.copy(...)`. A setter named like its field wins over the import, so that read is `p.icon`. A `copy` in the body
  without `p.` is the props' and hands back a `Props`, which then fails to type. And the field hides kyo-ui's paragraph `p`
  inside the class, so a paragraph there is `UI.p`.
- **Two groups are shared, one case class each**, for every component that has those fields: `validityV`
  (`FieldValidity.Slots`, what `FieldValidity(validityV)` resolves per render) and `access` (`Accessibility`, the name, its
  reference and the id). The empty ones are shared values, so a component nobody marks stores nothing for them. Do not group
  anything else without deciding it first.
- **A name the companion already used stays with its owner.** `ContextMenu.State` is public API and real state (the open menu
  a caller drives), so it keeps its name beside `ContextMenu.Props`. The wired per-mount state of the pickers is `Wired`.
- **What a reader needs of the state gets a named public getter, and only that** (decided 2026-11-04). `BreadcrumbItem.href`,
  since `onItemClick` hands the crumb over and a handler routes on it; `MeterGroup.Meter.labelText`, since a template composes its
  label from it and is handed the percent; `Theme.Brand.sheet`, since emitting it is what a brand is for, beside `attr` from
  `Palette`. A getter is a decision about the surface, so `NodeSurfaceTest` names each one.
- **What stays a case class:** data people construct and match on (`FilterRule`, `SortKey`, `TableState`, `TreeNode`,
  `ToastMessage`, ...). Those are not components.

## A component with state of its own is an instance; one without is not

The Solid model (decided 2026-11-06, ISA F4): a component draws once per instance and reads each prop where it uses it. A host
region that runs again must not start a component over and take the reader's state with it.

- **Only a component that holds state of its own draws through `UI.mountedWith`**: a slot it keeps while nothing binds it (an
  unbound `checked`, a typed text), an open panel, a highlight, minted ids. The placement keys it on where the app writes it
  (`Node.placed`), so a region around it that runs again keeps the instance and hands it the run's props through its `Live`.
- **A stateless component (Button) has no instance.** It reads its `Prop`s directly: a constant is a plain value, a signal a
  channel or one region (`Prop.render`, `Prop.zip`). A host repaint has nothing of it to lose.
- **The template is `ToggleButton` (and `Paginator`)**: `render = UI.mountedWith(p)(X.drawn)`, and `drawn(props: UI.Live[Props])`
  runs once per instance. Each place reads the field it shows where it shows it: `props(_.size)` into a class channel,
  `props(_.checked.value(false))` into `when(checked)(_.cssClass(...))` and `aria`, a label into the content's own region. A run that changes one field
  patches that place alone and draws nothing else again. A state slot is a `Controlled` with a `start` (a `UI.Ownable` prop): where a
  run binds nothing the engine binds the instance's own ref in its place, minted once, so `props(_.checked.value(...))` always reads
  a bound slot, keeps the reader's value across runs, and carries on from what was shown when a run stops binding it.
- **What builds the element and has no channel is one `UI.reactive` block** over a small value of the structural fields (ToggleButton's
  `Base`: editable, fixed, unheard, validity, access, radio, never a hearer), so a run that only brings new hearers leaves it equal and
  draws nothing again. Inside the block a value that only reaches channels stays a channel (the macro's data flow), and blocks nest.
- **A handler reads the run when its event comes**: `props.call(_.checked).map(s => ...)`, so a click writes the ref the latest run
  bound and reaches that run's `onChange`, and a `SignalRef` stays two way. Never close over a value read while drawing.
- **A host that binds the slot itself and draws the component in its own region** (SelectButton's options) calls a mount-free
  `renderInPlace = drawn(new UI.Live(p, Absent))`: a `Live` no run follows reads every prop as it is, so a constant costs no
  subscription, and a mount per option would be a mount inside a mount.
- **A native field whose chrome follows its own state switches in place, never draws itself again** (Password): its text is always a
  channel (the instance's own ref, or the caller's binding, never a stated constant), its `type` follows the reveal through kyo-ui's
  `textType` channel (the package-private `Input.revealing`, which needs no number flag and so no region), and only what is drawn from the text (the meter) is a region, a sibling of
  the field. SplitButton builds both segments once and draws only its menu panel as a region.
- **Nothing under a region that runs again may state what the reader changes** (T13, `UI.Loss`). A run keeps every element in place
  (focus, caret, typed text and ticks survive it), but a native field's `value`, `checked` or `selected` stated as a constant is put
  back by every run, and a mount with no key starts over. Hand a field its value as a channel (an argument that reads, or a signal)
  and give a mount a key. `UI.reactive` refuses both where it can see them; kyo-ui reports the rest to a devtools sink
  (`UI.RenderKind.Lost`), and every uic suite counts them (`UicTest.countLoss`, a `UIC-LOSS` line per place). A component a wave
  frees goes on the list for which a loss fails the test (T19).
- **Every converted component enrols in the contract's host-run case** with `placed = Present(_.toUI)` on its `SlotCase` (or
  `nativeText`): the reader's change, then a host region around it that runs again with a new hearer, and what it shows must be
  unchanged. The case reads the tree the instance refills its place with (a host's tree holds a kept instance only as its place, and
  reading that place draws the component anew, at its start), and a text the browser keeps is judged by the field drawing no value
  of its own over it. A component-specific repaint test adds what the case cannot say (`ToggleButtonTest`: nothing is drawn again for
  a new label or a new hearer; `ButtonTest` for the stateless case).
- **During the waves** a converted component's own fields are `Prop`s; the shared traits (validity, accessibility, tooltip,
  description) and `Controlled` stay on `ReactiveValue` until the last wave removes it. `Prop.map` needs its type parameters spelled
  out on a value whose static type is already `Prop[A]` (ISA finding, task T8).

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
- **The SELECTION and VALUE slots follow it too, and the list of them was never complete.** `Tabs.selected` was the
  first converted, `DataTable.selected` the second, and the second is the warning about how such a list gets drawn
  up: it was not on it. The family was enumerated from the pickers, and a table's row selection is the same slot
  wearing different clothes. `Select`/`CascadeSelect`/`SelectButton` take `selection(A | Signal[A])` and
  `selection(Maybe[A] | Signal[Maybe[A]])` (one `@targetName`, since both erase to `Object`);
  `MultiSelect`/`MultiSelectButton`/`Listbox`/`TreeSelect`/`Tree` followed with the state slots below. The fields'
  own values came last: `checked(Boolean | Signal[Boolean])` on the four boolean fields and `value(String | Signal[String])`
  on the text fields (`TextArea`, `Password`, `InputMask`, `InputOtp`, `AutoComplete`, `ColorPicker`), and `Input`'s generated
  `value` takes the same `Prop[String]`. Every field's value is a BINDING, the HTML/React `value` (kyo-ui's K4, see
  `UI.Bound`): a constant, a signal or a ref is what the field shows, and kyo-ui puts back an edit its owner does not take. A
  text the reader owns from a start is the field's `defaultValue` (Input, TextArea, Password: `Controlled.startAt`, the slot's
  `start`), the HTML `defaultValue`. A boolean field's constant nobody hears is FIXED, as below. `Input` is an instance where a
  default starts its own ref, which holds the reader's text through a host region that runs again, and a host that states the text itself and draws it in its own region (InputOtp's
  cells, Paginator's jump field, DataTable's filter fields) calls its `renderInPlace`. So a plain field leaves an unbound text to the browser, but a field that draws anything
  from its text after the page is shown (AutoComplete's suggestions, Password's meter and reveal) or builds its value
  from several controls (InputOtp's cells, ColorPicker's sliders) keeps the text itself, unbound and from a constant
  (`Controlled.owningFrom`, which still takes a constant as the start for these; Input, TextArea and Select's `filterQuery` bind it; Password, which the rulebook reached, through a `Controlled` whose `start` the engine binds, with or
  without its meter and eye), and its handlers read the value live, since a keystroke can land before the render of the one
  before it. `ReactiveValueContract.nativeText` and the `startText`/`keptNatively` slot fields hold them to it. A text
  field's `onBlur` reports the text kyo-ui's blur carries (kyo-ui's two-parameter `onBlur((e, text) => ...)`, read off the field as focus leaves it), never
  its binding: unbound or from a default the binding does not have the reader's text at all. `ReactiveValueContract.blurs`
  holds Input, TextArea, InputMask, Password and AutoComplete to it. A blur whose value is not the field's own text keeps
  reading what is held: InputOtp's code (a cell carries one character), ColorPicker's colour, DatePicker's parsed value,
  InputNumber's ref when bound to one. With no ref InputNumber reads the carried text as a number (clamped where it is
  bound to a number), since a commit with no ref writes nowhere and only the field holds what the reader typed.
- **A STATE slot (what the reader changes: a page, a sort, filters, expansion, a selection, a column layout) is a `Controlled[A]`**
  (`Controlled.scala`): what the caller bound, and who hears a reader's change. A setter is one line, `new X(p.copy(xSlot = xSlot.bind(v)))` and
  `new X(p.copy(xSlot = xSlot.hear(f)))`, and a field is never a binding and a callback side by side. It takes `A | Signal[A]` like any slot
  and reports every change the reader asks for: the component goes through `slot.change` (a new value) or `slot.update` (a step
  from the value shown), where a two-way binding is written first, atomically for a step, then the `on<Slot>Change` callback hears
  the new value, whatever the binding (`onPageChange`, `onSortChange`, `onColumnFiltersChange`, `onExpandedChange`, ...; the
  pickers keep their family's `onChange`). A SIGNAL nothing hears is a card (`slot.deaf`): the state lives somewhere else and the
  reader's change reaches nobody. One hearer, not a list: a second `onChange` replaces the first. An event that carries another
  value or fires at another moment (`onRowClick`, `onNodeToggle`, `onItemClick`, a text field's `onChange` beside its `onInput`)
  keeps its own field and calls `slot.heardToo` at its setter, so it counts as hearing the slot. `quieted` is for a host that
  reports the mistake itself, or a stand-in nobody bound. `restore` writes only two-way slots. A host that embeds a component hands
  a slot on as it was bound (`slot.bound`, or the package-internal `withOpen(slot)` where the host's hearer goes along), never as
  a ref it made up. DataTable and the choice types (`SingleChoice`, `MultiChoice`, `SetBinding`, `DatePicker.Binding`) still read
  the `ReactiveValue` functions `Controlled` delegates to, since their hearer's type or their write is not the binding's.
- **Every slot the reader changes: a CONSTANT nothing hears is fixed, and an UNBOUND slot is the component's own**
  (decided 2026-10-28, for every component). A constant nobody hears (`slot.fixed`) is not a mistake but a value meant to stay: the
  component draws it read-only, reachable and readable with nothing to change, the way `readonly` does, and says
  nothing. A slot nobody bound keeps what the reader does in state the component owns for as long as its mount instance
  lives, the way an HTML control nobody binds keeps its tick. There are four forms, smallest first: `slot.owned(start)` inside a mount
  the component already has (beside its ids and highlights: a mount around it would be a mount inside a mount for every instance,
  which `UicTest.mountsAbove` holds), `slot.ownedFrom(unbound)` the same where a constant is only where it starts, `Controlled.owning` a mount
  of its own, and `Controlled.draw` that with the card in front, for a container.
  `Controlled.drawField` is what a field with native state draws from. CheckBox, RadioButton and ToggleSwitch are one instance per place
  (`Controlled.instance`, a `mountedWith`), as ToggleButton is, so an unbound box keeps its tick through a host region that runs again.
  Elsewhere a region around a component that re-renders ends the instance and starts the state over, which a native control under a morph
  would not do; a parent that repaints and has to keep it binds the slot or gives
  the component a key. So a component that draws itself again for `disabled` reads that signal below the mount that holds its state,
  never around it (`SlotCase.disable` holds the fields that do). A host that supplies the held value itself (DataTable's filter selects through `Select.current`) is bound
  and keeps nothing of its own, and so does a paginator a table or a view embeds. A hearer turns `contextMenuRow` on as well
  (decided 2026-10-31): unbound with an `onContextMenuRowChange` the table keeps the row itself and marks it, and unbound with
  nothing hearing the rows leave the browser its own menu.
- **A feature that may be off has a switch, and the switch decides alone** (decided 2026-11-02). `toggleable` on Panel and Fieldset,
  `expandable` on OrganizationChart, `resizableColumns`, `reorderableColumns`, `collapsibleGroups`, `cellSelection`, `rowEditing` and
  `cellEditing` on DataTable each take `Boolean | Signal[Boolean]`, the rule `filterable` set for the pickers: with no switch a binding
  turns the feature on, and set, it alone decides. Own state is minted whenever a switch is present and not constant false
  (`BoolValue.mayTurnOn`, `on` in `Controlled.owning` and `Controlled.draw`), so it survives the switch going off and on. Off over the component's
  own state means not applied, never cleared. Off over a caller's binding depends on what is left: widths, an order, groups and a chart's
  subtrees keep the layout and lose only the affordance, as a fixed constant does; cell selection and editing have no layout to keep and
  read no binding at all; Panel and Fieldset are plain. A signal switch is read inside the mount, never around it. In a DataTable
  `switched` reads every signal switch as one region and `pinned` hands the table constants, so its predicates stay plain `Boolean`s;
  it sits below the mounts, because a mount keyed by the table's id would freeze a tree published around it. `Menu.popup()` and
  `TieredMenu.popup()` are the last setters with no argument: `popup` is both the mode and the open state there until the two are split.
- **A two-valued state has ONE event that carries the value, never a pair** (decided 2026-10-28): `onOpenChange(Boolean)`,
  `onCollapsedChange(Boolean)`, `onActiveChange(Boolean)`, not `onOpen`/`onClose`, since the two are never both. Events
  that carry something else stay (ConfirmDialog's `onAccept`/`onReject` are a choice, and each hears its own path).
  An overlay nothing inside can open (Dialog, Drawer, Toast, ConfirmDialog, a Popover without a trigger) starts OPEN
  when unbound, like an HTML `dialog` with `open`, and shows until the reader closes it; one with its own opener
  (SpeedDial, a Popover with a trigger, the pickers) starts closed.
- **A field with NATIVE state binds it through kyo-ui, which puts back what its owner did not take.** The browser moves a tick,
  a caret's text, a range thumb or a radio before the report reaches the owner, and an owner that declines leaves its binding where it was,
  so no emission would ever reach the control. Every binding of the native element (`value`, `checked`, `selected`: a constant, a signal or
  a ref, see `UI.Bound`) is controlled: after an edit, once the handlers and the regions they woke have run, the kernel reads the binding of
  the element then at that place, compares it with what the event carried and sends the patch when they differ (`indeterminate` goes back
  with a click). A commit always counts, a keystroke only where `onInput` is set; a radio's group gets its selection back with it. Only
  `defaultValue`/`defaultChecked`/`defaultSelected` (or no value) leave the control to the reader. A field whose chrome draws nothing from the value (Input, TextArea, Password without
  feedback) has no region around it, so a keystroke renders nothing; one whose chrome does (CheckBox, RadioButton, ToggleSwitch, Slider, Rating,
  InputNumber, InputOtp, Password with feedback) draws the chrome in a region over the signal and binds the native element to the signal inside
  it. A constant somebody hears is put back like any binding (the boolean fields, Slider, InputNumber's `value`, Rating and the text
  fields alike; `ReactiveValue.native` still hands it on as `Signal.initConst`, which kyo-ui treats the same). A constant nobody hears is fixed. A two-way ref is put back too, where a hearer writes back another value than the edit, and a field that draws everything itself (ToggleButton, ColorPicker,
  Knob, the pickers) has nothing native to fall out of step.
- **A `SignalRef` in a slot is a CAPABILITY demand, not a data hand-over.** It asks every
  caller for write access so that the callers who want write-back can have it. Reading needs
  none (`Signal.render` serves `Const`, `Dyn` and `ReactiveVariable` alike), so check which
  half of the slot actually needs the ref before writing the signature. In `Tabs` only the
  click path did; the union made the ask strictly weaker without changing the two-way case.
- **Widening one of these is not a rename.** In `DataTable` the field went to
  `selectedBinding: Maybe[ReactiveValue[…]]` with a private `selectedRef` deriving the writable
  case, and of the twelve uses only four were writes. The other eight still COMPILED against the
  derived ref and six of them would have been wrong: three read the value (a one-way binding would
  have reported `Set.empty` from `state`, from the render and from `RowContext.selected`) and three
  only ask whether anything is bound at all (`rowKey`'s card must still fire, and the click-clash
  card is about who claims the click, not who may write). Classify each use as read / write /
  bound-at-all before touching it; the compiler checks none of this.
- **Say what a one-way binding cannot do, in a card.** A slot that paints but never writes looks
  exactly like a working one until someone clicks it. `DataTable` reports the two shapes that have
  no outlet: a `Checkbox` column, whose boxes are not row clicks and so cannot fall back to
  `onRowClick`, and a `Single`/`Multiple` selection with no `onRowClick` bound at all.

## Diagnostics: what a component says on the page

A mistake a type cannot catch and nothing crashes on is said in a card (`Diagnostics.scala`, `.p-uic-diagnostic`). A `Diagnostic` is one
thing that may hold: `Diagnostic(when, advice, keys)`, `Diagnostic.naming(advice, keys)` that fires when there is something to name, and
`Diagnostic.unheard(slot, name, on)` for a slot nobody hears. `Controlled.Name` holds the one template of that last sentence, in the component's
companion, so a rename is one string.

- **One card per slot, and where it goes follows the component.** A container draws it in front (`Diagnostics.before`, `Controlled.draw`). A
  field draws it behind, with the message row between (`Diagnostics.after`). A table or a list draws it inside its root, as children
  (`Diagnostics.cards`), and a card that reads what a region reads is built in that region. With nothing to say `before` and `after` are
  the very `ui`, no fragment, so the bytes stay.
- **`on` says the reader can act at all.** A disabled or readonly control has nothing to hear: pass `on = interactive`. Under a signal
  switch the card comes and goes in a region of its own (`Diagnostics.cardsWhile`, `beforeWhile`).
- **A `Diagnostic`'s arguments are evaluated whether it fires or not.** A key that is costly to work out stays behind a plain `if` around
  `Diagnostics.cards`.

## Shared slots live in traits, not in every component

A slot several components carry is defined ONCE, in `FormControl.scala` (`HasValidity`: `invalid`,
`invalidMessage`) or `Capabilities.scala` (`HasAccessibleName`, `HasAccessibleNameRef`,
`HasAccessibleDescription`, `HasPlaceholder`, `HasTooltip`, `HasEmptyContent`). The trait carries
the setter and its scaladoc; a component supplies only the one-line `withX` writer. The slots that form one group
(validity, accessible name and id) have one reader and one writer for the whole group, see "A component is a class over its state".

- **A control a form binds is a `FormControl[A]` over the value type of its field** (`String`, `Boolean`, `Double`,
  `Maybe[A]`, `Set[A]`, `Seq[UI.FilePayload]`), never a trait per value type and never a marker: `bind(field: FormField[A])`
  takes exactly that field, so the value type alone picks the binding and a field of another type is reported against the
  field the control binds (`FormControlTest`). The control writes `bindRef(ref)` and `hearBlur(f)`, one line each over its own
  setters, generated (`this.value(ref)`) or written. A setter only some controls of a value type have, the binding asks for
  through `HasSetter` (a choice's `selection` over the option type, the number binding's `integer`), with an instance in the
  companion of a component that writes it by hand. A clause with a narrower receiver (`FormControl[Maybe[A]]`, which binds a
  field that always holds an option too) has ONE `bind` over the field's type, its field picked by evidence (`ChoiceField`):
  Scala picks the narrower receiver before it reads the field only while one alternative there takes it.
- **Adding a slot to a component that already has a trait for it means mixing the trait in**, not
  writing another setter. A second spelling of an existing slot is the drift these traits exist to
  stop.
- **A trait requires the storage to be genuinely uniform.** `disabled` is NOT one of these: 14
  components store `Maybe[BoolValue]` and honour a reactive value, 19 store a plain `Boolean` with
  no reactive path in their render. `severity` is not either: its storage disagrees on whether the
  slot is optional. Check the storage before lifting; forcing a trait onto the second group is a
  behaviour change per component, not a refactor.
- **The split above is not an argument for widening any given `disabled`.** Ask what the flag
  DESCRIBES. Almost every one describes the state of the page, and a plain `Boolean` is right.
  `MenuItem.disabled` is the exception because it describes the item's TARGET, which changes with
  each open, and rebuilding the list per target is not the alternative, since the host allocates
  its open state in its own unkeyed `UI.mounted`.

## A slot on a shared MODEL type is not a per-component decision

`MenuItem` is one type rendered by six hosts: Menu (which SplitButton delegates to), Menubar,
MegaMenu, TieredMenu, ContextMenu, SpeedDial. Widening a slot there means every host has to
honour it, or the same call means different things in different places with nothing in the type
to say so. Contrast `HasElementId`, where each component genuinely was its own decision.

- **A reactive slot on a model type is resolved by the HOST, before it builds rows.**
  `MenuRender.readDisabled` folds every reactive slot in the tree into ONE subscription chain
  and hands back items pinned to `Const`, so `MenuNav`, the navigable index set and the row
  builders all stay pure functions of a plain `Boolean` read through `constTrue`. This is
  `BoolValue.reactive` lifted from a value to a list.
- **Resolve INSIDE the mount, never around it.** These hosts keep their open state in their own
  `UI.mounted`; a reactive region placed around `render` rebuilds the mount on every emission and
  takes that state with it. The seam is `wired`, which is already below the mount boundary.
- **A forgotten host fails silently**: `constTrue` on an unresolved `Dyn` reads `false`. The
  sweep in `GoldenRenderTest` ("every host of MenuItem resolves a reactive disabled") is what
  makes that impossible to ship; a host added later belongs in that list.
- **A slot only some implementors have gets its own trait** (`HasAccessibleNameRef` extends
  `HasAccessibleName` for the 21 of 38 that carry the reference), never an abstract member the
  others cannot answer.

## A list setter takes `AEntry*` (an `Entry[A, AScope]`)

Every setter that takes a list of model entries (`items`, `nodes`, `panels`, `tabs`, `optionGroups`,
`column`) is `def x(es: AEntry*)`, where `type AEntry = Entry[A, AScope]` sits next to the scope, so
callers write `_.item("Save")` or `uic.MenuItem("Save")`, mixed, and still splat a prepared `Seq`.
Rules a new one follows:

- **One setter over the union, never an overload** beside the plain `A*`: with two alternatives the
  lambda is typed without an expected type and `_` has nothing to complete against.
- **The scope mirrors the entry's own constructors** (`XScope.item` = `X.apply`), and offers a reactive
  label where the component's single-entry builder does (`Tabs.tab`, `Accordion.panel`). It offers
  nothing else: a builder from another scope must not compile (`EntriesTest`).
- **Entries that nest take entries too** (`TreeNodeScope.node(children = Seq(_.node(...)))`), so a whole
  tree reads one way.
- Not for enum-valued lists (`Paginator.template`, `FileUpload.accept`): there is nothing to build.
- **A list that changes is a signal among the entries** (`LiveEntry[A, S] = Entry[A, S] | Signal[Seq[A]]`,
  `items(_.item("All"), held)`), never a component built again inside a region over the list: that starts the
  component's mount again with every change, and a click landing while it starts reaches nothing. The host
  keeps the entries (`entriesV`) and resolves them in its one region (`Entry.reading`, for menus
  `MenuRender.readItems`). A setter takes `LiveEntry` only once its host reads signals, or a signal would
  compile and be ignored; `GoldenRenderTest` ("every host of MenuItem draws its rows from a signal ...") holds
  the menu hosts to it.
- **A picker's options take the same union** (`options(Iterable[A] | Signal[Seq[A]])(label)`,
  `optionGroups(... | Signal[Seq[OptionItem[A]]])`), kept as `OptionSource`s and read with `OptionItem.reading`
  in the picker's one region; a picker built on `Entry` (Listbox, CascadeSelect, TreeSelect's `nodes`) takes
  `LiveEntry` like a menu. `GoldenRenderTest` ("every picker draws its options from a signal ...") holds them.
  Ids that follow the options derive from ONE minted base (`$base-<n>`), never one mint per option, or a
  list that grows has rows without ids.
- **Structural components take it too** (Tabs, Accordion, Breadcrumb, Galleria, Tree, TreeTable, Stepper
  through `LiveEntry`; Carousel and Timeline through `Iterable[A] | Signal[Seq[A]]`, read with `ReactiveValue.concat`). Each
  exposes its list as `entries` and keeps the resolved copy (`tabList`, `nodeList`) only for what a render
  hands its body; a host that builds one internally hands it resolved entries (`Tree.rooted`), or its methods
  see an empty list. Rows that carry content the reader works in (tab and accordion panels) are a list keyed
  by their id (`foreachKeyed`), so a change to the list builds only the rows that are new, and state that a
  row shows per selection is a reactive class or attribute on the row, never a region over it. All rows share
  one region (`ReactiveValue.children`), so Prime's sibling rules (`:first-child`, `:nth-child`, `~`) still see one
  run. `GoldenRenderTest` ("every structural component draws its entries from a signal ...") holds them.
- **Which submenus are open is ONE set of paths** (`Submenus`), not a ref per path: refs could only be
  allocated for the items the host was built with. A submenu panel is `Overlay.gated(subs.close(path))`.
- **Positional menu state belongs to its item** (`Anchored`): the open paths, a focus path and Menu's
  highlighted row are stored with the RAW root items they were written against, and each render translates
  them onto the roots it shows (`Anchored.index`: the same item by reference, else an equal one, else the same
  place when the list kept its length and the item there is new, as a label with a count is), so an item a signal
  adds before an open submenu leaves it open, one it removes takes its state along, and one it emits again as a new
  value keeps it. The raw roots come from `MenuRender.readRooted` (MegaMenu:
  `MegaMenuItemScope.rooted`), never from the pinned rows, which resolving a reactive `disabled` copies. A host
  writes through the `View` it builds in its one region (`subs.view(snap, roots)`, `focus.view(snap, roots)`);
  `reset`, `closeAll` and `openUnanchored` write with no roots, for values the same against any roots or a test.
  `MenubarTest`, `TieredMenuTest`, `MegaMenuTest`, `MenuTest`, `ContextMenuTest` and `CascadeSelectTest` hold it.

## A choice binds the option, never a key

What a reader chooses binds as the app's own type: `selection(A | Signal[A])` where the field always holds an
option, `selection(Maybe[A] | Signal[Maybe[A]])` where it may stay empty, `selection(Set[A] | Signal[Set[A]])` for several.
There is no public setter that takes the key a component compares by. Rules:

- **Keys stay inside.** `optionKey` is how options are matched (panel, keyboard, hidden `<input name>`), and
  `SingleChoice` / `MultiChoice` are the one place a typed ref is read as a key and written back as the
  option it names. Matching never uses `A`'s own equality.
- **Events carry the value**, not the key: `onChange(Maybe[A] => ...)` / `onChange(Set[A] => ...)`, fired only
  when the write changed what is held (a pick of the row already chosen fires nothing). `onBlur` the same.
- **A single choice also reports `onPick(A => ...)`**: the same moments as `onChange` minus a clear, fired after it.
  Both go through `SingleChoice.report`, so the order is one fact, not three. A field never empty reads its picks
  there instead of writing `_.fold(Kyo.unit)(...)`.
- **A single choice binds one way too.** `SingleChoice` holds a `ReactiveValue`: a `ReactiveVariable` is written, a
  `Dyn` or `Const` is only shown, and a pick against it is judged against the value shown and reported, never written,
  so the app writes its own state (a route). Forms are unaffected: `bind` passes the field's ref, which is two-way.
- **A value no option carries is a data error**, named in a `.p-uic-diagnostic` card
  (`Diagnostics.unknown`) and shown in the trigger from the value itself, unless `loading` says the
  options are still arriving.
- **Forms bind by the option type**: a single choice is a `FormControl[Maybe[A]]` with a `selection` over `A`, a multiple one a
  `FormControl[Set[A]]`, so `bind` takes a `FormField[A]` or `FormField[Maybe[A]]` (`form.ChoiceField` says which) or a
  `FormField[Set[A]]` (whatever its rules refine to).
- **An internal host** that renders inside its own subscription (the DataTable filter menu) passes the held
  option with `current(Maybe[A])` and reads `onChange`; it never reaches for keys.
- **A model carrying its own id takes the id's type**: `Listbox[K]` / `ListItem[K]`, `Tree[K]` / `TreeNode[K]`,
  `TreeSelect[K]`, `OrganizationChart[K]`, `Accordion[K]`, `Tabs[K]`, `Stepper[K]`. There `K` IS the identity,
  so sets compare by `K`'s `equals`, and a `private given CanEqual[K, K] = CanEqual.derived` says so where a
  signal needs it. Ids reach the DOM only through `toString` (derived header ids, a hidden input).
- **Tables hold rows**: `DataTable[A]` / `TreeTable[A]` bind `Set[A]` / `Maybe[A]` / `CellPath[A]` and match them
  by `rowKey`; internals stay on keys (`CellKey`), and one block of helpers (`rowsFor`, `cellsFor`, `rowFor`,
  `updateRows`) is the crossing. `TableState` alone keeps keys, because it is what gets serialized;
  `restore(s, byKey)` resolves them back.

## A date binds the value type, and the type is the mode

`DatePicker[A]` holds what its `DateKind[A]` says (`LocalDate`, `LocalDateTime`, `LocalTime`, `YearMonth`,
`Year`, or `DateKind.zoned` for a `ZonedDateTime`), and the kind decides the grid the picker opens on and
picks in. There is no `view`, `showTime` or `timeOnly` setter, because each would be a second way to say
what the type already says. Rules:

- **A setter some kinds cannot honour asks for evidence**, never a silent no-op: `values`/`range` need
  `DateKind.Days`, `hourFormat` `Clock`, `showWeek` `Weeks`, `currentView` `Drill`, `month` and
  `referenceDate` `Calendar`. The markers are flat (no subtyping between them), so one given never
  satisfies two of them ambiguously. `DatePickerTest` holds each refusal.
- **Inside, everything is ISO text at the kind's precision**, because the grid math is integer arithmetic
  over it. `DatePicker.Binding` is the one crossing: it reads the typed bindings (a constant, a one-way
  signal or a two-way ref, as every value slot takes them) as a `Snapshot` of ISO strings and writes back
  values parsed from them into a two-way one, comparing by the ISO image the way `SingleChoice` compares by
  key; a one-way binding is only reported. A write naming the value already held writes nothing and reports
  nothing, which is what keeps a zoned value's instant through a pick of its own day.
- **Bounds compare ISO strings of one kind**, and a time kind's bound reaches the time picker: a step that
  would leave the bounds renders disabled, and a day pick on a bound's own day is clamped to it.
- **Typed text is parsed, never bound.** The field shows the held value; a `change` that does not name a
  value within the bounds writes nothing and parks the text in the mount's `draft` ref, which the blur
  clears so the field shows the held value again. A const `value` alone cannot do that: re-rendering the
  same string patches nothing, and the reader's text would stay on screen beside a value it never became.
- **A set or a range is a type of its own** (`MultiDatePicker[A]` from `values`, `RangeDatePicker[A]` from
  `range`), the `SelectButton.multiple` split: each has exactly one `onChange` and one `onBlur` over its own
  value, so no callback can be set that never fires. The conversion drops the single-value binding and its
  callbacks; the wrappers delegate every shared setter to the `DatePicker` they hold and render through it,
  so no logic is duplicated. One type with three callback names, or with arity-one overloads, was the
  alternative: the first leaves dead callbacks possible, the second breaks lambda inference.
- **Only `DatePicker[A]` binds a form field** (a `FormControl[Maybe[A]]`). The wrappers carry the validation vocabulary
  (`HasValidity`) but no form binding: a set binds through `selection` (`FormControl[Set[A]]`), which would be a second
  name for the set `values` already bound, and no field holds a range's two refs.

## Form rules refine, and submit hands over what they yield

`Rule[-A, +B]` (opaque over `Rule.Run`, which runs an `A` under a `Rule.Ctx` to a
`Result.Partial[FieldError | S, B] < Async`; `Rule(f)` still takes a plain `A => Result.Partial[FieldError, B] < Async`)
checks an `A` and yields a `B`;
`Validator[A]` is the alias `Rule[A, A]` for a rule that only checks. `and` composes left to right with
the ordered short-circuit (`Rule[A, B] and Rule[B, C]: Rule[A, C]`), `Validator.chosen` refines
`Maybe[A]` to `A`, `Validator.ifPresent(r)` lifts a rule into `Maybe`, `Validator.refine` is the pure
"parse or fail with a code". Rules:

- **The field knows its type through a type member, not a type parameter.** `FormField[A]`,
  `NumberField[A]`, `FieldSpec[A]` and `NumberFieldSpec[A]` carry `type Valid`; `rules[B]` returns a
  spec refined to `Valid = B` and `declare` a `FormField[A] { type Valid = B }`. Every signature over
  `FormField[A]` (all the `bind` extensions) keeps working because a refinement is a subtype. The
  classes are sealed abstract with one anonymous implementation each (`FormField.declared`,
  `FieldSpec.of`), so the refinement is stated once. A setter that keeps the rules returns
  `X[A] { type Valid = X.this.Valid }`, or the refinement is lost after the first chained call.
  `addRule`/`satisfy`/`matches` hand that type back as a computation (`... < Sync`), not `this.type`:
  appending may start a subscription, and kyo cannot lift a value typed `this.type` into one. Sequence
  them (`_ <- f.addRule(r)`); `_ = f.addRule(r)` compiles and drops the rule.
- **`submit(field)(v => ...)` takes `Validated`** (the common supertrait of `FormField` and
  `NumberField`) with dependent method types, up to four fields. The named fields are validated by the
  submit itself (`verdict`, one run that both decides validity and yields the value) and skipped by the
  subtree walk, whose results get the named verdicts put back in declaration order for
  focus-first-invalid. Never re-read `value` in a handler for something a rule refined.
- **`addRule` appends a CHECK on the raw value and keeps the declared chain's output** (`andCheck`), so
  a post-hoc rule cannot change `Valid`. It takes `Rule[A, Any]`, which also restores the contravariant
  use the old `Validator[-A]` allowed (a check over a supertype).
- **Text belongs to the rule**: `rule.message(text)` sets `fallback`, and `FieldError.message(text)` (code
  `FieldError.messageCode`) backs `raise(text)` and `setError(text)`. Every translator answers in the
  order table entry, then `fallback`, then code: the translator speaks first so an i18n translator can
  replace every string. A field that needs its own words under an app-wide table gets its own code.
- **A hand-built `FieldArray` has no typed submit**: its row model is the app's own type, so it has no
  single `Valid` the form could name. A typed array is a group member (`form.array`, below).
- **When a rule runs is the rule's**: `rule.on(...)` beats the field's `on`, which beats the rule's own
  default (`Blur + Change`; `Blur` for `Validator.async`). Nested, the INNER `on` wins: an outer
  `(a and b).on(x)` only fills rules without their own. Every leaf gets a `Rule.Key` when it is built and
  carries its default; `Rule.leaf` is the one place a rule decides, through the `Rule.Ctx` every
  combinator passes on unchanged (a new combinator passes `ctx` too, or the rules under it lose their
  `on`). `Validator.none` is the exception: nothing to check, so it always runs.
- **Not due is not "passed"**: a rule not due for the event reuses its last verdict while its input is
  the one it judged (the field's `Rule.Store`, one per field), and is otherwise NOT YET CHECKED: the chain
  stops, the field publishes `ClientVerdict.NotChecked` (no error shown, never a `FieldError`), and
  the ungated `Form.isValid()` reads `false` through `invalidAgg` (an error OR not yet checked, per
  field `invalid`), which folds child scopes and array rows too. `isValid(onlyVisible = true)` and `submitDisabled` do not count it: they gate on what
  is shown, and a deferred rule must not lock the button that would run it. Submit (`verdict`), a
  DataTable cell commit (`run`) and a change of a `dependsOn` signal (`revalidateAll`, through
  `Rule.Sweep.Every`) run every rule: the field's own value did not move, so a not-due rule would keep the
  verdict it found against the dependency's old value. `dependsOn` is the general fallback; a rule that
  reads another field is a `reading` rule (below). `revalidate` (the mount value, a `dependsOn` signal's
  value at subscription, the value a reset leaves) is a `Change`.
- **A rule that reads another field carries the dependency** (`Validator.reading` / `Rule.reading`, over a
  `Dep`). The leaf reads every dependency once per run (`Rule.Source.current`), hands the values to its
  function and keeps its stored verdict for the value AND what it read (`Store.Entry.holds`), so "reuse while the input is the one it judged"
  covers dependencies with no special case. A change of one is a third kind of event
  (`Rule.Ctx.of` with the signal as trigger): a leaf reading it with `Dep.recheck` is due when the value of
  that signal it reads now differs from the one it last judged, unless its store already holds a verdict for
  exactly its current input (then it reuses it, so a change it has judged sends no second request), every
  other leaf is not due, so `Dep.discard` is just "not due with a different input", and
  a dependency changed back reuses the old verdict. `on` governs only the field's own events, the
  reaction only the dependency's; neither overrides the other. `Run.reads` is abstract, so every combinator states the signals of the rules under it (or a
  field goes deaf to them); the field subscribes one observer per distinct signal (by identity), never
  to its own `valueRef` (its edits are its own events; a rule gets its value as the first argument and
  must not read the field's own signal, which for a `NumberField`'s mapped `value` identity cannot catch).
  `RuleChain` holds the rules and the wiring in ONE `AtomicRef`, so the set of observed signals and the
  chain cannot disagree: the wiring (BEFORE the change observer starts, so before the mount's run, for
  static fields and rows alike) claims what the chain reads then, and an `addRule` after it claims what the new rule
  reads that is not observed yet, whichever comes first, with no signal lost or observed twice. The
  observers belong to the chain and end with `unwire` (the mount scope's `Scope.ensure`, a row's
  `cancelRowChangeActivations`), which puts the chain back to `Unwired`; `keep` interrupts fibers that
  arrive after the end. Appending runs
  nothing. Every value an observer delivers, the one at subscription included, is a dependency event,
  and the delivered value takes no part in it: each leaf decides against what it reads NOW and what it
  stored (`Store.Entry.read`, what it read when it last judged). Never decide at the field: a field-level
  "last read" is updated by the field's own runs too, so a keystroke that read the new value before the
  observer delivered it swallowed the change for every rule the keystroke was not due for. A leaf that has
  judged nothing compares with what the signal held at subscription (`Store.subscribe`, noted synchronously
  before the observer starts, so a change right after an `addRule` is not lost; `Store.baseline`). Since the
  subscription comes before the mount's run, there is no gap for a dependency to move in unheard, a normal
  mount runs nothing extra, a late or stale delivery runs nothing, and a rule whose own value moved while
  the dependency did not waits for its own event.
  `matches` is `addRule(Validator.matchesField(other))` and `matchesField` is a `reading` rule. Unlike a
  `dependsOn` change, a dependency change does not rerun the rules that do not read it.
- **`validating` and the debounce belong to a run that runs a rule.** Every trigger takes an epoch and
  runs the chain once. Before each leaf that is due, the run asks its `due` action (`startDue`, passed to
  `Rule.Ctx.of`) whether it still owns the field. The first time, it claims the field as its latest due run,
  shows `validating` and waits out the debounce; after the wait, and before every later leaf, it owns the
  field only while no later due run (keystroke, submit, reset) has claimed it. A run that lost the field runs
  no rule (the leaf answers `NotChecked`, unrecorded) and publishes nothing, so typing faster than the
  debounce asks the rules ONCE, about the last value, and a submit during a blur's debounce stops that run's
  request for the old value. A run no leaf ran for (recalled verdicts, or not yet checked) never claims, so it
  publishes at once with no `validating` and no debounce, and it never takes the field from a due run: a due
  run that finds the epoch moved only by such runs (`dueState` still its own) does not publish its own
  verdict, which may be about a value the field no longer holds: it settles (`Rule.Sweep.Settle`, every
  signal counts as changed) on the current value and ends `validating`. So a blur during a change's debounce
  cannot strand the field not yet checked, and a keystroke during a blur's request cannot let the old value's
  verdict through.
- **`validating` is derived from `dueState`, never set by a run on its own say.** `dueState` (one
  `AtomicLong`) packs the latest due run's epoch with whether that run shows `validating`. A claim is one
  update of it (an event run: shown; submit and reset, `claimQuietly`: not shown), a published end is one
  compare-and-set (shown to not shown, only while the run still owns the field), and whoever changed it then
  calls `syncValidating`, which writes what the state says and writes again if the state moved meanwhile.
  So the last write agrees with the last state whatever the interleaving: a run paused between its claim and
  its write, overtaken by a run that started and ended in between, cannot leave `validating` true.
  `FormField.afterClaim` (a `Local`, a no-op outside tests) is the seam a test pauses a run at there.
- **A run that reads the value itself takes its epoch FIRST** (`onCurrent`: the mount, a reset, a
  dependency, `dependsOn`, a settle, the bound blur; `verdict` the same way, epoch and quiet claim before
  its one read). These run on fibers beside the change observer's, and read-then-epoch let an edit land
  between the two: the edit took the earlier epoch, and the run on the old value, now the last, published its
  verdict over the edit's (a submit even took the field from the edit, so the edit ran nothing). Epoch-then-read
  makes every later write a later event. `FormField.startingRun` is the test seam between the two steps.
  `bind`'s blur therefore calls `blurred`, which ignores the value the control hands its blur (the field's ref
  holds the same text, written by every input event before the blur). `onEvent(v, event)` judges the value it is given: its one production
  caller is the change observer, whose next emission handles any write after its read; tests use it too.
- **A reset is judged like a mount.** `resetField` claims the field quietly, clears the store, touched,
  the named gate, the client verdict and the server error, writes the baseline if the value differs, and
  starts ONE change run (`revalidate`) on its own fiber, whether or not the value moved. The change
  observer passes over the emission of the value the reset wrote (`resetWrite`), so a moved reset runs once,
  not twice, and is not taken for an edit. Without that run an unmoved reset read valid while a moved one
  read not yet checked (a rule not due on a change finds the cleared store), two answers for one state.
- **The field wires both events whatever it declares**: the change observer and `onBlur` are always on,
  because a rule may ask for an event its field does not. `touched` stays bound to the field's OWN events
  (`touchedBy`: its `on`, else `Blur + Change`), so what makes a message visible did not move.

## A field group is a named tuple, and nothing else

`form.fields((name = spec, ...))` returns `NamedTuple[N, H]` of the declared handles, `form.array` and
`form.child` nest one, and `form.submit(group)(v => ...)` hands over `NamedTuple[N, V]` of the validated
values (`form/FieldGroup.scala`, `FieldGroupTest`). Rules:

- **A group has no members, so no field name is taken.** Everything that acts on one is a function over
  it: `form.submit(f)`, `Form.scopeOf(f.shipping)`, and the `FieldArray.Row` that `GroupArray.render`
  hands over beside each row. `GroupArray` carries the array's operations and no field names; a row
  carries field names and no operations. Do not add a method to a group, a row or a child, and do not
  wrap one in a class to hang one on.
- **Why not a `Selectable` class**: its members shadow fields, and so does ANY extension method in
  scope (checked on 3.9.0: an `extension [A](a: A) def submit` beat the field). A named-tuple selection
  beats extensions. What it cannot beat is `Any`'s members, and the compiler refuses to select `apply`;
  those are `FieldGroup.Reserved`, refused at the declaration through `FreeName`. Add to that list only
  what an experiment shows the named tuple answers itself.
- **Type classes, not match types.** A match type cannot bind a field's `Valid` refinement (`case
  FieldSpec[a] { type Valid = v }` leaves `v` unbound; the alias workaround is refused by SIP-56). Take
  the element and the rest as `Aux` with TYPE PARAMETERS, never as paths (`m.Handle`), or every message
  shows skolems (`?1.Out`). `Specs` maps specs to handles, `Shape` handles to values, so a submit needs
  the group and nothing else.
- **A group's members are declared in the group's scope** (`declareIn`), whichever form the spec came
  from. That is what makes `Form.scopeOf` answerable from the group alone: it is the scope its first
  member was declared in, and a child member lives in its scope's `outer`. Groups are non-empty
  (`S <: NonEmptyTuple`) so there always is a first member.
- **The submit is `submitWith` with every leaf named**: the array rows are read once when the submit
  starts, each field's rules run once (W32's single-run verdict), named gates open, focus-first-invalid
  is unchanged. Never validate a group member a second time in the walk.
- **`Specs`' `@implicitNotFound` hides `FreeName`'s**, so a reserved name reports the group's message,
  which lists the reserved names, followed by the compiler's own `NotGiven[("toString" : String) <:<
  ...Reserved]` line. `FieldGroupTest` pins that and the other messages.

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

`UI.commands` (read from a `Local` the runner installs per session) is the escape
hatch for the two things a render diff cannot express: imperative DOM commands
(`focusId`, `scrollIntoViewId`) and measuring an element (`requestMeasureById*`). Load-bearing rules:

- **Every component mount that does not wait is the `Sync` form of `UI.mounted`** (decided 2026-11-05): it allocates and draws in the
  render pass, so the first paint, a static page included, shows the real component, and a reader's first click or key works. It has
  no placeholder and no static copy of itself: the body draws once. `UI.id` gives the ids, `UI.whileMounted` runs what needs the live
  page (an observer, a feed, a bound style), and a handler reaches commands through `UI.commands` or `focusById`. An effect typed
  wider than it is counts as waiting, silently, with a placeholder window, so a helper stays `Sync` where it is. Only `form.Form` waits,
  since its builder may.
- **An effect that belongs to an element goes on the element**: `el.whileMounted(effect)`. A region that runs again has
  new content, so without a key the effect starts again with each run of its region, as an unkeyed mount does; what has to
  survive says so with `el.whileMountedKeyed(key)(effect)` (Overlay's panel and DataTable's header by their id, DataTable's
  source by itself). The element's id is no key, and no position among siblings is part of a key. No mount with empty
  content exists for this in uic. Its `Scope` ends with its instance, and a failure reaches the `onFailure` rules around the element.
- **A component that draws through `UI.mountedWith` is keyed on where the app places it** (`Node.placed`, behind both conversions
  and `toUI`), so a host region that runs again keeps its instance and hands it the new props through its `Live`; placements in one
  `map` are told apart by their order. A plain `UI.mounted` render stays unkeyed, since a held draw would keep the first run's values.
  `NodePlacementTest` holds it.
- **A component's mount that must survive its host's region running again is keyed on where the component was made**: Overlay
  takes `(using UI.MountSite)` at construction and keys the mounts that mint its panel's ids on it with `.keyedOnSite(site)`.
  Calls of one site in one range (one call in a `map`) are told apart by their order, so the caller writes no
  key; a key given with `.keyed` that repeats in one range is reported as a duplicate. An effect inside a real mount's draw that reads
  that mount's state stays `UI.whileMounted` (Carousel, VirtualScroller, DataTable's demand writes).
- **Commands resolve ONLY in `el.whileMounted`, `UI.whileMounted`, a waiting `UI.mounted` effect or an event handler**,
  never in a draw a static page also runs: outside a session `UI.commands` panics.
- **A reusable component has no way to learn its own render path.** Every command
  is id-addressed: in a `UI.mounted` effect, `id <- UI.id`, stamp it
  with `.id(id)` on your own element, then drive `focusId(id)` /
  `scrollIntoViewId(id)` / `requestMeasureById(id)`. `UI.id` derives from a keyed node's identity
  and from an unkeyed node's place, so a static page and the live one agree; an id only a running session can mint would make them differ.
- **Handlers take `UI.Action` (`Any < (Async & Abort[Any])`)**, on `uic.Button` and on a raw kyo element alike.
  A failure a handler leaves unhandled goes to the `onFailure` rules around it.
  A setter that takes an app's callback stores it as a `Callback` (`Callback.of(f)`,
  `Callback(action)`): the callback's failure goes to those rules right where it ran,
  and the component's own work after it (closing a panel, ending an edit, clearing a
  busy flag) still happens. A new setter does the same.
- **A command from a mount waits for that mount's content; one from a handler does not.**
  kyo-ui holds a command sent from `UI.whileMounted`, a waiting effect or a fiber it forked until
  the instance's own content is on the page, behind whatever boundary holds it back,
  and drops it (a measure then answers `Absent`) if the content never shows. So a
  mount may focus the element it draws, and a fiber it forked may measure it. A
  measure a waiting effect awaits BEFORE it yields goes out at once (holding it would hold
  the effect forever), so it can only see what is already on the page. A HANDLER's command goes out at
  once, and the client resolves `getElementById` immediately (no rAF, no retry): a
  handler that addresses an element its own write is about to insert can miss it.
  - Terminal auto-scroll: the newest row carries an `el.whileMounted` that sends
    `scrollIntoViewId` for the prompt, not a scroll from the append handler (that races the
    re-render; `scrollTop` stays 0 even though the container is scrollable). `UI.scrollIntoView` scrolls
    to the block's start and moves the page around the component too, so a component scrolling inside
    itself uses `scrollIntoViewId`, which goes to the nearest edge. See `Terminal.scala`.
  - OTP auto-advance advances from the input handler (the target cell already
    exists from the prior render), so no nested mount is needed there.
- **An effect never touches the DOM itself.** A draw runs before its content is on the page, and on a static page there is no DOM at all.
  Code that needs the real element uses `onAttached`, which runs once the element is in
  the document; a nested mount is not a substitute and broke the demo's editor.

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
which shows a `Sync` `UI.mounted` as what it draws, so a golden of `render` is the anatomy the reader meets. A component
still has a `private[uic] def wired(...)` where a test has to hand it state the draw would otherwise allocate (a ref to write, a
panel held open).

- **Keep the wired seam Commands-free** so `kyo.uic.test` can drive it: pass id
  lists plus a plain `String => UI.Action` scroll/focus function, not a
  `UI.Commands` (`Commands.init` is `private[kyo]`, unreachable from the test).
  Production wires `focusById`; the test wires `_ => ()`. See
  `InputOtp.wired`, `Terminal.wired`, `Carousel.wired`.
- `private[uic]` members (including `wired` and pure helpers like
  `Knob.valueFromPointer`, `Overlay.flipAnchor`) ARE reachable from `kyo.uic.test`,
  because package-private includes subpackages. Unit-test pure geometry directly there.
- Discoverability tests (`DiscoverabilityTest`, all four platforms) gate each setter by
  concrete return type: inapplicable options must not compile. When you change a
  setter's signature (e.g. FileUpload's file event from String to `Seq[FilePayload]`),
  update the discoverability property that pinned the old shape.

## Overlay and consumers

- `Overlay(open)(...).render` (the standard path) gets `autoFlip` (default true):
  on open it self-measures and flips Bottom to Top, or shifts horizontally. The
  host-gated `renderOpen` (menu-family nested submenus) keeps the declared anchor,
  because nesting a mount into an already-subscribed region would double-subscribe
  and duplicate the panel DOM (one subscription per ref chain; `renderOpen` is the
  single-subscription form). `renderShown` is the host-gated form WITH the full placement
  (autoFlip, portal, pointer anchor), for a host whose one region already reads the open state
  (the pickers, a popup menu, DatePicker); see "A control that reads several refs is ONE region".
- **A host-gated panel takes the host's id** (`renderShown(panelId)`, `renderGated(open, panelId)`,
  `s"$base-panel"`). The host redraws the panel with every change it reads, so a mount around the panel
  would start again on every arrow key, and while a mount restarts kyo-ui shows an inert snapshot that no
  click or key reaches. With the id the panel is drawn directly and only the measuring triggers are mounts,
  keyed by it, publishing nothing. `Absent` is for a static projection only. `OverlayTest` ("a host
  repainting around an open panel ...") holds it.
- `autoFlip` only flips DECISIVELY where nothing inside the panel is focus-seeded.
  kyo's focus-seed calls `element.focus()` without `preventScroll`, so a seeded
  element gets scrolled into view by the browser before the measure lands: the
  panel stays on-screen but does not visibly flip. That is now the menu family
  (whose LIST is seeded) and a Select/MultiSelect with its filter on (whose filter input
  is); the option panels themselves seed nothing. Verify flip on one of those
  near the viewport bottom.

- **Focus is a single-owner property, and `aria-activedescendant` follows it.** The
  attribute is read off the element with DOM focus or off nothing at all, so it
  belongs on the one element that holds focus while the popup is open: the trigger
  for Select/MultiSelect/CascadeSelect/TreeSelect, the input for AutoComplete and
  for a Select/MultiSelect with its filter on, the LIST for the popup menus. A panel that
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

**An announcement that waits for a caller's id is silence.** `aria-activedescendant` is the whole
of what a screen reader hears about a highlight, and every menu-family component used to render it
only when the page had called `id(...)`. Almost no page does, because nothing outside points at a
menu, so the highlight moved and nothing was said. The mount mints the base id the way it mints
every other id it needs, the caller's own still wins, and the `wired` seam takes the base so the
golden renders the shape the reader actually meets.

The same Tab rule holds where the popup's rows ARE real controls and hold real focus
(`SpeedDial`, whose actions are Buttons): the trigger is the widget's one tab stop, no row is in
the tab sequence, focus enters by opening and leaves by Tab, which closes. Giving the active row a
tab stop as well makes the open widget two of them, and Tab then walks the reader between the
trigger and its own popup instead of past the widget. Nothing is lost by it, because the way back
into an open popup is the same key that opened it.

## Nothing follows a highlight but the flag you put on it

A moving DOM focus is scrolled into view by the browser. A highlight is not focus: it is a class on a
row plus `aria-activedescendant` on a container that does not move, so in a panel capped at 14rem the
mark walks out of the panel and in a long page list off the screen, and the reader is steering
something they cannot see. Every host that moves a highlight marks the highlighted row with
`scrollAuto(true)` (kyo-ui), and the client scrolls it into view with `block: "nearest"` whenever the
flag lands on a different element than before.

- **A command cannot do this job on its own.** `cmds.scrollIntoViewId` works while the row is already
  on the page, and the moment that matters most is the other one: a panel opening onto the selected
  option scrolls to a row that does not exist when the handler runs (the DatePicker lesson). A flag
  on the row is read by the patch that inserts it.
- **`nearest` is not a detail.** It moves the scroller by the least it can and not at all when the row
  is already visible, so a highlight walking within view never yanks the panel under the reader.
- `GoldenRenderTest`'s "a highlight names an element that says what it is" holds the rule for the whole
  family: every element rendering `p-focus` carries the flag.

## A roving tab stop is state, and needs somewhere to live

A widget whose children hold real focus puts `tabIndex(0)` on the ACTIVE child, and active means
where the reader is, not where they started. Pinning it to the first child compiles, renders and
looks right in a screenshot, and costs the reader their place: they walk twenty rows down, Tab to
another control, Shift+Tab back, and land at the top with every arrow press to make again.

- **The DOM knows where focus is; a render does not.** So the cursor belongs in a ref the mount
  mints, next to the ids it already mints, and the tab stop is derived from it. That is a
  re-render of the region per arrow key, which is what the list family already pays for its
  highlight and what `DataTable` deliberately does not: its cells are a grid where the tab order
  IS the editable cells, so it has no cursor to keep.
- **Key it by the row's own key, not by its index**, or an expand above the cursor moves it.
- **A pointer seeds it too**, so a click leaves the widget in the state the next arrow reads. Where
  the rows already carry a click handler, that is where it goes; where they do not, a seed-only
  handler is still worth its markup.
- **Fall back in the order the reader would expect**: the cursor, else the row they chose, else the
  first. A cursor pointing at a row that a collapse took off the screen falls back the same way.
- **The page as first rendered is already one tab stop, where the id is.** Before the client attaches
  the mount has published nothing, so there is no cursor and no key handler yet, but Tab works and a
  field-shaped control's id is already on the page for `Label.forId` or a focus-by-id. So the static
  projection puts the single stop where the attached render will (the list, the chosen option, the
  checked star, the chosen row, the active header) and only the keys wait for the mount. Every roving
  widget once got this wrong one way or the other: `Listbox`, a single `SelectButton`, `Rating` (no
  radio `name` yet, so the other stars take `tabIndex(-1)`), `TreeTable` and a horizontal `Stepper` made
  every row a stop; the four menus and `Tree` made none, since the list only took its stop with the
  mount's focus ref, and a selectable `OrganizationChart` had neither a stop nor keys (its node cursor moves the way
  the chart is drawn, `OrgChartNav`). `ElementIdTest` and `GoldenRenderTest` ("every roving widget is one tab stop ...")
  hold them; a new roving widget belongs in that sweep. Groups whose members are each their own stop by
  design (`MultiSelectButton`, `Accordion`, a vertical `Stepper`, `InputOtp`'s cells) are not roving.

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
- **`ReactiveValue` is the module's way to do that**: `Const` / `Dyn`, combined with `and` and
  rendered once. A combination of constants renders with no region at all, so a static projection
  stays free of subscriptions. A slot is one already (`slot.reading(unbound)`); `optional` / `orElse`
  lift the rest, `keyed` reads a set of flags as one map, `MenuRender.readDisabled` and `readAll` the
  menu state, `SingleChoice.reading` and `SetBinding.signal` the choices. Wrap internal state with
  `ReactiveValue.Dyn(ref)`, never `ReactiveValue(ref)`: the smart constructor dispatches on the runtime
  class and would make the ref two way. `map` drops the ref, so a mapped value handed on through
  `bound` binds one way.
- **A panel the host gates itself is part of its one region.** A host that reads the open state
  renders the panel only while open and uses `Overlay.renderShown` (or `renderGated(open)` where
  the trigger stays), never `render`, whose own `when(openRef)` would be a second region inside.
- **A wrapper adds no region around a mount.** A state the wrapper needs from the control (FloatLabel's
  `p-filled`) is stamped by the control inside its own region (`AutoComplete.stampFilled`), and an
  embedding host binds a control one way (`CellEditor.select`) rather than rendering it per value.
- **A class that follows a field's native text is the client's** (kyo-ui's `filledClass`), never a region and never
  a stamp from the binding: the binding does not have the text of an unbound field or the reader's edit of a starting
  one, and a region over a ref draws the field again on every keystroke. Input, TextArea and InputMask (through its
  Input) mark `p-filled` on the field; Password and InputNumber mark Prime's `p-inputwrapper-filled` on their wrapper
  (`filledClass(name, closest)`, `Input.filledOn`), and since the static render cannot reach an ancestor, the wrapper
  stamps the starting class itself (Password's and InputNumber's text ref through a class channel, which agrees
  with the client because the ref holds the field's text). AutoComplete keeps its stamp: it draws its field from the
  text it owns on every keystroke, so the stamp is never behind. The browser half is held in kyo-ui
  (`FilledClassTest`, `DomBackendFilledTest`); the JVM suites here check the marker and the starting class.
- `UicTest.regionsAbove` counts the subscriptions a control wraps its markup in, so the rule is
  checked rather than remembered.
- The roving highlight of an embedded list belongs in the same combined signal, not in a region of
  its own: the columns and their cursors are one state.
- **One reading, sibling regions, and nothing that follows none of it under them.** `DataTable` reads its
  state as one reading and draws its parts as SIBLING regions over it, never as one region around them all: a
  region above a mount starts it again on every repaint, and siblings over one reading cannot repaint older state
  over each other. Its root, the header and footer slots and the paginator follow none of the state (the
  paginator takes the record count as a signal and the page and page size from their refs). The table container
  is a region over the column layout alone (`tableContainer`): `table`, `thead` and `tbody` are built once per
  layout; the header follows the sort, the widths and the column drag through class, attribute and inline-style
  channels (`inlineStyle`, the kyo-ui style channel, carries widths and frozen offsets), a column's filter is a
  region over its own filter and open state, and the rows (the body's, the frozen ones', a window's) are keyed
  lists over one signal of the state and the row bindings (a window's with the scroll ref), their row group's
  height or offset an inline-style channel. The `colgroup` and the footer are sibling regions over the state. A
  bound layout is the one region allowed above a mount besides its own: a mount in a column that moves belongs to
  the new layout. A handler that needs state the header no longer redraws for reads its ref when it fires (the
  column drag in `headerClick`). `aria-sort` follows Prime's rule (every column a sort can name, `none` while
  unsorted), so static and live agree. `UicTest.regionsAboveMount` counts it (`DataTableRegionsTest`).

## Validity is a channel, never a region

`invalid(Signal)` / `invalidMessage(Signal)` reach every control through `FieldValidity`, built once at
the top of the render. `mark` (or `markClass` / `markAria`, where the sheet hooks `.p-invalid` on another
element than the one carrying `aria-invalid`) stamps class and attribute channels; `follow` appends the
message row as a region of its own AFTER the control. The control is never rebuilt by its validity: a
rebuild replaced the textarea a reader was typing into at the very keystroke that turned it invalid.

- **Place `follow` outside the control's own value and open regions**, inside a mount where there is
  one (the static HTML keeps the row inside the mount's host). The exception is a field with a trailing
  sibling: the pickers' `.p-uic-diagnostic` card follows the field, and the row belongs between the two,
  so there `follow` wraps the field alone inside the body. The row region is then rebuilt with an open
  or highlight change, the field still never by its validity. `FieldValidityTest` sweeps every control:
  no validity region above the root, the channels on exactly the elements the constant render marks,
  one message region. A new form control belongs in that sweep.
- **A composite hands the slots on**: `InputMask` passes them to its `Input`; `Password` and `InputOtp`
  pass `validity.red` to their inner inputs and follow the whole with the one row.

## Filtering: `filter`, `Filter`, `filterable`

The pickers share three filtering words. `filterable(Boolean | Signal[Boolean])` switches the header filter
(Select, MultiSelect, Listbox); `filterQuery(ref)` hands it an app-owned query (the same three);
`filter((A, String) => Boolean)` says how an option matches the raw typed query (Select, MultiSelect,
AutoComplete). `Filter` (`Filter.scala`) holds the ready-made matchers: `contains`, `startsWith`,
`startsWithPerTerm` over any number of text projections (a match in any one is enough, both sides
lowercased) and `all`. The defaults are `contains` on the label for Select/MultiSelect/Listbox and
`startsWithPerTerm` on the label for AutoComplete, which always filters and has no switch.

- **Precedence mirrors `invalid` in `FieldValidity.apply`.** Without an explicit `filterable`, a `filter` or a
  `filterQuery` turns the header on; an explicit one, constant or signal, decides alone. Each component reads
  that through one private `filterOn: BoolValue`; never test `filterF.isDefined` or the query ref anywhere else.
- **Off means not applied, never cleared.** The header goes, every option shows, and neither the control's own
  query nor an app `filterQuery` ref is written, not even by the open path's query reset, so switching back on
  shows the old text and filters by it at once.
- **The switch is not a region over the field.** A closed Select/MultiSelect reads the switch only in the
  trigger's role, which is a `role` attribute channel, so a toggle re-renders nothing; the signal is subscribed
  only inside the open branch of `wired`. Listbox renders its header as a region of its own inside the root and
  subscribes its options to the query as the options see it (empty while off), so a toggle over an empty query
  repaints nothing. `SelectTest`, `MultiSelectTest` and `ListboxTest` pin the region counts against the
  constant render.
- **Inference comes from the setter.** `filter` takes a plain function, so `Filter.contains(_.name, _.email)`
  gets `A` from the picker's expected type; do not add an overload beside it, or the lambdas lose that
  expected type (see "Typed builders" above). `FilterTest` holds the call site.

## An unbound ref is not a reason to disable a control

Optional bindings (`month`, `currentView`, `selected`) exist so a page CAN drive a component from
outside, not so the component only works when it is driven. A control wired straight to such a ref
and rendered `disabled` without one is a control the reader can neither click nor tab to, and
whatever it leads to goes with it: DatePicker's month and year title buttons rendered disabled by
default, which left the month and year grids, and the keyboard those grids had just been given,
unreachable in every picker that bound no `currentView`.

- **The mount mints what the caller did not bind**, next to the ids it already mints
  (`DatePicker.render`: the open, month, view and cursor refs, and the held value itself when no
  `value` is bound), and hands them to the `wired`
  seam. The static projection is the only place a control renders disabled for want of a ref, and
  that projection is inert throughout. A slot the component keeps itself is the exception: there the
  page as first rendered draws the control the reader will use, never the fixed one, since nothing
  about it is fixed (Paginator's rows dropdown, TreeTable's sortable headers; the contract's "does not
  call an unbound slot fixed" holds them).
- **The seam must mint the way the mount does**, from the component's own fields
  (`dp.currentViewRefV` else `Signal.initRef(dp.kind.view)`), or the golden documents a control the
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
  page still sees the restored value, because that is where the value lives. A ONE-WAY binding is the
  exception: nothing was written, its owner took every pick from the event, so the undo asks the owner
  through that same event for what was held, and only where it differs from what the owner holds now.
- **An in-flow variant has nothing to cancel** (`DatePicker.inline`): it does not close, and what
  is picked in the page is picked. Guard the cancel on the floating form, not just its close.

## `readonly` is one thing

A readonly control is one a reader can REACH and read the value of and cannot change:
focusable, saying so in the vocabulary its own role has (`aria-readonly` where the role
supports it, `aria-disabled` on a `role="button"` or a `role="radio"`, neither of which has a
readonly state), and inert to every key and click that would change it. `disabled` is the
different thing, and stays native. Select, CheckBox, ToggleSwitch, RadioButton, Rating and
ToggleButton all answer this way; `ReadonlyTest` holds them to it.

- **A fixed state is readonly too, and keeps the tab stop the live control has.** A Listbox whose
  selection is a constant nobody hears hung its `tabIndex` on the same branch as its click and key
  handlers, so the fixed list said `aria-readonly` on an element no reader could Tab to. Give the
  tab stop on its own condition (not disabled), and the handlers on theirs (`ListboxTest`).
- **A state the role has no readonly vocabulary for says nothing; `aria-expanded` carries it.** A
  tree has no readonly state for which branches are open, and a combobox none for whether its list
  is open. So a fixed expansion (TreeSelect's `expanded`, and Tree's) and a combobox panel fixed
  open (Select, MultiSelect, CascadeSelect, TreeSelect) add no `aria-readonly` or `aria-disabled`:
  either would misstate the selection, which is still the reader's. They are fixed because nothing
  pretends to change them: no toggle answers, no click on the field asks for a close, nothing
  outside dismisses, and the keys change nothing. Their tests judge what answers, never what is
  said (the contract's `isFixed`, not `saysFixed`). A control that IS only the opener (DatePicker's dropdown button) keeps the button
  vocabulary: a tab stop with `aria-disabled` and no click.
- **The same split holds for the structural components.** A control that only changes the state says so the way its
  role can: Carousel's navigators and Galleria's, Paginator's page links, OrderList's and PickList's move buttons and
  OrganizationChart's toggle are buttons with `aria-disabled`; a `role="tab"` header (Tabs, a horizontal Stepper) has no
  readonly state either, so an inactive one says `aria-disabled` the way a radio does (a Tabs url header still navigates,
  so it says nothing); a fixed dropdown (Paginator's rows and jump list) is a readonly Select. A state that shares its
  element with another the reader keeps says nothing and is judged by `isFixed`: a tree's or a treegrid's selection
  (Tree, TreeTable; a treegrid's readonly state is about its cells), an OrganizationChart node (no role at all), a
  TreeTable expansion, a TreeTable sort (`aria-sort` carries it).
- **A fixed sort header stays a tab stop with no handlers** (decided 2026-10-31). It keeps `tabIndex(0)`, `aria-sort`
  and its direction icon, answers no press or key, and says nothing more; it drops the sortable class, whose pointer
  cursor and hover highlight say a press would change something. Removing the stop was removing the control
  (the sort slots of `TreeTableTest` and `DataTableTest` hold it).
- **A fixed state whose control is only an affordance drops the affordance and keeps the content and the layout**
  (decided 2026-10-31). A sort header, a column resize handle, a column drag and a row grip are that: fixed widths draw
  no `.p-datatable-column-resizer` (the resizable-table classes serve the handles alone and go with them) and an
  expanding table still states its width; a fixed order keeps every header where it says, without
  `p-datatable-reorderable-column`; fixed rows keep the grip column, without `p-datatable-reorderable-row-handle`. None
  of them is a mistake, so none draws a card. A control that is a real control stays one: a fixed filter row is a native
  readonly field showing the query, and its funnel (the mode list in the row display, the conditions in the menu
  display) still opens what it holds, read-only, since nothing else lets the reader read the mode or the conditions; a
  fixed expansion's row and group toggles are buttons with `aria-disabled`; a fixed selection's rows keep their tab
  stops and `aria-selected` and drop `p-datatable-selectable-row`. A `Checkbox` row is a tab stop like a `Single` or
  `Multiple` one (its box is `aria-hidden`), and Enter and Space toggle its box.
- **Do not implement readonly by disabling the native input.** It drops the tab stop, so a
  keyboard reader cannot reach the value at all, and it makes readonly and disabled the same
  thing to a screen reader (which reports the disabled state and ignores `aria-readonly`
  beside it).
- **A handler cannot decline the browser's default**, since it runs asynchronously and
  remotely: by the time it is asked, the checkbox has already toggled. `UI.preventActivation`
  is the kyo-ui primitive that declines it in the client, the same way `preventScrollKeys`
  declines the page scroll. Both read from `kyo.internal.KeyPolicy`, and `KeyPolicyTest`
  holds the two transports against it.
- **A radio group walks past an inert radio the way it walks past a disabled one.** The
  browser's arrow walk would land on the focusable inert radio and stick there, so both
  clients walk a group that has one themselves (`KeyPolicy.radioStep`): on to the next
  radio that is neither inert nor disabled, and an inert radio that is selected holds the
  group. `RadioTest` measures the native walk in Chrome and holds the client to it.

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

Compiling is not all of it. The demo's `ApiMacro` reads a setter's BODY to fill the Default column of its tables: it follows
`new X(p.copy(f = v))` and `withG(g.copy(f = v))` to the field and reads what an unconfigured component holds there. A setter that
reaches its field another way prints no default or a wrong one, and nothing fails, since `CheckDocs` does not judge the text.

## Effect-as-value trap

An untyped `match` with `Unit < Async` and `Unit` branches infers `Any`: the
effect lands inert in a by-name handler and silently does nothing (a "dead"
button). Type handler-effect branches explicitly (`val eff: UI.Action = ...`).
`maybe.fold(())(f)` is the same trap in one line, even where the expected type is
`UI.Action`: it handed the action back as a value, so the paginator's two dropdowns
and the filter menu's operator and mode dropdowns did nothing on a pick. Write
`fold[UI.Action](())(f)`, and test a handler by running it (`PaginatorTest`,
`DataTableTest`), since a test that writes the ref itself never notices.
The build runs `-Werror`, so a stray `@nowarn` that no longer suppresses anything
fails it.
