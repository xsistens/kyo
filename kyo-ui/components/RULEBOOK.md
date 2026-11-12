# uic rulebook: how a component's code is written

The rules a component in `prime/shared/src/main/scala/kyo/uic/` is written by, so its code is compact, tidy and reads the same in
every component. `CLAUDE.md` says what a component must do (the Solid model, slots, diagnostics, the contract); this file says
how its code looks. Each rule names the reason and shows the shape, from `ToggleButton` and `Button`, which follow all of
them.

## Migrating a component (read before the first edit)

A migrated component has ToggleButton's shape (Button's, where it keeps no state), not only the rules below applied line by line.
The target is the template in `CLAUDE.md` ("The template is `ToggleButton`") as much as this file, and a migration is judged by
comparing the result with ToggleButton, aspect by aspect.

- **Compare first, against the reference, and write the comparison down.** Before any edit, for each aspect say how the component
  does it and how ToggleButton does it: its own state (a `Controlled` with a `start` the engine binds, never a ref the component
  mints for a value), one `render = UI.mountedWith(p)(drawn)` (or Button's direct read where it keeps nothing), a `Base` that holds
  only what decides the structure (never a value that can be a channel), every field a `Prop` unless it decides the structure,
  text slots `Prop[String]`, setters generated, handlers reading the run when their event comes, no bridge read (`binding`,
  `ReactiveValue`, `.const`) in the component. Every aspect that differs is a finding, including those no numbered rule names.
- **Nothing is left out in silence.** Every finding ends applied, or stopped on as a question with the reason. "Kept because the
  bytes would change" and "kept because another component has to change first" are questions, never decisions.
- **Stop and ask when the clean result needs another component changed first** (its builder takes no `Prop`, has no `when`, holds
  a type this component should not): say what the other component lacks, what the clean result would be, and what the
  alternatives cost. Never work around it locally without the principal's answer.
- **Stop and ask when an output stops being byte-equal or a region count changes.** Show the diff of the rendered output and the
  region counts before and after, say why the new output is or is not correct, and wait. Byte equality is the check that nothing
  changed by accident, never a reason to keep a shape the rules replace.
- **The report names, per aspect of the comparison, what was applied, what is a question, and why.**

**The order of a migration.** The rules are numbered in the order they were found, not in the order they are applied, and a later
rule can make an earlier step wasted work (a hand-written setter renamed by rule 1 and then deleted by rule 11, a draw put into
chains by rule 3 and then rewritten by the template). So a migration goes outside in, and each phase changes only what the
phases after it keep:

1. **Structure, the template** (`CLAUDE.md`): the component's own state as a `Controlled` with a `start` the engine binds, one
   `render`, a `Base` of only what decides the structure, one draw path (rule 2). Everything after this is written into this shape.
2. **The props:** field names (rule 1), field types: `Prop[A]` wherever a constant or a signal makes sense (rule 8), an `Opt` for an
   optional prop the caller sets, a default where unset is a value and a `Maybe` only for a callback or a slot a host or trait writes
   (rule 9), text as `Prop[String]` (rule 4), host lists of props (rule 7).
3. **The setters, generated** (rule 11). Hand-written setters are deleted here, never edited before; the ones that stay are those
   that do more than store.
4. **The draw:** one `UI.reactive` block over the structure (rule 6), each element one chain (rule 3), `cssClass.matching` (rule 5), leaves
   handed the prop (rule 10), texts and host lists written with one call each (rules 4 and 7).
5. **Order and scaladoc** (rule 9).

A phase that needs another component changed first stops there and asks.

## 1. Props fields have plain names, and the body reads them through `p`

A field is named for what it holds (`size`, `disabled`, `onClick`), with no `V`, `Flag`, `Eff` or `F` suffix, and the class
body reads it as `p.size`, never through `import p.*`.

- **Why:** the setter and the field then share one name, so a reader sees at once which field a setter writes, and `Setters`
  generates the setter from the field (rule 11). The import is what made the suffixes necessary: with it, a field named like
  its setter is shadowed by the setter inside the class, and a read silently picks the method.
- **Shape:**

```scala
// in Props: the setter `size(v: Prop[Size])` is generated from it
size: Prop[Size] = Size.Normal,
// in the body
p.size
```

- A draw over a `UI.Live` reads `props(_.size)`, which needs no prefix at all.

## 2. One draw path; only what differs by element is written twice

A component that renders on more than one element (Button's `<button>` and its `<a>`), or for more than one caller
(ToggleButton's own instance and SelectButton's in-place option), writes what they share once, in one function over the
element type, and keeps apart only what truly differs.

- **Why:** two paths that repeat the same dozen attribute lines drift: a fix lands on one and not the other, and a reader has to
  diff them to learn which lines matter.
- **Shape:** the shared part takes any element of the kind both share (`[E <: (UI.Ast.Element & UI.Ast.Interactive) { type Self = E }]`, since focus and keys live on `Interactive`), so the button and the link both run
  through it.

```scala
private def shared[E <: (UI.Ast.Element & UI.Ast.Interactive) { type Self = E }](el: E, loading: Boolean)(using Frame): E = ... // classes, id, aria, title, role
private def renderButton(loading: Boolean)(using Frame): UI = shared(button.jsProp("type", ...), loading) ... // type, disabled, click
private def renderAnchor(h: String, loading: Boolean, disabled: Boolean)(using Frame): UI = shared(a, loading) ... // href, p-disabled
```

- The order of calls on a field that keeps one value matters where both paths set it: the later call wins (Button's host role
  over its `accessibleRole`), so the shared part runs after what it overrides.

## 3. An element is built as one chain, never as a `var` reassigned

Optional and conditional settings stay in the chain: `when(maybe)(_.x(_))` for a `Maybe`, `when(cond)(_.x)` for a condition,
and the setters that take a `Maybe` themselves (`aria(Aria.label -> access.name, Aria.labelledby -> access.nameRef)`). A branch
that builds two different chains is a `val` of an `if`, not a `var` set in each arm.

- **Why:** `var el = ...; x.foreach(v => el = el.x(v))` spends a line and a reassignment on every optional attribute, and
  hides the order of the chain in statements; the chain reads top to bottom as the element's attributes. uic had 789 such
  reassignments when `when` was added (2026-11-05), all from this one missing shape.
- **Shape:**

```scala
classed(el, loading)
    .when(p.access.id)(_.id(_))
    .focusAuto(p.focusSeed).focusRestore(p.focusSeed)
    .when(p.onKeyDown)(_.onKeyDown(_))
    .aria(Aria.label -> p.access.name)
```

- **A setter that takes the condition gets it directly** (`focusAuto(p.focusSeed)`, `disabled(off)`; kyo-ui's client-side markers
  such as `focusAuto` take `false` as unset); `when` is for a setter called only under a condition (`when(blurs)(_.onBlur(blur))`), and
  for one whose value the element may already carry from elsewhere: Button's popup trigger is the host's own Button, so
  `trigger.when(fixed)(_.ariaDisabled(true))` keeps a host's `ariaDisabled` where `ariaDisabled(fixed)` would reset it.

- In a migration rule 4 comes first (phase 2), so no step of the chain is a match over a value's form. Where a step still needs a
  `def`, it is a small local `def step(e: E): E`, applied in the chain.

## 4. A text slot is a `Prop[String]`, written with one call

A text a caller may give as a constant or a signal (a label, a tooltip, an accessible name or description) is stored as
`Prop[String]` (where it is optional, `Opt[Prop[String]]` as the component's own prop and `Maybe[Prop[String]]` in a shared slot
a trait writes) and written with the kyo-ui setter that takes it:
`aria(name -> prop)` and `aria(name -> maybe)` (one entry form, the value's type decides), `title(prop)`, `title(maybe)`, or as a
child (`liftPropString`). Never match it into its constant and its signal at the render site.

- **Why:** the setter already knows a constant from a signal (a constant as an attribute, a signal as its channel), so a match
  at each render site is the same five lines in every component, and the slot's type says nothing a reader can use. The shared
  slots (`HasTooltip`, `HasAccessibleDescription`, `Accessibility.name`) store `Maybe[Prop[String]]` for every component that
  carries them.
- **Shape:**

```scala
.aria(Aria.label -> p.access.name, Aria.description -> p.description)
.title(p.tooltip)
```

- `TextValue` (`ReactiveValue[String]`) is the form the components not yet converted still store; it goes with
  `ReactiveValue` in the last wave.

## 5. A class per case of a closed set goes through `cssClass.matching`

A prop over a closed set of cases that picks one class (a severity, a variant, a size, a position) is written with kyo-ui's
`el.cssClass.matching(prop) { case ... => class }`, each case giving its class as a `String` or `Absent` for none, never with a
helper of the component's own, and never with `s"p-x-${v.token}"` on a value read out of a prop.

- **Why:** a constant then adds its one class, and a signal becomes one class-name channel, so a change takes the old class off
  and puts the new one on in place and nothing is drawn again. The match names every case, so under `-Werror` a case left out
  does not compile.
- **Every case is written out** at the component, with its class spelled in full (`"p-button-sm"`), so the class a reader looks
  for in the CSS is the one the source shows. A mapping that the type knows and the class does not is the one place a function
  of the case stays, written inline in the `matching` call.
- **Shape:**

```scala
.cssClass.matching(p.size) {
    case Size.Small  => "p-button-sm"
    case Size.Normal => Absent
    case Size.Large  => "p-button-lg"
}
```

- Never one `when(sig.map(_ == case))(_.cssClass(name))` per case: that is N channels for one value, and a case left out of the
  list never gets its class.
- A class that follows a condition is `el.when(cond)(_.cssClass(name))` (rule 6), several classes are one
  `el.cssClass("a", "b")`.

## 6. Flags are read in one `UI.reactive` block, never matched by their form

Flags that may be a constant or a signal (`disabled`, `aria-disabled`, `loading`) are read with `.read` inside one
`UI.reactive` block and used as plain values: combined with `&&` and `||`, handed to the setters that take a `Prop`
(`disabled`, `silent`, an `aria` entry's value) or to `when(flag)(_.cssClass(name))`. A handler never checks a flag: a locked element is
silent. Never a match on `Boolean` against `Signal` at the place a flag is used, never a `var` set in its arms, never
`Prop.zip`/`Prop.map` by hand for what the block can say.

- **Why:** the code says what the button does and not which props are signals. The macro sorts it out: a value that only
  reaches channels and handlers stays a channel (no region), a handler reads it when its event comes, and only a read the
  structure needs (`busy`: the spinner and the icon-only class) makes the block a region. With every flag a constant the block
  is a plain element.
- **Shape:**

```scala
UI.reactive {
    val busy = loading.read
    val off  = busy || disabled.read
    val says = ariaDisabled.read && !off
    renderButton(busy).disabled(off)
        .when(p.onClick)(_.onClick(_))
        .aria(Aria.disabled -> says)
        .when(says)(_.cssClass("p-uic-aria-disabled"))(...)
}
```

- **`when` over a flag sets only what has a channel.** `el.when(cond)(f)` with `f` calling only the setters of kyo-ui's `Toggle`
  (classes, `aria` and `data` entries, `role`, `hidden`, `silent`, and `tabIndex`, `disabled`, `readOnly`, `required` where the element has
  them, each with plain values, an entry's value holding no signal) binds each to the flag's channel: a read or a `val` that only reaches channels stays a channel there, as
  it does in a setter that takes a `Prop` (the macro rewrites the call to the `Prop` form). An `f` with a handler, a style or a
  child is structure: the flag becomes the block's, a region, so such a `when` takes only what decides the structure (the `Base`).

- **A locked element is silent, never a handler that checks the flag.** `disabled(off)` silences the element by itself;
  a lock that is not native `disabled` (readonly, a fixed value) says `silent(locked)`. A silent element lists no event in
  `data-kyo-ev` and runs no handler, as target or ancestor, and a signal patches that in place, so the handler only does its
  job (`onClick(toggle)`) and the element draws nothing again. A click that has to stay while the element looks locked
  (Button's `ariaDisabled`, which routes the press to a form submit) is simply not silenced.
- **A text field's native `readonly` is not silenced either** (Input). The browser already refuses the edit, and the field
  stays a place the reader reaches and leaves: `silent` would drop its blur too, and the form layer's `Blur` trigger listens
  there. So `readOnly(locked)` alone says it, and the handlers stay.
- An `aria` or `data` value writes what it is, so a flag or a number is handed over as it is, never through `toString`:
  `aria(Aria.pressed -> checked, Aria.disabled -> says, Aria.valuenow -> now)` writes `"true"`, `"false"` and the number's text, and
  `aria-disabled="false"` means what no attribute means (ARIA 1.2). An attribute that must be left out names that with a
  `Maybe` (or a signal of one): `Absent` writes nothing. Several attributes are one call with several entries, and a group kept
  in a `val` is `Entry.of(...)`, spread with `aria(group*)`.
- A setter beside its union with a plain overload (`disabled(Boolean)` once) keeps a `Boolean` off the channel: kyo-ui has one
  setter per flag, and a component adds no plain overload of its own.

## 7. A list of host-added values holds props and is folded with the setter

What a host hands a component in a list (extra aria attributes, extra classes) holds the same type a single setter takes
(`List[(String, Prop[String])]`), and the render folds it with that setter in the chain:
`p.extraArias.foldLeft(el)((e, aria) => e.aria(aria))`, each pair converting to an entry as written. No helper that matches each entry into its constant and
its signal.

- **Why:** the setter already tells the forms apart (rule 4), so the fold is one line in the chain, and a host can hand over a
  constant or a signal without the component knowing which.

## 8. A field that takes a constant or a signal says `Prop[A]`

Every field that may take either form is typed `Prop[A]` (`disabled: Prop[Boolean]`, `size: Prop[Size]`, `icon: Opt[Prop[IconGlyph]]`),
never the spelled-out `A | Signal[A]`; its generated setter then takes the same (rule 11), and so does a hand-written one
(`apply(label: Prop[String])`). An optional prop the caller sets is `Opt[Prop[A]]`.

- **Why:** one name for one idea, so a reader sees at a glance which settings follow a signal, and a field and its setter say the
  same type. A field that was constant only because nobody needed more (Button's icons, a size) becomes a `Prop` too: it reaches a
  channel, or a leaf that draws it (rule 10), so a constant costs nothing and a signal patches in place. A field stays a plain
  value only where it decides the structure (Password's `feedback`, which adds the meter).
- **A redraw is avoided where it is not needed, not at any price.** A change a reader makes often patches in place; a rare change
  that needs another element (Input's type crossing to or from `Number`, a different element class) may draw the field again, and
  then the field is a plain `Prop[A]` that redraws only across that boundary. A union of a constant type and a narrower signal
  type (`InputType | Signal[UI.TextType]`) only to rule out that redraw is the API paying for the engine; never.

## 9. A field is a `Maybe` only where unset is not a value; the draw has its scaladoc

A prop whose unset form behaves as one value is that value as its default (`disabled: Prop[Boolean] = false`). A prop whose absence
draws something different from every value it can hold (no label span, an icon-only button, no link, no attribute) is an `Opt`,
so its generated setter takes the value itself; a `Maybe` is left for a callback (`Maybe[A => UI.Action]`, a handler or not) and for
a slot a host or a shared trait writes (`@noSetter`). Before keeping an `Opt` or a `Maybe`, check its readers: if each turns
`Absent` into the same value at once (`getOrElse(false)`), it is a default.

- **Why:** a `Maybe` that every reader unwraps to one value is a second spelling of that value, and each reader has to repeat it.
- **The option sits outside the prop: `Opt[Prop[A]]`, never `Prop[Opt[A]]`** (decided 2026-11-12, Skeleton). `Opt` is a `Maybe` to
  its reader and takes the value itself at its setter (`Opt.set`). Outside, the setter takes `"16px"` and a `Signal[String]` as they
  are, and only whether the prop is set is fixed when it is built. Inside (`Prop[Opt[A]]`), a signal could also switch the prop off,
  but the setter takes only an `Opt` or a `Signal[Opt[A]]`: a `Signal[String]`, a `Signal[Maybe[String]]`, `Present(...)` and `Absent`
  stop compiling, since `Signal` is invariant, a conversion does not reach into one, and `Opt` is no `Maybe` outside its own file
  (checked with scalac 3.9.0). A signal of the value is the common case, so the option stays outside, and kyo-ui's `aria` and
  `title` take the same `Maybe[Prop[A]]`. A block reads it in one read: `p.x.read` is a `Maybe[A]` (`props(_.x).read` over a
  `UI.Live`), `Absent` where unset and a signal inside followed, so a draw over an optional prop needs no `fold` into two blocks.
- **Order:** the class holds its setters, then the draw with a scaladoc saying what it reads where (no `//` block above it), then
  the private helpers. The companion holds `Props` (a case class that `derives CanEqual`), then the constructors.

## 10. A leaf that draws a value takes the value's prop

A shared building block that draws one value (`GlyphSvg` for an icon) takes `Prop[A]` and draws a signal with `Prop.render`
itself, so the caller hands the prop over: `p.icon.map(GlyphSvg(_, "p-button-icon", "p-button-icon-left"))`, never
`Prop.render(g)(GlyphSvg(_, ...))` at the call site.

- **Why:** the question constant or signal is answered once, where the value is drawn, as kyo-ui's text leaf takes a
  `Signal[String]`. A constant still draws with no region, so callers that hand a plain value see no difference.
- A helper that draws part of an element's content returns `UI.Children` and builds it with one
  `UI.children(leading, label, endIcon, extraChildren)`: a part is a child, a `Maybe` of one, a list or other `Children`, an absent
  part adds nothing, and the element splats it (`el(content*)`). Never `List[UI]` with `flatMap(_.toList)` and a second
  `UI.children` at the element.

## 11. A setter is generated from its field; the class writes only a setter that does more

A component is `final class X private[uic] (private[uic] val p: X.Props) extends Component(Setters[X.Props, X])`, and every field
of `Props` is a setter of its name, made by `Setters` at compile time and completed by the IDE from `Fields`. What a field's type
says decides the setter:

- **A plain field** takes its own type: `size: Prop[Size]` is `size(v: Prop[Size])`, `href: Opt[String]` takes the string.
- **A `UI.Ownable` field** (`Controlled[A]`) binds: `checked(v: Prop[Boolean])`, a ref two way and anything else one way, and it
  has an event setter that hears every change, `onCheckedChange`, or the name `@eventName("onChange")` gives.
- **A callback field** (`Maybe[A => UI.Action]`, `Maybe[(A, B) => UI.Action]`) takes the function itself: `onBlur(f)`.
- **An action field** (`Maybe[UI.Action]`) takes the action by name: Button's `onClick(action: => UI.Action)`, built each time
  its event comes and never where the setter is called.
- A function handed to an event or a callback, and an action, runs under `UI.attempt`, so its failure reaches the `onFailure`
  rules where it ran and the component's own work after it still happens. A hand-written callback setter does the same.

The class writes a setter by hand only where it does more than store what it is given (Button's `ariaDisabled`, with its gate),
under the field's name, and that member takes the generated one's place. A field a host, a shared trait or a setter of another
name writes is `@noSetter` (`access`, `validity`, `radio`). Generic code that has to call a setter it cannot name (the number
binding's `integer`) asks for a `HasSetter[X, "integer", Boolean]`: one is found in `Fields`, and a component that writes the setter
by hand gives its own in its companion (`InputNumber`), so both kinds of component serve it. What every control of a value type
answers is a member of the trait instead: a form binds a `FormControl[A]` through `bindRef` and `hearBlur`, one line each over the
control's own setters, `this.value(ref)` where they are generated.

- **Why:** a setter that only copies its value into its field is the field said twice, and the two drift (a renamed field, a
  setter that forgot `UI.attempt`). Generated, the field's type and its scaladoc are the setter's, and a reader finds what a
  component holds and what it can be told in one place, `Props`.
- **Shape:** `ToggleButton`: its `Props` carries `@eventName("onChange") checked: Controlled[Boolean]`, `onLabel`, `size`,
  `onBlur` and the rest, and the class writes only `noLabels` and its package-internal hooks.
