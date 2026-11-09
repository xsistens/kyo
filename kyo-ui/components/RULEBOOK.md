# uic rulebook: how a component's code is written

The rules a component in `shared/src/main/scala/kyo/uic/` is written by, so its code is compact, tidy and reads the same in
every component. `CLAUDE.md` says what a component must do (the Solid model, slots, diagnostics, the contract); this file says
how its code looks. Each rule names the reason and shows the shape, from `ToggleButton` and `Button`, which follow all of
them.

## 1. Props fields have plain names, and the body reads them through `p`

A field is named for what it holds (`size`, `disabled`, `onClick`), with no `V`, `Flag`, `Eff` or `F` suffix, and the class
body reads it as `p.size`, never through `import p.*`.

- **Why:** the setter and the field then share one name, so a reader sees at once which field a setter writes, and the demo's
  `ApiMacro` reads the same name in both places. The import is what made the suffixes necessary: with it, a field named like
  its setter is shadowed by the setter inside the class, and a read silently picks the method.
- **Shape:**

```scala
def size(v: Prop[Size]): Button = new Button(p.copy(size = v))
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
and the setters that take a `Maybe` themselves (`aria("label", access.name)`, `aria("labelledby", access.nameRef)`). A branch
that builds two different chains is a `val` of an `if`, not a `var` set in each arm.

- **Why:** `var el = ...; x.foreach(v => el = el.x(v))` spends a line and a reassignment on every optional attribute, and
  hides the order of the chain in statements; the chain reads top to bottom as the element's attributes. uic had 789 such
  reassignments when `when` was added (2026-11-05), all from this one missing shape.
- **Shape:**

```scala
classed(el, loading)
    .when(p.access.id)(_.id(_))
    .when(p.focusSeed)(_.focusAuto(true).focusRestore(true))
    .when(p.onKeyDown)(_.onKeyDown(_))
    .aria("label", p.access.name)
```

- A step that is a match over a value (until rule 4 removes it) is a small local `def step(e: E): E`, applied in the chain.

## 4. A text slot is a `Prop[String]`, written with one call

A text a caller may give as a constant or a signal (a label, a tooltip, an accessible name or description) is stored as
`Prop[String]` (or `Maybe[Prop[String]]` where it is optional) and written with the kyo-ui setter that takes it:
`aria(name, prop)`, `aria(name, maybe)`, `title(prop)`, `title(maybe)`, or as a child (`liftPropString`). Never match it into
its constant and its signal at the render site.

- **Why:** the setter already knows a constant from a signal (a constant as an attribute, a signal as its channel), so a match
  at each render site is the same five lines in every component, and the slot's type says nothing a reader can use. The shared
  slots (`HasTooltip`, `HasAccessibleDescription`, `Accessibility.name`) store `Maybe[Prop[String]]` for every component that
  carries them.
- **Shape:**

```scala
.aria("label", p.access.name)
.aria("description", p.description)
.title(p.tooltip)
```

- `TextValue` (`ReactiveValue[String]`) is the form the components not yet converted still store; it goes with
  `ReactiveValue` in the last wave.

## 5. A class per case of a closed set goes through `classFor`

A prop over a closed set of cases that picks one class (a severity, a variant, a size, a position) is written with the shared
`el.classFor(prop)(case => Maybe[class])` (`Classes.scala`), never with a helper of the component's own, and never with
`s"p-x-${v.token}"` on a value read out of a prop.

- **Why:** a constant then adds its one class, and a signal becomes one class-name channel (kyo-ui's
  `cssClass(Signal[Maybe[String]])`), so a change takes the old class off and puts the new one on in place and nothing is
  drawn again; the component writes only its mapping from case to class, and no list of cases.
- **The mapping lives on the type** when the classes follow one pattern across components: `Size.cls(prefix)` (`-sm`, `-lg`,
  `-xl`, none for Normal), `Severity.cls(prefix)` (none for Primary), `ButtonVariant.cls`. A component writes its own cases only
  where its classes break the pattern (Message's `error` for Danger).
- **Shape:**

```scala
.classFor(p.severity)(_.cls("p-button")).classFor(p.variant)(_.cls).classFor(p.size)(_.cls("p-button"))
```

- Never one `cssClass(name, sig.map(_ == case))` per case (Tag, Badge and Message still do, until their wave): that is N
  channels for one value, and a case left out of the list never gets its class.

## 6. Flags are read in one `UI.reactive` block, never matched by their form

Flags that may be a constant or a signal (`disabled`, `aria-disabled`, `loading`) are read with `.read` inside one
`UI.reactive` block and used as plain values: combined with `&&` and `||`, handed to the setters that take a `Prop`
(`disabled`, `silent`, `cssClass(name, on)`, `ariaFlag`). A handler never checks a flag: a locked element is silent. Never a match on `Boolean` against
`Signal` at the place a flag is used, never a `var` set in its arms, never `Prop.zip`/`Prop.map` by hand for what the block
can say.

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
        .ariaFlag("disabled", says)
        .cssClass("p-uic-aria-disabled", says)(...)
}
```

- **A locked element is silent, never a handler that checks the flag.** `disabled(off)` silences the element by itself;
  a lock that is not native `disabled` (readonly, a fixed value) says `silent(locked)`. A silent element lists no event in
  `data-kyo-ev` and runs no handler, as target or ancestor, and a signal patches that in place, so the handler only does its
  job (`onClick(toggle)`) and the element draws nothing again. A click that has to stay while the element looks locked
  (Button's `ariaDisabled`, which routes the press to a form submit) is simply not silenced.
- An attribute that a constant writes only while true (`aria-disabled`, `aria-busy`) is `ariaFlag`; one where `"false"` says
  something (`aria-pressed`) is `aria(name, on.toString)`.
- A setter beside its union with a plain overload (`disabled(Boolean)` once) keeps a `Boolean` off the channel: kyo-ui has one
  setter per flag, and a component adds no plain overload of its own.

## 7. A list of host-added values holds props and is folded with the setter

What a host hands a component in a list (extra aria attributes, extra classes) holds the same type a single setter takes
(`List[(String, Prop[String])]`), and the render folds it with that setter in the chain:
`p.extraArias.foldLeft(el)((e, aria) => e.aria(aria._1, aria._2))`. No helper that matches each entry into its constant and
its signal.

- **Why:** the setter already tells the forms apart (rule 4), so the fold is one line in the chain, and a host can hand over a
  constant or a signal without the component knowing which.

## 8. A setter that takes a constant or a signal says `Prop[A]`

Every setter and every field that takes either form is typed `Prop[A]` (`disabled(v: Prop[Boolean])`, `apply(label: Prop[String])`,
`icon(glyph: Prop[IconGlyph])`), never the spelled-out `A | Signal[A]`, and a slot that may stay unset is `Maybe[Prop[A]]`.

- **Why:** one name for one idea, so a reader sees at a glance which setters follow a signal, and a field and its setter say the
  same type. A slot that was constant only because nobody needed more (Button's icons) becomes a `Prop` too, drawn with
  `Prop.render`: a constant draws once with no region, a signal in a region of its own, below the component.

## 9. A field is a `Maybe` only where unset is not a value; the draw has its scaladoc

A prop whose unset form behaves as one value is that value as its default (`disabled: Prop[Boolean] = false`), and its setter is
`p.copy(x = v)`. A `Maybe` is for a slot whose absence draws something different from every value it can hold: no label span, an
icon-only button, no link, no attribute, no handler. Before keeping one, check its readers: if each turns `Absent` into the same
value at once (`getOrElse(false)`), it is a default.

- **Why:** a `Maybe` that every reader unwraps to one value is a second spelling of that value, and each reader has to repeat it.
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
