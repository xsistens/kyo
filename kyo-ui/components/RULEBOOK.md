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
  drawn again; the component writes only its mapping from case to class, and no list of cases. Inside a `UI.reactive` block the
  same fold would not stay a channel: a read a lambda uses makes the block a region.
- **Shape:**

```scala
.classFor(p.size) {
    case Size.Small  => Present("p-button-sm")
    case Size.Large  => Present("p-button-lg")
    case Size.Normal => Absent
}
```

- Never one `cssClass(name, sig.map(_ == case))` per case (Tag, Badge and Message still do, until their wave): that is N
  channels for one value, and a case left out of the list never gets its class.

## 6. Flags are read in one `UI.reactive` block, never matched by their form

Flags that may be a constant or a signal (`disabled`, `aria-disabled`, `loading`) are read with `.read` inside one
`UI.reactive` block and used as plain values: combined with `&&` and `||`, handed to the setters that take a `Prop`
(`disabled`, `cssClass(name, on)`, `ariaFlag`), and used in a handler as they are. Never a match on `Boolean` against
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
        .when(p.onClick)((e, click) => e.onClick(if off then Kyo.unit else click))
        .ariaFlag("disabled", says)
        .cssClass("p-uic-aria-disabled", says)(...)
}
```

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
