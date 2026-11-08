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
