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
