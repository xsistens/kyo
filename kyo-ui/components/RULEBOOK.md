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
