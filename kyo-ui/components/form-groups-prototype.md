# Typed form groups: prototype notes

Branch `proto/form-groups`, on top of the W32 commit (`a9689fec2`). This is a prototype to decide on, not a
finished feature: no README, no demo, and the module CLAUDE.md still says `FieldArray` has no typed submit.

## What it does

A typed submit used to name its fields again (`form.submit(body, due)((text, day) => ...)`), because a `Form`'s
type does not grow with each `declare`. A group declares everything at once and keeps the shape in its type:

```scala
for
  f <- form.fields((
         customer = form.field.rules(Validator.required()),
         due      = form.field(Maybe.empty[LocalDate]).rules(Validator.chosen()),
         lines    = form.array(initial = 1)(row => (
                      sku = row.field.rules(Validator.required()),
                      qty = row.numberField(1).rules(Validator.min(1))
                    )),
         shipping = form.child(scope => (
                      street = scope.field.rules(Validator.required()),
                      city   = scope.field.rules(Validator.required())
                    ))
       ))
yield div(
  uic.Input().bind(f.customer),
  uic.DatePicker[LocalDate]().bind(f.due),
  f.lines.render(line => div(uic.Input().bind(line.sku), uic.InputNumber().bind(line.qty))),
  uic.Input().bind(f.shipping.street), uic.Input().bind(f.shipping.city),
  uic.Button("Order").onClick(f.submit(v => order(v.customer, v.due, v.lines, v.shipping)))
)
```

This compiles exactly as written (`FieldGroupTest`'s `orderForm`), and `v` is

```scala
(customer: String, due: LocalDate, lines: Chunk[(sku: String, qty: Int)], shipping: (street: String, city: String))
```

which the test checks with `=:=`, so it is the exact type and not merely a supertype of it.

- `form.fields(specs)` declares every spec in one `< Async` effect, in tuple order (so declaration order, focus
  order and summary order are the tuple's), and returns a `FieldGroup`.
- `f.customer` is the declared `FormField[String] { type Valid = String }`; every `bind` works unchanged.
- `form.array(initial)(row => specs)` gives a `GroupArray` with `render(row => UI)`, `add`, `removeAt`, `move`,
  `rows`, `currentRows`, `satisfy`. A row is an `ArrayRow`: its fields by name, plus `remove`, `moveUp`,
  `moveDown`, `index`. It is a `FieldArray` underneath, so rows take part in errors, dirty tracking, reset and
  focus exactly as `fieldArrayOf` rows do.
- `form.child(scope => specs)` is `child()` plus the specs declared in it; `f.shipping.scope` is that child `Form`
  (own summary, anchor, reset).
- `f.submit(v => ...)` is the W32 submit with every leaf of the group named: it snapshots the current rows, passes
  the leaves to `submitWith` (so the subtree walk skips them and each field's rules run once per submit), runs
  their verdicts, and builds `v` from the same verdicts. Focus-first-invalid, `submitting` and `onFailure` are
  `submitWith`'s, untouched.

## Changes to existing code

Three, all small: `Form.submitWith` went from `private` to `private[form]`; `Form` gained `fields`, `array` and a
`child(build)` overload, each a one-line delegate; `FieldArray` gained a `private[form] renderWith(R => UI)`, since
a group array's row UI is given at `render` time rather than by the row builder. Everything else is in
`form/FieldGroup.scala`. No existing test changed; all 1148 JVM / 1146 JS tests still pass alongside the new ones.

## Type-level mechanism

Three pieces, no match types:

1. **A type class per spec kind**, `GroupMember[S]` with `type Handle` and `type Out`. Its four givens map
   `FieldSpec[A] { type Valid = B }` to `FormField[A] { type Valid = B }` and `B`, the same for number fields,
   `ArraySpec[N, H, V]` to `GroupArray[N, H, V]` and `Chunk[NamedTuple[N, V]]`, and `ChildSpec[N, H, V]` to
   `FieldGroup[N, H, V]` and `NamedTuple[N, V]`. The instances also carry the runtime half (declare a spec,
   snapshot a handle), so nothing is matched on `Any` at run time.
2. **A tuple-level type class**, `GroupMembers[N, S]`, derived by `empty` and `cons` over the names and specs
   together. `cons` reads each name with `ValueOf[L]`, so no `inline`/`constValueTuple` is needed.
3. **`Selectable` with `type Fields = NamedTuple[N, H]`**, which is what turns `f.customer` into a typed member
   access. `submit` takes `NamedTuple[N, V] => UI.Action`; a value of type `V` already is a `NamedTuple[N, V]` (the
   opaque type's lower bound), so the handler is called without a conversion.

Casts: none written by hand. The only one is the one the compiler inserts after `selectDynamic` (that is how
`Selectable` works); the validated values are built fully typed (`o *: os`, `Chunk[V]`).

### What did not work, precisely

- **Match types over the refinement.** The obvious `type HandleOf[S] = S match { case FieldSpec[a] { type Valid = v }
  => ... }` does not bind `v`: `Not found: type v` (3.9.0). Capture through an alias (`type TSpec[A, B] =
  FieldSpec[A] { type Valid = B }`, `case TSpec[a, v]`) is refused by SIP-56: `The match type contains an illegal
  case: case TSpec[a, v] => ... The pattern contains a type alias TSpec. (this error can be ignored for now with
  -source:3.3)`. Matching the type member would need it to be a type parameter, which is exactly what W32 decided
  against. The type class unifies `{ type Valid = B }` without trouble.
- **Path-dependent given results.** The first version computed `m.Handle *: rest.Handles` from the given
  parameters. It compiled and ran, but every message showed skolems: `value custmer is not a member of (customer :
  ?1.Out, qty : ?2.Out) where: ?1 is an unknown value of type GroupMember.Aux[...]`. Taking the element and the
  rest as `Aux[S, SH, SO]` with type parameters, and giving `fields`/`array`/`child` explicit `H`, `V` type
  parameters solved by the implicit search, makes every type concrete.
- `@implicitNotFound` on `GroupMembers` cannot name the specs (`${S}`) once the search goes through the `Aux`
  alias: it printed a literal `S`. The message now says "these specs".
- `Form.array`/`Form.child` cannot build the spec subclasses directly (`Cannot extend sealed class ArraySpec in a
  different source file`); they delegate to factories in `FieldGroup.scala`.

## Compiler messages (developer experience)

All four are pinned in `FieldGroupTest`.

- Mistyped name in the handler: `value custmer is not a member of (customer : String, qty : Int)`.
- Mistyped name on the group: `value custmer is not a member of kyo.uic.form.FieldGroup[Tuple1[("customer" :
  String)], Tuple1[kyo.uic.form.FormField[String]{type Valid = String}], Tuple1[String]]`. Accurate, but the full
  group type gets long for a real form (the order form's runs to several lines).
- Field bound to the wrong control (`uic.Input().bind(f.due)`): `Found: kyo.uic.form.FormField[kyo.Maybe[
  java.time.LocalDate]]{type Valid = java.time.LocalDate} Required: kyo.uic.form.FormField[String]`. The same
  message as for a field declared by hand.
- An already declared field in the tuple (`form.fields((name = declaredField))`): `Cannot declare a field group
  from these specs. Every member must be an UNDECLARED spec: ...`, followed by the compiler's own line naming the
  member: `no implicit values were found that match type GroupMember[FormField[String]{type Valid = String}]`.

## Tests

`form/FieldGroupTest.scala`, 12 tests (shared): the exact type of `v` (`=:=`), member types by name (field,
refining number field, row field, child field, `scope`), the four messages above, the reference example rendered
with its members bound by id and a valid submit through the rendered button handing over the exact values,
invalid top / row / child fields each blocking the handler and showing their error (top and row also take focus;
the child's error shows in `f.shipping.scope.allErrors`), rows added, removed, moved and self-removed reflected in
the submitted `Chunk`, and one rule run per field per submit at the top, in each row and in the child.

One thing the last test found: `FieldArray.add` wires a row's `Activation.Change` observer at once, and its silent
first validation is a rule run at `add`, not at submit. The test counts submit-only fields so it measures what
the submit runs; with default triggers a row field's rules run once more per added row, which is the existing
`FieldArray` behaviour and not something the group adds.

## Compile-time cost

Not measured in isolation, and not noticeable: an incremental compile of the main and test changes plus the
`kyo.uic.form.*` run took 36 to 42 seconds in total. Derivation is one `cons` per member and recursive only into arrays and children, so it grows linearly with the
form.

## Limits

- **A member named like a group method is shadowed**: `submit` and `scope` on every group, and `remove`,
  `moveUp`, `moveDown`, `index` on a row. `f.submit` on a field named `submit` silently selects the method. This
  is how `Selectable` resolves; the only cure is fewer members (for example `FieldGroup.submit(f)` in a companion).
- The specs must be built inside the tuple, on the scope they belong to (`row.field` in the row builder,
  `scope.field` in the child builder). A spec from another form is accepted and registers there, as `declare`
  would.
- The group covers the fields it declares. Other fields of the same form are still validated by the submit (it is
  the scope's submit), but their values do not appear in `v`.
- No `Validated` for a group: a group cannot be one argument of `Form.submit(a, b)`, and a child group's own
  `submit` validates only its child scope.
- The array snapshot is taken when the submit starts; a row added while an async rule is in flight is validated by
  the next submit, not this one.

## Open questions

- Should `fields` replace `declare` as the way to declare (one style), or sit beside it?
- Name shadowing: keep `submit`/`scope` as members, or move them off the `Selectable`?
- Should `FieldArray`'s builder-supplied row UI go, now that a typed array renders with `render(row => ...)`?
- `GroupArray.satisfy` gives typed rows to cross-row checks; should `Form.satisfy` get a group form too
  (`f.satisfy(code)(v => ...)` over the validated tuple)?

## Recommendation

**Pursue.** The reference example compiles as written, the handler gets exactly the validated types, the
mechanism reuses W32's single-run verdict without touching its logic, and it needed three small private hooks in
existing code. The type-level part is two small type classes and no casts, and the compiler messages are the
ordinary ones a Scala user already reads. Before shipping it needs the shadowing decision above, a README section,
the CLAUDE.md rule updated, and a demo page; the one real cost is that the group's own type in an error message
is long.
