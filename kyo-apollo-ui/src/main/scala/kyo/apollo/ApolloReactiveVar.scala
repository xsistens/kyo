package kyo.apollo

// getkyo.io library. This binding shares `package kyo.apollo` with `core` (the
// compat model — like `kyo-http` joining `kyo-core`'s `package kyo`), so
// `import kyo.*` resolves to the library root and pulls in Kyo's
// `<` / `Sync` / `Signal` / `Signal.SignalRef` / `Frame`.
import kyo.*

/** react-apollo's reactive variables (`makeVar` / `useReactiveVar`), as the
  * idiomatic kyo primitive they already are (Step 7a of the react-parity build).
  *
  * A [[kyo.Signal.SignalRef]] *is* a writable [[kyo.Signal]]: it holds a cell of
  * local `@client` state, re-emits to every observer when the value changes, and
  * offers atomic writes. So react's three reactive-var operations map with no
  * wrapper:
  *
  *   - `v()`               → `rv.current`      — one-shot read
  *   - `useReactiveVar(v)` → `rv` / `rv.map`   — the ref itself *is* the `Signal`
  *   - `v(x)`              → `rv.set(x)`        — plus atomic `updateAndGet` / `compareAndSet`
  *
  * [[ReactiveVar]] is therefore a *naming* alias, not a new type: it adds no
  * capability over `SignalRef`, only the familiar react vocabulary and a
  * constructor. Unlike a query watcher's `Signal` (which owns a cache
  * subscription to tear down), a reactive var is a plain cell — so [[reactiveVar]]
  * is a bare `Sync` effect with **no `Scope`**. Create it once, share the handle,
  * and hand its read-only `Signal[A]` view down (any sink typed `Signal[A]`
  * cannot write — the upcast is the capability boundary).
  *
  * {{{
  * import kyo.apollo.*
  *
  * Scope.run:
  *   for
  *     filter <- reactiveVar(Filter.All)              // local @client state
  *     _ <- render(filter.map(f => s"Filter: $f"))    // reactive read — a Signal[Filter]
  *     _ <- button("Active only").onClick(filter.set(Filter.Active))
  *     _ <- button("Toggle").onClick(filter.updateAndGet(_.toggle))
  *   yield ()
  * }}}
  */
type ReactiveVar[A] = Signal.SignalRef[A]

/** Create a [[ReactiveVar]] seeded with `initial` — react-apollo's `makeVar`.
  *
  * A bare `Sync` effect (no `Scope`): the cell owns no subscription, so there is
  * nothing to release. Run it once at setup and share the handle; pass its
  * `Signal[A]` view to consumers for read-only reactive access. `A` needs a
  * `CanEqual[A, A]` (as every `Signal` does) so change detection can compare.
  */
def reactiveVar[A](initial: A)(using Frame, CanEqual[A, A]): ReactiveVar[A] < Sync =
    Signal.initRef[A](initial)
