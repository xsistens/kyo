# kyo-apollo

A GraphQL client for kyo, modelled on Apollo Client / apollo-kotlin: an inline
selection DSL (`SelectionBuilder`), a normalized cache with declarative
policies, incremental delivery (`@defer` / `@stream`), and `kyo-apollo-codegen`
for schema-driven selector objects. See `docs/` for the design notes.

## What a selection can do

What a selection supports is its type, so a misuse does not compile:

- Every `SelectionBuilder[Origin, A]` decodes a response (`decode` returns a
  `Result[ApolloParseException, A]`). A `SelectionBuilder.Bidirectional` also
  encodes: named-tuple selections and `mapInto[C]` projections are, `map(f)`
  projections are not.
- Only `SelectionBuilder.Fields` (named-tuple selections) combine with `~`.
- Only `SelectionBuilder.Deferrable` selections — whose last-added operand is a
  single field selector — take `.deferred` and `.streamed`; `.streamed` also needs
  that field to be a list. The empty selection, a `defer(...)` group, a union
  branch, a fragment spread and a `@client` field do not.
- A nested field takes a bidirectional child, so `map` projects the whole selection
  an operation is built from, never a child (use `mapInto` there).
- A bidirectional root builds a normalizable operation (`Query.Normalizable`, …),
  the only kind `ApolloStore.writeOperation`/`updateOperation` accept. A query built
  from a `map` projection runs and decodes normally, but the cache interceptor does
  not normalize its responses; it logs each skip at debug level.

## Type conventions

The module follows kyo's `CONTRIBUTING.md`: **model types and return types carry
kyo data types; the standard library appears only as input.**

- A nullable GraphQL field decodes to `Maybe[T]`, a list field to `Chunk[T]`
  (`SelectionBuilder.Nesting`, `ScalarCodec.maybe` / `ScalarCodec.chunk`); the
  named-tuple result of every query is therefore `Maybe`/`Chunk`-typed, and the
  codegen emits the same types for selectors, input case classes and
  argument defaults (`= Absent`).
- `CompiledField.alias`/`stream`, `CompiledFragment.defer`, `DeferDirective.if`,
  `StreamDirective.if` are `Maybe`; `selections`, `arguments`, `possibleTypes`
  are `Chunk`.
- `FieldPolicy(keyArgs: Maybe[Chunk[String]], read: Maybe[…], merge: Maybe[…])`;
  a `FieldValueMerger` receives the existing value as `Maybe[RecordValue]`.
- A union branch (`SelectionBuilder.onType`) decodes to `Maybe[A]`; a `@defer`
  group to `Maybe[S]`.
- The happy-path unwrap of a `Maybe` into a typed error channel is
  `maybe.orFailWith(e: E)` (`MaybeOps`, `E <: ApolloException`).
- Inputs that only collect values (`FieldPolicies.fromList(policies: Seq[…])`,
  `ClientField.writeAll(values: Seq[…])`) accept any `Seq`, `Chunk` included.

### Documented exceptions

- **`Map[String, Json]`** — a JSON object (`Json.JObj`) is a keyed map by
  definition; there is no kyo map type, and insertion order (which the printer
  and the cache rely on) is kept with `scala.collection.immutable.VectorMap`.
  The same holds for `variables: Map[String, Json]` and record field maps.
- **`Set[String]`** — the set of changed record keys published after a write
  (`NormalizedCache.merge`, `ChangedKeysSubject`, `ClientField.write*`) is a
  membership set with no ordering; kyo has no `Set`, so `scala.collection.immutable.Set`
  stays.

## Devtools

On Scala.js, `builder.connectToDevtools(name, enabled)` replaces the terminal
`build()` and, when `enabled`, connects the client to the Apollo Client Devtools
browser extension. `enabled` has no default; with `enabled = false` the call is
exactly `build()` and touches no global.

**Warning:** an installed hook exposes the entire normalized cache and every
operation's variables to every script in the document; pass `enabled = isDevBuild`.
The Mutations tab shows a mutation's variables with every value replaced by
`"<redacted>"`, and the Cache tab leaves out `ROOT_MUTATION`, whose keys spell out
a mutation's arguments. Query variables and query data are shown as they are.
