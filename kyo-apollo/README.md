# kyo-apollo

A GraphQL client for kyo, modelled on Apollo Client / apollo-kotlin: an inline
selection DSL (`SelectionBuilder`), a normalized cache with declarative
policies, incremental delivery (`@defer` / `@stream`), and `kyo-apollo-codegen`
for schema-driven selector objects. See `docs/` for the design notes.

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
