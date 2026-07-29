# Apollo Client 4 alignment plan

Status: **P1 and P2 landed; P3–P6 outstanding.** Last updated 2026-07-29.

Source of the comparison: "Apollo Client 4" by Gerald Miller (Apollo client-team maintainer),
GraphQL Conf 2025 — <https://www.youtube.com/watch?v=QNzziV0L9Ks>.

The goal is **not** to reproduce Apollo Client's API. It is to make sure every *benefit* Apollo
Client 4 delivers is matched or beaten here, and to record — once — the places where kyo-apollo
reaches that benefit by a different route, so future audits stop re-reporting them as gaps.

Apollo Client 4 is explicitly not a feature release. It is a consistency/modernization pass plus a
set of recommendations about how the client *should* be used. Roughly half of it describes problems
kyo-apollo never had, because it ports Apollo Kotlin rather than Apollo JS and because an effect
system dissolves several of the React-specific concerns outright.

---

## 1. Verdicts

### 1.1 Already aligned, or ahead

| Apollo Client 4 change | Status here | Evidence |
| --- | --- | --- |
| Granular error types; stop wrapping every failure in `ApolloError` | Aligned | `exception/ApolloException.scala` — sealed subtypes; `ApolloGraphQLException` follows the JS `CombinedGraphQLErrors` message contract verbatim |
| All links class-based (`new`), no class/function split | Aligned | every interceptor in `interceptor/` is a `final class` |
| Stop tracking unsubscribed `ObservableQuery` (leaks, surprise refetches) | Aligned | `ActiveQueryRegistry.register` returns a dispose thunk; `ApolloQuery.scala:358` disposes via `Scope.ensure` |
| React APIs moved out of the top-level entry (`/react`) | Aligned | `kyo-apollo` core carries no UI dependency; the binding is the separate `kyo-apollo-ui` module |
| Stricter `variables` typing | Ahead | arguments are typed parameters on generated selectors — a missing required variable is a compile error, not an overload heuristic |
| `dataState` narrowing so `data` is not `T \| undefined` | Ahead (structurally) | `QueryState.Success(data: D, …)` — data is non-optional in the success arm; the enum narrows |
| Colocated/namespaced types (`useLazyQuery.Options`, …) | Aligned | `QueryState`, `MutationState`, `NetworkStatus`, constructors under `Apollo.*` |
| No codegen-generated per-operation hooks (the foot-gun Miller calls out) | Aligned | codegen emits schema types + selectors only (`ApolloClientWriter.emitSelectors`) |
| `notifyOnNetworkStatusChange` defaulting on | Aligned | `QueryHandle.networkStatus` always overlays `Refetch`/`Poll` |
| `@defer` | Aligned | selection combinators, multipart transport, `IncrementalAssembler` |
| `@stream` (Apollo ships this in 4.1) | **Ahead** | `StreamDirective`, `JsonPath.spliceItems`, end-to-end `StreamSpec` |
| Multipart upload, devtools | Aligned | `Upload.scala`, `devtools/ApolloDevtools.scala` |
| Suspense family | Aligned under a different name | `Signal[QueryState[D]].dataSignal` (`ApolloSignal.scala:186`) is `useSuspenseQuery`: it awaits the first settled state, suspends the effect, then follows. The escalating overload routes tail failures through node-scope supervision. |

### 1.2 Deliberate deviations — do not "fix" these

**`@nonreactive` — not adopted.** It exists because React re-renders the whole component on any
change to the query result; the directive carves nested fragments out of that. kyo-ui is
fine-grained reactive per `Signal`, so there is no whole-component re-render to carve out of. The
benefit is already paid for.

**Fragment registry — not adopted.** It solves a JS problem: fragments are template strings that
must be interpolated into operations, producing import chains. Here fragments are values you
import. No problem, no solution needed.

**`errorPolicy` covering transport errors — not adopted as such.** Apollo Client 4's goal is "you
should never have to guess whether you need a try/catch", achieved by resolving the promise even on
network failures. kyo-apollo reaches the same end through the `.data`/`.response` split:
`.data: D < Abort[ApolloException]` *cannot* return "no data plus an error", and `.response`
returns everything as values. `ErrorPolicy` therefore applies to GraphQL errors on `.data` only —
this is documented behaviour, not an oversight (`ErrorPolicy.scala:29`). The one genuine
user-visible piece of the V4 change is tracked separately as C2 below.

### 1.3 Gaps to close

| # | Gap | Evidence |
| --- | --- | --- |
| C1 | `ApolloResponse` carries `errors` *and* `exception` — the split V4 collapsed into one `error` | `network/ApolloResponse.scala:38-46` |
| C2 | A transport failure mid-stream overwrites the last good data, blanking the UI | `ApolloSignal.driveGated` — `ref.set(project(resp))` is unconditional (`ApolloSignal.scala:313-318`) |
| C3 | Non-2xx always becomes an opaque `ApolloHttpException`; the body is never parsed | `network/http/HttpNetworkTransport.scala:162` |
| C4 | No way to tell a still-growing `@stream`/`@defer` result from a complete one | `QueryState` has no streaming marker; `IncrementalAssembler` knows `hasNext` but does not surface it |
| C5 | `QueryHandle.onCompleted`/`onError` — removed from `useQuery`/`useLazyQuery` in V4 | `ApolloQuery.scala:237,243`; **zero call sites**, so removal needs no migration. `MutationHandle`'s stay: V4 keeps them on `useMutation`. |
| C6 | Fragment colocation and data masking absent | `Fragment[D]` is a cache read/write unit only; no spread into an operation, no masking |
| C7 | Only `deferSpec=20220824` + legacy incremental formats | `HttpRequestComposer.scala:202`; no GraphQL-17-alpha9 `pending`/`completed` |
| ~~C8~~ | ~~Three divergent cache-key configurations for one schema~~ — **withdrawn, the finding was wrong**; see below | — |
| C9 | Redundant type-name string on `Fragment.of` | `TypeName[Origin]` already exists and codegen already emits it (`ApolloClientWriter.scala:355`); `ClientField.create:136` already summons it — `Fragment.of` just does not |

Also worth noting: `QueryState.PartialData` means "data plus GraphQL errors", whereas V4's `partial`
means "incomplete cache read". Same word, different meaning. There is no `returnPartialData` mode
here, so the two never collide in practice — documented rather than renamed.

**C8 withdrawn (2026-07-29).** Counted properly while implementing P2, the divergence does not
exist. Every `Country` configuration across both repos keys by `code` — 33 sites, no exceptions;
`IdCacheKeyGenerator(List("code"))` and `TypePolicyCacheKeyGenerator.of(TypePolicy("Country",
List("code")))` are two spellings of one key choice, and the original audit read the spelling
difference as a configuration difference. The third site, `KyoTestSupport.scala:109`
`IdCacheKeyGenerator(List("id"))`, keys a `User` entity by `id`, which is correct; the enclosing
object is merely misnamed `CountryFixture` and defines no `Country` type at all. Nothing to unify.

---

## 2. The fragment design (C6)

The headline recommendation of the talk. Its benefit decomposes into four parts that stand very
differently here:

| Benefit | Status |
| --- | --- |
| Component declares its own data dependency | Possible today (selections are values) but **not practised anywhere** |
| Data masking — the parent cannot see the child's fields | **Missing** |
| Child re-renders only when *its* data changes | Already solved by per-`Signal` reactivity + `Apollo.fragment` |
| Named fragments in the document (smaller documents, better APQ hit rate) | Missing, low value |

So the work is masking, plus making colocation attractive enough to become the idiom.

### 2.1 Shape

Two constructors. The choice is made when the fragment is declared, never inferred at runtime:

```scala
object CountryCard:
    val fields = Fragment.entity[Country](_.name.capital)   // needs `using CacheIdentity[Country]`

    def view(ref: fields.Ref)(using ApolloClient, Frame) =
        Apollo.fragment(ref).map(_.render { c => UI.div(c.name, c.capital) })

object PageBadge:
    val fields = Fragment.embedded[PageInfo](_.hasNextPage.endCursor)   // no identity required
```

Spreading contributes exactly one named-tuple element — the ref — regardless of how many fields the
fragment selects:

```scala
val countryPage =
    Queries.country("DE")(Country.code ~ CountryCard.fields.spread).toQuery
//  : (code: String, countryCard: CountryCard.fields.Ref)
//    `name` and `capital` are unreachable from here
```

`FragmentRef` keeps its contents `private[apollo]`; only `Apollo.fragment` opens it. `ref.key:
CacheKey` is deliberately public — identity is not the masked data, and it is useful as a list key
for UI reconciliation.

### 2.2 Why the ref is not a case class you navigate

The parent must not reach the child's fields; that is the entire feature. Giving the ref a `Schema`
(§2.5) describes how it round-trips through the cache, and adds no field accessors. When a parent
legitimately needs a field the child also shows, it selects that field itself
(`Country.name ~ CountryCard.fields.spread`) — fetched once, but the parent's dependency is
explicit rather than borrowed.

### 2.3 Entity vs embedded, and why path keys are rejected

There is no keyless case in the normalizer: `Normalizer.scala:155-157` ends in
`.getOrElse(CacheKey.fromPath(path))`. Every object gets a key, of one of two kinds.

- **Entity key** (`Country:DE`) — identity-stable. Any query, any order, any list index resolves to
  the same record.
- **Path key** (`QUERY_ROOT.countries.3`) — positional, *including the list index*
  (`Normalizer.scala:140`). Harmless today because reads always traverse from the root and follow
  `CacheReference`s (`CacheBatchReader.scala:144`). A `FragmentRef` would **capture** such a key and
  read it independently later — at which point a list insertion silently repoints it at a different
  entity.

The original draft offered `CountryEdge` as a live instance of this hazard. It is not: every demo
configuration that fetches a connection registers `TypePolicy("CountryEdge", List("cursor"))`, and
every configuration that omits it never fetches an edge. The constraint below stands on its own —
it is about what `.spread` must refuse to produce, not about an existing bug.

Therefore: **`.spread` never produces a path-keyed ref.** `Fragment.entity` requires a declared
identity; `Fragment.embedded` carries the decoded value instead of a key, and its updates flow
through the parent's reactivity — which is the semantically correct behaviour for an object with no
independent identity, and something Apollo does not support at all.

### 2.4 Identity is declared once, without strings

Raw strings were removed from all three sites they appeared in:

```scala
// Field name: a generated selector, so a typo is a compile error and the field provably exists.
// A duplicate `given` for one type is an ambiguous implicit — Scala rejects it for free.
object CacheIdentities:
    given CacheIdentity[Country]     = CacheIdentity.by(_.code)
    given CacheIdentity[CountryEdge] = CacheIdentity.by(_.cursor)

// The client's table: codegen emits the enumeration of schema object types; `collect` is `inline`
// and expands at the call site, where the user's givens are in scope.
import CacheIdentities.given
Apollo.client { _.serverUrl(endpoint).normalizedCache(MemoryCache(), SchemaIdentities.generator) }
```

- **Type name** — from `given TypeName[Origin]`, which already exists and is already emitted.
- **Key field** — from the generated selector via `CacheIdentity.by`.
- **Fragment name** — derived from `Frame` (`className`/`callerName`, `Frame.scala:95`). Because it
  is the fully-qualified declaration site, it is globally unique by construction: neither typos nor
  name collisions are expressible.

`CacheIdentity` is **one per type** (identity). `Fragment.entity[Country]` is **many per type**
(what each component needs) and involves no given. Multiple fragments per type are the normal case:

```scala
object CountryRow:    val fields = Fragment.entity[Country](_.name.emoji)
object CountryFlag:   val fields = Fragment.entity[Country](_.emoji)         // overlaps
object CountryDetail: val fields = Fragment.entity[Country](_.name.capital)  // overlaps
```

### 2.5 Overlapping fragments merge correctly — verified

Three layers, all union-shaped:

1. **Collection** — `FieldCollector` merges fields sharing a response name at collection time
   (`internal/FieldCollector.scala:14,44`), before printing and before normalization.
2. **Field keys** — `FieldKey` uses the schema name, *not* the alias ("two aliases of the same
   field+arguments address the same stored value"), and canonicalizes arguments by recursively
   sorting object keys. Different arguments partition into different slots and never clobber.
3. **Record merge** — `NormalizedCache.mergeRecords:112` and `FieldPolicyRecordMerger` are both
   `old.fields ++ incoming.fields`: old fields survive, incoming wins on collision. A later query
   with disjoint fields *extends* the record.

Covered by existing tests: `StoreSpec.scala:76` (union with mixed disjoint/overlapping fields),
`NormalizerSpec.scala:206` (one entity selected twice in one response merges to one record),
`StoreSpec.scala:86` + `ReactivitySpec.scala:177` (identical rewrite reports no change, so
overlapping fragments cause no spurious re-emits), `WatcherSpec.scala:137,165` (watcher re-emits on
dependent change, stays silent otherwise).

**Not covered — must be added in P3:** the cross-operation composition. Query A writes
`Country:DE {code,name}`, query B then writes `Country:DE {code,capital}`, and a fragment selecting
`{name,capital}` reads successfully from the accumulated record while A's watcher survives B's
write. Every ingredient is tested in isolation; the composition is not, and it is precisely the path
masked components live on.

### 2.6 Named-tuple duplicates

`SelectionBuilder.Combine` does not deduplicate: `arity = left.arity + right.arity`,
`selections = left.selections ++ right.selections`. Verified against Scala 3.8.4 that
`NamedTuple.Concat` with a duplicate label compiles, keeps both elements (arity 3), and resolves
selection to the **first** occurrence:

```
arity   = 3
c.name  = ERSTES-name
c.emoji = DE
```

This does **not** reach `mapInto`, which decodes the raw response JSON through the target's own
`Schema` rather than the tuple — and the response carries each field once (the server merges
duplicate selections per GraphQL §5.3.2). Under masking the question dissolves anyway: each spread
contributes one element, so arity tracks the number of fragments, not the number of fields.

### 2.7 Open design items for P3

- **`Schema[FragmentRef[D]]`** — decided: option (a), the ref encodes as its entity key fields, so
  `mapInto` stays bidirectional over spread-containing selections. `.spread` forces the key fields
  into the document, so the encoding is well defined. Detailed shape still to be worked out.
- **`fields.Ref` as a path-dependent type** rather than `FragmentRef[fields.Data]`, so a ref cannot
  be handed to a different fragment whose field set is a superset (which would otherwise be a
  runtime cache miss).
- **Cache-miss diagnostics under masking** — "fragment CountryCard needs field `capital` on
  `Country:DE`; the record has `code`, `name`. Was the fragment spread into the query that fetched
  it?" rather than today's generic text.
- **Interfaces and unions** — the runtime `__typename` may differ from the declared type condition;
  the ref must take the type name from the response, not the declaration.
- **Double spread of one fragment into one query** — produces two tuple elements with the same
  label, not reported. Harmless (fragments merge in GraphQL). Could be caught with a
  `NamedTuple.Names` disjointness constraint on `~`; needs verification against the type system
  before being promised.

---

## 3. Phases

Sequential, each green and committed before the next begins.

**P1 — Response consolidation.** C1. First, because every later phase touches the same projection
sites (`ApolloSignal.project`, `MutationState.fromResponse`, `ApolloEffect.projectData`) and doing
this afterwards means doing that work twice. Single `error: Maybe[ApolloException]`; drop
`dataOrThrow()`, `dataAssertNoErrors()`, `exceptionOrNull` (throwing/nullable Kotlin-Java interop
forms in a value-based effect API). No backwards compatibility is required.

**P2 — Small corrections.** C5 (remove the query callbacks), C3 (parse non-2xx bodies when the media
type is `application/graphql-response+json`, per the GraphQL-over-HTTP spec), C2 (keep the last good
data on a transport failure — `Failure(exception, last: Maybe[D])`, so a view can render data plus
an error banner), C9 (drop the `Fragment.of` type-name parameter in favour of the existing
`TypeName` given). Each is independent and narrowly scoped. C8 was withdrawn during this phase —
see §1.3.

**P3 — Fragment masking.** C6, per §2. The only phase with a real design round up front: the
signatures above plus the open items in §2.7. Ends with a demo that establishes colocation as the
idiom — without it the API stays unused, exactly as the shared-selection pattern is today.

**P4 — Suspense naming.** `Apollo.preload` / `PreloadedQuery` (`read` suspends until first data,
then follows the watcher; `state` does not suspend) as the `preloadQuery` + `useReadQuery` analog,
plus `Apollo.fragmentData` (the `useSuspenseFragment` analog, depends on P3), plus a router-loader
example showing parallel preloading instead of waterfalls.

**P5 — Streaming marker.** C4. `Success(data, fromCache, complete: Boolean = true)` — a default
field rather than a separate enum case, so the ~90 % of queries that never stream are unchanged.
Requires surfacing `hasNext` from `IncrementalAssembler` onto `ApolloResponse`. Demo card with a
growing list.

**P6 — Documentation.** Move the parity map here from the (now demo-only) standalone `kyo-apollo`
repo, rewrite it against Apollo Client 4, and keep §1.2 of this document as the standing deviations
register.

**Conditional, with triggers rather than dates.** C7 — put `IncrementalAssembler` behind a small
interface now (Apollo's own pluggable-handler approach) and add the alpha9 `pending`/`completed`
format when a server we actually talk to emits it; check what Caliban emits in the gateway first.
Named fragment definitions in the printed document — only when document size or persisted queries
start to hurt; §2.1 does not block it.

---

## 4. Method

Every claim above was read out of the source at the cited location, or run. Specifically **run**:
the Scala 3.8.4 named-tuple duplicate-label probe in §2.6. Specifically **not** run: the existing
cache specs cited in §2.5 — they were read, not executed, which is part of why the composition test
is called out as missing rather than assumed to pass.

Repository state at the time of writing: `kyo-integration` @ `f29b6f72`, branch `fnd/integration`.
The standalone `kyo-apollo` repo has been demo-only since `674361b`.
