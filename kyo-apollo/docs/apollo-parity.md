# Apollo Client 4 → kyo-apollo parity map

> **Last verified:** 2026-07-29, against this repository (`fnd/integration`). Reference surface:
> **Apollo Client 4** (react docs + the GraphQL Conf 2025 "Apollo Client 4" talk). The deliberate
> deviations register lives in [apollo-4-alignment.md §1.2](apollo-4-alignment.md) — deviations are
> recorded there once, so this map does not re-litigate them.

> **Headline:** kyo-apollo is a port of Apollo **Kotlin**, not Apollo **JS**, so it covers the whole
> client engine — queries, mutations, subscriptions, all fetch policies, a full normalized cache
> (type/field policies, redirects, optimistic updates with layered rollback, eviction, GC, TTL/LRU,
> watchers), both WebSocket protocols, and APQ / batching / retry / auth / logging interceptors.
> The `kyo-apollo-ui` binding carries the React-convenience layer, and the 2026-07 alignment pass
> (P1–P5) brought the surface to Apollo Client 4 semantics: one error channel, fragment colocation
> **with data masking**, the preload/suspense family under honest names, and the streaming
> completeness marker. On `@stream` kyo-apollo is *ahead* of 4.0 (Apollo ships it in 4.1).

## How to read this

- **PRESENT** — implemented and (almost always) test-covered. Cited as `symbol · path`.
- **PARTIAL** — exists with a caveat, or only a lower-level primitive is exposed.
- **ABSENT** — no implementation (verified by targeted search).
- **N/A (by design)** — a documented deviation; see the alignment doc's register.

The mapping model: Apollo-React expresses data-fetching through hooks bound to a render lifecycle;
kyo-apollo expresses the same through kyo effects (`… < (Async & …)`) and kyo-ui `Signal`s. So
`useQuery`'s `{ data, dataState, error }` becomes a `Signal[QueryState[D]]`, and a one-shot fetch a
`D < (Async & Abort[ApolloException])`.

Paths are relative to the repository root; the modules are `kyo-apollo` (core, shared sources),
`kyo-apollo-ui`, `kyo-apollo-testing`, `kyo-apollo-codegen`.

---

## 1. Operations / hooks

| Apollo Client 4 | kyo-apollo | Status | Entry point |
|---|---|---|---|
| `useQuery` | `Apollo.query(call)` → `RawQueryHandle{state, networkStatus, refetch, startPolling/stopPolling, subscribeToMore}`; or `Apollo.watchSignal(call)` → `Signal[QueryState[D]]` | **PRESENT** | `kyo-apollo-ui/src/main/scala/kyo/apollo/ApolloQuery.scala`, `Apollo.scala` |
| `useLazyQuery` | `Apollo.lazyQuery(call)` → `LazyQueryHandle{state, load}` | **PRESENT** | `ApolloQuery.scala` |
| `useMutation` (incl. its kept `onCompleted`/`onError`) | `Apollo.mutation(call \| input => call)` → `MutationHandle{state, run, reset, onCompleted, onError}` | **PRESENT** | `ApolloMutation.scala` |
| `useQuery` callbacks (removed in V4) | removed here too — tap `state` (`state.current`/`state.next`/`observe`), the primitive they were built on | **N/A (by design)** | alignment doc §1.3 C5 |
| `useSubscription` | `sub.subscribeSignal` / `(skip, mode)` → `Signal[QueryState[D]]`; `.stream` for the raw feed | **PRESENT** | `ApolloSignal.scala`; `kyo-apollo/shared/…/network/ws/` |
| `useFragment` | `Apollo.fragment(fragment, cacheKey)` → `Signal[Maybe[D]]`; **masked form**: `Apollo.fragment(ref)` → total `Signal[D]` | **PRESENT** | `kyo-apollo-ui/src/main/scala/kyo/apollo/Apollo.scala` |
| `useSuspenseQuery` | `Signal[QueryState[D]].dataSignal` — awaits the first settled state (failure aborts on the Apollo channel), then follows; escalating overload routes tail failures through node-scope supervision | **PRESENT** | `ApolloSignal.scala` |
| `preloadQuery` + `useReadQuery` / `useBackgroundQuery` | `Apollo.preload(call)` → `PreloadedQuery{read, state}` — fetch starts at preload, `read` is the suspense form, `state` never suspends; parallel loader = no waterfall (pinned by test) | **PRESENT** | `ApolloPreload.scala`, `Apollo.scala` |
| `useSuspenseFragment` | `Apollo.fragment(ref)` *is* the analog — and total: the ref carries its response slice, so there is no pending phase left to suspend through | **PRESENT** | `Apollo.scala` |
| `useLoadableQuery` / `useQueryRefHandlers` | suspense-with-manual-trigger / queryRef plumbing — `Apollo.lazyQuery` + `PreloadedQuery` cover the use cases; no distinct API | **N/A (by design)** | — |
| `useApolloClient` | `given ApolloClient` in scope enables `op.call` | **PRESENT** | `kyo-apollo/shared/src/main/scala/kyo/apollo/OperationCall.scala` |
| `useReactiveVar` / `makeVar` | `reactiveVar(initial)` → `ReactiveVar[A]` (= writable `Signal.SignalRef[A]`) | **PRESENT** | `ApolloReactiveVar.scala` |

## 2. The V4 error model

| Apollo Client 4 | kyo-apollo | Status | Entry point |
|---|---|---|---|
| One `error` property (no `errors`+`networkError` split) | `ApolloResponse.error: Maybe[ApolloException]` — the single channel; typed GraphQL errors stay reachable via the `errors` projection | **PRESENT** | `kyo-apollo/shared/…/network/ApolloResponse.scala` |
| Granular error types (`CombinedGraphQLErrors`, …) | sealed `ApolloException` hierarchy; `ApolloGraphQLException.getMessage` follows the JS `CombinedGraphQLErrors` formatter verbatim | **PRESENT** | `…/exception/ApolloException.scala` |
| "never guess whether you need a try/catch" | the `.data` / `.response` split: `.data` is strict on `Abort`, `.response` is total values | **PRESENT** (different route) | `kyo-apollo-ui/…/ApolloEffect.scala`; deviation register |
| keep-last-data on a mid-stream failure | `QueryState.Failure(exception, last: Maybe[D])` — the signal drivers retain the last good data; views render stale data behind a banner | **PRESENT** | `ApolloSignal.scala` (`retainingData`) |
| `dataState: "streaming"` | `QueryState.Success(data, fromCache, complete)` — `complete = false` while incremental payloads are outstanding | **PRESENT** | `ApolloSignal.scala`; `…/runtime/IncrementalAssembler.scala` |
| GraphQL-over-HTTP: parse non-2xx `application/graphql-response+json` bodies | done; legacy `application/json` non-2xx still degrades to the status exception | **PRESENT** | `…/network/http/HttpNetworkTransport.scala` |

## 3. Fragments: colocation + data masking (the V4 headline)

| Apollo Client 4 | kyo-apollo | Status | Entry point |
|---|---|---|---|
| Colocated fragments | `Fragment.entity[T](_.fields)` declared next to the component; fragments are plain values you import (no registry needed) | **PRESENT** | `kyo-apollo/shared/…/cache/normalized/api/MaskedFragment.scala`, `Fragment.scala` |
| Data masking | `.spread` contributes ONE opaque `fields.Ref` element; the fragment's fields are `private[apollo]` on the ref — only `Apollo.fragment(ref)` opens them | **PRESENT** | `MaskedFragment.scala` |
| Fragment identity / keying | `CacheIdentity[T]` given, string-free (`CacheIdentity.by(_.code)`); codegen-emitted `SchemaIdentities.generator` collects the givens at client build; entity refs never capture positional path keys | **PRESENT** | `CacheIdentity.scala`; `kyo-apollo-codegen/…/ApolloClientWriter.scala` |
| Value-object fragments | `Fragment.embedded[T]` — ref carries the value, updates flow through the parent's reactivity (Apollo has no analog) | **PRESENT (ahead)** | `MaskedFragment.scala` |
| Cache-access fragments (`readFragment`/`writeFragment`) | `Fragment.of[T](_.fields)` (type name from the `TypeName` given, not a string) + `ApolloStore.readFragment`/`writeFragment` | **PRESENT** | `Fragment.scala`, `ApolloStore.scala` |
| `@nonreactive` | not needed: kyo-ui is fine-grained reactive per `Signal` | **N/A (by design)** | deviation register |
| Named fragment definitions in the printed document | spreads splice flat (server merges per GraphQL §5.3.2); named definitions deferred until document size / APQ hit rate hurts | **ABSENT (deferred)** | alignment doc §3 conditional |
| Codegen inline fragments (`... on T`) | deferred emitter feature | **ABSENT (deferred)** | — |

## 4. Caching / normalization

| Feature | Status | Entry point (`kyo-apollo/shared/…/cache/normalized/`) |
|---|---|---|
| Normalized cache, `MemoryCache` (LRU/TTL/max-age) | **PRESENT** | `NormalizedCache.scala`, `MemoryCache.scala` |
| Cache keys (`dataIdFromObject`), type policies (`keyFields`) | **PRESENT** | `api/CacheKeyGenerator.scala`, `api/TypePolicy.scala`, `api/CacheIdentity.scala` |
| Field policies + merge functions, cache redirects | **PRESENT** | `api/FieldPolicy.scala`, `api/FieldPolicies.scala`, `api/CacheKeyResolver.scala` |
| `readQuery`/`writeQuery`, `updateQuery` | **PRESENT** | `ApolloStore.readOperation`/`writeOperation`/`updateOperation` |
| `cache.modify` | **PARTIAL** | typed whole-record `updateFragment`; field-granular modify is N/A (records have no typed field access) |
| `cache.evict` / gc / `extract` | **PRESENT** | `ApolloStore.evict`/`garbageCollect`/`extract`; gc publishes what it removed |
| `cache.retain` / `cache.release` | **PRESENT** | `ApolloStore.retain` (released with its `Scope`); a live `watch()` retains the keys of its last read |
| Watchers (reactive reads), optimistic responses | **PRESENT** | `Watcher.scala`, `OptimisticUpdates.scala` |
| `resetStore` / `onResetStore`, `refetchQueries` | **PRESENT** | `ApolloClient` + `ActiveQueryRegistry` |

## 5. Fetch policies & fetching behaviors

`enum FetchPolicy { CacheFirst, NetworkOnly, CacheOnly, NetworkFirst, CacheAndNetwork, NoCache,
Standby }` · `FetchPolicy.scala`; watchers add `RefetchPolicy { CacheOnly, NetworkOnly, CacheFirst }`
(the `nextFetchPolicy` equivalent). All V3/V4 policies **PRESENT**. apollo-kotlin's `refetchPolicy`
takes the whole `FetchPolicy`; these three are the ones a watcher has a distinct meaning for, and
widening to `FetchPolicy` would break every call site and collide in the context bag.

| Feature | Status | Note |
|---|---|---|
| `refetch`, `skip` (Freeze/Unsubscribe), polling (declarative + imperative), `subscribeToMore` | **PRESENT** | `ApolloQuery.scala`, `ApolloSignal.scala` |
| `errorPolicy` (`none`/`ignore`/`all`) | **PRESENT** | GraphQL errors only, on `.data` — transport failures never suppressed (deviation register) · `ErrorPolicy.scala` |
| `notifyOnNetworkStatusChange` default ON (V4) | **PRESENT** | `QueryHandle.networkStatus` always overlays `Refetch`/`Poll` |
| no `ObservableQuery` tracking of unsubscribed queries (V4) | **PRESENT** | `ActiveQueryRegistry.register` is a `Scope.acquireRelease` pair; the enclosing `Scope` removes the entry |

## 6. Subscriptions & network

| Feature | Status | Entry point |
|---|---|---|
| WS transport, both protocols, multiplexing, reconnect (`reconnectWhen` + backoff), `connection_init` payload | **PRESENT** | `kyo-apollo/shared/…/network/ws/WebSocketNetworkTransport.scala` |
| Class-based links (V4: no class/function split) | **PRESENT** | every interceptor is a `final class` · `…/interceptor/` |
| APQ, HTTP batching, retry (transport-only, jittered backoff), auth, logging | **PRESENT** | `…/interceptor/*.scala` |
| `@defer` (multipart, `deferSpec=20220824`) | **PRESENT** | `.deferred` / `defer(label, …)` · `api/SelectionBuilder.scala`; `IncrementalAssembler` |
| `@stream` (Apollo ships in 4.1) | **PRESENT (ahead)** | `.streamed(initialCount)`, type-preserving; `JsonPath.spliceItems` |
| GraphQL-17-alpha9 incremental format (`pending`/`completed`) | **ABSENT (conditional)** | adopt when a server we talk to emits it · alignment doc §3 |
| Multipart file upload | **PRESENT** | platform-neutral `Upload(Span[Byte], …)`; browser bridge `UploadJs.fromFile` |
| Devtools (official extension) | **PRESENT** | `builder.connectToDevtools(name, enabled)` (no default for `enabled`) · `…/devtools/` |

## 7. Local state, pagination, codegen, testing

| Feature | Status | Entry point |
|---|---|---|
| `@client` fields in the normalized cache (typed, schema-shaped) | **PRESENT** | `ClientField.scala`, `api/ClientFieldStructure.scala` |
| Batched cache writes with one broadcast (Apollo Client's `cache.batch`) | **PRESENT** | `ApolloStore.writeFragments`, `ClientField.writeAll` — N keys, one merge, one changed-keys publish, so a watcher re-reads once instead of N times |
| Relay-style connection merge + imperative `fetchMore` | **PRESENT** | `api/FieldPolicy.scala` (`ConnectionFieldPolicy`); `ApolloPagination.scala` |
| A paginated query carries the same `skip` gate as a plain one | **PRESENT (ahead)** | react has no paginated hook to skip; `Apollo.paginatedQuery(initial, skip, mode)(page)` parks one and keeps its accumulated pages |
| Codegen: schema types + selectors, no per-operation hooks (the V4-echoed foot-gun) | **PRESENT** | `kyo-apollo-codegen/…/ApolloClientWriter.scala` |
| Codegen: `SchemaIdentities` (schema-wide `CacheIdentity` collector) | **PRESENT** | `ApolloClientWriter.emitSchemaIdentities` |
| Colocated/namespaced types (V4's `useLazyQuery.Options` idea) | **PRESENT** | `QueryState`, `MutationState`, `NetworkStatus`, `Apollo.*` |
| Mocked transports (`MockedProvider` analog) | **PRESENT** | `kyo-apollo-testing`: `TestHttpEngine`, `MockServer`, `TestNetworkTransport`, `MockWebSocketServer`, `StreamProbe` |

---

## Gap summary

Everything Apollo Client 4.0 ships is covered, matched by a documented deviation, or explicitly
deferred with a trigger:

- **Named fragment definitions in the printed document** — deferred until document size or
  persisted-query hit rates hurt; the flat splice is spec-correct.
- **GraphQL-17-alpha9 incremental format** — adopt when a server we actually talk to emits it
  (check what Caliban emits first).
- **`Schema[Ref]` for `mapInto` targets** — considered and declined (2026-07-29): the colocation
  idiom is named-tuple-shaped, and a view model embedding an opaque ref is a skewed pattern.
- **Field-granular `cache.modify`** — N/A by design (typed `updateFragment` is the replacement).

## Provenance

Statuses derive from the alignment pass recorded in [apollo-4-alignment.md](apollo-4-alignment.md)
(every claim there was read at the cited location or executed), the module test suites
(JVM + JS), and the demo repository's showcase/specs — including `MaskedColocationSpec`, which
exercises identity, masking, spread labeling, preloading and the streaming marker against the
generated selectors end to end. The Apollo Client 4 surface is taken from the official docs and
the GraphQL Conf 2025 talk. This document supersedes the standalone repo's
`docs/comparison/apollo-react-parity.md` (last verified 2026-07-13 against Apollo-React v3), whose
per-feature history remains available in that repo's git log.
