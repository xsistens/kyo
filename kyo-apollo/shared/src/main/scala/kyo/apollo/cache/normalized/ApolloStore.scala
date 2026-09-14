package kyo.apollo.cache.normalized

import kyo.<
import kyo.Abort
import kyo.Absent
import kyo.AllowUnsafe
import kyo.AtomicLong
import kyo.AtomicRef
import kyo.Chunk
import kyo.Frame
import kyo.Kyo
import kyo.Maybe
import kyo.Present
import kyo.Result
import kyo.Scope
import kyo.Sync
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.JsonCodec
import kyo.apollo.api.Operation
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.cache.normalized.internal.CacheBatchReader
import kyo.apollo.cache.normalized.internal.Normalizer
import kyo.apollo.exception.CacheMissException
import kyo.apollo.exception.CacheReadFailure
import kyo.apollo.exception.NoCacheIdentityException
import kyo.apollo.json.Json
import scala.collection.mutable

/** The coordinator that turns typed operation data into cache records and back,
  * over a pluggable [[NormalizedCache]] backend.
  *
  * `ApolloStore` is the single entry point the cache interceptor drives: it
  * normalizes a response into records and merges them ([[writeOperation]]),
  * reassembles typed data from the store ([[readOperation]]), and announces which
  * keys changed to any registered watchers ([[publish]]). All typed encode/decode
  * goes through the operation's own `dataSchema`, so a `writeOperation` →
  * `readOperation` round-trip yields data equal to what the network produced.
  * Mirrors apollo-kotlin's `ApolloStore` / `DefaultApolloStore`.
  *
  * Every operation that touches the store's state is an effect (`< Sync`). A read
  * runs against one state of the backend ([[NormalizedCache.read]]) and one
  * snapshot of the optimistic layers, so it never mixes two writes; an update
  * ([[updateOperation]], [[updateFragment]]) is one atomic
  * [[NormalizedCache.transact]], so two concurrent updates of the same data both
  * land.
  *
  * A read the cache cannot satisfy fails on its row, `Abort[CacheReadFailure]` —
  * the miss is the expected outcome this layer is built around (`CacheFirst`,
  * `NetworkFirst`, [[updateOperation]]), so it is a typed failure, never a throw.
  * A decode defect or a reader bug is not a miss: it is a panic, under every
  * handler.
  *
  * @param cache             the backing record store
  * @param cacheKeyGenerator the write-side object → [[CacheKey]] policy
  * @param cacheKeyResolver  the read-side field → [[CacheKey]] redirect policy
  * @param fieldPolicies     per-field declarative policies (connection
  *                          pagination keys, field read redirects, custom
  *                          merges); defaults to the identity policy
  * @param diagnostics       development-time warnings about ambiguous writes (see
  *                          [[CacheDiagnostics]]); silent by default
  */
final class ApolloStore(
    val cache: NormalizedCache,
    cacheKeyGenerator: CacheKeyGenerator = CacheKeyGenerator.default,
    cacheKeyResolver: CacheKeyResolver = CacheKeyResolver.default,
    fieldPolicies: FieldPolicies = FieldPolicies.empty,
    diagnostics: CacheDiagnostics = CacheDiagnostics.off
):

    /** The record-merge policy derived from [[fieldPolicies]]: a per-field merger
      * (e.g. connection edge unioning) when policies are configured, otherwise the
      * default field-wise union. The store's ONE answer to "two records under one
      * key": the [[Normalizer]] merges repeated keys of a response with it, a batch
      * of fragment writes merges repeated keys with it, and the backend merges every
      * commit with it — so merge policy lives with the store, not the storage layer.
      */
    private val recordMerger: RecordMerger = RecordMerger.fieldPolicies(fieldPolicies)

    /** The change-notification bus: a hot, multicast source of the changed-key set
      * of every write. Watchers [[ChangedKeysSubject.subscribe]] here to learn which
      * keys moved; every store write path funnels its changed keys through it via
      * [[publish]]. Exposed so a watcher can observe the store directly. Empty of
      * subscribers until a caller opts in.
      */
    val changedKeys: ChangedKeysSubject = new ChangedKeysSubject

    /** The store's write generation: incremented by every [[publish]] that carries
      * at least one changed key, *after* the write has landed in the backend and
      * *before* the changed keys fan out. A read that observed generation `g`
      * before it started is therefore current with respect to every write of
      * generation `<= g`; a write it may have raced lands as `> g`. A watcher keeps
      * the generation of the read behind its key set and compares it against
      * [[currentGeneration]] once that set is adopted — that is what turns "a write
      * slipped in between my read and my registration" from a lost notification
      * into a re-read.
      */
    private val generation: AtomicLong =
        AtomicLong.Unsafe.init(0L)(using AllowUnsafe.embrace.danger).safe

    /** The generation of the latest published write (0 while nothing has been
      * published). Monotone; see [[readOperationStamped]] for how a read is
      * compared against it.
      */
    def currentGeneration(using Frame): Long < Sync = generation.get

    /** One optimistic layer: the records (by key) a mutation wrote *optimistically*
      * before its network round-trip, tagged by the mutation's id.
      */
    final private case class Layer(mutationId: String, records: Map[CacheKey, Record])

    /** Optimistic record layers, stacked in application order (the `Chunk`'s
      * order *is* the stacking order: concurrent optimistic mutations resolve
      * latest-wins at read time). Reads overlay these on top of the persisted
      * cache so watchers see the optimistic state immediately, while the backing
      * [[NormalizedCache]] stays pristine — which is what makes rollback nothing
      * more than dropping a layer.
      *
      * Held as an immutable stack behind an atomic reference: a write swaps in a
      * new stack, and a read takes ONE snapshot at entry and overlays every record
      * it loads against that same stack. A layer dropped or added while the read is
      * in flight is therefore either seen by all of the read's loads or by none —
      * never by some, and never as a torn traversal.
      */
    private val optimisticLayers: AtomicRef[Chunk[Layer]] =
        AtomicRef.Unsafe.init(Chunk.empty[Layer])(using AllowUnsafe.embrace.danger).safe

    /** Resolve `key` as the persisted record `base` with every optimistic layer of
      * `layers` that defines it merged on top, in application (stacking) order —
      * the latest optimistic write wins on a field conflict, falling back through
      * the stack to the persisted record. Returns `Absent` only when neither the
      * cache nor any optimistic layer holds the key.
      */
    private def overlay(layers: Chunk[Layer], key: CacheKey, base: Maybe[Record]): Maybe[Record] =
        layers.foldLeft(base) { (below, layer) =>
            layer.records.get(key) match
                case None             => below
                case Some(optimistic) => Present(NormalizedCache.mergeRecords(below, optimistic)._1)
        }

    /** `base` with the optimistic stack `layers` overlaid on every record it loads —
      * the loader every store read hands the reader, in place of the bare backend
      * loader, so a read (and hence a watcher re-reading through it) reflects pending
      * optimistic mutations without the cache itself ever being mutated. `layers` is
      * the read's single snapshot: every batch of one read overlays the SAME stack.
      */
    private def overlayLoader(layers: Chunk[Layer], base: RecordLoader): RecordLoader =
        if layers.isEmpty then base
        else
            keys =>
                val persisted = base.load(keys)
                keys.foldLeft(persisted) { (loaded, key) =>
                    overlay(layers, key, Maybe.fromOption(persisted.get(key))) match
                        case Present(record) => loaded.updated(key, record)
                        case Absent          => loaded
                }

    /** Run the reader `f` over one snapshot of the optimistic stack and one state of
      * the backend — the shape of every store read — and lift its outcome onto the
      * read's row: a miss is an `Abort[CacheReadFailure]` failure, a defect stays a
      * panic under every handler.
      */
    private def readWith[A](f: RecordLoader => Result[CacheReadFailure, A])(using
        Frame
    ): A < (Sync & Abort[CacheReadFailure]) =
        optimisticLayers.get
            .map(layers => cache.read(base => f(overlayLoader(layers, base))))
            .map(outcome => Abort.get(outcome))

    /** Register `listener` to be run (via [[publish]]) on the changed keys of every
      * subsequent write until the enclosing `Scope` closes. The seam watchers hook
      * into.
      */
    def addChangedKeysListener(listener: Set[CacheKey] => Unit < Sync)(using Frame): Unit < (Sync & Scope) =
        changedKeys.subscribe(listener)

    /** Unsubscribe a `listener` previously registered with
      * [[addChangedKeysListener]] (by identity); a no-op if it was never
      * registered.
      */
    def removeChangedKeysListener(listener: Set[CacheKey] => Unit < Sync)(using Frame): Unit < Sync =
        changedKeys.unsubscribe(listener)

    /** Dump the whole normalized cache as one JSON object in the exact shape Apollo
      * Client's `InMemoryCache.extract()` returns — the shape the Apollo Client
      * Devtools "Cache" tab reads via `client.cache.extract(true)`.
      *
      * The result is a flat object whose keys are record cache-ids; every
      * [[RecordValue.Reference]] renders as `{"__ref": "<key>"}`, lists as JSON
      * arrays, scalars inline and [[RecordValue.Null]] as `null`. kyo's well-known
      * root keys (`QUERY_ROOT` / `MUTATION_ROOT` / `SUBSCRIPTION_ROOT`) are
      * translated to Apollo's (`ROOT_QUERY` / `ROOT_MUTATION` / `ROOT_SUBSCRIPTION`)
      * on both record keys and reference targets, so the panel renders a
      * native-looking cache; entity keys and links are otherwise verbatim and
      * self-consistent, so `__ref` links resolve in the UI.
      *
      * @param includeOptimistic overlay pending optimistic layers (latest wins),
      *                          matching Apollo's `extract(true)`; when false only
      *                          the persisted records are dumped.
      */
    def extract(includeOptimistic: Boolean = true)(using Frame): Json < Sync =
        cache.allRecords.map { base =>
            if !includeOptimistic then base
            else
                optimisticLayers.get.map { layers =>
                    val keys = base.keySet ++ layers.iterator.flatMap(_.records.keySet)
                    keys.iterator.flatMap { key =>
                        overlay(layers, key, Maybe.fromOption(base.get(key))) match
                            case Present(record) => Some(key -> record)
                            case _               => None
                    }.toMap
                }
        }.map(records => Json.JObj(records.map((key, record) => apolloizeKey(key) -> recordToJson(record))))
    end extract

    /** The devtools dump's rendering of a record key — the one place [[extract]]
      * lowers a [[CacheKey]] to a string. A kyo root key (or a path rooted at one)
      * is translated to Apollo's convention so the Devtools cache view is
      * recognizable; all other keys pass through.
      */
    private def apolloizeKey(cacheKey: CacheKey): String =
        val key = cacheKey.render
        if key == "QUERY_ROOT" then "ROOT_QUERY"
        else if key == "MUTATION_ROOT" then "ROOT_MUTATION"
        else if key == "SUBSCRIPTION_ROOT" then "ROOT_SUBSCRIPTION"
        else if key.startsWith("QUERY_ROOT.") then "ROOT_QUERY." + key.stripPrefix("QUERY_ROOT.")
        else if key.startsWith("MUTATION_ROOT.") then
            "ROOT_MUTATION." + key.stripPrefix("MUTATION_ROOT.")
        else if key.startsWith("SUBSCRIPTION_ROOT.") then
            "ROOT_SUBSCRIPTION." + key.stripPrefix("SUBSCRIPTION_ROOT.")
        else key
        end if
    end apolloizeKey

    private def recordToJson(record: Record): Json =
        Json.JObj(record.fields.map((fieldKey, value) => fieldKey.render -> recordValueToJson(value)))

    private def recordValueToJson(value: RecordValue): Json = value match
        case RecordValue.Scalar(json)   => json
        case RecordValue.Reference(ref) => Json.JObj(Map("__ref" -> Json.JStr(apolloizeKey(ref.key))))
        case RecordValue.RList(items)   => Json.JArr(items.map(recordValueToJson))
        case RecordValue.Null           => Json.JNull

    /** Normalize `operation`'s typed `data` into flat records without storing
      * them. The data is re-encoded through the operation's `dataSchema` into a
      * response map and walked by the [[Normalizer]] — the same data map the
      * network path would have decoded from. Pure: it reads no store state.
      */
    def normalize[D](operation: Operation[D], data: D): Map[CacheKey, Record] =
        Normalizer.normalize(
            operation,
            encode(operation.dataCodec, data),
            variablesOf(operation),
            cacheKeyGenerator,
            fieldPolicies,
            Present(recordMerger)
        )

    /** Commit the records `plan` computes against the backend's current state (see
      * [[commitWith]]).
      */
    private def commit(
        plan: RecordLoader => Chunk[Record],
        cacheHeaders: CacheHeaders
    )(using Frame): Set[CacheKey] < Sync =
        commitWith(loader => (plan(loader), ()), cacheHeaders).map(_._1)

    /** Commit the records `plan` computes against the backend's current state,
      * together with a result of `plan`'s own from the attempt that landed, and
      * report the positional-conflict warnings ([[CacheDiagnostics]]) of that
      * attempt. Every store write funnels through here — the one call of
      * [[NormalizedCache.transact]] — so the diagnostic cannot be wired into some
      * write paths and forgotten on others; while diagnostics are off it costs one
      * boolean read per record. `plan` may run more than once.
      */
    private def commitWith[A](
        plan: RecordLoader => (Chunk[Record], A),
        cacheHeaders: CacheHeaders
    )(using Frame): (Set[CacheKey], A) < Sync =
        def planned(loader: RecordLoader): (Chunk[Record], (Chunk[String], A)) =
            val (records, result) = plan(loader)
            val warnings =
                if !diagnostics.enabled || records.isEmpty then Chunk.empty[String]
                else
                    val stored = loader.load(records.map(_.key))
                    records.flatMap(incoming =>
                        Chunk.from(stored.get(incoming.key)).flatMap(diagnostics.positionalConflicts(_, incoming))
                    )
            (records, (warnings, result))
        end planned
        cache.transact(planned, cacheHeaders, recordMerger)
            .map { case (changed, (warnings, result)) => diagnostics.report(warnings).andThen((changed, result)) }
    end commitWith

    /** Merge `records` and [[publish]] the keys that changed. */
    private def writeAndPublish(records: => Chunk[Record], cacheHeaders: CacheHeaders)(using
        Frame
    ): Set[CacheKey] < Sync =
        Sync.defer(records).map { planned =>
            commit(_ => planned, cacheHeaders).map(changed => publish(changed).andThen(changed))
        }

    /** Normalize and merge `operation`'s response `data` into the cache, returning
      * the set of record keys whose stored value changed. Also [[publish]]es the
      * changed keys so watchers can react.
      *
      * @param cacheHeaders write hints forwarded to the backend (e.g. an expiry
      *                     stamp, or [[CacheHeaders.DoNotStore]])
      */
    def writeOperation[D](
        operation: Operation[D],
        data: D,
        cacheHeaders: CacheHeaders = CacheHeaders.None
    )(using Frame): Set[CacheKey] < Sync =
        writeAndPublish(Chunk.from(normalize(operation, data).values), cacheHeaders)

    /** Reassemble `operation`'s typed `data` from the cache.
      *
      * A [[kyo.apollo.exception.CacheMissException]] on the row when the cache cannot
      * satisfy every selected field, so the caller can fall through to the network or
      * surface the miss as a response value. A decode defect is a panic, not a miss.
      */
    def readOperation[D](operation: Operation[D])(using Frame): D < (Sync & Abort[CacheReadFailure]) =
        readOperationWithKeys(operation).map(_._1)

    /** Reassemble `operation`'s typed `data` from the cache *and* the set of record
      * keys the read depended on (root plus every reference/redirect target).
      *
      * The cache interceptor stamps these keys onto [[CacheInfo.dependentKeys]] so a
      * watcher knows exactly which keys to watch: it re-emits only when a write's
      * changed keys intersect this set. Fails like [[readOperation]].
      */
    def readOperationWithKeys[D](operation: Operation[D])(using
        Frame
    ): (D, Set[CacheKey]) < (Sync & Abort[CacheReadFailure]) =
        readWith(loader =>
            CacheBatchReader.readWithDependentKeys(
                operation,
                loader,
                variablesOf(operation),
                cacheKeyResolver,
                fieldPolicies
            )
        )

    /** [[readOperationWithKeys]] plus the store generation the read is current
      * at — sampled *before* the records are loaded, so the stamp is conservative:
      * a write published while the read was in flight has a higher generation and
      * shows up as `currentGeneration > stamp`. A watcher adopts the key set together
      * with this stamp and re-reads when the store has moved past it. Fails like
      * [[readOperation]].
      */
    def readOperationStamped[D](operation: Operation[D])(using
        Frame
    ): (D, Set[CacheKey], Long) < (Sync & Abort[CacheReadFailure]) =
        generation.get.map(stamp => readOperationWithKeys(operation).map((data, keys) => (data, keys, stamp)))

    /** Normalize and merge `fragment`'s typed `data` into the record stored under
      * `cacheKey`, returning the set of record keys whose stored value changed and
      * [[publish]]ing them so watchers react.
      *
      * The targeted-write counterpart to [[writeOperation]]: instead of rooting at
      * an operation key it roots the [[Normalizer]] at `cacheKey`, so
      * `writeFragment(userFragment, CacheKey("User", "1"), data)` merges straight
      * into `User:1` — the same record a full query reaches by reference — letting
      * app code imperatively update a single entity and have every watcher depending
      * on it re-emit.
      *
      * @param cacheHeaders write hints forwarded to the backend (e.g.
      *                     [[CacheHeaders.DoNotStore]])
      */
    def writeFragment[D](
        fragment: Fragment[D],
        cacheKey: CacheKey,
        data: D,
        cacheHeaders: CacheHeaders = CacheHeaders.None
    )(using Frame): Set[CacheKey] < Sync =
        writeAndPublish(Chunk.from(fragmentRecords(fragment, cacheKey, data).values), cacheHeaders)

    /** Write `fragment` at MANY cache keys as one store round: every entry is
      * normalized, the records are merged in a single [[NormalizedCache.transact]],
      * and the union of the changed keys is [[publish]]ed once.
      *
      * The difference from a loop over [[writeFragment]] is not the merge but the
      * broadcast. Each `publish` wakes every watcher whose last read touched one of
      * those records, and each of those re-reads its WHOLE operation — so N separate
      * writes to rows of one list cost a watcher N full re-reads of the list, while
      * this costs one. That is the shape a caller who knows all N keys up front
      * should be able to express, and the reason
      * [[kyo.apollo.ClientField.writeAll]] exists.
      *
      * Two entries may normalize to the same record key (the same entity written
      * twice, or a shared nested object). They are merged in argument order through
      * the store's record merger — what [[Normalizer]] does with two occurrences of
      * one key inside a single response, and what the backend does with two writes —
      * so a batch behaves like the one response it stands in for, field policies
      * included.
      *
      * An empty `entries` writes nothing and publishes nothing: a broadcast with no
      * change behind it is the cost this method exists to remove.
      */
    def writeFragments[D](
        fragment: Fragment[D],
        entries: Seq[(CacheKey, D)],
        cacheHeaders: CacheHeaders = CacheHeaders.None
    )(using Frame): Set[CacheKey] < Sync =
        if entries.isEmpty then Set.empty[CacheKey]
        else
            writeAndPublish(
                {
                    val merged = mutable.LinkedHashMap.empty[CacheKey, Record]
                    entries.foreach { (cacheKey, data) =>
                        fragmentRecords(fragment, cacheKey, data).foreach { (key, record) =>
                            merged.update(key, recordMerger.merge(Maybe.fromOption(merged.get(key)), record)._1)
                        }
                    }
                    Chunk.from(merged.values)
                },
                cacheHeaders
            )
    end writeFragments

    /** Normalize one fragment write into records, without touching the cache — the
      * step [[writeFragment]] and [[writeFragments]] share.
      */
    private def fragmentRecords[D](
        fragment: Fragment[D],
        cacheKey: CacheKey,
        data: D
    ): Map[CacheKey, Record] =
        // `addTypename` semantics for the fragment ROOT: the normalizer stamps a
        // static `__typename` onto NESTED objects (see Normalizer.compositeValue),
        // but the root object lands in `normalizeObject` directly — so a fragment
        // write that CREATES a fresh entity record would store it without
        // `__typename`, and a later operation read reaching it by reference (whose
        // object node compiles the implicit `__typename` selection) would abort
        // with a cache miss. Stamp it from the fragment's own type; a codec that
        // already emits one wins.
        val encoded = encode(fragment.dataCodec, data)
        val enriched =
            if encoded.contains("__typename") then encoded
            else encoded + ("__typename" -> Json.JStr(fragment.rootField.fieldType.leafType.name))
        new Normalizer(fragmentVariablesOf(fragment), cacheKey, cacheKeyGenerator, fieldPolicies, recordMerger)
            .normalize(enriched, fragment.rootField)
    end fragmentRecords

    /** The record key this store's [[CacheKeyGenerator]] gives an object of type
      * `typeName` with the response fields `obj` — a
      * [[kyo.apollo.exception.NoCacheIdentityException]] on the row when the
      * generator has no identity for it.
      *
      * The one door from a masked fragment ref to its record: a ref carries the
      * captured slice and its concrete type, never a key of its own, and a read
      * asks `keyOf(ref.typeName, ref.raw)`. That is the generator which normalized
      * the response, so the ref names the record that was actually written even
      * when the store keys the type differently from the [[CacheIdentity]] the
      * fragment was declared with. `typeName` overrides any `__typename` in `obj`.
      * No response path is supplied, so the generator's positional fallback cannot
      * apply: an entity without a usable identity has no key rather than a
      * position-based one that a list insertion would repoint. The failure shares
      * the read's row, so `keyOf(…).map(readFragment(fragment, _))` is one read
      * with one failure set.
      */
    def keyOf(typeName: String, obj: Map[String, Json])(using Frame): CacheKey < Abort[CacheReadFailure] =
        cacheKeyGenerator.cacheKeyForObject(
            obj.updated("__typename", Json.JStr(typeName)),
            CacheKeyGeneratorContext(CompiledField(typeName, CompiledNamedType(typeName)))
        ) match
            case Present(key) => key
            case Absent       => Abort.fail(NoCacheIdentityException(typeName))

    /** Reassemble `fragment`'s typed `data` from the record stored under
      * `cacheKey`.
      *
      * The targeted-read counterpart to [[readOperation]]: it denormalizes starting
      * at `cacheKey` rather than an operation root, reusing the same
      * [[CacheBatchReader]] decode so the result equals what a full operation would
      * have produced for that object. A [[kyo.apollo.exception.CacheMissException]]
      * on the row when the cache cannot satisfy every field the fragment selects; a
      * decode defect is a panic.
      */
    def readFragment[D](fragment: Fragment[D], cacheKey: CacheKey)(using Frame): D < (Sync & Abort[CacheReadFailure]) =
        readFragmentWithKeys(fragment, cacheKey).map(_._1)

    /** Reassemble `fragment`'s typed `data` from `cacheKey` *and* the set of record
      * keys the read depended on (`cacheKey` plus every reference/redirect target),
      * mirroring [[readOperationWithKeys]] so a watcher over a fragment knows exactly
      * which keys to watch. Fails like [[readFragment]].
      */
    def readFragmentWithKeys[D](fragment: Fragment[D], cacheKey: CacheKey)(using
        Frame
    ): (D, Set[CacheKey]) < (Sync & Abort[CacheReadFailure]) =
        readWith(loader => readFragmentFrom(loader, fragment, cacheKey))

    /** The fragment read over one loader — shared by [[readFragmentWithKeys]] and
      * the read half of [[updateFragment]].
      */
    private def readFragmentFrom[D](loader: RecordLoader, fragment: Fragment[D], cacheKey: CacheKey)(using
        Frame
    ): Result[CacheMissException, (D, Set[CacheKey])] =
        CacheBatchReader.readRooted(
            fragment.dataCodec,
            fragment.rootField,
            cacheKey,
            loader,
            fragmentVariablesOf(fragment),
            cacheKeyResolver,
            fieldPolicies
        )

    /** Read `operation`'s cached data, apply `update`, and write the result back —
      * react-apollo's `cache.updateQuery` — as ONE atomic step against the backend:
      * the write lands on exactly the state the read saw, so two concurrent updates
      * of the same data (say, two mutations each splicing an item into one cached
      * list) both land. A **cache miss is a no-op** (returns an empty changed-key
      * set); a successful write returns the changed keys, which re-emit every
      * dependent watcher. Use to splice a mutation result into a cached list (the
      * idiomatic alternative to `refetchQueries` for the common case) without a
      * network round-trip. A decode defect in the read, or a defect in `update`,
      * commits nothing and is a panic.
      *
      * `update` must be PURE: when another write commits between its read and its
      * write, the read is repeated on the newer state and `update` is applied again.
      */
    def updateOperation[D](operation: Operation[D])(update: D => D)(using Frame): Set[CacheKey] < Sync =
        updateWith { loader =>
            CacheBatchReader.read(operation, loader, variablesOf(operation), cacheKeyResolver, fieldPolicies)
                .map(data => Chunk.from(normalize(operation, update(data)).values))
        }

    /** Read a fragment for `cacheKey`, apply `update`, and write it back — the typed
      * equivalent of react-apollo's `cache.modify` on a single normalized record — as
      * one atomic step, like [[updateOperation]]. A **cache miss is a no-op**; a
      * defect commits nothing and is a panic. Returns the changed keys (re-emitting
      * dependent watchers). To delete a record instead, use [[evict]]. `update` must
      * be PURE (it may be applied more than once under contention).
      */
    def updateFragment[D](fragment: Fragment[D], cacheKey: CacheKey)(update: D => D)(using
        Frame
    ): Set[CacheKey] < Sync =
        updateWith { loader =>
            readFragmentFrom(loader, fragment, cacheKey)
                .map((data, _) => Chunk.from(fragmentRecords(fragment, cacheKey, update(data)).values))
        }

    /** The atomic read-modify-write both update paths share: `plan` reads through
      * the optimistic overlay (one snapshot of the stack) and yields the records to
      * merge; the changed keys are [[publish]]ed once the commit landed.
      *
      * A miss in `plan` is the documented no-op: nothing is merged. A panic in
      * `plan` merges nothing either, and is raised once the transaction is over —
      * so a defect neither commits a half-computed update nor passes for a miss.
      */
    private def updateWith(plan: RecordLoader => Result[CacheMissException, Chunk[Record]])(using
        Frame
    ): Set[CacheKey] < Sync =
        optimisticLayers.get.map { layers =>
            def planned(base: RecordLoader): (Chunk[Record], Result[Nothing, Unit]) =
                plan(overlayLoader(layers, base)) match
                    case Result.Success(records) => (records, Result.unit)
                    case Result.Failure(_)       => (Chunk.empty[Record], Result.unit)
                    case Result.Panic(defect)    => (Chunk.empty[Record], Result.panic[Nothing, Unit](defect))
            commitWith(planned, CacheHeaders.None).map { (changed, outcome) =>
                Abort.get(outcome).andThen(publish(changed)).andThen(changed)
            }
        }

    /** Fan `keys` out to every registered listener through the change bus (a no-op
      * when the set is empty or nothing is listening). Both the automatic write
      * paths ([[writeOperation]], [[remove]]) and manual/external invalidation go
      * through here — call it directly to notify watchers of a change made outside
      * the normal write paths (e.g. an optimistic update).
      *
      * A non-empty publish advances [[currentGeneration]] before the fan-out, so a
      * listener re-reading during delivery already stamps the new generation and
      * needs no follow-up read; an empty publish moves nothing and stamps nothing.
      */
    def publish(keys: Set[CacheKey])(using Frame): Unit < Sync =
        if keys.isEmpty then Kyo.unit
        else generation.incrementAndGet.andThen(changedKeys.publish(keys))

    /** Overlay `operation`'s optimistic `data` as a layer tagged by `mutationId`,
      * returning (and [[publish]]ing) the record keys it touches so watchers show
      * the optimistic state immediately.
      *
      * The data is normalized exactly like a real [[writeOperation]] — same
      * `dataSchema`, same key generator — but the records are held in a separate
      * optimistic layer over the pristine cache rather than merged into it.
      * Concurrent optimistic mutations **stack by `mutationId`**: each is an
      * independent layer, applied latest-wins at read time. Reversed by
      * [[rollbackOptimisticUpdates]] on failure, or superseded by
      * [[rollbackAndWrite]] on success. Mirrors apollo-kotlin's
      * `ApolloStore.writeOptimisticUpdates`.
      */
    def writeOptimisticUpdates[D](
        operation: Operation[D],
        data: D,
        mutationId: String
    )(using Frame): Set[CacheKey] < Sync =
        Sync.defer(normalize(operation, data)).map { records =>
            val layer = Layer(mutationId, records)
            // A re-write under an id already on the stack replaces that layer in place
            // (its stacking position is the mutation's, not the write's); a new id
            // goes on top.
            optimisticLayers.updateAndGet { layers =>
                if layers.exists(_.mutationId == mutationId) then
                    layers.map(l => if l.mutationId == mutationId then layer else l)
                else layers.append(layer)
            }.andThen {
                val changed = records.keySet
                publish(changed).andThen(changed)
            }
        }
    end writeOptimisticUpdates

    /** Drop the layer tagged by `mutationId` from the stack, returning the record
      * keys it held (empty if no such layer was on the stack). The one swap both
      * rollback paths share; the caller decides what to [[publish]].
      */
    private def dropLayer(mutationId: String)(using Frame): Set[CacheKey] < Sync =
        optimisticLayers.getAndUpdate(_.filter(_.mutationId != mutationId)).map { before =>
            before.foldLeft(Set.empty[CacheKey]) { (keys, layer) =>
                if layer.mutationId == mutationId then keys ++ layer.records.keySet else keys
            }
        }

    /** Drop the optimistic layer tagged by `mutationId` and [[publish]] the keys it
      * held so watchers re-read and revert to the persisted (or lower optimistic
      * layer's) value. The clean-rollback path for a *failed* optimistic mutation; a
      * no-op returning the empty set if the layer was already dropped. Mirrors
      * apollo-kotlin's `ApolloStore.rollbackOptimisticUpdates`.
      */
    def rollbackOptimisticUpdates(mutationId: String)(using Frame): Set[CacheKey] < Sync =
        dropLayer(mutationId).map(changed => publish(changed).andThen(changed))

    /** Complete a *successful* optimistic mutation: merge the real `data` into the
      * cache, then drop its optimistic layer, [[publish]]ing the union of the
      * optimistic keys and the real changed keys in a **single** notification.
      *
      * The single publish is deliberate: rolling back and writing separately would
      * publish twice, and a watcher reacting to the first publish would momentarily
      * re-read the pre-optimistic value before the real one landed (a visible
      * flicker). Publishing the union once lets watchers converge straight onto
      * server truth.
      *
      * The order is deliberate too. The layer lives in the store and the record in
      * the backend, so the two steps are not one atomic swap, and a read can land
      * between them. Committing first means that read sees the layer over the new
      * record — still the optimistic value — never the value from before the
      * mutation. It also means a commit that fails leaves the layer on the stack
      * untouched and unpublished, for the caller's rollback (the
      * [[CacheInterceptor]]'s `Scope` release) to drop and publish.
      *
      * @param cacheHeaders write hints forwarded to the backend (e.g. an expiry stamp)
      */
    def rollbackAndWrite[D](
        operation: Operation[D],
        data: D,
        mutationId: String,
        cacheHeaders: CacheHeaders = CacheHeaders.None
    )(using Frame): Set[CacheKey] < Sync =
        Sync.defer(Chunk.from(normalize(operation, data).values)).map { records =>
            commit(_ => records, cacheHeaders).map { realChangedKeys =>
                dropLayer(mutationId).map { optimisticKeys =>
                    val changed = optimisticKeys ++ realChangedKeys
                    publish(changed).andThen(changed)
                }
            }
        }
    end rollbackAndWrite

    /** The mutation ids of every optimistic layer currently overlaid, in stacking
      * order — a diagnostic view. A layer lives exactly as long as the `Scope` its
      * mutation stream is consumed in (see [[CacheInterceptor]]), so outside a
      * running optimistic mutation this is empty; a non-empty result after all
      * mutations have settled is the symptom of a leaked layer.
      */
    def optimisticLayerIds(using Frame): Chunk[String] < Sync = optimisticLayers.get.map(_.map(_.mutationId))

    /** Remove the record stored under `key` from the cache and, if a record was
      * actually present, [[publish]] `Set(key)` so watchers depending on it react.
      * Returns whether a record was removed. The imperative-invalidation path that
      * — unlike the bare `cache.remove` — does not drop the changed key on the
      * floor.
      */
    def remove(key: CacheKey)(using Frame): Boolean < Sync =
        cache.remove(Chunk(key)).map { removed =>
            if removed.isEmpty then false else publish(removed).andThen(true)
        }

    /** Sweep the cache for records unreachable from any root and remove them,
      * returning the set of removed keys.
      *
      * A reachability mark-and-sweep, seeded from the well-known operation roots
      * ([[CacheKey.QueryRoot]] / [[CacheKey.MutationRoot]] /
      * [[CacheKey.SubscriptionRoot]]) plus every record any live optimistic layer
      * holds or points at — so a record kept alive *only* by a pending optimistic
      * mutation survives GC and reappears if that mutation rolls back. Reachability
      * is transitively followed through [[Record.references]] (which already walks
      * lists), and every stored key not reached is dropped via `cache.remove`.
      *
      * No [[publish]] is issued: an unreachable record is by definition depended on
      * by no live read, so removing it cannot change any watcher's result. Mirrors
      * apollo-kotlin's `ApolloStore.garbageCollect`. This is the sweep helper the
      * public [[garbageCollect]] delegates to.
      */
    def removeUnreachableRecords(using Frame): Set[CacheKey] < Sync =
        cache.allRecords.map { all =>
            optimisticLayers.get.map { layers =>
                val unreachable = all.keySet.diff(reachableKeys(all, layers))
                cache.remove(Chunk.from(unreachable))
            }
        }

    /** Reclaim every record unreachable from a root, returning the removed keys.
      *
      * The public garbage-collection entry point; delegates to
      * [[removeUnreachableRecords]]. Run it after bulk invalidations (e.g. an
      * [[evict]] that orphaned a subtree) to reclaim the records left dangling.
      */
    def garbageCollect(using Frame): Set[CacheKey] < Sync = removeUnreachableRecords

    /** The set of record keys reachable from a root, over the `all`-records
      * snapshot: the operation roots and every live optimistic layer's records and
      * their referents, transitively closed through [[Record.references]].
      * Optimistic records live outside the backing cache, so their keys *and* the
      * keys they point at are seeded directly rather than discovered by walking
      * `all` — off one snapshot of the stack, like a read.
      */
    private def reachableKeys(all: Map[CacheKey, Record], layers: Chunk[Layer]): Set[CacheKey] =
        val optimisticRecords = layers.iterator.flatMap(_.records.values).toList
        val seeds =
            Set(CacheKey.QueryRoot, CacheKey.MutationRoot, CacheKey.SubscriptionRoot) ++
                optimisticRecords.iterator.map(_.key) ++
                optimisticRecords.iterator.flatMap(_.references.iterator.map(_.key))
        val reachable = mutable.Set.empty[CacheKey]
        val frontier  = mutable.Queue.from(seeds)
        while frontier.nonEmpty do
            val key = frontier.dequeue()
            if reachable.add(key) then
                all.get(key).foreach(_.references.foreach(ref => frontier.enqueue(ref.key)))
        end while
        reachable.toSet
    end reachableKeys

    /** Evict the record at `cacheKey`, [[publish]]ing the removed keys so watchers
      * react, and returning them.
      *
      * The targeted-invalidation counterpart to the sweeping [[garbageCollect]]:
      * with `cascade = false` (the default) it drops just `cacheKey`; with
      * `cascade = true` it additionally drops every record transitively reachable
      * from it through [[Record.references]], so evicting an entity also clears the
      * subtree it owned (mirroring apollo-kotlin's `ApolloStore.remove(cacheKey,
      * cascade)`). Returns the empty set (and publishes nothing) when `cacheKey`
      * was already absent.
      *
      * @param cacheKey the record to evict
      * @param cascade  whether to also evict the records `cacheKey` references
      */
    def evict(cacheKey: CacheKey, cascade: Boolean = false)(using Frame): Set[CacheKey] < Sync =
        val removal =
            if !cascade then cache.remove(Chunk(cacheKey))
            else cache.allRecords.map(all => cache.remove(Chunk.from(subtree(all, cacheKey))))
        removal.map(removed => publish(removed).andThen(removed))
    end evict

    /** `start` and every record transitively reachable from it over the `all`
      * snapshot — the keys a cascading [[evict]] removes. The traversal walks
      * [[Record.references]] from each present record, so a cascade clears an entire
      * owned subtree in one pass; absent keys along the way are simply skipped.
      */
    private def subtree(all: Map[CacheKey, Record], start: CacheKey): Set[CacheKey] =
        val found    = mutable.LinkedHashSet.empty[CacheKey]
        val frontier = mutable.Queue(start)
        while frontier.nonEmpty do
            val key = frontier.dequeue()
            if !found.contains(key) then
                all.get(key).foreach { record =>
                    found += key
                    record.references.foreach(ref => frontier.enqueue(ref.key))
                }
            end if
        end while
        found.toSet
    end subtree

    /** Drop every record from the backing cache. */
    def clearAll(using Frame): Unit < Sync = cache.clearAll

    /** Encode typed `data` into its response `data` map through its [[JsonCodec]] —
      * the write-side inverse of the decode the transport performs, so normalization
      * sees exactly the networked shape. Shared by the operation
      * ([[writeOperation]]) and fragment ([[writeFragment]]) write paths.
      */
    private def encode[D](codec: JsonCodec[D], data: D): Map[String, Json] =
        codec.encode(data) match
            case Json.JObj(fields) => fields
            case other =>
                throw IllegalArgumentException(
                    s"Operation data must encode to a JSON object, got ${other.getClass.getSimpleName}"
                )

    /** The `operation`'s variables as a `name -> JSON` map for argument-aware
      * field keys and redirect resolution — the same encoding [[FieldKey]] expects.
      */
    private def variablesOf(operation: Operation[?]): Map[String, Json] =
        variablesMap(operation.variables)

    /** The `fragment`'s variables as a `name -> JSON` map, the fragment analogue
      * of [[variablesOf]].
      */
    private def fragmentVariablesOf(fragment: Fragment[?]): Map[String, Json] =
        variablesMap(fragment.variables)

    /** Lower a `variables` [[Json.JObj]] to its underlying `name -> JSON` map
      * (empty for any non-object, defensively).
      */
    private def variablesMap(variables: Json): Map[String, Json] =
        variables match
            case Json.JObj(fields) => fields
            case _                 => Map.empty
end ApolloStore
