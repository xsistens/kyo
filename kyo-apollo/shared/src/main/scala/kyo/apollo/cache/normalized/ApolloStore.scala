package kyo.apollo.cache.normalized

import kyo.Maybe
import kyo.Present
import kyo.apollo.api.JsonCodec
import kyo.apollo.api.Operation
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.cache.normalized.internal.CacheBatchReader
import kyo.apollo.cache.normalized.internal.Normalizer
import kyo.apollo.exception.CacheMissException
import kyo.apollo.json.Json
import kyo.discard

/** The coordinator that turns typed operation data into cache records and back,
  * over a pluggable [[NormalizedCache]] backend.
  *
  * `ApolloStore` is the single entry point the cache interceptor (Phase 04
  * Task 6) drives: it normalizes a response into records and merges them
  * ([[writeOperation]]), reassembles typed data from the store
  * ([[readOperation]]), and announces which keys changed to any registered
  * watchers ([[publish]]; watchers themselves land in Phase 05). All typed
  * encode/decode goes through the operation's own `dataSchema`, so a
  * `writeOperation` → `readOperation` round-trip yields data equal to what the
  * network produced. Mirrors apollo-kotlin's `ApolloStore` / `DefaultApolloStore`.
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
      * default field-wise union. Handed to the backend on every write so merge
      * policy lives with the store, not the storage layer.
      */
    private val recordMerger: RecordMerger = RecordMerger.fieldPolicies(fieldPolicies)

    /** The change-notification bus: a hot, multicast source of the changed-key set
      * of every write. Watchers (Phase 05) [[ChangedKeysSubject.subscribe]] here to
      * learn which keys moved; every store write path funnels its changed keys
      * through it via [[publish]]. Exposed so a watcher can observe the store
      * directly. Empty of subscribers until a caller opts in.
      */
    val changedKeys: ChangedKeysSubject = new ChangedKeysSubject

    /** Optimistic record layers, keyed by mutation id and stacked in application
      * order. Each layer is the set of records (by key) a mutation wrote
      * *optimistically* before its network round-trip; reads overlay these on top
      * of the persisted cache (latest layer winning per field) so watchers see the
      * optimistic state immediately, while the backing [[NormalizedCache]] stays
      * pristine — which is what makes rollback nothing more than dropping a layer.
      * A `LinkedHashMap` because the *order* of layers is the stacking order:
      * concurrent optimistic mutations must resolve latest-wins at read time.
      */
    private val optimisticLayers =
        scala.collection.mutable.LinkedHashMap.empty[String, Map[String, Record]]

    /** Resolve `key` as the persisted record with every optimistic layer that
      * defines it merged on top, in application (stacking) order — the latest
      * optimistic write wins on a field conflict, falling back through the stack to
      * the persisted record. Returns `None` only when neither the cache nor any
      * optimistic layer holds the key.
      *
      * This is the read-time overlay every store read funnels through (in place of
      * a bare `cache.loadRecord`), so a `readOperation`/`readFragment` — and hence a
      * Phase 05 watcher re-reading through it — reflects pending optimistic
      * mutations without the cache itself ever being mutated.
      */
    private def loadRecordOverlay(key: String): Maybe[Record] =
        optimisticLayers.valuesIterator.foldLeft(cache.loadRecord(key)) { (base, layer) =>
            layer.get(key) match
                case None             => base
                case Some(optimistic) => Present(NormalizedCache.mergeRecords(base, optimistic)._1)
        }

    /** Register `listener` to be notified (via [[publish]]) of the changed keys of
      * every subsequent write, returning an unsubscribe thunk (`() => Unit`). The
      * seam Phase 05 watchers hook into; the returned thunk is how they tear the
      * subscription down on stream cancellation (or use
      * [[removeChangedKeysListener]] with the same callback).
      */
    def addChangedKeysListener(listener: Set[String] => Unit): () => Unit =
        changedKeys.subscribe(listener)

    /** Unsubscribe a `listener` previously registered with
      * [[addChangedKeysListener]] (by identity); a no-op if it was never
      * registered. The named teardown the watcher calls on cancellation.
      */
    def removeChangedKeysListener(listener: Set[String] => Unit): Unit =
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
    def extract(includeOptimistic: Boolean = true): Json =
        val base = cache.allRecords()
        val records: Map[String, Record] =
            if !includeOptimistic then base
            else
                val keys = base.keySet ++ optimisticLayers.valuesIterator.flatMap(_.keySet)
                keys.iterator.flatMap { key =>
                    loadRecordOverlay(key) match
                        case Present(record) => Some(key -> record)
                        case _               => None
                }.toMap
        Json.JObj(records.map((key, record) => apolloizeKey(key) -> recordToJson(record)))
    end extract

    /** Translate a kyo root key (or a path rooted at one) to Apollo's convention so
      * the Devtools cache view is recognizable; all other keys pass through.
      */
    private def apolloizeKey(key: String): String =
        if key == "QUERY_ROOT" then "ROOT_QUERY"
        else if key == "MUTATION_ROOT" then "ROOT_MUTATION"
        else if key == "SUBSCRIPTION_ROOT" then "ROOT_SUBSCRIPTION"
        else if key.startsWith("QUERY_ROOT.") then "ROOT_QUERY." + key.stripPrefix("QUERY_ROOT.")
        else if key.startsWith("MUTATION_ROOT.") then
            "ROOT_MUTATION." + key.stripPrefix("MUTATION_ROOT.")
        else if key.startsWith("SUBSCRIPTION_ROOT.") then
            "ROOT_SUBSCRIPTION." + key.stripPrefix("SUBSCRIPTION_ROOT.")
        else key

    private def recordToJson(record: Record): Json =
        Json.JObj(record.fields.map((fieldKey, value) => fieldKey -> recordValueToJson(value)))

    private def recordValueToJson(value: RecordValue): Json = value match
        case RecordValue.Scalar(json)   => json
        case RecordValue.Reference(ref) => Json.JObj(Map("__ref" -> Json.JStr(apolloizeKey(ref.key))))
        case RecordValue.RList(items)   => Json.JArr(items.map(recordValueToJson))
        case RecordValue.Null           => Json.JNull

    /** Normalize `operation`'s typed `data` into flat records without storing
      * them. The data is re-encoded through the operation's `dataSchema` into a
      * response map and walked by the [[Normalizer]] — the same data map the
      * network path would have decoded from.
      */
    def normalize[D](operation: Operation[D], data: D): Map[String, Record] =
        Normalizer.normalize(
            operation,
            encode(operation.dataCodec, data),
            variablesOf(operation),
            cacheKeyGenerator,
            fieldPolicies
        )

    /** Normalize and merge `operation`'s response `data` into the cache, returning
      * the set of record keys whose stored value changed. Also [[publish]]es the
      * changed keys so watchers can react.
      *
      * @param cacheHeaders write hints forwarded to the backend (e.g. an expiry
      *                     stamp, or [[CacheHeaders.DoNotStore]])
      */
    /** Every store write funnels through here, so the positional-conflict diagnostic
      * ([[CacheDiagnostics]]) cannot be wired into two of the three write paths and
      * forgotten on the third. While diagnostics are off it costs one boolean read.
      */
    private def mergeRecords(records: Iterable[Record], cacheHeaders: CacheHeaders): Set[String] =
        if diagnostics.enabled then
            records.foreach { incoming =>
                cache.loadRecord(incoming.key).foreach(diagnostics.positionalConflicts(_, incoming))
            }
        end if
        cache.merge(records, cacheHeaders, recordMerger)
    end mergeRecords

    def writeOperation[D](
        operation: Operation[D],
        data: D,
        cacheHeaders: CacheHeaders = CacheHeaders.None
    ): Set[String] =
        val changedKeys = mergeRecords(normalize(operation, data).values, cacheHeaders)
        publish(changedKeys)
        changedKeys
    end writeOperation

    /** Reassemble `operation`'s typed `data` from the cache.
      *
      * @throws kyo.apollo.exception.CacheMissException if the cache cannot satisfy
      *         every selected field (so the caller can fall through to the network
      *         or surface the miss as a response value).
      */
    def readOperation[D](operation: Operation[D]): D =
        readOperationWithKeys(operation)._1

    /** Reassemble `operation`'s typed `data` from the cache *and* the set of record
      * keys the read depended on (root plus every reference/redirect target).
      *
      * The cache interceptor stamps these keys onto [[CacheInfo.dependentKeys]] so a
      * Phase 05 watcher knows exactly which keys to watch: it re-emits only when a
      * write's changed keys intersect this set.
      *
      * @throws kyo.apollo.exception.CacheMissException if the cache cannot satisfy
      *         every selected field
      */
    def readOperationWithKeys[D](operation: Operation[D]): (D, Set[String]) =
        CacheBatchReader.readWithDependentKeys(
            operation,
            loadRecordOverlay,
            variablesOf(operation),
            cacheKeyResolver,
            fieldPolicies
        )

    /** Normalize and merge `fragment`'s typed `data` into the record stored under
      * `cacheKey`, returning the set of record keys whose stored value changed and
      * [[publish]]ing them so watchers react.
      *
      * The targeted-write counterpart to [[writeOperation]]: instead of rooting at
      * an operation key it roots the [[Normalizer]] at `cacheKey`, so
      * `writeFragment(userFragment, CacheKey("User:1"), data)` merges straight into
      * `User:1` — the same record a full query reaches by reference — letting app
      * code imperatively update a single entity and have every watcher depending on
      * it re-emit.
      *
      * @param cacheHeaders write hints forwarded to the backend (e.g.
      *                     [[CacheHeaders.DoNotStore]])
      */
    def writeFragment[D](
        fragment: Fragment[D],
        cacheKey: CacheKey,
        data: D,
        cacheHeaders: CacheHeaders = CacheHeaders.None
    ): Set[String] =
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
        val records =
            new Normalizer(fragmentVariablesOf(fragment), cacheKey.key, cacheKeyGenerator, fieldPolicies)
                .normalize(enriched, fragment.rootField)
        val changedKeys = mergeRecords(records.values, cacheHeaders)
        publish(changedKeys)
        changedKeys
    end writeFragment

    /** Reassemble `fragment`'s typed `data` from the record stored under
      * `cacheKey`.
      *
      * The targeted-read counterpart to [[readOperation]]: it denormalizes starting
      * at `cacheKey` rather than an operation root, reusing the same
      * [[CacheBatchReader]] decode so the result equals what a full operation would
      * have produced for that object.
      *
      * @throws kyo.apollo.exception.CacheMissException if the cache cannot satisfy every
      *         field the fragment selects
      */
    def readFragment[D](fragment: Fragment[D], cacheKey: CacheKey): D =
        readFragmentWithKeys(fragment, cacheKey)._1

    /** Reassemble `fragment`'s typed `data` from `cacheKey` *and* the set of record
      * keys the read depended on (`cacheKey` plus every reference/redirect target),
      * mirroring [[readOperationWithKeys]] so a watcher over a fragment knows exactly
      * which keys to watch.
      *
      * @throws kyo.apollo.exception.CacheMissException if the cache cannot satisfy every
      *         field the fragment selects
      */
    def readFragmentWithKeys[D](fragment: Fragment[D], cacheKey: CacheKey): (D, Set[String]) =
        val reader =
            new CacheBatchReader(
                loadRecordOverlay,
                fragmentVariablesOf(fragment),
                cacheKey.key,
                cacheKeyResolver,
                fieldPolicies
            )
        val data = reader.toData(fragment.rootField)
        (fragment.dataCodec.decode(data), reader.dependentKeys)
    end readFragmentWithKeys

    /** Read `operation`'s cached data, apply `update`, and write the result back —
      * react-apollo's `cache.updateQuery`. A **cache miss is a no-op** (returns an
      * empty changed-key set); a successful write returns the changed keys, which
      * re-emit every dependent watcher. Use to splice a mutation result into a
      * cached list (the idiomatic alternative to `refetchQueries` for the common
      * case) without a network round-trip.
      */
    def updateOperation[D](operation: Operation[D])(update: D => D): Set[String] =
        val current =
            try Present(readOperation(operation))
            catch case _: CacheMissException => Maybe.empty[D]
        current match
            case Present(data) => writeOperation(operation, update(data))
            case _             => Set.empty
    end updateOperation

    /** Read a fragment for `cacheKey`, apply `update`, and write it back — the typed
      * equivalent of react-apollo's `cache.modify` on a single normalized record. A
      * **cache miss is a no-op**. Returns the changed keys (re-emitting dependent
      * watchers). To delete a record instead, use [[evict]].
      */
    def updateFragment[D](fragment: Fragment[D], cacheKey: CacheKey)(update: D => D): Set[String] =
        val current =
            try Present(readFragment(fragment, cacheKey))
            catch case _: CacheMissException => Maybe.empty[D]
        current match
            case Present(data) => writeFragment(fragment, cacheKey, update(data))
            case _             => Set.empty
    end updateFragment

    /** Fan `keys` out to every registered listener through the change bus (a no-op
      * when the set is empty or nothing is listening). Both the automatic write
      * paths ([[writeOperation]], [[remove]]) and manual/external invalidation go
      * through here — call it directly to notify watchers of a change made outside
      * the normal write paths (e.g. an optimistic update).
      */
    def publish(keys: Set[String]): Unit = changedKeys.publish(keys)

    /** Overlay `operation`'s optimistic `data` as a layer tagged by `mutationId`,
      * returning (and [[publish]]ing) the record keys it touches so watchers show
      * the optimistic state immediately.
      *
      * The data is normalized exactly like a real [[writeOperation]] — same
      * `dataSchema`, same key generator — but the records are held in a separate
      * optimistic layer over the pristine cache rather than merged into it.
      * Concurrent optimistic mutations **stack by `mutationId`**: each is an
      * independent layer, applied latest-wins at read time (see [[loadRecordOverlay]]).
      * Reversed by [[rollbackOptimisticUpdates]] on failure, or superseded by
      * [[rollbackAndWrite]] on success. Mirrors apollo-kotlin's
      * `ApolloStore.writeOptimisticUpdates`.
      */
    def writeOptimisticUpdates[D](
        operation: Operation[D],
        data: D,
        mutationId: String
    ): Set[String] =
        val records = normalize(operation, data)
        optimisticLayers.update(mutationId, records)
        val changed = records.keySet
        publish(changed)
        changed
    end writeOptimisticUpdates

    /** Drop the optimistic layer tagged by `mutationId` and [[publish]] the keys it
      * held so watchers re-read and revert to the persisted (or lower optimistic
      * layer's) value. The clean-rollback path for a *failed* optimistic mutation; a
      * no-op returning the empty set if the layer was already dropped. Mirrors
      * apollo-kotlin's `ApolloStore.rollbackOptimisticUpdates`.
      */
    def rollbackOptimisticUpdates(mutationId: String): Set[String] =
        optimisticLayers.remove(mutationId) match
            case Some(records) =>
                val changed = records.keySet
                publish(changed)
                changed
            case None => Set.empty

    /** Complete a *successful* optimistic mutation: drop its optimistic layer and
      * merge the real `data` into the cache, [[publish]]ing the union of the
      * optimistic keys and the real changed keys in a **single** notification.
      *
      * The single publish is deliberate: rolling back and writing separately would
      * publish twice, and a watcher reacting to the first publish would momentarily
      * re-read the pre-optimistic value before the real one landed (a visible
      * flicker). Publishing the union once lets watchers converge straight onto
      * server truth.
      *
      * @param cacheHeaders write hints forwarded to the backend (e.g. an expiry stamp)
      */
    def rollbackAndWrite[D](
        operation: Operation[D],
        data: D,
        mutationId: String,
        cacheHeaders: CacheHeaders = CacheHeaders.None
    ): Set[String] =
        val optimisticKeys  = optimisticLayers.remove(mutationId).map(_.keySet).getOrElse(Set.empty)
        val realChangedKeys = mergeRecords(normalize(operation, data).values, cacheHeaders)
        val changed         = optimisticKeys ++ realChangedKeys
        publish(changed)
        changed
    end rollbackAndWrite

    /** Remove the record stored under `key` from the cache and, if a record was
      * actually present, [[publish]] `Set(key)` so watchers depending on it react.
      * Returns whether a record was removed. The imperative-invalidation path that
      * — unlike the bare `cache.remove` — does not drop the changed key on the
      * floor.
      */
    def remove(key: String): Boolean =
        val removed = cache.remove(key)
        if removed then publish(Set(key))
        removed
    end remove

    /** Remove the record identified by `cacheKey` — the [[CacheKey]] overload of
      * [[remove]].
      */
    def remove(cacheKey: CacheKey): Boolean = remove(cacheKey.key)

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
    def removeUnreachableRecords(): Set[String] =
        val all         = cache.allRecords()
        val reachable   = reachableKeys(all)
        val unreachable = all.keySet.diff(reachable)
        unreachable.foreach(cache.remove)
        unreachable
    end removeUnreachableRecords

    /** Reclaim every record unreachable from a root, returning the removed keys.
      *
      * The public garbage-collection entry point; delegates to
      * [[removeUnreachableRecords]]. Run it after bulk invalidations (e.g. an
      * [[evict]] that orphaned a subtree) to reclaim the records left dangling.
      */
    def garbageCollect(): Set[String] = removeUnreachableRecords()

    /** The set of record keys reachable from a root, over the `all`-records
      * snapshot: the operation roots and every live optimistic layer's records and
      * their referents, transitively closed through [[Record.references]].
      * Optimistic records live outside the backing cache, so their keys *and* the
      * keys they point at are seeded directly rather than discovered by walking
      * `all`.
      */
    private def reachableKeys(all: Map[String, Record]): Set[String] =
        val optimisticRecords = optimisticLayers.valuesIterator.flatMap(_.values).toList
        val seeds =
            Set(CacheKey.QueryRoot.key, CacheKey.MutationRoot.key, CacheKey.SubscriptionRoot.key) ++
                optimisticRecords.iterator.map(_.key) ++
                optimisticRecords.iterator.flatMap(_.references.iterator.map(_.key))
        val reachable = scala.collection.mutable.Set.empty[String]
        val frontier  = scala.collection.mutable.Queue.from(seeds)
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
    def evict(cacheKey: CacheKey, cascade: Boolean = false): Set[String] =
        val removed =
            if !cascade then if cache.remove(cacheKey.key) then Set(cacheKey.key) else Set.empty
            else cascadeEvict(cacheKey.key)
        if removed.nonEmpty then publish(removed)
        removed
    end evict

    /** Remove `start` and every record transitively reachable from it (over the
      * current snapshot), returning the removed keys. The traversal walks
      * [[Record.references]] from each removed record, so a cascade clears an entire
      * owned subtree in one pass; absent keys along the way are simply skipped.
      */
    private def cascadeEvict(start: String): Set[String] =
        val all      = cache.allRecords()
        var removed  = Set.empty[String]
        val frontier = scala.collection.mutable.Queue(start)
        while frontier.nonEmpty do
            val key = frontier.dequeue()
            if !removed.contains(key) then
                all.get(key).foreach { record =>
                    discard(cache.remove(key))
                    removed += key
                    record.references.foreach(ref => frontier.enqueue(ref.key))
                }
            end if
        end while
        removed
    end cascadeEvict

    /** Drop every record from the backing cache. */
    def clearAll(): Unit = cache.clearAll()

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
