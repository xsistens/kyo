package kyo.apollo.cache.normalized.internal

import kyo.Absent
import kyo.Chunk
import kyo.Frame
import kyo.Maybe
import kyo.Present
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.RecordLoader
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.exception.CacheMissException
import kyo.apollo.json.Json
import scala.collection.immutable.VectorMap

/** Denormalizes a set of [[Record]]s back into a GraphQL response `data` map by
  * walking an operation's selection tree — the read-side inverse of
  * [[Normalizer]].
  *
  * Starting from the operation's root record, it follows the *same*
  * [[CompiledField]] tree the [[Normalizer]] wrote against (via the shared
  * [[FieldCollector]]), resolving each [[RecordValue.Reference]] to its stored
  * [[Record]] and reassembling a nested [[Json.JObj]]. A required record or
  * field that is absent aborts the read with a [[CacheMissException]] rather than
  * producing partial data — so `CacheFirst`/`NetworkFirst` can fall through to
  * the network and `CacheOnly` can surface the miss as a response value.
  *
  * Records come from a [[RecordLoader]] in batches, ONE `load` per level of the
  * selection tree: the root record, then every record the root's selected fields
  * reference or redirect to, then every record those reference, and so on. A list
  * of a thousand entities is one load, not a thousand — the round-trip count a
  * persistent backend pays is the depth of the query, not its width. Once every
  * level is loaded the response is assembled from the loaded records without
  * further loads, so the misses it raises and the dependency keys it reports are
  * exactly those of a record-by-record walk. Mirrors apollo-kotlin's
  * `CacheBatchReader`.
  *
  * The assembled [[Json]] map is decoded through the operation's own
  * `dataSchema` (via [[kyo.apollo.json.SchemaJson.decode]], see
  * [[CacheBatchReader.read]]) — the same kyo-schema typed-decode path the HTTP
  * transport uses on a live body, which is what guarantees a `writeOperation` →
  * `readOperation` round-trip yields data equal to the networked value.
  *
  * @param loader           loads the records present among a batch of keys
  * @param variables        the operation's variables (name → encoded JSON), for
  *                         argument-aware [[FieldKey]]s and redirect resolution
  * @param rootKey          the record key the read starts from (see [[CacheKey.rootKey]])
  * @param cacheKeyResolver read-side redirects (e.g. `book(id:)` → `Book:id`);
  *                         defaults to never redirecting
  * @param fieldPolicies    per-field policies consulted for read-side storage
  *                         [[FieldKey]]s and per-field read redirects; defaults
  *                         to the identity policy
  */
final class CacheBatchReader(
    loader: RecordLoader,
    variables: Map[String, Json],
    rootKey: CacheKey,
    cacheKeyResolver: CacheKeyResolver = CacheKeyResolver.default,
    fieldPolicies: FieldPolicies = FieldPolicies.empty
)(using Frame):

    /** Accumulates the key of every [[Record]] successfully resolved during the
      * read (root + each redirect/reference target actually visited). This is the
      * dependency set a Phase 05 watcher intersects against a write's changed keys
      * to decide whether to re-emit. Populated as a side effect of [[resolve]];
      * read via [[dependentKeys]] after [[toData]] completes.
      */
    private val visitedKeys = scala.collection.mutable.LinkedHashSet.empty[CacheKey]

    /** Every record the level-wise loads returned, by key, and every key they were
      * asked for (present or not) — so no key is requested twice.
      */
    private val loaded    = scala.collection.mutable.HashMap.empty[CacheKey, Record]
    private val requested = scala.collection.mutable.HashSet.empty[CacheKey]

    /** The set of record keys this reader touched, in first-visit order. Meaningful
      * only after a successful [[toData]] (a miss aborts before the graph is fully
      * walked).
      */
    def dependentKeys: Set[CacheKey] = visitedKeys.toSet

    /** Resolve `key` to its loaded [[Record]], recording the key as a dependency on
      * a hit. A miss records nothing — the read is about to abort with a
      * [[CacheMissException]], so there is no successful graph to depend on. A key no
      * level asked for (which the level walk never leaves out) is loaded on its own.
      */
    private def resolve(key: CacheKey): Maybe[Record] =
        if !requested.contains(key) then fetch(Chunk(key))
        val record = Maybe.fromOption(loaded.get(key))
        if record.isDefined then visitedKeys += key
        record
    end resolve

    /** Load the keys among `keys` not requested before, in one batch. */
    private def fetch(keys: Chunk[CacheKey]): Unit =
        val fresh = keys.filterNot(requested.contains).distinct
        if fresh.nonEmpty then
            requested ++= fresh
            loaded ++= loader.load(fresh)
    end fetch

    /** The miss for record `key`, or for its field `field`. The exception carries
      * the key's rendered form (the exception model is key-type agnostic), lowered
      * here once for every miss this reader raises.
      */
    private def miss(key: CacheKey, field: Maybe[String] = Absent): CacheMissException =
        field.fold(CacheMissException(key.render))(CacheMissException(key.render, _))

    /** Assemble the response `data` object for `rootField`, resolving references
      * from the store. Throws [[CacheMissException]] if any required record or
      * field is missing.
      */
    def toData(rootField: CompiledField): Json.JObj =
        loadLevels(Chunk(Pending(rootKey, rootField.selections, rootField.fieldType.leafType.name)))
        val root = resolve(rootKey).getOrElse(throw miss(rootKey))
        readObject(root, rootField.selections, rootField.fieldType.leafType.name)
    end toData

    /** One object the walk still has to read: the record key it lives under, the
      * selections read from it, and its static type.
      */
    final private case class Pending(key: CacheKey, selections: Chunk[CompiledSelection], parentType: String)

    /** Load the selection tree level by level: one batch for the keys of `level`,
      * then the objects those records lead to become the next level. A record that is
      * absent, or a field that is, leads nowhere — the assembly step raises the miss.
      */
    @scala.annotation.tailrec
    private def loadLevels(level: Chunk[Pending]): Unit =
        if level.nonEmpty then
            fetch(level.map(_.key))
            loadLevels(level.flatMap(children))

    /** The objects one level below `pending`, following the same field collection,
      * redirects and field keys as [[readObject]] / [[readField]] / [[readValue]].
      */
    private def children(pending: Pending): Chunk[Pending] =
        loaded.get(pending.key) match
            case None => Chunk.empty
            case Some(record) =>
                val typename = recordTypename(record).getOrElse(pending.parentType)
                FieldCollector.collect(pending.selections, typename, injectTypename = false).flatMap { field =>
                    val childType = field.fieldType.leafType.name
                    redirect(typename, field) match
                        case Present(targetKey) => Chunk(Pending(targetKey, field.selections, childType))
                        case Absent =>
                            record.get(fieldPolicies.fieldKey(typename, field, variables)) match
                                case Present(value) => references(value).map(Pending(_, field.selections, childType))
                                case Absent         => Chunk.empty
                    end match
                }
        end match
    end children

    /** The record keys a stored value references, lists included. */
    private def references(value: RecordValue): Chunk[CacheKey] =
        value match
            case RecordValue.Reference(ref) => Chunk(ref.key)
            case RecordValue.RList(items)   => items.flatMap(references)
            case _                          => Chunk.empty

    /** Read one object `record` under `selections` into a [[Json.JObj]]. The
      * object's own stored `__typename` (when present) drives inline-fragment
      * resolution, falling back to the static `parentType` from the schema.
      */
    private def readObject(
        record: Record,
        selections: Chunk[CompiledSelection],
        parentType: String
    ): Json.JObj =
        val typename = recordTypename(record).getOrElse(parentType)
        val builder  = VectorMap.newBuilder[String, Json]
        for field <- FieldCollector.collect(selections, typename, injectTypename = false) do
            builder += (field.responseName -> readField(record, field, typename))
        Json.JObj(builder.result())
    end readObject

    /** Resolve a single selected field to JSON: a configured redirect reads a
      * different record, otherwise the field's own stored value is used. A field
      * selected but absent from its record is a [[CacheMissException]].
      * `parentType` is the resolved type of `record` (its stored `__typename`,
      * falling back to the schema's static type), matching the type the
      * [[Normalizer]] keyed the field under.
      */
    private def readField(record: Record, field: CompiledField, parentType: String): Json =
        redirect(parentType, field) match
            case Present(targetKey) =>
                val target = resolve(targetKey)
                    .getOrElse(throw miss(targetKey, Present(field.responseName)))
                readObject(target, field.selections, field.fieldType.leafType.name)
            case Absent =>
                val fieldKey = fieldPolicies.fieldKey(parentType, field, variables)
                record.get(fieldKey) match
                    case Present(value) => readValue(value, field)
                    // A local `@client` field that was never written is not a miss: yield
                    // JNull so the client node's decode produces its default (None / empty
                    // list / default object), rather than aborting the whole read.
                    case Absent if field.client => Json.JNull
                    case Absent                 => throw miss(record.key, Present(fieldKey.render))
                end match

    /** The redirect target for a composite `field`, if a policy supplies one.
      * A per-field [[FieldPolicy.read]] resolver takes precedence over the global
      * [[CacheKeyResolver]]. Scalar (leaf) fields are never redirected — a redirect
      * names another record to read a sub-selection from.
      */
    private def redirect(parentType: String, field: CompiledField): Maybe[CacheKey] =
        if field.selections.isEmpty then Absent
        else
            fieldPolicies
                .readRedirect(parentType, field, variables)
                .orElse(cacheKeyResolver.cacheKeyForField(field, variables))

    /** Turn a stored [[RecordValue]] back into JSON: scalars/null pass through,
      * lists recurse element-wise, and a reference reads its (already loaded)
      * target record under `field`'s selections.
      */
    private def readValue(value: RecordValue, field: CompiledField): Json = value match
        case RecordValue.Null         => Json.JNull
        case RecordValue.Scalar(json) => json
        case RecordValue.RList(items) => Json.JArr(items.map(readValue(_, field)))
        case RecordValue.Reference(ref) =>
            val child = resolve(ref.key)
                .getOrElse(throw miss(ref.key, Present(field.responseName)))
            readObject(child, field.selections, field.fieldType.leafType.name)

    /** An object's stored `__typename`, when present as a string scalar. */
    private def recordTypename(record: Record): Maybe[String] =
        record.get(FieldKey.Typename).collect { case RecordValue.Scalar(Json.JStr(t)) => t }
end CacheBatchReader

object CacheBatchReader:

    /** Denormalize `operation`'s records into a typed `D`.
      *
      * Assembles the response `data` map from the loader (starting at the
      * operation's root key) and decodes it through `operation.dataSchema` — the
      * same typed-decode path the HTTP transport uses — so the result equals what
      * the network would have produced. A pure function of the loader's records:
      * throws [[CacheMissException]] if they cannot satisfy every selected field.
      *
      * @param operation        the operation whose response is being read back
      * @param loader           loads the records present among a batch of keys
      * @param variables        the operation's encoded variables (default none)
      * @param cacheKeyResolver read-side redirect policy (default: none)
      * @param fieldPolicies    per-field read-side policies (default identity)
      */
    def read[D](
        operation: Operation[D],
        loader: RecordLoader,
        variables: Map[String, Json] = Map.empty,
        cacheKeyResolver: CacheKeyResolver = CacheKeyResolver.default,
        fieldPolicies: FieldPolicies = FieldPolicies.empty
    )(using Frame): D =
        readWithDependentKeys(operation, loader, variables, cacheKeyResolver, fieldPolicies)._1

    /** Denormalize `operation`'s records into a typed `D` *and* the set of record
      * keys the read depended on.
      *
      * Identical to [[read]] on the data path, additionally returning the
      * `dependentKeys` — every record visited while assembling the response (the
      * root plus each reference/redirect target). A Phase 05 watcher intersects
      * this set with a write's changed keys to decide whether the write affects it.
      *
      * @return the decoded data and the set of record keys it touched
      */
    def readWithDependentKeys[D](
        operation: Operation[D],
        loader: RecordLoader,
        variables: Map[String, Json] = Map.empty,
        cacheKeyResolver: CacheKeyResolver = CacheKeyResolver.default,
        fieldPolicies: FieldPolicies = FieldPolicies.empty
    )(using Frame): (D, Set[CacheKey]) =
        val rootKey = CacheKey.rootKey(operation)
        val reader =
            new CacheBatchReader(loader, variables, rootKey, cacheKeyResolver, fieldPolicies)
        val data = reader.toData(operation.rootField)
        (operation.dataCodec.decode(data), reader.dependentKeys)
    end readWithDependentKeys
end CacheBatchReader
