package kyo.apollo.cache.normalized.internal

import kyo.Absent
import kyo.Chunk
import kyo.Maybe
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.RecordMerger
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.json.Json
import scala.collection.mutable

/** Turns a GraphQL response `data` map into a flat set of normalized
  * [[Record]]s, keyed by [[CacheKey]].
  *
  * The normalizer walks the operation's [[Operation.rootField]] selection tree
  * alongside the response data map — the *same* [[CompiledField]] tree the typed
  * decoder uses — flattening the nested response into deduplicated records:
  *
  *   - a scalar leaf stays inline as a [[RecordValue.Scalar]];
  *   - a nested object is lifted into its own [[Record]] and replaced by a
  *     [[RecordValue.Reference]] in its parent, so the same entity referenced
  *     from several places is stored once;
  *   - a list becomes a [[RecordValue.RList]] of element values (references for
  *     object lists, scalars for scalar lists);
  *   - `null` becomes [[RecordValue.Null]].
  *
  * Each object's storage [[CacheKey]] comes from the [[CacheKeyGenerator]]
  * (typically `__typename` + id), falling back to a position-based key from the
  * response path when the object carries no stable id. Two occurrences of the
  * same key within one response are merged into a single record before the map
  * is returned — through the store's [[RecordMerger]], the same one the backend
  * applies when the records are committed. "Two records under one key, which
  * value holds?" has one answer whether both arrive in one response (a connection
  * selected twice under two aliases) or in two writes, so a field policy such as a
  * connection's edges union applies to both. Mirrors apollo-kotlin's `Normalizer`.
  *
  * That fallback path is rooted at the NEAREST KEYED ANCESTOR, not at the
  * operation root: below `Album:1` an id-less image is `Album:1.images.0`, never
  * `QUERY_ROOT.album.images.0`. Otherwise the same entity's id-less children get a
  * different key per writing operation, and the narrower writer repoints the
  * parent's field at records the wider reader's fields are missing from — a cache
  * miss on data nobody contradicted. Same rule as apollo-kotlin's `buildRecord`,
  * which hands its children `base = key` rather than the accumulated path.
  *
  * `__typename` is collected implicitly: every composite selection set is walked
  * as if it also requested `__typename`, so when the server includes it the value
  * is stored on the record (letting keys be computed and fragments re-resolved on
  * read). Until codegen requests `__typename` on every selection set, the key
  * generator degrades gracefully to path-based keys when it is absent.
  *
  * @param variables         the operation's variables (name → encoded JSON), used
  *                          to compute argument-aware [[FieldKey]]s
  * @param rootKey           the record key the root object is stored under
  *                          (see [[CacheKey.rootKey]])
  * @param cacheKeyGenerator the policy that assigns a [[CacheKey]] to each object
  * @param fieldPolicies     per-field policies consulted for the storage
  *                          [[FieldKey]] (e.g. a connection field dropping its
  *                          pagination arguments)
  * @param merger            the record merger for two occurrences of one key in
  *                          the response — the store's own, so the response is
  *                          merged exactly as the backend merges it
  */
final class Normalizer(
    variables: Map[String, Json],
    rootKey: CacheKey,
    cacheKeyGenerator: CacheKeyGenerator,
    fieldPolicies: FieldPolicies,
    merger: RecordMerger
):

    /** Records accumulated so far, keyed by [[Record.key]]. Insertion-ordered so
      * the returned map lists the root first, then records in the order they are
      * first encountered — handy for tests and debugging.
      */
    private val records = mutable.LinkedHashMap.empty[CacheKey, Record]

    /** Normalize `data` against `rootField`'s selections into a set of records.
      *
      * @param data      the response `data` object (as encoded by
      *                  `Operation.dataCodec`)
      * @param rootField the operation's [[Operation.rootField]]; its
      *                  [[CompiledField.selections]] are the top-level fields and
      *                  its leaf type names the root object's GraphQL type
      * @return every record produced, keyed by cache key, with duplicate keys
      *         within this response already merged
      */
    def normalize(data: Map[String, Json], rootField: CompiledField): Map[CacheKey, Record] =
        val rootType = rootField.fieldType.leafType.name
        // Stamp the root like every nested object (servers rarely send a root
        // `__typename`), so the record merger can resolve type-scoped field
        // policies for root fields too.
        val enrichedRoot =
            if data.contains("__typename") then data
            else data + ("__typename" -> Json.JStr(rootType))
        normalizeObject(
            obj = enrichedRoot,
            selections = rootField.selections,
            parentType = rootType,
            key = rootKey,
            path = List(rootKey.render)
        )
        records.toMap
    end normalize

    /** Build (and record) the [[Record]] for one object, recursing into its
      * composite fields. `key` is the object's already-decided cache key and
      * `path` the prefix its id-less descendants are keyed under — which is this
      * object's own key, not the response path that led here.
      */
    private def normalizeObject(
        obj: Map[String, Json],
        selections: Chunk[CompiledSelection],
        parentType: String,
        key: CacheKey,
        path: List[String]
    ): Unit =
        val typename     = objectTypename(obj).getOrElse(parentType)
        val recordFields = mutable.LinkedHashMap.empty[FieldKey, RecordValue]
        // Skip local `@client` fields: a network/operation write-back re-encodes the
        // decoded data (where a client field materializes to its default), which would
        // clobber the locally-written value. Client state is authored only via an
        // explicit `writeFragment` whose node is not client-marked, so it still lands.
        for field <- collectFields(selections, typename) if !field.client do
            obj.get(field.responseName).foreach { value =>
                val fieldKey = fieldPolicies.fieldKey(typename, field, variables)
                recordFields(fieldKey) = buildFieldValue(value, field, path :+ fieldKey.render)
            }
        end for
        mergeRecord(Record(key, recordFields.toMap))
    end normalizeObject

    /** Convert a single response value into its stored [[RecordValue]]. A field
      * with sub-selections is composite (object / list of objects); a field with
      * none is a leaf (scalar / list of scalars). `path` is this value's path
      * BELOW its nearest keyed ancestor, used to key id-less nested objects.
      */
    private def buildFieldValue(
        value: Json,
        field: CompiledField,
        path: List[String]
    ): RecordValue =
        if field.selections.isEmpty then leafValue(value)
        else compositeValue(value, field, path)

    /** A leaf field's value: scalars inline, lists recursed element-wise, `null`
      * explicit. A composite JSON value under a leaf field is a custom scalar
      * whose wire form happens to be an object/array — it is stored inline as-is.
      */
    private def leafValue(value: Json): RecordValue = value match
        case Json.JNull       => RecordValue.Null
        case Json.JArr(items) => RecordValue.RList(items.map(leafValue))
        case scalar           => RecordValue.Scalar(scalar)

    /** A composite field's value: an object becomes a separate record plus a
      * reference, a list recurses (appending the index to the path), `null` stays
      * null.
      */
    private def compositeValue(
        value: Json,
        field: CompiledField,
        path: List[String]
    ): RecordValue = value match
        case Json.JNull => RecordValue.Null
        case Json.JArr(items) =>
            RecordValue.RList(items.zipWithIndex.map { (item, index) =>
                compositeValue(item, field, path :+ index.toString)
            })
        case Json.JObj(objFields) =>
            // `addTypename` semantics at the store boundary: an object arriving
            // without `__typename` — typically a write-back re-encoded from a
            // `mapInto` projection, whose codec emits only the case-class fields —
            // is stamped with the field's STATIC leaf type, so (a) the key generator
            // can still key it as an entity instead of falling back to a path key,
            // and (b) the stored record satisfies the implicit `__typename`
            // selection every object node compiles, keeping the write → read
            // round-trip closed. A concrete wire `__typename` (interfaces/unions)
            // always wins when present.
            val enriched =
                if objFields.contains("__typename") then objFields
                else objFields + ("__typename" -> Json.JStr(field.fieldType.leafType.name))
            val childKey = cacheKeyGenerator
                .cacheKeyForObject(enriched, CacheKeyGeneratorContext(field, variables, path))
                .getOrElse(CacheKey.fromPath(path))
            normalizeObject(
                obj = enriched,
                selections = field.selections,
                parentType = field.fieldType.leafType.name,
                key = childKey,
                // The path RESTARTS at this object's key, the way `normalize` starts it at
                // `rootKey` (:82) and `ApolloStore.writeFragment` starts it at the fragment's
                // entity key. Carrying the response path down instead gave the same entity's
                // id-less children a different key per writing operation, so a write selecting
                // fewer subfields repointed the parent's field at records that were missing the
                // other writer's fields (GAPS.md F-18).
                //
                // A no-op wherever the key came from the path fallback: `CacheKey.fromPath` is
                // `path.mkString(".")`, so `List(childKey.render)` renders the identical string and
                // every deeper append is unchanged. It only bites where an identity exists,
                // which is exactly where it should.
                path = List(childKey.render)
            )
            RecordValue.Reference(CacheReference(childKey))
        case scalar =>
            // A composite field with a scalar value is malformed data; keep it inline
            // rather than dropping it, so the round-trip surfaces the discrepancy.
            RecordValue.Scalar(scalar)

    /** Flatten `selections` into the concrete fields that apply to an object of
      * type `typename`, injecting an implicit `__typename` so the type is captured
      * when the server sends it. Shared with the reader via [[FieldCollector]].
      */
    private def collectFields(
        selections: Chunk[CompiledSelection],
        typename: String
    ): Chunk[CompiledField] =
        FieldCollector.collect(selections, typename, injectTypename = true)

    /** Read an object's `__typename`, when present as a JSON string. */
    private def objectTypename(obj: Map[String, Json]): Maybe[String] =
        Maybe.fromOption(obj.get("__typename").collect { case Json.JStr(t) => t })

    /** Merge `record` into the accumulator through [[merger]] — a later occurrence of
      * the same key in this response is merged onto the earlier one exactly as a
      * later write is merged onto the stored record.
      */
    private def mergeRecord(record: Record): Unit =
        records.update(record.key, merger.merge(Maybe.fromOption(records.get(record.key)), record)._1)
end Normalizer

object Normalizer:
    /** Normalize `data` for `operation` into a set of [[Record]]s.
      *
      * Convenience over the class: derives the root key from the operation kind and
      * runs a fresh [[Normalizer]] that merges with `merger`. `data` is the
      * operation's response `data` map (as encoded by `operation.dataCodec`).
      *
      * @param operation         the operation whose response is being normalized
      * @param data              the response `data` object
      * @param variables         the operation's encoded variables (default none)
      * @param cacheKeyGenerator the key policy (default id-based)
      * @param fieldPolicies     per-field policies (default identity)
      * @param merger            the record merger; `Absent` (the default) takes the
      *                          one an [[kyo.apollo.cache.normalized.ApolloStore]]
      *                          derives from `fieldPolicies`
      */
    def normalize(
        operation: Operation[?],
        data: Map[String, Json],
        variables: Map[String, Json] = Map.empty,
        cacheKeyGenerator: CacheKeyGenerator = CacheKeyGenerator.default,
        fieldPolicies: FieldPolicies = FieldPolicies.empty,
        merger: Maybe[RecordMerger] = Absent
    ): Map[CacheKey, Record] =
        new Normalizer(
            variables,
            CacheKey.rootKey(operation),
            cacheKeyGenerator,
            fieldPolicies,
            merger.getOrElse(RecordMerger.fieldPolicies(fieldPolicies))
        ).normalize(data, operation.rootField)
    end normalize
end Normalizer
