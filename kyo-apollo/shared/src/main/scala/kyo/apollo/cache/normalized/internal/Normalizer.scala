package kyo.apollo.cache.normalized.internal

import kyo.Maybe
import kyo.apollo.api.*
import kyo.apollo.cache.normalized.api.*
import kyo.apollo.json.Json
import kyo.discard
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
  * is returned. Mirrors apollo-kotlin's `Normalizer`.
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
  *                          pagination arguments); defaults to the identity policy
  */
final class Normalizer(
    variables: Map[String, Json],
    rootKey: String,
    cacheKeyGenerator: CacheKeyGenerator,
    fieldPolicies: FieldPolicies = FieldPolicies.empty
):

    /** Records accumulated so far, keyed by [[Record.key]]. Insertion-ordered so
      * the returned map lists the root first, then records in the order they are
      * first encountered — handy for tests and debugging.
      */
    private val records = mutable.LinkedHashMap.empty[String, Record]

    /** Normalize `data` against `rootField`'s selections into a set of records.
      *
      * @param data      the response `data` object (as produced by
      *                  `Operation.dataSchema`)
      * @param rootField the operation's [[Operation.rootField]]; its
      *                  [[CompiledField.selections]] are the top-level fields and
      *                  its leaf type names the root object's GraphQL type
      * @return every record produced, keyed by cache key, with duplicate keys
      *         within this response already merged
      */
    def normalize(data: Map[String, Json], rootField: CompiledField): Map[String, Record] =
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
            path = List(rootKey)
        )
        records.toMap
    end normalize

    /** Build (and record) the [[Record]] for one object, recursing into its
      * composite fields. `key` is the object's already-decided cache key and
      * `path` its rooted response path (used for id-less child keys).
      */
    private def normalizeObject(
        obj: Map[String, Json],
        selections: List[CompiledSelection],
        parentType: String,
        key: String,
        path: List[String]
    ): Unit =
        val typename     = objectTypename(obj).getOrElse(parentType)
        val recordFields = mutable.LinkedHashMap.empty[String, RecordValue]
        // Skip local `@client` fields: a network/operation write-back re-encodes the
        // decoded data (where a client field materializes to its default), which would
        // clobber the locally-written value. Client state is authored only via an
        // explicit `writeFragment` whose node is not client-marked, so it still lands.
        for field <- collectFields(selections, typename) if !field.client do
            obj.get(field.responseName).foreach { value =>
                val fieldKey = fieldPolicies.fieldKey(typename, field, variables)
                recordFields(fieldKey) = buildFieldValue(value, field, path :+ fieldKey)
            }
        end for
        mergeRecord(Record(key, recordFields.toMap))
    end normalizeObject

    /** Convert a single response value into its stored [[RecordValue]]. A field
      * with sub-selections is composite (object / list of objects); a field with
      * none is a leaf (scalar / list of scalars). `path` is the rooted path to
      * this value, used to key id-less nested objects.
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
                key = childKey.key,
                path = path
            )
            RecordValue.Reference(CacheReference(childKey.key))
        case scalar =>
            // A composite field with a scalar value is malformed data; keep it inline
            // rather than dropping it, so the round-trip surfaces the discrepancy.
            RecordValue.Scalar(scalar)

    /** Flatten `selections` into the concrete fields that apply to an object of
      * type `typename`, injecting an implicit `__typename` so the type is captured
      * when the server sends it. Shared with the reader via [[FieldCollector]].
      */
    private def collectFields(
        selections: List[CompiledSelection],
        typename: String
    ): List[CompiledField] =
        FieldCollector.collect(selections, typename, injectTypename = true)

    /** Read an object's `__typename`, when present as a JSON string. */
    private def objectTypename(obj: Map[String, Json]): Maybe[String] =
        Maybe.fromOption(obj.get("__typename").collect { case Json.JStr(t) => t })

    /** Merge `record` into the accumulator, unioning fields (later values win) when
      * the same key appears more than once in a single response.
      */
    private def mergeRecord(record: Record): Unit =
        discard(records.updateWith(record.key) {
            case Some(existing) => Some(existing.copy(fields = existing.fields ++ record.fields))
            case None           => Some(record)
        })
end Normalizer

object Normalizer:
    /** Normalize `data` for `operation` into a set of [[Record]]s.
      *
      * Convenience over the class: derives the root key from the operation kind and
      * runs a fresh [[Normalizer]]. `data` is the operation's response `data` map
      * (as produced by `operation.dataSchema`).
      *
      * @param operation         the operation whose response is being normalized
      * @param data              the response `data` object
      * @param variables         the operation's encoded variables (default none)
      * @param cacheKeyGenerator the key policy (default id-based)
      * @param fieldPolicies     per-field storage-key policies (default identity)
      */
    def normalize(
        operation: Operation[?],
        data: Map[String, Json],
        variables: Map[String, Json] = Map.empty,
        cacheKeyGenerator: CacheKeyGenerator = CacheKeyGenerator.default,
        fieldPolicies: FieldPolicies = FieldPolicies.empty
    ): Map[String, Record] =
        val rootKey = CacheKey.rootKey(operation).key
        new Normalizer(variables, rootKey, cacheKeyGenerator, fieldPolicies)
            .normalize(data, operation.rootField)
    end normalize
end Normalizer
