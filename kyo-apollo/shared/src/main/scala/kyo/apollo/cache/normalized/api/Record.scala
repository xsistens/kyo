package kyo.apollo.cache.normalized.api

import kyo.Chunk
import kyo.Maybe
import kyo.apollo.json.Json

/** A value stored under a field of a [[Record]].
  *
  * Deliberately narrow so a record is always a flat, storable shape: a JSON
  * scalar stays inline, a nested object becomes a [[Reference]] to a separate
  * record, lists recurse element-wise, and an absent/`null` value is its own
  * case. Object values never appear here — the normalizer lifts them into their
  * own records and leaves a reference behind. Mirrors apollo-kotlin, where a
  * record value is a scalar, a `CacheKey` reference, a list, or `null`.
  */
sealed trait RecordValue derives CanEqual

object RecordValue:
    /** A JSON scalar held inline: a string, number or boolean.
      *
      * Reuses the existing [[kyo.apollo.json.Json]] scalars rather than inventing a
      * parallel scalar type. `null` is modelled by [[RecordValue.Null]], so a
      * `Scalar` is expected to wrap a non-null scalar ([[Json.JStr]],
      * [[Json.JNum]] or [[Json.JBool]]); [[scalar]] enforces this.
      */
    final case class Scalar(value: Json) extends RecordValue

    /** A pointer to another record — the flattened form of a nested object. */
    final case class Reference(reference: CacheReference) extends RecordValue

    /** A list of values, each itself a [[RecordValue]] (scalars, references or
      * further nested lists).
      */
    final case class RList(items: Chunk[RecordValue]) extends RecordValue

    /** An explicit `null` field value. */
    case object Null extends RecordValue

    /** Wrap a non-null JSON scalar, mapping [[Json.JNull]] to [[Null]].
      *
      * @throws IllegalArgumentException if `value` is a composite ([[Json.JObj]]
      *                                  or [[Json.JArr]]) — those must be
      *                                  normalized into references/lists first.
      */
    def scalar(value: Json): RecordValue = value match
        case Json.JNull                                  => Null
        case Json.JStr(_) | Json.JNum(_) | Json.JBool(_) => Scalar(value)
        case composite =>
            throw IllegalArgumentException(
                s"RecordValue.scalar expects a JSON scalar, got ${composite.getClass.getSimpleName}"
            )

    /** Convenience: a reference value from a [[CacheKey]]. */
    def reference(key: CacheKey): RecordValue = Reference(CacheReference(key))
end RecordValue

/** A single flat, normalized entry in the cache.
  *
  * A record is the deduplicated unit of storage: it holds one object's own
  * scalar fields inline and points at nested objects by [[CacheReference]]
  * rather than embedding them. Records are keyed by [[key]] (a
  * [[CacheKey.key]]), so the same entity referenced from many places is stored
  * once and updated in one place. Mirrors apollo-kotlin's `Record`.
  *
  * @param key      the record's cache key (see [[CacheKey]])
  * @param fields   this object's fields, keyed by [[FieldKey]] (field name plus
  *                 normalized arguments), to their stored [[RecordValue]]
  * @param metadata per-record cache bookkeeping (e.g. expiration stamps written
  *                 from cache headers); empty by default, populated by the store
  */
final case class Record(
    key: String,
    fields: Map[String, RecordValue],
    metadata: Map[String, Json] = Map.empty
):
    /** The field keys stored on this record. */
    def fieldKeys: Set[String] = fields.keySet

    /** Look up a stored field value by its [[FieldKey]]. */
    def get(fieldKey: String): Maybe[RecordValue] = Maybe.fromOption(fields.get(fieldKey))

    /** Every [[CacheReference]] reachable from this record's fields, including
      * those nested inside lists. Useful for garbage collection and for following
      * links during denormalization.
      */
    def references: Set[CacheReference] =
        def collect(value: RecordValue): Set[CacheReference] = value match
            case RecordValue.Reference(ref) => Set(ref)
            case RecordValue.RList(items)   => items.flatMap(collect).toSet
            case _                          => Set.empty
        fields.values.flatMap(collect).toSet
    end references
end Record

object Record:
    /** A record with no metadata. */
    def apply(key: CacheKey, fields: Map[String, RecordValue]): Record =
        Record(key.key, fields, Map.empty)
end Record
