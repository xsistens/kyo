package kyo.apollo.cache.normalized

import kyo.Absent
import kyo.Maybe
import kyo.Present
import kyo.apollo.cache.normalized.api.FieldKey
import kyo.apollo.cache.normalized.api.FieldPolicies
import kyo.apollo.cache.normalized.api.Record
import kyo.apollo.cache.normalized.api.RecordValue
import kyo.apollo.json.Json

/** The policy that merges an incoming [[Record]] onto whatever is already stored
  * under its key, reporting which field keys changed. Mirrors apollo-kotlin's
  * `RecordMerger`.
  *
  * The [[default]] is the field-wise union ("incoming wins") shared by every
  * backend (see [[NormalizedCache.mergeRecords]]). [[fieldPolicies]] wraps that
  * with per-field overrides from a [[FieldPolicies]] registry, so a
  * [[kyo.apollo.cache.normalized.api.ConnectionFieldPolicy]] can union paginated
  * edges instead of replacing them. A [[NormalizedCache]] backend applies the
  * merger the store hands it at write time, keeping merge policy out of the
  * storage layer.
  */
trait RecordMerger:
    /** Merge `incoming` onto the optional `existing` record of the same key,
      * returning the merged record and the set of field keys whose value changed
      * (a brand-new record reports all of its own fields).
      */
    def merge(existing: Maybe[Record], incoming: Record): (Record, Set[FieldKey])
end RecordMerger

object RecordMerger:
    /** The default field-wise union, delegating to [[NormalizedCache.mergeRecords]]. */
    val default: RecordMerger =
        (existing, incoming) => NormalizedCache.mergeRecords(existing, incoming)

    /** A merger that applies `policies`' per-field merges, falling back to the
      * default union for fields without one. Returns [[default]] unchanged when the
      * registry is empty, so the unpolicied path pays nothing.
      */
    def fieldPolicies(policies: FieldPolicies): RecordMerger =
        if policies.isEmpty then default
        else new FieldPolicyRecordMerger(policies)
end RecordMerger

/** Merges records field-by-field, consulting `policies` for a per-field override
  * (keyed by the record's type and the field's base name, dropping any `(args)`
  * suffix) and otherwise taking the incoming value.
  *
  * The record's type comes from its stored `__typename`: the [[internal.Normalizer]]
  * stamps one onto every object it normalizes (root included), so a policied
  * record always carries it. A record without one (e.g. hand-written through a
  * lower-level seam) simply matches no policy and merges the default way.
  */
final private class FieldPolicyRecordMerger(policies: FieldPolicies) extends RecordMerger:

    def merge(existing: Maybe[Record], incoming: Record): (Record, Set[FieldKey]) =
        existing match
            case Absent => (incoming, incoming.fieldKeys)
            case Present(old) =>
                val typename = recordTypename(incoming).orElse(recordTypename(old))
                var changed  = Set.empty[FieldKey]
                val mergedIncoming = incoming.fields.map { case (fieldKey, incomingValue) =>
                    val oldValue = old.get(fieldKey)
                    val newValue = typename
                        .flatMap(t => policies.fieldMerge(t, fieldKey.baseName))
                        .map(mergeFn => mergeFn(oldValue, incomingValue))
                        .getOrElse(incomingValue)
                    if !oldValue.contains(newValue) then changed += fieldKey
                    fieldKey -> newValue
                }
                val merged =
                    Record(incoming.key, old.fields ++ mergedIncoming, old.metadata ++ incoming.metadata)
                (merged, changed)

    /** The record's stored `__typename`, when present as a string scalar. */
    private def recordTypename(record: Record): Maybe[String] =
        record.get(FieldKey.Typename).collect { case RecordValue.Scalar(Json.JStr(t)) => t }
end FieldPolicyRecordMerger
