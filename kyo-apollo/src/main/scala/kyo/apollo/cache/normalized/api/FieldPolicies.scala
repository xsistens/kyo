package kyo.apollo.cache.normalized.api

import kyo.Maybe
import kyo.apollo.api.CompiledField
import kyo.apollo.json.Json

/** A registry of [[FieldPolicy]]s, indexed for the three seams that consult them:
  * the normalizer and reader (field-key computation), the reader (read
  * redirects), and the record merger (per-field merges). Policies are keyed by
  * field name — the identity available to all three walkers.
  *
  * Build one with [[FieldPolicies.of]] (varargs) or [[FieldPolicies.fromList]]
  * (handy for splicing in the multi-policy output of [[ConnectionFieldPolicy]]).
  * The empty registry ([[FieldPolicies.empty]]) is the identity policy the store
  * uses until fields are configured — every method then behaves exactly as the
  * unpolicied Phase 04 path did.
  */
final class FieldPolicies private (
    private val keyArgsByField: Map[String, List[String]],
    private val readByField: Map[String, FieldPolicyReadContext => Maybe[CacheKey]],
    private val mergeByField: Map[String, FieldValueMerger]
):

    /** True when no policy is configured, so callers can short-circuit to the
      * default (unpolicied) behaviour.
      */
    def isEmpty: Boolean =
        keyArgsByField.isEmpty && readByField.isEmpty && mergeByField.isEmpty

    /** The storage [[FieldKey]] for `field`, honouring a configured `keyArgs`
      * restriction (keeping only the named arguments) or falling back to the full
      * argument-aware key when the field has no policy.
      */
    def fieldKey(field: CompiledField, variables: Map[String, Json]): String =
        keyArgsByField.get(field.name) match
            case Some(keptArgs) => FieldKey(field, variables, keptArgs)
            case None           => FieldKey(field, variables)

    /** The read-redirect target for `field`, if a [[FieldPolicy.read]] resolver is
      * configured and produces one; otherwise `Absent`.
      */
    def readRedirect(field: CompiledField, variables: Map[String, Json]): Maybe[CacheKey] =
        Maybe
            .fromOption(readByField.get(field.name))
            .flatMap(_(FieldPolicyReadContext(field, variables)))

    /** The custom [[FieldValueMerger]] for a stored field whose base name (the part
      * before any `(args)`) matches a configured [[FieldPolicy.merge]], or `Absent`
      * to merge that field the default way (incoming wins).
      */
    def fieldMerge(fieldBaseName: String): Maybe[FieldValueMerger] =
        Maybe.fromOption(mergeByField.get(fieldBaseName))
end FieldPolicies

object FieldPolicies:
    /** The identity registry: no key restrictions, redirects, or custom merges. */
    val empty: FieldPolicies = new FieldPolicies(Map.empty, Map.empty, Map.empty)

    /** Build a registry from individual policies. */
    def of(policies: FieldPolicy*): FieldPolicies = fromList(policies.toList)

    /** Build a registry from a list of policies — convenient for splicing the
      * multi-policy output of [[ConnectionFieldPolicy]] alongside standalone ones,
      * e.g. `FieldPolicies.fromList(ConnectionFieldPolicy("Query", "feed") :+ other)`.
      * When two policies configure the same field name for the same aspect, the
      * later one wins.
      */
    def fromList(policies: List[FieldPolicy]): FieldPolicies =
        new FieldPolicies(
            policies.collect { case p if p.keyArgs.isDefined => p.fieldName -> p.keyArgs.get }.toMap,
            policies.collect { case p if p.read.isDefined => p.fieldName -> p.read.get }.toMap,
            policies.collect { case p if p.merge.isDefined => p.fieldName -> p.merge.get }.toMap
        )
end FieldPolicies
