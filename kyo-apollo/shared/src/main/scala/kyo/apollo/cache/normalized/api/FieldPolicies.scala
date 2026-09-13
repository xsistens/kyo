package kyo.apollo.cache.normalized.api

import kyo.Maybe
import kyo.apollo.api.CompiledField
import kyo.apollo.json.Json

/** A registry of [[FieldPolicy]]s, indexed for the three seams that consult them:
  * the normalizer and reader (field-key computation), the reader (read
  * redirects), and the record merger (per-field merges). Policies are keyed by
  * `(typeName, fieldName)`: each seam names the parent type it is walking (the
  * normalizer and reader know it from the selection tree and the object's
  * `__typename`, the merger reads the record's stored `__typename`), so a policy
  * for `Playlist.tracks` cannot leak onto an unrelated field that happens to
  * share the name `tracks` on another type.
  *
  * Policies bind to the CONCRETE object type: for a field selected through an
  * interface or union, declare the policy on the concrete member type, since the
  * walkers resolve the runtime `__typename`.
  *
  * Build one with [[FieldPolicies.of]] (varargs) or [[FieldPolicies.fromList]]
  * (handy for splicing in the multi-policy output of [[ConnectionFieldPolicy]]).
  * The empty registry ([[FieldPolicies.empty]]) is the identity policy the store
  * uses until fields are configured — every method then behaves exactly as the
  * unpolicied Phase 04 path did.
  */
final class FieldPolicies private (
    private val keyArgsByField: Map[(String, String), List[String]],
    private val readByField: Map[(String, String), FieldPolicyReadContext => Maybe[CacheKey]],
    private val mergeByField: Map[(String, String), FieldValueMerger]
):

    /** True when no policy is configured, so callers can short-circuit to the
      * default (unpolicied) behaviour.
      */
    def isEmpty: Boolean =
        keyArgsByField.isEmpty && readByField.isEmpty && mergeByField.isEmpty

    /** The storage [[FieldKey]] for `field` on an object of type `parentType`,
      * honouring a configured `keyArgs` restriction (keeping only the named
      * arguments) or falling back to the full argument-aware key when the field
      * has no policy.
      */
    def fieldKey(parentType: String, field: CompiledField, variables: Map[String, Json]): String =
        keyArgsByField.get((parentType, field.name)) match
            case Some(keptArgs) => FieldKey(field, variables, keptArgs)
            case None           => FieldKey(field, variables)

    /** The read-redirect target for `field` on an object of type `parentType`, if
      * a [[FieldPolicy.read]] resolver is configured and produces one; otherwise
      * `Absent`.
      */
    def readRedirect(
        parentType: String,
        field: CompiledField,
        variables: Map[String, Json]
    ): Maybe[CacheKey] =
        Maybe
            .fromOption(readByField.get((parentType, field.name)))
            .flatMap(_(FieldPolicyReadContext(field, variables)))

    /** The custom [[FieldValueMerger]] for a stored field of a `parentType` record
      * whose base name (the part before any `(args)`) matches a configured
      * [[FieldPolicy.merge]], or `Absent` to merge that field the default way
      * (incoming wins).
      */
    def fieldMerge(parentType: String, fieldBaseName: String): Maybe[FieldValueMerger] =
        Maybe.fromOption(mergeByField.get((parentType, fieldBaseName)))
end FieldPolicies

object FieldPolicies:
    /** The identity registry: no key restrictions, redirects, or custom merges. */
    val empty: FieldPolicies = new FieldPolicies(Map.empty, Map.empty, Map.empty)

    /** Build a registry from individual policies. */
    def of(policies: FieldPolicy*): FieldPolicies = fromList(policies.toList)

    /** Build a registry from a list of policies — convenient for splicing the
      * multi-policy output of [[ConnectionFieldPolicy]] alongside standalone ones,
      * e.g. `FieldPolicies.fromList(ConnectionFieldPolicy.of(…) :+ other)`.
      * When two policies configure the same `(typeName, fieldName)` for the same
      * aspect, the later one wins.
      */
    def fromList(policies: List[FieldPolicy]): FieldPolicies =
        new FieldPolicies(
            policies.collect {
                case p if p.keyArgs.isDefined => (p.typeName, p.fieldName) -> p.keyArgs.get
            }.toMap,
            policies.collect {
                case p if p.read.isDefined => (p.typeName, p.fieldName) -> p.read.get
            }.toMap,
            policies.collect {
                case p if p.merge.isDefined => (p.typeName, p.fieldName) -> p.merge.get
            }.toMap
        )
end FieldPolicies
