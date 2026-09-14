package kyo.apollo.cache.normalized.api

import kyo.Absent
import kyo.Maybe
import kyo.Present
import kyo.apollo.json.Json

/** A declarative cache-key policy for a GraphQL object type: the fields whose
  * values, together with the type name, form the record key for objects of that
  * type. Mirrors apollo-kotlin's `@typePolicy(keyFields = "...")`.
  *
  * A type declaring `TypePolicy("Country", List("code"))` keys its objects as
  * `Country:<code>` instead of by `id`; multiple key fields are concatenated
  * (in the declared order) so a composite key like `Book:<isbn>+<edition>` is
  * stable regardless of field order in the response.
  *
  * An EMPTY `keyFields` list declares a '''singleton''' type (Apollo Client's
  * `keyFields: []`): every object of the type normalizes into the single record
  * keyed by the bare typename. The shape for one-per-app values like a viewer's
  * playback state — a query snapshot, subscription events, and mutation
  * responses of the type all converge on the same record, so any watcher sees
  * every write (including optimistic overlays).
  *
  * @param typename  the `__typename` this policy applies to
  * @param keyFields the object fields whose scalar values compose the id part of
  *                  the key, tried/joined in the given order; empty for a
  *                  singleton type keyed by the bare typename
  */
final case class TypePolicy(typename: String, keyFields: List[String])

/** A [[CacheKeyGenerator]] driven by a set of [[TypePolicy]] declarations.
  *
  * For an object whose `__typename` has a registered [[TypePolicy]], the key is
  * `Typename:<v1>+<v2>+…` over the policy's key fields; when a key field is
  * absent it falls back to the object's path below its nearest keyed ancestor (so a
  * partially-selected object still gets a stable key). Objects of unpolicied types are keyed by the
  * supplied [[fallback]] (the default id-based generator), so this generalizes
  * [[IdCacheKeyGenerator]] rather than replacing it. Mirrors apollo-kotlin's
  * `TypePolicyCacheKeyGenerator`.
  *
  * @param policies key policies by `__typename`
  * @param fallback the generator used for types without a [[TypePolicy]]
  *                 (default: id-based)
  */
final class TypePolicyCacheKeyGenerator(
    policies: Map[String, TypePolicy],
    fallback: CacheKeyGenerator = CacheKeyGenerator.default
) extends CacheKeyGenerator:

    def cacheKeyForObject(
        obj: Map[String, Json],
        context: CacheKeyGeneratorContext
    ): Maybe[CacheKey] =
        typename(obj)
            .flatMap(t => Maybe.fromOption(policies.get(t)))
            .fold(fallback.cacheKeyForObject(obj, context))(policy =>
                keyFromFields(policy, obj).orElse(pathKey(context))
            )

    /** `Typename:<field values joined by +>`, or `Absent` if any key field is
      * missing or non-scalar (so the caller can fall back to a path key).
      */
    private def keyFromFields(policy: TypePolicy, obj: Map[String, Json]): Maybe[CacheKey] =
        val values = policy.keyFields.map(f =>
            obj.get(f).flatMap(CacheKey.scalarString(_).toOption)
        )
        if values.forall(_.isDefined) then
            Present(CacheKey(policy.typename, values.flatten.mkString("+")))
        else Absent
    end keyFromFields

    /** The path below the nearest keyed ancestor as a key, when one is available. */
    private def pathKey(context: CacheKeyGeneratorContext): Maybe[CacheKey] =
        if context.path.nonEmpty then Present(CacheKey.fromPath(context.path)) else Absent

    private def typename(obj: Map[String, Json]): Maybe[String] =
        Maybe.fromOption(obj.get("__typename").collect { case Json.JStr(t) => t })
end TypePolicyCacheKeyGenerator

object TypePolicyCacheKeyGenerator:
    /** Build a generator from the given policies (id-based fallback for the rest). */
    def of(policies: TypePolicy*): TypePolicyCacheKeyGenerator =
        new TypePolicyCacheKeyGenerator(policies.map(p => p.typename -> p).toMap)
end TypePolicyCacheKeyGenerator
