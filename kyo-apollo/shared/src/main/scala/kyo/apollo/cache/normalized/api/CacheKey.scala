package kyo.apollo.cache.normalized.api

import kyo.apollo.api.Mutation
import kyo.apollo.api.Operation
import kyo.apollo.api.Query
import kyo.apollo.api.Subscription

/** The identity of a [[Record]] in the cache — a thin value type over the
  * record key string.
  *
  * Every normalized object is stored under exactly one `CacheKey`. Root objects
  * use the well-known keys below (one per operation kind); entity objects use a
  * key derived by a [[CacheKeyGenerator]] from their `__typename` and id.
  * Mirrors apollo-kotlin's `CacheKey`.
  *
  * @param key the underlying record key string
  */
final case class CacheKey(key: String):
    override def toString: String = key

object CacheKey:
    /** The well-known root key for query responses. */
    val QueryRoot: CacheKey = CacheKey("QUERY_ROOT")

    /** The well-known root key for mutation responses. */
    val MutationRoot: CacheKey = CacheKey("MUTATION_ROOT")

    /** The well-known root key for subscription responses. */
    val SubscriptionRoot: CacheKey = CacheKey("SUBSCRIPTION_ROOT")

    /** The root record key for `operation`'s response, chosen by operation kind.
      *
      * Normalization starts writing an operation's records at this key, and
      * denormalization starts reading from it. Mirrors apollo-kotlin's
      * `CacheKey.rootKey()` / `QUERY_ROOT` conventions.
      */
    def rootKey(operation: Operation[?]): CacheKey = operation match
        case _: Query[?]        => QueryRoot
        case _: Mutation[?]     => MutationRoot
        case _: Subscription[?] => SubscriptionRoot

    /** Build a key from a typename and object id, e.g. `Country:DE`. The standard
      * shape [[CacheKeyGenerator]] emits when an object carries an id.
      */
    def apply(typename: String, id: String): CacheKey = CacheKey(s"$typename:$id")

    /** Build a position-based key from a rooted response `path`, e.g.
      * `List("QUERY_ROOT", "countries", "0")` becomes `QUERY_ROOT.countries.0`.
      * The fallback [[CacheKeyGenerator]] uses when an object carries no id.
      */
    def fromPath(path: List[String]): CacheKey = CacheKey(path.mkString("."))
end CacheKey
