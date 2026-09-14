package kyo.apollo.cache.normalized.api

import kyo.Absent
import kyo.Maybe
import kyo.Present
import kyo.apollo.api.Mutation
import kyo.apollo.api.Operation
import kyo.apollo.api.Query
import kyo.apollo.api.Subscription
import kyo.apollo.json.Json

/** The identity of a [[Record]] in the cache — an opaque type over the record
  * key string, so a record key can never be confused with a [[FieldKey]] or an
  * arbitrary string.
  *
  * Every normalized object is stored under exactly one `CacheKey`. Root objects
  * use the well-known keys below (one per operation kind); entity objects use a
  * key derived by a [[CacheKeyGenerator]] from their `__typename` and id, and
  * id-less objects a position-based key ([[CacheKey.fromPath]]). Those are the
  * only ways to obtain one. Mirrors apollo-kotlin's `CacheKey`.
  */
opaque type CacheKey = String

object CacheKey:
    /** The well-known root key for query responses. */
    val QueryRoot: CacheKey = "QUERY_ROOT"

    /** The well-known root key for mutation responses. */
    val MutationRoot: CacheKey = "MUTATION_ROOT"

    /** The well-known root key for subscription responses. */
    val SubscriptionRoot: CacheKey = "SUBSCRIPTION_ROOT"

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
      * shape [[CacheKeyGenerator]] emits when an object carries an id. An empty
      * id yields the bare typename, the singleton-record key an empty-keyFields
      * [[TypePolicy]] produces.
      */
    def apply(typename: String, id: String): CacheKey =
        if id.isEmpty then typename else s"$typename:$id"

    /** Build a position-based key from a `path`, e.g. `List("QUERY_ROOT",
      * "countries", "0")` becomes `QUERY_ROOT.countries.0`. The fallback
      * [[CacheKeyGenerator]] uses when an object carries no id.
      *
      * The path starts at the object's nearest keyed ancestor, so under an
      * identified parent the key is `Album:1.images.0` rather than the path the
      * response happened to take to get there.
      */
    def fromPath(path: List[String]): CacheKey = path.mkString(".")

    /** Render a JSON scalar as its raw id string (`42`, not `"42"`); composites and
      * `null` yield `Absent` and are not usable as key parts. The one scalar-to-id
      * rendering: every key generator, redirect resolver and masked-fragment
      * identity check goes through here, so they cannot disagree on a number's
      * spelling.
      */
    def scalarString(json: Json): Maybe[String] = json match
        case Json.JStr(s)                               => Present(s)
        case Json.JInt(_) | Json.JDec(_) | Json.JNum(_) => Present(json.render)
        case Json.JBool(b)                              => Present(b.toString)
        case _                                          => Absent

    extension (key: CacheKey)
        /** The key's string form — for messages, diagnostics and serialization
          * boundaries (e.g. the devtools cache dump). Never feed it back as a key.
          */
        def render: String = key
    end extension

    given CanEqual[CacheKey, CacheKey] = CanEqual.derived
end CacheKey
