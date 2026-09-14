package kyo.apollo.cache

import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.cache.normalized.api.CacheKey
import kyo.apollo.cache.normalized.api.FieldKey

/** Shorthands for the opaque cache keys in hand-built test records, each going
  * through the keys' public factories — a test cannot mint a key the production
  * code could not.
  */
object TestKeys:
    /** The storage key of an argument-less field `name`. */
    def fk(name: String): FieldKey = FieldKey(CompiledField(name, CompiledNamedType("String")))

    /** A position-based record key, e.g. `pathKey("QUERY_ROOT", "items", "0")`. */
    def pathKey(segments: String*): CacheKey = CacheKey.fromPath(segments.toList)
end TestKeys
