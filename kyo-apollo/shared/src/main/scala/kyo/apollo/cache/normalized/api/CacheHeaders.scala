package kyo.apollo.cache.normalized.api

import kyo.Maybe

/** A small, immutable bag of string-keyed hints attached to a cache read or
  * write.
  *
  * Cache headers let a caller steer store behaviour without widening every
  * method signature — e.g. stamping a write with the date its records were
  * received (so a time-based store can expire them), or asking the store not to
  * persist a particular response. Mirrors apollo-kotlin's `CacheHeaders`: an
  * immutable `Map[String, String]` plus a few well-known keys.
  *
  * @param headers the underlying header name → value map
  */
final case class CacheHeaders(headers: Map[String, String]):
    /** The value for `name`, if present. */
    def headerValue(name: String): Maybe[String] = Maybe.fromOption(headers.get(name))

    /** Whether `name` is set (to any value). */
    def hasHeader(name: String): Boolean = headers.contains(name)

    /** This bag with `name` set to `value` (replacing any prior value). */
    def withHeader(name: String, value: String): CacheHeaders =
        CacheHeaders(headers + (name -> value))
end CacheHeaders

object CacheHeaders:
    /** The empty header bag — the default for reads and writes. */
    val None: CacheHeaders = CacheHeaders(Map.empty)

    /** Well-known: the epoch-millis date a written record was received. A
      * time-based store (see [[kyo.apollo.cache.normalized.MemoryCache]]) stamps this
      * onto each record and expires it once older than its configured TTL. When
      * absent, the store falls back to its own clock at write time.
      */
    val Date: String = "apollo-cache-date"

    /** Well-known: when set to `"true"`, the store must not persist the records of
      * this write (a no-op merge that reports no changed keys).
      */
    val DoNotStore: String = "do-not-store"

    /** Build a header bag from name/value pairs. */
    def of(pairs: (String, String)*): CacheHeaders = CacheHeaders(pairs.toMap)
end CacheHeaders
