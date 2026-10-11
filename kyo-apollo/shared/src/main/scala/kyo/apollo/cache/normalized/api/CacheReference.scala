package kyo.apollo.cache.normalized.api

/** A pointer from one [[Record]] to another, by its record key.
  *
  * The normalizer replaces every nested object in a response with one of these
  * so records stay flat and deduplicated: a parent record stores a
  * `CacheReference` (as [[RecordValue.Reference]]) where the nested object used
  * to be, and the referenced record is written separately under [[key]]. The
  * reader follows references the other way — loading the referenced record from
  * the store when it walks back into that field. Mirrors apollo-kotlin's
  * `CacheKey` used as a record value.
  *
  * @param key the [[CacheKey]] of the record this points at
  */
final case class CacheReference(key: CacheKey):
    override def toString: String = s"CacheReference(${key.render})"
