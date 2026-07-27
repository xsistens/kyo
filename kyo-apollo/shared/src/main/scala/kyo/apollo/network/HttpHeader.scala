package kyo.apollo.network

/** A single HTTP header name/value pair.
  *
  * Headers are carried as an ordered `List[HttpHeader]` (never a `Map`) so that
  * ordering is preserved and duplicate names — legal for headers like
  * `Set-Cookie` — survive round-trips. Mirrors apollo-kotlin's `HttpHeader`.
  */
final case class HttpHeader(name: String, value: String)
