package kyo.apollo.network.http

import kyo.apollo.network.HttpHeader

/** A received HTTP response: the raw material [[HttpNetworkTransport]] decodes
  * into an [[kyo.apollo.network.ApolloResponse]].
  *
  * The body is held as an already-buffered `String` rather than a streaming
  * reader. Scala.js's `fetch` resolves the whole body via `Response.text()`
  * before we see it, so a lazy reader would buy nothing here; apollo-kotlin's
  * `BufferedSource`-based body reader is the JVM analog we collapse to a `String`
  * on this platform.
  *
  * @param statusCode the HTTP status (e.g. `200`, `404`)
  * @param headers    ordered response headers (duplicates preserved)
  * @param body       the fully-read response body text
  */
final case class HttpResponse(
    statusCode: Int,
    headers: List[HttpHeader] = Nil,
    body: String = ""
):

    /** True when [[statusCode]] is in the 2xx success range. */
    def isSuccessful: Boolean = statusCode >= 200 && statusCode < 300

    /** The first value for `name` (case-insensitive), if the header is present. */
    def header(name: String): Option[String] =
        headers.find(_.name.equalsIgnoreCase(name)).map(_.value)
end HttpResponse
