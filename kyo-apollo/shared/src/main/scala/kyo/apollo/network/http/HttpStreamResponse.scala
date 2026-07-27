package kyo.apollo.network.http

import kyo.*
import kyo.apollo.network.HttpHeader

/** A received HTTP response whose body may be streamed — the streaming counterpart
  * of [[HttpResponse]], used only by the incremental-delivery (`@defer`) path.
  *
  * A `multipart/mixed` response arrives as a [[HttpStreamBody.Chunked]] stream of
  * UTF-8 text chunks pulled from the `fetch` body reader as they land; every other
  * response (and every non-streaming engine) is a single
  * [[HttpStreamBody.Buffered]] string, so the ordinary path is untouched.
  *
  * @param statusCode the HTTP status
  * @param headers    ordered response headers
  * @param body       the buffered or chunked body
  */
final case class HttpStreamResponse(
    statusCode: Int,
    headers: List[HttpHeader] = Nil,
    body: HttpStreamBody = HttpStreamBody.Buffered("")
):

    /** True when [[statusCode]] is in the 2xx success range. */
    def isSuccessful: Boolean = statusCode >= 200 && statusCode < 300

    /** The first value for `name` (case-insensitive), if the header is present. */
    def header(name: String): Option[String] =
        headers.find(_.name.equalsIgnoreCase(name)).map(_.value)
end HttpStreamResponse

/** The body of an [[HttpStreamResponse]]: either a fully-buffered string (the
  * default for non-streaming engines and non-multipart responses) or a live stream
  * of UTF-8 text chunks (a `multipart/mixed` incremental-delivery body).
  */
enum HttpStreamBody:
    case Buffered(text: String)
    case Chunked(chunks: Stream[String, Async & Scope])
