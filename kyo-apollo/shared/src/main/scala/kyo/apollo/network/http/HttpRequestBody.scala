package kyo.apollo.network.http

import kyo.Absent
import kyo.Chunk
import kyo.HttpRequest
import kyo.Maybe
import kyo.Present

/** The body of an [[HttpEngine.Request]]: the one piece of a GraphQL request that
  * kyo-http has no single type for. kyo-http carries a request body as a route field
  * typed per form (`bodyText`, `bodyMultipart`, none); the engine seam needs one type
  * for the three forms [[HttpRequestComposer]] produces.
  *
  * A `multipart/form-data` body is a sequence of kyo-http [[kyo.HttpRequest.Part]]s,
  * which carry both the text fields and the files of the
  * graphql-multipart-request-spec. (`kyo.HttpFormCodec` does not fit: it encodes
  * `application/x-www-form-urlencoded`, which has no file parts.)
  */
enum HttpRequestBody derives CanEqual:

    /** No body: a `GET` carries the operation in its URL. */
    case Empty

    /** A JSON document, sent as `application/json`. */
    case Text(json: String)

    /** A `multipart/form-data` body (a file upload): parts without a filename are text
      * fields, parts with one are files. The engine lets the platform set the
      * `Content-Type` with the generated boundary.
      */
    case Multipart(parts: Chunk[HttpRequest.Part])

    /** The JSON document of a [[Text]] body; `Absent` for the other forms. */
    def text: Maybe[String] = this match
        case Text(json) => Present(json)
        case _          => Absent
end HttpRequestBody
