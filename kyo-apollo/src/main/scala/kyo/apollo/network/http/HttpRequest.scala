package kyo.apollo.network.http

import kyo.apollo.network.HttpHeader
import kyo.apollo.network.HttpMethod
import scala.scalajs.js

/** A ready-to-send HTTP request: the wire-level shape an [[ApolloRequest]] is
  * lowered to by [[HttpRequestComposer]] before an [[HttpEngine]] executes it.
  *
  * Deliberately transport-agnostic — it carries no GraphQL knowledge, only the
  * method, absolute `url`, ordered `headers`, and a body. The body is either a
  * textual `body` (`None` for a GET, `Some(json)` for a plain POST) or a
  * `multipart/form-data` `formBody` (a file upload); **exactly one** is set.
  * Mirrors the request half of apollo-kotlin's `HttpRequest`.
  *
  * @param method   the HTTP verb (`Get` encodes the operation in the URL, `Post`
  *                 carries a body)
  * @param url      the absolute request URL, including any GET query string
  * @param headers  ordered request headers (duplicates preserved)
  * @param body     a textual request body, present only for methods that carry one
  * @param formBody a `multipart/form-data` body (file upload); when set, the engine
  *                 sends a `FormData` and lets the platform set the `Content-Type`
  *                 boundary, so `body` must be `None`
  */
final case class HttpRequest(
    method: HttpMethod,
    url: String,
    headers: List[HttpHeader] = Nil,
    body: Option[String] = None,
    formBody: Option[HttpForm] = None
)

/** A `multipart/form-data` body: ordered text `fields` plus `files`. Generic (no
  * GraphQL knowledge) — the composer packs the graphql-multipart-request-spec's
  * `operations`/`map` as text fields and the uploads as file parts.
  */
final case class HttpForm(fields: List[(String, String)], files: List[HttpFormFile])

/** One file part of an [[HttpForm]]: the form field name, the JS `Blob`/`File`, and
  * the filename to send.
  */
final case class HttpFormFile(fieldName: String, blob: js.Any, fileName: String)
