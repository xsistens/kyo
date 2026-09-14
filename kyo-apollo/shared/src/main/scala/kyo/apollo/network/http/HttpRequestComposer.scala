package kyo.apollo.network.http

import java.nio.charset.StandardCharsets
import kyo.Absent
import kyo.Chunk
import kyo.HttpHeaders
import kyo.HttpMethod
import kyo.HttpRequest
import kyo.HttpUrl
import kyo.Present
import kyo.Span
import kyo.apollo.Upload
import kyo.apollo.api.Defer
import kyo.apollo.api.OperationRequestBody
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import scala.collection.immutable.VectorMap

/** Lowers an [[ApolloRequest]] into a wire-level [[HttpEngine.Request]].
  *
  * Two shapes, chosen by [[ApolloRequest.httpMethod]] (falling back to
  * `defaultHttpMethod`):
  *   - `GET` — the operation encoded as URL query params (`query`,
  *     `operationName`, `variables`, and `extensions` when present), each
  *     percent-encoded. Suitable for cacheable reads and GET-based APQ.
  *   - every other method (`POST` by default) — a JSON body built by
  *     [[OperationRequestBody]], the single source of request-body serialization.
  *     The composer never hand-builds `{ query, … }`; it delegates and only feeds an
  *     `extensions` map in.
  *
  * APQ handling is expressed purely through the two [[ApolloRequest]] flags:
  *   - `sendApqExtensions` adds the `persistedQuery` extension (version 1 plus
  *     the SHA-256 of the document — see [[Sha256]]), threaded through
  *     `OperationRequestBody`'s existing `extensions` parameter so that type
  *     stays APQ-agnostic.
  *   - `sendDocument = false` omits the `query` field entirely — the persisted
  *     hash stands in for the document on a registered-query hit.
  *
  * @param defaultHttpMethod the method used when a request does not pin one
  *                          (`POST`, matching apollo-kotlin's default)
  */
final class HttpRequestComposer(defaultHttpMethod: HttpMethod = HttpMethod.POST):

    /** Compose the [[HttpEngine.Request]] for `request` against `serverUrl`. */
    def compose[D](serverUrl: HttpUrl, request: ApolloRequest[D]): HttpEngine.Request =
        val method = request.httpMethod.getOrElse(defaultHttpMethod)
        if method == HttpMethod.GET then composeGet(serverUrl, request)
        else composeBody(method, serverUrl, request)
    end compose

    private def composeBody[D](
        method: HttpMethod,
        serverUrl: HttpUrl,
        request: ApolloRequest[D]
    ): HttpEngine.Request =
        val extensions = apqExtensions(request)
        val bodyJson =
            if request.sendDocument then OperationRequestBody(request.operation, extensions)
            else bodyWithoutDocument(request, extensions)
        val (nulledBody, uploads) = extractUploads(bodyJson, "")
        if uploads.isEmpty then
            // No file variables — the ordinary JSON body.
            HttpEngine.request(
                method,
                serverUrl,
                postHeaders(defers(request)).concat(request.httpHeaders),
                HttpRequestBody.Text(bodyJson.render)
            )
        else
            // graphql-multipart-request-spec: `operations` (files nulled) + `map` + parts.
            HttpEngine.request(
                method,
                serverUrl,
                multipartHeaders(defers(request)).concat(request.httpHeaders),
                uploadForm(nulledBody, uploads)
            )
        end if
    end composeBody

    /** Walk `json`, replacing every [[Json.JUpload]] with `null` and collecting each
      * upload's dotted path (relative to the operations object, e.g. `variables.file`,
      * `variables.files.0`), blob and filename — in document order.
      */
    private def extractUploads(
        json: Json,
        path: String
    ): (Json, List[(String, Upload)]) = json match
        case Json.JUpload(upload) => (Json.JNull, List((path, upload)))
        case Json.JObj(fields) =>
            val (newFields, uploads) =
                fields.foldLeft((VectorMap.empty[String, Json], List.empty[(String, Upload)])) {
                    case ((accFields, accUploads), (key, value)) =>
                        val childPath                = if path.isEmpty then key else s"$path.$key"
                        val (newValue, childUploads) = extractUploads(value, childPath)
                        (accFields.updated(key, newValue), accUploads ++ childUploads)
                }
            (Json.JObj(newFields), uploads)
        case Json.JArr(items) =>
            val (newItems, uploads) =
                items.zipWithIndex.foldLeft((Vector.empty[Json], List.empty[(String, Upload)])) {
                    case ((accItems, accUploads), (value, index)) =>
                        val (newValue, childUploads) = extractUploads(value, s"$path.$index")
                        (accItems :+ newValue, accUploads ++ childUploads)
                }
            (Json.JArr(Chunk.from(newItems)), uploads)
        case other => (other, Nil)

    /** Assemble the multipart body from the file-nulled `operations` body and the
      * ordered uploads: `operations` + `map` text parts, then one file part each.
      */
    private def uploadForm(
        operations: Json,
        uploads: List[(String, Upload)]
    ): HttpRequestBody =
        val indexed = uploads.zipWithIndex
        val map = Json.JObj(VectorMap.from(indexed.map { case ((path, _), index) =>
            index.toString -> Json.JArr(Chunk(Json.JStr(path)))
        }))
        val files = indexed.map { case ((_, upload), index) =>
            HttpRequest.Part(index.toString, Present(upload.fileName), Present(upload.contentType), upload.data)
        }
        HttpRequestBody.Multipart(
            Chunk(textPart("operations", operations.render), textPart("map", map.render)).concat(Chunk.from(files))
        )
    end uploadForm

    /** A text field of a multipart body: no filename, no content type. */
    private def textPart(name: String, value: String): HttpRequest.Part =
        HttpRequest.Part(name, Absent, Absent, Span.fromUnsafe(value.getBytes(StandardCharsets.UTF_8)))

    /** Headers for a multipart upload — like [[postHeaders]] but WITHOUT
      * `Content-Type`, which the platform sets (with the generated boundary).
      */
    private def multipartHeaders(deferring: Boolean): HttpHeaders =
        HttpHeaders.empty.add("Accept", accept(deferring))

    private def composeGet[D](
        serverUrl: HttpUrl,
        request: ApolloRequest[D]
    ): HttpEngine.Request =
        val extensions = apqExtensions(request)
        val params     = VectorMap.newBuilder[String, String]
        if request.sendDocument then params += "query"     -> request.operation.document
        params += "operationName"                          -> request.operation.name
        params += "variables"                              -> request.operation.variables.render
        if extensions.nonEmpty then params += "extensions" -> Json.JObj(extensions).render
        val queryString = params
            .result()
            .map((k, v) => s"${encode(k)}=${encode(v)}")
            .mkString("&")
        val query = serverUrl.rawQuery match
            case Present(existing) => s"$existing&$queryString"
            case Absent            => queryString
        HttpEngine.request(
            HttpMethod.GET,
            serverUrl.copy(rawQuery = Present(query)),
            getHeaders(defers(request)).concat(request.httpHeaders),
            HttpRequestBody.Empty
        )
    end composeGet

    /** The APQ `extensions` map for this request, or empty when APQ is off. */
    private def apqExtensions[D](request: ApolloRequest[D]): Map[String, Json] =
        if !request.sendApqExtensions then Map.empty
        else
            Map(
                "persistedQuery" -> Json.JObj(
                    VectorMap(
                        "version"    -> Json.JInt(1),
                        "sha256Hash" -> Json.JStr(Sha256.hex(request.operation.document))
                    )
                )
            )

    /** The POST body for an APQ request that omits the document: everything
      * `OperationRequestBody` emits except `query`.
      */
    private def bodyWithoutDocument[D](
        request: ApolloRequest[D],
        extensions: Map[String, Json]
    ): Json =
        val fields = VectorMap.newBuilder[String, Json]
        fields += "operationName"                          -> Json.JStr(request.operation.name)
        fields += "variables"                              -> request.operation.variables
        if extensions.nonEmpty then fields += "extensions" -> Json.JObj(extensions)
        Json.JObj(fields.result())
    end bodyWithoutDocument

    /** Percent-encode `value` exactly as JavaScript's `encodeURIComponent`: leave the
      * unreserved set `A-Za-z0-9 - _ . ! ~ * ' ( )` intact and uppercase-hex the
      * UTF-8 bytes of everything else. Portable across JS/Wasm/JVM/Native, byte-for-
      * byte identical to the former `js.URIUtils.encodeURIComponent`.
      */
    private def encode(value: String): String =
        val sb = StringBuilder()
        value.getBytes(StandardCharsets.UTF_8).foreach { b =>
            val c = (b & 0xff).toChar
            if HttpRequestComposer.uriUnreserved.indexOf(c.toInt) >= 0 then sb += c
            else sb ++= f"%%${b & 0xff}%02X"
        }
        sb.result()
    end encode

    /** Whether the operation uses `@defer` — decides the `Accept` header, so the
      * server may reply with a `multipart/mixed` incremental stream.
      */
    private def defers[D](request: ApolloRequest[D]): Boolean =
        Defer.has(request.operation.rootField)

    /** The `Accept` header value, widened to advertise incremental delivery when the
      * operation defers (per the `deferSpec=20220824` incremental-delivery spec).
      */
    private def accept(deferring: Boolean): String =
        val base = "application/graphql-response+json, application/json"
        if deferring then s"multipart/mixed; deferSpec=20220824, $base" else base

    private def postHeaders(deferring: Boolean): HttpHeaders =
        HttpHeaders.empty.add("Content-Type", "application/json").add("Accept", accept(deferring))

    private def getHeaders(deferring: Boolean): HttpHeaders =
        HttpHeaders.empty.add("Accept", accept(deferring))
end HttpRequestComposer

object HttpRequestComposer:
    /** The `encodeURIComponent` unreserved set: characters left un-escaped. */
    private val uriUnreserved =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.!~*'()"
end HttpRequestComposer
