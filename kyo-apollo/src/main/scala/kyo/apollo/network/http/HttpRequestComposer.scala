package kyo.apollo.network.http

import kyo.Chunk
import kyo.apollo.Upload
import kyo.apollo.api.Defer
import kyo.apollo.api.OperationRequestBody
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.HttpMethod
import scala.collection.immutable.VectorMap

/** Lowers an [[ApolloRequest]] into a wire-level [[HttpRequest]].
  *
  * Two shapes, chosen by [[ApolloRequest.httpMethod]] (falling back to
  * `defaultHttpMethod`):
  *   - `Post` — a JSON body built by [[OperationRequestBody]], the single source
  *     of request-body serialization. The composer never hand-builds
  *     `{ query, … }`; it delegates and only feeds an `extensions` map in.
  *   - `Get` — the operation encoded as URL query params (`query`,
  *     `operationName`, `variables`, and `extensions` when present), each
  *     percent-encoded. Suitable for cacheable reads and GET-based APQ.
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
  *                          (`Post`, matching apollo-kotlin's default)
  */
final class HttpRequestComposer(defaultHttpMethod: HttpMethod = HttpMethod.Post):

    /** Compose the [[HttpRequest]] for `request` against `serverUrl`. */
    def compose[D](serverUrl: String, request: ApolloRequest[D]): HttpRequest =
        request.httpMethod.getOrElse(defaultHttpMethod) match
            case HttpMethod.Get  => composeGet(serverUrl, request)
            case HttpMethod.Post => composePost(serverUrl, request)

    private def composePost[D](
        serverUrl: String,
        request: ApolloRequest[D]
    ): HttpRequest =
        val extensions = apqExtensions(request)
        val bodyJson =
            if request.sendDocument then OperationRequestBody(request.operation, extensions)
            else bodyWithoutDocument(request, extensions)
        val (nulledBody, uploads) = extractUploads(bodyJson, "")
        if uploads.isEmpty then
            // No file variables — the ordinary JSON POST, byte-identical to before.
            HttpRequest(
                method = HttpMethod.Post,
                url = serverUrl,
                headers = postHeaders(defers(request)) ++ request.httpHeaders,
                body = Some(bodyJson.render)
            )
        else
            // graphql-multipart-request-spec: `operations` (files nulled) + `map` + parts.
            HttpRequest(
                method = HttpMethod.Post,
                url = serverUrl,
                headers = multipartHeaders(defers(request)) ++ request.httpHeaders,
                body = None,
                formBody = Some(uploadForm(nulledBody, uploads))
            )
        end if
    end composePost

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

    /** Assemble the multipart form from the file-nulled `operations` body and the
      * ordered uploads: `operations` + `map` text fields, one file part each.
      */
    private def uploadForm(
        operations: Json,
        uploads: List[(String, Upload)]
    ): HttpForm =
        val indexed = uploads.zipWithIndex
        val map = Json.JObj(VectorMap.from(indexed.map { case ((path, _), index) =>
            index.toString -> Json.JArr(Chunk(Json.JStr(path)))
        }))
        val files = indexed.map { case ((_, upload), index) =>
            HttpFormFile(index.toString, upload.data, upload.fileName, upload.contentType)
        }
        HttpForm(
            fields = List("operations" -> operations.render, "map" -> map.render),
            files = files
        )
    end uploadForm

    /** POST headers for a multipart upload — like [[postHeaders]] but WITHOUT
      * `Content-Type`, which the platform sets (with the generated boundary).
      */
    private def multipartHeaders(deferring: Boolean): List[HttpHeader] =
        List(accept(deferring))

    private def composeGet[D](
        serverUrl: String,
        request: ApolloRequest[D]
    ): HttpRequest =
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
        val separator = if serverUrl.contains("?") then "&" else "?"
        HttpRequest(
            method = HttpMethod.Get,
            url = s"$serverUrl$separator$queryString",
            headers = getHeaders(defers(request)) ++ request.httpHeaders,
            body = None
        )
    end composeGet

    /** The APQ `extensions` map for this request, or empty when APQ is off. */
    private def apqExtensions[D](request: ApolloRequest[D]): Map[String, Json] =
        if !request.sendApqExtensions then Map.empty
        else
            Map(
                "persistedQuery" -> Json.JObj(
                    VectorMap(
                        "version"    -> Json.JNum(1),
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
        value.getBytes(java.nio.charset.StandardCharsets.UTF_8).foreach { b =>
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
    private def accept(deferring: Boolean): HttpHeader =
        val base = "application/graphql-response+json, application/json"
        HttpHeader("Accept", if deferring then s"multipart/mixed; deferSpec=20220824, $base" else base)

    private def postHeaders(deferring: Boolean): List[HttpHeader] = List(
        HttpHeader("Content-Type", "application/json"),
        accept(deferring)
    )

    private def getHeaders(deferring: Boolean): List[HttpHeader] = List(accept(deferring))
end HttpRequestComposer

object HttpRequestComposer:
    /** The `encodeURIComponent` unreserved set: characters left un-escaped. */
    private val uriUnreserved =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_.!~*'()"
end HttpRequestComposer
