package kyo.apollo.network.http

import java.nio.charset.StandardCharsets
import kyo.Absent
import kyo.Chunk
import kyo.HttpHeaders
import kyo.HttpMethod
import kyo.HttpUrl
import kyo.Present
import kyo.Schema
import kyo.Span
import kyo.apollo.Upload
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledFragment
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.DeferDirective
import kyo.apollo.api.JsonCodec
import kyo.apollo.api.Query
import kyo.apollo.api.ScalarCodec
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.TestIds
import scala.collection.immutable.VectorMap

/** Tests that [[HttpRequestComposer]] lowers an [[ApolloRequest]] into the right
  * wire [[HttpEngine.Request]]: POST JSON body vs GET query params, default and
  * per-request headers, and the APQ `persistedQuery` extension / document
  * omission driven by the request flags.
  */
class HttpRequestComposerSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** A minimal query with one `Int` variable — enough to exercise body/param
      * composition without pulling in the example module's richer operations.
      */
    final case class MiniQuery(limit: Int) extends Query.Normalizable[Int]:
        def name: String              = "Mini"
        def document: String          = "query Mini($limit: Int!) { x }"
        val dataCodec: JsonCodec[Int] = JsonCodec.fromSchema[Int]
        def rootField: CompiledField  = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json           = Json.JObj(VectorMap("limit" -> SchemaJson.encode(limit)))
    end MiniQuery

    /** Same shape as [[MiniQuery]] but its selection tree carries an anonymous
      * `@defer` fragment, so [[Defer.has]] is true and the Accept header widens.
      */
    final case class DeferQuery(limit: Int) extends Query.Normalizable[Int]:
        def name: String              = "Mini"
        def document: String          = "query Mini($limit: Int!) { x }"
        val dataCodec: JsonCodec[Int] = JsonCodec.fromSchema[Int]
        def rootField: CompiledField = CompiledField(
            "data",
            CompiledNamedType("Query"),
            selections = Chunk(
                CompiledFragment(
                    "",
                    Chunk.empty,
                    Chunk(CompiledField("x", CompiledNamedType("Int"))),
                    defer = Present(DeferDirective("d"))
                )
            )
        )
        def variables: Json = Json.JObj(VectorMap("limit" -> SchemaJson.encode(limit)))
    end DeferQuery

    /** A query with an `Upload` variable — a file can ride a query as well as a mutation. */
    final case class PreviewQuery(file: Upload) extends Query.Normalizable[Int]:
        def name: String              = "Preview"
        def document: String          = "query Preview($file: Upload!) { preview(file: $file) }"
        val dataCodec: JsonCodec[Int] = JsonCodec.fromSchema[Int]
        def rootField: CompiledField  = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json           = Json.JObj(VectorMap("file" -> ScalarCodec.upload.encode(file)))
    end PreviewQuery

    private val composer = HttpRequestComposer()
    private val urlText  = "https://example.com/graphql"
    private val url      = HttpUrl(Present("https"), "example.com", 443, "/graphql", Absent)

    private val plainAccept = "application/graphql-response+json, application/json"
    private val deferAccept = s"multipart/mixed; deferSpec=20220824, $plainAccept"

    "HttpRequestComposer" - {

        "default method is POST with the OperationRequestBody JSON body" in {
            val request = ApolloRequest(MiniQuery(5), TestIds.requestUuid)
            val http    = composer.compose(url, request)

            assert(http.method == HttpMethod.POST)
            assert(http.url.full == urlText)
            assert(
                http.fields.body ==
                    HttpRequestBody.Text(
                        """{"query":"query Mini($limit: Int!) { x }","operationName":"Mini","variables":{"limit":5}}"""
                    )
            )
        }

        "POST carries JSON content-type / accept headers, then per-request ones" in {
            val request =
                ApolloRequest(MiniQuery(1), TestIds.requestUuid, httpHeaders = HttpHeaders.empty.add("Authorization", "Bearer t"))
            val http = composer.compose(url, request)

            assert(
                http.headers ==
                    HttpHeaders.empty
                        .add("Content-Type", "application/json")
                        .add("Accept", "application/graphql-response+json, application/json")
                        .add("Authorization", "Bearer t")
            )
        }

        "GET encodes the operation as URL query params and sends no body" in {
            val request = ApolloRequest(MiniQuery(5), TestIds.requestUuid, httpMethod = Present(HttpMethod.GET))
            val http    = composer.compose(url, request)

            assert(http.method == HttpMethod.GET)
            assert(http.fields.body == HttpRequestBody.Empty)
            // query, operationName, variables — each percent-encoded, joined with '&'.
            assert(
                http.url.full ==
                    urlText + "?query=query%20Mini(%24limit%3A%20Int!)%20%7B%20x%20%7D" +
                    "&operationName=Mini&variables=%7B%22limit%22%3A5%7D"
            )
        }

        "GET appends params with '&' when the URL already has a query string" in {
            val request = ApolloRequest(MiniQuery(1), TestIds.requestUuid, httpMethod = Present(HttpMethod.GET))
            val http    = composer.compose(url.copy(rawQuery = Present("trace=1")), request)
            assert(http.url.full.startsWith(urlText + "?trace=1&query="))
        }

        "APQ POST adds the persistedQuery extension with the document sha256" in {
            val request = ApolloRequest(MiniQuery(2), TestIds.requestUuid, sendApqExtensions = true)
            val http    = composer.compose(url, request)

            val hash = Sha256.hex(MiniQuery(2).document)
            assert(
                http.fields.body ==
                    HttpRequestBody.Text(
                        s"""{"query":"query Mini($$limit: Int!) { x }","operationName":"Mini","variables":{"limit":2},""" +
                            s""""extensions":{"persistedQuery":{"version":1,"sha256Hash":"$hash"}}}"""
                    )
            )
        }

        "APQ with sendDocument=false omits the query field from the POST body" in {
            val request =
                ApolloRequest(MiniQuery(2), TestIds.requestUuid, sendApqExtensions = true, sendDocument = false)
            val http = composer.compose(url, request)

            val hash = Sha256.hex(MiniQuery(2).document)
            assert(
                http.fields.body ==
                    HttpRequestBody.Text(
                        s"""{"operationName":"Mini","variables":{"limit":2},""" +
                            s""""extensions":{"persistedQuery":{"version":1,"sha256Hash":"$hash"}}}"""
                    )
            )
            // No `query` field on the wire when the document is withheld.
            assert(!http.fields.body.text.exists(_.contains("\"query\"")))
        }

        "a @defer operation widens the POST Accept header to multipart/mixed" in {
            val http = composer.compose(url, ApolloRequest(DeferQuery(1), TestIds.requestUuid))
            assert(http.headers.getAll("Accept").contains(deferAccept))
            assert(!http.headers.getAll("Accept").contains(plainAccept))
        }

        "a @defer operation widens the GET Accept header too" in {
            val request = ApolloRequest(DeferQuery(1), TestIds.requestUuid, httpMethod = Present(HttpMethod.GET))
            assert(composer.compose(url, request).headers.getAll("Accept").contains(deferAccept))
        }

        "a non-defer operation keeps the plain Accept header" in {
            val http = composer.compose(url, ApolloRequest(MiniQuery(1), TestIds.requestUuid))
            assert(http.headers.getAll("Accept").contains(plainAccept))
        }

        "a GET request carrying an Upload variable is sent as multipart POST" in {
            val file   = Upload(Span.from("hello".getBytes(StandardCharsets.UTF_8)), "a.txt", "text/plain")
            val pinned = ApolloRequest(PreviewQuery(file), TestIds.requestUuid, httpMethod = Present(HttpMethod.GET))
            // A pinned GET and a GET default both switch.
            Seq(
                composer.compose(url, pinned),
                HttpRequestComposer(defaultHttpMethod = HttpMethod.GET).compose(url, ApolloRequest(PreviewQuery(file), TestIds.requestUuid))
            ).foreach { http =>
                assert(http.method == HttpMethod.POST, s"sent as ${http.method.name} ${http.url.full}")
                assert(http.url.rawQuery == Absent, s"the operation rode the query string: ${http.url.full}")
                http.fields.body match
                    case HttpRequestBody.Multipart(parts) =>
                        def text(name: String): String =
                            parts.find(_.name == name).map(p => new String(p.data.toArray, StandardCharsets.UTF_8)).getOrElse("<none>")
                        // The nulled variable in `operations` is mapped to a file part carrying the bytes.
                        assert(text("operations").contains("\"variables\":{\"file\":null}"), text("operations"))
                        assert(text("map") == """{"0":["variables.file"]}""", text("map"))
                        assert(parts.exists(p => p.name == "0" && p.filename == Present("a.txt")), s"no file part: $parts")
                        assert(text("0") == "hello")
                    case other => fail(s"expected a multipart body, got $other")
                end match
            }
        }

        "a request-pinned method overrides the composer default" in {
            val getComposer = HttpRequestComposer(defaultHttpMethod = HttpMethod.POST)
            val request     = ApolloRequest(MiniQuery(1), TestIds.requestUuid, httpMethod = Present(HttpMethod.GET))
            assert(getComposer.compose(url, request).method == HttpMethod.GET)
        }

        "extensions carry raw JSON through unchanged (Json.JObj round-trips)" in {
            // Sanity that the APQ extension really parses back to the same structure.
            val request = ApolloRequest(MiniQuery(1), TestIds.requestUuid, sendApqExtensions = true)
            val body    = kyo.apollo.json.JsonParser.parse(composer.compose(url, request).fields.body.text.getOrElse("")).getOrThrow
            body match
                case Json.JObj(fields) =>
                    assert(fields.contains("extensions"))
                case other => fail(s"expected object, got ${other.render}")
            end match
        }
    }
end HttpRequestComposerSpec
