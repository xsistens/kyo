package kyo.apollo.network.http

import kyo.Schema
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledFragment
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.DeferDirective
import kyo.apollo.api.Query
import kyo.apollo.json.Json
import kyo.apollo.json.SchemaJson
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.HttpMethod
import scala.collection.immutable.VectorMap

/** Tests that [[HttpRequestComposer]] lowers an [[ApolloRequest]] into the right
  * wire [[HttpRequest]]: POST JSON body vs GET query params, default and
  * per-request headers, and the APQ `persistedQuery` extension / document
  * omission driven by the request flags.
  */
class HttpRequestComposerSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** A minimal query with one `Int` variable — enough to exercise body/param
      * composition without pulling in the example module's richer operations.
      */
    final case class MiniQuery(limit: Int) extends Query[Int]:
        def name: String             = "Mini"
        def document: String         = "query Mini($limit: Int!) { x }"
        def dataSchema: Schema[Int]  = summon[Schema[Int]]
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Query"))
        def variables: Json          = Json.JObj(VectorMap("limit" -> SchemaJson.encode(limit)))
    end MiniQuery

    /** Same shape as [[MiniQuery]] but its selection tree carries an anonymous
      * `@defer` fragment, so [[Defer.has]] is true and the Accept header widens.
      */
    final case class DeferQuery(limit: Int) extends Query[Int]:
        def name: String            = "Mini"
        def document: String        = "query Mini($limit: Int!) { x }"
        def dataSchema: Schema[Int] = summon[Schema[Int]]
        def rootField: CompiledField = CompiledField(
            "data",
            CompiledNamedType("Query"),
            selections = List(
                CompiledFragment(
                    "",
                    Nil,
                    List(CompiledField("x", CompiledNamedType("Int"))),
                    defer = Some(DeferDirective("d"))
                )
            )
        )
        def variables: Json = Json.JObj(VectorMap("limit" -> SchemaJson.encode(limit)))
    end DeferQuery

    private val composer = HttpRequestComposer()
    private val url      = "https://example.com/graphql"

    private val plainAccept = "application/graphql-response+json, application/json"
    private val deferAccept = s"multipart/mixed; deferSpec=20220824, $plainAccept"

    "HttpRequestComposer" - {

        "default method is POST with the OperationRequestBody JSON body" in {
            val request = ApolloRequest(MiniQuery(5))
            val http    = composer.compose(url, request)

            assert(http.method == HttpMethod.Post)
            assert(http.url == url)
            assert(
                http.body ==
                    Some(
                        """{"query":"query Mini($limit: Int!) { x }","operationName":"Mini","variables":{"limit":5}}"""
                    )
            )
        }

        "POST carries JSON content-type / accept headers, then per-request ones" in {
            val request = ApolloRequest
                .builder(MiniQuery(1))
                .addHttpHeader("Authorization", "Bearer t")
                .build()
            val http = composer.compose(url, request)

            assert(
                http.headers ==
                    List(
                        HttpHeader("Content-Type", "application/json"),
                        HttpHeader("Accept", "application/graphql-response+json, application/json"),
                        HttpHeader("Authorization", "Bearer t")
                    )
            )
        }

        "GET encodes the operation as URL query params and sends no body" in {
            val request = ApolloRequest.builder(MiniQuery(5)).httpMethod(HttpMethod.Get).build()
            val http    = composer.compose(url, request)

            assert(http.method == HttpMethod.Get)
            assert(http.body == None)
            // query, operationName, variables — each percent-encoded, joined with '&'.
            assert(
                http.url ==
                    url + "?query=query%20Mini(%24limit%3A%20Int!)%20%7B%20x%20%7D" +
                    "&operationName=Mini&variables=%7B%22limit%22%3A5%7D"
            )
        }

        "GET appends params with '&' when the URL already has a query string" in {
            val request = ApolloRequest.builder(MiniQuery(1)).httpMethod(HttpMethod.Get).build()
            val http    = composer.compose(url + "?trace=1", request)
            assert(http.url.startsWith(url + "?trace=1&query="))
        }

        "APQ POST adds the persistedQuery extension with the document sha256" in {
            val request = ApolloRequest.builder(MiniQuery(2)).sendApqExtensions(true).build()
            val http    = composer.compose(url, request)

            val hash = Sha256.hex(MiniQuery(2).document)
            assert(
                http.body ==
                    Some(
                        s"""{"query":"query Mini($$limit: Int!) { x }","operationName":"Mini","variables":{"limit":2},""" +
                            s""""extensions":{"persistedQuery":{"version":1,"sha256Hash":"$hash"}}}"""
                    )
            )
        }

        "APQ with sendDocument=false omits the query field from the POST body" in {
            val request = ApolloRequest
                .builder(MiniQuery(2))
                .sendApqExtensions(true)
                .sendDocument(false)
                .build()
            val http = composer.compose(url, request)

            val hash = Sha256.hex(MiniQuery(2).document)
            assert(
                http.body ==
                    Some(
                        s"""{"operationName":"Mini","variables":{"limit":2},""" +
                            s""""extensions":{"persistedQuery":{"version":1,"sha256Hash":"$hash"}}}"""
                    )
            )
            // No `query` field on the wire when the document is withheld.
            assert(!http.body.get.contains("\"query\""))
        }

        "a @defer operation widens the POST Accept header to multipart/mixed" in {
            val http = composer.compose(url, ApolloRequest(DeferQuery(1)))
            assert(http.headers.contains(HttpHeader("Accept", deferAccept)))
            assert(!http.headers.contains(HttpHeader("Accept", plainAccept)))
        }

        "a @defer operation widens the GET Accept header too" in {
            val request = ApolloRequest.builder(DeferQuery(1)).httpMethod(HttpMethod.Get).build()
            assert(composer.compose(url, request).headers.contains(HttpHeader("Accept", deferAccept)))
        }

        "a non-defer operation keeps the plain Accept header" in {
            val http = composer.compose(url, ApolloRequest(MiniQuery(1)))
            assert(http.headers.contains(HttpHeader("Accept", plainAccept)))
        }

        "a request-pinned method overrides the composer default" in {
            val getComposer = HttpRequestComposer(defaultHttpMethod = HttpMethod.Post)
            val request     = ApolloRequest.builder(MiniQuery(1)).httpMethod(HttpMethod.Get).build()
            assert(getComposer.compose(url, request).method == HttpMethod.Get)
        }

        "extensions carry raw JSON through unchanged (Json.JObj round-trips)" in {
            // Sanity that the APQ extension really parses back to the same structure.
            val request = ApolloRequest.builder(MiniQuery(1)).sendApqExtensions(true).build()
            val body    = kyo.apollo.json.JsonParser.parse(composer.compose(url, request).body.get)
            body match
                case Json.JObj(fields) =>
                    assert(fields.contains("extensions"))
                case other => fail(s"expected object, got ${other.render}")
            end match
        }
    }
end HttpRequestComposerSpec
