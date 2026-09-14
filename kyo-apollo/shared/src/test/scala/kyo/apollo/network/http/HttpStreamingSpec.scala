package kyo.apollo.network.http

import kyo.*
import kyo.apollo.StreamProbe

/** Tests the streaming `HttpEngine` seam ([[HttpEngine.executeStreaming]] +
  * [[HttpEngine.StreamResponse]] / [[HttpStreamBody]]): the default buffers a
  * non-streaming engine's body, and a `Chunked` body streams its text chunks in
  * order with a `Scope`-bound teardown. `FetchHttpEngine`'s real `ReadableStream`
  * reader needs a live `fetch` + multipart server, so it is exercised in the
  * browser (C7) rather than here; this proves the seam its override plugs into.
  */
class HttpStreamingSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val request =
        HttpEngine.request(
            HttpMethod.POST,
            HttpUrl(Present("https"), "x", 443, "/graphql", Absent),
            HttpHeaders.empty,
            HttpRequestBody.Empty
        )

    "HttpEngine.executeStreaming" - {

        "the default buffers a non-streaming engine's body into Buffered" in {
            val engine = new HttpEngine:
                def execute(r: HttpEngine.Request)(using Frame): HttpEngine.Response < Async =
                    HttpEngine.response(HttpStatus.OK, "hello", HttpHeaders.empty.add("Content-Type", "application/json"))
            engine.executeStreaming(request).map { resp =>
                assert(resp.status == HttpStatus.OK)
                assert(resp.headers.get("Content-Type") == Present("application/json"))
                assert(resp.fields.body == HttpStreamBody.Buffered("hello"))
            }
        }

        "a Chunked body streams its text chunks in order" in {
            HttpStreamBody.Chunked(Stream.init(Seq("part-1\n", "part-2\n", "part-3"))) match
                case HttpStreamBody.Chunked(s) =>
                    StreamProbe
                        .collect(s)
                        .map(seen => assert(seen == List("part-1\n", "part-2\n", "part-3")))
                case HttpStreamBody.Buffered(_) => fail("expected Chunked")
        }

        "a Chunked body's Scope finalizer runs when its scope closes" in {
            var cleaned = false
            val s: Stream[String, Async & Scope] =
                Stream.unwrap(
                    Scope.ensure(Sync.defer { cleaned = true }).andThen(Stream.init(Seq("x", "y")))
                )
            // Discharge the Scope locally so the finalizer fires before we assert.
            Scope.run(s.run.map(_.toList)).map { seen =>
                assert(seen == List("x", "y"))
                assert(cleaned)
            }
        }
    }
end HttpStreamingSpec
