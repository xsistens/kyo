package kyo.apollo.network.http

import kyo.*
import org.scalajs.dom
import scala.scalajs.js as sjs

/** Unit tests for the multipart branch of [[FetchHttpEngine.requestInit]]: a
  * multipart request builds a `FormData` (and drops any caller `Content-Type` so
  * the platform sets the boundary); a plain text request is unchanged.
  */
class FetchUploadSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val engine = new FetchHttpEngine()
    private val url    = HttpUrl(Present("http"), "x", 80, "/graphql", Absent)

    "FetchHttpEngine.requestInit" - {

        "a multipart request builds a FormData body and drops a caller Content-Type" in {
            val req = HttpEngine.request(
                HttpMethod.POST,
                url,
                HttpHeaders.empty.add("Content-Type", "should-be-dropped"),
                HttpRequestBody.Multipart(Chunk(
                    HttpRequest.Part("operations", Absent, Absent, Span.from("{}".getBytes)),
                    HttpRequest.Part("map", Absent, Absent, Span.from("{}".getBytes)),
                    HttpRequest.Part("0", Present("a.txt"), Present("text/plain"), Span.from("hello".getBytes))
                ))
            )
            val init = engine.requestInit(req)
            assert(sjs.typeOf(init.body.asInstanceOf[sjs.Any]) == "object") // FormData, not a string
            val form = init.body.asInstanceOf[dom.FormData]
            assert(form.get("operations").asInstanceOf[String] == "{}")
            assert(sjs.typeOf(form.get("0").asInstanceOf[sjs.Any]) == "object") // a Blob/File, not text
            assert(!init.headers.asInstanceOf[dom.Headers].has("Content-Type"))
        }

        "a text-body request keeps the string body" in {
            val req  = HttpEngine.request(HttpMethod.POST, url, HttpHeaders.empty, HttpRequestBody.Text("{\"a\":1}"))
            val init = engine.requestInit(req)
            assert(sjs.typeOf(init.body.asInstanceOf[sjs.Any]) == "string")
            assert(init.body.asInstanceOf[String] == "{\"a\":1}")
        }
    }
end FetchUploadSpec
