package kyo.apollo.network.http

import kyo.apollo.network.HttpHeader
import kyo.apollo.network.HttpMethod
import org.scalajs.dom
import scala.scalajs.js as sjs

/** Unit tests for the multipart branch of [[FetchHttpEngine.requestInit]]: a
  * `formBody` request builds a `FormData` (and drops any caller `Content-Type` so
  * the platform sets the boundary); a plain text request is unchanged.
  */
class FetchUploadSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private val engine = new FetchHttpEngine()
    // A real Blob via the global constructor (Node 18+ has it), avoiding the typed
    // BlobPart constructor — the engine casts blob back to dom.Blob for FormData.
    private def blob: sjs.Any =
        sjs.Dynamic.newInstance(sjs.Dynamic.global.Blob)(sjs.Array[sjs.Any]("hello"))

    "FetchHttpEngine.requestInit" - {

        "a formBody request builds a FormData body and drops a caller Content-Type" in {
            val req = HttpRequest(
                method = HttpMethod.Post,
                url = "http://x/graphql",
                headers = List(HttpHeader("Content-Type", "should-be-dropped")),
                body = None,
                formBody = Some(
                    HttpForm(
                        fields = List("operations" -> "{}", "map" -> "{}"),
                        files = List(HttpFormFile("0", blob, "a.txt"))
                    )
                )
            )
            val init = engine.requestInit(req)
            assert(sjs.typeOf(init.body.asInstanceOf[sjs.Any]) == "object") // FormData, not a string
            assert(!init.headers.asInstanceOf[dom.Headers].has("Content-Type"))
        }

        "a text-body request keeps the string body" in {
            val req  = HttpRequest(HttpMethod.Post, "http://x/graphql", Nil, body = Some("{\"a\":1}"))
            val init = engine.requestInit(req)
            assert(sjs.typeOf(init.body.asInstanceOf[sjs.Any]) == "string")
            assert(init.body.asInstanceOf[String] == "{\"a\":1}")
        }
    }
end FetchUploadSpec
