package kyo.apollo.network.http

import java.nio.charset.StandardCharsets
import kyo.*

/** The JVM/Native [[HttpClientEngine]]'s file-upload path: when the composer produces a
  * `multipart/form-data` [[HttpRequestBody.Multipart]] (a GraphQL operation with an
  * `Upload` variable), the engine must send it as a real multipart request, not an empty
  * POST — the JVM/Native regression this guards (JS worked via `FetchHttpEngine`).
  */
class HttpClientEngineUploadSpec extends kyo.test.Test[Any]:

    // Real server (ephemeral listener fd the NIO transport defers closing); sequential, skip
    // the client-socket leak check, matching the other live engine specs.
    override def config = super.config.sequential.leakCheckSockets(false)

    private def textPart(name: String, value: String): HttpRequest.Part =
        HttpRequest.Part(name, Absent, Absent, Span.fromUnsafe(value.getBytes(StandardCharsets.UTF_8)))

    "execute sends a multipart/form-data upload body (operations + map + file), not an empty POST" in {
        val operations = """{"query":"mutation($f:Upload!){upload(file:$f)}","variables":{"file":null}}"""
        val mapField   = """{"0":["variables.file"]}"""
        val fileBytes  = "hello-upload".getBytes(StandardCharsets.UTF_8)
        val form = HttpRequestBody.Multipart(Chunk(
            textPart("operations", operations),
            textPart("map", mapField),
            HttpRequest.Part("0", Present("hello.txt"), Present("text/plain"), Span.fromUnsafe(fileBytes))
        ))

        // The server echoes back exactly what it received over the multipart body, so the
        // assertions fail loudly if the engine sent an empty POST (the pre-fix behavior).
        val route = HttpRoute.postRaw("upload").request(_.bodyMultipart).response(_.bodyText)
        val ep = route.handler { req =>
            val parts = req.fields.body
            def partText(name: String): String =
                parts.find(_.name == name).map(p => new String(p.data.toArrayUnsafe, StandardCharsets.UTF_8)).getOrElse("<none>")
            val fileP    = parts.find(_.name == "0")
            val fileName = fileP.flatMap(_.filename.toOption).getOrElse("<none>")
            val fileCt   = fileP.flatMap(_.contentType.toOption).getOrElse("<none>")
            HttpResponse.ok(
                s"parts=${parts.size}|operations=${partText("operations")}|map=${partText("map")}|" +
                    s"file=${partText("0")}|filename=$fileName|contentType=$fileCt"
            )
        }

        HttpServer.init(0, "127.0.0.1")(ep).map { server =>
            val engine = new HttpClientEngine
            val request = HttpEngine.request(
                HttpMethod.POST,
                HttpUrl(Present("http"), "127.0.0.1", server.port, "/upload", Absent),
                HttpHeaders.empty.add("Accept", "application/json"),
                form
            )
            engine.execute(request).map { resp =>
                assert(resp.status == HttpStatus.OK)
                val b = resp.fields.body
                assert(b.contains("parts=3"), s"server did not receive the 3 multipart parts: $b")
                assert(b.contains(s"operations=$operations"), s"operations part missing/corrupt: $b")
                assert(b.contains(s"map=$mapField"), s"map part missing/corrupt: $b")
                assert(b.contains("file=hello-upload"), s"file bytes missing/corrupt: $b")
                assert(b.contains("filename=hello.txt"), s"filename not carried: $b")
                assert(b.contains("contentType=text/plain"), s"file content-type not carried: $b")
            }
        }
    }
end HttpClientEngineUploadSpec
