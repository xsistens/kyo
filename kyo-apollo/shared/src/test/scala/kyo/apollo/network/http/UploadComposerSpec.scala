package kyo.apollo.network.http

import java.nio.charset.StandardCharsets
import kyo.Absent
import kyo.HttpUrl
import kyo.Present
import kyo.Schema
import kyo.Span
import kyo.apollo.Upload
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.JsonCodec
import kyo.apollo.api.Mutation
import kyo.apollo.api.ScalarCodec
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.TestIds
import scala.collection.immutable.VectorMap

/** Unit tests for the [[HttpRequestComposer]] multipart branch: an operation with
  * an `Upload` variable produces a spec-compliant `multipart/form-data` request
  * (`operations` with the file nulled + `map` + one file part); an operation with
  * no uploads is the byte-identical JSON POST it was before.
  */
class UploadComposerSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    final case class Ok(ok: Boolean) derives Schema

    final case class UploadMutation(file: Upload) extends Mutation.Normalizable[Ok]:
        def name                     = "UploadFile"
        def document                 = "mutation UploadFile($file: Upload!) { uploadFile(file: $file) { ok } }"
        val dataCodec: JsonCodec[Ok] = JsonCodec.fromSchema[Ok]
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Mutation"))
        def variables: Json          = Json.JObj(VectorMap("file" -> ScalarCodec.upload.encode(file)))
    end UploadMutation

    final case class PlainMutation() extends Mutation.Normalizable[Ok]:
        def name                     = "Plain"
        def document                 = "mutation Plain { plain { ok } }"
        val dataCodec: JsonCodec[Ok] = JsonCodec.fromSchema[Ok]
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Mutation"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end PlainMutation

    private val composer       = new HttpRequestComposer()
    private val url            = HttpUrl(Present("http"), "x", 80, "/graphql", Absent)
    private def upload: Upload = Upload(Span.from("hello".getBytes), "a.txt")

    "HttpRequestComposer multipart" - {

        "an Upload variable produces a multipart body (operations/map/part)" in {
            val req = composer.compose(url, ApolloRequest(UploadMutation(upload), TestIds.requestUuid))
            req.fields.body match
                case HttpRequestBody.Multipart(parts) =>
                    def text(name: String) =
                        parts.find(_.name == name).map(p => new String(p.data.toArray, StandardCharsets.UTF_8)).getOrElse("")
                    // the file variable is nulled in `operations`
                    assert(text("operations").contains("\"variables\":{\"file\":null}"))
                    // map points part "0" at the file's variable path
                    assert(text("map") == "{\"0\":[\"variables.file\"]}")
                    // one file part carrying the filename; the text parts carry none
                    val files = parts.filter(_.filename.isDefined)
                    assert(files.size == 1)
                    assert(files.head.name == "0" && files.head.filename == Present("a.txt"))
                case other => fail(s"expected a multipart body, got $other")
            end match
            // no Content-Type — the platform sets the multipart boundary
            assert(!req.headers.contains("Content-Type"))
        }

        "a no-upload operation is the unchanged JSON POST" in {
            val req = composer.compose(url, ApolloRequest(PlainMutation(), TestIds.requestUuid))
            assert(req.fields.body.text.exists(_.contains("\"query\":")))
            assert(req.headers.get("Content-Type") == Present("application/json"))
        }
    }
end UploadComposerSpec
