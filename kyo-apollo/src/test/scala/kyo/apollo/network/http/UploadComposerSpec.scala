package kyo.apollo.network.http

import kyo.Schema
import kyo.apollo.Upload
import kyo.apollo.api.CompiledField
import kyo.apollo.api.CompiledNamedType
import kyo.apollo.api.Mutation
import kyo.apollo.api.ScalarCodec
import kyo.apollo.json.Json
import kyo.apollo.network.ApolloRequest
import kyo.apollo.network.HttpMethod
import org.scalajs.dom
import scala.collection.immutable.VectorMap
import scala.scalajs.js as sjs

/** Unit tests for the [[HttpRequestComposer]] multipart branch: an operation with
  * an `Upload` variable produces a spec-compliant `multipart/form-data` request
  * (`operations` with the file nulled + `map` + one file part); an operation with
  * no uploads is the byte-identical JSON POST it was before.
  */
class UploadComposerSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    final case class Ok(ok: Boolean) derives Schema

    final case class UploadMutation(file: Upload) extends Mutation[Ok]:
        def name                     = "UploadFile"
        def document                 = "mutation UploadFile($file: Upload!) { uploadFile(file: $file) { ok } }"
        def dataSchema: Schema[Ok]   = summon[Schema[Ok]]
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Mutation"))
        def variables: Json          = Json.JObj(VectorMap("file" -> ScalarCodec.upload.encode(file)))
    end UploadMutation

    final case class PlainMutation() extends Mutation[Ok]:
        def name                     = "Plain"
        def document                 = "mutation Plain { plain { ok } }"
        def dataSchema: Schema[Ok]   = summon[Schema[Ok]]
        def rootField: CompiledField = CompiledField("data", CompiledNamedType("Mutation"))
        def variables: Json          = Json.JObj(VectorMap.empty)
    end PlainMutation

    private val composer = new HttpRequestComposer()
    private def blob: dom.Blob =
        sjs.Dynamic
            .newInstance(sjs.Dynamic.global.Blob)(sjs.Array[sjs.Any]("hello"))
            .asInstanceOf[dom.Blob]

    "HttpRequestComposer multipart" - {

        "an Upload variable produces a multipart formBody (operations/map/part)" in {
            val req = composer.compose(
                "http://x/graphql",
                ApolloRequest(UploadMutation(Upload(blob, "a.txt")))
            )
            assert(req.body.isEmpty)
            val form   = req.formBody.getOrElse(sys.error("expected a formBody"))
            val fields = form.fields.toMap
            // the file variable is nulled in `operations`
            assert(fields("operations").contains("\"variables\":{\"file\":null}"))
            // map points part "0" at the file's variable path
            assert(fields("map") == "{\"0\":[\"variables.file\"]}")
            // one ordered file part carrying the filename
            assert(form.files.size == 1)
            assert(form.files.head.fieldName == "0" && form.files.head.fileName == "a.txt")
            // no Content-Type — the platform sets the multipart boundary
            assert(!req.headers.exists(_.name.equalsIgnoreCase("Content-Type")))
        }

        "a no-upload operation is the unchanged JSON POST" in {
            val req = composer.compose("http://x/graphql", ApolloRequest(PlainMutation()))
            assert(req.formBody.isEmpty)
            assert(req.body.exists(_.contains("\"query\":")))
            assert(req.headers.exists(h => h.name == "Content-Type" && h.value == "application/json"))
        }
    }
end UploadComposerSpec
