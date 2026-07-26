package kyo.apollo

import kyo.apollo.api.ScalarCodec
import kyo.apollo.api.ScalarDecodeException
import kyo.apollo.json.Json
import org.scalajs.dom
import scala.scalajs.js as sjs

/** Unit tests for the `Upload` scalar representation: `ScalarCodec.upload` encodes
  * an [[Upload]] to a [[Json.JUpload]] placeholder (which renders as `null` in the
  * `operations` payload), and decoding is unsupported (uploads are input-only).
  */
class UploadCodecSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def fakeBlob: dom.Blob = sjs.Dynamic.literal(size = 3).asInstanceOf[dom.Blob]

    "Upload scalar codec" - {

        "encode wraps the blob + filename in a Json.JUpload" in {
            ScalarCodec.upload.encode(Upload(fakeBlob, "a.txt")) match
                case Json.JUpload(_, name) => assert(name == "a.txt")
                case other                 => assert(false, s"expected JUpload, got $other")
        }

        "a JUpload renders as null (nulled in the operations payload)" in {
            assert(Json.JUpload(fakeBlob, "a.txt").render == "null")
        }

        "decode is unsupported (uploads are input-only)" in {
            val _ = intercept[ScalarDecodeException] {
                ScalarCodec.upload.decode(Json.JStr("x"))
            }
            assert(true)
        }

        "Upload.apply(file) uses the file's own name" in {
            val file = sjs.Dynamic.literal(name = "photo.png").asInstanceOf[dom.File]
            assert(Upload(file).fileName == "photo.png")
        }
    }
end UploadCodecSpec
