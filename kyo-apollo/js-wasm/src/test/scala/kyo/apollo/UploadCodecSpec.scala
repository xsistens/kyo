package kyo.apollo

import kyo.Span
import kyo.apollo.api.ScalarCodec
import kyo.apollo.api.ScalarDecodeException
import kyo.apollo.json.Json
import org.scalajs.dom
import scala.scalajs.js as sjs

/** Unit tests for the `Upload` scalar representation: `ScalarCodec.upload` encodes
  * an [[Upload]] to a [[Json.JUpload]] placeholder (which renders as `null` in the
  * `operations` payload), and decoding is unsupported (uploads are input-only).
  * Also covers the JS [[UploadJs]] bridge that reads a browser `Blob` into an
  * [[Upload]].
  */
class UploadCodecSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    private def fakeUpload: Upload = Upload(Span.from("abc".getBytes), "a.txt")
    private def realBlob: dom.Blob =
        sjs.Dynamic.newInstance(sjs.Dynamic.global.Blob)(sjs.Array[sjs.Any]("hi")).asInstanceOf[dom.Blob]

    "Upload scalar codec" - {

        "encode wraps the upload in a Json.JUpload" in {
            ScalarCodec.upload.encode(fakeUpload) match
                case Json.JUpload(u) => assert(u.fileName == "a.txt")
                case other           => assert(false, s"expected JUpload, got $other")
        }

        "a JUpload renders as null (nulled in the operations payload)" in {
            assert(Json.JUpload(fakeUpload).render == "null")
        }

        "decode is unsupported (uploads are input-only)" in {
            val _ = intercept[ScalarDecodeException] {
                ScalarCodec.upload.decode(Json.JStr("x"))
            }
            assert(true)
        }

        "UploadJs.fromBlob reads the blob's bytes and carries the filename" in {
            UploadJs.fromBlob(realBlob, "photo.png", "image/png").map { u =>
                assert(
                    u.fileName == "photo.png" &&
                        u.contentType == "image/png" &&
                        new String(u.data.toArray) == "hi"
                )
            }
        }
    }
end UploadCodecSpec
