package kyo.apollo

import org.scalajs.dom

/** A GraphQL file-upload input value: a browser `Blob`/`File` plus the filename to
  * send. Used as the Scala type of a custom `Upload` scalar variable; the request
  * composer detects it (via [[kyo.apollo.json.Json.JUpload]]) and sends a
  * `multipart/form-data` request per the graphql-multipart-request-spec.
  *
  * Input-only — an `Upload` is never decoded from a response. Its leaf codec is
  * [[kyo.apollo.api.ScalarCodec.upload]].
  */
final case class Upload(blob: dom.Blob, fileName: String)

object Upload:
    /** Build an [[Upload]] from a browser `File`, using the file's own name. */
    def apply(file: dom.File): Upload = Upload(file, file.name)
