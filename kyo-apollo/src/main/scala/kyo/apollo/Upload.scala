package kyo.apollo

import kyo.Span

/** A GraphQL file-upload input value: the file bytes plus the filename and content
  * type to send. Used as the Scala type of a custom `Upload` scalar variable; the
  * request composer detects it (via [[kyo.apollo.json.Json.JUpload]]) and sends a
  * `multipart/form-data` request per the graphql-multipart-request-spec.
  *
  * Platform-neutral: the bytes are held eagerly as a [[kyo.Span]], matching
  * kyo-http's multipart `Part.data`, so the same `Upload` works on JS, Wasm, JVM
  * and Native. On JS/Wasm, build one from a browser `File`/`Blob` with
  * [[kyo.apollo.UploadJs]] (which reads the blob's bytes, an async operation).
  *
  * Input-only — an `Upload` is never decoded from a response. Its leaf codec is
  * [[kyo.apollo.api.ScalarCodec.upload]].
  */
final case class Upload(
    data: Span[Byte],
    fileName: String,
    contentType: String = "application/octet-stream"
) derives CanEqual
