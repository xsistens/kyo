package kyo.apollo

import kyo.*
import org.scalajs.dom
import scala.concurrent.Future
import scala.scalajs.js.Thenable.Implicits.*
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.scalajs.js.typedarray.Uint8Array

/** Browser bridge for building an [[Upload]] from a `File`/`Blob`.
  *
  * JS/Wasm only (moves to the `js-wasm` source root). Reading a `Blob`'s bytes is
  * asynchronous (`Blob.arrayBuffer()` returns a Promise), so these return
  * `Upload < Async` rather than a bare value — the bytes are materialized eagerly
  * into the platform-neutral [[Upload]] before the request is composed.
  */
object UploadJs:

    // Scala.js's microtask-queue ExecutionContext for the Promise->Future bridge.
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

    /** Build an [[Upload]] from a browser `File`, using the file's own name and type. */
    def fromFile(file: dom.File)(using Frame): Upload < Async =
        fromBlob(file, file.name, file.`type`)

    /** Build an [[Upload]] from a browser `Blob` with an explicit filename. */
    def fromBlob(
        blob: dom.Blob,
        fileName: String,
        contentType: String = "application/octet-stream"
    )(using Frame): Upload < Async =
        Async.fromFuture(blob.arrayBuffer(): Future[ArrayBuffer]).map { buffer =>
            val u8  = new Uint8Array(buffer)
            val arr = new Array[Byte](u8.length)
            var i   = 0
            while i < u8.length do
                arr(i) = u8(i).toByte
                i += 1
            val ct = if contentType.isEmpty then "application/octet-stream" else contentType
            Upload(Span.from(arr), fileName, ct)
        }
end UploadJs
