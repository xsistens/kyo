package kyo.apollo.network.http

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.HttpMethod
import org.scalajs.dom
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.*

/** The production [[HttpEngine]]: issues requests through the platform `fetch`.
  *
  * Works in the browser and under Node 18+ (both expose a global `fetch`; this
  * project runs on Node 26). The ordinary [[execute]] reads the whole response
  * body via `Response.text()` — see [[HttpResponse]] for why a buffered `String`.
  * [[executeStreaming]] additionally reads a `multipart/mixed` incremental-delivery
  * (`@defer`) body as a live chunk stream off the `fetch` body reader. Any `fetch`
  * rejection surfaces as a failed `Future`, folded into an `ApolloNetworkException`
  * by [[HttpNetworkTransport]].
  */
final class FetchHttpEngine extends HttpEngine:

    // Scala.js's microtask-queue ExecutionContext — the standard EC for JS
    // Futures. Imported as a value only, so it never collides with
    // `kyo.apollo.network.ExecutionContext` (the request-context bag) by name.
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

    def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
        // Build the `fetch` round-trip as a `Future` (dom.fetch is Promise-based),
        // then bridge it into `Async`. A rejection surfaces on the async Throwable
        // channel, which `HttpNetworkTransport` folds into an `ApolloNetworkException`.
        //
        // LAZINESS IS LOAD-BEARING: `Async.fromFuture` takes its Future BY VALUE, so
        // wrapping the fetch in it directly would START the request the moment this
        // effect is CONSTRUCTED (effects must be inert values — a handle that merely
        // holds a `refetch` effect would fire a phantom request per construction).
        // `Fiber.fromFuture` is by-name and defers into `Sync`, so the fetch starts
        // only when the effect actually runs.
        Fiber
            .fromFuture {
                // The `: Future[…]` ascriptions force the implicit Thenable→Future bridge on
                // the Promise-based `fetch` / `text()` results.
                for
                    response <- dom.fetch(request.url, requestInit(request)): Future[dom.Response]
                    text     <- response.text(): Future[String]
                yield HttpResponse(
                    statusCode = response.status,
                    headers = readHeaders(response.headers),
                    body = text
                )
            }
            .map(_.get)

    override def executeStreaming(
        request: HttpRequest
    )(using Frame): HttpStreamResponse < (Async & Scope) =
        // By-name `Fiber.fromFuture` for the same laziness reason as [[execute]].
        Fiber
            .fromFuture {
                dom.fetch(request.url, requestInit(request)).flatMap { response =>
                    val headers = readHeaders(response.headers)
                    if isMultipart(Option(response.headers.get("Content-Type"))) then
                        Future.successful(
                            HttpStreamResponse(
                                response.status,
                                headers,
                                HttpStreamBody.Chunked(bodyStream(response))
                            )
                        )
                    else
                        response.text().map { text =>
                            HttpStreamResponse(response.status, headers, HttpStreamBody.Buffered(text))
                        }
                    end if
                }
            }
            .map(_.get)

    /** The shared `fetch` `RequestInit` for a request. */
    private[http] def requestInit(request: HttpRequest): dom.RequestInit =
        val init = new dom.RequestInit {}
        init.method = request.method match
            case HttpMethod.Get  => dom.HttpMethod.GET
            case HttpMethod.Post => dom.HttpMethod.POST
        val headers = new dom.Headers()
        request.headers.foreach { h =>
            // For a multipart upload the platform must set `Content-Type` itself (with
            // the generated boundary), so never forward a caller-set one.
            if request.formBody.isDefined && h.name.equalsIgnoreCase("Content-Type") then ()
            else headers.append(h.name, h.value)
        }
        init.headers = headers
        request.formBody match
            case Some(form) =>
                val data = new dom.FormData()
                form.fields.foreach((name, value) => data.append(name, value))
                form.files.foreach { f =>
                    // Reconstruct a Blob from the portable byte payload. The bytes are raw
                    // (signedness is irrelevant to Blob), so an Int8Array view is fine.
                    val bag = new dom.BlobPropertyBag {}
                    bag.`type` = f.contentType
                    val blob = new dom.Blob(js.Array[dom.BlobPart](f.data.toArray.toTypedArray), bag)
                    data.append(f.fieldName, blob, f.fileName)
                }
                init.body = data
            case None =>
                request.body.foreach(b => init.body = b)
        end match
        init
    end requestInit

    /** Bridge the `fetch` body `ReadableStream` into a Kyo `Stream` of UTF-8 text
      * chunks, **pull-based**: `Stream.repeatPresent` reads exactly one chunk from
      * the reader per demand and emits it, ending when the reader signals `done`.
      * No producer/consumer channel, so there is no race between an eager pump and a
      * late consumer — the earlier `Channel` bridge filled AND closed the channel in
      * one microtask burst when a fast server flushed the whole `multipart/mixed`
      * reply at once, and a bounded channel's `close` drops its backlog, so every
      * chunk was lost before the consumer attached and `@defer` stalled on Loading.
      * `TextDecoder({stream:true})` keeps a multibyte char intact across a chunk
      * boundary; the `Scope` finalizer cancels the reader when the consumer stops early.
      */
    private def bodyStream(response: dom.Response)(using Frame): Stream[String, Async & Scope] =
        Stream.unwrap {
            Sync.defer(response.body.getReader()).map { reader =>
                val decoder    = new TextDecoder("utf-8")
                val streamOpts = js.Dynamic.literal(stream = true).asInstanceOf[js.Object]
                // One `reader.read()` per pull; `Present(chunk)` emits, `Absent` ends.
                def pull: Maybe[Seq[String]] < Async =
                    Async.fromFuture(reader.read(): Future[dom.Chunk[Uint8Array]]).map { chunk =>
                        if chunk.done then Absent
                        else Present(Seq(decoder.decode(chunk.value, streamOpts)))
                    }
                // `chunkSize = 1`: emit each network chunk downstream the instant it
                // arrives. The default rechunks (~4096 elements per chunk), which BUFFERS
                // — it holds emitted chunks until that many accumulate or the stream ends,
                // collapsing a progressive `multipart/mixed` reply into one batch at the
                // end (a `@defer`'s eager part would then wait for the deferred part).
                Scope
                    .ensure(Sync.defer { discard(reader.cancel(js.undefined)); () })
                    .andThen(Stream.repeatPresent(pull, chunkSize = 1))
            }
        }

    private def isMultipart(contentType: Option[String]): Boolean =
        contentType.exists(_.toLowerCase.contains("multipart/mixed"))

    /** Flatten the response `Headers` (a `[name, value]` iterable) into our list. */
    private def readHeaders(headers: dom.Headers): List[HttpHeader] =
        js.Array
            .from(headers)
            .toList
            .map(pair => HttpHeader(pair(0), pair(1)))
end FetchHttpEngine

/** Minimal facade for the global `TextDecoder` (absent from scalajs-dom 2.8.1).
  * `decode(bytes, {stream:true})` decodes a UTF-8 chunk, holding back any trailing
  * bytes of a multibyte char until the next chunk completes it.
  */
@js.native
@JSGlobal
private class TextDecoder(utfLabel: String = js.native) extends js.Object:
    def decode(input: Uint8Array, options: js.Object): String = js.native
end TextDecoder
