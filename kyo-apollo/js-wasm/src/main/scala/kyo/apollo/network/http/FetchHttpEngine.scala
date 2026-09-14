package kyo.apollo.network.http

import java.nio.charset.StandardCharsets
import kyo.*
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.HttpEngineFailure
import org.scalajs.dom
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.*

/** The production [[HttpEngine]] on JS/Wasm: issues requests through the platform
  * `fetch`.
  *
  * Works in the browser and under Node 18+ (both expose a global `fetch`; this
  * project runs on Node 26). The ordinary [[execute]] reads the whole response
  * body via `Response.text()`. [[executeStreaming]] additionally reads a
  * `multipart/mixed` incremental-delivery (`@defer`) body as a live chunk stream off
  * the `fetch` body reader. A rejected `fetch` (or body read) becomes the engine's
  * `Abort[HttpEngineFailure]`, an [[ApolloNetworkException]] carrying the JS error
  * as its `cause`.
  *
  * Every request gets its own `AbortController`, whose signal the `fetch` carries.
  * Interrupting [[execute]] aborts the request, so the browser drops the connection;
  * a streamed request is aborted when the `Scope` it was issued in closes (the
  * consumer is done or was interrupted). A rejection with `AbortError` is therefore
  * an interrupt, not a network failure, and reaches kyo as `Interrupted`.
  */
final class FetchHttpEngine extends HttpEngine:

    // Scala.js's microtask-queue ExecutionContext — the standard EC for JS
    // Futures. Imported as a value only, so it never collides with
    // `kyo.apollo.network.ExecutionContext` (the request-context bag) by name.
    import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

    def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < (Async & Abort[HttpEngineFailure]) =
        // Build the `fetch` round-trip as a `Future` (dom.fetch is Promise-based),
        // then bridge it into `Async`; a rejection is mapped onto the engine row at
        // that boundary ([[fetched]]).
        //
        // LAZINESS IS LOAD-BEARING: `Async.fromFuture` takes its Future BY VALUE, so
        // wrapping the fetch in it directly would START the request the moment this
        // effect is CONSTRUCTED (effects must be inert values — a handle that merely
        // holds a `refetch` effect would fire a phantom request per construction).
        // `Fiber.fromFuture` is by-name and defers into `Sync`, so the fetch starts
        // only when the effect actually runs.
        //
        // The finalizer runs on interrupt and on completion; after the body text has
        // been read, `abort()` has nothing left to cancel.
        Sync.defer(new dom.AbortController()).map { controller =>
            Sync.ensure(Sync.defer(controller.abort())) {
                Fiber
                    .fromFuture {
                        // The `: Future[…]` ascriptions force the implicit Thenable→Future bridge on
                        // the Promise-based `fetch` / `text()` results.
                        for
                            response <- dom.fetch(request.url.full, requestInit(request, controller.signal)): Future[dom.Response]
                            text     <- response.text(): Future[String]
                        yield HttpEngine.response(HttpStatus(response.status), text, readHeaders(response.headers))
                    }
                    .map(fiber => fetched(fiber.get))
            }
        }

    override def executeStreaming(
        request: HttpEngine.Request
    )(using Frame): HttpEngine.StreamResponse < (Async & Scope & Abort[HttpEngineFailure]) =
        // By-name `Fiber.fromFuture` for the same laziness reason as [[execute]]. The
        // controller is aborted when the caller's `Scope` closes, not when the head
        // arrives: the body is still being read after that.
        Sync.defer(new dom.AbortController()).map { controller =>
            Scope.ensure(Sync.defer(controller.abort())).andThen {
                Fiber
                    .fromFuture {
                        dom.fetch(request.url.full, requestInit(request, controller.signal)).flatMap { response =>
                            val status  = HttpStatus(response.status)
                            val headers = readHeaders(response.headers)
                            if isMultipart(Option(response.headers.get("Content-Type"))) then
                                Future.successful(
                                    HttpEngine.streamResponse(status, HttpStreamBody.Chunked(bodyStream(response)), headers)
                                )
                            else
                                response.text().map(text => HttpEngine.streamResponse(status, HttpStreamBody.Buffered(text), headers))
                            end if
                        }
                    }
                    .map(fiber => fetched(fiber.get))
            }
        }

    /** The `fetch` boundary: a rejected promise reaches kyo as a panic of the bridged
      * fiber and becomes the engine failure. An interrupt stays an interrupt, and so
      * does a rejection with `AbortError`: only this engine's own controller aborts a
      * request, and it does so because the request's fiber was interrupted.
      */
    private def fetched[A](result: A < Async)(using Frame): A < (Async & Abort[HttpEngineFailure]) =
        Abort.run[Throwable](result).map {
            case Result.Success(value)                => value
            case Result.Panic(interrupt: Interrupted) => Abort.panic(interrupt)
            case Result.Panic(e: js.JavaScriptException) if isAbortError(e.exception) =>
                Abort.panic(Interrupted(summon[Frame], Present("the fetch was aborted")))
            case Result.Error(rejection) => Abort.fail(ApolloNetworkException(cause = rejection))
        }

    /** Whether a `fetch` rejection value is the platform's `AbortError`. */
    private def isAbortError(rejection: Any): Boolean =
        js.typeOf(rejection) == "object" && !js.isUndefined(rejection) && (rejection.asInstanceOf[AnyRef] ne null) &&
            js.typeOf(rejection.asInstanceOf[js.Dynamic].name) == "string" &&
            rejection.asInstanceOf[js.Dynamic].name.asInstanceOf[String] == "AbortError"

    /** The shared `fetch` `RequestInit` for a request, carrying `signal`. */
    private[http] def requestInit(request: HttpEngine.Request, signal: js.UndefOr[dom.AbortSignal] = js.undefined): dom.RequestInit =
        val init      = new dom.RequestInit {}
        val multipart = request.fields.body.isInstanceOf[HttpRequestBody.Multipart]
        init.method = request.method.name.asInstanceOf[dom.HttpMethod]
        init.signal = signal
        val headers = new dom.Headers()
        request.headers.foreach { (name, value) =>
            // For a multipart upload the platform must set `Content-Type` itself (with
            // the generated boundary), so never forward a caller-set one.
            if multipart && name.equalsIgnoreCase("Content-Type") then ()
            else headers.append(name, value)
        }
        init.headers = headers
        request.fields.body match
            case HttpRequestBody.Empty      => ()
            case HttpRequestBody.Text(json) => init.body = json
            case HttpRequestBody.Multipart(parts) =>
                val data = new dom.FormData()
                parts.foreach { part =>
                    part.filename match
                        case Absent =>
                            data.append(part.name, new String(part.data.toArray, StandardCharsets.UTF_8))
                        case Present(fileName) =>
                            // Reconstruct a Blob from the portable byte payload. The bytes are raw
                            // (signedness is irrelevant to Blob), so an Int8Array view is fine.
                            val bag = new dom.BlobPropertyBag {}
                            part.contentType.foreach(bag.`type` = _)
                            val blob = new dom.Blob(js.Array[dom.BlobPart](part.data.toArray.toTypedArray), bag)
                            data.append(part.name, blob, fileName)
                }
                init.body = data
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
      * A rejected read (the body dropped) aborts the stream with the engine failure.
      */
    private def bodyStream(response: dom.Response)(using Frame): Stream[String, Async & Scope & Abort[HttpEngineFailure]] =
        Stream.unwrap {
            Sync.defer(response.body.getReader()).map { reader =>
                val decoder    = new TextDecoder("utf-8")
                val streamOpts = js.Dynamic.literal(stream = true).asInstanceOf[js.Object]
                // One `reader.read()` per pull; `Present(chunk)` emits, `Absent` ends.
                def pull: Maybe[Seq[String]] < (Async & Abort[HttpEngineFailure]) =
                    fetched(Async.fromFuture(reader.read(): Future[dom.Chunk[Uint8Array]])).map { chunk =>
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

    /** The response `Headers` (a `[name, value]` iterable) as kyo-http headers, in order. */
    private def readHeaders(headers: dom.Headers): HttpHeaders =
        js.Array.from(headers).foldLeft(HttpHeaders.empty)((acc, pair) => acc.add(pair(0), pair(1)))
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
