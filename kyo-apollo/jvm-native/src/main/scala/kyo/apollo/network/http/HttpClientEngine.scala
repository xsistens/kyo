package kyo.apollo.network.http

import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.StandardCharsets
import kyo.{HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.HttpMethod

/** The JVM/Native production [[HttpEngine]]: issues requests through kyo-http's
  * `Async` HTTP client (over kyo-net sockets), the counterpart to JS/Wasm's
  * `FetchHttpEngine`.
  *
  * kyo-http's body methods fail with `HttpStatusException` on non-2xx, but apollo's
  * contract is that a non-2xx status still *completes* (the transport interprets
  * status), so the `*Response` variants are used with `failOnError = false`. A
  * genuine transport failure (`Abort[HttpException]`) is re-raised as a panic, which
  * `HttpNetworkTransport` folds into an `ApolloNetworkException` value.
  *
  * [[executeStreaming]] is overridden for real incremental delivery (`@defer`
  * `multipart/mixed`): kyo-http hands the live body only *inside* the `sendWith`
  * continuation (the connection is released when it returns), whereas the seam must
  * hand a lazy [[HttpStreamBody.Chunked]] stream back to the transport for later
  * consumption. The two are bridged by a `Scope`-managed producer fiber that runs
  * the full `sendWith`, reports the response head through a promise, and pumps the
  * decoded body into a bounded [[Channel]] the returned stream drains — the same
  * shape kyo-http itself uses internally to expose a chunked body.
  *
  * `import kyo.*` is deliberately avoided: kyo-http also defines `HttpRequest` /
  * `HttpResponse`, which a wildcard import would shadow over apollo's same-named,
  * same-package types.
  */
final class HttpClientEngine extends HttpEngine:

    def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
        val headers = request.headers.map(h => h.name -> h.value)
        val call =
            request.method match
                case HttpMethod.Get =>
                    HttpClient.getTextResponse(request.url, headers, failOnError = false)
                case HttpMethod.Post =>
                    HttpClient.postTextResponse(request.url, request.body.getOrElse(""), headers, failOnError = false)
        Abort.run[HttpException](call).map {
            case Result.Success(resp) =>
                val hs = List.newBuilder[HttpHeader]
                resp.headers.foreach((n, v) => hs += HttpHeader(n, v))
                HttpResponse(resp.status.code, hs.result(), resp.fields.body)
            case Result.Failure(e) => Sync.defer(throw e)
            case Result.Panic(e)   => Sync.defer(throw e)
        }
    end execute

    override def executeStreaming(request: HttpRequest)(using Frame): HttpStreamResponse < (Async & Scope) =
        for
            head   <- Fiber.Promise.init[(Int, List[HttpHeader]), Abort[Throwable]]
            chunks <- Channel.init[Maybe[String]](chunkBufferSize)
            _      <- Fiber.init(runStreaming(request, head, chunks))
            resp <- Abort.run[Throwable](head.get).map {
                case Result.Success((code, headers)) =>
                    HttpStreamResponse(code, headers, HttpStreamBody.Chunked(bodyStream(chunks)))
                case Result.Failure(e) => Sync.defer(throw e)
                case Result.Panic(e)   => Sync.defer(throw e)
            }
        yield resp

    /** The lazy consumer side of the bridge: pull decoded text chunks from the body
      * channel until the producer's [[Absent]] end-marker. Deliberately `take`-based
      * (not `streamUntilClosed`) — a bounded channel's `close` hands its backlog to
      * the closer, so a producer that pushes several chunks then closes would drop
      * every chunk the consumer had not yet taken (the exact trap the browser
      * `FetchHttpEngine` documents). An explicit end-marker never drops a chunk.
      */
    private def bodyStream(chunks: Channel[Maybe[String]])(using Frame): Stream[String, Async] =
        Stream.repeatPresent {
            Abort.run[Closed](chunks.take).map {
                case Result.Success(Present(text)) => Present(Seq(text))
                case _                             => Absent
            }
        }

    /** Run the whole streamed round-trip in the producer fiber: complete `head` with
      * the response status + headers as soon as they land, then decode the live body
      * (UTF-8, stateful across byte-chunk boundaries) into `chunks`, ending with an
      * [[Absent]] marker. A failure before the head lands fails `head`; the `sendWith`
      * continuation drains fully before returning, so kyo-http releases the connection
      * only once the body is done (or the enclosing `Scope` interrupts this fiber and
      * tears it down).
      */
    private def runStreaming(
        request: HttpRequest,
        head: Fiber.Promise[(Int, List[HttpHeader]), Abort[Throwable]],
        chunks: Channel[Maybe[String]]
    )(using Frame): Unit < Async =
        val url = HttpUrl.parse(request.url).getOrThrow
        val send: Unit < (Async & Abort[HttpException]) =
            HttpClient.use { client =>
                request.method match
                    case HttpMethod.Get =>
                        val route = HttpRoute.getRaw("").response(_.bodyStream)
                        val req   = withHeaders(kyo.HttpRequest.getRaw(url), request)
                        client.sendWith(route, req)(drainInto(head, chunks))
                    case HttpMethod.Post =>
                        val route = HttpRoute.postRaw("").request(_.bodyText).response(_.bodyStream)
                        val req   = withHeaders(kyo.HttpRequest.postRaw(url).addField("body", request.body.getOrElse("")), request)
                        client.sendWith(route, req)(drainInto(head, chunks))
            }
        Abort.run[HttpException](send).map {
            case Result.Success(_) => ()
            case Result.Failure(e) => head.completeDiscard(Result.fail(e)).andThen(offerEnd(chunks))
            case Result.Panic(e)   => head.completeDiscard(Result.fail(e)).andThen(offerEnd(chunks))
        }
    end runStreaming

    private def withHeaders[F](req: kyo.HttpRequest[F], request: HttpRequest): kyo.HttpRequest[F] =
        request.headers.foldLeft(req)((r, h) => r.setHeader(h.name, h.value))

    private def drainInto(
        head: Fiber.Promise[(Int, List[HttpHeader]), Abort[Throwable]],
        chunks: Channel[Maybe[String]]
    )(resp: kyo.HttpResponse["body" ~ Stream[Span[Byte], Async]])(using Frame): Unit < (Async & Abort[HttpException]) =
        val hs = List.newBuilder[HttpHeader]
        resp.headers.foreach((n, v) => hs += HttpHeader(n, v))
        head.completeDiscard(Result.succeed((resp.status.code, hs.result()))).andThen {
            val decoder = new Utf8ChunkDecoder
            resp.fields.body
                .foreach(span => offerText(chunks, decoder.decode(span.toArrayUnsafe)))
                .andThen(offerText(chunks, decoder.flush()))
                .andThen(offerEnd(chunks))
        }
    end drainInto

    /** Push one decoded chunk, treating a `Closed` channel (the consumer abandoned
      * the stream) as a benign stop rather than a failure. Empty text is skipped.
      */
    private def offerText(chunks: Channel[Maybe[String]], text: String)(using Frame): Unit < Async =
        if text.isEmpty then ()
        else Abort.run[Closed](chunks.put(Present(text))).unit

    /** Push the end-of-stream marker (see [[bodyStream]] for why a marker, not close). */
    private def offerEnd(chunks: Channel[Maybe[String]])(using Frame): Unit < Async =
        Abort.run[Closed](chunks.put(Absent)).unit

    private val chunkBufferSize = 16
end HttpClientEngine

/** A stateful UTF-8 decoder for a byte stream arriving in arbitrary chunks: a
  * multibyte character split across a chunk boundary is held back and completed by
  * the next chunk (the JVM/Native analog of the browser `TextDecoder({stream:true})`
  * the `FetchHttpEngine` relies on), so `@defer` payloads are never corrupted at a
  * boundary. Single-fiber use only — no synchronization.
  */
final private[http] class Utf8ChunkDecoder:
    private val decoder            = StandardCharsets.UTF_8.newDecoder()
    private var carry: Array[Byte] = Array.emptyByteArray

    /** Decode `bytes` (prepended with any bytes carried from the previous chunk),
      * emitting the text decodable so far and retaining a trailing partial char.
      */
    def decode(bytes: Array[Byte]): String =
        if bytes.isEmpty then ""
        else
            val in = ByteBuffer.allocate(carry.length + bytes.length)
            in.put(carry).put(bytes)
            in.flip()
            val out = CharBuffer.allocate(carry.length + bytes.length + 1)
            discard(decoder.decode(in, out, false))
            carry = new Array[Byte](in.remaining())
            in.get(carry)
            out.flip()
            out.toString

    /** Flush the decoder at end-of-input — any bytes still carried are an incomplete
      * trailing sequence and decode to the replacement char.
      */
    def flush(): String =
        val in  = ByteBuffer.wrap(carry)
        val out = CharBuffer.allocate(carry.length + 1)
        discard(decoder.decode(in, out, true))
        discard(decoder.flush(out))
        carry = Array.emptyByteArray
        out.flip()
        out.toString
    end flush
end Utf8ChunkDecoder
