package kyo.apollo.network.http

import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets
import kyo.*
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.MarkedChannel

/** The JVM/Native production [[HttpEngine]]: sends the request through kyo-http's
  * `Async` HTTP client (over kyo-net sockets), the counterpart to JS/Wasm's
  * `FetchHttpEngine`.
  *
  * The request and the buffered response already are kyo-http's types, so
  * [[execute]] is a delegation to `HttpClient.sendWith` with the route that matches
  * the [[HttpRequestBody]] form and a text response body. `sendWith` hands every
  * status to the continuation (apollo's contract: a non-2xx status still
  * *completes*; the transport interprets it). A genuine transport failure
  * (`Abort[HttpException]`) becomes the engine's `Abort[HttpEngineFailure]`: an
  * [[ApolloNetworkException]] whose `cause` is the kyo-http leaf, so a caller can
  * still tell a refused connection from a DNS failure or a timeout. A panic from
  * kyo-http stays a panic. Interrupting the effect interrupts kyo-http's request.
  *
  * [[executeStreaming]] is overridden for real incremental delivery (`@defer`
  * `multipart/mixed`): kyo-http hands the live body only *inside* the `sendWith`
  * continuation (the connection is released when it returns), whereas the seam must
  * hand a lazy [[HttpStreamBody.Chunked]] stream back to the transport for later
  * consumption. The two are bridged by a `Scope`-managed producer fiber that runs
  * the full `sendWith`, reports the response head through a promise, and pumps the
  * decoded body into a bounded [[Channel]] the returned stream drains — the same
  * shape kyo-http itself uses internally to expose a chunked body.
  */
final class HttpClientEngine extends HttpEngine:
    import HttpClientEngine.*

    def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < (Async & Abort[HttpEngineFailure]) =
        onEngineRow(send(request, textReply)(identity))

    override def executeStreaming(request: HttpEngine.Request)(using
        Frame
    ): HttpEngine.StreamResponse < (Async & Scope & Abort[HttpEngineFailure]) =
        for
            head   <- Fiber.Promise.init[(HttpStatus, HttpHeaders), Abort[HttpEngineFailure]]
            chunks <- Channel.init[Maybe[String]](chunkBufferSize)
            _      <- Fiber.init(runStreaming(request, head, chunks))
            status <- head.get
        yield HttpEngine.streamResponse(status._1, HttpStreamBody.Chunked(bodyStream(chunks)), status._2)

    /** Send `request` over the ambient kyo-http client on the route of `reply` that
      * matches its body form, handing the response to `f` while the connection is
      * held. The method, URL and headers go out as they are.
      */
    private def send[Out, A](request: HttpEngine.Request, reply: Routes[Out])(
        f: HttpResponse[Out] => A < (Async & Abort[HttpException])
    )(using Frame): A < (Async & Abort[HttpException]) =
        val bare = HttpRequest(request.method, request.url, request.headers, Record.empty)
        HttpClient.use { client =>
            request.fields.body match
                case HttpRequestBody.Empty            => client.sendWith(reply.empty, bare)(f)
                case HttpRequestBody.Text(json)       => client.sendWith(reply.text, bare.addField("body", json))(f)
                case HttpRequestBody.Multipart(parts) => client.sendWith(reply.multipart, bare.addField("body", parts.toSeq))(f)
        }
    end send

    /** Move a kyo-http call onto the engine row: its typed failure becomes an
      * [[ApolloNetworkException]], a panic stays a panic.
      */
    private def onEngineRow[A](call: A < (Async & Abort[HttpException]))(using Frame): A < (Async & Abort[HttpEngineFailure]) =
        Abort.run[HttpException](call).map {
            case Result.Success(value) => value
            case Result.Failure(e)     => Abort.fail(engineFailure(e))
            case Result.Panic(e)       => Abort.panic(e)
        }

    /** The engine failure for a kyo-http failure: no response was received. The
      * kyo-http leaf is kept as the `cause`.
      */
    private def engineFailure(e: HttpException)(using Frame): HttpEngineFailure =
        val message = e match
            case _: HttpConnectionException => "Could not connect to the GraphQL server"
            case _: HttpRequestException    => "The GraphQL HTTP request failed before a response arrived"
            case _                          => "The GraphQL HTTP request could not be completed"
        ApolloNetworkException(message, e)
    end engineFailure

    /** The lazy consumer side of the bridge: decoded text chunks from the body channel
      * until the producer's [[Absent]] end-marker, each delivered the moment it is
      * taken — the first part of a `@defer` reply must not wait for the next one. An
      * end-marker, not `close`, so no chunk is dropped ([[MarkedChannel]] has both
      * traps).
      */
    private def bodyStream(chunks: Channel[Maybe[String]])(using Frame): Stream[String, Async] =
        MarkedChannel.untilEnd(chunks)

    /** Run the whole streamed round-trip in the producer fiber: complete `head` with
      * the response status + headers as soon as they land, then decode the live body
      * (UTF-8, stateful across byte-chunk boundaries) into `chunks`, ending with an
      * [[Absent]] marker. A failure before the head lands fails `head` with the engine
      * failure (a panic with the panic); the `sendWith` continuation drains fully
      * before returning, so kyo-http releases the connection only once the body is
      * done (or the enclosing `Scope` interrupts this fiber and tears it down).
      */
    private def runStreaming(
        request: HttpEngine.Request,
        head: Fiber.Promise[(HttpStatus, HttpHeaders), Abort[HttpEngineFailure]],
        chunks: Channel[Maybe[String]]
    )(using Frame): Unit < Async =
        Abort.run[HttpException](send(request, streamReply)(drainInto(head, chunks))).map {
            case Result.Success(_) => ()
            case Result.Failure(e) => head.completeDiscard(Result.fail(engineFailure(e))).andThen(offerEnd(chunks))
            case Result.Panic(e)   => head.completeDiscard(Result.panic(e)).andThen(offerEnd(chunks))
        }

    private def drainInto(
        head: Fiber.Promise[(HttpStatus, HttpHeaders), Abort[HttpEngineFailure]],
        chunks: Channel[Maybe[String]]
    )(resp: HttpResponse["body" ~ Stream[Span[Byte], Async]])(using Frame): Unit < (Async & Abort[HttpException]) =
        head.completeDiscard(Result.succeed((resp.status, resp.headers))).andThen {
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

private object HttpClientEngine:

    /** One kyo-http route per [[HttpRequestBody]] form, all reading the response body
      * the same way (`Out`). The routes carry only codecs: the method on the wire is
      * the request's, and the path is the request URL's.
      */
    final case class Routes[Out](
        empty: HttpRoute[Any, Out, Any],
        text: HttpRoute["body" ~ String, Out, Any],
        multipart: HttpRoute["body" ~ Seq[HttpRequest.Part], Out, Any]
    )

    /** The routes of [[HttpClientEngine.execute]]: the body read as text. */
    val textReply: Routes["body" ~ String] = Routes(
        HttpRoute.getRaw("").response(_.bodyText),
        HttpRoute.postRaw("").request(_.bodyText).response(_.bodyText),
        HttpRoute.postRaw("").request(_.bodyMultipart).response(_.bodyText)
    )

    /** The routes of [[HttpClientEngine.executeStreaming]]: the body as a live byte stream. */
    val streamReply: Routes["body" ~ Stream[Span[Byte], Async]] = Routes(
        HttpRoute.getRaw("").response(_.bodyStream),
        HttpRoute.postRaw("").request(_.bodyText).response(_.bodyStream),
        HttpRoute.postRaw("").request(_.bodyMultipart).response(_.bodyStream)
    )
end HttpClientEngine

/** A stateful UTF-8 decoder for a byte stream arriving in arbitrary chunks: a
  * multibyte character split across a chunk boundary is held back and completed by
  * the next chunk (the JVM/Native analog of the browser `TextDecoder({stream:true})`
  * the `FetchHttpEngine` relies on), so `@defer` payloads are never corrupted at a
  * boundary. Single-fiber use only — no synchronization.
  *
  * Malformed / unmappable input is REPLACEd with U+FFFD, matching the browser
  * `TextDecoder`'s default (`fatal: false`). The default REPORT action would instead
  * stop at the first bad byte and leave it (plus everything after it) in `carry`,
  * stalling the stream and growing `carry` without bound; REPLACE consumes the bad
  * byte and decodes on. A legitimate incomplete trailing sequence still returns
  * UNDERFLOW and is carried, not replaced, so boundary splits are unaffected.
  */
final private[http] class Utf8ChunkDecoder:
    private val decoder =
        StandardCharsets.UTF_8.newDecoder()
            .onMalformedInput(CodingErrorAction.REPLACE)
            .onUnmappableCharacter(CodingErrorAction.REPLACE)
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
