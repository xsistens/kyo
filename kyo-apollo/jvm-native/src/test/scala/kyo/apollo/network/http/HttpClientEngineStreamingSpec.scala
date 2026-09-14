package kyo.apollo.network.http

import java.nio.charset.StandardCharsets
import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.StreamProbe
import kyo.apollo.api.JsonCodec
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.HttpMethod

/** The JVM/Native [[HttpClientEngine]]'s real incremental-delivery path: its
  * `executeStreaming` override reads the response body as a live byte stream over
  * kyo-http and decodes it to UTF-8 statefully, rather than inheriting the buffered
  * default. Two angles:
  *
  *   - a direct check that [[Utf8ChunkDecoder]] carries a multibyte character split
  *     across two byte chunks (the property the streaming path depends on), and
  *   - an end-to-end check against a real kyo-http server that streams a
  *     `multipart/mixed` body in pieces — the engine must surface it as a
  *     [[HttpStreamBody.Chunked]] stream whose reassembled text is byte-exact.
  */
class HttpClientEngineStreamingSpec extends kyo.test.Test[Any]:

    // Starts a real server (an ephemeral listener fd the NIO transport defers closing,
    // as kyo-http's own suites note); run leaves sequentially and skip the socket leak check.
    override def config = super.config.sequential.leakCheckSockets(false)

    "Utf8ChunkDecoder carries a multibyte char split across two byte chunks" in {
        val decoder = new Utf8ChunkDecoder
        // 'ë' (U+00EB) is 0xC3 0xAB in UTF-8; feed the two bytes in separate chunks.
        val first  = decoder.decode(Array(0x5a.toByte, 0x6f.toByte, 0xc3.toByte)) // "Zo" + lead byte
        val second = decoder.decode(Array(0xab.toByte))                           // continuation byte
        val tail   = decoder.flush()
        assert(first == "Zo") // the partial 'ë' is held back, not corrupted
        assert(second == "ë") // completed on the next chunk
        assert(tail.isEmpty)
        assert(first + second + tail == "Zoë")
    }

    "Utf8ChunkDecoder replaces a malformed byte and keeps decoding (no stall, no unbounded carry)" in {
        val decoder = new Utf8ChunkDecoder
        // 0xFF is never a valid UTF-8 byte. A REPORT-mode decoder would stall here and
        // retain 0xFF plus everything after it in `carry` forever; REPLACE emits U+FFFD
        // and decodes on, matching the browser TextDecoder (fatal:false).
        val first  = decoder.decode(Array(0x41.toByte, 0xff.toByte, 0x42.toByte)) // "A" <bad> "B"
        val second = decoder.decode(Array(0x43.toByte, 0x44.toByte))              // "CD" — no stall
        val tail   = decoder.flush()
        assert(first == "A�B") // malformed byte became the replacement char (U+FFFD), not a stall
        assert(second == "CD") // the bad byte did not poison the carry
        assert(tail.isEmpty)
    }

    /** A trivial query, enough to drive the transport's streaming path (routing to `execute`
      * vs `executeStreaming` is the interceptor's job; here we call executeStreaming directly).
      */
    final private case class Ping() extends kyo.apollo.api.Query[Int]:
        def name: String              = "Ping"
        def document: String          = "query Ping { ping }"
        val dataCodec: JsonCodec[Int] = JsonCodec.fromSchema[Int]
        def rootField: kyo.apollo.api.CompiledField =
            kyo.apollo.api.CompiledField("data", kyo.apollo.api.CompiledNamedType("Query"))
        def variables: kyo.apollo.json.Json = kyo.apollo.json.Json.JObj(scala.collection.immutable.VectorMap.empty)
    end Ping

    "a malformed URL in the streaming path folds to an exception value, never hanging" in {
        // The pre-fix engine parsed the URL with an unguarded getOrThrow inside the streaming
        // producer fiber: a malformed URL panicked that fiber and left `head` forever
        // incomplete, so head.get (and therefore the whole ResponseStream) hung. The fix routes
        // the parse failure through head; the transport then folds it to an ApolloNetworkException
        // value ("failures are values"). The outer timeout turns a regressed hang into a test
        // failure rather than a wedged suite.
        val transport = new HttpNetworkTransport("", new HttpClientEngine)
        Abort.run[Timeout](Async.timeout(15.seconds)(
            StreamProbe.collect(transport.executeStreaming(kyo.apollo.network.ApolloRequest(
                Ping(),
                kyo.apollo.network.TestIds.requestUuid
            )))
        )).map {
            case Result.Failure(_) =>
                fail("executeStreaming hung on a malformed URL — the parse failure was not routed through head")
            case Result.Panic(e) =>
                fail(s"the malformed URL escaped as a panic instead of folding to a value: $e")
            case Result.Success(rs) =>
                assert(rs.nonEmpty, "expected a terminal exception value, got an empty stream")
                assert(
                    rs.last.error.exists(_.isInstanceOf[kyo.apollo.exception.ApolloNetworkException]),
                    s"expected an ApolloNetworkException value for the malformed URL, got $rs"
                )
        }
    }

    "executeStreaming surfaces a multipart/mixed body as a live Chunked stream, UTF-8-exact across chunk boundaries" in {
        val boundary = "graphql"
        val fullText =
            s"--$boundary\r\nContent-Type: application/json\r\n\r\n" +
                "{\"data\":{\"name\":\"Zoë\"}}" + // 'ë' is a 2-byte UTF-8 char
                s"\r\n--$boundary--\r\n"
        val bytes = fullText.getBytes(StandardCharsets.UTF_8)
        // Split the body inside the multibyte 'ë' so the server flushes its lead byte
        // in one chunk and its continuation byte in the next — only a stateful decoder
        // reassembles it correctly, and only a drop-free bridge delivers the tiny tail.
        val leadIdx     = bytes.indexWhere(_ == 0xc3.toByte)
        val (a, b)      = bytes.splitAt(leadIdx + 1)
        val serverBody  = Stream.init(Seq(Span.fromUnsafe(a), Span.fromUnsafe(b)))
        val contentType = s"multipart/mixed; boundary=$boundary"

        val route = HttpRoute.postRaw("graphql").request(_.bodyText).response(_.bodyStream)
        val ep = route.handler { _ =>
            kyo.HttpResponse.ok.addField("body", serverBody).setHeader("Content-Type", contentType)
        }

        HttpServer.init(0, "127.0.0.1")(ep).map { server =>
            val engine = new HttpClientEngine
            val request = HttpRequest(
                method = HttpMethod.Post,
                url = s"http://127.0.0.1:${server.port}/graphql",
                headers = List(HttpHeader("Content-Type", "application/json")),
                body = Some("{}")
            )
            engine.executeStreaming(request).map { resp =>
                assert(resp.statusCode == 200)
                assert(resp.header("Content-Type").exists(_.contains("multipart/mixed")))
                resp.body match
                    case HttpStreamBody.Chunked(stream) =>
                        StreamProbe.collect(stream).map { parts =>
                            val reassembled = parts.mkString
                            assert(reassembled == fullText, s"got ${parts.size} parts: $parts")
                            assert(reassembled.contains("Zoë"))
                        }
                    case other =>
                        fail(s"expected a Chunked streaming body, got $other")
                end match
            }
        }
    }

    "a streamed body reaches the consumer chunk by chunk: the first chunk arrives before the server sends the second" in {
        // The server sends the second chunk only once the consumer has received the first
        // (the latch). A body stream that regroups chunks holds the first one back until
        // more follow, so both sides wait on each other; the timeout only turns that
        // deadlock into a failure, it orders nothing.
        val boundary = "graphql"
        val first    = s"--$boundary\r\nContent-Type: application/json\r\n\r\n{\"data\":{\"a\":1},\"hasNext\":true}\r\n"
        val second   = s"--$boundary\r\nContent-Type: application/json\r\n\r\n{\"incremental\":[],\"hasNext\":false}\r\n--$boundary--\r\n"

        def span(text: String): Span[Byte] = Span.fromUnsafe(text.getBytes(StandardCharsets.UTF_8))

        for
            firstReceived <- Latch.init(1)
            secondSent    <- AtomicBoolean.init(false)
            route = HttpRoute.postRaw("graphql").request(_.bodyText).response(_.bodyStream)
            serverBody = Stream
                .init(Seq(span(first)))
                .concat(Stream.unwrap(firstReceived.await.andThen(secondSent.set(true)).andThen(Stream.init(Seq(span(second))))))
            ep = route.handler { _ =>
                kyo.HttpResponse.ok.addField("body", serverBody).setHeader("Content-Type", s"multipart/mixed; boundary=$boundary")
            }
            server <- HttpServer.init(0, "127.0.0.1")(ep)
            request = HttpRequest(
                method = HttpMethod.Post,
                url = s"http://127.0.0.1:${server.port}/graphql",
                headers = List(HttpHeader("Content-Type", "application/json")),
                body = Some("{}")
            )
            // Each text chunk as it reaches the consumer, with whether the server had sent the second chunk by then.
            consumed = new HttpClientEngine().executeStreaming(request).map { resp =>
                resp.body match
                    case HttpStreamBody.Chunked(stream) =>
                        stream.fold(Chunk.empty[(String, Boolean)]) { (seen, text) =>
                            secondSent.get.map { sent =>
                                if seen.isEmpty then firstReceived.release.andThen(seen.append((text, sent)))
                                else seen.append((text, sent))
                            }
                        }
                    case other => fail(s"expected a Chunked streaming body, got $other")
            }
            result <- Abort.run[Timeout](Async.timeout(30.seconds)(Abort.run[HttpEngineFailure](consumed)))
        yield result match
            case Result.Success(Result.Success(seen)) =>
                assert(seen.nonEmpty)
                assert(seen.head._2 == false, s"the first chunk reached the consumer only after the second was sent: $seen")
                assert(first.startsWith(seen.head._1), s"the first chunk carried more than the first part: $seen")
                assert(
                    seen.map(_._1).mkString == first + second,
                    s"the second chunk was never sent — the consumer got the first only once the connection ended: $seen"
                )
            case Result.Failure(_: Timeout) =>
                fail("the first chunk never reached the consumer before the body ended: the body stream holds chunks back")
            case other => fail(s"streaming failed: $other")
        end for
    }
end HttpClientEngineStreamingSpec
