package kyo.apollo.network.http

import java.net.InetAddress
import java.net.ServerSocket
import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.HttpMethod

/** The JVM/Native [[HttpClientEngine]]'s failure row: kyo-http's typed
  * `HttpException` arrives as the engine's `Abort[HttpEngineFailure]` — an
  * [[ApolloNetworkException]] that keeps the kyo-http leaf as its `cause` — on the
  * plain, the multipart and the streaming path, never as a thrown exception.
  */
class HttpClientEngineSpec extends kyo.test.Test[Any]:

    // Opens client sockets against a local port; run leaves sequentially and skip the
    // socket leak check, as the other engine suites do.
    override def config = super.config.sequential.leakCheckSockets(false)

    /** A loopback port nothing listens on: bound by the OS, then released. */
    private def closedPort(): Int =
        val socket = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
        val port   = socket.getLocalPort
        socket.close()
        port
    end closedPort

    private def post(url: String): HttpRequest =
        HttpRequest(HttpMethod.Post, url, List(HttpHeader("Content-Type", "application/json")), Some("{}"))

    private def expectEngineFailure[C <: Throwable](result: Result[HttpEngineFailure, Any])(using
        ct: ConcreteTag[C],
        scope: kyo.test.AssertScope
    ): Unit =
        result match
            case Result.Failure(e: ApolloNetworkException) =>
                assert(ct.accepts(e.getCause), s"expected the kyo-http leaf as cause, got ${e.getCause}")
            case other => fail(s"expected an ApolloNetworkException engine failure, got $other")

    "a refused connection is an engine failure carrying kyo-http's HttpConnectException" in {
        Abort.run[HttpEngineFailure](new HttpClientEngine().execute(post(s"http://127.0.0.1:${closedPort()}/graphql")))
            .map(expectEngineFailure[HttpConnectException])
    }

    "an unparseable URL on the multipart path is an engine failure, not a thrown exception" in {
        // The multipart path used to parse the URL with getOrThrow, outside any row.
        val upload = post("").copy(body = None, formBody = Some(HttpForm(List("operations" -> "{}"), Nil)))
        Abort.run[HttpEngineFailure](new HttpClientEngine().execute(upload))
            .map(expectEngineFailure[HttpUrlParseException])
    }

    "streaming: a refused connection fails the response head with the engine failure" in {
        Abort.run[HttpEngineFailure](
            Scope.run(new HttpClientEngine().executeStreaming(post(s"http://127.0.0.1:${closedPort()}/graphql")))
        ).map(expectEngineFailure[HttpConnectException])
    }
end HttpClientEngineSpec
