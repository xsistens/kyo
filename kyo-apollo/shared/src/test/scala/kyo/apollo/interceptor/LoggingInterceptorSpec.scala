package kyo.apollo.interceptor

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.cache.LogProbe
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.HttpMethod
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse

/** Tests the [[LoggingInterceptor]]: it logs through the ambient `Log` when it runs
  * and never when its effect is only built, at `debug` for the request line and
  * status and `trace` for headers and body, and it never writes a credential.
  */
class LoggingInterceptorSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** An engine that counts its executions and answers 200 with a session cookie. */
    final private class CountingEngine(executed: AtomicInt) extends HttpEngine:
        def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
            executed.incrementAndGet.andThen(HttpResponse(200, List(HttpHeader("Set-Cookie", "session=s3cr3t")), "ok"))

    private def request(headers: HttpHeader*): HttpRequest =
        HttpRequest(HttpMethod.Post, "https://example.com/graphql", headers.toList, Some("""{"query":"{ x }"}"""))

    private def chainOver(engine: HttpEngine): HttpInterceptorChain =
        DefaultHttpInterceptorChain(Chunk(new LoggingInterceptor()), 0, engine)

    "LoggingInterceptor" - {

        "a request that is built but never run logs nothing" in {
            for
                probe    <- LogProbe.init
                executed <- AtomicInt.init
                chain = chainOver(CountingEngine(executed))
                _        <- probe.run(Sync.defer(discard(chain.proceed(request(HttpHeader("X-Trace", "abc"))))))
                built    <- probe.lines
                runs     <- executed.get
                _        <- probe.run(chain.proceed(request(HttpHeader("X-Trace", "abc"))))
                afterRun <- probe.lines
            yield
                assert(runs == 0, s"the engine ran for a request that was only built: $runs")
                assert(built.isEmpty, s"a request that was only built was logged: $built")
                assert(afterRun.nonEmpty, "running the same request must log it")
            end for
        }

        "a run logs the request line and status at debug, headers and body at trace, and passes the response through" in {
            for
                probe    <- LogProbe.init
                executed <- AtomicInt.init
                response <- probe.run(chainOver(CountingEngine(executed)).proceed(request(HttpHeader("X-Trace", "abc"))))
                lines    <- probe.lines
            yield
                val debug = lines.filter(_.level == Log.Level.debug).map(_.message)
                val trace = lines.filter(_.level == Log.Level.trace).map(_.message)
                assert(response.statusCode == 200 && response.body == "ok")
                assert(debug == Chunk("--> Post https://example.com/graphql", "<-- 200 (Post https://example.com/graphql)"), s"$debug")
                assert(trace.exists(_.contains("X-Trace: abc")), s"$trace")
                assert(trace.exists(_.contains("""{"query":"{ x }"}""")), s"$trace")
            end for
        }

        "credential headers are redacted on the request and the response, whatever their case" in {
            for
                probe    <- LogProbe.init
                executed <- AtomicInt.init
                _ <- probe.run(chainOver(CountingEngine(executed)).proceed(request(
                    HttpHeader("Authorization", "Bearer x"),
                    HttpHeader("cookie", "id=42"),
                    HttpHeader("PROXY-AUTHORIZATION", "Basic cHJveHk="),
                    HttpHeader("X-Trace", "abc")
                )))
                lines <- probe.lines
            yield
                val messages = lines.map(_.message)
                val leaked =
                    messages.filter(m => m.contains("Bearer x") || m.contains("id=42") || m.contains("cHJveHk=") || m.contains("s3cr3t"))
                assert(leaked.isEmpty, s"a credential reached the log: $leaked")
                assert(messages.exists(_.contains("Authorization: <redacted>")), s"$messages")
                assert(messages.exists(_.contains("Set-Cookie: <redacted>")), s"$messages")
                assert(messages.exists(_.contains("X-Trace: abc")), s"$messages")
            end for
        }
    }
end LoggingInterceptorSpec
