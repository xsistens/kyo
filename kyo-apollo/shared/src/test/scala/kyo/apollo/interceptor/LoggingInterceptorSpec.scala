package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.cache.LogProbe
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequestBody

/** Tests the [[LoggingInterceptor]]: it logs through the ambient `Log` when it runs
  * and never when its effect is only built, at `debug` for the request line and
  * status and `trace` for headers and body, and it never writes a credential.
  */
class LoggingInterceptorSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** An engine that counts its executions and answers 200 with a session cookie. */
    final private class CountingEngine(executed: AtomicInt) extends HttpEngine:
        def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < Async =
            executed.incrementAndGet.andThen(
                HttpEngine.response(HttpStatus.OK, "ok", HttpHeaders.empty.add("Set-Cookie", "session=s3cr3t"))
            )
    end CountingEngine

    private val url = HttpUrl(Present("https"), "example.com", 443, "/graphql", Absent)

    private def request(headers: (String, String)*): HttpEngine.Request =
        HttpEngine.request(HttpMethod.POST, url, HttpHeaders.init(headers), HttpRequestBody.Text("""{"query":"{ x }"}"""))

    private def chainOver(engine: HttpEngine): HttpInterceptorChain =
        DefaultHttpInterceptorChain(Chunk(new LoggingInterceptor()), 0, engine)

    "LoggingInterceptor" - {

        "a request that is built but never run logs nothing" in {
            for
                probe    <- LogProbe.init
                executed <- AtomicInt.init
                chain = chainOver(CountingEngine(executed))
                _        <- probe.run(Sync.defer(discard(chain.proceed(request("X-Trace" -> "abc")))))
                built    <- probe.lines
                runs     <- executed.get
                _        <- probe.run(chain.proceed(request("X-Trace" -> "abc")))
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
                response <- probe.run(chainOver(CountingEngine(executed)).proceed(request("X-Trace" -> "abc")))
                lines    <- probe.lines
            yield
                val debug = lines.filter(_.level == Log.Level.debug).map(_.message)
                val trace = lines.filter(_.level == Log.Level.trace).map(_.message)
                assert(response.status == HttpStatus.OK && response.fields.body == "ok")
                assert(debug == Chunk("--> POST https://example.com/graphql", "<-- 200 (POST https://example.com/graphql)"), s"$debug")
                assert(trace.exists(_.contains("X-Trace: abc")), s"$trace")
                assert(trace.exists(_.contains("""{"query":"{ x }"}""")), s"$trace")
            end for
        }

        "credential headers are redacted on the request and the response, whatever their case" in {
            for
                probe    <- LogProbe.init
                executed <- AtomicInt.init
                _ <- probe.run(chainOver(CountingEngine(executed)).proceed(request(
                    "Authorization"       -> "Bearer x",
                    "cookie"              -> "id=42",
                    "PROXY-AUTHORIZATION" -> "Basic cHJveHk=",
                    "X-Trace"             -> "abc"
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

        "a mutation's variables never reach the log: every value is redacted and their shape stays visible" in {
            val login =
                s"""{"query":"mutation Login($$email: String!, $$password: String!) { login(email: $$email, password: $$password) }",""" +
                    s""""operationName":"Login","variables":{"email":"${Secrets.email}","credentials":{"password":"${Secrets.password}","remember":true}}}"""
            for
                probe    <- LogProbe.init
                executed <- AtomicInt.init
                _        <- probe.run(chainOver(CountingEngine(executed)).proceed(withBody(HttpRequestBody.Text(login))))
                lines    <- probe.lines
            yield
                val messages = lines.map(_.message)
                assert(
                    messages.forall(m => !m.contains(Secrets.password) && !m.contains(Secrets.email)),
                    s"a variable reached the log: $messages"
                )
                assert(
                    messages.exists(_.contains(
                        """"variables":{"email":"<redacted>","credentials":{"password":"<redacted>","remember":"<redacted>"}}"""
                    )),
                    s"the variables' shape is not in the log: $messages"
                )
                assert(messages.exists(_.contains(""""operationName":"Login"""")), s"$messages")
            end for
        }

        "a batched body has the variables of every element redacted" in {
            val batch =
                s"""[{"operationName":"A","variables":{"token":"${Secrets.password}"}},{"operationName":"B","variables":{"email":"${Secrets.email}"}}]"""
            for
                probe    <- LogProbe.init
                executed <- AtomicInt.init
                _        <- probe.run(chainOver(CountingEngine(executed)).proceed(withBody(HttpRequestBody.Text(batch))))
                lines    <- probe.lines
            yield
                val messages = lines.map(_.message)
                assert(
                    messages.forall(m => !m.contains(Secrets.password) && !m.contains(Secrets.email)),
                    s"a variable reached the log: $messages"
                )
                assert(
                    messages.exists(_.contains(
                        """[{"operationName":"A","variables":{"token":"<redacted>"}},{"operationName":"B","variables":{"email":"<redacted>"}}]"""
                    )),
                    s"$messages"
                )
            end for
        }

        "a body that is not JSON is logged as its kind and length, never as text" in {
            val parts = Chunk(
                HttpRequest.Part(
                    "operations",
                    Absent,
                    Absent,
                    Span.from(s"""{"variables":{"password":"${Secrets.password}","file":null}}""".getBytes)
                ),
                HttpRequest.Part("map", Absent, Absent, Span.from("""{"0":["variables.file"]}""".getBytes)),
                HttpRequest.Part("0", Present("a.txt"), Present("text/plain"), Span.from(Secrets.password.getBytes))
            )
            for
                probe    <- LogProbe.init
                executed <- AtomicInt.init
                _        <- probe.run(chainOver(CountingEngine(executed)).proceed(withBody(HttpRequestBody.Multipart(parts))))
                _ <- probe.run(chainOver(CountingEngine(executed)).proceed(withBody(HttpRequestBody.Text(s"password=${Secrets.password}"))))
                lines <- probe.lines
            yield
                val messages = lines.map(_.message)
                assert(messages.forall(!_.contains(Secrets.password)), s"a body reached the log as text: $messages")
                assert(messages.exists(_.contains("multipart/form-data body, 3 parts")), s"$messages")
                assert(
                    messages.exists(_.contains(s"text body, ${s"password=${Secrets.password}".length} characters, not JSON")),
                    s"$messages"
                )
            end for
        }

        "a GET request line leaves out the query string, which carries the variables" in {
            val get = HttpEngine.request(
                HttpMethod.GET,
                url.copy(rawQuery = Present(s"operationName=Me&variables=%7B%22token%22%3A%22${Secrets.password}%22%7D")),
                HttpHeaders.empty,
                HttpRequestBody.Empty
            )
            for
                probe    <- LogProbe.init
                executed <- AtomicInt.init
                _        <- probe.run(chainOver(CountingEngine(executed)).proceed(get))
                lines    <- probe.lines
            yield
                val messages = lines.map(_.message)
                assert(messages.forall(!_.contains(Secrets.password)), s"the query string reached the log: $messages")
                assert(messages.contains("--> GET https://example.com/graphql"), s"$messages")
            end for
        }
    }

    private def withBody(body: HttpRequestBody): HttpEngine.Request =
        HttpEngine.request(HttpMethod.POST, url, HttpHeaders.empty, body)
end LoggingInterceptorSpec

/** Values that must never appear in a log line. */
private object Secrets:
    val password = "hunter2-XYZZY-5731"
    val email    = "carol.secret@example.test"
end Secrets
