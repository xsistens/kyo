package kyo.apollo.interceptor

import kyo.{HttpMethod as _, HttpRequest as _, HttpResponse as _, *}
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.HttpHeader
import kyo.apollo.network.HttpMethod
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequest
import kyo.apollo.network.http.HttpResponse

/** Tests the HTTP-layer interceptor chain: order of invocation, terminal-engine
  * delegation, the two concrete example interceptors, and the `asEngine` adapter.
  */
class HttpInterceptorChainSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** An engine that records the request it received and returns 200, echoing the
      * request headers back so header mutations are observable.
      */
    private def recordingEngine(record: HttpRequest => Unit): HttpEngine =
        new HttpEngine:
            def execute(request: HttpRequest)(using Frame): HttpResponse < Async =
                record(request)
                HttpResponse(200, request.headers, "ok")

    private def request: HttpRequest =
        HttpRequest(HttpMethod.Post, "https://example.com/graphql", Nil, Some("{}"))

    "HttpInterceptorChain" - {

        "an empty interceptor list proceeds straight to the engine" in {
            var got   = Option.empty[HttpRequest]
            val chain = DefaultHttpInterceptorChain(Chunk.empty, 0, recordingEngine(r => got = Some(r)))
            chain.proceed(request).map { response =>
                assert(response.statusCode == 200)
                assert(got.map(_.url) == Some("https://example.com/graphql"))
            }
        }

        "AuthorizationHeaderInterceptor appends the auth header before the engine" in {
            var got = Option.empty[HttpRequest]
            val chain = DefaultHttpInterceptorChain(
                Chunk(AuthorizationHeaderInterceptor("Bearer t0ken")),
                0,
                recordingEngine(r => got = Some(r))
            )
            chain.proceed(request).map { _ =>
                assert(got.get.headers == List(HttpHeader("Authorization", "Bearer t0ken")))
            }
        }

        "AuthorizationHeaderInterceptor honours a custom header name" in {
            var got = Option.empty[HttpRequest]
            val chain = DefaultHttpInterceptorChain(
                Chunk(AuthorizationHeaderInterceptor("k3y", headerName = "X-Api-Key")),
                0,
                recordingEngine(r => got = Some(r))
            )
            chain.proceed(request).map { _ =>
                assert(got.get.headers == List(HttpHeader("X-Api-Key", "k3y")))
            }
        }

        "interceptors run in registration order, wrapping the engine" in {
            var order = List.empty[String]
            val a: HttpInterceptor = new HttpInterceptor:
                def intercept(req: HttpRequest, next: HttpInterceptorChain)(using
                    Frame
                ): HttpResponse < (Async & Abort[HttpEngineFailure]) =
                    order = order :+ "a-before"
                    next.proceed(req).map { r =>
                        order = order :+ "a-after"; r
                    }
                end intercept
            val b: HttpInterceptor = new HttpInterceptor:
                def intercept(req: HttpRequest, next: HttpInterceptorChain)(using
                    Frame
                ): HttpResponse < (Async & Abort[HttpEngineFailure]) =
                    order = order :+ "b-before"
                    next.proceed(req).map { r =>
                        order = order :+ "b-after"; r
                    }
                end intercept
            val chain = DefaultHttpInterceptorChain(
                Chunk(a, b),
                0,
                recordingEngine(_ => order = order :+ "engine")
            )
            chain.proceed(request).map { _ =>
                assert(order == List("a-before", "b-before", "engine", "b-after", "a-after"))
            }
        }

        "LoggingInterceptor logs request and response without altering the response" in {
            var logs = List.empty[String]
            val chain = DefaultHttpInterceptorChain(
                Chunk(new LoggingInterceptor(l => logs = logs :+ l)),
                0,
                recordingEngine(_ => ())
            )
            chain.proceed(request).map { response =>
                assert(response.statusCode == 200)
                assert(logs.exists(_.contains("Post")), s"no request line in $logs")
                assert(logs.exists(_.contains("200")), s"no response line in $logs")
            }
        }

        "asEngine runs the request through the chain as a plain HttpEngine" in {
            var got = Option.empty[HttpRequest]
            val engine = HttpInterceptorChain.asEngine(
                List(AuthorizationHeaderInterceptor("Bearer z")),
                recordingEngine(r => got = Some(r))
            )
            engine.execute(request).map { _ =>
                assert(got.get.headers == List(HttpHeader("Authorization", "Bearer z")))
            }
        }

        "asEngine with no interceptors returns the engine verbatim" in {
            val base = recordingEngine(_ => ())
            assert(HttpInterceptorChain.asEngine(Nil, base) eq base)
        }
    }
end HttpInterceptorChainSpec
