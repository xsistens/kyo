package kyo.apollo.interceptor

import kyo.*
import kyo.apollo.exception.HttpEngineFailure
import kyo.apollo.network.http.HttpEngine
import kyo.apollo.network.http.HttpRequestBody

/** Tests the HTTP-layer interceptor chain: order of invocation, terminal-engine
  * delegation, the concrete `AuthorizationHeaderInterceptor`, and the `asEngine`
  * adapter. The `LoggingInterceptor` has its own [[LoggingInterceptorSpec]].
  */
class HttpInterceptorChainSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** An engine that records the request it received and returns 200, echoing the
      * request headers back so header mutations are observable.
      */
    private def recordingEngine(record: HttpEngine.Request => Unit): HttpEngine =
        new HttpEngine:
            def execute(request: HttpEngine.Request)(using Frame): HttpEngine.Response < Async =
                record(request)
                HttpEngine.response(HttpStatus.OK, "ok", request.headers)

    private def request: HttpEngine.Request =
        HttpEngine.request(
            HttpMethod.POST,
            HttpUrl(Present("https"), "example.com", 443, "/graphql", Absent),
            HttpHeaders.empty,
            HttpRequestBody.Text("{}")
        )

    "HttpInterceptorChain" - {

        "an empty interceptor list proceeds straight to the engine" in {
            var got   = Option.empty[HttpEngine.Request]
            val chain = DefaultHttpInterceptorChain(Chunk.empty, 0, recordingEngine(r => got = Some(r)))
            chain.proceed(request).map { response =>
                assert(response.status == HttpStatus.OK)
                assert(got.map(_.url.full) == Some("https://example.com/graphql"))
            }
        }

        "AuthorizationHeaderInterceptor appends the auth header before the engine" in {
            var got = Option.empty[HttpEngine.Request]
            val chain = DefaultHttpInterceptorChain(
                Chunk(AuthorizationHeaderInterceptor("Bearer t0ken")),
                0,
                recordingEngine(r => got = Some(r))
            )
            chain.proceed(request).map { _ =>
                assert(got.get.headers == HttpHeaders.empty.add("Authorization", "Bearer t0ken"))
            }
        }

        "AuthorizationHeaderInterceptor honours a custom header name" in {
            var got = Option.empty[HttpEngine.Request]
            val chain = DefaultHttpInterceptorChain(
                Chunk(AuthorizationHeaderInterceptor("k3y", headerName = "X-Api-Key")),
                0,
                recordingEngine(r => got = Some(r))
            )
            chain.proceed(request).map { _ =>
                assert(got.get.headers == HttpHeaders.empty.add("X-Api-Key", "k3y"))
            }
        }

        "interceptors run in registration order, wrapping the engine" in {
            var order = List.empty[String]
            val a: HttpInterceptor = new HttpInterceptor:
                def intercept(req: HttpEngine.Request, next: HttpInterceptorChain)(using
                    Frame
                ): HttpEngine.Response < (Async & Abort[HttpEngineFailure]) =
                    order = order :+ "a-before"
                    next.proceed(req).map { r =>
                        order = order :+ "a-after"; r
                    }
                end intercept
            val b: HttpInterceptor = new HttpInterceptor:
                def intercept(req: HttpEngine.Request, next: HttpInterceptorChain)(using
                    Frame
                ): HttpEngine.Response < (Async & Abort[HttpEngineFailure]) =
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

        "asEngine runs the request through the chain as a plain HttpEngine" in {
            var got = Option.empty[HttpEngine.Request]
            val engine = HttpInterceptorChain.asEngine(
                List(AuthorizationHeaderInterceptor("Bearer z")),
                recordingEngine(r => got = Some(r))
            )
            engine.execute(request).map { _ =>
                assert(got.get.headers == HttpHeaders.empty.add("Authorization", "Bearer z"))
            }
        }

        "asEngine with no interceptors returns the engine verbatim" in {
            val base = recordingEngine(_ => ())
            assert(HttpInterceptorChain.asEngine(Nil, base) eq base)
        }
    }
end HttpInterceptorChainSpec
