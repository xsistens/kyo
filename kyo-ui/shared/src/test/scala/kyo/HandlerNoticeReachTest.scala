package kyo

import kyo.internal.MouseEventData
import kyo.internal.UIEvent

/** A boundary's `onNotice` sink travels with the UI, so a failing handler reaches it on the server-push transport exactly as in the
  * browser engine: the app code is the same for both. Here the UI is served by a real [[HttpServer]] from [[UI.runHandlers]], whose
  * session fibers inherit nothing from whoever built the handlers.
  */
class HandlerNoticeReachTest extends kyo.test.Test[Any]:

    private val click = UIEvent.Click(Seq("0", "0"), MouseEventData(UI.Modifiers.none, Absent))

    "a boundary's sink reaches a click handler served over HTTP".notNative in {
        for
            seen <- Promise.init[String, Any]
            app = UI.div(UI.boundary.onNotice(t => seen.completeDiscard(Result.succeed(t.getMessage)))(UI.div(
                UI.button("go").onClick(Abort.fail(new RuntimeException("click failed")))
            )))
            got <- Scope.run {
                for
                    handlers <- UI.runHandlers("/")(app)
                    server   <- HttpServer.init(0, "127.0.0.1")(handlers*)
                    got      <- HttpClient.webSocket(s"ws://127.0.0.1:${server.port}/_kyo/ws") { ws =>
                        for
                            // The session announces itself; a page with no reactive region sends no render after that.
                            _   <- ws.take()
                            _   <- ws.put(HttpWebSocket.Payload.Text(Json.encode[UIEvent](click)))
                            got <- Abort.run[Timeout](Async.timeout(5.seconds)(seen.get)).map(_.toMaybe)
                        yield got
                    }
                yield got
            }
        yield assert(got == Present("click failed"), s"the sink received: $got")
    }

end HandlerNoticeReachTest
