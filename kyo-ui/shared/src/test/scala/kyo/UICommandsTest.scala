package kyo

import kyo.internal.HtmlOp

/** [[kyo.UI.Commands]] on its own: the session channel driven directly, with this test standing in
  * for the client that answers it.
  *
  * The transport is not involved. What is under test is the bookkeeping between the two ways an
  * element's geometry is asked for, the continuous [[kyo.UI.Commands.observeViewportById]] stream and
  * the one-shot [[kyo.UI.Commands.requestMeasureByIds]] round trip, which the browser-driven measure
  * suites exercise one at a time and never against the same element.
  */
class UICommandsTest extends UITest:

    private val rect = UI.Rect(0.0, 120.0, 200.0, 40.0, 1280.0, 800.0)

    "a measure request for an element that is also observed comes back" in {
        for
            asked <- Promise.init[Unit, Any]
            cmds  <- UI.Commands.init {
                case HtmlOp.RequestMeasureById(_) => asked.completeUnitDiscard
                case _                            => ()
            }
            result <- Scope.run {
                for
                    live    <- cmds.observeViewportById("box")
                    pending <- Fiber.init(cmds.requestMeasureByIds(Seq("box")))
                    // The request has registered its promise by the time it emits its op, so the
                    // reply below cannot race ahead of it.
                    _        <- asked.get
                    observed <- cmds.observers.get
                    awaited  <- cmds.pendingById.get
                    _        <- cmds.deliverMeasureById("box", rect)
                    rects    <- pending.get
                    seen     <- live.currentWith(v => v)
                yield (observed.contains("box") && awaited.contains("box"), rects, seen)
            }
        yield
            val (both, rects, seen) = result
            assert(both, "the element is in both maps when the reply arrives, which is the case at issue")
            assert(rects == Chunk(Present(rect)), "the one-shot request is answered")
            assert(seen == Present(rect), "and the observation carries the same measurement")
    }

    "a measure of an id no element carries answers Absent, and a batch keeps the positions it could measure" in {
        for
            asked <- Promise.init[Unit, Any]
            cmds  <- UI.Commands.init {
                case HtmlOp.RequestMeasureById("gone") => asked.completeUnitDiscard
                case _                                 => ()
            }
            single <- Fiber.init(cmds.requestMeasureById("gone"))
            _      <- asked.get
            _      <- cmds.deliverMeasureMissing("gone")
            alone  <- single.get
            batch  <- Fiber.init(cmds.requestMeasureByIds(Seq("box", "gone")))
            _      <- assertEventually(cmds.pendingById.get.map(_.size == 2))
            _      <- cmds.deliverMeasureById("box", rect)
            _      <- cmds.deliverMeasureMissing("gone")
            both   <- batch.get
        yield
            assert(alone == Absent)
            assert(both == Chunk(Present(rect), Absent))
    }

end UICommandsTest
