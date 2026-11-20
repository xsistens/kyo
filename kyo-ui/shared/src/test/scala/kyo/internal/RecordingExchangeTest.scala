package kyo.internal

import kyo.*
import kyo.internal.RecordingExchange.Op
import kyo.internal.RecordingExchange.Sink

class RecordingExchangeTest extends kyo.test.Test[Any]:

    import UI.*

    private val context = ReactiveRegion.RegionIdentity(Seq.empty, 0)

    private def paint(rec: RecordingExchange, path: String*)(using Frame): Unit < Async =
        rec.onChange(ReactiveRegion.HtmlRange("r"), path, context, ReactiveRegion.ParentContext.Other, Absent, UI.empty)

    "keeps every channel patch beside the paints, in the order it arrived" in {
        val rec = new RecordingExchange
        for
            _   <- rec.onAttrPatch(Seq("a"), "title", "x")
            _   <- paint(rec, "b")
            _   <- rec.onClassPatch(Seq("c"), "hot", true)
            _   <- rec.onBoolAttrPatch(Seq("d"), "disabled", false)
            _   <- rec.onStylePatch(Seq("e"), Seq("color" -> "red"), Seq("width"))
            ops <- rec.ops
            cls <- rec.classPatches
        yield
            assert(ops == Chunk[Op](
                Op.Attr(Seq("a"), "title", "x", Sink.Fiber),
                Op.Paint(Seq("b"), Absent, UI.empty),
                Op.Class(Seq("c"), "hot", true, Sink.Fiber),
                Op.BoolAttr(Seq("d"), "disabled", false, Sink.Fiber),
                Op.Style(Seq("e"), Seq("color" -> "red"), Seq("width"), Sink.Fiber)
            ))
            assert(cls == Chunk(Op.Class(Seq("c"), "hot", true, Sink.Fiber)))
        end for
    }

    "without the synchronous sinks a channel has nothing to patch on the writer's stack" in {
        val rec = new RecordingExchange
        assert(rec.attrPatcherNow.isEmpty && rec.boolAttrPatcherNow.isEmpty && rec.classPatcherNow.isEmpty && rec.stylePatcherNow.isEmpty)
    }

    "the synchronous sinks record inside the call and are labelled apart from the fiber ones" in {
        val rec = new RecordingExchange(synchronous = true)
        rec.attrPatcherNow.foreach(_(Seq("a"), "title", "x"))
        rec.boolAttrPatcherNow.foreach(_(Seq("b"), "disabled", true))
        rec.classPatcherNow.foreach(_(Seq("c"), "hot", false))
        rec.stylePatcherNow.foreach(_(Seq("d"), Seq("color" -> "red"), Seq.empty))
        assert(rec.recorded == Chunk[Op](
            Op.Attr(Seq("a"), "title", "x", Sink.Now),
            Op.BoolAttr(Seq("b"), "disabled", true, Sink.Now),
            Op.Class(Seq("c"), "hot", false, Sink.Now),
            Op.Style(Seq("d"), Seq("color" -> "red"), Seq.empty, Sink.Now)
        ))
    }

    "keeps every paint in the order it arrived" in {
        val rec = new RecordingExchange
        for
            _   <- paint(rec, "a")
            _   <- paint(rec, "b")
            _   <- paint(rec, "a", "0")
            ops <- rec.ops
        yield
            assert(ops == Chunk[Op](
                Op.Paint(Seq("a"), Absent, UI.empty),
                Op.Paint(Seq("b"), Absent, UI.empty),
                Op.Paint(Seq("a", "0"), Absent, UI.empty)
            ))
            assert(rec.recorded == ops)
        end for
    }

    "paints answers with the paints, oldest first" in {
        val rec = new RecordingExchange
        for
            _     <- paint(rec, "b")
            _     <- paint(rec, "c")
            shown <- rec.paints
        yield assert(shown.map(_.path) == Chunk(Seq("b"), Seq("c")))
        end for
    }

    "awaitOp is released by an op already recorded" in {
        val rec = new RecordingExchange
        for
            _ <- paint(rec, "a")
            _ <- rec.awaitOp { case Op.Paint(path, _, _) => path == Seq("a"); case _ => false }
        yield succeed
        end for
    }

    "awaitOp is released by a later op, and only by one its predicate takes" in {
        val rec = new RecordingExchange
        for
            waiting <- Fiber.init(rec.awaitOp { case Op.Paint(path, _, _) => path == Seq("wanted"); case _ => false })
            _       <- paint(rec, "other")
            early   <- waiting.done
            _       <- paint(rec, "wanted")
            _       <- waiting.get
        yield assert(!early)
        end for
    }

    "beforePaint suspends a paint before it is recorded" in {
        for
            entered <- Promise.init[Unit, Any]
            release <- Promise.init[Unit, Any]
            rec = new RecordingExchange(beforePaint = (_, _) => entered.completeUnitDiscard.andThen(release.get))
            painting <- Fiber.init(paint(rec, "p"))
            _        <- entered.get
            during   <- rec.paints
            _        <- release.completeUnitDiscard
            _        <- painting.get
            after    <- rec.paints
        yield
            assert(during.isEmpty)
            assert(after.map(_.path) == Chunk(Seq("p")))
        end for
    }

    "subscribed hands over the subscription of the tree it runs" in {
        val rec = new RecordingExchange
        for
            ref <- Signal.initRef("a")
            ui = UI.div(ref.render(v => UI.span(v)))
            _ <- ReactiveUI.subscribed(ui, rec) { _ =>
                ref.set("b").andThen(rec.awaitOp { case Op.Paint(_, _, shown) => shown == UI.span("b"); case _ => false })
            }
            paints <- rec.paints
        yield assert(paints.map(_.path).distinct == Chunk(Seq("0")))
        end for
    }

end RecordingExchangeTest
