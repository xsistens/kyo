package kyo.internal

import kyo.*
import kyo.internal.RecordingExchange.Op
import kyo.internal.RecordingExchange.Sink

class RecordingExchangeTest extends kyo.test.Test[Any]:

    private val context = ReactiveRegion.RegionIdentity(Seq.empty, 0)

    private def paint(rec: RecordingExchange, path: String*)(using Frame): Unit < Async =
        rec.onChange(ReactiveRegion.HtmlRange("r"), path, context, ReactiveRegion.ParentContext.Other, Absent, UI.empty)

    "keeps every op in the order it arrived" in {
        val rec = new RecordingExchange
        for
            _   <- rec.onAttrPatch(Seq("a"), "title", "x")
            _   <- paint(rec, "b")
            _   <- rec.onClassPatch(Seq("c"), "hot", true)
            _   <- rec.onBoolAttrPatch(Seq("d"), "disabled", false)
            _   <- rec.onStylePatch(Seq("e"), Seq("color" -> "red"), Seq("width"))
            _   <- rec.onHold(ReactiveRegion.HtmlRange("r"), true)
            _   <- rec.onFreshInstance(ReactiveRegion.HtmlRange("r"))
            ops <- rec.ops
        yield
            assert(ops == Chunk[Op](
                Op.Attr(Seq("a"), "title", "x", Sink.Fiber),
                Op.Paint(Seq("b"), Absent, UI.empty),
                Op.Class(Seq("c"), "hot", true, Sink.Fiber),
                Op.BoolAttr(Seq("d"), "disabled", false, Sink.Fiber),
                Op.Style(Seq("e"), Seq("color" -> "red"), Seq("width"), Sink.Fiber),
                Op.Hold(ReactiveRegion.HtmlRange("r"), true),
                Op.FreshInstance(ReactiveRegion.HtmlRange("r"))
            ))
            assert(rec.recorded == ops)
        end for
    }

    "the queries answer with their own kind of op, oldest first" in {
        val rec = new RecordingExchange
        for
            _     <- rec.onAttrPatch(Seq("a"), "title", "x")
            _     <- paint(rec, "b")
            _     <- rec.onAttrPatch(Seq("a"), "title", "y")
            attrs <- rec.attrPatches
            shown <- rec.paints
        yield
            assert(attrs.map(_.value) == Chunk("x", "y"))
            assert(shown.map(_.path) == Chunk(Seq("b")))
        end for
    }

    "without the synchronous sinks a channel has nothing to patch on the writer's stack" in {
        val rec = new RecordingExchange
        assert(rec.attrPatcherNow.isEmpty && rec.boolAttrPatcherNow.isEmpty && rec.classPatcherNow.isEmpty)
        assert(rec.stylePatcherNow.isEmpty && rec.textPatcherNow.isEmpty)
    }

    "the synchronous sinks record inside the call and are labelled apart from the fiber ones" in {
        val rec = new RecordingExchange(synchronous = true)
        rec.attrPatcherNow.foreach(_(Seq("a"), "title", "x"))
        rec.boolAttrPatcherNow.foreach(_(Seq("b"), "disabled", true))
        rec.classPatcherNow.foreach(_(Seq("c"), "hot", false))
        rec.stylePatcherNow.foreach(_(Seq("d"), Seq("color" -> "red"), Seq.empty))
        rec.textPatcherNow.foreach(_(Seq("e"), "txt"))
        assert(rec.recorded == Chunk[Op](
            Op.Attr(Seq("a"), "title", "x", Sink.Now),
            Op.BoolAttr(Seq("b"), "disabled", true, Sink.Now),
            Op.Class(Seq("c"), "hot", false, Sink.Now),
            Op.Style(Seq("d"), Seq("color" -> "red"), Seq.empty, Sink.Now),
            Op.Text(Seq("e"), "txt")
        ))
    }

    "a tree's list patch keeps its rows only when asked" in {
        val rows                                       = Seq(ListRow("a", UI.empty, true))
        def patch(rec: RecordingExchange)(using Frame) =
            rec.onListPatch(ReactiveRegion.HtmlRange("r"), Seq("l"), context, ReactiveRegion.ParentContext.Other, Absent, rows)
        val keeping = new RecordingExchange(keepsRows = true)
        val whole   = new RecordingExchange
        for
            kept    <- patch(keeping)
            lists   <- keeping.listPatches
            repaint <- patch(whole)
            paints  <- whole.paints
            none    <- whole.listPatches
        yield
            assert(kept && lists.map(_.rows.map(_.key)) == Chunk(Seq("a")))
            assert(!repaint && paints.map(_.path) == Chunk(Seq("l")) && none.isEmpty)
        end for
    }

    "awaitOp is released by an op already recorded" in {
        val rec = new RecordingExchange
        for
            _ <- rec.onAttrPatch(Seq("a"), "title", "x")
            _ <- rec.awaitOp { case Op.Attr(_, _, value, _) => value == "x"; case _ => false }
        yield succeed
        end for
    }

    "awaitOp is released by a later op, and only by one its predicate takes" in {
        val rec = new RecordingExchange
        for
            waiting <- Fiber.init(rec.awaitOp { case Op.Attr(_, _, value, _) => value == "wanted"; case _ => false })
            _       <- rec.onAttrPatch(Seq("a"), "title", "other")
            early   <- waiting.done
            _       <- rec.onAttrPatch(Seq("a"), "title", "wanted")
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

    "subscribed hands over the subscription with the session's commands installed" in {
        val rec = new RecordingExchange
        for
            ref <- Signal.initRef("a")
            ui = UI.div(UI.span("x").title(ref: Signal[String]))
            found <- ReactiveUI.subscribed(ui, rec) { _ =>
                UICommands.commands.use(c => ref.set("b").andThen(rec.awaitOp(_ => true)).andThen(c.nonEmpty))
            }
            patches <- rec.attrPatches
        yield
            assert(found)
            assert(patches.map(_.value) == Chunk("b"))
        end for
    }

end RecordingExchangeTest
