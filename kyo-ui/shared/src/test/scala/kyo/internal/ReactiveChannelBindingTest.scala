package kyo.internal

import kyo.*
import kyo.internal.RecordingExchange.Op
import kyo.internal.RecordingExchange.Sink

/** The callback binding of the reactive channels (`ReactiveUI.bindChannel`) for attribute, boolean attribute and
  * class channels.
  *
  * Where the exchange offers a synchronous sink, a channel binds as a callback on its signal instead of a fiber.
  * Three things have to hold for each channel:
  *
  *   - the patch lands inside the writer's own `set`;
  *   - closing the owning Scope releases the registration, which sits on a masked promise that nothing
  *     interrupts, so a missing release retains the callback and the subtree behind it;
  *   - an exchange without a synchronous sink (the server transport) keeps the fiber path.
  *
  * The three channels share one binding, so they are asserted together.
  */
class ReactiveChannelBindingTest extends kyo.test.Test[Any]:

    /** Counts the two sinks separately, so a test can tell which path ran. `fast = false` is an exchange
      * without a synchronous sink.
      */
    final private class Recording(fast: Boolean):
        val exchange                    = new RecordingExchange(synchronous = fast)
        private def by(sink: Sink): Int = exchange.recorded.count {
            case op: Op.Attr     => op.sink == sink
            case op: Op.BoolAttr => op.sink == sink
            case op: Op.Class    => op.sink == sink
            case _               => false
        }
        def sync: Int  = by(Sink.Now)
        def async: Int = by(Sink.Fiber)
    end Recording

    /** One channel under test, built outside any scope the test opens so that `move` and `waiters` outlive it: a
      * release test that can only reach its signal from inside the scope proves nothing.
      *
      * The rendered snapshot equals each signal's initial value, so the initial emission is skipped on either
      * path and every count below describes a real change.
      *
      * @param move
      *   changes the channel's signal once
      * @param waiters
      *   registrations currently parked on that signal
      */
    final private case class Channel(name: String, ui: UI, move: Unit < Async, waiters: Int < Sync)

    private def attrChannel(using Frame): Channel < Sync =
        Signal.initRef("a").map(ref =>
            Channel("attr", UI.div(UI.span("x").title(ref: Signal[String])), ref.set("b").unit, ref.waiters)
        )

    private def boolChannel(using Frame): Channel < Sync =
        Signal.initRef(false).map(ref =>
            Channel("bool-attr", UI.div(UI.input.disabled(ref: Signal[Boolean])), ref.set(true).unit, ref.waiters)
        )

    private def classChannel(using Frame): Channel < Sync =
        Signal.initRef(false).map(ref =>
            Channel("class", UI.div(UI.span("x").when(ref: Signal[Boolean])(_.cssClass("hot"))), ref.set(true).unit, ref.waiters)
        )

    /** A class whose name the signal holds; moving from no class to one is one patch, as for the others. */
    private def classNameChannel(using Frame): Channel < Sync =
        Signal.initRef[Maybe[String]](Absent).map(ref =>
            Channel(
                "class-name",
                UI.div(UI.span("x").cssClass.matching(ref: Signal[Maybe[String]])(identity)),
                ref.set(Present("hot")).unit,
                ref.waiters
            )
        )

    /** An attribute a signal of a `Maybe` may leave out: `Absent` is no attribute, so moving it to a value is one attribute patch, as
      * for the others.
      */
    private def ariaFlagChannel(using Frame): Channel < Sync =
        Signal.initRef[Maybe[String]](Absent).map(ref =>
            Channel(
                "aria-flag",
                UI.div(UI.span("x").aria("disabled" -> (ref: Signal[Maybe[String]]))),
                ref.set(Present("true")).unit,
                ref.waiters
            )
        )

    /** A silent flag on an element with a handler: moving it on takes `data-kyo-ev` away, one patch, as for the others. */
    private def silentChannel(using Frame): Channel < Sync =
        Signal.initRef(false).map(ref =>
            Channel("silent", UI.div(UI.span("x").onClick(Kyo.unit).silent(ref: Signal[Boolean])), ref.set(true).unit, ref.waiters)
        )

    private def channels(using Frame): Seq[Channel] < Sync =
        Kyo.foreach(Chunk(attrChannel, boolChannel, classChannel, classNameChannel, ariaFlagChannel, silentChannel))(identity).map(_.toSeq)

    /** The patches of `data-kyo-ev` an exchange recorded, as `set <events>` and `off`, the `disabled` patches, and the paints. */
    final private class EventPatches(fast: Boolean):
        val exchange              = new RecordingExchange(synchronous = fast)
        def events: Chunk[String] = exchange.recorded.collect {
            case op: Op.Attr if op.name == "data-kyo-ev"     => s"set ${op.value}"
            case op: Op.BoolAttr if op.name == "data-kyo-ev" => if op.value then "on" else "off"
        }
        def disabled: Chunk[Boolean] = exchange.recorded.collect { case op: Op.BoolAttr if op.name == "disabled" => op.value }
        def drawn: Int               = exchange.recorded.count { case _: Op.Paint => true; case _ => false }
    end EventPatches

    "silent of a signal takes data-kyo-ev away while on and puts it back while off, in place, with nothing drawn again" in {
        Scope.run {
            Kyo.foreachDiscard(Chunk(true, false)) { fast =>
                val rec = new EventPatches(fast)
                for
                    muted <- Signal.initRef(false)
                    root  <- ReactiveUI.normalize(UI.div(UI.span("x").onClick(Kyo.unit).silent(muted: Signal[Boolean])), Seq.empty)
                    _     <- ReactiveUI.subscribe(root, rec.exchange)
                    _     <- muted.set(true)
                    _     <- assertEventually(Sync.defer(rec.events.size == 1))
                    _     <- muted.set(false)
                    _     <- assertEventually(Sync.defer(rec.events.size == 2))
                yield assert(rec.events == Chunk("off", "set click") && rec.drawn == 0, s"fast=$fast: ${rec.events}, drawn ${rec.drawn}")
                end for
            }
        }
    }

    "disabled of a signal silences the element too, and with silent of a signal it is silent while either holds" in {
        Scope.run {
            Kyo.foreachDiscard(Chunk(true, false)) { fast =>
                val rec = new EventPatches(fast)
                for
                    off   <- Signal.initRef(false)
                    muted <- Signal.initRef(false)
                    ui = UI.div(UI.button("Go").onClick(Kyo.unit).disabled(off: Signal[Boolean]).silent(muted: Signal[Boolean]))
                    root <- ReactiveUI.normalize(ui, Seq.empty)
                    _    <- ReactiveUI.subscribe(root, rec.exchange)
                    _    <- off.set(true)
                    _    <- assertEventually(Sync.defer(rec.events.size == 1 && rec.disabled.size == 1))
                    // Silent through each step until both are off: no data-kyo-ev patch in between.
                    _ <- muted.set(true)
                    _ <- off.set(false)
                    _ <- assertEventually(Sync.defer(rec.disabled.size == 2))
                    _ <- muted.set(false)
                    _ <- assertEventually(Sync.defer(rec.events.size == 2))
                yield assert(
                    rec.events == Chunk("off", "set click") && rec.disabled == Chunk(true, false) && rec.drawn == 0,
                    s"fast=$fast: ${rec.events}, disabled ${rec.disabled}, drawn ${rec.drawn}"
                )
                end for
            }
        }
    }

    "silent of a signal on a dropdown takes its wrapper's data-kyo-ev away and puts it back, in place" in {
        Scope.run {
            Kyo.foreachDiscard(Chunk(true, false)) { fast =>
                val rec = new EventPatches(fast)
                for
                    muted <- Signal.initRef(false)
                    root  <- ReactiveUI.normalize(UI.div(UI.dropdown("A" -> "a").id("d").silent(muted: Signal[Boolean])), Seq.empty)
                    _     <- ReactiveUI.subscribe(root, rec.exchange)
                    _     <- muted.set(true)
                    _     <- assertEventually(Sync.defer(rec.events.size == 1))
                    _     <- muted.set(false)
                    _     <- assertEventually(Sync.defer(rec.events.size == 2))
                yield assert(
                    rec.events == Chunk("off", "set click,keydown,change") && rec.drawn == 0,
                    s"fast=$fast: ${rec.events}, drawn ${rec.drawn}"
                )
                end for
            }
        }
    }

    "a signal of a Maybe writes its value while Present and takes the attribute away while Absent, in place" in {
        Scope.run {
            Kyo.foreachDiscard(Chunk(true, false)) { fast =>
                val exchange = new RecordingExchange(synchronous = fast)
                def ops      = exchange.recorded.collect {
                    case op: Op.Attr     => s"set ${op.name}=${op.value}"
                    case op: Op.BoolAttr => s"${if op.value then "on" else "off"} ${op.name}"
                }
                for
                    on   <- Signal.initRef[Maybe[String]](Present("true"))
                    root <- ReactiveUI.normalize(UI.div(UI.span("x").aria("disabled" -> (on: Signal[Maybe[String]]))), Seq.empty)
                    _    <- ReactiveUI.subscribe(root, exchange)
                    _    <- on.set(Absent)
                    _    <- assertEventually(Sync.defer(ops.size == 1))
                    _    <- on.set(Present("true"))
                    _    <- assertEventually(Sync.defer(ops.size == 2))
                yield assert(ops == Seq("off aria-disabled", "set aria-disabled=true"), s"fast=$fast: $ops")
                end for
            }
        }
    }

    "a class-name channel takes the class it held off and puts the one it holds on, in place" in {
        Scope.run {
            Kyo.foreachDiscard(Chunk(true, false)) { fast =>
                val exchange = new RecordingExchange(synchronous = fast)
                def classes  = exchange.recorded.collect { case op: Op.Class => (op.name, op.on) }
                for
                    size <- Signal.initRef[Maybe[String]](Present("p-sm"))
                    root <- ReactiveUI.normalize(UI.div(UI.span("x").cssClass.matching(size: Signal[Maybe[String]])(identity)), Seq.empty)
                    _    <- ReactiveUI.subscribe(root, exchange)
                    _    <- size.set(Present("p-lg"))
                    _    <- assertEventually(Sync.defer(classes.size == 2))
                    _    <- size.set(Absent)
                    _    <- assertEventually(Sync.defer(classes.size == 3))
                yield assert(
                    classes == Seq("p-sm" -> false, "p-lg" -> true, "p-lg" -> false),
                    s"fast=$fast: $classes"
                )
                end for
            }
        }
    }

    "every channel patches inside the writer's set, with no scheduler hop" in {
        Scope.run {
            channels.map(Kyo.foreachDiscard(_) { ch =>
                val rec = new Recording(fast = true)
                for
                    root <- ReactiveUI.normalize(ch.ui, Seq.empty)
                    _    <- ReactiveUI.subscribe(root, rec.exchange)
                    before = rec.sync
                    _ <- ch.move
                    // On the fiber path the patch would still be queued here.
                    after = rec.sync
                yield assert(before == 0 && after == 1 && rec.async == 0, s"${ch.name}: before=$before after=$after")
                end for
            })
        }
    }

    "closing the scope releases every channel's registration" in {
        channels.map(Kyo.foreachDiscard(_) { ch =>
            val rec = new Recording(fast = true)
            for
                _      <- Scope.run(ReactiveUI.normalize(ch.ui, Seq.empty).map(ReactiveUI.subscribe(_, rec.exchange).unit))
                parked <- ch.waiters
                // The signal moves after the scope closed: a binding that forgot to release would still patch
                // a subtree that is gone.
                _ <- ch.move
            yield assert(parked == 0 && rec.sync == 0, s"${ch.name}: parked=$parked patches=${rec.sync}")
            end for
        })
    }

    "an exchange without synchronous sinks keeps every channel on the fiber path" in {
        Scope.run {
            channels.map(Kyo.foreachDiscard(_) { ch =>
                val rec = new Recording(fast = false)
                for
                    root <- ReactiveUI.normalize(ch.ui, Seq.empty)
                    _    <- ReactiveUI.subscribe(root, rec.exchange)
                    _    <- ch.move
                    _    <- assertEventually(Sync.defer(rec.async == 1))
                yield assert(rec.sync == 0, s"${ch.name} took the fast path with no sink offered")
                end for
            })
        }
    }

    /** Records the last value each attribute channel patched, by path and name. */
    final private class LastValues:
        val exchange                                           = new RecordingExchange
        def at(path: Seq[String], name: String): Maybe[String] =
            exchange.recorded.foldLeft(Maybe.empty[String]) {
                case (_, op: Op.Attr) if op.path == path && op.name == name => Present(op.value)
                case (last, _)                                              => last
            }
    end LastValues

    "a channel on the root a region painted first still patches" in {
        // The region's first value is the one already on the page, so the region subscribes it without
        // repainting; the root's own channel has to start there too.
        Scope.run {
            val rec = new LastValues
            for
                gate  <- Signal.initRef(true)
                label <- Signal.initRef("a")
                root  <- ReactiveUI.normalize(UI.div(gate.map(_ => UI.span("x").title(label: Signal[String]))), Seq.empty)
                _     <- ReactiveUI.subscribe(root, rec.exchange)
                _     <- label.set("b")
                _     <- assertEventually(Sync.defer(rec.at(Seq("0"), "title") == Present("b")))
            yield ()
            end for
        }
    }

    "a channel on the root of a mounted node's content patches" in {
        Scope.run {
            val rec = new LastValues
            for
                label <- Signal.initRef("a")
                root  <- ReactiveUI.normalize(UI.div(UI.mounted(UI.span("x").title(label: Signal[String]))), Seq.empty)
                _     <- ReactiveUI.subscribe(root, rec.exchange)
                _     <- label.set("b")
                _     <- assertEventually(Sync.defer(rec.at(Seq("0"), "title") == Present("b")))
            yield ()
            end for
        }
    }

    "a failing sink is logged to the Log bound where the channel subscribed, and the writer's set completes" in {
        val exchange = new UIExchange:
            private def boom = throw new RuntimeException("boom")
            def onChange(
                region: ReactiveRegion,
                path: Seq[String],
                contentContext: ReactiveRegion.RegionIdentity,
                parentContext: ReactiveRegion.ParentContext,
                previous: Maybe[UI],
                changed: UI
            )(using Frame): Unit < Async                                                   = Kyo.unit
            override val attrPatcherNow: Maybe[(Seq[String], String, String) => Unit]      = Present((_, _, _) => boom)
            override val boolAttrPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit] = Present((_, _, _) => boom)
            override val classPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit]    = Present((_, _, _) => boom)
        Scope.run {
            channels.map(Kyo.foreachDiscard(_) { ch =>
                val logged = new UIScreen.Warnings
                for
                    root <- ReactiveUI.normalize(ch.ui, Seq.empty)
                    _    <- Log.let(logged.safe)(ReactiveUI.subscribe(root, exchange))
                    // Outside the binding: the writer's stack cannot name the Log, only the subscription could.
                    _ <- ch.move
                yield assert(logged.errors.exists(_.contains("channel binding failed")), s"${ch.name}: the bound Log got: ${logged.errors}")
                end for
            })
        }
    }

end ReactiveChannelBindingTest
