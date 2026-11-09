package kyo.internal

import java.util.concurrent.atomic.AtomicInteger
import kyo.*

/** Which handlers `EventDispatch` runs for an element that is silent (`silent`, or `disabled` where the element has it) when the event
  * arrives.
  *
  * A silent element runs none of its handlers, as the target or as an element the event bubbles through, and the event goes on to its
  * ancestors as if the element declared nothing. The events are sent to the dispatcher directly, as a forged event or one sent before the
  * patch that silenced the element reached the page would arrive: the client's own forwarding gate is not in the way.
  */
class EventDispatchTest extends kyo.test.Test[Any]:

    private def click(path: String*): UIEvent = UIEvent.Click(path, MouseEventData(UI.Modifiers.none, Absent))

    private def counter(using Frame): AtomicInteger < Sync = Sync.defer(new AtomicInteger(0))

    private def bump(c: AtomicInteger)(using Frame): Unit < Sync = Sync.defer(discard(c.incrementAndGet()))

    "a silent target runs none of its handlers and the click goes on to its parent; one that is not silent runs its own" in {
        for
            parent <- counter
            quiet  <- counter
            heard  <- counter
            ui = UI.div(UI.div.onClick(bump(parent))(
                UI.button("q").onClick(bump(quiet)).silent(true),
                UI.button("h").onClick(bump(heard)).silent(false)
            ))
            result <- ReactiveUI.subscribed(ui) { sub =>
                for
                    _ <- sub.handle(Seq("0", "0"), click("0", "0"))
                    _ <- sub.handle(Seq("0", "1"), click("0", "1"))
                    _ <- assertEventually(Sync.defer(parent.get == 2))
                yield (quiet.get, heard.get, parent.get)
            }
        yield assert(result == (0, 1, 2), s"(silent, heard, parent) = $result")
        end for
    }

    "silent of a signal is read when the event arrives, so an event sent before the patch reached the page reaches nothing" in {
        for
            parent <- counter
            button <- counter
            muted  <- Signal.initRef(false)
            ui = UI.div(UI.div.onClick(bump(parent))(UI.button("b").onClick(bump(button)).silent(muted: Signal[Boolean])))
            seen <- ReactiveUI.subscribed(ui) { sub =>
                def clickAndCount(clicks: Int): Int < Async =
                    sub.handle(Seq("0", "0"), click("0", "0"))
                        .andThen(assertEventually(Sync.defer(parent.get == clicks)))
                        .andThen(Sync.defer(button.get))
                for
                    heard  <- clickAndCount(1)
                    _      <- muted.set(true)
                    quiet  <- clickAndCount(2)
                    _      <- muted.set(false)
                    heard2 <- clickAndCount(3)
                yield Seq(heard, quiet, heard2)
                end for
            }
        yield assert(seen == Seq(1, 1, 2), s"button clicks after each click: $seen")
        end for
    }

    "a silent element the event bubbles through runs none of its handlers, and its stopPropagation stops nothing" in {
        for
            outer  <- counter
            middle <- counter
            inner  <- counter
            ui = UI.div(UI.div.onClick(bump(outer))(
                UI.div.onClick(bump(middle)).stopPropagation(true).silent(true)(UI.span("x").onClick(bump(inner)))
            ))
            result <- ReactiveUI.subscribed(ui) { sub =>
                sub.handle(Seq("0", "0", "0"), click("0", "0", "0"))
                    .andThen(assertEventually(Sync.defer(outer.get == 1)))
                    .andThen(Sync.defer((inner.get, middle.get, outer.get)))
            }
        yield assert(result == (1, 0, 1), s"(inner, silent middle, outer) = $result")
        end for
    }

    "a disabled element the event bubbles through runs none of its handlers, constant or from a signal" in {
        for
            outer   <- counter
            fixed   <- counter
            follows <- counter
            inner   <- counter
            off     <- Signal.initRef(true)
            ui = UI.div(UI.div.onClick(bump(outer))(
                UI.fieldset.disabled(true).onClick(bump(fixed))(UI.span("a").onClick(bump(inner))),
                UI.fieldset.disabled(off: Signal[Boolean]).onClick(bump(follows))(UI.span("b").onClick(bump(inner)))
            ))
            result <- ReactiveUI.subscribed(ui) { sub =>
                for
                    _ <- sub.handle(Seq("0", "0", "0"), click("0", "0", "0"))
                    _ <- sub.handle(Seq("0", "1", "0"), click("0", "1", "0"))
                    _ <- assertEventually(Sync.defer(outer.get == 2))
                    _ <- off.set(false)
                    _ <- sub.handle(Seq("0", "1", "0"), click("0", "1", "0"))
                    _ <- assertEventually(Sync.defer(outer.get == 3))
                yield (inner.get, fixed.get, follows.get, outer.get)
            }
        yield assert(result == (3, 0, 1, 3), s"(inner, disabled, disabled by a signal then enabled, outer) = $result")
        end for
    }

    "a silent dropdown takes no option pick, and one that is not silent does" in {
        for
            picks <- counter
            quiet <- Signal.initRef("a")
            heard <- Signal.initRef("a")
            ui = UI.div(
                UI.dropdown("A" -> "a", "B" -> "b").value(quiet).onChange(_ => bump(picks)).silent(true),
                UI.dropdown("A" -> "a", "B" -> "b").value(heard).onChange(_ => bump(picks))
            )
            result <- ReactiveUI.subscribed(ui) { sub =>
                for
                    _     <- sub.handle(Seq("0"), UIEvent.Change(Seq("0"), "b"))
                    _     <- sub.handle(Seq("1"), UIEvent.Change(Seq("1"), "b"))
                    _     <- assertEventually(heard.get.map(_ == "b"))
                    held  <- quiet.get
                    taken <- heard.get
                yield (held, taken, picks.get)
            }
        yield assert(result == ("a", "b", 1), s"(silent value, heard value, onChange calls) = $result")
        end for
    }

    "a silent element still starts its onAttached hook" in {
        for
            attached <- counter
            site     <- Scope.Finalizer.init(1)
            span = UI.span("x").onClick(Kyo.unit).silent(true)
            ui   = UI.div(span.withAttrs(span.attrs.copy(onAttached = Present(_ => bump(attached)))))
            result <- ReactiveUI.subscribed(ui) { sub =>
                // The browser mount dispatches an Attach with the element it found and that element's scope set, as here.
                AttachSite.local.let(Present(AttachSite.Site("the element", site)))(sub.handle(Seq("0"), UIEvent.Attach(Seq("0"))))
                    .andThen(assertEventually(Sync.defer(attached.get == 1)))
                    .andThen(Sync.defer(attached.get))
            }
            _ <- site.close(Absent)
        yield assert(result == 1, s"attach hooks started: $result")
        end for
    }

end EventDispatchTest
