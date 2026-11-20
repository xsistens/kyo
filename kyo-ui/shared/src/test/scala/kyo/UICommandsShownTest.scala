package kyo

import kyo.internal.HtmlOp
import kyo.internal.HtmlRenderer
import kyo.internal.ReactiveUI
import kyo.internal.UICommands

/** [[kyo.UI.Commands]] from inside a mount: a command waits for the paint of the instance's own content, and is dropped with it when
  * it never shows.
  *
  * The screen applies every paint the way the client does, so a command logged with `true` found its element on the page.
  */
class UICommandsShownTest extends UIScreen:

    import UI.*

    private val rect = UI.Rect(0.0, 0.0, 10.0, 10.0, 100.0, 100.0)

    /** Mounts `ui` with a session channel whose commands are logged as `verb:id:whether the element is on the page`; a measure is
      * answered the way the client answers it, with a rect for an element on the page and `Absent` for one that is not.
      */
    private def mountLogged[A](ui: UI)(f: (Screen, AtomicRef[Chunk[String]]) => A < (Async & Scope))(using Frame): A < Async =
        Scope.run {
            for
                initial <- HtmlRenderer.render(ui, Seq.empty)
                screen = new Screen(initial)
                log     <- AtomicRef.init(Chunk.empty[String])
                channel <- AtomicRef.init(Maybe.empty[UI.Commands])
                note = (verb: String, id: String) =>
                    screen.html.map { page =>
                        val there = page.contains(s"id=\"$id\"")
                        log.getAndUpdate(_.append(s"$verb:$id:$there")).andThen(there)
                    }
                cmds <- UI.Commands.init {
                    case HtmlOp.CommandById(id, verb)  => note(verb, id).unit
                    case HtmlOp.RequestMeasureById(id) =>
                        note("measure", id).map { there =>
                            channel.get.map(_.fold(Kyo.unit)(c =>
                                if there then c.deliverMeasureById(id, rect) else c.deliverMeasureMissing(id)
                            ))
                        }
                    case _ => Kyo.unit
                }
                _    <- channel.set(Present(cmds))
                root <- ReactiveUI.normalize(ui, Seq.empty)
                _    <- UICommands.commands.let(Present(cmds))(ReactiveUI.subscribe(root, screen))
                a    <- f(screen, log)
            yield a
        }

    /** A field that focuses itself from the effect that builds it, before the field is on any page. */
    private def selfFocusing(id: String)(using Frame): UI.Ast.Mounted =
        UI.mounted(UI.commands.map(_.focusId(id)).andThen(input.id(id)))

    "a command a mount's effect sends about its own content goes out once that content is on the page" in {
        mountLogged(div(selfFocusing("own"))) { (_, log) =>
            assertEventually(log.get.map(_.nonEmpty)).andThen(log.get.map(seen => assert(seen == Chunk("focus:own:true"), seen.toString)))
        }
    }

    "a command whose content never shows is dropped, and a measure waiting there answers Absent" in {
        for
            gate     <- Promise.init[Unit, Any]
            show     <- Signal.initRef(true)
            measured <- Promise.init[Maybe[UI.Rect], Any]
            asking   <- Promise.init[Unit, Any]
            held = UI.mounted(
                for
                    cmds <- UI.commands
                    _    <- cmds.focusId("never")
                    // A waiter the instance does not own, so it is still there to be answered when the instance goes; one it owns
                    // is interrupted with it.
                    // Whether the instance goes before or after the request is filed, the request is answered.
                    _ <- Fiber.initUnscoped(asking.completeUnitDiscard.andThen(cmds.requestMeasureById("never")).map(r =>
                        measured.completeDiscard(Result.succeed(r))
                    ))
                    _ <- gate.get
                yield input.id("never")
            ).placeholder(span("loading"))
            result <- mountLogged(div(show.render(on => if on then held else span("gone")))) { (screen, log) =>
                for
                    _    <- screen.await("the placeholder")(_.contains("loading"))
                    _    <- asking.get
                    _    <- show.set(false)
                    _    <- screen.await("the swap")(_.contains("gone"))
                    rect <- measured.get
                    _    <- gate.completeUnitDiscard
                    seen <- log.get
                yield (rect, seen)
            }
        yield
            val (rect, seen) = result
            assert(rect == Absent, s"measure: $rect")
            // The measure may go out while the effect still runs, and is answered Absent then; the focus waits for content that
            // never comes and is dropped.
            assert(!seen.exists(_.startsWith("focus")), s"sent for content that never showed: $seen")
    }

    "an effect that measures an element already on the page before it yields gets its answer" in {
        val ui = div(
            span("anchor").id("anchor"),
            UI.mounted(
                for
                    cmds <- UI.commands
                    at   <- cmds.requestMeasureById("anchor")
                yield span(s"measured:${at.isDefined}")
            ).placeholder(span("loading"))
        )
        mountLogged(ui) { (screen, log) =>
            screen.await("the content built from the measure")(_.contains("measured:true"))
                .andThen(log.get.map(seen => assert(seen == Chunk("measure:anchor:true"), seen.toString)))
        }
    }

end UICommandsShownTest
