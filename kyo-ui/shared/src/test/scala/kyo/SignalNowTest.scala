package kyo

import kyo.UI.*
import kyo.internal.HtmlRenderer
import kyo.internal.ReactiveUI
import kyo.internal.SignalNow

/** A synchronous pass reads a signal the way the fiber running it would, Locals included. */
class SignalNowTest extends kyo.test.Test[Any]:

    private val label: Local[String] = Local.init("unset")

    /** A signal over the local's value, as a translation chosen by an ambient language is. */
    private def labelled(using Frame): Signal[String] =
        Signal.withLocal(label)(l => Signal.initConst(s"label:$l"))

    "a signal over a Local reads the value the running fiber sees" in {
        label
            .let("set")(SignalNow.Locals.take(Sync.Unsafe.defer(SignalNow[String](labelled))))
            .map(read => assert(read == "label:set"))
    }

    "a text leaf over a Local renders the value the mount sees, not the Local's default" in {
        label
            .let("set")(HtmlRenderer.render(UI.div(UI.span(labelled).id("l")), Seq.empty))
            .map(html => assert(html.contains("label:set") && !html.contains("label:unset"), html))
    }

    "a keyed list whose items follow a Local renders its rows and keys by the value the mount sees" in {
        val rows = Signal.withLocal(label)(l => Signal.initConst(Chunk(s"$l-a", s"$l-b")))
        label
            .let("set")(HtmlRenderer.render(UI.ul(rows.foreachKeyed(identity)(v => UI.li(UI.span(v)))), Seq.empty))
            .map(html => assert(html.contains("set-a") && html.contains("set-b") && !html.contains("unset"), html))
    }

    "the walk that subscribes a page keeps, as what it painted, the value the mount sees" in {
        def all(rui: ReactiveUI): Seq[ReactiveUI] = rui +: rui.children.flatMap(all)
        label
            .let("set")(ReactiveUI.normalize(UI.div(UI.span(labelled).id("l")), Seq.empty))
            .map { rui =>
                val painted = all(rui).flatMap(_.renderedValue.toList).collect { case t: UI.Ast.Text => t.value }
                assert(painted == Seq("label:set"))
            }
    }

end SignalNowTest
