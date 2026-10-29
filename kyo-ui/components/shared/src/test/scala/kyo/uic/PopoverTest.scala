package kyo.uic

import kyo.*
import kyo.UI.*

/** [[Popover]]'s visibility under rule A ([[ReactiveValueContract]]): unbound it is the popover's own, closed with a trigger
  * to open it and open without one. The trigger is the control the reader reaches, so the state is said there: that it
  * opens a dialog, whether the dialog is open, and, where the state is fixed, that it changes nothing. The wrapper around
  * it only takes the click.
  */
class PopoverTest extends ReactiveValueContract:

    private def withTrigger(using Frame) = uic.Popover().trigger(uic.Button("More"))(p("inside"))

    private def panels(t: UI)(using Frame) = elementsWithClass(t, "p-popover")

    holds(new Slot[Popover, Boolean](
        name = "open, with a trigger",
        bare = withTrigger,
        bind = _.open(_),
        hear = (o, f) => o.onOpenChange(f),
        event = "onOpenChange",
        render = _.render,
        start = false,
        next = true,
        control = elementWithClass(_, "p-uic-popover-trigger"),
        shows = panels(_).map(_.nonEmpty),
        fixedOn = Present(elementWithClass(_, "p-button"))
    ))

    // Read off the page as a transport renders it: an unbound popover holds its state in a signal of its own, so
    // `aria-expanded` is a channel there, not one of the element's constant attributes.
    "a button trigger says it opens a dialog, and whether the dialog is open" in {
        for
            closed <- renderHtml(withTrigger.render)
            open   <- renderHtml(withTrigger.open(true).onOpenChange(_ => ()).render)
        yield
            assert(closed.contains("""aria-haspopup="dialog""""), closed)
            assert(closed.contains("""aria-expanded="false""""), closed)
            assert(open.contains("""aria-expanded="true""""), open)
        end for
    }

    "a trigger that is no control of its own becomes one: a button role, a tab stop, and Enter and Space open it" in {
        for
            held <- Signal.initRef(false)
            ui = uic.Popover().open(held).trigger(span.cssClass("more")("More"))(p("inside")).render
            more       <- elementWithClass(ui, "more")
            shell      <- elementWithClass(ui, "p-uic-popover-trigger")
            _          <- press(shell, UI.Keyboard.Enter)
            afterEnter <- held.get
            _          <- held.set(false)
            _          <- press(shell, UI.Keyboard.Space)
            afterSpace <- held.get
        yield
            assert(
                more.attrs.role.contains("button") && more.attrs.tabIndex.contains(0),
                s"role ${more.attrs.role}, tab ${more.attrs.tabIndex}"
            )
            assert(more.attrs.ariaAttrs.get("haspopup").contains("dialog"), s"aria: ${more.attrs.ariaAttrs}")
            assert(afterEnter && afterSpace, s"Enter opened: $afterEnter, Space opened: $afterSpace")
        end for
    }

    "a native control as trigger keeps its own role and keys: the browser turns its keys into the click" in {
        for
            ui = uic.Popover().trigger(button.cssClass("more")("More"))(p("inside")).render
            native <- elementWithClass(ui, "more")
            shell  <- elementWithClass(ui, "p-uic-popover-trigger")
        yield
            assert(native.attrs.role.isEmpty && native.attrs.tabIndex.isEmpty, s"role ${native.attrs.role}, tab ${native.attrs.tabIndex}")
            assert(native.attrs.ariaAttrs.get("haspopup").contains("dialog"), s"aria: ${native.attrs.ariaAttrs}")
            assert(shell.attrs.onKeyDown.isEmpty, "a key on a native button is its click, which the wrapper already takes")
        end for
    }

    private inline val preamble = "import kyo.*\nimport kyo.UI.*\nimport kyo.uic\n"

    "a trigger is an element or a component that can carry the popup's state, so a bare region does not compile" in {
        typeCheckFailure(preamble + """def x(s: Signal[String])(using Frame) = uic.Popover().trigger(s.render(t => span(t)))""")
    }

    "and the same region inside an element does" in {
        typeCheck(preamble + """def x(s: Signal[String])(using Frame) = uic.Popover().trigger(span(s.render(t => span(t))))""")
    }

    "an unbound popover without a trigger starts open" in {
        for shown <- panels(uic.Popover()(p("inside")).render)
        yield assert(shown.size == 1, "nothing could open it, so it shows until dismissed")
    }

    "the card for a signal nobody hears names the popover, not the overlay it draws through" in {
        for
            held <- Signal.initRef(false)
            deaf <- elementsWithClass(withTrigger.open(held.readOnly).render, "p-uic-key-error")
        yield assert(deaf.size == 1 && textOf(deaf.head).contains("Popover"), s"nobody hears: ${deaf.map(textOf)}")
    }

end PopoverTest
