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

    holds(new SlotCase[Popover, Boolean](
        name = "open, with a trigger",
        bare = Present(withTrigger),
        bind = withTrigger.open(_),
        hear = (o, f) => o.onOpenChange(f),
        event = "onOpenChange",
        render = _.render,
        start = false,
        next = true,
        control = elementWithClass(_, "p-uic-popover-trigger"),
        shows = panels(_).map(_.nonEmpty),
        fixedOn = Present(elementWithClass(_, "p-button"))
    ))

    private def onBackdrop(e: UI.Ast.Element): Boolean = e.attrs.cssClasses.contains("p-uic-overlay-backdrop")

    // Without a trigger nothing inside can open it, so it starts open and only an outside click or Escape closes it: a
    // fixed one is one nothing dismisses.
    holds(new SlotCase[Popover, Boolean](
        name = "open, without a trigger",
        bare = Present(uic.Popover()(p("inside"))),
        bind = uic.Popover().open(_)(p("inside")),
        hear = (o, f) => o.onOpenChange(f),
        event = "onOpenChange",
        render = _.render,
        start = true,
        next = false,
        control = elementWithClass(_, "p-uic-overlay-backdrop"),
        shows = panels(_).map(_.nonEmpty),
        act = Present(clickBubbling(_)(onBackdrop)),
        isFixed = Present(elementsWithClass(_, "p-uic-overlay-backdrop").map(_.isEmpty))
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
            _          <- press(more, UI.Keyboard.Enter)
            afterEnter <- held.get
            _          <- held.set(false)
            _          <- press(more, UI.Keyboard.Space)
            afterSpace <- held.get
        yield
            assert(
                more.attrs.role.contains("button") && more.attrs.tabIndex.contains(0),
                s"role ${more.attrs.role}, tab ${more.attrs.tabIndex}"
            )
            assert(more.attrs.ariaAttrs.get("haspopup").contains("dialog"), s"aria: ${more.attrs.ariaAttrs}")
            assert(afterEnter && afterSpace, s"Enter opened: $afterEnter, Space opened: $afterSpace")
            assert(
                more.attrs.onKeyDownSelf.nonEmpty && more.attrs.onKeyDown.isEmpty && shell.attrs.onKeyDown.isEmpty,
                "the keys are the trigger's own, so a key pressed in a control inside it does not reach them"
            )
        end for
    }

    "a link trigger with no address is no native control, so it is made one: a button role, a tab stop, its own keys" in {
        for
            held <- Signal.initRef(false)
            ui = uic.Popover().open(held).trigger(uic.Link("Details"))(p("inside")).render
            link <- elementWithClass(ui, "p-uic-link")
            _    <- press(link, UI.Keyboard.Enter)
            o    <- held.get
        yield
            assert(
                link.attrs.role.contains("button") && link.attrs.tabIndex.contains(0),
                s"role ${link.attrs.role}, tab ${link.attrs.tabIndex}"
            )
            assert(link.attrs.ariaAttrs.get("haspopup").contains("dialog"), s"aria: ${link.attrs.ariaAttrs}")
            assert(o, "Enter on the link opened the popover")
        end for
    }

    "a key handler the page put on its own trigger still runs, and the trigger still opens" in {
        for
            held  <- Signal.initRef(false)
            heard <- Signal.initRef(List.empty[String])
            ui = uic.Popover().open(held)
                .trigger(span.cssClass("more").onKeyDownSelf(evt => heard.updateAndGet(_ :+ evt.key.toString).unit)("More"))(
                    p("inside")
                ).render
            more <- elementWithClass(ui, "more")
            _    <- press(more, UI.Keyboard.Enter)
            o    <- held.get
            h    <- heard.get
        yield assert(o && h == List("Enter"), s"opened $o, the page heard $h")
    }

    "a native control as trigger keeps its own role and keys: the browser turns its keys into the click" in {
        for
            ui = uic.Popover().trigger(button.cssClass("more")("More"))(p("inside")).render
            native <- elementWithClass(ui, "more")
            shell  <- elementWithClass(ui, "p-uic-popover-trigger")
        yield
            assert(native.attrs.role.isEmpty && native.attrs.tabIndex.isEmpty, s"role ${native.attrs.role}, tab ${native.attrs.tabIndex}")
            assert(native.attrs.ariaAttrs.get("haspopup").contains("dialog"), s"aria: ${native.attrs.ariaAttrs}")
            assert(
                shell.attrs.onKeyDown.isEmpty && native.attrs.onKeyDownSelf.isEmpty,
                "a key on a native button is its click, which the wrapper already takes"
            )
        end for
    }

    /** What the dispatcher does with `key` pressed on the element with id `id` inside a trigger that is no control of its
      * own, as the client sends it: addressed by the path the element has on the page, bubbling from there.
      */
    private def pressInside(id: String, key: String)(using Frame): Boolean < Async =
        import kyo.internal.*
        Signal.initRef(false).map { open =>
            val ui = uic.Popover().open(open)
                .trigger(div.cssClass("card")(button.id("inner")("Go"), input.id("note")))(p("inside")).render
            HtmlRenderer.render(ui, Seq.empty).map { html =>
                val at = pathOf(html, id)
                Scope.run {
                    for
                        root <- ReactiveUI.normalize(ui, Seq.empty)
                        sub  <- ReactiveUI.subscribe(root, UIExchange.noop)
                        _    <- Abort.run[Any](sub.handle(at, UIEvent.KeyDown(at, KeyboardEventData(key, UI.Modifiers.none, Absent))))
                        o    <- open.get
                    yield o
                }
            }
        }
    end pressInside

    "Enter on a button inside the trigger is that button's click, so it opens the popover once" in {
        pressInside("inner", "Enter").map(o => assert(o, "the key and the click it became toggled twice"))
    }

    "a space typed into a field inside the trigger is the field's, and leaves the popover as it was" in {
        pressInside("note", " ").map(o => assert(!o, "typing a space toggled the popover"))
    }

    private inline val preamble = "import kyo.*\nimport kyo.UI.*\nimport kyo.uic\n"

    "a trigger is an element or a component that can carry the popup's state, so a bare region does not compile" in {
        typeCheckFailure(preamble + """def x(s: Signal[String])(using Frame) = uic.Popover().trigger(s.render(t => span(t)))""")
    }

    "and the same region inside an element does" in {
        typeCheck(preamble + """def x(s: Signal[String])(using Frame) = uic.Popover().trigger(span(s.render(t => span(t))))""")
    }

    "the card for a signal nobody hears names the popover, not the overlay it draws through" in {
        for
            held <- Signal.initRef(false)
            deaf <- elementsWithClass(withTrigger.open(held.readOnly).render, "p-uic-diagnostic")
        yield assert(deaf.size == 1 && textOf(deaf.head).contains("Popover"), s"nobody hears: ${deaf.map(textOf)}")
    }

end PopoverTest
