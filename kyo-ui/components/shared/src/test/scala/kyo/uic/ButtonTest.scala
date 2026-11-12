package kyo.uic

import kyo.*

class ButtonTest extends UicTest:

    private def renderHtml(ui: UI)(using Frame): String < Async =
        UI.runRender(ui).take(1).run.map(_.mkString)

    "a link button marks aria-disabled and the disabled style when ariaDisabled is set" in {
        for
            html <- renderHtml(uic.Button("Go").href("/go").ariaDisabled(true))
        yield
            assert(html.contains("<a "), "renders an anchor")
            assert(html.contains("""aria-disabled="true""""), "ariaDisabled marks the link aria-disabled")
            assert(html.contains("p-uic-aria-disabled"), "ariaDisabled applies the greyed style to the link")
    }

    "a disabled link drops its href and leaves the tab order" in {
        for
            html <- renderHtml(uic.Button("Go").href("/go").ariaDisabled(true))
        yield
            assert(!html.contains("href="), "a disabled link renders no href")
            assert(html.contains("""tabindex="-1""""), "a disabled link is out of the tab order")
    }

    "an icon-only button with only an end icon is laid out as icon-only" in {
        for
            html <- renderHtml(uic.Button().endIcon(uic.Icons.check))
        yield assert(html.contains("p-button-icon-only"), "an end icon alone makes the button icon-only")
    }

    "a button with an empty label and an icon is icon-only, with no label span" in {
        for
            html <- renderHtml(uic.Button("").icon(uic.Icons.check))
        yield
            assert(html.contains("p-button-icon-only"), "an empty label counts as no label")
            assert(!html.contains("p-button-label"), "an empty label emits no label span")
    }

    "a loading button stays in the tab order: aria-disabled, not the native disabled attribute" in {
        for
            html <- renderHtml(uic.Button("Save").loading(true))
        yield
            assert(!html.contains(" disabled"), "a loading button carries no native disabled attribute")
            assert(html.contains("""aria-disabled="true""""), "a loading button is aria-disabled")
            assert(html.contains("p-uic-aria-disabled"), "a loading button gets the greyed aria-disabled style")
    }

    "form(id) emits the native form attribute and no data-uic-form hint" in {
        for
            html <- renderHtml(uic.Button("Save").form("order-form"))
        yield
            assert(html.contains(""" form="order-form""""), s"the button carries the native form attribute: $html")
            assert(!html.contains("data-uic-form"), "the hint attribute is gone")
    }

    "a click on a loading button runs no action" in {
        for
            count <- AtomicInt.init(0)
            ui = uic.Button("Save").loading(true).onClick(count.incrementAndGet.map(_ => ()))
            el <- elementWithClass(ui, "p-button")
            _  <- click(el)
            n  <- count.get
        yield assert(n == 0, s"a click on a loading button ran the action $n time(s)")
    }
end ButtonTest
