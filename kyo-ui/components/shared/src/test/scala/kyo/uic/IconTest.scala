package kyo.uic

import kyo.*

/** An interactive [[Icon]] is a `span` the component makes focusable itself, so the two keys a
  * `role="button"` owes the reader are the component's to deliver: the browser gives a span
  * neither focus nor Enter.
  */
class IconTest extends UicTest:

    private def clicks(icon: Icon)(using Frame): (AtomicInt, UI) < Sync =
        AtomicInt.init(0).map(hits => (hits, icon.onClick(hits.incrementAndGet.unit).render))

    "an interactive icon acts on Enter and on Space, exactly as it does on a click" in {
        for
            (hits, ui) <- clicks(uic.Icon(uic.Icons.check).accessibleName("Save"))
            el         <- elementWithClass(ui, "p-uic-icon")
            _          <- press(el, UI.Keyboard.Enter)
            afterEnter <- hits.get
            _          <- press(el, UI.Keyboard.Space)
            afterSpace <- hits.get
            _          <- click(el)
            afterClick <- hits.get
        yield
            assert(afterEnter == 1, "Enter runs it")
            assert(afterSpace == 2, "and so does Space")
            assert(afterClick == 3, "and a click still does")
    }

    "an icon that takes no click stays out of the tab order and answers no key" in {
        for
            ui <- Kyo.lift(uic.Icon(uic.Icons.check).accessibleName("Save").render)
            el <- elementWithClass(ui, "p-uic-icon")
        yield
            assert(el.attrs.tabIndex.isEmpty, "nothing to operate, so nothing to focus")
            assert(el.attrs.onKeyDown.isEmpty)
    }

    "a key that is not an activation leaves the icon alone" in {
        for
            (hits, ui) <- clicks(uic.Icon(uic.Icons.check).accessibleName("Save"))
            el         <- elementWithClass(ui, "p-uic-icon")
            _          <- press(el, UI.Keyboard.ArrowDown)
            after      <- hits.get
        yield assert(after == 0)
    }

end IconTest
