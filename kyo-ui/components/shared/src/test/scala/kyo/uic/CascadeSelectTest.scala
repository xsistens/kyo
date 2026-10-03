package kyo.uic

import kyo.*
import kyo.UI.*

/** Where `uic.CascadeSelect` puts the keyboard highlight when its chain opens, and what closes the
  * chain again.
  *
  * Focus never leaves the trigger, however deep the chain goes, so the whole cascade keyboard runs
  * from there: the key that opens it has to land ON a row, and the Tab that carries the reader out
  * of the widget has to take every open panel with it.
  */
class CascadeSelectTest extends UicTest:

    private val options = List(
        uic.CascadeItem.group("Europe")(
            uic.CascadeItem.group("Germany")(uic.CascadeItem.leaf("Berlin"), uic.CascadeItem.leaf("Hamburg")),
            uic.CascadeItem.leaf("Bern")
        ),
        uic.CascadeItem.leaf("Sydney")
    )

    private def cascade(using Frame) = uic.CascadeSelect[String]().options(options)(identity).id("cs")

    private def refsOf(using Frame): List[(List[Int], SignalRef[Boolean])] < Async =
        Kyo.foreach(cascade.groupPaths)(p => Signal.initRef(false).map(p -> _)).map(_.toList)

    /** Presses `key` on the CLOSED trigger and reports whether the chain opened and where the
      * highlight landed.
      */
    private def opening(key: UI.Keyboard)(using Frame): (Boolean, List[Int]) < Async =
        for
            open  <- Signal.initRef(false)
            refs  <- refsOf
            focus <- Signal.initRef(List.empty[Int])
            value <- Signal.initRef("")
            ui = cascade.value(value).wired(open, refs, focus, Present("cs"))
            trigger <- elementWithClass(ui, "p-cascadeselect")
            _       <- press(trigger, key)
            isOpen  <- open.get
            at      <- focus.get
        yield (isOpen, at)

    "an opening key lands on the first option, so the next arrow moves rather than starts" in {
        for
            down  <- opening(UI.Keyboard.ArrowDown)
            enter <- opening(UI.Keyboard.Enter)
            space <- opening(UI.Keyboard.Space)
        yield assert(down == (true, List(0)) && enter == (true, List(0)) && space == (true, List(0)))
    }

    "Tab closes the whole chain, in both directions, however deep it was opened" in {
        val shift = UI.Modifiers(shift = true)
        for
            open  <- Signal.initRef(true)
            refs  <- refsOf
            _     <- Kyo.foreach(refs)((_, r) => r.set(true))
            focus <- Signal.initRef(List(0, 0, 1))
            value <- Signal.initRef("")
            ui = cascade.value(value).wired(open, refs, focus, Present("cs"))
            trigger <- elementWithClass(ui, "p-cascadeselect")
            _       <- press(trigger, UI.Keyboard.Tab)
            still   <- open.get
            groups  <- Kyo.foreach(refs)((_, r) => r.get)
            back    <- Signal.initRef(true)
            refs2   <- refsOf
            focus2  <- Signal.initRef(List(0))
            ui2 = cascade.value(value).wired(back, refs2, focus2, Present("cs"))
            trigger2  <- elementWithClass(ui2, "p-cascadeselect")
            _         <- press(trigger2, UI.Keyboard.Tab, shift)
            stillBack <- back.get
        yield assert(
            !still && !stillBack && groups.forall(_ == false),
            "the chain's keyboard lives on the trigger the Tab is leaving, so no panel outlives it"
        )
        end for
    }

    "the arrows still walk the level the highlight is on" in {
        for
            open  <- Signal.initRef(true)
            refs  <- refsOf
            focus <- Signal.initRef(List(0))
            value <- Signal.initRef("")
            ui = cascade.value(value).wired(open, refs, focus, Present("cs"))
            trigger <- elementWithClass(ui, "p-cascadeselect")
            _       <- press(trigger, UI.Keyboard.ArrowDown)
            at      <- focus.get
        yield assert(at == List(1), "Sydney, the second root option")
    }

end CascadeSelectTest
