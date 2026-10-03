package kyo.uic

import kyo.*
import kyo.UI.*

/** `uic.SelectButton`'s two arities, which are two different widgets.
  *
  * A choice of ONE out of several is a radio group however it is drawn: one tab stop for the whole
  * group, the arrows moving and selecting inside it, and options that report `aria-checked` rather
  * than a toggle button's `aria-pressed`. A choice of SEVERAL is what it looks like, a row of
  * independent toggle buttons, each its own tab stop, and it stays that way.
  */
class SelectButtonTest extends UicTest:

    private val sizes = Seq("Small", "Medium", "Large")

    private def group(using Frame) = uic.SelectButton[String]().options(sizes)

    private val ids = List("sb-0", "sb-1", "sb-2")

    /** The group wired the way its mount wires it, with the focus moves recorded. */
    private def wired(selected: String, disabled: String => Boolean = _ => false)(using
        Frame
    ): (SignalRef[String], SignalRef[List[String]], UI) < Async =
        for
            value <- Signal.initRef(selected)
            moved <- Signal.initRef(List.empty[String])
        yield (
            value,
            moved,
            group.optionDisabled(disabled).value(value).wired(ids, id => moved.updateAndGet(_ :+ id).unit)
        )

    private def options(ui: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elementsWithClass(ui, "p-togglebutton")

    /** Presses `key` on the option at `at` and reports where focus went and what got selected. */
    private def after(at: Int, key: UI.Keyboard, selected: String = "Medium")(using
        Frame
    ): (List[String], String) < Async =
        for
            (value, moved, ui) <- wired(selected)
            os                 <- options(ui)
            _                  <- press(os(at), key)
            went               <- moved.get
            got                <- value.get
        yield (went, got)

    "the group says it is one, and its options say they are radios" in {
        for
            (_, _, ui) <- wired("Medium")
            root       <- elementWithClass(ui, "p-selectbutton")
            os         <- options(ui)
        yield
            assert(root.attrs.role.contains("radiogroup"))
            assert(os.forall(_.attrs.role.contains("radio")))
            assert(os.map(_.attrs.ariaAttrs.getOrElse("checked", "-")) == Chunk("false", "true", "false"))
            assert(os.forall(!_.attrs.ariaAttrs.contains("pressed")), "one role, one state")
    }

    "one tab stop, and it sits on the option that is chosen" in {
        for
            (_, _, ui) <- wired("Large")
            os         <- options(ui)
        yield assert(os.map(_.attrs.tabIndex.getOrElse(99)) == Chunk(-1, -1, 0))
    }

    "and on the first option a reader may choose while nothing is" in {
        for
            (_, _, ui) <- wired("")
            os         <- options(ui)
        yield assert(os.map(_.attrs.tabIndex.getOrElse(99)) == Chunk(0, -1, -1), "a group nobody has answered is still tabbable")
    }

    "an arrow moves the focus and picks what it lands on, in both axes" in {
        for
            right <- after(1, UI.Keyboard.ArrowRight)
            down  <- after(1, UI.Keyboard.ArrowDown)
            left  <- after(1, UI.Keyboard.ArrowLeft)
            up    <- after(1, UI.Keyboard.ArrowUp)
        yield assert(
            right == (List("sb-2"), "Large") && down == (List("sb-2"), "Large") &&
                left == (List("sb-0"), "Small") && up == (List("sb-0"), "Small"),
            "the radio pattern answers Down and Right alike, and Up and Left alike"
        )
    }

    "the group wraps at its ends, as a radio group does" in {
        for
            past  <- after(2, UI.Keyboard.ArrowRight)
            below <- after(0, UI.Keyboard.ArrowLeft, selected = "Small")
        yield assert(past == (List("sb-0"), "Small") && below == (List("sb-2"), "Large"))
    }

    "Home and End reach the ends and pick there too" in {
        for
            home <- after(1, UI.Keyboard.Home)
            end  <- after(1, UI.Keyboard.End)
        yield assert(home == (List("sb-0"), "Small") && end == (List("sb-2"), "Large"))
    }

    "a key that lands where it already is picks nothing, so the choice cannot be cleared by moving" in {
        for
            (value, moved, ui) <- wired("Small")
            os                 <- options(ui)
            _                  <- press(os(0), UI.Keyboard.Home)
            went               <- moved.get
            got                <- value.get
        yield assert(went.isEmpty && got == "Small", "Home on the chosen option is not a second click on it")
    }

    "a disabled option is stepped over rather than landed on" in {
        for
            (value, moved, ui) <- wired("Small", disabled = _ == "Medium")
            os                 <- options(ui)
            _                  <- press(os(0), UI.Keyboard.ArrowRight)
            went               <- moved.get
            got                <- value.get
        yield assert(went == List("sb-2") && got == "Large")
    }

    "Enter and Space are the button's own, so this handler adds nothing to them" in {
        for
            enter <- after(1, UI.Keyboard.Enter)
            space <- after(1, UI.Keyboard.Space)
        yield assert(
            enter == (Nil, "Medium") && space == (Nil, "Medium"),
            "a third activation beside the browser's and the dispatcher's would be one too many"
        )
    }

    "a click still clears the chosen option, which is what allowEmpty means" in {
        for
            value <- Signal.initRef("Medium")
            ui = group.value(value).wired(ids, _ => ())
            os  <- options(ui)
            _   <- click(os(1))
            got <- value.get
        yield assert(got.isEmpty)
    }

    "several choices stay a group of toggle buttons, each its own tab stop" in {
        for
            value <- Signal.initRef(Set("Small"))
            ui = uic.SelectButton[String]().options(sizes).multiple(true).value(value).render
            root <- elementWithClass(ui, "p-selectbutton")
            os   <- options(ui)
        yield
            assert(root.attrs.role.contains("group"))
            assert(os.forall(_.attrs.role.isEmpty), "a toggle button is a button")
            assert(os.map(_.attrs.ariaAttrs.getOrElse("pressed", "-")) == Chunk("true", "false", "false"))
            assert(os.forall(_.attrs.tabIndex.isEmpty), "each keeps the tab stop a button has by itself")
    }

end SelectButtonTest
