package kyo.uic

import kyo.*
import kyo.UI.*

/** The keyboard of `uic.Select`'s open panel, which lives wherever focus does.
  *
  * Focus never enters the panel. Without a filter header it stays on the TRIGGER, so the trigger
  * carries the whole option keyboard; with one it goes into the header's INPUT, so the input does.
  * That is the same rule that decides which element is the combobox and where
  * `aria-activedescendant` is stamped, because a screen reader reads that attribute off the
  * focused element or off nothing at all.
  */
class SelectTest extends UicTest:

    final private case class Fruit(id: String, name: String, sold: Boolean) derives CanEqual

    private val fruit = List(
        Fruit("a", "Apple", false),
        Fruit("b", "Banana", true),
        Fruit("c", "Cherry", false),
        Fruit("d", "Date", false)
    )

    private def select(using Frame) =
        uic.Select[Fruit]().options(fruit)(_.name).optionKey(_.id).optionDisabled(_.sold).id("sel")

    /** The element the panel's keyboard lives on: the filter header's input where there is one,
      * the trigger otherwise.
      */
    private def keyboardHost(ui: UI, filter: Boolean)(using Frame): UI.Ast.Element < Sync =
        elementWithClass(ui, if filter then "p-select-filter" else "p-select")

    private def panel(from: Int, filter: Boolean)(using Frame): (SignalRef[Int], SignalRef[String], UI) < Async =
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(from)
            query <- Signal.initRef("")
            value <- Signal.initRef("")
            base = if filter then select.filterable(true) else select
        yield (hi, value, base.value(value).wired(open, hi, query, Present("sel")))

    /** Opens the panel with the highlight at `from`, presses `key`, and reports where it landed. */
    private def after(from: Int, key: UI.Keyboard, filter: Boolean = false)(using Frame): Int < Async =
        for
            (hi, _, ui) <- panel(from, filter)
            host        <- keyboardHost(ui, filter)
            _           <- press(host, key)
            at          <- hi.get
        yield at

    /** Presses `key` on the CLOSED trigger and reports whether the panel opened and where the
      * highlight landed.
      */
    private def opening(key: UI.Keyboard, value: String)(using Frame): (Boolean, Int) < Async =
        for
            open  <- Signal.initRef(false)
            hi    <- Signal.initRef(-1)
            query <- Signal.initRef("")
            ref   <- Signal.initRef(value)
            ui = select.value(ref).wired(open, hi, query, Present("sel"))
            trigger <- elementWithClass(ui, "p-select")
            _       <- press(trigger, key)
            isOpen  <- open.get
            at      <- hi.get
        yield (isOpen, at)

    /** Whether the open panel is still open after `key`. */
    private def stillOpen(key: UI.Keyboard, mods: UI.Modifiers = UI.Modifiers.none, filter: Boolean = false)(using
        Frame
    ): Boolean < Async =
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(1)
            query <- Signal.initRef("")
            value <- Signal.initRef("")
            base = if filter then select.filterable(true) else select
            ui   = base.value(value).wired(open, hi, query, Present("sel"))
            host  <- keyboardHost(ui, filter)
            _     <- press(host, key, mods)
            still <- open.get
        yield still

    "an opening key lands on the option already selected" in
        opening(UI.Keyboard.ArrowDown, "c").map((open, at) => assert(open && at == 2))

    "and on the first option a highlight may sit on where nothing is selected" in {
        for
            down  <- opening(UI.Keyboard.ArrowDown, "")
            enter <- opening(UI.Keyboard.Enter, "")
            space <- opening(UI.Keyboard.Space, "")
        yield assert(
            down == (true, 0) && enter == (true, 0) && space == (true, 0),
            "opening has to land ON an option, or the arrow that opened the panel moved nothing"
        )
    }

    "ArrowDown from nothing lands on the first option" in after(-1, UI.Keyboard.ArrowDown).map(at => assert(at == 0))

    "ArrowUp from nothing lands on the last, as it does everywhere else in the family" in
        after(-1, UI.Keyboard.ArrowUp).map(at => assert(at == 3))

    "a sold-out option is stepped over" in after(0, UI.Keyboard.ArrowDown).map(at => assert(at == 2))

    "Home and End reach the ends, which the hand-written loop had no answer for" in {
        for
            home <- after(3, UI.Keyboard.Home)
            end  <- after(0, UI.Keyboard.End)
        yield assert(home == 0 && end == 3)
    }

    "the ends hold, since Prime's select does not cycle" in {
        for
            top    <- after(0, UI.Keyboard.ArrowUp)
            bottom <- after(3, UI.Keyboard.ArrowDown)
        yield assert(top == 0 && bottom == 3)
    }

    "Enter picks the highlighted option" in {
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(2)
            query <- Signal.initRef("")
            value <- Signal.initRef("")
            ui = select.value(value).wired(open, hi, query, Present("sel"))
            trigger <- elementWithClass(ui, "p-select")
            _       <- press(trigger, UI.Keyboard.Enter)
            got     <- value.get
            still   <- open.get
        yield assert(got == "c" && !still, "it picks and closes")
    }

    "Space picks it too, where the panel has no text field to type into" in {
        for
            (_, value, ui) <- panel(2, filter = false)
            trigger        <- elementWithClass(ui, "p-select")
            _              <- press(trigger, UI.Keyboard.Space)
            got            <- value.get
        yield assert(got == "c")
    }

    "with a filter header Space belongs to the caret, so it picks nothing" in {
        for
            (_, value, ui) <- panel(2, filter = true)
            host           <- keyboardHost(ui, filter = true)
            _              <- press(host, UI.Keyboard.Space)
            got            <- value.get
        yield assert(got.isEmpty)
    }

    "Escape closes the panel, since the panel it would close never gets the key" in {
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(2)
            query <- Signal.initRef("")
            value <- Signal.initRef("")
            ui = select.value(value).wired(open, hi, query, Present("sel"))
            trigger <- elementWithClass(ui, "p-select")
            _       <- press(trigger, UI.Keyboard.Escape)
            still   <- open.get
        yield assert(!still)
    }

    "a printable key jumps to the option it starts" in
        after(-1, UI.Keyboard.Char('c')).map(at => assert(at == 2))

    "the same letter again walks to the next match" in {
        // Two options start with a D and a B, so C is the interesting single case; a repeat of a
        // letter with one match stays where it is.
        for
            first <- after(-1, UI.Keyboard.Char('a'))
            again <- after(0, UI.Keyboard.Char('a'))
        yield assert(first == 0 && again == 0)
    }

    "a printable key does not jump past a sold-out option" in
        after(-1, UI.Keyboard.Char('b')).map(at => assert(at == -1, "Banana is disabled, so nothing matches"))

    "with a filter header a printable key types instead of jumping" in
        after(-1, UI.Keyboard.Char('c'), filter = true).map(at => assert(at == -1))

    "the trigger is the combobox, and announces the highlighted option" in {
        for
            (_, _, ui) <- panel(2, filter = false)
            trigger    <- elementWithClass(ui, "p-select")
            list       <- elementWithClass(ui, "p-select-list")
            rows       <- elementsWithClass(ui, "p-select-option")
        yield
            assert(trigger.attrs.role.contains("combobox"))
            assert(trigger.attrs.ariaAttrs.get("controls").contains("sel-list"))
            assert(trigger.attrs.ariaAttrs.get("activedescendant").contains("sel-option-2"))
            assert(list.attrs.identifier.contains("sel-list"))
            assert(!list.attrs.ariaAttrs.contains("activedescendant"), "the unfocused list says nothing")
            assert(rows(2).attrs.identifier.contains("sel-option-2"), "and the option it names is there")
    }

    "with a filter header the header's input is the combobox instead" in {
        for
            (_, _, ui) <- panel(2, filter = true)
            trigger    <- elementWithClass(ui, "p-select")
            filterEl   <- elementWithClass(ui, "p-select-filter")
        yield
            assert(trigger.attrs.role.contains("button"), "the trigger is then the button that reveals it")
            assert(!trigger.attrs.ariaAttrs.contains("activedescendant"))
            assert(filterEl.attrs.role.contains("combobox"))
            assert(filterEl.attrs.focusAuto.contains(true), "and it is what the panel seeds focus onto")
            assert(filterEl.attrs.ariaAttrs.get("activedescendant").contains("sel-option-2"))
            assert(filterEl.attrs.ariaAttrs.get("controls").contains("sel-list"))
    }

    "nothing highlighted announces nothing" in {
        for
            (_, _, ui) <- panel(-1, filter = false)
            trigger    <- elementWithClass(ui, "p-select")
        yield assert(!trigger.attrs.ariaAttrs.contains("activedescendant"))
    }

    "Tab closes the panel, in both directions and with a filter header too" in {
        val shift = UI.Modifiers(ctrl = false, alt = false, shift = true, meta = false)
        for
            fwd      <- stillOpen(UI.Keyboard.Tab)
            back     <- stillOpen(UI.Keyboard.Tab, shift)
            inFilter <- stillOpen(UI.Keyboard.Tab, filter = true)
        yield assert(
            !fwd && !back && !inFilter,
            "a panel the reader has tabbed away from is one nothing answers"
        )
        end for
    }

    "the panel takes neither focus nor keys, so the two cannot disagree" in {
        for
            (_, _, ui) <- panel(2, filter = false)
            overlay    <- elementWithClass(ui, "p-uic-overlay-panel")
        yield
            assert(overlay.attrs.focusAuto.isEmpty)
            assert(overlay.attrs.onKeyDown.isEmpty)
    }

    "a pick whose onChange fails still takes the value and closes the panel" in {
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(0)
            query <- Signal.initRef("")
            value <- Signal.initRef("")
            ui = select.value(value).onChange(_ => Abort.fail("rejected")).wired(open, hi, query, Present("sel"))
            host   <- keyboardHost(ui, filter = false)
            _      <- press(host, UI.Keyboard.Enter)
            picked <- value.get
            still  <- open.get
        yield assert(picked == "a" && !still, s"the select finished its own work; value=$picked open=$still")
    }

    /** All the text an element holds, however deeply. */
    private def textOf(e: UI.Ast.Element)(using Frame): String =
        def walk(u: UI): String = u match
            case t: UI.Ast.Text     => t.value
            case el: UI.Ast.Element => el.children.map(walk).mkString
            case _                  => ""
        e.children.map(walk).mkString
    end textOf

    /** The select wired with its panel open, so its rows are on the tree. */
    private def opened(s: uic.Select[?])(using Frame): UI < Async =
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(-1)
            query <- Signal.initRef("")
        yield s.wired(open, hi, query, Present("sel"))

    private def rows(ui: UI)(using Frame): Chunk[UI.Ast.Element] < Sync = elementsWithClass(ui, "p-select-option")

    "selection binds the option itself: the ref picks the row, and a pick writes the option" in {
        for
            ref <- Signal.initRef(fruit(2))
            ui  <- opened(select.selection(ref))
            rs  <- rows(ui)
            _   <- click(rs(3))
            got <- ref.get
        yield
            assert(rs.map(_.attrs.cssClasses.contains("p-select-option-selected")) == Chunk(false, false, true, false))
            assert(got == fruit(3))
    }

    "a selection over Maybe starts empty under its placeholder, and clearing it writes Absent" in {
        for
            ref <- Signal.initRef(Maybe.empty[Fruit])
            base = select.placeholder("Pick one").showClear(true).selection(ref)
            ui   <- opened(base)
            lbl  <- elementWithClass(ui, "p-select-label")
            rs   <- rows(ui)
            _    <- click(rs(0))
            got  <- ref.get
            ui2  <- opened(base)
            x    <- elementWithClass(ui2, "p-select-clear-icon")
            _    <- click(x)
            gone <- ref.get
        yield
            assert(lbl.attrs.cssClasses.contains("p-placeholder") && textOf(lbl) == "Pick one")
            assert(got == Present(fruit(0)))
            assert(gone == Absent)
    }

    "a selection that always holds an option offers no clear, since there is nothing to clear it to" in {
        for
            ref <- Signal.initRef(fruit(0))
            ui  <- opened(select.showClear(true).selection(ref))
            xs  <- elementsWithClass(ui, "p-select-clear-icon")
        yield assert(xs.isEmpty)
    }

    "the option is matched by its key, not by the option type's own equality" in {
        // Reference equality: the ref holds an instance that equals none of the options.
        final class Plain(val id: String, val name: String)
        given CanEqual[Plain, Plain] = CanEqual.derived
        val plain                    = List(Plain("a", "Apple"), Plain("c", "Cherry"))
        for
            ref <- Signal.initRef(Plain("c", "a copy of Cherry"))
            ui  <- opened(uic.Select[Plain]().options(plain)(_.name).optionKey(_.id).selection(ref))
            lbl <- elementWithClass(ui, "p-select-label")
            rs  <- rows(ui)
            _   <- click(rs(1))
            got <- ref.get
        yield
            assert(textOf(lbl) == "Cherry")
            assert(got.name == "a copy of Cherry", "picking the row already chosen keeps the value the app holds")
        end for
    }

    "optionsWithNone puts the none option first and names it, and picking it writes Absent" in {
        for
            ref <- Signal.initRef(Maybe("Closed"))
            ui  <- opened(uic.Select[Maybe[String]]().optionsWithNone("All", Seq("Open", "Closed"))(_.toUpperCase).selection(ref))
            rs  <- rows(ui)
            _   <- click(rs(0))
            got <- ref.get
        yield
            assert(rs.map(textOf) == Chunk("All", "OPEN", "CLOSED"))
            assert(rs.map(_.attrs.cssClasses.contains("p-select-option-selected")) == Chunk(false, false, true))
            assert(got == Absent)
    }

    "optionsWithNone takes only a select whose options are Maybe" in {
        typeCheckFailure(
            "import kyo.*\nimport kyo.uic\ndef x = uic.Select[String]().optionsWithNone(\"All\", Seq(\"a\"))(identity)"
        )
    }

    "the binding set last wins, whether it is a key or the option" in {
        for
            keys  <- Signal.initRef("")
            typed <- Signal.initRef(fruit(0))
            ui    <- opened(select.selection(typed).value(keys))
            rs    <- rows(ui)
            _     <- click(rs(2))
            k     <- keys.get
            t     <- typed.get
            ui2   <- opened(select.value(keys).selection(typed))
            rs2   <- rows(ui2)
            _     <- click(rs2(3))
            k2    <- keys.get
            t2    <- typed.get
        yield
            assert(k == "c" && t == fruit(0))
            assert(k2 == "c" && t2 == fruit(3))
    }

    "itemTemplate draws each row and the chosen value, and the label stays their name" in {
        for
            ref <- Signal.initRef(fruit(2))
            ui  <- opened(select.itemTemplate(f => span.cssClass("fruit-tpl")(f.name.toUpperCase)).selection(ref))
            rs  <- rows(ui)
            lbl <- elementWithClass(ui, "p-select-label")
        yield
            assert(rs.map(textOf) == Chunk("APPLE", "BANANA", "CHERRY", "DATE"))
            assert(rs.map(_.attrs.ariaAttrs.getOrElse("label", "-")) == Chunk("Apple", "Banana", "Cherry", "Date"))
            assert(textOf(lbl) == "CHERRY")
    }

    "with nothing chosen the trigger shows the placeholder, not a template" in {
        for
            ref <- Signal.initRef(Maybe.empty[Fruit])
            ui  <- opened(select.placeholder("Pick one").itemTemplate(f => span(f.name.toUpperCase)).selection(ref))
            lbl <- elementWithClass(ui, "p-select-label")
        yield assert(textOf(lbl) == "Pick one")
    }

end SelectTest
