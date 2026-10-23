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

    private def panel(from: Int, filter: Boolean)(using Frame): (SignalRef[Int], SignalRef[Maybe[Fruit]], UI) < Async =
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(from)
            query <- Signal.initRef("")
            value <- Signal.initRef(Maybe.empty[Fruit])
            base = if filter then select.filterable(true) else select
        yield (hi, value, base.selection(value).wired(open, hi, query, Present("sel")))

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
            ref   <- Signal.initRef(Maybe.fromOption(fruit.find(_.id == value)))
            ui = select.selection(ref).wired(open, hi, query, Present("sel"))
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
            value <- Signal.initRef(Maybe.empty[Fruit])
            base = if filter then select.filterable(true) else select
            ui   = base.selection(value).wired(open, hi, query, Present("sel"))
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
            value <- Signal.initRef(Maybe.empty[Fruit])
            ui = select.selection(value).wired(open, hi, query, Present("sel"))
            trigger <- elementWithClass(ui, "p-select")
            _       <- press(trigger, UI.Keyboard.Enter)
            got     <- value.get.map(_.fold("")(_.id))
            still   <- open.get
        yield assert(got == "c" && !still, "it picks and closes")
    }

    "Space picks it too, where the panel has no text field to type into" in {
        for
            (_, value, ui) <- panel(2, filter = false)
            trigger        <- elementWithClass(ui, "p-select")
            _              <- press(trigger, UI.Keyboard.Space)
            got            <- value.get.map(_.fold("")(_.id))
        yield assert(got == "c")
    }

    "with a filter header Space belongs to the caret, so it picks nothing" in {
        for
            (_, value, ui) <- panel(2, filter = true)
            host           <- keyboardHost(ui, filter = true)
            _              <- press(host, UI.Keyboard.Space)
            got            <- value.get.map(_.fold("")(_.id))
        yield assert(got.isEmpty)
    }

    "Escape closes the panel, since the panel it would close never gets the key" in {
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(2)
            query <- Signal.initRef("")
            value <- Signal.initRef(Maybe.empty[Fruit])
            ui = select.selection(value).wired(open, hi, query, Present("sel"))
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
            value <- Signal.initRef(Maybe.empty[Fruit])
            ui = select.selection(value).onChange(_ => Abort.fail("rejected")).wired(open, hi, query, Present("sel"))
            host   <- keyboardHost(ui, filter = false)
            _      <- press(host, UI.Keyboard.Enter)
            picked <- value.get.map(_.fold("")(_.id))
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
            ref  <- Signal.initRef(Plain("c", "a copy of Cherry"))
            ui   <- opened(uic.Select[Plain]().options(plain)(_.name).optionKey(_.id).selection(ref))
            lbl  <- elementWithClass(ui, "p-select-label")
            rs   <- rows(ui)
            card <- elementsWithClass(ui, "p-uic-key-error")
            _    <- click(rs(1))
            got  <- ref.get
        yield
            assert(
                rs.map(_.attrs.cssClasses.contains("p-select-option-selected")) == Chunk(false, true),
                "the row keyed c is the chosen one"
            )
            assert(card.isEmpty, "so the held value is not reported as unknown")
            assert(textOf(lbl) == "Cherry", "the trigger shows the option the value matches")
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

    "there is no key binding beside the option, so a key no option carries cannot be bound" in {
        typeCheckFailure(
            "import kyo.*\nimport kyo.uic\ndef x(r: SignalRef[String]) = uic.Select[String]().options(Seq(\"a\")).value(r)"
        )
    }

    "onChange reports the option written, and a pick of the one already chosen fires nothing" in {
        for
            ref  <- Signal.initRef(Maybe(fruit(0)))
            seen <- Signal.initRef(List.empty[Maybe[String]])
            ui   <- opened(select.selection(ref).onChange(m => seen.updateAndGet(_ :+ m.map(_.id)).unit))
            rs   <- rows(ui)
            _    <- click(rs(0))
            _    <- click(rs(2))
            got  <- seen.get
        yield assert(got == List(Present("c")))
    }

    "a held option no row carries is named in a card, and shown from the value itself" in {
        val gone = Fruit("z", "Zucchini", false)
        for
            ref  <- Signal.initRef(Maybe(gone))
            ui   <- opened(select.selection(ref))
            lbl  <- elementWithClass(ui, "p-select-label")
            card <- elementWithClass(ui, "p-uic-key-error")
        yield
            assert(textOf(lbl) == "Zucchini")
            assert(textOf(card).contains("Zucchini"))
        end for
    }

    "while loading the same option is not reported, and the spinner stands in for the chevron" in {
        val gone = Fruit("z", "Zucchini", false)
        for
            ref     <- Signal.initRef(Maybe(gone))
            ui      <- opened(select.loading(true).selection(ref))
            cards   <- elementsWithClass(ui, "p-uic-key-error")
            spinner <- elementsWithClass(ui, "p-select-loading-icon")
            root    <- elementWithClass(ui, "p-select")
        yield
            assert(cards.isEmpty)
            assert(spinner.size == 1)
            assert(root.attrs.ariaAttrs.get("busy").contains("true"))
        end for
    }

    "a loading signal brings the card in once the load ends" in {
        val gone = Fruit("z", "Zucchini", false)
        for
            ref     <- Signal.initRef(Maybe(gone))
            loading <- Signal.initRef(true)
            ui1     <- opened(select.loading(loading).selection(ref))
            during  <- elementsWithClass(ui1, "p-uic-key-error")
            _       <- loading.set(false)
            ui2     <- opened(select.loading(loading).selection(ref))
            after   <- elementsWithClass(ui2, "p-uic-key-error")
        yield assert(during.isEmpty && after.size == 1)
        end for
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

    "onPick fires with the option picked, after onChange, and only when the pick changed the value" in {
        for
            ref  <- Signal.initRef(fruit(0))
            seen <- Signal.initRef(List.empty[String])
            ui   <- opened(
                select.selection(ref)
                    .onChange(m => seen.updateAndGet(_ :+ s"change ${m.fold("-")(_.id)}").unit)
                    .onPick(f => seen.updateAndGet(_ :+ s"pick ${f.id}").unit)
            )
            rs  <- rows(ui)
            _   <- click(rs(0))
            _   <- click(rs(2))
            got <- seen.get
        yield assert(got == List("change c", "pick c"), "the option already held fires neither")
    }

    "a clear fires onChange with Absent and no onPick, since nothing was picked" in {
        for
            ref   <- Signal.initRef(Maybe(fruit(0)))
            picks <- Signal.initRef(List.empty[String])
            seen  <- Signal.initRef(List.empty[Maybe[String]])
            ui    <- opened(
                select.showClear(true).selection(ref)
                    .onChange(m => seen.updateAndGet(_ :+ m.map(_.id)).unit)
                    .onPick(f => picks.updateAndGet(_ :+ f.id).unit)
            )
            x   <- elementWithClass(ui, "p-select-clear-icon")
            _   <- click(x)
            ps  <- picks.get
            cs  <- seen.get
            now <- ref.get
        yield
            assert(now == Absent)
            assert(cs == List(Absent))
            assert(ps.isEmpty)
    }

    "a one-way selection shows the signal, and a pick reports through onPick without writing" in {
        for
            route <- Signal.initRef(fruit(2))
            picks <- Signal.initRef(List.empty[String])
            ui    <- opened(select.selection(route.readOnly).onPick(f => picks.updateAndGet(_ :+ f.id).unit))
            rs    <- rows(ui)
            lbl   <- elementWithClass(ui, "p-select-label")
            _     <- click(rs(3))
            held  <- route.get
            got   <- picks.get
            _     <- route.set(fruit(3))
            ui2   <- opened(select.selection(route.readOnly))
            lbl2  <- elementWithClass(ui2, "p-select-label")
        yield
            assert(textOf(lbl) == "Cherry", "the field shows what the signal holds")
            assert(got == List("d"), "the pick is reported")
            assert(held == fruit(2), "and nothing was written: the app owns the state")
            assert(textOf(lbl2) == "Date", "an app write moves the field")
        end for
    }

    "a one-way selection reports no change for the option it already shows" in {
        for
            route <- Signal.initRef(Maybe(fruit(2)))
            seen  <- Signal.initRef(0)
            ui    <- opened(select.selection(route.readOnly).onChange(_ => seen.updateAndGet(_ + 1).unit).onPick(_ =>
                seen.updateAndGet(_ + 1).unit
            ))
            rs <- rows(ui)
            _  <- click(rs(2))
            n  <- seen.get
        yield assert(n == 0)
    }

    "a constant selection shows its option and only reports a pick" in {
        for
            picks <- Signal.initRef(List.empty[String])
            ui    <- opened(select.selection(fruit(3)).onPick(f => picks.updateAndGet(_ :+ f.id).unit))
            lbl   <- elementWithClass(ui, "p-select-label")
            rs    <- rows(ui)
            _     <- click(rs(0))
            got   <- picks.get
        yield
            assert(textOf(lbl) == "Date")
            assert(got == List("a"))
    }

    "selection takes a ref, a read-only signal, or a signal over Maybe through one name" in {
        typeCheck("""
            import kyo.*
            import kyo.uic
            def x(r: SignalRef[String], s: Signal[String], m: Signal[Maybe[String]]) =
                (uic.Select[String]().selection(r), uic.Select[String]().selection(s), uic.Select[String]().selection(m))
        """)
    }

    "options take any Iterable: an enum's values array and a set pass as they are" in {
        for
            fromArray <- opened(uic.Select[SelectTest.Tone]().options(SelectTest.Tone.values)(_.toString))
            arrayRows <- rows(fromArray)
            fromSet   <- opened(uic.Select[String]().options(scala.collection.immutable.ListSet("x", "y")))
            setRows   <- rows(fromSet)
            none      <- opened(uic.Select[Maybe[SelectTest.Tone]]().optionsWithNone("Any", SelectTest.Tone.values)(_.toString))
            noneRows  <- rows(none)
        yield
            assert(arrayRows.map(textOf) == Chunk("Warm", "Cool"))
            assert(setRows.map(textOf) == Chunk("x", "y"))
            assert(noneRows.map(textOf) == Chunk("Any", "Warm", "Cool"))
        end for
    }

    "every picker's options take an Iterable" in {
        typeCheck("""
            import kyo.*
            import kyo.uic
            enum T derives CanEqual:
                case A, B
            def x =
                (
                    uic.Select[T]().options(T.values)(_.toString).optionGroups(Set(uic.OptionItem.item(T.A)))(_.toString),
                    uic.Select[String]().options(Array("a", "b")),
                    uic.MultiSelect[T]().options(T.values)(_.toString).optionGroups(Vector(uic.OptionItem.item(T.A)))(_.toString),
                    uic.MultiSelect[String]().options(Set("a")),
                    uic.SelectButton[T]().options(T.values)(_.toString),
                    uic.SelectButton[String]().options(Set("a")),
                    uic.SelectButton[Maybe[T]]().optionsWithNone("Any", T.values)(_.toString),
                    uic.SelectButton[T]().multiple.options(T.values)(_.toString),
                    uic.SelectButton[String]().multiple.options(Set("a")),
                    uic.AutoComplete[T]().options(T.values)(_.toString).optionGroups(Set(uic.OptionItem.item(T.A)))(_.toString),
                    uic.AutoComplete[String]().options(Set("a")),
                    uic.CascadeSelect[T]().options(Set(uic.CascadeItem.leaf(T.A)))(_.toString),
                    uic.TreeSelect[T]().options(T.values)(_.toString)(_ => Set.empty[T]),
                    uic.TreeSelect[String]().options(T.values)(_.toString, _.toString)(_ => Vector.empty[T])
                )
        """)
    }

end SelectTest

object SelectTest:
    enum Tone derives CanEqual:
        case Warm, Cool
end SelectTest
