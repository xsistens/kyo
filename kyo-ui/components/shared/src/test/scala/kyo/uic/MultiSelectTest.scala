package kyo.uic

import kyo.*
import kyo.UI.*

/** The keyboard of `uic.MultiSelect`'s open panel, which lives wherever focus does: on the
  * TRIGGER without a filter header, in the header's INPUT with one.
  *
  * The same [[ListNav]] the rest of the family reads, with the one difference the pattern asks
  * for: an activation toggles the highlighted option and leaves the panel open.
  */
class MultiSelectTest extends UicTest:

    final private case class Fruit(id: String, name: String, sold: Boolean) derives CanEqual

    private val fruit = List(
        Fruit("a", "Apple", false),
        Fruit("b", "Banana", true),
        Fruit("c", "Cherry", false),
        Fruit("d", "Date", false)
    )

    private def multi(using Frame) =
        uic.MultiSelect[Fruit]().options(fruit)(_.name).optionKey(_.id).optionDisabled(_.sold).id("ms")

    private def panelOf(hiAt: Int, filter: Boolean = false)(using
        Frame
    ): (SignalRef[Int], SignalRef[Set[Fruit]], SignalRef[Boolean], UI) < Async =
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(hiAt)
            query <- Signal.initRef("")
            value <- Signal.initRef(Set.empty[Fruit])
            base = if filter then multi.filterable(true) else multi
        yield (hi, value, open, base.selection(value).wired(open, hi, query, Present("ms")))

    /** The element the panel's keyboard lives on: the filter header's input where there is one,
      * the trigger otherwise.
      */
    private def keyboardHost(ui: UI, filter: Boolean)(using Frame): UI.Ast.Element < Sync =
        elementWithClass(ui, if filter then "p-multiselect-filter" else "p-multiselect")

    private def after(from: Int, key: UI.Keyboard, filter: Boolean = false)(using Frame): Int < Async =
        for
            (hi, _, _, ui) <- panelOf(from, filter)
            host           <- keyboardHost(ui, filter)
            _              <- press(host, key)
            at             <- hi.get
        yield at

    /** Presses `key` on the CLOSED trigger and reports whether the panel opened and where the
      * highlight landed.
      */
    private def opening(key: UI.Keyboard, selected: Set[String])(using Frame): (Boolean, Int) < Async =
        for
            open  <- Signal.initRef(false)
            hi    <- Signal.initRef(-1)
            query <- Signal.initRef("")
            value <- Signal.initRef(fruit.filter(f => selected.contains(f.id)).toSet)
            ui = multi.selection(value).wired(open, hi, query, Present("ms"))
            trigger <- elementWithClass(ui, "p-multiselect")
            _       <- press(trigger, key)
            isOpen  <- open.get
            at      <- hi.get
        yield (isOpen, at)

    "an opening key lands on the first option already selected" in
        opening(UI.Keyboard.ArrowDown, Set("c")).map((open, at) => assert(open && at == 2))

    "and on the first option a highlight may sit on where none is" in {
        for
            down  <- opening(UI.Keyboard.ArrowDown, Set.empty)
            space <- opening(UI.Keyboard.Space, Set.empty)
        yield assert(
            down == (true, 0) && space == (true, 0),
            "opening has to land ON an option, or the arrow that opened the panel moved nothing"
        )
    }

    "Tab closes the panel, in both directions and with a filter header too" in {
        val shift = UI.Modifiers(shift = true)
        for
            (_, _, fwdOpen, fwdUi)   <- panelOf(2)
            fwdHost                  <- keyboardHost(fwdUi, filter = false)
            _                        <- press(fwdHost, UI.Keyboard.Tab)
            fwd                      <- fwdOpen.get
            (_, _, backOpen, backUi) <- panelOf(2)
            backHost                 <- keyboardHost(backUi, filter = false)
            _                        <- press(backHost, UI.Keyboard.Tab, shift)
            back                     <- backOpen.get
            (_, _, filOpen, filUi)   <- panelOf(2, filter = true)
            filHost                  <- keyboardHost(filUi, filter = true)
            _                        <- press(filHost, UI.Keyboard.Tab)
            fil                      <- filOpen.get
        yield assert(!fwd && !back && !fil, "a panel the reader has tabbed away from is one nothing answers")
        end for
    }

    "Ctrl or Cmd with A works the select-all the header shows, since that box is no tab stop" in {
        val ctrl = UI.Modifiers(ctrl = true)
        val cmd  = UI.Modifiers(meta = true)
        for
            (_, value, _, ui)   <- panelOf(0)
            host                <- keyboardHost(ui, filter = false)
            _                   <- press(host, UI.Keyboard.Char('a'), ctrl)
            all                 <- value.get.map(_.map(_.id))
            (_, value2, _, ui2) <- panelOf(0)
            _                   <- value2.set(fruit.filter(f => Set("a", "c", "d").contains(f.id)).toSet)
            host2               <- keyboardHost(ui2, filter = false)
            _                   <- press(host2, UI.Keyboard.Char('a'), cmd)
            cleared             <- value2.get.map(_.map(_.id))
        yield assert(
            all == Set("a", "c", "d") && cleared.isEmpty,
            "every enabled option, and the second press clears them; the sold-out one is never selected"
        )
        end for
    }

    "the select-all box is out of the tab order, so its keys cannot be read twice" in {
        for
            (_, _, _, ui) <- panelOf(2)
            box           <- elementWithClass(ui, "p-checkbox-input")
        yield
            assert(box.attrs.tabIndex.contains(-1), "a key pressed on it would reach the trigger by bubbling")
            assert(box.attrs.identifier.contains("ms-all"), "and that is how the trigger knows one did")
    }

    "a key that came from the select-all box is the box's own, so the option list ignores it" in {
        for
            (_, value, open, ui) <- panelOf(2)
            trigger              <- elementWithClass(ui, "p-multiselect")
            bubbled: (Any < Async) = trigger.attrs.onKeyDown match
                case Present(f) => handled(f(UI.KeyboardEvent(UI.Keyboard.Space, UI.Modifiers.none, Present("ms-all"))))
                case Absent     => ()
            _      <- bubbled
            picked <- value.get.map(_.map(_.id))
            still  <- open.get
        yield assert(picked.isEmpty && still, "the box toggles them all; the highlighted option is not a second answer")
    }

    "ArrowDown from nothing lands on the first option" in after(-1, UI.Keyboard.ArrowDown).map(at => assert(at == 0))

    "ArrowUp from nothing lands on the last" in after(-1, UI.Keyboard.ArrowUp).map(at => assert(at == 3))

    "a sold-out option is stepped over" in after(0, UI.Keyboard.ArrowDown).map(at => assert(at == 2))

    "Home and End reach the ends" in {
        for
            home <- after(3, UI.Keyboard.Home)
            end  <- after(0, UI.Keyboard.End)
        yield assert(home == 0 && end == 3)
    }

    "Enter toggles the highlighted option and leaves the panel open" in {
        for
            (_, value, open, ui) <- panelOf(2)
            host                 <- keyboardHost(ui, filter = false)
            _                    <- press(host, UI.Keyboard.Enter)
            picked               <- value.get.map(_.map(_.id))
            still                <- open.get
        yield assert(picked == Set("c") && still, "a reader picking several things keeps the list")
    }

    "Space toggles it too, where the panel has no text field" in {
        for
            (_, value, _, ui) <- panelOf(2)
            host              <- keyboardHost(ui, filter = false)
            _                 <- press(host, UI.Keyboard.Space)
            picked            <- value.get.map(_.map(_.id))
        yield assert(picked == Set("c"))
    }

    "with a filter header Space belongs to the caret" in {
        for
            (_, value, _, ui) <- panelOf(2, filter = true)
            host              <- keyboardHost(ui, filter = true)
            _                 <- press(host, UI.Keyboard.Space)
            picked            <- value.get.map(_.map(_.id))
        yield assert(picked.isEmpty)
    }

    "Escape closes the panel, since the panel it would close never gets the key" in {
        for
            (_, _, open, ui) <- panelOf(2)
            host             <- keyboardHost(ui, filter = false)
            _                <- press(host, UI.Keyboard.Escape)
            still            <- open.get
        yield assert(!still)
    }

    "a printable key jumps to the option it starts" in
        after(-1, UI.Keyboard.Char('d')).map(at => assert(at == 3))

    "and types instead, once there is a filter header" in
        after(-1, UI.Keyboard.Char('d'), filter = true).map(at => assert(at == -1))

    "the trigger is the combobox, and announces the highlighted option" in {
        for
            (_, _, _, ui) <- panelOf(2)
            trigger       <- elementWithClass(ui, "p-multiselect")
            list          <- elementWithClass(ui, "p-multiselect-list")
            rows          <- elementsWithClass(ui, "p-multiselect-option")
        yield
            assert(trigger.attrs.role.contains("combobox"))
            assert(trigger.attrs.ariaAttrs.get("activedescendant").contains("ms-option-2"))
            assert(trigger.attrs.ariaAttrs.get("controls").contains("ms-list"))
            assert(list.attrs.identifier.contains("ms-list"))
            assert(!list.attrs.ariaAttrs.contains("activedescendant"), "the unfocused list says nothing")
            assert(rows(2).attrs.identifier.contains("ms-option-2"))
    }

    "with a filter header the header's input is the combobox instead" in {
        for
            (_, _, _, ui) <- panelOf(2, filter = true)
            trigger       <- elementWithClass(ui, "p-multiselect")
            filterEl      <- elementWithClass(ui, "p-multiselect-filter")
        yield
            assert(trigger.attrs.role.contains("button"))
            assert(!trigger.attrs.ariaAttrs.contains("activedescendant"))
            assert(filterEl.attrs.role.contains("combobox"))
            assert(filterEl.attrs.focusAuto.contains(true))
            assert(filterEl.attrs.ariaAttrs.get("activedescendant").contains("ms-option-2"))
    }

    /** The multiselect wired with its panel `open`, holding the options keyed `held`. */
    private def field(base: uic.MultiSelect[Fruit], open: Boolean, held: Set[String] = Set.empty, query: String = "")(using
        Frame
    ): (SignalRef[Boolean], SignalRef[Set[Fruit]], UI) < Async =
        for
            o <- Signal.initRef(open)
            h <- Signal.initRef(-1)
            q <- Signal.initRef(query)
            v <- Signal.initRef(fruit.filter(f => held.contains(f.id)).toSet)
        yield (o, v, base.selection(v).wired(o, h, q, Present("ms")))

    private def classed(cls: String)(e: UI.Ast.Element): Boolean = e.attrs.cssClasses.contains(cls)

    private def chips(using Frame) = multi.display(uic.MultiSelectDisplay.Chip).showClear(true)

    "a click anywhere on the field opens it, the element carrying its id and a chip's body included" in {
        for
            (byId, _, ui1)   <- field(multi, open = false)
            _                <- clickBubbling(ui1)(_.attrs.identifier.contains("ms"))
            viaId            <- byId.get
            (byChip, _, ui2) <- field(chips, open = false, held = Set("a", "c"))
            _                <- clickBubbling(ui2)(classed("p-chip-label"))
            viaChip          <- byChip.get
            (shut, _, ui3)   <- field(multi, open = true)
            _                <- clickBubbling(ui3)(_.attrs.identifier.contains("ms"))
            closed           <- shut.get
        yield
            assert(viaId, "the field itself is the control, so a click on its padding or by its id opens it")
            assert(viaChip, "a chip is part of the field's face, as in Prime; only its remove button is its own")
            assert(!closed)
        end for
    }

    "a click on an option toggles it once and leaves the panel open" in {
        for
            (open, value, ui) <- field(multi, open = true)
            _                 <- clickBubbling(ui)(_.attrs.identifier.contains("ms-option-2"))
            picked            <- value.get.map(_.map(_.id))
            still             <- open.get
        yield assert(picked == Set("c") && still, "a reader picking several things keeps the list")
    }

    "a click inside the open panel that lands on no option leaves it open and picks nothing" in {
        val grouped =
            uic.MultiSelect[Fruit]().optionGroups(List(uic.OptionItem.group("Fruit")(fruit*)))(_.name).optionKey(_.id)
                .optionDisabled(_.sold).filterable(true).id("ms")
        val targets: List[(String, UI.Ast.Element => Boolean, String)] = List(
            ("group header", classed("p-multiselect-option-group"), ""),
            ("header", classed("p-multiselect-header"), ""),
            ("filter input", classed("p-multiselect-filter"), ""),
            ("empty list space", classed("p-multiselect-list-container"), ""),
            ("panel frame", classed("p-multiselect-overlay"), ""),
            ("sold-out option", _.attrs.identifier.contains("ms-option-1"), ""),
            ("empty message", classed("p-multiselect-empty-message"), "zzz")
        )
        Kyo.foreach(targets) { (name, target, query) =>
            for
                (open, value, ui) <- field(grouped, open = true, query = query)
                _                 <- clickBubbling(ui)(target)
                still             <- open.get
                got               <- value.get
            yield (name, still && got.isEmpty)
        }.map(rs => assert(rs.forall(_._2), rs.filterNot(_._2).map(_._1).mkString("toggled or picked: ", ", ", "")))
    }

    "a click on the clear button empties the field, and one on a chip's remove button drops that chip, neither opening it" in {
        for
            (open1, value1, ui1) <- field(chips, open = false, held = Set("a", "c"))
            _                    <- clickBubbling(ui1)(classed("p-multiselect-clear-icon"))
            cleared              <- value1.get
            opened1              <- open1.get
            (open2, value2, ui2) <- field(chips, open = false, held = Set("a", "c"))
            _                    <- clickBubbling(ui2)(classed("p-chip-remove-icon"))
            left                 <- value2.get.map(_.map(_.id))
            opened2              <- open2.get
        yield
            assert(cleared.isEmpty && !opened1)
            assert(left == Set("c") && !opened2)
        end for
    }

    "a chip's remove button keeps its keys too, so Enter on it removes the chip without opening the panel" in {
        for
            (_, _, ui) <- field(chips, open = false, held = Set("a", "c"))
            remove     <- elementWithClass(ui, "p-chip-remove-icon")
        yield assert(
            remove.attrs.onKeyDown.nonEmpty && remove.attrs.stopPropagation.contains(true),
            "a key the button declares ends there, so the field's opening keys never see it"
        )
    }

    "the field is the one element listening for a click: not its label, not its chevron" in {
        def listening(ui: UI)(using Frame): Chunk[String] < Sync =
            elements(ui).map(_.filter(declaresClick).map(_.attrs.cssClasses.headOption.getOrElse("?")))
        for
            (_, _, closed) <- field(chips, open = false, held = Set("a", "c"))
            live           <- listening(closed)
            (_, _, off)    <- field(chips.disabled(true), open = false, held = Set("a"))
            disabled       <- listening(off)
        yield
            assert(
                live == Chunk("p-multiselect", "p-chip-remove-icon", "p-chip-remove-icon", "p-multiselect-clear-icon"),
                live.toString
            )
            assert(disabled.isEmpty, s"a disabled field opens nothing and drops no chip: $disabled")
        end for
    }

    "selection binds the options themselves, and a toggle keeps a member no option carries" in {
        val gone = Fruit("z", "Zucchini", false)
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(2)
            query <- Signal.initRef("")
            ref   <- Signal.initRef(Set(fruit(0), gone))
            ui = multi.selection(ref).wired(open, hi, query, Present("ms"))
            rows    <- elementsWithClass(ui, "p-multiselect-option")
            trigger <- elementWithClass(ui, "p-multiselect")
            _       <- press(trigger, UI.Keyboard.Enter)
            got     <- ref.get
        yield
            assert(rows.map(_.attrs.ariaAttrs.getOrElse("selected", "-")) == Chunk("true", "false", "false", "false"))
            assert(got == Set(fruit(0), fruit(2), gone))
        end for
    }

    // ---- filter and filterable ----

    private val users = List(
        MultiSelectTest.User("Ada Lovelace", "ada@engine.org"),
        MultiSelectTest.User("Grace Hopper", "grace@navy.mil"),
        MultiSelectTest.User("Alan Turing", "alan@bletchley.uk")
    )

    private def people(using Frame) = uic.MultiSelect[MultiSelectTest.User]().options(users)(_.name).id("ms")

    /** `base` wired with its panel `open` over the query `query`, and the refs a test reads back. */
    private def search(base: uic.MultiSelect[MultiSelectTest.User], open: Boolean, query: String)(using
        Frame
    ): (SignalRef[Boolean], SignalRef[String], SignalRef[Set[MultiSelectTest.User]], UI) < Async =
        for
            o <- Signal.initRef(open)
            h <- Signal.initRef(-1)
            q <- Signal.initRef(query)
            v <- Signal.initRef(Set.empty[MultiSelectTest.User])
        yield (o, q, v, base.selection(v).wired(o, h, q, Present("ms")))

    private def shownNames(ui: UI)(using Frame): Chunk[String] < Sync =
        elementsWithClass(ui, "p-multiselect-option").map(_.map(textOf))

    private def hasFilter(ui: UI)(using Frame): Boolean < Sync = elementsWithClass(ui, "p-multiselect-filter").map(_.nonEmpty)

    private def roleOf(e: UI.Ast.Element)(using Frame): String < Sync =
        e.attrs.reactiveAttrs.get("role") match
            case Some(sig) => sig.current
            case None      => e.attrs.role.getOrElse("")

    private def boundTo(e: UI.Ast.Element, ref: SignalRef[String]): Boolean =
        e match
            case t: UI.Ast.TextInput =>
                t.value match
                    case Present(UI.Bound.Ref(r)) => r eq ref
                    case _                        => false
            case _ => false

    "a filter searches every text it is given, and select-all takes what it found" in {
        for
            (_, _, held, ui) <- search(people.filter(uic.Filter.contains(_.name, _.email)), open = true, query = "NAVY")
            names            <- shownNames(ui)
            header           <- hasFilter(ui)
            input            <- elementWithClass(ui, "p-multiselect-filter")
            _                <- press(input, UI.Keyboard.Char('a'), UI.Modifiers(ctrl = true))
            got              <- held.get
        yield
            assert(header, "setting a filter turns the header filter on")
            assert(names == Chunk("Grace Hopper"), s"the email is searched too: $names")
            assert(got.map(_.name) == Set("Grace Hopper"), s"select-all applies to what the filter found: $got")
    }

    "without a filter the label alone is searched, by contains" in {
        for
            (_, _, _, byLabel) <- search(people.filterable(true), open = true, query = "OPP")
            labels             <- shownNames(byLabel)
            (_, _, _, byEmail) <- search(people.filterable(true), open = true, query = "navy")
            emails             <- shownNames(byEmail)
        yield
            assert(labels == Chunk("Grace Hopper"))
            assert(emails.isEmpty, s"the email is no part of the default match: $emails")
    }

    "an explicit filterable(false) decides alone, over filter and filterQuery" in {
        for
            q                <- Signal.initRef("navy")
            (_, _, _, ui)    <- search(people.filter(uic.Filter.contains(_.name, _.email)).filterable(false), open = true, query = "navy")
            (_, _, _, byRef) <- search(people.filterable(false).filterQuery(q), open = true, query = "navy")
            names            <- shownNames(ui)
            header           <- hasFilter(ui)
            refNames         <- shownNames(byRef)
            refHeader        <- hasFilter(byRef)
            trigger          <- elementWithClass(ui, "p-multiselect")
            role             <- roleOf(trigger)
        yield
            assert(!header && !refHeader, "no filter input")
            assert(names.size == 3 && refNames.size == 3, s"every option: $names / $refNames")
            assert(role == "combobox", "without a filter the trigger stays the combobox")
    }

    "a filterable signal shows and hides the filter on the open panel, and the query outlives it" in {
        for
            on               <- Signal.initRef(true)
            (open, q, _, ui) <- search(people.filterable(on), open = true, query = "grace")
            headerOn         <- hasFilter(ui)
            namesOn          <- shownNames(ui)
            _                <- on.set(false)
            headerOff        <- hasFilter(ui)
            namesOff         <- shownNames(ui)
            stillOpen        <- open.get
            kept             <- q.get
            _                <- on.set(true)
            headerAgain      <- hasFilter(ui)
            namesAgain       <- shownNames(ui)
            input            <- elementWithClass(ui, "p-multiselect-filter")
        yield
            assert(headerOn && namesOn == Chunk("Grace Hopper"), s"on: filter=$headerOn shown=$namesOn")
            assert(!headerOff && namesOff.size == 3, s"off: no filter and every option, got filter=$headerOff shown=$namesOff")
            assert(stillOpen, "the panel stays open")
            assert(kept == "grace", s"the query is kept, only not applied: $kept")
            assert(headerAgain && namesAgain == Chunk("Grace Hopper"), s"back on, the old query filters at once: $namesAgain")
            assert(boundTo(input, q), "and the filter shows it, since it is bound to the same query")
    }

    "toggling filterable on a closed panel leaves the field alone, and only its role follows in place" in {
        for
            on                 <- Signal.initRef(true)
            (open, q, _, live) <- search(people.filterable(on), open = false, query = "grace")
            (_, _, _, fixed)   <- search(people.filterable(true), open = false, query = "grace")
            fixedRegions       <- regionsAbove(fixed, "p-multiselect")
            regionsOn          <- regionsAbove(live, "p-multiselect")
            roleOn             <- elementWithClass(live, "p-multiselect").map(roleOf)
            _                  <- on.set(false)
            regionsOff         <- regionsAbove(live, "p-multiselect")
            field              <- elementWithClass(live, "p-multiselect")
            roleOff            <- roleOf(field)
            stillClosed        <- open.get
            kept               <- q.get
        yield
            assert(
                regionsOn == fixedRegions && regionsOff == fixedRegions,
                s"no region of the switch over the closed field: $regionsOn / $regionsOff, a constant has $fixedRegions"
            )
            assert(field.attrs.reactiveAttrs.contains("role"), "the role is a channel on the field, patched in place")
            assert(roleOn == "button" && roleOff == "combobox", s"the role follows the switch: $roleOn then $roleOff")
            assert(!stillClosed && kept == "grace")
        end for
    }

    "the wired field is ONE reactive region over everything it reads, open or closed" in {
        for
            on               <- Signal.initRef(true)
            loading          <- Signal.initRef(false)
            ph               <- Signal.initRef("Pick some")
            (open, _, _, ui) <- search(people.filterable(on).loading(loading).placeholder(ph), open = true, query = "")
            whenOpen         <- regionsAbove(ui, "p-multiselect")
            panel            <- regionsAbove(ui, "p-multiselect-list")
            _                <- open.set(false)
            whenClosed       <- regionsAbove(ui, "p-multiselect")
        yield assert(
            whenOpen == 1 && panel == 1 && whenClosed == 1,
            s"a pick writes the value while the panel state moves; a nested region can repaint the older state: $whenOpen / $panel / $whenClosed"
        )
    }

    "opening while the filter is off leaves the query alone" in {
        for
            off                <- Signal.initRef(false)
            (open, q, _, live) <- search(people.filterable(off), open = false, query = "grace")
            trigger            <- elementWithClass(live, "p-multiselect")
            _                  <- press(trigger, UI.Keyboard.ArrowDown)
            opened             <- open.get
            kept               <- q.get
        yield assert(opened && kept == "grace", s"the query is not the panel's to clear while the filter is off ($kept)")
    }

end MultiSelectTest

object MultiSelectTest:
    final case class User(name: String, email: String) derives CanEqual
end MultiSelectTest
