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
  *
  * Its state slots (the selection over `Maybe` and over the option itself, the panel's visibility and the filter query)
  * are held to rule A ([[ReactiveValueContract]]).
  */
class SelectTest extends ReactiveValueContract:

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
            card <- elementsWithClass(ui, "p-uic-diagnostic")
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
            card <- elementWithClass(ui, "p-uic-diagnostic")
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
            cards   <- elementsWithClass(ui, "p-uic-diagnostic")
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
            during  <- elementsWithClass(ui1, "p-uic-diagnostic")
            _       <- loading.set(false)
            ui2     <- opened(select.loading(loading).selection(ref))
            after   <- elementsWithClass(ui2, "p-uic-diagnostic")
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

    // Shown, reported and not written is the contract's (see the selection slots below); this is the app's own write.
    "a one-way selection follows what the app writes" in {
        for
            route <- Signal.initRef(fruit(2))
            _     <- route.set(fruit(3))
            ui    <- opened(select.selection(route.readOnly).onPick(_ => ()))
            lbl   <- elementWithClass(ui, "p-select-label")
        yield assert(textOf(lbl) == "Date", "an app write moves the field")
        end for
    }

    "a selection over a signal derived from a ref reports a pick through onChange, as a paginator's page size does" in {
        for
            size <- Signal.initRef(10)
            seen <- Signal.initRef(List.empty[Maybe[Int]])
            ui   <- opened(
                uic.Select[Int]().options(Seq(10, 20, 50))(_.toString).selection(size.map(math.max(1, _)))
                    .onChange(v => seen.updateAndGet(_ :+ v).unit)
            )
            rs  <- rows(ui)
            _   <- click(rs(1))
            got <- seen.get
        yield assert(got == List(Present(20)), s"onChange saw $got")
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

    /** The select wired with its panel `open`, holding `held`, and the refs a click writes. */
    private def field(base: uic.Select[Fruit], open: Boolean, held: Maybe[Fruit] = Absent, query: String = "")(using
        Frame
    ): (SignalRef[Boolean], SignalRef[Maybe[Fruit]], UI) < Async =
        for
            o <- Signal.initRef(open)
            h <- Signal.initRef(-1)
            q <- Signal.initRef(query)
            v <- Signal.initRef(held)
        yield (o, v, base.selection(v).wired(o, h, q, Present("sel")))

    private def classed(cls: String)(e: UI.Ast.Element): Boolean = e.attrs.cssClasses.contains(cls)

    "a click anywhere on the field opens it, on the element carrying its id as on the label or the chevron" in {
        for
            (byId, _, ui1)    <- field(select, open = false)
            _                 <- clickBubbling(ui1)(_.attrs.identifier.contains("sel"))
            viaId             <- byId.get
            (byLabel, _, ui2) <- field(select, open = false)
            _                 <- clickBubbling(ui2)(classed("p-select-label"))
            viaLabel          <- byLabel.get
            (byIcon, _, ui3)  <- field(select, open = false)
            _                 <- clickBubbling(ui3)(classed("p-select-dropdown"))
            viaChevron        <- byIcon.get
            (shut, _, ui4)    <- field(select, open = true)
            _                 <- clickBubbling(ui4)(_.attrs.identifier.contains("sel"))
            closed            <- shut.get
        yield
            assert(viaId, "the field itself is the control, so a click on its padding or by its id opens it")
            assert(viaLabel && viaChevron, "and a click on what it contains reaches it")
            assert(!closed, "a second click closes it again")
        end for
    }

    "a click on an option picks it once and leaves the panel closed" in {
        for
            seen              <- Signal.initRef(0)
            (open, value, ui) <- field(select.onChange(_ => seen.updateAndGet(_ + 1).unit), open = true)
            _                 <- clickBubbling(ui)(e => classed("p-select-option-label")(e) && textOf(e) == "Cherry")
            got               <- value.get
            changes           <- seen.get
            still             <- open.get
        yield assert(got.map(_.id) == Present("c") && changes == 1 && !still)
    }

    "an outside click closes the panel at the backdrop and goes no further" in {
        for
            heard          <- Signal.initRef(0)
            (open, _, sel) <- field(select, open = true)
            // A container around the field that listens too: the backdrop's close is the whole answer to an outside
            // click, so neither the field's toggle nor anything above it may hear it as a second one.
            ui = UI.div.onClick(heard.updateAndGet(_ + 1).unit)(sel)
            _     <- clickBubbling(ui)(classed("p-uic-overlay-backdrop"))
            still <- open.get
            above <- heard.get
        yield
            assert(!still, "the backdrop closes the panel")
            assert(above == 0, s"the click ends at the backdrop, but $above handler(s) above it heard it")
    }

    "a click inside the open panel that lands on no option leaves it open and picks nothing" in {
        val grouped =
            uic.Select[Fruit]().optionGroups(List(uic.OptionItem.group("Fruit")(fruit*)))(_.name).optionKey(_.id)
                .optionDisabled(_.sold).filterable(true).id("sel")
        val targets: List[(String, UI.Ast.Element => Boolean, String)] = List(
            ("group header", classed("p-select-option-group"), ""),
            ("filter input", classed("p-select-filter"), ""),
            ("empty list space", classed("p-select-list-container"), ""),
            ("panel frame", classed("p-select-overlay"), ""),
            ("sold-out option", e => classed("p-select-option-label")(e) && textOf(e) == "Banana", ""),
            ("empty message", classed("p-select-empty-message"), "zzz")
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

    "a click on the clear button empties the field and does not open it" in {
        for
            (open, value, ui) <- field(select.showClear(true), open = false, held = Present(fruit(0)))
            _                 <- clickBubbling(ui)(classed("p-select-clear-icon"))
            got               <- value.get
            opened            <- open.get
        yield assert(got.isEmpty && !opened)
    }

    "the field is the one element listening for a click: not its label, not its chevron" in {
        def listening(ui: UI)(using Frame): Chunk[String] < Sync =
            elements(ui).map(_.filter(declaresClick).map(_.attrs.cssClasses.headOption.getOrElse("?")))
        for
            (_, _, closed) <- field(select.showClear(true), open = false, held = Present(fruit(0)))
            live           <- listening(closed)
            (_, _, off)    <- field(select.disabled(true), open = false)
            disabled       <- listening(off)
            (_, _, ro)     <- field(select.readonly(true), open = false)
            readonly       <- listening(ro)
        yield
            assert(live == Chunk("p-select", "p-select-clear-icon"), live.toString)
            assert(disabled.isEmpty && readonly.isEmpty, "a disabled or readonly field opens nothing")
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

    // ---- filter and filterable ----

    private val users = List(
        SelectTest.User("Ada Lovelace", "ada@engine.org"),
        SelectTest.User("Grace Hopper", "grace@navy.mil"),
        SelectTest.User("Alan Turing", "alan@bletchley.uk")
    )

    private def people(using Frame) = uic.Select[SelectTest.User]().options(users)(_.name).id("sel")

    /** `base` wired with its panel `open` over the query `query`, and the refs a test reads back. */
    private def search(base: uic.Select[SelectTest.User], open: Boolean, query: String)(using
        Frame
    ): (SignalRef[Boolean], SignalRef[String], UI) < Async =
        for
            o <- Signal.initRef(open)
            h <- Signal.initRef(-1)
            q <- Signal.initRef(query)
        yield (o, q, base.wired(o, h, q, Present("sel")))

    private def shownNames(ui: UI)(using Frame): Chunk[String] < Sync = rows(ui).map(_.map(textOf))

    private def hasHeader(ui: UI)(using Frame): Boolean < Sync = elementsWithClass(ui, "p-select-header").map(_.nonEmpty)

    /** The role an element carries: its channel's current value where it has one, its static role otherwise. */
    private def roleOf(e: UI.Ast.Element)(using Frame): String < Sync =
        e.attrs.reactiveAttrs.get("role") match
            case Some(sig) => sig.current
            case None      => e.attrs.role.getOrElse("")

    /** Whether `e` is a text input bound two-way to `ref`. */
    private def boundTo(e: UI.Ast.Element, ref: SignalRef[String]): Boolean =
        e match
            case t: UI.Ast.TextInput =>
                t.value match
                    case Present(UI.Bound.Ref(r)) => r eq ref
                    case _                        => false
            case _ => false

    "a filter searches every text it is given, so a user is found by email" in {
        for
            (_, _, ui) <- search(people.filter(uic.Filter.contains(_.name, _.email)), open = true, query = "NAVY.mil")
            names      <- shownNames(ui)
            header     <- hasHeader(ui)
        yield
            assert(header, "setting a filter turns the header filter on")
            assert(names == Chunk("Grace Hopper"), s"the email is searched too: $names")
    }

    "without a filter the label alone is searched, by contains" in {
        for
            (_, _, byLabel) <- search(people.filterable(true), open = true, query = "OPP")
            labels          <- shownNames(byLabel)
            (_, _, byEmail) <- search(people.filterable(true), open = true, query = "navy")
            emails          <- shownNames(byEmail)
        yield
            assert(labels == Chunk("Grace Hopper"))
            assert(emails.isEmpty, s"the email is no part of the default match: $emails")
    }

    "an explicit filterable(false) decides alone, before or after filter" in {
        val matcher = uic.Filter.contains[SelectTest.User](_.name, _.email)
        for
            (_, _, after)  <- search(people.filter(matcher).filterable(false), open = true, query = "navy")
            (_, _, before) <- search(people.filterable(false).filter(matcher), open = true, query = "navy")
            namesAfter     <- shownNames(after)
            namesBefore    <- shownNames(before)
            headerAfter    <- hasHeader(after)
            headerBefore   <- hasHeader(before)
            trigger        <- elementWithClass(after, "p-select")
            role           <- roleOf(trigger)
        yield
            assert(!headerAfter && !headerBefore, "no header")
            assert(namesAfter == Chunk("Ada Lovelace", "Grace Hopper", "Alan Turing"), s"every option: $namesAfter")
            assert(namesBefore == namesAfter, s"in either order: $namesBefore")
            assert(role == "combobox", "without a header the trigger stays the combobox")
        end for
    }

    "an explicit filterable(false) wins over filterQuery too" in {
        for
            q          <- Signal.initRef("grace")
            (_, _, ui) <- search(people.filterQuery(q).filterable(false), open = true, query = "grace")
            names      <- shownNames(ui)
            header     <- hasHeader(ui)
        yield assert(!header && names.size == 3, s"header=$header shown=$names")
    }

    "a filterable signal shows and hides the header on the open panel, and the query outlives it" in {
        for
            on            <- Signal.initRef(true)
            (open, q, ui) <- search(people.filterable(on), open = true, query = "grace")
            headerOn      <- hasHeader(ui)
            namesOn       <- shownNames(ui)
            _             <- on.set(false)
            headerOff     <- hasHeader(ui)
            namesOff      <- shownNames(ui)
            stillOpen     <- open.get
            kept          <- q.get
            _             <- on.set(true)
            headerAgain   <- hasHeader(ui)
            namesAgain    <- shownNames(ui)
            input         <- elementWithClass(ui, "p-select-filter")
        yield
            assert(headerOn && namesOn == Chunk("Grace Hopper"), s"on: header=$headerOn shown=$namesOn")
            assert(!headerOff && namesOff.size == 3, s"off: no header and every option, got header=$headerOff shown=$namesOff")
            assert(stillOpen, "the panel stays open")
            assert(kept == "grace", s"the query is kept, only not applied: $kept")
            assert(headerAgain && namesAgain == Chunk("Grace Hopper"), s"back on, the old query filters at once: $namesAgain")
            assert(boundTo(input, q), "and the header shows it, since it is bound to the same query")
    }

    "toggling filterable on a closed panel leaves the field alone, and only its role follows in place" in {
        for
            on              <- Signal.initRef(true)
            (open, q, live) <- search(people.filterable(on), open = false, query = "grace")
            (_, _, fixed)   <- search(people.filterable(true), open = false, query = "grace")
            fixedRegions    <- regionsAbove(fixed, "p-select")
            regionsOn       <- regionsAbove(live, "p-select")
            roleOn          <- elementWithClass(live, "p-select").map(roleOf)
            _               <- on.set(false)
            regionsOff      <- regionsAbove(live, "p-select")
            field           <- elementWithClass(live, "p-select")
            roleOff         <- roleOf(field)
            stillClosed     <- open.get
            kept            <- q.get
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
            on            <- Signal.initRef(true)
            loading       <- Signal.initRef(false)
            ph            <- Signal.initRef("Pick one")
            value         <- Signal.initRef(Maybe.empty[SelectTest.User])
            (open, _, ui) <- search(people.filterable(on).loading(loading).placeholder(ph).selection(value), open = true, query = "")
            whenOpen      <- regionsAbove(ui, "p-select")
            panel         <- regionsAbove(ui, "p-select-list")
            _             <- open.set(false)
            whenClosed    <- regionsAbove(ui, "p-select")
        yield assert(
            whenOpen == 1 && panel == 1 && whenClosed == 1,
            s"a pick writes the value and closes the panel; a nested region can repaint the older state: $whenOpen / $panel / $whenClosed"
        )
    }

    "opening while the filter is off leaves the query alone; opening while it is on starts a fresh one" in {
        for
            off              <- Signal.initRef(false)
            (open1, q1, ui1) <- search(people.filterable(off), open = false, query = "grace")
            t1               <- elementWithClass(ui1, "p-select")
            _                <- press(t1, UI.Keyboard.ArrowDown)
            opened1          <- open1.get
            kept             <- q1.get
            (open2, q2, ui2) <- search(people.filterable(true), open = false, query = "grace")
            t2               <- elementWithClass(ui2, "p-select")
            _                <- press(t2, UI.Keyboard.ArrowDown)
            opened2          <- open2.get
            fresh            <- q2.get
        yield
            assert(opened1 && kept == "grace", s"off: the query is not the panel's to clear ($kept)")
            assert(opened2 && fresh == "", s"on: a fresh query per open, as before ($fresh)")
    }

    "a click on an option the opening paint shows picks it, though the panel's own mount has not started yet" in {
        import kyo.internal.*
        // The opening click writes the panel's state, and the field's one region takes it up on a fiber of its own. A click
        // sent the moment the opening click returns lands before, during or after that repaint, whichever the scheduler
        // gives; the test below holds the one during it, and these rounds sweep the rest.
        val rounds                                                                        = 25
        def round(fieldPath: Seq[String], optionPath: Seq[String]): Maybe[String] < Async =
            for
                value <- Signal.initRef(Present(fruit.head): Maybe[Fruit])
                live  <- Promise.init[Unit, Any]
                cmds  <- UI.Commands.init(_ => Kyo.unit)
                ui = select.selection(value).render
                // The first paint is the field's mount publishing the live field, the one a click can open.
                exchange = new RecordingExchange(beforePaint = (_, _) => live.completeUnitDiscard)
                v <- Scope.run(UICommands.commands.let(Present(cmds)) {
                    for
                        root <- ReactiveUI.normalize(ui, Seq.empty)
                        sub  <- ReactiveUI.subscribe(root, exchange)
                        _    <- live.get
                        _    <- sub.handle(fieldPath, clickAt(fieldPath))
                        _    <- sub.handle(optionPath, clickAt(optionPath))
                        v    <- value.get
                    yield v
                })
            yield v.map(_.name)
        end round
        for
            closed         <- HtmlRenderer.render(select.selection(Present(fruit.head): Maybe[Fruit]).render, Seq.empty)
            (_, _, opened) <- field(select, open = true)
            shown          <- HtmlRenderer.render(opened, Seq.empty)
            picks          <- Kyo.foreach(1 to rounds)(_ => round(pathOf(closed, "sel"), pathOf(shown, "sel-option-2")))
        yield
            val missed = picks.count(_ != Present("Cherry"))
            assert(missed == 0, s"$missed of $rounds clicks on Cherry left the field elsewhere: ${picks.distinct}")
        end for
    }

    "a click on an option while the opening click's repaint is running picks it" in {
        import kyo.internal.*
        // The opening click writes the panel's state, and the field's one region takes it up on a fiber of its own. Here that
        // repaint is paused with its paints still waiting, and the click on the option arrives during it.
        for
            value  <- Signal.initRef(Present(fruit.head): Maybe[Fruit])
            live   <- Promise.init[Unit, Any]
            paused <- Promise.init[Unit, Any]
            letGo  <- Promise.init[Unit, Any]
            cmds   <- UI.Commands.init(_ => Kyo.unit)
            armed = new java.util.concurrent.atomic.AtomicBoolean(false)
            ui    = select.selection(value).render
            // The first paint is the field's mount publishing the live field, the one a click can open.
            exchange = new RecordingExchange(beforePaint = (_, _) => live.completeUnitDiscard)
            closed         <- HtmlRenderer.render(select.selection(Present(fruit.head): Maybe[Fruit]).render, Seq.empty)
            (_, _, opened) <- field(select, open = true)
            shown          <- HtmlRenderer.render(opened, Seq.empty)
            fieldPath  = pathOf(closed, "sel")
            optionPath = pathOf(shown, "sel-option-2")
            picked <- Scope.run(BoundaryHold.duringPass.let(at =>
                // The field's region sits at the root; the panel's measuring trigger has passes of its own, further down.
                if at.isEmpty && armed.compareAndSet(true, false) then paused.completeUnitDiscard.andThen(letGo.get) else Kyo.unit
            )(UICommands.commands.let(Present(cmds)) {
                for
                    root <- ReactiveUI.normalize(ui, Seq.empty)
                    sub  <- ReactiveUI.subscribe(root, exchange)
                    _    <- live.get
                    _    <- Sync.defer(armed.set(true))
                    _    <- sub.handle(fieldPath, clickAt(fieldPath))
                    _    <- paused.get
                    _    <- sub.handle(optionPath, clickAt(optionPath))
                    v    <- value.get
                    _    <- letGo.completeUnitDiscard
                yield v
            }))
        yield assert(picked.map(_.name) == Present("Cherry"), s"the click on Cherry left the field at $picked")
        end for
    }

    // ---- rule A ----

    private def bare(using Frame) = uic.Select[Fruit]().options(fruit)(_.name).optionKey(_.id)

    /** The trigger, which opens the panel and carries its keyboard: what a reader reaches, and what says it is fixed. */
    private def trigger(t: UI)(using Frame): UI.Ast.Element < Sync = elementWithClass(t, "p-select")

    /** Whether the trigger answers the reader at all: a click opens the panel, a key opens it or steers it. */
    private def opens(c: UI.Ast.Element): Boolean = declaresClick(c) || c.attrs.onKeyDown.nonEmpty

    private def readOnly(c: UI.Ast.Element): Boolean = c.attrs.ariaAttrs.get("readonly").contains("true")

    private def triggered(t: UI)(using Frame): Boolean < Sync = elementsWithClass(t, "p-select").map(_.exists(declaresClick))

    /** The option the label shows, by name. */
    private def picked(t: UI)(using Frame): Maybe[Fruit] < Sync =
        elementWithClass(t, "p-select-label").map(l => Maybe.fromOption(fruit.find(_.name == textOf(l))))

    /** A click on the row naming `f` in the panel the trigger opened. */
    private def pickRow(f: Fruit)(t: UI)(using Frame): Any < Async =
        clickBubbling(t)((e: UI.Ast.Element) => e.attrs.cssClasses.contains("p-select-option") && textOf(e) == f.name)

    private def maybeSlot(
        name: String,
        hear: (uic.Select[Fruit], Maybe[Fruit] => UI.Action) => Frame ?=> uic.Select[Fruit],
        event: String
    ) =
        new SlotCase[uic.Select[Fruit], Maybe[Fruit]](
            name = name,
            bare = Present(bare),
            bind = v => bare.selection(v),
            hear = hear,
            event = event,
            render = _.render,
            start = Absent,
            next = Present(fruit(1)),
            control = trigger(_),
            shows = picked(_),
            answers = opens,
            saysFixed = readOnly,
            ready = Present(triggered(_)),
            after = Seq(pickRow(fruit(1))(_))
        )

    // A pick is reported to onChange, then to onPick: each of them hears it.
    holds(maybeSlot("selection over Maybe", (s, f) => s.onChange(f), "onChange"))
    heardBy(maybeSlot("selection over Maybe, heard by onPick", (s, f) => s.onPick(a => f(Present(a))), "onPick"))

    private def valueSlot(name: String, hear: (uic.Select[Fruit], Fruit => UI.Action) => Frame ?=> uic.Select[Fruit], event: String) =
        new SlotCase[uic.Select[Fruit], Fruit](
            name = name,
            // A field over the option itself always holds one: unbound, a select holds `Maybe`.
            bare = Absent,
            bind = v => bare.selection(v),
            hear = hear,
            event = event,
            render = _.render,
            start = fruit(0),
            next = fruit(1),
            control = trigger(_),
            shows = t => picked(t).map(_.getOrElse(throw new AssertionError("the field shows no option"))),
            answers = opens,
            saysFixed = readOnly,
            ready = Present(triggered(_)),
            after = Seq(pickRow(fruit(1))(_))
        )

    holds(valueSlot("selection", (s, f) => s.onPick(f), "onPick"))
    heardBy(valueSlot(
        "selection, heard by onChange",
        (s, f) => s.onChange(m => m.fold(throw new AssertionError("a field over the option reported none"))(f)),
        "onChange"
    ))

    "a selection a ref holds, or one a disabled field shows, draws no card" in {
        def errors(s: uic.Select[String]): Chunk[UI.Ast.Element] < Async = elementsWithClass(s.render, "p-uic-diagnostic")
        def base(using Frame)                                            = uic.Select[String]().options(Seq("a", "b"))
        for
            held   <- Signal.initRef("a")
            twoWay <- errors(base.selection(held))
            off    <- errors(base.selection(held.readOnly).disabled(true))
        yield assert(twoWay.isEmpty && off.isEmpty, s"the ref takes the pick, or nobody can pick: ${(twoWay ++ off).map(textOf)}")
        end for
    }

    "a fixed select stays reachable: it keeps its tab stop" in {
        for
            field <- trigger(bare.selection(fruit(0)).render)
        yield assert(field.attrs.tabIndex.contains(0), "a reader can still land on it")
    }

    holds(new SlotCase[uic.Select[Fruit], Boolean](
        name = "open",
        bare = Present(bare),
        bind = bare.open(_),
        hear = (s, f) => s.onOpenChange(f),
        event = "onOpenChange",
        render = _.render,
        start = false,
        next = true,
        control = trigger(_),
        shows = elementsWithClass(_, "p-select-overlay").map(_.nonEmpty),
        answers = opens,
        saysFixed = readOnly,
        ready = Present(triggered(_))
    ))

    /** The select as its mount publishes it, the trigger wired. */
    private def mounted(ui: UI)(using Frame, kyo.test.AssertScope): UI < Async =
        mountedWhen(ui)(t => elementsWithClass(t, "p-select").map(_.nonEmpty))

    // A panel open by a constant somebody hears, bound one way or to a ref still closes on a click: the reader asks, and the
    // owner decides.
    heardBy(new SlotCase[uic.Select[Fruit], Boolean](
        name = "open, from open",
        bare = Absent,
        bind = bare.open(_),
        hear = (s, f) => s.onOpenChange(f),
        event = "onOpenChange",
        render = _.render,
        start = true,
        next = false,
        control = trigger(_),
        shows = elementsWithClass(_, "p-select-overlay").map(_.nonEmpty),
        answers = opens,
        ready = Present(triggered(_))
    ))

    // The combobox has no readonly state for whether its list is open, so a panel fixed open says nothing beyond
    // aria-expanded: it is fixed because nothing on the field pretends to close it, while the rows stay the reader's.
    "a panel fixed open stays: nothing outside dismisses it, and the field answers no click that would close it" in {
        for
            t    <- mountedWhen(bare.open(true).render)(elementsWithClass(_, "p-select-overlay").map(_.nonEmpty))
            pane <- elementsWithClass(t, "p-select-overlay")
            out  <- elementsWithClass(t, "p-uic-overlay-backdrop")
            tr   <- trigger(t)
        yield
            assert(pane.nonEmpty, "the panel shows")
            assert(out.isEmpty, "no backdrop takes an outside click")
            assert(!declaresClick(tr), "a click on the field would close nothing, so the field takes none")
            assert(tr.attrs.tabIndex.contains(0) && tr.attrs.onKeyDown.nonEmpty, "the field stays the tab stop with the rows' keyboard")
            assert(tr.attrs.ariaAttrs.get("expanded").contains("true"), "and says the list is open")
            assert(!readOnly(tr), "the selection is still the reader's, so the field is not read-only")
        end for
    }

    // The query is a text the select keeps itself, unbound and from a constant, since it draws the panel from it. Its field
    // is in the panel, which only the mount draws open: the panel is held open here, so the query is the slot under test.
    holds(new SlotCase[uic.Select[Fruit], String](
        name = "filterQuery",
        bare = Present(bare.open(true).filterable(true)),
        bind = bare.open(true).filterQuery(_),
        hear = (s, f) => s.onFilterQueryChange(f),
        event = "onFilterQueryChange",
        render = _.render,
        start = "",
        next = "ban",
        control = textField(_, "p-select-filter"),
        shows = t => textField(t, "p-select-filter").map(textIn),
        gesture = (c: UI.Ast.Element) => typed(c, "ban", commit = false),
        answers = editable,
        saysFixed = {
            case t: UI.Ast.TextInput => t.readOnly.contains(true)
            case _                   => false
        },
        ready = Present(elementsWithClass(_, "p-select-filter").map(_.nonEmpty)),
        startText = Present("ch"),
        mountDraws = true
    ))

    private def shownRows(t: UI)(using Frame): Chunk[String] < Sync = elementsWithClass(t, "p-select-option").map(_.map(textOf))

    "a query bound one way filters the panel by what the signal holds" in {
        for
            held <- Signal.initRef("ap")
            t    <- mounted(bare.open(true).filterQuery(held.readOnly).onFilterQueryChange(_ => ()).render)
            rows <- shownRows(t)
        yield assert(rows == Chunk("Apple"), s"the panel filters by the signal: $rows")
    }

    "a constant query filters the panel from the start" in {
        for
            t    <- mounted(bare.open(true).filterQuery("ch").render)
            rows <- shownRows(t)
        yield assert(rows == Chunk("Cherry"), s"the panel starts filtered: $rows")
    }

    // A reader who cannot act has nothing to hear: a disabled or readonly field neither opens nor takes a query.
    "a field the reader cannot act on says nothing about a panel or a query bound one way" in {
        for
            open  <- Signal.initRef(false)
            query <- Signal.initRef("")
            cases = Seq(
                ("disabled, open", bare.disabled(true).open(open.readOnly)),
                ("disabled, query", bare.disabled(true).filterQuery(query.readOnly)),
                ("readonly, open", bare.readonly(true).open(open.readOnly)),
                ("readonly, query", bare.readonly(true).filterQuery(query.readOnly))
            )
            said <- Kyo.foreach(cases)((name, c) => elementsWithClass(c.render, UicTest.cardClass).map(cs => (name, cs.map(textOf))))
        yield said.foreach((name, cards) => assert(cards.isEmpty, s"$name: $cards"))
        end for
    }

end SelectTest

object SelectTest:
    enum Tone derives CanEqual:
        case Warm, Cool

    final case class User(name: String, email: String) derives CanEqual

end SelectTest
