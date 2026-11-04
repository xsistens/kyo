package kyo.uic

import kyo.*
import kyo.UI.*

/** `id(...)` means "stamp this element's id", and the menu family plus `Tabs` used it as a BASE
  * only: the id a caller gave never reached the document, and the sole id that existed was the
  * derived `s"$id-active"` on whichever row the keyboard had highlighted (GAPS.md F-34).
  *
  * `HasElementId`'s own contract says what these owe (*"a container or display component puts it
  * on its own root"*), and `Panel` next door already does both halves. These pin the two halves
  * together, because they pull against each other: the components MINT a base when no id was
  * given, so stamping "the base" on the root would put a machine-made name in the document for
  * every menu that was never named. The caller's id and the part base are two values now, and
  * both cases below have to hold at once.
  */
class ElementIdTest extends UicTest:

    private val rows = List(
        uic.MenuItem("New").onSelect(()),
        uic.MenuItem("Search").onSelect(())
    )

    private def menu(using Frame) = uic.Menu().items(rows*)

    /** The id actually stamped on the element carrying `cls`. */
    private def idOf(ui: UI, cls: String)(using Frame): Maybe[String] < Sync =
        elementWithClass(ui, cls).map(_.attrs.identifier)

    "an inline Menu carries the id it was given, on its own root" in {
        for
            hi <- Anchored.init(1)
            ui = menu.id("mine").wired(hi, "minted")
            root <- idOf(ui, "p-menu")
            list <- elementWithClass(ui, "p-menu-list")
        yield
            assert(root == Present("mine"))
            // …and the derived part still comes off the same value, which is the pull: one
            // setter has to answer both without either answer displacing the other.
            assert(list.attrs.ariaAttrs.get("activedescendant").contains("mine-active"))
    }

    "a Menu given NO id puts no minted name in the document, and still announces" in {
        for
            hi <- Anchored.init(1)
            ui = menu.wired(hi, "minted")
            root <- idOf(ui, "p-menu")
            list <- elementWithClass(ui, "p-menu-list")
            row  <- elementWithClass(ui, "p-focus")
        yield
            assert(root.isEmpty, s"the mount's minted base must not become an element id, got $root")
            assert(list.attrs.ariaAttrs.get("activedescendant").contains("minted-active"))
            assert(row.attrs.identifier.contains("minted-active"))
    }

    "a popup Menu carries it on the list, the element that IS the menu" in {
        // The component's root in a popup is the overlay's panel, which the menu does not own.
        for
            open <- Signal.initRef(true)
            hi   <- Anchored.init(1)
            ui = menu.id("mine").popup(open).wired(hi, "minted")
            list <- idOf(ui, "p-menu-list")
        yield assert(list == Present("mine"))
    }

    "a Menubar carries it on its root" in {
        val bar = uic.Menubar().items(uic.MenuItem("File").items(rows*), uic.MenuItem("Edit"))
        for
            refs  <- Submenus.init(submenuPaths(bar.s.entriesV).filter(p => false).toSet)
            focus <- Anchored.init(List.empty[Int])
            ui   = bar.id("mine").wired(refs, focus, "minted")
            bare = bar.wired(refs, focus, "minted")
            named   <- idOf(ui, "p-menubar")
            unnamed <- idOf(bare, "p-menubar")
        yield
            assert(named == Present("mine"))
            assert(unnamed.isEmpty)
        end for
    }

    "a TieredMenu carries it on its root" in {
        val tm = uic.TieredMenu().items(uic.MenuItem("File").items(rows*), uic.MenuItem("Edit"))
        for
            refs  <- Submenus.init(submenuPaths(tm.s.entriesV).filter(p => false).toSet)
            focus <- Anchored.init(List.empty[Int])
            ui   = tm.id("mine").wired(refs, focus, "minted")
            bare = tm.wired(refs, focus, "minted")
            named   <- idOf(ui, "p-tieredmenu")
            unnamed <- idOf(bare, "p-tieredmenu")
        yield
            assert(named == Present("mine"))
            assert(unnamed.isEmpty)
        end for
    }

    "a MegaMenu carries it on its root" in {
        val mm = uic.MegaMenu().items(
            uic.MegaMenuItem("Furniture").column(uic.MenuGroup("Living").items(uic.MenuItem("Chairs"))),
            uic.MegaMenuItem("Contact")
        )
        for
            refs  <- Submenus.init(panelPaths(mm.s.entriesV).filter(p => false).toSet)
            focus <- Anchored.init(List.empty[Int])
            ui   = mm.id("mine").wired(refs, focus, "minted")
            bare = mm.wired(refs, focus, "minted")
            named   <- idOf(ui, "p-megamenu")
            unnamed <- idOf(bare, "p-megamenu")
        yield
            assert(named == Present("mine"))
            assert(unnamed.isEmpty)
        end for
    }

    "Tabs carries it on its root, alongside the headers derived from it" in {
        val strip = uic.Tabs[String]().tabs(
            uic.Tab("One", p("first"), "one"),
            uic.Tab("Two", p("second"), "two")
        )
        for
            ui = strip.id("mine").wired("mine", _ => ())
            root    <- idOf(ui, "p-tabs")
            headers <- elementsWithClass(ui, "p-tab")
        yield
            assert(root == Present("mine"))
            // The derived name is a different string, which is why the two uses never collided:
            // only one of them was ever rendered.
            assert(headers.map(_.attrs.identifier).toSeq == Seq(Present("mine-one"), Present("mine-two")))
        end for
    }

    // ==== Field-shaped controls: the id lands on the element that takes focus ====
    //
    // `HasElementId`'s other half. `Label.forId`, `Commands.focusById` and the form layer's
    // focus-first-invalid all hand the id to the browser as "the thing to focus", so an id on a
    // span inside the control is an id nothing can use: the label points at text, the submit
    // focuses nothing, and an app's `#id` highlight covers the label alone.

    /** Whether the browser would put focus on `e`: a native control not taken out of the tab
      * order, or any element given a tab stop.
      */
    private def focusable(e: UI.Ast.Element): Boolean =
        e.attrs.tabIndex match
            case Present(i) => i >= 0
            case Absent     => e.isInstanceOf[UI.Ast.Focusable]

    /** The element carrying `mine`, which must be focusable. */
    private def owner(ui: UI)(using Frame, kyo.test.AssertScope): UI.Ast.Element < Sync =
        elementWithId(ui, "mine").map { e =>
            assert(focusable(e), s"the element carrying the id must take focus, got ${e.attrs.cssClasses} tabIndex=${e.attrs.tabIndex}")
            e
        }

    final private case class Fruit(id: String, name: String) derives CanEqual
    private val fruit = List(Fruit("a", "Apple"), Fruit("b", "Banana"))

    "a Select carries it on the combobox, live and in the static projection" in {
        val sel = uic.Select[Fruit]().options(fruit)(_.name).optionKey(_.id).id("mine")
        for
            open  <- Signal.initRef(true)
            hi    <- Signal.initRef(0)
            q     <- Signal.initRef("")
            live  <- owner(sel.wired(open, hi, q, Present("mine")))
            stat  <- owner(sel.render)
            filt  <- owner(sel.filterable(true).wired(open, hi, q, Present("mine")))
            label <- elementWithClass(sel.wired(open, hi, q, Present("mine")), "p-select-label")
        yield
            assert(live.attrs.role.contains("combobox") && live.attrs.cssClasses.contains("p-select"))
            assert(stat.attrs.cssClasses.contains("p-select"))
            assert(filt.attrs.cssClasses.contains("p-select"), "with a filter header the trigger is still where Tab lands")
            assert(label.attrs.identifier.isEmpty, "the label text is not the control")
            // The parts derived from the id still come off the same value.
            assert(live.attrs.ariaAttrs.get("controls").contains("mine-list"))
            assert(live.attrs.ariaAttrs.get("activedescendant").contains("mine-option-0"))
        end for
    }

    "a CascadeSelect carries it on the combobox" in {
        val cs = uic.CascadeSelect[String]().options(List(uic.CascadeItem.leaf("Bern"), uic.CascadeItem.leaf("Rome")))(identity).id("mine")
        for
            open  <- Signal.initRef(false)
            focus <- Anchored.init(List.empty[Int])
            subs  <- Submenus.init()
            live  <- owner(cs.wired(open, subs, focus, Present("mine")))
            stat  <- owner(cs.render)
        yield
            assert(live.attrs.role.contains("combobox") && live.attrs.cssClasses.contains("p-cascadeselect"))
            assert(live.attrs.ariaAttrs.get("controls").exists(_.startsWith("mine")))
            assert(stat.attrs.cssClasses.contains("p-cascadeselect"))
        end for
    }

    "a MultiSelect carries it on the combobox" in {
        val ms = uic.MultiSelect[Fruit]().options(fruit)(_.name).optionKey(_.id).id("mine")
        for
            open <- Signal.initRef(true)
            hi   <- Signal.initRef(0)
            q    <- Signal.initRef("")
            live <- owner(ms.wired(open, hi, q, Present("mine")))
            stat <- owner(ms.render)
        yield
            assert(live.attrs.role.contains("combobox") && live.attrs.cssClasses.contains("p-multiselect"))
            assert(live.attrs.ariaAttrs.get("controls").contains("mine-list"))
            assert(stat.attrs.cssClasses.contains("p-multiselect"))
        end for
    }

    "a TreeSelect carries it on the combobox, and derives its rows from it" in {
        val ts = uic.TreeSelect[String]().nodes(TreeNode("src", "src"), TreeNode("build", "build")).id("mine")
        for
            open <- Signal.initRef(true)
            exp  <- Signal.initRef(Set.empty[String])
            hi   <- Signal.initRef(0)
            live <- owner(ts.wired(open, exp, hi, "mine"))
            stat <- owner(ts.render)
        yield
            assert(live.attrs.role.contains("combobox") && live.attrs.cssClasses.contains("p-treeselect"))
            assert(live.attrs.ariaAttrs.get("activedescendant").contains(uic.Tree.rowId("mine", 0)))
            assert(stat.attrs.cssClasses.contains("p-treeselect"))
        end for
    }

    "a Listbox carries it on the list, the one tab stop it roves from" in {
        val lb = uic.Listbox[String]().item("Paris", "par").item("Rome", "rom").selectionMode(uic.SelectionMode.Single).id("mine")
        for
            hi   <- Signal.initRef(0)
            live <- owner(lb.resolved(Set.empty, "", Present(hi), 0))
        yield
            assert(live.attrs.role.contains("listbox"))
            assert(live.attrs.ariaAttrs.get("activedescendant").exists(_.startsWith("mine")))
        end for
    }

    "a Listbox's page as first rendered already makes the list its one tab stop" in {
        // Before the client attaches there is no highlight to rove, and the rows used to hold the
        // tab stops then: the id sat on a list nothing could focus, so a `Label.forId` or a
        // focus-by-id reached nothing until the mount had published.
        def lb(using Frame) =
            uic.Listbox[String]().item("Paris", "par").item("Rome", "rom").selectionMode(uic.SelectionMode.Single).id("mine")
        for
            sel  <- Signal.initRef(Set("rom"))
            stat <- owner(lb.render)
            held <- owner(lb.selection(sel).render)
            rows <- elementsWithClass(lb.selection(sel).render, "p-listbox-option")
        yield
            assert(stat.attrs.role.contains("listbox") && held.attrs.role.contains("listbox"))
            assert(rows.size == 2 && rows.forall(_.attrs.tabIndex.isEmpty), "no row is a second tab stop beside the list")
        end for
    }

    "a Rating's page as first rendered carries it on the radio a Tab reaches" in {
        for
            ref  <- Signal.initRef(3)
            stat <- owner(uic.Rating().value(ref).id("mine").render)
            none <- owner(uic.Rating().id("mine").render)
        yield
            assert(stat.attrs.ariaAttrs.get("label").contains("3 stars"))
            assert(none.attrs.ariaAttrs.get("label").contains("1 star"))
        end for
    }

    /** Every element of `ui` a Tab can land on. Radios that share a `name` are one stop, as the browser groups them. */
    private def tabStops(ui: UI)(using Frame): Chunk[UI.Ast.Element] < Sync =
        elements(ui).map { es =>
            val stops                                     = es.filter(focusable)
            def groupOf(e: UI.Ast.Element): Maybe[String] = e match
                case r: UI.Ast.Radio => r.name
                case _               => Absent
            val (named, loose) = stops.partition(groupOf(_).isDefined)
            loose ++ Chunk.from(named.groupBy(groupOf).values.map(_.head))
        }

    "a radio group as first rendered is ONE tab stop, on the element carrying the id" in {
        // Before the client attaches, a single-choice SelectButton made every option a tab stop and
        // a Rating every star (its radios have no name until the mount mints one), so Tab walked the
        // reader through the whole group, where the attached render is one stop at the chosen option.
        for
            med    <- Signal.initRef(Maybe("Medium"))
            none   <- Signal.initRef(Maybe.empty[String])
            three  <- Signal.initRef(3)
            sbHeld <- tabStops(uic.SelectButton[String]().options(Seq("Small", "Medium", "Large")).selection(med).id("mine").render)
            sbNone <- tabStops(
                uic.SelectButton[String]().options(Seq(
                    "Small",
                    "Medium",
                    "Large"
                )).optionDisabled(_ == "Small").selection(none).id("mine").render
            )
            rtHeld <- tabStops(uic.Rating().value(three).id("mine").render)
            rtNone <- tabStops(uic.Rating().id("mine").render)
        yield
            assert(sbHeld.map(textOf) == Chunk("Medium"), "the chosen option")
            assert(sbNone.map(textOf) == Chunk("Medium"), "nothing chosen: the first option a reader can reach")
            assert(rtHeld.map(_.attrs.ariaAttrs.get("label")) == Chunk(Some("3 stars")), "the checked star")
            assert(rtNone.map(_.attrs.ariaAttrs.get("label")) == Chunk(Some("1 star")), "none checked: the first")
            assert(
                Chunk(sbHeld, sbNone, rtHeld, rtNone).forall(_.map(_.attrs.identifier) == Chunk(Present("mine"))),
                "and that one stop is the element carrying the id"
            )
        end for
    }

    "a single SelectButton carries it on its tab stop, and arrows still reach every option" in {
        val sizes = Seq("Small", "Medium", "Large")
        val ids   = "sb"
        for
            value <- Signal.initRef(Maybe("Medium"))
            moved <- Signal.initRef(List.empty[String])
            ui = uic.SelectButton[String]().options(sizes).selection(value).id("mine")
                .wired(ids, id => moved.updateAndGet(_ :+ id).unit)
            live <- owner(ui)
            opts <- elementsWithClass(ui, "p-togglebutton")
            _    <- press(opts(1), UI.Keyboard.ArrowRight)
            went <- moved.get
            root <- elementWithClass(ui, "p-selectbutton")
            none <- Signal.initRef(Maybe.empty[String])
            stat <- owner(uic.SelectButton[String]().options(sizes).selection(none).id("mine").render)
            med  <- Signal.initRef(Maybe("Medium"))
            held <- owner(uic.SelectButton[String]().options(sizes).selection(med).id("mine").render)
        yield
            assert(textOf(held) == "Medium", "before the client attaches the id is on the chosen option too")
            assert(live.attrs.ariaAttrs.get("checked").contains("true"), "the selected option is the group's tab stop")
            assert(opts.map(_.attrs.identifier) == Chunk(Present("sb-0"), Present("mine"), Present("sb-2")))
            assert(went == List("sb-2"))
            assert(root.attrs.identifier.isEmpty)
            assert(textOf(stat) == "Small", "with nothing chosen the first option is where Tab lands")
        end for
    }

    "a multiple SelectButton carries it on the first option a reader can reach" in {
        for
            value <- Signal.initRef(Set.empty[String])
            ui = uic.SelectButton[String]().options(Seq("Small", "Medium")).multiple
                .optionDisabled(_ == "Small").selection(value).id("mine").render
            live <- owner(ui)
        yield assert(textOf(live) == "Medium")
    }

    "a Rating carries it on the radio that holds the tab stop" in {
        for
            ref  <- Signal.initRef(3)
            live <- owner(uic.Rating().value(ref).id("mine").wired("r"))
            zero <- owner(uic.Rating().value(0).id("mine").wired("r"))
            root <- elementWithClass(uic.Rating().value(ref).id("mine").wired("r"), "p-rating")
        yield
            assert(live.attrs.ariaAttrs.get("label").contains("3 stars"), "the checked star, which a radio group tabs to")
            assert(zero.attrs.ariaAttrs.get("label").contains("1 star"), "with none checked, the first")
            assert(root.attrs.identifier.isEmpty)
        end for
    }

    "a ColorPicker carries it on the swatch, or inline on the plane" in {
        for
            ref    <- Signal.initRef("#ff0000")
            open   <- Signal.initRef(false)
            opened <- Signal.initRef("#ff0000")
            swatch <- owner(uic.ColorPicker().value(ref).id("mine").wired(uic.ColorPicker.Panel(open, opened), Present(ref), "#ff0000"))
            stat   <- owner(uic.ColorPicker().value(ref).id("mine").render)
            plane  <- owner(uic.ColorPicker().inline(true).value(ref).id("mine").render)
        yield
            assert(swatch.attrs.cssClasses.contains("p-colorpicker-preview"))
            assert(stat.attrs.cssClasses.contains("p-colorpicker-preview"))
            assert(plane.attrs.ariaAttrs.get("label").contains("Saturation and brightness"))
        end for
    }

    "a live DatePicker keeps the caller's id on its input, and derives its parts from it" in {
        import java.time.LocalDate
        import kyo.uic.DateKind.given
        for
            oref    <- Signal.initRef(true)
            mref    <- Signal.initRef("2026-07")
            cur     <- Signal.initRef("2026-07-09")
            vref    <- Signal.initRef(uic.DatePickerView.Date)
            seed    <- Signal.initRef(false)
            restore <- Signal.initRef(Maybe.empty[uic.DatePicker.Restore])
            draft   <- Signal.initRef(Maybe.empty[String])
            held    <- Signal.initRef(Maybe.empty[LocalDate])
            refs = uic.DatePicker.Refs(oref, uic.DatePicker.ShownMonth.Own(mref), cur, vref, seed, restore, draft)
            ui   = uic.DatePicker[LocalDate]().value(held).id("mine").wired(refs, "mine", _ => ())
            live <- owner(ui)
            grid <- elementsWithClass(ui, "p-datepicker-day-view")
        yield
            assert(live.attrs.cssClasses.contains("p-datepicker-input"))
            assert(grid.flatMap(_.attrs.identifier.toList) == Chunk("mine-grid"))
        end for
    }

    "a FileUpload's input stays in the tab order: hidden from sight, not from the keyboard" in {
        for
            live <- owner(uic.FileUpload().id("mine").render)
        yield
            assert(live.attrs.cssClasses.contains("p-hidden-accessible"))
            assert(
                uic.Theme.primeExtraCss.contains(".p-fileupload input[type='file'].p-hidden-accessible"),
                "the sheet's display:none on the file input is overridden, or the hidden-accessible class changes nothing"
            )
        end for
    }

    "every other field-shaped control already carries it on its focusable element" in {
        val cases: List[(String, UI)] = List(
            "Input"        -> uic.Input().id("mine").render,
            "TextArea"     -> uic.TextArea().id("mine").render,
            "InputMask"    -> uic.InputMask().id("mine").render,
            "InputNumber"  -> uic.InputNumber().id("mine").render,
            "Password"     -> uic.Password().id("mine").render,
            "InputOtp"     -> uic.InputOtp().id("mine").render,
            "AutoComplete" -> uic.AutoComplete[String]().id("mine").render,
            "Knob"         -> uic.Knob().id("mine").render,
            "Slider"       -> uic.Slider().id("mine").render,
            "CheckBox"     -> uic.CheckBox().id("mine").render,
            "RadioButton"  -> uic.RadioButton().id("mine").render,
            "ToggleSwitch" -> uic.ToggleSwitch().id("mine").render,
            "ToggleButton" -> uic.ToggleButton().id("mine").render,
            "DatePicker"   -> uic.DatePicker[java.time.LocalDate]().id("mine").render
        )
        Kyo.foreach(cases)((name, ui) => elementWithId(ui, "mine").map(e => name -> focusable(e))).map { got =>
            assert(got.filterNot(_._2).isEmpty, s"not focusable: ${got.filterNot(_._2).map(_._1)}")
        }
    }

end ElementIdTest
