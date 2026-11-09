package kyo.uic

import kyo.*
import kyo.UI.*

class ControlledTest extends UicTest:

    /** The four bindings a slot can hold, with what each answers: writable, a constant, a signal. */
    private def bindings(using Frame): Seq[(String, Maybe[Prop[Int]], Boolean, Boolean, Boolean)] =
        Seq(
            ("nothing", Absent, false, false, false),
            ("a constant", Present(1), false, true, false),
            ("a signal", Present(Signal.initConst(1)), false, false, true)
        )

    private val hearerF: Int => UI.Action = _ => ()

    "the truth table" - {
        "of heard, fixed and deaf over nothing, a constant and a signal, by hearer, by another event and by quiet" in {
            for
                (name, binding, writable, constant, signal) <- bindings
                hearer                                      <- Seq(false, true)
                elsewhere                                   <- Seq(false, true)
                quiet                                       <- Seq(false, true)
            do
                val s     = Controlled[Int](binding, if hearer then Present(hearerF) else Absent, elsewhere, quiet)
                val where = s"$name, hearer $hearer, elsewhere $elsewhere, quiet $quiet"
                val heard = writable || hearer || elsewhere
                assert(s.heard == heard, s"heard: $where")
                assert(s.fixed == (constant && !heard), s"fixed: $where")
                assert(s.deaf == (signal && !heard && !quiet), s"deaf: $where")
            end for
            succeed
        }

        "a ref is heard by itself, is never fixed and never deaf" in {
            Signal.initRef(1).map { ref =>
                val s = Controlled[Int]().bind(ref)
                assert(s.heard)
                assert(!s.fixed)
                assert(!s.deaf)
            }
        }

        "a read-only view of a ref is a signal, not a ref" in {
            Signal.initRef(1).map { ref =>
                val s = Controlled[Int]().bind(ref.readOnly)
                assert(!s.heard)
                assert(s.deaf)
            }
        }

        "fixedAt says fixed at that value, and nothing else" in {
            val s = Controlled[Int]().bind(1)
            assert(s.fixedAt(1))
            assert(!s.fixedAt(2))
            assert(!s.hear(hearerF).fixedAt(1))
            assert(!Controlled[Int]().fixedAt(1))
        }
    }

    "bind" - {
        "takes the runtime class of the value, as ReactiveValue does" in {
            Signal.initRef(1).map { ref =>
                assert(Controlled[Int]().bind(1).binding.exists(_.const.contains(1)))
                assert(Controlled[Int]().bind(ref.readOnly).binding.exists(b => b.dyn.isDefined && b.writable.isEmpty))
                assert(Controlled[Int]().bind(ref).binding.exists(_.writable.isDefined))
            }
        }

        "keeps who hears it" in {
            val s = Controlled[Int]().hear(hearerF).bind(1)
            assert(s.hearer.isDefined)
        }
    }

    "change" - {
        "writes a ref before it reports" in {
            for
                ref  <- Signal.initRef(0)
                seen <- Signal.initRef(Chunk.empty[(Int, Int)])
                s = Controlled[Int]().bind(ref).hear(v => ref.get.map(h => seen.updateAndGet(_.append((v, h))).unit))
                _   <- s.change(3)
                got <- seen.get
            yield assert(got == Chunk((3, 3)))
        }

        "a signal is not written, and the hearer hears what the reader asked for" in {
            for
                ref  <- Signal.initRef(0)
                seen <- Signal.initRef(Chunk.empty[Int])
                s = Controlled[Int]().bind(ref.readOnly).hear(v => seen.updateAndGet(_.append(v)).unit)
                _    <- s.change(3)
                got  <- seen.get
                held <- ref.get
            yield
                assert(got == Chunk(3))
                assert(held == 0)
        }

        "unbound, the hearer hears it" in {
            for
                seen <- Signal.initRef(Chunk.empty[Int])
                _    <- Controlled[Int]().hear(v => seen.updateAndGet(_.append(v)).unit).change(3)
                got  <- seen.get
            yield assert(got == Chunk(3))
        }

        "a second hear replaces the first: one hearer, as every setter in this module" in {
            for
                first  <- Signal.initRef(0)
                second <- Signal.initRef(0)
                s = Controlled[Int]().hear(v => first.set(v)).hear(v => second.set(v))
                _ <- s.change(3)
                a <- first.get
                b <- second.get
            yield
                assert(a == 0)
                assert(b == 3)
        }
    }

    "update" - {
        "steps a ref atomically: steps that interleave lose nothing" in {
            for
                ref <- Signal.initRef(0)
                s = Controlled[Int]().bind(ref)
                _    <- Async.fill(50, 50)(s.update(_ + 1))
                held <- ref.get
            yield assert(held == 50)
        }

        "steps a constant and a signal from the value shown, writes nothing, and reports the result" in {
            for
                ref  <- Signal.initRef(5)
                seen <- Signal.initRef(Chunk.empty[Int])
                hear = (v: Int) => seen.updateAndGet(_.append(v)).unit
                _    <- Controlled[Int]().bind(10).hear(hear).update(_ + 1)
                _    <- Controlled[Int]().bind(ref.readOnly).hear(hear).update(_ + 1)
                got  <- seen.get
                held <- ref.get
            yield
                assert(got == Chunk(11, 6))
                assert(held == 5)
        }

        "unbound does nothing and reports nothing: there is nothing to step from" in {
            for
                seen <- Signal.initRef(Chunk.empty[Int])
                _    <- Controlled[Int]().hear(v => seen.updateAndGet(_.append(v)).unit).update(_ + 1)
                got  <- seen.get
            yield assert(got.isEmpty)
        }
    }

    "what a render reads" - {
        "reading stands in for nothing bound, follows a signal and a ref, and stays fixed over a constant" in {
            Signal.initRef(7).map { ref =>
                Controlled[Int]().reading(1) match
                    case ReactiveValue.Const(v) => assert(v == 1)
                    case other                  => throw new AssertionError(s"nothing bound reads as $other")
                Controlled[Int]().bind(2).reading(1) match
                    case ReactiveValue.Const(v) => assert(v == 2)
                    case other                  => throw new AssertionError(s"a constant reads as $other")
                Controlled[Int]().bind(ref).reading(1) match
                    case ReactiveValue.Dyn(sig) => sig.current.map(v => assert(v == 7))
                    case other                  => throw new AssertionError(s"a ref reads as $other")
            }
        }

        "current reads the binding at this moment, or what stands in" in {
            for
                ref <- Signal.initRef(7)
                a   <- Controlled[Int]().current(1)
                b   <- Controlled[Int]().bind(2).current(1)
                c   <- Controlled[Int]().bind(ref).current(1)
                _   <- ref.set(8)
                d   <- Controlled[Int]().bind(ref).current(1)
            yield assert((a, b, c, d) == (1, 2, 7, 8))
        }

        "value is the binding, else where the slot starts, else what stands in" in {
            Signal.initRef(7).map { ref =>
                assert(Controlled[Int]().value(1).equals(1))
                assert(Controlled[Int](start = Present(3)).value(1).equals(3))
                assert(Controlled[Int](start = Present(3)).bind(2).value(1).equals(2))
                assert(Controlled[Int](start = Present(3)).bind(ref).value(1).equals(ref))
            }
        }

        "bound gives the binding back as the caller gave it" in {
            Signal.initRef(7).map { ref =>
                assert(Controlled[Int]().bound.isEmpty)
                assert(Controlled[Int]().bind(2).bound.exists(_.equals(2)))
                assert(Controlled[Int]().bind(ref).bound.exists(_.equals(ref)))
            }
        }
    }

    "the entry points" - {
        val name = Controlled.Name("X", "the thing", "a press", "onThing")
        val cls  = "p-slot-body"

        def body(s: Controlled[Int])(using Frame): UI =
            s.reading(-1).render(v =>
                UI.div.cssClass(cls).cssClass(if s.binding.exists(_.writable.isDefined) then "own" else "other")(v.toString)
            )

        def shown(ui: UI)(using Frame): Chunk[String] < Sync = elementsWithClass(ui, cls).map(_.map(textOf))

        "owned mints a ref for an unbound slot, and leaves anything bound" in {
            for
                own   <- Controlled[Int]().owned(3)
                stays <- Controlled[Int]().bind(1).owned(3)
                held  <- own.current(-1)
            yield
                assert(own.binding.exists(_.writable.isDefined))
                assert(held == 3)
                assert(stays.fixedAt(1))
        }

        "ownedFrom takes a constant as where it starts" in {
            for
                own  <- Controlled[Int]().bind(5).ownedFrom(3)
                none <- Controlled[Int]().ownedFrom(3)
                sig  <- Controlled[Int]().bind(Signal.initConst(7)).ownedFrom(3)
                a    <- own.current(-1)
                b    <- none.current(-1)
            yield
                assert(own.binding.exists(_.writable.isDefined))
                assert((a, b) == (5, 3))
                assert(sig.binding.exists(b => b.dyn.isDefined && b.writable.isEmpty))
        }

        "owning mounts where nothing is bound, not where something is, and not where the switch is constant off" in {
            for
                own   <- mountsAbove(Controlled.owning(Controlled[Int](), 0)(body), cls)
                bound <- mountsAbove(Controlled.owning(Controlled[Int]().bind(1), 0)(body), cls)
                off   <- mountsAbove(Controlled.owning(Controlled[Int](), 0, ReactiveValue.Const(false))(body), cls)
            yield
                assert(own == 1)
                assert(bound == 0)
                assert(off == 0)
        }

        "owning mounts under a signal switch whatever it holds now, so the state outlives it being off" in {
            mountsAbove(Controlled.owning(Controlled[Int](), 0, ReactiveValue.Dyn(Signal.initConst(false)))(body), cls)
                .map(n => assert(n == 1))
        }

        "owningFrom mints from a constant too" in {
            for
                first <- shown(Controlled.owningFrom(Controlled[Int]().bind(5), 0)(body))
                n     <- mountsAbove(Controlled.owningFrom(Controlled[Int]().bind(5), 0)(body), cls)
                off   <- mountsAbove(
                    Controlled.owningFrom(Controlled[Int]().bind(5), 0, ReactiveValue.Const(false))(body),
                    cls
                )
            yield
                assert(first == Chunk("5"))
                assert(n == 1)
                assert(off == 0)
        }

        "draw shows the body over the state it allocated, with no card" in {
            for
                page  <- shown(Controlled.draw(Controlled[Int](), name, 4)(body))
                cards <- elementsWithClass(Controlled.draw(Controlled[Int](), name, 4)(body), UicTest.cardClass)
            yield
                assert(page == Chunk("4"))
                assert(cards.isEmpty)
        }

        "draw puts the card of a slot nobody hears in front, only while the switch is on" in {
            for
                sw <- Signal.initRef(true)
                deaf = Controlled[Int]().bind(Signal.initConst(1))
                ui   = Controlled.draw(deaf, name, 0, ReactiveValue.Dyn(sw))(body)
                on    <- elementsWithClass(ui, UicTest.cardClass)
                _     <- sw.set(false)
                off   <- elementsWithClass(ui, UicTest.cardClass)
                _     <- sw.set(true)
                back  <- elementsWithClass(ui, UicTest.cardClass)
                still <- elementsWithClass(Controlled.draw(deaf, name, 0, ReactiveValue.Const(false))(body), UicTest.cardClass)
            yield
                assert(on.size == 1)
                assert(off.isEmpty)
                assert(back.size == 1)
                assert(still.isEmpty)
        }

        "drawField draws each binding the same way" in {
            def field(s: Controlled[Int])(using Frame): Controlled.Field[Int] < Sync =
                def walk(ui: UI): UI < Sync = ui match
                    case r: UI.Ast.Reactive[?] => r.signal.current(using r.frame).map(walk)
                    case other                 => other
                val cap = new scala.collection.mutable.ArrayBuffer[Controlled.Field[Int]]
                walk(Controlled.drawField(s, -1)(f =>
                    cap += f; UI.div
                )).map(_ => cap.last)
            end field
            for
                ref   <- Signal.initRef(2)
                two   <- field(Controlled[Int]().bind(ref))
                one   <- field(Controlled[Int]().bind(ref.readOnly))
                heard <- field(Controlled[Int]().bind(5).hear(_ => ()))
                fixed <- field(Controlled[Int]().bind(5))
                none  <- field(Controlled[Int]())
            yield
                assert(two.value == 2 && two.native.equals(2) && two.edit.isDefined && !two.fixed)
                assert(one.value == 2 && !one.native.equals(2) && one.edit.isEmpty && !one.fixed)
                assert(heard.value == 5 && !heard.native.equals(5) && heard.edit.isDefined && !heard.fixed)
                assert(fixed.value == 5 && fixed.native.equals(5) && fixed.edit.isEmpty && fixed.fixed)
                assert(none.value == -1 && none.native.equals(-1) && none.edit.isEmpty && !none.fixed)
            end for
        }

        // A component that mints its state beside its ids holds it in that one mount: a slot of its own around it would be a
        // mount inside a mount for every instance. Each row is such a component, with its slots unbound and its feature on.
        "what mints in a mount it already has adds no mount" - {
            val inOneMount: Seq[(String, Frame ?=> UI, String, Int)] = Seq(
                ("SpeedDial", uic.SpeedDial().items(uic.MenuItem("a")).render, "p-speeddial", 1),
                ("SplitButton", uic.SplitButton("a").items(uic.MenuItem("b")).render, "p-splitbutton", 1),
                ("Menu", uic.Menu().popup().items(uic.MenuItem("a")).render, "p-menu-list", 1),
                ("TieredMenu", uic.TieredMenu().popup().items(uic.MenuItem("a")).render, "p-tieredmenu-root-list", 1),
                ("Select", uic.Select[String]().options(Seq("a"))(identity).render, "p-select", 1),
                ("MultiSelect", uic.MultiSelect[String]().options(Seq("a"))(identity).render, "p-multiselect", 1),
                (
                    "CascadeSelect",
                    uic.CascadeSelect[String]().options(Seq.empty[uic.CascadeItem[String]])(identity).render,
                    "p-cascadeselect",
                    1
                ),
                ("TreeSelect", uic.TreeSelect[String]().render, "p-treeselect", 1),
                ("DatePicker", uic.DatePicker[java.time.LocalDate]().render, "p-datepicker", 1),
                ("Listbox", uic.Listbox[String]().selectionMode(uic.SelectionMode.Single).render, "p-listbox", 1),
                ("Tabs", uic.Tabs[String]().tab("a", "a")(UI.empty).render, "p-tabs", 1),
                ("Accordion", uic.Accordion[String]().panel("a", "a")(UI.empty).render, "p-accordion", 1),
                ("Stepper", uic.Stepper[String]().render, "p-stepper", 1),
                ("Galleria", uic.Galleria().render, "p-galleria", 1),
                ("Tree", uic.Tree[String]().selectionMode(uic.SelectionMode.Single).render, "p-tree", 1),
                ("TreeTable", uic.TreeTable[String]().selectionMode(uic.SelectionMode.Single).render, "p-treetable", 1),
                (
                    "OrganizationChart",
                    uic.OrganizationChart[String]().expandable(true).selectionMode(uic.SelectionMode.Single).render,
                    "p-organizationchart",
                    1
                ),
                ("OrderList", uic.OrderList[String]().items(Seq("a"))(identity).render, "p-orderlist", 1),
                ("PickList", uic.PickList[String]().render, "p-picklist", 1),
                ("Paginator", uic.Paginator().render, "p-paginator", 1)
            )
            inOneMount.foreach { (name, ui, cls, mounts) =>
                s"$name" in mountsAbove(ui, cls).map(n => assert(n == mounts, s"$name is under $n mounts"))
            }
        }

        "mapNative maps a value and a signal" in {
            for
                ref <- Signal.initRef(2)
                v = Controlled.mapNative[Int, Int](3)(_ + 1)
                m = Controlled.mapNative[Int, Int](ref.readOnly)(_ + 1)
                a <- m.asInstanceOf[Signal[Int]].current
            yield
                assert(v.equals(4))
                assert(a == 3)
        }
    }
end ControlledTest
