package kyo

import kyo.internal.HtmlRenderer
import kyo.internal.ReactiveUI
import kyo.internal.RecordingExchange
import kyo.internal.RecordingExchange.Op
import kyo.internal.RecordingExchange.Sink
import kyo.internal.UIEvent

/** `UI.Bound.Follow`: an element shows a read-only signal in place, through an attribute channel, with no region around it. */
class UIBoundFollowTest extends kyo.test.Test[Any]:

    private val at = Seq("0")

    /** Waits until the channel observing `ref` is parked on it. */
    private def parked(ref: SignalRef[?])(using Frame, kyo.test.AssertScope): Unit < Async =
        assertEventually(ref.waiters.map(_ == 1))

    "Bound.apply" - {
        "a SignalRef binds two way, whatever its static type" in {
            Signal.initRef("a").map { ref =>
                assert(UI.Bound[String](ref: Signal[String]).isInstanceOf[UI.Bound.Ref[?]])
            }
        }
        "any other signal is followed" in {
            Signal.initRef("a").map { ref =>
                assert(UI.Bound[String](ref.map(identity)).isInstanceOf[UI.Bound.Follow[?]])
                assert(UI.Bound[String](ref.readOnly).isInstanceOf[UI.Bound.Follow[?]])
            }
        }
        "a value is a constant" in {
            assert(UI.Bound[String]("a") match
                case UI.Bound.Const(v) => v == "a";
                case _                 => false)
        }
    }

    "delivery" - {
        "a write to the signal is one attribute patch and no paint, from the channel's fiber without a synchronous sink" in {
            val rec = new RecordingExchange
            for
                ref <- Signal.initRef("a")
                ops <- ReactiveUI.subscribed(UI.div(UI.input.value(ref.readOnly)), rec) { _ =>
                    parked(ref).andThen(ref.set("b")).andThen(rec.awaitOp(_ => true)).andThen(parked(ref)).andThen(rec.ops)
                }
            yield assert(ops == Chunk[Op](Op.Attr(at, "value", "b", Sink.Fiber)))
            end for
        }

        "with a synchronous sink the patch lands inside the writer's set" in {
            val rec = new RecordingExchange(synchronous = true)
            for
                ref <- Signal.initRef("a")
                ops <- ReactiveUI.subscribed(UI.div(UI.input.value(ref.readOnly)), rec)(_ => ref.set("b").andThen(rec.ops))
            yield assert(ops == Chunk[Op](Op.Attr(at, "value", "b", Sink.Now)))
            end for
        }

        "a value the page already shows is not patched again" in {
            val rec = new RecordingExchange(synchronous = true)
            for
                ref <- Signal.initRef("a")
                ops <- ReactiveUI.subscribed(UI.div(UI.input.value(ref.readOnly)), rec)(_ => rec.ops)
            yield assert(ops.isEmpty)
            end for
        }

        "every kind of field follows through the value channel" in {
            def carries(el: Signal[String] => UI)(using Frame, kyo.test.AssertScope): Unit < (Async & Abort[Closed]) =
                val rec = new RecordingExchange(synchronous = true)
                Signal.initRef("a").map(ref =>
                    ReactiveUI.subscribed(UI.div(el(ref.readOnly)), rec)(_ => ref.set("b").andThen(rec.attrPatches)).map(ops =>
                        assert(ops.map(op => (op.path, op.name, op.value)) == Chunk((at, "value", "b")))
                    )
                )
            end carries
            carries(UI.textarea.value(_))
                .andThen(carries(UI.passwordInput.value(_)))
                .andThen(carries(UI.emailInput.value(_)))
                .andThen(carries(UI.telInput.value(_)))
                .andThen(carries(UI.urlInput.value(_)))
                .andThen(carries(UI.searchInput.value(_)))
                .andThen(carries(UI.numberInput.value(_)))
                .andThen(carries(UI.dateInput.value(_)))
                .andThen(carries(UI.timeInput.value(_)))
                .andThen(carries(UI.colorInput.value(_)))
                .andThen(carries(UI.hiddenInput.value(_)))
                .andThen(carries(UI.select.value(_)))
        }

        "a checkbox and a radio follow through the checked channel" in {
            def carries(el: Signal[Boolean] => UI)(using Frame, kyo.test.AssertScope): Unit < (Async & Abort[Closed]) =
                val rec = new RecordingExchange(synchronous = true)
                Signal.initRef(false).map(ref =>
                    ReactiveUI.subscribed(UI.div(el(ref.readOnly)), rec)(_ => ref.set(true).andThen(rec.boolPatches)).map(ops =>
                        assert(ops.map(op => (op.path, op.name, op.value)) == Chunk((at, "checked", true)))
                    )
                )
            end carries
            carries(UI.checkbox.checked(_)).andThen(carries(UI.radio.checked(_)))
        }

        "a mask formats what the patch carries" in {
            val rec = new RecordingExchange(synchronous = true)
            for
                ref <- Signal.initRef("1234")
                ops <- ReactiveUI.subscribed(UI.div(UI.input.inputMask("99-99").value(ref.readOnly)), rec) { _ =>
                    ref.set("3456").andThen(rec.attrPatches)
                }
            yield assert(ops.map(_.value) == Chunk("34-56"))
            end for
        }

        "a range carries its number as the render writes it" in {
            val rec = new RecordingExchange(synchronous = true)
            for
                ref <- Signal.initRef(1.0)
                ops <- ReactiveUI.subscribed(UI.div(UI.rangeInput.value(ref.readOnly)), rec)(_ => ref.set(2.5).andThen(rec.attrPatches))
            yield assert(ops.map(_.value) == Chunk("2.5"))
            end for
        }

        "closing the scope releases the channel" in {
            val rec = new RecordingExchange
            for
                ref <- Signal.initRef("a")
                _   <- ReactiveUI.subscribed(UI.div(UI.input.value(ref.readOnly)), rec)(_ => parked(ref))
                w   <- ref.waiters
            yield assert(w == 0)
            end for
        }
    }

    "render" - {
        "follow writes what a constant of the same value writes, and forwards the commit" in {
            for
                ref <- Signal.initRef("v")
                on  <- HtmlRenderer.render(UI.input.value(ref.readOnly), Seq.empty)
                off <- HtmlRenderer.render(UI.input.value("v"), Seq.empty)
            yield assert(on.replace(""" data-kyo-ev="change"""", "") == off, s"$on against $off")
            end for
        }

        "a box stays marked as the render's to set" in {
            for
                ref <- Signal.initRef(true)
                on  <- HtmlRenderer.render(UI.checkbox.checked(ref.readOnly), Seq.empty)
                off <- HtmlRenderer.render(UI.checkbox.checked(true), Seq.empty)
            yield
                assert(on.contains("data-kyo-bound") && on.contains(" checked"))
                assert(on.replace(""" data-kyo-ev="change"""", "") == off)
            end for
        }

        "a textarea writes the value as its content" in {
            Signal.initRef("v").map(ref =>
                HtmlRenderer.render(UI.textarea.value(ref.readOnly), Seq.empty).map(html => assert(html.contains("v</textarea>")))
            )
        }

        "the setters take the three kinds and keep the chain of the element" in {
            for
                ref <- Signal.initRef(false)
                el = UI.checkbox.checked(ref.readOnly).id("c")
            yield
                val same: UI.Ast.Checkbox = el
                assert(same.attrs.identifier == Present("c"))
            end for
        }
    }

    "an edit its owner did not take" - {
        def inputEvent(v: String)  = UIEvent.Input(at, v)
        def changeEvent(v: String) = UIEvent.Change(at, v)

        "a keystroke is put back when onInput is set" in {
            val rec = new RecordingExchange
            for
                ref <- Signal.initRef("held")
                ops <- ReactiveUI.subscribed(UI.div(UI.input.value(ref.readOnly).onInput(_ => ())), rec) { sub =>
                    sub.handle(at, inputEvent("typed")).andThen(rec.attrPatches)
                }
            yield assert(ops.map(op => (op.path, op.name, op.value)) == Chunk((at, "value", "held")))
            end for
        }

        "a keystroke the owner took sends the emission's patch and no assert" in {
            val rec = new RecordingExchange
            for
                ref <- Signal.initRef("held")
                ops <- ReactiveUI.subscribed(UI.div(UI.input.value(ref.readOnly).onInput(ref.set(_))), rec) { sub =>
                    parked(ref).andThen(sub.handle(at, inputEvent("typed"))).andThen(rec.awaitOp(_ => true)).andThen(parked(ref))
                        .andThen(rec.attrPatches)
                }
            yield assert(ops.map(_.value) == Chunk("typed"))
            end for
        }

        "an onChange-only field keeps what is typed until the commit, which is put back" in {
            val rec = new RecordingExchange
            for
                ref <- Signal.initRef("held")
                r   <- ReactiveUI.subscribed(UI.div(UI.input.value(ref.readOnly).onChange(_ => ())), rec) { sub =>
                    for
                        _     <- sub.handle(at, inputEvent("typed"))
                        typed <- rec.attrPatches
                        _     <- sub.handle(at, changeEvent("typed"))
                        done  <- rec.attrPatches
                    yield (typed, done)
                }
            yield
                assert(r._1.isEmpty)
                assert(r._2.map(_.value) == Chunk("held"))
            end for
        }

        "a field with no handler at all is put back at the commit" in {
            val rec = new RecordingExchange
            for
                ref <- Signal.initRef("held")
                ops <- ReactiveUI.subscribed(UI.div(UI.input.value(ref.readOnly)), rec) { sub =>
                    sub.handle(at, changeEvent("typed")).andThen(rec.attrPatches)
                }
            yield assert(ops.map(_.value) == Chunk("held"))
            end for
        }

        "a handler that fails does not keep the edit" in {
            val rec = new RecordingExchange
            for
                ref <- Signal.initRef("held")
                ops <- ReactiveUI.subscribed(UI.div(UI.input.value(ref.readOnly).onInput(_ => Abort.fail("declined"))), rec) { sub =>
                    sub.handle(at, inputEvent("typed")).andThen(rec.attrPatches)
                }
            yield assert(ops.map(_.value) == Chunk("held"))
            end for
        }

        "a mask is compared as the field shows it" in {
            val rec = new RecordingExchange
            for
                ref <- Signal.initRef("1234")
                r   <- ReactiveUI.subscribed(UI.div(UI.input.inputMask("99-99").value(ref.readOnly).onInput(_ => ())), rec) { sub =>
                    for
                        _     <- sub.handle(at, inputEvent("12-34"))
                        same  <- rec.attrPatches
                        _     <- sub.handle(at, inputEvent("12-3"))
                        other <- rec.attrPatches
                    yield (same, other)
                }
            yield
                assert(r._1.isEmpty, s"what the field shows is what the owner holds: ${r._1}")
                assert(r._2.map(_.value) == Chunk("12-34"))
            end for
        }

        "a range is put back with the number the render writes" in {
            val rec = new RecordingExchange
            for
                ref <- Signal.initRef(1.5)
                ops <- ReactiveUI.subscribed(UI.div(UI.rangeInput.value(ref.readOnly).onChange(_ => ())), rec) { sub =>
                    sub.handle(at, UIEvent.ChangeNumeric(at, 3.0)).andThen(rec.attrPatches)
                }
            yield assert(ops.map(_.value) == Chunk("1.5"))
            end for
        }

        "a box and a radio are put back through the checked channel" in {
            def declined(el: Signal[Boolean] => UI)(using Frame, kyo.test.AssertScope): Unit < (Async & Abort[Closed]) =
                val rec = new RecordingExchange
                Signal.initRef(false).map(ref =>
                    ReactiveUI.subscribed(UI.div(el(ref.readOnly)), rec) { sub =>
                        sub.handle(at, UIEvent.ChangeChecked(at, true)).andThen(rec.boolPatches)
                    }.map(ops => assert(ops.map(op => (op.path, op.name, op.value)) == Chunk((at, "checked", false))))
                )
            end declined
            declined(UI.checkbox.checked(_).onChange(_ => ()))
                .andThen(declined(UI.radio.checked(_).onChange(_ => ())))
                // No handler at all: a radio drawn for its keyboard alone relies on this.
                .andThen(declined(UI.checkbox.checked(_)))
                .andThen(declined(UI.radio.checked(_)))
        }

        "a toggle its owner took sends the emission's patch and no assert" in {
            val rec = new RecordingExchange
            for
                ref <- Signal.initRef(false)
                ops <- ReactiveUI.subscribed(UI.div(UI.checkbox.checked(ref.readOnly).onChange(ref.set(_))), rec) { sub =>
                    parked(ref).andThen(sub.handle(
                        at,
                        UIEvent.ChangeChecked(at, true)
                    )).andThen(rec.awaitOp(_ => true)).andThen(parked(ref))
                        .andThen(rec.boolPatches)
                }
            yield assert(ops.map(_.value) == Chunk(true))
            end for
        }

        "an element that does not follow is left alone" in {
            val rec = new RecordingExchange
            for
                ops <- ReactiveUI.subscribed(UI.div(UI.input.value("start").onInput(_ => ())), rec) { sub =>
                    sub.handle(at, inputEvent("typed")).andThen(rec.ops)
                }
            yield assert(ops.isEmpty)
            end for
        }

        "a read-only field takes no edit, and nothing is sent" in {
            val rec = new RecordingExchange
            for
                ref <- Signal.initRef("held")
                ops <- ReactiveUI.subscribed(UI.div(UI.input.readOnly(true).value(ref.readOnly).onInput(_ => ())), rec) { sub =>
                    sub.handle(at, inputEvent("typed")).andThen(rec.ops)
                }
            yield assert(ops.isEmpty)
            end for
        }
    }

    "reads" - {
        "a blur with no text reports what the signal holds" in {
            for
                ref  <- Signal.initRef("held")
                seen <- AtomicRef.init("")
                _    <- ReactiveUI.subscribed(UI.div(UI.input.value(ref.readOnly).onBlur((_, v) => seen.set(v))), new RecordingExchange) {
                    sub =>
                        sub.handle(at, UIEvent.Blur(at, kyo.internal.MouseEventData(UI.Modifiers.none, Absent), Absent))
                }
                got <- seen.get
            yield assert(got == "held")
            end for
        }

        "Enter on a followed box toggles from what the signal holds and reports it" in {
            for
                ref  <- Signal.initRef(true)
                seen <- AtomicRef.init(Maybe.empty[Boolean])
                _    <- ReactiveUI.subscribed(
                    UI.div(UI.checkbox.checked(ref.readOnly).onChange(v => seen.set(Present(v)))),
                    new RecordingExchange
                ) { sub =>
                    sub.handle(at, UIEvent.KeyDown(at, kyo.internal.KeyboardEventData("Enter", UI.Modifiers.none, Absent)))
                }
                got  <- seen.get
                held <- ref.get
            yield
                assert(got == Present(false))
                assert(held, "the signal is its owner's, and stays")
            end for
        }
    }

end UIBoundFollowTest
