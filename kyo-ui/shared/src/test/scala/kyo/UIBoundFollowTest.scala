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

end UIBoundFollowTest
