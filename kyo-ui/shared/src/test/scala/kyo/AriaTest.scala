package kyo

import kyo.UI.*
import kyo.UI.Ast.*
import kyo.internal.HtmlRenderer

class AriaTest extends kyo.test.Test[Any]:

    private def renderHtml(ui: UI)(using Frame): String < Sync =
        HtmlRenderer.render(ui, Seq.empty)

    "an aria entry renders the aria-label attribute on div" in {
        val html = renderHtml(UI.div.aria("label" -> "Save"))
        html.map { s =>
            assert(s.contains("""aria-label="Save""""))
        }
    }

    "a Maybe aria entry sets the attribute from Present and nothing from Absent, the element as it is" in {
        val named: Maybe[Prop[String]]   = Present("Save")
        val unnamed: Maybe[Prop[String]] = Absent
        val el                           = UI.div
        for html <- renderHtml(el.aria("label" -> named))
        yield
            assert(html.contains("aria-label=\"Save\""), html)
            assert(el.aria("label" -> unnamed) eq el)
        end for
    }

    "a Maybe aria entry that holds a signal is the attribute channel" in {
        for
            ref <- Signal.initRef("Save")
            el = UI.div.aria("label" -> (Present(ref): Maybe[Prop[String]]))
        yield assert(el.attrs.reactiveAttrs.get("aria-label").exists(_ eq ref), s"${el.attrs}")
    }

    "aria with three entries renders all aria-* attributes sorted alphabetically" in {
        val html = renderHtml(
            UI.div.aria(
                "label" -> "Main",
                "live"  -> "polite",
                "busy"  -> "false"
            )
        )
        html.map { s =>
            assert(s.contains("""aria-busy="false""""))
            assert(s.contains("""aria-label="Main""""))
            assert(s.contains("""aria-live="polite""""))
            val busyPos  = s.indexOf("aria-busy")
            val labelPos = s.indexOf("aria-label")
            val livePos  = s.indexOf("aria-live")
            assert(busyPos < labelPos && labelPos < livePos)
        }
    }

    "setting the same aria name twice keeps only the last value" in {
        val html = renderHtml(
            UI.div.aria("label" -> "First").aria("label" -> "Second")
        )
        html.map { s =>
            assert(s.contains("""aria-label="Second""""))
            assert(!s.contains("""aria-label="First""""))
        }
    }

    "element with ariaAttrs emits aria-* before data-* before data-kyo-prop-* in deterministic order" in {
        val element = UI.checkbox
            .aria("role" -> "button")
            .aria("label" -> "Go")
            .data("track-id" -> "99")
            .data("env" -> "prod")
            .indeterminate(true)
        for
            s  <- renderHtml(element)
            s2 <- renderHtml(element)
        yield
            assert(s == s2)
            val ariaLabelPos = s.indexOf("aria-label")
            val ariaRolePos  = s.indexOf("aria-role")
            val dataEnvPos   = s.indexOf("data-env")
            val dataTrackPos = s.indexOf("data-track-id")
            val dataPropPos  = s.indexOf("data-kyo-prop-indeterminate")
            assert(ariaLabelPos < ariaRolePos)
            assert(dataEnvPos < dataTrackPos)
            assert(ariaRolePos < dataEnvPos)
            assert(dataTrackPos < dataPropPos)
        end for
    }

    "aria and data attributes both render sorted by name (structural attribute check)" in {
        val html = renderHtml(
            UI.div
                .aria("z-attr" -> "z")
                .aria("a-attr" -> "a")
                .data("z-data" -> "z")
                .data("a-data" -> "a")
        )
        html.map { s =>
            assert(s.indexOf("aria-a-attr") < s.indexOf("aria-z-attr"))
            assert(s.indexOf("data-a-data") < s.indexOf("data-z-data"))
            assert(s.contains("aria-a-attr"))
            assert(s.contains("aria-z-attr"))
            assert(s.contains("data-a-data"))
            assert(s.contains("data-z-data"))
        }
    }

    // ---- aria(entries*): name -> value, written by the value's type ----

    /** The attribute patches and paints an exchange records, as `name=value`, `off name` and `paint`. */
    private def patches(exchange: kyo.internal.RecordingExchange): Chunk[String] =
        import kyo.internal.RecordingExchange.Op
        exchange.recorded.collect {
            case op: Op.Attr     => s"${op.name}=${op.value}"
            case op: Op.BoolAttr => s"${if op.value then "on" else "off"} ${op.name}"
            case _: Op.Paint     => "paint"
        }
    end patches

    "aria entries write each value as what it is: a string itself, a Boolean and a number as their text" in {
        val html = renderHtml(
            UI.div.aria("label" -> "Save", "pressed" -> true, "busy" -> false, "valuenow" -> 3, "valuemax" -> 40L, "valuemin" -> 0.5)
        )
        html.map { s =>
            assert(
                s.contains("""aria-label="Save"""") && s.contains("""aria-pressed="true"""") && s.contains("""aria-busy="false"""") &&
                    s.contains("""aria-valuenow="3"""") && s.contains("""aria-valuemax="40"""") && s.contains("""aria-valuemin="0.5""""),
                s
            )
        }
    }

    "a Maybe entry writes what it holds, and Absent writes nothing, so an earlier value stays" in {
        val named: Maybe[String]   = Present("Save")
        val unnamed: Maybe[String] = Absent
        val count: Maybe[Int]      = Present(2)
        val el                     = UI.div.aria("label" -> "First")
        for
            set  <- renderHtml(UI.div.aria("label" -> named, "setsize" -> count))
            kept <- renderHtml(el.aria("label" -> unnamed))
        yield
            assert(set.contains("""aria-label="Save"""") && set.contains("""aria-setsize="2""""), set)
            assert(kept.contains("""aria-label="First""""), kept)
            val again = el.aria("label" -> unnamed).attrs
            assert(again.ariaAttrs == Map("label" -> "First") && again.reactiveAttrs.isEmpty && again.reactiveMaybeAttrs.isEmpty, s"$again")
        end for
    }

    "a signal of a string entry is the attribute channel itself, with no mapped signal between" in {
        for
            ref <- Signal.initRef("Save")
            el = UI.div.aria("label" -> (ref: Signal[String]))
        yield assert(el.attrs.reactiveAttrs.get("aria-label").exists(_ eq ref) && el.attrs.ariaAttrs.isEmpty, s"${el.attrs}")
    }

    "a signal entry patches the attribute in place, a Boolean as \"true\" and \"false\", a number as its text" in {
        import kyo.internal.ReactiveUI
        Scope.run {
            Kyo.foreachDiscard(Chunk(true, false)) { fast =>
                val exchange = new kyo.internal.RecordingExchange(synchronous = fast)
                for
                    pressed <- Signal.initRef(true)
                    now     <- Signal.initRef(1)
                    ui = UI.span("x").aria("pressed" -> (pressed: Signal[Boolean]), "valuenow" -> (now: Signal[Int]))
                    first <- renderHtml(UI.div(ui))
                    root  <- ReactiveUI.normalize(UI.div(ui), Seq.empty)
                    _     <- ReactiveUI.subscribe(root, exchange)
                    _     <- pressed.set(false)
                    _     <- assertEventually(Sync.defer(patches(exchange).size == 1))
                    _     <- now.set(7)
                    _     <- assertEventually(Sync.defer(patches(exchange).size == 2))
                yield
                    assert(first.contains("""aria-pressed="true"""") && first.contains("""aria-valuenow="1""""), first)
                    assert(patches(exchange) == Chunk("aria-pressed=false", "aria-valuenow=7"), s"fast=$fast: ${patches(exchange)}")
                end for
            }
        }
    }

    "a signal of a Maybe leaves the attribute out while it holds Absent, in place" in {
        import kyo.internal.ReactiveUI
        Scope.run {
            Kyo.foreachDiscard(Chunk(true, false)) { fast =>
                val exchange = new kyo.internal.RecordingExchange(synchronous = fast)
                for
                    said <- Signal.initRef[Maybe[Boolean]](Absent)
                    ui = UI.span("x").aria("expanded" -> (said: Signal[Maybe[Boolean]]))
                    first <- renderHtml(UI.div(ui))
                    root  <- ReactiveUI.normalize(UI.div(ui), Seq.empty)
                    _     <- ReactiveUI.subscribe(root, exchange)
                    _     <- said.set(Present(false))
                    _     <- assertEventually(Sync.defer(patches(exchange).size == 1))
                    _     <- said.set(Absent)
                    _     <- assertEventually(Sync.defer(patches(exchange).size == 2))
                yield
                    assert(!first.contains("aria-expanded"), first)
                    assert(patches(exchange) == Chunk("aria-expanded=false", "off aria-expanded"), s"fast=$fast: ${patches(exchange)}")
                end for
            }
        }
    }

    "a Maybe of a signal and a Prop entry are the channel when they hold a signal, and static when they hold a value" in {
        def bound(ref: Signal[Boolean]): Div =
            val held: Maybe[Signal[Boolean]] = Present(ref)
            val sig: Prop[Boolean]           = ref
            val con: Prop[Boolean]           = false
            UI.div.aria("checked" -> held, "pressed" -> sig, "disabled" -> con)
        end bound
        for
            ref <- Signal.initRef(true)
            el = bound(ref)
            html <- renderHtml(el)
        yield
            assert(el.attrs.reactiveAttrs.keySet == Set("aria-checked", "aria-pressed"), s"${el.attrs}")
            assert(el.attrs.ariaAttrs == Map("disabled" -> "false"), s"${el.attrs}")
            assert(html.contains("""aria-checked="true"""") && html.contains("""aria-pressed="true""""), html)
        end for
    }

    "a pair held in a value converts too, and Entry.of keeps a group to spread" in {
        val pair: (String, Prop[String]) = ("label", "Save")
        val range                        = Entry.of("valuemin" -> 0, "valuemax" -> 10)
        for
            one <- renderHtml(UI.div.aria(pair))
            two <- renderHtml(UI.div.aria(range*).aria("valuenow" -> 5))
        yield
            assert(one.contains("""aria-label="Save""""), one)
            assert(
                two.contains("""aria-valuemin="0"""") && two.contains("""aria-valuemax="10"""") && two.contains("""aria-valuenow="5""""),
                two
            )
        end for
    }

    "a value whose type has no AttrValue is a type error" in {
        typeCheckFailure("""
            import kyo.*
            UI.div.aria("x" -> 1.5f)
        """)("(String, Float)")
        typeCheckFailure("""
            import kyo.*
            val nested: Maybe[Maybe[String]] = Absent
            UI.div.aria("x" -> nested)
        """)("(String, kyo.Maybe[kyo.Maybe[String]])")
        typeCheckFailure("""
            import kyo.*
            def nested: Signal[Signal[String]] = ???
            UI.div.data("x" -> nested)
        """)("(String, kyo.Signal[kyo.Signal[String]])")
    }

    // ---- role: the bare `role` HTML attribute, distinct from aria-role ----

    "role(v) renders the bare role attribute on div" in {
        val html = renderHtml(UI.div.role("button"))
        html.map { s =>
            assert(s.contains("""role="button""""))
            // It must be the bare `role`, not `aria-role`.
            assert(!s.contains("""aria-role="button""""))
        }
    }

    "no role set emits no role attribute" in {
        val html = renderHtml(UI.div("x"))
        html.map { s =>
            assert(!s.contains("role="), s"Expected no role attribute but got: $s")
        }
    }

    "role and aria-role are distinct attributes and both render" in {
        val html = renderHtml(UI.div.role("img").aria("role" -> "button"))
        html.map { s =>
            assert(s.contains("""role="img""""), s"missing bare role: $s")
            assert(s.contains("""aria-role="button""""), s"missing aria-role: $s")
        }
    }

end AriaTest
