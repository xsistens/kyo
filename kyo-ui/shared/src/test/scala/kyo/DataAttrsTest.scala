package kyo

import kyo.UI.*
import kyo.UI.Ast.*
import kyo.internal.HtmlRenderer

class DataAttrsTest extends kyo.test.Test[Any]:

    private def renderHtml(ui: UI)(using Frame): String < Sync =
        HtmlRenderer.render(ui, Seq.empty)

    "a data entry renders data-user-id and does not collide with data-kyo-path" in {
        val html = renderHtml(UI.span.data(data"data-user-id" -> "42").onClick(()))
        html.map { s =>
            assert(s.contains("""data-user-id="42""""))
            assert(s.contains("data-kyo-path"))
        }
    }

    "two data calls merge into one dataAttrs map and render both sorted by name" in {
        val html = renderHtml(
            UI.div.data(data"data-z-index" -> "10").data(data"data-app-id" -> "abc")
        )
        html.map { s =>
            assert(s.contains("""data-app-id="abc""""))
            assert(s.contains("""data-z-index="10""""))
            assert(s.indexOf("data-app-id") < s.indexOf("data-z-index"))
        }
    }

    "a key with a quote stays inside data-kyo-path" in {
        val key  = "x\" onmouseover=\"alert(1)"
        val keys = Signal.initConst(Chunk(key))
        renderHtml(
            UI.div(
                UI.ul(keys.foreachKeyed(identity)(k => UI.li(k))),
                Svg.svg(keys.foreachKeyed(identity)(_ => Svg.g))
            )
        ).map { s =>
            assert(!s.contains(""" onmouseover="alert(1)"""), s)
            assert(s.contains("""data-kyo-path="0.0.x&quot; onmouseover=&quot;alert(1)""""), s)
            assert(s.contains("""data-kyo-path="1.0.x&quot; onmouseover=&quot;alert(1)""""), s)
        }
    }

    "data with kyo- prefix throws IllegalArgumentException with message containing kyo-" in {
        val ex = intercept[IllegalArgumentException] {
            val name = "kyo-internal"
            UI.div.data(data"data-$name" -> "x")
        }
        assert(ex.getMessage.contains("kyo-"))
    }

    "data entries with a kyo- prefix throw before any value is set" in {
        val ex = intercept[IllegalArgumentException] {
            val name = "kyo-a"
            UI.div.data(data"data-$name" -> "1", data"data-ok" -> "2")
        }
        assert(ex.getMessage.contains("kyo-"))
        val baseDiv = UI.Ast.Div()
        assert(baseDiv.attrs.dataAttrs.isEmpty)
    }

    // ---- data(entries*): name -> value, written by the value's type ----

    "data entries write each value as what it is, and Absent writes nothing" in {
        val form: Maybe[String] = Absent
        val html                =
            renderHtml(UI.div.data(
                data"data-vs-first" -> 3,
                data"data-open"     -> true,
                data"data-key"      -> "k",
                data"data-form"     -> form
            ))
        html.map { s =>
            assert(
                s.contains("""data-vs-first="3"""") && s.contains("""data-open="true"""") && s.contains("""data-key="k"""") &&
                    !s.contains("data-form"),
                s
            )
        }
    }

    "a signal data entry patches the attribute in place" in {
        import kyo.internal.ReactiveUI
        import kyo.internal.RecordingExchange
        import kyo.internal.RecordingExchange.Op
        Scope.run {
            Kyo.foreachDiscard(Chunk(true, false)) { fast =>
                val exchange = new RecordingExchange(synchronous = fast)
                def patches  = exchange.recorded.collect {
                    case op: Op.Attr => s"${op.name}=${op.value}"
                    case _: Op.Paint => "paint"
                }
                for
                    first <- Signal.initRef(0)
                    ui = UI.div("rows").data(data"data-vs-first" -> (first: Signal[Int]))
                    html <- renderHtml(UI.div(ui))
                    root <- ReactiveUI.normalize(UI.div(ui), Seq.empty)
                    _    <- ReactiveUI.subscribe(root, exchange)
                    _    <- first.set(20)
                    _    <- assertEventually(Sync.defer(patches.size == 1))
                yield
                    assert(html.contains("""data-vs-first="0""""), html)
                    assert(patches == Chunk("data-vs-first=20"), s"fast=$fast: $patches")
                end for
            }
        }
    }

    "a data entry with the kyo- prefix throws, whatever its value" in {
        val none: Maybe[Int] = Absent
        val kyoX             = "kyo-x"
        val kyoY             = "kyo-y"
        val one              = intercept[IllegalArgumentException](UI.div.data(data"data-$kyoX" -> 1))
        val absent           = intercept[IllegalArgumentException](UI.div.data(data"data-ok" -> 1, data"data-$kyoY" -> none))
        assert(one.getMessage.contains("kyo-x") && absent.getMessage.contains("kyo-y"), s"${one.getMessage} / ${absent.getMessage}")
    }

end DataAttrsTest
