package kyo

import kyo.UI.Ast.*
import kyo.UI.foreach
import kyo.UI.render
import kyo.internal.HtmlRenderer

/** Tests for the reactive-boundary typing: parameterized `Reactive[C]`, `Foreach[A, C]`, `Fragment[C]`,
  * and the narrowed return types on `when`, `render`, `foreach*`, and the Signal-overloaded setters.
  */
class UIReactiveTypingTest extends kyo.test.Test[Any]:

    private def renderHtml(ui: UI)(using Frame): String < Sync =
        HtmlRenderer.render(ui, Seq.empty)

    // UI.when returns Reactive[C] at the static type
    "UI.when returns Reactive typed to its body" in {
        val cond: Signal[Boolean]    = Signal.initConst(true)
        val r: Reactive[SpanElement] = UI.when(cond)(UI.span("x"))
        assert(r.isInstanceOf[Reactive[?]])
        val html = renderHtml(r)
        html.map { h => assert(h.contains("<span")) }
    }

    // Signal.render returns Reactive[C]
    "Signal.render returns Reactive typed to render function result" in {
        val sig                      = Signal.initConst("hello")
        val r: Reactive[SpanElement] = sig.render(s => UI.span(s))
        assert(r.isInstanceOf[Reactive[?]])
    }

    // ---- which shape a Signal-typed setter has ----
    //
    // The rule (stated on Element.withReactiveAttr): a setter is a CHANNEL — patching in place, returning
    // `Self`, chainable — exactly when its value maps to one plain HTML attribute whose written value is the
    // live state; otherwise it wraps the element in a `Reactive` and re-renders it. The split used to be
    // historical, and these pin it so it stays a rule. A channel setter that silently became a wrapper again
    // would cost a subtree repaint per emission without changing a single rendered byte.

    "a channel setter returns Self and stays chainable" in {
        val flag            = Signal.initConst(false)
        val text            = Signal.initConst("t")
        val el: SpanElement = UI.span("x").hidden(flag).title(text).cssClass("c", flag).id("after")
        val link: Anchor    = UI.a("l").href(Signal.initConst(UI.Href.Path("/p"))).id("also-after")
        assert(el.attrs.reactiveBoolAttrs.contains("hidden"))
        assert(el.attrs.identifier == Present("after"))
        assert(link.attrs.reactiveAttrs.contains("href") && link.attrs.identifier == Present("also-after"))
    }

    "a channel beats a static value in either setter order" in {
        // The precedence rule (also stated on withReactiveAttr): where a channel exists it IS the value. Both
        // orders have to agree, because the client makes them agree at runtime whatever the HTML said — its
        // patch sets and removes the attribute on every emission. Rendering `disabled` here from the static
        // value would paint a control the first patch then contradicts.
        val off = Signal.initConst(false)
        for
            a <- renderHtml(UI.button("x").disabled(true).disabled(off))
            b <- renderHtml(UI.button("x").disabled(off).disabled(true))
            c <- renderHtml(UI.div("x").hidden(true).hidden(off))
        yield
            assert(!a.contains("disabled"), s"static won over the channel: $a")
            assert(!b.contains("disabled"), s"static won over the channel: $b")
            assert(!c.contains("hidden"), s"static won over the channel: $c")
        end for
    }

    "a setter that no single attribute can carry stays a Reactive wrapper" in {
        // style: a pseudo-state Style becomes a generated class plus an injected rule, not an inline value.
        val sig                      = Signal.initConst(Style.empty)
        val r: Reactive[SpanElement] = UI.span("x").style(sig)
        assert(r.isInstanceOf[Reactive[?]])
    }

    // UI.when runtime renders true branch
    "UI.when runtime renders true branch" in {
        val cond = Signal.initConst(true)
        val r    = UI.when(cond)(UI.span("visible"))
        val html = renderHtml(r)
        html.map { h => assert(h.contains("<span")) }
    }

    // foreach returns Foreach[A, C] typed to render function result
    "Signal.foreach returns Foreach typed to render function result" in {
        val sig                           = Signal.initConst(Chunk(1, 2, 3))
        val fe: Foreach[Int, SpanElement] = sig.foreach(n => UI.span(n.toString))
        assert(fe.isInstanceOf[Foreach[?, ?]])
    }

    // Checkbox.indeterminate(Signal) returns Reactive[Checkbox]
    "Checkbox.indeterminate(Signal) returns Reactive[Checkbox]" in {
        for ref <- Signal.initRef(true)
        yield
            val r: Reactive[Checkbox] = UI.checkbox.indeterminate(ref: Signal[Boolean])
            assert(r.isInstanceOf[Reactive[?]])
    }

    // when[C] infers C as Svg.Circle in SVG context
    "when infers C in SVG context" in {
        val cond                    = Signal.initConst(true)
        val r: Reactive[Svg.Circle] = UI.when(cond)(Svg.circle.cx(1).cy(1).r(1))
        assert(r.isInstanceOf[Reactive[?]])
        // the typed reactive fits in an svg container
        val svgNode = Svg.svg(r)
        assert(svgNode.isInstanceOf[Svg.Root])
    }

    // implicit lifts return the narrowed Reactive[C]
    "implicit lifts return typed Reactive" in {
        val strSig: Signal[String] = Signal.initConst("x")
        val r1: Reactive[Text]     = strSig
        assert(r1.isInstanceOf[Reactive[?]])
        val spanSig: Signal[SpanElement] = Signal.initConst(UI.span("y"))
        val r2: Reactive[SpanElement]    = spanSig
        assert(r2.isInstanceOf[Reactive[?]])
    }

    // SVG elements inherit the Signal setters from Element, so they follow the same split.
    "SVG element inherits both setter shapes" in {
        val boolSig                       = Signal.initConst(false)
        val channel: Svg.Circle           = Svg.circle.cx(1).cy(1).r(1).hidden(boolSig)
        val wrapper: Reactive[Svg.Circle] = Svg.circle.cx(1).cy(1).r(1).style(Signal.initConst(Style.empty))
        assert(channel.attrs.reactiveBoolAttrs.contains("hidden"))
        assert(wrapper.isInstanceOf[Reactive[?]])
    }

end UIReactiveTypingTest
