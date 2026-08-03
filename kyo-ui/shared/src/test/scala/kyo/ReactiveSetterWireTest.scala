package kyo

import kyo.Browser.*
import kyo.internal.HtmlOp
import scala.language.implicitConversions

/** What a `Signal`-typed setter actually sends when its signal moves.
  *
  * The rule these guard is stated on `UI.Ast.Element.withReactiveAttr`: a setter is a CHANNEL — patching one
  * attribute in place — exactly when its value maps to one plain HTML attribute whose written value is the
  * live state; otherwise it wraps the element and re-renders it. `hidden`, `href` and `src` used to be
  * wrappers for no reason other than history, so toggling visibility repainted a whole subtree to change one
  * attribute while `disabled` beside it patched in place.
  *
  * The difference does not show in the DOM: both paths leave the same attribute on the same element, and the
  * morph even preserves node identity. It shows in the frame, which is what these read. A setter that
  * regressed to a wrapper would keep every attribute assertion in [[AttrsTest]] green while costing a subtree
  * repaint per emission.
  */
class ReactiveSetterWireTest extends UITest:

    /** One signal, one setter under test, and a button that moves the signal. */
    private def app(build: Signal[Boolean] => UI)(using Frame): UI < Async =
        for flag <- Signal.initRef(false)
        yield UI.div(
            build(flag),
            UI.button("go").id("go").onClick(flag.set(true))
        )

    "hidden(Signal) toggles the attribute in place" in {
        withUI(app(flag => UI.div("content").id("d").hidden(flag))) {
            for
                _  <- Browser.evalDiscard(captureFrames)
                _  <- Browser.click(Selector.id("go"))
                _  <- Browser.assertAttribute(Selector.id("d"), "hidden", "")
                op <- lastOp
            yield op match
                case HtmlOp.SetBoolAttrByPath(_, name, value) => assert(name == "hidden" && value)
                case other                                    => fail(s"expected an in-place bool patch, got $other")
        }
    }

    "hidden(Signal) keeps what the subtree was holding" in {
        // The payoff of the channel, and the reason it is worth a wire assertion: a re-render would put the
        // input through the morph, where an unhidden field is a fresh paint. In place, it is the same node
        // with the same caret it had before it was hidden.
        val ui: UI < Async =
            for flag <- Signal.initRef(false)
            yield UI.div(
                UI.div(UI.input.id("i")).id("box").hidden(flag),
                UI.button("hide").id("hide").onClick(flag.set(true)),
                UI.button("show").id("show").onClick(flag.set(false))
            )
        withUI(ui) {
            for
                _     <- Browser.fill(Selector.id("i"), "typed")
                _     <- Browser.evalDiscard("document.getElementById('i').__kyoProbe='kept'")
                _     <- Browser.evalDiscard("document.getElementById('hide').click()")
                _     <- Browser.assertAttribute(Selector.id("box"), "hidden", "")
                _     <- Browser.evalDiscard("document.getElementById('show').click()")
                probe <- Browser.evalJson[String]("String(document.getElementById('i').__kyoProbe)")
                value <- Browser.evalJson[String]("document.getElementById('i').value")
            yield assert(probe == "kept" && value == "typed", s"probe=$probe value=$value")
        }
    }

    "href(Signal) patches the attribute rather than replacing the link" in {
        val ui: UI < Async =
            for dest <- Signal.initRef(UI.Href.Path("/one"): UI.Href)
            yield UI.div(
                UI.a("link").id("a").href(dest),
                UI.button("go").id("go").onClick(dest.set(UI.Href.Fragment("two")))
            )
        withUI(ui) {
            for
                _  <- Browser.evalDiscard(captureFrames)
                _  <- Browser.click(Selector.id("go"))
                _  <- Browser.assertAttribute(Selector.id("a"), "href", "#two")
                op <- lastOp
            yield op match
                // Also pins that the channel renders the SAME string the renderer would (Href.attrValue).
                case HtmlOp.SetAttrByPath(_, name, value) => assert(name == "href" && value == "#two")
                case other                                => fail(s"expected an in-place attr patch, got $other")
        }
    }

    "src(Signal) on an iframe patches rather than reloading the frame" in {
        val ui: UI < Async =
            for src <- Signal.initRef("about:blank")
            yield UI.div(
                UI.iframe("about:blank").id("f").src(src),
                UI.button("go").id("go").onClick(src.set("about:blank#two"))
            )
        withUI(ui) {
            for
                _  <- Browser.evalDiscard(captureFrames)
                _  <- Browser.click(Selector.id("go"))
                op <- lastOp
            yield op match
                case HtmlOp.SetAttrByPath(_, name, value) => assert(name == "src" && value == "about:blank#two")
                case other                                => fail(s"expected an in-place attr patch, got $other")
        }
    }

    "style(Signal) still re-renders, because no single attribute carries it" in {
        // The other side of the rule. A pseudo-state Style becomes a generated class plus an injected
        // stylesheet rule, so there is nothing for an attribute channel to patch.
        val ui: UI < Async =
            for st <- Signal.initRef(Style.empty)
            yield UI.div(
                UI.div("x").id("d").style(st),
                UI.button("go").id("go").onClick(st.set(Style.bold))
            )
        withUI(ui) {
            for
                _  <- Browser.evalDiscard(captureFrames)
                _  <- Browser.click(Selector.id("go"))
                op <- lastOp
            yield op match
                case _: HtmlOp.Replace => succeed
                case other             => fail(s"expected a re-render, got $other")
        }
    }

end ReactiveSetterWireTest
