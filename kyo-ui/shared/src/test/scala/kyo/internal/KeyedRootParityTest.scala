package kyo.internal

import kyo.*

/** [[HtmlRenderer.paintsAsKeyedRoot]] against what [[HtmlRenderer.render]] actually emits.
  *
  * The predicate answers a question about the renderer without going through it, which is the only way a
  * transport can decide BEFORE sending whether a keyed frame can describe an emission at all. That makes it a
  * second statement of the renderer's root shape, and a second statement is a thing that drifts: the day a UI
  * node starts painting a wrapper, the predicate keeps saying what it said, the shape gate keeps letting the
  * emission through, and the rows quietly stop being addressable on the far side of a socket.
  *
  * So every case is driven through both and the answers are compared. Total match plus this table is the whole
  * anti-drift story: the compiler forces a NEW node to be answered here, and this forces the answer to be right.
  */
class KeyedRootParityTest extends kyo.test.Test[Any]:

    private val path                                     = Seq("r", "row1")
    private val pathAttr                                 = path.mkString(".")
    private val keyAttr                                  = s"""data-kyo-path="$pathAttr""""
    private def openMarker                               = RegionMarker.open(path)
    private def closeMarker                              = RegionMarker.close(path)
    private def occurrences(s: String, sub: String): Int = s.sliding(sub.length).count(_ == sub)

    /** True when `html` is exactly one logical child that `pathAttr` names: an element whose OWN tag carries
      * the path (children render at `pathAttr.0`, `pathAttr.1`, … and so never match the quoted form), or a
      * marker span opened and closed at it.
      */
    private def isOneKeyedRoot(html: String): Boolean =
        val elementRoot =
            html.startsWith("<") && occurrences(html, keyAttr) == 1 && html.indexOf(keyAttr) < html.indexOf(">")
        val spanRoot = html.startsWith(openMarker) && html.endsWith(closeMarker)
        elementRoot || spanRoot
    end isOneKeyedRoot

    private def check(name: String, ui: UI < Sync)(using Frame) =
        name in {
            for
                node <- ui
                html <- HtmlRenderer.render(node, path)
            yield
                val predicted = HtmlRenderer.paintsAsKeyedRoot(node)
                assert(
                    predicted == isOneKeyedRoot(html),
                    s"paintsAsKeyedRoot said $predicted, rendered:\n$html"
                )
        }

    check("an element", Kyo.lift(UI.li("x")))
    check("a void element", Kyo.lift(UI.input))
    check("a textarea", Kyo.lift(UI.textarea.value("x")))
    check("an iframe", Kyo.lift(UI.iframe("about:blank")))
    check("an element with children", Kyo.lift(UI.div(UI.span("a"), UI.span("b"))))
    check("a dropdown", Kyo.lift(UI.dropdown("A" -> "a", "B" -> "b")))
    check("an svg element", Kyo.lift(Svg.svg(Svg.circle.r(1))))
    check("text", Kyo.lift(UI.Ast.Text("plain")))
    check("raw html", Kyo.lift(UI.rawHtml("<b>x</b>")))
    check("an empty fragment", Kyo.lift(UI.empty))
    check("a fragment of one", Kyo.lift(UI.fragment(UI.li("only"))))
    check("a fragment of several", Kyo.lift(UI.fragment(UI.li("a"), UI.li("b"))))
    check("a reactive region", Signal.initRef("v").map(r => (r.map(v => UI.li(v): UI): UI)))
    check("a reactive region holding text", Signal.initRef("v").map(r => (r.map(v => UI.Ast.Text(v): UI): UI)))
    check("a keyed foreach region", Signal.initRef(Chunk("a")).map(r => UI.foreachKeyed(r)(identity)(i => UI.li(i)): UI))

end KeyedRootParityTest
