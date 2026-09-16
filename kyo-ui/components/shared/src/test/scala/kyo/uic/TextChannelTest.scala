package kyo.uic.test

import kyo.*
import kyo.UI.*
import kyo.test.AssertScope
import kyo.uic
import kyo.uic.UicTest
import scala.language.implicitConversions

/** Where a component's text slot is a CHANNEL and where it is a region.
  *
  * A `Signal[String]` label can be placed two ways, and both render the same markup. As a region
  * (`sig.render(t => span(t))`) every emission rebuilds the span: a fiber, a per-value Scope with its
  * finalizer queue, a re-walk and an HTML render, to arrive at one `Text.data` write. Placed as the
  * span's CHILD the signal lifts to a `UI.Ast.Reactive` carrying the string signal in `text`, which is
  * the one field `kyo.internal.ReactiveUI.bindTextRegion` keys on: it binds the signal straight to the
  * backend's text write and skips the region apparatus entirely.
  *
  * Because the two forms are indistinguishable in the rendered HTML, the golden suite cannot hold this
  * line — a refactor that reaches for `.render` again would be invisible there. So it is held here, on
  * the AST. The discriminator is where the reactive node sits: under the labelled element it is the
  * channel, and the labelled element itself is static; above it, the element is the region's output and
  * carries a plain `Text` child instead.
  */
class TextChannelTest extends UicTest:

    private def renderHtml(ui: UI)(using Frame): String < Async =
        UI.runRender(ui).take(1).run.map(_.mkString)

    /** The channel contract for one slot: the element carrying `cls` is built once and holds exactly one
      * reactive node, and that node carries a string signal.
      *
      * Stated on the element rather than on the whole tree so it reads the same for a component whose
      * slot sits behind an unrelated region of its own (a Terminal's history, a Panel's toggle). Under
      * the old region form this fails on the first assertion: the walk resolves the region, finds the
      * same span, and sees a plain `Text` child under it.
      */
    private def assertChannel(tree: UI, cls: String)(using Frame, AssertScope): Unit < Sync =
        for
            el    <- elementWithClass(tree, cls)
            nodes <- reactiveNodes(el)
        yield
            assert(nodes.size == 1, s"$cls must hold exactly one reactive node, found ${nodes.size}")
            assert(nodes.head.text.isDefined, s"$cls's reactive node must carry its string signal (text channel)")
        end for
    end assertChannel

    "Tag's Signal label is a text channel, not a region over the span" in {
        for
            ref <- Signal.initRef("● LIVE")
            tree = uic.Tag(ref).toUI
            _       <- assertChannel(tree, "p-tag-label")
            regions <- regionsAbove(tree, "p-tag-label")
            label   <- elementWithClass(tree, "p-tag-label")
            nodes   <- reactiveNodes(label)
            html    <- renderHtml(tree)
        yield
            assert(regions == 0, s"the label span is built once, not per emission (found $regions regions above it)")
            assert(label.children.size == 1, "the label span holds exactly the one reactive text node")
            // The region carries the caller's own signal, unmapped — that identity is what lets the
            // binder dedupe on the emitted string instead of on a fresh UI value per emission.
            val carriesCallerSignal = nodes.head.text match
                case Present(s) => s eq ref
                case Absent     => false
            assert(carriesCallerSignal, "the reactive node carries the caller's string signal itself")
            // Markup is unchanged: same span, same class, same text. Only the region's comment marker
            // pair moved from around the span to inside it.
            assert(html.contains("p-tag-label"), "renders Prime's label span")
            assert(html.contains("● LIVE"), "renders the signal's current value")
    }

    "a constant Tag label stays a plain text node — no reactive node at all" in {
        for
            tree  <- Kyo.lift(uic.Tag("Done").toUI)
            nodes <- reactiveNodes(tree)
            html  <- renderHtml(tree)
        yield
            assert(nodes.isEmpty, s"a constant label needs no reactive node, found ${nodes.size}")
            assert(html.contains("p-tag-label"), "and renders the same label span as the dynamic form")
    }

    /** The slot shape repeats once per component, so the rule is pinned per component rather than once on
      * Tag: a reader who adds a new text slot by copying a neighbour should copy the channel form.
      */
    "the module's plain text slots are channels, not regions" in {
        for
            ref <- Signal.initRef("value")
            _   <- assertChannel(uic.Button(ref).toUI, "p-button-label")
            _   <- assertChannel(uic.Label(ref).toUI, "p-uic-label")
            _   <- assertChannel(uic.Chip(ref).toUI, "p-chip-label")
            _   <- assertChannel(uic.Card().title(ref).toUI, "p-card-title")
            _   <- assertChannel(uic.Card().subtitle(ref).toUI, "p-card-subtitle")
            _   <- assertChannel(uic.Panel().header(ref).toUI, "p-panel-title")
            _   <- assertChannel(uic.Fieldset().legend(ref).toUI, "p-fieldset-legend-label")
            _   <- assertChannel(uic.Breadcrumb().item(ref).toUI, "p-breadcrumb-item-label")
            _   <- assertChannel(uic.Terminal().welcomeMessage(ref).toUI, "p-terminal-welcome-message")
            _   <- assertChannel(uic.Badge(ref).toUI, "p-badge")
        yield succeed("every slot above asserted through assertChannel")
    }

    "an empty-state slot bound to a signal patches its message in place" in {
        for
            ref <- Signal.initRef("Nothing here yet")
            tree = uic.DataView[String]().itemTemplate(s => span(s)).emptyContent(ref).toUI
            _    <- assertChannel(tree, "p-dataview-empty-message")
            html <- renderHtml(tree)
        yield assert(html.contains("Nothing here yet"), "renders the current empty message")
    }
end TextChannelTest
