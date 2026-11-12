package kyo.uic

import kyo.*
import kyo.UI.*

/** IconField: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * IconField/InputIcon pair: `div.p-iconfield` wrapping `span.p-inputicon`
  * icons around the field), so the extracted `@primeuix` iconfield CSS applies
  * verbatim. The CSS is position-keyed: an icon rendered BEFORE the field
  * (`.p-inputicon:first-child`) pins to the start and pads the field's start
  * edge; an icon AFTER it pins to the end.
  *
  * A single-host WRAPPER: it takes the field itself and keeps its concrete type,
  * which is what lets it stamp the padding classes onto that field. Contrast
  * [[InputGroup]], a container of finished children, and [[Label]], which stands
  * beside a field and links to it by id.
  *
  * Hosts: [[Input]] (Prime's canonical pairing), [[TextArea]], and [[Select]].
  * Prime's sheet only pads `.p-inputtext`; the TextArea and Select paddings
  * are `.p-uic-*` remainder rules with the same padding calc (a kyo extension
  * beyond Prime's CSS). On a Select host, prefer `iconStart`: the end edge
  * already hosts the dropdown chevron.
  *
  * {{{
  * uic.IconField(uic.Input().placeholder("Search")).iconStart(uic.Icons.search)
  * }}}
  */
final class IconField private[uic] (private[uic] val p: IconField.Props) extends Node, HasProps:
    import p.*

    type Self = IconField

    /** Icon pinned to the field's start edge (`.p-inputicon:first-child`). */
    def iconStart(glyph: IconGlyph): IconField = new IconField(p.copy(startIconV = Present(glyph)))

    /** Icon pinned to the field's end edge (`.p-inputicon:last-child`). */
    def iconEnd(glyph: IconGlyph): IconField = new IconField(p.copy(endIconV = Present(glyph)))

    private[uic] def render(using Frame): UI =
        val startSlot: List[UI] = startIconV.toList.map(g => GlyphSvg(g, "p-inputicon"))
        val endSlot: List[UI]   = endIconV.toList.map(g => GlyphSvg(g, "p-inputicon"))
        // Prime pads the field via sibling-position selectors (.p-inputtext:not(:first-child));
        // a two-way bound kyo field renders inside a classless reactive <span>, which
        // resets those positions, so stamp deterministic padding classes instead (the
        // remainder CSS keys on them with Prime's own padding calc).
        val classes =
            startIconV.toList.map(_ => "p-uic-iconfield-start") ++
                endIconV.toList.map(_ => "p-uic-iconfield-end")
        val field: UI = hostV match
            case IconField.Host.In(i)  => Node.placed(classes.foldLeft(i)(_.extraClass(_)))
            case IconField.Host.Ta(t)  => Node.placed(classes.foldLeft(t)(_.extraClass(_)))
            case IconField.Host.Sel(s) => classes.foldLeft[Select[?]](s)(_.extraClass(_)).render
        div.cssClass("p-iconfield")(
            UI.children(startSlot ++ List(field) ++ endSlot)*
        )
    end render
end IconField

object IconField:
    final private[uic] case class Props(
        hostV: IconField.Host,
        startIconV: Maybe[IconGlyph] = Absent,
        endIconV: Maybe[IconGlyph] = Absent
    )

    /** The wrapped field (Input, TextArea, or Select). */
    private[uic] enum Host:
        case In(i: Input)
        case Ta(t: TextArea)
        case Sel(s: Select[?])
    end Host

    /** Wraps `input`; add icons via [[IconField.iconStart]]/[[IconField.iconEnd]]. */
    def apply(input: Input): IconField = new IconField(Props(Host.In(input)))

    /** Wraps `textArea` (kyo extension beyond Prime's input-only CSS). */
    def apply(textArea: TextArea): IconField = new IconField(Props(Host.Ta(textArea)))

    /** Wraps `select` (kyo extension; prefer `iconStart`, since the end edge hosts the
      * dropdown chevron).
      */
    def apply(select: Select[?]): IconField = new IconField(Props(Host.Sel(select)))
end IconField
