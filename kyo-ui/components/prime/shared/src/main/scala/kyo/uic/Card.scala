package kyo.uic

import kyo.*
import kyo.UI.*

/** Card: native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Card
  * anatomy: `div.p-card` > optional `div.p-card-header` (custom header slot) +
  * `div.p-card-body` > optional `div.p-card-caption` (`.p-card-title` +
  * `.p-card-subtitle`) + `div.p-card-content` + optional `div.p-card-footer`),
  * so the extracted `@primeuix` card CSS applies verbatim.
  *
  * kyo extensions beyond Prime's slots live in the caption row (`p-uic-card-*`
  * remainder classes): `headerAvatar`/`headerAction` flank the caption,
  * `additionalText` right-aligns status text, and `interactive`/`onHeaderClick`
  * make the caption row a focusable `role="button"` activated by
  * click/Enter/Space.
  */
final class Card private[uic] (private[uic] val p: Card.Props) extends Node, HasAccessibleNameRef, HasProps:
    import p.*

    type Self = Card

    /** Card title. A `Signal[String]` re-renders it in place on emission (e.g. a locale-driven `I18n.t`
      * leaf).
      */
    def title(v: String | Signal[String]): Card = new Card(p.copy(titleText = Present(ReactiveValue(v))))

    /** Subtitle under the title. A `Signal[String]` re-renders it in place on emission.
      */
    def subtitle(v: String | Signal[String]): Card = new Card(p.copy(subtitleText = Present(ReactiveValue(v))))

    /** Right-aligned status text in the caption row (kyo extension, `p-uic-card-additional`). A
      * `Signal[String]` re-renders it in place on emission (e.g. a locale-driven `I18n.t` leaf).
      */
    def additionalText(v: String | Signal[String]): Card = new Card(p.copy(additionalTextV = Present(ReactiveValue(v))))

    /** Avatar slot rendered before the caption (kyo extension, `p-uic-card-avatar`). */
    def headerAvatar(v: UI): Card = new Card(p.copy(headerAvatarV = Present(v)))

    /** Action slot rendered at the caption row's far end (kyo extension, `p-uic-card-action`). */
    def headerAction(v: UI): Card = new Card(p.copy(headerActionV = Present(v)))

    /** Makes the caption row a focusable button activated by click/Enter/Space. */
    def interactive(v: Boolean): Card = new Card(p.copy(headerInteractiveFlag = v))

    /** Runs `action` when the interactive caption row is activated. Implies [[interactive]]. */
    def onHeaderClick(action: => UI.Action)(using Frame): Card =
        new Card(p.copy(onHeaderClickEff = Present(Callback(action)), headerInteractiveFlag = true))

    /** Custom header slot rendered as `div.p-card-header` above the body (Prime's
      * header slot, typically an image or free-form UI).
      */
    def header(v: UI): Card = new Card(p.copy(customHeaderV = Present(v)))

    /** Footer slot rendered as `div.p-card-footer` at the body's end (Prime's footer slot). */
    def footer(v: UI): Card = new Card(p.copy(footerV = Present(v)))

    private[uic] def accessibility: Accessibility              = access
    private[uic] def withAccessibility(v: Accessibility): Self = new Card(p.copy(access = v))

    /** Adds content children (rendered inside `div.p-card-content`). */
    def apply(cs: UI*): Card = new Card(p.copy(kids = kids ++ cs))

    private[uic] def render(using Frame): UI =
        val headerUI: List[UI] =
            customHeaderV.toList.map(h => div.cssClass("p-card-header")(h))
        val contentUI: UI      = div.cssClass("p-card-content")(UI.children(kids)*)
        val footerUI: List[UI] =
            footerV.toList.map(f => div.cssClass("p-card-footer")(f))
        val body: UI =
            div.cssClass("p-card-body")(UI.children(captionArea ++ (contentUI :: footerUI))*)

        var el = div.cssClass("p-card").cssClass("p-component")
        if access.name.isDefined || access.nameRef.isDefined then el = el.role("region")
        access.name.foreach(n => el = el.aria("label" -> n))
        access.nameRef.foreach(r => el = el.aria("labelledby" -> r))
        el(UI.children(headerUI :+ body)*)
    end render

    /** The caption area: Prime's `p-card-caption` (title + subtitle), wrapped in
      * the `p-uic-card-caption-row` when kyo's avatar/action/additionalText
      * extensions are present. Empty when no caption field is set.
      */
    private def captionArea(using Frame): List[UI] =
        val hasCaption = titleText.isDefined || subtitleText.isDefined
        val hasExtras  = headerAvatarV.isDefined || headerActionV.isDefined || additionalTextV.isDefined
        if !hasCaption && !hasExtras && !headerInteractiveFlag then Nil
        else
            val titleUI: List[UI] = titleText.toList.map {
                case TextValue.Const(t) => div.cssClass("p-card-title")(t): UI
                case TextValue.Dyn(s)   => div.cssClass("p-card-title")(s)
            }
            val subtitleUI: List[UI] = subtitleText.toList.map {
                case TextValue.Const(t) => div.cssClass("p-card-subtitle")(t): UI
                case TextValue.Dyn(s)   => div.cssClass("p-card-subtitle")(s)
            }
            val caption: UI = div.cssClass("p-card-caption")(UI.children(titleUI ++ subtitleUI)*)
            if !hasExtras && !headerInteractiveFlag then List(caption)
            else if !hasExtras then List(interactivize(div.cssClass("p-uic-card-caption-row"))(caption))
            else
                val avatarUI: List[UI] =
                    headerAvatarV.toList.map(av => div.cssClass("p-uic-card-avatar")(av))
                val additionalUI: List[UI] =
                    additionalTextV.toList.map {
                        case TextValue.Const(t) => div.cssClass("p-uic-card-additional")(t): UI
                        case TextValue.Dyn(s)   => div.cssClass("p-uic-card-additional")(s)
                    }
                val actionUI: List[UI] =
                    headerActionV.toList.map(ac => div.cssClass("p-uic-card-action")(ac))
                val row = interactivize(div.cssClass("p-uic-card-caption-row"))
                List(row(UI.children(avatarUI ++ (caption :: additionalUI) ++ actionUI)*))
            end if
        end if
    end captionArea

    /** Adds the button semantics + click/Enter/Space activation to the caption row. */
    private def interactivize(el: Ast.Div)(using Frame): Ast.Div =
        if !headerInteractiveFlag then el
        else
            val interactive =
                el.cssClass("p-uic-card-caption-row--interactive").role("button").tabIndex(0)
            onHeaderClickEff match
                case Present(e) =>
                    interactive
                        .onClick(e)
                        .onKeyDown { evt =>
                            evt.key match
                                case Keyboard.Enter | Keyboard.Space => e
                                case _                               => ()
                        }
                case Absent => interactive
            end match
end Card

object Card:
    final private[uic] case class Props(
        titleText: Maybe[TextValue] = Absent,
        subtitleText: Maybe[TextValue] = Absent,
        additionalTextV: Maybe[TextValue] = Absent,
        headerAvatarV: Maybe[UI] = Absent,
        headerActionV: Maybe[UI] = Absent,
        headerInteractiveFlag: Boolean = false,
        onHeaderClickEff: Maybe[UI.Action] = Absent,
        customHeaderV: Maybe[UI] = Absent,
        footerV: Maybe[UI] = Absent,
        access: Accessibility = Accessibility.none,
        kids: List[UI] = Nil
    )

    def apply(): Card = new Card(Props())
end Card
