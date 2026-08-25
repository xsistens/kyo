package kyo.uic

import kyo.*
import kyo.UI.*

/** Card — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Card
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
final case class Card private (
    titleText: Maybe[TextValue] = Absent,
    subtitleText: Maybe[TextValue] = Absent,
    additionalTextV: Maybe[TextValue] = Absent,
    headerAvatarV: Maybe[UI] = Absent,
    headerActionV: Maybe[UI] = Absent,
    headerInteractiveFlag: Boolean = false,
    onHeaderClickEff: Maybe[Any < Async] = Absent,
    customHeaderV: Maybe[UI] = Absent,
    footerV: Maybe[UI] = Absent,
    accessibleNameV: Maybe[TextValue] = Absent,
    accessibleNameRefV: Maybe[String] = Absent,
    kids: List[UI] = Nil
) extends Node:
    type Self = Card

    def title(v: String): Card    = copy(titleText = Present(TextValue.Const(v)))
    def subtitle(v: String): Card = copy(subtitleText = Present(TextValue.Const(v)))

    /** Reactive title that tracks `sig` — re-renders in place on emission (e.g. a
      * locale-driven `I18n.t` leaf).
      */
    def title(sig: Signal[String]): Card = copy(titleText = Present(TextValue.Dyn(sig)))

    /** Reactive subtitle that tracks `sig` — re-renders in place on emission. */
    def subtitle(sig: Signal[String]): Card = copy(subtitleText = Present(TextValue.Dyn(sig)))

    /** Right-aligned status text in the caption row (kyo extension — `p-uic-card-additional`). */
    def additionalText(v: String): Card = copy(additionalTextV = Present(TextValue.Const(v)))

    /** Reactive `additionalText` tracking `sig` — re-renders in place on emission (e.g. a
      * locale-driven `I18n.t` leaf).
      */
    def additionalText(sig: Signal[String]): Card = copy(additionalTextV = Present(TextValue.Dyn(sig)))

    /** Avatar slot rendered before the caption (kyo extension — `p-uic-card-avatar`). */
    def headerAvatar(v: UI): Card = copy(headerAvatarV = Present(v))

    /** Action slot rendered at the caption row's far end (kyo extension — `p-uic-card-action`). */
    def headerAction(v: UI): Card = copy(headerActionV = Present(v))

    /** Makes the caption row a focusable button activated by click/Enter/Space. */
    def interactive(v: Boolean): Card = copy(headerInteractiveFlag = v)

    /** Runs `action` when the interactive caption row is activated. Implies [[interactive]]. */
    def onHeaderClick(action: => Any < Async)(using Frame): Card =
        copy(onHeaderClickEff = Present(Sync.defer(action)), headerInteractiveFlag = true)

    /** Custom header slot rendered as `div.p-card-header` above the body (Prime's
      * header slot — typically an image or free-form UI).
      */
    def header(v: UI): Card = copy(customHeaderV = Present(v))

    /** Footer slot rendered as `div.p-card-footer` at the body's end (Prime's footer slot). */
    def footer(v: UI): Card = copy(footerV = Present(v))

    /** Accessible name announced for the card region (`aria-label`). */
    def accessibleName(v: String): Card = copy(accessibleNameV = Present(TextValue.Const(v)))

    /** Reactive accessible name — `aria-label` patched IN PLACE via kyo-ui's attribute
      * channel (`setAttribute`, no re-render).
      */
    def accessibleName(sig: Signal[String]): Card = copy(accessibleNameV = Present(TextValue.Dyn(sig)))

    /** ID reference(s) of the element(s) that label the card (`aria-labelledby`). */
    def accessibleNameRef(v: String): Card = copy(accessibleNameRefV = Present(v))

    /** Adds content children (rendered inside `div.p-card-content`). */
    def apply(cs: UI*): Card = copy(kids = kids ++ cs)

    private[uic] def render(using Frame): UI =
        val headerUI: List[UI] =
            customHeaderV.toList.map(h => div.cssClass("p-card-header")(toChild(h)))
        val contentUI: UI = div.cssClass("p-card-content")(kids.map(toChild)*)
        val footerUI: List[UI] =
            footerV.toList.map(f => div.cssClass("p-card-footer")(toChild(f)))
        val body: UI =
            div.cssClass("p-card-body")((captionArea ++ (contentUI :: footerUI)).map(toChild)*)

        var el = div.cssClass("p-card").cssClass("p-component")
        if accessibleNameV.isDefined || accessibleNameRefV.isDefined then el = el.role("region")
        accessibleNameV match
            case Present(TextValue.Const(n)) => el = el.aria("label", n)
            case Present(TextValue.Dyn(s))   => el = el.aria("label", s)
            case Absent                      => ()
        end match
        accessibleNameRefV.foreach(r => el = el.aria("labelledby", r))
        el((headerUI :+ body).map(toChild)*)
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
                case TextValue.Dyn(s)   => s.render(t => div.cssClass("p-card-title")(t))
            }
            val subtitleUI: List[UI] = subtitleText.toList.map {
                case TextValue.Const(t) => div.cssClass("p-card-subtitle")(t): UI
                case TextValue.Dyn(s)   => s.render(t => div.cssClass("p-card-subtitle")(t))
            }
            val caption: UI = div.cssClass("p-card-caption")((titleUI ++ subtitleUI).map(toChild)*)
            if !hasExtras && !headerInteractiveFlag then List(caption)
            else if !hasExtras then List(interactivize(div.cssClass("p-uic-card-caption-row"))(toChild(caption)))
            else
                val avatarUI: List[UI] =
                    headerAvatarV.toList.map(av => div.cssClass("p-uic-card-avatar")(toChild(av)))
                val additionalUI: List[UI] =
                    additionalTextV.toList.map {
                        case TextValue.Const(t) => div.cssClass("p-uic-card-additional")(t): UI
                        case TextValue.Dyn(s)   => s.render(t => div.cssClass("p-uic-card-additional")(t))
                    }
                val actionUI: List[UI] =
                    headerActionV.toList.map(ac => div.cssClass("p-uic-card-action")(toChild(ac)))
                val row = interactivize(div.cssClass("p-uic-card-caption-row"))
                List(row((avatarUI ++ (caption :: additionalUI) ++ actionUI).map(toChild)*))
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
    def apply(): Card = new Card()
