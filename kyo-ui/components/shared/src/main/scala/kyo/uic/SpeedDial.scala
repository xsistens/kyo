package kyo.uic

import kyo.*
import kyo.UI.*

/** Direction the [[SpeedDial]] action fan opens towards (Prime's linear
  * `direction` prop).
  */
enum SpeedDialDirection derives CanEqual:
    case Up, Down, Left, Right

    /** The `.p-speeddial-<token>` modifier suffix. */
    private[uic] def token: String = this match
        case SpeedDialDirection.Up    => "up"
        case SpeedDialDirection.Down  => "down"
        case SpeedDialDirection.Left  => "left"
        case SpeedDialDirection.Right => "right"
end SpeedDialDirection

/** SpeedDial — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * SpeedDial anatomy: `div.p-speeddial.p-component.p-speeddial-<direction>
  * [.p-speeddial-open]` > toggle `button.p-speeddial-button.p-speeddial-rotate`
  * (a rounded icon [[Button]]; the sheet rotates the plus glyph 45° while open)
  * + `ul.p-speeddial-list` of `li.p-speeddial-item` action buttons), so the
  * extracted `@primeuix` speeddial CSS applies verbatim — closed items sit at
  * `scale(0)` with `pointer-events: none`, opening scales them in. Prime lays
  * the linear directions out via inline styles; the theme glue expresses them
  * as `.p-speeddial-<direction>` CSS (the ToggleSwitch precedent).
  *
  * Actions are [[MenuItem]]s rendered as rounded secondary icon Buttons whose
  * `label` becomes the accessible name and native tooltip; `onSelect` runs and
  * the fan closes. `open(ref)` binds the fan two-way (self-managed otherwise).
  *
  * Honest deferrals: only the LINEAR type (circle/semi-circle/quarter-circle
  * need JS-measured per-item offsets); the fan-in/out transition is omitted
  * (kyo re-renders replace the subtree, restarting CSS transitions); no mask
  * variant (Prime styles the mask via inline styles and a JS-managed backdrop);
  * no Escape/outside-click dismissal (the fan is not an Overlay — close by
  * toggling).
  */
final case class SpeedDial private (
    itemsV: List[MenuItem] = Nil,
    directionV: SpeedDialDirection = SpeedDialDirection.Up,
    openRefV: Maybe[SignalRef[Boolean]] = Absent,
    disabledFlag: Boolean = false,
    accNameV: Maybe[String] = Absent
) extends Node:
    type Self = SpeedDial

    /** Appends actions ([[MenuItem]] — `icon` + `label` + `onSelect`). */
    def items(is: MenuItem*): SpeedDial = copy(itemsV = itemsV ++ is.toList)

    /** Fan direction (default `Up`). */
    def direction(v: SpeedDialDirection): SpeedDial = copy(directionV = v)

    /** Binds the fan visibility two-way to `ref` (optional — self-managed
      * otherwise).
      */
    def open(ref: SignalRef[Boolean]): SpeedDial = copy(openRefV = Present(ref))

    /** Disables the toggle (and thereby the fan). */
    def disabled(v: Boolean): SpeedDial = copy(disabledFlag = v)

    /** Accessible name of the toggle button (`aria-label`). */
    def accessibleName(v: String): SpeedDial = copy(accNameV = Present(v))

    private[uic] def render(using Frame): UI =
        val stat: UI = body(false, Absent)
        UI.mounted {
            for
                open <- openRefV match
                    case Present(r) => Kyo.lift(r)
                    case Absent     => Signal.initRef(false)
            yield wired(open)
        }.placeholder(stat)
    end render

    /** The subscription tree the mount publishes (golden-test seam). */
    private[uic] def wired(open: SignalRef[Boolean])(using Frame): UI =
        open.render(o => body(o, Present(open)))

    private def body(isOpen: Boolean, open: Maybe[SignalRef[Boolean]])(using Frame): UI =
        // Explicitly typed effect: an untyped match would infer `Any` and land the
        // effect inert in the by-name handler.
        val toggle: Any < Async =
            open match
                case Present(r) => r.getAndUpdate(!_)
                case Absent     => ()

        var toggleBtn = Button()
            .icon(Icons.plus)
            .rounded(true)
            .extraClass("p-speeddial-button")
            .extraClass("p-speeddial-rotate")
            .accessibleName(accNameV.getOrElse("Toggle actions"))
            .ariaRaw("haspopup", "menu")
            .ariaRaw("expanded", isOpen.toString)
            .disabled(disabledFlag)
        if !disabledFlag then toggleBtn = toggleBtn.onClick(toggle)

        val actionUIs: List[UI] = itemsV.map { it =>
            def activate: Any < Async =
                open match
                    case Present(r) =>
                        for
                            _ <- it.actionV match
                                case Present(eff) => eff
                                case Absent       => (): Any < Async
                            _ <- r.set(false)
                        yield ()
                    case Absent => ()
            var btn = Button()
                .rounded(true)
                .severity(Severity.Secondary)
                .size(Size.Small)
                .accessibleName(it.labelV.constOrEmpty)
                .tooltip(it.labelV.constOrEmpty)
                .disabled(it.disabledFlag)
            it.iconV.foreach(g => btn = btn.icon(g))
            if !it.disabledFlag && open.isDefined then btn = btn.onClick(activate)
            li.cssClass("p-speeddial-item").role("presentation")(toChild(btn: UI))
        }

        var el = div
            .cssClass("p-speeddial")
            .cssClass("p-component")
            .cssClass(s"p-speeddial-${directionV.token}")
        if isOpen then el = el.cssClass("p-speeddial-open")
        el(
            toChild(toggleBtn: UI),
            toChild(ul.cssClass("p-speeddial-list").role("menu")(actionUIs.map(toChild)*))
        )
    end body
end SpeedDial

object SpeedDial:
    /** An empty speed dial — add actions via [[SpeedDial.items]]. */
    def apply(): SpeedDial = new SpeedDial()
