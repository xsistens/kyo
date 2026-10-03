package kyo.uic

import kyo.*
import kyo.UI.*

/** Badge — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's Badge
  * anatomy: `span.p-badge.p-component[.p-badge-<severity>][.p-badge-sm|-lg|-xl]
  * [.p-badge-dot][.p-badge-circle]` with the value text), so the extracted
  * `@primeuix` badge CSS applies verbatim.
  *
  * `Badge()` without a value renders the `.p-badge-dot` status dot; a
  * single-character value gets `.p-badge-circle` automatically (Prime
  * semantics). Without a `severity`, the base `.p-badge` skin renders the
  * primary chip; Prime's badge vocabulary is
  * success/info/warn/danger/secondary/contrast — `Primary` (and `Help`) keep
  * the unsuffixed base skin.
  */
final case class Badge private (
    valueV: Maybe[TextValue] = Absent,
    severityV: Maybe[SeverityValue] = Absent,
    sizeV: ExtendedSize = Size.Normal,
    hostClassesV: List[String] = Nil
) extends Node:
    type Self = Badge

    /** Extra class on the badge root, for a host that positions it (the Paginator
      * precedent): DataTable stamps `.p-datatable-sort-badge` on the multi-sort ordinal.
      */
    private[uic] def hostClass(cls: String): Badge = copy(hostClassesV = hostClassesV :+ cls)

    /** Semantic accent (`.p-badge-<token>`); unset keeps the primary base skin. A `Signal[Severity]` swaps
      * the class IN PLACE via kyo-ui's class channel on emission (no re-render), e.g. a status badge going
      * success to danger.
      */
    def severity(v: Severity | Signal[Severity]): Badge = copy(severityV = Present(ReactiveValue(v)))

    /** Size: `.p-badge-sm` / default / `.p-badge-lg` / `.p-badge-xl`. */
    def size(v: ExtendedSize): Badge = copy(sizeV = v)

    /** The `.p-badge-<token>` suffix for a severity; `Primary`/`Help` render the unsuffixed base skin. */
    private def badgeToken(s: Severity): Maybe[String] = s match
        case Severity.Success   => Present("success")
        case Severity.Info      => Present("info")
        case Severity.Warn      => Present("warn")
        case Severity.Danger    => Present("danger")
        case Severity.Secondary => Present("secondary")
        case Severity.Contrast  => Present("contrast")
        case _                  => Absent

    private[uic] def render(using Frame): UI =
        var el = span.cssClass("p-badge").cssClass("p-component")
        // The single-character disc key off a constant value's length; a reactive
        // value's length is unknown at render time, so it never takes the circle.
        valueV match
            case Present(TextValue.Const(v)) if v.length == 1 => el = el.cssClass("p-badge-circle")
            case Absent                                       => el = el.cssClass("p-badge-dot")
            case _                                            => ()
        end match
        severityV match
            case Present(SeverityValue.Const(s)) => badgeToken(s).foreach(t => el = el.cssClass(s"p-badge-$t"))
            case Present(SeverityValue.Dyn(sig)) =>
                // One reactive class per token-bearing severity; only the active one is present, swapped in place.
                Severity.values.foreach(s => badgeToken(s).foreach(t => el = el.cssClass(s"p-badge-$t", sig.map(_ == s))))
            case Absent => ()
        end match
        sizeV match
            case Size.Small  => el = el.cssClass("p-badge-sm")
            case Size.Large  => el = el.cssClass("p-badge-lg")
            case Size.XLarge => el = el.cssClass("p-badge-xl")
            case Size.Normal => ()
        end match
        hostClassesV.foreach(c => el = el.cssClass(c))
        valueV match
            case Present(TextValue.Const(v)) => el(v)
            case Present(TextValue.Dyn(s))   => el(s)
            case Absent                      => el()
        end match
    end render
end Badge

object Badge:
    /** A badge showing `value`. A `Signal[String]` re-renders it in place on emission; the single-character
      * `.p-badge-circle` disc is not applied to a reactive value, since its length is unknown at render
      * time.
      */
    def apply(value: String | Signal[String]): Badge = new Badge(valueV = Present(ReactiveValue(value)))

    /** A value-less badge — the `.p-badge-dot` status dot. */
    def apply(): Badge = new Badge()
end Badge

/** OverlayBadge — native kyo-ui, PrimeOne design (mirrors PrimeVue/PrimeReact's
  * OverlayBadge anatomy: `div.p-overlaybadge` wrapping ONE child plus the
  * [[Badge]], which the overlaybadge CSS pins to the child's top-end corner
  * with the outline ring), so the extracted `@primeuix` overlaybadge CSS
  * applies verbatim. Prime puts no `p-component` on this root — neither do we.
  *
  * The kyo remainder makes the root an inline-flex row (Prime relies on a
  * plain block `div`; kyo's base CSS would make it a stretching column
  * flexbox, which mispositions the corner badge).
  */
final case class OverlayBadge private (
    childV: UI,
    badgeV: Maybe[Badge] = Absent
) extends Node:
    type Self = OverlayBadge

    /** The [[Badge]] overlaid on the child's top-end corner. */
    def badge(b: Badge): OverlayBadge = copy(badgeV = Present(b))

    private[uic] def render(using Frame): UI =
        div.cssClass("p-overlaybadge")(
            (childV :: badgeV.toList.map(_.render)).map(toChild)*
        )
end OverlayBadge

object OverlayBadge:
    /** Wraps `child`; attach the overlay via [[OverlayBadge.badge]]. */
    def apply(child: UI): OverlayBadge = new OverlayBadge(child)

    /** Wraps `child` with `badge` overlaid on its top-end corner. */
    def apply(child: UI)(badge: Badge): OverlayBadge = new OverlayBadge(child, Present(badge))
end OverlayBadge
