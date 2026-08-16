package kyo.uic

/** The harmonized cross-component vocabulary (PrimeOne semantics). One coherent
  * user-facing set; each component maps cases to its own `.p-*` CSS suffix
  * privately, absorbing Prime's internal inconsistencies (button `danger` vs
  * message `error`).
  *
  * "No accent" is expressed as `Maybe[Severity]` (`Absent`), never a `None`
  * enum case — form-field validation is the separate boolean `invalid(...)`
  * plus `invalidMessage(...)`, exactly like Prime's `.p-invalid` model.
  */
enum Severity derives CanEqual:
    case Primary, Secondary, Success, Info, Warn, Danger, Help, Contrast

    /** The `.p-<component>-<token>` suffix shared by most components. */
    private[uic] def token: String = this match
        case Severity.Primary   => "primary"
        case Severity.Secondary => "secondary"
        case Severity.Success   => "success"
        case Severity.Info      => "info"
        case Severity.Warn      => "warn"
        case Severity.Danger    => "danger"
        case Severity.Help      => "help"
        case Severity.Contrast  => "contrast"
end Severity

/** Button rendering variant (`.p-button-outlined` / `-text` / `-link`). */
enum ButtonVariant derives CanEqual:
    case Filled, Outlined, Text, Link

/** Form-field fill variant (`.p-variant-filled`). */
enum FieldVariant derives CanEqual:
    case Outlined, Filled

/** Component size (`.p-*-sm` / default / `.p-*-lg`; XLarge only where the
  * design system defines it, e.g. Avatar).
  */
enum Size derives CanEqual:
    case Small, Normal, Large, XLarge

/** Selection semantics shared by list-like components (Checkbox/Radio are
  * DataTable-only affordances).
  */
enum SelectionMode derives CanEqual:
    case None, Single, Multiple, Checkbox, Radio

/** Package-internal builder for the `invalidMessage` row shared by the migrated
  * form fields: when the field is `invalid` AND a message is set, the field is
  * followed by a `div.p-uic-invalid-message` (kyo extension — Prime leaves the
  * message to the form layer). Emitted as a fragment, so no wrapper element is
  * introduced around the field.
  */
private[uic] object FieldInvalid:
    import kyo.*
    import kyo.UI.*

    def withMessage(field: UI, invalid: Boolean, message: Maybe[String])(using Frame): UI =
        message match
            case Present(m) if invalid => fragment(field, div.cssClass("p-uic-invalid-message")(m))
            case _                     => field

    /** Reactive validity seam shared by every form field's `invalid(Signal)` /
      * `invalidMessage(Signal)` overloads. Derives the red state (explicit override,
      * else message presence) and the message text from the reactive slots, then
      * hands both to `build` inside a nested `Signal.render` boundary — `build`
      * re-renders the field for that `(red, message)` snapshot so the `.p-invalid`
      * stamp and the message row stay in sync. The const path never reaches here, so
      * static output (golden tests) is unaffected. Callers on mounted fields must
      * invoke this INSIDE their mount subscription, never around the `UI.mounted`
      * node, to avoid re-mounting on every emission.
      */
    def reactive(invalidDyn: Maybe[Signal[Boolean]], msgDyn: Maybe[Signal[Maybe[String]]], msgConst: Maybe[String])(
        build: (Boolean, Maybe[String]) => UI
    )(using Frame): UI =
        val msgSig: Signal[Maybe[String]] = msgDyn.getOrElse(Signal.initConst(msgConst))
        val redSig: Signal[Boolean]       = invalidDyn.getOrElse(msgSig.map(_.isDefined))
        redSig.render(red => msgSig.render(msg => build(red, msg)))
    end reactive
end FieldInvalid

/** Layout orientation (`.p-*-horizontal` / `.p-*-vertical`; MeterGroup's meters
  * and label list — Prime's `orientation` / `labelOrientation` props).
  */
enum Orientation derives CanEqual:
    case Horizontal, Vertical

    private[uic] def token: String = this match
        case Orientation.Horizontal => "horizontal"
        case Orientation.Vertical   => "vertical"
end Orientation

/** Placement of a secondary block relative to the main one (MeterGroup's label
  * list — Prime's `labelPosition="start"|"end"`).
  */
enum LabelPosition derives CanEqual:
    case Start, End

/** DatePicker picking granularity (Prime's `view="date"|"month"|"year"`): which
  * grid picks the VALUE — day cells (ISO `YYYY-MM-DD`), month cells (`YYYY-MM`),
  * or year cells (`YYYY`).
  */
enum DatePickerView derives CanEqual:
    case Date, Month, Year

/** DatePicker hour display format (Prime's `hourFormat="24"|"12"`); `H12` adds
  * the AM/PM picker.
  */
enum HourFormat derives CanEqual:
    case H24, H12

/** Anchoring for positioned overlays (Toast, ConfirmDialog, Dialog). */
enum OverlayPosition derives CanEqual:
    case TopLeft, TopCenter, TopRight, BottomLeft, BottomCenter, BottomRight, Center

    private[uic] def token: String = this match
        case OverlayPosition.TopLeft      => "top-left"
        case OverlayPosition.TopCenter    => "top-center"
        case OverlayPosition.TopRight     => "top-right"
        case OverlayPosition.BottomLeft   => "bottom-left"
        case OverlayPosition.BottomCenter => "bottom-center"
        case OverlayPosition.BottomRight  => "bottom-right"
        case OverlayPosition.Center       => "center"
end OverlayPosition
