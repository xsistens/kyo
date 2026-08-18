package kyo.uic

import kyo.*
import kyo.UI.*

/** Const-or-reactive carrier for a component's text slot — either a constant `String` or a
  * `Signal[String]` that tracks a downstream reactive source (a locale-driven `I18n.t` leaf).
  * The `String` specialization of the generic [[ReactiveValue]]; generalized to any text-bearing
  * slot (button/tag/label text; input placeholder; `aria-label`/`aria-description`; `title`).
  *
  * How a `Dyn` slot updates depends on WHERE the text lives:
  *   - a TEXT-NODE slot (button/tag/label content) re-renders only its own reactive boundary
  *     via `Signal.render`;
  *   - an ATTRIBUTE slot (`placeholder`, `aria-*`, `title`) is patched IN PLACE via kyo-ui's
  *     reactive-attribute channel — no re-render.
  */
private[uic] type TextValue = ReactiveValue[String]

/** Companion forwarding the [[ReactiveValue]] cases, so `TextValue.Const(...)` / `TextValue.Dyn(...)`
  * construction and pattern matching read against the alias.
  */
private[uic] object TextValue:
    export ReactiveValue.Const
    export ReactiveValue.Dyn

    /** Resolve a possibly-reactive text slot INSIDE a subscription: a `Dyn` slot re-renders `rebuild`
      * with the current `String` on every emission; a `Const`/`Absent` slot builds once. The plain-text
      * analogue of [[BoolValue.reactive]] — for text WOVEN with other build-time state (a mounted
      * select's placeholder fused with the selection label), where the in-place attribute channel does
      * not apply because the text also drives sibling classes.
      */
    def reactive(tv: Maybe[TextValue])(rebuild: Maybe[String] => UI)(using Frame): UI =
        tv match
            case Present(Dyn(sig)) => sig.render(v => rebuild(Present(v)))
            case Present(Const(v)) => rebuild(Present(v))
            case Absent            => rebuild(Absent)
end TextValue

extension (tv: TextValue)
    /** The constant text, or `""` for a reactive slot — for the few consumers that must collapse
      * the slot to a plain `String` (a computed accessible-name/tooltip fallback that fuses the
      * text with other state, an image `alt`). A `Dyn` value is dropped; use the reactive
      * render/channel path instead when in-place updates are needed.
      */
    private[uic] def constOrEmpty: String = tv match
        case ReactiveValue.Const(v) => v
        case ReactiveValue.Dyn(_)   => ""
end extension
