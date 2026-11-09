package kyo.uic

import kyo.*

/** Const-or-reactive carrier for a component's text slot: either a constant `String` or a
  * `Signal[String]` that tracks a downstream reactive source (a locale-driven `I18n.t` leaf).
  * The `String` specialization of the generic [[ReactiveValue]]; generalized to any text-bearing
  * slot (button/tag/label text; input placeholder; `aria-label`/`aria-description`; `title`).
  *
  * How a `Dyn` slot updates depends on WHERE the text lives:
  *   - a TEXT-NODE slot (button/tag/label content) re-renders only its own reactive boundary
  *     via `Signal.render`;
  *   - an ATTRIBUTE slot (`placeholder`, `aria-*`, `title`) is patched IN PLACE via kyo-ui's
  *     reactive-attribute channel, with no re-render.
  */
private[uic] type TextValue = ReactiveValue[String]

/** Companion forwarding the [[ReactiveValue]] cases, so `TextValue.Const(...)` / `TextValue.Dyn(...)`
  * construction and pattern matching read against the alias.
  */
private[uic] object TextValue:
    export ReactiveValue.Const
    export ReactiveValue.Dyn
end TextValue

extension (tv: TextValue)

    /** The constant text, or `""` for a reactive slot, for the few consumers that must collapse
      * the slot to a plain `String` (a computed accessible-name/tooltip fallback that fuses the
      * text with other state, an image `alt`). A `Dyn` value is dropped; use the reactive
      * render/channel path instead when in-place updates are needed.
      */
    private[uic] def constOrEmpty: String = tv match
        case ReactiveValue.Const(v) => v
        case ReactiveValue.Dyn(_)   => ""
end extension
