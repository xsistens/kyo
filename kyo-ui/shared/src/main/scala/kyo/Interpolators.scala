package kyo

/** The `aria` interpolator: an [[kyo.AriaKey]] from a literal with the `aria-` prefix, checked at compile time (see [[kyo.AriaKey]]).
  * `aria"aria-item-$n"` builds a key from a dynamic part, and the key is checked when it is built.
  */
extension (inline sc: StringContext)
    inline def aria(inline args: Any*): AriaKey = ${ kyo.internal.KeyMacro.aria('sc, 'args) }

/** The `data` interpolator: a [[kyo.DataKey]] from a literal with the `data-` prefix, checked at compile time (see [[kyo.DataKey]]).
  * `data"data-item-$n"` builds a key from a dynamic part, and the key is checked when it is built.
  */
extension (inline sc: StringContext)
    inline def data(inline args: Any*): DataKey = ${ kyo.internal.KeyMacro.data('sc, 'args) }
