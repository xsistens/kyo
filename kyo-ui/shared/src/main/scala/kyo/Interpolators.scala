package kyo

/** The `aria` interpolator: an [[kyo.Aria]] from a literal with the `aria-` prefix, checked at compile time (see [[kyo.Aria]]).
  * `aria"aria-item-$n"` builds a key from a dynamic part, and the key is checked when it is built.
  */
extension (inline sc: StringContext)
    inline def aria(inline args: Any*): Aria = ${ kyo.internal.KeyMacro.aria('sc, 'args) }

/** The `data` interpolator: a [[kyo.Data]] from a literal with the `data-` prefix, checked at compile time (see [[kyo.Data]]).
  * `data"data-item-$n"` builds a key from a dynamic part, and the key is checked when it is built.
  */
extension (inline sc: StringContext)
    inline def data(inline args: Any*): Data = ${ kyo.internal.KeyMacro.data('sc, 'args) }
