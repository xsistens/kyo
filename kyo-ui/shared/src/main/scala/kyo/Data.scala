package kyo

/** The key of a `data-*` attribute: the part after `data-`, as the entries of `UI.Ast.Element.data` name it.
  *
  * A key is built with the `data` interpolator, which checks it at compile time: `data"data-vs-first"` is the key `vs-first`. The literal
  * includes the `data-` prefix. Each static part must hold only lowercase letters, digits and hyphens, and the name must start with a
  * letter and must not start with `kyo-`, the prefix the framework reserves for its own attributes. A dynamic part
  * (`data"data-item-$n"`) is not known until run time, so the whole key is checked again when it is built, with the same rules, and an
  * invalid one throws. A plain string is not a key: a name held in a `String` is written as `data"data-$name"`.
  *
  * The type is a `String` underneath, so a key costs nothing beyond the string.
  */
opaque type Data = String

object Data:

    /** The attribute name a key stands for, the part after `data-`. */
    private[kyo] def name(key: Data): String = key

end Data
