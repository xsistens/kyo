package kyo

/** The key of an `aria-*` attribute: the part after `aria-`, as the entries of `UI.Ast.Element.aria` name it.
  *
  * A key is built with the `aria` interpolator, which checks it at compile time: `aria"aria-label"` is the key `label`. The literal
  * includes the `aria-` prefix. Each static part must hold only lowercase letters, digits and hyphens, and the name must start with a
  * letter. A dynamic part (`aria"aria-item-$n"`) is not known until run time, so the whole key is checked again when it is built, with
  * the same rules, and an invalid one throws. A plain string is not a key: a name held in a `String` is written as `aria"aria-$name"`.
  *
  * The type is a `String` underneath, so a key costs nothing beyond the string. The companion holds the standard keys the library uses,
  * so a call can name one with the compiler's help: `Aria.disabled -> true`.
  */
opaque type Aria = String

object Aria:

    /** The attribute name a key stands for, the part after `aria-`. */
    private[kyo] def name(key: Aria): String = key

    val activedescendant: Aria = aria"aria-activedescendant"
    val atomic: Aria           = aria"aria-atomic"
    val busy: Aria             = aria"aria-busy"
    val checked: Aria          = aria"aria-checked"
    val controls: Aria         = aria"aria-controls"
    val current: Aria          = aria"aria-current"
    val describedby: Aria      = aria"aria-describedby"
    val description: Aria      = aria"aria-description"
    val disabled: Aria         = aria"aria-disabled"
    val expanded: Aria         = aria"aria-expanded"
    val haspopup: Aria         = aria"aria-haspopup"
    val hidden: Aria           = aria"aria-hidden"
    val invalid: Aria          = aria"aria-invalid"
    val label: Aria            = aria"aria-label"
    val labelledby: Aria       = aria"aria-labelledby"
    val level: Aria            = aria"aria-level"
    val live: Aria             = aria"aria-live"
    val modal: Aria            = aria"aria-modal"
    val multiselectable: Aria  = aria"aria-multiselectable"
    val orientation: Aria      = aria"aria-orientation"
    val owns: Aria             = aria"aria-owns"
    val pressed: Aria          = aria"aria-pressed"
    val readonly: Aria         = aria"aria-readonly"
    val required: Aria         = aria"aria-required"
    val roledescription: Aria  = aria"aria-roledescription"
    val selected: Aria         = aria"aria-selected"
    val valuemax: Aria         = aria"aria-valuemax"
    val valuemin: Aria         = aria"aria-valuemin"
    val valuenow: Aria         = aria"aria-valuenow"
    val valuetext: Aria        = aria"aria-valuetext"

end Aria
