package kyo

/** The key of an `aria-*` attribute: the part after `aria-`, as the entries of `UI.Ast.Element.aria` name it.
  *
  * A key is built with the `aria` interpolator, which checks it at compile time: `aria"aria-label"` is the key `label`. The literal
  * includes the `aria-` prefix. Each static part must hold only lowercase letters, digits and hyphens, and the name must start with a
  * letter. A dynamic part (`aria"aria-item-$n"`) is not known until run time, so the whole key is checked again when it is built, with
  * the same rules, and an invalid one throws. A plain string is not a key: a name held in a `String` is written as `aria"aria-$name"`.
  *
  * The type is a `String` underneath, so a key costs nothing beyond the string. The companion holds the standard keys the library uses,
  * so a call can name one with the compiler's help: `AriaKey.disabled -> true`.
  */
opaque type AriaKey = String

object AriaKey:

    /** The attribute name a key stands for, the part after `aria-`. */
    private[kyo] def name(key: AriaKey): String = key

    val activedescendant: AriaKey = aria"aria-activedescendant"
    val atomic: AriaKey           = aria"aria-atomic"
    val busy: AriaKey             = aria"aria-busy"
    val checked: AriaKey          = aria"aria-checked"
    val controls: AriaKey         = aria"aria-controls"
    val current: AriaKey          = aria"aria-current"
    val describedby: AriaKey      = aria"aria-describedby"
    val description: AriaKey      = aria"aria-description"
    val disabled: AriaKey         = aria"aria-disabled"
    val expanded: AriaKey         = aria"aria-expanded"
    val haspopup: AriaKey         = aria"aria-haspopup"
    val hidden: AriaKey           = aria"aria-hidden"
    val invalid: AriaKey          = aria"aria-invalid"
    val label: AriaKey            = aria"aria-label"
    val labelledby: AriaKey       = aria"aria-labelledby"
    val level: AriaKey            = aria"aria-level"
    val live: AriaKey             = aria"aria-live"
    val modal: AriaKey            = aria"aria-modal"
    val multiselectable: AriaKey  = aria"aria-multiselectable"
    val orientation: AriaKey      = aria"aria-orientation"
    val owns: AriaKey             = aria"aria-owns"
    val pressed: AriaKey          = aria"aria-pressed"
    val readonly: AriaKey         = aria"aria-readonly"
    val required: AriaKey         = aria"aria-required"
    val roledescription: AriaKey  = aria"aria-roledescription"
    val selected: AriaKey         = aria"aria-selected"
    val valuemax: AriaKey         = aria"aria-valuemax"
    val valuemin: AriaKey         = aria"aria-valuemin"
    val valuenow: AriaKey         = aria"aria-valuenow"
    val valuetext: AriaKey        = aria"aria-valuetext"

end AriaKey
