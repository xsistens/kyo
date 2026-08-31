package kyo.uic

import kyo.*
import kyo.UI.*

/** The slots that recur across unrelated components, defined once.
  *
  * These are not a family the way [[FormControl]] is: an accessible name has nothing to do
  * with a tooltip, and a component claims each independently. What they share is a shape.
  * The slot is one of the module's const-or-reactive carriers, the setter is the single
  * `A | Signal[A]` union, and the wording of what the reactive case DOES is a property of
  * the slot rather than of the component holding it. Written per component, that wording
  * drifted: `accessibleName` carried eleven near-identical paragraphs and four spellings of
  * the same first sentence.
  *
  * A component opts in by adding the trait to its `extends` clause and supplying the
  * one-line WRITER, which is the only part that is genuinely per component (the field
  * names differ: `accNameV` in the field-shaped controls, `accessibleNameV` in the
  * containers). `type Self` fixes the return type, so the setter still hands back the
  * concrete component and autocomplete after `.` stays exact.
  *
  * Two slots that LOOK like they belong here and do not:
  *
  *   - `disabled`, because it is not one slot. Fourteen components store
  *     `Maybe[BoolValue]` and honour a reactive value; nineteen store a plain `Boolean`
  *     and have no reactive path in their render at all. A trait would force the second
  *     group to grow one, which is a behaviour change per component, not a lift.
  *   - `severity`, because its storage disagrees on whether the slot is optional:
  *     `Maybe[SeverityValue]` in Badge/Tag, a defaulted `SeverityValue` in
  *     Button/Message/SplitButton, `Maybe[Severity]` in Dialog. Unifying them would
  *     change what an unset severity means, which is a design-system question, not a
  *     refactor.
  */
private[uic] trait HasAccessibleName extends Node:
    /** Stores the resolved accessible-name slot. Implemented as `copy(accNameV = v)`. */
    private[uic] def withAccessibleName(v: Maybe[TextValue]): Self

    /** Accessible name → `aria-label`. A `Signal[String]` patches the attribute IN PLACE via
      * kyo-ui's attribute channel (`setAttribute`), with no re-render of the component.
      */
    final def accessibleName(v: String | Signal[String]): Self =
        withAccessibleName(Present(ReactiveValue(v)))
end HasAccessibleName

/** A component that can ALSO be labelled by reference to other elements. Separate from
  * [[HasAccessibleName]] rather than folded into it because only 21 of the 38 name-bearing
  * components carry the slot, and a trait whose members half the implementors cannot answer is
  * the exact problem [[FormControl]] was written to avoid. Extends the name trait because every
  * component offering the reference offers the direct name too.
  */
private[uic] trait HasAccessibleNameRef extends HasAccessibleName:
    /** Stores the `aria-labelledby` reference. Implemented as `copy(accNameRefV = v)`. */
    private[uic] def withAccessibleNameRef(v: Maybe[String]): Self

    /** ID reference(s) of the element(s) that label this component (`aria-labelledby`). Takes
      * precedence over an `aria-label` for assistive tech, so set one or the other.
      */
    final def accessibleNameRef(v: String): Self = withAccessibleNameRef(Present(v))
end HasAccessibleNameRef

/** A component carrying an `aria-description`, the longer companion to an accessible name.
  * Separate from [[HasAccessibleName]] because only four components have it and a name does not
  * imply a description. See [[HasAccessibleName]] for why these traits exist.
  */
private[uic] trait HasAccessibleDescription extends Node:
    /** Stores the resolved description slot. Implemented as `copy(accDescV = v)`. */
    private[uic] def withAccessibleDescription(v: Maybe[TextValue]): Self

    /** Additional accessible description → `aria-description`. A `Signal[String]` patches the
      * attribute IN PLACE via kyo-ui's attribute channel (`setAttribute`, no re-render).
      */
    final def accessibleDescription(v: String | Signal[String]): Self =
        withAccessibleDescription(Present(ReactiveValue(v)))
end HasAccessibleDescription

/** The placeholder slot of a field-shaped control: the text shown while the bound value is
  * empty. See [[HasAccessibleName]] for why these traits exist.
  */
private[uic] trait HasPlaceholder extends Node:
    /** Stores the resolved placeholder slot. Implemented as `copy(placeholderText = v)`. */
    private[uic] def withPlaceholder(v: Maybe[TextValue]): Self

    /** Text shown while the bound value is empty. A `Signal[String]` tracks a downstream source
      * (a locale-driven `I18n.t` leaf); how it lands depends on where the placeholder lives.
      * On a native input it is patched IN PLACE via kyo-ui's attribute channel, with no
      * re-render and no lost caret. On a picker's closed trigger the label is text rather than
      * an attribute, so that slot re-renders inside its own mount subscription instead.
      */
    final def placeholder(v: String | Signal[String]): Self =
        withPlaceholder(Present(ReactiveValue(v)))
end HasPlaceholder

/** The native tooltip slot (the `title` attribute). See [[HasAccessibleName]] for why these
  * traits exist.
  */
private[uic] trait HasTooltip extends Node:
    /** Stores the resolved tooltip slot. Implemented as `copy(tooltipV = v)`. */
    private[uic] def withTooltip(v: Maybe[TextValue]): Self

    /** Native tooltip (the `title` attribute). A `Signal[String]` patches `title` IN PLACE via
      * kyo-ui's attribute channel (`setAttribute`, no re-render).
      */
    final def tooltip(v: String | Signal[String]): Self = withTooltip(Present(ReactiveValue(v)))
end HasTooltip

/** What a row-bearing component shows in place of its rows when it has none. See
  * [[HasAccessibleName]] for why these traits exist, and [[EmptyContent]] for why the slot
  * carries arbitrary UI and not only text.
  */
private[uic] trait HasEmptyContent extends Node:
    /** Stores the resolved empty slot. Implemented as `copy(emptyContentV = v)`. */
    private[uic] def withEmptyContent(v: Maybe[EmptyContent]): Self

    /** Text shown in place of the rows when there are none. A `Signal[String]` re-renders the
      * empty slot in place on emission.
      */
    final def emptyContent(v: String | Signal[String]): Self =
        withEmptyContent(Present(EmptyContent.text(v)))

    /** Arbitrary UI for the empty state: an icon over a line of explanation and the button that
      * creates the first record, rendered in the same slot the text would occupy.
      */
    final def emptyContent(ui: UI): Self = withEmptyContent(Present(EmptyContent.ui(ui)))
end HasEmptyContent
