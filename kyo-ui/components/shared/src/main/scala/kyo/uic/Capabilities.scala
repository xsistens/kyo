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

/** A component whose rendered element carries a chosen DOM `id`.
  *
  * The slot every page needs and only a third of the components had. It began on
  * [[FormControl]], because the form layer stamps each field's minted id there so
  * focus-first-invalid can reach the control — which is why the 31 components that had it
  * were exactly the form controls plus the menus, and why `Card`, `Panel`, `Tag`, `Message`,
  * `DataTable` and the rest of the display family had none. `Node` carries no `id` either,
  * so there was no escape hatch at the base type.
  *
  * Storage was already uniform across every implementor (`copy(idV = Present(v))`), which is
  * the test `Capabilities.scala` applies before lifting anything — and the test `disabled`
  * and `severity` fail. `FileUpload` is the single exception and answers with its
  * `inputId`.
  *
  * ==Which element it lands on==
  *
  * The one a caller would want to address, which is not the same element in every family:
  *
  *   - a **field-shaped control** puts it on the focusable native input, because that is
  *     what `Commands.focusById` and a `Label.forId` pairing have to reach;
  *   - a **container or display component** puts it on its own root.
  *
  * ==Internal parts derive from it==
  *
  * A component with addressable internals treats this id as their BASE and derives
  * `s"$id-<part>"` — `Menu`'s highlighted row is `s"$id-active"`, `Panel`'s collapse button
  * is `s"$id-toggle"`, a `Tabs` header is `s"$id-<tab id>"`. A component that mints a base
  * for itself when none was given (so its ARIA still works unasked) must prefer the
  * caller's: `if idV.isDefined then this else copy(idV = Present(minted))`. Every derived
  * name is documented on the component that derives it — an undocumented derived id is as
  * unreachable as no id at all.
  */
private[uic] trait HasElementId extends Node:
    /** Stores the element id. Implemented as `copy(idV = v)`. */
    private[uic] def withElementId(v: Maybe[String]): Self

    /** Reads it back. Implemented as `idV`.
      *
      * The writer's twin, and it exists because "prefer the caller's id" is a rule this trait
      * already states and only a component could follow: everything else that stamps an id
      * — `form.bind` is the one — could not see whether the caller had already set one, so it
      * overwrote and the caller's id vanished with nothing said. A reader is what lets that be
      * a named error instead.
      */
    private[uic] def elementId: Maybe[String]

    /** Chosen DOM `id`. See [[HasElementId]] for which element it lands on and how a
      * component's internal parts derive from it.
      */
    final def id(v: String): Self = withElementId(Present(v))
end HasElementId

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
