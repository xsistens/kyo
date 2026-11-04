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
  * name). `type Self` fixes the return type, so the setter still hands back the
  * concrete component and autocomplete after `.` stays exact. The accessible name, its
  * reference and the element id live together in one [[Accessibility]] with one writer.
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
private[uic] trait HasAccessibleName extends HasAccessibility:
    /** Accessible name → `aria-label`. A `Signal[String]` patches the attribute IN PLACE via
      * kyo-ui's attribute channel (`setAttribute`), with no re-render of the component.
      */
    final def accessibleName(v: String | Signal[String]): Self =
        withAccessibility(accessibility.copy(nameV = Present(ReactiveValue(v))))
end HasAccessibleName

/** A component whose rendered element carries a chosen DOM `id`.
  *
  * The slot every page needs and only a third of the components had. It began on
  * [[FormControl]], because the form layer stamps each field's minted id there so
  * focus-first-invalid can reach the control, which is why the 31 components that had it
  * were exactly the form controls plus the menus, and why `Card`, `Panel`, `Tag`, `Message`,
  * `DataTable` and the rest of the display family had none. `Node` carries no `id` either,
  * so there was no escape hatch at the base type.
  *
  * Storage was already uniform across every implementor ([[Accessibility.idV]]), which is
  * the test `Capabilities.scala` applies before lifting anything, and the test `disabled`
  * and `severity` fail. `FileUpload` keeps its `inputId` there too, and overrides [[elementId]]
  * to tell its default apart from a caller's choice.
  *
  * ==Which element it lands on==
  *
  * The one a caller would want to address, which is not the same element in every family:
  *
  *   - a **field-shaped control** puts it on the element that takes focus, because that is
  *     what `Commands.focusById`, a `Label.forId` pairing and the form layer's
  *     focus-first-invalid have to reach. That is the native input where there is one; the
  *     root of a combobox that holds its own tab stop (`Select`, `MultiSelect`,
  *     `CascadeSelect`, `TreeSelect`), never the label text inside it; the list of a
  *     `Listbox`; and for a group that takes no focus itself (`SelectButton`, `Rating`) the
  *     option a Tab reaches, which moves with the selection. A component with no focusable
  *     element in some state (an inline `ColorPicker` with nothing bound) falls back to its
  *     root. `ElementIdTest` holds every form control to this;
  *   - a **container or display component** puts it on its own root.
  *
  * ==Internal parts derive from it==
  *
  * A component with addressable internals treats this id as their BASE and derives
  * `s"$id-<part>"`: `Menu`'s highlighted row is `s"$id-active"`, `Panel`'s collapse button
  * is `s"$id-toggle"`, a `Tabs` header is `s"$id-<tab id>"`. A component that mints a base
  * for itself when none was given (so its ARIA still works unasked) must prefer the
  * caller's, as `accessV.idV` holds it. Every derived
  * name is documented on the component that derives it; an undocumented derived id is as
  * unreachable as no id at all.
  */
private[uic] trait HasElementId extends HasAccessibility:
    /** The id the caller chose.
      *
      * It exists because "prefer the caller's id" is a rule this trait already states and only a
      * component could follow: everything else that stamps an id (`form.bind` is the one) could
      * not see whether the caller had already set one, so it overwrote and the caller's id
      * vanished with nothing said. A reader is what lets that be a named error instead.
      */
    private[uic] def elementId: Maybe[String] = accessibility.idV

    /** Chosen DOM `id`. See [[HasElementId]] for which element it lands on and how a
      * component's internal parts derive from it.
      */
    final def id(v: String): Self = withAccessibility(accessibility.copy(idV = Present(v)))
end HasElementId

/** A component that can ALSO be labelled by reference to other elements. Separate from
  * [[HasAccessibleName]] rather than folded into it because only 21 of the 38 name-bearing
  * components carry the slot, and a trait whose members half the implementors cannot answer is
  * the exact problem [[FormControl]] was written to avoid. Extends the name trait because every
  * component offering the reference offers the direct name too.
  */
private[uic] trait HasAccessibleNameRef extends HasAccessibleName:
    /** ID reference(s) of the element(s) that label this component (`aria-labelledby`). Takes
      * precedence over an `aria-label` for assistive tech, so set one or the other.
      */
    final def accessibleNameRef(v: String): Self = withAccessibility(accessibility.copy(nameRefV = Present(v)))
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

/** What a row-bearing component shows in place of rows that are still on their way. See
  * [[LoadingContent]] for why this is a slot of its own beside [[HasEmptyContent]].
  *
  * Unset, the component holds the place of each row that is coming with a skeleton per cell. The
  * `loadingContent` setters lay content over the same place instead: exactly as tall as the rows it
  * stands for (larger content is clipped) and centered. It is there for assistive technology to read,
  * where skeletons are hidden from it; that the rows are changing is said by `aria-busy` on the table
  * while they load, and nothing is announced on its own. [[loadingRow]] replaces the placeholder rows
  * with the caller's own. The four share one slot, so the last one set wins. Setting any of them
  * also turns a table's busy mask off unless it is asked for (`DataTable.loadingMask`), since the
  * mask's spinner would sit on top of it.
  */
private[uic] trait HasLoadingContent extends Node:
    /** Stores the resolved loading slot. Implemented as `copy(loadingContentV = v)`. */
    private[uic] def withLoadingContent(v: Maybe[LoadingContent]): Self

    /** Text shown over the place of the rows that are coming. A `Signal[String]` patches the text in
      * place on emission, so a translated table follows its locale.
      */
    final def loadingContent(v: String | Signal[String]): Self =
        withLoadingContent(Present(LoadingContent.text(v)))

    /** Arbitrary UI over the place of the rows that are coming: a spinner beside a line, an
      * illustration. It is exactly as tall as those rows; for content of its own height, see
      * [[loadingRow]].
      */
    final def loadingContent(ui: UI): Self = withLoadingContent(Present(LoadingContent.ui(ui)))

    /** UI over the place of the rows that are coming, built from how many are coming ("Loading 25
      * rows"). Called again whenever that number changes, which it does when the page size does.
      */
    final def loadingContent(f: Int => UI): Self = withLoadingContent(Present(LoadingContent.counted(f)))

    /** The caller's own row for each row that is coming, given its position among them and how
      * many there are: `(index, count) => UI`. Each lands in a row of its own spanning every column,
      * with nothing of the component's around it, so a placeholder can look like the row it stands
      * for (an avatar and two lines) and be as tall as it likes.
      */
    final def loadingRow(f: (Int, Int) => UI): Self = withLoadingContent(Present(LoadingContent.rows(f)))
end HasLoadingContent

/** What a component holds of its accessible name, its labelling reference and its element id, as its setters stored them. The one
  * field `accessV` of a component's state, under one name whichever family the component is from.
  */
final private[uic] case class Accessibility(
    nameV: Maybe[TextValue] = Absent,
    nameRefV: Maybe[String] = Absent,
    idV: Maybe[String] = Absent
)

private[uic] object Accessibility:
    /** A component with nothing set, shared so that one nobody names stores nothing of its own for it. */
    val none: Accessibility = Accessibility()

/** A component whose state holds an [[Accessibility]]: what [[HasAccessibleName]], [[HasAccessibleNameRef]] and
  * [[HasElementId]] read and write, each in its own setter.
  */
private[uic] trait HasAccessibility extends Node:
    /** The group as it is. Implemented as `accessV`. */
    private[uic] def accessibility: Accessibility

    /** Stores the group. Implemented as `new Input(s.copy(accessV = v))`. */
    private[uic] def withAccessibility(v: Accessibility): Self
end HasAccessibility
