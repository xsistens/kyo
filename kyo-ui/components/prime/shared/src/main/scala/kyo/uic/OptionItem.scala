package kyo.uic

import kyo.*
import kyo.UI.*

/** One row of a grouped option list: either a pickable option standing on its
  * own, or a labelled group of them. The flat sibling of [[CascadeItem]], which
  * nests without a depth limit; Prime's option-group model is exactly one level
  * deep, so a group carries plain values rather than further items.
  *
  * Rows render in declaration order and are never reordered or merged: two
  * groups sharing a label stay two groups, because that is what the sequence
  * said. This follows the rule [[DataView]] documents for sorting, where the
  * caller owns the order and the component renders it.
  */
enum OptionItem[A] derives CanEqual:
    case Item(value: A)
    case Group(label: String, items: List[A])

/** Where some of a picker's options come from: rows given as they are, or a signal of values or of rows, for options that
  * change while the picker is shown (the results of a search, the choices another field leaves). A picker keeps its
  * sources in the order its setters received them and reads them as ONE reading in its own region
  * ([[OptionItem.reading]]), so the picker is never built again around a list that moves.
  */
private[uic] enum OptionSource[A]:
    case Fixed(rows: List[OptionItem[A]])
    case Values(values: Signal[Seq[A]])
    case Rows(rows: Signal[Seq[OptionItem[A]]])
end OptionSource

private[uic] object OptionSource:
    /** The source a setter's `values` stand for: fixed while they are a collection, live while they are a signal. */
    def of[A](values: Iterable[A] | Signal[Seq[A]]): OptionSource[A] =
        values match
            case sig: Signal[?] => Values(sig.asInstanceOf[Signal[Seq[A]]])
            case is             => Fixed(is.asInstanceOf[Iterable[A]].iterator.map(OptionItem.Item(_)).toList)

    /** The same for grouped rows. */
    def rows[A](rows: Iterable[OptionItem[A]] | Signal[Seq[OptionItem[A]]]): OptionSource[A] =
        rows match
            case sig: Signal[?] => Rows(sig.asInstanceOf[Signal[Seq[OptionItem[A]]]])
            case is             => Fixed(is.asInstanceOf[Iterable[OptionItem[A]]].toList)
end OptionSource

object OptionItem:

    /** A picker's sources as ONE reading of its rows, in order: fixed while none of them is a signal, so a picker of plain
      * options draws no region for them.
      */
    private[uic] def reading[A](sources: List[OptionSource[A]])(using Frame): ReactiveValue[List[OptionItem[A]]] =
        given CanEqual[Seq[OptionItem[A]], Seq[OptionItem[A]]] = CanEqual.derived
        ReactiveValue.concat(sources.map {
            case OptionSource.Values(sig) => sig.map(_.map(OptionItem.Item(_)))
            case OptionSource.Rows(sig)   => sig
            case OptionSource.Fixed(rows) => rows
        })
    end reading

    /** A single ungrouped option, rendered as a plain row beside the groups. */
    def item[A](value: A): OptionItem[A] = OptionItem.Item(value)

    /** A group labelled `label` holding `items` in a nested list. */
    def group[A](label: String)(items: A*): OptionItem[A] = OptionItem.Group(label, items.toList)

    /** The pickable values in render order, which is the order the keyboard
      * highlight, the selection lookup and the key-collision check all address.
      * Group labels carry no value and drop out here.
      */
    private[uic] def flatten[A](is: List[OptionItem[A]]): List[A] =
        is.flatMap:
            case OptionItem.Item(a)      => List(a)
            case OptionItem.Group(_, as) => as

    /** Keeps the values `keep` accepts, then drops any group the filter emptied.
      * Without the second step a narrowing filter leaves a panel of bare headers
      * standing over no options.
      */
    private[uic] def filter[A](is: List[OptionItem[A]], keep: A => Boolean): List[OptionItem[A]] =
        is.flatMap:
            case i @ OptionItem.Item(a)      => if keep(a) then List(i) else Nil
            case OptionItem.Group(label, as) =>
                val kept = as.filter(keep)
                if kept.isEmpty then Nil else List(OptionItem.Group(label, kept))

    /** A group as a real `role=group` holding its options, so the grouping
      * reaches the accessibility tree rather than only the stylesheet.
      *
      * Prime renders the header as a flat `li` sibling of the options, which
      * styles correctly but tells a screen reader nothing: the options are the
      * header's siblings, not its children, so no relation survives. Here the
      * group is a `li[role=group]` whose nested `ul[role=none]` holds header and
      * options together, which is the listbox pattern ARIA specifies. The
      * `role=none` list drops out of the tree, leaving each option a descendant
      * of its group.
      *
      * The nested list would otherwise break Prime's layout, since every option
      * list is a flex column whose gap the options depend on. `.p-uic-option-group`
      * and `.p-uic-option-group-list` restate that column and inherit the gap, so
      * a grouped panel measures exactly like a flat one. The header keeps Prime's
      * own `.p-<component>-option-group` class on the `li` element its rule was
      * written for, and is hidden from the tree because the group already carries
      * the same text as its accessible name.
      */
    private def groupRow(label: String, headerClass: String, options: List[UI])(using Frame): UI =
        val header: UI = li.cssClass(headerClass).role("presentation").aria(Aria.hidden -> "true")(label)
        li.cssClass("p-uic-option-group").role("group").aria(Aria.label -> label)(
            ul.cssClass("p-uic-option-group-list").role("none")(UI.children(header :: options)*)
        )
    end groupRow

    /** The panel's rows in declaration order, each option carrying its position
      * in [[flatten]].
      *
      * That position is what the keyboard highlight, the Enter pick and the
      * disabled-row skip all address, and it has to stay a single flat sequence:
      * a header is rendered but not reachable, so counting rendered rows would
      * drift from counting pickable ones. Each group's options occupy a
      * contiguous run, so one running offset gives every row its index without a
      * second pass over the list.
      */
    private[uic] def rows[A](is: List[OptionItem[A]], headerClass: String)(option: (A, Int) => UI)(using Frame): List[UI] =
        val offsets = is.scanLeft(0)((n, it) =>
            n +
                (it match
                    case OptionItem.Item(_)      => 1
                    case OptionItem.Group(_, as) => as.size)
        )
        is.zip(offsets).map:
            case (OptionItem.Item(a), base)          => option(a, base)
            case (OptionItem.Group(label, as), base) =>
                groupRow(label, headerClass, as.zipWithIndex.map((a, j) => option(a, base + j)))
    end rows
end OptionItem
