package kyo.uic

import kyo.*
import kyo.UI.*
import kyo.UI.Ast.HtmlChildVal

/** What a component shows in place of rows that are still on their way.
  *
  * The sibling of [[EmptyContent]], and kept apart from it on purpose: "nothing matched" and
  * "nothing has arrived yet" are different statements, and a component that has only the first
  * makes it while the second is true. Unset, a component holds the place of the rows with
  * skeletons. The other cases come in two kinds:
  *
  *   - Over the place: constant text, text tracking a `Signal` (a locale-driven leaf), arbitrary UI,
  *     or UI built from how many rows are coming. The component still draws one invisible line per
  *     row that is coming, so the height is the rows' own in every theme, and lays the content over
  *     all of them, exactly that tall.
  *   - In place of each row: the caller's own row content, given its position and how many rows
  *     are coming. The component draws one full-width row per row that is coming and puts nothing
  *     else in it, so the look and the height are the caller's.
  */
private[uic] enum LoadingContent:
    case Text(value: TextValue)
    case Ui(value: UI)
    case Counted(f: Int => UI)
    case Rows(f: (Int, Int) => UI)
end LoadingContent

private[uic] object LoadingContent:

    /** The text case from the union a host's `loadingContent` setter takes: a constant builds once,
      * a `Signal[String]` patches the text in place on emission.
      */
    def text(v: String | Signal[String]): LoadingContent = Text(ReactiveValue(v))

    def ui(v: UI): LoadingContent = Ui(v)

    def counted(f: Int => UI): LoadingContent = Counted(f)

    def rows(f: (Int, Int) => UI): LoadingContent = Rows(f)

    /** The content laid over `count` rows inside `wrap`, or `Absent` for content drawn in place of
      * each row. A `Dyn` text goes in as the wrapper's text CHILD, so kyo-ui's text channel patches
      * it in place rather than a region rebuilding the wrapper.
      */
    def over(c: LoadingContent, count: Int)(wrap: HtmlChildVal => UI)(using Frame): Maybe[UI] =
        c match
            case Text(TextValue.Const(t)) => Present(wrap(toChild(stringToUI(t))))
            case Text(TextValue.Dyn(s))   => Present(wrap(toChild(signalStringToUI(s))))
            case Ui(u)                    => Present(wrap(toChild(u)))
            case Counted(f)               => Present(wrap(toChild(f(count))))
            case Rows(_)                  => Absent
end LoadingContent
