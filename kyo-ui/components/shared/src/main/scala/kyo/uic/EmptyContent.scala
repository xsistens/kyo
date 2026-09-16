package kyo.uic

import kyo.*
import kyo.UI.*
import kyo.UI.Ast.HtmlChildVal

/** What a component shows in place of its rows when it has none: constant text, text
  * tracking a `Signal` (a locale-driven leaf), or arbitrary UI.
  *
  * The UI case is what a plain [[TextValue]] cannot carry. An empty state is rarely a
  * sentence: it is usually an icon over a line of explanation and a button that creates
  * the first record, and a component that only accepts a `String` pushes that layout
  * outside the component, where it no longer sits inside the list, table body or panel
  * the emptiness belongs to.
  */
private[uic] enum EmptyContent:
    case Text(value: TextValue)
    case Ui(value: UI)

private[uic] object EmptyContent:

    /** The text case from the union a host's `emptyContent` setter takes: a constant builds once, a
      * `Signal[String]` re-renders the slot in place on emission.
      */
    def text(v: String | Signal[String]): EmptyContent = Text(ReactiveValue(v))

    def ui(v: UI): EmptyContent = Ui(v)

    /** Renders the slot inside `wrap`, falling back to `fallback` text when unset. For
      * the components that always show something (the tables, DataView, the pickers'
      * panels).
      */
    def render(c: Maybe[EmptyContent], fallback: String)(wrap: HtmlChildVal => UI)(using Frame): UI =
        one(c.getOrElse(text(fallback)))(wrap)

    /** Renders the slot only when it is set, for the components that leave the row out
      * entirely otherwise (Prime renders no empty row for an unset slot there).
      */
    def whenSet(c: Maybe[EmptyContent])(wrap: HtmlChildVal => UI)(using Frame): List[UI] =
        c.toList.map(v => one(v)(wrap))

    /** A `Dyn` slot goes in as the wrapper's text CHILD — kyo-ui's text channel patches it in place,
      * where a region around `wrap` would rebuild the whole empty-state element per emission. The
      * other two build once.
      */
    private def one(c: EmptyContent)(wrap: HtmlChildVal => UI)(using Frame): UI =
        c match
            case Text(TextValue.Const(t)) => wrap(toChild(stringToUI(t)))
            case Text(TextValue.Dyn(s))   => wrap(toChild(signalStringToUI(s)))
            case Ui(u)                    => wrap(toChild(u))
end EmptyContent
