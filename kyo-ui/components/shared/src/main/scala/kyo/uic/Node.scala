package kyo.uic

import kyo.*
import kyo.UI.Ast.HtmlChildVal
import scala.language.implicitConversions

/** Base type of every uic component.
  *
  * A component is a plain immutable builder, NOT a `kyo.UI`. The kyo HTML AST
  * (`UI` / `Element` / `HtmlContent`) is `sealed` and cannot be extended from
  * outside `kyo/UI.scala`, so a component instead renders itself into a `UI` on
  * demand and is auto-lifted at placement — mirroring how kyo lifts a bare
  * `String` into a `Text` node (`kyo.UI.stringToUI`).
  *
  * The chained setters each return the concrete `Self`, so IDE autocomplete after
  * `.` shows exactly the options valid for that component and nothing else.
  */
trait Node:
    type Self <: Node

    /** Render this component into the kyo `UI` AST. Internal: downstream code never
      * calls this — the placement conversions in [[Node]]'s companion do.
      */
    private[uic] def render(using Frame): UI
end Node

object Node:

    /** Single-hop lift so a component sits directly in a kyo container's
      * `HtmlChildVal*` argument list, e.g. `div(uic.Button("Save"))`.
      *
      * A `Node => UI => HtmlChildVal` chain would need two implicit hops, which Scala
      * does not perform, so this direct conversion is required. It uses the
      * `private[kyo]` `HtmlChildVal` constructor, reachable because `kyo.uic` is a
      * subpackage of `kyo`.
      */
    implicit def nodeToChild(c: Node)(using Frame): HtmlChildVal = new HtmlChildVal(c.render)

    /** Lift into a bare `UI` for `fragment(...)`, `when(...)`, and signal-render
      * contexts that expect `UI` rather than `HtmlChildVal`.
      */
    implicit def nodeToUI(c: Node)(using Frame): UI = c.render
end Node
