package kyo.uic

import kyo.*
import kyo.UI.Ast.HtmlChildVal
import scala.language.implicitConversions

/** Base type of every uic component.
  *
  * A component is a plain immutable builder over its state ([[HasProps]]), NOT a `kyo.UI`. The kyo HTML AST
  * (`UI` / `Element` / `HtmlContent`) is `sealed` and cannot be extended from
  * outside `kyo/UI.scala`, so a component instead renders itself into a `UI` on
  * demand and is auto-lifted at placement, mirroring how kyo lifts a bare
  * `String` into a `Text` node (`kyo.UI.stringToUI`).
  *
  * The chained setters each return the concrete `Self`, so IDE autocomplete after
  * `.` shows exactly the options valid for that component and nothing else.
  *
  * ==Who may own a fiber==
  *
  * This is the module's single rule on timers; components and services state their
  * position against it rather than restating it.
  *
  *   - `render` is pure. Projecting a component into a `UI` schedules nothing, so a
  *     server render, an SSG pass and a golden test never start a clock.
  *   - A component MAY own a fiber spawned with `Fiber.init` INSIDE its own
  *     `UI.mounted`. The mount's `Scope` is that component's lifetime, so mounted
  *     supervision cancels the fiber on unmount and it cannot leak. `Carousel`'s
  *     autoplay is the one component that does this today.
  *   - A timer that must outlive any single render does NOT belong to a component.
  *     It belongs to a service, which forks it with `Fiber.initUnscoped` and owns
  *     the cancellation itself; `ToastService`'s per-message auto-dismiss is the
  *     reference case.
  */
trait Node:
    type Self <: Node

    /** Render this component into the kyo `UI` AST. Internal: downstream code never
      * calls this; the placement conversions in [[Node]]'s companion do.
      */
    private[uic] def render(using Frame): UI

    /** This component as a plain `kyo.UI` value.
      *
      * Normal PLACEMENT needs nothing: a container's child list, a `UI`-typed
      * setter, `fragment(...)`, `when(...)` and a signal-render body all supply an
      * expected type, and the conversions in [[Node]]'s companion fire against it.
      *
      * A comprehension bound to a `val` FIRST does not. Written inline as an
      * argument the expected type still reaches the comprehension's element type,
      * but
      * {{{
      * val rows = for a <- xs yield uic.Button(a)   // List[Button], not List[UI]
      * fragment(rows*)                              // does not compile
      * }}}
      * has nothing constraining the element type at the definition, so the list
      * fixes to `List[Button]` and the placement one line later fails, with an
      * error that points at the collection rather than at the missing conversion.
      * No additional `Conversion` can rescue that: there is no expected type to
      * trigger one. Project explicitly instead, `yield uic.Button(a).toUI`.
      */
    final def toUI(using Frame): UI = render
end Node

object Node:

    /** Single-hop lift so a component sits directly in a kyo container's
      * `HtmlChildVal*` argument list, e.g. `div(uic.Button("Save"))`.
      *
      * A `Node => UI => HtmlChildVal` chain would need two implicit hops, which Scala
      * does not perform, so this direct conversion is required.
      */
    implicit def nodeToChild(c: Node)(using Frame): HtmlChildVal = HtmlChildVal.lift[UI](c.render)

    /** Lift into a bare `UI` for `fragment(...)`, `when(...)`, and signal-render
      * contexts that expect `UI` rather than `HtmlChildVal`.
      */
    implicit def nodeToUI(c: Node)(using Frame): UI = c.render
end Node

/** A builder that shows its setters and nothing else, and is compared by what it holds.
  *
  * A component is a `final class` whose one constructor parameter `s` is a `private[uic]` case class in its companion, so
  * outside this package completion offers the setters and no field, `copy`, `_1` or `product*`. What a case class gave for
  * free and something relies on comes back here: two builders of one class holding equal state are equal. `Anchored` finds a
  * menu item again by that, when a signal emits the list anew.
  *
  * A wrapper over another component ([[MultipleAccordion]], [[MultiSelectButton]], [[MultiDatePicker]], [[RangeDatePicker]])
  * holds that component as its state, which is why classes are compared as well.
  */
private[uic] trait HasProps:
    private[uic] def p: Any

    override def equals(other: Any): Boolean = other match
        case that: HasProps => getClass.equals(that.getClass) && p.equals(that.p)
        case _              => false

    override def hashCode: Int = p.hashCode

    override def toString: String = s"${getClass.getSimpleName}($p)"
end HasProps
