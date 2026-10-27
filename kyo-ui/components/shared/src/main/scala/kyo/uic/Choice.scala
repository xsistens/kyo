package kyo.uic

import kyo.*
import kyo.UI.*

/** What a single-choice field ([[Select]], [[CascadeSelect]], [[SelectButton]]) holds its choice in: the option itself
  * (`selection` over `A`), or the option or none (`selection` over `Maybe[A]`), each as the module's const-or-reactive
  * [[ReactiveValue]].
  *
  * A writable `SignalRef` binds TWO-WAY: a pick writes the option. Any other `Signal`, or a constant, binds ONE-WAY: the
  * field shows the value and a pick writes nothing, it only reports the change, so the app writes its own state (a route,
  * say) and the field follows it. The case is decided on the runtime class ([[ReactiveVariable]] before `Dyn`), so
  * `ref.readOnly` is how a ref opts out.
  *
  * The fields compare options by their key, because the key is what the panel, the keyboard and the hidden form input work
  * with, and never by the option type's own equality: an app value that is a copy of an option still picks that option's row.
  * A write that names the key already held changes nothing, so the instance the app wrote survives a pick of the row already
  * chosen, and a binding over `A` cannot hold none, so a step to none changes nothing either. Both cases report no change,
  * which is what keeps `onChange` and `onPick` from announcing a change that did not happen. A one-way binding answers the
  * same question against the value it shows.
  */
private[uic] enum SingleChoice[A]:
    case Value(v: ReactiveValue[A])
    case Optional(v: ReactiveValue[Maybe[A]])

    /** The held option as a [[Reading]], `Absent` for none, for a host that reads it beside its other state in one region. */
    def reading(using CanEqual[A, A], Frame): Reading[Maybe[A]] =
        this match
            case Value(ReactiveValue.Dyn(sig)) => Reading.Live(sig.map(a => Present(a)))
            case Value(ReactiveValue.Const(a)) => Reading.Fixed(Present(a))
            case Optional(v)                   => Reading.of(v)

    /** The held option as it is at this moment. */
    def current(using Frame): Maybe[A] < Sync =
        this match
            case Value(ReactiveValue.Dyn(sig))    => sig.current.map(a => Present(a))
            case Value(ReactiveValue.Const(a))    => Present(a)
            case Optional(ReactiveValue.Dyn(sig)) => sig.current
            case Optional(ReactiveValue.Const(m)) => m

    /** Runs `f` with the held option as it is at this moment. */
    def read(f: Maybe[A] => UI.Action)(using Frame): UI.Action =
        current.map(f)

    /** Writes `next` unless it names the key already held (or is none over a binding that cannot hold it), and answers the
      * value now held where that changed it. A one-way binding writes nothing and answers the same.
      */
    def write(next: Maybe[A], key: A => String)(using Frame): Maybe[Maybe[A]] < Sync =
        update(key)(_ => next)

    /** [[write]] of `f` applied to the held option, as one atomic step where the binding is writable. */
    def update(key: A => String)(f: Maybe[A] => Maybe[A])(using Frame): Maybe[Maybe[A]] < Sync =
        def same(a: Maybe[A], b: Maybe[A]): Boolean = a.map(key) == b.map(key)
        // Over `A` the binding takes only an option, so a step to none leaves it.
        def stepValue(prev: A): Maybe[A] =
            f(Present(prev)) match
                case n @ Present(a) if key(a) != key(prev) => n
                case _                                     => Absent
        def stepOptional(prev: Maybe[A]): Maybe[Maybe[A]] =
            val now = f(prev)
            if same(prev, now) then Absent else Present(now)
        this match
            case Value(ReactiveVariable(r)) =>
                r.getAndUpdate(prev => stepValue(prev).getOrElse(prev)).map(prev => stepValue(prev).map(a => Present(a)))
            case Optional(ReactiveVariable(r)) =>
                r.getAndUpdate(prev => stepOptional(prev).getOrElse(prev)).map(stepOptional)
            case Value(_) =>
                current.map(_.flatMap(stepValue).map(a => Present(a)))
            case Optional(_) =>
                current.map(stepOptional)
        end match
    end update

    /** Whether the binding can hold none, which is what a clear writes (or, one-way, reports). */
    def clearable: Boolean =
        this match
            case Value(_) => false
            case _        => true
end SingleChoice

private[uic] object SingleChoice:

    /** Fires what a single-choice field reports after a write that changed what it holds (`changed` is `Present` exactly
      * then): `onChange` with what it now holds, then `onPick` with the option where it holds one. Nothing fires for no
      * change, and `onPick` never fires for a clear. One place, so the order is the same in every field.
      */
    def report[A](
        changed: Maybe[Maybe[A]],
        onChange: Maybe[Maybe[A] => UI.Action],
        onPick: Maybe[A => UI.Action]
    )(using Frame): UI.Action =
        changed match
            case Present(now) =>
                val change: UI.Action = onChange match
                    case Present(g) => g(now)
                    case Absent     => ()
                val pick: UI.Action = (now, onPick) match
                    case (Present(a), Present(g)) => g(a)
                    case _                        => ()
                change.andThen(pick)
            case Absent => ()
end SingleChoice

/** What a multi-choice field ([[MultiSelect]], [[MultiSelectButton]]) holds its choice in: the set of chosen
  * options, as the module's const-or-reactive [[ReactiveValue]].
  *
  * The [[SingleChoice]] rules at a set: the field works on keys, and a write puts back the options that carry the keys it
  * names. A member the ref already holds stays as the app wrote it while its key stays chosen. A `SignalRef` binds two
  * way; any other signal, or a constant, binds one way: a pick writes nothing and only reports the set it asks for.
  */
final private[uic] case class MultiChoice[A](v: ReactiveValue[Set[A]]):

    /** The held options as a [[Reading]], for a host that reads them beside its other state in one region. */
    def reading(using Frame): Reading[Set[A]] = Reading.of(v)

    /** Renders `f` with the held options. */
    def render(f: Set[A] => UI)(using Frame): UI = reading.render(f)

    /** The held options as they are at this moment. */
    def current(using Frame): Set[A] < Sync =
        v match
            case ReactiveValue.Dyn(sig) => sig.current
            case ReactiveValue.Const(a) => a

    /** Runs `f` with the held options as they are at this moment. */
    def read(f: Set[A] => UI.Action)(using Frame): UI.Action = current.map(f)

    /** Whether a pick reaches anybody: a two-way binding, or `report`. */
    def heard[R](report: Maybe[R]): Boolean = ReactiveValue.heard(Present(v), report)

    /** Replaces the chosen keys `ks` with `f(ks)`, written back as the options among `options` that carry them where the
      * binding is two-way, and answers the set now held (or, one-way, asked for) where the keys changed.
      */
    def update(options: => Seq[A], key: A => String)(f: Set[String] => Set[String])(using Frame): Maybe[Set[A]] < Sync =
        def next(prev: Set[A]): Set[A] =
            val now   = f(prev.map(key))
            val kept  = prev.filter(a => now.contains(key(a)))
            val known = kept.map(key)
            kept ++ options.filter(a => now.contains(key(a)) && !known.contains(key(a)))
        end next
        def changed(prev: Set[A]): Maybe[Set[A]] =
            val now = next(prev)
            if now.map(key) == prev.map(key) then Absent else Present(now)
        v.writable match
            case Present(ref) => ref.getAndUpdate(next).map(changed)
            case Absent       => current.map(changed)
        end match
    end update
end MultiChoice

/** The selection a set-valued host ([[Listbox]], and through it [[OrderList]] and [[PickList]]) reads and writes, whatever
  * holds it: the app's own `SignalRef[Set[K]]`, or a typed `Set[A]` an embedding host keys by its own projection.
  *
  * A host that embeds a [[Listbox]] keyed by strings passes its typed binding through [[SetBinding.keyed]], so the list's
  * clicks and keys write the options the keys name and the host's API stays typed.
  */
private[uic] trait SetBinding[K]:
    def render(f: Set[K] => UI)(using Frame): UI

    /** The held set as a signal, for a host that reads it beside its other state in one region. */
    def signal(using Frame): Signal[Set[K]]
    def read(f: Set[K] => UI.Action)(using Frame): UI.Action

    /** Steps the held set, written where the binding is two-way, and answers the set now held (or, one-way, asked for)
      * where the step changed it.
      */
    def update(f: Set[K] => Set[K])(using Frame): Maybe[Set[K]] < (Async & Abort[Any])

    /** Whether a step reaches anybody: a two-way binding, or `report`. */
    def heard[R](report: Maybe[R]): Boolean
end SetBinding

private[uic] object SetBinding:

    /** The app's own set: a ref is written as it is, a signal or a constant only shown. */
    def of[K](v: ReactiveValue[Set[K]]): SetBinding[K] =
        // Ids are compared by their own `equals`, as everywhere a model carries its id (see `ListItem`).
        given CanEqual[Set[K], Set[K]] = CanEqual.derived
        new SetBinding[K]:
            def render(f: Set[K] => UI)(using Frame): UI             = Reading.of(v).render(f)
            def signal(using Frame): Signal[Set[K]]                  = Reading.of(v).signal
            def read(f: Set[K] => UI.Action)(using Frame): UI.Action = current.map(f)
            def heard[R](report: Maybe[R]): Boolean                  = ReactiveValue.heard(Present(v), report)
            private def current(using Frame): Set[K] < Sync          =
                v match
                    case ReactiveValue.Dyn(sig) => sig.current
                    case ReactiveValue.Const(a) => a
            def update(f: Set[K] => Set[K])(using Frame): Maybe[Set[K]] < (Async & Abort[Any]) =
                def changed(prev: Set[K]): Maybe[Set[K]] =
                    val now = f(prev)
                    if now == prev then Absent else Present(now)
                v.writable match
                    case Present(ref) => ref.getAndUpdate(f).map(changed)
                    case Absent       => current.map(changed)
            end update
        end new
    end of

    /** A typed set of options read and written through their keys, as [[MultiChoice]] does. An embedding host that
      * reports its typed selection hands `report` over: it hears the options the step leaves chosen, typed, before the
      * embedded list's own events.
      */
    def keyed[A](
        choice: MultiChoice[A],
        options: => Seq[A],
        key: A => String,
        report: Maybe[Set[A] => UI.Action] = Absent
    ): SetBinding[String] =
        new SetBinding[String]:
            def render(f: Set[String] => UI)(using Frame): UI             = choice.render(as => f(as.map(key)))
            def signal(using Frame): Signal[Set[String]]                  = choice.reading.map(_.map(key)).signal
            def read(f: Set[String] => UI.Action)(using Frame): UI.Action = choice.read(as => f(as.map(key)))
            def update(f: Set[String] => Set[String])(using Frame): Maybe[Set[String]] < (Async & Abort[Any]) =
                choice.update(options, key)(f).map {
                    case Present(now) => report.fold[UI.Action](())(_(now)).andThen(Present(now.map(key)))
                    case Absent       => Absent
                }
            def heard[R](other: Maybe[R]): Boolean = choice.heard(report.orElse(other))
end SetBinding
