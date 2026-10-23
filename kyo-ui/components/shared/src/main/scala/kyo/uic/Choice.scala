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

    /** Renders `f` with the held option, `Absent` for none. */
    def render(f: Maybe[A] => UI)(using Frame): UI =
        this match
            case Value(ReactiveValue.Dyn(sig))    => sig.render(a => f(Present(a)))
            case Value(ReactiveValue.Const(a))    => f(Present(a))
            case Optional(ReactiveValue.Dyn(sig)) => sig.render(f)
            case Optional(ReactiveValue.Const(m)) => f(m)

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
  * options (`selection` over a `SignalRef[Set[A]]`).
  *
  * The [[SingleChoice]] rules at a set: the field works on keys, and a write puts back the options that carry the keys it
  * names. A member the ref already holds stays as the app wrote it while its key stays chosen.
  */
final private[uic] case class MultiChoice[A](ref: SignalRef[Set[A]]):

    /** Renders `f` with the held options. */
    def render(f: Set[A] => UI)(using Frame): UI = ref.render(f)

    /** Runs `f` with the held options as they are at this moment. */
    def read(f: Set[A] => UI.Action)(using Frame): UI.Action = ref.use(f)

    /** Replaces the chosen keys `ks` with `f(ks)`, written back as the options among `options` that carry them, and answers
      * the set now held where the keys changed.
      */
    def update(options: => Seq[A], key: A => String)(f: Set[String] => Set[String])(using Frame): Maybe[Set[A]] < Sync =
        def next(prev: Set[A]): Set[A] =
            val now   = f(prev.map(key))
            val kept  = prev.filter(a => now.contains(key(a)))
            val known = kept.map(key)
            kept ++ options.filter(a => now.contains(key(a)) && !known.contains(key(a)))
        end next
        ref.getAndUpdate(next).map { prev =>
            val now = next(prev)
            if now.map(key) == prev.map(key) then Absent else Present(now)
        }
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
    def read(f: Set[K] => UI.Action)(using Frame): UI.Action
    def update(f: Set[K] => Set[K])(using Frame): UI.Action
end SetBinding

private[uic] object SetBinding:

    /** The app's own set, written as it is. */
    def of[K](ref: SignalRef[Set[K]]): SetBinding[K] =
        new SetBinding[K]:
            def render(f: Set[K] => UI)(using Frame): UI             = ref.render(f)
            def read(f: Set[K] => UI.Action)(using Frame): UI.Action = ref.use(f)
            def update(f: Set[K] => Set[K])(using Frame): UI.Action  = ref.getAndUpdate(f).unit

    /** A typed set of options read and written through their keys, as [[MultiChoice]] does. */
    def keyed[A](choice: MultiChoice[A], options: => Seq[A], key: A => String): SetBinding[String] =
        new SetBinding[String]:
            def render(f: Set[String] => UI)(using Frame): UI                 = choice.render(as => f(as.map(key)))
            def read(f: Set[String] => UI.Action)(using Frame): UI.Action     = choice.read(as => f(as.map(key)))
            def update(f: Set[String] => Set[String])(using Frame): UI.Action =
                choice.update(options, key)(f).unit
end SetBinding
