package kyo.uic

import kyo.*
import kyo.UI.*

/** What a single-choice field ([[Select]], [[CascadeSelect]], [[SelectButton]]) holds its choice in: the option itself
  * (`selection` over a `SignalRef[A]`), or the option or none (`selection` over a `SignalRef[Maybe[A]]`).
  *
  * The fields compare options by their key, because the key is what the panel, the keyboard and the hidden form input work
  * with, and never by the option type's own equality: an app value that is a copy of an option still picks that option's row.
  * A write that names the key the ref already holds leaves the ref as it is, so the instance the app wrote survives a pick of
  * the row already chosen, and a ref over `A` cannot hold none, so a write of none leaves it too. Both cases report no change,
  * which is what keeps `onChange` from announcing a write that did not happen.
  */
private[uic] enum SingleChoice[A]:
    case Value(ref: SignalRef[A])
    case Optional(ref: SignalRef[Maybe[A]])

    /** Renders `f` with the held option, `Absent` for none. */
    def render(f: Maybe[A] => UI)(using Frame): UI =
        this match
            case Value(r)    => r.render(a => f(Present(a)))
            case Optional(r) => r.render(f)

    /** Runs `f` with the held option as it is at this moment. */
    def read(f: Maybe[A] => UI.Action)(using Frame): UI.Action =
        this match
            case Value(r)    => r.use(a => f(Present(a)))
            case Optional(r) => r.use(f)

    /** Writes `next` unless it names the key already held (or is none over a ref that cannot hold it), and answers the value
      * now held where the write changed it.
      */
    def write(next: Maybe[A], key: A => String)(using Frame): Maybe[Maybe[A]] < Sync =
        update(key)(_ => next)

    /** [[write]] of `f` applied to the held option, as one atomic step. */
    def update(key: A => String)(f: Maybe[A] => Maybe[A])(using Frame): Maybe[Maybe[A]] < Sync =
        def same(a: Maybe[A], b: Maybe[A]): Boolean = a.map(key) == b.map(key)
        this match
            case Value(r) =>
                // A ref over `A` takes only an option, so a step to none leaves it.
                def step(prev: A): Maybe[A] =
                    f(Present(prev)) match
                        case n @ Present(a) if key(a) != key(prev) => n
                        case _                                     => Absent
                r.getAndUpdate(prev => step(prev).getOrElse(prev)).map(prev => step(prev).map(a => Present(a)))
            case Optional(r) =>
                r.getAndUpdate(prev => if same(prev, f(prev)) then prev else f(prev)).map { prev =>
                    val now = f(prev)
                    if same(prev, now) then Absent else Present(now)
                }
        end match
    end update

    /** Whether the ref can hold none, which is what a clear writes. */
    def clearable: Boolean =
        this match
            case Value(_) => false
            case _        => true
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
