package kyo.uic

import kyo.*
import kyo.UI.*

/** What a single-choice field ([[Select]], [[CascadeSelect]], [[SelectButton]]) holds its choice in: the chosen option's key
  * (`value`), the option itself (`selection` over a `SignalRef[A]`), or the option or none (`selection` over a
  * `SignalRef[Maybe[A]]`).
  *
  * The fields work on keys throughout, because the key is what the panel, the keyboard, the hidden form input and the form layer
  * compare, with `""` meaning none. This type is the one place a typed ref is read as a key and a key the field writes is
  * written back as the option it names. Matching goes through the key and never through the option type's own equality, so
  * an app value that is a copy of an option still picks that option's row.
  *
  * A write that changes nothing (the key the ref already names) leaves the ref as it is, so the instance the app holds
  * survives a pick of the row already chosen. A typed ref cannot hold a key no option carries, so such a write leaves it too;
  * and a ref over `A` cannot hold none, so a write of `""` leaves it, which is why the fields offer no clear over it.
  */
private[uic] enum SingleChoice[A]:
    case Keys(ref: SignalRef[String])
    case Value(ref: SignalRef[A])
    case Optional(ref: SignalRef[Maybe[A]])

    /** Renders `f` with the chosen key, `""` for none. */
    def render(key: A => String)(f: String => UI)(using Frame): UI =
        this match
            case Keys(r)     => r.render(f)
            case Value(r)    => r.render(a => f(key(a)))
            case Optional(r) => r.render(m => f(m.fold("")(key)))

    /** Runs `f` with the chosen key as it is at this moment. */
    def read(key: A => String)(f: String => UI.Action)(using Frame): UI.Action =
        this match
            case Keys(r)     => r.use(f)
            case Value(r)    => r.use(a => f(key(a)))
            case Optional(r) => r.use(m => f(m.fold("")(key)))

    /** Replaces the chosen key `k` with `f(k)`, written back as the option among `options` that carries it. */
    def update(options: => Seq[A], key: A => String)(f: String => String)(using Frame): UI.Action =
        def find(k: String): Maybe[A] = Maybe.fromOption(options.find(a => key(a) == k))
        this match
            case Keys(r)  => r.getAndUpdate(f).unit
            case Value(r) =>
                r.getAndUpdate { prev =>
                    val now = f(key(prev))
                    if now == key(prev) then prev else find(now).getOrElse(prev)
                }.unit
            case Optional(r) =>
                r.getAndUpdate { prev =>
                    val was = prev.fold("")(key)
                    val now = f(was)
                    if now == was then prev
                    else if now.isEmpty then Absent
                    else
                        find(now) match
                            case Present(a) => Present(a)
                            case Absent     => prev
                    end if
                }.unit
        end match
    end update

    /** Whether the ref can hold none, which is what a clear writes. */
    def clearable: Boolean =
        this match
            case Value(_) => false
            case _        => true
end SingleChoice

/** What a multi-choice field ([[MultiSelect]], [[SelectButton]] with `multiple(true)`) holds its choice in: the set of chosen
  * keys (`value`) or the set of chosen options (`selection` over a `SignalRef[Set[A]]`).
  *
  * The [[SingleChoice]] rules at a set: the field works on keys, and a write puts back the options that carry the keys it
  * names. A member the ref already holds stays as the app wrote it while its key stays chosen, so a member no option carries
  * (an option the list no longer shows) survives the toggles of the others.
  */
private[uic] enum MultiChoice[A]:
    case Keys(ref: SignalRef[Set[String]])
    case Values(ref: SignalRef[Set[A]])

    /** Renders `f` with the chosen keys. */
    def render(key: A => String)(f: Set[String] => UI)(using Frame): UI =
        this match
            case Keys(r)   => r.render(f)
            case Values(r) => r.render(as => f(as.map(key)))

    /** Runs `f` with the chosen keys as they are at this moment. */
    def read(key: A => String)(f: Set[String] => UI.Action)(using Frame): UI.Action =
        this match
            case Keys(r)   => r.use(f)
            case Values(r) => r.use(as => f(as.map(key)))

    /** Replaces the chosen keys `ks` with `f(ks)`, written back as the options among `options` that carry them. */
    def update(options: => Seq[A], key: A => String)(f: Set[String] => Set[String])(using Frame): UI.Action =
        this match
            case Keys(r)   => r.getAndUpdate(f).unit
            case Values(r) =>
                r.getAndUpdate { prev =>
                    val now   = f(prev.map(key))
                    val kept  = prev.filter(a => now.contains(key(a)))
                    val known = kept.map(key)
                    kept ++ options.filter(a => now.contains(key(a)) && !known.contains(key(a)))
                }.unit
end MultiChoice
