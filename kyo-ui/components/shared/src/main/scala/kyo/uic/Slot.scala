package kyo.uic

import kyo.*

/** One state slot under rule A (see "One setter per slot" in this module's CLAUDE.md): what the caller bound, and who hears a
  * reader's change.
  *
  * A component keeps one per slot in place of a binding and a callback side by side, and asks it the questions the rule asks:
  * is the change heard ([[heard]]), is this a value meant to stay ([[fixed]]), is it a mistake a card has to name ([[deaf]]).
  * [[change]] and [[update]] are the reader's change itself, written to a two-way binding first and then reported. A setter is
  * one line, `copy(xSlot = xSlot.bind(v))` and `copy(xSlot = xSlot.hear(f))`.
  *
  * One hearer, not a list: `.onChange(a).onChange(b)` replaces, as every setter in this module does. An event that carries
  * another value, or fires at another moment, keeps its own field and calls [[heardToo]] at its setter.
  *
  * @param binding
  *   what the caller bound, `Absent` where the slot is the component's own
  * @param hearer
  *   the slot's own event, which hears every change with the new value
  * @param heardElsewhere
  *   another event hears a change too, one that does not carry this value (`onNodeToggle`, `onPick`)
  * @param quiet
  *   a host that embeds this component reports the mistake itself, or the binding is a stand-in nobody made: never a card
  */
final private[uic] case class Slot[A](
    binding: Maybe[ReactiveValue[A]] = Absent,
    hearer: Maybe[A => UI.Action] = Absent,
    heardElsewhere: Boolean = false,
    quiet: Boolean = false
):
    /** The slot bound as the caller says: a ref two way, a signal one way, anything else a constant. */
    def bind(v: A | Signal[A]): Slot[A] = copy(binding = Present(ReactiveValue(v)))

    /** The slot with `f` hearing every change, in place of whoever heard it before. */
    def hear(f: A => UI.Action)(using Frame): Slot[A] = copy(hearer = Present(Callback.of(f)))

    /** The slot with an event of another shape hearing it too. */
    def heardToo: Slot[A] = copy(heardElsewhere = true)

    /** The slot with no card to show, whatever it is bound to. */
    def quieted: Slot[A] = copy(quiet = true)

    /** The slot over the constant signal a static placeholder draws from: the same binding class the mount's own state has,
      * so the page as first rendered does not call it fixed, and quiet, since nobody made this binding.
      */
    def standIn(start: A)(using CanEqual[A, A], Frame): Slot[A] =
        copy(binding = Present(ReactiveValue.Dyn(Signal.initConst(start))), quiet = true)

    /** Whether a reader's change reaches anybody: a two-way binding, the slot's event, or another event. */
    def heard: Boolean = binding.exists(_.writable.isDefined) || hearer.isDefined || heardElsewhere

    /** A constant nobody hears: a value meant to stay, which the component draws read-only and says nothing about. */
    def fixed: Boolean = binding.exists(_.const.isDefined) && !heard

    /** Whether the slot is fixed at exactly `v`. */
    def fixedAt(v: A)(using CanEqual[A, A]): Boolean = fixed && binding.exists(_.const.contains(v))

    /** A signal bound one way that nobody hears: the state lives somewhere else and the reader's change reaches nobody, the
      * one binding a card names.
      */
    def deaf: Boolean = !quiet && !heard && binding.exists(_.dyn.isDefined)

    /** A reader's change to `next`: a two-way binding is written first, then the hearer told, whatever the binding is. */
    def change(next: A)(using Frame): UI.Action = ReactiveValue.change(binding, next, hearer)

    /** A reader's change that is a step from the value held: a two-way binding steps its ref atomically, a one-way one the
      * value it shows, and the hearer is told the result. Unbound nothing happens, since there is nothing to step from.
      */
    def update(step: A => A)(using Frame): UI.Action = ReactiveValue.update(binding, step, hearer)

    /** What one render reads of the slot, with `unbound` standing in where nothing is bound. */
    def reading(unbound: A)(using CanEqual[A, A], Frame): Reading[A] =
        binding match
            case Present(v) => Reading.of(v)
            case _          => Reading.Fixed(unbound)

    /** The value at this moment, with `unbound` standing in where nothing is bound. */
    def current(unbound: A)(using Frame): A < Sync =
        binding match
            case Present(v) => v.current
            case _          => unbound

    /** The binding as the caller gave it, for a native setter or a component a host embeds. */
    def bound: Maybe[A | Signal[A]] = binding.map(_.bound)
end Slot
