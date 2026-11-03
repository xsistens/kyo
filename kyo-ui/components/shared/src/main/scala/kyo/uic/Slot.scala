package kyo.uic

import kyo.*
import kyo.UI.*

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

    /** The ref a reader's change is written into, where the slot binds two way. */
    def writable: Maybe[SignalRef[A]] = binding.flatMap(_.writable)

    /** Whether a reader's change reaches anybody: a two-way binding, the slot's event, or another event. */
    def heard: Boolean = writable.isDefined || hearer.isDefined || heardElsewhere

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

    /** Inside a mount the component already has: unbound, the slot becomes a ref of its own, starting at `start`; anything
      * bound stays. A second mount around a slot like this would be a mount inside a mount for every instance, and while a
      * mount restarts kyo-ui shows an inert snapshot.
      */
    def owned(start: A < Sync)(using CanEqual[A, A], Frame): Slot[A] < Sync =
        binding match
            case Present(_) => copy()
            case _          => start.map(mintFrom)

    /** [[owned]] where a constant is only where the slot starts: a text the reader goes on editing. */
    def ownedFrom(unbound: A)(using CanEqual[A, A], Frame): Slot[A] < Sync =
        binding match
            case Present(ReactiveValue.Const(a)) => mintFrom(a)
            case Present(_)                      => copy()
            case _                               => mintFrom(unbound)

    private def mintFrom(a: A)(using CanEqual[A, A], Frame): Slot[A] < Sync =
        Signal.initRef(a).map(ref => copy(binding = Present(ReactiveVariable(ref))))
end Slot

private[uic] object Slot:

    /** What a card for a slot nobody hears says, in one template, so every such card says the same thing.
      *
      * @param component
      *   the component that shows the card
      * @param subject
      *   what is bound one way
      * @param gesture
      *   the reader's act that nothing hears
      * @param event
      *   the event that would hear it
      * @param consequence
      *   what follows, for a slot where it is not that the reader cannot change it
      */
    final case class Name(
        component: String,
        subject: String,
        gesture: String,
        event: String,
        consequence: String = "the reader cannot change it"
    ):
        def advice: String = s"$subject is bound one way and nothing hears $gesture, so $consequence; add $event, or bind a SignalRef"
    end Name

    private val alwaysOn: BoolValue = ReactiveValue.Const(true)

    /** A mount of its own where nothing is bound and the slot's feature can be on; otherwise `draw` as bound, with no mount.
      *
      * `on` is the feature's switch. Own state is minted unless the switch is constant false: under a signal the state is held
      * while the switch is off, so it comes back as the reader left it, and whoever draws reads the switch inside.
      */
    def owning[A](slot: Slot[A], start: A, on: BoolValue = alwaysOn)(draw: Slot[A] => UI)(placeholder: => UI)(
        using
        CanEqual[A, A],
        Frame
    ): UI =
        if slot.binding.isDefined || on.const.contains(false) then draw(slot)
        else UI.owned(start)(own => draw(slot.copy(binding = Present(ReactiveVariable(own))))).placeholder(placeholder)

    /** [[owning]] where a constant is only where the slot starts. */
    def owningFrom[A](slot: Slot[A], unbound: A, on: BoolValue = alwaysOn)(draw: Slot[A] => UI)(placeholder: => UI)(
        using
        CanEqual[A, A],
        Frame
    ): UI =
        slot.binding match
            case Present(ReactiveValue.Const(a)) if !on.const.contains(false) =>
                UI.owned(a)(own => draw(slot.copy(binding = Present(ReactiveVariable(own))))).placeholder(placeholder)
            case Present(_) => draw(slot)
            case _          => owning(slot, unbound, on)(draw)(placeholder)

    /** [[owning]] for a container: the static placeholder is the component drawn over a stand-in, so it does not call the slot
      * fixed, and a card for a slot nobody hears goes in front, for as long as the switch is on.
      */
    def draw[A](slot: Slot[A], name: Name, start: A, on: BoolValue = alwaysOn)(body: Slot[A] => UI)(
        using
        CanEqual[A, A],
        Frame
    ): UI =
        val drawn = owning(slot, start, on)(body)(body(slot.standIn(start)))
        if slot.deaf then Diagnostics.beforeWhile(drawn)(on)(name.component)(b => Seq(Diagnostic.unheard(slot, name, b)))
        else drawn
    end draw

    /** What a field with native state draws from.
      *
      * @param value
      *   what it shows at this render
      * @param native
      *   what its native element binds: a value where a region around it follows the slot, a signal where the element has to
      *   follow it itself so kyo-ui puts back what its owner did not take
      * @param edit
      *   what the reader's change does, `Absent` where nobody hears it
      * @param fixed
      *   the slot is a constant nobody hears
      */
    final case class Field[A](value: A, native: A | Signal[A], edit: Maybe[A => UI.Action], fixed: Boolean)

    /** Draws a field with native state from its slot, the same way for each binding. */
    def drawField[A](slot: Slot[A], unbound: A)(body: Field[A] => UI)(using CanEqual[A, A], Frame): UI =
        val edit: Maybe[A => UI.Action] = if slot.heard then Present((a: A) => slot.change(a)) else Absent
        slot.binding match
            case Present(ReactiveVariable(ref))                    => ref.render(a => body(Field(a, a, edit, false)))
            case Present(ReactiveValue.Dyn(sig))                   => sig.render(a => body(Field(a, sig, edit, false)))
            case Present(c @ ReactiveValue.Const(a)) if slot.heard => body(Field(a, c.native, edit, false))
            case Present(ReactiveValue.Const(a))                   => body(Field(a, a, Absent, true))
            case _                                                 => body(Field(unbound, unbound, edit, false))
        end match
    end drawField

    /** A native binding through a pure function: a clamp, a format. */
    def mapNative[A, B](n: A | Signal[A])(f: A => B)(using CanEqual[B, B], Frame): B | Signal[B] =
        n match
            case s: Signal[A] @unchecked => s.map(f)
            case a: A @unchecked         => f(a)
end Slot
