package kyo.uic

import kyo.*
import kyo.UI.*

/** One state slot under rule A (see "One setter per slot" in this module's CLAUDE.md): what the caller bound, and who hears a
  * reader's change.
  *
  * A component keeps one per slot in place of a binding and a callback side by side, and asks it the questions the rule asks:
  * is the change heard ([[heard]]), is this a value meant to stay ([[fixed]]), is it a mistake a card has to name ([[deaf]]).
  * [[change]] and [[update]] are the reader's change itself, written to a two-way binding first and then reported. A setter is
  * one line, `new X(s.copy(xSlot = xSlot.bind(v)))` and `new X(s.copy(xSlot = xSlot.hear(f)))`.
  *
  * The name covers both of React's cases: controlled where the caller binds, and uncontrolled where nothing is bound and the
  * instance holds the value itself, from [[start]].
  *
  * One hearer, not a list: `.onChange(a).onChange(b)` replaces, as every setter in this module does. An event that carries
  * another value, or fires at another moment, keeps its own field and calls [[heardToo]] at its setter.
  *
  * @param bound
  *   what the caller bound, as a [[kyo.UI.Prop]]: a ref two way, any other signal one way, a constant; `Absent` where the slot
  *   is the component's own
  * @param hearer
  *   the slot's own event, which hears every change with the new value
  * @param heardElsewhere
  *   another event hears a change too, one that does not carry this value (`onNodeToggle`, `onPick`)
  * @param quiet
  *   a host that embeds this component reports the mistake itself, or the binding is a stand-in nobody made: never a card
  * @param start
  *   where the instance's own value starts, for a slot the component holds itself where the caller binds nothing (a
  *   [[kyo.UI.Ownable]] prop the engine binds to the instance's own ref); `Absent` where it holds none
  */
final private[uic] case class Controlled[A](
    bound: Maybe[Prop[A]] = Absent,
    hearer: Maybe[A => UI.Action] = Absent,
    heardElsewhere: Boolean = false,
    quiet: Boolean = false,
    start: Maybe[A] = Absent
) extends UI.Ownable[A]:
    /** The slot bound as the caller says: a ref two way, a signal one way, anything else a constant. Every binding holds the slot
      * (see [[kyo.UI.Bound]]): what the component shows is what the binding says. A start the slot already has stays, so an instance
      * still holds a value of its own to carry on from when a later run stops binding it.
      */
    def bind(v: Prop[A]): Controlled[A] = copy(bound = Present(v))

    /** The slot with `a` as where the instance's own value starts, for a value the reader owns from there: the `defaultValue` of a
      * field. A binding, where there is one, still holds the slot.
      */
    def startAt(a: A): Controlled[A] = copy(start = Present(a))

    /** The slot with nothing bound: the component's own. */
    def unbound: Controlled[A] = copy(bound = Absent)

    /** What was bound, as the `ReactiveValue` the components the waves have not reached yet read. A bridge: it goes with
      * `ReactiveValue` in the last wave.
      */
    def binding: Maybe[ReactiveValue[A]] = bound.map(ReactiveValue(_))

    /** The slot with `f` hearing every change, in place of whoever heard it before. */
    def hear(f: A => UI.Action)(using Frame): Controlled[A] = withHearer(Callback.of(f))

    /** The slot with `f` hearing every change, run as given: the generated event setter wraps `f` itself. */
    def withHearer(f: A => UI.Action): Controlled[A] = copy(hearer = Present(f))

    /** The slot with an event of another shape hearing it too. */
    def heardToo: Controlled[A] = copy(heardElsewhere = true)

    /** The slot with no card to show, whatever it is bound to. */
    def quieted: Controlled[A] = copy(quiet = true)

    /** The ref a reader's change is written into, where the slot binds two way. */
    def writable: Maybe[SignalRef[A]] = bound.flatMap(Prop.writable)

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
    def reading(unbound: A): ReactiveValue[A] = binding.getOrElse(ReactiveValue.Const(unbound))

    /** What a draw reads of the slot as a prop: the binding, else where the slot starts, else `unbound`. Inside a
      * `mountedWith` draw a slot with a start is always bound (the engine binds the instance's own ref where the run binds
      * nothing), so `unbound` stands in only where no engine runs, as for a host that draws the component in place.
      */
    def value(unbound: A): Prop[A] = bound.getOrElse(start.getOrElse(unbound))

    /** The value at this moment, with `unbound` standing in where nothing is bound. */
    def current(unbound: A)(using Frame): A < Sync = reading(unbound).current

    /** Inside a mount the component already has: unbound, the slot becomes a ref of its own, starting at `start`; anything
      * bound stays. A second mount around a slot like this would be a mount inside a mount for every instance, and while a
      * mount restarts kyo-ui shows an inert snapshot.
      */
    def owned(start: A < Sync)(using CanEqual[A, A], Frame): Controlled[A] < Sync =
        binding match
            case Present(_) => copy()
            case _          => start.map(mintFrom)

    private def mintFrom(a: A)(using CanEqual[A, A], Frame): Controlled[A] < Sync =
        Signal.initRef(a).map(ref => copy(bound = Present(ref)))
end Controlled

private[uic] object Controlled:

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
    def owning[A](slot: Controlled[A], start: A, on: BoolValue = alwaysOn)(draw: Controlled[A] => UI)(using CanEqual[A, A], Frame): UI =
        if slot.binding.isDefined || on.const.contains(false) then draw(slot)
        else UI.mounted(Signal.initRef(start).map(own => draw(slot.copy(bound = Present(own)))))

    /** One instance per place for a component whose props hold its own state in a [[Controlled]] slot with a start: the engine binds
      * the instance's own ref where the run binds nothing, and a host region that runs again hands the instance the run's props
      * instead of starting it over, so what the reader gave an unbound component stays. `draw` draws the props as resolved: drawn
      * once where no run can follow, and again for each run's props where one can.
      */
    def instance[P](props: P)(draw: P => UI)(using CanEqual[P, P], scala.deriving.Mirror.ProductOf[P], Frame): UI =
        UI.mountedWith(props) { live =>
            live(q => q: Prop[P]) match
                case runs: Signal[P] @unchecked => runs.render(draw)
                case q: P @unchecked            => draw(q)
        }

    /** [[owning]] for a container: a card for a slot nobody hears goes in front, for as long as the switch is on. */
    def draw[A](slot: Controlled[A], name: Name, start: A, on: BoolValue = alwaysOn)(body: Controlled[A] => UI)(using
        CanEqual[A, A],
        Frame
    ): UI =
        val drawn = owning(slot, start, on)(body)
        if slot.deaf then Diagnostics.beforeWhile(drawn)(on)(name.component)(b => Seq(Diagnostic.unheard(slot, name, b)))
        else drawn
    end draw

    /** What a field with native state draws from.
      *
      * @param value
      *   what it shows at this render
      * @param native
      *   what its native element binds: a value where a region around it follows the slot, a signal where the element
      *   follows it itself; either way kyo-ui puts back what its owner did not take (see [[kyo.UI.Bound]])
      * @param edit
      *   what the reader's change does, `Absent` where nobody hears it
      * @param fixed
      *   the slot is a constant nobody hears
      */
    final case class Field[A](value: A, native: A | Signal[A], edit: Maybe[A => UI.Action], fixed: Boolean)

    /** Draws a field with native state from its slot, the same way for each binding. */
    def drawField[A](slot: Controlled[A], unbound: A)(body: Field[A] => UI)(using CanEqual[A, A], Frame): UI =
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
end Controlled
