package kyo.internal

import kyo.*
import scala.deriving.Mirror

/** The props of an instance with each [[kyo.UI.Ownable]] prop resolved: where a run binds none, the instance's own ref is bound in
  * its place, so a draw reads every such prop as bound.
  *
  * The refs are made once, when the instance first draws, one for each ownable prop that has somewhere to start, found by its
  * position among the props' fields; every run's props are resolved against them. [[carry]] runs before a run's props are seen: a
  * prop the previous run bound and the new run leaves unbound hands the value it showed to the instance's own ref, so the instance
  * carries on from it.
  */
final private[kyo] class Owning[P] private (mirror: Maybe[Mirror.ProductOf[P]], refs: Chunk[(Int, SignalRef[Any])]):

    /** `props` with each unbound ownable prop bound to the instance's own ref. */
    def resolve(props: P): P =
        mirror match
            case Present(m) if refs.nonEmpty =>
                val fields = props.asInstanceOf[Product].productIterator.toArray
                refs.foreach { (i, ref) =>
                    fields(i) match
                        case c: UI.Ownable[?] if c.bound.isEmpty => fields(i) = c.asInstanceOf[UI.Ownable[Any]].bind(ref)
                        case _                                   => ()
                }
                m.fromProduct(Tuple.fromArray(fields))
            case _ => props

    /** Before the props `next` are seen: each ownable prop `prev` bound and `next` leaves unbound hands its value to the own ref. */
    def carry(prev: P, next: P)(using Frame): Unit < Sync =
        // Any equality is the props' own: the refs hold the values their props declare, compared as those values compare.
        given CanEqual[Any, Any] = CanEqual.derived
        Kyo.foreachDiscard(refs) { (i, ref) =>
            (field(prev, i), field(next, i)) match
                case (was: UI.Ownable[?], now: UI.Ownable[?]) if was.bound.isDefined && now.bound.isEmpty =>
                    UI.Prop.signal(was.bound.get.asInstanceOf[UI.Prop[Any]]).current.map(ref.set)
                case _ => Kyo.unit
        }
    end carry

    /** A kept instance over these refs, which later runs can hand new props: the first run's props resolved once, here. */
    def kept(first: P)(using CanEqual[P, P], Frame): UI.Live[P] < Sync =
        given CanEqual[Owning.Run[P], Owning.Run[P]] = CanEqual.derived
        Signal.initRef(Owning.Run(first, resolve(first))).map(ref => new UI.Live(first, Present(ref), this))
    end kept

    private def field(props: P, i: Int): Any = props.asInstanceOf[Product].productElement(i)
end Owning

private[kyo] object Owning:

    /** One run of a kept instance: its props as the run gave them, compared with the next run's, and as the draw reads them, with each
      * unbound ownable prop bound to the instance's own ref, resolved once when the run is offered.
      */
    final case class Run[P](props: P, resolved: P)

    /** No ownable props: the props as they are. */
    def none[P]: Owning[P] = new Owning(Absent, Chunk.empty)

    /** One own ref for each ownable prop of `first` that has a start, at the start it declares. */
    def start[P](first: P, mirror: Mirror.ProductOf[P])(using Frame): Owning[P] < Sync =
        given CanEqual[Any, Any] = CanEqual.derived
        val owned                = Chunk.from(first.asInstanceOf[Product].productIterator.zipWithIndex.collect {
            case (c: UI.Ownable[?], i) if c.start.isDefined => (i, c.start.get: Any)
        })
        if owned.isEmpty then none
        else Kyo.foreach(owned)((i, a) => Signal.initRef[Any](a).map(ref => (i, ref))).map(refs => new Owning(Present(mirror), refs))
    end start
end Owning
