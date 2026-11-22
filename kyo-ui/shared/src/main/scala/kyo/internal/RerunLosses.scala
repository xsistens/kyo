package kyo.internal

import kyo.*
import kyo.UI.Ast.*

/** What content drawn again over the page takes from the reader, as [[kyo.UI.Loss]] says: a native field whose default the run moves,
  * and a mount with no key.
  *
  * A binding (`value`, `checked`, `selected`) is never a loss: the field shows it on every run, which is what it says (see
  * [[kyo.UI.Bound]]). A default is the reader's field from its start, and a run draws it again without touching what the reader did,
  * unless the default itself moved: the field then starts over from the new one. So the run is compared with what it draws over, place
  * by place, and a default reported where the place had one with another value.
  *
  * The walk goes where the paint goes: through elements, fragments, keyed rows and the regions and lists the content holds, each at
  * the value it shows now, since a run draws those again with it. It stops at a mount, whose content is its instance's.
  */
private[kyo] object RerunLosses:

    /** What drawing `current` over `previous`, the content the same place showed before, takes from the reader. With no previous
      * content the place is new and only an unkeyed mount counts.
      */
    def of(previous: Maybe[UI], current: UI)(using Frame): Chunk[UI.Loss] < Sync =
        for
            before <- previous.fold(Kyo.lift(Chunk.empty[Place]))(walk(_, Nil))
            after  <- walk(current, Nil)
        yield
            given CanEqual[Any, Any] = CanEqual.derived
            val was                  = before.iterator.collect { case Place.Field(at, _, d) => (at, d) }.toMap
            after.flatMap {
                case Place.Mount(loss)     => Chunk(loss)
                case Place.Field(at, e, d) =>
                    was.get(at) match
                        case Some(old) if old.attribute == d.attribute && old.value != d.value =>
                            Chunk(UI.Loss.StatedValue(name(e), d.attribute, e.frame))
                        case _ => Chunk.empty
            }

    /** One default a native field states: the property it starts and the value it starts it at. */
    final private case class Default(attribute: String, value: Any) derives CanEqual

    /** A place the walk met: a native field with its default, at `at`, the path from the content's root by child position, keyed row
      * and region, which is what the paint keeps the element by; or a mount with no key.
      */
    private enum Place:
        case Field(at: List[String], element: Element, default: Default)
        case Mount(loss: UI.Loss)

    private def walk(ui: UI, at: List[String])(using Frame): Chunk[Place] < Sync =
        ui match
            case e: Element =>
                val here = defaultOf(e).fold(Chunk.empty[Place])(d => Chunk(Place.Field(at, e, d)))
                Kyo.foreach(Chunk.from(HtmlRenderer.childrenOf(e).zipWithIndex))((c, i) => walk(c, i.toString :: at))
                    .map(below => here.concat(below.flattenChunk))
            case m: Mounted =>
                if m.key.isEmpty then Chunk(Place.Mount(UI.Loss.UnkeyedMount(m.frame))) else Chunk.empty
            case d: Drawn =>
                if d.key.isEmpty then Chunk(Place.Mount(UI.Loss.UnkeyedMount(d.frame))) else Chunk.empty
            case r: Reactive[?] =>
                r.signal.current(using r.frame).map(walk(_, "r" :: at))
            case f: Foreach[?, ?] =>
                f.applyTyped([T] =>
                    (signal: Signal[Chunk[T]], keyFn: Maybe[T => String], render: (Int, T) => UI) =>
                        signal.current(using f.frame).map(items =>
                            Kyo.foreach(Chunk.from(items.zipWithIndex)) { (item, i) =>
                                val row = keyFn.fold(i.toString)(k => "k:" + k(item))
                                walk(render(i, item), row :: at)
                            }.map(_.flattenChunk)
                        )
                )
            case f: Fragment[?] =>
                Kyo.foreach(Chunk.from(f.children.zipWithIndex))((c, i) => walk(c, i.toString :: at)).map(_.flattenChunk)
            case k: KeyedChild[?] => walk(k.child, ("k:" + k.key) :: at)
            case b: Bounded       => walk(b.child, at)
            case _                => Chunk.empty

    /** The default the element states, if it is a native field and states one. */
    private def defaultOf(e: Element): Maybe[Default] =
        def default[A](attribute: String, b: Maybe[UI.Bound[A]]): Maybe[Default] = b match
            case Present(UI.Bound.Default(v)) => Present(Default(attribute, v))
            case _                            => Absent
        e match
            case t: TextInput    => default("value", t.value)
            case p: PickerInput  => default("value", p.value)
            case b: BooleanInput => default("checked", b.checked)
            case r: RangeInput   => default("value", r.value)
            case o: Opt          => default("selected", o.selected)
            case _               => Absent
        end match
    end defaultOf

    /** The element as a selector names it: its tag, then its id, else its classes. */
    private def name(e: Element): String =
        val tag = HtmlRenderer.tagName(e)
        e.attrs.identifier match
            case Present(id) => s"$tag#$id"
            case Absent      => if e.attrs.cssClasses.isEmpty then tag else e.attrs.cssClasses.mkString(tag + ".", ".", "")
    end name

end RerunLosses
