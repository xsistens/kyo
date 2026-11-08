package kyo.internal

import kyo.*
import kyo.UI.Ast.*

/** What content drawn again over the page takes from the reader, as [[kyo.UI.Loss]] says: a native field whose value the content
  * states as a constant, and a mount with no key.
  *
  * The walk goes where the paint goes: through elements, fragments, keyed rows and the regions and lists the content holds, each at
  * the value it shows now, since a run draws those again with it. It stops at a mount, whose content is its instance's.
  */
private[kyo] object RerunLosses:

    def of(ui: UI)(using Frame): Chunk[UI.Loss] < Sync =
        ui match
            case e: Element =>
                Kyo.foreach(e.children)(of).map(below => stated(e).concat(below.flattenChunk))
            case m: Mounted     => if m.key.isEmpty then Chunk(UI.Loss.UnkeyedMount(m.frame)) else Chunk.empty
            case d: Drawn       => if d.key.isEmpty then Chunk(UI.Loss.UnkeyedMount(d.frame)) else Chunk.empty
            case r: Reactive[?] =>
                r.signal.current(using r.frame).map(of)
            case f: Foreach[?, ?] =>
                f.applyTyped([T] =>
                    (signal: Signal[Chunk[T]], _: Maybe[T => String], render: (Int, T) => UI) =>
                        signal.current(using f.frame).map(items =>
                            Kyo.foreach(Chunk.from(items.zipWithIndex))((item, i) => of(render(i, item))).map(_.flattenChunk)
                        )
                )
            case f: Fragment[?]   => Kyo.foreach(f.children)(of).map(_.flattenChunk)
            case k: KeyedChild[?] => of(k.child)
            case b: Bounded       => of(b.child)
            case _                => Chunk.empty

    /** The value the element states as a constant, if it is a native field and states one. */
    private def stated(e: Element): Chunk[UI.Loss] =
        def loss(attribute: String)               = Chunk(UI.Loss.StatedValue(name(e), attribute, e.frame))
        def const(b: Maybe[UI.Bound[?]]): Boolean = b match
            case Present(_: UI.Bound.Const[?]) => true
            case _                             => false
        e match
            case t: TextInput if const(t.value)      => loss("value")
            case p: PickerInput if const(p.value)    => loss("value")
            case b: BooleanInput if const(b.checked) => loss("checked")
            case r: RangeInput if const(r.value)     => loss("value")
            case o: Opt if o.selected.nonEmpty       => loss("selected")
            case _                                   => Chunk.empty
        end match
    end stated

    /** The element as a selector names it: its tag, then its id, else its classes. */
    private def name(e: Element): String =
        val tag = HtmlRenderer.tagName(e)
        e.attrs.identifier match
            case Present(id) => s"$tag#$id"
            case Absent      => if e.attrs.cssClasses.isEmpty then tag else e.attrs.cssClasses.mkString(tag + ".", ".", "")
    end name

end RerunLosses
