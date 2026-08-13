package kyo.internal

import kyo.*
import kyo.UI.Ast.*

/** One row of a structural list patch: its key, the UI it paints from, and whether that UI is NEW.
  *
  * `changed = false` is the region's statement that it reused the row wholesale — same key, same item value,
  * live observers never torn down — so the painted DOM is already correct and carries any in-place channel
  * patches that landed on it. A backend that can address rows individually may skip such a row entirely.
  */
final private[kyo] case class ListRow(key: String, ui: UI, changed: Boolean)

/** Reactive change notification. Transport-agnostic; each backend renders in its own format.
  *
  * `region` identifies the reactive boundary being updated and `contentContext`/`parentContext` carry the
  * structural context the renderer needs to keep the produced HTML valid where it lands (see
  * [[ReactiveRegion]]).
  *
  * `previous` is the tree this region last rendered, `Absent` on its first render. A backend that can address
  * a nested node uses it to send only the parts that moved (see [[UIDiff]]); one that cannot ignores it and
  * re-sends the region.
  */
private[kyo] trait UIExchange:
    def onChange(
        region: ReactiveRegion,
        path: Seq[String],
        contentContext: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        previous: Maybe[UI],
        ui: UI
    )(using Frame): Unit < Async

    /** Declarative reactive-channel patch: update the attribute/class on the element at `path` IN PLACE (no
      * content replace). Defaulted to a no-op so exchanges without in-place patching (plain-HTML render) need
      * not override.
      */
    def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async      = Kyo.unit
    def onBoolAttrPatch(path: Seq[String], name: String, value: Boolean)(using Frame): Unit < Async = Kyo.unit
    def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async       = Kyo.unit

    /** Synchronous twins of the three channel patches above, for the callback-based binding path.
      *
      * A channel's whole job is one attribute write, so it does not need a fiber to deliver it. These are the
      * sinks for [[kyo.Signal.unsafeObserveProjected]]: they run on the writer's stack, inside the `set` that
      * changed the signal, and therefore must not suspend — which is why they return `Unit` rather than
      * `Unit < Async`. Each is the same write as its `on*Patch` twin, and a backend offering one is expected
      * to route the effectful twin through it so the two cannot drift apart.
      *
      * `Absent` for exchanges with no synchronous sink (the server transport has to go over a wire), which
      * keeps those on the fiber path unchanged.
      */
    def attrPatcherNow: Maybe[(Seq[String], String, String) => Unit]      = Absent
    def boolAttrPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit] = Absent
    def classPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit]    = Absent

    /** Synchronous sink for a region whose content is exactly one text node — a `Signal[String]` lifted into a
      * child position. Same contract as the three above: it runs on the writer's stack and must not suspend.
      *
      * This one has no effectful twin, and deliberately so. The three channel patches describe writes with no
      * equivalent in [[onChange]]: there is no way to say "set this attribute" through a region repaint. A text
      * region's patch does have one — repainting the region with the new `Text` is what the region path already
      * does, correctly, for every transport. `Absent` here is therefore the complete fallback, not a gap to fill
      * later, and a wire command for it would buy nothing over the repaint it would replace.
      */
    def textPatcherNow: Maybe[(Seq[String], String) => Unit] = Absent

    /** Structural patch of a KEYED list region: the whole row order, with the rows that actually need
      * repainting flagged.
      *
      * A list emission is the one change where the payload says almost nothing about the work: removing one
      * row of a thousand and reordering a thousand rows both arrive as "here is the new list". Rendering that
      * to a document and diffing it costs the size of the LIST, not the size of the CHANGE — and the region
      * already knows which rows it reused, so the information to do better is present and was being thrown
      * away at this boundary.
      *
      * The default reassembles the fragment and goes through [[onChange]], which is byte-for-byte the old
      * behavior: a transport that cannot address rows individually loses nothing by not overriding.
      */
    def onListPatch(
        region: ReactiveRegion,
        path: Seq[String],
        contentContext: ReactiveRegion.RegionIdentity,
        parentContext: ReactiveRegion.ParentContext,
        previous: Maybe[UI],
        rows: Seq[ListRow]
    )(using Frame): Unit < Async =
        onChange(
            region,
            path,
            contentContext,
            parentContext,
            previous,
            Fragment[UI](Chunk.from(rows.map(r => KeyedChild[UI](r.key, r.ui))))
        )
end UIExchange
