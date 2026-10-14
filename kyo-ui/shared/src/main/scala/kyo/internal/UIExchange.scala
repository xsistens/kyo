package kyo.internal

import kyo.*

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

    /** `region` keeps showing old content while its new content loads (`held`), or no longer. A backend that can mark it makes it inert;
      * one that cannot ignores this, which loses no update, only the marking.
      */
    def onHold(region: ReactiveRegion, held: Boolean)(using Frame): Unit < Async = Kyo.unit

    /** The waiting boundary this exchange belongs to, which counts the passes and mounts inside it; none for the page itself. */
    private[kyo] def gate: Maybe[BoundaryHold] = Absent
end UIExchange
