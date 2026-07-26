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

    /** Declarative reactive-channel patch: update the attribute/class on the element at `path` IN PLACE (no
      * content replace). Defaulted to a no-op so exchanges without in-place patching (plain-HTML render) need
      * not override.
      */
    def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async      = Kyo.unit
    def onBoolAttrPatch(path: Seq[String], name: String, value: Boolean)(using Frame): Unit < Async = Kyo.unit
    def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async       = Kyo.unit
end UIExchange
