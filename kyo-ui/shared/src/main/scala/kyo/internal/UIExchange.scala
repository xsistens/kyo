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

    /** The inline-style channel's patch: set the declarations in `set` on the element at `path` and remove the properties in
      * `removed` (the ones the previous value had and this one lacks), in place.
      */
    def onStylePatch(path: Seq[String], set: Seq[(String, String)], removed: Seq[String])(using Frame): Unit < Async = Kyo.unit

    /** Synchronous twins of the four channel patches above.
      *
      * A channel's whole job is one attribute write, so it needs no fiber to deliver it. These sinks run on the
      * writer's stack, inside the `set` that changed the signal, and therefore must not suspend, which is why
      * they return `Unit` rather than `Unit < Async`. Each is the same write as its `on*Patch` twin, and a backend
      * offering one routes the effectful twin through it so the two cannot drift apart.
      *
      * `Absent` for an exchange with no synchronous sink (the server transport has to go over a wire), which keeps
      * its channels on the fiber path.
      */
    def attrPatcherNow: Maybe[(Seq[String], String, String) => Unit]                      = Absent
    def boolAttrPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit]                 = Absent
    def classPatcherNow: Maybe[(Seq[String], String, Boolean) => Unit]                    = Absent
    def stylePatcherNow: Maybe[(Seq[String], Seq[(String, String)], Seq[String]) => Unit] = Absent
end UIExchange

private[kyo] object UIExchange:

    /** An exchange that takes every change and paints nothing: what runs a tree for its effects alone (its mounts, its
      * handlers, the signals they feed), where no page is there to paint.
      */
    val noop: UIExchange = new UIExchange:
        def onChange(
            region: ReactiveRegion,
            path: Seq[String],
            contentContext: ReactiveRegion.RegionIdentity,
            parentContext: ReactiveRegion.ParentContext,
            previous: Maybe[UI],
            ui: UI
        )(using Frame): Unit < Async = Kyo.unit
end UIExchange
