package kyo.internal

import kyo.*

/** Reactive change notification. Transport-agnostic; each backend renders in its own format.
  *
  * `mount` marks the paint of a keyless mount's content root: the backend flags that DOM node so a parent's
  * in-place update treats it as an opaque boundary (the mount owns and repaints its own subtree) rather than
  * reconciling it against the placeholder a top-down re-render emits there.
  */
private[kyo] trait UIExchange:
    def onChange(path: Seq[String], ui: UI, mount: Boolean = false)(using Frame): Unit < Async

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
end UIExchange
