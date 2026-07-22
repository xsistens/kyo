package kyo.internal

import kyo.*

/** Reactive change notification. Transport-agnostic; each backend renders in its own format.
  *
  * `mount` marks the paint of a keyless mount's content root: the backend flags that DOM node so a parent region's
  * in-place update treats it as an opaque boundary (the mount owns its own subtree and repaints it itself) rather
  * than reconciling it against the placeholder a top-down re-render emits there.
  */
private[kyo] trait UIExchange:
    def onChange(path: Seq[String], ui: UI, mount: Boolean = false)(using Frame): Unit < Async

    /** Declarative reactive-attribute patch: set `name=value` on the element at `path` IN PLACE (no
      * content replace), driven by a `Signal[String]` bound via `aria(name, sig)` / `placeholder(sig)` /
      * `title(sig)`. Defaulted so exchanges that don't support in-place attr patching (plain-HTML render)
      * need not override it.
      */
    def onAttrPatch(path: Seq[String], name: String, value: String)(using Frame): Unit < Async      = Kyo.unit
    def onBoolAttrPatch(path: Seq[String], name: String, value: Boolean)(using Frame): Unit < Async = Kyo.unit
    def onClassPatch(path: Seq[String], name: String, on: Boolean)(using Frame): Unit < Async       = Kyo.unit
end UIExchange
