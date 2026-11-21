package kyo.internal

import kyo.*
import kyo.UI.Ast.*

/** An element's reactive channels: the signals that patch its attributes, boolean attributes, classes and inline style in place.
  *
  * The one place that says what an element's channels are. Anything that adds a kind of binding merges it in [[Channels.of]], and every
  * reader (normalization, a region's root, the observers) follows. A check on every element of a render reads [[kyo.UI.Ast.Attrs.hasChannels]]
  * instead, which allocates nothing.
  */
final private[kyo] case class Channels(
    attrs: Map[String, Signal[String]],
    bools: Map[String, Signal[Boolean]],
    classes: Map[String, Signal[Boolean]],
    style: Maybe[Signal[Style]]
):
    def isEmpty: Boolean  = attrs.isEmpty && bools.isEmpty && classes.isEmpty && style.isEmpty
    def nonEmpty: Boolean = !isEmpty

    /** The values the channels hold now, which the enclosing paint is about to put into the DOM. */
    def rendered(using AllowUnsafe, Frame, SignalNow.Locals): RenderedChannels =
        RenderedChannels(
            Channels.currentValues(attrs),
            Channels.currentValues(bools),
            Channels.currentValues(classes),
            style.map(s => CssStyleRenderer.declarations(SignalNow(s)))
        )
end Channels

private[kyo] object Channels:
    val empty: Channels = Channels(Map.empty, Map.empty, Map.empty, Absent)

    /** The channels `el` carries. */
    def of(el: Element): Channels =
        Channels(el.attrs.reactiveAttrs, el.attrs.reactiveBoolAttrs, el.attrs.reactiveClasses, el.attrs.reactiveStyle)

    /** The current value of every bound signal in `channels`, by name. Most elements bind none, so an empty
      * map answers without a loop.
      */
    private def currentValues[A](channels: Map[String, Signal[A]])(using AllowUnsafe, Frame, SignalNow.Locals): Map[String, A] =
        if channels.isEmpty then Map.empty[String, A]
        else
            val b = Map.newBuilder[String, A]
            channels.foreach((n, s) => b += n -> SignalNow(s))
            b.result()
end Channels

/** Render-time snapshots of an element's [[Channels]]: the values the enclosing paint already put into the DOM. Subscribe skips an
  * observer's first emission while it still equals its snapshot, so a fresh paint is not patched again. `style` holds the inline-style
  * channel's declarations (see UI.Element.inlineStyle), which is what its first patch diffs against.
  */
final private[kyo] case class RenderedChannels(
    attrs: Map[String, String],
    bools: Map[String, Boolean],
    classes: Map[String, Boolean],
    style: Maybe[Seq[(String, String)]]
)

private[kyo] object RenderedChannels:
    val empty: RenderedChannels = RenderedChannels(Map.empty, Map.empty, Map.empty, Absent)
