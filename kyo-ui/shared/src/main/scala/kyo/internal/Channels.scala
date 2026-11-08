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
    classNames: Chunk[Signal[Maybe[String]]],
    style: Maybe[Signal[Style]]
):
    def isEmpty: Boolean  = attrs.isEmpty && bools.isEmpty && classes.isEmpty && classNames.isEmpty && style.isEmpty
    def nonEmpty: Boolean = !isEmpty

    /** The values the channels hold now, which the enclosing paint is about to put into the DOM. */
    def rendered(using AllowUnsafe, Frame, SignalNow.Locals): RenderedChannels =
        RenderedChannels(
            Channels.currentValues(attrs),
            Channels.currentValues(bools),
            Channels.currentValues(classes),
            classNames.map(SignalNow(_)),
            style.map(s => CssStyleRenderer.declarations(SignalNow(s)))
        )
end Channels

private[kyo] object Channels:
    val empty: Channels = Channels(Map.empty, Map.empty, Map.empty, Chunk.empty, Absent)

    /** The channels `el` carries, a `Bound.Follow` among them: its `value` as an attribute channel, masked and formatted the way a render
      * writes it, its `checked` as a boolean one.
      */
    def of(el: Element): Channels =
        val attrs = el.attrs
        val text  = followedValue(el).fold(attrs.reactiveAttrs)(v => attrs.reactiveAttrs.updated("value", v))
        val bools = followedChecked(el).fold(attrs.reactiveBoolAttrs)(c => attrs.reactiveBoolAttrs.updated("checked", c))
        Channels(text, bools, attrs.reactiveClasses, attrs.reactiveClassNames, attrs.reactiveStyle)
    end of

    /** What puts `el` back after the edit `event`, for an element that follows a signal: compares, once the event has finished bubbling, what
      * the signal holds with what the event carried, and sends the patch when they differ; when they are the same the owner took it. A commit
      * always counts. A keystroke counts only where `onInput` is set, so a field with no such handler keeps what is typed until its commit.
      */
    def putBack(el: Element, event: UIEvent, path: Seq[String])(using Frame): Maybe[UIExchange => Unit < Async] =
        def text(carried: String): Maybe[UIExchange => Unit < Async] =
            followedValue(el).map(held =>
                exchange => held.current.map(v => if v == carried then Kyo.unit else exchange.onAttrPatch(path, "value", v))
            )
        event match
            case e: UIEvent.Input =>
                el match
                    case ti: TextInput if ti.onInput.nonEmpty => text(e.value)
                    case _                                    => Absent
            case e: UIEvent.Change        => text(e.value)
            case e: UIEvent.ChangeNumeric => text(NumberFormat.double(e.value))
            case e: UIEvent.ChangeChecked =>
                followedChecked(el).map(held =>
                    exchange => held.current.map(v => if v == e.checked then Kyo.unit else exchange.onBoolAttrPatch(path, "checked", v))
                )
            case _ => Absent
        end match
    end putBack

    /** Whether `el` shows a signal through a `Bound.Follow`. A check on every element of a render, so it allocates nothing. */
    def follows(el: Element): Boolean =
        def isFollow(b: Maybe[UI.Bound[?]]): Boolean = b match
            case Present(_: UI.Bound.Follow[?]) => true
            case _                              => false
        el match
            case ti: TextInput    => isFollow(ti.value)
            case pi: PickerInput  => isFollow(pi.value)
            case ri: RangeInput   => isFollow(ri.value)
            case bi: BooleanInput => isFollow(bi.checked)
            case h: HiddenInput   => isFollow(h.value)
            case _                => false
        end match
    end follows

    private def followedValue(el: Element): Maybe[Signal[String]] =
        given Frame                                                                      = el.frame
        def text(b: Maybe[UI.Bound[String]], mask: Maybe[String]): Maybe[Signal[String]] = b match
            case Present(UI.Bound.Follow(sig)) => Present(mask.fold(sig)(m => sig.map(InputMasking.maskNormalize(m, _))))
            case _                             => Absent
        el match
            case ci: ConstrainedInput => text(ci.value, ci.inputMask)
            case ti: TextInput        => text(ti.value, Absent)
            case pi: PickerInput      => text(pi.value, Absent)
            case h: HiddenInput       => text(h.value, Absent)
            case ri: RangeInput       =>
                ri.value match
                    case Present(UI.Bound.Follow(sig)) => Present(sig.map(NumberFormat.double))
                    case _                             => Absent
            case _ => Absent
        end match
    end followedValue

    private def followedChecked(el: Element): Maybe[Signal[Boolean]] =
        el match
            case bi: BooleanInput =>
                bi.checked match
                    case Present(UI.Bound.Follow(sig)) => Present(sig)
                    case _                             => Absent
            case _ => Absent

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
    // The name each class-name channel held, by position in Channels.classNames.
    classNames: Chunk[Maybe[String]],
    style: Maybe[Seq[(String, String)]]
)

private[kyo] object RenderedChannels:
    val empty: RenderedChannels = RenderedChannels(Map.empty, Map.empty, Map.empty, Chunk.empty, Absent)
