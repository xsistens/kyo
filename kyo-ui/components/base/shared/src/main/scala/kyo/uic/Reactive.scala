package kyo.uic

import kyo.*
import scala.util.NotGiven

/** What a [[reactive]] block reads with: `.read` on a prop or a signal, which exists only inside the block.
  *
  * Outside a block `.read` is the value now (see the extensions after this class), and `NotGiven` keeps those from applying here. The
  * expansion takes every read out of the block; a read it reaches only through this scope handed on to other code is a compile error.
  */
final class ReactiveScope private[kyo] ():

    /** The prop's value, read once where the block starts. */
    extension [A](inline self: UI.Prop[A])
        inline def read: A = ${ kyo.uic.internal.ReactiveMacro.strayImpl[A]('self) }

    /** The signal's value, read once where the block starts. A receiver of its own, since a bare signal against [[UI.Prop]] would
      * read as the signal itself.
      */
    extension [A](inline self: Signal[A])
        @scala.annotation.targetName("readSignal")
        inline def read: A = ${ kyo.uic.internal.ReactiveMacro.strayImpl[A]('self) }

    /** An optional prop's value (an `Opt[Prop[A]]` field, or any `Maybe[Prop[A]]`), read once where the block starts: `Absent` where
      * it is unset, the value where it is set, and a signal inside it followed like a signal anywhere.
      */
    extension [A](inline self: Maybe[UI.Prop[A]])
        @scala.annotation.targetName("readOptional")
        inline def read(using CanEqual[A, A]): Maybe[A] = ${ kyo.uic.internal.ReactiveMacro.strayImpl[Maybe[A]]('self) }
end ReactiveScope

/** A prop's value now, outside a [[reactive]] block: a read a handler makes when its event comes, with no region and no subscription.
  * Sequenced with kyo-direct's `.now`, as `p.loading.read.now`. Inside a block the block's own `.read` applies instead.
  */
extension [A](self: UI.Prop[A])
    def read(using notInBlock: NotGiven[ReactiveScope], frame: Frame): A < Sync = UI.Prop.current(self)(using frame)

/** An optional prop's value now, outside a [[reactive]] block: `Absent` where it is unset, else the value inside it now. */
extension [A](self: Maybe[UI.Prop[A]])
    @scala.annotation.targetName("readOptionalNow")
    def read(using notInBlock: NotGiven[ReactiveScope], canEqual: CanEqual[A, A], frame: Frame): Maybe[A] < Sync =
        UI.Prop.current(kyo.uic.internal.ReactiveMacro.optional(self))
end extension

/** A signal's value now, outside a [[reactive]] block; see the prop form. */
extension [A](self: Signal[A])
    @scala.annotation.targetName("readSignalNow")
    def read(using notInBlock: NotGiven[ReactiveScope], frame: Frame): A < Sync = self.current(using frame)

/** A block over props and signals, written as if each were a plain value and read with `.read`, called as `UI.reactive`:
  *
  * {{{
  * val off: Prop[Boolean] = UI.reactive(props(_.disabled).read || props(_.loading).read)
  * UI.reactive(if props(_.loading).read then spinner else UI.empty)
  * }}}
  *
  * Every read is taken out of the block and the reads are combined into one value, so the block runs over one consistent set of
  * them: reads of one stable path are one read, and reads that reach one signal by different paths share it. Where every read is a
  * constant the block runs once and gives a plain value (a `Prop[T]` that is a `T`), with no subscription. Where one is a signal, a
  * block that yields a value is one mapped signal and a block that yields a UI draws in one region, compared by the values read,
  * the way `Prop.render` does.
  *
  * A read in an argument whose parameter takes `T | Signal[T]` (a [[UI.Prop]], as `aria`, `cssClass` or `disabled` do) is that
  * argument's own channel and not a read of the block: `el.aria(Aria.checked -> on.read)` patches the attribute in place and the
  * block draws no region for it. A text child is such an argument too: `span(s"v=${n.read}")` patches the text node in place, and
  * over a constant it is the plain text. A block `val` is a read to whoever uses it, so naming a read changes nothing: one that only
  * reaches channel arguments is a channel too, and one the structure also uses (a condition, a value drawn, a lambda) is the
  * region's, as is any read the region already has wherever it appears, with one value per run.
  *
  * A read has to be one the block itself would evaluate, once, while it runs. A read of a value the block defines, inside a lambda,
  * a local `def`, a lazy `val` or a by-name argument (which run later, again or not at all), or through the block's scope handed
  * on, is a compile error that says what to write instead: read into a `val` in the block, and read a callback when its event comes
  * with [[UI.Live.call]].
  *
  * An extension on `UI` rather than a member of it: kyo-ui knows nothing of it, and `import kyo.*` brings it in.
  */
extension (self: UI.type)
    transparent inline def reactive[T](inline body: ReactiveScope ?=> T)(using frame: Frame): Any =
        ${ kyo.uic.internal.ReactiveMacro.expand[T]('body, 'frame) }
