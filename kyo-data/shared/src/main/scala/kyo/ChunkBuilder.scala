package kyo

import java.util.ArrayDeque
import java.util.Arrays
import scala.annotation.nowarn
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuilder
import scala.collection.mutable.Builder
import scala.collection.mutable.ReusableBuilder
import scala.reflect.ClassTag

/** A **mutable** builder for creating Chunks.
  *
  * ChunkBuilder provides an efficient way to construct Chunks by incrementally adding elements. It can be reused after calling result().
  *
  * @tparam A
  *   the type of elements in the Chunk being built
  */
sealed class ChunkBuilder[A] extends ReusableBuilder[A, Chunk.Indexed[A]] with Serializable:
    private var builder = Maybe.empty[ChunkBuilder.Buffer[A]]

    final override def addOne(elem: A): this.type =
        builder match
            case Absent =>
                val buffer = ChunkBuilder.acquireBuffer[A]()
                buffer.add(elem)
                builder = Present(buffer)
            case Present(buffer) =>
                buffer.add(elem)
        end match
        this
    end addOne

    override def addAll(elems: IterableOnce[A]): this.type =
        elems match
            case elems: Chunk[A] =>
                elems.foreach(addOne)
            case _ =>
                val it                    = elems.iterator
                @tailrec def loop(): Unit =
                    if it.hasNext then
                        addOne(it.next())
                        loop()
                loop()
        end match
        this
    end addAll

    final override def clear(): Unit = builder.foreach(_.clear())

    final override def result(): Chunk.Indexed[A] =
        val chunk = builder.fold(Chunk.Indexed.empty[A])(b => Chunk.fromNoCopy(b.toArray.asInstanceOf[Array[A]]))
        builder.foreach(ChunkBuilder.releaseBuffer)
        builder = Absent
        chunk
    end result

    final override def knownSize: Int = builder.fold(0)(_.size)

    override def toString(): String =
        s"ChunkBuilder(size = $knownSize)"
end ChunkBuilder

object ChunkBuilder:

    /** The growable array behind a builder and `Chunk.foreach`, pooled per thread.
      *
      * It takes the place of `java.util.ArrayDeque`, whose `clear()` on Scala.js only resets its indices: a pooled deque there kept every
      * element it had ever held reachable. [[clear]] here nulls each slot it used, on every platform, so a buffer waiting in the pool
      * references nothing.
      */
    final private[kyo] class Buffer[A] extends Serializable:
        private var array  = new Array[AnyRef](16)
        private var length = 0

        def size: Int = length

        def add(elem: A): Unit =
            if length == array.length then grow()
            array(length) = elem.asInstanceOf[AnyRef]
            length += 1
        end add

        def apply(index: Int): A = array(index).asInstanceOf[A]

        def toArray: Array[AnyRef] = Arrays.copyOf(array, length)

        def clear(): Unit =
            Arrays.fill(array, 0, length, null)
            length = 0

        /** The slots that hold a reference, counted over the whole array rather than the used range. */
        def referencedSlots: Int =
            @tailrec def loop(index: Int, count: Int): Int =
                if index == array.length then count
                else loop(index + 1, if isNull(array(index)) then count else count + 1)
            loop(0, 0)
        end referencedSlots

        private def grow(): Unit =
            val capacity = array.length
            if capacity == MaxCapacity then throw new OutOfMemoryError(s"ChunkBuilder cannot hold more than $MaxCapacity elements")
            array = Arrays.copyOf(array, if capacity > MaxCapacity / 2 then MaxCapacity else capacity * 2)
        end grow
    end Buffer

    private inline val MaxCapacity = Int.MaxValue - 8

    private val bufferCache =
        new ThreadLocal[ArrayDeque[Buffer[?]]]:
            override def initialValue() = new ArrayDeque[Buffer[?]]

    private[kyo] def acquireBuffer[A](): Buffer[A] =
        Maybe(bufferCache.get().poll()).getOrElse(new Buffer).asInstanceOf[Buffer[A]]

    private[kyo] def releaseBuffer(buffer: Buffer[?]): Unit =
        buffer.clear()
        discard(bufferCache.get().add(buffer))

    /** The references held by the buffers in this thread's pool, which a released buffer must not keep. */
    private[kyo] def pooledReferences(): Int =
        @tailrec def loop(it: java.util.Iterator[Buffer[?]], count: Int): Int =
            if it.hasNext then loop(it, count + it.next().referencedSlots)
            else count
        loop(bufferCache.get().iterator(), 0)
    end pooledReferences

    /** Creates a new ChunkBuilder with no size hint.
      *
      * @tparam A
      *   the type of elements in the Chunk to be built
      * @return
      *   a new ChunkBuilder instance
      */
    def init[A]: ChunkBuilder[A] =
        new ChunkBuilder[A]

    @nowarn("msg=anonymous")
    inline def initTransform[A, B](inline f: (ChunkBuilder[B], A) => Unit): (A => Unit) & ChunkBuilder[B] =
        new ChunkBuilder[B] with Function1[A, Unit]:
            def apply(elem: A): Unit = f(this, elem)

end ChunkBuilder
