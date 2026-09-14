package kyo.apollo.network

import kyo.*

/** The consumer side of a producer-to-consumer bridge over a `Channel[Maybe[A]]`:
  * the producer puts every element as `Present`, then one `Absent` end marker. Both
  * the WebSocket engines (incoming frames) and the JVM/Native HTTP engine (a
  * streamed response body) hand their channel to the transport this way.
  *
  * It avoids two traps:
  *
  *   - `Channel.close` hands the channel's backlog to the *closer*, so a producer
  *     that puts its last elements and closes in one burst would drop them for a
  *     consumer that has not drained yet. With the marker, every element put before
  *     it reaches the stream, which then ends regularly. A `Closed` channel ends the
  *     stream too, so a consumer never hangs on a channel torn down underneath it.
  *   - `Stream.repeatPresent` regroups into chunks of up to 4096 elements and holds
  *     an element back until more follow or the producer ends. A `connection_ack`
  *     frame or the first part of a `@defer` body must reach the consumer the moment
  *     it is taken, so this stream emits one element per emit.
  */
private[apollo] object MarkedChannel:

    /** The elements of `channel` up to its `Absent` end marker, each emitted as soon
      * as it is taken.
      */
    def untilEnd[A](channel: Channel[Maybe[A]])(using Tag[Emit[Chunk[A]]], Frame): Stream[A, Async] =
        Stream[A, Async]:
            Loop.foreach:
                Abort.run[Closed](channel.take).map {
                    case Result.Success(Present(element)) => Emit.valueWith(Chunk(element))(Loop.continue)
                    case _                                => Loop.done
                }
end MarkedChannel
