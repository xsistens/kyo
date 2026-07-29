package kyo.apollo.runtime

import kyo.*
import kyo.apollo.StreamProbe
import kyo.apollo.exception.ApolloNetworkException
import kyo.apollo.network.ApolloResponse
import kyo.apollo.network.Uuid
import scala.concurrent.Future

/** Tests the [[ApolloCall]] async contract: `execute` is the first emission of
  * the stream-first `stream`, transport failures arrive as *values* inside
  * `ApolloResponse.error`, and only an empty or raising stream fails the
  * effect (surfaced here by running it through `Abort.run`).
  */
class ApolloCallSpec extends kyo.test.Test[Any]:

    given CanEqual[Any, Any] = CanEqual.derived

    /** A test double: an [[ApolloCall]] whose `stream` is whatever we hand it. */
    private def callOf[D](s: ResponseStream[D]): ApolloCall[D] =
        new ApolloCall[D]:
            def stream(using Frame, Tag[Emit[Chunk[ApolloResponse[D]]]]): ResponseStream[D] = s

    private def okResponse(uuid: Uuid): ApolloResponse[Int] =
        ApolloResponse(requestUuid = uuid, data = Present(1))

    "ApolloCall" - {

        "execute completes with the single emission of stream" in {
            val uuid = Uuid.random()
            callOf(Stream.init(Seq(okResponse(uuid)))).execute.map { response =>
                assert(response.requestUuid == uuid)
                assert(response.data == Present(1))
                assert(!response.hasErrors)
            }
        }

        "execute takes the first emission when stream is multi-emission" in {
            val first  = okResponse(Uuid.random())
            val second = okResponse(Uuid.random())
            callOf(Stream.init(Seq(first, second))).execute.map { response =>
                assert(response.requestUuid == first.requestUuid)
            }
        }

        "stream exposes every emission in order" in {
            val a = okResponse(Uuid.random())
            val b = okResponse(Uuid.random())
            StreamProbe.collect(callOf(Stream.init(Seq(a, b))).stream).map { seen =>
                assert(seen.map(_.requestUuid) == List(a.requestUuid, b.requestUuid))
            }
        }

        "a transport failure arrives as a value, not a failed effect" in {
            val offline = new ApolloNetworkException("offline")
            val failed  = ApolloResponse.fromException[Int](Uuid.random(), offline)
            callOf(Stream.init(Seq(failed))).execute.map { response =>
                assert(response.error == Present(offline))
                assert(response.data == Absent)
                assert(response.hasErrors)
            }
        }

        "execute on an empty stream fails with NoSuchElementException" in {
            Abort.run[Throwable](callOf(Stream.init(Seq.empty[ApolloResponse[Int]])).execute).map {
                case Result.Failure(e) => assert(e.isInstanceOf[NoSuchElementException], s"got $e")
                case Result.Panic(e)   => assert(e.isInstanceOf[NoSuchElementException], s"got $e")
                case other             => fail(s"expected NoSuchElementException, got $other")
            }
        }

        "a raising stream (wiring error) fails the execute effect" in {
            val boom = new IllegalStateException("chain exhausted")
            val raising: ResponseStream[Int] =
                Stream.init(Async.fromFuture(Future.failed[ApolloResponse[Int]](boom)).map(Seq(_)))
            Abort.run[Throwable](callOf(raising).execute).map {
                case Result.Failure(e) => assert(e == boom)
                case Result.Panic(e)   => assert(e == boom)
                case other             => fail(s"expected boom, got $other")
            }
        }
    }
end ApolloCallSpec
