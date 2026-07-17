package kyo

/** Tests for the non-consuming `Abort.tap`/`Abort.tapError` pair: the side effect fires on the matching error,
  * the error is re-raised unchanged (still observable by an enclosing handler), and the success path never
  * evaluates the observer.
  */
class AbortTapTest extends kyo.test.Test[Any]:

    private val boom = new RuntimeException("boom")

    "Abort.tap" - {
        "fires on failure and re-raises the same failure" in {
            for
                seen   <- AtomicRef.init(Maybe.empty[Throwable])
                result <- Abort.run[Throwable](Abort.tap[Throwable](e => seen.set(Present(e)))(Abort.fail(boom)))
                s      <- seen.get
            yield
                assert(result == Result.Failure(boom))
                assert(s == Present(boom))
        }

        "does not fire on success" in {
            for
                seen   <- AtomicRef.init(false)
                result <- Abort.run[Throwable](Abort.tap[Throwable](_ => seen.set(true))(42))
                s      <- seen.get
            yield
                assert(result == Result.Success(42))
                assert(!s)
        }

        "an enclosing recover still sees the tapped failure (tap does not consume)" in {
            for
                order <- AtomicRef.init(List.empty[String])
                result <- Abort.recover[Throwable](e => order.getAndUpdate("recover" :: _).map(_ => -1)) {
                    Abort.tap[Throwable](_ => order.getAndUpdate("tap" :: _).unit)(Abort.fail(boom): Int < Abort[Throwable])
                }
                o <- order.get
            yield
                assert(result == -1)
                assert(o.reverse == List("tap", "recover"))
        }
    }

    "Abort.tapError" - {
        "fires on a panic too, and re-raises it" in {
            for
                seen <- AtomicRef.init(Maybe.empty[Result.Error[Throwable]])
                result <- Abort.run[Throwable](
                    Abort.tapError[Throwable](err => seen.set(Present(err)))(
                        Abort.panic(boom): Int < Abort[Throwable]
                    )
                )
                s <- seen.get
            yield
                assert(result == Result.Panic(boom))
                assert(s == Present(Result.Panic(boom)))
        }

        "fires on a typed failure with the full Error" in {
            for
                seen <- AtomicRef.init(Maybe.empty[Result.Error[String]])
                result <- Abort.run[String](
                    Abort.tapError[String](err => seen.set(Present(err)))(
                        Abort.fail("domain"): Int < Abort[String]
                    )
                )
                s <- seen.get
            yield
                assert(result == Result.Failure("domain"))
                assert(s == Present(Result.Failure("domain")))
        }
    }
end AbortTapTest
