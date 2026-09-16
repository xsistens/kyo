package kyo

/** The callback observation path (`Signal.unsafeObserveProjected`), which delivers inside the writer's own
  * `set` instead of waking a fiber.
  *
  * Two properties carry the whole design and neither is covered by the existing Signal suites, because both
  * only exist on this path:
  *
  *   - **Release.** The registration sits on a MASKED promise, which deliberately outlives its subscribers
  *     (see IOPromise.onCompleteCancellable's note). Nothing interrupts it, so a caller that forgets to
  *     release leaks the callback and everything it captures — the exact leak the waiter fix closed for
  *     fibers. Group B pins that the last release takes the shared registration with it.
  *   - **Losslessness.** The fiber path gets exactness from the version-validated register/validate
  *     protocol in `nextSince`. The callback path has to reproduce it; group C pins that a write is never
  *     stranded, including a write issued from inside a delivery.
  *
  * Deliberately synchronous throughout: this path IS synchronous, so nothing here needs threads or timing.
  */
class SignalCallbackObserveTest extends kyo.test.Test[Any]:

    import AllowUnsafe.embrace.danger

    private def ref[A](initial: A)(using CanEqual[A, A]): Signal.SignalRef[A] =
        Signal.SignalRef.Unsafe.init(initial).safe

    private def observeFrom[A, B](sig: Signal[A], baseline: Maybe[B])(proj: A => B)(
        using
        CanEqual[B, B],
        Frame
    ): (scala.collection.mutable.ListBuffer[B], () => Unit) =
        val seen    = scala.collection.mutable.ListBuffer.empty[B]
        val release = sig.unsafeObserveProjected(proj, baseline, b => discard(seen += b))
        (seen, release.getOrElse(sys.error("expected a fast path for this signal")))
    end observeFrom

    private def observe[A, B](sig: Signal[A])(proj: A => B)(
        using
        CanEqual[B, B],
        Frame
    ): (scala.collection.mutable.ListBuffer[B], () => Unit) =
        observeFrom(sig, Absent)(proj)

    "A — delivery" - {

        "A1 delivers the current value on subscribe" in {
            val r         = ref(1)
            val (seen, _) = observe(r)(identity)
            assert(seen.toList == List(1))
        }

        "A2 skips the initial delivery when it equals the baseline" in {
            val r         = ref(1)
            val (seen, _) = observeFrom(r, Present(1))(identity)
            // The renderer passes the value it already painted; re-delivering it would repaint fresh state.
            assert(seen.isEmpty)
        }

        "A3 delivers inside the writer's own set, with no scheduler hop" in {
            val r         = ref(1)
            val (seen, _) = observe(r)(identity)
            r.unsafe.set(2)
            // The whole point of the path: by the time `set` returns, the sink has already run. On the
            // fiber path this list would still be List(1) here.
            assert(seen.toList == List(1, 2))
        }

        "A4 deduplicates on the IMAGE, not on the source value" in {
            val r         = ref(0)
            val (seen, _) = observe(r)(_ == 7)
            r.unsafe.set(1)
            r.unsafe.set(2)
            r.unsafe.set(7)
            r.unsafe.set(8)
            // Three source changes leave the image false; only crossing 7 moves it. Without image-level
            // dedup a thousand rows would each repaint on every selection change.
            assert(seen.toList == List(false, true, false))
        }

        "A5 a map chain keeps the fast path" in {
            val r         = ref(0)
            val (seen, _) = observe(r.map(_ == 3))(identity)
            r.unsafe.set(3)
            // `map` composes the projection down to the leaf; if it fell back to the trait default the
            // helper's getOrElse would have failed above.
            assert(seen.toList == List(false, true))
        }

        "A6 a constant delivers once and never again" in {
            val (seen, release) = observe(Signal.initConst(5))(identity)
            release()
            assert(seen.toList == List(5))
        }

        "A7 a combinator-derived signal has no fast path" in {
            val a = ref(0)
            val b = ref(0)
            // Not rooted in a single SignalRef: the caller must keep its fiber path for these.
            assert(a.combineLatest(b).unsafeObserveProjected[(Int, Int)](identity, Absent, _ => ()).isEmpty)
        }

        "A8 one failing subscriber does not silence the others" in {
            val r    = ref(0)
            val seen = scala.collection.mutable.ListBuffer.empty[Int]
            discard(r.unsafeObserveProjected[Int](identity, Absent, _ => throw new RuntimeException("boom")))
            discard(r.unsafeObserveProjected[Int](identity, Absent, v => discard(seen += v)))
            r.unsafe.set(1)
            // On the fiber path every observer had its own fiber, so a throwing sink could not take the
            // others down. The shared dispatch walk has to isolate per subscriber to keep that.
            assert(seen.toList == List(0, 1))
        }
    }

    "B — release" - {

        "B1 a released subscriber stops receiving" in {
            val r               = ref(0)
            val (seen, release) = observe(r)(identity)
            r.unsafe.set(1)
            release()
            r.unsafe.set(2)
            assert(seen.toList == List(0, 1))
        }

        "B2 the last release leaves no waiter on the signal" in {
            val r = ref(0)
            assert(r.unsafe.waiters() == 0)
            val (_, r1) = observe(r)(identity)
            val (_, r2) = observe(r)(identity)
            assert(r.unsafe.waiters() == 1) // one shared registration for the whole fan-out
            r1()
            assert(r.unsafe.waiters() == 1) // still one subscriber left
            r2()
            // The masked next-promise outlives its subscribers and nothing interrupts it, so this is the
            // only thing standing between a released binding and a retained callback.
            assert(r.unsafe.waiters() == 0)
        }

        "B3 a thousand subscribers share ONE registration" in {
            val r        = ref(0)
            val releases = (1 to 1000).map(i => observe(r)(_ == i)._2)
            // The point of the shared dispatcher: a write allocates one waiter and walks an array, instead
            // of a thousand registrations CASing onto the same chain.
            assert(r.unsafe.waiters() == 1)
            releases.foreach(_())
            assert(r.unsafe.waiters() == 0)
        }

        "B4 releasing twice is harmless" in {
            val r               = ref(0)
            val (seen, release) = observe(r)(identity)
            release()
            release()
            r.unsafe.set(1)
            assert(seen.toList == List(0))
            assert(r.unsafe.waiters() == 0)
        }

        "B5 re-subscribing after the last release arms again" in {
            val r       = ref(0)
            val (_, r1) = observe(r)(identity)
            r1()
            val (seen, _) = observe(r)(identity)
            r.unsafe.set(1)
            // A stale `armed` slot left behind by the release would leave this subscriber deaf.
            assert(seen.toList == List(0, 1))
            assert(r.unsafe.waiters() == 1)
        }
    }

    "C — losslessness" - {

        "C1 every distinct value in a burst is delivered" in {
            val r         = ref(0)
            val (seen, _) = observe(r)(identity)
            for i <- 1 to 100 do r.unsafe.set(i)
            assert(seen.toList == (0 to 100).toList)
        }

        "C2 a write issued from inside a delivery is not stranded" in {
            val r    = ref(0)
            val seen = scala.collection.mutable.ListBuffer.empty[Int]
            discard(r.unsafeObserveProjected[Int](
                identity,
                Absent,
                v =>
                    discard(seen += v)
                    // Re-entering the writer from inside the flush: the walk runs over an immutable
                    // snapshot, and the nested change must still reach this same subscriber.
                    if v == 1 then r.unsafe.set(2)
            ))
            r.unsafe.set(1)
            assert(seen.toList == List(0, 1, 2))
        }

        "C3 a subscriber added while another is being notified still sees later writes" in {
            val r    = ref(0)
            var late = Maybe.empty[scala.collection.mutable.ListBuffer[Int]]
            discard(r.unsafeObserveProjected[Int](
                identity,
                Absent,
                v =>
                    if v == 1 && late.isEmpty then
                        val (s, _) = observe(r)(identity)
                        late = Present(s)
            ))
            r.unsafe.set(1)
            r.unsafe.set(2)
            assert(late.map(_.toList) == Present(List(1, 2)))
        }

        "C4 an unchanged write delivers nothing" in {
            val r         = ref(0)
            val (seen, _) = observe(r)(identity)
            r.unsafe.set(0)
            // The ref itself gates on inequality (Unsafe.getAndSet), so no version bump, no dispatch.
            assert(seen.toList == List(0))
        }
    }

end SignalCallbackObserveTest
