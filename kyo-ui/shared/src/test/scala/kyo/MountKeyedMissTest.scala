package kyo

import kyo.internal.MountDispatch

/** The streak behind the keyed-rebuild hint.
  *
  * A keyed mount claims its instance from the registry of the nearest enclosing region, so its
  * continuity ends with that region. Under an intervening region — one re-subscribed whenever
  * something further out re-renders — the registry is new and empty every pass, the claim misses,
  * and the instance is rebuilt exactly as a keyless one would be. Nothing else reports it: the
  * keyless counter only counts keyless mounts, and the unstable-key streak lives in the registry
  * that just went away, which is why this count is held for the session instead.
  *
  * What is pinned here is the streak that decides whether anything is logged. The message text is
  * not: it goes to `Log.warn` and this suite has no log sink.
  */
class MountKeyedMissTest extends kyo.test.Test[Any]:

    private val path  = Seq("2", "0", "2", "0")
    private val other = Seq("2", "0", "5")
    private val key   = ("DataTable", "tracks")

    "the first miss at a path is an ordinary first mount" in {
        for
            d <- MountDispatch.init
            n <- d.noteKeyedMiss(path, key)
        yield assert(n == 1)
    }

    "the same key missing again at the same path is the fault, and it accumulates" in {
        for
            d      <- MountDispatch.init
            _      <- d.noteKeyedMiss(path, key)
            second <- d.noteKeyedMiss(path, key)
            third  <- d.noteKeyedMiss(path, key)
        yield
            assert(second == 2)
            assert(third == 3, "a mount rebuilt on every pass climbs to the threshold")
    }

    "a key that CHANGED is a deliberate re-creation, and starts over" in {
        // `.keyed(EraPanel -> era)` on an era switch is the intended teardown, not a lost instance,
        // so it must not walk the count towards a warning.
        for
            d       <- MountDispatch.init
            _       <- d.noteKeyedMiss(path, key)
            _       <- d.noteKeyedMiss(path, key)
            changed <- d.noteKeyedMiss(path, ("DataTable", "recommendations"))
        yield assert(changed == 1)
    }

    "two paths count independently" in {
        for
            d  <- MountDispatch.init
            _  <- d.noteKeyedMiss(path, key)
            _  <- d.noteKeyedMiss(path, key)
            b1 <- d.noteKeyedMiss(other, key)
            a3 <- d.noteKeyedMiss(path, key)
        yield
            assert(b1 == 1, "the same key at another path is another mount")
            assert(a3 == 3, "and the first path kept its own streak")
    }

end MountKeyedMissTest
