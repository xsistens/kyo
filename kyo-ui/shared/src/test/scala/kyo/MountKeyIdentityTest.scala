package kyo

import kyo.internal.ReactiveUI.MountRegistry

/** Which keys the unstable-key hint may judge by their CHANGES.
  *
  * The hint flags a key rebuilt per render, where "changed three passes running" really means
  * "was never the same value twice". That reading only holds for keys whose equality is by
  * identity. A key with value identity that changes means the caller asked for a different
  * instance — which is exactly what a route outlet does on every navigation, and flagging it
  * turns the diagnostic into noise every SPA learns to scroll past.
  */
class MountKeyIdentityTest extends kyo.test.Test[Any]:

    private enum Route derives CanEqual:
        case Queue
        case Track(id: String)

    final private class Opaque

    "primitives and String keep their exemption" in {
        assert(MountRegistry.valueIdentityKey("home"))
        assert(MountRegistry.valueIdentityKey(3))
        assert(MountRegistry.valueIdentityKey(true))
    }

    "a case object is a value, and a routed page keyed by one is not churn" in {
        // `Shell.at(route)` keys by the Route ADT value; navigating IS the key changing.
        assert(MountRegistry.valueIdentityKey(Route.Queue))
    }

    "a case class over stable values is a value too" in {
        assert(MountRegistry.valueIdentityKey(Route.Track("t1")))
        assert(Route.Track("t1") == Route.Track("t1"), "and equal ones really are equal")
    }

    "a Class is one instance per class, which is as stable as a name" in {
        // `Shell.live[P]` keys by `ct.runtimeClass`.
        assert(MountRegistry.valueIdentityKey(classOf[Route]))
    }

    "a bare object identity is still judged, which is the whole point of the hint" in {
        assert(!MountRegistry.valueIdentityKey(new Opaque))
    }

    "a function in the key is still judged" in {
        val f: Int => Int = _ + 1
        assert(!MountRegistry.valueIdentityKey(f))
    }

    "a tuple is only as stable as what is in it" in {
        // The idiomatic composite key: a component object tupled with an id. The component object
        // is not a case object, so this is judged — and that costs nothing, because a tuple of
        // stable parts compares EQUAL and never registers as a change.
        assert(MountRegistry.valueIdentityKey((Route.Queue, "a")), "stable parts, a stable tuple")
        assert(!MountRegistry.valueIdentityKey((Route.Queue, new Opaque)), "one unstable part is enough")
    }

end MountKeyIdentityTest
