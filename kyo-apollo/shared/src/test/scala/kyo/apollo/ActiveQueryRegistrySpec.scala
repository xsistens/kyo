package kyo.apollo

import kyo.*
import kyo.apollo.exception.ApolloException

/** Tests [[ActiveQueryRegistry]]: registration is owned by the enclosing `Scope`
  * (release removes exactly that entry), registration order is the refetch
  * order, and concurrent registrations never lose an entry or share an id.
  */
class ActiveQueryRegistrySpec extends kyo.test.Test[Any]:

    private def refetch: Unit < (Async & Abort[ApolloException]) = ()

    "two registrations released in either order leave the other intact" in {
        for
            registry <- ActiveQueryRegistry.init
            _ <- Scope.run {
                for
                    _ <- registry.register("a", refetch)
                    _ <- Scope.run(registry.register("b", refetch))
                    n <- registry.selected(Set.empty).map(_.size)
                    _ = assert(n == 1)
                    _ <- Scope.run {
                        for
                            _ <- registry.register("c", refetch)
                            _ <- Scope.run(registry.register("d", refetch))
                            e <- registry.entries
                        yield assert(e.map(_.name) == Chunk("a", "c"))
                    }
                    n <- registry.selected(Set.empty).map(_.size)
                yield assert(n == 1)
            }
            n <- registry.selected(Set.empty).map(_.size)
        yield assert(n == 0)
    }

    "selected filters by operation name and keeps registration order" in {
        Scope.run {
            for
                registry <- ActiveQueryRegistry.init
                _        <- registry.register("users", refetch)
                _        <- registry.register("posts", refetch)
                _        <- registry.register("users", refetch)
                all      <- registry.selected(Set.empty)
                users    <- registry.selected(Set("users"))
                none     <- registry.selected(Set("comments"))
                entries  <- registry.entries
            yield
                assert(all.size == 3)
                assert(users.size == 2)
                assert(none.isEmpty)
                assert(entries.map(_.name) == Chunk("users", "posts", "users"))
                assert(entries.map(_.id) == Chunk(1L, 2L, 3L))
        }
    }

    "reset hooks are owned by their Scope and share the id sequence" in {
        for
            registry <- ActiveQueryRegistry.init
            _ <- Scope.run {
                for
                    _     <- registry.registerResetHook(())
                    _     <- registry.register("a", refetch)
                    hooks <- registry.resetHookEffects
                    e     <- registry.entries
                yield
                    assert(hooks.size == 1)
                    assert(e.map(_.id) == Chunk(2L))
            }
            hooks <- registry.resetHookEffects
            e     <- registry.entries
        yield
            assert(hooks.isEmpty)
            assert(e.isEmpty)
    }

    "concurrent registrations never share an id" in {
        val fibers   = 64
        val perFiber = 8192
        Scope.run {
            for
                registry <- ActiveQueryRegistry.init
                latch    <- Latch.init(1)
                forks <- Kyo.foreach(1 to fibers) { i =>
                    Fiber.init(
                        latch.await.andThen(Loop.repeat(perFiber)(registry.register(s"q$i", refetch)))
                    )
                }
                _       <- latch.release
                _       <- Kyo.foreach(forks)(_.get)
                entries <- registry.entries
            yield
                assert(entries.size == fibers * perFiber)
                assert(entries.map(_.id).toSet.size == fibers * perFiber)
        }
    }
end ActiveQueryRegistrySpec
