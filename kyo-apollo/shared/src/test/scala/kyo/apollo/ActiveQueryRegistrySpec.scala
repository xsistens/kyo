package kyo.apollo

import kyo.*
import kyo.apollo.ActiveQueryRegistry.QueryKey
import kyo.apollo.ActiveQueryRegistry.Target
import kyo.apollo.exception.ApolloExecuteFailure
import kyo.apollo.exception.DefaultApolloException
import kyo.apollo.json.Json

/** Tests [[ActiveQueryRegistry]]: registration is owned by the enclosing `Scope`
  * (release removes exactly that entry), registration order is the refetch
  * order, an entry takes part only while its target is present, the targets of
  * one query are refetched once, and concurrent registrations never lose an entry
  * or share an id.
  */
class ActiveQueryRegistrySpec extends kyo.test.Test[Any]:

    private def key(name: String, id: String = "1"): QueryKey =
        QueryKey(name, s"query $name", Json.JObj(Map("id" -> Json.JStr(id))))

    private def target(name: String, fetch: Unit < (Async & Abort[ApolloExecuteFailure]) = ()): Target =
        Target(key(name), fetch, (), (), followsCache = true)

    "a refetch's row is the execute failures, not the module-wide ApolloException" in {
        typeCheck(
            "ActiveQueryRegistry.Target(ActiveQueryRegistry.QueryKey(\"a\", \"\", kyo.apollo.json.Json.JNull), (): Unit < (Async & Abort[kyo.apollo.exception.ApolloExecuteFailure]), (), (), true)"
        )
        typeCheckFailure(
            "ActiveQueryRegistry.Target(ActiveQueryRegistry.QueryKey(\"a\", \"\", kyo.apollo.json.Json.JNull), (): Unit < (Async & Abort[kyo.apollo.exception.ApolloException]), (), (), true)"
        )(
            "Abort[kyo.apollo.exception.ApolloException]"
        )
    }

    "two registrations released in either order leave the other intact" in {
        for
            registry <- ActiveQueryRegistry.init
            _        <- Scope.run {
                for
                    _ <- registry.register(Present(target("a")))
                    _ <- Scope.run(registry.register(Present(target("b"))))
                    n <- registry.live.map(_.size)
                    _ = assert(n == 1)
                    _ <- Scope.run {
                        for
                            _ <- registry.register(Present(target("c")))
                            _ <- Scope.run(registry.register(Present(target("d"))))
                            e <- registry.live
                        yield assert(e.map(_.key.name) == Chunk("a", "c"))
                    }
                    n <- registry.live.map(_.size)
                yield assert(n == 1)
            }
            n <- registry.entries.map(_.size)
        yield assert(n == 0)
    }

    "refetchPlan filters by operation name and keeps registration order" in {
        Scope.run {
            for
                log      <- AtomicRef.init(Chunk.empty[String])
                registry <- ActiveQueryRegistry.init
                note = (s: String) => log.updateAndGet(_ :+ s).unit
                _     <- registry.register(Present(target("users", note("users"))))
                _     <- registry.register(Present(target("posts", note("posts"))))
                all   <- registry.refetchPlan(Set.empty)
                users <- registry.refetchPlan(Set("users"))
                none  <- registry.refetchPlan(Set("comments"))
                _     <- Kyo.foreachDiscard(all)(identity)
                order <- log.get
            yield
                assert(all.size == 2)
                assert(users.size == 1)
                assert(none.isEmpty)
                assert(order == Chunk("users", "posts"))
        }
    }

    "the targets of one query are refetched once, and every one of them sees it begin and end" in {
        Scope.run {
            for
                log      <- AtomicRef.init(Chunk.empty[String])
                registry <- ActiveQueryRegistry.init
                note = (s: String) => log.updateAndGet(_ :+ s).unit
                one  = (tag: String) =>
                    Target(key("users"), note(s"fetch $tag"), note(s"begin $tag"), note(s"end $tag"), followsCache = true)
                _     <- registry.register(Present(one("a")))
                _     <- registry.register(Present(one("b")))
                _     <- registry.register(Present(target("users").copy(key = key("users", "2"), fetch = note("fetch other"))))
                plan  <- registry.refetchPlan(Set("users"))
                _     <- Kyo.foreachDiscard(plan)(identity)
                order <- log.get
            yield
                assert(plan.size == 2)
                assert(order == Chunk("begin a", "begin b", "fetch a", "end a", "end b", "fetch other"))
        }
    }

    "a target that does not follow the cache runs its own fetch" in {
        Scope.run {
            for
                log      <- AtomicRef.init(Chunk.empty[String])
                registry <- ActiveQueryRegistry.init
                note = (s: String) => log.updateAndGet(_ :+ s).unit
                _     <- registry.register(Present(target("users", note("watcher"))))
                _     <- registry.register(Present(target("users", note("lazy")).copy(followsCache = false)))
                plan  <- registry.refetchPlan(Set.empty)
                _     <- Kyo.foreachDiscard(plan)(identity)
                order <- log.get
            yield assert(order == Chunk("lazy"))
        }
    }

    "a failed fetch still ends every target of its query" in {
        Scope.run {
            for
                ended    <- AtomicInt.init(0)
                registry <- ActiveQueryRegistry.init
                failing = Target(
                    key("users"),
                    Abort.fail(DefaultApolloException("down")),
                    (),
                    ended.incrementAndGet.unit,
                    followsCache = true
                )
                _      <- registry.register(Present(failing))
                _      <- registry.register(Present(failing))
                plan   <- registry.refetchPlan(Set.empty)
                result <- Abort.run[ApolloExecuteFailure](Kyo.foreachDiscard(plan)(identity))
                n      <- ended.get
            yield
                assert(result.isFailure)
                assert(n == 2)
        }
    }

    "an entry takes part only while its target is present" in {
        Scope.run {
            for
                paused   <- AtomicBoolean.init(true)
                registry <- ActiveQueryRegistry.init
                _        <- registry.register(paused.get.map(p => if p then Absent else Present(target("users"))))
                before   <- registry.refetchPlan(Set.empty)
                _        <- paused.set(false)
                after    <- registry.refetchPlan(Set.empty)
            yield
                assert(before.isEmpty)
                assert(after.size == 1)
        }
    }

    "reset hooks are owned by their Scope and share the id sequence" in {
        for
            registry <- ActiveQueryRegistry.init
            _        <- Scope.run {
                for
                    _     <- registry.registerResetHook(())
                    _     <- registry.register(Present(target("a")))
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
                forks    <- Kyo.foreach(1 to fibers) { i =>
                    Fiber.init(
                        latch.await.andThen(Loop.repeat(perFiber)(registry.register(Present(target(s"q$i")))))
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
