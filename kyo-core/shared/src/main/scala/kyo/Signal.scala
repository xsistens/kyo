package kyo

import kyo.scheduler.IOPromise
import scala.annotation.implicitNotFound
import scala.annotation.nowarn
import scala.annotation.tailrec
import scala.compiletime.uninitialized
import scala.util.control.NonFatal

/** A reactive value that can change over time, providing both synchronous access to its current state and asynchronous notification of
  * changes.
  *
  * Signal provides two fundamental operations:
  *
  *   - `current`: synchronous access to the current value
  *   - `next`: asynchronous notification of the next change
  *
  * Changes can be observed through streaming operations:
  *
  *   - `streamCurrent`: emits the current value continuously
  *   - `streamChanges`: emits only when values change
  *
  * Note that `streamChanges` may skip intermediate values if changes occur faster than they can be processed. This makes it suitable for UI
  * updates or other scenarios where processing only the latest value is acceptable, but not for cases where capturing every single change
  * is critical.
  *
  * A change means a DIFFERENT value. Writing the value a signal already holds notifies nobody: `next` stays parked, `streamChanges` emits
  * nothing, and `observe` does not re-run. That is why `A` must have a `CanEqual[A, A]`, and it is why there is no `distinct` operator to
  * reach for: deduplication is the semantics rather than a combinator. A parked observation re-reads `current` on its repair timer, but a
  * timer that finds the value unchanged simply waits again, so it never turns into a spurious notification either.
  *
  * There is likewise no `filter`, and it is not an omission. A signal must always have a current value, and a filtered signal has none
  * before the first value that passes, so the type cannot be honoured. Filtering belongs to the change sequence rather than to the value:
  * `signal.streamChanges.filter(...)` is a `Stream`, which has no such obligation. `map`, `zip`, `combineLatest`, `combineLatestAll` and
  * `switchMap` are the value-level combinators.
  *
  * The companion object provides these creation methods:
  *
  *   - `Signal.initRef[A]`: creates a mutable `SignalRef[A]` initialized with a starting value
  *   - `Signal.initConst[A]`: creates an immutable `Signal[A]` that always returns the same value
  *   - `Signal.initRaw[A]`: (low-level API) creates a custom `Signal[A]` by directly implementing its fundamental operations, primarily
  *     intended for implementing signal combinators and custom signal types
  *
  * @tparam A
  *   The type of value contained in the signal. Must have an instance of `CanEqual[A, A]`
  */
sealed abstract class Signal[A](using CanEqual[A, A]) extends Serializable:
    self =>

    /** Retrieves the current value of the signal.
      *
      * This method provides synchronous access to the signal's current state. It's useful when you need immediate access to the value
      * without waiting for changes.
      *
      * @return
      *   The current value of type A
      */
    final def current(using Frame): A < Sync = currentWith(identity)

    /** Retrieves and transforms the current value of the signal.
      *
      * This method allows for synchronous access to the signal's current state while simultaneously applying a transformation function.
      * This is more efficient than calling `current` followed by a separate transformation as it combines both operations.
      *
      * @param f
      *   The transformation function to apply to the current value
      * @return
      *   The transformed value wrapped in combined effects S & Sync
      */
    def currentWith[B, S](f: A => B < S)(using Frame): B < (S & Sync)

    /** Waits for and returns the next value change in the signal.
      *
      * This method provides asynchronous notification of the next value change. It will wait until the signal's value changes before
      * completing, so on a signal that can never change (see [[Signal.initConst]]) it never completes.
      *
      * @return
      *   The next value of type A wrapped in an Async effect
      */
    final def next(using Frame): A < Async = nextWith(identity)

    /** Waits for the next value change and transforms it.
      *
      * This method combines waiting for the next value change with a transformation function. It's more efficient than calling `next`
      * followed by a separate transformation as it combines both operations.
      *
      * @param f
      *   The transformation function to apply to the next value
      * @return
      *   The transformed value wrapped in combined effects S & Async
      */
    def nextWith[B, S](f: A => B < S)(using Frame): B < (S & Async)

    /** Runs `f` for the current value and for every subsequent change, each inside a fresh [[Scope]] that closes when the next value arrives.
      *
      * This is a live subscription: `f` runs once for the current value, then again on every change, and the computation runs forever (fork it
      * and interrupt to stop). For each value, `f(value)` runs inside a new `Scope`: `f` sets the value up (renders, forks scoped children via
      * `Fiber.init`) and returns, then `observe` holds that per-value `Scope` open until the next change, at which point it closes the prior
      * value's `Scope` (interrupting whatever `f` forked, cascading to their descendants) before opening a fresh one for the new value. On
      * interrupt, the current value's `Scope` closes too. This is switch-with-resources: the inner lifetime is bounded by the outer value,
      * structurally, with no manual cleanup. A value that forks nothing just opens and closes an empty scope. Because each value's `Scope` is
      * closed before the next `f` runs, at most one value's children are alive at a time and no waiter or fiber accumulates across changes.
      *
      * It is designed never to permanently miss the latest value, even under a write that races the observation, and never to tear a
      * still-current value's `Scope` down on an idle timer. Delivery comes in two tiers. A signal whose value comes from [[SignalRef]]s
      * alone (a `SignalRef`, and any `map`, `zip`, `combineLatest`, `switchMap`, `zipAll`, `combineLatestAll` or [[Signal.withLocal]]
      * built on them, or a constant) observes exactly: a version-validated register/validate/await protocol over every ref the value was
      * read from makes every change wake the observer immediately, with no repair timer armed at all (see the
      * `SignalRef.observeProjected` override). A signal defined with `initRaw`, and a combinator over one, uses the repairing loop: it
      * reads `current`, runs `f`, then re-arms a
      * `nextWith`/`Async.sleep(repairInterval)` race that holds the value's `Scope` open until the next change. A write that lands in the
      * window between reading `current` and registering `nextWith` is missed by the immediate wakeup and reconciled when the repair timer
      * next fires and re-reads `current` (the hold re-waits on a still-current value, so a repair timer never closes its `Scope`). So the
      * final value is always delivered: immediately in the common case, and within `repairInterval` in the worst case when a write races
      * that window on such a signal. Correctness never depends on `repairInterval` ; only the worst-case reconciliation latency does. This
      * variant uses [[Signal.defaultRepairInterval]].
      *
      * @param f
      *   The per-value setup, run inside a fresh `Scope`; it may fork scoped children (`Fiber.init`) and should return once setup is done,
      *   leaving `observe` to hold the `Scope` until the next value
      * @see
      *   [[switchMap]] for the resource-free value-level switch
      */
    final def observe[S](f: A => Unit < (S & Async & Scope))(using Frame): Unit < (S & Async) =
        observe(Signal.defaultRepairInterval)(f)

    /** Like [[observe]] but with an explicit reconciliation interval.
      *
      * On a signal that observes exactly (see [[observe]]) the interval is not used. Otherwise the loop is the repairing form: it tracks
      * the last observed value and, while the current value is unchanged, re-arms a `nextWith`/`Async.sleep(repairInterval)` race so a
      * missed wakeup is reconciled within `repairInterval` WITHOUT tearing the still-current value's `Scope` down. The hold loops
      * until `current` actually differs, so a repair timer firing on a still-current value re-waits and keeps the per-value `Scope` open.
      *
      * @param repairInterval
      *   How often a parked observation re-reads `current` to reconcile a missed wakeup on the repair path; ignored by exact observers,
      *   which never miss a wakeup. `Duration.Infinity` arms no timer: the repairing loop then waits on `nextWith` alone and a missed
      *   wakeup stays missed until the next change. Under it, `zip` and `zipAll` keep their `nextWith` semantics and wait for every input
      *   to change, so their observation takes the repairing loop
      * @param f
      *   The per-value setup, run inside a fresh `Scope`
      */
    final def observe[S](repairInterval: Duration)(f: A => Unit < (S & Async & Scope))(using Frame): Unit < (S & Async) =
        observe(Absent, repairInterval)(f)

    /** Like [[observe]] but seeded: `f` is skipped while the current value still equals `baseline`.
      *
      * With `Absent` this is exactly [[observe]]. With `Present(v)` the loop treats `v` as the last observed value: the initial emission is
      * skipped when the current value still equals it, and the first differing value is delivered as usual. It serves a caller that already
      * processed a value (e.g. painted it) and only wants what changed since. The baseline is the value the caller processed, not the value
      * current at subscription: a write landing between the two is then delivered rather than taken for already seen.
      *
      * @param baseline
      *   The value already processed by the caller: `f` is not run while `current` still equals it
      * @param repairInterval
      *   How often a parked observation re-reads `current` to reconcile a missed wakeup on the repair path
      * @param f
      *   The per-value setup, run inside a fresh `Scope`
      */
    final def observe[S](baseline: Maybe[A], repairInterval: Duration)(f: A => Unit < (S & Async & Scope))(using
        Frame
    ): Unit < (S & Async) =
        observeProjected[A, S](Signal.identityProjection, baseline, repairInterval)(f)

    /** Runs `f` for the current value and then for every distinct value, until the enclosing `Scope` closes.
      *
      * Where the signal allows it, `f` runs without a fiber of its own: for a [[SignalRef]], a `map` or `readOnly` chain over one, and a
      * constant, `f` runs inside the `set` that changed the value, on the writer's stack, before that `set` returns. All `onChange`
      * subscribers of one ref share a single registration on it. Every other signal (`combineLatest`, `zip`, `switchMap`, [[Signal.initRaw]]
      * and the like) is observed by a fiber forked into the enclosing `Scope`, so `f` then runs shortly after the change instead.
      *
      * Because `f` runs inside somebody else's `set`, it must not block and should be cheap. The signal's `map` functions run there too. A
      * write issued from inside `f` is allowed and is delivered after the current delivery returns, so an `f` that writes a new value on
      * every delivery loops inside the writer. A failure thrown by `f` is logged and the subscription stays active: it neither aborts the
      * writer's `set` nor silences the signal's other subscribers, and on the fiber path the fiber keeps observing.
      *
      * Prefer [[observe]] for asynchronous or expensive work, or when each value needs a `Scope` of its own. Prefer `onChange` for cheap
      * synchronous side effects, in particular when many observers watch one signal: they cost one shared registration instead of a fiber
      * each.
      *
      * @param f
      *   The side effect for each distinct value
      */
    final def onChange(f: A => Unit < Sync)(using Frame): Unit < (Sync & Scope) =
        onChange(Absent)(f)

    /** Like [[onChange]] but seeded: `f` is skipped while the current value still equals `baseline`.
      *
      * @param baseline
      *   The value the caller has already processed (for example, already painted)
      * @param f
      *   The side effect for each distinct value
      */
    final def onChange(baseline: Maybe[A])(f: A => Unit < Sync)(using frame: Frame): Unit < (Sync & Scope) =
        Sync.Unsafe.defer {
            val run: A => Unit = a =>
                try Sync.Unsafe.evalOrThrow(f(a))
                catch case ex if NonFatal(ex) => Log.live.unsafe.error("Signal.onChange callback failed", ex)
            unsafeObserveProjected[A](Signal.identityProjection, baseline, run) match
                case Present(release) => Scope.ensure(Sync.Unsafe.defer(release()))
                case Absent           => Fiber.init(observe(baseline, Signal.defaultRepairInterval)(a => Sync.Unsafe.defer(run(a)))).unit
            end match
        }

    /** The observation primitive: observes `proj` of this signal and deduplicates on its result, the image.
      *
      * Every observation goes through here. [[observe]] passes the identity; `map` passes its function on to its source, so a chain of maps
      * is observed by one loop on the root signal, and [[SignalRef]] overrides this with its exact protocol. Comparing images rather than
      * source values is what a derived signal needs: a thousand rows deriving `selected.map(_ == row.id)` from one selection signal would
      * otherwise each tear down and set up their `Scope` on every selection change, though only two images moved.
      *
      * The per-image `Scope` opens when the image changes and stays open while it holds. `proj` is called through
      * [[Signal.lastProjection]], so an unchanged source value returns the image it gave last time. That matters for images that are not
      * `==` for equal inputs (a rendered UI tree never is): projecting again on every repair tick would count each tick as a change. `proj`
      * must therefore be a pure function of the source value.
      *
      * This default observes exactly while [[readExact]] answers, with the protocol of the `SignalRef.observeProjected` override over
      * every ref the value was read from. From the first read that does not answer on, it runs the repairing loop described on [[observe]].
      *
      * @param proj
      *   The view to observe, a pure function of the source value
      * @param baseline
      *   The image already processed by the caller: `g` is not run while the current image still equals it
      * @param repairInterval
      *   How often a parked observation re-reads `current` to reconcile a missed wakeup
      * @param g
      *   The per-image setup, run inside a fresh `Scope`
      */
    private[kyo] def observeProjected[B, S](proj: A => B, baseline: Maybe[B], repairInterval: Duration)(
        g: B => Unit < (S & Async & Scope)
    )(using CanEqual[B, B], Frame): Unit < (S & Async) =
        Local.snapshotWith { locals =>
            val image           = Signal.lastProjection(proj)
            val wakesOnAnyInput = repairInterval.isFinite
            // An infinite interval arms no timer: the loop then waits on `nextWith` alone, as `streamChanges` does.
            def await: Unit < Async =
                if repairInterval.isFinite then Async.race(Seq(nextWith(_ => ()), Async.sleep(repairInterval))).unit
                else nextWith(_ => ())
            // Loops until the image differs from `b`, so an idle repair timer never closes a still-current image's scope.
            def holdUntilChanged(b: B): Unit < (S & Async) =
                await.andThen(recheck(b))
            def recheck(b: B): Unit < (S & Async) =
                currentWith(c => if image(c) == b then holdUntilChanged(b) else (): Unit < (S & Async))
            // Cleared by the first read that is not exact, after which the observation stays on the repairing loop: without that
            // a signal over `initRaw` would run its exact parts twice per wakeup, once for the attempt and once for `currentWith`.
            var exact = true

            def holdExact(read: Signal.ExactRead, b: B): Unit < (S & Async) =
                read.awaitChange.andThen(Sync.Unsafe.defer {
                    val again = new Signal.ExactRead(locals)
                    readExact(again, wakesOnAnyInput) match
                        case Present(c) => if image(c) == b then holdExact(again, b) else (): Unit < (S & Async)
                        case Absent     =>
                            exact = false
                            recheck(b)
                    end match
                })
            def repairing(last: Maybe[B]): Unit < (S & Async) =
                currentWith { cur =>
                    val b = image(cur)
                    if last.exists(_ == b) then await.andThen(loop(last))
                    else Scope.run(g(b).andThen(holdUntilChanged(b))).andThen(loop(Present(b)))
                }
            def loop(last: Maybe[B]): Unit < (S & Async) =
                if !exact then repairing(last)
                else
                    Sync.Unsafe.defer {
                        val read = new Signal.ExactRead(locals)
                        readExact(read, wakesOnAnyInput) match
                            case Present(cur) =>
                                val b = image(cur)
                                if last.exists(_ == b) then read.awaitChange.andThen(loop(last))
                                else Scope.run(g(b).andThen(holdExact(read, b))).andThen(loop(Present(b)))
                            case Absent =>
                                exact = false
                                repairing(last)
                        end match
                    }
            loop(baseline)
        }
    end observeProjected

    /** Reads the current value together with every [[SignalRef]] it comes from, recording each ref with its version in `read`, or answers
      * `Absent` when the value does not come from refs alone, as for a signal defined with [[Signal.initRaw]].
      *
      * A signal that answers can be observed without a repair timer: its value can only change through a write to one of the recorded
      * refs, which moves that ref's version. `SignalRef` answers with itself, `map`, `switchMap` and `withLocal` with what they read
      * through (`withLocal` takes the local's value from `read`), and the other combinators with their inputs.
      *
      * @param wakesOnAnyInput
      *   Whether the caller re-reads on a change of any input, as an observation with a repair timer ends up doing. `zip` and `zipAll`
      *   answer only then: their `nextWith` waits for every input to change, and an observation without a timer keeps that.
      */
    private[kyo] def readExact(read: Signal.ExactRead, wakesOnAnyInput: Boolean)(using AllowUnsafe): Maybe[A] = Absent

    /** The current value, read on the caller's stack with no suspension.
      *
      * `Present` when the read is a field read behind pure projections: a [[SignalRef]], a constant, and `map`/`changesTo`/`readOnly`
      * chains over those. `Absent` for any other signal ([[Signal.initRaw]], the async combinators), which the caller then reads
      * through [[current]].
      *
      * For a synchronous pass that reads many signals, such as a UI tree walk and its HTML render. Through [[current]] every read is a
      * `Sync` suspension, which costs the pass a continuation for each frame enclosing the read. The value is the one [[current]]
      * would produce at the same point of the same pass.
      */
    private[kyo] def unsafeCurrent()(using AllowUnsafe): Maybe[A] = Absent

    /** Fiber-free observation for trivial, non-suspending sinks.
      *
      * `cb` is invoked on the writer's own stack, inside the `set` that changed the projected image, instead of waking a fiber through the
      * scheduler. The image-equality check therefore runs before any hop rather than after it, so a thousand observers of one signal cost a
      * thousand comparisons rather than a thousand scheduled tasks.
      *
      * Returns the release function. `Absent` means this signal has no callback path: only chains rooted in a [[SignalRef]] provide one, so
      * the caller keeps its fiber-based observation for everything else. The caller must call the release on teardown, since the
      * registration otherwise outlives it.
      *
      * `cb` must not suspend and must stay trivial: it runs inside somebody else's `set`. [[onChange]] is the public form, which releases
      * on the enclosing `Scope` and falls back to a fiber where this answers `Absent`.
      */
    private[kyo] def unsafeObserveProjected[B](proj: A => B, baseline: Maybe[B], cb: B => Unit)(
        using
        CanEqual[B, B],
        AllowUnsafe,
        Frame
    ): Maybe[() => Unit] = Absent

    /** Creates a new signal by applying a transformation function to this signal's values.
      *
      * This operation creates a derived signal that automatically updates whenever the source signal changes, lazily applying the given
      * transformation to each value.
      *
      * @param f
      *   The transformation function to apply to signal values
      * @return
      *   A new signal containing transformed values
      */
    @nowarn("msg=anonymous")
    inline def map[B](inline f: A => B)(using canEqualB: CanEqual[B, B], frame: Frame): Signal[B] =
        Signal._initRawF(
            [C, S] => g => self.currentWith(a => g(f(a))),
            [C, S] => g => self.nextWith(a => g(f(a))),
            allow => self.unsafeCurrent()(using allow).map(f),
            // A projection of `x.map(f)` is one of `x` through `proj` after `f`, so a chain rooted in a SignalRef keeps
            // that ref's exact protocol past every map. `proj` is called through `lastProjection` keyed on `f`'s result,
            // so an unchanged value at this stage hands on the same image and nothing is delivered for it.
            [C, S] =>
                (proj, baseline, ri, g, canEqualC) =>
                    Sync.defer {
                        val p = Signal.lastProjection(proj)
                        self.observeProjected[C, S](a => p(f(a)), baseline, ri)(g)(using canEqualC, frame)
                    },
            (read, wakesOnAnyInput) => self.readExact(read, wakesOnAnyInput).map(a => f(a)),
            // The same composition for the callback path, made per subscription. Its dispatch walks can overlap, so the
            // memo of this stage is the atomic `sharedLastProjection`.
            [C] =>
                (proj, baseline, cb, canEqualC, allow) =>
                    val p = Signal.sharedLastProjection(proj)
                    self.unsafeObserveProjected[C](a => p(f(a)), baseline, cb)(using canEqualC, allow, frame)
        )

    /** This signal's changes, carrying `b` in place of its own values.
      *
      * Not `map(_ => b)`: observation deduplicates on the image, so a constant image collapses to a single delivery however often the
      * source moves. That is right for a projection, whose view did not change, and wrong for a caller whose emitted value is a stable
      * handle and whose content is rebuilt from the source at delivery time. Such a caller needs the source's own change detection, which is
      * what this keeps: it observes the source on its values and hands over the constant.
      *
      * The baseline is in `b`'s space and `b` is the only value there, so it can only mean "the caller has already processed one
      * delivery". That is honoured by seeding the source observation with the source's current value, after which every source change
      * delivers again.
      *
      * The callback path declines (`Absent`): its subscriber deduplicates on the image too.
      */
    private[kyo] def changesTo[B](b: B)(using canEqualB: CanEqual[B, B], frame: Frame): Signal[B] =
        Signal._initRawF(
            [C, S] => g => self.currentWith(_ => g(b)),
            [C, S] => g => self.nextWith(_ => g(b)),
            _ => Present(b),
            [C, S] =>
                (proj, baseline, ri, g, canEqualC) =>
                    given CanEqual[C, C] = canEqualC
                    val image            = proj(b)
                    // `g(image)` inside the lambda: each delivery builds the caller's setup afresh, as `observe` does.
                    if baseline.exists(_ == image) then self.currentWith(a0 => self.observe[S](Present(a0), ri)(_ => g(image)))
                    else self.observe[S](Absent, ri)(_ => g(image))
            ,
            (read, wakesOnAnyInput) => self.readExact(read, wakesOnAnyInput).map(_ => b),
            [C] => (_, _, _, _, _) => Absent
        )
    end changesTo

    /** Dynamically switches to an inner signal based on the current value.
      *
      * When the outer signal changes, switches to the new inner signal produced by `f`. When the current inner signal changes, propagates
      * that change. This is switchMap semantics (no monad laws): the previous inner is implicitly dropped on outer change. The caller
      * re-arms via `nextWith` in a loop matching the `streamChanges` driver pattern.
      *
      * Note: like `streamChanges`, may skip intermediate values if changes occur faster than they can be processed. The combinator's own
      * await/re-read window applies to `next` (a write landing between the wakeup and the re-read is coalesced). An observation is exact
      * while this signal and every inner signal it has selected so far are; from the first inner signal that is not (one defined with
      * `initRaw`), a missed write is reconciled within the repair interval, or with `Duration.Infinity` (no timer) only with the next
      * change.
      *
      * @param f
      *   The function that produces an inner signal from the current value
      * @return
      *   A new signal that tracks the current inner signal
      */
    @nowarn("msg=anonymous")
    inline def switchMap[B](inline f: A => Signal[B])(using CanEqual[B, B], Frame): Signal[B] =
        Signal._initExact(
            [C, S] => g => self.currentWith(a => f(a).currentWith(g)),
            [C, S] =>
                g =>
                    self.currentWith { a =>
                        val inner = f(a)
                        Signal.awaitAny(Seq(self, inner))
                            .andThen(self.currentWith { a2 =>
                                (if a2 == a then inner else f(a2)).currentWith(g)
                            })
                    },
            (read, wakesOnAnyInput) => self.readExact(read, wakesOnAnyInput).flatMap(a => f(a).readExact(read, wakesOnAnyInput))
        )

    /** Pairs this signal with another, waiting for both to change before emitting.
      *
      * The wait for both belongs to `next` and to `streamChanges`. [[observe]] with a finite repair interval delivers the current pair on
      * a change of either input, as it always did once the repair timer fired; over [[SignalRef]]s it does so at once.
      *
      * @param other
      *   The signal to pair with
      * @return
      *   A signal of pairs that updates only when both inputs have changed since the last emit
      */
    @nowarn("msg=anonymous")
    inline def zip[B](other: Signal[B])(using CanEqual[(A, B), (A, B)], Frame): Signal[(A, B)] =
        Signal._initExact(
            [C, S] => g => self.currentWith(a => other.currentWith(b => g((a, b)))),
            [C, S] => g => Async.zip(self.next, other.next).andThen(self.currentWith(a => other.currentWith(b => g((a, b))))),
            (read, wakesOnAnyInput) =>
                if !wakesOnAnyInput then Absent
                else self.readExact(read, wakesOnAnyInput).flatMap(a => other.readExact(read, wakesOnAnyInput).map(b => (a, b)))
        )

    /** Pairs this signal with another, emitting when either changes (Rx combineLatest semantics).
      *
      * Note: like `streamChanges`, may skip intermediate values if changes occur faster than they can be processed.
      *
      * @param other
      *   The signal to pair with
      * @return
      *   A signal of pairs that updates when either input changes
      */
    @nowarn("msg=anonymous")
    inline def combineLatest[B](other: Signal[B])(using CanEqual[(A, B), (A, B)], Frame): Signal[(A, B)] =
        Signal._initExact(
            [C, S] => g => self.currentWith(a => other.currentWith(b => g((a, b)))),
            [C, S] => g => Signal.awaitAny(Seq(self, other)).andThen(self.currentWith(a => other.currentWith(b => g((a, b))))),
            (read, wakesOnAnyInput) =>
                self.readExact(read, wakesOnAnyInput).flatMap(a => other.readExact(read, wakesOnAnyInput).map(b => (a, b)))
        )

    /** Creates a stream that continuously emits the current value of the signal.
      *
      * This method produces a stream that will emit the signal's current value repeatedly. It's useful for scenarios where you need to
      * continuously monitor the signal's state, even when the value hasn't changed.
      *
      * @return
      *   A stream that continuously emits the current signal value
      */
    final def streamCurrent(using Frame, Tag[Emit[Chunk[A]]]): Stream[A, Async] =
        Stream {
            Loop.forever(currentWith(a => Emit.value(Chunk(a))))
        }

    /** Creates a stream that emits only when the signal's value changes.
      *
      * This method produces a stream that emits values only when they differ from the previous value, starting with the value current at
      * subscription. Note that rapid changes may result in some intermediate values being skipped if they occur faster than they can be
      * processed. Built on [[observe]], so on a signal that observes exactly the latest value is never stranded.
      *
      * A signal defined with `initRaw`, a combinator over one, and a `zip` or `zipAll` (which emit only once every input has changed) wait
      * on `nextWith` with no repair timer, so an idle stream costs nothing. A write landing between their read and their re-registration
      * is then emitted only with the next change; pass a finite `repairInterval` to `streamChanges(baseline, repairInterval)` to reconcile
      * it on a timer instead.
      *
      * @return
      *   A stream that emits only when values change
      */
    final def streamChanges(using Frame, Tag[Emit[Chunk[A]]]): Stream[A, Async] =
        streamChanges(Absent)

    /** Like [[streamChanges]] but seeded, as [[observe]] with a baseline is: the stream starts with the first value that differs from
      * `baseline`, so a caller that already processed `v` passes `Present(v)` and receives only what changed since.
      *
      * @param baseline
      *   The value already processed by the caller; `Absent` makes this [[streamChanges]]
      */
    final def streamChanges(baseline: Maybe[A])(using Frame, Tag[Emit[Chunk[A]]]): Stream[A, Async] =
        streamChanges(baseline, Duration.Infinity)

    /** Like [[streamChanges]] with a baseline, and a reconciliation interval for signals that do not observe exactly: a finite one
      * re-reads the value on a timer while the stream waits, which closes the read/register gap at the cost of one wakeup per interval.
      * A `zip` or `zipAll` then emits on a change of any input.
      */
    final def streamChanges(baseline: Maybe[A], repairInterval: Duration)(using Frame, Tag[Emit[Chunk[A]]]): Stream[A, Async] =
        Stream(observe[Emit[Chunk[A]]](baseline, repairInterval)(a => Emit.value(Chunk(a))))

end Signal

export Signal.SignalRef

object Signal:

    /** Default reconciliation interval used by [[Signal.observe]] when none is given.
      *
      * It bounds how soon a missed wakeup is reconciled by re-reading `current`: a write that races the observation's
      * read/register window is delivered within this interval. Real changes are otherwise immediate, so this can be
      * generous; it exists to bound that rare race, not to drive normal updates. Exact observers (signals whose value
      * comes from [[SignalRef]]s alone, see [[Signal.observe]]) never miss a wakeup and ignore it entirely, arming no
      * timer at all.
      */
    val defaultRepairInterval: Duration = 1.second

    private val identityAny: Any => Any = a => a

    /** The projection [[Signal.observe]] passes to [[Signal.observeProjected]]; [[lastProjection]] leaves it alone. */
    private[kyo] def identityProjection[A]: A => A = identityAny.asInstanceOf[A => A]

    /** `proj` remembering its last input and image, so an equal input returns that same image instance and the `==` that follows holds
      * even for images that are not `==` when built afresh. One is made per observation, which runs on one fiber at a time.
      */
    private[kyo] def lastProjection[A, B](proj: A => B)(using CanEqual[A, A]): A => B =
        if proj.asInstanceOf[AnyRef] eq identityAny then proj
        else
            new (A => B):
                private var projected  = false
                private var lastIn: A  = uninitialized
                private var lastOut: B = uninitialized
                def apply(a: A): B     =
                    if !projected || lastIn != a then
                        lastOut = proj(a)
                        lastIn = a
                        projected = true
                    end if
                    lastOut
                end apply

    /** [[lastProjection]] for the callback path, where two dispatch walks of one subscriber can overlap: the last input and its image
      * sit in one atomic cell, so a walk reads a consistent pair. An overlap can at worst project the same input twice.
      */
    private[kyo] def sharedLastProjection[A, B](proj: A => B)(using CanEqual[A, A]): A => B =
        if proj.asInstanceOf[AnyRef] eq identityAny then proj
        else
            val last = new java.util.concurrent.atomic.AtomicReference[Maybe[(A, B)]](Absent)
            a =>
                last.get() match
                    case Present((in, out)) if in == a => out
                    case _                             =>
                        val out = proj(a)
                        last.set(Present((a, out)))
                        out
                end match

    /** Waits for any of the given signals to change.
      *
      * No signal can change if there is none to watch, so an empty sequence never completes.
      *
      * @param signals
      *   The signals to watch
      */
    def awaitAny(signals: Seq[Signal[?]])(using Frame): Unit < Async =
        if signals.isEmpty then Async.never
        else Async.race(signals.map(_.next)).unit

    /** Zips a sequence of signals, waiting for all to change before emitting.
      *
      * As for [[Signal.zip]], the wait for all belongs to `next` and to `streamChanges`; [[Signal.observe]] with a finite repair interval
      * delivers on a change of any input.
      *
      * @param signals
      *   The signals to zip
      * @return
      *   A signal of Chunk that updates when all inputs have changed
      */
    @nowarn("msg=anonymous")
    inline def zipAll[A](signals: Seq[Signal[A]])(
        using
        Frame,
        CanEqual[A, A],
        CanEqual[Chunk[A], Chunk[A]]
    ): Signal[Chunk[A]] =
        signals.size match
            case 0 => initConst(Chunk.empty[A])
            case 1 => signals.head.map(Chunk(_))
            case n =>
                val sigs = Chunk.from(signals, n)
                _initExact(
                    [B, S] => f => Kyo.foreach(sigs)(_.current).map(f),
                    [B, S] => f => Async.foreachDiscard(sigs, sigs.size)(_.next).andThen(Kyo.foreach(sigs)(_.current).map(f)),
                    (read, wakesOnAnyInput) => if !wakesOnAnyInput then Absent else readAllExact(sigs, read, wakesOnAnyInput)
                )

    /** Zips a sequence of signals, emitting when any changes.
      *
      * Note: like `streamChanges`, may skip intermediate values if changes occur faster than they can be processed.
      *
      * @param signals
      *   The signals to zip
      * @return
      *   A signal of Chunk that updates when any input changes
      */
    @nowarn("msg=anonymous")
    inline def combineLatestAll[A](signals: Seq[Signal[A]])(using Frame, CanEqual[A, A]): Signal[Chunk[A]] =
        signals.size match
            case 0 => initConst(Chunk.empty[A])
            case 1 => signals.head.map(Chunk(_))
            case n =>
                val sigs = Chunk.from(signals, n)
                _initExact(
                    [C, S] => g => Kyo.foreach(sigs)(_.current).map(g),
                    [C, S] => g => awaitAny(sigs).andThen(Kyo.foreach(sigs)(_.current).map(g)),
                    (read, wakesOnAnyInput) => readAllExact(sigs, read, wakesOnAnyInput)
                )

    private[kyo] def readAllExact[A](sigs: Chunk[Signal[A]], read: ExactRead, wakesOnAnyInput: Boolean)(using
        AllowUnsafe
    ): Maybe[Chunk[A]] =
        val values                                 = new Array[Any](sigs.size)
        @tailrec def loop(i: Int): Maybe[Chunk[A]] =
            if i == values.length then Present(Chunk.fromNoCopy(values).asInstanceOf[Chunk[A]])
            else
                sigs(i).readExact(read, wakesOnAnyInput) match
                    case Present(a) =>
                        values(i) = a
                        loop(i + 1)
                    case Absent => Absent
        loop(0)
    end readAllExact

    private inline val missingCanEqual =
        "Cannot create Signal because values of type '${A}' cannot be compared for equality to detect changes. Make sure there is a 'CanEqual[${A}, ${A}]' instance available."

    /** Creates a new mutable signal reference with an initial value.
      *
      * This method initializes a new `SignalRef[A]` that can be modified over time. The reference starts with the provided initial value
      * and can be updated using methods like `set`, `getAndSet`, etc.
      *
      * @param initial
      *   The starting value for the signal reference
      * @return
      *   A new mutable `SignalRef[A]`
      * @tparam A
      *   The type of value contained in the signal. Must have an instance of `CanEqual[A, A]`
      */
    def initRef[A](initial: A)(
        using
        frame: Frame,
        @implicitNotFound(missingCanEqual)
        canEqual: CanEqual[A, A]
    ): SignalRef[A] < Sync =
        initRefWith[A](initial)(identity)

    /** Creates a new mutable signal reference with an initial value and applies a transformation function.
      *
      * This method initializes a new `SignalRef[A]` that can be modified over time, and immediately applies a transformation function to
      * it. The reference starts with the provided initial value and the transformation is applied within the same atomic operation.
      *
      * @param initial
      *   The starting value for the signal reference
      * @param f
      *   The transformation function to apply to the newly created reference
      * @return
      *   The result of applying the transformation function
      * @tparam A
      *   The type of value contained in the signal. Must have an instance of `CanEqual[A, A]`
      * @tparam B
      *   The return type of the transformation function
      * @tparam S
      *   The effect type of the transformation function
      */
    def initRefWith[A](initial: A)[B, S](f: SignalRef[A] => B < S)(
        using
        frame: Frame,
        @implicitNotFound(missingCanEqual)
        canEqual: CanEqual[A, A]
    ): B < (S & Sync) =
        Sync.Unsafe.defer(f(new SignalRef(SignalRef.Unsafe.init(initial))))

    /** Creates a new immutable signal with a constant value.
      *
      * This method creates a signal that always returns the same value. Unlike `SignalRef`, this signal cannot be modified after creation.
      * This is useful for cases where you need a signal interface but the value never changes.
      *
      * Since the value never changes, `next`/`nextWith` never complete. Read a constant with `current`/`currentWith`, and expect it to sit
      * out the change-driven combinators (`awaitAny`, `combineLatest`, `zip`) rather than drive them.
      *
      * @param value
      *   The constant value for the signal
      * @return
      *   A new immutable `Signal[A]` that always returns the provided value
      * @tparam A
      *   The type of value contained in the signal. Must have an instance of `CanEqual[A, A]`
      */
    def initConst[A](value: A)(
        using
        frame: Frame,
        @implicitNotFound(missingCanEqual)
        canEqual: CanEqual[A, A]
    ): Signal[A] =
        _initRawF(
            [B, S] => f => f(value),
            // Completing this immediately would let a constant win every `awaitAny` arm, firing
            // `combineLatest(ref, const).next` with no change to report and spinning an enclosing `observe`.
            [B, S] => _ => Async.never,
            _ => Present(value),
            // A constant cannot change, so there is nothing for a reconciliation timer to reconcile: the repairing loop would
            // re-arm a `nextWith`/`sleep` race every interval, forever, for each observer. Deliver once and hold the scope
            // instead; interrupting the observation still closes it.
            [C, S] =>
                (proj, baseline, repairInterval, g, canEqualC) =>
                    given CanEqual[C, C] = canEqualC
                    discard(repairInterval)
                    val image = proj(value)
                    if baseline.exists(_ == image) then Async.never[Unit]
                    else Scope.run(g(image).andThen(Async.never[Unit]))
            ,
            (_, _) => Present(value),
            // A constant delivers once and can never fire again, so the release is a no-op and there is
            // nothing to register on.
            [C] =>
                (proj, baseline, cb, canEqualC, _) =>
                    given CanEqual[C, C] = canEqualC
                    val image            = proj(value)
                    if !baseline.exists(_ == image) then cb(image)
                    Present(() => ())
        )

    /** Creates a new immutable signal with a constant value and applies a transformation function.
      *
      * This method creates a signal that always returns the same value and immediately applies a transformation function to it. Unlike
      * `SignalRef`, this signal cannot be modified after creation.
      *
      * @param value
      *   The constant value for the signal
      * @param f
      *   The transformation function to apply to the newly created signal
      * @return
      *   The result of applying the transformation function
      * @tparam A
      *   The type of value contained in the signal. Must have an instance of `CanEqual[A, A]`
      * @tparam B
      *   The return type of the transformation function
      * @tparam S
      *   The effect type of the transformation function
      */
    def initConstWith[A](value: A)[B, S](f: Signal[A] => B < S)(
        using
        frame: Frame,
        @implicitNotFound(missingCanEqual)
        canEqual: CanEqual[A, A]
    ): B < S =
        f(initConst(value))

    /** Creates a signal chosen from a [[Local]]: the signal `f` returns for the local's value where it is read.
      *
      * Every read (`current`, `next`, and each step of an observation) reads `local` in the reading fiber and then the signal `f` returns
      * for that value, so the same signal reads different sources under different `Local.let` bindings. An observation reads the local
      * of the observing fiber, which does not change while it runs.
      *
      * This is the alternative to [[initRaw]] for a signal chosen from context. A signal defined with `initRaw` hides the signal it
      * delegates to, so its observation keeps the repair timer; this one observes exactly whenever the chosen signal does (see
      * [[Signal.observe]]), with no repair timer.
      *
      * `f` must be a pure function of the local's value. A choice that changes over time belongs in [[Signal.switchMap]] over a signal
      * that holds it.
      *
      * @param local
      *   The local the signal is chosen from
      * @param f
      *   The signal for a value of `local`, applied on every read
      * @tparam L
      *   The type of the local's value
      * @tparam A
      *   The type of value contained in the signal. Must have an instance of `CanEqual[A, A]`
      * @return
      *   A signal that reads the signal `f` returns for the local's value where it is read
      */
    def withLocal[L, A](local: Local[L])(f: L => Signal[A])(
        using
        frame: Frame,
        @implicitNotFound(missingCanEqual)
        canEqual: CanEqual[A, A]
    ): Signal[A] =
        _initExact(
            [B, S] => g => local.use(l => f(l).currentWith(g)),
            [B, S] => g => local.use(l => f(l).nextWith(g)),
            (read, wakesOnAnyInput) => f(local.getIn(read.locals)).readExact(read, wakesOnAnyInput)
        )

    /** Creates a new signal by specifying its fundamental operations.
      *
      * This is a lower-level constructor that allows direct implementation of a signal's behavior through its currentWith and nextWith
      * operations. It's primarily intended for implementing signal combinators and custom signal types.
      *
      * @param currentWith
      *   The implementation of currentWith, handling synchronous value access and transformation
      * @param nextWith
      *   The implementation of nextWith, handling asynchronous value changes and transformation
      * @tparam A
      *   The type of value contained in the signal. Must have an instance of `CanEqual[A, A]`
      * @return
      *   A new signal with the specified behavior
      */
    @nowarn("msg=anonymous")
    inline def initRaw[A](
        inline currentWith: [B, S] => (A => B < S) => B < (S & Sync),
        inline nextWith: [B, S] => (A => B < S) => B < (S & Async)
    )(
        using
        frame: Frame,
        @implicitNotFound(missingCanEqual)
        canEqual: CanEqual[A, A]
    ): Signal[A] =
        _initRaw(currentWith, nextWith)

    /** Creates a new signal by specifying its fundamental operations and applies a transformation function.
      *
      * This is a lower-level constructor that allows direct implementation of a signal's behavior through its currentWith and nextWith
      * operations, and immediately applies a transformation function to the created signal. It's primarily intended for implementing signal
      * combinators and custom signal types.
      *
      * @param currentWith
      *   The implementation of currentWith, handling synchronous value access and transformation
      * @param nextWith
      *   The implementation of nextWith, handling asynchronous value changes and transformation
      * @param f
      *   The transformation function to apply to the newly created signal
      * @tparam A
      *   The type of value contained in the signal. Must have an instance of `CanEqual[A, A]`
      * @tparam B
      *   The return type of the transformation function
      * @tparam S
      *   The effect type of the transformation function
      * @return
      *   The result of applying the transformation function
      */
    @nowarn("msg=anonymous")
    inline def initRawWith[A](
        inline currentWith: [B, S] => (A => B < S) => B < (S & Sync),
        inline nextWith: [B, S] => (A => B < S) => B < (S & Async)
    )[B, S](f: Signal[A] => B < S)(
        using
        frame: Frame,
        @implicitNotFound(missingCanEqual)
        canEqual: CanEqual[A, A]
    ): B < S =
        f(initRaw(currentWith, nextWith))

    // Separated from initRaw to avoid name conflicts between parameters and Signal members
    @nowarn("msg=anonymous")
    private inline def _initRaw[A](
        inline _currentWith: [B, S] => (A => B < S) => B < (S & Sync),
        inline _nextWith: [B, S] => (A => B < S) => B < (S & Async)
    )(
        using
        frame: Frame,
        canEqual: CanEqual[A, A]
    ): Signal[A] =
        new Signal[A]:
            def currentWith[B, S](f: A => B < S)(using frame: Frame): B < (S & Sync) =
                _currentWith(f)
            def nextWith[B, S](f: A => B < S)(using frame: Frame): B < (S & Async) =
                _nextWith(f)
        end new
    end _initRaw

    // Like _initRaw but also supplies `readExact`, for the combinators whose value comes from their inputs alone.
    @nowarn("msg=anonymous")
    private inline def _initExact[A](
        inline _currentWith: [B, S] => (A => B < S) => B < (S & Sync),
        inline _nextWith: [B, S] => (A => B < S) => B < (S & Async),
        inline _readExact: (ExactRead, Boolean) => AllowUnsafe ?=> Maybe[A]
    )(
        using
        frame: Frame,
        canEqual: CanEqual[A, A]
    ): Signal[A] =
        new Signal[A]:
            def currentWith[B, S](f: A => B < S)(using frame: Frame): B < (S & Sync) =
                _currentWith(f)
            def nextWith[B, S](f: A => B < S)(using frame: Frame): B < (S & Async) =
                _nextWith(f)
            override private[kyo] def readExact(read: ExactRead, wakesOnAnyInput: Boolean)(using AllowUnsafe): Maybe[A] =
                _readExact(read, wakesOnAnyInput)
        end new
    end _initExact

    // Like _initExact but also supplies `observeProjected`, letting a structural combinator delegate observation to its
    // source's loop rather than running a second repair loop over its own `currentWith`/`nextWith`. `map` uses this so a
    // `map`-over-leaf chain observes through one loop rooted at the leaf.
    @nowarn("msg=anonymous")
    private inline def _initRawF[A](
        inline _currentWith: [B, S] => (A => B < S) => B < (S & Sync),
        inline _nextWith: [B, S] => (A => B < S) => B < (S & Async),
        // The suspension-free read (see `unsafeCurrent`), composed down the chain like the projections below.
        inline _unsafeCurrent: AllowUnsafe => Maybe[A],
        inline _observeProjected: [C, S] => (
            A => C,
            Maybe[C],
            Duration,
            C => Unit < (S & Async & Scope),
            CanEqual[C, C]
        ) => Unit < (S & Async),
        inline _readExact: (ExactRead, Boolean) => AllowUnsafe ?=> Maybe[A],
        inline _unsafeObserveProjected: [C] => (
            A => C,
            Maybe[C],
            C => Unit,
            CanEqual[C, C],
            AllowUnsafe
        ) => Maybe[() => Unit]
    )(
        using
        frame: Frame,
        canEqual: CanEqual[A, A]
    ): Signal[A] =
        new Signal[A]:
            def currentWith[B, S](f: A => B < S)(using frame: Frame): B < (S & Sync) =
                _currentWith(f)
            def nextWith[B, S](f: A => B < S)(using frame: Frame): B < (S & Async) =
                _nextWith(f)
            override private[kyo] def unsafeCurrent()(using allow: AllowUnsafe): Maybe[A] =
                _unsafeCurrent(allow)
            override private[kyo] def observeProjected[C, S](proj: A => C, baseline: Maybe[C], repairInterval: Duration)(
                g: C => Unit < (S & Async & Scope)
            )(using canEqualC: CanEqual[C, C], frame: Frame): Unit < (S & Async) =
                _observeProjected(proj, baseline, repairInterval, g, canEqualC)
            override private[kyo] def readExact(read: ExactRead, wakesOnAnyInput: Boolean)(using AllowUnsafe): Maybe[A] =
                _readExact(read, wakesOnAnyInput)
            override private[kyo] def unsafeObserveProjected[C](proj: A => C, baseline: Maybe[C], cb: C => Unit)(
                using
                canEqualC: CanEqual[C, C],
                allow: AllowUnsafe,
                frame: Frame
            ): Maybe[() => Unit] =
                _unsafeObserveProjected(proj, baseline, cb, canEqualC, allow)
        end new
    end _initRawF

    /** One callback subscriber of a [[SignalRef]], holding the projection it observes through and the last image it was told about.
      *
      * `last` is an atomic cell rather than a plain field because two dispatch walks can overlap: one from a fired waiter and one from the
      * re-arm loop that noticed a version change. Taking the cell with `getAndSet` makes exactly one of them call `cb`, so an overlap costs
      * a redundant projection instead of a duplicated side effect.
      */
    final private[kyo] class Sub[A, B](proj: A => B, cb: B => Unit, initial: Maybe[B])(using CanEqual[B, B]):
        private val last  = new java.util.concurrent.atomic.AtomicReference[Maybe[B]](initial)
        private val alive = new java.util.concurrent.atomic.AtomicBoolean(true)

        def isAlive: Boolean = alive.get()

        /** Retire this subscriber; `true` only for the caller that actually retired it, so releasing twice
          * is a no-op instead of decrementing the live count twice.
          */
        def kill(): Boolean = alive.compareAndSet(true, false)

        def deliver(value: A): Unit =
            val image = proj(value)
            if !last.getAndSet(Present(image)).exists(_ == image) then cb(image)
    end Sub

    /** A one-way view of another signal, deliberately not a [[SignalRef]]. See [[SignalRef.readOnly]].
      *
      * Every observation path forwards to the source, so the view costs one allocation and keeps the source's delivery protocol instead
      * of falling back to the repairing loop.
      */
    final private class ReadOnly[A](source: Signal[A])(using CanEqual[A, A]) extends Signal[A]:

        def currentWith[B, S](f: A => B < S)(using Frame): B < (S & Sync) = source.currentWith(f)

        def nextWith[B, S](f: A => B < S)(using Frame): B < (S & Async) = source.nextWith(f)

        override private[kyo] def unsafeCurrent()(using AllowUnsafe): Maybe[A] = source.unsafeCurrent()

        override private[kyo] def observeProjected[B, S](proj: A => B, baseline: Maybe[B], repairInterval: Duration)(
            g: B => Unit < (S & Async & Scope)
        )(using CanEqual[B, B], Frame): Unit < (S & Async) =
            source.observeProjected(proj, baseline, repairInterval)(g)

        override private[kyo] def readExact(read: ExactRead, wakesOnAnyInput: Boolean)(using AllowUnsafe): Maybe[A] =
            source.readExact(read, wakesOnAnyInput)

        override private[kyo] def unsafeObserveProjected[B](proj: A => B, baseline: Maybe[B], cb: B => Unit)(
            using
            CanEqual[B, B],
            AllowUnsafe,
            Frame
        ): Maybe[() => Unit] =
            source.unsafeObserveProjected(proj, baseline, cb)
    end ReadOnly

    /** The [[SignalRef]]s an exact read went through, each with the version it had before its value was read (see
      * [[Signal.readExact]]), and the wakeup an observation of that read parks on.
      *
      * [[awaitChange]] registers this on the next-change promise of every recorded ref and checks the versions again, as
      * `SignalRef.observeProjected` does for one ref. Once this completes, by a change or by the interrupt of the parked observer, it
      * deregisters from all of those promises, so a source that does not change keeps no registration of it. One instance serves one read
      * and at most one wait.
      *
      * It is itself the promise the observer parks on and the `Result => Unit` callback it registers with `onComplete`, so one wait
      * allocates no closure per ref and `remove` finds the registration by identity.
      *
      * @param locals
      *   The [[Local]] values bound where the observation runs (see [[Local.snapshotWith]]), for a signal defined with
      *   [[Signal.withLocal]], which cannot read its `Local` inside `readExact`
      */
    final private[kyo] class ExactRead(val locals: Map[Local[?], AnyRef])
        extends IOPromise[Nothing, Unit] with (Result[Any, Any] => Unit):
        private var refs: Array[SignalRef.Unsafe[?]]    = null
        private var versions: Array[Long]               = null
        private var size                                = 0
        private var watched: Array[IOPromise[Any, Any]] = null
        // Published after the registrations and read by `onComplete`: one of the two sides sees the other (see `watch`).
        @volatile private var watchedCount = 0

        def add(ref: SignalRef.Unsafe[?], version: Long): Unit =
            if refs == null then
                refs = new Array(4)
                versions = new Array(4)
            else if size == refs.length then
                val r = new Array[SignalRef.Unsafe[?]](size * 2)
                val v = new Array[Long](size * 2)
                System.arraycopy(refs, 0, r, 0, size)
                System.arraycopy(versions, 0, v, 0, size)
                refs = r
                versions = v
            end if
            refs(size) = ref
            versions(size) = version
            size += 1
        end add

        def changed()(using AllowUnsafe): Boolean =
            @tailrec def loop(i: Int): Boolean = i < size && (refs(i).version() != versions(i) || loop(i + 1))
            loop(0)

        /** Returns once one of the recorded refs has changed since the read.
          *
          * Once the result of an observer interrupted while parked is set, every registration is removed: its finalizer, which runs before
          * that, removes them itself even when a change completed this first. For an observer that a change resumed, the writer may still be
          * removing them for a moment.
          */
        def awaitChange(using Frame): Unit < Async =
            Sync.Unsafe.defer {
                if changed() then ()
                else
                    watch()
                    if changed() then
                        completeDiscard(Result.unit)
                        (): Unit < Async
                    else
                        Sync.ensure { (error: Maybe[Result.Error[Any]]) =>
                            // Winning the completion runs `unwatch` here through `onComplete`; losing it, the writer's may still run.
                            if error.isDefined && !complete(Result.unit) then unwatch()
                        }(Async.useResult(this)(_ => ()))
                    end if
            }

        private def watch()(using AllowUnsafe): Unit =
            val promises                    = new Array[IOPromise[Any, Any]](size)
            @tailrec def loop(i: Int): Unit =
                if i < size then
                    val p = refs(i).next().lower.asInstanceOf[IOPromise[Any, Any]]
                    promises(i) = p
                    p.onComplete(this)
                    loop(i + 1)
            loop(0)
            watched = promises
            // A change may have completed this while the loop ran, and its `onComplete` may have read the count before this write.
            watchedCount = size
            if done() then unwatch()
        end watch

        private def unwatch(): Unit =
            val n                           = watchedCount
            @tailrec def loop(i: Int): Unit =
                if i < n then
                    discard(watched(i).remove(this))
                    loop(i + 1)
            loop(0)
        end unwatch

        def apply(r: Result[Any, Any]): Unit = completeDiscard(Result.unit)

        override protected def onComplete(): Unit = unwatch()
    end ExactRead

    /** A mutable reference implementation of Signal that allows modification of its value over time.
      *
      * This class provides methods to get, set, and modify the contained value atomically. All operations are thread-safe and will properly
      * notify observers of changes.
      *
      * @tparam A
      *   The type of value contained in the reference. Must have an instance of `CanEqual[A, A]`
      */
    final class SignalRef[A] private[Signal] (_unsafe: SignalRef.Unsafe[A])(using CanEqual[A, A]) extends Signal[A]:

        def currentWith[B, S](f: A => B < S)(using Frame) = Sync.Unsafe.defer(f(unsafe.get()))

        override private[kyo] def unsafeCurrent()(using AllowUnsafe): Maybe[A] = Present(unsafe.get())

        def nextWith[B, S](f: A => B < S)(using Frame) = Sync.Unsafe.defer(unsafe.next().safe.use(f))

        override private[kyo] def readExact(read: ExactRead, wakesOnAnyInput: Boolean)(using AllowUnsafe): Maybe[A] =
            read.add(_unsafe, _unsafe.version())
            Present(_unsafe.get())

        /** The callback twin of [[observeProjected]]: delivery happens inside the writer's own `set`, with no scheduler hop.
          *
          * All subscribers of one ref share a single registration on the next-change promise (see `Unsafe.subscribe`), so a write fires one
          * waiter and walks an array rather than completing one waiter per subscriber on the writing thread.
          */
        override private[kyo] def unsafeObserveProjected[B](proj: A => B, baseline: Maybe[B], cb: B => Unit)(
            using
            CanEqual[B, B],
            AllowUnsafe,
            Frame
        ): Maybe[() => Unit] =
            val sub = new Signal.Sub[A, B](proj, cb, baseline)
            _unsafe.subscribe(sub)
            Present(() => _unsafe.unsubscribe(sub))
        end unsafeObserveProjected

        /** Observes exactly, without a repair timer, through a version-validated register/validate/await protocol.
          *
          * A write stores the value, increments the version, then swaps and completes the next-change promise (see `Unsafe.onUpdate`). The
          * observer reads the version before the value, runs `g`, and re-arms by capturing the next-change promise and checking the version
          * again, parking only while it is unchanged. A write that lands before the check is seen by the check; one that lands after it
          * completes exactly the captured promise. Either way no change is stranded, and an idle observer holds exactly one waiter.
          *
          * Reading the version before the value is what makes this sound: the other order could pair a fresh value with a stale version and
          * then wait on a promise that write has already completed and replaced. Observation stays level-based, so a change and its revert
          * during `g` wake the observer, which finds an unchanged image and waits again.
          *
          * `repairInterval` is not used: only signals that can miss a wakeup need it.
          */
        override private[kyo] def observeProjected[B, S](proj: A => B, baseline: Maybe[B], repairInterval: Duration)(
            g: B => Unit < (S & Async & Scope)
        )(using CanEqual[B, B], Frame): Unit < (S & Async) =
            def nextSince(v0: Long): Unit < Async =
                Sync.Unsafe.defer {
                    if _unsafe.version() != v0 then ()
                    else
                        // Parks directly on the masked next-change promise: an interrupted fiber releases the wakeup it
                        // registered there (see `IOTask`), so the observer stays interruptible without a wrapper.
                        val waiter = _unsafe.next().safe
                        if _unsafe.version() != v0 then (): Unit < Async
                        else waiter.use(_ => ())
                    end if
                }
            Sync.defer {
                val image                              = Signal.lastProjection(proj)
                def hold(v0: Long, b: B): Unit < Async =
                    nextSince(v0).andThen(Sync.Unsafe.defer {
                        val v1 = _unsafe.version()
                        if image(_unsafe.get()) == b then hold(v1, b) else (): Unit < Async
                    })
                def loop(last: Maybe[B]): Unit < (S & Async) =
                    Sync.Unsafe.defer {
                        val v0 = _unsafe.version()
                        val b  = image(_unsafe.get())
                        if last.exists(_ == b) then nextSince(v0).andThen(loop(last))
                        else Scope.run(g(b).andThen(hold(v0, b))).andThen(loop(Present(b)))
                    }
                loop(baseline)
            }
        end observeProjected

        /** Retrieves the current value of the reference.
          *
          * This is a convenience method equivalent to `current` but with a more familiar name for reference types.
          *
          * @return
          *   The current value
          */
        def get(using Frame): A < Sync = use(identity)

        /** Retrieves and transforms the current value of the reference.
          *
          * This is a convenience method that provides synchronous access to the reference's current value while applying a transformation
          * function. It's equivalent to `currentWith` but with a more familiar name for reference types.
          *
          * @param f
          *   The transformation function to apply to the current value
          * @return
          *   The transformed value wrapped in combined effects S & Sync
          */
        inline def use[B, S](inline f: A => B < S)(using Frame): B < (S & Sync) = Sync.Unsafe.defer(f(_unsafe.get()))

        /** A one-way view of this reference: the same values through the same delivery protocol, but not a `SignalRef`.
          *
          * A consumer that decides from the runtime class whether a binding is two-way treats a `SignalRef` as "write back to me", and a
          * type ascription cannot say otherwise, since `ref: Signal[A]` leaves the runtime class untouched. This is how a caller hands out
          * the values of a reference while keeping the writes to itself.
          *
          * @return
          *   A `Signal[A]` forwarding every read and observation to this reference
          */
        def readOnly: Signal[A] = ReadOnly(this)

        /** Sets the reference to a new value.
          *
          * Updates the reference's value and notifies any observers if the value has changed. The previous value is returned.
          *
          * @param value
          *   The new value to set
          */
        def set(value: A)(using Frame): Unit < Sync = Sync.Unsafe.defer(_unsafe.set(value))

        /** Updates the reference's value and returns the previous value.
          *
          * @param value
          *   The new value to set
          * @return
          *   The previous value
          */
        def getAndSet(value: A)(using Frame): A < Sync =
            Sync.Unsafe.defer(_unsafe.getAndSet(value))

        /** Atomically sets the value to the given updated value if the current value equals the expected value.
          *
          * @param curr
          *   The expected current value
          * @param next
          *   The new value to set if the current value matches
          * @return
          *   True if successful, false otherwise
          */
        def compareAndSet(curr: A, next: A)(using Frame): Boolean < Sync =
            Sync.Unsafe.defer(_unsafe.compareAndSet(curr, next))

        /** Atomically updates the current value using the provided function and returns the previous value.
          *
          * @param f
          *   The function to transform the current value
          * @return
          *   The previous value
          */
        def getAndUpdate(f: A => A)(using Frame): A < Sync =
            Sync.Unsafe.defer(_unsafe.getAndUpdate(f))

        /** Atomically updates the current value using the provided function and returns the new value.
          *
          * @param f
          *   The function to transform the current value
          * @return
          *   The new value
          */
        def updateAndGet(f: A => A)(using Frame): A < Sync =
            Sync.Unsafe.defer(_unsafe.updateAndGet(f))

        def waiters(using Frame): Int < Sync =
            Sync.Unsafe.defer(_unsafe.waiters())

        def unsafe: SignalRef.Unsafe[A] = _unsafe
    end SignalRef

    object SignalRef:

        /** WARNING: Low-level API meant for integrations, libraries, and performance-sensitive code. See AllowUnsafe for more details.
          *
          * The implementation uses two atomic references to manage state:
          *
          *   - An `AtomicRef[A]` storing the current value
          *   - An `AtomicRef[Promise]` managing change notifications
          *
          * Methods like `set`, `getAndSet`, and `compareAndSet` update the current value atomically and check if it has actually changed
          * using `CanEqual`. When values differ, `onUpdate` is triggered: the current promise is atomically replaced with a new
          * uninterruptible promise, then completed with the new value. This ensures the next promise is always ready before notifying of
          * changes.
          *
          * Promises are uninterruptible to prevent interrupt propagation between observers: if one observer is interrupted, the
          * interruption won't affect other observers waiting on the same signal.
          */
        final class Unsafe[A] private (
            currentRef: AtomicRef.Unsafe[A],
            nextPromise: AtomicRef.Unsafe[Promise.Unsafe[A, Any]],
            versionRef: AtomicLong.Unsafe
        )(using CanEqual[A, A]):

            def get()(using AllowUnsafe): A = currentRef.get()

            /** Monotonic change counter, incremented once per distinct-value update. `SignalRef.observeProjected` uses it
              * to validate that no write landed between reading `current` and capturing the next-change promise.
              */
            def version()(using AllowUnsafe): Long = versionRef.get()

            def set(value: A)(using AllowUnsafe): Unit =
                discard(getAndSet(value))

            def getAndSet(value: A)(using AllowUnsafe): A =
                val prev = currentRef.getAndSet(value)
                if prev != value then
                    onUpdate(value)
                prev
            end getAndSet

            def compareAndSet(curr: A, next: A)(using AllowUnsafe): Boolean =
                val r = currentRef.compareAndSet(curr, next)
                if r && curr != next then
                    discard(onUpdate(next))
                r
            end compareAndSet

            def getAndUpdate(f: A => A)(using AllowUnsafe): A =
                @tailrec
                def loop(): A =
                    val prev: A = currentRef.get()
                    val next: A = f(prev)
                    if prev == next then prev
                    else if currentRef.compareAndSet(prev, next) then
                        discard(onUpdate(next))
                        prev
                    else
                        loop()
                    end if
                end loop
                loop()
            end getAndUpdate

            def updateAndGet(f: A => A)(using AllowUnsafe): A =
                @tailrec
                def loop(): A =
                    val prev: A = currentRef.get()
                    val next: A = f(prev)
                    if prev == next then next
                    else if currentRef.compareAndSet(prev, next) then
                        discard(onUpdate(next))
                        next
                    else
                        loop()
                    end if
                end loop
                loop()
            end updateAndGet

            def next()(using AllowUnsafe): Fiber.Unsafe[A, Any] =
                nextPromise.get()

            private def onUpdate(value: A)(using AllowUnsafe): Unit =
                // The version MUST be bumped before the promise swap. Writer order is: value write (in the
                // caller), version increment, promise swap+complete. `SignalRef.observeProjected`'s register/validate
                // protocol relies on exactly this order for losslessness (see the override).
                discard(versionRef.incrementAndGet())
                nextPromise.getAndSet(Promise.Unsafe.initUninterruptible())
                    .completeDiscard(Result.succeed(value))
            end onUpdate

            def waiters()(using AllowUnsafe): Int = nextPromise.get().waiters()

            // Callback subscribers. All of them share one registration on the next-change promise, so a write costs one
            // waiter and one re-registration for the whole fan-out, and the writing thread walks an array of plain calls.
            // Subscribing is rare and notifying is hot, so the list is copy-on-write.

            // The dispatch walk runs on whatever thread completed the promise, with no user Frame in reach;
            // it is only used to attribute the log line of a failing subscriber.
            private given dispatchFrame: Frame = Frame.internal

            private val subs =
                new java.util.concurrent.atomic.AtomicReference[Chunk[Sub[A, ?]]](Chunk.empty)
            // The promise `fire` is registered on, or null. Every registration is the same `fire`, so `remove(fire)` drops one of
            // them by reference, and its `true` means that registration will never run.
            private val armed =
                new java.util.concurrent.atomic.AtomicReference[IOPromise[Any, Any]](null)
            // Live subscribers, tracked separately from `subs.size` because retired ones linger in the list
            // until a sweep. Reaching zero is what releases the shared registration.
            private val liveSubs = new java.util.concurrent.atomic.AtomicInteger(0)

            /** Register `sub`, deliver the current value to it, and make sure a waiter is armed. */
            private[kyo] def subscribe(sub: Sub[A, ?])(using AllowUnsafe): Unit =
                @tailrec def add(): Unit =
                    val cur = subs.get()
                    if !subs.compareAndSet(cur, cur.append(sub)) then add()
                discard(liveSubs.incrementAndGet())
                add()
                deliverTo(sub, currentRef.get())
                pump(dispatchFirst = false)
            end subscribe

            /** Drops `sub`; when the last one leaves, releases the shared registration. The next-change promise is masked and outlives its
              * subscribers, so leaving the waiter behind would retain every callback.
              *
              * Retiring is a flag flip, not a list rebuild: filtering the copy-on-write list per removal would make tearing down n
              * subscribers quadratic. Dead entries are skipped by the dispatch walk and swept out in one pass once they outnumber the live
              * ones; when everything goes at once, the list is dropped wholesale and needs no sweep.
              *
              * A `subscribe` can race the last leave: it counts itself live before it appends and arms. So the list is dropped only by a
              * CAS against the list read while nobody was live, and after the waiter is removed the count is read again: a subscriber
              * that arrived in between gets the current value and its waiter back from a fresh pump.
              */
            private[kyo] def unsubscribe(sub: Sub[A, ?])(using AllowUnsafe): Unit =
                if sub.kill() then
                    if liveSubs.decrementAndGet() == 0 then releaseIfIdle()
                    else sweepIfCluttered()
            end unsubscribe

            @tailrec private def releaseIfIdle()(using AllowUnsafe): Unit =
                val cur = subs.get()
                if liveSubs.get() == 0 then
                    if subs.compareAndSet(cur, Chunk.empty) then
                        val p = armed.getAndSet(null)
                        if p ne null then discard(p.remove(fire))
                        // Dispatching first: a write may have completed the promise after its waiter was removed here.
                        if liveSubs.get() > 0 then pump(dispatchFirst = true)
                    else releaseIfIdle()
                end if
            end releaseIfIdle

            private def sweepIfCluttered(): Unit =
                val cur = subs.get()
                if cur.size > 2 * liveSubs.get() + 16 then
                    discard(subs.compareAndSet(cur, cur.filter(_.isAlive)))
            end sweepIfCluttered

            // A failing sink must not silence the others sharing the dispatch walk, nor abort whoever is setting up a
            // subscription when it throws on the initial delivery.
            private def deliverTo(sub: Sub[A, ?], value: A)(using AllowUnsafe): Unit =
                try sub.deliver(value)
                catch case ex if NonFatal(ex) => Log.live.unsafe.error("signal subscriber failed", ex)

            private def dispatch()(using AllowUnsafe): Unit =
                val value = currentRef.get()
                subs.get().foreach(sub => if sub.isAlive then deliverTo(sub, value))

            /** Delivers and re-arms until the signal holds still, then leaves exactly one waiter behind.
              *
              * The version is read once per round, before dispatching, and checked again afterwards. That order makes the path lossless in
              * both ways a write can be dropped: one racing the registration (the case `nextSince` handles), and one issued from inside a
              * delivery, where the sink calls `set` and completes a promise nobody is registered on yet because the re-arm has not happened.
              * The second read catches both and runs another round; `Sub.deliver` deduplicates, so a redundant round costs a projection.
              */
            private def pump(dispatchFirst: Boolean)(using AllowUnsafe): Unit =
                var deliver = dispatchFirst
                var done    = false
                while !done do
                    if liveSubs.get() == 0 then done = true
                    else
                        val v0 = versionRef.get()
                        if deliver then dispatch()
                        val p = nextPromise.get().asInstanceOf[IOPromise[Any, Any]]
                        if versionRef.get() != v0 then deliver = true
                        else
                            p.onComplete(fire)
                            // The CAS loses when `p` had already completed, so `onComplete` ran `fire` inline and `fire` pumped
                            // (nothing is left on `p` to remove), or when a concurrent pump armed first, so this registration
                            // is a duplicate.
                            if !armed.compareAndSet(null, p) then discard(p.remove(fire))
                            done = true
                        end if
                    end if
                end while
            end pump

            // An error completion must not re-arm: the promise is only swapped on a value update (see onUpdate), so
            // re-arming would capture the same completed promise, whose registration fires inline, and recurse without bound.
            private val fire: Result[Any, Any] => Any = r =>
                import AllowUnsafe.embrace.danger
                armed.set(null)
                if r.isSuccess then pump(dispatchFirst = true)
            end fire

            def safe: SignalRef[A] = SignalRef(this)

        end Unsafe

        object Unsafe:

            /** WARNING: Low-level API meant for integrations, libraries, and performance-sensitive code. See AllowUnsafe for more details.
              */
            def init[A](initial: A)(using AllowUnsafe, CanEqual[A, A]): Unsafe[A] =
                Unsafe(
                    AtomicRef.Unsafe.init(initial),
                    AtomicRef.Unsafe.init(Promise.Unsafe.initUninterruptible()),
                    AtomicLong.Unsafe.init(0L)
                )
        end Unsafe

    end SignalRef
end Signal
