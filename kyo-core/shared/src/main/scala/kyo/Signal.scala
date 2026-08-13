package kyo

import kyo.scheduler.IOPromise
import scala.annotation.implicitNotFound
import scala.annotation.nowarn
import scala.annotation.tailrec
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
  *   - `Signal.initRef[A]`: creates a mutable `Signal.Ref[A]` initialized with a starting value
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
      * still-current value's `Scope` down on an idle timer. Delivery comes in two tiers. A [[SignalRef]] (and a `map` chain rooted in one)
      * observes exactly: a version-validated register/validate/await protocol makes every change wake the observer immediately, with no
      * repair timer armed at all (see the `SignalRef.observe` override). Combinator-derived signals (`zip`, `combineLatest`, `switchMap`,
      * `zipAll`, `combineLatestAll`, custom `initRaw`) use the repairing loop: it reads `current`, runs `f`, then re-arms a
      * `nextWith`/`Async.sleep(repairInterval)` race that holds the value's `Scope` open until the next change. A write that lands in the
      * window between reading `current` and registering `nextWith` is missed by the immediate wakeup and reconciled when the repair timer
      * next fires and re-reads `current` (the hold re-waits on a still-current value, so a repair timer never closes its `Scope`). So the
      * final value is always delivered: immediately in the common case, and within `repairInterval` in the worst case when a write races
      * that window on a derived signal. Correctness never depends on `repairInterval` ; only the worst-case reconciliation latency does.
      * This variant uses [[Signal.defaultRepairInterval]].
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
      * The loop is the repairing form: it tracks the last observed value and, while the current value is unchanged, re-arms a
      * `nextWith`/`Async.sleep(repairInterval)` race so a missed wakeup is reconciled within `repairInterval` WITHOUT tearing the still-current
      * value's `Scope` down. The hold loops until `current` actually differs, so a repair timer firing on a still-current value re-waits and
      * keeps the per-value `Scope` open.
      *
      * @param repairInterval
      *   How often a parked observation re-reads `current` to reconcile a missed wakeup on the repair path; ignored by exact observers
      *   (`SignalRef` and `map` chains rooted in one), which never miss a wakeup
      * @param f
      *   The per-value setup, run inside a fresh `Scope`
      */
    final def observe[S](repairInterval: Duration)(f: A => Unit < (S & Async & Scope))(using Frame): Unit < (S & Async) =
        observe(Absent, repairInterval)(f)

    /** Like [[observe]] but seeded: `f` is skipped while the current value still equals `baseline`.
      *
      * With `Absent` this is exactly [[observe]]. With `Present(v)` the loop treats `v` as the last observed value: the initial emission is
      * skipped when the current value still equals it, and the first differing value is delivered as usual. This is the primitive behind
      * [[observeChanges]] and behind callers that already processed the current value (e.g. painted it) and only want what changed since.
      *
      * This is the overridable observation primitive: [[SignalRef]] replaces the repairing loop with an exact register/validate/await
      * protocol (see there), and `map` delegates to its source's loop.
      *
      * @param baseline
      *   The value already processed by the caller: `f` is not run while `current` still equals it
      * @param repairInterval
      *   How often a parked observation re-reads `current` to reconcile a missed wakeup on the repair path
      * @param f
      *   The per-value setup, run inside a fresh `Scope`
      */
    def observe[S](baseline: Maybe[A], repairInterval: Duration)(f: A => Unit < (S & Async & Scope))(using Frame): Unit < (S & Async) =
        // Repairing default. Each value runs inside a fresh `Scope.run`; the inner `holdUntilChanged` loops until `current`
        // differs from the value `f` set up, so an idle repair timer NEVER closes a still-current value's scope. The scope
        // closes (releasing what `f` forked) only when the value actually changes; then the outer loop re-reads `current`.
        def holdUntilChanged(cur: A): Unit < (S & Async) =
            Async.race(Seq(nextWith(_ => ()), Async.sleep(repairInterval))).andThen {
                currentWith(c => if c == cur then holdUntilChanged(cur) else (): Unit < (S & Async))
            }
        def loop(last: Maybe[A]): Unit < (S & Async) =
            currentWith { cur =>
                if last.exists(_ == cur) then
                    Async.race(Seq(nextWith(_ => ()), Async.sleep(repairInterval))).andThen(loop(last))
                else
                    Scope.run(f(cur).andThen(holdUntilChanged(cur))).andThen(loop(Present(cur)))
            }
        loop(baseline)
    end observe

    /** Runs `f` for every change AFTER subscription, skipping the initial current value.
      *
      * Identical to [[observe]] except that the value current at subscription time is not delivered: the first emission is the first value
      * that differs from it. Same per-value [[Scope]] semantics and the same delivery tiers. This variant uses
      * [[Signal.defaultRepairInterval]].
      */
    /** Observation of a projected view of this signal, deduplicated on the PROJECTION rather than on this signal's own values.
      *
      * This is what a derived signal's observation needs. `map` delegates to its source's loop, and that loop compares SOURCE values, so
      * every observer of a derived signal wakes AND delivers on every source change even when its own image is unchanged: a thousand rows
      * deriving `selected.map(_ == row.id)` from one selection signal each run a full per-value `Scope` teardown and setup for a value that
      * did not move. Comparing images instead confines that to the rows whose image actually changed.
      *
      * The per-value `Scope` follows the IMAGE: it opens when the image changes and stays open while the image holds, so a source change
      * that leaves the image alone no longer releases what `g` set up for it.
      *
      * Structurally the repairing loop of [[observe]], with `proj(cur)` where that one has `cur`; [[SignalRef]] overrides it the same way,
      * so a projected observation of a ref keeps the exact protocol. A projection over an already-derived signal (a `map` of a `map`) falls
      * back to this repairing loop, exactly as an ordinary observation of one does.
      *
      * @param proj
      *   The view to observe; called on each source value and compared for equality against the last delivered image
      * @param baseline
      *   The image already processed by the caller: `g` is not run while the current image still equals it
      * @param repairInterval
      *   How often a parked observation re-reads `current` to reconcile a missed wakeup on the repair path
      * @param g
      *   The per-image setup, run inside a fresh `Scope`
      */
    /** PROTOTYPE — fiber-free observation for trivial, non-suspending sinks.
      *
      * `cb` is invoked on the WRITER's own stack, inside the `set` that changed the projected image
      * (`IOPromise.complete` → `flush` → `Pending.run`), instead of waking a fiber through the scheduler.
      * That removes ~3.5 macrotasks per observer per notification, and moves the image-equality check
      * BEFORE the hop instead of after it — which is what makes a thousand observers of one signal cost a
      * thousand comparisons rather than a thousand scheduled tasks.
      *
      * Returns the release function. `Absent` means this signal has no raw fast path: only chains rooted in
      * a [[SignalRef]] (or a constant) provide one, so the caller must keep its fiber-based path for
      * everything else. The caller MUST call the release on teardown — the registration sits on a masked
      * promise, which deliberately outlives its subscribers.
      *
      * `cb` must not suspend and must stay trivial: it runs inside somebody else's `set`.
      */
    private[kyo] def unsafeObserveProjected[B](proj: A => B, baseline: Maybe[B], cb: B => Unit)(
        using
        CanEqual[B, B],
        AllowUnsafe,
        Frame
    ): Maybe[() => Unit] = Absent

    def observeProjected[B, S](proj: A => B, baseline: Maybe[B], repairInterval: Duration)(
        g: B => Unit < (S & Async & Scope)
    )(using CanEqual[B, B], Frame): Unit < (S & Async) =
        def await: Unit < Async =
            Async.race(Seq(nextWith(_ => ()), Async.sleep(repairInterval))).unit
        def holdUntilChanged(b: B): Unit < (S & Async) =
            await.andThen(currentWith(c => if proj(c) == b then holdUntilChanged(b) else (): Unit < (S & Async)))
        def loop(last: Maybe[B]): Unit < (S & Async) =
            currentWith { cur =>
                val b = proj(cur)
                if last.exists(_ == b) then await.andThen(loop(last))
                else Scope.run(g(b).andThen(holdUntilChanged(b))).andThen(loop(Present(b)))
            }
        loop(baseline)
    end observeProjected

    final def observeChanges[S](f: A => Unit < (S & Async & Scope))(using Frame): Unit < (S & Async) =
        observeChanges(Signal.defaultRepairInterval)(f)

    /** Like [[observeChanges]] but with an explicit reconciliation interval. */
    final def observeChanges[S](repairInterval: Duration)(f: A => Unit < (S & Async & Scope))(using Frame): Unit < (S & Async) =
        currentWith(c => observe(Present(c), repairInterval)(f))

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
            // Observed through the source's projected loop, which compares IMAGES. Delegating to the source's
            // plain `observe` compared source values, so a source change delivered to every derived observer
            // even when its own image held still, and re-created each one's per-value Scope for nothing. The
            // baseline is now used directly: it is already in image space, so no translation is needed.
            [S] => (baseline, ri, g) => self.observeProjected[B, S](f, baseline, ri)(g),
            // Projections compose down the chain rather than stopping at this map: a projected observation
            // of `x.map(f)` becomes one of `x` through `proj ∘ f`. That is what keeps a chain rooted in a
            // SignalRef on that ref's exact protocol — without it the first map answers from the trait's
            // repairing loop, and a two-step chain silently reacquires the repair timer.
            [C, S] =>
                (proj, baseline, ri, g, canEqualC) =>
                    self.observeProjected[C, S](a => proj(f(a)), baseline, ri)(g)(using canEqualC, frame),
            // Same composition for the raw path: `x.map(f)` observed unsafely becomes `x` observed through
            // `proj ∘ f`, so a chain rooted in a SignalRef keeps the callback path instead of losing it at
            // the first map.
            [C] =>
                (proj, baseline, cb, canEqualC, allow) =>
                    self.unsafeObserveProjected[C](a => proj(f(a)), baseline, cb)(using canEqualC, allow, frame)
        )

    /** This signal's CHANGES, carrying `b` in place of its own values.
      *
      * Deliberately NOT `map(_ => b)`. Observation deduplicates on the IMAGE, so a constant image collapses to a
      * single delivery however often the source moves — correct for a projection, since the observer's view
      * genuinely did not change, and wrong for a caller whose emitted value is a stable HANDLE and whose content
      * is rebuilt from the source at delivery time. Such a caller needs the source's own change detection, which
      * is what this keeps: every arm below observes the source on ITS values and hands over the constant.
      *
      * The baseline is in `b`'s space and `b` is the only value there, so it can only mean "the caller has
      * already processed one delivery": that is honoured by seeding the SOURCE observation with the source's
      * current value, after which every source change delivers again.
      *
      * The raw callback path deliberately declines (`Absent`, the trait default): its subscriber deduplicates on
      * the image too, which is the same trap one layer down.
      */
    private[kyo] def changesTo[B](b: B)(using canEqualB: CanEqual[B, B], frame: Frame): Signal[B] =
        def observeSource[S](skipFirst: Boolean, ri: Duration, deliver: Unit < (S & Async & Scope))(
            using Frame
        ): Unit < (S & Async) =
            if skipFirst then self.currentWith(a0 => self.observe[S](Present(a0), ri)(_ => deliver))
            else self.observe[S](Absent, ri)(_ => deliver)
        Signal._initRawF(
            [C, S] => g => self.currentWith(_ => g(b)),
            [C, S] => g => self.nextWith(_ => g(b)),
            [S] => (baseline, ri, g) => observeSource[S](baseline.exists(_ == b), ri, g(b)),
            [C, S] =>
                (proj, baseline, ri, g, canEqualC) =>
                    given CanEqual[C, C] = canEqualC
                    val image            = proj(b)
                    observeSource[S](baseline.exists(_ == image), ri, g(image))
            ,
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
      * await/re-read window applies here (a write landing between the wakeup and the re-read is coalesced); observation over it is
      * reconciled within the repair interval.
      *
      * @param f
      *   The function that produces an inner signal from the current value
      * @return
      *   A new signal that tracks the current inner signal
      */
    @nowarn("msg=anonymous")
    inline def switchMap[B](inline f: A => Signal[B])(using CanEqual[B, B], Frame): Signal[B] =
        Signal.initRaw(
            currentWith = [C, S] => g => self.currentWith(a => f(a).currentWith(g)),
            nextWith = [C, S] =>
                g =>
                    self.currentWith { a =>
                        val inner = f(a)
                        Signal.awaitAny(Seq(self, inner))
                            .andThen(self.currentWith { a2 =>
                                (if a2 == a then inner else f(a2)).currentWith(g)
                            })
                }
        )

    /** Pairs this signal with another, waiting for both to change before emitting.
      *
      * @param other
      *   The signal to pair with
      * @return
      *   A signal of pairs that updates only when both inputs have changed since the last emit
      */
    @nowarn("msg=anonymous")
    inline def zip[B](other: Signal[B])(using CanEqual[(A, B), (A, B)], Frame): Signal[(A, B)] =
        Signal.initRaw(
            currentWith = [C, S] => g => self.currentWith(a => other.currentWith(b => g((a, b)))),
            nextWith = [C, S] => g => Async.zip(self.next, other.next).andThen(self.currentWith(a => other.currentWith(b => g((a, b)))))
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
        Signal.initRaw(
            currentWith = [C, S] => g => self.currentWith(a => other.currentWith(b => g((a, b)))),
            nextWith = [C, S] => g => Signal.awaitAny(Seq(self, other)).andThen(self.currentWith(a => other.currentWith(b => g((a, b)))))
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
      * The value current at subscription time is NOT emitted: the first element is the first value that differs from it. Built on
      * [[observeChanges]], so it inherits its delivery tiers: exact on a [[SignalRef]] (and `map` chains rooted in one), reconciled within
      * `repairInterval` on combinator-derived signals. Rapid changes may still coalesce (the stream is level-based, not a queue). This
      * variant uses [[Signal.defaultRepairInterval]].
      *
      * @return
      *   A stream that emits only when the value changes, skipping the subscription-time value
      */
    final def streamChanges(using Frame, Tag[Emit[Chunk[A]]]): Stream[A, Async] =
        streamChanges(Signal.defaultRepairInterval)

    /** Like [[streamChanges]] but with an explicit reconciliation interval. */
    final def streamChanges(repairInterval: Duration)(using Frame, Tag[Emit[Chunk[A]]]): Stream[A, Async] =
        Stream(observeChanges[Emit[Chunk[A]]](repairInterval)(a => Emit.value(Chunk(a))))

end Signal

export Signal.SignalRef

object Signal:

    /** Default reconciliation interval used by [[Signal.observe]] when none is given.
      *
      * It bounds how soon a missed wakeup is reconciled by re-reading `current`: a write that races the observation's
      * read/register window is delivered within this interval. Real changes are otherwise immediate, so this can be
      * generous; it exists to bound that rare race, not to drive normal updates. Exact observers ([[SignalRef]] and
      * `map` chains rooted in one) never miss a wakeup and ignore it entirely, arming no timer at all.
      */
    val defaultRepairInterval: Duration = 1.second

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
                initRaw(
                    currentWith = [B, S] => f => Kyo.foreach(sigs)(_.current).map(f),
                    nextWith = [B, S] => f => Async.foreachDiscard(sigs, sigs.size)(_.next).andThen(Kyo.foreach(sigs)(_.current).map(f))
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
                initRaw(
                    currentWith = [C, S] => g => Kyo.foreach(sigs)(_.current).map(g),
                    nextWith = [C, S] => g => awaitAny(sigs).andThen(Kyo.foreach(sigs)(_.current).map(g))
                )

    private inline val missingCanEqual =
        "Cannot create Signal because values of type '${A}' cannot be compared for equality to detect changes. Make sure there is a 'CanEqual[${A}, ${A}]' instance available."

    /** Creates a new mutable signal reference with an initial value.
      *
      * This method initializes a new `Signal.Ref[A]` that can be modified over time. The reference starts with the provided initial value
      * and can be updated using methods like `set`, `getAndSet`, etc.
      *
      * @param initial
      *   The starting value for the signal reference
      * @return
      *   A new mutable `Signal.Ref[A]`
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
      * This method initializes a new `Signal.Ref[A]` that can be modified over time, and immediately applies a transformation function to
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
      * This method creates a signal that always returns the same value. Unlike `Signal.Ref`, this signal cannot be modified after creation.
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
            // A constant cannot change, so there is nothing for a reconciliation timer to reconcile. Left to
            // the trait's repairing loop it would arm a `nextWith`/`sleep` race per interval forever, and
            // since `nextWith` never completes the sleep would win every time, re-read the same value and
            // re-arm: one perpetual timer per observer, for a signal that can never move. Deliver once and
            // hold the scope instead; interrupting the observation still closes it.
            [S] =>
                (baseline, repairInterval, f) =>
                    discard(repairInterval)
                    if baseline.exists(_ == value) then Async.never[Unit]
                    else Scope.run(f(value).andThen(Async.never[Unit]))
            ,
            [C, S] =>
                (proj, baseline, repairInterval, g, canEqualC) =>
                    given CanEqual[C, C] = canEqualC
                    discard(repairInterval)
                    val image = proj(value)
                    if baseline.exists(_ == image) then Async.never[Unit]
                    else Scope.run(g(image).andThen(Async.never[Unit]))
            ,
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
      * `Signal.Ref`, this signal cannot be modified after creation.
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

    // Like _initRaw but also supplies `observe`, letting a structural combinator delegate observation to its source's
    // `observe` loop (applying its transform to each value) rather than running a second repair loop over its own
    // `currentWith`/`nextWith`. `map` uses this so a `map`-over-leaf chain observes through one loop rooted at the leaf.
    @nowarn("msg=anonymous")
    private inline def _initRawF[A](
        inline _currentWith: [B, S] => (A => B < S) => B < (S & Sync),
        inline _nextWith: [B, S] => (A => B < S) => B < (S & Async),
        inline _observe: [S] => (Maybe[A], Duration, A => Unit < (S & Async & Scope)) => Unit < (S & Async),
        inline _observeProjected: [C, S] => (
            A => C,
            Maybe[C],
            Duration,
            C => Unit < (S & Async & Scope),
            CanEqual[C, C]
        ) => Unit < (S & Async),
        // PROTOTYPE: composed down the same way as _observeProjected, so a `map`-over-leaf chain keeps the
        // leaf's raw fast path instead of falling back to the trait's Absent.
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
            override def observe[S](baseline: Maybe[A], repairInterval: Duration)(f: A => Unit < (S & Async & Scope))(using
                frame: Frame
            ): Unit < (S & Async) =
                _observe(baseline, repairInterval, f)
            override def observeProjected[C, S](proj: A => C, baseline: Maybe[C], repairInterval: Duration)(
                g: C => Unit < (S & Async & Scope)
            )(using canEqualC: CanEqual[C, C], frame: Frame): Unit < (S & Async) =
                _observeProjected(proj, baseline, repairInterval, g, canEqualC)
            override private[kyo] def unsafeObserveProjected[C](proj: A => C, baseline: Maybe[C], cb: C => Unit)(
                using
                canEqualC: CanEqual[C, C],
                allow: AllowUnsafe,
                frame: Frame
            ): Maybe[() => Unit] =
                _unsafeObserveProjected(proj, baseline, cb, canEqualC, allow)
        end new
    end _initRawF

    /** PROTOTYPE — one callback subscriber of a [[SignalRef]], holding the projection it observes through and
      * the last image it was told about.
      *
      * `last` is an atomic cell rather than a plain field because two dispatch walks can overlap: one from a
      * fired waiter and one from the re-arm loop that noticed a version change. Taking the cell with
      * `getAndSet` makes exactly one of them call `cb`, so an overlap costs a redundant projection instead of
      * a duplicated side effect.
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

        def nextWith[B, S](f: A => B < S)(using Frame) = Sync.Unsafe.defer(unsafe.next().safe.use(f))

        // Exact, lossless observation via a version-validated register/validate/await protocol.
        //
        // Writer order (see Unsafe.onUpdate): value write, version increment, promise swap+complete. The observer
        // reads the version BEFORE the value, runs `f`, and re-arms through `nextSince(v0)`: capture the
        // next-change promise, re-check the version, and only park when it is unchanged.
        //   - If the validate step sees a newer version, `nextSince` returns immediately and the re-read observes
        //     the new value (the value write happens before the increment).
        //   - Otherwise the capture happened before that write's promise swap, and the swap completes exactly the
        //     captured promise. Either way no change is stranded, with no repair timer armed at all (so an idle
        //     parked observer costs nothing and holds exactly one waiter).
        //   - Version-before-value is load-bearing: reading the value first could pair a fresh value with a stale
        //     version and validate cleanly against a completion nobody held.
        // Coalescing stays allowed (observation is level-based): a change-and-revert during `f` wakes the
        // observer, re-reads an unchanged value, and re-arms.
        //
        // Scala Native note: earlier revisions of this override were blamed for kyo-browser native suite
        // SIGSEGVs ("miscompiles", "heap corruption"). Root-caused 2026-07-31: the crashes never involved
        // Signal at all (the class is unreachable from that binary; zero Signal symbols in the linked
        // artifact). The real fault is a Scala Native 0.5.12 libunwind null read (CFI_Parser::decodeFDE
        // during Throwable.fillInStackTrace) on certain deep continuation stacks, triggered whenever an
        // exception is constructed there, and layout-sensitive enough that unrelated source changes (like
        // adding a method here) flipped it on and off. The trigger on kyo's side was an exception thrown
        // per non-numeric token in SchemaSerializer.DiscriminatorReader.matchField, since made
        // exception-free. If the kyo-browser native suite segfaults after touching this file, suspect
        // exception construction on deep stacks (si_addr=(nil), libunwind frames in the core), not this
        // code.
        /** Await a change since version `v0`. Shared by the exact [[observe]] and [[observeProjected]] loops: both need
          * "nothing happened since I read this version", and duplicating the masked-promise handling below would let the
          * two drift apart.
          */
        private def nextSince(v0: Long)(using Frame): Unit < Async =
            Sync.Unsafe.defer {
                if _unsafe.version() != v0 then ()
                else
                    // Parks DIRECTLY on the masked next-promise. This used to go through a one-element
                    // `Async.race`, because a fiber parked on a masked promise was not released by its own
                    // interrupt (masking blocks the interrupt cascade that is otherwise the only thing that
                    // flushes the waiter chain), and parking behind a fresh unmasked result promise restored
                    // interruptibility. IOTask now cancels its own registration in `onComplete` and schedules
                    // itself, so the interrupt releases the fiber with no wrapper — see WaiterLifecycleTest
                    // B1. The wrapper was not free: per re-arm it cost a Race promise, an IOTask, a copied
                    // trace array and two waiter registrations, on the hot path of every observer.
                    val waiter = _unsafe.next().safe
                    if _unsafe.version() != v0 then (): Unit < Async
                    else waiter.use(_ => ())
                end if
            }

        /** PROTOTYPE — the callback twin of [[observeProjected]]: delivery happens inside the writer's own
          * `set`, with no scheduler hop.
          *
          * All subscribers of one ref share a SINGLE registration on the next-promise (see
          * [[Unsafe.subscribe]]). That matters as much as the missing hop: a thousand rows observing one
          * selection used to mean a thousand waiter allocations and a thousand CAS retries on the same
          * chain per write, all of them on the writing thread. Now a write fires one waiter and walks an
          * array.
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

        /** The exact protocol of [[observe]], comparing images instead of values. See [[Signal.observeProjected]] for why a
          * derived signal must deduplicate on its own image: without this, one selection change delivers to every row.
          */
        override def observeProjected[B, S](proj: A => B, baseline: Maybe[B], repairInterval: Duration)(
            g: B => Unit < (S & Async & Scope)
        )(using CanEqual[B, B], Frame): Unit < (S & Async) =
            // Unused here for the same reason as in `observe`: delivery is exact.
            discard(repairInterval)
            def hold(v0: Long, b: B): Unit < Async =
                nextSince(v0).andThen(Sync.Unsafe.defer {
                    val v1 = _unsafe.version()
                    if proj(_unsafe.get()) == b then hold(v1, b) else (): Unit < Async
                })
            def loop(last: Maybe[B]): Unit < (S & Async) =
                Sync.Unsafe.defer {
                    val v0 = _unsafe.version()
                    val b  = proj(_unsafe.get())
                    if last.exists(_ == b) then nextSince(v0).andThen(loop(last))
                    else Scope.run(g(b).andThen(hold(v0, b))).andThen(loop(Present(b)))
                }
            loop(baseline)
        end observeProjected

        override def observe[S](baseline: Maybe[A], repairInterval: Duration)(f: A => Unit < (S & Async & Scope))(using
            Frame
        ): Unit < (S & Async) =
            // The repair interval is unused here: delivery is exact. The parameter exists for signals that can
            // miss wakeups (combinator-derived ones run the trait's repairing loop).
            discard(repairInterval)
            def hold(v0: Long, cur: A): Unit < Async =
                nextSince(v0).andThen(Sync.Unsafe.defer {
                    val v1 = _unsafe.version()
                    if _unsafe.get() == cur then hold(v1, cur) else (): Unit < Async
                })
            def loop(last: Maybe[A]): Unit < (S & Async) =
                Sync.Unsafe.defer {
                    val v0  = _unsafe.version()
                    val cur = _unsafe.get()
                    if last.exists(_ == cur) then nextSince(v0).andThen(loop(last))
                    else Scope.run(f(cur).andThen(hold(v0, cur))).andThen(loop(Present(cur)))
                }
            loop(baseline)
        end observe

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
          * using `CanEqual`. When values differ, `onUpdate` is triggered: the current promise is atomically replaced with a new masked
          * promise, then completed with the new value. This ensures the next promise is always ready before notifying of changes.
          *
          * Promises are masked to prevent interrupt propagation between observers - if one observer is interrupted, the interruption won't
          * affect other observers waiting on the same signal. This ensures notification chains remain independent.
          */
        final class Unsafe[A] private (
            currentRef: AtomicRef.Unsafe[A],
            nextPromise: AtomicRef.Unsafe[Promise.Unsafe[A, Any]],
            versionRef: AtomicLong.Unsafe
        )(using CanEqual[A, A]):

            def get()(using AllowUnsafe): A = currentRef.get()

            /** Monotonic change counter, incremented once per distinct-value update. `SignalRef.observe` uses it
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
                // caller), version increment, promise swap+complete. `SignalRef.observe`'s register/validate
                // protocol relies on exactly this order for losslessness (see the override).
                discard(versionRef.incrementAndGet())
                nextPromise.getAndSet(Promise.Unsafe.initMasked())
                    .completeDiscard(Result.succeed(value))
            end onUpdate

            def waiters()(using AllowUnsafe): Int = nextPromise.get().waiters()

            // ---- PROTOTYPE: callback subscribers -------------------------------------------------
            //
            // Every subscriber of this ref shares ONE registration on the next-promise. Per write that is a
            // single waiter and a single re-registration for the whole fan-out, instead of one per observer:
            // the writing thread walks an array of plain calls rather than allocating N waiters and CASing N
            // times onto the same chain. Subscribing is rare and notifying is hot, so the list is
            // copy-on-write.

            // The dispatch walk runs on whatever thread completed the promise, with no user Frame in reach;
            // it is only used to attribute the log line of a failing subscriber.
            private given dispatchFrame: Frame = Frame.internal

            private val subs =
                new java.util.concurrent.atomic.AtomicReference[Chunk[Sub[A, ?]]](Chunk.empty)
            private val armed =
                new java.util.concurrent.atomic.AtomicReference[IOPromise.Waiter[Any, Any]](null)
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

            /** Drop `sub`; when the last one leaves, release the shared registration. The next-promise is
              * masked and outlives its subscribers, so leaving the waiter behind would retain every callback.
              *
              * Retiring is a flag flip, not a list rebuild. Filtering the copy-on-write list per removal made
              * teardown quadratic — tearing down a thousand rows meant a thousand passes over a thousand
              * entries, and ten thousand rows would have been a hundred times worse. Dead entries are skipped
              * by the dispatch walk and swept out in one pass once they outnumber the live ones; the common
              * case (everything goes at once) needs no sweep at all, because the list is dropped wholesale.
              */
            private[kyo] def unsubscribe(sub: Sub[A, ?])(using AllowUnsafe): Unit =
                if sub.kill() then
                    if liveSubs.decrementAndGet() == 0 then
                        subs.set(Chunk.empty)
                        val w = armed.getAndSet(null)
                        if w ne null then discard(w.cancel())
                    else sweepIfCluttered()
            end unsubscribe

            private def sweepIfCluttered(): Unit =
                val cur = subs.get()
                if cur.size > 2 * liveSubs.get() + 16 then
                    discard(subs.compareAndSet(cur, cur.filter(_.isAlive)))
            end sweepIfCluttered

            // Per-subscriber isolation: on the fiber path each observer had its own fiber, so one failing
            // sink could not silence the others. The shared dispatch walk has to reproduce that, and so does
            // the initial delivery in `subscribe` — a sink that throws there would otherwise abort whoever
            // was setting the subscription up.
            private def deliverTo(sub: Sub[A, ?], value: A)(using AllowUnsafe): Unit =
                try sub.deliver(value)
                catch case ex if NonFatal(ex) => Log.live.unsafe.error("signal subscriber failed", ex)

            private def dispatch()(using AllowUnsafe): Unit =
                val value = currentRef.get()
                subs.get().foreach(sub => if sub.isAlive then deliverTo(sub, value))

            /** Deliver and re-arm until the signal holds still, then leave exactly one waiter behind.
              *
              * The version is read ONCE per round, BEFORE dispatching, and re-checked afterwards. That
              * ordering is what makes the path lossless in the two ways a write can be dropped: one racing
              * the registration (the case [[nextSince]] handles), and one issued from INSIDE a delivery —
              * the sink calls `set`, which completes a promise nobody is registered on yet, because the
              * re-arm has not happened. Re-reading the version after the dispatch catches both and sends us
              * round again; `Sub.deliver` deduplicates, so a redundant round costs a projection.
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
                            val w = p.onCompleteCancellable(fire)
                            // If `p` had already completed, `onCompleteCancellable` ran `fire` inline and
                            // `fire` pumped, so this CAS loses and `w` is a dead duplicate.
                            if !armed.compareAndSet(null, w) then discard(w.cancel())
                            done = true
                        end if
                    end if
                end while
            end pump

            // An error completion must NOT re-arm: the promise is only swapped on a value update (see
            // onUpdate), so re-arming would capture the same completed promise, whose registration fires
            // inline — unbounded recursion.
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
                    // Use initMasked so that interrupting one subscriber cannot propagate through the
                    // signal's next-promise and accidentally interrupt other subscribers waiting on the
                    // same signal. All subsequent promises (created in onUpdate) are already masked for
                    // the same reason; the initial promise must be consistent.
                    AtomicRef.Unsafe.init(Promise.Unsafe.initMasked()),
                    AtomicLong.Unsafe.init(0L)
                )
        end Unsafe

    end SignalRef
end Signal
