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
      * skipped when the current value still equals it, and the first differing value is delivered as usual. It serves a caller that already
      * processed a value (e.g. painted it) and only wants what changed since. The baseline is the value the caller processed, not the value
      * current at subscription: a write landing between the two is then delivered rather than taken for already seen.
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
      * `cb` must not suspend and must stay trivial: it runs inside somebody else's `set`.
      */
    private[kyo] def unsafeObserveProjected[B](proj: A => B, baseline: Maybe[B], cb: B => Unit)(
        using
        CanEqual[B, B],
        AllowUnsafe,
        Frame
    ): Maybe[() => Unit] = Absent

    /** Observation of a projected view of this signal, deduplicated on the projection rather than on this signal's own values.
      *
      * This is what a derived signal's observation needs. Comparing source values would make every observer of a derived signal wake and
      * deliver on every source change even when its own image is unchanged: a thousand rows deriving `selected.map(_ == row.id)` from one
      * selection signal would each run a full per-value `Scope` teardown and setup for a value that did not move. Comparing images confines
      * that to the rows whose image actually changed.
      *
      * The per-value `Scope` follows the image: it opens when the image changes and stays open while the image holds, so a source change
      * that leaves the image alone keeps what `g` set up for it.
      *
      * Structurally the repairing loop of [[observe]], with `proj(cur)` where that one has `cur`; [[SignalRef]] overrides it the same way,
      * so a projected observation of a ref keeps the exact protocol. A projection over an already-derived signal (a `map` of a `map`) falls
      * back to this repairing loop, exactly as an ordinary observation of one does.
      *
      * A wakeup that finds the source value unchanged does not project again: the loop also keeps the source value it last projected and
      * calls `proj` only for a value that differs from it. Images need not be `==` for equal source values (a rendered UI tree never is),
      * so re-projecting on every repair timer would count each tick as a change and rerun `g` once per `repairInterval`. `proj` must
      * therefore be a pure function of the source value: state it reads besides that value is not picked up by a tick. The kept source value
      * is dropped once an image's `Scope` has closed, so a source that returns to it during the close is projected and compared again.
      *
      * @param proj
      *   The view to observe, a pure function of the source value; called only for a source value that differs from the last one projected,
      *   and compared for equality against the last delivered image
      * @param baseline
      *   The image already processed by the caller: `g` is not run while the current image still equals it
      * @param repairInterval
      *   How often a parked observation re-reads `current` to reconcile a missed wakeup on the repair path
      * @param g
      *   The per-image setup, run inside a fresh `Scope`
      */
    def observeProjected[B, S](proj: A => B, baseline: Maybe[B], repairInterval: Duration)(
        g: B => Unit < (S & Async & Scope)
    )(using CanEqual[B, B], Frame): Unit < (S & Async) =
        def await: Unit < Async =
            Async.race(Seq(nextWith(_ => ()), Async.sleep(repairInterval))).unit
        // `src` projects to `b`: a wakeup with an unchanged source waits again without calling `proj`.
        def holdUntilChanged(src: A, b: B): Unit < (S & Async) =
            await.andThen(currentWith { c =>
                if c == src then holdUntilChanged(src, b)
                else if proj(c) == b then holdUntilChanged(c, b)
                else (): Unit < (S & Async)
            })
        // `seen` projects to `last`; dropped once a Scope has closed.
        def loop(seen: Maybe[A], last: Maybe[B]): Unit < (S & Async) =
            currentWith { cur =>
                if seen.exists(_ == cur) then await.andThen(loop(seen, last))
                else
                    val b = proj(cur)
                    if last.exists(_ == b) then await.andThen(loop(Present(cur), last))
                    else Scope.run(g(b).andThen(holdUntilChanged(cur, b))).andThen(loop(Absent, Present(b)))
            }
        loop(Absent, baseline)
    end observeProjected

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
            // Observed through the source's projected loop, which projects only when the source moved and delivers only
            // when the image moved. The baseline is already in image space.
            [S] => (baseline, ri, g) => self.observeProjected[B, S](f, baseline, ri)(g),
            // A projection of `x.map(f)` is one of `x` through `proj` after `f`, so a chain rooted in a SignalRef keeps
            // that ref's exact protocol past every map.
            [C, S] =>
                (proj, baseline, ri, g, canEqualC) =>
                    self.observeProjected[C, S](a => proj(f(a)), baseline, ri)(g)(using canEqualC, frame),
            // The same composition for the callback path.
            [C] =>
                (proj, baseline, cb, canEqualC, allow) =>
                    self.unsafeObserveProjected[C](a => proj(f(a)), baseline, cb)(using canEqualC, allow, frame)
        )

    /** This signal's changes, carrying `b` in place of its own values.
      *
      * Not `map(_ => b)`: observation deduplicates on the image, so a constant image collapses to a single delivery however often the
      * source moves. That is right for a projection, whose view did not change, and wrong for a caller whose emitted value is a stable
      * handle and whose content is rebuilt from the source at delivery time. Such a caller needs the source's own change detection, which is
      * what this keeps: every arm below observes the source on its values and hands over the constant.
      *
      * The baseline is in `b`'s space and `b` is the only value there, so it can only mean "the caller has already processed one
      * delivery". That is honoured by seeding the source observation with the source's current value, after which every source change
      * delivers again.
      *
      * The callback path declines (`Absent`): its subscriber deduplicates on the image too.
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
      * This method produces a stream that emits values only when they differ from the previous value, starting with the value current at
      * subscription. Note that rapid changes may result in some intermediate values being skipped if they occur faster than they can be
      * processed. Built on [[observe]], so the latest value is never stranded: exact on a [[SignalRef]] (and `map` chains rooted in one),
      * reconciled within [[Signal.defaultRepairInterval]] on combinator-derived signals.
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
        streamChanges(baseline, Signal.defaultRepairInterval)

    /** Like [[streamChanges]] with a baseline, and an explicit reconciliation interval for combinator-derived signals. */
    final def streamChanges(baseline: Maybe[A], repairInterval: Duration)(using Frame, Tag[Emit[Chunk[A]]]): Stream[A, Async] =
        Stream(observe[Emit[Chunk[A]]](baseline, repairInterval)(a => Emit.value(Chunk(a))))

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
            // A constant cannot change, so there is nothing for a reconciliation timer to reconcile: the repairing loop would
            // re-arm a `nextWith`/`sleep` race every interval, forever, for each observer. Deliver once and hold the scope
            // instead; interrupting the observation still closes it.
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

        /** Awaits a change since version `v0`. Shared by the exact [[observe]] and [[observeProjected]] loops, so the masked-promise
          * handling below exists once.
          */
        private def nextSince(v0: Long)(using Frame): Unit < Async =
            Sync.Unsafe.defer {
                if _unsafe.version() != v0 then ()
                else
                    // Parks directly on the masked next-change promise: an interrupted fiber releases the wakeup it registered
                    // there (see `IOTask`), so the observer stays interruptible without a wrapper.
                    val waiter = _unsafe.next().safe
                    if _unsafe.version() != v0 then (): Unit < Async
                    else waiter.use(_ => ())
                end if
            }

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

        /** The exact protocol of [[observe]], comparing images instead of values. See [[Signal.observeProjected]] for why a
          * derived signal must deduplicate on its own image: without this, one selection change delivers to every row.
          */
        override def observeProjected[B, S](proj: A => B, baseline: Maybe[B], repairInterval: Duration)(
            g: B => Unit < (S & Async & Scope)
        )(using CanEqual[B, B], Frame): Unit < (S & Async) =
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

        /** Observes exactly, without a repair timer, through a version-validated register/validate/await protocol.
          *
          * A write stores the value, increments the version, then swaps and completes the next-change promise (see `Unsafe.onUpdate`). The
          * observer reads the version before the value, runs `f`, and re-arms by capturing the next-change promise and checking the version
          * again, parking only while it is unchanged. A write that lands before the check is seen by the check; one that lands after it
          * completes exactly the captured promise. Either way no change is stranded, and an idle observer holds exactly one waiter.
          *
          * Reading the version before the value is what makes this sound: the other order could pair a fresh value with a stale version and
          * then wait on a promise that write has already completed and replaced. Observation stays level-based, so a change and its revert
          * during `f` wake the observer, which re-reads an unchanged value and waits again.
          *
          * `repairInterval` is not used: only signals that can miss a wakeup need it.
          */
        override def observe[S](baseline: Maybe[A], repairInterval: Duration)(f: A => Unit < (S & Async & Scope))(using
            Frame
        ): Unit < (S & Async) =
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
              */
            private[kyo] def unsubscribe(sub: Sub[A, ?])(using AllowUnsafe): Unit =
                if sub.kill() then
                    if liveSubs.decrementAndGet() == 0 then
                        subs.set(Chunk.empty)
                        val p = armed.getAndSet(null)
                        if p ne null then discard(p.remove(fire))
                    else sweepIfCluttered()
            end unsubscribe

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
