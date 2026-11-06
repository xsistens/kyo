---
name: "kyo-ui instance identity"
task: "kyo-ui and uic: predictable identity, props and robustness"
slug: 20261106-104000_kyo-ui-stage1-identity
project: kyo-ui (fork, fork/uic)
phase: climbing
progress: 17/28
started: 2026-11-06T10:40:00Z
updated: 2026-11-06T17:10:00Z
principal_stated_goal: "Ich möchte ein verhersagbares, konsistentes verhalten, was out of the box in den meisten Szenarien funktioniert und nur in speziellen klar definierten Szenarien eine entsprechende ergänzung benötigt. Außerdem möchte ich nicht von der norm aus anderen frameworks abweichen, es sei denn wir können eine bessere Lösung bereitstellen."
principal_stated_goal_source: prompt
principal_stated_goal_signal: 2
principal_stated_goal_locked: 2026-11-05T00:00:00Z
context_sufficient: true
interview_invoked: false
absence_review: skipped — anti-criteria come from findings already reviewed with the principal over three sessions; the principal asked to save tokens
---

<!-- Fork-only file. Lives in its own commit on fork/uic and never reaches an upstream PR branch. -->

## Ideal State

### L3 · One sentence

kyo-ui and uic behave as predictably as the established fine-grained frameworks: an instance lives exactly as long as its call site or its key, and its state is never lost and never jumps unexpectedly.

### L2 · Summary

An app author writes plain kyo-ui and uic code and gets the lifetime, props and event behavior they would expect from Solid, Svelte, Leptos or Compose, with an addition needed only in clearly named cases.

- Identity: a region that runs again builds new content; a key or the component's `MountSite` survives the rebuild; an explicit key used twice in one range is an error.
- Props: a kept instance sees every new call value through `mountedWith`, and fixed values and signals are equal for the app.
- Robustness: no event is lost, no paint hangs and no page tears, also under load, each proven by a deterministic test.
- Agreement: the page and the session give the same identities and ids on JVM and JS, and the work lands in commits that can go upstream one layer at a time.

### L1 · Full articulation

The reader of an app should never have to know how kyo-ui tracks instances. A component written once keeps its open menu, its typed filter and its scroll position across every repaint of the region around it, because uic components key themselves on the call site. Content without a key is new whenever its region runs again, which is the rule of Solid, Svelte and Leptos and the original upstream idea that `signal.render(f)` builds fresh. Where the same call site runs several times in one range, the order of the calls tells the instances apart, as Compose does for calls without a key; an explicit key that repeats in one range is a mistake the engine reports, as React, Svelte and Compose all treat it. A kept instance reads its current props through one signal the engine writes on every run, so a `Signal[List[X]]` and a plain list are the same to the app. Events that arrive while a region repaints, a claim files an instance or a subscription takes a draw still reach the instance the reader sees. Boundaries show a page only as a whole, never as a torn intermediate state. Server render and live session agree on every identity and every `UI.id`, on JVM and JS. Every behavior is pinned by a test that fails without the fix and passes in loops under load without a wall-clock budget, and the work is cut into commits, one layer each, that read well to an upstream reviewer.

## Problem

Instance identity in kyo-ui grew in steps: a positional index path, keyed mounts, whileShown keys and element ids all carried parts of the lifetime. The same code kept or restarted state depending on code around it, uic components restarted their overlays on host repaints, and several races lost clicks, hung paints or tore pages, some of them already on upstream HEAD 3cbd6fb98.

## Vision

An app author reads the README rule once ("a region that runs again is new, a key or a component's call site survives") and never meets an exception to it. Every uic component keeps its state out of the box.

## Out of Scope

- The field echo that overwrites typing under load (T69, T84): it exists upstream and gets its own upstream issue and fix. The red case is parked in `~/.cache/kyo-runs/review-fork-uic/t69/`.
- A waiting form of `mountedWith` (for a draw that is `< Async`), so `form/Form.scala` keeps its three waiting mounts.
- Element ids as keys: an id is an address, not a lifetime.

## Principles

- Follow the norm of the established frameworks unless we can provide something better, and say which framework a rule follows.
- Code around a component must not change its behavior.
- Complexity stays inside the engine, never shifted to the app author.

## Constraints

- No wall-clock time as a pass condition; loops use time only as a hang detector.
- A reproducing test fails before every fix and stays as a regression guard.
- One heavy JVM build at a time through the build queue; full suites once at the end, Wasm targeted at the end.
- No em-dashes or en-dashes in code, comments or commits; one layer per commit; this file never upstream.
- Nothing is committed or pushed without the principal's approval.

## Features

### F0 · Cross-cutting
Why: the whole stage lands green on every platform as reviewable commits, and the rules stay true everywhere at once.

- [ ] ISC-1: The full kyo-ui and uic suites pass once on JVM and JS on the final tree.
- [ ] ISC-2: The targeted Wasm suites for the changed kyo-ui files pass on the final tree.
- [ ] ISC-3: `build-commits.sh check` reports "end tree matches working state", with the boundary fix in its own commit after the sink (T82) and this ISA in its own fork commit.
- [ ] ISC-4: Anti: no scratch loop, probe or orphan test remains in the tree (probe: `rg 'SCRATCH|HoldProbe|DrawProbe|hang-probe' kyo-ui` finds nothing).
- [ ] ISC-5: Anti: after the work no sbt, bloop or Chrome process started by us is left running (probe: `pgrep` against the PIDs our runs logged; the principal's own sbt is untouched).

### F1 · Error view on a first value that already failed
Why: a `whileMounted` effect that fails before its subscription reads the state still shows the node's error view (T90).

- [x] ISC-6: "a failure of what runs while it is shown shows the node's error view" in `UIDrawnTest` passes 320 of 320 runs, 16 at a time, under load, where the tree before the fix hung in 1 to 7 of 320.
- [x] ISC-7: The on-page shortcut skips the first paint only while the instance state is still the drawn state (`takeOnPage` with `drawnState` in `MountTable.scala`), proven by a deterministic red-then-green test.

### F2 · A boundary in a keyed row shows its ready paint
Why: the test for a boundary inside a keyed row checks the real scenario on every run, not on seven of eight (T89, T92).

- [x] ISC-8: "a boundary in a keyed row that revealed while the boundary around it was waiting shows its content after the reveal" reads the inner range and passes 320 of 320 runs under load.
- [x] ISC-9: The test forces the order "outer boundary waits, inner reveals, outer reveals" deterministically (gated recorder), so every run checks the scenario its name states.

### F3 · Identity and duplicates
Why: one rule for when an instance is kept, with the order among same-site calls following Compose and duplicate explicit keys reported as the norm does.

- [x] ISC-10: Two calls of one implicit site in a fixed range keep each its own instance across a repaint, told apart by their order.
- [x] ISC-11: The second occurrence's `UI.id` carries `-o1`, the first keeps its id, and page and session agree on both.
- [x] ISC-12: An explicit key used twice in one range reports the duplicate error again (the tests at `MountedTest.scala` 990 and 2381 pin the error), while the same identity in two regions at once stays an error.
- [x] ISC-13: Whether a key came from the caller or from the `MountSite` is held explicitly in the identity, never inferred from the key value.
- [x] ISC-14: Plain `UI.mounted` without a key: dropping the first of three tiles in a `show.render` region restarts all remaining tiles.
- [x] ISC-15: A site-keyed component called from one `map` over `x, y, x`: dropping the first keeps instances at their positions, so the first instance now shows the item that moved to the first place, the second instance stays second, and the third instance is released.
- [x] ISC-16: The same component as three separate calls on three lines, the first under `if`: dropping the first leaves the other two with their own instances.
- [x] ISC-17: A keyed instance whose region runs again while its claim files it is still let go (T79).
- [x] ISC-18: A click on a kept instance while its region paints the next value reaches that instance (T87).

### F4 · Props and MountSite in uic
Why: every uic component keeps its state and sees its current props out of the box, as SplitButton does.

- [x] ISC-19: SplitButton reads its props through `mountedWith(props)(draw)` and keys itself on `UI.MountSite`.
- [ ] ISC-20: `Slot.owning` and `owningFrom` have a form with `mountedWith` and `MountSite`, and all components of group B (16, plus 5 with both forms) use it.
- [ ] ISC-21: The 33 components of group A move their own `UI.mounted` onto `mountedWith` and `MountSite`.
- [ ] ISC-22: The 16 overlay components of group C carry `siteV` in their props and pass the caller's site to the overlay, so two Selects of one app get two overlay sites (T74).
- [ ] ISC-28: An unkeyed `UI.mounted` drawn by a kept instance keeps its instance and state when that instance draws again with new props (probe: `UIDrawnTest` "an unkeyed mount drawn by a kept instance keeps its state when the instance draws again with new props", red first).
- [ ] ISC-23: Anti: no uic component restarts its overlay or loses its open state on a host repaint (probe: the per-component repaint test of each changed component).

### F5 · Test harness that cleans up and waits on state
Why: the harness proves behavior without time budgets and leaves nothing behind.

- [x] ISC-24: `UicTest.drive` waits on a condition, not on 5 seconds, and the 52 uic suites pass on JVM and JS (T78).
- [x] ISC-25: The DataTable harness race is gone (`publishedThroughSwitch`), 3 loops of 738 green.
- [x] ISC-26: A boundary pass that holds more than one paint shows its range once from the live state (T80).
- [ ] ISC-27: `Browser.runShared` on JS closes its shared Chrome and removes its profile when the run ends (T83).

## Test Strategy

| isc | type | check | threshold | tool | anchors_to | severity | tier |
|-----|------|-------|-----------|------|------------|----------|------|
| ISC-1 | bash | full kyo-ui suites JVM and JS | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-uiJVM/test; kyo-uiJS/test; kyo-ui-componentsJVM/test; kyo-ui-componentsJS/test'` | literal |  | build |
| ISC-2 | bash | targeted Wasm suites | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-uiWasm/testOnly kyo.MountedTest kyo.UIDrawnTest kyo.BoundaryTest'` | literal |  | build |
| ISC-3 | bash | commit split rebuilds the tree | end tree matches | `bash ~/.cache/kyo-runs/review-fork-uic/stage1/build-commits.sh check` | literal |  | integrity |
| ISC-4 | bash | no scratch or probe left | 0 matches | `! rg -q -g '!ISA.md' 'SCRATCH\|HoldProbe\|DrawProbe' kyo-ui` | derived: clean-tree |  | liveness |
| ISC-5 | manual | no leftover processes from our runs | 0 | pgrep against PIDs logged by our runs | derived: no-leftover-servers |  |  |
| ISC-6 | bash | T90 case in a loop under load | hung=0 of 320 | `bun ~/.cache/kyo-runs/review-fork-uic/stage1/tools/hang-scratch.ts` (UIDrawnTest case) | literal |  | build |
| ISC-7 | bun-test | deterministic red-then-green test for the skipped first paint | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-uiJVM/testOnly kyo.UIDrawnTest'` | literal |  | integrity |
| ISC-8 | bash | T89 case in a loop under load | hung=0 of 320 | `bun ~/.cache/kyo-runs/review-fork-uic/stage1/tools/hang-scratch.ts` (MountedTest case) | literal |  | build |
| ISC-9 | manual | test forces the outer-wait, inner-reveal order with a gate | gate present, no random order | review of the test in `MountedTest.scala` | derived: deterministic-tests |  |  |
| ISC-10 | bun-test | two implicit calls keep instances by order | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-uiJVM/testOnly kyo.MountedTest'` | literal |  | integrity |
| ISC-11 | bun-test | second occurrence id and page equals session | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-uiJVM/testOnly kyo.UIDrawnTest'` | literal |  | integrity |
| ISC-12 | bun-test | explicit duplicate key reports the error | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-uiJVM/testOnly kyo.MountedTest kyo.UIDrawnTest'` | derived: norm-duplicate-keys |  | integrity |
| ISC-13 | bun-test | caller key and site key are told apart by a field | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-uiJVM/testOnly kyo.MountedTest'` | derived: no-inference |  | integrity |
| ISC-14 | bun-test | unkeyed tiles restart on region rerun | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-uiJVM/testOnly kyo.MountedTest'` | derived: region-rule |  | integrity |
| ISC-15 | bun-test | site-keyed map shifts by position | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-uiJVM/testOnly kyo.MountedTest'` | derived: compose-order |  | integrity |
| ISC-16 | bun-test | separate calls do not shift | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-uiJVM/testOnly kyo.MountedTest'` | derived: compose-order |  | integrity |
| ISC-17 | bun-test | claim filing race | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-uiJVM/testOnly kyo.MountedTest'` | literal |  | integrity |
| ISC-18 | bun-test | click during region paint | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-uiJVM/testOnly kyo.MountedTest'` | literal |  | integrity |
| ISC-19 | bun-test | SplitButton props and site | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-ui-componentsJVM/testOnly kyo.uic.SplitButtonTest'` | literal |  | integrity |
| ISC-20 | bun-test | group B components | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-ui-componentsJVM/testOnly <group B suites>'` | literal |  | integrity |
| ISC-21 | bun-test | group A components | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-ui-componentsJVM/testOnly <group A suites>'` | literal |  | integrity |
| ISC-22 | bun-test | two Selects get two overlay sites | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-ui-componentsJVM/testOnly kyo.uic.SelectTest kyo.uic.OverlayTest'` | literal |  | integrity |
| ISC-23 | bun-test | per-component repaint keeps overlay state | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-ui-componentsJVM/test'` | derived: surrounding-code-rule |  | build |
| ISC-24 | bun-test | uic harness waits on state | 52 suites green | `scripts/sbt.sh test-jvm 'kyo-ui-componentsJVM/test'` | literal |  | integrity |
| ISC-25 | bun-test | DataTable switch cases | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-ui-componentsJVM/testOnly kyo.uic.DataTableTest'` | literal |  | integrity |
| ISC-26 | bun-test | boundary with several held paints | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-uiJVM/testOnly kyo.BoundaryTest'` | literal |  | integrity |
| ISC-28 | bun-test | inner mount kept across a props redraw | Failed 0 | `scripts/sbt.sh test-jvm 'kyo-uiJVM/testOnly kyo.UIDrawnTest'` | literal |  | integrity |
| ISC-27 | bun-test | shared Chrome closed on JS | no process, no profile | `scripts/sbt.sh test-jvm 'kyo-uiJS/test'` then `pgrep` and profile check | literal |  | integrity |

## Decisions

- 2026-11-05: `whileShown` is named `whileMounted`, the key form `whileMountedKeyed(k)(effect)`, `UI.whileShown` is `UI.whileMounted`. "shown" misled, because an element with `.hidden(true)` keeps running.
- 2026-11-05: An element id is not a key. Id and key are different concepts.
- 2026-11-05: A region that runs again builds new content; what must survive says so with a key. No sibling position in an identity with a lifetime; the index path is only a DOM address. Follows Solid, Svelte, Leptos and the upstream idea.
- 2026-11-05: `UI.id` is an address (option A): keyed from owner, `MountSite`, key and row keys without position; unkeyed from the place; no run counter; number keys platform independent.
- 2026-11-05: A uic component's `State` is its `Props`; `Stated` is `HasProps`, the field `s` is `p`. The engine writes the new call values into a `SignalRef` of the kept instance on every run.
- 2026-11-06: `mountedWith[P](props: P)(draw: Signal[P] => UI < Sync)(using CanEqual[P, P], Frame): Drawn` keeps its name.
- 2026-11-06: The component model embraces the reactive signal model (Solid), not Compose's redraw (Chris). A kept component draws once; what follows a prop reads that prop reactively, and a coarse region over all props (`runs.render(now => ...)`) is not the target shape. The concrete design (how a prop that is itself a signal is read, how constants and callbacks reach the instance, what becomes of ISC-28) is still to be laid out before anything changes.
- 2026-11-06 (design in discussion, not decided): a prop is the plain union alias `type Prop[A] = A | Signal[A]`, so a default reads `label: Prop[String] = ""` or `= someSignal` with no wrapper; the component reads every prop through one view of its props (`Live`), which hands kyo-ui the value as it is (a constant stays a plain value, no subscription) where the instance can never be offered new props, and follows the props signal (`switchMap`) where it can. Scratch probe `PropAliasProbeTest` (JVM, 4/4 under `-Werror` and strict equality, log `prop-probe.log`, file deleted): `""` and a signal fit the alias as default and in `copy`; props compare by value for constants and by reference for signals; `UI.input.disabled` takes the alias as it is; a generic read `case s: Signal[?]` compiles with no warning. Two gaps: `UI.span(label)` does not type-check (kyo-ui lifts a `String` and a `Signal[String]` into a child, not their union), and `label == "a"` does not type-check under strict equality.
- 2026-11-06 (side quest T7, not decided): a `reactive { ... }` macro over props (each `.now` hoisted, the reads combined once, the body one `map` or one `render`, all constants giving a plain value). Scratch probe `ReactiveProbeMacro` + `ReactiveProbeTest` (JVM, files deleted; logs `reactive-probe8.log`, `reactive-neg.log`, `reactive-neg2.log`): 5/5 behaviour cases pass. All constants: the body runs once and the result is a plain value. One signal among constants: one signal that follows it. Two signals: one `combineLatestAll`, both updates seen. A `UI` block: a constant gives the node itself, a signal one `Reactive` region through `render`. A bare `Signal` read: needs its own more specific `now` (`Signal[A]` receiver), since against the union alone `A` is inferred as the signal itself. Compile errors as designed, checked by compiling: `.now` outside a block ("must be used within a reactive block") and `.now` on a value defined inside it. `typeChecks` cannot judge a macro defined in the same compilation run (a valid control block also reads false), so the real macro lives in main sources. Second round (logs `reactive-probe10.log`, `reactive-neg3.log`, `reactive-neg4.log`, files deleted): a read inside a lambda that does not escape (`items.map(i => i + a.now)`) is hoisted and follows the signal; a read of the lambda's own parameter (`ps.map(p => p.now)`) is refused at compile time ("defined inside"); a lambda that escapes (`reactive { () => a.now }`) is refused only by accident (no `CanEqual` for `Function0`), so the real macro needs an explicit check and message. The same prop read twice gives the right value, and an observer over three writes saw no mixed pair (`(x, y)` always equal); it still takes two slots in `combineLatestAll`, so the real macro dedups reads of one stable path. Not probed: JS/Native.
- 2026-11-06: The same call site several times in one range is told apart by the order of the calls (Compose), a `foreach` without key by its index, only a splice by a key. The occurrence index enters `UI.id` from the second occurrence on.
- 2026-11-06: refined: the order rule applies only to implicit identity (a component keyed on `MountSite`). An explicit key used twice in one range is an error again, as React ("Keys must be unique among siblings"), Svelte (`each_key_duplicate`) and the Compose KDoc of `key` treat it. The same identity in two regions at once stays an error.
- 2026-11-06: The field echo fix (T69) is deferred to a separate upstream issue and fix.
- 2026-11-06: The rest of stage 1 runs from this ISA with one fresh agent per feature. Agents never write the ISA; the main session ticks criteria from evidence it has seen.
- 2026-11-06: T89 was a test bug, not an engine bug: the outer boundary does not wait for the inner one, and a read at `Seq("0")` never sees the inner paint at `0.0.0.panel.0`. In the forced order the engine paints correctly.
- 2026-11-06: The absence review is skipped: the anti-criteria come from findings the principal reviewed one by one, and the principal asked to save tokens.
- 2026-11-06: ❌ DEAD END: the T79 fix that patched the keep path (`Stranded`, `launchedNow`) only covered the window from the other side; the cause was a launch outside the atomic claim step, now done uninterruptibly with the filing.

## Not yet specified

- F4 is on hold (Chris, 2026-11-06): the site plumbing (`siteV`, `keyedOnSite`, passing the site to the overlay) may move into the engine, with the component as an identity boundary at the placement conversion. Probe result (2026-11-06, JVM, 7/7, scratch test deleted): a `UI.MountSite` taken by a placement conversion (`implicit def place(c)(using Frame, UI.MountSite)`) carries the place where the app writes the node: two placements in one argument list, on two lines, or a chained expression each get their own site; one placement in a `map` is one site for all elements (the order rule tells them apart); a site taken at construction differs from the placement site. Limit: a helper that places a node it was handed gives all its callers the helper's site, unless the helper itself takes `(using UI.MountSite)`, which then hands the callers' sites on. So inner placements in a component's `render` share one site across instances (T74) unless the engine scopes them under the outer site.

- How the full gate (ISC-1) runs within the memory guard: compiling every kyo-ui test source at once, and every uic test source at once (f3-uic-jvm, 2026-11-06), trips the watchdog, so targeted runs filter the test sources. The gate needs a split that still covers every suite.

## Verification

- ISC-6: `UIDrawnTest` "a failure of what runs while it is shown shows the node's error view", f1-loop hung=0 of 320 twice, 369/369 per round
- ISC-7: `UIDrawnTest` "an instance whose effect failed before it is shown leaves its first paint to the page only while it shows what the page drew", f1-red-jvm red, f1-green-jvm 158/158, f1-green-js2 173/173
- ISC-8: `MountedTest` "a boundary in a keyed row that revealed while the boundary around it was waiting shows its content after the reveal", f2-loop1 hung=0 of 320, f2-green-jvm 154/154, f2-green-js 167/167
- ISC-9: same test, order forced by waiting for the inner gate's reveal, then `HeldCommand` via `afterShown` on the inner gate before `slowGate` is released; every run asserts the order (MountedTest.scala)
- ISC-10: `MountedTest` "two calls of one keyed site in a fixed range keep each its own instance ...", order-red5 red, order-green4 8 runs green
- ISC-11: `UIDrawnTest` "the second call of one keyed site in a range gets the first one's id with its order after it ..." and "a server session gives the second call ... the id the page has", order-red5 red, order-js2 218/218
- ISC-17: `MountedTest` "a keyed instance whose region runs again while its claim is filing it is still let go", t78-mtred 3/6 hung, t78-atomb1..10 312/312
- ISC-18: `MountedTest` "a click on a kept instance while its region paints the next value reaches that instance", t87-red1 red, t87-fix2..4 1120 loop runs
- ISC-19: `SplitButtonTest`, sb-red-jvm 1 of 13 red, sb-jvm 256 green
- ISC-28: test written in `UIDrawnTest` (2026-11-06). First red run `isc28-red` hit the 2m timeout (53/54, wrong reason: a wait on the paint's text); the wait now reads the props the instance drew. `isc28-red3` red for the right reason (53/54): after the kept instance draws props 1, its inner unkeyed mount is a new instance at 0 instead of the live one at 5 (`(1,false,0)`). Engine fix not started. 2026-11-06, plan step 0 (Chris: yes): the test checks the coarse `runs.render` shape the Solid model drops, so it is parked out of the tree (`~/.cache/kyo-runs/review-fork-uic/solid/isc28-old-test.scala`) and rewritten for a Solid-shaped component in plan step 3. The coarse kept-instance work (Slot's kept forms, SplitButton, ToggleButton, Password, their repaint tests, the README paragraph and the CLAUDE.md section) is reset to the props-rename tree and saved as `solid/kept-instance.patch` for the pilot (plan step 6). Tree after the reset compiles: `kyo-uiJVM/Test/compile` and `kyo-ui-componentsJVM/Test/compile` `[success]` (`stage1/step0-compile3.log`, role `compile`; the `test-jvm` role's 12 GB driver tripped the 4 GB watchdog twice). Stage-1 commits built with `stage1/build-commits.sh commit` (bucket after the props rename split into F3 Overlay and F5 harness, the ISA its own fork commit).
- ISC-24: t78-harness-jvm 1744 green, t78-harness-js 1744 green
- ISC-25: t78-loopnew1..3 738/738
- ISC-26: `BoundaryTest` "navigating to a page that draws at once keeps the old page until the mount in its content has loaded", t80-red1 red, t80-green2 361/361, t80-js 230/230
- ISC-12: `MountedTest`, the eight duplicate-error tests plus "rows spliced into a fragment whose mounts share a key without data are reported", f3-red12d red with the occurrence guard (MountTable.scala 353) removed: exactly these 9 timed out waiting for the report, 107 passed; f3-green-jvm 116/116
- ISC-13: `MountedTest` "a key the caller chose and a key a component took from its site are different identities, however equal their values", f3-red13 red with `origin` forced to Caller in `MountIdentity` (MountTable.scala 575): only this test timed out, 115 passed; f3-green-jvm 116/116
- ISC-14, ISC-15, ISC-16: `MountedTest` "plain unkeyed mounts restart when their region runs again ...", "a site-keyed component called from one map over x, y, x ..." and the separate-calls test, green in f3-green-jvm 116/116 and f3-green-js 217/217; they pin the region rule and the order rule decided 2026-11-05/06, no separate red run
- F3 overall: f3-green-jvm 217/217 (MountedTest, UIDrawnTest, BoundaryTest), f3-uic-jvm2 28/28 (SplitButtonTest 13, OverlayTest 15, test sources filtered), f3-green-js 217/217
- Runner note 2026-11-06: f3-red12/12b/12c were stopped by the `run-in.sh` memory watchdog (queue exit 143), not by the tests; sbt output had overwritten the WATCHDOG line until `run-in.sh` switched to append mode.
