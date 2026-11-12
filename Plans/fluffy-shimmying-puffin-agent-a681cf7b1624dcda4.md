# Plan: silent event handlers in kyo-ui (worktree uic-demo)

Status: plan only. Plan mode was active for this agent, so no source, test or ISA edit and no build was made.

## Facts read (file:line, worktree /home/crz/Programming/kyo/.worktrees/uic-demo/kyo-ui)

- `shared/src/main/scala/kyo/UI.scala:1994` `ariaFlag`: constant writes or drops `ariaAttrs`; signal maps to `Signal[Maybe[String]]` and lands in `Attrs.reactiveMaybeAttrs`.
- `UI.scala:2556` `HasDisabled.disabled(Boolean | Signal[Boolean])`: constant `withDisabled`, signal `reactiveBoolAttrs("disabled")`. Precedence rule at `UI.scala:1890`: where a channel exists it is the value, in either setter order.
- `UI.scala:2784-2799` `Attrs` reactive fields and `hasChannels` (used by `HtmlRenderer.bindsSignal:77`, `addressedByClient:1422`, `ReactiveUI.needsOwnNode:538`, `forkRootChannels:1059`).
- `internal/HtmlRenderer.scala:421` `renderEventAttr` writes `data-kyo-ev` first in the tag; its Boolean result decides `data-kyo-path` together with `carriesPath:1412`. Body at `1335-1393`, writes as it walks so a plain element allocates nothing.
- `HtmlRenderer.scala:1030` `renderReactiveAttrs` writes `attrs.reactiveMaybeAttrs` only (a channel added in `Channels.of` alone is not written twice).
- `internal/Channels.scala:41` `Channels.of(el)` is the one place that lists an element's channels; `rendered` snapshots them.
- `internal/ReactiveUI.scala:1165-1177` maybe-attr binding: Present -> SetAttr, Absent -> BoolAttr off, fast sink and fiber path.
- `HtmlRenderer.scala:2221` client `he(el,t)` and `js-wasm/.../DomBackend.scala:1398` read `data-kyo-ev` at event time. DomBackend renders through `HtmlRenderer` (DomBackend.scala:403, 781), so no second writer of the attribute exists.
- `internal/EventDispatch.scala:280` `isDisabled` (channel first, then static), `unlessInert:303` guards the TARGET only; `dispatchToElement:531` is where every handler of one element runs.
- Dropdown: `HtmlRenderer.scala:813` hardcodes `data-kyo-ev="click,keydown,change"`, and the client option pick (`HtmlRenderer.scala:2553-2560`) posts `Change` without consulting `data-kyo-ev`.
- Signal: `kyo-core/.../Signal.scala:517` `combineLatest` emits on either input.
- No `EventDispatchTest.scala` exists. Dispatch-level tests use `ReactiveUI.normalize` + `subscribe(...).handle(path, UIEvent.Click(...))` (`internal/ClickTargetResolutionTest.scala`). `EventHandlerTest` is browser-level (`Browser.click`), which cannot send a stale event.
- `ReactiveChannelBindingTest` is at `shared/src/test/scala/kyo/internal/`, not `kyo/` as the brief says.

## Design

1. `Attrs`: two fields, mirroring disabled's static/channel split:
   `silent: Boolean = false` and `silentSignal: Maybe[Signal[Boolean]] = Absent`. `hasChannels` adds `|| silentSignal.nonEmpty`.
2. `Element.silent(v: Boolean | Signal[Boolean]): Self` next to `hidden`, scaladoc: constant sets `silent`; signal sets `silentSignal` (channel wins in either order, same rule as `disabled`).
3. Silence of an element (one definition, in `Channels`):
   - disabled part: `reactiveBoolAttrs.get("disabled")` if present, else `HasDisabled.disabled.getOrElse(false)`, else false.
   - silent part: `silentSignal` if present, else `silent`.
   - a constant true part makes the element constantly silent; otherwise one signal is `map`ped, two are `combineLatest` then `map(_ || _)`.
4. HtmlRenderer:
   - split `renderEventAttr` into `private inline def declaredEvents(elem)(inline add: String => Unit)` (inlined, no closure, so a plain element still allocates nothing) plus `private[kyo] def eventList(elem): String`.
   - `renderEventAttr` first asks `silentNow(elem)` (static fields, `SignalNow` of the channels) and returns false writing nothing when silent. Not silent: unchanged bytes.
   - Path: a signal-silent element has `hasChannels`, so it keeps `data-kyo-path` while silent (needed for the patch); a constantly silent one is byte-equal to the same element with no handler.
5. Channels.of: when the silence is a signal and `eventList(el)` is non-empty, add `"data-kyo-ev" -> silence.map(s => if s then Absent else Present(events))` to `maybeAttrs`. Snapshot and binding come for free through the existing maybe-attr channel; no new op kind. Events list is fixed per element value (handlers are fields), so a mapped constant string is correct.
6. EventDispatch: `isSilent(elem)` = `isDisabled(elem)` or the silent part read now (`silentSignal.current` else `silent`). In `dispatchToElement`, every case except `UIEvent.Attach` (a lifecycle hook, not a handler) returns `true` (keep bubbling, as an element with no handler) when silent, for the target and for an element on the bubble path. `disabledTarget` form-submit suppression stays as it is.

## Tests (red first)

- `kyo/AttrsTest.scala`: (a) `onClick(...).silent(true)` on span and button renders byte-equal to the same element with no handler; (b) `disabled(true).onClick(...)` button equals `disabled(true)` button with no handler; (c) `silent(false)` equals the element without the call; (d) `silent(sig)` renders `data-kyo-ev="click"` while off (byte-equal to the plain onClick element) and none while on, `data-kyo-path` kept; (e) `silent(sig).disabled(true)` writes none.
- `kyo/internal/ReactiveChannelBindingTest.scala`: silent signal on a span with onClick, both sinks: ops on `data-kyo-ev` are exactly `off data-kyo-ev`, `set data-kyo-ev=click`, no `Op.Paint`; same for `disabled(sig)` on a button with onClick (data-kyo-ev ops plus the disabled bool ops); combined `disabled(sig).silent(sig)`; add a `silentChannel` to `channels` so the fast-path, release and fiber-path cases cover it.
- `kyo/internal/EventDispatchTest.scala` (new, 1:1 with `EventDispatch.scala`, no orphan): silent target's click handler not called and the parent's is; `silent(false)` target's is called; signal silent read at dispatch time (off: called, on: not called while the parent is, off again: called); a silent element on the bubble path runs no handler while its child and grandparent do; a constantly disabled element on the bubble path runs no handler.
- Red run: compile failure "value silent is not a member" (log `stage1/silent-red.log`). Green run: the three suites, then the whole `kyo-uiJVM/test` (log `stage1/silent-green.log`), since silence changes behaviour of disabled elements on the bubble path and of non-click handlers on disabled targets.

## Choices to confirm (under "byte-identical for everything neither disabled nor silent")

1. Dropdown: render left as it is (its wrapper `data-kyo-ev` is widget wiring, and the client posts the option pick's `Change` without reading it); silence for Dropdown is the server dispatch skip only. Making it client-silent needs a clientJs and DomBackend change, which the brief rules out ("clients unchanged"). Needs Chris's or the DA's call.
2. Disabled counts only where the element has `disabled` (`HasDisabled`: inputs, Textarea, Select, Button, Checkbox, Radio, Fieldset). A disabled Fieldset silences its own handlers, not its descendants'.
3. Silence skips handlers on the bubble path too (not only on the target as `unlessInert` does today), because "as if the element had no handler" holds for both. This changes behaviour for disabled elements that are not the target (e.g. a disabled Fieldset with onClick around a clicked span), which the brief's item 1 implies; the full kyo-ui run will show any existing test that pinned the old behaviour.
4. `Attach` is exempt: a silent or disabled element still starts its `onAttached` hook.
5. A signal-silent element with no events at all gets `data-kyo-path` and an empty channel set (bytes change only for elements that called `silent(sig)`).

## ISA

Append to the `- T23,` line of `kyo-ui/ISA.md` (after "... regions equal or fewer. Pending run.") one entry before the first source edit and one with the red and green log paths.
