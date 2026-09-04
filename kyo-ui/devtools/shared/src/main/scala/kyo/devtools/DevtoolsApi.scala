package kyo.devtools

import kyo.*
import kyo.internal.Devtools as Spi

/** Render devtools for a kyo-ui app: one wrapper at the root, and nothing else anywhere.
  *
  * {{{
  * // browser mount (Scala.js)
  * Devtools.runMount() { UI.runMount(Shell.view, "#app") }
  *
  * // server-push
  * handlers <- Devtools.runHandlers("/")(page)
  * }}}
  *
  * No component is touched and there is nothing to opt into per component. kyo-ui reports from the only
  * places an update can originate — its reactive regions — and a static subtree has none, so the overlay
  * appears exactly where a re-render is possible. That is also why it does not bury a page in badges.
  *
  * Not for production. The measurement is cheap; the overlay is several hundred lines of DOM code, and the
  * reason it lives in its own artifact is so a build that does not depend on it cannot link it.
  *
  * '''Why a trait with one object per platform.''' `runMount` only exists where a document does, and the
  * obvious way to add it — an `extension (d: Devtools.type)` in the Scala.js sources, the way kyo-ui adds
  * `UI.runMount` — does not survive contact with the call site. An extension method is resolved by NAME, so
  * importing this one into the file that also calls `UI.runMount` makes the compiler try
  * `kyo.devtools.runMount(UI)` for kyo-ui's own extension and fail. Every app that wants devtools has exactly
  * that file. A real member on a per-platform object has no such interaction.
  */
trait DevtoolsApi:

    /** Frequency of the hand-off to the overlay. Not configurable: below it the badges visibly lag the app,
      * above it nothing is gained and the snapshot starts showing up in profiles instead of the app.
      */
    private[devtools] val pushInterval: Duration = Spi.statsInterval

    /** Server-push handlers with devtools attached, in place of `UI.runHandlers`.
      *
      * The overlay script is registered for the page, and each session gets a fiber that ships snapshots over
      * its own socket. Registration is a process-wide handshake rather than a `Local` because kyo-http builds
      * these handlers at setup and runs them on fibers that inherit nothing from here — the context gap
      * `UI.notices` documents. `UI.devtools` installs the sink the same way, for the same reason.
      */
    def runHandlers(basePath: String, config: DevtoolsConfig = DevtoolsConfig.default)(ui: => UI < Async)(using
        Frame
    ): Seq[HttpHandler[?, ?, ?]] < Sync =
        val store = new DevtoolsStore(config)
        Sync.defer {
            Spi.pageScript = Present(DevtoolsJs.script + "\nwindow.__kyoDev.install({});\n")
            Spi.statsProvider = Present(() => payload(store))
        }.andThen(UI.devtools(store.record)(UI.runHandlers(basePath)(ui)))
    end runHandlers

    private[devtools] def payload(store: DevtoolsStore): String =
        DevtoolsWire.snapshotJson(store.snapshot())

end DevtoolsApi
