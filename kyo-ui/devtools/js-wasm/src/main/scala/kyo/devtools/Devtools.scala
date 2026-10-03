package kyo.devtools

import kyo.*

/** The browser-mount build of [[DevtoolsApi]]. */
object Devtools extends DevtoolsApi:

    /** Wraps a browser mount, in place of calling `UI.runMount` directly:
      *
      * {{{
      * Devtools.runMount() {
      *   UI.runMount(Shell.view, "#app").andThen(Async.never[Unit])
      * }
      * }}}
      *
      * Installs the store as the engine's sink, injects the overlay, and forks one fiber that hands the
      * overlay a snapshot ten times a second.
      */
    def runMount[A, S](config: DevtoolsConfig = DevtoolsConfig.default)(v: A < (Async & Scope & S))(using
        Frame
    ): A < (Async & Scope & S) =
        val store = new DevtoolsStore(config)
        for
            _ <- Sync.defer {
                DevtoolsBrowser.install()
                DevtoolsBrowser.onCommand {
                    // Pause is handled inside the overlay, because freezing the display needs nothing from
                    // here and works identically under the transport that has no way back up the socket.
                    case "reset" => store.reset()
                    case _       => ()
                }
            }
            // Forked BEFORE the body: the body is a mount that parks forever, so anything sequenced after it
            // would never start.
            _      <- Fiber.init(pushLoop(store))
            result <- UI.devtools(store.record)(v)
        yield result
        end for
    end runMount

    private def pushLoop(store: DevtoolsStore)(using Frame): Unit < Async =
        Loop.forever {
            Sync.defer(DevtoolsBrowser.push(payload(store)))
                .andThen(Async.sleep(pushInterval))
        }

end Devtools
