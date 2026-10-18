package kyo.devtools

import kyo.discard
import org.scalajs.dom
import scala.scalajs.js

/** Puts [[DevtoolsJs]] into the live document and talks to it.
  *
  * The whole browser-facing surface of the SPA runner is here, and it is deliberately thin: inject the script
  * once, hand it snapshots, take commands back. Anything that had to decide something would be logic living
  * where no test can reach it.
  */
private[devtools] object DevtoolsBrowser:

    private var installed = false

    /** Inject the overlay and start it. Idempotent on both sides — this flag, and the script's own guard on
      * `window.__kyoDev` for the case where a page ends up with two copies of it.
      */
    def install(): Unit =
        if !installed then
            installed = true
            val tag = dom.document.createElement("script")
            tag.setAttribute("data-kyo-devtools", "")
            tag.textContent = DevtoolsJs.script
            discard(dom.document.head.appendChild(tag))
            discard(api.install(js.Dynamic.literal()))
        end if
    end install

    def push(json: String): Unit =
        if installed then discard(api.push(js.JSON.parse(json)))

    /** Route the panel's buttons back to whatever owns the store. */
    def onCommand(handler: String => Unit): Unit =
        if installed then api.onCommand = js.Any.fromFunction1[String, Unit](handler)

    private def api: js.Dynamic = js.Dynamic.global.window.__kyoDev

end DevtoolsBrowser
