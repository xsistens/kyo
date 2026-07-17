package kyo

import kyo.Browser.*

/** `onContextMenu` (right-click) end-to-end over the server-push transport (withUI → runHandlers →
  * HtmlRenderer.clientJs) in real Chrome — the same harness as FocusAutoScenarioItTest. Right-clicks are
  * driven as synthetic `contextmenu` MouseEvents (same technique as the synthetic clicks other suites use);
  * the client's capture-phase body listener sees synthetic events exactly like trusted ones.
  *
  * DomBackend's SPA-mode mirror (contextmenu listener + preventDefault-when-declared) has no browser
  * harness (the Node test env has no DOM); it is kept in sync by construction with the clientJs
  * implementation pinned here.
  */
class ContextMenuScenarioItTest extends UITest:

    /** Dispatch a synthetic right-click and return whether the NATIVE context menu would still open
      * (dispatchEvent returns false iff some listener called preventDefault on the cancelable event).
      */
    private def rightClick(id: String)(using Frame) =
        Browser.evalBoolean(
            s"""document.getElementById('$id').dispatchEvent(new MouseEvent('contextmenu',{bubbles:true,cancelable:true,ctrlKey:false}))"""
        )

    "onContextMenu emits the contextmenu event marker; absent on plain elements" in {
        val app: UI < Async = UI.div(
            UI.div("target").id("t").onContextMenu(()),
            UI.div("plain").id("plain")
        )
        withUI(app) {
            for
                _ <- Browser.assertAttribute(Selector.id("t"), "data-kyo-ev", "contextmenu")
                _ <- Browser.assertNoAttribute(Selector.id("plain"), "data-kyo-ev")
            yield ()
        }
    }

    "right-click fires the handler and suppresses the native menu" in {
        val app: UI < Async =
            for ref <- Signal.initRef(0)
            yield UI.div(
                UI.div("target").id("t").onContextMenu(ref.getAndUpdate(_ + 1).unit),
                ref.map(n => UI.span(n.toString).id("v"))
            )
        withUI(app) {
            for
                nativeMenu <- rightClick("t")
                _          <- Browser.assertText(Selector.id("v"), "1")
                // preventDefault fired (an ancestor declared the handler), so the native menu is suppressed.
                _ = assert(!nativeMenu)
            yield ()
        }
    }

    "right-click on a plain element does not fire and keeps the native menu" in {
        val app: UI < Async =
            for ref <- Signal.initRef(0)
            yield UI.div(
                UI.div("target").id("t").onContextMenu(ref.getAndUpdate(_ + 1).unit),
                UI.div("plain").id("plain"),
                ref.map(n => UI.span(n.toString).id("v"))
            )
        withUI(app) {
            for
                nativeMenu <- rightClick("plain")
                // Positive control AFTER the plain-element right-click: the socket preserves order, so once
                // v=1 is observed the plain dispatch has been fully processed too — if it had wrongly fired,
                // v would read 2.
                _ <- rightClick("t")
                _ <- Browser.assertText(Selector.id("v"), "1")
                // No handler in the chain: preventDefault must NOT run, the native menu stays available.
                _ = assert(nativeMenu)
            yield ()
        }
    }

    "right-click on a descendant bubbles to the ancestor's handler" in {
        val app: UI < Async =
            for ref <- Signal.initRef(0)
            yield UI.div(
                UI.div.id("parent").onContextMenu(ref.getAndUpdate(_ + 1).unit)(
                    UI.span("child").id("child")
                ),
                ref.map(n => UI.span(n.toString).id("v"))
            )
        withUI(app) {
            for
                nativeMenu <- rightClick("child")
                _          <- Browser.assertText(Selector.id("v"), "1")
                _ = assert(!nativeMenu)
            yield ()
        }
    }

    "typed onContextMenu receives the MouseEvent payload (target id)" in {
        val app: UI < Async =
            for ref <- Signal.initRef("")
            yield UI.div(
                UI.div("target").id("t").onContextMenu(me => ref.set(me.targetId.getOrElse("none"))),
                ref.map(v => UI.span(v).id("v"))
            )
        withUI(app) {
            for
                _ <- rightClick("t")
                _ <- Browser.assertText(Selector.id("v"), "t")
            yield ()
        }
    }

end ContextMenuScenarioItTest
