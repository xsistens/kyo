package kyo

import kyo.Browser.*

/** Unit 1: declarative enter/leave CSS transition lifecycle that survives kyo's outerHTML-replace rendering.
  *
  * Drives the server-push transport (withUI -> runHandlers -> HtmlRenderer.clientJs) in real Chrome. The
  * DomBackend SPA mirror has no browser harness (same convention as FocusAutoScenarioItTest) and is kept in
  * sync by construction with the clientJs implementation these suites pin down.
  *
  * The transition is put on a plain CSS CLASS ("boxbase") rather than the element id, because the leave ghost
  * is a clone with all `data-kyo-*`/`id` attributes stripped (visual-only, dead interactivity) — a regular
  * class survives the strip so the ghost actually transitions.
  */
class EnterLeaveTransitionScenarioItTest extends UITest:

    private val styleTag: String =
        """<style>.boxbase{opacity:1;transition:opacity 0.15s linear;} .efrom{opacity:0;} .lto{opacity:0;}</style>"""

    "enterTransition emits data-kyo-enter" in {
        withUI(UI.div(UI.div("hi").id("box").enterTransition("efrom"))) {
            Browser.assertAttribute(Selector.id("box"), "data-kyo-enter", "efrom").unit
        }
    }

    "leaveTransition emits data-kyo-leave" in {
        withUI(UI.div(UI.div("bye").id("box").leaveTransition("lto"))) {
            Browser.assertAttribute(Selector.id("box"), "data-kyo-leave", "lto").unit
        }
    }

    "toggling an enter element visible runs its enter transition then clears the enter class" in {
        val app: UI < Async =
            for visible <- Signal.initRef(false)
            yield UI.div(
                UI.rawHtml(styleTag),
                UI.button("show").id("show").onClick(visible.set(true)),
                UI.when(visible)(
                    UI.div("hi").id("box").cssClass("boxbase").enterTransition("efrom")
                )
            )
        withUI(app) {
            for
                // Record a transitionend on #box — it fires only if the enter class was applied (opacity 0)
                // and then removed (opacity -> 1), i.e. the enter lifecycle actually ran.
                _ <- Browser.evalDiscard(
                    "window.__enterDone=false;document.body.addEventListener('transitionend',function(e){if(e.target&&e.target.id==='box')window.__enterDone=true;},true);"
                )
                _ <- Browser.click(Selector.id("show"))
                _ <- Browser.assertExists(Selector.id("box"))
                // The enter transition ran to completion.
                _ <- Browser.waitFor("window.__enterDone")
                // And the enter class was removed (final resting state, opacity 1).
                _ <- Browser.waitFor("!document.getElementById('box').classList.contains('efrom')")
            yield ()
        }
    }

    "toggling a leave element hidden spawns a body-level ghost carrying the leave class, then removes it" in {
        val app: UI < Async =
            for visible <- Signal.initRef(true)
            yield UI.div(
                UI.rawHtml(styleTag),
                UI.button("hide").id("hide").onClick(visible.set(false)),
                UI.when(visible)(
                    UI.div("bye").id("box").cssClass("boxbase").leaveTransition("lto")
                )
            )
        withUI(app) {
            for
                _ <- Browser.assertExists(Selector.id("box"))
                // Observe ghost insertion into <body> and whether it later carries the leave class.
                _ <- Browser.evalDiscard(
                    "window.__ghostSeen=false;window.__ghostHadClass=false;new MutationObserver(function(ms){ms.forEach(function(m){for(var i=0;i<m.addedNodes.length;i++){var n=m.addedNodes[i];if(n.getAttribute&&n.getAttribute('data-kyo-ghost')){window.__ghostSeen=true;(function(nd){requestAnimationFrame(function(){requestAnimationFrame(function(){if(nd.classList&&nd.classList.contains('lto'))window.__ghostHadClass=true;});});})(n);}}});}).observe(document.body,{childList:true});"
                )
                _ <- Browser.click(Selector.id("hide"))
                // The real element is gone (the patch was applied).
                _ <- Browser.assertNotExists(Selector.id("box"))
                // A body-level ghost appeared...
                _ <- Browser.waitFor("window.__ghostSeen")
                // ...and it carried the leave class (added on the next frame).
                _ <- Browser.waitFor("window.__ghostHadClass")
                // ...and it was eventually removed (transitionend cleanup, or the 1s safety timeout).
                _ <- Browser.waitFor("document.querySelectorAll('[data-kyo-ghost]').length===0")
            yield ()
        }
    }

end EnterLeaveTransitionScenarioItTest
