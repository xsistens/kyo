package kyo

import kyo.Browser.*

/** Declarative enter/leave CSS transition lifecycle over the server-push transport in real Chrome. The DomBackend
  * SPA mirror has no browser harness (same convention as FocusAutoScenarioItTest) and is kept in sync by construction.
  *
  * The transition is on a plain CSS class ("boxbase"), not the element id, because the leave ghost is a clone with
  * all `data-kyo-*`/`id` attributes stripped; a regular class survives the strip so the ghost actually transitions.
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
                // A transitionend on #box fires only if the enter class was applied (opacity 0) then removed
                // (opacity 1), i.e. the enter lifecycle actually ran.
                _ <- Browser.evalDiscard(
                    "window.__enterDone=false;document.body.addEventListener('transitionend',function(e){if(e.target&&e.target.id==='box')window.__enterDone=true;},true);"
                )
                _ <- Browser.click(Selector.id("show"))
                _ <- Browser.assertExists(Selector.id("box"))
                _ <- Browser.waitFor("window.__enterDone")
                _ <- Browser.waitFor("!document.getElementById('box').classList.contains('efrom')")
            yield ()
        }
    }

    "toggling a leave element hidden animates the live element in place, then removes it" in {
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
                _ <- Browser.click(Selector.id("hide"))
                // The departing element is the real one: still in the DOM, still under its original parent,
                // now carrying the leave class. Nothing is cloned and nothing is re-parented to <body>.
                _ <- Browser.waitFor(
                    "(function(){var b=document.getElementById('box');return !!b&&b.classList.contains('lto')&&b.parentNode!==document.body;})()"
                )
                _ <- Browser.waitFor("document.querySelectorAll('[data-kyo-ghost]').length===0")
                // The swap lands once the transition has ended.
                _ <- Browser.assertNotExists(Selector.id("box"))
            yield ()
        }
    }

    "a leaving element keeps its own geometry no matter what its classes declare" in {
        // Regression anchor for the whole bug class. Animating a CLONE re-parented to <body> re-resolves every
        // percentage on the surviving classes against the viewport: `padding:10%` alone pushed a 120x46 element
        // to 160x160, because the base reset's global `box-sizing: border-box` floors the border box at
        // padding+border and so overrides the pinned pixel width; `min-width`/`min-height` override it outright.
        // Animating the LIVE node where it already sits cannot drift, whatever the classes declare.
        val pinStyleTag: String =
            """<style>.pinbox{width:120px;align-items:flex-start;}
              |.pinned{min-width:100%;min-height:100%;padding:10%;right:0;bottom:0;opacity:1;transition:opacity 0.15s linear;}
              |.lto{opacity:0;}</style>""".stripMargin
        val app: UI < Async =
            for visible <- Signal.initRef(true)
            yield UI.div(
                UI.rawHtml(pinStyleTag),
                UI.button("hide").id("hide").onClick(visible.set(false)),
                UI.div(
                    UI.when(visible)(
                        UI.div("bye").id("box").cssClass("pinned").leaveTransition("lto")
                    )
                ).cssClass("pinbox")
            )
        withUI(app) {
            for
                _ <- Browser.assertExists(Selector.id("box"))
                // Record the rect, then re-measure the same element the moment the leave class lands on it.
                _ <- Browser.evalDiscard(
                    "window.__leave=null;var b=document.getElementById('box');var r=b.getBoundingClientRect();window.__srcW=r.width;window.__srcH=r.height;window.__bodyW=document.body.getBoundingClientRect().width;new MutationObserver(function(){if(window.__leave===null&&b.classList.contains('lto')){var g=b.getBoundingClientRect();window.__leave={w:g.width,h:g.height};}}).observe(b,{attributes:true,attributeFilter:['class']});"
                )
                // The container is far narrower than the viewport, so a box sized against <body> is unmistakable.
                _ <- Browser.waitFor("window.__bodyW>window.__srcW*2")
                _ <- Browser.click(Selector.id("hide"))
                _ <- Browser.waitFor("window.__leave!==null")
                _ <- Browser.waitFor("Math.abs(window.__leave.w-window.__srcW)<2")
                _ <- Browser.waitFor("Math.abs(window.__leave.h-window.__srcH)<2")
            yield ()
        }
    }

end EnterLeaveTransitionScenarioItTest
