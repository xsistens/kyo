package kyo

/** Pure-seam tests for [[UILocation]] members that do not require a real browser DOM.
  *
  * The Node.js test environment (NodeJSEnv) has no DOM globals. The following UILocation members
  * require a real browser DOM and are therefore validated by exercising the compiled bundle in a
  * real browser:
  *
  *   - `assign(uri)` invoking `window.location.assign` (requires `window.location`)
  *   - `push`, `replace`, `back`, `forward`, `go` (require `window.history`)
  *   - `current` reactive signal (requires `window.location.pathname`)
  *
  * Browser-side behavior is validated by real use in the compiled bundle, matching the module
  * convention: UILocation and UIWindow have no committed JS browser tests because the NodeJS test
  * environment has no DOM.
  *
  * The anchor interceptor's decision for a single click needs only a document, so that one runs against
  * jsdom ([[DomTestEnv]]).
  */
class UILocationTest extends kyo.test.Test[Any]:

    "a click on a download anchor is left to the browser rather than routed" in {
        import org.scalajs.dom
        import scala.scalajs.js as scalajs
        DomTestEnv.install
        discard(UILocation.current)
        val anchor = dom.document.createElement("a")
        // Same (empty) host as jsdom's about:blank page but a different path, so without `download` the interceptor would
        // route this click.
        anchor.setAttribute("href", "about:srcdoc")
        anchor.setAttribute("download", "rows.csv")
        discard(dom.document.body.appendChild(anchor))
        val event = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].MouseEvent)(
            "click",
            scalajs.Dynamic.literal(bubbles = true, cancelable = true)
        )
        discard(anchor.asInstanceOf[scalajs.Dynamic].dispatchEvent(event))
        anchor.remove()
        assert(!event.defaultPrevented.asInstanceOf[Boolean])
    }

    "a click on a blocked anchor is neither routed nor followed" in {
        import org.scalajs.dom
        import scala.scalajs.js as scalajs
        DomTestEnv.install
        discard(UILocation.current)
        val anchor = dom.document.createElement("a")
        // Same origin and a different path, so the interceptor would route this click with pushState (the browser would
        // otherwise follow the href). aria-disabled is the marker Button draws on a loading or inert link.
        anchor.setAttribute("href", "about:srcdoc")
        anchor.setAttribute("aria-disabled", "true")
        anchor.setAttribute("data-kyo-blocked", "1")
        discard(dom.document.body.appendChild(anchor))
        val history = dom.window.history.asInstanceOf[scalajs.Dynamic]
        val real    = history.pushState
        var pushes  = 0
        history.pushState = scalajs.Any.fromFunction3((_: scalajs.Any, _: scalajs.Any, _: scalajs.Any) => pushes += 1)
        val event = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].MouseEvent)(
            "click",
            scalajs.Dynamic.literal(bubbles = true, cancelable = true)
        )
        try discard(anchor.asInstanceOf[scalajs.Dynamic].dispatchEvent(event))
        finally
            history.pushState = real
            anchor.remove()
        end try
        assert(event.defaultPrevented.asInstanceOf[Boolean])
        assert(pushes == 0)
    }

    "a click on an anchor that only says aria-disabled is routed as before" in {
        import org.scalajs.dom
        import scala.scalajs.js as scalajs
        DomTestEnv.install
        discard(UILocation.current)
        val anchor = dom.document.createElement("a")
        // aria-disabled alone is a hint: without the blocked marker the interceptor routes the click as it does any anchor.
        anchor.setAttribute("href", "about:srcdoc")
        anchor.setAttribute("aria-disabled", "true")
        discard(dom.document.body.appendChild(anchor))
        val history = dom.window.history.asInstanceOf[scalajs.Dynamic]
        val real    = history.pushState
        var pushes  = 0
        history.pushState = scalajs.Any.fromFunction3((_: scalajs.Any, _: scalajs.Any, _: scalajs.Any) => pushes += 1)
        val event = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].MouseEvent)(
            "click",
            scalajs.Dynamic.literal(bubbles = true, cancelable = true)
        )
        try discard(anchor.asInstanceOf[scalajs.Dynamic].dispatchEvent(event))
        finally
            history.pushState = real
            anchor.remove()
        end try
        assert(pushes == 1)
    }

    "a click on an anchor inside a blocked anchor is neither routed nor followed" in {
        import org.scalajs.dom
        import scala.scalajs.js as scalajs
        DomTestEnv.install
        discard(UILocation.current)
        val outer = dom.document.createElement("a")
        outer.setAttribute("aria-disabled", "true")
        outer.setAttribute("data-kyo-blocked", "1")
        val inner = dom.document.createElement("a")
        inner.setAttribute("href", "about:srcdoc")
        outer.appendChild(inner)
        discard(dom.document.body.appendChild(outer))
        val history = dom.window.history.asInstanceOf[scalajs.Dynamic]
        val real    = history.pushState
        var pushes  = 0
        history.pushState = scalajs.Any.fromFunction3((_: scalajs.Any, _: scalajs.Any, _: scalajs.Any) => pushes += 1)
        val event = scalajs.Dynamic.newInstance(dom.window.asInstanceOf[scalajs.Dynamic].MouseEvent)(
            "click",
            scalajs.Dynamic.literal(bubbles = true, cancelable = true)
        )
        try discard(inner.asInstanceOf[scalajs.Dynamic].dispatchEvent(event))
        finally
            history.pushState = real
            outer.remove()
        end try
        // The page's own click handling declines a click inside a blocked anchor, so the router must agree with it.
        assert(event.defaultPrevented.asInstanceOf[Boolean])
        assert(pushes == 0)
    }

    "UILocation Sync-effect members compile to the expected effect types (type-level gate)" in {
        // Each val ascription is a compile-time witness: it fails if the member's return type
        // drifts. The lambdas are never called, so no DOM global is accessed at runtime.
        val _: String => Unit < Sync = uri => UILocation.assign(uri)
        val _: String => Unit < Sync = uri => UILocation.push(uri)
        val _: String => Unit < Sync = uri => UILocation.replace(uri)
        val _: Unit < Sync           = UILocation.back
        val _: Unit < Sync           = UILocation.forward
        val _: Int => Unit < Sync    = n => UILocation.go(n)
        succeed
    }

end UILocationTest
