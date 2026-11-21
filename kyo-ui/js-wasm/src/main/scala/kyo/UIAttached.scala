package kyo

import org.scalajs.dom

extension (e: UI.Ast.Element)
    /** Runs `f` with this element once it is in the document, for code that needs the real element: a chart, an editor, a map widget.
      *
      * The hook starts when the browser has the element, wherever it sits: under a mount, a region or a boundary that holds its content
      * back until everything inside has loaded, in which case it starts once the boundary shows that content. It never starts for an
      * element that leaves before it arrived. It gets a [[kyo.Scope]] that closes when the element leaves the document, which
      * interrupts the hook if it still runs and releases what it acquired, so a widget is torn down with
      * `Scope.acquireRelease(Sync.defer(Chart.draw(el)))(c => Sync.defer(c.destroy()))`.
      *
      * The hook is tied to the element value, the way a `UI.mounted` without a key is tied to its node: a re-render that builds the
      * element again closes the old scope and starts the hook for the rebuilt one, whether or not the page kept the DOM node. A
      * re-render that shows the same value again (one held in a `val`, say) keeps the hook running and leaves what it put inside the
      * element in place. So an element whose hook should outlive the re-renders around it is built once, outside them, and inputs
      * that should reach a running hook belong in a `Signal` it observes.
      *
      * The hook may wait, which is how work that started earlier is used once the element is there: start it in the enclosing
      * `UI.mounted` with `Fiber.init`, and wait for the fiber here. Waiting holds up neither events nor a boundary, since nothing waits
      * for the hook.
      *
      * A failure the hook leaves is logged, as an event handler's is, and `UI.commands` is available inside it. This is the one place
      * kyo-ui hands out a raw `dom.Element`, since the libraries it is for take nothing else; it exists only in the browser mount
      * (`UI.runMount`), the one transport whose code runs where the element is.
      */
    def onAttached(f: dom.Element => Any < (Async & Scope & Abort[Any])): e.Self =
        e.withAttrs(e.attrs.copy(onAttached = Present((el: Any) => f(el.asInstanceOf[dom.Element]))))
end extension
