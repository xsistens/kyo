# kyo-ui-devtools

Makes a running kyo-ui app's re-render behaviour visible, so it can be looked at instead of guessed at.

```scala
// browser mount
Devtools.runMount() {
  UI.runMount(Shell.view, "#app")
}

// server-push
handlers <- Devtools.runHandlers("/")(page)
```

That is the whole integration. No component is wrapped, nothing is registered per component, and the app
itself is unchanged — you keep using it normally while the overlay draws over it.

## Why it needs no per-component work

kyo-ui has no component re-execution to instrument. A `uic.Button` is a builder that runs once at
tree-construction time and then does not exist; what exists at runtime is a tree of **reactive regions**
(`Reactive`, `Foreach`, `Mounted`), and a region is the only thing in the engine that can re-run. A static
subtree is walked once and never renders again.

So a counter per region is, exactly and without filtering, a counter per place where a re-render is possible.
That is also why the overlay stays readable: it can only appear where something can actually happen.

## What it reports

A "re-render" in this engine is not one thing, and counting the four as one would point a reader at the
wrong work:

| Kind | What it is | How it is drawn |
|---|---|---|
| **Repaint** | a region re-rendered its subtree and handed the backend HTML | badge + outline |
| **ListPatch** | a keyed list emission, with the rows it actually repainted out of the rows it holds | badge showing `n/m` |
| **Channel** | one attribute or class patched in place — no region re-render at all | counted, no badge |
| **Text** | a `Signal[String]` bound straight to a text node, bypassing the region fiber | counted, no badge |

The last two are already as cheap as this engine gets. They are kept out of the rate and the sparkline on
purpose: making them glow would send every reader to optimize the one thing that needs it least.

Per region, the numbers that matter:

- **self vs. parent-driven** — a region whose own signal fired is a different finding from one that was
  rebuilt by an enclosing repaint. "This rendered 200 times" means opposite things in the two cases.
- **wasted** — the render happened and produced no DOM change. Under server-push this falls out of the
  filter the transport already applies; under the browser mount it is a fingerprint of the painted HTML
  against the previous paint. This is usually the most actionable number on the screen.
- rate (renders/s, exponentially weighted), peak, duration avg and p95, bytes patched, rows repainted, and a
  60-second sparkline.
- the **mount site** — file, line, enclosing method and source snippet, from the `Frame` every UI node
  carries. Read the caveat below before trusting it as the region's own line.

## The overlay

Regions are **named from the DOM**: the tag the region painted plus its most telling class, so the playback
bar's clock reads `span.playback-time` and a kyo-uic component reads by its own `p-<name>` class. See the
caveat below for why not from the source position.

Three layers, so the always-visible part stays quiet:

- **A badge per region** carrying the render count and nothing else. Colour and opacity come from the
  **rate**, not the total, so a region that rendered once at load fades out and a busy one stands out. Above
  a wasted-render ratio of 50% the badge turns red regardless of rate — two wasted renders a second is a
  better find than twenty justified ones.
- **A popover** on hover or click with everything above, including the source snippet, and links to the
  parent and child regions.
- **A panel**, collapsed to a pill showing the page's total rate and wasted share. Expanded it ranks the top
  offenders, carries a page-wide renders/second band for "it stuttered a second ago, what was that", and
  holds the display controls.

The overlay lives in a shadow root on a fixed, pointer-transparent host outside the app's container. It is
not built from kyo-ui, owns no reactive region, and therefore cannot appear in its own statistics.

## Why it is a separate artifact

kyo-ui carries only the reporting SPI: a `Local` read per paint, and nothing at all when no sink is
installed. The overlay is several hundred lines of DOM code that a production Scala.js bundle has no business
linking, and a runtime flag would not let dead-code elimination decide that. Depending on this module is the
switch; not depending on it is the default.

## The `Frame` caveat

The obvious way to name a region is its source position, and kyo hands every UI node a macro-derived `Frame`.
It does not work, for a reason worth knowing: a component written `def view(using Frame): UI` passes its own
frame to every node it builds — that is what a `using` parameter is for. An app that threads `Frame` down from
its entry point, which is normal style, gives every region in the tree the same file and line. Measured on the
spotify showcase: all 19 live regions reported `Main.scala:38`.

Nor can a fresh position be taken inside the framework: the AST nodes are built in package `kyo`, where the
`Frame` macro refuses to expand precisely so that frames point at user code. The `using Frame` at the API
boundary IS that mechanism.

So the frame is reported as the **mount site**, under that name, and the region's name comes from the DOM,
which knows it per region and cannot be threaded away.

## Known gaps

- The overlay is a plain JavaScript module rather than Scala.js, because it must also run under
  `UI.runHandlers`, where the browser holds no Scala.js at all. It follows the precedent of
  `HtmlRenderer.clientJs`.
- Under server-push there is no channel from the overlay back to the server, so the panel's **Pause** freezes
  the display locally and **Reset** is only offered under the browser mount, where it can reach the store.
