package kyo.devtools

import kyo.*

/** How the render devtools aggregate and how much they keep.
  *
  * Every number here buys memory or smoothing, and the defaults are chosen for the one case that matters —
  * watching a live app with a hand on the mouse — not for offline analysis. A devtool that keeps everything
  * ends up profiling itself.
  *
  * @param rateTau
  *   time constant of the rate estimate. A region rendering steadily at `r`/s converges to `r`; when it stops,
  *   the estimate decays to a tenth in roughly `2.3 * rateTau`. Short enough that a burst is visible while it
  *   happens, long enough that a single render does not light the page up.
  * @param historySeconds
  *   how far the per-region sparkline reaches back, one bucket per second.
  * @param durationSamples
  *   the window the duration statistics are computed over. Exact within the window, and small enough that the
  *   percentile can be taken by sorting a copy when someone actually opens the panel.
  * @param maxRegions
  *   the cap on tracked regions. A page whose tree grows without bound (a route that never unmounts, a keyed
  *   list rebuilt under fresh paths) would otherwise turn the store into the leak it is meant to find. Beyond
  *   the cap new regions are ignored and the snapshot says so, rather than evicting live ones and reporting
  *   numbers that silently restart.
  */
final case class DevtoolsConfig(
    rateTau: Duration = 2.seconds,
    historySeconds: Int = 60,
    durationSamples: Int = 64,
    maxRegions: Int = 2000
):
    require(rateTau > Duration.Zero, s"rateTau must be positive: $rateTau")
    require(historySeconds > 0, s"historySeconds must be positive: $historySeconds")
    require(durationSamples > 0, s"durationSamples must be positive: $durationSamples")
    require(maxRegions > 0, s"maxRegions must be positive: $maxRegions")
end DevtoolsConfig

object DevtoolsConfig:
    val default: DevtoolsConfig = DevtoolsConfig()
