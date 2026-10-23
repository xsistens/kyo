#!/usr/bin/env bash
# Populates the generator inputs (input/, work/, npm/node_modules/, all gitignored).
# Run from anywhere; then `sbt kyo-ui-components-gen/run` from the repo root and
# commit the regenerated Scala.
#
# PrimeOne design system (all MIT): pinned in npm/package.json:
#   @primeuix/themes (token presets), @primeuix/styles (component CSS with
#   dt() refs), @primeuix/styled (resolution engine), primeicons (SVGs).
# `bun install --frozen-lockfile` installs exactly what npm/bun.lock pins (it
# fails instead of resolving newer versions; drop the flag only to bump), and
# --ignore-scripts keeps package lifecycle scripts from running.
# extract.mjs runs the engine once at build time (PrimeVue's styled mode,
# frozen to static output) and writes work/{tokens,css}.
#
# Legacy: @ui5/webcomponents-icons stays pinned for the FioriIcons set
# (kept alongside PrimeIcons by explicit decision, 2026-07-15).
set -euo pipefail
UI5_ICONS_VERSION="2.24.0"
cd "$(dirname "$0")"

(cd npm && bun install --frozen-lockfile --ignore-scripts)
bun extract.mjs

mkdir -p input/icons work
curl -sL "https://registry.npmjs.org/@ui5/webcomponents-icons/-/webcomponents-icons-${UI5_ICONS_VERSION}.tgz" -o work/icons.tgz
tar xzf work/icons.tgz -C work
cp work/package/dist/v5/*.js input/icons/
rm -rf work/package work/icons.tgz

echo "inputs ready: $(ls input/icons | wc -l) fiori icons, $(ls npm/node_modules/primeicons/raw-svg | wc -l) prime icons, $(ls work/tokens 2>/dev/null | wc -l) token presets, $(ls work/css 2>/dev/null | wc -l) css files"
