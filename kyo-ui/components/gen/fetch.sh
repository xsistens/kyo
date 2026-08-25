#!/usr/bin/env bash
# Populates the generator inputs. Rerun to bump versions, then `sbt gen/run`
# and commit the regenerated Scala.
#
# PrimeOne design system (all MIT): pinned in gen/npm/package.json —
#   @primeuix/themes (token presets), @primeuix/styles (component CSS with
#   dt() refs), @primeuix/styled (resolution engine), primeicons (SVGs).
# extract.mjs runs the engine once at build time (PrimeVue's styled mode,
# frozen to static output) and writes gen/work/{tokens,css}.
#
# Legacy: @ui5/webcomponents-icons stays pinned for the FioriIcons set
# (kept alongside PrimeIcons by explicit decision, 2026-07-15).
set -euo pipefail
UI5_ICONS_VERSION="2.24.0"
cd "$(dirname "$0")"

(cd npm && bun install)
bun extract.mjs

mkdir -p input/icons work
curl -sL "https://registry.npmjs.org/@ui5/webcomponents-icons/-/webcomponents-icons-${UI5_ICONS_VERSION}.tgz" -o work/icons.tgz
tar xzf work/icons.tgz -C work
cp work/package/dist/v5/*.js input/icons/
rm -rf work/package work/icons.tgz

echo "inputs ready: $(ls input/icons | wc -l) fiori icons, $(ls npm/node_modules/primeicons/raw-svg | wc -l) prime icons, $(ls work/tokens 2>/dev/null | wc -l) token presets, $(ls work/css 2>/dev/null | wc -l) css files"
