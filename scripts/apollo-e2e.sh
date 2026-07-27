#!/usr/bin/env bash
# Cross-platform apollo end-to-end suite: start one caliban GraphQL backend, then run
# kyo.apollo.ApolloE2ESpec on every client platform (JVM/JS/Native/Wasm) against it.
#
# The spec is env-gated on APOLLO_IT_URL, so a plain `sbt test` skips it — this script
# is the only thing that stands the backend up and points the client suites at it.
#
# Usage:  scripts/apollo-e2e.sh          # all four platforms
#         scripts/apollo-e2e.sh JVM JS   # a subset
set -euo pipefail
cd "$(dirname "$0")/.."

PLATFORMS=("$@")
[ ${#PLATFORMS[@]} -eq 0 ] && PLATFORMS=(JVM JS Native Wasm)

# The caliban server is JVM-only, so it is launched as a plain java process (decoupled
# from the sbt instances that run the client suites) via its exported runtime classpath.
echo ">> resolving itserver classpath"
CP=$(sbt -batch "export kyo-apollo-itserverJVM/Runtime/fullClasspath" | grep -vE '^\[|^>|Total time|^$' | tail -1)
[ -z "$CP" ] && { echo "failed to resolve classpath"; exit 1; }

LOG=$(mktemp)
echo ">> launching caliban itserver"
java --enable-native-access=ALL-UNNAMED -cp "$CP" apolloit.ItServer > "$LOG" 2>&1 &
SRV_PID=$!
trap 'kill "$SRV_PID" 2>/dev/null || true; rm -f "$LOG"' EXIT

PORT=""
for _ in $(seq 1 60); do
    PORT=$(grep -oE 'APOLLO_IT_PORT=[0-9]+' "$LOG" | head -1 | cut -d= -f2 || true)
    [ -n "$PORT" ] && break
    kill -0 "$SRV_PID" 2>/dev/null || { echo "itserver exited early:"; cat "$LOG"; exit 1; }
    sleep 1
done
[ -z "$PORT" ] && { echo "itserver did not report a port:"; cat "$LOG"; exit 1; }
echo ">> itserver listening on 127.0.0.1:$PORT"

export APOLLO_IT_URL="127.0.0.1:$PORT"
CMDS=()
for p in "${PLATFORMS[@]}"; do
    CMDS+=("kyo-apollo${p}/testOnly kyo.apollo.ApolloE2ESpec")
done
echo ">> running ApolloE2ESpec on: ${PLATFORMS[*]}"
sbt -batch "${CMDS[@]}"
echo ">> apollo E2E complete"
