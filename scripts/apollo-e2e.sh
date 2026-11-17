#!/usr/bin/env bash
# Cross-platform apollo end-to-end suite: start one caliban GraphQL backend, then run
# kyo.apollo.ApolloE2ESpec on every client platform (JVM/JS/Native/Wasm) against it.
#
# The spec is env-gated on APOLLO_IT_URL, so a plain `sbt test` skips it: this script
# is the only thing that stands the backend up and points the client suites at it.
#
# Both sbt runs go through scripts/sbt.sh, which takes the driver heap from
# scripts/sbt-heap-lib.sh: `compile` to resolve the server's classpath, which compiles it
# and everything it depends on, `link` for the client suites, whose heaviest step is
# linking the Native and Wasm test binaries in one driver.
#
# Usage:  scripts/apollo-e2e.sh          # all four platforms
#         scripts/apollo-e2e.sh JVM JS   # a subset
set -euo pipefail
cd "$(dirname "$0")/.."

PLATFORMS=("$@")
[ ${#PLATFORMS[@]} -eq 0 ] && PLATFORMS=(JVM JS Native Wasm)

# The caliban server is JVM-only, so it is launched as a plain java process (decoupled
# from the sbt instances that run the client suites) via its runtime classpath. sbt 2's
# `export` prints no classpath and its `print` shows entries as `${OUT}`-style references,
# so a throwaway task on the server's project maps them to paths through the build's file
# converter and prints them joined. It is scoped to that one project: a key in ThisBuild
# prints once for every project the root aggregates.
echo ">> resolving itserver classpath"
SERVER=kyo-apollo-itserverJVM
CP_TASK='LocalProject("'$SERVER'") / TaskKey[String]("apolloItClasspath") := {
    val conv = fileConverter.value
    (LocalProject("'$SERVER'") / Runtime / fullClasspath).value
        .map(entry => conv.toPath(entry.data).toString)
        .mkString(java.io.File.pathSeparator)
}'
CP=$(scripts/sbt.sh compile -batch "set $CP_TASK" "print $SERVER/apolloItClasspath" | grep -vE '^\[|^>|Total time|^$' | tail -1 | sed -E 's/^[[:space:]]+//')
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
scripts/sbt.sh link -batch "${CMDS[@]}"
echo ">> apollo E2E complete"
