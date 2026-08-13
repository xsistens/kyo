#!/usr/bin/env bash
# Publishes the 36 artifacts the demo consumes at the version named in the args file.
# Lives here rather than in a scratchpad because /tmp is cleaned between sessions.
set -o pipefail
cd /home/crz/Programming/kyo
export SBT_OPTS="-Xmx6G -Xss10M -XX:MaxMetaspaceSize=1G -XX:ReservedCodeCacheSize=256M -Dfile.encoding=UTF-8"
export JAVA_OPTS="$SBT_OPTS"
# The args path must be ABSOLUTE: the cd above happens first, and sbt drops into an interactive
# shell on an empty command list rather than saying anything.
mapfile -t ARGS < "$1"
sbt -batch "${ARGS[@]}" < /dev/null
echo "SBT_EXIT=$?"
