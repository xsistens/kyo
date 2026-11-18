#!/usr/bin/env bash
set -uo pipefail
#
# Self-test harness for scripts/publish-local.sh.
#
# Every case builds a throwaway repo, a throwaway Ivy directory and throwaway
# demo repos under a temp dir. sbt is a stand-in on PATH: it records its
# arguments and, for publishLocal, lays out what a real publish leaves in the
# Ivy directory. The real scripts/sbt.sh and sbt-heap-lib.sh sit in the
# throwaway repo, so the whole chain from publish-local.sh to the sbt command
# line runs. Each case asserts the exact output, the exit code and the
# recorded effect (sbt's arguments, the Ivy directory before and after). The
# global and system git config are cut off, so the user's hooks and signing
# never run. No case touches the real repo, the real Ivy directory or a demo.

SELF_DIR="$(cd "$(dirname "$0")" && pwd)"
SCRIPT="$SELF_DIR/publish-local.sh"

PASS=0; FAIL=0; TOTAL=0

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1
export GIT_AUTHOR_NAME=selftest GIT_AUTHOR_EMAIL=selftest@example.invalid
export GIT_COMMITTER_NAME=selftest GIT_COMMITTER_EMAIL=selftest@example.invalid
# A fixed runner size, so the heap sbt.sh picks does not depend on this machine.
export SBT_HEAP_MEMORY_MB=32768
unset SBT_OPTS JAVA_OPTS JVM_OPTS

STUB_BIN="$WORK/bin"
mkdir -p "$STUB_BIN"
# The sbt stand-in. It writes one argument per line to $SBT_LOG. On kyoJVM/publishLocal and
# kyoJS/publishLocal it creates kyo-core of that platform under $KYO_IVY_LOCAL at the version of
# the last `set ThisBuild / version`, with a docs/ directory when STUB_SBT_DOCS=1. It exits with
# STUB_SBT_EXIT, and with STUB_SBT_EXIT set it publishes nothing. STUB_SBT_ONLY=jvm skips the JS publish.
cat > "$STUB_BIN/sbt" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$@" > "$SBT_LOG"
if [ -n "${STUB_SBT_EXIT:-}" ]; then exit "$STUB_SBT_EXIT"; fi
version=""
for arg in "$@"; do
    case "$arg" in
        'set ThisBuild / version := "'*'"') version="${arg#*\"}"; version="${version%\"}" ;;
        kyoJVM/publishLocal) module=kyo-core_3 ;;
        kyoJS/publishLocal) if [ "${STUB_SBT_ONLY:-}" = jvm ]; then continue; fi; module=kyo-core_sjs1_3 ;;
        *) continue ;;
    esac
    case "$arg" in
        */publishLocal)
            mkdir -p "$KYO_IVY_LOCAL/$module/$version/jars" "$KYO_IVY_LOCAL/$module/$version/ivys"
            echo jar > "$KYO_IVY_LOCAL/$module/$version/jars/$module.jar"
            if [ "${STUB_SBT_DOCS:-}" = 1 ]; then mkdir -p "$KYO_IVY_LOCAL/$module/$version/docs"; fi ;;
    esac
done
exit 0
EOF
chmod +x "$STUB_BIN/sbt"
export PATH="$STUB_BIN:$PATH"

# What sbt.sh passes for the publish role at the fixed runner size.
HEAP=$(. "$SELF_DIR/sbt-heap-lib.sh" 2>/dev/null && sbt_heap publish)

CASE_N=0
CASE_DIR="" # root of the current case
REPO=""     # main checkout of the current case
IVY=""      # Ivy directory of the current case (stands for ~/.ivy2/local/io.getkyo)
DEMO_A=""; DEMO_B=""
OUT=""      # stdout and stderr of the last run
RC=""       # exit code of the last run

# A fresh repo shaped like the Hauptlinie: tag v1.0.0-RC7 on an upstream commit, then a [local]
# commit that carries scripts/publish-local.sh, sbt.sh, sbt-heap-lib.sh and an sbt 2 build.properties.
new_case() {
    CASE_N=$((CASE_N+1))
    CASE_DIR="$WORK/case $CASE_N"
    REPO="$CASE_DIR/repo"; IVY="$CASE_DIR/ivy/io.getkyo"
    DEMO_A="$CASE_DIR/demo-a"; DEMO_B="$CASE_DIR/demo-b"
    mkdir -p "$REPO/scripts" "$REPO/project" "$IVY"
    git -C "$REPO" init -q -b hauptlinie
    echo readme > "$REPO/README"
    git -C "$REPO" add -A && git -C "$REPO" commit -q -m upstream || setup_failed "upstream commit"
    git -C "$REPO" tag v1.0.0-RC7
    echo later > "$REPO/LATER"
    git -C "$REPO" add -A && git -C "$REPO" commit -q -m "after the release" || setup_failed "upstream commit"
    git -C "$REPO" branch upstream-main
    cp "$SCRIPT" "$SELF_DIR/sbt.sh" "$SELF_DIR/sbt-heap-lib.sh" "$REPO/scripts/"
    # Stand-ins for the four staging scripts: each appends its path to $CASE_DIR/stage.log and fails
    # when STUB_STAGE_FAIL names it.
    local stage
    for stage in kyo-net/build/boringssl/build-boringssl.sh kyo-aeron/scripts/build-aeron.sh \
        kyo-sql-doltlite/scripts/build-doltlite.sh kyo-sql-sqlite/scripts/build-sqlite.sh; do
        mkdir -p "$REPO/$(dirname "$stage")"
        printf '%s\n' "echo $stage >> \"$CASE_DIR/stage.log\"" \
            "if [ \"\${STUB_STAGE_FAIL:-}\" = $stage ]; then echo 'stage broke'; exit 5; fi" > "$REPO/$stage"
    done
    echo "sbt.version=2.1.0-RC3" > "$REPO/project/build.properties"
    git -C "$REPO" add -A && git -C "$REPO" commit -q -m "[local] scripts" || setup_failed "local commit"
}

setup_failed() { echo "SETUP FAILED in case $CASE_N: $*" >&2; exit 3; }

# A demo repo whose branch $2 carries the files given as path=content pairs, committed.
demo_branch() {
    local demo="$1" branch="$2"; shift 2
    if [ ! -d "$demo/.git" ]; then
        git init -q -b "$branch" "$demo" || setup_failed "demo init"
    else
        git -C "$demo" checkout -q --orphan "$branch" || setup_failed "demo branch $branch"
        git -C "$demo" rm -q -rf . >/dev/null 2>&1 || setup_failed "demo clear $branch"
    fi
    local pair
    for pair in "$@"; do
        mkdir -p "$(dirname "$demo/${pair%%=*}")"
        printf '%s\n' "${pair#*=}" > "$demo/${pair%%=*}"
    done
    git -C "$demo" add -A && git -C "$demo" commit -q -m "$branch" || setup_failed "demo commit $branch"
}

# Module $1 (a path for an sbt plugin, as kyo-ffi-plugin/scala_2.12/sbt_1.0) at version $2 in the
# Ivy directory, with $3 bytes of jar.
ivy_version() {
    mkdir -p "$IVY/$1/$2/jars" "$IVY/$1/$2/ivys"
    head -c "$3" /dev/zero > "$IVY/$1/$2/jars/${1%%/*}.jar"
    echo '<ivy-module/>' > "$IVY/$1/$2/ivys/ivy.xml"
}

run_publish() {
    local dir="${1:-$REPO}"; shift || true
    OUT=$(cd "$dir" && SBT_LOG="$CASE_DIR/sbt.log" KYO_IVY_LOCAL="$IVY" "$SCRIPT" "$@" 2>&1)
    RC=$?
}

# The answer $1 on stdin; $2 replaces the demo list.
run_prune() {
    local input="$1" demos="${2:-$DEMO_A:$DEMO_B}"
    OUT=$(cd "$REPO" && printf '%s' "$input" | KYO_IVY_LOCAL="$IVY" KYO_DEMOS="$demos" "$SCRIPT" --prune 2>&1)
    RC=$?
}

record() {
    TOTAL=$((TOTAL+1))
    if [ "$1" = "ok" ]; then
        echo "  PASS: $2"; PASS=$((PASS+1))
    else
        echo "  FAIL: $2"; FAIL=$((FAIL+1))
        printf '    exit %s, output:\n' "$RC"
        printf '%s\n' "$OUT" | sed 's/^/    | /'
        if [ -n "${3:-}" ]; then printf '%s\n' "$3" | sed 's/^/    > /'; fi
    fi
}

# The output is exactly the given lines (in order), with the given exit code.
expect() {
    local rc="$1" name="$2"; shift 2
    local want=""
    if [ $# -gt 0 ]; then want=$(printf '%s\n' "$@"); fi
    if [ "$RC" = "$rc" ] && [ "$OUT" = "$want" ]
    then record ok "$name"
    else record no "$name" "wanted exit $rc and:${want:+$'\n'$want}"; fi
}

# A condition on the recorded effect.
check() {
    local name="$1"; shift
    if "$@"; then record ok "$name"; else record no "$name"; fi
}

# Neither a staging script nor sbt ran.
sbt_not_started() { [ ! -e "$CASE_DIR/sbt.log" ] && [ ! -e "$CASE_DIR/stage.log" ]; }

# The staging scripts ran in exactly this order.
staged_are() { [ "$(cat "$CASE_DIR/stage.log" 2>/dev/null)" = "$(printf '%s\n' "$@")" ]; }
STAGES=(kyo-net/build/boringssl/build-boringssl.sh kyo-aeron/scripts/build-aeron.sh
    kyo-sql-doltlite/scripts/build-doltlite.sh kyo-sql-sqlite/scripts/build-sqlite.sh)

# sbt got exactly these arguments, after the heap and --server that sbt.sh adds.
sbt_args_are() {
    local want got
    want=$(printf '%s\n' "$HEAP" --server "$@")
    got=$(cat "$CASE_DIR/sbt.log" 2>/dev/null)
    [ "$got" = "$want" ] || { printf '    sbt got:\n'; printf '%s\n' "$got" | sed 's/^/    | /'; return 1; }
}

# The version directories (those holding ivys/) in the Ivy directory as <module path>/<version>, sorted.
ivy_listing() { (cd "$IVY" && find . -mindepth 3 -type d -name ivys -printf '%h\n' | sed 's|^\./||' | LC_ALL=C sort); }

# Everything under the case: files with sizes, so a deletion or a change shows.
snapshot() { (cd "$CASE_DIR" && find . -path ./sbt.log -prune -o -printf '%p %y %s\n' | LC_ALL=C sort); }

sha9() { git -C "${2:-$REPO}" rev-parse "$1" | cut -c1-9; }

publish_args() {
    printf '%s\n' "set ThisBuild / version := \"$1\"" \
        "set ThisBuild / Compile / packageDoc / publishArtifact := false" \
        kyoJVM/publishLocal kyoJS/publishLocal
}

echo "Running publish-local-selftest.sh..."

# --- Publishing ---

# 1. A clean Hauptlinie publishes JVM and JS at <prefix>+<sha9>-SNAPSHOT and ends with the demo's version line.
new_case
V="1.0.0-RC7+$(sha9 HEAD)-SNAPSHOT"
run_publish
expect 0 "a clean tree publishes and ends with the version line" \
    "sbt.sh: role publish, $HEAP (runner memory 32768MB)" \
    "publish-local: published $V for JVM and JS" \
    "val KyoVersion = \"$V\""
mapfile -t ARGS < <(publish_args "$V")
check "sbt sets the version, turns off doc jars and publishes both aggregates" sbt_args_are "${ARGS[@]}"
check "the four natives are staged before sbt, in CI's order" staged_are "${STAGES[@]}"
check "both platforms are in the Ivy directory at that version" \
    test "$(ivy_listing)" = "$(printf '%s\n' "kyo-core_3/$V" "kyo-core_sjs1_3/$V")"

# 1a. The version line is the one for the given commit, not a prefix of a longer hash or another commit.
new_case
git -C "$REPO" commit -q --allow-empty -m "[local] one more" || setup_failed "commit"
V="1.0.0-RC7+$(sha9 HEAD)-SNAPSHOT"
run_publish
check "the version names HEAD's first nine hex digits" test "$(printf '%s\n' "$OUT" | tail -1)" = "val KyoVersion = \"$V\""
check "sbt publishes HEAD's version, not its parent's" grep -qxF "set ThisBuild / version := \"$V\"" "$CASE_DIR/sbt.log"

# 1b. The prefix follows the nearest upstream tag reachable from HEAD; a tag off HEAD's history and
# a tag of another shape do not count.
new_case
git -C "$REPO" tag v1.0.0-RC8 upstream-main
git -C "$REPO" tag archive/later HEAD
git -C "$REPO" checkout -q -b side upstream-main && git -C "$REPO" commit -q --allow-empty -m side \
    && git -C "$REPO" tag v1.0.0-RC9 && git -C "$REPO" checkout -q hauptlinie || setup_failed "side tag"
V="1.0.0-RC8+$(sha9 HEAD)-SNAPSHOT"
run_publish
check "the prefix is the nearest reachable upstream tag" test "$(printf '%s\n' "$OUT" | tail -1)" = "val KyoVersion = \"$V\""

# 1c. Without a reachable upstream tag there is no prefix, and sbt never starts.
new_case
git -C "$REPO" tag -d v1.0.0-RC7 >/dev/null
run_publish
expect 1 "without an upstream tag publishing stops" \
    "publish-local: no upstream tag v<version> is reachable from HEAD"
check "without an upstream tag sbt is not started" sbt_not_started

# 1d. Outside a repository there is nothing to publish.
new_case
mkdir -p "$CASE_DIR/plain"
GIT_CEILING_DIRECTORIES="$CASE_DIR" run_publish "$CASE_DIR/plain"
expect 1 "outside a repository publishing stops" \
    "publish-local: not inside a git repository"
check "outside a repository sbt is not started" sbt_not_started

# 1e. An experiment worktree publishes its own HEAD by the same scheme.
new_case
git -C "$REPO" worktree add -q -b exp/probe "$REPO/.worktrees/exp-probe" >/dev/null 2>&1 || setup_failed "worktree"
git -C "$REPO/.worktrees/exp-probe" commit -q --allow-empty -m "probe" || setup_failed "commit"
V="1.0.0-RC7+$(sha9 HEAD "$REPO/.worktrees/exp-probe")-SNAPSHOT"
run_publish "$REPO/.worktrees/exp-probe"
expect 0 "an experiment worktree publishes its own HEAD" \
    "sbt.sh: role publish, $HEAP (runner memory 32768MB)" \
    "publish-local: published $V for JVM and JS" \
    "val KyoVersion = \"$V\""

# 2. Unclean trees: a modified file, a staged file, an untracked file. sbt never starts.
new_case
echo change >> "$REPO/README"
run_publish
expect 1 "a modified file aborts" \
    "publish-local: the working tree is not clean; commit it first (to its experiment branch):" \
    " M README"
check "with a modified file sbt is not started" sbt_not_started

new_case
echo staged > "$REPO/STAGED"; git -C "$REPO" add STAGED
run_publish
expect 1 "a staged file aborts" \
    "publish-local: the working tree is not clean; commit it first (to its experiment branch):" \
    "A  STAGED"
check "with a staged file sbt is not started" sbt_not_started

new_case
mkdir -p "$REPO/src"; echo 'object New' > "$REPO/src/New.scala"
run_publish
expect 1 "an untracked file aborts" \
    "publish-local: the working tree is not clean; commit it first (to its experiment branch):" \
    "?? src/New.scala"
check "with an untracked file sbt is not started" sbt_not_started

# 3. PR worktrees never publish: a pr/* branch, the fixed name ui-mounted, and a tree without the local layer.
new_case
git -C "$REPO" worktree add -q -b pr/a1-fix "$REPO/.worktrees/pr-a1-fix" >/dev/null 2>&1 || setup_failed "worktree"
run_publish "$REPO/.worktrees/pr-a1-fix"
expect 1 "a pr/* branch refuses" \
    "publish-local: pr/a1-fix is a PR branch; PR worktrees never publish"
check "on a pr/* branch sbt is not started" sbt_not_started

new_case
git -C "$REPO" worktree add -q -b ui-mounted "$REPO/.worktrees/ui-mounted" >/dev/null 2>&1 || setup_failed "worktree"
run_publish "$REPO/.worktrees/ui-mounted"
expect 1 "ui-mounted refuses" \
    "publish-local: ui-mounted is a PR branch; PR worktrees never publish"
check "on ui-mounted sbt is not started" sbt_not_started

new_case
git -C "$REPO" worktree add -q -b exp/from-upstream "$REPO/.worktrees/exp-from-upstream" upstream-main >/dev/null 2>&1 || setup_failed "worktree"
run_publish "$REPO/.worktrees/exp-from-upstream"
expect 1 "a tree without the local layer refuses" \
    "publish-local: HEAD carries no scripts/publish-local.sh, so it has no local layer (a PR worktree or upstream); it never publishes"
check "without the local layer sbt is not started" sbt_not_started

# 3a. An unclean PR worktree is told it is a PR worktree, not to commit to an experiment branch.
new_case
git -C "$REPO" worktree add -q -b pr/a2-fix "$REPO/.worktrees/pr-a2-fix" >/dev/null 2>&1 || setup_failed "worktree"
echo change >> "$REPO/.worktrees/pr-a2-fix/README"
run_publish "$REPO/.worktrees/pr-a2-fix"
expect 1 "an unclean pr/* branch refuses as a PR branch" \
    "publish-local: pr/a2-fix is a PR branch; PR worktrees never publish"

# 4. A failing sbt run prints no version line and passes the failure on.
new_case
STUB_SBT_EXIT=7 run_publish
expect 7 "a failing sbt prints no version line" \
    "sbt.sh: role publish, $HEAP (runner memory 32768MB)" \
    "publish-local: sbt failed (exit 7); nothing to put into a demo"

# 4'. A staging script that fails stops the publish before sbt and before the scripts after it.
new_case
STUB_STAGE_FAIL=kyo-aeron/scripts/build-aeron.sh run_publish
expect 1 "a failing staging script stops the publish" \
    "stage broke" \
    "publish-local: staging with kyo-aeron/scripts/build-aeron.sh failed (exit 5); sbt not started"
check "after a failing staging script sbt is not started" test ! -e "$CASE_DIR/sbt.log"
check "the staging scripts after the failing one do not run" staged_are "${STAGES[@]:0:2}"

# 4a. A publish that leaves doc jars, or misses a platform, is not a publish of this scheme.
new_case
V="1.0.0-RC7+$(sha9 HEAD)-SNAPSHOT"
STUB_SBT_DOCS=1 run_publish
expect 1 "a publish that left doc jars fails" \
    "sbt.sh: role publish, $HEAP (runner memory 32768MB)" \
    "publish-local: sbt published doc jars: $IVY/kyo-core_3/$V/docs $IVY/kyo-core_sjs1_3/$V/docs"

new_case
V="1.0.0-RC7+$(sha9 HEAD)-SNAPSHOT"
STUB_SBT_ONLY=jvm run_publish
expect 1 "a publish that misses a platform fails" \
    "sbt.sh: role publish, $HEAP (runner memory 32768MB)" \
    "publish-local: sbt left no $IVY/kyo-core_sjs1_3/$V"

# 5. Arguments other than none or --prune alone.
for args in "--frobnicate" "--prune --prune" "''"; do
    new_case
    eval "run_publish \"\$REPO\" $args"
    expect 2 "the arguments ($args) are a usage error" \
        "usage: scripts/publish-local.sh [--prune]"
    check "with the arguments ($args) sbt is not started" sbt_not_started
done

# --- Pruning ---

# A demo layout used by the prune cases: demo A names one version on main and another on a
# branch; demo B names three more in project/Dependencies.scala, a nested build.sbt and a nested
# project/ source. A mention outside a build file, in a comment without quotes, or only in a
# working tree does not count. Everything else falls, sbt plugin versions included.
prune_layout() {
    demo_branch "$DEMO_A" main \
        'build.sbt=val KyoVersion = "1.0.0-RC7+aaaaaaaaa-SNAPSHOT" // was 1.0.0-RC5-old-SNAPSHOT' \
        'README.md=Built against "1.0.0-RC5-old-SNAPSHOT" once.'
    demo_branch "$DEMO_A" feature \
        'build.sbt=val KyoVersion = "1.0.0-RC7+bbbbbbbbb-SNAPSHOT"'
    git -C "$DEMO_A" checkout -q main
    demo_branch "$DEMO_B" master \
        'build.sbt=lazy val root = project' \
        'project/Dependencies.scala=object Dependencies { val kyo = "1.0.0-RC7+ccccccccc-SNAPSHOT" }' \
        'examples/one/build.sbt=libraryDependencies += "io.getkyo" %% "kyo-core" % "1.0.0-RC7+eeeeeeeee-SNAPSHOT"' \
        'project/sub/Versions.scala=val v = "1.0.0-RC7+fffffffff-SNAPSHOT"'
    # A literal only in the working tree of a demo is not the current state of a branch.
    echo 'val KyoVersion = "1.0.0-RC7+ddddddddd-SNAPSHOT"' > "$DEMO_B/build.sbt"
    ivy_version kyo-core_3 1.0.0-RC7+aaaaaaaaa-SNAPSHOT 1024
    ivy_version kyo-core_sjs1_3 1.0.0-RC7+aaaaaaaaa-SNAPSHOT 1024
    ivy_version kyo-core_3 1.0.0-RC7+bbbbbbbbb-SNAPSHOT 1024
    ivy_version kyo-core_3 1.0.0-RC7+ccccccccc-SNAPSHOT 1024
    ivy_version kyo-core_3 1.0.0-RC7+ddddddddd-SNAPSHOT 2048
    ivy_version kyo-core_3 1.0.0-RC5-old-SNAPSHOT 4096
    ivy_version kyo-core_sjs1_3 1.0.0-RC5-old-SNAPSHOT 4096
    ivy_version kyo-ui_sjs1_3 1.0.0-RC5-old-SNAPSHOT 4096
    ivy_version kyo-core_3 1.0.0-RC7+eeeeeeeee-SNAPSHOT 512
    ivy_version kyo-core_3 1.0.0-RC7+fffffffff-SNAPSHOT 512
    ivy_version kyo-ffi-plugin/scala_2.12/sbt_1.0 1.0.0-RC7+aaaaaaaaa-SNAPSHOT 256
    ivy_version kyo-ffi-plugin/scala_2.12/sbt_1.0 1.0.0-RC5-old-SNAPSHOT 256
    ivy_version kyo-doctest-plugin/scala_2.12/sbt_1.0 1.0.0-RC5-plug-SNAPSHOT 256
}

# The size of every directory of the given versions, at any depth, as --prune prints it.
size_of() {
    local dirs=() version
    for version in "$@"; do
        mapfile -t -O ${#dirs[@]} dirs < <(find "$IVY" -mindepth 3 -type d -name ivys -path "*/$version/ivys" -printf '%h\n')
    done
    du -sbc "${dirs[@]}" | tail -1 | cut -f1 | numfmt --to=iec-i --suffix=B
}

# 6. --prune lists exactly the versions no literal names, with size, and deletes nothing without "y".
for answer in "" "n" "no" "yes please"; do
    new_case
    prune_layout
    old_size=$(size_of 1.0.0-RC5-old-SNAPSHOT)
    plug_size=$(size_of 1.0.0-RC5-plug-SNAPSHOT)
    working_tree_size=$(size_of 1.0.0-RC7+ddddddddd-SNAPSHOT)
    falling_size=$(size_of 1.0.0-RC5-old-SNAPSHOT 1.0.0-RC5-plug-SNAPSHOT 1.0.0-RC7+ddddddddd-SNAPSHOT)
    before=$(snapshot)
    run_prune "${answer:+$answer$'\n'}"
    after=$(snapshot)
    expect 0 "--prune lists what falls and keeps it on answer '${answer}'" \
        "Kept, named by a demo branch:" \
        "  1.0.0-RC7+aaaaaaaaa-SNAPSHOT"$'\t'"3 modules" \
        "  1.0.0-RC7+bbbbbbbbb-SNAPSHOT"$'\t'"1 module" \
        "  1.0.0-RC7+ccccccccc-SNAPSHOT"$'\t'"1 module" \
        "  1.0.0-RC7+eeeeeeeee-SNAPSHOT"$'\t'"1 module" \
        "  1.0.0-RC7+fffffffff-SNAPSHOT"$'\t'"1 module" \
        "Falls, named by no demo branch:" \
        "  1.0.0-RC5-old-SNAPSHOT"$'\t'"4 modules"$'\t'"$old_size" \
        "  1.0.0-RC5-plug-SNAPSHOT"$'\t'"1 module"$'\t'"$plug_size" \
        "  1.0.0-RC7+ddddddddd-SNAPSHOT"$'\t'"1 module"$'\t'"$working_tree_size" \
        "Delete 3 versions ($falling_size)? [y/N] nothing deleted"
    check "--prune deletes nothing on answer '${answer}'" test "$before" = "$after"
done

# 6a. Only "y" deletes, and it deletes exactly what was listed; a module it emptied goes too.
for answer in y Y; do
    new_case
    prune_layout
    falling_size=$(size_of 1.0.0-RC5-old-SNAPSHOT 1.0.0-RC5-plug-SNAPSHOT 1.0.0-RC7+ddddddddd-SNAPSHOT)
    run_prune "$answer"$'\n'
    check "--prune on '$answer' reports the deletion" \
        test "$(printf '%s\n' "$OUT" | tail -1)" = "Delete 3 versions ($falling_size)? [y/N] deleted 3 versions"
    check "--prune on '$answer' leaves exactly the named versions" \
        test "$(ivy_listing)" = "$(printf '%s\n' \
            kyo-core_3/1.0.0-RC7+aaaaaaaaa-SNAPSHOT kyo-core_3/1.0.0-RC7+bbbbbbbbb-SNAPSHOT \
            kyo-core_3/1.0.0-RC7+ccccccccc-SNAPSHOT kyo-core_3/1.0.0-RC7+eeeeeeeee-SNAPSHOT \
            kyo-core_3/1.0.0-RC7+fffffffff-SNAPSHOT kyo-core_sjs1_3/1.0.0-RC7+aaaaaaaaa-SNAPSHOT \
            kyo-ffi-plugin/scala_2.12/sbt_1.0/1.0.0-RC7+aaaaaaaaa-SNAPSHOT)"
    check "--prune on '$answer' removes the modules it emptied" \
        test ! -e "$IVY/kyo-ui_sjs1_3" -a ! -e "$IVY/kyo-doctest-plugin"
    check "--prune on '$answer' leaves the demos as they were" \
        test "$(git -C "$DEMO_B" status --porcelain)" = " M build.sbt"
done

# 6b. Nothing to prune: every version is named.
new_case
demo_branch "$DEMO_A" main 'build.sbt=val KyoVersion = "1.0.0-RC7+aaaaaaaaa-SNAPSHOT"'
demo_branch "$DEMO_B" master 'build.sbt=val KyoVersion = "1.0.0-RC7+aaaaaaaaa-SNAPSHOT"'
ivy_version kyo-core_3 1.0.0-RC7+aaaaaaaaa-SNAPSHOT 10
run_prune ""
expect 0 "--prune with nothing to delete asks nothing" \
    "Kept, named by a demo branch:" \
    "  1.0.0-RC7+aaaaaaaaa-SNAPSHOT"$'\t'"1 module" \
    "Nothing falls."

# 6c. A demo that cannot be read stops the prune before it lists or deletes anything.
new_case
prune_layout
rm -rf "$DEMO_B"
before=$(snapshot)
run_prune "y"$'\n'
after=$(snapshot)
expect 2 "a missing demo stops the prune" \
    "publish-local: cannot read the demo '$DEMO_B'; nothing deleted"
check "a missing demo deletes nothing" test "$before" = "$after"

new_case
prune_layout
git -C "$DEMO_B" checkout -q --detach && git -C "$DEMO_B" branch -q -D master
before=$(snapshot)
run_prune "y"$'\n'
after=$(snapshot)
expect 2 "a demo without branches stops the prune" \
    "publish-local: the demo $DEMO_B has no branch; nothing deleted"
check "a demo without branches deletes nothing" test "$before" = "$after"

# 6c'. A build file whose content git cannot read stops the prune too.
new_case
prune_layout
blob=$(git -C "$DEMO_B" rev-parse master:project/Dependencies.scala)
rm -f "$DEMO_B/.git/objects/${blob:0:2}/${blob:2}"
before=$(snapshot)
run_prune "y"$'\n'
after=$(snapshot)
check "an unreadable build file stops the prune" test "$RC" = 2 -a "$(printf '%s\n' "$OUT" | tail -1)" = \
    "publish-local: cannot read project/Dependencies.scala on refs/heads/master of the demo $DEMO_B; nothing deleted"
check "an unreadable build file deletes nothing" test "$before" = "$after"

# 6c''. An empty entry in the demo list would read the current directory as a demo.
new_case
prune_layout
before=$(snapshot)
run_prune "y"$'\n' "$DEMO_A::$DEMO_B"
after=$(snapshot)
expect 2 "an empty demo entry stops the prune" \
    "publish-local: cannot read the demo ''; nothing deleted"
check "an empty demo entry deletes nothing" test "$before" = "$after"

# 6d. A missing Ivy directory is an error, not an empty prune.
new_case
prune_layout
rm -rf "$IVY"
run_prune ""
expect 2 "a missing Ivy directory stops the prune" \
    "publish-local: no Ivy directory at $IVY"

# 6d'. An empty Ivy directory has nothing to keep or delete.
new_case
prune_layout
rm -rf "$IVY"; mkdir -p "$IVY"
run_prune ""
expect 0 "an empty Ivy directory has nothing to prune" "Nothing falls."

# 6e. --prune publishes nothing and runs in an unclean tree too.
new_case
prune_layout
echo change >> "$REPO/README"
run_prune ""
check "--prune does not start sbt" sbt_not_started
check "--prune runs in an unclean tree" test "$RC" = 0

echo ""
echo "publish-local-selftest.sh: $PASS/$TOTAL passed"
[ "$FAIL" = 0 ]
