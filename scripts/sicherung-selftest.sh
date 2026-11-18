#!/usr/bin/env bash
set -uo pipefail
#
# Self-test harness for scripts/sicherung.sh.
#
# Every case builds a throwaway repo with a bare remote named fork under a
# temp dir, runs the script there and asserts its exit code, its exact lines,
# the sicherung/* tags on both sides and that hauptlinie on the remote is
# where the case left it: the script never moves it. The last case runs the
# local pre-push guard as the real repo installs it (hook.local-guard), to
# show a force-push refused before the script and accepted after it. The
# global and system git config are cut off, so the user's hooks and signing
# never run. No case touches the real repo or any of its remotes.

SELF_DIR="$(cd "$(dirname "$0")" && pwd)"
SCRIPT="$SELF_DIR/sicherung.sh"

PASS=0; FAIL=0; TOTAL=0

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1
export GIT_AUTHOR_NAME=selftest GIT_AUTHOR_EMAIL=selftest@example.invalid
export GIT_COMMITTER_NAME=selftest GIT_COMMITTER_EMAIL=selftest@example.invalid
# The script names the day's Sicherung from KYO_TODAY, so no case depends on the real date.
export KYO_TODAY=2026-01-02
D=sicherung/$KYO_TODAY

CASE_N=0
CASE_DIR="" # root of the current case
REPO=""     # the repo the script runs in
REMOTE=""   # the bare remote, named fork in REPO
OUT=""      # stdout and stderr of the last run
RC=""       # exit code of the last run
HEAD_BEFORE="" # hauptlinie on the remote before the last run

# A fresh repo on hauptlinie with one commit, pushed to an empty bare remote `fork`.
new_case() {
    CASE_N=$((CASE_N+1))
    CASE_DIR="$WORK/case $CASE_N"
    REPO="$CASE_DIR/repo"; REMOTE="$CASE_DIR/fork.git"
    mkdir -p "$CASE_DIR"
    git init -q --bare "$REMOTE" || setup_failed "init remote"
    git init -q -b hauptlinie "$REPO" || setup_failed "init repo"
    git -C "$REPO" remote add fork "$REMOTE"
    commit "Block one"
    setup_push hauptlinie
}

# A failed setup step ends the run: an assertion after it would fail for the wrong reason.
setup_failed() { echo "SETUP FAILED in case $CASE_N: $*" >&2; exit 3; }
commit() { git -C "${2:-$REPO}" commit -q --allow-empty -m "$1" || setup_failed "commit $1"; }
sha() { git -C "$REPO" rev-parse "$1^{commit}"; }
short() { sha "$1" | cut -c1-10; }
setup_push() { git -C "$REPO" push -q --no-verify fork "$@" 2>/dev/null || setup_failed "push $*"; }
# Moves hauptlinie on the remote to a new commit, the way another machine would.
advance_remote() {
    commit "$1"
    setup_push --force hauptlinie
}
# Sets a Sicherung by hand on both sides, as it stood before the script existed.
setup_sicherung() {
    git -C "$REPO" tag "$1" "$2" || setup_failed "tag $1"
    setup_push "refs/tags/$1"
}

run_script() {
    HEAD_BEFORE=$(remote_sha refs/heads/hauptlinie)
    OUT=$(cd "$REPO" && "$SCRIPT" "$@" 2>&1)
    RC=$?
}

remote_sha() { git --git-dir="$REMOTE" rev-parse -q --verify "$1^{commit}" 2>/dev/null || echo none; }
# "<name> <commit>" per sicherung/* tag of the given git dir, sorted by name.
# An annotated tag prints its peeled commit first, so the first 40 digits are the commit.
tags_of() { git --git-dir="$1" for-each-ref --format='%(refname:short) %(*objectname)%(objectname)' 'refs/tags/sicherung/' | awk '{ print $1, substr($2, 1, 40) }'; }
remote_tags() { tags_of "$REMOTE"; }
local_tags() { tags_of "$REPO/.git"; }
script_lines() { printf '%s\n' "$OUT" | sed -n 's/^sicherung: //p'; }

record() {
    TOTAL=$((TOTAL+1))
    if [ "$1" = "ok" ]; then
        echo "  PASS: $2"; PASS=$((PASS+1))
    else
        echo "  FAIL: $2"; FAIL=$((FAIL+1))
        printf '    %s\n' "$3"
        printf '    exit %s, output:\n' "$RC"
        printf '%s\n' "$OUT" | sed 's/^/    | /'
    fi
}

# expect <name> <exit> <tags> <line>...: the exit code, the sicherung/* tags
# ("<name> <commit>" lines) on the remote and, identically, locally, the
# script's exact lines, and hauptlinie on the remote untouched.
expect() {
    local name="$1" want_rc="$2" want_tags="$3"; shift 3
    local want_lines; want_lines=$(printf '%s\n' "$@")
    if [ "$RC" != "$want_rc" ]; then record no "$name" "exit $RC, want $want_rc"
    elif [ "$(remote_tags)" != "$want_tags" ]; then record no "$name" "$(tags_differ remote "$(remote_tags)" "$want_tags")"
    elif [ "$(local_tags)" != "$want_tags" ]; then record no "$name" "$(tags_differ local "$(local_tags)" "$want_tags")"
    elif [ "$(remote_sha refs/heads/hauptlinie)" != "$HEAD_BEFORE" ]; then record no "$name" "the script moved hauptlinie on the remote"
    elif [ "$(script_lines)" != "$want_lines" ]; then
        record no "$name" "lines differ; want:"$'\n'"$(printf '%s\n' "$want_lines" | sed 's/^/    > /')"
    else record ok "$name"; fi
}

tags_differ() {
    printf '%s tags differ; got:\n%s\n    want:\n%s' "$1" "$(printf '%s\n' "$2" | sed 's/^/    < /')" "$(printf '%s\n' "$3" | sed 's/^/    > /')"
}
tags() { local t out=""; for t in "$@"; do out+="$t"$'\n'; done; printf '%s' "${out%$'\n'}"; }
set_line() { printf 'set %s on %s, the tip of fork/hauptlinie, and pushed it to fork' "$1" "$(short "$2")"; }
drop_line() { printf 'dropped %s, locally and on fork' "$1"; }

echo "Running sicherung-selftest.sh..."

# 1. The first Sicherung of the day sits on the remote tip, on both sides.
new_case
A=$(sha hauptlinie)
run_script
expect "the first Sicherung of the day is sicherung/<date> on the remote tip" 0 \
    "$(tags "$D $A")" \
    "$(set_line "$D" "$A")"

# 2. The tag sits on what the remote carries after a fresh fetch, not on the
# local tip: here the local hauptlinie is rewritten and the remote tip was
# pushed from another clone, so this repo has never seen it.
new_case
OTHER="$CASE_DIR/other"
git clone -q -b hauptlinie "$REMOTE" "$OTHER" || setup_failed "clone"
commit "Pushed from elsewhere" "$OTHER"
git -C "$OTHER" push -q --no-verify origin hauptlinie || setup_failed "push from other"
B=$(git -C "$OTHER" rev-parse hauptlinie)
git -C "$REPO" commit -q --amend --allow-empty -m "Block one, rewritten" || setup_failed "amend"
run_script
expect "the tag sits on the freshly fetched remote tip, not the local one" 0 \
    "$(tags "$D $B")" \
    "$(set_line "$D" "$B")"
if [ "$(sha hauptlinie)" = "$B" ] || [ "$(sha refs/remotes/fork/hauptlinie)" != "$B" ]; then
    record no "the fetch updates fork/hauptlinie and leaves the local branch alone" "local $(sha hauptlinie), fork/hauptlinie $(sha refs/remotes/fork/hauptlinie)"
else record ok "the fetch updates fork/hauptlinie and leaves the local branch alone"; fi

# 3. The second and third Sicherung of the day are -b and -c; the third drops
# the third-youngest, locally and on the remote, and two remain.
new_case
A=$(sha hauptlinie)
run_script
advance_remote "Block one, after review"
B=$(sha hauptlinie)
run_script
expect "the second Sicherung of the day is -b, and both remain" 0 \
    "$(tags "$D $A" "$D-b $B")" \
    "$(set_line "$D-b" "$B")"
advance_remote "Block one, after the second review"
C=$(sha hauptlinie)
run_script
expect "the third Sicherung of the day is -c and drops the third-youngest" 0 \
    "$(tags "$D-b $B" "$D-c $C")" \
    "$(set_line "$D-c" "$C")" \
    "$(drop_line "$D")"

# 4. The fourth of the day continues after the highest letter: sicherung/<date>
# was dropped by the third, and reusing it would make the youngest sort oldest.
advance_remote "Block one, after the third review"
E=$(sha hauptlinie)
run_script
expect "the fourth Sicherung of the day is -d, not the freed sicherung/<date>" 0 \
    "$(tags "$D-c $C" "$D-d $E")" \
    "$(set_line "$D-d" "$E")" \
    "$(drop_line "$D-b")"

# 5. Earlier days sort before today: the oldest of them falls.
new_case
A=$(sha hauptlinie)
setup_sicherung sicherung/2025-12-30 "$A"
advance_remote "Block one, after review"
B=$(sha hauptlinie)
setup_sicherung sicherung/2025-12-31-b "$B"
advance_remote "Block one, after the second review"
C=$(sha hauptlinie)
run_script
expect "a Sicherung of today drops the oldest of earlier days" 0 \
    "$(tags "sicherung/2025-12-31-b $B" "$D $C")" \
    "$(set_line "$D" "$C")" \
    "$(drop_line sicherung/2025-12-30)"

# Makes the remote refuse every pushed ref update matching the given awk test
# on "<old> <new> <ref>"; ls-remote and fetch still work.
refuse_on_remote() {
    printf '#!/bin/sh\nawk '\''%s { bad = 1 } END { exit bad }'\''\n' "$1" > "$REMOTE/hooks/pre-receive"
    chmod +x "$REMOTE/hooks/pre-receive"
}
ZERO=0000000000000000000000000000000000000000

# 6. When the tag cannot be pushed, the script fails and leaves no tag behind,
# so the next run starts from the same state.
new_case
refuse_on_remote '$1 ~ /^0+$/ && $3 ~ /^refs\/tags\//'
run_script
expect "a refused tag push fails and leaves no tag on either side" 1 "" \
    "could not push $D to fork"

# 7. When the old Sicherung cannot be deleted on the remote, the script fails;
# the new one stays, and the old one stays on both sides for the next run.
new_case
A=$(sha hauptlinie)
setup_sicherung sicherung/2025-12-30 "$A"
advance_remote "Block one, after review"
B=$(sha hauptlinie)
setup_sicherung sicherung/2025-12-31 "$B"
advance_remote "Block one, after the second review"
C=$(sha hauptlinie)
refuse_on_remote "\$2 == \"$ZERO\""
run_script
expect "a refused delete fails and keeps the old Sicherung on both sides" 1 \
    "$(tags "sicherung/2025-12-30 $A" "sicherung/2025-12-31 $B" "$D $C")" \
    "$(set_line "$D" "$C")" \
    "could not delete sicherung/2025-12-30 on fork"
rm "$REMOTE/hooks/pre-receive"
run_script
expect "the next run after a refused delete finishes the drop" 0 \
    "$(tags "sicherung/2025-12-31 $B" "$D $C")" \
    "fork/hauptlinie at ${C:0:10} already carries $D" \
    "$(drop_line sicherung/2025-12-30)"

# 8. A tip the youngest Sicherung already covers gets no second one.
new_case
A=$(sha hauptlinie)
run_script
run_script
expect "a tip the youngest Sicherung covers gets no new one" 0 \
    "$(tags "$D $A")" \
    "fork/hauptlinie at ${A:0:10} already carries $D"

# 9. A Sicherung that only an older one covers gets a new one, since the
# older one is the next to fall.
new_case
A=$(sha hauptlinie)
setup_sicherung sicherung/2025-12-30 "$A"
commit "Block two"
setup_push hauptlinie
B=$(sha hauptlinie)
setup_sicherung sicherung/2025-12-31 "$B"
setup_push --force "$A:refs/heads/hauptlinie"
run_script
expect "a tip covered only by an older Sicherung gets a new one" 0 \
    "$(tags "sicherung/2025-12-31 $B" "$D $A")" \
    "$(set_line "$D" "$A")" \
    "$(drop_line sicherung/2025-12-30)"

# 10. A Sicherung dated after today would sort as the youngest and keep the new
# one from being kept; the script refuses before it tags anything.
new_case
A=$(sha hauptlinie)
setup_sicherung sicherung/2026-01-03 "$A"
advance_remote "Block one, after review"
run_script
expect "a Sicherung dated after today is refused" 1 \
    "$(tags "sicherung/2026-01-03 $A")" \
    "sicherung/2026-01-03 is dated after today ($KYO_TODAY); check the clock, or delete the tag by hand"
# Only the remote decides: a local-only tag dated after today is no Sicherung and falls.
new_case
A=$(sha hauptlinie)
git -C "$REPO" tag sicherung/2026-01-03 "$A"
run_script
expect "a local-only tag dated after today falls instead of blocking" 0 \
    "$(tags "$D $A")" \
    "$(set_line "$D" "$A")" \
    "dropped sicherung/2026-01-03, which was only local"

# 11. The local side follows the remote: a local-only tag is no Sicherung and
# falls, a kept one missing here is fetched, a kept one on another commit here
# is moved to the remote's commit. Names taken only here are not reused.
new_case
A=$(sha hauptlinie)
setup_sicherung sicherung/2025-12-30 "$A"
setup_sicherung sicherung/2025-12-31 "$A"
git -C "$REPO" tag -d sicherung/2025-12-30 sicherung/2025-12-31 >/dev/null
advance_remote "Block one, after review"
B=$(sha hauptlinie)
git -C "$REPO" tag "$D" "$A"
run_script
expect "the local side ends with exactly the remote's two" 0 \
    "$(tags "sicherung/2025-12-31 $A" "$D-b $B")" \
    "$(set_line "$D-b" "$B")" \
    "dropped sicherung/2025-12-30 on fork, it was not here" \
    "dropped $D, which was only local"
new_case
A=$(sha hauptlinie)
setup_sicherung sicherung/2025-12-31 "$A"
advance_remote "Block one, after review"
B=$(sha hauptlinie)
git -C "$REPO" tag -f sicherung/2025-12-31 "$B" >/dev/null
advance_remote "Block one, after the second review"
C=$(sha hauptlinie)
run_script
expect "a kept Sicherung on another commit here is moved to the remote's" 0 \
    "$(tags "sicherung/2025-12-31 $A" "$D $C")" \
    "$(set_line "$D" "$C")"

# 12. Tags under sicherung/ that are not of the form sicherung/<date>[-b..-z]
# are no Sicherung: they are named, left alone, and never counted.
new_case
A=$(sha hauptlinie)
setup_sicherung sicherung/before-split "$A"
advance_remote "Block one, after review"
B=$(sha hauptlinie)
run_script
expect "a malformed sicherung/* tag is named and left alone" 0 \
    "$(tags "$D $B" "sicherung/before-split $A")" \
    "left sicherung/before-split alone: not of the form sicherung/<YYYY-MM-DD>[-b..-z]" \
    "$(set_line "$D" "$B")"

# 13. Without a reachable hauptlinie on the remote nothing is tagged.
new_case
git --git-dir="$REMOTE" update-ref -d refs/heads/hauptlinie
run_script
expect "a remote without hauptlinie is refused" 1 "" \
    "could not fetch hauptlinie from fork"
new_case
run_script nowhere
expect "a remote name that is not configured is refused" 1 "" \
    "nowhere is no remote of this repo (git remote -v)"

new_case
KYO_TODAY=2026-1-2 run_script
expect "a date not of the form YYYY-MM-DD is refused" 1 "" \
    "today's date 2026-1-2 is not of the form YYYY-MM-DD"

# 14. With the local pre-push guard installed as in the real repo, a
# force-push of hauptlinie is refused and names this script; after the script
# the same push goes through. The guard is not tracked (.git/info/exclude):
# the one next to this script is taken, and in a worktree, which has no copy,
# the one the shared hook.local-guard entry names.
GUARD="$SELF_DIR/hooks/pre-push-local"
[ -x "$GUARD" ] || GUARD=$(git -C "$SELF_DIR" config --get hook.local-guard.command 2>/dev/null || printf '%s' "$GUARD")
new_case
if [ ! -x "$GUARD" ]; then
    record no "the guard refuses the force-push before the script" "no pre-push guard at $GUARD"
else
    git -C "$REPO" config hook.local-guard.event pre-push
    git -C "$REPO" config hook.local-guard.command "$GUARD"
    A=$(sha hauptlinie)
    git -C "$REPO" commit -q --amend --allow-empty -m "Block one, rewritten" || setup_failed "amend"
    NEW=$(sha hauptlinie)
    PUSH_OUT=$(git -C "$REPO" push --force fork hauptlinie 2>&1); PUSH_RC=$?
    WANT_GUARD="pre-push-local: refused refs/heads/hauptlinie: this push overwrites ${A:0:10} on fork, and no Sicherung (sicherung/<YYYY-MM-DD>[-b..-z]) on fork points at it
pre-push-local: next: scripts/sicherung.sh fork (docs/arbeitsablauf.md, section 4), then push again"
    if [ "$PUSH_RC" = 0 ] || [ "$(remote_sha refs/heads/hauptlinie)" != "$A" ]; then
        record no "the guard refuses the force-push before the script" "push went through"
    elif [ "$(printf '%s\n' "$PUSH_OUT" | grep '^pre-push-local: ')" != "$WANT_GUARD" ]; then
        record no "the guard refuses the force-push before the script" "guard lines differ; got:"$'\n'"$PUSH_OUT"
    else record ok "the guard refuses the force-push before the script, naming it"; fi
    run_script
    expect "the script sets the Sicherung the guard asked for" 0 \
        "$(tags "$D $A")" \
        "$(set_line "$D" "$A")"
    PUSH_OUT=$(git -C "$REPO" push --force fork hauptlinie 2>&1); PUSH_RC=$?
    if [ "$PUSH_RC" != 0 ] || [ "$(remote_sha refs/heads/hauptlinie)" != "$NEW" ]; then
        record no "the guard lets the force-push through after the script" "exit $PUSH_RC:"$'\n'"$PUSH_OUT"
    else record ok "the guard lets the force-push through after the script"; fi
fi

echo ""
echo "Results: $PASS/$TOTAL passed, $FAIL failed"
[ "$FAIL" -eq 0 ]
