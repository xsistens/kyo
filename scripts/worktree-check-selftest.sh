#!/usr/bin/env bash
set -uo pipefail
#
# Self-test harness for scripts/worktree-check.sh.
#
# Every case builds a throwaway repo with its own Orca directory under a temp
# dir, lays out the worktrees the case needs, runs the check from the main
# checkout and asserts its exact report lines and exit code. The global and
# system git config are cut off, so the user's hooks and signing never run.
# No case touches the real repo.

SELF_DIR="$(cd "$(dirname "$0")" && pwd)"
CHECK="$SELF_DIR/worktree-check.sh"

PASS=0; FAIL=0; TOTAL=0

WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1
export GIT_AUTHOR_NAME=selftest GIT_AUTHOR_EMAIL=selftest@example.invalid
export GIT_COMMITTER_NAME=selftest GIT_COMMITTER_EMAIL=selftest@example.invalid

TODAY=$(date +%F)
YESTERDAY=$(date -d yesterday +%F)
TWO_DAYS_AGO=$(date -d '2 days ago' +%F)
TOMORROW=$(date -d tomorrow +%F)
TAB=$'\t'

CASE_N=0
CASE_DIR="" # root of the current case
MAIN=""     # main checkout of the current case
ORCA=""     # Orca directory of the current case
OUT=""      # stdout and stderr of the last run
RC=""       # exit code of the last run

# A fresh repo whose single commit is dated 2026-01-02, so the age of an Orca
# branch on it has a fixed date part. docs/ is tracked, as in kyo, so a
# symlink under docs/ shows as itself and not as an untracked docs/.
new_case() {
    CASE_N=$((CASE_N+1))
    CASE_DIR="$WORK/case $CASE_N"
    MAIN="$CASE_DIR/repo"; ORCA="$CASE_DIR/orca"
    mkdir -p "$MAIN/docs" "$ORCA" "$CASE_DIR/shared/agents" "$CASE_DIR/shared/adr"
    git -C "$MAIN" init -q -b hauptlinie
    echo readme > "$MAIN/README"; echo docs > "$MAIN/docs/README"
    git -C "$MAIN" add -A
    GIT_COMMITTER_DATE=2026-01-02T12:00:00 GIT_AUTHOR_DATE=2026-01-02T12:00:00 \
        git -C "$MAIN" commit -q -m base
    # Targets for the section 8 links, standing in for the main checkout's local files.
    for f in CLAUDE.local.md GLOSSARY.md arbeitsablauf.md katalog.md; do echo "$f" > "$CASE_DIR/shared/$f"; done
}

# A failed setup step ends the run: an assertion after it would fail for the wrong reason.
setup_failed() { echo "SETUP FAILED in case $CASE_N: $*" >&2; exit 3; }
add_worktree() { git -C "$MAIN" worktree add -q "$@" >/dev/null 2>&1 || setup_failed "worktree add $*"; }
lock_worktree() { git -C "$MAIN" worktree lock --reason "$2" "$1" || setup_failed "worktree lock $1"; }
age_of() { git -C "$MAIN" log -1 --format=%cr "$1"; }

run_check() {
    OUT=$(cd "${1:-$MAIN}" && KYO_ORCA_DIR="$ORCA" "$CHECK" 2>&1)
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
    fi
}

# The report is exactly the given lines (in order), with the given exit code.
expect() {
    local rc="$1" name="$2"; shift 2
    local want=""
    if [ $# -gt 0 ]; then want=$(printf '%s\n' "$@"); fi
    if [ "$RC" = "$rc" ] && [ "$OUT" = "$want" ]
    then record ok "$name"
    else record no "$name"; fi
}

orphan() { printf '  %s\t%s\t%s' "$1" "$2" "$3"; }
open_ws() { printf '  %s\t%s\tlast commit 2026-01-02 (%s)' "$1" "$2" "$(age_of "$2")"; }

# Everything the check could change: files, links, worktree registrations and locks, refs, stashes.
snapshot() {
    (cd "$CASE_DIR" && find . -printf '%p %y %s %l\n' | sort)
    git -C "$MAIN" worktree list --porcelain
    git -C "$MAIN" for-each-ref
    git -C "$MAIN" stash list
}

echo "Running worktree-check-selftest.sh..."

# 1. Clean: a main checkout alone reports nothing.
new_case
run_check
expect 0 "a main checkout alone reports nothing"

# 1a. Clean: worktrees at .worktrees/<branch with - for />, unlocked, report nothing.
new_case
add_worktree -b exp/foo "$MAIN/.worktrees/exp-foo"
add_worktree -b pr/a1-fix "$MAIN/.worktrees/pr-a1-fix"
run_check
expect 0 "worktrees at .worktrees/<branch with - for /> report nothing"

# 2. Wrong place: outside .worktrees/, unlocked.
new_case
add_worktree -b exp/foo "$CASE_DIR/elsewhere/foo"
run_check
expect 1 "a worktree outside .worktrees/ is orphaned" \
    "Orphaned worktrees:" \
    "$(orphan "$CASE_DIR/elsewhere/foo" exp/foo "path outside .worktrees/; not locked")"

# 2a. Wrong place: under .worktrees/ with a name that is not its branch.
new_case
add_worktree -b exp/foo "$MAIN/.worktrees/foo"
run_check
expect 1 "a worktree under .worktrees/ named other than its branch is orphaned" \
    "Orphaned worktrees:" \
    "$(orphan "$MAIN/.worktrees/foo" exp/foo "path is not .worktrees/exp-foo; not locked")"

# 3. Harness name in .claude/worktrees/, unlocked: both rules are named.
new_case
add_worktree -b worktree-agent-a1b2 "$MAIN/.claude/worktrees/agent-a1b2"
run_check
expect 1 "a harness worktree left unlocked is orphaned" \
    "Orphaned worktrees:" \
    "$(orphan "$MAIN/.claude/worktrees/agent-a1b2" worktree-agent-a1b2 "path outside .worktrees/; harness name; not locked")"

# 3a. Harness name at its own .worktrees/ place is still a harness name.
new_case
add_worktree -b worktree-agent-c3 "$MAIN/.worktrees/worktree-agent-c3"
run_check
expect 1 "a harness branch under .worktrees/ is orphaned" \
    "Orphaned worktrees:" \
    "$(orphan "$MAIN/.worktrees/worktree-agent-c3" worktree-agent-c3 "harness name; not locked")"

# 4. No branch: a detached worktree, unlocked.
new_case
add_worktree --detach "$MAIN/.worktrees/measure"
run_check
expect 1 "a worktree without a branch is orphaned" \
    "Orphaned worktrees:" \
    "$(orphan "$MAIN/.worktrees/measure" "(no branch)" "no branch; not locked")"

# 5. Claimed: a lock dated today or yesterday holds, also with a comma inside the session name.
new_case
add_worktree -b worktree-agent-d4 "$MAIN/.claude/worktrees/agent-d4"
add_worktree -b worktree-agent-e5 "$MAIN/.claude/worktrees/agent-e5"
lock_worktree "$MAIN/.claude/worktrees/agent-d4" "session d4, ticket 11, $TODAY"
lock_worktree "$MAIN/.claude/worktrees/agent-e5" "session e5, $YESTERDAY"
run_check
expect 0 "a harness worktree locked today or yesterday reports nothing"

# 6. Old lock: dated two days back.
new_case
add_worktree -b worktree-agent-f6 "$MAIN/.claude/worktrees/agent-f6"
lock_worktree "$MAIN/.claude/worktrees/agent-f6" "session f6, $TWO_DAYS_AGO"
run_check
expect 1 "a lock dated older than one day does not hold" \
    "Orphaned worktrees:" \
    "$(orphan "$MAIN/.claude/worktrees/agent-f6" worktree-agent-f6 "path outside .worktrees/; harness name; lock dated $TWO_DAYS_AGO is older than one day")"

# 6a. A lock without a date, or with one that is no calendar day, cannot show the session is still running.
new_case
add_worktree -b worktree-agent-g7 "$MAIN/.claude/worktrees/agent-g7"
add_worktree -b worktree-agent-g8 "$MAIN/.claude/worktrees/agent-g8"
lock_worktree "$MAIN/.claude/worktrees/agent-g7" "session g7"
lock_worktree "$MAIN/.claude/worktrees/agent-g8" "session g8, 2026-13-45"
run_check
expect 1 "a lock without a real date does not hold" \
    "Orphaned worktrees:" \
    "$(orphan "$MAIN/.claude/worktrees/agent-g7" worktree-agent-g7 "path outside .worktrees/; harness name; lock carries no date")" \
    "$(orphan "$MAIN/.claude/worktrees/agent-g8" worktree-agent-g8 "path outside .worktrees/; harness name; lock carries no date")"

# 6b. A lock dated in the future would hold forever.
new_case
add_worktree -b worktree-agent-h9 "$MAIN/.claude/worktrees/agent-h9"
lock_worktree "$MAIN/.claude/worktrees/agent-h9" "session h9, $TOMORROW"
run_check
expect 1 "a lock dated in the future does not hold" \
    "Orphaned worktrees:" \
    "$(orphan "$MAIN/.claude/worktrees/agent-h9" worktree-agent-h9 "path outside .worktrees/; harness name; lock dated $TOMORROW lies in the future")"

# 6c. A registered worktree whose directory is gone.
new_case
add_worktree -b exp/gone "$MAIN/.worktrees/exp-gone"
rm -rf "$MAIN/.worktrees/exp-gone"
run_check
expect 1 "a worktree whose directory is gone is orphaned" \
    "Orphaned worktrees:" \
    "$(orphan "$MAIN/.worktrees/exp-gone" exp/gone "directory missing; not locked")"

# 7. Orca: a clean exp/* workspace is listed as open, with branch and age of its last commit.
new_case
add_worktree -b exp/perf "$ORCA/perf"
run_check
expect 0 "a clean Orca workspace on exp/* is listed as open" \
    "Open Orca workspaces:" \
    "$(open_ws "$ORCA/perf" exp/perf)"

# 8. Orca: a branch other than exp/*, even locked, and a detached one.
new_case
add_worktree -b refine/perf-1a2b "$ORCA/refine"
lock_worktree "$ORCA/refine" "orca, $TODAY"
add_worktree --detach "$ORCA/bisect"
run_check
expect 1 "an Orca workspace without an exp/* branch is orphaned" \
    "Orphaned worktrees:" \
    "$(orphan "$ORCA/bisect" "(no branch)" "Orca branch is not exp/*")" \
    "$(orphan "$ORCA/refine" refine/perf-1a2b "Orca branch is not exp/*")"

# 9. Orca: uncommitted content, unlocked, is orphaned; locked, the workspace is open.
new_case
add_worktree -b exp/dirty "$ORCA/dirty"
add_worktree -b exp/held "$ORCA/held"
echo change >> "$ORCA/dirty/README"
echo new > "$ORCA/held/notes.txt"
lock_worktree "$ORCA/held" "orca, 2026-01-03"
run_check
expect 1 "an unlocked Orca workspace with uncommitted content is orphaned, a locked one is open" \
    "Orphaned worktrees:" \
    "$(orphan "$ORCA/dirty" exp/dirty "uncommitted content and not locked")" \
    "Open Orca workspaces:" \
    "$(open_ws "$ORCA/held" exp/held)"

# 9a. Orca: both rules broken at once names both; a staged rename is uncommitted content too.
new_case
add_worktree -b refine/both "$ORCA/both"
add_worktree -b exp/moved "$ORCA/moved"
echo change >> "$ORCA/both/README"
git -C "$ORCA/moved" mv README README.md || setup_failed "mv"
run_check
expect 1 "an Orca workspace breaking both rules names both, a staged rename counts as content" \
    "Orphaned worktrees:" \
    "$(orphan "$ORCA/both" refine/both "Orca branch is not exp/*; uncommitted content and not locked")" \
    "$(orphan "$ORCA/moved" exp/moved "uncommitted content and not locked")"

# 9b. Orca: a workspace whose directory is gone, and one git cannot read.
new_case
add_worktree -b exp/gone "$ORCA/gone"
add_worktree -b exp/broken "$ORCA/broken"
add_worktree -b exp/fine "$ORCA/fine"
rm -rf "$ORCA/gone"
echo "gitdir: $CASE_DIR/nowhere" > "$ORCA/broken/.git"
run_check
expect 1 "an Orca workspace that is gone or unreadable is orphaned, the others are still listed" \
    "Orphaned worktrees:" \
    "$(orphan "$ORCA/broken" exp/broken "git cannot read the worktree")" \
    "$(orphan "$ORCA/gone" exp/gone "directory missing")" \
    "Open Orca workspaces:" \
    "$(open_ws "$ORCA/fine" exp/fine)"

# 10. The section 8 symlinks are not uncommitted content, even without an exclude entry; a file of the same name is.
new_case
add_worktree -b exp/linked "$ORCA/linked"
add_worktree -b exp/copied "$ORCA/copied"
shared="$CASE_DIR/shared"
ln -s "$shared/CLAUDE.local.md" "$ORCA/linked/CLAUDE.local.md"
ln -s "$shared/GLOSSARY.md" "$ORCA/linked/GLOSSARY.md"
ln -s "$shared/agents" "$ORCA/linked/docs/agents"
ln -s "$shared/adr" "$ORCA/linked/docs/adr"
ln -s "$shared/arbeitsablauf.md" "$ORCA/linked/docs/arbeitsablauf.md"
ln -s "$shared/katalog.md" "$ORCA/linked/docs/katalog.md"
cp "$shared/GLOSSARY.md" "$ORCA/copied/GLOSSARY.md"
run_check
expect 1 "the section 8 symlinks are not uncommitted content, a copied file is" \
    "Orphaned worktrees:" \
    "$(orphan "$ORCA/copied" exp/copied "uncommitted content and not locked")" \
    "Open Orca workspaces:" \
    "$(open_ws "$ORCA/linked" exp/linked)"

# 11. The check deletes and changes nothing: many cases at once, compared before and after.
new_case
add_worktree -b worktree-agent-k1 "$MAIN/.claude/worktrees/agent-k1"
add_worktree -b worktree-agent-k2 "$MAIN/.claude/worktrees/agent-k2"
lock_worktree "$MAIN/.claude/worktrees/agent-k2" "session k2, 2026-01-01"
add_worktree --detach "$MAIN/.worktrees/measure"
add_worktree -b exp/elsewhere "$CASE_DIR/elsewhere"
add_worktree -b exp/gone "$MAIN/.worktrees/exp-gone"; rm -rf "$MAIN/.worktrees/exp-gone"
add_worktree -b exp/dirty "$ORCA/dirty"; echo change >> "$ORCA/dirty/README"; echo new > "$ORCA/dirty/untracked.txt"
add_worktree -b refine/x "$ORCA/refine"
before=$(snapshot)
run_check
after=$(snapshot)
expect 1 "every orphan of a mixed repo is listed" \
    "Orphaned worktrees:" \
    "$(orphan "$CASE_DIR/elsewhere" exp/elsewhere "path outside .worktrees/; not locked")" \
    "$(orphan "$CASE_DIR/orca/dirty" exp/dirty "uncommitted content and not locked")" \
    "$(orphan "$CASE_DIR/orca/refine" refine/x "Orca branch is not exp/*")" \
    "$(orphan "$MAIN/.claude/worktrees/agent-k1" worktree-agent-k1 "path outside .worktrees/; harness name; not locked")" \
    "$(orphan "$MAIN/.claude/worktrees/agent-k2" worktree-agent-k2 "path outside .worktrees/; harness name; lock dated 2026-01-01 is older than one day")" \
    "$(orphan "$MAIN/.worktrees/exp-gone" exp/gone "directory missing; not locked")" \
    "$(orphan "$MAIN/.worktrees/measure" "(no branch)" "no branch; not locked")"
if [ "$before" = "$after" ]
then record ok "the check deletes and changes nothing"
else
    record no "the check deletes and changes nothing"
    diff <(printf '%s\n' "$before") <(printf '%s\n' "$after") | sed 's/^/    /'
fi

# 12. Run from inside a linked worktree, the check sees the same set.
new_case
add_worktree -b exp/foo "$MAIN/.worktrees/exp-foo"
add_worktree -b worktree-agent-j1 "$MAIN/.claude/worktrees/agent-j1"
run_check "$MAIN/.worktrees/exp-foo"
expect 1 "run from a linked worktree, the check reports the same" \
    "Orphaned worktrees:" \
    "$(orphan "$MAIN/.claude/worktrees/agent-j1" worktree-agent-j1 "path outside .worktrees/; harness name; not locked")"

# 13. Outside any repository the check cannot read anything and says so.
new_case
mkdir -p "$CASE_DIR/plain"
# The ceiling keeps git's discovery, and so its message, from depending on where the temp dir lives.
GIT_CEILING_DIRECTORIES="$CASE_DIR" run_check "$CASE_DIR/plain"
expect 2 "outside a repository the check exits 2" \
    "fatal: not a git repository (or any of the parent directories): .git" \
    "worktree-check: cannot list the worktrees of this repository"

echo ""
echo "worktree-check-selftest.sh: $PASS/$TOTAL passed"
[ "$FAIL" = 0 ]
