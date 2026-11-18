#!/usr/bin/env bash
set -euo pipefail
#
# Lists orphaned worktrees (Verwaiste Worktrees) and open Orca workspaces, by
# the rules of section 8 of docs/arbeitsablauf.md. It only reads: it deletes,
# moves, locks and unlocks nothing, and it never refreshes an index. The
# verdict over what it lists stays with the human.
#
# Every linked worktree outside the Orca directory is orphaned when it breaks
# a rule (path outside .worktrees/ or not .worktrees/<branch with - for />,
# harness name worktree-agent-*, no branch, directory missing) and its lock
# does not hold. A lock holds when its reason ends in ", <YYYY-MM-DD>" with a
# real date of yesterday or today; no lock, no such date, an older date or a
# date in the future does not hold.
#
# A worktree under the Orca directory (KYO_ORCA_DIR, default
# ~/orca/workspaces/kyo) has no day limit. It is orphaned when its branch is
# not exp/*, when it carries uncommitted content and is unlocked, or when its
# directory is missing or git cannot read it. The symlinks section 8 places in
# a worktree are not uncommitted content. Every other Orca worktree is listed
# as an open workspace, with its branch and the date and age of its last
# commit.
#
# Runs from the main checkout or any of its worktrees, on Linux (GNU date and
# realpath). Prints nothing when there is nothing to list. Exit 0 when no
# worktree is orphaned, 1 when one is, 2 when it cannot read the repo.

ORCA_DIR="$(realpath -m "${KYO_ORCA_DIR:-$HOME/orca/workspaces/kyo}")"
TODAY="$(date +%F)"
YESTERDAY="$(date -d yesterday +%F)"

# The links of section 8, relative to the worktree root.
SECTION8_LINKS=(CLAUDE.local.md GLOSSARY.md docs/agents docs/adr docs/arbeitsablauf.md docs/katalog.md)

orphans=()
open_orca=()
main_root=""
NL=$'\n'

# Why the lock does not hold, empty when it does.
lock_failure() {
    local locked="$1" reason="$2"
    if [ "$locked" = 0 ]; then echo "not locked"; return 0; fi
    local date="${reason##*,}"
    date="${date#"${date%%[![:space:]]*}"}"; date="${date%"${date##*[![:space:]]}"}"
    if [ "$reason" = "${reason%,*}" ] || ! [[ "$date" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] \
       || [ "$(date -d "$date" +%F 2>/dev/null)" != "$date" ]; then
        echo "lock carries no date"
    elif [[ "$date" < "$YESTERDAY" ]]; then
        echo "lock dated $date is older than one day"
    elif [[ "$date" > "$TODAY" ]]; then
        echo "lock dated $date lies in the future"
    fi
    return 0
}

is_section8_link() {
    local root="$1" path="$2" link
    [ -L "$root/$path" ] || return 1
    for link in "${SECTION8_LINKS[@]}"; do
        if [ "$path" = "$link" ]; then return 0; fi
    done
    return 1
}

# Prints "dirty" or "clean", or fails when git cannot read the worktree.
# Plain porcelain quotes an unusual path, so it can never equal a link name and counts as content.
uncommitted_state() {
    local root="$1" status entry
    status="$(GIT_OPTIONAL_LOCKS=0 git -C "$root" status --porcelain=v1 --untracked-files=all 2>/dev/null)" || return 1
    while IFS= read -r entry; do
        if [ -z "$entry" ]; then continue; fi
        if [ "${entry:0:3}" = "?? " ] && is_section8_link "$root" "${entry:3}"; then continue; fi
        echo dirty; return 0
    done <<< "$status"
    echo clean
}

# Reasons an Orca worktree is orphaned, one per line, then its open-list entry as a line "open <entry>".
orca_reasons() {
    local path="$1" branch="$2" locked="$3" missing="$4"
    if [ "$missing" = 1 ]; then echo "directory missing"; fi
    if [[ "$branch" != exp/* ]]; then echo "Orca branch is not exp/*"; fi
    if [ "$missing" = 1 ]; then return 0; fi
    local state last
    if ! state="$(uncommitted_state "$path")" || ! last="$(git -C "$path" log -1 --format='%cs (%cr)' HEAD 2>/dev/null)"; then
        echo "git cannot read the worktree"; return 0
    fi
    if [ "$state" = dirty ] && [ "$locked" = 0 ]; then echo "uncommitted content and not locked"; fi
    echo "open last commit $last"
}

# Reasons any other worktree is orphaned, one per line.
other_reasons() {
    local path="$1" branch="$2" locked="$3" reason="$4" missing="$5"
    local real; real="$(realpath -m "$path")"
    local worktrees="$main_root/.worktrees"
    local broken=()
    if [ "$missing" = 1 ]; then broken+=("directory missing"); fi
    if [[ "$real" != "$worktrees"/* ]]; then
        broken+=("path outside .worktrees/")
    elif [ -n "$branch" ] && [ "$real" != "$worktrees/${branch//\//-}" ]; then
        broken+=("path is not .worktrees/${branch//\//-}")
    fi
    if [[ "$branch" == worktree-agent-* ]]; then broken+=("harness name"); fi
    if [ -z "$branch" ]; then broken+=("no branch"); fi
    if [ ${#broken[@]} -eq 0 ]; then return 0; fi
    local lock; lock="$(lock_failure "$locked" "$reason")"
    if [ -z "$lock" ]; then return 0; fi
    printf '%s\n' "${broken[@]}" "$lock"
}

judge() {
    local path="$1" branch="$2" locked="$3" reason="$4" missing="$5"
    local real; real="$(realpath -m "$path")"
    local reasons open=""
    if [[ "$real" == "$ORCA_DIR"/* ]]; then
        local lines line
        lines="$(orca_reasons "$path" "$branch" "$locked" "$missing")"
        reasons=""
        while IFS= read -r line; do
            case "$line" in
                "open "*) open="${line#open }" ;;
                ?*) reasons+="${reasons:+$NL}$line" ;;
            esac
        done <<< "$lines"
    else
        reasons="$(other_reasons "$path" "$branch" "$locked" "$reason" "$missing")"
    fi
    local shown="${branch:-(no branch)}"
    if [ -n "$reasons" ]; then
        orphans+=("$path"$'\t'"$shown"$'\t'"$(printf '%s' "$reasons" | sed ':a;N;$!ba;s/\n/; /g')")
    elif [ -n "$open" ]; then
        open_orca+=("$path"$'\t'"$shown"$'\t'"$open")
    fi
}

# One record of `git worktree list --porcelain -z`: fields end in NUL, an empty field ends the record.
path=""; branch=""; locked=0; reason=""; missing=0
flush() {
    if [ -z "$path" ]; then return 0; fi
    if [ -z "$main_root" ]; then
        main_root="$(realpath -m "$path")"
    else
        if [ ! -d "$path" ]; then missing=1; fi
        judge "$path" "$branch" "$locked" "$reason" "$missing"
    fi
    path=""; branch=""; locked=0; reason=""; missing=0
}

if ! listing_file="$(mktemp)"; then exit 2; fi
trap 'rm -f "$listing_file"' EXIT
if ! git worktree list --porcelain -z > "$listing_file"; then
    echo "worktree-check: cannot list the worktrees of this repository" >&2
    exit 2
fi
while IFS= read -r -d '' field; do
    case "$field" in
        "") flush ;;
        "worktree "*) path="${field#worktree }" ;;
        "branch refs/heads/"*) branch="${field#branch refs/heads/}" ;;
        "locked") locked=1 ;;
        "locked "*) locked=1; reason="${field#locked }" ;;
        "prunable"*) missing=1 ;;
    esac
done < "$listing_file"
flush

if [ ${#orphans[@]} -gt 0 ]; then
    echo "Orphaned worktrees:"
    printf '%s\n' "${orphans[@]}" | LC_ALL=C sort | sed 's/^/  /'
fi
if [ ${#open_orca[@]} -gt 0 ]; then
    echo "Open Orca workspaces:"
    printf '%s\n' "${open_orca[@]}" | LC_ALL=C sort | sed 's/^/  /'
fi
[ ${#orphans[@]} -eq 0 ]
