#!/usr/bin/env bash
set -uo pipefail
#
# Sets the Sicherung before a force-push of hauptlinie, by the rules of
# section 4 of docs/arbeitsablauf.md, and keeps the last two.
#
# Usage: scripts/sicherung.sh [remote]   (default: fork)
#
# 1. Fetches hauptlinie from the remote and takes the commit it carries now,
#    never the local tip: that is the state a force-push would overwrite.
# 2. Unless the youngest Sicherung on the remote already points there, tags
#    that commit sicherung/<YYYY-MM-DD> (lightweight), or -b, -c and on after
#    the highest letter of the day taken here or on the remote, and pushes it.
#    A failed push removes the local tag again and fails.
# 3. Keeps the youngest two Sicherungen on the remote and deletes every older
#    one there and here; a sicherung/* tag that exists only here is no
#    Sicherung and falls too. The two kept are fetched, so both sides end
#    equal.
#
# The remote is the truth: which Sicherungen exist and which is youngest is
# read from it, by name (date, then "" < -b < -c ...). A Sicherung on the
# remote dated after today is refused before anything is tagged, since it
# would sort as the youngest and push the new one out. A local-only one is no
# Sicherung and falls like any other. A sicherung/* tag of another form is
# named and left alone. The script never pushes hauptlinie. Its pushes carry
# only tags, so they skip the pre-push hooks (--no-verify): the format check
# has nothing to check in a tag, and the local guard lets sicherung/* through
# by rule. KYO_TODAY, when set, replaces today's date.
#
# Exit 0 when the remote tip carries a Sicherung and at most two remain, 1 otherwise.
# Self-test: scripts/sicherung-selftest.sh

remote="${1:-fork}"
branch=hauptlinie
today=${KYO_TODAY:-$(date +%F)}
KEEP=2
# The form of a Sicherung; scripts/hooks/pre-push-local counts the same form.
FORM='sicherung/[0-9]{4}-[0-9]{2}-[0-9]{2}(-[b-z])?'

say() { printf 'sicherung: %s\n' "$*"; }
fail() { printf 'sicherung: %s\n' "$*" >&2; exit 1; }

# "<name> <commit>" per sicherung/* tag on the remote, sorted by name. An
# annotated tag lists its object and then the commit on a peeled ^{} line.
remote_tags() {
    local listing
    listing=$(git ls-remote --tags "$remote" 'refs/tags/sicherung/*') || return 1
    printf '%s\n' "$listing" | awk '
        NF { n = $2; sub(/^refs\/tags\//, "", n)
             if (sub(/\^\{\}$/, "", n)) peeled[n] = $1; else obj[n] = $1 }
        END { for (n in obj) print n, (n in peeled ? peeled[n] : obj[n]) }' | LC_ALL=C sort
}
local_names() { git tag -l 'sicherung/*' | LC_ALL=C sort; }
# The names of the well-formed Sicherungen among the given names, oldest first.
well_formed() { grep -xE "$FORM" | LC_ALL=C sort -u; }
# The lines of $1 that are also lines of $2 (in_set) or not (not_in_set).
in_set() { [ -n "$2" ] && printf '%s\n' "$1" | grep -xF "$2"; }
not_in_set() { if [ -n "$2" ]; then printf '%s\n' "$1" | grep -vxF "$2"; else printf '%s\n' "$1" | grep -v '^$'; fi; }

# The next name of today after the highest one taken. Never the first gap:
# after the third of the day the first has fallen, and reusing its name
# would make the youngest Sicherung sort as the oldest.
next_name() {
    local last suffix letters=bcdefghijklmnopqrstuvwxyz rest
    last=$(printf '%s\n' "$1" | grep -xE "sicherung/$today(-[b-z])?" | LC_ALL=C sort | tail -n 1)
    [ -n "$last" ] || { printf 'sicherung/%s\n' "$today"; return 0; }
    suffix=${last#"sicherung/$today"}
    [ -n "$suffix" ] || { printf 'sicherung/%s-b\n' "$today"; return 0; }
    rest=${letters#*"${suffix#-}"}
    [ -n "$rest" ] || return 1
    printf 'sicherung/%s-%s\n' "$today" "${rest:0:1}"
}

[[ "sicherung/$today" =~ ^${FORM}$ ]] || fail "today's date $today is not of the form YYYY-MM-DD"
git remote get-url "$remote" >/dev/null 2>&1 || fail "$remote is no remote of this repo (git remote -v)"
git fetch -q --no-tags "$remote" "+refs/heads/$branch:refs/remotes/$remote/$branch" ||
    fail "could not fetch $branch from $remote"
tip=$(git rev-parse -q --verify "refs/remotes/$remote/$branch^{commit}") ||
    fail "$remote/$branch does not name a commit"
on_remote=$(remote_tags) || fail "could not read the sicherung/* tags of $remote"

all_names=$( { printf '%s\n' "$on_remote" | awk 'NF { print $1 }'; local_names; } | LC_ALL=C sort -u | grep -v '^$')
while IFS= read -r odd; do
    [ -n "$odd" ] && say "left $odd alone: not of the form sicherung/<YYYY-MM-DD>[-b..-z]"
done < <(printf '%s\n' "$all_names" | grep -vxE "$FORM")
youngest=$(printf '%s\n' "$on_remote" | grep -E "^$FORM " | tail -n 1)
if [ -n "$youngest" ] && [[ "${youngest:10:10}" > "$today" ]]; then
    fail "${youngest%% *} is dated after today ($today); check the clock, or delete the tag by hand"
fi

if [ -n "$youngest" ] && [ "${youngest#* }" = "$tip" ]; then
    say "$remote/$branch at ${tip:0:10} already carries ${youngest%% *}"
else
    name=$(next_name "$all_names") || fail "sicherung/$today-z is taken, no name of today is left"
    git tag "$name" "$tip" || fail "could not tag $name"
    if ! git push -q --no-verify "$remote" "refs/tags/$name"; then
        git tag -d "$name" >/dev/null
        fail "could not push $name to $remote"
    fi
    say "set $name on ${tip:0:10}, the tip of $remote/$branch, and pushed it to $remote"
    on_remote=$(remote_tags) || fail "could not read the sicherung/* tags of $remote"
fi

remote_names=$(printf '%s\n' "$on_remote" | awk 'NF { print $1 }' | well_formed)
here_names=$(local_names | well_formed)
keep=$(printf '%s\n' "$remote_names" | tail -n "$KEEP")
drop_remote=$(not_in_set "$remote_names" "$keep")
drop_here=$(not_in_set "$here_names" "$keep")

if [ -n "$drop_remote" ]; then
    mapfile -t refs < <(printf '%s\n' "$drop_remote" | sed 's|^|refs/tags/|')
    git push -q --no-verify "$remote" --delete "${refs[@]}" ||
        fail "could not delete $(printf '%s\n' "$drop_remote" | paste -sd ' ') on $remote"
fi
while IFS= read -r old; do
    [ -n "$old" ] || continue
    if in_set "$old" "$drop_here" >/dev/null; then
        git tag -d "$old" >/dev/null || fail "could not delete $old locally"
        if in_set "$old" "$drop_remote" >/dev/null; then say "dropped $old, locally and on $remote"
        else say "dropped $old, which was only local"; fi
    else
        say "dropped $old on $remote, it was not here"
    fi
done < <(printf '%s\n%s\n' "$drop_remote" "$drop_here" | LC_ALL=C sort -u)

# The two kept live on both sides, on the remote's commit.
mapfile -t specs < <(printf '%s\n' "$keep" | sed 's|.*|+refs/tags/&:refs/tags/&|')
git fetch -q --no-tags "$remote" "${specs[@]}" ||
    fail "could not fetch $(printf '%s\n' "$keep" | paste -sd ' ') from $remote"
