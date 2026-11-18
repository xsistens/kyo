#!/usr/bin/env bash
set -euo pipefail
#
# Publishes the repo of the current directory to the local Ivy directory for
# the demos, by section 7 of docs/arbeitsablauf.md, and with --prune removes
# what no demo names any more.
#
#   scripts/publish-local.sh            publish HEAD
#   scripts/publish-local.sh --prune    list and, after a "y", delete
#
# Publishing refuses a PR worktree (branch pr/* or ui-mounted, or a HEAD
# without this script, which has no local layer) and an unclean working tree
# (modified, staged or untracked) before sbt starts. The version is
# <prefix>+<sha9>-SNAPSHOT: the prefix is the nearest upstream tag v<prefix>
# reachable from HEAD, <sha9> the first nine hex digits of HEAD. It stages
# the vendored natives for this host (BoringSSL, libaeron, DoltLite, SQLite)
# with the scripts CI runs, then sbt runs
# through scripts/sbt.sh with the publish role and publishes the whole kyoJVM
# and kyoJS aggregates with packageDoc turned off. The publish counts only
# when kyo-core of both platforms is in the Ivy directory at that version
# without doc jars. The last line on stdout is the version line for a demo.
#
# --prune keeps a version when its name, in double quotes, occurs in a build
# definition (*.sbt, project/**/*.scala) at the tip of any local branch of a
# demo. A version is a directory holding ivys/, at any depth under a module
# (an sbt plugin sits under scala_2.12/sbt_1.0/). It lists every version with
# its module count, the ones that fall with their size, and deletes them only
# on a "y" read from stdin. A demo it cannot read stops it before it lists
# anything. It neither starts sbt nor reads the working tree, so it runs from
# any checkout. Needs GNU awk, find and numfmt.
#
# KYO_IVY_LOCAL (default ~/.ivy2/local/io.getkyo) is where sbt publishes the
# io.getkyo organisation; it must match sbt's Ivy home. KYO_DEMOS (default the
# two demos under ~/Programming, separated by ":") lists the demo repos. Exit 0
# on success, 1 when publishing is refused or fails (sbt's own exit code when
# sbt fails), 2 on a usage error or when --prune cannot read its inputs.

IVY="${KYO_IVY_LOCAL:-$HOME/.ivy2/local/io.getkyo}"
DEMOS="${KYO_DEMOS:-$HOME/Programming/kyo-apollo-examples:$HOME/Programming/kyo-ui-components}"

# The scripts of .github/actions/stage-natives, in its order, for the host's os-arch.
STAGE_SCRIPTS=(
    kyo-net/build/boringssl/build-boringssl.sh
    kyo-aeron/scripts/build-aeron.sh
    kyo-sql-doltlite/scripts/build-doltlite.sh
    kyo-sql-sqlite/scripts/build-sqlite.sh
)

say() { echo "publish-local: $*" >&2; }
human() { numfmt --to=iec-i --suffix=B "$1"; }
plural() { if [ "$1" = 1 ]; then echo "$1 $2"; else echo "$1 $2s"; fi; }

publish() {
    local top
    if ! top="$(git rev-parse --show-toplevel 2>/dev/null)"; then
        say "not inside a git repository"; return 1
    fi
    cd "$top"

    local branch
    branch="$(git symbolic-ref --quiet --short HEAD || true)"
    if [[ "$branch" == pr/* || "$branch" == ui-mounted ]]; then
        say "$branch is a PR branch; PR worktrees never publish"; return 1
    fi
    if ! git cat-file -e HEAD:scripts/publish-local.sh 2>/dev/null; then
        say "HEAD carries no scripts/publish-local.sh, so it has no local layer (a PR worktree or upstream); it never publishes"
        return 1
    fi

    local dirty
    dirty="$(git status --porcelain=v1 --untracked-files=all)"
    if [ -n "$dirty" ]; then
        say "the working tree is not clean; commit it first (to its experiment branch):"
        printf '%s\n' "$dirty" >&2
        return 1
    fi

    local tag
    if ! tag="$(git describe --tags --match 'v[0-9]*' --abbrev=0 HEAD 2>/dev/null)"; then
        say "no upstream tag v<version> is reachable from HEAD"; return 1
    fi
    local version
    version="${tag#v}+$(git rev-parse HEAD | cut -c1-9)-SNAPSHOT"

    # The vendored natives the FFI modules compile against, staged for this host as CI's stage-natives does.
    # Each script returns at once when its tree is staged. Without the BoringSSL tree kyo-net would ship
    # its stub silently; without the others ffiCompile fails.
    local stage rc=0
    for stage in "${STAGE_SCRIPTS[@]}"; do
        bash "$stage" >&2 || rc=$?
        if [ "$rc" != 0 ]; then
            say "staging with $stage failed (exit $rc); sbt not started"; return 1
        fi
    done

    scripts/sbt.sh publish \
        "set ThisBuild / version := \"$version\"" \
        "set ThisBuild / Compile / packageDoc / publishArtifact := false" \
        kyoJVM/publishLocal kyoJS/publishLocal || rc=$?
    if [ "$rc" != 0 ]; then
        say "sbt failed (exit $rc); nothing to put into a demo"; return "$rc"
    fi

    local module
    for module in kyo-core_3 kyo-core_sjs1_3; do
        if [ ! -d "$IVY/$module/$version" ]; then
            say "sbt left no $IVY/$module/$version"; return 1
        fi
    done
    local docs
    docs="$(find "$IVY" -mindepth 2 -type d -path "*/$version/docs" | LC_ALL=C sort | tr '\n' ' ')"
    if [ -n "$docs" ]; then
        say "sbt published doc jars: ${docs% }"; return 1
    fi

    say "published $version for JVM and JS"
    echo "val KyoVersion = \"$version\""
}

# The build definitions at the tip of every local branch of every demo, concatenated into the file $1.
# Fails, with a message, when a demo, a branch or a file cannot be read.
demo_builds() {
    local out="$1" demos demo branches branch paths path
    IFS=: read -r -a demos <<< "$DEMOS"
    for demo in "${demos[@]}"; do
        if [ -z "$demo" ] || ! git -C "$demo" rev-parse --git-dir >/dev/null 2>&1; then
            say "cannot read the demo '$demo'; nothing deleted"; return 1
        fi
        if ! branches="$(git -C "$demo" for-each-ref --format='%(refname)' refs/heads)"; then
            say "cannot list the branches of the demo $demo; nothing deleted"; return 1
        fi
        if [ -z "$branches" ]; then
            say "the demo $demo has no branch; nothing deleted"; return 1
        fi
        while IFS= read -r branch; do
            if ! paths="$(git -C "$demo" ls-tree -r --name-only "$branch")"; then
                say "cannot read $branch of the demo $demo; nothing deleted"; return 1
            fi
            while IFS= read -r path; do
                case "$path" in
                    *.sbt | project/*.scala) ;;
                    *) continue ;;
                esac
                if ! git -C "$demo" cat-file blob "$branch:$path" >> "$out"; then
                    say "cannot read $path on $branch of the demo $demo; nothing deleted"; return 1
                fi
            done <<< "$paths"
        done <<< "$branches"
    done
}

prune() {
    if [ ! -d "$IVY" ]; then
        say "no Ivy directory at $IVY"; return 2
    fi
    builds="$(mktemp)"
    trap 'rm -f "$builds"' EXIT
    demo_builds "$builds" || return 2

    # Every version directory as module<TAB>version<TAB>bytes, the module being its path below $IVY.
    local dirs=()
    mapfile -t dirs < <(find "$IVY" -mindepth 3 -type d -name ivys -printf '%h\n' | LC_ALL=C sort)
    # version<TAB>modules<TAB>bytes<TAB>kept, one line per version, sorted.
    local table=""
    if [ ${#dirs[@]} -gt 0 ]; then
        table="$(du -sb "${dirs[@]}" | awk -F'\t' -v root="$IVY/" -v builds="$builds" '
            BEGIN { RS = "^$"; getline text < builds; RS = "\n" }
            {
                n = split(substr($2, length(root) + 1), part, "/"); v = part[n]
                mods[v]++; bytes[v] += $1
            }
            END { for (v in mods) print v "\t" mods[v] "\t" bytes[v] "\t" (index(text, "\"" v "\"") ? 1 : 0) }
            ' | LC_ALL=C sort)"
    fi

    local version count bytes kept
    local kept_lines=() fall_lines=() falling=() fall_bytes=0
    while IFS=$'\t' read -r version count bytes kept; do
        if [ -z "$version" ]; then continue; fi
        if [ "$kept" = 1 ]; then
            kept_lines+=("  $version"$'\t'"$(plural "$count" module)")
        else
            fall_lines+=("  $version"$'\t'"$(plural "$count" module)"$'\t'"$(human "$bytes")")
            falling+=("$version")
            fall_bytes=$((fall_bytes + bytes))
        fi
    done <<< "$table"

    if [ ${#kept_lines[@]} -gt 0 ]; then
        echo "Kept, named by a demo branch:"
        printf '%s\n' "${kept_lines[@]}"
    fi
    if [ ${#falling[@]} -eq 0 ]; then
        echo "Nothing falls."
        return 0
    fi
    echo "Falls, named by no demo branch:"
    printf '%s\n' "${fall_lines[@]}"

    local answer=""
    printf 'Delete %s (%s)? [y/N] ' "$(plural "${#falling[@]}" version)" "$(human "$fall_bytes")"
    read -r answer || true
    case "$answer" in
        y | Y) ;;
        *) echo "nothing deleted"; return 0 ;;
    esac

    local dir parent
    for dir in "${dirs[@]}"; do
        version="${dir##*/}"
        if [[ " ${falling[*]} " != *" $version "* ]]; then continue; fi
        rm -rf -- "$dir"
        # Remove the parents it emptied, up to but not including $IVY.
        parent="${dir%/*}"
        while [ "$parent" != "$IVY" ] && rmdir -- "$parent" 2>/dev/null; do parent="${parent%/*}"; done
    done
    echo "deleted $(plural "${#falling[@]}" version)"
}

if [ $# = 0 ]; then
    publish
elif [ $# = 1 ] && [ "$1" = --prune ]; then
    prune
else
    echo "usage: scripts/publish-local.sh [--prune]" >&2; exit 2
fi
