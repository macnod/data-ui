# checkout.sh — shared checkout machinery for Data UI deployment scripts.
#
# Sourced (never executed) by scripts/data-ui through the invoking
# script's canonical root, so every clone runs its own copy. This file
# owns the re-exec protocol line: any change to the envelope grammar,
# the handoff path grammar, or the MODEL_FILE contract bumps
# DATA_UI_REEXEC_PROTOCOL in the same commit.
#
# It provides: defaults for the three source environments (development
# is wherever this script runs; staging and production are independent
# clones under /data/data-ui/repos), source-spec parsing, the single
# cleanliness implementation, fetch + ff-only selection on per-upstream
# deploy branches, flock role locks with the inherited-descriptor
# ownership probe, and the internal re-exec envelope shared by deploy,
# delete, and the staging update child.

# The re-exec envelope protocol version. The marker an outer arm passes
# is __DATA_UI_REEXEC_V${DATA_UI_REEXEC_PROTOCOL}__; an inner arm
# rejects every other marker spelling with a protocol-mismatch error.
DATA_UI_REEXEC_PROTOCOL=1

# die is defined by scripts/data-ui before sourcing in normal use; the
# fallback keeps this library usable standalone (tests, ad-hoc shells).
if ! declare -F die >/dev/null 2>&1; then
    die() { echo "ERROR: $*" >&2; exit 1; }
fi

# Environment defaults. `:=` so pre-set values (tests, operators)
# survive; the export keeps children on the same page. These are guard
# inputs, not conveniences: STAGING_HOST exists so a split-host
# configuration is a visible error instead of a silent divergence.
: "${DEPLOY_CHECKOUT:=/data/data-ui/repos/production}"
: "${STAGING_CHECKOUT:=/data/data-ui/repos/staging}"
: "${DEPLOY_HOST:=evo-x2}"
: "${STAGING_HOST:=evo-x2}"
: "${CHECKOUT_LOCK_ROOT:=/data/data-ui/repos}"
: "${DEPLOY_BOOTSTRAP_CHECKOUT:=/home/macnod/common-lisp/data-ui}"
: "${DATA_UI_PUBLIC_URL:=git@github.com:macnod/data-ui.git}"
: "${DATA_UI_PRIVATE_URL:=git@github.com:macnod/data-ui-private.git}"
export DEPLOY_CHECKOUT STAGING_CHECKOUT DEPLOY_HOST STAGING_HOST \
       CHECKOUT_LOCK_ROOT DEPLOY_BOOTSTRAP_CHECKOUT \
       DATA_UI_PUBLIC_URL DATA_UI_PRIVATE_URL

canonical_checkout_root() {
    # Canonical root of the invoking script: readlink -f of the script,
    # parent of scripts/. Routing compares canonical roots, never cwd.
    dirname "$(dirname "$(readlink -f "$0")")"
}

#
# Git hygiene. Every Git command that targets a deployment checkout
# goes through checkout_git: an explicit -C path, its own exit status
# consumed by the caller, and every GIT_* input variable (GIT_DIR,
# GIT_WORK_TREE, GIT_INDEX_FILE, and friends) cleared from the child
# environment — for selection and for every later rev-parse / tag call,
# so a caller's Git environment can never redirect a command aimed at
# a deployment checkout.
#

checkout_git() {
    # checkout_git PATH ARGS...
    local path="$1"; shift
    local -a scrub=(env)
    local name
    while IFS='=' read -r name _; do
        [[ "$name" == GIT_* ]] && scrub+=(-u "$name")
    done < <(env)
    "${scrub[@]}" git -C "$path" "$@"
}

checkout_git_plain() {
    # Repo-independent git verbs (check-ref-format; clone, which takes
    # its own destination). Same GIT_* scrub as checkout_git.
    local -a scrub=(env)
    local name
    while IFS='=' read -r name _; do
        [[ "$name" == GIT_* ]] && scrub+=(-u "$name")
    done < <(env)
    "${scrub[@]}" git "$@"
}

#
# Operand and source grammar
#

valid_model_operand() {
    # Deploy and delete model operands: ^[a-z][-a-z0-9]*$, and
    # profile / profile-* are reserved for host profiles. This also
    # rejects absolute paths, ../x, whitespace, quotes, backslashes,
    # newlines, leading dashes, and bad punctuation — the charset does
    # the work. Validated at the fail-fast boundary, before SSH, Git,
    # temp-file, database, or cluster access. It is the operand
    # grammar only; the :name equality check runs later, where the
    # source is readable.
    local name="$1" what="${2:-model}"
    [[ "$name" =~ ^[a-z][-a-z0-9]*$ ]] \
        || die "Invalid ${what} '${name}': must match ^[a-z][-a-z0-9]*\$ (lowercase letters, digits, hyphens)."
    case "$name" in
        profile|profile-*)
            die "'${name}' is reserved for host profiles and cannot be a ${what}."
            ;;
    esac
}

parse_source_spec() {
    # "REMOTE/BRANCH" — split on the FIRST slash, so branches may
    # contain further slashes — into CHECKOUT_REMOTE / CHECKOUT_BRANCH.
    local spec="$1" remote branch
    [[ -n "$spec" ]] || die "Empty source spec (wanted REMOTE/BRANCH, e.g. public/master)."
    [[ "$spec" != -* ]] || die "Source spec must not begin with a dash: '${spec}'."
    [[ "$spec" == */* ]] || die "Source spec '${spec}' has no REMOTE/BRANCH slash."
    remote="${spec%%/*}"
    branch="${spec#*/}"
    [[ -n "$remote" ]] || die "Empty remote in source spec '${spec}'."
    [[ -n "$branch" ]] || die "Empty branch in source spec '${spec}'."
    [[ "$remote" =~ ^[A-Za-z0-9][A-Za-z0-9._-]*$ ]] \
        || die "Invalid remote name '${remote}' in '${spec}'."
    # The branch must be a selectable refs/heads name: pass
    # git check-ref-format --branch, then reject the spellings that
    # pass the format check but are not branches (HEAD and @ resolve to
    # whatever is checked out; refs/... selects outside refs/heads;
    # .. and @{ are ref operators; a leading dash is option injection).
    checkout_git_plain check-ref-format --branch "$branch" >/dev/null 2>&1 \
        || die "Invalid branch '${branch}' in source spec '${spec}'."
    case "$branch" in
        HEAD|@|-*|refs/*|*..*|*@{*)
            die "Branch '${branch}' is not a selectable source branch (wanted a plain refs/heads name)."
            ;;
    esac
    CHECKOUT_REMOTE="$remote"
    CHECKOUT_BRANCH="$branch"
}

parse_deploy_args() {
    # Exactly `deploy MODEL` or `deploy MODEL --from REMOTE/BRANCH`.
    # Rejects missing values, duplicate --from, unknown flags, extra
    # positionals, and --from=... spellings before any side effect.
    # Marker-shaped strings never reach here (the dispatcher rejects
    # them first). demo/e-demo --from keeps meaning a snapshot.
    local -a pos=()
    local from_spec="" saw_from=0
    while (( $# > 0 )); do
        case "$1" in
            --from)
                (( $# >= 2 )) || die "--from requires a value: '--from REMOTE/BRANCH'."
                (( saw_from == 0 )) || die "Duplicate --from."
                saw_from=1
                from_spec="$2"
                shift 2
                ;;
            --from=*)
                die "Use '--from REMOTE/BRANCH' (space form); '${1}' is not accepted."
                ;;
            --*)
                die "Unknown option '${1}' for deploy."
                ;;
            *)
                pos+=("$1")
                shift
                ;;
        esac
    done
    (( ${#pos[@]} == 1 )) || die "deploy takes exactly one model operand."
    valid_model_operand "${pos[0]}" "deploy operand"
    parse_source_spec "${from_spec:-public/master}"
    DEPLOY_MODEL_NAME="${pos[0]}"
    DEPLOY_FROM_SPEC="${from_spec:-public/master}"
}

#
# Cleanliness (the single implementation; only selection calls it)
#

require_checkout_clean() {
    # Clean = `git status --porcelain --untracked-files=all` (no
    # --ignored: web/dist, node_modules, and models/local/ are allowed)
    # with the exit status checked — an empty FAILED git status is
    # never clean. Never waived for DRY_RUN or MODEL_FILE. Never
    # git clean / reset --hard / auto-stash / force checkout.
    local path="$1" status
    if ! status=$(checkout_git "$path" status --porcelain --untracked-files=all); then
        die "git status failed in ${path}; treating the checkout as not clean."
    fi
    if [[ -n "$status" ]]; then
        die "Checkout ${path} is not clean (untracked files count; ignored files are fine). Selection refuses to touch it — commit, stash, or remove the changes there."
    fi
}

require_incoming_ignored() {
    # The candidate directory must be ignored by the clone's rules
    # (the root /models/local/ line covers it); otherwise an installed
    # candidate would make every future selection fail.
    local path="$1"
    checkout_git "$path" check-ignore -q "models/local/deploy-incoming/x.lisp" \
        || die "models/local/deploy-incoming/ is not git-ignored in ${path}; refusing to use it for candidates."
}

#
# Selection: fetch, then ff-only onto per-upstream deploy branches
#

checkout_remote_ref() {
    echo "refs/remotes/$1/$2"
}

checkout_deploy_branch() {
    echo "deploy/$1/$2"
}

checkout_fetch_source() {
    # Fetch exactly one branch into its remote-tracking ref and capture
    # the full SHA. The `+` forces only the remote-tracking ref, never
    # the working tree; every argv element is quoted separately and
    # nothing is interpolated into a compound shell or SSH string.
    local path="$1" remote="$2" branch="$3" ref sha
    ref=$(checkout_remote_ref "$remote" "$branch")
    checkout_git "$path" fetch --no-tags "$remote" "+refs/heads/${branch}:${ref}" \
        || die "git fetch of ${remote}/${branch} failed in ${path} (missing remote branch? network?)."
    sha=$(checkout_git "$path" rev-parse --verify "${ref}^{commit}") \
        || die "Cannot resolve ${ref} to a commit after fetching it."
    [[ "$sha" =~ ^[0-9a-f]{40}$ ]] || die "Unexpected SHA '${sha}' for ${ref}."
    SELECTED_FULL_SHA="$sha"
}

checkout_move_to_source() {
    # Put the working tree on the per-upstream deploy branch at exactly
    # SHA. One local branch per upstream (deploy/REMOTE/BRANCH):
    # switching sources switches branches and never resets one branch
    # across unrelated histories. Absent branch: git switch -C at the
    # captured SHA (never checkout -B). Existing branch: git switch,
    # set-upstream-to the unambiguous refs/remotes spelling, merge
    # --ff-only the captured SHA, then RECHECK that branch, upstream,
    # and HEAD all equal it — the merge exits 0 even when the local
    # branch is ahead, so the recheck is the gate. Divergence, wrong
    # upstream, upstream rewind, local-ahead, or non-ff aborts with no
    # build, no clean/reset/stash, and no force.
    local path="$1" remote="$2" branch="$3" sha="$4"
    local ref lb
    ref=$(checkout_remote_ref "$remote" "$branch")
    lb=$(checkout_deploy_branch "$remote" "$branch")
    require_checkout_clean "$path"
    if checkout_git "$path" rev-parse --verify --quiet "refs/heads/${lb}" >/dev/null 2>&1; then
        checkout_git "$path" switch "$lb" \
            || die "git switch ${lb} failed in ${path}."
        checkout_git "$path" branch --set-upstream-to "$ref" "$lb" \
            || die "Cannot set ${lb}'s upstream to ${ref} in ${path}."
        checkout_git "$path" merge --ff-only "$sha" \
            || die "Fast-forward of ${lb} to ${sha} failed in ${path}; refusing to touch the checkout further."
    else
        checkout_git "$path" switch -C "$lb" "$sha" \
            || die "git switch -C ${lb} at ${sha} failed in ${path}."
        checkout_git "$path" branch --set-upstream-to "$ref" "$lb" \
            || die "Cannot set ${lb}'s upstream to ${ref} in ${path}."
    fi
    local got_branch got_upstream got_head
    got_branch=$(checkout_git "$path" rev-parse "refs/heads/${lb}") \
        || die "Cannot read refs/heads/${lb} after selection."
    got_upstream=$(checkout_git "$path" rev-parse "$ref") \
        || die "Cannot read ${ref} after selection."
    got_head=$(checkout_git "$path" rev-parse HEAD) \
        || die "Cannot read HEAD after selection."
    [[ "$got_branch" == "$sha" && "$got_upstream" == "$sha" && "$got_head" == "$sha" ]] \
        || die "Selection recheck failed in ${path}: ${lb}=${got_branch:0:12}, upstream=${got_upstream:0:12}, HEAD=${got_head:0:12}, wanted ${sha}. A local-ahead branch or an upstream rewind aborts; never force."
    SELECTED_LOCAL_BRANCH="$lb"
}

select_checkout_source() {
    # The whole selection in one call: require clean, fetch, move.
    local path="$1" remote="$2" branch="$3"
    require_checkout_clean "$path"
    checkout_fetch_source "$path" "$remote" "$branch"
    checkout_move_to_source "$path" "$remote" "$branch" "$SELECTED_FULL_SHA"
}

print_checkout_identity() {
    # Role, canonical path, source, local branch, full SHA — kept for
    # deploy output and logs. SHORT_HASH and every commit-derived
    # identity downstream come from the printed SHA, never a HEAD
    # reread.
    local role="$1" path="$2" remote="$3" branch="$4" lb="$5" sha="$6"
    echo "Selected source: ${role} checkout ${path}"
    echo "  remote/branch: ${remote}/${branch}"
    echo "  local branch:  ${lb}"
    echo "  full SHA:      ${sha}"
}

#
# Bootstrap (non-destructive)
#

ensure_checkout() {
    # ensure_checkout PATH PUBLIC_URL PRIVATE_URL — create or validate
    # a deployment clone. Bootstrap clones public under the remote
    # name `public` (no origin ever exists) and adds `private`; a
    # nonempty non-repository destination fails untouched. Missing
    # known remotes are added under their canonical URLs; a remote
    # with an unexpected URL, or a legacy `origin` without `public`,
    # is an error — never renamed, never rewritten.
    local path="$1" public_url="$2" private_url="$3"
    local real dev_root r url want
    real=$(readlink -m "$path")
    [[ "$real" != "/" ]] || die "Refusing to bootstrap a checkout at /."
    # The development checkout is the BOOTSTRAP checkout (the dev
    # clone), not the invoking script's root: a deployment clone's own
    # script is a legitimate host-side launcher for deploying THAT
    # clone (the plan's remote production arm — the launcher runs the
    # outer deploy against its own checkout). What must never happen
    # is deploying INTO the development checkout itself.
    dev_root=$(readlink -m "${DEPLOY_BOOTSTRAP_CHECKOUT:-/nonexistent-dev-checkout}")
    [[ -n "$dev_root" && "$real" != "$dev_root" ]] \
        || die "Checkout path ${path} is the development checkout itself (${dev_root}); deployment clones are independent directories."
    # The two deployment environments must never collide: a shared path
    # would make one environment's update move the other's tree.
    [[ "$(readlink -m "$DEPLOY_CHECKOUT")" != "$(readlink -m "$STAGING_CHECKOUT")" ]] \
        || die "DEPLOY_CHECKOUT and STAGING_CHECKOUT are the same directory ($(readlink -m "$DEPLOY_CHECKOUT"))."
    if [[ ! -e "$path" ]]; then
        mkdir -p "$(dirname "$real")" \
            || die "Cannot create the parent of ${path}."
        checkout_git_plain clone -o public "$public_url" "$real" \
            || die "git clone of ${public_url} into ${real} failed."
        checkout_git "$real" remote add private "$private_url" \
            || die "Cannot add the private remote (${private_url}) to ${real}."
        echo "Bootstrapped checkout at ${real} (remotes: public, private)."
        return 0
    fi
    [[ -d "$path" ]] || die "${path} exists and is not a directory; leaving it untouched."
    if [[ -z "$(ls -A "$path" 2>/dev/null)" ]]; then
        checkout_git_plain clone -o public "$public_url" "$real" \
            || die "git clone of ${public_url} into ${real} failed."
        checkout_git "$real" remote add private "$private_url" \
            || die "Cannot add the private remote (${private_url}) to ${real}."
        echo "Bootstrapped checkout at ${real} (remotes: public, private)."
        return 0
    fi
    checkout_git "$path" rev-parse --is-inside-work-tree >/dev/null 2>&1 \
        || die "${path} is not empty and not a git repository; leaving it untouched."
    local -a remotes=()
    mapfile -t remotes < <(checkout_git "$path" remote 2>/dev/null)
    local has_origin=0 has_public=0
    for r in "${remotes[@]}"; do
        [[ "$r" == "origin" ]] && has_origin=1
        [[ "$r" == "public" ]] && has_public=1
    done
    if (( has_origin && ! has_public )); then
        die "${path} has a legacy 'origin' remote and no 'public'; refusing to rename or rewrite anything. See docs/deployment.md for the migration path."
    fi
    for r in public private; do
        want="$public_url"
        [[ "$r" == "private" ]] && want="$private_url"
        local present=0 r2
        for r2 in "${remotes[@]}"; do
            [[ "$r2" == "$r" ]] && present=1
        done
        if (( present )); then
            url=$(checkout_git "$path" remote get-url "$r")
            [[ "$url" == "$want" ]] \
                || die "Remote '${r}' in ${path} has unexpected URL ${url} (wanted ${want})."
        else
            checkout_git "$path" remote add "$r" "$want" \
                || die "Cannot add the missing remote '${r}' (${want}) to ${path}."
            echo "Added remote ${r} -> ${want} in ${path}."
        fi
    done
}

#
# Locks. One flock per role — production.lock (deploy, delete, the
# production half of staging update and activation) and staging.lock —
# under CHECKOUT_LOCK_ROOT, which is created first so the first-ever
# bootstrap can lock. A flock lives until the last descriptor on its
# open file description closes; the descriptor is opened persistently
# in the main shell and carried across the production exec (never
# flock -o / --close). Never fcntl / POSIX record locks.
#

checkout_lock_file() {
    echo "${CHECKOUT_LOCK_ROOT}/${1}.lock"
}

declare -A CHECKOUT_LOCK_FDS=()

with_checkout_lock() {
    # Acquire ROLE's lock, blocking, on a persistent dynamically opened
    # descriptor in the main shell — never in a command substitution or
    # subshell, never closed on exec. This is the only acquisition
    # path; there is no environment bypass.
    local role="$1" lock fd
    case "$role" in
        production|staging) ;;
        *) die "Unknown checkout lock role '${role}'." ;;
    esac
    [[ -z "${CHECKOUT_LOCK_FDS[$role]:-}" ]] \
        || die "Internal error: lock '${role}' is already acquired in this process."
    mkdir -p "$CHECKOUT_LOCK_ROOT" \
        || die "Cannot create CHECKOUT_LOCK_ROOT ${CHECKOUT_LOCK_ROOT}."
    lock=$(checkout_lock_file "$role")
    exec {fd}<>"$lock" || die "Cannot open the lock file ${lock}."
    flock -x "$fd" || die "Cannot acquire ${lock}."
    CHECKOUT_LOCK_FDS[$role]=$fd
    echo "Acquired the ${role} lock (${lock}, fd ${fd})."
}

checkout_lock_fd() {
    local role="$1"
    [[ -n "${CHECKOUT_LOCK_FDS[$role]:-}" ]] \
        || die "Internal error: lock '${role}' is not acquired in this process."
    echo "${CHECKOUT_LOCK_FDS[$role]}"
}

ensure_checkout_lock() {
    # Acquire ROLE's lock unless this process already holds it. The
    # staging update acquires production inside quiescence when the
    # full path runs (after the drain, so a lock-holding worker can
    # finish); a SKIPPED quiescence — no modelbank profile, or the
    # unit stopped — must not skip the lock: the child envelope
    # carries both lock slots and the mutation stays serialized
    # against concurrent deploys.
    local role="$1"
    [[ -n "${CHECKOUT_LOCK_FDS[$role]:-}" ]] && return 0
    with_checkout_lock "$role"
}

checkout_probe_lock_fd() {
    # Ownership probe for one populated envelope lock slot, run once
    # per slot. ORDER IS LOAD-BEARING (all four cases verified on this
    # host, 2026-11-05):
    #   1. The fresh-descriptor check must come FIRST: flock -n
    #      ACQUIRES, it does not query; probing the inherited fd first
    #      would let a forged descriptor on a free lock acquire it and
    #      pass.
    #   2. The device:inode check proves the descriptor is open on
    #      THIS lock file — without it, any descriptor on another file
    #      (stdin on /dev/null, a temp file) passes whenever the real
    #      lock happens to be held.
    #   3. The probe descriptor opens with <> (never >, which
    #      truncates); never LOCK_UN or F_DUPFD here — releasing would
    #      let a waiter steal the lock.
    # A free lock means the envelope is forged or the outer arm died;
    # recursive success on the inherited descriptor (flock is per open
    # file description) proves it is us.
    local fd="$1" lock="$2" probe
    [[ "$fd" =~ ^[0-9]+$ ]] || die "Lock fd slot '${fd}' is not a number."
    (( fd >= 3 )) || die "Lock fd ${fd} is a standard descriptor; refusing it."
    [ -e "/proc/self/fd/$fd" ] || die "Lock descriptor $fd is not open."
    [ "$(stat -L -c %d:%i "/proc/self/fd/$fd")" = "$(stat -c %d:%i "$lock")" ] \
        || die "Descriptor $fd is not open on $lock."
    exec {probe}<>"$lock"
    if flock -n -x "$probe"; then
        exec {probe}>&-
        die "Lock $lock is held by nobody; the envelope is forged or the outer arm died."
    fi
    exec {probe}>&-
    flock -n -x "$fd" || die "Lock $lock is held by another process, not by us."
}

checkout_run_without_locks() {
    # Run "$@" with every acquired checkout-lock descriptor closed in
    # the child. Children that can daemonize — notably any ssh that
    # could start a ControlMaster / ControlPersist master — must not
    # inherit a lock: the flock would live as long as that daemon's
    # inherited descriptor. Short-lived git, docker build, kubectl,
    # ros run, and npm children may inherit it.
    local -a pre=(exec)
    local role
    for role in "${!CHECKOUT_LOCK_FDS[@]}"; do
        pre+=("${CHECKOUT_LOCK_FDS[$role]}>&-")
    done
    ( "${pre[@]}"; "$@" )
}

#
# The internal re-exec envelope.
#
# Marker __DATA_UI_REEXEC_V<n>__, then fixed slots, never omitted,
# '-' when not applicable:
#   ROLE REMOTE BRANCH FULL_SHA LOCK_FD PROD_LOCK_FD
#   HANDOFF_KIND HANDOFF_PATH HANDOFF_SHA ACTION ARGS...
#

checkout_envelope_marker() {
    echo "__DATA_UI_REEXEC_V${DATA_UI_REEXEC_PROTOCOL}__"
}

envelope_marker_shaped() {
    # Any __DATA_UI_REEXEC_V<n>__ spelling, ours or not — the
    # dispatcher uses this to reject user-supplied markers at the
    # public boundary with a protocol-mismatch message instead of
    # "unknown command".
    [[ "${1:-}" =~ ^__DATA_UI_REEXEC_V[0-9]+__$ ]]
}

internal_envelope_present() {
    [[ "${1:-}" == "$(checkout_envelope_marker)" ]]
}

checkout_handoff_path_valid() {
    [[ "$1" =~ ^models/local/deploy-incoming/[A-Za-z0-9_-]+\.lisp$ ]]
}

parse_internal_envelope() {
    # Grammar-only parse of the re-exec envelope (the inner arm's entry
    # point). A malformed envelope — bad, duplicate, extra, or unknown
    # fields — is a hard error here, before any fallible guard; there
    # is never a fallback to public dispatch. A user-supplied marker
    # that reaches this far dies on the guards (typically the lock
    # probe), which is the design: the envelope cannot be forged
    # without an inherited, held lock descriptor.
    [[ "${1:-}" == "$(checkout_envelope_marker)" ]] \
        || die "Internal error: parse_internal_envelope called without the marker."
    shift
    (( $# >= 10 )) \
        || die "Malformed internal envelope: wanted 10 fixed slots then ACTION [ARGS...]; got too few."
    ENVELOPE_ROLE="$1"; ENVELOPE_REMOTE="$2"; ENVELOPE_BRANCH="$3"
    ENVELOPE_FULL_SHA="$4"; ENVELOPE_LOCK_FD="$5"; ENVELOPE_PROD_LOCK_FD="$6"
    ENVELOPE_HANDOFF_KIND="$7"; ENVELOPE_HANDOFF_PATH="$8"; ENVELOPE_HANDOFF_SHA="$9"
    ENVELOPE_ACTION="${10}"
    ENVELOPE_ARGS=("${@:11}")
    case "$ENVELOPE_ROLE" in
        production|staging) ;;
        *) die "Malformed internal envelope: unknown ROLE '${ENVELOPE_ROLE}'." ;;
    esac
    if [[ "$ENVELOPE_REMOTE" == "-" ]]; then
        [[ "$ENVELOPE_BRANCH" == "-" ]] \
            || die "Malformed internal envelope: BRANCH '${ENVELOPE_BRANCH}' without REMOTE."
    else
        [[ "$ENVELOPE_BRANCH" != "-" ]] \
            || die "Malformed internal envelope: REMOTE '${ENVELOPE_REMOTE}' without BRANCH."
        parse_source_spec "${ENVELOPE_REMOTE}/${ENVELOPE_BRANCH}"
    fi
    [[ "$ENVELOPE_FULL_SHA" == "-" || "$ENVELOPE_FULL_SHA" =~ ^[0-9a-f]{40}$ ]] \
        || die "Malformed internal envelope: FULL_SHA '${ENVELOPE_FULL_SHA}' is neither '-' nor a full SHA."
    local slot ref val
    for slot in LOCK_FD PROD_LOCK_FD; do
        ref="ENVELOPE_${slot}"
        val="${!ref}"
        [[ "$val" == "-" || "$val" =~ ^[0-9]+$ ]] \
            || die "Malformed internal envelope: ${slot} '${val}' is neither '-' nor a number."
        [[ "$val" == "-" ]] || (( val >= 3 )) \
            || die "Malformed internal envelope: ${slot} ${val} is a standard descriptor."
    done
    [[ "$ENVELOPE_PROD_LOCK_FD" != "$ENVELOPE_LOCK_FD" ]] \
        || die "Malformed internal envelope: PROD_LOCK_FD equals LOCK_FD (${ENVELOPE_LOCK_FD})."
    case "$ENVELOPE_HANDOFF_KIND" in
        none|local|remote) ;;
        *) die "Malformed internal envelope: HANDOFF_KIND '${ENVELOPE_HANDOFF_KIND}'." ;;
    esac
    if [[ "$ENVELOPE_HANDOFF_KIND" == "none" ]]; then
        [[ "$ENVELOPE_HANDOFF_PATH" == "-" && "$ENVELOPE_HANDOFF_SHA" == "-" ]] \
            || die "Malformed internal envelope: handoff slots set under KIND none."
    else
        checkout_handoff_path_valid "$ENVELOPE_HANDOFF_PATH" \
            || die "Malformed internal envelope: HANDOFF_PATH '${ENVELOPE_HANDOFF_PATH}' is neither '-' nor models/local/deploy-incoming/<token>.lisp."
        [[ "$ENVELOPE_HANDOFF_SHA" =~ ^[0-9a-f]{64}$ ]] \
            || die "Malformed internal envelope: HANDOFF_SHA is not a SHA-256 hex string."
    fi
    # Per-action shape.
    case "$ENVELOPE_ACTION" in
        deploy)
            (( ${#ENVELOPE_ARGS[@]} == 1 )) \
                || die "Malformed internal envelope: deploy takes exactly one model operand."
            valid_model_operand "${ENVELOPE_ARGS[0]}" "deploy operand"
            [[ "$ENVELOPE_REMOTE" != "-" && "$ENVELOPE_FULL_SHA" != "-" ]] \
                || die "Malformed internal envelope: deploy carries a real REMOTE and FULL_SHA."
            [[ "$ENVELOPE_LOCK_FD" != "-" && "$ENVELOPE_PROD_LOCK_FD" == "-" ]] \
                || die "Malformed internal envelope: deploy carries LOCK_FD only."
            ;;
        delete)
            (( ${#ENVELOPE_ARGS[@]} == 1 )) \
                || die "Malformed internal envelope: delete takes exactly one model operand."
            valid_model_operand "${ENVELOPE_ARGS[0]}" "delete operand"
            [[ "$ENVELOPE_REMOTE" == "-" && "$ENVELOPE_BRANCH" == "-" && "$ENVELOPE_FULL_SHA" == "-" ]] \
                || die "Malformed internal envelope: delete carries '-' in REMOTE/BRANCH/FULL_SHA."
            [[ "$ENVELOPE_LOCK_FD" != "-" && "$ENVELOPE_PROD_LOCK_FD" == "-" ]] \
                || die "Malformed internal envelope: delete carries LOCK_FD only."
            ;;
        staging-update-child)
            (( ${#ENVELOPE_ARGS[@]} == 0 )) \
                || die "Malformed internal envelope: staging-update-child takes no ARGS."
            [[ "$ENVELOPE_REMOTE" != "-" && "$ENVELOPE_FULL_SHA" != "-" ]] \
                || die "Malformed internal envelope: staging-update-child carries a real REMOTE and FULL_SHA."
            [[ "$ENVELOPE_LOCK_FD" != "-" && "$ENVELOPE_PROD_LOCK_FD" != "-" ]] \
                || die "Malformed internal envelope: staging-update-child carries both lock slots."
            ;;
        *)
            die "Malformed internal envelope: unknown ACTION '${ENVELOPE_ACTION}'."
            ;;
    esac
}

verify_internal_envelope() {
    # The fallible guards, in order: canonical script root, role host,
    # selected full SHA (where not '-'), and lock ownership for each
    # populated lock slot. Callers install their cleanup traps for
    # inherited temporary artifacts BEFORE calling this.
    local role="$ENVELOPE_ROLE" checkout root
    case "$role" in
        production) checkout="$DEPLOY_CHECKOUT" ;;
        staging) checkout="$STAGING_CHECKOUT" ;;
    esac
    checkout=$(readlink -m "$checkout")
    root=$(canonical_checkout_root)
    [[ "$root" == "$checkout" ]] \
        || die "Internal envelope root mismatch: this script is ${root}, the ${role} checkout is ${checkout}."
    case "$role" in
        production)
            [[ "$(hostname)" == "$DEPLOY_HOST" ]] \
                || die "Production envelope on the wrong host (this is $(hostname), DEPLOY_HOST is ${DEPLOY_HOST})."
            ;;
        staging)
            [[ "$(hostname)" == "$STAGING_HOST" ]] \
                || die "Staging envelope on the wrong host (this is $(hostname), STAGING_HOST is ${STAGING_HOST})."
            ;;
    esac
    if [[ "$ENVELOPE_FULL_SHA" != "-" ]]; then
        local head
        head=$(checkout_git "$checkout" rev-parse HEAD) \
            || die "Cannot read HEAD of ${checkout} under the envelope."
        [[ "$head" == "$ENVELOPE_FULL_SHA" ]] \
            || die "Checkout moved under the envelope: HEAD is ${head}, envelope says ${ENVELOPE_FULL_SHA}."
    fi
    if [[ "$ENVELOPE_LOCK_FD" != "-" ]]; then
        checkout_probe_lock_fd "$ENVELOPE_LOCK_FD" "$(checkout_lock_file "$role")"
    fi
    if [[ "$ENVELOPE_PROD_LOCK_FD" != "-" ]]; then
        checkout_probe_lock_fd "$ENVELOPE_PROD_LOCK_FD" "$(checkout_lock_file production)"
    fi
}
