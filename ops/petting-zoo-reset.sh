#!/bin/bash
# petting-zoo-reset.sh — nightly golden reset for Data UI e-demos
# (demo-apps-plan Phase 7, D13/D14/D16; Snapshot 2 addresses).
#
# Restores every enabled e-demo profile to its stg-<profile>-golden
# snapshot: day accounts, junk rows, and uploaded files not in golden
# are destroyed; exposure, credentials, and stg-<p>-last are
# untouched. modelbank (class demo) is never in the default set —
# name it explicitly for a grungy-day restore (D13); that wipes every
# VIP minted since the last golden re-take.
#
# Usage (as root — systemd unit or sudo):
#   petting-zoo-reset.sh              # every enabled e-demo (nightly)
#   petting-zoo-reset.sh <p> [...]    # explicit profiles, any class;
#                                     # bypasses the enabled check
#
# Don't-unify (D16), load-bearing here:
#   - raw systemctl stop/start, never the start/stop verbs. The stop
#     verb would refresh p-last with day-account grime; the start
#     verb's volume-wins would skip the golden restore entirely.
#   - the stg-<p>-last slots are never touched by this script.
#
# Source of record: ops/petting-zoo-reset.sh in the data-ui repo.
# Edit the source, reinstall; never edit the installed copy.
set -euo pipefail

DATA_UI_CHECKOUT="${DATA_UI_CHECKOUT:-/home/macnod/common-lisp/data-ui}"
DATA_UI_STATE="${DATA_UI_STATE:-/data/data-ui}"
PROFILES_DIR="${DATA_UI_STATE}/profiles"
SNAPSHOT_DIR="${DATA_UI_STATE}/snapshots"
UNIT_RUN_USER="macnod"
UNIT_RUN_HOME="/home/macnod"
SCRIPT_NAME=$(basename "$0")

# systemd starts this in / and sudo env_reset strips the interactive
# environment; the defaults above cover both paths (the unit pins
# DATA_UI_STATE as belt, D15 risk 28).
cd "$DATA_UI_CHECKOUT" || { echo "$SCRIPT_NAME: cannot cd to ${DATA_UI_CHECKOUT}" >&2; exit 1; }

log() { echo "$SCRIPT_NAME: $*"; }
warn() { echo "$SCRIPT_NAME: warning: $*" >&2; }
die() { echo "$SCRIPT_NAME: ERROR: $*" >&2; exit 1; }

# Parse one KEY=VALUE out of a profile.env without sourcing it into
# the root shell (keep the root env surface minimal). Last value wins,
# matching how `source` would resolve duplicates.
env_key() {
    sed -n "s/^${1}=//p" "$2" | tail -n1
}

profile_env() { echo "${PROFILES_DIR}/${1}/profile.env"; }

# The nightly set: every profile whose DEMO_CLASS stamp says e-demo
# AND whose unit is enabled (paused = disable = skipped; a
# stopped-but-enabled unit is still in the set and is reset — D14).
# Never a hardcoded list; a deleted profile self-removes (the stamp
# dies with profile.env).
enumerate_e_demos() {
    local env p class
    [[ -d "$PROFILES_DIR" ]] || return 0
    for env in "$PROFILES_DIR"/*/profile.env; do
        [[ -f "$env" ]] || continue
        p="$(basename "$(dirname "$env")")"
        class="$(env_key DEMO_CLASS "$env")"
        [[ "$class" == "e-demo" ]] || continue
        systemctl is-enabled --quiet "dataui@${p}" 2>/dev/null || continue
        echo "$p"
    done
}

# Poll the app's /health endpoint. $2 attempts × 2s.
wait_for_health() {
    local port="$1" tries="$2" i
    for ((i = 0; i < tries; i++)); do
        if curl -fsS -m 5 "http://127.0.0.1:${port}/health" \
                >/dev/null 2>&1; then
            return 0
        fi
        sleep 2
    done
    return 1
}

reset_profile() {
    local profile="$1" explicit="$2"
    local env port unit state
    env="$(profile_env "$profile")"
    unit="dataui@${profile}"

    [[ -f "$env" ]] \
        || die "no profile.env for '${profile}' (unknown profile?)"
    port="$(env_key HTTP_PORT "$env")"
    [[ -n "$port" ]] || die "profile '${profile}' has no HTTP_PORT"

    # An e-demo in the nightly set without a golden is the bootstrap
    # window (started, not yet snapshotted): skip, do not fail (Phase 7).
    # Explicit args fall through and let snapshot restore fail hard —
    # a missing named snapshot is an error, not a degrade (D16).
    if [[ "$explicit" != "t" ]] \
       && [[ ! -f "${SNAPSHOT_DIR}/stg-${profile}-golden.dump" ]]; then
        warn "${profile}: no stg-${profile}-golden snapshot (bootstrap" \
             "window) — skipped"
        return 0
    fi

    state="$(systemctl is-active "$unit" 2>/dev/null || true)"
    if [[ "$state" == "active" || "$state" == "activating" \
          || "$state" == "reloading" ]]; then
        # Pre-wait: /health, not "active" — Type=simple is active
        # before (main) listens, and at boot catch-up this script
        # races the dataui@ units themselves (D14). Compile-tolerant.
        # Timeout = skip: never restore into a half-raised state.
        if ! wait_for_health "$port" 300; then
            warn "${profile}: never reached /health within 600s —" \
                 "skipped (the next 04:10 normalizes)"
            return 0
        fi
    else
        # Stopped-but-enabled (e.g. the morning after a failed
        # nightly): still in the set and still reset (D14). The app
        # is fully down — nothing to pre-wait; the stop below is a
        # no-op and snapshot restore enforces Postgres itself.
        log "${profile}: unit not active (${state:-unknown}) —" \
            "proceeding (stop is a no-op)"
    fi

    systemctl stop "$unit"
    # runuser (not sudo -u): initializes the target's groups, and
    # HOME is pinned — docker's config lookup under /root would
    # otherwise fail for the unit user. scripts/data-ui owns env
    # resolution (load_profile / resolve_snapshot_address).
    # The app is down here by design: restore + unreachable /api/info
    # skips the confirm on either qualifier (verb-split policy,
    # Snapshot 2) — no FORCE anywhere.
    if ! runuser -u "$UNIT_RUN_USER" \
             -- env HOME="$UNIT_RUN_HOME" \
             scripts/data-ui snapshot restore \
                 "stg-${profile}-golden" "stg:${profile}"; then
        # The restore rolls back from its staged safety pair, so the
        # database is unchanged; the unit is left stopped (the 503s
        # through the domain are the alarm — do not re-enable).
        # Rotating pre-restore slot (D16), refreshed by the attempt.
        die "restore of stg-${profile}-golden failed — unit left" \
            "stopped. Pre-restore state: ${SNAPSHOT_DIR}/stg-${profile}-prerestore" \
            "(rotating slot). Repair per runbook item 6, then re-run."
    fi
    systemctl start "$unit"

    # Warm check: fasls exist, so 30 × 2s. A failed poll is not a
    # failed app — Restart=always brings the unit back; only this
    # report suffers (Phase 7).
    if wait_for_health "$port" 30; then
        log "${profile}: reset to stg-${profile}-golden complete"
    else
        warn "${profile}: did not return to /health within 60s" \
             "(Restart=always will keep trying)"
    fi
}

if [[ "$(id -u)" -ne 0 ]]; then
    die "must run as root (sudo $0, or the dataui-reset.service unit)"
fi

targets=()
if [[ $# -eq 0 ]]; then
    while IFS= read -r p; do
        targets+=("$p")
    done < <(enumerate_e_demos)
    if [[ ${#targets[@]} -eq 0 ]]; then
        log "no enabled e-demo profiles — nothing to reset"
        exit 0
    fi
    log "nightly set (enabled e-demos): ${targets[*]}"
else
    targets=("$@")
    log "explicit profiles: ${targets[*]}"
fi

for profile in "${targets[@]}"; do
    reset_profile "$profile" "$([[ $# -eq 0 ]] && echo "" || echo t)"
done

log "done: ${#targets[@]} profile(s) processed"
