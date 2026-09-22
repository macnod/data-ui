#!/bin/bash
# dataui@<profile>.service wrapper (D4/D14/D15).
#
# The unit's ExecStart. Sources the profile env, raises the database,
# applies init.sql with errors suppressed, then execs (main) — one
# start path shared by first bring-up, reboot, and post-pause resume.
# Slime is a passenger: (main) → (init) → start-swank-server serves
# Swank on SWANK_PORT (loopback via SWANK_INTERFACE) with
# :dont-close t, so clients attach and detach freely.
#
# Source of record: ops/profile-run.sh in the data-ui repo. Edit the
# source, reinstall; never edit the installed copy.
set -euo pipefail

PROFILE="$1"
DATA_UI_CHECKOUT="${DATA_UI_CHECKOUT:-/home/macnod/common-lisp/data-ui}"
DATA_UI_STATE="${DATA_UI_STATE:-/data/data-ui}"
ENV_FILE="${DATA_UI_STATE}/profiles/${PROFILE}/profile.env"

die() { echo "profile-run[$PROFILE]: ERROR: $1" >&2; exit 1; }

[[ -f "$ENV_FILE" ]] || die "no profile.env at ${ENV_FILE}"
cd "$DATA_UI_CHECKOUT" || die "cannot cd to ${DATA_UI_CHECKOUT}"

set -a
# shellcheck source=/dev/null
source "$ENV_FILE"
set +a

# (main) reads MODEL_NAME from the environment; a missing key must
# fail here, loudly — not reach Lisp as (set-model nil).
[[ -n "${MODEL_NAME:-}" ]] || die "MODEL_NAME missing from ${ENV_FILE}"

# D4: never set LOG_FILE empty — lisp/data-ui.lisp treats "" as a path
# ((or (u:getenv "LOG_FILE") *standard-output*)). Unset it so logs go
# to stdout, which journald owns.
unset LOG_FILE
# D4 belt: the Lisp default for the swank interface is 0.0.0.0; an
# unset var silently exposes Swank on every interface. Loopback unless
# profile.env deliberately overrides.
export SWANK_INTERFACE="${SWANK_INTERFACE:-127.0.0.1}"

mkdir -p "$DOCUMENT_ROOT" "$FS_TEMP_DIRECTORY"
# Mode 700, host user — never let docker create the bind source as
# root (verified uid pattern, D16). A hand-run `systemctl start` on a
# vacuum must not bypass this either.
mkdir -m 700 -p "$PGDATA_DIR"

docker compose -p "$DB_CONTAINER" -f "$DB_DOCKER_COMPOSE" up -d \
    || die "compose up failed"

# pg_isready pin (D14): exec -T (dies without a TTY under systemd)
# and -U "$DB_USER" (never -U postgres; a profile DB has no postgres
# role). Never reuse wait_for_postgres — both branches are exec -it.
waited=0
until docker compose -p "$DB_CONTAINER" -f "$DB_DOCKER_COMPOSE" \
        exec -T "$DB_SERVICE" pg_isready -U "$DB_USER" &>/dev/null; do
    if (( waited >= 120 )); then
        die "PostgreSQL did not become ready in 120s"
    fi
    sleep 2; waited=$(( waited + 2 ))
done

# init.sql, errors suppressed. Load-bearing after a restore: bare
# CREATE TABLEs (no IF NOT EXISTS) over a restored schema would all
# fail, and that is fine — the tables already exist. Do not "fix" the
# suppression (D16 don't-unify). Rendered per-profile to avoid the
# shared tests/init.sql path racing concurrent units.
INIT_SQL="$(mktemp)"
trap 'rm -f "$INIT_SQL"' EXIT
sed "s/:database_name:/${DB_NAME}/" tests/init-template.sql > "$INIT_SQL"
psql -h "$DB_HOST" -p "$DB_PORT" -d "$DB_NAME" -U "$DB_USER" \
     -f "$INIT_SQL" &>/dev/null || true

exec ros run -- --disable-debugger \
    --eval "(require :data-ui)" \
    --eval "(in-package :data-ui)" \
    --eval "(main)"
