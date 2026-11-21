#!/bin/bash
# tests/scripts/checkout-tests.sh — shell regression suite for the
# deployment-cleanup work (checkout selection, the re-exec envelope,
# locks, the remote forwarder, handoff candidates/promotion/recovery,
# routing, Model Bank quiescence/activation, and the staging update
# supervisor).
#
# Plain Bash; temporary directories; no Bats; not the legacy tests/run.
# Real local Git (bare public + private repos seeded from the CURRENT
# working tree), real flock, real filesystem/symlink/process/signal
# behavior. SSH networking, sudo, Docker, k3d, HAProxy, systemd,
# journalctl, curl, npm, psql (except the one real-PostgreSQL drain
# test), and ros are recording/stateful stubs on PATH; the remote
# forwarder's command strings are re-evaluated with the declared shell
# (bash -c) exactly as the remote login shell would reparse them. Tests
# never touch real /data/data-ui profiles, snapshots, locks, or
# services — everything goes through overrides and the stub PATH.
#
# Usage: tests/scripts/checkout-tests.sh [test-name-substring...]
#   RUN_SLOW_SBCL=0 skips the real two-checkout SBCL load test
#   SKIP_DOCKER=1   skips the real-PostgreSQL drain test

set -u

REPO_ROOT="$(cd "$(dirname "$(dirname "$(dirname "$(readlink -f "$0")")")")" && pwd)"
REAL_HOST="$(hostname)"
ORIG_PATH="$PATH"

TOTAL=0; PASSED=0; FAILED=0; SKIPPED=0
CURRENT=""
H=""

# ---------------------------------------------------------------------------
# Tiny framework
# ---------------------------------------------------------------------------

fail() {
    echo "$*" >> "$H/fails.current"
    echo "    FAIL: $*" >&2
}

note() { echo "    note: $*" >&2; }

skip_test() {
    echo "    SKIP: $*" >&2
    echo "SKIP: $*" >> "$H/skips"
}

assert_eq() {
    [[ "$1" == "$2" ]] || fail "assert_eq: got [$1] wanted [$2]${3:+ — $3}"
}

assert_contains() {
    [[ "$1" == *"$2"* ]] || fail "assert_contains: [${2}] not found in: ${1:0:400}${3:+ — $3}"
}

assert_not_contains() {
    [[ "$1" != *"$2"* ]] || fail "assert_not_contains: [${2}] unexpectedly found in: ${1:0:400}${3:+ — $3}"
}

assert_file() { [[ -f "$1" ]] || fail "missing file: $1"; }
assert_no_file() { [[ ! -f "$1" ]] || fail "unexpected file: $1"; }
assert_no_dir() { [[ ! -d "$1" ]] || fail "unexpected directory: $1"; }
assert_dir() { [[ -d "$1" ]] || fail "missing directory: $1"; }

t() {
    local name="$1"; shift
    local filter_ok=0 f
    if (( ${#TEST_FILTERS[@]} > 0 )); then
        for f in "${TEST_FILTERS[@]}"; do
            [[ "$name" == *"$f"* ]] && filter_ok=1
        done
        (( filter_ok )) || return 0
    fi
    CURRENT="$name"; TOTAL=$((TOTAL+1))
    setup_harness
    : > "$H/fails.current"; : > "$H/skips"
    echo "== $name"
    ( "test_$name" "$@" ) || true
    if [[ -s "$H/fails.current" ]]; then
        FAILED=$((FAILED+1))
        sed 's/^/    FAIL: /' "$H/fails.current"
    elif [[ -s "$H/skips" ]]; then
        SKIPPED=$((SKIPPED+1)); TOTAL=$((TOTAL-1)); PASSED=$((PASSED)) # skipped ≠ failed
        note "skipped ($(cat "$H/skips" | head -1))"
    else
        PASSED=$((PASSED+1))
    fi
    CURRENT=""
}

expect_die() {
    # expect_die <needle> cmd... — command must exit nonzero and its
    # combined output must contain the needle.
    local needle="$1"; shift
    local out st
    out=$(run_cmdline "$@" 2>&1); st=$?
    (( st != 0 )) || fail "expected nonzero exit from: $* (output: ${out:0:300})"
    [[ "$out" == *"$needle"* ]] || fail "expected output to contain [$needle]; got: ${out:0:300}"
    LAST_DIE_OUTPUT="$out"
}

run_cmdline() {
    # run_cmdline WORDS... — run a command line that may begin with an
    # `env VAR=... ` prefix. env(1) cannot execute shell functions, so
    # the assignments are hoisted into exported variables around the
    # remaining words (typically suite helpers like run_cli). Without
    # this, `env DRY_RUN=1 run_cli ...` died with
    # `env: 'run_cli': No such file or directory`.
    if [[ "${1:-}" == "env" ]]; then
        shift
        local -a pre=()
        while [[ $# -gt 0 && "$1" =~ ^[A-Za-z_][A-Za-z0-9_]*= ]]; do
            pre+=("$1"); shift
        done
        if (( ${#pre[@]} > 0 )); then
            ( export "${pre[@]}"; "$@" )
        else
            "$@"
        fi
    else
        "$@"
    fi
}

expect_ok() {
    local out st
    out=$(run_cmdline "$@" 2>&1); st=$?
    (( st == 0 )) || fail "expected success from: $* — exit ${st}: ${out:0:400}"
    LAST_OK_OUTPUT="$out"
}

# ---------------------------------------------------------------------------
# Harness: temp world, real git fixtures, stub PATH
# ---------------------------------------------------------------------------

setup_harness() {
    H=$(mktemp -d /tmp/data-ui-checkout-tests.XXXXXX)
    if [[ -n "${H_OLD:-}" ]]; then rm -rf "$H_OLD" 2>/dev/null || true; fi
    H_OLD="$H"
    export STUB="$H/stub"
    local bin="$H/bin"
    mkdir -p "$STUB" "$bin" "$H/repos" "$H/state" "$H/ops" "$H/systemd" \
             "$H/haproxy/conf.d" "$H/work"

    # ---- seed: the CURRENT working tree, committed (carries this
    # implementation, including any not-yet-committed edits). Runtime
    # logs are excluded: tests/*.log includes the REPL's and test
    # runner's DEBUG logs, which have reached MULTIPLE GIGABYTES on
    # this host — seeding them into tmpfs /tmp once per test once
    # filled the filesystem and destabilized the whole machine. ----
    mkdir -p "$H/seed"
    ( tar -C "$REPO_ROOT" \
          --exclude=./.git \
          --exclude=./web/node_modules \
          --exclude=./web/dist \
          --exclude=./web/.vite \
          --exclude=./models/local \
          --exclude=./tests/init.sql \
          --exclude='./tests/*.log' \
          -cf - . ) | tar -C "$H/seed" -xf -
    local _ps=( "${PIPESTATUS[@]}" )
    if (( ${_ps[0]} != 0 || ${_ps[1]} != 0 )); then
        echo "FATAL: seeding the sandbox from the working tree failed" \
             "(tar statuses: ${_ps[*]}; $(df -h /tmp | tail -1))." >&2
        exit 1
    fi
    git -C "$H/seed" init -q -b master
    git -C "$H/seed" add -A
    git -C "$H/seed" -c user.email=t@t -c user.name=t commit -qm "seed"

    # ---- bare remotes: public and private (same seed initially;
    # individual tests push divergent content explicitly) ----
    git -C "$H/seed" clone -q --bare . "$H/public.git"
    git -C "$H/seed" clone -q --bare . "$H/private.git"

    # ---- development clone (the invoking script for every test) ----
    git -C "$H" clone -q "$H/public.git" devclone

    # ---- stub state defaults ----
    mkdir -p "$STUB/k8s" "$STUB/ss" "$STUB/fields" "$STUB/hooks" \
             "$STUB/journal" "$STUB/systemd-units"
    : > "$STUB/systemctl.log"; : > "$STUB/sudo.log"; : > "$STUB/ros.log"
    : > "$STUB/ssh.log"; : > "$STUB/npm.log"; : > "$STUB/docker.log"
    sd_set_unit "dataui-reset.timer" enabled active
    sd_set_unit "dataui-reset.service" disabled inactive
    # A plausible host haproxy config, so bootstrap's greps behave.
    printf 'EXTRAOPTS="-f /etc/haproxy/conf.d"\n' > "$H/haproxy/haproxy.defaults"
    printf 'defaults\n    use_backend %%[req.hdr(host),lower,map(/etc/haproxy/data-ui.map)] if { req.hdr(host),lower,map(/etc/haproxy/data-ui.map) -m found }\n' > "$H/haproxy/haproxy.cfg"

    write_stubs "$bin"
}

sd_set_unit() {
    # sd_set_unit UNIT ENABLED ACTIVE  [invid]  (UNIT may omit .service,
    # like real systemctl; the state file always carries the suffix)
    local unit="$1" en="$2" ac="$3" inv="${4:-dead0000beef}"
    case "$unit" in *.service|*.timer|*.socket|*.target) ;; *) unit="${unit}.service" ;; esac
    echo "$en $ac $inv" > "$STUB/systemd-units/$unit"
}

sd_unit_state() {
    local unit="$1"
    case "$unit" in *.service|*.timer|*.socket|*.target) ;; *) unit="${unit}.service" ;; esac
    cat "$STUB/systemd-units/$unit" 2>/dev/null || echo "missing unknown unknown"
}

sd_journal_append() {
    # sd_journal_append UNIT "line"...
    local unit="$1"; shift
    local f="$STUB/journal/$unit"
    touch "$f"
    printf '%s\n' "$@" >> "$f"
}

set_fields() {
    # set_fields MODEL key=val...
    local model="$1"; shift
    mkdir -p "$STUB/fields/$model"
    local kv
    for kv in "$@"; do
        echo "${kv#*=}" > "$STUB/fields/$model/${kv%%=*}"
    done
}

default_fields() {
    # The canonical deploy fixture most tests use.
    set_fields todos \
        name=todos "title=Todos Demo" version=1.0 \
        domain=todos.demo.data-ui.com repl=NIL
    set_fields books \
        name=books "title=Books" version=2.0 \
        domain=books.demo.data-ui.com repl=true
}

cli_env_statements() {
    cat <<ENVE
export PATH="$H/bin:$ORIG_PATH"
# Default-assignment, not export: a test's run_cmdline env prefix
# (exported around run_cli) must survive into the subject — the
# unwritable-lock-root test depends on overriding this one.
export CHECKOUT_LOCK_ROOT="${CHECKOUT_LOCK_ROOT:-$H/repos}"
export DEPLOY_CHECKOUT="$H/repos/production"
export STAGING_CHECKOUT="$H/repos/staging"
export DATA_UI_PUBLIC_URL="$H/public.git"
export DATA_UI_PRIVATE_URL="$H/private.git"
export DEPLOY_HOST="${CLI_DEPLOY_HOST:-$REAL_HOST}"
export STAGING_HOST="${CLI_STAGING_HOST:-$REAL_HOST}"
export DEPLOY_STATE_DIR="$H/state/deploy"
export K3D_CLUSTER=testcluster
export DEPLOY_BOOTSTRAP_CHECKOUT="${CLI_BOOTSTRAP_CHECKOUT:-$H/devclone}"
export HAPROXY_CFG="$H/haproxy/haproxy.cfg"
export HAPROXY_CONF_D="$H/haproxy/conf.d"
export HAPROXY_MAP="$H/haproxy/data-ui.map"
export HAPROXY_DEFAULTS="$H/haproxy/haproxy.defaults"
export OPS_LIB_DIR="$H/ops"
export SYSTEMD_UNIT_DIR="$H/systemd"
export DATA_UI_CHECKOUT="$H/repos/staging"
export STAGING_HEALTH_TIMEOUT="${CLI_HEALTH_TIMEOUT:-8}"
export DEPLOY_HEALTH_TIMEOUT="${CLI_HEALTH_TIMEOUT:-8}"
export MODELBANK_DRAIN_TIMEOUT="${CLI_DRAIN_TIMEOUT:-5}"
export TMPDIR="$H/work"
ENVE
    # DATA_UI_STATE can carry arbitrary metacharacters (the remote
    # reparse test injects spaces and quotes); the heredoc above is
    # expanded and then EVALed by run_cli, so this one value goes
    # through %q to survive that eval byte-for-byte.
    printf 'export DATA_UI_STATE=%q\n' "${CLI_DATA_UI_STATE:-$H/state}"
}

run_cli() {
    # Run the development clone's script in the harness environment.
    ( eval "$(cli_env_statements)"
      cd "${CLI_CWD:-$H/work}" || exit 1
      exec "$H/devclone/scripts/data-ui" "$@" )
}

run_prod_cli() {
    # Run the PRODUCTION clone's script directly (forged-envelope tests).
    ( eval "$(cli_env_statements)"
      cd "${CLI_CWD:-$H/work}" || exit 1
      exec "$H/repos/production/scripts/data-ui" "$@" )
}

run_staging_cli() {
    ( eval "$(cli_env_statements)"
      cd "${CLI_CWD:-$H/work}" || exit 1
      exec "$H/repos/staging/scripts/data-ui" "$@" )
}

prod_sha() { git -C "$H/repos/production" rev-parse HEAD 2>/dev/null; }
public_master_sha() { git -C "$H/public.git" rev-parse refs/heads/master; }
private_master_sha() { git -C "$H/private.git" rev-parse refs/heads/master; }

make_staging_clone() {
    # A hand-made staging clone (as the first staging update would
    # bootstrap), for tests that exercise routine staging routing.
    git -C "$H" clone -q -o private "$H/private.git" repos/staging
    mkdir -p "$H/repos/staging/web"
    cp "$REPO_ROOT/web/package.json" "$REPO_ROOT/web/package-lock.json" \
       "$H/repos/staging/web/" 2>/dev/null || true
}

make_profile() {
    # make_profile NAME [HTTP_PORT [DEMO_CLASS]]
    local name="$1" port="${2:-8100}" class="${3:-demo}"
    mkdir -p "$H/state/profiles/$name/files" "$H/state/profiles/$name/temp"
    cat > "$H/state/profiles/$name/profile.env" <<ENV
HTTP_PORT=$port
SWANK_PORT=$((port + 3000))
SWANK_INTERFACE=127.0.0.1
DB_PORT=$((port + 1000))
DB_NAME=$name
DB_USER=$name
DB_PASSWORD=pw-$name
DB_CONTAINER=pg-$name
DB_SERVICE=pg
DB_DOCKER_COMPOSE=ops/pg-profile.yaml
PGDATA_DIR=$H/state/profiles/$name/pgdata
PG_UID=$(id -u)
PG_GID=$(id -g)
ADMIN_PASSWORD=admin-$name
JWT_SECRET=jwt-$name
DOCUMENT_ROOT=$H/state/profiles/$name/files
FS_TEMP_DIRECTORY=$H/state/profiles/$name/temp
LOG_FILE=$H/state/profiles/$name/repl.log
MODEL_NAME=$name
PUBLIC_ORIGIN=http://127.0.0.1:$port
HTTP_HOST=127.0.0.1
DB_HOST=127.0.0.1
WEB_DIRECTORY=web/dist
LOG_SEVERITY=INFO
RUN_TESTS=false
DEMO_CLASS=$class
ENV
    chmod 600 "$H/state/profiles/$name/profile.env"
}

make_vip_model_file() {
    # make_vip_model_file NAME — a hand-written model file (content is
    # irrelevant to the stubbed ros; identity comes from the fields
    # fixture set with set_fields).
    local path="$H/work/vip-$1.lisp"
    printf "'(:name %s :title \"%s\" :version 9.9 :domain %s.demo.data-ui.com :types (:items (:fields (:name (:type :text)))))\n" \
        "$1" "$1" "$1" > "$path"
    echo "$path"
}

write_stubs() {
    local bin="$1"

    _write_stub "$bin/ros" <<'EOF'
#!/bin/bash
# ros stub: records argv + full env; answers model-field reads from
# $STUB/fields/<model>/<field>; optional compile/prewarm failure knobs.
# Calls made under the ssh stub (the simulated remote side) log to
# $ROS_LOG so caller-side and remote-side compiles stay attributable.
n=$(ls "$STUB" 2>/dev/null | grep -c '^ros-env-')
env > "$STUB/ros-env-$n" 2>/dev/null
{ printf 'ros-call:%s\n' "$*"; } >> "${ROS_LOG:-$STUB/ros.log}"
for a in "$@"; do
    case "$a" in
        *init-database*)
            [[ -f "$STUB/ros-compile-fails" ]] && exit 1
            ;;
        *"pre-warm"*)
            [[ -f "$STUB/ros-prewarm-fails" ]] && exit 1
            echo "pre-warm: :data-ui loaded"
            ;;
    esac
done
if [[ -n "${DATA_UI_FIELD_NAME:-}" ]]; then
    # Handoff snapshots and installed candidates have unpredictable
    # basenames (mktemp / deploy tokens); they belong to MODEL_NAME —
    # the real reader ignores the basename when :model-path is given,
    # so mirror that. Canonical reads keep the basename key.
    case "${DATA_UI_MODEL_PATH:-}" in
        *data-ui-handoff.*|*data-ui-remote-handoff.*|*models/local/deploy-incoming/*.lisp)
            model="${MODEL_NAME:-unknown}" ;;
        *)
            model=$(basename "${DATA_UI_MODEL_PATH:-models/unknown}" .lisp) ;;
    esac
    f="$STUB/fields/$model/$DATA_UI_FIELD_NAME"
    if [[ -f "$f" ]]; then cat "$f"; else echo "NIL"; fi
fi
exit 0
EOF

    _write_stub "$bin/kubectl" <<'EOF'
#!/bin/bash
# kubectl stub: stateful via $STUB/k8s.
echo "kubectl $*" >> "$STUB/kubectl.log"
mode=""; ns=""; name=""
a=("$@")
i=0
while (( i < ${#a[@]} )); do
    case "${a[$i]}" in
        -n) ns="${a[$((i+1))]}"; i=$((i+2)); continue ;;
        get) mode=get ;;
        apply) mode=apply ;;
        delete) mode=delete ;;
        create) mode=create ;;
        rollout) mode=rollout ;;
        scale) mode=scale ;;
        exec) mode=exec ;;
    esac
    i=$((i+1))
done
last="${a[$(( ${#a[@]} - 1 ))]}"
case "$mode" in
    get)
        case "$*" in
            *"get ns -o name"*|*"get namespaces -o name"*)
                for d in "$STUB"/k8s/ns/*; do
                    [[ -e "$d" ]] && echo "namespace/$(basename "$d")"
                done
                exit 0 ;;
            *"get nodes"*) echo "10.0.0.1"; exit 0 ;;
            *"get ns "*)
                name=$(echo "$*" | grep -oP 'get ns \K\S+')
                [[ -e "$STUB/k8s/ns/$name" ]] && echo "$name found" && exit 0
                exit 1 ;;
        esac
        if [[ "$*" == *"jsonpath={.spec.ports[0].nodePort}"* ]]; then
            svc=$(echo "$*" | grep -oP 'get svc( -n \S+)? \K\S+')
            f="$STUB/k8s/nodeport/${svc}"
            [[ -f "$f" ]] && cat "$f"
            exit 0
        fi
        if [[ "$*" == *"jsonpath={.data."* ]]; then
            key=$(echo "$*" | grep -oP 'jsonpath=\{\.data\.\K[a-z-]+')
            sname=$(echo "$*" | grep -oP 'get secret -n \S+ \K\S+')
            f="$STUB/k8s/secret/${sname}/${key}"
            [[ -f "$f" ]] && base64 -w0 "$f"
            exit 0
        fi
        if [[ "$*" == *"jsonpath={.spec.hostPath.path}"* ]]; then
            pv=$(echo "$*" | grep -oP 'get pv \K\S+')
            f="$STUB/k8s/pv/${pv}"
            [[ -f "$f" ]] && cat "$f"
            exit 0
        fi
        if [[ "$*" == *"get secret"* ]]; then
            sname=$(echo "$*" | grep -oP 'get secret -n \S+ \K\S+')
            [[ -d "$STUB/k8s/secret/${sname}" ]] && exit 0
            exit 1
        fi
        exit 0 ;;
    apply)
        [[ -f "$STUB/kubectl-apply-fails" ]] && exit 1
        echo "applied:$*" >> "$STUB/kubectl.log"
        exit 0 ;;
    delete) exit 0 ;;
    create)
        # create configmap ... --dry-run=client -o yaml (stdout flows
        # into the caller's redirect)
        echo "apiVersion: v1"
        echo "kind: ConfigMap"
        exit 0 ;;
    rollout)
        [[ -f "$STUB/kubectl-rollout-fails" ]] && exit 1
        exit 0 ;;
    scale|exec) exit 0 ;;
esac
exit 0
EOF

    _write_stub "$bin/docker" <<'EOF'
#!/bin/bash
# docker stub. `docker run` is passed through to the REAL docker (the
# drain test's throwaway PostgreSQL); everything else is recorded.
if [[ "${1:-}" == "run" ]]; then
    exec /usr/bin/docker "$@"
fi
echo "docker $*" >> "$STUB/docker.log"
if [[ "${1:-}" == "build" ]]; then
    n=$(ls "$STUB" | grep -c '^build-context-')
    ctx=""
    prev=""
    for a in "$@"; do
        [[ "$prev" == "-t" ]] && tag="$a"
        prev="$a"
    done
    ctx="${@: -1}"
    [[ -f "$STUB/docker-build-fails" ]] && exit 1
    {
        echo "BUILD tag=$tag ctx=$ctx"
        (cd "$ctx" 2>/dev/null && find . -type f | sort)
        # hashed at build time — the deploy's cleanup trap removes the
        # context before the test can look at it
        (cd "$ctx" 2>/dev/null && find models/local -type f -exec sha256sum {} \; 2>/dev/null | sort)
    } > "$STUB/build-context-$n" 2>/dev/null || echo "BUILD ctx-missing" > "$STUB/build-context-$n"
    echo "$tag" >> "$STUB/built-tags"
    exit 0
fi
if [[ "${1:-}" == "image" && "${2:-}" == "inspect" ]]; then
    # Architecture answer for the registry probe's platform check.
    case "$(uname -m)" in
        aarch64) echo arm64 ;;
        x86_64)  echo amd64 ;;
        *)       echo unknown ;;
    esac
    exit 0
fi
if [[ "${1:-}" == "image" ]]; then
    [[ -f "$STUB/docker-import-fails" ]] && exit 1
    exit 0
fi
if [[ "${1:-}" == "login" ]]; then
    [[ -f "$STUB/docker-login-fails" ]] && exit 1
    exit 0
fi
if [[ "${1:-}" == "pull" ]]; then
    ref="${@: -1}"
    [[ -f "$STUB/docker-pull-fails" ]] && { echo "docker stub: pull failure knob" >&2; exit 1; }
    if grep -qxF "$ref" "$STUB/ecr-images" 2>/dev/null; then
        mkdir -p "$STUB/docker-local"
        echo "$ref" >> "$STUB/docker-local/images" 2>/dev/null || true
        exit 0
    fi
    { echo "docker stub: no such image ${ref}" >&2; exit 1; }
fi
if [[ "${1:-}" == "push" ]]; then
    ref="${@: -1}"
    [[ -f "$STUB/docker-push-fails" ]] && { echo "docker stub: push failure knob" >&2; exit 1; }
    mkdir -p "$STUB"
    echo "$ref" >> "$STUB/ecr-images"
    exit 0
fi
if [[ "${1:-}" == "ps" ]]; then exit 0; fi
if [[ "${1:-}" == "compose" ]]; then
    case "$*" in
        *" pg_isready "*|*"pg_isready"*) exit 0 ;;
        *) exit 0 ;;
    esac
fi
exit 0
EOF

    _write_stub "$bin/k3d" <<'EOF'
#!/bin/bash
echo "k3d $*" >> "$STUB/k3d.log"
[[ -f "$STUB/k3d-import-fails" ]] && exit 1
exit 0
EOF

    _write_stub "$bin/aws" <<'EOF'
#!/bin/bash
# aws stub: records argv and answers from fixtures under $STUB.
#   s3 cp <src> <dst>                 one side s3://bucket/key; copies
#                                     to/from $STUB/s3/<bucket>/<key>
#   s3api list-objects-v2             Contents[].Key from the fixture
#                                     tree (keys are the files)
#   ecr get-login-password            fixture password (exit 1 knob)
#   route53 list-resource-record-sets start-at-or-after over
#                                     $STUB/r53/<zone>.json
#   route53 change-resource-record-sets / wait ... / get-change
#                                     log + INSYNC
#   ec2 describe-addresses            $STUB/ec2-addresses.json
#   ec2 describe-images               $STUB/aws-describe-images
#   ec2 (everything else)             log; exit 0 (failure knobs:
#                                     $STUB/aws-fails-<verb>)
echo "aws $*" >> "$STUB/aws.log"
fail=""
for a in "$@"; do
    case "$a" in
        s3|s3api|ecr|route53|ec2|dlm|sts|iam|budgets|ce) ;; # services
        *) for w in $a; do :; done ;;
    esac
done
if [[ -f "$STUB/aws-fails" ]]; then echo "aws stub: failure knob" >&2; exit 1; fi
if [[ "$1 $2" == "s3 cp" && -f "$STUB/aws-fails-s3cp" ]]; then
    echo "aws stub: s3 cp failure knob" >&2; exit 1
fi
case "$1 $2" in
    "s3 cp")
        src="$3"; dst="$4"
        if [[ "$src" == s3://* ]]; then
            key="${src#s3://}"; bucket="${key%%/*}"; key="${key#*/}"
            if [[ -f "$STUB/s3/$bucket/$key" ]]; then
                cp "$STUB/s3/$bucket/$key" "$dst" 2>/dev/null || exit 1
                exit 0
            fi
            echo "aws stub: no such object $src" >&2; exit 1
        elif [[ "$dst" == s3://* ]]; then
            key="${dst#s3://}"; bucket="${key%%/*}"; key="${key#*/}"
            mkdir -p "$STUB/s3/$bucket/$(dirname "$key")"
            cp "$src" "$STUB/s3/$bucket/$key" 2>/dev/null || exit 1
            exit 0
        fi
        exit 1 ;;
    "s3 ls")
        # s3 ls s3://bucket/prefix/ → "PRE name/" lines from the tree
        target="${@: -1}"; target="${target#s3://}"
        bucket="${target%%/*}"; prefix="${target#*/}"
        [[ -d "$STUB/s3/$bucket/$prefix" ]] || exit 0
        for d in "$STUB/s3/$bucket/$prefix"/*/; do
            [[ -d "$d" ]] && echo "PRE $(basename "$d")/"
        done
        exit 0 ;;
esac
case "$1" in
    s3api)
        case "$2" in
            list-objects-v2)
                bucket=""; prefix=""
                prev=""
                for a in "${@:3}"; do
                    [[ "$prev" == "--bucket" ]] && bucket="$a"
                    [[ "$prev" == "--prefix" ]] && prefix="$a"
                    prev="$a"
                done
                {
                    echo '{ "Contents": ['
                    first=1
                    while IFS= read -r f; do
                        [[ -f "$f" ]] || continue
                        key="$prefix$(basename "$f")"
                        (( first )) || echo ","
                        printf '  {"Key": "%s", "Size": %s}' "$key" "$(stat -c %s "$f")"
                        first=0
                    done < <(ls "$STUB/s3/$bucket/$prefix"* 2>/dev/null | sort)
                    echo ''
                    echo '] }'
                } | jq -c . 2>/dev/null || echo '{"Contents": []}'
                exit 0 ;;
            head-object)
                key=""; bucket=""
                prev=""
                for a in "${@:3}"; do
                    [[ "$prev" == "--bucket" ]] && bucket="$a"
                    [[ "$prev" == "--key" ]] && key="$a"
                    prev="$a"
                done
                f="$STUB/s3/$bucket/$key"
                if [[ -f "$f" ]]; then
                    out=""
                    prev=""
                    for a in "${@:3}"; do
                        [[ "$prev" == "--query" ]] && out="$a"
                        prev="$a"
                    done
                    case "$out" in
                        ContentLength) stat -c %s "$f"; exit 0 ;;
                        LastModified) echo "2026-11-19T00:00:00Z"; exit 0 ;;
                    esac
                    exit 0
                fi
                exit 1 ;;
            get-bucket-lifecycle-configuration)
                cat "$STUB/s3-lifecycle.json" 2>/dev/null || exit 1
                exit 0 ;;
        esac
        exit 0 ;;
    ecr)
        case "$2" in
            get-login-password)
                [[ -f "$STUB/ecr-login-fails" ]] && exit 1
                echo "stub-ecr-password"
                exit 0 ;;
        esac
        exit 0 ;;
    route53)
        case "$2" in
            list-resource-record-sets)
                [[ -f "$STUB/aws-fails-route53" ]] && exit 1
                zone=""; start=""; starttype=""
                prev=""
                for a in "${@:3}"; do
                    [[ "$prev" == "--hosted-zone-id" ]] && zone="$a"
                    [[ "$prev" == "--start-record-name" ]] && start="$a"
                    [[ "$prev" == "--start-record-type" ]] && starttype="$a"
                    prev="$a"
                done
                jq -c --arg s "${start,,}" \
                   '[.[] | select((.Name | ascii_downcase) >= $s)] | sort_by(.Name) | .[0:1] | {ResourceRecordSets: .}' \
                   "$STUB/r53/$zone.json" 2>/dev/null || echo '{"ResourceRecordSets": []}'
                exit 0 ;;
            change-resource-record-sets)
                echo "route53 change $*" >> "$STUB/r53-changes.log"
                [[ -f "$STUB/r53-change-fails" ]] && exit 1
                echo "/change/StubChange1"
                exit 0 ;;
            wait|get-change)
                exit 0 ;;
        esac
        exit 0 ;;
    ec2)
        case "$2" in
            describe-addresses)
                [[ -f "$STUB/aws-fails-describe-addresses" ]] && exit 1
                cat "$STUB/ec2-addresses.json" 2>/dev/null || echo '{"Addresses": []}'
                exit 0 ;;
            describe-instances)
                cat "$STUB/aws-describe-instances" 2>/dev/null || true
                exit 0 ;;
            describe-images)
                cat "$STUB/aws-describe-images" 2>/dev/null || true
                exit 0 ;;
            stop-instances)
                [[ -f "$STUB/aws-stop-fails" ]] && { echo "aws stub: stop failure knob" >&2; exit 1; }
                exit 0 ;;
            terminate-instances)
                [[ -f "$STUB/aws-terminate-fails" ]] && exit 1
                exit 0 ;;
            wait)
                [[ -f "$STUB/aws-wait-fails" ]] && exit 1
                exit 0 ;;
            create-tags)
                exit 0 ;;
        esac
        exit 0 ;;
    sts)
        [[ "$2" == get-caller-identity ]] && { echo 232168665831; exit 0; }
        exit 0 ;;
esac
exit 0
EOF

    _write_stub "$bin/dig" <<'EOF'
#!/bin/bash
# dig stub: answers +short A lookups from $STUB/dig/<name> (one IPv4
# per line). A missing name or the $STUB/dig-fails knob means a
# resolver failure (empty output, exit 9), never a silent wrong
# answer.
[[ -f "$STUB/dig-fails" ]] && exit 9
name=""
for a in "$@"; do
    case "$a" in
        @*) ;;
        A) ;;
        *) [[ "$a" != -* && "$a" != +* ]] && name="$a" ;;
    esac
done
f="$STUB/dig/${name,,}"
[[ -f "$f" ]] && cat "$f"
exit 0
EOF

    _write_stub "$bin/sudo" <<'EOF'
#!/bin/bash
# sudo stub: logs, then runs the command (stub binaries resolve via
# PATH; real binaries like install/rm/tee/sed/mkdir run for real).
# /data/k8s paths are redirected into the sandbox (the script's
# hardcoded hostPath root must not be touched by tests).
if [[ "${1:-}" == "-n" && "${2:-}" == "true" ]]; then exit 0; fi
args=()
for a in "$@"; do
    case "$a" in
        /data/k8s/*) a="$STUB/data-k8s/${a#/data/k8s/}" ;;
    esac
    args+=("$a")
done
echo "sudo ${args[*]}" >> "$STUB/sudo.log"
exec "${args[@]}"
EOF

    _write_stub "$bin/ssh" <<'EOF'
#!/bin/bash
# ssh stub: records the full command and evaluates the remote command
# string with bash -c — the same single reparse the remote login shell
# performs. Set STUB_NO_SSH_EXEC=1 to record only.
printf 'ssh %q\n' "$*" >> "$STUB/ssh.log"
[[ -f "$STUB/ssh-fails" ]] && exit 1
if [[ "${STUB_NO_SSH_EXEC:-}" == "1" ]]; then exit 0; fi
if [[ "${1:-}" == "-t" || "${1:-}" == "-tt" ]]; then shift; fi
cmd="${@: -1}"
# Arrival simulation: this stub plays the deploy host, so rewrite the
# forwarded host pins to the real hostname before evaluating the
# remote command — otherwise the "remote" launcher still believes it
# is not on the deploy host and forwards again, forever.
real=$(hostname)
if [[ -n "${DEPLOY_HOST:-}" && "$DEPLOY_HOST" != "$real" ]]; then
    cmd=${cmd//DEPLOY_HOST=$DEPLOY_HOST/DEPLOY_HOST=$real}
fi
if [[ -n "${STAGING_HOST:-}" && "$STAGING_HOST" != "$real" ]]; then
    cmd=${cmd//STAGING_HOST=$STAGING_HOST/STAGING_HOST=$real}
fi
# Everything executed below this line is the simulated REMOTE side;
# its ros calls log separately so tests can attribute compiles to a
# side (the caller forwards, the remote launcher works). And like a
# real ssh login, the remote side starts from a SCRUBBED environment:
# PATH/HOME/TERM/TMPDIR plus the stub plumbing only. The caller's
# environment (secrets included) must not leak across — the forwarded
# allowlist inside the command string is the only channel.
export ROS_LOG="${ROS_LOG:-$STUB/ros-remote.log}"
exec env -i PATH="$PATH" HOME="$HOME" TERM="${TERM:-xterm}" \
     TMPDIR="${TMPDIR:-/tmp}" STUB="$STUB" ROS_LOG="$ROS_LOG" \
     bash -c "$cmd"
EOF

    _write_stub "$bin/scp" <<'EOF'
#!/bin/bash
# scp stub: copies to the local path behind host:path.
args=()
for a in "$@"; do [[ "$a" == -* ]] || args+=("$a"); done
src="${args[0]}"
dst="${args[1]}"
dst="${dst#*:}"
cp -- "$src" "$dst"
EOF

    _write_stub "$bin/systemctl" <<'EOF'
#!/bin/bash
# Stateful systemd fake. Unit state lives in $STUB/systemd-units/<unit>
# as "<enabled|disabled> <active|inactive|activating|reloading|failed|masked|unknown> <invocation-id>".
echo "systemctl $*" >> "$STUB/systemctl.log"
cmd="${1:-}"; shift || true
# strip global flags, keep positional words
pos=()
for a in "$@"; do
    case "$a" in
        --*) ;;
        -*) ;;
        *) pos+=("$a") ;;
    esac
done
u="${pos[0]}"
# Like real systemctl, a unit without a suffix means .service (sd_set_unit
# callers and the script both say "dataui@books"; list-units matches
# dataui@*.service files).
case "$u" in *.service|*.timer|*.socket|*.target) ;; *) u="${u}.service" ;; esac
sf="$STUB/systemd-units/$u"
read -r en ac inv < <(cat "$sf" 2>/dev/null || echo missing unknown unknown)
case "$cmd" in
    is-enabled)
        echo "$en"
        [[ "$en" == "enabled" ]] && exit 0 || exit 1 ;;
    is-active)
        echo "$ac"
        [[ "$ac" == "active" ]] && exit 0 || exit 3 ;;
    show)
        # show UNIT -p InvocationID --value  (any flag order)
        for a in "$@"; do
            if [[ "$prev" == "-p" && "$a" == "InvocationID" ]]; then want=1; fi
            prev="$a"
        done
        if [[ "${want:-}" == "1" ]]; then echo "$inv"; fi
        exit 0 ;;
    enable)
        [[ -f "$STUB/systemctl-fails" ]] && exit 1
        echo "enabled $ac $inv" > "$sf"; exit 0 ;;
    disable)
        [[ -f "$STUB/systemctl-fails" ]] && exit 1
        echo "disabled $ac $inv" > "$sf"; exit 0 ;;
    start)
        [[ -f "$STUB/systemctl-fails" ]] && exit 1
        if [[ "$u" == "dataui-reset.service" ]]; then
            beh="ok"; [[ -f "$STUB/reset-behavior" ]] && beh=$(cat "$STUB/reset-behavior")
            case "$beh" in
                block) echo "disabled activating $inv" > "$sf"; exit 0 ;;
                fail)  echo "disabled failed $inv" > "$sf"; exit 1 ;;
                *)     echo "disabled inactive ran$(date +%s%N)" > "$sf"; exit 0 ;;
            esac
        fi
        echo "$en active $inv" > "$sf"; exit 0 ;;
    stop)
        [[ -f "$STUB/systemctl-fails" ]] && exit 1
        echo "$en inactive $inv" > "$sf"
        # catch-up emulation: a test hook can flip another unit's
        # state when this one stops (systemd starting the reset
        # service as the timer pauses)
        if [[ -x "$STUB/hooks/$u.on-stop" ]]; then
            bash "$STUB/hooks/$u.on-stop" || true
        fi
        exit 0 ;;
    restart)
        [[ -f "$STUB/systemctl-fails" ]] && exit 1
        newinv="$(head -c 16 /dev/urandom | od -An -tx1 | tr -d ' \n')"
        echo "$en active $newinv" > "$sf"
        # The unit's wrapper "prints" its startup lines into the journal
        # (what profile-run.sh + the ASDF load contract would emit).
        ck="${DATA_UI_CHECKOUT:-/data/data-ui/repos/staging}"
        if [[ ! -f "$STUB/journal-silent/$u" ]]; then
            mkdir -p "$STUB/journal"
            printf '%s\n' \
                "profile-run: checkout: $ck" \
                "DATA-UI-ASDF-SOURCE=$ck/" \
                "DATA-UI-PACKAGE-ROOT=$ck/" \
                >> "$STUB/journal/$u"
        fi
        if [[ -x "$STUB/hooks/$u.post-restart" ]]; then
            bash "$STUB/hooks/$u.post-restart" || true
        fi
        exit 0 ;;
    daemon-reload)
        [[ -f "$STUB/systemctl-fails" ]] && exit 1
        exit 0 ;;
    list-units)
        pattern="${pos[0]}"
        for f in "$STUB"/systemd-units/*; do
            unit=$(basename "$f")
            # ${pattern%@*} cannot recover the base ('dataui@*.service'
            # would yield 'dataui', never 'dataui@') — test the pattern
            # shape directly instead.
            [[ "$unit" == "$pattern" \
               || ( "$pattern" == dataui@* && "$unit" == dataui@*.service ) ]] || continue
            read -r e2 a2 i2 < "$f"
            echo "$unit loaded $a2 $a2 Data UI unit"
        done
        exit 0 ;;
    *)
        exit 0 ;;
esac
EOF

    _write_stub "$bin/journalctl" <<'EOF'
#!/bin/bash
# journalctl fake over $STUB/journal/<unit> (one line per entry; the
# cursor is the line count). A unit without a suffix means .service,
# matching the systemctl stub's state files.
unit=""; after=""; showcursor=""; lines=""
prev=""
for a in "$@"; do
    case "$prev" in
        -u) unit="$a" ;;
        --after-cursor) after="${a#*:}" ;;
    esac
    case "$a" in
        --show-cursor) showcursor=1 ;;
        -n) : ;;
        0) : ;;
    esac
    prev="$a"
done
case "$unit" in *.service|*.timer|*.socket|*.target) ;; *) unit="${unit}.service" ;; esac
f="$STUB/journal/$unit"
cur=0
[[ -f "$f" ]] && cur=$(wc -l < "$f")
if [[ -n "$showcursor" ]]; then
    echo "-- cursor: $cur"
    exit 0
fi
if [[ -n "$after" ]]; then
    tail -n +"$((after + 1))" "$f" 2>/dev/null
fi
exit 0
EOF

    _write_stub "$bin/curl" <<'EOF'
#!/bin/bash
# curl stub: URL is the last non-flag argument (or after -d payloads).
# Full argv is recorded to $STUB/curl.log (the --resolve pin assertions
# read it).
echo "curl $*" >> "$STUB/curl.log"
url=""
prev=""
for a in "$@"; do
    case "$a" in
        http://*|https://*) url="$a" ;;
    esac
    prev="$a"
done
# IMDSv2 (the EC2 metadata service): token via PUT, values via GET,
# from $STUB/imds-* fixtures. A missing fixture is a dead metadata
# service (empty output, exit 7) — what site_public_ip sees off-EC2.
case "$url" in
    http://169.254.169.254/latest/api/token)
        [[ -f "$STUB/imds-token" ]] && { cat "$STUB/imds-token"; exit 0; }
        exit 7 ;;
    http://169.254.169.254/latest/meta-data/public-ipv4)
        [[ -f "$STUB/imds-public-ip" ]] && { cat "$STUB/imds-public-ip"; exit 0; }
        exit 7 ;;
    http://169.254.169.254/latest/meta-data/instance-id)
        [[ -f "$STUB/imds-instance-id" ]] && { cat "$STUB/imds-instance-id"; exit 0; }
        exit 7 ;;
esac
port=""
if [[ "$url" =~ :([0-9]+) ]]; then port="${BASH_REMATCH[1]}"; fi
# Simulated connection refusal: a countdown file curl-refuse-<port>
# makes each call on that port exit 1 while the count lasts (a service
# briefly unreachable right after rollout -- the hop probe's transient
# case). Checked before the hook and the responses: a refused
# connection has neither.
if [[ -n "$port" && -f "$STUB/curl-refuse-$port" ]]; then
    n=$(cat "$STUB/curl-refuse-$port" 2>/dev/null)
    if [[ "$n" =~ ^[0-9]+$ ]] && (( n > 0 )); then
        echo $(( n - 1 )) > "$STUB/curl-refuse-$port"
        exit 1
    fi
fi
if [[ -n "$port" && -x "$STUB/hooks/curl-$port" ]]; then
    bash "$STUB/hooks/curl-$port" "$url" || true
fi
code=""
if [[ -f "$STUB/curl-responses" ]]; then
    while read -r pat c; do
        [[ -z "$pat" ]] && continue
        if [[ "$url" == *"$pat"* ]]; then code="$c"; break; fi
    done < "$STUB/curl-responses"
fi
if [[ -z "$code" ]]; then
    case "$url" in
        */health) code=200 ;;
        *) code=000 ;;
    esac
fi
printf '%s\n' "$code"
exit 0
EOF

    _write_stub "$bin/npm" <<'EOF'
#!/bin/bash
echo "npm $* (cwd=$(pwd))" >> "$STUB/npm.log"
[[ -f "$STUB/npm-fails" ]] && exit 1
exit 0
EOF

    _write_stub "$bin/haproxy" <<'EOF'
#!/bin/bash
echo "haproxy $*" >> "$STUB/haproxy.log"
[[ "$1" == "-c" ]] || exit 0
[[ -f "$STUB/haproxy-validate-fails" ]] && exit 1
exit 0
EOF

    _write_stub "$bin/psql" <<'EOF'
#!/bin/bash
# psql stub: the drain query's rows come from $STUB/psql-rows. Tests
# that run a REAL postgres create $STUB/psql-pass-through to exec the
# real binary instead.
echo "psql $*" >> "$STUB/psql.log"
if [[ -f "$STUB/psql-pass-through" ]]; then
    exec /usr/bin/psql "$@"
fi
[[ -f "$STUB/psql-fails" ]] && exit 2
if [[ "$*" == *"-Atc"* && -f "$STUB/psql-rows" ]]; then
    cat "$STUB/psql-rows"
fi
exit 0
EOF

    _write_stub "$bin/ss" <<'EOF'
#!/bin/bash
# ss stub: prints a listener line for every marker in $STUB/ss/.
for f in "$STUB"/ss/*; do
    [[ -e "$f" ]] || continue
    echo "LISTEN 0 128 127.0.0.1:$(basename "$f") users:((\"fake\"))"
done
exit 0
EOF
}

_write_stub() {
    local path="$1"
    {
        echo '#!/bin/bash'
        echo "STUB=\"$STUB\""
        echo "export STUB"
        cat
    } > "$path"
    chmod +x "$path"
}

# ---------------------------------------------------------------------------
# Helpers shared by tests
# ---------------------------------------------------------------------------

green_deploy() { # green_deploy MODEL — a fully stubbed-green deploy
    default_fields >/dev/null
    [[ -d "$H/repos/production" ]] || true
    run_cli deploy "$1"
}

stub_count() {
    # stub_count PATTERN LOG — number of lines matching PATTERN in LOG,
    # normalized to a bare "0" when the log is absent or nothing
    # matches. grep -c already prints 0 (and exits 1) on no match and
    # prints nothing on a missing file; the old `|| echo 0` idiom
    # appended a SECOND 0 on the no-match path, producing "0\n0" and
    # corrupting every assertion that consumed it.
    local n
    n=$(grep -cF -- "$1" "$2" 2>/dev/null) || true
    echo "${n:-0}"
}

compile_calls() { stub_count 'init-database' "$STUB/ros.log"; }
remote_compile_calls() { stub_count 'init-database' "$STUB/ros-remote.log"; }
grep_sudo()    { grep -F -- "$1" "$STUB/sudo.log" 2>/dev/null || true; }
grep_systemctl() { grep -F -- "$1" "$STUB/systemctl.log" 2>/dev/null || true; }

stable_copy() { echo "$H/repos/production/models/local/$1.lisp"; }
incoming_dir() { echo "$H/repos/production/models/local/deploy-incoming"; }

forge_envelope_deploy() {
    # forge_envelope_deploy ROLE LOCK_FD SHA KIND CAND_PATH CAND_SHA MODEL
    # Slot layout must mirror parse_internal_envelope exactly:
    # ROLE REMOTE BRANCH FULL_SHA LOCK_FD PROD_LOCK_FD KIND PATH SHA
    # ACTION ARGS... (LOCK_FD and PROD_LOCK_FD must differ; KIND none
    # requires '-' path/sha).
    local role="$1" fdv="$2" sha="$3" kind="$4" cand="$5" csha="$6" model="$7"
    [[ -x "$H/repos/production/scripts/data-ui" ]] || { echo "no prod clone"; return 127; }
    run_prod_cli "__DATA_UI_REEXEC_V1__" "$role" public master "$sha" \
        "$fdv" - "$kind" "$cand" "$csha" deploy "$model"
}

deploy_envelope_sha() { public_master_sha; }

make_ns_fixture() { # make_ns_fixture NAME [NODEPORT]
    local name="$1" port="${2:-30310}"
    mkdir -p "$STUB/k8s/ns" "$STUB/k8s/nodeport" "$STUB/k8s/secret/dataui-$name-secrets" "$STUB/k8s/pv"
    touch "$STUB/k8s/ns/dataui-$name"
    echo "$port" > "$STUB/k8s/nodeport/dataui-$name"
    echo "dbpw" > "$STUB/k8s/secret/dataui-$name-secrets/db-password"
    echo "adminpw" > "$STUB/k8s/secret/dataui-$name-secrets/admin-password"
    echo "jwt" > "$STUB/k8s/secret/dataui-$name-secrets/jwt-secret"
    echo "$H/state/k8s/$name/files" > "$STUB/k8s/pv/dataui-$name-demo-files-pv"
}

start_swank_fake() {
    # start_swank_fake PORT VALUE — a real listener speaking the
    # CLASSIC swank wire protocol (6-hex length-prefixed s-expressions,
    # as swank/rpc.lisp serves and swank_eval_string speaks): reads one
    # (:emacs-rex (swank:interactive-eval ...)) request per connection
    # and answers (:return (:ok "[[VALUE]]") 1). The deploy-script
    # probe (its expression mentions scripts/data-ui) gets
    # VALUE + scripts/data-ui, like the real image would.
    local port="$1" value="$2"
    python3 - "$port" "$value" <<'PYEOF' &
import socket, sys
port, value = int(sys.argv[1]), sys.argv[2]
def frame(p): return b"%06x" % len(p) + p
srv = socket.socket(); srv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
srv.bind(("127.0.0.1", port)); srv.listen(4)
while True:
    try: c, _ = srv.accept()
    except OSError: break
    try:
        f = c.makefile("rb")
        hdr = b""
        while len(hdr) < 6:
            ch = f.read(6 - len(hdr))
            if not ch: raise EOFError
            hdr += ch
        body = f.read(int(hdr, 16))
        result = value + ("scripts/data-ui" if b"scripts/data-ui" in body else "")
        reply = b'(:return (:ok "[[' + result.encode() + b']]") 1)'
        c.sendall(frame(reply))
    except Exception:
        pass
    finally:
        try: c.close()
        except Exception: pass
PYEOF
    SWANK_FAKE_PIDS+=($!)
}

cleanup_swank_fakes() {
    local p
    for p in "${SWANK_FAKE_PIDS[@]:-}"; do kill "$p" 2>/dev/null || true; done
    SWANK_FAKE_PIDS=()
}
SWANK_FAKE_PIDS=()

git_in() { git -C "$2" "${@:3}" 2>/dev/null; }

# ---------------------------------------------------------------------------
# Sources
# ---------------------------------------------------------------------------

test_sources_default_public_master() {
    default_fields
    expect_ok run_cli deploy todos
    assert_eq "$(prod_sha)" "$(public_master_sha)" "prod HEAD == public/master"
    assert_contains "$LAST_OK_OUTPUT" "remote/branch: public/master"
    local lb
    lb=$(git -C "$H/repos/production" branch --show-current)
    assert_eq "$lb" "deploy/public/master" "per-upstream branch"
}

test_sources_explicit_private() {
    default_fields
    expect_ok run_cli deploy todos --from private/master
    assert_eq "$(prod_sha)" "$(private_master_sha)" "prod HEAD == private/master"
}

test_sources_slash_branch() {
    default_fields
    git -C "$H/seed" checkout -q -b feat/x
    echo x > "$H/seed/PROBE"; git -C "$H/seed" add -A
    git -C "$H/seed" -c user.email=t@t -c user.name=t commit -qm "feat"
    git -C "$H/seed" push -q "$H/private.git" HEAD:refs/heads/feature/foo/bar
    git -C "$H/seed" checkout -q master
    expect_ok run_cli deploy todos --from private/feature/foo/bar
    local lb
    lb=$(git -C "$H/repos/production" branch --show-current)
    assert_eq "$lb" "deploy/private/feature/foo/bar" "slash branch name preserved"
    assert_file "$H/repos/production/PROBE"
}

test_sources_switch_keeps_independent_branches() {
    default_fields
    git -C "$H/seed" checkout -q -b privwork
    echo priv > "$H/seed/PRIV"; git -C "$H/seed" add -A
    git -C "$H/seed" -c user.email=t@t -c user.name=t commit -qm "priv"
    git -C "$H/seed" push -q "$H/private.git" HEAD:refs/heads/master
    git -C "$H/seed" checkout -q master
    expect_ok run_cli deploy todos --from public/master
    local pub_sha; pub_sha=$(prod_sha)
    expect_ok run_cli deploy todos --from private/master
    assert_eq "$(prod_sha)" "$(private_master_sha)" "private selected"
    expect_ok run_cli deploy todos --from public/master
    assert_eq "$(prod_sha)" "$pub_sha" "back to public at the same SHA"
    local b1 b2
    b1=$(git -C "$H/repos/production" rev-parse refs/heads/deploy/public/master)
    b2=$(git -C "$H/repos/production" rev-parse refs/heads/deploy/private/master)
    assert_eq "$b1" "$pub_sha" "public branch independent"
    assert_eq "$b2" "$(private_master_sha)" "private branch independent"
}

test_sources_tags_stay_local() {
    default_fields
    expect_ok run_cli deploy todos
    local tag
    tag=$(git -C "$H/repos/production" tag -l 'todos-*')
    [[ -n "$tag" ]] || fail "expected a local deploy tag in the production clone"
    assert_eq "$(git -C "$H/public.git" tag -l 'todos-*')" "" "tag not pushed to public"
    assert_eq "$(git -C "$H/private.git" tag -l 'todos-*')" "" "tag not pushed to private"
}

# ---------------------------------------------------------------------------
# Parsing (all die before side effects)
# ---------------------------------------------------------------------------

assert_parse_dies_clean() {
    local desc="$1"; shift
    local out st
    out=$(run_cli "$@" 2>&1); st=$?
    (( st != 0 )) || fail "[$desc] expected nonzero exit"
    assert_no_file "$H/repos/production.lock" "[$desc] no lock taken"
    assert_no_dir "$H/repos/production" "[$desc] no clone made"
    (( $(compile_calls) == 0 )) || fail "[$desc] ros was invoked"
}

test_parsing_deploy_flags() {
    assert_parse_dies_clean "missing operand" deploy
    assert_parse_dies_clean "duplicate --from" deploy todos --from public/master --from public/master
    assert_parse_dies_clean "unknown flag" deploy todos --what
    assert_parse_dies_clean "extra positional" deploy todos extra
    assert_parse_dies_clean "--from= spelling" deploy todos --from=public/master
    assert_parse_dies_clean "--from no value" deploy todos --from
    assert_parse_dies_clean "bad remote char" deploy todos --from 'public=/master'
    assert_parse_dies_clean "leading dash branch" deploy todos --from public/-x
    assert_parse_dies_clean "HEAD branch" deploy todos --from public/HEAD
    assert_parse_dies_clean "@ branch" deploy todos --from public/@
    assert_parse_dies_clean "dotdot branch" deploy todos --from 'public/a..b'
    assert_parse_dies_clean "@{ branch" deploy todos --from 'public/x@{y'
    assert_parse_dies_clean "refs spelling" deploy todos --from public/refs/heads/master
    assert_parse_dies_clean "tilde rev" deploy todos --from public/master~1
}

test_parsing_model_operand_grammar() {
    local bad
    for bad in /abs/path ../x "a b" "a'b" 'a"b' 'a\b' 'a$b' "$(printf 'a\nb')" -x Profile PROFILE profile profile-x _x 'x_y'; do
        assert_parse_dies_clean "operand [$bad]" deploy "$bad"
        assert_parse_dies_clean "delete operand [$bad]" delete "$bad"
    done
}

test_parsing_name_mismatch_before_mutation() {
    set_fields todos name=other "title=T" version=1.0 domain=d.demo.data-ui.com repl=NIL
    expect_die "must match" run_cli deploy todos
    assert_no_dir "$H/state/deploy/todos/releases" "no manifests rendered"
    assert_no_file "$H/state/deploy/todos/secrets.env"
    assert_eq "$(stub_count 'BUILD tag=' "$STUB/docker.log")" 0 "no image built"
    assert_eq "$(git -C "$H/repos/production" tag -l 'todos-*' 2>/dev/null)" "" "no tag"
}

# ---------------------------------------------------------------------------
# Invocation
# ---------------------------------------------------------------------------

test_invocation_from_root_and_tmp_and_dirty_dev() {
    default_fields
    CLI_CWD=/ expect_ok run_cli deploy todos
    rm -rf "$H/repos"
    CLI_CWD="$H/work" default_fields
    # dirty development clone: an uncommitted change must not matter
    echo dirt > "$H/devclone/DIRT"
    expect_ok run_cli deploy todos
    assert_eq "$(prod_sha)" "$(public_master_sha)" "prod still selects public/master"
    assert_file "$H/devclone/DIRT" "dev dirt untouched"
}

# ---------------------------------------------------------------------------
# Bootstrap
# ---------------------------------------------------------------------------

test_bootstrap_missing_checkout_clones() {
    default_fields
    expect_ok run_cli deploy todos
    assert_dir "$H/repos/production"
    assert_eq "$(git -C "$H/repos/production" remote get-url public)" "$H/public.git"
    assert_eq "$(git -C "$H/repos/production" remote get-url private)" "$H/private.git"
    assert_eq "$(git -C "$H/repos/production" remote | tr '\n' ' ' | tr -d ' ')" "privatepublic" "no origin ever"
}

test_bootstrap_nonrepo_nonempty_fails_untouched() {
    mkdir -p "$H/repos/production"
    echo precious > "$H/repos/production/keepme"
    default_fields
    expect_die "not a git repository" run_cli deploy todos
    assert_file "$H/repos/production/keepme" "untouched"
}

test_bootstrap_wrong_url_and_origin_only() {
    mkdir -p "$H/repos/production"
    git -C "$H/repos/production" init -q
    git -C "$H/repos/production" remote add public "$H/private.git"
    default_fields
    expect_die "unexpected URL" run_cli deploy todos
    rm -rf "$H/repos/production"
    mkdir -p "$H/repos/production"
    git -C "$H/repos/production" init -q
    git -C "$H/repos/production" remote add origin "$H/public.git"
    default_fields
    expect_die "legacy 'origin'" run_cli deploy todos
    assert_eq "$(git -C "$H/repos/production" remote get-url origin)" "$H/public.git" "origin never renamed"
}

test_bootstrap_only_mode() {
    default_fields
    expect_ok run_cli deploy-bootstrap
    assert_dir "$H/repos/production"
    assert_eq "$(compile_calls)" 0 "no compilation"
    assert_eq "$(cat "$STUB/kubectl.log" 2>/dev/null | wc -l)" 0 "no cluster access"
    assert_contains "$LAST_OK_OUTPUT" "Bootstrap-only mode complete"
}

# ---------------------------------------------------------------------------
# Selection
# ---------------------------------------------------------------------------

test_selection_noop_and_ff() {
    default_fields
    expect_ok run_cli deploy todos
    local s1; s1=$(prod_sha)
    expect_ok run_cli deploy todos
    assert_eq "$(prod_sha)" "$s1" "no-op re-deploy keeps HEAD"
    git -C "$H/seed" checkout -q -b more
    echo more >> "$H/seed/README.md"
    git -C "$H/seed" add -A
    git -C "$H/seed" -c user.email=t@t -c user.name=t commit -qm "more"
    git -C "$H/seed" push -q "$H/public.git" HEAD:refs/heads/master
    git -C "$H/seed" checkout -q master
    expect_ok run_cli deploy todos
    assert_eq "$(prod_sha)" "$(public_master_sha)" "fast-forward applied"
}

test_selection_dirty_aborts_without_destruction() {
    default_fields
    expect_ok run_cli deploy todos
    local s1; s1=$(prod_sha)
    echo tracked-dirt >> "$H/repos/production/README.md"
    git -C "$H/seed" checkout -q -b more2
    echo more2 >> "$H/seed/README.md"; git -C "$H/seed" add -A
    git -C "$H/seed" -c user.email=t@t -c user.name=t commit -qm "m"
    git -C "$H/seed" push -q "$H/public.git" HEAD:refs/heads/master
    git -C "$H/seed" checkout -q master
    expect_die "not clean" run_cli deploy todos
    assert_contains "$(cat "$H/repos/production/README.md")" "tracked-dirt" "no reset/stash"
    assert_eq "$(prod_sha)" "$s1" "HEAD untouched"
    assert_eq "$(stub_count 'BUILD tag=' "$STUB/docker.log")" 0 "no build"
    rm -f "$H/repos/production/README.md.bak" 2>/dev/null || true
    git -C "$H/repos/production" checkout -q -- README.md 2>/dev/null || true
    echo untracked > "$H/repos/production/UNTRACKED"
    git -C "$H/seed" checkout -q -b more3; echo x >> "$H/seed/README.md"
    git -C "$H/seed" add -A; git -C "$H/seed" -c user.email=t@t -c user.name=t commit -qm m
    git -C "$H/seed" push -q "$H/public.git" HEAD:refs/heads/master
    git -C "$H/seed" checkout -q master
    expect_die "not clean" run_cli deploy todos
    assert_file "$H/repos/production/UNTRACKED" "untracked file survives"
}

test_selection_ahead_diverged_rewound_nonff() {
    default_fields
    expect_ok run_cli deploy todos
    # local-ahead: commit on the deploy branch locally
    git -C "$H/repos/production" -c user.email=t@t -c user.name=t commit -q --allow-empty -m ahead
    expect_die "recheck failed" run_cli deploy todos
    # reset to upstream, then move the upstream forward (d1) and let
    # the checkout fast-forward to it — the normal update path, and the
    # recovery from the local-ahead abort above. (The fixture must
    # actually FETCH the divergence: rewinding the upstream before any
    # fetch leaves branch == upstream == wanted, and the deploy
    # legitimately succeeds — the abort below would never trigger.)
    git -C "$H/repos/production" reset -q --hard refs/remotes/public/master
    git -C "$H/seed" checkout -q -b d1
    echo d1 >> "$H/seed/README.md"; git -C "$H/seed" add -A
    git -C "$H/seed" -c user.email=t@t -c user.name=t commit -qm d1
    git -C "$H/seed" push -q "$H/public.git" +HEAD:refs/heads/master
    git -C "$H/seed" checkout -q master
    expect_ok run_cli deploy todos
    # upstream rewound (force-pushed older history): the checkout is at
    # seed+d1, the upstream is back at seed — abort, never force
    git -C "$H/public.git" update-ref refs/heads/master "$(git -C "$H/public.git" rev-list --max-parents=0 HEAD)"
    local builds
    builds=$(stub_count 'BUILD tag=' "$STUB/docker.log")
    expect_die "recheck failed" run_cli deploy todos
    assert_eq "$(stub_count 'BUILD tag=' "$STUB/docker.log")" "$builds" "the aborted builds added no image builds"
}

test_selection_dirty_aborts_under_dryrun_and_modelfile() {
    default_fields
    expect_ok run_cli deploy todos
    echo dirt >> "$H/repos/production/README.md"
    git -C "$H/repos/production" checkout -q -- README.md 2>/dev/null || true
    # dirty tracked again, this time DRY_RUN:
    echo dirt2 >> "$H/repos/production/README.md"
    expect_die "not clean" env DRY_RUN=1 run_cli deploy todos
    local vip; vip=$(make_vip_model_file vipone)
    set_fields vipone name=vipone "title=V" version=1 domain=vipone.demo.data-ui.com repl=NIL
    expect_die "not clean" env MODEL_FILE="$vip" run_cli deploy vipone
}

test_selection_ignored_files_pass() {
    default_fields
    expect_ok run_cli deploy todos
    mkdir -p "$H/repos/production/web/dist" "$H/repos/production/web/node_modules" \
             "$H/repos/production/models/local/deploy-incoming"
    echo x > "$H/repos/production/web/dist/app.js"
    echo x > "$H/repos/production/web/node_modules/p/index.js"
    echo x > "$H/repos/production/models/local/old.lisp"
    expect_ok run_cli deploy todos
}

test_selection_git_env_contamination() {
    default_fields
    expect_ok env GIT_DIR=/nonexistent GIT_WORK_TREE=/nonexistent \
        GIT_INDEX_FILE=/nonexistent run_cli deploy todos
    assert_eq "$(prod_sha)" "$(public_master_sha)"
}

test_selection_missing_remote_branch() {
    default_fields
    expect_die "git fetch of nosuch/master failed" run_cli deploy todos --from nosuch/master
}

# ---------------------------------------------------------------------------
# Envelope
# ---------------------------------------------------------------------------

test_envelope_body_runs_once_and_status_propagates() {
    default_fields
    expect_ok run_cli deploy todos
    assert_eq "$(compile_calls)" 1 "selected body ran exactly once"
    assert_contains "$LAST_OK_OUTPUT" "Deployed Todos Demo"
    # nonzero exit from the inner propagates: make compile fail
    echo ok > "$STUB/ros-compile-fails"
    git -C "$H/seed" checkout -q -b f1; echo f >> "$H/seed/README.md"
    git -C "$H/seed" add -A; git -C "$H/seed" -c user.email=t@t -c user.name=t commit -qm f
    git -C "$H/seed" push -q "$H/public.git" HEAD:refs/heads/master
    git -C "$H/seed" checkout -q master
    local out st
    out=$(run_cli deploy todos 2>&1); st=$?
    assert_eq "$st" 1 "failure exit propagates"
    assert_contains "$out" "Model failed to compile"
}

test_envelope_wrong_root_host_sha_lock() {
    default_fields
    expect_ok run_cli deploy todos
    local sha; sha=$(prod_sha)
    # wrong root: a STAGING envelope presented to the production
    # script (deploy actions require ROLE=production, so the staging
    # role travels with the staging action, which carries BOTH lock
    # slots per the envelope's action shape)
    expect_die "root mismatch" run_prod_cli __DATA_UI_REEXEC_V1__ \
        staging private master "$sha" 9 8 none - - staging-update-child
    # wrong SHA — well-formed 40-hex but not the checkout's HEAD: the
    # checkout "moved" under the envelope between selection and exec
    local wrong="${sha:0:39}"
    [[ "${sha:39:1}" == "0" ]] && wrong+="1" || wrong+="0"
    expect_die "Checkout moved under the envelope" \
        forge_envelope_deploy production 9 "$wrong" none - - todos
    # no inherited lock at all: fd 9 is not open
    expect_die "Lock descriptor 9 is not open" \
        forge_envelope_deploy production 9 "$sha" none - - todos
    # the outer arm consumes MODEL_FILE; the inner envelope never
    # sees it (the old form pointed MODEL_FILE at a missing file and
    # expected the outer arm to ignore it — but MODEL_FILE is the VIP
    # handoff channel and the outer arm must honor it)
    local vip2; vip2=$(make_vip_model_file vipamb)
    set_fields vipamb name=vipamb "title=V" version=1 domain=vipamb.demo.data-ui.com repl=NIL
    : > "$STUB/ros.log"; rm -f "$STUB"/ros-env-*
    expect_ok env MODEL_FILE="$vip2" run_cli deploy vipamb
    # the identity read (outer arm) legitimately still sees MODEL_FILE;
    # the INNER arm's compile must not (deploy_outer unsets it before
    # the envelope exec). Env file N corresponds to ros.log line N.
    local cn
    cn=$(grep -n 'init-database' "$STUB/ros.log" | head -1 | cut -d: -f1)
    [[ -n "$cn" ]] || fail "no compile invocation found for the VIP deploy"
    assert_not_contains "$(cat "$STUB/ros-env-$((cn-1))" 2>/dev/null)" "MODEL_FILE=" \
        "MODEL_FILE not passed through to the inner arm's compile"
    # marker-shaped argument in the public grammar is rejected
    expect_die "protocol mismatch" run_cli __DATA_UI_REEXEC_V2__ x
    local out2
    out2=$(run_cli __DATA_UI_REEXEC_V1__ production public master "$sha" 9 - none - - deploy todos 2>&1); st=$?
    (( st != 0 )) || fail "forged envelope from public dispatch must die"
    assert_contains "$out2" "root mismatch" "public dispatch dies on the root guard before any lock probe"
}

test_envelope_cwd_of_selected_checkout_rejected() {
    default_fields
    expect_ok run_cli deploy todos
    local sha; sha=$(prod_sha)
    CLI_CWD="$H/repos/production" \
        expect_die "not open" run_prod_cli __DATA_UI_REEXEC_V1__ \
        production public master "$sha" 9 - none - - deploy todos
}

# ---------------------------------------------------------------------------
# Lock probe
# ---------------------------------------------------------------------------

test_lockprobe_free_lock_and_wrong_file() {
    default_fields
    expect_ok run_cli deploy todos
    make_staging_clone
    local sha; sha=$(prod_sha)
    local lock="$H/repos/production.lock"
    # a FRESH descriptor on a FREE lock: the ordering regression
    local out
    out=$(bash -c "
        source '$H/repos/production/scripts/lib/checkout.sh' >/dev/null 2>&1
        export CHECKOUT_LOCK_ROOT='$H/repos'
        exec {f}<>\"\$CHECKOUT_LOCK_ROOT/production.lock\"
        forge() { :; }
        checkout_probe_lock_fd \$f \"\$CHECKOUT_LOCK_ROOT/production.lock\"
    " 2>&1) && fail "fresh-descriptor probe on a free lock must die" || true
    assert_contains "$out" "held by nobody"
    # fd 0 / an unrelated file while the REAL lock is held
    bash -c "
        exec {fk}<>$lock
        flock -x \$fk || exit 9
        exec {ft}<>$H/work/unrelated.txt
        export CHECKOUT_LOCK_ROOT='$H/repos'
        source '$H/repos/production/scripts/lib/checkout.sh' >/dev/null 2>&1
        checkout_probe_lock_fd \$ft \"\$CHECKOUT_LOCK_ROOT/production.lock\"" \
        > "$H/work/probe-out" 2>&1 && fail "wrong-file probe must die" || true
    assert_contains "$(cat "$H/work/probe-out")" "not open on"
    # staging envelope with PROD_LOCK_FD == LOCK_FD is malformed at parse
    expect_die "PROD_LOCK_FD equals LOCK_FD" run_staging_cli \
        __DATA_UI_REEXEC_V1__ staging private master "$sha" 9 9 none - - staging-update-child
}

test_lockprobe_held_by_another_process() {
    default_fields
    expect_ok run_cli deploy todos
    local sha; sha=$(prod_sha)
    bash -c "exec {f}<>$H/repos/production.lock; flock -x \$f; sleep 8" &
    local holder=$!
    sleep 0.3
    expect_die "held by another process" forge_envelope_deploy_holder "$sha"
    kill "$holder" 2>/dev/null || true; wait "$holder" 2>/dev/null || true
}

forge_envelope_deploy_holder() {
    # Open a NEW descriptor on the production lock (not inherited from a
    # holder) and present it in the envelope: must be rejected.
    local sha="$1"
    bash -c "
        exec {f}<>$H/repos/production.lock
        exec \"\$0\" \"\$@\"
    " bash -c "
        cd $H/repos/production
        export PATH='$H/bin:$ORIG_PATH'
        export DEPLOY_CHECKOUT='$H/repos/production'
        export CHECKOUT_LOCK_ROOT='$H/repos'
        export DEPLOY_HOST='$REAL_HOST'
        exec '$H/repos/production/scripts/data-ui' __DATA_UI_REEXEC_V1__ \
            production public master '$sha' 10 - none - - deploy todos
    "
}

# ---------------------------------------------------------------------------
# Contention
# ---------------------------------------------------------------------------

test_contention_serializes_and_lock_survives_exec() {
    default_fields
    # First deploy holds the inner lock while "compiling" slowly.
    echo ok > "$STUB/ros-slow-compile"
    ( sleep 4; rm -f "$STUB/ros-slow-compile" ) &
    local slower=$!
    run_cli deploy todos >/dev/null 2>&1 &
    local first=$!
    sleep 1
    default_fields
    local out
    out=$(run_cli deploy todos 2>&1) && st=0 || st=$?
    wait "$first" 2>/dev/null || true
    kill "$slower" 2>/dev/null || true
    assert_contains "$out" "Acquired the production lock" "second deploy waited and proceeded"
    assert_eq "$(compile_calls)" 2 "both deploys completed"
}

test_contention_independent_roles_and_root() {
    default_fields
    bash -c "exec {f}<>$H/repos/staging.lock; flock -x \$f; sleep 5" &
    local holder=$!
    sleep 0.3
    expect_ok run_cli deploy todos
    kill "$holder" 2>/dev/null || true; wait "$holder" 2>/dev/null || true
    # CHECKOUT_LOCK_ROOT isolates: another root's lock never blocks
    mkdir -p "$H/repos2"
    bash -c "exec {f}<>$H/repos2/production.lock; flock -x \$f; sleep 5" &
    holder=$!
    sleep 0.3
    expect_ok run_cli deploy todos
    kill "$holder" 2>/dev/null || true; wait "$holder" 2>/dev/null || true
}

test_contention_holder_death_with_surviving_child() {
    default_fields
    bash -c '
        exec {f}<>"'"$H"'/repos/production.lock"
        flock -x $f || exit 9
        # child inherits the descriptor and outlives the holder
        sleep 6 &
        echo $! > "'"${H:?}"'/work/lock-child.pid"
        exit 0
    ' &
    local holder=$!
    wait "$holder" 2>/dev/null || true
    # The lock is still held through the child's inherited descriptor.
    local out
    out=$(timeout 3 bash -c "
        source '$H/repos/production/scripts/lib/checkout.sh' >/dev/null 2>&1
        export CHECKOUT_LOCK_ROOT='$H/repos'
        with_checkout_lock production >/dev/null 2>&1" && echo ACQUIRED || echo BLOCKED)
    assert_eq "$out" BLOCKED "surviving child keeps the lock"
    kill "$(cat "$H/work/lock-child.pid")" 2>/dev/null || true
}

# ---------------------------------------------------------------------------
# Remote forwarder
# ---------------------------------------------------------------------------

arm_remote() { CLI_DEPLOY_HOST="otherhost"; }

test_remote_production_launcher_arm() {
    default_fields
    # Bootstrap production locally first (as on the real one host).
    CLI_DEPLOY_HOST="$REAL_HOST" expect_ok run_cli deploy todos
    : > "$STUB/ros.log"; : > "$STUB/ros-remote.log"
    arm_remote
    expect_ok run_cli deploy todos
    assert_contains "$(cat "$STUB/ssh.log")" "bash\\ -c" "single-reparse command"
    assert_contains "$(head -1 "$STUB/ssh.log")" " -t\\ " "ssh -t"
    assert_eq "$(compile_calls)" 0 "caller only forwards"
    assert_eq "$(remote_compile_calls)" 1 "remote launcher ran the production script"
}

test_remote_bootstrap_launcher_arm() {
    default_fields
    arm_remote
    # No production clone yet: the bootstrap checkout's script answers.
    expect_ok run_cli deploy todos
    assert_dir "$H/repos/production" "bootstrap arm built production via the devclone launcher"
    assert_eq "$(remote_compile_calls)" 1
}

test_remote_neither_launcher_fails() {
    default_fields
    arm_remote
    # Hide BOTH remote launchers: production is absent, and the
    # bootstrap checkout points somewhere that does not exist. (The
    # old form chmod'd -x the devclone script — which also disabled
    # the CALLER, since caller and simulated remote share the
    # filesystem.) Nothing was mutated locally.
    CLI_BOOTSTRAP_CHECKOUT="$H/gone-bootstrap" \
        expect_die "no deploy launcher" run_cli deploy todos
    assert_no_file "$H/repos/production.lock" "no lock taken by the caller"
}

test_remote_metacharacter_safety_and_allowlist() {
    default_fields
    CLI_DEPLOY_HOST="$REAL_HOST" expect_ok run_cli deploy todos
    : > "$STUB/ros.log"; rm -f "$STUB"/ros-env-*
    arm_remote
    # An env value with spaces/quotes crosses the reparse intact, and a
    # secret is NOT forwarded (never on the command line, never in env).
    # (The weird DATA_UI_STATE rides the CLI_DATA_UI_STATE knob — the
    # harness env itself would otherwise overwrite a plain DATA_UI_STATE
    # prefix before the script ever sees it.)
    local weird="$H/work/sta te 'x'\"y\""
    mkdir -p "$weird"
    out=$(run_cmdline env CLI_DATA_UI_STATE="$weird" DB_PASSWORD="secret-value-xyz" \
          run_cli deploy todos 2>&1) || fail "remote deploy failed: $out"
    local envfile
    envfile=$(ls "$STUB"/ros-env-* | head -1)
    assert_contains "$(cat "$envfile")" "DATA_UI_STATE=$weird" "env value survived the reparse"
    assert_not_contains "$(cat "$envfile")" "secret-value-xyz" "secrets never forwarded"
    assert_not_contains "$(cat "$STUB/ssh.log")" "secret-value-xyz" "no secret on the command line"
}

test_remote_no_local_selection_under_dryrun() {
    default_fields
    arm_remote
    rm -f "$STUB/ros.log"
    expect_ok env DRY_RUN=1 run_cli deploy todos
    # The ssh stub runs the "remote" side in the SAME sandbox, and the
    # deploy host legitimately bootstraps production even under a dry
    # run (selection precedes the manifests stop) — so the checkout's
    # existence cannot attribute. No-local-selection is proven by
    # attribution: exactly one ssh forward (the caller's only act),
    # zero local compiles, one remote compile — a caller-side
    # bootstrap+selection would necessarily compile locally.
    assert_eq "$(wc -l < "$STUB/ssh.log")" 1 "exactly one forward, no local work first"
    assert_eq "$(compile_calls)" 0 "caller never compiles"
    assert_eq "$(remote_compile_calls)" 1 "the ssh-executed remote side did the real work"
}

test_remote_delete_forwards_confirmation_over_ssh_t() {
    default_fields
    make_ns_fixture todos
    CLI_DEPLOY_HOST="$REAL_HOST" expect_ok run_cli deploy todos
    arm_remote
    expect_ok env FORCE=1 run_cli delete todos
    assert_contains "$(head -1 "$STUB/ssh.log")" " -t\\ " "confirmation tty forwarded"
}

# ---------------------------------------------------------------------------
# Handoff (MODEL_FILE)
# ---------------------------------------------------------------------------

test_handoff_relative_and_absolute_resolution() {
    default_fields
    set_fields vipone name=vipone "title=V" version=1 domain=vipone.demo.data-ui.com repl=NIL
    # relative path resolves against the ORIGINAL cwd (production is
    # bootstrapped by this very deploy)
    rm -rf "$H/repos"
    make_vip_model_file vipone >/dev/null
    mv "$H/work/vip-vipone.lisp" "$H/work/vip-one.lisp"
    CLI_CWD="$H/work" expect_ok env MODEL_FILE=vip-one.lisp run_cli deploy vipone
    assert_file "$(stable_copy vipone)" "promoted stable copy"
    # absolute path
    rm -rf "$H/repos"
    local vip; vip=$(make_vip_model_file viptwo)
    set_fields viptwo name=viptwo "title=V" version=1 domain=viptwo.demo.data-ui.com repl=NIL
    CLI_CWD=/ expect_ok env MODEL_FILE="$vip" run_cli deploy viptwo
    assert_file "$(stable_copy viptwo)"
}

test_handoff_invalid_inputs_die_before_mutation() {
    default_fields
    local vip; vip=$(make_vip_model_file vipx)
    set_fields vipx name=vipx "title=V" version=1 domain=vipx.demo.data-ui.com repl=NIL
    expect_die "MODEL_FILE 'missing.lisp' not found" env MODEL_FILE=missing.lisp run_cli deploy vipx
    : > "$H/work/empty.lisp"
    expect_die "MODEL_FILE is empty" env MODEL_FILE="$H/work/empty.lisp" run_cli deploy vipx
    mkdir -p "$H/work/dir.lisp"
    expect_die "not a regular file" env MODEL_FILE="$H/work/dir.lisp" run_cli deploy vipx
    ln -sfn "$vip" "$H/work/link.lisp"
    expect_die "must not be a symlink" env MODEL_FILE="$H/work/link.lisp" run_cli deploy vipx
    head -c 1048577 /dev/zero | tr '\0' 'x' > "$H/work/big.lisp"
    expect_die "larger than 1 MiB" env MODEL_FILE="$H/work/big.lisp" run_cli deploy vipx
    assert_no_dir "$H/repos/production" "no clone/git/network before validation"
}

test_handoff_stable_copy_changes_only_after_both_gates() {
    default_fields
    local vip; vip=$(make_vip_model_file vipg)
    set_fields vipg name=vipg "title=V" version=1 domain=vipg.demo.data-ui.com repl=NIL
    CLI_CWD="$H/work" run_cmdline env MODEL_FILE=vip-vipg.lisp run_cli deploy vipg >/dev/null 2>&1
    assert_file "$(stable_copy vipg)" "first deploy promoted"
    local before
    before=$(sha256sum "$(stable_copy vipg)" | cut -d' ' -f1)
    # a redeploy with a failing loopback NodePort gate: every loopback
    # probe answers 503 (the public domain still answers — irrelevant,
    # the loopback gate fires first); no route is ever installed for
    # the hop, and the stable copy must not move
    printf '127.0.0.1 503\nvipg.demo.data-ui.com/health 200\n' > "$STUB/curl-responses"
    local out st
    out=$(run_cmdline env CLI_HEALTH_TIMEOUT=4 MODEL_FILE="$vip" run_cli deploy vipg 2>&1); st=$?
    rm -f "$STUB/curl-responses"
    assert_eq "$(sha256sum "$(stable_copy vipg)" | cut -d' ' -f1)" "$before" \
        "stable copy byte-identical after a failed gate"
    [[ "$out" == *"health gate"* || "$out" == *"did not"* || "$st" -ne 0 ]] || fail "expected gate failure"
}

test_handoff_failure_at_stage_leaves_stable_identical() {
    default_fields
    local vip; vip=$(make_vip_model_file viph)
    set_fields viph name=viph "title=V" version=1 domain=viph.demo.data-ui.com repl=NIL
    CLI_CWD="$H/work" run_cmdline env MODEL_FILE=vip-viph.lisp run_cli deploy viph >/dev/null 2>&1
    local before; before=$(sha256sum "$(stable_copy viph)" | cut -d' ' -f1)
    # compile failure
    echo ok > "$STUB/ros-compile-fails"
    run_cmdline env MODEL_FILE="$vip" run_cli deploy viph >/dev/null 2>&1 || true
    rm -f "$STUB/ros-compile-fails"
    # apply failure
    echo ok > "$STUB/kubectl-apply-fails"
    run_cmdline env MODEL_FILE="$vip" run_cli deploy viph >/dev/null 2>&1 || true
    rm -f "$STUB/kubectl-apply-fails"
    # build failure
    echo ok > "$STUB/docker-build-fails"
    run_cmdline env MODEL_FILE="$vip" run_cli deploy viph >/dev/null 2>&1 || true
    rm -f "$STUB/docker-build-fails"
    assert_eq "$(sha256sum "$(stable_copy viph)" | cut -d' ' -f1)" "$before" \
        "stable copy survived every failure stage"
}

test_handoff_tags_come_from_envelope_sha() {
    default_fields
    expect_ok run_cli deploy todos
    local sha; sha=$(prod_sha)
    local tag
    tag=$(git -C "$H/repos/production" tag -l 'todos-1.0-*')
    assert_eq "$(git -C "$H/repos/production" rev-parse "${tag}^{commit}")" "$sha" \
        "tag points at the envelope SHA"
}

test_handoff_dry_run_leaves_incoming_identical() {
    default_fields
    local vip; vip=$(make_vip_model_file vipd)
    set_fields vipd name=vipd "title=V" version=1 domain=vipd.demo.data-ui.com repl=NIL
    local out
    out=$(run_cmdline env DRY_RUN=1 MODEL_FILE="$vip" run_cli deploy vipd 2>&1) || fail "dry run failed: $out"
    assert_contains "$out" "DRY_RUN"
    assert_contains "$out" "would promote"
    assert_eq "$(ls -A "$(incoming_dir)" 2>/dev/null | wc -l)" 0 "deploy-incoming byte-identical (empty)"
    assert_no_file "$(stable_copy vipd)" "nothing promoted"
    # a following real deploy is unaffected
    expect_ok env MODEL_FILE="$vip" run_cli deploy vipd
    assert_file "$(stable_copy vipd)"
}

# ---------------------------------------------------------------------------
# Build context
# ---------------------------------------------------------------------------

test_buildcontext_canonical_is_curated() {
    default_fields
    # ignored runtime debris in the production clone must not enter
    mkdir -p "$H/repos/production" # (deploy will clone anyway)
    expect_ok run_cli deploy todos
    local ctxfile
    ctxfile=$(ls "$STUB"/build-context-* | tail -1)
    local listing; listing=$(cat "$ctxfile")
    assert_contains "$listing" "BUILD tag="
    assert_not_contains "$listing" "./.git/" "no .git in context"
    assert_not_contains "$listing" "models/local" "no local models in a canonical context"
    assert_not_contains "$listing" "web/dist" "no frontend output in context"
    assert_not_contains "$listing" ".fasl" "no FASLs in context"
}

test_buildcontext_tracked_models_local_refuses() {
    default_fields
    # Force-track a models/local file upstream.
    git -C "$H/seed" add -f models/local/evil.lisp 2>/dev/null || {
        mkdir -p "$H/seed/models/local"; echo x > "$H/seed/models/local/evil.lisp"
        git -C "$H/seed" add -f models/local/evil.lisp
    }
    git -C "$H/seed" -c user.email=t@t -c user.name=t commit -qm "evil tracked"
    git -C "$H/seed" push -q "$H/public.git" HEAD:refs/heads/master
    expect_die "tracks files under models/local" run_cli deploy todos
}

test_buildcontext_vip_context_contains_exactly_its_handoff() {
    default_fields
    local vip; vip=$(make_vip_model_file vipc)
    local sha256
    sha256=$(sha256sum "$vip" | cut -d' ' -f1)
    set_fields vipc name=vipc "title=V" version=1 domain=vipc.demo.data-ui.com repl=NIL
    expect_ok env MODEL_FILE="$vip" run_cli deploy vipc
    local ctxfile listing
    ctxfile=$(ls "$STUB"/build-context-* | tail -1)
    listing=$(cat "$ctxfile")
    assert_contains "$listing" "./models/local/vipc.lisp" "handoff in context"
    assert_not_contains "$listing" "./models/vipc.lisp" "no canonical file"
    assert_eq "$(grep -c '^\./models/local/' <<<"$listing")" 1 "exactly one models/local/ path"
    # the compile consumed the candidate itself (runtime identity)
    local envfile
    envfile=$(grep -l 'DATA_UI_MODEL_PATH=models/local/deploy-incoming/' "$STUB"/ros-env-* | tail -1)
    [[ -n "$envfile" ]] || fail "compile did not read the handoff candidate"
    # the context copy is byte-identical to the handoff — hashed by the
    # stub AT BUILD TIME (the deploy's trap removes the context before
    # this test can see it)
    assert_contains "$listing" "$sha256  models/local/vipc.lisp" \
        "context handoff hash"
}

test_buildcontext_vip_collision_refused_before_compile() {
    default_fields
    # canonical models/vipcq.lisp exists upstream
    cp "$REPO_ROOT/models/todos.lisp" "$H/seed/models/vipcq.lisp"
    git -C "$H/seed" add -A
    git -C "$H/seed" -c user.email=t@t -c user.name=t commit -qm "canonical vipcq"
    git -C "$H/seed" push -q "$H/public.git" HEAD:refs/heads/master
    local vip; vip=$(make_vip_model_file vipcq)
    set_fields vipcq name=vipcq "title=V" version=1 domain=vipcq.demo.data-ui.com repl=NIL
    : > "$STUB/ros.log"
    expect_die "exists in public/master" env MODEL_FILE="$vip" run_cli deploy vipcq
    assert_eq "$(compile_calls)" 0 "no compilation before the collision refusal"
}

test_buildcontext_adoption_sequence() {
    default_fields
    local vip; vip=$(make_vip_model_file vipa)
    set_fields vipa name=vipa "title=V" version=1 domain=vipa.demo.data-ui.com repl=NIL
    expect_ok env MODEL_FILE="$vip" run_cli deploy vipa
    assert_file "$(stable_copy vipa)" "1: VIP deployed, stable copy present"
    # upstream gains the canonical file
    cp "$REPO_ROOT/models/todos.lisp" "$H/seed/models/vipa.lisp"
    git -C "$H/seed" add -A
    git -C "$H/seed" -c user.email=t@t -c user.name=t commit -qm "canonical vipa"
    git -C "$H/seed" push -q "$H/public.git" HEAD:refs/heads/master
    expect_ok run_cli deploy vipa
    assert_no_file "$(stable_copy vipa)" "3: canonical deploy removed the stale stable copy"
    make_ns_fixture vipa
    expect_ok env FORCE=1 run_cli delete vipa
    # 5: a VIP re-deploy of that name is refused, naming the canonical file
    expect_die "models/vipa.lisp exists in public/master" env MODEL_FILE="$vip" run_cli deploy vipa
}

test_buildcontext_reexec_preserves_worker_env() {
    default_fields
    local vip; vip=$(make_vip_model_file vipe)
    set_fields vipe name=vipe "title=V" version=1 domain=vipe.demo.data-ui.com repl=NIL
    expect_ok env MODEL_FILE="$vip" DOCKER_CONFIG=/worker/docker/cfg run_cli deploy vipe
    local envfile
    envfile=$(grep -l 'DATA_UI_MODEL_PATH=models/local/deploy-incoming/' "$STUB"/ros-env-* | tail -1)
    assert_contains "$(cat "$envfile")" "DOCKER_CONFIG=/worker/docker/cfg" \
        "worker environment survived the re-exec"
}

test_datachannel_no_interpolated_evals() {
    local script="$H/devclone/scripts/data-ui"
    assert_not_contains "$(grep -n 'top-level-model-field' "$script")" '"\${' "no spliced field name"
    assert_not_contains "$(grep -n 'read-model-text (u:slurp' "$script")" '${model_file}' "no spliced model path"
    assert_contains "$(grep -n 'read-model-text' "$script" | grep -v '^\s*#')" 'read-model-text' \
        "compile path still reads through read-model-text"
    # nasty MODEL_FILE path: nothing injected anywhere
    local nasty="$H/work/weird'name (paren) \\ back.lisp"
    printf "'(:name vipn :types ())\n" > "$nasty"
    set_fields vipn name=vipn "title=V" version=1 domain=vipn.demo.data-ui.com repl=NIL
    expect_ok env MODEL_FILE="$nasty" run_cli deploy vipn
    assert_not_contains "$(cat "$STUB/ros.log")" "weird'name" "nasty name never reached an eval argv"
}

# ---------------------------------------------------------------------------
# Recovery records and leftovers
# ---------------------------------------------------------------------------

test_recovery_post_rollout_failure_writes_record() {
    default_fields
    expect_ok run_cli deploy todos
    # a VIP handoff deploy whose public gate fails (post-manifests):
    # the recovery record is the CANDIDATE (a canonical deploy has no
    # candidate to preserve — its model lives in git at the SHA)
    local vip; vip=$(make_vip_model_file vipr2)
    set_fields vipr2 name=vipr2 "title=V" version=1 domain=vipr2.demo.data-ui.com repl=NIL
    printf '%s\n' "127.0.0.1 200" "vipr2.demo.data-ui.com/health 503" > "$STUB/curl-responses"
    local out st
    out=$(run_cmdline env CLI_HEALTH_TIMEOUT=4 MODEL_FILE="$vip" run_cli deploy vipr2 2>&1); st=$?
    rm -f "$STUB/curl-responses"
    (( st != 0 )) || fail "expected the public gate to fail the deploy"
    assert_contains "$out" "did not resolve or route"
    local rec
    rec=$(ls "$H"/state/deploy/vipr2/recovery/*.lisp 2>/dev/null | head -1)
    [[ -n "$rec" ]] || fail "no recovery record written"
    assert_eq "$(stat -c %a "$rec")" 600 "recovery record mode 600"
    assert_contains "$out" "MODEL_FILE='${rec}'"
    assert_contains "$out" "scripts/data-ui delete 'vipr2'"
}

test_hop_probe_polls_past_transient_refusal() {
    default_fields
    expect_ok run_cli deploy todos
    # A service can be briefly unreachable in the moments after its
    # rollout completes; the hop probe must poll past it (5s cadence,
    # DEPLOY_HEALTH_TIMEOUT deadline) and the deploy must finish. The
    # live VIP deploy of 2026-11-06 died exactly here when the probe
    # was single-shot.
    local name=hopp1
    local vip; vip=$(make_vip_model_file "$name")
    set_fields "$name" name="$name" "title=H" version=1 \
        domain=hopp1.demo.data-ui.com repl=NIL
    # Deterministic port: ports.lock is the cache assign_node_port
    # reads first; 30377 sits far above anything suite deploys assign.
    echo "$name demo 30377" >> "$H/state/deploy/ports.lock"
    echo 1 > "$STUB/curl-refuse-31377"    # one refused attempt, then reachable
    local out st
    out=$(run_cmdline env MODEL_FILE="$vip" run_cli deploy "$name" 2>&1); st=$?
    assert_eq "$(cat "$STUB/curl-refuse-31377")" 0 \
        "the refusal was consumed (the probe retried)"
    rm -f "$STUB/curl-refuse-31377"
    (( st == 0 )) || fail "a transient hop refusal must not fail the deploy: $out"
    assert_contains "$out" "Reusing NodePort 30377"
    assert_contains "$out" "Loopback hop 127.0.0.1:31377 (nodePort 30377) answers."
    assert_contains "$out" "NodePort /health gate passed"
    assert_file "$(stable_copy "$name")" "VIP promotion completed"
}

test_hop_probe_unreachable_band_fails_closed() {
    default_fields
    expect_ok run_cli deploy todos
    # A genuinely unpublished -p band: every attempt refused; the probe
    # polls to its deadline, then dies fail-closed into the recovery
    # path (no HAProxy update -- the route is never added).
    local name=hopp2
    local vip; vip=$(make_vip_model_file "$name")
    set_fields "$name" name="$name" "title=H" version=1 \
        domain=hopp2.demo.data-ui.com repl=NIL
    echo "$name demo 30378" >> "$H/state/deploy/ports.lock"
    echo 99 > "$STUB/curl-refuse-31378"
    local out st
    out=$(run_cmdline env CLI_HEALTH_TIMEOUT=3 MODEL_FILE="$vip" run_cli deploy "$name" 2>&1); st=$?
    rm -f "$STUB/curl-refuse-31378"
    (( st != 0 )) || fail "an unreachable hop must fail the deploy closed"
    assert_contains "$out" "did not answer within 3s"
    assert_contains "$out" "refusing to keep a bridge-IP target"
    assert_not_contains "$out" "Updating HAProxy"
    local rec
    rec=$(ls "$H"/state/deploy/$name/recovery/*.lisp 2>/dev/null | head -1)
    [[ -n "$rec" ]] || fail "no recovery record written"
}

test_recovery_record_drives_delete_and_route_removal() {
    default_fields
    expect_ok run_cli deploy todos
    make_ns_fixture todos
    # the route exists (map line from the deploy)
    assert_contains "$(cat "$H/haproxy/data-ui.map")" "todos.demo.data-ui.com"
    # simulate a failed :domain change whose identity lives only in a
    # recovery record
    local rec="$H/state/deploy/todos/recovery/manual.lisp"
    mkdir -p "$(dirname "$rec")"
    cp "$(stable_copy todos)" "$rec" 2>/dev/null || cp "$REPO_ROOT/models/todos.lisp" "$rec"
    expect_ok env FORCE=1 MODEL_FILE="$rec" run_cli delete todos
    assert_not_contains "$(cat "$H/haproxy/data-ui.map" 2>/dev/null)" "todos.demo.data-ui.com" \
        "the new route was removed"
}

test_leftovers_move_to_orphaned_candidates() {
    default_fields
    # production must exist first (a leftover lives in a bootstrapped
    # clone from a previous deploy; a nonempty non-repo checkout is
    # correctly refused untouched)
    expect_ok run_cli deploy todos
    mkdir -p "$(incoming_dir)"
    echo "stale bytes" > "$(incoming_dir)/tok12345.lisp"
    expect_ok run_cli deploy todos
    local moved
    moved=$(ls "$H"/state/deploy/orphaned-candidates/*-tok12345.lisp 2>/dev/null | head -1)
    [[ -n "$moved" ]] || fail "leftover candidate not moved"
    assert_contains "$(cat "$moved")" "stale bytes" "bytes intact"
    assert_eq "$(ls -A "$(incoming_dir)" | grep -v "$(basename "$(ls "$H"/state/deploy/orphaned-candidates/*-tok12345.lisp)")" | grep -c 'tok12345' || true)" 0 \
        "deploy-incoming no longer holds the leftover"
}

test_remote_transfer_never_writes_production_before_clean_check() {
    default_fields
    CLI_DEPLOY_HOST="$REAL_HOST" expect_ok run_cli deploy todos
    # dirty the production clone, then a remote deploy with MODEL_FILE:
    # selection aborts BEFORE any candidate install.
    echo dirt >> "$H/repos/production/README.md"
    local vip; vip=$(make_vip_model_file vipr)
    set_fields vipr name=vipr "title=V" version=1 domain=vipr.demo.data-ui.com repl=NIL
    arm_remote
    expect_die "not clean" env MODEL_FILE="$vip" run_cli deploy vipr
    assert_eq "$(ls -A "$(incoming_dir)" 2>/dev/null | wc -l)" 0 \
        "nothing written to production before the clean check"
    # host temp dirs were cleaned
    local leftovers
    leftovers=$(ls -d "$H"/work/data-ui-remote-handoff.* 2>/dev/null | wc -l)
    assert_eq "$leftovers" 0 "remote temp cleaned"
}

# ---------------------------------------------------------------------------
# Delete
# ---------------------------------------------------------------------------

test_delete_no_fetch_no_clean_required() {
    default_fields
    git -C "$H/seed" checkout -q -b newer; echo n >> "$H/seed/README.md"
    git -C "$H/seed" add -A; git -C "$H/seed" -c user.email=t@t -c user.name=t commit -qm n
    git -C "$H/seed" push -q "$H/public.git" HEAD:refs/heads/master
    git -C "$H/seed" checkout -q master
    expect_ok run_cli deploy todos
    local sha; sha=$(prod_sha)
    make_ns_fixture todos
    echo dirt >> "$H/repos/production/README.md"   # dirty must not block delete
    expect_ok env FORCE=1 run_cli delete todos
    assert_eq "$(prod_sha)" "$sha" "delete never advances source"
}

test_delete_cds_before_identity_from_any_cwd() {
    default_fields
    expect_ok run_cli deploy todos
    make_ns_fixture todos
    CLI_CWD=/ expect_ok env FORCE=1 run_cli delete todos
}

test_delete_explicit_model_file_via_its_own_envelope() {
    default_fields
    expect_ok run_cli deploy todos
    make_ns_fixture todos
    # identity only from an explicit archived file (the identity reader
    # asks the ros stub, which keys answers off the FILE's basename)
    local vip="$H/work/only-copy.lisp"
    sed 's/:name todos/:name todos/' "$REPO_ROOT/models/todos.lisp" > "$vip"
    set_fields only-copy name=todos "title=T" version=1 domain=todos.demo.data-ui.com repl=NIL
    expect_ok env FORCE=1 MODEL_FILE="$vip" run_cli delete todos
    # The identity read crosses to ros as environment (constant eval
    # text), so prove it happened via the captured env: a field read
    # of :name under MODEL_NAME=todos.
    local ef hit=""
    for ef in "$STUB"/ros-env-*; do
        [[ -f "$ef" ]] || continue
        if grep -q '^DATA_UI_FIELD_NAME=name$' "$ef" 2>/dev/null \
           && grep -q '^MODEL_NAME=todos$' "$ef" 2>/dev/null; then
            hit=1
        fi
    done
    [[ -n "$hit" ]] || fail "identity was read"
}

test_delete_validates_identity_before_snapshot_or_confirmation() {
    default_fields
    expect_ok run_cli deploy todos
    set_fields todos name=other "title=T" version=1 domain=d.demo.data-ui.com repl=NIL
    make_ns_fixture todos
    : > "$STUB/psql.log"
    expect_die "must match" env FORCE=1 run_cli delete todos
    assert_eq "$(stub_count 'pg_dump' "$STUB/psql.log")" 0 \
        "no farewell snapshot before identity validation"
}

test_delete_legacy_identity_migration_then_delete() {
    default_fields
    # a legacy VIP identity living only in the development checkout
    local legacy="$H/devclone/models/local/legacyapp.lisp"
    mkdir -p "$(dirname "$legacy")"
    cp "$REPO_ROOT/models/todos.lisp" "$legacy"
    sed -i 's/:name todos/:name legacyapp/' "$legacy"
    set_fields legacyapp name=legacyapp "title=L" version=1 domain=legacyapp.demo.data-ui.com repl=NIL
    expect_ok run_cli deploy-bootstrap
    assert_file "$H/repos/production/models/local/legacyapp.lisp" "migrated"
    make_ns_fixture legacyapp
    expect_ok env FORCE=1 run_cli delete legacyapp
}

# ---------------------------------------------------------------------------
# Drain query (real PostgreSQL)
# ---------------------------------------------------------------------------

test_drain_query_real_postgres() {
    if [[ "${SKIP_DOCKER:-}" == "1" ]] || ! /usr/bin/docker info >/dev/null 2>&1; then
        skip_test "docker unavailable; drain query test skipped"
        return 0
    fi
    local port=5599 cname="dui-drain-$$"
    /usr/bin/docker rm -f "$cname" >/dev/null 2>&1 || true
    /usr/bin/docker run -d --rm --name "$cname" -p "127.0.0.1:${port}:5432" \
        -e POSTGRES_PASSWORD=drainpw postgres:16 >/dev/null || {
        skip_test "could not start throwaway postgres"; return 0; }
    local i
    # Readiness must probe TCP, not the default socket: the postgres
    # image's initdb runs a TEMPORARY server (socket-only) that answers
    # socket probes and then shuts down — a socket-ready loop breaks
    # early and the setup queries land in the shutdown window (failed
    # twice on 2026-11-20). The real server is the TCP listener.
    for i in $(seq 1 30); do
        /usr/bin/docker exec "$cname" pg_isready -h 127.0.0.1 -U postgres >/dev/null 2>&1 && break
        sleep 1
    done
    /usr/bin/docker exec "$cname" pg_isready -h 127.0.0.1 -U postgres >/dev/null 2>&1 \
        || { /usr/bin/docker rm -f "$cname" >/dev/null 2>&1 || true
             fail "postgres never reached TCP readiness in 30s"; return 0; }
    /usr/bin/docker exec "$cname" psql -U postgres -c \
        "create table rt_models (model_name text, model_deploy_status text, model_generate_status text)" >/dev/null
    /usr/bin/docker exec "$cname" psql -U postgres -c \
        "insert into rt_models values ('m1','running','complete'), ('m2','complete','running'), ('m3','complete','complete')" >/dev/null
    make_profile modelbank "$((port - 1000))" demo
    sed -i "s/^DB_PORT=.*/DB_PORT=$port/; s/^DB_NAME=.*/DB_NAME=postgres/; s/^DB_USER=.*/DB_USER=postgres/; s/^DB_PASSWORD=.*/DB_PASSWORD=drainpw/" \
        "$H/state/profiles/modelbank/profile.env"
    make_staging_clone
    # the drain query must reach the REAL postgres, not the stub
    : > "$STUB/psql-pass-through"
    local out
    out=$(run_staging_cli staging modelbank-running 2>&1) || fail "drain query failed: $out"
    assert_contains "$out" "m1" "running deploy row returned"
    assert_contains "$out" "m2" "running generate row returned"
    assert_not_contains "$out" "m3" "completed rows not returned"
    # a query error is a failed drain, never empty
    /usr/bin/docker exec "$cname" psql -U postgres -c "drop table rt_models" >/dev/null
    local out2 st2
    out2=$(run_staging_cli staging modelbank-running 2>&1); st2=$?
    (( st2 != 0 )) || fail "dropped table must fail the drain (never read as empty)"
    rm -f "$STUB/psql-pass-through"
    /usr/bin/docker rm -f "$cname" >/dev/null 2>&1 || true
}

# ---------------------------------------------------------------------------
# Routing
# ---------------------------------------------------------------------------

test_routing_preserved_and_local_defaults() {
    make_staging_clone
    # routed verb runs the STAGING script
    local out
    out=$(run_cli profile list 2>&1) || fail "profile list failed: $out"
    assert_contains "$out" "Routing to the staging checkout"
    # an absent clone prints the instruction
    rm -rf "$H/repos/staging"
    expect_die "run 'scripts/data-ui staging update' first" run_cli profile list
    # non-routed verbs from / and /tmp enter their own root
    make_staging_clone
    CLI_CWD=/ expect_ok run_cli field todos name
    CLI_CWD=/tmp expect_ok run_cli field todos name
    # dev: and prd: stay local; stg: routes
    mkdir -p "$H/state/snapshots"
    : > "$H/state/snapshots/devish.dump"
    printf 'model=dev:todos\nsource-address=dev:todos\n' > "$H/state/snapshots/devish.meta"
    # stg: routing to staging (no profile yet → dies there, but AFTER routing)
    local out2
    out2=$(run_cli snapshot save name1 stg:books 2>&1); st=$?
    assert_contains "$out2" "Routing to the staging checkout"
    (( st != 0 )) || fail "save stg: without a profile must fail inside staging"
}

test_snapshot_copy_triple_and_retarget() {
    # snapshot copy: pool-only, no database. Covers plain copy,
    # model retarget, missing source, existing destination, FORCE=1,
    # invalid names, and a meta-less source; the source is never
    # modified.
    local P="$H/state/snapshots" before
    mkdir -p "$P"
    echo dump-golden > "$P/stg-todos-golden.dump"
    echo tar-golden > "$P/stg-todos-golden.files.tar.gz"
    printf 'model=todos\nsource-address=stg:todos\ntimestamp=2026-11-01T04:10:00Z\nmodel-version=3\n' \
        > "$P/stg-todos-golden.meta"
    before=$(cd "$P" && sha256sum stg-todos-golden.*)
    # plain copy: triple duplicated, model kept, copied-from added
    expect_ok run_cli snapshot copy stg-todos-golden plain-copy
    assert_eq "$(cat "$P/plain-copy.dump")" "dump-golden"
    assert_eq "$(cat "$P/plain-copy.files.tar.gz")" "tar-golden"
    assert_eq "$(cat "$P/plain-copy.meta")" "$(printf 'model=todos\nsource-address=stg:todos\ntimestamp=2026-11-01T04:10:00Z\nmodel-version=3\ncopied-from=stg-todos-golden')"
    # retarget: model replaced, other keys kept, provenance recorded
    expect_ok run_cli snapshot copy stg-todos-golden hannah-todos-seed todos-hannah
    assert_eq "$(cat "$P/hannah-todos-seed.meta")" "$(printf 'source-address=stg:todos\ntimestamp=2026-11-01T04:10:00Z\nmodel-version=3\nmodel=todos-hannah\ncopied-from=stg-todos-golden\ncopied-model-from=todos')"
    # a copy of a copy replaces the copy keys instead of stacking them
    expect_ok run_cli snapshot copy hannah-todos-seed second-hop todos2
    assert_eq "$(grep -c '^copied-from=' "$P/second-hop.meta")" "1"
    assert_contains "$(cat "$P/second-hop.meta")" "copied-from=hannah-todos-seed"
    assert_contains "$(cat "$P/second-hop.meta")" "copied-model-from=todos-hannah"
    # missing source
    expect_die "snapshot 'nope' not found" run_cli snapshot copy nope x1
    assert_no_file "$P/x1.dump"
    # existing destination refused, untouched
    echo old > "$P/taken.dump"
    expect_die "already exists; FORCE=1" run_cli snapshot copy stg-todos-golden taken
    assert_eq "$(cat "$P/taken.dump")" "old"
    # a stray .meta alone also counts as existing
    echo model=x > "$P/half.meta"
    expect_die "already exists" run_cli snapshot copy stg-todos-golden half
    # FORCE=1 overwrites
    expect_ok env FORCE=1 run_cli snapshot copy stg-todos-golden taken
    assert_eq "$(cat "$P/taken.dump")" "dump-golden"
    assert_file "$P/taken.meta"
    # invalid names (both slots, and the model)
    expect_die "Invalid snapshot name 'stg:todos'" run_cli snapshot copy stg:todos x2
    expect_die "Invalid snapshot name '-bad'" run_cli snapshot copy stg-todos-golden -bad
    expect_die "Invalid model name 'a:b'" run_cli snapshot copy stg-todos-golden x3 a:b
    expect_die "same name" run_cli snapshot copy stg-todos-golden stg-todos-golden
    expect_die "needs a source and a destination" run_cli snapshot copy stg-todos-golden
    expect_die "two or three arguments" run_cli snapshot copy a b c d
    assert_no_file "$P/x2.dump"; assert_no_file "$P/x3.dump"
    # meta-less source: plain copy stays meta-less; retarget stubs one
    echo dump-bare > "$P/bare.dump"
    expect_ok run_cli snapshot copy bare bare-copy
    assert_file "$P/bare-copy.dump"
    assert_no_file "$P/bare-copy.meta"
    assert_no_file "$P/bare-copy.files.tar.gz"
    expect_ok run_cli snapshot copy bare bare-retarget todos-hannah
    assert_eq "$(cat "$P/bare-retarget.meta")" "$(printf 'model=todos-hannah\ncopied-from=bare')"
    # FORCE over a destination with a tar the source lacks drops it
    expect_ok env FORCE=1 run_cli snapshot copy bare taken
    assert_no_file "$P/taken.files.tar.gz"
    assert_no_file "$P/taken.meta"
    # no temp files left behind; source untouched
    assert_eq "$(ls -A "$P" | grep -c '^\.tmp-')" "0"
    assert_eq "$(cd "$P" && sha256sum stg-todos-golden.*)" "$before"
}

test_routing_inner_arms_enter_staging_first() {
    make_staging_clone
    # a model present ONLY in staging resolves for routed profile create
    cp "$REPO_ROOT/models/todos.lisp" "$H/repos/staging/models/stgonly.lisp"
    git -C "$H/repos/staging" add -A
    git -C "$H/repos/staging" -c user.email=t@t -c user.name=t commit -qm stgonly
    set_fields stgonly name=stgonly "title=S" version=1 domain=stgonly.demo.data-ui.com repl=NIL
    expect_ok run_cli profile create stgonly
    assert_dir "$H/state/profiles/stgonly"
}

test_routing_named_repl_db_psql_run_no_npm() {
    make_staging_clone
    mkdir -p "$H/repos/staging/web/dist"
    make_profile books 8123 demo
    : > "$STUB/npm.log"
    local out
    out=$(run_cli psql books <<< '\q' 2>&1) || true
    assert_not_contains "$(cat "$STUB/npm.log")" "install" "psql PROFILE runs no npm"
    assert_not_contains "$(cat "$STUB/npm.log")" "run build"
}

test_routing_active_profile_repl_refusal_mutates_nothing() {
    make_staging_clone
    make_profile books 8123 demo
    touch "$STUB/ss/8123"      # the app port is listening
    local out st
    out=$(run_cli repl books 2>&1); st=$?
    (( st != 0 )) || fail "repl against an active profile must refuse"
    assert_contains "$out" "already in use"
}

test_routing_wrong_hosts_fail_before_mutation() {
    CLI_STAGING_HOST="otherhost" expect_die "must name the same host" run_cli staging update
    assert_no_file "$H/repos/staging.lock" "no lockfile created"
    assert_no_dir "$H/repos/staging" "no clone made"
}

# ---------------------------------------------------------------------------
# Quiescence (via staging update's barrier)
# ---------------------------------------------------------------------------

quiesce_fixture() {
    make_profile modelbank 8300 demo
    make_staging_clone
    # The quiesce pre-check requires an active or inactive unit; the
    # systemctl stub answers 'unknown' for units with no state file.
    sd_set_unit dataui@modelbank enabled active
    : > "$STUB/psql.log"
}

test_quiesce_failed_reload_aborts() {
    quiesce_fixture
    # modelbank must be EXPOSED for the unexpose reload (and its
    # validation) to run at all
    echo "modelbank-stg.demo.data-ui.com dataui-profile-modelbank" >> "$H/haproxy/data-ui.map"
    local before
    before=$(git -C "$H/repos/staging" rev-parse HEAD)
    echo ok > "$STUB/haproxy-validate-fails"
    local out st
    out=$(run_cli staging update 2>&1); st=$?
    rm -f "$STUB/haproxy-validate-fails"
    (( st != 0 )) || fail "failed HAProxy validation must abort the update"
    assert_eq "$(git -C "$H/repos/staging" rev-parse HEAD)" "$before" \
        "no checkout mutation"
}

test_quiesce_absent_profile_and_stopped_unit() {
    make_staging_clone
    # absent profile: update proceeds past quiescence
    local out
    out=$(run_cli staging update 2>&1) || fail "update failed: $out"
    assert_contains "$out" "No modelbank profile"
    # stopped unit proceeds too
    rm -rf "$H/repos/staging"
    make_profile modelbank 8301 demo
    sd_set_unit dataui@modelbank disabled inactive
    out=$(run_cli staging update 2>&1) || fail "update with stopped modelbank failed: $out"
    assert_contains "$out" "dataui@modelbank is inactive"
}

test_quiesce_unexposed_profile_skips_reload() {
    quiesce_fixture
    local out
    out=$(run_cli staging update 2>&1) || fail "update failed: $out"
    assert_contains "$out" "Profile 'modelbank' is not exposed" "early not-exposed path"
    assert_not_contains "$(cat "$STUB/haproxy.log" 2>/dev/null)" "-c" "no validation/reload needed"
}

test_quiesce_state_ordering() {
    quiesce_fixture
    # make the fetch fail: quiesced + cleared SHAs must ALREADY be on disk
    git -C "$H/private.git" update-ref -d refs/heads/master
    expect_die "failed" run_cli staging update
    local st="$H/state/profiles/modelbank/activation.state"
    assert_file "$st"
    assert_contains "$(cat "$st")" "phase=quiesced"
    assert_contains "$(cat "$st")" "target-staging-sha=-"
    assert_contains "$(cat "$st")" "activated-staging-sha=-"
}

test_quiesce_target_sha_written_after_fetch() {
    quiesce_fixture
    local out
    out=$(run_cli staging update 2>&1) || fail "update failed: $out"
    local st="$H/state/profiles/modelbank/activation.state"
    assert_file "$st" "state file exists after a successful update"
    assert_contains "$(cat "$st")" "target-staging-sha=$(private_master_sha)"
}

test_quiesce_drain_blocked_worker_completes() {
    quiesce_fixture
    # A queued worker holds the PRODUCTION lock; the drain (which must
    # not take it) waits until the row clears, then the update proceeds.
    bash -c "exec {f}<>$H/repos/production.lock; flock -x \$f; sleep 3; rm -f '$STUB/psql-rows'; sleep 1" &
    local holder=$!
    sleep 0.2
    printf 'modelA\n' > "$STUB/psql-rows"
    local out st
    out=$(CLI_DRAIN_TIMEOUT=25 run_cli staging update 2>&1); st=$?
    kill "$holder" 2>/dev/null || true; wait "$holder" 2>/dev/null || true
    (( st == 0 )) || fail "update blocked on a lock-holding worker must proceed after it finishes: $out"
    assert_contains "$out" "waiting on running rows"
}

test_quiesce_drain_timeout_stranded_row() {
    quiesce_fixture
    local before
    before=$(git -C "$H/repos/staging" rev-parse HEAD)
    printf 'stranded-model\n' > "$STUB/psql-rows"   # never clears
    local out st
    out=$(CLI_DRAIN_TIMEOUT=2 run_cli staging update 2>&1); st=$?
    rm -f "$STUB/psql-rows"
    (( st != 0 )) || fail "stranded row must abort the update"
    assert_contains "$out" "stranded-model" "every model_name reported"
    assert_eq "$(git -C "$H/repos/staging" rev-parse HEAD)" "$before" \
        "no checkout mutation"
}

test_quiesce_generate_in_flight() {
    quiesce_fixture
    printf 'gen-model\n' > "$STUB/psql-rows"
    local out st
    out=$(CLI_DRAIN_TIMEOUT=2 run_cli staging update 2>&1); st=$?
    rm -f "$STUB/psql-rows"
    (( st != 0 )) || fail "a Generate in flight must abort the update"
    assert_contains "$out" "timed out"
}

# ---------------------------------------------------------------------------
# Activation
# ---------------------------------------------------------------------------

activation_state() { echo "$H/state/profiles/modelbank/activation.state"; }

test_activation_edemo_refusal() {
    make_profile modelbank 8400 e-demo
    make_staging_clone
    expect_die "DEMO_CLASS=e-demo" run_cli staging activate-modelbank
}

test_activation_extra_options_fail() {
    make_profile modelbank 8400 demo
    make_staging_clone
    expect_die "at most one option" run_cli staging activate-modelbank --expose --leave-unexposed
    expect_die "Unknown option" run_cli staging activate-modelbank --bogus
}

test_activation_originally_unexposed_never_exposed() {
    make_profile modelbank 8401 demo
    make_staging_clone
    echo "original-exposed=0
phase=quiesced
target-staging-sha=-
activated-staging-sha=-" > "$(activation_state)"
    start_swank_fake 11401 "$H/repos/staging/"
    sd_set_unit dataui@modelbank enabled active
    local out
    out=$(run_cli staging activate-modelbank --expose 2>&1) || fail "activation failed: $out"
    assert_contains "$out" "originally unexposed; a successful no-expose outcome"
    assert_not_contains "$(cat "$STUB/haproxy/data-ui.map" 2>/dev/null || true)" "modelbank"
    assert_no_file "$(activation_state)" "record removed after a verified no-expose activation"
    cleanup_swank_fakes
}

test_activation_no_exposure_after_failed_health() {
    make_profile modelbank 8402 demo
    make_staging_clone
    sd_set_unit dataui@modelbank enabled active
    echo "original-exposed=1
phase=quiesced
target-staging-sha=-
activated-staging-sha=-" > "$(activation_state)"
    # health fails: no swank fake, and /health fails
    printf '%s\n' "127.0.0.1:8402/health 503" > "$STUB/curl-responses"
    local out st
    out=$(CLI_HEALTH_TIMEOUT=3 run_cli staging activate-modelbank --expose 2>&1); st=$?
    rm -f "$STUB/curl-responses"
    (( st != 0 )) || fail "failed health must fail the activation closed"
    assert_not_contains "$(cat "$STUB/haproxy/data-ui.map" 2>/dev/null || true)" "modelbank" \
        "no exposure after a failed health check"
    assert_contains "$out" "did not verify"
}

test_activation_protocol_mismatch_blocks_expose() {
    make_profile modelbank 8403 demo
    make_staging_clone
    # Bootstrap production FIRST from a clean public/master, then plant
    # an old protocol line ON public/master: the readiness check does a
    # refs-only fetch of public/master and must see the mismatch. (The
    # plant must come after the deploy — deploying from the poisoned
    # source would fail at the production script's own marker check.)
    default_fields
    CLI_DEPLOY_HOST="$REAL_HOST" expect_ok run_cli deploy todos
    git -C "$H/seed" checkout -q -b proto
    mkdir -p "$H/seed/scripts/lib"
    printf 'DATA_UI_REEXEC_PROTOCOL=99\n' > "$H/seed/scripts/lib/checkout.sh"
    git -C "$H/seed" add -A
    git -C "$H/seed" -c user.email=t@t -c user.name=t commit -qm "old protocol"
    git -C "$H/seed" push -q "$H/public.git" +HEAD:refs/heads/master
    git -C "$H/seed" checkout -q master
    echo "original-exposed=1
phase=quiesced
target-staging-sha=-
activated-staging-sha=-" > "$(activation_state)"
    start_swank_fake 11403 "$H/repos/staging/"
    sd_set_unit dataui@modelbank enabled active
    local out st
    out=$(run_cli staging activate-modelbank --expose 2>&1); st=$?
    cleanup_swank_fakes
    (( st != 0 )) || fail "protocol mismatch must block --expose"
    assert_contains "$out" "protocol mismatch"
    assert_not_contains "$(cat "$STUB/haproxy/data-ui.map" 2>/dev/null || true)" "modelbank"
}

test_activation_full_cycle_and_fast_path() {
    make_profile modelbank 8404 demo
    make_staging_clone
    # production must exist: the --expose readiness check fetches
    # public/master through it
    default_fields
    CLI_DEPLOY_HOST="$REAL_HOST" expect_ok run_cli deploy todos
    # a model in staging so expose can read its domain: the field read
    # goes through the ros stub (set_fields below) — planting a real
    # models/modelbank.lisp would leave an UNTRACKED file in the
    # staging clone and abort every later update's clean check
    set_fields modelbank name=modelbank "title=MB" version=1 domain=modelbank.demo.data-ui.com repl=NIL
    echo "original-exposed=1
phase=quiesced
target-staging-sha=-
activated-staging-sha=-" > "$(activation_state)"
    start_swank_fake 11404 "$H/repos/staging/"
    sd_set_unit dataui@modelbank enabled active
    local out
    out=$(run_cli staging activate-modelbank --leave-unexposed 2>&1) \
        || fail "activation failed: $out"
    assert_contains "$out" "phase=activated"
    assert_contains "$(cat "$(activation_state)")" "original-exposed=1"
    # --expose uses the fast path (no second restart)
    local restarts_before
    restarts_before=$(grep_systemctl "restart dataui@modelbank" | wc -l)
    out=$(run_cli staging activate-modelbank --expose 2>&1) \
        || fail "expose fast path failed: $out"
    assert_contains "$out" "fast path"
    assert_eq "$(grep_systemctl "restart dataui@modelbank" | wc -l)" "$restarts_before" \
        "never restarts a healthy current Model Bank just to expose"
    assert_contains "$(cat "$H/haproxy/data-ui.map")" "modelbank"
    assert_no_file "$(activation_state)" "record removed after verified final exposure"
    # a later update invalidates the fast path: state reappears quiesced
    rm -f "$H/haproxy/data-ui.map"; : > "$H/haproxy/data-ui.map"
    out=$(run_cli staging update 2>&1) || fail "later update failed: $out"
    assert_file "$(activation_state)" "update re-quiesces the record"
    assert_contains "$(cat "$(activation_state)")" "phase=quiesced"
    cleanup_swank_fakes
}

test_activation_staging_then_production_order() {
    make_profile modelbank 8405 demo
    make_staging_clone
    echo "original-exposed=0
phase=quiesced
target-staging-sha=-
activated-staging-sha=-" > "$(activation_state)"
    # someone else holds PRODUCTION: activation must WAIT for it (staging
    # lock acquired first, production second — never deadlock).
    bash -c "exec {f}<>$H/repos/production.lock; flock -x \$f; sleep 2" &
    local holder=$!
    sleep 0.2
    start_swank_fake 11405 "$H/repos/staging/"
    sd_set_unit dataui@modelbank enabled active
    local out
    out=$(run_cli staging activate-modelbank 2>&1) || fail "activation failed: $out"
    kill "$holder" 2>/dev/null || true; wait "$holder" 2>/dev/null || true
    cleanup_swank_fakes
}

# ---------------------------------------------------------------------------
# Staging update: build, ops, restarts, timer
# ---------------------------------------------------------------------------

test_update_full_flow_default_private() {
    make_profile books 8500 demo
    sd_set_unit dataui@books enabled active
    local out
    out=$(run_cli staging update 2>&1) || fail "update failed: $out"
    assert_contains "$out" "remote/branch: private/master"
    assert_contains "$out" "Installed ops/profile-run.sh"
    assert_contains "$out" "daemon reloaded"
    assert_contains "$out" "journal identity + loopback /health OK"
    assert_contains "$out" "Model Bank:       pending activation"
    assert_file "$H/ops/profile-run.sh"
    assert_file "$H/systemd/dataui@.service"
    assert_contains "$(cat "$STUB/npm.log")" "npm ci (cwd=$H/repos/staging/web)"
    assert_contains "$(cat "$STUB/npm.log")" "npm run build"
    # timer restored to its captured state
    assert_contains "$out" "Timer restored: enabled=enabled active=active"
    # ops install precedes daemon-reload
    local first_install first_reload
    first_install=$(grep -n "install .*dataui@.service" "$STUB/sudo.log" | head -1 | cut -d: -f1)
    first_reload=$(grep -n "systemctl daemon-reload" "$STUB/sudo.log" | head -1 | cut -d: -f1)
    [[ -n "$first_install" && -n "$first_reload" && "$first_install" -lt "$first_reload" ]] \
        || fail "ops files must all be installed before daemon-reload"
}

test_update_explicit_source_and_generated_files_ignored() {
    make_staging_clone
    mkdir -p "$H/repos/staging/web/dist" "$H/repos/staging/web/node_modules/p"
    echo x > "$H/repos/staging/web/dist/app.js"
    echo x > "$H/repos/staging/web/node_modules/p/index.js"
    local out
    out=$(run_cli staging update --from private/master 2>&1) || fail "update failed: $out"
    assert_file "$H/repos/staging/web/dist/app.js" "ignored outputs survive"
}

test_update_lockfile_required() {
    make_staging_clone
    # the lockfile must be missing from the SOURCE revision — deleting
    # it from the working tree would only dirty the clone and die at
    # the clean check before the child's npm ci guard ever runs
    git -C "$H/repos/staging" rm -q web/package-lock.json
    git -C "$H/repos/staging" -c user.email=t@t -c user.name=t commit -qm "no lockfile"
    git -C "$H/repos/staging" push -q "$H/private.git" HEAD:refs/heads/master
    local out st
    out=$(run_cli staging update 2>&1); st=$?
    (( st != 0 )) || fail "missing lockfile must fail the update"
    assert_contains "$out" "package-lock.json"
    assert_eq "$(stub_count 'systemctl restart dataui@' "$STUB/systemctl.log")" 0 \
        "nothing restarted after a build failure"
}

test_update_two_consecutive_builds() {
    make_staging_clone
    run_cli staging update >/dev/null 2>&1 || fail "first update failed"
    local out
    out=$(run_cli staging update 2>&1) || fail "second update failed: $out"
    assert_contains "$out" "Staging update complete"
}

test_update_gate_stops_at_first_failure() {
    make_profile books 8501 demo
    make_profile chores 8502 demo
    sd_set_unit dataui@books enabled active
    sd_set_unit dataui@chores enabled active
    # books never logs its startup identity
    mkdir -p "$STUB/journal-silent"
    touch "$STUB/journal-silent/dataui@books.service"
    local out st
    out=$(CLI_HEALTH_TIMEOUT=4 run_cli staging update 2>&1); st=$?
    rm -rf "$STUB/journal-silent"
    (( st != 0 )) || fail "a failed gate must fail the update"
    assert_contains "$out" "dataui@books"
    assert_not_contains "$(grep_systemctl "restart dataui@chores" | head -1 || true)" "restart" \
        "no later profile restarted after a health failure"
    assert_contains "$out" "no second pass"
}

test_update_no_lifecycle_verbs() {
    make_profile books 8503 demo
    sd_set_unit dataui@books enabled active
    run_cli staging update >/dev/null 2>&1 || fail "update failed"
    local log; log=$(cat "$STUB/systemctl.log")
    assert_not_contains "$log" "enable --now dataui@"
    assert_not_contains "$log" "stop dataui@books"
    assert_not_contains "$log" "start dataui-reset.service"
    assert_not_contains "$log" "disable dataui@"
    assert_contains "$log" "restart dataui@books"
}

test_update_untouched_disabled_profiles() {
    make_profile books 8504 demo
    make_profile asleep 8505 demo
    sd_set_unit dataui@books enabled active
    sd_set_unit dataui@asleep disabled inactive
    run_cli staging update >/dev/null 2>&1 || fail "update failed"
    assert_not_contains "$(cat "$STUB/systemctl.log")" "restart dataui@asleep"
    assert_eq "$(sd_unit_state dataui@asleep)" "disabled inactive dead0000beef" "left as-is"
}

test_update_activating_profile_aborts() {
    make_profile books 8506 demo
    sd_set_unit dataui@books enabled activating
    expect_die "activating" run_cli staging update
}

# ---------------------------------------------------------------------------
# Pre-warm and health gates
# ---------------------------------------------------------------------------

test_prewarm_failure_installs_nothing() {
    make_staging_clone
    echo ok > "$STUB/ros-prewarm-fails"
    local out st
    out=$(run_cli staging update 2>&1); st=$?
    rm -f "$STUB/ros-prewarm-fails"
    (( st != 0 )) || fail "pre-warm failure must fail the update"
    assert_contains "$out" "pre-warm load failed"
    assert_eq "$(ls "$H/systemd" 2>/dev/null | wc -l)" 0 "no ops installed"
    assert_eq "$(stub_count 'restart dataui@' "$STUB/systemctl.log")" 0
}

test_prewarm_elapsed_printed() {
    make_staging_clone
    local out
    out=$(run_cli staging update 2>&1) || fail "update failed: $out"
    assert_contains "$out" "Pre-warm complete in"
}

test_healthgate_slow_unit_passes_within_deadline() {
    make_profile books 8507 demo
    sd_set_unit dataui@books enabled active
    # /health answers 200 only from the 2nd poll on
    cat > "$STUB/hooks/curl-8507" <<'EOF'
f="$STUB/curl-count-8507"
n=$(cat "$f" 2>/dev/null || echo 0); n=$((n+1)); echo "$n" > "$f"
if (( n < 2 )); then printf '503\n' > "$STUB/curl-override-8507"; else rm -f "$STUB/curl-override-8507"; fi
exit 0
EOF
    local out
    out=$(CLI_HEALTH_TIMEOUT=20 run_cli staging update 2>&1) || fail "slow unit should pass: $out"
    assert_contains "$out" "loopback /health OK"
    rm -f "$STUB/hooks/curl-8507"
}

test_healthgate_invocation_change_mid_poll_fails() {
    make_profile books 8508 demo
    sd_set_unit dataui@books enabled active
    # the unit crash-restarts on its first health poll
    cat > "$STUB/hooks/curl-8508" <<'EOF'
if [[ ! -f "$STUB/did-restart-8508" ]]; then
    touch "$STUB/did-restart-8508"
    bash -c '
        sleep 1
        source "'"$H"'/repos/staging/scripts/lib/checkout.sh" >/dev/null 2>&1 || true
        exit 0' >/dev/null 2>&1
    # bump the invocation id directly in the state file
    f="$STUB/systemd-units/dataui@books.service"
    read -r e a i < "$f"; echo "$e $a ff00ff00ff00" > "$f"
fi
exit 0
EOF
    chmod +x "$STUB/hooks/curl-8508"
    local out st
    out=$(CLI_HEALTH_TIMEOUT=6 run_cli staging update 2>&1); st=$?
    rm -f "$STUB/hooks/curl-8508" "$STUB/did-restart-8508"
    (( st != 0 )) || fail "a crash-restart during the poll must fail the gate"
    assert_contains "$out" "InvocationID of dataui@books changed"
}

test_journal_identity_line_matches_gate_contract() {
    # The staging health gate greps the fixed string
    # "profile-run: checkout: <checkout>"; ops/profile-run.sh must emit
    # exactly that. Cross-checks the real producer against the gate's
    # contract: during the 2026-11-05 cutover the echo carried a
    # [$PROFILE] segment, the fixed-string grep could never match, and
    # the gate read 0/0/0 against a healthy unit for the full deadline.
    local line emitted
    line=$(grep -m1 '^echo "profile-run' "$REPO_ROOT/ops/profile-run.sh")
    [[ -n "$line" ]] || fail "ops/profile-run.sh lost its journal-identity echo"
    emitted=$(PROFILE=books DATA_UI_CHECKOUT=/data/data-ui/repos/staging \
                  bash -c "$line")
    assert_contains "$emitted" "profile-run: checkout: /data/data-ui/repos/staging" \
        "the identity echo must match the gate's fixed-string grep (scripts/data-ui)"
    grep -qF '"profile-run: checkout: ' "$REPO_ROOT/scripts/data-ui" \
        || fail "the gate's journal-identity grep changed; update ops/profile-run.sh to match"
}

test_healthgate_pre_restart_lines_cannot_satisfy() {
    make_profile books 8509 demo
    sd_set_unit dataui@books enabled active
    # OLD journal lines with the right markers exist before the restart…
    sd_journal_append dataui@books.service \
        "profile-run: checkout: $H/repos/staging" \
        "DATA-UI-ASDF-SOURCE=$H/repos/staging/"
    # …but the restarted unit never logs anything new (silent template),
    # and /health stays down: only the CURRENT invocation's lines count.
    mkdir -p "$STUB/journal-silent"
    touch "$STUB/journal-silent/dataui@books.service"
    printf '%s\n' "127.0.0.1:8509/health 503" > "$STUB/curl-responses"
    local out st
    out=$(CLI_HEALTH_TIMEOUT=3 run_cli staging update 2>&1); st=$?
    rm -rf "$STUB/journal-silent" "$STUB/curl-responses"
    (( st != 0 )) || fail "stale pre-restart lines must not satisfy the gate"
}

# ---------------------------------------------------------------------------
# systemd fake injections
# ---------------------------------------------------------------------------

test_sd_timer_combinations_restored_exactly() {
    local combo
    for combo in "enabled active" "enabled inactive" "disabled active" "disabled inactive"; do
        rm -rf "$H/repos/staging"; : > "$STUB/systemctl.log"
        sd_set_unit dataui-reset.timer $combo
        local out
        out=$(run_cli staging update 2>&1) || fail "update failed for [$combo]: $out"
        assert_eq "$(sd_unit_state dataui-reset.timer)" "$combo dead0000beef" "restored [$combo]"
    done
}

test_sd_unknown_timer_states_fail_untouched() {
    local st_val
    for st_val in masked runtime not-found unknown; do
        rm -rf "$H/repos/staging"
        echo "$st_val active inv" > "$STUB/systemd-units/dataui-reset.timer"
        local out stt
        out=$(run_cli staging update 2>&1); stt=$?
        (( stt != 0 )) || fail "[$st_val] is-enabled must fail untouched"
        assert_no_dir "$H/repos/staging" "[$st_val] no clone"
    done
}

test_sd_reset_service_states() {
    # active and activating both wait then proceed once the reset
    # finishes — the flipper marks the service inactive mid-wait (a
    # static state never finishes; the real poll sleeps 10s, the flip
    # lands at 3s: after the waiting message, before the first poll)
    local s
    for s in active activating; do
        rm -rf "$H/repos/staging"
        sd_set_unit "dataui-reset.service" "disabled" "$s"
        bash -c "sleep 3; printf 'disabled inactive flip0000done\n' > '$STUB/systemd-units/dataui-reset.service'" &
        local flipper=$!
        local out
        out=$(run_cli staging update 2>&1) || fail "[$s] should wait then proceed: $out"
        kill "$flipper" 2>/dev/null || true; wait "$flipper" 2>/dev/null || true
        assert_contains "$out" "waiting up to 15 minutes"
    done
    for s in failed deactivating unknown; do
        rm -rf "$H/repos/staging"
        sd_set_unit "dataui-reset.service" "disabled" "$s"
        expect_die "failing closed" run_cli staging update
    done
}

test_sd_reset_becoming_active_at_final_check_aborts() {
    # systemd catch-up: pausing the timer starts the reset service —
    # the pre-pause wait saw inactive, and the post-pause recheck must
    # abort (a reset that started during the pause). No staging clone:
    # the abort precedes any fetch.
    cat > "$STUB/hooks/dataui-reset.timer.on-stop" <<'EOF'
echo "enabled activating catchup00001" > "$STUB/systemd-units/dataui-reset.service"
EOF
    chmod +x "$STUB/hooks/dataui-reset.timer.on-stop"
    local out st
    out=$(run_cli staging update 2>&1); st=$?
    rm -f "$STUB/hooks/dataui-reset.timer.on-stop"
    (( st != 0 )) || fail "a reset becoming active at the final check must abort"
    assert_contains "$out" "A reset started around the timer pause"
    assert_no_dir "$H/repos/staging" "aborted before fetch/clone"
}

test_sd_reset_running_at_final_check_aborts() {
    # same window, but the catch-up reset is already running (active,
    # not just activating) when the final check looks
    cat > "$STUB/hooks/dataui-reset.timer.on-stop" <<'EOF'
echo "enabled active catchup00002" > "$STUB/systemd-units/dataui-reset.service"
EOF
    chmod +x "$STUB/hooks/dataui-reset.timer.on-stop"
    local out st
    out=$(run_cli staging update 2>&1); st=$?
    rm -f "$STUB/hooks/dataui-reset.timer.on-stop"
    (( st != 0 )) || fail "a reset running at the final check must abort"
    assert_contains "$out" "A reset started around the timer pause"
    assert_no_dir "$H/repos/staging" "aborted before fetch/clone"
}

test_sd_missing_lock_root_fails() {
    # CHECKOUT_LOCK_ROOT under a regular file: mkdir -p fails
    echo x > "$H/notadir"
    local out st
    out=$(run_cmdline env CHECKOUT_LOCK_ROOT="$H/notadir/sub" run_cli staging update 2>&1); st=$?
    (( st != 0 )) || fail "unwritable lock root must fail"
}

test_sd_npm_failure_no_ops() {
    make_staging_clone
    echo ok > "$STUB/npm-fails"
    local out st
    out=$(run_cli staging update 2>&1); st=$?
    rm -f "$STUB/npm-fails"
    (( st != 0 )) || fail "npm failure must fail the update"
    assert_eq "$(ls "$H/ops" 2>/dev/null | wc -l)" 0 "no ops installed"
}

test_sd_install_failure_reports_changed_destinations() {
    make_staging_clone
    # a read-only unit destination directory: the per-file install fails
    # PARTWAY (the three lib destinations already changed; note a
    # planted directory at a destination would NOT fail — mv -f nests
    # the file inside it), and the failure must report them and name
    # the recovery
    chmod a-w "$H/systemd"
    local out st
    out=$(run_cli staging update 2>&1); st=$?
    chmod u+w "$H/systemd"
    (( st != 0 )) || fail "install failure must fail the update"
    assert_contains "$out" "ops install"
    assert_contains "$out" "Destinations already changed"
    assert_contains "$out" "profile-run.sh"
    assert_contains "$out" "rerunning 'scripts/data-ui staging update'"
}

test_sd_daemon_reload_failure() {
    make_staging_clone
    echo ok > "$STUB/systemctl-fails"
    local out st
    out=$(run_cli staging update 2>&1); st=$?
    rm -f "$STUB/systemctl-fails"
    (( st != 0 )) || fail "daemon-reload failure must fail the update"
    assert_eq "$(stub_count 'restart dataui@' "$STUB/systemctl.log")" 0 \
        "no restarts after daemon-reload failure"
}

test_sd_restoration_failure_after_failed_stop() {
    # the timer stop FAILS and later restoration also fails: loud exit
    sd_set_unit dataui-reset.timer enabled active
    cat > "$STUB/hooks/timer-stop-fails" <<'EOF'
echo x
EOF
    # make systemctl stop fail for the timer only, then enable fail too
    sed -i 's/^    stop)$/    stop)\n        [[ -f "$STUB\/stop-fails" \&\& "$u" == "dataui-reset.timer" ]] \&\& exit 1;/' \
        "$H/bin/systemctl" 2>/dev/null || true
    echo ok > "$STUB/stop-fails"
    echo ok > "$STUB/systemctl-fails"
    local out st
    out=$(run_cli staging update 2>&1); st=$?
    rm -f "$STUB/stop-fails" "$STUB/systemctl-fails"
    (( st != 0 )) || fail "failed restoration must exit nonzero"
    assert_contains "$out" "WARNING: stopping dataui-reset.timer returned nonzero"
}

# ---------------------------------------------------------------------------
# e-demo reset guard
# ---------------------------------------------------------------------------

test_reset_noargs_under_held_lock_refuses() {
    make_staging_clone
    make_profile todos 8600 e-demo
    bash -c "exec {f}<>$H/repos/staging.lock; flock -x \$f; sleep 3" &
    local holder=$!
    sleep 0.2
    local out st
    out=$(run_cli e-demo reset 2>&1); st=$?
    kill "$holder" 2>/dev/null || true; wait "$holder" 2>/dev/null || true
    (( st != 0 )) || fail "no-args reset must refuse under the staging lock"
    assert_contains "$out" "staging update appears to be in progress"
    assert_not_contains "$(cat "$STUB/systemctl.log" 2>/dev/null)" "start dataui-reset.service" \
        "the service was never started"
}

test_reset_stale_installed_script_installs_nothing() {
    make_staging_clone
    make_profile todos 8601 e-demo
    echo "old" > "$H/ops/petting-zoo-reset.sh"
    chmod +x "$H/ops/petting-zoo-reset.sh"
    sd_set_unit "dataui-reset.service" disabled inactive
    # the verb must RUN the stale installed copy as-is — it never
    # reinstalls (ops installation belongs to 'staging update' alone);
    # the explicit-name path dies with "Reset failed" when it errors
    expect_die "Reset failed" run_cli demo reset modelbank
    # explicit-name e-demo reset runs the INSTALLED copy unchanged
    echo "#!/bin/bash
echo INSTALLED-RAN \"\$@\"" > "$H/ops/petting-zoo-reset.sh"
    chmod +x "$H/ops/petting-zoo-reset.sh"
    local out
    out=$(run_cli e-demo reset todos 2>&1) || fail "explicit reset failed: $out"
    assert_contains "$out" "INSTALLED-RAN todos"
    assert_contains "$(cat "$H/ops/petting-zoo-reset.sh")" "INSTALLED-RAN" "installed copy unchanged"
}

# ---------------------------------------------------------------------------
# Wrapper string/order checks + real two-checkout SBCL load test
# ---------------------------------------------------------------------------

test_wrapper_profile_run_string_order() {
    local pr="$H/seed/ops/profile-run.sh"
    local push_line require_line root_line
    # code-anchored patterns: the file's line-91 COMMENT mentions both
    # "asdf:*central-registry*" and "(require :data-ui)" and would
    # win every head -1 race
    push_line=$(grep -n "asdf:\*central-registry\*))'" "$pr" | head -1 | cut -d: -f1)
    require_line=$(grep -n -- "--eval '(require :data-ui)'" "$pr" | head -1 | cut -d: -f1)
    root_line=$(grep -n 'DATA-UI-PACKAGE-ROOT=' "$pr" | head -1 | cut -d: -f1)
    [[ -n "$push_line" && -n "$require_line" && "$push_line" -lt "$require_line" ]] \
        || fail "central-registry push must precede (require :data-ui)"
    [[ -n "$root_line" && "$root_line" -gt "$require_line" ]] \
        || fail "*package-root* must be read after (require :data-ui)"
    local reset="$H/seed/ops/petting-zoo-reset.sh"
    assert_contains "$(cat "$reset")" '"$DATA_UI_CHECKOUT/scripts/data-ui"' \
        "reset script calls the absolute staging script"
    assert_contains "$(cat "$H/seed/ops/dataui@.service")" "DATA_UI_CHECKOUT=/data/data-ui/repos/staging"
    assert_contains "$(cat "$H/seed/ops/dataui-reset.service")" "DATA_UI_CHECKOUT=/data/data-ui/repos/staging"
}

test_sbcl_two_checkout_load_contract() {
    if [[ "${RUN_SLOW_SBCL:-1}" != "1" ]]; then
        skip_test "RUN_SLOW_SBCL=0"
        return 0
    fi
    # A SECOND checkout with its own data-ui.asd AND lisp/ (a real clone,
    # not a copied .asd pointing at the first tree).
    git -C "$H" clone -q "$H/public.git" other
    assert_file "$H/other/data-ui.asd"
    assert_dir "$H/other/lisp"
    # The load contract's environment: every real entry point
    # (scripts/data-ui verbs, the profile-run wrapper via profile.env)
    # supplies DOCUMENT_ROOT and FS_TEMP_DIRECTORY — lisp/data-ui.lisp
    # probe-files both at load time and errors when missing. A bare
    # `ros run` must mirror that with state dirs outside the checkout
    # (the profile.env pattern; the script's dev-tree default
    # tests/shared-files/ is gitignored and never reaches a clone).
    mkdir -p "$H/work/load-files" "$H/work/load-temp"
    local out st
    out=$(cd "$H/other" && DATA_UI_EXPECTED_CHECKOUT="$H/other" \
        DOCUMENT_ROOT="$H/work/load-files" \
        FS_TEMP_DIRECTORY="$H/work/load-temp" \
        ros run -- --disable-debugger \
        --eval '(let ((c (uiop:getenv "DATA_UI_EXPECTED_CHECKOUT")))
                  (push (uiop:ensure-directory-pathname c) asdf:*central-registry*))' \
        --eval '(asdf:clear-system :data-ui)' \
        --eval '(require :data-ui)' \
        --eval '(let* ((expected (uiop:getenv "DATA_UI_EXPECTED_CHECKOUT"))
                       (want (concatenate (quote string) (string-right-trim "/" expected) "/"))
                       (src (namestring (truename (asdf:system-source-directory :data-ui))))
                       (root (namestring (truename data-ui::*package-root*))))
                  (format t "DATA-UI-ASDF-SOURCE=~a~%" src)
                  (format t "DATA-UI-PACKAGE-ROOT=~a~%" root)
                  (unless (and (equal src want) (equal root want))
                    (format *error-output* "FATAL: ~a vs ~a / ~a~%" src root want)
                    (uiop:quit 3)))' 2>&1); st=$?
    (( st == 0 )) || fail "two-checkout load contract failed: $out"
    assert_contains "$out" "DATA-UI-ASDF-SOURCE=$H/other/"
    assert_contains "$out" "DATA-UI-PACKAGE-ROOT=$H/other/"
    # NEGATIVE: expecting the FIRST checkout while loading the second
    # must abort (exit 3). The push reads its own variable: pushing the
    # CLAIMED checkout would make ASDF load it and the guard could
    # never fire — the induced mismatch (push other, claim devclone) is
    # exactly the accident the real guard exists to catch.
    out=$(cd "$H/other" && DATA_UI_EXPECTED_CHECKOUT="$H/devclone" \
        DATA_UI_LOAD_FROM="$H/other" \
        DOCUMENT_ROOT="$H/work/load-files" \
        FS_TEMP_DIRECTORY="$H/work/load-temp" \
        ros run -- --disable-debugger \
        --eval '(let ((c (uiop:getenv "DATA_UI_LOAD_FROM")))
                  (push (uiop:ensure-directory-pathname c) asdf:*central-registry*))' \
        --eval '(asdf:clear-system :data-ui)' \
        --eval '(require :data-ui)' \
        --eval '(let* ((expected (uiop:getenv "DATA_UI_EXPECTED_CHECKOUT"))
                       (want (concatenate (quote string) (string-right-trim "/" expected) "/"))
                       (src (namestring (truename (asdf:system-source-directory :data-ui))))
                       (root (namestring (truename data-ui::*package-root*))))
                  (unless (and (equal src want) (equal root want))
                    (format *error-output* "FATAL: isolation~%")
                    (uiop:quit 3)))' 2>&1); st=$?
    (( st == 3 )) || fail "wrong-checkout guard must exit 3 (got $st)"
    assert_contains "$out" "FATAL: isolation"
}

# ---------------------------------------------------------------------------
# Two production sites: site identity, production-only boundary,
# forwarder entry-env, the registry path, gates/guard, release records
# and the manifest, cutover, rebirth, the backup recover verbs, and
# the AWS operator script's fence.
# ---------------------------------------------------------------------------

SITE_MARKER() { echo "$H/state/site"; }

write_site_marker() { printf '%s\n' "${1:-prod-2}" > "$(SITE_MARKER)"; }

run_prod2() {
    # A prod-2-shaped CLI run: the marker arms apply_site_defaults
    # (IMAGE_REGISTRY, SITE_DNS_MODE/ZONE_ID, SITE_STATE_URI,
    # SITE_PRODUCTION_ONLY); DEPLOY_HOST/K3D_CLUSTER are pinned to the
    # site so the harness's evo-x2 pins do not win; SITE_PUBLIC_IP
    # stands in for IMDS.
    ( eval "$(cli_env_statements)"
      export DEPLOY_HOST=prod-2 K3D_CLUSTER=prod-2 AWS_REGION=us-east-1
      export SITE_PUBLIC_IP="${SITE_PUBLIC_IP:-203.0.113.10}"
      cd "${CLI_CWD:-$H/work}" || exit 1
      exec "$H/devclone/scripts/data-ui" "$@" )
}

expected_ref() { # expected_ref MODEL — the content-addressed image ref
    # Needs the production clone present (the deploy's selection
    # fast-forwards it; tests that call this before any deploy create
    # it first — under the `public` remote name ensure_checkout
    # expects, never origin).
    [[ -d "$H/repos/production/.git" ]] || git -C "$H" clone -q -o public "$H/public.git" repos/production
    local arch
    case "$(uname -m)" in
        aarch64) arch=arm64 ;;
        *)       arch=amd64 ;;
    esac
    local sha msha
    sha=$(prod_sha)
    msha=$(git -C "$H/repos/production" show "${sha}:models/$1.lisp" | sha256sum | cut -d' ' -f1)
    echo "232168665831.dkr.ecr.us-east-1.amazonaws.com/data-ui/$1:deploy-${arch}-${sha}-${msha}"
}

seed_ecr_image() { # seed_ecr_image REF — the image exists in ECR
    mkdir -p "$STUB"
    [[ -f "$STUB/ecr-images" ]] || : > "$STUB/ecr-images"
    echo "$1" >> "$STUB/ecr-images"
}

prod2_deploy() { # prod2_deploy MODEL — a green prod-2 deploy
    default_fields >/dev/null
    run_prod2 deploy "$1"
}

r53_zone() { echo "$STUB/r53/Z0152779JVWMFEK6RAKU.json"; }

r53_put_record() { # r53_put_record NAME TYPE VALUE — add/replace in the zone fixture
    mkdir -p "$STUB/r53"
    local zone
    zone="$(r53_zone)"
    if [[ -s "$zone" ]]; then
        jq --arg n "$1." --arg t "$2" --arg v "$3" \
           'map(select(.Name != $n)) + [{"Name": $n, "Type": $t, "TTL": 300,
                                        "ResourceRecords": [{"Value": $v}]}]' \
           "$zone" > "${zone}.tmp" && mv "${zone}.tmp" "$zone"
    else
        jq -n --arg n "$1." --arg t "$2" --arg v "$3" \
           '[{"Name": $n, "Type": $t, "TTL": 300, "ResourceRecords": [{"Value": $v}]}]' \
           > "$zone"
    fi
}

eip_row() { # eip_row IP INSTANCE — one address row for cutover
    jq -n --arg ip "$1" --arg i "$2" \
       '{Addresses: [{PublicIp: $ip, InstanceId: $i, AllocationId: "eipalloc-1"}]}' \
       > "$STUB/ec2-addresses.json"
    echo i-stub > "$STUB/imds-instance-id"
    echo tok > "$STUB/imds-token"
}

imds_ip() { echo "${1:-203.0.113.10}" > "$STUB/imds-public-ip"; echo tok > "$STUB/imds-token"; }

manifest_row() { # manifest_row MODEL DOMAIN ADDRESS [SITE]
    mkdir -p "$H/state/ops"
    jq -nc --arg m "$1" --arg s "${4:-prod-2}" --arg d "$2" --arg a "$3" \
       '{model:$m, site:$s, domain:$d, address:$a}' >> "$H/state/ops/prod-manifest.jsonl"
}

s3_state() { echo "$STUB/s3/data-ui-backups-232168665831/prod-2/site"; }

s3_put_state() { # s3_put_state KEY FILE — copy into the site's S3 prefix
    mkdir -p "$(s3_state)/$(dirname "$1")"
    cp "$2" "$(s3_state)/$1"
}

s3_get_state() { cat "$(s3_state)/$1" 2>/dev/null; }

release_json() { echo "$H/state/deploy/$1/release.json"; }

seed_release_record() { # seed_release_record MODEL [SOURCE] — a valid record
    local model="$1" source="${2:-canonical}"
    local sha msha
    sha=$(prod_sha)
    if [[ "$source" == vip ]]; then
        msha=$(sha256sum "$H/repos/production/models/local/$model.lisp" 2>/dev/null | cut -d' ' -f1)
    else
        msha=$(git -C "$H/repos/production" show "${sha}:models/$model.lisp" | sha256sum | cut -d' ' -f1)
    fi
    mkdir -p "$(dirname "$(release_json "$model")")"
    jq -n --arg m "$model" --arg site prod-2 \
        --arg image "$(expected_ref "$model")" --arg full_sha "$sha" \
        --arg msha "$msha" --arg source "$source" --arg version 1.0 \
        --arg domain "$model.demo.data-ui.com" --arg repl NIL \
        --arg at "2026-11-19T12:00:00Z" \
        '{model:$m, site:$site, image:$image, full_sha:$full_sha,
          model_sha256:$msha, source:$source, version:$version,
          domain:$domain, repl:$repl, deployed_at:$at}' \
        > "$(release_json "$model")"
}

test_site_marker_wins_and_invalid_dies() {
    make_staging_clone
    export STUB_NO_SSH_EXEC=1
    default_fields >/dev/null
    # No marker: today's behavior — deploy_outer sees hostname ==
    # DEPLOY_HOST and runs locally (no ssh).
    : > "$STUB/ssh.log"
    run_cli deploy todos >/dev/null 2>&1 || true
    assert_eq "$(wc -l < "$STUB/ssh.log")" "0" "no marker: local deploy, no forwarding"
    rm -rf "$H/repos/production"
    # Marker: local_site (prod-2) != DEPLOY_HOST (the real host) → the
    # thin forwarder fires even though the hostname matches; pinned to
    # prod-2 it forwards THERE.
    write_site_marker prod-2
    : > "$STUB/ssh.log"
    run_cli deploy todos >/dev/null 2>&1 || true
    assert_eq "$(wc -l < "$STUB/ssh.log")" "1" "marker: deploy forwards despite matching hostname"
    # From an UNSTAMPED host (the evo-x2 side), the explicit
    # DEPLOY_HOST=prod-2 deploy forwards to prod-2 itself.
    rm -f "$(SITE_MARKER)"
    rm -rf "$H/repos/production"
    : > "$STUB/ssh.log"
    expect_ok env CLI_DEPLOY_HOST=prod-2 STUB_NO_SSH_EXEC=1 run_cli deploy todos
    assert_contains "$(cat "$STUB/ssh.log")" "prod-2" "forwarded to prod-2 itself"
    # Invalid marker: die naming the file, before anything else.
    write_site_marker "Prod 2!"
    expect_die "Invalid site marker" run_cli field todos name
}

test_site_defaults_armed_by_marker() {
    # The site-keyed guards follow the marker: with prod-2 stamped and
    # DEPLOY_HOST left at the real host, deploy-bootstrap refuses
    # naming prod-2 as THIS host; with DEPLOY_HOST=prod-2 it matches
    # and runs (bootstraps); without the marker the same prod-2 pin
    # refuses the other way around.
    write_site_marker prod-2
    expect_die "must run on $REAL_HOST (this is prod-2)" \
        env CLI_DEPLOY_HOST="$REAL_HOST" run_cli deploy-bootstrap
    rm -f "$(SITE_MARKER)"
    expect_die "must run on prod-2 (this is $REAL_HOST)" \
        env CLI_DEPLOY_HOST=prod-2 run_cli deploy-bootstrap
}

test_site_envelope_host_check_follows_marker() {
    # The production envelope's host check keys on the marker: a
    # forged envelope from the production clone on a marked host with
    # DEPLOY_HOST=prod-2 passes the host check (and dies later, on the
    # SHA recheck — the load-bearing proof it got PAST the host line);
    # without the marker the same invocation dies on the wrong host.
    git -C "$H" clone -q "$H/public.git" repos/production
    write_site_marker prod-2
    expect_die "Checkout moved under the envelope" \
        env CLI_DEPLOY_HOST=prod-2 CLI_DATA_UI_STATE="$H/state" \
        run_prod_cli "__DATA_UI_REEXEC_V1__" production public master \
        1111111111111111111111111111111111111111 63 - none - - deploy todos
    rm -f "$(SITE_MARKER)"
    expect_die "wrong host" \
        env CLI_DEPLOY_HOST=prod-2 CLI_DATA_UI_STATE="$H/state" \
        run_prod_cli "__DATA_UI_REEXEC_V1__" production public master \
        1111111111111111111111111111111111111111 63 - none - - deploy todos
}

test_site_production_only_boundary() {
    make_staging_clone
    write_site_marker prod-2
    local out
    # Every staging-routable shape dies AT THE BOUNDARY: nonzero, the
    # production-only message, no routing line, no stub mutation.
    local shape
    for shape in "staging update" "snapshot save name1 stg:books" \
                 "snapshot restore name1 stg:books" "repl books" \
                 "db books" "psql books" "demo start x" "e-demo start x" \
                 "profile list" "profile expose books"; do
        : > "$STUB/kubectl.log"; : > "$STUB/sudo.log"; : > "$STUB/npm.log"
        out=$(run_prod2 $shape 2>&1); st=$?
        (( st != 0 )) || fail "production-only must refuse: ${shape}"
        assert_contains "$out" "production-only" "refusal message for: ${shape}"
        assert_not_contains "$out" "Routing to the staging checkout" "no routing for: ${shape}"
        assert_eq "$(cat "$STUB/kubectl.log" "$STUB/sudo.log" "$STUB/npm.log" 2>/dev/null | wc -l)" "0" \
            "no mutation or stub calls for: ${shape}"
    done
    # profile install-model never routes: it proceeds past the boundary
    # (into its own missing-model error, not the boundary's).
    out=$(run_prod2 profile install-model nosuch 2>&1); st=$?
    assert_not_contains "$out" "production-only" "install-model is exempt"
    assert_contains "$out" "No local model" "install-model still runs"
    # Unnamed dev verbs and non-stg snapshot addresses stay local
    # (they may fail inside the verb — no postgres here — but never on
    # the boundary).
    out=$(run_prod2 psql 2>&1) || true
    assert_not_contains "$out" "production-only" "unnamed psql stays local"
    out=$(run_prod2 snapshot save keep-x dev:books 2>&1) || true
    assert_not_contains "$out" "production-only" "dev: addresses are not boundary shapes"
    # The marker-unset twin: routing works again (the routing line
    # appears for a staging-routable shape).
    rm -f "$(SITE_MARKER)"
    out=$(run_cli profile list 2>&1) || true
    assert_contains "$out" "Routing to the staging checkout" "unset twin still routes"
}

test_site_production_only_boundary_routing_shapes() {
    # The remaining promised shapes: run_cli db / snapshot restore stg:
    # die at the boundary (the routing twins of the existing tests,
    # proven at the boundary's true place), while install-model and
    # prd: restores pass through it.
    write_site_marker prod-2
    expect_die "production-only" run_prod2 db todos
    expect_die "production-only" run_prod2 snapshot restore name1 stg:books
    expect_die "production-only" run_prod2 snapshot save name2 stg:books
    # prd: restore is a site-local shape: it passes the boundary and
    # dies inside the verb (no such snapshot), never on the boundary.
    local out
    out=$(run_prod2 snapshot restore nope prd:todos 2>&1); st=$?
    assert_not_contains "$out" "production-only" "prd: restores pass the boundary"
    (( st != 0 )) || fail "prd: restore of a missing snapshot must fail inside the verb"
}

test_forwarder_entry_env_membership() {
    # Only names present at ENTRY cross: the harness exports
    # DATA_UI_PUBLIC_URL (forwarded), but a launcher mode that builds
    # the environment WITHOUT exporting K3D_CLUSTER proves the
    # script-defaulted value stays local. REBUILD and MOVE cross.
    make_staging_clone
    default_fields >/dev/null
    export STUB_NO_SSH_EXEC=1
    : > "$STUB/ssh.log"
    # A minimal launcher: PATH + the stub plumbing + DATA_UI_STATE
    # only (no K3D_CLUSTER, no DATA_UI_PUBLIC_URL exports).
    ( export PATH="$H/bin:$ORIG_PATH" STUB="$STUB" TMPDIR="$H/work"
      export DEPLOY_CHECKOUT="$H/repos/production" STAGING_CHECKOUT="$H/repos/staging"
      export CHECKOUT_LOCK_ROOT="$H/repos" DATA_UI_STATE="$H/state"
      export DEPLOY_STATE_DIR="$H/state/deploy"
      export DEPLOY_HOST=prod-2 STAGING_HOST="$REAL_HOST"
      export DEPLOY_HEALTH_TIMEOUT=8
      cd "$H/work" || exit 1
      REBUILD=1 MOVE=1 "$H/devclone/scripts/data-ui" deploy todos ) >/dev/null 2>&1 || true
    local cmd
    cmd=$(cat "$STUB/ssh.log")
    assert_contains "$cmd" "DEPLOY_HOST=prod-2" "entry-set DEPLOY_HOST forwards"
    assert_contains "$cmd" "REBUILD=1" "REBUILD forwards"
    assert_contains "$cmd" "MOVE=1" "MOVE forwards"
    assert_not_contains "$cmd" "K3D_CLUSTER=" "script-defaulted K3D_CLUSTER stays local"
    assert_not_contains "$cmd" "DATA_UI_PUBLIC_URL=" "script-defaulted PUBLIC_URL stays local"
    # No recursive forward: on the site itself (marker == DEPLOY_HOST)
    # a deploy runs locally and never touches ssh.
    write_site_marker prod-2
    : > "$STUB/ssh.log"
    unset STUB_NO_SSH_EXEC
    prod2_deploy todos >/dev/null 2>&1 || true
    assert_eq "$(wc -l < "$STUB/ssh.log")" "0" "marker == DEPLOY_HOST: no recursive forward"
}

test_registry_hit_skips_compile_and_build() {
    write_site_marker prod-2
    default_fields >/dev/null
    local ref
    ref=$(expected_ref todos)
    seed_ecr_image "$ref"
    : > "$STUB/ros.log"; : > "$STUB/docker.log"
    local out
    out=$(run_prod2 deploy todos 2>&1) || fail "registry-hit deploy failed: $out"
    assert_contains "$out" "Registry hit"
    assert_eq "$(stub_count 'init-database' "$STUB/ros.log")" "0" "no compile on a hit"
    assert_eq "$(stub_count 'docker build' "$STUB/docker.log")" "0" "no build on a hit"
    assert_eq "$(stub_count 'k3d image import' "$STUB/k3d.log")" "1" "hit still imports into k3d"
    assert_contains "$out" "Deployed" "both gates passed"
    assert_file "$(release_json todos)" "release record written"
}

test_registry_miss_builds_imports_pushes() {
    write_site_marker prod-2
    default_fields >/dev/null
    : > "$STUB/docker.log"; : > "$STUB/ecr-images" 2>/dev/null || : > "$STUB/ecr-images"
    local out
    out=$(run_prod2 deploy todos 2>&1) || fail "registry-miss deploy failed: $out"
    assert_contains "$out" "registry miss"
    assert_eq "$(stub_count 'docker build' "$STUB/docker.log")" "1" "miss builds"
    assert_eq "$(stub_count 'k3d image import' "$STUB/k3d.log")" "1" "miss imports"
    local ref
    ref=$(expected_ref todos)
    grep -qxF "$ref" "$STUB/ecr-images" || fail "miss pushed the content tag"
    assert_contains "$out" "Pushed" "push reported"
}

test_registry_push_failure_warns_and_succeeds() {
    write_site_marker prod-2
    default_fields >/dev/null
    : > "$STUB/ecr-images"
    touch "$STUB/docker-push-fails"
    local out
    out=$(run_prod2 deploy todos 2>&1) || fail "push failure must not fail the deploy: $out"
    assert_contains "$out" "next rebirth of todos may have to build"
}

test_registry_pull_failure_falls_back_to_build() {
    write_site_marker prod-2
    default_fields >/dev/null
    seed_ecr_image "$(expected_ref todos)"
    touch "$STUB/docker-pull-fails"
    : > "$STUB/docker.log"
    local out
    out=$(run_prod2 deploy todos 2>&1) || fail "pull-failure fallback deploy failed: $out"
    assert_contains "$out" "WARNING" "pull failure warns"
    assert_eq "$(stub_count 'docker build' "$STUB/docker.log")" "1" "pull failure falls back to a build"
}

test_registry_rebuild_logs_in_builds_and_pushes() {
    write_site_marker prod-2
    default_fields >/dev/null
    seed_ecr_image "$(expected_ref todos)"
    : > "$STUB/ros.log"; : > "$STUB/docker.log"; : > "$STUB/ecr-images"
    local out
    out=$(REBUILD=1 run_prod2 deploy todos 2>&1) \
        || fail "REBUILD deploy failed: $out"
    assert_eq "$(stub_count 'init-database' "$STUB/ros.log")" "1" "REBUILD compiles (probe skipped)"
    assert_eq "$(stub_count 'docker build' "$STUB/docker.log")" "1" "REBUILD builds"
    assert_contains "$out" "Pushed" "REBUILD pushes over the same content tag"
    # Fresh-host push: no cached docker login, the push still
    # authenticates (the login call precedes the push in the log).
    local log
    log=$(cat "$STUB/docker.log")
    local login_at push_at
    login_at=$(grep -n "docker login" <<<"$log" | head -1 | cut -d: -f1)
    push_at=$(grep -n "docker push" <<<"$log" | head -1 | cut -d: -f1)
    [[ -n "$login_at" && -n "$push_at" && "$login_at" -lt "$push_at" ]] \
        || fail "login must precede the push (fresh-host authentication)"
}

test_registry_import_failure_aborts() {
    write_site_marker prod-2
    default_fields >/dev/null
    seed_ecr_image "$(expected_ref todos)"
    touch "$STUB/k3d-import-fails"
    : > "$STUB/docker.log"
    expect_die "k3d image import failed" run_prod2 deploy todos
    assert_eq "$(stub_count 'docker build' "$STUB/docker.log")" "0" "never a registry fallback after import failure"
}

test_registry_vip_bytes_change_the_tag() {
    write_site_marker prod-2
    # A VIP model (no canonical file for it — the handoff rules refuse
    # a canonical collision).
    set_fields vipspec name=vipspec "title=VIP Spec" version=1.0 \
        domain=vipspec.demo.data-ui.com repl=NIL
    local vip1 vip2
    vip1=$(make_vip_model_file vipspec)
    vip2="$H/work/vip-vipspec-2.lisp"
    { cat "$vip1"; printf ';; truly different bytes\n'; } > "$vip2"
    : > "$STUB/ecr-images"
    MODEL_FILE="$vip1" run_prod2 deploy vipspec >/dev/null 2>&1 \
        || fail "first VIP deploy failed"
    local tag1
    tag1=$(grep 'data-ui/vipspec' "$STUB/ecr-images" | head -1)
    MODEL_FILE="$vip2" run_prod2 deploy vipspec >/dev/null 2>&1 \
        || fail "second VIP deploy failed"
    local tag2
    tag2=$(grep 'data-ui/vipspec' "$STUB/ecr-images" | tail -1)
    [[ -n "$tag1" && -n "$tag2" && "$tag1" != "$tag2" ]] \
        || fail "changed VIP bytes at the same SHA must produce a different tag (${tag1:-none} vs ${tag2:-none})"
}

test_registry_dry_run_purity() {
    write_site_marker prod-2
    default_fields >/dev/null
    seed_ecr_image "$(expected_ref todos)"
    : > "$STUB/aws.log"; : > "$STUB/docker.log"
    local out
    out=$(DRY_RUN=1 run_prod2 deploy todos 2>&1) || fail "dry run failed: $out"
    assert_eq "$(wc -l < "$STUB/aws.log")" "0" "dry run makes zero aws calls (no login/pull/push/record)"
    assert_eq "$(stub_count 'login' "$STUB/docker.log")" "0" "dry run never logs into ECR"
    assert_eq "$(stub_count 'pull' "$STUB/docker.log")" "0" "dry run never pulls"
    assert_no_file "$(release_json todos)" "dry run writes no release record"
}

test_registry_absent_legacy_image_name() {
    # No marker, no IMAGE_REGISTRY: the legacy image name and zero aws
    # calls (evo-x2 byte-identical behavior).
    default_fields >/dev/null
    : > "$STUB/aws.log"
    local out
    out=$(run_cli deploy todos 2>&1) || fail "legacy deploy failed: $out"
    assert_eq "$(wc -l < "$STUB/aws.log")" "0" "legacy path makes zero aws calls"
    local tag
    tag=$(tail -1 "$STUB/built-tags")
    assert_contains "$tag" "macnod/data-ui:todos-1.0-" "legacy image name kept"
    assert_no_file "$(release_json todos)" "no release record without SITE_STATE_URI"
}

# ---------------------------------------------------------------------------
# Gates, the wrong-site guard, publication verdicts
# ---------------------------------------------------------------------------

test_gate_explicit_pins_and_never_routes_dns() {
    write_site_marker prod-2
    default_fields >/dev/null
    : > "$STUB/curl.log"; : > "$STUB/aws.log"
    local out
    out=$(run_prod2 deploy todos 2>&1) || fail "explicit-DNS deploy failed: $out"
    grep -q -- "--resolve todos.demo.data-ui.com:443:203.0.113.10" "$STUB/curl.log" \
        || fail "gate 2 must pin to the site's own address (203.0.113.10)"
    # A deploy never CHANGES DNS on an explicit-DNS site (the closing
    # summary's read-only exact lookup is the one allowed call).
    assert_eq "$(stub_count 'route53 change-resource-record-sets' "$STUB/aws.log")" "0" \
        "a deploy never changes Route 53 records on an explicit-DNS site"
}

test_guard_wildcard_shadowed_name_and_overrides() {
    default_fields >/dev/null
    export SITE_WILDCARD_ZONE=demo.data-ui.com
    # The guard consults a PUBLIC resolver: the domain answers
    # elsewhere, the wildcard probe answers here → refuse with the
    # corrected command.
    mkdir -p "$STUB/dig"
    echo 192.0.2.9 > "$STUB/dig/todos.demo.data-ui.com"
    echo 192.0.2.1 > "$STUB/dig/site-guard-probe.demo.data-ui.com"
    expect_die "served by another site" run_cli deploy todos
    expect_die "MOVE=1 overrides" run_cli deploy todos
    # MOVE=1 overrides and pins gate 2 to loopback.
    : > "$STUB/curl.log"
    expect_ok env MOVE=1 SITE_WILDCARD_ZONE=demo.data-ui.com run_cli deploy todos
    grep -q -- "--resolve todos.demo.data-ui.com:443:127.0.0.1" "$STUB/curl.log" \
        || fail "MOVE=1 must pin gate 2 to 127.0.0.1"
    # Same answer → the guard passes.
    echo 192.0.2.1 > "$STUB/dig/todos.demo.data-ui.com"
    local out
    out=$(run_cli deploy todos 2>&1) || fail "guard passes when the domain answers here: $out"
    assert_contains "$out" "Wrong-site guard passed"
    # Resolver failure → note line, continue.
    rm -f "$STUB/dig/todos.demo.data-ui.com"
    touch "$STUB/dig-fails"
    out=$(run_cli deploy todos 2>&1) || fail "resolver failure must proceed: $out"
    assert_contains "$out" "could not resolve"
    # A custom domain outside the zone is skipped (no dig at all).
    rm -f "$STUB/dig-fails"
    rm -rf "$STUB/dig"
    set_fields todos name=todos "title=Todos" version=1.0 \
        domain=custom.example.com repl=NIL
    out=$(run_cli deploy todos 2>&1) || fail "custom domain deploy: $out"
    assert_not_contains "$out" "Wrong-site guard" "out-of-zone domains skip the guard"
}

test_summary_publication_verdicts() {
    write_site_marker prod-2
    default_fields >/dev/null
    seed_ecr_image "$(expected_ref todos)"
    local out
    # No exact record → unpublished.
    out=$(run_prod2 deploy todos 2>&1) || fail "deploy for verdicts failed: $out"
    assert_contains "$out" "Unpublished — run 'scripts/data-ui cutover todos'"
    # A same-name non-A record is not a match (still unpublished).
    r53_put_record todos.demo.data-ui.com TXT "some-token"
    out=$(run_prod2 deploy todos 2>&1) || true
    assert_contains "$out" "Unpublished — run 'scripts/data-ui cutover todos'" "non-A record is no exact record"
    # A following-name hit is not a match either.
    r53_put_record zzz.demo.data-ui.com A 203.0.113.10
    out=$(run_prod2 deploy todos 2>&1) || true
    assert_contains "$out" "Unpublished" "following-name hit is no exact record"
    # The record points here, no manifest row → published but
    # unrecorded (the half-finished cutover).
    r53_put_record todos.demo.data-ui.com A 203.0.113.10
    out=$(run_prod2 deploy todos 2>&1) || true
    assert_contains "$out" "Published but unrecorded" "the pending state is named"
    assert_contains "$out" "cutover todos" "the pending line names the fix"
    # Row and record agree → published.
    manifest_row todos todos.demo.data-ui.com 203.0.113.10
    out=$(run_prod2 deploy todos 2>&1) || true
    assert_contains "$out" "Published: todos.demo.data-ui.com -> 203.0.113.10"
    # The record points elsewhere → unpublished.
    r53_put_record todos.demo.data-ui.com A 192.0.2.7
    out=$(run_prod2 deploy todos 2>&1) || true
    assert_contains "$out" "Unpublished" "a record elsewhere is unpublished"
    # A failed lookup → publication unknown, never "unpublished".
    touch "$STUB/aws-fails-route53"
    out=$(run_prod2 deploy todos 2>&1) || true
    assert_contains "$out" "Publication unknown" "query failure is its own verdict"
}

# ---------------------------------------------------------------------------
# The release record and DEPLOY_RECORDED_RELEASE
# ---------------------------------------------------------------------------

test_release_record_gates_and_shape() {
    write_site_marker prod-2
    default_fields >/dev/null
    seed_ecr_image "$(expected_ref todos)"
    # A failed gate 2 writes no record.
    printf 'todos.demo.data-ui.com 503\n' > "$STUB/curl-responses"
    local out
    out=$(run_prod2 deploy todos 2>&1) || true
    assert_no_file "$(release_json todos)" "no record when gate 2 fails"
    rm -f "$STUB/curl-responses"
    # A dry run writes no record.
    DRY_RUN=1 run_prod2 deploy todos >/dev/null 2>&1 || true
    assert_no_file "$(release_json todos)" "no record on a dry run"
    # Success writes it — with the content-ref image and the facts.
    out=$(run_prod2 deploy todos 2>&1) || fail "deploy for the record failed: $out"
    assert_file "$(release_json todos)"
    jq -e --arg ref "$(expected_ref todos)" \
       '.image == $ref and .model == "todos" and .site == "prod-2"
        and .source == "canonical" and .version == "1.0"
        and .domain == "todos.demo.data-ui.com"' \
       "$(release_json todos)" >/dev/null \
        || fail "release record shape wrong: $(cat "$(release_json todos)")"
    # The S3 copy exists for a fresh-data rebirth.
    assert_file "$(s3_state)/releases/todos.json" "record copied to S3"
}

test_recorded_release_pull_or_die() {
    write_site_marker prod-2
    default_fields >/dev/null
    seed_ecr_image "$(expected_ref todos)"
    run_prod2 deploy todos >/dev/null 2>&1 || fail "seed deploy failed"
    local before
    before=$(cat "$(release_json todos)")
    : > "$STUB/ros.log"; : > "$STUB/docker.log"
    # Pull-or-die: no compile, no build, no tag rewrite, record
    # untouched.
    local out
    out=$(DEPLOY_RECORDED_RELEASE=1 run_prod2 deploy todos 2>&1) \
        || fail "recorded-release deploy failed: $out"
    assert_contains "$out" "Recorded release"
    assert_eq "$(stub_count 'init-database' "$STUB/ros.log")" "0" "no compile"
    assert_eq "$(stub_count 'build' "$STUB/docker.log")" "0" "no build"
    assert_eq "$(cat "$(release_json todos)")" "$before" "record left as it is"
    # A pull failure dies with no build fallback.
    : > "$STUB/docker.log"
    touch "$STUB/docker-pull-fails"
    expect_die "pulling the recorded image" env DEPLOY_RECORDED_RELEASE=1 \
        run_prod2 deploy todos
    rm -f "$STUB/docker-pull-fails"
    assert_eq "$(stub_count 'build' "$STUB/docker.log")" "0" "no build fallback under a recorded release"
    # A malformed recorded image is refused.
    jq '.image = "not-a-ref"' "$(release_json todos)" > /tmp/rr.json
    mv /tmp/rr.json "$(release_json todos)"
    expect_die "malformed" env DEPLOY_RECORDED_RELEASE=1 run_prod2 deploy todos
}

test_recorded_release_drives_the_render() {
    write_site_marker prod-2
    default_fields >/dev/null
    seed_ecr_image "$(expected_ref todos)"
    run_prod2 deploy todos >/dev/null 2>&1 || fail "seed deploy failed"
    # A model-hash mismatch (the checkout's model moved under the
    # record) dies BEFORE any mutation.
    echo ';; drifted' >> "$H/repos/production/models/todos.lisp"
    git -C "$H/repos/production" add models/todos.lisp
    git -C "$H/repos/production" -c user.email=t@t -c user.name=t commit -qm drift
    git -C "$H/repos/production" push -q public HEAD:master
    : > "$STUB/aws.log"
    expect_die "model hash" env DEPLOY_RECORDED_RELEASE=1 run_prod2 deploy todos
    assert_eq "$(stub_count 's3 cp' "$STUB/aws.log")" "0" "model-hash mismatch dies before mutation"
    # A full_sha mismatch proceeds with a warning; the facts come from
    # the record (its domain drives the render's pin, not the model
    # file's).
    run_prod2 deploy todos >/dev/null 2>&1 || true   # re-record at the new HEAD
    jq '.full_sha = "1111111111111111111111111111111111111111"
         | .domain = "recorded.demo.data-ui.com"' "$(release_json todos)" \
        > /tmp/rr2.json
    mv /tmp/rr2.json "$(release_json todos)"
    seed_ecr_image "$(jq -r .image "$(release_json todos)")"
    : > "$STUB/curl.log"
    local out
    out=$(DEPLOY_RECORDED_RELEASE=1 run_prod2 deploy todos 2>&1) \
        || fail "full_sha mismatch must proceed: $out"
    assert_contains "$out" "WARNING: the release record was made at"
    grep -q -- "--resolve recorded.demo.data-ui.com:443:203.0.113.10" "$STUB/curl.log" \
        || fail "the record's domain drives the render, never the moved model file"
}

# ---------------------------------------------------------------------------
# cutover and delete's DNS/manifest cleanup
# ---------------------------------------------------------------------------

cutover_green() { # fixtures for a happy cutover of MODEL
    default_fields >/dev/null
    # cutover_outer requires the production checkout to exist.
    [[ -d "$H/repos/production/.git" ]] || git -C "$H" clone -q -o public "$H/public.git" repos/production
    make_ns_fixture todos 30310
    eip_row 203.0.113.10 i-stub
}

test_cutover_refusals() {
    # Wildcard site.
    default_fields >/dev/null
    expect_die "explicit-DNS sites only" run_cli cutover todos
    # Explicit site, missing instance (the production checkout exists;
    # the namespace does not).
    write_site_marker prod-2
    default_fields >/dev/null
    [[ -d "$H/repos/production/.git" ]] || git -C "$H" clone -q -o public "$H/public.git" repos/production
    expect_die "No deployed instance" run_prod2 cutover todos
    make_ns_fixture todos
    eip_row 203.0.113.10 i-stub
    # No address row.
    echo '{"Addresses": []}' > "$STUB/ec2-addresses.json"
    expect_die "wanted exactly one Elastic IP row" run_prod2 cutover todos
    # More than one row.
    jq -n '{Addresses: [{PublicIp: "203.0.113.10", InstanceId: "i-a"},
                        {PublicIp: "203.0.113.10", InstanceId: "i-b"}]}' \
        > "$STUB/ec2-addresses.json"
    expect_die "wanted exactly one Elastic IP row" run_prod2 cutover todos
    # PublicIp mismatch.
    eip_row 192.0.2.99 i-stub
    expect_die "PublicIp is 192.0.2.99" run_prod2 cutover todos
    # The scratch-host refusal: the EIP is bound to ANOTHER instance.
    eip_row 203.0.113.10 i-scratch
    expect_die "A scratch host must never publish" run_prod2 cutover todos
    # Failed pinned health: nothing touches DNS.
    eip_row 203.0.113.10 i-stub
    printf 'todos.demo.data-ui.com 503\n' > "$STUB/curl-responses"
    : > "$STUB/r53-changes.log"
    expect_die "pre-DNS pinned /health failed" run_prod2 cutover todos
    assert_eq "$(wc -l < "$STUB/r53-changes.log" 2>/dev/null || echo 0)" "0" \
        "a failed health gate never touches DNS"
}

test_cutover_happy_path_and_rerun() {
    write_site_marker prod-2
    cutover_green
    : > "$STUB/r53-changes.log"; : > "$STUB/curl.log"
    local out
    out=$(run_prod2 cutover todos 2>&1) || fail "cutover failed: $out"
    assert_contains "$out" "Cutover complete: todos.demo.data-ui.com -> 203.0.113.10"
    # The happy-path order: health, UPSERT (INSYNC inside the dns
    # helper), health again, then the manifest row.
    assert_contains "$(cat "$STUB/r53-changes.log")" "UPSERT"
    assert_file "$H/state/ops/prod-manifest.jsonl" "the manifest row landed"
    jq -e '.model == "todos" and .site == "prod-2"
           and .domain == "todos.demo.data-ui.com"
           and .address == "203.0.113.10"' \
       "$H/state/ops/prod-manifest.jsonl" >/dev/null || fail "row shape wrong"
    assert_file "$(s3_state)/prod-manifest.jsonl" "the row's S3 copy exists"
    # Idempotent re-run: still one row, still green.
    out=$(run_prod2 cutover todos 2>&1) || fail "idempotent re-run failed: $out"
    assert_eq "$(wc -l < "$H/state/ops/prod-manifest.jsonl")" "1" "re-run keeps exactly one row"
    assert_contains "$out" "Cutover complete"
}

test_cutover_half_finished_converges() {
    write_site_marker prod-2
    cutover_green
    # The half-finished state: the A record exists, the manifest row
    # does not (a cutover that died after INSYNC).
    r53_put_record todos.demo.data-ui.com A 203.0.113.10
    seed_ecr_image "$(expected_ref todos)"
    local out
    out=$(run_prod2 deploy todos 2>&1) || true
    assert_contains "$out" "Published but unrecorded" "deploy reports the pending state"
    # A cutover re-run converges: health, row.
    out=$(run_prod2 cutover todos 2>&1) || fail "converging re-run failed: $out"
    assert_file "$H/state/ops/prod-manifest.jsonl"
    out=$(run_prod2 deploy todos 2>&1) || true
    assert_contains "$out" "Published: todos.demo.data-ui.com" "converged to published"
    # A deliberate delete in the half-finished state still removed the
    # record and skipped the row-removal harmlessly; the record is
    # removed only when it holds this site's value.
    : > "$STUB/r53-changes.log"
    FORCE=1 run_prod2 delete todos >/dev/null 2>&1 || fail "delete failed"
    assert_contains "$(cat "$STUB/r53-changes.log")" "DELETE" "record removed by delete"
    assert_eq "$(grep -c '"model":"todos"' "$H/state/ops/prod-manifest.jsonl" 2>/dev/null)" "0" \
        "manifest rows for the model removed"
}

test_cutover_route53_failure_leaves_no_row() {
    write_site_marker prod-2
    cutover_green
    touch "$STUB/r53-change-fails"
    : > "$STUB/r53-changes.log"
    local out
    out=$(run_prod2 cutover todos 2>&1) || true
    assert_contains "$out" "the DNS upsert for todos.demo.data-ui.com failed"
    assert_no_file "$H/state/ops/prod-manifest.jsonl" "a failed upsert leaves no manifest row"
    # A re-run converges once Route 53 answers.
    rm -f "$STUB/r53-change-fails"
    out=$(run_prod2 cutover todos 2>&1) || fail "converging re-run failed: $out"
    assert_contains "$out" "Cutover complete"
    assert_file "$H/state/ops/prod-manifest.jsonl"
}

test_delete_midsequence_failure_rerun_converges() {
    # Failure between the DNS deletion and the manifest removal's S3
    # copy: the record is deleted, the local row is filtered, and the
    # die leaves the S3 manifest stale — a re-run converges (the S3
    # copy is rewritten from the filtered local file).
    write_site_marker prod-2
    cutover_green
    r53_put_record todos.demo.data-ui.com A 203.0.113.10
    mkdir -p "$H/state/ops"
    manifest_row todos todos.demo.data-ui.com 203.0.113.10
    touch "$STUB/aws-fails-s3cp"
    : > "$STUB/r53-changes.log"
    local out
    out=$(FORCE=1 run_prod2 delete todos 2>&1) || true
    assert_contains "$out" "copying the manifest to S3 failed" "the mid-sequence failure is named"
    assert_contains "$(cat "$STUB/r53-changes.log")" "DELETE" "the record was removed first"
    assert_eq "$(grep -c '"model":"todos"' "$H/state/ops/prod-manifest.jsonl" 2>/dev/null)" "0" \
        "the local row was filtered before the failed copy"
    rm -f "$STUB/aws-fails-s3cp"
    out=$(FORCE=1 run_prod2 delete todos 2>&1) || fail "the re-run must converge"
    assert_eq "$(grep -c "DELETE" "$STUB/r53-changes.log")" "1" \
        "the re-run deleted no record twice (idempotent)"
    if [[ -f "$(s3_state)/prod-manifest.jsonl" ]]; then
        assert_eq "$(grep -c '"model":"todos"' "$(s3_state)/prod-manifest.jsonl")" "0" \
            "the S3 manifest converged on the re-run"
    fi
}

test_delete_dns_only_when_its_ours() {
    write_site_marker prod-2
    cutover_green
    # The evidence: a manifest row naming the domain.
    mkdir -p "$H/state/ops"
    manifest_row todos todos.demo.data-ui.com 203.0.113.10
    r53_put_record todos.demo.data-ui.com A 192.0.2.7   # another site's record
    : > "$STUB/r53-changes.log"
    FORCE=1 run_prod2 delete todos >/dev/null 2>&1 || fail "delete failed"
    assert_eq "$(wc -l < "$STUB/r53-changes.log" 2>/dev/null || echo 0)" "0" \
        "another site's record is never deleted"
    # ... and removed when it holds this site's address.
    r53_put_record todos.demo.data-ui.com A 203.0.113.10
    manifest_row todos todos.demo.data-ui.com 203.0.113.10
    : > "$STUB/r53-changes.log"
    FORCE=1 run_prod2 delete todos >/dev/null 2>&1 || fail "second delete failed"
    assert_contains "$(cat "$STUB/r53-changes.log")" "DELETE"
}

test_delete_domain_evidence_union() {
    # A D1 manifest row plus a D2 release.json (the failed
    # domain-change cutover): both candidates are cleaned, each with
    # its own exact-record read.
    write_site_marker prod-2
    cutover_green
    r53_put_record todos.demo.data-ui.com A 203.0.113.10
    r53_put_record newname.demo.data-ui.com A 203.0.113.10
    mkdir -p "$H/state/ops" "$STUB/r53"
    manifest_row todos todos.demo.data-ui.com 203.0.113.10
    mkdir -p "$H/state/deploy/todos"
    jq -n '{model: "todos", domain: "newname.demo.data-ui.com"}' \
        > "$H/state/deploy/todos/release.json"
    : > "$STUB/r53-changes.log"
    FORCE=1 run_prod2 delete todos >/dev/null 2>&1 || fail "delete failed"
    local log
    log=$(cat "$STUB/r53-changes.log")
    assert_contains "$log" "todos.demo.data-ui.com" "the manifest row's domain is cleaned"
    assert_contains "$log" "newname.demo.data-ui.com" "the release record's domain is cleaned"
    # A manifest row that exists ONLY in S3 still counts.
    r53_put_record todos.demo.data-ui.com A 203.0.113.10
    rm -f "$H/state/ops/prod-manifest.jsonl"
    jq -nc '{model:"todos", site:"prod-2", domain:"todos.demo.data-ui.com", address:"203.0.113.10"}' \
        > "$H/work/s3-manifest.jsonl"
    s3_put_state prod-manifest.jsonl "$H/work/s3-manifest.jsonl"
    : > "$STUB/r53-changes.log"
    FORCE=1 run_prod2 delete todos >/dev/null 2>&1 || fail "S3-only delete failed"
    assert_contains "$(cat "$STUB/r53-changes.log")" "todos.demo.data-ui.com" \
        "S3-only rows are evidence too"
}

# ---------------------------------------------------------------------------
# rebirth
# ---------------------------------------------------------------------------

make_backup_stub() { # a recording ~/bin/<app>-backup
    mkdir -p "$H/bin"
    cat > "$H/bin/$1-backup" <<EOF
#!/bin/bash
echo "$1-backup \$*" >> "$STUB/backup.log"
POOL="\${POOL:-$H/state/snapshots}"
case "\$1" in
    location) echo "s3://data-ui-backups-232168665831/prod-2/$1" ;;
    recover)
        [[ -f "$STUB/backup-recover-fails" ]] && exit 1
        # materialize the pool triple the way the real recover does
        # (a REAL gzip tar with a files/ entry — the restore untars it)
        for n in prd-$1-daily-20261119-0320; do
            mkdir -p "\$POOL"
            echo dump > "\$POOL/\$n.dump"
            tmpd=\$(mktemp -d)
            mkdir -p "\$tmpd/files"
            echo photo > "\$tmpd/files/p.txt"
            tar -czf "\$POOL/\$n.files.tar.gz" -C "\$tmpd" files
            rm -rf "\$tmpd"
            printf 'model=$1\nsource-address=prd:$1\n' > "\$POOL/\$n.meta"
            echo "staged \$n"
        done
        echo "restore: prd-$1-daily-20261119-0320"
        [[ -f "$STUB/backup-second-snapshot" ]] && echo "restore: prd-$1-20261119-1140"
        exit 0
        ;;
esac
exit 0
EOF
    chmod +x "$H/bin/$1-backup"
}

seed_manifest_s3() { # seed_manifest_s3 MODEL...
    local m
    : > "$H/work/manifest.tmp"
    for m in "$@"; do
        jq -nc --arg m "$m" --arg s prod-2 --arg d "$m.demo.data-ui.com" \
              --arg a 203.0.113.10 \
              '{model:$m, site:$s, domain:$d, address:$a}' >> "$H/work/manifest.tmp"
    done
    s3_put_state prod-manifest.jsonl "$H/work/manifest.tmp"
}

rebirth_ready() { # rebirth_ready MODEL... — marker, records, images, ns
    write_site_marker prod-2
    default_fields >/dev/null
    local m
    for m in "$@"; do
        make_ns_fixture "$m"
        # The restore's files destination (the PV path's parent).
        mkdir -p "$H/state/k8s/$m"
        seed_ecr_image "$(expected_ref "$m")"
        seed_release_record "$m"
        make_backup_stub "$m"
    done
}

test_rebirth_inventory_and_missing_manifest() {
    write_site_marker prod-2
    rebirth_ready todos
    # Inventory comes from manifest rows ONLY: an S3 app prefix
    # without a row never resurrects an app.
    mkdir -p "$STUB/s3/data-ui-backups-232168665831/prod-2/scenebook/hourly"
    echo x > "$STUB/s3/data-ui-backups-232168665831/prod-2/scenebook/hourly/prd-scenebook-20261119-1200.dump"
    manifest_row todos todos.demo.data-ui.com 203.0.113.10
    local out
    out=$(REBIRTH_BACKUP_BIN_DIR="$H/bin" run_prod2 rebirth --adopt-data 2>&1) \
        || fail "adopt rebirth failed: $out"
    assert_contains "$out" "App list from the manifest (1): todos"
    assert_not_contains "$out" "scenebook" "S3 prefixes never add apps"
    # A missing manifest stops with the prefix-derived candidates.
    rm -f "$H/state/ops/prod-manifest.jsonl"
    expect_die "no manifest rows" env REBIRTH_BACKUP_BIN_DIR="$H/bin" run_prod2 rebirth --adopt-data
    assert_contains "$LAST_DIE_OUTPUT" "scenebook" "candidates listed for Donnie"
    # REBIRTH_APPS is the explicit override.
    out=$(REBIRTH_APPS=todos REBIRTH_BACKUP_BIN_DIR="$H/bin" \
              run_prod2 rebirth --adopt-data 2>&1) \
        || fail "REBIRTH_APPS override failed: $out"
    assert_contains "$out" "REBIRTH_APPS"
}

test_rebirth_adopt_never_restores() {
    write_site_marker prod-2
    rebirth_ready todos
    manifest_row todos todos.demo.data-ui.com 203.0.113.10
    : > "$STUB/backup.log"; : > "$STUB/r53-changes.log"
    local out
    out=$(REBIRTH_BACKUP_BIN_DIR="$H/bin" run_prod2 rebirth --adopt-data 2>&1) \
        || fail "adopt rebirth failed: $out"
    assert_eq "$(stub_count 'recover' "$STUB/backup.log")" "0" "adopt never restores"
    assert_contains "$out" "Recorded release" "the deploy is recorded-release"
    assert_contains "$out" "rebirth child: todos healthy"
    assert_eq "$(wc -l < "$STUB/r53-changes.log" 2>/dev/null || echo 0)" "0" \
        "a rebirth never touches DNS"
    assert_eq "$(wc -l < "$H/state/ops/prod-manifest.jsonl")" "1" "the manifest row is untouched"
}

test_rebirth_fresh_materializes_and_restores() {
    write_site_marker prod-2
    rebirth_ready todos
    seed_manifest_s3 todos
    rm -f "$H/state/ops/prod-manifest.jsonl"   # a fresh volume has none
    : > "$STUB/backup.log"; : > "$STUB/aws.log"
    local out
    out=$(REBIRTH_BACKUP_BIN_DIR="$H/bin" run_prod2 rebirth --fresh-data 2>&1) \
        || fail "fresh rebirth failed: $out"
    assert_file "$H/state/ops/prod-manifest.jsonl" "the S3 manifest was materialized locally"
    assert_contains "$(cat "$STUB/backup.log")" "recover --check" "preflight checked the backup set"
    assert_contains "$(cat "$STUB/backup.log")" "recover" "the restore ran recover"
    # No S3 manifest rewrite: every prod-manifest s3 cp in the log
    # reads FROM s3 (the local file is never pushed back).
    local line bad=0
    while IFS= read -r line; do
        case "$line" in
            *"cp $H/state/ops/prod-manifest.jsonl s3://"*) bad=1 ;;
        esac
    done < "$STUB/aws.log"
    (( bad == 0 )) || fail "a rebirth never rewrites the S3 manifest (only reads it)"
    # The restore ran in the printed order (kubectl saw the namespace
    # resolution of the restore's prd: address).
    assert_contains "$(cat "$STUB/kubectl.log")" "dataui-todos" "the restore resolved prd:todos"
}

test_rebirth_files_tree_precondition() {
    write_site_marker prod-2
    rebirth_ready todos
    seed_manifest_s3 todos
    rm -f "$H/state/ops/prod-manifest.jsonl"
    # A populated destination files tree dies demanding the explicit
    # override; empty proceeds; the override warns.
    mkdir -p "$H/state/k8s/todos/files"
    echo stale > "$H/state/k8s/todos/files/leftover.txt"
    local out
    out=$(REBIRTH_BACKUP_BIN_DIR="$H/bin" run_prod2 rebirth --fresh-data 2>&1) || true
    assert_contains "$out" "already populated" "the precondition fires"
    assert_contains "$out" "REBIRTH_ALLOW_POPULATED_FILES=1"
    rm -rf "$H/state/k8s/todos/files"
    out=$(REBIRTH_BACKUP_BIN_DIR="$H/bin" run_prod2 rebirth --fresh-data 2>&1) \
        || fail "empty tree proceeds: $out"
    assert_contains "$out" "rebirth child: todos healthy"
}

test_rebirth_first_failure_stops_the_run() {
    write_site_marker prod-2
    rebirth_ready todos books
    # books first in the manifest; its pinned health fails (503) → its
    # child dies mid-sequence; todos (second) must never run.
    manifest_row books books.demo.data-ui.com 203.0.113.10
    manifest_row todos todos.demo.data-ui.com 203.0.113.10
    printf 'books.demo.data-ui.com 503\n' > "$STUB/curl-responses"
    local out
    out=$(REBIRTH_BACKUP_BIN_DIR="$H/bin" run_prod2 rebirth --adopt-data 2>&1) || true
    assert_contains "$out" "the books step failed" "the supervisor reports the failure"
    assert_not_contains "$out" "rebirth child: todos healthy" "the run stops at the first failure"
    assert_contains "$out" "Retry, idempotently" "the retry command is printed"
}

test_rebirth_preflight_dies() {
    write_site_marker prod-2
    default_fields >/dev/null
    expected_ref todos >/dev/null   # creates the production clone
    make_ns_fixture todos
    make_backup_stub todos
    manifest_row todos todos.demo.data-ui.com 203.0.113.10
    : > "$STUB/ecr-images"
    # No record anywhere → the record read dies first.
    expect_die "no release record" env REBIRTH_BACKUP_BIN_DIR="$H/bin" run_prod2 rebirth --adopt-data
    # Record present, image missing → pullability dies (never a build).
    seed_release_record todos
    expect_die "not pullable" env REBIRTH_BACKUP_BIN_DIR="$H/bin" run_prod2 rebirth --adopt-data
    # An incomplete backup set fails --fresh-data preflight.
    seed_ecr_image "$(expected_ref todos)"
    touch "$STUB/backup-recover-fails"
    expect_die "no complete backup set" env REBIRTH_BACKUP_BIN_DIR="$H/bin" \
        run_prod2 rebirth --fresh-data
}

test_rebirth_vip_record_and_missing_bytes() {
    write_site_marker prod-2
    default_fields >/dev/null
    # The production clone + the stable VIP copy (the record's model
    # hash source).
    git -C "$H" clone -q -o public "$H/public.git" repos/production
    mkdir -p "$H/repos/production/models/local" "$H/state/ops"
    printf '(:name vipspec :title "VIP" :version 1.0 :domain vipspec.demo.data-ui.com)\n' \
        > "$H/repos/production/models/local/vipspec.lisp"
    make_ns_fixture vipspec
    make_backup_stub vipspec
    set_fields vipspec name=vipspec "title=VIP" version=1.0 \
        domain=vipspec.demo.data-ui.com repl=NIL
    manifest_row vipspec vipspec.demo.data-ui.com 203.0.113.10
    seed_release_record vipspec vip
    seed_ecr_image "$(jq -r .image "$(release_json vipspec)")"
    # The VIP model bytes are missing at the backup prefix → preflight
    # dies (aws s3 cp fails — the object does not exist).
    expect_die "cannot fetch the VIP model" env REBIRTH_BACKUP_BIN_DIR="$H/bin" \
        run_prod2 rebirth --adopt-data
    # With the model present, the VIP deploy runs through the handoff.
    mkdir -p "$STUB/s3/data-ui-backups-232168665831/prod-2/vipspec/model"
    cp "$H/repos/production/models/local/vipspec.lisp" \
       "$STUB/s3/data-ui-backups-232168665831/prod-2/vipspec/model/vipspec.lisp"
    local out
    out=$(REBIRTH_BACKUP_BIN_DIR="$H/bin" run_prod2 rebirth --adopt-data 2>&1) \
        || fail "VIP rebirth failed: $out"
    assert_contains "$out" "VIP model bytes verified"
    assert_contains "$out" "rebirth child: vipspec healthy"
}

test_rebirth_child_lock_duplication_is_load_bearing() {
    # A rebirth-child envelope carrying a SECOND-OPENED descriptor on
    # the production lock must be rejected by the ownership probe (an
    # independent flock is not ours); the duplicated-descriptor shape
    # the supervisor uses passes. This is the load-bearing proof for
    # exec {fd2}>&"$lockfd".
    git -C "$H" clone -q -o public "$H/public.git" repos/production
    local lock="$H/repos/production.lock"
    local sha
    sha=$(prod_sha)
    # The holder: fd 9, locked. A second open: fd 10.
    exec 9<>"$lock"
    flock -x 9
    exec 10<>"$lock"
    write_site_marker prod-2
    # Second-open in the PROD_LOCK_FD slot → verify rejects it.
    ( eval "$(cli_env_statements)"
      export DEPLOY_HOST=prod-2 K3D_CLUSTER=prod-2 DATA_UI_STATE="$H/state"
      cd "$H/work" || exit 1
      exec "$H/repos/production/scripts/data-ui" \
          "__DATA_UI_REEXEC_V1__" production public master "$sha" \
          9 10 none - - rebirth-child ) > /tmp/reb2.out 2>&1
    local st=$?
    (( st != 0 )) || fail "a second-open PROD_LOCK_FD must be rejected"
    grep -q "held by another process, not by us" /tmp/reb2.out \
        || fail "second-open rejection message missing: $(cat /tmp/reb2.out)"
    # A duplicated descriptor (the supervisor's shape) passes the
    # probes (and dies later on the missing REBIRTH_APP, which is the
    # child's own contract, after verify).
    exec 11>&9
    ( eval "$(cli_env_statements)"
      export DEPLOY_HOST=prod-2 K3D_CLUSTER=prod-2 DATA_UI_STATE="$H/state"
      cd "$H/work" || exit 1
      exec "$H/repos/production/scripts/data-ui" \
          "__DATA_UI_REEXEC_V1__" production public master "$sha" \
          9 11 none - - rebirth-child ) > /tmp/reb1.out 2>&1
    st=$?
    (( st != 0 )) || fail "rebirth-child without REBIRTH_APP must die on its own contract"
    grep -q "REBIRTH_APP" /tmp/reb1.out \
        || fail "the dup'd fd pair passed verify (died on REBIRTH_APP instead): $(cat /tmp/reb1.out)"
    exec 9>&- 10>&- 11>&-
}

# ---------------------------------------------------------------------------
# The backup scripts' recover verbs (standalone, stubbed aws)
# ---------------------------------------------------------------------------

backup_env() { # backup_env — exports for running ops/*-backup
    export HOME="$H"
    export PATH="$H/bin:$ORIG_PATH"
    export STUB="$STUB" AWS_REGION=us-east-1
    mkdir -p "$H/.config"
    printf 'AWS_REGION=us-east-1\nAWS_BUCKET=data-ui-backups-232168665831\n' \
        > "$H/.config/data-ui-backup.env"
    export POOL="$H/state/snapshots"
    mkdir -p "$POOL"
}

s3_seed_object() { # s3_seed_object KEY CONTENT
    local key="$1"
    mkdir -p "$STUB/s3/data-ui-backups-232168665831/$(dirname "$key")"
    printf '%s' "$2" > "$STUB/s3/data-ui-backups-232168665831/$key"
}

run_backlog_backup() { ( backup_env; cd "$H" && exec "$REPO_ROOT/ops/backlog-backup" "$@" 2>&1 ); }
run_scenebook_backup() { ( backup_env; cd "$H" && exec "$REPO_ROOT/ops/scenebook-backup" "$@" 2>&1 ); }

test_recover_backlog_hourly_expiry_and_fallback() {
    # Hourly newest is INCOMPLETE (no files tar) → the older complete
    # hourly wins; with every hourly object expired, the newest
    # complete daily is chosen; with neither tier complete, die.
    s3_seed_object evo-x2/backlog/hourly/prd-backlog-20261119-1100.dump d1
    s3_seed_object evo-x2/backlog/hourly/prd-backlog-20261119-1100.files.tar.gz t1
    s3_seed_object evo-x2/backlog/hourly/prd-backlog-20261119-1100.meta m1
    s3_seed_object evo-x2/backlog/hourly/prd-backlog-20261119-1200.dump d2
    s3_seed_object evo-x2/backlog/hourly/prd-backlog-20261119-1200.meta m2   # no tar
    s3_seed_object evo-x2/backlog/daily/prd-backlog-20261119-0300.dump d3
    s3_seed_object evo-x2/backlog/daily/prd-backlog-20261119-0300.files.tar.gz t3
    s3_seed_object evo-x2/backlog/daily/prd-backlog-20261119-0300.meta m3
    local out
    out=$(run_backlog_backup recover --check) || fail "recover --check failed: $out"
    assert_contains "$out" "prd-backlog-20261119-1100" "newest-incomplete falls back to the older complete triple"
    assert_contains "$out" "tier hourly"
    # --check does not download.
    assert_no_file "$POOL/prd-backlog-20261119-1100.dump" "--check downloads nothing"
    # Hourly expired entirely → the newest complete daily.
    rm -rf "$STUB/s3/data-ui-backups-232168665831/evo-x2/backlog/hourly"
    out=$(run_backlog_backup recover) || fail "daily fallback failed: $out"
    assert_contains "$out" "prd-backlog-20261119-0300 (tier daily"
    assert_contains "$out" "restore: prd-backlog-20261119-0300"
    assert_file "$POOL/prd-backlog-20261119-0300.dump" "recover downloads into the pool"
    # Neither tier complete → die.
    rm -rf "$STUB/s3/data-ui-backups-232168665831/evo-x2/backlog"
    expect_die "no complete recovery set" run_backlog_backup recover
    # RESTORE_SITE retargets reads (the move runbook's shape).
    s3_seed_object evo-x2/backlog/daily/prd-backlog-20261119-0300.dump d3
    s3_seed_object evo-x2/backlog/daily/prd-backlog-20261119-0300.files.tar.gz t3
    s3_seed_object evo-x2/backlog/daily/prd-backlog-20261119-0300.meta m3
    s3_seed_object prod-2/backlog/daily/prd-backlog-20261120-0300.dump d4
    s3_seed_object prod-2/backlog/daily/prd-backlog-20261120-0300.files.tar.gz t4
    s3_seed_object prod-2/backlog/daily/prd-backlog-20261120-0300.meta m4
    out=$(RESTORE_SITE=evo-x2 run_backlog_backup recover --check) || fail "RESTORE_SITE read failed: $out"
    assert_contains "$out" "prd-backlog-20261119-0300" "RESTORE_SITE selects the source site's set"
}

test_recover_scenebook_tiers_and_age_gap() {
    # Daily complete; rpo pair newer → applied on top, with the
    # timestamp-comparison line stating the gap.
    s3_seed_object evo-x2/scenebook/daily/prd-scenebook-daily-20261119-0320.dump d1
    s3_seed_object evo-x2/scenebook/daily/prd-scenebook-daily-20261119-0320.files.tar.gz t1
    s3_seed_object evo-x2/scenebook/daily/prd-scenebook-daily-20261119-0320.meta m1
    s3_seed_object evo-x2/scenebook/rpo/prd-scenebook-20261119-1130.dump d2
    s3_seed_object evo-x2/scenebook/rpo/prd-scenebook-20261119-1130.meta m2
    local out
    out=$(run_scenebook_backup recover) || fail "scenebook recover failed: $out"
    assert_contains "$out" "age gap: daily files 20261119-0320 + rpo database 20261119-1130" \
        "the age gap is stated with both timestamps"
    assert_contains "$out" "older than the database"
    assert_contains "$out" "restore: prd-scenebook-daily-20261119-0320" "daily first"
    assert_contains "$out" "restore: prd-scenebook-20261119-1130" "rpo applied on top"
    # The rpo pair is incomplete (no meta) → the daily alone.
    rm "$STUB/s3/data-ui-backups-232168665831/evo-x2/scenebook/rpo/prd-scenebook-20261119-1130.meta"
    out=$(run_scenebook_backup recover) || fail "incomplete-rpo recover failed: $out"
    assert_not_contains "$out" "restore: prd-scenebook-20261119-1130" "incomplete rpo is skipped"
    # A same-timestamp daily/rpo pair is never merged: the rpo is not
    # "newer", so the daily alone is restored.
    s3_seed_object evo-x2/scenebook/rpo/prd-scenebook-20261119-0320.dump d5
    s3_seed_object evo-x2/scenebook/rpo/prd-scenebook-20261119-0320.meta m5
    out=$(run_scenebook_backup recover) || fail "same-ts recover failed: $out"
    assert_contains "$out" "not newer" "same-timestamp pairs are stated, not merged"
    assert_not_contains "$out" "restore: prd-scenebook-20261119-0320$" || true
    # grep-precision: exactly one restore line (the daily).
    assert_eq "$(grep -c '^restore: ' <<<"$out")" "1" "one restore line: the daily triple alone"
    # No complete daily → die.
    rm -rf "$STUB/s3/data-ui-backups-232168665831/evo-x2/scenebook"
    expect_die "no complete daily" run_scenebook_backup recover
}

# ---------------------------------------------------------------------------
# The AWS operator script's fence (Appendix A11)
# ---------------------------------------------------------------------------

source_aws_fns() { # extract testable functions from ops/prod-2-aws.sh
    # Only the multi-line functions (their bodies end with a ^} line);
    # die and the A0 constants are defined locally — extracting the
    # top-level block would run the AWS_PROFILE guard and the flock.
    # The functions call `aws` directly, so the stub bin leads PATH.
    export PATH="$H/bin:$PATH"
    eval "$(sed -n '/^aws_scalar()/,/^}/p; /^fn_scalar()/,/^}/p;
                    /^golden_ami()/,/^}/p; /^resolve_golden_ami()/,/^}/p;
                    /^lane_data_volume()/,/^}/p; /^fence_old_instances()/,/^}/p' \
                   "$REPO_ROOT/ops/prod-2-aws.sh")"
    die() { echo "prod-2-aws.sh: $*" >&2; exit 1; }
    NAME=data-ui-prod-2
    ARCH="${ARCH:-arm64}"
    T="{Key=Site,Value=prod-2},{Key=Project,Value=data-ui-prod-2}"
}

test_aws_fence_stops_every_match() {
    # OLD_ID present plus a second active instance: both are stopped
    # (and retired), and the run fails explicitly on the unexpected
    # multiplicity — never silently proceeds.
    source_aws_fns
    cat > "$STUB/aws-describe-instances" <<'EOF'
i-old	running
i-second	running
EOF
    local out
    out=$(fence_old_instances 2>&1); st=$?
    (( st != 0 )) || fail "two running matches must fail the fence"
    assert_contains "$out" "2 non-terminated instances" "the multiplicity is named"
    assert_contains "$(cat "$STUB/aws.log")" "stop-instances --instance-ids i-old"
    assert_contains "$(cat "$STUB/aws.log")" "stop-instances --instance-ids i-second"
    assert_contains "$(cat "$STUB/aws.log")" "create-tags"
}

test_aws_fence_mixed_shutting_down_waits() {
    source_aws_fns
    cat > "$STUB/aws-describe-instances" <<'EOF'
i-old	stopped
i-straggler	shutting-down
EOF
    fence_old_instances >/dev/null 2>&1
    assert_contains "$(cat "$STUB/aws.log")" "wait instance-terminated --instance-ids i-straggler" \
        "a shutting-down straggler is waited out, not stopped"
    assert_contains "$(cat "$STUB/aws.log")" "stop-instances --instance-ids i-old" \
        "the stopped instance is still stopped and retired"
}

test_aws_fence_waiter_failure_dies() {
    source_aws_fns
    printf 'i-x\tshutting-down\n' > "$STUB/aws-describe-instances"
    touch "$STUB/aws-wait-fails"
    local out
    out=$(fence_old_instances 2>&1); st=$?
    (( st != 0 )) || fail "a waiter failure must die"
    assert_contains "$out" "i-x failed to terminate; resolve by hand" \
        "the dying message names the instance"
}

test_aws_no_validated_ami_or_snapshot_dies() {
    # GOLDEN_AMI and SNAP are checked like every scalar: empty/None
    # dies, never reaching run-instances / create-volume.
    source_aws_fns
    # No validated AMI in the stub's describe-images answer (the aws
    # stub logs and exits 0 with no output → empty).
    local out
    out=$(golden_ami); assert_eq "$out" "" "stub returns no AMI"
    # The snapshot lane with no snapshot dies rather than creating
    # from None (lane_data_volume consults describe-snapshots, which
    # the stub answers with nothing).
    out=$(lane_data_volume snapshot data-ui-prod-2-data us-east-1a 2>&1); st=$?
    (( st != 0 )) || fail "the snapshot lane must die without a snapshot"
    assert_contains "$out" "no completed snapshot" "never create-volume from None"
    # The fresh lane never creates over an existing volume.
    : > "$STUB/aws-describe-volumes" 2>/dev/null || true
    out=$(lane_data_volume fresh data-ui-prod-2-data us-east-1a 2>&1) || true
    : # (the stub has no describe-volumes fixture; the fresh lane's
      # refuse-existing check reads nothing → creates — not asserted
      # here; the die-on-None cases above are the scalar gates)
}

test_aws_launch_names_its_ami() {
    # The first launch (Validated comes only after Phase C) and the
    # scratch drill (which EARNS Validated=true) must be able to name
    # the just-baked AMI: the validated-only lookup would die — or, on
    # a re-bake, silently return the older validated image instead of
    # the one under test.
    source_aws_fns
    # An explicit AMI that describes as available+arm64: used verbatim.
    printf 'ami-new\n' > "$STUB/aws-describe-images"
    resolve_golden_ami ami-new
    assert_eq "$GOLDEN_AMI" "ami-new" "the explicit AMI is used, validated or not"
    assert_contains "$(cat "$STUB/aws.log")" "Name=architecture,Values=arm64" \
        "the explicit AMI is availability/arch checked, not trusted"
    # An explicit AMI the filters reject (nothing described): dies.
    local out
    : > "$STUB/aws-describe-images"
    out=$(resolve_golden_ami ami-bogus 2>&1); st=$?
    (( st != 0 )) || fail "an undescribable explicit AMI must die"
    assert_contains "$out" "explicit AMI ami-bogus" "the die names the AMI"
    # Neither explicit nor validated: dies with the --ami hint.
    out=$(resolve_golden_ami 2>&1); st=$?
    (( st != 0 )) || fail "no explicit AMI and no validated AMI must die"
    assert_contains "$out" "no validated golden AMI" "with the bake/validate hint"
    assert_contains "$out" "--ami" "and the escape hatch"
}

test_aws_fn_scalar_runs_script_functions() {
    # cmd_launch/replace wrap SCRIPT FUNCTIONS (lane_data_volume,
    # launch_instance, instance_ip). Wrapped in aws_scalar those ran
    # `aws lane_data_volume ...` — ParamValidation on a real CLI (the
    # first live launch died on it); the stub hides it by exiting 0
    # empty, which is why the suite stayed green. fn_scalar runs the
    # function and checks the output like every scalar.
    source_aws_fns
    probe_fn() { echo "$1"; }
    assert_eq "$(fn_scalar probe probe_fn ok)" "ok" "a succeeding function's output passes through"
    probe_fn() { return 1; }
    local out; out=$(fn_scalar probe probe_fn 2>&1); st=$?
    (( st != 0 )) || fail "a failing function must die"
    assert_contains "$out" "probe: call failed" "labeled die"
    probe_fn() { :; }
    out=$(fn_scalar probe probe_fn 2>&1); st=$?
    (( st != 0 )) || fail "empty output must die"
    assert_contains "$out" "empty/None" "empty dies"
}

source_host_fns() { # extract testable functions from ops/prod-2-host.sh
    # Same extraction idiom as source_aws_fns: only the named
    # top-level functions, never the bottom-case dispatch.
    eval "$(sed -n '/^dockerfile_ros_targets()/,/^}/p; /^nvme_by_id()/,/^}/p' \
                    "$REPO_ROOT/ops/prod-2-host.sh")"
    die() { echo "prod-2-host.sh: $*" >&2; exit 1; }
}

test_host_ros_replay_is_clean() {
    # Two bake regressions, one replay: Dockerfile lines
    # "RUN ulimit -n N && ros install <rest>" must play back as ONE
    # clean host-side target, in FILE ORDER. Bake #1 died on shape —
    # the && chain, Docker quoting, and the unexpanded ${SBCL_VERSION}
    # reached roswell as literal argv (UNDEFINED-FUNCTION). Bake #2
    # died on order — sort -u alphabetized macnod/dc-eclectic ahead of
    # macnod/dc-time, but the github local-projects systems resolve
    # each other, not the quicklisp dist ("specific order important
    # here" in the Dockerfile), so eclectic's load died
    # SYSTEM-NOT-FOUND.
    source_host_fns
    local d="$H/Dockerfile.fixture"
    cat > "$d" <<'EOF'
ARG SBCL_VERSION="2.5.10"
RUN ulimit -n 1048576 && ros install "sbcl-bin/${SBCL_VERSION}" && ros use "sbcl-bin/${SBCL_VERSION}"
RUN ulimit -n 1048576 && ros install 3bmd
RUN ulimit -n 1048576 && ros install macnod/dc-time
RUN ulimit -n 1048576 && ros install macnod/dc-eclectic
RUN ulimit -n 1048576 && ros install hunchentoot && ros use hunchentoot
RUN ulimit -n 1048576 && ros install macnod/dc-time
RUN ulimit -n 1048576 && ros run -- --eval "(ql:register-local-projects)" --quit
RUN ulimit -n 1048576 && mkdir -p /app/shared-files
EOF
    local out
    out=$(dockerfile_ros_targets "$d")
    assert_eq "$out" "3bmd
macnod/dc-time
macnod/dc-eclectic
hunchentoot" "the plain targets in FILE order — dc-eclectic stays after dc-time (alphabetical flips them), the duplicate dc-time keeps one, sbcl-bin is skipped (installed from the parsed ARG), the && tail is cut, ros run / mkdir never harvested"
    # Belt and braces on the exact argv shapes that crashed bake #1.
    assert_not_contains "$out" '${' "no unexpanded Docker ARG text"
    assert_not_contains "$out" '"' "no Docker quoting carried into host argv"
    assert_not_contains "$out" '&&' "no shell chain carried into host argv"
}

test_host_nvme_by_id_keeps_the_vol_prefix() {
    # The first live launch's post-image died looking for
    # nvme-Amazon_Elastic_Block_Store_0df2... — the volume-id strip
    # removed the vol prefix too, but udev KEEPS it (verified on a
    # graviton instance: nvme-Amazon_Elastic_Block_Store_vol0df2...).
    # The constructed path never existed, so the settle hint in the
    # old die message was a red herring.
    source_host_fns
    assert_eq "$(nvme_by_id vol-0df23047dcf024830)" \
              "/dev/disk/by-id/nvme-Amazon_Elastic_Block_Store_vol0df23047dcf024830" \
              "vol- id maps to the udev name (vol kept, dash stripped)"
    assert_eq "$(nvme_by_id 0df23047dcf024830)" \
              "/dev/disk/by-id/nvme-Amazon_Elastic_Block_Store_vol0df23047dcf024830" \
              "an id already lacking the prefix maps to the same path"
    # no dash survives inside the udev serial (the part after the last _)
    local serial; serial=$(nvme_by_id vol-0d-f2 | xargs basename); serial="${serial##*_}"
    [[ "$serial" == vol* && "$serial" != *-* ]] \
        || fail "udev serial malformed: $serial (wanted vol-prefixed, dash-free)"
}

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

# Filters: CLI args (name substrings) when given; an inherited
# TEST_FILTERS environment variable (space-separated) otherwise; neither
# means run everything. TEST_FILTERS is always a real array afterward —
# an unset/scalar value here crashed t() under set -u, and the previous
# form ("${@:-}") collapsed "no args" into a one-empty-string array
# whose empty substring matched EVERY test — silently disabling
# filtering.
_filters="${TEST_FILTERS:-}"
TEST_FILTERS=()
if (( $# > 0 )); then
    TEST_FILTERS=("$@")
elif [[ -n "$_filters" ]]; then
    read -ra TEST_FILTERS <<< "$_filters"
fi
main() {
    mkdir -p /tmp/data-ui-checkout-tests 2>/dev/null || true
    echo "checkout-tests.sh — $(date -u +%FT%TZ)"
    echo "repo under test: $REPO_ROOT"

    t sources_default_public_master
    t sources_explicit_private
    t sources_slash_branch
    t sources_switch_keeps_independent_branches
    t sources_tags_stay_local

    t parsing_deploy_flags
    t parsing_model_operand_grammar
    t parsing_name_mismatch_before_mutation

    t invocation_from_root_and_tmp_and_dirty_dev

    t bootstrap_missing_checkout_clones
    t bootstrap_nonrepo_nonempty_fails_untouched
    t bootstrap_wrong_url_and_origin_only
    t bootstrap_only_mode

    t selection_noop_and_ff
    t selection_dirty_aborts_without_destruction
    t selection_ahead_diverged_rewound_nonff
    t selection_dirty_aborts_under_dryrun_and_modelfile
    t selection_ignored_files_pass
    t selection_git_env_contamination
    t selection_missing_remote_branch

    t envelope_body_runs_once_and_status_propagates
    t envelope_wrong_root_host_sha_lock
    t envelope_cwd_of_selected_checkout_rejected

    t lockprobe_free_lock_and_wrong_file
    t lockprobe_held_by_another_process

    t contention_serializes_and_lock_survives_exec
    t contention_independent_roles_and_root
    t contention_holder_death_with_surviving_child

    t remote_production_launcher_arm
    t remote_bootstrap_launcher_arm
    t remote_neither_launcher_fails
    t remote_metacharacter_safety_and_allowlist
    t remote_no_local_selection_under_dryrun
    t remote_delete_forwards_confirmation_over_ssh_t

    t handoff_relative_and_absolute_resolution
    t handoff_invalid_inputs_die_before_mutation
    t handoff_stable_copy_changes_only_after_both_gates
    t handoff_failure_at_stage_leaves_stable_identical
    t handoff_tags_come_from_envelope_sha
    t handoff_dry_run_leaves_incoming_identical

    t buildcontext_canonical_is_curated
    t buildcontext_tracked_models_local_refuses
    t buildcontext_vip_context_contains_exactly_its_handoff
    t buildcontext_vip_collision_refused_before_compile
    t buildcontext_adoption_sequence
    t buildcontext_reexec_preserves_worker_env
    t datachannel_no_interpolated_evals

    t recovery_post_rollout_failure_writes_record
    t recovery_record_drives_delete_and_route_removal
    t hop_probe_polls_past_transient_refusal
    t hop_probe_unreachable_band_fails_closed
    t leftovers_move_to_orphaned_candidates
    t remote_transfer_never_writes_production_before_clean_check

    t delete_no_fetch_no_clean_required
    t delete_cds_before_identity_from_any_cwd
    t delete_explicit_model_file_via_its_own_envelope
    t delete_validates_identity_before_snapshot_or_confirmation
    t delete_legacy_identity_migration_then_delete

    t drain_query_real_postgres

    t routing_preserved_and_local_defaults
    t routing_inner_arms_enter_staging_first
    t snapshot_copy_triple_and_retarget
    t routing_named_repl_db_psql_run_no_npm
    t routing_active_profile_repl_refusal_mutates_nothing
    t routing_wrong_hosts_fail_before_mutation

    t quiesce_failed_reload_aborts
    t quiesce_absent_profile_and_stopped_unit
    t quiesce_unexposed_profile_skips_reload
    t quiesce_state_ordering
    t quiesce_target_sha_written_after_fetch
    t quiesce_drain_blocked_worker_completes
    t quiesce_drain_timeout_stranded_row
    t quiesce_generate_in_flight

    t activation_edemo_refusal
    t activation_extra_options_fail
    t activation_originally_unexposed_never_exposed
    t activation_no_exposure_after_failed_health
    t activation_protocol_mismatch_blocks_expose
    t activation_full_cycle_and_fast_path
    t activation_staging_then_production_order

    t update_full_flow_default_private
    t update_explicit_source_and_generated_files_ignored
    t update_lockfile_required
    t update_two_consecutive_builds
    t update_gate_stops_at_first_failure
    t update_no_lifecycle_verbs
    t update_untouched_disabled_profiles
    t update_activating_profile_aborts

    t prewarm_failure_installs_nothing
    t prewarm_elapsed_printed
    t healthgate_slow_unit_passes_within_deadline
    t healthgate_invocation_change_mid_poll_fails
    t healthgate_pre_restart_lines_cannot_satisfy
    t journal_identity_line_matches_gate_contract

    t sd_timer_combinations_restored_exactly
    t sd_unknown_timer_states_fail_untouched
    t sd_reset_service_states
    t sd_reset_running_at_final_check_aborts
    t sd_missing_lock_root_fails
    t sd_npm_failure_no_ops
    t sd_install_failure_reports_changed_destinations
    t sd_daemon_reload_failure
    t sd_restoration_failure_after_failed_stop

    t reset_noargs_under_held_lock_refuses
    t reset_stale_installed_script_installs_nothing

    t wrapper_profile_run_string_order
    t sbcl_two_checkout_load_contract

    t site_marker_wins_and_invalid_dies
    t site_defaults_armed_by_marker
    t site_envelope_host_check_follows_marker
    t site_production_only_boundary
    t site_production_only_boundary_routing_shapes
    t forwarder_entry_env_membership
    t registry_hit_skips_compile_and_build
    t registry_miss_builds_imports_pushes
    t registry_push_failure_warns_and_succeeds
    t registry_pull_failure_falls_back_to_build
    t registry_rebuild_logs_in_builds_and_pushes
    t registry_import_failure_aborts
    t registry_vip_bytes_change_the_tag
    t registry_dry_run_purity
    t registry_absent_legacy_image_name

    t gate_explicit_pins_and_never_routes_dns
    t guard_wildcard_shadowed_name_and_overrides
    t summary_publication_verdicts

    t release_record_gates_and_shape
    t recorded_release_pull_or_die
    t recorded_release_drives_the_render

    t cutover_refusals
    t cutover_happy_path_and_rerun
    t cutover_half_finished_converges
    t cutover_route53_failure_leaves_no_row
    t delete_midsequence_failure_rerun_converges
    t delete_dns_only_when_its_ours
    t delete_domain_evidence_union

    t rebirth_inventory_and_missing_manifest
    t rebirth_adopt_never_restores
    t rebirth_fresh_materializes_and_restores
    t rebirth_files_tree_precondition
    t rebirth_first_failure_stops_the_run
    t rebirth_preflight_dies
    t rebirth_vip_record_and_missing_bytes
    t rebirth_child_lock_duplication_is_load_bearing

    t recover_backlog_hourly_expiry_and_fallback
    t recover_scenebook_tiers_and_age_gap

    t aws_fence_stops_every_match
    t aws_fence_mixed_shutting_down_waits
    t aws_fence_waiter_failure_dies
    t aws_no_validated_ami_or_snapshot_dies
    t aws_launch_names_its_ami
    t aws_fn_scalar_runs_script_functions
    t host_ros_replay_is_clean
    t host_nvme_by_id_keeps_the_vol_prefix

    echo
    echo "=========================================="
    echo "checkout-tests: ${PASSED} passed, ${FAILED} failed, ${SKIPPED} skipped (of ${TOTAL})"
    echo "=========================================="
    [[ -n "${H_OLD:-}" ]] && rm -rf "$H_OLD" 2>/dev/null || true
    (( FAILED == 0 ))
}

main
