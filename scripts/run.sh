#!/bin/bash

SCRIPT_NAME=$(basename $0)
ACTION=$1

export ADMIN_PASSWORD="admin-password-1"
export HTTP_HOST="127.0.0.1"
export DB_HOST="127.0.0.1"
export DB_INIT_TEMPLATE="tests/init-template.sql"
export DB_INIT_SQL="tests/init.sql"
export DB_NAME="dataui"
export DB_USER="dataui"
export DB_PASSWORD="dataui-password"
export PGPASSWORD="$DB_PASSWORD"
export DOCUMENT_ROOT="tests/shared-files/"
export FS_TEMP_DIRECTORY="tests/temp-uploads/"

TIMEOUT=60
SLEEP_INTERVAL=1
SLEEP_MAX=10

sed "s/:database_name:/$DB_NAME/" $DB_INIT_TEMPLATE > $DB_INIT_SQL

function usage {
    echo "Usage:"
    echo "    $SCRIPT_NAME {command}"
    echo
    echo "Commands:"
    echo "repl     Starts a Common Lisp REPL (using Roswell) that loads the"
    echo "         test file at $TEST_FILE, but doesn't run the tests. You"
    echo "         can connect to the REPL with the a client like Slime at"
    echo "         the reported Swank port."
    echo "db       Start the PostgreSQL database and connect to it."
    echo "psql     Connect to the PostgreSQL database for the REPL."
    echo "tests    Runs all the tests and reports success or failure."
    echo "docs     Generates the REAME file."
    echo "stop     Stops the database container and removes it. This is"
    echo "         normally not necessary."
    echo "title    Get the default model's title"
    echo "debug    Runs the tests, printing all output from postgres and"
    echo "         docker compose."
    echo "deploy   Builds the default model's Docker image, generates the"
    echo "         Kubernetes manifests, deploys the app to k3d, and routes"
    echo "         the model's domain to the app via HAProxy. Requires a"
    echo "         clean git tree and sudo (for HAProxy). Set DRY_RUN=1 to"
    echo "         stop after generating the manifests. When run on a"
    echo "         machine other than \$DEPLOY_HOST (default evo-x2), pushes"
    echo "         the current branch and tag to origin and re-runs the"
    echo "         deploy on \$DEPLOY_HOST over ssh, in the deploy clone at"
    echo "         \$DEPLOY_CHECKOUT (default \$HOME/deploy/data-ui)."
    echo "         Deployment state (manifests, ports.lock, secrets) lives"
    echo "         outside the repo in \$DEPLOY_STATE_DIR (default"
    echo "         ~/.local/state/data-ui-deploy)."
    echo "help     Displays this help text."
}

function wait_for_postgres {
    local quiet="$1"
    local interval=$SLEEP_INTERVAL
    SECONDS=0
    if [[ -z "$quiet" ]]; then
        echo "Waiting for postgres..."
        until docker compose -f "$DB_DOCKER_COMPOSE" exec -it "$DB_CONTAINER" \
                     pg_isready -U postgres; do
            if (( SECONDS >= TIMEOUT )); then
                echo "ERROR: Timed out waiting for PostgreSQL. Waited $SECONDS seconds." >&2
                return 1
            fi
            sleep $interval
            if (( interval < SLEEP_MAX )); then
                interval=$(( interval + 1 ))
            fi
        done
    else
        until docker compose -f "$DB_DOCKER_COMPOSE" exec -it "$DB_CONTAINER" \
                     pg_isready -U postgres &>/dev/null; do
            if (( SECONDS >= TIMEOUT )); then
                echo "ERROR: Timed out waiting for PostgreSQL. Waited $SECONDS seconds." >&2
                return 1
            fi
            sleep $interval
            if (( interval < SLEEP_MAX )); then
                interval=$(( interval + 1 ))
            fi
        done
    fi
}

function start_postgres_quietly {
    echo
    echo "Starting PostgreSQL"
    if ! docker compose -f "$DB_DOCKER_COMPOSE" up -d &>/dev/null; then
        echo "ERROR: Failed to start PostgreSQL with docker compose" >&2
        return 1
    fi
    wait_for_postgres quiet
    echo "PostgreSQL is up on port ${DB_PORT}"
    sleep 2
}

function start_postgres {
    echo
    echo "Starting PostgreSQL"
    echo "docker compose -f \"${DB_DOCKER_COMPOSE}\" up -d"
    if ! docker compose -f "$DB_DOCKER_COMPOSE" up -d; then
        echo "ERROR: Failed to start PostgreSQL with docker compose" >&2
        return 1
    fi
    wait_for_postgres
    echo "PostgreSQL is up on port ${DB_PORT}"
    sleep 2
}

function initialize_database_quietly {
    echo
    echo -n "Initializing database... "
    psql -h $DB_HOST -p $DB_PORT -d $DB_NAME -U $DB_USER \
         -f "$DB_INIT_SQL" >&/dev/null
    echo "Done."
}

function initialize_database {
    echo
    echo "Initializing database"
    psql -h $DB_HOST -p $DB_PORT -d $DB_NAME -U $DB_USER \
         -f "$DB_INIT_SQL"
}

function start_psql {
    echo
    echo "Connecting to database $DB_NAME at $BD_PORT"
    psql -P pager=off -h $DB_HOST -p $DB_PORT -d $DB_NAME -U $DB_USER
}

function start_repl {
    echo
    echo "Loading data-ui and starting Swank server on port $SWANK_PORT."
    ros run -- \
        --eval "(require :data-ui)" \
        --eval "(in-package :data-ui)" \
        --eval "(init)"
}

function run_tests {
    echo
    echo "Running tests"
    ros run -- --disable-debugger --load "$TEST_FILE" --quit
    export REPORT=$( [[ $? -eq 0 ]] && echo "PASS" || echo "FAIL" )
}

function generate_readme {
    ros run -- --disable-debugger \
        --eval '(asdf:load-system :rbac :force t)' \
        --load "$TEST_FILE" \
        --eval "(rbac::generate-readme)" --quit
    if [[ $? -eq 0 ]]; then
        echo "Generated README.md"
    else
        echo "Failed to generate README.md"
    fi
}

function compile {
    ros run -- --disable-debugger \
        --eval "(push (uiop:getcwd) asdf:*central-registry*)" \
        --eval "(ql:register-local-projects)" \
        --eval "(asdf:load-system :rbac :force t)" \
        --load "$TEST_FILE" \
        --quit
    if [[ $? -eq 0 ]]; then
        echo
        echo "Compilation successful."
    else
        echo
        echo "Compilation failed."
    fi
}

function stop_database {
    echo "Stopping and removing database container"
    echo "docker compose -f \"${DB_DOCKER_COMPOSE}\" down --remove-orphans --volumes"
    docker compose -f "$DB_DOCKER_COMPOSE" down --remove-orphans --volumes
}

function stop_database_quietly {
    echo "Stopping and removing database container"
    docker compose -f "$DB_DOCKER_COMPOSE" down &>/dev/null
}

function export_repl_environment {
    export DB_CONTAINER="pg-data-ui-repl"
    export DB_DOCKER_COMPOSE="tests/docker-compose-${DB_CONTAINER}.yaml"
    export DB_PORT="5444"
    export LOG_FILE="tests/repl.log"
    export LOG_SEVERITY="DEBUG"
    export SWANK_PORT="4010"
    export HTTP_PORT="8081"
    export RUN_TESTS="false"
}

function export_test_environment {
    export DB_CONTAINER="pg-data-ui-test"
    export DB_DOCKER_COMPOSE="tests/docker-compose-${DB_CONTAINER}.yaml"
    export DB_PORT="5445"
    export LOG_FILE="tests/test.log"
    export LOG_SEVERITY="DEBUG"
    export SWANK_PORT="4011"
    export HTTP_PORT="8082"
    export RUN_TESTS="true"
}

function export_docs_environment {
    export SKIP_DB="true"
    export LOG_FILE="tests/docs.log"
    unset SWANK_PORT &>/dev/null
    export RUN_TESTS="false"
}

function get_model_field {
    ros run -- \
        --load "lisp/deployment.lisp" \
        --eval '(in-package :deployment)' \
        --eval "(top-level-model-field \"${FIELD_NAME}\")" \
        --quit
}

#
# Deployment
#

function export_deploy_environment {
    export DEPLOY_ENV="demo"
    # The machine that hosts the k3d cluster and HAProxy. Must be both a
    # valid ssh destination (see ~/.ssh/config) and the machine's hostname,
    # since the latter decides local vs remote deploy.
    export DEPLOY_HOST="${DEPLOY_HOST:-evo-x2}"
    # Deploy clone on DEPLOY_HOST used by remote deploys; kept separate
    # from any development checkout so a remote deploy never disturbs
    # work in progress. $HOME is escaped so it expands on the remote.
    export DEPLOY_CHECKOUT="${DEPLOY_CHECKOUT:-\$HOME/deploy/data-ui}"
    # Machine-local deployment state (rendered manifests, ports.lock,
    # per-instance secrets). Deliberately outside the repository: it is
    # derived output and cluster-state cache, not source.
    export DEPLOY_STATE_DIR="${DEPLOY_STATE_DIR:-$HOME/.local/state/data-ui-deploy}"
    export K3D_CLUSTER="${K3D_CLUSTER:-evo-x2}"
    export IMAGE_REPO="macnod/data-ui"
    export TEMPLATES_DIR="deploy/templates"
    export PORTS_LOCK="${DEPLOY_STATE_DIR}/ports.lock"
    export PORT_MIN=30300
    export HAPROXY_CFG="/etc/haproxy/haproxy.cfg"
    export HAPROXY_CONF_D="/etc/haproxy/conf.d"
    export HAPROXY_MAP="/etc/haproxy/data-ui.map"
    export HAPROXY_DEFAULTS="/etc/default/haproxy"
}

function die {
    echo "ERROR: $1" >&2
    exit 1
}

function model_field {
    FIELD_NAME="$1" get_model_field 2>/dev/null | tail -1
}

function require_clean_tree {
    if [[ -n "$(git status --porcelain)" ]]; then
        die "Working tree is not clean. Commit or stash your changes first."
    fi
}

function export_deploy_db_environment {
    # Throwaway database for the deploy-time model compile. Uses its own
    # container and port so it never collides with a running repl (5444)
    # or test (5445) database.
    export DB_CONTAINER="pg-data-ui-deploy"
    export DB_DOCKER_COMPOSE="tests/docker-compose-${DB_CONTAINER}.yaml"
    export DB_PORT="5446"
    export LOG_FILE="tests/deploy-compile.log"
}

function compile_default_model {
    echo
    echo "Compiling the default model..."
    # Runs the compilation phase of SET-MODEL (validation, SQL generation,
    # lambda compilation) against a throwaway database, skipping the
    # cluster-facing side effects (CREATE-TABLES, START-WEB-SERVER, etc).
    # COMPILE-MODEL merges in *base-model* itself, but its stage-1 needs
    # *rbac* initialized, hence the database and INIT-DATABASE.
    export_deploy_db_environment
    start_postgres_quietly
    initialize_database_quietly
    local status=0
    ros run -- --disable-debugger \
        --eval "(require :data-ui)" \
        --eval "(in-package :data-ui)" \
        --eval "(init-database)" \
        --eval '(let ((model (with-open-file (in "models/default-model.lisp") (read in)))) (top-level-settings model) (compile-model (getf model :types)) (format t "Model compiled OK.~%"))' \
        --quit || status=1
    stop_database_quietly
    (( status == 0 )) \
        || die "Default model failed to compile; aborting deployment."
}

function gather_deploy_facts {
    echo "Reading identity from the default model..."
    NAME=$(model_field name)
    TITLE=$(model_field title)
    VERSION=$(model_field version)
    DOMAIN=$(model_field domain)
    REPL=$(model_field repl)
    # The model compile step already requires these; the checks here only
    # guard against get_model_field plumbing failures.
    [[ -n "$NAME" && "$NAME" != "NIL" ]] || die "Model has no :name."
    [[ -n "$TITLE" && "$TITLE" != "NIL" ]] || die "Model has no :title."
    [[ -n "$VERSION" && "$VERSION" != "NIL" ]] || die "Model has no :version."
    [[ -n "$DOMAIN" && "$DOMAIN" != "NIL" ]] || die "Model has no :domain."
    SHORT_HASH=$(git rev-parse --short HEAD)
    TAG="${NAME}-${VERSION}-${SHORT_HASH}"
    IMAGE="${IMAGE_REPO}:${TAG}"
    RELEASE_NAME="${TITLE} ${TAG}"
    NAMESPACE="dataui-${NAME}"
    OUT_DIR="${DEPLOY_STATE_DIR}/${NAME}/releases/${VERSION}-${SHORT_HASH}"
    SECRETS_ENV="${DEPLOY_STATE_DIR}/${NAME}/secrets.env"
    echo "  name:      $NAME"
    echo "  title:     $TITLE"
    echo "  version:   $VERSION"
    echo "  domain:    $DOMAIN"
    echo "  repl:      $REPL"
    echo "  tag:       $TAG"
    echo "  image:     $IMAGE"
    echo "  manifests: $OUT_DIR"
}

function create_deploy_tag {
    if git rev-parse -q --verify "refs/tags/${TAG}" >/dev/null; then
        if [[ "$(git rev-parse "${TAG}^{commit}")" != "$(git rev-parse HEAD)" ]]; then
            die "Tag ${TAG} exists but points at a different commit."
        fi
        echo "Tag ${TAG} already exists for HEAD; reusing it."
    else
        git tag -a "$TAG" -m "$RELEASE_NAME"
        echo "Created tag ${TAG} (\"${RELEASE_NAME}\")."
    fi
}

function build_image {
    echo
    echo "Building Docker image ${IMAGE}"
    docker build -t "$IMAGE" . || die "Docker build failed."
    echo
    echo "Importing image into k3d cluster ${K3D_CLUSTER}"
    k3d image import "$IMAGE" -c "$K3D_CLUSTER" \
        || die "k3d image import failed."
}

function assign_node_port {
    mkdir -p "$DEPLOY_STATE_DIR"
    touch "$PORTS_LOCK"
    NODE_PORT=$(awk -v n="$NAME" -v e="$DEPLOY_ENV" \
                    '$1==n && $2==e {print $3}' "$PORTS_LOCK")
    if [[ -n "$NODE_PORT" ]]; then
        echo "Reusing NodePort ${NODE_PORT} from ${PORTS_LOCK}."
        return
    fi
    # The lock file is only a cache; the live Service is the source of
    # truth. If the instance is already deployed, recover its NodePort
    # rather than assigning a new one.
    NODE_PORT=$(kubectl get svc -n "$NAMESPACE" "dataui-${NAME}" \
                    -o jsonpath='{.spec.ports[0].nodePort}' 2>/dev/null)
    if [[ -n "$NODE_PORT" ]]; then
        echo "$NAME $DEPLOY_ENV $NODE_PORT" >> "$PORTS_LOCK"
        echo "Recovered NodePort ${NODE_PORT} from the live Service" \
             "(recorded in ${PORTS_LOCK})."
        return
    fi
    local used_lock used_k8s port
    used_lock=$(awk '{print $3}' "$PORTS_LOCK")
    used_k8s=$(kubectl get svc -A \
                   -o jsonpath='{.items[*].spec.ports[*].nodePort}' \
                   2>/dev/null | tr ' ' '\n')
    port=$PORT_MIN
    while printf '%s\n%s\n' "$used_lock" "$used_k8s" \
            | grep -qx "$port"; do
        port=$(( port + 1 ))
    done
    NODE_PORT=$port
    echo "$NAME $DEPLOY_ENV $NODE_PORT" >> "$PORTS_LOCK"
    echo "Assigned new NodePort ${NODE_PORT} (recorded in ${PORTS_LOCK})."
}

function get_cluster_secret {
    kubectl get secret -n "$NAMESPACE" "dataui-${NAME}-secrets" \
            -o jsonpath="{.data.$1}" | base64 -d
}

function ensure_instance_secrets {
    if [[ -f "$SECRETS_ENV" ]]; then
        echo "Reusing instance secrets from ${SECRETS_ENV}."
        source "$SECRETS_ENV"
        return
    fi
    mkdir -p "$(dirname "$SECRETS_ENV")"
    # The secrets file is only a cache; the live Secret is the source of
    # truth. Never generate fresh credentials for an existing instance:
    # the PostgreSQL volume keeps the old password, and new credentials
    # would break database auth.
    if kubectl get secret -n "$NAMESPACE" "dataui-${NAME}-secrets" \
               &>/dev/null; then
        {
            echo "DB_PASSWORD=$(get_cluster_secret db-password)"
            echo "ADMIN_PASSWORD=$(get_cluster_secret admin-password)"
            echo "JWT_SECRET=$(get_cluster_secret jwt-secret)"
        } > "$SECRETS_ENV"
        chmod 600 "$SECRETS_ENV"
        echo "Recovered instance secrets from the cluster into ${SECRETS_ENV}."
    else
        {
            echo "DB_PASSWORD=$(openssl rand -hex 16)"
            echo "ADMIN_PASSWORD=$(openssl rand -hex 8)"
            echo "JWT_SECRET=$(openssl rand -hex 16)"
        } > "$SECRETS_ENV"
        chmod 600 "$SECRETS_ENV"
        echo "Generated new instance secrets in ${SECRETS_ENV}."
    fi
    source "$SECRETS_ENV"
}

function render_template {
    local src="$1" dst="$2"
    sed -e "s|{{NAME}}|${NAME}|g" \
        -e "s|{{NAMESPACE}}|${NAMESPACE}|g" \
        -e "s|{{VERSION}}|${VERSION}|g" \
        -e "s|{{TAG}}|${TAG}|g" \
        -e "s|{{IMAGE}}|${IMAGE}|g" \
        -e "s|{{DOMAIN}}|${DOMAIN}|g" \
        -e "s|{{ENV}}|${DEPLOY_ENV}|g" \
        -e "s|{{NODE_PORT}}|${NODE_PORT}|g" \
        -e "s|{{NODE_IP}}|${NODE_IP}|g" \
        -e "s|{{DB_PASSWORD}}|${DB_PASSWORD}|g" \
        -e "s|{{ADMIN_PASSWORD}}|${ADMIN_PASSWORD}|g" \
        -e "s|{{JWT_SECRET}}|${JWT_SECRET}|g" \
        "$src" > "$dst"
    # Lines marked #@repl are kept (marker stripped) only when the model's
    # :repl key is true; otherwise they are removed.
    if [[ "$REPL" == "true" ]]; then
        sed -i 's| *#@repl$||' "$dst"
    else
        sed -i '/#@repl$/d' "$dst"
    fi
}

function generate_manifests {
    echo
    echo "Generating manifests in ${OUT_DIR}"
    mkdir -p "$OUT_DIR"
    local tpl base
    for tpl in "$TEMPLATES_DIR"/*.yaml.tpl; do
        base=$(basename "$tpl" .tpl)
        render_template "$tpl" "${OUT_DIR}/${base}"
        echo "  ${OUT_DIR}/${base}"
    done
    # Database init SQL, shipped to the init container via a ConfigMap.
    sed "s/:database_name:/${DB_NAME}/" "$DB_INIT_TEMPLATE" \
        > "${OUT_DIR}/init.sql"
    kubectl create configmap "dataui-${NAME}-init-sql" \
            --namespace "$NAMESPACE" \
            --from-file=init.sql="${OUT_DIR}/init.sql" \
            --dry-run=client -o yaml > "${OUT_DIR}/20-init-sql.yaml"
    rm "${OUT_DIR}/init.sql"
    echo "  ${OUT_DIR}/20-init-sql.yaml"
    # HAProxy drop-in snippet and map entry.
    render_template "${TEMPLATES_DIR}/haproxy-backend.cfg.tpl" \
                    "${OUT_DIR}/haproxy-backend.cfg"
    echo "${DOMAIN} dataui-${NAME}" > "${OUT_DIR}/haproxy-map-entry"
    echo "  ${OUT_DIR}/haproxy-backend.cfg"
    echo "  ${OUT_DIR}/haproxy-map-entry"
}

function apply_manifests {
    echo
    echo "Applying manifests to cluster ${K3D_CLUSTER}"
    kubectl apply -f "$OUT_DIR" || die "kubectl apply failed."
    kubectl -n "$NAMESPACE" rollout status \
            "deploy/dataui-${NAME}-postgres" --timeout=300s \
        || die "PostgreSQL rollout failed."
    kubectl -n "$NAMESPACE" rollout status \
            "deploy/dataui-${NAME}" --timeout=600s \
        || die "Data UI rollout failed."
}

function update_haproxy {
    echo
    echo "Updating HAProxy (requires sudo)"
    sudo install -d "$HAPROXY_CONF_D"
    [[ -f "$HAPROXY_MAP" ]] || sudo touch "$HAPROXY_MAP"
    # One-time: load conf.d in addition to the main config. EXTRAOPTS in
    # /etc/default/haproxy overrides the unit's, so keep the -S option.
    if ! grep -q 'conf\.d' "$HAPROXY_DEFAULTS"; then
        echo 'EXTRAOPTS="-S /run/haproxy-master.sock -f /etc/haproxy/conf.d"' \
            | sudo tee -a "$HAPROXY_DEFAULTS" >/dev/null
        echo "Enabled ${HAPROXY_CONF_D} via EXTRAOPTS in ${HAPROXY_DEFAULTS}."
    fi
    # One-time: map-based routing rule in the https frontend.
    if ! grep -q 'data-ui\.map' "$HAPROXY_CFG"; then
        sudo sed -i '/^ *default_backend guru/i\    use_backend %[req.hdr(host),lower,map(/etc/haproxy/data-ui.map)] if { req.hdr(host),lower,map(/etc/haproxy/data-ui.map) -m found }' \
             "$HAPROXY_CFG"
        echo "Added data-ui.map routing rule to ${HAPROXY_CFG}."
    fi
    # Per-instance backend and map entry.
    sudo install -m 644 "${OUT_DIR}/haproxy-backend.cfg" \
         "${HAPROXY_CONF_D}/dataui-${NAME}.cfg"
    if grep -q "^${DOMAIN} " "$HAPROXY_MAP"; then
        sudo sed -i "s|^${DOMAIN} .*|${DOMAIN} dataui-${NAME}|" "$HAPROXY_MAP"
    else
        echo "${DOMAIN} dataui-${NAME}" | sudo tee -a "$HAPROXY_MAP" >/dev/null
    fi
    # Validate, then reload.
    if sudo haproxy -c -f "$HAPROXY_CFG" -f "$HAPROXY_CONF_D" >/dev/null; then
        sudo systemctl reload haproxy
        echo "HAProxy reloaded."
    else
        die "HAProxy configuration validation failed; not reloading."
    fi
}

function remote_deploy {
    echo "Not on ${DEPLOY_HOST}; deploying remotely via ssh."
    require_clean_tree
    local branch sha origin_url
    branch=$(git branch --show-current)
    sha=$(git rev-parse HEAD)
    origin_url=$(git remote get-url origin)
    [[ -n "$branch" ]] || die "Detached HEAD; check out a branch to deploy."
    # Create the tag here, so its provenance is the machine the deploy
    # was run from, and push it along with the branch. The remote side
    # finds the tag already present and reuses it.
    if [[ -z "$DRY_RUN" ]]; then
        gather_deploy_facts
        create_deploy_tag
        git push origin "$branch" "refs/tags/${TAG}" || die "git push failed."
    else
        git push origin "$branch" || die "git push failed."
    fi
    # -t allocates a tty so the HAProxy step's sudo can prompt. Note that
    # $DEPLOY_CHECKOUT contains a literal \$HOME, expanded on the remote.
    ssh -t "$DEPLOY_HOST" "
        set -e
        if [[ ! -d \"$DEPLOY_CHECKOUT/.git\" ]]; then
            echo \"Creating deploy clone at $DEPLOY_CHECKOUT\"
            git clone '$origin_url' \"$DEPLOY_CHECKOUT\"
        fi
        cd \"$DEPLOY_CHECKOUT\"
        git fetch origin '+refs/heads/*:refs/remotes/origin/*' --tags --force
        git checkout --detach $sha
        DRY_RUN='$DRY_RUN' scripts/run.sh deploy
    " || die "Remote deploy failed."
}

function deploy {
    export_deploy_environment
    if [[ "$(hostname)" != "$DEPLOY_HOST" ]]; then
        remote_deploy
        return
    fi
    [[ -n "$DRY_RUN" ]] || require_clean_tree
    compile_default_model
    gather_deploy_facts
    [[ -n "$DRY_RUN" ]] || create_deploy_tag
    NODE_IP=$(kubectl get nodes -o \
        jsonpath='{.items[0].status.addresses[?(@.type=="InternalIP")].address}')
    [[ -n "$NODE_IP" ]] || die "Could not determine the k3d node IP."
    assign_node_port
    ensure_instance_secrets
    generate_manifests
    if [[ -n "$DRY_RUN" ]]; then
        echo
        echo "DRY_RUN set; stopping after manifest generation."
        return 0
    fi
    build_image
    apply_manifests
    update_haproxy
    echo
    echo "Deployed ${RELEASE_NAME}."
    echo "  Domain:   https://${DOMAIN}"
    echo "  NodePort: http://${NODE_IP}:${NODE_PORT}"
    if [[ "$REPL" == "true" ]]; then
        echo "  Swank:    kubectl -n ${NAMESPACE} port-forward" \
             "deploy/dataui-${NAME} 4005:4005"
    fi
}

case "$ACTION" in
    repl)
        export_repl_environment
        start_postgres_quietly
        initialize_database_quietly
        start_repl
        stop_database_quietly
        ;;
    db)
        export_repl_environment
        start_postgres
        initialize_database
        start_psql
        stop_database_quietly
        ;;
    psql)
        export_repl_environment
        start_psql
        ;;
    tests)
        export_test_environment
        start_postgres_quietly
        initialize_database_quietly
        run_tests
        stop_database_quietly
        echo $REPORT
        ;;
    debug)
        export_test_environment
        start_postgres
        initialize_database
        run_tests
        stop_database
        echo $REPORT
        ;;
    compile)
        export_docs_environment
        compile
        ;;
    docs)
        export_docs_environment
        generate_readme
        ;;
    stop)
        export_test_environment
        stop_database
        ;;
    field)
        FIELD_NAME=$2
        if [[ -z "$FIELD_NAME" ]]; then
            echo "Field name required."
            exit 1
        fi
        get_model_field
        ;;
    deploy)
        deploy
        ;;
    help)
        usage
        exit 0
        ;;
    *)
        if [[ -z "$ACTION" ]]; then
            echo "Missing command"
        else
            echo "Unknown command $ACTION"
        fi
        usage
        exit 1
        ;;
esac
