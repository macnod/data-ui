#!/bin/bash
#
# prod-2-prune.sh — daily image-cache hygiene on prod-2 (user timer,
# 09:40 UTC). Root's disk is 50 GiB and every deploy leaves ≈1.3 GB in
# host Docker plus ≈1.3 GB in the k3d node with nothing else pruning
# them; BuildKit cache grew to 233 GB on evo-x2. Under flock on the
# production checkout's lock — the deploy lock, so a prune never
# races a build — it removes:
#   - all BuildKit cache (docker builder prune -af)
#   - every local */data-ui/* image on the host daemon (the running
#     images live in the k3d node; everything host-side is re-pullable
#     from ECR)
#   - every unused image inside the k3d node (crictl rmi --prune);
#     running images stay (imagePullPolicy: IfNotPresent + k3d image
#     import made them node-local)
#
# The prune script's own copy lives in the bootstrap/production clone
# at ops/prod-2-prune.sh; the timer runs this file via the clone.

set -euo pipefail

CLUSTER="${K3D_CLUSTER:-prod-2}"
LOCK="${CHECKOUT_LOCK_ROOT:-/data/data-ui/repos}/production.lock"

exec 9>"$LOCK"
flock -n 9 || { echo "prod-2-prune: a deploy holds the lock; skipping this run."; exit 0; }

echo "prod-2-prune: $(date -u +%FT%TZ)"
docker builder prune -af
docker images --format '{{.Repository}}:{{.Tag}}' | grep '/data-ui/' | xargs -r docker rmi -f || true
docker exec "k3d-${CLUSTER}-server-0" crictl rmi --prune >/dev/null 2>&1 || \
    echo "prod-2-prune: node prune skipped (node not reachable?)" >&2
echo "prod-2-prune: done."
