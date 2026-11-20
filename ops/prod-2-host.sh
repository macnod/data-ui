#!/bin/bash
#
# prod-2-host.sh — one-time host setup for the Data UI prod-2 EC2
# site. Two modes, both run as root (sudo bash -s -- <mode> [...]):
#
#   base
#     Runs on the BUILDER instance; its result is the golden AMI.
#     Installs OS packages, snaps, k3d (checksum-verified), Roswell
#     (the Dockerfile's architecture switch), the macnod user (fixed
#     UID/GID so an adopted /data volume keeps its ownership across
#     AMI generations), the sudoers rule, the HAProxy baseline (with
#     a self-signed placeholder pem), the /data fstab line, pre-pulled
#     base images, the Docker drop-ins, the prune units, and — as
#     macnod — an HTTPS bootstrap clone plus the SBCL/Quicklisp world
#     parsed out of that checkout's Dockerfile (one source of truth
#     for the host compile gate). NO cluster, NO credentials, NO
#     data: nothing secret can reach the image.
#
#   post-image fresh|adopt <volume-id>
#     Runs on a launched instance with its data volume attached.
#     fresh refuses to format anything that already holds a
#     filesystem; adopt requires the data-ui-data label and never
#     formats. Mounts /data, creates the state tree (fresh) or
#     requires the site marker (adopt), moves the letsencrypt lineage
#     onto /data, starts Docker and HAProxy, creates the k3d cluster,
#     imports postgres:16, runs deploy-bootstrap from the freshened
#     bootstrap clone, runs setup-tls.sh with the instance role,
#     symlinks the backup scripts into ~/bin (timers NOT started),
#     and enables the prune timer.

set -euo pipefail

die() { echo "prod-2-host.sh: $*" >&2; exit 1; }

MACNOD_USER=macnod
MACNOD_UID=1500
MACNOD_GID=1500
K3D_VERSION=v5.8.3
K3S_VERSION=v1.31.5-k3s1
K3D_SHA256_ARM64=0b8110f2229631af7402fb828259330985918b08fefd38b7f1b788a1c8687216
K3D_SHA256_AMD64=dbaa79a76ace7f4ca230a1ff41dc7d8a5036a8ad0309e9c54f9bf3836dbe853e
ROSWELL_VERSION=v23.10.14.114
ROSWELL_URL_PREFIX="https://github.com/roswell/roswell/releases/download"
BOOTSTRAP_CLONE="/home/${MACNOD_USER}/common-lisp/data-ui"
DATA_LABEL=data-ui-data

host_arch() {
    case "$(uname -m)" in
        aarch64) echo arm64 ;;
        x86_64)  echo amd64 ;;
        *) die "unsupported architecture $(uname -m)." ;;
    esac
}

# ---------------------------------------------------------------------------
# base — the AMI's contents
# ---------------------------------------------------------------------------

host_base() {
    [[ "$(id -u)" == 0 ]] || die "base must run as root (sudo bash -s -- base)."
    local arch
    arch=$(host_arch)

    echo "== packages =="
    apt update
    apt upgrade -y
    apt install -y \
        docker.io docker-buildx docker-compose-v2 \
        haproxy certbot python3-certbot-dns-route53 \
        jq git curl bzip2 make dnsutils openssl
    snap install aws-cli --classic
    snap install kubectl --classic

    echo "== k3d ${K3D_VERSION} (checksum-verified) =="
    local url tmp got want
    url="https://github.com/k3d-io/k3d/releases/download/${K3D_VERSION}/k3d-linux-${arch}"
    tmp=$(mktemp)
    curl -fsSL "$url" -o "$tmp"
    got=$(sha256sum "$tmp" | cut -d' ' -f1)
    case "$arch" in
        arm64) want="$K3D_SHA256_ARM64" ;;
        amd64) want="$K3D_SHA256_AMD64" ;;
    esac
    [[ "$got" == "$want" ]] || { rm -f "$tmp"; die "k3d checksum mismatch (${got}, wanted ${want})."; }
    install -m 755 "$tmp" /usr/local/bin/k3d
    rm -f "$tmp"
    k3d version

    echo "== Roswell ${ROSWELL_VERSION} (the Dockerfile's architecture switch) =="
    if [[ "$arch" == "amd64" ]]; then
        curl -fsSL \
            "${ROSWELL_URL_PREFIX}/${ROSWELL_VERSION}/roswell_${ROSWELL_VERSION#v}-1_amd64.deb" \
            -o /tmp/roswell.deb
        dpkg -i /tmp/roswell.deb
        rm -f /tmp/roswell.deb
    else
        curl -fsSL \
            "${ROSWELL_URL_PREFIX}/${ROSWELL_VERSION}/roswell-${ROSWELL_VERSION#v}-linux-arm64-glibc2.31.tar.bz2" \
            -o /tmp/roswell.tar.bz2
        tar -xjf /tmp/roswell.tar.bz2 -C /tmp
        ( cd /tmp/roswell && make install )
        rm -rf /tmp/roswell /tmp/roswell.tar.bz2
    fi

    echo "== user ${MACNOD_USER} (fixed UID/GID ${MACNOD_UID}) =="
    groupadd -g "$MACNOD_GID" "$MACNOD_USER" 2>/dev/null || true
    useradd -m -u "$MACNOD_UID" -g "$MACNOD_GID" -s /bin/bash \
            "$MACNOD_USER" 2>/dev/null || true
    usermod -aG docker "$MACNOD_USER"
    loginctl enable-linger "$MACNOD_USER" 2>/dev/null || true
    mkdir -p "/home/${MACNOD_USER}/.ssh"
    cp /home/ubuntu/.ssh/authorized_keys "/home/${MACNOD_USER}/.ssh/authorized_keys"
    chown -R "${MACNOD_UID}:${MACNOD_GID}" "/home/${MACNOD_USER}/.ssh"
    chmod 700 "/home/${MACNOD_USER}/.ssh"
    chmod 600 "/home/${MACNOD_USER}/.ssh/authorized_keys"

    echo "== sudoers (evo-x2's rule plus mkdir and chown, which"
    echo "   apply_manifests calls through sudo; with tee, install, and"
    echo "   sed in it the list is a documented surface, not a"
    echo "   privilege boundary) =="
    cat > /etc/sudoers.d/data-ui-deploy <<SUDOERS
${MACNOD_USER} ALL=(root) NOPASSWD: /usr/bin/install, /usr/bin/touch, /usr/bin/tee, /usr/bin/sed, /usr/bin/rm, /usr/bin/mkdir, /usr/bin/chown, /usr/sbin/haproxy, /usr/bin/systemctl restart haproxy, /usr/bin/systemctl reload haproxy, /usr/bin/socat
SUDOERS
    chmod 440 /etc/sudoers.d/data-ui-deploy
    visudo -cf /etc/sudoers.d/data-ui-deploy

    echo "== bootstrap clone (anonymous HTTPS — no GitHub credential on prod-2) =="
    sudo -u "$MACNOD_USER" git clone \
        https://github.com/macnod/data-ui.git "$BOOTSTRAP_CLONE"

    echo "== SBCL + Quicklisp world from the checkout's Dockerfile =="
    local dockerfile="${BOOTSTRAP_CLONE}/Dockerfile"
    [[ -f "$dockerfile" ]] || die "no Dockerfile in the bootstrap clone (${dockerfile})."
    local sbcl_version line
    sbcl_version=$(sed -n 's/^ARG SBCL_VERSION="\(.*\)"/\1/p' "$dockerfile" | head -1)
    [[ -n "$sbcl_version" ]] || die "cannot parse SBCL_VERSION out of ${dockerfile}."
    sudo -u "$MACNOD_USER" ros install "sbcl-bin/${sbcl_version}"
    sudo -u "$MACNOD_USER" ros use "sbcl-bin/${sbcl_version}"
    while IFS= read -r line; do
        # "RUN ulimit -n 1048576 && ros install <project>" — one
        # source of truth with the image build.
        echo "  ros install ${line}"
        sudo -u "$MACNOD_USER" ros install ${line}
    done < <(sed -n 's/^RUN ulimit -n [0-9]* && ros install //p' "$dockerfile" | sort -u)

    echo "== HAProxy baseline =="
    # This script arrives on stdin from the clone's ops/, so the
    # baseline cannot be found beside it; use the clone's copy.
    local ops_dir="${BOOTSTRAP_CLONE}/ops"
    [[ -f "${ops_dir}/prod-2-haproxy.cfg" ]] \
        || die "no prod-2-haproxy.cfg in ${ops_dir}."
    install -m 644 "${ops_dir}/prod-2-haproxy.cfg" /etc/haproxy/haproxy.cfg
    install -d -m 755 /etc/haproxy/certs /etc/haproxy/conf.d
    touch /etc/haproxy/data-ui.map
    chmod 644 /etc/haproxy/data-ui.map
    printf '# conf.d placeholder; deploy backends land here\n' \
        > /etc/haproxy/conf.d/00-placeholder.cfg
    # Self-signed placeholder so the crt directory loads before the
    # lineage exists; setup-tls.sh's pem lands in the same directory.
    local pem=/etc/haproxy/certs/placeholder-selfsigned.pem
    openssl req -x509 -newkey rsa:2048 -sha256 -days 3650 -nodes \
        -keyout /tmp/placeholder.key -out /tmp/placeholder.crt \
        -subj "/CN=placeholder.prod-2.internal" 2>/dev/null
    cat /tmp/placeholder.key /tmp/placeholder.crt > "$pem"
    rm -f /tmp/placeholder.key /tmp/placeholder.crt
    chmod 600 "$pem"
    # The exact EXTRAOPTS line haproxy_bootstrap writes, pre-installed
    # (the baseline already carries conf.d and the map rule, so
    # bootstrap's greps find the work done).
    if ! grep -q 'conf\.d' /etc/default/haproxy; then
        printf 'EXTRAOPTS="-S /run/haproxy-master.sock -f /etc/haproxy/conf.d"\n' \
            >> /etc/default/haproxy
    fi
    haproxy -c -f /etc/haproxy/haproxy.cfg -f /etc/haproxy/conf.d \
        || die "the prod-2 HAProxy baseline does not validate."
    systemctl stop haproxy || true
    systemctl disable haproxy || true

    echo "== /data fstab line (by label: a snapshot-restored volume mounts unchanged) =="
    grep -q "LABEL=${DATA_LABEL}" /etc/fstab \
        || printf 'LABEL=%s /data ext4 defaults,nofail,x-systemd.device-timeout=30s 0 2\n' \
                   "$DATA_LABEL" >> /etc/fstab
    mkdir -p /data

    echo "== pre-pulled base images (no external registry in the recovery path) =="
    systemctl start docker
    docker pull "rancher/k3s:${K3S_VERSION}"
    docker pull "ghcr.io/k3d-io/k3d-proxy:5.8.3"
    docker pull "ghcr.io/k3d-io/k3d-tools:5.8.3"
    docker pull postgres:16

    echo "== Docker drop-ins =="
    install -d /etc/systemd/system/docker.service.d
    # evo-x2's fix for slow SBCL builds (the fd-sweep startup cost).
    cat > /etc/systemd/system/docker.service.d/limit-nofile.conf <<UNIT
[Service]
LimitNOFILE=1048576
UNIT
    # Installed LAST: Docker — and with it the whole cluster — never
    # starts on an unmounted /data (a failed mount must not serve
    # empty databases).
    cat > /etc/systemd/system/docker.service.d/require-data-mount.conf <<UNIT
[Unit]
RequiresMountsFor=/data
[Service]
ConditionPathIsMountPoint=/data
UNIT
    systemctl daemon-reload

    echo "== prune units (installed, NOT enabled — post-image enables the timer) =="
    sudo -u "$MACNOD_USER" mkdir -p "/home/${MACNOD_USER}/.config/systemd/user"
    sudo -u "$MACNOD_USER" cp "${BOOTSTRAP_CLONE}/ops/prod-2-prune.service" \
        "/home/${MACNOD_USER}/.config/systemd/user/prod-2-prune.service"
    sudo -u "$MACNOD_USER" cp "${BOOTSTRAP_CLONE}/ops/prod-2-prune.timer" \
        "/home/${MACNOD_USER}/.config/systemd/user/prod-2-prune.timer"

    systemctl stop docker || true

    echo "== base complete: a secret-free image; cloud-init is cleaned by the caller =="
}

# ---------------------------------------------------------------------------
# post-image — a launched instance's first boot work
# ---------------------------------------------------------------------------

post_image() {
    local mode="$1" volume_id="$2"
    [[ "$(id -u)" == 0 ]] || die "post-image must run as root."
    [[ "$mode" == fresh || "$mode" == adopt ]] \
        || die "post-image wants fresh|adopt <volume-id>."

    echo "== data volume ${volume_id} (${mode}) =="
    local nvme_id=${volume_id#vol-}
    local by_id="/dev/disk/by-id/nvme-Amazon_Elastic_Block_Store_${nvme_id}"
    [[ -e "$by_id" ]] \
        || die "no NVMe device for ${volume_id} at ${by_id} (attachment not settled? wait and re-run)."
    local fs label
    fs=$(blkid -o value -s TYPE "$by_id" 2>/dev/null || true)
    label=$(blkid -o value -s LABEL "$by_id" 2>/dev/null || true)
    if [[ "$mode" == fresh ]]; then
        [[ -z "$fs" ]] \
            || die "fresh refuses to format: ${by_id} already holds a filesystem (${fs}). Use adopt, or a truly fresh volume."
        mkfs.ext4 -L "$DATA_LABEL" "$by_id"
    else
        [[ "$label" == "$DATA_LABEL" ]] \
            || die "adopt requires the ${DATA_LABEL} label on ${by_id} (found '${label:-none}'); adopt never formats."
    fi
    mkdir -p /data
    mount "$by_id" /data || mount LABEL="$DATA_LABEL" /data \
        || die "cannot mount ${by_id} at /data."
    mountpoint /data >/dev/null || die "/data is not a mountpoint after mounting."

    if [[ "$mode" == fresh ]]; then
        echo "== state tree (fresh) =="
        mkdir -p /data/data-ui/ops /data/data-ui/deploy /data/data-ui/snapshots \
                 /data/data-ui/repos /data/k8s
        chown -R "${MACNOD_UID}:${MACNOD_GID}" /data/data-ui /data/k8s
        echo prod-2 > /data/data-ui/site
    else
        echo "== site marker (adopt) =="
        [[ "$(cat /data/data-ui/site 2>/dev/null || true)" == prod-2 ]] \
            || die "adopt requires the prod-2 site marker at /data/data-ui/site; this volume is not a prod-2 data volume."
    fi
    chmod 644 /data/data-ui/site

    echo "== letsencrypt lineage onto /data (a re-attached volume needs no issuance) =="
    if [[ -d /etc/letsencrypt && ! -e /data/letsencrypt ]]; then
        mv /etc/letsencrypt /data/letsencrypt
    fi
    [[ -d /data/letsencrypt ]] || mkdir -p /data/letsencrypt
    chown -R "${MACNOD_UID}:${MACNOD_GID}" /data/letsencrypt 2>/dev/null || true
    ln -sfn /data/letsencrypt /etc/letsencrypt

    echo "== Docker + cluster =="
    systemctl start docker
    local cluster="${K3D_CLUSTER:-prod-2}"
    if ! sudo -u "$MACNOD_USER" k3d cluster list "$cluster" >/dev/null 2>&1; then
        sudo -u "$MACNOD_USER" k3d cluster create "$cluster" \
            --image "rancher/k3s:${K3S_VERSION}" \
            --api-port 127.0.0.1:6445 \
            -p 127.0.0.1:31303-31322:30303-30322@server:0 \
            -v /data/k8s:/data/k8s \
            --k3s-arg '--disable=local-storage@server:0' \
            --wait
    else
        echo "Cluster ${cluster} already exists (re-run); starting it."
        sudo -u "$MACNOD_USER" k3d cluster start "$cluster"
    fi
    sudo -u "$MACNOD_USER" k3d image import postgres:16 -c "$cluster"

    echo "== deploy-bootstrap from the freshened bootstrap clone =="
    sudo -u "$MACNOD_USER" git -C "$BOOTSTRAP_CLONE" fetch origin master \
        || die "cannot fetch the bootstrap clone (network?)."
    sudo -u "$MACNOD_USER" git -C "$BOOTSTRAP_CLONE" reset --hard origin/master
    sudo -u "$MACNOD_USER" env DATA_UI_STATE=/data/data-ui \
        "$BOOTSTRAP_CLONE/scripts/data-ui" deploy-bootstrap

    echo "== HAProxy up, TLS via the instance role =="
    systemctl enable --now haproxy
    sudo -u "$MACNOD_USER" env TLS_AWS_AUTH=instance-role \
        "$BOOTSTRAP_CLONE/deploy/setup-tls.sh"
    certbot renew --dry-run \
        || die "certbot renew --dry-run failed (the lineage or the Route 53 role is broken)."

    echo "== backup scripts into ~/bin (timers NOT started here) =="
    # Re-point at the production clone's ops/ copy when it exists (the
    # settled symlink pattern; the bootstrap clone is the fallback).
    local ops_src="${BOOTSTRAP_CLONE}/ops"
    [[ -d /data/data-ui/repos/production/ops ]] && ops_src=/data/data-ui/repos/production/ops
    sudo -u "$MACNOD_USER" mkdir -p "/home/${MACNOD_USER}/bin"
    local unit
    for unit in backlog-backup scenebook-backup; do
        sudo -u "$MACNOD_USER" ln -sfn "${ops_src}/${unit}" \
            "/home/${MACNOD_USER}/bin/${unit}"
    done
    # The env file holds ONLY the region and bucket on prod-2 — the
    # instance role carries the credentials. Not overwritten when
    # present (an adopted volume keeps its configuration).
    if [[ ! -f "/home/${MACNOD_USER}/.config/data-ui-backup.env" ]]; then
        sudo -u "$MACNOD_USER" mkdir -p "/home/${MACNOD_USER}/.config"
        sudo -u "$MACNOD_USER" tee "/home/${MACNOD_USER}/.config/data-ui-backup.env" >/dev/null <<ENVEOF
# prod-2 backup credentials: the instance role (no keys here).
AWS_REGION=us-east-1
AWS_BUCKET=data-ui-backups-232168665831
ENVEOF
        sudo -u "$MACNOD_USER" chmod 600 "/home/${MACNOD_USER}/.config/data-ui-backup.env"
    fi

    echo "== prune timer =="
    sudo -u "$MACNOD_USER" cp "${ops_src}/prod-2-prune.service" \
        "/home/${MACNOD_USER}/.config/systemd/user/prod-2-prune.service" 2>/dev/null || true
    sudo -u "$MACNOD_USER" cp "${ops_src}/prod-2-prune.timer" \
        "/home/${MACNOD_USER}/.config/systemd/user/prod-2-prune.timer" 2>/dev/null || true
    sudo -u "$MACNOD_USER" systemctl --user daemon-reload
    sudo -u "$MACNOD_USER" systemctl --user enable --now prod-2-prune.timer

    echo "== post-image (${mode}) complete =="
}

case "${1:-}" in
    base)
        host_base
        ;;
    post-image)
        [[ -n "${2:-}" && -n "${3:-}" && -z "${4:-}" ]] \
            || die "post-image wants exactly fresh|adopt <volume-id>."
        post_image "$2" "$3"
        ;;
    *)
        die "usage: sudo bash -s -- {base | post-image fresh|adopt <volume-id>}"
        ;;
esac
