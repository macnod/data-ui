#!/bin/bash
# Certbot deploy hook: rebuild the combined HAProxy pem for the renewed
# lineage and reload HAProxy.
#
# Installed by deploy/setup-tls.sh to
# /etc/letsencrypt/renewal-hooks/deploy/haproxy-pem.sh. Certbot runs it
# as root after every successful issuance or renewal, with
# RENEWED_LINEAGE set to the lineage directory
# (e.g. /etc/letsencrypt/live/demo.data-ui.com).
set -euo pipefail

CERTS_DIR=/etc/haproxy/certs
lineage="${RENEWED_LINEAGE:?RENEWED_LINEAGE not set}"
name=$(basename "$lineage")

install -d -m 755 "$CERTS_DIR"
umask 077
cat "$lineage/fullchain.pem" "$lineage/privkey.pem" \
    > "$CERTS_DIR/$name.pem.new"
mv "$CERTS_DIR/$name.pem.new" "$CERTS_DIR/$name.pem"
echo "Rebuilt $CERTS_DIR/$name.pem"

if systemctl is-active --quiet haproxy; then
    systemctl reload haproxy
    echo "HAProxy reloaded."
fi
