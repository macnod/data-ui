#!/bin/bash
# One-time TLS setup for *.demo.data-ui.com on the deploy host (evo-x2).
#
# - Creates an access key for the certbot-evo-x2 IAM user (least
#   privilege: DNS-01 record changes in the data-ui.com hosted zone)
# - Installs it as root's AWS credentials (certbot renewals run as root)
# - Installs the HAProxy renewal hook
# - Obtains the certificate via certbot (DNS-01 through Route 53)
# - Builds the combined pem and adds /etc/haproxy/certs/ to the :443
#   bind, so SNI picks the right cert per domain from here on
#
# Run as your normal user; sudo is used where needed. Renewals are
# automatic afterwards via certbot.timer + the deploy hook.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LINEAGE="demo.data-ui.com"
CERTS_DIR="/etc/haproxy/certs"
HOOK_SRC="$SCRIPT_DIR/letsencrypt-haproxy-hook.sh"
HOOK_DST="/etc/letsencrypt/renewal-hooks/deploy/haproxy-pem.sh"
HAPROXY_CFG="/etc/haproxy/haproxy.cfg"
IAM_USER="certbot-evo-x2"

# 1. Root AWS credentials for certbot-dns-route53.
if sudo test -f /root/.aws/credentials; then
    echo "Root AWS credentials already present; leaving them alone."
else
    key=$(aws iam create-access-key --user-name "$IAM_USER" \
              --query 'AccessKey.[AccessKeyId,SecretAccessKey]' \
              --output text)
    sudo mkdir -p /root/.aws
    printf '[default]\naws_access_key_id = %s\naws_secret_access_key = %s\n' \
           $key | sudo tee /root/.aws/credentials >/dev/null
    sudo chmod 700 /root/.aws
    sudo chmod 600 /root/.aws/credentials
    printf '[default]\nregion = us-east-1\n' \
        | sudo tee /root/.aws/config >/dev/null
    echo "Installed root AWS credentials (key id ${key%%	*})."
fi

# 2. Renewal hook (before certonly, so first issuance triggers it too).
sudo install -D -m 755 "$HOOK_SRC" "$HOOK_DST"
echo "Installed renewal hook at $HOOK_DST."

# 3. Certificate. The wildcard does not cover the bare domain, so
# request both. Idempotent: certbot keeps the existing lineage if it is
# still valid.
sudo certbot certonly --dns-route53 -n \
     --cert-name "$LINEAGE" \
     -d "$LINEAGE" -d "*.$LINEAGE"

# 4. Make sure the combined pem exists (the deploy hook only fires on
# new issuance, not when certbot decides the cert is still valid).
if sudo test ! -f "$CERTS_DIR/$LINEAGE.pem"; then
    sudo RENEWED_LINEAGE="/etc/letsencrypt/live/$LINEAGE" "$HOOK_DST"
fi

# 5. Add the certs directory to the :443 bind. The existing explicit
# cert stays first, so it remains the default for non-SNI clients.
if ! grep -Eq "bind \*:443 .*crt $CERTS_DIR/" "$HAPROXY_CFG"; then
    sudo sed -i \
         "s|^\( *bind \*:443 ssl crt [^ ]*\)$|\1 crt $CERTS_DIR/|" \
         "$HAPROXY_CFG"
    echo "Added crt $CERTS_DIR/ to the :443 bind in $HAPROXY_CFG."
fi

# 6. Validate and reload.
if [[ -d /etc/haproxy/conf.d ]]; then
    sudo haproxy -c -f "$HAPROXY_CFG" -f /etc/haproxy/conf.d >/dev/null
else
    sudo haproxy -c -f "$HAPROXY_CFG" >/dev/null
fi
sudo systemctl reload haproxy
echo
echo "Done. Verify with:"
echo "  curl -sv https://anything.demo.data-ui.com 2>&1 | grep -E 'subject|issuer'"
