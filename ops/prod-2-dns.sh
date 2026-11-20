#!/bin/bash
#
# prod-2-dns.sh — per-app A records in the demo.data-ui.com hosted
# zone (prod-2's explicit-DNS publication path; the wildcard is never
# touched).
#
# Verbs:
#   upsert <domain> <ip>            UPSERT the exact A record (TTL 300)
#   delete-if-value <domain> <ip>   read the exact record first;
#                                   delete ONLY when its value is <ip>
#                                   (another site's record is never
#                                   touched)
#
# The zone comes from SITE_DNS_ZONE_ID (set by the site defaults on
# prod-2). One change batch per call; aws route53 wait
# resource-record-sets-changed waits the returned change to INSYNC.
# Refuses the zone apex and any wildcard name. Called by scripts/
# data-ui's cutover and delete verbs; safe to run by hand.
#
# Never refuses on a missing record for delete-if-value (idempotent
# re-run after a partial delete); dies on any AWS failure.

set -euo pipefail

die() { echo "prod-2-dns.sh: $*" >&2; exit 1; }

usage() {
    cat >&2 <<EOF
Usage: ops/prod-2-dns.sh upsert <domain> <ip>
       ops/prod-2-dns.sh delete-if-value <domain> <ip>
EOF
    exit 1
}

[ "${SITE_DNS_ZONE_ID:-}" ] || die "SITE_DNS_ZONE_ID is not set (this script runs on prod-2, where the site defaults arm it)."

ZONE_ID="$SITE_DNS_ZONE_ID"
TTL=300

normalize() { # lowercase, exactly one trailing dot
    local d="$1"
    d="${d,,}"
    d="${d%.}."
    echo "$d"
}

check_name() { # refuse apex and wildcards; print the FQDN
    local fqdn
    fqdn=$(normalize "$1")
    [[ "$fqdn" != *..* ]] || die "malformed domain '$1'."
    case "$fqdn" in
        \*.) die "refusing the wildcard name '$1'." ;;
    esac
    [ "$fqdn" != "demo.data-ui.com." ] \
        || die "refusing the zone apex 'demo.data-ui.com' (the wildcard cert covers it; no apex record is managed here)."
    echo "$fqdn"
}

change_batch() { # $1 = UPSERT|DELETE, $2 = fqdn, $3 = ip (empty for DELETE)
    if [ "$1" = UPSERT ]; then
        jq -n --arg n "$2" --arg v "$3" --argjson ttl "$TTL" \
            '{Comment: "data-ui prod-2 per-app A record",
              Changes: [{Action: "UPSERT",
                         ResourceRecordSet: {Name: $n, Type: "A", TTL: $ttl,
                                             ResourceRecords: [{Value: $v}]}}]}'
    else
        jq -n --arg n "$2" \
            '{Comment: "data-ui prod-2 per-app A record removal",
              Changes: [{Action: "DELETE",
                         ResourceRecordSet: {Name: $n, Type: "A", TTL: 300}}]}'
    fi
}

apply_change() { # $1 = batch JSON; waits the change to INSYNC
    local batch="$1" change_id
    change_id=$(aws route53 change-resource-record-sets \
                    --hosted-zone-id "$ZONE_ID" \
                    --change-batch "$batch" \
                    --query ChangeInfo.Id --output text) \
        || die "the Route 53 change was rejected."
    aws route53 wait resource-record-sets-changed --id "$change_id" \
        || die "waiting for INSYNC of change ${change_id} failed."
}

exact_a_value() { # $1 = fqdn; prints the record's value, empty if none
    local out name type
    out=$(aws route53 list-resource-record-sets \
              --hosted-zone-id "$ZONE_ID" \
              --start-record-name "$1" --start-record-type A \
              --max-items 1 --output json) || return 0
    name=$(jq -r '.ResourceRecordSets[0].Name // empty' <<<"$out")
    type=$(jq -r '.ResourceRecordSets[0].Type // empty' <<<"$out")
    [ "$name" = "$1" ] && [ "$type" = "A" ] || return 0
    jq -r '.ResourceRecordSets[0].ResourceRecords[0].Value // empty' <<<"$out"
}

cmd="${1:-}"; shift || true
case "$cmd" in
    upsert)
        [ "${1:-}" ] || usage
        [ "${2:-}" ] || die "upsert needs a domain and an ip."
        fqdn=$(check_name "$1")
        ip="$2"
        [[ "$ip" =~ ^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$ ]] \
            || die "'$ip' is not an IPv4 address."
        echo "Upserting ${fqdn} A ${ip} (TTL ${TTL}) in zone ${ZONE_ID}..."
        apply_change "$(change_batch UPSERT "$fqdn" "$ip")"
        echo "INSYNC: ${fqdn} A ${ip}."
        ;;
    delete-if-value)
        [ "${1:-}" ] || usage
        [ "${2:-}" ] || die "delete-if-value needs a domain and an ip."
        fqdn=$(check_name "$1")
        ip="$2"
        [[ "$ip" =~ ^[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+$ ]] \
            || die "'$ip' is not an IPv4 address."
        value=$(exact_a_value "$fqdn")
        if [ -z "$value" ]; then
            echo "No exact A record for ${fqdn}; nothing to remove."
            exit 0
        fi
        if [ "$value" != "$ip" ]; then
            echo "A record for ${fqdn} holds ${value}, not ${ip} — another site's record; leaving it."
            exit 0
        fi
        echo "Deleting ${fqdn} A ${ip} (it holds this site's address)..."
        apply_change "$(change_batch DELETE "$fqdn" "")"
        echo "Deleted: ${fqdn} A ${ip}."
        ;;
    *)
        usage
        ;;
esac
