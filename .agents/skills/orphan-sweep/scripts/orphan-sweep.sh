#!/bin/bash
# orphan-sweep.sh — detect and sweep orphaned name-keyed `resources` rows in a
# Data UI deployment. Orphan = a resource whose name prefix maps to a
# row-resourced table (FK to resources) but whose id is no row id in it.
#
# Report mode is READ-ONLY. Sweep mode requires --confirm, backs up the exact
# delete set first, deletes surgically joined on the backup, and verifies.
#
# Usage:
#   orphan-sweep.sh list
#   orphan-sweep.sh report <target>
#   orphan-sweep.sh sweep <target> --confirm
#   orphan-sweep.sh [--runner '<psql command prefix>'] report|sweep [target] [--confirm]
#
# Targets:
#   dev                    dev REPL DB (docker pg-data-ui-repl, dataui/dataui)
#   demo:<app>             demo container pg-data-ui-<app> (user/db from env)
#   k8s:<namespace>        k3d app: the namespace's postgres pod (user/db from env)
#   pod:<namespace>/<pod>  explicit postgres pod
#   --runner '<cmd...>'    any command prefix that reads SQL on stdin and runs
#                          psql against the target (e.g. ssh-based for prod-2:
#                          "ssh prod-2 psql -U dataui -d dataui")
set -u

WORKROOT="${HOME}/workbench"
IDENT="rosie-orphan-sweep"

usage() { sed -n '2,20p' "$0" | sed 's/^# \{0,2\}//'; exit 2; }

RUNNER=()
TARGET_LABEL=""

# ---------------------------------------------------------------- targets ---
resolve_runner() {
  local target="$1" app ns pod u d
  case "$target" in
    dev)
      RUNNER=(docker exec -i pg-data-ui-repl psql -U dataui -d dataui)
      TARGET_LABEL="dev" ;;
    demo:*)
      app="${target#demo:}"
      read -r u d <<< "$(docker exec "pg-data-ui-$app" sh -c 'echo $POSTGRES_USER $POSTGRES_DB' 2>/dev/null)"
      [ -n "${u:-}" ] || { echo "target $target: container pg-data-ui-$app not reachable" >&2; exit 1; }
      d="${d:-$u}"
      RUNNER=(docker exec -i "pg-data-ui-$app" psql -U "$u" -d "$d")
      TARGET_LABEL="demo-$app" ;;
    k8s:*)
      ns="${target#k8s:}"
      pod="$(kubectl get pods -n "$ns" -o name 2>/dev/null | grep -m1 postgres | sed 's|^pod/||')"
      [ -n "$pod" ] || { echo "target $target: no postgres pod in namespace $ns" >&2; exit 1; }
      read -r u d <<< "$(kubectl exec -n "$ns" "$pod" -- sh -c 'echo $POSTGRES_USER $POSTGRES_DB' 2>/dev/null)"
      [ -n "${u:-}" ] || { echo "target $target: cannot read postgres env from $pod" >&2; exit 1; }
      d="${d:-$u}"
      RUNNER=(kubectl exec -i -n "$ns" "$pod" -- psql -U "$u" -d "$d")
      TARGET_LABEL="k8s-$ns" ;;
    pod:*)
      ns="${target#pod:}"; ns="${ns%%/*}"; pod="${target#pod:}"; pod="${pod#*/}"
      read -r u d <<< "$(kubectl exec -n "$ns" "$pod" -- sh -c 'echo $POSTGRES_USER $POSTGRES_DB' 2>/dev/null)"
      [ -n "${u:-}" ] || { echo "target $target: cannot read postgres env from $pod" >&2; exit 1; }
      d="${d:-$u}"
      RUNNER=(kubectl exec -i -n "$ns" "$pod" -- psql -U "$u" -d "$d")
      TARGET_LABEL="pod-$ns-${pod##*-}" ;;
    *) return 1 ;;
  esac
}

cmd_list() {
  echo "Data UI orphan-sweep targets on this host:"
  echo
  for c in $(docker ps --format '{{.Names}}' | grep '^pg-data-ui-' | sort); do
    app="${c#pg-data-ui-}"
    [ "$app" = "repl" ] && app="dev" || app="demo:$app"
    echo "  $app   ($c)"
  done
  echo
  for ns in $(kubectl get ns -o custom-columns=:metadata.name 2>/dev/null | grep '^dataui-' | sort); do
    pod="$(kubectl get pods -n "$ns" -o name | grep -m1 postgres | sed 's|^pod/||')"
    [ -n "$pod" ] && echo "  k8s:$ns   ($pod)"
  done
  echo
  echo "  Anything else: --runner '<psql command prefix reading SQL on stdin>'"
}

# ------------------------------------------------------------ sql helpers ---
run_sql() { "${RUNNER[@]}" -v ON_ERROR_STOP=1; }                 # SQL on stdin
run_q()   { "${RUNNER[@]}" -t -A -F$'\t' -v ON_ERROR_STOP=1 -c "$1"; }  # rows only

# prefix -> row table (table is prefix with - -> _ , with or without rt_ )
table_for_prefix() {
  local pfx="$1" t1 t2
  t1="$(echo "$pfx" | tr '-' '_')"
  t2="rt_$t1"
  for cand in "$t1" "$t2"; do
    if echo "$FKTABLES" | grep -qx "$cand"; then echo "$cand"; return 0; fi
  done
  return 1
}

# Collect the environment: prefixes, FK tables. Sets PREFIXES, FKTABLES.
collect_env() {
  FKTABLES="$(run_q "select regexp_replace(conrelid::regclass::text, '^.*\\.', '') from pg_constraint where confrelid='resources'::regclass and contype='f' order by 1")" \
    || { echo "cannot read the FK graph — is this a Data UI database?" >&2; exit 1; }
  PREFIXES="$(run_q "select distinct split_part(resource_name,':',1) from resources where split_part(resource_name,':',1) not like 'type-%' order by 1")" \
    || { echo "cannot read resources — is this a Data UI database?" >&2; exit 1; }
  if [ -z "$FKTABLES" ]; then echo "no tables reference resources — not a Data UI database?" >&2; exit 1; fi
}

# ----------------------------------------------------------------- report ---
# For each prefix with a row table: res, rows, by-id, orphans, roleless by-id,
# rows-without-resource. Orphan select arms are saved in ARMS[] (quoted SQL
# fragments) for the histogram and the sweep.
ARMS=()
ARM_PREFIXES=()
NOTABLE=()   # prefixes with resources but no table (orphans of dropped types)

report() {
  local pfx tbl res rows orph roleless rwr total=0 out=""
  for pfx in $PREFIXES; do
    if tbl=$(table_for_prefix "$pfx"); then
      res=$(run_q "select count(*) from resources where split_part(resource_name,':',1)='$pfx'")
      rows=$(run_q "select count(*) from $tbl")
      orph=$(run_q "select count(*) from resources r where split_part(r.resource_name,':',1)='$pfx' and r.id not in (select id from $tbl)")
      roleless=$(run_q "select count(*) from resources r join $tbl t on t.id=r.id where split_part(r.resource_name,':',1)='$pfx' and not exists (select 1 from resource_roles rr where rr.resource_id=r.id)")
      rwr=$(run_q "select count(*) from $tbl t where not exists (select 1 from resources r where r.id=t.id)")
      printf '  %-14s %-14s res %5s  rows %5s  orphans %5s  roleless-by-id %3s  rows-wo-res %3s\n' \
        "$pfx" "$tbl" "$res" "$rows" "$orph" "$roleless" "$rwr"
      [ "$orph" != "0" ] && { ARMS+=("(split_part(r.resource_name,':',1)='$pfx' and r.id not in (select id from $tbl))"); ARM_PREFIXES+=("$pfx"); }
      total=$((total + orph))
    else
      NOTABLE+=("$pfx")
    fi
  done

  echo
  if [ "${#ARMS[@]}" -gt 0 ]; then
    echo "Orphan birth signature (server timezone $(run_q "select current_setting('timezone')"), top hours):"
    local union_where; union_where="$(printf ' OR %s' "${ARMS[@]}")"; union_where="${union_where# OR }"
    run_q "select date_trunc('hour', created_at), count(*) from resources r where $union_where group by 1 order by 2 desc, 1 limit 12" | sed 's/^/  /'
  fi
  if [ "${#NOTABLE[@]}" -gt 0 ]; then
    echo "Prefixes with resources but NO row table (orphans of dropped types — NOT swept, judge by hand):"
    for pfx in "${NOTABLE[@]}"; do
      echo "  $pfx: $(run_q "select count(*) from resources where split_part(resource_name,':',1)='$pfx'") resources"
    done
  fi

  echo
  echo "TOTAL ORPHANS: $total"
  # verdict
  local unsafe=""
  for pfx in $PREFIXES; do
    if tbl=$(table_for_prefix "$pfx"); then
      rwr=$(run_q "select count(*) from $tbl t where not exists (select 1 from resources r where r.id=t.id)")
      roleless=$(run_q "select count(*) from resources r join $tbl t on t.id=r.id where split_part(r.resource_name,':',1)='$pfx' and not exists (select 1 from resource_roles rr where rr.resource_id=r.id)")
      [ "$rwr" != "0" ] && unsafe="$unsafe $pfx(rows-wo-res:$rwr)"
      [ "$roleless" != "0" ] && unsafe="$unsafe $pfx(roleless-by-id:$roleless)"
    fi
  done
  if [ "$total" = "0" ]; then
    echo "VERDICT: nothing to sweep."
  elif [ -n "$unsafe" ]; then
    echo "VERDICT: NOT SAFE to sweep —$unsafe"
    echo "  Heal/inspect those first (roleless by-id needs roles re-stamped like prod-2's perf tag;"
    echo "  rows without a by-id resource need resources minted) — ask before proceeding."
  else
    echo "VERDICT: SAFE TO SWEEP (by-id set complete and roled everywhere)."
  fi
}

# ------------------------------------------------------------------ sweep ---
sweep() {
  local confirm="${1:-}"
  collect_env
  # fresh state: rebuild ARMS from the report numbers only
  ARMS=(); ARM_PREFIXES=(); NOTABLE=()
  # pre-flight: previous artifacts?
  local prev
  prev=$(run_q "select count(*) from information_schema.tables where table_schema='public' and table_name like 'zz_orphan_sweep%'")
  if [ "$prev" != "0" ]; then
    echo "REFUSING: zz_orphan_sweep* tables already exist — a previous sweep's backup is still in the DB." >&2
    echo "  Verify it (TSVs in ~/workbench/orphan-sweep-*), then drop those tables before sweeping again." >&2
    exit 1
  fi

  echo "== sweep pre-report ($TARGET_LABEL) =="
  report
  if [ "${#ARMS[@]}" -eq 0 ]; then echo "No table-backed orphans to sweep."; exit 0; fi

  if [ "$confirm" != "--confirm" ]; then
    echo
    echo "Not sweeping: run again with --confirm after the human has seen and approved this report."
    exit 3
  fi

  local dir union_where sqlfile nowts
  nowts=$(date +%Y%m%d-%H%M%S)
  dir="$WORKROOT/orphan-sweep-$nowts-$TARGET_LABEL"
  mkdir -p "$dir"
  union_where="$(printf ' OR %s' "${ARMS[@]}")"; union_where="${union_where# OR }"
  sqlfile=$(mktemp /tmp/opencode/orphan-sweep.XXXXXX.sql)

  cat > "$sqlfile" <<EOF
-- generated by orphan-sweep.sh for $TARGET_LABEL at $nowts
CREATE TABLE zz_orphan_sweep_backup AS
  SELECT r.id, r.resource_name, r.created_at, split_part(r.resource_name,':',1) AS type
  FROM resources r
  WHERE $union_where;

CREATE TABLE zz_orphan_sweep_roles_backup AS
  SELECT rr.id, rr.created_at, rr.updated_at, rr.resource_id, rr.role_id
  FROM resource_roles rr
  JOIN zz_orphan_sweep_backup b ON rr.resource_id = b.id;
EOF
  if ! run_sql < "$sqlfile"; then
    echo "Backup-table creation FAILED — nothing deleted. zz_ state is partial; inspect by hand." >&2
    rm -f "$sqlfile"; exit 1
  fi
  rm -f "$sqlfile"

  local nb nr
  nb=$(run_q "select count(*) from zz_orphan_sweep_backup")
  nr=$(run_q "select count(*) from zz_orphan_sweep_roles_backup")
  run_q "select id, resource_name, created_at, type from zz_orphan_sweep_backup order by type, created_at" > "$dir/resources.tsv"
  run_q "select id, created_at, updated_at, resource_id, role_id from zz_orphan_sweep_roles_backup order by resource_id" > "$dir/resource_roles.tsv"
  {
    echo "orphan-sweep backup — target $TARGET_LABEL — $nowts"
    echo "resources: $nb rows   role links (cascade-deleted): $nr"
    echo "backed up BEFORE deletion; DELETE joins on zz_orphan_sweep_backup."
    echo "rollback: re-INSERT resources rows from resources.tsv, then role links from resource_roles.tsv."
    echo "the zz_ tables remain in the DB until dropped after verification."
  } > "$dir/README.txt"

  echo "Backed up $nb resources + $nr role links -> $dir"

  echo "DELETE FROM resources r USING zz_orphan_sweep_backup b WHERE r.id = b.id;"
  run_q "DELETE FROM resources r USING zz_orphan_sweep_backup b WHERE r.id = b.id"

  echo "== post-checks =="
  local pfx tbl orph bad=0
  for pfx in "${ARM_PREFIXES[@]}"; do
    tbl=$(table_for_prefix "$pfx")
    orph=$(run_q "select count(*) from resources r where split_part(r.resource_name,':',1)='$pfx' and r.id not in (select id from $tbl)")
    echo "  $pfx orphans now: $orph"
    [ "$orph" != "0" ] && bad=1
  done
  for pfx in $PREFIXES; do
    if tbl=$(table_for_prefix "$pfx"); then
      run_q "select count(*) from $tbl t where not exists (select 1 from resources r where r.id=t.id)" | grep -qv '^0$' \
        && { echo "  $pfx: rows without resource appeared!" ; bad=1; }
    fi
  done
  if [ "$bad" = "0" ]; then
    echo "POST-CHECKS CLEAN. zz_ tables + TSVs left in place:"
    echo "  DB: zz_orphan_sweep_backup, zz_orphan_sweep_roles_backup (drop after verification)"
    echo "  Files: $dir"
    echo "Next: API smoke (list + one write on the app), then drop the zz_ tables."
  else
    echo "POST-CHECK PROBLEMS — inspect by hand; rollback artifacts are in $dir" >&2
    exit 1
  fi
}

# ------------------------------------------------------------------ main ----
MODE=""; TARGET=""; CONFIRM=""; RUNNER_STR=""
while [ $# -gt 0 ]; do
  case "$1" in
    --runner) RUNNER_STR="$2"; shift 2 ;;
    report|sweep|list) MODE="$1"; shift ;;
    --confirm) CONFIRM="--confirm"; shift ;;
    *) TARGET="$1"; shift ;;
  esac
done
[ -n "$MODE" ] || usage

if [ -n "$RUNNER_STR" ]; then
  read -ra RUNNER <<< "$RUNNER_STR"
  TARGET_LABEL="${TARGET:-custom}"
elif [ "$MODE" = "list" ]; then
  cmd_list; exit 0
elif [ -n "$TARGET" ]; then
  resolve_runner "$TARGET" || usage
else
  usage
fi

if [ "$MODE" = "report" ]; then
  echo "== orphan report: $TARGET_LABEL ($(date '+%Y-%m-%d %H:%M:%S %Z')) =="
  collect_env
  report
elif [ "$MODE" = "sweep" ]; then
  sweep "$CONFIRM"
fi
