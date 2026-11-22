---
description: "Detect and sweep orphaned name-keyed resources rows from any Data UI deployment — the dev REPL, demo containers, k3d apps, or any reachable psql — as the invoking agent. Report mode is read-only: derives the row-resourced table set from the resources FK graph (never hardcoded), reports per-type orphan counts, the birth-hour histogram, roleless-by-id and rows-without-resource safety checks, and a sweep-safety verdict. Sweep mode runs only after the report has been presented and the human has explicitly approved: it backs up the exact delete set to zz_ tables and TSVs under ~/workbench, deletes surgically joined on the backup (never touching type-* or non-row resources), verifies zero orphans after, and leaves the rollback artifacts for review. Use when Donnie asks whether a deployment has orphan resources, wants an orphan check or a cleanup ('check the demos for orphans', 'does SceneBook have the orphan problem', 'sweep SceneBook', '/orphan-sweep report k8s:dataui-scenebook'). Project-scoped to data-ui; runs on evo-x2 where docker/kubectl reach the instances."
name: orphan-sweep
---

# Skill: orphan-sweep

Detect — and, only after reporting and asking, clear — orphaned `resources` rows in a Data UI database. An orphan is a resource whose name prefix maps to a row-resourced table (a table with `FOREIGN KEY (id) REFERENCES resources(id)`) but whose id is no longer any row's id: the residue of rows deleted outside `be-delete`'s `:auto` arm — classically a data load wiped by `set-model`'s `reset-tables`, which drops row tables without removing their resources. Orphans are inert until something resolves a resource by name and hits a duplicate (`be-id`'s "More than one match" failure — the prod-2 perf-tag incident), and they ride every snapshot/restore downstream. Root cause and full timeline: backlog item "Fix reset-tables to remove row resources" and its "Root cause: the reset path is the orphan factory" note.

## When to Use

- Donnie asks whether a deployment, demo, or the dev image has orphan resources, or wants them dated/quantified.
- Donnie asks to clean, clear, or sweep orphans from an instance — after the report and his explicit go-ahead, only.
- Post-restore verification (the recurrence recipe's step 1), or any suspicion of by-name resolution failures.
- **Not** for changing models, healing schema drift (`heal-model`), or anything but pure SQL on `resources` / `resource_roles`.

## The script

Everything runs through `scripts/orphan-sweep.sh` beside this file (bash; needs docker and kubectl on evo-x2):

```bash
S=<skill-base>/scripts/orphan-sweep.sh

$S list                                   # enumerate targets on this host
$S report <target>                        # read-only detection + verdict
$S sweep <target> --confirm               # gated cleanup (refuses without --confirm)
$S --runner '<psql prefix>' report|sweep [label] [--confirm]   # any other psql target
```

Targets: `dev` (REPL, docker `pg-data-ui-repl`), `demo:<app>` (docker `pg-data-ui-<app>`, credentials read from the container env), `k8s:<namespace>` (the namespace's postgres pod, credentials from the pod env), `pod:<namespace>/<pod>`, or `--runner` with any command prefix that reads SQL on stdin and runs psql against the target — the escape hatch for remote hosts (e.g. an ssh + kubectl exec chain for prod-2: `--runner "ssh <host> kubectl exec -i -n <ns> <postgres-pod> -- psql -U <user> -d <db>"`, all as plain words) or for a throwaway database restored from a snapshot.

## Procedure

1. **Report first, always.** Resolve the target from Donnie's words (`list` when unsure; "the demos" means every `demo:*`; "SceneBook" is `k8s:dataui-scenebook`) and run `report`. Read-only; safe on live instances. Read the output before saying anything: per-type counts (res / rows / orphans / roleless-by-id / rows-wo-res), the birth-hour histogram (`created_at` is stored in the server's timezone — UTC on every current server), total, and the verdict. The signature patterns: exactly-half orphans = a load that was wiped and re-loaded (the `set-model` reset factory); usage-heavy orphans = base rows deleted outside `be-delete`; an 8h33m uniform pair delta is the SceneBook birthmark.
2. **Present the report and ask.** Give Donnie the numbers that matter (total, worst types, birth window, verdict) and ask whether to sweep. Never run `sweep` in the same breath as an unconfirmed `report`, and never batch sweeps of multiple targets under one confirmation — each target gets its own go-ahead.
3. **Sweep only on verdict `SAFE TO SWEEP`** (by-id set complete and roled everywhere) — with `--confirm`. If the verdict is `NOT SAFE` (roleless by-id > 0 or rows without resources), stop: role re-stamping is model-specific judgment (prod-2's perf tag was healed with its model's standard roles by hand) — report the finding and let Donnie decide. Prefixes with resources but no row table (orphans of dropped types) are reported but never swept by the script; flag them and ask.
4. **The script's sweep order is non-negotiable and self-guarding:** pre-report → refuse if `zz_orphan_sweep*` tables already exist (a prior sweep's backup is still unverified — have it dropped first) → create `zz_orphan_sweep_backup` / `zz_orphan_sweep_roles_backup` with the exact delete set → TSVs + README to `~/workbench/orphan-sweep-<ts>-<target>/` → one `DELETE ... USING` the backup (the FK cascades the role links) → per-type post-checks. The delete predicate is prefix-scoped per row table, so `type-*` and other non-row resources are untouched by construction. There are no transactions: if anything fails midway, say exactly what landed (backup tables? TSVs? delete count?) — the script prints each step's result.
5. **After a live sweep:** API-smoke the app — a list, a journal/child read, and one write through the app's own surface (for the backlog app, the `backlog` wrapper; for SceneBook, a login and a list) — to prove the rbac join path is healthy. Report the TSV directory and the two `zz_` tables; **Donnie drops the `zz_` tables after his own verification** — never drop them yourself. Rollback, if ever needed: re-INSERT from `resources.tsv` then `resource_roles.tsv`.
6. **Never heal with `set-model` or `reset-database`** — `reset-tables` drops user tables and destroys data; the sweep is pure SQL. `heal-model` stays for schema/RBAC drift, untouched by this skill.

## Notes

- To check a **snapshot** (is this dump contaminated?): `pg_restore` it into a throwaway postgres (docker `postgres:16`, throwaway port), then `--runner "docker exec -i <scratch> psql -U postgres -d <db>" report <label>`; remove the container after. Validated against `prd-scenebook-1118` (1,097 orphans, swept clean in the scratch).
- Both postgres servers on evo-x2 (REPL and k8s) store `created_at` in UTC while the host runs PST — subtract 8 hours for local wall-clock when reporting birth windows.
- The report is the evidence layer for the two backlog items: "Fix reset-tables to remove row resources" (the factory — engine fix) and "Post-restore orphan resource sweep" (restore-side defense-in-depth). After any sweep, a short note on the relevant item (create-note) recording target, counts, and TSV path keeps the journal truthful.
- Known-clean instances (2026-11-22): prod-2's backlog (hand-swept), the dev image's scenebook state (`dev-scenebook-prerestore`). Known-contaminated: k3s SceneBook production 1,097, dev REPL backlog 1,750, todos 11, books 21, chores 9, modelbank 10 — and every snapshot taken after a birth event.
