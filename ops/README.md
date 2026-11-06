# ops/ — host-side operations for Data UI demo profiles

Sources for the petting-zoo host machinery (demo-apps-plan D3/D15). Edit here, reinstall via `scripts/data-ui staging update`; never edit the installed copies.

Contents:

- `pg-profile.yaml` — per-profile docker compose file (bind-mounted pgdata, loopback-only ports, user = unit user).
- `profile-run.sh` → `/usr/local/lib/data-ui/profile-run.sh` — the dataui@.service ExecStart wrapper.
- `dataui@.service` → `/etc/systemd/system/dataui@.service` — the systemd template unit.
- `petting-zoo-reset.sh` → `/usr/local/lib/data-ui/` — nightly golden reset (Phase 7). Runs as root; no args = every enabled e-demo, explicit args (any class, e.g. modelbank for a grungy-day restore, D13) bypass the enabled check.
- `dataui-reset.service` + `dataui-reset.timer` → `/etc/systemd/system/` — the 04:10 schedule (Persistent=true boot catch-up) and the on-demand `systemctl start` path.
- `demo-directory.py` → `/opt/demo-directory/` + `demo-directory.service` → `/etc/systemd/system/` + `demo404.cfg` → `/etc/haproxy/conf.d/` — the demo directory / 404 page for `*.demo.data-ui.com`; see "How HAProxy Routing Works" in `docs/deployment.md`. Managed by the `demo-directory` CLI → `/usr/local/bin/demo-directory` (`install`, `update`, `status`, `start`, `stop`).

Installation comes from the **staging checkout**: `scripts/data-ui staging update` installs exactly this set (pg-profile.yaml → `/usr/local/lib/data-ui/` 0644, reference copy only — compose reads the checkout's copy; profile-run.sh and petting-zoo-reset.sh → `/usr/local/lib/data-ui/` 0755; the three unit files → `/etc/systemd/system/` 0644), each via sudo with an atomic same-directory temp file, everything before a checked `daemon-reload`. `demo-directory` is NOT touched by staging update (it runs from `/opt/demo-directory`; its CLI still defaults `DATA_UI_CHECKOUT` to the development checkout — any `scripts/data-ui` it calls follows the normal staging routing). Manual installation, as a last resort, follows the same destinations and modes.

Host state lives under `/data/data-ui` (profiles, snapshots, deploy; D15) — env-overridable via DATA_UI_STATE, but every context (units, shells, the reset script) must agree or state silently splits.

Credentials never live in this repo: per-profile `profile.env` (mode 600) and `/data/data-ui/CREDENTIALS` carry them.

## Variables

| Variable | Default | Meaning |
|---|---|---|
| `DATA_UI_CHECKOUT` | `/data/data-ui/repos/staging` | the checkout host apps load (wrapper, reset script, and units all pin it) |
| `STAGING_CHECKOUT` | `/data/data-ui/repos/staging` | staging clone location |
| `DEPLOY_CHECKOUT` | `/data/data-ui/repos/production` | production clone location |
| `STAGING_HOST` / `DEPLOY_HOST` | `evo-x2` | must name the same host for now (one-host rule; a split is a separate item) |
| `DEPLOY_BOOTSTRAP_CHECKOUT` | `~/common-lisp/data-ui` | last-resort remote launcher when no production clone exists yet |
| `CHECKOUT_LOCK_ROOT` | `/data/data-ui/repos` | production/staging flock directory |
| `STAGING_HEALTH_TIMEOUT` | `300` | per-profile restart gate deadline, seconds. The default was raised from the old 120 s (which was tuned to *warm* starts) because `staging update` pre-warms SBCL, making a cold start the normal case the deadline must cover; it is env-overridable, and the pre-warm is what keeps the deadline measuring health rather than compilation |
| `MODELBANK_DRAIN_TIMEOUT` | `600` | Model Bank drain deadline, seconds |

## Update flow

`staging update` first captures the `dataui-reset.timer` state (the restoration truth), waits out a running nightly reset (up to 15 minutes; never stops it), pauses the timer, quiesces Model Bank, selects the staging clone (fetch → write `target-staging-sha` → fast-forward), then as a child: `npm ci` + `npm run build` (the committed `web/package-lock.json` is required; there is no `npm install` fallback), a **pre-warm** (a fresh SBCL loads `:data-ui` under the ASDF load contract and exits — a compile gate that also fills the `~/.cache/common-lisp` FASL cache every `dataui@` restart shares; it runs without sudo and as the unit's user, because a root-owned cache would make the health deadline measure compilation again), the ops installation, and plain `systemctl restart` per active non-Model-Bank profile, each gated (journal identity for the current invocation + loopback `/health` + the exposed domain's `/health`) before the next. The first profile is the canary; there is no second pass. Publishing comes before the first bootstrap: the selected upstream (normally `private/master`) must already contain the updater.

**`Persistent=true` catch-up:** restoring an originally *active* timer after the update window crossed 04:10 fires the reset at once, on success or failure — documented systemd.timer(5) behavior (stopped counts as inactive), accepted. An originally inactive timer stays inactive. The nightly set is only enabled `e-demo` profiles, so catch-up cannot restart Model Bank.

## Model Bank

Model Bank is quiesced before every staging checkout mutation (always — no exemption even when an `activated` record exists) and stays unexposed until `staging activate-modelbank --expose`, because every Deploy runs `scripts/data-ui` afresh from Model Bank's loaded checkout: once that checkout's script changes, old Lisp would call a new script with old-contract inputs. The quiescence barrier: unit state check (active/inactive proceed; anything else fails closed) → the `activation.state` file (the operator writes `original-exposed` once, at the very first quiescence; every `phase`/SHA transition belongs to the update/activation verbs — `phase=quiesced` and cleared SHAs before any checkout mutation, `target-staging-sha` only after a successful fetch) → unexpose **confirmed at the HAProxy reload** (disk state goes false before routing stops; a failed reload aborts) → drain without the production lock (a running worker's script needs that lock to finish), up to `MODELBANK_DRAIN_TIMEOUT`, via the query `select model_name from rt_models where model_deploy_status = 'running' or model_generate_status = 'running'` — a failed query is a failed drain, never empty, and a stranded row is never reset → only then the production lock, plus one recheck.

`activation.state` (mode 600, beside the profile, never sourced) holds exactly `original-exposed=0|1`, `phase=quiesced|activated`, `target-staging-sha=<full SHA|->`, `activated-staging-sha=<full SHA|->`. `staging activate-modelbank` (default `--leave-unexposed`) takes both locks itself, refuses an e-demo-stamped Model Bank (the nightly reset would restart it outside quiescence), plain-restarts the unit, verifies journal identity (cursor + InvocationID), loopback `/health`, and — through the unit's Swank passenger — the live image's `*package-root*` and deploy script path; on success it sets `phase=activated` and `activated-staging-sha=<staging HEAD>`. The `--expose` fast path (already activated at the current HEAD) revalidates without restarting — never restart a healthy current Model Bank just to change exposure — and restores only the recorded original exposure, gated on protocol readiness (the `DATA_UI_REEXEC_PROTOCOL` line in `refs/remotes/public/master`'s `scripts/lib/checkout.sh` equals staging's; a mismatch means publish to public first). The record is removed after a verified final exposure, or a verified activation of an originally-unexposed instance. `dataui-reset.service` is `Type=oneshot` without `RemainAfterExit`: a running reset reads `active` or `activating`, finished reads `inactive`, anything else fails closed.

## Orphans

`scripts/data-ui staging modelbank-orphans` is the read-only report of `models/local/deploy-incoming/` entries: one line per orphan with path, state, and reason — missing incoming file, dead child without an ack, malformed or unrecognizable record, or stale temp. The startup reaper handles the provable cases automatically; everything this reports needs per-entry manual remediation (inspect the model, finish or discard it, then remove the files by hand).

