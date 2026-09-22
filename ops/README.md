# ops/ — host-side operations for Data UI demo profiles

Sources for the petting-zoo host machinery (demo-apps-plan D3/D15).
Edit here, reinstall; never edit the installed copies.

Contents:

- `pg-profile.yaml` — per-profile docker compose file (bind-mounted
  pgdata, loopback-only ports, user = unit user).
- `profile-run.sh` → `/usr/local/lib/data-ui/profile-run.sh` — the
  dataui@.service ExecStart wrapper.
- `dataui@.service` → `/etc/systemd/system/dataui@.service` — the
  systemd template unit.
- `petting-zoo-reset.sh` → `/usr/local/lib/data-ui/` — nightly golden
  reset (Phase 7). Runs as root; no args = every enabled e-demo,
  explicit args (any class, e.g. modelbank for a grungy-day restore,
  D13) bypass the enabled check.
- `dataui-reset.service` + `dataui-reset.timer` →
  `/etc/systemd/system/` — the 04:10 schedule (Persistent=true
  boot catch-up) and the on-demand `systemctl start` path.
- `demote-vips.sh` → `/usr/local/lib/data-ui/` — bulk VIP demotion
  (D13).

Install (from the checkout root):

    sudo install -D -m 644 ops/pg-profile.yaml \
         /usr/local/lib/data-ui/pg-profile.yaml   # reference copy only;
                                                   # compose reads the
                                                   # checkout's copy
    sudo install -D -m 755 ops/profile-run.sh \
         /usr/local/lib/data-ui/profile-run.sh
    sudo install -D -m 644 ops/dataui@.service \
         /etc/systemd/system/dataui@.service
    sudo install -D -m 755 ops/petting-zoo-reset.sh \
         /usr/local/lib/data-ui/petting-zoo-reset.sh
    sudo install -D -m 644 ops/dataui-reset.service \
         /etc/systemd/system/dataui-reset.service
    sudo install -D -m 644 ops/dataui-reset.timer \
         /etc/systemd/system/dataui-reset.timer
    sudo systemctl daemon-reload
    sudo systemctl enable --now dataui-reset.timer

Host state lives under `/data/data-ui` (profiles, snapshots, deploy;
D15) — env-overridable via DATA_UI_STATE, but every context (units,
shells, the reset script) must agree or state silently splits.

Credentials never live in this repo: per-profile `profile.env` (mode
600) and `/data/data-ui/CREDENTIALS` carry them.
