# Data UI Scripts

Reference documentation for the shell scripts in `scripts/`.

---

## `data-ui`

The primary entry point for development, testing, deployment, and
database management. All commands are run from the repository root.

### Synopsis

    scripts/data-ui <command> [arguments]

### Commands

#### `repl`

Starts a Common Lisp REPL (via Roswell) with the Data UI system loaded
and initialized. Starts a PostgreSQL container, initializes the
database, builds the frontend, and starts a Swank server.

Without `[profile]`, uses built-in dev defaults (HTTP 8081, Swank
4010, DB 5444) and does not create a profile.

With `[profile]`, loads
`/data/data-ui/profiles/<profile>/profile.env` (complete partition)
and auto `set-model` when `MODEL_NAME` is set.

Connect to the REPL from Emacs using `M-x slime-connect` (or your
preferred Slime client) on the reported Swank port. When you exit the
REPL, the database container is stopped automatically.

**Examples:**

    scripts/data-ui repl
    scripts/data-ui repl modelbank

#### `db`

Starts the PostgreSQL database container, initializes it, and opens an
interactive `psql` session. Useful for manual database inspection.

Without `[profile]`, uses the same built-in dev defaults as `repl`
and does not create a profile. With `[profile]`, uses that profile's
database settings.

**Examples:**

    scripts/data-ui db
    scripts/data-ui db modelbank

#### `psql`

Connects to the REPL database via `psql` without starting or stopping
the container. Use this when the REPL is already running and you want
to inspect the database alongside it.

Without `[profile]`, uses built-in dev defaults. With `[profile]`,
loads that profile and connects to its database.

**Examples:**

    scripts/data-ui psql
    scripts/data-ui psql modelbank

#### `profile`

Manages host profiles — named local instances (see
[Host profiles](#host-profiles)). `create` makes one for a top-level
model, `delete` destroys one, `list` shows the existing ones, and
`expose` / `unexpose` toggle its reachability through the HAProxy TLS
front door.

Subcommands:

- `create <model> [profile]` — Create a host profile for a model
- `delete <profile>` — Delete a host profile
- `list` — List host profiles with model, ports, and status
- `expose <profile>` — Make a profile publicly reachable via HAProxy
- `unexpose <profile>` — Remove the HAProxy exposure

**`create <model> [profile]`** creates a host profile for a top-level
model. The model must exist as `models/<model>.lisp` (test fixtures
under `models/test/` are not accepted). The profile name defaults to
the model name; pass a second argument for a different profile name.

    scripts/data-ui profile create <model> [profile]

Behavior:

- Fails if the profile already exists (no `--force`; use
  `profile delete` first)
- Allocates free HTTP / Swank / DB ports, skipping reserved ports
  (unnamed repl, tests, deploy compile), ports claimed by other
  profiles' `profile.env` files, and live listeners
- Generates fresh secrets and prints the admin password on stdout
- Writes `profile.env` (mode 600) with absolute paths; hyphens in the
  profile name become underscores in `DB_NAME` / `DB_USER` (the
  container name stays hyphenated)
- Does not start Postgres or the REPL

**Examples:**

    scripts/data-ui profile create todos
    scripts/data-ui profile create todos todos-2

**`delete <profile>`** deletes a host profile: runs
`docker compose down --volumes` for its database project and removes
the profile directory.

    scripts/data-ui profile delete <profile>

Behavior:

- Refuses if the profile's HTTP or DB port is listening (exit the
  REPL first)
- Asks you to type the profile name to confirm, unless `FORCE=1`
- Removes the profile's HAProxy exposure if present (on `DEPLOY_HOST`;
  otherwise prints "HAProxy cleanup skipped")
- Leaves snapshots and deploy state alone (matching snapshots are
  mentioned, not deleted)
- Does not touch the unnamed-repl, test, or deploy containers

**Examples:**

    scripts/data-ui profile delete todos
    FORCE=1 scripts/data-ui profile delete todos-2

**`expose <profile>`** makes a locally-run host profile reachable
from the outside through the existing HAProxy TLS front door:
`https://<model :domain-stg>` → `127.0.0.1:<HTTP_PORT>`.

    scripts/data-ui profile expose <profile>

Behavior:

- Primary profiles only (profile name equals its `MODEL_NAME`;
  `modelbank` yes, `modelbank-2` no)
- Requires `sudo` and must run on `DEPLOY_HOST` (default `evo-x2`),
  from a checkout containing `models/<model>.lisp` (and `ros`)
- The hostname comes from the model's `:domain-stg` when present,
  else `:domain` with `-stg` suffixed onto the first DNS label
  (the compiler's default derivation, duplicated here because the
  script reads the raw model file); the model must have a
  `:domain`, or the profile cannot be exposed
- Refuses if the domain is claimed by a deployed instance (use
  `delete` first) or by another profile (use `profile unexpose`
  first); re-exposing the same profile is idempotent
- TLS is covered by the existing `*.demo.data-ui.com` wildcard
  certificate — no TLS work needed
- Warns (does not fail) if the profile is not listening yet;
  expose-then-start is the intended workflow

**Example:**

    scripts/data-ui profile expose modelbank

**`unexpose <profile>`** removes a profile's HAProxy exposure (backend
drop-in and map entry). Idempotent: prints "not exposed" and exits 0
when there is nothing to remove. Requires `sudo` and must run on
`DEPLOY_HOST`.

    scripts/data-ui profile unexpose <profile>

**Example:**

    scripts/data-ui profile unexpose modelbank

#### `tests`

Runs the full FiveAM test suite against a dedicated test database.
Output is suppressed except for test results. The database container is
started and stopped automatically.

- **Database port:** 5445 (separate from the REPL database)

**Example:**

    scripts/data-ui tests

#### `debug`

Same as `tests` but with full output from PostgreSQL and Docker
Compose visible. Useful for diagnosing failures.

**Example:**

    scripts/data-ui debug

#### `compile`

Compiles the system (loads `:rbac` and the Data UI test file) without
starting a database or running tests. Verifies that the code loads
cleanly.

**Example:**

    scripts/data-ui compile

#### `docs`

Generates `README.md` from the RBAC source using
`rbac:generate-readme`. Does not require a database.

**Example:**

    scripts/data-ui docs

#### `stop`

Stops and removes the test database container (including volumes).
Normally not necessary — the `repl` and `tests` commands clean up
after themselves.

**Example:**

    scripts/data-ui stop

#### `field`

Reads a top-level field from `models/<model-name>.lisp`. Used
internally by the deploy pipeline, but can be called directly.

**Example:**

    scripts/data-ui field todos title
    scripts/data-ui field todos name

#### `deploy`

Builds the Docker image for `models/<model-name>.lisp`, generates
Kubernetes manifests, deploys to the k3d cluster, and configures
HAProxy routing for the model's domain.

Requirements:
- Clean git working tree (unless `DRY_RUN=1`)
- `sudo` access (for HAProxy configuration)
- When run on a machine other than `DEPLOY_HOST`, pushes the current
  branch and tag to origin, then re-runs the deploy on `DEPLOY_HOST`
  over SSH

Set `DRY_RUN=1` to generate manifests without deploying — useful for
verifying template changes.

Deployment state (rendered manifests, `ports.lock`, per-instance
secrets) lives outside the repo in `/data/data-ui/deploy/`.

**Examples:**

    scripts/data-ui deploy todos
    DRY_RUN=1 scripts/data-ui deploy todos

#### `delete`

Undeploys the named model's app. Deletes the Kubernetes namespace
and persistent volumes, wipes application data on the host, removes the
HAProxy backend and map entry, and removes deploy state (including
secrets). Asks for confirmation unless `FORCE=1` is set.

Docker images and git release tags are left in place.

**Examples:**

    scripts/data-ui delete todos
    FORCE=1 scripts/data-ui delete todos

#### `traffic`

Shows HAProxy's per-IP rate-limit counters (concurrent connections,
connection rate, request rate) and flags clients near or over the
limits. Requires `sudo` for the HAProxy admin socket.

**Example:**

    scripts/data-ui traffic

#### `snapshot`

Saves or restores a snapshot between explicit environments. A
snapshot is a portable artifact in one shared pool — not "a profile's
memory" — aimed wherever the command says.

Subcommands:

- `save <name> <src-address>` — Save the source's database state
- `restore <name> <dst-address>` — Restore a snapshot onto a destination
- `list` — List the shared pool; names + provenance
- `drop <name>` — Delete a snapshot (dump + files tar + .meta)
- `migrate` — One-time maintenance: rename legacy slots, stub .meta
  (idempotent)

**Addresses:** every address is `<qualifier>:<name>`, both tokens
mandatory on `save` / `restore` — no defaults, no unqualified
fallback. The one-arg forms (`snapshot restore books`,
`snapshot save books-1`) are hard errors that name both legal
shapes. The second token is the SOURCE on `save`, the DESTINATION on
`restore` — same position, same vocabulary, role fixed by the verb.

- `dev:<model>` — the built-in dev environment (the 5444 REPL),
  asserting the loaded model: `dev:books` requires
  `models/books.lisp` and confirms the live model via `/api/info`
  when reachable
- `stg:<profile>` — a host profile: `stg:books` resolves
  `/data/data-ui/profiles/books/profile.env`; model identity is its
  `MODEL_NAME` (the colon-tail is a PROFILE name, not a model name:
  `stg:mybooks` may run `MODEL_NAME=todos`)

Qualifiers are case-sensitive and resolved through a two-arm registry
(`dev`, `stg`; cluster substrates are post-MVP). Snapshot names share
the profile charset (`^[A-Za-z0-9][A-Za-z0-9_-]*$`, colon illegal) —
an address-shaped name (`snapshot save stg:books stg:books`) is a
parse error, not a minted file.

Snapshots live in the shared pool `/data/data-ui/snapshots/` (the
`DATA_UI_STATE` root; see [Host profiles](#host-profiles)). Dump and
restore run inside the PostgreSQL server container (`compose exec`),
so the client tools always match the server version.

**The .meta manifest:** `save` writes `<name>.meta` beside the pair —
key=value lines (model, source-address, timestamp; model-version
best-effort via the model file). Nothing in `.meta` comes from
`/api/info`. `restore` refuses a model mismatch (`.meta.model` vs the
destination address's model — never FORCE-able) and warns when both
sides carry a model version and they differ. A missing `.meta` (old
snapshots) warns — provenance unknown — then proceeds.

**Password re-stamping:** auth rows travel with the dump; the
destination's `ADMIN_PASSWORD` does not. After a successful
`pg_restore`, restore unconditionally re-stamps the destination's
own admin password hash (`UPDATE users SET password_hash = ... WHERE
user_name = 'admin'`, the hash computed exactly as rbac's
`a:password-hash` does it). The working admin password after any
restore is therefore the destination's own: the static
`admin-password-1` for `dev:`, the `ADMIN_PASSWORD` line in
`profile.env` for `stg:`. Cross-environment restores still print a
NOTICE (auth rows otherwise traveled; log in again — sessions break
because the user table was swapped, and the dest `JWT_SECRET` is
unchanged). A snapshot carrying a foreign admin hash — the
historical residue of cross-env auditions — is neutralized by the
next restore into any environment.

**Unreachable `/api/info`:** a login 401 is never "app down" (it is
the expected residue of a cross-env restore) and can never be the
source of identity. Policy splits by verb:

- `save` + unreachable `dev:` refuses (start the dev REPL first)
  unless `FORCE=1`
- `save` + unreachable `stg:` skips the confirm quietly (identity is
  `profile.env`, already checked)
- `restore` + unreachable (either qualifier) skips the confirm — the
  app down is the natural restore shape (the nightly reset and the
  stop verb's p-last refresh both depend on this branch)

(The historical "401 with the app up" save residue — a cross-env
restore leaving the DB answering to the source's password — is gone:
restore re-stamps the destination's own admin hash.)

**Files side:** a snapshot is a triple — `<name>.dump` (the
database), `<name>.files.tar.gz` (the `DOCUMENT_ROOT` tree, whenever
it exists at save time), and `<name>.meta`. The tar is optional
throughout: snapshots without one restore the database only, and a
save with a missing `DOCUMENT_ROOT` warns and saves the DB alone
(still dropping a stale tar). The tar packs the tree under a
canonical top-level `files/` entry regardless of the source's
basename (dev packs `tests/shared-files/`, stg packs
`profiles/<p>/files/`), so tars are portable across environments;
restore accepts exactly one top-level directory of any name and
renames it to the destination's `DOCUMENT_ROOT` basename during the
swap — old tars on disk become portable retroactively.

Restore swaps, never deletes-then-untars: extract to staging, move
the old tree aside as `files.pre-restore-<timestamp>`, move the
fresh one in, and delete the old copy only after the swap succeeds.
An unreadable or wrong-layout tar dies with the database restored
and the files left untouched. If the swap itself is interrupted
between the two `mv`s, the `.pre-restore-` copy (timestamped, beside
`DOCUMENT_ROOT`) recovers the previous tree by a manual `mv`.
`FS_TEMP_DIRECTORY` (two-phase upload staging) is a sibling of
`files/`, so staging garbage is deliberately not captured.

**How restore works:** before touching anything, the restore checks
the `.meta` model against the destination address, validates the
snapshot as a readable archive (`pg_restore -l`), and stages a
safety dump (plus the current files tree) to temp names. It then
drops the entire `public` schema (tables, functions, triggers,
sequences), recreates it, and loads the dump with
`--single-transaction`, so a mid-restore failure leaves the schema
as it was. If the restore fails, the script rolls back from the
staged dump. On success the staged triple rotates onto the
`<dest>-prerestore` slot (keyed on the destination address, colon →
dash: `stg-books-prerestore`, `dev-books-prerestore`) — the
pre-restore state is kept findable, the slot carries its own
`.meta`, and it is overwritten by the next restore. On modelbank,
that slot may be the only copy of the grungy state being destroyed,
so copy it aside first when it matters. `save` stages the triple and
moves it onto the slot names only when dump + `.meta` succeeded (a
new dump beside a stale tar is a mixed state). Both commands require
the destination's PostgreSQL to be running (`scripts/data-ui db` or
`repl` for `dev:`, `e-demo start` / `demo start` for `stg:`).

**Named slots:** slots key on the destination address —
`stg-<p>-golden` (deliberate: nightly reset source for e-demos,
manual grungy-day restore on modelbank), `stg-<p>-last` (automatic,
refreshed by the stop verb: the state at the last deliberate pause),
`stg-<p>-prerestore` / `dev-<model>-prerestore` (automatic, rotated
by restore; dev has no golden/last — no nightly reset, no stop verb
rewrites its history). Nothing accumulates; the dated
`pre-restore-*` branch is retired. Hand-named snapshots are
deliberate history. Reserved names (runbook note): a hand-run `save`
naming a slot overwrites it; a hand-run `drop` of a slot silently
kills the nightly's golden (or the demo class's last) — no
confirmation exists; and the pool is flat, not per-address —
`save books-1 stg:books` then `save books-1 dev:books` overwrites
the first.

**Typical workflows:**

Audition new data in stg, then discard it:

    scripts/data-ui snapshot save books-1 dev:books
    scripts/data-ui snapshot restore books-1 stg:books
    # ... play in stg; the 04:10 timer restores stg-books-golden

Promote auditioned data to golden:

    scripts/data-ui snapshot save stg-books-golden stg:books

Staging data into dev:

    scripts/data-ui snapshot restore stg-books-golden dev:books

**Listing and deleting snapshots:**

    scripts/data-ui snapshot list
    scripts/data-ui snapshot drop books-1

**One-time migration:** after landing this CLI on a host with
legacy profile-keyed slots, run `scripts/data-ui snapshot migrate`
(as the state's owner — macnod, never sudo: the pool is macnod-owned
and a root-written stub `.meta` sits unwritable in the next save's
way). It renames `<p>-{golden,last,prerestore}` to `stg-<p>-*` and
writes stub `.meta` files, idempotently per file — safe to re-run if
interrupted. The CLI change, the migration, and the ops-script
reinstall are one atomic change before the 04:10 timer can fire
(disable the timer across the window).

#### `help`

Displays the built-in help text.

#### `e-demo` / `demo`

Demo lifecycle verbs for host profiles. `e-demo` is the *ephemeral*
class (data is disposable, included in the 04:10 nightly golden
reset); `demo` is the persistent class (no nightly reset; the golden
stays manual-only).

- `e-demo start <profile> [--from <snapshot>]` — start (optionally
  seeding from a named snapshot instead of the golden)
- `e-demo stop <profile>` — teardown; refreshes the `stg-<p>-last`
  snapshot first
- `e-demo reset [profile...]` — nightly golden reset on demand: no
  args = every enabled e-demo; explicit profiles bypass the enabled
  check
- `demo start|stop|reset` — same verbs for the persistent class

Both drive the `dataui@<profile>` systemd units. See
[Host profiles](#host-profiles) and `docs/snapshots.md`.

### Host profiles

Named local instances live under `/data/data-ui/profiles/` (the
`DATA_UI_STATE` root, env-overridable; snapshots under
`/data/data-ui/snapshots/`, deploy state under `/data/data-ui/deploy/`).
A profile exists if and only if `<name>/profile.env` exists. Profile
identity is the directory name. Create profiles with `profile create`
and destroy them with `profile delete`. A primary profile can be made
publicly reachable through HAProxy with `profile expose` (staging
hostname: the model's `:domain-stg`, or the `-stg`-suffixed
`:domain`; see [Environments](`docs/deployment.md`)) and removed
again with `profile unexpose`.

Unnamed `repl` / `db` / `psql` still use built-in dev defaults
(HTTP 8081, Swank 4010, DB 5444, `tests/shared-files/`) and do not
create a profile.

`profile.env` is the complete partition. Required keys:

- `HTTP_PORT`, `SWANK_PORT`, `SWANK_INTERFACE`
- `DB_PORT`, `DB_NAME`, `DB_USER`, `DB_PASSWORD`
- `DB_CONTAINER`, `DB_SERVICE`, `DB_DOCKER_COMPOSE`
- `PGDATA_DIR`, `PG_UID`, `PG_GID` (the pgdata bind mount and the unit
  user's ids — never `$(id -u)` of the caller)
- `ADMIN_PASSWORD`, `JWT_SECRET`
- `DOCUMENT_ROOT`, `FS_TEMP_DIRECTORY`, `LOG_FILE`
- `MODEL_NAME`

Optional keys:

- `HTTP_HOST`, `DB_HOST`, `WEB_DIRECTORY`, `LOG_SEVERITY`, `RUN_TESTS`
- `PROTECTED_USERS` (FR-12; stamped `demos`)
- `DEMO_CLASS` (D14; stamped `e-demo` or `demo` by the start verb —
  absent = not in the nightly set)

Per-profile runtime files live beside `profile.env`:

- `repl.log` (`LOG_FILE` default; the systemd unit wrapper unsets it,
  so demo units log to journald instead)
- `files/` (`DOCUMENT_ROOT`)
- `temp/` (`FS_TEMP_DIRECTORY`)

`ports.lock` (deploy-side port cache) lives under
`/data/data-ui/deploy/`. Local file data does not live under
`~/k3d/volumes/`; that tree is for cluster PVs.

Postgres data for a named profile lives in the bind-mounted
`<profile>/pgdata/` directory (D16; mode 700, owned by the unit user —
the container runs as that uid via `user:` in `ops/pg-profile.yaml`,
and the DB port publishes loopback-only). Profile databases use
`ops/pg-profile.yaml`; the old `tests/docker-compose-pg-data-ui-repl.yaml`
remains for the dev `repl` verb only.

Demo profiles run as systemd units (`ops/dataui@.service` +
`/usr/local/lib/data-ui/profile-run.sh`), started with the lifecycle
verbs — `e-demo start/stop/reset <p>` (nightly-reset class) and `demo
start/stop/reset <p>` (no-reset class). `reset` is the nightly golden
reset on demand: no args (e-demo only) runs the enabled set through
`dataui-reset.service`; explicit profiles (either verb) run the
grungy-day restore, any class. It reinstalls `ops/petting-zoo-reset.sh`
to `/usr/local/lib/data-ui/` first when the installed copy is stale —
the 04:10 timer runs the installed copy, not the checkout's. `repl` is
a development verb for the throwaway 5444 database; there is no REPL
path into a demo profile — Slime attaches as a passenger to the unit's
swank port.

### Environment Variables

The script reads several environment variables with hardcoded
defaults. Override them by exporting before invocation.

**Database:**

- `DB_HOST` (default: `127.0.0.1`)
- `DB_PORT` (default: varies by subcommand — 5444 for repl, 5445 for tests)
- `DB_NAME` (default: `dataui`)
- `DB_USER` (default: `dataui`)
- `DB_PASSWORD` (default: `dataui-password`)

**Application:**

- `HTTP_HOST` (default: `127.0.0.1`)
- `HTTP_PORT` (default: varies by subcommand — 8081 for repl)
- `ADMIN_PASSWORD` (default: `admin-password-1` — the static
  development admin password; profiles and deploys carry their own.
  Snapshot restore re-stamps this value — the destination's own —
  into the restored database's admin hash.)

**Deployment (override with environment variables):**

- `DEPLOY_HOST` (default: `evo-x2`)
- `DEPLOY_CHECKOUT` (default: `$HOME/deploy/data-ui`)
- `DEPLOY_STATE_DIR` (default: `/data/data-ui/deploy/`)
- `DATA_UI_STATE` (default: `/data/data-ui`; the root for profiles,
  snapshots, and deploy state)
- `K3D_CLUSTER` (default: `evo-x2`)
- `DRY_RUN` (set to `1` for dry-run deploys)
- `FORCE` (set to `1` to skip the delete / profile-delete
  confirmations, and as the one escape when `snapshot save` hits an
  unreachable `dev:` `/api/info`. It NEVER overrides a snapshot
  model mismatch, and the restore-unreachable skip needs no FORCE.)

---

## `publish-data-ui`

Squashes private work into a clean commit and pushes to the public
remote, then syncs the squashed history back to the private remote.
This keeps the public history clean (one commit per publish) while the
private remote retains full development history.

### Synopsis

    scripts/publish-data-ui "commit message"

### Prerequisites

- Must be on the `master` branch
- Working tree must be clean (all changes committed to private)
- The `public` and `private` git remotes must be configured

### What It Does

1. Verifies the current branch is `master`
2. Checks for uncommitted changes
3. Fetches both remotes and merges incoming `private/master` and
   `public/master` work into `master` (nothing is missed)
4. Backs up `master` to `private` with a plain push (local `master`
   is never reset and `private` is never force-pushed)
5. Compares `HEAD` against `public/master`; exits if there is nothing
   to publish
6. Counts the commits to be squashed, then builds the squash with
   `git commit-tree` (master itself is untouched)
7. Pushes the squashed history to `public`

### Examples

    scripts/publish-data-ui "Add snapshot command"
    scripts/publish-data-ui "Fix RBAC role assignment"

If no argument is given, the commit message defaults to `Update`.
