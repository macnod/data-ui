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
`~/.local/state/data-ui-host/<profile>/profile.env` (complete
partition) and auto `set-model` when `MODEL_NAME` is set.

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

#### `create-profile`

Creates a host profile for a top-level model. The model must exist
as `models/<model>.lisp` (test fixtures under `models/test/` are not
accepted). The profile name defaults to the model name; pass a second
argument for a different profile name.

    scripts/data-ui create-profile <model> [profile]

Behavior:

- Fails if the profile already exists (no `--force`; use
  `delete-profile` first)
- Allocates free HTTP / Swank / DB ports, skipping reserved ports
  (unnamed repl, tests, deploy compile), ports claimed by other
  profiles' `profile.env` files, and live listeners
- Generates fresh secrets and prints the admin password on stdout
- Writes `profile.env` (mode 600) with absolute paths; hyphens in the
  profile name become underscores in `DB_NAME` / `DB_USER` (the
  container name stays hyphenated)
- Does not start Postgres or the REPL

**Examples:**

    scripts/data-ui create-profile todos
    scripts/data-ui create-profile todos todos-2

#### `delete-profile`

Deletes a host profile: runs `docker compose down --volumes` for its
database project and removes the profile directory.

    scripts/data-ui delete-profile <profile>

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

    scripts/data-ui delete-profile todos
    FORCE=1 scripts/data-ui delete-profile todos-2

#### `expose-profile`

Makes a locally-run host profile reachable from the outside through
the existing HAProxy TLS front door:
`https://<model :domain>` → `127.0.0.1:<HTTP_PORT>`.

    scripts/data-ui expose-profile <profile>

Behavior:

- Primary profiles only (profile name equals its `MODEL_NAME`;
  `modelbank` yes, `modelbank-2` no)
- Requires `sudo` and must run on `DEPLOY_HOST` (default `evo-x2`),
  from a checkout containing `models/<model>.lisp` (and `ros`)
- The hostname comes from the model's `:domain`; the model must have
  one, or the profile cannot be exposed
- Refuses if the domain is claimed by a deployed instance (use
  `delete` first) or by another profile (use `unexpose-profile`
  first); re-exposing the same profile is idempotent
- TLS is covered by the existing `*.demo.data-ui.com` wildcard
  certificate — no TLS work needed
- Warns (does not fail) if the profile is not listening yet;
  expose-then-start is the intended workflow

**Example:**

    scripts/data-ui expose-profile modelbank

#### `unexpose-profile`

Removes a profile's HAProxy exposure (backend drop-in and map
entry). Idempotent: prints "not exposed" and exits 0 when there is
nothing to remove. Requires `sudo` and must run on `DEPLOY_HOST`.

    scripts/data-ui unexpose-profile <profile>

**Example:**

    scripts/data-ui unexpose-profile modelbank

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

Compiles the system (RBAC + Data UI) without starting a database or
running tests. Verifies that the code loads cleanly.

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

    scripts/data-ui field title
    scripts/data-ui field name

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
secrets) lives outside the repo in
`~/.local/state/data-ui-deploy/`.

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

Saves or restores the database for model switching. Since `set-model`
drops all application tables when switching models, snapshots let you
preserve a model's data and restore it after switching back.

Subcommands:

- `save [name]` — Save the current database state
- `restore [name]` — Restore a saved snapshot
- `list` — List available snapshots
- `drop [name]` — Delete a snapshot

If `[name]` is omitted, the script auto-detects the currently-loaded
model name by querying the running instance's `/api/info` endpoint.
Snapshots are stored as `pg_dump` custom-format files in
`~/.local/state/data-ui-snapshots/`. A snapshot name resolves a host
profile when
`~/.local/state/data-ui-host/<name>/profile.env` exists
(suffix strip falls back to the profile prefix).

Dump and restore run inside the PostgreSQL server container
(`compose exec`), so the client tools always match the server version.
A snapshot taken on one machine restores on another regardless of the
host's installed PostgreSQL client (a newer host client, e.g. pg_restore
18 against a postgres:16 server, emits SQL the server rejects).

**Typical workflow:**

If you're running `model-1` and want to work on something else without
losing the `model-1` data:

1. Create a snapshot: `scripts/data-ui snapshot save`
2. Switch to another model in Lisp: `(set-model "model-2")`
3. Work with the other model for a while
4. Switch back to the original model: `(set-model "model-1")`
5. Restore the original model's data: `scripts/data-ui snapshot restore`

If you need multiple snapshots of the same model, pass the snapshot
name you want to use:

- `scripts/data-ui snapshot save modelbank-1`
- `scripts/data-ui snapshot save todos-2`

**Listing and deleting snapshots:**

    scripts/data-ui snapshot list
    scripts/data-ui snapshot drop modelbank-1

**How restore works:** Before touching anything, the restore validates
that the snapshot is a readable archive (`pg_restore -l`) and takes a
safety dump of the current database. It then drops the entire `public`
schema (tables, functions, triggers, sequences), recreates it, and
loads the dump with `--single-transaction`, so a mid-restore failure
leaves the schema as it was (empty, in the drop-then-fail window). If
the restore fails anyway, the script attempts to roll back from the
safety dump and keeps it (named `pre-restore-<name>-<timestamp>.dump`)
if the rollback also fails. `save` writes to a temp file first, so a
failed dump never clobbers an existing snapshot. Both commands require
the instance's PostgreSQL to be running (`scripts/data-ui repl
[profile]`).

#### `help`

Displays the built-in help text.

### Host profiles

Named local instances live under `~/.local/state/data-ui-host/`. A
profile exists if and only if `<name>/profile.env` exists. Profile
identity is the directory name. Create profiles with `create-profile`
and destroy them with `delete-profile`. A primary profile can be made
publicly reachable through HAProxy with `expose-profile` (and removed
again with `unexpose-profile`).

Unnamed `repl` / `db` / `psql` still use built-in dev defaults
(HTTP 8081, Swank 4010, DB 5444, `tests/shared-files/`) and do not
create a profile.

`profile.env` is the complete partition. Required keys:

- `HTTP_PORT`, `SWANK_PORT`, `SWANK_INTERFACE`
- `DB_PORT`, `DB_NAME`, `DB_USER`, `DB_PASSWORD`
- `DB_CONTAINER`, `DB_SERVICE`, `DB_DOCKER_COMPOSE`
- `ADMIN_PASSWORD`, `JWT_SECRET`
- `DOCUMENT_ROOT`, `FS_TEMP_DIRECTORY`, `LOG_FILE`
- `MODEL_NAME`

Optional keys:

- `HTTP_HOST`, `DB_HOST`, `WEB_DIRECTORY`, `LOG_SEVERITY`, `RUN_TESTS`

Per-profile runtime files live beside `profile.env`:

- `repl.log`, `start.log`, `fifo`
- `files/` (`DOCUMENT_ROOT`)
- `temp/` (`FS_TEMP_DIRECTORY`)

Host-wide files (`tests-run.log`, later `ports.lock`) stay at the
root of `data-ui-host/`. Local file data does not live under
`~/k3d/volumes/`; that tree is for cluster PVs.

Postgres data for a named profile lives in the named Docker volume
`${DB_CONTAINER}-pgdata` (for modelbank: `pg-data-ui-modelbank-pgdata`).
`repl` / `db` stop the container on exit but keep that volume.
Unnamed `repl` uses the same compose file, so its volume is
`pg-data-ui-repl-pgdata`. Tests use a separate compose file with no
named volume.

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
- `ADMIN_PASSWORD` (default: `admin-password-1`)

**Deployment (override with environment variables):**

- `DEPLOY_HOST` (default: `evo-x2`)
- `DEPLOY_CHECKOUT` (default: `$HOME/deploy/data-ui`)
- `DEPLOY_STATE_DIR` (default: `~/.local/state/data-ui-deploy`)
- `K3D_CLUSTER` (default: `evo-x2`)
- `DRY_RUN` (set to `1` for dry-run deploys)
- `FORCE` (set to `1` to skip delete confirmation)

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
3. Compares `HEAD` against `public/master`; exits if there is nothing
   to publish
4. Counts the commits to be squashed
5. Soft-resets to `public/master`, then creates a single new commit
   with the provided message
6. Pushes to `public`
7. Force-pushes the squashed history to `private` (so both remotes
   agree on `master`)

### Examples

    scripts/publish-data-ui "Add snapshot command"
    scripts/publish-data-ui "Fix RBAC role assignment"

If no argument is given, the commit message defaults to `Update`.
