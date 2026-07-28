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

- **Swank port:** 4010
- **HTTP port:** 8081
- **Database port:** 5444

Connect to the REPL from Emacs using `M-x slime-connect` (or your
preferred Slime client) on the reported Swank port. When you exit the
REPL, the database container is stopped automatically.

**Example:**

    scripts/data-ui repl

#### `db`

Starts the PostgreSQL database container, initializes it, and opens an
interactive `psql` session. Useful for manual database inspection.

**Example:**

    scripts/data-ui db

#### `psql`

Connects to the REPL database via `psql` without starting or stopping
the container. Use this when the REPL is already running and you want
to inspect the database alongside it.

**Example:**

    scripts/data-ui psql

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

    scripts/data-ui deploy
    DRY_RUN=1 scripts/data-ui deploy

#### `delete`

Undeploys the default model's app. Deletes the Kubernetes namespace
and persistent volumes, wipes application data on the host, removes the
HAProxy backend and map entry, and removes deploy state (including
secrets). Asks for confirmation unless `FORCE=1` is set.

Docker images and git release tags are left in place.

**Examples:**

    scripts/data-ui delete
    FORCE=1 scripts/data-ui delete

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
`~/.local/state/data-ui-snapshots/`.

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

**How restore works:** The restore drops the entire `public` schema
(tables, functions, triggers, sequences) and recreates it before
loading the dump. This ensures a clean restore regardless of what
tables the currently-loaded model has created. The restore is
non-transactional — it either succeeds completely or leaves the
schema partially rebuilt (same as any `pg_restore`).

#### `help`

Displays the built-in help text.

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
