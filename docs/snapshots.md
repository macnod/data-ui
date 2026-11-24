# Snapshots

A snapshot captures a Data UI environment — its database and uploaded files — as a portable artifact in one shared pool (`/data/data-ui/snapshots/`, the `DATA_UI_STATE` root). Snapshots move state between environments: audition dev data in staging, promote auditioned data to golden, or bring staging data back into dev.

All commands run from the repo root via `scripts/data-ui snapshot …`. For the command reference see [cli.md](cli.md); for the surrounding deployment machinery see [deployment.md](deployment.md); for changing a model whose data must survive a snapshot round trip (the restore-then-reload ordering, `heal-model` vs restart, hand SQL) see [migration.md](migration.md).

## The grammar

```sh
scripts/data-ui snapshot save    <name> <src-address>
scripts/data-ui snapshot restore <name> <dst-address>
scripts/data-ui snapshot list
scripts/data-ui snapshot copy    <src-name> <dst-name> [<model>]
scripts/data-ui snapshot drop    <name>
scripts/data-ui snapshot migrate        # one-time maintenance
```

Every address is `<qualifier>:<name>`, both tokens mandatory — no defaults, no unqualified fallback:

- `dev:<model>` — the built-in dev environment (the 5444 REPL), asserting the loaded model. `dev:books` requires `models/books.lisp` or `models/local/books.lisp` (set-model's order) to exist and confirms the live model.
- `stg:<profile>` — a host profile. `stg:books` resolves through `/data/data-ui/profiles/books/profile.env`; the model is the profile's `MODEL_NAME` (the tail is a profile name, not a model name).
- `prd:<instance>` — a k3d production deploy. `prd:todos` resolves from the live cluster (namespace `dataui-<name>`); the tail IS the model name. Runs on the deploy host only, from a checkout (the verb `cd`s to it). See [Production](#production).

The second token is the SOURCE on `save` and the DESTINATION on `restore` — same position, same vocabulary, role fixed by the verb. The one-arg forms (`snapshot save books-1`, `snapshot restore books`) are hard errors that name both legal shapes. Snapshot names share the profile charset `^[A-Za-z0-9][A-Za-z0-9_-]*$` (colon illegal), so a name can never masquerade as an address.

## Examples

### Create a snapshot of books in the development environment

```sh
scripts/data-ui repl     # terminal 1: dev REPL + Postgres on 5444
```

In the REPL, load the model:

```lisp
(set-model "books")
```

Then save:

```sh
scripts/data-ui snapshot save books-1 dev:books
```

### Restore that snapshot to books in the staging environment

```sh
scripts/data-ui snapshot restore books-1 stg:books
```

The profile's Postgres must be up (an `e-demo start books` brings the whole unit; the restore itself only needs the database). Stopping the app unit first — what the nightly reset does — keeps the swap quiescent, but restoring over the running app works too (the audition flow relies on it).

This is a cross-environment restore: read the password notice below. The admin password is re-stamped to the destination's own, so no manual truing up is needed.

### Make books (an e-demo) reset to the new snapshot

e-demos are nightly-reset: at 04:10 the timer restores `stg-books-golden` into `stg:books`. Making the reset use the new data means re-taking golden from the running staging instance after auditioning it:

```sh
scripts/data-ui snapshot save stg-books-golden stg:books
```

Every nightly reset from then on restores this state.

### Create a snapshot of books in the staging environment

```sh
scripts/data-ui snapshot save books-stg-1 stg:books
```

Pick any name that is not a reserved slot name (see [Reserved names](#reserved-names)).

### Restore that snapshot to books in the development environment

Postgres up, REPL down is the natural restore shape (never drop `public` under a live REPL connection pool):

```sh
scripts/data-ui db     # terminal 1: dev Postgres; psql stays a client
scripts/data-ui snapshot restore books-stg-1 dev:books
```

Dev's pre-restore state is saved to `dev-books-prerestore`, and the admin password is re-stamped to dev's own static `admin-password-1` (restore always re-stamps; notice printed).

### More examples

List and delete:

```sh
scripts/data-ui snapshot list
scripts/data-ui snapshot drop books-1
```

Start a demo from an explicit snapshot:

```sh
scripts/data-ui e-demo start books --from books-1
```

Undo a bad restore — the prerestore slot holds the destination's pre-restore state (restoring it rotates the slot again, as always):

```sh
scripts/data-ui snapshot restore stg-books-prerestore stg:books
```

What refusal looks like (each error names the legal shapes):

```sh
scripts/data-ui snapshot restore books     # missing address
scripts/data-ui snapshot save x dev:nosuch # unknown model / instance
scripts/data-ui snapshot restore books-1 dev:todos   # model mismatch
```

A model mismatch is never overridable — `FORCE=1` does not apply.

## Runbook: a dev → staging round trip

A five-step loop exercising every verb and both addresses — the audition shape in miniature (`A` and `B` are placeholder snapshot names; the `dev:` tail is a model name, the `stg:` tail a profile name):

1. Save snapshot A from dev.
2. Restore A back to dev.
3. Restore A to staging.
4. Save snapshot B from staging.
5. Restore B to staging.

### 0. The password story (restore re-stamps)

Auth rows travel with the dump, but every restore unconditionally re-stamps the destination's own admin password hash afterward (`ADMIN_PASSWORD` from the destination env: the static `admin / admin-password-1` for `dev:`, the profile's `profile.env` line for `stg:`). Steps 2–5 therefore always leave each environment answering to its own password; nothing needs truing up afterward. Old JWT sessions still break (the user table was swapped); log in again with the destination's own password.

### 1. Save A from dev — app up, model loaded

```sh
scripts/data-ui repl     # terminal 1: dev REPL + Postgres on 5444
```

In the REPL:

```lisp
(set-model "books")
```

Then:

```sh
scripts/data-ui snapshot save A dev:books
```

A `dev:` save refuses when `/api/info` is unreachable — the confirm must see the live model.

### 2. Restore A to dev — Postgres up, REPL down

Quit the SBCL image from step 1 (never drop `public` under a live REPL connection pool) and keep Postgres up:

```sh
scripts/data-ui db
scripts/data-ui snapshot restore A dev:books
```

Same address, so no cross-env notice; the admin password is re-stamped (to dev's own — a no-op when it already matched). The undo slot `dev-books-prerestore` holds dev's pre-restore state. Restart the REPL and re-load the model afterward.

### 3. Restore A to staging — DB up, app either way

```sh
scripts/data-ui snapshot restore A stg:books
```

Bring the profile's Postgres up first if it is down (`e-demo start books` brings the whole unit). Restoring over the running app is fine — the audition flow relies on it.

Cross-environment, so the notice applies: old JWT sessions break (log in again — the user table was swapped, not the secret; use staging's own password from its `profile.env`, re-stamped by the restore). See [Admin passwords](#admin-passwords-precautions). Undo slot: `stg-books-prerestore`.

### 4. Save B from staging

```sh
scripts/data-ui snapshot save B stg:books
```

Identity is `profile.env`, so the app may be up or down. B carries staging's current auth rows — which answer to staging's own re-stamped password.

### 5. Restore B to staging

```sh
scripts/data-ui snapshot restore B stg:books
```

Same address: no notice; the admin password is re-stamped to staging's own again (a no-op when it already matched); the app may keep running; log in again because the user table was swapped.

### Net state

Dev and staging both hold A's data; B is staging as of step 4 (A's data plus anything written since); each `-prerestore` slot holds the state before that restore; every environment answers to its own admin password (each restore re-stamped it).

## Admin passwords (precautions)

**Auth rows travel with the dump; `ADMIN_PASSWORD` does not — so restore re-stamps it.** The dump contains the source's users and password hashes, including the source's admin hash. After a successful restore, the script unconditionally writes the destination's own admin hash back: `sha512("admin" ++ password)` truncated to 32 hex chars, exactly matching rbac's `a:password-hash` (computed host-side with `sha512sum`; only the provably-hex hash ever reaches SQL). The destination's `ADMIN_PASSWORD` — the `profile.env` line for `stg:`, the static `admin-password-1` for `dev:` — is therefore always the working password after a restore.

Only the `admin` user is re-stamped. Every other user row arrives from the source with whatever account state it carried (names, hashes, roles).

Precautions:

1. **Log in again after every restore, with the destination's own password.** Old JWT sessions break because the user table was swapped, not because the secret changed — `JWT_SECRET` lives in the environment and never travels with the dump.
2. **Do not "fix" the destination by editing its env.** Changing `ADMIN_PASSWORD` in `profile.env` does not affect a restored database; the file then lies about the state. (The restore's own re-stamp is what keeps env and DB in agreement.)
3. **Other users travel.** Only `admin` is re-stamped; any other account the source carried keeps its source-side password. Mind that when an exposed profile's public URL serves restored data.
4. **Treat snapshot files as sensitive.** Dumps contain password hashes; the pool is shared host state. Protect it like `profile.env` when copying or backing up. (`.meta` holds no secrets and is parsed, never sourced.)

Same-address restores — the nightly `stg:books` to `stg:books` — re-stamp too (a no-op when the hash already matched) and print no notice.

## Historical: truing up passwords

Before restore re-stamped the admin password (the earlier, deliberately-post-MVP gap), a cross-environment restore left the destination answering to the SOURCE's admin password, and the runbook was to "true up": change the password in the Admin UI, or change the DB by hand, so env and rows agreed again. Restore now does that automatically for `admin` on every restore — the section survives as history and as the manual fallback should a restored database ever need its `admin` hash fixed by hand:

    # hash = first 32 hex chars of sha512("admin" ++ password)
    printf '%s' "admin${ADMIN_PASSWORD}" | sha512sum | cut -c1-32
    # then, against the destination's Postgres:
    update users set password_hash = '<hash>' where user_name = 'admin';

## How snapshots work

### The pool and the triple

A snapshot is up to three files under `/data/data-ui/snapshots/`:

- `<name>.dump` — custom-format `pg_dump` (`--no-owner --no-privileges`, so role names may differ per environment);
- `<name>.files.tar.gz` — the `DOCUMENT_ROOT` tree, whenever one exists at save time (optional throughout; a save with a missing files tree stores the DB alone and drops a stale tar);
- `<name>.meta` — key=value manifest: `model`, `source-address`, `timestamp`, plus best-effort `model-version` read from the model file. Nothing in `.meta` comes from the live app.

Dump and restore run inside the respective environment's PostgreSQL server container (the source's on `save`, the destination's on `restore`), so client tools always match the server version.

### Password re-stamping

After a successful `pg_restore`, restore unconditionally re-stamps the destination's admin password hash: the same value rbac's `initialize-database` would have seeded — `sha512("admin" ++ ADMIN_PASSWORD)` hex, first 32 characters (`a:password-hash`; computed host-side, the provably-hex hash interpolated into a single `UPDATE ... WHERE user_name = 'admin'`). The destination's `ADMIN_PASSWORD` (dev static default or the profile's `profile.env` line, both already loaded by address resolution) is therefore always the working password after a restore. A zero-row update (no `admin` user in the dump — not a legal Data UI state) warns and continues; the restore itself has already succeeded. The stamp runs inside the destination's container, so it needs no host-side Postgres client.

### Identity and checks

Model identity comes from the address, never from the live app: the `dev:` token, or the profile's `MODEL_NAME` for `stg:`. `/api/info` is a positive confirmation only. Restore refuses when the snapshot's `.meta.model` differs from the destination's model — never FORCE-able. The version compare warns only when both sides carry a version and they differ. A missing `.meta` (old snapshots) warns — provenance unknown, though the admin password is still re-stamped from the destination's env — then proceeds.

### Restore mechanics

Before touching anything, restore validates the archive (`pg_restore -l`), stages a safety dump of the current DB (plus its files tree and a fresh `.meta`) to temp names, drops and recreates the `public` schema, and loads the dump with `--single-transaction`. On failure it rolls back from the staged dump; if even the rollback fails, the staged triple is promoted to the prerestore slot with a CRITICAL message pointing there. On success the staged triple rotates onto the `<dest>-prerestore` slot (destination address, colon to dash: `stg-books-prerestore`, `dev-books-prerestore`).

The files side swaps rather than deletes: extract to staging, require exactly one top-level directory, `mv` the old tree aside, swap in the new one, delete the aside copy after success. A bad tar dies with the database restored and files untouched. Tars are packed under a canonical `files/` top-level entry regardless of the source's basename, so they are portable across environments — and old tars on disk restore retroactively.

### The post-restore orphan sweep

After the success line — on `prd:` after the revive, so the app is serving — every restore automatically sweeps orphaned `resources` rows (restore-side defense-in-depth: a contaminated snapshot carries name-keyed orphans with the dump, and one roleless duplicate blocks every by-name update; the engine factory itself is already fixed). The row-resourced table set derives from the database's FK graph, filtered to id-column FKs (`resource_roles` links on `resource_id`, so it never counts), never hardcoded; the sweep reads no model. A report prints per table (res / rows / orphans / roleless-by-id / rows-without-resource) plus the prefixes of dropped types (inert, reported, never deleted, never failed). The gate: sweep only when every row-resourced table reports rows-without-resource = 0 and roleless-by-id = 0 ("with roles" = at least one `resource_roles` row; every row resource is born with `admin` and every edit force-adds it back). SAFE + orphans → one prefix-scoped DELETE (type-level resources untouched by construction, role links cascade), post-verify, exit 0. NOT SAFE → nothing deleted, the restore itself completed (the database is the dump, the app is up), exit 1 and a healing pointer: re-stamp a roleless by-id resource **by id** from a roled by-id sibling of the same type, or mint a missing one (`insert into resources (id, resource_name) values (<row-id>, '<prefix>:<row-id>')`) — never through the name-based rbac API, whose duplicate-name resolution is the failure itself; then re-run `scripts/data-ui snapshot sweep-orphans <dst-address>` (the standalone arm: same sweep, no restore, never quiesces). A source slot that will be restored again — a demo's golden, restored nightly — must be healed and re-taken (`snapshot save stg-<p>-golden stg:<p>`), or every later restore of it stops here again. Unsafe is never FORCE-able, mirroring the model-mismatch rule. No backup artifacts (no `zz_` tables, no TSVs — dropping them into a just-restored database would pollute it): the rollback is the snapshot itself; re-restore re-imports the exact pre-sweep state and the sweep converges, idempotent. The verdict rides the exit status only — it never changes the restore's outcome or the app's availability. The one non-fatal caller is the rebirth loop (below); every hand-run restore, runbook sequences included, is fatal on a refused verdict.

### Reserved names

The address-keyed slots are managed by the machinery:

- `stg-<p>-golden` — the nightly reset source for e-demos (manual re-take, as in the examples above);
- `stg-<p>-last` — refreshed automatically by the stop verbs (the state at the last deliberate pause);
- `stg-<p>-prerestore` / `dev-<model>-prerestore` — rotated by restore (dev has no golden/last: nothing rewrites its history);
- `prd-<name>-last` — the delete verb's farewell snapshot, saved before the instance's volumes are destroyed (the prd twin of the stop verb's p-last; rotates if the same name is redeployed and deleted again).

Names are otherwise plain, and the pool is flat. A hand-run `save` naming a slot overwrites it; a hand-run `drop` of a slot silently kills the nightly's golden (or the demo's last) — no confirmation exists. And `save books-1 stg:books` followed by `save books-1 dev:books` overwrites the first snapshot.

### When the app is unreachable

`/api/info` may be down during a snapshot operation (a unit coming up, or deliberately stopped). The policy splits by verb:

- `save` + unreachable `dev:` refuses — "what am I dumping" cannot be checked — unless `FORCE=1`;
- `save` + unreachable `stg:` skips the confirm quietly (identity is `profile.env`, already checked);
- `save` + unreachable `prd:` skips the confirm quietly (identity is the namespace + Secret, already checked);
- `restore` + unreachable (either qualifier) skips the confirm — the app-down restore is the natural shape, and the nightly reset depends on this branch. Under `prd:` the confirm is skipped outright (see [Production](#production)).

`FORCE=1` buys exactly one thing here — the unreachable-dev save escape — plus the usual confirmation skips (and, outside this policy, letting `copy` overwrite an existing destination). It never overrides a model mismatch.

### Copying a snapshot

`snapshot copy <src-name> <dst-name> [<model>]` duplicates a snapshot under a new name in the pool: `<src>.dump`, `<src>.files.tar.gz` (when present), and `<src>.meta`. Both names go through the snapshot-name check, the source must have a `.dump`, and the source is never modified. A destination that already exists (any of its three files) is refused unless `FORCE=1`; a forced copy replaces the whole destination triple, dropping a files tar or `.meta` the source lacks. The copy stages to temp names and moves onto the destination names only when everything staged, like `save`.

The copy's `.meta` keeps every source key and adds `copied-from=<src>`. With `<model>` (same charset as names), the `model` key is replaced and `copied-model-from=<old model>` records what it was; `source-address`, `timestamp`, and `model-version` stay as they were, so the provenance still points at where the data came from. A source without a `.meta` gets a stub (`model` plus `copied-from`) when `<model>` is given, and stays `.meta`-less otherwise.

The use case is a renamed instance. A `prd:` address asserts the instance name as the model, and restore refuses a model mismatch, so a `todos` snapshot cannot restore straight into a test deploy named `todos-hannah`. The schema is the same (only the name differs), so retarget a copy and restore that:

```sh
scripts/data-ui snapshot copy stg-todos-golden hannah-todos-seed todos-hannah
scripts/data-ui snapshot restore hannah-todos-seed prd:todos-hannah
```

Retargeting is a deliberate assertion that the data fits the named model; restore still checks the archive and re-stamps the destination's admin password as usual. Drop the copy when the test instance is gone.

### One-time migration

`snapshots migrate` renames legacy profile-keyed slots (`<p>-golden` etc.) to `stg-<p>-*` and stubs their `.meta`, idempotently per file. Run it as the state's owner (macnod, never sudo — a root-written stub blocks the next save).

## Production (`prd:`)

`prd:<name>` aims a snapshot at a k3d production deploy. The resolver reads everything from the cluster: the namespace `dataui-<name>` must exist, the NodePort comes from the live Service (the verb talks plain HTTP over the `127.0.0.1:<nodePort+1000>` loopback hop for `/api/info` confirms), `DB_PASSWORD` / `ADMIN_PASSWORD` come from the `dataui-<name>-secrets` Secret (never the `secrets.env` cache), and `DOCUMENT_ROOT` from the live files PV's `hostPath` (an instance with no files PV resolves to an empty `DOCUMENT_ROOT` — a DB-only snapshot, legal like any other). Database work runs via `kubectl exec -i` into the `dataui-<name>-postgres` Deployment (TCP auth with the Secret's password, the init container's mechanism), so no host-side Postgres client is needed and the client version always matches the server.

Constraints:

- **Deploy host only.** The verb dies with a clear message on any other machine (cluster truth and the snapshot pool live on `$DEPLOY_HOST`; there is no ssh re-exec). Run it from a checkout — the verb `cd`s to the repo root itself, like the demo verbs.
- **A `prd:` restore takes downtime — by design.** Every check that can fail (model match, archive TOC, postgres reachable) runs *before* anything is scaled; a corrupt archive or a mismatch dies with prod still up. The restore then scales the app Deployment to 0, waits until the pod is gone (the files PVC is RWO), does the safety dump / schema drop / `--single-transaction` restore / files swap / admin re-stamp, scales back to 1, and requires HTTP 200 on `/health` before reporting success — and only then, with the app serving, runs the post-restore orphan sweep. Never a live-app restore — ratified: downtime beats it while there are no transactions. The sweep is the one deliberate exception to the never-live rule: it runs after the success line, the app is up, and its verdict never changes the restore's outcome or the app's availability (see the orphan-sweep section above; exit 1 = the sweep refused, the restore itself completed).
- **The EXIT trap bounds availability, not data validity.** Kill the process mid-restore and the trap puts back any mid-swap files tree, scales the app back to 1, and prints which state holds: drop-not-started (database untouched), drop-started (database NOT guaranteed valid — the message names the `prd-<name>-prerestore` slot as the manual undo), or restored (only the coda was cut short). `kill -9` is the one exception: nothing runs, the app stays at 0, and the manual revive is `kubectl -n dataui-<name> scale deploy/dataui-<name> --replicas=1`.
- **The admin password is the Secret's.** Restore re-stamps from the cluster Secret's `admin-password` key, so after any restore the working login is the Secret's value (see [Admin passwords](#admin-passwords-precautions)).
- **Slots.** `prd-<name>-prerestore` rotates exactly like the dev/stg slots. There is no `prd:` golden — no nightly reset — and the only `prd:` `-last` is the delete verb's farewell snapshot, saved to the shared pool before the instance's volumes are destroyed (rotating; `FORCE=1 delete` skips it). Scheduled off-site prd saves exist as host-side per-app timers (`~/bin/scenebook-backup` every 30 minutes, `~/bin/backlog-backup` hourly) that upload fresh triples into tiered S3 prefixes — see `docs/deployment.md` → Production Backups; they are host machinery, not part of this verb.

Typical flow — seed a fresh deploy from a saved snapshot:

```sh
scripts/data-ui snapshot save chores-initial stg:chores
scripts/data-ui deploy chores
scripts/data-ui snapshot restore chores-initial prd:chores
```

## Cross-site restores (prod-2)

`prd:` snapshot verbs are **site-local**: the resolver reads cluster truth and the snapshot pool on the deploy host, so a `prd:` verb on prod-2 addresses prod-2's cluster, and evo-x2's `prd:` verbs cannot reach prod-2 (and vice versa). A cross-site restore is therefore two steps on the destination site: the backup script stages the other site's objects into the local pool, then the normal restore machinery takes over — the examples below land it in `prd:` (back into a cluster) and in `dev:` (an audit on the destination site's own environment).

```sh
# on prod-2: stage evo-x2's freshest set, then restore what it printed
RESTORE_SITE=evo-x2 ~/bin/backlog-backup recover
scripts/data-ui snapshot restore prd-backlog-<ts> prd:backlog
```

The mirror — stage prod-2's set on evo-x2 to audit the moved app's data, no cluster involved:

```sh
# on evo-x2: stage prod-2's freshest set, then restore it into development
RESTORE_SITE=prod-2 ~/bin/backlog-backup recover
scripts/data-ui snapshot restore prd-backlog-<ts> dev:backlog
```

`dev:` is the natural destination once the app has moved: `prd:backlog` on evo-x2 would need the namespace back (a `MOVE=1` deploy — the wrong-site guard refuses a bare one — or a `snapshot copy` retargeted at a scratch profile for a drill), while `dev:backlog` needs only the usual dev shape (Postgres up, REPL down; restart the REPL and `(set-model "backlog")` after). This one *is* a cross-environment restore like any other dev one: the notice prints, the admin password re-stamps to dev's static `admin-password-1`, and the undo slot is `dev-backlog-prerestore`. The evo-x2 side needs the backup user's read-only grant on `prod-2/*` (see [deployment.md](deployment.md) → Production Backups); `recover` prints its follow-up command against `prd:<app>` by default — `RESTORE_PRINT_TARGET=dev:backlog` retargets the printed line, the text only, never where anything lands.

Two things change across sites, both stated here so they surprise nobody:

- **The admin password after a cross-site restore is the destination site's Secret** (the restore re-stamps from the destination's cluster Secret), and **no cross-environment notice prints** — both sites' addresses read `prd:<name>`, so the dev→stg-style warning does not apply. Every user signs in again anyway (the JWT secret is the destination's, and the user table arrived with the dump).
- **No `heal-model` after a `prd:` restore.** The restore scales the app to 0 and back, so it always ends in a fresh process — and docs/migration.md's rule is that a fresh process needs neither `heal-model` nor a restart. On a recorded-release rebirth the model is the one that wrote the snapshot anyway.

