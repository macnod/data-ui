# Snapshots

A snapshot captures a Data UI environment — its database and uploaded
files — as a portable artifact in one shared pool
(`/data/data-ui/snapshots/`, the `DATA_UI_STATE` root). Snapshots move
state between environments: audition dev data in staging, promote
auditioned data to golden, or bring staging data back into dev.

All commands run from the repo root via `scripts/data-ui snapshot …`.
For the command reference see `scripts/README.md`; for the surrounding
deployment machinery see `docs/deployment.md`.

## The grammar

```sh
scripts/data-ui snapshot save    <name> <src-address>
scripts/data-ui snapshot restore <name> <dst-address>
scripts/data-ui snapshot list
scripts/data-ui snapshot drop    <name>
scripts/data-ui snapshot migrate        # one-time maintenance
```

Every address is `<qualifier>:<name>`, both tokens mandatory — no
defaults, no unqualified fallback:

- `dev:<model>` — the built-in dev environment (the 5444 REPL),
  asserting the loaded model. `dev:books` requires
  `models/books.lisp` to exist and confirms the live model.
- `stg:<profile>` — a host profile. `stg:books` resolves through
  `/data/data-ui/profiles/books/profile.env`; the model is the
  profile's `MODEL_NAME` (the tail is a profile name, not a model
  name).

The second token is the SOURCE on `save` and the DESTINATION on
`restore` — same position, same vocabulary, role fixed by the verb.
The one-arg forms (`snapshot save books-1`, `snapshot restore books`)
are hard errors that name both legal shapes. Snapshot names share the
profile charset `^[A-Za-z0-9][A-Za-z0-9_-]*$` (colon illegal), so a
name can never masquerade as an address.

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

The profile's Postgres must be up (an `e-demo start books` brings the
whole unit; the restore itself only needs the database). Stopping the
app unit first — what the nightly reset does — keeps the swap
quiescent, but restoring over the running app works too (the audition
flow relies on it).

This is a cross-environment restore: read the password notice below.
The admin password is re-stamped to the destination's own, so no
manual truing up is needed.

### Make books (an e-demo) reset to the new snapshot

e-demos are nightly-reset: at 04:10 the timer restores
`stg-books-golden` into `stg:books`. Making the reset use the new
data means re-taking golden from the running staging instance after
auditioning it:

```sh
scripts/data-ui snapshot save stg-books-golden stg:books
```

Every nightly reset from then on restores this state.

### Create a snapshot of books in the staging environment

```sh
scripts/data-ui snapshot save books-stg-1 stg:books
```

Pick any name that is not a reserved slot name (see
[Reserved names](#reserved-names)).

### Restore that snapshot to books in the development environment

Postgres up, REPL down is the natural restore shape (never drop
`public` under a live REPL connection pool):

```sh
scripts/data-ui db     # terminal 1: dev Postgres; psql stays a client
scripts/data-ui snapshot restore books-stg-1 dev:books
```

Dev's pre-restore state is saved to `dev-books-prerestore`, and the
admin password is re-stamped to dev's own static
`admin-password-1` (restore always re-stamps; notice printed).

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

Undo a bad restore — the prerestore slot holds the destination's
pre-restore state (restoring it rotates the slot again, as always):

```sh
scripts/data-ui snapshot restore stg-books-prerestore stg:books
```

What refusal looks like (each error names the legal shapes):

```sh
scripts/data-ui snapshot restore books     # missing address
scripts/data-ui snapshot save x prod:books # unknown qualifier
scripts/data-ui snapshot restore books-1 dev:todos   # model mismatch
```

A model mismatch is never overridable — `FORCE=1` does not apply.

## Runbook: a dev → staging round trip

A five-step loop exercising every verb and both addresses — the
audition shape in miniature (`A` and `B` are placeholder snapshot
names; the `dev:` tail is a model name, the `stg:` tail a profile
name):

1. Save snapshot A from dev.
2. Restore A back to dev.
3. Restore A to staging.
4. Save snapshot B from staging.
5. Restore B to staging.

### 0. The password story (restore re-stamps)

Auth rows travel with the dump, but every restore unconditionally
re-stamps the destination's own admin password hash afterward
(`ADMIN_PASSWORD` from the destination env: the static
`admin / admin-password-1` for `dev:`, the profile's `profile.env`
line for `stg:`). Steps 2–5 therefore always leave each environment
answering to its own password; nothing needs truing up afterward.
Old JWT sessions still break (the user table was swapped); log in
again with the destination's own password.

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

A `dev:` save refuses when `/api/info` is unreachable — the confirm
must see the live model.

### 2. Restore A to dev — Postgres up, REPL down

Quit the SBCL image from step 1 (never drop `public` under a live
REPL connection pool) and keep Postgres up:

```sh
scripts/data-ui db
scripts/data-ui snapshot restore A dev:books
```

Same address, so no cross-env notice; the admin password is
re-stamped (to dev's own — a no-op when it already matched). The
undo slot `dev-books-prerestore` holds dev's pre-restore state.
Restart the REPL and re-load the model afterward.

### 3. Restore A to staging — DB up, app either way

```sh
scripts/data-ui snapshot restore A stg:books
```

Bring the profile's Postgres up first if it is down (`e-demo start
books` brings the whole unit). Restoring over the running app is
fine — the audition flow relies on it.

Cross-environment, so the notice applies: old JWT sessions break
(log in again — the user table was swapped, not the secret; use
staging's own password from its `profile.env`, re-stamped by the
restore). See
[Admin passwords](#admin-passwords-precautions). Undo slot:
`stg-books-prerestore`.

### 4. Save B from staging

```sh
scripts/data-ui snapshot save B stg:books
```

Identity is `profile.env`, so the app may be up or down. B carries
staging's current auth rows — which answer to staging's own
re-stamped password.

### 5. Restore B to staging

```sh
scripts/data-ui snapshot restore B stg:books
```

Same address: no notice; the admin password is re-stamped to
staging's own again (a no-op when it already matched); the app may
keep running; log in again because the user table was swapped.

### Net state

Dev and staging both hold A's data; B is staging as of step 4 (A's
data plus anything written since); each `-prerestore` slot holds the
state before that restore; every environment answers to its own
admin password (each restore re-stamped it).

## Admin passwords (precautions)

**Auth rows travel with the dump; `ADMIN_PASSWORD` does not — so
restore re-stamps it.** The dump contains the source's users and
password hashes, including the source's admin hash. After a
successful restore, the script unconditionally writes the
destination's own admin hash back: `sha512("admin" ++ password)`
truncated to 32 hex chars, exactly matching rbac's `a:password-hash`
(computed host-side with `sha512sum`; only the provably-hex hash
ever reaches SQL). The destination's `ADMIN_PASSWORD` — the
`profile.env` line for `stg:`, the static `admin-password-1` for
`dev:` — is therefore always the working password after a restore.

Only the `admin` user is re-stamped. Every other user row arrives
from the source with whatever account state it carried (names,
hashes, roles).

Precautions:

1. **Log in again after every restore, with the destination's own
   password.** Old JWT sessions break because the user table was
   swapped, not because the secret changed — `JWT_SECRET` lives in
   the environment and never travels with the dump.
2. **Do not "fix" the destination by editing its env.** Changing
   `ADMIN_PASSWORD` in `profile.env` does not affect a restored
   database; the file then lies about the state. (The restore's own
   re-stamp is what keeps env and DB in agreement.)
3. **Other users travel.** Only `admin` is re-stamped; any other
   account the source carried keeps its source-side password. Mind
   that when an exposed profile's public URL serves restored data.
4. **Treat snapshot files as sensitive.** Dumps contain password
   hashes; the pool is shared host state. Protect it like
   `profile.env` when copying or backing up. (`.meta` holds no
   secrets and is parsed, never sourced.)

Same-address restores — the nightly `stg:books` to `stg:books` —
re-stamp too (a no-op when the hash already matched) and print no
notice.

## Historical: truing up passwords

Before restore re-stamped the admin password (the earlier,
deliberately-post-MVP gap), a cross-environment restore left the
destination answering to the SOURCE's admin password, and the
runbook was to "true up": change the password in the Admin UI, or
change the DB by hand, so env and rows agreed again. Restore now
does that automatically for `admin` on every restore — the section
survives as history and as the manual fallback should a restored
database ever need its `admin` hash fixed by hand:

    # hash = first 32 hex chars of sha512("admin" ++ password)
    printf '%s' "admin${ADMIN_PASSWORD}" | sha512sum | cut -c1-32
    # then, against the destination's Postgres:
    update users set password_hash = '<hash>' where user_name = 'admin';

## How snapshots work

### The pool and the triple

A snapshot is up to three files under `/data/data-ui/snapshots/`:

- `<name>.dump` — custom-format `pg_dump` (`--no-owner
  --no-privileges`, so role names may differ per environment);
- `<name>.files.tar.gz` — the `DOCUMENT_ROOT` tree, whenever one
  exists at save time (optional throughout; a save with a missing
  files tree stores the DB alone and drops a stale tar);
- `<name>.meta` — key=value manifest: `model`, `source-address`,
  `timestamp`, plus best-effort `model-version` read from the model
  file. Nothing in `.meta` comes from the live app.

Dump and restore run inside the respective environment's PostgreSQL
server container (the source's on `save`, the destination's on
`restore`), so client tools always match the server version.

### Password re-stamping

After a successful `pg_restore`, restore unconditionally re-stamps
the destination's admin password hash: the same value rbac's
`initialize-database` would have seeded — `sha512("admin" ++
ADMIN_PASSWORD)` hex, first 32 characters (`a:password-hash`;
computed host-side, the provably-hex hash interpolated into a
single `UPDATE ... WHERE user_name = 'admin'`). The destination's
`ADMIN_PASSWORD` (dev static default or the profile's `profile.env`
line, both already loaded by address resolution) is therefore always
the working password after a restore. A zero-row update (no `admin`
user in the dump — not a legal Data UI state) warns and continues;
the restore itself has already succeeded. The stamp runs inside the
destination's container, so it needs no host-side Postgres client.

### Identity and checks

Model identity comes from the address, never from the live app: the
`dev:` token, or the profile's `MODEL_NAME` for `stg:`. `/api/info`
is a positive confirmation only. Restore refuses when the snapshot's
`.meta.model` differs from the destination's model — never
FORCE-able. The version compare warns only when both sides carry a
version and they differ. A missing `.meta` (old snapshots) warns —
provenance unknown, though the admin password is still re-stamped
from the destination's env — then proceeds.

### Restore mechanics

Before touching anything, restore validates the archive (`pg_restore
-l`), stages a safety dump of the current DB (plus its files tree and
a fresh `.meta`) to temp names, drops and recreates the `public`
schema, and loads the dump with `--single-transaction`. On failure it
rolls back from the staged dump; if even the rollback fails, the
staged triple is promoted to the prerestore slot with a CRITICAL
message pointing there. On success the staged triple rotates onto the
`<dest>-prerestore` slot (destination address, colon to dash:
`stg-books-prerestore`, `dev-books-prerestore`).

The files side swaps rather than deletes: extract to staging, require
exactly one top-level directory, `mv` the old tree aside, swap in the
new one, delete the aside copy after success. A bad tar dies with the
database restored and files untouched. Tars are packed under a
canonical `files/` top-level entry regardless of the source's
basename, so they are portable across environments — and old tars on
disk restore retroactively.

### Reserved names

The address-keyed slots are managed by the machinery:

- `stg-<p>-golden` — the nightly reset source for e-demos (manual
  re-take, as in the examples above);
- `stg-<p>-last` — refreshed automatically by the stop verbs (the
  state at the last deliberate pause);
- `stg-<p>-prerestore` / `dev-<model>-prerestore` — rotated by
  restore (dev has no golden/last: nothing rewrites its history).

Names are otherwise plain, and the pool is flat. A hand-run `save`
naming a slot overwrites it; a hand-run `drop` of a slot silently
kills the nightly's golden (or the demo's last) — no confirmation
exists. And `save books-1 stg:books` followed by
`save books-1 dev:books` overwrites the first snapshot.

### When the app is unreachable

`/api/info` may be down during a snapshot operation (a unit coming
up, or deliberately stopped). The policy splits by verb:

- `save` + unreachable `dev:` refuses — "what am I dumping" cannot be
  checked — unless `FORCE=1`;
- `save` + unreachable `stg:` skips the confirm quietly (identity is
  `profile.env`, already checked);
- `restore` + unreachable (either qualifier) skips the confirm — the
  app-down restore is the natural shape, and the nightly reset
  depends on this branch.

`FORCE=1` buys exactly one thing — the unreachable-dev save escape —
plus the usual confirmation skips. It never overrides a model
mismatch.

### One-time migration

`snapshots migrate` renames legacy profile-keyed slots
(`<p>-golden` etc.) to `stg-<p>-*` and stubs their `.meta`,
idempotently per file. Run it as the state's owner (macnod, never
sudo — a root-written stub blocks the next save).
