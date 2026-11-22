# Migrations

How to change a model whose data you want to keep. "Migration" here means the manual discipline — snapshot, reload, hand SQL — not an automated engine (see the backlog item "Migrations when the model changes" for that; it is not built).

Every recipe on this page rests on four engine facts, verified in `lisp/model.lisp` and `lisp/database.lisp`:

- `set-model` compiles the file on disk, and when a model is already loaded it first runs `reset-tables`, dropping every user table of the loaded model. The drop is guarded by `(when *compiled-model* …)` — a fresh process (no model compiled yet) skips it. The reset also deletes the dropped tables' row resources first — one pre-drop statement over the row-resourced set derived from the compiled model (the same `:id` → `:resources` fields that generated the FKs), so a warm `set-model` no longer orphans them. Resources orphaned before this fix are not cleaned here: the sweep items own those.
- `heal-model` re-runs `set-model`'s idempotent tail against the already-compiled model: create missing tables, ensure roles, add resources. It never drops, but it also cannot load a different model file — it has no argument. Same model only.
- `create-tables` probes `information_schema` per table and creates only what is missing. It never alters an existing table: changed columns, constraints, and identity indexes in an existing table are invisible to it, and `create index if not exists` skips an index whose name already exists even when its column list changed.
- A snapshot restore replaces the entire `public` schema (`DROP SCHEMA public CASCADE; CREATE SCHEMA public`), then `pg_restore`s the dump and re-stamps the destination's admin password. Snapshots carry data and files, never code. Each restore saves the destination's pre-restore state to the `<name>-prerestore` slot and can be undone by restoring that.

Consequences:

- A **changed column type**, a **renamed or dropped field**, or a **changed identity index** is never applied to an existing table — that is hand SQL (the worked example below shows the shape).
- A **new type** or **new field on a new join table** needs no SQL at all: the reload path creates what is missing.
- Dropping a field from the model does not drop the column. The column stays (NOT NULL constraints included) until dropped by hand, and inserts that omit it only work while the column is nullable or has a default.
- Until the backlog item "Load a changed model without dropping tables" lands, loading a changed model over live data requires a fresh process — that is the nil guard doing the work. After it lands, `(heal-model "backlog")` (or equivalent) replaces the restart step.

## Which reload call, when

Situation → call:

- **Fresh process, changed model file** (the migration path): `(set-model "backlog")`. No model is compiled, so the drop is skipped; `create-tables` creates the missing tables; hand SQL finishes what it cannot.
- **Live image, same model, drift to heal** (after a restore onto a running image, or engine code gained a new system table): `(heal-model)`. Catches the schema up to the compiled model without dropping anything.
- **Live image, changed model file**: restart the process, then `set-model` (the fresh-process case). There is no one-call path today — that is the gap the backlog item names. A second `set-model` in the live image drops the loaded model's tables, restored rows included.
- **Just re-reading the same file after an edit that keeps all tables and columns**: a restart is still the safe answer. `heal-model` alone does not re-read the file; a bare `set-model` on a live image drops the tables.

Never `reset-database` or `reset-to-model` over data you want to keep — both drop everything.

## The dev recipe

The dev database is throwaway: `repl` and `db` remove the container and its volume on exit, so nothing durable ever lives only in the dev database. A migration therefore starts and ends with snapshots.

1. **Snapshot first, before editing the model.** With the dev app running (`scripts/data-ui repl`, model loaded):

   ```sh
   scripts/data-ui snapshot save dev-backlog dev:backlog
   ```

2. **Edit the model file** (`models/<name>.lisp`, unstaged is fine — the reload reads the file on disk). Bump `:version`: the snapshot's `.meta` records the model version at save time, and restore warns when snapshot and on-disk model disagree — an unbumped version makes that warning impossible.

3. **Start a fresh REPL** (the existing one has a model compiled; a `set-model` there would drop your tables). Terminal 1:

   ```sh
   scripts/data-ui repl
   ```

   Do not call `set-model` yet. Leave `MODEL_NAME` unset so nothing auto-loads.

4. **Restore the snapshot.** Terminal 2, while the REPL's Postgres (5444) is up:

   ```sh
   scripts/data-ui snapshot restore dev-backlog dev:backlog
   ```

   A `dev:` restore over a REPL with no model loaded prints `note: /api/info unreachable; proceeding (identity is .meta vs the address, both static)` — expected, not a failure (a model-less `/api/info` confirms nothing; the restore falls back to the static `.meta`-vs-address identity check). The pre-restore (empty) state rotates into `dev-backlog-prerestore`.

5. **Load the edited model** — in the REPL now:

   ```lisp
   (set-model "backlog")
   ```

   `*compiled-model*` is nil, so `reset-tables` is skipped and your restored rows survive. `create-tables` creates the tables the new model adds (a new joiner, say) and leaves existing tables untouched.

6. **Hand SQL for what the compiler cannot change in place** — via `scripts/data-ui psql` (the REPL's own container; no separate `db` session needed). Column types, identity indexes, dropped columns, NOT NULL relaxations. The worked example below is exactly this step.

7. **Verify, then save the result under a new name** so the pre-migration snapshot stays untouched (in the backlog run below: `dev-backlog-1` beside `dev-backlog`):

   ```sh
   scripts/data-ui snapshot save dev-backlog-m2m dev:backlog
   ```

   The moment the REPL exits, the volume is gone; if you skip this step the migrated state dies with the session.

## Restoring onto a live image

`snapshots.md` says "Postgres up, REPL down — never drop `public` under a live REPL connection pool". In practice (verified by repeated use on the dev image), restoring into a running Slime-connected REPL works: the restore drops and rebuilds `public` under the app's connection pool, and the image carries on. The documented caution dates from before that was exercised. Two real effects to plan around:

- **Everything the dump predates vanishes** — tables and RBAC rows introduced by newer engine code (the `usage` table after the action-log work, for example). That is what `(heal-model)` is for; run it after any restore onto a live image. Never `set-model` there — its `reset-tables` drops the loaded model's user tables, the just-restored rows included. (A fresh process needs neither: the nil guard skips the drop at startup.)
- **The live image is now serving the old compiled model against the restored schema.** Restoring does not recompile anything. Same-model drift → `heal-model`; changed model file → restart into the dev recipe above.

Also note the shape `snapshots.md` recommends for dev restores — "quit the REPL, keep Postgres up via `scripts/data-ui db`, restore" — cannot hand the data to a REPL afterwards: `repl` refuses to start while `db`'s Postgres holds 5444, and `db` removes the volume on exit. Either restore onto the live REPL (as above) or use the fresh-REPL order of the dev recipe (start `repl` first, then restore into it).

The pre-restore slot rotates on every restore, so a bad one is undoable (`snapshot restore dev-backlog-prerestore dev:backlog`) — but note the undo itself is another restore, and it overwrites the current `dev-backlog-prerestore` slot with what it saves (the bad state). Keep genuinely wanted states in explicitly named snapshots.

Staging (`stg:`) and production (`prd:`) restores are covered in `snapshots.md` — including the production rule that restores always scale the app to 0 (downtime by design) and back.

## Worked example: backlog, plans 1:N → M2M

The first real migration: `models/backlog.lisp` changed the `:plans` field on `:items` from a 1:N foreign key (`rt_plans.plan_item`) to an M2M list field through a new joiner table, `:item-plans`. `:identity t` had to come off `:plans :item` (the identity had been the composite `(title, item)`), so a freshly compiled `rt_plans` now has a single-column identity index on `plan_title` alone.

What the reload path did automatically, per the engine facts above:

- `rt_item_plans` (columns `item_id`, `plan_id`, both `references … on delete cascade`) — created by `create-tables`, because the table did not exist in the restored dump.
- Roles, type resources, settings — healed by the same tail.

What it could not do, and the hand SQL for each:

- **The identity index.** The restored `rt_plans` still carried the old two-column index `ix_rt_plans_identity (plan_title, plan_item)`; the compile emits `create unique index if not exists ix_rt_plans_identity on rt_plans (plan_title)`, which no-ops because the name exists. The 46 plan titles are distinct, so the replacement is safe on this data (check yours first — a duplicate title is a data problem the old index was hiding):

  ```sql
  drop index ix_rt_plans_identity;
  create unique index ix_rt_plans_identity on rt_plans (plan_title);
  ```

- **The join-table backfill.** The item→plan links still lived only in `rt_plans.plan_item`. Copy them into the joiner (idempotent by the `not exists` guard):

  ```sql
  insert into rt_item_plans (item_id, plan_id)
  select p.plan_item, p.id
  from rt_plans p
  where not exists (
    select 1 from rt_item_plans ip
    where ip.item_id = p.plan_item and ip.plan_id = p.id);
  ```

- **The old FK column.** `rt_plans.plan_item` stays — column and its NOT NULL — until dropped by hand. Dropping it is deliberately sequenced after the backfill (it is the backfill's source), and it is also what makes the M2M display real: while `:plans :item` remains in the model, the `:items` view reaches `rt_plans` through that FK, not through the joiner, so the Plans field shows the FK links and edits write join rows the display never reads. Removing `:item` from `:plans` in the model (a second reload, second migration) flips the view onto `rt_item_plans`. Then:

  ```sql
  alter table rt_plans drop column plan_item;
  ```

  (Not-null first means new plan inserts through the old model would still require it — another reason the model-side removal and the column drop belong to the same step.)

Check column names before running backfill SQL against your own tables: `\d rt_item_plans`. Joiner column names follow `<type>_id` from the two `:reference` fields.

Finally, snapshot the result under a new name (in the actual run: `snapshot save dev-backlog-1 dev:backlog`, leaving the pre-migration `dev-backlog` untouched — both slots exist in the pool as of 2026-11-16).

## Out of scope

- No ALTER synthesis, column-type changes, or constraint migration — hand SQL, forever (for now).
- No transactions around any of this: a failed hook or SQL statement does not roll back prior side effects (`docs/hook-registry.md` → MVP caveat). Sequence destructive steps so a failure leaves the database re-runnable (the backfill above is idempotent for exactly this reason).
- YAML/JSON model input, model versioning machinery, and automatic model-change detection — all post-MVP backlog items.
