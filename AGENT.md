# Data UI – Agent Overview

Data UI compiles a small nested-plist model into a complete RBAC-backed
application (PostgreSQL + generic backend + REST API + React frontend).
The model is the DNA of the app; the compiler guarantees the expansion
is consistent. Tagline: "Your whole app, in an email."

Consequence for agents: the model format is an **API for a non-human
consumer**. An AI does not write arbitrary code into a Data UI app — it
selects from a defined vocabulary (the hook registry) and fills
parameters.

## Where to Look (read before acting)

Detail lives in the docs, not here. Consult the right one before
touching the corresponding area:

- **Framing, thesis, two tiers, goals, marketplace, current status,
  road to MVP** → `README.md`
- **Model vocabulary** — every top-level key, type key, field key,
  `:ui` subkey, forms, views, write-through, rollups, trees, join
  tables, known gaps → `docs/model-reference.md` (read before editing
  any model or fixture). M2M list fields: join-table row-display
  `:source :agg` is `:distinct` (`:list` there is a compile error;
  omitted `:agg` defaults to `:distinct`).
- **Hooks and the registry** — all three contracts, registry API,
  action hooks, the no-transactions caveat → `docs/hook-registry.md`
  (read before authoring any hook or touching hook code)
- **REST endpoints** — all routes with parameters → `docs/rest.md`
- **Deployment** — every step, traps, admin password location, TLS,
  troubleshooting, clean-slate recovery → `docs/deployment.md`;
  session-by-session history in `~/.debug/deployment-work.md`
- **Lisp style and error reporting** — `u:` preference, small
  functions, `report-e` / `report-ve`, `valid-*` naming →
  `docs/lisp-style.md` (read before writing any Lisp: tests, helpers,
  or engine code with permission)
- **Dead-code decisions** — `docs/model-accessors.md` (check before
  removing anything that looks unused)
- **Backlog** — `docs/todo.org`

## Key Architecture

- `lisp/model.lisp` – model compilation and the RBAC base model
  (`*base-model*`)
- `lisp/backend.lisp` – backend functions (`be-list`, `be-insert`,
  `be-update`, `be-delete`, `be-action`, `be-landing-page`, ...)
- `lisp/rest.lisp` – REST API layer (Hunchentoot handlers)
- `lisp/predicates.lisp` – type/field predicates; `:button` field type
- `lisp/database.lisp` – database initialization and table creation
- `lisp/aux.lisp` – helpers (`report-e`, `report-ve`, path utilities)
- `lisp/plist-json.lisp`, `lisp/deployment.lisp`, `lisp/startup.lisp`,
  `lisp/data-ui.lisp`, `lisp/data-ui-package.lisp`
- `models/` – example models, one per file (e.g. `todos.lisp`,
  `modelbank.lisp`, `widgets.lisp`), each a bare model plist. Load with
  `(set-model "todos")` — bare file name, no path, no `.lisp`
  extension. Test fixtures under `models/test/`; `set-model` checks
  `models/` first, then falls back to `models/test/`. `list-models`
  returns top-level models only.
- `web/` – React frontend (Vite + TypeScript), intentionally minimal
  and schema-driven: consumes `list-form` / `add-form` /
  `update-form`, `records`, and `allowed-values` from the API
- `tests/` – FiveAM suites (`predicate-`, `backend-`, `rest-`,
  `scoping-`, `action-tests.lisp`) plus `helpers.lisp` and
  `model-template.lisp`

Non-frontend code is SBCL Common Lisp, written by a human.

## Interaction Conventions

- AI agents must not modify any Lisp source files outside `web/` and
  `tests/helpers.lisp` without special permission from a human. That
  code is complex and largely outside of AI's current capabilities.
- The React frontend in `web/` was built with AI assistance; future
  frontend work will also involve AI.
- Do not refactor, clean up, or "improve" Lisp code unless explicitly
  instructed.
- Before any code is written, a thorough discussion of the goals must
  happen.
- Some functions with no in-codebase callers are intentionally
  retained; see `docs/model-accessors.md` before removing "dead" code.

## Live Introspection: `eval-in-data-ui`

The Elisp helper `eval-in-data-ui` (in `~/r/elisp/dc-ai.el`) evaluates
a Common Lisp form against a **running** Data UI instance (over Slime,
in the `:data-ui` package) and returns the result. Invoke it via the
`Eval` tool:

    (eval-in-data-ui "(a:list-role-names *rbac*)")

Package-local nicknames like `a:` (the rbac library) are available.
Read-most: prefer it for verification and introspection over guessing
from source; treat state-mutating forms with REPL-level care.

**Reload before testing.** After editing a Lisp file, reload it into
the live image before running tests or introspecting, or you test
stale code:

    (eval-in-data-ui "(load \"~/common-lisp/data-ui/lisp/model.lisp\")")

**Full reload:** `(hard-reset)` (from `lisp/startup.lisp`) runs
`asdf:load-system :force t` + `init-database` + `reset-database` in
one call. Use it (e.g. when `asdf:load-system` resets `*rbac*` to nil)
instead of the three steps manually.

**Gotcha — reloading `rest.lisp` with the server running** can nil out
`*http-server*` while the OS still holds the port; the next
`set-model` → `start-web-server` fails with `ADDRESS-IN-USE-ERROR`.
After reloading `rest.lisp`, verify `*http-server*`; if nil,
`(stop-web-server)` before the next `set-model`.

## Running Tests

Use the helpers in `tests/helpers.lisp` — never call `fiveam:run!`
directly. They handle model loading, database reset, and suite
selection.

- `(run-tests)` — backend, predicates, scoping, hook registry,
  lifecycle, and action suites, via `with-model` on the `test-model`
  fixture (resets the database and loads the model automatically)
- `(run-action-tests)` — action hook suite (buttons, `be-action`,
  status transitions, in-progress guard, permissions, form exclusions)
- `(run-scoping-tests)` — scoping suite on the `modelbank-test`
  fixture under `models/test/` (not the top-level `modelbank` demo)

For a focused run, add a `run-*` helper to `tests/helpers.lisp` —
agents are explicitly permitted to modify that file for this. Do not
modify the test suites themselves without human permission.

### Writing a New Test Suite

1. Create `tests/<name>-tests.lisp` following the existing pattern:
   `in-package`, `def-suite`, `in-suite`, then `test` forms using
   FiveAM primitives (`is`, `is-true`, `is-false`, `signals`,
   `finishes`).
2. Register the file in `data-ui.asd` in the `tests` module, after the
   existing test files.
3. Add a `run-*` helper in `tests/helpers.lisp`, wrapped in
   `(with-model "test-model" nil ...)`.
4. Add the helper to `run-tests` in the same file.
5. `(load "...")` the file (and `helpers.lisp`, if changed) before
   running.

Rules:
- Tests run inside `with-model`, which resets the DB and recompiles.
  Base types (from `*base-model*`) are present in every model.
- **Test models come only from `models/test/`** (e.g. `test-model`,
  `m2m-test`, `static-select-test`). Never point `with-model` /
  `set-model` at top-level demo models (`todos`, `books`, `chores`,
  `modelbank`, ...) — they change with product work and break tests.
  If a needed shape only exists in a demo, copy a minimal stable
  fixture into `models/test/`. Manual REPL smoke against a demo is
  fine.
- Clean up any inserted rows so tests are order-independent.
- `be-types` returns plists, not alists — search with
  `(getf entry :name)` as the `:key`, not `#'car`.

## Feature and Issue Tracking

`docs/todo.org` is the canonical backlog (org TODO / PLANNING /
IN-PROGRESS / READY / DONE states + tags). Refer to it when deciding
what to work on next.

Lifecycle: TODO → PLANNING (a plan is being drafted) → READY (plan
complete and approved; waiting to implement) → IN-PROGRESS (being
implemented) → DONE. Plan completion and review happen at READY,
not IN-PROGRESS; flip to IN-PROGRESS only when implementation
actually begins.

- Every item carries a `:PROPERTIES:` drawer with `:CREATED:` (and,
  once done, `:COMPLETED:`) org inactive timestamps, e.g.
  `[2026-08-10 Sun]`.
- New items: include the drawer with `:CREATED:` set to today. Marking
  DONE: set `:COMPLETED:` to today.
- **After any change to `docs/todo.org`, run `(fix-data-ui-todo-dates)`**
  (defined in `~/r/elisp/dc-ai.el`, no parameters). It enforces the
  date rules; just write today's date and run it.

Org outline hierarchy: `*` section, `**` parent todo, `***` its direct
children (children inherit tags/context). Never mix — do not put `**`
children under a `**` parent; always step to `***`:

    * Post MVP
    ** TODO Lifecycle side effects
    *** TODO Cross-table upsert
    *** TODO HTTP fetch/post

## Working with the Frontend

- Build: `npm install && npm run build` in `web/`. Hunchentoot serves
  `web/dist/`; there is no dev server. **Without `npm run build`,
  frontend changes are invisible** (it runs `tsc`, then `vite build`).
- The app is intentionally simple — avoid heavy routing, state
  libraries, or styling until MVP is proven. All forms render from the
  schema returned by `/api/list`; the frontend does not hard-code
  field lists.
- `:ui` is the extension point, passed through verbatim — new
  `:widget` values work without backend changes. `:widget`,
  `:read-only`, and `:table` are consumed by the React components.
- Permission flags (`create` / `delete` / `update`) from `/api/list`
  control visibility of Add, Delete, and Edit controls.
- `:widget :button` renders a button on the edit form; `onClick`
  posts to `/api/actions`; disabled while status is `running`.
- `:widget :image` / `:image-list` render thumbnail grids with a modal
  lightbox; the `:table` key tells the frontend which type to use for
  `/api/file` URLs.

## Status Digest (June 2026)

- **End-to-end proven:** `scripts/data-ui deploy todos` →
  https://todo.demo.data-ui.com (k3d, HAProxy, TLS).
- Full CRUD on all types (built-in RBAC types included); JWT auth;
  view-level and field-level scoping; write-through core path; action
  hooks; rollups (Phase A); file upload / list / delete (two-phase
  upload: `multipart` to `/api/upload`, then JSON `/api/insert` with
  the `file-token`).
- Frontend: type selector with categories, dynamic lists / forms,
  inline edit, sortable columns, debounced search, pagination driven
  by `total`, rollup boards, role management, image lightbox.
- Known gaps: `:agg` fields on regular types are not sortable (use a
  rollup); file update unimplemented; one flaky scoping test; UI
  polish pending; frontend shows as logged-in after failed token
  refresh, and "No records" covers both empty and failed requests
  (good first UI tasks).
- Full catalog: README → Current Status; `docs/model-reference.md` →
  Known gaps and gotchas.

## Current Focus (MVP)

Deadline: complete MVP, including the demo video, by end of December
2026.

**Model Bank is the priority function** — the MVP's fitness function.
Gaps surfaced by building it are, by definition, the highest-priority
work. Polish waits; capability gaps that block real apps do not.

1. Build Model Bank (relationships, ownership, images, ratings)
2. Write-through edge cases (e.g. clear-to-NULL)
3. UI polish (the video shows the UI)
4. The 30-second video (nothing → deployed app)
5. File update (only if time permits)

**Explicitly post-MVP (do not quietly pull in):** transactions /
rollback around primary write + hooks + write-through; idempotent
database initialization; single-statement `ON CONFLICT` upserts;
YAML/JSON model input and the hosted AI front door.

**Critical invariant — no transactions today.** Hooks, write-through,
and actions are NOT atomic with the primary write. A failing hook
fails the operation without rolling back prior side effects.
Design accordingly; see `docs/hook-registry.md` → MVP caveat.
