# Data UI – Agent Overview

Data UI compiles a small nested-plist model into a complete RBAC-backed application (PostgreSQL + generic backend + REST API + React frontend).

## The Thesis (read this first)

The hard part of building evolving, RBAC-heavy collaborative apps is not writing
code — it is holding a web of invariants (every role × every resource × every
operation, over time) globally consistent. Humans and AIs both drift on this at
scale.

Data UI lets the **entire** application be expressed as a small artifact that
"fits in an email," and the compiler **guarantees** the expansion is consistent.
The model is the DNA of the app. This is why the project exists and why the model
stays small and the compiler stays authoritative.

Strategic consequence relevant to agents: the model format is an **API for a
non-human consumer**. An AI does not write arbitrary code into a Data UI app — it
selects from a defined vocabulary (the hook registry) and fills parameters. Keep
this in mind whenever reasoning about how features should be expressed.

Tagline: **"Your whole app, in an email."**

See README.md for the full framing (Big Idea, Why AI Needs Data UI, Hooks and
the Registry, the Marketplace).

## Core Concept

Describe your data model once. The system generates:
- PostgreSQL tables, views, and triggers
- Parameterized CRUD SQL
- Full RBAC integration (via companion `rbac` library)
- Generic backend functions (`be-*`)
- Generic REST endpoints (`/api/list`, `/api/insert`, `/api/update`, `/api/delete`, etc.)
- Schema-driven React frontend
- Kubernetes manifests for deployment

The goal is deterministic, repeatable development: change the model, recompile, and the application updates.

## Key Architecture

- `lisp/model.lisp` – Model compilation and the RBAC base model (`*base-model*`)
- `lisp/backend.lisp` – Backend functions (`be-list`, `be-insert`, `be-update`, `be-delete`, etc.)
- `lisp/rest.lisp` – REST API layer (Hunchentoot handlers)
- `lisp/predicates.lisp` – Type/field predicates (`table-p`, `base-type-p`, etc.)
- `lisp/database.lisp` – Database initialization and table creation
- `lisp/aux.lisp` – Auxiliary helpers (path utilities, scoped paths)
- `lisp/plist-json.lisp` – Plist ↔ JSON serialization
- `lisp/deployment.lisp` – Model field extraction for deploy scripts
- `lisp/startup.lisp`, `lisp/data-ui.lisp`, `lisp/data-ui-package.lisp` – System startup and package definition
- `models/` – Example models, one per file (e.g. `todos.lisp`, `parts.lisp`, `file-server.lisp`, `modelbank.lisp`, `widgets.lisp`). Each file holds a bare model plist (no `defparameter`, no wrapping variable). Load one with `(set-model "todos")` — pass just the file name, with no path and no `.lisp` extension.
- `web/` – React frontend (Vite + TypeScript)
- `tests/` – FiveAM test suites: `predicate-tests.lisp`, `backend-tests.lisp`, `rest-tests.lisp`, `scoping-tests.lisp`, plus `helpers.lisp` and `model-template.lisp`

The non-frontend code is almost entirely Common Lisp (SBCL).

The frontend is intentionally minimal and schema-driven. It consumes `list-form`/`add-form`/`update-form`, `records`, and `allowed-values` from the API to render dynamic lists and forms.

## Live Introspection: `eval-in-data-ui`

An Elisp helper, `eval-in-data-ui`, evaluates a Common Lisp form against a
**running** Data UI instance (over Slime, in the `:data-ui` package) and returns
the result. Use it to inspect live state — the compiled model, RBAC roles/users,
hook/registry behavior — rather than guessing from source.

- Argument: a single `form`, given as a string (or a Lisp form).
- Evaluation context: the `:data-ui` package, so package-local nicknames like
  `a:` (the rbac library) are available.
- Invoke it via the `Eval` tool.

Example:

    (eval-in-data-ui "(a:list-role-names *rbac*)")
    ;; => ("admin" "admin:exclusive" "guest:exclusive" "logged-in"
    ;;     "parts" "public" "settings")

When you have modified a Lisp source file and want to test the changes, you must
reload the file into the live image before running tests or introspecting. For
example, after editing `lisp/model.lisp`:

    (eval-in-data-ui "(load \"~/common-lisp/data-ui/lisp/model.lisp\")")

Without this step, the running image still holds the old code and tests will pass
(or fail) against the stale version.

This is read-most: prefer it for verification and introspection. It runs real
Lisp against the live image, so treat state-mutating forms with the same care you
would in a REPL, and remember the project rule that Lisp source outside `web/`
and `tests/` is not modified without human permission.

## Running Tests

Use the helper functions in `tests/helpers.lisp` — never call
`fiveam:run!` directly. These helpers handle model loading, database
resetting, and test-suite selection so you don't run unnecessary tests
or forget setup steps.

- `(run-tests)` — runs backend, predicates, and REST suites using the
  `test-model` fixture model (via `with-model`, which resets the
  database and loads the model automatically).
- `(run-modelbank-tests)` — runs the scoping suite using the `modelbank`
  model. (This function is a placeholder name and will be cleaned up
  post-MVP.)

When you need a focused test run for a specific model or suite, add a
new `run-*` function to `tests/helpers.lisp`. AI agents are explicitly
permitted to modify `tests/helpers.lisp` for this purpose — add as many
`run-*` functions as needed to keep the development cycle tight. Do not
modify the test suites themselves (`backend-tests.lisp`,
`scoping-tests.lisp`, etc.) without human permission.

## AI Agent Workflow & Tooling
- Use specialized file tools exclusively: `Glob`, `Grep`, `Read`, `Edit`, `Write` (never `Bash` for file inspection/editing)
- Delegate open-ended research, codebase exploration, or multi-file searches to `researcher` agent
- Use `TodoWrite` to track any multi-step work (3+ distinct steps/phases)
- Delegate complex multi-file edits or systematic refactors to `executor`
- All pre-work discussions and planning remain mandatory before code changes
- **Never stage or commit changes.** The workflow is:
  1. The human provides a clean repo (no untracked or unstaged changes).
  2. The agent makes changes but does not stage or commit anything.
  3. The human reviews unstaged changes and untracked files.
  4. If approved, the human stages and commits. If not, the review cycle repeats.


- All Lisp code (model compiler, backend functions, REST endpoints, database layer, etc.) was written by a human.
- AI agents must not modify any Lisp source files outside `web/` and `tests/helpers.lisp` without special permission from a human. That code is complex and largely outside of AI's current capabilities.
- The React frontend in `web/` was built with AI assistance. Future frontend work will also involve AI.
- Do not refactor, clean up, or "improve" Lisp code unless explicitly instructed.
- Before any code is written, a thorough discussion of the goals must happen.
- Some functions have no in-codebase callers but are intentionally retained (REPL debugging, external scripts). See `docs/model-accessors.md` before removing "dead" code.

## Current Status (MVP)

- **End-to-end pipeline proven (June 2026): model → compile → deploy →
  live app.** The to-do model is deployed and working at
  https://todo.demo.data-ui.com via `scripts/data-ui deploy`.
- Backend compilation, SQL generation, RBAC, and generic endpoints are working
- `models/todos.lisp` contains an example model for a To Do list; load it with
  `(set-model "todos")`. The deploy pipeline deploys `models/default-model.lisp`,
  which is an exact copy of `todos.lisp`.
- Full CRUD works on **all** types — both the built-in RBAC types (users, roles,
  permissions, resources, etc.) and user-defined types
- **Scoping** is implemented at both the view level and the field level.
  - View-level `:scope :user` filters `be-list` results to records owned by
    the current user.
  - Field-level `:scope :user` on a field's `:source` filters aggregated field
    values to the current user (e.g. Model Bank "my rating"). It does **not**
    control field visibility or editability in the UI.
  - Tests in `tests/scoping-tests.lisp`. One view-level behavioral test remains
    flaky / TODO.
- **Write-through** (`:write-to` + `:identity t`) is implemented: compile-time
  validation, `:search-sql`, unique identity indexes, and backend execute path
  in `be-insert` / `be-update` (best-effort `handler-case`, non-transactional).
  Used by Model Bank ratings. Remaining work is edge cases (e.g. clear-to-NULL)
  and Model Bank completion — not the core execute path.
- **Model features now in use** (exercised by `models/modelbank.lisp`):
  - `:tree t` / `:is-leaf` / `:parent-type` / `:fs-backed t` — tree-structured
    types with filesystem backing (directories, file storage)
  - `:path t` — marks the path field on fs-backed types
  - `:autofill :user` — auto-populates a field with the current username
  - `:per-user t` — type-level flag (used by settings); suppresses roles field
  - `:identity t` / `:write-to` — natural keys and related-table upserts
  - `:render-as` UI hint (`:code`, `:image`, `:image-list`) — passed through the
    `:ui` plist to the frontend for custom cell/form rendering
  - `:input-type` values now include `:textbox`, `:select`, `:read-only`,
    `:file`, `:check-box` (in addition to `:line`, `:checkbox-list`)
- React frontend has:
  - Type selector (`/api/types`)
  - Dynamic list with conditional Add / Delete Selected buttons and per-row Edit buttons
  - Delete checkboxes (shown when `delete: true`)
  - Expandable Add/Edit form (uses `add-form` / `update-form`)
  - Inline edit mode (per-row Edit button populates the form with record values)
  - Role management via injected `roles` field (filtered `allowed-values`)
  - Image support: thumbnail grids, image preview modals/lightbox with
    navigation and download
  - The `/api/list` response now includes `create`/`delete`/`update` booleans to control which action buttons are shown
- REST API endpoints (all in `lisp/rest.lisp`):
  - `/api/list`, `/api/item`, `/api/id`, `/api/value`, `/api/value-id`,
    `/api/column` — data retrieval
  - `/api/insert`, `/api/update`, `/api/delete` — CRUD mutations
  - `/api/upload` — file upload (multipart, returns `file-token`)
  - `/api/validate-field`, `/api/validate-form` — validation
  - `/api/types`, `/api/info` — schema/metadata
  - `/api/login`, `/api/refresh` — JWT auth (access + refresh tokens)
  - `/api/file` — file serving (with token auth)
  - `/health` — health check
- File handling: upload, list, and **delete** (including recursive directory
  delete) work. The upload flow does a two-phase POST (`multipart/form-data`
  to `/api/upload`, then a JSON `/api/insert` carrying the returned
  `file-token`)

### Known gaps / next up

- **Write-through edge cases** — core path landed; remaining: clear-to-NULL and
  other edge cases surfaced by Model Bank. Unblocks remaining scoping
  behavioral test polish.
- **Transactions / rollback** — deliberately deferred to post-MVP.
  Lifecycle hooks and write-through are **not** atomic with the primary
  write. A failing hook fails the operation without rolling back prior
  side effects. Database init (`rbac:initialize-database`) is also not
  idempotent. Design hooks and write-through with eventual transaction
  wrapping in mind; do not assume atomicity today.
- **Shell hooks** (`:shell ...`) — planned; rejected at compile time today.
- File **update** is not implemented; may be deferred past the MVP
- UI polish — important for the video; deferred relative to compiler /
  backend capability work (frontend changes are cheaper)
- More example models (prove generality)
- The 30-second create-model→deployed-app video (MVP deliverable,
  deadline end of December 2026)
 
## Deployment (working; read this before touching it)

`scripts/data-ui deploy` (renamed from `scripts/run.sh`) deploys
`models/default-model.lisp` (exact copy of `todos.lisp`) to a k3d cluster on
the deploy host (`evo-x2`) behind HAProxy + TLS. Full detail in
**docs/deployment.md**; session-by-session history of how it was built (with
every bug and fix) in **~/.debug/deployment-work.md**. Key facts:

- The model's top-level keys (`:name`, `:version`, `:domain`, `:repl`)
  drive everything: tag `<name>-<version>-<githash>`, namespace
  `dataui-<name>`, HAProxy map entry for `:domain`, Swank port iff
  `:repl t`.
- Deploy state (rendered manifests, ports.lock, secrets.env) lives
  outside the repo in `~/.local/state/data-ui-deploy/`. ports.lock and
  secrets.env are caches; the live cluster is the source of truth.
  Admin password: `grep ADMIN_PASSWORD
  ~/.local/state/data-ui-deploy/todos/secrets.env` (or the instance name
  derived from the model's `:name`).
- App data is durable on the host: `~/k3d/volumes/dataui` is mounted
  into the k3d node at `/data/dataui`; instance PVs use
  `/data/dataui/<name>-<env>/`.
- TLS: wildcard cert for `*.demo.data-ui.com` (certbot + Route 53
  DNS-01), auto-renewing via certbot.timer + the hook in
  `deploy/letsencrypt-haproxy-hook.sh`. One-time host setup:
  `deploy/setup-tls.sh`.
- `DRY_RUN=1 scripts/data-ui deploy` renders manifests and stops —
  use it before any template change.
- **Trap 1:** rbac's `initialize-database` is NOT idempotent. If first
  boot dies mid-init, the instance wedges ("permission 'create' already
  exists" crash loop). Recovery = clean-slate procedure in
  docs/deployment.md (delete namespace + PVs + host data + optionally
  secrets.env, redeploy). Transactional init is deliberately post-MVP.
- **Trap 2:** `systemctl reload haproxy` does not pick up newly added
  EXTRAOPTS (`-f conf.d`); only a full restart does. The script handles
  this; don't "simplify" it away.
- **Trap 3:** generated admin passwords must satisfy rbac's policy
  (letter + digit + punctuation) — hence the `-a1` suffix in
  `ensure_instance_secrets`.
- **Always run `npm run build` after making frontend changes.**
  Hunchentoot serves the built files from `web/dist/`; there is no dev
  server. `npm run build` runs `tsc` first (typecheck) then `vite build`
  (bundle). Without this step, changes are invisible.
- **`:repl t` works** and exposes Swank for the instance (SSH tunnel
  required to connect). Prefer `:repl nil` in production — it is an
  extra attack surface even behind a tunnel.

## Two Tiers, One Engine

Data UI serves two audiences through a single compiler. Agents must keep the
distinction straight:

- **Expert / self-hosting tier** — the open-source Common Lisp engine. Full power:
  raw Lisp lambdas as hooks/validations, function overrides for lifecycle ops.
  The guardrail is the developer's own judgment (a "shotgun" philosophy — it does
  not stop you from doing whatever you want). **MVP safety note:** transactions
  and rollback are deliberately deferred (see Known gaps). A failing lifecycle
  hook does not roll back the primary write.
- **AI / no-code / hosted tier** — model is pure data (YAML/JSON), hooks come from
  a curated, parameterized registry, no raw-code escape hatch. The constraint is
  the product, not a limitation: it makes the tier safe to operate and consumable
  by an AI. The escape valve for power users is to self-host the open engine.
 
Both tiers reduce to the same hook contract before anything runs; the compiler
never special-cases one against the other.

## Hooks and the Registry

All custom logic (validation + lifecycle) attaches via hooks that reduce to a
single calling contract.

- Validation contract: `(lambda (type-key field-key value user) -> nil | error-string)`
- Lifecycle contract (e.g.): `(lambda (type-key data user &key roles) -> effect)`
- Hooks are **lists**; multiple hooks may attach. Forms:
  - `(:keyword args...)` — registry entry (data-only; AI/no-code/hosted tier) — **supported**
  - `(lambda ...)` — raw Lisp (expert/self-host tier only) — **supported**
  - `(:shell "script" args...)` — compiler-generated subprocess adapter — **planned**;
    rejected at compile time today
- The **registry** generalizes the existing keyword→lambda validation pattern by
  letting entries take **parameters as data**. A registry entry = name +
  parameter-schema + factory. The parameter schema does triple duty: validates
  hosted-tier data, drives the no-code UI palette, and serves as the AI
  function-calling spec. API: `register-hook` (not `register-validation`).
- Shell hooks (when implemented): input as JSON on stdin; exit status/output
  determines success/failure; the adapter will conform to the standard contract.

**MVP caveat (transactions deferred):** lifecycle hooks are NOT
transaction-wrapped. A failing hook fails the operation WITHOUT rollback of
the primary write or earlier hooks in the list. Write-through follows the
same rule: primary write commits first; related-table upserts run after and
are best-effort (`handler-case`, log, continue). Transactions and rollback
are deliberately deferred to post-MVP; the eventual boundary is intended to
wrap primary write + hook list + write-through as a unit. Design hooks with
that future in mind, and never assume atomicity in MVP code or docs.

## Lisp Coding Style

- **Prefer explicit parameter passing over dynamic (special) variables.**
  Dynamic variables (`*foo*`) are reserved for values that are truly
  global to the entire system (e.g. `*compiled-model*`, `*rbac*`).
  Request-scoped or function-chain-scoped values (e.g. the current
  user's ID during a `be-list` call) must be threaded as parameters,
  not bound dynamically. Dynamic binding is technically thread-safe in
  SBCL, but it creates hidden coupling: the reader must know a variable
  is special, find where it's bound, and trace its extent. Explicit
  parameters make data flow visible at the call site.

## Error Reporting: `report-e` and `report-ve`

Never use raw `(error ...)` calls. Use the two macros defined in
`lisp/aux.lisp` instead:

- **`report-e`** — for system/structural errors (unknown hooks, wrong
  kind, unsupported features, model compilation failures). Calls
  `error` under the hood.
- **`report-ve`** — for validation errors (bad user input, invalid
  parameters, schema violations). Signals a validation error condition
  under the hood.

Both generate a **Guru Meditation Number** (a deterministic 6-hex-hash
of the function name + 3 random hex digits, e.g. `897270-ff3`) that is
appended to the error message and logged. The function name can be
recovered from the hash via `gmn-fname`.

### Signature

Both macros share the same form:

```lisp
(report-ve function-name format-string &rest var-specs)
(report-e  function-name format-string &rest var-specs)
```

- **`function-name`** — string, the name of the calling function
  (e.g. `"valid-hook-params"`).
- **`format-string`** — a `format` directive string. Use `~a` (not
  `~s`) for cleaner error messages; raw values are logged separately.
- **`var-specs`** — symbols (variable names), **not** expressions.

### The tilde convention

Each var-spec is a symbol. If the symbol is prefixed with `~`, it is
included as a `format` argument (the tilde is stripped). If not
prefixed, it is **log-only** — it appears in the `pl:plog` entry but
is not interpolated into the error message.

This lets you provide extra debugging context to the log without
cluttering the error shown to the user:

```lisp
(report-ve "valid-hook-params"
           "Hook ~a parameter ~a must be an integer, got ~a"
           ~hook-name ~key ~val)
```

All three vars are logged as `:hook-name`, `:key`, `:val`. All three
are also format arguments (all have tildes).

```lisp
(report-ve "valid-filter"
           "Invalid field key ~a for type ~a."
           ~field-key ~type-key request-id)
```

Here `request-id` is logged but not shown in the error message.

### Practical constraints

- Var-specs must be **symbols** (lexical variable names), not
  arbitrary expressions. Bind expressions to local variables first.
- The log key is derived from the cleaned symbol name (tilde
  stripped), converted to a keyword (e.g. `~hook-name` → `:hook-name`).
- When choosing between `report-e` and `report-ve`: if the error is
  validating user-supplied input against a schema or contract, use
  `report-ve`. If it's a structural/system error (something is wrong
  with the model or code path), use `report-e`.

### Naming convention: `valid-*`

Functions that check or validate data are named with the `valid-`
prefix (e.g. `valid-hook-params`, `valid-target`, `valid-view-scope`).
Do not use `check-` or other prefixes for this purpose. The `valid-*`
family has mixed return semantics — some signal on error and return
`nil` otherwise, others return a resolved value — but they share the
common purpose of validating input against a schema or contract.

## Important Design Decisions

- RBAC types are treated exactly like user-defined types. Thus, you can add a role to a user in the same way that you would add a tag to a To Do item.
- Non-base types automatically receive a `roles` field (checkbox-list) in all forms, unless `:per-user t` is set on the type
- The backend injects filtered `allowed-values.roles` so users only see roles they can assign
- Forms are schema-driven; the frontend does not hard-code field lists
- The `:ui` plist is passed through to the frontend verbatim — new UI hints (like `:render-as`) work without backend changes
- Delete uses the existing single-record-delete endpoint in a loop (acceptable for MVP)
- Keep models small; hide RBAC complexity from model authors

## Working with the Frontend

- Location: `web/`
- Build: `npm install && npm run build` (Hunchentoot serves `web/dist/` — there is no dev server)
- The app is intentionally simple — avoid adding heavy routing, state libraries, or styling until MVP is proven
- All forms render from the schema returned by `/api/list`
- Permission flags (`create`/`delete`/`update`) returned by `/api/list` control visibility of Add, Delete, and Edit controls
- The `:ui` plist is the extension point for frontend rendering — `:render-as`, `:input-type`, and `:table` are consumed by the React components
- Image rendering: `:render-as :image` and `:render-as :image-list` trigger thumbnail grids with modal/lightbox preview; the `:table` key on the field tells the frontend which type to use for `/api/file` URLs

## Goals

Deliver a working MVP by December 2026 that demonstrates the full path from model
to deployed application, including a minimal but functional React UI. The MVP
ships with a 30-second video that goes from nothing to a deployed app.

Post-MVP (for context, not current work): hosted service with YAML/JSON model
input and AI prompts; the curated hook registry as the AI/no-code escape hatch;
and the Marketplace. The Marketplace exists in two forms — (a) a deliberately
tiny **open-source reference** app whose smallness is the proof that "the model
is the whole app," and (b) a **closed-source production** app (YAML/JSON/AI input,
corpus, hosting, one-click deploy). The line is crisp: the open app is the
application logic; the closed product adds operational concerns (hosting, AI front
door, billing) that are infrastructure, not application.

## Current Focus / To Do (MVP)
Deadline: complete MVP, including the demo video, by end of December 2026.

**Model Bank is the priority function.** The MVP must prove that real,
non-trivial applications can be built on Data UI significantly faster
than any alternative — and the way to prove that is to build one.
Model Bank (a model-sharing application with relationships, ownership,
image association, and ratings) is that application. It is no longer
just another example model; it is the fitness function for the MVP.
Gaps surfaced by building Model Bank are, by definition, the
highest-priority work. Polish and rough edges can wait; capability
gaps that block building real apps cannot.

Priorities now:
1. **Build Model Bank** — the live priority function; gaps it surfaces
   drive everything below
2. **Write-through edge cases** — core path (`:write-to` + `:identity t`,
   search SQL, unique identity indexes, `be-insert` / `be-update` execute)
   is landed and non-transactional. Remaining: edge cases (e.g.
   clear-to-NULL) and Model Bank completion. Compiler remains
   authoritative for all SQL.
3. **UI polish** — the video shows the UI; it must look clean.
   Frontend work is deliberately deferred behind compiler/backend
   capability gaps (frontend is cheaper to change).
4. **The 30-second video** — nothing → deployed app
5. File update (only if time permits; otherwise post-MVP)
6. Shell hooks (planned; not required for MVP demo)

**Explicitly post-MVP (do not quietly pull in):**
- Transactions / rollback around primary write + hooks + write-through
- Idempotent database initialization
- Single-statement `ON CONFLICT` upserts (blocked on two-phase resource insert)
- YAML/JSON model input and the hosted AI front door

Frontend known weaknesses (from deployment testing, good first UI
tasks once capability work is unblocked): failed token refresh leaves
the app rendering as logged-in instead of returning to the login form;
"No records" is shown for both empty results and failed requests.
