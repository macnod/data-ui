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
- `models/` – Example models, one per file (e.g. `todos.lisp`, `parts.lisp`, `file-server.lisp`). Each file holds a bare model plist (no `defparameter`, no wrapping variable). Load one with `(set-model "todos")` — pass just the file name, with no path and no `.lisp` extension.
- `lisp/backend.lisp` – Backend functions (`be-list`, `be-insert`, `be-update`, `be-delete`, etc.)
- `lisp/rest.lisp` – REST API layer (Hunchentoot handlers)
- `web/` – React frontend (Vite + TypeScript)

The non-frontend code is almost entirely Common Lisp (SBCL). Tests live in the `tests/` directory.

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

This is read-most: prefer it for verification and introspection. It runs real
Lisp against the live image, so treat state-mutating forms with the same care you
would in a REPL, and remember the project rule that Lisp source outside `web/` is
not modified without human permission.

## AI Agent Workflow & Tooling
- Use specialized file tools exclusively: `Glob`, `Grep`, `Read`, `Edit`, `Write` (never `Bash` for file inspection/editing)
- Delegate open-ended research, codebase exploration, or multi-file searches to `researcher` agent
- Use `TodoWrite` to track any multi-step work (3+ distinct steps/phases)
- Delegate complex multi-file edits or systematic refactors to `executor`
- All pre-work discussions and planning remain mandatory before code changes


- All Lisp code (model compiler, backend functions, REST endpoints, database layer, etc.) was written by a human.
- AI agents must not modify any Lisp source files outside `web/` without special permission from a human. That code is complex and largely outside of AI's current capabilities.
- The React frontend in `web/` was built with AI assistance. Future frontend work will also involve AI.
- Do not refactor, clean up, or "improve" Lisp code unless explicitly instructed.
- Before any code is written, a thorough discussion of the goals must happen.

## Current Status (MVP)

- **End-to-end pipeline proven (June 2026): model → compile → deploy →
  live app.** The to-do model is deployed and working at
  https://todo.demo.data-ui.com via `scripts/data-ui deploy`.
- Backend compilation, SQL generation, RBAC, and generic endpoints are working
- `models/todos.lisp` contains an example model for a To Do list; load it with `(set-model "todos")`. The deploy pipeline deploys `models/default-model.lisp`.
- Full CRUD works on **all** types — both the built-in RBAC types (users, roles,
  permissions, resources, etc.) and user-defined types
- React frontend has:
  - Type selector (`/api/types`)
  - Dynamic list with conditional Add / Delete Selected buttons and per-row Edit buttons
  - Delete checkboxes (shown when `delete: true`)
  - Expandable Add/Edit form (uses `add-form` / `update-form`)
  - Role management via injected `roles` field (filtered `allowed-values`)
  - The `/api/list` response now includes `create`/`delete`/`update` booleans to control which action buttons are shown
- File handling: upload, list, and **delete** (including recursive directory
  delete) work. The upload flow does a two-phase POST (`multipart/form-data`
  to `/api/upload`, then a JSON `/api/insert` carrying the returned
  `file-token`)

### Known gaps / next up

- File **update** is not implemented; may be deferred past the MVP
- UI polish — important, a current focus
- More example models (prove generality) — important, a current focus
- The 30-second create-model→deployed-app video (MVP deliverable,
  deadline end of December 2026)

## Deployment (working; read this before touching it)

`scripts/data-ui deploy` (renamed from `scripts/run.sh`) deploys
`models/default-model.lisp` to a k3d cluster on the deploy host
(`evo-x2`) behind HAProxy + TLS. Full detail in **docs/deployment.md**;
session-by-session history of how it was built (with every bug and fix)
in **~/.debug/deployment-work.md**. Key facts:

- The model's top-level keys (`:name`, `:version`, `:domain`, `:repl`)
  drive everything: tag `<name>-<version>-<githash>`, namespace
  `dataui-<name>`, HAProxy map entry for `:domain`, Swank port iff
  `:repl t`.
- Deploy state (rendered manifests, ports.lock, secrets.env) lives
  outside the repo in `~/.local/state/data-ui-deploy/`. ports.lock and
  secrets.env are caches; the live cluster is the source of truth.
  Admin password: `grep ADMIN_PASSWORD
  ~/.local/state/data-ui-deploy/todo/secrets.env`.
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
- `vite dev` does not typecheck; `npm run build` runs `tsc` first. Run
  a build before deploying frontend changes.
- The model's `:repl` key has a TODO — it is "not taking effect yet"
  and must be `nil` in production once it does.

## Two Tiers, One Engine

Data UI serves two audiences through a single compiler. Agents must keep the
distinction straight:

- **Expert / self-hosting tier** — the open-source Common Lisp engine. Full power:
  raw Lisp lambdas as hooks/validations, function overrides for lifecycle ops.
  The guardrail is the developer's own judgment (a "shotgun" philosophy — it does
  not stop you from doing whatever you want). MVP-stage safety concerns (e.g.
  transactions) are deliberately deferred.
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
- Hooks are **lists**; multiple hooks may attach. Three forms coexist:
  - `(:keyword args...)` — registry entry (data-only; AI/no-code/hosted tier)
  - `(:shell "script" args...)` — compiler-generated subprocess adapter (data-only)
  - `(lambda ...)` — raw Lisp (expert/self-host tier only)
- The **registry** generalizes the existing keyword→lambda validation pattern by
  letting entries take **parameters as data**. A registry entry = name +
  parameter-schema + factory. The parameter schema does triple duty: validates
  hosted-tier data, drives the no-code UI palette, and serves as the AI
  function-calling spec.
- Shell hooks: input as JSON on stdin; exit status/output determines
  success/failure; the adapter conforms to the standard contract.

**MVP caveat:** lifecycle hooks are NOT transaction-wrapped. A failing hook fails
the operation WITHOUT rollback. Transactions/rollback are deferred to post-MVP;
the eventual boundary is intended to wrap a whole hook list as a unit.

## Important Design Decisions

- RBAC types are treated exactly like user-defined types. Thus, you can add a role to a user in the same way that you would add a tag to a To Do item.
- Non-base types automatically receive a `roles` field (checkbox-list) in all forms
- The backend injects filtered `allowed-values.roles` so users only see roles they can assign
- Forms are schema-driven; the frontend does not hard-code field lists
- Delete uses the existing single-record-delete endpoint in a loop (acceptable for MVP)
- Keep models small; hide RBAC complexity from model authors

## Working with the Frontend

- Location: `web/`
- Run: `npm install && npm run dev` (proxies `/api` to backend on port 8081)
- The app is intentionally simple — avoid adding heavy routing, state libraries, or styling until MVP is proven
- All forms render from the schema returned by `/api/list`
- Permission flags (`create`/`delete`/`update`) returned by `/api/list` control visibility of Add, Delete, and Edit controls

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
See TODO.md for the live prioritized list. Deadline: complete MVP,
including the demo video, by end of December 2026. Priorities now:
1. **UI polish** — the video shows the UI; it must look clean
2. **More example models** — prove the compiler generalizes
   (`models/parts.lisp`, `models/file-server.lisp` are candidates)
3. **The 30-second video** — nothing → deployed app
4. File update (only if time permits; otherwise post-MVP)

Frontend known weaknesses (from deployment testing, good first UI
tasks): failed token refresh leaves the app rendering as logged-in
instead of returning to the login form; "No records" is shown for both
empty results and failed requests.
