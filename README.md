# Data UI

**Your whole app, in an email.**

Describe your data once. Get a deployed, RBAC-backed application —
deterministically. Manage users, roles, and permissions live, in the running
app.

This is no longer just a thesis. As of June 2026, the full pipeline works
end to end: a one-page model compiles into a complete application —
PostgreSQL, REST API, RBAC, React frontend — and **one command deploys it
to Kubernetes behind TLS at its own domain**. The first deployed instance
is a to-do app whose entire description fits on a napkin.

Repo at [github.com/macnod/data-ui](https://github.com/macnod/data-ui).

## Table of Contents

- [The Big Idea](#the-big-idea)
- [Why AI Needs Data UI](#why-ai-needs-data-ui)
- [Overview](#overview)
- [Core Philosophy](#core-philosophy)
- [Example Model](#example-model)
- [Example Compilation Results](#example-compilation-results)
- [How It Works](#how-it-works)
- [Key Model Features](#key-model-features)
- [Hooks and the Registry](#hooks-and-the-registry)
- [API Approach](#api-approach)
- [Development](#development)
- [Deployment](#deployment)
- [Current Status (June 2026)](#current-status-june-2026)
- [Road to MVP](#road-to-mvp)
- [Goals & Vision](#goals--vision)
- [The Marketplace](#the-marketplace)
- [Business & Monetization](#business--monetization)
- [Related Repositories](#related-repositories)
- [License](#license)


## The Big Idea

Building solid, evolving, RBAC-heavy collaborative applications requires holding
a web of invariants — every role against every resource against every operation,
changing over time — consistent across thousands of lines of code. This is the
part that is genuinely hard, and it is the part that breaks under iteration,
whether the iteration is done by a human or by an AI.

Data UI lets you express the **entire** application as a small, reviewable
artifact that fits comfortably in the body of an email, and **guarantees** that
the expansion of that artifact into a running system is correct. You describe
your application (entities, relationships, UI hints, etc.) once. The compiler
produces the database, the API, the RBAC enforcement, the frontend, and the
deployment — deterministically, with no per-type boilerplate and no hidden
permission bugs.

Change the model, recompile, and everything updates consistently. The model is
the DNA of the application. At less than a page of code for many applications,
that DNA is tiny compared to the many thousands of lines that would otherwise
be needed to describe such an application.


## Why AI Needs Data UI

The bottleneck in building this class of application is not code generation. A
modern AI can emit plausible code all day. The bottleneck is **specification
compression and invariant enforcement**.

An AI is good at producing a 40-line model. It is bad at producing a 40,000-line
application whose permissions remain globally consistent as the application
evolves, because it pattern-matches locally and drifts globally — it has no
enforcement mechanism. A smarter model does not close this gap; it just drifts
more eloquently.

Data UI closes the gap by reducing the dimensionality of the thing that has to
be gotten right. With Data UI, the AI operates in the regime where it is strong
— producing a small, structured model — and the compiler handles the regime
where the AI is weak — expanding that model into a system with globally
consistent RBAC and relational integrity.

This is the same relationship a programmer has with a type checker: even a
superhuman programmer benefits from offloading invariant-enforcement to a
deterministic tool. **AI needs a substrate like this — and Data UI is it.**

In practice this means the model format is an API for a non-human consumer. An AI
does not write arbitrary code into a Data UI application; it selects from a
defined vocabulary and fills in parameters, exactly as it fills a tool call. See
[Hooks and the Registry](#hooks-and-the-registry).


## Overview

If you aim to develop solid, dependable, performant, maintainable,
database-backed, ready-to-deploy applications that include full support for
Role-Based Access Control (RBAC), and you want a deterministic development
process (no countless iterations with an AI only to have to fix the difficult
problems yourself in the end), then Data UI is your friend.

Data UI is a Common Lisp system that takes a simple nested plist model and
**compiles it** into a full, production-ready data application:

- PostgreSQL tables (with defaults, constraints, triggers)
- Smart joined views for lists and forms
- Parameterized CRUD SQL (insert, update, delete)
- Full RBAC integration via the companion [macnod/rbac](https://github.com/macnod/rbac) library
- Generic, model-driven backend functions and API endpoints
- UI hints for dynamic React forms and lists
- Per-field and per-form data validation endpoints
- View-level scoping (`:scope :user`) for per-user data filtering
- Tree-structured types with filesystem backing (directories, file storage)
- Complete React frontend
- Kubernetes manifests for deployment

No manual migrations. No per-type boilerplate. Change the model, call
`(set-model "todos")`, and everything updates deterministically. And this
is not a half-built promise: write the model, compile it, run
`scripts/data-ui deploy`, and minutes later your application is serving
real users over TLS at its own domain. We know because that is exactly
how the [live demo](#deployment) got there.


## Core Philosophy

You describe your entities, relations, and UI behavior in one place. Then, Data UI:

1. Merges your model with a complete RBAC base model (`*base-model*`)
2. Enriches types with default fields (`:id`, `:created-at`, `:updated-at`)
3. Resolves references and generates join tables/views
4. Produces ready-to-run SQL and pre-compiled validation logic
5. Stores everything in `*compiled-model*` for fast runtime use

Generic endpoints like `/api/list?type=todos` work for **any** type — including
the built-in RBAC tables themselves.

### Two tiers, one engine

Data UI deliberately supports two audiences through one compiler:

- **The expert, self-hosting tier.** Written in Common Lisp, the open-source
  engine gives you full power. You can attach raw Lisp lambdas as hooks and
  validations, override any lifecycle operation with your own function, and do
  anything the language allows. The guardrail here is your own experience and
  judgment. This tier is a shotgun: it does not stop you from doing whatever you
  want.

- **The AI / no-code / hosted tier.** Here the model is pure data (YAML or JSON),
  hooks are chosen from a curated, parameterized registry, and there is no
  raw-code escape hatch. This constraint is not a limitation — it *is* what makes
  the tier safe to operate at scale and consumable by an AI. When a hosted user
  needs power beyond the data vocabulary, the escape valve is to self-host the
  open engine.

Both tiers reduce to the same contract before anything runs, so the compiler
never special-cases one against the other.

## Example Model

This example is the full contents of `models/todos.lisp`. Each file in the
`models/` directory holds a bare model plist (no `defparameter` and no wrapping
variable). The top-level keys (`:title`, `:name`, `:version`, `:domain`, `:repl`)
carry the model's identity, and `:types` holds the type definitions. Load the
model with `(set-model "todos")` — pass just the file name, with no path and no
`.lisp` extension.

```lisp
(:title "To Do List"
  :name "todos"
  :version "0.1"
  :domain "todo.demo.data-ui.com"
  :repl t
  :types
  (:todos
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("todo-users")
      :views (:main (:tables (:todos :todo-tags :tags))
               :tags (:tables (:tags)))
      :fields
      (:name
        (:type :text
          :ui (:label "To Do" :input-type :line)
          :validations (:required
                         (lambda (type-key field-key value user)
                           (declare (ignore user))
                           (unless (< (length value) 20)
                             (validation-error-string
                               type-key field-key value
                               "must be less than 20 characters."))))
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :points
        (:type :integer :default 0
          :ui (:label "Points" :input-type :line)
          :validations (:required)
          :source (:view :main :column :points :agg :first)
          :column t :not-null t)
        :done
        (:type :boolean :default :false
          :ui (:label "Done" :input-type :check-box)
          :source (:view :main :column :done :agg :first)
          :column t :not-null t)
        :tags
        (:type :list
          :ui (:label "Tags" :input-type :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :tags :column :name :agg :list)
          :source-all (:view :tags :table :tags :column :name :agg :list)
          :join-table :todo-tags))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :tags
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("todo-users")
      :fields
      (:name
        (:type :text
          :ui (:label "Tag" :input-type :line)
          :validations (:required)
          :source (:view :main :table :tags :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :todo-tags
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :todos)
        :reference (:target :tags)))))
```

This single definition aims to give you:

- Complete PostgreSQL tables with UUID primary keys, audit timestamps, and automatic `updated_at` triggers
- Smart joined views (e.g. `:main`) that pull related data like tags without extra queries
- Automatic many-to-many relationship handling via declared joiner tables
- Parameterized CRUD SQL ready for safe execution
- Full RBAC protection on every operation (via `macnod/rbac`)
- UI hints (`:label`, `:input-type`, form layouts) that a React frontend can read directly to generate dynamic forms and lists
- A complete React frontend
- Kubernetes manifests for easy, consistent, reproducible deployment

The full RBAC system (`:users`, `:roles`, `:permissions`, `:resources`, and associated
join tables) is automatically included from `*base-model*`. A user settings table is also
included.

### Example Compilation Results

This section presents some tiny pieces of the resulting enriched model, after
compilation with `(set-model "todos")`.

#### Create Table SQL for `:todos`

```lisp
(:TODOS
 (:CREATE-TABLE-SQL
  (:TABLE "
create table if not exists rt_todos (
    id uuid primary key not null references resources(id) on delete cascade,
    created_at timestamp not null default now(),
    updated_at timestamp not null default now(),
    todo_name text not null unique,
    todo_points integer not null default 0,
    todo_done boolean not null default 'false'
)
"
   :TRIGGER "
do $$
begin
    if not exists (
        select 1 from pg_trigger
        where tgname = 'set_rt_todos_updated_at'
        and tgrelid = 'rt_todos'::regclass::oid
    ) then
        create trigger set_rt_todos_updated_at
            before update on rt_todos
            for each row
            execute function set_updated_at_column();
    end if;
end $$;
")
```

#### View SQL for `:todos`

```lisp
(:VIEWS
 (:MAIN
  (:TABLES (:TODOS :TODO-TAGS :TAGS) :SQL "
select
  rt_todos.id             rt_todos_id,
  rt_todos.created_at     rt_todos_created_at,
  rt_todos.updated_at     rt_todos_updated_at,
  rt_todos.todo_name      rt_todos_todo_name,
  rt_todos.todo_points    rt_todos_todo_points,
  rt_todos.todo_done      rt_todos_todo_done,
  rt_todo_tags.id         rt_todo_tags_id,
  rt_todo_tags.created_at rt_todo_tags_created_at,
  rt_todo_tags.updated_at rt_todo_tags_updated_at,
  rt_todo_tags.todo_id    rt_todo_tags_todo_id,
  rt_todo_tags.tag_id     rt_todo_tags_tag_id,
  rt_tags.id              rt_tags_id,
  rt_tags.created_at      rt_tags_created_at,
  rt_tags.updated_at      rt_tags_updated_at,
  rt_tags.tag_name        rt_tags_tag_name
from rt_todos
  left join rt_todo_tags on rt_todos.id = rt_todo_tags.todo_id
  left join rt_tags on rt_tags.id = rt_todo_tags.tag_id"
   :ALIASES
   (:TODOS
    (:ID :RT-TODOS-ID :CREATED-AT :RT-TODOS-CREATED-AT :UPDATED-AT
     :RT-TODOS-UPDATED-AT :NAME :RT-TODOS-TODO-NAME :POINTS
     :RT-TODOS-TODO-POINTS :DONE :RT-TODOS-TODO-DONE)
    :TAGS
    (:ID :RT-TAGS-ID :CREATED-AT :RT-TAGS-CREATED-AT :UPDATED-AT
     :RT-TAGS-UPDATED-AT :NAME :RT-TAGS-TAG-NAME))
   :COLUMNS
   (:TODOS
    (:ID "rt_todos.id" :CREATED-AT "rt_todos.created_at" :UPDATED-AT
     "rt_todos.updated_at" :NAME "rt_todos.todo_name" :POINTS
     "rt_todos.todo_points" :DONE "rt_todos.todo_done")
    :TAGS
    (:ID "rt_tags.id" :CREATED-AT "rt_tags.created_at" :UPDATED-AT
     "rt_tags.updated_at" :NAME "rt_tags.tag_name")))
  :TAGS
  (:TABLES (:TAGS) :SQL "
select
  rt_tags.id         rt_tags_id,
  rt_tags.created_at rt_tags_created_at,
  rt_tags.updated_at rt_tags_updated_at,
  rt_tags.tag_name   rt_tags_tag_name
from rt_tags"
   :ALIASES
   (:TAGS
    (:ID :RT-TAGS-ID :CREATED-AT :RT-TAGS-CREATED-AT :UPDATED-AT
     :RT-TAGS-UPDATED-AT :NAME :RT-TAGS-TAG-NAME))
   :COLUMNS
   (:TAGS
    (:ID "rt_tags.id" :CREATED-AT "rt_tags.created_at" :UPDATED-AT
     "rt_tags.updated_at" :NAME "rt_tags.tag_name")))))
```

#### Fields Enrichment for `:todos :fields :name`

```lisp
(:TODOS
 (:FIELDS
  (:NAME
   (:BASE-FIELD NIL :UI (:LABEL "To Do" :INPUT-TYPE :LINE) :UNIQUE T
    :PRIMARY-KEY NIL :TARGET NIL :JOIN-TABLE NIL :VALIDATIONS
    (#<FUNCTION V-TYPE> #<FUNCTION V-REQUIRED>
     #<FUNCTION (LAMBDA (TYPE-KEY FIELD-KEY VALUE USER)) {B80133ADAB}>)
    :FORCE-SQL-NAME NIL :NAME-SQL "todo_name" :TYPE-SQL "text" :CREATE-SQL
    "todo_name text not null unique" :SOURCE
    (:VIEW :MAIN :COLUMN :NAME :AGG :FIRST :ALIAS-KEY :RT-TODOS-TODO-NAME
     :COLUMN-NAME "rt_todos.todo_name")
    :SOURCE-ALL NIL :TYPE :TEXT :COLUMN T :NOT-NULL T :REFERENCE NIL :DEFAULT
    :NULL))))
```

## How It Works

- `set-model` (in `lisp/model.lisp`) — Compiles the model, enriches it, generates all SQL/views, and stores the result in `*compiled-model*`.
- **Compilation** — Adds default fields, resolves `:reference` into proper foreign keys, builds joined view SQL, prepares parameterized CRUD statements.
- **Runtime** — Generic backend functions (`be-list`, `be-insert`, `be-update`, `be-delete`, `be-item`, etc. in `lisp/backend.lisp`) pull pre-generated SQL from the compiled model.
- **RBAC** — Every operation is gated by `user-allowed` from the rbac library. RBAC tables are treated exactly like your own types, so you can manage users, roles, permissions, and resource access through the same UI/API.
- **Validation** — Per-field lambdas, parameterized registry entries, or common validator keywords (with support for lists). Pre-compiled during `set-model`. Separate validation functions are available.


## Key Model Features

- `:reference` instead of manual IDs for clean relations
- `:target` as a shorthand for `:reference` on non-joiner fields (sets up FK + UUID column)
- `:views` to explicitly control joins (e.g., `:main (:tables (:todos :todo-tags :tags))`)
- `:scope :user` on a view to filter `be-list` results to records owned by the current user
- `:ui` hints (`:label`, `:input-type`, `:render-as`) for frontend rendering
- `:render-as` values: `:code`, `:image`, `:image-list` — trigger specialized frontend rendering (code blocks, thumbnail grids, lightbox preview)
- `:input-type` values: `:line`, `:textbox`, `:select`, `:check-box`, `:checkbox-list`, `:read-only`, `:file`, `:hidden`
- `:validations` common validation names, parameterized registry entries, or lambdas that validate form/field data
- `:join-table` / `:ids-table` for many-to-many relationships
- `:is-joiner t` for explicit join tables
- `:tree t` / `:is-leaf` / `:parent-type` / `:fs-backed t` for tree-structured types with filesystem backing (directories, file storage)
- `:path t` to mark the path field on fs-backed types
- `:autofill :user` to auto-populate a field with the current username
- `:per-user t` (type-level) to suppress the roles field (used by settings)
- `:type-roles` to declare which roles can access a type
- `:force-sql-name` to override the generated SQL column name
- `:auto` for create/update/delete → generated SQL (or override with your own function)
- Lifecycle hooks (`:create`, `:update`, `:delete`, `:post-create`, `:pre-delete`, etc.) that accept registry entries, shell calls, or raw functions
- Non-base tables get an `rt_` prefix to avoid name collisions with RBAC tables


## Hooks and the Registry

Custom logic — validation and lifecycle behavior — attaches through **hooks**.
Every hook, whatever its surface form, reduces to a single calling contract
before it runs, so the compiler treats them uniformly.

There are three ways to express a hook, spanning the two tiers:

| Form in the model            | Who writes the Lisp                  | Tier                       |
|------------------------------|--------------------------------------|----------------------------|
| `(:keyword args...)`         | the registry author (you/community)  | AI / no-code / hosted      |
| `(:shell "script" args...)`  | nobody (compiler generates adapter)  | AI / no-code / hosted      |
| `(lambda ...)`               | the model author, raw                | expert / self-host only    |

### The contract

A **validation** hook conforms to:

    (lambda (type-key field-key value user) -> nil | error-string)

A **lifecycle** hook conforms to (for example):

    (lambda (type-key data user &key roles) -> effect)

Returning `nil` (or no error) means success; returning an error string fails the
operation. Hooks are lists, so multiple hooks can be attached and each reduces to
this contract.

> Note on the MVP: lifecycle hooks are **not** transaction-wrapped in the MVP. If
> one hook in a list fails, the operation fails **without rollback**. Transactions
> and rollback are deliberately deferred to post-MVP. The eventual transaction
> boundary is intended to wrap a whole hook list as a unit; design hooks with that
> future in mind.

### The registry: parameterized, data-only hooks

The registry generalizes the existing keyword-to-lambda pattern used for
validations. A registry entry is a named factory that **closes over parameters
supplied as data** and returns a contract-conforming closure.

For example, a maximum-length validation written as pure data:

```lisp
:validations (:required (:max-length 20))
```

is backed by a registry entry whose Lisp lives in the engine, written once:

```lisp
(register-validation :max-length
  (lambda (max)                                   ; parameter from the model
    (lambda (type-key field-key value user)       ; conforms to the contract
      (unless (< (length value) max)
        (validation-error-string type-key field-key value
          (format nil "must be less than ~d characters." max))))))
```

The model author wrote only data — `(:max-length 20)` — which serializes cleanly
to YAML or JSON. The same pattern applies to lifecycle hooks:

```lisp
:post-create (:add-user-settings)                       ; zero-arg entry
:post-create ((:send-webhook :url "https://...") )      ; parameterized entry
:post-create ((:shell "thumbnail.sh"))                  ; shell adapter
```

### Why the registry matters

Each registry entry carries three things:

1. A **name** (the keyword the model uses).
2. A **parameter schema** (the legal arguments and their types).
3. A **factory** (the Lisp that builds the contract-conforming closure).

The parameter schema does triple duty:

- it **validates** data-only models in the hosted tier,
- it generates the **no-code UI** palette for choosing and configuring hooks, and
- it serves as the **function-calling spec** an AI uses to select and parameterize
  a hook.

This is the mechanism that makes the model AI-consumable: an AI does not write
hooks, it picks registry entries and fills parameters. The Lisp lives in the
registry; the model author — human or AI — writes only data.

### Shell hooks

A shell hook is expressed as a tagged form, e.g. `(:shell "thumbnail.sh" ...)`.
The compiler generates an adapter that wraps the subprocess to satisfy the same
hook contract as any other hook. Input is delivered to the script as JSON on
stdin; the script's exit status and output determine success or failure. Because
the adapter conforms to the standard contract, shell hooks coexist in the same
hook list as registry entries and raw lambdas.

## API Approach

All endpoints stay **generic** — no per-type handler generation needed:

- `GET /api/list?type=todos` → RBAC-gated results from the compiled view, including schema (`list-form`, `add-form`, `update-form`, `allowed-values`) and permission flags (`create`, `delete`, `update`)
- `GET /api/item`, `/api/id`, `/api/value`, `/api/value-id`, `/api/column` → targeted data retrieval
- `POST /api/insert`, `/api/update`, `/api/delete` → CRUD mutations (validation runs first)
- `POST /api/upload` → file upload (multipart, returns `file-token`)
- `POST /api/validate-field`, `/api/validate-form` → per-field and per-form validation
- `GET /api/types`, `/api/info` → schema and metadata
- `POST /api/login`, `/api/refresh` → JWT auth (access + refresh tokens)
- `GET /api/file` → file serving (with token auth)
- `GET /health` → health check

React (or any frontend) fetches items with their schema and renders
forms/lists automatically. The `:ui` plist on each field is the extension
point — `:render-as`, `:input-type`, and `:table` are consumed directly by
the frontend components.


## Development

- Start a repl-environment terminal

    cd data-ui
    scripts/data-ui repl

- Connect Slime to the Data UI Swank server.
  - In Emacs: `M-x slime-connect RET localhost RET 4010`
    - Host: localhost
    - Port: 4010, or whatever the repl-environment terminal says
- Compile a model
  - In Slime: `(set-model "todos")`
  - Optionally, run tests with: `(run-tests)`
- Build the frontend (one-time, or after frontend changes)

      cd data-ui/web
      npm install
      npm run build

- Start the web server
  - In Slime: `(start-web-server)`
  - The server serves both the API and the frontend

- Navigate to http://localhost:8080


## Deployment

Deployment is part of the compiler's promise, not an afterthought. The
model itself declares the application's identity:

```lisp
(:title "To Do List"
  :name "todo"
  :version "0.1"
  :domain "todo.demo.data-ui.com"
  :repl t
  :types ...)
```

and one command turns that into a running, public application:

```sh
scripts/data-ui deploy
```

Behind that command: the model is compile-checked against a throwaway
database, the release is tagged from the model's version plus the git
hash, a Docker image is built (React frontend compiled in one stage,
precompiled SBCL runtime in another), Kubernetes manifests are rendered
from templates and applied to a k3d cluster (each instance in its own
namespace, with its own PostgreSQL and persistent volumes), and HAProxy
routing is updated so the model's `:domain` serves the app over TLS — a
wildcard Let's Encrypt certificate that renews itself.

The deploy is deterministic and repeatable: every fact is derived from
the model and the git commit. Secrets and port assignments are generated
once and thereafter recovered from the live cluster, so a deploy can be
re-run from a fresh machine without breaking a running instance.

The full story — every step, every file, where the admin password lives,
how cert renewal works, troubleshooting — is in
[docs/deployment.md](docs/deployment.md).


## Current Status (June 2026)

The project is in active development, and the core claim is now
demonstrated end to end:

- **The full pipeline works: model → compiled application → deployed,
  TLS-terminated, RBAC-backed app at its own domain.** The example to-do
  model is live in production on a k3d cluster, deployed with a single
  command.
- Full CRUD operations work via the backend, REST API, and frontend React
  code, across **all** types — both the built-in RBAC types (users, roles,
  permissions, resources, etc.) and user-defined types.
- JWT-based authentication (access + refresh tokens) protects the API.
- **Scoping** (`:scope :user` at the view level) is implemented in the
  compiler and backend — `be-list` results are filtered to records owned
  by the current user. Field-level scoping is not yet implemented.
- **Model features in active use** (exercised by `models/modelbank.lisp`):
  tree-structured types with filesystem backing (`:tree`, `:is-leaf`,
  `:parent-type`, `:fs-backed`), path fields (`:path`), auto-populated
  fields (`:autofill :user`), per-user types (`:per-user`), and UI hints
  for code blocks, images, and image lists (`:render-as`).
- File handling: uploading, listing, and deleting files and directories
  works end-to-end (uploads use a two-phase flow: `multipart/form-data`
  POST to `/api/upload`, then a JSON `/api/insert` carrying the returned
  `file-token`). File **update** is not yet implemented and may be
  deferred past the MVP.
- React frontend: log in, navigate as a user, perform CRUD with RBAC
  enforcement, manage roles, upload and preview images (thumbnail grids
  with modal/lightbox), inline edit mode. The UI works but needs polish
  — this is a current focus.
- Tests for compilation, predicates, backend, REST, and scoping are in
  `tests/` (FiveAM): `predicate-tests.lisp`, `backend-tests.lisp`,
  `rest-tests.lisp`, `scoping-tests.lisp`, plus `helpers.lisp` and
  `model-template.lisp`.

Model compilation, SQL generation for tables/views/triggers, RBAC
integration, validation, CRUD, and Kubernetes deployment are implemented
and exercised in production. Work continues on UI refinement and
additional example models.

Deliberately deferred to post-MVP: transactions and rollback (including
idempotent database initialization). See
[Hooks and the Registry](#hooks-and-the-registry).

See `lisp/model.lisp` for the current `*base-model*` and the `models/`
directory for example models (one per file, e.g. `todos.lisp`,
`modelbank.lisp`, `widgets.lisp`), each loadable with
`(set-model "todos")`, `lisp/backend.lisp` for the `be-*` API,
`lisp/rest.lisp` for HTTP endpoints, and the `tests/` directory for
usage examples. Contributions welcome — this is early stage!


## Road to MVP

**Target: a complete MVP by the end of December 2026, including a
30-second video that goes from nothing — no database, no code — to a
deployed, working application.**

Odds of hitting the date: **strong.** The reasoning, plainly:

- The riskiest milestones are already behind us. The compiler, RBAC
  integration, generic API, and — as of June — the entire deployment
  pipeline are working in production. These were the make-or-break
  items; everything that could have invalidated the core thesis has
  instead confirmed it.
- What remains is effort-bounded, not research-bounded: building Model
  Bank (the fitness function), write-through for related tables,
  field-level scoping, UI polish, and the video itself. None of it
  requires solving an open problem; six months remain for work measured
  in weeks.
- The main schedule risks are scope creep and polish perfectionism. The
  mitigations are written down: file update may ship after MVP,
  transactions are explicitly post-MVP, and the UI bar is "clean and
  demo-ready," not "design award."

**Model Bank is the priority function.** The MVP must prove that real,
non-trivial applications can be built on Data UI significantly faster
than any alternative — and the way to prove that is to build one.
Model Bank (a model-sharing application with relationships, ownership,
image association, and ratings) is that application. Gaps surfaced by
building Model Bank are, by definition, the highest-priority work.


## Goals & Vision

Data UI exists to solve a problem that existing low-code and backend tools handle
poorly: building production-grade, multi-user applications that allow users to
interact with each other, share resources, and that require robust, evolving
role-based access control.

Most collaborative applications — internal tools, client portals, team
workspaces, resource-sharing systems — need fine-grained permissions that change
over time. Current low-code platforms either offer weak or bolted-on RBAC, or
they generate large amounts of opaque code that must be manually finished and
maintained. The result is slow iteration, hidden permission bugs, and painful
refactoring when requirements change.

Data UI takes a different approach. You describe your data model, relationships,
and UI hints in one small, reviewable plist. The system compiles this
into:

- PostgreSQL tables, views, and triggers
- Parameterized CRUD operations with full RBAC enforcement
- A complete schema-driven React frontend
- Generic REST endpoints that work for every type, including the built-in RBAC types themselves

Because RBAC entities (users, roles, permissions) are treated as first-class
types, permission changes are made through the same interface as any other data —
no separate admin layer or model edits required.

The model acts as the DNA of the application. Small, auditable changes produce
deterministic, system-wide updates. This makes iteration fast and safe: refine
your vision by editing the model rather than rewriting code.

For custom logic and external integrations, Data UI provides typed hooks that
receive pre-evaluated authorization context and a well-defined payload.
Developers attach behavior without rebuilding the core application architecture.

The result is a tool that lets technical users, small teams, and AI agents build
reliable, RBAC-protected collaborative applications much faster and with greater
long-term maintainability than traditional development or existing low-code
platforms.

**MVP target: December 2026.** A minimal but production-capable system that
delivers a complete RBAC-protected application (database, React frontend, and
Kubernetes deployment) from a small model in under 30 minutes. The MVP ships with
a 30-second video that goes from nothing to a deployed app. The deployment
pipeline — historically the riskiest part of such a promise — is already
working in production; see [Road to MVP](#road-to-mvp).

After the MVP, planned work includes a hosted service with JSON/YAML model input
and AI prompts, a curated hook registry as the AI-and-no-code escape hatch, the
marketplace described below, and professional support services.


## The Marketplace

The marketplace is the growth engine. It does three things at once: it solves
onboarding by making the first experience *copy a working thing* rather than
*author from a blank page*; it creates network effects; and it becomes a
retrieval corpus that both humans and AI draw from — find a near-fit model, adapt
it, change the appearance, deploy.

Creating a new application becomes: find a model in the marketplace, copy it,
modify it slightly, optionally restyle it, and deploy.

We pursue the marketplace in two forms:

- **(a) An open-source reference Marketplace.** Its job is not to be the product —
  it is to be the **proof**. You can stare at a fraction of a page of model and
  realize it represents the entire Marketplace application. Its smallness is the
  point and is defended as a feature. Being open and copyable, it is also the
  canonical first entry in the corpus — the template everyone forks.

- **(b) A closed-source, production-grade Marketplace.** This is where iteration
  and revenue live: YAML/JSON model input, AI prompts, the corpus, search,
  hosting, and one-click deploy.

The line between them is crisp: the open reference app **is** the application
logic; the closed product adds **operational** concerns (hosting, AI front door,
billing, scaling, moderation) that are infrastructure, not application. Keeping
that line clear is what lets the proof and the product reinforce each other
rather than undercut the central claim.


## Business & Monetization

The Data UI **engine** is and will remain fully open source under the MIT license.
The core (model compiler, SQL generation, RBAC integration, CRUD layer, the
reference Marketplace) is free for anyone to use, self-host, or modify.

The **hosting** is a separate, closed-source product. Initially it has no
raw-code escape hatch: models are pure data, hooks come from the curated
registry. When a hosted user needs power beyond the data vocabulary, the escape
valve is to self-host the open engine.

Initially we will focus on building custom applications for clients while
dogfooding the tool on our own projects.

After the MVP, to fund continued development and provide additional value to
users, we plan to provide:

- A user-friendly YAML/JSON + visual modeling frontend, plus AI prompts (for those who prefer not to write Lisp models)
- Managed hosting (one-click deploy, updates, backups, scaling)
- The production-grade Marketplace
- Professional support, SLAs, and custom development services for clients

If you're building internal tools or client apps and want help, feel free to
reach out. Contributions and feedback are very welcome — this is still early
stage!


## Related Repositories

- [macnod/rbac](https://github.com/macnod/rbac) — Mature RBAC library with users, roles, permissions, resources, and comprehensive query functions.
- [macnod/dc-ds](https://github.com/macnod/dc-ds) - Nested data structure navigation and operations.
- [macnod/p-log](https://github.com/macnod/p-log) - Simple logging library with support for multiple backends and structured logs.
- [macnod/dc-eclectic](https://github.com/macnod/dc-eclectic) - A collection of utilities and helpers for Common Lisp development.


## License

MIT
