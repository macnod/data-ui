# Data UI

**Your whole app, in an email.**

Built by Donnie Cameron at [Sinister Code](https://sinistercode.com), programs that write programs.

Describe your data once. Get a deployed, RBAC-backed application, deterministically. Manage users, roles, and permissions live, in the running app.

This is no longer just a thesis. As of July 2026, the full pipeline works end to end: a one-page model compiles into a complete application, PostgreSQL, REST API, RBAC, React frontend, and **one command deploys it to Kubernetes behind TLS at its own domain**. The first deployed instance, a to-do app whose entire description fits on a napkin, went from model to production that way.

Repo at [github.com/macnod/data-ui](https://github.com/macnod/data-ui).

## Start Here

Data UI compiles a small model into a complete, RBAC-backed application — database, API, React frontend, Kubernetes deployment — in one command.

- **Evaluating as an investor or partner?** [Live demos](#live-demos) → [The Big Idea](#the-big-idea) → [Current Status](#current-status-september-2026) → [Road to MVP](#road-to-mvp) → [Business & Monetization](#business--monetization), then [docs/competitive-landscape.md](docs/competitive-landscape.md) for the field.
- **Need an application built?** Client engagements open after the MVP (target: December 2026). Early conversations welcome: [Contact](#contact).
- **Engineer, or evaluating the tech?** Read top to bottom; the meat starts at [Overview](#overview) and the [Example Model](#example-model).

## Table of Contents

- [Start Here](#start-here)
- [Live Demos](#live-demos)
- [The Big Idea](#the-big-idea)
- [The Thesis in Six Lines](#the-thesis-in-six-lines)
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
- [Current Status (September 2026)](#current-status-september-2026)
- [Road to MVP](#road-to-mvp)
- [Goals & Vision](#goals--vision)
- [Competitive Landscape](docs/competitive-landscape.md)
- [57 vs ~20k](docs/57-vs-20k.org)
- [The Marketplace](#the-marketplace)
- [Business & Monetization](#business--monetization)
- [Related Repositories](#related-repositories)
- [Contact](#contact)
- [License](#license)


## Live Demos

Three applications compiled from models in this repo, running now.

- [To Do List](https://todo-stg.demo.data-ui.com/) — the napkin-sized model from [Example Model](#example-model), compiled and deployed. Log in as `demos` / `TryDataUI2026!` to add, edit, and tag items.
- [Books & Authors](https://books-stg.demo.data-ui.com/) — relationships, cover images, and a rating rollup. Same login: `demos` / `TryDataUI2026!`.

Both reset to a known state every morning at 04:10 (US Pacific). Anything you change is gone by then; that is the point of a shared demo.

- [Model Bank](https://modelbank.demo.data-ui.com/) — a gallery of models, with ownership, images, and ratings. Log in as `guest` (no password); it is read-only. Write access, including the Generate and Deploy buttons, is available on request: [Contact](#contact).

The `demos` password is public by design. The apps are sandboxed, reset nightly, and can be taken offline in under a minute.


## The Big Idea

Building solid, evolving, RBAC-heavy collaborative applications requires holding a web of invariants: every role against every resource against every operation, changing over time, consistent across thousands of lines of code. This is the part that is genuinely hard, and it is the part that breaks under iteration, whether the iteration is done by a human or by an AI.

Data UI lets you express the **entire** application as a small, reviewable artifact that fits comfortably in the body of an email, and **guarantees** that the expansion of that artifact into a running system is correct. You describe your application (entities, relationships, UI hints, etc.) once. The compiler produces the database, the API, the RBAC enforcement, the frontend, and the deployment, deterministically, with no per-type boilerplate and no hidden permission bugs.

Change the model, recompile, and everything updates consistently. The model is the DNA of the application. At less than a page of code for many applications, that DNA is tiny compared to the many thousands of lines that would otherwise be needed to describe such an application. The napkin-sized to-do model is ~57 lines; a Java team matching what it produces would need to write an estimated ~20,000 lines of production artifacts. The walkthrough: [57 vs ~20k](docs/57-vs-20k.org).


## The Thesis in Six Lines

> The 40,000-line application is dead. The 40-line model that compiles into one just won.

> Change one file. Recompile. Every schema, every endpoint, every permission check, every UI form updates together, because they were never separate things.

> An application that once required a team, a quarter, and a budget now fits in the body of an email and deploys in one command.

> Before Data UI: "We'll need a backend engineer, a frontend engineer, a DevOps person, and six weeks." After Data UI: "I just sent you the model."

> You don't maintain a Data UI application. You maintain a 40-line description, and the running system is a pure function of that description. The concept of "maintenance" as we know it just disappeared.

> Data UI is the only product that lets an AI effectively author complete, UI-administrable, row-level RBAC applications. AI can now ship production applications. Not suggest code. Not draft migrations. Ship. The missing piece was never the AI. It was the invariant engine that lets the AI's output be trusted.


## Why AI Needs Data UI

The bottleneck in building this class of application is not code generation. A modern AI can emit plausible code all day. The bottleneck is **specification compression and invariant enforcement**.

An AI is good at producing a 40-line model. It is bad at producing a 40,000-line application whose permissions remain globally consistent as the application evolves, because it pattern-matches locally and drifts globally, it has no enforcement mechanism. A smarter model does not close this gap; it just drifts more eloquently.

Data UI closes the gap by reducing the dimensionality of the thing that has to be gotten right. With Data UI, the AI operates in the regime where it is strong (producing a small, structured model) and the compiler handles the regime where the AI is weak (expanding that model into a system with globally consistent RBAC and relational integrity).

This is the same relationship a programmer has with a type checker: even a superhuman programmer benefits from offloading invariant-enforcement to a deterministic tool. **AI needs a substrate like this, and Data UI is it.**

In practice this means the model format is an API for a non-human consumer. An AI does not write arbitrary code into a Data UI application; it selects from a defined vocabulary and fills in parameters, exactly as it fills a tool call. See [Hooks and the Registry](#hooks-and-the-registry).


## Overview

If you aim to develop solid, dependable, performant, maintainable, database-backed, ready-to-deploy applications that include full support for Role-Based Access Control (RBAC), and you want a deterministic development process (no countless iterations with an AI only to have to fix the difficult problems yourself in the end), then Data UI is your friend.

Data UI is a Common Lisp system that takes a simple nested plist model and **compiles it** into a full, production-ready data application:

- PostgreSQL tables (with defaults, constraints, triggers)
- Smart joined views for lists and forms
- Parameterized CRUD SQL (insert, update, delete)
- Full RBAC integration via the companion [macnod/rbac](https://github.com/macnod/rbac) library
- Generic, model-driven backend functions and API endpoints
- UI hints for dynamic React forms and lists
- Per-field and per-form data validation endpoints
- View-level and field-level scoping (`:scope :user`) for per-user data
  filtering
- Tree-structured types with filesystem backing (directories, file storage)
- Complete React frontend
- Kubernetes manifests for deployment

No manual migrations. No per-type boilerplate. Change the model, call `(set-model "todos")`, and everything updates deterministically. And this is not a half-built promise: write the model, compile it, run `scripts/data-ui deploy todos`, and minutes later your application is serving real users over TLS at its own domain. That is exactly how the first deployed instance got there (July 2026).


## Core Philosophy

You describe your entities, relations, and UI behavior in one place. Then, Data UI:

1. Merges your model with a complete RBAC base model (`*base-model*`)
2. Enriches types with default fields (`:id`, `:created-at`, `:updated-at`)
3. Resolves references and generates join tables/views
4. Produces ready-to-run SQL and pre-compiled validation logic
5. Stores everything in `*compiled-model*` for fast runtime use

Generic endpoints like `/api/list?type=todos` work for **any** type, including the built-in RBAC tables themselves.

### Two tiers, one engine

Data UI deliberately supports two audiences through a single compiler:

- **The expert, self-hosting tier.** Written in Common Lisp, the open-source engine gives you full power. You can write custom registry entries (hook factories in Lisp), override lifecycle ops (`:create` / `:update` / `:delete`) with your own functions, and do anything the language allows. The guardrail here is your own experience and judgment. This tier is a shotgun: it does not stop you from doing whatever you want.

- **The AI / no-code / hosted tier.** Here the model is pure data (YAML or JSON), hooks are chosen from a curated, parameterized registry, and there is no raw-code escape hatch. This constraint is not a limitation. It *is* what makes the tier safe to operate at scale and consumable by an AI. When a hosted user needs power beyond the data vocabulary, the escape valve is to self-host the open engine.

Both tiers reduce to the same contract before anything runs, so the compiler never special-cases one against the other.

**Why Common Lisp stays.** Despite its age, Common Lisp remains the most powerful programming language in a purely technical sense. Its object system, condition system, live image, incremental compilation of running code to efficient machine code, and full programmability have no real peers. Without that power gap, Data UI would not exist.

That the power of Common Lisp is invisible to most working programmers is itself a moat: it keeps casual competitors out while steering sophisticated users who want the capability without becoming a Lisp shop straight to the hosted product. The open engine remains fully available for those who want that power unmediated.

## Example Model

This example is abbreviated from `models/todos.lisp` (which also sets
`:domain-stg`, `:guest-allowed t`, `:guest-auto nil`, `:api-roles`,
`:default-sort`, `:sortable` / `:searchable` on fields, `"public"` in
`:type-roles`, and `:type-roles` overlays on the built-in `:users` /
`:roles` / `:permissions` types). Each file in the `models/` directory
holds a bare model plist (no `defparameter` and no wrapping variable).
The top-level keys (`:title`, `:name`, `:version`, `:domain`,
`:domain-stg`, `:repl`, `:guest-allowed`, `:guest-auto`, `:api-roles`,
`:landing-page`, `:new-roles`) carry the model's identity, and `:types`
holds the type definitions. Load the model with `(set-model "todos")`,
pass just the file name, with no path and no `.lisp` extension. Prefer
`:repl nil` in production (see [Deployment](#deployment)).

```lisp
(:title "To Do List"
  :name "todos"
  :version "0.1"
  :domain "todo.demo.data-ui.com"
  :repl t
  :landing-page :todos
  :types
  (:todos
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("todo-users")
      :views (:main (:tables (:todos :todo-tags :tags))
               :tags (:tables (:tags)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "To Do" :widget :textbox)
          :validations (:required (:max-length :max 80))
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :done
        (:type :boolean :default :false
          :ui (:label "Done" :widget :checkbox)
          :source (:view :main :column :done :agg :first)
          :column t :not-null t)
        :tags
        (:type :list
          :ui (:label "Tags" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :tags :column :name :agg :distinct)
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
        (:type :text :identity t
          :ui (:label "Tag" :widget :textbox)
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
- UI hints (`:label`, `:widget`, form layouts) that a React frontend can read directly to generate dynamic forms and lists
- A complete React frontend
- Kubernetes manifests for easy, consistent, reproducible deployment

The full RBAC system (`:users`, `:roles`, `:permissions`, `:resources`, and associated join tables) is automatically included from `*base-model*`. A user settings table is also included.

What that ~57-line model produces, and why a matching Java application is estimated at ~20,000 lines: [57 vs ~20k](docs/57-vs-20k.org).

### Example Compilation Results

This section presents some tiny pieces of the resulting enriched model, after compilation with `(set-model "todos")`.

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
     :RT-TODOS-UPDATED-AT :NAME :RT-TODOS-TODO-NAME
     :DONE :RT-TODOS-TODO-DONE)
    :TAGS
    (:ID :RT-TAGS-ID :CREATED-AT :RT-TAGS-CREATED-AT :UPDATED-AT
     :RT-TAGS-UPDATED-AT :NAME :RT-TAGS-TAG-NAME))
   :COLUMNS
   (:TODOS
    (:ID "rt_todos.id" :CREATED-AT "rt_todos.created_at" :UPDATED-AT
     "rt_todos.updated_at" :NAME "rt_todos.todo_name"
     :DONE "rt_todos.todo_done")
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

- `set-model` (in `lisp/model.lisp`): Compiles the model, enriches it, generates all SQL/views, and stores the result in `*compiled-model*`.
- **Compilation**: Adds default fields, resolves `:reference` into proper foreign keys, builds joined view SQL, prepares parameterized CRUD statements.
- **Runtime**: Generic backend functions (`be-list`, `be-insert`, `be-update`, `be-delete`, `be-item`, etc. in `lisp/backend.lisp`) pull pre-generated SQL from the compiled model.
- **RBAC**: Every operation is gated by `user-allowed` from the rbac library. RBAC tables are treated exactly like your own types, so you can manage users, roles, permissions, and resource access through the same UI/API.
- **Validation**: Parameterized registry entries or common validator keywords (with support for lists). Pre-compiled during `set-model`. Separate validation functions are available.

### What `*compiled-model*` actually is

The compiler stores its output in `*compiled-model*`, a single structure that is simultaneously the application specification (data), the deployment configuration (data), and the executable application logic (native machine code). SBCL compiles every backend function, every RBAC check, and every hook, including validation, lifecycle, and action hooks, to native x86-64 or ARM instructions. No interpreter. No VM. No JIT warmup. When a validation hook runs, it calls a function pointer to compiled code that was placed in the model at compile time. The model is not just a description of the application; it *is* the application, in executable form.

For a detailed comparison of Data UI's approach against existing tools, see [Competitive Landscape](docs/competitive-landscape.md).


## Key Model Features

- `:reference` instead of manual IDs for clean relations
- `:target` as a shorthand for `:reference` on non-joiner fields (sets up FK + UUID column)
- `:views` to explicitly control joins (e.g., `:main (:tables (:todos :todo-tags :tags))`)
- `:scope :user` on a view to filter `be-list` results to records owned by the current user
- `:scope :user` on a field's `:source` to filter aggregated field values to the current user (e.g. "my rating")
- `:identity t` marks a field as the natural key used for write-through lookups and unique indexes
- `:write-to` declares related-table upserts from a field write (e.g. rating → ratings row); non-transactional in MVP
- `:ui` hints (`:label`, `:widget`, `:read-only`, `:precision`, `:options`) for frontend rendering
- `:widget` values: `:textbox`, `:textarea`, `:code`, `:stars`, `:select`, `:checkbox`, `:checkbox-list`, `:file`, `:hidden`, `:password`, `:button`, `:image`, `:image-list`
- `:options` with `:widget :select` for static dropdowns (the stored value is the option string)
- `:read-only t` on `:ui` renders a field's display variant instead of an editor (boolean flag, not a widget value)
- `:button` field type with `:action`: clickable control on the update form that runs a registry action hook; compiler synthesizes a companion `:<field>-status` column
- `:validations` common validation names or parameterized registry entries that validate form/field data
- `:join-table` for many-to-many relationships
- `:is-joiner t` for explicit join tables
- `:tree t` / `:is-leaf` / `:parent-type` / `:fs-backed t` for tree-structured types with filesystem backing (directories, file storage)
- `:path t` to mark the path field on fs-backed types
- `:autofill :user` to auto-populate a field with the current username
- `:user-setting t` (type-level) to mark per-user settings types; auto-sets `:suppress-roles t` and derives category `:settings` if omitted
- `:suppress-roles t` (type-level) to suppress the injected `roles` field in forms
- `:category` (type-level) to place a type in the selector: `:user`, `:settings` (Settings tab), or `:system`. Author key, not reserved to built-ins
- `:type-roles` to declare which roles can access a type (also overrides defaults on built-in types)
- `:sortable t` / `:searchable t` on a field to enable list sorting (clickable headers) and free-text search (ILIKE)
- `:default-sort` (type-level) to declare the sort used when a request sends none
- `:rollup t` + `:grain` for read-only analytical types (SQL `GROUP BY` aggregates, no physical table)
- `:compose` to build a stored field value from other fields server-side (e.g. full name from parts)
- `:landing-page` (top-level) to declare which type the frontend shows on load (resolved per-user via `be-landing-page`)
- `:guest-allowed` / `:guest-auto` / `:api-roles` (top-level) for passwordless guest login and app-level endpoint gating
- `:force-sql-name` to override the generated SQL column name
- `:auto` for create/update/delete → generated SQL (or override with your own function)
- Lifecycle hooks (`:pre-create`, `:post-create`, `:pre-update`, `:post-update`, `:pre-delete`, `:post-delete`) via registry entries (raw functions are internal base-model only)
- Action hooks on `:button` fields (e.g. `:deploy-model`, `:generate-model`) via the same registry
- Non-base tables get an `rt_` prefix to avoid name collisions with RBAC tables

Full model vocabulary: [docs/model-reference.md](docs/model-reference.md).
Hook contracts and builtins: [docs/hook-registry.md](docs/hook-registry.md).


## Hooks and the Registry

Custom logic (validation, lifecycle, and **actions**) attaches through **hooks**. Every hook reduces to a single calling contract per kind before it runs, so the compiler treats them uniformly.

Hooks are expressed through the registry (the sole author surface form):

| Form in the model    | Who writes the Lisp                  | Tier                  | Status    |
|----------------------|--------------------------------------|-----------------------|-----------|
| `(:keyword args...)` | the registry author (you/community)  | AI / no-code / hosted | Supported |
| `:keyword`           | the registry author (zero-arg entry) | AI / no-code / hosted | Supported |

Raw lambdas are not a model-author form; they exist only as an internal pass-through for base-model lifecycle functions. Expert/self-host power users extend the vocabulary with `register-hook`, not by embedding code in the model.

### The contracts

A **validation** hook conforms to:

    (lambda (type-key field-key value user) -> nil | error-string)

A **lifecycle** hook conforms to (for example):

    (lambda (type-key data user &key id roles record) -> nil | plist)

An **action** hook conforms to:

    (lambda (type-key field-key record user
             &key roles status-field set-status)
      -> nil | plist)

Validation: return `nil` on success or an error string on failure. Lifecycle: return `nil` for no change, or a plist whose keys are merged into `data` (overwriting existing keys); non-plist non-nil is an error. This is the **data-effect contract** that powers `:compose-string` and future cross-table hooks. Action: return `nil` (or any non-async result) for sync completion, or `(:async t :message "...")` so a worker owns status via `set-status`. Hooks are lists where the slot allows multiple entries; each reduces to its kind's contract.

Action hooks attach to `:button` fields on the **update form only**. The compiler synthesizes a companion `:<field>-status` column (`idle` → `running` → `complete` | `failed: <reason>`). `POST /api/actions` invokes them via `be-action`. Details: [docs/hook-registry.md](docs/hook-registry.md).

> **MVP caveat: transactions deferred:** lifecycle hooks are **not** transaction-wrapped. If one hook in a list fails, the operation fails **without rollback** of the primary write or earlier hooks. The same rule applies to write-through (`:write-to`): the primary row commits first; related-table upserts run after and are best-effort. Action hooks are likewise non-transactional; a process restart can leave status stuck at `running` (no job queue in MVP, operator resets manually). Transactions and rollback (including idempotent database initialization) are deliberately deferred to post-MVP. The eventual transaction boundary is intended to wrap primary write + hook list + write-through as a unit; design hooks with that future in mind, and never assume atomicity in MVP code or docs.

### The registry: parameterized, data-only hooks

The registry provides parameterized, data-only hooks for validations, lifecycle, and actions. A registry entry is a named factory that **closes over parameters supplied as data** and returns a contract-conforming closure.

For example, a maximum-length validation written as pure data:

```lisp
:validations (:required (:max-length :max 20))
```

is backed by a registry entry whose Lisp lives in the engine, written once:

```lisp
(register-hook :max-length :validation
  '(:max :integer)
  (lambda (&key max)                              ; parameter from the model
    (lambda (type-key field-key value user)       ; conforms to the contract
      (when (and value (stringp value) (not (equal value "")))
        (when (> (length value) max)
          (validation-error-string type-key field-key value
            (format nil "must be at most ~d characters." max)))))))
```

The model author wrote only data, `(:max-length :max 20)`, which serializes cleanly to YAML or JSON. The same pattern applies to lifecycle and action hooks:

```lisp
:post-create (:add-user-settings)                       ; zero-arg entry
:post-create ((:send-webhook :url "https://...") )      ; parameterized entry
:action (:deploy-model :field :model)                   ; action on a :button
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

This is the mechanism that makes the model AI-consumable: an AI does not write hooks, it picks registry entries and fills parameters. The Lisp lives in the registry; the model author, human or AI, writes only data.

## API Approach

All endpoints stay **generic**, no per-type handler generation needed:

- `GET /api/list?type=todos` → RBAC-gated results from the compiled view, including schema (`list-form`, `add-form`, `update-form`, `allowed-values`) and permission flags (`create`, `delete`, `update`)
- `GET /api/item`, `/api/id`, `/api/value`, `/api/value-id`, `/api/column` → targeted data retrieval
- `POST /api/insert`, `/api/update`, `/api/delete` → CRUD mutations (validation runs first)
- `POST /api/actions` → run an action hook on a `:button` field (`{"type", "id", "field"}`)
- `POST /api/upload` → file upload (multipart, returns `file-token`)
- `POST /api/validate-field`, `/api/validate-form` → per-field and per-form validation
- `GET /api/types`, `/api/info` → schema and metadata (`/api/types` returns a `:category` per type: `:system`, `:settings`, or `:user`; authors set `:category` or it is derived)
- `GET /api/public-info` → unauthenticated app title + guest-login flags (login screen)
- `GET /api/css-variables` → theme variables from the user's settings row
- `POST /api/login`, `/api/refresh` → JWT auth (access + refresh tokens; `/api/login` also serves the passwordless guest path when the model sets `:guest-allowed t`)
- `GET /api/file` → file serving (with token auth)
- `GET /health` → health check

React (or any frontend) fetches items with their schema and renders forms/lists automatically. The `:ui` plist on each field is the extension point, `:widget`, `:read-only`, `:precision`, and `:table` are consumed directly by the frontend components.


## Development

For full reference on the `scripts/data-ui` and `scripts/publish-data-ui`
commands, see [scripts/README.md](scripts/README.md).

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

- Navigate to http://localhost:8081 or whatever the repl-environment terminal
  says

## Deployment

Deployment is part of the compiler's promise, not an afterthought. The model itself declares the application's identity:

```lisp
(:title "To Do List"
  :name "todos"
  :version "0.1"
  :domain "todo.demo.data-ui.com"
  :domain-stg "todo-stg.demo.data-ui.com"
  ;; Prefer :repl nil in production (extra attack surface; SSH tunnel still required)
  :repl t
  :landing-page :todos
  :types ...)
```

and one command turns that into a running, public application:

```sh
scripts/data-ui deploy todos
```

Behind that command: the model is compile-checked against a throwaway database, the release is tagged from the model's version plus the git hash, a Docker image is built (React frontend compiled in one stage, precompiled SBCL runtime in another), Kubernetes manifests are rendered from templates and applied to a k3d cluster (each instance in its own namespace, with its own PostgreSQL and persistent volumes), and HAProxy routing is updated so the model's `:domain` serves the app over TLS, a wildcard Let's Encrypt certificate that renews itself.

The deploy is deterministic and repeatable: every fact is derived from the model and the git commit. Secrets and port assignments are generated once and thereafter recovered from the live cluster, so a deploy can be re-run from a fresh machine without breaking a running instance.

`:repl t` works and exposes Swank for the instance (reachable over an SSH tunnel). Prefer `:repl nil` in production, it is an extra attack surface even behind a tunnel.

The full story, every step, every file, where the admin password lives, how cert renewal works, troubleshooting, is in [docs/deployment.md](docs/deployment.md).

Instances run in one of three *environments* — development (`scripts/data-ui repl`), staging (`repl <profile>`, exposed via the model's `:domain-stg`), or production (`deploy`, which serves `:domain`). Tiers are product offerings; environments are where an instance runs. See [docs/deployment.md](docs/deployment.md) → Environments.


## Current Status (September 2026)

The project is in active development, and the core claim is now demonstrated end to end:

- **The full pipeline works: model → compiled application → deployed, TLS-terminated, RBAC-backed app at its own domain.** The example to-do model was deployed to production on a k3d cluster with a single command (July 2026). The public demos now run as staged apps on their own domains — see [Live Demos](#live-demos).
- Full CRUD operations work via the backend, REST API, and frontend React code, across **all** types, both the built-in RBAC types (users, roles, permissions, resources, etc.) and user-defined types.
- JWT-based authentication (access + refresh tokens) protects the API, including the passwordless **guest login** path (`:guest-allowed t`, `:guest-auto`, `:api-roles`).
- **Scoping** is implemented at both the view level and the field level. View-level `:scope :user` filters `be-list` results to records owned by the current user. Field-level scoping (`:scope :user` on a field's `:source`) filters aggregated field values to the current user (e.g. "my rating" on Model Bank). It does not control field visibility or editability in the UI.
- **Write-through** (`:write-to` + `:identity t`) is implemented: related- table upserts run from `be-insert` / `be-update` (best-effort, non- transactional). Used by Model Bank ratings. Some edge cases (e.g. clear-to-NULL) remain open.
- **Action hooks** (`:button` fields + `:action`, `POST /api/actions`, companion status column, sync/async protocol) are implemented. The `:deploy-model` registry entry powers Model Bank deploy-from-record (role-gated to `deployer`, writes to `models/local/`); `:generate-model` calls an LLM to author a model from a description (role-gated to `ai-user`).
- **List UX**: clickable column sorting (`:sortable t`), free-text search with negative terms (`:searchable t`, "Not…" box), `:default-sort`, filter chips on M2M checkbox values, pagination driven by `total`.
- **Rollup types** (`:rollup t` + `:grain`): read-only analytical types served from `GROUP BY` SQL (e.g. books-by-rating).
- **Model features in active use** (exercised by `models/modelbank.lisp` and `models/books.lisp`): tree-structured types with filesystem backing (`:tree`, `:is-leaf`, `:parent-type`, `:fs-backed`), path fields (`:path`), auto-populated fields (`:autofill :user`), per-user settings types (`:user-setting`), write-through ratings (`:write-to`, `:identity`), action buttons (`:button`, `:action`), `:compose` server-side field composition, static selects (`:ui :options`), and UI hints for code blocks, images, image lists, and star ratings (`:widget :stars`).
- File handling: uploading, listing, and deleting files and directories works end-to-end (uploads use a two-phase flow: `multipart/form-data` POST to `/api/upload`, then a JSON `/api/insert` carrying the returned `file-token`). File **update** is not yet implemented and may be deferred past the MVP.
- React frontend: log in (including guest), navigate as a user, perform CRUD with RBAC enforcement, manage roles, upload and preview images (thumbnail grids with modal/lightbox), inline edit mode, action buttons on update forms. The UI works but needs polish; this is a current focus.
- **Ops tooling** around the engine: environments (development / staging / production) with host profiles exposed at each model's `:domain-stg`; `e-demo` / `demo` systemd-managed demo lifecycles with nightly golden resets; a shared **snapshot pool** (`scripts/data-ui snapshot save|restore|list|drop|migrate`) for capturing and moving database + file state between environments.
- Tests: `tests/` holds 30+ FiveAM suites (compilation, predicates, backend, REST, scoping, actions, hooks, rollups, M2M, compose, search/sort, guest/API-roles, generator, and more — see `data-ui.asd`). One view-level scoping behavioral test remains flaky / TODO.

Model compilation, SQL generation for tables/views/triggers, RBAC integration, validation, CRUD, write-through, action hooks, and Kubernetes deployment are implemented and exercised. Work continues on Model Bank completion, write-through edge cases, UI refinement, and additional example models.

Deliberately deferred to post-MVP (do not assume these exist today):

- **Transactions and rollback.** Lifecycle hooks and action hooks are not transaction-wrapped. A failing hook fails the operation without rolling back the primary write or earlier hooks. Write-through (`:write-to`) follows the same rule: primary write commits first; related-table upserts are best-effort. Idempotent database initialization is also deferred (see deployment Trap 1).
- Single-statement `ON CONFLICT` upserts (blocked on the two-phase resource insert).
- YAML/JSON model input and the hosted AI front door.

See [Hooks and the Registry](#hooks-and-the-registry) and [docs/hook-registry.md](docs/hook-registry.md) for the hook contracts and the MVP atomicity caveat. Model vocabulary: [docs/model-reference.md](docs/model-reference.md).

See `lisp/model.lisp` for the current `*base-model*` and the `models/` directory for example models (one per file, e.g. `todos.lisp`, `modelbank.lisp`, `widgets.lisp`), each loadable with `(set-model "todos")`, `lisp/backend.lisp` for the `be-*` API, `lisp/rest.lisp` for HTTP endpoints, and the `tests/` directory for usage examples. Contributions welcome; this is early stage!


## Road to MVP

**Target: a complete MVP by the end of December 2026, including a 30-second video that goes from nothing, no database, no code, to a deployed, working application.**

Odds of hitting the date: **strong.** The reasoning, plainly:

- The riskiest milestones are already behind us. The compiler, RBAC integration, generic API, write-through, and, as of July, the entire deployment pipeline are working in production. These were the make-or-break items; everything that could have invalidated the core thesis has instead confirmed it.
- What remains is effort-bounded, not research-bounded: finishing Model Bank (the fitness function), hardening write-through edge cases, UI polish, and the video itself. None of it requires solving an open problem; five months remain for work measured in weeks.
- The main schedule risks are scope creep and polish perfectionism. The mitigations are written down: file update may ship after MVP, **transactions and rollback are explicitly post-MVP** (hooks and write-through are not atomic with the primary write today), and the UI bar is "clean and demo-ready," not "design award." Frontend polish is deliberately sequenced after compiler/backend capability work because frontend changes are cheaper.

**Model Bank is the priority function.** The MVP must prove that real, non-trivial applications can be built on Data UI significantly faster than any alternative, and the way to prove that is to build one. Model Bank (a model-sharing application with relationships, ownership, image association, and ratings) is that application. Gaps surfaced by building Model Bank are, by definition, the highest-priority work.


## Goals & Vision

Data UI exists to solve a problem that existing low-code and backend tools handle poorly: building production-grade, multi-user applications that allow users to interact with each other, share resources, and that require robust, evolving role-based access control.

Most collaborative applications (internal tools, client portals, team workspaces, resource-sharing systems) need fine-grained permissions that change over time. Current low-code platforms either offer weak or bolted-on RBAC, or they generate large amounts of opaque code that must be manually finished and maintained. The result is slow iteration, hidden permission bugs, and painful refactoring when requirements change.

Data UI takes a different approach. You describe your data model, relationships, and UI hints in one small, reviewable plist. The system compiles this into:

- PostgreSQL tables, views, and triggers
- Parameterized CRUD operations with full RBAC enforcement
- A complete schema-driven React frontend
- Generic REST endpoints that work for every type, including the built-in RBAC types themselves

Because RBAC entities (users, roles, permissions) are treated as first-class types, permission changes are made through the same interface as any other data, no separate admin layer or model edits required.

The model acts as the DNA of the application. Small, auditable changes produce deterministic, system-wide updates. This makes iteration fast and safe: refine your vision by editing the model rather than rewriting code.

For custom logic and external integrations, Data UI provides typed hooks that receive pre-evaluated authorization context and a well-defined payload. Developers attach behavior without rebuilding the core application architecture.

The result is a tool that lets technical users, small teams, and AI agents build reliable, RBAC-protected collaborative applications much faster and with greater long-term maintainability than traditional development or existing low-code platforms.

**MVP target: December 2026.** A minimal but production-capable system that delivers a complete RBAC-protected application (database, React frontend, and Kubernetes deployment) from a small model in under 30 minutes. The MVP ships with a 30-second video that goes from nothing to a deployed app. The deployment pipeline, historically the riskiest part of such a promise, is already working in production; see [Road to MVP](#road-to-mvp).

After the MVP, planned work includes a hosted service with JSON/YAML model input and AI prompts, a curated hook registry as the AI-and-no-code escape hatch, the marketplace described below, and professional support services.


## The Marketplace

The marketplace is the growth engine. It does three things at once: it solves onboarding by making the first experience *copy a working thing* rather than *author from a blank page*; it creates network effects; and it becomes a retrieval corpus that both humans and AI draw from, find a near-fit model, adapt it, change the appearance, deploy.

Creating a new application becomes: find a model in the marketplace, copy it, modify it slightly, optionally restyle it, and deploy.

I pursue the marketplace in two forms:

- **(a) An open-source reference Marketplace.** Its job is not to be the product, it is to be the **proof**. You can stare at a fraction of a page of model and realize it represents the entire Marketplace application. Its smallness is the point and is defended as a feature. Being open and copyable, it is also the canonical first entry in the corpus, the template everyone forks.

- **(b) A closed-source, production-grade Marketplace.** This is where iteration and revenue live: YAML/JSON model input, AI prompts, the corpus, search, hosting, and one-click deploy.

The line between them is crisp: the open reference app **is** the application logic; the closed product adds **operational** concerns (hosting, AI front door, billing, scaling, moderation) that are infrastructure, not application. Keeping that line clear is what lets the proof and the product reinforce each other rather than undercut the central claim.


## Business & Monetization

The Data UI **engine** is and will remain fully open source under the MIT license. The core (model compiler, SQL generation, RBAC integration, CRUD layer, the reference Marketplace) is free for anyone to use, self-host, or modify.

The **hosting** is a separate, closed-source product. Initially it has no raw-code escape hatch: models are pure data, hooks come from the curated registry. When a hosted user needs power beyond the data vocabulary, the escape valve is to self-host the open engine.

After the MVP, I will focus on building custom applications for clients while dogfooding the tool on my own projects.

After the MVP, to fund continued development and provide additional value to users, I plan to provide:

- A user-friendly YAML/JSON + visual modeling frontend, plus AI prompts (for those who prefer not to write Lisp models)
- Managed hosting (one-click deploy, updates, backups, scaling)
- The production-grade Marketplace
- Professional support, SLAs, and custom development services for clients

If you're building internal tools or client apps and want help, feel free to reach out; engagements begin after the MVP (December 2026).

Contributions and feedback are very welcome; this is still early stage!


## Related Repositories

- [macnod/rbac](https://github.com/macnod/rbac): Mature RBAC library with users, roles, permissions, resources, and comprehensive query functions.
- [macnod/dc-ds](https://github.com/macnod/dc-ds): Nested data structure navigation and operations.
- [macnod/p-log](https://github.com/macnod/p-log): Simple logging library with support for multiple backends and structured logs.
- [macnod/dc-eclectic](https://github.com/macnod/dc-eclectic): A collection of utilities and helpers for Common Lisp development.

## Contact

Donnie Cameron - macnod@gmail.com - [Sinister Code](https://sinistercode.com) - [LinkedIn](https://linkedin.com/in/macnod)


## License

MIT
