# Data-UI

**Describe your data model once. Get a complete RBAC-backed application.**

If you aim to develop solid, dependable, performant, maintainable, database-backed, ready-to-deploy applications that include full support for Role-Based Access Control (RBAC), and you want a deterministic development process (no countless iterations with an AI only to have to fix the difficult problems yourself in the end), then Data-UI is your friend.

Data-UI is a Common Lisp system that takes a simple nested plist model and compiles it into a full production-ready data application:

-   PostgreSQL tables (with defaults, constraints, triggers)
-   Smart joined views for lists and forms
-   Parameterized CRUD SQL (insert, update, delete)
-   Full RBAC integration via the companion [macnod/rbac](https://github.com/macnod/rbac) library
-   Generic, model-driven backend functions and API endpoints
-   UI hints for dynamic React forms and lists
-   Per-field and per-form data validation endpoints
-   Complete React frontend
-   Kubernetes manifests for deployment

No manual migrations. No per-type boilerplate. Change the model, call `(set-model *model*)`, and everything updates deterministically. Data-UI aims to let you write the model, compile it into an application, and deploy the application in a half hour.


## Core Philosophy

You describe your entities, relations, and UI behavior in one place. Then, Data-UI:

1.  Merges your model with a complete RBAC base model (`*base-model*`)
2.  Enriches types with default fields (`:id`, `:created-at`, `:updated-at`)
3.  Resolves references and generates join tables/views
4.  Produces ready-to-run SQL and pre-compiled validation logic
5.  Stores everything in `*compiled-model*` for fast runtime use

Generic endpoints like `/api/list?type=todos` and `/api/schema/todos/add-form` work for **any** type — including the built-in RBAC tables themselves.


## Example Model (`*model*` in `lisp/workbench.lisp`)

    (defparameter *model*
      `(:todos
         (:table t
           :create :auto :update :auto :delete :auto
           :views (:main (:tables (:todos :todo-tags :tags)))
           :fields (:name (:type :text
                            :ui (:label "To Do" :input-type :line)
                            :column t :not-null t :unique t)
                     :tags (:type :list
                             :ui (:label "Tags" :input-type :checkbox-list)
                             :source (:list (:view :main :column :tag-name))
                             :checked (:list (:view :main :column :tag-name))
                             :unchecked (:list (:table :tags :column :tag-name))
                             :ids-table :tags
                             :join-table :todo-tags))
           :list-form (:fields (:name t))
           :update-form (:fields (:name t :tags t))
           :add-form (:fields (:name t :tags t)))

         :todo-tags
         (:table t :is-joiner t
           :fields (:reference (:target :todos)
                     :reference (:target :tags)))

         :tags
         (:table t
           :create :auto :update :auto :delete :auto
           :fields (:name (:type :text
                            :ui (:label "Tag" :input-type :line)
                            :column t :not-null t :unique t))
           :list-form (:fields (:name t))
           :update-form (:fields (:name t))
           :add-form (:fields (:name t)))))

This single definition aims to give you:

-   Complete PostgreSQL tables with UUID primary keys, audit timestamps, and automatic `updated_at` triggers
-   Smart joined views (e.g. `:main`) that pull related data like tags without extra queries
-   Automatic many-to-many relationship handling via declared joiner tables
-   Parameterized CRUD SQL ready for safe execution
-   Full RBAC protection on every operation (via `macnod/rbac`)
-   UI hints (`:label`, `:input-type`, form layouts) that a React frontend can read directly to generate dynamic forms and lists
-   A complete React frontend
-   Kubernetes manifests for easy, consistent, reproducible deployment

The full RBAC system (`:users`, `:roles`, `:permissions`, `:resources`, and all join tables) is automatically included from `*base-model*`.


## How It Works

-   `set-model` — Compiles the model, enriches it, generates all SQL/views, and stores the result in `*compiled-model*`.
-   **Compilation** — Adds default fields, resolves `:reference` into proper foreign keys, builds joined view SQL, prepares parameterized CRUD statements.
-   **Runtime** — Generic backend functions (`be-list`, `be-insert`, `be-update`, `be-delete`) pull pre-generated SQL from the compiled model.
-   **RBAC** — Every operation is gated by `user-allowed` from the rbac library. RBAC tables are treated exactly like your own types, so you can manage users, roles, permissions, and resource access through the same UI/API.
-   **Validation** — Per-field lambdas or common validator keywords (with support for lists). Pre-compiled into machine code during `set-model`. Separate `be-validate` (full form) and `be-validate-field` (real-time single field) functions.


## Key Model Features

-   `:reference` instead of manual IDs for clean relations
-   `:views` to explicitly control joins (e.g., `:main (:tables (:todos :todo-tags :tags))`)
-   `:ui` hints (`:label`, `:input-type :checkbox-list`, etc.) for frontend rendering
-   `:validation` common validation names or lambdas that validate form/field data
-   `:join-table` / `:ids-table` for many-to-many relationships
-   `:is-joiner t` for explicit join tables
-   `:auto` for create/update/delete → generated SQL (or override with your own function)
-   Non-base tables get an `rt_` prefix to avoid name collisions with RBAC tables


## API Approach

All endpoints stay **generic** — no per-type handler generation needed:

-   `GET /api/schema/:type/:form` → JSON with fields, UI hints, and validation rules (for dynamic React forms)
-   `GET /api/list?type=todos` → RBAC-gated results from the compiled view
-   `POST /api/validate`, `/api/insert`, `/api/update` → call `be-validate` first, then use compiled SQL

React (or any frontend) can fetch the schema once and render forms/lists automatically.


## Current Status (March 2026)

-   Model compilation + enrichment is working
-   SQL generation for tables, joined views, triggers, and basic CRUD is implemented and tested against PostgreSQL
-   `be-list-internal` and supporting helpers are in place
-   Full RBAC integration via `macnod/rbac` (including `user-allowed`, list functions, and `with-rbac`)
-   Validation architecture finalized (pre-compiled lambdas + common validators)
-   Primary focus now: Solidifying the `be-*` CRUD functions

The active implementation lives in `lisp/workbench.lisp`. Ignore all other files.

Target MVP date: December 2026.


## Goals & Vision

-   Build simple multi-table applications extremely quickly
-   Make RBAC complexity completely invisible to the developer
-   Achieve deterministic, reproducible deploys (target: under 30 minutes from model change to live app)
-   Support future YAML/JSON model input and AI-assisted model generation
-   Business model: Sell custom apps to clients, offer hosting & support, and make the system available to others


## Related Repositories

-   [macnod/rbac](https://github.com/macnod/rbac) — Mature RBAC library with users, roles, permissions, resources, and comprehensive query functions.


## License

MIT
