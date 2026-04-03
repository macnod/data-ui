# Data-UI

**Describe your data model once. Get a complete RBAC-backed application.**

Repo at [github.com/macnod/data-ui](https://github.com/macnod/data-ui).

# Table of Contents

- [Overview](#overview)
- [Core Philosophy](#core-philosophy)
- [Example Model](#example-model)
- [Example Compilation Results](#example-compilation-results)
- [How It Works](#how-it-works)
- [Key Model Features](#key-model-features)
- [API Approach](#api-approach)
- [Current Status (March 2026)](#current-status-march-2026)
- [Goals & Vision](#goals--vision)
- [Related Repositories](#related-repositories)
- [License](#license)

# Overview
If you aim to develop solid, dependable, performant, maintainable, database-backed, ready-to-deploy applications that include full support for Role-Based Access Control (RBAC), and you want a deterministic development process (no countless iterations with an AI only to have to fix the difficult problems yourself in the end), then Data-UI is your friend.

Data-UI is a Common Lisp system that takes a simple nested plist model and compiles it into a full production-ready data application:

- PostgreSQL tables (with defaults, constraints, triggers)
- Smart joined views for lists and forms
- Parameterized CRUD SQL (insert, update, delete)
- Full RBAC integration via the companion [macnod/rbac](https://github.com/macnod/rbac) library
- Generic, model-driven backend functions and API endpoints
- UI hints for dynamic React forms and lists
- Per-field and per-form data validation endpoints
- Complete React frontend
- Kubernetes manifests for deployment

No manual migrations. No per-type boilerplate. Change the model, call `(set-model *model*)`, and everything updates deterministically. Data-UI aims to let you write the model, compile it into an application, and deploy the application in a half hour.


## Core Philosophy

You describe your entities, relations, and UI behavior in one place. Then, Data-UI:

1. Merges your model with a complete RBAC base model (`*base-model*`)
2. Enriches types with default fields (`:id`, `:created-at`, `:updated-at`)
3. Resolves references and generates join tables/views
4. Produces ready-to-run SQL and pre-compiled validation logic
5. Stores everything in `*compiled-model*` for fast runtime use

Generic endpoints like `/api/list?type=todos` and `/api/schema/todos/add-form` work for **any** type — including the built-in RBAC tables themselves.


## Example Model

This example is from what's assigned to the `*model*` variable in `lisp/workbench.lisp`.

    (defparameter *model*
      `(:todos
         (:table t
           :create :auto :update :auto :delete :auto
           :views (:main (:tables (:todos :todo-tags :tags))
                    :tags (:tables (:tags)))
           :fields (:name (:type :text
                            :ui (:label "To Do" :input-type :line)
                            :source (:view :main :column :todo-name :agg :first)
                            :column t :not-null t :unique t)
                     :tags (:type :list
                             :ui (:label "Tags" :input-type :checkbox-list)
                             :source-sel (:list (:view :main :column :tag-name))
                             :source-all (:list (:table :tags :column :tag-name))
                             :ids-table :tags
                             :join-table :todo-tags))
           :list-form (:fields t)
           :update-form (:fields t)
           :add-form (:fields t))

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
           :list-form (:fields t)
           :update-form (:fields t)
           :add-form (:fields t))))

This single definition aims to give you:

- Complete PostgreSQL tables with UUID primary keys, audit timestamps, and automatic `updated_at` triggers
- Smart joined views (e.g. `:main`) that pull related data like tags without extra queries
- Automatic many-to-many relationship handling via declared joiner tables
- Parameterized CRUD SQL ready for safe execution
- Full RBAC protection on every operation (via `macnod/rbac`)
- UI hints (`:label`, `:input-type`, form layouts) that a React frontend can read directly to generate dynamic forms and lists
- A complete React frontend
- Kubernetes manifests for easy, consistent, reproducible deployment

The full RBAC system (`:users`, `:roles`, `:permissions`, `:resources`, and all join tables) is automatically included from `*base-model*`.

### Example Compilation Results

This section presents some tiny pieces of the resulting enriched model, after compilation with `(set-model *model*`.

#### Create Table SQL for `:todos`

    (:TODOS
     (:CREATE-TABLE-SQL
      (:TABLE
         "
           create table if not exists rt_todos (
               id uuid primary key default uuid_generate_v4(),
               created_at timestamp not null default now(),
               updated_at timestamp not null default now(),
               rt_todo_name text not null unique
           )
         "
       :TRIGGER
         "
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
         "
      )
    ⋮

#### View SQL for `:todos`

    (:VIEWS
     (:MAIN
      (:TABLES (:TODOS :TODO-TAGS :TAGS)
       :SQL
         "
           select
             rt_todos.id         rt_todo_id,
             rt_todos.created_at rt_todo_created_at,
             rt_todos.updated_at rt_todo_updated_at,
                                 rt_todo_name,
             rt_tags.id          rt_tag_id,
             rt_tags.created_at  rt_tag_created_at,
             rt_tags.updated_at  rt_tag_updated_at,
                                 rt_tag_name
           from rt_todos
             join rt_todo_tags on rt_todos.id = rt_todo_tags.todo_id
             join rt_tags on rt_todo_tags.tag_id = rt_tags.id
         "
       :COLUMNS
         (:TODOS (:RT-TODO-ID "rt_todo_id" 
                  :RT-TODO-CREATED-AT "rt_todo_created_at"
                  :RT-TODO-UPDATED-AT "rt_todo_updated_at"
                  :RT-TODO-NAME "rt_todo_name")
          :TAGS (:RT-TAG-ID "rt_tag_id"
                 :RT-TAG-CREATED-AT "rt_tag_created_at"
                 :RT-TAG-UPDATED-AT "rt_tag_updated_at"
                 :RT-TAG-NAME "rt_tag_name")))
      :TAGS
      (:TABLES (:TAGS)
       :SQL
         "
           select
             rt_tags.id         rt_tag_id,
             rt_tags.created_at rt_tag_created_at,
             rt_tags.updated_at rt_tag_updated_at,
                                rt_tag_name
           from rt_tags
         "
       :COLUMNS
       (:TAGS (:RT-TAG-ID "rt_tag_id"
               :RT-TAG-CREATED-AT "rt_tag_created_at"
               :RT-TAG-UPDATED-AT "rt_tag_updated_at"
               :RT-TAG-NAME "rt_tag_name")))))

#### Fields Enrichment for `:todos :fields :name`
    
    (:TODOS
     (:FIELDS
      (:NAME
       (:CHECKED NIL
        :UNCHECKED NIL
        :UI (:LABEL "To Do" :INPUT-TYPE :LINE)
        :UNIQUE T
        :PRIMARY-KEY NIL
        :FS-BACKED NIL
        :TARGET NIL
        :IDS-TABLE NIL
        :JOIN-TABLE NIL
        :FORCE-SQL-NAME NIL
        :SQL-NAME "rt_todo_name"
        :SQL-ALIAS "rt_todo_name"
        :TYPE-SQL "text"
        :CREATE-SQL "rt_todo_name text not null unique"
        :ALIAS-KEY :RT-TODO-NAME
        :SOURCE (:VIEW :MAIN :COLUMN :TODO-NAME :AGG :FIRST)
        :TYPE :TEXT
        :COLUMN T
        :NOT-NULL T
        :REFERENCE NIL))))

## How It Works

- `set-model` — Compiles the model, enriches it, generates all SQL/views, and stores the result in `*compiled-model*`.
- **Compilation** — Adds default fields, resolves `:reference` into proper foreign keys, builds joined view SQL, prepares parameterized CRUD statements.
- **Runtime** — Generic backend functions (`be-list`, `be-insert`, `be-update`, `be-delete`) pull pre-generated SQL from the compiled model.
- **RBAC** — Every operation is gated by `user-allowed` from the rbac library. RBAC tables are treated exactly like your own types, so you can manage users, roles, permissions, and resource access through the same UI/API.
- **Validation** — Per-field lambdas or common validator keywords (with support for lists). Pre-compiled into machine code during `set-model`. Separate `be-validate` (full form) and `be-validate-field` (real-time single field) functions.


## Key Model Features

- `:reference` instead of manual IDs for clean relations
- `:views` to explicitly control joins (e.g., `:main (:tables (:todos :todo-tags :tags))`)
- `:ui` hints (`:label`, `:input-type :checkbox-list`, etc.) for frontend rendering
- `:validation` common validation names or lambdas that validate form/field data
- `:join-table` / `:ids-table` for many-to-many relationships
- `:is-joiner t` for explicit join tables
- `:auto` for create/update/delete → generated SQL (or override with your own function)
- Non-base tables get an `rt_` prefix to avoid name collisions with RBAC tables


## API Approach

All endpoints stay **generic** — no per-type handler generation needed:

- `GET /api/schema/:type/:form` → JSON with fields, UI hints, and validation rules (for dynamic React forms)
- `GET /api/list?type=todos` → RBAC-gated results from the compiled view
- `POST /api/validate`, `/api/insert`, `/api/update` → call `be-validate` first, then use compiled SQL

React (or any frontend) can fetch the schema once and render forms/lists automatically.


## Current Status (March 2026)

- Model compilation + enrichment is working
- SQL generation for tables, joined views, triggers, and basic CRUD is implemented and tested against PostgreSQL
- `be-list-internal` and supporting helpers are in place
- Full RBAC integration via `macnod/rbac` (including `user-allowed`, list functions, and `with-rbac`)
- Validation architecture finalized (pre-compiled lambdas + common validators)
- Primary focus now: Solidifying the `be-*` CRUD functions

The active implementation lives in [lisp/workbench.lisp](https://github.com/macnod/data-ui/blob/master/lisp/workbench.lisp). Ignore all other files for now.

Target MVP date: December 2026.


## Goals & Vision

- Build simple multi-table applications extremely quickly
- Make RBAC complexity completely invisible to the developer
- Achieve deterministic, reproducible deploys (target: under 30 minutes from nothing to live app)
- Support future YAML/JSON model input and AI-assisted model generation
- Business model: Sell custom apps to clients, offer hosting & support, and make the system available to others

## Business & Monetization

Data-UI is and will remain fully open source under the MIT license. The core (model compiler, SQL generation, RBAC integration, CRUD layer, etc.) is free for anyone to use, self-host, or modify.

At first, we plan to use Data-UI for our own projects and client work.

After the MVP, to sustain development and offer more value, we plan to provide:
- A user-friendly YAML + visual modeling frontend (for developers who prefer not to write Lisp models)
- Managed hosting (one-click deploy, updates, backups, scaling)
- Professional support, SLAs, and custom development services for clients

If you're building internal tools or client apps and want help, feel free to reach out.

Contributions and feedback are very welcome — this is still early stage!

## Related Repositories

- [macnod/rbac](https://github.com/macnod/rbac) — Mature RBAC library with users, roles, permissions, resources, and comprehensive query functions.
- [macnod/dc-ds](https://github.com/macnod/dc-ds) - Nested data structure navigation and operations.
- [macnod/p-log](https://github.com/macnod/p-log) - Simple logging library with support for multiple backends and structured logs.
- [macnod/dc-eclectic](https://github.com/macnod/dc-eclectic) - A collection of utilities and helpers for Common Lisp development.

## License

MIT

