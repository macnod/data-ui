# Data UI – Agent Overview

Data UI compiles a small nested-plist model into a complete RBAC-backed application (PostgreSQL + generic backend + REST API + React frontend).

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

- `lisp/model.lisp` – Model definition (`*model*`, `*base-model*`) and compilation
- `lisp/backend.lisp` – Backend functions (`be-list`, `be-insert`, `be-update`, `be-delete`, etc.)
- `lisp/rest.lisp` – REST API layer (Hunchentoot handlers)
- `web/` – React frontend (Vite + TypeScript)

The non-frontend code is almost entirely Common Lisp (SBCL). Tests live in the `tests/` directory.

The frontend is intentionally minimal and schema-driven. It consumes `list-form`/`add-form`/`update-form`, `records`, and `allowed-values` from the API to render dynamic lists and forms.

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

- Backend compilation, SQL generation, RBAC, and generic endpoints are working
- `*model*`, defined in `lisp/model.lisp`, contains an example model for a To Do list
- Full CRUD works on **all** types — both the built-in RBAC types (users, roles,
  permissions, resources, etc.) and user-defined types
- React frontend has:
  - Type selector (`/api/types`)
  - Dynamic list with conditional Add / Delete Selected buttons and per-row Edit buttons
  - Delete checkboxes (shown when `delete: true`)
  - Expandable Add/Edit form (uses `add-form` / `update-form`)
  - Role management via injected `roles` field (filtered `allowed-values`)
  - The `/api/list` response now includes `create`/`delete`/`update` booleans to control which action buttons are shown
- File handling: uploading and listing files and directories works. The upload
  flow does a two-phase POST (`multipart/form-data` to `/api/upload`, then a JSON
  `/api/insert` carrying the returned `file-token`)

### Known gaps / next up

- File **delete** and **update** are not yet implemented (only upload + list)
- The UI is functional but rough and needs significant refinement/polish

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

Deliver a working MVP by December that demonstrates the full path from model to deployed application, including a minimal but functional React UI.

## Current Focus / To Do (MVP)
See TODO.md for the live prioritized list. Key items include:
- Model/compiler extensions: password hashing, read-only types, :fs-backed support
- JWT auth, schema endpoints, validation
- React: auth context, dynamic forms/menus, file handling UI
- Tests, deployment manifests, model input formats, demo video
