# REST API Reference

All endpoints are defined in `lisp/rest.lisp` using Hunchentooth
`define-easy-handler`. Authentication is JWT-based (access + refresh
tokens via `/api/login` and `/api/refresh`). Most data endpoints
require a valid access token in the `Authorization` header.

## Data retrieval

### `GET /api/list`

List records for a type. Returns records, form schemas (`list-form`,
`add-form`, `update-form`), `allowed-values` for dropdown/checkbox
fields, and permission flags (`create`, `delete`, `update`).

**Parameters:** `type` (required), plus optional pagination/filter
parameters.

### `GET /api/item`

Fetch a single record by UUID.

**Parameters:** `type`, `id`.

### `GET /api/id`

Resolve a record UUID by identity field value.

**Parameters:** `type`, `field`, `value`.

### `GET /api/value`

Fetch a single column value from a record.

**Parameters:** `type`, `id`, `field`.

### `GET /api/value-id`

Fetch a column value, resolving the record by identity field.

**Parameters:** `type`, `field`, `value`, `column`.

### `GET /api/column`

Fetch all values for a column across a type.

**Parameters:** `type`, `field`.

## CRUD mutations

### `POST /api/insert`

Create a new record. Validation runs before the SQL insert. Lifecycle
hooks (`:pre-create` / `:post-create`) run around the write.

**Body:** JSON with `type` and field values.

### `POST /api/update`

Update an existing record. Validation runs before the SQL update.
Lifecycle hooks (`:pre-update` / `:post-update`) run around the write.

**Body:** JSON with `type`, `id`, and field values.

### `POST /api/delete`

Delete a record. For filesystem-backed types, deletes files/directories
recursively. Lifecycle hooks (`:pre-delete` / `:post-delete`) run
around the delete.

**Body:** JSON with `type` and `id`.

## Action hooks

### `POST /api/actions`

Execute an action hook on a `:button` field (update form only).

**Body:**
```json
{"type": "models", "id": "<record-uuid>", "field": "deploy"}
```

Returns sync result (`complete` / `failed`) or async acceptance.

## File handling

### `POST /api/upload`

File upload via `multipart/form-data`. Returns a `file-token` used in
the subsequent `/api/insert` call.

### `GET /api/file`

Serve an uploaded file (with token auth).

**Parameters:** `token` (file token).

## Validation

### `POST /api/validate-field`

Per-field validation against the model's validation hooks.

### `POST /api/validate-form`

Full-form validation (all fields at once).

## Schema and metadata

### `GET /api/types`

Available types, grouped by category (`:user`, `:settings`, `:system`).

### `GET /api/info`

Schema metadata, landing page resolution (per-user via
`be-landing-page`).

### `GET /api/public-info`

Public schema info (no auth required).

### `GET /api/css-variables`

CSS variable values from settings types (for theme support).

### `GET /api/users`

User list for the current session.

## Authentication

### `POST /api/login`

Authenticate with username/password. Returns JWT access token (1-hour
expiry) and refresh token (7-day expiry).

### `POST /api/refresh`

Exchange a refresh token for a new access token.

## Health

### `GET /health`

Health check (no auth required).
