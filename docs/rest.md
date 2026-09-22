# REST API Reference

All endpoints are defined in `lisp/rest.lisp` using Hunchentooth
`define-easy-handler`. Authentication is JWT-based (access + refresh
tokens via `/api/login` and `/api/refresh`). Most data endpoints
require a valid access token in the `Authorization` header.

## Data retrieval

### `GET /api/list`

List records for a type. Returns records, form schemas (`list-form`,
`add-form`, `update-form`), `allowed-values` for dropdown/checkbox
fields, permission flags (`create`, `delete`, `update`), and paging
keys `total` and `sort`.

**Parameters:**

| Param | Required | Default | Meaning |
|-------|----------|---------|---------|
| `type` | yes | — | Type key, e.g. `todos`, `users` |
| `form` | no | `list-form` | `list-form` \| `update-form` \| `add-form` |
| `filters` | no | — | JSON list of `[type field operator value]` rows; `in` / `not-in` take a non-empty list of the field's atom type (join-table fields may also be targeted, e.g. `["tags","name","in",["chores","errands"]]`) |
| `limit` | no | `20` | Max records; non-negative integer; clamped server-side to `200` |
| `offset` | no | `0` | Records to skip; non-negative integer |
| `sort` | no | — | `"field:asc"` / `"field:desc"`; direction defaults to `asc`; field must be `:sortable t` |
| `search` | no | — | Free-text term, ILIKE against `:searchable t` fields; trimmed; blank ignored; clamped to 200 chars |

**Filter operators** (lowercase in JSON): `eq`, `ne`, `gt`, `lt`,
`gte`, `lte`, `like`, `ilike`, `not-like`, `not-ilike`, `in`,
`not-in`. Filters may target a joined table's fields (the example
above targets the `tags` joiner through the listed type).

**Response** (in addition to `records`, forms, `allowed-values`,
permission flags): `total` (always present — pre-paging count of
matching records; may exceed the number of returned records) and
`sort` (the effective sort: `{"field": "...", "dir": "asc"|"desc"}`).
When no sort was requested, `sort` reports the type's `:default-sort`
declaration when one exists, else the rollup default policy (first
sortable measure, `desc`) on a rollup type, else `null` on a base type.

**400 errors:** non-integer `limit`/`offset`; unknown sort field,
non-`:sortable` field, or bad direction; non-blank `search` on a type
with zero `:searchable` fields.

**Rollup types:** `type` may name a read-only analytical
(rollup) type — served from the same surface (see
`docs/model-reference.md` → List queries and → Rollup types). Only
list-family endpoints accept a rollup: `/api/list` and `/api/column`
work; `/api/item`, `/api/id`, `/api/value`, `/api/value-id`,
`/api/validate-*`, `/api/actions`, and `/api/upload` reject one.
Rollup responses carry `create`/`update`/`delete` all `false`, and
`total` is the grain-row count.

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

Fetch all values for a column across a type. Unpaged / full-set: this
endpoint ignores the paging parameters (it exists to feed dropdowns and
autocomplete, not paged tables). Accepts rollup types.

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

The three app-level endpoints in this section have no type whose roles they
could consult, so they gate on the model's top-level `:api-roles` key
(default `("logged-in")`). A request whose user holds none of the listed
roles gets **401**. Type-gated endpoints (`/api/list`, `/api/item`, ...)
are unaffected; see `docs/model-reference.md` → API roles.

### `GET /api/types`

Available types, grouped by category (`:user`, `:settings`, `:system`).

### `GET /api/info`

Schema metadata, landing page resolution (per-user via
`be-landing-page`). Includes the top-level settings (`:title`,
`:name`, `:version`, `:domain`, `:domain-stg`, `:repl`,
`:guest-allowed`, with `:landing-page` resolved per user).

### `GET /api/public-info`

Public schema info (no auth required). Returns the model `title` and a
`guest-allowed` JSON boolean (whether `:guest-allowed t` is set — the
login screen uses it to offer / auto-run guest sign-in).

### `GET /api/css-variables`

CSS variable values from settings types (for theme support). Always
responds with a JSON object; a user with no readable settings row (e.g.
guest) gets `{}`, which the frontend treats as the default light theme.

## Authentication

### `POST /api/login`

Authenticate with username/password. Returns JWT access token (1-hour
expiry) and refresh token (7-day expiry).

### `POST /api/refresh`

Exchange a refresh token for a new access token.

## Health

### `GET /health`

Health check (no auth required).
