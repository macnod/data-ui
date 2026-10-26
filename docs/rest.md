# REST API Reference

All endpoints are defined in `lisp/rest.lisp` using Hunchentooth `define-easy-handler`. Authentication is JWT-based (access + refresh tokens via `/api/login` and `/api/refresh`). Most data endpoints require a valid access token in the `Authorization` header.

## Data retrieval

### `GET /api/list`

List records for a type. Returns records, form schemas (`list-form`, `add-form`, `update-form`, `view-form`), `allowed-values` for dropdown/checkbox fields, permission flags (`create`, `delete`, `update`), the type's RBAC role names (`type-roles` — see [`POST /api/insert`](#post-apiinsert)), and paging keys `total` and `sort`.

**Parameters:**

| Param | Required | Default | Meaning |
|-------|----------|---------|---------|
| `type` | yes | — | Type key, e.g. `todos`, `users` |
| `form` | no | `list-form` | `list-form` \| `update-form` \| `add-form` \| `view-form` |
| `filters` | no | — | JSON list of `[type field operator value]` rows; `in` / `not-in` take a non-empty list of the field's atom type (join-table fields may also be targeted, e.g. `["tags","name","in",["chores","errands"]]`; a target-keyed row names the FK target's type and identity column, e.g. `["users","name","in",["admin"]]` — the reference chip picker's emission) |
| `limit` | no | `20` | Max records; non-negative integer; clamped server-side to `200` |
| `offset` | no | `0` | Records to skip; non-negative integer |
| `sort` | no | — | `"field:asc"` / `"field:desc"`; direction defaults to `asc`; field must be `:sortable t` |
| `search` | no | — | Free-text term, ILIKE against `:searchable t` fields; trimmed; blank ignored; clamped to 200 chars |

**Filter operators** (lowercase in JSON): `eq`, `ne`, `gt`, `lt`, `gte`, `lte`, `like`, `ilike`, `not-like`, `not-ilike`, `in`, `not-in`, `has-all`. Filters may target a joined table's fields (the example above targets the `tags` joiner through the listed type). `has-all` is field-scoped instead — the row is `[listedType, m2mFieldKey, "has-all", values]` — and targets the listed type's M2M list field (the field must declare `:join-table`); values AND: the record must have all of them (`in` stays has-any). A reference-chip row is target-keyed — `[targetType, targetIdentityColumn, "in", values]`, e.g. `["users","name","in",["admin"]]` — comparing the target's identity display values directly against the joined target table (union within the row; unknown names match zero rows, never a 400).

**Response** (in addition to `records`, forms, `allowed-values`, permission flags): `total` (always present — pre-paging count of matching records; may exceed the number of returned records) and `sort` (the effective sort: `{"field": "...", "dir": "asc"|"desc"}`). When no sort was requested, `sort` reports the type's `:default-sort` declaration when one exists, else the rollup default policy (first sortable measure, `desc`) on a rollup type, else `null` on a base type.

**Per-record flags:** every record object carries `update` / `delete` JSON booleans computed from the caller's permissions on that record's own resource (row-level — the same gate as [`POST /api/update`](#post-apiupdate) / [`POST /api/delete`](#post-apidelete); a creator with `create` but no type-level `update` gets `update: true` on rows her exclusive role sits on). Rollup records carry both `false`. The envelope `create` / `update` / `delete` flags stay type-level (Add / Delete Selected gating, legacy consumers). `/api/column` is unaffected — it projects one field key per row and discards the record plists, so the flag keys never reach its output.

**400 errors:** non-integer `limit`/`offset`; unknown sort field, non-`:sortable` field, or bad direction; non-blank `search` on a type with zero `:searchable` fields.

**Rollup types:** `type` may name a read-only analytical (rollup) type — served from the same surface (see `docs/model-reference.md` → List queries and → Rollup types). Only list-family endpoints accept a rollup: `/api/list` and `/api/column` work; `/api/item`, `/api/id`, `/api/value`, `/api/value-id`, `/api/validate-*`, `/api/actions`, and `/api/upload` reject one. Rollup responses carry `create`/`update`/`delete` all `false`, and `total` is the grain-row count.

### `GET /api/item`

Fetch a single record by UUID. The `record` object carries the same per-record `update` / `delete` JSON booleans as `/api/list` (row-level, computed from the caller's permissions on this record's own resource).

**Parameters:** `type`, `id`; optional `form`, default `update-form`, accepts `view-form`.

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

Fetch all values for a column across a type. Unpaged / full-set: this endpoint ignores the paging parameters (it exists to feed dropdowns and autocomplete, not paged tables). Accepts rollup types.

**Parameters:** `type`, `field`; optional `form`, default `list-form`, accepts `view-form`.

## CRUD mutations

### `POST /api/insert`

Create a new record. Validation runs before the SQL insert. Lifecycle hooks (`:pre-create` / `:post-create`) run around the write.

**Body:** JSON with `type`, field values, and an optional `roles` array.

**Roles and row visibility.** Every row is an RBAC resource: other users can read or write it only through a role the row carries. Two roles are always attached and can never be removed: the creator's exclusive role (`<user>:exclusive`) and `admin`.

- `roles` omitted → **private by default**: the new row is visible only to its creator and admin.
- `roles` present: each named role must already belong to the caller (admin may attach any role; another user's shareable exclusive role may also be attached, for point-to-point sharing). Requests naming roles the caller does not hold are rejected.
- **Recommended:** pass the type's roles — what the built-in frontend does. It pre-fills the Add form's roles checkboxes from the `type-roles` array of the `/api/list` response and submits them with the insert, so new rows carry the ambient visibility the model author intended for the type. Omitting `roles` is only appropriate when rows should start private.

**Creating users.** `type: "users"` changes the meaning of `roles`: they become the new user's role *memberships* (not resource roles), and nothing is pre-selected server-side — the API caller is on their own. The built-in frontend pre-fills the Add form from the `type-roles` array of the `/api/list` response for `users`, which for that type is the union of `:users`' own declared roles and every user-defined type's roles; an API caller who wants the same memberships must pass them explicitly. Automatic regardless of what is passed: the new user's exclusive role (`<user>:exclusive`), `logged-in`, and `settings`. The hold-them-first rule still applies (admin may pass any role; `ai-user` / `deployer` are admin-only assignments).

### `POST /api/update`

Update an existing record. Validation runs before the SQL update. Lifecycle hooks (`:pre-update` / `:post-update`) run around the write. The caller needs `update` on the target row's own resource, not on the type (base types degenerate to the type resource; `tokens` is gated on the type resource).

**Body:** JSON with `type`, `id`, field values, and an optional `roles` array. Roles on the row may be grown or trimmed through this field, subject to the same hold-them-first rule as insert; the creator's exclusive role and `admin` are force re-added server-side and cannot be stripped.

### `POST /api/delete`

Delete a record. For filesystem-backed types, deletes files/directories recursively. Lifecycle hooks (`:pre-delete` / `:post-delete`) run around the delete.

**Body:** JSON with `type` and `id`.

## Action hooks

### `POST /api/actions`

Execute an action hook on a `:button` field (update form only). The caller needs `update` on the target row's own resource, not on the type (base types degenerate to the type resource).

**Body:** ```json {"type": "models", "id": "<record-uuid>", "field": "deploy"} ```

Optional `data` object: the string-valued form fields the user typed on the edit form (the frontend whitelists password/textbox/textarea/select/code values). Forwarded to the hook as its `:data` keyword; hooks that need typed input (e.g. the built-in `:change-password` on `:settings`) read it, other hooks ignore it. Example:

```json
{"type": "settings", "id": "<settings-uuid>", "field": "change-password",
 "data": {"current-password": "old-pass", "new-password": "new-pass-9"}}
```

Returns sync result (`complete` / `failed`) or async acceptance.

## File handling

### `POST /api/upload`

File upload via `multipart/form-data`. Returns a `file-token` used in the subsequent `/api/insert` call.

### `GET /api/file`

Serve an uploaded file (with token auth).

**Parameters:** `token` (file token).

## Validation

### `POST /api/validate-field`

Per-field validation against the model's validation hooks.

### `POST /api/validate-form`

Full-form validation (all fields at once).

## Schema and metadata

The three app-level endpoints in this section have no type whose roles they could consult, so they gate on the model's top-level `:api-roles` key (default `("logged-in")`). A request whose user holds none of the listed roles gets **401**. Type-gated endpoints (`/api/list`, `/api/item`, ...) are unaffected; see `docs/model-reference.md` → API roles.

### `GET /api/types`

Available types, each entry an object: `name` (string), `category` (`user` / `settings` / `system`), `user-setting` (boolean). Two optional fields ride the entry only when the model author declared them (`:page-description` / `:page-description-open`): `page-description` (markdown string, the list-page intro block) and `page-description-open` (boolean; an explicit `:page-description-open nil` serializes as `false`, never dropped). RBAC scoping is inherent — `/api/types` returns only types the caller may read, so a user without read on a type sees no entry, and no description.

### `GET /api/info`

Schema metadata, landing page resolution (per-user via `be-landing-page`). Includes the top-level settings (`:title`, `:name`, `:version`, `:domain`, `:domain-stg`, `:repl`, `:guest-allowed`, with `:landing-page` resolved per user).

### `GET /api/public-info`

Public schema info (no auth required). Returns the model `title` and a `guest-allowed` JSON boolean (whether `:guest-allowed t` is set — the login screen uses it to offer / auto-run guest sign-in).

### `GET /api/css-variables`

CSS variable values from settings types (for theme support). Always responds with a JSON object; a user with no readable settings row (e.g. guest) gets `{}`, which the frontend treats as the default light theme.

## Authentication

### `POST /api/login`

Authenticate with username/password. Returns JWT access token (1-hour expiry) and refresh token (7-day expiry).

### `POST /api/refresh`

Exchange a refresh token for a new access token.

## Health

### `GET /health`

Health check (no auth required).
