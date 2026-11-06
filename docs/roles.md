# Type Roles and Row Roles

Two different resources, answering two different questions. Agents keep collapsing them into one. Read this before reasoning about who can see a tab, who can create a row, or who can read one.

## Why `:type-roles` exists

Access to an existing row flows from that row's own resource. A row that does not exist yet has no resource, so there is nothing to check. `:type-roles` is the stand-in for that case: the roles on the *type* resource (`type-<type>`, `type-resource-name`), consulted by `be-insert` through `valid-user-permissions` on `"create"`. That is the question the key was added to answer.

An omitted declaration compiles to `("admin")`. `"admin"` is always appended, whatever the model declares. A string entry such as `"page-editors"` is created with full CRUD; the `(role-name perm…)` form narrows that. Built-in types have their own defaults (`:users` gets `"logged-in"` and `"user-creator"`, and so on) — those are not what an author type gets. Full vocabulary: `docs/model-reference.md` → Type roles.

## What a row carries

The caller decides, not the type. `insert-normal` attaches exactly the `roles` argument it was passed, plus the creator's exclusive role (`<user>:exclusive`). `update-roles` force-adds `"admin"`. Nothing in that path reads `:type-roles`.

- Omit `roles` and the row is private to its creator and admin.
- Name `roles` and each one must already belong to the caller (admin may attach any role; another user's shareable exclusive role may also be attached). A request naming a role the caller does not hold is rejected.
- The two lists are independent from the first moment. A later edit of the row's roles does not change who may create a row of the type, and tightening `:type-roles` does not touch roles already on rows.

The built-in UI pre-checks the type's roles on the Add form — it reads the `type-roles` array of the `/api/list` response and fills the checkboxes — and the user may uncheck any of them before submit. That pre-check is a convenience. It is not a copy, and it is not what the row ends up with unless the user leaves the boxes checked.

The API pre-selects nothing. A caller of `POST /api/insert` must name the roles explicitly. Full statement: `docs/rest.md` → `POST /api/insert`.

## What decides an existing row

Reads, updates, and deletes of an existing row go through `user-allowed-resource` on the row's own resource: `be-rec`, `be-id`, `be-update`, `be-delete`. `valid-user-permissions` is not on that path. Holding create on the type says nothing about whether you can read a row someone else created, and holding read on a row says nothing about whether you can create one.

## Tab visibility is a reuse

`be-types` emits a type only when the user holds read on the type resource, so the type selector happens to follow `:type-roles`. That is the same resource being consulted for a second question, not the reason the key exists. Do not treat "cannot see the tab" as "cannot read the row," or the reverse: a user can hold read on a row whose type they cannot see, and a user can see a tab and still be unable to read a particular row.

## REST entry points

The REST handlers used to add a third, contradictory check here: `require-auth` was called with `get-type-roles` before the backend ran, so a caller who could read a row but held none of the type's roles got a 401 and the row check never ran. That membership preflight is gone from the existing-row endpoints (`/api/list`, `/api/item`, `/api/id`, `/api/value-id`, `/api/value`, `/api/column`, `/api/update`, `/api/delete`, `/api/actions`, `/api/file`): they authenticate (a missing or invalid token is still a 401) and then authorize on the real resource — the row itself for item/value/update/delete/action/file, the readable-row set for lists.

What stays type-level: create (`/api/insert`, `/api/upload` — a row that does not exist yet has no resource), the pre-row validation endpoints (`/api/validate-field`, `/api/validate-form`), app-level metadata (`/api/types`, `/api/info`, `/api/css-variables`), and list access for rollups and built-in types — rollup rows have no resources, and built-in types keep their system contracts, so `be-list` gates those on the type resource. An ordinary author-defined type's list needs only an authenticated caller: the list filters to the rows that caller can read, so a `public` grant on one row of an admin-only type is reachable directly even though the type never appears in that caller's type selector. "Public" means the `public` role held by an authenticated user, never anonymous access.
