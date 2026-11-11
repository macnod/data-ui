# Type Roles and Row Roles

Two different resources, answering two different questions. Agents keep collapsing them into one. Read this before reasoning about who can see a tab, who can create a row, or who can read one.

## Which resource does this request check?

The one question that prevents nearly every confusion below: for the request at hand, which resource does the system consult — the *type* resource (`type-<type>`) or the *row's own* resource? Answer it first, then read the detail sections.

| Request | Resource consulted | Gate |
|---|---|---|
| Type appears in the type selector (`be-types`) | type resource | read |
| `POST /api/insert`, `POST /api/upload` (a row that does not exist yet) | type resource | create |
| `/api/validate-field`, `/api/validate-form` (pre-row validation) | type resource | type-level |
| `/api/item`, `/api/id`, `/api/value`, `/api/value-id` | the row's own resource | read |
| `/api/update`, `/api/actions`, `/api/file` | the row's own resource | update (file: read / delete per operation) |
| `/api/delete` | the row's own resource | delete |
| List of an ordinary author type (`/api/list`, `/api/column`) | neither — authenticated caller, rows filtered to the readable set | — |
| List of a built-in type or a rollup | type resource | read (built-ins then filter rows to the readable set; rollups have no row resources) |
| `/api/types`, `/api/info`, `/api/css-variables` | no resource — app-level | `:api-roles` membership |

Base types (`:users`, `:roles`, `:permissions`, `:resources`, `:settings`, the joiners) have no row resources — every check on them degenerates to their type resource, and `:tokens` (opaque built-in, no row resource) gates on its type resource the same way. Everything else — including the non-base built-ins `:secrets`, `:usage`, and `:styling` — gives each row its own resource, and that row resource is what existing-row requests consult.

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

## Getting a role onto rows

The type resource and the row's resource are independent from the row's first moment, so a role declared in `:type-roles` never flows down to a row by itself. Exactly three paths put a role onto a row's resource:

- **Add-form preselection — the default path, UI only.** For a non-base type without `:suppress-roles t`, the built-in Add form pre-checks the type's `:type-roles` among the Roles checkboxes (`selectable-roles`); whatever is left checked rides with the insert. A holder of the role who creates a row and touches nothing gets a row every other holder can read and edit. Unchecking only locks other holders out, and admin can always repair that. This is the system's only automatic bridge from a type's roles onto new rows, and `:suppress-roles t` switches it off entirely — no palette renders, for admin included, so a role-suppressed type's rows can never carry the type's roles through the UI.
- **Writer attach.** The type's writer attaches the role at insert time — the `:usage` precedent (`usage-insert` calls `update-roles` with `("admin" "usage" …)` on every row). Covers rows created after the change, forever; requires a code change in the writer.
- **Manual attach, once.** Admin edits the row's roles through the Edit form (the same palette, so still requires the type not be role-suppressed), or a REPL `a:add-resource-role` on the row's resource. The only path that reaches rows created before either of the above.

The symptom of skipping this step is the **empty-list trap**: a user holding a type role with no attach on the rows sees the tab (read on the type) and an empty list, and is denied on every existing row. The grant looks alive and does nothing. It is not a bug; it is the two lists being independent.

## What decides an existing row

Reads, updates, and deletes of an existing row go through `user-allowed-resource` on the row's own resource: `be-rec`, `be-id`, `be-update`, `be-delete`. `valid-user-permissions` is not on that path. Holding create on the type says nothing about whether you can read a row someone else created, and holding read on a row says nothing about whether you can create one.

## Where the admin user's roles come from

Every check above has two halves. `a:user-allowed` — the join under both `valid-user-permissions` (type resources) and `user-allowed-resource` (rows) — answers yes only when the user *holds* the role (`role_users`), the role is *attached to the resource* (`resource_roles`), and the role carries the permission. Holding a role alone does nothing; a role on a resource alone does nothing. Admin's reach is these two halves meeting.

The hold side is bootstrapped once, by `a:initialize-database` on a fresh database — `init-database` reaches it through `initialize-tables` when the admin user is absent, and `reset-database` after truncating; it short-circuits when any user exists. It creates the base permissions, the base roles (`admin` with full CRUD; `public` and `logged-in` with read only), and the two base users. Admin is created through `add-user … :roles *init-roles*`, and `add-user` appends `*default-user-roles*` (`public`, `logged-in`) for every user, links the user to each resulting role, and `set-exclusive-role` creates and links a full-CRUD `<user>:exclusive`. From birth, then, admin holds `admin`, `public`, `logged-in`, and `admin:exclusive`. Guest ends up holding `public` and `guest:exclusive`; the `logged-in` default is stripped immediately after creation.

The rest of the hold side arrives one role at a time: every role created through `a:add-role` is linked to admin as part of its creation — the `(link rbac "roles" "users" role *admin*)` tail, which is why init uses the internal `insert-role` instead (admin does not exist yet). Both of data-ui's role creators go through `add-role`: `ensure-model-roles` (`:type-roles` entries) and `ensure-declared-roles` (model `:new-roles`). Admin holds every role the running model has ever declared, without anyone granting them.

The attach side is the rest of this document: `add-type-roles` always appends `admin` to the type resource, `update-roles` force-adds `admin` to every row resource, and the library's own `add-resource` attaches `*default-resource-roles*` (`admin`) to every resource it creates. Held plus attached, with `admin` carrying full CRUD, is why admin passes every `user-allowed` in the system — `deploy-model-find-secret-id`'s `(be-list :secrets "admin" …)` is the production proof on a built-in, non-base type.

Do not claim that the user named `admin` does not hold a role without checking — a review during the styling-type work made exactly that claim about the role `admin` itself, and it did not survive the source. When a permission check fails for admin, suspect the attach side (is the role on the resource?) before the hold side.

## A worked example: a narrow role for a built-in type

The recurring ask, in the exact shape it arrives: let a trusted non-admin manage `:styling` (banner / footer) without admin powers. The base model now ships exactly this configuration — `:type-roles ("styling")`, no `:suppress-roles t` — so the recipe is live: grant `styling` and it works. The steps below are its anatomy, and what you would repeat for another role name or another built-in.

1. Declare the role on the type — either the base-model def or a per-model redeclaration (`:styling (:type-roles ("styler"))`, legal on any non-internal built-in; the author's list replaces the default). The string form creates the role with full CRUD, admin holds it automatically, and `type-styling` carries it. Grant it to the user (Users → Edit → the role name).
2. Stopping there fails in a specific, silent way: the holder sees the Styling tab, gets an empty list, and is denied on the existing rows — those rows' resources carry only `admin` and the creator's exclusive role. That is the empty-list trap, not a bug.
3. The finishing line is `:suppress-roles` off the type (a code change in the base def — suppression is not reachable from a model redeclaration). The Add form's Roles palette now preselects the type's role, the rows a holder creates carry it, and every holder can edit them. Declaration plus one line — both shipped for `:styling` under the role name `styling`.
4. Pre-existing rows are reached only by manual attach (REPL, once per row) or by recreating them through the now-unsuppressed form.
5. Aftermath: admin always retains every row (`update-roles` force-adds `admin`), and a holder can grant `styler` onward only to users they can edit — in practice nobody but admin.

## Recipes

- **Gate who may create rows of a type** → `:type-roles` alone. That is the question the key exists to answer.
- **Control tab visibility** → read permission on the type resource. A plist role without `"read"` (e.g. `("r" :create)`) hides the tab from holders while API inserts still work.
- **Let holders read or edit the rows** → the declaration plus one of the three attach paths above. There is no fourth path; without one, the empty-list trap.
- **Narrow a role's powers** → the plist form in `:type-roles` (`("r" :read :update :create)`), or `:new-roles` for a badge role that gates membership without touching any type.
- **Propagate a grant** → admin can grant any role to any user; a user editing a row can attach roles they hold to that row's resource; grants on `:users` rows are admin-only in practice.
- **Tighten a type on a live database** → `add-type-roles` is add-only; old grants survive a redeploy. Removal is explicit (`be-remove-type-roles`, admin edit) or a clean slate.

## Tab visibility is a reuse

`be-types` emits a type only when the user holds read on the type resource, so the type selector happens to follow `:type-roles`. That is the same resource being consulted for a second question, not the reason the key exists. Do not treat "cannot see the tab" as "cannot read the row," or the reverse: a user can hold read on a row whose type they cannot see, and a user can see a tab and still be unable to read a particular row.

A `:page t` type follows the same rule with nothing behind it: read on the *page's own* type resource is what puts the tab in `be-types` and what `be-list` checks — the content type's type resource and the content row's roles are never consulted (the rollup precedent; no row-level RBAC on content rows). `:type-roles` on a page type is who may read the projection; the author who wants a private page is an author who points no page at that row. Creating the prose itself is the ordinary rule: `:type-roles` on the *content* type gates `be-insert`, and the rows carry their own roles as usual.

## REST entry points

The REST handlers used to add a third, contradictory check here: `require-auth` was called with `get-type-roles` before the backend ran, so a caller who could read a row but held none of the type's roles got a 401 and the row check never ran. That membership preflight is gone from the existing-row endpoints (`/api/list`, `/api/item`, `/api/id`, `/api/value-id`, `/api/value`, `/api/column`, `/api/update`, `/api/delete`, `/api/actions`, `/api/file`): they authenticate (a missing or invalid token is still a 401) and then authorize on the real resource — the row itself for item/value/update/delete/action/file, the readable-row set for lists.

What stays type-level: create (`/api/insert`, `/api/upload` — a row that does not exist yet has no resource), the pre-row validation endpoints (`/api/validate-field`, `/api/validate-form`), app-level metadata (`/api/types`, `/api/info`, `/api/css-variables`), and list access for rollups and built-in types — rollup rows have no resources, and built-in types keep their system contracts, so `be-list` gates those on the type resource. An ordinary author-defined type's list needs only an authenticated caller: the list filters to the rows that caller can read, so a `public` grant on one row of an admin-only type is reachable directly even though the type never appears in that caller's type selector. "Public" means the `public` role held by an authenticated user; under `/api/*` it is never anonymous. Under `/p/` it is: a `public` grant on a row of an eligible type in a `:guest-allowed` model is anonymous read of that row's View-form projection — exactly what the guest token already reaches, no more.
