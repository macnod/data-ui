# Model Syntax Reference

A Data UI **model** is a nested plist that describes an entire application:
types, fields, views, forms, RBAC roles, hooks, and deploy metadata. The
compiler expands the model into PostgreSQL schema, parameterized SQL, backend
functions, REST endpoints, and a schema-driven React UI.

Tagline: **"Your whole app, in an email."**

This document is the author-facing vocabulary. Companion docs:

- `docs/hook-registry.md`: validation, lifecycle, and action hook contracts
- `docs/model-accessors.md`: REPL/debug accessors and deploy metadata readers
- `docs/deployment.md`: how top-level keys drive `scripts/data-ui deploy`
- `AGENT.md`: architecture, MVP status, and agent workflow

Sources of truth: `lisp/model.lisp`, `lisp/backend.lisp`, `lisp/predicates.lisp`,
`lisp/database.lisp`, and the example models in `models/`.

---

## File form and loading

Each file under `models/` is a **bare quoted plist**: no `defparameter`, no
wrapping variable:

### File header (recommended)

Precede the model plist with a `;;` comment block that states what the
model is for and where it came from. Put it above the opening quote.
Include:

1. **Purpose**: what the app does (a few sentences is fine)
2. **Author**: who wrote it
3. **Created**: creation date
4. **Prompt**: if an AI produced the model, the prompt that was used

Example:

```lisp
;; Purpose: Shared to-do list with tags and point scoring.
;; Author: Donnie Cameron
;; Created: 2025-06-12
;; Prompt: (not AI-generated)
'(:title "To Do List"
  :name "todos"
  ...)
```

For an AI-authored model, keep the prompt verbatim so the model can be
regenerated or audited later:

```lisp
;; Purpose: Parts inventory with locations and reorder levels.
;; Author: Claude (via Data UI hosted tier)
;; Created: 2026-03-01
;; Prompt: Build a parts inventory app. Each part has a SKU, name,
;;   location, quantity on hand, and reorder level. Users should be
;;   able to filter by location.
'(:title "Parts Inventory"
  ...)
```

Load with:

```lisp
(set-model "todos")   ; file name only, no path, no .lisp
```

Conventions (`models/README.md`):

| File | Role |
|------|------|
| `models/<name>.lisp` | Named application model |
| `models/local/<name>.lisp` | Locally deployed (VIP) model; gitignored, shadows same-named test fixture |
| `models/test/<name>.lisp` | Test fixture; do not change unless changing tests |

`(set-model "<name>")` tries `models/<name>.lisp` first, then
`models/local/<name>.lisp`, then falls back to `models/test/<name>.lisp`.

Because the form starts with a quote, Lisp can `read` it. That is useful for
paren-balance checks; it is not how the compiler is invoked.

---

## Top-level keys

Recognized keys: `*top-level-keys*` =
`(:title :name :version :domain :domain-stg :repl :guest-allowed
:guest-auto :api-roles :landing-page :new-roles)`.

`:types` is required alongside those settings but is handled separately by
`compile-model`. Any other root key is ignored by `top-level-settings`.

| Key | Required | Value | Consumed by |
|-----|----------|-------|-------------|
| `:title` | yes | string (display title; restricted charset) | page title, deploy |
| `:name` | yes | string `^[a-z][-a-z0-9]*`; `profile` and `profile-*` are reserved | deploy tag/namespace `dataui-<name>` |
| `:version` | yes | string (semver-ish) | image tag |
| `:domain` | yes | FQDN-like string | HAProxy map, TLS host |
| `:domain-stg` | no (default: `-stg` suffixed onto the first DNS label of `:domain`) | FQDN-like string | staging exposure (`profile expose`); must differ from `:domain`; requires `:domain` when written explicitly |
| `:repl` | no (default `nil`) | boolean | Swank port iff `t`; **nil in production** |
| `:guest-allowed` | no (default `nil`) | boolean | passwordless guest login via `/api/login`; see [Guest login](#guest-login) |
| `:guest-auto` | no (default `t`) | boolean | whether the frontend may auto-login as guest without showing the login page (only meaningful with `:guest-allowed t`; `nil` keeps the login page as the first screen). Served as a JSON boolean by `/api/public-info` |
| `:api-roles` | no (default `("logged-in")`) | non-empty list of role-name strings, no duplicates | app-level REST endpoints; see [API roles](#api-roles) |
| `:landing-page` | no | type keyword present in `:types`, or nil | `/api/info` via `be-landing-page`; falls back to first non-base type the user can access |
| `:new-roles` | no | plist: role-name keyword → non-empty list of permission strings | `ensure-declared-roles` at `set-model` time |
| `:types` | yes | plist of type-key → type-def | compiler |

`:name` values `profile` and `profile-*` are reserved for host-profile
HAProxy backends (`scripts/data-ui profile expose`); models using them
fail compilation. Similar words (e.g. `profiles`) are fine.

Minimal skeleton:

```lisp
'(:title "To Do List"
  :name "todos"
  :version "0.1"
  :domain "todo.demo.data-ui.com"
  :domain-stg "todo-stg.demo.data-ui.com"
  :repl t
  :guest-allowed t
  :landing-page :todos
  :types
  (:todos
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("todo-users")
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "To Do" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))))
```

---

## Type definitions

Under `:types`, each entry is `type-key` → plist.

### Author-facing type keys

| Key | Meaning | Notes |
|-----|---------|-------|
| `:table` | Type is backed by a DB table | Always `t` in real models |
| `:create` | Insert strategy | `:auto` \| `nil` \| function (base-model only). Author models use `:auto` |
| `:update` | Update strategy | `:auto` \| `nil`. Exposed to FE as boolean |
| `:delete` | Delete strategy | `:auto` \| `nil` \| function. `:auto` + path field → FS delete |
| `:display` | Show in type selector | `t`/`nil`. Also requires not `:internal` and read permission |
| `:type-roles` | RBAC roles for the type | list of role-name strings, or `(role-name perm…)` forms. Missing roles are created. `"admin"` is always added |
| `:default-sort` | Sort used when a list request sends none | `(:field :asc\|:desc)`; direction optional, defaults `:asc` (**always write the direction**: omitted means `:asc` even on measures, where the automatic policy is `:desc`). Field must exist and be `:sortable t` (compile error otherwise). Absent = current behavior (id-ASC on base types, first-sortable-measure `:desc` on rollups). An explicit request sort always wins. Applies to every `be-list` read of the type with no request sort, including `allowed-values` FK option lists targeting it |
| `:views` | Named join views | plist; see [Views](#views). Default if omitted (non-joiner): `(:main (:tables (<type-key>)))` |
| `:fields` | Field definitions | plist; see [Fields](#fields) |
| `:list-form` / `:add-form` / `:update-form` | FE form specs | `(:fields t)` or `(:fields (:a :b))`; absent/nil → no form |
| `:tree` | Tree-structured type | requires `:parent-type` |
| `:is-leaf` | Leaf vs directory | requires `:tree t` |
| `:parent-type` | Parent type-key | existing non-base type; requires `:tree t` |
| `:fs-backed` | Filesystem-backed paths | requires `:tree t` |
| `:user-setting` | Per-user settings type | auto-sets `:suppress-roles t`; category `:settings` if `:category` omitted. One row per user; lifecycle creates/deletes that row with the user |
| `:suppress-roles` | Hide injected `:roles` field | auto when `:user-setting t`; can set alone |
| `:category` | Type-selector group (author key) | `:user` \| `:settings` \| `:system`. Put a type under the Settings tab with `:category :settings`. Derived if omitted |
| `:base` | RBAC/base type (no `resources` row) | normally only in `*base-model*` |
| `:built-in` | Built-in table name (no `rt_` prefix) | base-model / secrets |
| `:internal` | Hidden from public BE API | default = `:is-joiner`; joiners are internal |
| `:is-joiner` | M2M join table | fields are `:reference` pairs; no public CRUD SQL |
| `:rollup` | Read-only analytical type (no table) | see [Rollup types](#rollup-types-read-only-analytical); with `:grain` and type-level `:filter` |
| Lifecycle slots | Custom logic | `:pre-create` `:post-create` `:pre-update` `:post-update` `:pre-delete` `:post-delete`; see [Hooks](#hooks) |

### CRUD strategy values

For author models, use `:auto` or `nil`:

- `:auto`: generated SQL path (`insert-normal` / update / `remove-resource`, with FS branches when applicable)
- `nil`: operation disabled
- raw functions: internal base-model escape hatch only; prefer lifecycle hooks

### Type roles

```lisp
:type-roles ("todo-users")
;; or with explicit permissions:
:type-roles (("models-user" :create :read :update :delete))
```

String form gets full CRUD permissions when the role is created. Every type that
participates in RBAC also receives `"admin"`.

#### Default type-roles for built-in types

| Type | Default `:type-roles` | Rationale |
|------|----------------------|-----------|
| `:users` | `("logged-in" "user-creator")` | All authenticated users can read; creators get full CRUD |
| `:permissions` | `("logged-in" "permission-creator")` | Same pattern |
| `:roles` | `("logged-in" "role-creator")` | Same pattern |
| `:settings` | `("settings")` | Gated behind the `settings` role |
| `:secrets` | `("settings")` | Same as settings |
| `:resources` | *(none; internal, no CRUD)* | |
| `:role-permissions` / `:resource-roles` / `:role-users` | *(none; internal joiners)* | |
| `:tokens` | `("admin")` via the no-declaration default | `:display nil` (hidden from the selector) but **not** internal, so it still gets a `type-tokens` resource — and is overridable |

Types with no explicit `:type-roles` default to `("admin")`. The `"admin"`
role is always appended by `add-type-roles` regardless of what the model
declares.

#### Overriding defaults on built-in types

A model may redeclare a non-internal built-in type with **only**
`:type-roles`; the author's list **replaces** the default above (it is not
unioned). The redeclaration is a partial overlay — fields, views, and RBAC
CRUD functions come from the base model, so you never restate them:

```lisp
:types
(:items (...)                             ; ordinary author types
 :users (:type-roles ("admin"))           ; hide Users from non-admins
 :roles (:type-roles ("admin"))
 :permissions (:type-roles ("admin")))
```

Rules (all compile errors via `report-e`):

- `:type-roles` is the only legal key on a redeclaration ("you may retarget
  who sees a built-in type; you may not reshape it"). `:display` included —
  visibility is `:type-roles`, and hiding the tab would not close the API.
- Internal base types (`:resources`, the three joiners) cannot be
  overridden. `:tokens`, `:settings`, and `:secrets` can.
- The value must be a non-empty list (same shapes as any `:type-roles`);
  `nil`, `()`, a bare string, or a redeclaration without `:type-roles` is
  rejected.
- `"admin"` is still always granted by `add-type-roles`.

Tightening roles on an **existing** database does not revoke old grants —
`add-type-roles` only inserts missing resources. Role tightening needs a
clean slate (`reset-database` / fresh deploy); new deploys are correct on
first boot.

Known gap: any type with a `:target :users` field becomes unlistable for
non-admins until `allowed-values` is hardened (the FK palette read
re-enters `be-list :users` as the acting user). Restricting `:users` is
not yet safe on models like Model Bank / chores / books.

### Declared roles (`:new-roles`)

```lisp
:new-roles (:ai-user ("read")
            :bank-admin ("create" "read" "update" "delete"))
```

Optional top-level key declaring additional roles the model needs. Keys are
role-name keywords (downcased to the rbac string, `:ai-user` → `"ai-user"`);
values are non-empty lists of permission **strings** from the closed set
`"create" "read" "update" "delete"` — keywords (`(:read)`) are a compile
error.

At `set-model` time the compiler creates each declared role that does not
yet exist, granting exactly the declared permissions. Roles that already
exist are **never modified** — a role an admin has since edited keeps its
edits, and redeploying or re-running `set-model` is idempotent. New roles
are auto-assigned to admin (rbac `add-role` behavior), so they are
immediately usable and appear in the Roles UI / assignment palettes.

A declared role does not have to appear in any `:type-roles` — badge roles
(e.g. `:ai-user` for the model-generator button, which gates on membership,
not permissions) are declared exactly so they exist with known permissions.
When a declared role is *also* named in a `:type-roles` string list, the
declared permission list wins over the full-CRUD default that
`:type-roles` would otherwise grant.

Reserved role names (`"admin"`, `"settings"`, `"logged-in"`, `"public"`,
`"guest"`, `"user-creator"`, `"role-creator"`, `"permission-creator"`,
anything ending `:exclusive` or prefixed `admin:` / `guest:`) are compile
errors: they already exist or carry rbac semantics of their own.

### Guest login (`:guest-allowed`)

```lisp
:guest-allowed t
```

Optional boolean, default `nil`. When `t`, `POST /api/login` accepts the
seeded `guest` user with **any password** (including blank): the login
handler short-circuits password verification and issues normal access and
refresh tokens for the guest user id.

- The model is the switch: `t` opens the door even if an admin later changes
  the guest password, and removing the key (or setting `nil`) closes it
  again — guest login then requires the actual password.
- Guest stays read-only: it carries only the seeded `public` and
  `guest:exclusive` roles, and roles are resolved from the database per
  request, not baked into the token.
- Login is not access. Guest still sees only types whose resources carry a
  guest-readable role — resources created by `add-type-roles` are
  admin-plus-declared-roles only, so without `:type-roles` (or admin
  role-editing) that names a guest role, guest sees an empty type selector.
  Combine with [`:api-roles`](#api-roles) (so guest can even reach
  `/api/types`) and `"public"` in a type's `:type-roles` (so guest can read
  that type) for a browse-only guest demo. Row visibility is still
  per-record: rows created via the UI copy the type's roles, so new rows
  carry `public` too, but pre-existing rows need their resources edited.
- The flag flows to the frontend through `/api/public-info` (no auth) as
  `guest-allowed` (JSON boolean), so the login screen can offer — and the
  app auto-run — a guest sign-in without further backend support.

### API roles (`:api-roles`)

```lisp
:api-roles ("logged-in" "public")
```

Optional list of role-name strings, default `("logged-in")` (via
`model-api-roles`; the default lives in the accessor, the model plist stays
verbatim). Names the roles required by the **app-level** REST endpoints
that have no type to consult: `/api/types`, `/api/info`, and
`/api/css-variables`. A request whose user holds none of the roles gets
**401**.

- Shape validation only: non-empty list of non-empty, unique strings. Role
  *existence* is not checked at `set-model` time (`:new-roles` /
  `:type-roles` roles are created after top-level validation runs, so an
  existence check would reject first loads). `"logged-in"` and `"public"`
  always exist after database initialization.
- Type-gated endpoints (`/api/list`, `/api/item`, `/api/insert`, ...) are
  untouched — they read the type's own roles (`get-type-roles`).
- `/api/login`, `/api/refresh`, and `/api/public-info` stay
  unauthenticated / token-only.
- Adding `"public"` here does not open any type or row: it only lets the
  seeded guest user past the app-level gates. Pair it with `"public"` in
  `:type-roles` (see [Guest login](#guest-login)).

### Category

`:category` is an author-facing key, not reserved. Valid values:
`:user`, `:settings`, `:system`.

- `:settings`: type appears under the frontend Settings tab (e.g. `:secrets`)
- `:system`: Admin / system group
- `:user`: main app type selector

If `:category` is omitted, it is derived:

| Condition | Category |
|-----------|----------|
| `:user-setting t` | `:settings` |
| `:built-in t` | `:system` |
| else | `:user` |

`:user-setting t` is a stronger flag (one row per user + lifecycle hooks +
`:suppress-roles`). Multi-record types that only need the Settings tab should
set `:category :settings` without `:user-setting`.

`/api/types` returns `:category` so the frontend can group the type selector.

---

## Rollup types (read-only analytical)

A `:rollup t` type is a read-only analytical view: **no physical table, no
DDL/DML, no CRUD**. Rows are one per grain record, with measures aggregated
in SQL (`GROUP BY` + `SUM`/`COUNT`/`AVG`), not collapsed in Lisp.

```lisp
:user-leaderboard
(:rollup t
  :grain :users                          ; one row per user
  :type-roles ("leaderboard-viewers")    ; the only type-level gate
  :filter ((:chores :completed :eq t))   ; only completed chores count
  :views (:main (:tables (:users :chores)))
  :list-form (:fields t)
  :fields
  ((:name (:source (:view :main :table :users :column :name :agg :first)
             :ui (:label "User")))
   (:total-points (:type :integer
                   :source (:view :main :table :chores :column :points :agg :sum)
                   :ui (:label "Points")))
   (:chores-done (:type :integer
                  :source (:view :main :table :chores :column :id :agg :count)
                  :ui (:label "Completed")))))
```

Note the `:fields` form on a rollup is a **list of `(field-key def)` pairs**
(not the plist used by table types).

### Type keys

| Key | Meaning | Notes |
|-----|---------|-------|
| `:rollup` | Declares the type a rollup | `t` is the sole declaration; `:type :rollup` / `:table nil` are compile errors |
| `:grain` | The grain type-key | must be a type with a physical table; must appear in `:views :main :tables` and be **first** (it is the `FROM` anchor) |
| `:filter` | Model-declared row predicates | list of 4-tuples; see below. Rollup-only; absent is legal, `nil`/`()` are not |
| `:list-form` | Required | `(:fields t)` or a non-empty field list; missing / nil / `(:fields nil)` is a compile error |

Compiler-injected (not author-set): `:phase-a-shape :measure` (every other type
compiles with `:phase-a-shape :base`), `:suppress-roles t`,
`:display t` (default; explicit `nil` honored), category `:user` (via the normal
derivation), and a single injected `:id` field: a grain pass-through
(`users.id`, `:agg :first`), not a column. No `:created-at` / `:updated-at`
are injected, and the compiled type has **no `:table-name`**: a rollup has
no physical table.

### Single fact table

A rollup has exactly one fact table F. Every real measure (`:sum` / `:count` /
`:avg` / `:list` / `:distinct`) reads from F; `:tables` is exactly the path
grain → … → F. Compile errors (all `report-e`):

- grain-only rollup (no non-`:first` measure)
- a real measure reading from the grain
- `:agg :first` on a non-grain table (`:first` is grain-only)
- measures on two different fact tables (mixed-depth)
- a table in `:tables` but not on the path grain → F (extra arm / star)
- F not last in `:tables` (a hop past F)
- no xref path from grain to F (name intermediate hops explicitly)

### Field `:type` vs `:agg`

The default-to-`:text` rule stands (no inference). Validate against the
grain / source column:

| `:agg` | Required `:type` |
|--------|-------------------|
| `:first` | equals the grain field's `:type` (text pass-throughs may omit) |
| `:count` | `:integer` only |
| `:avg` | `:real` only |
| `:sum` | same numeric type as the source column (source must be numeric) |
| `:list` / `:distinct` | same type as the source column |

### Model-declared `:filter`

Always a list of 4-tuples, `(table column op value)`, even for one clause.
No singular form, no 3-ary, no sugar. The operator set is separate from the
request-time one (`:like` / `:in` never appear here):

| Family | Operators | Rules |
|--------|-----------|-------|
| Discrete (boolean / text / integer / real / uuid) | `:eq`, `:ne` | value is a real literal matching the column type (any number for real columns); booleans are Lisp `t`/`nil`; password columns count as text |
| Rolling date (timestamp) | `:last-days` | integer `1..1825`; `ratings.created_at >= NOW() - INTERVAL 'n days'` |
| Calendar date (timestamp) | `:calendar` | value `:month` only (the value slot is the extension point) |

`:eq` / `:ne` on a date/timestamp column is a compile error. The referenced
table must be in `:views :main :tables` and the column must exist on it.
Grain-table clauses are not orphans (they become `WHERE`); non-grain clauses
on the path grain → F bind into every real measure (`FILTER`), never a global
`WHERE`; that would drop zero-fact grain rows.

### Incompatible keys

`:table`, type-level `:type`, `:tree` / `:fs-backed` / `:is-leaf` /
`:parent-type`, lifecycle slots, `:write-to`, `:create` / `:update` /
`:delete`, `:add-form` / `:update-form` (even nil), extra views beyond
`:main`, author `:id`, `:column t`, `:button` fields, `:suppress-roles nil`,
type-level `:user-setting` / `:is-joiner` / `:built-in`, and on fields:
`:action`, `:validations`, `:compose-string`, `:compose`, `:autofill`,
`:source-all`, `:write-to`, `:join-table`, `:target`, `:identity t`.
`:grain` or `:filter` on a non-rollup type is also a compile error.

### Runtime behavior

Rollups are fully readable. `be-list` / `GET /api/list` on a rollup runs
**measure Phase A**: one `GROUP BY` grain query with aggregates, no
per-record RBAC resources, no Phase B hydration. The same paging / sort /
`filters` request surface and the same `:total` / `:sort` response shape
apply (see [List queries](#list-queries-paging-sort-search)).

- **SQL shape.** Grain-table `:filter` clauses become `WHERE`; every
  non-grain clause binds into each real measure as `FILTER (WHERE …)` —
  never a global `WHERE` on a joined table, which would drop zero-fact
  grain rows.
- **Zero-fact grain rows are retained.** `:sum` → 0, `:count` → 0,
  `:avg` → SQL NULL (JSON `null`; frontend renders "N/A"),
  `:list` / `:distinct` → empty.
- **`:total`** is the live grain-row count (users in a users-grain board),
  never the fact count.
- **Sort** runs on the measure's SELECT alias with `NULLS LAST` on `:avg`
  (zero-fact rows trail); every measure ORDER BY appends the grain
  primary key ASC as a stable-paging tiebreaker. The **default sort**
  (request sent none, no `:default-sort` declared) is the first sortable
  non-pass-through measure, descending; it is what the `:sort` echo
  reports. A declared `:default-sort` overrides this policy.
- **Request-time `filters` on a rollup** accept grain-column pass-throughs
  only: a field whose table is the listed rollup type with `:agg :first`,
  or the injected `:id`. Filters on fact tables, intermediate tables, or
  measure fields (`HAVING`) → 400. A bare UUID is rewritten to a grain-id
  filter — a one-row fetch is `be-list` with a grain-id `filters` value.
- **`search` is rejected** on rollups (no `:searchable` fields exist on a
  rollup).
- **View-level `:scope :user`** on `:main` is honored at runtime on the
  grain table (grain `:users` → `users.id = <you>`; otherwise the grain's
  `:user` column).
- **Endpoint restrictions.** Only list-family reads work on a rollup:
  `GET /api/list` and `GET /api/column` are allowed; `/api/item`,
  `/api/id`, `/api/value`, `/api/value-id`, `/api/validate-*`,
  `/api/actions`, and `/api/upload` reject it (400), and `be-insert` /
  `be-update` / `be-delete` on a rollup signal a validation error. The
  response carries `:create` / `:update` / `:delete` all `false`, so the
  frontend renders a read-only board (no Add / Edit / Delete controls).
- **Timezone stance.** `:last-days` / `:calendar` windows compare against
  the server's local clock. Timezone-aware windows are a known limitation
  and are post-MVP.

---

## Views

```lisp
:views (:main (:tables (:models :images :ratings))
         :tags (:tables (:tags))
         :scoped (:tables (:images :users) :scope :user))
```

| Key | Meaning |
|-----|---------|
| `:tables` | Ordered list of type-keys to JOIN (linear/star graphs only; **diamond joins unsupported**) |
| `:scope` | View-level row filter. Runtime implements bare `:user` only |

### View-level `:scope :user`

`be-list` adds a filter so only rows owned by the current user are returned
(via the users join / user id). Distinct from [field-level scope](#field-level-scope).

A plist form of scope is accepted by validation (`valid-view-scope`) but **not**
implemented at runtime; use the keyword `:user`.

Compiler injects per view (not author-set): `:sql`, `:aliases`, `:columns`,
normalized `:scope`.

Join emission is deterministic: **field declaration order within a type does
not affect the generated view SQL.** The join walk skips redundant FK edges
whose both endpoints are already joined and never re-joins a table that is
already in the view (each table joins exactly once, through one edge).
**Diamond join graphs remain unsupported.**

Joiners get no views.

---

## List queries: paging, sort, search

Every list read — `be-list` from Lisp, `GET /api/list` over HTTP — accepts the
same paging / sort / search parameters and returns the same response shape
regardless of whether the type is a normal table type or a
[rollup](#rollup-types-read-only-analytical). The caller does not choose an
execution path; the compiled type declaration does.

### `be-list` keyword arguments

| Argument | Default | Meaning |
|----------|---------|---------|
| `:limit` | `20` | Max records to return. Server clamps any larger value to `200`. `nil` means **all rows** (no LIMIT clause) |
| `:offset` | `0` | Records to skip |
| `:sort` | `nil` | `(:field :asc)` or `(:field :desc)`; direction optional (defaults `:asc`); `nil` = no requested sort |
| `:search` | `nil` | Free-text string, ILIKE-matched against the type's `:searchable t` fields |
| `:form` | `list-form` | Form key to render (`:list-form` / `:update-form` / `:add-form`) |
| `:filters` | `nil` | List of `(type-key field-key operator value)` 4-tuples (see below); a bare UUID string is also accepted (single-record fetch) |

**Filter operators** (`operator-sql`): `:eq :ne :gt :lt :gte :lte :like
:ilike :not-like :not-ilike :in :not-in`. `:in` / `:not-in` take a
non-empty list of the field's atom type. A filter row may target a joined
table's field (including a join-table field); that pulls the join into
Phase A (`SELECT DISTINCT id … JOIN …`).

```lisp
(be-list :todos "admin" :limit 20 :offset 40 :sort '(:name :desc)
         :filters '((:todos :done :eq :false)))
```

### `/api/list` query parameters

| Param | Default | Meaning |
|-------|---------|---------|
| `limit` | `20` | Non-negative integer; clamped server-side to `200` |
| `offset` | `0` | Non-negative integer |
| `sort` | — | `"field:asc"` / `"field:desc"`; direction optional (defaults `asc`). Unknown field, non-`:sortable` field, or bad direction → **400** |
| `search` | — | Trimmed; blank/whitespace-only ignored; silently clamped to 200 characters |

### Response: `:total` and `:sort`

Every successful `be-list` / `/api/list` response carries:

- `:total` — **always present**, the pre-paging count of matching records
  (RBAC + scope + filters + search applied, no LIMIT/OFFSET). May exceed the
  number of returned records.
- `:sort` — the **effective** sort as `{"field": "...", "dir": "asc"|"desc"}`
  (JSON), or JSON `null`:
  - the requested sort, when one was sent;
  - the type's `:default-sort` declaration, when none was sent and the
    type declares one;
  - otherwise the **default sort** on a rollup (first sortable
    non-pass-through measure, `desc`);
  - JSON `null` when none was sent, the type declares no `:default-sort`,
    and it is a base (non-rollup) type;
  - the primary-key tiebreaker that stabilizes paging is **never** echoed.

### `:sort` default and the echo

A type may declare `:default-sort` (see the type-key table above). When a
request sends no sort, that declaration applies on both base and rollup
types; an explicit request sort always wins. The `:sort` echo always
reports the **effective** sort (request, declaration, or rollup policy),
never the tiebreaker, so the frontend paints the applied order without
asking. The declaration also orders every `allowed-values` FK option list
targeting the type, since `be-list` drives those.

### Ordering of operations

RBAC read permissions → view scope → request filters + search → sort →
page. Filtering always runs on the id-selection query, never on the
collapsed result; paging is the last step.

### Phase A / Phase B (normal types)

List reads on ordinary table types run in two phases:

- **Phase A** selects the page of primary ids with SQL `WHERE` / `ORDER BY`
  / `LIMIT` / `OFFSET` — fast and indexed. Joins appear in Phase A **only
  when filters reference joined tables**, in which case it becomes
  `SELECT DISTINCT id FROM … JOIN …`.
- **Phase B** hydrates only those ids (the full join view, `WHERE id IN
  (…)`) and collapses join fan-out into one plist per record in Lisp
  (`:agg :first`, `:list`, `:distinct`…).

**Collapse is not paging.** Collapse reduces fan-out rows to one record per
id; it is a display-shaping step, not a result-limiting step. Paging happens
entirely in Phase A; the count query runs the same predicates without
ORDER BY / LIMIT / OFFSET.

On [rollup types](#rollup-types-read-only-analytical), the same request
surface is served by measure Phase A (`GROUP BY` grain) instead — same
`be-list` contract, no Phase B.

### Errors

- Sort on unknown field, non-`:sortable` field, or invalid direction → 400
- Non-blank `search` on a type with zero `:searchable` fields → 400
  (rollups compile to zero searchable fields, so they reject `search`)
- Non-integer `limit` / `offset` → 400

REST-specific details (auth, error envelope) live in `docs/rest.md` →
`GET /api/list`.

---

## Fields

Under `:fields`, each entry is `field-key` → plist (except joiner
`:reference` entries; see [Join tables](#join-tables-m2m)).

### Core field keys

| Key | Meaning |
|-----|---------|
| `:type` | Field type keyword (see below). Compile default: `:text` |
| `:ui` | plist passed through to the frontend. **Required** for a column to participate in insert/update SQL and for `fe-fields` emission (needs `:widget`) |
| `:column` | `t` → physical DB column. Forced `t` if `:target`; forced `nil` if `:type :button` |
| `:default` | Default value. Booleans: `:true`/`:false`. Timestamps: `:now`. UUID: `:generate-uuid` |
| `:not-null` | DDL `NOT NULL`. Optional on `:target` fields (see [Foreign keys](#foreign-keys-target)) |
| `:unique` | DDL `UNIQUE` |
| `:identity` | Natural-key participant. See [Identity fields](#identity-fields) |
| `:compose` | Format string for server-side field composition. See [`:compose`](#compose) |
| `:required` | Legacy: if truthy, compiler prepends `#'v-required`. Prefer `:validations (:required)` |
| `:validations` | list of validation hook forms |
| `:source` | how to read the field from a view |
| `:source-all` | options source for select / checkbox-list (`allowed-values`) |
| `:join-table` | M2M joiner type-key; field is a virtual list |
| `:target` | FK to another type (UUID + `ON DELETE CASCADE`). Display value resolved via target identity |
| `:write-to` | write-through spec (see [Write-through](#write-through)) |
| `:autofill` | currently only `:user` → current username at write time |
| `:force-sql-name` | override generated column name string (e.g. `"rating_user"`) |
| `:path` | marks the FS path field on fs-backed types (at most one per type) |
| `:action` | **only** on `:type :button`; single action hook form |
| `:sortable` | `t` → field is eligible for `ORDER BY` in list queries (clickable header). Base (non-rollup) types: only valid on `:column t` fields; compiler emits a sort index when no covering index exists. Rollup types only: the `:column t` rule is relaxed — pass-throughs and `:sum` / `:count` / `:avg` measures are legal; `:list` / `:distinct` measures are a compile error. On a hybrid (regular type with aggregated fields) `:sortable t` on a Phase B measure stays a compile error |
| `:searchable` | `t` → column is included in free-text `:search` (ILIKE OR-group in Phase A). Only valid on `:type :text` base columns without `:target`. Independent of `:sortable`. Distinct from type-level `:search-sql` (write-through identity lookup). Serialized as a JSON boolean (`true`/`false`, never `[]`). See [List queries](#list-queries-paging-sort-search) |
| `:default-from` | `:user` → copy username when creating user-setting rows |
| `:css-value` | `t` → included in CSS-vars API (e.g. settings `:dark-mode`) |
| `:primary-key` | DDL primary key (injected on `:id`) |

### Field types (`*field-types*`)

| `:type` | General | SQL | Storage / behavior |
|---------|---------|-----|--------------------|
| `:text` | `:text` | `text` | ordinary string column |
| `:password` | `:text` | `text` | hashed on write; omitted from update payload if absent |
| `:real` | `:number` | `real` | |
| `:integer` | `:number` | `integer` | |
| `:boolean` | `:boolean` | `boolean` | values `:true` / `:false` |
| `:uuid` | `:text` | `uuid` | |
| `:timestamp` | `:text` | `timestamp` | |
| `:list` | `:list` | _(none)_ | virtual M2M list via `:join-table` |
| `:file` | `:text` | _(none)_ | upload token flow; excluded from list-form |
| `:button` | `:button` | _(none)_ | control only; requires `:action` |

### `:source`

```lisp
:source (:view :main
         :table :ratings      ; optional; default = owning type
         :column :rating      ; required when source is present
         :agg :first          ; aggregation
         :scope :user)        ; optional field-level scope
```

If `:source` is omitted but `:column t`, the compiler defaults to:

```lisp
(:view :main :column <field-key> :agg :first)
```

**`:agg` values** (`aggregate-values` in backend):

| Agg | Result |
|-----|--------|
| `:first` | first non-null |
| `:list` | list of non-null |
| `:distinct` | distinct non-null |
| `:avg` | float average |
| `:sum` | sum |
| `:count` | count of non-null |

For M2M list fields (`:type :list` + `:join-table`), the row-display
`:source :agg` **must be `:distinct`** — it is a compile error to write
any other value there (see [Join tables (M2M)](#join-tables-m2m)).

### Field-level scope

```lisp
:source (... :scope :user :agg :first)
```

Filters aggregated rows to the current user's UUID (via the view alias of the
source table's `:user` field). Used for "my rating" style fields.

**M2M list fields (`:type :list` + `:join-table`) have set semantics: the
row-display `:source :agg` must be `:distinct`** (omitting `:agg` is
allowed — the compiler fills `:distinct`; writing `:list` or any other
agg is a compile error). `:source-all` keeps `:agg :list`. A flat LEFT
JOIN view with two or more one-to-many arms yields the cross product of
the arms, so `:agg :list` duplicates every value; see
[M2M row-display is `:distinct`](#m2m-row-display-is-distinct-fan-out-duplicate-guard).

**Does not** control field visibility or editability in the UI.

Requires the source table to expose a `:user` field in that view's aliases.

### `:source-all`

Same shape as `:source`. Feeds `allowed-values` for dropdowns and checkbox
lists (distinct from the row-display `:source`). Runtime prefers
`:source-all` when present, otherwise falls back to `:source`.

```lisp
:source     (:view :main :table :tags :column :name :agg :distinct)
:source-all (:view :tags :table :tags :column :name :agg :list)
```

`allowed-values-for-field` reads `:table` and `:column` from that plist and
loads options via `be-list-column` on the related type. It does not use
`:view`. Authors should still set both as in todos/modelbank: `:source` for
row display, `:source-all` for the options contract. Preferring
`:source-all` alone does not break bidirectional M2M (recursion is
handled by the `:skip-allowed-values` flag in the backend).

For M2M list fields, the column used for options and join lookup must be the
**single identity field** of the related type (see [Identity fields](#identity-fields)).

### `:ui` subkeys

The `:ui` plist is passed through to the frontend **verbatim** (plus a few
keys injected by `fe-fields`). Unknown subkeys are harmless extension points.

| Subkey | Values / meaning |
|--------|------------------|
| `:label` | display label string (auto-generated from field key if omitted) |
| `:widget` | `:textbox` \| `:textarea` \| `:code` \| `:stars` \| `:select` \| `:file` \| `:checkbox` \| `:checkbox-list` \| `:password` \| `:hidden` \| `:button` \| `:image` \| `:image-list` |
| `:read-only` | boolean (`t` / `nil`); renders display variant instead of editor |
| `:precision` | number; JavaScript `toFixed` for numeric display (e.g. average rating) |
| `:options` | list of non-empty strings; static dropdown values (requires `:widget :select`) |
| `:table` | **injected by `fe-fields`** from source table / type-key; used for `/api/file` URLs; do not set manually |

Widget semantics:

- `:textbox`: single-line `<input type="text">`
- `:textarea`: multi-line `<textarea>` (~8 rows, resizable)
- `:code`: monospace `<textarea>` (~12 rows)
- `:stars`: interactive StarRating (editable) or static (read-only / list)
- `:checkbox`: single boolean checkbox
- `:checkbox-list`: multi-select from `allowed-values`; the frontend
  adds client-side search once the option set reaches 10
- `:select`: `<select>` dropdown. Two modes:
  - **Relation** (default): options from `allowed-values` via `:target`
    (foreign key). Requires `:target` + `:source` + `:source-all`.
  - **Static**: options from `:options` (a list of non-empty strings).
    Requires `:ui (:widget :select :options (...))`. No `:target` or
    `:join-table`. The stored value is the option string itself.
    See `models/test/static-select-test.lisp` for an example.
  - A bare `:widget :select` with neither `:options` nor `:target`
    is a compile error (empty dropdowns are not a valid mode).
- `:file`: file input + two-phase upload
- `:password`: masked password input
- `:button`: action button (update form only)
- `:hidden`: omitted from form entirely
- `:image`: display thumbnail (always read-only for MVP)
- `:image-list`: display thumbnail grid (always read-only for MVP)

Compiler default injection (missing keys get safe compile-time defaults):

- Missing `:widget` → compiler injects `:widget :textbox`
- Missing `:read-only` on `:image` or `:image-list` → compiler injects
  `:read-only t` (both are display-only for MVP)
- Explicit `:read-only nil` on `:image` or `:image-list` → compile error
  (editable image widgets are post-MVP)
- Missing `:label` (when `:ui` is present) → humanized field key:
  split on `-` / `_`, title-case each word, join with spaces
  (e.g. `:average-rating` → "Average Rating", `:name` → "Name")
- `:hidden` is never implied by omission; it must be set explicitly
- Principle: the compiler injects safe defaults for missing keys so author
  models stay small and syntax can simplify later (AI/no-code tiers)

Abolished keys and values (compile-time errors if present):

- `:input-type`: renamed to `:widget`
- `:render-as`: deleted; presentation derives from `:widget`
- `:form-control`: rejected name; never shipped
- `:line` as a widget value: use `:textbox`
- `:text` as a widget value: use `:textarea`
- `:read-only` as a widget value: use `:read-only t` boolean flag

Notes:

- `:widget :hidden` → excluded from `fe-fields`
- `:widget :button` is required on button fields so the FE renders a control

### Foreign keys (`:target`)

```lisp
:user
(:type :text
  :autofill :user
  :force-sql-name "image_user"
  :ui (:label "Owner" :widget :textbox :read-only t)
  :target :users
  :source (:view :main :table :users :column :name :agg :first)
  :source-all (:view :users :table :users :column :name :agg :list)
  :column t :not-null t)
```

- SQL type becomes `uuid` with `references … on delete cascade`
- Target type must have exactly one `:identity t` field
- UI typically shows the identity display value (often `:name`), not the UUID
- `:autofill :user` fills the current username on insert
- **`:not-null` is optional**: the compiler respects the author's
  `:not-null` setting on `:target` fields. Set `:not-null t` for a
  required FK (the common case); omit it (or `:not-null nil`) for a
  nullable / optional FK reference. When `:not-null` is absent, the
  generated DDL omits `NOT NULL`, and the backend accepts `nil` /
  `:null` values on insert and update (writing SQL `NULL`).
  Validation (`v-type`, `value-type-p`) also passes `nil` for nullable
  fields without complaint.
- For "zero or more" references, use an M2M list field (`:type :list` +
  `:join-table`); the join table can be empty, so the relationship is
  naturally optional.

### Identity fields

`:identity t` marks natural-key participants. Behavior depends on **how the
type is used**:

| Use | Multiple `:identity t` fields? | Mechanism |
|-----|--------------------------------|-----------|
| Uniqueness / write-through search | **Yes**: all identity columns form one composite unique index `ix_<table>_identity` | `create-table-sql`, `search-sql`, `identity-keys` |
| Type is a **`:target`** of an FK | **No**: exactly one identity field | `valid-target` (compile error otherwise) |
| Type is the other side of an **M2M** list | **No**: join insert/update resolves checkbox values via `identity-field` → single key, then `list-ids` | `insert-join-table-rows`, `update-join-tables` |

**Composite identity is real** (Model Bank `:ratings` uses `:book` + `:user`
both `:identity t`). That works because ratings are matched by write-through
search, not by a single display string in a checkbox list.

**Referenced types need a scalar display identity.** If authors (or any type)
appear in another type's `:type :list` / `:join-table` field or as a `:target`,
they must have **exactly one** `:identity t` field. That field's values are
what the UI sends and what the backend looks up.

**Do not** mark only `:last-name` (or any non-unique part of a name) as
identity when the type is M2M-referenced: the unique index is then on that
column alone, so two "Cameron" rows collide (`23505` on
`ix_rt_<type>_identity`), and the checkbox list cannot distinguish them.

**Workaround for structured names (first / middle / last):**

1. **Single full-name field:** one full-name field as the sole identity
   (e.g. `:name` = "Donald Roy Cameron"). Optionally keep first/middle/last
   as ordinary non-identity columns if you still want them on the form.
2. **Composed identity field (preferred for structured names):** keep the
   parts as ordinary fields and add a stored identity field whose value is
   composed server-side via `:compose`. The compiler synthesizes
   `:compose-string` lifecycle hooks for `:pre-create` and `:pre-update`
   that build the composed value before validation and write:

   ```lisp
   :first-name (:type :text ...)
   :middle-name (:type :text ...)
   :last-name (:type :text ...)
   :full-name (:type :text :identity t :unique t :not-null t
                :compose ":first-name :middle-name :last-name"
                :ui (:label "Full Name" :widget :textbox :read-only t)
                ...)
   ```

   See [`:compose`](#compose) below for the format language and constraints.
3. **Not supported yet:** multi-column reference lookup (composite identity
   as the M2M/FK display protocol).

**Rule of thumb:** if another type's list or select must point at this type,
give it one unique identity string. Use composite identity only for types that
are write-through targets (or otherwise searched by full identity key sets),
not for checkbox-list labels.

---

## `:compose`

The `:compose` field attribute is sugar for the `:compose-string` lifecycle
hook. It lets the compiler build a stored field value from other fields on the
same type, most commonly to create a single composed identity string from
structured parts (e.g. full name from first / middle / last).

### Format

```lisp
:full-name (:type :text :identity t :unique t :not-null t
             :compose ":first-name :middle-name :last-name"
             :ui (:label "Full Name" :widget :textbox :read-only t)
             :source (:view :main :column :full-name :agg :first)
             :column t)
```

The `:compose` value is a **template string** using the same format language
as `:compose-string`:

- Placeholders are bare keyword tokens: `:first-name`, `:last-name`, etc.
- Each placeholder is replaced by the string value of that field in the
  record's data.
- Missing, `nil`, or `:null` placeholders become empty string.
- Whitespace is collapsed (runs of spaces → single space) and trimmed.

### What the compiler does

For each field with `:compose`, the compiler synthesizes `:compose-string`
forms and appends them to the type's `:pre-create` and `:pre-update` hook
lists **after** any author-declared hooks. The composed value is therefore
available to validation and written to the database like any field value.

### Constraints

- **Placeholders must name existing fields** on the type (compile-time error).
- **The `:into` target is the field itself**; no separate `:into` needed.
- **Self-reference is a compile error**: a field's `:compose` template may
  not reference itself.
- **Duplicate is a compile error**: if a field has `:compose` *and* the same
  field is targeted by a manual `:compose-string` hook (via `:pre-create` or
  `:pre-update`), compilation fails.

### Type-level `:compose-string` (alternative)

For cases needing more control (e.g. composing into a field from a different
hook, or conditional logic alongside composition), use the `:compose-string`
lifecycle hook directly on `:pre-create` / `:pre-update`:

```lisp
:pre-create (:compose-string
              :format ":first-name :middle-name :last-name"
              :into :full-name)
```

See `docs/hook-registry.md` → Registered Lifecycle Hooks for the full
`:compose-string` contract.

### Form guidance

- **`:add-form`:** omit the composed field. The server builds it from the
  parts; showing it is redundant and it has no value until save.
- **`:update-form`:** include the composed field with `:read-only t` so users
  can see the derived value but not edit it directly (editing would be
  overwritten on the next save).

---

## Forms

```lisp
:list-form   (:fields t)
:add-form    (:fields (:name :description :model))
:update-form (:fields t)
```

Only `:fields` is meaningful under each form:

| Value | Meaning |
|-------|---------|
| `t` | all eligible fields |
| list of keywords | explicit set; `:id` is always prepended by `form-field-keys` |
| absent / nil | no form |

### Eligibility (`fe-fields`)

A field is emitted when:

1. the form includes it (`t` or explicit list; non-base `:roles` always eligible when shown)
2. it has `:ui :widget` (or is injected roles)
3. widget is not `:hidden`
4. not (`:list-form` and `:type :file`)
5. not (`:list-form` or `:add-form` and `:type :button`)

Buttons appear on the **update form only**.

If an explicit `:update-form` lists a button field, the compiler auto-appends
the companion `:<button>-status` field (`augment-update-form`).

Emitted field meta includes `:default`, `:path`, `:table`, and the full `:ui`
plist.

---

## Join tables (M2M)

Virtual list field on the owning type:

```lisp
:tags
(:type :list
  :ui (:label "Tags" :widget :checkbox-list)
  :validations (:join-items-exist)
  :source (:view :main :table :tags :column :name :agg :distinct)
  :source-all (:view :tags :table :tags :column :name :agg :list)
  :join-table :todo-tags)
```

On `:type :list` + `:join-table`:

- Row-display `:source :agg` must be `:distinct` (omit `:agg` and the
  compiler fills `:distinct`)
- `:source (... :agg :list)` is a compile error
- `:source-all` stays `:agg :list` (dedicated single-table view; no fan-out)
- Other `:agg` values on that `:source` are compile errors
- This is set semantics, not a multi-chain special case. Write `:distinct`
  even on a one-joiner view (todos tags) so a later second arm does not
  duplicate values

Joiner type:

```lisp
:todo-tags
(:table t :is-joiner t :internal t
  :fields
  (:reference (:target :todos)
    :reference (:target :tags)))
```

- Each `:reference` is rewritten to `:<singular-target>-id` (e.g. `:todo-id`,
  `:tag-id`)
- Joiners are internal: no public insert/update/delete SQL, hidden from type
  selector
- Author `:internal t` on joiners is conventional but redundant (`:is-joiner`
  defaults internal)
- A type may have multiple M2M joiners (e.g. chores has both
  `:chore-tags` and `:chore-users`). Each joiner's insert/update/delete
  SQL is isolated to its own two FK columns. List field keys need not
  match the target type key; `:source :table` identifies the other
  side (e.g. `:completed-by` → `:users`).

### M2M row-display is `:distinct` (fan-out duplicate guard)

The compiler enforces the rule above: on any `:type :list` field backed
by a `:join-table`, the row-display `:source :agg` must be `:distinct`
(`:list` or any other value is a compile error; an omitted `:agg` is
injected as `:distinct`).

Why the rule exists: when a type's `:main` view includes **two or more
one-to-many arms** (e.g. `:chores` joins both `chore-tags` → `tags` and
`chore-users` → `users`; or `:books` joins authors plus ratings), the
flat LEFT JOIN view SQL produces the **cross product** of the arms: a
chore with 3 tags and 2 completers yields 3 × 2 = 6 rows, where every
tag value appears twice and every user value appears three times.

`:agg :list` collects every non-null value from those rows, so the
duplicates become visible in list cells and edit forms: a single
completer renders as `("amanda" "amanda")` as soon as the chore has
2 tags. With only one arm in the view (e.g. todos: tags only) `:list`
would show each value once; the bug is invisible until a second arm is
added — which is why the rule is unconditional, not "two or more arms".

This is inherent to flat-join view SQL (no per-arm subqueries in the
MVP); the data itself is not duplicated. `:source-all` stays
`:agg :list`; it reads a dedicated single-table view, so it has no
fan-out to dedupe.

```lisp
;; chores: main view joins chore-tags AND chore-users arms
:completed-by
(:type :list
  ...
  :source (:view :main :table :users :column :name :agg :distinct)
  :source-all (:view :users :table :users :column :name :agg :list)
  :join-table :chore-users)
```

Cousin (not covered by the rule): 1:N list fields **without**
`:join-table` (Model Bank `:images`, FK-based) can still fan out when
the view has another arm; authors should use `:agg :distinct` there too.

Related trap, **not** fixed by `:distinct`: numeric aggs (`:avg` /
`:sum` / `:count`) over a sibling arm can still double-count the cross
product (a book's average rating counts each rating once per author).
That is a separate backlog item; do not treat `:distinct` on list
fields as fixing it.

### Bidirectional M2M (list fields on both ends)

Both ends of a joiner may have `:type :list` fields. The backend breaks
the `allowed-values` ↔ `be-list` recursion by skipping nested
allowed-values computation when fetching column values in service of
building options (`:skip-allowed-values t` on the inner `be-list-column`
call).

**Pattern** (both sides share one joiner):

| Side | `:type :list` + `:join-table`? | Main view joins through joiner? |
|------|--------------------------------|----------------------------------|
| Either end (e.g. `:books`) | Yes | Yes: `(:books :joiner :authors)` |
| Other end (e.g. `:authors`) | Yes | Yes: `(:authors :joiner :books)` |
| Joiner | n/a | `:is-joiner t` + two `:reference`s |

Each side needs:
- A `:type :list` field with `:join-table` pointing at the shared joiner
- `:source` joining through the joiner in the main view for row display
- `:source-all` pointing at a named view on the other type for options
- A named view (e.g. `:authors (:tables (:authors))`) for `:source-all`

Example (abbreviated):

```lisp
:books
(:table t
  :views (:main (:tables (:books :book-authors :authors))
           :authors (:tables (:authors)))
  :fields
  (:title (:type :text :identity t ...)
    :authors (:type :list
      :source (:view :main :table :authors :column :name :agg :distinct)
      :source-all (:view :authors :table :authors
                   :column :name :agg :list)
      :join-table :book-authors))
  ...)

:authors
(:table t
  :views (:main (:tables (:authors :book-authors :books))
           :books (:tables (:books)))
  :fields
  (:name (:type :text :identity t ...)
    :books (:type :list
      :source (:view :main :table :books :column :title :agg :distinct)
      :source-all (:view :books :table :books
                   :column :title :agg :list)
      :join-table :book-authors))
  ...)

:book-authors
(:table t :is-joiner t :internal t
  :fields
  (:reference (:target :books)
    :reference (:target :authors)))
```

Both ends share **one** joiner type; do not invent a second joiner for
the reverse direction.

Unidirectional M2M (list field on one end only) continues to work as
before. Base model follows that pattern: users have `:roles`, roles have
`:permissions`; not users↔roles both as list fields.

### M2M value resolution

Checkbox-list values are the related type's **single identity field** values
(strings). On insert/update, the backend maps those strings to UUIDs with
`list-ids` + `identity-field`. The list field's `:source` / `:source-all`
`:column` should be that identity field (commonly `:name`).

If the identity column is not unique in practice (e.g. last name only), join
rows attach to the wrong row or inserts fail the identity unique index.

---

## Write-through

Write-through lets a field on type A upsert a related row on type B when A is
inserted or updated. Used by Model Bank ratings.

```lisp
:rating
(:type :integer
  :ui (:label "My Rating" :widget :stars)
  :validations ((:in-range :min 1 :max 5))
  :source (:view :main :table :ratings :column :rating
           :scope :user :agg :first)
  :write-to (:table :ratings
              :model :this
              :user  :user
              :rating :value))
```

### `:write-to` structure

| Key | Meaning |
|-----|---------|
| `:table` | target type (existing, non-internal) |
| other keys | field names on the **target** type |
| values | `:this` (primary record UUID) \| `:user` (current user UUID) \| `:value` (this field's value) |

Rules (`write-to` in `model.lisp`):

- a `:value` tag is required (the payload); with more than one, the first
  silently wins — write exactly one
- every `:identity t` field on the target must appear as a key
- compile-time validated

### Ratings pattern (my rating + average)

A plain integer `:rating` column on the parent type only stores one value per
row, not per-user ratings and not an average. For multi-user ratings, follow
Model Bank:

1. **Separate `:ratings` type** with composite identity (e.g. `:book` +
   `:user`, or `:model` + `:user`), each a `:target` FK, plus a `:rating`
   integer column. Include those types in views as needed.
2. **On the parent type (e.g. `:books`):**
   - **My Rating**: virtual/write-through field; `:source` from ratings with
     `:scope :user` and `:agg :first`; `:write-to` upserts the ratings row
     (`:book :this`, `:user :user`, `:rating :value`). UI label "My Rating",
     `:widget :stars`, `:validations ((:in-range :min 1 :max 5))`.
   - **Average (label "Rating")**: read-only `:type :real`, `:source` from
     ratings with `:agg :avg`, `:widget :stars :read-only t`, optional `:precision 1`.
     No `:write-to`, no `:column` required on the parent.
3. **Parent main view** must join the ratings table (and any M2M tables), e.g.
   `(:main (:tables (:books :book-authors :authors :ratings)))`.
4. **Forms:**
   - list: average only (not my-rating), e.g.
     `(:fields (:title :isbn :description :average-rating :authors))`
   - add: my-rating, not average:
     `(:fields (:title :isbn :description :rating :authors))`
   - update: both; `(:fields t)` is fine

Write-through target identity keys in `:write-to` must match the ratings type
field names (`:book` / `:user` / `:rating`, not hard-coded `:model` unless the
type uses `:model`).

See `models/modelbank.lisp` for the canonical example.

### Runtime (`execute-write-to`)

1. Primary insert/update **commits first**
2. Expand tags → `search-sql` on target identities
3. Update if found, else insert resource + row
4. A newly inserted related row **inherits the source record's role set**
   (plus `admin` and the creator's exclusive role), so its visibility
   matches what it was derived from (a rating inherits the rated model's
   visibility) with no author declaration
5. Best-effort `handler-case`; errors are logged, not rolled back

**MVP:** not transactional with the primary write. Clear-to-NULL and other edge
cases are still open (see AGENT.md).

---

## Buttons and action hooks

```lisp
:deploy
(:type :button
  :ui (:label "Deploy Model" :widget :button)
  :action (:deploy-model :field :model))
```

| Requirement | Detail |
|-------------|--------|
| `:type :button` | no storage column |
| `:action` | single registry form (keyword or `(:name …params)`); **only** valid on buttons |
| `:ui :widget` | must be `:button` |

### Status field (compiler-synthesized)

For button field `F`, compiler injects `:F-status` if absent (else compile
error):

| Property | Value |
|----------|-------|
| `:type` | `:text` |
| `:column` | `t` |
| `:default` | `"idle"` |
| `:not-null` | `t` |
| `:ui` | `(:label "<ButtonLabel> Status" :widget :textbox :read-only t)` |
| `:source` | `(:view :main :column :F-status :agg :first)` |

Status writes go through `be-set-field-value` only.

### Status protocol

| Status | Who sets | Terminal? |
|--------|----------|-----------|
| `idle` | column default | yes |
| `running` | `be-action` before hooks | no |
| `complete` | framework (sync) or worker via `set-status` | yes |
| `failed: <reason>` | framework (sync error) or worker | yes |

- **In-progress guard:** refuse to start if status is exactly `"running"`
- **Sync hooks:** framework sets `complete` / `failed: …`
- **Async hooks** (result includes `:async t`): worker must call `set-status`;
  process restart can leave `running` forever (no job queue in MVP)

### Placement and permissions

- Update form only
- Permission: type-level `update` + record-level access (same as `be-update`)
- REST: `POST /api/actions` with `{"type", "id", "field"}`

Full contract and registered actions: `docs/hook-registry.md`.

---

## Tree and filesystem-backed types

```lisp
:directories
(:table t :create :auto :update :auto :delete :auto :display t
  :tree t :is-leaf nil :parent-type :directories :fs-backed t
  :type-roles ("directories-user")
  :fields
  (:name
    (:type :text :identity t :path t
      :ui (:label "Directory" :widget :textbox)
      :validations (:required)
      :source (:view :main :column :name :agg :first)
      :column t :not-null t :unique t))
  ...)

:files
(:table t ...
  :tree t :is-leaf t :parent-type :directories :fs-backed t
  :fields
  (:name (... :path t ...)
   :file (:type :file
           :ui (:label "Select File" :widget :file)
           :validations (:required)))
  ...)
```

Constraints (`validate-tree`):

- `:is-leaf`, `:parent-type`, `:fs-backed` each require `:tree t`
- `:tree t` requires `:parent-type` naming an existing non-base type
- at most one `:path t` field per type (`mark-path-field`); if omitted, compiler
  picks `:name` or the sole user text field

Behavior:

- `add-root-fs-resources` inserts logical `"/"` for non-leaf tree types
- insert validates/stores directories; delete is recursive (FS + resources)
- path values: directories end with `/`; leaves must not

---

## Hooks

All custom logic attaches via the **registry** (data-only forms). Raw lambdas
are not an author surface; register a custom hook with `register-hook` instead.
Details: `docs/hook-registry.md`.

### Surface forms

```lisp
:required                            ; bare keyword
(:max-length :max 19)                ; parameterized
(:in-range :min 1 :max 5)
(:deploy-model :field :model)        ; action
```

### Attachment points

| Kind | Where | Compile | Runtime |
|------|-------|---------|---------|
| validation | field `:validations` | `compile-validations` (always prepends type check; optional required) | `validate-field-internal` |
| lifecycle | type `:pre-create` `:post-create` `:pre-update` `:post-update` `:pre-delete` `:post-delete` | `compile-lifecycle-hooks` | `run-lifecycle-hooks` in be-insert/update/delete |
| action | field `:action` on `:button` | → `:compiled-hook` | `be-action` |

Lifecycle may be a single form or a list. Validation is always a list.

### Registered validation hooks

| Name | Params | Behavior |
|------|--------|----------|
| `:required` | n/a | reject empty/nil |
| `:user-name` | n/a | username policy |
| `:password` | n/a | password policy |
| `:email` | n/a | email format |
| `:join-items-exist` | n/a | each list item exists |
| `:exists` | n/a | single referenced value exists |
| `:max-length` | `:max` integer | string length ≤ max (no-op if empty) |
| `:in-range` | `:min` `:max` integers | numeric inclusive (no-op if empty) |

### Registered action hooks

| Name | Params | Behavior |
|------|--------|----------|
| `:deploy-model` | `:field` keyword | deployer role required; validate model text in-process; async deploy worker (writes `models/local/`, no git commit) |
| `:generate-model` | `:description-field`, `:model-field` keywords | ai-user role required; async LLM call writes a validated model into `:model-field` (config in the admin `llm-config` secret) |
| `:spawn` | `:close` plist, `:clear` list | close the record + insert a fresh successor (recurring-instance pattern); sync; reserved close values `:now` / `:user` |

Full contracts and per-hook detail: `docs/hook-registry.md`.

(Test-only hooks may also be registered in the live image: `:test-sync`,
`:test-async`, `:test-error`.)

### Contracts (summary)

```text
validation: (type-key field-key value user) → nil | error-string
lifecycle:  (type-key data user &key id roles record) → nil | plist
            nil = no change; plist = merge into data; non-plist = error
action:     (type-key field-key record user &key roles status-field set-status)
            → nil | (:async t :message "…")
```

### MVP caveat

Lifecycle hooks, action hooks, and write-through are **not** transaction-wrapped
with the primary write. A failing hook fails the operation without rolling back
prior side effects. Design for eventual transactional wrapping; do not assume
atomicity today.

---

## Compiler-injected behavior

### Default fields (`default-fields`)

Every non-internal type receives:

| Field | Type | Notes |
|-------|------|-------|
| `:id` | `:uuid` | PK; non-base → `:target :resources`; base/joiner → `:default :generate-uuid` |
| `:created-at` | `:timestamp` | `:default :now`, `:not-null t` |
| `:updated-at` | `:timestamp` | `:default :now` + trigger `set_<table>_updated_at` |

Marked `:base-field t`. Not part of author field lists.

### Roles field

Non-base types without `:suppress-roles` get a synthetic `:roles` checkbox-list
on forms. `allowed-values.roles` is filtered to roles the current user may
assign: their own roles, "public", the type's `:type-roles`, and every other
user's shareable exclusive role (point-to-point sharing).

### Table and column naming

| Kind | Table name |
|------|------------|
| built-in | `to_sql_identifier(type)` e.g. `users`, `settings` |
| user types | `rt_<name>` e.g. `rt_todos` |

Columns: `<singular_table>_<field>` unless `:force-sql-name`, default field, or
simple joiner FK.

### Identity index

If any field has `:identity t`:

```sql
create unique index if not exists ix_<table>_identity
  on <table> (<identity columns>);
```

### Built-In Types (`*base-model*`)

Always merged into every compiled model:

| Type | Flags | Notes |
|------|-------|-------|
| `:users` | base, built-in | custom create/delete; settings row lifecycle; `:name` and `:email` are `:searchable t` and `:sortable t` |
| `:resources` | base, built-in, internal | no CRUD |
| `:permissions` | base, built-in | `:name` is `:searchable t` and `:sortable t` |
| `:roles` | base, built-in | `:name` is `:searchable t` and `:sortable t` |
| `:role-permissions`, `:resource-roles`, `:role-users` | base, built-in, joiner, internal | |
| `:settings` | base, built-in, user-setting | dark-mode, display-name, bio; view scope `:user` |
| `:secrets` | built-in, not base, suppress-roles, category `:settings` | per-user secrets; `:name` is `:searchable t` |
| `:tokens` | built-in, not base, no display | internal token store |

RBAC types are treated like user-defined types at the API level (e.g. assign
roles the same way you assign tags).

---

## Compilation pipeline

```
set-model
  → top-level-settings          ; validate A keys → *top-level-settings*
  → compile-model (:types)
       stage-1 validate-model:
         append *base-model*
         preliminary-model-check
         per type:
           ensure-action-on-buttons
           synthesize-status-fields
           compile-fields (+ validations, write-to, action)
           mark-path-field
           validate-tree
           compile-lifecycle-hooks
           compute-category / suppress-roles
           augment-update-form
       stage-2:
         enrich-views (SQL / aliases)
         insert / update / delete / search SQL
         create-table-sql
       stage-3:
         compile-fields-stage-2 (alias-key, scope-alias on sources)
  → create-tables
  → ensure-model-roles / add-type-roles
  → add-system-user-settings
  → add-root-fs-resources
  → start-web-server
```

Pure validation without side effects: `validate-model` (used by `:deploy-model`
before spawning a deploy).

---

## Non-author / compiler-only keys

Do **not** set these in model files. They appear on `*compiled-model*` after
compile:

**Type-level:** `:table-name`, `:create-table-sql`, `:insert-sql`,
`:update-sql`, `:delete-sql`, `:search-sql`, compiled lifecycle function lists,
resolved `:category` / `:internal`, `:searchable-fields` (stage 3),
`:phase-a-shape` (`:base` or `:measure`).

**View-level:** `:sql`, `:aliases`, `:columns`, normalized `:scope`,
`:phase-a-base-sql`, `:phase-a-join-sql`; on rollups also
`:measure-phase-a-select` / `-group-by` / `-count-select` / `-grain-where`.

**Field-level:** `:name-sql`, `:type-sql`, `:create-sql`, compiled
`:validations` (function list), `:base-field`, `:compiled-hook`,
`:status-field`, source enrichments (`:alias-key`, `:column-name`,
`:scope-alias`, `:scope-kind`).

---

## Worked examples

### 1. Simple CRUD + M2M (todos)

Abbreviated from `models/todos.lisp` (which additionally sets
`:domain-stg`, `:guest-allowed t`, `:guest-auto nil`,
`:api-roles`, `:default-sort`, `:sortable` / `:searchable` on fields,
`"public"` in `:type-roles`, and D1 `:type-roles` overlays on the
built-in `:users` / `:roles` / `:permissions`):

```lisp
'(:title "To Do List"
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

### 2. Write-through + field scope + stars (modelbank)

```lisp
:rating
(:type :integer
  :ui (:label "My Rating" :widget :stars)
  :validations ((:in-range :min 1 :max 5))
  :source (:view :main :table :ratings :column :rating
           :scope :user :agg :first)
  :write-to (:table :ratings
              :model :this
              :user :user
              :rating :value))
:average-rating
(:type :real
  :ui (:label "Rating" :widget :stars :read-only t :precision 1)
  :source (:view :main :table :ratings :column :rating :agg :avg))
```

### 3. Button + action (Model Bank)

```lisp
:deploy
(:type :button
  :ui (:label "Deploy Model" :widget :button)
  :action (:deploy-model :field :model))
```

### 3b. Spawn button (recurring instances)

```lisp
:complete
(:type :button
  :ui (:label "Complete" :widget :button)
  :action (:spawn
            :close (:completed :true
                    :completed-at :now
                    :completed-by :user)
            :clear (:notes :instance-id)))
```

One click closes the record (close fields written to the old row, which
becomes history) and inserts a fresh successor with definition fields
copied and cleared fields reset to their defaults. A spawnable type's
`:identity t` field should carry `:default :generate-uuid` so cleared
successors never collide on the identity index. See
`docs/hook-registry.md` → `:spawn`.

### 4. Tree / fs-backed leaf with file upload (file-server)

```lisp
:files
(:table t
  :create :auto :update :auto :delete :auto :display t
  :tree t :is-leaf t :parent-type :directories :fs-backed t
  :type-roles ("file-users")
  :views (:main (:tables (:files)))
  :fields
  (:name
    (:type :text :identity t :path t
      :ui (:label "File" :widget :textbox)
      :validations (:required)
      :source (:view :main :column :name :agg :first)
      :column t :not-null t :unique t)
    :file
    (:type :file
      :ui (:label "Select File" :widget :file)
      :validations (:required)))
  :list-form (:fields t)
  :update-form (:fields t)
  :add-form (:fields t))
```

---

## Known gaps and gotchas

1. **Obsolete patterns**: older notes mentioning `:checks-fn`, `:options-fn`,
   root-level `:widget`, or `:lookup-field` are dead. Use `:validations`,
   `:source-all`, and `:ui` instead.

2. **`:required` dual path**: field key `:required t` and/or
   `:validations (:required)`. Prefer the validations form (what all current
   models use).

3. **Field `:update nil`**: documented for status fields in AGENT.md but **not**
   retained by `compile-field`. Status protection is practical (read-only UI +
   `be-set-field-value`), not a compiler-enforced update block.

4. **View scope plist form**: validated, not implemented. Use `:scope :user`.

5. **Type→widget inference**: not implemented; all fields default to
   `:textbox` regardless of `:type`. Smarter inference (e.g. `:type :text`
   → `:textarea`) is an MVP Backlog item.

6. **`:type :file` validation**: models comment that `:valid-file` should
   exist; it is not implemented yet.

7. **Diamond join graphs**: unsupported.

8. **Write-through clear-to-NULL** and **transactions**: post-MVP / open.

9. **`:source-sel`**: handled in stage-2 plumbing but unused by any model;
   treat as unfinished.

10. **Joiner `:internal t`**: redundant with `:is-joiner` default; still
    conventional in examples.

11. ~~**Bidirectional M2M list fields**~~: **Resolved.** Both ends of a
    joiner may now have `:type :list` fields. The backend breaks the
    `allowed-values` ↔ `be-list` recursion via a `:skip-allowed-values`
    flag on the inner column fetch. See
    [Bidirectional M2M](#bidirectional-m2m-list-fields-on-both-ends).

12. **Reference identity is scalar**: types used as `:target` or M2M list
    targets need exactly one `:identity t` field. Composite identity is for
    write-through / uniqueness, not checkbox-list lookup (see
    [Identity fields](#identity-fields)).

13. ~~**No computed/composed fields**~~: **Resolved.** The `:compose` field
    attribute and `:compose-string` lifecycle hook now support server-side
    composition of stored field values (e.g. full name from first/middle/last).
    See [`:compose`](#compose) and [Identity fields](#identity-fields).

14. **Sorting a CRUD list by an aggregated field is not supported.** A
    regular (non-rollup) type's `:agg` fields — e.g. Model Bank
    `:models` `:average-rating` — are computed in Phase B and cannot be
    sorted honestly under `LIMIT`. `:sortable t` on such a field is a
    compile error on base types (the `:column t` rule). Ranked-by-
    aggregate UX uses a [rollup](#rollup-types-read-only-analytical)
    type instead (see MVP Backlog "Regular-type computed-field sort").

15. **`:new-roles` is additive-only.** Removing a role from `:new-roles`
    does not delete it from the database, and changing the permission
    list of a role that already exists has no effect until the role is
    deleted (the role keeps whatever permissions it had).

16. **`:type-roles` override on built-in types does not revoke live
    grants.** `add-type-roles` only inserts missing resources, so
    tightening `:users` to `("admin")` on an already-initialized database
    leaves `"logged-in"` attached to `type-users`. Clean slate (new deploy
    / `reset-database`) required. Also, any type with a
    `:target :users` field becomes unlistable for non-admins until
    `allowed-values` is hardened — see
    [Overriding defaults on built-in types](#overriding-defaults-on-built-in-types).

---

## Quick key index

**Top-level:** `:title` `:name` `:version` `:domain` `:domain-stg`
`:repl` `:guest-allowed` `:guest-auto`
`:api-roles` `:landing-page` `:new-roles` `:types`

**Type:** `:table` `:create` `:update` `:delete` `:display` `:type-roles`
`:default-sort` `:views` `:fields` `:list-form` `:add-form` `:update-form`
`:tree` `:is-leaf`
`:parent-type` `:fs-backed` `:user-setting` `:suppress-roles` `:category`
`:base` `:built-in` `:internal` `:is-joiner` `:rollup` `:grain` `:filter`
lifecycle slots

**View:** `:tables` `:scope`

**Field:** `:type` `:ui` `:column` `:default` `:not-null` `:unique` `:identity`
`:compose` `:required` `:validations` `:source` `:source-all` `:join-table`
`:target` `:write-to` `:autofill` `:force-sql-name` `:path` `:action` `:sortable`
`:searchable` `:default-from` `:css-value` `:primary-key` / joiner `:reference`

**UI:** `:label` `:widget` `:read-only` `:precision` `:options`

**Source:** `:view` `:table` `:column` `:agg` `:scope`

**Write-to tags:** `:table` + target fields mapped to `:this` \| `:user` \|
`:value`

**Field types:** `:text` `:password` `:real` `:integer` `:boolean` `:uuid`
`:timestamp` `:list` `:file` `:button`

**Aggs:** `:first` `:list` `:distinct` `:count` `:avg` `:sum`
