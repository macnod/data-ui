# Model Syntax Reference

A Data UI **model** is a nested plist that describes an entire application:
types, fields, views, forms, RBAC roles, hooks, and deploy metadata. The
compiler expands the model into PostgreSQL schema, parameterized SQL, backend
functions, REST endpoints, and a schema-driven React UI.

Tagline: **"Your whole app, in an email."**

This document is the author-facing vocabulary. Companion docs:

- `docs/hook-registry.md` — validation, lifecycle, and action hook contracts
- `docs/model-accessors.md` — REPL/debug accessors and deploy metadata readers
- `docs/deployment.md` — how top-level keys drive `scripts/data-ui deploy`
- `AGENT.md` — architecture, MVP status, and agent workflow

Sources of truth: `lisp/model.lisp`, `lisp/backend.lisp`, `lisp/predicates.lisp`,
`lisp/database.lisp`, and the example models in `models/`.

---

## File form and loading

Each file under `models/` is a **bare quoted plist** — no `defparameter`, no
wrapping variable:

### File header (recommended)

Precede the model plist with a `;;` comment block that states what the
model is for and where it came from. Put it above the opening quote.
Include:

1. **Purpose** — what the app does (a few sentences is fine)
2. **Author** — who wrote it
3. **Created** — creation date
4. **Prompt** — if an AI produced the model, the prompt that was used

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
| `models/test/<name>.lisp` | Test fixture — do not change unless changing tests |

`(set-model "<name>")` tries `models/<name>.lisp` first, then falls back
to `models/test/<name>.lisp`.

Because the form starts with a quote, Lisp can `read` it. That is useful for
paren-balance checks; it is not how the compiler is invoked.

---

## Top-level keys

Recognized keys: `*top-level-keys*` =
`(:title :name :version :domain :repl :landing-page)`.

`:types` is required alongside those settings but is handled separately by
`compile-model`. Any other root key is ignored by `top-level-settings`.

| Key | Required | Value | Consumed by |
|-----|----------|-------|-------------|
| `:title` | yes | string (display title; restricted charset) | page title, deploy |
| `:name` | yes | string `^[a-z][-a-z0-9]*` | deploy tag/namespace `dataui-<name>` |
| `:version` | yes | string (semver-ish) | image tag |
| `:domain` | yes | FQDN-like string | HAProxy map, TLS host |
| `:repl` | no (default `nil`) | boolean | Swank port iff `t` — **nil in production** |
| `:landing-page` | no | type keyword present in `:types`, or nil | `/api/info` via `be-landing-page`; falls back to first non-base type the user can access |
| `:types` | yes | plist of type-key → type-def | compiler |

Minimal skeleton:

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
| Lifecycle slots | Custom logic | `:pre-create` `:post-create` `:pre-update` `:post-update` `:pre-delete` `:post-delete` — see [Hooks](#hooks) |

### CRUD strategy values

For author models, use `:auto` or `nil`:

- `:auto` — generated SQL path (`insert-normal` / update / `remove-resource`, with FS branches when applicable)
- `nil` — operation disabled
- raw functions — internal base-model escape hatch only; prefer lifecycle hooks

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
| `:resources` | *(none — internal, no CRUD)* | |
| `:tokens` | *(none — internal)* | |

Types with no explicit `:type-roles` default to `("admin")`. The `"admin"`
role is always appended by `add-type-roles` regardless of what the model
declares.

When implementing `:type-roles` overrides on built-in types, these are the
defaults you are replacing.

### Category

`:category` is an author-facing key, not reserved. Valid values:
`:user`, `:settings`, `:system`.

- `:settings` — type appears under the frontend Settings tab (e.g. `:secrets`)
- `:system` — Admin / system group
- `:user` — main app type selector

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
implemented at runtime — use the keyword `:user`.

Compiler injects per view (not author-set): `:sql`, `:aliases`, `:columns`,
normalized `:scope`.

Joiners get no views.

---

## Fields

Under `:fields`, each entry is `field-key` → plist (except joiner
`:reference` entries — see [Join tables](#join-tables-m2m)).

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
| `:action` | **only** on `:type :button` — single action hook form |
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

### Field-level scope

```lisp
:source (... :scope :user :agg :first)
```

Filters aggregated rows to the current user's UUID (via the view alias of the
source table's `:user` field). Used for "my rating" style fields.

**Does not** control field visibility or editability in the UI.

Requires the source table to expose a `:user` field in that view's aliases.

### `:source-all`

Same shape as `:source`. Feeds `allowed-values` for dropdowns and checkbox
lists (distinct from the row-display `:source`). Runtime prefers
`:source-all` when present, otherwise falls back to `:source`.

```lisp
:source     (:view :main :table :tags :column :name :agg :list)
:source-all (:view :tags :table :tags :column :name :agg :list)
```

`allowed-values-for-field` reads `:table` and `:column` from that plist and
loads options via `be-list-column` on the related type. It does not use
`:view`. Authors should still set both as in todos/modelbank: `:source` for
row display, `:source-all` for the options contract. Preferring
`:source-all` alone does not break bidirectional M2M recursion (see known
gap on join tables).

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
| `:table` | **injected by `fe-fields`** from source table / type-key — used for `/api/file` URLs; do not set manually |

Widget semantics:

- `:textbox` — single-line `<input type="text">`
- `:textarea` — multi-line `<textarea>` (~8 rows, resizable)
- `:code` — monospace `<textarea>` (~12 rows)
- `:stars` — interactive StarRating (editable) or static (read-only / list)
- `:checkbox` — single boolean checkbox
- `:checkbox-list` — multi-select from `allowed-values`
- `:select` — `<select>` dropdown. Two modes:
  - **Relation** (default): options from `allowed-values` via `:target`
    (foreign key). Requires `:target` + `:source` + `:source-all`.
  - **Static**: options from `:options` (a list of non-empty strings).
    Requires `:ui (:widget :select :options (...))`. No `:target` or
    `:join-table`. The stored value is the option string itself.
    See `models/test/static-select-test.lisp` for an example.
  - A bare `:widget :select` with neither `:options` nor `:target`
    is a compile error (empty dropdowns are not a valid mode).
- `:file` — file input + two-phase upload
- `:password` — masked password input
- `:button` — action button (update form only)
- `:hidden` — omitted from form entirely
- `:image` — display thumbnail (always read-only for MVP)
- `:image-list` — display thumbnail grid (always read-only for MVP)

Compiler default injection (missing keys get safe compile-time defaults):

- Missing `:widget` → compiler injects `:widget :textbox`
- Missing `:read-only` on `:image` or `:image-list` → compiler injects
  `:read-only t` (both are display-only for MVP)
- Explicit `:read-only nil` on `:image` or `:image-list` → compile error
  (editable image widgets are post-MVP)
- Missing `:label` (when `:ui` is present) → humanized field key:
  split on `-` / `_`, title-case each word, join with spaces
  (e.g. `:average-rating` → "Average Rating", `:name` → "Name")
- `:hidden` is never implied by omission — it must be set explicitly
- Principle: the compiler injects safe defaults for missing keys so author
  models stay small and syntax can simplify later (AI/no-code tiers)

Abolished keys and values (compile-time errors if present):

- `:input-type` — renamed to `:widget`
- `:render-as` — deleted; presentation derives from `:widget`
- `:form-control` — rejected name; never shipped
- `:line` as a widget value — use `:textbox`
- `:text` as a widget value — use `:textarea`
- `:read-only` as a widget value — use `:read-only t` boolean flag

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
- **`:not-null` is optional** — the compiler respects the author's
  `:not-null` setting on `:target` fields. Set `:not-null t` for a
  required FK (the common case); omit it (or `:not-null nil`) for a
  nullable / optional FK reference. When `:not-null` is absent, the
  generated DDL omits `NOT NULL`, and the backend accepts `nil` /
  `:null` values on insert and update (writing SQL `NULL`).
  Validation (`v-type`, `value-type-p`) also passes `nil` for nullable
  fields without complaint.
- For "zero or more" references, use an M2M list field (`:type :list` +
  `:join-table`) — the join table can be empty, so the relationship is
  naturally optional.

### Identity fields

`:identity t` marks natural-key participants. Behavior depends on **how the
type is used**:

| Use | Multiple `:identity t` fields? | Mechanism |
|-----|--------------------------------|-----------|
| Uniqueness / write-through search | **Yes** — all identity columns form one composite unique index `ix_<table>_identity` | `create-table-sql`, `search-sql`, `identity-keys` |
| Type is a **`:target`** of an FK | **No** — exactly one identity field | `valid-target` (compile error otherwise) |
| Type is the other side of an **M2M** list | **No** — join insert/update resolves checkbox values via `identity-field` → single key, then `list-ids` | `insert-join-table-rows`, `update-join-tables` |

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

1. **Preferred today:** one full-name field as the sole identity (e.g. `:name`
   = "Donald Roy Cameron"). Optionally keep first/middle/last as ordinary
   non-identity columns if you still want them on the form.
2. **Not supported yet:** server-side compose of a hidden/read-only `:name`
   from F/M/L via a lifecycle hook (`:compose-string` — designed, not
   implemented; see `~/workbench/compose-string-investigation.org`).
3. **Not supported yet:** multi-column reference lookup (composite identity
   as the M2M/FK display protocol).

**Rule of thumb:** if another type's list or select must point at this type,
give it one unique identity string. Use composite identity only for types that
are write-through targets (or otherwise searched by full identity key sets),
not for checkbox-list labels.

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
  :source (:view :main :table :tags :column :name :agg :list)
  :source-all (:view :tags :table :tags :column :name :agg :list)
  :join-table :todo-tags)
```

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
  match the target type key — `:source :table` identifies the other
  side (e.g. `:completed-by` → `:users`).

### One-way M2M only (do not put list fields on both ends)

**Supported pattern** (todos, base users→roles):

| Side | `:type :list` + `:join-table`? | Main view joins through joiner? |
|------|--------------------------------|----------------------------------|
| Owner (e.g. `:todos`, `:books`) | Yes | Yes — `(:owner :joiner :other)` |
| Other (e.g. `:tags`, `:authors`) | **No** | **No** — default main view is the table alone |
| Joiner | n/a | `:is-joiner t` + two `:reference`s |

**Do not** give both ends a `:type :list` field on the same joiner (e.g. books
have `:authors` and authors have `:books`). That compiles, but **runtime
stack-overflows** on `/api/list`:

```
be-list(:books)
  → allowed-values → authors list field
    → be-list-column(:authors …) → be-list(:authors)
      → allowed-values → books list field
        → be-list(:books)   ; infinite mutual recursion
```

Empty tables still hit this path: `be-list` always builds `allowed-values`
via `list-result`. The request never completes, so the frontend never receives
`create: true` and the Add button does not appear (symptom of the failed list
call, not a separate create-flag bug).

`allowed-values-for-field` calls full `be-list` / `be-list-column` on the
related type (preferring `:source-all`, else `:source`). There is no
recursion guard.

**Workaround:** one-directional M2M only. To show "books by this author,"
filter the books list (or wait for a backend fix that loads allowed-values
without re-entering `be-list` / `allowed-values`).

Base model follows the same rule: users have `:roles`, roles have
`:permissions` — not users↔roles both as list fields.

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

- exactly one value tag must be `:value` (the payload)
- every `:identity t` field on the target must appear as a key
- compile-time validated

### Ratings pattern (my rating + average)

A plain integer `:rating` column on the parent type only stores one value per
row — not per-user ratings and not an average. For multi-user ratings, follow
Model Bank:

1. **Separate `:ratings` type** with composite identity (e.g. `:book` +
   `:user`, or `:model` + `:user`), each a `:target` FK, plus a `:rating`
   integer column. Include those types in views as needed.
2. **On the parent type (e.g. `:books`):**
   - **My Rating** — virtual/write-through field: `:source` from ratings with
     `:scope :user` and `:agg :first`; `:write-to` upserts the ratings row
     (`:book :this`, `:user :user`, `:rating :value`). UI label "My Rating",
     `:widget :stars`, `:validations ((:in-range :min 1 :max 5))`.
   - **Average (label "Rating")** — read-only `:type :real`, `:source` from
     ratings with `:agg :avg`, `:widget :stars :read-only t`, optional `:precision 1`.
     No `:write-to`, no `:column` required on the parent.
3. **Parent main view** must join the ratings table (and any M2M tables), e.g.
   `(:main (:tables (:books :book-authors :authors :ratings)))`.
4. **Forms:**
   - list: average only (not my-rating) — e.g.
     `(:fields (:title :isbn :description :average-rating :authors))`
   - add: my-rating, not average —
     `(:fields (:title :isbn :description :rating :authors))`
   - update: both — `(:fields t)` is fine

Write-through target identity keys in `:write-to` must match the ratings type
field names (`:book` / `:user` / `:rating`, not hard-coded `:model` unless the
type uses `:model`).

See `models/modelbank.lisp` for the canonical example.

### Runtime (`execute-write-to`)

1. Primary insert/update **commits first**
2. Expand tags → `search-sql` on target identities
3. Update if found, else insert resource + row
4. Best-effort `handler-case` — errors are logged, not rolled back

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
are not an author surface — register a custom hook with `register-hook` instead.
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
| `:required` | — | reject empty/nil |
| `:user-name` | — | username policy |
| `:password` | — | password policy |
| `:email` | — | email format |
| `:join-items-exist` | — | each list item exists |
| `:exists` | — | single referenced value exists |
| `:max-length` | `:max` integer | string length ≤ max (no-op if empty) |
| `:in-range` | `:min` `:max` integers | numeric inclusive (no-op if empty) |

### Registered action hooks

| Name | Params | Behavior |
|------|--------|----------|
| `:deploy-model` | `:field` keyword | validate model text in-process; async deploy worker |

(Test-only hooks may also be registered in the live image: `:test-sync`,
`:test-async`, `:test-error`.)

### Contracts (summary)

```text
validation: (type-key field-key value user) → nil | error-string
lifecycle:  (type-key data user &key id roles record) → effect
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
assign.

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
| `:users` | base, built-in | custom create/delete; settings row lifecycle |
| `:resources` | base, built-in, internal | no CRUD |
| `:permissions` | base, built-in | |
| `:roles` | base, built-in | |
| `:role-permissions`, `:resource-roles`, `:role-users` | base, built-in, joiner, internal | |
| `:settings` | base, built-in, user-setting | dark-mode, font-size, display-name, bio; view scope `:user` |
| `:secrets` | built-in, not base, suppress-roles, category `:settings` | per-user secrets |
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
resolved `:category` / `:internal`.

**View-level:** `:sql`, `:aliases`, `:columns`, normalized `:scope`.

**Field-level:** `:name-sql`, `:type-sql`, `:create-sql`, compiled
`:validations` (function list), `:base-field`, `:compiled-hook`,
`:status-field`, source enrichments (`:alias-key`, `:column-name`,
`:scope-alias`, `:scope-kind`).

---

## Worked examples

### 1. Simple CRUD + M2M (todos)

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
          :validations (:required (:max-length :max 19))
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :points
        (:type :integer :default 0
          :ui (:label "Points" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :points :agg :first)
          :column t :not-null t)
        :done
        (:type :boolean :default :false
          :ui (:label "Done" :widget :checkbox)
          :source (:view :main :column :done :agg :first)
          :column t :not-null t)
        :tags
        (:type :list
          :ui (:label "Tags" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :tags :column :name :agg :list)
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

1. **Obsolete patterns** — older notes mentioning `:checks-fn`, `:options-fn`,
   root-level `:widget`, or `:lookup-field` are dead. Use `:validations`,
   `:source-all`, and `:ui` instead.

2. **`:required` dual path** — field key `:required t` and/or
   `:validations (:required)`. Prefer the validations form (what all current
   models use).

3. **Field `:update nil`** — documented for status fields in AGENT.md but **not**
   retained by `compile-field`. Status protection is practical (read-only UI +
   `be-set-field-value`), not a compiler-enforced update block.

4. **View scope plist form** — validated, not implemented. Use `:scope :user`.

5. **Type→widget inference** — not implemented; all fields default to
   `:textbox` regardless of `:type`. Smarter inference (e.g. `:type :text`
   → `:textarea`) is an MVP Backlog item.

6. **`:type :file` validation** — models comment that `:valid-file` should
   exist; it is not implemented yet.

7. **Diamond join graphs** — unsupported.

8. **Write-through clear-to-NULL** and **transactions** — post-MVP / open.

9. **`:source-sel`** — handled in stage-2 plumbing but unused by any model;
   treat as unfinished.

10. **Joiner `:internal t`** — redundant with `:is-joiner` default; still
    conventional in examples.

11. **Bidirectional M2M list fields** — compile but stack-overflow at
    `/api/list` via `allowed-values` ↔ `be-list` recursion. Use one-way M2M
    only (see [Join tables](#join-tables-m2m)). Compiler does not reject this
    yet.

12. **Reference identity is scalar** — types used as `:target` or M2M list
    targets need exactly one `:identity t` field. Composite identity is for
    write-through / uniqueness, not checkbox-list lookup (see
    [Identity fields](#identity-fields)).

13. **No computed/composed fields** — cannot derive a stored identity
    (e.g. full name) from other columns without a lifecycle data-effect hook
    (`:compose-string` designed, not implemented).

---

## Quick key index

**Top-level:** `:title` `:name` `:version` `:domain` `:repl` `:landing-page`
`:types`

**Type:** `:table` `:create` `:update` `:delete` `:display` `:type-roles`
`:views` `:fields` `:list-form` `:add-form` `:update-form` `:tree` `:is-leaf`
`:parent-type` `:fs-backed` `:user-setting` `:suppress-roles` `:category`
`:base` `:built-in` `:internal` `:is-joiner` lifecycle slots

**View:** `:tables` `:scope`

**Field:** `:type` `:ui` `:column` `:default` `:not-null` `:unique` `:identity`
`:required` `:validations` `:source` `:source-all` `:join-table` `:target`
`:write-to` `:autofill` `:force-sql-name` `:path` `:action` `:default-from`
`:css-value` `:primary-key` / joiner `:reference`

**UI:** `:label` `:widget` `:read-only` `:precision` `:options`

**Source:** `:view` `:table` `:column` `:agg` `:scope`

**Write-to tags:** `:table` + target fields mapped to `:this` \| `:user` \|
`:value`

**Field types:** `:text` `:password` `:real` `:integer` `:boolean` `:uuid`
`:timestamp` `:list` `:file` `:button`

**Aggs:** `:first` `:list` `:distinct` `:avg` `:sum`
