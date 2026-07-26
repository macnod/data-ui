# Hook Registry

All custom logic in Data UI — validation, lifecycle, and actions — attaches via
hooks that reduce to one of three calling contracts. The registry is the
curated, parameterized vocabulary that makes hooks expressible as pure data (no
raw code required), enabling the AI / no-code / hosted tier.

Source: `lisp/model.lisp`, section "Hook Registry".


## Three Contracts, One Registry

The registry holds entries of three kinds. The kind discriminant determines
which contract the factory's returned function must conform to.

### Validation contract

```
(lambda (type-key field-key value user) → nil | error-string)
```

- **Returns** `nil` when the value is valid, or a human-readable error string
  when it is not.
- **Invoked** per-field by `validate-field-internal` (backend.lisp), which loops
  over the compiled `:validations` list for the field and collects all non-nil
  results.
- **Never signals.** Returning a string is the failure path; the caller
  aggregates.

### Lifecycle contract

```
(lambda (type-key data user &key id roles record) → ignored)
```

- **Returns** are ignored today (no effect protocol yet).
- **Invoked** per-record, not per-field. Receives the full write-data plist, not
  a single value.
- **Keyword args** carry call-site context:

| Site        | `:id`  | `:roles` | `:record` |
|-------------|--------|----------|-----------|
| pre-create  | —      | roles    | —         |
| post-create | new-id | roles    | —         |
| pre-update  | uuid   | roles    | record    |
| post-update | uuid   | roles    | —         |
| pre-delete  | uuid   | —        | record    |
| post-delete | uuid   | —        | record    |

All six lifecycle slots are compiled at model-compile time into function lists
on `*compiled-model*`. The runtime calls them via `run-lifecycle-hooks`
(backend.lisp) — no registry lookup occurs at runtime.


### Action contract

```
(lambda (type-key field-key record user
         &key roles status-field set-status)
  → nil | plist)
```

- **Returns** `nil` for sync completion, or a plist like
  `(:async t :message "Deploy started")` for async operations.
- **Invoked** by `be-action` (backend.lisp) when a user clicks a `:button`
  field on the update form.
- **Keyword args** carry call-site context:

| Arg | Meaning |
|-----|---------|
| `roles` | Reserved on the contract; **`be-action` does not pass it today** (always default/`nil`). Do not rely on it in MVP hooks. |
| `status-field` | Keyword of the companion status column (e.g. `:deploy-status`) |
| `set-status` | `(lambda (message) ...)` — sole way for hooks to write status |

- **Status protocol:** the framework sets `"running"` before calling the hook.
  For sync hooks (no `:async t`), the framework auto-sets `"complete"` on
  success or `"failed: <message>"` on error. For async hooks, the worker must
  call `set-status` with a terminal value.
- **Never call** `be-update` or direct SQL from inside an action hook to write
  status — use `set-status` only.

Action hooks are compiled at model-compile time and stored on the compiled
field definition as `:compiled-hook`. The runtime calls them via `be-action` —
no registry lookup occurs at runtime.


## Registry API

Defined in `lisp/model.lisp`:

| Function            | Purpose                                                |
|---------------------|--------------------------------------------------------|
| `register-hook`     | Register a named hook with kind (`:validation`, `:lifecycle`, `:action`), param schema, factory |
| `get-hook`          | Look up a hook entry by keyword name                   |
| `list-hook-names`   | List registered names, optionally filtered by kind     |
| `valid-hook-params` | Validate a plist against an entry's param schema       |
| `resolve-hook-form` | Resolve a single hook form into a function             |
| `resolve-hook-list` | Resolve a list of forms into a list of functions       |

Registry lookup is **compile-time only.** At runtime, the compiled model holds
resolved function lists — no registry access occurs.


## Hook Forms

Model authors can express hooks in two surface forms. All reduce to the same
contract before anything runs.

### 1. Keyword (zero-arg registry entry)

```lisp
:validations (:required :email)
```

The keyword names a registry entry with no parameters. The factory is called
with no arguments.

### 2. Plist list (parameterized registry entry)

```lisp
:validations ((:max-length :max 20)
              (:in-range :min 1 :max 5))
```

The first element names the registry entry; the remaining plist provides
parameters. `valid-hook-params` validates the plist against the entry's
parameter schema before the factory runs.

The registry is the sole hook surface form. Raw lambda forms and shell
hooks are not accepted. For expert/self-host needs, register a custom
hook via `register-hook`.

## Registered Validation Hooks

| Name | Parameters | Behavior |
|------|------------|----------|
| `:required` | none | Rejects empty/nil values |
| `:user-name` | none | Validates username format |
| `:password` | none | Validates password policy |
| `:email` | none | Validates email format |
| `:join-items-exist` | none | Validates join-table references exist |
| `:exists` | none | Validates a referenced record exists |
| `:max-length` | `:max` (integer) | Inclusive string length ≤ max |
| `:in-range` | `:min` (integer), `:max` (integer) | Inclusive numeric range |

Range and length validators are no-ops on empty/nil values. Use
`:required` separately to enforce presence.


## Parameter Schema

Each registry entry has a parameter schema: a plist of keyword → type tag.
`valid-hook-params` special-cases:

- `:integer` — parsed from integer or numeric string
- `:number` — parsed via `parse-number`

Any other type tag (including `:keyword`, used by `:deploy-model`) is
**pass-through**: the raw value is accepted unchanged, with no type check.
Missing parameters still signal a validation error (`report-ve`) at compile
time.


## Compilation Pipeline

```
model source
  ↓  compile-validations → resolve-hook-list (:kind :validation)
  ↓  compile-lifecycle-hooks → resolve-hook-list (:kind :lifecycle)
  ↓  compile-field (per :button) → resolve-hook-form (:kind :action)
  ↓  resolve-hook-form per hook:
       keyword/plist → registry lookup
       raw function → pass through as-is (internal base-model use)
  ↓
*compiled-model* (holds function lists for validations, lifecycle, and actions)
  ↓
runtime: validate-field-internal / run-lifecycle-hooks / be-action
```

`compile-validations` and `compile-lifecycle-hooks` resolve hook forms at
compile time into function lists on `*compiled-model*`. Action hooks are
resolved inline in `compile-field` (when `:type :button` and `:action` are
present) and stored on the field as `:compiled-hook`. The runtime never
touches the registry.


## Lifecycle Hooks

Lifecycle slots are compiled at model-compile time via
`compile-lifecycle-hooks` (model.lisp). All six slots are resolved into
function lists and stored on the compiled type definition, overriding the
raw model values.

| Slot | Base model value | Purpose |
|------|-----------------|---------|
| `:post-create` | `#'add-user-setting-rows` | Creates a per-user settings row on user creation |
| `:pre-delete` | `#'remove-user-setting-rows` | Cleans up settings row on user deletion |
| `:pre-create` | — | — |
| `:post-delete` | — | — |
| `:pre-update` | — | — |
| `:post-update` | — | — |

### Runtime invocation

All call sites use `run-lifecycle-hooks` (backend.lisp), which iterates
the compiled function list and calls each hook with the unified contract:

```
(run-lifecycle-hooks hooks type-key data user
  &key id roles record)
```

Call sites:

| Function | Slots invoked |
|----------|--------------|
| `be-insert` | `:pre-create` (before write), `:post-create` (after write, with `:id new-id`) |
| `be-insert-internal` | `:post-create` (after write, with `:id new-id`) |
| `be-update` | `:pre-update` (before write), `:post-update` (after write-through) |
| `be-delete` | `:pre-delete` (before write), `:post-delete` (after write) |

### Surface forms on lifecycle slots

Same as validation — both forms are accepted:

```lisp
;; Keyword (zero-arg registry entry, :lifecycle kind)
:post-create :my-hook

;; Plist list (parameterized registry entry)
:post-update (:my-hook :param value)

;; List of hooks (both forms may be mixed)
:post-create (:hook-a (:hook-b :param 1))
```

Internal base-model lifecycle hooks use compiled function references
(`#'foo`) which pass through `resolve-hook-form` as-is. This is an
internal mechanism, not a model-author surface form.


## Action Hooks

Action hooks attach to `:button` fields and execute when a user clicks the
button on the update form. They are resolved at model-compile time inside
`compile-field` (model.lisp) via `resolve-hook-form`. The resolved hook
function is stored on the compiled field definition as `:compiled-hook`.

### Field authoring

```lisp
:deploy
(:type :button
  :ui (:label "Deploy Model" :widget :button)
  :action (:deploy-model :field :model))
```

- `:type :button` — no storage column.
- `:action` — a single registry form `(:keyword args...)`.
- `:action` is valid **only** on `:type :button` (compile-time error otherwise).
- `:ui` must include `:widget :button`.

### Status field (auto-synthesized)

Each `:button` field gets a companion `:<field>-status` column:

| Property | Value |
|----------|-------|
| Type | `:text` |
| Column | `t` |
| Default | `"idle"` |
| Not-null | `t` |
| UI | `(:label "<Button> Status" :widget :textbox :read-only t)` |
| Source | `(:view :main :column <status-key> :agg :first)` |

Status writes go through `be-set-field-value` only (the action path). The
field is read-only in the UI.

Status vocabulary: `idle` → `running` → `complete` | `failed: <reason>`.

The compiler auto-includes the status field on `:update-form` when the button
is listed there. If the status key already exists as an author-declared field,
compilation fails.

### Placement

Buttons appear on the **update form only**. `fe-fields` excludes `:button`
fields from `:list-form` and `:add-form`.

### Runtime invocation

`be-action` (backend.lisp) is the sole runtime entry point:

1. Validates type, record, field is `:button` with a compiled hook.
2. Checks type-level `update` permission + record-level access.
3. Reads current status; rejects if `"running"` (in-progress guard).
4. Sets status to `"running"` via `be-set-field-value`.
5. Calls the hook with the action contract.
6. Sync success → sets `"complete"`. Sync error → sets `"failed: <msg>"`.
   (A result plist with `:status "failed"` is also treated as sync failure.)
7. Async (`:async t` in result) → returns immediately; worker sets terminal
   status via `set-status`.

REST endpoint: `POST /api/actions` with `{"type", "id", "field"}`.

### Registered Action Hooks

| Name | Parameters | Behavior |
|------|------------|----------|
| `:deploy-model` | `:field` (keyword) | Reads model text from the record's `:field`, validates in-process via `validate-model`. On validation failure returns `(:status "failed" :message …)` immediately (no worker). On success spawns an async worker that writes the model file, commits, shells out to `scripts/data-ui deploy`, and records the admin password in `:secrets`. Returns `(:async t :message "Deploy started")`. |


## MVP Caveat: No Transactional Guarantees

Lifecycle hooks and action hooks are **not** transaction-wrapped with the
primary write. A failing hook fails the operation **without rollback** of the
primary write or earlier hooks. Transactions and rollback are deliberately
deferred to post-MVP. Design hooks with that future boundary in mind;
never assume atomicity today.

Action hooks have an additional caveat: if the process restarts while an
async action is `running`, the status remains `running` forever. There is no
job queue or reconciler in MVP. An operator must reset the status manually.
