# Hook Registry

All custom logic in Data UI — validation and lifecycle — attaches via hooks that
reduce to one of two calling contracts. The registry is the curated,
parameterized vocabulary that makes hooks expressible as pure data (no raw code
required), enabling the AI / no-code / hosted tier.

Source: `lisp/model.lisp`, section "Hook Registry".


## Two Contracts, One Registry

The registry holds entries of two kinds. The kind discriminant determines which
contract the factory's returned function must conform to.

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

### Lifecycle contract (not yet implemented)

```
(lambda (type-key data user &key id roles record) → effect)
```

- **Returns** an effect (semantics TBD).
- **Invoked** per-record, not per-field. Receives the full write-data plist, not
  a single value.
- **Keyword args** carry call-site context:

| Site        | `:id`  | `:roles` | `:record` |
|-------------|--------|----------|-----------|
| pre-create  | —      | roles    | —         |
| post-create | new-id | roles    | —         |
| pre-update  | uuid   | roles?   | optional  |
| post-update | uuid   | roles?   | —         |
| pre-delete  | uuid   | —        | record    |
| post-delete | uuid   | —        | record    |

**Current state:** lifecycle hooks are raw functions stored directly on the
model (e.g. `:post-create ,#'add-user-settings`). They are not yet compiled
through the registry. The call sites in `be-insert`, `be-insert-internal`, and
`be-delete` use inconsistent argument shapes.


## Registry API

Defined in `lisp/model.lisp`:

| Function            | Purpose                                                |
|---------------------|--------------------------------------------------------|
| `register-hook`     | Register a named hook with kind, param schema, factory |
| `get-hook`          | Look up a hook entry by keyword name                   |
| `list-hook-names`   | List registered names, optionally filtered by kind     |
| `valid-hook-params` | Validate a plist against an entry's param schema       |
| `resolve-hook-form` | Resolve a single hook form into a function             |
| `resolve-hook-list` | Resolve a list of forms into a list of functions       |

Registry lookup is **compile-time only.** At runtime, the compiled model holds
resolved function lists — no registry access occurs.


## Hook Forms

Model authors can express hooks in three surface forms. All reduce to the same
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

### 3. Raw lambda (expert / self-host tier only)

```lisp
:validations ((lambda (type-key field-key value user)
                (when (and value (> (length value) 100))
                  "must be 100 characters or fewer.")))
```

Compiled as-is. No registry lookup. This is the escape hatch for
self-hosting developers. Not available in the AI / no-code / hosted
tier.

### Rejected forms

- `(:shell "script" ...)` — explicitly rejected with a compile error.
  Shell hooks are planned but not yet implemented.

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

Each registry entry has a parameter schema: a plist of keyword → type.
Currently supported types:

- `:integer` — parsed from integer or numeric string
- `:number` — parsed via `parse-number`

Missing or wrong-type parameters signal a validation error
(`report-ve`) at compile time.


## Compilation Pipeline

```
model source
  ↓  compile-validations (model.lisp)
  ↓  resolve-hook-list → resolve-hook-form per hook
  ↓  keyword/plist → registry lookup; lambda → compile
  ↓
*compiled-model* (holds function lists)
  ↓
runtime: validate-field-internal / be-insert / be-delete
```

`compile-validations` resolves all hook forms at compile time and stores
the resulting function lists on `*compiled-model*`. The runtime never
touches the registry.


## Lifecycle Hooks (Current State)

Lifecycle hooks are stored as raw functions on the base model, not yet
compiled through the registry.

| Slot | Base model value | Purpose |
|------|-----------------|---------|
| `:post-create` | `#'add-user-settings` | Creates a per-user settings row on user creation |
| `:pre-delete` | `#'remove-user-settings` | Cleans up settings row on user deletion |
| `:pre-update` / `:post-update` | not implemented | — |

### Current call-site signatures (inconsistent)

```
pre-create:   (funcall pre-create type-key data roles user)
post-create:  (funcall post-create nil nil data nil nil)
pre-delete:   (funcall pre-delete type-key uuid record user)
post-delete:  (funcall post-delete type-key uuid record user)
```

These are the signatures in `be-insert`, `be-insert-internal`, and
`be-delete` today. Plan 3 will unify them to the target lifecycle
contract.


## MVP Caveat: No Transactional Guarantees

Lifecycle hooks are **not** transaction-wrapped with the primary write.
A failing hook fails the operation **without rollback** of the primary
write or earlier hooks. Transactions and rollback are deliberately
deferred to post-MVP. Design hooks with that future boundary in mind;
never assume atomicity today.


## Implementation Plans

The hook registry was built in three phases:

| Plan | Status | Topic |
|------|--------|-------|
| Plan 1 | Done | Registry API + validation path + `:in-range` / `:max-length` |
| Plan 2 | Done | Write-path field validation (enforce compiled validators on insert/update) |
| Plan 3 | Not started | Lifecycle compile + unified call sites |

Plans live in `~/workbench/data-ui-hook-registry-plan-{0..3}.org`.
