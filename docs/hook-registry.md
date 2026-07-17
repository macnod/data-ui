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

### Lifecycle contract

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
| pre-update  | uuid   | roles    | record    |
| post-update | uuid   | roles    | —         |
| pre-delete  | uuid   | —        | record    |
| post-delete | uuid   | —        | record    |

All six lifecycle slots are compiled at model-compile time into function lists
on `*compiled-model*`. The runtime calls them via `run-lifecycle-hooks`
(backend.lisp) — no registry lookup occurs at runtime.


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

Each registry entry has a parameter schema: a plist of keyword → type.
Currently supported types:

- `:integer` — parsed from integer or numeric string
- `:number` — parsed via `parse-number`

Missing or wrong-type parameters signal a validation error
(`report-ve`) at compile time.


## Compilation Pipeline

```
model source
  ↓  compile-validations → resolve-hook-list (:kind :validation)
  ↓  compile-lifecycle-hooks → resolve-hook-list (:kind :lifecycle)
  ↓  resolve-hook-form per hook:
       keyword/plist → registry lookup
       raw function → pass through as-is (internal base-model use)
  ↓
*compiled-model* (holds function lists for both validations and lifecycle)
  ↓
runtime: validate-field-internal / run-lifecycle-hooks
```

Both `compile-validations` and `compile-lifecycle-hooks` resolve all hook
forms at compile time and store the resulting function lists on
`*compiled-model*`. The runtime never touches the registry.


## Lifecycle Hooks

Lifecycle slots are compiled at model-compile time via
`compile-lifecycle-hooks` (model.lisp). All six slots are resolved into
function lists and stored on the compiled type definition, overriding the
raw model values.

| Slot | Base model value | Purpose |
|------|-----------------|---------|
| `:post-create` | `#'add-user-settings` | Creates a per-user settings row on user creation |
| `:pre-delete` | `#'remove-user-settings` | Cleans up settings row on user deletion |
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


## MVP Caveat: No Transactional Guarantees

Lifecycle hooks are **not** transaction-wrapped with the primary write.
A failing hook fails the operation **without rollback** of the primary
write or earlier hooks. Transactions and rollback are deliberately
deferred to post-MVP. Design hooks with that future boundary in mind;
never assume atomicity today.
