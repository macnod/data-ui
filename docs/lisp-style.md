# Lisp Style and Error Reporting

Normative for all Common Lisp written in this repository — engine code (written by humans) and test/helper code (which agents may write). Read this before writing any Lisp here.

## Coding Style

- **Prefer `u:` functions (from `dc-eclectic`) over raw CL or `uiop:` equivalents.** The `dc-eclectic` library (nickname `u:`) provides cleaner, more ergonomic wrappers for common operations. Use them whenever an equivalent exists. Examples: `u:getenv` (not `uiop:getenvp`), `u:join-paths` (not `merge-pathnames`), `u:slurp` (not `with-open-file` + read loops), `u:spew` (not `with-open-file` + write). When writing new Lisp code, check whether `u:` has a suitable function before reaching for the standard library. The library covers environment access (`u:getenv`, `u:setenv`), filesystem (`u:file-exists-p`, `u:directory-exists-p`, `u:copy-file`, `u:file-extension`, `u:file-name-only`, `u:path-only`, `u:path-parent`), plist utilities (`u:plistp`, `u:plist-keys`, `u:plist-values`, `u:tree-get`), string operations (`u:trim`, `u:split-n-trim`, `u:starts-with`, `u:ends-with`, `u:make-keyword`), collections (`u:distinct-values`, `u:distinct-strings`, `u:safe-sort`, `u:has`, `u:has-some`, `u:deep-copy`, `u:singular`, `u:plural`), and shell commands (`u:shell-command-` family). This is a partial list; browse the source in the `dc-eclectic` Quicklisp local project for the full API.
- **Prefer `u:tree-get` over nested `getf` calls.** When accessing deeply nested plist values, use `(u:tree-get tree :a :b :c)` instead of `(getf (getf (getf tree :a) :b) :c)`. It is cleaner, more readable, and consistent with the `u:` preference above.
- **Prefer explicit parameter passing over dynamic (special) variables.** Dynamic variables (`*foo*`) are reserved for values that are truly global to the entire system (e.g. `*compiled-model*`, `*rbac*`). Request-scoped or function-chain-scoped values (e.g. the current user's ID during a `be-list` call) must be threaded as parameters, not bound dynamically. Dynamic binding is technically thread-safe in SBCL, but it creates hidden coupling: the reader must know a variable is special, find where it's bound, and trace its extent. Explicit parameters make data flow visible at the call site.
- **Prefer small, single-purpose functions.** Every function should be short enough to grasp at a glance — roughly half a page or less. When a function grows beyond that, extract named sub-functions even if they're only called once. A descriptive function name documents intent better than inlined code, and the reader sees a simple outline rather than a wall of logic. The complexity of Data UI lives in the architecture and the interaction of its parts — never in any individual function.

## Error Reporting: `report-e` and `report-ve`

Never use raw `(error ...)` calls. Use the two macros defined in `lisp/aux.lisp` instead:

- **`report-e`** — for system/structural errors (unknown hooks, wrong kind, unsupported features, model compilation failures). Calls `error` under the hood.
- **`report-ve`** — for validation errors (bad user input, invalid parameters, schema violations). Signals a validation error condition under the hood.

Both generate a **Guru Meditation Number** (a deterministic 6-hex-hash of the function name + 3 random hex digits, e.g. `897270-ff3`) that is appended to the error message and logged. The function name can be recovered from the hash via `gmn-fname`.

### Signature

Both macros share the same form:

```lisp
(report-ve function-name format-string &rest var-specs)
(report-e  function-name format-string &rest var-specs)
```

- **`function-name`** — string, the name of the calling function (e.g. `"valid-hook-params"`).
- **`format-string`** — a `format` directive string. Use `~a` (not `~s`) for cleaner error messages; raw values are logged separately. **Exception:** use `~s` for keyword symbols and keys (e.g. type keys, field keys) so they display with their leading colon (e.g. `:FOO-STATUS` rather than `FOO-STATUS`), making them visually identifiable as keys.
- **`var-specs`** — symbols (variable names), **not** expressions.

### The tilde convention

Each var-spec is a symbol. If the symbol is prefixed with `~`, it is included as a `format` argument (the tilde is stripped). If not prefixed, it is **log-only** — it appears in the `pl:plog` entry but is not interpolated into the error message.

This lets you provide extra debugging context to the log without cluttering the error shown to the user:

```lisp
(report-ve "valid-hook-params"
           "Hook ~a parameter ~a must be an integer, got ~a"
           ~hook-name ~key ~val)
```

All three vars are logged as `:hook-name`, `:key`, `:val`. All three are also format arguments (all have tildes).

```lisp
(report-ve "valid-filter"
           "Invalid field key ~a for type ~a."
           ~field-key ~type-key request-id)
```

Here `request-id` is logged but not shown in the error message.

### Practical constraints

- Var-specs must be **symbols** (lexical variable names), not arbitrary expressions. Bind expressions to local variables first.
- The log key is derived from the cleaned symbol name (tilde stripped), converted to a keyword (e.g. `~hook-name` → `:hook-name`).
- When choosing between `report-e` and `report-ve`: if the error is validating user-supplied input against a schema or contract, use `report-ve`. If it's a structural/system error (something is wrong with the model or code path), use `report-e`.

### Naming convention: `valid-*`

Functions that check or validate data are named with the `valid-` prefix (e.g. `valid-hook-params`, `valid-target`, `valid-view-scope`). Do not use `check-` or other prefixes for this purpose. The `valid-*` family has mixed return semantics — some signal on error and return `nil` otherwise, others return a resolved value — but they share the common purpose of validating input against a schema or contract.
