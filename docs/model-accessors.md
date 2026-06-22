# Model Accessors — Retention Notes

These function families in `lisp/model.lisp` have **no in-codebase callers** but
are intentionally retained. Do not remove them in dead-code sweeps.

## `model-*-for` — Debugging / REPL Accessors

Convenience wrappers around `u:tree-get` on `*compiled-model*`. They exist for
REPL inspection and ad-hoc debugging — printing a type's compiled definition,
inspecting generated SQL, checking field attributes, etc.

| Function | Purpose |
|---|---|
| `model-for` | Get the full compiled definition for a type |
| `model-views-for` | Get all views for a type |
| `model-view-for` | Get a specific named view for a type |
| `model-field-for` | Get a specific field definition for a type |
| `model-sql-for` | Get all generated SQL (create/insert/update/delete) for a type |
| `model-fields-for` | Get all field definitions for a type |
| `model-view-sql-for` | Get the SELECT SQL for a specific view (default `:main`) |
| `model-field-names-for` | Get specific attributes (e.g. `:name-sql`, `:alias-key`) for all fields |
| `model-field-create-sql-for` | Get the column DDL for all fields of a type |
| `model-field-attribute-for` | Get a single attribute across all fields of a type |

Example REPL usage:

```lisp
(model-view-sql-for :todos)
;; => "select rt_todos.id as todos_id, rt_todos.name as todos_name, ..."

(model-field-attribute-for :todos :type)
;; => (:name :text :completed :boolean :created-at :timestamp ...)
```

## `model-` Top-Level Accessors — Used by Deployment Scripts

These read top-level model metadata (`:name`, `:title`, `:version`, `:domain`,
`:repl`) from `*top-level-settings*` (populated during `set-model`). They are
used at runtime by deployment tooling.

| Function | Model Key |
|---|---|
| `model-title` | `:title` |
| `model-name` | `:name` |
| `model-version` | `:version` |
| `model-domain` | `:domain` |
| `model-repl` | `:repl` (defaults to nil) |

The `scripts/data-ui` deploy script reads the same fields from the raw model
file *before* the system is loaded (via `get_model_field` →
`top-level-model-field`), but these Lisp accessors serve the equivalent purpose
at runtime. Example from `scripts/data-ui`:

```bash
function gather_deploy_facts {
    NAME=$(model_field name)
    TITLE=$(model_field title)
    VERSION=$(model_field version)
    DOMAIN=$(model_field domain)
    REPL=$(model_field repl)
    ...
    TAG="${NAME}-${VERSION}-${SHORT_HASH}"
    IMAGE="${IMAGE_REPO}:${TAG}"
}
```
