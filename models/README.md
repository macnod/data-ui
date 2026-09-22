# Models

The files in this directory define applications.

By convention, each model file is named after the model it defines
(e.g., `todos.lisp` defines the "todos" model). Load a model with
`(set-model "todos")` — pass just the file name, with no path and no
`.lisp` extension.

## Layout

- **Top level** — real example models (`todos`, `modelbank`, `parts`,
  `file-server`, etc.). These are the models shown by `list-models` and
  used by the deploy pipeline.
- **`local/`** — locally deployed (VIP) models, written by the
  `:deploy-model` hook; gitignored. Shadows a same-named `test/`
  fixture.
- **`test/`** — test fixtures (`test-model`, `m2m-test`,
  `nullable-fk-test`, `static-select-test`, `modelbank-test`, and many
  more — see the directory). Do not change these unless you are
  changing the test suite.

## Model resolution

`(set-model "<name>")` tries `models/<name>.lisp` first, then
`models/local/<name>.lisp`, then falls back to `models/test/<name>.lisp`.
Top level wins on name collision.

`modelbank-test.lisp` under `test/` is the copy used by the generator
test suite.

## Domains

Models that are exposed carry two FQDN keys: `:domain` (production,
consumed by `deploy`) and `:domain-stg` (staging, consumed by
`profile expose`). Omit `:domain-stg` and the compiler derives it by
suffixing `-stg` onto the first DNS label of `:domain`
(`todo.demo.data-ui.com` → `todo-stg.demo.data-ui.com`); an explicit
value always wins. House convention: write `:domain-stg` explicitly
(even when it equals the derived value) so readers never guess. See
`docs/deployment.md` → Environments.
