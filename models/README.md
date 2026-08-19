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
- **`test/`** — test fixtures (`test-model`, `m2m-test`,
  `nullable-fk-test`, `static-select-test`, `modelbank-test`). Do not
  change these unless you are changing the test suite.

## Model resolution

`(set-model "<name>")` tries `models/<name>.lisp` first, then falls back
to `models/test/<name>.lisp`. Top-level wins on name collision.

`modelbank-test.lisp` under `test/` is the copy used by the generator
test suite.
