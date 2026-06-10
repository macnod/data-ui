# Models

The files in this directory define applications.

By convention, each model file is named after the model it defines (e.g., `todos.lisp` defines the "todos" model). Load a model with `(set-model "todos")` — pass just the file name, with no path and no `.lisp` extension. The files `default-model.lisp` and `test-model.lisp` are exceptions to this naming rule.

The file `test-model.lisp` is used by the test suite. Do not change this file unless you plan on altering the test suite.

The file `default-model.lisp` is the model loaded when the Lisp image starts in deployment mode. For any deployed environment, `default-model.lisp` is what runs.

When developing a model for deployment, keep it in a file named after the model. Once the model is stable, copy it to `default-model.lisp`. There should always be a named model file whose contents match `default-model.lisp`.
