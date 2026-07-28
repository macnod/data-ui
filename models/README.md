# Models

The files in this directory define applications.

By convention, each model file is named after the model it defines (e.g., `todos.lisp` defines the "todos" model). Load a model with `(set-model "todos")` — pass just the file name, with no path and no `.lisp` extension. The file `test-model.lisp` is an exception to this naming rule.

The file `test-model.lisp` is used by the test suite. Do not change this file unless you plan on altering the test suite.

