---
description: "Use the eval-in-data-ui function to evaluate Common Lisp forms against a running Data UI SBCL REPL via Slime. Only available when working in the data-ui project."
name: eval-in-data-ui
---
You have access to an Elisp function called `eval-in-data-ui` that
evaluates Common Lisp forms in the context of a running Data UI
instance.

**Function signature:**
`(eval-in-data-ui FORM &optional TIMEOUT)` where FORM is a string
containing a Common Lisp expression and TIMEOUT is an optional
maximum wait in seconds (default 10).

**Return format:**
Returns a list of three strings: `(OUTPUT VALUES-STRING ERROR-STRING)`.

- On success: `("printed-output" "values" nil)` — OUTPUT captures
  anything written to `*standard-output*`, VALUES is the
  `prin1`-ed return value(s), ERROR is `nil`.
- On error: `("" "" "TYPE: message")` — the error is returned as a
  string, never propagated to the Swank debugger.
- On timeout: `("" "" "TIMEOUT after N seconds")`.
- On connection failure: `("" "" "CONNECTION ERROR: ...")`.

**Requirements:**
- A `*slime-repl sbcl*` buffer must exist and be connected to the
  Data UI instance.
- The function evaluates in the `:data-ui` package, so package-local
  nicknames (e.g., `a:` for the rbac library) are available.
- On first call after connecting, `eval-safely` is auto-installed on
  the Lisp side (lazy initialization with fallback).

**Example usage:**
```elisp
(eval-in-data-ui "(+ 1 2)")
;; => ("" "3" nil)

(eval-in-data-ui "(format t \"hello\")")
;; => ("hello" "NIL" nil)

(eval-in-data-ui "(a:list-role-names *rbac*)")
;; => ("" "(\"admin\" \"admin:exclusive\" ...)" nil)

(eval-in-data-ui "(error \"test\")")
;; => ("" "" "SIMPLE-ERROR: test")

(eval-in-data-ui "(sleep 100)" 2)
;; => ("" "" "TIMEOUT after 2 seconds")
```

**Limitations:**
- This function only works when a Slime REPL to the Data UI SBCL
  instance is active.
- Prefer read-only inspection; use caution with state-mutating forms.
- Lisp source outside `web/` must not be modified without explicit
  human permission.
- Catastrophic conditions (heap exhaustion, memory faults, segfaults)
  cannot be caught and will break the connection.

When the user asks you to inspect Data UI runtime state, model
contents, RBAC configuration, or backend behavior, use this function
via the `Eval` tool.
