---
description: "Use the eval-in-data-ui function to evaluate Common Lisp forms against a running Data UI SBCL REPL via Slime. Only available when working in the data-ui project."
name: eval-in-data-ui
---
You have access to an Elisp function called `eval-in-data-ui` that evaluates Common Lisp forms in the context of a running Data UI instance.

**Function signature:**
`(eval-in-data-ui FORM)` where FORM is a string containing a Common Lisp expression.

**Requirements:**
- A `*slime-repl sbcl*` buffer must exist and be connected to the Data UI instance.
- The function evaluates in the `:data-ui` package, so package-local nicknames (e.g., `a:` for the rbac library) are available.
- Use this for inspecting live state: compiled model, RBAC roles/users, hook behavior, etc.

**Example usage:**
```elisp
(eval-in-data-ui "(a:list-role-names *rbac*)")
;; => ("admin" "admin:exclusive" "guest:exclusive" "logged-in" "parts" "public" "settings")
```

**Limitations:**
- This function only works when a Slime REPL to the Data UI SBCL instance is active.
- Prefer read-only inspection; use caution with state-mutating forms.
- Lisp source outside `web/` must not be modified without explicit human permission.

When the user asks you to inspect Data UI runtime state, model contents, RBAC configuration, or backend behavior, use this function via the `Eval` tool.