(in-package :data-ui)

(def-suite new-directory-suite
  :description "Regression suite for the valid-new-directory report-ve
messages: the ~{~a^~, ~} separator typo that made the error path die in
FORMAT, and the wrong var-spec bound to the \"Requested roles\" directive
in both roles messages (the full request rendered instead of the excess).")

(in-suite new-directory-suite)

;; Probes recorded live (2026-10-17, modelbank-test fixture):
;;   root "/" roles: ("admin" "admin:exclusive" "directories-user")
;;   (a:exclusive-role-for "user-x") => "user-x:exclusive"
;;   /child/ roles (seeded with the exclusive role at insert):
;;     ("admin" "directories-user" "user-x:exclusive")
;; list-resource-role-names / set-difference ordering is unstable, so
;; assertions use search on single-element excesses, never joined lists.

(defun nd-message (form)
  "Run FORM, expect a validation-error, return its rendered text."
  (handler-case
      (progn
        (funcall form)
        (fail "Expected a validation-error"))
    (validation-error (e) (princ-to-string e))))

(test broken-format-regression
  "The non-parent-roles message signals a validation-error (not a
FORMAT error) and names the excess roles, not the full request."
  (let ((text (nd-message
                (lambda ()
                  (be-insert :directories '(:name "/garbage/") "admin"
                    :roles '("directories-user" "public"))))))
    ;; The intended message, not a FORMAT death.
    (is-true (search "Can't assign roles that parent doesn't have." text))
    (is-false (search "error in FORMAT" text))
    (is-false (search "~{" text))
    ;; Parent roles render (joined, order-stable content check).
    (is-true (search "directories-user" text))
    ;; "Requested roles" is the excess alone (the superset request is
    ;; what distinguishes the fixed binding from the old one).
    (is-true (search "Requested roles: public" text))
    (is-false (search "Requested roles: directories-user" text))))

(test happy-path-control
  "A request that is a subset of the parent roles succeeds and cleans
up after itself."
  (multiple-value-bind (id inserted)
      (be-insert :directories '(:name "/ok-dir/") "admin"
        :roles '("directories-user"))
    (is-true id)
    (is-true inserted)
    (be-delete :directories
      '((:directories :name :eq "/ok-dir/")) "admin")))

(test non-user-roles-message
  "The non-user-roles message fires end-to-end via a shareable
exclusive role: user-y requests user-x's exclusive role on a child
directory user-x seeded with that role at its own insert."
  (th-make-user "user-x" :roles '("directories-user"))
  (th-make-user "user-y" :roles '("directories-user"))
  ;; Root roles are exactly (type-role + admin roles added by
  ;; update-roles during the root's own insert); type roles do not
  ;; reach the resource. Grant user-x's exclusive role to "/" itself
  ;; so the /child/ seed below passes validation (its request must be
  ;; a subset of "/"'s roles at check time).
  (a:add-resource-role *rbac*
    (find-resource-name :directories '((:directories :name :eq "/")))
    "user-x:exclusive")
  ;; Seed: user-x requests his own shareable exclusive role alongside
  ;; the type role, so /child/ carries user-x:exclusive from the
  ;; moment it exists (validation of later children sees exactly this
  ;; list; update-roles additions land only after validation passed).
  (be-insert :directories '(:name "/child/") "user-x"
    :roles '("directories-user" "user-x:exclusive"))
  ;; Confirm the seeded state at the moment validation will see it.
  (is-true (member "user-x:exclusive"
             (a:list-resource-role-names *rbac*
               (find-resource-name :directories
                 '((:directories :name :eq "/child/"))))
             :test #'equal))
  (let ((text (nd-message
                (lambda ()
                  (be-insert :directories '(:name "/child/sub/") "user-y"
                    :roles '("directories-user" "user-x:exclusive"))))))
    (is-true (search "Can't assign roles that user doesn't have." text))
    (is-false (search "error in FORMAT" text))
    (is-false (search "~{" text))
    (is-true (search "Requested roles: user-x:exclusive" text))
    (is-false (search "Requested roles: directories-user" text)))
  ;; The failing call signals before store-directory, so only /child/
  ;; needs cleanup. Users and the exclusive role row survive; that is
  ;; harmless and keeps tests re-runnable (th-make-user re-adds roles).
  (be-delete :directories
    '((:directories :name :eq "/child/")) "admin"))
