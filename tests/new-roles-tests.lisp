(in-package :data-ui)

(def-suite new-roles-suite
  :description ":new-roles top-level model key tests")

(def-suite new-roles-validation-suite
  :description "Compile-time validation probes (no model needed)"
  :in new-roles-suite)

(def-suite new-roles-db-suite
  :description "set-model DB-init behavior (new-roles-test fixture)"
  :in new-roles-suite)

;;; ---------------------------------------------------------------------------
;;; Validation probes (no model, no DB)
;;; ---------------------------------------------------------------------------
;;; valid-top-level-field wants a model-shaped plist, so these probes
;;; call it with the :new-roles key on a minimal skeleton model.

(in-suite new-roles-validation-suite)

(defun th-nr-model (new-roles)
  "Build a minimal model plist with :new-roles set to NEW-ROLES."
  (list :title "NR" :name "nr" :version "0.1"
    :domain "nr.test.data-ui.com"
    :new-roles new-roles
    :types '(:widgets (:table t :fields (:name (:type :text))))))

(test new-roles-nil-accepted
  "nil :new-roles (key absent or explicit nil) is fine."
  (is (null (valid-top-level-field (th-nr-model nil) :new-roles)))
  (is (null (valid-new-roles nil))))

(test new-roles-well-formed-returned-as-author-plist
  "A well-formed value validates and is stored as the author plist
(top-level-settings stores getf, never a rewritten form)."
  (let ((value '(:ai-user ("read") :test-writer ("create" "update"))))
    (finishes (valid-new-roles value))
    (is (equal value (getf (top-level-settings (th-nr-model value))
                       :new-roles)))))

(test new-roles-non-plist-signals
  "A non-plist value signals."
  (signals error (valid-new-roles "ai-user"))
  (signals error (valid-new-roles '(:ai-user))))

(test new-roles-keyword-permissions-signal
  "Keyword permissions (:read, not \"read\") signal, naming the role
and the permission."
  (signals error (valid-new-roles '(:ai-user (:read))))
  (handler-case
    (valid-new-roles '(:ai-user (:read)))
    (error (e)
      (let ((msg (format nil "~a" e)))
        (is (search "ai-user" msg))
        (is (search "READ" msg))))))

(test new-roles-unknown-permission-signals
  "An unknown permission string signals."
  (signals error (valid-new-roles '(:ai-user ("write")))))

(test new-roles-empty-permission-list-signals
  "An empty permission list signals."
  (signals error (valid-new-roles '(:ai-user ()))))

(test new-roles-duplicate-permission-signals
  "A repeated permission in one role's list signals."
  (signals error (valid-new-roles '(:ai-user ("read" "read")))))

(test new-roles-reserved-names-signal
  "Reserved / system role names are compile errors, not no-ops."
  (loop for name in '(:admin :settings :logged-in :guest :public
                      :user-creator :role-creator :permission-creator)
    do (signals error (valid-new-roles (list name '("read"))))))

(test new-roles-reserved-exclusive-and-prefixed-signal
  "Names ending :exclusive or prefixed admin:/guest: signal."
  (signals error (valid-new-roles (list :|vip:exclusive| '("read"))))
  (signals error (valid-new-roles (list :|admin:thing| '("read"))))
  (signals error (valid-new-roles (list :|guest:thing| '("read")))))

(test new-roles-bad-name-shape-signals
  "A role keyword that downcases to an invalid role name signals."
  (signals error (valid-new-roles '(:1bad-name ("read"))))
  (signals error (valid-new-roles (list :|has:two:colons| '("read")))))

(test new-roles-string-key-signals
  "A non-keyword role name signals."
  (signals error (valid-new-roles '("ai-user" ("read")))))

(test new-roles-duplicate-keys-signal
  "Duplicate role names in the plist signal."
  (signals error
    (valid-new-roles '(:ai-user ("read") :ai-user ("update")))))

(test new-roles-via-valid-top-level-field
  "The :new-roles case in valid-top-level-field delegates to
valid-new-roles: bad value fails through the model-level entry, nil
passes."
  (signals error
    (valid-top-level-field (th-nr-model '(:ai-user (:read))) :new-roles))
  (finishes
    (valid-top-level-field (th-nr-model '(:ai-user ("read"))) :new-roles))
  (finishes
    (valid-top-level-field (th-nr-model nil) :new-roles)))

;;; ---------------------------------------------------------------------------
;;; DB-init behavior (new-roles-test fixture)
;;; ---------------------------------------------------------------------------
;;; The fixture declares :ai-user ("read"), :test-reader ("read"),
;;; :test-writer ("create" "update"). :type-roles on :widgets names
;;; widgets-user, test-reader, ai-user — so test-reader's declared
;;; ("read") must win over ensure-model-roles' full-CRUD default
;;; (D4 ordering). :test-writer appears in no :type-roles.

(in-suite new-roles-db-suite)

(defun th-nr-sorted-perms (role)
  "Declared permission names for ROLE, sorted for comparison."
  (sort (copy-list (a:list-role-permission-names *rbac* role))
    #'string<))

(test new-roles-all-declared-roles-exist
  "All three declared roles exist after set-model."
  (loop for role in '("ai-user" "test-reader" "test-writer")
    do (is (a:get-id *rbac* "roles" role)
           "Role ~a should exist after set-model" role)))

(test new-roles-ai-user-has-read-only
  ":ai-user has exactly (\"read\")."
  (is (equal '("read") (th-nr-sorted-perms "ai-user"))))

(test new-roles-test-writer-has-create-update
  ":test-writer has exactly (\"create\" \"update\")."
  (is (equal '("create" "update") (th-nr-sorted-perms "test-writer"))))

(test new-roles-admin-has-declared-roles
  "rbac add-role links new roles to admin, so admin has them."
  (loop for role in '("ai-user" "test-reader" "test-writer")
    do (is (a:user-has-role *rbac* "admin" role)
           "Admin should have role ~a" role)))

(test new-roles-idempotent-second-set-model
  "A second set-model leaves permissions unchanged (no duplicates,
no rewrite)."
  (let ((before (mapcar #'th-nr-sorted-perms
                   '("ai-user" "test-reader" "test-writer"))))
    (set-model "new-roles-test")
    (let ((after (mapcar #'th-nr-sorted-perms
                   '("ai-user" "test-reader" "test-writer"))))
      (is (equal before after)))))

(test new-roles-declared-permissions-win-over-type-roles
  "D4 ordering: test-reader is on the type-widgets resource and has
exactly (\"read\"), not ensure-model-roles' full-CRUD default."
  (is (member "test-reader"
        (a:list-resource-role-names *rbac* "type-widgets")
        :test #'equal))
  (is (equal '("read") (th-nr-sorted-perms "test-reader"))))

(test new-roles-declared-roles-in-selectable-roles
  "Declared roles appear in admin's selectable-roles palette."
  (let ((palette (selectable-roles :widgets "admin")))
    (loop for role in '("ai-user" "test-reader" "test-writer")
      do (is (member role palette :test #'equal)
           "Palette should contain ~a" role))))

(test new-roles-unreferenced-role-still-exists
  ":test-writer is in no :type-roles but still exists."
  (is (a:get-id *rbac* "roles" "test-writer"))
  (is (not (member "test-writer"
             (a:list-resource-role-names *rbac* "type-widgets")
             :test #'equal))))
