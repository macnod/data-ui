(in-package :data-ui)

(def-suite change-password-suite
  :description ":change-password action hook tests (base model).")

(in-suite change-password-suite)

(defun cp-settings-id (user)
  "Settings row ID for USER (created via th-make-user before calling)."
  (let ((id (be-value-id :settings :user user "admin")))
    (is-true id "Precondition: settings row for ~a exists" user)
    id))

(defun cp-clean-user (user)
  "Delete USER (be-delete runs remove-user-setting-rows). Safe when absent."
  (let ((id (a:get-id *rbac* "users" user)))
    (when id (be-delete :users id "admin"))))

(test change-password-happy-path
  "A correct current password plus a valid new password changes the
password; the old one stops working."
  (cp-clean-user "cp-alice")
  (th-make-user "cp-alice")
  (unwind-protect
    (let* ((id (cp-settings-id "cp-alice"))
           (result (be-action :settings id :change-password "cp-alice"
                      '(:current-password "password-1"
                        :new-password "new-pass-9"))))
      (is (equal (getf result :status) "complete"))
      (is-true (a:login *rbac* "cp-alice" "new-pass-9"))
      (is-false (a:login *rbac* "cp-alice" "password-1")))
    (cp-clean-user "cp-alice")))

(test change-password-status-transition
  "Success writes complete to the companion status column."
  (cp-clean-user "cp-alice")
  (th-make-user "cp-alice")
  (unwind-protect
    (let* ((id (cp-settings-id "cp-alice")))
      (be-action :settings id :change-password "cp-alice"
        '(:current-password "password-1" :new-password "new-pass-9"))
      (let ((status (getf (getf (rec id "admin" :type-key :settings)
                            :record)
                     :change-password-status)))
        (is (equal status "complete"))))
    (cp-clean-user "cp-alice")))

(test change-password-wrong-current
  "A wrong current password fails with the intended message and does
not change the password."
  (cp-clean-user "cp-alice")
  (th-make-user "cp-alice")
  (unwind-protect
    (let* ((id (cp-settings-id "cp-alice"))
           (result (be-action :settings id :change-password "cp-alice"
                      '(:current-password "wrong-password"
                        :new-password "new-pass-9"))))
      (is (equal (getf result :status) "failed"))
      (is (equal (getf result :message)
                 "Current password is incorrect"))
      (is-true (a:login *rbac* "cp-alice" "password-1"))
      (is-false (a:login *rbac* "cp-alice" "new-pass-9")))
    (cp-clean-user "cp-alice")))

(test change-password-blank-fields
  "Blank, nil, or absent current or new password fails with the
required message — not the misleading login-failure message (the
frontend normalizes untouched boxes to empty strings)."
  (cp-clean-user "cp-alice")
  (th-make-user "cp-alice")
  (unwind-protect
    (let* ((id (cp-settings-id "cp-alice"))
           (cases '(("blank current"
                      (:current-password "" :new-password "new-pass-9"))
                   ("blank new"
                      (:current-password "password-1" :new-password ""))
                   ("absent current" (:new-password "new-pass-9"))
                   ("absent new" (:current-password "password-1")))))
      (loop for (label data) in cases
        for result = (be-action :settings id :change-password
                        "cp-alice" data)
        do
        (is (equal (getf result :status) "failed") "~a" label)
        (is (equal (getf result :message)
               "Current and new password are required") "~a" label))
      (is-true (a:login *rbac* "cp-alice" "password-1")))
    (cp-clean-user "cp-alice")))

(test change-password-invalid-new
  "A new password that fails a:valid-password-p is rejected; the old
password keeps working."
  (cp-clean-user "cp-alice")
  (th-make-user "cp-alice")
  (unwind-protect
    (let* ((id (cp-settings-id "cp-alice"))
           (result (be-action :settings id :change-password "cp-alice"
                      '(:current-password "password-1"
                        :new-password "bad"))))
      (is (equal (getf result :status) "failed"))
      (is (equal (getf result :message)
             "New password is not a valid password"))
      (is-true (a:login *rbac* "cp-alice" "password-1")))
    (cp-clean-user "cp-alice")))

(test change-password-own-row-guard
  "bob cannot change alice's password through alice's settings row
(settings rows are not record-scoped; the hook enforces own-row)."
  (cp-clean-user "cp-alice")
  (cp-clean-user "cp-bob")
  (th-make-user "cp-alice")
  (th-make-user "cp-bob")
  (unwind-protect
    (let* ((id (cp-settings-id "cp-alice"))
           (result (be-action :settings id :change-password "cp-bob"
                      '(:current-password "password-1"
                        :new-password "hacked-99"))))
      (is (equal (getf result :status) "failed"))
      (is (equal (getf result :message)
             "You may only change your own password"))
      (is-true (a:login *rbac* "cp-alice" "password-1"))
      (is-false (a:login *rbac* "cp-alice" "hacked-99")))
    (cp-clean-user "cp-alice")
    (cp-clean-user "cp-bob")))

(test change-password-no-persistence-on-save
  "The virtual password fields are no-column: a normal be-update
carrying them persists the real fields and never changes the
password."
  (cp-clean-user "cp-alice")
  (th-make-user "cp-alice")
  (unwind-protect
    (let* ((id (cp-settings-id "cp-alice")))
      (finishes
        (be-update :settings id
          '(:current-password "x" :new-password "y" :bio "cp-bio")
          "cp-alice"))
      (let ((bio (getf (getf (rec id "cp-alice" :type-key :settings)
                          :record)
                   :bio)))
        (is (equal bio "cp-bio")))
      (is-true (a:login *rbac* "cp-alice" "password-1"))
      (is-false (a:login *rbac* "cp-alice" "x")))
    (cp-clean-user "cp-alice")))

(test change-password-roles-survive
  "The admin-elevated be-update on :users must not touch the user's
roles (update-join-tables no-op / update-roles keyword gate)."
  (cp-clean-user "cp-alice")
  (th-make-user "cp-alice" :roles '("cp-extra-role"))
  (unwind-protect
    (let* ((id (cp-settings-id "cp-alice"))
           (before (a:list-user-role-names *rbac* "cp-alice")))
      (be-action :settings id :change-password "cp-alice"
        '(:current-password "password-1" :new-password "new-pass-9"))
      (is (equal (a:list-user-role-names *rbac* "cp-alice") before))
      (is-true (a:login *rbac* "cp-alice" "new-pass-9")))
    (cp-clean-user "cp-alice")))

(test change-password-in-progress-guard
  "A status field stuck at running rejects further be-action calls
(the sync hook cannot interleave, so the guard is driven by forcing
the status first)."
  (cp-clean-user "cp-alice")
  (th-make-user "cp-alice")
  (unwind-protect
    (let* ((id (cp-settings-id "cp-alice")))
      (be-set-field-value :settings id :change-password-status
        "running" "admin")
      (signals error
        (be-action :settings id :change-password "cp-alice"
          '(:current-password "password-1" :new-password "new-pass-9")))
      ;; Reset so later actions on the row work
      (be-set-field-value :settings id :change-password-status
        "idle" "admin"))
    (cp-clean-user "cp-alice")))

(test change-password-failed-status-echo
  "A failed hook writes \"failed: <message>\" to the status column."
  (cp-clean-user "cp-alice")
  (th-make-user "cp-alice")
  (unwind-protect
    (let* ((id (cp-settings-id "cp-alice")))
      (be-action :settings id :change-password "cp-alice"
        '(:current-password "wrong" :new-password "new-pass-9"))
      (let ((status (getf (getf (rec id "admin" :type-key :settings)
                            :record)
                     :change-password-status)))
        (is (equal status "failed: Current password is incorrect"))))
    (cp-clean-user "cp-alice")))

(test change-password-widget-assignments
  "Password widget split: :users :password and :settings
:new-password carry :password-new (stored secrets); :settings
:current-password carries :password-read (collect an existing
secret). A misassignment compiles clean and keeps every hook test
green (the hook reads :data, not widgets), so assert the widgets
directly."
  (is (eq :password-new
        (u:tree-get *compiled-model* :users :fields :password :ui :widget)))
  (is (eq :password-new
        (u:tree-get *compiled-model* :settings :fields :new-password
          :ui :widget)))
  (is (eq :password-read
        (u:tree-get *compiled-model* :settings :fields :current-password
          :ui :widget))))
