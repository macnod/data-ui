(in-package :data-ui)

(def-suite guest-suite
  :description ":guest-allowed top-level model key tests")

(def-suite guest-validation-suite
  :description "Compile-time validation probes (no model needed)"
  :in guest-suite)

(def-suite guest-db-suite
  :description "Login behavior (guest-allowed-test fixture)"
  :in guest-suite)

;;; ---------------------------------------------------------------------------
;;; Validation probes (no model, no DB)
;;; ---------------------------------------------------------------------------

(in-suite guest-validation-suite)

(defun th-ga-model (guest-allowed)
  "Build a minimal model plist with :guest-allowed set to GUEST-ALLOWED
(:absent omits the key entirely)."
  (let ((model (list :title "GA" :name "ga" :version "0.1"
                 :domain "ga.test.data-ui.com"
                 :types '(:widgets (:table t :fields (:name (:type :text)))))))
    (unless (eq guest-allowed :absent)
      (setf (getf model :guest-allowed) guest-allowed))
    model))

(test guest-allowed-nil-and-t-accepted
  "nil and t both pass; the key is optional (:repl precedent)."
  (finishes (valid-top-level-field (th-ga-model nil) :guest-allowed))
  (finishes (valid-top-level-field (th-ga-model t) :guest-allowed))
  (finishes (valid-top-level-field (th-ga-model :absent) :guest-allowed)))

(test guest-allowed-non-boolean-signals
  "Only booleans validate; strings, keywords, and integers signal."
  (loop for bad in '("true" :true 1 "yes")
    do (signals error
         (valid-top-level-field (th-ga-model bad) :guest-allowed))))

(test guest-allowed-flows-into-top-level-settings
  "top-level-settings picks up the key; absence yields nil via getf."
  (is (eq t (getf (top-level-settings (th-ga-model t)) :guest-allowed)))
  (is (null (getf (top-level-settings (th-ga-model nil)) :guest-allowed))))

;;; ---------------------------------------------------------------------------
;;; Login behavior (guest-allowed-test fixture)
;;; ---------------------------------------------------------------------------
;;; The fixture sets :guest-allowed t. with-model resets the database,
;;; so the seeded guest user (public + guest:exclusive roles, bogus
;;; password) is always present.

(in-suite guest-db-suite)

(defun th-ga-user-id (name)
  (a:get-id *rbac* "users" name))

(test guest-login-fixture-guest-allowed-t
  "Fixture wiring: the model compiles with :guest-allowed t and
model-guest-allowed reads it back."
  (is (eq t (model-guest-allowed))))

(test guest-passwordless-login-succeeds
  "login-user-id returns the guest user id for any password,
including the empty string."
  (let ((guest-id (th-ga-user-id "guest")))
    (is (equal guest-id (login-user-id "guest" "bogus")))
    (is (equal guest-id (login-user-id "guest" "")))))

(test guest-has-public-but-not-logged-in
  "The seeded guest user carries public (read-only) and its
exclusive role, but never logged-in."
  (let ((roles (a:list-user-role-names *rbac* "guest")))
    (is (member "public" roles :test #'equal))
    (is (member "guest:exclusive" roles :test #'equal))
    (is (not (member "logged-in" roles :test #'equal)))))

(test guest-login-does-not-elevate-other-users
  "The passwordless path is guest-only: another user with a wrong
password still fails, and admin's real password still works."
  (is-false (login-user-id "admin" "bogus"))
  (is-true (th-make-user "ga-user"))
  (is-false (login-user-id "ga-user" "bogus"))
  (is (equal (th-ga-user-id "ga-user")
        (login-user-id "ga-user" "password-1"))))

(test guest-login-disabled-without-key
  "Without :guest-allowed, guest login falls back to a:login, so a
wrong password fails even though login-user-id is the entry point.
Loads the fixture file, strips the key, and recompiles via the
set-model list method."
  (let* ((file (u:join-paths *package-root* "models" "test"
                "guest-allowed-test.lisp"))
          (model (with-open-file (s file) (second (read s)))))
    (remf model :guest-allowed)
    (set-model model)
    (is (null (model-guest-allowed)))
    (is-false (login-user-id "guest" "bogus"))
    (is-false (login-user-id "guest" "")))
  ;; Restore the fixture model for any later tests in this suite.
  (set-model "guest-allowed-test")
  (is (eq t (model-guest-allowed))))
