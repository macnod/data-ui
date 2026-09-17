(in-package :data-ui)

(def-suite api-roles-suite
  :description ":api-roles top-level model key tests")

(def-suite api-roles-validation-suite
  :description "Compile-time validation probes (no model needed)"
  :in api-roles-suite)

(def-suite api-roles-db-suite
  :description "Accessor and behavior tests (api-roles-test fixture)"
  :in api-roles-suite)

;;; ---------------------------------------------------------------------------
;;; Validation probes (no model, no DB)
;;; ---------------------------------------------------------------------------

(in-suite api-roles-validation-suite)

(defun th-ar-model (api-roles)
  "Build a minimal model plist; API-ROLES is the :api-roles value
or :absent to omit the key entirely."
  (let ((model (list :title "AR" :name "ar" :version "0.1"
                 :domain "ar.test.data-ui.com"
                 :types '(:widgets (:table t
                                  :fields (:name (:type :text)))))))
    (unless (eq api-roles :absent)
      (setf (getf model :api-roles) api-roles))
    model))

(test api-roles-accepted-lists
  "Non-empty lists of role-name strings validate."
  (finishes (valid-top-level-field (th-ar-model '("logged-in")) :api-roles))
  (finishes
    (valid-top-level-field (th-ar-model '("logged-in" "public")) :api-roles))
  (finishes (valid-top-level-field (th-ar-model '("public")) :api-roles)))

(test api-roles-absent-finishes
  "Absent key skips validation; the accessor default is covered in
the db suite."
  (finishes (valid-top-level-field (th-ar-model :absent) :api-roles)))

(test api-roles-bad-values-signal
  "Atoms, explicit nil / (), non-strings, empty strings, and
duplicates all signal (nil / () only signal because of the member
presence check in valid-top-level-field)."
  (loop for bad in '("logged-in" 42 :logged-in nil () ("logged-in" 42)
                     ("" "x") ("a" "a"))
    do (signals error
         (valid-top-level-field (th-ar-model bad) :api-roles))))

(test api-roles-flows-into-top-level-settings
  "top-level-settings stores the authored list verbatim; absence
yields nil via getf."
  (is (equal '("logged-in" "public")
        (getf (top-level-settings
                (th-ar-model '("logged-in" "public")))
          :api-roles)))
  (is (null (getf (top-level-settings (th-ar-model :absent)) :api-roles))))

;;; ---------------------------------------------------------------------------
;;; Accessor + behavior (api-roles-test fixture)
;;; ---------------------------------------------------------------------------
;;; The fixture sets :api-roles ("logged-in" "public") and opens the
;;; :widgets type to "public". with-model resets the database, so the
;;; seeded guest user (public + guest:exclusive, no logged-in) is
;;; always present.

(in-suite api-roles-db-suite)

(defun th-ar-widget-count ()
  "Number of widget records visible to admin."
  (length (getf (be-list :widgets "admin" :limit 1000) :records)))

(test api-roles-accessor-returns-authored-list
  "model-api-roles returns the authored list."
  (is (equal '("logged-in" "public") (model-api-roles))))

(test api-roles-accessor-default-when-absent
  "Without the key (fixture list, remf, set-model), the accessor
defaults to (\"logged-in\") — the guest-tests pattern."
  (let* ((file (u:join-paths *package-root* "models" "test"
                "api-roles-test.lisp"))
          (model (with-open-file (s file) (second (read s)))))
    (remf model :api-roles)
    (set-model model)
    (is (null (getf *top-level-settings* :api-roles)))
    (is (equal '("logged-in") (model-api-roles))))
  ;; Restore the fixture model for any later tests in this suite.
  (set-model "api-roles-test")
  (is (equal '("logged-in" "public") (model-api-roles))))

(test api-roles-guest-roles-unchanged
  "Regression: the seeded guest user still carries public +
guest:exclusive and never logged-in."
  (let ((roles (a:list-user-role-names *rbac* "guest")))
    (is (member "public" roles :test #'equal))
    (is (member "guest:exclusive" roles :test #'equal))
    (is (not (member "logged-in" roles :test #'equal)))))

(test api-roles-be-types-includes-public-type
  "be-types as guest includes :widgets (type-roles public)."
  (let ((names (mapcar (lambda (e) (getf e :name)) (be-types "guest"))))
    (is (member :widgets names))))

(test api-roles-guest-sees-public-rows
  "A row inserted with the public role is visible to guest."
  (let ((before (th-ar-widget-count)))
    (be-insert :widgets '(:name "ar-w1") "admin" :roles '("public"))
    (is (= (1+ before) (th-ar-widget-count)))
    (let ((guest-names (mapcar (lambda (r) (getf r :name))
                            (getf (be-list :widgets "guest") :records))))
      (is (member "ar-w1" guest-names :test #'equal)))))

(test api-roles-list-result-flags-per-user
  "D8: list-result :create/:update/:delete are :false for guest
(no write permissions on type-widgets) and :true for admin."
  (let ((guest (getf (be-list :widgets "guest") :create))
        (admin (getf (be-list :widgets "admin") :create)))
    (is (eq :false guest))
    (is (eq :true admin)))
  (let ((guest (getf (be-list :widgets "guest") :update))
        (admin (getf (be-list :widgets "admin") :update)))
    (is (eq :false guest))
    (is (eq :true admin)))
  (let ((guest (getf (be-list :widgets "guest") :delete))
        (admin (getf (be-list :widgets "admin") :delete)))
    (is (eq :false guest))
    (is (eq :true admin))))

(test api-roles-guest-insert-still-denied
  "be-insert as guest still signals a validation error (read-only
public role; only the wire flags changed, not the write gates)."
  (signals error (be-insert :widgets '(:name "nope") "guest")))
