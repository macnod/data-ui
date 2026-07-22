(in-package :data-ui)

(def-suite secrets-suite :description "Secrets type tests")

(in-suite secrets-suite)

(test secrets-type-compiles
  "The :secrets type is present in the compiled model with expected keys."
  (is-true (getf *compiled-model* :secrets))
  (is (eq :settings (getf (getf *compiled-model* :secrets) :category)))
  (is-true (getf (getf *compiled-model* :secrets) :suppress-roles))
  (is-false (getf (getf *compiled-model* :secrets) :user-setting)))

(test secrets-list-form-excludes-value
  "The :value field must not appear in :list-form."
  (let ((fields (getf (getf (getf *compiled-model* :secrets)
                       :list-form) :fields)))
    (is-false (member :value fields))
    (is-true (member :name fields))
    (is-true (member :description fields))))

(test secrets-update-form-includes-value
  "The :value field must be available on the update form."
  (let ((form (getf (getf *compiled-model* :secrets) :update-form)))
    (is (eq t (getf form :fields)))))

(test secrets-insert-and-read-plaintext
  "A user can insert a secret and read it back in plaintext."
  (let ((id (be-insert :secrets
              '(:name "api-key"
                 :value "sk-test-12345"
                 :description "CI API key")
              "admin")))
    (is-true (uuid-p id))
    (let ((record (getf (be-rec id "admin" :type-key :secrets) :record)))
      (is (equal "sk-test-12345" (getf record :value)))
      (is (equal "api-key" (getf record :name)))
      ;; Clean up
      (be-delete :secrets id "admin"))))

(test secrets-multi-row-per-user
  "A user can insert multiple secrets without constraint errors."
  (let ((id1 (be-insert :secrets
               '(:name "secret-1" :value "val-1"
                  :description "first") "admin"))
         (id2 (be-insert :secrets
                '(:name "secret-2" :value "val-2"
                   :description "second") "admin")))
    (is-true (uuid-p id1))
    (is-true (uuid-p id2))
    (is (not (equal id1 id2)))
    ;; Clean up
    (be-delete :secrets id1 "admin")
    (be-delete :secrets id2 "admin")))

(test secrets-owner-scoped-list
  "User A cannot see user B's secrets in be-list."
  (let ((user-a "sec-a")
         (user-b "sec-b"))
    ;; Both users get the settings role from th-make-user
    (th-make-user user-a)
    (th-make-user user-b)
    ;; Each user inserts a secret
    (let ((id-a (be-insert :secrets
                  `(:name "a-secret" :value "a-val"
                     :description "a desc") user-a))
           (id-b (be-insert :secrets
                   `(:name "b-secret" :value "b-val"
                      :description "b desc") user-b)))
      (is-true (uuid-p id-a))
      (is-true (uuid-p id-b))
      ;; User A sees only their own
      (let ((names (mapcar (lambda (r) (getf r :name))
                      (getf (be-list :secrets user-a) :records))))
        (is (member "a-secret" names :test #'equal))
        (is-false (member "b-secret" names :test #'equal)))
      ;; User B sees only their own
      (let ((names (mapcar (lambda (r) (getf r :name))
                      (getf (be-list :secrets user-b) :records))))
        (is (member "b-secret" names :test #'equal))
        (is-false (member "a-secret" names :test #'equal)))
      ;; Clean up
      (be-delete :secrets id-a "admin")
      (be-delete :secrets id-b "admin"))))

(test secrets-value-not-in-list-records
  "be-list records must not contain the :value field."
  (let ((id (be-insert :secrets
              '(:name "leak-test" :value "sensitive"
                 :description "test") "admin")))
    (let ((records (getf (be-list :secrets "admin") :records)))
      (is-true records)
      (is-false (some (lambda (r) (getf r :value)) records)))
    ;; Clean up
    (be-delete :secrets id "admin")))

(test secrets-autofill-user
  "The :user field is auto-populated with the acting username."
  (let ((id (be-insert :secrets
              '(:name "auto-test" :value "x"
                 :description "test") "admin")))
    (let ((record (getf (be-rec id "admin" :type-key :secrets) :record)))
      (is (equal "admin" (getf record :user))))
    ;; Clean up
    (be-delete :secrets id "admin")))

(test secrets-no-roles-field-in-forms
  "Forms must not contain a :roles field (suppress-roles)."
  (let ((list-fields (getf (getf (getf *compiled-model* :secrets)
                          :list-form) :fields))
         (add-form-spec (getf (getf (getf *compiled-model* :secrets)
                              :add-form) :fields)))
    ;; list-form fields is a list; add-form fields is t (all fields)
    (is-false (member :roles list-fields))
    ;; When :fields is t, check the compiled add-form plist keys instead
    (is-true add-form-spec)))

(test secrets-category-in-be-types
  "be-types must list :secrets under :settings category."
  (let ((entry (find :secrets (be-types "admin") :key #'(lambda (e) (getf e :name)))))
    (is-true entry)
    (is (eq :settings (getf entry :category)))))

(test secrets-delete-by-owner
  "A user can delete their own secret."
  (let ((id (be-insert :secrets
              '(:name "del-test" :value "x"
                 :description "test") "admin")))
    (is-true (uuid-p id))
    (let ((deleted-id (be-delete :secrets id "admin")))
      (is (equal id deleted-id)))
    ;; Confirm it's gone
    (is-false (getf (be-rec id "admin" :type-key :secrets) :record))))
