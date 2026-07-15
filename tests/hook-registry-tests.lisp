(in-package :data-ui)

(def-suite hook-registry-suite
  :description "Hook registry API and parameterized validation unit tests")

(def-suite hook-registry-lifecycle-suite
  :description "Lifecycle compile, unified call sites, and registry lifecycle")

(def-suite hook-registry-integration-suite
  :description "Hook registry integration with be-validate-field (test-model)")

(def-suite hook-registry-modelbank-suite
  :description "Hook registry integration with be-validate-field (modelbank)")

(in-suite hook-registry-suite)

;;; ---------------------------------------------------------------------------
;;; Registry API tests
;;; ---------------------------------------------------------------------------

(test register-and-get-hook
  (let ((entry (get-hook :required)))
    (is-true entry)
    (is (eq (hook-entry-kind entry) :validation)))
  (let ((entry (get-hook :max-length)))
    (is-true entry)
    (is (eq (hook-entry-kind entry) :validation)))
  (let ((entry (get-hook :in-range)))
    (is-true entry)
    (is (eq (hook-entry-kind entry) :validation)))
  ;; Nonexistent
  (is-false (get-hook :nonexistent-hook)))

(test list-hook-names
  (let ((all (list-hook-names))
        (val (list-hook-names :validation)))
    (is (member :required all))
    (is (member :max-length all))
    (is (member :in-range all))
    (is (member :required val))
    (is (member :max-length val))
    ;; No lifecycle hooks registered yet
    (is-false (member :foo (list-hook-names :lifecycle)))))

;;; ---------------------------------------------------------------------------
;;; resolve-hook-form tests
;;; ---------------------------------------------------------------------------

(test resolve-keyword-hook
  ;; Bare keyword → zero-arg registry entry
  (let ((fn (resolve-hook-form :required)))
    (is (functionp fn))))

(test resolve-plist-hook
  ;; Parameterized plist form
  (let ((fn (resolve-hook-form '(:max-length :max 20))))
    (is (functionp fn)))
  (let ((fn (resolve-hook-form '(:in-range :min 1 :max 5))))
    (is (functionp fn))))

(test resolve-lambda-hook
  ;; Raw lambda still works (expert tier)
  (let ((fn (resolve-hook-form
              '(lambda (tk fk v u)
                (declare (ignore tk fk u))
                (when (string= v "bad")
                  "bad value")))))
    (is (functionp fn))
    (is-false (funcall fn :test :field "ok" "admin"))
    (is (string= (funcall fn :test :field "bad" "admin")
                 "bad value"))))

(test resolve-hook-list-preserves-order
  (let ((fns (resolve-hook-list
               '(:required (:max-length :max 10)
                  (lambda (tk fk v u)
                    (declare (ignore tk fk u))
                    nil)))))
    (is (= 3 (length fns)))
    (is (every #'functionp fns))))

(test resolve-unknown-hook-errors
  (signals error (resolve-hook-form :totally-unknown))
  (signals error (resolve-hook-form '(:totally-unknown :foo 1))))

(test resolve-shell-hook-errors
  (signals error (resolve-hook-form '(:shell "some-script"))))

(test resolve-missing-params-error
  (signals error (resolve-hook-form '(:max-length)))    ; no :max
  (signals error (resolve-hook-form '(:in-range :min 1))) ; no :max
  (signals error (resolve-hook-form '(:in-range :max 5)))) ; no :min

;;; ---------------------------------------------------------------------------
;;; :max-length behavior tests
;;; ---------------------------------------------------------------------------

(test max-length-accepts-at-limit
  (let ((fn (resolve-hook-form '(:max-length :max 5))))
    (is-false (funcall fn :test :field "hello" "admin"))
    (is-false (funcall fn :test :field "hi" "admin"))))

(test max-length-rejects-over-limit
  (let ((fn (resolve-hook-form '(:max-length :max 5))))
    (is-true (funcall fn :test :field "hello world" "admin"))))

(test max-length-noop-on-empty
  (let ((fn (resolve-hook-form '(:max-length :max 5))))
    (is-false (funcall fn :test :field nil "admin"))
    (is-false (funcall fn :test :field "" "admin"))))

;;; ---------------------------------------------------------------------------
;;; :in-range behavior tests
;;; ---------------------------------------------------------------------------

(test in-range-accepts-in-range
  (let ((fn (resolve-hook-form '(:in-range :min 1 :max 5))))
    (is-false (funcall fn :test :field "1" "admin"))
    (is-false (funcall fn :test :field "3" "admin"))
    (is-false (funcall fn :test :field "5" "admin"))))

(test in-range-rejects-out-of-range
  (let ((fn (resolve-hook-form '(:in-range :min 1 :max 5))))
    (is-true (funcall fn :test :field "0" "admin"))
    (is-true (funcall fn :test :field "6" "admin"))
    (is-true (funcall fn :test :field "-1" "admin"))))

(test in-range-rejects-non-numeric
  (let ((fn (resolve-hook-form '(:in-range :min 1 :max 5))))
    (is-true (funcall fn :test :field "abc" "admin"))))

(test in-range-noop-on-empty
  (let ((fn (resolve-hook-form '(:in-range :min 1 :max 5))))
    (is-false (funcall fn :test :field nil "admin"))
    (is-false (funcall fn :test :field "" "admin"))))

;;; ---------------------------------------------------------------------------
;;; Model-dependent integration tests (need compiled model)
;;; ---------------------------------------------------------------------------

(in-suite hook-registry-integration-suite)

(test mixed-validations-keyword-and-lambda
  ;; The test-model has :todos with :name (:required + lambda)
  (let ((result (be-validate-field :todos :name "" "admin")))
    (is (equal (getf result :valid) :false)))
  (let ((result (be-validate-field :todos :name "test" "admin")))
    (is (equal (getf result :valid) :true)))
  ;; Lambda validation still works (< 20 chars)
  (let ((result (be-validate-field :todos :name
                   "this is a very long name" "admin")))
    (is (equal (getf result :valid) :false))))

;;; ---------------------------------------------------------------------------
;;; Model Bank :in-range integration
;;; ---------------------------------------------------------------------------

(in-suite hook-registry-modelbank-suite)

(test modelbank-rating-in-range-accepts
  (is (equal (getf (be-validate-field :models :rating "3" "admin")
                   :valid)
             :true))
  (is (equal (getf (be-validate-field :models :rating "1" "admin")
                   :valid)
             :true))
  (is (equal (getf (be-validate-field :models :rating "5" "admin")
                   :valid)
             :true)))

(test modelbank-rating-in-range-rejects
  (is (equal (getf (be-validate-field :models :rating "0" "admin")
                   :valid)
             :false))
  (is (equal (getf (be-validate-field :models :rating "6" "admin")
                   :valid)
             :false)))

;;; ---------------------------------------------------------------------------
;;; Lifecycle hook tests
;;; ---------------------------------------------------------------------------

(in-suite hook-registry-lifecycle-suite)

(test resolve-function-hook
  "Raw function values pass through resolve-hook-form as-is"
  (let ((fn (lambda (tk d u &key id roles record)
              (declare (ignore tk d u id roles record)))))
    (is (eq (resolve-hook-form fn) fn))))

(test compile-lifecycle-hooks-on-base-model
  ":users :post-create and :pre-delete compile to function lists"
  (let ((users (getf *compiled-model* :users)))
    (is (listp (getf users :post-create)))
    (is (every #'functionp (getf users :post-create)))
    (is (listp (getf users :pre-delete)))
    (is (every #'functionp (getf users :pre-delete)))
    ;; Slots not set on :users should be nil
    (is-false (getf users :pre-create))
    (is-false (getf users :pre-update))
    (is-false (getf users :post-update))
    (is-false (getf users :post-delete))))

(test settings-created-on-user-insert
  "Creating a user via be-insert fires :post-create → add-user-settings"
  (is (string= (th-make-user "lifecycle-test-user") "lifecycle-test-user"))
  ;; With :scope :user, use be-value-id (not scoped) to verify the row
  (let ((settings-id (be-value-id :settings :user
                        "lifecycle-test-user" "admin")))
    (is-true settings-id)
    (let ((display-name (be-val settings-id :display-name
                          "admin" :type-key :settings)))
      (is (string= display-name "lifecycle-test-user")))))

(test settings-removed-on-user-delete
  "Deleting a user fires :pre-delete → remove-user-settings"
  (th-make-user "delete-test-user")
  (let ((settings-id-before
          (be-value-id :settings :user "delete-test-user" "admin")))
    (is-true settings-id-before)
    ;; Look up the user's UUID, then delete by UUID
    (let ((user-uuid (be-id :users
                     `((:users :name :eq "delete-test-user"))
                     "admin")))
      (is-true user-uuid)
      (be-delete :users user-uuid "admin")
      (let ((settings-id-after
              (be-value-id :settings :user "delete-test-user" "admin")))
        (is-false settings-id-after)))))

(test registry-lifecycle-hook-runs
  "A registered :lifecycle hook in keyword form compiles and runs
with the unified contract, receiving :id on post-create."
  (let ((captured nil))
    (register-hook :capture-test :lifecycle nil
      (lambda ()
        (lambda (type-key data user &key id roles record)
          (declare (ignore roles record))
          (push (list :type-key type-key :data data :user user
                      :id id)
                captured))))
    ;; Inject the hook into the compiled :todos type
    (let* ((todos-def (getf *compiled-model* :todos))
           (compiled-hooks
             (compile-lifecycle-hooks
               (list :todos
                     (add-to-plist todos-def
                       (list :post-create '((:capture-test)))))
               :todos)))
      (is-true (getf compiled-hooks :post-create))
      (let ((hooks (getf compiled-hooks :post-create)))
        (is (= 1 (length hooks)))
        (is (functionp (car hooks)))
        ;; Run the hook manually to verify the contract
        (run-lifecycle-hooks hooks :todos '(:name "test") "admin"
          :id "fake-id" :roles nil)
        (is-true captured)
        (let ((latest (car captured)))
          (is (eq (getf latest :type-key) :todos))
          (is (string= (getf (getf latest :data) :name) "test"))
          (is (string= (getf latest :id) "fake-id")))))))

(test pre-update-hook-runs
  ":pre-update hooks fire with the unified contract via run-lifecycle-hooks"
  (let ((captured nil))
    (register-hook :capture-update :lifecycle nil
      (lambda ()
        (lambda (type-key data user &key id roles record)
          (declare (ignore roles record))
          (push (list :type-key type-key :id id :user user)
                captured))))
    ;; Inject into :todos and run manually
    (let* ((todos-def (getf *compiled-model* :todos))
           (compiled-hooks
             (compile-lifecycle-hooks
               (list :todos
                     (add-to-plist todos-def
                       (list :pre-update '((:capture-update)))))
               :todos)))
      (is-true (getf compiled-hooks :pre-update))
      (let ((hooks (getf compiled-hooks :pre-update)))
        (is (= 1 (length hooks)))
        (is (functionp (car hooks)))
        (run-lifecycle-hooks hooks :todos '(:name "updated") "admin"
          :id "test-uuid" :roles nil :record nil)
        (is-true captured)
        (let ((latest (car captured)))
          (is (eq (getf latest :type-key) :todos))
          (is (string= (getf latest :id) "test-uuid")))))))
