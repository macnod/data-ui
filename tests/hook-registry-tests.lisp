(in-package :data-ui)

(def-suite hook-registry-suite
  :description "Hook registry API and parameterized validation unit tests")

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
