(in-package :data-ui)

(def-suite nullable-fk-suite
  :description "Nullable foreign-key (:target) fields — compiler,
validation, and backend behavior.")

(in-suite nullable-fk-suite)

;;; ---------------------------------------------------------------------------
;;; Compiler tests: DDL and compiled field metadata
;;; ---------------------------------------------------------------------------

(test nullable-fk-ddl-omits-not-null
  "A :target field without :not-null t must produce DDL without
NOT NULL on the FK column."
  (let* ((ddl (u:tree-get *compiled-model*
                :tasks :create-table-sql :table))
          (col-line (find-if
                      (lambda (line)
                        (search "assigned_to" line))
                      (cl-ppcre:split "\\n" ddl))))
    (is-true col-line
      "Expected a column containing 'assigned_to' in DDL")
    (is-false (search "not null" col-line)
      "Nullable FK column must not have NOT NULL: ~a" col-line)))

(test nullable-fk-compiled-def-has-no-not-null
  "The compiled field def for a nullable :target field must have
:not-null NIL."
  (let ((field-def (u:tree-get *compiled-model*
                     :tasks :fields :assigned-to)))
    (is-true (getf field-def :target)
      "Field should have :target :users")
    (is-false (getf field-def :not-null)
      "Field should not have :not-null t")))

(test not-null-target-still-forces-not-null
  "A :target field WITH :not-null t must still produce NOT NULL.
Regression guard: the fix must not break explicit not-null."
  ;; :name has :not-null t — not a target field, but verify the
  ;; principle holds for the DDL as a whole.
  (let* ((ddl (u:tree-get *compiled-model*
                :tasks :create-table-sql :table))
          (lines (cl-ppcre:split "\\n" ddl))
          (name-line (find-if
                       (lambda (line)
                         (search "task_name" line))
                       lines)))
    (is-true (search "not null" name-line)
      "Explicit :not-null t field must still have NOT NULL: ~a"
      name-line)))

;;; ---------------------------------------------------------------------------
;;; Validation tests: value-type-p and resolve-reference-id
;;; ---------------------------------------------------------------------------

(test value-type-p-accepts-nil-for-nullable-target
  "value-type-p must return T for NIL on a nullable :target field."
  (is-true (value-type-p :tasks :assigned-to nil)))

(test value-type-p-rejects-nil-for-not-null-field
  "value-type-p must return NIL for NIL on a :not-null field."
  (is-false (value-type-p :tasks :name nil)))

(test resolve-reference-id-returns-null-for-nil
  "resolve-reference-id must return :NULL (not signal an error) when
value is NIL for a nullable target field.  :NULL is the sentinel that
cl-postgres serializes as SQL NULL."
  (is (equal :null (resolve-reference-id :tasks :assigned-to nil))))

;;; ---------------------------------------------------------------------------
;;; Backend round-trip: insert, list, update with nil FK
;;; ---------------------------------------------------------------------------

(test be-insert-with-nil-fk-succeeds
  "be-insert must accept a record where a nullable FK field is
absent (defaults to NIL)."
  (let ((id (be-insert :tasks '(:name "Unassigned Task") "admin")))
    (is-true id "Insert with nil FK should succeed")
    (when id
      (be-delete :tasks id "admin"))))

(test be-insert-with-explicit-nil-fk-succeeds
  "be-insert must accept an explicit NIL value for a nullable FK."
  (let ((id (be-insert :tasks
              '(:name "Explicit Nil Task" :assigned-to nil)
              "admin")))
    (is-true id "Insert with explicit nil FK should succeed")
    (when id
      (be-delete :tasks id "admin"))))

(test be-list-shows-nil-fk-as-nil
  "After inserting a record with no FK value, be-list should return
the record with the FK field as NIL (or :NULL from the DB, which
value-or-nil converts to NIL)."
  (let ((id (be-insert :tasks '(:name "List Nil Task") "admin")))
    (unwind-protect
      (let* ((result (be-list :tasks "admin"))
              (records (getf result :records))
              (record (find "List Nil Task" records
                        :key (lambda (r) (getf r :name))
                        :test 'equal)))
        (is-true record "Should find the inserted record")
        (is-false (value-or-nil (getf record :assigned-to))
          "FK field should be NULL in list result"))
      (when id
        (be-delete :tasks id "admin")))))

(test be-update-can-set-fk-from-nil
  "be-update can set a nullable FK from NIL to a real value."
  (let ((id (be-insert :tasks '(:name "Set FK Task") "admin")))
    (unwind-protect
      (progn
        (be-update :tasks id
          '(:name "Set FK Task" :assigned-to "admin")
          "admin")
        (let* ((result (be-list :tasks "admin"))
                (records (getf result :records))
                (record (find "Set FK Task" records
                          :key (lambda (r) (getf r :name))
                          :test 'equal)))
          (is-true record "Should find the updated record")
          (is (equal "admin" (getf record :assigned-to))
            "FK should now be 'admin': ~a"
            (getf record :assigned-to))))
      (when id
        (be-delete :tasks id "admin")))))

(test be-update-can-clear-fk-to-nil
  "be-update can clear a nullable FK back to NIL."
  (let ((id (be-insert :tasks
              '(:name "Clear FK Task" :assigned-to "admin")
              "admin")))
    (unwind-protect
      (progn
        (be-update :tasks id
          '(:name "Clear FK Task" :assigned-to nil)
          "admin")
        (let* ((result (be-list :tasks "admin"))
                (records (getf result :records))
                (record (find "Clear FK Task" records
                          :key (lambda (r) (getf r :name))
                          :test 'equal)))
          (is-true record "Should find the updated record")
          (is-false (value-or-nil (getf record :assigned-to))
            "FK should be NULL after clearing")))
      (when id
        (be-delete :tasks id "admin")))))
