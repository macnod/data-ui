(in-package :data-ui)

(def-suite sortable-suite
  :description "Tests for :sortable field attribute and sort behavior.")

(in-suite sortable-suite)

(test sortable-compiles-on-base-column
  ":sortable t on a base-column field compiles and appears in
the compiled field plist."
  (is-true
    (u:tree-get *compiled-model* :todos :fields :name :sortable)))

(test sortable-absent-when-not-declared
  "Fields without :sortable t should have :sortable nil in the
compiled field plist."
  (is-false
    (u:tree-get *compiled-model* :todos :fields :done :sortable)))

(test sortable-forwarded-in-fe-fields
  "fe-fields forwards :sortable into the list-form field plist
for sortable fields."
  (let ((list-form (getf (fe-fields :todos "admin") :list-form)))
    (is-true (getf (getf list-form :points) :sortable))
    (is-false (getf (getf list-form :done) :sortable))))

(test sortable-index-emitted-for-non-indexed-field
  "create-table-sql emits a sort index for :sortable t fields
that don't already have a covering index."
  (let ((sort-index (u:tree-get *compiled-model*
                       :todos :create-table-sql :sort-index)))
    (is-true sort-index)
    (is-true
      (some (lambda (ddl)
              (search "ix_rt_todos_todo_points" ddl))
        sort-index))))

(test sortable-no-index-for-unique-field
  "create-table-sql does NOT emit a sort index for :sortable t
fields that already have :unique t (the unique constraint
already covers the index)."
  (let ((sort-index (u:tree-get *compiled-model*
                       :todos :create-table-sql :sort-index)))
    (is-false
      (some (lambda (ddl)
              (search "todo_name" ddl))
        sort-index))))

(test sortable-default-order-by-id
  "When sort is nil, phase-a-build-order-by defaults to
ORDER BY <table>.id."
  (let ((clause (phase-a-build-order-by :todos nil)))
    (is-true
      (search "order by rt_todos.id" clause))))

(test sortable-valid-field-asc
  "phase-a-build-order-by produces correct ORDER BY for a
sortable field with :asc direction."
  (let ((clause (phase-a-build-order-by :todos '(:points :asc))))
    (is-true (search "order by" clause))
    (is-true (search "ASC" clause))))

(test sortable-valid-field-desc
  "phase-a-build-order-by produces correct ORDER BY for a
sortable field with :desc direction."
  (let ((clause (phase-a-build-order-by :todos '(:points :desc))))
    (is-true (search "order by" clause))
    (is-true (search "DESC" clause))))

(test sortable-rejects-non-sortable-field
  "phase-a-order-by-column signals a validation error when the
sort field is not marked :sortable."
  (signals error
    (phase-a-order-by-column :todos '(:done))))

(test sortable-rejects-unknown-field
  "phase-a-order-by-column signals a validation error when the
sort field does not exist."
  (signals error
    (phase-a-order-by-column :todos '(:nonexistent))))

(test sortable-be-list-with-sort
  "be-list returns records sorted by the sortable field."
  ;; Insert test data
  (be-insert :todos '(:name "Charlie" :points 30) "admin")
  (be-insert :todos '(:name "Alice" :points 10) "admin")
  (be-insert :todos '(:name "Bob" :points 20) "admin")
  ;; Sort by points ascending
  (let* ((result (be-list :todos "admin" :sort '(:points :asc)))
          (records (getf result :records))
          (names (loop for r in records collect (getf r :name))))
    (is (equal '("Alice" "Bob" "Charlie") names)))
  ;; Cleanup
  (loop for r in (getf (be-list :todos "admin") :records)
        do (be-delete :todos (getf r :id) "admin")))

(test sortable-be-list-with-sort-desc
  "be-list returns records sorted descending by the sortable field."
  (be-insert :todos '(:name "Charlie" :points 30) "admin")
  (be-insert :todos '(:name "Alice" :points 10) "admin")
  (be-insert :todos '(:name "Bob" :points 20) "admin")
  (let* ((result (be-list :todos "admin" :sort '(:points :desc)))
          (records (getf result :records))
          (names (loop for r in records collect (getf r :name))))
    (is (equal '("Charlie" "Bob" "Alice") names)))
  ;; Cleanup
  (loop for r in (getf (be-list :todos "admin") :records)
        do (be-delete :todos (getf r :id) "admin")))
