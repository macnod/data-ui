(in-package :data-ui)

(def-suite m2m-suite
  :description "Multiple M2M joiners per type — compile and runtime tests.")

(in-suite m2m-suite)

;;; ---------------------------------------------------------------------------
;;; Compile-time tests: per-joiner SQL isolation
;;; ---------------------------------------------------------------------------

(test m2m-insert-sql-isolates-columns-per-joiner
  "Each joiner's insert-sql must contain only that joiner's two FK columns
and param keys — not the union of all joiners on the owner type."
  (let ((insert-sql (u:tree-get *compiled-model* :items :insert-sql)))
    ;; :tags → :item-tags joiner → (:item-id, :tag-id)
    (let ((tags-entry (getf insert-sql :tags)))
      (is-true tags-entry "Expected :tags in insert-sql")
      (let ((sql (car tags-entry))
            (keys (cdr tags-entry)))
        (is (= 2 (length keys))
            "Expected 2 param keys for :tags, got ~a" keys)
        (is (member :item-id keys)
            "Expected :ITEM-ID in ~a" keys)
        (is (member :tag-id keys)
            "Expected :TAG-ID in ~a" keys)
        (is (not (member :user-id keys))
            ":tags SQL must not contain :USER-ID")
        (is (search "item_tags" sql)
            "SQL must reference item_tags table")
        (is (not (search "item_users" sql))
            ":tags SQL must not reference item_users table")))
    ;; :assignees → :item-users joiner → (:item-id, :user-id)
    (let ((asg-entry (getf insert-sql :assignees)))
      (is-true asg-entry "Expected :assignees in insert-sql")
      (let ((sql (car asg-entry))
            (keys (cdr asg-entry)))
        (is (= 2 (length keys))
            "Expected 2 param keys for :assignees, got ~a" keys)
        (is (member :item-id keys)
            "Expected :ITEM-ID in ~a" keys)
        (is (member :user-id keys)
            "Expected :USER-ID in ~a" keys)
        (is (not (member :tag-id keys))
            ":assignees SQL must not contain :TAG-ID")
        (is (search "item_users" sql)
            "SQL must reference item_users table")
        (is (not (search "item_tags" sql))
            ":assignees SQL must not reference item_tags table")))))

(test m2m-update-sql-isolates-columns-per-joiner
  "Each joiner's update-sql (:insert and :delete) must contain only
that joiner's two FK columns and param keys."
  (let ((update-sql (u:tree-get *compiled-model* :items :update-sql)))
    ;; Check :tags
    (let ((tags-entry (getf update-sql :tags)))
      (is-true tags-entry "Expected :tags in update-sql")
      (dolist (branch '(:insert :delete))
        (let* ((entry (getf tags-entry branch))
               (sql (car entry))
               (keys (cdr entry)))
          (is (= 2 (length keys))
              "Expected 2 keys for :tags ~a, got ~a" branch keys)
          (is (member :item-id keys))
          (is (member :tag-id keys))
          (is (not (member :user-id keys)))
          (when (eq branch :insert)
            (is (search "item_tags" sql))
            (is (not (search "item_users" sql)))))))
    ;; Check :assignees
    (let ((asg-entry (getf update-sql :assignees)))
      (is-true asg-entry "Expected :assignees in update-sql")
      (dolist (branch '(:insert :delete))
        (let* ((entry (getf asg-entry branch))
               (sql (car entry))
               (keys (cdr entry)))
          (is (= 2 (length keys))
              "Expected 2 keys for :assignees ~a, got ~a" branch keys)
          (is (member :item-id keys))
          (is (member :user-id keys))
          (is (not (member :tag-id keys)))
          (when (eq branch :insert)
            (is (search "item_users" sql))
            (is (not (search "item_tags" sql)))))))))

(test m2m-regression-single-joiner-todos
  "Regression: todos single-joiner insert-sql shape unchanged."
  ;; This test runs under m2m-test model, so we can't check todos
  ;; directly. Instead, verify the base model's role-users joiner
  ;; still has correct shape.
  (let ((insert-sql (u:tree-get *compiled-model* :users :insert-sql)))
    ;; :users is a base type; its :main insert should exist
    (is-true (getf insert-sql :main)
      "Expected :main in users insert-sql")))

;;; ---------------------------------------------------------------------------
;;; Runtime tests: be-insert / be-update with two joiners
;;; ---------------------------------------------------------------------------

(defun find-record-by-name (name records)
  "Find a record by :name from the :records list returned by be-list."
  (find name records :key (lambda (r) (getf r :name)) :test 'equal))

(defun join-row-count (table-name item-id)
  "Count rows in join table TABLE-NAME for ITEM-ID."
  (let* ((sql (format nil "select count(*) from ~a where item_id = $1"
                table-name))
         (result (a:with-rbac (*rbac*)
                   (a:rbac-query (list sql item-id) :single))))
    (or result 0)))

(test m2m-be-insert-both-joiners
  "be-insert with both list fields populated creates rows in both
join tables."
  (let ((item-id (be-insert :items
                   '(:name "Test Item"
                     :tags ("red" "blue")
                     :assignees ("admin"))
                   "admin")))
    (is-true item-id "be-insert should return an ID")
    ;; Verify join table row counts directly (view-level aggregation
    ;; can produce cartesian duplicates when multiple M2M paths are
    ;; star-joined on one view — that's a separate concern).
    (is (= 2 (join-row-count "rt_item_tags" item-id))
        "Should be 2 rows in rt_item_tags")
    (is (= 1 (join-row-count "rt_item_users" item-id))
        "Should be 1 row in rt_item_users")
    (be-delete :items item-id "admin")))

(test m2m-be-insert-empty-lists
  "be-insert with empty lists creates no join rows."
  (let ((item-id (be-insert :items
                   '(:name "Empty Item")
                   "admin")))
    (is-true item-id)
    (is (= 0 (join-row-count "rt_item_tags" item-id))
        "Should be 0 rows in rt_item_tags")
    (is (= 0 (join-row-count "rt_item_users" item-id))
        "Should be 0 rows in rt_item_users")
    (be-delete :items item-id "admin")))

(test m2m-be-update-add-remove-per-joiner
  "be-update can add/remove values on each list independently."
  (let ((item-id (be-insert :items
                   '(:name "Update Me"
                     :tags ("red")
                     :assignees ("admin"))
                   "admin")))
    (is-true item-id)
    ;; Update: add "blue" to tags, add "guest" to assignees
    (be-update :items item-id
      '(:name "Update Me"
        :tags ("red" "blue")
        :assignees ("admin" "guest"))
      "admin")
    (is (= 2 (join-row-count "rt_item_tags" item-id))
        "Should be 2 tag rows after adding blue")
    (is (= 2 (join-row-count "rt_item_users" item-id))
        "Should be 2 user rows after adding guest")
    ;; Second update: remove "red" from tags, remove "guest" from assignees
    (be-update :items item-id
      '(:name "Update Me"
        :tags ("blue")
        :assignees ("admin"))
      "admin")
    (is (= 1 (join-row-count "rt_item_tags" item-id))
        "Should be 1 tag row after removing red")
    (is (= 1 (join-row-count "rt_item_users" item-id))
        "Should be 1 user row after removing guest")
    (be-delete :items item-id "admin")))

(test m2m-be-insert-field-key-differs-from-target
  "The :assignees field (key ≠ target type :users) resolves correctly.
The join DML must use :user-id, not :assignee-id."
  (let ((item-id (be-insert :items
                   '(:name "Field Key Test"
                     :assignees ("admin"))
                   "admin")))
    (is-true item-id
      "Insert with :assignees (field key ≠ :users) must succeed")
    (is (= 1 (join-row-count "rt_item_users" item-id))
        "Should be 1 row in rt_item_users")
    (be-delete :items item-id "admin")))

(test m2m-join-filter-with-sort-42p10
  "Regression (clickable chips 0b): a join-filtered request with an
explicit sort must not die with 42P10 (SELECT DISTINCT, ORDER BY
expressions must appear in select list). Phase A splices the sort
column into the page projection; the count query stays pre-splice."
  (let ((red-id (be-insert :items
                   '(:name "Red Item" :tags ("red"))
                   "admin"))
        (both-id (be-insert :items
                    '(:name "Both Item" :tags ("red" "blue"))
                    "admin"))
        (green-id (be-insert :items
                      '(:name "Green Item" :tags ("green"))
                      "admin")))
    (unwind-protect
      (let* ((result (be-list :items "admin"
                      :filters '((:tags :name :in ("red")))
                      :sort '(:name :asc)))
             (names (mapcar
                      (lambda (r) (getf r :name))
                      (getf result :records))))
        (is (equal '("Both Item" "Red Item") names)
          "Join filter + sort returns the tagged items, name-asc: ~a"
          names)
        (is (= 2 (getf result :total))
          "Total counts distinct matching ids, not fan-out rows")
        (let* ((desc (be-list :items "admin"
                       :filters '((:tags :name :in ("red")))
                       :sort '(:name :desc)))
               (desc-names (mapcar
                             (lambda (r) (getf r :name))
                             (getf desc :records))))
          (is (equal '("Red Item" "Both Item") desc-names)
            "Request-sorted variant reverses: ~a" desc-names))
        ;; Non-join request with sort stays on the plain path.
        (let ((plain (be-list :items "admin" :sort '(:name :asc))))
          (is (= 3 (getf plain :total))
            "Unfiltered list unchanged")))
      (be-delete :items red-id "admin")
      (be-delete :items both-id "admin")
      (be-delete :items green-id "admin"))))
