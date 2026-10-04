(in-package :data-ui)

(def-suite timestamp-null-suite
  :description "Clear-to-NULL on nullable scalar columns — the 22007
regression (nil bound as SQL FALSE), the is-null filter translation,
and the nil-element list-filter rejection.")

(in-suite timestamp-null-suite)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun tnu-insert-item (name &key (notes "seed-notes"))
  "Insert one :items row on the spawn-test fixture. NOTES defaults to
a marker string; the omitted-default test passes :notes-omitted
instead (be-insert fills the compiled default \"\" when the key is
absent). Returns the id."
  (be-insert :items
    (if (eq notes :notes-omitted)
      (list :name name :points 2)
      (list :name name :points 2 :notes notes))
    "admin"))

(defun tnu-item-notes (id)
  "Raw :notes value of one :items row (:null from the DB → nil)."
  (getf (first (getf (be-list :items "admin"
                        :filters `((:items :id :eq ,id))
                        :limit 1)
                   :records))
    :notes))

(defun tnu-item-completed-at (id)
  "Raw :completed-at value of one :items row."
  (getf (first (getf (be-list :items "admin"
                        :filters `((:items :id :eq ,id))
                        :limit 1)
                   :records))
    :completed-at))

;;; ---------------------------------------------------------------------------
;;; Update path: explicit nil on nullable scalar columns
;;; ---------------------------------------------------------------------------

(test be-update-explicit-nil-timestamp
  "The 22007 regression: be-update with an explicit nil :completed-at
on a fresh row must succeed and leave the column NULL. Before the
db-value fix, the driver bound Lisp nil as SQL FALSE and Postgres
rejected the literal with invalid input syntax for type timestamp."
  (let ((id (tnu-insert-item "tnu-timestamp")))
    (unwind-protect
      (progn
        (be-update :items id
          '(:name "tnu-timestamp" :completed-at nil)
          "admin")
        (is-false (value-or-nil (tnu-item-completed-at id))
          ":completed-at must stay NULL after the update"))
      (be-delete :items id "admin"))))

(test be-update-json-empty-array-timestamp
  "The wire shape of the bug: json-to-plist of \"completed-at\": []
is nil — the same update through that door must preserve NULL."
  (let ((id (tnu-insert-item "tnu-json")))
    (unwind-protect
      (let ((data (json-to-plist
                    "{\"name\": \"tnu-json\", \"completed-at\": []}")))
        (is-false (getf data :completed-at)
          "sanity: [] must decode to nil")
        (be-update :items id data "admin")
        (is-false (value-or-nil (tnu-item-completed-at id))
          ":completed-at must stay NULL through the JSON round trip"))
      (be-delete :items id "admin"))))

(test be-update-nil-text-writes-null-not-false
  "Nullable text clear-to-NULL: nil must write real NULL, not the
string \"false\" (the silent-corruption variant of the same driver
fact — bare nil binds as SQL FALSE, which text columns accept)."
  (let ((id (tnu-insert-item "tnu-text" :notes "to-be-cleared")))
    (unwind-protect
      (progn
        (is (equal "to-be-cleared" (tnu-item-notes id))
          "sanity: notes seeded")
        (be-update :items id
          '(:name "tnu-text" :notes nil)
          "admin")
        (is-false (value-or-nil (tnu-item-notes id))
          ":notes must be NULL after the clear, not \"false\": ~a"
          (tnu-item-notes id)))
      (be-delete :items id "admin"))))

;;; ---------------------------------------------------------------------------
;;; Insert path: defaults still fill, explicit nil still clears
;;; ---------------------------------------------------------------------------

(test be-insert-omitted-key-fills-compiled-default
  "The insert path the plan claims is unchanged: an omitted :notes
key takes the compiled default (\"\" on this fixture), not NULL."
  (let ((id (tnu-insert-item "tnu-default" :notes :notes-omitted)))
    (unwind-protect
      (is (equal "" (value-or-nil (tnu-item-notes id)))
        "omitted :notes must hold the compiled default \"\": ~a"
        (tnu-item-notes id))
      (be-delete :items id "admin"))))

(test be-update-not-null-nil-still-rejected
  "nil on a :not-null column is a validation error before db-value
ever runs (:name is :not-null t on this fixture)."
  (let ((id (tnu-insert-item "tnu-reject")))
    (unwind-protect
      (signals validation-error
        (be-update :items id '(:name nil) "admin"))
      (be-delete :items id "admin"))))

;;; ---------------------------------------------------------------------------
;;; Filter nil guard: is [not] null translation
;;; ---------------------------------------------------------------------------

(test be-list-eq-nil-matches-null-rows
  ":eq nil must translate to IS NULL: exactly the rows with a NULL
:completed-at return, with no 22007 / 22P02 and no literal-\"false\"
match."
  (let* ((null-id (tnu-insert-item "tnu-filter-null"))
         (set-id (tnu-insert-item "tnu-filter-set")))
    ;; give one row a timestamp, the other stays NULL
    (be-update :items set-id
      '(:name "tnu-filter-set"
        :completed-at "2026-10-01 12:00:00")
      "admin")
    (unwind-protect
      (let* ((result (be-list :items "admin"
                       :filters '((:items :completed-at :eq nil))
                       :limit 1000))
             (got (mapcar (lambda (r) (getf r :name))
                        (getf result :records))))
        (is (member "tnu-filter-null" got :test 'equal)
          "the NULL row must match :eq nil")
        (is (not (member "tnu-filter-set" got :test 'equal))
          "the non-NULL row must not match :eq nil"))
      (be-delete :items null-id "admin")
      (be-delete :items set-id "admin"))))

(test be-list-ne-nil-matches-non-null-rows
  ":ne nil must translate to IS NOT NULL."
  (let* ((null-id (tnu-insert-item "tnu-ne-null"))
         (set-id (tnu-insert-item "tnu-ne-set")))
    (be-update :items set-id
      '(:name "tnu-ne-set"
        :completed-at "2026-10-01 12:00:00")
      "admin")
    (unwind-protect
      (let* ((result (be-list :items "admin"
                       :filters '((:items :completed-at :ne nil))
                       :limit 1000))
             (got (mapcar (lambda (r) (getf r :name))
                        (getf result :records))))
        (is (member "tnu-ne-set" got :test 'equal)
          "the non-NULL row must match :ne nil")
        (is (not (member "tnu-ne-null" got :test 'equal))
          "the NULL row must not match :ne nil"))
      (be-delete :items null-id "admin")
      (be-delete :items set-id "admin"))))

(test be-list-nil-filter-keeps-placeholder-alignment
  "Placeholder alignment: a non-nil filter after a nil one must still
bind correctly. add-where-clause numbers $N from the count of values
collected; a translation that collected a stray value for the nil
filter would desynchronize the later $N (wrong bind or a bind-count
error)."
  (let ((id (tnu-insert-item "tnu-align")))
    (unwind-protect
      (let* ((result (be-list :items "admin"
                       :filters `((:items :completed-at :eq nil)
                                  (:items :name :eq "tnu-align"))
                       :limit 1000))
             (got (mapcar (lambda (r) (getf r :name))
                        (getf result :records))))
        (is (equal '("tnu-align") got)
          "exactly the one NULL+name row must return: ~a" got))
      (be-delete :items id "admin"))))

;;; ---------------------------------------------------------------------------
;;; List-filter nil elements are rejected
;;; ---------------------------------------------------------------------------

(test be-list-in-with-nil-element-rejects
  "A nil element inside :in is a validation error, not a silent
in (NULL) that matches nothing (option a, plan fix-chores)."
  (signals validation-error
    (be-list :items "admin"
      :filters '((:items :notes :in ("x" nil))))))

(test valid-filter-in-lone-nil-element-rejects
  "(nil) alone is likewise rejected — the atom path and the whole-value
path share filter-value-type-p."
  (signals validation-error
    (valid-filter '(:items :notes :in (nil))))
  ;; :eq nil itself stays legal (that is the is-null feature)
  (is (null (valid-filter '(:items :notes :eq nil)))))
