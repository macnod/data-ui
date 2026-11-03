(in-package :data-ui)

(def-suite fk-eq-suite
  :description ":fk-eq filter operator: SQL text, raw binding,
storage-column matches, guards, rejections, predicate shape")

(in-suite fk-eq-suite)

(defun seed-fk-eq-fixture ()
  "Seed the fk-eq-test fixture. Posts A and B; the collision probe:
post B first, then post A whose :title is literally B's uuid, with
every comment attached only to A via be-set-field-value (a
uuid-looking title fed through be-insert data would take the
resolve-reference-id uuid short-circuit and attach to the row with
that uuid — the exact bug the probe pins). c3 also carries the
:crosspost leg of the two-FK probe. Returns the id plist bound to
*fixture*."
  (let* ((b-id (be-insert :posts '(:title "Post B") "admin"))
         (a-id (be-insert :posts `(:title ,b-id) "admin"))
         (flag-id (be-insert :flags '(:name "important") "admin"))
         (c1-id (be-insert :comments
                   '(:title "c1" :body "first comment on A")
                   "admin"))
         (c2-id (be-insert :comments
                   '(:title "c2" :body "second comment on A")
                   "admin"))
         (c3-id (be-insert :comments
                   '(:title "c3" :body "crosspost comment")
                   "admin")))
    ;; uuid-valued FKs are set with be-set-field-value, never
    ;; through be-insert data (record-permalink-tests precedent:
    ;; single-column update with no reference resolution).
    (be-set-field-value :comments c1-id :post a-id "admin")
    (be-set-field-value :comments c2-id :post a-id "admin")
    (be-set-field-value :comments c3-id :post a-id "admin")
    (be-set-field-value :comments c3-id :crosspost b-id "admin")
    (be-set-field-value :comments c1-id :flag flag-id "admin")
    (list :post-a a-id :post-b b-id :flag flag-id
      :c1 c1-id :c2 c2-id :c3 c3-id)))

;;; --- SQL text and binding ---

(test fk-eq-sql-text-base
  "add-where-clause on phase-a-base-sql emits the table-qualified
physical column against $1, with the raw uuid in the cdr; the row
does not force join SQL (filters-require-join-p nil)."
  (let* ((sql (u:tree-get *compiled-model* :comments
                :views :main :phase-a-base-sql))
         (a-id (getf *fixture* :post-a))
         (result (add-where-clause sql
                    `((:comments :post :fk-eq ,a-id)) "admin")))
    (is (search "where" (car result) :test #'char-equal))
    (is (search "rt_comments.comment_post = $1" (car result)
          :test #'char-equal))
    (is (equal (list a-id) (cdr result)))
    (is-false (filters-require-join-p :comments
                `((:comments :post :fk-eq ,a-id))))))

(test fk-eq-sql-text-after-in-filter
  "Two-filter placeholder alignment: an ordinary value-bearing
filter first, the :fk-eq placeholder is the next index and params
retain filter order."
  (let* ((sql (u:tree-get *compiled-model* :comments
                :views :main :phase-a-base-sql))
         (a-id (getf *fixture* :post-a))
         (result (add-where-clause sql
                    `((:comments :title :in ("c1" "c2"))
                      (:comments :post :fk-eq ,a-id))
                    "admin")))
    (is (search "rt_comments.comment_post = $3" (car result)
          :test #'char-equal))
    (is (equal (list "c1" "c2" a-id) (cdr result)))))

(test fk-eq-raw-binding-no-db-value
  "The bind is the wire value, unconverted: a known non-uuid post
title fed straight to add-where-clause stays that title in the
params (a path through db-value would resolve it to the post's
id). Valid-uuid probes cannot distinguish the two."
  (let* ((sql (u:tree-get *compiled-model* :comments
                :views :main :phase-a-base-sql))
         (result (add-where-clause sql
                    `((:comments :post :fk-eq "Post B")) "admin")))
    (is (equal (list "Post B") (cdr result)))
    ;; and the validator rejects that same title
    (signals validation-error
      (valid-filter '(:comments :post :fk-eq "Post B")))))

(test fk-eq-sql-text-join-context
  "The qualified physical column also renders when another row
forces phase-a-join-sql: beside the joined chip condition, with
params in filter order."
  (let* ((sql (u:tree-get *compiled-model* :comments
                :views :main :phase-a-join-sql))
         (a-id (getf *fixture* :post-a))
         (b-id (getf *fixture* :post-b))
         (result (add-where-clause sql
                    `((:posts :title :eq ,b-id)
                      (:comments :post :fk-eq ,a-id))
                    "admin")))
    (is (search "rt_comments.comment_post = $2" (car result)
          :test #'char-equal))
    (is (equal (list b-id a-id) (cdr result)))))

;;; --- Storage-column matches ---

(test fk-eq-be-list-matches
  "be-list returns exactly the parent's comments with the right
:total, coexisting with ordinary filters, :created-at sort
(injected :sortable t), and limit/offset."
  (let* ((a-id (getf *fixture* :post-a))
         (r (be-list :comments "admin"
               :filters `((:comments :post :fk-eq ,a-id))
               :sort '(:created-at :asc))))
    (is (= 3 (getf r :total)))
    (is (equal '("c1" "c2" "c3")
            (mapcar (lambda (x) (getf x :title))
              (getf r :records))))
    ;; limit/offset page inside the match
    (let ((page (be-list :comments "admin"
                  :filters `((:comments :post :fk-eq ,a-id))
                  :sort '(:created-at :asc)
                  :limit 1 :offset 1)))
      (is (= 3 (getf page :total)))
      (is (equal '("c2")
              (mapcar (lambda (x) (getf x :title))
                (getf page :records)))))))

(test fk-eq-view-sql-context
  "View SQL (the :views :main :sql FROM shape, used by be-id and
internally by rec): the qualified physical column resolves there
too. Seed/select exactly one match; be-id signals on many."
  (let* ((a-id (getf *fixture* :post-a))
         (sql (u:tree-get *compiled-model* :comments
                :views :main :sql))
         (result (add-where-clause sql
                    `((:comments :post :fk-eq ,a-id)
                      (:comments :title :eq "c1"))
                    "admin")))
    (is (search "rt_comments.comment_post = $1" (car result)
          :test #'char-equal))
    (is (equal (getf *fixture* :c1)
            (be-id :comments
              `((:comments :post :fk-eq ,a-id)
                (:comments :title :eq "c1"))
              "admin")))))

(test fk-eq-join-sql-execution
  "be-list pairing the listed-type :fk-eq row with an ordinary
target-keyed chip (the chip forces phase-a-join-sql — the comments
tab's post-scope-plus-author-chip combination). The chip value is
post A's display title, which is literally B's uuid, and the
storage filter still selects A's rows: display chip and storage
filter agree on A despite the collision."
  (let* ((a-id (getf *fixture* :post-a))
         (b-id (getf *fixture* :post-b))
         (r (be-list :comments "admin"
               :filters `((:posts :title :eq ,b-id)
                          (:comments :post :fk-eq ,a-id)))))
    (is (= 3 (getf r :total)))
    (is (equal '("c1" "c2" "c3")
            (sort (mapcar (lambda (x) (getf x :title))
                     (getf r :records))
              #'string<)))))

(test fk-eq-title-collision
  "Post B, then a post A whose title is literally B's uuid, with
every comment attached only to A: :fk-eq with B's uuid matches
nothing — the compare is storage, not display (a display-column
compare with B's uuid would match all of A's comments, whose post
title IS that uuid)."
  (let* ((b-id (getf *fixture* :post-b))
         (a-id (getf *fixture* :post-a))
         (r (be-list :comments "admin"
               :filters `((:comments :post :fk-eq ,b-id)))))
    (is (zerop (getf r :total)))
    (is-false (getf r :records))
    ;; and the storage filter on A's own uuid finds all three
    (is (= 3 (getf (be-list :comments "admin"
                     :filters `((:comments :post :fk-eq ,a-id)))
              :total)))))

(test fk-eq-unknown-uuid-empty
  "Unknown well-formed uuid: empty records, :total 0, no error (not
a valid-existing-uuid 400)."
  (let ((r (be-list :comments "admin"
              :filters
              '((:comments :post :fk-eq
                 "00000000-0000-0000-0000-000000000000")))))
    (is (zerop (getf r :total)))
    (is-false (getf r :records))))

(test fk-eq-two-fks-distinct-columns
  "Two FKs to one target (:post + :crosspost) are distinct physical
columns and each filters only its own rows — the suppression shape
suppresses permalink siblings, never storage columns."
  (let* ((a-id (getf *fixture* :post-a))
         (b-id (getf *fixture* :post-b))
         (by-post-a (be-list :comments "admin"
                       :filters `((:comments :post :fk-eq ,a-id))))
         (by-post-b (be-list :comments "admin"
                       :filters `((:comments :post :fk-eq ,b-id))))
         (by-cross-a (be-list :comments "admin"
                       :filters
                       `((:comments :crosspost :fk-eq ,a-id))))
         (by-cross-b (be-list :comments "admin"
                       :filters
                       `((:comments :crosspost :fk-eq ,b-id)))))
    (is (= 3 (getf by-post-a :total)))
    (is (zerop (getf by-post-b :total)))
    (is (zerop (getf by-cross-a :total)))
    (is (= 1 (getf by-cross-b :total)))
    (is (equal "c3"
            (getf (car (getf by-cross-b :records)) :title)))))

;;; --- Guards and rejections ---

(test fk-eq-cross-type-rejected
  "A tuple naming another type while listing :posts is rejected by
valid-filters' :type-key guard with the message naming both types
(rejection precedes SQL selection)."
  (let ((a-id (getf *fixture* :post-a)))
    (signals validation-error
      (valid-filters `((:comments :post :fk-eq ,a-id))
        :type-key :posts))
    (handler-case
      (progn
        (be-list :posts "admin"
          :filters `((:comments :post :fk-eq ,a-id)))
        (fail "be-list :posts with cross-type :fk-eq must reject"))
      (validation-error (e)
        (is (re:scan "field-scoped to the listed type"
              (format nil "~a" e)))))))

(test fk-eq-valid-filter-rejections
  "valid-filter rejects: non-FK field (:body), nil / :null /
title-string values, the M2M :tags field, the injected :id, and the
target-keyed chip spelling (identity columns are not FK fields)."
  (let ((a-id (getf *fixture* :post-a)))
    (signals validation-error
      (valid-filter `(:comments :body :fk-eq ,a-id)))
    (signals validation-error
      (valid-filter '(:comments :post :fk-eq nil)))
    (signals validation-error
      (valid-filter '(:comments :post :fk-eq :null)))
    (signals validation-error
      (valid-filter '(:comments :post :fk-eq "Post B")))
    (signals validation-error
      (valid-filter `(:comments :tags :fk-eq ,a-id)))
    (signals validation-error
      (valid-filter `(:comments :id :fk-eq ,a-id)))
    (signals validation-error
      (valid-filter `(:posts :title :fk-eq ,a-id)))))

(test fk-eq-filter-p-shape-split
  "filter-p accepts a title string and a uuid string on the
text-declared :post AND on the boolean-declared :flag (the
string-shaped arm — shape only, uuid-p semantics in valid-filter),
so nobody later tightens the predicate either way."
  (let ((a-id (getf *fixture* :post-a)))
    (is-true (filter-p `(:comments :post :fk-eq "Post B")))
    (is-true (filter-p `(:comments :post :fk-eq ,a-id)))
    (is-true (filter-p `(:comments :flag :fk-eq "Post B")))
    (is-true (filter-p `(:comments :flag :fk-eq ,a-id)))
    ;; non-strings still fail the shape check
    (is-false (filter-p `(:comments :post :fk-eq :null)))
    (is-false (filter-p `(:comments :post :fk-eq nil)))))

(test fk-eq-parse-operator
  "The REST parser accepts the token with no rest.lisp edit."
  (is (eq :fk-eq (parse-operator "fk-eq"))))

(test fk-eq-operator-sql-inert
  "operator-sql answers the inert token (never spliced as SQL by
add-where-clause — its fk-eq arm builds the condition itself)."
  (is (equal "fk-eq" (operator-sql :fk-eq)))
  (is-true (operator-key-p :fk-eq)))
