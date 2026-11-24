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

;;; --- Ordinary :eq / :ne on singular-target fields (the
;;; same-type :eq repair: storage-column compare, display/uuid
;;; resolution, both the lone 500 and the joined silent zero) ---

(test eq-storage-sql-text-base
  "A lone same-type (:comments :post :eq uuid) row: the condition
aliases the physical storage column (rt_comments.comment_post = $1),
binds the uuid, and needs no join (filters-require-join-p nil) —
the old code emitted the display column rt_posts.post_title here
and Postgres 500'd with 42P01 (missing FROM-clause entry)."
  (let* ((sql (u:tree-get *compiled-model* :comments
                :views :main :phase-a-base-sql))
         (a-id (getf *fixture* :post-a))
         (result (add-where-clause sql
                    `((:comments :post :eq ,a-id)) "admin")))
    (is (search "rt_comments.comment_post = $1" (car result)
          :test #'char-equal))
    (is (equal (list a-id) (cdr result)))
    (is-false (filters-require-join-p :comments
                `((:comments :post :eq ,a-id))))
    (is (search "rt_comments" (car result) :test #'char-equal))
    (is-false (search "rt_posts" (car result) :test #'char-equal))))

(test eq-storage-be-list-matches
  "be-list with (:comments :post :eq a-uuid) returns all three
comments (c1/c2/c3) with :total 3 — the lone-row 500 is gone."
  (let ((a-id (getf *fixture* :post-a)))
    (let ((r (be-list :comments "admin"
                :filters `((:comments :post :eq ,a-id)))))
      (is (= 3 (getf r :total)))
      (is (equal '("c1" "c2" "c3")
              (sort (mapcar (lambda (x) (getf x :title))
                       (getf r :records))
                #'string<))))))

(test eq-storage-crosspost-display-and-uuid
  "(:comments :crosspost :eq \"Post B\") and (:comments :crosspost
:eq b-uuid) both select the crosspost storage column, resolve to
B's uuid, and return only c3 — display name and uuid are
equivalent spellings, and the field-specific storage column is
selected (:post never matches, :crosspost does)."
  (let ((b-id (getf *fixture* :post-b)))
    (dolist (value (list "Post B" b-id))
      (let ((r (be-list :comments "admin"
                  :filters `((:comments :crosspost :eq ,value)))))
        (is (= 1 (getf r :total)))
        (is (equal '("c3")
                (mapcar (lambda (x) (getf x :title))
                  (getf r :records))))))
    (let* ((sql (u:tree-get *compiled-model* :comments
                   :views :main :phase-a-base-sql))
           (result (add-where-clause sql
                      `((:comments :crosspost :eq "Post B")) "admin")))
      (is (search "rt_comments.comment_crosspost = $1" (car result)
            :test #'char-equal))
      (is (equal (list b-id) (cdr result))
        "the display name resolves to the uuid before the bind"))))

(test eq-ne-storage-be-list
  ":ne with both spellings on :post: the != storage-column
condition, resolved uuid parameter, and all three comments back
(none of them has post = B)."
  (let ((b-id (getf *fixture* :post-b)))
    (dolist (value (list "Post B" b-id))
      (let* ((sql (u:tree-get *compiled-model* :comments
                     :views :main :phase-a-base-sql))
             (result (add-where-clause sql
                        `((:comments :post :ne ,value)) "admin"))
             (r (be-list :comments "admin"
                   :filters `((:comments :post :ne ,value)))))
        (is (search "rt_comments.comment_post != $1" (car result)
              :test #'char-equal))
        (is (equal (list b-id) (cdr result)))
        (is (= 3 (getf r :total)))
        (is (equal '("c1" "c2" "c3")
                (sort (mapcar (lambda (x) (getf x :title))
                         (getf r :records))
                  #'string<)))))))

(test eq-uuid-display-collision
  "The collision pin: (:comments :post :eq b-uuid) returns zero
even though post A's title IS that uuid — the compare is the
storage column (uuid), never the display column (a display-column
compare would match all of A's comments; the old joined path did
exactly that in reverse and silently matched zero)."
  (let ((b-id (getf *fixture* :post-b)))
    (let ((r (be-list :comments "admin"
                :filters `((:comments :post :eq ,b-id)))))
      (is (zerop (getf r :total)))
      (is-false (getf r :records)))))

(test eq-joined-beside-sibling
  "The joined cases: a join-forcing sibling (:posts :title :eq)
beside the same-type row — the old silent zero. The :posts join
rides :post's own FK (rt_posts joined on comment_post), so a
title sibling selects comments whose :post IS that post: A's
title (literally B's uuid, bound as a-title) selects all three,
\"Post B\" selects none. Joined :post :eq a-id beside A's title
row returns all three; joined :post :ne \"Post B\" beside A's
title row returns all three; joined :crosspost :eq \"Post B\"
beside A's title row returns only c3 (the crosspost leg never
rides the B-title row — no comment's :post is B); joined
:crosspost :ne b-id beside A's title row returns c1/c2. Each
reference condition names its qualified rt_comments FK column;
parameters remain in filter order."
  (let* ((a-id (getf *fixture* :post-a))
         (b-id (getf *fixture* :post-b))
         (a-title b-id) ; A's display title is literally B's uuid
         (sql (u:tree-get *compiled-model* :comments
                :views :main :phase-a-join-sql)))
    ;; joined :post :eq a-id beside A's title row
    (let ((result (add-where-clause sql
                    `((:posts :title :eq ,a-title)
                      (:comments :post :eq ,a-id))
                    "admin"))
          (r (be-list :comments "admin"
               :filters `((:posts :title :eq ,a-title)
                          (:comments :post :eq ,a-id)))))
      (is (search "rt_comments.comment_post = $2" (car result)
            :test #'char-equal))
      (is (equal (list a-title a-id) (cdr result)))
      (is (= 3 (getf r :total)))
      (is (equal '("c1" "c2" "c3")
              (sort (mapcar (lambda (x) (getf x :title))
                       (getf r :records))
                #'string<))))
    ;; joined :post :ne "Post B" beside A's title row
    (let ((result (add-where-clause sql
                    `((:posts :title :eq ,a-title)
                      (:comments :post :ne "Post B"))
                    "admin"))
          (r (be-list :comments "admin"
               :filters `((:posts :title :eq ,a-title)
                          (:comments :post :ne "Post B")))))
      (is (search "rt_comments.comment_post != $2" (car result)
            :test #'char-equal))
      (is (equal (list a-title b-id) (cdr result))
        "the display name resolves to B's uuid before the bind")
      (is (= 3 (getf r :total)))
      (is (equal '("c1" "c2" "c3")
              (sort (mapcar (lambda (x) (getf x :title))
                       (getf r :records))
                #'string<))))
    ;; joined :crosspost :eq "Post B" beside A's title row
    (let ((result (add-where-clause sql
                    `((:posts :title :eq ,a-title)
                      (:comments :crosspost :eq "Post B"))
                    "admin"))
          (r (be-list :comments "admin"
               :filters `((:posts :title :eq ,a-title)
                          (:comments :crosspost :eq "Post B")))))
      (is (search "rt_comments.comment_crosspost = $2" (car result)
            :test #'char-equal))
      (is (equal (list a-title b-id) (cdr result)))
      (is (= 1 (getf r :total)))
      (is (equal '("c3")
              (mapcar (lambda (x) (getf x :title))
                (getf r :records)))))
    ;; joined :crosspost :ne b-id beside A's title row: c1/c2 have
    ;; NULL crossposts, and NULL != B is UNKNOWN (three-valued
    ;; logic), not TRUE — they drop. Only c3 has a non-null
    ;; crosspost, and it equals B, so zero rows. The positive
    ;; spelling :crosspost :ne a-id keeps c3 (B ≠ A, TRUE).
    (let ((result (add-where-clause sql
                    `((:posts :title :eq ,a-title)
                      (:comments :crosspost :ne ,b-id))
                    "admin"))
          (r (be-list :comments "admin"
               :filters `((:posts :title :eq ,a-title)
                          (:comments :crosspost :ne ,b-id))))
          (r-a (be-list :comments "admin"
                 :filters `((:posts :title :eq ,a-title)
                            (:comments :crosspost :ne ,a-id)))))
      (is (search "rt_comments.comment_crosspost != $2" (car result)
            :test #'char-equal))
      (is (zerop (getf r :total))
        "NULL != B is UNKNOWN: the NULL-crosspost rows drop")
      (is (= 1 (getf r-a :total)))
      (is (equal '("c3")
              (mapcar (lambda (x) (getf x :title))
                (getf r-a :records)))))))

(test eq-joined-two-fk-fields-one-query
  ":post :eq a-id AND :crosspost :eq \"Post B\" in one query: both
distinct FK conditions present, both resolved parameters, and only
c3 (the one row matching both)."
  (let* ((a-id (getf *fixture* :post-a))
         (b-id (getf *fixture* :post-b))
         (sql (u:tree-get *compiled-model* :comments
                :views :main :phase-a-base-sql))
         (result (add-where-clause sql
                    `((:comments :post :eq ,a-id)
                      (:comments :crosspost :eq "Post B"))
                    "admin"))
         (r (be-list :comments "admin"
              :filters `((:comments :post :eq ,a-id)
                         (:comments :crosspost :eq "Post B")))))
    (is (search "rt_comments.comment_post = $1" (car result)
          :test #'char-equal))
    (is (search "rt_comments.comment_crosspost = $2" (car result)
          :test #'char-equal))
    (is (equal (list a-id b-id) (cdr result)))
    (is (= 1 (getf r :total)))
    (is (equal '("c3")
            (mapcar (lambda (x) (getf x :title))
              (getf r :records))))))

(test eq-null-storage-preserved
  "Null arms ride the same storage predicate: :crosspost :eq nil
emits IS NULL with no value parameter and returns c1/c2;
:crosspost :ne :null emits IS NOT NULL and returns c3. A
joined-sibling null assertion keeps the two-FK/null guarantee
exercised beside the broadened storage predicate."
  (let* ((sql (u:tree-get *compiled-model* :comments
                 :views :main :phase-a-base-sql))
         (eq-result (add-where-clause sql
                      `((:comments :crosspost :eq nil)) "admin"))
         (ne-result (add-where-clause sql
                      `((:comments :crosspost :ne :null)) "admin"))
         (eq-r (be-list :comments "admin"
                  :filters `((:comments :crosspost :eq nil))))
         (ne-r (be-list :comments "admin"
                  :filters `((:comments :crosspost :ne :null))))
         (join-sql (u:tree-get *compiled-model* :comments
                      :views :main :phase-a-join-sql))
         (join-result (add-where-clause join-sql
                        `((:posts :title :eq "Post B")
                          (:comments :crosspost :eq nil))
                        "admin"))
         (join-r (be-list :comments "admin"
                    :filters `((:posts :title :eq "Post B")
                               (:comments :crosspost :eq nil)))))
    (is (search "rt_comments.comment_crosspost is null" (car eq-result)
          :test #'char-equal))
    (is (null (cdr eq-result)))
    (is (= 2 (getf eq-r :total)))
    (is (equal '("c1" "c2")
            (sort (mapcar (lambda (x) (getf x :title))
                     (getf eq-r :records))
              #'string<)))
    (is (search "rt_comments.comment_crosspost is not null"
          (car ne-result) :test #'char-equal))
    (is (null (cdr ne-result)))
    (is (= 1 (getf ne-r :total)))
    (is (equal '("c3")
            (mapcar (lambda (x) (getf x :title))
              (getf ne-r :records))))
    (is (search "rt_comments.comment_crosspost is null" (car join-result)
          :test #'char-equal))
    (is (equal '("Post B") (cdr join-result)))
    (is (= 0 (getf join-r :total))
      "beside the B-title join row: no comment has both crosspost
B and crosspost null")))

(test eq-unknown-display-name-rejected
  "Unknown non-UUID display names for both :eq and :ne signal
validation-error through be-list (resolve-reference-id's unknown-
value report; rest-list maps it to the 400)."
  (signals validation-error
    (be-list :comments "admin"
      :filters '((:comments :post :eq "No Such Post"))))
  (signals validation-error
    (be-list :comments "admin"
      :filters '((:comments :crosspost :ne "No Such Post")))))

(test eq-unknown-uuid-matches-zero
  "An unknown well-formed uuid under :eq returns zero, no signal —
no existence lookup was added (mirrors fk-eq-unknown-uuid-empty)."
  (let ((r (be-list :comments "admin"
              :filters
              '((:comments :post :eq
                 "00000000-0000-0000-0000-000000000000")))))
    (is (zerop (getf r :total)))
    (is-false (getf r :records))))

(test eq-guest-rbac-composition
  "RBAC composition: guest (seeded with the public role) sees only
c1 and c3 (their resources temporarily carry public) under :post
:eq a-uuid, with :total 2, while the admin control still returns
all three — the corrected storage predicate composes with
base-be-list's readable-ID filter, not just admin-visible rows.
No fixture or seed-data change; unwind-protect restores."
  (let* ((a-id (getf *fixture* :post-a))
         (c1 (getf *fixture* :c1))
         (c3 (getf *fixture* :c3)))
    (a:add-resource-role *rbac* (id-to-resource-name c1) "public")
    (a:add-resource-role *rbac* (id-to-resource-name c3) "public")
    (unwind-protect
        (progn
          (let ((r (be-list :comments "guest"
                      :filters `((:comments :post :eq ,a-id)))))
            (is (= 2 (getf r :total)))
            (is (equal '("c1" "c3")
                    (sort (mapcar (lambda (x) (getf x :title))
                             (getf r :records))
                      #'string<))))
          (is (= 3 (getf (be-list :comments "admin"
                           :filters `((:comments :post :eq ,a-id)))
                    :total))))
      (a:remove-resource-role *rbac* (id-to-resource-name c1) "public")
      (a:remove-resource-role *rbac* (id-to-resource-name c3)
        "public"))))

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
