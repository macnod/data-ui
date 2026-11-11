(in-package :data-ui)

(def-suite fk-dedup-suite
  :description "id-from-data dedup on singular-target fields: the
storage-column compare (:fk-eq with the resolved uuid), the null-FK
IS NULL arm, display-name resolution, the distinct-payload unique
error, and a base-type target")

(in-suite fk-dedup-suite)

;;; The seeded fk-eq-test context (seed-fk-eq-fixture): post A's
;;; title is literally post B's uuid, so uuid spellings name rows
;;; exactly and only "Post B" is a safe display spelling.

(test fkd-uuid-duplicate-inserted-nil
  "The scenebook repro: the same data twice — title, body, and the FK
as a uuid — the second be-insert returns the first row's id with
inserted NIL (before the fix, the :eq dedup compared the display
column against a uuid, never matched, and the real INSERT died on
the title unique index)."
  (let* ((a-id (getf *fixture* :post-a))
         (data `(:title "dupe-uuid" :body "same body" :post ,a-id)))
    (multiple-value-bind (id-1 inserted-1)
        (be-insert :comments data "admin")
      (is-true (uuid-p id-1))
      (is-true inserted-1)
      (multiple-value-bind (id-2 inserted-2)
          (be-insert :comments data "admin")
        (is (equal id-1 id-2))
        (is-false inserted-2)
        (is (= 1 (getf (be-list :comments "admin"
                          :filters
                          `((:comments :title :eq "dupe-uuid")))
                    :total)))))))

(test fkd-display-name-duplicate
  "The display-name spelling dedups too: resolve-reference-id turns
the wire title into the target uuid before the :fk-eq compare, the
same resolution the INSERT itself performs."
  (let ((data '(:title "dupe-display" :body "same body" :post "Post B")))
    (multiple-value-bind (id-1 inserted-1)
        (be-insert :comments data "admin")
      (is-true inserted-1)
      (multiple-value-bind (id-2 inserted-2)
          (be-insert :comments data "admin")
        (is (equal id-1 id-2))
        (is-false inserted-2)))))

(test fkd-distinct-fk-still-signals
  "Same identity, different FK: the dedup keys differ, so the lookup
misses on purpose and the database's unique index rejects the insert
(the sty-unique-index doctrine carried to FK types — a differing
payload stays a loud error, never a silent dedup)."
  (let ((a-id (getf *fixture* :post-a))
        (b-id (getf *fixture* :post-b)))
    (is-true (be-insert :comments
                `(:title "clash" :body "on A" :post ,a-id) "admin"))
    (signals error
      (be-insert :comments `(:title "clash" :body "on B" :post ,b-id)
        "admin"))))

(test fkd-null-fk-duplicate
  "A null FK keeps the :eq IS NULL arm: two identical inserts with
:post nil collapse to one row (the pre-fix behavior, unchanged —
valid-filter rejects a null :fk-eq, so null rides the ordinary
path)."
  (let ((data '(:title "dupe-null" :body "no post")))
    (multiple-value-bind (id-1 inserted-1)
        (be-insert :comments data "admin")
      (is-true inserted-1)
      (multiple-value-bind (id-2 inserted-2)
          (be-insert :comments data "admin")
        (is (equal id-1 id-2))
        (is-false inserted-2)))))

(test fkd-mixed-fks-one-null
  "One lookup with both arms at once: :post resolved through :fk-eq,
:crosspost nil through IS NULL — the duplicate collapses; the two
local fields beside them ride ordinary :eq filters."
  (let* ((a-id (getf *fixture* :post-a))
         (data `(:title "dupe-mixed" :body "both legs" :post ,a-id
                 :crosspost nil)))
    (multiple-value-bind (id-1 inserted-1)
        (be-insert :comments data "admin")
      (is-true inserted-1)
      (multiple-value-bind (id-2 inserted-2)
          (be-insert :comments data "admin")
        (is (equal id-1 id-2))
        (is-false inserted-2)))))

(test fkd-id-from-data-two-fks
  "Direct unit probe: a row written with two uuid FKs through insert
data is found by id-from-data given the same uuids (before the fix
this returned NIL for every FK-carrying type)."
  (let* ((a-id (getf *fixture* :post-a))
         (b-id (getf *fixture* :post-b))
         (id (be-insert :comments
               `(:title "probe" :body "two fks" :post ,a-id
                 :crosspost ,b-id)
               "admin")))
    (is-true (uuid-p id))
    (is (equal id
           (id-from-data :comments
             `(:title "probe" :body "two fks" :post ,a-id
               :crosspost ,b-id))))))

(test fkd-id-from-data-out-of-band-fk
  "The lookup also finds rows whose FK was written out-of-band
(be-set-field-value, the seeding path the fk-eq fixture uses) — the
storage column holds the uuid either way. c2: post A, no crosspost,
no flag."
  (let ((a-id (getf *fixture* :post-a))
        (c2-id (getf *fixture* :c2)))
    (is (equal c2-id
           (id-from-data :comments
             `(:title "c2" :body "second comment on A" :post ,a-id))))))

;;; --- Base-type target (nullable-fk-test context) ---

(def-suite fk-dedup-users-suite
  :description "id-from-data dedup against a base-type target
(:users) on the nullable-fk-test fixture")

(in-suite fk-dedup-users-suite)

(defun seed-fk-dedup-users-fixture ()
  "Seed the nullable-fk-test context: one user the :assigned-to
display spelling can resolve to. Returns the user id bound to
*fixture*."
  (th-make-user "fk-dedup-user"))

(test fkd-users-target-duplicate
  "A :users-targeting FK: the display-name spelling resolves through
the base type's identity column, and the duplicate insert collapses
to one row with inserted NIL."
  (let ((data '(:name "t1" :assigned-to "fk-dedup-user")))
    (multiple-value-bind (id-1 inserted-1)
        (be-insert :tasks data "admin")
      (is-true (uuid-p id-1))
      (is-true inserted-1)
      (multiple-value-bind (id-2 inserted-2)
          (be-insert :tasks data "admin")
        (is (equal id-1 id-2))
        (is-false inserted-2)))))
