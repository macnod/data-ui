(in-package :data-ui)

(def-suite sort-measures-suite
  :description "09 sort + page on measures: compile relaxation,
SELECT-alias ORDER BY, grain-id tiebreaker, NULLS LAST on :avg,
default first-sortable-measure DESC, :sort echo on both paths")

(in-suite sort-measures-suite)

;;; --- Shared helpers ---

(defun sms-seed-fixture ()
  "Seed the rollup-sort-test fixture (plan 09):

- alice: 3 chores — 2 completed (3 + 4 pts), 1 incomplete (2 pts)
- bob:   2 completed chores, both 3 pts (ties alice's total? no: 6)
- carol: 2 completed chores at 3 pts (ties bob exactly — tiebreaker)
- dave:  no chores (zero-fact row; :avg is NULL)"
  (dolist (u '("alice" "bob" "carol" "dave"))
    (th-make-user u :roles '("chore-user" "leaderboard-viewers"
                             "rating-user")))
  (be-insert :chores '(:title "dishes" :user "alice" :points 3
                     :completed :true) "admin")
  (be-insert :chores '(:title "laundry" :user "alice" :points 4
                     :completed :true) "admin")
  (be-insert :chores '(:title "trash" :user "alice" :points 2
                     :completed :false) "admin")
  ;; bob and carol tie: 6 points, 2 done — the grain-id tiebreaker
  ;; keeps their relative page order stable.
  (be-insert :chores '(:title "b1" :user "bob" :points 3
                     :completed :true) "admin")
  (be-insert :chores '(:title "b2" :user "bob" :points 3
                     :completed :true) "admin")
  (be-insert :chores '(:title "c1" :user "carol" :points 3
                     :completed :true) "admin")
  (be-insert :chores '(:title "c2" :user "carol" :points 3
                     :completed :true) "admin")
  nil)

(defun sms-record-by-name (result name)
  (find name (getf result :records)
    :key (lambda (r) (getf r :name)) :test #'equal))

(defun sms-names (result)
  (loop for r in (getf result :records) collect (getf r :name)))

(defun sms-capture-page-sql (thunk)
  "Run THUNK (e.g. a be-list call) and return the page SQL strings
measure-be-list logged. Adds a temporary string-stream p-log sink
(plain format: key=value; key=value entries) around the call and
extracts everything between 'page-sql=' and the following
'; count-sql=' (the SQL itself spans multiple lines). Returns nil
when nothing matched."
  (let ((stream (make-string-output-stream)))
    (pl:make-log-stream "sms-test" stream
      :log-format :plain :severity-threshold :debug)
    (unwind-protect
      (progn
        (funcall thunk)
        (force-output stream)
        (let* ((text (get-output-stream-string stream))
               (pos (search "in=measure-be-list" text)))
          (when pos
            (let* ((p (search "page-sql=" text :start2 pos))
                   (rest (subseq text (+ p (length "page-sql="))))
                   (end (or (search "; count-sql=" rest) (length rest))))
              (list (subseq rest 0 end))))))
      (pl:close-log-stream "sms-test"))))

(defun sms-page-sql (rollup-key &key sort (limit 20) (offset 0))
  "Run be-list on ROLLUP-KEY and return the page SQL string
measure-be-list logged (the value between the page-sql= and
count-sql= log keys)."
  (first (sms-capture-page-sql
           (lambda ()
             (be-list rollup-key "admin" :sort sort
               :limit limit :offset offset)))))

;;; --- Compiler: :sortable relaxation ---

(test sms-sortable-compiles-on-rollup-fields
  ":sortable t compiles on rollup grain pass-throughs and :sum /
:count / :avg measures (09 Step 1)."
  (is-true (u:tree-get *compiled-model* :user-leaderboard :fields
              :name :sortable))
  (is-true (u:tree-get *compiled-model* :user-leaderboard :fields
              :total-points :sortable))
  (is-true (u:tree-get *compiled-model* :user-leaderboard :fields
              :chores-done :sortable))
  (is-true (u:tree-get *compiled-model* :titles-board :fields
              :avg-points :sortable)))

(test sms-sortable-list-distinct-rejected
  ":sortable t on a :list / :distinct measure is a compile error
(09 Step 1)."
  (signals error
    (compile-model
      (append (ru-base-model)
        (list :user-leaderboard
          (ru-with (ru-leaderboard)
            (list :fields
              (list
                (list :name
                  (list :source (list :view :main :table :users
                                  :column :name :agg :first)))
                (list :titles
                  (list :type :text
                    :source (list :view :main :table :chores
                              :column :title :agg :list)
                    :sortable t))))))))))

(test sms-sortable-hybrid-rejected
  ":sortable t on a hybrid Phase B measure (base type) stays a
compile error — the relaxation does not leak off rollups (Issue 12)."
  (signals error
    (compile-model
      (append (ru-base-model)
        (list :hybrid
          (list :table t :create :auto :update :auto :delete :auto
            :views (list :main (list :tables '(:hybrid :users)))
            :fields (list
                      :user (list :type :text :target :users
                              :source (list :view :main :table :users
                                        :column :name :agg :first)
                              :column t :not-null t)
                      :avg-value (list :type :real
                                   :source (list :view :main :table
                                             :hybrid :column :points
                                             :agg :avg)
                                   :sortable t))
            :list-form (list :fields t)
            :update-form (list :fields t)
            :add-form (list :fields t)))))))

(test sms-sortable-unsortable-measure-at-runtime
  "Non-sortable rollup fields (e.g. :titles on titles-board) reject
at request time with report-ve."
  (signals validation-error
    (valid-measure-sort :titles-board '(:titles :asc))))

;;; --- Backend: ORDER BY shape ---

(test sms-order-by-uses-select-alias
  "Sort by a measure emits ORDER BY on the SELECT alias
(to-sql-identifier field-key) plus the grain-id tiebreaker — never
:source :column-name (the fact column), never :name-sql."
  (let ((sql (sms-page-sql :user-leaderboard :sort '(:total-points :asc))))
    (is-true (search "order by total_points asc, users.id" sql))))

(test sms-order-by-passthrough-uses-select-alias
  "Sort by a grain pass-through resolves the SELECT alias (name), not
:source :column-name (users.user_name)."
  (let ((sql (sms-page-sql :user-leaderboard :sort '(:name :desc))))
    (is-true (search "order by name desc, users.id" sql))))

(test sms-default-sort-first-sortable-measure
  "Nil sort on a rollup with sortable measures: first sortable
measure DESC (declaration order), then grain id. Worked example:
:total-points before :chores-done."
  (let ((sql (sms-page-sql :user-leaderboard)))
    (is-true (search "order by total_points desc, users.id" sql))))

(test sms-default-sort-none-sortable-grain-id-only
  "Nil sort with no sortable measure: grain id ASC only, and no
measure alias in the ORDER BY (the SELECT still emits the measure
expression; only the ORDER BY clause is checked)."
  (let ((sql (sms-page-sql :bare-board)))
    (is-true (search "order by users.id" sql))
    (is-false (search "order by total_points" sql))))

(test sms-avg-nulls-last
  "Sort by an :avg measure appends NULLS LAST (requested and
default); zero-fact rows trail. :sum / :count do not get it."
  (let ((sql (sms-page-sql :titles-board :sort '(:avg-points :asc))))
    (is-true (search "order by avg_points asc nulls last, users.id" sql)))
  ;; default on titles-board: first sortable measure is :avg-points
  (let ((sql (sms-page-sql :titles-board)))
    (is-true (search "order by avg_points desc nulls last, users.id" sql)))
  ;; :sum never gets NULLS LAST
  (let ((sql (sms-page-sql :user-leaderboard :sort '(:total-points :asc))))
    (is-false (search "nulls last" sql))))

;;; --- Backend: sort values, tiebreaker, paging ---

(test sms-sort-values-desc
  "Requested sort orders rows by the measure values: alice 7, then
bob/carol tied at 6 (tiebreaker order), dave 0."
  (let* ((r (be-list :user-leaderboard "admin"
               :sort '(:total-points :desc)))
         (names (remove-if-not
                  (lambda (n) (member n '("alice" "bob" "carol" "dave")
                                    :test #'equal))
                  (sms-names r))))
    (is (equal "alice" (first names)))
    (is (equal "dave" (fourth names)))
    (is (equal '(:field :total-points :dir :desc) (getf r :sort)))))

(test sms-tiebreaker-stable-across-requests
  "bob and carol tie on total-points; the grain-id tiebreaker keeps
their relative order identical across repeated requests and pages."
  (let* ((names-1 (sms-names (be-list :user-leaderboard "admin"
                              :sort '(:total-points :desc))))
         (names-2 (sms-names (be-list :user-leaderboard "admin"
                              :sort '(:total-points :desc)))))
    (is (equal names-1 names-2))
    (let ((bob-pos (position "bob" names-1 :test #'equal))
          (carol-pos (position "carol" names-1 :test #'equal)))
      (is-true (and bob-pos carol-pos))
      ;; both present, adjacent, same order every time
      (is (= 1 (abs (- bob-pos carol-pos)))))))

(test sms-pagination-on-grouped-rows
  "LIMIT / OFFSET page the grouped rows; :total is the grouped row
count (all 6 users), not the page size or the raw join row count."
  (let* ((page-1 (be-list :user-leaderboard "admin" :limit 2 :offset 0
                   :sort '(:total-points :desc)))
         (page-2 (be-list :user-leaderboard "admin" :limit 2 :offset 2
                   :sort '(:total-points :desc)))
         (n1 (sms-names page-1))
         (n2 (sms-names page-2)))
    (is (= 2 (length n1)))
    (is (= 2 (length n2)))
    (is-false (intersection n1 n2 :test #'equal))
    (is (= 6 (getf page-1 :total)))
    (is (= 6 (getf page-2 :total)))
    ;; page slice respects the sort: alice first, dave last
    (is (equal "alice" (first n1)))
    (is (equal "dave"
             (first (last
                      (remove-if-not
                        (lambda (n)
                          (member n '("alice" "bob" "carol" "dave")
                            :test #'equal))
                        (sms-names
                          (be-list :user-leaderboard "admin" :limit 20
                            :sort '(:total-points :desc))))))))))

(test sms-avg-zero-fact-rows-trail
  "DESC avg sort with NULLS LAST: dave (no facts, NULL avg) sorts
after alice (the only measured user on titles-board's avg)."
  (let ((names (remove-if-not
                 (lambda (n) (member n '("alice" "dave") :test #'equal))
                 (sms-names (be-list :titles-board "admin"
                              :sort '(:avg-points :desc))))))
    (is (equal '("alice" "dave") names))))

(test sms-sort-rejects-unknown-and-unsortable
  "Unknown field key and unsortable field report-ve on the measure
branch (no silent ignore)."
  (signals validation-error
    (be-list :user-leaderboard "admin" :sort '(:nope :asc)))
  (signals validation-error
    (be-list :titles-board "admin" :sort '(:titles :asc))))

;;; --- Backend: :sort echo ---

(test sms-echo-requested-sort
  "Response :sort echoes the user-facing request as the pinned wire
plist (:field ... :dir ...), never the tiebreaker."
  (let ((r (be-list :user-leaderboard "admin"
               :sort '(:total-points :desc))))
    (is (equal '(:field :total-points :dir :desc) (getf r :sort)))))

(test sms-echo-default-sort
  "Nil request sort with a sortable measure: echo the default
first-sortable-measure (:total-points, desc)."
  (let ((r (be-list :user-leaderboard "admin")))
    (is (equal '(:field :total-points :dir :desc) (getf r :sort)))))

(test sms-echo-no-default-null
  "Nil request sort, no sortable measure: :sort is :null (JSON null,
never [])."
  (let ((r (be-list :bare-board "admin")))
    (is (eq :null (getf r :sort)))))

(test sms-echo-base-path
  "Base path echo: request sort echoes; nil sort echoes :null."
  (let ((r (be-list :user-leaderboard "admin" :sort '(:name :asc))))
    (is (equal '(:field :name :dir :asc) (getf r :sort))))
  (let ((r (be-list :bare-board "admin")))
    (is (eq :null (getf r :sort)))))

(test sms-echo-wire-shape
  "The echo serializes as {\"field\": ..., \"dir\": ...} or JSON
null — not a raw plist object, not []. The keyword :dir proves
byte-identical JSON to the old string form."
  (let ((ok (plist-to-json
              (list :sort (json-sort-echo '(:total-points :desc))))))
    (is (equal "{\"sort\":{\"field\":\"total-points\",\"dir\":\"desc\"}}"
            ok)))
  (let ((ok (plist-to-json (list :sort (json-sort-echo nil)))))
    (is (equal "{\"sort\":null}" ok))))

