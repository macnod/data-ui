(in-package :data-ui)

(def-suite filtered-rollup-suite
  :description "Plan 10: behavioral rollup tests on the
rollup-test fixture (generalization gate)")

(in-suite filtered-rollup-suite)

;;; --- Shared helpers ---

(defun rt-three (result)
  "The board rows for the three test users only (admin and any
transient users like dave stay out of board-wide expectations)."
  (remove-if-not
    (lambda (r) (member (getf r :name) '("alice" "bob" "carol")
                   :test #'equal))
    (getf result :records)))

(defun rt-rec (result name)
  "The board row whose :name is NAME."
  (find name (getf result :records)
    :key (lambda (r) (getf r :name)) :test #'equal))

(defun rt-names (result)
  "The :name values of RESULT's records, in order."
  (loop for r in (getf result :records) collect (getf r :name)))

(defun rt-three-names (result)
  "The :name values of the three test users' rows, in order."
  (mapcar (lambda (r) (getf r :name)) (rt-three result)))

(defun rt-live-user-count ()
  "The actual number of users. Board-independent: do not read
:total from a rollup, or a grain-broken board would make both
boards shrink together and the assert would lie."
  (getf (be-list :users "admin") :total))

(defun rt-days-ago-string (days)
  "A timestamp string (the format timestamp-p accepts) for now
minus DAYS days, built from universal time: no clock, no
timezone translation."
  (multiple-value-bind (s m h d mo y)
    (decode-universal-time
      (- (get-universal-time) (* days 24 60 60)) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
      y mo d h m s)))

(defun rt-seed-fixture ()
  "Seed the rollup-test fixture (plan 10 Test 3 / 12 / 13 board):

- alice: 3 completed tasks (10, 20, 30 pts; recent completed-at),
  1 incomplete (5 pts), plus an old completed 40-pointer for the
  7-day window, and 3 notes (2 on a completed task, 1 on the
  incomplete one)
- bob: 2 completed tasks (15, 25 pts; recent), 1 note on a
  completed task
- carol: no tasks at all (zero-fact row)

Timestamps are strings computed at seed time: recent = now minus
3 days, old = now minus 10 days. Both are more than a day clear
of the 7-day boundary, so clock skew cannot flip membership."
  (dolist (u '("alice" "bob" "carol"))
    (th-make-user u :roles '("task-user" "rollup-viewers")))
  (let ((recent (rt-days-ago-string 3))
        (old (rt-days-ago-string 10)))
    ;; alice: three recent completed (10 20 30), one incomplete (5).
    (be-insert :tasks
      (list :name "a1" :assignee "alice" :points 10
        :completed :true :completed-at recent)
      "admin")
    (be-insert :tasks
      (list :name "a2" :assignee "alice" :points 20
        :completed :true :completed-at recent)
      "admin")
    (be-insert :tasks
      (list :name "a3" :assignee "alice" :points 30
        :completed :true :completed-at recent)
      "admin")
    (be-insert :tasks
      (list :name "a-open" :assignee "alice" :points 5
        :completed :false)
      "admin")
    ;; Test 12: alice's old completed 40-pointer (10 days back).
    (be-insert :tasks
      (list :name "a-old" :assignee "alice" :points 40
        :completed :true :completed-at old)
      "admin")
    ;; bob: two recent completed (15 25).
    (be-insert :tasks
      (list :name "b1" :assignee "bob" :points 15
        :completed :true :completed-at recent)
      "admin")
    (be-insert :tasks
      (list :name "b2" :assignee "bob" :points 25
        :completed :true :completed-at recent)
      "admin")
    ;; Test 13: notes. alice: two on a completed task, one on the
    ;; incomplete task; bob: one on a completed task; carol: none.
    (be-insert :task-notes (list :body "n1" :task "a1") "admin")
    (be-insert :task-notes (list :body "n2" :task "a1") "admin")
    (be-insert :task-notes (list :body "n3" :task "a-open") "admin")
    (be-insert :task-notes (list :body "n4" :task "b1") "admin")
    ;; 10b board (does not change task-summary numbers)
    (be-insert :awards
      '(:name "shared-10" :points 10 :completed :true
        :winners ("alice" "bob"))
      "admin")
    (be-insert :awards
      '(:name "open-20" :points 20 :completed :false
        :winners ("alice"))
      "admin")
    (be-insert :awards
      '(:name "orphan-15" :points 15 :completed :true)
      "admin"))
  nil)

(defun rt-ids (result)
  (loop for r in (getf result :records) collect (getf r :id)))

(defun rt-page-sql (rollup-key &key sort (limit 20) (offset 0))
  "The page SQL be-list logs for one ROLLUP-KEY request (reuses the
09 suite's capture helper; it is defined in sort-measures-tests)."
  (first (sms-capture-page-sql
           (lambda ()
             (be-list rollup-key "admin" :sort sort
               :limit limit :offset offset)))))

(defun rt-ru-summary (&rest overrides)
  "task-summary as a plist with OVERRIDES merged (ru-with keeps
positions, no duplicate keys; the builders live in rollup-tests).
Validates green against ru-base-model: its :tasks carries :points
(mirroring the fixture), so the finishes-baseline in Test 15 is
honest."
  (ru-with
    (list
      :rollup t
      :grain :users
      :type-roles '("rollup-viewers")
      :filter '((:tasks :completed :eq t))
      :views (list :main (list :tables '(:users :tasks)))
      :list-form (list :fields t)
      :fields (list
                (list :name
                  (list :source (list :view :main :table :users
                                  :column :name :agg :first)
                    :sortable t
                    :ui (list :label "User")))
                (list :total-points
                  (list :type :integer
                    :source (list :view :main :table :tasks
                              :column :points :agg :sum)
                    :sortable t
                    :ui (list :label "Points")))
                (list :tasks-done
                  (list :type :integer
                    :source (list :view :main :table :tasks
                              :column :id :agg :count)
                    :sortable t
                    :ui (list :label "Done")))
                (list :avg-points
                  (list :type :real
                    :source (list :view :main :table :tasks
                              :column :points :agg :avg)
                    :sortable t
                    :ui (list :label "Avg")))))
    overrides))

(defun rt-ru-notes (&rest overrides)
  "notes-summary shape with OVERRIDES merged."
  (ru-with
    (list
      :rollup t
      :grain :users
      :type-roles '("rollup-viewers")
      :filter '((:tasks :completed :eq t))
      :views (list :main (list :tables '(:users :tasks :task-notes)))
      :list-form (list :fields t)
      :fields (list
                (list :name
                  (list :source (list :view :main :table :users
                                  :column :name :agg :first)
                    :sortable t
                    :ui (list :label "User")))
                (list :notes-on-done
                  (list :type :integer
                    :source (list :view :main :table :task-notes
                              :column :id :agg :count)
                    :sortable t
                    :ui (list :label "Notes on Done")))))
    overrides))

(defun rt-validate (rollup-spec)
  "Compile a model whose :types is the 08a base types plus
ROLLUP-SPEC as :probe (validate-model = stage-1; stage-1/2/3
take the bare :types plist, not the wrapped model)."
  (validate-model
    (append (ru-base-model)
      (list :probe rollup-spec)))
  t)

;;; --- Test 1: compiles ---

(test rt-1-rollup-compiles
  ":rollup t compiles with :phase-a-shape :measure and the
measure Phase A SQL parts."
  (let* ((td (u:tree-get *compiled-model* :task-summary))
         (view (u:tree-get td :views :main)))
    (is (eq (getf td :phase-a-shape) :measure))
    (dolist (part '(:measure-phase-a-select :measure-phase-a-group-by
                    :measure-phase-a-count-select))
      (is-true (stringp (getf view part))))))

;;; --- Test 2: no physical table ---

(test rt-2-no-physical-table
  "No CREATE TABLE for :task-summary; be-insert / be-update /
be-delete reject."
  (is-false (getf (u:tree-get *compiled-model* :task-summary)
              :create-table-sql))
  (signals validation-error
    (be-insert :task-summary '(:name "x") "admin"))
  (signals validation-error
    (be-update :task-summary
      '((:task-summary :name :eq "alice")) '(:name "x") "admin"))
  (signals validation-error
    (be-delete :task-summary
      '((:task-summary :name :eq "alice")) "admin")))

;;; --- Test 3: basic measures + zero-fact ---

(test rt-3-measure-values
  "Hand-computed board over the three test users. Note: the board
includes alice's old completed 40-pointer (the Test 12 window
fixture), so alice is 100 / 4 / avg 25. carol's zero-fact row
survives (Issue 8); unfiltered :total is the live user count."
  (let ((r (be-list :task-summary "admin")))
    (is (= (rt-live-user-count) (getf r :total)))
    (let ((alice (rt-rec r "alice"))
          (bob (rt-rec r "bob"))
          (carol (rt-rec r "carol")))
      (is (= 100 (getf alice :total-points)))
      (is (= 4 (getf alice :tasks-done)))
      (is (= 40 (getf bob :total-points)))
      (is (= 2 (getf bob :tasks-done)))
      (is (= 0 (getf carol :total-points)))
      (is (= 0 (getf carol :tasks-done)))
      (is (= 3 (length (rt-three r)))))))

;;; --- Test 4: FILTER excludes incomplete tasks ---

(test rt-4-filter-excludes-incomplete
  "alice's incomplete 5-pointer never lands in :total-points
(FILTER, not WHERE): 100, not 105."
  (is (= 100 (getf (rt-rec (be-list :task-summary "admin") "alice")
               :total-points))))

;;; --- Test 5: sort by measure ---

(test rt-5-sort-by-measure
  "Sort :total-points desc / asc over the three users with the
grain-id tiebreaker in the SQL; nil sort defaults to
:total-points desc and echoes it."
  (let ((r (be-list :task-summary "admin" :sort '(:total-points :desc))))
    (is (equal '("alice" "bob" "carol") (rt-three-names r))))
  (let ((r (be-list :task-summary "admin" :sort '(:total-points :asc))))
    (is (equal '("carol" "bob" "alice") (rt-three-names r))))
  (is-true (search ", users.id"
             (rt-page-sql :task-summary :sort '(:total-points :asc))))
  ;; nil sort: first sortable measure is :total-points
  (let ((r (be-list :task-summary "admin")))
    (is (equal '("alice" "bob" "carol") (rt-three-names r)))
    (is (equal '(:field :total-points :dir :desc) (getf r :sort)))))

;;; --- Test 6: pagination on measures ---

(test rt-6-pagination
  "LIMIT / OFFSET page the grouped rows; pages are disjoint and,
walking offset 0 by one, cover every user (relative checks only:
admin / guest tie carol at zero, so the grain-id tiebreaker
decides among them; a fixed 2+2 carve cannot cover 5 rows)."
  (let ((page-1 (be-list :task-summary "admin" :limit 2 :offset 0
                   :sort '(:total-points :desc)))
        (page-2 (be-list :task-summary "admin" :limit 2 :offset 1
                   :sort '(:total-points :desc))))
    ;; alice(100) then bob(40); page 2 slides the window by one.
    (is (equal '("alice" "bob") (rt-names page-1)))
    (is (equal "bob" (first (rt-names page-2)))))
  (let* ((total (rt-live-user-count))
         (sliding (loop for off from 0 below total
                    append (rt-ids
                             (be-list :task-summary "admin" :limit 2
                               :offset off
                               :sort '(:total-points :desc)))))
         (ids-all (rt-ids
                    (be-list :task-summary "admin" :limit 200
                      :sort '(:total-points :desc)))))
    (is (= total (getf (be-list :task-summary "admin") :total)))
    (is (= total (length ids-all)))
    ;; every page-id is a board id; sliding union covers the board
    (is (null (set-difference sliding ids-all :test #'equal)))
    (is (null (set-difference ids-all sliding :test #'equal)))
    ;; tiebreaker stability: two identical requests, same order
    (is (equal ids-all
             (rt-ids
               (be-list :task-summary "admin" :limit 200
                 :sort '(:total-points :desc)))))
    ;; the plan's disjoint carve: consecutive non-overlapping pages
    ;; (limit 2, offset 0/2/4/...) tile the board exactly, in the
    ;; full-list order. Relative to the live count, not a fixed
    ;; 2+2: admin/guest/dave tie at zero and the grain-id
    ;; tiebreaker decides among them.
    (let ((pages (loop for off from 0 below total by 2
                   append (rt-ids
                            (be-list :task-summary "admin" :limit 2
                              :offset off
                              :sort '(:total-points :desc))))))
      (is (equal ids-all pages)))))

;;; --- Test 7: sort by grain pass-through ---

(test rt-7-sort-by-passthrough
  "Sort by :name asc: alphabetical by name."
  (let ((r (be-list :task-summary "admin" :sort '(:name :asc))))
    (is (equal '("alice" "bob" "carol") (rt-three-names r)))))

;;; --- Test 8: RBAC on rollup ---

(test rt-8-rbac-role-gate
  "dave (task-user only) cannot see :task-summary (be-types omits,
be-list refuses); alice (rollup-viewers) sees every grain row.
Grain read on :users is neither sufficient nor required."
  (th-make-user "dave" :roles '("task-user"))
  (is-false (member :task-summary
              (mapcar (lambda (e) (getf e :name)) (be-types "dave"))
              :test #'eq))
  (signals validation-error
    (be-list :task-summary "dave"))
  (let ((r (be-list :task-summary "alice")))
    (is (= (rt-live-user-count) (getf r :total)))
    ;; dave's row shows up on the board while he exists
    (is-true (rt-rec r "dave"))))

;;; --- Test 9: CRUD flags false ---

(test rt-9-crud-flags-false
  "list-result :create / :update / :delete are :false on the
rollup."
  (let ((r (be-list :task-summary "admin")))
    (is (eq :false (getf r :create)))
    (is (eq :false (getf r :update)))
    (is (eq :false (getf r :delete)))))

;;; --- Test 11: :avg ---

(test rt-11-avg
  ":avg matches hand-computed means as Lisp floats; carol's avg is
:null (JSON null) yet her row stays (Issues 8 + 13); sorting by
:avg-points appends NULLS LAST (carol trails). alice's mean
includes the old 40-pointer: (10+20+30+40)/4 = 25."
  (let ((r (be-list :task-summary "admin")))
    (let ((alice (rt-rec r "alice"))
          (bob (rt-rec r "bob"))
          (carol (rt-rec r "carol")))
      (is (= 25.0 (getf alice :avg-points)))
      (is (floatp (getf alice :avg-points)))
      (is (= 20.0 (getf bob :avg-points)))
      (is (eq :null (getf carol :avg-points)))))
  (is-true (search "nulls last"
             (rt-page-sql :task-summary :sort '(:avg-points :desc))))
  (let* ((r (be-list :task-summary "admin"
              :sort '(:avg-points :desc)))
         (three (rt-three-names r)))
    (is (equal "carol" (third three)))
    (is (equal '("alice" "bob") (subseq three 0 2)))))

;;; --- Test 12: time-window (:last-days) ---

(test rt-12-time-window
  ":task-summary-recent drops the 10-day-old 40-pointer: alice's
:recent-points is 60 (not 100), the incomplete 5 never counted;
nil sort defaults to :recent-points desc (skipping the
unsortable :avg declared first)."
  (let* ((r (be-list :task-summary-recent "admin"))
         (alice (rt-rec r "alice"))
         (bob (rt-rec r "bob"))
         (carol (rt-rec r "carol")))
    (is (= 60 (getf alice :recent-points)))
    (is (= 40 (getf bob :recent-points)))
    (is (= 0 (getf carol :recent-points)))
    (is (eq :null (getf carol :avg-points))))
  (is (equal '(:field :recent-points :dir :desc)
    (getf (be-list :task-summary-recent "admin") :sort))))

;;; --- Test 13: 3-table chain join ---

(test rt-13-chain-join
  "users -> tasks -> task-notes with COUNT on the leaf: alice 2,
bob 1, carol 0 (zero-fact row stays)."
  (let ((r (be-list :notes-summary "admin")))
    (is (= 2 (getf (rt-rec r "alice") :notes-on-done)))
    (is (= 1 (getf (rt-rec r "bob") :notes-on-done)))
    (is (= 0 (getf (rt-rec r "carol") :notes-on-done)))))

;;; --- Test 13b: intermediate-path filter (Issue 17) ---

(test rt-13b-intermediate-path-filter
  "Test 13's numbers are the behavior (alice 2, not 3). Here: the
SQL keeps the predicate in FILTER (never a WHERE on tasks before
GROUP BY); zero-fact carol stays; :total is unchanged by the fact
filter; an off-path :filter table is a compile error."
  (let* ((v (u:tree-get *compiled-model* :notes-summary :views :main))
         (select (getf v :measure-phase-a-select)))
    (is-true (search "filter (where" select :test #'char-equal))
    (is (search
          "count(rt_task_notes.id) filter (where rt_task_notes.id is not null and rt_tasks.task_completed)"
          select :test #'char-equal))
    (is-false (search "where rt_tasks" select :test #'char-equal)))
  (let ((r (be-list :notes-summary "admin")))
    (is (= (rt-live-user-count) (getf r :total)))
    (is-true (rt-rec r "carol")))
  (signals error
    (rt-validate
      (rt-ru-notes :filter '((:task-summary :completed :eq t))))))

;;; --- Test 14: endpoints (Issue 9) ---

(test rt-14-endpoints
  "be-rec / be-id / be-val / be-validate-field / be-insert
report-ve on the rollup; be-list-column on :name returns one
value per grain row; non-empty :search report-ve's."
  (let ((id (getf (rt-rec (be-list :task-summary "admin") "alice")
               :id)))
    (signals validation-error
      (be-rec id "admin" :type-key :task-summary))
    (signals validation-error
      (be-id :task-summary
        '((:task-summary :name :eq "alice")) "admin"))
    (signals validation-error
      (be-val id :name "admin" :type-key :task-summary))
    (signals validation-error
      (be-validate-field :task-summary :name "x" "admin"))
    (signals validation-error
      (be-insert :task-summary '(:name "x") "admin")))
  (let* ((r (be-list-column :task-summary :name "admin" :limit 200))
         (values (sort (copy-list (getf r :values)) #'string<)))
    (is (= (rt-live-user-count) (length values)))
    (is-true (member "carol" values :test #'equal)))
  (signals validation-error
    (be-list :task-summary "admin" :search "alice")))

;;; --- Test 15: authoring compile errors ---

(test rt-15-authoring-errors
  "Missing / nil / empty :list-form, add/update forms, author :id,
grain-only, :count without :integer, :avg not :real, pass-through
:type mismatch, :button, lifecycle slot, :write-to, extra :views,
mixed depth, extra arm, hop past F, grain-as-fact, :agg :first
off grain. All report-e via validate-model on inline models."
  ;; green-path baseline: both builders are valid specs against
  ;; ru-base-model (whose :tasks now carries :points, mirroring
  ;; the fixture). Without this, the signals-error cases below
  ;; can pass by firing on a rotten builder instead of the
  ;; intended check (review: Test 15's builders once measured
  ;; :tasks :points, a column ru-base-model's :tasks lacked).
  (finishes (rt-validate (rt-ru-summary)))
  (finishes (rt-validate (rt-ru-notes)))
  ;; missing / nil / empty :list-form
  (signals error (rt-validate (ru-sans (rt-ru-summary) :list-form)))
  (signals error (rt-validate (rt-ru-summary :list-form nil)))
  (signals error
    (rt-validate (rt-ru-summary :list-form (list :fields nil))))
  ;; add / update forms present (even nil)
  (signals error
    (rt-validate (rt-ru-summary :add-form (list :fields t))))
  (signals error (rt-validate (rt-ru-summary :add-form nil)))
  (signals error
    (rt-validate (rt-ru-summary :update-form (list :fields t))))
  (signals error (rt-validate (rt-ru-summary :update-form nil)))
  ;; author-declared :id
  (signals error
    (rt-validate
      (rt-ru-summary
        :fields (list
                  (list :id
                    (list :type :uuid
                      :source (list :view :main :table :users
                                  :column :id :agg :first)))
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :total-points
                    (list :type :integer
                      :source (list :view :main :table :tasks
                                :column :points :agg :sum)))))))
  ;; grain-only: every author field :agg :first
  (signals error
    (rt-validate
      (rt-ru-summary
        :filter nil
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))))))
  ;; :count without :type :integer (text default fails the check)
  (signals error
    (rt-validate
      (rt-ru-summary
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :tasks-done
                    (list
                      :source (list :view :main :table :tasks
                                :column :id :agg :count)))
                  (list :total-points
                    (list :type :integer
                      :source (list :view :main :table :tasks
                                :column :points :agg :sum)))))))
  ;; :avg with :type :integer (must be :real)
  (signals error
    (rt-validate
      (rt-ru-summary
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :avg-points
                    (list :type :integer
                      :source (list :view :main :table :tasks
                                :column :points :agg :avg)))))))
  ;; pass-through :type mismatch (users.name is text, not integer)
  (signals error
    (rt-validate
      (rt-ru-summary
        :fields (list
                  (list :name
                    (list :type :integer
                      :source (list :view :main :table :users
                                  :column :name :agg :first)))
                  (list :total-points
                    (list :type :integer
                      :source (list :view :main :table :tasks
                                :column :points :agg :sum)))))))
  ;; :button field
  (signals error
    (rt-validate
      (rt-ru-summary
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :go
                    (list :type :button
                      :action '(:noop)
                      :ui (list :widget :button)))
                  (list :total-points
                    (list :type :integer
                      :source (list :view :main :table :tasks
                                :column :points :agg :sum)))))))
  ;; lifecycle slot
  (signals error
    (rt-validate (rt-ru-summary :pre-create :some-hook)))
  ;; :write-to
  (signals error
    (rt-validate
      (rt-ru-summary
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :wrote
                    (list :type :text
                      :write-to (list :table :tasks :title :value)
                      :source (list :view :main :table :tasks
                                :column :title :agg :first)))
                  (list :total-points
                    (list :type :integer
                      :source (list :view :main :table :tasks
                                :column :points :agg :sum)))))))
  ;; extra :views beyond :main
  (signals error
    (rt-validate
      (rt-ru-summary
        :views (list :main (list :tables '(:users :tasks))
                 :extra (list :tables '(:users))))))
  ;; mixed depth (Issue 16): measures on :tasks and :task-notes
  (signals error
    (rt-validate
      (rt-ru-notes
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :tasks-done
                    (list :type :integer
                      :source (list :view :main :table :tasks
                                :column :id :agg :count)))
                  (list :notes-on-done
                    (list :type :integer
                      :source (list :view :main :table :task-notes
                                :column :id :agg :count)))))))
  ;; extra arm: :task-notes in :tables, measure only on :tasks
  (signals error
    (rt-validate
      (rt-ru-summary
        :views (list :main
                 (list :tables '(:users :tasks :task-notes))))))
  ;; hop past F: 3-table chain, the only measure on the middle
  (signals error
    (rt-validate
      (rt-ru-notes
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :tasks-done
                    (list :type :integer
                      :source (list :view :main :table :tasks
                                :column :id :agg :count)))))))
  ;; grain-as-fact: a measure whose :source :table is the grain
  (signals error
    (rt-validate
      (rt-ru-summary
        :filter nil
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :user-count
                    (list :type :integer
                      :source (list :view :main :table :users
                                :column :id :agg :count)))))))
  ;; :agg :first on a non-grain table
  (signals error
    (rt-validate
      (rt-ru-summary
        :filter nil
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :task-name
                    (list :source (list :view :main :table :tasks
                                    :column :title :agg :first)))
                  (list :total-points
                    (list :type :integer
                      :source (list :view :main :table :tasks
                                :column :points :agg :sum))))))))

;;; --- Test 16: request-time grain filters (Issue 14) ---

(test rt-16-request-grain-filters
  "Listed-type pass-through shrinks rows and :total; fact-table
tuple, measure field, grain-type tuple, unexposed grain column
(:email) all report-ve; bare UUID of a zero-fact row returns that
one row; unfiltered still includes carol."
  (let ((r (be-list :task-summary "admin"
              :filters '((:task-summary :name :eq "alice")))))
    (is (= 1 (getf r :total)))
    (is (equal '("alice") (rt-names r))))
  (signals validation-error
    (be-list :task-summary "admin"
      :filters '((:tasks :name :like "%x%"))))
  (signals validation-error
    (be-list :task-summary "admin"
      :filters '((:task-summary :total-points :gte 40))))
  (signals validation-error
    (be-list :task-summary "admin"
      :filters '((:users :name :eq "alice"))))
  (signals validation-error
    (be-list :task-summary "admin"
      :filters '((:task-summary :email :eq "a@b.c"))))
  (let* ((carol-id
           (getf (rt-rec (be-list :task-summary "admin") "carol")
             :id))
         (r (be-list :task-summary "admin" :filters carol-id)))
    (is (= 1 (getf r :total)))
    (is (= 0 (getf (rt-rec r "carol") :total-points))))
  (is-true (rt-rec (be-list :task-summary "admin") "carol")))

;;; --- Tests 17-21 (10b): M2M-joiner hop ---

(defun rt-ru-award-base ()
  "ru-base-model plus the 10b award types (awards, award-users),
for pure validate-model probes: the joiner gives :awards a real
xref edge to :users."
  (append (ru-base-model)
    (list
      :awards
      (list :table t :create :auto :update :auto :delete :auto
        :type-roles '("task-user")
        :views (list :main (list :tables '(:awards :award-users :users))
                 :users (list :tables '(:users)))
        :fields (list
                  :name (list :type :text :identity t
                          :ui (list :label "Award" :widget :textbox)
                          :source (list :view :main :column :name
                                    :agg :first)
                          :column t :not-null t)
                  :points (list :type :integer
                            :ui (list :label "Points" :widget :textbox)
                            :source (list :view :main :column :points
                                      :agg :first)
                            :column t :not-null t)
                  :completed (list :type :boolean :default :false
                               :ui (list :label "Done" :widget :checkbox)
                               :source (list :view :main :column
                                         :completed :agg :first)
                               :column t :not-null t)
                  :winners (list :type :list
                             :ui (list :label "Winners"
                                   :widget :checkbox-list)
                             :source (list :view :main :table :users
                                       :column :name :agg :distinct)
                             :source-all (list :view :users :table :users
                                           :column :name :agg :list)
                             :join-table :award-users))
        :list-form (list :fields t)
        :update-form (list :fields t)
        :add-form (list :fields t))
      :award-users
      (list :table t :is-joiner t :internal t
        :fields (list :reference (list :target :awards)
                  :reference (list :target :users))))))

(defun rt-ru-award (&rest overrides)
  "award-board shape with OVERRIDES merged (ru-with keeps
positions, no duplicate keys)."
  (ru-with
    (list
      :rollup t
      :grain :users
      :type-roles '("rollup-viewers")
      :filter '((:awards :completed :eq t))
      :views (list :main (list :tables '(:users :award-users :awards)))
      :list-form (list :fields t)
      :fields (list
                (list :name
                  (list :source (list :view :main :table :users
                                  :column :name :agg :first)
                    :sortable t
                    :ui (list :label "User")))
                (list :total-points
                  (list :type :integer
                    :source (list :view :main :table :awards
                              :column :points :agg :sum)
                    :sortable t
                    :ui (list :label "Points")))
                (list :awards-done
                  (list :type :integer
                    :source (list :view :main :table :awards
                              :column :id :agg :count)
                    :sortable t
                    :ui (list :label "Done")))))
    overrides))

(defun rt-validate-award (rollup-spec)
  "Compile the award base types plus ROLLUP-SPEC as :probe
(validate-model = stage-1; pure: no DB, no set-model)."
  (validate-model
    (append (rt-ru-award-base)
      (list :probe rollup-spec)))
  t)

(test rt-17-joiner-hop-compiles
  ":award-board compiles with :phase-a-shape :measure and the hop
path (:users :award-users :awards): the joiner is the middle
hop, its SQL joins users -> joiner -> awards."
  (let* ((td (u:tree-get *compiled-model* :award-board))
         (view (u:tree-get td :views :main))
         (select (getf view :measure-phase-a-select)))
    (is (eq (getf td :phase-a-shape) :measure))
    (is-true (stringp select))
    (is-true (search "left join rt_award_users"
              select :test #'char-equal))
    (is-true (search "rt_award_users.user_id = users.id"
              select :test #'char-equal))
    (is-true (search "rt_awards.id = rt_award_users.award_id"
              select :test #'char-equal))))

(test rt-18-joiner-hop-measures
  "Multi-completer fan-out: alice and bob each take the full
10 / 1 from the shared award. Zero-fact carol stays with integer
0 / 0. The unattributed completed orphan counts for nobody.
Unfiltered :total is the live user count: the fact filter never
shrinks the board."
  (let ((r (be-list :award-board "admin")))
    (is (= (rt-live-user-count) (getf r :total)))
    (let ((alice (rt-rec r "alice"))
          (bob (rt-rec r "bob"))
          (carol (rt-rec r "carol")))
      (is (= 10 (getf alice :total-points)))
      (is (= 1 (getf alice :awards-done)))
      (is (= 10 (getf bob :total-points)))
      (is (= 1 (getf bob :awards-done)))
      (is-true carol)
      (is (= 0 (getf carol :total-points)))
      (is (integerp (getf carol :awards-done)))
      (is (= 0 (getf carol :awards-done))))))

(test rt-19-joiner-hop-incomplete-excluded
  "alice's total is 10, not 30: the incomplete 20-pointer never
contributes (FILTER, not WHERE, even across the joiner hop)."
  (is (= 10 (getf (rt-rec (be-list :award-board "admin") "alice")
               :total-points))))

(test rt-20-joiner-hop-sort
  "Sort by :total-points desc: both 10s (alice and bob; the
grain-id tiebreaker decides between them, so no order assert
there) land before carol's 0."
  (let ((three (rt-three-names
                 (be-list :award-board "admin"
                   :sort '(:total-points :desc)))))
    (is (= 3 (length three)))
    (is (equal "carol" (third three)))
    (is (equal '("alice" "bob")
           (sort (subseq three 0 2) #'string<)))))

(test rt-21-two-table-skip
  "The two-table (:users :awards) shape report-e's: no xref path
from grain to fact without the joiner named in :tables. Green
baseline first so the signals case cannot fire on a rotten
builder."
  (finishes (rt-validate-award (rt-ru-award)))
  (let ((msg
          (handler-case
              (progn
                (rt-validate-award
                  (rt-ru-award
                    :views (list :main
                             (list :tables '(:users :awards)))))
                nil)
            (error (e) (princ-to-string e)))))
    (is-true (and msg (search "No xref path" msg :test #'char-equal)))
    (is-true (and msg
              (search "name the intermediate hops"
                msg :test #'char-equal)))))

;;; --- Test 10: all-grain-zero (own unseeded context, last) ---

(test rt-10-all-grain-zero
  "In a fresh unseeded model context, every grain row survives
with 0 / 0 / NULL and :total is the live user count (zero-fact
retention, Issue 8). Runs last: its inner with-model resets the
database, so any test after it would see an empty board."
  (with-model "rollup-test" nil
    (let ((r (be-list :task-summary "admin" :limit 50)))
      (is (= (rt-live-user-count) (getf r :total)))
      (is (= (getf r :total) (length (getf r :records))))
      (loop for row in (getf r :records)
        do (progn
             (is (= 0 (getf row :total-points)))
             (is (= 0 (getf row :tasks-done)))
             (is (eq :null (getf row :avg-points))))))))
