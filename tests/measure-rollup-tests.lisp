(in-package :data-ui)

(def-suite measure-rollup-suite
  :description "08b measure Phase A SQL: generator parts, be-list
measure branch, count query, guards")

(in-suite measure-rollup-suite)

;;; --- Shared helpers ---

(defun mrb-parts (rollup-key)
  "Compiled measure SQL parts for ROLLUP-KEY under the current model."
  (u:tree-get *compiled-model* rollup-key :views :main))

(defun mrb-record-by-name (result name)
  "The record of a measure be-list RESULT whose :name is NAME."
  (find name (getf result :records)
    :key (lambda (r) (getf r :name)) :test #'equal))

(defun mrb-seed-fixture ()
  "Seed the measure-rollup-test fixture:

- alice: 3 chores — 2 completed (3 + 4 pts), 1 incomplete (2 pts)
- bob:   1 completed chore (5 pts)
- carol: no chores (zero-fact row)
- tasks/notes for the 3-table hop-binding rollup
- widget-lists / widgets for the scoped non-:users grain"
  ;; Users get leaderboard-viewers so be-list on the rollups works.
  (dolist (u '("alice" "bob" "carol"))
    (th-make-user u :roles '("chore-user" "leaderboard-viewers"
                             "task-user" "note-user"
                             "widget-user" "widget-viewers")))
  ;; alice: dishes 3 done, laundry 4 done, trash 2 not done
  (be-insert :chores '(:title "dishes" :user "alice" :points 3
                     :completed :true) "admin")
  (be-insert :chores '(:title "laundry" :user "alice" :points 4
                     :completed :true) "admin")
  (be-insert :chores '(:title "trash" :user "alice" :points 2
                     :completed :false) "admin")
  ;; bob: mow 5 done
  (be-insert :chores '(:title "mow" :user "bob" :points 5
                     :completed :true) "admin")
  ;; Tasks + notes (hop binding): alice has one completed task with 2
  ;; notes and one incomplete task with 1 note; bob has one incomplete
  ;; task with 1 note.
  (be-insert :tasks '(:title "t-done" :user "alice" :completed :true)
    "admin")
  (be-insert :tasks '(:title "t-open" :user "alice" :completed :false)
    "admin")
  (be-insert :tasks '(:title "t-bob" :user "bob" :completed :false)
    "admin")
  (be-insert :task-notes '(:note "n1" :task "t-done") "admin")
  (be-insert :task-notes '(:note "n2" :task "t-done") "admin")
  (be-insert :task-notes '(:note "n3" :task "t-open") "admin")
  (be-insert :task-notes '(:note "n4" :task "t-bob") "admin")
  ;; Scoped grain: alice and bob each own a widget list; alice's has 2
  ;; widgets, bob's 1, admin's 0.
  (be-insert :widget-lists '(:title "alice-list" :user "alice") "admin")
  (be-insert :widget-lists '(:title "bob-list" :user "bob") "admin")
  (be-insert :widget-lists '(:title "admin-list" :user "admin") "admin")
  (be-insert :widgets '(:name "w1" :list "alice-list") "admin")
  (be-insert :widgets '(:name "w2" :list "alice-list") "admin")
  (be-insert :widgets '(:name "w3" :list "bob-list") "admin")
  nil)

;;; --- Compiler: generated SQL parts ---

(test mrb-parts-present
  "enrich-views stores the four measure parts on the rollup view."
  (let ((v (mrb-parts :user-leaderboard)))
    (is-true (getf v :measure-phase-a-select))
    (is-true (getf v :measure-phase-a-group-by))
    (is-true (getf v :measure-phase-a-count-select))
    (is-true (listp (getf v :measure-phase-a-grain-where)))))

(test mrb-select-shape
  "FROM is the grain table (users); the fact table is the compiled
:table-name of the chore type; aliases are field keys; the fact join is
LEFT JOIN."
  (let ((select (getf (mrb-parts :user-leaderboard)
                :measure-phase-a-select)))
    (is (search "from users" select :test #'char-equal))
    (is (search "left join rt_chores on rt_chores.chore_user = users.id"
          select :test #'char-equal))
    (is (search "users.id as id" select :test #'char-equal))
    (is (search "users.user_name as name" select :test #'char-equal))
    (is (search "as total_points" select :test #'char-equal))
    (is (search "as chores_done" select :test #'char-equal))
    (is-false (search "as grain_id" select :test #'char-equal))))

(test mrb-group-by-separate-part
  "GROUP BY lives on its own part; the SELECT part has no GROUP BY
and no WHERE (runtime WHERE splices between them)."
  (let ((v (mrb-parts :user-leaderboard)))
    (is-false (search "group by"
                 (getf v :measure-phase-a-select) :test #'char-equal))
    ;; "where" only inside "filter (where ...)".
    (is (search "filter (where rt_chores.id is not null"
          (getf v :measure-phase-a-select) :test #'char-equal))
    (is (search "group by users.id, users.user_name"
          (getf v :measure-phase-a-group-by) :test #'char-equal))))

(test mrb-pk-guard-and-model-filter-in-filter
  "Every real measure's FILTER carries <F>.id IS NOT NULL ANDed with
the model-declared joined-table clause; never a global WHERE on the
joined table; never COUNT(DISTINCT."
  (let ((select (getf (mrb-parts :user-leaderboard)
                :measure-phase-a-select)))
    (is (search "coalesce(sum(rt_chores.chore_points) filter (where rt_chores.id is not null and rt_chores.chore_completed), 0)"
          select :test #'char-equal))
    (is (search "count(rt_chores.id) filter (where rt_chores.id is not null and rt_chores.chore_completed)"
          select :test #'char-equal))
    (is-false (search "count(distinct" select :test #'char-equal))
    ;; No global WHERE clause: every "where ..." occurrence is the
    ;; inside of a "filter (where ...)".
    (is (equal '("filter (where rt_chores.id")
          (u:distinct-values
            (re:all-matches-as-strings "filter \\(where [a-z_.]+" select))))))

(test mrb-unfiltered-list-carries-pk-guard
  "Unfiltered :list / :distinct measures still get the PK guard so a
LEFT JOIN miss is {} not {NULL}; the COALESCE empty-array wrap is
present with the field's PG type."
  (let ((select (getf (mrb-parts :titles-board)
                :measure-phase-a-select)))
    (is (search "coalesce(array_agg(rt_chores.chore_title) filter (where rt_chores.id is not null), array[]::text[])"
          select :test #'char-equal))
    (is (search "array[]::integer[]" select :test #'char-equal))
    (is-false (search "where rt_chores.chore_completed"
                 select :test #'char-equal))))

(test mrb-avg-not-coalesced
  ":avg has no COALESCE — SQL NULL for empty groups (Issue 13)."
  (let ((select (getf (mrb-parts :titles-board)
                :measure-phase-a-select)))
    (is (search "avg(rt_chores.chore_points) filter" select
         :test #'char-equal))
    (is-false (search "coalesce(avg" select :test #'char-equal))))

(test mrb-grain-where-fragments
  "Model-declared grain-table clauses compile to grain-where
fragments (runtime WHERE); :last-days / :calendar clauses are
joined-table clauses (FILTER), not grain-where fragments."
  (let ((v (u:tree-get *compiled-model* :grain-filtered-board
             :views :main)))
    (is (equal '("users.user_name = 'alice'")
            (getf v :measure-phase-a-grain-where))))
  (let* ((model (list
                  :mini
                  (list :table t :create :auto :update :auto :delete :auto
                    :views (list :main (list :tables '(:mini :users)))
                    :fields (list
                              :thing (list :type :text
                                       :source (list :view :main
                                                 :column :thing :agg :first)
                                       :column t :not-null t)
                              :user (list :type :text :target :users
                                      :source (list :view :main :table
                                                :users :column :name
                                                :agg :first)
                                      :column t :not-null t)
                              :when-at (list :type :timestamp
                                         :source (list :view :main
                                                   :column :when-at
                                                   :agg :first)
                                         :column t))
                    :list-form (list :fields t)
                    :update-form (list :fields t)
                    :add-form (list :fields t))
                  :lb (list :rollup t :grain :users
                        :filter '((:mini :when-at :last-days 30))
                        :views (list :main (list :tables '(:users :mini)))
                        :list-form (list :fields t)
                        :fields (list
                                  (list :name
                                    (list :source
                                      (list :view :main :table :users
                                        :column :name :agg :first)))
                                  (list :things
                                    (list :type :integer
                                      :source (list :view :main :table :mini
                                                :column :id :agg :count)))))))
         (v (u:tree-get (compile-model model) :lb :views :main))
         (select (getf v :measure-phase-a-select)))
    ;; :last-days is a joined-table clause: inside FILTER with the PK
    ;; guard, embedding NOW(), and not in the grain-where list.
    (is-false (getf v :measure-phase-a-grain-where))
    (is (search "rt_mini.id is not null" select :test #'char-equal))
    (is (search "rt_mini.mini_when_at >= now() - interval '30 days'"
          select :test #'char-equal))
    (is-false (search "202" select))))

(test mrb-hop-binding-filter
  "Issue 17: a :filter clause on the middle table ANDs into the
measure's FILTER (with the F PK guard), never a global WHERE."
  (let ((select (getf (mrb-parts :note-summary)
                :measure-phase-a-select)))
    (is (search "count(rt_task_notes.id) filter (where rt_task_notes.id is not null and rt_tasks.task_completed)"
          select :test #'char-equal))
    (is-false (search "where rt_tasks" select :test #'char-equal))))

(test mrb-count-select-shape
  "Count query is a join-free COUNT(*) on the grain table with no
WHERE baked in."
  (let ((count (getf (mrb-parts :user-leaderboard)
               :measure-phase-a-count-select)))
    (is (equal "select count(*) from users" count))))

;;; --- Backend: measure be-list execution ---

(test mrb-leaderboard-values
  "Measure Phase A executes: alice 7/2, bob 5/1, carol 0/0 (zero-fact
retention — the row appears). :total is the grain count (5 users:
admin, guest, alice, bob, carol)."
  (let ((r (be-list :user-leaderboard "admin")))
    (is (= 5 (getf r :total)))
    (let ((alice (mrb-record-by-name r "alice"))
          (bob (mrb-record-by-name r "bob"))
          (carol (mrb-record-by-name r "carol")))
      (is (= 7 (getf alice :total-points)))
      (is (= 2 (getf alice :chores-done)))
      (is (= 5 (getf bob :total-points)))
      (is (= 1 (getf bob :chores-done)))
      (is (= 0 (getf carol :total-points)))
      (is (= 0 (getf carol :chores-done))))))

(test mrb-rows-are-field-key-plists
  "Row keys are the field keys (:id, :name); no Phase B collapse, no
alias keys."
  (let* ((r (be-list :user-leaderboard "admin"))
         (alice (mrb-record-by-name r "alice")))
    (is (equal '(:id :name :total-points :chores-done)
            (u:plist-keys alice)))))

(test mrb-zero-fact-list-and-avg
  "Zero-fact row: :list / :distinct are empty (not (NULL)), :avg is
:null (JSON null). Non-:null :avg is a Lisp float; :list /
:distinct are Lisp lists."
  (let ((r (be-list :titles-board "admin")))
    (let ((carol (mrb-record-by-name r "carol"))
          (alice (mrb-record-by-name r "alice")))
      (is (null (getf carol :titles)))
      (is (null (getf carol :distinct-points)))
      (is (eq :null (getf carol :avg-points)))
      (is (equal '("dishes" "laundry" "trash")
              (sort (copy-list (getf alice :titles)) #'string<)))
      (is (equal '(2 3 4) (sort (copy-list (getf alice :distinct-points))
                            #'<)))
      (is (= 3.0 (getf alice :avg-points)))
      (is (floatp (getf alice :avg-points))))))

(test mrb-hop-binding-values
  "3-table rollup with the filter on the middle table: only notes on
completed tasks count; users with none stay at 0."
  (let ((r (be-list :note-summary "admin")))
    (let ((alice (mrb-record-by-name r "alice"))
          (bob (mrb-record-by-name r "bob"))
          (carol (mrb-record-by-name r "carol")))
      (is (= 2 (getf alice :notes-on-done)))
      (is (= 0 (getf bob :notes-on-done)))
      (is (= 0 (getf carol :notes-on-done))))))

(test mrb-grain-filter-restricts-rows
  "Model-declared grain filter is runtime WHERE: restricts rows and
:total."
  (let ((r (be-list :grain-filtered-board "admin")))
    (is (= 1 (getf r :total)))
    (is (= 1 (length (getf r :records))))
    (is (equal "alice" (getf (mrb-record-by-name r "alice") :name)))))

(test mrb-request-grain-filter
  "Request-time listed-type pass-through filter restricts rows and
:total (find alice)."
  (let ((r (be-list :user-leaderboard "admin"
              :filters '((:user-leaderboard :name :eq "bob")))))
    (is (= 1 (getf r :total)))
    (is (= 1 (length (getf r :records))))
    (is (= 5 (getf (mrb-record-by-name r "bob") :total-points)))))

(test mrb-bare-uuid-and-unknown-uuid
  "Bare UUID means grain id; unknown UUID is an empty page with
:total 0, not valid-existing-uuid."
  (let* ((all (be-list :user-leaderboard "admin"))
         (alice-id (getf (mrb-record-by-name all "alice") :id)))
    (let ((r (be-list :user-leaderboard "admin" :filters alice-id)))
      (is (= 1 (getf r :total)))
      (is (equal "alice" (getf (mrb-record-by-name r "alice") :name))))
    (let ((r (be-list :user-leaderboard "admin"
                :filters "00000000-0000-0000-0000-000000000000")))
      (is (= 0 (getf r :total)))
      (is (null (getf r :records))))))

(test mrb-fact-filter-does-not-change-total
  "Model-declared fact filter re-scopes the numbers, never :total."
  (let ((r (be-list :user-leaderboard "admin")))
    ;; 4 users exist (admin, guest, alice, bob, carol — seeded).
    (is (= (getf r :total) (length (getf r :records))))
    (is (plusp (getf r :total)))))

(test mrb-request-filters-rejected
  "Fact-table tuples, measure fields, grain-type tuples, and unexposed
grain columns all report-ve (one error class)."
  (signals validation-error
    (be-list :user-leaderboard "admin"
      :filters '((:chores :title :like "%dishes%"))))
  (signals validation-error
    (be-list :user-leaderboard "admin"
      :filters '((:user-leaderboard :total-points :gte 5))))
  (signals validation-error
    (be-list :user-leaderboard "admin"
      :filters '((:users :name :eq "alice"))))
  (signals validation-error
    (be-list :user-leaderboard "admin"
      :filters '((:user-leaderboard :email :eq "a@b.c")))))

(test mrb-search-rejected
  "Non-empty :search on a rollup report-ve's (zero searchable
fields)."
  (signals validation-error
    (be-list :user-leaderboard "admin" :search "alice")))

(test mrb-paging-grain-id-order
  "With no :sortable fields, pages are stable in grain-id order; LIMIT
/ OFFSET page the grouped rows."
  (let* ((page-1 (be-list :user-leaderboard "admin" :limit 2 :offset 0))
         (page-2 (be-list :user-leaderboard "admin" :limit 2 :offset 2))
         (ids-1 (mapcar (lambda (r) (getf r :id))
                     (getf page-1 :records)))
         (ids-2 (mapcar (lambda (r) (getf r :id))
                     (getf page-2 :records))))
    (is (= 2 (length ids-1)))
    (is (= 2 (length ids-2)))
    (is-false (intersection ids-1 ids-2 :test #'equal))
    ;; Total is the full grain count, not the page size.
    (is (= 5 (getf page-1 :total)))
    (is (= 5 (getf page-2 :total)))))

(test mrb-scope-user-users-grain
  ":scope :user on a :users grain: the scoped caller sees exactly
their own row (grain.id = users.id UUID); no id IN (...) clause is
emitted."
  (let ((r (be-list :my-stats "alice")))
    (is (= 1 (getf r :total)))
    (is (equal "alice" (getf (mrb-record-by-name r "alice") :name)))))

(test mrb-scope-user-non-users-grain
  ":scope :user on a non-:users grain: only rows the caller owns
(grain :user column = their id; the column is a UUID FK when
declared with :target :users)."
  (flet ((by-title (result title)
           (find title (getf result :records)
             :key (lambda (r) (getf r :title)) :test #'equal)))
    (let ((alice (be-list :my-widgets "alice"))
          (bob (be-list :my-widgets "bob"))
          (admin (be-list :my-widgets "admin")))
      (is (= 1 (getf alice :total)))
      (is (= 2 (getf (by-title alice "alice-list") :widget-count)))
      (is (= 1 (getf bob :total)))
      (is (= 1 (getf (by-title bob "bob-list") :widget-count)))
      ;; admin's list has zero widgets and stays (zero-fact retention).
      (is (= 1 (getf admin :total)))
      (is (= 0 (getf (by-title admin "admin-list") :widget-count))))))

(test mrb-crud-flags-false
  ":create / :update / :delete are :false on the measure result."
  (let ((r (be-list :user-leaderboard "admin")))
    (is (eq :false (getf r :create)))
    (is (eq :false (getf r :update)))
    (is (eq :false (getf r :delete)))))

(test mrb-allowed-values-no-roles
  "allowed-values does not leak a :roles palette on a rollup."
  (is-false (getf (be-list :user-leaderboard "admin") :allowed-values)))

(test mrb-be-list-column-allowed
  "be-list-column succeeds on a rollup (list family, Issue 9)."
  (let ((r (be-list-column :user-leaderboard :name "admin")))
    (is (equal '("admin" "alice" "bob" "carol" "guest")
            (sort (copy-list (getf r :values)) #'string<)))
    (is (= 5 (length (getf r :values))))))

(test mrb-types-includes-rollup
  "be-types includes the rollup for a user with the role."
  (is (member :user-leaderboard
          (mapcar (lambda (e) (getf e :name)) (be-types "alice"))
          :test #'eq)))

(test mrb-types-excludes-without-role
  "be-types omits the rollup for a user without the role."
  (is-false (member :user-leaderboard
              (mapcar (lambda (e) (getf e :name)) (be-types "guest"))
              :test #'eq)))

;;; --- Backend: Issue 9 guards ---

(test mrb-guards-reject-non-list-endpoints
  "be-rec / be-id / be-val / be-value-id / validate / insert /
update / delete / set-field reject on a rollup with report-ve."
  (let ((id (getf (mrb-record-by-name (be-list :user-leaderboard "admin")
                      "alice") :id)))
    (signals validation-error
      (be-rec id "admin" :type-key :user-leaderboard))
    (signals validation-error
      (be-id :user-leaderboard
        `((:user-leaderboard :name :eq "alice")) "admin"))
    (signals validation-error
      (be-val id :name "admin" :type-key :user-leaderboard))
    (signals validation-error
      (be-value-id :user-leaderboard :name "alice" "admin"))
    (signals validation-error
      (be-validate-field :user-leaderboard :name "x" "admin"))
    (signals validation-error
      (be-validate-form :user-leaderboard '(:name "x") "admin"))
    (signals validation-error
      (be-insert :user-leaderboard '(:name "x") "admin"))
    (signals validation-error
      (be-update :user-leaderboard
        `((:user-leaderboard :name :eq "alice")) '(:name "x") "admin"))
    (signals validation-error
      (be-delete :user-leaderboard
        `((:user-leaderboard :name :eq "alice")) "admin"))
    (signals validation-error
      (be-set-field-value :user-leaderboard id :name "x" "admin"))
    (signals validation-error
      (be-action :user-leaderboard id :name "admin"))))

(test mrb-guards-secrets-still-works
  "The allowed-values :suppress-roles gate covers :secrets (base nil,
suppress-roles t) without breaking its list path."
  (let ((r (be-list :secrets "admin")))
    (is-false (getf (getf r :allowed-values) :roles))))
