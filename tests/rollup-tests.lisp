(in-package :data-ui)

(def-suite rollup-suite
  :description "08a rollup compile surface: matrix, single-fact, filter DSL,
:type/:agg checks, no DDL/DML")

(in-suite rollup-suite)

;;; --- Helpers ---

(defun ru-sans (plist &rest keys)
  "Copy PLIST without any key in KEYS."
  (loop for k in plist by #'cddr
    for v in (cdr plist) by #'cddr
    unless (member k keys)
    append (list k v)))

(defun ru-with (base overrides)
  "Merge OVERRIDES into BASE: replaced keys keep their position, new keys
append. No duplicate keys (unlike append)."
  (loop with replaced = (u:plist-keys overrides)
    for k in base by #'cddr
    for v in (cdr base) by #'cddr
    when (member k replaced)
    append (list k (getf overrides k)) into merged
    else
    append (list k v) into merged
    finally
    (return
      (append merged
        (loop with base-keys = (u:plist-keys base)
          for k in overrides by #'cddr
          for v in (cdr overrides) by #'cddr
          unless (member k base-keys)
          append (list k v))))))

(defun ru-base-model ()
  "Base model types shared by the rollup tests."
  (list
    :chores
    (list :table t :create :auto :update :auto :delete :auto
      :type-roles '("chore-user")
      :views (list :main (list :tables '(:chores :users)))
      :fields (list
                :title (list :type :text :identity t
                         :ui (list :label "Title" :widget :textbox)
                         :source (list :view :main :column :title
                                   :agg :first)
                         :column t :not-null t)
                :user (list :type :text
                        :ui (list :label "User" :widget :textbox)
                        :target :users
                        :source (list :view :main :table :users
                                  :column :name :agg :first)
                        :column t :not-null t)
                :points (list :type :integer
                          :ui (list :label "Points" :widget :textbox)
                          :source (list :view :main :column :points
                                    :agg :first)
                          :column t :not-null t)
                :completed (list :type :boolean :css-value t
                             :ui (list :label "Completed"
                                   :widget :checkbox)
                             :source (list :view :main :column
                                       :completed :agg :first)
                             :column t :not-null t)
                :completed-at (list :type :timestamp
                                :ui (list :label "Completed At"
                                      :widget :textbox)
                                :source (list :view :main :column
                                          :completed-at :agg :first)
                                :column t))
      :list-form (list :fields t)
      :update-form (list :fields t)
      :add-form (list :fields t))

    :tasks
    (list :table t :create :auto :update :auto :delete :auto
      :type-roles '("task-user")
      :views (list :main (list :tables '(:tasks :users)))
      :fields (list
                :title (list :type :text :identity t
                         :ui (list :label "Title" :widget :textbox)
                         :source (list :view :main :column :title
                                   :agg :first)
                         :column t :not-null t)
                :user (list :type :text
                        :ui (list :label "User" :widget :textbox)
                        :target :users
                        :source (list :view :main :table :users
                                  :column :name :agg :first)
                        :column t :not-null t)
                ;; :points mirrors the rollup-test fixture's tasks
                ;; shape so plan 10's Test 15 builders (measures on
                ;; :tasks :points) validate against this base model
                ;; too. Harmless elsewhere: validate-model never
                ;; runs DDL for ru-base-model.
                :points (list :type :integer
                          :ui (list :label "Points" :widget :textbox)
                          :source (list :view :main :column :points
                                    :agg :first)
                          :column t :not-null t)
                :completed (list :type :boolean :css-value t
                             :ui (list :label "Completed"
                                   :widget :checkbox)
                             :source (list :view :main :column
                                       :completed :agg :first)
                             :column t :not-null t))
      :list-form (list :fields t)
      :update-form (list :fields t)
      :add-form (list :fields t))

    :task-notes
    (list :table t :create :auto :update :auto :delete :auto
      :type-roles '("note-user")
      :views (list :main (list :tables '(:task-notes :tasks)))
      :fields (list
                :note (list :type :text
                        :ui (list :label "Note" :widget :textarea)
                        :source (list :view :main :column :note
                                  :agg :first)
                        :column t :not-null t)
                :task (list :type :text
                        :ui (list :label "Task" :widget :textbox)
                        :target :tasks
                        :source (list :view :main :table :tasks
                                  :column :title :agg :first)
                        :column t :not-null t))
      :list-form (list :fields t)
      :update-form (list :fields t)
      :add-form (list :fields t))

    ;; A table with no :user field, for the negative :scope test.
    ;; :plain-notes is its fact table.
    :plain-things
    (list :table t :create :auto :update :auto :delete :auto
      :type-roles '("plain-user")
      :views (list :main (list :tables '(:plain-things)))
      :fields (list
                :title (list :type :text :identity t
                         :ui (list :label "Title" :widget :textbox)
                         :source (list :view :main :column :title
                                   :agg :first)
                         :column t :not-null t)
                :size (list :type :integer
                        :ui (list :label "Size" :widget :textbox)
                        :source (list :view :main :column :size
                                  :agg :first)
                        :column t :not-null t))
      :list-form (list :fields t)
      :update-form (list :fields t)
      :add-form (list :fields t))

    :plain-notes
    (list :table t :create :auto :update :auto :delete :auto
      :type-roles '("plain-note-user")
      :views (list :main (list :tables '(:plain-notes :plain-things)))
      :fields (list
                :note (list :type :text
                        :ui (list :label "Note" :widget :textarea)
                        :source (list :view :main :column :note
                                  :agg :first)
                        :column t :not-null t)
                :thing (list :type :text
                         :ui (list :label "Thing" :widget :textbox)
                         :target :plain-things
                         :source (list :view :main :table
                                   :plain-things
                                   :column :title :agg :first)
                         :column t :not-null t))
      :list-form (list :fields t)
      :update-form (list :fields t)
      :add-form (list :fields t))))

(defun ru-model (rollup-spec)
  "User types + one rollup type :user-leaderboard whose type-def
is ROLLUP-SPEC. compile-model appends *base-model* itself
(stage-1); do not include it here."
  (append (ru-base-model)
    (list :user-leaderboard rollup-spec)))

(defun ru-leaderboard (&rest overrides)
  "The 07e freeze example (no :sortable variant) as a plist,
with OVERRIDES merged last (ru-with: no duplicate keys)."
  (ru-with
    (list
      :rollup t
      :grain :users
      :type-roles '("leaderboard-viewers")
      :filter '((:chores :completed :eq t))
      :views (list :main (list :tables '(:users :chores)))
      :list-form (list :fields t)
      :fields (list
                (list :name
                  (list :source (list :view :main :table :users
                                  :column :name :agg :first)
                    :ui (list :label "User")))
                (list :total-points
                  (list :type :integer
                    :source (list :view :main :table :chores
                              :column :points :agg :sum)
                    :ui (list :label "Points")))
                (list :chores-done
                  (list :type :integer
                    :source (list :view :main :table :chores
                              :column :id :agg :count)
                    :ui (list :label "Completed")))))
    overrides))


(defun ru-compile (rollup-spec)
  "Compile a model containing ROLLUP-SPEC as :user-leaderboard.
Returns the compiled :user-leaderboard type-def."
  (getf (compile-model (ru-model rollup-spec)) :user-leaderboard))

(defun ru-note-summary (&rest overrides)
  "3-table chain (:users :tasks :task-notes), F on the leaf."
  (ru-with
    (list
      :rollup t
      :grain :users
      :type-roles '("note-viewers")
      :views (list :main (list :tables '(:users :tasks :task-notes)))
      :list-form (list :fields t)
      :fields (list
                (list :name
                  (list :source (list :view :main :table :users
                                  :column :name :agg :first)))
                (list :notes-on-done
                  (list :type :integer
                    :source (list :view :main :table :task-notes
                              :column :id :agg :count)))))
    overrides))

;;; --- Freeze example compiles (07e, no-sortable variant) ---

(test rollup-freeze-example-compiles
  "The 07e freeze example (no :sortable) compiles with
:phase-a-shape :measure."
  (let ((td (ru-compile (ru-leaderboard))))
    (is (eq (getf td :phase-a-shape) :measure))))

(test rollup-no-table-name
  "No :table-name on the compiled rollup; no phantom
rt_user-leaderboard even as a string."
  (is-false (getf (ru-compile (ru-leaderboard)) :table-name)))

(test rollup-no-ddl-dml
  "No :create-table-sql / :insert-sql / :update-sql /
:delete-sql / :search-sql on the compiled rollup."
  (let ((td (ru-compile (ru-leaderboard))))
    (dolist (key '(:create-table-sql :insert-sql :update-sql
                    :delete-sql :search-sql))
      (is-false (getf td key)))))

(test rollup-no-default-timestamps
  "No injected :created-at / :updated-at on a rollup."
  (let ((td (ru-compile (ru-leaderboard))))
    (is-false (getf (getf td :fields) :created-at))
    (is-false (getf (getf td :fields) :updated-at))))

(test rollup-injected-id-is-grain-passthrough
  "Injected :id is a grain pass-through: :source names the grain
table with :agg :first, and there is no :column t."
  (let* ((td (ru-compile (ru-leaderboard)))
          (id-def (getf (getf td :fields) :id)))
    (is (eq (getf id-def :type) :uuid))
    (is-false (getf id-def :column))
    (is-false (getf id-def :primary-key))
    (is (eq (u:tree-get id-def :source :table) :users))
    (is (eq (u:tree-get id-def :source :column) :id))
    (is (eq (u:tree-get id-def :source :agg) :first))))

(test rollup-view-no-flat-sql
  "The rollup view keeps :tables / :aliases / :columns but has
no :sql / :phase-a-base-sql / :phase-a-join-sql."
  (let ((view (u:tree-get (ru-compile (ru-leaderboard))
                :views :main)))
    (is (equal (getf view :tables) '(:users :chores)))
    (is (u:tree-get view :aliases :users :name))
    (is (u:tree-get view :columns :users :name))
    (is-false (getf view :sql))
    (is-false (getf view :phase-a-base-sql))
    (is-false (getf view :phase-a-join-sql))))

(test rollup-display-defaults-t
  ":display defaults to t on a rollup when omitted; explicit
nil is honored."
  (is-true (getf (ru-compile (ru-leaderboard)) :display))
  (is-false (getf (ru-compile (ru-leaderboard :display nil))
              :display)))

(test rollup-suppress-roles-t
  "The compiler auto-sets :suppress-roles t on a rollup."
  (is-true (getf (ru-compile (ru-leaderboard)) :suppress-roles)))

(test rollup-category-user
  "A rollup with no :category derives :user."
  (is (eq (getf (ru-compile (ru-leaderboard)) :category) :user)))

(test rollup-crud-absent
  ":create / :update / :delete are nil on the compiled rollup."
  (let ((td (ru-compile (ru-leaderboard))))
    (is-false (getf td :create))
    (is-false (getf td :update))
    (is-false (getf td :delete))))

;;; --- preliminary-model-check accepts :filter (0a) ---

(test rollup-walker-accepts-filter
  "Type-level :filter (a list of 4-tuples) survives
preliminary-model-check."
  (finishes
    (preliminary-model-check
      (append *base-model*
        (list :user-leaderboard (ru-leaderboard))))))

;;; --- 07b incompatible-key matrix ---

(test rollup-rejects-table-key
  ":table present on a rollup is report-e."
  (signals error (ru-compile (ru-leaderboard :table t))))

(test rollup-rejects-type-level-type
  "Type-level :type is report-e; :rollup t is the sole
declaration."
  (signals error (ru-compile (ru-leaderboard :type :rollup))))

(test rollup-rejects-grain-missing
  "Missing :grain is report-e."
  (signals error
    (ru-compile (ru-sans (ru-leaderboard) :grain))))

(test rollup-rejects-grain-not-first
  "Grain not first in :tables is report-e; no silent reorder."
  (signals error
    (ru-compile
      (ru-leaderboard
        :views (list :main (list :tables '(:chores :users)))))))

(test rollup-rejects-grain-absent-from-tables
  "Grain absent from :tables is report-e."
  (signals error
    (ru-compile
      (ru-leaderboard
        :views (list :main (list :tables '(:users :tasks)))))))

(test rollup-rejects-missing-list-form
  "Missing / nil :list-form on a rollup is report-e."
  (signals error
    (ru-compile (ru-sans (ru-leaderboard) :list-form)))
  (signals error (ru-compile (ru-leaderboard :list-form nil))))

(test rollup-rejects-empty-list-form-fields
  ":list-form (:fields nil) is report-e."
  (signals error
    (ru-compile (ru-leaderboard :list-form (list :fields nil)))))

(test rollup-rejects-add-form
  ":add-form present at all (even nil) is report-e."
  (signals error
    (ru-compile (ru-leaderboard :add-form (list :fields t))))
  (signals error (ru-compile (ru-leaderboard :add-form nil))))

(test rollup-rejects-update-form
  ":update-form present at all is report-e."
  (signals error
    (ru-compile (ru-leaderboard :update-form (list :fields t)))))

(test rollup-rejects-extra-views
  "Extra views beyond :main are report-e."
  (signals error
    (ru-compile
      (ru-leaderboard
        :views (list :main (list :tables '(:users :chores))
                 :extra (list :tables '(:users)))))))

(test rollup-rejects-lifecycle-slots
  "Lifecycle slots on a rollup are report-e."
  (signals error
    (ru-compile (ru-leaderboard :pre-create :some-hook))))

(test rollup-rejects-write-to
  ":write-to on a rollup field is report-e."
  (signals error
    (ru-compile
      (ru-leaderboard
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :wrote
                    (list :type :text
                      :write-to (list :table :chores :title :value)
                      :source (list :view :main :table :chores
                                :column :title :agg :first))))))))

(test rollup-rejects-button-field
  ":button fields are not allowed on a rollup."
  (signals error
    (ru-compile
      (ru-leaderboard
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :go
                    (list :type :button
                      :action '(:noop)
                      :ui (list :widget :button))))))))

(test rollup-rejects-author-id
  "Author-declared :id is report-e; the compiler injects it."
  (signals error
    (ru-compile
      (ru-leaderboard
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
                      :source (list :view :main :table :chores
                                :column :points :agg :sum))))))))

(test rollup-rejects-column-t
  ":column t on a rollup field is report-e (a rollup has no
table)."
  (signals error
    (ru-compile
      (ru-leaderboard
        :fields (list
                  (list :name
                    (list :column t
                      :source (list :view :main :table :users
                                :column :name :agg :first))))))))

(test rollup-rejects-suppress-roles-nil
  "Author :suppress-roles nil on a rollup is report-e; the
compiler auto-sets it."
  (signals error
    (ru-compile (ru-leaderboard :suppress-roles nil))))

(test rollup-rejects-create-key
  "Explicit :create on a rollup is report-e."
  (signals error (ru-compile (ru-leaderboard :create :auto))))

(test rollup-rejects-update-delete-keys
  "Explicit :update / :delete on a rollup is report-e."
  (signals error (ru-compile (ru-leaderboard :update :auto)))
  (signals error (ru-compile (ru-leaderboard :delete :auto))))

(test rollup-rejects-type-flag-keys
  "Type-flag keys (:tree / :fs-backed / :is-leaf / :parent-type /
:user-setting / :is-joiner / :built-in) on a rollup are
report-e."
  (dolist (key '(:tree :fs-backed :is-leaf :parent-type
                  :user-setting :is-joiner :built-in))
    (signals error (ru-compile (ru-leaderboard key t)))))

(test rollup-rejects-field-forbidden-attrs
  "Field attributes :identity / :autofill / :compose /
:source-all on a rollup field are report-e."
  (dolist (attr '(:identity :autofill :compose :source-all))
    (signals error
      (ru-compile
        (ru-with
          (ru-sans (ru-leaderboard) :filter)
          (list
            :fields (list
                      (list :name
                        (append
                          (list :source (list :view :main :table :users
                                          :column :name :agg :first))
                          (list attr
                            (case attr
                              (:identity t)
                              (:autofill :user)
                              (:compose "x")
                              (:source-all
                                (list :view :main :table :chores
                                  :column :title :agg :list))))))
                      (list :total-points
                        (list :type :integer
                          :source (list :view :main :table :chores
                                    :column :points :agg :sum))))))))))

(test rollup-rejects-rollup-grain
  "A rollup cannot be a grain (no physical table). Self-grain is
the degenerate case of the same check."
  (signals error
    (ru-compile (ru-with (ru-sans (ru-leaderboard) :filter)
                  (list :grain :user-leaderboard)))))

(test non-rollup-rejects-table-nil
  ":table nil on a non-rollup is report-e — a rejected :rollup
synonym, not an alias (07e freeze)."
  (signals error
    (compile-model
      (append (ru-base-model)
        (list :plain
          (list :table nil :create :auto :update :auto :delete :auto
            :views (list :main (list :tables '(:plain)))
            :fields (list
                      :name (list :type :text
                              :ui (list :label "N" :widget :textbox)
                              :source (list :view :main :column :name
                                        :agg :first)
                              :column t :not-null t))
            :list-form (list :fields t)
            :update-form (list :fields t)
            :add-form (list :fields t)))))))

(defun ru-plain-model ()
  "One plain table type, for non-rollup shape checks."
  (list :plain
    (list :table t :create :auto :update :auto :delete :auto
      :views (list :main (list :tables '(:plain)))
      :fields (list
                :name (list :type :text
                        :ui (list :label "N" :widget :textbox)
                        :source (list :view :main :column :name
                                  :agg :first)
                        :column t :not-null t))
      :list-form (list :fields t)
      :update-form (list :fields t)
      :add-form (list :fields t))))

(test non-rollup-gets-phase-a-shape-base
  "Non-rollup types compile with :phase-a-shape :base (07d)."
  (let ((td (getf (compile-model (ru-plain-model)) :plain)))
    (is (eq (getf td :phase-a-shape) :base))))

(test non-rollup-rejects-grain
  ":grain on a non-rollup type is report-e."
  (signals error
    (compile-model
      (append (ru-base-model)
        (list :plain
          (list :table t :create :auto :update :auto :delete :auto
            :grain :users
            :views (list :main (list :tables '(:plain)))
            :fields (list
                      :name (list :type :text
                              :ui (list :label "N" :widget :textbox)
                              :source (list :view :main :column :name
                                        :agg :first)
                              :column t :not-null t))
            :list-form (list :fields t)
            :update-form (list :fields t)
            :add-form (list :fields t)))))))

(test non-rollup-rejects-filter
  "Model-declared :filter on a non-rollup type is report-e."
  (signals error
    (compile-model
      (append (ru-base-model)
        (list :plain
          (list :table t :create :auto :update :auto :delete :auto
            :filter '((:chores :completed :eq t))
            :views (list :main (list :tables '(:plain)))
            :fields (list
                      :name (list :type :text
                              :ui (list :label "N" :widget :textbox)
                              :source (list :view :main :column :name
                                        :agg :first)
                              :column t :not-null t))
            :list-form (list :fields t)
            :update-form (list :fields t)
            :add-form (list :fields t)))))))

;;; --- Single fact table (Issue 16) ---

(test rollup-rejects-grain-only
  "A grain-only rollup (every author field :agg :first) is
report-e."
  (signals error
    (ru-compile
      (ru-leaderboard
        :filter nil
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first))))))))

(test rollup-rejects-measure-on-grain
  "A real measure whose :source :table is the grain is
report-e."
  (signals error
    (ru-compile
      (ru-leaderboard
        :filter nil
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :user-count
                    (list :type :integer
                      :source (list :view :main :table :users
                                :column :id :agg :count))))))))

(test rollup-rejects-first-off-grain
  ":agg :first on a non-grain table is report-e."
  (signals error
    (ru-compile
      (ru-leaderboard
        :filter nil
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :chore-title
                    (list :source (list :view :main :table :chores
                                    :column :title :agg :first)))
                  (list :total-points
                    (list :type :integer
                      :source (list :view :main :table :chores
                                :column :points :agg :sum))))))))

(test rollup-rejects-mixed-depth
  "Two distinct non-grain :source :table keys among real
measures is report-e."
  (signals error
    (ru-compile
      (ru-leaderboard
        :filter nil
        :views (list :main (list :tables '(:users :chores :tasks)))
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :total-points
                    (list :type :integer
                      :source (list :view :main :table :chores
                                :column :points :agg :sum)))
                  (list :task-count
                    (list :type :integer
                      :source (list :view :main :table :tasks
                                :column :id :agg :count))))))))

(test rollup-rejects-extra-arm
  "A table in :tables but not on the path grain → F is an extra
arm (report-e). F is last; :tasks is the extra arm."
  (signals error
    (ru-compile
      (ru-leaderboard
        :filter nil
        :views (list :main (list :tables '(:users :tasks :chores)))
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :total-points
                    (list :type :integer
                      :source (list :view :main :table :chores
                                :column :points :agg :sum)))
                  (list :chores-done
                    (list :type :integer
                      :source (list :view :main :table :chores
                                :column :id :agg :count))))))))

(test rollup-rejects-hop-past-f
  "F not last in :tables (a hop past F) is report-e."
  (signals error
    (ru-compile
      (ru-note-summary
        :views (list :main
                 (list :tables '(:users :task-notes :tasks)))))))

;;; --- Hop-binding (Issue 17) ---

(test rollup-hop-binding-filter-compiles
  "A :filter clause naming the intermediate table (on the path
grain → F) compiles."
  (finishes
    (ru-compile (ru-note-summary
                  :filter '((:tasks :completed :eq t))))))

(test rollup-off-path-filter-rejected
  "A :filter clause naming a table not in :tables is
report-e."
  (signals error
    (ru-compile (ru-note-summary
                  :filter '((:chores :completed :eq t))))))

;;; --- Filter DSL grammar (07c) ---

(test rollup-rejects-filter-nil
  ":filter nil and :filter () are compile errors (absent is
legal)."
  (signals error (ru-compile (ru-leaderboard :filter nil)))
  (signals error (ru-compile (ru-leaderboard :filter '()))))

(test rollup-rejects-singular-filter
  "A singular 4-tuple (not wrapped in a list) is report-e."
  (signals error
    (ru-compile (ru-leaderboard :filter '(:chores :completed :eq t)))))

(test rollup-rejects-3-ary-clause
  "A 3-element clause is report-e."
  (signals error
    (ru-compile (ru-leaderboard :filter '((:chores :completed :eq))))))

(test rollup-rejects-non-list-clause
  "A non-list clause is report-e."
  (signals error
    (ru-compile (ru-leaderboard :filter '(:chores)))))

(test rollup-rejects-unknown-filter-table
  "A clause naming a table not in :tables is report-e."
  (signals error
    (ru-compile (ru-leaderboard :filter '((:tasks :completed :eq t))))))

(test rollup-rejects-unknown-filter-column
  "A clause naming a column that does not exist on the table is
report-e."
  (signals error
    (ru-compile (ru-leaderboard :filter '((:chores :bogus :eq t))))))

(test rollup-rejects-request-time-operator
  ":like / :in never enter the model-declared path."
  (signals error
    (ru-compile (ru-leaderboard :filter '((:chores :title :like "x")))))
  (signals error
    (ru-compile
      (ru-leaderboard :filter '((:chores :title :in ("x")))))))

(test rollup-rejects-eq-on-timestamp
  ":eq on a timestamp column is report-e (use :last-days /
:calendar)."
  (signals error
    (ru-compile
      (ru-leaderboard
        :filter '((:chores :completed-at :eq "2026-01-01 00:00:00"))))))

(test rollup-accepts-last-days
  ":last-days on a timestamp column with an integer 1..1825
compiles."
  (finishes
    (ru-compile (ru-leaderboard :filter '((:chores :completed-at
                                            :last-days 30))))))

(test rollup-rejects-bad-last-days
  ":last-days with 0, a negative, a float, a string, or > 1825
is report-e."
  (dolist (bad '(0 -5 30.0 "30" 1826))
    (signals error
      (ru-compile
        (ru-leaderboard
          :filter `((:chores :completed-at :last-days ,bad)))))))

(test rollup-rejects-last-days-on-non-timestamp
  ":last-days on a boolean column is report-e."
  (signals error
    (ru-compile (ru-leaderboard :filter '((:chores :completed
                                            :last-days 30))))))

(test rollup-accepts-calendar-month
  ":calendar :month on a timestamp column compiles."
  (finishes
    (ru-compile (ru-leaderboard :filter '((:chores :completed-at
                                            :calendar :month))))))

(test rollup-rejects-calendar-units
  ":calendar values other than :month are report-e (strings
too)."
  (signals error
    (ru-compile (ru-leaderboard :filter '((:chores :completed-at
                                            :calendar :year)))))
  (signals error
    (ru-compile (ru-leaderboard :filter '((:chores :completed-at
                                            :calendar "month"))))))

(test rollup-rejects-bad-discrete-value
  ":eq t on a text column is report-e; \"true\" on a boolean is
report-e."
  (signals error
    (ru-compile (ru-leaderboard :filter '((:chores :title :eq t)))))
  (signals error
    (ru-compile (ru-leaderboard :filter '((:chores :completed :eq
                                            "true"))))))

(test rollup-accepts-grain-table-clause
  "A grain-table clause is not an orphan; it compiles (08b
renders it as WHERE)."
  (finishes
    (ru-compile (ru-leaderboard
                  :filter '((:chores :completed :eq t)
                             (:users :name :eq "alice"))))))

;;; --- Field :type vs :agg (07a / Issue 13) ---

(test rollup-rejects-passthrough-type-mismatch
  "A pass-through whose declared :type differs from the grain
field's is report-e."
  (signals error
    (ru-compile
      (ru-leaderboard
        :filter nil
        :fields (list
                  (list :name
                    (list :type :integer
                      :source (list :view :main :table :users
                                :column :name :agg :first))))))))

(test rollup-passthrough-text-default
  "A text pass-through may omit :type (defaults to :text and
matches)."
  (finishes
    (ru-compile
      (ru-with
        (ru-sans (ru-leaderboard) :filter)
        (list
          :fields (list
                    (list :name
                      (list :source (list :view :main :table :users
                                      :column :name :agg :first)))
                    (list :total-points
                      (list :type :integer
                        :source (list :view :main :table :chores
                                  :column :points :agg :sum)))))))))

(test rollup-rejects-count-not-integer
  ":count must be :type :integer."
  (signals error
    (ru-compile
      (ru-leaderboard
        :filter nil
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :chores-done
                    (list :type :real
                      :source (list :view :main :table :chores
                                :column :id :agg :count))))))))

(test rollup-rejects-avg-not-real
  ":avg must be :type :real."
  (signals error
    (ru-compile
      (ru-leaderboard
        :filter nil
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :avg-points
                    (list :type :integer
                      :source (list :view :main :table :chores
                                :column :points :agg :avg))))))))

(test rollup-rejects-sum-type-mismatch
  ":sum must declare the same numeric type as the source
column."
  (signals error
    (ru-compile
      (ru-leaderboard
        :filter nil
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :total-points
                    (list :type :real
                      :source (list :view :main :table :chores
                                :column :points :agg :sum))))))))

(test rollup-rejects-sum-non-numeric
  ":sum needs a numeric source column."
  (signals error
    (ru-compile
      (ru-leaderboard
        :filter nil
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :titles
                    (list :type :text
                      :source (list :view :main :table :chores
                                :column :title :agg :sum))))))))

(test rollup-rejects-list-type-mismatch
  ":list / :distinct must declare the source column's type."
  (signals error
    (ru-compile
      (ru-leaderboard
        :filter nil
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)))
                  (list :titles
                    (list :type :integer
                      :source (list :view :main :table :chores
                                :column :title :agg :list))))))))

;;; --- :scope :user compile checks ---

(test rollup-scope-user-on-users-grain-compiles
  ":scope :user on a :users grain compiles."
  (finishes
    (ru-compile
      (ru-leaderboard
        :views (list :main
                 (list :tables '(:users :chores) :scope :user))))))

(test rollup-scope-user-on-grain-with-user-field-compiles
  ":scope :user on a non-user grain that has a :user field
(:tasks) compiles."
  (finishes
    (ru-compile
      (ru-note-summary
        :grain :tasks
        :views (list :main
                 (list :tables '(:tasks :task-notes) :scope :user))
        :fields (list
                  (list :title
                    (list :source (list :view :main :table :tasks
                                    :column :title :agg :first)))
                  (list :note-count
                    (list :type :integer
                      :source (list :view :main :table :task-notes
                                :column :id :agg :count))))))))

(test rollup-scope-user-on-userless-grain-rejected
  ":scope :user on a non-user grain with no :user field is
report-e."
  (signals error
    (ru-compile
      (ru-note-summary
        :grain :plain-things
        :views (list :main
                 (list :tables '(:plain-things :plain-notes)
                   :scope :user))
        :fields (list
                  (list :title
                    (list :source (list :view :main :table
                                    :plain-things
                                    :column :title :agg :first)))
                  (list :note-count
                    (list :type :integer
                      :source (list :view :main :table :plain-notes
                                :column :id :agg :count))))))))

;;; --- Multi-hop compile (Issue 16 legal case) ---

(test rollup-multi-hop-compiles
  "A 3-table chain with the measure on the leaf compiles
(item 10 Test 13 shape)."
  (let ((td (ru-compile (ru-note-summary))))
    (is (eq (getf td :phase-a-shape) :measure))))
