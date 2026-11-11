;; Test fixture: referrers declared before their :target types —
;; ordinary FK fields and an internal joiner alike.  Used by
;; tests/forward-ref-tests.lisp to pin dependency-ordered table
;; creation (create-tables): declaration order must not constrain FK
;; targets.  Without the ordering, rt_assignments and rt_work_tasks
;; both die on Postgres 42P01 (relation does not exist).
'(:title "Forward Reference Test"
  :name "forward-ref-test"
  :version "0.1"
  :domain "forward-ref-test.demo.data-ui.com"
  :repl nil
  :types
  (:assignments
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:assignments :workers :tasks)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Assignment" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :worker
        (:type :text :target :workers
          :ui (:label "Worker" :widget :search-select)
          :source (:view :main :table :workers :column :name
                   :agg :first)
          :source-all (:view :main :table :workers :column :name
                       :agg :list)
          :column t)
        :task
        (:type :text :target :tasks
          :ui (:label "Task" :widget :search-select)
          :source (:view :main :table :tasks :column :title
                   :agg :first)
          :source-all (:view :main :table :tasks :column :title
                       :agg :list)
          :column t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

    ;; Internal joiner declared before its parents: its CREATE
    ;; carries inline FKs to two missing tables.
    :work-tasks
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :workers)
        :reference (:target :tasks)))

    :workers
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Worker" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

    :tasks
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :fields
      (:title
        (:type :text :identity t
          :ui (:label "Task" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))))
