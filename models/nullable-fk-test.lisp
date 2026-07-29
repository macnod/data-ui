;; Test fixture: a type with a nullable foreign-key field (:target
;; without :not-null t).  Used by tests/nullable-fk-tests.lisp to
;; verify that the compiler, validation layer, and backend all accept
;; NIL for optional FK references.
'(:title "Nullable FK Test"
  :name "nullable-fk-test"
  :version "0.1"
  :domain "nullable-fk-test.demo.data-ui.com"
  :repl nil
  :types
  (:tasks
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:tasks :users)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Task" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :assigned-to
        (:type :text :target :users
          :ui (:label "Assigned To" :widget :select)
          :source (:view :main :table :users :column :name
                   :agg :first)
          :source-all (:view :main :table :users :column :name
                       :agg :list)
          :column t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t)))))
