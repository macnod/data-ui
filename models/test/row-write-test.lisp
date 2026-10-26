'(:title "Row Write"
  :name "row-write-test"
  :version "0.1"
  :domain "row-write.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  :new-roles (:notes-user ("create" "read"))
  :types
  (:notes
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("notes-user")
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Note" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :body
        (:type :text
          :ui (:label "Body" :widget :textarea)
          :source (:view :main :column :body :agg :first)
          :column t)
        :test-sync
        (:type :button
          :ui (:label "Test Sync" :widget :button)
          :action (:test-sync)))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))))
