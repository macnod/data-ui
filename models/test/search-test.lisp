'(:Title "Search Test"
  :name "search-test"
  :version "0.1"
  :domain "search-test.demo.data-ui.com"
  :repl nil
  :types
  (:items
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("search-test-users")
      :fields
      (:name
        (:type :text :identity t :searchable t
          :ui (:label "Name" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :description
        (:type :text :searchable t
          :ui (:label "Description" :widget :textarea)
          :source (:view :main :column :description :agg :first)
          :column t :not-null nil))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))))
