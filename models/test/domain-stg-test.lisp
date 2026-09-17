'(:title "Domain Stg Test"
  :name "domain-stg-test"
  :version "0.1"
  :domain "domain-stg.demo.data-ui.com"
  :repl t
  :types
  (:widgets
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("widgets-user")
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Name" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))))
