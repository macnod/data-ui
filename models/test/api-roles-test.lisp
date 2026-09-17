'(:title "API Roles Test"
  :name "api-roles-test"
  :version "0.1"
  :domain "api-roles-test.demo.data-ui.com"
  :repl t
  :guest-allowed t
  :api-roles ("logged-in" "public")
  :landing-page :widgets
  :types
  (:widgets
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("public")
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
