'(:title ":model:"
  :name ":model:"
  :version "0.1"
  :domain ":model:.demo.data-ui.com"
  :repl t
  :types
  (::model:
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles (":model:-users")
      :views (:main (:tables (::model:)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label ":model: name" :input-type :line)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t)))
