(:title "Widgets"
  :name "widgets"
  :version "0.1"
  :domain "widgets.demo.data-ui.com"
  :repl t
  :types
  (:widget
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("widget-users")
      :views (:main (:tables (:widgets)))
      :fields
      (:name
        (:type :text
          :ui (:label "Widget Name" :input-type :line)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t)))
