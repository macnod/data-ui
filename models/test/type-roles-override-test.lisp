'(:title "Type Roles Override Test"
  :name "type-roles-override-test"
  :version "0.1"
  :domain "type-roles-override-test.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  :landing-page :items
  :types
  (:items
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("item-users")
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Item" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    ;; Base-type overrides: admin-only Users / Roles / Permissions.
    ;; :settings is deliberately NOT restricted (users must still
    ;; reach Settings). Partial overlay only — fields, views, and
    ;; RBAC CRUD functions come from *base-model*.
    :users (:type-roles ("admin"))
    :roles (:type-roles ("admin"))
    :permissions (:type-roles ("admin"))))
