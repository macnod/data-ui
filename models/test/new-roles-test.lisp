'(:title "New Roles Test"
  :name "new-roles-test"
  :version "0.1"
  :domain "new-roles-test.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  :landing-page :widgets
  ;; Declared roles exist after set-model, even when no :type-roles
  ;; references them (:test-writer is deliberately unreferenced).
  ;; :test-reader also appears in :type-roles below — the declared
  ;; permission list must win over ensure-model-roles' full-CRUD
  ;; default. :ai-user is the model-generator badge role.
  :new-roles (:ai-user ("read")
              :test-reader ("read")
              :test-writer ("create" "update"))
  :types
  (:widgets
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("widgets-user" "test-reader" "ai-user")
      :fields
      (:name
        (:type :text :identity t :searchable t
          :ui (:label "Name" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))))
