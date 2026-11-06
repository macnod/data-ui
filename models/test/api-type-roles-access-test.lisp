'(:title "API Type-Roles Access Test"
  :name "api-type-roles-access-test"
  :version "0.1"
  :domain "api-type-roles-access-test.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  ;; Guest login is on so the seeded guest (public role, no Notes
  ;; type role) is an authenticated REST caller; :api-roles admits
  ;; public so guest also passes the app-level gates.
  :guest-allowed t
  :api-roles ("logged-in" "public")
  ;; row-writer carries exactly read/update/delete on row resources
  ;; that name it — a row-grant role with no type role anywhere.
  :new-roles (:row-writer ("read" "update" "delete"))
  :types
  (:notes
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("admin")
      :fields
      (:name
        (:type :text :identity t :searchable t
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
      :add-form (:fields t))

    ;; Built-in-base control: :users stays admin-only here, so a
    ;; type-gated built-in list denial is assertable on this fixture.
    ;; Partial overlay only — fields, views, and RBAC CRUD functions
    ;; come from *base-model*.
    :users (:type-roles ("admin"))))
