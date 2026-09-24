'(:title "To Do List"
  :name "todos"
  :version "0.1"
  :domain "todo.demo.data-ui.com"
  :domain-stg "todo-stg.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  ;; Anyone can log in as the read-only seeded guest user, with any
  ;; password. :api-roles lets the guest (public role, no logged-in)
  ;; reach the app-level endpoints (/api/types, /api/info,
  ;; /api/css-variables), and "public" in :type-roles opens the todos
  ;; and tags types to guest reads; row visibility is still per-record
  ;; (rows created via the UI copy the type roles, including public).
  :guest-allowed t
  :guest-auto nil
  :api-roles ("logged-in" "public")
  :landing-page :todos
  :types
  (:todos
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("todo-users" "public")
      :default-sort (:done :asc)
      :views (:main (:tables (:todos :todo-tags :tags))
               :tags (:tables (:tags)))
      :fields
      (:name 
        (:type :text :identity t :sortable t :searchable t
          :ui (:label "To Do" :widget :textbox)
          :validations (:required (:max-length :max 80))
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :done
        (:type :boolean :default :false :sortable t
          :ui (:label "Done" :widget :checkbox)
          :source (:view :main :column :done :agg :first)
          :column t :not-null t)
        :tags
        (:type :list
          :ui (:label "Tags" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :tags :column :name :agg :distinct)
          :source-all (:view :tags :table :tags :column :name :agg :list)
          :join-table :todo-tags))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :tags
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("todo-users" "public")
      :fields 
      (:name
        (:type :text :identity t :sortable t
          :ui (:label "Tag" :widget :textbox)
          :validations (:required)
          :source (:view :main :table :tags :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :todo-tags
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :todos)
        :reference (:target :tags)))
     ;; Petting-zoo D1 overlays: guest reads the built-in account /
     ;; role / permission lists (tier identity stays visible; :settings
     ;; stays structurally unreachable — guest is never granted the
     ;; settings role). add-type-roles only inserts missing resources,
     ;; so these must be present before the profile's first set-model.
     :users (:type-roles ("logged-in" "public" "user-creator"))
     :roles (:type-roles ("logged-in" "public" "role-creator"))
     :permissions (:type-roles ("logged-in" "public" "permission-creator"))))
