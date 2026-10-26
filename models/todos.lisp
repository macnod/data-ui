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
      :page-description "
A deliberately tiny to-do list: each item is a name, a **Done** checkbox, and
any number of **Tags**. Every item is owned by its creator, who decides which
users or roles can see it and which can edit it, and every change you make on
this page is plain CRUD on a real PostgreSQL database.

None of it was hand-built and none of it was generated. There are no thousands
of lines of emitted code to own. The engine running this app is generic; this
model is the only app-specific part in existence. There is nothing else: no
backend to write, no screens to build, no deployment config to write. Even the
domain you're reading this on is a line in the model. See `:domain`, near the
top.

Here is the whole application in less than fifty lines. Comments and this
description are the only omissions:

```
'(:title \"To Do List\"
  :name \"todos\" :version \"0.1\"
  :domain \"todo.demo.data-ui.com\" :domain-stg \"todo-stg.demo.data-ui.com\"
  :repl t :guest-allowed t :guest-auto nil
  :api-roles (\"logged-in\" \"public\") :landing-page :todos
  :types
  (:todos
    (:table t :create :auto :update :auto :delete :auto :display t
      :type-roles (\"todo-users\" \"public\") :default-sort (:done :asc)
      :views (:main (:tables (:todos :todo-tags :tags)) :tags (:tables (:tags)))
      :fields
      (:name (:type :text :identity t :sortable t :searchable t
               :ui (:label \"To Do\" :widget :textbox)
               :validations (:required (:max-length :max 80))
               :source (:view :main :column :name :agg :first)
               :column t :not-null t :unique t)
        :done (:type :boolean :default :false :sortable t
                :ui (:label \"Done\" :widget :checkbox :filter-with :boolean)
                :source (:view :main :column :done :agg :first)
                :column t :not-null t)
        :tags (:type :list
                :ui (:label \"Tags\" :widget :checkbox-list)
                :validations (:join-items-exist)
                :source (:view :main :table :tags :column :name :agg :distinct)
                :source-all (:view :tags :table :tags :column :name :agg :list)
                :join-table :todo-tags))
      :list-form (:fields t) :update-form (:fields t) :add-form (:fields t))

    :tags
    (:table t :create :auto :update :auto :delete :auto :display t
      :type-roles (\"todo-users\" \"public\")
      :fields
      (:name (:type :text :identity t :sortable t
               :ui (:label \"Tag\" :widget :textbox)
               :validations (:required)
               :source (:view :main :table :tags :column :name :agg :first)
               :column t :not-null t :unique t))
      :list-form (:fields t) :update-form (:fields t) :add-form (:fields t))

    :todo-tags
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :todos)
        :reference (:target :tags)))
     :users (:type-roles (\"logged-in\" \"public\" \"user-creator\"))
     :roles (:type-roles (\"logged-in\" \"public\" \"role-creator\"))
     :permissions (:type-roles (\"logged-in\" \"public\" \"permission-creator\"))))
```
"
      :page-description-open nil
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
          :ui (:label "Done" :widget :checkbox :filter-with :boolean)
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
      :view-form (:fields t)
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
      :view-form (:fields t)
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
