'(:title "To Do List"
  :name "todos"
  :version "0.1"
  :domain "todo.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  :landing-page :todos
  :types
  (:todos
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("todo-users")
      :default-sort (:points :desc)
      :views (:main (:tables (:todos :todo-tags :tags))
               :tags (:tables (:tags)))
      :fields
      (:name 
        (:type :text :identity t :sortable t
          :ui (:label "To Do" :widget :textbox)
          :validations (:required (:max-length :max 19))
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :points
        (:type :integer :default 0 :sortable t
          :ui (:label "Points" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :points :agg :first)
          :column t :not-null t)
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
      :type-roles ("todo-users")
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
        :reference (:target :tags)))))
