'(:title "To Do List"
  :name "todo"
  :version "0.1"
  :domain "todo.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  ;; TODO: :repl is not taking effect yet
  :repl t
  :types   
  (:todos
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("todo-users")
      :views (:main (:tables (:todos :todo-tags :tags))
               :tags (:tables (:tags)))
      :fields
      (:name 
        (:type :text
          :ui (:label "To Do" :input-type :line)
          :validations (:required
                         (lambda (type-key field-key value user)
                           (declare (ignore user))
                           (unless (< (length value) 20)
                             (validation-error-string
                               type-key field-key value
                               "must be less than 20 characters."))))
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :points
        (:type :integer :default 0
          :ui (:label "Points" :input-type :line)
          :validations (:required)
          :source (:view :main :column :points :agg :first)
          :column t :not-null t)
        :done
        (:type :boolean :default :false
          :ui (:label "Done" :input-type :check-box)
          :source (:view :main :column :done :agg :first)
          :column t :not-null t)
        :tags
        (:type :list
          :ui (:label "Tags" :input-type :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :tags :column :name :agg :list)
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
        (:type :text
          ;; TODO: Add checks for :input-type value
          :ui (:label "Tag" :input-type :line)
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
