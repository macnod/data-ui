;; Test fixture for the :spawn action hook (template→instance
;; completion).  One instance type: :name is plain non-unique text
;; (an :identity t name would collide on the second spawn), :tags
;; exercises the M2M copy through the join table, :notes exercises
;; :clear, and :completed / :completed-at / :completed-by are the
;; close targets.  Used by tests/spawn-tests.lisp.
'(:title "Spawn Test"
  :name "spawn-test"
  :version "0.1"
  :domain "spawn.test.data-ui.com"
  :repl nil
  :types
  (:items
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("item-users")
      :views (:main (:tables (:items :item-tags :tags
                          :item-users :users))
               :tags (:tables (:tags))
               :users (:tables (:users)))
      :fields
      (:name
        (:type :text
          :ui (:label "Item" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t)
        :slug
        (:type :uuid :identity t :default :generate-uuid
          :ui (:label "Slug" :widget :textbox :read-only t)
          :source (:view :main :column :slug :agg :first)
          :column t :not-null t)
        :points
        (:type :integer :default 1
          :ui (:label "Points" :widget :textbox)
          :validations (:required (:in-range :min 1 :max 4))
          :source (:view :main :column :points :agg :first)
          :column t :not-null t)
        :tags
        (:type :list
          :ui (:label "Tags" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :tags :column :name :agg :distinct)
          :source-all (:view :tags :table :tags :column :name :agg :list)
          :join-table :item-tags)
        :notes
        (:type :text :default ""
          :ui (:label "Notes" :widget :textarea)
          :source (:view :main :column :notes :agg :first)
          :column t)
        :completed
        (:type :boolean :default :false
          :ui (:label "Done" :widget :checkbox)
          :source (:view :main :column :completed :agg :first)
          :column t :not-null t)
        :completed-at
        (:type :timestamp
          :ui (:label "Completed At" :widget :textbox)
          :source (:view :main :column :completed-at :agg :first)
          :column t)
        :completed-by
        (:type :list
          :ui (:label "Completed By" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :users :column :name :agg :distinct)
          :source-all (:view :users :table :users :column :name :agg :list)
          :join-table :item-users)
        :complete
        (:type :button
          :ui (:label "Complete" :widget :button)
          :action (:spawn
                    :close (:completed :true
                            :completed-at :now
                            :completed-by :user)
                    :clear (:notes :slug))))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :tags
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("item-users")
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Tag" :widget :textbox)
          :validations (:required)
          :source (:view :main :table :tags :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :item-tags
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :items)
        :reference (:target :tags)))

    :item-users
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :items)
        :reference (:target :users)))))
