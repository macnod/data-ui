;; Test fixture: a type with two M2M joiners.
;; Used by tests/m2m-tests.lisp to verify per-joiner SQL isolation
;; and backend join DML with field keys that differ from target type keys.
'(:title "M2M Test"
  :name "m2m-test"
  :version "0.1"
  :domain "m2m-test.demo.data-ui.com"
  :repl nil
  :types
  (:items
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("m2m-users")
      :views (:main (:tables (:items :item-tags :tags
                          :item-users :users)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Item" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :tags
        (:type :list
          :ui (:label "Tags" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :tags :column :name :agg :distinct)
          :source-all (:view :tags :table :tags :column :name :agg :list)
          :join-table :item-tags)
        :assignees
        (:type :list
          :ui (:label "Assignees" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :users :column :name :agg :distinct)
          :source-all (:view :users :table :users :column :name :agg :list)
          :join-table :item-users))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :tags
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("m2m-users")
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
