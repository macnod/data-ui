;; Test fixture: joined-column sort (:sortable t on :target fields).
;; Used by tests/target-sort-tests.lisp. :items carries two sortable
;; singular targets — :owner (built-in :users, nullable; the Model
;; Bank Owner shape) and :category (a custom type; the Mel's App
;; shape) — plus an M2M :tags chain whose fan-out the DISTINCT /
;; COUNT(DISTINCT) pins need. Targets are declared before :items —
;; unneeded since "Sort DDL" (create-tables dependency-orders the
;; walk via type-ddl-order) but kept as the probed shape.
'(:title "Target Sort Test"
  :name "target-sort-test"
  :version "0.1"
  :domain "target-sort-test.demo.data-ui.com"
  :repl nil
  :types
  (:categories
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("target-sort-users")
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Category" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

    :tags
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("target-sort-users")
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Tag" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

    :items
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("target-sort-users")
      :default-sort (:owner :asc)
      :views (:main (:tables (:items :users :categories
                              :item-tags :tags))
               :users (:tables (:users))
               :categories (:tables (:categories))
               :tags (:tables (:tags)))
      :fields
      (:name
        (:type :text :identity t :sortable t :searchable t
          :ui (:label "Item" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :owner
        (:type :text :target :users :sortable t
          :ui (:label "Owner" :widget :search-select)
          :source (:view :main :table :users :column :name
                   :agg :first)
          :source-all (:view :users :table :users :column :name
                       :agg :list)
          :column t)
        :category
        (:type :text :target :categories :sortable t
          :ui (:label "Category" :widget :search-select)
          :source (:view :main :table :categories :column :name
                   :agg :first)
          :source-all (:view :categories :table :categories
                      :column :name :agg :list)
          :column t)
        :tags
        (:type :list
          :ui (:label "Tags" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :tags :column :name
                   :agg :distinct)
          :source-all (:view :tags :table :tags :column :name
                       :agg :list)
          :join-table :item-tags))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

    :item-tags
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :items)
        :reference (:target :tags)))))
