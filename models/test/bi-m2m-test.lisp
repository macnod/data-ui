;; Test fixture: bidirectional M2M (books <-> authors via one joiner).
;; Used by tests/bi-m2m-tests.lisp to verify that be-list on both
;; ends of a bidirectional M2M does not stack-overflow, and that
;; allowed-values are correct on both sides.
'(:title "Bidirectional M2M Test"
  :name "bi-m2m-test"
  :version "0.1"
  :domain "bi-m2m-test.demo.data-ui.com"
  :repl nil
  :types
  (:books
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("bi-m2m-users")
      :views (:main (:tables (:books :book-authors :authors))
               :authors (:tables (:authors)))
      :fields
      (:title
        (:type :text :identity t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t :unique t)
        :authors
        (:type :list
          :ui (:label "Authors" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :authors :column :name :agg :distinct)
          :source-all (:view :authors :table :authors
                       :column :name :agg :list)
          :join-table :book-authors))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :authors
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("bi-m2m-users")
      :views (:main (:tables (:authors :book-authors :books))
               :books (:tables (:books)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Name" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :books
        (:type :list
          :ui (:label "Books" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :books :column :title :agg :distinct)
          :source-all (:view :books :table :books
                       :column :title :agg :list)
          :join-table :book-authors))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :book-authors
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :books)
        :reference (:target :authors)))))
