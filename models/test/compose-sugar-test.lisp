;; Test fixture: field-level :compose sugar.
;; Same as compose-test.lisp but uses :compose on the field instead
;; of type-level :pre-create/:pre-update hooks.
'(:title "Compose Sugar Test"
   :name "compose-sugar-test"
   :version "0.1"
   :domain "compose-sugar-test.demo.data-ui.com"
   :repl nil
   :types
   (:authors
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :type-roles ("compose-users")
       :views (:main (:tables (:authors)))
       :fields
       (:name
         (:type :text :identity t
           :compose ":first-name :middle-name :last-name"
           :ui (:label "Name" :widget :textbox :read-only t)
           :validations (:required)
           :source (:view :main :column :name :agg :first)
           :column t :not-null t :unique t)
         :first-name
         (:type :text
           :ui (:label "First Name" :widget :textbox)
           :validations (:required)
           :source (:view :main :column :first-name :agg :first)
           :column t :not-null t)
         :middle-name
         (:type :text
           :ui (:label "Middle Name" :widget :textbox)
           :source (:view :main :column :middle-name :agg :first)
           :column t)
         :last-name
         (:type :text
           :ui (:label "Last Name" :widget :textbox)
           :validations (:required)
           :source (:view :main :column :last-name :agg :first)
           :column t :not-null t))
       :list-form (:fields (:name :first-name :middle-name :last-name))
       :add-form (:fields (:first-name :middle-name :last-name))
       :update-form (:fields (:name :first-name :middle-name :last-name)))

     :books
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :type-roles ("compose-users")
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
       :list-form (:fields (:title :authors))
       :update-form (:fields (:title :authors))
       :add-form (:fields (:title :authors)))

     :book-authors
     (:table t :is-joiner t :internal t
       :fields
       (:reference (:target :books)
         :reference (:target :authors)))))
