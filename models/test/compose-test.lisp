;; Test fixture: compose-string lifecycle hook.
;; Authors have structured name parts (first/middle/last) that are
;; composed into a single :name identity field via :compose-string.
;; Books reference authors via M2M checkbox-list sourcing :name.
'(:title "Compose String Test"
   :name "compose-test"
   :version "0.1"
   :domain "compose-test.demo.data-ui.com"
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
       :pre-create ((:compose-string
                      :format ":first-name :middle-name :last-name"
                      :into :name))
       :pre-update ((:compose-string
                      :format ":first-name :middle-name :last-name"
                      :into :name))
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
           :source (:view :main :table :authors :column :name :agg :list)
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
