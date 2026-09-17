'(:title "Books & Authors"
  :name "books"
  :version "0.1"
  :domain "books.demo.data-ui.com"
  :domain-stg "books-stg.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  :landing-page :books
  :types
  (:books
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("book-users")
      :views (:main (:tables (:books :book-authors :authors :ratings))
               :authors (:tables (:authors)))
      :fields
      (:title
        (:type :text :identity t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t :unique t)
        :isbn
        (:type :text
          :ui (:label "ISBN" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :isbn :agg :first)
          :column t :not-null t :unique t)
        :description
        (:type :text
          :ui (:label "Description" :widget :textarea)
          :source (:view :main :column :description :agg :first)
          :column t)
        :rating
        (:type :integer
          :ui (:label "My Rating" :widget :stars)
          :validations ((:in-range :min 1 :max 5))
          :source (:view :main :table :ratings :column :rating
                   :scope :user :agg :first)
          :write-to (:table :ratings
                      :book :this
                      :user :user
                      :rating :value))
        :average-rating
        (:type :real
          :ui (:label "Rating" :widget :stars :read-only t :precision 1)
          :source (:view :main :table :ratings :column :rating :agg :avg))
        :authors
        (:type :list
          :ui (:label "Authors" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :authors :column :name :agg :distinct)
          :source-all (:view :authors :table :authors :column :name :agg :list)
          :join-table :book-authors))
      :list-form (:fields (:title :isbn :description :average-rating :authors))
      :update-form (:fields t)
      :add-form (:fields (:title :isbn :description :rating :authors)))

    :authors
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("book-users")
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Name" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :ratings
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("ratings-user")
      :views (:main (:tables (:ratings :books :users) :scope :user)
               :books (:tables (:books))
               :users (:tables (:users) :scope :user))
      :fields
      (:book
        (:type :text :identity t
          :force-sql-name "rating_book"
          :ui (:label "Book" :widget :select)
          :target :books
          :source (:view :main :table :books :column :title :agg :first)
          :source-all (:view :books :table :books :column :title :agg :list)
          :column t :not-null t)
        :user
        (:type :text :identity t
          :autofill :user
          :force-sql-name "rating_user"
          :ui (:label "User" :widget :textbox :read-only t)
          :target :users
          :source (:view :main :table :users :column :name :agg :first)
          :source-all (:view :users :table :users :column :name :agg :list)
          :column t :not-null t)
        :rating
        (:type :integer
          :ui (:label "Rating" :widget :stars)
          :validations ((:in-range :min 1 :max 5))
          :source (:view :main :table :ratings :column :rating :agg :first)
          :column t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :book-authors
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :books)
        :reference (:target :authors)))))
