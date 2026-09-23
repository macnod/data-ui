'(:title "Books & Authors"
  :name "books"
  :version "0.1"
  :domain "books.demo.data-ui.com"
  :domain-stg "books-stg.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  ;; Petting-zoo guest surface (D1): passwordless guest login, guest
  ;; reaches the app-level endpoints via "public" in :api-roles, and
  ;; the built-in type-roles overlays below open :users / :roles /
  ;; :permissions to guest reads; "public" on every user-defined type
  ;; opens the app data itself (row visibility stays per-record).
  :guest-allowed t
  :guest-auto nil
  :api-roles ("logged-in" "public")
  :landing-page :books
  :types
  (:books
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("books-user" "public")
      :views (:main (:tables (:books :book-authors :authors :book-genres :genres :my-ratings :covers))
               :authors (:tables (:authors))
               :genres (:tables (:genres)))
      :fields
      (:covers
        (:type :list
          :ui (:label "Cover" :widget :image-list)
          :source (:view :main :table :covers :column :name :agg :distinct))
        :title
        (:type :text :sortable t :searchable t :identity t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t :unique t)
        :genres
        (:type :list
          :ui (:label "Genre" :widget :checkbox-list)
          :source (:view :main :table :genres :column :name :agg :distinct)
          :source-all (:view :genres :table :genres :column :name :agg :list)
          :join-table :book-genres)
        :isbn
        (:type :text :searchable t :sortable t
          :ui (:label "ISBN" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :isbn :agg :first)
          :column t :not-null t :unique t)
        :description
        (:type :text :searchable t
          :ui (:label "Description" :widget :textarea)
          :source (:view :main :column :description :agg :first)
          :column t)
        :rating
        (:type :integer
          :ui (:label "My Rating" :widget :stars)
          :validations ((:in-range :min 1 :max 5))
          :source (:view :main :table :my-ratings :column :rating
                   :scope :user :agg :first)
          :write-to (:table :my-ratings
                      :book :this
                      :user :user
                      :rating :value))
        :average-rating
        (:type :real
          :ui (:label "Rating" :widget :stars :read-only t :precision 1)
          :source (:view :main :table :my-ratings :column :rating :agg :avg))
        :authors
        (:type :list
          :ui (:label "Authors" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :authors :column :name :agg :distinct)
          :source-all (:view :authors :table :authors :column :name :agg :list)
          :join-table :book-authors)
        :published
        (:type :timestamp :sortable t
          :ui (:label "Published")
          :source (:view :main :table :books :column :published :agg :first)
          :column t))
      :list-form (:fields (:covers :title :isbn :description :genres
                            :average-rating :authors :published))
      :update-form (:fields t)
      :add-form (:fields (:title :isbn :description :genres :rating :authors
                           :published)))

    :genres
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("books-user" "public")
      :views (:main (:tables (:genres)))
      :fields
      (:name
        (:type :text :identity t :searchable t :sortable t
          :ui (:label "Genre" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :book-genres
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :books)
        :reference (:target :genres)))

    :authors
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("books-user" "public")
      :views (:main (:tables (:authors :book-authors :books))
               :books (:tables (:books)))
      :fields
      (:name
        (:type :text :identity t :searchable t :sortable t
          :ui (:label "Name" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :books
        (:type :list
          :ui (:label "Books" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :books :column :title :agg :distinct)
          :source-all (:view :books :table :books :column :title :agg :list)
          :join-table :book-authors))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :my-ratings
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("books-user" "public")
      :views (:main (:tables (:my-ratings :books :users) :scope :user)
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
          :source (:view :main :table :my-ratings :column :rating :agg :first)
          :column t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :books-by-rating
    (:rollup t
      :grain :books
      :type-roles ("books-user" "public")
      :default-sort (:average-rating :desc)
      :views (:main (:tables (:books :my-ratings)))
      :list-form (:fields t)
      :fields
      (:title
        (:source (:view :main :table :books :column :title :agg :first)
          :sortable t
          :ui (:label "Title"))
        :average-rating
        (:type :real
          :source (:view :main :table :my-ratings :column :rating :agg :avg)
          :sortable t
          :ui (:label "Rating" :widget :stars :read-only t :precision 1))
        :ratings-count
        (:type :integer
          :source (:view :main :table :my-ratings :column :id :agg :count)
          :sortable t
          :ui (:label "Ratings"))))

    :directories
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :tree t :is-leaf nil :parent-type :directories :fs-backed t
      :type-roles ("directories-user" "public")
      :views (:main (:tables (:directories)))
      :fields
      (:name
        (:type :text :identity t :path t
          :ui (:label "Directory" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :covers
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :tree t :is-leaf t :parent-type :directories :fs-backed t
      :type-roles ("books-user" "public")
      :views (:main (:tables (:covers :books))
               :books (:tables (:books)))
      :fields
      (:image
        (:type :text
          :ui (:label "Image" :widget :image)
          :source (:view :main :column :name :agg :first)
          :column nil)
        :name
        (:type :text :identity t :path t :sortable t :searchable t
          :ui (:label "File" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :file
        (:type :file
          :ui (:label "Select File" :widget :file)
          :validations (:required))
        :book
        (:type :text
          :force-sql-name "cover_book"
          :ui (:label "Book" :widget :select)
          :target :books
          :source (:view :main :table :books :column :title :agg :first)
          :source-all (:view :books :table :books :column :title :agg :list)
          :column t :not-null t :unique nil))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :book-authors
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :books)
        :reference (:target :authors)))
     ;; Petting-zoo D1 overlays: guest reads the built-in account /
     ;; role / permission lists (tier identity stays visible; :settings
     ;; stays structurally unreachable — guest is never granted the
     ;; settings role). add-type-roles only inserts missing resources,
     ;; so these must be present before the profile's first set-model.
     :users (:type-roles ("logged-in" "public" "user-creator"))
     :roles (:type-roles ("logged-in" "public" "role-creator"))
     :permissions (:type-roles ("logged-in" "public" "permission-creator"))))
