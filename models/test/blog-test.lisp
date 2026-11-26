;; Test fixture for the embedded child region vocabulary (plan 02)
;; and the shared blog surface 03/04 consume. Complete on landing:
;; guest surface, seeded rows (suite-side, never here), M2M tags,
;; author/reference chip filtering, and the permission recipe.
;; comments deliberately carry NO :identity field ("Nice post!"
;; exists more than once); :post-tags holds the storage-edge
;; :reference fields to comments and tags, the zero-identity
;; joiner carve-out's positive case.
;;
;; Panel widget parity ("Child region widget parity"): comments'
;; add-form carries a static :select (:kind), a :checkbox
;; (:featured), and the tags :checkbox-list; the second region
;; (on :tags) holds :attachments, a real tree leaf whose file +
;; file-path pair exercises the panel's two-phase upload. Its
;; uploadable shape is load-bearing — the directory type must be
;; named :directories with the path field :name (add-root-fs-resources
;; seeds "/" by exactly those names), and :type :file alone emits
;; no form field (the :widget :file is what fe-fields needs).
'(:title "Blog Test"
  :name "blog-test"
  :version "0.1"
  :domain "blog-test.demo.data-ui.com"
  :repl nil
  :guest-allowed t
  :guest-auto nil
  :api-roles ("logged-in" "public")
  :landing-page :posts
  :new-roles (:comments-user ("create" "read"))
  :types
  (:posts
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("public")
      :views (:main (:tables (:posts)))
      :fields
      (:title
        (:type :text :identity t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t :unique t :sortable t)
        :body
        (:type :text
          :ui (:label "Body" :widget :markdown)
          :validations (:required)
          :source (:view :main :column :body :agg :first)
          :column t :not-null t)
        :created-at
        (:ui (:label "Posted" :widget :textbox :read-only t)))
      :list-form (:fields (:title :created-at))
      :view-form (:fields (:title :body)
                  :hide-list t
                  :children ((:type :comments
                              :via :post
                              :sort (:created-at :asc))))
      :update-form (:fields (:title :body))
      :add-form (:fields (:title :body)))

   :tags
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("public")
      :views (:main (:tables (:tags)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Tag" :widget :textbox)
          :validations (:required)
          :source (:view :main :table :tags :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields (:name))
      :view-form (:fields (:name)
                  :hide-list t
                  :children ((:type :attachments
                              :via :tag)))
      :update-form (:fields (:name))
      :add-form (:fields (:name)))

   :comments
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("public" "comments-user")
      :views
      (:main (:tables (:comments :posts :users :post-tags :tags))
        :posts (:tables (:posts))
        :users (:tables (:users))
        :tags (:tables (:tags)))
      :fields
      (:user
        (:type :text :target :users
          :ui (:label "Author" :widget :textbox :read-only t
            :filter-with (:kind :reference))
          :source (:view :main :table :users :column :name :agg :first)
          :source-all (:view :users :table :users :column :name
                       :agg :list)
          :column t :not-null t
          :autofill :user)
        :body
        (:type :text
          :ui (:label "Body" :widget :markdown)
          :validations (:required)
          :searchable t
          :source (:view :main :column :body :agg :first)
          :column t :not-null t)
        :post
        (:type :text :target :posts
          :ui (:label "Post" :widget :search-select
            :filter-with (:kind :reference))
          :source (:view :main :table :posts :column :title
                   :agg :first)
          :source-all (:view :posts :table :posts :column :title
                       :agg :list)
          :column t :not-null t)
        :kind
        (:type :text
          :ui (:label "Kind" :widget :select
            :options ("note" "question" "review"))
          :source (:view :main :column :kind :agg :first)
          :column t)
        :featured
        (:type :boolean :default :false
          :ui (:label "Featured" :widget :checkbox)
          :source (:view :main :column :featured :agg :first)
          :column t :not-null t)
        :tags
        (:type :list
          :ui (:label "Tags" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :tags :column :name :agg :distinct)
          :source-all (:view :tags :table :tags :column :name :agg :list)
          :join-table :post-tags)
        :created-at
        (:ui (:label "Posted" :widget :textbox :read-only t)))
      :list-form (:fields (:post :user :body :created-at :tags)
                  :default-sort (:created-at :desc))
      :view-form (:fields (:user :body :post))
      :update-form (:fields (:user :body :post :tags))
      :add-form (:fields (:user :body :post :kind :featured :tags)))

   :directories
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :tree t :is-leaf nil :parent-type :directories :fs-backed t
      :type-roles ("public")
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
      :view-form (:fields t)
      :add-form (:fields t))

   :attachments
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :tree t :is-leaf t :parent-type :directories :fs-backed t
      :type-roles ("public")
      :views (:main (:tables (:attachments :tags)))
      :fields
      (:name
        (:type :text :identity t :path t
          :ui (:label "File" :widget :file-path
            :directory-type :directories)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :file
        (:type :file
          :ui (:label "Select File" :widget :file)
          :validations (:required))
        :tag
        (:type :text :target :tags
          :ui (:label "Tag" :widget :search-select
            :filter-with (:kind :reference))
          :source (:view :main :table :tags :column :name :agg :first)
          :source-all (:view :main :table :tags :column :name
                       :agg :list)
          :column t :not-null t))
      :list-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t)
      :update-form (:fields t))

   :post-tags
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :comments)
        :reference (:target :tags)))))
