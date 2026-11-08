;; Test fixture for the bot-crawlable public pages (todo child 1,
;; consumed by children 2 and 3). Built from blog-test.lisp (copied,
;; that file untouched): :guest-allowed t, M2M tags through this
;; fixture's own :post-tags joiner (blog-test's joins comments to
;; tags — it cannot be reused), a cover image widget sourced from
;; the fs-backed :images type, real password / hidden / file / button
;; fields named by the explicit :view-form list so public-view-field-
;; keys must drop them itself, a :moderate button whose :test-async
;; hook (tests/action-tests.lisp, loaded before the fixture compiles)
;; never completes, and the eligibility negative cases (:notes,
;; :private, :mine, :links, :folders).
'(:title "Public Pages Test"
  :name "public-pages-test"
  :version "0.1"
  :domain "pp-test.demo.data-ui.com"
  :repl nil
  :guest-allowed t
  :guest-auto nil
  :api-roles ("logged-in" "public")
  :landing-page :posts
  :new-roles (:comments-user ("create" "read"))
  :types
  (:directories
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

   :images
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :tree t :is-leaf t :parent-type :directories :fs-backed t
      :type-roles ("public")
      :views (:main (:tables (:images :users))
               :users (:tables (:users)))
      :fields
      (:name
        (:type :text :identity t :path t
          :ui (:label "File" :widget :image)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :file
        (:type :file
          :ui (:label "Select File" :widget :file)
          :validations (:required))
        :user
        (:type :text
          :autofill :user
          :force-sql-name "image_user"
          :ui (:label "Owner" :widget :textbox :read-only t)
          :target :users
          :source (:view :main :table :users :column :name :agg :first)
          :source-all (:view :users :table :users :column :name
                       :agg :list)
          :column t :not-null t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

   :posts
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("public")
      :page-description "Everything the **public pages** renderer test pins."
      :views (:main (:tables (:posts :users :post-tags :tags :images)))
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
        :cover
        (:type :text :target :images
          :ui (:label "Cover" :widget :image)
          :source (:view :main :table :images :column :name :agg :first)
          :source-all (:view :images :table :images :column :name
                       :agg :list)
          :column t :not-null nil)
        :featured
        (:type :boolean
          :ui (:label "Featured" :widget :checkbox)
          :source (:view :main :column :featured :agg :first)
          :column t)
        :rating
        (:type :integer
          :ui (:label "Rating" :widget :stars :precision 1)
          :source (:view :main :column :rating :agg :first)
          :column t)
        :tags
        (:type :list
          :ui (:label "Tags" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :tags :column :name :agg :distinct)
          :source-all (:view :tags :table :tags :column :name :agg :list)
          :join-table :post-tags)
        :author
        (:type :text :target :users
          :ui (:label "Author" :widget :textbox :read-only t)
          :source (:view :main :table :users :column :name :agg :first)
          :source-all (:view :users :table :users :column :name
                       :agg :list)
          :column t :not-null nil
          :autofill :user)
        :api-key
        (:type :password
          :ui (:label "API Key" :widget :password-new)
          :source (:view :main :column :api-key :agg :first)
          :column t)
        :internal-note
        (:type :text
          :ui (:label "Internal note" :widget :hidden)
          :source (:view :main :column :internal-note :agg :first)
          :column t)
        :attachment
        (:type :file
          :ui (:label "Attachment" :widget :file))
        :publish
        (:type :button
          :ui (:label "Publish" :widget :button)
          :action (:test-sync)))
      :list-form (:fields (:title :created-at))
      :view-form (:fields (:title :body :cover :featured :rating
                          :tags :author :api-key :internal-note
                          :attachment :publish)
                  :hide-list t
                  :children ((:type :comments :via :post)))
      ;; (:fields t) on purpose: the timestamp comparison test reads
      ;; :created-at / :updated-at through this form as admin.
      :update-form (:fields t)
      :add-form (:fields (:title :body :cover :featured :rating
                         :tags :author)))

   :comments
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("public" "comments-user")
      :views
      (:main (:tables (:comments :posts :users)))
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
        :secret
        (:type :text
          :ui (:label "Secret" :widget :textbox)
          :source (:view :main :column :secret :agg :first)
          :column t)
        :note
        (:type :text
          :ui (:label "Note" :widget :textbox)
          :source (:view :main :column :note :agg :first)
          :column t)
        :moderate
        (:type :button
          :ui (:label "Moderate" :widget :button)
          :action (:test-async))
        ;; :moderate-status is synthesized by synthesize-status-fields
        ;; (read-only :textbox); the View form names it so the SPA's
        ;; status poll sees it.
        :created-at
        (:ui (:label "Posted" :widget :textbox :read-only t)))
      :list-form (:fields (:post :user :body :created-at)
                  :default-sort (:created-at :desc))
      :view-form (:fields (:user :body :post :note :moderate-status))
      :update-form (:fields (:user :body :post :note))
      :add-form (:fields (:user :body :post :note)))

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
      :view-form (:fields (:name))
      :update-form (:fields (:name))
      :add-form (:fields (:name)))

   :post-tags
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :posts)
        :reference (:target :tags)))

   ;; 02's non-fs-leaf 404 case: a tree leaf that is not fs-backed
   ;; has no path field and no file route.
   :folders
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :tree t :is-leaf t :parent-type :directories
      :type-roles ("public")
      :views (:main (:tables (:folders)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Folder" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t))
      :list-form (:fields (:name))
      :update-form (:fields (:name))
      :add-form (:fields (:name)))

   ;; Guest-readable, no :view-form: no pages.
   :notes
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("public")
      :views (:main (:tables (:notes)))
      :fields
      (:note
        (:type :text
          :ui (:label "Note" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :note :agg :first)
          :column t :not-null t))
      :list-form (:fields (:note))
      :update-form (:fields (:note))
      :add-form (:fields (:note)))

   ;; :type-roles without "public": nothing publishes even though the
   ;; row itself carries the role.
   :private
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("private-user")
      :views (:main (:tables (:private)))
      :fields
      (:note
        (:type :text :identity t
          :ui (:label "Note" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :note :agg :first)
          :column t :not-null t))
      :list-form (:fields (:note))
      :view-form (:fields (:note))
      :update-form (:fields (:note))
      :add-form (:fields (:note)))

   ;; Guest-readable with a :view-form but view-scoped: the scoped
   ;; exclusion case (be-list applies :scope :user, be-rec does not).
   :mine
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("public")
      :views (:main (:tables (:mine) :scope :user))
      :fields
      (:note
        (:type :text
          :ui (:label "Note" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :note :agg :first)
          :column t :not-null t))
      :list-form (:fields (:note))
      :view-form (:fields (:note))
      :update-form (:fields (:note))
      :add-form (:fields (:note)))

   ;; Two singular targets to :posts on one type: suppression keeps
   ;; them text, and the type itself is eligible so its render test is
   ;; not vacuous.
   :links
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("public")
      :views (:main (:tables (:links :posts)))
      :fields
      (:first
        (:type :text :target :posts
          :ui (:label "First" :widget :search-select
            :filter-with (:kind :reference))
          :source (:view :main :table :posts :column :title :agg :first)
          :source-all (:view :posts :table :posts :column :title
                       :agg :list)
          :column t :not-null nil)
        :second
        (:type :text :target :posts
          :ui (:label "Second" :widget :search-select
            :filter-with (:kind :reference))
          :source (:view :main :table :posts :column :title :agg :first)
          :source-all (:view :posts :table :posts :column :title
                       :agg :list)
          :column t :not-null nil))
      :list-form (:fields (:first :second))
      :view-form (:fields (:first :second))
      :update-form (:fields (:first :second))
      :add-form (:fields (:first :second)))))
