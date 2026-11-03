;; Test fixture for the :fk-eq filter operator (storage-column
;; compare on FK fields). Tiny posts/comments pair with NO
;; :children declaration (that is 02's), plus the predicate-arm and
;; rejection shapes: a boolean-declared FK (:flag pointing at
;; :flags, the record-permalink :flag-editor hazard shape pinning
;; the string-shaped filter-value-type-p arm on a non-text FK), and
;; an M2M tags field for the :join-table rejection. :comments
;; :title carries the complete reference-chip identity spelling so
;; today's exactly-one-identity valid-target compiles the joiner
;; (the zero-identity relaxation belongs to 02). Every seeded
;; comment gets a distinct nonblank :title (the identity's unique
;; index requires it), and every uuid-looking FK is populated with
;; be-set-field-value, never through be-insert data (the
;; resolve-reference-id uuid short-circuit would attach the wrong
;; row - the title-collision probe's whole point).
'(:title "fk-eq Test"
  :name "fk-eq-test"
  :version "0.1"
  :domain "fk-eq-test.demo.data-ui.com"
  :repl nil
  :types
  (:posts
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:posts)))
      :fields
      (:title
        (:type :text :identity t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

   :flags
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:flags)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Name" :widget :textbox)
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
      :type-roles ("logged-in")
      :views (:main (:tables (:tags)))
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

   :comments
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views
      (:main (:tables (:comments :posts :flags :comment-tags :tags)))
      :fields
      (:title
        (:type :text :identity t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t :unique t)
        :body
        (:type :text
          :ui (:label "Body" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :body :agg :first)
          :column t :not-null t)
        :post
        (:type :text :target :posts
          :ui (:label "Post" :widget :search-select)
          :source (:view :main :table :posts :column :title
                   :agg :first)
          :column t)
        :crosspost
        (:type :text :target :posts
          :ui (:label "Crosspost" :widget :search-select)
          :source (:view :main :table :posts :column :title
                   :agg :first)
          :column t)
        :flag
        (:type :boolean :target :flags
          :ui (:label "Flag" :widget :checkbox)
          :source (:view :main :table :flags :column :name
                   :agg :first)
          :column t)
        :tags
        (:type :list
          :ui (:label "Tags" :widget :checkbox-list)
          :source (:view :main :table :tags :column :name
                   :agg :distinct)
          :join-table :comment-tags))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

   :comment-tags
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :comments)
        :reference (:target :tags)))))
