'(:title "Markdown Test"
  :name "markdown-test"
  :version "0.1"
  :domain "markdown-test.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  :landing-page :posts
  :types
  (:posts
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("md-users")
      :views (:main (:tables (:posts)))
      :fields
      (:title
        (:type :text :identity t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t)
       :body
       (:type :text :column t
         :ui (:label "Body" :widget :markdown :hide-label t)
         :source (:view :main :column :body :agg :first)))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))))
