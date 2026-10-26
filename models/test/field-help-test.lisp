'(:title "Field Help Test"
  :name "field-help-test"
  :version "0.1"
  :domain "field-help-test.demo.data-ui.com"
  :repl t
  :landing-page :posts
  :types
  (:posts
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("fh-users")
      :views (:main (:tables (:posts)))
      :fields
      (:title
        (:type :text :identity t
          :ui (:label "Title"
            :help "The **primary** identifier.

- Use `kebab-case`
- Keep it short")
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t)
       :body
       (:type :text :column t
         :ui (:label "Body" :widget :markdown)
         :source (:view :main :column :body :agg :first)))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))))
