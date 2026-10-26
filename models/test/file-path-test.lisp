'(:title "File Path Test"
  :name "file-path-test"
  :version "0.1"
  :domain "file-path-test.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  :landing-page :files
  :types
  (:directories
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :tree t :is-leaf nil :parent-type :directories :fs-backed t
      :type-roles ("fp-users")
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
    :files
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :tree t :is-leaf t :parent-type :directories :fs-backed t
      :type-roles ("fp-users")
      :views (:main (:tables (:files)))
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
          :validations (:required)))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))))
