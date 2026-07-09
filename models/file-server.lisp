'(:title "File Server"
  :name "files"
  :version "0.1"
  :domain "files.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  :types
  (:directories
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :tree t :is-leaf nil
      ;; TODO: Ensure that parent-type is present in types with :tree t
      :parent-type :directories
      :fs-backed t
      :type-roles ("file-users")
      :views (:main (:tables (:directories)))
      :fields
      (:name 
        (:type :text :identity t :path t
          :ui (:label "Directory" :input-type :line)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :bogus
        (:type :text :default ""
          :ui (:label "Bogus" :input-type :line)
          :source (:view :main :column :name :agg :first)
          :column t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))
    :files
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :tree t :is-leaf t :parent-type :directories :fs-backed t
      :type-roles ("file-users")
      :views (:main (:tables (:files)))
      :fields
      (:name
        (:type :text :identity t :path t
          :ui (:label "File" :input-type :line)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :file 
        (:type :file
          :ui (:label "Select File" :input-type :file)
          :validations (:required)))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))))
