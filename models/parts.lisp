(:title "Parts Tracker"
  :name "parts"
  :version "0.1"
  :domain "parts.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  :types
  (:directories
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :tree t :is-leaf nil :parent-type :directories :fs-backed t
      :type-roles ("parts")
      :views (:main (:tables (:directories)))
      :fields
      (:name
        (:type :text :path t
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
      :type-roles ("parts")
      :views (:main (:tables (:files)))
      :fields
      (:name
        (:type :text :path t
          :ui (:label "File" :input-type :line)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :file 
        ;; TODO: For :type :file, the validation :valid-file should exist
        ;;       The validation should allow for NIL, but should otherwise
        ;;       check that the file path is correct, that the directory
        ;;       exists, and that the file doesn't already exist.
        (:type :file
          :ui (:label "Select File" :input-type :file)
          :validations (:required)))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))
    
    :parts
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("parts")
      :views (:main (:tables (:parts :part-files :files))
               :files (:tables (:files)))
      :fields 
      (:name 
        (:type :text
          :ui (:label "Part Number" :input-type :line)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :description
        (:type :text
          :ui (:label "Part Description" :input-type :text)
          :source (:view :main :column :description :agg :first)
          :column t :not-null nil :unique nil)
        ;; TODO: This field should be able to have a different
        ;; name.
        :files
        (:type :list
          :ui (:label "Images" :input-type :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :files :column :name :agg :list)
          :source-all (:view :files :table :files :column :name :agg :list)
          :join-table :part-files))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :part-files
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :parts)
        :reference (:target :files)))))
