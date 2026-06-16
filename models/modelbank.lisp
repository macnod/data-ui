(:title "Model Bank"
  :name "modelbank"
  :version "0.1"
  :domain "modelbank.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  :types
  (:directories
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :tree t :is-leaf nil :parent-type :directories :fs-backed t
      :type-roles ("directories-user")
      :views (:main (:tables (:directories)))
      :fields
      (:name
        (:type :text :path t
          :ui (:label "Directory" :input-type :line)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :images
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :tree t :is-leaf t :parent-type :directories :fs-backed t
      :type-roles ("images-user")
      :views (:main (:tables (:images)))
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
    
    :models
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("models-user")
      :views (:main (:tables (:models :model-images :images))
               :images (:tables (:images))
               :ratings (:tables (:model-ratings)))
      :fields 
      (:name 
        (:type :text
          :ui (:label "Model Name" :input-type :line)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :description
        (:type :text
          :ui (:label "Model Description" :input-type :textbox)
          :source (:view :main :column :description :agg :first)
          :column t :not-null nil :unique nil)
        :my-rating
        (:type :integer
          :ui (:label "My Rating" :input-type :line)
          :source (:views :ratings :table :model-ratings :column :rating :agg :first))
        :average-rating
        (:type :real
          :ui (:label "Average Rating" :input-type :read-only)
          :source (:views :ratings :table :model-ratings :column :rating :agg :average))
        :images
        (:type :list
          :ui (:label "Images" :input-type :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :images :column :name :agg :list)
          :source-all (:view :images :table :files :column :name :agg :list)
          :join-table :model-images))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :model-ratings
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("model-ratings-user")
      :views (:main (:tables (:model-ratings :models :users))
               :models (:tables (:models))
               :users (:tables (:users)))
      :fields
      (:rating
        (:type :integer :default 0
          :ui (:label "Model Rating" :input-type :line)
          :source (:view :main :column :rating :agg :first)
          :column t :not-null t)
        :reference (:target :models)
        :reference (:target :users)))

    :model-images
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :models)
        :reference (:target :images)))))
