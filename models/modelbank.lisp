'(:title "Model Bank"
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

     :models
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :type-roles ("models-user")
       :views (:main (:tables (:models :images)))
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
         :model
         (:type :text
           :ui (:label "Model Code" :input-type :textbox :render-as :code)
           :source (:view :main :column :model :agg :first)
           :column t :not-null t :unique nil)
         :images
         (:type :list
           :ui (:label "Images" :input-type :read-only :render-as :image-list)
           :source (:view :main :table :images :column :name :agg :list)))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :images
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :tree t :is-leaf t :parent-type :directories :fs-backed t
       :type-roles ("images-user")
       :views (:main (:tables (:images :users :models) :scope :user)
                :users (:tables (:users) :scope :user)
                :models (:tables (:models) :scope :user))
       :fields
       (:name
         (:type :text :path t
           :ui (:label "File" :input-type :line :render-as :image)
           :validations (:required)
           :source (:view :main :column :name :agg :first)
           :column t :not-null t :unique t)
         :file
         ;; TODO: For :type :file, the validation :valid-file should exist
         ;;       The validation should allow for NIL, but should otherwise
         ;;       check that the file path is correct, that the directory
         ;;       exists, and that the file does not already exist.
         (:type :file
           :ui (:label "Select File" :input-type :file)
           :validations (:required))
         :user 
         (:type :text
           :force-sql-name "image_user"
           :ui (:label "Owner" :input-type :read-only)
           :target :users
           :source (:view :main :table :users :column :name :agg :first)
           :source-all (:view :users :table :users :column :name :agg :list)
           :column t :not-null t)
         :model
         (:type :text
           :force-sql-name "image_model"
           :ui (:label "Model" :input-type :read-only)
           :target :models
           :source (:view :main :table :models :column :name :agg :first)
           :source-all (:view :models :table :models :column :name :agg :list)
           :column t :not-null t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))))


