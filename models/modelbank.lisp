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
         (:type :text :identity t :path t
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
       :views (:main (:tables (:models :images :ratings)))
       :fields
       (:name
         (:type :text :identity t
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
           :source (:view :main :table :images :column :name :agg :list))
         :rating
         (:type :integer
           :ui (:label "Rating" :input-type :line :render-as :stars)
           :validations ((:in-range :min 1 :max 5))
           :source (:view :main :table :ratings :column :rating
                    :scope :user :agg :first)
           :write-to (:table :ratings
                       :model :this
                       :user :user
                       :rating :value))
         :average-rating
         (:type :real
           :ui (:label "Average Rating" :input-type :read-only
                :render-as :stars :precision 1)
           :source (:view :main :table :ratings :column :rating
                    :agg :avg)))
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
         (:type :text :identity t :path t
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
           :autofill :user
           :force-sql-name "image_user"
           :ui (:label "Owner" :input-type :read-only)
           :target :users
           :source (:view :main :table :users :column :name :agg :first)
           :source-all (:view :users :table :users :column :name :agg :list)
           :column t :not-null t)
         :model
         (:type :text
           :force-sql-name "image_model"
           :ui (:label "Model" :input-type :select)
           :target :models
           :source (:view :main :table :models :column :name :agg :first)
           :source-all (:view :models :table :models :column :name :agg :list)
           :column t :not-null t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :ratings
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :type-roles ("ratings-user")
       :views (:main (:tables (:ratings :models :users) :scope :user)
                :models (:tables (:models))
                :users (:tables (:users) :scope :user))
       :fields
       (:model
         (:type :text :identity t
           :force-sql-name "rating_model"
           :ui (:label "Model" :input-type :select)
           :target :models
           :source (:view :main :table :models :column :name :agg :first)
           :source-all (:view :models :table :models :column :name :agg :list)
           :column t :not-null t)
         :user
         (:type :text :identity t
           :autofill :user
           :force-sql-name "rating_user"
           :ui (:label "User" :input-type :read-only)
           :target :users
           :source (:view :main :table :users :column :name :agg :first)
           :source-all (:view :users :table :users :column :name :agg :list)
           :column t :not-null t)
         :rating
         (:type :integer
           :ui (:label "Rating" :input-type :line :render-as :stars)
           :validations ((:in-range :min 1 :max 5))
           :source (:view :main :table :ratings :column :rating :agg :first)
           :column t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))))


