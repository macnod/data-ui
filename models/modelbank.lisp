;;
;; Model Bank
;;
;; Track, share, copy, rate, create, and deploy models to production.
;;
;; - Create: Enter the code for a model.
;;
;; - Generate: Enter a short, plain-English description of the model, and use
;;   the Generate button to have an AI generate the model code for you.
;;
;; - Deploy: Press the Deploy button to compile your model and put it into
;;   production.
;;
;; - Rate: Rate models and sort them by average rating.
;;
;; For Deploy, you must be a member of the deployer role (granted by
;; admin only). Being a member of models-user does not allow deploying.
;;
;; For Generate, you must be a member of the ai-user role.
;;
;; For Generate to work, you must provide a secret ("secrets" tab in the
;; Settings section) with the name llm-config and a value that looks like this:
;;     :url "https://api.z.ai/api/coding/paas/v4/chat/completions"
;;     :model "glm-5.3"
;;     :api-key "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
;;
'(:title "Model Bank"
   :name "modelbank"
   :version "0.2"
   :domain "modelbank-p.demo.data-ui.com"
   :domain-stg "modelbank.demo.data-ui.com"
   ;; WARNING: :repl must be nil in production
   :repl t
   ;; Petting-zoo guest surface (D1): passwordless guest login; guest
   ;; reaches the app-level endpoints via "public" in :api-roles, and
   ;; the built-in type-roles overlays below open :users / :roles /
   ;; :permissions to guest reads; "public" on every user-defined type
   ;; opens the gallery itself (row visibility stays per-record).
   :guest-allowed t
  :guest-auto nil
   :api-roles ("logged-in" "public")
   :landing-page :models
   :new-roles (:ai-user ("read") :deployer ("read"))
   :types
   (:directories
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :tree t :is-leaf nil :parent-type :directories :fs-backed t
       :type-roles ("directories-user" "public")
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
       :add-form (:fields t))

     :models
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :type-roles ("models-user" "public")
       :default-sort (:name :asc)
       :views (:main (:tables (:models :images :ratings :users)))
       :fields
       (:name
         (:type :text :identity t :searchable t :sortable t
           :ui (:label "Model Name" :widget :textbox)
           :validations (:required)
           :source (:view :main :column :name :agg :first)
           :column t :not-null t :unique t)
         :user
         (:type :text
           :autofill :user
           :force-sql-name "model_user"
           :ui (:label "Owner" :widget :textbox :read-only t)
           :target :users
           :source (:view :main :table :users :column :name :agg :first)
           :column t :not-null t)
         :description
         (:type :text
           :ui (:label "Model Description" :widget :textarea)
           :source (:view :main :column :description :agg :first)
           :column t :not-null nil :unique nil)
         :model
         (:type :text
           :ui (:label "Model Code" :widget :code)
           :source (:view :main :column :model :agg :first)
           :column t :not-null nil :unique nil)
         :images
         (:type :list
           :ui (:label "Images" :widget :image-list)
           :source (:view :main :table :images :column :name :agg :distinct))
         :rating
         (:type :integer
           :ui (:label "My Rating" :widget :stars)
           :validations ((:in-range :min 1 :max 5))
           :source (:view :main :table :ratings :column :rating
                    :scope :user :agg :first)
           :write-to (:table :ratings
                       :model :this
                       :user :user
                       :rating :value))
         :average-rating
         (:type :real
           :ui (:label "Rating" :widget :stars :read-only t :precision 1)
           :source (:view :main :table :ratings :column :rating
                    :agg :avg))
         :deploy
         (:type :button
           :ui (:label "Deploy Model" :widget :button)
           :action (:deploy-model :field :model))
         :generate
         (:type :button
           :ui (:label "Generate Model" :widget :button)
           :action (:generate-model :description-field :description
                                    :model-field :model)))
       :list-form (:fields (:name :user :description :model :images :average-rating))
       :update-form (:fields t)
       :add-form (:fields (:name :description :model :images :rating)))

     :images
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :tree t :is-leaf t :parent-type :directories :fs-backed t
       :type-roles ("models-user" "public")
       :views (:main (:tables (:images :users :models) :scope :user)
                :users (:tables (:users) :scope :user)
                :models (:tables (:models) :scope :user))
       :fields
       (:name
         (:type :text :identity t :path t
           :ui (:label "File" :widget :textbox)
           :validations (:required)
           :source (:view :main :column :name :agg :first)
           :column t :not-null t :unique t)
         :file
         ;; TODO: For :type :file, the validation :valid-file should exist
         ;;       The validation should allow for NIL, but should otherwise
         ;;       check that the file path is correct, that the directory
         ;;       exists, and that the file does not already exist.
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
           :source-all (:view :users :table :users :column :name :agg :list)
           :column t :not-null t)
         :model
         (:type :text
           :force-sql-name "image_model"
           :ui (:label "Model" :widget :select)
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
       :type-roles ("models-user" "public")
       :views (:main (:tables (:ratings :models :users))
                :models (:tables (:models))
                :users (:tables (:users) :scope :user))
       :fields
       (:model
         (:type :text :identity t
           :force-sql-name "rating_model"
           :ui (:label "Model" :widget :select)
           :target :models
           :source (:view :main :table :models :column :name :agg :first)
           :source-all (:view :models :table :models :column :name :agg :list)
           :column t :not-null t)
         :user
         (:type :text :identity t
           :autofill :user
           :force-sql-name "rating_user"
           :ui (:label "User" :widget :textbox :read-only t)
           :target :users
           :source (:view :main :table :users :column :name :agg :first)
           :source-all (:view :users :table :users :column :name :agg :list)
           :column t :not-null t)
         :rating
         (:type :integer
           :ui (:label "Rating" :widget :stars)
           :validations ((:in-range :min 1 :max 5))
           :source (:view :main :table :ratings :column :rating :agg :first)
           :column t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :hot-models
     (:rollup t
       :grain :models
       :type-roles ("models-user" "public")
       :filter ((:ratings :created-at :last-days 30))
       :views (:main (:tables (:models :ratings)))
       :list-form (:fields t)
       :fields
       (:name
         (:source (:view :main :table :models :column :name :agg :first)
           :sortable t
           :ui (:label "Model"))
         :recent-ratings
         (:type :integer
           :source (:view :main :table :ratings :column :id :agg :count)
           :sortable t
           :ui (:label "Recent Ratings" :widget :stars))
         :recent-avg
         (:type :real
           :source (:view :main :table :ratings :column :rating :agg :avg)
           :sortable t
           :ui (:label "Recent Average" :widget :stars))))

     :top-contributors
     (:rollup t
       :grain :users
       :type-roles ("models-user" "public")
       :views (:main (:tables (:users :models)))
       :list-form (:fields t)
       :fields
       (:name
         (:source (:view :main :table :users :column :name :agg :first)
           :sortable t
           :ui (:label "User"))
         :model-count
         (:type :integer
           :source (:view :main :table :models :column :id :agg :count)
           :sortable t
           :ui (:label "Models"))))
     ;; Petting-zoo D1 overlays: guest reads the built-in account /
     ;; role / permission lists (tier identity stays visible; :settings
     ;; stays structurally unreachable — guest is never granted the
     ;; settings role). add-type-roles only inserts missing resources,
     ;; so these must be present before the profile's first set-model.
     :users (:type-roles ("logged-in" "public" "user-creator"))
     :roles (:type-roles ("logged-in" "public" "role-creator"))
     :permissions (:type-roles ("logged-in" "public" "permission-creator"))))
