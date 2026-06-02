(defparameter *model-1*
`(:todos
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :type-roles ("todo-users")
       :views (:main (:tables (:todos :todo-tags :tags))
                :tags (:tables (:tags)))
       :fields (:name (:type :text
                        :ui (:label "To Do" :input-type :line)
                        :validations (:required
                                       (lambda (type-key field-key value user)
                                         (declare (ignore user))
                                         (unless (< (length value) 20)
                                           (validation-error-string
                                              type-key field-key value
                                              "must be less than 20 characters."))))
                        :source (:view :main :column :name :agg :first)
                        :column t :not-null t :unique t)
                 :points (:type :integer :default 0
                           :ui (:label "Points" :input-type :line)
                           :validations (:required)
                           :source (:view :main :column :points :agg :first)
                           :column t :not-null t)
                 :done (:type :boolean :default :false
                         :ui (:label "Done" :input-type :check-box)
                         :source (:view :main :column :done :agg :first)
                         :column t :not-null t)
                 :tags (:type :list
                         :ui (:label "Tags" :input-type :checkbox-list)
                         :validations (:join-items-exist)
                         :source (:view :main :table :tags :column :name :agg :list)
                         :source-all (:view :tags :table :tags :column :name :agg :list)
                         :join-table :todo-tags))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :tags
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :type-roles ("todo-users")
       :fields (:name (:type :text
                        :ui (:label "Tag" :input-type :line)
                        :validations (:required)
                        :source (:view :main :table :tags :column :name :agg :first)
                        :column t :not-null t :unique t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :todo-tags
     (:table t :is-joiner t :internal t
       :fields (:reference (:target :todos)
                 :reference (:target :tags)))))

(defparameter *model-2*
  `(:directories
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :tree t :is-leaf nil :parent-type :directories :fs-backed t
       :type-roles ("file-users")
       :views (:main (:tables (:directories)))
       :fields (:name (:type :text :path t
                        :ui (:label "Directory" :input-type :line)
                        :validations (:required)
                        :source (:view :main :column :name :agg :first)
                        :column t :not-null t :unique t)
                 :bogus (:type :text :default ""
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
       :fields (:name (:type :text :path t
                        :ui (:label "File" :input-type :line)
                        :validations (:required)
                        :source (:view :main :column :name :agg :first)
                        :column t :not-null t :unique t)
                 :file (:type :file
                         :ui (:label "Select File" :input-type :file)
                         :validations (:required)))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))))

(defparameter *model-3*
  `(:directories
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :tree t :is-leaf nil :parent-type :directories :fs-backed t
       :type-roles ("parts")
       :views (:main (:tables (:directories)))
       :fields (:name (:type :text :path t
                        :ui (:label "Directory" :input-type :line)
                        :validations (:required)
                        :source (:view :main :column :name :agg :first)
                        :column t :not-null t :unique t)
                 :bogus (:type :text :default ""
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
       :fields (:name (:type :text :path t
                        :ui (:label "File" :input-type :line)
                        :validations (:required)
                        :source (:view :main :column :name :agg :first)
                        :column t :not-null t :unique t)
                 :file (:type :file
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
       :fields (:name 
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
       :fields (:reference (:target :parts)
                 :reference (:target :files)))))
