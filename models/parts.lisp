'(:title "Parts Tracker"
   :name "parts"
   :version "0.1"
   :domain "parts.demo.data-ui.com"
   :domain-stg "parts-stg.demo.data-ui.com"
   ;; WARNING: :repl must be nil in production
   :repl t
   :types
   (:directories
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :tree t :is-leaf nil :parent-type :directories :fs-backed t
       :type-roles ("parts")
       :default-sort (:name :asc)
       :views (:main (:tables (:directories)))
       :fields
       (:name
         (:type :text :identity t :path t :searchable t :sortable t
           :ui (:label "Directory" :widget :textbox)
           :validations (:required)
           :source (:view :main :column :name :agg :first)
           :column t :not-null t :unique t)
         :description 
         (:type :text :default "" :sortable t :searchable t
           :ui (:label "Bogus" :widget :textbox)
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
       :default-sort (:name :asc)
       :views (:main (:tables (:files)))
       :fields
       (:image
         (:type :text
           :ui (:label "Image" :widget :image)
           :source (:view :main :column :name :agg :first)
           :column nil)
         :file 
         ;; TODO: For :type :file, the validation :valid-file should exist
         ;;       The validation should allow for NIL, but should otherwise
         ;;       check that the file path is correct, that the directory
         ;;       exists, and that the file doesn't already exist.
         (:type :file
           :ui (:label "Select File" :widget :file)
           :validations (:required))
         :name
         (:type :text :identity t :path t :sortable t :searchable t
           :ui (:label "File" :widget :file-path
                 :directory-type :directories)
           :validations (:required)
           :source (:view :main :column :name :agg :first)
           :column t :not-null t :unique t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))
     
     :parts
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :type-roles ("parts")
       :default-sort (:name :asc)
       :views (:main (:tables (:parts :part-files :files))
                :files (:tables (:files))
                :primary-image (:tables (:parts :files)))
       :fields 
       (:name 
         (:type :text :identity t :sortable t :searchable t
           :ui (:label "Part Number" :widget :textbox)
           :validations (:required)
           :source (:view :main :column :name :agg :first)
           :column t :not-null t :unique t)
         :description
         (:type :text :searchable t
           :ui (:label "Part Description" :widget :textarea)
           :source (:view :main :column :description :agg :first)
           :column t :not-null nil :unique nil)
         ;; TODO: This field should be able to have a different
         ;; name.
         :files
         (:type :list
           :ui (:label "Images" :widget :checkbox-list)
           :validations (:join-items-exist)
           :source (:view :main :table :files :column :name :agg :distinct)
           :source-all (:view :files :table :files :column :name :agg :list)
           :join-table :part-files)
         ;; First plain FK in this model (not a joiner): pick the
         ;; canonical image for a part. Sits beside the :files
         ;; checkbox list on the same form — one widget picks
         ;; many, this one picks the one. Nullable on purpose (a
         ;; part may have no primary image), so it is the field
         ;; that exercises the SearchSelect ✕ clear path. Its
         ;; display source lives on its own view: :main has no
         ;; edge into :files for it (the :part-files joiner is
         ;; the only other relation and find-table-xrefs only
         ;; walks :target fields — putting this source on :main
         ;; is the ordered-xrefs diamond).
         :primary-image
         (:type :text
           :ui (:label "Primary Image" :widget :search-select)
           :target :files
           :source (:view :primary-image :table :files :column :name :agg :first)
           :source-all (:view :files :table :files :column :name :agg :list)
           :column t :not-null nil :unique nil))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :part-files
     (:table t :is-joiner t :internal t
       :fields
       (:reference (:target :parts)
         :reference (:target :files)))))
