;;
;; SceneBook — film continuity tracker (Mel's app)
;;
;; The script, structured: characters x scenes x looks x state, sliced
;; by chips, sorted by padded labels. Eight table types, two page
;; projections, a photos grid on every noun that can appear in frame
;; or be worn. See workbench/mels-app-notes.org.
;;
;; Every target field is :sortable t — sorting rides the joined
;; display column ("Sort by :target field display column",
;; 2026-11-11); each target's table sits in the type's
;; :views :main :tables.
;;
'(:title "SceneBook"
  :name "scenebook"
  :version "0.1"
  :domain "scenebook.demo.data-ui.com"
  :guest-allowed t
  :guest-auto nil
  :api-roles ("logged-in" "public")
  ;; WARNING: :repl must be nil in production
  :repl t
  :landing-page :home
  :types
  (;; Home strip, declaration order = tab order.

   :home
   (:page t
     :of :pages
     :filter ((:pages :slug :eq "home"))
     :type-roles ("public")
     :view-form (:fields (:title :body))
     :fields (:title (:ui (:label "Title" :widget :heading
                          :hide-label t))
              :body (:ui (:widget :markdown :hide-label t))
              :slug (:ui (:label "Slug"))))

   :appearances
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("logged-in")
     :default-sort (:label :asc)
     :views (:main (:tables (:appearances :appearance-costumes
                             :costumes :characters :scenes
                             :scripts :photos))
             :costumes (:tables (:costumes)))
     :fields
     (:label
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Appearance" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :label :agg :first)
         :column t :not-null t :unique t)
      :character
       (:type :text :sortable t
         :ui (:label "Character" :widget :search-select
              :filter-with :reference)
         :target :characters
         :source (:view :main :table :characters :column :name
                  :agg :first)
         :source-all (:table :characters :column :name :agg :list)
         :column t)
      :scene
       (:type :text :sortable t
         :ui (:label "Scene" :widget :search-select
              :filter-with :reference)
         :target :scenes
         :source (:view :main :table :scenes :column :label
                  :agg :first)
         :source-all (:table :scenes :column :label :agg :list)
         :column t)
      :script
       (:type :text :sortable t
         :ui (:label "Script" :widget :search-select
              :filter-with :reference)
         :target :scripts
         :source (:view :main :table :scripts :column :title
                  :agg :first)
         :source-all (:table :scripts :column :title :agg :list)
         :column t)
      :wardrobe
       (:type :text :sortable t :searchable t
         :ui (:label "Wardrobe" :widget :textarea)
         :source (:view :main :column :wardrobe :agg :first)
         :column t)
      :makeup
       (:type :text :sortable t :searchable t
         :ui (:label "Makeup" :widget :textarea)
         :source (:view :main :column :makeup :agg :first)
         :column t)
      :hair
       (:type :text :sortable t :searchable t
         :ui (:label "Hair" :widget :textarea)
         :source (:view :main :column :hair :agg :first)
         :column t)
      :state
       (:type :text :sortable t
         :ui (:label "State" :widget :select
              :options ("Planned" "Filmed" "Cleared"))
         :source (:view :main :column :state :agg :first)
         :column t)
      :notes
       (:type :text :sortable t
         :ui (:label "Notes" :widget :textarea)
         :source (:view :main :column :notes :agg :first)
         :column t)
      :costumes
       (:type :list
         :ui (:label "Costumes" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :costumes :column :name
                  :agg :distinct)
         :source-all (:view :costumes :table :costumes :column :name
                  :agg :list)
         :join-table :appearance-costumes))
     :list-form (:fields t)
     :update-form (:fields t)
     :view-form (:fields t)
     :add-form (:fields t))

   :scenes
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("logged-in")
     :default-sort (:label :asc)
     :views (:main (:tables (:scenes :scripts :photos)))
     :fields
     (:label
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Scene" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :label :agg :first)
         :column t :not-null t :unique t)
      :scene-number
       (:type :integer :sortable t
         :ui (:label "Scene Number" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :scene-number :agg :first)
         :column t :not-null t)
      :shooting-order
       (:type :integer :sortable t
         :ui (:label "Shooting Order" :widget :textbox)
         :source (:view :main :column :shooting-order :agg :first)
         :column t)
      :description
       (:type :text :sortable t
         :ui (:label "Description" :widget :textarea)
         :source (:view :main :column :description :agg :first)
         :column t)
      :location
       (:type :text :sortable t :searchable t
         :ui (:label "Location" :widget :textbox)
         :source (:view :main :column :location :agg :first)
         :column t)
      :day-night
       (:type :text :sortable t
         :ui (:label "Day / Night" :widget :select
              :options ("Day" "Night" "Dusk" "Dawn"))
         :source (:view :main :column :day-night :agg :first)
         :column t)
      :script
       (:type :text :sortable t
         :ui (:label "Script" :widget :search-select
              :filter-with :reference)
         :target :scripts
         :source (:view :main :table :scripts :column :title
                  :agg :first)
         :source-all (:table :scripts :column :title :agg :list)
         :column t)
      :notes
       (:type :text :sortable t
         :ui (:label "Notes" :widget :textarea)
         :source (:view :main :column :notes :agg :first)
         :column t)
      :photos
       (:type :list
         :ui (:label "Photos" :widget :image-list)
         :source (:view :main :table :photos :column :name
                  :agg :distinct)))
     :list-form (:fields t)
     :update-form (:fields t)
     :view-form (:fields t)
     :add-form (:fields t))

   :characters
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("logged-in")
     :default-sort (:name :asc)
     :views (:main (:tables (:characters :actors :scripts :photos)))
     :fields
     (:name
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Character" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :name :agg :first)
         :column t :not-null t :unique t)
      :description
       (:type :text :sortable t
         :ui (:label "Description" :widget :textarea)
         :source (:view :main :column :description :agg :first)
         :column t)
      :actor
       (:type :text :sortable t
         :ui (:label "Actor" :widget :search-select
              :filter-with :reference)
         :target :actors
         :source (:view :main :table :actors :column :name
                  :agg :first)
         :source-all (:table :actors :column :name :agg :list)
         :column t)
      :script
       (:type :text :sortable t
         :ui (:label "Script" :widget :search-select
              :filter-with :reference)
         :target :scripts
         :source (:view :main :table :scripts :column :title
                  :agg :first)
         :source-all (:table :scripts :column :title :agg :list)
         :column t)
      :notes
       (:type :text :sortable t
         :ui (:label "Notes" :widget :textarea)
         :source (:view :main :column :notes :agg :first)
         :column t)
      :photos
       (:type :list
         :ui (:label "Photos" :widget :image-list)
         :source (:view :main :table :photos :column :name
                  :agg :distinct)))
     :list-form (:fields t)
     :update-form (:fields t)
     :view-form (:fields t)
     :add-form (:fields t))

   :costumes
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("logged-in")
     :default-sort (:name :asc)
     :views (:main (:tables (:costumes :scripts :characters :photos)))
     :fields
     (:name
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Costume" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :name :agg :first)
         :column t :not-null t :unique t)
      :description
       (:type :text :sortable t
         :ui (:label "Description" :widget :textarea)
         :source (:view :main :column :description :agg :first)
         :column t)
      :script
       (:type :text :sortable t
         :ui (:label "Script" :widget :search-select
              :filter-with :reference)
         :target :scripts
         :source (:view :main :table :scripts :column :title
                  :agg :first)
         :source-all (:table :scripts :column :title :agg :list)
         :column t)
      :character
       (:type :text :sortable t
         :ui (:label "Owner" :widget :search-select
              :filter-with :reference)
         :target :characters
         :source (:view :main :table :characters :column :name
                  :agg :first)
         :source-all (:table :characters :column :name :agg :list)
         :column t)
      :photos
       (:type :list
         :ui (:label "Photos" :widget :image-list)
         :source (:view :main :table :photos :column :name
                  :agg :distinct)))
     :list-form (:fields t)
     :update-form (:fields t)
     :view-form (:fields t)
     :add-form (:fields t))

   :actors
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("logged-in")
     :default-sort (:name :asc)
     :views (:main (:tables (:actors :photos)))
     :fields
     (:name
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Actor" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :name :agg :first)
         :column t :not-null t :unique t)
      :notes
       (:type :text :sortable t
         :ui (:label "Notes" :widget :textarea)
         :source (:view :main :column :notes :agg :first)
         :column t)
      :photos
       (:type :list
         :ui (:label "Photos" :widget :image-list)
         :source (:view :main :table :photos :column :name
                  :agg :distinct)))
     :list-form (:fields t)
     :update-form (:fields t)
     :view-form (:fields t)
     :add-form (:fields t))

   :scripts
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("logged-in")
     :default-sort (:title :asc)
     :views (:main (:tables (:scripts)))
     :fields
     (:title
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Script" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :title :agg :first)
         :column t :not-null t :unique t)
      :production
       (:type :text :sortable t
         :ui (:label "Production" :widget :textbox)
         :source (:view :main :column :production :agg :first)
         :column t)
      :notes
       (:type :text :sortable t
         :ui (:label "Notes" :widget :textarea)
         :source (:view :main :column :notes :agg :first)
         :column t))
     :list-form (:fields t)
     :update-form (:fields t)
     :view-form (:fields t)
     :add-form (:fields t))

   :photos
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :tree t :is-leaf t :parent-type :directories :fs-backed t
     :type-roles ("logged-in")
     :default-sort (:name :asc)
     :views (:main (:tables (:photos :appearances :actors
                             :scenes :characters :costumes)))
     :fields
     (:image
       (:type :text
         :ui (:label "Image" :widget :image)
         :source (:view :main :column :name :agg :first)
         :column nil)
      :file
       (:type :file
         :ui (:label "Select File" :widget :file)
         :validations (:required))
      :name
       (:type :text :identity t :path t :sortable t :searchable t
         :ui (:label "File" :widget :file-path
              :directory-type :directories)
         :validations (:required)
         :source (:view :main :column :name :agg :first)
         :column t :not-null t :unique t)
      :appearance
       (:type :text :sortable t
         :ui (:label "Appearance" :widget :search-select
              :filter-with :reference)
         :target :appearances
         :source (:view :main :table :appearances :column :label
                  :agg :first)
         :source-all (:table :appearances :column :label :agg :list)
         :column t)
      :actor
       (:type :text :sortable t
         :ui (:label "Actor" :widget :search-select
              :filter-with :reference)
         :target :actors
         :source (:view :main :table :actors :column :name
                  :agg :first)
         :source-all (:table :actors :column :name :agg :list)
         :column t)
      :scene
       (:type :text :sortable t
         :ui (:label "Scene" :widget :search-select
              :filter-with :reference)
         :target :scenes
         :source (:view :main :table :scenes :column :label
                  :agg :first)
         :source-all (:table :scenes :column :label :agg :list)
         :column t)
      :character
       (:type :text :sortable t
         :ui (:label "Character" :widget :search-select
              :filter-with :reference)
         :target :characters
         :source (:view :main :table :characters :column :name
                  :agg :first)
         :source-all (:table :characters :column :name :agg :list)
         :column t)
      :costume
       (:type :text :sortable t
         :ui (:label "Costume" :widget :search-select
              :filter-with :reference)
         :target :costumes
         :source (:view :main :table :costumes :column :name
                  :agg :first)
         :source-all (:table :costumes :column :name :agg :list)
         :column t))
     ;; Filing workbench, not a gallery: thumbnail, name, and the
     ;; five targets as columns / chips.
     :list-form (:fields (:image :name :appearance :actor :scene
                         :character :costume))
     :update-form (:fields (:appearance :actor :scene :character
                            :costume))
     :view-form (:fields (:image :name :appearance :actor :scene
                         :character :costume))
     :add-form (:fields (:file :name :appearance :actor :scene
                         :character :costume)))

   :about
   (:page t
     :of :pages
     :filter ((:pages :slug :eq "about"))
     :type-roles ("public")
     :view-form (:fields (:title :body))
     :fields (:title (:ui (:label "Title" :widget :heading
                          :hide-label t))
              :body (:ui (:widget :markdown :hide-label t))
              :slug (:ui (:label "Slug"))))

   ;; Admin view.

   :pages
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :category :system
     :type-roles ("logged-in")
     :default-sort (:slug :asc)
     :fields
     (:slug
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Slug" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :slug :agg :first)
         :column t :not-null t :unique t)
      :title
       (:type :text :sortable t
         :ui (:label "Title" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :title :agg :first)
         :column t :not-null t)
      :body
       (:type :text :sortable t
         :ui (:label "Body" :widget :textarea)
         :source (:view :main :column :body :agg :first)
         :column t))
     :list-form (:fields t)
     :update-form (:fields t)
     :view-form (:fields (:title :body))
     :add-form (:fields t))

   :directories
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :tree t :is-leaf nil :parent-type :directories :fs-backed t
     :category :system
     :type-roles ("logged-in")
     :default-sort (:name :asc)
     :views (:main (:tables (:directories)))
     :fields
     (:name
       (:type :text :identity t :path t :sortable t :searchable t
         :ui (:label "Directory" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :name :agg :first)
         :column t :not-null t :unique t))
     :list-form (:fields t)
     :update-form (:fields t)
     :view-form (:fields t))

   ;; Joiner (internal).

   :appearance-costumes
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :appearances)
       :reference (:target :costumes)))))
