;;
;; SceneBook — film continuity tracker (Mel's app)
;;
;; The script, structured: characters x scenes x looks x state, sliced
;; by chips, sorted by padded labels. Nine working tables, two system
;; tables, three page projections (home, script, about), a photos grid
;; on every noun that can appear in frame or be worn (looks included),
;; story days as a cross-department breakdown axis, and cross-lists
;; both ways — characters / looks / story days on scenes, scenes and
;; casting on characters (a growing character carries several actors).
;; One script per instance; the script itself is a page (v0.6). See
;; workbench/mels-app/notes.org.
;;
;; Every target field is :sortable t — sorting rides the joined
;; display column ("Sort by :target field display column",
;; 2026-11-11); each target's table sits in the type's
;; :views :main :tables.
;;
'(:title "SceneBook"
  :name "scenebook"
  :version "0.6"
  :domain "scenebook.demo.data-ui.com"
  :guest-allowed t
  :guest-auto nil
  :api-roles ("logged-in" "public")
  ;; WARNING: :repl must be nil in production
  :repl t
  :landing-page :home
  ;; reader = read-only, editor = full CRUD; rows carry both (Add form
  ;; preselects them; API inserts name them explicitly).
  :new-roles (:reader ("read")
              :editor ("create" "read" "update" "delete"))
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

   :script
   (:page t
     :of :pages
     :filter ((:pages :slug :eq "script"))
     :type-roles ("reader" "editor")
     :view-form (:fields (:title :body))
     :fields (:title (:ui (:label "Title" :widget :heading
                          :hide-label t))
              :body (:ui (:widget :markdown :hide-label t))
              :slug (:ui (:label "Slug"))))

   :appearances
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("reader" "editor")
     :default-sort (:label :asc)
     :views (:main (:tables (:appearances :appearance-wardrobe
                             :wardrobe :appearance-makeup :makeup
                             :appearance-hair :hair :characters :scenes
                             :photos))
             :wardrobe (:tables (:wardrobe))
             :makeup (:tables (:makeup))
             :hair (:tables (:hair)))
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
      :makeups
       (:type :list
         :ui (:label "Makeup" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :makeup :column :name
                  :agg :distinct)
         :source-all (:view :makeup :table :makeup :column :name
                       :agg :list)
         :join-table :appearance-makeup)
      :hairs
       (:type :list
         :ui (:label "Hair" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :hair :column :style
                  :agg :distinct)
         :source-all (:view :hair :table :hair :column :style
                       :agg :list)
         :join-table :appearance-hair)
      :state
       (:type :text :sortable t
         :ui (:label "State" :widget :select
              :options ("Planned" "Filmed" "Cleared"))
         :source (:view :main :column :state :agg :first)
         :column t)
      :notes
       (:type :text :sortable t
         :ui (:label "Notes" :widget :markdown)
         :source (:view :main :column :notes :agg :first)
         :column t)
      :wardrobe
       (:type :list
         :ui (:label "Wardrobe" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :wardrobe :column :name
                  :agg :distinct)
         :source-all (:view :wardrobe :table :wardrobe :column :name
                       :agg :list)
         :join-table :appearance-wardrobe)
      :photos
       (:type :list
         :ui (:label "Photos" :widget :image-list)
         :source (:view :main :table :photos :column :name
                  :agg :distinct)))
     :list-form (:fields t)
     :update-form (:fields t)
     :view-form (:fields t)
     :add-form (:fields t))

   :story-days
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("reader" "editor")
     :default-sort (:label :asc)
     :views (:main (:tables (:story-days :story-day-scenes :scenes
                             :story-day-characters :characters))
             :scenes (:tables (:scenes))
             :characters (:tables (:characters)))
     :fields
     (:label
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Story Day" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :label :agg :first)
         :column t :not-null t :unique t)
      :script-pages
       (:type :text :sortable t :searchable t
         :ui (:label "Script Pages" :widget :textbox)
         :source (:view :main :column :script-pages :agg :first)
         :column t)
      :scenes
       (:type :list
         :ui (:label "Scenes" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :scenes :column :label
                  :agg :distinct)
         :source-all (:view :scenes :table :scenes :column :label
                       :agg :list)
         :join-table :story-day-scenes)
      :characters
       (:type :list
         :ui (:label "Characters" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :characters :column :name
                  :agg :distinct)
         :source-all (:view :characters :table :characters
                       :column :name :agg :list)
         :join-table :story-day-characters)
      :notes
       (:type :text :sortable t
         :ui (:label "Notes" :widget :markdown)
         :source (:view :main :column :notes :agg :first)
         :column t))
     :list-form (:fields t)
     :update-form (:fields t)
     :view-form (:fields t)
     :add-form (:fields t))

   :scenes
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("reader" "editor")
     :default-sort (:label :asc)
     :views (:main (:tables (:scenes :character-scenes :characters
                             :scene-makeup :makeup :scene-hair
                             :hair :wardrobe-scenes :wardrobe
                             :story-day-scenes :story-days
                             :photos))
             :characters (:tables (:characters))
             :makeup (:tables (:makeup))
             :hair (:tables (:hair))
             :wardrobe (:tables (:wardrobe))
             :story-days (:tables (:story-days)))
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
         :ui (:label "Description" :widget :markdown)
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
      :characters
       (:type :list
         :ui (:label "Characters" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :characters :column :name
                  :agg :distinct)
         :source-all (:view :characters :table :characters
                       :column :name :agg :list)
         :join-table :character-scenes)
      :makeups
       (:type :list
         :ui (:label "Makeup" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :makeup :column :name
                  :agg :distinct)
         :source-all (:view :makeup :table :makeup :column :name
                       :agg :list)
         :join-table :scene-makeup)
      :hairs
       (:type :list
         :ui (:label "Hair" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :hair :column :style
                  :agg :distinct)
         :source-all (:view :hair :table :hair :column :style
                       :agg :list)
         :join-table :scene-hair)
      :wardrobe
       (:type :list
         :ui (:label "Wardrobe" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :wardrobe :column :name
                  :agg :distinct)
         :source-all (:view :wardrobe :table :wardrobe :column :name
                       :agg :list)
         :join-table :wardrobe-scenes)
      :story-days
       (:type :list
         :ui (:label "Story Days" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :story-days :column :label
                  :agg :distinct)
         :source-all (:view :story-days :table :story-days
                       :column :label :agg :list)
         :join-table :story-day-scenes)
      :notes
       (:type :text :sortable t
         :ui (:label "Notes" :widget :markdown)
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
     :type-roles ("reader" "editor")
     :default-sort (:name :asc)
     :views (:main (:tables (:characters :character-makeup :makeup
                             :character-hair :hair :character-wardrobe
                             :wardrobe :character-scenes :scenes
                             :character-actors :actors
                             :story-day-characters :story-days
                             :photos))
             :makeup (:tables (:makeup))
             :hair (:tables (:hair))
             :wardrobe (:tables (:wardrobe))
             :scenes (:tables (:scenes))
             :actors (:tables (:actors))
             :story-days (:tables (:story-days)))
     :fields
     (:name
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Character" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :name :agg :first)
         :column t :not-null t :unique t)
      :description
       (:type :text :sortable t
         :ui (:label "Description" :widget :markdown)
         :source (:view :main :column :description :agg :first)
         :column t)
      :actors
       (:type :list
         :ui (:label "Actors" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :actors :column :name
                  :agg :distinct)
         :source-all (:view :actors :table :actors :column :name
                       :agg :list)
         :join-table :character-actors)
      :scenes
       (:type :list
         :ui (:label "Scenes" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :scenes :column :label
                  :agg :distinct)
         :source-all (:view :scenes :table :scenes :column :label
                       :agg :list)
         :join-table :character-scenes)
      :makeups
       (:type :list
         :ui (:label "Makeup" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :makeup :column :name
                  :agg :distinct)
         :source-all (:view :makeup :table :makeup :column :name
                       :agg :list)
         :join-table :character-makeup)
      :hairs
       (:type :list
         :ui (:label "Hair" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :hair :column :style
                  :agg :distinct)
         :source-all (:view :hair :table :hair :column :style
                       :agg :list)
         :join-table :character-hair)
      :wardrobe
       (:type :list
         :ui (:label "Wardrobe" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :wardrobe :column :name
                  :agg :distinct)
         :source-all (:view :wardrobe :table :wardrobe :column :name
                       :agg :list)
         :join-table :character-wardrobe)
      :story-days
       (:type :list
         :ui (:label "Story Days" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :story-days :column :label
                  :agg :distinct)
         :source-all (:view :story-days :table :story-days
                       :column :label :agg :list)
         :join-table :story-day-characters)
      :notes
       (:type :text :sortable t
         :ui (:label "Notes" :widget :markdown)
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

   :makeup
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("reader" "editor")
     :default-sort (:name :asc)
     :views (:main (:tables (:makeup :character-makeup :characters
                             :scene-makeup :scenes :story-day-makeup
                             :story-days :photos))
             :characters (:tables (:characters))
             :scenes (:tables (:scenes))
             :story-days (:tables (:story-days)))
     :fields
     (:name
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Look" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :name :agg :first)
         :column t :not-null t :unique t)
      :characters
       (:type :list
         :ui (:label "Characters" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :characters :column :name
                  :agg :distinct)
         :source-all (:view :characters :table :characters
                       :column :name :agg :list)
         :join-table :character-makeup)
      :scenes
       (:type :list
         :ui (:label "Scenes" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :scenes :column :label
                  :agg :distinct)
         :source-all (:view :scenes :table :scenes :column :label
                       :agg :list)
         :join-table :scene-makeup)
      :story-days
       (:type :list
         :ui (:label "Story Days" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :story-days :column :label
                  :agg :distinct)
         :source-all (:view :story-days :table :story-days
                       :column :label :agg :list)
         :join-table :story-day-makeup)
      :foundation-color
       (:type :text :sortable t :searchable t
         :ui (:label "Foundation Color" :widget :textbox)
         :source (:view :main :column :foundation-color :agg :first)
         :column t)
      :concealer-color
       (:type :text :sortable t :searchable t
         :ui (:label "Concealer Color" :widget :textbox)
         :source (:view :main :column :concealer-color :agg :first)
         :column t)
      :lip-color
       (:type :text :sortable t :searchable t
         :ui (:label "Lip Color" :widget :textbox)
         :source (:view :main :column :lip-color :agg :first)
         :column t)
      :contour
       (:type :text :sortable t :searchable t
         :ui (:label "Contour" :widget :textbox)
         :source (:view :main :column :contour :agg :first)
         :column t)
      :blush
       (:type :text :sortable t :searchable t
         :ui (:label "Blush" :widget :textbox)
         :source (:view :main :column :blush :agg :first)
         :column t)
      :eye-product
       (:type :text :sortable t :searchable t
         :ui (:label "Eye Product" :widget :textbox)
         :source (:view :main :column :eye-product :agg :first)
         :column t)
      :notes
       (:type :text :sortable t
         :ui (:label "Notes" :widget :markdown)
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

   :hair
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("reader" "editor")
     :default-sort (:style :asc)
     :views (:main (:tables (:hair :character-hair :characters
                             :scene-hair :scenes :story-day-hair
                             :story-days :photos))
             :characters (:tables (:characters))
             :scenes (:tables (:scenes))
             :story-days (:tables (:story-days)))
     :fields
     (:style
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Style" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :style :agg :first)
         :column t :not-null t :unique t)
      :characters
       (:type :list
         :ui (:label "Characters" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :characters :column :name
                  :agg :distinct)
         :source-all (:view :characters :table :characters
                       :column :name :agg :list)
         :join-table :character-hair)
      :scenes
       (:type :list
         :ui (:label "Scenes" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :scenes :column :label
                  :agg :distinct)
         :source-all (:view :scenes :table :scenes :column :label
                       :agg :list)
         :join-table :scene-hair)
      :story-days
       (:type :list
         :ui (:label "Story Days" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :story-days :column :label
                  :agg :distinct)
         :source-all (:view :story-days :table :story-days
                       :column :label :agg :list)
         :join-table :story-day-hair)
      :products
       (:type :text :sortable t :searchable t
         :ui (:label "Products" :widget :textarea)
         :source (:view :main :column :products :agg :first)
         :column t)
      :notes
       (:type :text :sortable t
         :ui (:label "Notes" :widget :markdown)
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

   :wardrobe
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("reader" "editor")
     :default-sort (:name :asc)
     :views (:main (:tables (:wardrobe :character-wardrobe
                             :characters :wardrobe-scenes :scenes
                             :story-day-wardrobe :story-days
                             :photos))
             :characters (:tables (:characters))
             :scenes (:tables (:scenes))
             :story-days (:tables (:story-days)))
     :fields
     (:name
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Wardrobe" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :name :agg :first)
         :column t :not-null t :unique t)
      :description
       (:type :text :sortable t
         :ui (:label "Description" :widget :markdown)
         :source (:view :main :column :description :agg :first)
         :column t)
      :characters
       (:type :list
         :ui (:label "Characters" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :characters :column :name
                  :agg :distinct)
         :source-all (:view :characters :table :characters
                       :column :name :agg :list)
         :join-table :character-wardrobe)
      :scenes
       (:type :list
         :ui (:label "Scenes" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :scenes :column :label
                  :agg :distinct)
         :source-all (:view :scenes :table :scenes :column :label
                       :agg :list)
         :join-table :wardrobe-scenes)
      :story-days
       (:type :list
         :ui (:label "Story Days" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :story-days :column :label
                  :agg :distinct)
         :source-all (:view :story-days :table :story-days
                       :column :label :agg :list)
         :join-table :story-day-wardrobe)
      :notes
       (:type :text :sortable t
         :ui (:label "Notes" :widget :markdown)
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

   :actors
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("reader" "editor")
     :default-sort (:name :asc)
     :views (:main (:tables (:actors :character-actors :characters
                             :actor-scenes :scenes :actor-story-days
                             :story-days :photos))
             :characters (:tables (:characters))
             :scenes (:tables (:scenes))
             :story-days (:tables (:story-days)))
     :fields
     (:name
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Actor" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :name :agg :first)
         :column t :not-null t :unique t)
      :characters
       (:type :list
         :ui (:label "Characters" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :characters :column :name
                  :agg :distinct)
         :source-all (:view :characters :table :characters
                       :column :name :agg :list)
         :join-table :character-actors)
      :scenes
       (:type :list
         :ui (:label "Scenes" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :scenes :column :label
                  :agg :distinct)
         :source-all (:view :scenes :table :scenes :column :label
                       :agg :list)
         :join-table :actor-scenes)
      :story-days
       (:type :list
         :ui (:label "Story Days" :widget :checkbox-list)
         :validations (:join-items-exist)
         :source (:view :main :table :story-days :column :label
                  :agg :distinct)
         :source-all (:view :story-days :table :story-days
                       :column :label :agg :list)
         :join-table :actor-story-days)
      :notes
       (:type :text :sortable t
         :ui (:label "Notes" :widget :markdown)
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

   :photos
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :tree t :is-leaf t :parent-type :directories :fs-backed t
     :type-roles ("reader" "editor")
     :default-sort (:name :asc)
     :views (:main (:tables (:photos :appearances :actors
                             :scenes :characters :wardrobe
                             :makeup :hair)))
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
      :wardrobe
       (:type :text :sortable t
         :ui (:label "Wardrobe" :widget :search-select
              :filter-with :reference)
         :target :wardrobe
         :source (:view :main :table :wardrobe :column :name
                  :agg :first)
         :source-all (:table :wardrobe :column :name :agg :list)
         :column t)
      :makeup
       (:type :text :sortable t
         :ui (:label "Makeup" :widget :search-select
              :filter-with :reference)
         :target :makeup
         :source (:view :main :table :makeup :column :name
                  :agg :first)
         :source-all (:table :makeup :column :name :agg :list)
         :column t)
      :hair
       (:type :text :sortable t
         :ui (:label "Hair" :widget :search-select
              :filter-with :reference)
         :target :hair
         :source (:view :main :table :hair :column :style
                  :agg :first)
         :source-all (:table :hair :column :style :agg :list)
         :column t))
     ;; Filing workbench, not a gallery: thumbnail, name, and the
     ;; seven targets as columns / chips.
     :list-form (:fields (:image :name :appearance :actor :scene
                         :character :wardrobe :makeup :hair))
     :update-form (:fields (:name :appearance :actor :scene
                            :character :wardrobe :makeup :hair))
     :view-form (:fields (:image :name :appearance :actor :scene
                         :character :wardrobe :makeup :hair))
     :add-form (:fields (:file :name :appearance :actor :scene
                         :character :wardrobe :makeup :hair)))

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
     :type-roles ("reader" "editor")
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
       (:type :text
         :ui (:label "Body" :widget :markdown)
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
     :type-roles ("reader" "editor")
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

   ;; Joiners (internal).

   :appearance-wardrobe
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :appearances)
       :reference (:target :wardrobe)))

   :appearance-makeup
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :appearances)
       :reference (:target :makeup)))

   :appearance-hair
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :appearances)
       :reference (:target :hair)))

   :character-makeup
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :characters)
       :reference (:target :makeup)))

   :character-hair
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :characters)
       :reference (:target :hair)))

   :character-wardrobe
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :characters)
       :reference (:target :wardrobe)))

   :character-scenes
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :characters)
       :reference (:target :scenes)))

   :character-actors
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :characters)
       :reference (:target :actors)))

   :scene-makeup
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :scenes)
       :reference (:target :makeup)))

   :scene-hair
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :scenes)
       :reference (:target :hair)))

   :wardrobe-scenes
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :wardrobe)
       :reference (:target :scenes)))

   :story-day-scenes
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :story-days)
       :reference (:target :scenes)))

   :story-day-characters
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :story-days)
       :reference (:target :characters)))

   :story-day-wardrobe
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :story-days)
       :reference (:target :wardrobe)))

   :story-day-makeup
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :story-days)
       :reference (:target :makeup)))

   :story-day-hair
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :story-days)
       :reference (:target :hair)))

   :actor-scenes
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :actors)
       :reference (:target :scenes)))

   :actor-story-days
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :actors)
       :reference (:target :story-days)))))
