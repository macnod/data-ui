;;
;; Data UI Backlog — the backlog app (dogfood)
;;
;; Data UI's own development backlog as a Data UI application: items
;; with status, tags, contributors, plans, and a dated journal of
;; event notes, replacing docs/todo.org + docs/done.org as the
;; substrate (org stays the source of truth until cutover; the dev
;; import is one command from the workbench's backlog/import.el).
;;
;; Shape (backlog-database-plan.org): six states as rows with
;; two-digit prefixes so lexicographic order = lifecycle order; the
;; items list is a total-order queue (:queue-slot, gaps of 100 from
;; 1000); status is a reference field (chip, sortable through the
;; joined display column); notes embed under the items view-form as a
;; child region (the journal); plans are 1:N rows surfaced on items
;; as a plain value list; contributors M2M through authors; :pages
;; holds the About content behind the Admin view with :about the
;; public projection. plan-kinds / note-kinds are the reference
;; tables behind the kind chips (the states pattern: rows seeded
;; from their JSONs, kind fields :filter-with :reference);
;; file-backed notes ride the notes type as kind Note beside the
;; drawer-stamp events (kind Event). Descriptions are never
;; :searchable — nothing index-bearing on the big text columns (a
;; plan is a whole file); search lives on titles and summaries.
;; Notes are inserted with roles ["backlog-agents"
;; "contributors"], mirroring plans; the public rows are items,
;; states, tags, and authors, inserted with ["public"
;; "backlog-agents" "contributors"] — the API pre-selects nothing.
;;
;; Author: Rosie (glm-5.3) from the plan; Donnie Cameron
;; Created: 2026-11-15
;; Prompt: /implement-data-ui-plan "Backlog database"
;;
'(:title "Data UI Backlog"
  :name "backlog"
  :version "0.2"
  :domain "backlog.demo.data-ui.com"
  :domain-stg "backlog-stg.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  :guest-allowed t
  :guest-auto nil
  :api-roles ("logged-in" "public")
  :landing-page :items
  ;; Narrowed roles declared here (the scenebook reader/editor
  ;; pattern) instead of the (role perms…) :type-roles form:
  ;; add-type-roles passes raw :type-roles entries to a:add-resource,
  ;; which chokes on list specs (only ensure-model-roles half-supports
  ;; the form) — engine gap, backlog item.
  :new-roles (:backlog-agents ("create" "read" "update")
              :contributors ("read"))
  :types
  (;; Home strip, declaration order = tab order.

   :items
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("backlog-agents" "contributors" "public")
     :default-sort (:queue-slot :asc)
     :views (:main (:tables (:items :item-tags :tags
                            :item-contributors :authors
                            :states :plans))
             :tags (:tables (:tags))
             :states (:tables (:states))
             :authors (:tables (:authors)))
     :fields
     (:title
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Title" :widget :textbox)
         :validations (:required (:max-length :max 120))
         :source (:view :main :column :title :agg :first)
         :column t :not-null t :unique t)
      :description
       (:type :text
         :ui (:label "Description" :widget :markdown)
         :source (:view :main :column :description :agg :first)
         :column t)
      :status
       (:type :text :sortable t
         :ui (:label "Status" :widget :search-select
              :filter-with :reference)
         :target :states
         :source (:view :main :table :states :column :name :agg :first)
         :source-all (:table :states :column :name :agg :list)
         :column t)
      :target-date
       (:type :text :sortable t
         :ui (:label "Target" :widget :textbox)
         :source (:view :main :column :target-date :agg :first)
         :column t)
      :queue-slot
       (:type :integer :sortable t
         :ui (:label "Slot" :widget :textbox)
         :source (:view :main :column :queue-slot :agg :first)
         :column t)
      :tags
       (:type :list :validations (:join-items-exist)
         :ui (:label "Tags" :widget :checkbox-list)
         :source (:view :main :table :tags :column :name :agg :distinct)
         :source-all (:view :tags :table :tags :column :name :agg :list)
         :join-table :item-tags)
      :contributors
       (:type :list :validations (:join-items-exist)
         :ui (:label "Contributors" :widget :checkbox-list)
         :source (:view :main :table :authors :column :name
                  :agg :distinct)
         :source-all (:view :authors :table :authors :column :name
                      :agg :list)
         :join-table :item-contributors)
      :plans
       (:type :list :validations (:join-items-exist)
         :ui (:label "Plans" :widget :checkbox-list)
         :source (:view :main :table :plans :column :title :agg :distinct)
         :source-all (:table :plans :column :title :agg :list)
         :join-table :item-plans)
      :created-at
       (:ui (:label "Created" :widget :textbox :read-only t)
         :sortable t)
      :updated-at
       (:ui (:label "Updated" :widget :textbox :read-only t)
         :sortable t))
     :list-form (:fields (:title :status :tags :contributors :plans
                         :target-date :queue-slot :created-at
                         :updated-at))
     :update-form (:fields t)
     :view-form (:fields t
                 :hide-list t
                 :children ((:type :notes :via :item
                             :sort (:date :desc))))
     :add-form (:fields (:title :description :status :tags
                         :contributors :target-date :queue-slot)))

   :tags
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("backlog-agents" "contributors" "public")
     :default-sort (:name :asc)
     :views (:main (:tables (:tags)))
     :fields
     (:name
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Tag" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :name :agg :first)
         :column t :not-null t :unique t)
      :description
       (:type :text
         :ui (:label "Description" :widget :markdown)
         :source (:view :main :column :description :agg :first)
         :column t))
     :list-form (:fields t) :update-form (:fields t)
     :view-form (:fields t) :add-form (:fields t))

   :states
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("contributors" "public")
     :default-sort (:name :asc)
     :views (:main (:tables (:states)))
     :fields
     (:name
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "State" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :name :agg :first)
         :column t :not-null t :unique t)
      :description
       (:type :text
         :ui (:label "Description" :widget :markdown)
         :source (:view :main :column :description :agg :first)
         :column t))
     :list-form (:fields t) :update-form (:fields t)
     :view-form (:fields t) :add-form (:fields t))

   :authors
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("backlog-agents" "contributors" "public")
     :default-sort (:name :asc)
     :views (:main (:tables (:authors)))
     :fields
     (:name
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Name" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :name :agg :first)
         :column t :not-null t :unique t)
      :description
       (:type :text
         :ui (:label "Description" :widget :markdown)
         :source (:view :main :column :description :agg :first)
         :column t)
      :model
       (:type :text :sortable t :searchable t
         :ui (:label "Model" :widget :textbox)
         :source (:view :main :column :model :agg :first)
         :column t)
      :provider
       (:type :text :sortable t :searchable t
         :ui (:label "Provider" :widget :textbox)
         :source (:view :main :column :provider :agg :first)
         :column t))
     :list-form (:fields t) :update-form (:fields t)
     :view-form (:fields t) :add-form (:fields t))

   ;; Plan kinds and note kinds — the reference tables behind the
   ;; kind chips (the states pattern). Rows are seeded from
   ;; plan-kinds.json / note-kinds.json by the load stage; the
   ;; kind fields on plans / notes point here with :filter-with
   ;; :reference, which is what makes a field chip-eligible in
   ;; the frontend.

   :plan-kinds
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("backlog-agents" "contributors")
     :default-sort (:name :asc)
     :views (:main (:tables (:plan-kinds)))
     :fields
     (:name
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Plan kind" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :name :agg :first)
         :column t :not-null t :unique t)
      :description
       (:type :text
         :ui (:label "Description" :widget :markdown)
         :source (:view :main :column :description :agg :first)
         :column t))
     :list-form (:fields t) :update-form (:fields t)
     :view-form (:fields t) :add-form (:fields t))

   :note-kinds
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("backlog-agents" "contributors")
     :default-sort (:name :asc)
     :views (:main (:tables (:note-kinds)))
     :fields
     (:name
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Note kind" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :name :agg :first)
         :column t :not-null t :unique t)
      :description
       (:type :text
         :ui (:label "Description" :widget :markdown)
         :source (:view :main :column :description :agg :first)
         :column t))
     :list-form (:fields t) :update-form (:fields t)
     :view-form (:fields t) :add-form (:fields t))

   ;; The journal. Identity is the composite (:title :item): a
   ;; "Created" event exists once per item, not once in the world
   ;; (the single-column note_title index made every note after the
   ;; first collide — 420 of the 425 first-run insert failures).
   ;; :title is declared first deliberately: identity-keys follows
   ;; declaration order and public-title takes the first identity
   ;; key with a non-blank value, and FK fields hydrate to their
   ;; display string on be-rec records (verified 2026-11-15: an
   ;; item's :status reads "10 Open", the uuid rides :status-id) —
   ;; an :item-first order would title every public note page by
   ;; its item. :plans mirrors the shape.
   :notes
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("backlog-agents" "contributors")
     :default-sort (:date :desc)
     :views (:main (:tables (:notes :items :authors :note-kinds))
             :items (:tables (:items))
             :authors (:tables (:authors)))
     :fields
     (:title
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Title" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :title :agg :first)
         :column t :not-null t)
      :item
       (:type :text :force-sql-name "note_item"
         :identity t
         :ui (:label "Item" :widget :search-select
              :filter-with :reference)
         :target :items
         :sortable t
         :source (:view :main :table :items :column :title :agg :first)
         :source-all (:table :items :column :title :agg :list)
         :column t :not-null t)
      :kind
       (:type :text
         :ui (:label "Kind" :widget :search-select
              :filter-with :reference)
         :target :note-kinds
         :sortable t
         :source (:view :main :table :note-kinds :column :name :agg :first)
         :source-all (:table :note-kinds :column :name :agg :list)
         :column t :not-null t)
      :author
       (:type :text :force-sql-name "note_author"
         :ui (:label "Author" :widget :search-select
              :filter-with :reference)
         :target :authors
         :sortable t
         :source (:view :main :table :authors :column :name
                  :agg :first)
         :source-all (:table :authors :column :name :agg :list)
         :column t)
      :date
       (:type :text :sortable t
         :ui (:label "Date" :widget :textbox)
         :source (:view :main :column :date :agg :first)
         :column t)
      :summary
       (:type :text :searchable t
         :ui (:label "Summary" :widget :markdown)
         :source (:view :main :column :summary :agg :first)
         :validations ((:max-length :max 8000))
         :column t)
      :description
       (:type :text
         :ui (:label "Description" :widget :markdown)
         :source (:view :main :column :description :agg :first)
         :column t))
     :list-form (:fields (:title :item :kind :author :date :summary
                         :created-at :updated-at))
     :update-form (:fields t)
     :view-form (:fields t) :add-form (:fields t))

   :plans
   (:table t
     :create :auto :update :auto :delete :auto :display t
     :type-roles ("backlog-agents" "contributors")
     :default-sort (:updated-at :desc)
     :views (:main (:tables (:plans :items :authors :plan-kinds))
             :items (:tables (:items))
             :authors (:tables (:authors)))
     :fields
     (:title
       (:type :text :identity t :sortable t :searchable t
         :ui (:label "Title" :widget :textbox)
         :validations (:required)
         :source (:view :main :column :title :agg :first)
         :column t :not-null t)
      :item
       (:type :text :force-sql-name "plan_item"
         :ui (:label "Item" :widget :search-select
              :filter-with :reference)
         :target :items
         :sortable t
         :source (:view :main :table :items :column :title :agg :first)
         :source-all (:table :items :column :title :agg :list)
         :column t :not-null t)
      :kind
       (:type :text
         :ui (:label "Kind" :widget :search-select
              :filter-with :reference)
         :target :plan-kinds
         :sortable t
         :source (:view :main :table :plan-kinds :column :name :agg :first)
         :source-all (:table :plan-kinds :column :name :agg :list)
         :column t :not-null t)
      :author
       (:type :text :force-sql-name "plan_author"
         :ui (:label "Author" :widget :search-select
              :filter-with :reference)
         :target :authors
         :sortable t
         :source (:view :main :table :authors :column :name
                  :agg :first)
         :source-all (:table :authors :column :name :agg :list)
         :column t)
      :summary
       (:type :text :searchable t
         :ui (:label "Summary" :widget :markdown)
         :source (:view :main :column :summary :agg :first)
         :validations ((:max-length :max 8000))
         :column t)
      :description
       (:type :text
         :ui (:label "Plan" :widget :markdown)
         :source (:view :main :column :description :agg :first)
         :column t))
     :list-form (:fields (:title :item :kind :author :summary
                         :created-at :updated-at))
     :update-form (:fields t)
     :view-form (:fields t) :add-form (:fields t))

   ;; The About tab, last in the Home strip.

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
     :type-roles ("backlog-agents" "contributors")
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
     :list-form (:fields t) :update-form (:fields t)
     :view-form (:fields t) :add-form (:fields t))

   ;; Joiners (internal).

   :item-tags
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :items)
       :reference (:target :tags)))

   :item-plans
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :items)
       :reference (:target :plans)))

   :item-contributors
   (:table t :is-joiner t :internal t
     :fields
     (:reference (:target :items)
       :reference (:target :authors)))))
