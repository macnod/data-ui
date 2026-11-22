;; Test fixture for the render-cost suite (item "Optimize
;; rendering"): the palette shapes no existing fixture exercises end
;; to end. :stages is the fast path (default sort on a local integer
;; column, order differs from name order); :labels the M2M target
;; through :task-labels (non-:name identity :title); :teams the
;; joined-sort fallback (default sort on a sortable :target :users
;; field, the target-sort-test :items shape); :vaults the view-scope
;; fallback (modelbank-test :ratings shape: :users in the view's
;; tables, autofilled :user reference); :empties a target with no
;; rows; :cross the cross-source guard (a :text display field whose
;; (:column ...) sources the target's boolean / timestamp column —
;; legacy conversion is keyed on the source alias's field, so these
;; must stay on the legacy path); :tasks the owning type with one
;; field per shape plus a :ui :options field and an admin-only
;; :users :type-roles (the allowed-values-test suppression shape).
'(:title "Render Cost Test"
  :name "render-cost-test"
  :version "0.1"
  :domain "render-cost-test.demo.data-ui.com"
  :repl nil
  :types
  (:stages
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("rc-users" "admin-only")
      :default-sort (:position :asc)
      :views (:main (:tables (:stages)))
      :fields
      (:name
        (:type :text :identity t :sortable t
          :ui (:label "Stage" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :position
        (:type :integer :sortable t
          :ui (:label "Position" :widget :textbox)
          :source (:view :main :column :position :agg :first)
          :column t :not-null t)
        ;; left out of :list-form on purpose — the fast-path
        ;; eligibility clause that reads it (the legacy path reads
        ;; palette values off a :list-form record)
        :note
        (:type :text
          :ui (:label "Note" :widget :textarea)
          :source (:view :main :column :note :agg :first)
          :column t))
      :list-form (:fields (:name :position))
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

    :labels
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("rc-users")
      :views (:main (:tables (:labels)))
      :fields
      (:title
        (:type :text :identity t
          :ui (:label "Label" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

    :teams
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("rc-users")
      :default-sort (:owner :asc)
      :views (:main (:tables (:teams :users)))
      :fields
      (:name
        (:type :text :identity t :sortable t
          :ui (:label "Team" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :owner
        (:type :text :target :users :sortable t
          :ui (:label "Owner" :widget :search-select)
          :source (:view :main :table :users :column :name :agg :first)
          :source-all (:view :users :table :users :column :name
                       :agg :list)
          :column t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

    :vaults
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("rc-users")
      :views (:main (:tables (:vaults :users) :scope :user)
               :users (:tables (:users) :scope :user))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Vault" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :user
        (:type :text
          :autofill :user
          :force-sql-name "vault_user"
          :ui (:label "Owner" :widget :textbox :read-only t)
          :target :users
          :source (:view :main :table :users :column :name :agg :first)
          :source-all (:view :users :table :users :column :name
                       :agg :list)
          :column t :not-null t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

    :empties
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("rc-users")
      :views (:main (:tables (:empties)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Empty" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

    ;; Cross-source guard targets: a :text display field whose
    ;; :column names another field's stored column. :flag-text
    ;; sources the boolean column, :stamp-text the timestamp
    ;; column — both stay legacy (conversion keyed on the source
    ;; alias's field type).
    :cross
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("rc-users")
      :views (:main (:tables (:cross)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Name" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :flag
        (:type :boolean
          :ui (:label "Flag" :widget :checkbox)
          :source (:view :main :column :flag :agg :first)
          :column t)
        :stamp
        (:type :timestamp
          :ui (:label "Stamp" :widget :textbox :read-only t)
          :source (:view :main :column :stamp :agg :first)
          :column t)
        :flag-text
        (:type :text
          :ui (:label "Flag text" :widget :textbox)
          :source (:view :main :column :flag :agg :first)
          :column t)
        :stamp-text
        (:type :text
          :ui (:label "Stamp text" :widget :textbox)
          :source (:view :main :column :stamp :agg :first)
          :column t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

    :tasks
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("rc-users")
      :views (:main (:tables (:tasks :stages :teams :vaults
                              :task-labels :labels :users)))
      :fields
      (:name
        (:type :text :identity t :sortable t :searchable t
          :ui (:label "Task" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :stage
        (:type :text :target :stages
          :ui (:label "Stage" :widget :select)
          :source (:view :main :table :stages :column :name
                   :agg :first)
          :source-all (:view :main :table :stages :column :name
                       :agg :list)
          :column t)
        :labels
        (:type :list
          :ui (:label "Labels" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :labels :column :title
                   :agg :distinct)
          :source-all (:view :main :table :labels :column :title
                       :agg :list)
          :join-table :task-labels)
        :team
        (:type :text :target :teams
          :ui (:label "Team" :widget :select)
          :source (:view :main :table :teams :column :name
                   :agg :first)
          :source-all (:view :main :table :teams :column :name
                       :agg :list)
          :column t)
        :vault
        (:type :text :target :vaults
          :ui (:label "Vault" :widget :select)
          :source (:view :main :table :vaults :column :name
                   :agg :first)
          :source-all (:view :main :table :vaults :column :name
                       :agg :list)
          :column t)
        :empty
        (:type :text :target :empties
          :ui (:label "Empty" :widget :select)
          :source (:view :main :table :empties :column :name
                   :agg :first)
          :source-all (:view :main :table :empties :column :name
                       :agg :list)
          :column t)
        ;; same target and display column as :empty — the memo's
        ;; empty-palette case (one read for both fields)
        :empty-2
        (:type :text :target :empties
          :ui (:label "Empty 2" :widget :select)
          :source (:view :main :table :empties :column :name
                   :agg :first)
          :source-all (:view :main :table :empties :column :name
                       :agg :list)
          :column t)
        :cross-flag
        (:type :text :target :cross
          :ui (:label "Cross flag" :widget :select)
          :source (:view :main :table :cross :column :flag-text
                   :agg :first)
          :source-all (:view :main :table :cross :column :flag-text
                       :agg :list)
          :column t)
        :cross-stamp
        (:type :text :target :cross
          :ui (:label "Cross stamp" :widget :select)
          :source (:view :main :table :cross :column :stamp-text
                   :agg :first)
          :source-all (:view :main :table :cross :column :stamp-text
                       :agg :list)
          :column t)
        :owner
        (:type :text :target :users
          :ui (:label "Owner" :widget :select)
          :source (:view :main :table :users :column :name
                   :agg :first)
          :source-all (:view :users :table :users :column :name
                       :agg :list)
          :column t)
        ;; same target and display column as :owner — the memo's
        ;; denied-palette case (each denied field logs its own
        ;; suppression) and the shared-read case for admin
        :owner-2
        (:type :text :target :users
          :ui (:label "Owner 2" :widget :select)
          :source (:view :main :table :users :column :name
                   :agg :first)
          :source-all (:view :users :table :users :column :name
                       :agg :list)
          :column t)
        :priority
        (:type :text
          :ui (:label "Priority" :widget :select
            :options ("low" "high")))
        :created-at
        (:ui (:label "Created" :widget :textbox :read-only t)))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

    :task-labels
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :tasks)
        :reference (:target :labels)))

    ;; the whole point: users hidden from non-admins (the
    ;; allowed-values-test shape — :owner palette denial)
    :users (:type-roles ("admin"))))
