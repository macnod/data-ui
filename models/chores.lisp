;; Home chore tracker
;; 
;; This is the :spawn pattern pilot demo. Completing a chore leaves durable
;; history and produces a fresh open instance in one click: the :complete
;; button's :spawn action closes the row (completed / completed-at /
;; completed-by written to the old row) and inserts a successor with definition
;; fields (name, points, tags) copied and state (notes) reset. A scoreboard
;; rollup derives per-user point totals from completed chores. Instance :name is
;; plain non-unique text: a spawned successor legitimately repeats its name (an
;; :identity t name would collide on the second spawn).
;;
'(:title "Home Chores"
   :name "chores"
   :version "0.5"
   :domain "chores.demo.data-ui.com"
   :domain-stg "chores-stg.demo.data-ui.com"
   :repl t :guest-allowed t :guest-auto nil
   :landing-page :chores
   :types
   (:chores
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :type-roles ("chore-users")
       :views (:main (:tables (:chores :chore-tags :tags
                                :chore-users :users))
                :tags (:tables (:tags))
                :users (:tables (:users)))
       :default-sort (:name :asc)
       :fields
       (:name
         (:type :text :sortable t :searchable t
           :ui (:label "Chore" :widget :textbox)
           :validations (:required)
           :source (:view :main :column :name :agg :first)
           :column t :not-null t)
         :complete
         (:type :button
           :ui (:label "Complete" :widget :button)
           :action (:spawn
                     :close (:completed :true
                              :completed-at :now
                              :completed-by :user)
                     :clear (:notes :instance-id)))
         :instance-id
         (:type :uuid :identity t :default :generate-uuid
           :ui (:label "Instance" :widget :textbox :read-only t)
           :source (:view :main :column :instance-id :agg :first)
           :column t :not-null t)
         :description
         (:type :text :default "" :searchable t
           :ui (:label "Description" :widget :textarea)
           :source (:view :main :column :description :agg :first)
           :column t)
         :points
         (:type :integer :default 1 :sortable t
           :ui (:label "Points" :widget :textbox)
           :validations (:required (:in-range :min 1 :max 10))
           :source (:view :main :column :points :agg :first)
           :column t :not-null t)
         :tags
         (:type :list
           :ui (:label "Tags" :widget :checkbox-list)
           :validations (:join-items-exist)
           :source (:view :main :table :tags :column :name :agg :distinct)
           :source-all (:view :tags :table :tags :column :name :agg :list)
           :join-table :chore-tags)
         :notes
         (:type :text :default ""
           :ui (:label "Notes" :widget :textarea)
           :source (:view :main :column :notes :agg :first)
           :column t)
         :completed
         (:type :boolean :default :false :sortable t
           :ui (:label "Done" :widget :checkbox
                :filter-with (:kind :boolean :default :false))
           :source (:view :main :column :completed :agg :first)
           :column t :not-null t)
         :completed-at
         (:type :timestamp :sortable t
           :ui (:label "Completed At" :widget :textbox)
           :source (:view :main :column :completed-at :agg :first)
           :column t)
         :completed-by
         (:type :list
           :ui (:label "Completed By" :widget :checkbox-list)
           :validations (:join-items-exist)
           :source (:view :main :table :users :column :name :agg :distinct)
           :source-all (:view :users :table :users :column :name :agg :list)
           :join-table :chore-users))
       ;; completed / completed-at / completed-by stay in :list-form
       ;; (durable history, scoreboard reads them) but never appear on
       ;; forms: they are owned by the :complete button's :spawn — the
       ;; single write path for closure. :complete stays listed on
       ;; :update-form because :complete-status auto-appends to it
       ;; (augment-update-form rides on the button field).
       :list-form (:fields (:name :description :points :tags
                             :completed :completed-at :completed-by))
       :update-form (:fields (:name :instance-id :description :points :tags
                               :notes :complete :completed :completed-at
                               :completed-by))
       :add-form (:fields (:name :description :points :tags :notes)))

     :tags
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :type-roles ("chore-users")
       :default-sort (:name :asc)
       :fields
       (:name
         (:type :text :identity t :sortable t :searchable t
           :ui (:label "Tag" :widget :textbox)
           :validations (:required)
           :source (:view :main :table :tags :column :name :agg :first)
           :column t :not-null t :unique t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :scoreboard
     (:rollup t
       :grain :users
       :type-roles ("chore-users")
       :filter ((:chores :completed :eq t))
       :views (:main (:tables (:users :chore-users :chores)))
       :list-form (:fields t)
       :fields
       ((:name (:source (:view :main :table :users :column :name :agg :first)
                 :sortable t
                 :ui (:label "User")))
         (:total-points (:type :integer
                          :source (:view :main :table :chores :column :points :agg :sum)
                          :sortable t
                          :ui (:label "Points")))
         (:chores-done (:type :integer
                         :source (:view :main :table :chores :column :id :agg :count)
                         :sortable t
                         :ui (:label "Done")))))

     :chore-tags
     (:table t :is-joiner t :internal t
       :fields
       (:reference (:target :chores)
         :reference (:target :tags)))

     :chore-users
     (:table t :is-joiner t :internal t
       :fields
       (:reference (:target :chores)
         :reference (:target :users)))))
