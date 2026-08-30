;; Recurring chore tracker
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
'(:title "Recurring Chores"
  :name "recurring"
  :version "0.1"
  :domain "recurring.demo.data-ui.com"
  :repl t
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
      :fields
      (:name
        (:type :text :sortable t
          :ui (:label "Chore" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t)
        :instance-id
        (:type :uuid :identity t :default :generate-uuid
          :ui (:label "Instance" :widget :textbox :read-only t)
          :source (:view :main :column :instance-id :agg :first)
          :column t :not-null t)
        :description
        (:type :text :default ""
          :ui (:label "Description" :widget :textarea)
          :source (:view :main :column :description :agg :first)
          :column t)
        :points
        (:type :integer :default 1 :sortable t
          :ui (:label "Points" :widget :textbox)
          :validations (:required (:in-range :min 1 :max 4))
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
          :ui (:label "Done" :widget :checkbox)
          :source (:view :main :column :completed :agg :first)
          :column t :not-null t)
        :completed-at
        (:type :timestamp :sorted-at t
          :ui (:label "Completed At" :widget :textbox)
          :source (:view :main :column :completed-at :agg :first)
          :column t)
        :completed-by
        (:type :list
          :ui (:label "Completed By" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :users :column :name :agg :distinct)
          :source-all (:view :users :table :users :column :name :agg :list)
          :join-table :chore-users)
        :complete
        (:type :button
          :ui (:label "Complete" :widget :button)
          :action (:spawn
                    :close (:completed :true
                            :completed-at :now
                            :completed-by :user)
                    :clear (:notes :instance-id))))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :tags
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("chore-users")
      :fields
      (:name
        (:type :text :identity t
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
