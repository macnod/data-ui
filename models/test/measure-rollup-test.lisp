;; 08b test fixture: measure Phase A SQL generator + be-list branch.
;; No :sortable t anywhere (plan 04's :column t check still rejects it on
;; rollup fields until 09 relaxes it). Reuses the 08a fixture shapes
;; (:chores / :tasks / :task-notes / :widget-lists / :widgets /
;; :plain-things) plus rollups:
;;   :user-leaderboard — 2-table, model-declared fact filter + grain filter
;;   :titles-board     — :list / :avg / :distinct measures, unfiltered list
;;   :note-summary     — 3-table chain, F on the leaf, middle-table filter
;;   :my-widgets       — non-:users grain with :scope :user
'(:title "Measure Rollup Test"
   :name "measure-rollup-test"
   :version "0.1"
   :domain "measure.test.data-ui.com"
   :repl nil
   :types
   (:chores
     (:table t
       :create :auto :update :auto :delete :auto
       :type-roles ("chore-user")
       :views (:main (:tables (:chores :users)))
       :fields (:title (:type :text :identity t
                         :ui (:label "Title" :widget :textbox)
                         :source (:view :main :column :title :agg :first)
                         :column t :not-null t)
                 :user (:type :text
                         :ui (:label "User" :widget :textbox)
                         :target :users
                         :source (:view :main :table :users :column :name
                                   :agg :first)
                         :column t :not-null t)
                 :points (:type :integer
                           :ui (:label "Points" :widget :textbox)
                           :source (:view :main :column :points :agg :first)
                           :column t :not-null t)
                 :completed (:type :boolean
                              :css-value t
                              :ui (:label "Completed" :widget :checkbox)
                              :source (:view :main :column :completed
                                        :agg :first)
                              :column t :not-null t)
                 :completed-at (:type :timestamp
                                 :ui (:label "Completed At" :widget :textbox)
                                 :source (:view :main :column :completed-at
                                           :agg :first)
                                 :column t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :user-leaderboard
     (:rollup t
       :grain :users
       :type-roles ("leaderboard-viewers")
       :filter ((:chores :completed :eq t))
       :views (:main (:tables (:users :chores)))
       :list-form (:fields t)
       :fields
       ((:name (:source (:view :main :table :users :column :name :agg :first)
                 :ui (:label "User")))
         (:total-points (:type :integer
                          :source (:view :main :table :chores :column :points
                                    :agg :sum)
                          :ui (:label "Points")))
         (:chores-done (:type :integer
                         :source (:view :main :table :chores :column :id
                                   :agg :count)
                         :ui (:label "Completed")))))

     ;; :list / :avg / :distinct measures; no model filter (tests the
     ;; unfiltered PK guard and zero-fact / :avg NULL behavior).
     :titles-board
     (:rollup t
       :grain :users
       :type-roles ("leaderboard-viewers")
       :views (:main (:tables (:users :chores)))
       :list-form (:fields t)
       :fields
       ((:name (:source (:view :main :table :users :column :name :agg :first)
                 :ui (:label "User")))
         (:titles (:type :text
                    :source (:view :main :table :chores :column :title
                              :agg :list)
                    :ui (:label "Titles")))
         (:distinct-points (:type :integer
                             :source (:view :main :table :chores :column
                                       :points :agg :distinct)
                             :ui (:label "Point Values")))
         (:avg-points (:type :real
                        :source (:view :main :table :chores :column :points
                                  :agg :avg)
                        :ui (:label "Avg")))))

     ;; Grain-table model filter → runtime WHERE, not FILTER.
     :grain-filtered-board
     (:rollup t
       :grain :users
       :type-roles ("leaderboard-viewers")
       :filter ((:users :name :eq "alice"))
       :views (:main (:tables (:users :chores)))
       :list-form (:fields t)
       :fields
       ((:name (:source (:view :main :table :users :column :name :agg :first)
                 :ui (:label "User")))
         (:total-points (:type :integer
                          :source (:view :main :table :chores :column :points
                                    :agg :sum)
                          :ui (:label "Points")))))

     :tasks
     (:table t
       :create :auto :update :auto :delete :auto
       :type-roles ("task-user")
       :views (:main (:tables (:tasks :users)))
       :fields (:title (:type :text :identity t
                         :ui (:label "Title" :widget :textbox)
                         :source (:view :main :column :title :agg :first)
                         :column t :not-null t)
                 :user (:type :text
                         :ui (:label "User" :widget :textbox)
                         :target :users
                         :source (:view :main :table :users :column :name
                                   :agg :first)
                         :column t :not-null t)
                 :completed (:type :boolean
                              :css-value t
                              :ui (:label "Completed" :widget :checkbox)
                              :source (:view :main :column :completed
                                        :agg :first)
                              :column t :not-null t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :task-notes
     (:table t
       :create :auto :update :auto :delete :auto
       :type-roles ("note-user")
       :views (:main (:tables (:task-notes :tasks)))
       :fields (:note (:type :text
                        :ui (:label "Note" :widget :textarea)
                        :source (:view :main :column :note :agg :first)
                        :column t :not-null t)
                 :task (:type :text
                         :ui (:label "Task" :widget :textbox)
                         :target :tasks
                         :source (:view :main :table :tasks :column :title
                                   :agg :first)
                         :column t :not-null t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     ;; Issue 17 hop-binding: filter names the middle table :tasks; the
     ;; fact table is :task-notes.
     :note-summary
     (:rollup t
       :grain :users
       :type-roles ("note-viewers")
       :filter ((:tasks :completed :eq t))
       :views (:main (:tables (:users :tasks :task-notes)))
       :list-form (:fields t)
       :fields
       ((:name (:source (:view :main :table :users :column :name :agg :first)
                 :ui (:label "User")))
         (:notes-on-done (:type :integer
                           :source (:view :main :table :task-notes :column :id
                                     :agg :count)
                           :ui (:label "Notes on Done")))))

     ;; Non-:users grain with :scope :user. Grain :user columns store the
     ;; username string; the scope predicate binds <grain>.<user-col> =
     ;; <username>, not the UUID.
     :widget-lists
     (:table t
       :create :auto :update :auto :delete :auto
       :type-roles ("widget-user")
       :views (:main (:tables (:widget-lists :users)))
       :fields (:title (:type :text :identity t
                         :ui (:label "Title" :widget :textbox)
                         :source (:view :main :column :title :agg :first)
                         :column t :not-null t)
                 :user (:type :text
                         :ui (:label "User" :widget :textbox)
                         :target :users
                         :source (:view :main :table :users :column :name
                                   :agg :first)
                         :column t :not-null t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :widgets
     (:table t
       :create :auto :update :auto :delete :auto
       :type-roles ("widget-user")
       :views (:main (:tables (:widgets :widget-lists)))
       :fields (:name (:type :text
                        :ui (:label "Name" :widget :textbox)
                        :source (:view :main :column :name :agg :first)
                        :column t :not-null t)
                 :list (:type :text
                         :ui (:label "List" :widget :textbox)
                         :target :widget-lists
                         :source (:view :main :table :widget-lists
                                   :column :title :agg :first)
                         :column t :not-null t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :my-widgets
     (:rollup t
       :grain :widget-lists
       :type-roles ("widget-viewers")
       :views (:main (:tables (:widget-lists :widgets) :scope :user))
       :list-form (:fields t)
       :fields
       ((:title (:source (:view :main :table :widget-lists :column :title
                           :agg :first)
                  :ui (:label "Title")))
         (:widget-count (:type :integer
                          :source (:view :main :table :widgets :column :id
                                    :agg :count)
                          :ui (:label "Widgets")))))

     ;; :users grain with :scope :user — the scoped caller sees one row
     ;; (their own), bound by the users.id UUID.
     :my-stats
     (:rollup t
       :grain :users
       :type-roles ("leaderboard-viewers")
       :views (:main (:tables (:users :chores) :scope :user))
       :list-form (:fields t)
       :fields
       ((:name (:source (:view :main :table :users :column :name :agg :first)
                 :ui (:label "User")))
         (:chore-count (:type :integer
                         :source (:view :main :table :chores :column :id
                                   :agg :count)
                         :ui (:label "Chores")))))))
