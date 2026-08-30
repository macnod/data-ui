;; Test fixture for the 08a rollup compile surface.
;; No :sortable t anywhere: plan 04's :column t check still
;; rejects it on rollup fields until 09 relaxes it.
;; Compile-only (validate-model / compile-model). Do not call
;; be-list on :user-leaderboard until 08b lands.

'(:title "Rollup Test"
  :name "rollup-test"
  :version "0.1"
  :domain "rollup.test.data-ui.com"
  :repl nil
  :types
  (:chores
  (:table t
    :create :auto :update :auto :delete :auto
    :type-roles ("chore-user")
    :views (:main (:tables (:chores :users)))
    :fields (:title (:type :text
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

  ;; Issue 17 hop-binding: filter names the intermediate table
  ;; :tasks; the fact table is :task-notes.
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

  ;; Scope tests: :widget-lists has a :user field; :widgets is the
  ;; fact table under it.
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

  ;; A table with no :user field, for the negative :scope test.
  :plain-things
  (:table t
    :create :auto :update :auto :delete :auto
    :type-roles ("plain-user")
    :views (:main (:tables (:plain-things)))
    :fields (:name (:type :text
                    :ui (:label "Name" :widget :textbox)
                    :source (:view :main :column :name :agg :first)
                    :column t :not-null t)
             :size (:type :integer
                    :ui (:label "Size" :widget :textbox)
                    :source (:view :main :column :size :agg :first)
                    :column t :not-null t))
    :list-form (:fields t)
    :update-form (:fields t)
    :add-form (:fields t))))

