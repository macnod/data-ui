;; Purpose: Home chore tracker. Chores have points (1-4 difficulty),
;;   a description, a completed flag, tags, and a completed-by field
;;   (M2M to users) for attributing completion. A scoreboard rollup
;;   derives per-user point totals and chore counts at read time
;;   from completed chores; nothing is stored or maintained by hand.
;; Author: Rose (via Data UI)
;; Created: 2025-07-10
;; Prompt: Create a web app that tracks chores in the home, and who
;;   completed a chore. A chore should have a name (identity), points
;;   (a value between 1 and 4 that describes how difficult or
;;   time-consuming the chore is), tags, a completed checkbox, and a
;;   user field that allows the chore to be attributed to the user
;;   that completed the chore. That field should be empty initially,
;;   until the chore is completed. Chores should be visible and
;;   editable by all users. A tag has a name field (identity) and
;;   that's it. In addition to chores and tags, there should be a
;;   scoreboard that shows the number of points and the number of
;;   chores each user has accumulated, derived from completed chores.
'(:title "Home Chores"
  :name "chores"
  :version "0.4"
  :domain "chores.demo.data-ui.com"
  :domain-stg "chores-stg.demo.data-ui.com"
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
        (:type :text :identity t :sortable t
          :ui (:label "Chore" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :description
        (:type :text :default ""
          :ui (:label "Description" :widget :textarea)
          :source (:view :main :column :description :agg :first)
          :column t)
        :points
        (:type :integer :default 1
          :ui (:label "Points" :widget :textbox)
          :validations (:required (:in-range :min 1 :max 4))
          :source (:view :main :column :points :agg :first)
          :column t :not-null t)
        :completed
        (:type :boolean :default :false
          :ui (:label "Done" :widget :checkbox)
          :source (:view :main :column :completed :agg :first)
          :column t :not-null t)
        :tags
        (:type :list
          :ui (:label "Tags" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :tags :column :name :agg :distinct)
          :source-all (:view :tags :table :tags :column :name :agg :list)
          :join-table :chore-tags)
        :completed-by
        (:type :list
          :ui (:label "Completed By" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :users :column :name :agg :distinct)
          :source-all (:view :users :table :users :column :name :agg :list)
          :join-table :chore-users))
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
