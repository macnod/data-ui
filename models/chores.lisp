;; Purpose: Home chore tracker. Chores have points (1-4 difficulty),
;;   a description, a completed flag, and a completed-by field (M2M to
;;   users) for attributing completion. A scores table tracks per-user
;;   point totals and chore counts. Scores are currently manual;
;;   auto-aggregation requires lifecycle data-effect hooks (not yet
;;   implemented — see data-ui-todo.org).
;;
;;   Tags were removed from this model due to a compiler bug: types
;;   with multiple M2M joiners produce incorrect insert SQL. See
;;   data-ui-todo.org ("Fix join-table insert SQL generation").
;;   Tags will be reattached when that bug is fixed.
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
;;   scores table that tracks the number of points and the number of
;;   chores that each user has accumulated. So the fields for this
;;   table would be user, chore-count, and total points.
'(:title "Home Chores"
  :name "chores"
  :version "0.2"
  :domain "chores.demo.data-ui.com"
  :repl t
  :landing-page :chores
  :types
  (:chores
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("chore-users")
      :views (:main (:tables (:chores :chore-users :users)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Chore" :widget :line)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :description
        (:type :text :default ""
          :ui (:label "Description" :widget :textbox)
          :source (:view :main :column :description :agg :first)
          :column t)
        :points
        (:type :integer :default 1
          :ui (:label "Points" :widget :line)
          :validations (:required (:in-range :min 1 :max 4))
          :source (:view :main :column :points :agg :first)
          :column t :not-null t)
        :completed
        (:type :boolean :default :false
          :ui (:label "Done" :widget :checkbox)
          :source (:view :main :column :completed :agg :first)
          :column t :not-null t)
        :completed-by
        (:type :list
          :ui (:label "Completed By" :widget :checkbox-list)
          :validations (:join-items-exist)
          :source (:view :main :table :users :column :name :agg :list)
          :source-all (:view :users :table :users :column :name :agg :list)
          :join-table :chore-users))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :scores
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("chore-users")
      :views (:main (:tables (:scores :users)))
      :fields
      (:user
        (:type :text
          :target :users
          :identity t
          :ui (:label "User" :widget :select)
          :source (:view :main :table :users :column :name :agg :first)
          :source-all (:view :users :table :users :column :name :agg :list)
          :column t :not-null t)
        :chore-count
        (:type :integer :default 0
          :ui (:label "Chores Done" :widget :line)
          :validations (:required)
          :source (:view :main :column :chore-count :agg :first)
          :column t :not-null t)
        :total-points
        (:type :integer :default 0
          :ui (:label "Total Points" :widget :line)
          :validations (:required)
          :source (:view :main :column :total-points :agg :first)
          :column t :not-null t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :chore-users
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :chores)
        :reference (:target :users)))))
