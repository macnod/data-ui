;; 09 test fixture: sort + paging on measure Phase A output.
;; Sibling of measure-rollup-test's shapes (:chores / :users), with
;; :sortable t on pass-through and measure fields. This fixture owns
;; the :sortable declarations (plan 10's rollup-test re-proves the
;; same behavior later; measure-rollup-test stays no-sortable).
'(:title "Rollup Sort Test"
   :name "rollup-sort-test"
   :version "0.1"
   :domain "rollup-sort.test.data-ui.com"
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
                              :column t :not-null t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     ;; Leaderboard with sortable pass-through + :sum / :count
     ;; measures. First sortable measure in declaration order is
     ;; :total-points (the default ranking).
     :user-leaderboard
     (:rollup t
       :grain :users
       :type-roles ("leaderboard-viewers")
       :filter ((:chores :completed :eq t))
       :views (:main (:tables (:users :chores)))
       :list-form (:fields t)
       :fields
       ((:name (:source (:view :main :table :users :column :name :agg :first)
                 :sortable t
                 :ui (:label "User")))
         (:total-points (:type :integer
                          :source (:view :main :table :chores :column :points
                                    :agg :sum)
                          :sortable t
                          :ui (:label "Points")))
         (:chores-done (:type :integer
                         :source (:view :main :table :chores :column :id
                                   :agg :count)
                         :sortable t
                         :ui (:label "Completed")))))

     ;; Sortable :avg (NULLS LAST path) plus unsortable :list /
     ;; :distinct measures. First sortable measure is :avg-points.
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
         (:avg-points (:type :real
                        :source (:view :main :table :chores :column :points
                                  :agg :avg)
                        :sortable t
                        :ui (:label "Avg")))
         (:distinct-points (:type :integer
                             :source (:view :main :table :chores :column
                                       :points :agg :distinct)
                             :ui (:label "Point Values")))))

     ;; No sortable measure anywhere: nil sort must fall back to grain
     ;; id ASC only and the response :sort must be null.
     :bare-board
     (:rollup t
       :grain :users
       :type-roles ("leaderboard-viewers")
       :views (:main (:tables (:users :chores)))
       :list-form (:fields t)
       :fields
       ((:name (:source (:view :main :table :users :column :name :agg :first)
                 :sortable t
                 :ui (:label "User")))
         (:total-points (:type :integer
                          :source (:view :main :table :chores :column :points
                                    :agg :sum)
                          :ui (:label "Points")))))

     ;; Hybrid shape for the leak test: a base type with a Phase B
     ;; aggregated field. :sortable t on :avg-rating must stay a
     ;; compile error (Issue 12).
     :ratings
     (:table t
       :create :auto :update :auto :delete :auto
       :type-roles ("rating-user")
       :views (:main (:tables (:ratings :users)))
       :fields (:value (:type :integer
                         :ui (:label "Value" :widget :textbox)
                         :source (:view :main :column :value :agg :first)
                         :column t :not-null t)
                 :user (:type :text
                         :ui (:label "User" :widget :textbox)
                         :target :users
                         :source (:view :main :table :users :column :name
                                   :agg :first)
                         :column t :not-null t)
                 :avg-rating (:type :real
                               :source (:view :main :table :ratings
                                         :column :value :agg :avg)
                               :ui (:label "Avg Rating")))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))))

