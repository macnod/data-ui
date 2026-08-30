;; Test fixture for measure Phase A (analytical rollups)
;; Exercises: grain, measures, filters, sort, pagination,
;; :avg, time-window (:last-days), 3-table chain join,
;; intermediate-path filter. Single file, no mid-suite edits.
'(:title "Rollup Test"
  :name "rollup-test"
  :version "0.1"
  :domain "rollup.test.data-ui.com"
  :repl nil
  :types
  (:tasks
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("task-user")
      :views (:main (:tables (:tasks :users)))
      :fields (:name (:type :text :identity t
                       :ui (:label "Task" :widget :textbox)
                       :source (:view :main :column :name :agg :first)
                       :column t :not-null t)
        :assignee (:type :text
                   :force-sql-name "task_assignee"
                   :ui (:label "Assignee" :widget :select)
                   :target :users
                   :source (:view :main :table :users
                             :column :name :agg :first)
                   :column t :not-null t)
        :points (:type :integer
                 :ui (:label "Points" :widget :textbox)
                 :source (:view :main :column :points :agg :first)
                 :column t :not-null t)
        :completed (:type :boolean :default :false
                    :ui (:label "Done" :widget :checkbox)
                    :source (:view :main :column :completed :agg :first)
                    :column t :not-null t)
        :completed-at (:type :timestamp
                      :ui (:label "Completed At" :widget :textbox)
                      :source (:view :main :column :completed-at
                                :agg :first)
                      :column t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    ;; Test 13/13b: leaf fact table two hops from grain,
    ;; with an intermediate-path filter (Issue 17).
    :task-notes
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("task-user")
      :views (:main (:tables (:task-notes :tasks)))
      :fields (:body (:type :text
              :ui (:label "Note" :widget :textarea)
              :source (:view :main :column :body :agg :first)
              :column t :not-null t)
        :task (:type :text
               :force-sql-name "note_task"
               :ui (:label "Task" :widget :select)
               :target :tasks
               :source (:view :main :table :tasks
                         :column :name :agg :first)
               :column t :not-null t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    ;; Tests 1-9, 14, 16: the board. SUM + COUNT over completed
    ;; tasks (FILTER, not WHERE), AVG incl. zero-fact NULL.
    :task-summary
    (:rollup t
      :grain :users
      :type-roles ("rollup-viewers")
      :filter ((:tasks :completed :eq t))
      :views (:main (:tables (:users :tasks)))
      :list-form (:fields t)
      :fields
      ((:name (:source (:view :main :table :users
                          :column :name :agg :first)
                 :sortable t
                 :ui (:label "User")))
        (:total-points (:type :integer
                        :source (:view :main :table :tasks
                                  :column :points :agg :sum)
                        :sortable t
                        :ui (:label "Points")))
        (:tasks-done (:type :integer
                       :source (:view :main :table :tasks
                                 :column :id :agg :count)
                       :sortable t
                       :ui (:label "Done")))
        (:avg-points (:type :real
                       :source (:view :main :table :tasks
                                 :column :points :agg :avg)
                       :sortable t
                       :ui (:label "Avg")))))

    ;; Test 12: time-window filter. Same shape as task-summary
    ;; plus :last-days. :avg is unsortable here to prove a
    ;; first-sortable-measure default that is not the first
    ;; measure (09 Step 4 walk skips it).
    :task-summary-recent
    (:rollup t
      :grain :users
      :type-roles ("rollup-viewers")
      :filter ((:tasks :completed :eq t)
               (:tasks :completed-at :last-days 7))
      :views (:main (:tables (:users :tasks)))
      :list-form (:fields t)
      :fields
      ((:name (:source (:view :main :table :users
                          :column :name :agg :first)
                 :ui (:label "User")))
        (:avg-points (:type :real
                       :source (:view :main :table :tasks
                                 :column :points :agg :avg)
                       :ui (:label "Avg")))
        (:recent-points (:type :integer
                          :source (:view :main :table :tasks
                                    :column :points :agg :sum)
                          :sortable t
                          :ui (:label "Recent Points")))))

    ;; Tests 13 / 13b: chain join users -> tasks -> task-notes,
    ;; measure on the leaf only (Issue 16), intermediate-path
    ;; filter on :tasks (Issue 17, downstream binding).
    :notes-summary
    (:rollup t
      :grain :users
      :type-roles ("rollup-viewers")
      :filter ((:tasks :completed :eq t))
      :views (:main (:tables (:users :tasks :task-notes)))
      :list-form (:fields t)
      :fields
      ((:name (:source (:view :main :table :users
                          :column :name :agg :first)
                 :sortable t
                 :ui (:label "User")))
        (:notes-on-done (:type :integer
                          :source (:view :main :table :task-notes
                                    :column :id :agg :count)
                          :sortable t
                          :ui (:label "Notes on Done")))))

    ;; 10b: M2M-joiner hop. Isolated from the tasks board.
    ;; Path is users -> award-users -> awards (joiner is the hop).
    :awards
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("task-user")
      :views (:main (:tables (:awards :award-users :users))
               :users (:tables (:users)))
      :fields
      (:name (:type :text :identity t
              :ui (:label "Award" :widget :textbox)
              :source (:view :main :column :name :agg :first)
              :column t :not-null t)
        :points (:type :integer
                 :ui (:label "Points" :widget :textbox)
                 :source (:view :main :column :points :agg :first)
                 :column t :not-null t)
        :completed (:type :boolean :default :false
                    :ui (:label "Done" :widget :checkbox)
                    :source (:view :main :column :completed :agg :first)
                    :column t :not-null t)
        :winners (:type :list
                  :ui (:label "Winners" :widget :checkbox-list)
                  :source (:view :main :table :users :column :name :agg :distinct)
                  :source-all (:view :users :table :users :column :name :agg :list)
                  :join-table :award-users))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))

    :award-users
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :awards)
        :reference (:target :users)))

    :award-board
    (:rollup t
     :grain :users
     :type-roles ("rollup-viewers")
     :filter ((:awards :completed :eq t))
     :views (:main (:tables (:users :award-users :awards)))
     :list-form (:fields t)
     :fields
     ((:name (:source (:view :main :table :users :column :name :agg :first)
             :sortable t
             :ui (:label "User")))
      (:total-points (:type :integer
                  :source (:view :main :table :awards :column :points :agg :sum)
                  :sortable t
                  :ui (:label "Points")))
      (:awards-done (:type :integer
                 :source (:view :main :table :awards :column :id :agg :count)
                 :sortable t
                 :ui (:label "Done")))))))
