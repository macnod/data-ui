;; Test fixture for the reference chip picker (:filter-with
;; :reference). Two user types: :projects (:title identity — pins
;; the non-:name identity case) declared BEFORE :tasks (fixture
;; order is load-bearing: create-tables walks author order and
;; rt_tasks's task_project FK references rt_projects — a referrer
;; declared first dies with 42P01), and :tasks with two FK fields
;; (:assigned-to → :users, :project → :projects), both carrying
;; :filter-with :reference. Both types :type-roles ("logged-in")
;; so a logged-in reader gets readable targets and palettes. The
;; identity fields spell :source (:view :main :column <author-key>
;; :agg :first) — nullable-fk-test's convention; on a user type
;; whose identity key differs from its name-sql, the spelled author
;; key is the one value that compiles.
'(:title "Reference Chip Test"
  :name "reference-chip-test"
  :version "0.1"
  :domain "reference-chip-test.demo.data-ui.com"
  :repl nil
  :types
  (:projects
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:projects)))
      :fields
      (:title
        (:type :text :identity t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))
  :tasks
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:tasks :users :projects)))
      :fields
      (:name
        (:type :text :identity t :sortable t
          :ui (:label "Task" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :assigned-to
        (:type :text :target :users
          :ui (:label "Assigned To" :widget :search-select
                :filter-with :reference)
          :source (:view :main :table :users :column :name
                   :agg :first)
          :source-all (:view :main :table :users :column :name
                       :agg :list)
          :column t)
        :project
        (:type :text :target :projects
          :ui (:label "Project" :widget :search-select
                :filter-with :reference)
          :source (:view :main :table :projects :column :title
                   :agg :first)
          :source-all (:view :main :table :projects :column :title
                       :agg :list)
          :column t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))))
