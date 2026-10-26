'(:title "Page Description Test"
  :name "page-description-test"
  :version "0.1"
  :domain "page-description-test.demo.data-ui.com"
  :repl t
  :landing-page :notes
  :types
  (:notes
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("pd-users")
      :views (:main (:tables (:notes :users)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Name" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t)
       :user
       (:type :text
         :ui (:label "User" :widget :textbox)
         :target :users
         :source (:view :main :table :users :column :name
                   :agg :first)
         :column t :not-null t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t)
      :page-description "These are the **notes**."
      :page-description-open t)

    ;; Explicit-nil companion: the wire layer's "serializes false,
    ;; never dropped" subject.
    :collapsed
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("pd-users")
      :views (:main (:tables (:collapsed)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Name" :widget :textbox)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t)
      :page-description "Collapsed by default."
      :page-description-open nil)

    ;; Minimal-payload probe: neither key, nothing on the wire.
    :plain
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("pd-users")
      :views (:main (:tables (:plain)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Name" :widget :textbox)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

    ;; Rollup path, deliberately display-less: the rollup overlay
    ;; defaults :display t, so the board reaches be-types and the
    ;; all-models gate proves a rollup omitting :display carries
    ;; the key.
    :board
    (:rollup t
      :grain :users
      :type-roles ("pd-users")
      :views (:main (:tables (:users :notes)))
      :list-form (:fields t)
      :page-description "Counts per user."
      :fields
      ((:name (:source (:view :main :table :users :column :name
                          :agg :first)
                 :ui (:label "User")))
        (:notes-count (:type :integer
                          :source (:view :main :table :notes
                                    :column :id :agg :count)
                          :ui (:label "Note Rows")))))))
