'(:title "Allowed Values Test"
  :name "allowed-values-test"
  :version "0.1"
  :domain "allowed-values-test.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  :landing-page :items
  :types
  (:items
    (:table t
      :create :auto :update :auto :delete :auto :display t
      ;; Author types default to :type-roles ("admin") — without
      ;; this, a non-admin fails the OUTER gate on :items, not the
      ;; palette gate, and the palette tests prove nothing.
      :type-roles ("logged-in")
      ;; Load-bearing, not decoration: the :user field's :source
      ;; names (:view :main :table :users ...), and a :main view
      ;; that does not list :users does not compile. Same shape as
      ;; modelbank :models.
      :views (:main (:tables (:items :users)))
      :fields
      (:name
        (:type :text :identity t
          :ui (:label "Name" :widget :textbox)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t)
        :user
        (:type :text
          :autofill :user
          :force-sql-name "item_user"
          :ui (:label "Owner" :widget :textbox :read-only t)
          :target :users
          :source (:view :main :table :users :column :name :agg :first)
          :column t :not-null t)
        :rating
        ;; :text, not :integer — mirrors static-select-test
        ;; :priority. :ui :options compares with #'equal against the
        ;; literal list, and every option list in the codebase is
        ;; strings; an :integer field here would fail v-options the
        ;; moment full-data fills :default 0 (0 is not a member of
        ;; ("1" "2" "3")).
        (:type :text :default "1"
          :ui (:label "Rating" :widget :select
               :options ("1" "2" "3"))
          :source (:view :main :column :rating :agg :first)
          :column t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

    ;; the whole point: users hidden from non-admins
    :users (:type-roles ("admin"))))
