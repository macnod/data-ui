'(:title "Guest Allowed Test"
  :name "guest-allowed-test"
  :version "0.1"
  :domain "guest-allowed-test.demo.data-ui.com"
  ;; WARNING: :repl must be nil in production
  :repl t
  :landing-page :widgets
  ;; Passwordless guest login: /api/login accepts any password for
  ;; the seeded guest user, who carries only the read-only public
  ;; role. Guest still needs read access on a type's resource to
  ;; see anything (widgets-user below is not a guest role).
  :guest-allowed t
  :types
  (:widgets
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("widgets-user")
      :fields
      (:name
        (:type :text :identity t :searchable t
          :ui (:label "Name" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))))
