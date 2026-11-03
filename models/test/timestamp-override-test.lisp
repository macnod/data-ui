;; Purpose: Timestamp presentation-override fixture — :posts uses the
;;   blog spellings (:created-at full :ui override, :updated-at
;;   label-only, :default-sort (:created-at :desc), update form present
;;   so be-update's rec path works), :notes is the no-override control
;;   that names both timestamps in its list form (the :ui-gate-closed
;;   regression, non-vacuous).
;; Author: Rose (via implement-data-ui-plan)
;; Created: 2026-11-18
;; Prompt: (see ~/workbench/timestamp-field-overrides-plan.org)
'(:title "Timestamp Override Test"
  :name "timestamp-override-test"
  :version "0.1"
  :domain "timestamp-override-test.demo.data-ui.com"
  :repl t
  :landing-page :posts
  :types
  (:posts
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:posts)))
      :default-sort (:created-at :desc)
      :fields
      (:title
        (:type :text :identity t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t)
       :body
        (:type :text
          :ui (:label "Body" :widget :textarea)
          :source (:view :main :column :body :agg :first)
          :column t)
       :created-at
        (:ui (:label "Posted" :widget :textbox :read-only t))
       :updated-at
        (:ui (:label "Updated" :read-only t)))
      :list-form (:fields (:title :created-at :updated-at))
      :update-form (:fields t)
      :add-form (:fields t))

   :notes
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:notes)))
      :fields
      (:note
        (:type :text :identity t
          :ui (:label "Note" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :note :agg :first)
          :column t :not-null t))
      :list-form (:fields (:note :created-at :updated-at))
      :update-form (:fields t)
      :add-form (:fields t))))
