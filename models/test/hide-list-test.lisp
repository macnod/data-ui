;; Purpose: Hide-list fixture — :posts declares :hide-list on its
;;   three record forms (t on view/add, an explicit nil on update —
;;   the present-nil pin and the disagree-forms case), :drafts
;;   declares nothing (the byte-safety and omitted-means-show
;;   control). The body field carries :searchable t so the browser
;;   pass can see the search row disappear under an open form. The
;;   seeder (hl-seed-fixture) lives in tests/hide-list-tests.lisp
;;   and must run after set-model — set-model seeds nothing.
;; Author: Rose (via implement-data-ui-plan)
;; Created: 2026-10-26
;; Prompt: (see ~/workbench/hide-list-plan.org)
'(:title "Hide List Test"
  :name "hide-list-test"
  :version "0.1"
  :domain "hide-list-test.demo.data-ui.com"
  :repl t
  :landing-page :posts
  :types
  (:posts
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:posts)))
      :fields
      (:title
        (:type :text :identity t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t)
       :body
       (:type :text :searchable t
         :ui (:label "Body" :widget :textarea)
         :source (:view :main :column :body :agg :first)
         :column t))
      :list-form (:fields (:title))
      :view-form (:fields t :hide-list t)
      :update-form (:fields t :hide-list nil)
      :add-form (:fields t :hide-list t))

   :drafts
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:drafts)))
      :fields
      (:title
        (:type :text :identity t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t)
       :body
       (:type :text :searchable t
         :ui (:label "Body" :widget :textarea)
         :source (:view :main :column :body :agg :first)
         :column t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))))
