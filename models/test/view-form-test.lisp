;; Purpose: View-form fixture — a dedicated reading surface (:posts
;;   declares :view-form) beside the absent case (:notes declares
;;   none), plus a button whose wire exclusion and status-companion
;;   rules the suite probes. The button reuses :test-sync, registered
;;   by tests/action-tests.lisp at load time (same pattern as
;;   test-model's :test-action); load that file before set-model.
;; Author: Rose (via implement-data-ui-plan)
;; Created: 2026-10-22
;; Prompt: (see ~/workbench/view-form-plan.org)
'(:title "View Form Test"
  :name "view-form-test"
  :version "0.1"
  :domain "view-form-test.demo.data-ui.com"
  :repl t
  :landing-page :posts
  :types
  (:posts
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("vf-users")
      :views (:main (:tables (:posts)))
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
       :test-action
       (:type :button
         :ui (:label "Test Action" :widget :button)
         :action (:test-sync)))
      :list-form (:fields (:title))
      :update-form (:fields (:title :body :test-action))
      :view-form (:fields (:title :body))
      :add-form (:fields (:title :body)))

   :notes
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("vf-users")
      :views (:main (:tables (:notes)))
      :fields
      (:title
        (:type :text :identity t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t))
      :list-form (:fields t)
      :update-form (:fields t)
      :add-form (:fields t))))
