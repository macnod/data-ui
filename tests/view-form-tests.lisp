(in-package :data-ui)

(def-suite view-form-compile
  :description ":view-form compile acceptance / rejection probes")

(def-suite view-form-wire
  :description ":view-form wire emission and backend reads on
view-form-test")

(in-suite view-form-compile)

(test forms-has-four-members
  "*forms* gains :view-form in fourth position (the existing three
stay stable for any payload-key-order consumers)."
  (is (equal '(:list-form :add-form :update-form :view-form) *forms*)))

(test view-form-fields-t-compiles
  "(:fields t) on :view-form compiles like any sibling form."
  (let ((compiled
          (getf (compile-model
                  `(:wt
                     (:table t
                       :create :auto :update :auto :delete :auto
                       :type-roles ("wt-users")
                       :views (:main (:tables (:wt)))
                       :fields
                       (:title
                         (:type :text
                           :ui (:label "Title" :widget :textbox)
                           :source (:view :main :column :title
                                     :agg :first)
                           :column t :not-null t))
                       :list-form (:fields t)
                       :update-form (:fields t)
                       :view-form (:fields t)
                       :add-form (:fields t))))
            :wt)))
    (is (equal '(:fields t) (getf compiled :view-form)))))

(test view-form-explicit-list-compiles
  "An explicit (:fields (...)) list on :view-form compiles (the
walker arm probe)."
  (let ((compiled
          (getf (compile-model
                  `(:wt
                     (:table t
                       :create :auto :update :auto :delete :auto
                       :type-roles ("wt-users")
                       :views (:main (:tables (:wt)))
                       :fields
                       (:title
                         (:type :text
                           :ui (:label "Title" :widget :textbox)
                           :source (:view :main :column :title
                                     :agg :first)
                           :column t :not-null t)
                        :body
                        (:type :text
                          :ui (:label "Body" :widget :textarea)
                          :source (:view :main :column :body
                                    :agg :first)
                          :column t))
                       :list-form (:fields t)
                       :update-form (:fields t)
                       :view-form (:fields (:title :body))
                       :add-form (:fields t))))
            :wt)))
    (is (equal '(:fields (:title :body)) (getf compiled :view-form)))))

(test view-form-unknown-field-signals
  "An explicit view-form field list naming an unknown field
signals valid-form-fields with a message naming :view-form."
  (signals error
    (compile-model
      `(:wt
         (:table t
           :create :auto :update :auto :delete :auto
           :type-roles ("wt-users")
           :views (:main (:tables (:wt)))
           :fields
           (:title
             (:type :text
               :ui (:label "Title" :widget :textbox)
               :source (:view :main :column :title :agg :first)
               :column t :not-null t))
           :list-form (:fields t)
           :update-form (:fields t)
           :view-form (:fields (:nope))
           :add-form (:fields t))))))

(test view-form-absent-compiles-without-key
  "A type omitting :view-form compiles and the compiled type-def
carries no :view-form key (payload presence ⇔ declaration)."
  (let ((compiled
          (getf (compile-model
                  `(:wt
                     (:table t
                       :create :auto :update :auto :delete :auto
                       :type-roles ("wt-users")
                       :views (:main (:tables (:wt)))
                       :fields
                       (:title
                         (:type :text
                           :ui (:label "Title" :widget :textbox)
                           :source (:view :main :column :title
                                     :agg :first)
                           :column t :not-null t))
                       :list-form (:fields t)
                       :update-form (:fields t)
                       :add-form (:fields t))))
            :wt)))
    (is-false (member :view-form compiled))))

(test rollup-view-form-signals
  ":view-form on a rollup type signals valid-rollup-matrix."
  (signals error
    (compile-model
      `(:wt
         (:table t
           :create :auto :update :auto :delete :auto
           :type-roles ("wt-users")
           :views (:main (:tables (:wt)))
           :fields
           (:name
             (:type :text :identity t
               :ui (:label "Name" :widget :textbox)
               :source (:view :main :column :name :agg :first)
               :column t :not-null t :unique t))
           :list-form (:fields t)
           :update-form (:fields t)
           :add-form (:fields t))
        :board
        (:rollup t
          :grain :wt
          :type-roles ("wt-users")
          :views (:main (:tables (:wt)))
          :list-form (:fields t)
          :view-form (:fields t)
          :fields
          ((:total (:type :integer
                    :source (:view :main :table :wt :column :id
                             :agg :count)
                    :ui (:label "Total")))))))))

(test rollup-view-form-nil-signals
  "Present-nil :view-form on a rollup signals too (u:has on plist
keys — present-nil is presence)."
  (signals error
    (compile-model
      `(:wt
         (:table t
           :create :auto :update :auto :delete :auto
           :type-roles ("wt-users")
           :views (:main (:tables (:wt)))
           :fields
           (:name
             (:type :text :identity t
               :ui (:label "Name" :widget :textbox)
               :source (:view :main :column :name :agg :first)
               :column t :not-null t :unique t))
           :list-form (:fields t)
           :update-form (:fields t)
           :add-form (:fields t))
        :board
        (:rollup t
          :grain :wt
          :type-roles ("wt-users")
          :views (:main (:tables (:wt)))
          :list-form (:fields t)
          :view-form nil
          :fields
          ((:total (:type :integer
                    :source (:view :main :table :wt :column :id
                             :agg :count)
                    :ui (:label "Total")))))))))

(in-suite view-form-wire)

(test parse-form-accepts-view-form
  "parse-form (the REST form parameter parser, a loop over
*forms*) resolves \"view-form\"."
  (is (eq :view-form (parse-form "view-form"))))

(test fe-fields-view-form-field-keys
  "fe-fields emits :view-form for :posts with exactly the author
fields of the explicit list — no :id (author fields only), no
:test-action (button exclusion), no :test-action-status (explicit
lists get no status auto-append), no synthetic :roles (view form
excluded)."
  (let* ((fields (fe-fields :posts "admin"))
         (view (getf fields :view-form)))
    (is (u:plistp view))
    (is (equal '(:title :body) (u:plist-keys view)))))

(test fe-fields-update-form-keeps-button-and-status
  ":posts's update form still carries the button and the status
companion (the exclusion is view-form only)."
  (let* ((fields (fe-fields :posts "admin"))
         (update (getf fields :update-form)))
    (is (u:has (u:plist-keys update) :test-action))
    (is (u:has (u:plist-keys update) :test-action-status))))

(test fe-fields-undeclared-type-has-no-view-form
  ":notes declares no :view-form: fe-fields has no :view-form key
and the JSON serialization contains no \"view-form\" string (the
byte-identical payload claim, per type)."
  (let ((fields (fe-fields :notes "admin")))
    (is-false (member :view-form fields))
    (is-false
      (search "view-form" (plist-to-json fields)))))

(test show-roles-p-excludes-view-form
  "The synthetic :roles is add/update only: nil on :view-form,
non-nil on :update-form, for a non-base type."
  (is-false (show-roles-p :posts :view-form "admin"))
  (is-true (show-roles-p :posts :update-form "admin")))

(test form-field-keys-cons-id
  "form-field-keys on an explicit view-form list returns the list
verbatim plus the documented leading :id."
  (is (equal '(:id :title :body)
        (form-field-keys :posts :view-form))))

(test fe-fields-users-view-form-present
  "The base model's :users carries :view-form (:fields t)."
  (is (u:plistp (getf (fe-fields :users "admin") :view-form))))

(test be-rec-and-be-list-serve-view-form
  "The load-bearing backend reads: be-rec / be-list with
:form :view-form return records keyed to the view field set
(:id :title :body). The trailing :update / :delete are the
per-record capability flags every record now carries. Inserts
and deletes its own post so the suite stays order-independent."
  (multiple-value-bind (id inserted)
    (be-insert :posts '(:title "vf probe" :body "hello")
      "admin")
    (is-true inserted)
    (unwind-protect
      (progn
        (let ((record (getf (be-rec id "admin"
                             :form :view-form :type-key :posts)
                       :record)))
          (is (equal '(:id :title :body :update :delete)
                (u:plist-keys record)))
          (is (equal "vf probe" (getf record :title))))
        (let* ((result (be-list :posts "admin" :form :view-form))
               (probe (find id (getf result :records)
                        :key (lambda (r) (getf r :id))
                        :test #'equal)))
          (is-true probe)
          (when probe
            (is (equal '(:id :title :body :update :delete)
                  (u:plist-keys probe))))))
      (be-delete :posts id "admin"))))
