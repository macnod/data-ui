(in-package :data-ui)

(def-suite form-fields-suite
  :description "Compile-time validation of form field references")

(in-suite form-fields-suite)

;;; --- Helpers ---

(defun ff-compile (&key
                     (list-form '(:fields t))
                     (update-form '(:fields t))
                     (add-form '(:fields t)))
  "Compile a minimal model with one type :ff whose fields are
:name and :age. Returns the compiled :ff type-def.
LIST-FORM, UPDATE-FORM, ADD-FORM are the form spec plists."
  (getf
    (compile-model
      `(:ff
         (:table t
           :create :auto :update :auto :delete :auto
           :type-roles ("ff-user")
           :views (:main (:tables (:ff)))
           :fields
           (:name
             (:type :text
               :ui (:label "Name" :widget :textbox)
               :validations (:required)
               :source (:view :main :column :name :agg :first)
               :column t :not-null t)
            :age
             (:type :integer
               :ui (:label "Age" :widget :textbox)
               :source (:view :main :column :age :agg :first)
               :column t))
           :list-form ,list-form
           :update-form ,update-form
           :add-form ,add-form)))
    :ff))

;;; --- Happy path tests ---

(test form-fields-all-fields-t
  ":fields t on every form compiles without error."
  (finishes (ff-compile)))

(test form-fields-explicit-valid
  "Explicit field lists referencing real fields compile fine."
  (finishes
    (ff-compile
      :list-form '(:fields (:name :age))
      :update-form '(:fields (:name))
      :add-form '(:fields (:name :age)))))

(test form-fields-includes-default-fields
  "Forms may reference :id, :created-at, :updated-at."
  (finishes
    (ff-compile
      :list-form '(:fields (:id :name :created-at :updated-at))
      :update-form '(:fields (:id :name))
      :add-form '(:fields (:name)))))

(test form-fields-absent-forms
  "Absent form specs compile without error."
  (finishes
    (ff-compile
      :list-form nil
      :update-form nil
      :add-form nil)))

;;; --- Rejection tests ---

(test form-fields-reject-bad-list-form-key
  "Unknown field in :list-form signals an error."
  (signals error
    (ff-compile
      :list-form '(:fields (:name :nonexistent)))))

(test form-fields-reject-bad-update-form-key
  "Unknown field in :update-form signals an error."
  (signals error
    (ff-compile
      :update-form '(:fields (:name :bogus)))))

(test form-fields-reject-bad-add-form-key
  "Unknown field in :add-form signals an error."
  (signals error
    (ff-compile
      :add-form '(:fields (:name :ghost)))))

(test form-fields-reject-typo
  "A typo in a field name is caught at compile time."
  (signals error
    (ff-compile
      :list-form '(:fields (:nme :age)))))

(test form-fields-reject-multiple-bad-keys
  "Multiple unknown fields in one form are all caught."
  (signals error
    (ff-compile
      :list-form '(:fields (:bad1 :bad2 :bad3)))))

(test form-fields-reject-bad-key-in-one-form-only
  "A bad key in one form is caught even if other forms are fine."
  (signals error
    (ff-compile
      :list-form '(:fields (:name :age))
      :update-form '(:fields (:name :age))
      :add-form '(:fields (:name :wrong)))))

;;; --- Edge cases ---

(test form-fields-empty-list-allowed
  "An empty field list compiles without error (means no fields)."
  (finishes
    (ff-compile
      :list-form '(:fields nil))))

(test form-fields-single-field-list
  "A single-element field list works."
  (finishes
    (ff-compile
      :list-form '(:fields (:name))
      :update-form '(:fields (:age))
      :add-form '(:fields (:name)))))

(test form-fields-all-existing-models-compile
  "Every model in models/ passes form-field validation."
  (let ((model-dir (u:join-paths *package-root* "models")))
    (loop for file in (directory (u:join-paths model-dir "*.lisp"))
          for stem = (pathname-name file)
          unless (member stem '("static-select-test" "nullable-fk-test"
                                "test-model" "widgets") :test #'string=)
          do (finishes
               (with-open-file (in file)
                 (compile-model
                   (getf (cadr (read in)) :types)))))))
