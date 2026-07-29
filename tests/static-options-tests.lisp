(in-package :data-ui)

(def-suite static-options-suite
  :description "Static dropdown :options tests")

(in-suite static-options-suite)

;;; --- Helpers for compile-reject tests ---

(defun options-test-model (&rest field-plists)
  "Build a minimal types plist with one type :ot whose fields
are FIELD-PLISTS. Each entry is (field-key . field-def-plist)."
  `(:ot
     (:table t
       :create :auto :update :auto :delete :auto
       :type-roles ("ot-users")
       :views (:main (:tables (:ot)))
       :fields
       ,(loop
          for (key . def) in field-plists
          appending (list key def))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))))

(defun options-compile (&rest field-plists)
  "Compile a minimal model with the given fields and return
the compiled :ot type definition."
  (getf (compile-model (apply #'options-test-model field-plists))
    :ot))

;;; --- Compile-time: happy path ---

(test options-compile-happy-path
  "static-select-test compiles and :priority has :options."
  (let ((ui (u:tree-get *compiled-model*
              :todos :fields :priority :ui)))
    (is (eq :select (getf ui :widget)))
    (is (equal '("high" "medium" "low") (getf ui :options)))))

(test options-compile-passthrough
  ":options survives compilation intact on the compiled field."
  (let* ((compiled (options-compile
                     (cons :status
                       '(:type :text
                          :ui (:label "Status" :widget :select
                               :options ("active" "inactive"))
                          :source (:view :main :column :status
                                   :agg :first)
                          :column t))))
         (ui (getf (getf (getf compiled :fields) :status) :ui)))
    (is (equal '("active" "inactive") (getf ui :options)))))

;;; --- Compile-time: shape errors ---

(test options-reject-not-a-list
  ":options not a list fails compile."
  (signals error
    (options-compile
      (cons :f
        '(:type :text
           :ui (:widget :select :options "high")
           :source (:view :main :column :f :agg :first)
           :column t)))))

(test options-reject-empty-list
  ":options empty list fails compile."
  (signals error
    (options-compile
      (cons :f
        '(:type :text
           :ui (:widget :select :options ())
           :source (:view :main :column :f :agg :first)
           :column t)))))

(test options-reject-non-string-element
  ":options with non-string element fails compile."
  (signals error
    (options-compile
      (cons :f
        '(:type :text
           :ui (:widget :select :options ("a" 42 "c"))
           :source (:view :main :column :f :agg :first)
           :column t)))))

(test options-reject-empty-string-element
  ":options with empty string element fails compile."
  (signals error
    (options-compile
      (cons :f
        '(:type :text
           :ui (:widget :select :options ("a" "" "c"))
           :source (:view :main :column :f :agg :first)
           :column t)))))

;;; --- Compile-time: widget / exclusivity / bare select ---

(test options-reject-non-select-widget
  ":options with :widget :textbox fails compile."
  (signals error
    (options-compile
      (cons :f
        '(:type :text
           :ui (:widget :textbox :options ("a" "b"))
           :source (:view :main :column :f :agg :first)
           :column t)))))

(test options-reject-with-target
  ":options + :target fails compile."
  ;; Need a target type for valid-target to pass first,
  ;; so we test via the mutual exclusivity check.
  ;; We build a two-type model.
  (signals error
    (compile-model
      `(:refs
         (:table t
           :create :auto :update :auto :delete :auto
           :type-roles ("refs-users")
           :views (:main (:tables (:refs)))
           :fields
           (:name (:type :text :identity t
                    :ui (:label "Name" :widget :textbox)
                    :source (:view :main :column :name :agg :first)
                    :column t :not-null t :unique t)
             :ref (:type :text
                    :ui (:label "Ref" :widget :select
                         :options ("x" "y"))
                    :target :refs
                    :source (:view :main :column :name :agg :first)
                    :column t))
           :list-form (:fields t)
           :update-form (:fields t)
           :add-form (:fields t))))))

(test options-reject-bare-select
  ":widget :select without :options or :target fails compile."
  (signals error
    (options-compile
      (cons :f
        '(:type :text
           :ui (:widget :select)
           :source (:view :main :column :f :agg :first)
           :column t)))))

;;; --- Read path ---

(test options-allowed-values-for-field
  "allowed-values-for-field returns static options list."
  (let ((vals (allowed-values-for-field :todos :priority "admin")))
    (is (equal '("high" "medium" "low") vals))))

(test options-allowed-values-includes-priority
  "allowed-values includes :priority with the options list."
  (let ((av (allowed-values :todos "admin")))
    (is (equal '("high" "medium" "low") (getf av :priority)))))

(test options-allowed-values-no-rbac-filter
  "Static options are the same for all users."
  (let ((admin-vals (allowed-values-for-field :todos :priority
                      "admin")))
    ;; Even a non-existent user gets the same options
    (let ((other-vals (allowed-values-for-field :todos :priority
                         "nobody")))
      (is (equal admin-vals other-vals)))))

;;; --- Write path ---

(defun th-options-insert-todo (name priority)
  "Insert a todo with the given priority. Returns the UUID."
  (be-insert :todos
    `(:name ,name :priority ,priority :points "1")
    "admin"))

(defun th-options-cleanup (id)
  "Delete a todo by ID if it exists."
  (when id
    (be-delete :todos id "admin")))

(test options-insert-valid
  "Insert with a valid option succeeds."
  (let ((id (th-options-insert-todo "opt-test-valid" "high")))
    (is-true id "Insert with valid option should succeed")
    (th-options-cleanup id)))

(test options-insert-invalid-fails
  "Insert with an invalid option fails validation."
  (signals validation-error
    (be-insert :todos
      '(:name "opt-test-invalid" :priority "urgent" :points "1")
      "admin")))

(test options-insert-nil-succeeds
  "Insert without :priority (nullable) succeeds."
  (let ((id (be-insert :todos
              '(:name "opt-test-nil" :points "1")
              "admin")))
    (is-true id "Insert without priority should succeed")
    (th-options-cleanup id)))

(test options-update-valid
  "Update with a valid option succeeds."
  (let ((id (th-options-insert-todo "opt-test-update" "low")))
    (unwind-protect
      (progn
        (is-true id "Precondition: insert succeeded")
        (be-update :todos id
          `(:priority "high") "admin")
        (let* ((record (getf (rec id "admin"
                              :type-key :todos) :record))
               (priority (getf record :priority)))
          (is (equal "high" priority)
              "Updated priority should be high")))
      (th-options-cleanup id))))

(test options-update-invalid-fails
  "Update with an invalid option fails validation."
  (let ((id (th-options-insert-todo "opt-test-update-bad" "low")))
    (unwind-protect
      (progn
        (is-true id "Precondition: insert succeeded")
        (signals validation-error
          (be-update :todos id
            '(:priority "banana") "admin")))
      (th-options-cleanup id))))
