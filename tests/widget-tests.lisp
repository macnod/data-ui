(in-package :data-ui)

(def-suite widget-suite :description "Widget allow-list and UI emission tests")

(in-suite widget-suite)

;;; --- Helpers for building minimal test models ---

(defun widget-test-model (&rest field-plists)
  "Build a minimal types plist with one type :wt whose fields
are FIELD-PLISTS. Each entry is (field-key . field-def-plist)."
  `(:wt
     (:table t
       :create :auto :update :auto :delete :auto
       :type-roles ("wt-users")
       :views (:main (:tables (:wt)))
       :fields
       ,(loop
          for (key . def) in field-plists
          appending (list key def))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))))

(defun widget-compile (&rest field-plists)
  "Compile a minimal model with the given fields and return
the compiled :wt type definition."
  (getf (compile-model (apply #'widget-test-model field-plists)) :wt))

(defun compiled-field-ui (type-key field-key)
  "Extract the :ui plist from a compiled field in *compiled-model*."
  (let* ((fields (u:tree-get *compiled-model* type-key :fields))
         (def (getf fields field-key)))
    (getf def :ui)))

;;; --- Tests ---

(test widget-happy-path
  "test-model compiles and sample fields have valid :widget values."
  (let ((name-ui (compiled-field-ui :todos :name)))
    (is (eq :textbox (getf name-ui :widget))))
  (let ((done-ui (compiled-field-ui :todos :done)))
    (is (eq :checkbox (getf done-ui :widget))))
  (let ((tags-ui (compiled-field-ui :todos :tags)))
    (is (eq :checkbox-list (getf tags-ui :widget)))))

(test widget-default-missing-widget
  "A field whose :ui has :label but no :widget defaults to :textbox."
  (let* ((compiled (widget-compile
                     (cons :my-field
                       '(:type :text
                          :ui (:label "My Field")
                          :source (:view :main :column :my-field :agg :first)
                          :column t))))
         (fields (getf compiled :fields))
         (def (getf fields :my-field))
         (ui (getf def :ui)))
    (is (eq :textbox (getf ui :widget)))))

(test widget-default-image-read-only
  ":image and :image-list without :read-only get :read-only t."
  (let* ((compiled (widget-compile
                     (cons :img
                       '(:type :text
                          :ui (:widget :image :table :images)
                          :source (:view :main :column :img :agg :first)
                          :column t))
                     (cons :imgs
                       '(:type :text
                          :ui (:widget :image-list :table :images)
                          :source (:view :main :column :imgs :agg :first)
                          :column t))))
         (fields (getf compiled :fields)))
    (is (eq t (getf (getf (getf fields :img) :ui) :read-only)))
    (is (eq t (getf (getf (getf fields :imgs) :ui) :read-only)))))

(test widget-reject-image-read-only-nil
  ":image with :read-only nil fails compile."
  (signals error
    (widget-compile
      (cons :img
        '(:type :text
           :ui (:widget :image :read-only nil :table :images)
           :source (:view :main :column :img :agg :first)
           :column t)))))

(test widget-default-missing-label
  "Missing :label is humanized from the field key."
  (let* ((compiled (widget-compile
                     (cons :average-rating
                       '(:type :text
                          :ui (:widget :stars)
                          :source (:view :main :column :average-rating :agg :first)
                          :column t))))
         (fields (getf compiled :fields))
         (label (getf (getf (getf fields :average-rating) :ui) :label)))
    (is (equal "Average Rating" label))))

(test widget-reject-unknown-widget
  "Unknown :widget value fails compile."
  (signals error
    (widget-compile
      (cons :bad
        '(:type :text
           :ui (:widget :not-a-widget)
           :source (:view :main :column :bad :agg :first)
           :column t)))))

(test widget-reject-read-only-as-widget
  ":read-only is not a valid widget value."
  (signals error
    (widget-compile
      (cons :ro
        '(:type :text
           :ui (:widget :read-only)
           :source (:view :main :column :ro :agg :first)
           :column t)))))

(test widget-reject-line-and-text-widgets
  ":line and :text are dead widget values (use :textbox)."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :text
           :ui (:widget :line)
           :source (:view :main :column :f1 :agg :first)
           :column t))))
  (signals error
    (widget-compile
      (cons :f2
        '(:type :text
           :ui (:widget :text)
           :source (:view :main :column :f2 :agg :first)
           :column t)))))

(test widget-reject-dead-ui-keys
  "Dead keys :render-as, :input-type, :form-control fail compile."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :text
           :ui (:render-as :stars :widget :textbox)
           :source (:view :main :column :f1 :agg :first)
           :column t))))
  (signals error
    (widget-compile
      (cons :f2
        '(:type :text
           :ui (:input-type :textbox)
           :source (:view :main :column :f2 :agg :first)
           :column t))))
  (signals error
    (widget-compile
      (cons :f3
        '(:type :text
           :ui (:form-control :textbox)
           :source (:view :main :column :f3 :agg :first)
           :column t)))))

(test widget-roles-injection-keyword
  "Roles injection uses keyword :checkbox-list, not string."
  ;; The roles field is injected by fe-fields, not compile-field,
  ;; so we check fe-fields output on a non-base type.
  ;; fe-fields merges :ui into the field plist, so :widget
  ;; appears at the top level of the emitted field.
  (let ((fe (fe-fields :todos "admin")))
    (let ((update-fields (getf fe :update-form)))
      (is (getf update-fields :roles)
          ":roles should appear in update-form")
      (let ((roles-def (getf update-fields :roles)))
        (is (eq :checkbox-list (getf roles-def :widget))
            ":roles widget must be keyword :checkbox-list")))))

(test widget-button-status-synthesis
  "Button status fields synthesize :widget :textbox + :read-only t."
  (let ((status-ui (compiled-field-ui :todos :test-action-status)))
    (is (eq :textbox (getf status-ui :widget)))
    (is (eq t (getf status-ui :read-only)))))

(test widget-all-models-compile
  "Every real model file in models/ compiles under the allow-list.
Skips widgets.lisp (a template with :model: placeholders, not a
loadable model)."
  (let ((model-dir (u:join-paths *package-root* "models")))
    (loop for file in (directory (u:join-paths model-dir "*.lisp"))
          for stem = (pathname-name file)
          unless (string= stem "widgets")
          do (finishes
               (with-open-file (in file)
                 (compile-model
                   (getf (cadr (read in)) :types)))))))
