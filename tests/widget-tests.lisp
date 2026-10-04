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

(test widget-reject-unknown-ui-key
  "A :ui key outside *ui-keys* fails compile."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :text
           :ui (:labell "My Field")
           :source (:view :main :column :f1 :agg :first)
           :column t)))))

(test widget-reject-label-keyword
  ":label must be a string, not a keyword."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :text
           :ui (:label :my-field)
           :source (:view :main :column :f1 :agg :first)
           :column t)))))

(test widget-reject-precision-string
  ":precision must be a number, not a string."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :real
           :ui (:precision "1")
           :source (:view :main :column :f1 :agg :first)
           :column t)))))

(test widget-reject-table-string
  ":table must be a keyword, not a string."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :text
           :ui (:table "images")
           :source (:view :main :column :f1 :agg :first)
           :column t)))))

(test widget-filter-with-rejects-non-boolean-value
  ":filter-with :select fails compile (only :boolean is legal)."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :boolean
           :ui (:filter-with :select)
           :source (:view :main :column :f1 :agg :first)
           :column t)))))

(test widget-filter-with-boolean-compiles
  ":filter-with :boolean compiles on :type :boolean with :column t, and
the compiled :ui still carries :filter-with."
  (let* ((compiled
           (widget-compile
             (cons :f1
               '(:type :boolean
                  :ui (:filter-with :boolean)
                  :source (:view :main :column :f1 :agg :first)
                  :column t))))
         (ui (getf (getf (getf compiled :fields) :f1) :ui)))
    (is (eq :boolean (getf ui :filter-with)))))

(test widget-filter-with-rejects-text-field
  ":filter-with :boolean on a :type :text field fails compile."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :text
           :ui (:filter-with :boolean)
           :source (:view :main :column :f1 :agg :first)
           :column t)))))

(test widget-filter-with-rejects-no-column
  ":filter-with :boolean on a :type :boolean field without :column t
fails compile."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :boolean
           :ui (:filter-with :boolean)
           :source (:view :main :column :f1 :agg :first))))))

(test filter-with-wire-value
  "Boolean :eq over REST. parse-filters coerces the strings \":true\" and
\":false\" to keywords when the field is :type :boolean, which is the only
encoding value-type-p accepts. Runs against the live *compiled-model*
(todos is loaded), whose :settings :dark-mode is :type :boolean with
:column t."
  (is (value-type-p :settings :dark-mode :true))
  (is (equal '((:settings :dark-mode :eq :true))
        (parse-filters
          "[[\"settings\",\"dark-mode\",\"eq\",\":true\"]]")))
  (finishes
    (valid-filters
      (parse-filters
        "[[\"settings\",\"dark-mode\",\"eq\",\":true\"]]"))))

(test widget-filter-with-plist-default-compiles
  "The configured form (:kind :boolean :default :true) compiles on
:type :boolean / :column t and survives verbatim into the compiled
:ui. The default is frontend initial state only; the compiler
stores the plist untouched."
  (let* ((compiled
           (widget-compile
             (cons :f1
               '(:type :boolean
                  :ui (:filter-with (:kind :boolean :default :true))
                  :source (:view :main :column :f1 :agg :first)
                  :column t))))
         (ui (getf (getf (getf compiled :fields) :f1) :ui)))
    (is (equal '(:kind :boolean :default :true)
          (getf ui :filter-with)))))

(test widget-filter-with-plist-no-default-compiles
  "(:kind :boolean) with no :default compiles — omitted default is Any."
  (let* ((compiled
           (widget-compile
             (cons :f1
               '(:type :boolean
                  :ui (:filter-with (:kind :boolean))
                  :source (:view :main :column :f1 :agg :first)
                  :column t))))
         (ui (getf (getf (getf compiled :fields) :f1) :ui)))
    (is (equal '(:kind :boolean)
          (getf ui :filter-with)))))

(test widget-filter-with-rejects-default-any
  ":default :any is a compile error — Any is the omitted default and
is not spellable."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :boolean
             :ui (:filter-with (:kind :boolean :default :any))
             :source (:view :main :column :f1 :agg :first)
             :column t)))))

(test widget-filter-with-rejects-default-maybe
  ":default :maybe is a compile error — only :true / :false are legal."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :boolean
             :ui (:filter-with (:kind :boolean :default :maybe))
             :source (:view :main :column :f1 :agg :first)
             :column t)))))

(test widget-filter-with-rejects-unknown-kind
  "(:kind :select ...) fails compile — :select is not in
*filter-with-kinds*."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :boolean
             :ui (:filter-with (:kind :select :default :true))
             :source (:view :main :column :f1 :agg :first)
             :column t)))))

(test widget-filter-with-rejects-positional-form
  "(:boolean :true) fails compile — the positional form is a
keyword-headed plist with no :kind."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :boolean
             :ui (:filter-with (:boolean :true))
             :source (:view :main :column :f1 :agg :first)
             :column t)))))

(test widget-filter-with-rejects-unknown-tail-key
  "(:kind :boolean :bogus :x) fails compile — the tail key set is
closed (:kind, :default)."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :boolean
             :ui (:filter-with (:kind :boolean :bogus :x))
             :source (:view :main :column :f1 :agg :first)
             :column t)))))

(test widget-filter-with-plist-rejects-text-field
  "The plist form on a :type :text field fails compile (field gate,
new shape)."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :text
             :ui (:filter-with (:kind :boolean :default :false))
             :source (:view :main :column :f1 :agg :first)
             :column t)))))

(test widget-filter-with-plist-rejects-no-column
  "The plist form without :column t fails compile (field gate, new
shape)."
  (signals error
    (widget-compile
      (cons :f1
        '(:type :boolean
             :ui (:filter-with (:kind :boolean :default :false))
             :source (:view :main :column :f1 :agg :first))))))

(test widget-filter-with-plist-wire-json
  "Wire tripwire: the :filter-with plist rides the :ui verbatim
passthrough. plist-to-json renders :true / :false as JSON booleans
(plist-to-json-atom's :true / :false rule, not the quoted-string
symbol path — the plan's quoted-string expectation was wrong; the
frontend normalizes both) and the bare (:kind :boolean) form with
no default key at all. Fails if anyone 'fixes' the serializer."
  (let* ((compiled
           (widget-compile
             (cons :f1
               '(:type :boolean
                  :ui (:filter-with (:kind :boolean :default :false))
                  :source (:view :main :column :f1 :agg :first)
                  :column t))
             (cons :f2
               '(:type :boolean
                  :ui (:filter-with (:kind :boolean))
                  :source (:view :main :column :f2 :agg :first)
                  :column t))))
         (ui-1 (getf (getf (getf compiled :fields) :f1) :ui))
         (ui-2 (getf (getf (getf compiled :fields) :f2) :ui))
         (json-1 (plist-to-json (list :filter-with
                                (getf ui-1 :filter-with))))
         (json-2 (plist-to-json (list :filter-with
                                (getf ui-2 :filter-with)))))
    (is (search "\"filter-with\":{\"kind\":\"boolean\",\"default\":false}"
          json-1))
    (is (search "{\"kind\":\"boolean\"}" json-2))
    (is (null (search "default" json-2)))))

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
  "Every real model file in models/ (including models/test/) compiles
under the allow-list. Skips widgets.lisp (a template with :model:
placeholders, not a loadable model)."
  (let ((model-dir (u:join-paths *package-root* "models")))
    (loop for file in (u:directory-listing model-dir
                        :files-only t
                        :leaf-filter "(?i)\\.lisp$")
          for stem = (pathname-name file)
          unless (string= stem "widgets")
          do (finishes
               (with-open-file (in file)
                 (compile-model
                   (getf (cadr (read in)) :types)))))))
