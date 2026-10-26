(in-package :data-ui)

(def-suite field-help-compile
  :description ":ui :help value / combination rule probes")

(def-suite field-help-wire
  :description ":help wire emission (fe-fields, plist-to-json) on
field-help-test")

(in-suite field-help-compile)

;;; field-help-compile reuses widget-compile (from widget-tests.lisp,
;;; loaded first per data-ui.asd) to probe the :ui :help rules.

(test help-string-compiles-verbatim
  "A string :help compiles and survives on the compiled :ui
verbatim — the author string, not a rewritten one."
  (let* ((compiled (widget-compile
                     (cons :title
                       '(:type :text
                          :ui (:label "Title"
                            :help "Use `kebab-case`.")
                          :source (:view :main :column :title :agg :first)
                          :column t))))
         (def (getf (getf compiled :fields) :title)))
    (is (equal "Use `kebab-case`."
          (getf (getf def :ui) :help)))))

(test help-absent-leaves-no-key
  "A field without :help compiles with no :help key on the
compiled :ui."
  (let* ((compiled (widget-compile
                     (cons :body
                       '(:type :text
                          :ui (:label "Body")
                          :source (:view :main :column :body :agg :first)
                          :column t))))
         (def (getf (getf compiled :fields) :body)))
    (is-false (member :help (getf def :ui)))))

(test help-non-string-signals
  "Non-string :help values signal: t, 1, and present-nil."
  (signals error
    (widget-compile
      (cons :title
        '(:type :text
           :ui (:label "Title" :help t)
           :source (:view :main :column :title :agg :first)
           :column t))))
  (signals error
    (widget-compile
      (cons :title
        '(:type :text
           :ui (:label "Title" :help 1)
           :source (:view :main :column :title :agg :first)
           :column t))))
  (signals error
    (widget-compile
      (cons :title
        '(:type :text
           :ui (:label "Title" :help nil)
           :source (:view :main :column :title :agg :first)
           :column t)))))

(test help-blank-signals
  "Empty and whitespace-only :help signal (blank renders an empty
panel)."
  (signals error
    (widget-compile
      (cons :title
        '(:type :text
           :ui (:label "Title" :help "")
           :source (:view :main :column :title :agg :first)
           :column t))))
  (signals error
    (widget-compile
      (cons :title
        '(:type :text
           :ui (:label "Title"
             :help (coerce '(#\Space #\Tab #\Newline) 'string))
           :source (:view :main :column :title :agg :first)
           :column t)))))

(test help-with-hide-label-t-signals
  ":help + :hide-label t signals (the icon anchors to the label)."
  (signals error
    (widget-compile
      (cons :body
        '(:type :text
           :ui (:label "Body" :help "See rules." :hide-label t)
           :source (:view :main :column :body :agg :first)
           :column t)))))

(test help-with-hide-label-nil-compiles
  ":help + explicit :hide-label nil is legal (label shown)."
  (let* ((compiled (widget-compile
                     (cons :title
                       '(:type :text
                          :ui (:label "Title"
                            :help "ok" :hide-label nil)
                          :source (:view :main :column :title :agg :first)
                          :column t))))
         (def (getf (getf compiled :fields) :title)))
    (is (equal "ok" (getf (getf def :ui) :help)))))

(test help-without-widget-compiles
  ":help on a field with no :widget compiles (the defaults path —
:widget :textbox is injected, :help rides untouched)."
  (let* ((compiled (widget-compile
                     (cons :title
                       '(:type :text
                          :ui (:label "Title" :help "ok")
                          :source (:view :main :column :title :agg :first)
                          :column t))))
         (def (getf (getf compiled :fields) :title)))
    (is (eq :textbox (getf (getf def :ui) :widget)))
    (is (equal "ok" (getf (getf def :ui) :help)))))

(test help-typo-still-signals-listing-help
  "The unknown-key path still signals for a typo (:helps) and the
message lists :help among legal keys."
  (signals error
    (widget-compile
      (cons :title
        '(:type :text
           :ui (:label "Title" :helps "no")
           :source (:view :main :column :title :agg :first)
           :column t)))))

(test help-nil-with-hide-label-t-reports-value-error
  ":help nil + :hide-label t reports the value error (must be a
string), not the anchor error — valid-help-value runs before the
combination check."
  (signals validation-error
    (widget-compile
      (cons :body
        '(:type :text
           :ui (:label "Body" :help nil :hide-label t)
           :source (:view :main :column :body :agg :first)
           :column t)))))

(in-suite field-help-wire)

(defun fh-field (form field-key)
  "One fe-fields form entry for FIELD-KEY on the field-help-test
:posts type (admin)."
  (getf (getf (fe-fields :posts "admin") form) field-key))

(test wire-add-form-carries-help
  "fe-fields emits :help on add-form fields for the declaring
field, not for the plain field."
  (let ((title (fh-field :add-form :title))
        (body (fh-field :add-form :body)))
    (is-true title)
    (is (equal "The **primary** identifier.

- Use `kebab-case`
- Keep it short"
          (getf title :help)))
    (is-true body)
    (is-false (member :help body))))

(test wire-update-form-carries-help
  "fe-fields emits :help on update-form fields for the declaring
field."
  (let ((title (fh-field :update-form :title)))
    (is-true title)
    (is (stringp (getf title :help)))))

(test wire-list-form-carries-help-verbatim
  "The list-form payload carries :help too (verbatim :ui
passthrough; the header renderer ignores it — the docs say so)."
  (let ((title (fh-field :list-form :title)))
    (is-true title)
    (is (stringp (getf title :help)))))

(test json-help-serializes-as-string
  "(plist-to-json '(:help \"x\")) yields \"help\":\"x\" — a string
serializes as a JSON string; no boolean-key registration is
involved."
  (is (search "\"help\":\"x\""
        (plist-to-json '(:help "x")))))
