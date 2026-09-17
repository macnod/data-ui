(in-package :data-ui)

(def-suite domain-stg-suite
  :description ":domain-stg top-level model key tests")

(def-suite domain-stg-validation-suite
  :description "Compile-time probes (no model, no DB)"
  :in domain-stg-suite)

(def-suite domain-stg-db-suite
  :description "Accessor tests (domain-stg-test fixture)"
  :in domain-stg-suite)

;;; ---------------------------------------------------------------------------
;;; Validation probes (no model, no DB)
;;; ---------------------------------------------------------------------------
;;; with-model cannot cover the missing-:domain case (:domain is
;;; required and set-model dies on that first), so these call
;;; top-level-settings / valid-top-level-field on inline plists.

(in-suite domain-stg-validation-suite)

(defun th-ds-model (&key domain domain-stg)
  "Minimal full model plist; DOMAIN-STG omitted when nil."
  (let ((model (list :title "DS" :name "ds" :version "0.1"
                 :domain domain
                 :types '(:widgets (:table t
                                  :fields (:name (:type :text)))))))
    (when domain-stg
      (setf (getf model :domain-stg) domain-stg))
    model))

(test domain-stg-helper-suffix-rule
  "domain-stg-from-domain appends -stg to the first DNS label."
  (is (equal "todo-stg.demo.data-ui.com"
        (domain-stg-from-domain "todo.demo.data-ui.com")))
  (is (equal "a-stg.b.c"
        (domain-stg-from-domain "a.b.c"))))

(test domain-stg-derived-default-materialized
  "Absent :domain-stg with :domain present: top-level-settings
fills the suffix-derived default."
  (is (equal "todo-stg.demo.data-ui.com"
        (getf (top-level-settings
                (th-ds-model :domain "todo.demo.data-ui.com"))
          :domain-stg))))

(test domain-stg-explicit-wins
  "An explicit :domain-stg passes through unchanged; no
derivation."
  (is (equal "stg.example.com"
        (getf (top-level-settings
                (th-ds-model :domain "example.com"
                  :domain-stg "stg.example.com"))
          :domain-stg))))

(test domain-stg-without-domain-signals
  ":domain-stg without :domain signals (via :domain's own
required check — :domain is validated first in the key order)."
  (signals error
    (top-level-settings
      (th-ds-model :domain nil :domain-stg "stg.example.com"))))

(test domain-stg-equal-to-domain-signals
  ":domain-stg equal to :domain signals."
  (signals error
    (top-level-settings
      (th-ds-model :domain "x.example.com"
        :domain-stg "x.example.com")))
  ;; Direct field probe too — the check must fire inside
  ;; valid-top-level-field, not only via key ordering.
  (signals error
    (valid-top-level-field
      (th-ds-model :domain "x.example.com"
        :domain-stg "x.example.com")
      :domain-stg)))

(test domain-stg-absent-finishes
  "Absent key with :domain present validates (derivation fills
it)."
  (finishes
    (valid-top-level-field
      (th-ds-model :domain "todo.demo.data-ui.com")
      :domain-stg)))

(test domain-stg-bad-values-signal
  "Non-FQDN strings signal. (nil is treated as absent and 42 is
caught by :domain-stg's own nil-tolerant predicate, so neither
belongs here.)"
  (loop for bad in '("nope" "todo." "todo")
    do (signals error
         (valid-top-level-field
           (th-ds-model :domain "todo.demo.data-ui.com"
             :domain-stg bad)
           :domain-stg))))

;;; ---------------------------------------------------------------------------
;;; Accessor (domain-stg-test fixture — declares :domain only)
;;; ---------------------------------------------------------------------------

(in-suite domain-stg-db-suite)

(test domain-stg-accessor-derived-in-set-model
  "set-model on the fixture (declares :domain only) leaves the
derived staging FQDN in *top-level-settings*, and model-domain-stg
returns it."
  (is (equal "domain-stg-stg.demo.data-ui.com"
        (getf *top-level-settings* :domain-stg)))
  (is (equal "domain-stg-stg.demo.data-ui.com"
        (model-domain-stg)))
  (is (equal "domain-stg.demo.data-ui.com" (model-domain))))
