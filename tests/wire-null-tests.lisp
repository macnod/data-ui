(in-package :data-ui)

(def-suite wire-null-suite
  :description "Record values on the REST wire: a nil scalar field
serializes as JSON null (never the truthy [] of plist-to-json's
nil-value default), while list-valued fields and the envelope's
collections stay []. Internal readers keep nil. Runs on the
modelbank-test fixture (nullable :description / :model text, the
:rating write-through, the :average-rating :avg, the :images
:list).")

(in-suite wire-null-suite)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun wn-insert-model (name)
  "Insert a :models row with only :name, so :description, :model,
:rating, and :average-rating are all NULL and :images is empty.
Returns the id."
  (be-insert :models (list :name name) "admin"))

(defun wn-rating-row-count (model-id)
  "Number of :ratings rows (any user) pointing at MODEL-ID."
  (let ((sql (format nil "select count(*) from ~a where rating_model = $1"
               (table-name :ratings))))
    (a:with-rbac (*rbac*)
      (a:rbac-query (list sql model-id) :single))))

;;; ---------------------------------------------------------------------------
;;; Field classification
;;; ---------------------------------------------------------------------------

(test wn-wire-field-value-classifies-fields
  "Scalars (text, the write-through integer, the :avg real) map nil to
:null; the :list field keeps nil; a key that is not a model field
passes through."
  (is (eq :null (wire-field-value :models :description nil)))
  (is (eq :null (wire-field-value :models :rating nil)))
  (is (eq :null (wire-field-value :models :average-rating nil)))
  (is (null (wire-field-value :models :images nil)))
  (is (null (wire-field-value :models :not-a-field nil)))
  (is (equal "x" (wire-field-value :models :description "x"))))

;;; ---------------------------------------------------------------------------
;;; /api/item and /api/list JSON
;;; ---------------------------------------------------------------------------

(test wn-item-json-null-scalars-empty-lists
  "The /api/item body: NULL scalars are null, the empty :images list
is [], and be-rec itself still returns nil (internal callers)."
  (let ((id (wn-insert-model "wn-item")))
    (unwind-protect
      (let* ((result (be-rec id "admin" :type-key :models))
             (json (plist-to-json (wire-item-result result))))
        (is (null (getf (getf result :record) :description))
          "be-rec keeps nil for internal callers")
        (is (search "\"description\":null" json) "~a" json)
        (is (search "\"model\":null" json) "~a" json)
        (is (search "\"rating\":null" json) "~a" json)
        (is (search "\"average-rating\":null" json) "~a" json)
        (is (search "\"images\":[]" json) "~a" json)
        (is-false (search "\"description\":[]" json)))
      (be-delete :models id "admin"))))

(test wn-list-json-null-scalars-empty-lists
  "The /api/list body: same per-record rule; an empty page keeps
\"records\":[]."
  (let ((id (wn-insert-model "wn-list")))
    (unwind-protect
      (let* ((result (be-list :models "admin"
                       :filters `((:models :id :eq ,id))))
             (json (plist-to-json (wire-list-result result)))
             (empty (plist-to-json
                      (wire-list-result
                        (be-list :models "admin"
                          :filters '((:models :name :eq "wn-no-such")))))))
        (is (null (getf (first (getf result :records)) :description))
          "be-list keeps nil for internal callers")
        (is (search "\"description\":null" json) "~a" json)
        (is (search "\"average-rating\":null" json) "~a" json)
        (is (search "\"images\":[]" json) "~a" json)
        (is (search "\"records\":[]" empty) "~a" empty))
      (be-delete :models id "admin"))))

(test wn-inbound-null-and-empty-array-decode-to-nil
  "Inbound, JSON null and [] are the same Lisp nil, so a client
echoing either spelling back means the same thing."
  (is (null (getf (json-to-plist "{\"description\": null}") :description)))
  (is (null (getf (json-to-plist "{\"description\": []}") :description))))

;;; ---------------------------------------------------------------------------
;;; Untouched write-through field
;;; ---------------------------------------------------------------------------

(test wn-update-without-write-through-key-writes-no-row
  "An update that leaves the write-through :rating key out (the
frontend's update diff for an untouched field) inserts no :ratings
row; the absent key resolves to the compiled :null default, which
execute-write-to treats as no value supplied."
  (let ((id (wn-insert-model "wn-rating")))
    (unwind-protect
      (progn
        (be-update :models id
          '(:name "wn-rating" :description "edited") "admin")
        (is (= 0 (wn-rating-row-count id))
          "no :ratings row may appear for an untouched rating"))
      (be-delete :models id "admin"))))
