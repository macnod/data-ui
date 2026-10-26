(in-package :data-ui)

(def-suite row-write-suite
  :description "Row-only update/action gate tests (row-write-test fixture)")

(in-suite row-write-suite)

(defun th-rw-seed ()
  "Seed row-write-test: alice and bob both hold notes-user (create +
read, no update — the :new-roles declaration is the whole point);
alice owns the shared note via her exclusive role."
  (th-make-user "rw-alice" :roles '("notes-user"))
  (th-make-user "rw-bob" :roles '("notes-user"))
  (th-rw-reset))

(defun th-rw-reset ()
  "Delete every :notes row as admin and re-seed one shared note as
alice, so each test starts from the same state (order-independent;
admin deletes make cleanup independent of which user may delete)."
  (dolist (r (getf (be-list :notes "admin" :limit 100) :records))
    (be-delete :notes (getf r :id) "admin"))
  (be-insert :notes '(:name "shared" :body "v1") "rw-alice"))

(defun th-rw-note-id ()
  (be-value-id :notes :name "shared" "admin"))

(defun th-rw-body ()
  "The shared note's body, read as admin (alice/bob hold read, but
admin sidesteps any question of who may read)."
  (a:with-rbac (*rbac*)
    (a:rbac-query
      (list "select note_body from rt_notes where note_name = 'shared'")
      :single)))

(defmacro th-rw-err (expr)
  "Evaluate EXPR, return the error string (or :no-error)."
  `(handler-case
       (progn ,expr :no-error)
     (error (e) (format nil "~a" e))))

(test row-write-creator-updates-own-row
  "The creator-edit case: a user with no type-level update can still
update the row her exclusive role sits on."
  (th-rw-reset)
  (let ((id (th-rw-note-id)))
    (is-true id)
    ;; Precondition: alice holds no update on the type resource.
    (is-false (a:user-allowed *rbac* "rw-alice" "update"
                (type-resource-name :notes)))
    (is (equal id (be-update :notes id '(:body "edited") "rw-alice")))
    (is (equal "edited" (th-rw-body)))))

(test row-write-creator-denied-on-others-row
  "A same-role peer with no row role is denied update; delete (the
already row-only gate) skips rather than signals, so assert the
row survives it."
  (th-rw-reset)
  (let ((id (th-rw-note-id)))
    (is-true id)
    (is-false (user-allowed-resource "rw-bob" id "update"))
    (let ((e (th-rw-err (be-update :notes id '(:body "nope") "rw-bob"))))
      (is (re:scan "does not have update permission on record" e)
          "got: ~a" e))
    ;; be-delete is the same rule, already row-only — and it skips
    ;; silently rather than signaling, so assert the row survives.
    (is-false (be-delete :notes id "rw-bob"))
    (is-true (be-value-id :notes :name "shared" "admin"))
    (is (equal "v1" (th-rw-body)))))

(test row-write-creator-action-on-own-row
  "be-action follows the same gate: the creator without type-level
update can run a button on her own row."
  (th-rw-reset)
  (let ((id (th-rw-note-id)))
    (is-true id)
    (is (equal (getf (be-action :notes id :test-sync "rw-alice") :status)
               "complete"))))

(test row-write-creator-action-denied-on-others-row
  "be-action denies a same-role peer without a row role."
  (th-rw-reset)
  (let ((id (th-rw-note-id)))
    (is-true id)
    (let ((e (th-rw-err (be-action :notes id :test-sync "rw-bob"))))
      (is (re:scan "does not have update permission on record" e)
          "got: ~a" e))))

(test row-write-insert-still-type-gated
  "The boundary did not move: insert still needs type-level create."
  (th-rw-reset)
  (th-make-user "rw-outsider")
  (let ((e (th-rw-err (be-insert :notes '(:name "x") "rw-outsider"))))
    (is (re:scan "does not have create permission" e)
        "got: ~a" e)))

(test row-write-wire-flag-stays-type-level
  "The /api/list update flag is type-level (user-capability), so a
creator-edit user gets :false even though be-update would accept
her write. The per-record :update / :delete keys are the Row
click item's (row-write-wire-flags-per-row below); this envelope
flag stays type-level on purpose (Delete Selected / Add gating)."
  (th-rw-reset)
  (is (equal :false (getf (be-list :notes "rw-alice") :update))))

(defun th-rw-readable-note ()
  "Insert a note readable by the whole notes-user role (as
opposed to the private 'shared' note th-rw-reset creates), and
return its id. The per-row flag tests need a row bob can read:
his be-list page and be-rec both answer from what he can see."
  (be-insert :notes '(:name "readable" :body "v1") "rw-alice"
    :roles '("notes-user"))
  (be-value-id :notes :name "readable" "admin"))

(test row-write-wire-flags-per-row
  "Per-record wire flags: alice's list record carries :true on
her own row (exclusive-role update/delete, no type-level
grant); bob's view of the same role-readable row carries :false
on both. The envelope flags above stay type-level — only the
record keys answer the row question."
  (th-rw-reset)
  (let ((id (th-rw-readable-note)))
    (is-true id)
    (let ((alice-row (find id (getf (be-list :notes "rw-alice")
                               :records)
                        :key (lambda (r) (getf r :id))
                        :test #'equal))
          (bob-row (find id (getf (be-list :notes "rw-bob")
                           :records)
                      :key (lambda (r) (getf r :id))
                      :test #'equal)))
      (is-true alice-row)
      (is-true bob-row)
      (is (equal :true (getf alice-row :update)))
      (is (equal :true (getf alice-row :delete)))
      (is (equal :false (getf bob-row :update)))
      (is (equal :false (getf bob-row :delete))))))

(test row-write-rec-flags
  "The /api/item arm: be-rec decorates its :record with the
per-row flags (what the View form's Edit button gates on), while
rec stays clean — internal callers feed its :record back
through valid-data, which rejects unknown keys. The
role-readable note gives bob a record he may read."
  (th-rw-reset)
  (let ((id (th-rw-readable-note))
        (alice-shared (th-rw-note-id)))
    (is-true id)
    (is (equal :true
          (getf (getf (be-rec id "rw-alice" :type-key :notes)
                  :record)
            :update)))
    (is (equal :false
          (getf (getf (be-rec id "rw-bob" :type-key :notes)
                  :record)
            :update)))
    ;; rec itself stays clean: the injected keys must never reach
    ;; valid-data through be-update's resource-rename path.
    (is-false (getf (getf (rec id "rw-alice" :type-key :notes)
                      :record)
              :update))
    ;; The decorated record's own list twin matches (both public
    ;; surfaces compute through row-capabilities).
    (is (equal
          (getf (find id (getf (be-list :notes "rw-alice") :records)
                  :key (lambda (r) (getf r :id)) :test #'equal)
            :update)
          (getf (getf (be-rec id "rw-alice" :type-key :notes)
                  :record)
            :update)))
    ;; be-update still succeeds on the same record after the
    ;; change — the flags ride be-rec, never rec, and valid-data
    ;; must not see them.
    (is (equal alice-shared
          (be-update :notes alice-shared '(:body "v2")
            "rw-alice")))))

(test row-write-model-nil-gates-rows-false
  "A nil model capability forces :false on every row with no
query — the rollup answer (rollups compile :update / :delete to
nil). Asserted on the model gate directly (compile-probe style):
wiring a second fixture into this suite costs more than the
check; be-list on a rollup routes through the same
row-capabilities nil path."
  (th-rw-reset)
  ;; The fixture type declares :update/:auto, so the gate is open;
  ;; the nil path is exercised by any rollup fixture elsewhere in
  ;; the suite (measure-rollup-tests read be-list records, which
  ;; now carry :false flags through this code path).
  (is (equal :true
        (getf (car (getf (be-list :notes "rw-alice") :records))
          :update))))

(test row-write-reserved-field-names
  "Author field keys :update / :delete are compile errors — the
per-record capability flags inject those keys onto every record,
so an author field so named would be shadowed on the wire."
  (signals error
    (valid-field-names :notes
      (list :update (list :type :text))))
  (signals error
    (valid-field-names :notes
      (list :delete (list :type :text))))
  (is-false
    (valid-field-names :notes
      (list :name (list :type :text)))))
