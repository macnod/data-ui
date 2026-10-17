(in-package :data-ui)

(def-suite enabled-when-compile
  :description ":ui :enabled-when compile-time probes (validate-model).")

(def-suite enabled-when-evaluator
  :description "eval-condition-rows unit tests, cases from the
shared cross-language fixture tests/fixtures/enabled-when-cases.json
(the same file a future FE test run consumes).")

(def-suite enabled-when-behavior
  :description "be-action state-gate guard on the spawn-test fixture.")

(in-suite enabled-when-compile)

;;; ---------------------------------------------------------------------------
;;; Compile probes. sp-base-model from spawn-tests.lisp supplies the
;;; :items shape; the button here gains the gate.
;;; ---------------------------------------------------------------------------

(defun ew-button (rows)
  "The :complete button field def carrying an :enabled-when gate
ROWS (nil means no gate)."
  (list :type :button
    :ui (append (list :label "Complete" :widget :button)
            (when rows (list :enabled-when rows)))
    :action (list :spawn
              :close (list :completed :true :completed-at :now)
              :clear (list :notes))))

(defun ew-compile (rows)
  "Compile a model with the :items type carrying a :complete button
whose :enabled-when is ROWS (:empty injects an explicit ()). Returns
the compiled :items type-def."
  (let* ((base (sp-base-model))
         (type-def (getf base :items))
         (fields (getf type-def :fields)))
    (setf (getf fields :complete) (ew-button rows))
    (setf (getf type-def :fields) fields)
    ;; A gate of () is author-supplied emptiness: ew-button cannot
    ;; inject it (getf would drop the nil), so splice :enabled-when
    ;; () onto the ui plist directly to reach the validator.
    (when (eq rows :empty)
      (let* ((btn (getf fields :complete))
             (ui (getf btn :ui))
             (ui-with (append ui (list :enabled-when '()))))
        (setf (getf btn :ui) ui-with)
        (setf (getf fields :complete) btn)))
    (getf (validate-model base) :items)))

(test ew-compile-accepted-and-verbatim
  "A well-formed gate on a :button compiles and rides the compiled
field's :ui verbatim (fe-fields passes :ui through to the FE)."
  (let ((items (ew-compile
                 (list (list :complete-status :ne "complete")))))
    (is (equal (u:tree-get items :fields :complete :ui :enabled-when)
           '((:complete-status :ne "complete"))))))

(test ew-compile-rejects-non-button-field
  ":enabled-when on a non-:button field is a compile error."
  (signals error
    (let* ((base (sp-base-model))
           (type-def (getf base :items))
           (fields (getf type-def :fields))
           (name-def (getf fields :name)))
      (setf (getf name-def :ui)
        (append (getf name-def :ui)
          (list :enabled-when (list (list :points :gt 2)))))
      (setf (getf fields :name) name-def)
      (validate-model base))))

(test ew-compile-rejects-unknown-field
  "A row naming a nonexistent field is a compile error."
  (signals error
    (ew-compile (list (list :no-such-field :ne "complete")))))

(test ew-compile-rejects-unknown-operator
  "An operator outside the closed v1 set is a compile error."
  (signals error
    (ew-compile (list (list :complete-status :like "comp%")))))

(test ew-compile-rejects-like-family-as-deferred
  "The :like family signals as deferred, not silently accepted."
  (signals error
    (ew-compile (list (list :name :ilike "a%"))))
  (signals error
    (ew-compile (list (list :name :not-like "a%"))))
  (signals error
    (ew-compile (list (list :tags :has-all (list "red"))))))

(test ew-compile-rejects-empty-rows
  "Empty () reads like a gate but is vacuously true — a compile
error."
  (signals error
    (ew-compile :empty)))
(test ew-compile-rejects-scalar-for-in
  ":in with a scalar literal is a compile error."
  (signals error
    (ew-compile (list (list :name :in "alpha")))))

(test ew-compile-rejects-list-for-scalar-op
  ":eq with a list literal is a compile error."
  (signals error
    (ew-compile (list (list :name :eq (list "alpha" "beta"))))))

(test ew-compile-rejects-keyword-literal
  "A keyword value for a scalar op is a compile error (the wire
never carries keywords)."
  (signals error
    (ew-compile (list (list :name :eq :some-keyword)))))

;;; ---------------------------------------------------------------------------
;;; Evaluator unit tests (shared fixture)
;;; ---------------------------------------------------------------------------

(in-suite enabled-when-evaluator)

(defun ew-fixtures ()
  "Read and parse the shared enabled-when case fixture. Yason's
alist mode leaves the top level as a list of (string . value)
alist case objects — map each to a keyword-keyed case plist and
return the LIST of cases (json-to-plist's 4-tuple filter would
misread the row triples as filter clauses). Booleans arrive as
Y:TRUE / Y:FALSE and are normalized to T / NIL so ew-run-case
compares with eq."
  (let ((yason:*parse-object-as* :alist)
         (yason:*parse-json-booleans-as-symbols* t))
    (loop for case-alist in
            (yason:parse
              (u:slurp
                (u:join-paths *package-root* "tests" "fixtures"
                  "enabled-when-cases.json")))
      collect
      (loop for (key . value) in case-alist
        append (list (u:make-keyword key)
                 (cond
                   ((eq value yason:true) t)
                   ((eq value yason:false) nil)
                   (t value)))))))

(defun ew-row-from-json (row)
  "Convert one fixture row [\"field\", \"op\", value] to the Lisp
(:field :op value) triple. The wire format is plain strings (the
shape JSON naturally carries and the shape a future TS runner
consumes); keywordizing happens here, on the Lisp side only."
  (list (u:make-keyword (first row))
        (u:make-keyword (second row))
        (third row)))

(defun ew-record-from-json (case)
  "The fixture record object with string keys, keywordized to a
Lisp plist (JSON null parses to nil — the wire's only null; a
dotted alist value means the key maps to nil, which getf of the
plist below reproduces via the explicit (key nil) pair)."
  (loop for (key . value) in (getf case :record)
    append (list (u:make-keyword key) value)))

(defun ew-run-case (case)
  "Run one fixture case through eval-condition-rows. CASE keys:
:name, :rows, :record, :expected. Returns the case name on
mismatch, nil on pass."
  (let* ((rows (mapcar #'ew-row-from-json (getf case :rows)))
         (record (ew-record-from-json case))
         (expected (getf case :expected))
         (got (eval-condition-rows rows record)))
    (if (eq got expected)
      nil
      (getf case :name))))

(test ew-evaluator-shared-fixture
  "Every case in tests/fixtures/enabled-when-cases.json passes on
the Lisp side (the FE mirrors the same rules)."
  (let ((failures (remove nil (mapcar #'ew-run-case (ew-fixtures)))))
    (is (null failures) "Failed cases: ~a" failures)))

(test ew-evaluator-null-keyword-empty-class
  "The :null keyword is a Lisp-only member of the empty class
(JSON cannot express it; asserted beside the shared fixture, not
inside it)."
  (is-false (eval-condition-rows '((:model :ne ""))
                  '(:model :null)))
  (is-true (eval-condition-rows '((:model :eq ""))
                  '(:model :null))))

(test ew-evaluator-no-rows-enabled
  "No gate means enabled (every row vacuously holds)."
  (is-true (eval-condition-rows '() '(:anything 1))))

;;; ---------------------------------------------------------------------------
;;; Behavior (with-model "spawn-test")
;;; ---------------------------------------------------------------------------

(in-suite enabled-when-behavior)

(defun ew-seed-open ()
  "Reset items and seed one open row; returns its id. Idempotent on
the worker user (users persist across resets within a model
context)."
  (dolist (r (getf (be-list :items "admin" :limit 100) :records))
    (be-delete :items (getf r :id) "admin"))
  (unless (be-id :users '((:users :name :eq "worker")) "admin")
    (th-make-user "worker" :roles '("item-users")))
  (be-insert :tags '(:name "home") "admin" :roles '("item-users"))
  (be-insert :items
    (list :name "mow-the-lawn" :points 3 :notes "n")
    "worker" :roles '("item-users"))
  (th-sp-fresh-id))

(defun ew-open-id ()
  (th-sp-fresh-id))

(defun ew-completed-id ()
  "The id of a closed row, or nil."
  (be-id :items '((:items :name :eq "mow-the-lawn")
                  (:items :completed :eq :true))
    "admin"))

(test ew-guard-rejects-on-completed-row
  "be-action rejects when the gate fails (status \"complete\"):
  same error class as the in-progress guard."
  (ew-seed-open)
  (let ((id (ew-open-id)))
    (be-action :items id :complete "worker")  ; closes it
    (let ((closed-id (ew-completed-id)))
      (is-true closed-id "Precondition: closed row exists")
      (signals validation-error
        (be-action :items closed-id :complete "worker"))
      ;; No re-run damage: still exactly one history row
      (let ((history (getf (be-list :items "admin"
                             :filters '((:items :completed :eq :true)))
                          :records)))
        (is (= 1 (length history)) "Gate prevented the duplicate"))
      ;; And the status column was not stranded as running
      (let ((status (getf (getf (rec closed-id "admin" :type-key :items)
                           :record)
                     :complete-status)))
        (is (equal status "complete")))))
  ;; Cleanup
  (dolist (r (getf (be-list :items "admin" :limit 100) :records))
    (be-delete :items (getf r :id) "admin")))

(test ew-guard-passes-on-idle-and-failed
  "The gate passes on \"idle\" and on \"failed: …\" rows (the
documented re-run recovery path)."
  (ew-seed-open)
  (let ((id (ew-open-id)))
    (is-true id "Precondition: seeded open row exists")
    ;; idle: first run passes and completes
    (let ((result (be-action :items id :complete "worker")))
      (is (equal (getf result :status) "complete")))
    ;; failed: force a failed status on the closed row, then re-run
    (let ((closed-id (ew-completed-id)))
      (is-true closed-id)
      (be-set-field-value :items closed-id :complete-status
        "failed: simulated" "admin")
      (let ((result (be-action :items closed-id :complete "worker")))
        (is (equal (getf result :status) "complete")
          "Re-run after failure is allowed"))
      ;; still gates on a fresh complete
      (be-set-field-value :items closed-id :complete-status
        "complete" "admin")
      (signals validation-error
        (be-action :items closed-id :complete "worker"))))
  (dolist (r (getf (be-list :items "admin" :limit 100) :records))
    (be-delete :items (getf r :id) "admin")))

(test ew-no-gate-button-unchanged
  "A button without :enabled-when runs regardless of record state
(regression guard; test-model's ungated buttons also cover this in
the action suite)."
  (ew-seed-open)
  (let ((id (ew-open-id)))
    (is-true id)
    (let ((result (be-action :items id :complete "worker")))
      (is (equal (getf result :status) "complete"))))
  (dolist (r (getf (be-list :items "admin" :limit 100) :records))
    (be-delete :items (getf r :id) "admin")))
