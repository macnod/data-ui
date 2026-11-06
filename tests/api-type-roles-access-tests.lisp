(in-package :data-ui)

(def-suite api-type-roles-access-suite
  :description "API type-roles access bug tests (api-type-roles-access-test fixture)")

(def-suite api-type-roles-rollup-control-suite
  :description "Rollup list-gate negative control (measure-rollup-test fixture)")

(in-suite api-type-roles-access-suite)

;;; ---------------------------------------------------------------------------
;;; Fixture helpers
;;; ---------------------------------------------------------------------------

(defun th-atr-seed ()
  "Seed the fixture: atr-writer holds row-writer (read / update /
delete on row resources that name it) and nothing else. The notes
rows are (re)created by th-atr-reset, which every mutating test
calls first."
  (th-make-user "atr-writer" :roles '("row-writer"))
  (th-atr-reset)
  nil)

(defun th-atr-reset ()
  "Delete every :notes row as admin, then insert the shared row
(roles public + row-writer: readable by guest, writable by
row-writer) and the private row (roles omitted: admin only). Admin
creates both, so the roles are always attachable. Calling this at
the top of each test keeps the suite order-independent."
  (dolist (r (getf (be-list :notes "admin" :limit 100) :records))
    (be-delete :notes (getf r :id) "admin"))
  (be-insert :notes '(:name "shared" :body "v1") "admin"
    :roles '("public" "row-writer"))
  (be-insert :notes '(:name "private" :body "secret") "admin"))

(defun th-atr-note-id (name)
  "The id of the note named NAME (admin lookup)."
  (be-value-id :notes :name name "admin"))

(defun th-atr-body (name)
  "The body of the note named NAME, read as admin."
  (let ((id (th-atr-note-id name)))
    (when id
      (getf (getf (rec id "admin" :type-key :notes) :record) :body))))

(defun th-atr-delete-user (name)
  "Delete the user NAME (no-op when absent) — test cleanup."
  (let ((uid (a:get-id *rbac* "users" name)))
    (when uid (be-delete :users uid "admin"))))

;;; ---------------------------------------------------------------------------
;;; REST harness (direct easy-handler calls, transport stubbed only)
;;; ---------------------------------------------------------------------------

(define-condition atr-aborted (error)
  ((code :initarg :code :reader atr-aborted-code)
   (message :initarg :message :reader atr-aborted-message))
  (:documentation "Stand-in for hunchentoot's abort-request-handler:
the abort must not return, and a stub that returned nil would let
execution fall into the logging call the abort was supposed to
prevent. atr-aborted-code carries the HTTP status."))

(defparameter *atr-token-users*
  '(("atr-token-admin" . "admin")
    ("atr-token-writer" . "atr-writer")
    ("atr-token-guest" . "guest")
    ("atr-token-nobody" . "atr-nobody")
    ("atr-token-plain" . "atr-plain")
    ("atr-token-empty" . "atr-empty")
    ("atr-token-rollup-plain" . "atr-rollup-plain"))
  "Fixed token-to-user map behind the validate-access-token stub.
The users behind the last four entries are created (and removed) by
the tests that use their tokens.")

(defun th-atr-fake-request ()
  "A bare Hunchentoot request for direct handler calls (the
th-al-fake-request shape): the handlers evaluate (h:real-remote-addr)
for record-usage's :ip argument even though record-usage itself is
stubbed — the argument evaluates first."
  (let ((h:*reply* (make-instance 'h:reply))
        (h:*session* nil)
        (h:*acceptor* *http-server*))
    (declare (dynamic-extent h:*reply* h:*session* h:*acceptor*))
    (make-instance 'h:request
      :headers-in (list (cons :x-forwarded-for "203.0.113.7"))
      :method :get
      :uri "/api/test"
      :server-protocol :http/1.1
      :local-addr "127.0.0.1"
      :local-port 8081
      :remote-addr "127.0.0.1"
      :remote-port 12345)))

(defun th-atr-call-with-stubs (bindings thunk)
  "Install symbol-function stubs from BINDINGS — (fn lambda) entries
— for the duration of THUNK, restoring the real definitions on every
exit including a signal (the th-al-call-with-stubs shape)."
  (let ((saved nil))
    (unwind-protect
        (progn
          (dolist (binding bindings)
            (push (cons (first binding) (symbol-function (first binding)))
              saved)
            (setf (symbol-function (first binding)) (second binding)))
          (funcall thunk))
      (dolist (pair saved)
        (setf (symbol-function (car pair)) (cdr pair))))))

(defmacro th-atr-with-stubs (bindings &body body)
  "Run BODY with the symbol-function stubs in BINDINGS installed (a
literal list of (function-name lambda) pairs, or any expression
evaluating to one)."
  `(th-atr-call-with-stubs ,bindings (lambda () ,@body)))

(defun th-atr-handler-stubs (&key (token "atr-token-admin") tree render-json)
  "The transport-only stubs: token extraction (get-bearer-token /
get-access-token) and validation (validate-access-token against the
fixed token map), record-usage (no-op), parse-posted-json (TREE),
abort-request (signals atr-aborted with the status code), and
render-output (identity — or left real when RENDER-JSON, so the
caller receives the actual plist-to-json wire output). Everything
authorization-shaped (require-auth, require-auth-with-query-token,
get-type-roles, the RBAC checks, the backend operation) runs for
real: stubbing require-auth would make the central regression —
the role-membership preflight — untestable."
  (let ((stubbed
          (list
            (list 'get-bearer-token (lambda () token))
            (list 'get-access-token (lambda () token))
            (list 'validate-access-token
              (lambda (tok)
                (cdr (assoc tok *atr-token-users* :test #'equal))))
            (list 'record-usage
              (lambda (&rest args) (declare (ignore args)) nil))
            (list 'parse-posted-json (lambda () tree))
            (list 'abort-request
              (lambda (error-code message &optional details-plist)
                (declare (ignore details-plist))
                (error 'atr-aborted :code error-code
                  :message message))))))
    (if render-json
      stubbed
      (append stubbed
        (list (list 'render-output (lambda (result) result)))))))

(defun th-atr-strip-harness-keys (args)
  "Remove the harness-only keys (:token :tree :render-json) from a
handler argument plist."
  (loop for (key val) on args by #'cddr
    unless (member key '(:token :tree :render-json))
    append (list key val)))

(defun th-atr-call (fn &rest args &key (token "atr-token-admin") tree
                     (render-json nil)
                   &allow-other-keys)
  "Call the REST easy-handler FN with ARGS under the atr stub
harness. TOKEN selects the caller (nil = missing token,
\"atr-token-bogus\" = a token no user is behind); TREE is the
parse-posted-json body; RENDER-JSON leaves render-output real so the
return value is the JSON string. Binds a fake h:*request* (when
unbound) and a fresh h:*reply*: be-rec's cap decoration reaches
h:*reply* (the al-rest-item-view-only precedent)."
  (let ((handler-args (th-atr-strip-harness-keys args)))
    (th-atr-call-with-stubs
        (th-atr-handler-stubs :token token :tree tree
          :render-json render-json)
      (lambda ()
        (if (boundp 'h:*request*)
          (let ((h:*reply* (make-instance 'h:reply)))
            (apply fn handler-args))
          (let ((h:*request* (th-atr-fake-request))
                (h:*reply* (make-instance 'h:reply)))
            (apply fn handler-args)))))))

(defun th-atr-rest-list (&rest args) (apply #'th-atr-call #'rest-list args))
(defun th-atr-rest-item (&rest args) (apply #'th-atr-call #'rest-item args))
(defun th-atr-rest-id (&rest args) (apply #'th-atr-call #'rest-id args))
(defun th-atr-rest-value-id (&rest args)
  (apply #'th-atr-call #'rest-value-id args))
(defun th-atr-rest-value (&rest args) (apply #'th-atr-call #'rest-value args))
(defun th-atr-rest-column (&rest args)
  (apply #'th-atr-call #'rest-column args))
(defun th-atr-rest-insert (&rest args)
  (apply #'th-atr-call #'rest-insert args))
(defun th-atr-rest-upload (&rest args)
  (apply #'th-atr-call #'rest-upload args))
(defun th-atr-rest-update (&rest args)
  (apply #'th-atr-call #'rest-update args))
(defun th-atr-rest-delete (&rest args)
  (apply #'th-atr-call #'rest-delete args))
(defun th-atr-rest-actions (&rest args)
  (apply #'th-atr-call #'rest-actions args))
(defun th-atr-rest-file (&rest args) (apply #'th-atr-call #'rest-file args))
(defun th-atr-rest-validate-field (&rest args)
  (apply #'th-atr-call #'rest-validate-field args))
(defun th-atr-rest-validate-form (&rest args)
  (apply #'th-atr-call #'rest-validate-form args))

;;; ---------------------------------------------------------------------------
;;; Preconditions
;;; ---------------------------------------------------------------------------

(test atr-preconditions
  "The fixture says what the tests need it to say: Notes is
admin-only (:type-roles (\"admin\")), guest and atr-writer hold
none of those roles, guest holds public, the shared row (public +
row-writer) is readable by both and writable by the writer, and the
private row is invisible to both."
  (th-atr-reset)
  (let ((shared (th-atr-note-id "shared"))
        (private (th-atr-note-id "private")))
    (is-true shared)
    (is-true private)
    (is (equal '("admin") (get-type-roles :notes)))
    (is-false (a:user-allowed *rbac* "guest" "read"
                (type-resource-name :notes)))
    (is-false (a:user-allowed *rbac* "atr-writer" "read"
                (type-resource-name :notes)))
    (is-true (a:user-has-role *rbac* "guest" "public"))
    ;; row access: the shared row, not the private one
    (is-true (user-allowed-resource "guest" shared "read"))
    (is-true (user-allowed-resource "atr-writer" shared "read"))
    (is-false (user-allowed-resource "guest" private "read"))
    (is-false (user-allowed-resource "atr-writer" private "read"))
    (is-true (user-allowed-resource "atr-writer" shared "update"))
    (is-true (user-allowed-resource "atr-writer" shared "delete"))
    (is-false (user-allowed-resource "atr-writer" private "update"))
    (is-false (user-allowed-resource "atr-writer" private "delete"))))

;;; ---------------------------------------------------------------------------
;;; Backend list gate
;;; ---------------------------------------------------------------------------

(test atr-be-list-row-read-decides
  "Direct be-list / be-list-column as guest return only the shared
row / value with a readable-rows total; a caller with no readable
rows gets a successful empty result rather than a type-permission
error."
  (th-atr-reset)
  (let* ((result (be-list :notes "guest"))
         (names (mapcar (lambda (r) (getf r :name))
                     (getf result :records))))
    (is (equal '("shared") names))
    (is (= 1 (getf result :total))))
  ;; be-list-column inherits the corrected rule through delegation
  (is (equal '("shared")
        (getf (be-list-column :notes :name "guest") :values)))
  ;; no readable rows: every fresh user holds public (the base-model
  ;; seed), and the shared row is public-readable, so a no-grants
  ;; caller has to lose that role first. Empty success, not an error.
  (th-make-user "atr-nobody")
  (a:remove-user-role *rbac* "atr-nobody" "public")
  (unwind-protect
      (let ((result (be-list :notes "atr-nobody")))
        (is (null (getf result :records)))
        (is (equal 0 (getf result :total))))
    (th-atr-delete-user "atr-nobody")))

(test atr-list-capability-split
  "Envelope create/update/delete stay type-level (admin-only Notes:
:false for the writer) while the per-record update/delete flags
answer the row question (:true on the shared row for the writer,
:false for the guest). Private rows never appear."
  (th-atr-reset)
  (let* ((result (be-list :notes "atr-writer"))
         (records (getf result :records)))
    (is (equal :false (getf result :create)))
    (is (equal :false (getf result :update)))
    (is (equal :false (getf result :delete)))
    (let ((shared (car records)))
      (is (equal "shared" (getf shared :name)))
      (is (equal :true (getf shared :update)))
      (is (equal :true (getf shared :delete)))))
  (let ((shared (car (getf (be-list :notes "guest") :records))))
    (is (equal :false (getf shared :update)))
    (is (equal :false (getf shared :delete)))))

;;; ---------------------------------------------------------------------------
;;; Authentication still required on the changed endpoints
;;; ---------------------------------------------------------------------------

(defun th-atr-read-calls (token shared)
  "Labelled thunks calling every changed existing-row read endpoint
as the caller behind TOKEN — the auth matrix and the shared-row
matrix iterate these."
  (list
    (cons "list"
      (lambda () (th-atr-rest-list :type "notes" :token token)))
    (cons "item"
      (lambda () (th-atr-rest-item :type "notes" :id shared
                   :form "update-form" :token token)))
    (cons "id"
      (lambda ()
        (th-atr-rest-id :type "notes"
          :filters "[[\"notes\",\"name\",\"eq\",\"shared\"]]"
          :token token)))
    (cons "value-id"
      (lambda ()
        (th-atr-rest-value-id :type "notes" :field "name"
          :value "shared" :token token)))
    (cons "value"
      (lambda ()
        (th-atr-rest-value :type "notes" :id shared :field "body"
          :form "update-form" :token token)))
    (cons "column"
      (lambda ()
        (th-atr-rest-column :type "notes" :field "name"
          :token token)))))

(test atr-rest-auth-still-required
  "Missing and invalid tokens still abort 401 on every changed read
endpoint — removing the role-membership preflight did not remove
authentication."
  (th-atr-reset)
  (let ((shared (th-atr-note-id "shared")))
    (dolist (token '(nil "atr-token-bogus"))
      (dolist (call (th-atr-read-calls token shared))
        (handler-case
            (progn
              (funcall (cdr call))
              (fail "~a: expected a 401 abort" (car call)))
          (atr-aborted (e)
            (is (= 401 (atr-aborted-code e)) "~a" (car call))))))))

(test atr-rest-write-auth-still-required
  "The same 401 for update, delete, actions, and file with a missing
token (the file arm also carries its own 401 inside its test)."
  (th-atr-reset)
  (let ((shared (th-atr-note-id "shared")))
    (dolist (call
             (list
               (cons "update"
                 (lambda ()
                   (th-atr-rest-update
                     :tree `(:type "notes" :filters ,shared
                             :data (:body "x"))
                     :token nil)))
               (cons "delete"
                 (lambda ()
                   (th-atr-rest-delete
                     :tree `(:type "notes" :filters ,shared)
                     :token nil)))
               (cons "actions"
                 (lambda ()
                   (th-atr-rest-actions
                     :tree `(:type "notes" :id ,shared
                             :field "test-sync")
                     :token nil)))))
      (handler-case
          (progn
            (funcall (cdr call))
            (fail "~a: expected a 401 abort" (car call)))
        (atr-aborted (e)
          (is (= 401 (atr-aborted-code e)) "~a" (car call)))))))

;;; ---------------------------------------------------------------------------
;;; The defining case: row access without type roles, through REST
;;; ---------------------------------------------------------------------------

(test atr-rest-guest-reads-shared-row
  "Guest (public role, no Notes type role) reads the shared row
through every existing-row read endpoint and never learns the
private row exists. Identity lookups use non-UUID values — be-id's
pre-existing direct-UUID early return is a separate backend
contract this item does not change."
  (th-atr-reset)
  (let* ((shared (th-atr-note-id "shared"))
         (private (th-atr-note-id "private")))
    ;; list
    (let ((names (mapcar (lambda (r) (getf r :name))
                     (getf (th-atr-rest-list :type "notes"
                              :token "atr-token-guest")
                       :records))))
      (is (equal '("shared") names)))
    ;; item
    (let ((result (th-atr-rest-item :type "notes" :id shared
                    :form "update-form" :token "atr-token-guest")))
      (is (equal "shared" (getf (getf result :record) :name))))
    (let ((result (th-atr-rest-item :type "notes" :id private
                    :form "update-form" :token "atr-token-guest")))
      (is (null result)))
    ;; id via identity filters
    (is (equal shared
          (th-atr-rest-id :type "notes"
            :filters "[[\"notes\",\"name\",\"eq\",\"shared\"]]"
            :token "atr-token-guest")))
    (is (null (th-atr-rest-id :type "notes"
                :filters "[[\"notes\",\"name\",\"eq\",\"private\"]]"
                :token "atr-token-guest")))
    ;; value-id — the backlog omission
    (is (equal shared
          (th-atr-rest-value-id :type "notes" :field "name"
            :value "shared" :token "atr-token-guest")))
    (is (null (th-atr-rest-value-id :type "notes" :field "name"
                :value "private" :token "atr-token-guest")))
    ;; value with an explicit form (the handler's default-form typo
    ;; is out of scope)
    (is (equal "v1"
          (th-atr-rest-value :type "notes" :id shared :field "body"
            :form "update-form" :token "atr-token-guest")))
    (is (null (th-atr-rest-value :type "notes" :id private
                :field "body" :form "update-form"
                :token "atr-token-guest")))
    ;; column
    (is (equal '("shared")
          (getf (th-atr-rest-column :type "notes" :field "name"
                   :token "atr-token-guest")
            :values)))))

(test atr-rest-writer-updates-shared-row
  "atr-writer (row-writer, no Notes type role) updates the shared
row through /api/update; guest and writer updates of rows they hold
no grant on are denied with the backend's validation-error 400 and
change nothing."
  (th-atr-reset)
  (let ((shared (th-atr-note-id "shared")))
    ;; writer: the defining write case
    (let ((result (th-atr-rest-update
                    :tree `(:type "notes" :filters ,shared
                            :data (:body "writer-v2"))
                    :token "atr-token-writer")))
      (is (equal shared result))
      (is (equal "writer-v2" (th-atr-body "shared"))))
    ;; guest: no update grant anywhere → 400, body unchanged
    (handler-case
        (progn
          (th-atr-rest-update
            :tree `(:type "notes" :filters ,shared
                    :data (:body "nope"))
            :token "atr-token-guest")
          (fail "guest update should have been denied"))
      (atr-aborted (e) (is (= 400 (atr-aborted-code e)))))
    (is (equal "writer-v2" (th-atr-body "shared")))
    ;; writer on the private row: denied, unchanged
    (let ((private (th-atr-note-id "private")))
      (handler-case
          (progn
            (th-atr-rest-update
              :tree `(:type "notes" :filters ,private
                      :data (:body "nope"))
              :token "atr-token-writer")
            (fail "writer update of the private row should have been denied"))
        (atr-aborted (e) (is (= 400 (atr-aborted-code e)))))
      (is (equal "secret" (th-atr-body "private"))))))

(test atr-rest-delete-boundaries
  "atr-writer deletes the shared row via /api/delete; a guest delete
keeps be-delete's denied-nil convention (200 with a nil result, row
survives)."
  (th-atr-reset)
  (let ((shared (th-atr-note-id "shared")))
    ;; guest: denied → nil result, row intact
    (is (null (th-atr-rest-delete
                :tree `(:type "notes" :filters ,shared)
                :token "atr-token-guest")))
    (is-true (th-atr-note-id "shared"))
    ;; writer: deletes it
    (is (equal shared
          (th-atr-rest-delete
            :tree `(:type "notes" :filters ,shared)
            :token "atr-token-writer")))
    (is-false (th-atr-note-id "shared"))))

(test atr-rest-actions-row-gated
  "atr-writer runs the synchronous action on the shared row with no
type role; a guest press is denied by be-action's row-update gate
(400) before the status is stamped or the hook runs."
  (th-atr-reset)
  ;; writer: completes
  (let* ((shared (th-atr-note-id "shared"))
         (result (th-atr-rest-actions
                   :tree `(:type "notes" :id ,shared
                           :field "test-sync")
                   :token "atr-token-writer")))
    (is (equal "complete" (getf result :status))))
  ;; guest: denied before the status stamp / hook
  (th-atr-reset)
  (handler-case
      (progn
        (th-atr-rest-actions
          :tree `(:type "notes" :id ,(th-atr-note-id "shared")
                  :field "test-sync")
          :token "atr-token-guest")
        (fail "guest action should have been denied"))
    (atr-aborted (e) (is (= 400 (atr-aborted-code e)))))
  (let* ((shared (th-atr-note-id "shared"))
         (record (getf (rec shared "admin" :type-key :notes) :record)))
    ;; the companion status field rests at its idle default — the
    ;; denial came before any status stamp or hook run
    (is (equal "idle" (getf record :test-sync-status)))))

;;; ---------------------------------------------------------------------------
;;; /api/file
;;; ---------------------------------------------------------------------------

(defun th-atr-file-stubs (shared private tmp)
  "Filesystem-transport stubs for /api/file: fs-path (fixed temp
file), find-resource-name (path → the real Notes resource names),
and h:handle-static-file (captures the served path). The
authentication helper and the a:user-allowed read check on the
resolved resource run for real."
  (list
    (list 'fs-path
      (lambda (type-key path)
        (declare (ignore type-key path))
        tmp))
    (list 'find-resource-name
      (lambda (type-key filters)
        (declare (ignore type-key))
        ;; The handler builds (type path-field :eq path); the value
        ;; is the fourth element (path-field is nil on this fixture).
        (let ((path (fourth (first filters))))
          (cond
            ((equal path "/shared.txt")
              (id-to-resource-name shared))
            ((equal path "/private.txt")
              (id-to-resource-name private))
            (t nil)))))
    (list 'h:handle-static-file
      (lambda (fs-path &optional content-type)
        (declare (ignore content-type))
        fs-path))))

(test atr-rest-file-row-gated
  "/api/file authenticates, then checks read on the file row's own
resource: the writer serves the shared file (the query-token arm is
the same path past get-access-token), is 403 on the private file,
404 on a missing resource, and 401 without a token."
  (th-atr-reset)
  (let* ((shared (th-atr-note-id "shared"))
         (private (th-atr-note-id "private"))
         (tmp "/tmp/atr-rest-file.txt"))
    (u:spew "atr file" tmp)
    (th-atr-with-stubs (th-atr-file-stubs shared private tmp)
      ;; writer, bearer auth: served
      (is (equal tmp
            (th-atr-rest-file :type "notes" :path "/shared.txt"
              :token "atr-token-writer")))
      ;; query-token auth: the same decision past get-access-token
      (is (equal tmp
            (th-atr-rest-file :type "notes" :path "/shared.txt"
              :token "atr-token-writer")))
      ;; private file: authenticated, no row read → 403
      (handler-case
          (progn
            (th-atr-rest-file :type "notes" :path "/private.txt"
              :token "atr-token-writer")
            (fail "expected 403"))
        (atr-aborted (e) (is (= 403 (atr-aborted-code e)))))
      ;; missing resource → 404
      (handler-case
          (progn
            (th-atr-rest-file :type "notes" :path "/nope.txt"
              :token "atr-token-writer")
            (fail "expected 404"))
        (atr-aborted (e) (is (= 404 (atr-aborted-code e)))))
      ;; missing token → 401
      (handler-case
          (progn
            (th-atr-rest-file :type "notes" :path "/shared.txt"
              :token nil)
            (fail "expected 401"))
        (atr-aborted (e) (is (= 401 (atr-aborted-code e))))))))

;;; ---------------------------------------------------------------------------
;;; Preserved boundaries
;;; ---------------------------------------------------------------------------

(test atr-create-boundaries-unchanged
  "Create and pre-row validation keep their type-role preflights:
/api/insert and the validate endpoints 401 for guest and writer,
direct be-insert still fails type create, and the admin control
insert succeeds."
  (th-atr-reset)
  (th-make-user "atr-plain")
  (unwind-protect
      (progn
        ;; REST insert preflight: 401 for callers without the type role
        (dolist (token '("atr-token-guest" "atr-token-writer"))
          (handler-case
              (progn
                (th-atr-rest-insert
                  :tree `(:type "notes"
                          :data (:name ,(format nil "~a-note" token)))
                  :token token)
                (fail "~a insert should have been denied" token))
            (atr-aborted (e) (is (= 401 (atr-aborted-code e))))))
        ;; direct be-insert: type create still required
        (handler-case
            (progn
              (be-insert :notes '(:name "plain-note") "atr-plain")
              (fail "be-insert should have been denied"))
          (validation-error ()
            (is-false (th-atr-note-id "plain-note"))))
        ;; admin control: insert works
        (is-true (be-insert :notes '(:name "admin-note") "admin"))
        ;; validate-field / validate-form keep their preflights
        (dolist (token '("atr-token-guest" "atr-token-writer"))
          (handler-case
              (progn
                (th-atr-rest-validate-field
                  :tree '(:type "notes" :field "name" :value "x")
                  :token token)
                (fail "validate-field should have been denied"))
            (atr-aborted (e) (is (= 401 (atr-aborted-code e)))))
          (handler-case
              (progn
                (th-atr-rest-validate-form
                  :tree '(:type "notes" :data (:name "x"))
                  :token token)
                (fail "validate-form should have been denied"))
            (atr-aborted (e) (is (= 401 (atr-aborted-code e)))))))
    (th-atr-delete-user "atr-plain")))

(test atr-upload-preflight-unchanged
  "/api/upload keeps its type-role preflight (focused assertion):
a multipart-shaped request from guest or writer dies at require-auth
with 401 before any filesystem work. Full upload behavior is
covered by the file suites."
  (th-atr-reset)
  (th-atr-with-stubs
      (list
        (list 'h:header-in*
          (lambda (name &optional request)
            (declare (ignore request))
            (when (eq name :content-type) "multipart/form-data")))
        (list 'h:post-parameter
          (lambda (name &optional request)
            (declare (ignore request))
            (if (equal name "type") "notes" '("/tmp/atr-upload"))))
        (list 'fs-path
          (lambda (type-key path)
            (declare (ignore type-key path))
            "/tmp/atr-upload")))
    (dolist (token '("atr-token-guest" "atr-token-writer"))
      (handler-case
          (progn
            (th-atr-rest-upload :token token)
            (fail "~a upload should have been denied" token))
        (atr-aborted (e) (is (= 401 (atr-aborted-code e))))))))

(test atr-builtin-list-gates-unchanged
  "Built-in types keep their type-level list gates: :users
(admin-only on this fixture) and :usage deny a non-admin list.
Direct be-list signals the validation error; the REST list — now
without the membership preflight — surfaces the backend denial
through its existing 400 path instead of the old 401."
  (th-atr-reset)
  ;; :users — the built-in base control
  (signals validation-error (be-list :users "atr-writer"))
  (handler-case
      (progn
        (th-atr-rest-list :type "users" :token "atr-token-writer")
        (fail "users list should have been denied"))
    (atr-aborted (e) (is (= 400 (atr-aborted-code e)))))
  ;; :usage — the built-in non-base arm (al-non-admin-list-denied's
  ;; contract, asserted through the new REST path)
  (signals validation-error (be-list :usage "atr-writer"))
  (handler-case
      (progn
        (th-atr-rest-list :type "usage" :token "atr-token-writer")
        (fail "usage list should have been denied"))
    (atr-aborted (e) (is (= 400 (atr-aborted-code e))))))

(test atr-tab-visibility-unchanged
  "Row access does not make the type tab visible: be-types omits
admin-only Notes for guest and writer, and the /api/types wire
response carries no notes entry."
  (th-atr-reset)
  (dolist (user '("guest" "atr-writer"))
    (is-false (find :notes (be-types user)
                 :key (lambda (e) (getf e :name)))))
  (let ((json (th-atr-call #'rest-types :token "atr-token-guest"
                :render-json t)))
    (is (search "\"status\":\"success\"" json))
    (is-false (search "\"name\":\"notes\"" json))))

;;; ---------------------------------------------------------------------------
;;; Wire-level serialization
;;; ---------------------------------------------------------------------------

(test atr-wire-serialization
  "Wire-level: the shared-row read envelope is a normal success
with JSON-boolean capability flags; an empty readable set is a
successful empty result, not a fabricated 401/404; private ids and
values do not leak."
  (th-atr-reset)
  (let* ((private (th-atr-note-id "private"))
         (list-json (th-atr-rest-list :type "notes"
                       :token "atr-token-guest" :render-json t)))
    (is (search "\"status\":\"success\"" list-json))
    (is (search "\"name\":\"shared\"" list-json))
    (is (search "\"total\":1" list-json))
    (is-false (search "private" list-json))
    (is-false (search private list-json))
    ;; capability flags are JSON booleans, never [] / truthy arrays
    (is (search "\"update\":false" list-json))
    (is (search "\"delete\":false" list-json)))
  ;; empty readable set: successful empty result (fresh users hold
  ;; public, so the caller loses it first — see
  ;; atr-be-list-row-read-decides)
  (th-make-user "atr-empty")
  (a:remove-user-role *rbac* "atr-empty" "public")
  (unwind-protect
      (let ((json (th-atr-rest-list :type "notes"
                     :token "atr-token-empty" :render-json t)))
        (is (search "\"status\":\"success\"" json))
        (is (search "\"total\":0" json))
        (is (search "\"records\":[]" json)))
    (th-atr-delete-user "atr-empty")))

;;; ---------------------------------------------------------------------------
;;; Rollup negative control (measure-rollup-test context)
;;; ---------------------------------------------------------------------------

(in-suite api-type-roles-rollup-control-suite)

(test atr-rollup-list-gate-intact
  "The ordinary-type relaxation is category-gated: on the
measure-rollup-test fixture, a user without the viewer role still
fails direct be-list and the REST list on :user-leaderboard (the
backend validation error, surfacing as 400 through the handler)."
  (th-make-user "atr-rollup-plain")
  (unwind-protect
      (progn
        (signals validation-error
          (be-list :user-leaderboard "atr-rollup-plain"))
        (handler-case
            (progn
              (th-atr-rest-list :type "user-leaderboard"
                :token "atr-token-rollup-plain")
              (fail "rollup list should have been denied"))
          (atr-aborted (e) (is (= 400 (atr-aborted-code e))))))
    (th-atr-delete-user "atr-rollup-plain")))
