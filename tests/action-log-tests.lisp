(in-package :data-ui)

(def-suite action-log-suite
  :description ":usage behavior tests (test-model context)")

(def-suite action-log-compile
  :description ":usage compile probes (bare compile-model, no DB)")

(def-suite action-log-guest-suite
  :description "Guest login usage tests (guest-allowed-test context)")

(in-suite action-log-suite)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun th-al-usage-ids (&key (user-name "admin"))
  "All :usage row ids for USER-NAME, newest first (:created-at :desc
is the type default). Direct be-list read as admin."
  (mapcar (lambda (r) (getf r :id))
    (getf (be-list :usage "admin"
             :filters `((:usage :user-name :eq ,user-name)))
      :records)))

(defun th-al-usage-count (&rest keys &key (user-name "admin"))
  (declare (ignore keys))
  (length (set-difference (th-al-usage-ids :user-name user-name)
              *al-baseline-ids* :test #'equal)))

(defparameter *al-cleanup-ids* nil
  "Usage row ids created by the running test; removed on exit.")

(defparameter *al-baseline-ids* nil
  "Usage row ids that existed when th-al-with-rows opened; the
rest-handler tests count only rows new since then (with-model
resets once per suite, not per test, so earlier tests' rows are
still in the table).")

(defun th-al-rest-list (&rest args &key type &allow-other-keys)
  "Call the rest-list easy-handler function directly. Hunchentoot's
easy handlers take their parameters as keyword arguments at call
time; the stub harness replaces the Hunchentoot-touching helpers."
  (declare (ignore type))
  (apply #'rest-list args))

(defun th-al-rest-item (&rest args)
  (apply #'rest-item args))

(defun th-al-rest-insert (&rest args)
  (apply #'rest-insert args))

(defun th-al-rest-update (&rest args)
  (apply #'rest-update args))

(defun th-al-rest-delete (&rest args)
  (apply #'rest-delete args))

(defun th-al-rest-actions (&rest args)
  (apply #'rest-actions args))

(defun th-al-rest-login (&rest args)
  (apply #'rest-login args))

(defun th-al-record-and-cleanup (user-name event &rest args)
  "Call record-usage and register any created rows for removal in
an enclosing th-al-with-rows. Returns the id of the new row, or
nil when nothing was inserted."
  (let ((before (th-al-usage-ids :user-name user-name)))
    (apply #'record-usage user-name event args)
    (let* ((after (th-al-usage-ids :user-name user-name))
           (new (set-difference after before :test #'equal)))
      (dolist (id new) (push id *al-cleanup-ids*))
      (car new))))

(defmacro th-al-with-rows (&body body)
  "Run BODY with usage-row cleanup: every :usage resource created
inside (tracked by th-al-record-and-cleanup) is removed via
a:remove-resource on every exit, including a signal. Cleanup is
a:remove-resource, not be-delete — the type is :delete nil and the
append-only gate rejects be-delete of :usage anyway; removing the
resources row takes the usage row with it (the FK from usage.id)."
  `(let ((*al-cleanup-ids* nil)
         (*al-baseline-ids* (th-al-usage-ids)))
     (unwind-protect
         (progn ,@body)
       ;; Remove every row that appeared during the body: the
       ;; tracked ids (th-al-record-and-cleanup) plus any the
       ;; handler calls wrote (they are never tracked — the writer
       ;; is record-usage inside rest-*). Rows from earlier tests
       ;; are baseline and stay untouched.
       (let ((new (set-difference (th-al-usage-ids) *al-baseline-ids*
                    :test #'equal)))
         (dolist (id (append (reverse *al-cleanup-ids*) new))
           (let ((name (id-to-resource-name id)))
             (when name (a:remove-resource *rbac* name))))))))

(defun th-al-call-with-stubs (bindings thunk)
  "Install symbol-function stubs from BINDINGS — a list of
(fn (quote-lambda)) entries where each lambda is evaluated — for
the duration of THUNK, restoring the real definitions on every
exit including a signal. Binds h:*request* to a bare fake request
when unbound: the rest-* handlers read real-remote-addr for
record-usage's :ip argument, and the direct handler calls run
outside any request context (al-rest-item-view-only's explicit
h:*reply* binding is the precedent). A real request context, if
one exists, is left alone."
  (let ((saved nil))
    (unwind-protect
        (progn
          (dolist (binding bindings)
            (let ((fn (first binding))
                  (lambda (second binding)))
              (push (cons fn (symbol-function fn)) saved)
              (setf (symbol-function fn) lambda)))
          (if (boundp 'h:*request*)
            (funcall thunk)
            (let ((h:*request* (th-al-fake-request)))
              (funcall thunk))))
      (dolist (pair saved)
        (setf (symbol-function (car pair)) (cdr pair))))))

(defmacro th-al-with-stubs (bindings &body body)
  "Run BODY with symbol-function stubs installed. BINDINGS is a
literal list of (function-name lambda) pairs, or any expression
evaluating to one (al-handler-stubs returns one)."
  `(th-al-call-with-stubs ,bindings (lambda () ,@body)))

(defun th-al-fake-request ()
  "A bare Hunchentoot request object for direct handler calls:
headers-in alist (X-Forwarded-For naming a documentation
address, so real-remote-addr returns it — record-usage's :ip
normalization is exercised on every handler test), method, uri,
and addresses. initialize-instance :after touches h:*reply*
(a bad-request return-code), h:*session* (session-verify's
writer), and h:*acceptor* (session-verify reads its
session-cookie-name for the cookie lookup) — all three bound
around the make-instance call, never left dynamic-extent over
the test body. The acceptor slot stays nil: only handle-request
reads it, and these objects never reach one."
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

(define-condition al-aborted (error)
  ((code :initarg :code :reader al-aborted-code)
   (message :initarg :message :reader al-aborted-message))
  (:documentation "Stand-in for hunchentoot's abort-request-handler:
the abort-* family must not return, and a stub that returned nil
would let execution fall into the logging call the abort was
supposed to prevent."))

(defun al-abort-stub ()
  (lambda (&rest args)
    (declare (ignore args))
    (error 'al-aborted :code 401 :message "aborted")))

(defun al-auth-stub (&optional (user "admin"))
  (lambda (&rest args)
    (declare (ignore args))
    user))

(defun al-json-stub (&optional (tree nil tree-p))
  (if tree-p
    (lambda () tree)
    (lambda () nil)))

(defun al-render-stub ()
  (lambda (result) result))

(defun al-handler-stubs (&key (user "admin") (tree nil tree-p))
  "The four Hunchentoot-touching functions the REST handlers reach:
require-auth, parse-posted-json, render-output, abort-request. Note
render-output must NOT be stubbed for handlers whose result path
touches Hunchentoot state deeper than content-type* (be-rec's cap
decoration reaches h:*reply* via a postmodern/hunchentoot side
effect); those tests bind a real *reply* instead."
  (declare (ignore tree-p))
  (list
    (list 'require-auth (al-auth-stub user))
    (list 'parse-posted-json (al-json-stub tree))
    (list 'render-output (al-render-stub))
    (list 'abort-request (al-abort-stub))))

;;; ---------------------------------------------------------------------------
;;; record-usage behavior
;;; ---------------------------------------------------------------------------

(test al-insert-and-read-back
  "record-usage inserts one row; a second identical call inside the
window inserts nothing; be-list as admin reads it back with the
right values."
  (th-al-with-rows
    (let ((id (th-al-record-and-cleanup "admin" :list
                 :on :todos :detail "milk")))
      (is-true (uuid-p id))
      ;; dedup: same call again writes nothing
      (is (null (th-al-record-and-cleanup "admin" :list
               :on :todos :detail "milk")))
      ;; different detail writes
      (is-true (th-al-record-and-cleanup "admin" :list
                 :on :todos :detail "bread"))
      (let ((records (getf (be-list :usage "admin"
                            :filters `((:usage :id :eq ,id)))
                      :records)))
        (is (= 1 (length records)))
        (let ((r (car records)))
          (is (equal "admin" (getf r :user-name)))
          (is (equal "list" (getf r :event)))
          (is (equal "todos" (getf r :on)))
          (is (equal "milk" (getf r :detail)))
          (is (equal "admin" (getf r :user)))
          (is (and (stringp (getf r :when))
                (re:scan *timestamp-regex* (getf r :when))
                (search "T" (getf r :when)))))))))

(test al-normalization-before-checks
  "Normalization runs before the vocabulary check, the guard, and
the dedup: \" milk \" trims to the same key, \"LIST\"/\"TODOS\" is
the same event, and \"USAGE\" cannot sneak past the self-log guard."
  (th-al-with-rows
    (is-true (th-al-record-and-cleanup "admin" :list
               :on :todos :detail "milk"))
    (is (null (th-al-record-and-cleanup "admin" "LIST"
             :on "TODOS" :detail " milk ")))
    (is (null (th-al-record-and-cleanup "admin" :list
             :on "USAGE" :detail "bread")))))

(test al-blank-detail-is-null
  "nil and \"\" details are the same key (both store NULL); two of
them inside the window produce one row. The SQL column is null,
not an absent plist key."
  (th-al-with-rows
    (let ((id (th-al-record-and-cleanup "admin" :list :on :todos)))
      (is-true id)
      (is (null (th-al-record-and-cleanup "admin" :list
               :on :todos :detail "")))
      (is (equal :null
            (query (format nil "select usage_detail from usage ~
                                 where id = '~a'" id)
              :result-type :single))))))

(test al-same-second-two-rows
  "The id-from-data bypass: two :dedup nil calls with identical data
in the same second produce two rows with two distinct ids. If the
bypass is missing, id-from-data finds the first row and the second
is dropped."
  (th-al-with-rows
    (let ((id-1 (th-al-record-and-cleanup "admin" :login :dedup nil))
          (id-2 (th-al-record-and-cleanup "admin" :login :dedup nil)))
      (is-true (uuid-p id-1))
      (is-true (uuid-p id-2))
      (is (not (equal id-1 id-2))))))

(test al-unknown-user-null-fk
  "A failed login for an unknown name stores NULL in :user and the
attempted name in :user-name."
  (th-al-with-rows
    (let ((id (th-al-record-and-cleanup "nobody" :login-failed :dedup nil)))
      (is-true id)
      (is (equal :null
            (query (format nil "select usage_user from usage where id = '~a'"
                     id)
              :result-type :single)))
      (let ((records (getf (be-list :usage "admin"
                            :filters `((:usage :id :eq ,id)))
                      :records)))
        (is (= 1 (length records)))
        (is (equal "nobody" (getf (car records) :user-name)))))))

(test al-self-log-guard-before-dedup
  "(record-usage \"admin\" :list :on :usage) writes nothing; the
guard runs before usage-recent-p (no query, no insert)."
  (th-al-with-rows
    (is (null (th-al-record-and-cleanup "admin" :list :on :usage)))))

(test al-unknown-event-swallowed
  "(record-usage \"admin\" :nope) returns nil and inserts nothing
(the vocabulary guard, swallowed by the handler-case)."
  (th-al-with-rows
    (is (null (record-usage "admin" :nope)))
    (is (null (th-al-record-and-cleanup "admin" :nope)))))

(test al-usage-insert-error-swallowed
  "A usage-insert that signals is swallowed: record-usage returns
nil and inserts nothing (the handler-case pins it)."
  (th-al-with-rows
    (th-al-with-stubs
        (list (list 'usage-insert
                (lambda (&rest args)
                  (declare (ignore args))
                  (error "stub failure"))))
      (is (null (record-usage "admin" :list :on :todos)))
      (is (= 0 (th-al-usage-count))))))

(test al-handler-survives-usage-failure
  "The handler contract: with usage-insert stubbed to signal,
rest-list still returns its result and the primary operation
committed (todos listed), while the usage row is absent."
  (th-al-with-rows
    (let ((todo-id (be-insert :todos '(:name "al-survivor") "admin")))
      (unwind-protect
          (th-al-with-stubs
              (list (list 'usage-insert
                      (lambda (&rest args)
                        (declare (ignore args))
                        (error "stub failure"))))
            (th-al-with-stubs (al-handler-stubs)
              (finishes
                (let ((result (th-al-rest-list :type "todos")))
                  (declare (ignore result))
                  (is (plusp (th-al-usage-count-from-todos)))))
              (is (= 0 (th-al-usage-count)))))
        (be-delete :todos todo-id "admin")))))

(defun th-al-usage-count-from-todos ()
  "The todos rows the stubbed rest-list fetched (sanity: the
primary operation ran)."
  (length (getf (be-list :todos "admin" :limit 1) :records)))

(test al-append-only-gates
  "be-update / be-delete of a usage row as admin signal
validation-error; the row is unchanged. be-update of a :tokens row
still succeeds (the gate did not become global)."
  (th-al-with-rows
    (let ((id (th-al-record-and-cleanup "admin" :list :on :todos)))
      (is-true id)
      (signals validation-error
        (be-update :usage id '(:detail "rewritten") "admin"))
      (signals validation-error
        (be-delete :usage id "admin"))
      (is (equal "todos"
            (query (format nil
                     "select on_type from usage where id = '~a'" id)
              :result-type :single))))
    ;; :tokens still updates (store-token's path)
    (let ((user (th-make-user "al-token-user")))
      (unwind-protect
          (finishes
            ;; first call inserts the tokens row, second updates it —
            ;; both go through be-update's carve-out for opaque
            ;; built-ins; the :usage gate must not have gone global
            (store-token user "token-1")
            (store-token user "token-2"))
        (let ((user-id (a:get-id *rbac* "users" user)))
          (when user-id (be-delete :users user-id "admin")))))))

(test al-user-delete-keeps-log
  "Deleting a user leaves that user's rows behind: the :user FK is
on delete set null, and :user-name still holds the name."
  (th-al-with-rows
    (let* ((user (th-make-user "al-doomed"))
           (user-id (a:get-id *rbac* "users" user))
           (id (th-al-record-and-cleanup "al-doomed" :list :on :todos)))
      (is-true id)
      ;; remove from cleanup tracking: the row must survive the
      ;; user delete, and we clean it up ourselves below
      (setf *al-cleanup-ids* (remove id *al-cleanup-ids* :test #'equal))
      (finishes (be-delete :users user-id "admin"))
      ;; row still there, FK null, name kept
      (is (equal :null
            (query (format nil
                     "select usage_user from usage where id = '~a'" id)
              :result-type :single)))
      (is (equal "al-doomed"
            (query (format nil
                     "select usage_user_name from usage where id = '~a'"
                     id)
              :result-type :single)))
      (a:remove-resource *rbac* (id-to-resource-name id)))))

(test al-failed-insert-cleans-resource
  "A failure after a:add-resource succeeded removes the new
resource (no leak). Baseline: the set of usage-prefixed resource
names is unchanged."
  (let ((baseline (th-al-usage-resource-names)))
    (th-al-with-stubs
        ;; insert-main-query signals before the main insert:
        ;; a:add-resource already ran inside usage-insert, then the
        ;; row insert fails
        (list (list 'insert-main-query
                (lambda (&rest args)
                  (declare (ignore args))
                  (error "stub failure"))))
      (is (null (record-usage "admin" :list :on :todos)))
      (is (equal nil
            (set-difference (th-al-usage-resource-names) baseline
              :test #'equal))))))

(defun th-al-usage-resource-names ()
  "All resource names starting with \"usage:\" — the usage rows'
resources."
  (mapcar (lambda (r) (getf r :name))
    (query "select resource_name as name from resources
             where resource_name like 'usage:%'"
      :result-type :plists)))

(test al-non-admin-list-denied
  "A be-list of :usage by a user without the role is a validation
error from valid-user-permissions, not an empty list (:type-roles
(\"usage\") — admin is force-attached at compile time, test-model
does not override the role; a user holding neither cannot read).
A user granted the \"usage\" role can read it."
  (let ((user (th-make-user "al-noob"))
        (user-with (th-make-user "al-reader" :roles '("usage"))))
    (unwind-protect
        (progn
          (signals validation-error
            (be-list :usage user))
          (finishes (getf (be-list :usage user-with) :records)))
      (dolist (u (list user user-with))
        (let ((user-id (a:get-id *rbac* "users" u)))
          (when user-id (be-delete :users user-id "admin")))))))

(test al-hour-prefix-search
  "Search finds the inserted row by its :when hour prefix; a search
for \"login\" returns both login and login-failed rows (substring
across every searchable column — login-failed contains login)."
  (th-al-with-rows
    (let* ((id (th-al-record-and-cleanup "admin" :list :on :todos))
           (when-val (query
                       (format nil
                         "select usage_when from usage where id = '~a'" id)
                       :result-type :single))
           (hour-prefix (format nil "~a:" (subseq when-val 0 13))))
      (is-true (re:scan *timestamp-regex* when-val))
      (let ((records (getf (be-list :usage "admin" :search hour-prefix)
                      :records)))
        (is (member id records :key (lambda (r) (getf r :id))
              :test #'equal))))))

;;; ---------------------------------------------------------------------------
;;; Dedup matrix
;;; ---------------------------------------------------------------------------

(test al-dedup-different-keys-write
  "Different user, event, on, or detail each write despite the
window."
  (th-al-with-rows
    (is-true (th-al-record-and-cleanup "admin" :list :on :todos
               :detail "milk"))
    (th-make-user "al-other")
    (unwind-protect
        (progn
          (is-true (th-al-record-and-cleanup "al-other" :list :on :todos
                     :detail "milk"))
          (is-true (th-al-record-and-cleanup "admin" :view :on :todos))
          (is-true (th-al-record-and-cleanup "admin" :list :on :tags
                     :detail "milk"))
          (is-true (th-al-record-and-cleanup "admin" :list :on :todos
                     :detail "bread")))
      ;; be-delete :users wants a uuid filter or filters; look the
      ;; id up first (a bare username would signal invalid-uuid)
      (let ((user-id (a:get-id *rbac* "users" "al-other")))
        (when user-id (be-delete :users user-id "admin")))
      (dolist (id (th-al-usage-ids :user-name "al-other"))
        (a:remove-resource *rbac* (id-to-resource-name id))))))

(test al-dedup-window-expires
  "A row older than 60 seconds does not collapse a new one. Move
created_at back with a direct query; do not sleep."
  (th-al-with-rows
    (let ((id (th-al-record-and-cleanup "admin" :list :on :todos)))
      (is-true id)
      (query (format nil
               "update usage set created_at = now() - interval '120 seconds'
                 where id = '~a'" id))
      (is-true (th-al-record-and-cleanup "admin" :list :on :todos)))))

(test al-ip-normalization
  ":ip trims a string and stores it verbatim; nil, blank, and
non-string write NULL (direct SQL, the guest-login test's
discipline). Never truncated to *usage-detail-max*."
  (th-al-with-rows
    (let ((id (th-al-record-and-cleanup "admin" :list :on :todos
                 :ip " 203.0.113.7 ")))
      (is-true id)
      (is (equal "203.0.113.7"
            (query (format nil
                     "select usage_ip from usage where id = '~a'" id)
              :result-type :single))))
    (dolist (ip (list nil "" "   "))
      (let ((id (th-al-record-and-cleanup "admin" :login-failed :dedup nil
                   :ip ip)))
        (is-true id)
        (is (equal :null
              (query (format nil
                       "select usage_ip from usage where id = '~a'" id)
                :result-type :single)))))))

(test al-ip-not-in-dedup-key
  "Two :list calls inside the window with the same user, type, and
detail but different :ip values collapse to one row (IP is not in
the dedup key: a shared NAT must keep collapsing repeated lists)."
  (th-al-with-rows
    (is-true (th-al-record-and-cleanup "admin" :list :on :todos
               :detail "milk" :ip "203.0.113.7"))
    (is (null (th-al-record-and-cleanup "admin" :list :on :todos
             :detail "milk" :ip "198.51.100.9")))))

(test al-dedup-offset-and-sort-via-rest
  "Offset and sort are not in the dedup key: two rest-list calls
that differ only in offset (and two in sort) produce one list row
between them. Exercised through the handler, not by passing
arguments record-usage does not have."
  (th-al-with-rows
    (th-al-with-stubs (al-handler-stubs)
      (finishes (th-al-rest-list :type "todos" :offset "0"))
      (finishes (th-al-rest-list :type "todos" :offset "20"))
      (is (= 1 (th-al-usage-count))
        "two offsets, one row")
      (finishes (th-al-rest-list :type "todos" :sort "created-at:asc"))
      (finishes (th-al-rest-list :type "todos" :sort "created-at:desc"))
      (is (= 1 (th-al-usage-count))
        "two sorts still one row (sort is not in the key)"))))

;;; ---------------------------------------------------------------------------
;;; Handler coverage (stub harness)
;;; ---------------------------------------------------------------------------

(in-suite action-log-suite)

(test al-rest-list-writes-and-dedups
  "rest-list writes one list row for :todos; a second call inside
the window does not. Blank/whitespace search writes a null detail
column; a 200-char search clamps to 200 (parse-search-param) then
caps at 80 (usage-detail) — the stored value is the composition."
  (th-al-with-rows
    (th-al-with-stubs (al-handler-stubs)
      (finishes (th-al-rest-list :type "todos"))
      (is (= 1 (th-al-usage-count)))
      (finishes (th-al-rest-list :type "todos"))
      (is (= 1 (th-al-usage-count)))
      (finishes (th-al-rest-list :type "todos" :search "   "))
      (is-false (query "select 1 from usage where usage_detail is not null
                         and usage_event = 'list'"
                  :result-type :single)
        "whitespace search stores NULL detail")
      (finishes (th-al-rest-list :type "todos"
                        :search (make-string 200 :initial-element #\x)))
      (is (equal 80
            (length (query "select usage_detail from usage
                             where usage_detail is not null limit 1"
                      :result-type :single)))))))

(test al-rest-item-view-only
  "rest-item writes a view row only for form=view-form with a real
record; update-form and a missing uuid write nothing. be-rec's cap
decoration reaches h:*reply*, so bind one (the stub harness covers
require-auth / render-output / abort-request only)."
  (th-al-with-rows
    (let ((id (be-insert :todos '(:name "al-view-me") "admin")))
      (unwind-protect
          (th-al-with-stubs
              (list (list 'require-auth (al-auth-stub))
                    (list 'abort-request (al-abort-stub)))
            (let ((h:*reply* (make-instance 'h:reply)))
              (finishes
                (th-al-rest-item :type "todos" :id id :form "view-form"))
              (is (= 1 (th-al-usage-count)))
              (is (equal (format nil "al-view-me / ~a" id)
                    (query "select usage_detail from usage
                             where usage_event = 'view'
                             order by created_at desc limit 1"
                      :result-type :single))
                "view detail names the viewed row")
              (finishes
                (th-al-rest-item :type "todos" :id id :form "update-form"))
              (is (= 1 (th-al-usage-count))
                "update-form fetch is not a view")
              (finishes
                (th-al-rest-item :type "todos" :id (u:uuid)
                  :form "view-form"))
              (is (= 1 (th-al-usage-count))
                "missing uuid writes nothing (nil :record)")))
        (be-delete :todos id "admin")))))

(test al-rest-insert-inserted-only
  "rest-insert writes one insert row only when inserted is true; a
duplicate identity returns the existing id and nil, and logs
nothing."
  (th-al-with-rows
    ;; The pair runs purely through the handler: first call inserts
    ;; (logged), second hits the identity and is not logged.
    (unwind-protect
        (progn
          (th-al-with-stubs
              (al-handler-stubs
                :tree `(:type "todos" :data (:name "al-once")))
            (finishes (th-al-rest-insert))
            (is (= 1 (th-al-usage-count))
              "first insert logged")
            (is (equal
                  (format nil "al-once / ~a"
                    (getf (car (getf (be-list :todos "admin"
                                        :filters
                                        `((:todos :name :eq "al-once")))
                                 :records))
                      :id))
                  (query "select usage_detail from usage
                           where usage_event = 'insert'
                           order by created_at desc limit 1"
                    :result-type :single))
              "insert detail names the new row")
            (th-al-with-stubs
              (al-handler-stubs
                :tree `(:type "todos" :data (:name "al-once")))
              (finishes (th-al-rest-insert))
              (is (= 1 (th-al-usage-count))
                "duplicate identity not logged"))))
      (let ((left (be-id :todos `((:todos :name :eq "al-once"))
                     "admin")))
        (when left (be-delete :todos left "admin"))))))

(test al-rest-update-writes
  "rest-update writes one update row on success; detail names the
updated row (identity / uuid) read back after the write."
  (th-al-with-rows
    (let ((id (be-insert :todos '(:name "al-upd") "admin")))
      (unwind-protect
          (th-al-with-stubs
              (al-handler-stubs
                :tree `(:type "todos" :filters ,id :data (:points 3)))
            (finishes (th-al-rest-update))
            (is (= 1 (th-al-usage-count)))
            (is (equal (format nil "al-upd / ~a" id)
                  (query "select usage_detail from usage
                           where usage_event = 'update'
                           order by created_at desc limit 1"
                    :result-type :single))))
        (be-delete :todos id "admin")))))

(test al-rest-delete-matched-only
  "rest-delete writes one delete row only when be-delete returns a
uuid; a delete that matches nothing returns nil and writes
nothing. Detail names the deleted row, pre-fetched before the
write."
  (th-al-with-rows
    (let ((id (be-insert :todos '(:name "al-gone") "admin")))
      (unwind-protect
          (th-al-with-stubs
              (al-handler-stubs :tree `(:type "todos" :filters ,id))
            (finishes (th-al-rest-delete))
            (is (= 1 (th-al-usage-count)))
            (is (equal (format nil "al-gone / ~a" id)
                  (query "select usage_detail from usage
                           where usage_event = 'delete'
                           order by created_at desc limit 1"
                    :result-type :single))))
        ;; id already deleted; a second delete matches nothing
        (th-al-with-stubs
            (al-handler-stubs :tree `(:type "todos" :filters ,id))
          (finishes (th-al-rest-delete))
          (is (= 1 (th-al-usage-count))
            "no-match delete writes nothing"))))))

(test al-rest-actions-writes-even-failed
  "rest-actions writes one action row whose detail is
\"<field-key> / <identity> / <uuid>\", including when be-action
returns (:status \"failed\" ...)."
  (th-al-with-rows
    (let ((id (be-insert :todos '(:name "al-act") "admin")))
      (unwind-protect
          (th-al-with-stubs
              (al-handler-stubs
                :tree `(:type "todos" :id ,id :field "test-error-action"))
            (finishes (th-al-rest-actions))
            (is (= 1 (th-al-usage-count)))
            (is (equal
                  (format nil "test-error-action / al-act / ~a" id)
                  (query
                    "select usage_detail from usage
                     where usage_event = 'action'
                     order by created_at desc limit 1"
                    :result-type :single))))
        (be-delete :todos id "admin")))))

(test al-rest-login-paths
  "rest-login writes login on success, login-failed before the
abort on a bad password (non-empty username), nothing on a blank
username. login-user-id is stubbed (rest-login does not call
require-auth)."
  (th-al-with-rows
    ;; success
    (th-al-with-stubs
        (list (list 'login-user-id
                (lambda (name password)
                  (declare (ignore password))
                  (a:get-id *rbac* "users" name)))
              (list 'parse-posted-json
                (lambda () '(:username "admin" :password "whatever")))
              (list 'render-output (al-render-stub))
              (list 'abort-request (al-abort-stub)))
      (finishes (th-al-rest-login))
      (is (= 1 (th-al-usage-count)))
      (is (equal "login"
            (query "select usage_event from usage
                     where usage_event = 'login' limit 1"
              :result-type :single))))
    ;; bad password, non-empty username: login-failed + still aborts
    (th-al-with-stubs
        (list (list 'login-user-id
                (lambda (name password)
                  (declare (ignore name password))
                  nil))
              (list 'parse-posted-json
                (lambda () '(:username "admin" :password "wrong")))
              (list 'render-output (al-render-stub))
              (list 'abort-request (al-abort-stub)))
      (signals al-aborted (th-al-rest-login))
      (is (= 2 (th-al-usage-count)))
      (is (equal "login-failed"
            (query "select usage_event from usage
                     order by created_at desc limit 1"
              :result-type :single))))
    ;; blank username: nothing written, still aborts
    (th-al-with-stubs
        (list (list 'login-user-id
                (lambda (name password)
                  (declare (ignore name password))
                  nil))
              (list 'parse-posted-json
                (lambda () '(:username "" :password "x")))
              (list 'render-output (al-render-stub))
              (list 'abort-request (al-abort-stub)))
      (signals al-aborted (th-al-rest-login))
      (is (= 2 (th-al-usage-count)) "blank username writes nothing"))))

(test al-signaled-backend-writes-nothing
  "A backend that signals (handler-case abort path) writes no
usage row. be-list stubbed to signal: rest-list aborts and writes
nothing (the stub signals a plain error, which abort-error's
funnel through the abort stub turns into al-aborted — the
signals form pins the abort path, and the is-form the no-write).
A returned failure is the other case (rest-actions above already
pins that one)."
  (th-al-with-rows
    ;; be-list stubbed to signal: rest-list aborts (the abort stub
    ;; re-raises it as al-aborted) — and writes nothing. The count
    ;; must run AFTER the stub is gone: th-al-usage-count reads
    ;; through be-list itself.
    (th-al-with-stubs
        (list (list 'be-list
                (lambda (&rest args)
                  (declare (ignore args))
                  (error 'al-aborted :code 500 :message "stub signal"))))
      (th-al-with-stubs (al-handler-stubs)
        (signals al-aborted (th-al-rest-list :type "todos"))))
    (is (= 0 (th-al-usage-count)))))

(test al-usage-state-off-stops-writes
  "The runtime gate: under *top-level-settings* rebound with
:usage (:state :off), record-usage returns nil, writes nothing,
and never reads the dedup query (usage-recent-p stubbed to
signal — a gate that still ran the dedup read would fail here,
not just quietly skip the insert). After the binding restores,
writes resume (the reversibility guarantee)."
  (th-al-with-rows
    (let* ((copy (copy-list *top-level-settings*)))
      (setf (getf copy :usage) '(:state :off))
      (let ((*top-level-settings* copy))
        (th-al-with-stubs
            (list (list 'usage-recent-p
                    (lambda (&rest args)
                      (declare (ignore args))
                      (error "gate must not read: usage-recent-p called"))))
          (is (null (th-al-record-and-cleanup "admin" :list :on :todos)))
          (finishes (th-al-record-and-cleanup "admin" :list :on :todos)))))
    (is-true (th-al-record-and-cleanup "admin" :list :on :todos)
      "binding restored: writes resume")))

(in-suite action-log-compile)

(defun th-al-compile ()
  "Compile a minimal model and return the compiled defs plist."
  (compile-model
    `(:widgets
       (:table t
         :create :auto :update :auto :delete :auto :display t
         :type-roles ("al-users")
         :fields
         (:name (:type :text :identity t
                  :source (:view :main :column :name :agg :first)
                  :column t :not-null t))
         :list-form (:fields t)
         :view-form (:fields t)))))

(test al-compile-type-flags
  ":usage compiles built-in, not base, displayed, create nil, table
name usage, default sort (:created-at :desc)."
  (let* ((m (th-al-compile))
         (def (getf m :usage)))
    (is (eq t (getf def :built-in)))
    (is (null (getf def :base)))
    (is (eq t (getf def :display)))
    (is (null (getf def :create)))
    (is (equal "usage" (getf def :table-name)))
    (is (equal '(:created-at :desc) (getf def :default-sort)))))

(test al-compile-user-fk-set-null
  ":user is a nullable :target :users FK with :on-delete :set-null;
its create-sql says set null. Another FK in the same model keeps
on delete cascade and carries no :on-delete."
  (let* ((m (th-al-compile))
         (user-def (getf (getf (getf m :usage) :fields) :user)))
    (is (eq :users (getf user-def :target)))
    (is (null (getf user-def :not-null)))
    (is (eq :set-null (getf user-def :on-delete)))
    (is (search "on delete set null" (getf user-def :create-sql)
          :test #'char-equal))
    ;; some other FK still cascades and carries no :on-delete:
    ;; :settings :user (a plain :target :users FK, no :on-delete)
    (let ((settings-user (getf (getf (getf m :settings) :fields) :user)))
      (is (search "on delete cascade"
            (getf settings-user :create-sql) :test #'char-equal))
      (is-false (getf settings-user :on-delete)))))

(test al-compile-no-identity-index
  "No :usage field carries :identity; the compiled create-table-sql
:index is nil (no ix_usage_identity)."
  (let* ((m (th-al-compile))
         (fields (getf (getf m :usage) :fields)))
    (loop for def in (cdr fields) by #'cddr
      do (is (null (getf def :identity))))
    (is-false (getf (getf (getf m :usage) :create-table-sql) :index))))

(test al-compile-field-flags
  ":user-name not-null searchable sortable; :event not-null
searchable sortable; :on force-sql-name on_type searchable
sortable; :detail searchable sortable; :when searchable
sortable; :ip searchable sortable."
  (let* ((m (th-al-compile))
         (fields (getf (getf m :usage) :fields)))
    (is (eq t (getf (getf fields :user-name) :not-null)))
    (is (eq t (getf (getf fields :user-name) :searchable)))
    (is (eq t (getf (getf fields :user-name) :sortable)))
    (is (eq t (getf (getf fields :event) :not-null)))
    (is (eq t (getf (getf fields :event) :searchable)))
    (is (eq t (getf (getf fields :event) :sortable)))
    (is (equal "on_type" (getf (getf fields :on) :force-sql-name)))
    (is (equal "on_type" (getf (getf fields :on) :name-sql)))
    (is (eq t (getf (getf fields :on) :searchable)))
    (is (eq t (getf (getf fields :on) :sortable)))
    (is (eq t (getf (getf fields :detail) :searchable)))
    (is (eq t (getf (getf fields :detail) :sortable)))
    (is (eq t (getf (getf fields :ip) :searchable))
      ":ip is searchable")
    (is (null (getf (getf fields :ip) :not-null))
      ":ip is nullable")
    (is (equal "usage_ip" (getf (getf fields :ip) :name-sql))
      ":ip column is usage_ip")
    (is (eq t (getf (getf fields :ip) :sortable))
      ":ip is sortable")
    (is (eq t (getf (getf fields :when) :searchable))
      ":when is searchable")
    (is (eq t (getf (getf fields :when) :sortable))
      ":when is sortable")))

(test al-compile-forms
  ":list-form names the seven fields and not :created-at; no
:add-form, no :update-form."
  (let* ((m (th-al-compile))
         (def (getf m :usage)))
    (is (equal '(:when :user :user-name :ip :event :on :detail)
          (getf (getf def :list-form) :fields)))
    (is (null (getf def :add-form)))
    (is (null (getf def :update-form)))))

(test al-compile-wire
  "fe-fields for :usage emits :filter-with :reference on :user,
:searchable t on :when, and :sortable t on :ip (the sortable flag
reaches fe-fields; binds *compiled-model*)."
  (let* ((m (th-al-compile))
         (*compiled-model* m)
         (fe (fe-fields :usage "admin"))
         (list-form (getf fe :list-form)))
    (is (eq :reference
          (getf (getf list-form :user) :filter-with)))
    (is (eq t (getf (getf list-form :when) :searchable)))
    (is (eq t (getf (getf list-form :ip) :sortable))
      ":ip sortable reaches fe-fields")))

(test al-dedup-sql-pins-on-type
  "The dedup query follows the compiled names: the SQL text
contains on_type and not usage_on (the :force-sql-name pin)."
  (let* ((m (th-al-compile))
         (*compiled-model* m))
    (is (search "on_type"
          (th-al-dedup-sql-text)))
    (is-false (search "usage_on"
                (th-al-dedup-sql-text)))))

;;; ---------------------------------------------------------------------------
;;; :usage state (top-level :usage key)
;;; ---------------------------------------------------------------------------

(test al-usage-state-validator
  "valid-usage accepts (:state :off), (:state :on), and () (an
absent :state is the :on default, so the empty plist is legal);
it signals on non-plists, unknown :state values (nil included —
presence is tested via u:plist-keys, not getf truthiness), and
unknown keys. top-level-settings probes run on a complete legal
skeleton (every required key present) so validation reaches
:usage instead of dying on :title; (:usage nil) is rejected by
the member gate (the :api-roles precedent); the accessor
defaults to :on for the absent key."
  ;; acceptance
  (dolist (value '((:state :off) (:state :on) ()))
    (is (equal value (valid-usage value))))
  ;; rejection
  (dolist (value '(:off "off" 42 (:state :disabled) (:state nil)
                   (:state :off :retention 30)))
    (signals error (valid-usage value)))
  ;; end-to-end through the real entry point
  (signals error (top-level-settings (th-al-usage-model
                                       :usage '(:state :bogus)
                                       :usage-p t)))
  (signals error (top-level-settings (th-al-usage-model
                                       :usage nil :usage-p t)))
  (let ((settings (top-level-settings (th-al-usage-model
                                        :usage '(:state :off)
                                        :usage-p t))))
    (is (equal '(:state :off) (getf settings :usage))))
  ;; absent key stores nil; the accessor reads :on
  (let ((settings (top-level-settings (th-al-usage-model))))
    (is (null (getf settings :usage)))
    (let ((*top-level-settings* settings))
      (is (eq :on (model-usage-state))))))

(defun th-al-usage-model (&key usage usage-p)
  "Minimal legal model skeleton for :usage probes. Every required
key present (:title :name :version :domain plus one type) so
validation reaches :usage instead of dying on :title. USAGE-P (not
USAGE truthiness) splices the key: (:usage nil) — present-nil — is
exactly the member-gate probe and must not collapse to absent."
  `(:title "Usage Probe" :name "usage-probe" :version "1"
    :domain "probe.example.com"
    :types (:probe (:name "probe"
                   :fields ((:name "label" :type "text"))))
    ,@(when usage-p `(:usage ,usage))))

(defun th-al-dedup-sql-text ()
  "The SQL usage-recent-p builds (recomputed here for text
inspection; the same format string the function runs)."
  (let* ((m *compiled-model*)
         (table (u:tree-get m :usage :table-name))
         (name-col (u:tree-get m :usage :fields :user-name :name-sql))
         (event-col (u:tree-get m :usage :fields :event :name-sql))
         (on-col (u:tree-get m :usage :fields :on :name-sql))
         (detail-col (u:tree-get m :usage :fields :detail :name-sql)))
    (format nil
      "select 1 from ~a where ~a = $1 and ~a = $2 ~
       and ~a is not distinct from $3 ~
       and ~a is not distinct from $4 ~
       and created_at > now() - interval '~a seconds' ~
       limit 1"
      table name-col event-col on-col detail-col
      *usage-window-seconds*)))

;;; ---------------------------------------------------------------------------
;;; Guest login (guest-allowed-test context)
;;; ---------------------------------------------------------------------------

(in-suite action-log-guest-suite)

(test al-guest-login-writes
  "Guest login writes a login row with :user-name guest (guest
takes the success path: login-user-id short-circuits a:login for
the guest name on this fixture)."
  (th-al-with-rows
    (th-al-with-stubs
        (list (list 'parse-posted-json
                (lambda () '(:username "guest" :password "anything")))
              (list 'render-output (al-render-stub))
              (list 'abort-request (al-abort-stub)))
      (finishes (th-al-rest-login))
      (is (= 1 (length (th-al-usage-ids :user-name "guest"))))
      (is (equal "login"
            (query "select usage_event from usage
                     where usage_user_name = 'guest' limit 1"
              :result-type :single))))))
