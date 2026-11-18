(in-package :data-ui)

(setf dr:*text-content-types* '((nil . "json")))

(defun serialize-filters (filters)
  (h:url-encode
    (loop for filter in filters
      collect (format nil "[~{\"~a\"~^,~}]" filter) into lines
      finally (return (format nil "[~{~a~^,~}]" lines)))))

(defun http-get (url)
  (let ((dr:*text-content-types* '((nil . "json"))))
    (multiple-value-bind (data code meta)
      (dr:http-request url
        :method :get
        :accept "application/json"
        :preserve-uri t
        :decode-content t)
      (list
        :data (json-to-plist data)
        :code code
        :meta (loop for item in meta appending (list (car item) (cdr item)))))))


(defun http-post (url body)
  "POST the plist BODY as JSON to URL; the result shape matches
http-get (:data :code :meta). The body is encoded with plist-to-json
so the wire carries real JSON tokens: :null spells null (a Lisp NIL
would spell []), :true / :false spell booleans, numbers pass
through, and a key omitted from BODY is omitted from the wire — a
missing key, JSON null, and [] all arrive as NIL server-side, so a
401 assertion cannot tell them apart."
  (let ((dr:*text-content-types* '((nil . "json"))))
    (multiple-value-bind (data code meta)
      (dr:http-request url
        :method :post
        :accept "application/json"
        :preserve-uri t
        :decode-content t
        :content (plist-to-json body)
        :content-type "application/json")
      (list
        :data (json-to-plist data)
        :code code
        :meta (loop for item in meta appending (list (car item) (cdr item)))))))


(defun make-url (endpoint &rest params)
  (if params
    (loop for key in params by #'cddr
      for val-raw in (cdr params) by #'cddr
      for val = (case key
                  (:filters (serialize-filters val-raw))
                  (otherwise val-raw))
      appending (list key val) into processed-params
      finally
      (return
        (format nil "http://~a:~a~a?~{~(~a~)=~a~^&~}"
          *http-host* *http-port* endpoint processed-params)))
    (format nil "http://~a:~a~a"
      *http-host* *http-port* endpoint)))

(def-suite rest-suite :description "ReST endpoint tests")

(in-suite rest-suite)

;; TODO: ReST tests

;; (test rest-list
;;   (let ((response (http-get (make-url "/api/list" :type :permissions))))
;;     (is (equal (getf response :code) 200))
;;     (is (equal (u:tree-get response :data :status) "success"))
;;     (is (equal (u:tree-get response :data :result :type-key) "permissions"))
;;     (is (equal
;;           (u:safe-sort (u:tree-col response :data :result :records :name))
;;           (u:safe-sort '("create" "read" "update" "delete"))))
;;     (is (equal
;;           (u:safe-sort (u:plist-keys (u:tree-get response :data :result :records 0)))
;;           (u:safe-sort '(:id :created-at :updated-at :name))))
;;     (is-false (u:tree-get response :data :result :allowed-values))
;;     (is (> (parse-integer (u:tree-get response :meta :content-length))
;;           500))))

;; (test rest-id
;;   (let ((response (http-get (make-url "/api/id"
;;                               :type :users
;;                               :filters '((:users :name :eq "admin"))))))
;;     (is (equal (getf response :code) 200))
;;     (is (equal (u:tree-get response :data :status) "success"))
;;     (is-true (uuid-p (u:tree-get response :data :result)))))


;;; ---------------------------------------------------------------------------
;;; POST /api/login malformed-credential matrix (the missing-username
;;; login 500). The guard sits in login-user-id: nil / non-string
;;; credentials return nil and rest-login takes its ordinary abort-auth
;;; path (401, "Invalid username or password") instead of signaling
;;; no-applicable-method on RBAC's string-specialized login.
;;; ---------------------------------------------------------------------------

(defun th-rt-assert-401 (response)
  "RESPONSE (the http-post result shape) is the clean login-failure
shape: HTTP 401, status fail, the standard error text, and no
tokens."
  (and (equal 401 (getf response :code))
    (equal "fail" (u:tree-get response :data :status))
    (equal "Invalid username or password"
      (u:tree-get response :data :error))
    (not (u:tree-get response :data :result :access-token))
    (not (u:tree-get response :data :result :refresh-token))))

(defun th-rt-usage-table ()
  "The physical :usage table name from the compiled model (the
usage-recent-p shape, so the SQL cannot desync from the type)."
  (u:tree-get *compiled-model* :usage :table-name))

(defun th-rt-usage-col (field-key)
  "The physical :usage column for FIELD-KEY (compiled :name-sql)."
  (u:tree-get *compiled-model* :usage :fields field-key :name-sql))

(defun th-rt-login-failed-total ()
  "Direct-SQL count of all login-failed usage rows."
  (query (format nil "select count(*) from ~a where ~a = 'login-failed'"
           (th-rt-usage-table) (th-rt-usage-col :event))
    :result-type :single))

(defun th-rt-login-failed-count (name)
  "Direct-SQL count of login-failed usage rows for NAME. Direct SQL,
not be-list: listing :usage is what record-usage's self-log guard
exists to avoid, and be-list would page, hydrate, and check
permissions."
  (query (format nil "select count(*) from ~a where ~a = $1 and ~a = 'login-failed'"
           (th-rt-usage-table) (th-rt-usage-col :user-name)
           (th-rt-usage-col :event))
    :params (list name)
    :result-type :single))

(defun th-rt-usage-ids (names)
  "Usage row ids for NAMES (direct SQL on the physical columns)."
  (loop for name in names
    append (query (format nil "select id from ~a where ~a = $1"
                    (th-rt-usage-table) (th-rt-usage-col :user-name))
              :params (list name)
              :result-type :column)))

(defun th-rt-remove-usage-rows (ids)
  "Remove each :usage row in IDS via its resource — a:remove-resource
on id-to-resource-name, the th-al-with-rows pattern
(tests/action-log-tests.lisp). be-delete cannot remove a :usage row
(the type is :delete nil), and a raw DELETE FROM usage would orphan
the resources row: usage.id is the resource uuid usage-insert takes
from a:add-resource."
  (dolist (id ids)
    (let ((rn (id-to-resource-name id)))
      (when rn (a:remove-resource *rbac* rn)))))

(test rest-login-malformed-username-401
  "Malformed usernames — a missing key (the report's alternate name
spelling), JSON null, an integer, booleans, and the empty string —
take the clean 401 mismatch path, never the no-applicable-method
500. The password is a valid string, so only the username varies."
  (let ((url (make-url "/api/login")))
    (dolist (body '((:name "rosie" :password "x")
                    (:username :null :password "x")
                    (:username 42 :password "x")
                    (:username :true :password "x")
                    (:username :false :password "x")
                    (:username "" :password "x")))
      (is-true (th-rt-assert-401 (http-post url body))
        "expected the clean 401 for body ~s" body))))

(test rest-login-malformed-password-401
  "Malformed passwords — a missing key, JSON null, an integer,
booleans, and the empty string — take the same clean 401 when the
username is a unique non-empty string per case, so a bad username
cannot mask password coverage. Includes the guest name with a
non-string password: test-model leaves :guest-allowed unset, so the
guest short-circuit is off and the ordinary branch answers (the
fix, not a regression). Each case writes one login-failed usage
row; cleanup removes exactly the rows this test wrote."
  (let* ((url (make-url "/api/login"))
         (names '("rt-pw-missing" "rt-pw-null" "rt-pw-int"
                   "rt-pw-true" "rt-pw-false" "rt-pw-empty" "guest"))
         (bodies '((:username "rt-pw-missing")
                   (:username "rt-pw-null" :password :null)
                   (:username "rt-pw-int" :password 42)
                   (:username "rt-pw-true" :password :true)
                   (:username "rt-pw-false" :password :false)
                   (:username "rt-pw-empty" :password "")
                   (:username "guest" :password 42)))
         (baseline-ids (th-rt-usage-ids names)))
    (unwind-protect
        (progn
          (dolist (body bodies)
            (is-true (th-rt-assert-401 (http-post url body))
              "expected the clean 401 for body ~s" body)))
      (th-rt-remove-usage-rows
        (set-difference (th-rt-usage-ids names) baseline-ids
          :test #'equal)))))

(test rest-login-usage-logging
  "The 401s write usage rows by the existing username-shape rules:
no login-failed row for a missing, null, non-string, or empty
username; exactly one per unique non-empty username when the
password is the malformed side. The names are unknown on purpose —
record-usage omits the :user FK when the name does not resolve, and
the row still lands. Cleanup removes exactly the rows this test
wrote."
  (let* ((url (make-url "/api/login"))
         (names '("rt-log-missing" "rt-log-null" "rt-log-int"
                   "rt-log-empty"))
         (bodies '((:username "rt-log-missing")
                   (:username "rt-log-null" :password :null)
                   (:username "rt-log-int" :password 42)
                   (:username "rt-log-empty" :password "")))
         (baseline (th-rt-login-failed-total))
         (baseline-ids (th-rt-usage-ids names)))
    (unwind-protect
        (progn
          ;; Malformed username (password valid): no failure rows.
          (dolist (body '((:name "rosie" :password "x")
                          (:username :null :password "x")
                          (:username 42 :password "x")
                          (:username "" :password "x")))
            (http-post url body))
          (is (equal baseline (th-rt-login-failed-total))
            "malformed usernames must add no login-failed rows")
          ;; Malformed password (username a real non-empty string,
          ;; unique per case): exactly one row per name.
          (dolist (body bodies)
            (http-post url body))
          (dolist (name names)
            (is (equal 1 (th-rt-login-failed-count name))
              "expected exactly one login-failed row for ~a" name)))
      (th-rt-remove-usage-rows
        (set-difference (th-rt-usage-ids names) baseline-ids
          :test #'equal)))))

(test rest-refresh-tokenless-401
  "Regression control: tokenless POST /api/refresh stays the clean
401 it already was; refresh production code is untouched by the
login guard."
  (let ((r (http-post (make-url "/api/refresh") '())))
    (is (equal 401 (getf r :code)))
    (is (equal "fail" (u:tree-get r :data :status)))
    (is (equal "Invalid or expired token"
      (u:tree-get r :data :error)))))
