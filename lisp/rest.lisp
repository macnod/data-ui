(in-package :data-ui)

;; Environment variables
(defparameter *http-port* (u:getenv "HTTP_PORT" :default 8080 :type :integer))
(defparameter *document-root*
  (let* ((dir (u:getenv "DOCUMENT_ROOT"
               :default "/app/shared-files/"))
          (normalized (probe-file dir)))
    (format nil "~a" normalized)))
(defparameter *temp-directory* (u:getenv "FS_TEMP_DIRECTORY"
                                 :default "/app/temp-files/"))
(defparameter *web-directory* (u:getenv "WEB_DIRECTORY" :default "/app/web"))
(defparameter *jwt-secret*
  (b:string-to-octets (u:getenv "JWT_SECRET" :default "32-char secret")))

;; Constants

;; Global variables
(defparameter *guest* "guest")
(defparameter *favicon* (u:join-paths *web-directory* "favicon.ico"))
(defparameter *http-server* nil)

;;
;; BEGIN Custom Hunchentoot acceptor
;; For plog logging
;;
(defclass fs-acceptor (h:easy-acceptor)
  ())

(defmethod h:acceptor-log-access ((acceptor fs-acceptor) &key return-code)
  "Override to route access logs through pl:plog."
  (let* ((code (h:return-code*))
          (uri (h:request-uri*))
          (health-log (equal uri "/health"))
          (log-severity (cond
                          (health-log :debug)
                          ((< code 300) :info)
                          ((< code 500) :warn)
                          (t :error))))
    (unless (and health-log *log-suppress-health*)
      (pl:plog log-severity
        (list
          :type "access"
          :client (h:real-remote-addr)
          :hop (h:remote-addr*)
          :server (h:local-addr*)
          :host (h:host)
          :method (h:request-method*)
          :uri uri
          :return-code code
          :status return-code
          :content-length (or (h:content-length*) 0)
          :content-type (or (h:content-type*) "unknown")
          :referer (h:referer)
          :agent (h:user-agent))))))

(defmethod h:acceptor-log-message ((acceptor fs-acceptor)
                                    log-level
                                    format-string &rest format-arguments)
  (let* ((log-severity (case log-level
                         (:error :error)
                         (:warning :warn)
                         (:info :info)
                         (t :debug)))
          (message (apply #'format
                     (append nil format-string) format-arguments)))
    (pl:plog log-severity (list :text message))))
;;
;; END Custom Hunchentoot acceptor
;;

;; Where Hunchentoot should store temporary files during uploads
(setf h:*tmp-directory* *temp-directory*)

;;
;; BEGIN JWT Token
;;

(defun issue-jwt (user-id &optional (expiration-seconds 3600))
  ":private: Issue a JWT for a user."
  (let* ((claims `(("sub" . ,user-id)
                    ("exp" . ,(+ (dt:universal-time-to-unix-time)
                                expiration-seconds)))))
    (j:encode :hs256 *jwt-secret* claims)))

(defun validate-jwt (token)
  ":private: Validate a JWT token. Returns username if JWT token validates and user
exists. Otherwise, logs a message and returns NIL."
  (handler-case
    (multiple-value-bind (claims headers sig)
      (jose:decode :hs256 *jwt-secret* token)
      (declare (ignore headers sig))
      (when claims
        (let ((user-id (cdr (assoc "sub" claims :test #'string=))))
          (if user-id
            (let ((user (handler-case
                          (a:get-value *rbac* "users" "username"
                            "id" user-id)
                          (error (e)
                            (pl:pwarn :status "invalid user id in jwt"
                              :condition e)
                            nil))))
              (if user
                user
                (progn
                  (pl:pwarn :status "user id not in database" :user-id user-id)
                  nil)))
            (progn
              (pl:pwarn :status "user id not in jwt")
              nil)))))
    (error (e)
      (pl:pwarn :status "invalid jwt" :condition e)
      nil)))

;;
;; END JWT Token
;;

(defun get-bearer-token ()
  "Extracts the JWT from the Authorization: Bearer header."
  (let ((auth (h:header-in* "Authorization")))
    (when (and auth (u:starts-with "Bearer " auth))
      (u:trim (subseq auth 7)))))

(defun current-user (&optional required-roles)
  "Returns (VALUES USER ALLOWED REQUIRED-ROLES).
  - USER is the authenticated user or *GUEST* if no valid token.
  - ALLOWED is T if the user has at least one of the REQUIRED-ROLES.
  - REQUIRED-ROLES is the same as what was passed in, for convenience."
  (let* ((token (get-bearer-token))
          (user (if token (validate-jwt token) *guest*))
          (user-roles (when user (a:get-id *rbac* "users" user)))
          (allowed (if required-roles
                     (u:has-some required-roles user-roles)
                     t)))
    (values user allowed required-roles)))

(defun make-json-error (status-code error-string)
  (let ((errors (cond ((listp error-string)
                        (format nil "~{~a~^ ~}." error-string))
                  ((stringp error-string)
                    error-string)
                  (t (format nil "Bad error string: ~s"
                       error-string)))))
    (setf (h:content-type*) "application/json")
    (setf (h:return-code*) status-code)
    (plist-to-json (list :status-code status-code :error errors))))

(defun render-output (result)
  (setf (h:content-type*) "application/json")
  (plist-to-json result))

(defun require-auth (&optional required-roles)
  "Returns the current user if authorized.
   Otherwise aborts the request with 401 or 403."
  ;; The following two lines are temporary, until I implement the login
  ;; endpoint. As soon as I've done that, I need to remove these two lines
  ;; and uncomment the lines that follow.
  (declare (ignore required-roles))
  "admin")
  ;; (multiple-value-bind (user allowed required)
  ;;     (current-user required-roles)
  ;;   (declare (ignore required))
  ;;   (cond
  ;;     ((not (stringp (get-bearer-token)))
  ;;      (h:abort-request-handler
  ;;        (make-json-error
  ;;          h:+http-authorization-required+ "missing or invalid token")))
  ;;     ((not allowed)
  ;;       (h:abort-request-handler
  ;;         (make-json-error h:+http-forbidden+ "forbidden")))
  ;;     (t user))))

(defun session-user (required-roles)
  (let* ((token (h:session-value :jwt-token))
          (user (if token (validate-jwt token) *guest*))
          (user-roles (when user (a:list-user-role-names *rbac* user)))
          (allowed (u:has-some required-roles user-roles)))
    (pl:pdebug :in "session-user"
      :token token
      :required-roles required-roles
      :roles user-roles
      :allowed allowed)
    (values user allowed required-roles)))

(defun abort-request (error-code format-string params)
  (h:abort-request-handler
    (make-json-error
      error-code
      (apply #'format (append (list nil format-string) params)))))

(defun abort-not-found (format-string &rest params)
  (abort-request h:+http-not-found+ format-string params))

(defun abort-bad-request (format-string &rest params)
  (abort-request h:+http-bad-request+ format-string params))

(defun abort-error (format-string &rest params)
  (abort-request h:+http-internal-server-error+ format-string params))

(defun abort-forbidden (format-string &rest params)
  (abort-request h:+http-forbidden+ format-string params))

(defun abort-auth (format-string &rest params)
  (abort-request h:+http-authorization-required+ format-string params))

(defun parse-type (type-string)
  (if (and
        type-string
        (stringp type-string)
        (not (zerop (length type-string)))
        (re:scan "^[-a-zA-Z0-9]" type-string)
        (not (re:scan "^[-0-9]|[-0-9]$" type-string))
        (u:tree-get *compiled-model* (u:make-keyword type-string)))
    (u:make-keyword type-string)
    (abort-not-found "Type ~s not found." type-string)))

(defun parse-field (type-key field-string)
  (if (and
        type-key
        (keywordp type-key)
        field-string
        (stringp field-string)
        (not (zerop (length field-string)))
        (re:scan "^[-a-zA-Z0-9]" field-string)
        (not (re:scan "^[-0-9]|[-0-9]$" field-string))
        (u:tree-get *compiled-model* type-key :fields
          (u:make-keyword field-string)))
    (u:make-keyword field-string)
    (abort-not-found "Field ~s ~s not found" type-key field-string)))

(defun parse-operator (operator-string)
  (if (and
        (stringp operator-string)
        (not (zerop (length operator-string)))
        (< (length operator-string) 20)
        (re:scan "^[-a-zA-Z]+$" operator-string)
        (handler-case (operator-sql (u:make-keyword operator-string))
          (error nil)))
    (u:make-keyword operator-string)
    (abort-not-found "Operator ~s not found" operator-string)))

(defun parse-filters (filters-json)
  (loop with lists = (y:parse filters-json)
    for (type-string field-string op-string value) in lists
    for type-key = (parse-type type-string)
    for field-key = (parse-field type-key field-string)
    for op-key = (parse-operator op-string)
    for filter = (append
                   (remove-if-not
                     #'identity
                     (list type-key field-key op-key))
                   (list value))
    when (every #'identity filter)
    collect filter into good
    else collect filter into bad
    finally
    (if bad
      (abort-bad-request "Bad filters ~{~s~^ ~}" filter)
      (return good))))

(defun parse-form (form-string)
  (loop with form-keys = (list :list-form :update-form :add-form)
    for form-key in form-keys
    when (equal (format nil "~(~a~)" form-key) form-string)
    do (return form-key)))

(h:define-easy-handler (health :uri "/health") ()
  (format nil "OK~%"))

(h:define-easy-handler (rest-list :uri "/api/list" :default-request-type :get)
  (type
    (filters :init-form nil)
    (form :init-form "list-form"))
  (let* ((type-key (parse-type type))
          (type-roles (when type-key (get-type-roles type-key)))
          (user (require-auth type-roles))
          (filters-parsed (when filters (parse-filters filters)))
          (form-key (when form (parse-form form)))
          (result (handler-case
                    (be-list type-key user
                      :form form-key :filters filters-parsed)
                    (error (e) (abort-error e)))))
    (render-output result)))

(h:define-easy-handler (rest-item :uri "/api/item" :default-request-type :get)
  (id type (form :init-form "update-form"))
  (let* ((type-key (parse-type type))
          (type-roles (when type-key (get-type-roles type-key)))
          (user (require-auth type-roles))
          (form-key (when form (parse-form form)))
          (result (handler-case
                    (be-rec id user :type-key type-key :form form-key)
                    (error (e) (abort-error e)))))
    (render-output result)))

(defun start-web-server ()
  (setf *http-server* (make-instance 'fs-acceptor
                        :port *http-port*
                        :document-root *document-root*))
  (setf
    h:*show-lisp-errors-p* t
    (h:acceptor-persistent-connections-p *http-server*) nil)
  (pl:pinfo :in "start-web-server"
    :status "server started"
    :endpoint (format nil "http://localhost:~d" *http-port*))
  (h:start *http-server*))
