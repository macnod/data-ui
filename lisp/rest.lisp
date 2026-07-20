(in-package :data-ui)

;; Environment variables
(defparameter *http-host* (u:getenv "HTTP_HOST" :default "127.0.0.1"))
(defparameter *http-port* (u:getenv "HTTP_PORT" :default 8080 :type :integer))
(defparameter *web-directory* (u:getenv "WEB_DIRECTORY" :default "/app/web"))
(defparameter *jwt-secret*
  (b:string-to-octets (u:getenv "JWT_SECRET" :default "32-char secret")))

;; Constants

;; Global variables
(defparameter *guest* "guest")
(defparameter *favicon* (u:join-paths *web-directory* "favicon.ico"))
(defvar *http-server* nil)

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
          :in "acceptor-log-access"
          :type "access"
          :client (h:real-remote-addr)
          :hop (h:remote-addr*)
          :server (h:local-addr*)
          :host (h:host)
          :method (h:request-method*)
          :uri uri
          :status (or return-code code)
          :content-length (or (h:content-length*) 0)
          :content-type (or (h:content-type*) "")
          :referer (or (h:referer) "")
          :agent (or (h:user-agent) ""))))))

(defmethod h:acceptor-log-message ((acceptor fs-acceptor)
                                    log-level
                                    format-string &rest format-arguments)
  (let* ((log-severity (case log-level
                         (:error :error)
                         (:warning :warn)
                         (:info :info)
                         (t :debug)))
          (message (apply #'format (append (list nil format-string) format-arguments))))
    (pl:plog log-severity (list :in "acceptor-log-message" :text message))))
;;
;; END Custom Hunchentoot acceptor
;;

;; Where Hunchentoot should store temporary files during uploads
(setf h:*tmp-directory* *temp-directory*)

;;
;; BEGIN JWT Token
;;

(defun issue-jwt (type user-id expiration-seconds)
  ":private: Issue a JWT token of the specifiied type for a user."
  (let* ((claims `(("sub" . ,user-id)
                    ("exp" . ,(+ (dt:universal-time-to-unix-time)
                                expiration-seconds))
                    ("type" . ,type))))
    (j:encode :hs256 *jwt-secret* claims)))

(defun issue-access-token (user-id &optional (expiration-seconds 3600))
  ":private: Issue a JWT access token for a user."
  (issue-jwt "access" user-id expiration-seconds))

(defun issue-refresh-token (user-id &optional (expiration-seconds (* 24 3600 7)))
  ":private: Issue a JWT refresh token for a user."
  (issue-jwt "refresh" user-id expiration-seconds))

(defun validate-jwt (token type)
  ":private: Validate a JWT token of the given type. Returns username if JWT
token validates and user exists. Otherwise, logs a message and returns NIL."
  (handler-case
    (multiple-value-bind (claims headers sig)
      (jose:decode :hs256 *jwt-secret* token)
      (declare (ignore headers sig))
      (when claims
        (let* ((user-id (cdr (assoc "sub" claims :test #'string=)))
                (token-type (cdr (assoc "type" claims :test #'string=)))
                (user (when user-id
                        (handler-case
                          (a:get-value *rbac* "users" "user_name" "id" user-id)
                          (error (e)
                            (pl:pwarn :in "validate-jwt"
                              :status "error looking up user id in database"
                              :user-id user-id
                              :condition (princ-to-string e)))))))
          (cond
            ((not (equal token-type type))
              (pl:pwarn :in "validate-jwt"
                :status "invalid token type"
                :token token
                :expected-type type
                :actual-type token-type
                :claims claims)
              nil)
            ((not user-id)
              (pl:pwarn :in "validate-jwt"
                :status "missing user id in token"
                :token token
                :claims claims)
              nil)
            ((not user)
              (pl:pwarn :in "validate-jwt"
                :status "user id not found in database"
                :user-id user-id)
              nil)
            (t user)))))
    (jose/errors:jwt-claims-expired (e)
      (abort-auth e :reason "token expired"
        :token token :type type :condition (princ-to-string e)))
    (error (e)
      (abort-auth e :reason "invalid token"
        :token token :type type :condition (princ-to-string e)))))

(defun validate-access-token (token)
  ":private: Validate a JWT access token. Returns username if JWT token
validates and user exists. Otherwise, logs a message and returns NIL."
  (validate-jwt token "access"))

(defun validate-refresh-token (token)
  ":private: Validate a JWT refresh token. Returns username if JWT token
validates and user exists. Otherwise, logs a message and returns NIL."
  (validate-jwt token "refresh"))

;;
;; END JWT Token
;;

(defun get-bearer-token ()
  "Extracts the JWT from the Authorization: Bearer header."
  (let ((auth (h:header-in* "Authorization")))
    (pl:pdebug :in "get-bearer-token" :auth auth)
    (when (and auth (u:starts-with auth "Bearer "))
      (let ((token (u:trim (subseq auth 7))))
        (pl:pdebug :in "get-bearer-token" :token token)
        token))))

(defun get-access-token ()
  "Extract JWT from Bearer header or ?token= query parameter.
  Returns the token string or NIL."
  (or (get-bearer-token)
      (h:get-parameter "token")))

(defun current-user (&optional required-roles)
  "Returns (VALUES USER ALLOWED REQUIRED-ROLES).
  - USER is the authenticated user or *GUEST* if no valid token.
  - ALLOWED is T if the user has at least one of the REQUIRED-ROLES.
  - REQUIRED-ROLES is the same as what was passed in, for convenience."
  (let* ((token (get-bearer-token))
          (user (if token (validate-access-token token) *guest*))
          (user-roles (when user (a:list-user-role-names *rbac* user)))
          (allowed (if required-roles
                     (u:has-some required-roles user-roles)
                     t)))
    (pl:pdebug :in "current-user" :token token :user user :user-roles user-roles
      :required-roles required-roles :allowed allowed)
    (unless user
      (abort-auth "Invalid or expired token."))
    (values user allowed required-roles)))

(defun store-token (username token)
  ":private: Store the JWT token in the database."
  (handler-case
    (let ((id (be-value-id :tokens :user username "admin"))
           (data `(:user ,username :value ,token)))
      (if id
        (be-update :tokens id data "admin")
        (be-insert-internal :tokens data "admin")))
    (error (e)
      (abort-error e :reason "Error storing token in database."
        :token token :username username :value token))))

(defun make-json-error (status-code error-string &optional details-plist)
  (let ((errors (cond ((listp error-string)
                        (format nil "~{~a~^ ~}." error-string))
                  ((stringp error-string)
                    error-string)
                  (t (format nil "Bad error string: ~s"
                       error-string)))))
    (setf (h:content-type*) "application/json")
    (setf (h:return-code*) status-code)
    (plist-to-json
      (add-to-plist
        (list
          :status "fail"
          :status-code status-code
          :error errors)
        (when details-plist
          (list :details details-plist))))))

(defun render-output (result)
  (setf (h:content-type*) "application/json")
  (plist-to-json (list
                   :status "success"
                   :result result)))

(defun require-auth (&optional required-roles)
  "Returns the current user if authorized.
   Otherwise aborts the request with 401 or 403."
  ;; The following two lines are temporary, until I implement the login
  ;; endpoint. As soon as I've done that, I need to remove these two lines
  ;; and uncomment the lines that follow.
  (handler-case
    (multiple-value-bind (user allowed required)
      (current-user required-roles)
      (pl:pdebug :in "require-auth" :user user :allowed allowed :required required)
      (cond
        ((not (stringp (get-bearer-token)))
          (abort-auth "Missing or invalid token"))
        ((not allowed)
          (abort-auth "User does not have required roles"
            :user user :required-roles required-roles))
        ((not user)
          (abort-auth "User not found"
            :user user :required-roles required-roles))
        (t user)))
    (error (e)
      (abort-auth e :reason "error during authentication"
        :required-roles required-roles))))

(defun require-auth-with-query-token (&optional required-roles)
  "Like require-auth, but also accepts JWT via ?token= query param.
  For endpoints that need browser-native loading (e.g. <img src>)."
  (handler-case
    (multiple-value-bind (user allowed required)
      (let* ((token (get-access-token))
             (user (if token
                     (validate-access-token token)
                     *guest*))
             (user-roles (when user
                          (a:list-user-role-names *rbac* user)))
             (allowed (if required-roles
                        (u:has-some required-roles user-roles)
                        t)))
        (unless user
          (abort-auth "Invalid or expired token."))
        (values user allowed required-roles))
      (cond
        ((not (get-access-token))
          (abort-auth "Missing or invalid token"))
        ((not allowed)
          (abort-auth "User does not have required roles"
            :user user :required-roles required-roles))
        ((not user)
          (abort-auth "User not found"
            :user user :required-roles required-roles))
        (t user)))
    (error (e)
      (abort-auth e :reason "error during authentication"
        :required-roles required-roles))))

(defun abort-request (error-code message &optional details-plist)
  (let ((details (loop for key in details-plist by #'cddr
                   for val in (cdr details-plist) by #'cddr
                   when val append (list key val))))
    (pl:plog :error
      (add-to-plist
        (list
          :in "abort-request"
          :error-code error-code
          :reason message)
        (when details-plist (list :details details))))
    (h:abort-request-handler (make-json-error error-code message details))))

(defun abort-not-found (error-object &rest details-plist)
  (abort-request
    h:+http-not-found+
    (princ-to-string error-object)
    details-plist))

(defun abort-bad-request (error-object &rest details-plist)
  (abort-request
    h:+http-bad-request+
    (princ-to-string error-object)
    details-plist))

(defun abort-error (error-object &rest details-plist)
  (abort-request
    h:+http-internal-server-error+
    (princ-to-string error-object)
    details-plist))

(defun abort-forbidden (error-object &rest details-plist)
  (abort-request
    h:+http-forbidden+
    (princ-to-string error-object)
    details-plist))

(defun abort-auth (error-object &rest details-plist)
  (abort-request
    h:+http-authorization-required+
    (princ-to-string error-object)
    details-plist))

(defun parse-posted-json ()
  (let ((json (h:raw-post-data :force-text t)))
    (handler-case (json-to-plist json)
      (error (e)
        (abort-bad-request e
          :reason "Invalid JSON in request body."
          :json json)))))

(defun parse-id (id)
  (cond
    ((uuid-p id) id)
    ((or (null id) (zerop (length id)))
      (abort-bad-request "Parameter 'id' is required."))
    (t (abort-bad-request
         "The value for 'id' is not a valid UUID."
         :id id))))

(defun parse-type (type-string)
  (if (and
        type-string
        (stringp type-string)
        (not (zerop (length type-string)))
        (re:scan "^[-a-zA-Z0-9]" type-string)
        (not (re:scan "^[-0-9]|[-0-9]$" type-string))
        (u:tree-get *compiled-model* (u:make-keyword type-string)))
    (u:make-keyword type-string)
    (if type-string
      (abort-not-found "Type not found." :type type-string)
      (abort-bad-request "Parameter 'type' is required."))))

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
    (abort-not-found "Field not found"
      :type type-key
      :field field-string)))

(defun parse-operator (operator-string)
  (if (and
        (stringp operator-string)
        (not (zerop (length operator-string)))
        (< (length operator-string) 20)
        (re:scan "^[-a-zA-Z]+$" operator-string)
        (handler-case (operator-sql (u:make-keyword operator-string))
          (error nil)))
    (u:make-keyword operator-string)
    (abort-bad-request "Operator not found" :operator operator-string)))

(defun parse-filters (filters-json &key allow-id)
  (handler-case
    (when filters-json
      (if (and allow-id (uuid-p filters-json))
        filters-json
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
            (abort-bad-request "Bad filters ~{~s~^ ~}" bad)
            (return good)))))
    (error (e)
      (abort-bad-request e
        :reason "Invalid JSON in filters parameter."
        :filters-json filters-json
        :allow-id allow-id))))

(defun parse-form (form-string)
  (loop for form-key in *forms*
    when (equal (format nil "~(~a~)" form-key) form-string)
    do (return form-key)
    finally (abort-bad-request "Form not found" :form form-string)))

(h:define-easy-handler (health :uri "/health") ()
  (format nil "OK~%"))

(h:define-easy-handler (rest-list :uri "/api/list" :default-request-type :get)
  (type
    (filters :init-form nil)
    (form :init-form "list-form"))
  ":public: Endpoint for listing items.

Method

GET

Parameters

type

Required. This will be converted to a keyword and used to look up the type in
the model. Examples 'user', 'permissions', 'todos'.

filters

Optional. A JSON string representing a list of filters. Each filter is a list of
[type field operator value]. Example:

    [[\"user\", \"name\", \"like\", \"j%\"]]

form

Optional. The name of the form to use for rendering the output. This will be
converted to a keyword and used to look up the form in the model.  Valid values
are 'list-form', 'update-form', and 'add-form'."
  (let* ((type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (filters-parsed (parse-filters filters))
          (form-key (when form (parse-form form)))
          (result (handler-case
                    (be-list type-key user
                      :form form-key :filters filters-parsed)
                    (validation-error (e)
                      (abort-bad-request e :type type :filters filters
                        :form form))
                    (error (e)
                      (abort-error e :type type :filters filters :form form)))))
    (render-output result)))

(h:define-easy-handler (rest-item :uri "/api/item" :default-request-type :get)
  (type id (form :init-form "update-form"))
  ":public: Endpoint for getting a single item.
Method

GET

Parameters

type

Required. This will be converted to a keyword and used to look up the type in
the model. Examples 'user', 'permissions', 'todos'.

id

Required. The UUID of the item to retrieve.

form

Optional. The name of the form to use for rendering the output. This will be
converted to a keyword and used to look up the form in the model.  Valid values
are 'list-form', 'update-form', and 'add-form'.

GET /api/item"
  (let* ((id (parse-id id))
          (type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (form-key (when form (parse-form form)))
          (result (handler-case
                    (be-rec id user :type-key type-key :form form-key)
                    (validation-error (e)
                      (abort-bad-request e :type type :id id :form form))
                    (error (e) (abort-error e :type type :id id :form form)))))
    (render-output result)))

(h:define-easy-handler (rest-id :uri "/api/id" :default-request-type :get)
  (type filters)
  ":public: Endpoint for getting a single item ID based on filters.

Method

GET

Parameters

type

Required. This will be converted to a keyword and used to look up the type in
the model. Examples 'user', 'permissions', 'todos'.

filters

Required. A JSON string representing a list of filters. Each filter is a list of
[type field operator value]. For this endpoint, the filters must uniquely
identify a single record. Example:

    [[\"user\", \"name\", \"eq\", \"admin\"]]

Alternatively, a single ID can be specified as the filter, in which case the
'filters' parameter value consists of a simple UUID string instead of a
JSON-encoded list of lists.

GET /api/id"
  (let* ((type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (filters-parsed (parse-filters filters))
          (result (handler-case
                    (be-id type-key filters-parsed user)
                    (validation-error (e)
                      (abort-bad-request e :type type :filters filters))
                    (error (e)
                      (abort-error e :type type :filters filters)))))
    (render-output result)))

(h:define-easy-handler (rest-value-id :uri "/api/value-id"
                         :default-request-type :get)
  (type field value)
  ":public: Endpoint for getting the ID of a record based on a field value. This
is useful for lookups where the client knows a unique field value but not the ID
of the associated record.
Method

GET

Parameters

type

Required. This will be converted to a keyword and used to look up the type in
the model. Examples 'user', 'permissions', 'todos'.

field

Required. The name of a unique field in the type. This will be converted to a
keyword and used to look up the field in the model.

value

Required. The value to look up in the specified field.

GET /api/id"
  (let* ((type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (field-key (parse-field type-key field))
          (result (handler-case
                    (be-value-id type-key field-key value user)
                    (validation-error (e)
                      (abort-bad-request e
                        :type type :field field :value value))
                    (error (e)
                      (abort-error e :type type :field field :value value)))))
    (render-output result)))

(h:define-easy-handler (rest-value :uri "/api/value" :default-request-type :get)
  (type id field (form :init-form "udpate-form"))
  ":public: Endpoint for getting the value of a specific field in the record
with the specified ID.

Method

GET

Parameters

type

Required. This will be converted to a keyword and used to look up the type in
the model. Examples 'user', 'permissions', 'todos'.

id

Required. The UUID of the record to retrieve.

field

Required. The name of the field to retrieve. This will be converted to a keyword
and used to look up the field in the model.

GET /api/value"
  (let* ((id (parse-id id))
          (type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (field-key (parse-field type-key field))
          (result (handler-case
                    (be-val id field-key user :form form :type-key type-key)
                    (validation-error (e)
                      (abort-bad-request e
                        :type type :id id :field field :form form))
                    (error (e)
                      (abort-error e
                        :type type :id id :field field :form form)))))
    (render-output result)))

(h:define-easy-handler (rest-column :uri "/api/column"
                         :default-request-type :get)
  (type field filters (form :init-form "list-form"))
  ":public: Endpoint for getting a list of values from a specific field, with
optional filters to narrow down the records. This is useful for populating
dropdowns or for autocomplete functionality.
Method

GET

Parameters

type

Required. This will be converted to a keyword and used to look up the type in
the model. Examples 'user', 'permissions', 'todos'.

field

Required. The name of the field to retrieve values from. This will be converted
to a keyword and used to look up the field in the model.

filters

Optional. A JSON string representing a list of filters. Each filter is a list of
type, field, operator, and value. Example:

    [[\"user\", \"name\", \"like\", \"j%\"]]

form

Optional. The name of the form to use for rendering the output. This will be
converted to a keyword and used to look up the form in the model.  Valid values
are 'list-form', 'update-form', and 'add-form'.

GET /api/column"
  (let* ((type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (field-key (parse-field type-key field))
          (filters-parsed (parse-filters filters))
          (form-key (parse-form form))
          (result (handler-case
                    (be-list-column type-key field-key user
                      :form form-key :filters filters-parsed)
                    (validation-error (e)
                      (abort-bad-request e
                        :type type :field field :filters filters :form form))
                    (error (e)
                      (abort-error e
                        :type type :field field :filters filters :form form)))))
    (render-output result)))

(h:define-easy-handler (rest-insert :uri "/api/insert"
                         :default-request-type :post)
  ()
  ":public: Endpoint for inserting a new record.

Method

POST

Body

The body of the POST request should be a JSON object with a structure that is
similar to the following example:

    {
      \"type\": \"todos\",
      \"data\": {
        \"name\": \"clean kitchen\",
        \"points\": 3,
        \"tags\": [\"home\", \"chores\"]
      },
      \"roles\": [\"role-1\", \"role-2\"]
    }

The 'type' field is required and will be used to look up the type in the model.

The 'data' field is an object containing the field names and values for the new
record. The field names must match the field names defined in the model for the
specified type.

The 'roles' field is optional and can be used to specify a list of roles that
the new record will be associated with. You can only specify roles that you
have yourself. If you don't specify any roles, the new record will be given
the same roles as the user who is creating it. No matter what roles you specify,
the system always assigns the 'admin' role to every record. Except that in the
case of the 'users' type, the system assigns the 'logged-in' and 'public' roles
instead of 'admin'. The system also assigns the user's exclusive role to the
record, but that exclusive role is never listed. The exclusive role of the user
is always '{username}:exclusive', for example 'alice:exclusive'. A user's
exclusive role is unique to the user and can never be assigned to another user.
It allows users to make resources available to themselves only.

This endpoint returns a JSON object with the following structure:

    {
      \"status\": \"success\",
      \"result\": {
        \"id\": \"uuid-of-new-record\",
        \"inserted\": true
      }

If the record already exists and therefore was not inserted, the 'inserted'
field in the result will be false. Otherwise, if the record was successfully
inserted, the 'inserted' field will be true. In either case, the 'id' field
will containe the UUID of the new or existing record.

If the record already exists, this endpoint will not update the existing
record in any way. It will simply return the ID of the existing record.

The POST must include the header 'Content-Type: application/json'.

POST /api/insert"
  (let* ((tree (parse-posted-json))
          (type (getf tree :type))
          (file-token (getf tree :file-token))
          (data (getf tree :data))
          (roles (getf tree :roles))
          (type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (path-field (path-field type-key))
          (logical-path (when path-field (getf data path-field))))
    (pl:pdebug :in "rest-insert" :endpoint "/api/insert"
      :path-field path-field :logical-path logical-path
      :type type-key :user user :roles roles :type-roles type-roles)
    (multiple-value-bind (id inserted)
      (handler-case
        (progn
          (valid-new-directory type-key logical-path file-token user roles)
          (be-insert type-key data user :roles roles :file-token file-token))
        (validation-error (e)
          (abort-bad-request
            e :type type :data data :roles roles :user user))
        (error (e)
          (abort-error e :type type :data data :roles roles :user user)))
      (render-output (list :id id :inserted (if inserted :true :false))))))

(h:define-easy-handler (rest-upload :uri "/api/upload")
  ()
  ":public: Accepts a file upload and returns a path to the temporary location
of the uploaded file. A POST to this endpoint should occur immediately prior
to a POST to /api/insert. Unlike regular POSTs to /api/insert, the POST that
follows the upload should include an additional top-level field called `file`.

The POST to this endpoing must be in the format multipart/form-data, and it must
include the same fields, in that format, as the later POST to /api/insert:

'type' (required)

    String denoting the user-defined type for this upload. The string must match
    one of the user-defined types in the model, and that type must include a
    file field (`:type :file`) and a path field (`:path t`).

{path} (required)

    The name of this field must be the name of the field tagged with `:path t`
    in the definition of the given type. The value must be the path where you
    want the file to be stored in Data UI. This is not the path to the file in
    the client's computer. The path must be a valid absolute Linux path that
    ends with a file name. The directory portion of the path (up to the file
    name) must point to a directory that already exists in Data UI.

'roles' (optional)

    If this field is not provided, the system will assign the same roles to the
    file as currently exist for the parent directory. Otherwise, these roles
    will be assigned to the file. However, you cannot assign roles to the file
    that the parent directory doesn't already have. And, you cannot assign
    roles that you yourself don't have.

{file} (required)

    The name of this field must correspond to the field tagged with `:type
    :file` in the definition of the given type. The value must be the uploaded
    file.

This POST returns JSOn that looks like this:

    {
      \"status\": \"success\",
      \"result\": {
        \"token\": \"/a/b/c.txt\"
    }

The `token` key and value must be submitted at the top level of the JSON POST
to /api/insert, which should follow this post immediately.

This endpoint will return failure if any of these conditions arise:

  - The file already exists.
  - The user doesn't have create permissions on the parent directory.
  - The parent directory doesn't exist.
  - The user doesn't have create permission on the given type.
  - The user tries to assign roles that the parent directory or the user doesn't
    have.

The POST must include the header 'Content-Type: multipart/form-data'.

POST /api/upload"
  (unless (search "multipart/form-data" (h:header-in* :content-type))
    (abort-bad-request "Content-Type header must be multipart/form-data"
      :content-type (h:header-in* :content-type)))
  (let* ((type (h:post-parameter "type" h:*request*))
          (type-key (parse-type type))
          (file-field (format nil "~(~a~)" (file-field type-key)))
          (file (h:post-parameter file-field h:*request*))
          (temp-path (format nil "~a" (first file)))
          (target-field (format nil "~(~a~)" (path-field type-key)))
          (logical-path (h:post-parameter target-field h:*request*))
          (fs-path (fs-path type-key logical-path))
          (roles (let ((r (h:post-parameter "roles" h:*request*)))
                   (if (listp r) (list r))))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (file-token (u:safe-encode fs-path)))
    (pl:pdebug :in "rest-upload" :type type :type-key type-key
      :target-field target-field :logical-path logical-path
      :temp-path temp-path :type-roles type-roles 
      :file-field file-field
      :file-token file-token
      :fs-path fs-path)
    (handler-case
      (progn
        ;; TODO: Check file size and available disk space?
        (valid-file-meta type-key logical-path user roles)
        (u:copy-file temp-path fs-path)
        (render-output (list :file-token file-token)))
      (validation-error (e)
        (abort-bad-request e
          :type type
          :type-key type-key
          :path-field target-field
          :logical-path logical-path
          :roles roles
          :user user
          :file-token file-token))
      (error (e)
        (abort-error e
          :type type
          :type-key type-key
          :path-field target-field
          :logical-path logical-path
          :roles roles
          :user user
          :file-token file-token)))))

(h:define-easy-handler (rest-update :uri "/api/update"
                          :default-request-type :post)
  ()
  ":public: Endpoint for updating an existing record.

Method

POST

Body

The body of the POST request must be a JSON object with a structure that is
similar to the following example:

    {
      \"type\": \"todos\",
      \"filters\": [[\"todo\", \"name\", \"eq\", \"clean kitchen\"]],
      \"data\": {\"points\": 5},
      \"roles\": [\"role-1\", \"role-2\"]
    }

The 'type' field is required and will be used to look up the type in the model.

The 'filters' field is required and should be a JSON string representing a list
of filters. Each filter is a list of [type field operator value]. The filters
specify which record should be updated. The list of filters must uniquely
identify a single record. Alternatively, instead of a list of filters, you can
specify a single ID as the filter, in which case the 'filters' parameter
consists of a single string instead of a list of lists.

The 'data' field is an object containing the field names and new values for the
record to be updated. The field names must match the field names defined in the
model for the specified type. Any fields that are not included in the 'data'
object will be left unchanged.

The 'roles' field requires that you specify the new, complet list of roles that
the record should have. The system will add and remove roles as necessary to
match the list you provide. You can only specify roles that you have youself.
If you don't specify any roles, the record will keep the same roles it had.

The POST must include the header 'Content-Type: application/json'.

POST /api/update"
  (let* ((tree (parse-posted-json))
          (type (getf tree :type))
          (type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (filters (getf tree :filters))
          (data (getf tree :data))
          (roles (getf tree :roles))
          (result (handler-case
                    (be-update type-key filters data user :roles roles)
                    (validation-error (e)
                      (abort-bad-request e
                        :type type :filters filters :data data :roles roles))
                    (error (e)
                      (abort-error e
                        :type type :filters filters :data data :roles roles)))))
    (render-output result)))

(h:define-easy-handler (rest-delete :uri "/api/delete"
                         :default-request-type :post)
  ()
  ":public: Endpoint for deleting a record.

Method

POST

Body

The body of the POST request must be a JSON object with a structure that is
similar to the following example:

    {
      \"type\": \"todos\",
      \"filters\": [[\"todo\", \"name\", \"eq\", \"clean kitchen\"]]
    }

The 'type' field is required and will be used to look up the type in the mode.

The 'filters' field is required and should be a JSON string representing a list
of filters. Each filter is a list of [type field operator value]. The filters
specify which record should be deleted. The list of filters must uniquely
identify a single record. Alternatively, instead of a list of filters, you can
specify a single ID as the filter, in which case the 'filters' value consists of
a single string instead of a list of lists.

The POST must include the header 'Content-Type: application/json'.

POST /api/delete"
  (let* ((tree (parse-posted-json))
          (type (getf tree :type))
          (type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (filters (parse-filters (getf tree :filters) :allow-id t))
          (result (handler-case
                    (be-delete type-key filters user)
                    (validation-error (e)
                      (abort-bad-request e :type type :filters filters))
                    (error (e)
                      (abort-error e :type type :filters filters)))))
    (render-output result)))

(h:define-easy-handler (rest-actions :uri "/api/actions"
                         :default-request-type :post)
  ()
  ":public: Endpoint for executing an action hook on a record.

Method

POST

Body

The body of the POST request must be a JSON object with a structure that is
similar to the following example:

    {
      \"type\": \"models\",
      \"id\": \"<record-uuid>\",
      \"field\": \"deploy\"
    }

The 'type' field is required and identifies the type in the model.

The 'id' field is required and identifies the target record by UUID.

The 'field' field is required and must name a :button field that has a compiled
action hook.

The response will be a JSON object with the following structure:

    {
      \"status\": \"success\",
      \"result\": {
        \"status\": \"complete\"
      }
    }

The 'result.status' value will be \"complete\" (sync success), \"accepted\" with
an 'async' flag (async hook started), or \"failed\" with a 'message' field (sync
error during hook execution).

Validation errors (unknown type, bad field, missing permissions, action already
running) return 400. Other errors return 500.

POST /api/actions"
  (let* ((tree (parse-posted-json))
          (type (getf tree :type))
          (id (getf tree :id))
          (field (getf tree :field))
          (type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (field-key (when field
                       (if (stringp field)
                           (u:make-keyword field)
                           (abort-bad-request
                            "Parameter 'field' must be a string."
                            :field field))))
          (result (handler-case
                    (be-action type-key id field-key user)
                    (validation-error (e)
                      (abort-bad-request
                       e :type type :id id :field field :user user))
                    (error (e)
                      (abort-error
                       e :type type :id id :field field :user user)))))
    (render-output result)))

(h:define-easy-handler (rest-validate-field :uri "/api/validate-field"
                         :default-request-type :post)
  ()
  ":public: Endpoint for validating a field value. The front end can use this to
check fields in a form as the user fills them out.

Method

POST

Body

The body of the POST request must be a JSON object with a structure that is
similar to the following example:

    {
      \"type\": \"todos\",
      \"field\": \"name\",
      \"value\": \"clean kitchen\"
    }

The 'type' field is required and will be used to look up the type in the model.

The 'field' field is required and will be used to look up the field in the
model.

The 'value' field is required and is the value to validate. The validation rules
are defined in the model for each field. For example, you can specify that a
field is required, or that it must be a number. The model supports a variety of
validation rules, which you can specify with keywords. The model also supports
custom validation function, which you can provide as parameterized
registry entries.

The response will be a JSON object with the following structure:

    {
      \"status\": \"success\",
      \"result\": {
        \"valid\": true,
        \"errors\": []
      }
    )

If the value is valid, the 'valid' field in the result will be true and the
'errors' field will be an empty list. If the value is invalid, the 'valid' field
will be false and the 'errors' field will contain a list of error messages
explaining why the value is invalid.

POST /api/validate-field"
  (let* ((tree (parse-posted-json))
          (type (getf tree :type))
          (field (getf tree :field))
          (value (getf tree :value))
          (type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (field-key (parse-field type-key field))
          (result (handler-case
                    (be-validate-field type-key field-key value user)
                    (validation-error (e)
                      (abort-bad-request
                        e :type type :field field :value value))
                    (error (e)
                      (abort-error e :type type :field field :value value)))))
    (render-output result)))

(h:define-easy-handler (rest-validate-form :uri "/api/validate-form"
                         :default-request-type :post)
  ()
  ":public: Endpoint for validating a form. The front end can use this to check
all the fields in a form at once, for example just prior to submitting the
form. While this is more efficient that validating each field individually, it
is in practice still helpful to provide the field-level validation endpoint to
give users more immediate feedback as they fill out the form.

Method

POST

Body

The body of the POST request must be a JSON object with a structure that is
similar to the following example:

    {
      \"type\": \"todos\",
      \"data\": {
        \"name\": \"clean kitchen\",
        \"points\": 3,
        \"tags\": [\"home\", \"chores\"]
      }
    }

The 'type' field is required and will be used to look up the type in the model.

The 'data' field is an object containing the field names and values for the form
to be validated. The field names must match the field names defined in the model
for the specified type. The validation rules are defined in the model for each
field. For example, you can specify that a field is required, or that it must be
a number. The model supports a variety of validation rules, which you can
specify with keywords. The model also supports custom validation
function, which you can provide as parameterized registry entries.

The response will be a JSON object with the following structure:

    {
      \"status\": \"success\",
      \"result\": {
        \"valid\": false,
        \"errors\": {
          \"field1\": [\"error message 1\", \"error message 2\"],
          \"field2\": [\"error message 3\"]
        }
      }
    }

If the form is valid, the 'valid' field in the result will be true and the
'errors' field will be an empty object. If the form is invalid, the 'valid'
field will be false and the 'errors' field will be an object where the keys
are the names of the fields that have errors and the values are lists of
error messages explaining what is wrong with the value in that field.

POST /api/validate-form"
  (let* ((tree (parse-posted-json))
          (type (getf tree :type))
          (data (getf tree :data))
          (type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (result (handler-case
                    (be-validate-form type-key data user)
                    (validation-error (e)
                      (abort-bad-request e :type type :data data))
                    (error (e)
                      (abort-error e :type type :data data)))))
    (render-output result)))

(h:define-easy-handler (rest-types :uri "/api/types" :default-request-type :get)
  ()
  ":public: Endpoint for getting a list of all the defined types. This is useful
for dynamically creating a menu in the frontend.

GET /api/types"
  (let* ((user (require-auth '("logged-in"))))
    (render-output
      (mapcar
        (lambda (e)
          (list :name (format nil "~(~a~)" (getf e :name))
                :category (getf e :category)))
        (be-types user)))))

(h:define-easy-handler (rest-users :uri "/api/users" :default-request-type :get)
  ((q :init-form ""))
  ":public: Endpoint for searching users by name. Returns up to 10
matching usernames. Used by the roles UI for point-to-point sharing.

GET /api/users?q=<query>"
  (let* ((user (require-auth '("logged-in")))
         (query (string-trim '(#\space #\tab) q)))
    (render-output
      (if (or (null query) (zerop (length query)))
        nil
        (let ((matches
                (remove-if
                  (lambda (name) (equal name user))
                  (remove-if-not
                    (lambda (name)
                      (search (string-downcase query)
                              (string-downcase name)))
                    (a:list-user-names *rbac*)))))
          (subseq matches 0 (min 10 (length matches))))))))

(h:define-easy-handler (rest-public-info
                         :uri "/api/public-info"
                         :default-request-type :get)
  ()
  ":public: Unauthenticated endpoint for non-sensitive app metadata.
Currently returns only the model title.

GET /api/public-info"
  (render-output (list :title (getf *top-level-settings* :title))))

(h:define-easy-handler (rest-info :uri "/api/info" :default-request-type :get)
  ()
  ":public: Endpoint for retrieving information about the deployed app,
including title, name, version, domain, and repl (which indicates if the app
provides a Common Lisp REPL.

GET /api/info"
  (let* ((user (require-auth '("logged-in"))))
    (when user
      (let ((settings (copy-list *top-level-settings*)))
        (setf (getf settings :landing-page)
              (be-landing-page user))
        (render-output settings)))))

(h:define-easy-handler (rest-css-variables
                         :uri "/api/css-variables"
                         :default-request-type :get)
  ()
  ":public: Returns a JSON map of field-name -> value for every :css-value t
field on the current user's settings record. Pure data extraction — the frontend
owns all CSS interpretation.

GET /api/css-variables"
  (let* ((user (require-auth '("logged-in")))
         (settings-id (be-value-id :settings :user user "admin")))
    (when settings-id
      (let ((record (be-rec settings-id user :type-key :settings)))
        (when record
          (let ((rec-data (getf record :record)))
            (loop with fields = (u:tree-get *compiled-model* :settings :fields)
              for field-key in fields by #'cddr
              for field-def in (cdr fields) by #'cddr
              when (getf field-def :css-value)
              append (list field-key (getf rec-data field-key)) into css-vars
              finally (return (render-output css-vars)))))))))

(h:define-easy-handler (rest-login :uri "/api/login" :default-request-type :post)
  ()
  ":public: Endpoint for logging in and getting a JWT token.

POST /api/login"
  (let* ((tree (parse-posted-json))
          (username (getf tree :username))
          (password (getf tree :password))
          (user-id (let ((id (handler-case
                               (a:login *rbac* username password)
                               (error (e) (abort-error e
                                            :reason "Error during login")))))
                     (unless id (abort-auth "Invalid username or password"))
                     id))
          (refresh-token (issue-refresh-token user-id))
          (access-token (issue-access-token user-id))
          (result `(:access-token ,access-token
                     :refresh-token ,refresh-token)))
    (store-token username refresh-token)
    (render-output result)))

(h:define-easy-handler (rest-refresh :uri "/api/refresh" :default-request-type :post)
  ()
  ":public: Endpoint for refreshing a JWT token. This is useful for getting a
new short-lived token using a long-lived token, without requiring the user to
log in again with their username and password.

POST /api/refresh"
  (let* ((token (get-bearer-token))
          (user (when token (validate-refresh-token token)))
          (user-id (when user (a:get-id *rbac* "users" user)))
          (token-exists (when user-id
                          (handler-case
                            (be-value-id :tokens :value token "admin")
                            (error (e)
                              (abort-error e
                                :reason "Error looking up token"))))))
    (pl:pdebug :in "rest-refresh" :user user :user-id user-id
      :token-exists token-exists :token token)
    (if token-exists
      (render-output `(:access-token ,(issue-access-token user-id)))
      (abort-auth "Invalid or expired token"))))

(h:define-easy-handler (rest-file :uri "/api/file" :default-request-type :get)
  (type path)
  (let* ((type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth-with-query-token type-roles))
          (fs-path (fs-path type-key path))
          (path-field (path-field type-key))
          (resource (find-resource-name type-key
                      `((,type-key ,path-field :eq ,path)))))
    (unless (and (u:file-exists-p fs-path) resource)
      (abort-not-found "File not found" :file path))
    (unless (a:user-allowed *rbac* user "read" resource)
      (abort-forbidden "User lacks read permission on this file." 
        :user user :type type :file path))
    (h:handle-static-file fs-path)))

(defun serve-frontend ()
  "Serve a real file from *web-directory* if it exists; otherwise serve
index.html so client-side routing can take over."
  (let* ((web-root (uiop:ensure-directory-pathname *web-directory*))
          (rel-path (h:request-pathname h:*request* "/"))
          (file (and rel-path
                  (probe-file (merge-pathnames rel-path web-root)))))
    (h:handle-static-file
      (if (and file (not (uiop:directory-pathname-p file)))
        file
        (merge-pathnames "index.html" web-root)))))

(defun start-web-server (&optional restart)
  (when (and restart *http-server*) (stop-web-server))
  (unless *http-server*
    (setf *http-server* (make-instance 'fs-acceptor
                          :port *http-port*
                          :document-root *doc-root*))
    (setf
      h:*show-lisp-errors-p* t
      (h:acceptor-persistent-connections-p *http-server*) nil
      h:*dispatch-table* (list
                           ;; /api/*, /health
                           'h:dispatch-easy-handlers
                           ;; everything else
                           (h:create-prefix-dispatcher "/" 'serve-frontend)))
    (pl:pinfo :in "start-web-server"
      :status "web server started"
      :endpoint (format nil "http://localhost:~d" *http-port*))
    (h:start *http-server*)))

(defun stop-web-server ()
  (h:stop *http-server*)
  (setf *http-server* nil)
  (pl:pinfo :in "start-web-server" :status "web server stopped"))

