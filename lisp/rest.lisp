(in-package :data-ui)

;; Environment variables
(defparameter *http-host* (u:getenv "HTTP_HOST" :default "127.0.0.1"))
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
          :in "acceptor-log-access"
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
                     (append (list nil format-string) format-arguments))))
    (pl:plog log-severity (list :in "log-message" :text message))))
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
    (plist-to-json (list
                     :status "fail"
                     :status-code status-code
                     :error errors))))

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
  (let ((reason (if params
                  (apply #'format (append nil format-string) params)
                  format-string)))
    (pl:pdebug :in "abort-request" :error-code error-code
      :format-string format-string
      :params params
      :reason reason)
    (h:abort-request-handler (make-json-error error-code reason))))

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

(defun parse-posted-json ()
  (let ((json (h:raw-post-data :force-text t)))
    (handler-case (json-to-plist json)
      (error (e) (abort-bad-request "Invalid JSON in request body: ~a~%~a"
               json e)))))

(defun parse-id (id)
  (cond
    ((uuid-p id) id)
    ((or (null id) (zerop (length id)))
      (abort-bad-request "Parameter 'id' is required."))
    (t (abort-bad-request "The value for 'id', ~s, is not a valid UUID." id))))

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
      (abort-not-found "Type ~s not found." type-string)
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

(defun parse-filters (filters-json &key allow-id)
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
          (abort-bad-request "Bad filters ~{~s~^ ~}" filter)
          (return good))))))

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
                    (error (e) (abort-error e)))))
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
are 'list-form', 'update-form', and 'add-form'."
  (let* ((id (parse-id id))
          (type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (form-key (when form (parse-form form)))
          (result (handler-case
                    (be-rec id user :type-key type-key :form form-key)
                    (error (e) (abort-error e)))))
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
JSON-encoded list of lists."
  (let* ((type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (filters-parsed (parse-filters filters))
          (result (handler-case
                    (be-id type-key filters-parsed user)
                    (error (e) (abort-error e)))))
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

Required. The value to look up in the specified field."
  (let* ((type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (field-key (parse-field type-key field))
          (result (handler-case
                    (be-value-id type-key field-key value user)
                    (error (e) (abort-error e)))))
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
and used to look up the field in the model."
  (let* ((id (parse-id id))
          (type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (field-key (parse-field type-key field))
          (result (handler-case
                    (be-val id field-key user :form form :type-key type-key)
                    (error (e) (abort-error e)))))
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
are 'list-form', 'update-form', and 'add-form'."
  (let* ((type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (field-key (parse-field type-key field))
          (filters-parsed (parse-filters filters))
          (form-key (parse-form form))
          (result (handler-case
                    (be-list-column type-key field-key user
                      :form form-key :filters filters-parsed)
                    (error (e) (abort-error e)))))
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

The POST must include a header of 'Content-Type: application/json'."
  (let* ((tree (parse-posted-json))
          (type (getf tree :type))
          (data (getf tree :data))
          (roles (getf tree :roles))
          (type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles)))
    (multiple-value-bind (id inserted)
      (handler-case
        (be-insert type-key data user :roles roles)
        (error (e) (abort-error e)))
      (render-output (list :id id :inserted (if inserted :true :false))))))

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

The POST must include a header of 'Content-Type: application/json'."
  (let* ((tree (parse-posted-json))
          (type (getf tree :type))
          (type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (filters (parse-filters (getf tree :filters) :allow-id t))
          (data (getf tree :data))
          (roles (getf tree :roles))
          (result (handler-case
                    (be-update type-key filters data user :roles roles)
                    (error (e) (abort-error e)))))
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

The POST must include a header of 'Content-Type: application/json'."
  (let* ((tree (parse-posted-json))
          (type (getf tree :type))
          (type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (filters (parse-filters (getf tree :filters) :allow-id t))
          (result (handler-case
                    (be-delete type-key filters user)
                    (error (e) (abort-error e)))))
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
custom validation function, which you can provide as lambdas.

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
explaining why the value is invalid."
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
                    (error (e) (abort-error e)))))
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
specify with keywords. The model also supports custom validation function, which
you can provide as lambdas.

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
error messages explaining what is wrong with the value in that field."
  (let* ((tree (parse-posted-json))
          (type (getf tree :type))
          (data (getf tree :data))
          (type-key (parse-type type))
          (type-roles (get-type-roles type-key))
          (user (require-auth type-roles))
          (result (handler-case
                    (be-validate-form type-key data user)
                    (error (e) (abort-error e)))))
    (render-output result)))

(h:define-easy-handler (rest-types :uri "/api/types" :default-request-type :get)
  ()
  ":public: Endpoint for getting a list of all the defined types. This is useful
for dynamically creating a menu in the frontend."
  (let* ((user (require-auth '("logged-in"))))
    (render-output
      (mapcar
        (lambda (x) (format nil "~(~a~)" x))
        (be-types user)))))

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

(defun stop-web-server ()
  (h:stop *http-server*)
  (setf *http-server* nil))
