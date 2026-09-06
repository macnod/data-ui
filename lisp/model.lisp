(in-package :data-ui)

(defparameter *top-level-settings* nil)
(defparameter *top-level-keys*
  '(:title :name :version :domain :repl :landing-page :new-roles))

(defun parse-number (s)
  ":private: Parses S into a number. Returns the number upon success, or NIL if
the string is not a valid number. If S is is already a number, this function
returns S. If S is not a string or a number, this function returns NIL."
  (cond
    ((numberp s) s)
    ((stringp s)
      (let ((s (re:regex-replace "^\\+" (u:trim s) "")))
        (when (and
                (string s)
                (> (length s) 0)
                (re:scan "^-?\\d*(\\.\\d+)?$" s))
          (let ((num (handler-case
                       (read-from-string s)
                       (error () nil))))
            (when (numberp num) num)))))
    (t nil)))

(defun is-integer (s)
  (integerp (parse-number s)))

(defun general-type (type-key field-key)
  (let ((field-type-key (u:tree-get *compiled-model*
                          type-key :fields field-key :type)))
    (u:tree-get *field-types* field-type-key :general)))

(defun valid-value-string (value)
  (or (null value) (stringp value)))

(defun sql-type (type-key field-key field-type-key)
  (or (u:tree-get *field-types* field-type-key :sql)
    (error "Unsupported field type key ~s for field ~s ~s."
      field-type-key type-key field-key)))

(defun validation-error-string (type-key field-key value message)
  (let ((type (u:singular (format nil "~(~a~)" type-key)))
         (field (format nil "~(~a~)" field-key))
         (short-value (when (stringp value)
                        (if (> (length value) 20)
                          (format nil "~a..." (subseq value 0 20))
                          value))))
    (format nil "~a ~a~a~a"
      type
      field
      (if (not (zerop (length short-value)))
        (format nil " ~s " short-value)
        " ")
      message)))

(defun v-required (type-key field-key value user)
  (declare (ignore user))
  (let ((valid (case (general-type type-key field-key)
                 (:text (and (not (null value)) (not (equal value ""))))
                 (:number (not (null value)))
                 (:list (not (zerop (length value)))))))
    (unless valid
      (validation-error-string type-key field-key value "is required."))))

(defun v-options (type-key field-key value user)
  (declare (ignore user))
  (let ((options (u:tree-get *compiled-model*
                   type-key :fields field-key :ui :options)))
    (when (and options value (not (equal value :null)))
      (unless (member value options :test #'equal)
        (validation-error-string
          type-key field-key value
          (format nil "must be one of: ~{~a~^, ~}." options))))))

(defun v-type (type-key field-key value user)
  (declare (ignore user))
  (let* ((field-def (u:tree-get *compiled-model* type-key :fields field-key))
          (not-null (getf field-def :not-null)))
    (cond
      ((and (or (null value) (equal value :null)) (not not-null))
        nil)
      ;; :generate-uuid is a reserved default on :uuid fields (same
      ;; shape as the base-model :id default); runtime values are
      ;; always real UUID strings, but full-data fills the reserved
      ;; keyword before validation runs, so exempt it here.
      ((and (equal value :generate-uuid)
         (equal (u:tree-get *compiled-model* type-key :fields
                  field-key :type) :uuid))
        nil)
      (t
        (let* ((field-type-key (u:tree-get *compiled-model*
                                 type-key :fields field-key :type))
                (valid (case field-type-key
                         (:text (valid-value-string value))
                         (:password (valid-value-string value))
                         (:real (parse-number value))
                         (:integer (is-integer value))
                         (:boolean (member (u:make-keyword value) '(:true :false)))
                         (:uuid (re:scan *uuid-regex* value))
                         (:timestamp (re:scan *timestamp-regex* value))
                         (:list (and (listp value)
                                  (every #'valid-value-string value)))
                         (otherwise nil))))
          (unless valid
            (validation-error-string
              type-key field-key value
              (format nil "must be a valid ~s." field-type-key))))))))
(defun v-user-name (type-key field-key value user)
  (declare (ignore user))
  (unless (a:valid-user-name-p *rbac* value)
    (validation-error-string
      type-key field-key value "is not a valid user name.")))

(defun v-password (type-key field-key value user)
  (declare (ignore user))
  (unless (a:valid-password-p *rbac* value)
    (validation-error-string
      type-key field-key value "is not a valid password.")))

(defun v-email (type-key field-key value user)
  (declare (ignore user))
  (unless (a:valid-email-p *rbac* value)
    (validation-error-string
      type-key field-key value "is not a valid email address.")))

(defun v-exists (type-key field-key value user)
  (let ((table (u:tree-get *compiled-model* type-key :fields field-key
                 :source :table))
         (field (u:tree-get *compiled-model* type-key :fields field-key
                  :source :column)))
    (unless (resource-exists-p table field value user)
      (validation-error-string
        table field value "not found."))))

(defun v-join-items-exist (type-key field-key value user)
  (loop for v in value
    when (v-exists type-key field-key v user)
    collect v into missing
    finally
    (return
      (when missing
        (validation-error-string
          type-key field-key nil
          (format nil "not found: ~{~a~^, ~}" missing))))))

(defun rbac-add-user (type-key data user &key roles)
  (declare (ignore type-key))
  (let ((all-roles (add-to-list roles "settings")))
    (pl:pdebug :in "rbac-add-user" :user user
      :roles roles :all-roles all-roles)
    (a:add-user *rbac*
      (getf data :name)
      (getf data :email "no-email")
      (getf data :password)
      :roles all-roles)))

(defun rbac-remove-user (type-key data user)
  (declare (ignore type-key user))
  (a:remove-user *rbac* (getf data :name)))

(defun rbac-add-role (type-key data user &key roles)
  (declare (ignore type-key user roles))
  (let ((name (getf data :name))
         (permissions (getf data :permissions)))
    (if permissions
      (a:add-role *rbac* name :permissions permissions)
      (a:add-role *rbac* name))))

(defun rbac-remove-role (type-key data user)
  (declare (ignore type-key user))
  (a:remove-role *rbac* (getf data :name)))

(defun rbac-add-permission (type-key data user &key roles)
  (declare (ignore type-key user roles))
  (a:add-permission *rbac* (getf data :name)))

(defun rbac-remove-permission (type-key data user)
  (declare (ignore type-key user))
  (a:remove-permission *rbac* (getf data :name)))

(defun build-default-setting-plist (type-key username type-def)
  "Build an insert plist for a user-setting row. Populates :user
and any field with :default-from :user."
  (declare (ignore type-key))
  (let ((fields (getf type-def :fields))
         (plist (list :user username)))
    (loop for fk in fields by #'cddr
      for fd = (getf fields fk)
      when (eq (getf fd :default-from) :user)
      do (setf (getf plist fk) username))
    plist))

(defun add-user-setting-rows (type-key data user &key id roles record)
  "Generic post-create hook: insert a default row for every
:user-setting t type in the compiled model."
  (declare (ignore type-key id roles record))
  (let ((username (getf data :name)))
    (pl:pdebug :in "add-user-setting-rows"
      :username username)
    (loop for type-key in *compiled-model* by #'cddr
      for type-def = (getf *compiled-model* type-key)
      when (getf type-def :user-setting)
      do (be-insert-internal type-key
           (build-default-setting-plist type-key username type-def)
           user))))

(defun remove-user-setting-rows (type-key data user &key id roles record)
  "Generic pre-delete hook: remove the user's row from every
:user-setting t type."
  (declare (ignore type-key data user id roles))
  (let ((username (getf record :name)))
    (pl:pdebug :in "remove-user-setting-rows"
      :username username)
    (loop for type-key in *compiled-model* by #'cddr
      for type-def = (getf *compiled-model* type-key)
      when (getf type-def :user-setting)
      do (let ((row-id (be-value-id type-key :user username "admin")))
           (when row-id
             (delete-by-id type-key row-id))))))

;; ---------------------------------------------------------------------------
;; Hook Registry
;;
;; A curated registry of named hooks (validations, lifecycle, and actions).
;; 
;; Each entry stores: name (keyword), kind (:validation | :lifecycle | :action),
;; a parameter schema (plist of keyword → type), and a factory function that
;; accepts the resolved parameters and returns a contract-conforming function.
;;
;; Validation contract: (lambda (type-key field-key value user)
;;                        → nil | error-string)
;; ---------------------------------------------------------------------------

(defparameter *hook-registry* (make-hash-table))

(defstruct hook-entry
  name kind parameters factory)

(defun register-hook (name kind parameters factory)
  "Register a hook named NAME (keyword) of KIND (:validation, :lifecycle, or
:action).  PARAMETERS is a plist specifying required keyword params and their
types, e.g. (:max integer), or NIL if the hook takes no parameters.  FACTORY is
a function that receives the resolved parameter values as keyword args and
returns a validation/lifecycle/action function."
  (check-type name keyword)
  (check-type kind (member :validation :lifecycle :action))
  (check-type parameters (or null list))
  (check-type factory (or function symbol))
  (setf (gethash name *hook-registry*)
    (make-hook-entry :name name
      :kind kind
      :parameters parameters
      :factory factory))
  name)

(defun get-hook (name)
  "Return the HOOK-ENTRY for NAME, or NIL if not registered."
  (check-type name keyword)
  (gethash name *hook-registry*))

(defun list-hook-names (&optional kind)
  "Return a list of registered hook names, optionally filtered by KIND."
  (loop for entry being the hash-value of *hook-registry*
    when (or (null kind) (eq (hook-entry-kind entry) kind))
    collect (hook-entry-name entry)))

(defun valid-hook-params (entry plist)
  "Validate PLIST against ENTRY's parameter schema.  Returns a plist of resolved
keyword/value pairs.  Signals an error on missing or wrong-type params."
  (let ((schema (hook-entry-parameters entry))
         (hook-name (hook-entry-name entry))
         (result nil))
    (loop for (key type) on schema by #'cddr
      for val = (getf plist key)
      unless val do
      (report-ve "valid-hook-params"
        "Hook ~a requires parameter ~a"
        ~hook-name ~key)
      do
      (case type
        (:integer
          (let ((n (cond
                     ((integerp val) val)
                     ((stringp val)
                       (ignore-errors
                         (parse-integer val :junk-allowed nil)))
                     (t nil))))
            (unless n
              (report-ve "valid-hook-params"
                "Hook ~a parameter ~a must be an integer, got ~a"
                ~hook-name ~key ~val))
            (setf (getf result key) n)))
        (:number
          (let ((n (parse-number val)))
            (unless n
              (report-ve "valid-hook-params"
                "Hook ~a parameter ~a must be a number, got ~a"
                ~hook-name ~key ~val))
            (setf (getf result key) n)))
        (:string
          (unless (stringp val)
            (report-ve "valid-hook-params"
              "Hook ~a parameter ~a must be a string, got ~a"
              ~hook-name ~key ~val))
          (setf (getf result key) val))
        (otherwise
          (setf (getf result key) val))))
    result))

(defun resolve-hook-form (form &key (kind :validation)
                           type-key field-key)
  "Resolve a single hook FORM into a contract-conforming function.
FORM may be:

  - A keyword:  :required  → zero-arg registry lookup
  - A plist list: (:max-length :max 20) → parameterized registry lookup
  - A compiled function (internal base-model use only)

The registry is the sole hook surface form for model authors. TYPE-KEY and
FIELD-KEY are optional context used only for error messages.  Signals an error
for unknown hooks, wrong kind, or bad params."
  (flet ((ctx (fmt)
           (if (and type-key field-key)
             (format nil "~a for ~(~a/~a~): " fmt type-key field-key)
             (if type-key
               (format nil "~a for ~(~a~): " fmt type-key)
               (format nil "~a" fmt)))))
    (cond
      ;; Compiled function — internal base-model pass-through only
      ((functionp form)
        form)
      ;; Keyword alone: zero-arg registry entry
      ((keywordp form)
        (let ((entry (get-hook form)))
          (unless entry
            (report-e "resolve-hook-form"
              (ctx "Unknown hook: ~a")
              ~form))
          (let ((actual-kind (hook-entry-kind entry)))
            (unless (eq actual-kind kind)
              (report-e "resolve-hook-form"
                (ctx "Hook ~a is kind ~a, expected ~a")
                ~form ~actual-kind ~kind)))
          ;; Zero-arg: call factory with no params
          (funcall (hook-entry-factory entry))))
      ;; Plist list: (:hook-name :param value ...)
      ((and (consp form) (keywordp (car form)))
        (let* ((name (car form))
                (entry (get-hook name)))
          (unless entry
            (report-e "resolve-hook-form"
              (ctx "Unknown hook: ~a")
              ~name))
          (let ((actual-kind (hook-entry-kind entry)))
            (unless (eq actual-kind kind)
              (report-e "resolve-hook-form"
                (ctx "Hook ~a is kind ~a, expected ~a")
                ~name ~actual-kind ~kind)))
          (let ((params (valid-hook-params entry (cdr form))))
            (apply (hook-entry-factory entry) params))))
      (t
        (report-e "resolve-hook-form"
          (ctx "Invalid hook form: ~a")
          ~form)))))

(defun resolve-hook-list (forms &key (kind :validation)
                           type-key field-key)
  "Resolve a list of hook FORMS into a list of functions, preserving order.
TYPE-KEY and FIELD-KEY are optional context passed through to
resolve-hook-form for error messages."
  (loop for form in forms
    collect (resolve-hook-form form :kind kind
              :type-key type-key
              :field-key field-key)))

;; ---------------------------------------------------------------------------
;; Registry builtins — migrated from *validation-map*
;; ---------------------------------------------------------------------------

(register-hook :required :validation
  nil (lambda () #'v-required))
(register-hook :user-name :validation
  nil (lambda () #'v-user-name))
(register-hook :password :validation
  nil (lambda () #'v-password))
(register-hook :email :validation
  nil (lambda () #'v-email))
(register-hook :join-items-exist :validation
  nil (lambda () #'v-join-items-exist))
(register-hook :exists :validation
  nil (lambda () #'v-exists))

;;; :max-length — inclusive string length ≤ :max
(register-hook :max-length :validation
  '(:max :integer)
  (lambda (&key max)
    (lambda (type-key field-key value user)
      (declare (ignore user))
      ;; No-op on empty/nil (use :required for that)
      (when (and value (stringp value) (not (equal value "")))
        (when (> (length value) max)
          (validation-error-string type-key field-key value
            (format nil "must be at most ~d characters." max)))))))

;;; :in-range — inclusive numeric range, min ≤ value ≤ max
(register-hook :in-range :validation
  '(:min :integer :max :integer)
  (lambda (&key min max)
    (lambda (type-key field-key value user)
      (declare (ignore user))
      ;; No-op on empty/nil (use :required for that)
      (when (and value (not (equal value "")))
        (let ((num (parse-number value)))
          (cond
            ((null num)
              (validation-error-string type-key field-key value
                "must be a valid number."))
            ((or (< num min) (> num max))
              (validation-error-string type-key field-key value
                (format nil "must be between ~d and ~d." min max)))))))))

;; ---------------------------------------------------------------------------
;; :compose-string — lifecycle hook for server-side field composition
;; ---------------------------------------------------------------------------

(defun compose-string-placeholders (format)
  "Return a list of keyword placeholders found in FORMAT string.
Placeholders match :[a-z][-a-z0-9]* (colon followed by lowercase word)."
  (let ((matches nil))
    (cl-ppcre:do-scans (match-start match-end
                         reg-starts reg-ends
                         ":[a-z][-a-z0-9]*"
                         format)
      (declare (ignore reg-starts reg-ends))
      (push (u:make-keyword
              (subseq format (1+ match-start) match-end))
        matches))
    (nreverse matches)))

(defun compose-string-apply (format data)
  "Apply FORMAT template against DATA plist, returning the composed string.
Each :field-key placeholder is replaced with the stringified value
(or empty string if missing/nil). Whitespace runs are collapsed and
the result is trimmed."
  (let ((result format))
    ;; Replace each placeholder with its value
    (dolist (key (compose-string-placeholders format))
      (let ((val (getf data key)))
        (setf result
          (cl-ppcre:regex-replace-all
            (format nil ":~(~a~)" key)
            result
            (if (and val (not (eq val :null)))
              (string val)
              "")))))
    ;; Collapse whitespace runs and trim
    (u:trim
      (cl-ppcre:regex-replace-all "[ \\t]+" result " "))))

(defun valid-compose-placeholders (type-key format into model)
  "Validate that all placeholders in FORMAT name existing fields on TYPE-KEY,
and that INTO names an existing field. Signals report-e on failure."
  (let ((fields (u:plist-keys
                  (u:tree-get model type-key :fields))))
    (dolist (ph (compose-string-placeholders format))
      (unless (u:has fields ph)
        (report-e "valid-compose-placeholders"
          "Unknown placeholder ~s in :compose-string format for type ~s."
          ~ph ~type-key)))
    (unless (u:has fields into)
      (report-e "valid-compose-placeholders"
        ":compose-string :into ~s is not a field on type ~s."
        ~into ~type-key))))

(register-hook :compose-string :lifecycle
  '(:format :string :into :keyword)
  (lambda (&key format into)
    (lambda (type-key data user &key id roles record)
      (declare (ignore type-key user id roles record))
      (list into (compose-string-apply format data)))))

(defun strip-leading-lisp-comments (text)
  ":private: Removes any comment lines that may exist at the beginning of
TEXT. Returns a new string that is like TEXT, but without the comments at the
top. Note: this function retains comments in TEXT at are inside the lisp
code (or model code), removing only those that precede any Lisp."
  (loop with lines = (re:split "\\n" text)
    for line in lines
    for found = nil then (when clean-lines t)
    for clean-line = (u:trim line)
    when (or
           found
           (and
             (not (u:starts-with clean-line ";"))
             (not (zerop (length clean-line)))))
    collect line into clean-lines
    finally (return (format nil "~{~a~%~}" clean-lines))))

(defun validate-deploy-model-text (model-text)
  ":private: Validate model text for the deploy-model hook.
Returns (:ok model-plist) on success or (:error message) on failure.
Skips leading ;; comment lines and blank lines before checking for
the quoted plist."
  (flet ((ok (model-plist) (list :ok model-plist))
          (err (msg) (list :error msg)))
    (unless (and model-text (stringp model-text)
              (> (length (string-trim " " model-text)) 0))
      (return-from validate-deploy-model-text (err "model text is empty")))
    (let ((stripped (u:trim (strip-leading-lisp-comments model-text))))
      (unless (plusp (length stripped))
        (return-from validate-deploy-model-text
          (err "model text contains only comments")))
      (unless (char= (char stripped 0) #\')
        (return-from validate-deploy-model-text
          (err "model text must be a quoted plist (leading ')")))
      (let ((model-plist (ignore-errors (read-from-string stripped))))
        (unless model-plist (return-from validate-deploy-model-text
                              (err "parse error in model text")))
        (let ((model-plist (cadr model-plist)))
          (unless (and model-plist (u:plistp model-plist))
            (return-from validate-deploy-model-text
              (err "parse error in model text")))
          (let ((types (getf model-plist :types)))
            (unless types
              (return-from validate-deploy-model-text
                (err "model has no :types key")))
            (handler-case
              (progn (validate-model types)
                (ok model-plist))
              (error (e) (err (format nil "~a" e))))))))))

(defun deploy-model-write-file (model-plist package-root)
  ":private: Write MODEL-PLIST to models/<name>-<timestamp>.lisp.
Returns the relative file path (for the deploy script) and the model name."
  (let* ((model-name (or (getf model-plist :name) "model"))
          (timestamp (dt:current-unix-time))
          (filename (format nil "~a-~a.lisp" model-name timestamp))
          (models-dir (merge-pathnames "models/" package-root))
          (model-path (merge-pathnames filename models-dir))
          (model-string (with-output-to-string (s)
                          (write-char #\' s)
                          (prin1 model-plist s))))
    (ensure-directories-exist models-dir)
    (with-open-file (out model-path :direction :output :if-exists :supersede)
      (write-string model-string out))
    (values (format nil "models/~a" filename) model-name model-path)))

(defun deploy-model-git-commit (model-path model-name repo-root)
  ":private: Stage and commit the model file so the tree is clean
for deploy."
  (uiop:run-program (list "git" "add"
                      (namestring model-path))
    :input nil :directory repo-root)
  (uiop:run-program (list "git" "commit" "-m"
                      (format nil "Deploy ~a" model-name))
    :input nil :directory repo-root))

(defun deploy-model-run-script (package-root model-name)
  ":private: Run scripts/data-ui deploy <model-name>. Returns (values
stdout stderr exit-code).  Does not signal on non-zero exit — the caller
inspects exit-code and stderr."
  (let ((script-path (namestring
                       (merge-pathnames "scripts/data-ui" package-root)))
         (repo-root (namestring package-root)))
    (uiop:run-program (list script-path "deploy" model-name)
      :input nil
      :output :string :error-output :string
      :ignore-error-status t
      :directory repo-root)))

(defun deploy-model-record-secret (model-name model-domain user)
  ":private: After a successful deploy, read the generated admin password from
the deploy state directory and insert a row into the :secrets table so the user
can see it in the UI."
  (handler-case
    (let ((admin-password (get-deployed-admin-password model-name)))
      (when admin-password
        (be-insert :secrets
          (list :name (format nil "~a admin password" model-name)
            :value admin-password
            :description (format nil "Admin password for ~a"
                           (or model-domain model-name)))
          user
          :roles (list "settings"))
        (pl:pinfo :in "deploy-model-record-secret"
          :model-name model-name :status "recorded")))
    (error (e)
      (pl:perror :in "deploy-model-record-secret"
        :model-name model-name :error e
        :status "failed to record secret"))))

(defun deploy-model-async (model-plist set-status package-root user)
  ":private: Worker body for the deploy-model hook. Writes the model file,
commits it, runs the deploy script, records the admin password in the :secrets
table, and updates status.  Wraps everything in a handler-case so errors become
'failed: <message>' rather than silent thread death."
  (handler-case
    (multiple-value-bind (model-file model-name model-path)
      (deploy-model-write-file model-plist package-root)
      (declare (ignore model-file))
      (deploy-model-git-commit
        model-path model-name
        (namestring package-root))
      (multiple-value-bind (stdout stderr exit-code)
        (deploy-model-run-script package-root model-name)
        (declare (ignore stdout))
        (if (zerop exit-code)
          (progn
            (deploy-model-record-secret model-name
              (getf model-plist :domain) user)
            (funcall set-status "complete"))
          (let ((msg (format nil "deploy exited ~a: ~a"
                       exit-code
                       (string-trim '(#\Newline #\Space) stderr))))
            (pl:pinfo :in "deploy-model-async"
              :status "failed" :reason msg)
            (funcall set-status
              (format nil "failed: ~a"
                (subseq msg 0 (min (length msg) 180))))))))
    (error (e)
      (let ((msg (format nil "~a" e)))
        (pl:pinfo :in "deploy-model-async"
          :status "failed" :reason msg)
        (funcall set-status
          (format nil "failed: ~a"
            (subseq msg 0 (min (length msg) 180))))))))

;; :deploy-model — async deploy hook for Model Bank.
;;
;; Reads model text from a field (param :field), validates it in-process via
;; validate-model, then spawns a worker thread that writes the model to
;; models/<name>-<timestamp>.lisp, commits it, and shells out to
;; scripts/data-ui deploy with MODEL_FILE set.  Validation failures produce
;; an immediate "failed: <message>" without spawning a subprocess.
(register-hook :deploy-model :action
  '(:field :keyword)
  (lambda (&key field)
    (lambda (type-key field-key record user
              &key roles status-field set-status)
      (declare (ignore type-key field-key roles status-field))
      (let ((result (validate-deploy-model-text (getf record field))))
        (if (getf result :error)
          (progn
            (pl:pinfo :in "deploy-model"
              :status "failed" :reason (getf result :error))
            (list :status "failed" :message (getf result :error)))
          (let ((model-plist (getf result :ok)))
            (sb-thread:make-thread
              (lambda ()
                (deploy-model-async
                  model-plist set-status *package-root* user))
              :name "data-ui-deploy-model")
            (list :async t :message "Deploy started")))))))

(defvar *generate-model-llm-override* nil
  ":private: When non-nil, generate-model-llm-call calls this function instead
of making an HTTP request.  Used by tests.  Should be a lambda accepting a
description string and returning (:ok text) or (:error msg).")

(defun admin-secret-value (name)
  ":private: Return the :value string for admin's secret named NAME. Returns NIL
if not found."
  (loop 
    with admin-secrets = (be-list :secrets "admin" :form :update-form)
    with records = (getf admin-secrets :records)
    for record in records
    when (equal (getf record :name) name)
    do (return (getf record :value))))

(defun read-llm-config ()
  ":private: Load and parse the admin llm-config secret. Returns
(:ok config-plist) or (:error message). Requires keys :url, :model, :api-key.
Optional :temperature (default 0.3) and :max-tokens (default 16384)."
  (let* ((raw (u:trim (admin-secret-value "llm-config")))
          (wrapped (when (and raw (not (zerop (length raw))))
                     (if (u:starts-with raw "(")
                       raw
                       (format nil "(~a)" raw))))
          (parsed (when wrapped
                    (ignore-errors (read-from-string wrapped))))
          (is-plist (when parsed (u:plistp parsed)))
          (url (when is-plist (getf parsed :url)))
          (model (when is-plist (getf parsed :model)))
          (api-key (when is-plist (getf parsed :api-key)))
          (temperature (when is-plist (getf parsed :temperature 0.3)))
          (max-tokens (when is-plist (getf parsed :max-tokens 16384))))
    (cond
      ((not raw) '(:error "llm-config is not present"))
      ((not parsed) '(:error "llm-config is not a valid plist"))
      ((not url) '(:error "llm-config missing :url"))
      ((not model) '(:error "llm-config missing :model"))
      ((not api-key) '(:error "llm-config missing :api-key"))
      ((or (not (numberp temperature))
         (< temperature 0.0)
         (> temperature 1.9))
        `(:error ,(format nil "llm-config :temperature must be a floating ~
                               number between 0.0 and 1.9")))
      ((or (not (numberp max-tokens))
         (< max-tokens 1024)
         (> max-tokens 1000000))
        `(:error ,(format nil "llm-config :max-tokens must be number between ~
                               1024 and 1000000")))
      (t (list :ok (list :url url :model model :api-key api-key
                     :temperature temperature :max-tokens max-tokens))))))

(defun generate-model-build-request-json
  (model temperature max-tokens system-prompt description)
  ":private: Build the OpenAI-compatible chat completions JSON body."
  (plist-to-json
    `(:model ,model
       :temperature ,temperature
       :max_tokens ,max-tokens
       :messages ((:role "system" :content ,system-prompt)
                   (:role "user"
                     :content ,(format nil
                                 "Generate a Data UI model for this ~
                                 application description. Return ONLY ~
                                 a quoted Common Lisp plist starting ~
                                 with ', no markdown fences, no ~
                                 explanation.~%~%~a"
                                 description))))
    :nil-value "false"))

(defun generate-model-system-prompt ()
  ":private: Return the model reference as the LLM system prompt."
  (u:slurp (u:join-paths *package-root* "docs/model-reference.md")))

(defun generate-model-llm-call (description)
  ":private: Call the LLM to generate a model from DESCRIPTION.
Returns (:ok model-text) or (:error message)."
  (when *generate-model-llm-override*
    (return-from generate-model-llm-call
      (funcall *generate-model-llm-override* description)))
  (let ((config-result (read-llm-config)))
    (if (getf config-result :error)
      (list :error (getf config-result :error))
      (let* ((config (getf config-result :ok))
              (url (getf config :url))
              (api-key (getf config :api-key))
              (model (getf config :model))
              (temperature (getf config :temperature))
              (max-tokens (getf config :max-tokens))
              (system-prompt (generate-model-system-prompt)))
        (handler-case
          (let* ((body (generate-model-build-request-json
                         model temperature max-tokens
                         system-prompt description))
                  (raw-response (dr:http-request url
                                  :method :post
                                  :content-type "application/json"
                                  :accept "application/json"
                                  :force-binary t
                                  :external-format-out :utf-8
                                  :additional-headers
                                  `(("Authorization" . ,(format nil "Bearer ~a"
                                                          api-key)))
                                  :content body
                                  :connection-timeout 120))
                  (response (flex:octets-to-string raw-response
                              :external-format :utf-8)))
            (generate-model-parse-llm-response response))
          (error (e)
            (list :error (format nil "LLM request failed: ~a" e))))))))

(defun gethash-openai-content (parsed)
  ":private: Extract content from OpenAI-format response.
Returns string or nil."
  (let* ((choices (gethash "choices" parsed)))
    (when (and choices (listp choices) choices)
      (let* ((first-choice (first choices))
              (message (when (hash-table-p first-choice)
                         (gethash "message" first-choice))))
        (when (and message (hash-table-p message))
          (let ((content (gethash "content" message)))
            (when (stringp content)
              content)))))))

(defun gethash-anthropic-content (parsed)
  ":private: Extract text from Anthropic/GLM-format response.
content is a list of {type:text, text:...} blocks.  Returns the
concatenated text or nil."
  (let ((content (gethash "content" parsed)))
    (when (and content (listp content) content)
      (let ((first-block (first content)))
        (when (and (hash-table-p first-block)
                (string= (gethash "type" first-block) "text"))
          (gethash "text" first-block))))))

(defun generate-model-parse-llm-response (response)
  ":private: Extract the assistant message content from the LLM JSON response.
Handles both OpenAI format (choices[].message.content) and Anthropic/GLM format
(content[].text).  Returns (:ok text) or (:error message)."
  (handler-case
    (let* ((parsed (yason:parse response))
            ;; OpenAI format: choices[0].message.content (string)
            (content (or (gethash-openai-content parsed)
                       ;; GLM/Anthropic format: content[0].text
                       (gethash-anthropic-content parsed))))
      (if content
        (list :ok content)
        (list :error (format nil "LLM response missing message content. Raw: ~a"
                       (subseq response 0 (min (length response) 500))))))
    (error (e)
      (list :error (format nil "Failed to parse LLM response: ~a" e)))))

(defun clean-llm-response (raw-text)
  ":private: Strip markdown code fences and surrounding noise from
RAW-TEXT.  Returns the cleaned string."
  (let ((trimmed (u:trim raw-text)))
    ;; If there are code fences, extract content between first and last
    (if (re:scan "```" trimmed)
      (let* ((no-leading
               (re:regex-replace "(?s)^.*?```[a-zA-Z-]*\\s*"
                 trimmed ""))
              (no-trailing
                (re:regex-replace "(?s)```.*$" no-leading "")))
        (u:trim no-trailing))
      trimmed)))

(defun generate-model-header (description)
  ":private: Build the ;; comment header for a generated model."
  (let ((date (dt:timestamp-string))
         (prompt (string-trim '(#\Newline #\Return #\Tab) description)))
    (when (> (length prompt) 60)
      (setf prompt (concatenate 'string (subseq prompt 0 57) "...")))
    (format nil ";; Generated by Data UI Model Generator~%~
                 ;; Created: ~a~%~
                 ;; Prompt: ~a"
      date prompt)))

(defun apply-generated-model-text
  (type-key record user model-field description model-text)
  ":private: Clean + header + validate + write model text. Returns (:ok) or
(:error message).  Does not touch status. On validation failure, :model is left
untouched."
  (let* ((cleaned (clean-llm-response model-text))
          (header (generate-model-header description))
          (full-text (format nil "~a~%~a" header cleaned))
          (result (validate-deploy-model-text full-text)))
    (if (getf result :error)
      (list :error (getf result :error))
      (progn
        (be-set-field-value type-key (getf record :id)
          model-field full-text user)
        (list :ok t)))))

(defun generate-model-async
  (type-key record user description-field model-field
    description set-status)
  ":private: Worker body for the generate-model hook.  Calls the LLM, cleans the
response, validates the model, writes it to :model, and updates status.  Wraps
everything in a handler-case so errors become 'failed: <message>' rather than
silent thread death."
  (handler-case
    (multiple-value-bind (llm-result)
      (generate-model-llm-call description)
      (if (getf llm-result :error)
        (funcall set-status
          (format nil "failed: ~a"
            (subseq (getf llm-result :error)
              0 (min (length (getf llm-result :error)) 180))))
        (let ((apply-result
                (apply-generated-model-text
                  type-key record user model-field
                  description (getf llm-result :ok))))
          (if (getf apply-result :error)
            (funcall set-status
              (format nil "failed: ~a"
                (subseq (getf apply-result :error)
                  0 (min (length (getf apply-result :error)) 180))))
            (funcall set-status "complete")))))
    (condition (c)
      (let ((msg (format nil "~a" c)))
        (pl:pinfo :in "generate-model-async"
          :status "failed" :reason msg)
        (funcall set-status
          (format nil "failed: ~a"
            (subseq msg 0 (min (length msg) 180))))))))

;; :generate-model — async LLM-powered model generation hook.
;;
;; Reads a natural-language description from a field, sends it to an LLM
;; (configured via admin secrets), validates the returned model plist, and
;; writes it to the :model field.  Async like deploy.
(register-hook :generate-model :action
  '(:description-field :keyword :model-field :keyword)
  (lambda (&key description-field model-field)
    (lambda (type-key field-key record user
              &key roles status-field set-status)
      (declare (ignore type-key field-key status-field))
      (block hook
        ;; Role check: must have ai-user role
        (unless (member "ai-user" roles :test #'equal)
          (return-from hook
            (list :status "failed"
              :message "ai-user role required")))
        ;; Description must be non-empty
        (let ((description (getf record description-field)))
          (unless (and description (stringp description)
                    (> (length (string-trim " " description)) 0))
            (return-from hook
              (list :status "failed"
                :message "description is empty")))
          ;; LLM config must exist (fail fast on misconfiguration)
          (let ((config-result (read-llm-config)))
            (when (getf config-result :error)
              (return-from hook
                (list :status "failed"
                  :message (getf config-result :error)))))
          ;; Spawn async worker
          (let ((override *generate-model-llm-override*))
            (sb-thread:make-thread
              (lambda ()
                (let ((*generate-model-llm-override* override))
                  (generate-model-async
                    type-key record user description-field model-field
                    description set-status)))
              :name "data-ui-generate-model"))
          (list :async t :message "Generation started"))))))

;;
;; END Register hook :generate-model
;;

;;
;; BEGIN Register hook :spawn (template→instance completion)
;;
;; :spawn closes the record the button sits on and inserts a fresh successor:
;; the recurring-instance pattern (chores, tickets, inspection rounds).  The
;; instance is its own template — the hook copies the record it sits on.  Close
;; fields are written to the old row (durable history); clear fields reset to
;; their declared defaults on the new row; everything else (column and M2M
;; fields) is copied.  Sync by design: one be-update, one be-insert, no worker
;; thread.  Not transactional (standing MVP caveat): if the insert fails, the
;; old row stays closed with no successor and the button is re-runnable.

(defun spawn-close-value (type-key field-key value user)
  ":private: Resolve one :spawn :close VALUE for FIELD-KEY of TYPE-KEY. Reserved
values: :now (hook-run timestamp string, :timestamp fields only — enforced at
compile time) and :user (the acting user's name; wrapped in a one-element list
when the field is a :list join field, because the write replaces the join list,
it does not append).  Any other value passes through as a literal."
  (let ((field-type (u:tree-get *compiled-model* type-key :fields
                      field-key :type)))
    (case value
      (:now (dt:timestamp-string))
      (:user (if (equal field-type :list)
               (list user)
               user))
      (otherwise value))))

(defun spawn-close-data (type-key close user)
  ":private: Build the be-update data plist for :spawn's :close param, resolving
reserved values against USER.  Walks the raw plist with GETF rather than
u:plist-keys (which errors when CLOSE is not a plist; valid-spawn-params and the
factory guards already cover that)."
  (loop for (field-key value) on close by #'cddr
    appending (list field-key
                (spawn-close-value type-key field-key value user))))

(defun spawn-status-keys (type-key)
  ":private: Field keys of the companion status columns for every
:button field on TYPE-KEY (compiled model)."
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-def in (cdr fields) by #'cddr
    for status-key = (getf field-def :status-field)
    when status-key collect status-key))

(defun spawn-data (type-key record close-keys clear-keys)
  ":private: Build the be-insert data plist for :spawn: copy the record's column
and M2M field values, minus base fields, buttons, status companions, passwords,
close fields, and clear fields.  Fields left out fall back to their declared
defaults via full-data inside be-insert.  The walk is over the compiled field
list, never the raw record plist — :id, :roles, and timestamps cannot leak in."
  (let ((status-keys (spawn-status-keys type-key)))
    (loop with fields = (u:tree-get *compiled-model* type-key :fields)
      for field-key in fields by #'cddr
      for field-def in (cdr fields) by #'cddr
      for field-type = (getf field-def :type)
      for copyable = (or (getf field-def :column)
                       (getf field-def :join-table))
      unless (or (getf field-def :base-field)
               (equal field-type :button)
               (equal field-type :password)
               (member field-key status-keys)
               (member field-key close-keys)
               (member field-key clear-keys)
               (not copyable))
      append (list field-key (getf record field-key)))))

(defun spawn-inherit-roles (old-id new-id)
  ":private: Copy the old row's resource roles onto the spawned row so the
successor is exactly as visible as the record it replaces (same move as
write-through's execute-write-to).  No-op when either row has no resource name."
  (let* ((old-resource (id-to-resource-name old-id))
          (new-resource (id-to-resource-name new-id)))
    (when (and old-resource new-resource)
      (let ((existing (a:list-resource-role-names *rbac* new-resource)))
        (loop for role in (a:list-resource-role-names *rbac* old-resource)
          unless (member role existing :test 'equal)
          do (a:add-resource-role *rbac* new-resource role))))))

(defun spawn-button-status-keys (model type-key)
  ":private: Field keys of the status companions for every :button
field on TYPE-KEY in the raw (pre-compile) model."
  (loop with fields = (u:tree-get model type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    when (equal (getf field-def :type) :button)
    collect (u:make-keyword (format nil "~a-status" field-key))))

(defun valid-spawn-params (model type-key field-key action-form)
  ":private: Compile-time validation for :spawn :action forms, called from
compile-field's button branch, where TYPE-KEY and the raw form are in hand (the
registry factory never receives the type).  Checks that :close / :clear name
real column or M2M fields (not buttons, status companions, or base fields), that
literal close values pass their field's type predicate, that :now is only used
on :timestamp fields and :user on :text or M2M list fields, that no field
appears in both lists, and that every :unique t / :identity t field on the type
is in :clear (a copied unique value can only collide; :identity t emits a unique
index even without :unique t).  Returns ACTION-FORM."
  (let* ((params (cdr action-form))
          (close (getf params :close))
          (clear (getf params :clear))
          (close-keys (u:plist-keys close))
          (clear-keys clear)
          (fields (u:tree-get model type-key :fields))
          (status-keys (spawn-button-status-keys model type-key)))
    ;; Field membership: exists, not base/button/status, column or M2M
    (dolist (field-key (append close-keys clear-keys))
      (let ((field-def (getf fields field-key)))
        (cond
          ((null field-def)
            (report-ve "valid-spawn-params"
              ":spawn field ~s does not exist on type ~s."
              ~field-key ~type-key))
          ((or (getf field-def :base-field)
             (equal (getf field-def :type) :button)
             (member field-key status-keys))
            (report-ve "valid-spawn-params"
              ":spawn field ~s on type ~s is not a close/clear target ~
               (base, button, or status field)."
              ~field-key ~type-key))
          ((not (or (getf field-def :column)
                  (getf field-def :join-table)))
            (report-ve "valid-spawn-params"
              ":spawn field ~s on type ~s is not a column or M2M field."
              ~field-key ~type-key)))))
    ;; Close values: reserved or literal passing the field predicate
    (loop for field-key in close-keys
      for value = (getf close field-key)
      for field-def = (getf fields field-key)
      for field-type = (or (getf field-def :type) :text)
      do
      (case value
        (:now
          (unless (equal field-type :timestamp)
            (report-ve "valid-spawn-params"
              ":spawn :now is only valid on :timestamp fields; field ~s ~
               on type ~s is ~s."
              ~field-key ~type-key ~field-type)))
        (:user
          (unless (or (equal field-type :text)
                    (and (equal field-type :list)
                      (getf field-def :join-table)))
            (report-ve "valid-spawn-params"
              ":spawn :user is only valid on :text or M2M list fields; ~
               field ~s on type ~s is ~s."
              ~field-key ~type-key ~field-type)))
        (otherwise
          (let ((test (u:tree-get *field-types* field-type :test)))
            (unless (and test (funcall test value))
              (report-ve "valid-spawn-params"
                ":spawn close value ~s does not match type ~s of field ~
                 ~s on type ~s."
                ~value ~field-type ~field-key ~type-key))))))
    ;; No field in both :close and :clear
    (dolist (field-key close-keys)
      (when (member field-key clear-keys)
        (report-ve "valid-spawn-params"
          ":spawn field ~s on type ~s appears in both :close and :clear."
          ~field-key ~type-key)))
    ;; Unique / identity fields must reset to defaults, never copy
    (loop for field-key in fields by #'cddr
      for field-def in (cdr fields) by #'cddr
      when (or (getf field-def :unique) (getf field-def :identity))
      unless (member field-key clear-keys)
      do (report-ve "valid-spawn-params"
           ":spawn field ~s on type ~s is :unique t / :identity t (a copied ~
         value can only collide); add it to :clear."
           ~field-key ~type-key))
    action-form))

(register-hook :spawn :action
  '(:close :list :clear :list)
  (lambda (&key close clear)
    ;; Param-shape guards: a bare-keyword action form skips valid-hook-params,
    ;; so the factory re-checks presence and shape here (it cannot check fields;
    ;; no type-key reaches the factory).
    (unless (and (u:plistp close) close)
      (report-ve "spawn-hook-factory"
        ":spawn requires a non-empty :close plist (field → value)."))
    (unless (and (listp clear) clear (every #'keywordp clear))
      (report-ve "spawn-hook-factory"
        ":spawn requires :clear as a non-empty list of field keys."))
    (lambda (type-key field-key record user
              &key roles status-field set-status)
      (declare (ignore field-key roles status-field set-status))
      ;; Close first: the old row becomes history.  A later insert failure
      ;; leaves it closed with no successor; the button is re-runnable (no
      ;; transactions — standing MVP caveat).
      (be-update type-key (getf record :id)
        (spawn-close-data type-key close user) user)
      ;; Then spawn: copy + defaults through the ordinary insert path, as the
      ;; acting user (create permission required).
      (multiple-value-bind (new-id inserted)
        (be-insert type-key
          (spawn-data type-key record
            (loop for k in close by #'cddr collect k)
            clear)
          user)
        (declare (ignore inserted))
        (spawn-inherit-roles (getf record :id) new-id))
      ;; Sync success: be-action sets the status column to "complete".
      nil)))

;;
;; END Register hook :spawn
;;

(defparameter *forms* '(:list-form :add-form :update-form))

(defparameter *widgets*
  '(:textbox :textarea :code :stars :checkbox :checkbox-list :select
     :file :password :button :hidden :image :image-list)
  "Allowed values for the :widget key on a field :ui plist.")

(setq *base-model*
  `(:users
     (:table t :base t :built-in t
       :create ,#'rbac-add-user
       ;; RBAC API doesn't include an update function, so just use :auto here
       :update :auto
       :delete ,#'rbac-remove-user
       :display t
       ;; Role logged-in already exists, with only read permission.  Role
       ;; user-creator does not exist, and is created with default permissions
       ;; create, read, update, and delete. If you want more specific
       ;; permissions, just pass a list instead of a role string. The first
       ;; element of the role string must be the role. The remaining elements,
       ;; of which there must be at least one, must be existing permissions.
       :type-roles ("logged-in" "user-creator")
       ;; TODO: Add processing for the :post-create key.
       :post-create ,#'add-user-setting-rows
       :pre-delete ,#'remove-user-setting-rows
       :views (:main (:tables (:users :role-users :roles))
                :roles (:tables (:roles)))
       :fields (:name (:type :text :identity t
                        :source (:view :main :column :name :agg :first)
                        :ui (:label "Username" :widget :textbox)
                        :validations (:required :user-name)
                        :column t :searchable t :not-null t :unique t)
                 :password (:type :password
                             :source (:view :main :column :password :agg :first)
                             :force-sql-name "password_hash"
                             :ui (:label "Password" :widget :password)
                             :validations (:required :password)
                             :column t :not-null t)
                 :email (:type :text
                          :default "no-email"
                          :force-sql-name "email"
                          :source (:view :main :column :email :agg :first)
                          :ui (:label "Email" :widget :textbox)
                          :validations (:email)
                          :column t :searchable t :not-null t)
                 :roles (:type :list
                          :ui (:label "Roles" :widget :checkbox-list)
                          :validations (:join-items-exist)
                          :source (:view :main :table :roles :column :name :agg :distinct)
                          :source-all (:view :roles :table :roles :column :name :agg :list)
                          :join-table :role-users))
       :list-form (:fields (:name :created-at :updated-at :email :roles))
       :update-form (:fields t)
       :add-form (:fields t))

     ;; TODO: Mark as internal. User should not be able to interact with this table
     ;; via the UI or the backend functions.
     :resources
     (:table t :base t :built-in t :internal t
       :create nil :update nil :delete nil :display nil
       :views (:main (:tables (:resources :resource-roles :roles))
                :roles (:tables (:roles)))
       :fields (:name (:type :text :identity t
                        :ui (:label "Resource" :widget :textbox)
                        :source (:view :main :column :name :agg :first)
                        :validations (:required)
                        :column t :not-null t :unique t)
                 :roles (:type :list
                          :ui (:label "Roles" :widget :checkbox-list)
                          :source (:view :main :table :roles :column :name :agg :distinct)
                          :source-all (:view :roles :table :roles :column :name :agg :list)
                          :validations (:required)
                          :ids-table :roles
                          :join-table :resource-roles))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     ;; TODO: Currently, a default list such as ("create" "read" ...) doesn't work
     ;; because of some kind of validation issue. Fix.
     :permissions
     (:table t :base t :built-in t
       :create ,#'rbac-add-permission
       :update :auto
       :delete ,#'rbac-remove-permission
       :display t
       :type-roles ("logged-in" "permission-creator")
       :fields (:name (:type :text :identity t
                        :ui (:label "Permission" :widget :textbox)
                        :source (:view :main :column :name :agg :first)
                        :column t :not-null t :unique t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :roles
     (:table t :base t :built-in t
       :create ,#'rbac-add-role
       :update :auto
       :delete ,#'rbac-remove-role
       :display t
       :type-roles ("logged-in" "role-creator")
       :views (:main (:tables (:roles :role-permissions :permissions))
                :permissions (:tables (:permissions)))
       :fields (:name (:type :text :identity t
                        :ui (:label "Role" :widget :textbox)
                        :source (:view :main :column :name :agg :first)
                        :column t :not-null t :unique t)
                 :permissions (:type :list
                                :ui (:label "Permissions" :widget :checkbox-list)
                                :source (:view :main
                                          :table :permissions
                                          :column :name
                                          :agg :distinct)
                                :source-all (:view :permissions
                                              :table :permissions
                                              :column :name
                                              :agg :list)
                                :join-table :role-permissions))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :role-permissions
     (:table t :base t :built-in t :is-joiner t :internal t
       :fields (:reference (:target :roles)
                 :reference (:target :permissions)))

     :resource-roles
     (:table t :base t :built-in t :is-joiner t :internal t
       :fields (:reference (:target :resources)
                 :reference (:target :roles)))

     :role-users
     (:table t :base t :built-in t :is-joiner t :internal t
       :fields (:reference (:target :roles)
                 :reference (:target :users)))

     :settings
     (:table t :built-in t
       :base t
       :create nil :update :auto :delete nil :display t
       :type-roles ("settings")
       :user-setting t
       :views (:main (:tables (:settings :users) :scope :user)
                :users (:tables (:users)))
       :fields (:user (:type :text :identity t
                        ;; TODO: This should not be needed. Fix compiler.
                        :force-sql-name "setting_user"
                        :ui (:label "Login" :widget :textbox :read-only t)
                        :target :users
                        :source (:view :users :table :users :column :name :agg :first)
                        :column t :not-null t :unique t)
                 :dark-mode (:type :boolean :default :false
                              :css-value t
                              :ui (:label "Dark Mode" :widget :checkbox)
                              :source (:view :main :column :dark-mode :agg :first)
                              :column t :not-null t)
                 :display-name (:type :text :default "(non specified)"
                                 :default-from :user
                                 :ui (:label "Real Name" :widget :textbox)
                                 :source (:view :main :column :display-name :agg :first)
                                 :column t :not-null t)
                 :bio (:type :text :default "(non specified)"
                        :ui (:label "Bio" :widget :textarea)
                        :source (:view :main :column :bio :agg :first)
                        :column t :not-null t))
       :list-form (:fields (:user :dark-mode :display-name :bio))
       :update-form (:fields t)
       :add-form (:fields t))

     :secrets
     (:table t :built-in t
       :base nil
       :create :auto :update :auto :delete :auto :display t
       :type-roles ("settings")
       :suppress-roles t
       :category :settings
       :views (:main (:tables (:secrets :users) :scope :user)
                :users (:tables (:users)))
       :fields (:user (:type :text
                        :force-sql-name "secret_user"
                        :ui (:label "Login" :widget :textbox :read-only t)
                        :target :users
                        :autofill :user
                        :source (:view :users :table :users
                                  :column :name :agg :first)
                        :column t :not-null t)
                 :name (:type :text
                         :ui (:label "Name" :widget :textbox)
                         :source (:view :main :column :name :agg :first)
                         :column t :not-null t)
                 :value (:type :text
                          :ui (:label "Value" :widget :textarea)
                          :source (:view :main :column :value :agg :first)
                          :column t :not-null t)
                 :description (:type :text
                                :ui (:label "Description" :widget :textarea)
                                :source (:view :main
                                          :column :description :agg :first)
                                :column t))
       :list-form (:fields (:user :name :description))
       :update-form (:fields t)
       :add-form (:fields t))

     :tokens
     (:table t :base nil :built-in t
       :create nil :update nil :delete nil :display nil
       :views (:main (:tables (:tokens)))
       :fields (:user (:type :text
                        ;; TODO: We shouldn't need to have :ui hints for this
                        ;;       type, yet there are failures if we don't
                        ;;       include :ui here.
                        :ui (:label "User" :widget :textbox)
                        ;; TODO: A bad :view, :table, :column, or :agg should
                        ;;       raise a compile-time error.
                        :source (:view :main :column :user :agg :first)
                        :column t :not-null t :unique t)
                 :value (:type :text
                          :ui (:label "Value" :widget :textbox)
                          :source (:view :main :column :value :agg :first)
                          :column t :not-null t :unique t)))))

(defun to-sql-identifier (keyword &key (format-string "~a") (form :as-is))
  (let ((s (format nil "~(~a~)" keyword)))
    (format nil format-string
      (re:regex-replace-all
        "-"
        (case form
          (:as-is s)
          (:singular (u:singular s))
          (:plural (u:plural s))
          (otherwise (error "Unsupported value for FORM: ~a" form)))
        "_"))))

(defun table-name (type-key &optional
                    (built-in (built-in-p type-key)))
  (let ((format-string (if built-in "~a" "rt_~a")))
    (to-sql-identifier type-key :format-string format-string)))

(defun table-reference (keyword)
  (when keyword
    (u:make-keyword
      (format nil "~a-id" (u:singular (format nil "~a" keyword))))))

(defun column-name (model type-key field-key field-def)
  (when (or (getf field-def :column) (getf field-def :target))
    (let* ((table-name (table-name type-key (built-in-p type-key model)))
            (singular-table-name (re:regex-replace "^rt_" (u:singular table-name) ""))
            (force-sql-name (getf field-def :force-sql-name))
            (simple (and (u:tree-get model type-key :is-joiner)
                      (getf field-def :target)))
            (field-name (to-sql-identifier field-key)))
      (cond
        (simple (to-sql-identifier field-key))
        (force-sql-name force-sql-name)
        ((member field-key (default-fields :keys-only t))
          (to-sql-identifier field-key))
        (t (format nil "~a_~a" singular-table-name field-name))))))

(defun updated-at-trigger-sql (table)
  (format nil
    "
do $$
begin
    if not exists (
        select 1 from pg_trigger
        where tgname = 'set_~a_updated_at'
        and tgrelid = '~a'::regclass::oid
    ) then
        create trigger set_~a_updated_at
            before update on ~a
            for each row
            execute function set_updated_at_column();
    end if;
end $$;
"
    table table table table))

(defun sort-index-ddl (table fields)
  ":private: Returns a list of CREATE INDEX DDL strings for sortable fields that
lack an existing single-column index. Skips fields that already have :unique t
or are the sole :identity field (those already have a covering index). Returns
nil when no new indexes are needed."
  (loop 
    with id-fields = (remove-if-not
                       (lambda (f) (getf f :identity))
                       (u:plist-values fields))
    with sole-identity-col = (when (= (length id-fields) 1)
                               (getf (car id-fields) :name-sql))
    for field in (u:plist-values fields)
    for sortable = (getf field :sortable)
    for col = (getf field :name-sql)
    for unique-p = (getf field :unique)
    when (and sortable col
           (not unique-p)
           (not (equal col sole-identity-col)))
    collect (format nil
              "create index if not exists ix_~a_~a ~
                           on ~a (~a)"
              table col table col)))

(defun create-table-sql (table fields)
  (let ((identity-fields (remove-if-not
                           (lambda (f) (getf f :identity))
                           (u:plist-values fields))))
    (list
      :table (format
               nil
               "~%create table if not exists ~a (~%    ~{~a~^,~%    ~}~%)~%"
               table
               (mapcar
                 (lambda (field) (getf field :create-sql))
                 (remove-if-not
                   (lambda (field) (getf field :column))
                   (u:plist-values fields))))
      :trigger (updated-at-trigger-sql table)
      :index (when identity-fields
               (format nil
                 "create unique index if not exists ix_~a_identity ~
                on ~a (~{~a~^, ~})"
                 table table
                 (mapcar (lambda (f) (getf f :name-sql)) identity-fields)))
      :sort-index (sort-index-ddl table fields))))

(defun filtered-fields (fields keys)
  "Returns field key and definition for fields that have non-NIL values for all
KEYS."
  (loop
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    when (every (lambda (k) (getf field-def k)) keys)
    append (list field-key field-def)))

(defun fields-attribute (fields have-keys &optional attribute)
  "Returns a list of cons pairs of field key and the value of ATTRIBUTE for
fields that have non-NIL values for all HAVE-KEYS."
  (loop with some-fields = (filtered-fields fields have-keys)
    for field-key in some-fields by #'cddr
    for field-def in (cdr some-fields) by #'cddr
    collect
    (cons
      field-key
      (if attribute
        (getf field-def attribute)
        field-def))))

(defun joiner-fk-columns (model joiner-type-key)
  "Return (field-key . name-sql) pairs for JOINER-TYPE-KEY's :target columns
only."
  (fields-attribute
    (u:tree-get model joiner-type-key :fields)
    '(:target)
    :name-sql))

(defun insert-keys (model type-key)
  (loop
    with fields = (u:tree-get model type-key :fields)
    and base = (u:tree-get model type-key :base)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for source-table = (u:tree-get field-def :source :table)
    when (getf field-def :join-table)
    collect field-key into keys
    finally (return
              (if base
                (cons :main keys)
                (append '(:resource :main) keys)))))

(defun insert-sql (model type-key)
  (loop
    with fields = (u:tree-get model type-key :fields)
    with insert-keys = (insert-keys model type-key)
    with res-columns = (list
                         (cons
                           :name
                           (u:tree-get model :resources :fields :name :name-sql)))
    with main-columns = (cons '(:id . "id")
                          (fields-attribute fields '(:column :ui) :name-sql))
    with sql = "insert into ~a (~{~a~^, ~}) values (~{~a~^, ~}) returning id"
    for key in insert-keys
    for table-key = (case key
                      (:resource :resources)
                      (:main type-key)
                      (otherwise (u:tree-get fields key :join-table)))
    for table-name = (table-name table-key (built-in-p table-key model))
    for table-cols = (case key
                       (:resource res-columns)
                       (:main (if (u:tree-get model table-key :base)
                                (cdr main-columns)
                                main-columns))
                       (otherwise (joiner-fk-columns model table-key)))
    append (list key (cons
                       (format nil sql
                         table-name
                         (mapcar #'cdr table-cols)
                         (placeholders table-cols))
                       (mapcar #'car table-cols)))))

(defun update-keys (model type-key)
  (loop
    with fields = (u:tree-get model type-key :fields)
    and base = (u:tree-get model type-key :base)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for join-table = (getf field-def :join-table)
    when join-table
    append (list field-key join-table)
    into keys
    finally (return (append (list :main type-key) keys))))

(defun update-sql (model type-key)
  (loop
    with fields = (u:tree-get model type-key :fields)
    with update-keys = (update-keys model type-key)
    with columns = (fields-attribute fields '(:column :ui) :name-sql)
    with main-sql = "update ~a set ~{~a = ~a~^, ~} where id = $~d"
    with delete-sql = "delete from ~a where ~{~a = ~a~^ and ~}"
    with insert-sql = "insert into ~a (~{~a~^, ~}) values (~{~a~^, ~})"
    for key in update-keys by #'cddr
    for table in (cdr update-keys) by #'cddr
    for table-name = (table-name table (built-in-p table model))
    for table-cols = (case key
                       (:main columns)
                       (otherwise (joiner-fk-columns model table)))
    for sql = (case key
                (:main main-sql)
                (otherwise (list
                             :insert insert-sql
                             :delete delete-sql)))
    append
    (list key (case key
                (:main
                  (cons
                    (format nil sql
                      table-name
                      (u:zip (mapcar #'cdr table-cols) (placeholders table-cols))
                      (1+ (length table-cols)))
                    (append (mapcar #'car table-cols) '(:id))))
                (t
                  (list
                    :delete (cons
                              (format nil (getf sql :delete)
                                table-name
                                (u:zip
                                  (mapcar #'cdr table-cols)
                                  (placeholders table-cols))
                                (mapcar #'car table-cols))
                              (mapcar #'car table-cols))
                    :insert (cons
                              (format nil (getf sql :insert)
                                table-name
                                (mapcar #'cdr table-cols)
                                (placeholders table-cols))
                              (mapcar #'car table-cols))))))))

(defun table-column (model type-key field-key)
  (let* ((type-def (u:tree-get model type-key))
          (built-in (u:tree-get type-def :built-in))
          (field-def (u:tree-get type-def :fields field-key))
          (table-name (table-name type-key built-in))
          (column-name (column-name model type-key field-key field-def))
          (column-list (list table-name column-name))
          (column-string (format nil "~{~a~^.~}" column-list))
          (alias-list (list table-name column-name))
          (alias-string (format nil "~{~a~^_~}" alias-list)))
    (values column-string alias-string)))

(defun table-columns (model type-key)
  (loop
    with fields = (u:tree-get model type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for (column alias) = (multiple-value-bind (c a)
                           (table-column model type-key field-key)
                           (list c a))
    when (getf field-def :column)
    collect (list
              :table type-key
              :field-key field-key
              :alias alias
              :alias-key (u:make-keyword alias)
              :column column
              :column-key (u:make-keyword column))))

(defun max-string-width (strings)
  (loop for s in strings
    for l = (length s)
    for max = l then (if (> l max) l max)
    finally (return max)))

(defun formatted-table-columns (model type-keys)
  (loop for type-key in type-keys
    for table-columns = (table-columns model type-key)
    for cc = (mapcar (lambda (c) (getf c :column)) table-columns)
    for aa = (mapcar (lambda (c) (getf c :alias)) table-columns)
    append cc into columns
    append aa into aliases
    finally (let* ((width (max-string-width columns))
                    (format-string (format nil "~~~da" width))
                    (fcolumns (mapcar
                                (lambda (s) (format nil format-string s))
                                columns)))
              (return
                (mapcar
                  (lambda (a b) (format nil "~a ~b" a b))
                  fcolumns aliases)))))

(defun find-table-xrefs (model view-tables table)
  ":private: Returns a list of xrefs that connect TABLE to any table in
VIEW_TABLES. An xref describes a connection between two tables and looks like
this:
    (:source source-table-key
     :source-field source-field-key
     :target target-table-key)"
  (loop with fields = (u:tree-get model table :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for target = (getf field-def :target)
    when (and target (member target view-tables))
    collect (list
              :source table
              :source-field field-key
              :target target)))

(defun xref-joinable-p (source target joined-tables)
  ":private: T when exactly one of SOURCE / TARGET is in JOINED-TABLES. An edge
with both endpoints joined is a redundant second path to a table that is already
reachable; an edge with neither endpoint joined cannot become a join clause yet."
  (and (or (member source joined-tables)
         (member target joined-tables))
    (not (and (member source joined-tables)
           (member target joined-tables)))))

(defun find-joinable-xref-connecting (table xrefs joined-tables base-table)
  ":private: Returns the first xref where TABLE is either :source or :target and
exactly one of the xref's endpoints is already joined. When several edges
qualify, an edge whose already-joined endpoint is BASE-TABLE (the view's first
table) wins, so a satellite with FKs into several joined tables joins through
the view subject rather than a sibling satellite. Edges whose endpoints are both
joined (a redundant second path to an already-reachable table) are skipped, not
consumed."
  (labels ((touching-p (xref)
             (or (equal (getf xref :source) table)
               (equal (getf xref :target) table)))
            (joinable-p (xref)
              (xref-joinable-p (getf xref :source)
                (getf xref :target) joined-tables))
            (base-anchor-p (xref)
              (and (or (equal (getf xref :source) base-table)
                     (equal (getf xref :target) base-table))
                (joinable-p xref))))
    (or (find-if
          (lambda (x) (and (touching-p x) (base-anchor-p x)))
          xrefs)
      (find-if
        (lambda (x) (and (touching-p x) (joinable-p x)))
        xrefs))))

(defun ordered-xrefs (model view-tables)
  ":private: Returns a list of xrefs in the context of VIEW-TABLES. The xrefs
are returned in the proper order, such that when they're used serially to create
join clauses, any external references point to tables that have already been
joined.

The walk processes VIEW-TABLES in order and, for each table NOT yet joined,
consumes the first remaining edge that touches it and has exactly one endpoint
already joined, preferring an edge anchored at the view's first table. A table
that an earlier turn already joined (as an edge's far endpoint) gets no turn:
consuming another edge for it would re-join it through a second path and steal
the edge a later table needs (the modelbank :ratings shape: :models joined via
ratings.rating_model, then :models' own model_user edge would wrongly claim
:users). Redundant edges (both endpoints joined, a second path to an
already-reachable table) are skipped naturally: no unjoined table claims them.

Because a table's turn only sees edges with exactly one endpoint joined, the
join column chosen for a satellite depends on which edges remain, not on field
declaration order within a type: :ratings (:model ... :user ...) and
:ratings (:user ... :model ...) compile to the same view SQL.

MVP limitation: this ordering works for linear chains (A->B->C), star
patterns (A<-B, A<-C), and fan-in shapes with redundant edges. It can
still starve a table in diamond patterns (A->B, A->C, B->D, C->D) where a
table is reachable only via two paths and the :tables order never yields
a joinable edge for it. This is acceptable for the MVP."
  (loop for table in view-tables
    append (find-table-xrefs model view-tables table) into xrefs
    finally
    (return
      (loop
        with remaining-xrefs = (u:deep-copy xrefs)
        with joined-tables = (list (car view-tables))
        with base-table = (car view-tables)
        for j-table in view-tables
        for j-xref = (unless (member j-table joined-tables)
                       (find-joinable-xref-connecting j-table
                         remaining-xrefs joined-tables base-table))
        when j-xref
        do (setf remaining-xrefs (remove j-xref remaining-xrefs :test #'equal))
        and do (pushnew (if (member (getf j-xref :source) joined-tables)
                          (getf j-xref :target)
                          (getf j-xref :source))
                 joined-tables)
        and collect j-xref))))

(defun xref-reversed (source target joined-tables)
  ":private: Sometimes, when creating a join clause from xrefs, the source table
is joined. Other times, the source table has already been joined and instead the
target table must be joined. This function returns a boolean value that
indicates if the xref's source and target tables should be reversed. SOURCE is
the source table from the xref. TARGET is the target table from the xref.
JOINED-TABLES is a list of tables that have already been joined."
  (cond
    ((and
       (member source joined-tables)
       (not (member target joined-tables)))
      t)
    ((and
       (not (member source joined-tables))
       (member target joined-tables))
      nil)
    (t (error "Cannot determine join direction: neither or both of source ~
(~s) and target (~s) are in the already-joined tables ~s"
         source target joined-tables))))

(defun view-sql (model view-def)
  ":private: Returns an SQL query that selects all the fields associated with
the tables in VIEW-DEF (a list of type/table keys). If more than one table
is provided in VIEW-DEF, then the SQL query will necessarily include joins,
which this code assembles automatically. This code also aliases the fields as
necessary."
  (loop
    with view-tables = (u:deep-copy (getf view-def :tables))
    with joined-tables = (list (car view-tables))
    for xref in (ordered-xrefs model view-tables)
    for source = (getf xref :source)
    for target = (getf xref :target)
    for source-field = (getf xref :source-field)
    for reversed = (xref-reversed source target joined-tables)
    for join-table = (if reversed
                       (u:tree-get model target :table-name)
                       (u:tree-get model source :table-name))
    for join-field = (if reversed
                       "id"
                       (u:tree-get model source :fields source-field :name-sql))
    for target-table = (if reversed
                         (u:tree-get model source :table-name)
                         (u:tree-get model target :table-name))
    for target-field = (if reversed
                         (u:tree-get model source :fields source-field :name-sql)
                         "id")
    for join = (format nil "left join ~a on ~a.~a = ~a.~a"
                 join-table join-table join-field target-table target-field)
    collect join into joins
    do (push (if reversed target source) joined-tables)
    finally
    (let ((format-string "~%select~%~{  ~a~^,~%~}~%from ~{~a~^~%  ~}")
           (first-table (u:tree-get model (car view-tables) :table-name))
           (columns (formatted-table-columns model (getf view-def :tables))))
      (return
        (format nil format-string columns (cons first-table joins))))))

(defun phase-a-join-sql (model view-def)
  ":private: Returns Phase A SQL for join-filter pushdown: SELECT DISTINCT
<base-table>.id with the same FROM/JOIN structure as VIEW-SQL but selecting
only the base table's id column. Used when request-time filters reference
joined tables."
  (loop
    with view-tables = (u:deep-copy (getf view-def :tables))
    with joined-tables = (list (car view-tables))
    for xref in (ordered-xrefs model view-tables)
    for source = (getf xref :source)
    for target = (getf xref :target)
    for source-field = (getf xref :source-field)
    for reversed = (xref-reversed source target joined-tables)
    for join-table = (if reversed
                       (u:tree-get model target :table-name)
                       (u:tree-get model source :table-name))
    for join-field = (if reversed
                       "id"
                       (u:tree-get model source :fields source-field :name-sql))
    for target-table = (if reversed
                         (u:tree-get model source :table-name)
                         (u:tree-get model target :table-name))
    for target-field = (if reversed
                         (u:tree-get model source :fields source-field :name-sql)
                         "id")
    for join = (format nil "left join ~a on ~a.~a = ~a.~a"
                 join-table join-table join-field target-table target-field)
    collect join into joins
    do (push (if reversed target source) joined-tables)
    finally
    (let ((first-table (u:tree-get model (car view-tables) :table-name)))
      (return
        (format nil "select distinct ~a.id from ~{~a~^~%  ~}"
          first-table (cons first-table joins))))))

(defun delete-sql (model type-key)
  (let* ((base (u:tree-get model type-key :base))
          (table-name (table-name type-key (built-in-p type-key model))))
    (if base
      (list (format nil "delete from ~a where id = $1" table-name) :id)
      (list "delete from resources where id = $1" :id))))

(defun search-sql (model type-key)
  (loop with fields = (u:tree-get model type-key :fields)
    and table-name = (table-name type-key (built-in-p type-key model))
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for column = (getf field-def :name-sql)
    when (getf field-def :identity)
    collect field-key into keys
    and collect column into cols
    finally
    (return
      (when keys
        (cons
          (format nil "select id from ~a where ~{~a = ~a~^ and ~}"
            table-name
            (u:zip cols (placeholders cols)))
          keys)))))

(defun value-sql (value field-type-key &key quote)
  (case field-type-key
    (:boolean (case value
                (:true (if quote "'true'" "true"))
                (:false (if quote "'false'" "false"))
                (otherwise (error "Unsupported boolean value '~a'" value))))
    (:integer (cond
                ((equal value :null) nil)
                ((integerp value) value)
                (t (error "Invalid integer value '~a'" value))))
    (:float (cond
              ((equal value :null) nil)
              ((numberp value) value)
              (t (error "Invalid float value '~a'" value))))
    (:timestamp (cond
                  ((equal value :null) nil)
                  ((equal value :now) "now()")
                  ((and
                     (stringp value)
                     (re:scan *timestamp-regex* value))
                    (if quote (format nil "'~a'" value) value))
                  (t (error "Invalid timestamp string '~a'" value))))
    (:text (cond
             ((equal value :null) nil)
             ((stringp value)
               (if quote (format nil "'~a'" value) value))
             (t (error "Invalid text value '~a'" value))))
    (:uuid (cond
             ((equal value :null) nil)
             ((equal value :generate-uuid) "uuid_generate_v4()")
             ((and
                (stringp value)
                (re:scan *uuid-regex* value))
               value)
             (t (error "Invalid UUID value '~a'" value))))
    (otherwise (error "Unknown FIELD-TYPE-KEY '~a'" field-type-key))))

(defun field-source (model type-key field-key field-def name-key)
  (let* ((source (or
                   (getf field-def :source)
                   (when (getf field-def :column)
                     `(:view :main :column ,name-key :agg :first))))
          (source-table (getf source :table))
          (t-key (if source-table source-table type-key))
          (f-key (getf source :column))
          (internal (or (u:tree-get model type-key :internal)
                      (u:tree-get model type-key :join-table))))
    (if internal
      source
      (progn
        (when (and
                t-key
                f-key
                (not (or
                       (u:tree-get model t-key :fields f-key)
                       (member f-key (default-fields :keys-only t)))))
          (error "Unknown field at ~(~s~) :fields ~(~s~) :source ~(~s~)"
            type-key field-key f-key))
        (when (and source t-key (not f-key))
          (error ":column spec missing from field ~(~s~) ~(~s~) :source"
            type-key field-key))
        source))))

(defun compile-validations (model type-key field-key)
  (let ((vfs (resolve-hook-list
               (u:tree-get model type-key :fields
                 field-key :validations)
               :kind :validation
               :type-key type-key
               :field-key field-key)))
    (push #'v-type vfs)
    (when (u:tree-get model type-key :fields field-key :required)
      (push #'v-required vfs))
    (when (u:tree-get model type-key :fields field-key :ui :options)
      (push #'v-options vfs))
    vfs))

(defparameter *lifecycle-keys*
  '(:pre-create :post-create
     :pre-update :post-update
     :pre-delete :post-delete))

(defun compile-lifecycle-hooks (model type-key)
  "Resolve all lifecycle slots for TYPE-KEY into lists of functions. Returns a
plist of :key → function-list for each lifecycle slot that has a value in the
model."
  ;; Compile-time validation of :compose-string placeholders
  (loop for key in *lifecycle-keys*
    for raw = (getf (getf model type-key) key)
    when raw
    do (dolist (form (if (listp raw) raw (list raw)))
         (when (and (consp form) (eq (car form) :compose-string))
           (let ((fmt (getf (cdr form) :format))
                  (into (getf (cdr form) :into)))
             (valid-compose-placeholders type-key fmt into model)))))
  (loop for key in *lifecycle-keys*
    for raw = (getf (getf model type-key) key)
    when raw
    append (list key
             (if (listp raw)
               ;; A list of hook forms — resolve each
               (resolve-hook-list raw
                 :kind :lifecycle
                 :type-key type-key)
               ;; Single form (function, keyword, or plist)
               (resolve-hook-list (list raw)
                 :kind :lifecycle
                 :type-key type-key)))))

(defun write-to (model type-key field-key field-def)
  (let ((wt (getf field-def :write-to)))
    (when wt
      (unless (u:plistp wt)
        (error "The ~s ~s :write-to value must be a proper plist"
          type-key field-key))
      (unless (u:has (u:plist-keys wt) :table)
        (error "The ~s ~s :write-to value must include a :table key"
          type-key field-key))
      (let ((to-table (getf wt :table)))
        (unless (and
                  (u:tree-get model to-table)
                  (not (u:tree-get model to-table :internal)))
          (error "The ~s ~s :write-to :table value must be an existing type ~
                  and not :internal" type-key field-key))
        (unless (u:has (u:plist-values wt) :value)
          (error "The ~s ~s :write-to value must include a field with with ~
                :value as its value tag." type-key field-key))
        (loop for k in wt by #'cddr
          unless (or (equal k :table) (u:tree-get model to-table :fields k))
          do (error "The ~s ~s :write-to key ~s doesn't exist in :write-to ~
                     :table ~s" type-key field-key k to-table))
        (loop
          for k in wt by #'cddr
          for v in (cdr wt) by #'cddr
          unless (or (equal k :table) (u:has '(:this :user :value) v))
          do (error "The ~s ~s :write-to key ~s must have a value of ~
                     :this, :user, or :value. Unknown value ~s"
               type-key field-key k v))
        (loop with fields = (u:tree-get model to-table :fields)
          and wt-keys = (u:plist-keys wt)
          for k in fields by #'cddr
          for v in (cdr fields) by #'cddr
          when (and (getf v :identity) (not (u:has wt-keys k)))
          do (error "The ~s ~s :write-to value is missing required ~s ~
                     identity field ~s"
               type-key field-key to-table k))
        wt))))

(defun valid-target (model type-key field-key field-def)
  (let ((target (getf field-def :target)))
    (when target
      (unless (u:has (u:plist-keys model) target)
        (error "Unknown target ~s in type ~s, field ~s"
          target type-key field-key))
      (loop with fields = (u:tree-get model target :fields)
        for f-key in fields by #'cddr
        for f-def in (cdr fields) by #'cddr
        when (getf f-def :identity) count f-key into id-field-count
        finally
        (unless (= id-field-count 1)
          (error "Field ~s of type ~s targets type ~s, which has ~d identity ~
                fields. However, targets must have exactly 1 identity field."
            field-key type-key target id-field-count)))
      target)))

(defun humanize-field-key (field-key)
  "Convert a field key like :average-rating into a human label
like \"Average Rating\". Splits on dashes and underscores,
capitalizes each word, and joins with spaces."
  (let ((name (string-downcase (string field-key))))
    (with-output-to-string (out)
      (loop
        with capitalize-next = t
        for ch across name
        do
        (cond
          ((or (char= ch #\-) (char= ch #\_))
            (write-char #\Space out)
            (setq capitalize-next t))
          (capitalize-next
            (write-char (char-upcase ch) out)
            (setq capitalize-next nil))
          (t
            (write-char ch out)))))))

(defun valid-ui-keys (ui)
  "Check that no dead/rejected keys are present on the :ui plist. Signals via
report-e if any are found."
  (loop for bad-key in '(:render-as :input-type :form-control)
    when (u:has (u:plist-keys ui) bad-key)
    do (report-e "valid-ui-keys"
         "Rejected key ~s found on :ui plist. ~
          Use :widget instead."
         ~bad-key)))

(defun valid-widget-value (widget)
  "Check that WIDGET is a known widget keyword. Signals via report-e if not."
  (unless (member widget *widgets*)
    (report-e "valid-widget-value"
      "Unknown :widget ~s. Must be one of: ~{~a~^, ~}."
      ~widget *widgets*)))

(defun valid-read-only-value (ui)
  "Check that :read-only, if present, is t or nil. Signals via report-ve
otherwise."
  (let ((ro (getf ui :read-only :missing)))
    (unless (or (eq ro :missing) (eq ro t) (null ro))
      (report-ve "valid-read-only-value"
        ":read-only must be t or nil, got ~s."
        ~ro))))

(defun valid-options-value (ui)
  "Check :options on :ui: non-empty list of non-empty strings; only legal with
:widget :select."
  (let ((options (getf ui :options :missing)))
    (unless (eq options :missing)
      (unless (and (listp options)
                options
                (every (lambda (s)
                         (and (stringp s) (plusp (length s))))
                  options))
        (report-ve "valid-options-value"
          ":options must be a non-empty list of non-empty ~
           strings, got ~a"
          ~options))
      (let ((widget (getf ui :widget)))
        (unless (eq widget :select)
          (report-e "valid-options-value"
            ":options is only valid with :widget :select, got ~s"
            ~widget))))))

(defun finalize-ui (field-key ui)
  "Compile-time gate for field :ui plists. Validates keys and widget values, then
injects safe defaults:
  - :widget defaults to :textbox when missing
  - :label defaults to humanized field-key when missing
  - :read-only defaults to t on :image / :image-list when missing

Rejects:
  - Dead keys (:render-as, :input-type, :form-control)
  - Unknown widget values
  - :read-only with non-boolean values
  - :read-only nil on :image / :image-list (post-MVP)"
  ;; Reject dead keys before anything else
  (valid-ui-keys ui)
  ;; Validate :read-only value if present
  (valid-read-only-value ui)
  ;; Validate :options shape/widget if present
  (valid-options-value ui)
  ;; Validate widget if explicitly present
  (let ((widget (getf ui :widget)))
    (when widget
      (valid-widget-value widget)))
  ;; Inject defaults
  (let* ((with-widget
           (if (getf ui :widget)
             ui
             (add-to-plist ui (list :widget :textbox))))
          (with-label
            (if (getf with-widget :label)
              with-widget
              (add-to-plist with-widget
                (list :label (humanize-field-key field-key)))))
          (final-widget (getf with-label :widget)))
    ;; Image read-only rules (existing logic)
    (if (member final-widget '(:image :image-list))
      (let ((ro (getf with-label :read-only :missing)))
        (cond
          ((eq ro :missing)
            (add-to-plist with-label (list :read-only t)))
          ((null ro)
            (report-e "finalize-ui"
              "Widget ~a is display-only for MVP; ~
               :read-only nil is not allowed."
              ~final-widget))
          (t with-label)))
      with-label)))

(defun valid-join-table-agg (type-key field-key join-table source)
  ":private: M2M row-display :agg contract. On a field with :join-table set, the
row-display :source :agg must be :distinct: an omitted :agg is injected, any
other declared value signals report-e (set semantics; a flat-join view with
sibling chains duplicates :list values). :source-all is untouched (:list is
correct there). Returns the effective :source plist."
  (let ((agg (getf source :agg)))
    (cond
      ((and join-table source (null agg))
        (add-to-plist source (list :agg :distinct)))
      ((and join-table source (not (eq agg :distinct)))
        (report-e "valid-join-table-agg"
          "Join-table row-display :source :agg must be :distinct ~
           (or omitted); field ~s of type ~s has :agg ~s."
          ~field-key ~type-key ~agg))
      (t source))))

(defun compile-field (model type-key old-field-key new-field-key field-def)
  (loop
    with force-sql-name = (getf field-def :force-sql-name)
    with name-sql = (or
                      force-sql-name
                      (column-name model type-key new-field-key field-def))
    with target = (valid-target model type-key old-field-key field-def)
    with type-sql = (sql-type
                      type-key
                      new-field-key
                      (if target :uuid (getf field-def :type)))
    and column = (if target t
                   (if (equal (getf field-def :type) :button)
                     nil
                     (getf field-def :column)))
    and primary-key = (when (getf field-def :primary-key) "primary key")
    and not-null = (when (getf field-def :not-null) "not null")
    with references = (when target
                        (format nil "references ~a(id) on delete cascade"
                          (table-name target (built-in-p target model))))
    and unique = (when (getf field-def :unique) "unique")
    and default = (when (getf field-def :default)
                    (format nil "default ~a"
                      (value-sql
                        (getf field-def :default)
                        (getf field-def :type)
                        :quote t)))
    and default-value = (getf field-def :default
                          (unless (equal (getf field-def :type) :list) :null))
    and validations = (compile-validations model type-key new-field-key)
    and field-type = (getf field-def :type)
    and action = (getf field-def :action)
    with is-button = (and (equal field-type :button) action)
    with status-key = (when is-button
                        (u:make-keyword
                          (format nil "~a-status" new-field-key)))
    and compiled-hook = (when is-button
                          (progn
                            (when (eq (car action) :spawn)
                              (valid-spawn-params model type-key
                                new-field-key action))
                            (resolve-hook-form action
                              :kind :action
                              :type-key type-key
                              :field-key new-field-key)))
    with sql-parts = (remove-if-not #'identity
                       (list name-sql type-sql primary-key not-null
                         references unique default))
    with new-def = (let ((name-key (u:make-keyword name-sql)))
                     (list
                       :validations validations
                       :force-sql-name force-sql-name
                       :name-sql name-sql
                       :type-sql type-sql
                       :create-sql (format nil "~{~a~^ ~}" sql-parts)
                       :source (field-source model type-key new-field-key
                                 field-def name-key)
                       :source-all (getf field-def :source-all)
                       :write-to (write-to model type-key
                                   new-field-key field-def)
                       :type (or (getf field-def :type) :text)
                       :column column
                       :not-null (getf field-def :not-null)
                       :reference (when (equal old-field-key :reference) t)
                       :default default-value))
    with attrs = '(:base-field :ui :unique :primary-key :target :join-table
                    :autofill :identity :default-from :css-value :action
                    :compose :sortable :searchable)
    for attr in attrs
    append (list attr (getf field-def attr)) into def
    finally
    (return
      (let* ((ui-val (getf def :ui))
              (ui-options (getf ui-val :options :missing))
              (has-target (getf def :target))
              (has-join-table (getf def :join-table))
              (widget (getf ui-val :widget))
              (source (valid-join-table-agg type-key new-field-key
                        has-join-table (getf new-def :source))))
        ;; :options exclusive with relation sources
        (when (and (not (eq ui-options :missing))
                (or has-target has-join-table))
          (report-e "compile-field"
            ":options is mutually exclusive with :target / ~
             :join-table on field ~s of type ~s."
            ~new-field-key ~type-key))
        ;; :select requires a value source
        (when (and (eq widget :select)
                (eq ui-options :missing)
                (not has-target))
          (report-e "compile-field"
            ":widget :select requires either :options or :target ~
             on field ~s of type ~s."
            ~new-field-key ~type-key))
        ;; :sortable requires a base column, except on a rollup (plan 09): there
        ;; the ORDER BY targets the SELECT alias, so grain pass-throughs and
        ;; :sum / :count / :avg measures may sort.  :list / :distinct stay
        ;; unsortable (array compare is not a leaderboard). Hybrids (base types
        ;; with Phase B measures) keep the strict rule: no aggregate alias in
        ;; base Phase A.
        (when (getf def :sortable)
          (if (getf model type-key :rollup)
            (when (member (u:tree-get field-def :source :agg)
                    '(:list :distinct))
              (report-e "compile-field"
                ":sortable t is not valid on :list / :distinct ~
                 measures; field ~s of rollup ~s."
                ~new-field-key ~type-key))
            (unless column
              (report-e "compile-field"
                ":sortable t is only valid on base-column fields; ~
                 field ~s of type ~s has no :column t."
                ~new-field-key ~type-key))))
        ;; :searchable requires a base text column that is not an FK
        (when (getf def :searchable)
          (unless column
            (report-e "compile-field"
              ":searchable t is only valid on base-column fields; ~
               field ~s of type ~s has no :column t."
              ~new-field-key ~type-key))
          (unless (eq (or field-type :text) :text)
            (report-e "compile-field"
              ":searchable t is only valid on :type :text fields; ~
               field ~s of type ~s has :type ~s."
              ~new-field-key ~type-key ~field-type))
          (when has-target
            (report-e "compile-field"
              ":searchable t is not valid on :target (FK) fields; ~
               field ~s of type ~s."
              ~new-field-key ~type-key)))
        (let* ((final-ui (when ui-val
                           (finalize-ui new-field-key ui-val)))
                (final-def (if final-ui
                             (add-to-plist def (list :ui final-ui))
                             def)))
          (append final-def
            (add-to-plist new-def (list :source source))
            (when is-button
              (list
                :compiled-hook compiled-hook
                :status-field status-key))))))))

(defun resolve-scope-alias (model type-key view-key table-key scope)
  "Resolve a :scope keyword on a field source to the alias key that the view
result uses for the scoped column. Currently only :scope :user is
supported. When :scope :user is specified, the function looks up the :user field
on TABLE-KEY in the view's aliases and returns its alias-key. Returns NIL when
SCOPE is NIL."
  (when scope
    (case scope
      (:user
        (let* ((user-alias (u:tree-get model type-key :views view-key
                             :aliases table-key :user)))
          (unless user-alias
            (error "Field-level :scope :user on ~s requires that ~
                    table ~s has a :user field in view ~s"
              type-key table-key view-key))
          (u:make-keyword user-alias)))
      (t (error "Unsupported field-level scope ~s on ~s" scope type-key)))))

(defun compile-field-stage-2 (model type-key field-def)
  (cond
    ((u:tree-get field-def :source :view)
      (let* ((table-key (or (u:tree-get field-def :source :table) type-key))
              (column-key (u:tree-get field-def :source :column))
              (view-key (u:tree-get field-def :source :view))
              (scope (u:tree-get field-def :source :scope))
              (alias (u:tree-get model type-key :views view-key :aliases table-key column-key))
              (column (u:tree-get model type-key :views view-key :columns table-key column-key))
              (scope-alias (resolve-scope-alias
                             model type-key view-key table-key scope)))
        (add-to-plist
          field-def
          (list
            :source (add-to-plist
                      (u:tree-get field-def :source)
                      (list
                        :alias-key (u:make-keyword alias)
                        :column-name column
                        :scope-alias scope-alias
                        :scope-kind scope))))))
    ((and
       (u:tree-get field-def :source-sel :view)
       (u:tree-get field-def :source-all :view))
      (let* ((sel-table-key (u:tree-get field-def :source-sel :table))
              (all-table-key (u:tree-get field-def :source-all :table))
              (sel-column-key (u:tree-get field-def :source-sel :column))
              (all-column-key (u:tree-get field-def :source-all :column))
              (sel-view-key (u:tree-get field-def :source-sel :view))
              (all-view-key (u:tree-get field-def :source-all :view))
              (sel-alias (u:tree-get model type-key :views sel-view-key
                           :aliases sel-table-key sel-column-key))
              (all-alias (u:tree-get model type-key :views all-view-key
                           :aliases all-table-key all-column-key)))
        (add-to-plist
          field-def
          (list
            :source-sel (add-to-plist
                          (u:tree-get field-def :source-sel)
                          (list :alias-key (u:make-keyword sel-alias)))
            :source-all (add-to-plist
                          (u:tree-get field-def :source-all)
                          (list :alias-key (u:make-keyword all-alias)))))))
    (t (copy-seq field-def))))

(defun compile-fields-stage-2 (model type-key)
  (loop with fields = (u:tree-get model type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    appending
    (list
      field-key
      (compile-field-stage-2 model type-key field-def))))

(defun collect-searchable-fields (fields)
  ":private: Returns the list of table-qualified column names for fields marked
:searchable t. FIELDS is the post-stage-2 field plist

  (where :source :column-name is present)

Returns nil when the type has no searchable fields."
  (loop
    for field-def in (cdr fields) by #'cddr
    for column-name = (u:tree-get field-def :source :column-name)
    when (and (getf field-def :searchable) column-name)
    collect column-name))

(defun default-fields (&key model type-key keys-only)
  (let* ((internal (or (u:tree-get model type-key :internal)
                     (u:tree-get model type-key :is-joiner)))
          (base (u:tree-get model type-key :base))
          (target-resources (when
                              (or
                                (not model)
                                (not type-key)
                                keys-only
                                (and (not internal) (not base)))
                              t))
          (generate-uuid (or internal base))
          (fields `(:id
                     (:type :uuid
                       :source (:view :main :column :id :agg :first)
                       :column t
                       :primary-key t
                       :update nil
                       :target ,(when target-resources :resources)
                       :base-field t
                       :default ,(when generate-uuid :generate-uuid))
                     :created-at
                     (:type :timestamp
                       :source (:view :main :column :created-at :agg :first)
                       :column t
                       :update nil
                       :not-null t
                       :default :now
                       :base-field t)
                     :updated-at
                     (:type :timestamp
                       :source (:view :main :column :updated-at :agg :first)
                       :column t
                       :update nil
                       :not-null t
                       :default :now
                       :base-field t))))
    (if keys-only
      (u:plist-keys fields)
      fields)))

(defun add-default-fields (type-key model)
  (append
    (default-fields :model model :type-key type-key)
    (u:deep-copy (u:tree-get model type-key :fields))))

(defun synthesize-status-fields (type-key fields)
  "For each :button field, inject a companion :<field>-status field if it does
not already exist.  Returns the augmented fields plist."
  (loop
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for field-type = (getf field-def :type)
    when (equal field-type :button)
    collect field-key into button-keys
    finally
    (return
      (loop
        for button-key in button-keys
        for status-key = (u:make-keyword (format nil "~a-status" button-key))
        for button-label = (or (u:tree-get fields button-key :ui :label)
                             (format nil "~@(~a~)" button-key))
        for status-label = (format nil "~a Status" button-label)
        when (getf fields status-key)
        do (report-e "synthesize-status-fields"
             "Field ~s on type ~s conflicts with auto-generated status field ~
              for button ~s"
             ~status-key ~type-key ~button-key)
        appending
        `(,status-key
           (:type :text
             :column t
             :ui (:label ,status-label :widget :textbox :read-only t)
             :source (:view :main :column ,status-key :agg :first)
             :default "idle"
             :not-null t))
        into status-fields
        finally (return (append fields status-fields))))))

(defun ensure-action-on-buttons (type-key fields)
  "Ensure :action only appears on :button fields.  Signals an error if :action
is present on a non-button field."
  (loop for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for field-type = (getf field-def :type)
    when (and (getf field-def :action)
           (not (equal field-type :button)))
    do (report-e "ensure-action-on-buttons"
         ":action is only valid on :button fields, but ~
                      field ~s on type ~s has type ~s"
         ~field-key ~type-key ~field-type)))

(defun compile-fields (type-key model)
  (loop with fields = (add-default-fields type-key model)
    for old-field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for new-field-key = (if (equal old-field-key :reference)
                          (u:make-keyword
                            (table-reference (getf field-def :target)))
                          old-field-key)
    appending
    (list
      new-field-key
      (compile-field model type-key old-field-key new-field-key field-def))))

(defun view-aliases (model view-def)
  (loop for table-key in (u:tree-get view-def :tables)
    unless (u:tree-get model table-key :is-joiner)
    append
    (list table-key
      (loop for column in (table-columns model table-key)
        append (list (getf column :field-key) (getf column :alias-key))))))

(defun view-columns (model view-def)
  (loop for table-key in (u:tree-get view-def :tables)
    unless (u:tree-get model table-key :is-joiner)
    append
    (list table-key
      (loop for column in (table-columns model table-key)
        append (list (getf column :field-key) (getf column :column))))))

(defun valid-view-scope (type-key view-scope)
  (when view-scope
    (let ((scope-types '(:user))
           (scope-details '(:table :field)))
      (unless (or (keywordp view-scope) (u:plistp view-scope))
        (error "View :scope for ~s must be a plist, got ~s" type-key view-scope))
      (unless (or
                (keywordp view-scope)
                (u:has scope-types (u:plist-keys view-scope)))
        (error "View :scope for ~s has an invalid key: ~s"
          type-key (u:plist-keys view-scope)))
      (when (u:plistp view-scope)
        (loop for scope-type in scope-types
          for scope-def = (getf view-scope scope-type)
          unless (or
                   (keywordp scope-def)
                   (null scope-def)
                   (u:has (u:plist-keys scope-def) scope-details))
          do (error "View :scope for ~s ~s has an invalid key: ~a"
               type-key scope-type (u:plist-keys scope-def))))
      view-scope)))

(defun enrich-views (model type-key)
  (loop
    with type-def = (getf model type-key)
    with views = (let ((v (getf type-def :views))
                        (j (getf type-def :is-joiner)))
                   (if v
                     (u:deep-copy v)
                     (if j nil `(:main (:tables (,type-key))))))
    for view-key in views by #'cddr
    for view-def in (cdr views) by #'cddr
    for scope-def = (getf view-def :scope)
    ;; Rollup (measure shape): keep tables / scope / aliases / columns, but no
    ;; flat-generator SQL (the strings would name a table that does not exist).
    ;; Measure SQL parts are 08b.
    for measure = (eq (getf type-def :phase-a-shape) :measure)
    for table-name = (u:tree-get model type-key :table-name)
    for view-def-new = (append
                         (list
                           :tables (getf view-def :tables)
                           :scope (valid-view-scope type-key scope-def)
                           :sql (unless measure
                                  (view-sql model view-def))
                           :phase-a-base-sql
                           (unless measure
                             (format nil "select ~a.id from ~a"
                               table-name table-name))
                           :phase-a-join-sql
                           (unless measure
                             (phase-a-join-sql model view-def))
                           :aliases (view-aliases model view-def)
                           :columns (view-columns model view-def))
                         ;; 08b: measure Phase A SQL parts, spliced flat
                         ;; onto the view (empty on non-measure shapes).
                         (when measure
                           (measure-phase-a-parts model type-key)))
    appending (list view-key view-def-new)))

;;
;; BEGIN Measure Phase A SQL (08b)
;;
;; The second Phase A shape: GROUP BY grain with SQL aggregates and FILTER
;; clauses, replacing flat SELECT + LEFT JOIN + Lisp collapse.  The compiler
;; emits parts, not one sealed string: a query that already ends in GROUP BY
;; cannot take a runtime WHERE. be-list splices runtime predicates (scope,
;; request-time grain filters, compiled grain WHERE) between the SELECT part and
;; the GROUP BY part.
;;

(defun measure-qualified-column (model type-key field-key)
  "Fully qualified <table>.<column> for FIELD-KEY on TYPE-KEY, using the type's
compiled :table-name and the field's :name-sql. ID on the grain is <grain>.id."
  (format nil "~a.~a"
    (u:tree-get model type-key :table-name)
    (u:tree-get model type-key :fields field-key :name-sql)))

(defun aggregate-sql-expression (agg qualified-column filter-sql)
  "Raw SQL aggregate expression for AGG over QUALIFIED-COLUMN, wrapped with
FILTER (WHERE ...) when FILTER-SQL is non-empty. No COALESCE, no GROUP BY
assumption — the rollup SELECT wraps display coalescing and the future hybrid
ORDER BY (Post MVP) uses the raw form. Never emits COUNT(DISTINCT ...)."
  (let ((expr (case agg
                (:sum (format nil "sum(~a)" qualified-column))
                (:count (format nil "count(~a)" qualified-column))
                (:avg (format nil "avg(~a)" qualified-column))
                (:list (format nil "array_agg(~a)" qualified-column))
                (:distinct (format nil "array_agg(distinct ~a)"
                             qualified-column))
                (t (report-e "aggregate-sql-expression"
                     "Unknown :agg ~s." ~agg)))))
    (if filter-sql
      (format nil "~a filter (where ~a)" expr filter-sql)
      expr)))

(defun measure-field-sql (model field-key field-def fact-filter-sql)
  "SELECT expression for one rollup field. Pass-throughs (:agg :first, including
the injected :id) select the grain column and alias it as the field key (SQL
identifiers are the field keys, so the row comes back as (:id ...) (:name ...),
never :grain-id). Real measures wrap the mapper expression in COALESCE for
:sum / :list / :distinct; :count is naturally 0 and :avg stays NULL (Issue 13)."
  (let* ((source (getf field-def :source))
          (table (getf source :table))
          (column (getf source :column))
          (agg (getf source :agg))
          (alias (to-sql-identifier field-key)))
    (if (eq agg :first)
      (format nil "~a as ~a"
        (measure-qualified-column model table column)
        alias)
      (let ((qualified (measure-qualified-column model table column)))
        (case agg
          (:count
            (format nil "~a as ~a"
              (aggregate-sql-expression agg qualified fact-filter-sql)
              alias))
          (:avg
            (format nil "~a as ~a"
              (aggregate-sql-expression agg qualified fact-filter-sql)
              alias))
          (:sum
            (format nil "coalesce(~a, 0) as ~a"
              (aggregate-sql-expression agg qualified fact-filter-sql)
              alias))
          ((:list :distinct)
            (let* ((field-type (getf field-def :type))
                    (pg-type (u:tree-get *field-types* field-type :sql))
                    (empty (format nil "array[]::~a[]" pg-type)))
              (format nil "coalesce(~a, ~a) as ~a"
                (aggregate-sql-expression agg qualified fact-filter-sql)
                empty
                alias)))
          (t (report-e "measure-field-sql"
               "Unknown :agg ~s on rollup field ~s." ~agg ~field-key)))))))

(defun measure-model-filter-sql (model type-key clause)
  "One compile-time-closed SQL predicate for a model-declared :filter clause
(07c). Discrete :eq / :ne embed the validated literal (booleans as bare column /
NOT for :eq t / :eq nil); :last-days and :calendar embed PostgreSQL clock
functions so a model compiled Monday still filters against 'now' on
Friday. Value is never user input, so embedding is safe."
  (destructuring-bind (table column op value) clause
    (declare (ignore type-key))
    (let ((qualified (measure-qualified-column model table column))
           (col-type (rollup-column-type model table column)))
      (case op
        (:eq
          (cond
            ((eq col-type :boolean)
              (if value
                (format nil "~a" qualified)
                (format nil "not ~a" qualified)))
            ((member col-type '(:text :password :uuid))
              (format nil "~a = '~a'" qualified value))
            (t (format nil "~a = ~a" qualified value))))
        (:ne
          (cond
            ((eq col-type :boolean)
              (if value
                (format nil "not ~a" qualified)
                (format nil "~a" qualified)))
            ((member col-type '(:text :password :uuid))
              (format nil "~a != '~a'" qualified value))
            (t (format nil "~a != ~a" qualified value))))
        (:last-days
          (format nil "~a >= now() - interval '~a days'"
            qualified value))
        (:calendar
          ;; :month is the sole MVP unit (07c).
          (format nil
            "~a >= date_trunc('month', now()) and ~a < date_trunc('month', now()) + interval '1 month'"
            qualified qualified))))))

(defun measure-filters-sql (model type-key filter)
  "SQL predicates for every model-declared :filter clause on TYPE-KEY. Returns
two values: grain-table fragments (runtime WHERE) and joined-table
fragments (FILTER conditions ANDed into every real measure)."
  (loop with grain = (u:tree-get model type-key :grain)
    for clause in filter
    for table = (first clause)
    if (eq table grain)
    collect (measure-model-filter-sql model type-key clause)
    into grain-frags
    else
    collect (measure-model-filter-sql model type-key clause)
    into joined-frags
    finally (return (values grain-frags joined-frags))))

(defun measure-pk-guard-sql (model fact-table)
  "The shared per-table predicate attached to every real measure from the fact
table F: <F>.id IS NOT NULL. This is what makes unfiltered array_agg empty (not
{NULL}) on a LEFT JOIN miss and keeps SUM / COUNT / AVG honest. Not per-measure:
all measures from F share one FILTER base."
  (format nil "~a.id is not null"
    (u:tree-get model fact-table :table-name)))

(defun measure-fact-filter-sql (model fact-table joined-frags)
  "The FILTER condition for every real measure: the PK guard ANDed with every
model-declared joined-table (path) clause — downstream binding (Issue 17). The
guard is always present; clauses AND onto it."
  (let ((base (measure-pk-guard-sql model fact-table)))
    (if joined-frags
      (format nil "~a~{ and ~a~}" base joined-frags)
      base)))

(defun measure-join-sql (model view-tables)
  "LEFT JOIN clauses for the rollup view, same walk as VIEW-SQL /
ordered-xrefs. The grain is first (validated in 08a) and is never re-joined."
  (loop
    with joined-tables = (list (car view-tables))
    for xref in (ordered-xrefs model view-tables)
    for source = (getf xref :source)
    for target = (getf xref :target)
    for source-field = (getf xref :source-field)
    for reversed = (xref-reversed source target joined-tables)
    for join-table = (if reversed
                       (u:tree-get model target :table-name)
                       (u:tree-get model source :table-name))
    for join-field = (if reversed
                       "id"
                       (u:tree-get model source :fields source-field
                         :name-sql))
    for target-table = (if reversed
                         (u:tree-get model source :table-name)
                         (u:tree-get model target :table-name))
    for target-field = (if reversed
                         (u:tree-get model source :fields source-field
                           :name-sql)
                         "id")
    collect (format nil "left join ~a on ~a.~a = ~a.~a"
              join-table join-table join-field target-table target-field)
    into joins
    do (push (if reversed target source) joined-tables)
    finally (return joins)))

(defun measure-phase-a-parts (model type-key)
  "Measure Phase A SQL parts for a rollup TYPE-KEY (08b Step 2). Returns a plist
of compile-time-closed fragments stored on :VIEWS :MAIN by ENRICH-VIEWS:

  :MEASURE-PHASE-A-SELECT
      SELECT ... FROM <grain> LEFT JOIN ...
      (no WHERE, no GROUP BY)

  :MEASURE-PHASE-A-GROUP-BY
      GROUP BY <grain>.id, <pass-through cols>

  :MEASURE-PHASE-A-COUNT-SELECT
      SELECT COUNT(*) FROM <grain_table>
      (no WHERE, no join)

  :MEASURE-PHASE-A-GRAIN-WHERE
      list of SQL fragments for
      model-declared grain filters

The generator assumes 08a guarantees: grain first in :tables with a real table,
single fact table F last, every real measure reads from F."
  (let* ((type-def (getf model type-key))
          (grain (getf type-def :grain))
          (view-tables (u:tree-get type-def :views :main :tables))
          (filter (getf type-def :filter))
          (fields (getf type-def :fields))
          (grain-table (u:tree-get model grain :table-name))
          (fact-table (car (last view-tables))))
    (multiple-value-bind (grain-frags joined-frags)
      (measure-filters-sql model type-key filter)
      (let* ((fact-filter (measure-fact-filter-sql model fact-table
                            joined-frags))
              (select-exprs
                (loop for field-key in fields by #'cddr
                  for field-def in (cdr fields) by #'cddr
                  collect (measure-field-sql model field-key field-def
                            fact-filter)))
              (group-cols
                (cons (format nil "~a.id" grain-table)
                  (loop for field-key in fields by #'cddr
                    for field-def in (cdr fields) by #'cddr
                    for table = (u:tree-get field-def :source :table)
                    for column = (u:tree-get field-def :source :column)
                    when (and (eq (u:tree-get field-def :source :agg) :first)
                           (not (eq column :id)))
                    collect (measure-qualified-column model table column))))
              (joins (measure-join-sql model view-tables))
              (select-sql
                (format nil "select~%  ~{~a~^,~%  ~}~%from ~{~a~^~%  ~}"
                  select-exprs (cons grain-table joins))))
        (list
          :measure-phase-a-select select-sql
          :measure-phase-a-group-by
          (format nil "group by ~{~a~^, ~}" group-cols)
          :measure-phase-a-count-select
          (format nil "select count(*) from ~a" grain-table)
          :measure-phase-a-grain-where grain-frags)))))

;;
;; END Measure Phase A SQL (08b)
;;

(defun type-has-roles (type-def)
  (unless (getf type-def :internal) t))

(defun compile-create (fn)
  (cond
    ((equal fn :auto) :auto)
    ((functionp fn) fn)
    ((null fn) nil)
    (t (error "Invalid :create function definition: ~a" fn))))

(defun validate-tree (model type-key tree is-leaf parent-type fs-backed)
  ;; Ensure that if :is-leaf or :fs-backed is true, then :tree is true and
  ;; :parent-type is set to a non-base type in the model.
  (when tree
    (unless parent-type
      (error "~(~s~) :parent-type must be set because :tree is T" type-key))
    (when (loop
            with types = (remove-if #'base-type-p (u:plist-keys model))
            for type-key in types
            never (equal parent-type type-key))
      (error "~(~s~) :parent-type ~(~s~) must be an existing model key"
        type-key parent-type)))
  (when (and (not tree) (or is-leaf parent-type fs-backed))
    (error "~(~s~) :is-leaf, :parent-type, and :fs-backed require :tree t"
      type-key)))

(defun mark-path-field (type-key fs-backed fields)
  ":private: Adds :path t to one of the fields if the type is fs-backed."
  (if fs-backed
    (let* (;; Determine if there's a field marked :path
            (pf-1 (loop for field-key in fields by #'cddr
                    for field-def in (cdr fields) by #'cddr
                    for path = (getf field-def :path)
                    when path collect field-key into path-fields
                    finally
                    (if (> (length path-fields) 1)
                      (error "Multiple :path fields in ~(~s~): ~s" type-key path-fields)
                      (return (car path-fields)))))
            ;; Otherwise, find the :name field with field type :text
            (pf-2 (if pf-1 pf-1
                    (loop for field-key in fields by #'cddr
                      for field-def in (cdr fields) by #'cddr
                      for field-type = (getf field-def :type)
                      when (and (equal field-key :name)
                             (equal field-type :text))
                      do (return field-key))))
            ;; Otherwise, determine if the count of non-base fields
            ;; is exactly 1...
            (user-fields (loop for field-key in (u:plist-keys fields)
                           unless (member field-key
                                    (default-fields :keys-only t))
                           collect field-key))
            ;; and, if so, check that the single field is of type :text
            (pf-3 (if pf-2 pf-2
                    (when (and (= (length user-fields) 1)
                            (equal (u:tree-get fields (car user-fields) :type)
                              :text))
                      (car user-fields)))))
      (unless pf-3
        (error "~(~s~) For :fs-backed types, at least one field must be :path t"
          type-key))
      (loop for field-key in fields by #'cddr
        for field-def in (cdr fields) by #'cddr
        append (list field-key
                 (if (equal field-key pf-3)
                   (add-to-plist field-def (list :path t))
                   field-def))))
    fields))

(defun augment-update-form (type-def fields)
  "Ensure :update-form :fields includes status keys for any button fields it
lists. When a model designer adds a button field with an :action attribute, the
compiler automatically adds a status field that's associated with that button
field. This status field is necessary to display the status of the actions taken
when the button is pressed. The model designer does not need to think about that
field. Similarly, this function adds the field to the list of fields the model
designer provided to display in the update form.  If :update-form :fields is
t (all fields) or absent, no augmentation is needed. Returns the type-def with
updated form."
  (let ((uf-fields (u:tree-get type-def :update-form :fields)))
    (if (or (null uf-fields) (eq uf-fields t))
      type-def
      ;; Explicit field list - append missing status keys
      (let ((button-keys (loop
                           for field-key in fields by #'cddr
                           for field-def in (cdr fields) by #'cddr
                           for field-type = (getf field-def :type)
                           when (equal field-type :button)
                           collect field-key))
             (current-keys (if (listp uf-fields) uf-fields (list uf-fields))))
        (if (null button-keys)
          type-def
          (let ((missing (loop
                           for button-key in button-keys
                           for status-key = (u:make-keyword
                                              (format nil "~a-status"
                                                button-key))
                           when (and (member button-key current-keys)
                                  (not (member status-key current-keys)))
                           collect status-key)))
            (if (null missing)
              type-def
              (let ((new-fields (append current-keys missing)))
                (add-to-plist type-def
                  (list :update-form
                    (add-to-plist
                      (getf type-def :update-form)
                      (list :fields new-fields))))))))))))

(defun valid-category (type-key category)
  ":private: Validates a declared :category value. Returns CATEGORY if valid;
signals via report-e if not."
  (unless (member category '(:settings :system :user))
    (report-e "valid-category"
      "Type ~s has invalid :category ~s. Must be one of: :settings, :system, :user."
      ~type-key ~category))
  category)

(defun compute-category (type-def)
  ":private: Derives :category from type flags when not explicitly declared.
Mirrors the previous runtime type-category logic."
  (let ((category (getf type-def :category)))
    (valid-category
      type-def
      (cond
        (category category)
        ((getf type-def :user-setting) :settings)
        ((getf type-def :built-in) :system)
        (t :user)))))

(defun valid-form-fields (type-key type-def)
  (loop
    for form-field in '(:list-form :add-form :update-form)
    for field-keys = (u:tree-get type-def form-field :fields)
    when (and field-keys (not (equal field-keys t)))
    do (loop for field-key in field-keys
         unless (u:tree-get type-def :fields field-key)
         do (report-e "valid-form-fields"
              "In type ~s, ~s contains an unknown field ~s"
              ~type-key ~form-field ~field-key))))

(defun valid-default-sort (type-key type-def)
  "Validate a type-level :default-sort declaration against the compiled TYPE-DEF
(fields are compiled by this point). Skips when no declaration is present. Shape
is (:field-key) or (:field-key :asc|:desc), the request-sort convention: the
direction is optional (defaults :asc at SQL-build time), so a direction-less
declaration is stored verbatim, never normalized. The field must exist and carry
:sortable t, the same rule the request path enforces (phase-a-order-by-column /
valid-measure-sort)."
  (let ((sort (getf type-def :default-sort)))
    (when sort
      (let ((field-key (first sort))
             (direction (second sort)))
        ;; Shape: a proper list of one or two elements, first a keyword. The
        ;; consp walk also rejects dotted tails ((cdr sort) is a non-nil atom
        ;; there) and non-lists.
        (unless (and (consp sort)
                  (keywordp field-key)
                  (or (null (cdr sort))
                    (and (consp (cdr sort))
                      (null (cddr sort)))))
          (report-e "valid-default-sort"
            ":default-sort on type ~s must be (:field :asc|:desc), ~
             got ~s."
            ~type-key ~sort))
        (when (and direction (not (member direction '(:asc :desc))))
          (report-e "valid-default-sort"
            ":default-sort direction on type ~s must be :asc or ~
             :desc, got ~s."
            ~type-key ~sort))
        (let ((field-def (u:tree-get type-def :fields field-key)))
          (cond
            ((null field-def)
              (report-e "valid-default-sort"
                "In type ~s, :default-sort names an unknown field ~s."
                ~type-key ~field-key))
            ((not (getf field-def :sortable))
              (report-e "valid-default-sort"
                "In type ~s, :default-sort field ~s is not :sortable t."
                ~type-key ~field-key))))))))

(defun expand-compose-hooks (type-key type-def)
  "Scan TYPE-DEF's fields for :compose and synthesize :compose-string lifecycle
hook forms. Returns a plist of :pre-create / :pre-update hook lists to append
after author-declared hooks.

Validates:
- Template is a non-empty string
- Placeholders name existing fields on this type
- No self-reference (placeholder matching the composed field)
- No duplicate :compose-string into the same field via manual hooks"
  (let ((fields (getf type-def :fields))
         (compose-forms nil))
    (loop for field-key in fields by #'cddr
      for field-def in (cdr fields) by #'cddr
      for template = (getf field-def :compose)
      when template
      do (let ((tpl (if (stringp template) template
                      (report-e "expand-compose-hooks"
                        ":compose on field ~s of type ~s must be a string."
                        ~field-key ~type-key))))
           (when (string= tpl "")
             (report-e "expand-compose-hooks"
               ":compose on field ~s of type ~s must be non-empty."
               ~field-key ~type-key))
           ;; Validate placeholders
           (let ((placeholders (compose-string-placeholders tpl)))
             (dolist (ph placeholders)
               (unless (u:has (u:plist-keys fields) ph)
                 (report-e "expand-compose-hooks"
                   "Unknown placeholder ~s in :compose on field ~s of type ~s."
                   ~ph ~field-key ~type-key)))
             ;; Reject self-reference
             (when (member field-key placeholders)
               (report-e "expand-compose-hooks"
                 "Self-reference in :compose on field ~s of type ~s."
                 ~field-key ~type-key)))
           ;; Check for duplicate manual :compose-string into same field
           (dolist (lk '(:pre-create :pre-update))
             (let ((raw (getf type-def lk)))
               (when raw
                 (dolist (form (if (listp raw) raw (list raw)))
                   (when (and (consp form)
                           (eq (car form) :compose-string)
                           (eq (getf (cdr form) :into) field-key))
                     (report-e "expand-compose-hooks"
                       "Duplicate :compose-string into ~s on type ~s: ~
                            field :compose and manual hook both target it."
                       ~field-key ~type-key))))))
           ;; Synthesize the hook form
           (push (list :compose-string
                   :format tpl :into field-key)
             compose-forms)))
    ;; Return plist of hook lists to append
    (when compose-forms
      (let ((forms (nreverse compose-forms)))
        (list :pre-create forms :pre-update forms)))))

;;
;; BEGIN Rollup types (08a compile surface)
;;
;; A :rollup t type is a read-only analytical type: no physical table,
;; no DDL/DML, one grain, one fact table. The compiler stores :phase-a-shape
;; :measure here. The measure SQL generator and the be-list measure branch are
;; 08b; until 08b lands a compiled rollup is fatal if anyone calls be-list. All
;; contract violations below signal report-e at compile time.
;;

(defparameter *model-filter-operators* '(:eq :ne :last-days :calendar)
  "Closed operator set for model-declared :filter clauses (07c). Separate from
the request-time operator table; :like / :in never enter this path.")

(defparameter *rollup-forbidden-field-keys*
  '(:action :validation :validations :compose :compose-string
     :autofill :source-all :write-to :join-table :target)
  "Field attributes that never appear on a rollup field (07b matrix). :button
arrives as a :type value and is checked separately, as are :identity t and
:column t.")

(defun rollup-column-type (model table-key column-key)
  "Effective :type of COLUMN-KEY on TABLE-KEY in the raw model, covering
injected default fields (:id :uuid, timestamps) and the omitted-:type default of
:text."
  (or
    (u:tree-get model table-key :fields column-key :type)
    (u:tree-get (default-fields) column-key :type)
    :text))

(defun valid-non-rollup-keys (model type-key)
  "Reject rollup-only keys on a non-rollup type (07b matrix)."
  (let ((type-def (getf model type-key)))
    (when (u:has (u:plist-keys type-def) :grain)
      (report-e "valid-non-rollup-keys"
        ":grain is rollup-only; type ~s is not a rollup."
        ~type-key))
    (when (u:has (u:plist-keys type-def) :filter)
      (report-e "valid-non-rollup-keys"
        "Model-declared :filter is rollup-only; type ~s is not a ~
         rollup."
        ~type-key))
    ;; :table nil is a rejected :rollup synonym, not an alias
    ;; (07e freeze checklist).
    (when (and (u:has (u:plist-keys type-def) :table)
            (null (getf type-def :table)))
      (report-e "valid-non-rollup-keys"
        ":table nil on type ~s is a compile error, not a rollup ~
         alias; :rollup t is the sole declaration."
        ~type-key))))

(defun valid-rollup-matrix (model type-key type-def grain views)
  "Type-level incompatible-key matrix for :rollup t (07b), including the
grain / :tables contract and the :scope :user compile check. Signals report-e on
the first violation."
  (when (u:has (u:plist-keys type-def) :table)
    (report-e "valid-rollup-matrix"
      "Rollup type ~s must not declare :table; a rollup has no ~
       physical table."
      ~type-key))
  (when (u:has (u:plist-keys type-def) :type)
    (report-e "valid-rollup-matrix"
      "Type ~s has a type-level :type key. Types have no :type; ~
       :rollup t is the sole rollup declaration."
      ~type-key))
  (dolist (key '(:tree :fs-backed :is-leaf :parent-type
                  :user-setting :is-joiner :built-in))
    (when (getf type-def key)
      (report-e "valid-rollup-matrix"
        "Key ~s is not compatible with :rollup t on type ~s."
        ~key ~type-key)))
  (dolist (key '(:create :update :delete))
    (when (getf type-def key)
      (report-e "valid-rollup-matrix"
        "Rollup type ~s is read-only; ~s must be absent or nil."
        ~type-key ~key)))
  (dolist (key *lifecycle-keys*)
    (when (getf type-def key)
      (report-e "valid-rollup-matrix"
        "Lifecycle slot ~s is not allowed on rollup type ~s."
        ~key ~type-key)))
  (when (getf type-def :write-to)
    (report-e "valid-rollup-matrix"
      ":write-to is not allowed on rollup type ~s."
      ~type-key))
  (when (and (u:has (u:plist-keys type-def) :suppress-roles)
          (null (getf type-def :suppress-roles)))
    (report-e "valid-rollup-matrix"
      "Author :suppress-roles nil on rollup ~s; the compiler ~
       auto-sets :suppress-roles t."
      ~type-key))
  (dolist (key '(:add-form :update-form))
    (when (u:has (u:plist-keys type-def) key)
      (report-e "valid-rollup-matrix"
        "~s is not allowed on rollup type ~s (even nil); only ~
         :list-form is legal."
        ~key ~type-key)))
  (let ((list-form (getf type-def :list-form :missing)))
    (when (or (eq list-form :missing) (null list-form))
      (report-e "valid-rollup-matrix"
        "Rollup type ~s requires :list-form; missing or nil is a ~
         compile error."
        ~type-key))
    (when (equal (getf list-form :fields) nil)
      (report-e "valid-rollup-matrix"
        "Rollup type ~s :list-form (:fields nil) is a compile ~
         error; use (:fields t) or a non-empty field list."
        ~type-key)))
  (let ((view-keys (u:plist-keys views)))
    (unless (and (= (length view-keys) 1) (eq (first view-keys) :main))
      (report-e "valid-rollup-matrix"
        "Rollup type ~s must declare exactly one view (:main); ~
         extra views are not allowed."
        ~type-key)))
  (unless grain
    (report-e "valid-rollup-matrix"
      "Rollup type ~s is missing :grain."
      ~type-key))
  (let ((grain-def (getf model grain)))
    (unless grain-def
      (report-e "valid-rollup-matrix"
        "Rollup type ~s has unknown :grain ~s."
        ~type-key ~grain))
    (unless (getf grain-def :table)
      (report-e "valid-rollup-matrix"
        "Rollup grain ~s of ~s must be a type with a physical ~
         table; a rollup cannot be another rollup's grain."
        ~grain ~type-key)))
  (let ((view-tables (u:tree-get views :main :tables)))
    (cond
      ((null view-tables)
        (report-e "valid-rollup-matrix"
          "Rollup type ~s :views :main :tables is missing or ~
           empty."
          ~type-key))
      ((not (member grain view-tables))
        (report-e "valid-rollup-matrix"
          "Rollup grain ~s of ~s must appear in :views :main ~
           :tables."
          ~grain ~type-key))
      ((not (eq (first view-tables) grain))
        (report-e "valid-rollup-matrix"
          "Rollup grain ~s of ~s must be first in :views :main ~
           :tables; no silent reorder."
          ~grain ~type-key))))
  ;; :scope :user requires :users grain or a grain :user field (same requirement
  ;; as field-level :scope :user). Runtime binding is 08b Step 5; the compile
  ;; error lives here.
  (let ((scope (u:tree-get views :main :scope)))
    (when (or (eq scope :user)
            (and (u:plistp scope) (getf scope :user)))
      (unless (or (eq grain :users)
                (u:tree-get model grain :fields :user))
        (report-e "valid-rollup-matrix"
          ":scope :user on rollup ~s requires grain :users or a ~
           grain type with a :user field; grain ~s has neither."
          ~type-key ~grain)))))

(defun valid-rollup-fields (type-key raw-fields view-tables)
  "Field-level rollup matrix (07b): author :id, forbidden attributes, :button,
missing :agg, and :source :table membership in :tables."
  (when (u:has (u:plist-keys raw-fields) :id)
    (report-e "valid-rollup-fields"
      "Rollup type ~s must not declare :id; the compiler injects ~
       it from the grain PK."
      ~type-key))
  (loop
    for field-key in raw-fields by #'cddr
    for field-def in (cdr raw-fields) by #'cddr
    for source = (getf field-def :source)
    for source-table = (getf source :table)
    do
    (dolist (key *rollup-forbidden-field-keys*)
      (when (getf field-def key)
        (report-e "valid-rollup-fields"
          "Field attribute ~s is not allowed on rollup field ~s ~
               of ~s."
          ~key ~field-key ~type-key)))
    (when (equal (getf field-def :type) :button)
      (report-e "valid-rollup-fields"
        ":button fields are not allowed on rollup type ~s."
        ~type-key))
    (when (getf field-def :identity)
      (report-e "valid-rollup-fields"
        ":identity t is not allowed on rollup field ~s of ~s."
        ~field-key ~type-key))
    (when (getf field-def :column)
      (report-e "valid-rollup-fields"
        ":column t is a lie on rollup field ~s of ~s; a ~
             rollup has no table."
        ~field-key ~type-key))
    (unless (getf source :agg)
      (report-e "valid-rollup-fields"
        "Rollup field ~s of ~s must declare :agg in :source."
        ~field-key ~type-key))
    (unless (member source-table view-tables)
      (report-e "valid-rollup-fields"
        "Rollup field ~s of ~s reads from table ~s, which is ~
             not in :views :main :tables."
        ~field-key ~type-key ~source-table))))

(defun rollup-measure-facts (raw-fields)
  "Alist of (field-key . source-table) for real measures — fields whose :agg is
not :first."
  (loop for field-key in raw-fields by #'cddr
    for field-def in (cdr raw-fields) by #'cddr
    for source = (getf field-def :source)
    unless (eq (getf source :agg) :first)
    collect (cons field-key (getf source :table))))

(defun rollup-has-target-p (model source-type target-type)
  (loop with fields = (getf (getf model source-type) :fields)
    for field-def in (cdr fields) by #'cddr
    thereis (eq (getf field-def :target) target-type)))

(defun rollup-xref-neighbors (model view-tables table)
  "Tables in VIEW-TABLES connected to TABLE by a :target field in either
direction (xref edges are undirected for joins)."
  (remove table
    (remove-duplicates
      (loop for other in view-tables
        when (or (rollup-has-target-p model table other)
               (rollup-has-target-p model other table))
        collect other))))

(defun rollup-path-tables (model view-tables grain f-table)
  "Tables on some xref path GRAIN → F-TABLE within VIEW-TABLES. NIL when F-TABLE
is unreachable from GRAIN."
  (let ((on-path nil))
    (labels ((walk (node visited)
               (if (eq node f-table)
                 (progn (pushnew node on-path) t)
                 (let ((reached nil))
                   (dolist (next
                             (rollup-xref-neighbors model
                               view-tables node))
                     (unless (member next visited)
                       (when (walk next (cons next visited))
                         (setf reached t))))
                   (when reached (pushnew node on-path))
                   reached))))
      (walk grain (list grain)))
    on-path))

(defun valid-rollup-single-fact (model type-key grain raw-fields
                                  view-tables)
  "Single fact table (Issue 16) and grain-only (Issue 13) checks."
  (let ((facts (rollup-measure-facts raw-fields)))
    (unless facts
      (report-e "valid-rollup-single-fact"
        "Rollup type ~s is grain-only; at least one author field ~
         must have :agg other than :first."
        ~type-key))
    (loop for (field-key . table) in facts
      when (eq table grain)
      do (report-e "valid-rollup-single-fact"
           "Measure ~s on rollup ~s reads from the grain ~s; a ~
                real measure must read from the fact table."
           ~field-key ~type-key ~table))
    (loop for field-key in raw-fields by #'cddr
      for field-def in (cdr raw-fields) by #'cddr
      for source = (getf field-def :source)
      for table = (getf source :table)
      when (and (eq (getf source :agg) :first)
             (not (eq table grain)))
      do (report-e "valid-rollup-single-fact"
           ":agg :first is grain-only; field ~s on rollup ~s ~
                reads from non-grain table ~s."
           ~field-key ~type-key ~table))
    (let ((fact-tables (remove-duplicates (mapcar #'cdr facts))))
      (when (> (length fact-tables) 1)
        (report-e "valid-rollup-single-fact"
          "Rollup ~s mixes fact tables ~s; one fact table per ~
           rollup (mixed-depth is post-MVP)."
          ~type-key ~fact-tables))
      (let ((f-table (first fact-tables)))
        (unless (eq f-table (car (last view-tables)))
          (report-e "valid-rollup-single-fact"
            "Fact table ~s of rollup ~s must be last in :views ~
             :main :tables; a hop past F is a compile error."
            ~f-table ~type-key))
        (let ((on-path (rollup-path-tables model view-tables
                         grain f-table)))
          (unless on-path
            (report-e "valid-rollup-single-fact"
              "No xref path from grain ~s to fact table ~s for ~
               rollup ~s; name the intermediate hops in :tables."
              ~grain ~f-table ~type-key))
          (dolist (table view-tables)
            (unless (member table on-path)
              (report-e "valid-rollup-single-fact"
                "Table ~s on rollup ~s is an extra arm (not on the ~
                 path ~s → ~s); stars are post-MVP."
                ~table ~type-key ~grain ~f-table))))))))

(defun valid-model-filter-value (type-key clause col-type value)
  "Discrete-family literal check for :eq / :ne (07c). Boolean literals are Lisp
t / nil, not \"true\" or 1."
  (unless (case col-type
            (:boolean (typep value 'boolean))
            ((:text :password) (stringp value))
            (:integer (integerp value))
            (:real (numberp value))
            (:uuid (uuid-p value))
            (t nil))
    (report-e "valid-model-filter-value"
      "Filter clause ~s on ~s needs a literal matching column ~
       type ~s; got ~s."
      ~clause ~type-key ~col-type ~value)))

(defun valid-model-filter-clause (model type-key view-tables clause)
  "Validate one model-declared :filter 4-tuple (07c): table in :tables, column
exists, operator in the closed set, family rules."
  (unless (and (listp clause) (= (length clause) 4))
    (report-e "valid-model-filter"
      "Filter clause ~s on ~s must be a list of exactly four ~
       elements: (table column op value)."
      ~clause ~type-key))
  (destructuring-bind (table-key column-key op value) clause
    (unless (member table-key view-tables)
      (report-e "valid-model-filter"
        "Filter clause ~s on ~s names table ~s, which is not in ~
         :views :main :tables."
        ~clause ~type-key ~table-key))
    (unless (or (u:tree-get model table-key :fields column-key)
              (member column-key (default-fields :keys-only t)))
      (report-e "valid-model-filter"
        "Filter clause ~s on ~s names column ~s, which does not ~
         exist on ~s."
        ~clause ~type-key ~column-key ~table-key))
    (let ((ops *model-filter-operators*))
      (unless (member op ops)
        (report-e "valid-model-filter"
          "Filter operator ~s on ~s is not in the closed set ~s."
          ~op ~type-key ~ops)))
    (let ((col-type (rollup-column-type model table-key column-key)))
      (case op
        ((:eq :ne)
          (when (eq col-type :timestamp)
            (report-e "valid-model-filter"
              ":eq / :ne on date/timestamp column ~s of ~s is a ~
               compile error; use :last-days or :calendar."
              ~column-key ~type-key))
          (valid-model-filter-value type-key clause col-type value))
        (:last-days
          (unless (eq col-type :timestamp)
            (report-e "valid-model-filter"
              ":last-days on ~s requires a timestamp column; ~s ~
               is ~s."
              ~type-key ~column-key ~col-type))
          (unless (and (integerp value) (>= value 1) (<= value 1825))
            (report-e "valid-model-filter"
              ":last-days value ~s on ~s must be an integer in ~
               1..1825."
              ~value ~type-key)))
        (:calendar
          (unless (eq col-type :timestamp)
            (report-e "valid-model-filter"
              ":calendar on ~s requires a timestamp column; ~s is ~
               ~s."
              ~type-key ~column-key ~col-type))
          (unless (eq value :month)
            (report-e "valid-model-filter"
              ":calendar value ~s on ~s must be :month (the value ~
               slot is the extension point)."
              ~value ~type-key)))))))

(defun valid-model-filter (model type-key view-tables)
  "Validate a model-declared :filter (07c): always a list of 4-tuples, own
operator table, family rules, path-bound tables (a clause table in :tables is on
the path after Issue 16 checks). Absent :filter is legal; nil / () are not."
  (let ((type-def (getf model type-key)))
    (when (u:has (u:plist-keys type-def) :filter)
      (let ((filter (getf type-def :filter)))
        (unless (and (listp filter) filter (every #'listp filter))
          (report-e "valid-model-filter"
            ":filter on ~s must be a non-empty list of 4-tuple ~
             clauses; nil, (), a singular clause, and non-lists ~
             are compile errors."
            ~type-key))
        (dolist (clause filter)
          (valid-model-filter-clause model type-key view-tables
            clause))))))

(defun valid-rollup-field-types (model type-key raw-fields)
  "Field :type vs :agg checks (07a / Issue 13). The default-to-:text rule
stands; no inference from grain or agg."
  (loop for field-key in raw-fields by #'cddr
    for field-def in (cdr raw-fields) by #'cddr
    for source = (getf field-def :source)
    for agg = (getf source :agg)
    for column = (getf source :column)
    for declared = (or (getf field-def :type) :text)
    for source-type = (rollup-column-type model
                        (getf source :table) column)
    do (case agg
         (:first
           (unless (eq declared source-type)
             (report-e "valid-rollup-field-types"
               "Pass-through ~s on ~s declares :type ~s but ~
                    grain column is ~s."
               ~field-key ~type-key ~declared ~source-type)))
         (:count
           (unless (eq declared :integer)
             (report-e "valid-rollup-field-types"
               ":count measure ~s on ~s must be :type ~
                    :integer; got ~s."
               ~field-key ~type-key ~declared)))
         (:avg
           (unless (eq declared :real)
             (report-e "valid-rollup-field-types"
               ":avg measure ~s on ~s must be :type :real; ~
                    got ~s."
               ~field-key ~type-key ~declared)))
         (:sum
           (unless (member source-type '(:integer :real))
             (report-e "valid-rollup-field-types"
               ":sum measure ~s on ~s needs a numeric source ~
                    column; ~s is ~s."
               ~field-key ~type-key ~column ~source-type))
           (unless (eq declared source-type)
             (report-e "valid-rollup-field-types"
               ":sum measure ~s on ~s must declare :type ~s ~
                    (same as source); got ~s."
               ~field-key ~type-key ~source-type ~declared)))
         ((:list :distinct)
           (unless (eq declared source-type)
             (report-e "valid-rollup-field-types"
               ":list / :distinct measure ~s on ~s must ~
                    declare :type ~s (same as source); got ~s."
               ~field-key ~type-key ~source-type ~declared)))
         (otherwise
           (report-e "valid-rollup-field-types"
             "Unknown :agg ~s on field ~s of rollup ~s."
             ~agg ~field-key ~type-key)))))

(defun rollup-normalize-fields (type-key fields)
  "Rollup :fields are written as a list of (key def) pairs (the 07a–07e
authoring form). A plist is also accepted.  Returns the plist form; signals
report-e on any other shape."
  (cond
    ((null fields)
      (report-e "rollup-normalize-fields"
        "Rollup type ~s has no :fields."
        ~type-key))
    ((u:plistp fields) fields)
    ((and (listp fields)
       (every (lambda (p)
                (and (listp p) (= (length p) 2)
                  (keywordp (first p))))
         fields))
      (apply #'append fields))
    (t
      (report-e "rollup-normalize-fields"
        ":fields on rollup ~s must be a list of (field ~
         definition) pairs; got ~s."
        ~type-key ~fields))))

(defun rollup-injected-id (grain)
  "The single injected field on a rollup: :id from the grain PK. No :column t,
no :primary-key t, no :target, no default."
  `(:id (:type :uuid
          :source (:view :main :table ,grain :column :id
                    :agg :first))))

(defun rollup-compile-fields (type-key model grain raw-fields)
  "Compile a rollup's fields: the injected grain :id plus the author fields
(already normalized to a plist). Omitted :type defaults to :text (07a / Issue
13) — applied here, not in compile-field, so base-type behavior is unchanged. No
:created-at / :updated-at, no :reference renaming."
  (labels ((default-type (field-def)
             (if (getf field-def :type)
               field-def
               (add-to-plist field-def (list :type :text)))))
    (let ((fields (append (rollup-injected-id grain)
                    (mapcar
                      (lambda (f)
                        (if (and (listp f) (keywordp (first f)))
                          (default-type f)
                          f))
                      (u:deep-copy raw-fields)))))
      (loop
        for field-key in fields by #'cddr
        for field-def in (cdr fields) by #'cddr
        appending
        (list field-key
          (compile-field model type-key field-key field-key
            field-def))))))

(defun compile-rollup-type-def (model type-key)
  "Compile a :rollup t type-def (07a/07b/07c/07d Lisp). No :table-name key is
stored — a rollup has no physical table, and a phantom rt_<rollup> must not
exist even as a string. DDL / DML skips key off :phase-a-shape :measure (stage-2
/ create-tables). The measure SQL generator and the be-list branch are 08b."
  (let* ((type-def (getf model type-key))
          (grain (getf type-def :grain))
          (views (getf type-def :views))
          (view-tables (u:tree-get views :main :tables))
          (raw-fields (rollup-normalize-fields type-key
                        (getf type-def :fields))))
    (valid-rollup-matrix model type-key type-def grain views)
    (valid-rollup-fields type-key raw-fields view-tables)
    (valid-rollup-single-fact model type-key grain raw-fields
      view-tables)
    (valid-model-filter model type-key view-tables)
    (valid-rollup-field-types model type-key raw-fields)
    (let* ((fields (rollup-compile-fields type-key
                     (add-to-plist model
                       (list type-key
                         (add-to-plist type-def
                           (list :fields raw-fields))))
                     grain raw-fields))
            (roles (when (type-has-roles type-def)
                     (getf type-def :type-roles '("admin"))))
            (final-def (add-to-plist
                         type-def
                         (list
                           :internal nil
                           :create nil
                           :type-roles roles
                           :category (compute-category type-def)
                           :fields fields
                           :phase-a-shape :measure
                           :grain grain
                           :suppress-roles t
                           :default-sort (getf type-def :default-sort)
                           :display
                           (if (u:has (u:plist-keys type-def)
                                 :display)
                             (getf type-def :display)
                             t)))))
      (valid-form-fields type-key final-def)
      (valid-default-sort type-key final-def)
      final-def)))

(defun compile-type-def (model type-key)
  (if (getf (getf model type-key) :rollup)
    (compile-rollup-type-def model type-key)
    (compile-base-type-def model type-key)))

(defun compile-base-type-def (model type-key)
  (valid-non-rollup-keys model type-key)
  (let* ((type-def (getf model type-key))
          (built-in (getf type-def :built-in))
          (is-joiner (getf type-def :is-joiner))
          (internal (getf type-def :internal is-joiner))
          (create (compile-create (getf type-def :create)))
          ;; Augment model with synthesized status fields so that
          ;; compile-fields / field-source can find them
          (raw-fields (u:deep-copy
                        (u:tree-get model type-key :fields)))
          (validated-fields (progn
                              (ensure-action-on-buttons type-key raw-fields)
                              raw-fields))
          (augmented-fields (synthesize-status-fields type-key
                              validated-fields))
          (augmented-model (if (eq augmented-fields validated-fields)
                             model
                             (let* ((td (getf model type-key))
                                     (new-td (add-to-plist td
                                               (list :fields augmented-fields))))
                               (add-to-plist model
                                 (list type-key new-td)))))
          (fields (compile-fields type-key augmented-model))
          (table-name (table-name type-key built-in))
          (tree (getf type-def :tree))
          (is-leaf (getf type-def :is-leaf))
          (parent-type (getf type-def :parent-type))
          (fs-backed (getf type-def :fs-backed))
          (roles (when (type-has-roles type-def)
                   (getf type-def :type-roles '("admin"))))
          (category (compute-category type-def)))
    (validate-tree model type-key tree is-leaf parent-type fs-backed)
    (let* ((fields-with-path (mark-path-field type-key fs-backed fields))
            (user-setting (getf type-def :user-setting))
            (default-sort (getf type-def :default-sort))
            (augmented-def (augment-update-form type-def fields))
            ;; Expand :compose sugar into lifecycle hook forms
            (compose-hooks (expand-compose-hooks type-key type-def))
            (compose-model (if compose-hooks
                             (let ((td-with-hooks type-def))
                               (loop for (lk forms) on compose-hooks by #'cddr
                                 do (let ((existing (getf td-with-hooks lk)))
                                      (setq td-with-hooks
                                        (add-to-plist td-with-hooks
                                          (list lk
                                            (append
                                              (when (listp existing)
                                                existing)
                                              forms))))))
                               (add-to-plist model
                                 (list type-key td-with-hooks)))
                             model))
            (final-def (add-to-plist
                         augmented-def
                         (append
                           (list
                             :internal internal
                             :create create
                             :type-roles roles
                             :category category
                             :table-name table-name
                             :fields fields-with-path
                             :tree tree
                             :is-leaf is-leaf
                             :parent-type parent-type
                             :fs-backed fs-backed
                             :user-setting user-setting
                             :default-sort default-sort
                             :phase-a-shape :base
                             :suppress-roles
                             (or (getf type-def :suppress-roles) user-setting))
                           ;; Compiled lifecycle hooks override raw values on 
                           ;; type-def
                           (compile-lifecycle-hooks compose-model type-key)))))
      (valid-form-fields type-key final-def)
      (valid-default-sort type-key final-def)
      final-def)))

(defun preliminary-model-check (&optional def key-path)
  (loop with current = (apply #'u:tree-get (cons def key-path))
    for key in current by #'cddr
    for sdef in (cdr current) by #'cddr
    do (cond
         ((or (not sdef) (atom sdef))
           nil)
         ((and
            (listp sdef)
            (equal (car sdef) 'quote))
           (error
             "Quoted list in model definition at ~{~(~s~)~^ -> ~}."
             (append key-path (list key))))
         ((and
            (member :views key-path)
            (equal key :tables)
            (listp sdef))
           ;; A :tables list is a list of type keys (never a plist,
           ;; never 4-tuples); do not recurse into it. Any nesting
           ;; depth (:views :main :tables and beyond) hits this arm.
           t)
         ((and
            (equal (second key-path) :fields)
            (equal key :validations)
            (listp sdef))
           t)
         ((and
            (equal (second key-path) :fields)
            (equal key :action)
            (or (keywordp sdef) (listp sdef)))
           t)
         ((and
            (equal key :type-roles)
            (listp sdef)
            t))
         ((and
            (equal key :filter)
            (listp sdef))
           ;; Type-level :filter (rollup-only, 07c). The walker does not recurse
           ;; into the 4-tuples; grammar validation lives in valid-model-filter.
           t)
         ((equal key :default-sort)
           ;; Type-level :default-sort ((:field :asc|:desc)). The walker
           ;; must not recurse into it: the 2-element form is a plist,
           ;; and the 1-element form is not. Shape, field existence, and
           ;; :sortable are checked in valid-default-sort with specific
           ;; messages.
           t)
         ((and
            (equal key :fields)
            (getf (apply #'u:tree-get (cons def key-path)) :rollup))
           ;; Rollup :fields are written as a list of (key def) pairs (07a–07e
           ;; form). compile-rollup-type-def normalizes them to a plist; the
           ;; pair definitions are validated by the rollup validators and
           ;; compile-field.
           t)
         ((and
            (member key '(:pre-create :post-create
                           :pre-update :post-update
                           :pre-delete :post-delete))
            (listp sdef))
           t)
         ((and
            (member (second key-path) '(:list-form :update-form :add-form))
            (equal key :fields))
           t)
         ((and
            (equal key :options)
            (member :ui key-path)
            (listp sdef))
           t)
         ((u:plistp sdef)
           (preliminary-model-check def (append key-path (list key))))
         ((equal 'quote (car sdef))
           (error
             "Quoted list in model definition at ~{~(~s~)~^ -> ~}."
             (append key-path (list key))))
         (t (error
              "Invalid value in model definition at ~{~(~s~)~^ -> ~}: ~s"
              (append key-path (list key)) sdef)))))

(defun stage-1 (model)
  (loop
    with full-model = (append *base-model* model)
    initially (preliminary-model-check full-model)
    for type-key in full-model by #'cddr
    for type-def in (cdr full-model) by #'cddr
    for compiled-def = (compile-type-def full-model type-key)
    appending (list type-key compiled-def)))

(defun stage-2 (model)
  (loop
    for type-key in model by #'cddr
    for type-def = (getf model type-key)
    for built-in = (getf type-def :built-in)
    for fields = (getf type-def :fields)
    for joiner = (getf type-def :is-joiner)
    ;; Rollup (measure shape): no DDL / DML, no table-name
    ;; recompute. enrich-views still runs (aliases / columns /
    ;; scope only). Same precedent as the joiner cut.
    for measure = (eq (getf type-def :phase-a-shape) :measure)
    for table-name = (unless measure
                       (table-name type-key built-in))
    for views = (enrich-views model type-key)
    for insert-sql = (unless (or joiner measure)
                       (insert-sql model type-key))
    for update-sql = (unless (or joiner measure)
                       (update-sql model type-key))
    for delete-sql = (unless (or joiner measure)
                       (delete-sql model type-key))
    for search-sql = (unless (or joiner measure)
                       (search-sql model type-key))
    for create-table-sql = (unless measure
                             (create-table-sql table-name fields))
    for new-def = (remove-null-value-pairs
                    (list
                      :create-table-sql create-table-sql
                      :views views
                      :insert-sql insert-sql
                      :update-sql update-sql
                      :delete-sql delete-sql
                      :search-sql search-sql))
    appending (list type-key (add-to-plist type-def new-def))))

(defun stage-3 (model)
  (loop
    for type-key in model by #'cddr
    for type-def in (cdr model) by #'cddr
    for fields = (compile-fields-stage-2 model type-key)
    for new-def = (add-to-plist type-def
                    (list
                      :fields fields
                      :searchable-fields (collect-searchable-fields fields)))
    appending (list type-key new-def)))

(defun validate-model (model)
  "Pure validation: structural checks + stage-1 compile. No side effects (no DB,
no RBAC mutation). Returns the stage-1 compiled model or signals an error."
  (stage-1 model))

(defun compile-model (model)
  (let* ((model-1 (validate-model model))
          (model-2 (stage-2 model-1))
          (model-3 (stage-3 model-2)))
    model-3))

(defun add-system-user-settings ()
  (loop
    for user in (list "admin" "guest")
    for setting-id = (be-value-id :settings :user user "admin")
    unless setting-id do
    (add-user-setting-rows
      :users
      `(:name ,user)
      "admin")))

(defun add-root-fs-resources ()
  (unless (probe-file *doc-root*)
    (error "Document root not found: ~a" *doc-root*))
  (loop
    for type-key in *compiled-model* by #'cddr
    for type-def in (cdr *compiled-model*) by #'cddr
    for roles = (getf type-def :type-roles)
    for tree = (getf type-def :tree)
    for path-field = (path-field type-key)
    for is-leaf = (getf type-def :is-leaf)
    when (and tree (not is-leaf) path-field) do
    (let* ((logical-path "/")
            (type-path (format nil "/~(~a~)/" type-key))
            (fs-path (u:join-paths *doc-root* type-path))
            (resource-name (find-resource-name
                             type-key
                             `((,type-key ,path-field :eq ,logical-path)))))
      (unless resource-name
        (ensure-directories-exist fs-path)
        (be-insert-internal
          :directories
          `(:name ,logical-path)
          "admin"
          :roles roles)))))

(defun resolve-model-path (file)
  "Resolve model name to a path: models/<name>.lisp, then
models/test/<name>.lisp. Signals a report-e error if neither exists."
  (let ((primary (u:join-paths *package-root* "models"
                   (format nil "~a.lisp" file)))
         (fallback (u:join-paths *package-root* "models" "test"
                     (format nil "~a.lisp" file))))
    (cond
      ((u:file-exists-p primary) primary)
      ((u:file-exists-p fallback) fallback)
      (t (report-e "resolve-model-path"
           "Model file ~a.lisp not found in models/ or models/test/"
           ~file)))))

(defun list-models ()
  "Return model names from the top level of models/ only. Files under
models/test/ are test fixtures and are excluded."
  (mapcar
    (lambda (f) (u:filename-only (u:replace-extension f "")))
    (remove-if
      (lambda (path)
        (search (u:join-paths *package-root* "models" "test")
          (namestring path)))
      (u:directory-listing (u:join-paths *package-root* "models/")
        :files-only t
        :leaf-filter "(?i)\\.lisp$"))))

(defgeneric set-model (model)
  (:method ((model list))
    (loop
      initially
      (when *compiled-model* (reset-tables *compiled-model*))
      (setf
        *compiled-model* nil
        *top-level-settings* (top-level-settings model))
      with compiled-model = (compile-model (getf model :types))
      for type-key in (u:plist-keys compiled-model)
      for type-def in (u:plist-values compiled-model)
      for table-name = (getf type-def :table-name)
      appending (list type-key table-name) into summary
      finally
      (setf *compiled-model* compiled-model)
      (create-tables)
      (ensure-declared-roles)
      (ensure-model-roles)
      (add-type-roles)
      (add-system-user-settings)
      (add-root-fs-resources)
      (start-web-server)
      (return summary)))
  (:method ((file string))
    "Accepts a file name (no path and no extension), resolves the path via
RESOLVE-MODEL-PATH (checking models/ then models/test/), reads the model from
that file, and sets that model with SET-MODEL."
    (let ((path (resolve-model-path file)))
      (with-open-file (in path)
        (set-model (cadr (read in))))))
  (:documentation ":public: Sets the model to the given plist. If given a
string instead of a plist, resolves the string to a file in the `models`
directory, loads the plist from there, and then sets the model that
plist. Setting the model involves compiling the model into an enriched model
that includes compiled functions (machine code), generated parameterized SQL, as
well as maps, other data structures, and settings that Data UI can use to
efficiently instantiate and support the application described by MODEL."))

(defun reset-to-model (model)
  ":public: Resets the database (drop all records from all tables, drop all
tables associated with user-defined types), then call SET-MODEL with MODEL."
  (reset-database)
  (set-model model))

(defun create-tables ()
  (loop with m = *compiled-model*
    for type-key in m by #'cddr
    ;; Rollup (measure shape): no physical table, nothing to
    ;; create. Do not probe information_schema with a nil
    ;; :table-name and do not db:query nil.
    unless (eq (u:tree-get m type-key :phase-a-shape) :measure)
    do
    (let* ((table-name (u:tree-get m type-key :table-name))
            (table (u:tree-get m type-key :create-table-sql :table))
            (trigger (u:tree-get m type-key :create-table-sql :trigger))
            (index (u:tree-get m type-key :create-table-sql :index))
            (sort-indexes (u:tree-get m type-key :create-table-sql
                            :sort-index)))
      (unless (a:with-rbac (*rbac*)
                (a:rbac-query
                  (list
                    "select 1 from information_schema.tables where table_name = $1"
                    table-name)
                  :single))
        (a:with-rbac (*rbac*)
          (db:query table)
          (db:query trigger)
          (when index (db:query index))
          (when sort-indexes
            (loop for ddl in sort-indexes do (db:query ddl))))
        (pl:pdebug :in "create-tables"
          :state "added tables, indexes, and triggers"
          :table table-name :type-key type-key)))))

(defun type-resource-name (type-key)
  (format nil "type-~(~a~)" type-key))

(defun ensure-model-roles ()
  "Create any roles declared in :type-roles that don't yet exist in RBAC. Called
after compilation (compile-model is now pure) and before add-type-roles (which
needs roles to exist)."
  (loop with m = *compiled-model*
    for type-key in m by #'cddr
    for type-def in (cdr m) by #'cddr
    for roles = (getf type-def :type-roles)
    when (type-has-roles type-def)
    do
    (loop for role-spec in roles
      for role = (if (stringp role-spec) role-spec (car role-spec))
      for permissions = (if (stringp role-spec)
                          '("create" "read" "update" "delete")
                          (cdr role-spec))
      unless (a:get-id *rbac* "roles" role)
      do (a:add-role *rbac* role :permissions permissions))))

(defun ensure-declared-roles ()
  "Create roles declared in the model's :new-roles key that don't yet exist in
RBAC, granting exactly the declared permissions. Existing roles are never
modified. Called from set-model after create-tables and before
ensure-model-roles, so a declared role also named in :type-roles keeps its
declared permissions (ensure-model-roles would otherwise default a new role to
full CRUD)."
  (loop with new-roles = (model-new-roles)
    for rest = new-roles then (cddr rest)
    while rest
    for role-key = (first rest)
    for permissions = (second rest)
    for role = (string-downcase (symbol-name role-key))
    unless (a:get-id *rbac* "roles" role)
    do
    (pl:pinfo :in "ensure-declared-roles" :state "adding declared role"
      :role role :permissions permissions)
    (a:add-role *rbac* role :permissions permissions)))

(defun add-type-roles ()
  (loop with m = *compiled-model*
    for type-key in m by #'cddr
    for type-def in (cdr m) by #'cddr
    for roles = (add-to-list (getf type-def :type-roles) "admin")
    for resource-name = (type-resource-name type-key)
    when (and (type-has-roles type-def)
           (not (a:get-id *rbac* "resources" resource-name)))
    do
    (pl:pdebug :in "add-type-roles" :state "adding type role"
      :type-key type-key :resource-name resource-name :type-roles roles)
    (a:add-resource *rbac* resource-name :roles roles)))

(defun model-for (type-key)
  (getf *compiled-model* type-key))

(defun model-views-for (type-key)
  (list :views (u:tree-get *compiled-model* type-key :views)))

(defun model-view-for (type-key view-key)
  (list view-key (u:tree-get *compiled-model* type-key :views view-key)))

(defun model-field-for (type-key field-key)
  (list type-key
    (list :fields
      (list field-key
        (u:tree-get *compiled-model* type-key :fields field-key)))))

(defun model-sql-for (type-key)
  (loop for key in '(:create-table-sql :insert-sql :update-sql :delete-sql)
    for value = (u:tree-get *compiled-model* type-key key)
    append (list key value) into plist
    finally (return (list type-key plist))))

(defun model-fields-for (type-key)
  (list type-key
    (list :fields (u:tree-get *compiled-model* type-key :fields))))

(defun model-view-sql-for (type-key &key (view-key :main))
  (u:tree-get *compiled-model* type-key :views view-key :sql))

(defun model-field-names-for (type-key &key
                               (keys '(:name-sql :alias-key)))
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    append (list field-key
             (loop for key in keys
               appending (list key (getf field-def key))))))

(defun model-field-create-sql-for (type-key)
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for sql = (getf field-def :create-sql)
    append (list field-key sql)))

(defun model-field-attribute-for (type-key attribute)
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for attr-value = (getf field-def attribute)
    append (list field-key attr-value)))

(defun booleanp (x)
  (when (member x '(t nil)) t))

(defvar *valid-permissions* '("create" "read" "update" "delete")
  ":private: Closed permission vocabulary for :new-roles lists. These rows
already exist in rbac after initialize-database; anything else fails inside
a:add-role. Keywords are also wrong here — permission values in the model
language are strings.")

(defvar *reserved-role-names*
  '("admin" "admin:exclusive" "guest" "guest:exclusive" "public" "settings"
     "logged-in" "user-creator" "role-creator" "permission-creator")
  ":private: Role names a model may not declare under :new-roles. These exist
after initialize-database or carry rbac semantics of their own;
ensure-declared-roles would silently skip them (they already exist), so they are
compile errors instead. Any name ending in \":exclusive\" or prefixed \"admin:\"
/ \"guest:\" is reserved too (checked separately).")

(defun valid-permission-list (permissions role)
  ":private: Validate PERMISSIONS, the declared permission list for ROLE (a
downcased role-name string) under :new-roles. Each entry must be a non-keyword
string from *valid-permissions*; the list must be non-empty and duplicate-free.
Signals report-e otherwise."
  (unless (and (listp permissions) permissions)
    (report-e "valid-permission-list"
      "Role ~a in :new-roles has an empty permission list."
      ~role))
  (loop for permission in permissions
    unless (stringp permission)
    do (report-e "valid-permission-list"
         "Permission ~s for role ~a in :new-roles must be a string."
         ~permission ~role)
    unless (member permission *valid-permissions* :test #'equal)
    do (report-e "valid-permission-list"
         "Permission ~s for role ~a in :new-roles is not one of ~
            ~{~a~^, ~}."
         ~permission ~role *valid-permissions*)
    when (member permission (cdr (member permission permissions))
           :test #'equal)
    do (report-e "valid-permission-list"
         "Permission ~a appears more than once for role ~a in ~
            :new-roles."
         ~permission ~role)))

(defun reserved-role-name-p (role)
  ":private: T when ROLE (a downcased role-name string) may not be declared
under :new-roles: an entry in *reserved-role-names*, any name ending in
\":exclusive\", or any \"admin:\" / \"guest:\" ~ prefixed name."
  (or
    (member role *reserved-role-names* :test #'equal)
    (u:ends-with role ":exclusive")
    (u:starts-with role "admin:")
    (u:starts-with role "guest:")))

(defun valid-new-roles (value)
  ":private: Validate a :new-roles VALUE: a keyword-keyed plist of role name to
non-empty list of permission strings. Signals report-e on bad shape, duplicate
keys, reserved role names, invalid role names, or bad permission lists. Returns
VALUE unchanged — top-level-settings stores the author plist via getf; no
normalized form exists."
  (unless (u:plistp value)
    (report-e "valid-new-roles"
      ":new-roles value must be a plist of role name to permission ~
       list, got ~s."
      ~value))
  (let ((repeats (plist-repeated-keys value)))
    (when repeats
      (report-e "valid-new-roles"
        ":new-roles has repeated role names ~{~s~^, ~}."
        ~repeats)))
  (loop with role-key = nil and permissions = nil
    for rest = value then (cddr rest)
    while rest
    do
    (setf role-key (first rest) permissions (second rest))
    (unless (keywordp role-key)
      (report-e "valid-new-roles"
        ":new-roles role name ~s must be a keyword."
        ~role-key))
    (let ((role (string-downcase (symbol-name role-key))))
      (when (reserved-role-name-p role)
        (report-e "valid-new-roles"
          ":new-roles role name ~a is reserved for the system."
          ~role))
      (unless (a:valid-role-p *rbac* role)
        (report-e "valid-new-roles"
          ":new-roles role name ~a is not a valid role name."
          ~role))
      (valid-permission-list permissions role))))

(defun model-new-roles ()
  ":public: Declared-roles plist for the current model, or NIL. Keys are
role-name keywords; values are permission-string lists, exactly as the author
wrote them."
  (getf *top-level-settings* :new-roles))

(defun valid-top-level-value (key value regex &key
                               (required t)
                               (predicates (list #'stringp)))
  (let ((regexes (if (stringp regex) (list regex) regex)))
    (unless (if required value t)
      (error "Value for ~(~s~) is required." key))
    (loop for p in predicates
      unless (funcall p value)
      do (error "~(~s~) value ~s looks fishy, doesn't pass ~{~a~}."
           key value predicates))
    (loop for r in regexes
      unless (re:scan r value)
      do (error "~(~s~) value ~s does not look like a ~(~a~)."
           key value key))))

(defun valid-top-level-field (model key)
  (let ((value (getf model key)))
    (case key
      (:title
        (valid-top-level-value
          key value
          "^[a-zA-Z0-9][-a-zA-Z0-9+_',.?/`~!@#$%^&*()+=\\[\\]\\{\\}]*"))
      (:name
        (valid-top-level-value key value "^[a-z][-a-z0-9]*")
        ;; "profile" and "profile-*" deploy as dataui-profile[-*],
        ;; colliding with exposed host-profile HAProxy backends
        ;; (scripts/data-ui expose-profile).
        (when (or (string= value "profile")
                (u:starts-with value "profile-"))
          (error "~(~s~) value ~s is reserved for host-profile HAProxy backends."
            key value)))
      (:version
        (valid-top-level-value key value
          "^[a-z0-9.](?:[a-z0-9]|[._+-][a-z0-9])*$"))
      (:domain
        (valid-top-level-value key value
          (format nil "~a~a"
            "^(?:[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?\.)+"
            "[a-zA-Z]{2,}$")))
      (:repl
        (valid-top-level-value key value nil
          :required nil :predicates (list #'booleanp)))
      (:landing-page
        (valid-top-level-value key value nil
          :required nil
          :predicates (list (lambda (x) (or (null x) (keywordp x)))))
        (when value
          (unless (getf (getf model :types) value)
            (error "~(~s~) value ~s is not a defined type." key value))))
      (:types
        (valid-top-level-value key value nil
          :predicates (list #'u:plistp)))
      (:new-roles
        (when value (valid-new-roles value))))))

(defun top-level-settings (model)
  (loop for k in *top-level-keys*
    do (valid-top-level-field model k)
    unless (equal k :types)
    append (list k (getf model k))))

(defun top-level-model-field (key &key
                               (top-level *top-level-settings*)
                               default)
  (unless top-level
    (error "Model has not been compiled."))
  (or (getf top-level key default)
    (error "~(~s~) missing from model." key)))

(defun model-title ()
  (top-level-model-field :title))

(defun model-name ()
  (top-level-model-field :name))

(defun model-version ()
  (top-level-model-field :version))

(defun model-domain ()
  (top-level-model-field :domain))

(defun model-repl ()
  (top-level-model-field :repl :default nil))

(defun model-landing-page ()
  "Configured (not user-resolved) landing type, or NIL."
  (getf *top-level-settings* :landing-page))
