(in-package :data-ui)

(defparameter *top-level-settings* nil)
(defparameter *top-level-keys*
  '(:title :name :version :domain :repl :landing-page))

(defparameter *max-value-length* 1000)

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
  (and (or (null value) (stringp value))
    (<= (length value) *max-value-length*)))

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

(defun v-type (type-key field-key value user)
  (declare (ignore user))
  (pl:pdebug :in "v-type" :step 1
    :type-key type-key :field-key field-key :value value)
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
        (format nil "must be a valid ~s." field-type-key)))))

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

(defun add-user-settings (type-key data user &key id roles record)
  (declare (ignore type-key id roles record))
  (let ((username (getf data :name)))
    (pl:pdebug :in "add-user-settings" :data data :username username)
    (be-insert-internal :settings `(:dark-mode :false
                                     :font-size 10
                                     :display-name ,username
                                     :user ,username)
      "admin")))

(defun remove-user-settings (type-key data user &key id roles record)
  (declare (ignore type-key data user id roles))
  (let* ((username (u:tree-get record :record :name))
          (settings-id (when username
                         (be-id :settings
                           `((:users :name :eq ,username))
                           "admin"))))
    (pl:pdebug :in "remove-user-settings" :step 2
      :username username
      :settings-id settings-id)
    (when settings-id
      (delete-by-id :settings settings-id))))

;;; ---------------------------------------------------------------------------
;;; Hook Registry
;;;
;;; A curated registry of named hooks (validations and lifecycle).
;;; Each entry stores: name (keyword), kind (:validation | :lifecycle),
;;; a parameter schema (plist of keyword → type), and a factory function
;;; that accepts the resolved parameters and returns a contract-conforming
;;; function.
;;;
;;; Validation contract: (lambda (type-key field-key value user)
;;;                        → nil | error-string)
;;; ---------------------------------------------------------------------------

(defparameter *hook-registry* (make-hash-table))

(defstruct hook-entry
  name kind parameters factory)

(defun register-hook (name kind parameters factory)
  "Register a hook named NAME (keyword) of KIND (:validation or :lifecycle).
PARAMETERS is a plist specifying required keyword params and their types,
e.g. (:max integer), or NIL if the hook takes no parameters.  FACTORY is a
function that receives the resolved parameter values as keyword args and
returns a validation/lifecycle function."
  (check-type name keyword)
  (check-type kind (member :validation :lifecycle))
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
            (otherwise
              (setf (getf result key) val))))
    result))

(defun resolve-hook-form (form &key (kind :validation)
                                 type-key field-key)
  "Resolve a single hook FORM into a contract-conforming function.
FORM may be:
  - A keyword:  :required  → zero-arg registry lookup
  - A plist list: (:max-length :max 20) → parameterized registry lookup
  - A lambda:    (lambda ...) → compiled as-is (expert tier)
TYPE-KEY and FIELD-KEY are optional context used only for error
messages.
Signals an error for unknown hooks, wrong kind, :shell forms, or bad params."
  (flet ((ctx (fmt)
           (if (and type-key field-key)
               (format nil "~a for ~(~a/~a~): " fmt type-key field-key)
               (if type-key
                   (format nil "~a for ~(~a~): " fmt type-key)
                   (format nil "~a" fmt)))))
    (cond
    ;; Raw function — expert tier, pass through as-is
    ((functionp form)
     form)
    ;; Raw lambda — expert tier, pass through
    ((and (consp form) (eq (car form) 'lambda))
     (compile nil form))
    ;; :shell — explicitly rejected in this plan
    ((and (consp form) (eq (car form) :shell))
     (report-e "resolve-hook-form"
               (ctx ":shell hooks are not supported yet")))
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

;;; ---------------------------------------------------------------------------
;;; Registry builtins — migrated from *validation-map*
;;; ---------------------------------------------------------------------------

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

(defparameter *forms* '(:list-form :add-form :update-form))

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
       :post-create ,#'add-user-settings
       :pre-delete ,#'remove-user-settings
       :views (:main (:tables (:users :role-users :roles))
                :roles (:tables (:roles)))
       :fields (:name (:type :text :identity t
                        :source (:view :main :column :name :agg :first)
                        :ui (:label "Username" :input-type :line)
                        :validations (:required :user-name)
                        :column t :not-null t :unique t)
                 :password (:type :password
                             :source (:view :main :column :password :agg :first)
                             :force-sql-name "password_hash"
                             :ui (:label "Password" :input-type :password)
                             :validations (:required :password)
                             :column t :not-null t)
                 :email (:type :text
                          :default "no-email"
                          :force-sql-name "email"
                          :source (:view :main :column :email :agg :first)
                          :ui (:label "Email" :input-type :line)
                          :validations (:email)
                          :column t :not-null t)
                 :roles (:type :list
                          :ui (:label "Roles" :input-type :checkbox-list)
                          :validations (:join-items-exist)
                          :source (:view :main :table :roles :column :name :agg :list)
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
                        :ui (:label "Resource" :input-type :line)
                        :source (:view :main :column :name :agg :first)
                        :validations (:required)
                        :column t :not-null t :unique t)
                 :roles (:type :list
                          :ui (:label "Roles" :input-type :checkbox-list)
                          :source (:view :main :table :roles :column :name :agg :list)
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
                        :ui (:label "Permission" :input-type :line)
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
                        :ui (:label "Role" :input-type :line)
                        :source (:view :main :column :name :agg :first)
                        :column t :not-null t :unique t)
                 :permissions (:type :list
                                :ui (:label "Permissions" :input-type :checkbox-list)
                                :source (:view :main
                                          :table :permissions
                                          :column :name
                                          :agg :list)
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
                        :ui (:label "Login" :input-type :read-only)
                        :target :users
                        :source (:view :users :table :users :column :name :agg :first)
                        :column t :not-null t :unique t)
                 :dark-mode (:type :boolean :default :false
                              :ui (:label "Dark Mode" :input-type :check-box)
                              :source (:view :main :column :dark-mode :agg :first)
                              :column t :not-null t)
                 :font-size (:type :integer :default 12
                              :ui (:label "Font Size" :input-type :line)
                              :source (:view :main :column :font-size :agg :first)
                              :column t :not-null t)
                 :display-name (:type :text :default "(non specified)"
                                 :ui (:label "Real Name" :input-type :line)
                                 :source (:view :main :column :display-name :agg :first)
                                 :column t :not-null t)
                 :bio (:type :text :default "(non specified)"
                        :ui (:label "Bio" :input-type :text)
                        :source (:view :main :column :bio :agg :first)
                        :column t :not-null t))
       :list-form (:fields (:user :dark-mode :font-size :display-name :bio))
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
                        :ui (:label "User" :input-type :line)
                        ;; TODO: A bad :view, :table, :column, or :agg should
                        ;;       raise a compile-time error.
                        :source (:view :main :column :user :agg :first)
                        :column t :not-null t :unique t)
                 :value (:type :text
                          :ui (:lable "Value" :input-type :line)
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
                 (mapcar (lambda (f) (getf f :name-sql)) identity-fields))))))

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

(defun fk-columns (model type-key)
  (loop with fields = (u:tree-get model type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for joiner = (getf field-def :join-table)
    when joiner
    append (fields-attribute
             (u:tree-get model joiner :fields) '(:target) :name-sql)))

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
    with fk-columns = (fk-columns model type-key)
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
                       (otherwise fk-columns))
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
    with fk-columns = (fk-columns model type-key)
    with main-sql = "update ~a set ~{~a = ~a~^, ~} where id = $~d"
    with delete-sql = "delete from ~a where ~{~a = ~a~^ and ~}"
    with insert-sql = "insert into ~a (~{~a~^, ~}) values (~{~a~^, ~})"
    for key in update-keys by #'cddr
    for table in (cdr update-keys) by #'cddr
    for table-name = (table-name table (built-in-p table model))
    for table-cols = (case key (:main columns) (otherwise fk-columns))
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

(defun find-xref-connecting (table xrefs)
  "Returns the first xref where TABLE is either :source or :target."
  (loop for xref in xrefs
        for source = (getf xref :source)
        for target = (getf xref :target)
        when (or (equal source table) (equal target table))
        do (return (u:deep-copy xref))))

(defun ordered-xrefs (model view-tables)
  ":private: Returns a list of xrefs in the context of VIEW-TABLES. The xrefs
are returned in the proper order, such that when they're used serially to create
join clauses, any external references point to tables that have already been
joined.

MVP limitation: this ordering works for linear chains (A->B->C) and star
patterns (A<-B, A<-C). It will break on diamond patterns (A->B, A->C, B->D,
C->D) where a table is reachable via two paths. This is acceptable for the MVP."
  (loop for table in view-tables
    append (find-table-xrefs model view-tables table) into xrefs
    finally
    (return
      (loop
        with remaining-xrefs = (u:deep-copy xrefs)
        for j-table in view-tables
        for j-xref = (find-xref-connecting j-table remaining-xrefs)
        when j-xref
        do (setf remaining-xrefs (remove j-xref remaining-xrefs :test #'equal))
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
    vfs))

(defparameter *lifecycle-keys*
  '(:pre-create :post-create
    :pre-update :post-update
    :pre-delete :post-delete))

(defun compile-lifecycle-hooks (model type-key)
  "Resolve all lifecycle slots for TYPE-KEY into lists of functions.
Returns a plist of :key → function-list for each lifecycle slot that
has a value in the model."
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
    and column = (if target t (getf field-def :column))
    and primary-key = (when (getf field-def :primary-key) "primary key")
    and not-null = (if target
                     "not null"
                     (when (getf field-def :not-null) "not null"))
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
                       :not-null (if target t (getf field-def :not-null))
                       :reference (when (equal old-field-key :reference) t)
                       :default default-value))
    with attrs = '(:base-field :ui :unique :primary-key :target :join-table
                    :autofill :identity)
    for attr in attrs
    append (list attr (getf field-def attr)) into def
    finally (return (append def new-def))))

(defun resolve-scope-alias (model type-key view-key table-key scope)
  "Resolve a :scope keyword on a field source to the alias key
that the view result uses for the scoped column. Currently only
:scope :user is supported. When :scope :user is specified, the
function looks up the :user field on TABLE-KEY in the view's
aliases and returns its alias-key. Returns NIL when SCOPE is NIL."
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
    for view-def-new = (list
                         :tables (getf view-def :tables)
                         :scope (valid-view-scope type-key scope-def)
                         :sql (view-sql model view-def)
                         :aliases (view-aliases model view-def)
                         :columns (view-columns model view-def))
    appending (list view-key view-def-new)))

(defun type-has-roles (type-def)
  (unless (getf type-def :internal) t))

(defun compile-create (fn)
  (cond
    ((equal fn :auto) :auto)
    ((equal (type-of fn) 'compiled-function) fn)
    ((and (equal (type-of fn) 'cons) (eq (car fn) 'lambda))
      (compile nil fn))
    ((null fn) nil)
    (t (error "Invalid :create function definitions: ~a" fn))))

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

(defun compile-type-def (model type-key)
  (let* ((type-def (getf model type-key))
          (built-in (getf type-def :built-in))
          (is-joiner (getf type-def :is-joiner))
          (internal (getf type-def :internal is-joiner))
          (create (compile-create (getf type-def :create)))
          (fields (compile-fields type-key model))
          (table-name (table-name type-key built-in))
          (tree (getf type-def :tree))
          (is-leaf (getf type-def :is-leaf))
          (parent-type (getf type-def :parent-type))
          (fs-backed (getf type-def :fs-backed))
          (roles (when (type-has-roles type-def)
                   (getf type-def :type-roles '("admin")))))
    ;; TODO: Documentation. Be very careful when explicitly adding roles to a
    ;;       type, because if the role doesn't exist, it will be created here
    ;;       with full permissions and associated with the type.
    (loop
      with default-permissions = '("create" "read" "update" "delete")
      for role-spec in roles
      for role = (if (stringp role-spec) role-spec (car role-spec))
      for permissions = (if (stringp role-spec)
                          default-permissions
                          (cdr role-spec))
      when (not (a:get-id *rbac* "roles" role))
      do (a:add-role *rbac* role :permissions permissions))
    (validate-tree model type-key tree is-leaf parent-type fs-backed)
    (let ((fields-with-path (mark-path-field type-key fs-backed fields))
          (user-setting (getf type-def :user-setting)))
      (add-to-plist
        type-def
        (append
          (list
            :internal internal
            :create create
            :type-roles roles
            :table-name table-name
            :fields fields-with-path
            :tree tree
            :is-leaf is-leaf
            :parent-type parent-type
            :fs-backed fs-backed
            :user-setting user-setting
            :suppress-roles
              (or (getf type-def :suppress-roles) user-setting))
          ;; Compiled lifecycle hooks override raw values on type-def
          (compile-lifecycle-hooks model type-key))))))

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
            (equal (second key-path) :views)
            (equal key :tables)
            (listp sdef))
           t)
         ((and
            (equal (second key-path) :fields)
            (equal key :validations)
            (listp sdef))
           t)
         ((and
            (equal key :type-roles)
            (listp sdef)
            t))
         ((and
            (member (second key-path) '(:list-form :update-form :add-form))
            (equal key :fields))
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
    for type-def in (cdr model) by #'cddr
    for built-in = (getf type-def :built-in)
    for fields = (getf type-def :fields)
    for joiner = (getf type-def :is-joiner)
    for table-name = (table-name type-key built-in)
    for views = (enrich-views model type-key)
    for insert-sql = (unless joiner (insert-sql model type-key))
    for update-sql = (unless joiner (update-sql model type-key))
    for delete-sql = (unless joiner (delete-sql model type-key))
    for search-sql = (unless joiner (search-sql model type-key))
    for create-table-sql = (create-table-sql table-name fields)
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
    for new-def = (add-to-plist type-def (list :fields fields))
    appending (list type-key new-def)))

(defun compile-model (model)
  (let* ((model-1 (stage-1 model))
          (model-2 (stage-2 model-1))
          (model-3 (stage-3 model-2)))
    model-3))

(defun add-system-user-settings ()
  (loop
    for user in (list "admin" "guest")
    for setting-id = (be-value-id :settings :user user "admin")
    unless setting-id do
    (add-user-settings
      :settings
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
      (add-type-roles)
      (add-system-user-settings)
      (add-root-fs-resources)
      (start-web-server)
      (return summary)))
  (:method ((file string))
    "Accepts a file name (no path and no extension), computes the path of the
file by prepending the model directory path to the file name, adds the extension
'.lisp', reads the model from that file, and sets that model with SET-MODEL. For
example, for the string `todos`, this function will compute the file path
`/path/to/app/models/todos.lisp`, read that file, and set the model to the value
of that file."
    (let ((path (u:join-paths
                  *package-root*
                  "models"
                  (format nil "~a.lisp" file))))
      (with-open-file (in path)
        (set-model (cadr (read in))))))
  (:method ((file null))
    (set-model "default-model"))
  (:documentation ":public: Sets the model to the given plist. If given a
string instead of a plist, resolves the string to a file in the `models`
directory, loads the plist from there, and then sets the model that
plist. Setting the model involves compiling the model into an enriched model
that includes compiled functions (machine code), generated parameterized SQL, as
well as maps, other data structures, and settings that Data UI can use to
efficiently instantiate and support the application described by MODEL."))

(defun reset-to-model (model)
  ":public: Resets the database (drop all records from all tables, drop all tables associated with user-defined types), then call SET-MODEL with MODEL."
  (reset-database)
  (set-model model))

(defun create-tables ()
  (loop with m = *compiled-model*
    for type-key in m by #'cddr
    for table-name = (u:tree-get m type-key :table-name)
    for table = (u:tree-get m type-key :create-table-sql :table)
    for trigger = (u:tree-get m type-key :create-table-sql :trigger)
    for index = (u:tree-get m type-key :create-table-sql :index)
    unless (a:with-rbac (*rbac*)
             (a:rbac-query
               (list
                 "select 1 from information_schema.tables where table_name = $1"
                 table-name)
               :single))
    do
    (a:with-rbac (*rbac*)
      (db:query table)
      (db:query trigger)
      (when index (db:query index)))
    (pl:pdebug :in "create-tables" :state "added table and triggers"
      :table table-name :type-key type-key)))

(defun type-resource-name (type-key)
  (format nil "type-~(~a~)" type-key))

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
        (valid-top-level-value key value "^[a-z][-a-z0-9]*"))
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
          :predicates (list #'u:plistp))))))

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
