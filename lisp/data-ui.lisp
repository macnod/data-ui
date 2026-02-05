(in-package :data-ui)

;; Database
(defparameter *db-host* (u:getenv "DB_HOST" :default "localhost")
  "The database host. Retrieved from environment variable DB_HOST. Defaults to
'localhost'.")
(defparameter *db-port* (u:getenv "DB_PORT" :default 5432 :type :integer)
  "The database port. Retrieved from environment variable DB_PORT. Defaults to
5432.")
(defparameter *db-name* (u:getenv "DB_NAME" :default "dataui")
  "The name of the database. Retrieved from environment variable DB_NAME.
Defaults to 'dataui'.")
(defparameter *db-user* (u:getenv "DB_USER" :default "dataui")
  "The user name for connecting to the database. Retrieved from environment
variable DB_USER. Defaults to 'dataui'.")
(defparameter *db-password* (u:getenv "DB_PASSWORD"
                              :default "dataui-password")
  "The password for connecting to the database. Retrieved from environment
variable DB_PASSWORD. Default to 'dataui-password'.")

;; Admin
(defparameter *admin-password* (u:getenv "ADMIN_PASSWORD"
                                 :default "admin-password-1"))

(defparameter *web-directory* (u:getenv "WEB_DIRECTORY"
                               :default "/app/web"))
(defparameter *favicon* (u:join-paths *web-directory* "favicon.ico"))

;; HTTP and Swank servers
(defparameter *http-port* (u:getenv "HTTP_PORT" :default 8080 :type :integer))
(defparameter *document-root*
  (let* ((dir (u:getenv "DOCUMENT_ROOT"
               :default "/app/shared-files/"))
          (normalized (probe-file dir)))
    (format nil "~a" normalized)))
(defparameter *temp-directory* (u:getenv "FS_TEMP_DIRECTORY"
                                 :default "/app/temp-files/"))
(defparameter *swank-port* (u:getenv "SWANK_PORT" :default 4005 :type :integer))

;; Logs
(defparameter *log-file* (or (u:getenv "LOG_FILE") *standard-output*))
(defparameter *log-severity-threshold*
  (intern (string-upcase (or (u:getenv "LOG_SEVERITY") "DEBUG")) :keyword))
(defparameter *log-suppress-health*
  (u:getenv "LOG_SUPPRESS_HEALTH" :type :boolean :default t))

;; Environment
(defparameter *version* (u:getenv "DATAUI_VERSION" :default "0.0"))
(defparameter *environment*
  (u:getenv "DATAUI_ENVIRONMENT" :default "unknown"))

;; Other
(defparameter *http-server* nil)
(defparameter *swank-server* nil)
(defparameter *all-results-page-size* 10000)

;; Defaults
(defparameter *default-file-permissions* '("create" "read" "update" "delete"))
(defparameter *default-directory-permissions*
  '("create" "read" "update" "delete" "list"))

;; Resource definitions
(defparameter *valid-type-keys*
  '(:anti-patterns
     :base
     :enable
     :patterns
     :fields
     :fs-backed))
(defparameter *valid-field-keys*
  '(:anti-patterns
     :default
     :name
     :patterns
     :reference
     :required
     :type
     :unique))
(defparameter *valid-field-types*
  '(:float
     :integer
     :text
     :timestamp))
(defparameter *valid-descriptor-keys*
  '(:exists
     :fs-backed
     :fs-entry-exists
     :fs-storage
     :resource-exists
     :resource-id
     :resource-name
     :resource-name-field
     :resource-table
     :rt-entry-exists))
(defparameter *valid-fs-storage-keys*
  '(:exists
     :file-name-only
     :is-directory
     :is-file
     :leaf-directory
     :logical-path
     :logical-path-only
     :physical-path
     :physical-path-only))
(defparameter *resource-types*
  ;; Tables where :base is t already exist in the RBAC system, so they won't be
  ;; created by this system. Tables where :base is nil will be created by this
  ;; system. Also, users of data-ui can add resources of types where :base is
  ;; nil, but not of types where base is t. Resources of base types are managed
  ;; entirely by the RBAC system, via the RBAC API. Thus, for example, adding a
  ;; user follows a different code path than adding a file, setting, or
  ;; directory.
  '(:users (:enable t :base t :fs-backed nil)
     :resources (:enable t :base t :fs-backed nil)
     ;; Because this is not a base type, its table will be named with the rt_
     ;; prefix: rt_directories. The automatically-created field
     ;; rt_directories.directory_name will hold strings consisting of the prefix
     ;; 'directory:' followed by the logical path of the directory, i.e.
     ;; "directory:/a/b/".  The resources.resource_name field will contain the
     ;; same value.
     :directories (:enable t :fs-backed t :fields nil
                    :patterns ("^/[-a-zA-Z0-9_ @./]+/$|^/$")
                    :anti-patterns ("//" "/[- @]| /" " $"))
     :files (:enable t :fs-backed t :fields nil
              :patterns ("^/[-a-zA-Z0-9_. @/]+$")
              :anti-patterns ("//" "/$" "/[- @]| /" " $"))
     :settings (:enable t :fs-backed nil
                 :patterns ("^[a-zA-Z][-a-zA-Z0-9]*$")
                 :anti-patterns ("[-]$")
                 :fields
                 ;; The automaticall-created rt_settings.setting_name field will
                 ;; consist of the prefix 'setting:' followed by the user name
                 ;; and setting name, separated by a /, e.g.,
                 ;; "setting:alice/dark-mode". As usual, the
                 ;; resources.resource_name field will contain the same value.
                 ((:reference :users)
                   (:name "setting_value" :type :text :default "NIL")))))

;; Connect to the database
(defparameter *rbac* nil)

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

(defun immutable-user-roles (user-name)
  (list "logged-in" "public" (a:exclusive-role-for user-name)))

(defun validate-resource-types (&optional (types *resource-types*))
  "Check that resource types are well-formed."
  (loop for type-key in (u:plist-keys types)
    for type-keys = (u:plist-keys (getf types type-key))
    for fields = (u:tree-get types type-key :fields)
    unless type-keys do (error "Resource type ~a is not defined." type-key)
    unless (u:has *valid-type-keys* type-keys)
    do (error "Invalid resource type keys in type ~(~a~): ~{~(~a~)~^, ~}."
         type-key
	       (set-difference type-keys *valid-type-keys* :test 'eql))
    do (loop for field in fields do
         (validate-field types type-key field))))

(defun validate-resource-descriptor (descriptor)
  "Check that RESOURCE-DESCRIPTOR is valid."
  (let ((missing (set-difference *valid-descriptor-keys*
                          (u:plist-keys descriptor)
                          :test 'eql)))
    (when missing
      (error "Resource descriptor key~p missing: ~{~(~a~)~^, ~}."
        (length missing) missing)))
  (when (getf descriptor :fs-backed)
    (let ((missing (set-difference *valid-fs-storage-keys*
                     (u:plist-keys (getf descriptor :fs-storage)))))
      (when missing
        (error "Descriptor's fs-storage key~p missing: ~{~(~a~)~^, ~}."
          (length missing) missing))))
  t)

(defun validate-resource-descriptor-key (key)
  (unless (u:has *valid-descriptor-keys* key)
    (error "Invalid resource descriptor key: ~a." key)))

(defun validate-fs-storage-key (key)
  (unless (u:has *valid-fs-storage-keys* key)
    (error "Invalid fs-storage key: ~a." key)))

(defun descriptor-get (resource-descriptor &rest keys)
  (validate-resource-descriptor resource-descriptor)
  (unless (member (length keys) '(1 2))
    (error "descriptor-get only supports one or two keys."))
  (validate-resource-descriptor-key (car keys))
  (validate-fs-storage-key (cadr keys)))

(defun type-strings (&optional (types *resource-types*))
  (mapcar
    (lambda (k) (format nil "~(~a~)" k))
    (type-keywords types)))

(defun type-keywords (&optional (types *resource-types*))
  (u:plist-keys types))

(defun table-name (resource-name &key (types *resource-types*))
  (let* ((type (car (re:split ":" resource-name))))
    (cond
      ((member type (type-strings (non-base-types types)) :test 'equal)
        (format nil "rt_~a" type))
      ((member type (type-strings (base-types types)) :test 'equal)
        (format nil "~a" type))
      (t (error "Unknown resource type '~a' in resource name '~a'."
           type resource-name)))))

(defun name-field (resource-name &key (types *resource-types*))
  (let* ((type (car (re:split ":" resource-name))))
    (unless (u:has (type-strings types) type)
      (error "Unknown resource type '~a'" type))
    (format nil "~a_name" (u:singular type))))

(defun resource-id (resource-name)
  (when *rbac*
    (a:get-id *rbac* "resources" resource-name)))

(defun resource-exists (type-key name &optional (types *resource-types*))
  (when *rbac*
    (let* ((resource-name (resource-name type-key name))
            (table (table-name resource-name))
            (field (name-field resource-name))
            (fs-backed (u:tree-get types type-key :fs-backed))
            (physical-path (when fs-backed
                             (format nil "~a"
                               (or
                                 (probe-file name)
                                 (probe-file
                                   (u:join-paths *document-root* name))))))
            (sql (format nil "select 1 from ~a where ~a = $1" table field)))
      (pl:pdebug :in "resource-exists"
        :type type-key
        :name name
        :resource-name resource-name
        :table table
        :field field
        :fs-backed fs-backed
        :physical-path physical-path)
      (and
        (resource-id resource-name)
        (a:with-rbac (*rbac*) (db:query sql name :single))
        (or
          (not fs-backed)
          (and physical-path
            (if (u:ends-with name "/")
              (u:directory-exists-p physical-path)
              (u:file-exists-p physical-path))))))))

(defun probe (fs-backed name-only root)
  (when fs-backed
    (if (equal (format nil "~a" name-only) "/")
      (probe-file root)
      (or
        (probe-file name-only)
        (probe-file (u:join-paths root name-only))))))

(defun sprobe (fs-backed probe name-only root)
  (when fs-backed
    (format nil "~a"
      (or probe
        (if (u:starts-with (format nil "~a" name-only) root)
          (format nil "~a" name-only)
          (format nil "~a" (u:join-paths root name-only)))))))

(defun spath (fs-backed probe sprobe root)
  (when fs-backed
    (if probe
      sprobe
      (if (and
            (>= (length sprobe) (length root))
            (string= root sprobe :end2 (length root)))
        (u:join-paths root (subseq sprobe (length root)))
        sprobe))))

(defun make-resource-descriptor (type name-only &key references keys)
  (validate-type-keyword type)
  (validate-value-pattern type (keyword-to-name-field type)
    (resource-name type name-only :references references))
  (when keys
    (validate-resource-descriptor-key (car keys))
    (when (cadr keys)
      (validate-fs-storage-key (cadr keys))))
  (when (> (length keys) 2) (error "Too many keys."))
  (let* ((root *document-root*)
          (fs-backed (u:tree-get *resource-types* type :fs-backed))
          (probe (when fs-backed (probe fs-backed name-only root)))
          (sprobe (when fs-backed (sprobe fs-backed probe name-only root)))
          (spath (when fs-backed (spath fs-backed probe sprobe root)))
          (is-directory (when fs-backed (is-directory type name-only)))
          (is-file (when fs-backed (is-file type name-only)))
          (physical-path (when fs-backed (physical-path type name-only)))
          (logical-path (when fs-backed (logical-path type name-only)))
          (resource-name (resource-name type name-only :references references))
          (resource-id (resource-id resource-name))
          (resource-table (table-name resource-name))
          (name-field (name-field resource-name))
          (resource-exists (when (and *rbac* (resource-id resource-name)) t))
          (rt-entry-query (format nil "select 1 from ~a where ~a = $1"
                            resource-table name-field))
          (rt-entry-exists (when (and
                                   '*rbac*
                                   (a:with-rbac (*rbac*)
                                     (db:query rt-entry-query resource-name
                                       :single)))
                             t))
          (fs-entry-exists (when fs-backed
                             (if is-directory
                               (u:directory-exists-p physical-path)
                               (u:file-exists-p physical-path))))
          (exists (and resource-exists rt-entry-exists
                    (or (not fs-backed) fs-entry-exists)))
          (result (list
                    :resource-id resource-id
                    :resource-name resource-name
                    :resource-table resource-table
                    :resource-name-field name-field
                    :resource-exists resource-exists
                    :rt-entry-exists rt-entry-exists
                    :fs-entry-exists fs-entry-exists
                    :exists exists
                    :fs-backed fs-backed
                    :fs-storage
                    (when fs-backed
                      (list
                        :is-directory is-directory
                        :is-file is-file
                        :logical-path logical-path
                        :physical-path physical-path
                        :file-name-only (unless is-directory
                                          (u:filename-only spath))
                        :leaf-directory (u:leaf-directory-only
                                          (if is-directory
                                            logical-path
                                            (u:path-only logical-path)))
                        :logical-path-only (if is-directory
                                             logical-path
                                             (u:path-only logical-path))
                        :physical-path-only (if is-directory
                                              physical-path
                                              (u:path-only physical-path))
                        :exists fs-entry-exists)))))
    (if keys (apply #'u:tree-get (cons result keys)) result)))

;;
;; BEGIN Database initialization
;;

(defun to-sql-identifier (keyword &key (format-string "~a") singular)
  (let ((s (format nil "~(~a~)" keyword)))
    (format nil format-string
      (re:regex-replace-all
        "-"
        (if singular (u:singular s) s)
        "_"))))

(defun keyword-to-table-name (keyword)
  (let ((format-string (if (member keyword (u:plist-keys (base-types)))
                            "~a"
                            "rt_~a")))
    (to-sql-identifier keyword :format-string format-string)))

(defun keyword-to-table-reference (keyword)
  (to-sql-identifier keyword :format-string "~a_id" :singular t))

(defun keyword-to-name-field (keyword)
  (to-sql-identifier keyword :format-string "~a_name" :singular t))

(defun updated-at-trigger-sql-template ()
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
")

(defun filter-types (types predicate)
  (loop for type in (u:plist-keys types)
    for type-definition = (getf types type)
    for flag = (funcall predicate type-definition)
    when flag append (list type type-definition)))

(defun base-types (&optional (types *resource-types*))
  (filter-types types (lambda (v) (getf v :base))))

(defun non-base-types (types)
  (filter-types types (lambda (v) (not (getf v :base)))))

(defun enabled-types (types)
  (filter-types types (lambda (v) (getf v :enable))))

(defun create-resource-tables-sql (types)
  (loop
    with base-types = (base-types types)
    and non-base-types = (non-base-types types)
    and defined-types = (enabled-types types)
    and trigger-sql = (updated-at-trigger-sql-template)
    for type-key in (u:plist-keys non-base-types)
    for type-definition = (getf types type-key)
    for enabled = (getf type-definition :enabled)
    for fields = (getf type-definition :fields)
    for table = (keyword-to-table-name type-key)
    for updated-at-trigger = (format nil trigger-sql table table table table)
    for field-definitions = (field-definitions defined-types type-key fields)
    appending
    (list
      (format nil "~%create table if not exists ~a (~%    ~{~a~^,~%    ~}~%)~%"
        table field-definitions)
      updated-at-trigger)))

(defun name-field-present (name-field fields)
  (some (lambda (f) (equal (getf f :name) name-field)) fields))

(defun base-field-definitions ()
  (list
    "id uuid primary key references resources(id) on delete cascade"
    "created_at timestamp not null default now()"
    "updated_at timestamp not null default now()"))

(defun default-definitions (name-field fields)
  (if (name-field-present name-field fields)
    (base-field-definitions)
    (cons (format nil "~a text not null unique" name-field)
      (base-field-definitions))))

(defun field-definitions (defined-types resource-type fields)
  (loop
    with base-types = (base-types defined-types)
    and name-field = (keyword-to-name-field resource-type)
    for field in fields
    for validation = (validate-field defined-types resource-type field)
    for reference = (getf field :reference)
    for name = (getf field :name)
    for type-key = (getf field :type :text)
    for type = (format nil "~(~a~)" type-key)
    for unique = (when (getf field :unique) "unique")
    for default-value = (getf field :default)
    for default = (when default-value
                    (format nil "default ~a"
                      (if (eql type-key :text)
                        (format nil "'~a'" default-value)
                        default-value)))
    for required-value = (getf field :required)
    for required = (when required-value "not null")
    for reference-row = (when reference
                          (format nil
                            "~a uuid not null references ~a(id) on delete cascade"
                            (keyword-to-table-reference reference)
                            (keyword-to-table-name reference)))
    for regular-row = (unless reference
                        (format nil "~{~a~^ ~}"
                          (remove-if-not
                            #'identity
                            (list name type unique default required))))
    collect (if reference reference-row regular-row)
    into definitions
    finally
    (return (append
              definitions
              (default-definitions name-field fields)))))

(defun validate-field (defined-types resource-type field)
  (let ((reference (getf field :reference)))
    (if reference
      (validate-reference-field defined-types resource-type reference field)
      (validate-regular-field defined-types resource-type field))))

(defun validate-regular-field (defined-types resource-type field)
  (let* ((unknown-keys (set-difference (u:plist-keys field) *valid-field-keys*
                         :test 'eql))
          (name-field (keyword-to-name-field resource-type))
          (name (getf field :name))
          (is-name-field (equal name name-field))
          (type (getf field :type :text))
          (patterns (u:tree-get defined-types resource-type :patterns))
          (anti-patterns (u:tree-get defined-types resource-type :anti-patterns))
          (field-patterns (or (getf field :patterns)
                            (when is-name-field patterns)))
          (field-anti-patterns (or (getf field :anti-patterns)
                                 (when is-name-field anti-patterns)))
          (default (getf field :default))
          (required (getf field :required))
          (unique (getf field :unique))
          (errors nil))
    (unless (and (stringp name) (not (zerop (length name))))
      (push (format nil "All non-reference fields in ~a must have a name."
              resource-type)
        errors))
    (when unknown-keys
      (push (format nil
              "~@(~r~) unknown field key~:p in field ~a.~a: ~{~a~^, ~}."
              (length unknown-keys) resource-type name unknown-keys)
        errors))
    (unless (every #'stringp field-patterns)
      (push (format nil "All patterns in field ~a.~a must be strings."
              resource-type name)
        errors))
    (unless (every #'stringp field-anti-patterns)
      (push (format nil "All anti-patterns in field ~a.~a must be strings."
              resource-type name)
        errors))
    (unless (member type *valid-field-types*)
      (push (format nil "Invalid field type '~a' in field ~a.~a."
              type resource-type name)
        errors))
    (when (and default required)
      (push (format nil "Field ~a.~a is required and has a default."
              resource-type name)
        errors))
    (unless (member required '(t nil))
      (push (format nil
              "Field ~a.~a has an invalid value for :required: ~a."
              resource-type name required)
        errors))
    (unless (member unique '(t nil))
      (push (format nil "Field ~a.~a has an invalid value for :unique: ~a."
              resource-type name unique)
        errors))
    (unless (validate-field-default-type field)
      (push (format nil "Invalid type for default value in field ~a.~a."
              resource-type name)
        errors))
    (when errors (error "~{~a~^ ~}" errors))))

(defun validate-field-default-type (field)
  (let ((type (getf field :type :text))
         (default (getf field :default)))
    (cond
      ((not default) t)
      ((and (equal type :text) (stringp default)) t)
      ((and (equal type :float) (integerp default)) t)
      ((and (equal type :float) (floatp default)) t)
      ((and (equal type :integer) (integerp default)) t)
      ((and
         (equal type :timestamp)
         (re:scan
           "^[12][09]\\d{2}-[0-3]\\d-[0-3]\\dT[12]\\d:[0-5]\\d:[0-5]\\d$"
           default))
        t)
      (t nil))))

(defun validate-reference-field (defined-types resource-type reference field)
  (let (errors)
    (unless (member reference defined-types)
      (push (format nil "Resource not among defined types: ~a." reference)
        errors))
    (unless (= (length (u:plist-keys field)) 1)
      (push (format nil
              "Reference field in ~a cannot have other field keys"
              resource-type)
        errors))
    (when errors (error "~{~a~^ ~}" errors))))

;;
;; END Database initialization
;;

;;
;; BEGIN Support functions
;;

(defun compute-filter (type)
  ":private: Computes the filter key for filtering by TYPE, which can be :file
or :directory. Returns nil if type is nil, which means that no filtering is to
be done. Otherwise, returns :is-directory or :is-file, depending on the value of
TYPE."
  (cond
    ((eql type :directory) :is-directory)
    ((eql type :file) :is-file)
    ((null type) nil)
    (t (error "Unknown type ~a" type))))

(defun filter-by-type (resources type)
  ":private: Filters RESOURCES by TYPE, which can be :file, :directory, or nil.
Each element of RESOURCES consists of a resource descriptor plist. Returns the
filtered list."
  (let ((filter (compute-filter type)))
    (if filter
      (remove-if-not
        (lambda (p) (getf p filter))
        resources)
      resources)))

(defun exclude-resource-with-physical-path (resources path)
  ":private: Excludes from RESOURCES any resource whose :physical-path is PATH.
Because a resouce's physical path is unique, this function removes at most one
resource from RESOURCES."
  (remove-if
    (lambda (p)
      (equal (u:tree-get p :fs-storage :physical-path) path))
    resources))

(defun resource-results (resource-descriptors keys)
  ":private: Given a list of RESOURCE-DESCRIPTORS (resource descriptor plists)
and a list of KEYS, returns the results according to KEYS. If KEYS has more than
one key, the result is a list of lists, where each inner list contains the
values for the given KEYS for each resource descriptor. If KEYS has a single
key, then the result is a list of the values for that key for each resource
descriptor.  Finally, if KEYS is nil, the result is simply RESOURCE-DESCRIPTORS.
A KEY consists of a list of one or two keys: the first key is a resource
descriptor key and the second key (required only if the first key is
:FS-STORAGE) is an :FS-STORAGE key."
  (let* ((key-count (length keys)))
    (cond
      ((> key-count 1)
        (loop for resource in resource-descriptors collect
          (loop for key-set in keys
            for key-list = (if (listp key-set) key-set (list key-set))
            for params = (cons resource key-list)
            collect (apply #'u:tree-get params))))
      ((= key-count 1)
        (loop with key-set = (car keys)
          for resource in resource-descriptors
          for params = (cons resource key-set)
          collect (apply #'u:tree-get params)))
      (t resource-descriptors))))

(defun list-directory-recursively (path &key keys type exclude-path)
  ":public: Recursively list the contents of the directory given by PATH, which
can be a string or a PATHNAME. The result, by default, consists of a list of
resource descriptors of all the files and directories that exist at any level
below PATH, sorted by descending length of logical path. This allows the caller
to delete all directories and files in the list in the proper order, such that a
directory is always empty when it's deleted. A resource descriptor consists of a
plist that describes the resource.

The KEYS value, causes the result to consist of a list of lists instead of a
list of resource descriptors. Each inner list contains the values associated
with the given KEYS for each resource descriptor, in the same order as the
associated keys provided in KEYS. If KEYS consists of a single key, then the
result consists of a list of the values associated with that key in each
resource descriptor. A KEY consists of a list of one or two keys: the first key
is a resource descriptor key and the second key (required only if the first key is
:FS-STORAGE) is an :FS-STORAGE key.

TYPE can be :file or :directory. When given, it filters the result to include
elements that are of that type only. Otherwise, the result includes both files
and directories."
  (let* ((path-rd (make-resource-descriptor :directories path))
          (path-abs (u:tree-get path-rd :fs-storage :physical-path))
          paths)
    (cl-fad:walk-directory
      path-abs
      (lambda (p) (push p paths))
      :directories t)
    (let* ((all-resources (mapcar
                            (lambda (p)
                              (if (u:ends-with (format nil "~a" p) "/")
                                (make-resource-descriptor :directories p)
                                (make-resource-descriptor :files p)))
                            paths))
            (resources (if exclude-path
                         (exclude-resource-with-physical-path
                           all-resources path-abs)
                         all-resources))
            (filtered (filter-by-type resources type))
            (sorted (sort filtered #'>
                      :key (lambda (d)
                             (length
                               (u:tree-get d :fs-storage :logical-path))))))
      (resource-results sorted keys))))

(defun list-directory (path &key keys type)
  ":public: List the contents of the directory given by PATH, which can be a
string or a PATHNAME. Only items that are direct children of PATH are included
in the result. The result, by default, consists of a list of resource
descriptors of all the files and directories that exist directly under PATH,
sorted by resource name. A resource descriptor consists of a plist that
describes the resource.

The KEYS value, always a list, when provided, causes the result to consist of a
list of lists instead of a list of resource descriptors. Each inner list
contains the values associated with the given KEYS for each resource descriptor,
in the same order as the associated keys provided in KEYS. If KEYS consists of a
single key, then the result consists of a list of the values associated with
that key in each resource descriptor.

TYPE can be :file or :directory. When given, it filters the result to include
elements that are of that type only. Otherwise, the result includes both files
and directories."
  (let* ((path-rd (make-resource-descriptor :directory path))
          (path-abs (u:tree-get path-rd :fs-storage :physical-path))
          (paths (cl-fad:list-directory path-abs))
          (resources (mapcar
                       (lambda (p)
                         (if (u:ends-with (format nil "~a" p) "/")
                            (make-resource-descriptor :directory p)
                            (make-resource-descriptor :file p)))
                       paths))
          (filtered (filter-by-type resources type))
          (sorted (sort filtered #'string<
                    :key (lambda (d) (getf d :resource-name)))))
    (resource-results sorted keys)))

;;
;; END Support functions
;;

;;
;; BEGIN Database operations
;;

(defun validate-type-string (type-string &optional (types *resource-types*))
  (unless (u:has (type-strings types) type-string)
    (error "Unknown resource type: ~a." type-string)))

(defun validate-type-keyword (type-key)
  (unless (u:has (u:plist-keys *resource-types*) type-key)
    (error "Unknown resource type: ~a." type-key)))

(defun name-and-base-fields (type)
  (let* ((name-field (keyword-to-name-field type))
          (base-fields (u:tree-get *resource-types* type :fields)))
    (if (some
          (lambda (f) (equal (getf f :name) name-field))
          base-fields)
      base-fields
      (cons
        (list :name name-field :type :text :unique t :required t)
        base-fields))))

(defun select-text-field (type field-name)
  (let* ((fields (name-and-base-fields type)))
    (car (remove-if-not
           (lambda (f)
             (and
               (equal (getf f :type) :text)
               (equal (getf f :name) field-name)))
           fields))))

(defun strip-prefix-and-references (field-value)
  (let* ((parts (re:split ":" (format nil "~a" field-value)))
          (type-string (car parts))
          (type-key (intern (format nil "~:@(~a~)" type-string) :keyword))
          (references (reference-list type-key)))
    (if references
      (let ((limit (1+ (length references)))
             (refs-in-value (re:split "/" (cadr parts))))
        (when (< (length refs-in-value) limit)
          (error "Value '~a' is missing reference parts." field-value))
        (when (some
                (lambda (r) (zerop (length r)))
                refs-in-value)
          (error "Value '~a' has empty reference parts." field-value))
        (car
          (last
            (re:split
              "/"
              (cadr (re:split ":" (format nil "~a" field-value))) :limit limit))))
      (let ((value (cadr parts)))
        (when (zerop (length value))
          (error "Value '~a' has empty value part." field-value))
        value))))

(defun validate-value-pattern (type field-name field-value &key
                                (types *resource-types*))
  (let* ((g-patterns (u:tree-get types type :patterns))
          (g-anti-patterns (u:tree-get types type :anti-patterns))
          (field (select-text-field type field-name))
          (svalue (strip-prefix-and-references field-value))
          (patterns (when field
                      (or (getf field :patterns) g-patterns)))
          (anti-patterns (when field
                           (or (getf field :anti-patterns) g-anti-patterns))))
    (pl:pdebug :in "validate-value-pattern"
      :type type
      :field-name field-name
      :field-value svalue
      :svalue svalue
      :patterns patterns
      :anti-patterns anti-patterns)
    (unless (re:scan ".+:.+" (format nil "~a" field-value))
      (error "Field value '~a' missing type prefix." field-value))
    (let* ((unmatched-patterns
             (remove-if
               (lambda (p) (re:scan p svalue))
               patterns))
            (p-count (length unmatched-patterns))
            (matched-anti-patterns
              (remove-if
                (lambda (a) (not (re:scan a svalue)))
                anti-patterns))
            (a-count (length matched-anti-patterns)))
      (when unmatched-patterns
        (pl:pdebug :in "validate-value-pattern"
          :status "unmatched patterns"
          :field-name field-name
          :field-value svalue
          :unmatched-patterns unmatched-patterns)
        (error "Field ~a value '~a' does not match pattern~p: ~{~a~^, ~}."
          field-name svalue p-count unmatched-patterns))
      (when matched-anti-patterns
        (pl:pdebug :in "validate-value-pattern"
          :status "matched anti-patterns"
          :field-name field-name
          :field-value svalue
          :matched-anti-patterns matched-anti-patterns)
        (error "Field ~a value '~a' matches anti-pattern~p: ~{~a~^, ~}."
          field-name svalue a-count matched-anti-patterns)))))

(defun validate-roles (roles)
  (let ((unknown-roles (set-difference roles
                         (a:list-role-names *rbac*)
                         :test 'string=)))
    (when unknown-roles
      (error "Unknown role~p: ~{~a~^, ~}."
        (length unknown-roles) unknown-roles))))

(defun parent-directory (resource-descriptor)
  (when (u:tree-get resource-descriptor :fs-backed)
    (u:path-parent
      (u:tree-get resource-descriptor
        :fs-storage :physical-path-only))))

(defun validate-directory-structure (type-key name references)
  (let* ((rd (make-resource-descriptor type-key name :references references))
          (is-root (equal (getf rd :resource-name) "directories:/"))
          (fs-backed (getf rd :fs-backed))
          (is-directory (u:tree-get rd :fs-storage :is-directory))
          (parent-dir (when fs-backed
                        (if (u:tree-get rd :fs-storage :is-directory)
                            (parent-directory rd)
                            (u:tree-get rd :fs-storage :physical-path-only))))
          (exists (and fs-backed
                    (getf rd :fs-backend)
                    (u:tree-get rd :resource-exists)
                    (u:tree-get rd :rt-entry-exists)
                    (u:tree-get rd :fs-storage :exists))))
    (pl:pdebug :in "validate-directory-structure"
      :type-key type-key
      :name name
      :fs-backed fs-backed
      :is-directory is-directory
      :is-root is-root
      :parent-dir parent-dir
      :exists exists)
    (when (and
            fs-backed
            (not is-root)
            (not (u:directory-exists-p parent-dir)))
      (error "Parent directory does not exist for resource ~a: ~a."
        (u:tree-get rd :resource-name) parent-dir))
    (when (and fs-backed exists)
      (error "Resource ~a already exists at ~a."
        (getf rd :resource-name)
        (u:tree-get rd :fs-storage :physical-path)))))

(defun validate-source-file (type-key name-only source-file references)
  (let* ((rd (make-resource-descriptor type-key name-only :references references))
          (is-file (u:tree-get rd :fs-storage :is-file)))
    (when (equal type-key :files)
      (unless source-file
        (error "Source file is required for file resources."))
      (unless (and is-file (u:file-exists-p source-file))
        (error "Source file '~a' does not exist." source-file)))))

(defun validate-references-exist (type-key references)
  (when references
    (let* ((type-fields (u:tree-get *resource-types* type-key :fields))
            (type-ref-keys (loop for field in type-fields
                             when (u:tree-get field :reference)
                             collect (getf field :reference)))
            (reference-keys (u:plist-keys references))
            (missing-references (set-difference type-ref-keys reference-keys))
            (unknown-references (set-difference reference-keys type-ref-keys))
            errors)
      (unless (u:has (u:plist-keys *resource-types*) type-key)
        (error "Unknown resource type :~(~a~)." type-key))
      (when missing-references
        (push (format nil
                "Missing :~(~a~) reference~p: ~{:~(~a~)~^, ~}."
                type-key (length missing-references) missing-references)
          errors))
      (when unknown-references
        (push (format nil
                ":~(~a~) does not reference ~{:~(~a~)~^, ~}."
                type-key unknown-references)
          errors))
      (when errors (error "~{~a~^ ~}" errors))
      (loop
        for key in type-ref-keys
        for table = (keyword-to-table-name key)
        for field = (keyword-to-name-field key)
        for name = (getf references key)
        for sql = (format nil "select 1 from ~a where ~a = $1" table field)
        for exists = (when *rbac*
                       (a:with-rbac (*rbac*)
                         (db:query sql name :single)))
        unless exists do (error "Table ~a has no entry with ~a = ~a."
                           table field name)))))

(defun referenced-name-values (references)
  (loop for key in references by #'cddr
    for name in (cdr references) by #'cddr
    for table = (keyword-to-table-name key)
    for name-field = (keyword-to-name-field key)
    for id-field = (keyword-to-table-reference key)
    for sql = (format nil "select id from ~a where ~a = $1" table name-field)
    for id-value = (a:with-rbac (*rbac*) (db:query sql name :single))
    collect (cons id-field id-value)))

(defun logical-path (type-key name-only)
  (validate-type-keyword type-key)
  (let* ((root *document-root*)
          (drl (length root))
          (fs-backed (u:tree-get *resource-types* type-key :fs-backed))
          (spath (format nil "~a" name-only))
          (is-physical (is-physical type-key name-only))
          (relative (if is-physical
                      (format nil "/~a" (subseq spath drl))
                      spath)))
    (when fs-backed
      (if is-physical relative spath))))

(defun physical-path (type-key name-only)
  (validate-type-keyword type-key)
  (let* ((root *document-root*)
          (fs-backed (u:tree-get *resource-types* type-key :fs-backed))
          (spath (format nil "~a" name-only)))
    (when fs-backed
      (if (is-physical type-key name-only)
        spath
        (u:join-paths root spath)))))

(defun reference-list (type-key)
  (loop for field in (u:tree-get *resource-types* type-key :fields)
    for reference = (u:tree-get field :reference)
    when reference collect reference into refs
    finally (return (sort refs #'string<))))

(defun resource-name (type-key name-only &key references)
  (validate-type-keyword type-key)
  (validate-references-exist type-key references)
  (let* ((fs-backed (u:tree-get *resource-types* type-key :fs-backed))
          (resource-prefix (format nil "~(~a~)" type-key))
          (ref-values (loop for ref in (cdr references) by #'cddr
                        collect ref))
          (name (cond
                  (fs-backed (logical-path type-key name-only))
                  (ref-values (format nil "~{~a~^/~}/~a" ref-values name-only))
                  (t name-only)))
          (full-name (format nil "~a:~a" resource-prefix name)))
    (validate-value-pattern type-key (keyword-to-name-field type-key) full-name)
    full-name))

(defun is-file (type-key name-only)
  (validate-type-keyword type-key)
  (let ((fs-backed (u:tree-get *resource-types* type-key :fs-backed)))
    (when fs-backed
      (not (u:ends-with (format nil "~a" name-only) "/")))))

(defun is-directory (type-key name-only)
  (validate-type-keyword type-key)
  (let ((fs-backed (u:tree-get *resource-types* type-key :fs-backed)))
    (when fs-backed
      (u:ends-with (format nil "~a" name-only) "/"))))

(defun is-physical (type-key name-only)
  (validate-type-keyword type-key)
  (let* ((root *document-root*)
          (fs-backed (u:tree-get *resource-types* type-key :fs-backed))
          (spath (format nil "~a" name-only)))
    (when fs-backed
      (u:starts-with spath root))))

(defun named-values (id name other-named-values)
  (unless (consp id)
    (error "ID must be a (name . value) pair."))
  (unless (consp name)
    (error "NAME must be a (name . value) pair."))
  (unless (and (listp other-named-values)
            (every #'consp other-named-values)
            (every (lambda (nv)
                     (and (stringp (car nv)) (atom (cdr nv))))
              other-named-values))
    (error "OTHER-NAMED-VALUES must be a list of (name . value) pairs."))
  (let* ((named-values (append (list id name) other-named-values))
          (names (mapcar 'car named-values))
          (values (mapcar 'cdr named-values))
          (place-holders (loop for a from 1 to (length names)
                           collect (format nil "$~d" a))))
    (unless (every
              (lambda (v)
                (and (consp v) (stringp (car v)) (atom (cdr v))))
              named-values)
      (error "ID and NAME must be (name . value) pairs. OTHER-NAMED-VALUES must
be a list of such pairs."))
    (list names values place-holders)))

(defun references-user (type-key &optional (types *resource-types*))
  (let* ((fields (u:tree-get types type-key :fields)))
    (some (lambda (f) (equal (getf f :reference) :users)) fields)))

(defun add-resource (type-key name &key
                      (roles '("admin"))
                      references
                      other-named-values
                      source-file)
  (validate-type-keyword type-key)
  (validate-roles roles)
  (validate-value-pattern type-key (keyword-to-name-field type-key)
    (resource-name type-key name :references references))
  (validate-directory-structure type-key name references)
  (validate-source-file type-key name source-file references)
  (validate-references-exist type-key references)
  (let* ((id (a:add-resource *rbac*
               (resource-name type-key name :references references)
               :roles roles))
          (rd (when id (make-resource-descriptor type-key name
                         :references references))))
    (unless rd
      (error "Failed to add resource ~a." (resource-name type-key name)))
    (pl:pdebug :in "add-resource" :status "added resource"
      :id id
      :resource-id (getf rd :resource-id)
      :resource-name (getf rd :resource-name)
      :fs-backed (getf rd :fs-backed)
      :other-named-values other-named-values
      :source-fie source-file
      :roles roles)
    (when (u:tree-get rd :fs-backed)
      (ensure-directories-exist
        (u:tree-get rd :fs-storage :physical-path-only))
      (pl:pdebug :in "add-resource" :status "directory ensured"
        :physical-path (u:tree-get rd :fs-storage :physical-path-only))
      (when (u:tree-get rd :fs-storage :is-file)
        (u:copy-file source-file
          (u:tree-get rd :fs-storage :physical-path))
        (pl:pdebug :in "add-resource" :status "file copied")))
    (let* ((table-name (table-name (getf rd :resource-name)))
            (name-field (keyword-to-name-field type-key))
            (named-values (named-values
                            `("id" . ,(getf rd :resource-id))
                            `(,name-field . ,(getf rd :resource-name))
                            (append other-named-values
                              (referenced-name-values references))))
            (names (first named-values))
            (values (second named-values))
            (place-holders (third named-values))
            (sql (format nil "insert into ~a (~{~a~^, ~}) values (~{~a~^, ~})"
                   table-name names place-holders))
            (params (cons sql values)))
      (pl:pdebug :in "add-resource" :status "inserting record"
        :sql sql :params values)
      (a:with-rbac (*rbac*)
        (a:rbac-query params))
      (make-resource-descriptor type-key name :references references))))

(defun delete-directory-recursively (path)
  (loop for descriptor in (list-directory-recursively path)
    for resource-name = (u:tree-get descriptor :resource-name)
    for is-file = (u:tree-get descriptor :fs-storage :is-file)
    for physical-path = (u:tree-get descriptor
                          :fs-storage :physical-path)
    do (a:remove-resource *rbac* resource-name)
    when is-file do (delete-file physical-path)
    else do (cl-fad:delete-directory-and-files
              physical-path
              :if-does-not-exist :ignore)))

(defun delete-resource (resource-descriptor)
  (when (u:tree-get resource-descriptor :exists)
    (let* ((resource-name (u:tree-get resource-descriptor
                            :resource-name))
            (physical-path (u:tree-get resource-descriptor
                             :fs-storage :physical-path))
            (is-directory (u:tree-get resource-descriptor
                            :fs-storage :is-directory))
            (is-file (u:tree-get resource-descriptor
                       :fs-storage :is-file)))
      (cond
        (is-directory (delete-directory-recursively physical-path))
        (is-file
          (delete-file physical-path)
          (a:remove-resource *rbac* resource-name))
        (t (a:remove-resource *rbac* resource-name)))
      (pl:pdebug :in "delete-resource" :status "deleted resource"
        :resource-name resource-name))))

;;
;; END Database operations

;;
;; BEGIN Init
;;

(defun start-logging ()
  (when *log-file*
    (pl:make-log-stream "data-ui" *log-file*)))

(defun create-resource-tables (types)
  (pl:pdebug :in "create-resource-tables"
    :types (mapcar
             (lambda (s) (format nil "~(~a~)" s))
             (u:plist-keys (non-base-types types))))
  (loop for sql in (create-resource-tables-sql types)
    do (a:with-rbac (*rbac*) (db:query sql))))

(defun init-database ()
  (pl:pinfo :in "init-database"
    :host *db-host*
    :port *db-port*
    :db-name *db-name*
    :db-user *db-user*)
  (validate-resource-types)
  (setf a:*default-page-size* 10000)
  (setf *rbac* (make-instance 'a:rbac-pg
                 :db-host *db-host*
                 :db-port *db-port*
                 :db-name *db-name*
                 :db-user *db-user*
                 :db-password *db-password*))
  (let ((success (handler-case
                   (progn (a:initialize-database *rbac* *admin-password*) t)
                   (error (condition)
                     (pl:perror :in "init-database"
                       :status "failed to initialize database"
                       :condition (format nil "~a" condition))))))
    (pl:pinfo :in "init-database"
      :status (if success
                "initialize-database call succeeded"
                "initialize-database call failed"))
    (when success
      (create-resource-tables *resource-types*))
      (when (not (a:get-id *rbac* "permissions" "list"))
        (a:add-permission *rbac* "list"
          :description "List contents of a directory"))
      (when (not (a:get-id *rbac* "resources" (resource-name :directories "/")))
        (pl:pdebug :in "init-database" :status "adding root directory resource")
        (add-resource :directories "/" :roles '("public")))))

;; (defun start-web-server ()
;;   (setf h:*tmp-directory* *temp-directory*)

;;
;; END Init
;;
