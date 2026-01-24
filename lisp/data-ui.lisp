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
(defparameter *root-userid* nil)
(defparameter *directory-syncing* t)
(defparameter *resource-types* '(:file :directory))
(defparameter *all-results-page-size* 10000)

;; Defaults
(defparameter *default-file-permissions* '("create" "read" "update" "delete"))
(defparameter *default-directory-permissions*
  '("create" "read" "update" "delete" "list"))

;; Resource definitions
(defparameter *valid-field-keys* '(:name :type :default :required :unique
                                    :reference))
(defparameter *valid-field-types* '(:text :integer :float :timestamp))
(defparameter *resource-types*
  '(:users (:enable t :base t)
     :directories (:enable t :fields nil)
     :files (:enable t :fields ((:reference :directories)))
     :settings (:enable t
                 :fields
                 ((:reference :users)
                   ;; The system normally creates this field, setting_name,
                   ;; automatically, but we're defining it here explicitly
                   ;; because we don't want it to be unique, which is the
                   ;; default behavior for generated name fields.
                   (:name "setting_name" :type :text :required t)
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

(defun resource-name-for (resource-name)
  (unless (re:scan "^/" resource-name)
    (error "Invalid resource-name '~a'. Must start with /" resource-name))
  (let ((resource-type (if (re:scan "/" resource-name) :directory :file)))
    (format nil "~(~a~):~a" resource-type resource-name)))

(defun immutable-user-roles (user-name)
  (list "logged-in" "public" (a:exclusive-role-for user-name)))

(defun user-directory-permissions (user-name permission)
  ":private: Creates a hash table where the keys are directories to which
USER-NAME has PERMISSION access"
  (loop
    with table = (make-hash-table :test 'equal)
    with resources = (a:list-user-resource-names *rbac* user-name permission)
    for resource in resources
    for permissions = (a:list-user-resource-permission-names
                        *rbac* user-name resource)
    do (setf (gethash resource table) permissions)
    finally (return table)))

(defun list-user-directories-fs (user-name)
  (loop
    with table = (make-hash-table :test 'equal)
    with dirs-raw = (mapcar
                      (lambda (d) (format nil "~a" d))
                      (directory (format nil "~a**/" *document-root*)))
    with l = (1- (length (u:root-path dirs-raw)))
    with dirs-clean = (u:safe-sort (mapcar (lambda (dir) (subseq dir l)) dirs-raw))
    with directory-permissions = (user-directory-permissions user-name "read")
    for dir in dirs-clean
    for permissions = (gethash dir directory-permissions)
    when permissions
    collect (list :directory dir :permissions permissions)))

(defun clean-path (path)
  "Returns the path portion of PATH, which must be a string that starts with a
slash. If PATH points to a directory, then this function adds the trailing slash
if necessary. Otherwise, if PATH points to a file, this function removes the
file name and returns the path to the file with a trailing slash."
  (if (equal path "/")
    path
    (let* ((abs-path (u:join-paths *document-root* path))
            (path-only (if (eql (u:path-type abs-path) :directory)
                         path
                         (u:path-only path)))
            (clean-path (if (equal path-only "/")
                          "/"
                          (format nil "/~a/" (string-trim "/" path-only)))))
      (pl:pdebug :in "clean-path"
        :path-only path
        :clean-path clean-path)
      clean-path)))

(defun absolute-path (path)
  (u:join-paths *document-root* path))

(defun check-descriptor (resource-descriptor)
  (unless (u:plistp resource-descriptor)
    (error "Resource descriptor is not a plist."))
  (let* ((keys (u:plist-keys resource-descriptor))
          (expected '(:resource-name
                       :logical-path
                       :physical-path
                       :file-name-only
                       :leaf-directory
                       :logical-path-only
                       :physical-path-only
                       :exists-in-storage
                       :exists-in-database
                       :is-directory))
          (missing (set-difference expected keys)))
    (if missing
      (error "Resource descriptor is missing keys: ~{~a~^, ~}" missing))))

(defun resource-exists-in-storage (resource-descriptor)
  (if (getf resource-descriptor :is-directory)
    (u:directory-exists-p (getf resource-descriptor :absolute))
    (u:file-exists-p (getf resource-descriptor :absolute))))

(defun is-directory-p (resource-descriptor)
  (unless (getf resource-descriptor :is-directory) t))

(defun resource-id (resource-name)
  (let* ((parts (re:split ":" resource-name))
          (type (car parts))
          (value (cadr parts))
          (table (format nil "rt_~a" (u:plural type)))
          (field (format nil "~a_name" type))
          (sql (format nil "select id from ~a where ~a = $1" table field)))
    (pl:pdebug :in "resource-id"
      :resource-name resource-name
      :table table
      :field field
      :sql sql)
    (a:with-rbac (*rbac*)
      (db:query sql value :single))))

(defun make-resource-descriptor (path &optional key)
  (let* ((root *document-root*)
          (drl (length root))
          (probe (if (equal (format nil "~a" path) "/")
                   (probe-file root)
                   (or
                     (probe-file path)
                     (probe-file (u:join-paths root path)))))
          (sprobe (format nil "~a" (or probe
                                     (if (u:starts-with (format nil "~a" path) root)
                                        (format nil "~a" path)
                                       (format nil "~a"
                                         (u:join-paths root path))))))
          (spath (if probe
                   sprobe
                   (if (and
                         (>= (length sprobe) drl)
                         (string= root sprobe :end2 drl))
                     (u:join-paths root (subseq sprobe drl))
                     sprobe)))
          (is-physical (u:starts-with spath root))
          (is-directory (if probe
                          (equal (u:path-type spath) :directory)
                          (u:ends-with (format nil "~a" path) "/")))
          (is-file (if probe
                     (equal (u:path-type spath) :file)
                     (not (u:ends-with (format nil "~a" path) "/"))))
          (physical-path (if is-directory
                           spath
                           (re:regex-replace "/$" spath "")))
          (relative (if is-physical (subseq spath drl) spath))
          (logical-path (if is-physical
                          (format nil "/~a" relative)
                          spath))
          (resource-prefix (if is-directory "directory" "file"))
          (resource-name (format nil "~a:~a" resource-prefix logical-path))
          (result (list
                    :resource-name resource-name
                    :resource-id (when *rbac* (resource-id resource-name))
                    :logical-path logical-path
                    :physical-path physical-path
                    :file-name-only
                    (unless is-directory (u:filename-only spath))
                    :leaf-directory
                    (if is-directory
                      (u:leaf-directory-only logical-path)
                      (u:leaf-directory-only (u:path-only physical-path)))
                    :logical-path-only
                    (if is-directory logical-path (u:path-only logical-path))
                    :physical-path-only (if is-directory
                                          physical-path
                                          (u:path-only physical-path))
                    :exists-in-storage (if is-directory
                                         (u:directory-exists-p physical-path)
                                         (u:file-exists-p physical-path))
                    :exists-in-database (when (and *rbac*
                                                (resource-id resource-name))
                                          t)
                    :is-directory is-directory
                    :is-file is-file)))
    (pl:pdebug :in "make-resource-descriptor"
      :root root
      :drl drl
      :probe probe
      :sprobe sprobe
      :spath spath
      :is-physical is-physical
      :is-directory is-directory
      :is-file is-file
      :physical-path physical-path
      :relative relative
      :logical-path logical-path
      :resource-prefix resource-prefix
      :resource-name resource-name
      :resource-id (getf result :resource-id)
      :file-name-only (getf result :file-name-only)
      :leaf-directory (getf result :leaf-directory)
      :logical-path-only (getf result :logical-path-only)
      :physical-path-only (getf result :physical-path-only)
      :exists-in-storage (getf result :exists-in-storage)
      :exists-in-database (getf result :exists-in-database))
    (if key (getf result key) result)))

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

(defun keyword-to-table-name (keyword base-types)
  (let ((format-string (if (member keyword (u:plist-keys base-types))
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

(defun base-types (types)
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
    for type in (u:plist-keys non-base-types)
    for type-definition = (getf types type)
    for enabled = (getf type-definition :enabled)
    for fields = (getf type-definition :fields)
    for table = (keyword-to-table-name type base-types)
    for updated-at-trigger = (format nil trigger-sql table table table table)
    for field-definitions = (field-definitions defined-types type fields)
    appending
    (list
      (format nil "~%create table if not exists ~a (~%    ~{~a~^,~%    ~}~%)~%"
        table field-definitions)
      updated-at-trigger)))

(defun field-definitions (defined-types resource-type fields)
  (loop
    with base-types = (base-types defined-types)
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
                            (keyword-to-table-name reference base-types)))
    for regular-row = (unless reference
                        (format nil "~{~a~^ ~}"
                          (remove-if-not
                            #'identity
                            (list name type unique default required))))
    collect (if reference reference-row regular-row)
    into definitions
    finally
    (let* ((name-field (keyword-to-name-field resource-type))
            (name-field-present (some
                                  (lambda (f)
                                    (equal (getf f :name) name-field))
                                  fields))
            (default-base-definitions
              (list
                "id uuid primary key default uuid_generate_v4()"
                "created_at timestamp not null default now()"
                "updated_at timestamp not null default now()"))
            (default-name-definition
              (list
                (format nil "~a text not null unique" name-field)))
            (default-definitions
              (if name-field-present
                default-base-definitions
                (append default-base-definitions default-name-definition))))
      (return
        (append default-definitions definitions)))))

(defun validate-field (defined-types resource-type field)
  (let ((reference (getf field :reference)))
    (if reference
      (validate-reference-field defined-types resource-type reference field)
      (validate-regular-field resource-type field))))

(defun validate-regular-field (resource-type field)
  (let ((unknown-keys (set-difference (u:plist-keys field) *valid-field-keys*
                        :test 'eql))
         (name (getf field :name))
         (type (getf field :type :text))
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
      (equal (getf p :physical-path) path))
    resources))

(defun resource-results (resources keys)
  ":private: Given a list of RESOURCES (resource descriptor plists) and a list
of KEYS, returns the results according to KEYS. If KEYS has more than one key,
the result is a list of lists, where each inner list contains the values for
the given KEYS for each resource descriptor. If KEYS has a single key, then
the result is a list of the values for that key for each resource descriptor.
Finally, if KEYS is nil, the result is simply RESOURCES."
  (let* ((key-count (length keys)))
    (cond
      ((> key-count 1)
        (loop for resource in resources collect
          (loop for key in keys collect (getf resource key))))
      ((= key-count 1)
        (mapcar (lambda (p) (getf p (car keys))) resources))
      (t resources))))

(defun list-directory-recursively (path &key keys type)
  ":public: Recursively list the contents of the directory given by PATH, which
can be a string or a PATHNAME. The result, by default, consists of a list of
resource descriptors of all the files and directories that exist at any level
below PATH, sorted by resource name. A resource descriptor consists of a plist
that describes the resource.

The KEYS value, always a list, when provided, causes the result to consist of a
list of lists instead of a list of resource descriptors. Each inner list
contains the values associated with the given KEYS for each resource descriptor,
in the same order as the associated keys provided in KEYS. If KEYS consists of a
single key, then the result consists of a list of the values associated with
that key in each resource descriptor.

TYPE can be :file or :directory. When given, it filters the result to include
elements that are of that type only. Otherwise, the result includes both files
and directories."
  (let* ((path-rd (make-resource-descriptor path))
          (path-abs (getf path-rd :physical-path))
          paths)
    (cl-fad:walk-directory
      path-abs
      (lambda (p) (push p paths))
      :directories t)
    (let* ((resources (exclude-resource-with-physical-path
                        (mapcar #'make-resource-descriptor paths)
                        path-abs))
            (filtered (filter-by-type resources type))
            (sorted (sort filtered #'string<
                      :key (lambda (d) (getf d :resource-name)))))
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
  (let* ((path-rd (make-resource-descriptor path))
          (path-abs (getf path-rd :physical-path))
          (paths (cl-fad:list-directory path-abs))
          (resources (mapcar #'make-resource-descriptor paths))
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

(defun add-directory (path roles)
  (unless (re:scan "^/" path)
    (error "Absolute PATH is required."))
  (unless (re:scan "/$" path)
    (error "PATH must be a directory (must end with /)."))
  (let* ((path-rd (make-resource-descriptor path))
          (path-abs (getf path-rd :physical-path))
          (parent-abs (u:path-parent path-abs))
          (parent-rd (make-resource-descriptor parent-abs))
          (resource-name (getf path-rd :resource-name)))
    (when (getf path-rd :exists-in-storage)
      (error "Directory already exists."))
    (unless (getf parent-rd :exists-in-storage)
      (error "Parent directory doesn't exist."))
    (let ((id (a:add-resource *rbac* resource-name :roles roles)))
      (ensure-directories-exist path-abs)
      id)))

(defun remove-directory (path user)
  (unless (re:scan "^/" path)
    (error "Absolute PATH is required."))
  (unless (re:scan "/$" path)
    (error "PATH must be a directory and must end with /."))
  (let* ((path-rd (make-resource-descriptor path)))
    (unless (getf path-rd :exists-in-storage)
      (error "PATH ~a doest not exist." path))
    (let* ((resources (sort
                        (list-directory-recursively
                          (getf path-rd :logical-path)
                          :keys '(:resource-name :physical-path :is-file))
                        #'string>
                        :key #'cadr))
            (no-delete (remove-if
                         (lambda (r)
                           (a:user-allowed *rbac* user "delete" (car r)))
                         resources)))
      (when no-delete
        (error "~a ~a ~a ~a ~a ~{~a~^, ~}"
          "Failed to remove directory" path
          ", because user" user
          "lacks delete permissions on these resources: " no-delete))
      (loop for (resource physical-path is-file) in resources
        do (a:remove-resource *rbac* resource)
        if is-file do (uiop:delete-file-if-exists physical-path)
        else do (uiop:delete-empty-directory physical-path)))))

(defun add-user (user password &key
                  (email "no-email")
                  (roles a:*default-user-roles*)
                  other-user)
  (let ((all-roles (if other-user
                     (u:distinct-values
                       (append
                         (a:list-user-role-names *rbac* other-user)
                         roles))
                     roles)))
    (a:add-user *rbac* user email password :roles all-roles)))

(defun remove-user (user)
  (a:remove-user *rbac* user))


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
    :types (u:plist-keys
             (mapcar
               (lambda (s) (format nil "~(~a~)" s))
               (non-base-types types))))
  (loop for sql in (create-resource-tables-sql types)
    do (a:with-rbac (*rbac*) (a:rbac-query (list sql)))))

(defun init-database ()
  (pl:pinfo :in "init-database"
    :host *db-host*
    :port *db-port*
    :db-name *db-name*
    :db-user *db-user*)
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
      (a:add-permission *rbac* "list"
        :description "List contents of a directory")
      ;; Add resource tables
      (create-resource-tables *resource-types*))))

;; (defun start-web-server ()
;;   (setf h:*tmp-directory* *temp-directory*)

;;
;; END Init
;;
