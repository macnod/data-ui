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
  '(:base
     :enable
     :fields
     :fs-backed))
(defparameter *valid-field-keys*
  '(:default
     :name
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
     :fs-storage
     :resource-id
     :resource-name
     :resource-name-field
     :resource-table))
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
  ;; system.
  '(:users (:enable t :base t :fs-backed nil)
     :resources (:enable t :base t :fs-backed nil)
     ;; Because this is not a base type, its table will be named with the rt_
     ;; prefix: rt_directories. The automatically-created field
     ;; rt_directories.directory_name will hold strings consisting of the prefix
     ;; 'directory:' followed by the logical path of the directory, i.e.
     ;; "directory:/a/b/".  The resources.resource_name field will contain the
     ;; same value.
     :directories (:enable t :fs-backed t :fields nil)
     :files (:enable t :fs-backed t :fields nil)
     :settings (:enable t :fs-backed nil :fields
                 ;; The automaticall-created rt_settings.setting_name field will
                 ;; consist of the prefix 'setting:' followed by the user name
                 ;; and setting name, separated by a colon, e.g.,
                 ;; "setting:alice:dark-mode". As usual, the
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

(defun make-resource-descriptor (type name-only &rest keys)
  (unless (member type (u:plist-keys *resource-types*))
    (error "Unknown resource type: ~a." type))
  (unless (u:has (cons nil *valid-descriptor-keys*) (car keys))
    (error "Unknown resource descriptor key: ~a." (car keys)))
  (unless (u:has (cons nil *valid-fs-storage-keys*) (cdr keys))
    (error "Unknown fs-storage key: ~a." (cadr keys)))
  (when (> (length keys) 2)
    (error "Too many keys provided to make-resource-descriptor."))
  (let* ((root *document-root*)
          (drl (length root))
          (fs-backed (u:tree-get *resource-types* type :fs-backed))
          (probe (when fs-backed (probe fs-backed name-only root)))
          (sprobe (when fs-backed (sprobe fs-backed probe name-only root)))
          (spath (when fs-backed (spath fs-backed probe sprobe root)))
          (is-physical (when fs-backed (u:starts-with spath root)))
          (is-directory (when fs-backed
                          (if probe
                            (equal (u:path-type spath) :directory)
                            (u:ends-with (format nil "~a" name-only) "/"))))
          (is-file (when fs-backed
                     (if probe
                       (equal (u:path-type spath) :file)
                       (not (u:ends-with (format nil "~a" name-only) "/")))))
          (physical-path (when fs-backed
                           (if is-directory
                             spath
                             (re:regex-replace "/$" spath ""))))
          (relative (when fs-backed
                      (if is-physical (subseq spath drl) spath)))
          (logical-path (when fs-backed
                          (if is-physical
                            (format nil "/~a" relative)
                            spath)))
          (resource-prefix (format nil "~(~a~)" type))
          (resource-name (format nil "~a:~a" resource-prefix
                           (if fs-backed logical-path name-only)))
          (resource-id (resource-id resource-name))
          (resource-table (table-name resource-name))
          (name-field (name-field resource-name))
          (result (list
                    :resource-id resource-id
                    :resource-name resource-name
                    :resource-table resource-table
                    :resource-name-field name-field
                    :exists (when (and *rbac* (resource-id resource-name)) t)
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
                        :leaf-directory (if is-directory
                                          (u:leaf-directory-only logical-path)
                                          (u:leaf-directory-only
                                            (u:path-only logical-path)))
                        :logical-path-only (if is-directory
                                             logical-path
                                             (u:path-only logical-path))
                        :physical-path-only (if is-directory
                                              physical-path
                                              (u:path-only physical-path))
                        :exists (if is-directory
                                  (u:directory-exists-p physical-path)
                                  (u:file-exists-p physical-path)))))))
    (pl:pdebug :in "make-resource-descriptor"
      :rbac-present (when *rbac* t)
      :root root
      :drl drl
      :probe probe
      :sprobe sprobe
      :spath spath
      :resource-prefix resource-prefix
      :resource-name resource-name
      :resource-id (getf result :resource-id)
      :exists (getf result :exists)
      :is-physical is-physical
      :relative relative
      :fs-backed fs-backed
      :is-directory is-directory
      :is-file is-file
      :logical-path logical-path
      :physical-path physical-path
      :file-name-only (u:tree-get result :fs-storage :file-name-only)
      :leaf-directory (u:tree-get result :fs-storage :leaf-directory)
      :logical-path-only (u:tree-get result :fs-storage :logical-path-only)
      :physical-path-only (u:tree-get result :fs-storage :physical-path-only)
      :exists-in-storage (u:tree-get result :fs-storage :exists-in-storage))
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
                "id uuid primary key references resources(id) on delete cascade"
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

(defun resource-results (resource-descriptors keys)
  ":private: Given a list of RESOURCE-DESCRIPTORS (resource descriptor plists) and a list
of KEYS, returns the results according to KEYS. If KEYS has more than one key,
the result is a list of lists, where each inner list contains the values for
the given KEYS for each resource descriptor. If KEYS has a single key, then
the result is a list of the values for that key for each resource descriptor.
Finally, if KEYS is nil, the result is simply RESOURCE-DESCRIPTORS."
  (let* ((key-count (length keys)))
    (cond
      ((> key-count 1)
        (loop for resource in resource-descriptors collect
          (loop for key in keys collect (getf resource key))))
      ((= key-count 1)
        (mapcar (lambda (p) (getf p (car keys))) resource-descriptors))
      (t resource-descriptors))))

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
  (let* ((path-rd (make-resource-descriptor :directory path))
          (path-abs (u:tree-get path-rd :fs-storage :physical-path))
          paths)
    (cl-fad:walk-directory
      path-abs
      (lambda (p) (push p paths))
      :directories t)
    (let* ((resources (exclude-resource-with-physical-path
                        (mapcar
                          (lambda (p)
                            (if (u:ends-with (format nil "~a" p) "/")
                              (make-resource-descriptor :directory p)
                              (make-resource-descriptor :file p)))
                          paths)
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

(defun add-resource (type-key name roles &key source-file)
  (validate-type-keyword type-key)
  (let ((unknown-roles (set-difference roles
                         (a:list-role-names *rbac*)
                         :test 'string=)))
    (when unknown-roles
      (error "Unknown role~p: ~{~a~^, ~}."
        (length unknown-roles) unknown-roles)))
  (let* ((rd (make-resource-descriptor type-key name))
          (parent-directory (when (u:tree-get rd :fs-backed)
                              (u:path-parent
                                (u:tree-get rd
                                  :fs-storage :physical-path-only)))))
    (when (and
            (u:tree-get rd :fs-backed)
            (not (u:directory-exists-p parent-directory)))
        (error "Parent directory does not exist for resource ~a."
          (getf rd :resource-name)))
    (when (or (and (u:tree-get rd :fs-storage :exists)
                (not (getf rd :resource-name "directories:/")))
            (u:tree-get rd :exists))
      (error "Resource already exists."))
    (when (and
            (equal type-key :file)
            (or (not source-file) (not (u:file-exists-p source-file))))
      (error "Source file must be provided and must exist for file resources."))
    (let ((id (a:add-resource *rbac* (getf rd :resource-name) :roles roles)))
      (unless id
        (error "Failed to add resource ~a." (getf rd :resource-name)))
      (when (u:tree-get rd :fs-backed)
        (ensure-directories-exist
          (u:tree-get rd :fs-storage :physical-path))
        (when (u:tree-get rd :fs-storage :is-file)
          (u:copy-file source-file
            (u:tree-get rd :fs-storage :physical-path))))
      (let* ((table-name (table-name (getf rd :resource-name)))
              (name-field (keyword-to-name-field type-key))
              (sql (format nil "insert into ~a (id, ~a) values ($1, $2)"
                     table-name name-field)))
        (a:with-rbac (*rbac*)
          (db:query sql id (getf rd :resource-name))))
      (make-resource-descriptor type-key name))))

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
      (create-resource-tables *resource-types*)
      (when (not (a:get-id *rbac* "permissions" "list"))
        (a:add-permission *rbac* "list"
          :description "List contents of a directory"))
      (when (not (a:get-id *rbac* "resources" "directory:/"))
        (add-resource :directories "/" '("public"))))))

;; (defun start-web-server ()
;;   (setf h:*tmp-directory* *temp-directory*)

;;
;; END Init
;;
