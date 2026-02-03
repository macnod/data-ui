(require :fiveam)
(require :cl-ppcre)
(require :hunchentoot)
(require :swank)
(require :spinneret)
(require :jose)
(require :lass)
(require :postmodern)
(require :cl-fad)
(require :dc-ds)
(require :dc-time)
(require :p-log)
(require :rbac)
(require :dc-eclectic)

(push (uiop:getcwd) asdf:*central-registry*)
(ql:register-local-projects)
(asdf:load-system :data-ui)

(defpackage :data-ui-test
  (:use :cl :fiveam :data-ui :p-log)
  (:local-nicknames
    (:a :rbac)
    (:re :cl-ppcre)
    (:db :postmodern)
    (:ds :dc-ds)
    (:u :dc-eclectic)))

(in-package :data-ui-test)

;; Environment variables
(defparameter *log-file* (u:getenv "LOG_FILE"))
(defparameter *run-tests* (u:getenv "RUN_TESTS" :type :boolean :default t))
(defparameter *repl-enabled*
  (u:getenv "REPL_ENABLED" :type :boolean :default nil))
(defparameter *swank-port* (u:getenv "SWANK_PORT" :type :integer))

;; Other
(defparameter uuid-regex "^[a-f0-9]{8}(-[a-f0-9]{4}){3}-[a-f0-9]{12}$")
(defparameter *package-root* (asdf:system-relative-pathname :data-ui #P""))

;; Improved error checking
(defmacro error-matches (expr regex failure-text)
  `(handler-case
      (progn
        ,expr
        (fail ,failure-text))
     (error (e)
       (is (re:scan ,regex (format nil "~a" e))))))

(when *log-file* (make-log-stream "tests" *log-file* :append nil))

(init-database)

(defun is-uuid (s)
  (when (re:scan uuid-regex s) t))

(defun ascii-all ()
  (concatenate 'string
    (u:ascii-alpha-num)
    "[-!@#$%^&*()\+={}[\]|:;<>,.?/~`]"))

(defun clear-shared-files ()
  (cl-fad:delete-directory-and-files *document-root* :if-does-not-exist :ignore)
  (ensure-directories-exist *document-root*))

(defun clear-users ()
  (loop with users = (u:exclude
                       (a:list-user-names *rbac*)
                       (a:initial-users))
    for user in users
    for id = (a:remove-user *rbac* user)
    always id))

(defun clear-permissions ()
  (loop with permissions = (u:exclude
                             (a:list-permission-names *rbac*)
                             (cons "list" (a:initial-permissions)))
    for permission in permissions
    for id = (a:remove-permission *rbac* permission)
    always id))

(defun clear-roles ()
  (loop with roles = (u:exclude
                       (a:list-role-names *rbac*)
                       (a:initial-roles))
    for role in roles
    for id = (a:remove-role *rbac* role)
    always id))

(defun resource-names ()
  (u:safe-sort
    (u:exclude (a:list-resource-names *rbac*) "directories:/")
    :predicate #'string=))

(defun clear-resources ()
  (loop
    for resource in (resource-names)
    for id = (a:remove-resource *rbac* resource)
    always id))

(defun input-file (&optional file)
  (u:join-paths *package-root* "tests" "input-files" (if file file "/")))

(defun reset-input-files ()
  (let ((input-files (input-file)))
    (when (u:directory-exists-p input-files)
      (cl-fad:delete-directory-and-files input-files
        :if-does-not-exist :ignore))
    (ensure-directories-exist input-files)
    (loop for a from 1 to 5
      for file = (format nil "file-~d.txt" a)
      for content = (format nil "File ~d content" a)
      do (u:spew content (u:join-paths input-files file)))))

(defun clear-data ()
  (reset-input-files)
  (loop with resource-names = (resource-names)
    for resource in resource-names
    for parts = (re:split ":" resource)
    for type-string = (car parts)
    for type = (intern (string-upcase type-string) :keyword)
    for name = (if (data-ui::references-user type)
                 (cadr (re:split "/" parts))
                 (cadr parts))
    for rd = (make-resource-descriptor type name)
    for table = (getf rd :resource-table)
    for field = (getf rd :resource-name-field)
    for is-file = (u:tree-get rd :fs-storage :is-file)
    for is-directory = (u:tree-get rd :fs-storage :is-directory)
    for file = (when is-file
                 (u:tree-get rd :fs-storage :physical-path))
    for file-exists = (when file (u:file-exists-p file))
    for directory = (when is-directory
                      (u:tree-get rd :fs-storage :physical-path))
    for directory-exists = (when directory (u:directory-exists-p directory))
    for sql = (format nil "delete from ~a where ~a = $1" table field)
    do (pdebug :in "clear-data" :status "deleting"
         :type-string type-string
         :type type
         :name name
         :is-file is-file
         :is-directory is-directory
         :resource-descriptor (format nil "~a" rd)
         :resource-table table
         :resource-name-field field
         :file file
         :directory directory
         :sql sql)
    when file-exists do
    (pdebug :in "clear-data" :status "deleting file" :file file)
    (delete-file file)
    when directory-exists do
    (pdebug :in "clear-data" :status "deleting directory"
      :directory directory)
    (cl-fad:delete-directory-and-files directory
      :if-does-not-exist :ignore)
    finally
    (let ((users-cleared (clear-users))
           (permissions-cleared (clear-permissions))
           (roles-cleared (clear-roles))
           (resources-cleared (clear-resources)))
      (pinfo :in "clear-data"
        :status "cleared data"
        :resource-names resource-names
        :all-users-cleared users-cleared
        :all-permission-cleared permissions-cleared
        :all-roles-cleared roles-cleared
        :all-resources-cleared resources-cleared))
    ;; In case a test terminates uncleanly and the system doesn't have
    ;; the opportunity to delete all files associated with resources
    ;; and rt entries.
    (clear-shared-files)))

(def-suite data-ui-suite :description "FiveAM tests for the data-ui package")

(in-suite data-ui-suite)

(test clear-shared-files
  (clear-shared-files)
  (is-true (re:scan "\\b0 directories, 0 files$"
             (u:shell-command-to-string
               (format nil "tree '~a'" *document-root*)))))

(test current-directory
  (is-true
    (member "data-ui-tests.lisp"
      (mapcar (lambda (d) (u:filename-only (namestring d)))
        (directory (parse-namestring
                     (u:join-paths *package-root* "tests" "/*.*"))))
      :test 'equal)
    "Expected directory ~a to contain the file data-ui-tests.lisp"
    (u:join-paths *package-root* "tests")))

(test create-resource-tables-sql
  (let ((reference
          '(:users (:enable t :base t :fs-backed nil)
             :resources (:enable t :base t :fs-backed nil)
             :directories (:enable t :fs-backed t :fields nil
                            :patterns ("^/[-a-zA-Z0-9_ @./]+/$|^/$")
                            :anti-patterns ("//" "^[- @]"))
             :files (:enable t :fs-backed t
                      :patterns ("^/[-a-zA-Z0-9_. @/]+$")
                      :anti-patterns ("//" "/$" "^[- @]")
                      :fields
                      ((:name "size" :type :integer)
                        (:name "type" :type :text :required t)
                        (:name "xgroup" :type :text :unique t
                          :default "main")))
             :settings (:enable t :fs-backed nil
                         :patterns ("^[-a-zA-Z0-9.]+:[-a-zA-Z0-9.]+$")
                         :anti-patterns ("^[-.]" "[-.]$")
                         :fields
                         ((:reference :users)
                           (:name "setting_value" :type :text
                             :default "NIL"))))))
  ;; Happy case
    (is (equal '("
create table if not exists rt_directories (
    directory_name text not null unique,
    id uuid primary key references resources(id) on delete cascade,
    created_at timestamp not null default now(),
    updated_at timestamp not null default now()
)
"
 "
do $$
begin
    if not exists (
        select 1 from pg_trigger
        where tgname = 'set_rt_directories_updated_at'
        and tgrelid = 'rt_directories'::regclass::oid
    ) then
        create trigger set_rt_directories_updated_at
            before update on rt_directories
            for each row
            execute function set_updated_at_column();
    end if;
end $$;
"
 "
create table if not exists rt_files (
    size integer,
    type text not null,
    xgroup text unique default 'main',
    file_name text not null unique,
    id uuid primary key references resources(id) on delete cascade,
    created_at timestamp not null default now(),
    updated_at timestamp not null default now()
)
"
 "
do $$
begin
    if not exists (
        select 1 from pg_trigger
        where tgname = 'set_rt_files_updated_at'
        and tgrelid = 'rt_files'::regclass::oid
    ) then
        create trigger set_rt_files_updated_at
            before update on rt_files
            for each row
            execute function set_updated_at_column();
    end if;
end $$;
"
 "
create table if not exists rt_settings (
    user_id uuid not null references users(id) on delete cascade,
    setting_value text default 'NIL',
    setting_name text not null unique,
    id uuid primary key references resources(id) on delete cascade,
    created_at timestamp not null default now(),
    updated_at timestamp not null default now()
)
"
 "
do $$
begin
    if not exists (
        select 1 from pg_trigger
        where tgname = 'set_rt_settings_updated_at'
        and tgrelid = 'rt_settings'::regclass::oid
    ) then
        create trigger set_rt_settings_updated_at
            before update on rt_settings
            for each row
            execute function set_updated_at_column();
    end if;
end $$;
")
          (create-resource-tables-sql reference)))

    ;; If the :name key goes missing, we should get an error
    (let ((copy (u:deep-copy reference)))
      ;; Change the size field's :name key to :namex, such that there's no
      ;; longer a :name key.
      (u:tree-put :namex copy :files :fields 0 0)
      (is (equal :name (u:tree-get reference :files :fields 0 0)))
      (is (equal :namex (u:tree-get copy :files :fields 0 0)))
      (is (equal 6 (length (create-resource-tables-sql reference))))
      (error-matches
        (create-resource-tables-sql copy)
        "must have a name"
        "create-resource-tables-sql should have failed; missing :name field"))

    ;; If a field can't have both :required t and a value for :default
    (let ((copy (u:deep-copy reference)))
      (u:tree-put '(:name "xgroup" :type :integer :required t :default 1)
        copy :files :fields 2)
      (error-matches
        (create-resource-tables-sql copy)
        "is required and has a default"
        "create-resource-should have failed; required and default"))

    ;; If a field has a default, that default has to be of the same type as the
    ;; field. (If a field type is not specified, the the field's type is assumed
    ;; to be :text.)
    (let ((copy (u:deep-copy reference)))
      ;; Change the default attribute of the description field to an integer
      (u:tree-put 1 copy :files :fields 2 :default)
      (error-matches
        (create-resource-tables-sql copy)
        "Invalid type"
        "create-resource-tables-sql should have failed: bad type for default"))

    ;; If a field includes an unknown attribute, that is an error
    (let ((copy (u:deep-copy reference)))
      ;; Change the :type key of the size field to :typex, which is an unknown
      ;; attribute.
      (u:tree-put :typex copy :files :fields 0 2)
      (error-matches
        (create-resource-tables-sql copy)
        "unknown field key"
        "create-resource-tables-sql should have failed: unknown attribute"))

    ;; :unique must be t or nil
    (let ((copy (u:deep-copy reference)))
      ;; Change the value of the :unique attribute to 1 (instead of t)
      (u:tree-put 1 copy :files :fields 2 :unique)
      (error-matches
        (create-resource-tables-sql copy)
        "invalid value for :unique"
        "create-resource-tables-sql should have failed: bad value for :unique"))

    ;; :required must be t or nil
    (let ((copy (u:deep-copy reference)))
      ;; Change the value of the :required attribute to 1 (instead of t)
      (u:tree-put 1 copy :files :fields 1 :required)
      (error-matches
        (create-resource-tables-sql copy)
        "invalid value for :required"
        "create-resource-tables-sql should have failed: bad value for :required"))

    ;; Set :required to nil
    (let* ((copy (u:deep-copy reference))
            (files-sql (car
                         (remove-if-not
                           (lambda (s) (re:scan "create table .+ rt_files" s))
                           (create-resource-tables-sql copy)))))
      (is-true (re:scan "xgroup text unique default 'main'" files-sql))
      (is-false (re:scan "xgroup text\\n" files-sql)))))

(test create-resource-tables-sql-with-reference
  (let* ((reference '(:directories (:enable t :fields nil)
                      :files (:enable t :fields ((:reference :directories)))))
          (copy (u:deep-copy reference))
          (table-sql (nth 2 (create-resource-tables-sql copy)))
          (fields-sql (re:split "\\n" table-sql))
          (reference-field-sql (car
                                 (remove-if-not
                                   (lambda (s) (re:scan "^ *directory_id" s))
                                   fields-sql))))
    (is (equal
          (format nil "~a ~a"
            "directory_id uuid not null references rt_directories(id)"
            "on delete cascade,")
          (when reference-field-sql (u:trim reference-field-sql))))
    (u:tree-put :folders copy :files :fields 0 :reference)
    (error-matches
      (create-resource-tables-sql copy)
      "Resource not among defined types"
      "create-resource-tables-sql should have failed: :folders not defined")))

(test make-resource-descriptor
  (clear-data)
  (let ((root *document-root*))
    (let* ((id (a:get-id *rbac* "resources" "directories:/"))
            (reference (list
                         :resource-id id
                         :resource-name "directories:/"
                         :resource-table "rt_directories"
                         :resource-name-field "directory_name"
                         :resource-exists t
                         :rt-entry-exists t
                         :fs-entry-exists t
                         :exists t
                         :fs-backed t
                         :fs-storage
                         (list
                           :is-directory t
                           :is-file nil
                           :logical-path "/"
                           :physical-path *document-root*
                           :file-name-only nil
                           :leaf-directory "/"
                           :logical-path-only "/"
                           :physical-path-only *document-root*
                           :exists t))))
      (is (equal reference (make-resource-descriptor :directories "/")))
      (is (equal reference (make-resource-descriptor :directories #P"/")))
      (is (equal reference (make-resource-descriptor :directories root))))
    ;; /alpha/
    (is (equal "/alpha/" (make-resource-descriptor :directories "/alpha/"
                           :keys '(:fs-storage :logical-path))))
    (is (equal "/alpha/"
          (make-resource-descriptor :directories (u:join-paths root "/alpha/")
            :keys '(:fs-storage :logical-path))))
    (is (equal "directories:/alpha/"
          (make-resource-descriptor :directories "/alpha/"
            :keys '(:resource-name))))
    (is-false (make-resource-descriptor :directories "/alpha/"
                :keys '(:resource-id)))
    (is-false (make-resource-descriptor :directories "/alpha/"
                :keys '(:resource-exists)))
    (is-true (make-resource-descriptor :directories "/alpha/"
               :keys '(:fs-backed)))
    (is-true (make-resource-descriptor :directories "/alpha/"
               :keys '(:fs-storage :is-directory)))
    (is-false (make-resource-descriptor :directories "/alpha/"
                :keys '(:fs-storage :is-file)))
    (is-false (make-resource-descriptor :directories "/alpha/"
                :keys '(:fs-storage :exists)))
    ;; /alpha/bravo/c.txt
    (let ((rd (make-resource-descriptor :files "/alpha/bravo/c.txt")))
      (is-false (is-uuid (getf rd :resource-id)))
      (is (equal "files:/alpha/bravo/c.txt" (getf rd :resource-name)))
      (is-false (getf rd :resource-exists))
      (is-true (getf rd :fs-backed))
      (is-false (u:tree-get rd :fs-storage :is-directory))
      (is-true (u:tree-get rd :fs-storage :is-file))
      (is (equal "/alpha/bravo/c.txt"
            (u:tree-get rd :fs-storage :logical-path)))
      (is (equal (u:join-paths root "/alpha/bravo/c.txt")
            (u:tree-get rd :fs-storage :physical-path)))
      (is (equal "c.txt" (u:tree-get rd :fs-storage :file-name-only)))
      (is (equal "bravo" (u:tree-get rd :fs-storage :leaf-directory)))
      (is (equal "/alpha/bravo/"
            (u:tree-get rd :fs-storage :logical-path-only)))
      (is (equal (u:join-paths root "/alpha/bravo/")
            (u:tree-get rd :fs-storage :physical-path-only)))
      (is-false (u:tree-get rd :fs-storage :exists)))))

(test add-directory
  (clear-data)
  (let* ((rd-alpha (add-resource :directories "/alpha/"))
          (rd-bravo (make-resource-descriptor :directories "/bravo/"))
          (id (getf rd-alpha :resource-id)))
    (is-true (is-uuid id))
    (is (equal id (getf rd-alpha :resource-id)))
    (is-true (u:tree-get rd-alpha :fs-storage :is-directory))
    (is-false (u:tree-get rd-alpha :fs-storage :is-file))
    (is-true (getf rd-alpha :resource-exists))
    (is-true (u:tree-get rd-alpha :fs-storage :exists))
    (is-false (getf rd-bravo :resource-id))
    (is-true (validate-resource-descriptor
               (add-resource :directories "/alpha/one/")))
    (error-matches
      (add-resource :directories "/bravo/one/")
      "Parent directory does not exist"
      "add-resource should have failed as parent folder doesn't exist")
    (error-matches
      (add-resource :directories "/alpha/")
      "already exists"
      "add-resource should have failed. /alpha/ already exists.")
    (is-true (add-resource :directories "/bravo/"))
    (is-true (add-resource :directories "/bravo/one/"))
    ;; Fail pattern matches for directories
    (error-matches
      (add-resource :directories "/bravo/ two/")
      "matches anti-pattern"
      "File '/bravo/ two/' should have failed an anti-pattern check")
    (error-matches
      (add-resource :directories "/bravo//two/")
      "matches anti-pattern"
      "File '/bravo//two/' should have failed on anti-pattern check")
    (error-matches
      (add-resource :directories " /bravo/two")
      "does not match pattern"
      "File ' /bravo/two' should have failed on pattern check")
    (error-matches
      (add-resource :directories "/bravo/ two/")
      "matches anti-pattern"
      "File '/bravo/ two/' should have failed on anti-pattern check")
    (error-matches
      (add-resource :directories "//bravo/two/")
      ""
      "File '//bravo/two/' should have failed on anti-pattern check")
    (error-matches
      (add-resource :directories "/bravo/two//")
      "matches anti-pattern"
      "File '/bravo/two//' should have failed on anti-pattern check")
    (error-matches
      (add-resource :directories "/bravo/two /")
      "matches anti-pattern"
      "File '/bravo/two /' should have failed on anti-pattern check")))

(test add-file
  (clear-data)
  (add-resource :directories "/alpha/")
  (let* ((file-1 "/alpha/file-1.txt")
          (file-2 "/bravo/file-2.txt")
          ;; The following fail pattern matches for files
          (file-3 "/bravo/ file-3.txt") ;; file name starts with space
          (file-4 " /bravo/file-4.txt") ;; file's path starts with space
          (file-5 "/bravo/file-5.txt ") ;; file's path ends with space
          (file-6 "/bravo//file-6.txt") ;; file's path has '//'
          (file-7 "//bravo/file-7.txt") ;; file's path starts with '//'
          (file-8 "/bravo/file-8.txt/") ;; file name ends with '/'
          (file-9 "/bravo/file-9.txt")  ;; source file does not exist
          (resource-name-1 (format nil "files:~a" file-1))
          (rd-file (add-resource :files file-1
                    :source-file (input-file (u:filename-only file-1)))))
    (is-true (is-uuid (getf rd-file :resource-id)))
    (is (equal resource-name-1 (getf rd-file :resource-name)))
    (is-true (u:tree-get rd-file :fs-backed))
    (is-true (u:tree-get rd-file :fs-storage :is-file))
    (is-false (u:tree-get rd-file :fs-storage :is-directory))
    (error-matches
      (add-resource :files file-2
        :source-file (input-file (u:filename-only file-2)))
      "Parent directory does not exist"
      "add-resource should have failed as parent folder doesn't exist")
    (add-resource :directories "/bravo/")
    (is-true (validate-resource-descriptor
               (add-resource :files file-2
                 :source-file (input-file (u:filename-only file-2)))))
    (error-matches
      (add-resource :files file-3
        :source-file (input-file (u:filename-only file-3)))
      "matches anti-pattern"
      "File '/bravo/ file-3.txt' should have failed an anti-pattern check")
    (error-matches
      (add-resource :files file-4
        :source-file (input-file (u:filename-only file-4)))
      "does not match pattern"
      "File ' /bravo/file-4.txt' should have failed a pattern check")
    (error-matches
      (add-resource :files file-5
        :source-file (input-file (u:filename-only file-5)))
      "matches anti-pattern"
      "File '/bravo/file-5.txt ' should have failed an anti-pattern check")
    (error-matches
      (add-resource :files file-6
        :source-file (input-file (u:filename-only file-6)))
      "matches anti-pattern"
      "File '/bravo//file-6.txt' should have failed an anti-pattern check")
    (error-matches
      (add-resource :files file-7
        :source-file (input-file (u:filename-only file-7)))
      "matches anti-pattern"
      "File '//bravo/file-7.txt' should have failed an anti-pattern check")
    (error-matches
      (add-resource :files file-8
        :source-file (input-file (u:filename-only file-8)))
      "matches anti-pattern"
      "File '/bravo/file-8.txt/' should have failed an anti-pattern check")
    (error-matches
      (add-resource :files file-9
        :source-file (input-file (u:filename-only file-9)))
      "does not exist"
      "File '/bravo/file-9.txt' should have failed; it doesn't exist.")
    (error-matches
      (add-resource :files file-1
        :source-file (input-file (u:filename-only file-1)))
      "already exists"
      "add-resource should have failed. /alpha/file-1.txt already exists.")))

(test add-setting
  (clear-data)
  (let ((rd (add-resource :settings "dark-mode" :references '(:users "admin"))))
    (is-false (u:tree-get rd :fs-backed))
    (is-false (u:tree-get rd :fs-storage))
    (is-true (is-uuid (u:tree-get rd :resource-id)))
    (is (equal "settings:admin/dark-mode" (u:tree-get rd :resource-name)))
    (is-true (u:tree-get rd :rt-entry-exists))
    (is-false (u:tree-get rd :fs-entry-exists))
    (is-true (u:tree-get rd :exists))
    (is (equal "rt_settings" (u:tree-get rd :resource-table)))
    (is (equal "setting_name" (u:tree-get rd :resource-name-field)))))

;;
;; Run tests
;;
(if *run-tests*
  (progn
    (format t "Running DATA-UI tests...~%")
    (clear-shared-files)
    (let ((test-results (run-all-tests)))
      (close-log-stream "tests")
      (unless test-results
        (sb-ext:quit :unix-status 1))))
  (if *repl-enabled*
    (progn
      (format t "Compiling and loading DATA-UI system with Swank server...~%")
      (pinfo :in "data-ui-tests"
        :status "Starting Swank server for interactive debugging"
        :port *swank-port*)
      (defparameter *swank-server* (swank:create-server
                                     :interface "0.0.0.0"
                                     :port *swank-port*
                                     :style :spawn
                                     :dont-close t))
      (pinfo :in "rbac-tests" :status "Swank server started" :port *swank-port*))
    (format t "DATA-UI system loaded.~%")))
