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

(when *log-file* (make-log-stream "tests" *log-file* :append nil))

(init-database)

;; What data-ui starts with
;; (defparameter *base-users* (list-user-names *rbac*))
;; (defparameter *base-roles* (list-role-names *rbac*))
;; (defparameter *base-permissions* (list-permission-names *rbac*))

;; (defun clear-database ()
;;   (loop for user in (u:exclude (list-user-names *rbac*) *base-users*)
;;     do (remove-user *rbac* user))
;;   (loop for permission in (u:exclude (list-permission-names *rbac*)
;;                             *base-permissions*)
;;     do (remove-permission *rbac* permission))
;;   (loop for role in (u:exclude (list-role-names *rbac*) *base-roles*)
;;     do (remove-role *rbac* role))
;;   (loop for resource in (list-resource-names *rbac*)
;;     do (remove-resource *rbac* resource))
;;   t)

(defun is-uuid (s)
  (when (re:scan uuid-regex s) t))

(defun ascii-all ()
  (concatenate 'string
    (u:ascii-alpha-num)
    "[-!@#$%^&*()\+={}[\]|:;<>,.?/~`]"))

(defun clear-shared-files ()
  (cl-fad:delete-directory-and-files *document-root* :if-does-not-exist :ignore)
  (ensure-directories-exist *document-root*))

(defun clear-data ()
  (loop with resource-names = (u:safe-sort
                                (u:exclude
                                  (a:list-resource-names *rbac*)
                                  "directories:/")
                                :predicate #'string=)
    for resource in resource-names
    for parts = (re:split ":" resource)
    for type-string = (car parts)
    for type = (intern (string-upcase type-string) :keyword)
    for name = (cadr parts)
    for rd = (make-resource-descriptor type name)
    for table = (getf rd :resource-table)
    for field = (getf rd :resource-name-field)
    for is-file = (u:tree-get rd :fs-storage :is-file)
    for is-directory = (u:tree-get rd :fs-storage :is-directory)
    for file = (when is-file
                 (u:tree-get rd :fs-storage :physical-path))
    for directory = (when is-directory
                      (u:tree-get rd :fs-storage :physical-path))
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
    when file do
    (pdebug :in "clear-data" :status "deleting file" :file file)
    (delete-file file)
    when directory do
    (pdebug :in "clear-data" :status "deleting directory"
      :directory directory)
    (cl-fad:delete-directory-and-files directory
      :if-does-not-exist :ignore)
    finally
    (let ((users-cleared
            (loop with users = (u:exclude
                                 (a:list-user-names *rbac*)
                                 (a:initial-users))
              for user in users
              for id = (a:remove-user *rbac* user)
              always id))
           (permissions-cleared
             (loop with permissions = (u:exclude
                                        (a:list-permission-names *rbac*)
                                        (cons "list" (a:initial-permissions)))
               for permission in permissions
               for id = (a:remove-permission *rbac* permission)
               always id))
           (roles-cleared
             (loop with roles = (u:exclude
                                  (a:list-role-names *rbac*)
                                  (a:initial-roles))
               for role in roles
               for id = (a:remove-role *rbac* role)
               always id))
           (resources-cleared
            (loop
              for resource in resource-names
              for id = (a:remove-resource *rbac* resource)
              always id)))
      (pinfo :in "clear-data"
        :status "cleared data"
        :resource-names resource-names
        :all-users-cleared users-cleared
        :all-permission-cleared permissions-cleared
        :all-roles-cleared roles-cleared
        :all-resources-cleared resources-cleared))))

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
          '(:users (:enable t :base t)
             :directories (:enable t :fields nil)
             :files
             (:enable t
               :fields ((:name "size" :type :integer)
                         (:name "type" :type :text)
                         (:name "description" :type :text
                           :default "No description")
                         (:name "hash" :type :text :unique t)
                         (:name "xgroup" :type :integer :required t)))
             :settings
             (:enable t
               :fields
               ((:reference :users)
                 (:name "setting_name" :type :text :required t)
                 (:name "setting_value" :type :text :default "NIL"))))))
  ;; Happy case
    (is (equal '("
create table if not exists rt_directories (
    id uuid primary key references resources(id) on delete cascade,
    created_at timestamp not null default now(),
    updated_at timestamp not null default now(),
    directory_name text not null unique
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
    id uuid primary key references resources(id) on delete cascade,
    created_at timestamp not null default now(),
    updated_at timestamp not null default now(),
    file_name text not null unique,
    size integer,
    type text,
    description text default 'No description',
    hash text unique,
    xgroup integer not null
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
    id uuid primary key references resources(id) on delete cascade,
    created_at timestamp not null default now(),
    updated_at timestamp not null default now(),
    user_id uuid not null references users(id) on delete cascade,
    setting_name text not null,
    setting_value text default 'NIL'
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
      (is (equal '(:users (:enable t :base t)
                    :directories (:enable t :fields nil)
                    :files
                    (:enable t
                      :fields ((:namex "size" :type :integer)
                                (:name "type" :type :text)
                                (:name "description" :type :text
                                  :default "No description")
                                (:name "hash" :type :text :unique t)
                                (:name "xgroup" :type :integer :required t)))
                    :settings
                    (:enable t
                      :fields
                      ((:reference :users)
                        (:name "setting_name" :type :text :required t)
                        (:name "setting_value" :type :text :default "NIL"))))
            copy))
      (is (equal 6 (length (create-resource-tables-sql reference))))
      (signals error (create-resource-tables-sql copy)))

    ;; If a field can't have both :unique t and :required t
    (let ((copy (u:deep-copy reference)))
      (u:tree-put '(:name "xgroup" :type :integer :required t :default 1)
        copy :files :fields 4)
      (signals error (create-resource-tables-sql copy)))

    ;; If a field has a default, that default has to be of the same type as the
    ;; field. (If a field type is not specified, the the field's type is assumed
    ;; to be :text.)
    (let ((copy (u:deep-copy reference)))
      ;; Change the default attribute of the description field to an integer
      (u:tree-put 1 copy :files :fields 2 :default)
      (signals error (create-resource-tables-sql copy)))

    ;; If a field includes an unknown attribute, that is an error
    (let ((copy (u:deep-copy reference)))
      ;; Change the :type key of the size field to :typex, which is an unknown
      ;; attribute.
      (u:tree-put :typex copy :files :fields 0 2)
      (signals error (create-resource-tables-sql copy)))

    ;; :unique must be t or nil
    (let ((copy (u:deep-copy reference)))
      ;; Change the value of the :unique attribute to 1 (instead of t)
      (u:tree-put 1 copy :files :fields 3 :unique)
      (signals error (create-resource-tables-sql copy)))

    ;; :required must be t or nil
    (let ((copy (u:deep-copy reference)))
      ;; Change the value of the :required attribute to 1 (instead of t)
      (u:tree-put 1 copy :files :fields 4 :required)
      (signals error (create-resource-tables-sql copy)))

    ;; Set :required to nil
    (let* ((copy (u:deep-copy reference))
            (files-sql (car
                         (remove-if-not
                           (lambda (s) (re:scan "create table .+ rt_files" s))
                           (create-resource-tables-sql copy)))))
      (u:tree-put nil copy :files :fields 4 :required)
      (u:tree-put nil copy :files :fields 3 :unique)
      (is-true (re:scan "xgroup integer not null" files-sql))
      (is-false (re:scan "xgroup integer\\n" files-sql))
      (is-true (re:scan "hash text unique" files-sql))
      (is-false  (re:scan "hash text,\\n" files-sql)))))

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
            "on delete cascade")
          (when reference-field-sql (u:trim reference-field-sql))))
    (u:tree-put :folders copy :files :fields 0 :reference)
    (signals error (create-resource-tables-sql copy))))

(test make-resource-descriptor
  (clear-data)
  (let ((root *document-root*))
    (let* ((id (a:get-id *rbac* "resources" "directories:/"))
            (reference (list
                         :resource-id id
                         :resource-name "directories:/"
                         :resource-table "rt_directories"
                         :resource-name-field "directory_name"
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
                           :fs-storage :logical-path)))
    (is (equal "/alpha/"
          (make-resource-descriptor :directories (u:join-paths root "/alpha/")
            :fs-storage :logical-path)))
    (is (equal "directories:/alpha/"
          (make-resource-descriptor :directories "/alpha/" :resource-name)))
    (is-false (make-resource-descriptor :directories "/alpha/" :resource-id))
    (is-false (make-resource-descriptor :directories "/alpha/" :exists))
    (is-true (make-resource-descriptor :directories "/alpha/" :fs-backed))
    (is-true (make-resource-descriptor :directories "/alpha/"
               :fs-storage :is-directory))
    (is-false (make-resource-descriptor :directories "/alpha/"
                :fs-storage :is-file))
    (is-false (make-resource-descriptor :directories "/alpha/"
                :fs-storage :exists))
    ;; /alpha/bravo/c.txt
    (let ((rd (make-resource-descriptor :files "/alpha/bravo/c.txt")))
      (is-false (is-uuid (getf rd :resource-id)))
      (is (equal "files:/alpha/bravo/c.txt" (getf rd :resource-name)))
      (is-false (getf rd :exists))
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
  (let* ((rd-alpha (add-resource :directories "/alpha/" '("admin")))
          (rd-bravo (make-resource-descriptor :directories "/bravo/"))
          (id (getf rd-alpha :resource-id)))
    (is-true (is-uuid id))
    (is (equal id (getf rd-alpha :resource-id)))
    (is-true (u:tree-get rd-alpha :fs-storage :is-directory))
    (is-false (u:tree-get rd-alpha :fs-storage :is-file))
    (is-true (getf rd-alpha :exists))
    (is-true (u:tree-get rd-alpha :fs-storage :exists))
    (is-false (getf rd-bravo :resource-id))
    (is-true (validate-resource-descriptor
               (add-resource :directories "/alpha/one/" '("admin"))))
    (signals error
      (add-resource :directories "/bravo/one/" '("admin")))
    (is-true (add-resource :directories "/bravo/" '("admin")))
    (is-true (add-resource :directories "/bravo/one/" '("admin")))))

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
