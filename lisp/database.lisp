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

;; admin user
(defparameter *admin-user* "admin")
(defparameter *admin-password* (u:getenv "ADMIN_PASSWORD"
                                 :default "admin-password-1234"))

;; guest user
(defparameter *guest-user* "guest")
(defparameter *guest-password* "guest-password-1234")

;; Roles
(defparameter *admin-role* "admin")
(defparameter *system-role* "system")
(defparameter *public-role* "public")
(defparameter *logged-in-role* "logged-in")

;; Tables
(defparameter *users-table* "users")
(defparameter *roles-table* "roles")
(defparameter *resources-table* "resources")

;; Database handle
(defparameter *rbac* nil)

;; This will be redefined later, in the model module. Defining here for now to
;; avoid compilation warnings
(defparameter *base-model* nil)

(defun table-exists (table)
  (when (and
          table
          (a:with-rbac (*rbac*)
            (a:rbac-query
              (list
                "select 1 from information_schema.tables where table_name = $1"
                table)
              :single)))
          t))

(defun drop-table (table)
  (when (table-exists table)
    (let ((sql (format nil "drop table if exists ~a cascade" table)))
      (a:with-rbac (*rbac*)
        (handler-bind
          ((warning (lambda (w)
                      (when (search "drop cascades" (format nil "~a" w))
                        (muffle-warning)))))
          (a:rbac-query (list sql))))
      t)))

(defun reset-tables (compiled-model)
  (loop with m = compiled-model
    for type-key in m by #'cddr
    for type-def in (cdr m) by #'cddr
    for table-name = (u:tree-get m type-key :table-name)
    for is-table = (u:tree-get m type-key :table)
    for is-base = (u:tree-get m type-key :base)
    for is-settings = (equal type-key :settings)
    when (and is-table (or (not is-base) is-settings))
    do (drop-table table-name)
    finally (initialize-tables)))

(defun reset-database ()
  (loop
    with sql-tables = (format nil
                        "select table_name from information_schema.tables ~
                         where table_name like 'rt_%'")
    with tables = (query sql-tables :result-type :column)
    for table in tables
    do (drop-table table))
  (loop with sql = "truncate ~{~a~^, ~} cascade"
    for table-key in *base-model* by #'cddr
    for table-def in (cdr *base-model*) by #'cddr
    for is-base = (u:tree-get *base-model* table-key :base)
    for built-in = (u:tree-get *base-model* table-key :built-in)
    for table-name = (table-name table-key built-in)
    when is-base collect table-name into tables
    finally
    (query (format nil sql tables)))
  (a:initialize-database *rbac* *admin-password*))

(defun initialize-tables ()
  (a:initialize-database *rbac* *admin-password*))

(defun admin-user-present ()
  (When (a:get-id *rbac* *users-table* *admin-user*) t))

(defun init-database ()
  (pl:pinfo :in "init-database"
    :host *db-host*
    :port *db-port*
    :db-name *db-name*
    :db-user *db-user*)
  ;; Create the connection
  (setf *rbac* (make-instance 'a:rbac-pg
                 :db-host *db-host*
                 :db-port *db-port*
                 :db-name *db-name*
                 :db-user *db-user*
                 :db-password *db-password*
                 :resource-regex "^.+$"))
  (pl:pdebug :in "init-database" :status "set *rbac*"
    :rbac *rbac*)
  (unless (admin-user-present) (initialize-tables))
  *rbac*)

;; TODO: Dead?
(defun immutable-user-roles (user-name)
  (let ((roles (list "public" (a:exclusive-role-for user-name))))
    (unless (equal user-name *guest-user*)
      (push *logged-in-role* roles))
    (u:safe-sort roles)))

;; dcdebug: Debug-only. Remove at some point.
(defun query (sql &key params (result-type :plists))
  (a:with-rbac (*rbac*) (a:rbac-query (cons sql params) result-type)))
