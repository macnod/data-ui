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
  (unless (admin-user-present)
    (a:initialize-database *rbac* *admin-password*))
  *rbac*)

(defun immutable-user-roles (user-name)
  (let ((roles (list "public" (a:exclusive-role-for user-name))))
    (unless (equal user-name *guest-user*)
      (push *logged-in-role* roles))
    (u:safe-sort roles)))

;; dcdebug: Debug-only. Remove at some point.
(defun query (sql)
  (a:with-rbac (*rbac*) (db:query sql)))
