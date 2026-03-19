(in-package :data-ui)

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

(defun is-uuid (s)
  (when (re:scan uuid-regex s) t))

(defun ascii-all ()
  (concatenate 'string
    (u:ascii-alpha-num)
    "[-!@#$%^&*()\+={}[\]|:;<>,.?/~`]"))

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

(defun clear-data ()
  (clear-users)
  (clear-permissions)
  (clear-roles))

(t:def-suite data-ui-suite :description "FiveAM tests for the data-ui package")

(t:in-suite data-ui-suite)
