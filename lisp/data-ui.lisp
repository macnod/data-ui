(in-package :data-ui)

(defparameter *swank-port* (u:getenv "SWANK_PORT" :type :integer))
(defparameter *swank-server* nil)

;; Logs
(defparameter *log-file* (or (u:getenv "LOG_FILE") *standard-output*))
(defparameter *log-severity-threshold*
  (u:make-keyword (or (u:getenv "LOG_SEVERITY") "DEBUG")))
(defparameter *log-suppress-health*
  (u:getenv "LOG_SUPPRESS_HEALTH" :type :boolean :default t))

;; Environment
(defparameter *version* (u:getenv "DATAUI_VERSION" :default "0.0"))
(defparameter *environment*
  (u:getenv "DATAUI_ENVIRONMENT" :default "unknown"))

;; Other
(defparameter *large-page-size* 10000)

;; Defaults
(defparameter *default-file-permissions* '("create" "read" "update" "delete"))
(defparameter *default-directory-permissions*
  '("create" "read" "update" "delete" "list"))

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

(defun start-swank-server ()
  (when (and *swank-port* (not *swank-server*))
    (pl:pinfo :in "run" :status "starting swank")
    (setf *swank-server*
      (swank:create-server
        :interface "0.0.0.0"
        :port *swank-port*
        :style :spawn
        :dont-close t))))

;;
;; BEGIN Init
;;

;; Start logging
(when *log-file*
  (pl:make-log-stream "data-ui" *log-file*))

(start-swank-server)
(init-database)

;; (defun create-resource-tables (types)
;;   (pl:pdebug :in "create-resource-tables"
;;     :types (mapcar #'to-sql-identifier (u:plist-keys (non-base-types types))))
;;   (loop for sql in (create-resource-tables-sql types)
;;     do (a:with-rbac (*rbac*) (db:query sql))))

;; (defun start-web-server ()
;;   (setf h:*tmp-directory* *temp-directory*)

;;
;; END Init
;;
