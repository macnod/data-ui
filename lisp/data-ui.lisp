(in-package :data-ui)

;; Logs
(defparameter *log-file* (or (u:getenv "LOG_FILE") *standard-output*))
(defparameter *log-severity-threshold*
  (u:make-keyword (or (u:getenv "LOG_SEVERITY") "DEBUG")))
(defparameter *log-suppress-health*
  (u:getenv "LOG_SUPPRESS_HEALTH" :type :boolean :default t))

;; Environment
(defparameter *version* (u:getenv "DATAUI_VERSION" :default "0.0"))
(defparameter *environment*
  (u:getenv "DATAUI_ENVIRONMENT" :default "dev"))

;; Directories
(defparameter *package-root* (asdf:system-relative-pathname :data-ui #P""))
(defparameter *doc-root*
  (let* ((dir (u:getenv "DOCUMENT_ROOT"
               :default "/app/shared-files/"))
          (normalized (probe-file dir)))
    (if normalized
      (format nil "~a" normalized)
      (error "Document root not found: ~a" dir))))
(defparameter *temp-directory*
  (let* ((dir (u:getenv "FS_TEMP_DIRECTORY"
                :default "/app/temp-files/"))
          (normalized (probe-file dir)))
    (if normalized
      (format nil "~a" normalized)
      (error "Temporary directory not found: ~a" dir))))

;; Other
(defparameter *large-page-size* 10000)

;; Defaults
(defparameter *default-file-permissions* '("create" "read" "update" "delete"))
(defparameter *default-directory-permissions*
  '("create" "read" "update" "delete" "list"))

;; Model
(defparameter *compiled-model* nil)
(defparameter *field-types* nil)
(defparameter *timestamp-regex*
  "^\\d{4}-[01][0-9]-[0-3][0-9][T ][0-2][[0-9]:[0-5][0-9]:[0-5][0-9]$")
(defparameter *uuid-regex*
  (ppcre:create-scanner
    (format nil "^~{~a~^-~}$"
      (list
        "[0-9a-fA-F]{8}"
        "[0-9a-fA-F]{4}"
        "[0-9a-fA-F]{4}"
        "[0-9a-fA-F]{4}"
        "[0-9a-fA-F]{12}"))))

(setf a:*default-page-size* 10000)
