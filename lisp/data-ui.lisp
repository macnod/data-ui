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

;; Public pages: the absolute origin (scheme, host, optional port,
;; nothing else) the renderer builds canonical /p/ URLs from.
;; Configured, never sniffed: dev default below, staging profile.env
;; (expose_profile), production https://{{DOMAIN}} (deploy template).
;; Stored as read; valid-public-origin (public-pages.lisp) validates
;; and canonicalizes at the start of every render, so a REPL can fix
;; it. Only the parameter lives here — data-ui.lisp loads before
;; aux.lisp, so report-e is unavailable in this file.
(defparameter *public-origin*
  (or (u:getenv "PUBLIC_ORIGIN")
    (format nil "http://127.0.0.1:~d"
      (u:getenv "HTTP_PORT" :default 8080 :type :integer))))

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
