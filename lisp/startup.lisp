(in-package :data-ui)

;; This file is loaded last (see data-ui.asd), so the functions here can
;; freely reference anything defined in the rest of the system, such as
;; SET-MODEL and START-WEB-SERVER.

;; Swank
(defparameter *swank-port* (u:getenv "SWANK_PORT" :type :integer))
(defparameter *swank-interface*
  (u:getenv "SWANK_INTERFACE" :default "0.0.0.0"))
(defparameter *swank-server* nil)

(defun start-swank-server ()
  (when (and *swank-port* (not *swank-server*))
    (pl:pinfo :in "run" :status "starting swank")
    (setf *swank-server*
      (swank:create-server
        :interface *swank-interface*
        :port *swank-port*
        :style :spawn
        :dont-close t))))

(defun init ()
  "One-time process initialization: logging, swank and database. Safe to call
from a REPL; does not block."
  (when *log-file*
    (pl:make-log-stream "data-ui" *log-file*))
  (start-swank-server)
  (init-database))

(defun deploy-incoming-pid-alive-p (pid)
  ":private: Liveness for registry PIDs: /proc/<pid> existence in any state
(zombie included) means live. The reaping process did not spawn the child
(a freshly started profile, or a different profile), so process-status and
waitpid cannot answer; every ambiguity (including PID reuse) errs toward
'live', which retains and reports — the required direction."
  (and (integerp pid) (plusp pid)
       (uiop:directory-exists-p
         (parse-namestring (format nil "/proc/~a/" pid)))))

(defun reap-deploy-incoming-orphans (&optional (package-root *package-root*))
  "Reap provably-completed deploy-incoming entries in PACKAGE-ROOT (default
the loaded checkout's). Called from MAIN between (init) and (set-model),
and from the named REPL after (init) — before the application HTTP server
accepts requests and before the REPL prompt. Swank is already listening by
then, which is intended: Swank is an operator channel, so the reap stays
AFTER start-swank-server. The registry is per checkout and every staging
profile reaps it, which is intended; do not special-case Model Bank. A
silent no-op when deploy-incoming/ is absent (production containers,
development); handles entries one by one; tolerates ENOENT from concurrent
reapers or the owning worker (two profiles booting together both succeed).

Reap rule: done, pending with a dead worker, or ready with a dead child
authorizes removing the incoming file, THEN the record, in that order.
Unrecognizable records and stray incoming files are retained and reported
(operator-only remediation — see 'staging modelbank-orphans'); automatic
reaping covers only provable completion. A stale temp file is removed only
together with its token's reapable record; otherwise it is reported as
stale-temp. Returns the list of reports ((kind path reason) ...)."
  (let ((dir (deploy-model-incoming-dir package-root)))
    (unless (uiop:directory-exists-p dir)
      (return-from reap-deploy-incoming-orphans nil))
    (let ((reports '())
          (reaped-tokens '()))
      (labels
          ((report (kind path reason)
             (push (list kind path reason) reports))
           (remove-quietly (path)
             (handler-case (delete-file path) (file-error ())))
           (reap (token incoming record state)
             ;; incoming file first, then the record, in that order.
             (remove-quietly incoming)
             (remove-quietly record)
             (push token reaped-tokens)
             (report :reaped (namestring incoming) state)))
        ;; Registry records.
        (dolist (record (directory (merge-pathnames "*.registry" dir)))
          (let* ((token (pathname-name record))
                 (incoming (merge-pathnames
                             (format nil "~a.lisp" token) dir)))
            (multiple-value-bind (state worker child _path ok reason)
                (parse-deploy-registry-record record)
              (declare (ignore _path))
              (cond
                ((not ok)
                 (report :unrecognizable (namestring record) reason))
                ((not (uiop:file-exists-p incoming))
                 (report :missing-incoming (namestring record) state))
                ((equal state "done")
                 ;; The worker died between the done ack and its own
                 ;; cleanup: completion is provable.
                 (reap token incoming record "done"))
                ((equal state "pending")
                 (unless (deploy-incoming-pid-alive-p worker)
                   (reap token incoming record "pending/dead-worker")))
                ((equal state "ready")
                 (unless (deploy-incoming-pid-alive-p child)
                   (reap token incoming record "ready/dead-child")))))))
        ;; Incoming files without a record: the worker died between the
        ;; file write and the record write — not provable, retain+report.
        (dolist (incoming (directory (merge-pathnames "*.lisp" dir)))
          (let* ((token (pathname-name incoming))
                 (record (merge-pathnames
                          (format nil "~a.registry" token) dir)))
            (unless (or (uiop:file-exists-p record)
                        (member token reaped-tokens :test #'equal))
              (report :no-record (namestring incoming)
                      "incoming file without a registry record"))))
        ;; Stale temps: removed only together with their token's reapable
        ;; record (already reaped above); anything else is reported.
        ;; Temp names are .<token>.lisp.tmp / .<token>.registry.tmp —
        ;; the pathname TYPE is "tmp" and the NAME keeps the leading
        ;; dot (e.g. name ".abc.lisp"), so split on the type, never by
        ;; name-suffix.
        (dolist (tmp (uiop:directory-files dir))
          (let ((name (pathname-name tmp))
                (type (pathname-type tmp)))
            (when (and (stringp name) (>= (length name) 2)
                       (char= #\. (char name 0))
                       (equal type "tmp"))
              (let* ((stem (subseq name 1))
                     (token
                       (cond
                         ((and (>= (length stem) 5)
                               (string= ".lisp"
                                        (subseq stem (- (length stem) 5))))
                          (subseq stem 0 (- (length stem) 5)))
                         ((and (>= (length stem) 9)
                               (string= ".registry"
                                        (subseq stem (- (length stem) 9))))
                          (subseq stem 0 (- (length stem) 9)))
                         (t stem))))
                (if (member token reaped-tokens :test #'equal)
                  (remove-quietly tmp)
                  (report :stale-temp (namestring tmp) token)))))))
      ;; Non-fatal by design: every failure path retains and reports.
      (dolist (r reports)
        (pl:pinfo :in "reap-deploy-incoming-orphans" :report r))
      (reverse reports))))

(defun hard-reset ()
  "Reload all source files and reset the database. Nearly equivalent to
restarting the image, but without quitting."
  (asdf:load-system :data-ui :force t)
  (in-package :data-ui)
  (init-database)
  (reset-database)
  t)

(defun main (&optional (model (u:getenv "MODEL_NAME")))
  "Container entry point. Initializes the process, reaps deploy-incoming
orphans (before the application HTTP server accepts requests — Swank is
already up, which is intended), loads MODEL (which starts the web server),
then blocks forever to keep the process alive. MODEL defaults to the
MODEL_NAME environment variable. Do not call from a REPL; use INIT and
SET-MODEL instead."
  (init)
  (reap-deploy-incoming-orphans)
  (set-model model)
  (loop (sleep 60)))
