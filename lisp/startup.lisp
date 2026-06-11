(in-package :data-ui)

;; This file is loaded last (see data-ui.asd), so the functions here can
;; freely reference anything defined in the rest of the system, such as
;; SET-MODEL and START-WEB-SERVER.

;; Swank
(defparameter *swank-port* (u:getenv "SWANK_PORT" :type :integer))
(defparameter *swank-server* nil)

(defun start-swank-server ()
  (when (and *swank-port* (not *swank-server*))
    (pl:pinfo :in "run" :status "starting swank")
    (setf *swank-server*
      (swank:create-server
        :interface "0.0.0.0"
        :port *swank-port*
        :style :spawn
        :dont-close t))))

(defun init ()
  "One-time process initialization: logging, swank and database.
Safe to call from a REPL; does not block."
  (when *log-file*
    (pl:make-log-stream "data-ui" *log-file*))
  (start-swank-server)
  (init-database))

(defun main (&optional (model "default-model"))
  "Container entry point. Initializes the process, loads MODEL (which
starts the web server), then blocks forever to keep the process alive.
Do not call from a REPL; use INIT and SET-MODEL instead."
  (init)
  (set-model model)
  (loop (sleep 60)))
