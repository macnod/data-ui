(asdf:defsystem :deployment :description "Deployment helpers")

(defpackage :deployment (:use :cl))

(in-package :deployment)

(defparameter *root*
  (let ((lisp-folder "/lisp/")
         (lisp-path (format nil "~a"
                     (asdf:system-relative-pathname :deployment #P""))))
    (subseq lisp-path 0 (- (length lisp-path) (length lisp-folder)))))

(defun top-level-model-field (key-string &optional (model-file "default-model"))
  (let* ((path (format nil "~a/models/~a.lisp" *root* model-file))
          (model (with-open-file (in path) (read in)))
          (key (intern (string-upcase key-string) :keyword))
          (raw-value (getf model key))
          (value (cond
                   ((equal key :repl) (if raw-value "true" "false"))
                   (t raw-value))))
    (format t "~a~%" value)))
