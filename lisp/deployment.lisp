(asdf:defsystem :deployment :description "Deployment helpers")

(defpackage :deployment (:use :cl))

(in-package :deployment)

(defparameter *root*
  (let ((lisp-folder "/lisp/")
         (lisp-path (format nil "~a"
                     (asdf:system-relative-pathname :deployment #P""))))
    (subseq lisp-path 0 (- (length lisp-path) (length lisp-folder)))))

(defun top-level-model-field (key-string model-file &key model-path)
  "Print the top-level KEY-STRING field of the model in MODEL-FILE
(bare name resolved under models/) — or, when MODEL-PATH is given,
of the model at that checkout-relative path (FR-9: VIP deploys read
models/local/<name>.lisp without it being committed)."
  (let* ((path (or (and model-path
                     (format nil "~a/~a" *root* model-path))
             (format nil "~a/models/~a.lisp" *root* model-file)))
          (model (with-open-file (in path) (cadr (read in))))
          (key (intern (string-upcase key-string) :keyword))
          (raw-value (getf model key))
          (value (cond
                   ((equal key :repl) (if raw-value "true" "false"))
                   (t raw-value))))
    (format t "~a~%" value)))
