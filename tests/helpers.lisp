(in-package :data-ui)

;; Other
(defparameter *package-root* (asdf:system-relative-pathname :data-ui #P""))

;; Improved error checking
(defmacro error-matches (expr regex failure-text)
  `(handler-case
      (progn
        ,expr
        (fail ,failure-text))
     (error (e)
       (is (re:scan ,regex (format nil "~a" e))))))
