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

(defun run-tests ()
  (query "delete from resources")
  (query "delete from users where user_name not in ($1, $2)"
    :params '("admin" "guest"))
  (query "delete from roles where role_name not in ($1, $2, $3, $4, $5)"
    :params '("admin" "logged-in" "public"
               "admin:exclusive" "guest:exclusive"))
  (set-model *model*)
  (run-all-tests))
