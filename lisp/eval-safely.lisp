(in-package :data-ui)

(defun eval-safely (string)
  "Evaluate STRING in the :data-ui package.
Returns (OUTPUT VALUES-STRING ERROR-STRING).
Errors are always returned, never propagated to the debugger."
  (handler-case
      (let ((*package* (find-package :data-ui))
            (out (make-string-output-stream))
            (err (make-string-output-stream)))
        (let ((*standard-output* out)
              (*error-output* err)
              (*trace-output* out))
          (let ((vals (multiple-value-list
                       (eval (read-from-string string)))))
            (list (get-output-stream-string out)
                  (format nil "~{~S~^~%~}" vals)
                  nil))))
    (serious-condition (e)
      (list ""
            ""
            (format nil "~A: ~A" (type-of e) e)))))
