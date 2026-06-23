(in-package :data-ui)

(defparameter *test-model* "test-model")
(defparameter *fixture* nil)

;; TODO: Dead?
;; Improved error checking
(defmacro error-matches (expr regex failure-text)
  `(handler-case
      (progn
        ,expr
        (fail ,failure-text))
     (error (e)
       (is (re:scan ,regex (format nil "~a" e))))))

(defmacro with-model (model-name seeding-fn &body body)
  (let ((fixture (gensym "fixture")))
    `(let ((,fixture (progn
                       (reset-database)
                       (set-model ,model-name)
                       (funcall ,seeding-fn))))
       (let ((*fixture* ,fixture))
         ,@body))))

(defun seed-scoping-fixture ())

(defun run-tests ()
  (with-model "test-model" (lambda ())
    (run! 'backend-suite)
    (run! 'predicates-suite)
    (run! 'rest-suite))
  (with-model "modelbank" #'seed-scoping-fixture
    (run! 'scoping-suite)))

;; TODO: Remove after all modelbank tests are in place. After that, use
;; RUN-TESTS to run all the tests.
(defun run-modelbank-tests ()
  (with-model "modelbank" #'seed-scoping-fixture
    (run! 'scoping-suite)))

