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

;;
;; BEGIN Test Helpers
;;
;; These functions help avoid boiler plate in tests to make the tests easier
;; to buiild, and more readable. Functions of this type start with the prefix
;; `th-`.
;;

(defun th-make-user (name &key
                      (password "password-1")
                      (email "no-email")
                      roles)
  "Create a user via a:add-user, with NAME and PASSWORD. The a:add-user function
assigns default roles to the new user. Beyond that, this function assigns each
role in ROLES to the user, it the role hasn't been assigned yet. If a role in
ROLES does not exist, this function creates it. Returns NAME or NIL, depending
on success."
  ;; Create any roles in ROLES that don't already exist
  (loop for role in roles
    unless (a:get-id *rbac* "roles" role)
    do (a:add-role *rbac* role
         :description (format nil "Test role created for user ~a" user)))
  ;; Create a user with default roles
  (a:add-user *rbac* name email password)
  ;; Associate user with any roles in ROLES that the user doesn't already have
  (loop for role in roles
    unless (a:user-has-role *rbac* user role)
    do (a:add-user-role *rbac* user role))
  user)

(defun th-slurp-model (name)
  "Slurps the file with file name NAME and extension .lisp from the models
directory. Returns the file as a string."
  (u:slurp (u:join-paths *package-root* "models" (format nil "~a.lisp" name))))

(defun th-make-model-record (model-name user &key roles)
  "Insert a :models record via be-insert. Returns the record ID."
  (be-insert :models
    `(:name ,name
       :description (format nil "Test model ~s" name)
       :model ,(th-slurp-model "widgets"))
    user
    :roles roles))

(defun th-make-file (type-key logical-path &key
                      (data-string "bogus data") source-file)
  "Creates a file at the appropriate file-system location. The location is
derived from the system's root directory for files, TYPE, and LOGICAL-PATH,
and the contents of the file is taken from SOURCE-FILE or DATA-STRING. This
function ensures that the path portion of the file points to a path that
already exists in the file system and that is already tracked as a resource.
Returns a file token that is just like the one that the ReST endpoint
/api/upload returns, This file token can be used with BE-INSERT. "
  (unless (u:starts-with logical-path "/")
    (error "LOGICAL-PATH ~s must start with `/`"))
  (unless (getf *compiled-model* type-key)
    (error "UNKNOWN type key ~(~s~)" type-key))
  (unless (path-field type-key)
    (error "Type ~(~s~) is not file-system-backed" type-key))
  ;; Make sure that the parent directory exists in both the file system as well
  ;; as in the database
  (let* ((fs-path (fs-path type-key logical-path))
          (logical-parent (u:path-parent logical-path))
          (fs-parent (u:path-parent fs-path))
          (parent-type-key (u:tree-get *compiled-model* type-key :parent-type))
          (parent-path-field (path-field parent-type-key))
          (parent-rn (find-resource-name
                       parent-type-key
                       `((,parent-type-key
                           ,parent-path-field :eq ,logical-parent)))))
    (unless (u:directory-exists-p fs-parent)
      (error "Parent directory does not exist in file system: ~s" fs-parent))
    (unless parent-rn
      (error "Parent not in database: ~(s) ~s" parent-type-key logical-parent))
    (if source-file
      (u:copy-file source-file fs-path)
      (u:spew data-string fs-path))
    (u:safe-encode fs-path)))



;;
;; END Test Helpers
;;

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
