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
                       (when ,seeding-fn (funcall ,seeding-fn)))))
       (let ((*fixture* ,fixture))
         ,@body))))

;;
;; BEGIN Test Helpers
;;
;; These functions help avoid boiler plate in tests to make the tests easier
;; to buiild, and more readable. Functions of this type start with the prefix
;; `th-`.
;;

(defun th-make-user (user &key
                      (password "password-1")
                      (email "no-email")
                      roles)
  "Create a user via be-insert, with name USER and PASSWORD. Using be-insert
ensures that :post-create hooks (e.g., creating a settings row) fire.
Beyond that, this function assigns each role in ROLES to the user, if the
role hasn't been assigned yet. If a role in ROLES does not exist, this
function creates it. Returns USER or NIL, depending on success."
  ;; Create any roles in ROLES that don't already exist
  (loop for role in roles
    unless (a:get-id *rbac* "roles" role)
    do (a:add-role *rbac* role
         :description (format nil "Test role created for user ~a" user)))
  ;; Create a user via be-insert so :post-create hooks fire
  (be-insert :users
    `(:name ,user :password ,password :email ,email)
    "admin")
  ;; Associate user with any roles in ROLES that the user doesn't already have
  (loop for role in roles
    unless (a:user-has-role *rbac* user role)
    do (a:add-user-role *rbac* user role))
  user)

(defun th-slurp-model (name)
  "Slurps the file with file name tests/model-template.lisp, replaces the
place-holders in the file with NAME, and returns the resulting model as a
string. This is useful for creating test models when the attributes of the
model don't matter."
  (let* ((template-file (u:join-paths *package-root*
                          "tests" "model-template.lisp"))
          (template (u:slurp template-file)))
    (re:regex-replace-all ":model:" template name)))

(defun th-make-model (model-name user &key roles)
  "Insert a :models record via be-insert. Returns the record ID."
  (be-insert :models
    `(:name ,model-name
       :description ,(format nil "Test model ~s" model-name)
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
    (error "LOGICAL-PATH ~s must start with `/`" logical-path))
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
      (error "Parent not in database: ~(~s) ~s" parent-type-key logical-parent))
    (if source-file
      (u:copy-file source-file fs-path)
      (u:spew (format nil "~a~%" data-string) fs-path))
    (u:safe-encode fs-path)))

(defun th-make-mb-image (logical-path user model-name
                          &key
                          (roles (u:tree-get *compiled-model* 
                                   :images :type-roles))
                          (source-data (format nil "bogus image data for ~a"
                                         logical-path))
                          source-file)
  "Creates an image by simulating an upload. This function creates the file in
the correct directory and creates the associated resource in the database,
in the same way that the file and resource would be created if the image was
uploaded via the ReST API. In the case of this helper, the image that is being
created is defined in modelbank. That image includes foreign-key fields to
the :users type and to the :models type.

LOGICAL-PATH is the logical path of the image file. Something like /file.txt.
The logical path must start with a / and must end in a file name. The path
that precedes the file name must exist. The only path that exists by default
is /.

USER is a string that identifies the user.

MODEL-NAME is a string that identifies the model.

ROLES is a list of strings representing the roles that you want to assign to
the new image. These roles must already exist.

SOURCE-DATA is a string. The string will be written to the file, and the file
will not be a proper image file. But the file will exist and allow us to pass
some tests.

SOURCE-FILE is a file-system path to an existing image file. This file will
be copied to the final location of the image file. If this value is specified
SOURCE-DATA will be ignored.
"

  (let ((images-resource (type-resource-name :images))
         (models-resource (type-resource-name :models)))
    (unless (a:user-allowed *rbac* user "create" images-resource)
      (error "User ~s does not have ~s permission on ~s."
        user "create" :images))
    (unless (a:user-allowed *rbac* user "create" models-resource)
      (error "User ~s does not have ~s permission on reosurce ~s."
        user "create" :models))
    (let ((file-token (th-make-file 
                        :images logical-path 
                        :data-string source-data
                        :source-file source-file))
           (path-field (path-field :images)))
      ;; Insert image
      (be-insert :images (list
                           path-field logical-path
                           :user user
                           :model model-name)
        user
        :roles roles
        :file-token file-token))))

(defun th-make-directory (type-key logical-path &key
                           (roles '("directories-user")))
  "Insert a directory record. Returns the directory record ID."
  (let* ((path-field (path-field type-key))
          (id (be-id type-key `((,type-key ,path-field :eq ,logical-path)))))
    (unless id
      (let* ((fs-path (fs-path type-key logical-path))
              (path-parent (u:path-parent logical-path))
              (fs-path-parent (u:path-parent fs-path))
              (is-dir (and (u:starts-with logical-path "/")
                        (u:ends-with logical-path "/"))))
        (unless (u:directory-exists-p fs-path-parent)
          (error "Parent directory does not exist: ~a." fs-path-parent))
        (unless is-dir
          (error "Directory must start and end with a /."))
        (ensure-directories-exist fs-path)
        (a:add-resource *rbac*
          (make-resource-name type-key `(,path-field ,logical-path))
          :roles roles
          :description (format nil "Test directory ~a" logical-path))))))

(defun th-seed-scoping-fixture ()
  "Seed shared state for the scoping test suite. Creates 2 users (user-1,
user-2), with appropriate roles and one model. Returns a plist, bound to 
*fixture*, that looks like this:
  (:users (\"user-1\" \"user-2\") :models (\"test-model\"))"
  (th-make-user "user-1" :roles '("role-1"))
  (th-make-user "user-2" :roles '("role-1"))
  (th-make-model "model-1" "user-1" :roles '("role-1"))
  (list :users '("user-1" "user-2") :models '("model-1")))
  
;;
;; END Test Helpers
;;

(defun run-backend-tests ()
  (with-model "test-model" nil
    (let ((results (append (run 'backend-suite)
                     (run 'predicates-suite))))
      (explain! results)
      results)))

(defun run-scoping-tests ()
  (with-model "modelbank" nil
    (let ((results (run 'scoping-suite)))
      (explain! results)
      results)))

(defun run-hook-registry-tests ()
  "Run hook registry unit tests (no model needed) then integration tests against
both test-model and modelbank."
  (let ((results
          (append (run 'hook-registry-suite)
            (with-model "test-model" nil
              (run 'hook-registry-integration-suite))
            (with-model "modelbank" nil
              (run 'hook-registry-modelbank-suite)))))
    (explain! results)
    results))

(defun run-lifecycle-tests ()
  "Run lifecycle hook tests."
  (with-model "test-model" nil
    (let ((results (run 'hook-registry-lifecycle-suite)))
      (explain! results)
      results)))

(defun run-action-tests ()
  "Run action hook tests."
  (with-model "test-model" nil
    (let ((results (run 'action-suite)))
      (explain! results)
      results)))

(defun run-secrets-tests ()
  "Run secrets type tests."
  (with-model "test-model" nil
    (let ((results (run 'secrets-suite)))
      (explain! results)
      results)))

(defun run-widget-tests ()
  "Widget allow-list and UI emission tests."
  (with-model "test-model" nil
    (let ((results (run 'widget-suite)))
      (explain! results)
      results)))

(defun run-m2m-tests ()
  "Multiple M2M joiners per type — compile and runtime tests."
  (with-model "m2m-test" #'seed-m2m-fixture
    (let ((results (run 'm2m-suite)))
      (explain! results)
      results)))

(defun run-generator-tests ()
  "Generate-model hook tests."
  (let ((results (run 'generator-suite)))
    (explain! results)
    results))

(defun seed-m2m-fixture ()
  "Seed tags and verify admin user exists for M2M runtime tests."
  (be-insert :tags '(:name "red") "admin")
  (be-insert :tags '(:name "blue") "admin")
  (be-insert :tags '(:name "green") "admin")
  nil)

(defun run-nullable-fk-tests ()
  "Nullable foreign-key field tests."
  (with-model "nullable-fk-test" nil
    (let ((results (run 'nullable-fk-suite)))
      (explain! results)
      results)))

(defun run-static-options-tests ()
  "Static dropdown :options tests."
  (with-model "static-select-test" nil
    (let ((results (run 'static-options-suite)))
      (explain! results)
      results)))

(defun run-form-fields-tests ()
  "Compile-time form field validation tests."
  (let ((results (run 'form-fields-suite)))
    (explain! results)
    results))

(defun run-tests ()
  "Run all test suites and print a consolidated summary at the end.
Each run-* helper returns a list of FiveAM result objects; this
function collects them, prints per-group reports via explain!, and
then prints a final summary showing total checks, failures, and
which groups had failures."
  (let* ((groups
           (list
             (cons "backend"        (run-backend-tests))
             (cons "scoping"        (run-scoping-tests))
             (cons "hook-registry"  (run-hook-registry-tests))
             (cons "lifecycle"      (run-lifecycle-tests))
             (cons "action"         (run-action-tests))
             (cons "secrets"        (run-secrets-tests))
             (cons "widget"         (run-widget-tests))
             (cons "m2m"            (run-m2m-tests))
             (cons "generator"      (run-generator-tests))
             (cons "nullable-fk"    (run-nullable-fk-tests))
             (cons "static-options" (run-static-options-tests))
             (cons "form-fields"    (run-form-fields-tests))))
          (all-results (loop for g in groups append (cdr g)))
          (total (length all-results))
          (failed (loop for r in all-results
                    when (typep r 'fiveam::test-failure)
                    collect r))
          (failed-groups (loop for g in groups
                           unless (every #'fiveam::test-passed-p
                                    (cdr g))
                           collect (car g))))
    (format t "~2&========================================~%")
    (if failed-groups
      (progn
        (format t "~d/~d checks FAILED in:~%" (length failed) total)
        (loop for name in failed-groups
          do (format t "  ~a~%" name)))
      (format t "All ~d checks passed across ~d groups.~%"
        total (length groups)))
    (format t "========================================~%")))
