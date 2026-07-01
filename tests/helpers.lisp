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

(defun th-make-user (user &key
                      (password "password-1")
                      (email "no-email")
                      roles)
  "Create a user via a:add-user, with name USER and PASSWORD. The a:add-user
function assigns default roles to the new user. Beyond that, this function
assigns each role in ROLES to the user, it the role hasn't been assigned yet. If
a role in ROLES does not exist, this function creates it. Returns USER or NIL,
depending on success."
  ;; Create any roles in ROLES that don't already exist
  (loop for role in roles
    unless (a:get-id *rbac* "roles" role)
    do (a:add-role *rbac* role
         :description (format nil "Test role created for user ~a" user)))
  ;; Create a user with default roles
  (a:add-user *rbac* user email password)
  ;; Associate user with any roles in ROLES that the user doesn't already have
  (loop for role in roles
    unless (a:user-has-role *rbac* user role)
    do (a:add-user-role *rbac* user role))
  user)

(defun th-slurp-model (name)
  "Slurps the file with file name NAME and extension .lisp from the models
directory. Returns the file as a string."
  (u:slurp (u:join-paths *package-root* "models" (format nil "~a.lisp" name))))

(defun th-make-model (model-name user &key roles)
  "Insert a :models record via be-insert. Returns the record ID."
  (be-insert :models
    `(:name ,model-name
       :description (format nil "Test model ~s" model-name)
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
    (error "LOGICAL-PATH ~s must start with `/`" logial-path))
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

(defun th-make-mb-image (logical-path user model-name
                          &key
                          (roles (u:tree-get *compiled-model* 
                                   :images :type-roles))
                          (source-data "bogus image data")
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

(defun run-tests ()
  (with-model "test-model" (lambda ())
    (run! 'backend-suite)
    (run! 'predicates-suite)
    (run! 'rest-suite)))
  ;; TODO: Include scoping tests here
  ;; (with-model "modelbank" (lambda ())
  ;;   (run! 'scoping-suite)))

;; TODO: Remove after all modelbank tests are in place. After that, use
;; RUN-TESTS to run all the tests.
(defun run-modelbank-tests ()
  (with-model "modelbank" (lambda ())
    (run! 'scoping-suite)))
