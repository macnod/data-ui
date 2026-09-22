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

SOURCE-FILE is a file-system path to an existing image file. This file will be
copied to the final location of the image file. If this value is specified
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
  (:users (\"user-1\" \"user-2\") :models (\"model-1\"))"
  (th-make-user "user-1" :roles '("role-1"))
  (th-make-user "user-2" :roles '("role-1"))
  (th-make-model "model-1" "user-1" :roles '("role-1"))
  (list :users '("user-1" "user-2") :models '("model-1")))

(defun th-seed-users (count &rest roles)
  "Add some users, tags, and todo items to the todos app."
  (loop with names = (u:choose-some
                       (mapcar
                         #'string-downcase
                         (re:split "\\n"
                           (u:slurp
                             (u:join-paths *package-root* "tests/names.txt"))))
                       count)
    for name in names
    for password = (format nil "password-1-~a" (u:safe-encode name))
    for email = "no-email"
    for listing = (be-list :users "admin" :filters `((:users :name :eq ,name)))
    for exists = (plusp (length (getf listing :records)))
    unless exists
    do (be-insert
         :users
         `(:name ,name :password ,password :email ,email)
         "admin"
         :roles roles)
    and count name))

(defun th-seeded-user-password (user)
  (format nil "password-1-~a" (u:safe-encode user)))

(defun th-seed-chores (count)
  "Add some tags and chores to the chores model."
  (let ((new-tags (loop for a from 1 to 9 collect (format nil "tag-~d" a)))
         (existing-tags (mapcar
                          (lambda (r) (getf r :name))
                          (getf (be-list :tags "admin" :limit 1000) :records))))
    (loop for tag in new-tags
      unless (u:has existing-tags tag) do
      (be-insert :tags `(:name ,tag) "admin" :roles '("chore-users")))
    (loop
      with existing-chores = (mapcar
                               (lambda (c) (getf c :name))
                               (getf 
                                 (be-list :chores "admin" :limit 1000)
                                 :records))
      for a from 1 to count
      for name = (format nil "chore-~2,'0d" a)
      unless (u:has existing-chores name) do
      (be-insert :chores
        (list
          :name name
          :description (format nil "Long description for ~a" name)
          :points (1+ (mod a 4))
          :tags (u:choose-some new-tags 2))
        "admin"
        :roles '("chore-users"))
      and count name)))

(defun th-seed-tags (count &rest roles)
  (loop 
    with new-tags = (loop for a from 1 to count
                      collect (format nil "tag-~d" a))
    and existing-tags = (mapcar
                          (lambda (r) (getf r :name))
                          (getf (be-list :tags "admin" :limit 1000) :records))
    for tag in new-tags
    unless (u:has existing-tags tag) do
    (be-insert :tags `(:name ,tag) "admin" :roles roles)
    finally (return new-tags)))

(defun th-seed-todos (count &rest roles)
  (loop
    with tags = (mapcar
                  (lambda (r) (getf r :name))
                  (getf (be-list :tags "admin" :limit 1000) :records))
    with existing-todos = (mapcar
                            (lambda (r) (getf r :name))
                            (getf
                              (be-list :todos "admin" :limit 1000)
                              :records))
    for a from 1 to count
    for todo = (format nil "todo-~2,'0d" a)
    for points = (random 5)
    for todo-tag-count = (1+ (random 3))
    for todo-tags = (u:choose-some tags todo-tag-count)
    unless (u:has existing-todos todo) do
    (be-insert :todos
      (list
        :name todo
        :points points
        :tags todo-tags)
      "admin"
      :roles roles)))

(defun th-seed-todos-real ()
  (let* ((todos-path "/home/macnod/workbench/todos-demo.lisp")
          (todos (with-open-file (in todos-path) (cadr (read in))))
          (roles '("public" "todo-users")))
    (loop
      with existing-tags = (mapcar 
                             (lambda (x) (getf x :name))
                             (getf
                               (be-list :tags "admin" :limit 1000)
                               :records))
      for tag in (getf todos :tags)
      unless (member tag existing-tags)
      do (be-insert :tags `(:name ,tag) "admin" :roles roles))
    (loop
      with existing-todos = (mapcar
                              (lambda (x) (getf x :name))
                              (getf
                                (be-list :todos "admin" :limit 1000)
                                :records))
      for todo in (getf todos :todos)
      for name = (getf todo :title)
      for tags = (getf todo :tags)
      for done = (if (getf todo :done) :true :false)
      unless (member todo existing-todos)
      do (be-insert :todos `(:name ,name :done ,done :tags ,tags)
           "admin" :roles roles))))
      
      

(defun th-seed-recurring (count)
  "Add some tags and recurring chores to the recurring model."
  (let ((new-tags (loop for a from 1 to 9 collect (format nil "tag-~d" a)))
         (existing-tags (mapcar
                          (lambda (r) (getf r :name))
                          (getf (be-list :tags "admin" :limit 1000) :records))))
    (loop for tag in new-tags
      unless (u:has existing-tags tag) do
      (be-insert :tags `(:name ,tag) "admin" :roles '("chore-users")))
    (loop
      with existing-chores = (mapcar
                               (lambda (c) (getf c :name))
                               (getf 
                                 (be-list :chores "admin" :limit 1000)
                                 :records))
      for a from 1 to count
      for name = (format nil "chore-~2,'0d" a)
      unless (u:has existing-chores name) do
      (be-insert :chores
        (list
          :name name
          :description (format nil "Long description for ~a" name)
          :points (1+ (mod a 4))
          :tags (u:choose-some new-tags 2))
        "admin"
        :roles '("chore-users"))
      and count name)))
  

(defun th-seed-models (count)
  "Add some models to the modelbank model."
  (loop with users = (u:exclude
                       (mapcar
                         (lambda (r) (getf r :name))
                         (getf (be-list :users "admin" :limit 1000) :records))
                       '("admin" "guest"))
    and roles = '("models-user")
    and existing-models = (mapcar
                            (lambda (m) (getf m :name))
                            (getf 
                              (be-list :users "admin" :limit 1000)
                              :records))
    for a from 1 to count
    for name = (format nil "Model ~2,'0d" a)
    for user = (u:choose-one users)
    for description = (format nil "Description of ~a" name)
    for model = (format nil "'(:title ~s)" name)
    for rating = (1+ (random 5))
    unless (u:has existing-models name) do
    (be-insert :models
      (list
        :name name
        :user user
        :description description
        :model model
        :rating rating)
      user
      :roles roles)
    and count name))

(defun th-seed-books-model-users (books-directory)
  (loop with existing-users = (u:exclude
                                (mapcar
                                  (lambda (r) (getf r :name))
                                  (getf
                                    (be-list :users "admin" :limit 1000)
                                    :records))
                                '("admin" "guest"))
    with new-users = (u:exclude
                       (re:split "\\n"
                         (u:slurp
                           (u:join-paths books-directory "book-users.txt")))
                       existing-users)
    and roles = '("public" "books-user")
    and email = "no-email"
    for user in new-users
    for password = (format nil "password-1-~a" (u:safe-encode user))
    do (be-insert :users
         `(:name ,user :password ,password :email ,email)
         "admin"
         :roles roles)
    counting user))

(defun th-seed-book-cover (title source)
  (let* ((logical-path (u:join-paths "/" (u:filename-only source)))
          (file-token (th-make-file :covers logical-path :source-file source))
          (in-database (be-list :covers "admin" 
                         :filters `((:covers :name :eq ,logical-path)))))
    (unless (plusp (length (getf in-database :records)))
      (be-insert :covers
        `(:name ,logical-path :book ,title)
        "admin"
        :roles '("public" "books-user")
        :file-token file-token))))

(defun th-seed-books-model-authors (books-directory)
  (loop with existing-authors = (mapcar 
                                  (lambda (b) (getf b :title))
                                  (getf 
                                    (be-list :authors "admin" :limit 1000)
                                    :records))
    with books-path = (u:join-paths books-directory "books.lisp")
    with books = (with-open-file (in books-path) (cadr (read in)))
    and new-author-count = 0
    for book in books
    for authors = (getf book :authors)
    do (loop for author in authors
         unless (u:has existing-authors author) do
         (be-insert :authors `(:name ,author) "admin"
           :roles '("public" "books-user"))
         (push author existing-authors)
         (incf new-author-count))
    finally (return new-author-count)))

(defun th-seed-books-model-books (books-directory)
  (loop with existing-books = (mapcar
                                (lambda (b) (getf b :title))
                                (getf
                                  (be-list :books "admin" :limit 1000)
                                  :records))
    with books-path = (u:join-paths books-directory "books.lisp")
    with books = (with-open-file (in books-path) (cadr (read in)))
    with one-day = (* 3600 24)
    with ten-years = (* one-day 365 10)
    with ten-years-ago = (- (get-universal-time) ten-years)
    for book in books
    for title = (getf book :title)
    for genre = (getf book :genre)
    for description = (getf book :description)
    for isbn = (getf book :isbn)
    for authors = (getf book :authors)
    for covers = (getf book :cover)
    for random-time = (- (+ ten-years-ago (random ten-years)) one-day)
    for published = (dt:timestamp-string :universal-time  random-time)
    unless (u:has existing-books title) do
    (be-insert :books
      `(:title ,title
         :genre ,genre
         :description ,description
         :isbn ,isbn
         :authors ,authors
         :published ,published)
      "admin"
      :roles '("public" "books-user"))
    and count book))

(defun th-seed-books-model-covers (books-directory)
  (loop with existing-covers = (mapcar
                                 (lambda (c) (getf c :name))
                                 (getf
                                   (be-list :covers "admin" :limit 1000)
                                   :records))
    and books-path = (u:join-paths books-directory "books.lisp")
    with books = (with-open-file (in books-path) (cadr (read in)))
    for book in books
    for title = (getf book :title)
    for covers = (getf book :cover)
    for front-source = (car covers)
    for back-source = (cadr covers)
    for front-name = (u:join-paths "/" (u:filename-only front-source))
    for back-name = (u:join-paths "/" (u:filename-only back-source))
    unless (u:has existing-covers front-name)
    do (th-seed-book-cover title front-source)
    and count front-source
    unless (u:has existing-covers back-name)
    do (th-seed-book-cover title back-source)
    and count back-source))

(defun th-seed-books-model-user-ratings (books-directory)
  (loop with users = (re:split "\\n"
                         (u:slurp
                           (u:join-paths books-directory "book-users.txt")))
    for user in users do
    (loop with books = (u:choose-some
                         (mapcar
                           (lambda (b) (getf b :title))
                           (getf
                             (be-list :books "admin" :limit 1000)
                             :records))
                         (random 20))
      for book in books
      for rating = (1+ (random 5))
      do (be-insert :my-ratings
           `(:book ,book :user ,user :rating ,rating)
           "admin"
           :roles '("public" "books-user")))))

(defun th-seed-books-model (books-directory)
  (th-seed-books-model-users books-directory)
  (th-seed-books-model-authors books-directory)
  (th-seed-books-model-books books-directory)
  (th-seed-books-model-covers books-directory)
  (th-seed-books-model-user-ratings books-directory))

;;
;; END Test Helpers
;;

(defun run-backend-tests (&optional collect)
  (let ((results
          (with-model "test-model" nil
            (append (run 'backend-suite)
                    (run 'predicates-suite)))))
    (explain! results)
    (when collect results)))

(defun run-phase-a-tests (&optional collect)
  "Run Phase A (id-first paging) tests only."
  (let ((results
          (with-model "test-model" nil
            (run 'backend-suite))))
    ;; Filter to just phase-a tests
    (let ((phase-a-results
            (loop for r in results
              when (re:scan "phase-a"
                     (princ-to-string (type-of r)))
              collect r)))
      (explain! results)
      (when collect results))))

(defun run-phase-b-tests (&optional collect)
  "Run Phase B (hydrate/collapse) tests.
The first three tests run under test-model (simple, has tags M2M).
The m2m fan-out test runs under m2m-test (two join tables)."
  (let ((results
          (append
            (with-model "test-model" nil
              (append
                (fiveam:run 'phase-b-hydrate-exact-id-set)
                (fiveam:run 'phase-b-no-extra-filters)
                (fiveam:run 'phase-b-preserves-phase-a-order)))
            (with-model "m2m-test" #'seed-m2m-fixture
              (fiveam:run 'phase-b-m2m-fanout-collapse)))))
    (explain! results)
    (when collect results)))

(defun run-scoping-tests (&optional collect)
  (let ((results
          (with-model "modelbank-test" nil
            (run 'scoping-suite))))
    (explain! results)
    (when collect results)))

(defun run-hook-registry-tests (&optional collect)
  "Run hook registry unit tests (no model needed) then integration
tests against both test-model and modelbank-test."
  (let ((results
          (append (run 'hook-registry-suite)
                  (with-model "test-model" nil
                    (run 'hook-registry-integration-suite))
                  (with-model "modelbank-test" nil
                    (run 'hook-registry-modelbank-suite)))))
    (explain! results)
    (when collect results)))

(defun run-lifecycle-tests (&optional collect)
  "Run lifecycle hook tests."
  (let ((results
          (with-model "test-model" nil
            (run 'hook-registry-lifecycle-suite))))
    (explain! results)
    (when collect results)))

(defun run-action-tests (&optional collect)
  "Run action hook tests."
  (let ((results
          (with-model "test-model" nil
            (run 'action-suite))))
    (explain! results)
    (when collect results)))

(defun run-secrets-tests (&optional collect)
  "Run secrets type tests."
  (let ((results
          (with-model "test-model" nil
            (run 'secrets-suite))))
    (explain! results)
    (when collect results)))

(defun run-widget-tests (&optional collect)
  "Widget allow-list and UI emission tests."
  (let ((results
          (with-model "test-model" nil
            (run 'widget-suite))))
    (explain! results)
    (when collect results)))

(defun run-m2m-tests (&optional collect)
  "Multiple M2M joiners per type — compile and runtime tests."
  (let ((results
          (with-model "m2m-test" #'seed-m2m-fixture
            (run 'm2m-suite))))
    (explain! results)
    (when collect results)))

(defun run-generator-tests (&optional collect)
  "Generate-model hook tests.

Pure function tests run without any model.  Integration tests share a
single modelbank-test context (one reset-database + set-model) instead
of one per test.  The integration tests are self-contained: they seed
or delete the llm-config secret as needed, and th-gen-make-user is
idempotent so repeated calls for the same user are safe."
  (let ((results
          (append
            ;; Pure function tests (no model, no DB)
            (run 'generator-pure-suite)
            ;; Integration tests (single shared modelbank-test context)
            (with-model "modelbank-test" nil
              (run 'generator-integration-suite)))))
    (explain! results)
    (when collect results)))

(defun seed-m2m-fixture ()
  "Seed tags and verify admin user exists for M2M runtime tests."
  (be-insert :tags '(:name "red") "admin")
  (be-insert :tags '(:name "blue") "admin")
  (be-insert :tags '(:name "green") "admin")
  nil)

(defun seed-bi-m2m-fixture ()
  "Seed authors and books without join rows for bi-m2m-test."
  (be-insert :authors '(:name "A1") "admin")
  (be-insert :authors '(:name "A2") "admin")
  (be-insert :authors '(:name "A3") "admin")
  (be-insert :books '(:title "B1") "admin")
  (be-insert :books '(:title "B2") "admin")
  (be-insert :books '(:title "B3") "admin")
  nil)

(defun run-bi-m2m-tests (&optional collect)
  "Bidirectional M2M tests (models/test fixtures only)."
  (let ((results
          (append
            (with-model "bi-m2m-test" #'seed-bi-m2m-fixture
              (run 'bi-m2m-suite))
            ;; One-way / multi-joiner regression: stable test fixture
            (with-model "m2m-test" #'seed-m2m-fixture
              (run 'bi-m2m-one-way-regression-suite)))))
    (explain! results)
    (when collect results)))

(defun run-nullable-fk-tests (&optional collect)
  "Nullable foreign-key field tests."
  (let ((results
          (with-model "nullable-fk-test" nil
            (run 'nullable-fk-suite))))
    (explain! results)
    (when collect results)))

(defun run-static-options-tests (&optional collect)
  "Static dropdown :options tests."
  (let ((results
          (with-model "static-select-test" nil
            (run 'static-options-suite))))
    (explain! results)
    (when collect results)))

(defun run-form-fields-tests (&optional collect)
  "Compile-time form field validation tests."
  (let ((results (run 'form-fields-suite)))
    (explain! results)
    (when collect results)))

(defun run-compose-tests (&optional collect)
  "Compose-string lifecycle hook and data-effect contract tests.
Unit tests run bare (no model needed). Behavioral tests share a
single compose-test context."
  (let ((results
          (append
            ;; Pure unit tests (no model, no DB)
            (run 'compose-unit-suite)
            ;; Behavioral tests in shared context
            (with-model "compose-test" nil
              (run 'compose-suite)))))
    (explain! results)
    (when collect results)))

(defun run-compose-sugar-tests (&optional collect)
  "Field-level :compose sugar tests.
Compile-error tests run bare. Behavioral tests share a single
compose-sugar-test context."
  (let ((results
          (append
            ;; Compile-error tests (no model needed)
            (run 'compose-sugar-unit-suite)
            ;; Behavioral tests in shared context
            (with-model "compose-sugar-test" nil
              (run 'compose-sugar-suite)))))
    (explain! results)
    (when collect results)))

(defun run-sortable-tests (&optional collect)
  "Sortable field attribute and sort behavior tests."
  (let ((results
          (with-model "test-model" nil
            (run 'sortable-suite))))
    (explain! results)
    (when collect results)))

(defun run-searchable-tests (&optional collect)
  "Searchable field attribute and search behavior tests (test-model)."
  (let ((results
          (with-model "test-model" nil
            (run 'searchable-suite))))
    (explain! results)
    (when collect results)))

(defun run-search-or-tests (&optional collect)
  "OR-across-fields search tests (search-test fixture)."
  (let ((results
          (with-model "search-test" nil
            (run 'search-or-suite))))
    (explain! results)
    (when collect results)))

(defun run-rollup-tests (&optional collect)
  "08a rollup compile-surface tests (compile-only, no DB)."
  (let ((results (run 'rollup-suite)))
    (explain! results)
    (when collect results)))

(defun run-measure-rollup-tests (&optional collect)
  "08b measure Phase A SQL tests (generator parts + be-list branch).
Seeds the measure-rollup-test fixture once."
  (let ((results
          (with-model "measure-rollup-test" #'mrb-seed-fixture
            (run 'measure-rollup-suite))))
    (explain! results)
    (when collect results)))

(defun run-sort-measures-tests (&optional collect)
  "09 sort + page on measures: compile relaxation, ORDER BY aliases,
tiebreaker, NULLS LAST, default sort, :sort echo. Seeds the
rollup-sort-test fixture once."
  (let ((results
          (with-model "rollup-sort-test" #'sms-seed-fixture
            (run 'sort-measures-suite))))
    (explain! results)
    (when collect results)))

(defun run-default-sort-tests (&optional collect)
  ":default-sort type attribute: compile probes plus base and
rollup nil-sort behavior. Seeds the default-sort-test fixture
once; the suite itself is read-only."
  (let ((results
          (with-model "default-sort-test" #'dss-seed-fixture
            (run 'default-sort-suite))))
    (explain! results)
    (when collect results)))

(defun run-filtered-rollup-tests (&optional collect)
  "Run plan 10's behavioral rollup suite (generalization gate).
Test 10 opens its own unseeded context inside the suite."
  (let ((results
          (with-model "rollup-test" #'rt-seed-fixture
            (run 'filtered-rollup-suite))))
    (explain! results)
    (when collect results)))

(defun run-agg-distinct-tests (&optional collect)
  "M2M row-display :agg :distinct contract: compile-time probes
(no model needed) plus behavioral two-chain and single-chain
collapse tests, each in its own model context.

The test-model fixture has :button fields whose actions resolve
from the registry at compile time, so the test action hooks must
be registered before set-model. Loading action-tests.lisp does
that (and is harmless when already loaded)."
  (load (u:join-paths *package-root* "tests" "action-tests.lisp"))
  (let ((results
          (append
            ;; Compile-time probes only (the behavioral tests are
            ;; run by name below, inside their model contexts)
            (fiveam:run 'agg-distinct-compile-tests)
            ;; Behavioral: two joiner chains (seed tags)
            (with-model "m2m-test" #'seed-m2m-fixture
              (fiveam:run 'ad-two-chains-values-appear-once))
            ;; Behavioral: single-chain regression (seed one tag)
            (with-model "test-model"
              (lambda ()
                (be-insert :tags '(:name "red") "admin")
                nil)
              (fiveam:run 'ad-single-chain-round-trip)))))
    (explain! results)
    (when collect results)))

(defun run-update-permission-tests (&optional collect)
  "be-update permission gate suite on the modelbank-test fixture."
  (let ((results
          (with-model "modelbank-test" #'th-up-seed
            (run 'update-permission-suite))))
    (explain! results)
    (when collect results)))

(defun seed-spawn-fixture ()
  "Seed the spawn-test fixture for :spawn behavior tests."
  (th-make-user "worker" :roles '("item-users"))
  (be-insert :tags '(:name "home") "admin" :roles '("item-users"))
  (be-insert :tags '(:name "yard") "admin" :roles '("item-users"))
  (be-insert :items
    (list :name "mow-the-lawn"
      :points 3
      :tags '("home" "yard")
      :notes "prior-round-notes")
    "worker"
    :roles '("item-users"))
  nil)

(defun run-spawn-tests (&optional collect)
  ":spawn action hook tests: compile probes (no model needed) plus
close-and-respawn behavior on the spawn-test fixture."
  (let ((results
          (append
            ;; Compile-time probes (pure validate-model, no DB)
            (fiveam:run 'spawn-compile-probes)
            ;; Behavioral tests in shared seeded context
            (with-model "spawn-test" #'seed-spawn-fixture
              (fiveam:run 'spawn-behavior)))))
    (explain! results)
    (when collect results)))

(defun run-new-roles-tests (&optional collect)
  ":new-roles top-level key tests.
Validation probes run bare (no model). DB-init tests run in a
new-roles-test context."
  (let ((results
          (append
            ;; Compile-time probes (no model, no DB)
            (run 'new-roles-validation-suite)
            ;; set-model behavior (declared roles in RBAC)
            (with-model "new-roles-test" nil
              (run 'new-roles-db-suite)))))
    (explain! results)
    (when collect results)))

(defun run-guest-tests (&optional collect)
  ":guest-allowed top-level key tests.
Validation probes run bare (no model). Login behavior runs in a
guest-allowed-test context."
  (let ((results
          (append
            ;; Compile-time probes (no model, no DB)
            (run 'guest-validation-suite)
            ;; Login behavior (fixture sets :guest-allowed t)
            (with-model "guest-allowed-test" nil
              (run 'guest-db-suite)))))
    (explain! results)
    (when collect results)))

(defun run-api-roles-tests (&optional collect)
  ":api-roles top-level key tests.
Validation probes run bare (no model). Accessor and behavior tests
run in an api-roles-test context."
  (let ((results
          (append
            ;; Compile-time probes (no model, no DB)
            (run 'api-roles-validation-suite)
            ;; Accessor + guest behavior (fixture sets
            ;; :api-roles ("logged-in" "public"))
            (with-model "api-roles-test" nil
              (run 'api-roles-db-suite)))))
    (explain! results)
    (when collect results)))

(defun run-type-roles-override-tests (&optional collect)
  ":type-roles override on base-model types: compile probes
plus behavioral RBAC checks. Compile probes run bare; behavior
runs in a type-roles-override-test context."
  (let ((results
          (append
            ;; Compile-time probes (no model, no DB)
            (fiveam:run 'type-roles-override-compile-tests)
            ;; Behavioral (fixture restricts :users / :roles /
            ;; :permissions to admin)
            (with-model "type-roles-override-test" #'tro-seed-fixture
              (fiveam:run 'type-roles-override-behavior)))))
    (explain! results)
    (when collect results)))

(defun run-domain-stg-tests (&optional collect)
  ":domain-stg top-level key tests.
Validation probes run bare (no model). Accessor tests run in a
domain-stg-test context (fixture declares :domain only; the
derived value is asserted there)."
  (let ((results
          (append
            ;; Compile-time probes (no model, no DB)
            (run 'domain-stg-validation-suite)
            ;; Accessor (fixture declares :domain only)
            (with-model "domain-stg-test" nil
              (run 'domain-stg-db-suite)))))
    (explain! results)
    (when collect results)))

(defun run-tests ()
  "Run all test suites and print a consolidated summary at the end.
Each run-* helper is called with collect t so its result objects
are collected for counting. The summary shows total wall-clock time,
total checks, failures, which groups had failures, and the 5 slowest
groups."
  (let* ((start (get-internal-real-time))
         (timings nil)
         (groups
           (loop for (name . fn) in
                 '(("backend"        . run-backend-tests)
                   ("scoping"        . run-scoping-tests)
                   ("hook-registry"  . run-hook-registry-tests)
                   ("lifecycle"      . run-lifecycle-tests)
                   ("action"         . run-action-tests)
                   ("secrets"        . run-secrets-tests)
                   ("widget"         . run-widget-tests)
                   ("m2m"            . run-m2m-tests)
                   ("bi-m2m"         . run-bi-m2m-tests)
                   ("generator"      . run-generator-tests)
                   ("nullable-fk"    . run-nullable-fk-tests)
                   ("static-options" . run-static-options-tests)
                   ("form-fields"    . run-form-fields-tests)
                   ("compose"        . run-compose-tests)
                   ("compose-sugar"  . run-compose-sugar-tests)
                   ("sortable"       . run-sortable-tests)
                   ("searchable"     . run-searchable-tests)
                   ("search-or"      . run-search-or-tests)
                   ("rollup"         . run-rollup-tests)
                   ("measure-rollup" . run-measure-rollup-tests)
                   ("sort-measures"  . run-sort-measures-tests)
                   ("default-sort"   . run-default-sort-tests)
                   ("filtered-rollup" . run-filtered-rollup-tests)
                   ("agg-distinct"    . run-agg-distinct-tests)
                   ("update-permission" . run-update-permission-tests)
                   ("spawn"          . run-spawn-tests)
                   ("new-roles"      . run-new-roles-tests)
                   ("guest"          . run-guest-tests)
                   ("api-roles"      . run-api-roles-tests)
                   ("type-roles-override" . run-type-roles-override-tests)
                   ("domain-stg"     . run-domain-stg-tests))
                 for t0 = (get-internal-real-time)
                 for results = (funcall fn t)
                 for elapsed = (/ (- (get-internal-real-time) t0)
                                  internal-time-units-per-second)
                 do (push (cons name elapsed) timings)
                 collect (cons name results)))
         (wall (/ (- (get-internal-real-time) start)
                  internal-time-units-per-second))
         (all-results (loop for g in groups append (cdr g)))
         (total (length all-results))
         (failed (loop for r in all-results
                    when (typep r 'fiveam::test-failure)
                    collect r))
         (failed-groups (loop for g in groups
                          unless (every #'fiveam::test-passed-p
                                   (cdr g))
                          collect (car g)))
         (slowest (subseq (sort timings #'> :key #'cdr) 0
                          (min 5 (length timings)))))
    (format t "~2&========================================~%")
    (if failed-groups
      (progn
        (format t "~d/~d checks FAILED in:~%" (length failed) total)
        (loop for name in failed-groups
          do (format t "  ~a~%" name)))
      (format t "All ~d checks passed across ~d groups.~%"
        total (length groups)))
    (format t "Time: ~,2fs~%" wall)
    (format t "Slowest groups:~%")
    (loop for (name . secs) in slowest
          do (format t "  ~a: ~,2fs~%" name secs))
    (format t "========================================~%")))
