(in-package :data-ui)

(def-suite backend-suite :description "Backend tests")

(in-suite backend-suite)

(test placeholders
  (is (equal (placeholders (list "a" "b" "c")) '("$1" "$2" "$3")))
  (let ((count 10))
    (is (equal
          (placeholders
            (loop for a from 0 below count
              collect (format nil "~a" (code-char (+ a 97)))))
          (loop for a from 1 to count
            collect (format nil "$~d" a)))))
  (is-false (placeholders nil)))

(test aggregate-values
  (is (equal (aggregate-values :first '(1 2 3)) 1))
  (is (equal (aggregate-values :list '(1 2 3)) '(1 2 3)))
  (is (equal (aggregate-values :distinct '(1 1 1 2 2 3)) '(1 2 3)))
  (is (equal (aggregate-values :first '("abc" "def")) "abc"))
  (is (equal (aggregate-values :first '("a")) "a"))
  (is (equal (aggregate-values :distinct '("a" "b" "a")) '("a" "b")))
  (signals error (aggregate-values :unknown '(1 2 3))))

(test field-values-for-id
  (set-model *model*)
  (let* ((id (a:get-id *rbac* "users" "admin"))
          (query (list "
select
  users.id            users_id,
  users.created_at    users_created_at,
  users.updated_at    users_updated_at,
  users.user_name     users_user_name,
  users.password_hash users_password_hash,
  users.email         users_email,
  roles.id            roles_id,
  roles.created_at    roles_created_at,
  roles.updated_at    roles_updated_at,
  roles.role_name     roles_role_name
from users
  join role_users on users.id = role_users.user_id
  join roles on role_users.role_id = roles.id
where
  users.id = $1
"
                   id))
          (view-result (a:with-rbac (*rbac*) (a:rbac-query query))))
    (is (equal (field-values-for-id view-result :users-id id :roles-role-name :first)
          "admin"))))

(test view-result-values
  (set-model *model*)
  (let* ((id (a:get-id *rbac* "users" "admin"))
          (query (list "
select
  users.id            users_id,
  users.created_at    users_created_at,
  users.updated_at    users_updated_at,
  users.user_name     users_user_name,
  users.password_hash users_password_hash,
  users.email         users_email,
  roles.id            roles_id,
  roles.created_at    roles_created_at,
  roles.updated_at    roles_updated_at,
  roles.role_name     roles_role_name
from users
  join role_users on users.id = role_users.user_id
  join roles on role_users.role_id = roles.id
where
  users.id = $1
"
                   id))
          (view-result (a:with-rbac (*rbac*) (a:rbac-query query)))
          (field-keys (form-field-keys :users :list-form))
          (result (car (view-result-values :users field-keys view-result)))
          (roles (getf result :roles)))
    (is (equal (getf result :id) id))
    (is (equal (getf result :name) "admin"))
    (is (equal (getf result :email) "no-email"))
    (is (equal (length roles) 3))
    (is (member "admin" roles :test 'equal))
    (is (member "logged-in" roles :test 'equal))
    (is (member "public" roles :test 'equal))))

(test view-result-ids
  (set-model *model*)
  (loop for id in (query "select id from rt_todos" :result-type :column)
    for resource-name = (id-to-resource-name id)
    do (a:remove-resource *rbac* resource-name))
  (loop for id in (query "select id from rt_tags" :result-type :column)
    for resource-name = (id-to-resource-name id)
    do (a:remove-resource *rbac* resource-name))
  (let* ((tag-names (loop for tag from 1 to 5
                      for tag-name = (format nil "~r" tag)
                      collect tag-name))
          (tag-ids (loop for tag-name in tag-names
                     collect (be-insert :tags `(:name ,tag-name))))
          (todo-ids (loop for todo from 10 to 15
                      for todo-name = (format nil "~r" todo)
                      for tags = (u:choose-some tag-names (+ 1 (mod todo 3)))
                      collect
                      (be-insert :todos `(:name ,todo-name :tags ,tags))))
          (tag-keys (form-field-keys :tags :list-form))
          (todo-keys (form-field-keys :todos :list-form))
          (tags-sql (u:tree-get *compiled-model* :tags :views :main :sql))
          (todos-sql (u:tree-get *compiled-model* :todos :views :main :sql))
          (tag-vr (a:with-rbac (*rbac*) (a:rbac-query (list tags-sql))))
          (todo-vr (a:with-rbac (*rbac*) (a:rbac-query (list todos-sql))))
          (tag-vr-ids (view-result-ids :tags tag-keys tag-vr))
          (todo-vr-ids (view-result-ids :todos todo-keys todo-vr)))
    (is (equal (sort tag-ids #'string<) (sort tag-vr-ids #'string<)))
    (is (equal (sort todo-ids #'string<) (sort todo-vr-ids #'string<)))))

(test next-param-index
  (is (= (next-param-index
           "select * from users where id = $1 and name = $2")
        3))
  (is (= (next-param-index
           "select * from users where id = $1 and name = $2 and email = $5")
           6))
  (is (= (next-param-index "") 1))
  (is (= (next-param-index "select * from users") 1))
  (is (= (next-param-index "select * from users where id = $10") 11)))

(test add-where-clause
  (is (equal
        (add-where-clause
          "
select users.user_name users_user_name from users"
          '((:users :name :eq "admin")))
        (list "
select users.user_name users_user_name from users
where
  users.user_name = $1
"
          "admin"))))

(test add-ids-clause
  (is (equal
        (add-ids-clause
          :users
          "
select users.user_name users_user_name from users"
          '("1" "2" "3"))
        (list "
select users.user_name users_user_name from users
where users.id in (
  $1,
  $2,
  $3
)"
          "1" "2" "3"))))

(test alias-keys
  (set-model *model*)
  (is (equal
        (alias-keys :users (form-field-keys :users :list-form))
        (list :users-id :users-user-name :users-email :roles-role-name)))
  (is (equal
        (alias-keys :users (form-field-keys :users :update-form))
        (list :users-id :users-created-at :users-updated-at :users-user-name
          :users-password-hash :users-email :roles-role-name)))
  (is (equal
        (alias-keys :todos (form-field-keys :todos :list-form))
        (list :rt-todos-id :rt-todos-created-at :rt-todos-updated-at
          :rt-todos-todo-name :rt-tags-tag-name))))

(test aggregations
  (set-model *model*)
  (is (equal
        (aggregations :todos (form-field-keys :todos :list-form))
        (list :first :first :first :first :list))))

(test form-field-keys
  (set-model *model*)
  (is (equal
        (form-field-keys :users :list-form)
        (list :id :name :email :roles)))
  (is (equal
        (form-field-keys :users :update-form)
        (list :id :created-at :updated-at :name :password :email :roles))))

(test resource-id-keys
  (set-model *model*)
  (is (equal (resource-id-keys :todos) '(:id :todo-id)))
  (is (equal (resource-id-keys :users) '(:id :user-id))))

(test local-fields
  (set-model *model*)
  (is (equal (local-fields :todos) '(:name)))
  (is (equal (local-fields :users) '(:name :password :email))))

(test resource-name
  (set-model *model*)
  (is (equal
        (resource-name :todos '(:name "Buy milk"))
        "todos:buy milk:062a683e"))
  (is (equal
        (resource-name :users '(:name "admin" :email "no-email"
                                 :password "password"))
        "users:admin:63acbe08")))

;; Credit: grok-4.20-0309-reasoning
(test insert-main-query
  (set-model *model*)
  ;; Happy path - uses compiled :insert-sql :main, supplies UUID for :id,
  ;; collects data values for subsequent keys (skipping :id)
  (let* ((type-key :tags)
         (insert (u:tree-get *compiled-model* type-key :insert-sql))
         (uuid "123e4567-e89b-12d3-a456-426614174000")
         (data '(:name "Test Tag"))
         (result (insert-main-query type-key insert uuid data))
         (main-qt (getf insert :main)))
    (is (and (consp result) (= 3 (length result))))
    (is (equal (first result) (first main-qt)))
    (is (equal (second result) uuid))
    (is (equal (third result) (getf data :name))))

  (let* ((type-key :todos)
         (insert (u:tree-get *compiled-model* type-key :insert-sql))
         (uuid "123e4567-e89b-12d3-a456-426614174000")
         (data '(:name "Test Todo"))
         (result (insert-main-query type-key insert uuid data))
         (main-qt (getf insert :main)))
    (is (and (consp result) (= 3 (length result))))
    (is (equal (first result) (first main-qt)))
    (is (equal (second result) uuid))
    (is (equal (third result) (getf data :name))))

  ;; Validation error paths (insert-main-query calls valid-insert, valid-uuid,
  ;; valid-data)
  (signals error (insert-main-query :nonexistent-type '(:main t) "bad-uuid" '(:name "x")))
  (let ((insert (u:tree-get *compiled-model* :tags :insert-sql))
        (uuid "123e4567-e89b-12d3-a456-426614174000"))
    (signals error (insert-main-query :tags nil uuid '(:name "foo")))
    (signals error (insert-main-query :tags insert "invalid-uuid-string" '(:name "foo")))
    (signals error (insert-main-query :tags insert uuid '(:unknown-field "value")))))

;; Credit: grok-4.20-0309-reasoning
(test id-from-filters
  (is-false (id-from-filters nil))
  (is-false (id-from-filters '((:users :name :eq "foo"))))
  (is (equal "123" (id-from-filters '((:users :id :eq "123")))))
  (is (equal "abc" (id-from-filters '((:users :name :eq "foo")
                                      (:todos :id :eq "abc")))))
  (is-false (id-from-filters '((:users :id :like "%123%")))))

;; Credit: grok-4.20-0309-reasoning
(test id-from-filters-and-data
  (set-model *model*)
  ;; Filters is string
  (is (equal "abc123" (id-from-filters-and-data :users "abc123")))
  ;; Data has :id
  (is (equal
        "def456"
        (id-from-filters-and-data
          :users '((:users :name :eq "foo")) '(:id "def456" :name "bar"))))
  ;; Uses id-from-filters
  (is (equal
        "ghi789"
        (id-from-filters-and-data :users '((:users :id :eq "ghi789")))))
  ;; No id in data or filters, does lookup (uses existing test data like admin
  ;; user)
  (let ((admin-id (a:get-id *rbac* "users" "admin")))
    (is (equal
          admin-id
          (id-from-filters-and-data :users '((:users :name :eq "admin"))))))
  ;; Returns nil when no match
  (is-false (id-from-filters-and-data
              :users '((:users :name :eq "nonexistent-user")))))

;; Credit: grok-4.20-0309-reasoning
(test update-roles
  (set-model *model*)
  (let ((test-todo-name "test-update-roles-todo")
        (test-tag-name "test-update-roles-tag"))
    ;; Cleanup from any previous partial test run
    (be-delete :todos `((:todos :name :eq ,test-todo-name)))
    ;; Force full cleanup of any stale RBAC resource (name collision on hash)
    (a:with-rbac (*rbac*)
      (let ((stale-rn (resource-name :todos `(:name ,test-todo-name))))
        (when (a:get-id *rbac* "resources" stale-rn)
          (a:remove-resource *rbac* stale-rn))))
    (a:with-rbac (*rbac*)
      (let ((stale-rn (resource-name :tags `(:name ,test-tag-name))))
        (when (a:get-id *rbac* "resources" stale-rn)
          (a:remove-resource *rbac* stale-rn)))) 
    ;; Standard DB cleanup
    (be-delete :todos `((:todos :name :eq ,test-todo-name)))
    (be-delete :tags `((:tags :name :eq ,test-tag-name))) 
    (be-delete :tags `((:tags :name :eq ,test-tag-name)))

    ;; Test with a new :todos record
    (let* ((todo-id (be-insert :todos `(:name ,test-todo-name)))
           (todo-data `(:name ,test-todo-name))
           (rn (resource-name :todos todo-data))
           (initial-roles (a:list-resource-role-names *rbac* rn)))
      (is (member "admin" initial-roles :test #'equal))
      ;; Update to add roles (forces admin)
      (is (null (update-roles :todos todo-data '("public" "logged-in"))))
      (let ((updated-roles (a:list-resource-role-names *rbac* rn)))
        (is (subsetp '("admin" "logged-in" "public") updated-roles :test #'equal))
        (is (= 3 (length updated-roles))))
      ;; Update to remove all but admin (use existing role)
      (is (null (update-roles :todos todo-data '("logged-in"))))
      (let ((final-roles (a:list-resource-role-names *rbac* rn)))
        (is (equal '("admin" "logged-in") (sort (copy-list final-roles) #'string<))))
      (be-delete :todos todo-id))

    ;; Test adding roles to a new :tags record
    (let* ((tag-id (be-insert :tags `(:name ,test-tag-name)))
           (tag-data `(:name ,test-tag-name))
           (rn (resource-name :tags tag-data)))
      (is (null (update-roles :tags tag-data '("public"))))
      (let ((roles (a:list-resource-role-names *rbac* rn)))
        (is (equal '("admin" "public") (sort (copy-list roles) #'string<))))
      (be-delete :tags tag-id))))

(test list-ids
  (set-model *model*)
  ;; Empty values list returns NIL/falsey
  (is-false (list-ids :todos :name nil))
  (is (null (list-ids :todos :name '())))
  ;; Returns list of UUID strings for matching names (uses existing test data)
  (let ((todo-ids (list-ids :todos :name '("ten" "twelve" "fifteen"))))
    (is (= 3 (length todo-ids)))
    (is (every #'stringp todo-ids)))
  (let ((tag-ids (list-ids :tags :name '("one" "four"))))
    (is (= 2 (length tag-ids)))
    (is (every #'stringp tag-ids)))
  ;; Validation errors
  (signals error (list-ids :nonexistent-type :name '("x")))
  (signals error (list-ids :todos :unknown-field '("x")))
  (signals error (list-ids :todos :name "not-a-list")))

(test id-to-resource-name
  (set-model *model*)
  ;; Returns resource name string for valid ID (uses existing test data)
  (let* ((todo-id (car (list-ids :todos :name '("ten"))))
         (tag-id (car (list-ids :tags :name '("one"))))
         (todo-rn (id-to-resource-name todo-id))
         (tag-rn (id-to-resource-name tag-id)))
    (is (stringp todo-rn))
    (is (search "todos:ten:" todo-rn))
    (is (stringp tag-rn))
    (is (search "tags:one:" tag-rn)))
  ;; Returns NIL for non-existent ID
  (is-false (id-to-resource-name "00000000-0000-0000-0000-000000000000"))
  ;; Validation (expects valid UUID or queries safely)
  (is-false (id-to-resource-name nil)))

(test eformat
  ;; Single newlines collapse to spaces (paragraph flow)
  (is (equal "Hello world. This is a test."
             (eformat "Hello world.
This is a test.")))
  ;; Double newlines preserve paragraph breaks
  (is (equal "Paragraph one.

Paragraph two."
             (eformat "Paragraph one.

Paragraph two.")))
  ;; Indented lines (bullets, lists, poems) are preserved
  (is (equal "Here's a poem:

  - She drew a circle
  - That shut me out
  - Heretic rebel
  - A thing to flout

Do you like it?"
             (eformat "
Here's a poem:

  - She drew a circle
  - That shut me out
  - Heretic rebel
  - A thing to flout

Do you like it?
")))
  ;; Numbers, letters, other whitespace-prefixed lines preserved
  (is (equal "Steps:
  1. First
  2. Second

Notes:
  a. Alpha
  b. Beta"
             (eformat "Steps:
  1. First
  2. Second

Notes:
  a. Alpha
  b. Beta")))
  ;; Trimming of leading/trailing whitespace and newlines
  (is (equal "Trimmed text."
             (eformat "

  Trimmed text.


")))
  ;; Formatting with params (like FORMAT)
  (is (equal "Value: 42 and string: hello"
             (eformat "Value: ~a and string: ~a" 42 "hello")))
  ;; Edge cases
  (is (equal "" (eformat "")))
  (is (equal "" (eformat "


")))
  (is (equal "Single line." (eformat "Single line."))))

(test validation-error
  (signals validation-error (signal-validation-error "Test error."))
  (handler-case
      (signal-validation-error "Invalid ~s for ~s." :foo :bar)
    (validation-error (e)
      (is (equal "Invalid :FOO for :BAR." (princ-to-string e)))
      (is (eq 'validatioN (context e))))))

(test valid-filter
  (set-model *model*)
  ;; Valid filter (no error)
  (is (null (valid-filter '(:todos :name :eq "test"))))
  ;; Required but missing
  (signals validation-error (valid-filter nil :required t))
  ;; Not a list
  (signals validation-error (valid-filter "not-a-list"))
  ;; Wrong number of elements
  (signals validation-error (valid-filter '(:todos :name :eq)))
  ;; Invalid type key
  (signals validation-error (valid-filter '(:invalid-type :name :eq "test")))
  ;; Invalid field key
  (signals validation-error (valid-filter '(:todos :invalid-field :eq "test")))
  ;; Invalid operator
  (signals validation-error (valid-filter '(:todos :name :invalid-op "test")))
  ;; Invalid value type
  (signals validation-error (valid-filter '(:todos :name :eq 123))))

(test valid-filters
  (set-model *model*)
  ;; Valid filters list (no error)
  (is (null (valid-filters '((:todos :name :eq "test")
                             (:tags :name :eq "tag1")))))
  ;; Required but NIL/empty
  (signals validation-error (valid-filters nil :required t))
  (signals validation-error (valid-filters '() :required t))
  ;; Invalid filter inside list
  (signals validation-error (valid-filters '((:todos :name :eq "ok")
                                             "invalid-filter"))))

(test valid-type-key
  (set-model *model*)
  (is (null (valid-type-key :todos)))
  (signals validation-error (valid-type-key :invalid-type))
  (signals validation-error (valid-type-key nil))
  (signals validation-error (valid-type-key "string")))

(test valid-field-key
  (set-model *model*)
  (is (null (valid-field-key :todos :name)))
  (signals validation-error (valid-field-key :todos :invalid-field))
  (signals validation-error (valid-field-key :invalid-type :name))
  (signals validation-error (valid-field-key nil :name)))

(test valid-data
  (set-model *model*)
  ;; Valid data for :todos
  (is (null (valid-data :todos '(:name "Test Todo" :tags '("one" "two")))))
  ;; Invalid type-key
  (signals validation-error (valid-data :invalid-type '(:name "x")))
  ;; Invalid field
  (signals validation-error (valid-data :todos '(:invalid-field "x")))
  ;; Wrong value type
  (signals validation-error (valid-data :todos '(:name 123)))
  ;; Invalid UUID for :id
  (signals validation-error (valid-data :todos '(:id "not-uuid")))
  ;; Join table must be list of atomic values
  (signals validation-error (valid-data :todos '(:tags '(1 (2 3))))))

(test valid-uuid
  (is (null (valid-uuid "123e4567-e89b-12d3-a456-426614174000")))
  (signals validation-error (valid-uuid "not-uuid"))
  (signals validation-error (valid-uuid nil))
  (signals validation-error (valid-uuid 123)))

(test valid-insert
  (set-model *model*)
  (let ((insert (u:tree-get *compiled-model* :todos :insert-sql)))
    (is (null (valid-insert insert))))
  (signals validation-error (valid-insert nil))
  (signals validation-error (valid-insert '(:not-main t))))

(test valid-roles
  (is (null (valid-roles '("admin" "public"))))
  (is (null (valid-roles nil)))
  (is (null (valid-roles '("abc-def" "abc:def"))))
  (signals validation-error (valid-roles '("abc def")))
  (signals validation-error (valid-roles '("ADMIN" "PUBLIC")))
  (signals validation-error (valid-roles '("Admin" "Public")))
  (signals validation-error (valid-roles '("!@#$%%%^" ")(*)*)SDF"))))

(test valid-existing-roles
  (is (null (valid-existing-roles '("admin"))))
  (signals validation-error (valid-existing-roles '("nonexistent-role"))))

(test valid-values-list
  (is (null (valid-values-list '("a" "b"))))
  (is (null (valid-values-list nil)))
  (signals validation-error (valid-values-list "not-list"))
  (signals validation-error (valid-values-list '(1 (2 3)))))

(test id-key
  (set-model *model*)
  (is (eq :todo-id (id-key :todos)))
  (is (eq :user-id (id-key :users)))
  (is (eq :tag-id (id-key :tags)))
  (is (eq :role-id (id-key :roles)))
  ;; Edge cases
  (signals validation-error (id-key nil))
  (signals validation-error (id-key :foo)))

(test be-id
  (set-model *model*)
  (let ((test-todo-name "test-be-id-todo")
        (test-tag-name "test-be-id-tag"))
    ;; Cleanup any previous test data
    (be-delete :todos `((:todos :name :eq ,test-todo-name)))
    (be-delete :tags `((:tags :name :eq ,test-tag-name)))

    ;; Create test records
    (let ((tag-id (be-insert :tags `(:name ,test-tag-name)))
           (todo-id (be-insert :todos
                      `(:name ,test-todo-name :tags (,test-tag-name)))))

      ;; By string UUID
      (is (equal todo-id (be-id :todos todo-id)))
      (is (equal tag-id (be-id :tags tag-id)))
      ;; By filter on unique field
      (is (equal todo-id (be-id :todos `((:todos :name :eq ,test-todo-name)))))
      (is (equal tag-id (be-id :tags `((:tags :name :eq ,test-tag-name)))))
      ;; No match returns NIL
      (is-false (be-id :todos '((:todos :name :eq "nonexistent-todo"))))
      ;; Cleanup
      (be-delete :todos todo-id)
      (be-delete :tags tag-id)))

  ;; Invalid inputs
  (signals error (be-id :invalid-type '((:users :name :eq "x"))))
  (signals error (be-id :users "not-uuid-string")))
