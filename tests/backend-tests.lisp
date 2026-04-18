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
