(in-package :data-ui)

(def-suite predicates-suite :description "Predicate tests")

(in-suite predicates-suite)

(test uuid-p
  (is-true (uuid-p  "b94f1905-464d-4da6-b6c8-878b9d014fc9"))
  (is-false (uuid-p "b94f1905-464d-4da6-b6c8-878b9d014fc"))
  (is-false (uuid-p ""))
  (is-false (uuid-p 123))
  (is-false (uuid-p nil)))

(test timestamp-p
  (is-true (timestamp-p "2026-04-17T14:38:01"))
  (is-true (timestamp-p "2026-04-17 14:38:01"))
  (is-false (timestamp-p "12345"))
  (is-false (timestamp-p nil))
  (is-false (timestamp-p ""))
  (is-false (timestamp-p "20260417 143801"))
  (is-false (timestamp-p "20260417")))

(test filters-require-join-p
  (is-true (filters-require-join-p
             :users
             '((:users :name :eq "alice")
                (:roles :name :eq "admin"))))
  (is-false (filters-require-join-p
              :users
              '((:users :name :eq "alice")
                 (:users :password :eq "alice-password"))))
  (is-false (filters-require-join-p
              :permissions
              '((:permissions :name :eq "read")
                 (:permissions :name :eq "create")))))

(test type-key-p
  (set-model *model*)
  (is-true (type-key-p :users))
  (is-true (type-key-p :roles))
  (is-true (type-key-p :resources))
  (is-true (type-key-p :permissions))
  (is-true (type-key-p :role-users))
  (is-true (type-key-p :resource-roles))
  (is-true (type-key-p :todos))
  (is-true (type-key-p :tags))
  (is-true (type-key-p :todo-tags))
  (is-false (type-key-p :non-existent))
  (is-false (type-key-p :role-resources)))

(test field-type-key-p
  (set-model *model*)
  (is-true (field-key-p :users :id))
  (is-true (field-key-p :users :name))
  (is-true (field-key-p :users :password))
  (is-true (field-key-p :users :roles))
  (is-true (field-key-p :roles :name))
  (is-true (field-key-p :resources :name))
  (is-true (field-key-p :permissions :name))
  (is-true (field-key-p :todos :created-at))
  (is-true (field-key-p :todos :name))
  (is-true (field-key-p :todos :tags))
  (is-true (field-key-p :tags :name))
  (is-true (field-key-p :todo-tags :id))
  (is-true (field-key-p :todo-tags :todo-id))
  (is-true (field-key-p :todo-tags :tag-id))
  (is-false (field-key-p :users :non-existent))
  (is-false (field-key-p :non-existent :name)))

(test operator-key-p
  (is-true (operator-key-p :eq))
  (is-true (operator-key-p :ne))
  (is-true (operator-key-p :gt))
  (is-true (operator-key-p :lt))
  (is-true (operator-key-p :gte))
  (is-true (operator-key-p :lte))
  (is-true (operator-key-p :in))
  (is-true (operator-key-p :not-in))
  (is-false (operator-key-p :non-existent))
  (is-false (operator-key-p nil))
  (is-false (operator-key-p "eq"))
  (is-false (operator-key-p 123)))

(test filter-p
  (is-true (filter-p '(:users :name :eq "alice")))
  (is-true (filter-p '(:users :roles :in '("admin" "editor"))))
  (is-false (filter-p '(:users :non-existent :eq "value")))
  (is-false (filter-p '(:non-existent :name :eq "value")))
  (is-false (filter-p '(nil :name :eq "value")))
  (is-false (filter-p '(:users nil :eq "value")))
  (is-false (filter-p '(:users :name nil "value")))
  (is-false (filter-p '(:users :name :eq nil)))
  (is-false (filter-p '(:users :name :eq 123)))
  (is-false (filter-p "hello"))
  (is-false (filter-p 123))
  (is-false (filter-p '(:users :name "alice")))
  (is-false (filter-p '(:users :name :eq)))
  (is-false (filter-p '(:users))))

(test value-type-p
  (set-model *model*)
  (is-true (value-type-p :users :name "alice"))
  (is-true (value-type-p :users :roles '("admin" "editor")))
  (is-false (value-type-p :users :name 123))
  (is-false (value-type-p :users :name nil))
  (is-true (value-type-p :todos :created-at "2026-04-17T14:38:01"))
  (is-true (value-type-p :users :id "b94f1905-464d-4da6-b6c8-878b9d014fc9"))
  (is-false (value-type-p :todos :id "b94f1905-464d-4da6-b6c8-878b9d014fc"))
  (is-false (value-type-p :tags :name 123))
  (is-true (value-type-p :tags :name "urgent")))
