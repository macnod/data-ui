(in-package :data-ui)

(def-suite update-permission-suite
  :description "be-update permission gate tests (modelbank-test fixture)")

(in-suite update-permission-suite)

(defun th-up-seed ()
  "Seed modelbank-test: alice and bob are models-user peers; alice owns
one model with no extra roles (creator exclusive only), so bob cannot
read it."
  (th-make-user "alice" :roles '("models-user"))
  (th-make-user "bob" :roles '("models-user"))
  (be-insert :models '(:name "up-model" :description "gate test") "alice"))

(defun th-up-model-id ()
  (be-value-id :models :name "up-model" "admin"))

(defun th-up-cleanup ()
  (let ((id (th-up-model-id)))
    (when id
      (a:remove-resource *rbac* (id-to-resource-name id))
      (a:with-rbac (*rbac*)
        (a:rbac-query (list "delete from rt_models where id = $1" id))))))

(defmacro th-up-err (expr)
  "Evaluate EXPR, return the error string (or :no-error)."
  `(handler-case
       (progn ,expr :no-error)
     (error (e) (format nil "~a" e))))

(test be-update-peer-denied
  "A models-user peer without record access gets the permission error."
  (let ((id (th-up-model-id)))
    (is-true id)
    (is-false (user-allowed-resource "bob" id "update"))
    (let ((e (th-up-err (be-update :models id '(:rating 5) "bob"))))
      (is (re:scan "does not have update permission" e)
          "got: ~a" e))
    ;; same for the full-payload retry from the bug report
    (let ((e (th-up-err
               (be-update :models id
                 '(:name "up-model" :rating 5) "bob"))))
      (is (re:scan "does not have update permission" e)
          "got: ~a" e))))

(test be-update-no-partial-write
  "Denied update leaves no main-row write and no write-through orphans."
  (let ((id (th-up-model-id)))
    (is-true id)
    (signals error (be-update :models id '(:name "up-model" :rating 5)
                  "bob"))
    (let* ((row (car (a:with-rbac (*rbac*)
                  (a:rbac-query
                    (list "select model_name, model_description
                           from rt_models where id = $1" id)))))
           (ratings (a:with-rbac (*rbac*)
                      (a:rbac-query
                        (list "select count(*) from rt_ratings rt
                               join resources r on r.id = rt.id
                               where r.resource_name like 'ratings:%'")
                        :single))))
      (is (equal (getf row :model-name) "up-model"))
      (is (equal (getf row :model-description) "gate test"))
      (is (zerop (or ratings 0))
          "no write-through ratings rows expected, got ~a" ratings))))

(test be-update-unmatched-filters
  "Filters that match nothing fail with the no-record error."
  (let ((e (th-up-err
             (be-update :models '((:models :name :eq "no-such-model"))
               '(:description "x") "admin"))))
    (is (re:scan "No record of type :MODELS matches" e)
        "got: ~a" e)))

(test be-update-bogus-uuid
  "A well-formed but nonexistent UUID fails with the permission error
(bogus ids carry no roles)."
  (let ((e (th-up-err
             (be-update :models (u:uuid) '(:description "x") "admin"))))
    (is (re:scan "does not have update permission on record" e)
        "got: ~a" e)))

(test be-update-creator-and-admin-succeed
  "Creator and admin can still update; write-through fires for the creator."
  (let ((id (th-up-model-id)))
    (is-true id)
    (is (equal id (be-update :models id '(:rating 4) "alice")))
    (is (equal id (be-update :models id
                    '(:description "admin edited") "admin")))
    (let ((row (car (a:with-rbac (*rbac*)
                 (a:rbac-query
                   (list "select model_description from rt_models
                          where id = $1" id)))))
          (rating (a:with-rbac (*rbac*)
                    (a:rbac-query
                      (list "select rating_rating from rt_ratings rt
                             join resources r on r.id = rt.id
                             where r.resource_name like 'ratings:%'
                             and rt.rating_user = (select id from users
                                                    where user_name = 'alice')")
                      :single))))
      (is (equal (getf row :model-description) "admin edited"))
      (is (= 4 rating) "alice's write-through rating expected, got ~a"
          rating))))

(test be-update-base-type-path
  "Admin updating a base-type (:users) row still works (password
omitted means keep the existing hash)."
  (let* ((bob-id (be-value-id :users :name "bob" "admin"))
         (result (be-update :users bob-id
                    '(:name "bob" :password nil
                      :email "bob@data-ui.test")
                    "admin")))
    (is (equal result bob-id))
    (is (equal "bob@data-ui.test"
          (a:with-rbac (*rbac*)
            (a:rbac-query
              (list "select email from users where id = $1" bob-id)
              :single))))
    ;; login still works with the preserved hash
    (is-true (a:login *rbac* "bob" "password-1"))))

(test be-update-tokens-carve-out
  "be-update on the opaque built-in :tokens type still works as admin
(the REST store-token login path)."
  (let* ((tid (or (be-value-id :tokens :user "admin" "admin")
                  (be-insert-internal :tokens
                    '(:user "admin" :value "tok-initial") "admin")))
         (result (be-update :tokens tid
                    '(:user "admin" :value "tok-updated") "admin")))
    (is (equal result tid))
    (is (equal "tok-updated"
          (a:with-rbac (*rbac*)
            (a:rbac-query
              (list "select token_value from tokens where id = $1" tid)
              :single))))))
