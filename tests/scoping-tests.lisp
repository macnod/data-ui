(in-package :data-ui)

(def-suite scoping-suite :description "Scoping tests")

(in-suite scoping-suite)

(test view-scope
  (is (equal
        ':user
        (u:tree-get *compiled-model* :images :views :main :scope))))

;; TODO: Fix. Not working, likely because of write-thru issues.
(test images-scoped-by-user
  (let ((user-1 "user-1")
         (user-2 "user-2")
         (model-1 "model-1")
         (model-2 "model-2")
         (roles '("images-user" "models-user" "role-1" "directories-user")))
    ;; Create 2 users
    (th-make-user user-1 :roles roles)
    (th-make-user user-2 :roles roles)
    ;; Create 2 models
    (th-make-model model-1 user-1 :roles '("role-1" "models-user"))
    (th-make-model model-2 user-2 :roles '("role-1" "models-user"))
    (loop for a from 1 to 2
      for user = user-1 then user-2
      for model = model-1 then model-2
      do
      ;; Each user adds 3 images
      (loop for b from 1 to 3
        for image-name = (format nil "image-~d-~d.png" a b)
        for image-path = (format nil "/~a" image-name)
        for source-file = (format nil "tests/images/~a" image-name)
        do (th-make-mb-image image-path user model
             :roles '("role-1") :source-file source-file)))
    ;; user-1 can see the images user-1 added, and no others, because the main
    ;; image view is scoped to the user.
    (let ((image-names (mapcar
                         (lambda (x) (getf x :name))
                         (getf (be-list :images user-1) :records))))
      (is (= 3 (length image-names)))
      (is-true (every
                 (lambda (x) (u:starts-with x "/image-1-"))
                 image-names)))
    ;; user-2 can see the images user-2 added, and no others
    (let ((image-names (mapcar
                         (lambda (x) (getf x :name))
                         (getf (be-list :images user-2) :records))))
      (is (= 3 (length image-names)))
      (is-true (every
                 (lambda (x) (u:starts-with x "/image-2-"))
                 image-names)))))

(defun count-ratings-for-model (model-name)
  "Count ratings rows for MODEL-NAME via direct SQL."
  (let ((result (query
                  "select count(*) from rt_ratings
                   where rating_model = (
                     select id from rt_models where model_name = $1)"
                  :params (list model-name)
                  :result-type :column)))
    (if (stringp (car result))
      (parse-integer (car result))
      (car result))))

(defun get-rating-value (model-name user-name)
  "Get the rating value for MODEL-NAME by USER-NAME via direct SQL."
  (let ((result (query
                  "select rating_rating from rt_ratings
                   where rating_model = (
                     select id from rt_models where model_name = $1)
                   and rating_user = (
                     select id from users where user_name = $2)"
                  :params (list model-name user-name)
                  :result-type :column)))
    (car result)))

;; TODO: Clear the rating (set to NULL) — blocked on full-data preserving
;; old values over submitted nil. The =(or data-value record-value ...)=
;; in full-data discards a submitted nil when the record already has a
;; value.
(test write-through-rating
  (let ((user-1 "rater-1")
         (user-2 "rater-2")
         (model-1 "wt-model-1")
         (roles '("models-user" "ratings-user" "role-1")))
    ;; Create users with roles needed for models + ratings
    (th-make-user user-1 :roles roles)
    (th-make-user user-2 :roles roles)
    ;; Create a model as user-1
    (let ((model-id (th-make-model model-1 user-1
                      :roles '("role-1" "models-user"))))
      (is-true (uuid-p model-id))
      ;; 1. user-1 rates the model via write-through
      (be-update :models model-id `(:rating 5) user-1)
      ;; Verify exactly one ratings row exists for this model
      (is (= 1 (count-ratings-for-model model-1)))
      ;; Verify the rating value is 5
      (is (= 5 (get-rating-value model-1 user-1)))
      ;; 2. user-1 updates the rating — should update, not duplicate
      (be-update :models model-id `(:rating 3) user-1)
      (is (= 1 (count-ratings-for-model model-1)))
      (is (= 3 (get-rating-value model-1 user-1)))
      ;; 3. user-2 rates the same model — separate row
      (be-update :models model-id `(:rating 4) user-2)
      (is (= 2 (count-ratings-for-model model-1)))
      (is (= 4 (get-rating-value model-1 user-2)))
      ;; user-1's rating should be unchanged
      (is (= 3 (get-rating-value model-1 user-1))))))

(test field-level-scope-shows-current-user-rating
  "Two users rate the same model. Each user's be-list for :models
should show their own rating in the :rating field (scoped via
:scope :user on the field source), while :average-rating shows
the unscoped average across all users."
  (let ((user-1 "fls-1")
         (user-2 "fls-2")
         (user-3 "fls-3")
         (model-1 "fls-model-1")
         (roles '("models-user" "ratings-user" "role-1")))
    ;; Create users
    (th-make-user user-1 :roles roles)
    (th-make-user user-2 :roles roles)
    (th-make-user user-3 :roles roles)
    ;; Create a model
    (let ((model-id (th-make-model model-1 user-1
                      :roles '("role-1" "models-user"))))
      (is-true (uuid-p model-id))
      ;; user-1 rates it 5, user-2 rates it 3
      (be-update :models model-id `(:rating 5) user-1)
      (be-update :models model-id `(:rating 3) user-2)
      ;; user-3 does not rate it
      ;; Verify: user-1 sees their own rating (5)
      (let ((rec (find model-1
                   (getf (be-list :models user-1) :records)
                   :key (lambda (r) (getf r :name))
                   :test #'equal)))
        (is-true rec)
        (is (= 5 (getf rec :rating)))
        (is (= 4.0 (getf rec :average-rating))))
      ;; Verify: user-2 sees their own rating (3)
      (let ((rec (find model-1
                   (getf (be-list :models user-2) :records)
                   :key (lambda (r) (getf r :name))
                   :test #'equal)))
        (is-true rec)
        (is (= 3 (getf rec :rating)))
        (is (= 4.0 (getf rec :average-rating))))
      ;; Verify: user-3 sees NIL (no rating) but avg is still 4.0
      (let ((rec (find model-1
                   (getf (be-list :models user-3) :records)
                   :key (lambda (r) (getf r :name))
                   :test #'equal)))
        (is-true rec)
        (is (null (getf rec :rating)))
        (is (= 4.0 (getf rec :average-rating)))))))

;;
;; Write-path field validation tests — modelbank
;;

(test write-path-in-range-fails-on-insert
  "Insert with out-of-range rating should fail with validation-error."
  (signals validation-error
    (be-insert :models
      '(:name "bad-rating-model"
         :description "test"
         :model "code"
         :rating 10)
      "admin"))
  ;; Verify nothing was inserted
  (is-false (be-id :models '((:models :name :eq "bad-rating-model"))
                   "admin")))

(test write-path-in-range-fails-on-update
  "Update with out-of-range rating should fail without writing."
  (let ((id (be-insert :models
              '(:name "rating-update-test"
                 :description "test"
                 :model "code")
              "admin")))
    (is-true (uuid-p id))
    ;; Attempt update with out-of-range rating
    (signals validation-error
      (be-update :models id '(:rating 99) "admin"))
    ;; Clean up
    (is-true (uuid-p (be-delete :models id "admin")))))
