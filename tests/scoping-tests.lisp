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

