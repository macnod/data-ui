(in-package :data-ui)

(def-suite directory-update-suite
  :description "valid-updated-directory gate: be-update rejects any change
to a :path field's value on fs-backed types (moves are not supported, so an
accepted rename desynchronizes row / resource / filesystem). Unchanged
values must pass — roles-only and ordinary edits ride the same path.")

(in-suite directory-update-suite)

(defun du-message (form)
  "Run FORM, expect a validation-error, return its rendered text."
  (handler-case
      (progn
        (funcall form)
        (fail "Expected a validation-error"))
    (validation-error (e) (princ-to-string e))))

(defun du-cleanup-gate-model ()
  "Drop the gate-model row and any leftover image rows referencing it
(file deletion normally happened in the test body; this sweep keeps a
re-run safe against the :models :name unique index and FK). Join-key
filters are not supported on be-list — page through :images and match
:model in Lisp."
  (loop for image-row in (getf (be-list :images "admin" :limit 200)
                          :records)
    for image-id = (getf image-row :id)
    for image-model = (getf image-row :model)
    when (and image-id (equal image-model "gate-model"))
    do (be-delete :images `((:images :id :eq ,image-id)) "admin"))
  (let ((model-id (be-id :models '((:models :name :eq "gate-model"))
                     "admin")))
    (when model-id
      (be-delete :models `((:models :id :eq ,model-id)) "admin"))))

(defun du-make-gate-image ()
  "Create the gate-model row then a leaf :images record under it
(th-make-mb-image resolves :model as a :target at write time, so the
model must exist first — the th-make-model / th-make-mb-image pair the
scoping tests use). Returns the image ID."
  (th-make-model "gate-model" "admin" :roles '("models-user"))
  (th-make-mb-image "/gate-image.txt" "admin" "gate-model"))

(defun du-insert-directory (logical-path)
  "Insert a directory record (root \"/\" exists on the modelbank-test
fixture); caller deletes it for cleanup."
  (multiple-value-bind (id inserted)
      (be-insert :directories `(:name ,logical-path) "admin"
        :roles '("directories-user"))
    (is-true id)
    (is-true inserted)
    id))

(test update-no-rename-passes
  "Control: be-update with the path value unchanged returns a uuid
(roles-only edits merge :name unchanged through this same path)."
  (let ((id (du-insert-directory "/ok-dir/")))
    (is-true (uuid-p (be-update :directories
                      `((:directories :name :eq "/ok-dir/"))
                      '(:name "/ok-dir/") "admin")))
    (be-delete :directories
      `((:directories :name :eq "/ok-dir/")) "admin")))

(test rename-malformed-rejected
  "The bug: renaming /ok-dir/ to a slash-less name must signal. The
message names both values and says moves are unsupported."
  (du-insert-directory "/ok-dir/")
  (let ((text (du-message
                (lambda ()
                  (be-update :directories
                    '((:directories :name :eq "/ok-dir/"))
                    '(:name "secret") "admin")))))
    (is-true (search "moves are not supported" text))
    (is-true (search "secret" text))
    (is-true (search "/ok-dir/" text)))
  (be-delete :directories
    '((:directories :name :eq "/ok-dir/")) "admin"))

(test rename-wellformed-rejected
  "A well-formed rename is equally unsupported — the gate is about the
change, not the shape (shape stays an insert-time concern)."
  (du-insert-directory "/ok-dir/")
  (let ((text (du-message
                (lambda ()
                  (be-update :directories
                    '((:directories :name :eq "/ok-dir/"))
                    '(:name "/renamed/") "admin")))))
    (is-true (search "moves are not supported" text))
    (is-true (search "/renamed/" text)))
  (be-delete :directories
    '((:directories :name :eq "/ok-dir/")) "admin"))

(test root-rename-rejected
  "Belt: the root directory row is protected by the same rule — no
special case, no special-case message."
  (let ((text (du-message
                (lambda ()
                  (be-update :directories
                    '((:directories :name :eq "/"))
                    '(:name "/root/") "admin")))))
    (is-true (search "moves are not supported" text))
    (is-true (search "/root/ -> /" text))))

(test file-path-change-rejected
  "Leaf types gate identically: renaming an image's :name would
desynchronize row / resource / filesystem the same way."
  (let ((id (du-make-gate-image)))
    (let ((text (du-message
                  (lambda ()
                    (be-update :images `((:images :id :eq ,id))
                      '(:name "/renamed.txt") "admin")))))
      (is-true (search "moves are not supported" text))
      (is-true (search "/renamed.txt" text))
      (is-true (search "/gate-image.txt" text)))
    (be-delete :images `((:images :id :eq ,id)) "admin")
    (du-cleanup-gate-model)))

(test file-update-no-change-passes
  "Leaf control: updating a file record with its :name unchanged
succeeds (metadata edits ride this path)."
  (let ((id (du-make-gate-image)))
    (is-true (uuid-p (be-update :images `((:images :id :eq ,id))
                      '(:name "/gate-image.txt") "admin")))
    (be-delete :images `((:images :id :eq ,id)) "admin")
    (du-cleanup-gate-model)))
