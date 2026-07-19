(in-package :data-ui)

;; Register test action hooks. These must exist before the model is
;; compiled (set-model), because compile-action-hooks resolves them
;; from the registry.

(register-hook :test-sync :action
  nil (lambda ()
        (lambda (type-key field-key record user
                 &key roles status-field set-status)
          (declare (ignore type-key field-key record user
                           roles status-field))
          (funcall set-status "sync step done")
          nil)))

(register-hook :test-async :action
  nil (lambda ()
        (lambda (type-key field-key record user
                 &key roles status-field set-status)
          (declare (ignore type-key field-key record user
                           roles status-field))
          (list :async t :message "Async started"))))

(register-hook :test-error :action
  nil (lambda ()
        (lambda (type-key field-key record user
                 &key roles status-field set-status)
          (declare (ignore type-key field-key record user
                           roles status-field set-status))
          (error "Deliberate test failure"))))

(def-suite action-suite :description "Action hook tests")

(in-suite action-suite)

(defun th-action-todo-id ()
  "Insert a test todo and return its ID. Cleans up any prior copy."
  (be-delete :todos
    `((:todos :name :eq "action-test-todo")) "admin")
  (be-insert :todos '(:name "action-test-todo" :points 1) "admin"))

(test be-set-field-value-basic
  "be-set-field-value writes a single column."
  (let ((id (th-action-todo-id)))
    (is-true id "Precondition: test todo exists")
    (unwind-protect
      (progn
        (be-set-field-value :todos id :test-action-status
          "custom-value" "admin")
        (let* ((record (getf (rec id "admin"
                              :type-key :todos) :record))
               (status (getf record :test-action-status)))
          (is (equal status "custom-value"))))
      (be-delete :todos id "admin"))))

(test be-action-sync-success
  "be-action with a sync hook transitions running → complete."
  (let ((id (th-action-todo-id)))
    (is-true id)
    (unwind-protect
      (let ((result (be-action :todos id :test-action "admin")))
        (is (equal (getf result :status) "complete"))
        (let* ((record (getf (rec id "admin"
                              :type-key :todos) :record))
               (status (getf record :test-action-status)))
          (is (equal status "complete"))))
      (be-delete :todos id "admin"))))

(test be-action-async-leaves-running
  "be-action with an async hook leaves status at running."
  (let ((id (th-action-todo-id)))
    (is-true id)
    (unwind-protect
      (let ((result (be-action :todos id :test-async-action
                     "admin")))
        (is (getf result :async) "Result should have :async t")
        (is (equal (getf result :message) "Async started"))
        (let* ((record (getf (rec id "admin"
                              :type-key :todos) :record))
               (status (getf record :test-async-action-status)))
          (is (equal status "running"))))
      (be-delete :todos id "admin"))))

(test be-action-sync-error-sets-failed
  "be-action with a failing sync hook sets failed status."
  (let ((id (th-action-todo-id)))
    (is-true id)
    (unwind-protect
      (let ((result (be-action :todos id :test-error-action
                     "admin")))
        (is (equal (getf result :status) "failed"))
        (let* ((record (getf (rec id "admin"
                              :type-key :todos) :record))
               (status (getf record :test-error-action-status)))
          (is (search "failed:" status)
              "Status should start with 'failed:'")))
      (be-delete :todos id "admin"))))

(test be-action-in-progress-guard
  "Second be-action while running is rejected."
  (let ((id (th-action-todo-id)))
    (is-true id)
    (unwind-protect
      (progn
        (be-set-field-value :todos id :test-action-status
          "running" "admin")
        (signals error
          (be-action :todos id :test-action "admin")))
      (be-delete :todos id "admin"))))

(test be-action-permission-denied
  "be-action rejects user without update permission."
  (let ((id (th-action-todo-id)))
    (is-true id)
    (unwind-protect
      (progn
        (th-make-user "action-noob" :roles nil)
        (signals error
          (be-action :todos id :test-action "action-noob")))
      (be-delete :todos id "admin"))))

(test fe-fields-excludes-button-from-list-add
  ":button fields are excluded from list-form and add-form."
  (let ((fe (fe-fields :todos "admin")))
    (let ((list-fields (getf fe :list-form))
          (add-fields (getf fe :add-form)))
      (is (not (getf list-fields :test-action))
          ":test-action must not appear in list-form")
      (is (not (getf add-fields :test-action))
          ":test-action must not appear in add-form"))
    (let ((update-fields (getf fe :update-form)))
      (is (getf update-fields :test-action)
          ":test-action must appear in update-form"))))

(test fe-fields-status-on-update-form
  "Status companion field appears on update-form."
  (let ((fe (fe-fields :todos "admin")))
    (let ((update-fields (getf fe :update-form)))
      (is (getf update-fields :test-action-status)
          ":test-action-status must appear in update-form"))))
