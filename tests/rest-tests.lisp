(in-package :data-ui)

(def-suite rest-suite :description "ReST endpoint tests")

(in-suite rest-suite)

(defun http-get (url)
  (multiple-value-bind (data code meta)
    (dr:http-request url :method :get :accept "application/json")
    (list
      :data (json-to-plist (babel:octets-to-string data :encoding :utf-8))
      :code code
      :meta (loop for item in meta appending (list (car item) (cdr item))))))


(defun make-url (endpoint params)
  (if params
    (format nil "http://~a:~a~a?~{~(~a~)=~a~^&~}"
      *http-host* *http-port* endpoint params)
    (format nil "http://~a:~a~a"
      *http-host* *http-port* endpoint)))

(test rest-list
  (let ((response (http-get (make-url "/api/list" '(:type :permissions)))))
    (is (equal (getf response :code) 200))
    (is (equal (u:tree-get response :data :type) "permissions"))
    (is (equal
          (u:safe-sort (u:tree-col response :data :records :name))
          (u:safe-sort '("create" "read" "update" "delete"))))
    (is (equal
          (u:safe-sort (u:plist-keys (u:tree-get response :data :records 0)))
          (u:safe-sort '(:id :created-at :updated-at :name))))
    (is-false (u:tree-get response :data :allowed-values))
    (is (> (parse-integer (u:tree-get response :meta :content-length))
          500))))
