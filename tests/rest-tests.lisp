(in-package :data-ui)

(setf dr:*text-content-types* '((nil . "json")))

(defun serialize-filters (filters)
  (h:url-encode
    (loop for filter in filters
      collect (format nil "[~{\"~a\"~^,~}]" filter) into lines
      finally (return (format nil "[~{~a~^,~}]" lines)))))

(defun http-get (url)
  (let ((dr:*text-content-types* '((nil . "json"))))
    (multiple-value-bind (data code meta)
      (dr:http-request url
        :method :get
        :accept "application/json"
        :preserve-uri t
        :decode-content t)
      (list
        :data (json-to-plist data)
        :code code
        :meta (loop for item in meta appending (list (car item) (cdr item)))))))


(defun make-url (endpoint &rest params)
  (if params
    (loop for key in params by #'cddr
      for val-raw in (cdr params) by #'cddr
      for val = (case key
                  (:filters (serialize-filters val-raw))
                  (otherwise val-raw))
      appending (list key val) into processed-params
      finally
      (return
        (format nil "http://~a:~a~a?~{~(~a~)=~a~^&~}"
          *http-host* *http-port* endpoint processed-params)))
    (format nil "http://~a:~a~a"
      *http-host* *http-port* endpoint)))

(def-suite rest-suite :description "ReST endpoint tests")

(in-suite rest-suite)

;; (test rest-list
;;   (let ((response (http-get (make-url "/api/list" :type :permissions))))
;;     (is (equal (getf response :code) 200))
;;     (is (equal (u:tree-get response :data :status) "success"))
;;     (is (equal (u:tree-get response :data :result :type-key) "permissions"))
;;     (is (equal
;;           (u:safe-sort (u:tree-col response :data :result :records :name))
;;           (u:safe-sort '("create" "read" "update" "delete"))))
;;     (is (equal
;;           (u:safe-sort (u:plist-keys (u:tree-get response :data :result :records 0)))
;;           (u:safe-sort '(:id :created-at :updated-at :name))))
;;     (is-false (u:tree-get response :data :result :allowed-values))
;;     (is (> (parse-integer (u:tree-get response :meta :content-length))
;;           500))))

;; (test rest-id
;;   (let ((response (http-get (make-url "/api/id"
;;                               :type :users
;;                               :filters '((:users :name :eq "admin"))))))
;;     (is (equal (getf response :code) 200))
;;     (is (equal (u:tree-get response :data :status) "success"))
;;     (is-true (uuid-p (u:tree-get response :data :result)))))
