(asdf:defsystem :data-ui
  :description "Data Apps in 1 Hour"
  :author "Donnie Cameron <macnod@gmail.com>"
  :licence "MIT License"
  :depends-on (:cl-ppcre
                :postmodern
                :cl-fad
                :yason
                :fiveam
                :swank
                :dc-ds
                :dc-time
                :p-log
                :rbac
                :dc-eclectic)

                ;; :cl-fad
                ;; :hunchentoot
                ;; :swank
                ;; :spinneret
                ;; :jose
                ;; :lass

  :serial t
  :components ((:module "lisp"
                 :components ((:file "data-ui-package")
                               (:file "database")
                               (:file "data-ui")
                               (:file "workbench")
                               (:file "plist-json")
                               (:file "backend")))
                (:module "tests"
                  :components ((:file "data-ui-tests")))))

                               ;; (:file "model")
                               ;; (:file "data-ui")
                               ;; (:file "rest")

                               ;; (:file "utils")
                               ;; (:file "css")
                               ;; (:file "data-ui")))))
