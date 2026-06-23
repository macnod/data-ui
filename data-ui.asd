(asdf:defsystem :data-ui
  :description "Data Apps in 1 Hour"
  :author "Donnie Cameron <macnod@gmail.com>"
  :licence "MIT License"
  :depends-on (:babel
                :cl-ppcre
                :dc-ds
                :dc-eclectic
                :dc-time
                :drakma
                :fiveam
                :hunchentoot
                :jose
                :p-log
                :postmodern
                :rbac
                :swank
                :yason)
  :serial t
  :components ((:module "lisp"
                 :components ((:file "data-ui-package")
                               (:file "database")
                               (:file "data-ui")
                               (:file "aux")
                               (:file "predicates")
                               (:file "plist-json")
                               (:file "model")
                               (:file "backend")
                               (:file "rest")
                               (:file "startup")))
                (:module "tests"
                  :components ((:file "helpers")
                                (:file "predicate-tests")
                                (:file "backend-tests")
                                (:file "rest-tests")
                                (:file "scoping-tests")))))
