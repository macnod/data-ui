(asdf:defsystem :data-ui
  :description "Data Apps in 1 Hour"
  :author "Donnie Cameron <macnod@gmail.com>"
  :licence "MIT License"
  :depends-on (:cl-ppcre
                :postmodern
                :cl-fad
                :drakma
                :yason
                :fiveam
                :swank
                :hunchentoot
                :jose
                :babel
                :dc-ds
                :dc-time
                :p-log
                :rbac
                :dc-eclectic)
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
                               (:file "rest")))
                (:module "tests"
                  :components ((:file "models")
                                (:file "helpers")
                                (:file "predicate-tests")
                                (:file "backend-tests")
                                (:file "rest-tests")))))
