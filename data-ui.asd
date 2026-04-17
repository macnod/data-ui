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
  :serial t
  :components ((:module "lisp"
                 :components ((:file "data-ui-package")
                               (:file "database")
                               (:file "data-ui")
                               (:file "predicates")
                               (:file "plist-json")
                               (:file "backend")
                               (:file "model")))
                (:module "tests"
                  :components ((:file "helpers")
                                (:file "predicates")))))
