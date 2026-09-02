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
                               (:file "eval-safely")
                               (:file "startup")))
                (:module "tests"
                  :components ((:file "helpers")
                                (:file "predicate-tests")
                                (:file "backend-tests")
                                (:file "rest-tests")
                                (:file "hook-registry-tests")
                                (:file "scoping-tests")
                                (:file "action-tests")
                                (:file "secrets-tests")
                                (:file "widget-tests")
                                (:file "m2m-tests")
                                (:file "bi-m2m-tests")
                                (:file "generator-tests")
                                (:file "nullable-fk-tests")
                                (:file "static-options-tests")
                                (:file "form-fields-tests")
                                (:file "compose-tests")
                                (:file "compose-sugar-tests")
                                (:file "sortable-tests")
                                (:file "searchable-tests")
                                (:file "rollup-tests")
                                (:file "measure-rollup-tests")
                                (:file "sort-measures-tests")
                                (:file "default-sort-tests")
                                (:file "filtered-rollup-tests")
                (:file "agg-distinct-tests")
                (:file "update-permission-tests")
                (:file "spawn-tests")
                (:file "new-roles-tests")))))
