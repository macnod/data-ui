(asdf:defsystem :data-ui
  :description "Data Apps in 1 Hour"
  :author "Donnie Cameron <macnod@gmail.com>"
  :licence "MIT License"
  :depends-on (:cl-ppcre
                :hunchentoot
                :swank
                :spinneret
                :jose
                :lass
                :postmodern
                :cl-fad
                :dc-ds
                :dc-time
                :p-log
                :rbac
                :dc-eclectic)
  :serial t
  :components ((:module "lisp"
                 :components ((:file "data-ui-package")
                               ;; (:file "utils")
                               ;; (:file "css")
                               (:file "data-ui")))))
