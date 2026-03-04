(asdf:defsystem :data-ui
  :description "Data Apps in 1 Hour"
  :author "Donnie Cameron <macnod@gmail.com>"
  :licence "MIT License"
  :depends-on (:cl-ppcre
                :postmodern
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
                               (:file "model")))))

                               ;; (:file "utils")
                               ;; (:file "css")
                               ;; (:file "data-ui")))))
