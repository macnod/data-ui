(defpackage :data-ui
  (:use :cl)
  (:local-nicknames
    (:a :rbac)
    (:u :dc-eclectic)
    (:pl :p-log)
    (:dt :dc-time)
    (:ds :dc-ds)
    (:re :ppcre)
    (:h :hunchentoot)
    (:s :spinneret)
    (:b :babel)
    (:l :lass)
    (:db :postmodern)
    (:j :jose))
  (:export
    *document-root*
    *rbac*
    add-to-class
    add-to-url-query
    additional-text
    alist-to-hash-table
    create-resource-tables
    create-resource-tables-sql
    exclude
    exclude-regex
    exclusive-role-for
    form-text
    form-title
    has
    has-some
    html-list
    init-database
    input-checkbox
    input-checkbox-list
    input-checkbox-pre
    input-file
    input-form
    input-hidden
    input-password
    input-submit-button
    input-text
    invert-hex-color
    issue-jwt
    join-html
    label-to-name
    make-resource-descriptor
    name-to-id
    readable-time-stamp
    render-pager
    upload-form
    validate-jwt
    ))
