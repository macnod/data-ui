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
    *resource-types*
    add-resource
    base-types
    create-resource-tables
    create-resource-tables-sql
    descriptor-get
    enabled-types
    filter-types
    immutable-user-roles
    init-database
    list-directory
    list-directory-recursively
    make-resource-descriptor
    name-field
    non-base-types
    table-name
    type-keywords
    type-strings
    validate-fs-storage-key
    validate-resource-descriptor
    validate-resource-descriptor-key
    validate-resource-types
    validate-type-keyword
    validate-type-string))
