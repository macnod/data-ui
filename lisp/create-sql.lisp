(require :rbac)
(require :dc-eclectic)
(require :dc-time)
(require :p-log)
(require :dc-ds)
(require :cl-ppcre)
(require :postmodern)

(defpackage :create-sql
  (:use :cl)
  (:local-nicknames
    (:a :rbac)
    (:u :dc-eclectic)
    (:dt :dc-time)
    (:pl :p-log)
    (:ds :dc-ds)
    (:re :ppcre)
    (:db :postmodern)))

(in-package :create-sql)

(defparameter *resource-types*
  '(:users (:base t)
     :resources (:base t)

     :directories
     (:fs-backed t
       :fields
       (:path
         (:type :text
           :ui (:input-type :line :label "Directory")
           :required t
           :unique t
           :validation
           (and
             (re:scan "^/[-a-zA-Z0-9_ @./]+/$|^/$" it)
             (not (re:scan "//|/[- @]| /| $|^[^a-zA-Z0-9_.]" it)))
           :validation-tests
           (:pass ("/" "/one/" "/one/two/" "/one/two/three")
             :fail ("//" "/one//two" "/ one/two/" "/one/two"
                     "/one /two/" "/one/ two/")))))

     :files
     (:fs-backed t
       :fields
       (:path
         (:type :text
           :ui (:input-type :file :label "File")
           :input-type :file
           :label "File Name"
           :required :t
           :unique :t
           :validation
           (and
             (re:scan "^/[-a-zA-Z0-9_. @/]+$" it)
             (not (re:scan "//|/$|/[- @]| /| $|^[^a-zA-Z0-9_.]" it)))
           :validation-tests
           (:pass ("/one.txt" "/one/1.txt" "/one/two/three.txt")
             (:fail ("//" "/one/" "one/" "/one/1.txt " "one.txt"
                      "one/one.txt" "/one/two/tree.txt "))))))

     :global-settings
     (:fields
       (:name (:type :text
                :ui (:input-type :label)
                :validation
                (and
                  (re:scan "^[a-zA-Z][-a-zA-Z0-9]*$" it)
                  (not (re:scan "-$" it)))
                :validation-tests
                (:pass ("a" "abc" "dark-mode" "abc-123")
                  :fail ("-abc" "0-abc" "abc-" "dark_mode")))
         :type (:type :text :input-type :hidden :default ":text")
         :value (:type :text :input-type :type-field)))

     :settings
     (:fields
       (:name (:type :text :required t
                :ui (:input-type :label)
                :validation
                (and
                  (re:scan "^[a-zA-Z][-a-zA-Z0-9]*$" it)
                  (not (re:scan "-$" it)))
                :validation-tests
                (:pass ("a" "abc" "dark-mode" "abc-123")
                  :fail ("-abc" "0-abc" "abc-" "dark_mode")))
         :value (:type :text :input-type :input-type-field :required t)
         :input-type (:default ":boolean")
         :reference (:target :users :lookup-field :name)))))

(defun to-sql-identifier (keyword &key (format-string "~a") (form :as-is))
  (let ((s (format nil "~(~a~)" keyword)))
    (format nil format-string
      (re:regex-replace-all
        "-"
        (case form
          (:as-is s)
          (:singular (u:singular s))
          (:plural (u:plural s))
          (otherwise (error "Unsupported value for FORM: ~a" form)))
        "_"))))

(defun filter-types (types predicate)
  (loop for type in (u:plist-keys types)
    for type-definition = (getf types type)
    for flag = (funcall predicate type-definition)
    when flag append (list type type-definition)))

(defun base-types (&optional (types *resource-types*))
  (filter-types types (lambda (v) (getf v :base))))

(defun non-base-types (types)
  (filter-types types (lambda (v) (not (getf v :base)))))

(defun keyword-to-table-name (keyword)
  (let ((format-string (if (member keyword (u:plist-keys (base-types)))
                            "~a"
                            "rt_~a")))
    (to-sql-identifier keyword :format-string format-string)))

(defun keyword-to-table-reference (keyword)
  (when keyword
    (to-sql-identifier keyword :format-string "~a_id" :form :singular)))

(defun keyword-to-table-reference-key (keyword)
  (when keyword
    (u:make-keyword
      (format nil "~a-id" (u:singular (format nil "~a" keyword))))))

(defun column-name (type-key field-key field-definition)
  (let ((table-name (to-sql-identifier type-key :form :singular)))
    (if (or
          (getf field-definition :target)
          (equal field-key :input-type))
      (to-sql-identifier field-key)
      (format nil "~a_~a" table-name (to-sql-identifier field-key)))))

(defun updated-at-trigger-sql (table)
  (format nil
    "
do $$
begin
    if not exists (
        select 1 from pg_trigger
        where tgname = 'set_~a_updated_at'
        and tgrelid = '~a'::regclass::oid
    ) then
        create trigger set_~a_updated_at
            before update on ~a
            for each row
            execute function set_updated_at_column();
    end if;
end $$;
"
    table table table table))

(defun resource-table-sql (table field-definitions)
  (format
    nil
    "~%create table if not exists ~a (~%    ~{~a~^,~%    ~}~%)~%"
    table field-definitions))

(defun field-name (type-key field-key types)
  (let* ((field (u:tree-get types type-key :fields field-key))
          (reference (getf field :references))
          (prefix (to-sql-identifier
                    (if reference
                      reference
                      type-key)
                    :form :singular))
          (name (if reference
                  "id"
                  (to-sql-identifier field-key))))
    (format nil "~a_~a" prefix name)))

(defun field-type (type-key field-key types)
  (let* ((reference (u:tree-get types type-key :fields field-key :references)))
    (if reference
      :uuid
      (or (u:tree-get types type-key :fields field-key :type) :text))))

(defun default-definitions ()
  (list
    "id uuid primary key references resources(id) on delete cascade"
    "created_at timestamp not null default now()"
    "updated_at timestamp not null default now()"))

(defun field-definitions (type-key types)
  (loop
    for field-key in (u:plist-keys (u:tree-get types type-key :fields))
    for field = (u:tree-get types type-key :fields field-key)
    for reference-key = (getf field :references)
    for reference = (keyword-to-table-reference reference-key)
    for reference-target = (when reference
                             (format nil "references ~a(id)"
                               (keyword-to-table-name reference-key)))
    for name = (field-name type-key field-key types)
    for field-type-key = (field-type type-key field-key types)
    for type = (to-sql-identifier field-type-key)
    for unique = (when (and (getf field :unique) (not reference))
                   "unique")
    for default-value = (unless reference (getf field :default))
    for default = (when default-value
                    (let ((fs (if (member field-type-key '(:text :uuid))
                                "default '~a'"
                                "default ~a")))
                      (format nil fs default-value)))
    for required = (when (or reference (getf field :required)) "not null")
    for cascade = (when reference "on delete cascade")
    for column = (format nil "~{~a~^ ~}"
                   (remove-if-not
                     #'identity
                     (list name type unique default required reference-target cascade)))
    collect column into columns
    finally (return (append (default-definitions) columns))))

(defun parse-type-fields (type-key types)
  (loop
    with field-keys = (cons :id
                        (u:plist-keys (u:tree-get types type-key :fields)))
    for key in field-keys
    for def = (if (equal key :id)
                (list
                  :target :resources
                  :type :uuid
                  :primary-key t
                  :required t
                  :cascade t)
                (u:tree-get types type-key :fields key))
    for new-key = (if (equal key :reference)
                    (keyword-to-table-reference-key
                      (getf def :target))
                    key)
    for new-def = (let* ((copy (u:deep-copy def)))
                    (when (equal key :reference)
                      (setf (getf copy :type) :uuid)
                      (setf (getf copy :required) t)
                      (setf (getf copy :cascade) t)
                      (unless (getf copy :lookup-field)
                        (setf (getf copy :lookup-field) :id)))
                    (loop for def-key in (u:plist-keys copy)
                      for def-value = (getf copy def-key)
                      do (setf (getf copy :column-name)
                           (column-name type-key new-key copy))
                      when (equal def-key :validation)
                      do (setf (getf copy :validation-compiled)
                           (compile nil `(lambda (it)
                                           ,(getf copy :validation)))))
                    copy)
    appending (list new-key new-def)))

(defun parse-type-def (type-key types)
  (list
    :base (u:tree-get types type-key :base)
    :fs-backed (u:tree-get types type-key :fs-backed)
    :fields (parse-type-fields type-key types)))

(defun parse-types (types)
  (loop
    for type-key in (u:plist-keys types)
    appending (list type-key (parse-type-def type-key types))))

(defun column-names (parsed-types type-key)
  (loop
    for field-key in (u:plist-keys (u:tree-get parsed-types type-key :fields))
    for field-def = (u:tree-get parsed-types type-key :fields field-key)
    for is-reference = (getf field-def :target)
    collect

(defun create-resource-tables-sql (types)
  (loop
    with new-types = (non-base-types types)
    for type-key in (u:plist-keys new-types)
    for table = (keyword-to-table-name type-key)
    for field-definitions = (field-definitions type-key types)
    appending (list
                (resource-table-sql table field-definitions)
                (updated-at-trigger-sql table))))

(defun create-insert-sql (type-key data-plist types)
  (loop
    for field-key in (u:plist-keys (u:tree-get types type-key :fields))
    for field-spec = (u:tree-get types type-key :fields field-key)
    for field-name = (field-name type-key field-key types)
    for required = (getf field-spec :required)
    for field-value = (if required
                        (getf data-plist field-key)
                        (or
                          (getf field-spec :default)
                          (getf data-plist field-key)))
    for placeholder = 1 then (1+ placeholder)
    collect field-name into field-names
    collect field-value into field-values
    collect (format nil "$~d" placeholder) into placeholders
    finally
    (return
      (cons
        (format nil "insert into ~a (~{~a~^, ~}) values(~{~a~^, ~})"
          (keyword-to-table-name type-key) field-names placeholders)
        field-values))))
