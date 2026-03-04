(in-package :data-ui)

(defparameter *timestamp-regex*
  "^\\d{4}-[01][0-9]-[0-3][0-9][T ][0-2][[0-9]:[0-5][0-9]:[0-5][0-9]$")

(defparameter *default-fields*
  '(:id (:target :resources
          :type :uuid
          :input-type :none
          :primary-key t
          :required t
          :cascade t)
     :created-at (:type :timestamp
                   :input-type :none
                   :required t
                   :default :now)
     :updated-at (:type :timestamp
                   :input-type :none
                   :required t
                   :default :now)))

(defparameter *file-name-validation*
  '(and
     (re:scan "^/[-a-zA-Z0-9_ @./]+/$|^/$" it)
     (not (re:scan "//|/[- @]| /| $|^[^a-zA-Z0-9_.]" it))))

(defparameter *file-name-validation-tests*
  '(:pass ("/" "/one/" "/one/two/" "/one/two/three")
     :fail ("//" "/one//two" "/ one/two/" "/one/two"
             "/one /two/" "/one/ two/"))

(defparameter *type-spec-example*
  '(:users (:base t)
     :resources (:base t)

     :directories
     (:fs-backed t
       :fields
       (:path
         (:type :directory-name
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
         (:type :file-name
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
         :value (:type :text :input-type :input-type-field :required t)
         :input-type nil))

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
         :input-type nil
         :reference (:target :users :lookup-field :name)))))

(defparameter *resource-types* nil)

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

(defun base-types (types)
  (filter-types types (lambda (v) (getf v :base))))

(defun non-base-types (types)
  (filter-types types (lambda (v) (not (getf v :base)))))

(defun table-name (keyword types)
  (let ((format-string (if (member keyword (u:plist-keys (base-types types)))
                            "~a"
                            "rt_~a")))
    (to-sql-identifier keyword :format-string format-string)))

(defun table-reference (keyword)
  (when keyword
    (u:make-keyword
      (format nil "~a-id" (u:singular (format nil "~a" keyword))))))

(defun column-name (type-key field-key field-definition)
  (let ((table-name (to-sql-identifier type-key :form :singular)))
    (if (or
          (getf field-definition :target)
          (member field-key '(:input-type :created-at :updated-at)))
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

(defun field-type (field-def)
  (if (getf field-def :target)
    :uuid
    (or (getf field-def :type) :text)))

(defun sql-type (field-type-key)
  (to-sql-identifier field-type-key))

(defun value-sql (value type-key &key quote)
  (case type-key
    (:boolean (case value
                (:true (if quote "'true'" "true"))
                (:false (if quote "'false'" "false"))
                (otherwise (error "Unsupported boolean value '~a'" value))))
    (:integer (if (integerp value)
                value
                (error "Invalid integer value '~a'" value)))
    (:float (if (numberp value)
              value
              (error "Invalid float value '~a'" value)))
    (:timestamp (cond
                  ((and
                     (stringp value)
                     (re:scan *timestamp-regex* value))
                    (if quote (format nil "'~a'" value) value))
                  ((equal value :now) "now()")
                  (t (error "Invalid timestamp string '~a'" value))))
    (:text (if (stringp value)
             (if quote (format nil "'~a'" value) value)
             (if (null value)
               (if quote "''" "")
               (if quote (format nil "'~a'" value) value))))
    (otherwise (error "Unknown TYPE-KEY '~a'" type-key))))

(defun field-definitions (type-key types)
  (loop
    for field-key in (u:plist-keys (u:tree-get types type-key :fields))
    for field-def in (u:plist-values (u:tree-get types type-key :fields))
    for target-key = (getf field-def :target)
    for reference = (when target-key
                      (format nil "references ~a(id)"
                        (table-name target-key types)))
    for name = (getf field-def :column-name)
    for field-type-key = (field-type field-def)
    for type = (sql-type field-type-key)
    for unique = (when (and (not reference) (getf field-def :unique))
                   "unique")
    for default-value = (when (and (not reference)
                                (getf field-def :default))
                          (value-sql
                            (getf field-def :default) field-type-key :quote t))
    for default = (when default-value (format nil "default ~a" default-value))
    for required = (when (and
                           (or reference (getf field-def :required))
                           (not (getf field-def :primary-key)))
                     "not null")
    for cascade = (when reference "on delete cascade")
    for primary = (when (getf field-def :primary-key) "primary key")
    for column = (format nil "~{~a~^ ~}"
                   (remove-if-not
                     #'identity
                     (list name type primary unique required default reference
                       cascade)))
    collect column))

(defun parse-type-fields (type-key types)
  (loop
    with fields = (append
                    *default-fields*
                    (u:tree-get types type-key :fields))
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for new-key = (if (equal field-key :reference)
                    (table-reference (getf field-def :target))
                    field-key)
    for new-def = (let* ((copy (u:deep-copy field-def)))
                    (case field-key
                      (:reference
                        (setf (getf copy :type) :uuid)
                        (setf (getf copy :required) t)
                        (setf (getf copy :cascade) t)
                        (unless (getf copy :lookup-field)
                          (setf (getf copy :lookup-field) :id)))
                      (:input-type
                        (setf (getf copy :type) :text)
                        (setf (getf copy :required) t)
                        (let ((default (getf copy :default)))
                          (if default
                            (setf (getf copy :default)
                              (format nil "~a" default))
                            (setf (getf copy :default) "text")))))
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
    :table-name (table-name type-key types)
    :fields (parse-type-fields type-key types)))

(defun parse-types (types)
  (loop
    for type-key in (u:plist-keys types)
    appending (list type-key (parse-type-def type-key types))))

(defun column-names (parsed-types type-key)
  (loop
    for field-def in (u:plist-values (u:tree-get parsed-types type-key :fields))
    collect (getf field-def :column-name)))

(defun set-resource-types (types-spec)
  ":public: Parses TYPES-SPEC to produce a representation of the application
model. Returns a plist with the model's types and associated SQL table names."
  (setf *resource-types* (parse-types types-spec))
  (loop for type-key in (u:plist-keys *resource-types*)
    appending
    (list type-key (u:tree-get *resource-types* type-key :table-name))))

(defun get-resource-types ()
  ":public: Returns the current application model. The return value is NIL
if SET-RESOURCE-TYPE has not yet been called."
  *resource-types*)

(defun create-resource-tables-sql (&optional
                                    (parsed-types *resource-types*))
  ":public: Returns the SQL for the application model. The SQL consists of
table creation statements for the model's non-base resource types. Tables
already exist for the base resource types, so this function does not create
them."
  (loop
    with new-types = (non-base-types parsed-types)
    for type-key in (u:plist-keys new-types)
    for table = (table-name type-key new-types)
    for field-definitions = (field-definitions type-key new-types)
    appending (list
                (resource-table-sql table field-definitions)
                (updated-at-trigger-sql table))))
