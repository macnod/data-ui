(in-package :data-ui)

;;
;; BEGIN Internal helper functions
;;

(defun placeholders (fields &key (start-at 1))
  (loop for a from start-at to (1- (+ start-at (length fields)))
    collect (format nil "$~d" a)))

(defun aggregate-values (aggregation values)
  ":private: Performs AGGREGATION on the VALUES list."
  (case aggregation
    (:first (car values))
    (:list values)
    (:distinct (u:distinct-values values))
    (t (error "Invalid aggregation ~s." aggregation))))

(defun field-values-for-id (view-result id-key id-value alias-key aggregation)
  ":private: Retrieves from VIEW-RESULT the field values for the record where
ID-KEY contains ID-VALUE, with those field values aggregated according to
AGGREGATION.

VIEW-RESULT is the result of running one of the :VIEWS queries associated with a
type key in *COMPILED-MODEL*. That :VIEWS structure also contains metadata such
as the alias keys that VIEW-RESULT uses for the fields. The SQL in the :VIEWS
structure can often return multiple rows for a given record, due to joins with
other tables. Thus, we need to aggregate the values for a given field.

ID-KEY is the alias key (from the :VIEWS metadata) that VIEW-RESULT uses for the
ID field. ID-VALUE is the value of the ID field for the record whose field
values we want to retrieve.

ALIAS-KEY is the alias key (also from the :VEWS metadata) for the field that
contains the values we want to retrieve. AGGREGATION is a keyword that specifies
how to aggregate the values for the field."
  (loop for row in view-result
    for value = (getf row alias-key)
    ;; when (and
    ;;        (equal (getf row id-key) id-value)
    ;;        value
    ;;        (not (equal value :null)))
    when (equal (getf row id-key) id-value)
    collect value into values
    finally (return (aggregate-values aggregation values))))

(defun view-result-values (type-key field-keys view-result)
  ":private: Given a VIEW-RESULT, which is the result of running one of the
:VIEWS queries associated with TYPE-KEY, and which can contain multiple rows for
a given record (due to joins with other tables), this function collapses the
rows into a list of plists, with one plist per record. That means that only one
plist will be returned for a given ID, even if there are multiple rows in
VIEW-RESULT with that ID. The field values in the returned plists consist of the
same values that are in VIEW-RESULT, except that they are aggregated according
to the :SOURCE :AGG metadata for the fields in TYPE-KEY. The end result is a a
list of plists that corresponds in length to the number of distinct IDs in
VIEW-RESULT, with the fields containing aggregated values for each record. This
aggregation can, for example, consist of simply associating a list of values
with a field key in the returned plists. Thus, for example, if you have a
:USERS record with 3 associated roles, this function will return a single
plist for that record, and the :ROLES field in that plist will have a list of
the 3 roles."
  (loop
    with alias-keys = (alias-keys type-key field-keys)
    with id-key = (car alias-keys)
    with distinct-ids = (view-result-ids type-key field-keys view-result)
    with aggregations = (aggregations type-key field-keys)
    for id in distinct-ids
    collect
    (append (list :id id)
      (loop for key in (cdr field-keys)
        for alias in (cdr alias-keys)
        for aggregation in (cdr aggregations)
        appending
        (list
          key
          (field-values-for-id view-result id-key id alias aggregation))))))

(defun view-result-ids (type-key field-keys view-result)
  ":private: "
  (let* ((alias-keys (alias-keys type-key field-keys))
          (id-key (car alias-keys)))
    (u:distinct-values
      (mapcar
        (lambda (r) (getf r id-key))
        view-result))))

(defun next-param-index (sql)
  ":private: Finds the largest placeholder in SQL and returns an integer that
exceeds the placeholder's integer portion by 1."
  (loop with matches = (re:all-matches-as-strings "\\$\\d+" sql)
    for match in matches
    for index = (parse-integer (subseq match 1))
    for max = index then (if (> index max) index max)
    finally (return (1+ (or max 0)))))

(defun add-where-clause (sql filters)
  ":private: Given SQL and FILTERS, this function extends the SQL with a where
clause that includes placeholders and returns a list of the SQL plus the values
for the placeholders."
  (if filters
    (loop
      for index = (next-param-index sql)
      then (1+ (length (u:flatten values)))
      for (table-key field-key op-key value) in filters
      for alias = (to-sql-identifier
                    (u:tree-get *compiled-model*
                      table-key :fields field-key
                      :source :column-name))
      for op = (operator-sql op-key)
      for placeholders = (if (member op-key '(:in :not-in))
                           (format nil "(~{~a~^, ~})"
                             (placeholders value :start-at index))
                           (format nil "$~d" index))
      collect value into values
      collect (format nil "~a ~a ~a" alias op placeholders)
      into conditions
      finally
      (return
        (cons
          (format nil "~a~%where~%  ~{~a~^~%  and ~}~%" sql conditions)
          (u:flatten values))))
    (list sql)))

(defun add-ids-clause (type-key sql ids)
  ":private: Returns SQL for fetching the records that match the IDs in
VIEW-RESULT."
  (let ((id-column (u:tree-get *compiled-model* type-key :views :main :columns
                     type-key :id)))
    (cons
      (format nil "~a~%where ~a in (~%  ~{~a~^,~%  ~}~%)"
        sql id-column (placeholders ids))
      ids)))


(defun alias-keys (type-key field-keys)
  ":private: Returns a list of the view-result aliases for FIELD-KEYS in
TYPE-KEY."
  (loop
    with m = *compiled-model*
    for field-key in field-keys
    for alias-key = (u:tree-get m type-key :fields field-key :source :alias-key)
    when alias-key collect alias-key))

(defun aggregations (type-key field-keys)
  ":private: Returns a list of the aggregation keys associated with the fields
given by FIELD-KEYS in the TYPE-KEY model."
  (loop
    with m = *compiled-model*
    for field-key in field-keys
    collect (u:tree-get m type-key :fields field-key :source :agg)))

(defun form-field-keys (type-key form)
  ":private: Returns a list of the field keys in the TYPE-KEY model's FORM node.
If there is no such FORM node or if the value associated with FORM is not a list
but rather T, then this function returns a list of all the field keys associated
with TYPE-KEY. The result always starts with :ID, even when FORM doesn't include
the :ID field key."
  (let ((field-keys (u:tree-get *compiled-model* type-key form :fields)))
    (if (or (not field-keys) (equal field-keys t))
      (u:plist-keys (u:tree-get *compiled-model* type-key :fields))
      (cons :id field-keys))))

(defun resource-id-keys (type-key)
  (list :id
    (u:make-keyword
      (format nil "~a-id"
        (u:singular (format nil "~(~a~)" type-key))))))

(defun local-fields (type-key &key exclude)
  ":private: Returns a list of TYPE-KEY field keys, excluding default fields such
as :id, :created-at, and :updated-at, and excluding any fields that aren't an
actual column in the associated table, such as fields that have a non-nil
:JOIN-TABLE value."
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    unless (or
             (getf field-def :join-table)
             (getf field-def :base-field)
             (member field-key exclude))
    collect field-key))

(defun local-values-for-update (type-key data record &key id)
  ":private: Finds the keys listed in the model for TYPE-KEY :SQL-UPDATE, looks
up the values for each key, and returns the values as a string. Each value is
chosen from DATA. If the value is not in DATA, then the value is taken from
RECORD. If the value is not in RECORD, and the field key is :ID, then the value
given in ID is used."
  ;; Validations
  (valid-type-key type-key)
  ;; Collect data
  (loop with type-def = (getf *compiled-model* type-key)
    with main-update = (u:tree-get type-def :update-sql :main)
    for field-key in (cdr main-update)
    for field-def = (u:tree-get type-def :fields field-key)
    for data-value = (getf data field-key)
    for record-value = (getf record field-key)
    for default-value = (getf field-def :default)
    for id-value = (when (equal field-key :id) id)
    for field-value = (or data-value record-value default-value id-value)
    collect field-value))

(defun user-fields (type-key)
  ":private: Returns a list of the field keys that should be validated for
TYPE-KEY. This includes all the local fields (see LOCAL-FIELDS) plus join-table
fields."
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    unless (getf field-def :base-field)
    collect field-key))

(defun make-resource-name (type-key data)
  ":private: Returns a resource name for DATA, a row of TYPE-KEY. This name is
composed of the type-key, the value associated with the first key in DATA, and a
short (8 chars) hash of the concatenated, colon-separated values in DATA. We
want resource names to be unique, and some types, such as user-settings might
have the same :NAME and require the additional :USER value to to be unique.
**WARNING**: Do not use this function to retrieve the resource name for an existing
record! Use FIND-RESOURCE-NAME instead."
  (loop
    for field in (local-fields type-key)
    for name-part = (format nil "~(~a~)" (getf data field))
    collect name-part into name-parts
    finally (return (format nil "~(~a~):~a:~a"
                      type-key
                      (car name-parts)
                      (u:hash-string
                        (format nil "~{~(~a~^:~)~}" name-parts)
                        :size 8)))))

(defun find-resource-name (type-key filters)
  (if (uuid-p filters)
    (id-to-resource-name filters)
    (let* ((id (be-id type-key filters "admin"))
            (item (when id (be-rec id "admin" :type-key :resources))))
      (when item (getf item :name)))))

(defun insert-main-query (type-key insert uuid data)
  ":private: Returns a list consisting of and SQL string with placeholders
and parameters for those placeholders. INSERT is the value associated with
the :INSERT-SQL key of a type in *compiled-resources*. DATA is a plist
where the keys represent FIELDS and the values represent the values to be
inserted."
  (valid-insert insert)
  (valid-uuid uuid)
  (valid-data type-key data)
  (let* ((main-qt (getf insert :main))
          (main-sql (car main-qt)))
    (cons main-sql
      (cons uuid
        (loop for k in (cdr main-qt)
          for v = (or (getf data k)
                    (u:tree-get *compiled-model* type-key :fields k :default))
          unless (equal k :id)
          collect v)))))

(defun id-from-filters (filters)
  ":private: Returns an ID if FILTERS includes a filter with a field key of :ID
and an operator key of :EQ. Otherwise, returns NIL."
  (if (stringp filters)
    (progn
      (valid-existing-uuid filters)
      filters)
    (loop for (table-key field-key op-key value) in filters
      when (and (equal field-key :id) (equal op-key :eq))
      do (return value))))

(defun id-from-filters-and-data (type-key filters &optional data)
  ":private: Returns the ID of the record of type TYPE-KEY. If FILETRS is a
string, then this function returns FILTERS as the ID. Otherwise, if DATA
contains an :ID key, then this function returns the value associated with
that key. Otherwise, this function looks for a filter in FILTERS that has
a field key of :ID and an operator key of :EQ. If such a filter exists,
then this function returns the value associated with that filter. Otherwise,
this functon looks up the ID in the database using FILTERS and returns the
looked-up result. If the function fails to find an ID using any of the
above methods, it returns NIL."
  (let* ((data-id (and data (getf data :id)))
          (filters-id (unless data-id
                        (if (stringp filters)
                          filters
                          (id-from-filters filters))))
          (lookup-id (when (and filters (not (or data-id filters-id)))
                       (be-id type-key filters "admin"))))
      (or data-id filters-id lookup-id)))

(defun base-resource-type-key-p (type-key)
  ":private: Returns T if TYPE-KEY satisfies the following conditions:
    1. Base type in the model
    2. Not marked as a joiner
    3. Not the :resources type"
  (let* ((type-def (u:tree-get *compiled-model* type-key))
          (is-joiner (getf type-def :is-joiner))
          (base (getf type-def :base)))
    (and base (not is-joiner) (not (equal type-key :resources)))))

(defun base-resource-type-keys ()
  ":private: Returns a list of type keys for which are base types in the model,
excluding types that are marked as joiners and the :resources type."
  (loop for type-key in *compiled-model* by #'cddr
    when (base-resource-type-key-p type-key)
    collect type-key))

(defun full-data (type-key data &key record local-only)
  ":private: Returns a plist like DATA, but with any missing fields filled in
with their defaults from the model. This is useful for ensuring that we have
all the right fields when we perform inserts or updates."
  (loop with fields = (append
                        (if local-only
                          (local-fields type-key)
                          (user-fields type-key))
                        (when record (list :id)))
    for field-key in fields
    for field-def = (u:tree-get *compiled-model* type-key :fields field-key)
    for default-value = (getf field-def :default)
    for data-value = (getf data field-key)
    for record-value = (getf record field-key)
    appending (list field-key (or data-value record-value default-value))))

(defun uuid-exists-p (uuid)
  (valid-uuid uuid)
  (when
    (loop for type in '("resources" "users" "permissions" "roles")
      for sql = (format nil "select 1 from ~a where id = $1" type)
      for query = (list sql uuid)
      thereis (a:with-rbac (*rbac*) (a:rbac-query query :single)))
    t))

;;
;; END Internal helper functions
;;

;;
;; BEGIN Database helper functions
;;

(defun update-roles (type-key filters roles)
  ":private: Updates the RBAC roles for the resource of type TYPE-KEY with ID.
ROLES is a list of role names. ID is a UUID string. This function returns the
resource name of the updated record if the update is successful, or an error if
the update fails."
  (valid-type-key type-key)
  (valid-roles roles)
  (if (uuid-p filters)
    (valid-existing-uuid filters)
    (valid-filters filters :required t))
  ;; Compute the existing roles for the resource
  (let* ((roles (if (member "admin" roles) roles (cons "admin" roles)))
          (resource-name (find-resource-name type-key filters))
          (existing-roles (a:list-resource-role-names *rbac* resource-name))
          (to-add (set-difference roles existing-roles :test 'equal))
          (to-remove (set-difference existing-roles roles :test 'equal)))
    ;; Add new roles
    (loop for role in to-add
      do (a:add-resource-role *rbac* resource-name role))
    ;; Remove old roles
    (loop for role in to-remove
      do (a:remove-resource-role *rbac* resource-name role))
    resource-name))

(defun update-resource-name (type-key data)
  ":internal: Updates the resource name for the given record. DATA must include
the ID of the record. This does nothing for base tables, because base tables
aren't resources, and the resources table is an internal table that represents
user resources."
  (valid-type-key type-key)
  (let ((resource-name (make-resource-name type-key data))
         (id (getf data :id)))
    (when (and
            (not (base-type-p type-key))
            (not (equal resource-name (find-resource-name type-key id))))
      (a:with-rbac (*rbac*)
        (db:query "update resources set resource_name = $1 where id = $2"
          resource-name id)))))

(defun list-ids (type-key field-key values)
  ":private: Returns a list of IDs of records of type TYPE-KEY where FIELD-KEY
has a value in VALUES."
  (valid-type-key type-key)
  (valid-field-key type-key field-key)
  (valid-values-list values)
  (when values
    (let* ((m *compiled-model*)
            (table (u:tree-get m type-key :table-name))
            (id-col "id")
            (val-col (u:tree-get m type-key :fields field-key :name-sql))
            (sql (format nil "select ~a from ~a where ~a in (~{~a~^,~})"
                   id-col table val-col (placeholders values)))
            (query (cons sql values)))
      (a:with-rbac (*rbac*) (a:rbac-query query :column)))))

(defun unknown-values (type-key field-key values)
  ":private: Returns a list of the values in VALUES that don't exist in the
database for FIELD-KEY of TYPE-KEY. This is useful for validating that the
values in a list of values for a field are valid before performing an insert
or update that includes those values."
  (valid-type-key type-key)
  (valid-field-key type-key field-key)
  (valid-values-list values)
  (loop for value in values
    for id = (be-value-id type-key field-key value "admin")
    when (not id) collect value))

(defun id-to-resource-name (id)
  ":private: Returns the resource name for the resource of type with
ID. This is necessary for dealing with RBAC resources given that the RBAC API
uses names instead of IDs. Returns NIL if ID is not a valid UUID or does not
exist."
  (when (and id (uuid-p id))
    (a:with-rbac (*rbac*)
      (loop for type-key in *compiled-model* by #'cddr
        for sql = "select resource_name from resources where id = $1"
        for resource-name = (when (not (base-resource-type-key-p type-key))
                              (a:rbac-query (list sql id) :single))
        until resource-name
        finally (return resource-name)))))

(defun id-from-data (type-key data)
  ":private: Returns the ID of the record of type TYPE-KEY that matches DATA.
If DATA contains an :ID key, then the value associated with that key is
returned. Otherwise, this function expects DATA to contain a value for each
local field (See the LOCAL-FIELDS function) of TYPE-KEY, and looks up the ID
of the record that matches those values. If no record matches, then this
function returns NIL. This function ignores any non-local fields in DATA.
If DATA points to multiple records, then this function raises an error."
  (or (getf data :id)
    (let ((filters (loop for field in (local-fields type-key)
                     for value = (getf data field)
                     do (valid-value-type type-key field value)
                     collect (list type-key field :eq value))))
      (be-id type-key filters "admin"))))

(defun id-to-type-key (id)
  ":private: Returns the type key associated with ID."
  (valid-uuid id)
  (let* ((resource-name (id-to-resource-name id))
          (type-string (and
                         resource-name
                         (car (re:split ":" resource-name :limit 2)))))
    (and type-string (u:make-keyword type-string))))

(defun user-read-type-ids (user type-key)
  ":private: Returns a list of resource IDs for resources of type TYPE-KEY that
USER has 'read' permissions for."
  (let* ((search (format nil "~(~a~):%" type-key)))
    (mapcar
      (lambda (r) (getf r :resource-id))
      (a:list-user-resources
        *rbac*
        user
        "read"
        :filters `(("resource_name" "like" ,search))))))

(defun user-allowed-resource (user uuid permission)
  ":private: Returns T if USER has PERMISSION on the resource with UUID and NIL
otherwise."
  (let ((resource-name (id-to-resource-name uuid))
         (resource-type (id-to-type-key uuid)))
    (when resource-name
      (if (member resource-type (base-resource-type-keys))
        (equal user "admin")
        (a:user-allowed *rbac* user permission (id-to-resource-name uuid))))))

(defun resource-exists-p (type-key field-key value user)
  ":private: Returns T if a records of type TYPE-KEY exists where FIELD-KEY has
VALUE and USER has 'read' permissions for the record. Otherwise, returns NIL."
  (pl:pdebug
    :in "resource-exists-p"
    :status "checking if resource exists"
    :type-key type-key
    :field-key field-key
    :value (format nil "~s" value)
    :user user
    :type-key-p (type-key-p type-key)
    :field-key-p (field-key-p type-key field-key)
    :value-type-p (value-type-p type-key field-key value)
    :valid-existing-user (a:get-id *rbac* "users" user)
    :be-id (be-id type-key `((,type-key ,field-key :eq ,value)) user))
  (when (and
          (type-key-p type-key)
          (field-key-p type-key field-key)
          (value-type-p type-key field-key value)
          (a:get-id *rbac* "users" user)
          (be-id type-key `((,type-key ,field-key :eq ,value)) user))
    (pl:pdebug :in "resource-exists-p" :status "resource exists"
      :type-key type-key :field-key field-key :value value :user user)
    t))

(defun update-join-tables (type-key id data record)
  ":private: Updates the join tables for TYPE-KEY for the fields in JOIN-KEYS
with the values in DATA."
  (loop
    with m = *compiled-model*
    with update = (u:tree-get m type-key :update-sql)
    with join-keys = (join-keys type-key)
    for key in join-keys
    for column = (u:tree-get m type-key :fields key :source :column)
    for existing-values = (getf record key)
    for new-values = (getf data key)
    for to-add = (set-difference new-values existing-values :test 'equal)
    for to-delete = (set-difference existing-values new-values :test 'equal)
    for q-delete = (u:tree-get update key :delete)
    for q-delete-sql = (car q-delete)
    for q-delete-keys = (cdr q-delete)
    for q-delete-key-1 = (id-key type-key)
    for q-delete-key-2 = (id-key key)
    for q-insert = (u:tree-get update key :insert)
    for q-insert-sql = (car q-insert)
    for q-insert-keys = (cdr q-insert)
    for q-insert-key-1 = (id-key type-key)
    for q-insert-key-2 = (id-key key)
    do
    ;; Delete
    (loop for x-id in (list-ids key column to-delete)
      for q-delete-values = (loop for k in q-delete-keys
                              when (equal k q-delete-key-1) collect id
                              when (equal k q-delete-key-2) collect x-id)
      for q-delete-query = (cons q-delete-sql q-delete-values)
      do (a:with-rbac (*rbac*) (a:rbac-query q-delete-query)))
    ;; Insert
    (loop for x-id in (list-ids key column to-add)
      for q-insert-values = (loop for k in q-insert-keys
                              when (equal k q-insert-key-1) collect id
                              when (equal k q-insert-key-2) collect x-id)
      for q-insert-query = (cons q-insert-sql q-insert-values)
      do (a:with-rbac (*rbac*) (a:rbac-query q-insert-query)))))

(defun join-keys (type-key)
  (loop with update = (u:tree-get *compiled-model* type-key :update-sql)
    for key in update by #'cddr
    unless (equal key :main) collect key))
;;
;; END Internal database helper functions
;;

;;
;; BEGIN Error checking functions
;;

(defun eformat (s &rest params)
  ":private: Formats S with PARAMS, just like the FORMAT function would. But, it
first replaces any single newline in S with a space *unless* that newline is
followed by whitespace (to preserve indented lists, bullets, poems, code
blocks, etc.). Paragraph breaks (two or more consecutive newlines) are
preserved. The result is trimmed of leading/trailing whitespace. This is useful
for formatting error messages and multi-line strings."
  (let ((ss (re:regex-replace-all "([^\\n])\\n([^\\s])" s "\\1 \\2")))
    (u:trim (apply #'format (append (list nil ss) params)))))

(define-condition validation-error (simple-error error)
  ((context :initarg :context :reader context))
  (:report (lambda (c s)
             (apply #'format s
               (simple-condition-format-control c)
               (simple-condition-format-arguments c)))))

(defun signal-validation-error (format-control &rest format-arguments)
  (error 'validation-error
    :format-control format-control
    :format-arguments format-arguments
    :context 'validatioN))

(defun valid-value-type (type-key field-key value)
  ":private: Returns T if VALUE is of the correct type for FIELD-KEY in TYPE-KEY
in the model. Otherwise, returns NIL."
  (unless (value-type-p type-key field-key value)
    (signal-validation-error
      "Invalid value type for field ~s ~s. Expected type ~s, but got value ~s."
      type-key
      field-key
      (u:tree-get *compiled-model* type-key :fields field-key :type)
      value)))

(defun valid-filter (filter &key required)
  ":private: Returns NIL if FILTER is a list of the form (TYPE-KEY FIELD-KEY
OPERATOR-KEY VALUE), with TYPE-KEY being a valid type key in the model,
FIELD-KEY being a valid field key for the type, OPERATOR-KEY being a valid
operator key, and VALUE being of the correct type for the field. Otherwise,
raises a VALIDATION-ERROR condition with a message describing the problem with
FILTER."
  (when (and required (not filter))
    (signal-validation-error "Filter cannot be NIL."))
  (unless (listp filter)
    (signal-validation-error "Filter ~s is not a list." filter))
  (unless (= (length filter) 4)
    (signal-validation-error "Filter ~s does not have 4 elements." filter))
  (destructuring-bind (type-key field-key op-key value) filter
    (unless (type-key-p type-key)
      (signal-validation-error "Invalid type key ~s." type-key))
    (unless (field-key-p type-key field-key)
      (signal-validation-error "Invalid field key ~s for type ~s."
        field-key type-key))
    (unless (operator-key-p op-key)
      (signal-validation-error "Invalid operator key ~s." op-key))
    (unless (field-type-key-p
              (u:tree-get *compiled-model* type-key :fields field-key :type))
      (signal-validation-error "Invalid field type key for field ~s ~s ~s."
        type-key field-key type-key))
    (unless (value-type-p type-key field-key value)
      (signal-validation-error
        (eformat "
Invalid value type for field ~s ~s.
Expected a value of type ~s, but got value ~s."
          type-key field-key
          (u:tree-get *compiled-model* type-key :fields field-key :type)
          value)))))

(defun valid-filters (filters &key required)
  ":private: Checks that FILTERS is valid."
  (when (and required (null filters))
    (signal-validation-error "FILTERS cannot be NIL."))
  (loop for filter in filters
    do (valid-filter filter :required t)))

(defun valid-type-key (type-key)
  ":private: Checks that TYPE-KEY is a valid type key in the model."
  (unless (type-key-p type-key)
    (signal-validation-error "Invalid type key ~a" type-key)))

(defun valid-field-key (type-key field-key)
  ":private: Checks that FIELD-KEY is a valid field key for TYPE-KEY in the
model."
  (unless (field-key-p type-key field-key)
    (signal-validation-error "Invalid field key ~a ~a." type-key field-key)))

(defun valid-data (type-key data)
  ":private: Checks that DATA is valid for TYPE-KEY. DATA is a plist where the
keys are field keys and the values are the values to be inserted or updated.
This function checks that each field key is valid for TYPE-KEY and that each
value is of the correct type for its field key."
  (valid-type-key type-key)
  (loop
    for field-key in data by #'cddr
    for value in (cdr data) by #'cddr
    for field-def = (u:tree-get *compiled-model* type-key :fields field-key)
    do
    (cond
      ((getf field-def :join-table)
        (valid-values-list value))
      ((not (getf field-def :base))
        (valid-value-type type-key field-key value))
      ((equal field-key :id)
        (valid-uuid value))
      (t (signal-validation-error
           "Invalid field key ~s for type ~s." field-key type-key)))))

(defun valid-existing-join-data (type-key data)
  ":private: Checks that the values in DATA for JOIN-KEYS exist in the database
for TYPE-KEY."
  (loop
    with m = *compiled-model*
    with join-keys = (join-keys type-key)
    for key in join-keys
    for column = (u:tree-get m type-key :fields key :source :column)
    for new-values = (or (getf data key)
                       (u:tree-get m type-key :fields  key :default))
    for unknown-values = (unknown-values key column new-values)
    when unknown-values
    do (signal-validation-error
         "Invalid value~p for field ~s ~s: ~s"
         (length unknown-values) type-key key unknown-values)))

(defun valid-uuid (uuid)
  (unless (uuid-p uuid)
    (signal-validation-error "Invalid UUID ~s" uuid)))

(defun valid-existing-uuid (uuid)
  (unless (uuid-exists-p uuid)
    (signal-validation-error "UUID ~a not found." uuid)))

(defun valid-insert (insert)
  (unless (and (u:plistp insert) (getf insert :main))
    (signal-validation-error "Invalid insert ~s" insert)))

(defun valid-roles (&rest roles)
  (loop for role in (u:flatten roles)
    unless (be-id :roles `((:roles :name :eq ,role)) "admin")
    do (signal-validation-error "Invalid role name ~s." role)))

(defun valid-existing-roles (roles)
  (valid-roles roles)
  (loop with existing-roles = (a:list-role-names *rbac*)
    for role in roles
    when (not (member role existing-roles :test 'equal))
    do (signal-validation-error "Role ~s does not exist." role)))

(defun valid-user-roles (user roles)
  ":private: Returns nil (success) if every role in ROLES exists and either USER
is 'admin' or USER has every role in ROLES. Otherwise, raises a validation
error."
  (valid-existing-user user)
  (valid-existing-roles roles)
  (let* ((user-roles (a:list-user-role-names *rbac* user))
          (non-user-roles (if (equal user "admin")
                            nil
                            (remove-if
                              (lambda (r) (member r user-roles :test 'equal))
                              roles))))
    (when non-user-roles
      (signal-validation-error
        "User ~s does not have the following roles: ~{~s~^, ~}."
        user non-user-roles))))

(defun valid-existing-user (user)
  (unless (a:get-id *rbac* "users" user)
    (signal-validation-error "User ~s does not exist." user)))

(defun valid-user-permissions (user type-key &rest permissions)
  (valid-existing-user user)
  (loop with resource-name = (type-resource-name type-key)
    for permission in permissions
    unless (a:user-allowed *rbac* user permission resource-name)
    do (signal-validation-error
         "User ~a does not have ~a permission for resource of type ~s."
         user permission type-key)))

(defun valid-values-list (values)
  (unless (listp values)
    (signal-validation-error "Expected a list of values, but got ~s." values))
  (unless (every #'atom values)
    (signal-validation-error "Expected a list of atomic values, but got ~s."
      values)))

(defun id-key (type-key)
  (valid-type-key type-key)
  (u:make-keyword (format nil "~a-id" (u:singular (format nil "~a" type-key)))))

;;
;; END Error checking functions
;;

;;
;; BEGIN Public backend functions
;;

(defun be-id (type-key filters user)
  ":public: Returns the ID of the resource of type TYPE-KEY that matches
FILTERS, provided the record is accessible to USER. FILTERS is a list of
filters, where each filter contains a type key (not necessarily TYPE-KEY), a
field key, an operator, and a value. All filters must match in order for a
record to be selected. Matching more than one record is an error.

Alternatiively, FILTERS may be a string instead of a list, and in that case,
FILTERS must be the UUID of the record. If FILTERS is a string, then this
function simply returns FILTERS, without performing any database query.

Matching no records returns NIL.

This function returns a string representing the ID of the matched record,
NIL if no records match, or an error if more than one record matches. An
error consists of a list of error messages, where each message is a string.

The following example returns the ID of the user with the role \"admin\":
    `(be-id :users '((:roles :name :eq \"admin\")))`

If there are multiple users with the role \"admin\", then the above example
returns an error.

Users have distinct user names, thus the following will never return an
error:
    `(be-id :users '((:users :name :eq \"guest\")))`
"
  (valid-type-key type-key)
  (valid-existing-user user)
  (if (stringp filters)
    (valid-uuid filters)
    (valid-filters filters :required t))
  (let* ((m *compiled-model*)
          (is-joiner (u:tree-get m type-key :is-joiner))
          (is-base (u:tree-get m type-key :base))
          (is-admin (member "admin"
                      (a:list-user-role-names *rbac* user)
                      :test 'equal))
          (sql (u:tree-get m type-key :views :main :sql))
          (efilters (if (uuid-p filters)
                      `((,type-key :id :eq ,filters))
                      filters))
          (where (add-where-clause sql efilters))
          (view-result (a:with-rbac (*rbac*) (a:rbac-query where)))
          (field-keys '(:id))
          (result (view-result-values type-key field-keys view-result)))
    (cond
      ((zerop (length result)) nil)
      ((> (length result) 1) (error "More than one match."))
      (t (let ((id (getf (car result) :id)))
           (if (and is-base (not is-joiner))
             (when is-admin id)
             (when (user-allowed-resource user id "read") id)))))))

(defun be-value-id (type-key field-key value user)
  ":public: Returns the ID of the record of type TYPE-KEY where FIELD-KEY has
the value VALUE."
  (valid-type-key type-key)
  (valid-field-key type-key field-key)
  (valid-value-type type-key field-key value)
  (valid-existing-user user)
  (be-id type-key `((,type-key ,field-key :eq ,value)) user))

(defun be-rec (id user &key (form :update-form) type-key)
  ":public: Returns the TYPE-KEY record with the given ID, provided that it is
accessible to USER. Given the IDs are UUIDs (globally unique), TYPE-KEY is
optional. However, providing TYPE-KEY helps the function avoid an extra database
lookup."
  (valid-existing-user user)
  (when type-key (valid-type-key type-key))
  (when (and (user-allowed-resource user id "read") (uuid-exists-p id))
    (let* ((m *compiled-model*)
            (type-key (if type-key type-key (id-to-type-key id)))
            (sql (u:tree-get m type-key :views :main :sql))
            (where (add-where-clause sql (list (list type-key :id :eq id))))
            (view-result (a:with-rbac (*rbac*) (a:rbac-query where)))
            (field-keys (form-field-keys type-key form)))
      (car
        (view-result-values type-key field-keys view-result)))))

(defun be-val (id field-key user &key (form :update-form) type-key)
  ":public: Returns the value for field FIELD-KEY in the TYPE-KEY record with
ID. Providing TYPE-KEY is optional, but helps the function avoid an extra
database query."
  (valid-existing-uuid id)
  (valid-existing-user user)
  (when type-key
    (valid-type-key type-key)
    (valid-field-key type-key field-key)
    (valid-user-permissions user type-key "read"))
  (let ((record (be-rec id user :form form :type-key type-key)))
    (getf record field-key)))

;; This function relies on a single query when there are no filters and on 2
;; queries where there are filters. This is necessary with the current approach
;; because the first query returns N rows, then the code collapses those rows
;; into some number <= N. The orginal result contains multiple rows for some
;; given ID when the ID is associated with multiple items in another table (one
;; row for each association). The code that collapses the results reduces the
;; number of rows to the number of distinct IDs in the result, converting the
;; associated values into lists for the field that holds the associations.  This
;; is bad, because the second query does something like `where id in (...)`.
;; Eventually we'll need to fix this so that the collapsing occurs in the
;; database, with something like `array_agg(...) group by id` in the SQL.
;;
;; TODO: Add pagination support
(defun be-list (type-key user &key (form :list-form) filters)
  ":public: Returns a list of records of type TYPE-KEY that match FILTERS and
that USER has `read` permissions for. Each record is returned as a plist, where
the keys are field keys and the values are the corresponding field values. The
following example returns a list of all the :todos records that have the tag
'chores' and that the user 'admin' has permission to read:

    `(be-list :todos \"admin\" :filters '((:tags :name :eq \"chores\")))`"
  (valid-type-key type-key)
  (valid-user-permissions user type-key "read")
  (let* ((m *compiled-model*)
          (user-roles (a:list-user-role-names *rbac* user))
          (ids (if (base-resource-type-key-p type-key)
                 (when (member "admin" user-roles :test 'equal)
                   (let ((table (table-name type-key
                                  (u:tree-get m type-key :base))))
                     (a:with-rbac (*rbac*)
                       (db:query (format nil "select id from ~a" table)
                         :column))))
                 (user-read-type-ids user type-key))))
    (when ids
      (let* ((all-filters (append filters `((,type-key :id :in ,ids))))
              (sql (u:tree-get m type-key :views :main :sql))
              (where (add-where-clause sql all-filters))
              (view-result (a:with-rbac (*rbac*) (a:rbac-query where)))
              (field-keys (form-field-keys type-key form)))
        (if (filters-require-join-p type-key filters)
          (let* ((ids (view-result-ids type-key field-keys view-result))
                  (ids-query (add-ids-clause type-key sql ids))
                  (view-result (a:with-rbac (*rbac*) (a:rbac-query ids-query))))
            (view-result-values type-key field-keys view-result))
          (view-result-values type-key field-keys view-result))))))

;; TODO: Add pagination support
(defun be-list-column (type-key field-key user &key filters)
  ":public: Returns a list of the values in FIELD-KEY for the records of type
TYPE-KEY that match FILTERS and that USER has 'read' permissions for. This is
like BE-LIST, but it returns a list of values instead of a list of records."
  (valid-type-key type-key)
  (valid-field-key type-key field-key)
  (valid-existing-user user)
  (if (uuid-p filters)
    (valid-existing-uuid filters)
    (valid-filters filters))
  (let ((records (be-list type-key user :filters filters)))
    (mapcar (lambda (r) (getf r field-key)) records)))

;; TODO: Transaction!
(defun be-insert (type-key data user &key roles)
  ":public: Inserts a record of type TYPE-KEY with the given DATA, provided
USER has `create` permissions in TYPE-KEY. DATA is a plist where the keys are
field keys and the values are the values to be inserted. This function returns
two values: The ID of the record and T if the record this function inserted the
record or NIL if the error already exists.  This function does not perform an
update if a record with the same unique fields already exists. The following
example inserts a todo with the name \"clean the kitchen\":

    `(be-insert :todos
                '(:name \"clean the kitchen\"
                  :tags (\"chores\" \"kitchen\")
                \"admin\")`
"
  (valid-type-key type-key)
  (valid-user-roles user roles)
  (valid-user-permissions user type-key "create")
  (let ((data (full-data type-key data)))
    (valid-data type-key data)
    (let ((id (id-from-data type-key data)))
      (if id
        (values id nil)
        (let* ((insert (u:tree-get *compiled-model* type-key :insert-sql))
                ;; Insert RBAC resource row
                (resource-name (make-resource-name type-key data))
                (uuid (a:add-resource *rbac* resource-name :roles roles))
                (f (u:tree-get *compiled-model* type-key :create)))
          (cond
            ((equal f :auto)
              (pl:pdebug :in "be-insert" :branch "auto-insert"
                :type-key type-key :uuid uuid :data data)
              ;; Insert TYPE-KEY row
              (a:with-rbac (*rbac*)
                (a:rbac-query (insert-main-query type-key insert uuid data)))
              ;; Insert rows in join tables
              (pl:pdebug :in "be-insert" :status "inserting join table rows"
                :type-key type-key :data data)
              (loop with keys = (remove-if
                                  (lambda (k) (member k '(:resource :main)))
                                  (u:plist-keys insert))
                for key in keys
                for qt = (getf insert key)
                for sql = (car qt)
                for names = (getf data key)
                ;; TODO: Use list-ids function instead of be-id
                for value-ids = (list-ids key :name names)
                do (pl:pdebug :in "be-insert" :status "inserting join table rows"
                     :type-key type-key :field-key key :names names
                     :value-ids value-ids :data data)
                when value-ids do
                (loop for value-id in value-ids
                  for query = (cons sql (list uuid value-id))
                  do
                  (pl:pdebug :in "be-insert" :status "join table insert"
                    :sql sql :uuid uuid :valid-id value-id)
                  (a:with-rbac (*rbac*) (a:rbac-query query))))
              ;; Roles update
              (when roles (update-roles type-key uuid roles))
              (values uuid t))
            ((functionp f)
              (funcall f type-key data user :roles roles))
            (t (signal-validation-error
                 "Invalid create function for type ~s: ~s" type-key f))))))))

;; TODO:
;;   - Transaction
;;   - Don't update if there are no changes
(defun be-update (type-key filters data user &key roles)
  ":public: Updates the record of type TYPE-KEY that matches FILTERS with the
given DATA.

FILTERS is a list of filters, where each filter contains a type key (not
necessarily TYPE-KEY), a field key, an operator, and a value. All filters must
match in order for a record to be selected. Matching more than one record is an
error. Alternatively, FILTERS may be string instead of a list, and in that case,
FILTER must be the UUID of the record to be updated.

DATA is a plist where the keys are field keys and the values are the new values
for those fields.

This function returns the ID of the updated record, or a list of errors if the
update fails.
"
  (valid-type-key type-key)
  (if (stringp filters)
    (valid-uuid filters)
    (valid-filters filters :required t))
  (valid-data type-key data)
  (valid-user-roles user roles)
  (let* ((m *compiled-model*)
          (uuid (id-from-filters-and-data type-key filters data))
          (record (be-rec uuid user :type-key type-key))
          (sql (car (u:tree-get m type-key :update-sql :main)))
          (values (local-values-for-update type-key data record :id uuid))
          (update-query (cons sql values))
          (full-data (full-data type-key data :record record)))
    (valid-existing-join-data type-key full-data)
    ;; Update TYPE-KEY row (main update)
    (when (equal type-key :resources)
      (signal-validation-error "Can't update internal type :resources"))
    (multiple-value-bind (result update-row-count)
      (a:with-rbac (*rbac*) (a:rbac-query update-query))
      (declare (ignore result))
      (unless (= update-row-count 1)
        (error "Error updating main ~a row (update row count ~d)"
          type-key update-row-count))
      update-row-count)
    ;; Update resource name (update resources record)
    (update-resource-name type-key full-data)
    ;; Update join tables
    (update-join-tables type-key uuid full-data record)
    ;; Update roles
    (when roles (update-roles type-key data roles))
    uuid))

(defun be-delete (type-key filters user)
  ":public: Deletes the record of type TYPE-KEY that matches FILTERS. If more
than one record matches, this function raises an error. If no records match,
this function does nothing. Upon successful deletion, this function returns
the ID of the deleted record.

FILTERS is a list of filters, where each filter contains a type key (not
necessarily TYPE-KEY), a field key, an operator key, and a value of the
appropriate type for the field. All filters must match in order for a record to
be selected. Alternatively, FILTERS may be string instead, in which case it is
treated as the UUID of the record to be deleted."
  (valid-type-key type-key)
  (if (stringp filters)
    (valid-uuid filters)
    (valid-filters filters :required t))
  (valid-existing-user user)
  (let ((uuid (id-from-filters-and-data type-key filters)))
    (when (and uuid (user-allowed-resource user uuid "delete"))
      (let* ((record (be-rec uuid user :type-key :resources))
             (resource-name (getf record :name)))
        (when resource-name
          (a:remove-resource *rbac* resource-name)
          uuid)))))

(defun be-add-type-roles (type-key user &rest roles)
  ":public: Adds ROLE to the list of roles associated with TYPE-KEY. This
function returns T if ROLES are successfully added or determined to exist, and a
validation-error if TYPE-KEY doesn't exist or ROLES includes a ROLE that doesn't
exist or that is not available to USER."
  (valid-type-key type-key)
  (valid-user-roles user roles)
  (let ((resource-name (type-resource-name type-key)))
    (loop with resource-roles = (a:list-resource-role-names *rbac* resource-name)
      for role in (u:distinct-values roles)
      unless (member role resource-roles :test 'equal)
      do
      (pl:pdebug :in "be-add-type-roles" :status "adding type role"
        :type-key type-key :role role)
      (a:add-resource-role *rbac* resource-name role))
    (a:list-resource-role-names *rbac* resource-name)))

(defun be-remove-type-roles (type-key user &rest roles)
  ":public: Removes ROLE from the list of roles associated with TYPE-KEY. This
function returns T if ROLES are successfully removed or determined not to be
associated with TYPE-KEY. The function raises a VALIDATION-ERROR if TYPE-KEY
doesn't exist or ROLES includes a ROLE that doesn't exist or that is not
available to USER. The admin role cannot be removed from a type, this function
ignores that role if it's included in ROLES."
  (valid-type-key type-key)
  (valid-user-roles user roles)
  (let ((resource-name (type-resource-name type-key)))
    (loop with user-roles = (a:list-user-role-names *rbac* user)
      for role in (U:distinct-values roles)
      when (member role user-roles :test 'equal)
      do (a:remove-resource-role *rbac* resource-name role))
    (a:list-resource-role-names *rbac* resource-name)))

(defun be-validate-field (type-key field-key value user)
  ":public: Validates that VALUE is acceptable for FIELD-KEY of TYPE-KEY. This
function returns

    (:valid t)

if VALUE is valid for the field, and

    (:valid nil :errors {error-message-list})

if VALUE is not valid for the field. `{error-message-list}` is a list of strings
describing the problems with VALUE."
  (loop
    with validations = (u:tree-get *compiled-model*
                         type-key :fields field-key :validations)
    initially
    (valid-type-key type-key)
    (valid-field-key type-key field-key)
    for validation in validations
    for clean-value = (if (listp value)
                        (mapcar #'u:trim value)
                        (u:trim value))
    for e = (funcall validation type-key field-key clean-value user)
    when e collect e into errors
    finally (return
              (if errors
                (list :valid :false :errors errors)
                (list :valid :true)))))

(defun be-validate-form (type-key values user)
  ":public: Validates that each value in the VALUES plist is acceptable for its
field in TYPE-KEY. In VALUES, the keys are field keys and the values are the
values to be validated for those fields. This function returns

    (:valid t)

when all values are valid, and

    (:valid nil :errors {error-message-list})

when even one value is invalid. `{error-message-list}` is a list of strings."
  (loop with data = (loop for key in (user-fields type-key)
                      for value = (getf values key)
                      append (list key value))
    for field-key in data by #'cddr
    for value in (cdr data) by #'cddr
    for e = (be-validate-field type-key field-key value user)
    when (equal (getf e :valid) :false)
    append (getf e :errors) into errors
    finally (return
              (if errors
                (list :valid :false :errors errors)
                (list :valid :true)))))

;;
;; END Public backend functions
;;
