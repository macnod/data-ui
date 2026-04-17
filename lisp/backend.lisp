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
    (:distinct (u:distinct-values values))))

(defun field-values-for-id (view-result id-key id-value alias-key aggregation)
  ":private: Retrieves field values for the record where ID-KEY contains
ID-VALUE. VIEW-RESULT is the result of running an SQL query that's called
a view in Data UI. ALIAS-KEY is used to retrieve a value from each row in
VIEW-RESULT. AGGREGATION is a keyword that designates a function to be performed
against the retrieved values. This function returns the result of calling
the aggregation function on the retrieved values."

  (loop for row in view-result
    when (equal (getf row id-key) id-value)
    collect (getf row alias-key) into values
    finally (return (aggregate-values aggregation values))))

(defun view-result-values (type-key field-keys view-result)
  ":private:"
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
      for index = (next-param-index sql) then (1+ index)
      for (table-key field-key op-key value) in filters
      for alias = (to-sql-identifier
                    (u:tree-get *compiled-model*
                      table-key :fields field-key
                      :source :column-name))
      for op = (operator-sql op-key)
      collect (format nil "~a ~a $~d" alias op index)
      into conditions
      finally
      (return
        (cons
          (format nil "~a~%where~%  ~{~a~^~%  and ~}~%" sql conditions)
          (mapcar #'cadddr filters))))
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
    collect (u:tree-get m type-key :fields field-key :source :alias-key)))

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

(defun plist-select (plist keys)
  ":private: Returns selected key/value pairs in PLIST, where a pair is selected
if the key is among KEYS."
  (loop for key in keys
    append (list key (getf plist key))))

(defun plist-exclude (plist keys)
  ":private: Returns key/value pairs in PLIST, excluding pairs where the key is
among KEYS."
  (loop for key in plist by #'cddr
    unless (member key keys)
    append (list key (getf plist key))))

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

(defun resource-name (type-key data)
  ":private: Returns a resource name for DATA, a row of TYPE-KEY. This name is
composed of the type-key, the value associated with the first key in DATA, and a
short (8 chars) hash of the concatenated, colon-separated values in DATA. We
want resource names to be unique, and some types, such as user-settings might
have the same :NAME and require the additional :USER value to to be unique."
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
          unless (equal k :id)
          collect (getf data k))))))

(defun id-from-filters (filters)
  ":private: Returns an ID if FILTERS includes a filter with a field key of :ID
and an operator key of :EQ. Otherwise, returns NIL."
  (loop for (table-key field-key op-key value) in filters
         when (and (equal field-key :id) (equal op-key :eq))
         do (return value)))

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
         (lookup-id (unless (or data-id filters-id)
                      (and filters (be-id type-key filters)))))
    (or data-id filters-id lookup-id)))

;;
;; END Internal helper functions
;;

;;
;; BEGIN Database helper functions
;;

(defun update-roles (type-key data roles)
  ":private: Updates the RBAC roles for the resource of type TYPE-KEY with ID.
ROLES is a list of role names. ID is a UUID string. This function returns NIL if
the update is successful, or an error if the update fails."
  ;; Compute the existing roles for the resource
  (let* ((roles (if (member "admin" roles) roles (cons "admin" roles)))
          (resource-name (resource-name type-key data))
          (existing-roles (a:list-resource-role-names *rbac* resource-name))
          (to-add (set-difference roles existing-roles :test 'equal))
          (to-remove (set-difference existing-roles roles :test 'equal)))
    ;; Add new roles
    (loop for role in to-add
      do (a:add-resource-role *rbac* resource-name role))
    ;; Remove old roles
    (loop for role in to-remove
      do (a:remove-resource-role *rbac* resource-name role))))

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


;;
;; END Internal database helper functions
;;

;;
;; BEGIN Error checking functions
;;

(defun eformat (s &rest params)
  ":private: Formats S with PARAMS, just like the FORMAT function would. But, it
first replaces any single newline in S with a spaces. This allows paragraphs to
be formatted as long lines, but retains paragraph breaks when there is more than
one consecutive newline. This is useful for formatting error messages, which
would otherwise turn into long lines in the code."
  (let ((ss (re:regex-replace-all "([^\\n])\\n([^\\n])" s (format nil "\\1 \\2"))))
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
  (if (type-key-p type-key)
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
             "Invalid field key ~s for type ~s." field-key type-key))))))

(defun valid-uuid (uuid)
  (unless (uuid-p uuid)
    (signal-validation-error "Ivalid UUID ~s")))

(defun valid-insert (insert)
  (unless (and (u:plistp insert) (getf insert :main))
    (signal-validation-error "Invalid insert ~s" insert)))

(defun valid-roles (roles)
  (when roles
    (loop for role in roles
      unless (a:valid-role-p *rbac* role)
      do (signal-validation-error "Invalid role name ~s." role))))

(defun valid-existing-roles (roles)
  (valid-roles roles)
  (loop with existing-roles = (a:list-role-names *rbac*)
    for role in roles
    when (not (member role existing-roles :test 'equal))
    do (signal-validation-error "Role ~s does not exist." role)))

(defun valid-values-list (values)
  (unless (listp values)
    (signal-validation-error "Expected a list of values, but got ~s." values))
  (unless (every #'atom values)
    (signal-validation-error "Expected a list of atomic values, but got ~s."
      values)))

(defun id-key (type-key)
  (u:make-keyword (format nil "~a-id" (u:singular (format nil "~a" type-key)))))

;;
;; END Error checking functions
;;

;;
;; BEGIN Public backend functions
;;

(defun be-id (type-key filters)
  ":public: Returns the ID of the resource of type TYPE-KEY that matches
FILTERS. FILTERS is a list of filters, where each filter contains a type
key (not necessarily TYPE-KEY), a field key, an operator, and a value. All
filters must match in order for a record to be selected. Matching more than one
record is an error.

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
  (if (stringp filters)
    (valid-uuid filters)
    (valid-filters filters :required t))
  (let* ((m *compiled-model*)
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
      (t (getf (car result) :id)))))

(defun be-value-id (type-key field-key value)
  ":public: Returns the ID of the record of type TYPE-KEY where FIELD-KEY has
the value VALUE."
  (be-id type-key `((,type-key ,field-key :eq ,value))))

(defun be-item (type-key id &key (form :update-form))
  ":public: Returns the data associated with the TYPE-KEY record with the given
ID. This is perfect for rendering the data in the front end as a form."
  (let* ((m *compiled-model*)
          (sql (u:tree-get m type-key :views :main :sql))
          (where (add-where-clause sql (list (list type-key :id :eq id))))
          (view-result (a:with-rbac (*rbac*) (a:rbac-query where)))
          (field-keys (form-field-keys type-key form)))
    (car
      (view-result-values type-key field-keys view-result))))

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
(defun be-list (type-key &key (form :list-form) filters)
  (let* ((m *compiled-model*)
          (sql (u:tree-get m type-key :views :main :sql))
          (where (add-where-clause sql filters))
          (view-result (a:with-rbac (*rbac*) (a:rbac-query where)))
          (field-keys (form-field-keys type-key form)))
    (if (filters-require-join-p type-key filters)
      (let* ((ids (view-result-ids type-key field-keys view-result))
              (ids-query (add-ids-clause type-key sql ids))
              (view-result (a:with-rbac (*rbac*) (a:rbac-query ids-query))))
        (view-result-values type-key field-keys view-result))
      (view-result-values type-key field-keys view-result))))

;; TODO: Transaction!
(defun be-insert (type-key data &key roles)
  ":public: Inserts a record of type TYPE-KEY with the given DATA. DATA is a
plist where the keys are field keys and the values are the values to be
inserted. This function returns the ID of the newly inserted record, or an error
if the insert fails. The following example inserts a user with the name \"john\"
and the role \"admin\":

    `(be-insert :users '(:name \"john\" :roles (\"admin\")))`
"
  (valid-type-key type-key)
  (valid-data type-key data)
  (valid-existing-roles roles)
  (let* ((insert (u:tree-get *compiled-model* type-key :insert-sql))
          ;; Insert RBAC resource row
          (resource-name (resource-name type-key data))
          (uuid (a:add-resource *rbac* resource-name :roles roles)))
    ;; Insert TYPE-KEY row
    (a:with-rbac (*rbac*)
      (a:rbac-query (insert-main-query type-key insert uuid data)))
    ;; Insert rows in join tables
    (loop with keys = (remove-if
                        (lambda (k) (member k '(:resource :main)))
                        (u:plist-keys insert))
      for key in keys
      for qt = (getf insert key)
      for sql = (car qt)
      for names = (getf data key)
      ;; TODO: Use list-ids function instead of be-id
      for value-ids = (loop for name in names
                        collect (be-id key `((,key :name :eq ,name))))
      do (loop for value-id in value-ids
           for query = (cons sql (list uuid value-id))
           do (a:with-rbac (*rbac*) (a:rbac-query query))))
    ;; Roles update
    (when roles (update-roles type-key data roles))
    uuid))

;; TODO:
;;   - Transaction
;;   - Don't update if there are no changes
(defun be-update (type-key filters data &key roles)
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
  (valid-existing-roles roles)
  (let* ((m *compiled-model*)
          (update (u:tree-get m type-key :update-sql))
          (sql (car (getf update :main)))
          (uuid (id-from-filters-and-data type-key filters data))
          (record (be-item type-key uuid))
          (local-fields (local-fields type-key))
          (values (loop for k in local-fields
                    collect (or (getf data k) (getf record k))))
          (update-query (append (cons sql values) (list uuid))))
    ;; Update TYPE-KEY row
    (a:with-rbac (*rbac*) (a:rbac-query update-query))
    ;; Update join tables
    (loop
      with keys = (remove-if (lambda (k) (equal k :main)) (u:plist-keys update))
      for key in keys
      for column = (u:tree-get m type-key :fields key :source :column)
      for existing-values = (getf record key)
      for new-values = (or (getf data key) existing-values)
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
      (loop for id in (list-ids key column to-delete)
        for q-delete-values = (loop for k in q-delete-keys
                                when (equal k q-delete-key-1) collect uuid
                                when (equal k q-delete-key-2) collect id)
        for q-delete-query = (cons q-delete-sql q-delete-values)
        do (a:with-rbac (*rbac*) (a:rbac-query q-delete-query)))
      ;; Insert
      (loop for id in (list-ids key column to-add)
        for q-insert-values = (loop for k in q-insert-keys
                                when (equal k q-insert-key-1) collect uuid
                                when (equal k q-insert-key-2) collect id)
        for q-insert-query = (cons q-insert-sql q-insert-values)
        do (a:with-rbac (*rbac*) (a:rbac-query q-insert-query))))
    ;; Update roles
    (when roles (update-roles type-key data roles))
    uuid))

;;
;; END Public backend functions
;;
