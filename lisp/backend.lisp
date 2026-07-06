(in-package :data-ui)

;;
;; BEGIN Internal helper functions
;;

(defun placeholders (fields &key (start-at 1))
  (loop for a from start-at to (1- (+ start-at (length fields)))
    collect (format nil "$~d" a)))

(defun value-or-nil (value)
  ":private: Returns value unless value is the keyword :null, in which case this
function returns NIL. This is useful for dealing the values that the database
returns for fields that are null."
  (if (equal value :null) nil value))

(defun aggregate-values (aggregation values)
  ":private: Performs AGGREGATION on the VALUES list."
  (when values
    (case aggregation
      (:first (value-or-nil (car values)))
      (:list (remove-if-not #'value-or-nil values))
      (:distinct (u:distinct-values (remove-if-not #'value-or-nil values)))
      (t (error "Invalid aggregation ~s." aggregation)))))

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
    when (equal (getf row id-key) id-value)
    collect value into values
    finally (return (aggregate-values aggregation values))))

(defun alias-key-to-field-key (type-key alias-key)
  ":private: Returns the field key associated with ALIAS-KEY in the :VIEWS
metadata for TYPE-KEY."
  (loop
    with m = *compiled-model*
    with aliases = (u:tree-get m type-key :views :main :aliases)
    for alias-type-key in aliases by #'cddr
    for alias-type-def in (cdr aliases) by #'cddr
    for field-key = (loop for field-key in alias-type-def by #'cddr
                      for alias in (cdr alias-type-def) by #'cddr
                      when (equal alias alias-key)
                      do (return field-key))
    when field-key do (return field-key)))

(defun view-result (type-key query)
  ":private: Runs QUERY against the database and returns the result. QUERY is a
list whose car is an SQL string with placeholders and whose cdr is a list of the
values for those placeholders. The result is a list of plists, where each plist
represents a row in the query result, with the keys being the column names or
aliases from the SQL and the values being the corresponding field values for
that row. TYPE-KEY is used to look up the type of each field, to determine if
any further transformation of the field's value is necessary, such as
converting timetstamps from integers to ISO 8601 strings."
  (let ((result (a:with-rbac (*rbac*) (a:rbac-query query))))
    (loop for row in result
      collect
      (loop with m = *compiled-model*
        with fields = (u:tree-get m type-key :fields)
        and aliases = (u:tree-get m type-key :views :main :aliases)
        for key in row by #'cddr
        for value in (cdr row) by #'cddr
        for field-key = (alias-key-to-field-key type-key key)
        for field-type = (u:tree-get fields field-key :type)
        for final-value = (case field-type
                            (:timestamp (when (and
                                                value
                                                (not (equal value :null)))
                                          (dt:timestamp-string
                                            :universal-time value)))
                            (:boolean (if value :true :false))
                            (t value))
        appending (list key final-value)))))

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
  (when view-result
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
            (field-values-for-id view-result id-key id alias aggregation)))))))

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

(defun add-where-clause (sql filters user)
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
      for log = (pl:pdebug :in "add-where-clause"
                  :table-key table-key
                  :field-key field-key
                  :op-key op-key
                  :value value
                  :alias alias)
      for placeholders = (if (member op-key '(:in :not-in))
                           (format nil "(~{~a~^, ~})"
                             (placeholders value :start-at index))
                           (format nil "$~d" index))
      collect (db-value table-key field-key user value) into values
      collect (format nil "~a ~a ~a" alias op placeholders)
      into conditions
      finally
      (let ((where (format nil "~a~%where~%  ~{~a~^~%  and ~}~%"
                     sql conditions))
             (params (u:flatten values)))
        (pl:pdebug :in "add-where-clause" :where where :params params :filters filters)
        (return (cons where (u:flatten values)))))
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
  (let* ((type-def (u:tree-get *compiled-model* type-key))
          (form-field-keys (u:tree-get type-def form :fields))
          (all-field-keys (remove-if
                            (lambda (k)
                              (equal
                                (u:tree-get type-def :fields k :type)
                                :file))
                            (u:plist-keys (u:tree-get type-def :fields)))))
    (cond
      ((equal form-field-keys t)
        all-field-keys)
      ((listp form-field-keys) (not (zerop (length form-field-keys)))
        (cons :id form-field-keys))
      ((null form-field-keys)
        nil)
      (t (report-e "form-field-keys" "Invalid ~s specification for ~s"
           ~form ~type-key)))))

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
             (not (getf field-def :column))
             (member field-key exclude))
    collect field-key))

(defun db-value (type-key field-key user value)
  (let* ((field-def (u:tree-get *compiled-model* type-key :fields field-key))
         (target (getf field-def :target))
         (joiner (getf field-def :join-table)))
    (cond ((and target (not joiner))
           (resolve-reference-id type-key field-key value))
          (t
           (let ((type (getf field-def :type)))
             (case type
               (:text value)
               (:password (a:password-hash user value))
               (:real (if (numberp value) value (parse-number value)))
               (:integer (if (numberp value) value (parse-number value)))
               (:boolean (format nil "~(~a~)" value))
               (:uuid value)
               (:timestamp value)
               (:list value)
               (t (report-ve "db-value"
                    "Invalid value ~s for field ~s ~s with field type ~s"
                    ~value ~type-key ~field-key ~type))))))))

(defun local-values-for-update (type-key data record user &key id)
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
    for field-type = (getf field-def :type)
    for column = (getf field-def :column)
    when column
    collect (if (and (eq field-type :password) (not data-value))
              record-value
              (db-value type-key field-key user field-value))))

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
  ":private: Returns a resource name for DATA, representing a new or existing
record of type TYPE-KEY. This name is composed of the type-key, the first 10
characters of the stringified value associated with one of the fields in DATA,
and a random UUID. If the :name field is present in DATA and valid for TYPE-KEY,
that field value is chosen.  Otherwise, the first non-base field from the
compiled model that appears in DATA is used. The resulting ID looks like this:

  todos:todo-1:6b3de23c-bf06-9b80-1e8e-40e9b66c3eaa

This function signals error conditions for the following:

  - Invalid value in TYPE-KEY
  - Value of TYPE-KEY points to a :base type
  - DATA is not a plist
  - DATA lacks a key that is among the non-default fields keys for TYPE-KEY
    in the compiled model
  - A value in DATA does not comply with it's keys declared type

Upon success, this function returns the resource name.

The purpose of the resource name is to give the developer or debugger a chance
to identify resources directly in the resources table, without having to do
joins. A join might still be necessary in some cases, because this
resource-naming convention will fail to identify records uniquely in some cases.

Also, the resources table requires every resource name, across all other
tables, to have a unique name. The resources table is what allows this code to
transparently weave RBAC capabilities into all user-defined types.

**WARNING**: Do not use this function to retrieve the resource name for an
existing record! Use FIND-RESOURCE-NAME instead."
  (valid-non-base-type-key type-key)
  (valid-data type-key data)
  (let* ((type-string (format nil "~(~a~)" type-key))
          (base-fields (default-fields
                         :model *compiled-model*
                         :type-key type-key
                         :keys-only t))
          (data-fields (u:plist-keys data))
          (remaining-fields (remove-if-not #'identity
                              (stable-set-difference
                                data-fields
                                base-fields)))
          (name-value (getf data :name))
          (field-key (if name-value
                       :name
                       (loop for key in remaining-fields
                         when (getf data key) do (return key))))
          (field-string (if field-key
                          (format nil "~(~a~)" field-key)
                          "NIL"))
          (field-value (when field-key (getf data field-key)))
          (mvl 32)
          (formatted-value (if field-value
                             (let* ((s1 (format nil "~a" field-value))
                                     (s2 (re:regex-replace-all
                                           "^-|-$"
                                           (re:regex-replace-all
                                             "[^-/a-zA-Z0-9]" s1 "-")
                                           "")))
                               (cond
                                 ((> (length s2) mvl)
                                   (format nil "~a" (subseq s2 0 mvl)))
                                 ((zerop (length s2))
                                   "NIL")
                                 (t s2)))
                             "NIL")))
    (format nil "~a:~a:~a:~a" type-string field-string formatted-value (u:uuid))))

(defun find-resource-name (type-key filters)
  (if (uuid-p filters)
    (id-to-resource-name filters)
    (let* ((id (be-id type-key filters "admin")))
      (when id (id-to-resource-name id)))))

(defun insert-main-query (type-key insert uuid data user)
  ":private: Returns a list consisting of and SQL string with placeholders
and parameters for those placeholders. INSERT is the value associated with
the :INSERT-SQL key of a type in *compiled-resources*. DATA is a plist
where the keys represent FIELDS and the values represent the values to be
inserted."
  (valid-insert insert)
  (when uuid (valid-uuid uuid))
  (valid-data type-key data)
  (let* ((main-qt (getf insert :main))
          (main-sql (car main-qt))
          (values (loop for k in (cdr main-qt)
                    for v = (getf data k
                              (u:tree-get *compiled-model* type-key :fields k :default))
                    unless (equal k :id)
                    collect (db-value type-key k user v))))
    (when uuid
      (push uuid values))
    (cons main-sql values)))

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
          (internal (getf type-def :internal))
          (base (getf type-def :base)))
    (and base (not internal) (not (equal type-key :resources)))))

(defun base-resource-type-keys ()
  ":private: Returns a list of type keys which are base types in the model,
excluding types that are marked as joiners and the :resources type."
  (loop for type-key in *compiled-model* by #'cddr
    when (base-resource-type-key-p type-key)
    collect type-key))

(defun full-data (type-key data user &key record local-only)
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
    for autofill = (getf field-def :autofill)
    for autofill-value = (case autofill
                           (:user user)
                           (otherwise nil))
    appending (list field-key
                ;; Order is important here!
                (or data-value record-value autofill-value default-value))))

(defun uuid-exists-p (uuid)
  (valid-uuid uuid)
  (when
    (loop for type-key in (cons :resources (base-resource-type-keys))
      for type = (table-name type-key)
      for sql = (format nil "select 1 from ~a where id = $1" type)
      for query = (list sql uuid)
      thereis (a:with-rbac (*rbac*) (a:rbac-query query :single)))
    t))

(defun get-type-roles (type-key)
  (pl:pdebug :in "get-type-roles" :type-key type-key
    :resource-name (type-resource-name type-key))
  (when (type-key-p type-key)
    (a:list-resource-role-names *rbac* (type-resource-name type-key))))

;; TODO: Is this dead?
(defun set-type-roles (type-key roles)
  (let* ((existing (get-type-roles type-key))
          (to-add (set-difference roles existing :test 'equal))
          (to-remove (set-difference existing roles :test 'equal))
          (resource (type-resource-name type-key)))
    (loop for role in to-remove
      do (a:remove-resource-role *rbac* resource role))
    (loop for role in to-add
      do (a:add-resource-role *rbac* resource role))))

(defun format-list-elements (list format-string)
  (mapcar
    (lambda (element) (format nil format-string element))
    list))

(defun show-roles-p (type-key form user)
  (let* ((m *compiled-model*)
          (fields (u:tree-get m type-key form :fields))
          (base (u:tree-get m type-key :base))
          (per-user (u:tree-get m type-key :per-user)))
    (when (and
            (not base)
            (not per-user)
            (or
              (equal fields t)
              (u:has (a:list-user-role-names *rbac* user) "admin")))
      t)))

(defun fe-fields (type-key user)
  (loop
    for form in *forms*
    when (u:tree-get *compiled-model* type-key form)
    append
    (list
      form
      (loop
        with base = (u:tree-get *compiled-model* type-key :base)
        with fields = (append
                        (u:tree-get *compiled-model* type-key :fields)
                        (when (show-roles-p type-key form user)
                          `(:roles (:ui (:label "Roles"
                                          :input-type "checkbox-list")))))
        and form-fields = (u:tree-get *compiled-model* type-key form :fields)
        for field-key in fields by #'cddr
        for field-def in (cdr fields) by #'cddr
        for field-type = (getf field-def :type)
        for default = (getf field-def :default)
        for path = (getf field-def :path :false)
        for non-base-roles-field = (and (not base) (equal field-key :roles))
        for ui = (getf field-def :ui)
        for table = (or (u:tree-get field-def :source :table) type-key)
        when (and
               (or
                 (equal form-fields t)
                 (member field-key form-fields)
                 non-base-roles-field)
               (or (getf ui :input-type) non-base-roles-field)
               (not (equal (getf ui :input-type) :hidden))
               (not (and
                      (equal form :list-form)
                      (equal field-type :file))))
        append (list field-key (add-to-plist (list
                                               :default default
                                               :path path
                                               :table table)
                                 ui))))))

(defun true-or-false (&rest path)
  (if (apply #'u:tree-get (cons *compiled-model* path)) :true :false))

(defun list-result (type-key user form &optional field-keys view-result)
  (add-to-plist
    (list
      :type-key type-key
      :create (true-or-false type-key :create)
      :update (true-or-false type-key :update)
      :delete (true-or-false type-key :delete)
      :records (add-roles-to-view
                 type-key
                 form
                 user
                 (view-result-values type-key field-keys view-result))
      :allowed-values (allowed-values type-key user))
    (fe-fields type-key user)))

;;
;; END Internal helper functions
;;

;;
;; BEGIN Filesystem functions
;;

(defun path-field (type-key)
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    when (getf field-def :path) do (return field-key)))

(defun file-field (type-key)
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    when (equal (getf field-def :type) :file)
    do (return field-key)))

(defun parent-type (type-key)
  (when (u:tree-get *compiled-model* type-key :tree)
    (or
      (u:tree-get *compiled-model* type-key :parent-type)
      type-key)))

(defun fs-path (type-key path)
  (u:join-paths *doc-root* (scoped-path (parent-type type-key) path)))

(defun store-directory (type-key logical-path user roles)
  (let ((fs-path (fs-path type-key logical-path)))
    (pl:pdebug :in "store-directory" :step 1
      :type-key type-key :logical-path logical-path :fs-path fs-path
      :user user :roles roles)
    (ensure-directories-exist fs-path)
    logical-path))

(defun delete-single-file (type-key logical-path &optional resource)
  (let* ((is-leaf (u:tree-get *compiled-model* type-key :is-leaf))
          (fs-path (fs-path type-key logical-path))
          (path-field (path-field type-key))
          (name (if resource
                  resource
                  (find-resource-name
                    type-key
                    `((,type-key ,path-field :eq ,logical-path))))))
    (pl:pdebug :in "delete-single-file" :step 1
      :is-leaf is-leaf :fs-path fs-path :path-field path-field
      :resource name :logical-path logical-path :type-key type-key)
    (a:remove-resource *rbac* name)
    (if is-leaf
      (progn
        (pl:pdebug :in "delete-single-file" :step 2
          :status "deleting file"
          :type-key type-key :logical-path logical-path :fs-path fs-path)
        (uiop:delete-file-if-exists fs-path))
      (progn
        (pl:pdebug :in "delete-single-file" :step 3
          :status "deleting directory"
          :type-key type-key :logical-path logical-path :fs-path fs-path)
        (uiop:delete-empty-directory fs-path)))))

(defun child-types (parent-type-key)
  (loop with m = *compiled-model*
    for type-key in m by #'cddr
    for type-def in (cdr m) by #'cddr
    for is-tree = (getf type-def :tree)
    for parent-type = (getf type-def :parent-type)
    when (and is-tree (equal parent-type parent-type-key))
    collect type-key))

(defun recursive-directory-listing (dir-type-key logical-path)
  (loop for type-key in (child-types dir-type-key)
    for path-field = (path-field type-key)
    for search = (format nil "~a%" logical-path)
    for paths = (getf
                  (be-list-column type-key path-field "admin"
                    :filters `((,type-key ,path-field :like ,search)))
                  :values)
    appending (mapcar
                (lambda (p) (cons type-key p))
                paths)
    into all-paths
    finally
    (let ((ordered-paths (sort
                           (remove-if (lambda (p) (equal p "/")) all-paths)
                           #'>
                           :key (lambda (c) (length (cdr c))))))
      (pl:pdebug :in "recursive-directory-listing" :paths ordered-paths)
      (return ordered-paths))))

(defun delete-directory-recursively (type-key logical-path)
  (pl:pdebug :in "delete-directory-recursively" :step 1
    :type-key type-key :logical-path logical-path)
  (loop
    with paths = (recursive-directory-listing type-key logical-path)
    and path-field = (path-field type-key)
    initially
    (pl:pdebug :in "delete-directory-recursively" :step 2
      :paths paths :path-field path-field)
    for (child-type-key . path) in paths
    for resource = (find-resource-name
                     child-type-key
                     `((,child-type-key ,path-field :eq ,path)))
    for log = (pl:pdebug :in "delete-directory-recursively" :step 3
                :child-type-key child-type-key :path path :resource resource)
    when (and path resource) do
    (delete-single-file child-type-key path resource)
    (pl:pdebug :in "delete-directory-recursively" :step 4
      :status "deleted file or directory"
      :path path)))

(defun delete-file-or-directory (type-key logical-path)
  (pl:pdebug :in "delete-file-or-directory"
    :type-key type-key :logical-path logical-path)
  (if (u:tree-get *compiled-model* type-key :is-leaf)
    (delete-single-file type-key logical-path)
    (delete-directory-recursively type-key logical-path)))

;;
;; END Filesystem functions
;;

;;
;; BEGIN Database helper functions
;;

(defun update-roles (type-key filters user roles)
  ":private: Updates the RBAC roles for the resource of type TYPE-KEY with ID.
ROLES is a list of role names. ID is a UUID string. This function returns the
resource name of the updated record if the update is successful, or an error if
the update fails."
  (valid-type-key type-key)
  (valid-roles roles)
  (if (uuid-p filters)
    (valid-existing-uuid filters)
    (valid-filters filters :required t))
  ;; The update-roles function should work for changing the roles associated
  ;; with a user or with a resource.
  (case type-key
    (:users
      (let* ((user-id (be-id :users filters user))
              (user-name (a:get-value *rbac* "users" "user_name" "id" user-id))
              (exclusive-roles (a:exclusive-role-for user-name))
              (roles (add-to-list roles "logged-in" exclusive-roles))
              (existing-roles (a:list-user-role-names *rbac* user-name))
              (to-add (set-difference roles existing-roles :test 'equal))
              (to-remove (set-difference existing-roles roles :test 'equal)))
        (loop for role in to-add
          do (a:add-user-role *rbac* user-name role))
        (loop for role in to-remove
          do (a:remove-user-role *rbac* user-name role))))
      (otherwise
        (let* ((exclusive-role (a:exclusive-role-for user))
                (roles (add-to-list roles "admin" exclusive-role))
                (resource-name (find-resource-name type-key filters))
                (existing-roles (a:list-resource-role-names *rbac* resource-name))
                (to-add (set-difference roles existing-roles :test 'equal))
                (to-remove (set-difference existing-roles roles :test 'equal)))
          (loop for role in to-add
            do (a:add-resource-role *rbac* resource-name role))
          (loop for role in to-remove
            do (a:remove-resource-role *rbac* resource-name role))
          resource-name))))

(defun update-resource-name (type-key record)
  ":internal: Updates the resource name for the given record. DATA must include
the ID of the record. This does nothing for base tables, because base tables
aren't resources, and the resources table is an internal table that represents
user resources."
  (valid-type-key type-key)
  (let ((resource-name (make-resource-name type-key record))
         (id (getf record :id)))
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

(defun resolve-reference-id (type-key field-key value)
  ":private: If the field has a :target (and no :join-table), treat VALUE as a
display name from the target's :source :column (default 'name'), look it up and
return the matching UUID. Signals validation error if no match or value is not
a string. For non-reference fields return VALUE unchanged."
  (let* ((m *compiled-model*)
          (field-def (u:tree-get m type-key :fields field-key))
          (target (getf field-def :target))
          (joiner (getf field-def :join-table)))
    (if (and target (not joiner))
      (cond
        ((uuid-p value) value)           ; already an ID (e.g. synthetic :id field with :target :resources)
        ((listp value) value)            ; lists are handled via join-table path
        ((not (stringp value))
          (report-ve "resolve-reference-id"
            "Reference field ~s ~s expects a string value, got ~s"
            ~type-key ~field-key ~value))
        (t
          (let* ((target-type target)
                  (col-key (or (u:tree-get field-def :source :column) :name))
                  (table (u:tree-get m target-type :table-name))
                  (val-col (u:tree-get m target-type :fields col-key :name-sql))
                  (sql (format nil "select id from ~a where ~a = $1" table val-col))
                  (query (list sql value))
                  (id (a:with-rbac (*rbac*) (a:rbac-query query :single))))
            (or id
              (report-ve "result-reference-id"
                "Unknown value for reference field ~s ~s: ~s"
                ~type-key ~field-key ~value)))))
      value)))

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
uses names instead of IDs. Returns NIL if ID is not a valid UUID, does not
exist, or points to a base or internal table."
  (when (and id (uuid-p id))
    (query "select resource_name from resources where id = $1"
      :params (list id)
      :result-type :single)))

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
    (if type-string
      (u:make-keyword type-string)
      (loop with base-type-keys = (remove-if-not
                                    (lambda (k)
                                      (u:tree-get *compiled-model* k :base))
                                    (u:plist-keys *compiled-model*))
        and sql-template = "select 1 from ~a where id = $1"
        for type-key in base-type-keys
        for table = (table-name type-key)
        for sql = (format nil sql-template table)
        for query = (list sql id)
        for exists = (a:with-rbac (*rbac*) (a:rbac-query query :single))
        when exists do (return type-key)))))

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
  (let ((type-key (id-to-type-key uuid)))
    (if (base-type-p type-key)
      (a:user-allowed *rbac* user permission
        (type-resource-name type-key))
      (let ((resource-name (id-to-resource-name uuid))
             (resource-type (id-to-type-key uuid)))
        (when resource-name
          (if (member resource-type (base-resource-type-keys))
            (equal user "admin")
            (a:user-allowed *rbac* user permission
              (id-to-resource-name uuid))))))))

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

(defun rec (id user &key (form :update-form) type-key (public t))
  ":private: Returns the TYPE-KEY record with the given ID, provided that it is
accessible to USER. Given the IDs are UUIDs (globally unique), TYPE-KEY is
optional. However, providing TYPE-KEY helps the function avoid an extra database
lookup. PUBLIC tells this function to accept only non-internal TYPE-KEYs."
  (valid-existing-user user)
  (when type-key (if public
                   (valid-be-type-key type-key)
                   (valid-type-key type-key)))
  (let ((type-key (if type-key type-key (id-to-type-key id))))
    (when (and (user-allowed-resource user id "read") (uuid-exists-p id))
      (pl:pdebug :in "rec" :filters (list type-key :id :eq id))
      (let* ((m *compiled-model*)
              (type-key (if type-key type-key (id-to-type-key id)))
              (sql (u:tree-get m type-key :views :main :sql))
              (where (add-where-clause sql (list (list type-key :id :eq id)) user))
              (view-result (view-result type-key where))
              (field-keys (form-field-keys type-key form)))
        (list
          :type type-key
          :fields (fe-fields type-key user)
          :record (car
                    (view-result-values type-key field-keys view-result))
          :allowed-values (allowed-values type-key user))))))

(defun insert-join-table-rows (type-key uuid data)
  (loop
    with m = *compiled-model*
    with uuid-key = (u:make-keyword
                      (to-sql-identifier type-key
                        :format-string "~a-id" :form :singular))
    with insert = (u:tree-get m type-key :insert-sql)
    with keys = (remove-if
                  (lambda (k) (member k '(:resource :main)))
                  (u:plist-keys insert))
    for key in keys
    for qt = (getf insert key)
    for sql = (car qt)
    for param-keys = (cdr qt)
    for names = (getf data key)
    for ref-type-key = (u:tree-get m type-key :fields key :source :table)
    for value-ids = (list-ids ref-type-key :name names)
    when value-ids do
    (loop with value-key = (u:make-keyword
                             (to-sql-identifier key
                               :format-string "~a-id" :form :singular))
      for value-id in value-ids
      for params = (loop for key in param-keys
                     collect (cond
                               ((eq key uuid-key) uuid)
                               ((eq key value-key) value-id)
                               (t (error "Unknown param key ~s"
                                    key))))
      for query = (cons sql params)
      do
      (pl:pdebug :in "insert-join-table-rows"
        :status "executing sql"
        :sql sql
        :params params
        :desired-param-keys (format-list-elements param-keys "~(~s~)")
        :uuid-key uuid-key
        :value-key value-key)
      (a:with-rbac (*rbac*) (a:rbac-query query)))))

(defun delete-by-id (type-key uuid)
  (let* ((delete (u:tree-get *compiled-model* type-key :delete-sql))
          (sql (car delete)))
    (unless (equal (list :id) (cdr delete))
      (let ((param-keys (format-list-elements (cdr delete) "~(~s~)")))
        (report-ve "delete-by-id"
          "delete-by-id works on some base types only"
          type-key uuid sql param-keys)))
    (a:with-rbac (*rbac*)
      (a:rbac-query (list sql uuid)))))

(defun insert-normal (type-key data roles user)
  ;; Insert resources row
  (let* ((resource-name (make-resource-name type-key data))
          (uuid (a:add-resource *rbac* resource-name :roles roles))
          (insert (u:tree-get *compiled-model* type-key :insert-sql))
          (all-roles (cons (a:exclusive-role-for user) roles)))
    (pl:pdebug :in "insert-normal"
      :type-key type-key :uuid uuid :data data
      :sql (car (insert-main-query type-key insert uuid data user))
      :params (cdr (insert-main-query type-key insert uuid data user)))
    ;; Insert TYPE-KEY row
    (a:with-rbac (*rbac*)
      (a:rbac-query (insert-main-query type-key insert uuid data user)))
    ;; Insert rows in join tables
    (insert-join-table-rows type-key uuid data)
    ;; Update roles
    (update-roles type-key uuid user all-roles)
    (values uuid t)))

(defun insert-base (type-key data user)
  (pl:pdebug :in "insert-base")
  (let ((insert (u:tree-get *compiled-model* type-key :insert-sql)))
    (pl:pdebug :in "insert-base"
      :type-key type-key :data data
      :sql (car (insert-main-query type-key insert nil data user))
      :params (cdr (insert-main-query type-key insert nil data user)))
    ;; Insert TYPE-KEY row
    (let ((uuid (a:with-rbac (*rbac*)
                  (a:rbac-query
                    (insert-main-query type-key insert nil data user)
                    :single))))
      ;; Insert rows in join tables
      (insert-join-table-rows type-key uuid data)
      (values uuid t))))

(defun allowed-values-for-field (type-key field-key user)
  (let* ((m *compiled-model*)
          (source (u:tree-get m type-key :fields field-key :source))
          (x-type-key (getf source :table))
          (x-field-key (getf source :column)))
    (getf (be-list-column x-type-key x-field-key user) :values)))

(defun allowed-values (type-key user)
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    and base = (u:tree-get *compiled-model* type-key :base)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for join-table = (getf field-def :join-table)
    for target = (getf field-def :target)
    when (and (or join-table target) (not (equal field-key :id)))
    appending
    (list field-key (allowed-values-for-field type-key field-key user))
    into allowed
    finally
    (return
      (if base
        allowed
        (append
          allowed
          (list :roles
            (remove-if
              (lambda (r)
                (member r '("admin" "admin:exclusive" "guest:exclusive")
                  :test 'equal))
              (a:list-role-names *rbac*))))))))

(defun add-roles-to-view (type-key form user view)
  (when view
    (let* ((m *compiled-model*)
            (fields (u:tree-get m type-key form :fields))
            (base (u:tree-get m type-key :base)))
      (pl:pdebug :in "add-roles-to-view"
        :step 1
        :type-key type-key
        :form form
        :fields fields
        :abse base)
      (if (show-roles-p type-key form user)
        (loop
          for record in view
          for id = (getf record :id)
          for resource-name = (id-to-resource-name id)
          for log-1 = (pl:pdebug :in "add-roles-to-view"
                        :step 2
                        :type-key type-key
                        :resource-name resource-name)
          for roles = (remove-if
                        (lambda (x) (equal x "admin"))
                        (a:list-resource-role-names *rbac* resource-name))
          collect (add-to-plist record (list :roles roles)))
        view))))

(defun remove-existing-non-user-roles (user roles)
  (let* ((existing-roles (a:list-role-names *rbac*))
          (user-roles (a:list-user-role-names *rbac* user)))
    (loop for role in roles
      when (or (u:has user-roles role)
             (not (u:has existing-roles role)))
      collect role)))

(defun filters-for-be-list (type-key ids user-filters user)
  (let* ((m *compiled-model*)
          (user-scope (equal (u:tree-get m type-key :views :main :scope) :user))
          (user-id (when user-scope (a:get-id *rbac* "users" user)))
          (scope-filter (when user-scope `(:users :id :eq ,user-id)))
          (ids-filter `(,type-key :id :in ,ids)))
    (append
      user-filters
      (list ids-filter)
      (when scope-filter (list scope-filter)))))

;;
;; END Internal database helper functions
;;

;;
;; BEGIN Error checking functions
;;

(defun valid-compiled-model ()
  (unless
    (and
      *compiled-model*
      (u:plistp *compiled-model*)
      (loop with type-keys = '(:users :resources :permissions :roles :settings)
        for type-key in type-keys
        always (u:plistp (getf *compiled-model* type-key))))
    (report-ve "valid-compiled-model" "No model has been compiled yet.")))

(defun valid-value-type (type-key field-key value)
  ":private: Returns T if VALUE is of the correct type for FIELD-KEY in TYPE-KEY
in the model. Otherwise, returns NIL."
  (unless (value-type-p type-key field-key value)
    (let ((field-type (u:tree-get *compiled-model*
                        type-key :fields field-key :type)))
      (report-ve
        "valid-value-type"
        "Invalid value type for field ~s ~s. Expected type ~s, but got value ~s."
        ~type-key ~field-key ~field-type ~value))))

(defun valid-filter (filter &key required)
  ":private: Returns NIL if FILTER is a list of the form (TYPE-KEY FIELD-KEY
OPERATOR-KEY VALUE), with TYPE-KEY being a valid type key in the model,
FIELD-KEY being a valid field key for the type, OPERATOR-KEY being a valid
operator key, and VALUE being of the correct type for the field. Otherwise,
raises a VALIDATION-ERROR condition with a message describing the problem with
FILTER."
  (when (and required (not filter))
    (report-ve "valid-filter" "Filter cannot be NIL."))
  (unless (listp filter)
    (report-ve "valid-filter" "Filter ~s is not a list." ~filter))
  (unless (= (length filter) 4)
    (report-ve "valid-filter" "Filter ~s does not have 4 elements." ~filter))
  (destructuring-bind (type-key field-key op-key value) filter
    (unless (type-key-p type-key)
      (report-ve "valid-filter" "Invalid type key ~s." ~type-key))
    (unless (field-key-p type-key field-key)
      (report-ve "valid-filter" "Invalid field key ~s for type ~s."
        ~field-key ~type-key))
    (unless (operator-key-p op-key)
      (report-ve "valid-filter" "Invalid operator key ~s." ~op-key))
    (unless (field-type-key-p
              (u:tree-get *compiled-model* type-key :fields field-key :type))
      (report-ve "valid-filter" "Invalid field type key for field ~s ~s ~s."
        ~type-key ~field-key ~type-key))
    (unless (value-type-p type-key field-key value)
      (let* ((field-type (u:tree-get *compiled-model*
                           type-key :fields field-key :type))
              (err (eformat "
Invalid value type for field ~s ~s.
Expected a value of type ~s, but got value ~s."
                   type-key field-key field-type value)))
        (report-ve "valid-filter" err)))))

(defun valid-filters (filters &key required)
  ":private: Checks that FILTERS is valid."
  (when (and required (null filters))
    (report-ve "valid-filters" "FILTERS cannot be NIL."))
  (loop for filter in filters
    do (valid-filter filter :required t)))

(defun valid-type-key (type-key)
  ":private: Checks that TYPE-KEY is a valid type key in the model."
  (unless (type-key-p type-key)
    (report-ve "valid-type-key" "Invalid type key ~a" ~type-key)))

(defun valid-non-base-type-key (type-key)
  ":private: Checks that TYPE-KEY is a valid non-base type key in the model,
meaning that the type lacks `:base t`."
  (unless (and
            (type-key-p type-key)
            (not (u:tree-get *compiled-model* type-key :base)))
    (report-ve "valid-non-base-type-key"
      "Invalid non-base type key ~a" ~type-key)))

(defun valid-be-type-key (type-key)
  (unless (and
            (type-key-p type-key)
            (not (u:tree-get *compiled-model* type-key :internal)))
    (report-ve "valid-be-type-key" "Invalid backend type key ~a" ~type-key)))

(defun valid-field-key (type-key field-key)
  ":private: Checks that FIELD-KEY is a valid field key for TYPE-KEY in the
model."
  (unless (field-key-p type-key field-key)
    (report-ve "valid-field-key"
      "Invalid field key ~a ~a." ~type-key ~field-key)))

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
      ((null field-def)
        (report-ve "valid-data"
          "Unknown field key ~s for type ~s." ~field-key ~type-key))
      ((getf field-def :join-table)
        (valid-values-list value))
      ((not (getf field-def :base))
        (valid-value-type type-key field-key value))
      ((equal field-key :id)
        (valid-uuid value))
      (t (report-ve "valid-data"
           "Invalid field key ~s for type ~s." ~field-key ~type-key)))
    (let ((target (getf field-def :target))
          (joiner (getf field-def :join-table))
          (is-id-or-pk (or
                         (equal field-key :id)
                         (getf field-def :primary-key))))
      (when (and (not target) (not joiner) (not is-id-or-pk) value)
        ;; This will signal a validation error if an ID is not found for value
        (resolve-reference-id type-key field-key value)))))

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
    for l = (length unknown-values)
    when unknown-values
    do (report-ve "valid-existing-join-data"
         "Invalid value~p for field ~s ~s: ~s"
         ~l ~type-key ~key ~unknown-values)))

(defun valid-uuid (uuid)
  (unless (uuid-p uuid)
    (report-ve "valid-uuid" "Invalid UUID ~s" ~uuid)))

(defun valid-existing-uuid (uuid)
  (unless (uuid-exists-p uuid)
    (report-ve "valid-existing-uuid" "UUID ~a not found." ~uuid)))

(defun valid-insert (insert)
  (unless (and (u:plistp insert) (getf insert :main))
    (report-ve "valid-insert" "Invalid insert ~s" ~insert)))

(defun valid-roles (&rest roles)
  (loop for role in (u:flatten roles)
    unless (be-id :roles `((:roles :name :eq ,role)) "admin")
    do (report-ve "valid-roles" "Invalid role name ~s." ~role)))

(defun valid-existing-roles (roles)
  (valid-roles roles)
  (loop with existing-roles = (a:list-role-names *rbac*)
    for role in roles
    when (not (member role existing-roles :test 'equal))
    do (report-ve "valid-existing-roles" "Role ~s does not exist." ~role)))

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
      (report-ve "valid-user-roles"
        "User ~s does not have the following roles: ~{~s~^, ~}."
        ~user ~non-user-roles))))

(defun valid-existing-user (user)
  (unless (a:get-id *rbac* "users" user)
    (report-ve "valid-existing-user" "User ~s does not exist." ~user)))

(defun valid-user-permissions (user type-key &rest permissions)
  (valid-existing-user user)
  (loop with resource-name = (type-resource-name type-key)
    for permission in permissions
    unless (a:user-allowed *rbac* user permission resource-name)
    do (report-ve "valid-existing-user-permissions"
         "User ~a does not have ~a permission for resource of type ~s."
         ~user ~permission ~type-key)))

(defun valid-values-list (values)
  (unless (listp values)
    (report-ve "valid-values-list"
      "Expected a list of values, but got ~s." ~values))
  (unless (every #'atom values)
    (report-ve "valid-values-list"
      "Expected a list of atomic values, but got ~s." ~values)))

(defun valid-file-token (file-token)
  (when file-token
    (let ((path (u:safe-decode file-token)))
      (unless (probe-file path)
        (report-ve "valid-file-token"
          "Referenced file does not exist."
          file-token
          path)))))

(defun valid-file-meta (type-key logical-path user roles)
  (pl:pdebug :in "valid-file-meta" :step 1
    :type-key type-key
    :logical-path logical-path
    :user user
    :roles roles)
  (let* ((logical-parent (u:path-parent logical-path))
          (name-field (path-field type-key))
          (resource (find-resource-name
                      type-key
                      `((,type-key ,name-field :eq ,logical-path))))
          (parent-type-key (parent-type type-key))
          (parent-name-field (path-field parent-type-key))
          (parent-resource (find-resource-name
                             parent-type-key
                             `((,parent-type-key
                                 ,parent-name-field :eq ,logical-parent))))
          (parent-roles (when parent-resource
                          (a:list-resource-role-names *rbac* parent-resource)))
          (req-roles (if roles roles parent-roles))
          (user-roles (a:list-user-role-names *rbac* user))
          (non-parent-roles (set-difference req-roles parent-roles
                              :test #'equal))
          (non-user-roles (set-difference
                            req-roles
                            (append '("admin" "admin:exclusive") user-roles)
                            :test #'equal)))
    ;; Ensure the file or directory doesn't already exist
    (when resource
      (report-ve "check-file" "File already exists: ~a."
        ~logical-path resource))
    ;; Ensure the parent directory already exists
    (unless parent-resource
      (report-ve "check-file" "Parent directory doesn't exist: ~a"
        ~logical-parent parent-resource))
    ;; Check user access to parent directory
    (unless (a:user-allowed *rbac* user "create" parent-resource)
      (report-ve "check-file"
        "User ~a does not have the create permission on directory ~a."
        ~user ~logical-parent))
    ;; Check the roles the user is assigning to this file
    (when non-parent-roles
      (report-ve "check-file" "Can't specify roles that parent doesn't have."
        logical-parent parent-roles non-parent-roles req-roles user-roles))
    (when non-user-roles
      (report-ve "check-file"
        "
Can't specify roles that user doesn't have.
  - User: ~a
  - User roles: ~{~a~^, ~}
  - Parent roles: ~{~a~^, ~}
  - Requested roles: ~{~a~^, ~}"
        ~user ~user-roles ~req-roles ~parent-roles non-user-roles))))

(defun valid-new-directory (type-key logical-path file-token user roles)
  (when (and logical-path (not file-token))
    (let* ((logical-parent (u:path-parent logical-path))
            (fs-path (fs-path type-key logical-path))
            (fs-parent-path (fs-path type-key logical-parent))
            (name-field (path-field type-key)))
      (pl:pdebug :in "validate-new-directory" :step 2
        :logical-parent logical-parent
        :fs-path fs-path
        :fs-parent-path fs-parent-path
        :name-field name-field)
      (let* ((resource (find-resource-name
                         type-key
                         `((,type-key ,name-field :eq ,logical-path))))
              (parent-resource (find-resource-name
                                 type-key
                                 `((,type-key ,name-field :eq ,logical-parent)))))
        (pl:pdebug :in "validate-new-directory" :step 3
          :resource resource
          :parent-resource parent-resource)
        (let* ((parent-roles (when parent-resource
                               (a:list-resource-role-names
                                 *rbac* parent-resource)))
                (user-roles (if (equal user "admin")
                              (a:list-role-names *rbac*)
                              (a:list-user-role-names *rbac* user)))
                (req-roles (if roles roles parent-roles))
                (non-parent-roles (set-difference req-roles parent-roles
                                    :test #'equal))
                (non-user-roles (set-difference req-roles user-roles
                                  :test #'equal)))
          (pl:pdebug :in "validate-new-directory" :step 2)
          (when resource
            (report-ve "validate-new-directory" "Directory already exists."
              logical-path fs-path resource))
          (unless parent-resource
            (report-ve "validate-new-directory" "Parent directory doesn't exist: ~a"
              logical-path fs-path ~logical-parent fs-parent-path))
          (unless (a:user-allowed *rbac* user "create" parent-resource)
            (report-ve "validate-new-directory"
              "User does not have access to parent directory."
              user logical-path fs-path logical-parent fs-parent-path
              parent-resource))
          (when non-parent-roles
            (report-ve "validate-new-directory"
              "
Can't assign roles that parent doesn't have.
  - Parent directory: ~a
  - Parent roles: ~{~a^~, ~}
  - Requested roles: ~{~a~^, ~}"
              logical-path ~logical-parent non-parent-roles ~parent-roles
              ~req-roles user-roles))
          (when non-user-roles
            (report-ve "validate-new-directory"
              "
Can't assign roles that user doesn't have.
  - User: ~a
  - User roles: ~{~a~^, ~}
  - Requested roles: ~{~a~^, ~}"
              ~user ~user-roles non-user-roles ~req-roles parent-roles)))))))

(defun valid-existing-file-or-directory (type-key logical-path)
  (when logical-path
    (let ((path-field (path-field type-key))
           (is-leaf (u:tree-get *compiled-model* type-key :is-leaf)))
      (when (and is-leaf (u:ends-with logical-path "/"))
        (report-ve "valid-existing-file-or-directory"
          "~(~s~) marked as leaf, but ~(~s~) value looks like a directory: ~a"
          ~type-key ~path-field ~logical-path is-leaf))
      (when (and (not is-leaf) (not (u:ends-with logical-path "/")))
        (report-ve "valid-existing-file-or-directory"
          "~(~s~) marked as not a leaf, but ~(~s~) value looks like a file: ~a"
          ~type-key ~path-field ~logical-path is-leaf))
      (let* ((thing (if is-leaf "file" "directory"))
              (fs-path (fs-path type-key logical-path))
              (probed (probe-file fs-path)))
        (unless probed
          (report-ve "valid-existing-file-or-directory"
            "~(~s~) ~a does not exist in file system: ~a" ~type-key ~thing
            ~logical-path is-leaf fs-path))
        (let* ((path-field (path-field type-key))
                (resource (find-resource-name
                            type-key
                            `((,type-key ,path-field :eq ,logical-path)))))
          (unless resource
            (report-ve "valid-existing-file-or-directory"
              "~(~s~) ~a does not have a resource record: ~a" ~type-key ~thing
              ~logical-path is-leaf fs-path path-field)))))))

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
  (valid-compiled-model)
  (valid-be-type-key type-key)
  (valid-existing-user user)
  (if (stringp filters)
    (valid-uuid filters)
    (valid-filters filters :required t))
  (pl:pdebug :in "be-id" :filters filters)
  (let* ((m *compiled-model*)
          (internal (u:tree-get m type-key :internal))
          (is-base (u:tree-get m type-key :base))
          (is-admin (member "admin"
                      (a:list-user-role-names *rbac* user)
                      :test 'equal))
          (sql (u:tree-get m type-key :views :main :sql))
          (efilters (if (uuid-p filters)
                      `((,type-key :id :eq ,filters))
                      filters))
          (where (add-where-clause sql efilters user))
          (view-result (view-result type-key where))
          (field-keys '(:id))
          (result (view-result-values type-key field-keys view-result)))
    (cond
      ((zerop (length result)) nil)
      ((> (length result) 1) (error "More than one match."))
      (t (let ((id (getf (car result) :id)))
           (if (and is-base (not internal))
             (when is-admin id)
             (when (user-allowed-resource user id "read") id)))))))

(defun be-value-id (type-key field-key value user)
  ":public: Returns the ID of the record of type TYPE-KEY where FIELD-KEY has
the value VALUE."
  (valid-compiled-model)
  (valid-be-type-key type-key)
  (valid-field-key type-key field-key)
  (valid-value-type type-key field-key value)
  (valid-existing-user user)
  (let* ((table (u:tree-get *compiled-model*
                 type-key :fields field-key :source :table))
          (target-type-key (if table
                             (case table
                               (:main type-key)
                               (otherwise table))
                             type-key))
          (target-field-key (if (equal target-type-key type-key)
                              field-key
                              (u:tree-get *compiled-model*
                                type-key :fields field-key
                                :source :column))))
    (be-id type-key `((,target-type-key ,target-field-key :eq ,value)) user)))

(defun be-rec (id user &key (form :update-form) type-key)
  ":public: Returns the TYPE-KEY record with the given ID, provided that it is
accessible to USER. Given the IDs are UUIDs (globally unique), TYPE-KEY is
optional. However, providing TYPE-KEY helps the function avoid an extra database
lookup."
  (valid-compiled-model)
  (rec id user :form form :type-key type-key :public t))

(defun be-val (id field-key user &key (form :update-form) type-key)
  ":public: Returns the value for field FIELD-KEY in the TYPE-KEY record with
ID. Providing TYPE-KEY is optional, but helps the function avoid an extra
database query."
  (valid-compiled-model)
  (valid-existing-uuid id)
  (valid-existing-user user)
  (let ((type-key (or type-key (id-to-type-key id))))
    (valid-be-type-key type-key)
    (valid-field-key type-key field-key)
    (user-allowed-resource user id "read")
    (let ((record (rec id user :form form :type-key type-key)))
      (u:tree-get record :record field-key))))

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
  (valid-compiled-model)
  (valid-be-type-key type-key)
  (valid-user-permissions user type-key "read")
  (if (uuid-p filters)
    (valid-existing-uuid filters)
    (valid-filters filters))
  (let* ((m *compiled-model*)
          (user-roles (a:list-user-role-names *rbac* user))
          (type-roles (u:tree-get m type-key :type-roles))
          (ids (if (base-resource-type-key-p type-key)
                 (when (u:has-some user-roles type-roles)
                   (let ((table (table-name type-key)))
                     (a:with-rbac (*rbac*)
                       (db:query (format nil "select id from ~a" table)
                         :column))))
                 (user-read-type-ids user type-key))))
    (pl:pdebug :in "be-list" :filters filters)
    (if ids
      (let* ((all-filters (filters-for-be-list type-key ids filters user))
              (sql (u:tree-get m type-key :views :main :sql))
              (where (add-where-clause sql all-filters user))
              (view-result (view-result type-key where))
              (field-keys (form-field-keys type-key form)))
        (if (filters-require-join-p type-key filters)
          (let ((ids (view-result-ids type-key field-keys view-result)))
            (when ids
              (let* ((ids-query (add-ids-clause type-key sql ids))
                      (view-result (view-result type-key ids-query)))
                (list-result type-key user form field-keys view-result))))
          (list-result type-key user form field-keys view-result)))
      (list-result type-key user form))))

;; TODO: Add pagination support
(defun be-list-column (type-key field-key user &key (form :list-form) filters)
  ":public: Returns a list of the values in FIELD-KEY for the records of type
TYPE-KEY that match FILTERS and that USER has 'read' permissions for. This is
like BE-LIST, but it returns a list of values instead of a list of records."
  (valid-compiled-model)
  (valid-type-key type-key)
  (valid-field-key type-key field-key)
  (valid-existing-user user)
  (if (uuid-p filters)
    (valid-existing-uuid filters)
    (valid-filters filters))
  (let ((records (getf
                   (be-list type-key user :form form :filters filters)
                   :records)))
    (add-to-plist
      (list
        :type type-key
        :values (mapcar (lambda (r) (getf r field-key)) records))
      (list
        form
        (list
          field-key
          (u:tree-get (fe-fields type-key user) form field-key))))))

;; TODO: Transaction!
(defun be-insert (type-key data user &key roles file-token)
  ":public: Inserts a record of type TYPE-KEY with the given DATA, provided
USER has `create` permissions in TYPE-KEY. DATA is a plist where the keys are
field keys and the values are the values to be inserted. ROLES is a list of
roles that you want to associate with the new resource. FILE-TOKEN is a token
that the /api/upload endpoint returns when you upload a file. It represents the
temporary uploaded file.

This function returns two values: The ID of the record and T if the record this
function inserted the record or NIL if the error already exists.  This function
does not perform an update if a record with the same unique fields already
exists. The following example inserts a todo with the name \"clean the
kitchen\":

    `(be-insert :todos
                '(:name \"clean the kitchen\"
                  :tags (\"chores\" \"kitchen\")
                \"admin\")`
"
  (valid-compiled-model)
  (valid-type-key type-key)
  (valid-user-roles user roles)
  (valid-user-permissions user type-key "create")
  (valid-file-token file-token)
  (let ((data (full-data type-key data user)))
    (valid-data type-key data)
    (let ((id (id-from-data type-key data)))
      (if id
        (values id nil)
        (progn
          (pl:pdebug :in "be-insert" :step 1
            :type-key type-key :status "validated")
          (let* ((m *compiled-model*)
                  (f (u:tree-get m type-key :create))
                  (base (u:tree-get m type-key :base))
                  (internal (u:tree-get m type-key :internal))
                  (path-field (path-field type-key))
                  (logical-path (when path-field (getf data path-field)))
                  (pre-create (u:tree-get m type-key :pre-create))
                  (post-create (u:tree-get m type-key :post-create)))
            (valid-new-directory type-key logical-path file-token user roles)
            (when pre-create
              (funcall pre-create type-key data roles user))
            (when (and logical-path (not file-token))
              (store-directory type-key logical-path user roles))
            (let ((new-id (cond
                            ((and (equal f :auto) (not base) (not internal))
                              (insert-normal type-key data roles user))
                            ((and (equal f :auto) (not internal))
                              (insert-base type-key data user))
                            ((functionp f)
                              (funcall f type-key data user :roles roles))
                            (t (report-ve "be-insert"
                                 "Invalid create function for type ~s: ~s"
                                 ~type-key ~f)))))
              (when post-create
                (funcall post-create nil nil data nil nil))
              (values new-id t))))))))

(defun be-insert-internal (type-key data user &key roles)
  ":private: Inserts a record of type TYPE-KEY with the given DATA, provided
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

This function differs from be-insert in that be-insert requires the :create
key to have a valid function or :auto, where as this internal version does
not.
"
  (valid-compiled-model)
  (valid-type-key type-key)
  (pl:pdebug :in "be-insert-internal"
    :type-key type-key :data data :roles roles)
  (valid-user-roles user roles)
  (valid-user-permissions user type-key "create")
  (let ((data (full-data type-key data user)))
    (valid-data type-key data)
    (let ((id (id-from-data type-key data)))
      (if id
        (values id nil)
        (let* ((m *compiled-model*)
                (f (u:tree-get m type-key :create))
                (base (u:tree-get m type-key :base))
                (internal (u:tree-get m type-key :internal))
                (post-create (u:tree-get m type-key :post-create))
                (new-id (cond
                          ((and
                             (or (null f) (equal f :auto))
                             (not base) (not internal))
                            (insert-normal type-key data roles user))
                          ((and
                             (or (null f) (equal f :auto))
                             (not internal))
                            (insert-base type-key data user))
                          ((functionp f)
                            (funcall f type-key data user :roles roles))
                          (t (report-ve "be-insert-internal"
                               "Invalid create function for type ~s: ~s"
                               ~type-key ~f)))))
          (when post-create
            (funcall post-create nil nil data nil nil))
          (values new-id t))))))

;; TODO: Remove. This is for debugging only.
(defparameter *be-update-call* nil)

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
  (setf *be-update-call*
    (lambda () (be-update type-key filters data user :roles roles)))
  (valid-compiled-model)
  (valid-type-key type-key)
  (if (stringp filters)
    (valid-uuid filters)
    (valid-filters filters :required t))
  (valid-data type-key data)
  (valid-user-roles user (remove-existing-non-user-roles user roles))
  (let* ((m *compiled-model*)
          (uuid (id-from-filters-and-data type-key filters data))
          (record (getf (rec uuid user :type-key type-key) :record))
          (sql (car (u:tree-get m type-key :update-sql :main)))
          (values (local-values-for-update type-key data record user :id uuid))
          (update-query (cons sql values))
          (full-data (full-data type-key data user :record record)))
    (valid-existing-join-data type-key full-data)
    ;; Update TYPE-KEY row (main update)
    (when (equal type-key :resources)
      (report-ve "be-update" "Can't update internal type :resources"))
    (multiple-value-bind (result update-row-count)
      (a:with-rbac (*rbac*) (a:rbac-query update-query))
      (declare (ignore result))
      (unless (= update-row-count 1)
        (error "Error updating main ~a row (update row count ~d)"
          type-key update-row-count))
      update-row-count)
    ;; Update resource name (update resources record)
    (unless (base-type-p type-key)
      (update-resource-name type-key full-data))
    ;; Update join tables
    (update-join-tables type-key uuid full-data record)
    ;; Update roles
    (when roles (update-roles type-key filters user roles))
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
  (valid-compiled-model)
  (valid-type-key type-key)
  (if (stringp filters)
    (valid-uuid filters)
    (valid-filters filters :required t))
  (valid-existing-user user)
  (let* ((m *compiled-model*)
          (uuid (be-id type-key filters "admin"))
          (record (when uuid
                    (getf (rec uuid "admin" :type-key type-key) :record)))
          (path-field (path-field type-key))
          (logical-path (when path-field (getf record path-field)))
          (f (u:tree-get m type-key :delete))
          (pre-delete (u:tree-get m type-key :pre-delete))
          (post-delete (u:tree-get m type-key :post-delete)))
    (pl:pdebug :in "be-delete" :step 1 :type-key type-key
      :uuid uuid
      :record record
      :path-field path-field
      :logical-path logical-path)
    (valid-existing-file-or-directory type-key logical-path)
    (when (and uuid (user-allowed-resource user uuid "delete"))
      (when pre-delete
        (funcall pre-delete type-key uuid record user))
      (cond
        ((equal (type-of f) 'keyword)
          (case f
            (:auto
              (if logical-path
                (delete-file-or-directory type-key logical-path)
                (let ((resource-name (id-to-resource-name uuid)))
                  (when resource-name
                    (a:remove-resource *rbac* resource-name)))))
            (otherwise
              (report-e "be-delete"
                "Can't delete ~s (~a) with :delete function ~s"
                type-key uuid f))))
        ((functionp f)
          (funcall f type-key record user))
        (t (report-ve "be-delete"
             "Invalid delete function for type ~s: ~s" ~type-key ~f)))
      (when post-delete
        (pl:pdebug :in "be-delete" :step 2)
        (funcall post-delete type-key uuid record user)
        (pl:pdebug :in "be-delete" :step 3 :uuid uuid))
      uuid)))

(defun be-add-type-roles (type-key user &rest roles)
  ":public: Adds ROLE to the list of roles associated with TYPE-KEY. This
function returns T if ROLES are successfully added or determined to exist, and a
validation-error if TYPE-KEY doesn't exist or ROLES includes a ROLE that doesn't
exist or that is not available to USER."
  (valid-compiled-model)
  (valid-type-key type-key)
  (valid-user-roles user roles)
  (let ((resource-name (type-resource-name type-key)))
    (loop
      initially (pl:pdebug :in "be-add-type-roles" :step 1
                  :type-key type-key
                  :resource-name resource-name)
      with resource-roles = (a:list-resource-role-names *rbac* resource-name)
      for role in (u:distinct-values roles)
      unless (member role resource-roles :test 'equal)
      do
      (pl:pdebug :in "be-add-type-roles" :step 2
        :type-key type-key :resource-name resource-name :role role)
      (a:add-resource-role *rbac* resource-name role))
    (pl:pdebug :in "be-add-type-roles" :step 3
      :type-key type-key :resource-name resource-name)
    (a:list-resource-role-names *rbac* resource-name)))

(defun be-remove-type-roles (type-key user &rest roles)
  ":public: Removes ROLE from the list of roles associated with TYPE-KEY. This
function returns T if ROLES are successfully removed or determined not to be
associated with TYPE-KEY. The function raises a VALIDATION-ERROR if TYPE-KEY
doesn't exist or ROLES includes a ROLE that doesn't exist or that is not
available to USER. The admin role cannot be removed from a type, this function
ignores that role if it's included in ROLES."
  (valid-compiled-model)
  (valid-type-key type-key)
  (valid-user-roles user roles)
  (let ((resource-name (type-resource-name type-key)))
    (loop with user-roles = (a:list-user-role-names *rbac* user)
      for role in (U:distinct-values roles)
      when (member role user-roles :test 'equal)
      do (a:remove-resource-role *rbac* resource-name role))
    (pl:pdebug :in "be-remove-type-roles"
      :type-key type-key :resource-name resource-name)
    (a:list-resource-role-names *rbac* resource-name)))

(defun trim-maybe (value)
  ":private: If VALUE is a string, this function returns the trimmed value. If
VALUE is not a string, this function returns VALUE. Otherwise, if VALUE is an
array this function returns the same array with any string elements in the
array trimmed."
  (pl:pdebug :in "trim-maybe" :step 1 :value value)
  (let ((result (if (listp value)
                  (progn
                    (pl:pdebug :in "trim-maybe" :step 2 :value value)
                    (mapcar
                      (lambda (s) (if (string s) (u:trim s) s))
                      value))
                  (if (stringp value)
                    (progn
                      (pl:pdebug :in "trim-maybe" :step 3 :value value)
                      (u:trim value))
                    value))))
    (pl:pdebug :in "trim-maybe" :step 4 :result result)
    result))

(defun be-validate-field (type-key field-key value user)
  ":public: Validates that VALUE is acceptable for FIELD-KEY of TYPE-KEY. This
function returns

    (:valid t)

if VALUE is valid for the field, and

    (:valid nil :errors {error-message-list})

if VALUE is not valid for the field. `{error-message-list}` is a list of strings
describing the problems with VALUE."
  (valid-compiled-model)
  (pl:pdebug :in "be-validate-field" :step 1
    :type-key type-key :field-key field-key :value value :user user)
  (loop
    with validations = (u:tree-get *compiled-model*
                         type-key :fields field-key :validations)
    initially
    (valid-type-key type-key)
    (valid-field-key type-key field-key)
    (pl:pdebug :in "be-validate-field" :step 2
      :type-key type-key :field-key field-key :value value)
    for validation in validations
    for clean-value = (trim-maybe value)
    for log-1 = (pl:pdebug :in "be-validate-field" :step 3
                  :clean-value clean-value
                  :validation validation)
    for e = (funcall validation type-key field-key clean-value user)
    for log-2 = (pl:pdebug :in "be-validate-field" :step 4 :e e)
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
  (valid-compiled-model)
  (loop
    with fields = (u:tree-get *compiled-model* type-key :fields)
    with data = (loop for key in (user-fields type-key)
                  for default = (u:tree-get fields key :default)
                  for value = (getf values key default)
                  do (pl:pdebug :in "be-validate-form" :step 1
                       :key key :default default :value value)
                  append (list key value))
    for field-key in data by #'cddr
    for value in (cdr data) by #'cddr
    for log-1 = (pl:pdebug :in "be-validate-form"
                  :step 2
                  :type-key type-key
                  :field-key field-key :value value)
    for e = (be-validate-field type-key field-key value user)
    when (equal (getf e :valid) :false)
    append (getf e :errors) into errors
    finally
    (pl:pdebug :in "be-validate-form"
      :step 3
      :type-key type-key
      :valid (if errors :false :true)
      :errors errors)
    (return
      (if errors
        (list :valid :false :errors errors)
        (list :valid :true)))))

(defun be-types (user)
  (valid-compiled-model)
  (loop with m = *compiled-model*
    for type-key in m by #'cddr
    for type-def in (cdr m) by #'cddr
    for allowed = (a:user-allowed *rbac*
                    user "read" (type-resource-name type-key))
    for internal = (getf type-def :internal)
    for display = (getf type-def :display)
    when (and display allowed (not internal))
    collect type-key))

;;
;; END Public backend functions
;;
