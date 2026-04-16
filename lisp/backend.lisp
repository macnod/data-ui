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

;;
;; END Internal helper functions
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
          type-key field-key)
        (u:tree-get *compiled-model* type-key :fields
          field-key :type)
        value))))

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
      for e1 = (valid-field-key type-key field-key)
      for e2 = (valid-value-type type-key field-key value)
      when e1 collect e1 into errors
      when e2 collect e2 into errors
      finally (when errors
                (signal-validation-error
                  "Invalid data for type ~a. Errors: ~{~a~^ ~}."
                  type-key errors))
    (signal-validation-error "Invalid type key ~s." type-key))))

;;
;; END Error checking functions
;;

;;
;; BEGIN Public backend functions
;;

(defun be-id (type-key filters &key uuid-lookup)
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
  (valid-filters filters :required t)
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

(defun be-insert (type-key data &key roles)
  ":public: Inserts a record of type TYPE-KEY with the given DATA. DATA is a
plist where the keys are field keys and the values are the values to be
inserted. This function returns the ID of the newly inserted record, or a
list of errors if the insert fails. The following example inserts a user with
the name \"john\" and the role \"admin\":

    `(be-insert :users '(:name \"john\" :roles (\"admin\")))`
"
  (let* ((insert (u:tree-get *compiled-model* type-key :insert-sql))
          ;; RBAC resource insert
          ;;
          ;; We'll ignore the insert that the model provides and, instead, we'll
          ;; just use an RBAC method to do the insert. This is because the RBAC
          ;; system does some extra work when inserting resources, such as
          ;; including default roles for the resource, and we want to make sure
          ;; that work gets done. The method will return the UUID of the newly
          ;; inserted resource, which we can then use for the main resource
          ;; insert and the join table inserts.
          (resource-qt (getf insert :resource))
          (resource-name (format nil "~(~a~):~a"
                           type-key
                           (getf data (cadr resource-qt))))
          (uuid (a:add-resource *rbac* resource-name :roles roles))

          ;; Main resource insert
          (main-qt (getf insert :main))
          (main-sql (car main-qt))
          (main-query (cons main-sql
                        (cons uuid
                          (loop for k in (cdr main-qt)
                            unless (equal k :id)
                            collect (getf data k))))))
    (a:with-rbac (*rbac*)
      (a:rbac-query main-query :single))
    ;; Join table inserts
    (loop for key in (plist-exclude
                       (u:plist-keys insert) '(:resource :main))
      for qt = (getf insert key)
      for sql = (car qt)
      for names = (getf data key)
      for value-ids = (loop for name in names
                        collect (be-id key `((:name :eq ,name))))
      do (loop for value-id in value-ids
           for query = (cons sql (list uuid value-id))
           do (a:with-rbac (*rbac*) (a:rbac-query query))))
    uuid))

;; NOTE: Not yet working. Needs to be able to handle updates to join tables.
(defun be-update (type-key filters data)
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
  (valid-filters filters :required t)
  (valid-data type-key data)
  (let* ((id (if (stringp filters)
               filters
               (if (member :id (u:plist-keys data))
                 (getf data :id)
                 (be-id type-key filters))))
          (main-qt (u:tree-get *compiled-model* type-key :update-sql :main))
          (main-sql (car main-qt))
          (main-data (unless (member :id (u:plist-keys data))
                       (append (list :id id) data)))
          (main-query (cons main-sql
                        (loop for k in (cdr main-qt)
                          collect (getf main-data k)))))
    main-query))

;;
;; END Public backend functions
;;
