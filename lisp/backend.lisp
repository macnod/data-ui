(in-package :data-ui)

;;
;; BEGIN Internal helper functions
;;

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
    with distinct-ids = (u:distinct-values
                          (mapcar
                            (lambda (r) (getf r id-key))
                            view-result))
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

(defun operator-sql (operator-key)
  ":private:"
  (case operator-key
    (:eq "=")
    (:not-eq "!=")
    (:gt ">")
    (:lt "<")
    (:gte ">=")
    (:lte "<=")
    (:like "like")
    (:ilike "ilike")
    (otherwise (error "Unsupported operator ~a" operator-key))))

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
          (format nil "~a where~%  ~{~a~^~%  and ~}~%" sql conditions)
          (mapcar #'cadddr filters))))
    (list sql)))

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

;;
;; END Internal helper functions
;;

;;
;; BEGIN Public backend functions
;;

(defun be-id (type-key filters)
  ":public: Returns the ID of the resource of type TYPE-KEY that matches FILTERS. FILTERS
is a list of filters, where each filter contains a type key (not necessarily
TYPE-KEY), a field key, an operator, and a value. All filters must match in
order for a record to be selected. Matching more than one record is an error.
(Use bMatching no records returns NIL. This function returns a string representing
the ID of the matched record.

The following example assumes that there is only one user with the \"admin\"
role:
    `(be-id :users '((:roles :name :eq \"admin\")))`

Users have distinct user names, thus the following will never return a
\"More than one match\" error:
    `(be-id :users '((:users :name :eq \"guest\")))`
"
  (let* ((m *compiled-model*)
          (sql (u:tree-get m type-key :views :main :sql))
          (where (add-where-clause sql filters))
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

(defun be-list (type-key &key (form :list-form) filters)
  (let* ((m *compiled-model*)
          (sql (u:tree-get m type-key :views :main :sql))
          (where (add-where-clause sql filters))
          (view-result (a:with-rbac (*rbac*) (a:rbac-query where)))
          (field-keys (form-field-keys type-key form)))
    (view-result-values type-key field-keys view-result)))

;;
;; END Public backend functions
;;
