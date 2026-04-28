(in-package :data-ui)

(defun operator-sql (operator-key)
  ":private:"
  (case operator-key
    (:eq "=")
    (:ne "!=")
    (:gt ">")
    (:lt "<")
    (:gte ">=")
    (:lte "<=")
    (:like "like")
    (:ilike "ilike")
    (:not-like "not like")
    (:not-ilike "not ilike")
    (:in "in")
    (:not-in "not in")
    (otherwise (error "Unsupported operator ~a" operator-key))))

(defun uuid-p (s)
  (when (and (stringp s) (re:scan *uuid-regex* s)) t))

(defun timestamp-p (value)
  ":private: Returns T if VALUE is a timestamp string in the format
YYYY-MM-DD HH:MM:SS. (The space may be a T instead.) Otherwise, returns NIL."
  (and (stringp value) (re:scan *timestamp-regex* value)))

(defun filters-require-join-p (type-key filters)
  ":private: Returns T if any of the filters in FILTERS require a join table.
Otherwise, returns NIL."
  (loop for filter in filters
    for table-key = (car filter)
    for field-key = (cadr filter)
    thereis (not (equal table-key type-key))))

(defun type-key-p (key)
  ":private: Returns T if KEY is a keyword and a valid type key representing a
table in the model. Otherwise, returns NIL."
  (when (and (keywordp key)
          (u:tree-get *compiled-model* key))
    t))

(defun field-type-key-p (field-type-key)
  ":private: Returns T if FIELD-TYPE-KEY is a keyword and a valid type key for a
field."
  (when (and (keywordp field-type-key)
          (u:tree-get *field-types* field-type-key))
    t))

(defun field-key-p (type-key field-key)
  ":private: Returns T if FIELD-KEY is a keyword and a valid field key for
TYPE-KEY in the model. Otherwise, returns NIL."
  (when (and
          (keywordp field-key)
          (type-key-p type-key)
          (u:tree-get *compiled-model* type-key :fields field-key))
    t))

(defun operator-key-p (key)
  ":private: Returns T if KEY is a keyword and a valid operator key. Otherwise,
returns NIL."
  (when (and (keywordp key)
          (handler-case (operator-sql key)
            (error () nil)))
    t))

(defun filter-p (filter)
  ":private: Returns T if FILTER is a list of the form (TYPE-KEY FIELD-KEY
OPERATOR-KEY VALUE), with TYPE-KEY being a valid type key in the model,
FIELD-KEY being a valid field key for the type, OPERATOR-KEY being a valid
operator key, and VALUE being of the correct type for the field. Otherwise,
returns NIL."
  (and
    (listp filter)
    (= (length filter) 4)
    (type-key-p (car filter))
    (field-key-p (car filter) (cadr filter))
    (operator-key-p (caddr filter))
    (value-type-p (car filter) (cadr filter) (cadddr filter))))

(defun value-type-p (type-key field-key value)
  ":private: Returns T if VALUE is of the correct type for FIELD-KEY in
TYPE-KEY"
  (let* ((expected-type (u:tree-get *compiled-model* type-key :fields field-key
                          :type))
          (test (u:tree-get *field-types* expected-type :test)))
    (when (and test (funcall test value)) t)))

(setf *field-types*
  `(:text (:general :text :sql "text" :test ,#'stringp)
     :password (:general :text :sql "text" :test ,#'stringp)
     :real (:general :number :sql "real" :test ,#'numberp)
     :integer (:general :number :sql "integer" :test ,#'integerp)
     :boolean (:general :boolean :sql "boolean"
                :test ,(lambda (v) (member v '(:true :false))))
     :uuid (:general :text :sql "uuid" :test ,#'uuid-p)
     :timestamp (:general :text :sql "timestamp" :test ,#'timestamp-p)
     :list (:general :list :sql "" :test ,#'listp)))

(defun base-type-p (type-key)
  (u:tree-get *compiled-model* type-key :base))
