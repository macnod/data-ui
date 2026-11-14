(in-package :data-ui)

;;
;; BEGIN Internal helper functions
;;

(defun aggregate-values (aggregation values)
  ":private: Performs AGGREGATION on the VALUES list."
  (when values
    (case aggregation
      (:first (value-or-nil (car values)))
      (:list (remove-if-not #'value-or-nil values))
      (:distinct (u:distinct-values (remove-if-not #'value-or-nil values)))
      (:avg (let ((valid (remove-if-not #'value-or-nil values)))
              (when valid
                (float (/ (reduce #'+ valid) (length valid))))))
      (:sum (let ((valid (remove-if-not #'value-or-nil values)))
              (when valid (reduce #'+ valid))))
      (:count (let ((valid (remove-if-not #'value-or-nil values)))
                (when valid (length valid))))
      (t (error "Invalid aggregation ~s." aggregation)))))

(defun field-values-for-id (view-result id-key id-value alias-key aggregation
                            &key scope-alias scope-value)
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
how to aggregate the values for the field.

SCOPE-ALIAS and SCOPE-VALUE, when provided, filter the rows so that only rows
where the column identified by SCOPE-ALIAS matches SCOPE-VALUE are collected.
This is used for field-level scoping (e.g., :scope :user on a field source)."
  (loop for row in view-result
    for value = (getf row alias-key)
    when (and
           (equal (getf row id-key) id-value)
           (or
             (null scope-alias)
             (equal (getf row scope-alias) scope-value)))
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

(defun singular-target-field-p (field-def)
  ":private: T when FIELD-DEF is a singular-target field —
:target present, no :join-table, not the injected :id (an RBAC
base type's :id carries :target :resources; a generic rule would
emit :id-id on every base type). The one predicate classifying
linkable FKs at every site: derivation, collision checking,
conversion exclusion, suppression marking, decoration, and the
fe-fields :target wire key. M2M list fields never count."
  (and (getf field-def :target)
    (not (getf field-def :join-table))
    (not (getf field-def :base-field))))

(defun target-storage-alias-key (type-key field-key)
  ":private: The singular-target field's storage alias key —
the compiled <table>_<name-sql> alias under the field's own
:type-key in :views :main :aliases (a :target field's storage
column, selected by table-columns; it cannot collide with
another field's alias). NIL when the field has none."
  (u:make-keyword
    (u:tree-get *compiled-model* type-key :views :main :aliases
      type-key field-key)))

(defun storage-alias-exempt-p (type-key field-key field-def)
  ":private: T when FIELD-KEY's storage alias is exempt from
display-type conversion in view-result: the field is a singular
target field whose storage alias differs from the field's
compiled display :source :alias-key (the alias-global rule: a
claimed storage alias holds the FK UUID or :null whenever the
display value reads another alias; an own-column display keeps
today's conversion — its display value is genuinely that
column's). Without the exemption a populated :type :timestamp
target field signals inside view-result before it returns, and
a :type :boolean owner turns the UUID and a null FK both into
:true."
  (and (singular-target-field-p field-def)
    (let ((storage (target-storage-alias-key type-key field-key))
          (display (u:tree-get field-def :source :alias-key)))
      (and storage
        (not (equal storage display))))))

(defun exempt-storage-aliases (type-key)
  ":private: Alias-keyed, alias-global set of every exempt
storage alias on TYPE-KEY — the aliases view-result must not
display-convert for ANY field reading them (the wire contract
for a claimed alias is the raw column value: UUID or :null).
Built from the singular-target fields whose storage alias
differs from their display alias."
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    when (storage-alias-exempt-p type-key field-key field-def)
    collect (target-storage-alias-key type-key field-key)))

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
    (loop with exemptions = (exempt-storage-aliases type-key)
      for row in result
      collect
      (loop with m = *compiled-model*
        with fields = (u:tree-get m type-key :fields)
        for key in row by #'cddr
        for value in (cdr row) by #'cddr
        for field-key = (alias-key-to-field-key type-key key)
        for field-type = (u:tree-get fields field-key :type)
        ;; Record permalinks: an exempt storage alias carries
        ;; the raw UUID / :null — never display-converted for
        ;; any field reading it (the alias-global rule).
        for exempt = (and (member key exemptions) t)
        for final-value = (if exempt
                            value
                            (case field-type
                              (:timestamp (when (and
                                                  value
                                                  (not (equal value :null)))
                                            (dt:timestamp-string
                                              :universal-time value)))
                              ;; Public pages (Donnie, 2026-11-06): a NULL
                              ;; boolean stays :null (JSON null). The old
                              ;; (if value :true :false) read SQL nil as
                              ;; false but :null as true, so every nullable
                              ;; boolean reached the API as true.
                              (:boolean (cond
                                          ((eq value :null) :null)
                                          (value :true)
                                          (t :false)))
                              (t value)))
        appending (list key final-value)))))

(defun view-result-values (type-key field-keys view-result &key user)
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
      with scope-args = (scope-args type-key field-keys)
      for id in distinct-ids
      collect
      (append (list :id id)
        (loop for key in (cdr field-keys)
          for alias in (cdr alias-keys)
          for aggregation in (cdr aggregations)
          for scope-arg in (cdr scope-args)
          appending
          (list
            key
            (field-values-for-id view-result id-key id alias aggregation
              :scope-alias (getf scope-arg :alias)
              :scope-value (scope-value-for-kind
                             (getf scope-arg :kind) user))))))))

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

(defun has-all-exists-fragment (joiner-table value-table grain-fk
                                 value-fk display-column grain-table
                                 index)
  ":private: One correlated EXISTS subquery for a :has-all VALUE —
the record must join a row whose display column equals the value.
See has-all-conditions."
  (format nil "exists (select 1 from ~a j ~
               join ~a v on v.id = j.~a ~
               where j.~a = ~a.id and v.~a = $~d)"
    joiner-table
    value-table value-fk
    grain-fk grain-table
    (subseq display-column (1+ (position #\. display-column
                                 :from-end t)))
    index))

(defun has-all-conditions (table-key field-key values index)
  ":private: EXISTS fragments + bind values for one :has-all filter tuple
(LISTED-TYPE FIELD-KEY :HAS-ALL VALUES). Returns (values conditions params): one
correlated EXISTS per value through the field's :join-table, ANDed by
add-where-clause. The grain FK is the joiner reference targeting TABLE-KEY; the
value FK targets the field's :source :table; the display column is the compiled
:source :column-name (already stage-2 resolved)."
  (let* ((m *compiled-model*)
         (field-def (u:tree-get m table-key :fields field-key))
         (joiner-key (getf field-def :join-table))
         (value-table-key (u:tree-get field-def :source :table)))
    (unless (and joiner-key value-table-key)
      (report-ve "has-all-conditions"
        "Field ~s on ~s does not resolve to a join table." ~field-key
        ~table-key))
    (labels
      ((fk-name (target-key)
        "FK :name-sql of the joiner reference targeting TARGET-KEY."
        (loop for fk in (u:plist-keys
                          (u:tree-get m joiner-key :fields))
              for def = (u:tree-get m joiner-key :fields fk)
              when (eq (getf def :target) target-key)
              do (return (getf def :name-sql))
              finally
              (report-ve "has-all-conditions"
                "Joiner ~s has no reference targeting ~s." ~joiner-key
                ~target-key))))
      (let* ((grain-fk (fk-name table-key))
             (value-fk (fk-name value-table-key))
             (grain-table (u:tree-get m table-key :table-name))
             (value-table (u:tree-get m value-table-key :table-name))
             (display-column
               (let ((column-name
                       (u:tree-get field-def :source :column-name)))
                 (unless column-name
                   (report-ve "has-all-conditions"
                     "Field ~s on ~s has no display column."
                     ~field-key ~table-key))
                 column-name))
             (joiner-table (u:tree-get m joiner-key :table-name)))
        (values
          (loop for value in values
            for i from index
            collect
            (has-all-exists-fragment joiner-table value-table
              grain-fk value-fk display-column grain-table i))
          values)))))

(defun fk-eq-condition-column (type-key field-key)
  ":private: The table-qualified physical storage column for a singular-target
FIELD-KEY — the first table-column value from the compiled view's :columns
metadata (rt_comments.comment_post). Not the select-list output
alias (<table>_<column>) — PostgreSQL does not expose select-list aliases to the
same query's WHERE."
  (u:tree-get *compiled-model* type-key :views :main :columns
    type-key field-key))

(defun add-where-clause (sql filters user)
  ":private: Given SQL and FILTERS, this function extends the SQL with a where
clause that includes placeholders and returns a list of the SQL plus the values
for the placeholders."
  (if filters
    (loop
      for index = (next-param-index sql)
      then (1+ (length (u:flatten values)))
      for (table-key field-key op-key value) in filters
      ;; nil-valued :eq / :ne → is [not] null, no placeholder and no
      ;; collected value. Decided on the raw value before anything is
      ;; emitted, inside the loop: placeholder numbering counts the
      ;; values collected so far, so a translation that collected a
      ;; stray value would desynchronize every later $N (wrong bind or
      ;; a bind-count error). Post-db-value-fix, a nil reaching the
      ;; :eq arm would bind :null and never match (col = NULL is not
      ;; true in three-valued logic) — this arm makes the intent
      ;; match the SQL. :null from the wire is the same nil.
      for null-eq-p = (and (member op-key '(:eq :ne))
                        (or (null value) (equal value :null)))
      for fk-eq-p = (eq op-key :fk-eq)
      ;; A null :eq / :ne on a singular-target field compares the
      ;; storage column too, not the display :source :column-name:
      ;; two FKs to one table share the join (the suppressed
      ;; sibling's display names the other leg's rows), and the
      ;; display column is null-exactly-when-the-FK-is-null only as
      ;; a LEFT JOIN accident. The same column :fk-eq pins.
      for null-target-p = (and null-eq-p
                            (singular-target-field-p
                              (u:tree-get *compiled-model*
                                table-key :fields field-key)))
      for storage-p = (or fk-eq-p null-target-p)
      for fk-eq-column = (when storage-p
                           (or (fk-eq-condition-column table-key
                                 field-key)
                             (report-ve "add-where-clause"
                               "No storage column for :fk-eq / null ~
                                target field ~s on ~s." ~field-key
                               ~table-key)))
      for alias = (if storage-p
                    fk-eq-column
                    (to-sql-identifier
                      (u:tree-get *compiled-model*
                        table-key :fields field-key
                        :source :column-name)))
      for op = (operator-sql op-key)
      for log = (pl:pdebug :in "add-where-clause"
                  :table-key table-key
                  :field-key field-key
                  :op-key op-key
                  :value value
                  :alias alias)
      for has-all-p = (eq op-key :has-all)
      for exists-result = (when has-all-p
                            (multiple-value-list
                              (has-all-conditions table-key field-key
                                value index)))
      for has-all-fragments = (car exists-result)
      for has-all-values = (cadr exists-result)
      for placeholders = (cond
                           (null-eq-p nil)
                           ((member op-key '(:in :not-in))
                             (format nil "(~{~a~^, ~})"
                               (placeholders value :start-at index)))
                           (has-all-p
                             (format nil "(~{~a~^~%    and ~})"
                               has-all-fragments))
                           (t (format nil "$~d" index)))
      collect (cond
                (null-eq-p nil)
                ((member op-key '(:in :not-in))
                  (mapcar
                    (lambda (v)
                      (db-value table-key field-key user v))
                    value))
                ;; :has-all: EXISTS fragments bind display-column
                ;; atoms directly (identity text columns by
                ;; construction), one param per value — exactly the
                ;; :in element contract. NOTE: :has-all reaching
                ;; be-update / be-delete through this WHERE builder
                ;; works mechanically but is untested (REST sends
                ;; bare UUIDs there).
                (has-all-p has-all-values)
                ;; :fk-eq binds the wire uuid raw — never through
                ;; db-value: its target arm routes to
                ;; resolve-reference-id, whose uuid-p short-circuit
                ;; is real, but a display string still resolves — a
                ;; missed validator would become a silent title
                ;; compare instead of an error.
                (fk-eq-p value)
                (t (db-value table-key field-key user value)))
        into values
      ;; The :has-all condition is the parenthesized EXISTS group
      ;; alone — a standalone boolean expression, not the
      ;; "alias op value" shape the other operators produce.
      ;; The null-eq-p translation is "alias is [not] null" with
      ;; no placeholder — a singular-target field's null rides its
      ;; qualified storage column (the same column :fk-eq pins).
      ;; :fk-eq emits the plain "= $N" shape
      ;; against its qualified physical column.
      collect (cond
                (has-all-p placeholders)
                (null-eq-p
                  (format nil "~a is ~:[not ~;~]null" alias
                    (eq op-key :eq)))
                (fk-eq-p
                  (format nil "~a = ~a" alias placeholders))
                (t (format nil "~a ~a ~a" alias op placeholders)))
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

(defun scope-args (type-key field-keys)
  ":private: Returns a list of scope plists for FIELD-KEYS in TYPE-KEY.
Each element is (:alias alias-key :kind scope-kind) or NIL when the
field has no field-level scope."
  (loop
    with m = *compiled-model*
    for field-key in field-keys
    for scope-alias = (u:tree-get m type-key :fields field-key
                         :source :scope-alias)
    for scope-kind = (u:tree-get m type-key :fields field-key
                        :source :scope-kind)
    when scope-alias
      collect (list :alias scope-alias :kind scope-kind)
      else collect nil))

(defun scope-value-for-kind (scope-kind user)
  ":private: Resolve the scope value for SCOPE-KIND from USER.
Currently only :user is supported, which returns the user's UUID."
  (when scope-kind
    (case scope-kind
      (:user (a:get-id *rbac* "users" user))
      (t (error "Unsupported scope kind ~s" scope-kind)))))

(defun form-field-keys (type-key form)
  ":private: Returns a list of the field keys in the TYPE-KEY model's FORM node.
If there is no such FORM node or if the value associated with FORM is not a list
but rather T, then this function returns a list of all the field keys associated
with TYPE-KEY. The result always starts with :ID, even when FORM doesn't include
the :ID field key."
  (let* ((type-def (u:tree-get *compiled-model* type-key))
          (form-field-keys (u:tree-get type-def form :fields))
          (exclude-field-keys '(:file :button))
          (all-field-keys (remove-if
                            (lambda (k)
                              (member
                                (u:tree-get type-def :fields k :type)
                                exclude-field-keys))
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
             ;; NIL on a nullable scalar column is a clear-to-NULL:
             ;; map it to :null (SQL NULL). The driver binds bare NIL
             ;; as SQL FALSE, which 22007s on timestamp and silently
             ;; stores "false" on text. :boolean keeps NIL (a
             ;; legitimate false encoding); :list / :password never
             ;; reach the driver from these arms (join tables and
             ;; password hashing have their own write paths).
             ;; Public pages (2026-11-07): the compiler injects
             ;; :default :null on nullable columns, so full-data
             ;; hands db-value the :null keyword for every absent
             ;; nullable field — the scalar arms must treat it
             ;; exactly like nil (SQL NULL). Before this, a nullable
             ;; boolean bound the string "null" (22P02), a nullable
             ;; integer bound 'false' (parse-number of :null), and a
             ;; nullable password crashed in password-hash.
             (case type
               (:text (if (null value) :null value))
               (:password (if (eq value :null)
                            :null
                            (a:password-hash user value)))
               (:real (if (or (null value) (eq value :null))
                        :null
                        (if (numberp value) value (parse-number value))))
               (:integer (if (or (null value) (eq value :null))
                           :null
                           (if (numberp value) value (parse-number value))))
               (:boolean (cond
                           ((or (null value) (eq value :null)) :null)
                           (t (format nil "~(~a~)" value))))
               (:uuid (cond
                        ((or (null value) (eq value :null)) :null)
                        ((eq value :generate-uuid) (u:uuid))
                        (t value)))
               (:timestamp (if (or (null value) (eq value :null))
                             :null
                             value))
               (:list value)
               (t (report-ve "db-value"
                    "Invalid value ~s for field ~s ~s with field type ~s"
                    ~value ~type-key ~field-key ~type))))))))

(defun local-values-for-update (type-key data record user &key id)
  ":private: Finds the keys listed in the model for TYPE-KEY :SQL-UPDATE, looks
up the values for each key, and returns the values as a string. Each value is
chosen from DATA. If the value is not in DATA, then the value is taken from
RECORD. If the value is not in RECORD, and the field key is :ID, then the value
given in ID is used.  When a nullable field is explicitly NIL in DATA, that NIL
is respected (not overridden by the record value)."
  ;; Validations
  (valid-type-key type-key)
  ;; Collect data
  (loop with type-def = (getf *compiled-model* type-key)
    with main-update = (u:tree-get type-def :update-sql :main)
    for field-key in (cdr main-update)
    for field-def = (u:tree-get type-def :fields field-key)
    for data-value = (getf data field-key '%%absent%%)
    for data-has-key = (not (eq data-value '%%absent%%))
    for record-value = (getf record field-key)
    for default-value = (getf field-def :default)
    for id-value = (when (equal field-key :id) id)
    for field-value = (if data-has-key
                        data-value
                        (or record-value default-value id-value))
    for field-type = (getf field-def :type)
    for column = (getf field-def :column)
    when column
    collect (if (and (eq field-type :password) (not data-value))
              record-value
              (db-value type-key field-key
                (if (eq field-type :password)
                  (getf record :name user)
                  user)
                field-value))))

(defun user-fields (type-key)
  ":private: Returns a list of all non-base field keys for TYPE-KEY. Excludes
fields marked =:base-field=. Used by =full-data= to assemble the data plist for
insert/update."
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    unless (getf field-def :base-field)
    collect field-key))

(defun make-resource-name (type-key data)
  ":private: Returns a resource name for DATA, representing a new or existing
record of type TYPE-KEY. This name is composed of the downcased type-key, the
downcased key of the chosen field, the sanitized stringified value of that
field, and a random UUID. The field is :name when DATA carries a non-nil :name
value; otherwise the first non-base field in DATA's own order that carries a
value is used. The value has characters other than alphanumerics, dashes, and
slashes replaced with dashes, a leading and a trailing dash stripped, and the
result capped at 32 characters; an empty or absent value yields NIL. The
resulting name looks like this:

  todos:name:todo-1:6b3de23c-bf06-9b80-1e8e-40e9b66c3eaa

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
    for data-value = (getf data field-key '%%absent%%)
    for data-has-key = (not (eq data-value '%%absent%%))
    for field-type = (getf field-def :type)
    for record-value = (getf record field-key)
    for autofill = (getf field-def :autofill)
    for autofill-value = (case autofill
                           (:user user)
                           (otherwise nil))
    ;; Never pull password hashes from the record — they are
    ;; internal artifacts, not user-supplied data, and must not
    ;; flow back through validation.
    appending (list field-key
                ;; When DATA explicitly provides a key (even nil),
                ;; respect it. Only fall through to record/autofill/
                ;; default when the key is absent from DATA.
                (if data-has-key
                  data-value
                  (or (unless (eq field-type :password) record-value)
                      autofill-value default-value)))))

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

(defun user-defined-type-roles ()
  ":private: Returns a deduplicated list of all roles associated with
user-defined types (non-:built-in), excluding \"admin\"."
  (remove "admin"
    (remove-duplicates
      (loop
        for type-key in *compiled-model* by #'cddr
        for type-def in (cdr *compiled-model*) by #'cddr
        unless (u:tree-get type-def :built-in)
        appending (get-type-roles type-key))
      :test #'equal)
    :test #'equal))

(defun format-list-elements (list format-string)
  (mapcar
    (lambda (element) (format nil format-string element))
    list))

(defun show-roles-p (type-key form user)
  "Non-nil when the synthetic :roles checkbox-list belongs on FORM of
TYPE-KEY: non-base, not :suppress-roles, and FORM is neither :list-form
nor :view-form (list views show no Roles column; the synthetic :roles
is add/update only — a view surface declares the real :roles field
itself when it wants it). :users' real :roles M2M field is
a normal field and unaffected."
  (declare (ignore user))
  (let* ((m *compiled-model*)
          (base (u:tree-get m type-key :base))
          (suppress (u:tree-get m type-key :suppress-roles)))
    (when (and (not base) (not suppress)
               (not (member form '(:list-form :view-form))))
      t)))

(defun fe-fields (type-key user)
  (loop
    for form in *forms*
    when (u:tree-get *compiled-model* type-key form)
    append
    (list
      form
      (append
        (loop
          with base = (u:tree-get *compiled-model* type-key :base)
          with fields = (append
                          (u:tree-get *compiled-model* type-key :fields)
                          (when (show-roles-p type-key form user)
                            `(:roles (:ui (:label "Roles"
                                            :widget :checkbox-list)))))
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
                 (or (getf ui :widget) non-base-roles-field)
                 (not (equal (getf ui :widget) :hidden))
                 (not (and
                        (equal form :list-form)
                        (equal field-type :file)))
                 (not (and
                        (member form '(:list-form :add-form :view-form))
                        (equal field-type :button))))
          append (list field-key
                   (add-to-plist
                     (append
                       (list
                         :default default
                         :path path
                         :table table
                         :not-null (getf field-def :not-null)
                         :sortable (getf field-def :sortable)
                         :searchable (getf field-def :searchable))
                         ;; Two relation keys ride the leading
                         ;; plist, one per field family. :column
                         ;; rides it only on :target fields (the
                         ;; target's identity display column,
                         ;; downcased string — the reference chip
                         ;; picker's :in row names it);
                         ;; :record-lookup (the clause below) rides
                         ;; it only on :join-table fields — the M2M
                         ;; chip lookup. Never an unconditional nil:
                         ;; add-to-plist emits every union key, and
                         ;; a nil :column would serialize as
                         ;; "column":[] through plist-to-json's
                         ;; nil-value default. Neither key is an
                         ;; *ui-keys* member, so the :ui spread can
                         ;; never collide with them.
                         (when (getf field-def :target)
                           (list
                             :column
                             (string-downcase
                               (string
                                 (identity-field
                                   (getf field-def :target))))
                             ;; Record permalinks: the additive
                             ;; target wire key — the field's
                             ;; actual :target type key
                             ;; (singular-target fields only,
                             ;; including suppression-marked ones;
                             ;; the sibling's absence withholds the
                             ;; link on those, not this key).
                             :target (getf field-def :target)))
                         ;; :record-lookup rides join-table fields
                         ;; only: the /api/value-id call (type +
                         ;; field) that resolves one chip value to
                         ;; its target row (chip-to-record
                         ;; navigation). Omitted, never nil,
                         ;; elsewhere.
                         (let ((jt (getf field-def :join-table))
                               (st (u:tree-get field-def :source :table))
                               (sc (u:tree-get field-def :source :column)))
                           (when (and jt st sc)
                             (list :record-lookup
                                   (list :type st
                                         :field (string-downcase
                                                  (string sc)))))))
                     ui)))
        ;; Form-level :hide-list (sibling of :fields) rides the same
        ;; form object as a boolean — but only when declared. append
        ;; of a nil tail is a no-op, so an undeclared form stays
        ;; byte-identical. Key membership on u:plist-keys, never raw
        ;; member on the plist (raw member scans values too) and
        ;; never add-to-plist (its (or new old) overlay cannot pin a
        ;; new nil). Registration as a JSON boolean key serializes a
        ;; declared nil as false, never [] (truthy in JS).
        (let ((form-plist (u:tree-get *compiled-model* type-key form)))
          (when (member :hide-list (u:plist-keys form-plist))
            (list :hide-list (getf form-plist :hide-list))))))))

(defun true-or-false (&rest path)
  (if (apply #'u:tree-get (cons *compiled-model* path)) :true :false))

(defun user-can-p (user type-key permission)
  ":private: T when USER holds PERMISSION on TYPE-KEY's type resource.
list-result ANDs this with the model's :create/:update/:delete capabilities so
the wire flags reflect the actual user (a guest who can read a public type must
not see Add / Edit / Delete)."
  (a:user-allowed *rbac* user permission
    (type-resource-name type-key)))

(defun user-capability (user type-key capability)
  ":private: :true when the model grants CAPABILITY (:create, :update, :delete)
on TYPE-KEY and USER holds the matching RBAC permission on the type's resource,
:false otherwise. Model nil (e.g. read-only rollups) always wins."
  (if (and (u:tree-get *compiled-model* type-key capability)
           (user-can-p user type-key
             (string-downcase capability)))
    :true :false))

(defun region-spec-list (children)
  ":private: Wire emission of a compiled :view-form :children value (plan 02).
Returns a one-element list of wire specs: :type and :via become downcased
strings (plist-to-json preserves strings verbatim, and plain (string :comments)
would yield \"COMMENTS\"); :sort normalizes to a real two-element string list
with the direction defaulted and normalized — never a sort plist, which u:plistp
would send down the plist branch and serialize as {\"field\":\"dir\"} (the
json-sort-echo trap). The string pair (\"created-at\" \"asc\") has string keys,
so plist-to-json emits the JSON array [\"created-at\",\"asc\"]. An omitted :sort
and an explicit (:created-at) both normalize to (\"created-at\" \"asc\"); the
compiled def keeps the author spec verbatim (asserted by tests) — normalization
happens here, at attach time only. Pure: takes the children value, so tests pass
author specs directly."
  (loop for spec in children
    for s = (or (getf spec :sort) '(:created-at :asc))
    collect (list
              :type (string-downcase (string (getf spec :type)))
              :via (string-downcase (string (getf spec :via)))
              :sort (list (string-downcase (string (first s)))
                      (string-downcase
                        (string (or (second s) :asc)))))))

(defun json-sort-echo (sort)
  ":private: Wire shape of the list-result :sort echo (plan 09 Step 5, pinned in
plan 01). SORT is nil or (:field-key :asc|:desc). Returns (:field <field-key>
:dir :asc|:desc) or :null. :dir is a keyword: plist-to-json downcases symbols
into quoted strings, so the JSON is byte-identical to the string form it
replaced ({\"dir\":\"desc\"}). Built explicitly: the raw plist would serialize
as a single-key object {\"field\": \"dir\"}, and bare nil would serialize as
[] (the :avg trap), both wrong. rest.lisp ships this plist / :null through
plist-to-json as {\"field\": ..., \"dir\": ...} / JSON null."
  (if sort
    (list :field (first sort)
      :dir (or (second sort) :asc))
    :null))

(defun list-result (type-key user form &key
                     field-keys view-result total skip-allowed-values
                     records sort)
  ;; Record-permalink siblings ride list records too (same
  ;; decoration, same data on both public read surfaces; list
  ;; cells ignore the key). Public pages: :public-url rides the
  ;; records of a public type (decorate-public-url, public-pages.lisp
  ;; — a forward call, the start-web-server precedent), computed from
  ;; one permitted-row-ids pass for the whole page.
  (let* ((decorated-records
           (or records
             (view-result-values type-key field-keys view-result
               :user user)))
         (public-ids
           (when (public-type-p type-key)
             (permitted-row-ids type-key *guest-user* "read")))
         (id-alias
           (when (and view-result field-keys)
             (u:tree-get *compiled-model* type-key :fields :id
               :source :alias-key)))
         (sibling-keys
           (when (and view-result field-keys)
             (loop
               with m = *compiled-model*
               for field-key in (cdr field-keys)
               for field-def = (u:tree-get m type-key :fields
                                 field-key)
               for eligible = (and
                                (singular-target-field-p field-def)
                                (not (getf field-def
                                     :suppress-sibling)))
               for storage = (when eligible
                               (target-storage-alias-key type-key
                                 field-key))
               when storage
               collect field-key
               and collect storage
               and collect (u:tree-get field-def :source :scope-alias)
               and collect (u:tree-get field-def :source
                             :scope-kind)))))
    (add-to-plist
      (list
        :type-key type-key
        :total (or total 0)
        ;; Plan 01 / 09: the user-facing effective sort on every
        ;; successful list read (base and measure). The measure
        ;; branch passes the default first-sortable-measure when
        ;; the request sent none; the base branches pass the
        ;; effective sort (request or :default-sort) or nil.
        ;; Never the id / grain-id tiebreaker.
        :sort (json-sort-echo sort)
        :create (user-capability user type-key :create)
        :update (user-capability user type-key :update)
        :delete (user-capability user type-key :delete)
        :records (mapcar
                   (lambda (r)
                     (decorate-public-url r type-key public-ids))
                   (decorate-records-with-caps type-key
                     (add-roles-to-view type-key form user
                       (mapcar
                         (lambda (r)
                           (let ((blanked
                                   (blank-password-fields type-key r)))
                             (if (and sibling-keys
                                   (getf blanked :id)
                                   (not (null view-result)))
                               (append
                                 (if (listp blanked) blanked
                                   (list blanked))
                                 (loop
                                   for (field-key storage scope-alias
                                          scope-kind)
                                     on sibling-keys by (lambda (l)
                                                          (nthcdr 4 l))
                                   for value = (field-values-for-id
                                                 view-result id-alias
                                                 (getf blanked :id)
                                                 storage :first
                                                 :scope-alias scope-alias
                                                 :scope-value
                                                 (scope-value-for-kind
                                                   scope-kind user))
                                   when (and value
                                          (not (equal value :null)))
                                   collect (u:make-keyword
                                             (format nil "~a-id"
                                               field-key))
                                   and collect value))
                               blanked)))
                         ;; 08b measure path: RECORDS are already
                         ;; field-key plists from the GROUP BY
                         ;; query. No Phase B collapse.
                         decorated-records))
                     user))
        :allowed-values (if skip-allowed-values
                          nil
                          (allowed-values type-key user))
        :type-roles (if (equal type-key :users)
                      (remove-duplicates
                        (append (u:tree-get *compiled-model* type-key
                                  :type-roles)
                          (user-defined-type-roles))
                        :test #'equal)
                      (u:tree-get *compiled-model* type-key
                        :type-roles)))
      (fe-fields type-key user))))

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

(defun admin-only-user-roles ()
  '("ai-user" "deployer"))

(defun valid-admin-only-user-roles (user new-roles
                                     &optional existing-roles)
  ":private: FR-11 gate. Signals a validation error when a non-admin actor is
*adding* an admin-only role (ai-user / deployer) to a user's membership.
Stripping a stale grant is allowed (demos cannot lock a VIP in). Keyword :roles
on a non-:users be-insert / be-update is resource roles and must not call this."
  (unless (equal user "admin")
    (let* ((existing (or existing-roles
                       (a:list-user-role-names *rbac* user)))
           (adds (set-difference new-roles existing :test 'equal)))
      (loop for role in (admin-only-user-roles)
        when (member role adds :test 'equal)
        do (report-ve "valid-admin-only-user-roles"
             "Only admin may assign role ~s to a user." ~role)))))

(defun protected-users ()
  ":private: FR-12. Extra protected user names from the PROTECTED_USERS
environment variable (comma-separated), read at call time so a test can bind
it. NIL when unset or empty."
  (let ((raw (u:getenv "PROTECTED_USERS")))
    (when (and raw (plusp (length raw)))
      (remove "" (mapcar #'u:trim (re:split "," raw))
        :test #'equal))))

(defun valid-protected-record (type-key record user)
  ":private: FR-12 gate. Signals a validation error when a non-admin actor
updates or deletes a record whose identity is in the protected set for TYPE-KEY:
admin / guest / PROTECTED_USERS on :users, *reserved-role-names* on :roles,
a:*init-permissions* on :permissions. No-op for every other type and for admin."
  (unless (or (equal user "admin")
            (not (member type-key '(:users :roles :permissions))))
    (let ((name (getf record :name)))
      (when (and name
              (member name
                (case type-key
                  (:users
                    (list* a:*admin* a:*guest* (protected-users)))
                  (:roles *reserved-role-names*)
                  (:permissions (a:initial-permissions)))
                :test #'equal))
        (report-ve "valid-protected-record"
          "Record ~s of type ~s is protected; only admin may change ~
           or delete it."
          ~name ~type-key)))))

(defun update-roles (type-key filters user roles)
  ":private: Updates the RBAC roles for the resource of type TYPE-KEY with ID.
ROLES is a list of role names. ID is a UUID string. This function returns the
resource name of the updated record if the update is successful, or an error if
the update fails."
  (valid-type-key type-key)
  (valid-roles roles)
  (if (uuid-p filters)
    (valid-existing-uuid filters)
    (valid-filters filters :required t :type-key type-key))
  ;; The update-roles function should work for changing the roles associated
  ;; with a user or with a resource.
  (case type-key
    (:users
      (let* ((user-id (be-id :users filters user))
              (user-name (a:get-value *rbac* "users" "user_name" "id" user-id))
              (exclusive-roles (a:exclusive-role-for user-name))
              ;; D12 keep-only force-reattach: the Edit form never
              ;; sends these three (add-roles-to-view strips them from
              ;; the view; App.tsx resubmits the stripped list), so
              ;; re-attach each only when the target already holds it.
              ;; An unconditional add would grant settings to guest on
              ;; any save (the D1 theme leak) and strip a demoted
              ;; VIP's self-serve secrets path.
              (held-roles (a:list-user-role-names *rbac* user-name))
              (reattach (loop for role in (list "logged-in" "settings"
                                          exclusive-roles)
                          when (member role held-roles :test 'equal)
                          collect role))
              (roles (append reattach roles))
              (existing-roles (a:list-user-role-names *rbac* user-name))
              (to-add (set-difference roles existing-roles :test 'equal))
              (to-remove (set-difference existing-roles roles :test 'equal)))
        ;; FR-11 gate: the single must-gate. The Edit form's keyword
        ;; :roles path lands here (App.tsx sends roles top-level →
        ;; REST → be-update &key roles → this branch). be-update has
        ;; already loosened the holder check with
        ;; remove-existing-non-user-roles; this is the re-tightening.
        (valid-admin-only-user-roles user roles existing-roles)
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

(defun identity-field (type-key)
  ":private: For types that are referenced as target types, which must have a
single identity field, this function returns the field key for that identity
field."
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    when (getf field-def :identity)
    do (return field-key)))

(defun resolve-reference-id (type-key field-key value)
  ":private: If the field has a :target (and no :join-table), treat VALUE as a
display name from the target's :source :column (defaults to the target type's
identity field), look it up and return the matching UUID. Signals validation
error if no match or value is not a string. For non-reference fields return
VALUE unchanged."
  (let* ((m *compiled-model*)
          (field-def (u:tree-get m type-key :fields field-key))
          (target (getf field-def :target))
          (joiner (getf field-def :join-table)))
    (if (and target (not joiner))
      (cond
        ((or (null value) (equal value :null)) :null)
        ((uuid-p value) value)
        ((listp value) value)
        ((not (stringp value))
          (report-ve "resolve-reference-id"
            "Reference field ~s ~s expects a string value, got ~s"
            ~type-key ~field-key ~value))
        (t
          (let* ((target-type target)
                  (col-key (or
                             (u:tree-get field-def :source :column)
                             (identity-field target-type)))
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
or update that includes those values. For join-table list fields, resolves
the target type and identity field from model metadata."
  (valid-type-key type-key)
  (valid-field-key type-key field-key)
  (valid-values-list values)
  (let* ((m *compiled-model*)
         (field-def (u:tree-get m type-key :fields field-key))
         (joiner (getf field-def :join-table))
         (target-type (if joiner
                        (u:tree-get field-def :source :table)
                        type-key))
         (target-field (if joiner
                         (identity-field target-type)
                         field-key)))
    (loop for value in values
      for id = (be-value-id target-type target-field value "admin")
      when (not id) collect value)))

(defun id-to-resource-name (id)
  ":private: Returns the resource name for the resource of type with
ID. This is necessary for dealing with RBAC resources given that the RBAC API
uses names instead of IDs. Returns NIL if ID is not a valid UUID, does not
exist, or points to a base or internal table."
  (when (and id (uuid-p id))
    (query "select resource_name from resources where id = $1"
      :params (list id)
      :result-type :single)))

(defun id-dedup-filter (type-key field-key value)
  ":private: The dedup filter tuple for FIELD-KEY of TYPE-KEY carrying
wire VALUE. A singular-target field compares on its physical storage
column: :fk-eq with the reference resolved to the target row's uuid
(resolve-reference-id takes both wire spellings — uuid and display
name). An :eq filter on a :target field never matches: its alias is
the field's joined display column while db-value resolves the bind to
a uuid, a text-versus-uuid compare — the skip-existing lookup misses,
and the real INSERT dies on the identity unique index (23505). Null
FK values keep the :eq spelling (valid-filter rejects a null :fk-eq;
add-where-clause renders the null :eq as the display column IS NULL,
which a null storage column satisfies under the LEFT JOIN)."
  (if (and (singular-target-field-p
             (u:tree-get *compiled-model* type-key :fields field-key))
           (not (null value))
           (not (equal value :null)))
    (list type-key field-key :fk-eq
      (resolve-reference-id type-key field-key value))
    (list type-key field-key :eq value)))

(defun id-from-data (type-key data)
  ":private: Returns the ID of the record of type TYPE-KEY that matches DATA.
If DATA contains an :ID key, then the value associated with that key is
returned. Otherwise, this function expects DATA to contain a value for each
local field (See the LOCAL-FIELDS function) of TYPE-KEY, and looks up the ID
of the record that matches those values. If no record matches, then this
function returns NIL. This function ignores any non-local fields in DATA.
If DATA points to multiple records, then this function raises an error.
Singular-target fields match through their storage column (see
ID-DEDUP-FILTER), the same comparison the INSERT itself writes."
  (or (getf data :id)
    (let ((filters (loop for field in (local-fields type-key)
                     for value = (getf data field)
                     ;; Reserved default keywords (:generate-uuid, :now)
                     ;; are not user-supplied identity data; they must
                     ;; not become :eq filter values (Postgres cannot
                     ;; bind a function name as a uuid/timestamp).
                     unless (member value '(:generate-uuid :now))
                     do (valid-value-type type-key field value)
                     collect (id-dedup-filter type-key field value))))
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
USER has 'read' permissions for. Public pages (Donnie, 2026-11-06): delegates
to permitted-row-ids, which pages through the RBAC join to the user's resource
count — this function's former single a:list-user-resources call saw one page
of a:*default-page-size* role×resource rows and silently truncated at 10,000,
dropping readable rows from every be-list (SPA and /p/ alike). Same return
shape: resource-id strings."
  (permitted-row-ids type-key user "read"))

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

(defun permitted-row-ids (type-key user permission)
  ":private: Resource ids of TYPE-KEY rows USER holds PERMISSION on —
the set question behind user-allowed-resource's per-id ask, answered
through the same library join the per-row gate uses (a:list-user-resources
filtered to the type's resource-name prefix, the user-read-type-ids
precedent; the library's filter grammar has no id-set operator, so the
prefix filter is the page-batched form). Consumed past the library's
pagination: pages repeat (role x resource) rows, so the stop condition
is distinct ids reaching a:user-resource-count's count with the same
filters (count is the robust stop; a short page only means no more).
An empty page is the belt: roles changing mid-loop must not spin
forever. Returns NIL when the user holds the permission on none."
  (let* ((search (format nil "~(~a~):%" type-key))
         (filters `(("resource_name" "like" ,search)))
         (count (a:user-resource-count *rbac* user permission
                   :filters filters)))
    (when (plusp count)
      (loop
        with ids = nil
        and page = 1
        and page-size = (max a:*default-page-size* count)
        while (< (length ids) count)
        do
        (let ((rows (a:list-user-resources *rbac* user permission
                      :page page :page-size page-size
                      :filters filters)))
          (unless rows (return ids))
          (dolist (row rows)
            (pushnew (getf row :resource-id) ids :test #'equal))
          (incf page))
        finally (return ids)))))

(defun row-capabilities (type-key records user)
  ":private: Per-record update/delete capability flags for every
record in RECORDS (field-key plists). Returns a plist of two alists
(:update :delete), each mapping every record id to :true / :false —
keyword booleans, so plist-to-json emits JSON booleans without the
registry (do not register these keys: a registered bare nil would
serialize as false, silently converting a forgotten emission into a
visible denial). Answers exactly what be-update / be-delete answer
per row (user-allowed-resource semantics), never a hand-rolled join
that could drift from a:user-allowed. Base types degenerate to the
one live arm of user-allowed-resource (base-type-p → a:user-allowed
on the type resource; the member/base-resource-type-keys arm beside
it is dead code, and not because the else branch re-derives what
base-type-p rejected — that argument is false — but because
base-resource-type-keys holds only non-internal base types whose
rows have no resources rows, so the when resource-name gate ahead of
it already returns nil). The model-level :update / :delete
capabilities gate per type with user-capability's own truthiness —
non-nil = granted (:EQ T IS WRONG: base types store :AUTO) — and a
nil model capability forces :false on every row with no query, which
is also the rollup answer (rollups compile both caps to nil)."
  (flet ((kw (x) (if x :true :false)))
    (let* ((model-update (u:tree-get *compiled-model* type-key :update))
           (model-delete (u:tree-get *compiled-model* type-key :delete))
           (base-p (base-type-p type-key))
           (type-update
             (and model-update
               (a:user-allowed *rbac* user "update"
                 (type-resource-name type-key))))
           (type-delete
             (and model-delete
               (a:user-allowed *rbac* user "delete"
                 (type-resource-name type-key))))
           (update-ids (when (and model-update (not base-p))
                         (permitted-row-ids type-key user "update")))
           (delete-ids (when (and model-delete (not base-p))
                         (permitted-row-ids type-key user "delete"))))
      (loop for record in records
        for id = (getf record :id)
        collect (cons id
                 (kw (if base-p
                       type-update
                       (and update-ids
                         (member id update-ids :test #'equal)))))
          into update-alist
        collect (cons id
                 (kw (if base-p
                       type-delete
                       (and delete-ids
                         (member id delete-ids :test #'equal)))))
          into delete-alist
        finally (return (list :update update-alist
                          :delete delete-alist))))))

(defun record-capability (id alist)
  ":private: Lookup ID in a row-capabilities alist, defaulting to
:FALSE on a miss so an id that somehow escapes the map still emits
a wire flag (a nil on the record plist would serialize as [] and
the frontend's === true gate would read it as not-permitted)."
  (or (cdr (assoc id alist :test #'equal)) :false))

(defun decorate-records-with-caps (type-key records user)
  ":private: Append :update / :delete keyword flags to every record
plist, computed per row by row-capabilities. Appends (never
add-to-plist, whose (or new old) lookup would treat :false as
unspecified and silently drop the key); reserved-name guards keep
author fields off these keys, so no shadowing."
  (let* ((caps (row-capabilities type-key records user))
         (update-alist (getf caps :update))
         (delete-alist (getf caps :delete)))
    (mapcar
      (lambda (r)
        (let ((id (getf r :id)))
          (append r
            (list :update (record-capability id update-alist)
              :delete (record-capability id delete-alist)))))
      records)))

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
    for meta = (join-field-meta type-key key)
    for other-type = (getf meta :other-type)
    for column = (u:tree-get m type-key :fields key :source :column)
    for existing-values = (getf record key)
    for new-values = (getf data key)
    for to-add = (set-difference new-values existing-values :test 'equal)
    for to-delete = (set-difference existing-values new-values :test 'equal)
    for q-delete = (u:tree-get update key :delete)
    for q-delete-sql = (car q-delete)
    for q-delete-keys = (cdr q-delete)
    for q-delete-key-1 = (getf meta :owner-fk-key)
    for q-delete-key-2 = (getf meta :other-fk-key)
    for q-insert = (u:tree-get update key :insert)
    for q-insert-sql = (car q-insert)
    for q-insert-keys = (cdr q-insert)
    for q-insert-key-1 = (getf meta :owner-fk-key)
    for q-insert-key-2 = (getf meta :other-fk-key)
    do
    ;; Delete
    (loop for x-id in (list-ids other-type column to-delete)
      for q-delete-values = (loop for k in q-delete-keys
                              when (equal k q-delete-key-1) collect id
                              when (equal k q-delete-key-2) collect x-id)
      for q-delete-query = (cons q-delete-sql q-delete-values)
      do (a:with-rbac (*rbac*) (a:rbac-query q-delete-query)))
    ;; Insert
    (loop for x-id in (list-ids other-type column to-add)
      for q-insert-values = (loop for k in q-insert-keys
                              when (equal k q-insert-key-1) collect id
                              when (equal k q-insert-key-2) collect x-id)
      for q-insert-query = (cons q-insert-sql q-insert-values)
      do (a:with-rbac (*rbac*) (a:rbac-query q-insert-query)))))

(defun join-keys (type-key)
  (loop with update = (u:tree-get *compiled-model* type-key :update-sql)
    for key in update by #'cddr
    unless (equal key :main) collect key))

(defun blank-password-fields (type-key record)
  "Return RECORD with any :password-typed fields set to nil, so password
hashes never reach the frontend."
  (when record
    (loop with fields = (user-fields type-key)
          for (key val) on record by #'cddr
          for field-def = (u:tree-get *compiled-model*
                                       type-key :fields key)
          for field-type = (getf field-def :type)
          when (member key fields)
            appending (list key (if (eq field-type :password) nil val))
          else
            appending (list key val))))

(defun decorate-record-with-target-ids (type-key form field-keys
                                          view-result user record)
  ":private: Record-permalink sibling emission: for each
requested singular-target field on FORM that is not
suppression-marked, append the pair \"<field-key>-id\" = the
field's storage alias collapsed with :agg :first from the
already-computed VIEW-RESULT rows (the same
field-values-for-id machinery the display value uses), with the
field's existing scope-args applied (a scoped display whose
scope filters the viewer out emits no sibling — no label-less
link). The key is omitted entirely when the value is null (:null
or nil — never a present nil, which plist-to-json renders as []
and JS reads as truthy). The field's own key keeps the display
string. No resolver, no second query."
  (let ((m *compiled-model*))
    (loop
      with id-alias = (u:tree-get m type-key :fields :id
                        :source :alias-key)
      for field-key in (cdr field-keys)
      for field-def = (u:tree-get m type-key :fields field-key)
      for eligible = (and (singular-target-field-p field-def)
                       (not (getf field-def :suppress-sibling)))
      when eligible
      collect
      (let* ((storage
               (target-storage-alias-key type-key field-key))
             (scope-alias
               (u:tree-get field-def :source :scope-alias))
             (scope-kind
               (u:tree-get field-def :source :scope-kind))
             (value
               (field-values-for-id view-result id-alias
                 (getf record :id) storage :first
                 :scope-alias scope-alias
                 :scope-value (scope-value-for-kind scope-kind
                                user))))
        (when (and storage value (not (equal value :null)))
          (list (u:make-keyword
                  (format nil "~a-id" field-key))
            value)))
      into pairs
      finally (return (apply #'append pairs)))))

(defun rec (id user &key (form :update-form) type-key (public t)
             (blank-passwords t))
  ":private: Returns the TYPE-KEY record with the given ID, provided that it is
accessible to USER. Given the IDs are UUIDs (globally unique), TYPE-KEY is
optional. However, providing TYPE-KEY helps the function avoid an extra database
lookup. PUBLIC tells this function to accept only non-internal TYPE-KEYs.
BLANK-PASSWORDS controls whether password hashes are nilled out before returning
(default T for frontend safety; pass NIL for internal callers that need the real
hash, e.g. be-update).
Record permalinks: rec returns a second value — the raw
view-result rows it already computed — beside the unchanged
envelope, for be-rec's sibling decoration (nil exactly when the
record is inaccessible). Internal callers read the first value
only and never see the sibling key."
  (valid-existing-user user)
  (when type-key (if public
                   (valid-be-type-key type-key)
                   (valid-type-key type-key)))
  (let ((type-key (if type-key type-key (id-to-type-key id))))
    ;; Issue 9: rollups are list-family only; a rollup id never reaches here (no
    ;; inserts), but an explicit :type-key rollup must reject before the
    ;; view-SQL path below.
    (valid-non-rollup-type-key type-key)
    (when (and (user-allowed-resource user id "read") (uuid-exists-p id))
      (pl:pdebug :in "rec" :filters (list type-key :id :eq id))
      (let* ((m *compiled-model*)
              (type-key (if type-key type-key (id-to-type-key id)))
              (sql (u:tree-get m type-key :views :main :sql))
              (where (add-where-clause
                       sql (list (list type-key :id :eq id)) user))
              (view-result (view-result type-key where))
              (field-keys (form-field-keys type-key form)))
        (values
          (list
            :type type-key
            :fields (fe-fields type-key user)
            :record (car
                      (add-roles-to-view
                        type-key form user
                        (mapcar
                          (if blank-passwords
                            (lambda (r) (blank-password-fields type-key r))
                            #'identity)
                          (view-result-values type-key field-keys view-result
                            :user user))))
          :allowed-values (allowed-values type-key user))
          view-result)))))

(defun join-field-meta (owner-type-key list-field-key)
  "Return plist :other-type :owner-fk-key :other-fk-key for a
:type :list field with :join-table. Derives all values from
compiled model metadata — no naming convention assumptions."
  (let* ((m *compiled-model*)
         (field-def (u:tree-get m owner-type-key :fields list-field-key))
         (joiner (getf field-def :join-table))
         (other-type (u:tree-get field-def :source :table))
         (owner-fk-key (id-key owner-type-key))
         (other-fk-key (id-key other-type)))
    (unless (and joiner other-type)
      (report-e "join-field-meta"
        "Field ~s on ~s is not a valid join-table list field"
        ~list-field-key ~owner-type-key))
    (list :other-type other-type
          :owner-fk-key owner-fk-key
          :other-fk-key other-fk-key)))

(defun insert-join-table-rows (type-key uuid data)
  (loop
    with m = *compiled-model*
    with uuid-key = (id-key type-key)
    with insert = (u:tree-get m type-key :insert-sql)
    with keys = (remove-if
                  (lambda (k) (member k '(:resource :main)))
                  (u:plist-keys insert))
    for key in keys
    for qt = (getf insert key)
    for sql = (car qt)
    for param-keys = (cdr qt)
    for names = (getf data key)
    for meta = (join-field-meta type-key key)
    for ref-type-key = (getf meta :other-type)
    for value-key = (getf meta :other-fk-key)
    for value-ids = (list-ids ref-type-key
                        (identity-field ref-type-key) names)
    when value-ids do
    (loop for value-id in value-ids
      for params = (loop for k in param-keys
                     collect (cond
                               ((eq k uuid-key) uuid)
                               ((eq k value-key) value-id)
                               (t (report-e "insert-join-table-rows"
                                    "Unknown param key ~s in ~s"
                                    ~k ~param-keys))))
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

(defun log-palette-denied (type-key field-key target-type-key user e)
  "Log that the palette for FIELD-KEY on TYPE-KEY came back empty:
the palette query raised a validation error E — usually read
denied on TARGET-TYPE-KEY for USER, but any palette validation
failure lands here. Empty palette, not an error — one field's
palette must not fail the parent list."
  (pl:perror :in "log-palette-denied"
    :status "palette suppressed"
    :type-key type-key :field-key field-key
    :target-type-key target-type-key :user user :error e))

(defun allowed-values-for-field (type-key field-key user)
  (let* ((m *compiled-model*)
          (options (u:tree-get m type-key :fields field-key
                     :ui :options)))
    (if options
      (copy-list options)
      (let* ((source (or (u:tree-get m type-key :fields field-key
                           :source-all)
                       (u:tree-get m type-key :fields field-key
                         :source)))
              (x-type-key (getf source :table))
              (x-field-key (getf source :column))
              (values (getf
                        (handler-case
                          (be-list-column x-type-key x-field-key user
                            :skip-allowed-values t)
                          (validation-error (e)
                            ;; A palette is best-effort: the palette
                            ;; query raised a validation error — most
                            ;; often valid-user-permissions on the
                            ;; target (e.g. :users restricted via
                            ;; :type-roles), but any palette
                            ;; validation failure lands here too. One
                            ;; field's palette must never fail the
                            ;; parent list — return an empty palette
                            ;; and log why.
                            (log-palette-denied type-key field-key
                              x-type-key user e)
                            nil))
                        :values)))
        (if (and (equal type-key :users)
              (equal field-key :roles))
          ;; Users can never be assigned an exclusive role directly,
          ;; so no :exclusive-suffixed role belongs on this palette.
          ;; FR-11: ai-user / deployer are admin-only assignments —
          ;; hide them from non-admins (the visible path; the write
          ;; gate lives in update-roles).
          (remove-if
            (lambda (r)
              (or (member r '("admin" "settings" "logged-in")
                          :test 'equal)
                  (and (not (equal user "admin"))
                    (member r (admin-only-user-roles) :test 'equal))
                  (exclusive-role-p r)))
            values)
          values)))))

(defun selectable-roles (type-key user)
  ":private: Returns the list of roles the current USER can assign on
TYPE-KEY. For admin, returns all roles minus system exclusives: admin,
admin:exclusive, guest:exclusive, settings, the auto-granted logged-in,
and the viewer's own exclusive. For other users, returns the user's own
roles plus \"public\" plus the type's :type-roles plus every other
user's shareable exclusive role (point-to-point sharing)."
  (let ((system-exclusives
          (list* "admin" "admin:exclusive" "guest:exclusive" "settings"
                 "logged-in"           ; auto-granted, not assignable
                 (when user
                   (list (a:exclusive-role-for user))))))
    (if (equal user "admin")
      (remove-if
        (lambda (r) (member r system-exclusives :test 'equal))
        (a:list-role-names *rbac*))
      (let* ((user-roles (a:list-user-role-names *rbac* user))
             (type-roles (u:tree-get *compiled-model* type-key :type-roles))
             (shareable-exclusives
               (remove-if-not #'shareable-exclusive-role-p
                 (a:list-role-names *rbac*))))
        (remove-duplicates
          (remove-if
            (lambda (r) (member r system-exclusives :test 'equal))
            (append user-roles (list "public") type-roles
              shareable-exclusives))
          :test 'equal)))))

(defun allowed-values (type-key user)
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    and base = (u:tree-get *compiled-model* type-key :base)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for join-table = (getf field-def :join-table)
    for target = (getf field-def :target)
    for ui-options = (u:tree-get field-def :ui :options)
    when (and
           (or join-table target ui-options)
           (not (equal field-key :id)))
    appending
    (list field-key (allowed-values-for-field type-key field-key user))
    into allowed
    finally
    (return
      (if (or base
            ;; 08b: :suppress-roles t (rollups, :secrets) has no roles palette.
            ;; Gate on the flag, not on shape.
            (u:tree-get *compiled-model* type-key :suppress-roles))
        allowed
        (append
          allowed
          (list :roles (selectable-roles type-key user)))))))

(defun add-roles-to-view (type-key form user view)
  (when view
    (let* ((m *compiled-model*)
            (fields (u:tree-get m type-key form :fields))
            (base (u:tree-get m type-key :base)))
      (cond
        ((show-roles-p type-key form user)
          (loop
            for record in view
            for id = (getf record :id)
            for resource-name = (id-to-resource-name id)
            for roles = (remove-if
                          (lambda (x)
                            (u:has
                              (list "admin" "settings"
                                (a:exclusive-role-for "admin")
                                (a:exclusive-role-for user))
                              x))
                          (a:list-resource-role-names *rbac* resource-name))
            collect (add-to-plist record (list :roles roles))))
        ((equal type-key :users)
          (loop
            for record in view
            for user-name = (getf record :name)
            for roles = (remove-if
                          (lambda (x)
                            (u:has
                              (list "logged-in" "settings"
                                (a:exclusive-role-for user-name))
                              x))
                          (a:list-user-role-names *rbac* user-name))
            collect (add-to-plist record (list :roles roles))))
        (t view)))))

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

(defun expand-value-tag (tag &key this user value)
  (case tag
    (:this this)
    (:user (a:get-id *rbac* "users" user))
    (:value value)
    (otherwise
      (report-e "expand-value-tag" "Unknown :write-to value tag ~s" ~tag))))

(defun identity-keys (type-key)
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    when (getf field-def :identity)
    collect field-key))

(defun execute-write-to (type-key field-key data uuid user)
  (let* ((m *compiled-model*)
          (write-to (u:tree-get m type-key :fields field-key :write-to))
          (to-type-key (getf write-to :table))
          (to-type-id-keys (identity-keys to-type-key))
          (search-pairs (loop
                          for k in write-to by #'cddr
                          for v in (cdr write-to) by #'cddr
                          when (u:has to-type-id-keys k)
                          append
                          (list k (expand-value-tag v :this uuid :user user))))
          (value-key (loop
                       for k in write-to by #'cddr
                       for v in (cdr write-to) by #'cddr
                       when (equal v :value)
                       do (return k)))
          (value (getf data value-key))
          (search-sql (u:tree-get m to-type-key :search-sql))
          (search-query (cons (car search-sql)
                          (loop for key in (cdr search-sql)
                            collect (getf search-pairs key))))
          (to-row-id (a:with-rbac (*rbac*) (a:rbac-query search-query :single)))
          (to-row-values (append
                           search-pairs
                           (list
                             value-key
                             (db-value to-type-key value-key user value)))))
    ;; Public pages (2026-11-07): both callers pass full-data, whose
    ;; absent-value spelling is the compiler-injected :null default —
    ;; a :null here means "no value supplied", and the write-through
    ;; is a no-op (before the db-value :null fix it errored, which
    ;; the callers logged and swallowed). An explicit nil (clear to
    ;; NULL) is a different spelling and still writes SQL NULL.
    (when (eq value :null)
      (return-from execute-write-to nil))
    (if to-row-id
      (let* ((to-row-values (append to-row-values (list :id to-row-id)))
              (update-sql (u:tree-get m to-type-key :update-sql :main))
              (update-query (cons (car update-sql)
                              (loop for key in (cdr update-sql)
                                collect (getf to-row-values key)))))
        (a:with-rbac (*rbac*) (a:rbac-query update-query)))
      (let* ((resource (make-resource-name to-type-key to-row-values))
              (insert-resource-sql (car (u:tree-get m to-type-key :insert-sql :resource)))
              (insert-resource-query (list insert-resource-sql resource))
              (to-row-id (a:with-rbac (*rbac*)
                           (a:rbac-query insert-resource-query :single))))
        (setf (getf to-row-values :id) to-row-id)
        (let* ((insert-row-sql (u:tree-get m to-type-key :insert-sql :main))
                (insert-row-query (cons (car insert-row-sql)
                                    (loop for key in (cdr insert-row-sql)
                                      collect (getf to-row-values key)))))
          (handler-case
            (a:with-rbac (*rbac*) (a:rbac-query insert-row-query))
            (error (e)
              ;; The resource row is inserted first because its id is
              ;; the main row's primary key. When the main insert
              ;; fails, remove the resource again so it does not leak
              ;; as an orphan (no transactions in MVP).
              (a:remove-resource *rbac* resource)
              (error e)))
          ;; Role links, mirroring insert-normal / update-roles: the
          ;; new related row inherits the source record's roles (the
          ;; resource list is the full role set as stored, including
          ;; "admin" and the creator's exclusive role). Without them
          ;; the new row is invisible to every user (user-read-type-ids
          ;; sees no roles), admin included. Inheriting the source
          ;; roles makes the related row exactly as visible as the
          ;; record it was created from.
          (let ((source-resource (id-to-resource-name uuid)))
            (loop
              for role in (a:list-resource-role-names *rbac* source-resource)
              do (a:add-resource-role *rbac* resource role))))))))

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
    ;; :has-all semantics: the tuple is field-scoped (listed type +
    ;; M2M field key), so the field must carry a :join-table. Shape
    ;; alone (filter-p) still accepts it — do not tighten the
    ;; predicate (chips 0a doctrine: shape in the predicate,
    ;; semantics in the validator).
    (when (eq op-key :has-all)
      (unless (u:tree-get *compiled-model* type-key :fields field-key
                :join-table)
        (report-ve "valid-filter"
          ":has-all targets an M2M list field: ~s on ~s has no ~
           :join-table." ~field-key ~type-key)))
    ;; :fk-eq semantics (chips 0a doctrine beside :has-all): the
    ;; field must be a singular-target FK on the tuple's own type
    ;; and the value a uuid. This arm completes the validation —
    ;; nothing after it re-examines the value (the generic
    ;; field-type / filter-value-type-p checks are skipped, so a
    ;; uuid survives a boolean/timestamp-declared FK).
    (when (eq op-key :fk-eq)
      (unless (singular-target-field-p
                (u:tree-get *compiled-model* type-key :fields field-key))
        (report-ve "valid-filter"
          ":fk-eq targets a singular :target field: ~s on ~s is ~
           not one (no :target, M2M :join-table, injected/base, or ~
           a non-FK column)." ~field-key ~type-key))
      (unless (uuid-p value)
        (report-ve "valid-filter"
          ":fk-eq takes a row uuid: ~s is not one (nil, :null, and ~
           display names are rejected)." ~value)))
    (unless (field-type-key-p
              (u:tree-get *compiled-model* type-key :fields field-key :type))
      (report-ve "valid-filter" "Invalid field type key for field ~s ~s ~s."
        ~type-key ~field-key ~type-key))
    (unless (filter-value-type-p type-key field-key op-key value)
      (let* ((field-type (u:tree-get *compiled-model*
                           type-key :fields field-key :type))
              (err (if (member op-key '(:in :not-in))
                     (eformat "
Invalid value type for field ~s ~s.
Expected a non-empty list of values of type ~s, but got value ~s."
                       type-key field-key field-type value)
                     (eformat "
Invalid value type for field ~s ~s.
Expected a value of type ~s, but got value ~s."
                       type-key field-key field-type value))))
        (report-ve "valid-filter" err)))))

(defun valid-filters (filters &key required (type-key nil type-key-p))
  ":private: Checks that FILTERS is valid. TYPE-KEY, when supplied,
is the queried (listed) type: an :fk-eq row whose tuple type differs
from it is field-scoped to the wrong type and is rejected (the
tuple-local valid-filter cannot see the queried type; ordinary
operators keep their cross-type rows)."
  (when (and required (null filters))
    (report-ve "valid-filters" "FILTERS cannot be NIL."))
  (loop for filter in filters
    for tuple-type-key = (when (listp filter) (car filter))
    for tuple-op-key = (when (listp filter) (caddr filter))
    do (valid-filter filter :required t)
    when (and type-key-p (eq tuple-op-key :fk-eq))
    unless (equal tuple-type-key type-key)
    do (report-ve "valid-filters"
         ":fk-eq is field-scoped to the listed type: tuple names ~s ~
          but the listed type is ~s." ~tuple-type-key ~type-key)))

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

(defun valid-non-rollup-type-key (type-key)
  ":private: Rejects computed, tableless shapes for every non-list
record endpoint (Issue 9): rollups (:phase-a-shape :measure —
read-only computed views) and pages (:phase-a-shape :page —
read-only projections of one content row). Only list reads reach
them (be-list; be-list-column allows rollups but carries its own
page guard). Guards live in the be-* functions so REST and REPL
share them; rest.lisp does not grow a parallel type check."
  (let ((shape (u:tree-get *compiled-model* type-key :phase-a-shape)))
    (when (eq shape :measure)
      (report-ve "valid-non-rollup-type-key"
        "Rollup type ~s is read-only; only list reads (/api/list, ~
         /api/column) are supported on it."
        ~type-key))
    (when (eq shape :page)
      (report-ve "valid-non-rollup-type-key"
        "Page type ~s is a read-only projection; only list reads ~
         (/api/list) are supported on it."
        ~type-key))))

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
      ((and
         (not (getf field-def :base))
         (getf field-def :column))
        ;; NIL is valid for nullable fields and for password fields
        ;; on update (passwords are blanked before reaching the
        ;; frontend; NIL means "keep existing hash").
        (unless (and (null value)
                     (or (not (getf field-def :not-null))
                         (eq (getf field-def :type) :password)))
          (valid-value-type type-key field-key value)))
      ((equal field-key :id)
        (valid-uuid value))
      ((and
         (not (getf field-def :base))
         (getf field-def :write-to)
         (not (getf field-def :column)))
        ;; Virtual write-to field — skip validation, handled by execute-write-to
        nil)
      ((and
         (not (getf field-def :base))
         (not (getf field-def :column))
         (not (getf field-def :write-to))
         (not (getf field-def :join-table)))
        ;; Pure computed/read-only field — no column, no write-to.
        ;; Value comes from view aggregation, not from user input.
        nil)
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
    for new-values = (or (getf data key)
                       (u:tree-get m type-key :fields key :default))
    for unknown-values = (unknown-values type-key key new-values)
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

(defun exclusive-role-p (role)
  ":private: Returns t if ROLE is any exclusive role
(\"name:exclusive\"), including \"admin:exclusive\" and
\"guest:exclusive\"."
  (and (stringp role)
       (u:ends-with role ":exclusive")))

(defun shareable-exclusive-role-p (role)
  ":private: Returns t if ROLE is a user-exclusive role that can be
assigned by any user for point-to-point sharing. Excludes
\"admin:exclusive\" and \"guest:exclusive\"."
  (and (exclusive-role-p role)
       (not (member role '("admin:exclusive" "guest:exclusive")
                    :test 'equal))))

(defun valid-user-roles (user roles)
  ":private: Returns nil (success) if every role in ROLES exists and either
USER is 'admin', USER has every role in ROLES, or the role is another
user's exclusive role (for point-to-point sharing). Otherwise, raises a
validation error."
  (valid-existing-user user)
  (valid-existing-roles roles)
  (let* ((user-roles (a:list-user-role-names *rbac* user))
         (non-user-roles (if (equal user "admin")
                           nil
                           (remove-if
                             (lambda (r)
                               (or (member r user-roles :test 'equal)
                                   (shareable-exclusive-role-p r)))
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
  (valid-non-rollup-type-key type-key)
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
  (valid-non-rollup-type-key type-key)
  (when (and logical-path (not file-token))
    ;; A directory path names a directory when it ends in / (the
    ;; same contract as u:path-parent: a directory path must end
    ;; in /, and file parents are looked up with the trailing
    ;; slash). A slash-less name like "/gallery" slips through the
    ;; parent checks below, but no file can ever be stored under
    ;; it: check-file computes the parent of "/gallery/x.png" as
    ;; "/gallery/" and finds no such row — and store-directory,
    ;; handed a slash-less path, creates only the parent. Reject
    ;; the shape at insert time instead.
    (unless (u:ends-with logical-path "/")
      (report-ve "valid-new-directory"
        "Directory path must end with a slash: ~a" ~logical-path))
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
  - Parent roles: ~{~a~^, ~}
  - Requested roles: ~{~a~^, ~}"
              logical-path ~logical-parent ~parent-roles ~non-parent-roles
              req-roles user-roles))
          (when non-user-roles
            (report-ve "validate-new-directory"
              "
Can't assign roles that user doesn't have.
  - User: ~a
  - User roles: ~{~a~^, ~}
  - Requested roles: ~{~a~^, ~}"
              ~user ~user-roles ~non-user-roles req-roles
              parent-roles)))))))

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

(defun valid-updated-directory (type-key record data)
  ":private: Fs-backed path fields are immutable until moves are
supported. Rejects any be-update that changes a :path field's value
on a tree/fs-backed type; an unchanged value passes. Applies to both
leaf (file) and non-leaf (directory) types — neither has a move
implementation, and both desynchronize row / resource / filesystem
when renamed."
  (let* ((path-field (path-field type-key))
         (old-path (when path-field (getf record path-field)))
         (new-path (when path-field (getf data path-field))))
    (when (and old-path new-path (not (equal old-path new-path)))
      (report-ve "valid-updated-directory"
        "Path cannot be changed (moves are not supported): ~a -> ~a"
        ~new-path ~old-path))))

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
  (valid-non-rollup-type-key type-key)
  (valid-existing-user user)
  (if (stringp filters)
    (valid-uuid filters)
    (valid-filters filters :required t :type-key type-key))
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
          (result (view-result-values type-key field-keys view-result
                   :user user)))
    (cond
      ((zerop (length result)) nil)
      ((> (length result) 1) (error "More than one match."))
      (t (let ((id (getf (car result) :id)))
           (when (user-allowed-resource user id "read") id))))))

(defun be-value-id (type-key field-key value user)
  ":public: Returns the ID of the record of type TYPE-KEY where FIELD-KEY has
the value VALUE."
  (valid-compiled-model)
  (valid-be-type-key type-key)
  (valid-non-rollup-type-key type-key)
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
lookup. The :record plist carries :update / :delete keyword flags computed from
USER's permissions on this row's own resource (the /api/item arm; rec stays
clean — internal callers feed its :record back through valid-data, which would
reject the injected keys).
Record permalinks: be-rec also decorates the record with a
\"<field>-id\" UUID sibling for each requested singular-target
field (omitted when the FK is null or suppression-marked) —
the public read surface the View-form FK link consumes.
Embedded child regions (plan 02): on :view-form fetches of a type
that declares :children, be-rec attaches the emitted region spec
beside :fields (region-spec-list — :type / :via as strings, :sort
as a two-element string pair, the json-sort-echo trap avoided).
Triple-guarded: inside the record guard, only for :view-form, and
only when the compiled :view-form declares :children — no other
form, no nil-valued key on childless types. rec's other four
callers (be-update, be-val, be-delete, be-action) never see it."
  (valid-compiled-model)
  (valid-non-rollup-type-key (if type-key type-key (id-to-type-key id)))
  (multiple-value-bind (result rows)
    (rec id user :form form :type-key type-key :public t)
    (let* ((record (getf result :record))
           (record-type (or type-key (getf result :type))))
      (when record
        (setf (getf result :record)
          (car (decorate-records-with-caps record-type
                 (list
                   (append
                     (if (listp record) record (list record))
                     (decorate-record-with-target-ids record-type
                       form
                       (form-field-keys record-type form)
                       rows user record)))
                 user)))
        ;; Public pages: :public-url on the wire record (fresh guest
        ;; check; omitted, never nil). A forward call into
        ;; public-pages.lisp — the start-web-server precedent.
        (setf (getf result :record)
          (decorate-public-url (getf result :record) record-type))
        (when (and (eq form :view-form) record-type
                (u:has (u:plist-keys
                         (u:tree-get *compiled-model* record-type
                           :view-form))
                  :children))
          (setf (getf result :children)
            (region-spec-list
              (u:tree-get *compiled-model* record-type
                :view-form :children)))))
      result)))

(defun be-val (id field-key user &key (form :update-form) type-key)
  ":public: Returns the value for field FIELD-KEY in the TYPE-KEY record with
ID. Providing TYPE-KEY is optional, but helps the function avoid an extra
database query."
  (valid-compiled-model)
  (valid-existing-uuid id)
  (valid-existing-user user)
  (let ((type-key (or type-key (id-to-type-key id))))
    (valid-be-type-key type-key)
    (valid-non-rollup-type-key type-key)
    (valid-field-key type-key field-key)
    (user-allowed-resource user id "read")
    (let ((record (rec id user :form form :type-key type-key)))
      (u:tree-get record :record field-key))))

;;;
;;; Phase A: ID selection with paging, sorting, and filtering.
;;;
;;; Phase A selects a page of primary IDs using fast indexed queries on base
;;; columns (WHERE / ORDER BY / LIMIT / OFFSET). Join-table filters are also in
;;; Phase A via JOIN + WHERE so that paging is honest. A sort on a :target
;;; field also runs on the join SQL and orders by the target's display column.
;;; Phase B then hydrates only those IDs.
;;;
;;; See docs/todo.org item 2 and workbench/list-plan/02-id-first-phase-a.org.

(defparameter +max-limit+ 200
  "Server-side maximum for the LIMIT clause in be-list.")

(defun clamp-limit (limit)
  ":private: Clamp LIMIT to the server-side maximum. Returns nil when LIMIT is
nil (meaning no limit / all rows). Otherwise clamps to [1, +max-limit+]."
  (when limit
    (min (max limit 1) +max-limit+)))

(defun phase-a-target-sort-p (type-key sort)
  ":private: The :target type key when SORT names a :target (FK) field, else
NIL. Such a sort orders by the target's display column, which lives on a joined
table (the joined-column sort)."
  (when sort
    (u:tree-get *compiled-model* type-key :fields (car sort)
      :target)))

(defun phase-a-join-required-p (type-key filters sort)
  ":private: True when Phase A must run on the join SQL: a filter references a
joined table, or SORT names a :target field."
  (or (filters-require-join-p type-key filters)
    (phase-a-target-sort-p type-key sort)))

(defun phase-a-select-sql (type-key filters sort)
  ":private: Returns the base SQL string for Phase A: the simple SELECT id FROM
<table>, or the SELECT DISTINCT ... JOIN version when filters reference joined
tables or SORT names a :target field (the joined-column sort)."
  (let ((m *compiled-model*))
    (if (phase-a-join-required-p type-key filters sort)
      (u:tree-get m type-key :views :main :phase-a-join-sql)
      (u:tree-get m type-key :views :main :phase-a-base-sql))))

(defun phase-a-order-by-column (type-key sort)
  ":private: Returns the table-qualified column name for the sort field, or nil
if SORT is nil. Validates that the field is marked :sortable t."
  (when sort
    (let* ((field-key (car sort))
           (field-def (u:tree-get *compiled-model* type-key :fields field-key)))
      (unless field-def
        (report-ve "phase-a-order-by-column"
          "Cannot sort on unknown field ~s for type ~s."
          ~field-key ~type-key))
      (unless (getf field-def :sortable)
        (report-ve "phase-a-order-by-column"
          "Field ~s is not sortable for type ~s."
          ~field-key ~type-key))
      (u:tree-get field-def :source :column-name))))

(defun phase-a-build-order-by (type-key sort)
  ":private: Returns an ORDER BY clause string for the Phase A query. SORT is
nil or a plist (:field-key :asc|:desc). Defaults to ORDER BY id when SORT is
nil. Sorted queries append the table-qualified primary key as a tiebreaker so
paging (LIMIT/OFFSET) is stable among tied rows."
  (if sort
    (let* ((table (u:tree-get *compiled-model* type-key :table-name))
           (column (phase-a-order-by-column type-key sort))
            (direction (or (cadr sort) :asc)))
      (unless (member direction '(:asc :desc))
        (report-ve "phase-a-build-order-by"
          "Invalid sort direction ~s. Use :asc or :desc." ~direction))
      (format nil " order by ~a ~a, ~a.id" column direction table))
    (let ((table (u:tree-get *compiled-model* type-key :table-name)))
      (format nil " order by ~a.id" table))))

(defun phase-a-build-count-sql (base-sql where-result)
  ":private: Transforms the Phase A base SQL + WHERE into a COUNT query.
Replaces the SELECT clause with COUNT(*) or COUNT(DISTINCT ...) depending on
whether the base SQL contains a JOIN."
  (let* ((where-sql (car where-result))
          (where-params (cdr where-result))
          (has-join-p (search " join " base-sql :test #'char-equal))
          (table (if has-join-p
                   (let ((distinct-pos (search "select distinct "
                                         base-sql :test #'char-equal)))
                     (subseq base-sql
                       (length "select distinct ")
                       (search " from" base-sql :test #'char-equal)))
                   (subseq base-sql
                     (length "select ")
                     (search " from" base-sql :test #'char-equal))))
          (count-expr (if has-join-p
                        (format nil "count(distinct ~a)" table)
                        "count(*)"))
          ;; Extract the FROM ... portion from the WHERE-augmented SQL.
          ;; The WHERE result is base-sql + "where ..." appended.
          (from-pos (search " from" where-sql :test #'char-equal))
          (from-portion (subseq where-sql from-pos))
          (count-sql (format nil "select ~a ~a" count-expr from-portion)))
    (cons count-sql where-params)))

(defun escape-ilike-pattern (term)
  ":private: Escape LIKE metacharacters in TERM for use with
ILIKE ... ESCAPE '\\'. Escapes backslash, then %, then _. Does not wrap
in %...%; the caller does."
  (with-output-to-string (out)
    (loop for c across (or term "")
      do (case c
           (#\\ (write-string "\\\\" out))
           (#\% (write-string "\\%" out))
           (#\_ (write-string "\\_" out))
           (t (write-char c out))))))

(defun phase-a-search-clause (type-key search start-index)
  ":private: Build a parenthesized OR ILIKE group for :search.
Returns nil when SEARCH is blank/whitespace-only. Otherwise returns
(cons fragment pattern) where FRAGMENT is

  \"(col1 ILIKE $N ESCAPE '\\\\' OR col2 ILIKE $N ESCAPE '\\\\')\"

and PATTERN is the escaped term wrapped in %...%. One bind param is reused.

Invariant: FRAGMENT never contains the substring \" from\"
(phase-a-build-count-sql slices where-sql at \" from\"). No search index:
leading % defeats btree.

Non-blank SEARCH on a type with zero :searchable-fields signals report-ve."
  (let ((trimmed (when search (u:trim search))))
    (when (and trimmed (plusp (length trimmed)))
      (let ((cols (u:tree-get *compiled-model* type-key :searchable-fields)))
        (unless cols
          (report-ve "phase-a-search-clause"
            "Type ~s has no searchable fields." ~type-key))
        (let* ((pattern (format nil "%~a%" (escape-ilike-pattern trimmed)))
                (preds (mapcar
                         (lambda (col)
                           (format nil "~a ILIKE $~d ESCAPE '\\'"
                             col start-index))
                         cols))
                (fragment (format nil "(~{~a~^ OR ~})" preds)))
          (cons fragment pattern))))))

(defun add-page-clause (sql params limit offset)
  ":private: Append LIMIT/OFFSET placeholders to SQL when LIMIT is non-nil.
Returns the full query (cons sql params). With LIMIT nil the query is
returned unchanged (no limit / all rows). Both be-list Phase A branches
(base and measure) compose their SQL with this helper; 09 composes the
measure ORDER BY here too. OFFSET defaults to 0 when nil: callers may
thread an unsupplied offset down (e.g. be-list-column), and a bare nil
param would reach Postgres as 'false'."
  (if limit
    (cons
      (format nil "~a~%limit $~a offset $~a"
        sql (next-param-index sql) (1+ (next-param-index sql)))
      (append params (list limit (or offset 0))))
    (cons sql params)))

(defun phase-a-query (type-key filters sort limit offset user
                       &key search)
  ":private: Build and execute Phase A: select a page of primary IDs.
Returns (values ids total).

FILTERS is the full filter list (user filters + RBAC ids + scope).
SORT is nil or (:field-key :asc|:desc).
LIMIT is nil (no limit) or an integer (already clamped).
OFFSET is an integer.
SEARCH is nil or a non-blank string matched with ILIKE against the type's
:searchable-fields (OR group), before paging."
  (let* ((base-sql (phase-a-select-sql type-key filters sort))
          (where-result (add-where-clause base-sql filters user))
          (where-sql (car where-result))
          (where-params (cdr where-result))
          ;; Splice search OR-group into WHERE before ORDER BY / LIMIT / count.
          (search-clause (phase-a-search-clause type-key search
                           (next-param-index where-sql)))
          (where-sql
            (if search-clause
              (if (string= base-sql where-sql)
                (format nil "~a~%where~%  ~a~%" where-sql (car search-clause))
                (format nil "~a  and ~a~%" where-sql (car search-clause)))
              where-sql))
          (where-params
            (if search-clause
              (append where-params (list (cdr search-clause)))
              where-params))
          (where-result (cons where-sql where-params))
          (order-by (phase-a-build-order-by type-key sort))
          ;; Build page SQL: ORDER BY always; LIMIT/OFFSET only when limit is
          ;; non-nil (08b: add-page-clause, shared with the measure branch).
          ;; When Phase A runs on the join SQL (join-filters or a :target sort)
          ;; and a sort is present, splice the sort column into the page
          ;; projection: Postgres requires DISTINCT ORDER BY expressions to
          ;; appear in the select list. The (id, sort-col) pair still dedups
          ;; exactly. The sort column is functionally determined by id: a
          ;; base-table column, or the display column of a :target field joined
          ;; through that field's own FK (valid-sortable-target-fields
          ;; guarantees the edge). The count query below is fed the pre-splice
          ;; strings, so :total never sees the extra column. Query in default
          ;; plist mode and extract (getf row :id).
          (page-sql
            (let* ((spliced-p (and
                                (phase-a-join-required-p type-key filters sort)
                                sort))
                    (sort-col (when spliced-p
                                (phase-a-order-by-column type-key sort)))
                    (page-base (if spliced-p
                                 (format nil "select distinct ~a.id, ~a~a"
                                   (u:tree-get *compiled-model* type-key
                                     :table-name)
                                   sort-col
                                   (subseq where-sql
                                     (search " from" where-sql
                                       :test #'char-equal)))
                                 where-sql)))
              (declare (ignore spliced-p))
              (format nil "~a~a" page-base order-by)))
          (page-query (add-page-clause page-sql
                        where-params limit offset))
          ;; Count query: same WHERE/JOIN, no ORDER BY/LIMIT/OFFSET.
          (count-result (phase-a-build-count-sql base-sql where-result))
          (count-query count-result))
    (pl:pdebug :in "phase-a-query"
      :base-sql base-sql
      :page-sql (car page-query)
      :count-sql (car count-query))
    (let ((raw-rows (a:with-rbac (*rbac*)
                      (a:rbac-query page-query)))
           (total (a:with-rbac (*rbac*)
                    (a:rbac-query count-query :single))))
      (values (mapcar (lambda (row) (getf row :id)) raw-rows)
        total))))

;;;
;;; be-list - Phase A -> Phase B flow.
;;;
;;; Phase A selects a page of primary IDs (fast, indexed, with join-filter
;;; pushdown). Phase B hydrates only those IDs via the view-result path. All
;;; filtering happens in Phase A; Phase B does not re-apply filters.
;;;

(defun reorder-to-match (records ids)
  ":private: Reorder RECORDS (plists with :ID) to match the order of IDS.
Phase B (hydration) does not preserve the order from Phase A; this function
restores it."
  (loop for id in ids
    for record = (find id records :key (lambda (r) (getf r :id)) :test #'equal)
    when record collect record))

(defun phase-b-hydrate (type-key ids user &key 
                         form field-keys total skip-allowed-values
                         sort)
  ":private: Hydrate and collapse the given ID set.

Runs the existing join query with WHERE id IN (ids), collapses fan-out via
aggregate-values, and returns the list-result plist. No filters are applied
here; all filtering happened in Phase A. The only WHERE condition is id IN
(phase-a-ids).

Record order is restored to match the Phase A ID order via reorder-to-match,
because the WHERE id IN (...) query does not guarantee row ordering in
PostgreSQL."
  (let* ((m *compiled-model*)
          (sql (u:tree-get m type-key :views :main :sql))
          (ids-query (add-ids-clause type-key sql ids))
          (view-result (view-result type-key ids-query))
          (result (list-result type-key user form
                    :field-keys field-keys
                    :view-result view-result
                    :total total
                    :sort sort
                    :skip-allowed-values skip-allowed-values)))
    (setf (getf result :records)
      (reorder-to-match (getf result :records) ids))
    result))

(defun be-list (type-key user
                 &key (form :list-form) filters
                 (limit 20) (offset 0) sort
                 (search nil)
                 skip-allowed-values)
  ":public: Returns a list of records of TYPE-KEY that match FILTERS and that
USER has `read` permissions for. Each record is returned as a plist, where the
keys are field keys and the values are the corresponding field values.

Supports paging via LIMIT (default 20, max 200) and OFFSET (default 0).
SORT is nil or a plist (:field-key :asc|:desc). When SORT is nil, the
type's :default-sort declaration (if any) applies.
SEARCH is nil or a string; matched with ILIKE against :searchable fields.

The following example returns a list of all the :todos records that have the tag
'chores' and that the user 'admin' has permission to read:

    `(be-list :todos \"admin\" :filters '((:tags :name :eq \"chores\")))`"
  (valid-compiled-model)
  (valid-be-type-key type-key)
  (let* ((shape (u:tree-get *compiled-model* type-key :phase-a-shape))
         (measure-p (eq shape :measure)))
    ;; Authorization (the API type-roles access fix): rollups (no
    ;; per-row resources), built-in types (their system contracts),
    ;; and the Page projection shape keep the type-resource read
    ;; gate. An ordinary author-defined resource-backed type needs
    ;; only an authenticated existing user — base-be-list's
    ;; user-read-type-ids filtering then decides which rows are
    ;; visible, so a row-level grant is reachable without holding
    ;; any of the type's roles.
    (if (or measure-p (built-in-p type-key) (eq shape :page))
      (valid-user-permissions user type-key "read")
      (valid-existing-user user))
    ;; Validate search early so empty-rbac short-circuit still 400s on
    ;; non-empty search against a type with zero searchable fields.
    ;; Rollups compile to zero searchable fields, so this is the 07d
    ;; reject on the measure path too.
    (phase-a-search-clause type-key search 1)
    (let ((sort (or sort
                    ;; A :list-form :default-sort (the blog tab spelling)
                    ;; is the effective default too; type-level wins when
                    ;; both exist (the compiled def carries it).
                    (or (u:tree-get *compiled-model* type-key :default-sort)
                        (u:tree-get *compiled-model* type-key :list-form
                          :default-sort)))))
      (if measure-p
        ;; 08b measure Phase A: GROUP BY grain, no Phase B, no
        ;; user-read-type-ids / filters-for-be-list (the rollup has no
        ;; per-row resources; the type-level role check above already
        ;; ran on the rollup). The bare-UUID rewrite also lives here:
        ;; valid-existing-uuid looks at resources / base tables and
        ;; would 400 an unknown grain id.
        (measure-be-list type-key user :form form :filters filters
          :limit limit :offset offset :sort sort
          :skip-allowed-values skip-allowed-values)
        (if (eq shape :page)
          ;; Page projection: the same authorization arm as a rollup
          ;; (the type-resource read above) and the same no-filter
          ;; discipline — a page's row selection is model-declared.
          ;; Search was already rejected by phase-a-search-clause
          ;; above (page fields are not :searchable); limit / offset
          ;; are ignored (a page is not a page of a list, and under
          ;; the identity-pin compile rule paging cannot hide a
          ;; duplicate).
          (page-be-list type-key user :form form :filters filters
            :sort sort
            :skip-allowed-values skip-allowed-values)
          (progn
            (if (uuid-p filters)
              (valid-existing-uuid filters)
              (valid-filters filters :type-key type-key))
            (base-be-list type-key user :form form :filters filters
              :limit limit :offset offset :sort sort :search search
              :skip-allowed-values skip-allowed-values)))))))

(defun base-be-list (type-key user
                      &key (form :list-form) filters
                      (limit 20) (offset 0) sort
                      (search nil)
                      skip-allowed-values)
  ":private: Base-shape be-list body (Phase A id select + Phase B hydrate).
Validations have already run in BE-LIST."
  (let* ((m *compiled-model*)
          (user-roles (a:list-user-role-names *rbac* user))
          (type-roles (u:tree-get m type-key :type-roles))
          (page-limit (clamp-limit limit))
          (rbac-ids (if (base-resource-type-key-p type-key)
                      (when (u:has-some user-roles type-roles)
                        (let ((table (table-name type-key)))
                          (a:with-rbac (*rbac*)
                            (db:query (format nil "select id from ~a" table)
                              :column))))
                      (user-read-type-ids user type-key))))
    (pl:pdebug :in "be-list" :filters filters :limit page-limit :offset offset)
    (if rbac-ids
      (let* ((all-filters (filters-for-be-list type-key rbac-ids filters user))
              (field-keys (form-field-keys type-key form)))
        ;; Phase A: select a page of IDs + total count.
        (multiple-value-bind (ids total)
          (phase-a-query type-key all-filters sort page-limit offset user
            :search search)
          (if ids
            ;; Phase B: hydrate only the page IDs.
            (phase-b-hydrate type-key ids user
              :form form :field-keys field-keys
              :total total :sort sort
              :skip-allowed-values skip-allowed-values)
            ;; Empty page — return empty result with total count.
            (list-result type-key user form
              :field-keys field-keys
              :total total
              :sort sort
              :skip-allowed-values skip-allowed-values))))
      (list-result type-key user form
        :sort sort
        :skip-allowed-values skip-allowed-values))))

(defun measure-row-value (type-key field-key value)
  ":private: Convert one raw measure-Phase-A row value for FIELD-KEY to its
JSON-ready form (08b Step 6.6). The SQL aliases are the field keys, so no
alias mapping happens here — only value conversion:

  - :avg → keep :NULL as :null (JSON null; plist-to-json renders nil as
    []); coerce non-null PG numeric (a Lisp ratio) to a float, like
    Phase B's aggregate-values does.
  - :list / :distinct → array_agg vectors to Lisp lists (nil when
    empty).
  - boolean pass-throughs → SQL t → :true, SQL nil → :false (never
    bare nil; plist-to-json renders nil as []). :null stays :null."
  (let* ((fields (u:tree-get *compiled-model* type-key :fields))
         (field-def (getf fields field-key))
         (agg (u:tree-get field-def :source :agg))
         (field-type (getf field-def :type)))
    (cond
      ((eq agg :avg)
        (if (or (null value) (eq value :null))
          :null
          (float value)))
      ((member agg '(:list :distinct))
        (etypecase value
          (null nil)
          (vector (coerce value 'list))
          (list value)))
      ((and (eq field-type :boolean) (eq agg :first))
        (cond
          ((eq value :null) :null)
          (value :true)
          (t :false)))
      (t value))))

(defun measure-row-to-plist (type-key row)
  ":private: Convert one raw GROUP BY row (SQL aliases are field keys)
to a field-key plist with JSON-ready values."
  (loop for key in row by #'cddr
    for value in (cdr row) by #'cddr
    append (list key (measure-row-value type-key key value))))

(defun measure-grain-column-filter (type-key filter)
  ":private: Classifier for one request-time filter tuple on a rollup
(07c / Issue 14). Returns the resolved grain column (qualified
<table>.<column> SQL string) when the tuple is a grain-column filter —
table-key equals the listed type AND field-key is the injected :id or an
author pass-through (:agg :first). Returns NIL otherwise; the caller
report-ve's anything that is not a grain-column filter."
  (destructuring-bind (table-key field-key op-key value) filter
    (declare (ignore op-key value))
    (when (eq table-key type-key)
      (let* ((fields (u:tree-get *compiled-model* type-key :fields))
             (field-def (getf fields field-key))
             (agg (u:tree-get field-def :source :agg)))
        (when (and (eq agg :first) field-def)
          (u:tree-get field-def :source :column-name))))))

(defun measure-scope-predicate (type-key user)
  ":private: Runtime :scope :user predicate for a rollup (08b Step 5).
Grain :users → <grain>.id = <users.id UUID>. Grain otherwise → the
grain's :user column, join-free. The bind value depends on how the
author declared that :user field: with :target :users (the common
case — compile-field makes the column a UUID FK storing users.id) bind
the UUID; a plain text :user column stores the username string, so
bind the username. Returns two values: the SQL fragment and its
single bind parameter, or NIL / NIL when the rollup does not declare
:scope :user."
  (let* ((m *compiled-model*)
         (scope (u:tree-get m type-key :views :main :scope)))
    (when (eq scope :user)
      (let* ((grain (u:tree-get m type-key :grain))
             (grain-table (u:tree-get m grain :table-name)))
        (if (eq grain :users)
          (values (format nil "~a.id = $1" grain-table)
            (a:get-id *rbac* "users" user))
          (let* ((user-def (u:tree-get m grain :fields :user))
                 (user-col (getf user-def :name-sql))
                 (fk-p (getf user-def :target)))
            (values (format nil "~a.~a = $1" grain-table user-col)
              (if (eq fk-p :users)
                (a:get-id *rbac* "users" user)
                user))))))))

(defun measure-request-grain-filters (type-key filters user
                                       &key (start-at 1))
  ":private: Classify request-time FILTERS for a rollup (07c option a /
Issue 14). Rewrites a bare UUID string to a grain-id tuple first (unknown
id → empty page later, not valid-existing-uuid). Returns (values
conditions params) where CONDITIONS are parameterized WHERE fragments on
the grain table. Anything that is not a grain-column filter is
report-ve'd — one error class, never a silent ignore, never a global
WHERE on a joined table."
  (let* ((norm (if (uuid-p filters)
                 (list (list type-key :id :eq filters))
                 filters)))
    (loop
      with conditions = nil
      with params = nil
      for filter in norm
      do
      (valid-filter filter :required t)
      (destructuring-bind (table-key field-key op-key value) filter
        (let ((column (measure-grain-column-filter type-key filter))
              ;; :fk-eq is rejected before both the generic
              ;; non-grain-column rejection and the operator-sql
              ;; splice below (the inert token must never reach the
              ;; splice). A rollup-self tuple already died in
              ;; valid-filter's singular-target check; this arm is
              ;; the belt for tuple-local-valid cross-type FK rows.
              (fk-eq-p (eq op-key :fk-eq))
              ;; nil :eq / :ne → is [not] null with no placeholder and
              ;; no param appended — the same translation as
              ;; add-where-clause; the index below counts
              ;; (length params), so appending nothing keeps both
              ;; sides of the numbering in sync.
              (null-eq-p (and (member op-key '(:eq :ne))
                            (or (null value) (equal value :null)))))
          (when fk-eq-p
            (report-ve "measure-request-grain-filters"
              ":fk-eq is not supported on rollups; a rollup row has ~
               no storage FK." ~table-key ~field-key))
          (unless column
            (report-ve "measure-request-grain-filters"
              "Filter (~s ~s ~s ...) is not a grain-column filter on ~
               rollup ~s; only listed-type pass-throughs and :id can be ~
               filtered (measure fields, fact tables, and unexposed ~
               grain columns are not supported on rollups)."
              ~table-key ~field-key ~op-key ~type-key))
          (if null-eq-p
            (push (format nil "~a is ~:[not ~;~]null" column
                    (eq op-key :eq))
              conditions)
            (let* ((index (+ start-at (length params)))
                   (op (operator-sql op-key))
                   (placeholders
                     (if (member op-key '(:in :not-in :has-all))
                       (format nil "(~{~a~^, ~})"
                         (placeholders value :start-at index))
                       (format nil "$~d" index))))
              (push (format nil "~a ~a ~a" column op placeholders)
                conditions)
              (setf params (append params
                            (if (member op-key '(:in :not-in :has-all))
                              (mapcar
                                (lambda (v)
                                  (db-value type-key field-key user v))
                                value)
                              (list
                                (db-value type-key field-key user
                                  value)))))))))
      finally
      (return (values (nreverse conditions) params)))))

(defun measure-default-sort (type-key)
  ":private: Default user-facing sort for a rollup when neither a request sort
nor a :default-sort declaration fills it (plan 09 Step 4): the first
sortable /measure/ field (declaration order), :desc, plus the grain-id
tiebreaker appended by the caller. Returns nil when the rollup declares no
sortable measure (caller falls back to grain id ASC only, and the response :sort
is nil). Pass-throughs (:agg :first) and :list / :distinct measures never
qualify (compile-time rule)."
  (loop
    with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for agg = (u:tree-get field-def :source :agg)
    when (and (getf field-def :sortable)
           (not (eq agg :first)))
    do (return (list field-key :desc))
    finally (return nil)))

(defun valid-measure-sort (type-key sort)
  ":private: Validate a request SORT plist for a rollup. Returns SORT. Unknown
field keys and non-sortable fields report-ve, same error class as the base
path (phase-a-order-by-column). Compile-time rules already keep :sortable t off
:list / :distinct measures and hybrids; this is the belt over live compiled
defs."
  (when sort
    (let* ((field-key (car sort))
            (field-def (u:tree-get *compiled-model* type-key
                         :fields field-key)))
      (unless field-def
        (report-ve "valid-measure-sort"
          "Cannot sort on unknown field ~s for type ~s."
          ~field-key ~type-key))
      (unless (getf field-def :sortable)
        (report-ve "valid-measure-sort"
          "Field ~s is not sortable for type ~s."
          ~field-key ~type-key))
      sort)))

(defun resolve-measure-sort-column (type-key field-key)
  ":private: Resolve a sort field key to its SELECT alias in measure Phase A
(plan 09 Step 2). Pass-throughs and measures both alias as
(to-sql-identifier field-key), the injected :id aliases as id, never grain_id.
Deliberately not :source :column-name (the fact column, not in GROUP BY) and not
:name-sql (the rollup field's invented name).  Postgres allows ORDER BY on
output aliases of aggregate expressions."
  (declare (ignore type-key))
  (to-sql-identifier field-key))

(defun measure-order-by-sql (type-key grain-table sort)
  ":private: ORDER BY clause for measure Phase A (plan 09 Step 3 / Step 4). SORT
is nil or a validated (:field-key :asc|:desc) plist.  Every measure ORDER BY
appends the grain PK ASC as a tiebreaker so page boundaries stay stable across
requests (same carve as plan 06 Step 1b). :avg measures append NULLS LAST so
zero-fact rows trail (Issue 13); :sum / :count are coalesced / naturally 0 and
do not."
  (let* ((effective (or sort (measure-default-sort type-key))))
    (if effective
      (let* ((field-key (first effective))
              (direction (or (second effective) :asc)))
        (unless (member direction '(:asc :desc))
          (report-ve "measure-order-by-sql"
            "Invalid sort direction ~s. Use :asc or :desc." ~direction))
        (let* ((alias (resolve-measure-sort-column type-key field-key))
                (agg (u:tree-get *compiled-model* type-key :fields
                       field-key :source :agg))
                (nulls (when (eq agg :avg) " nulls last")))
          (format nil "order by ~a ~(~a~)~a, ~a.id"
            alias direction (or nulls "") grain-table)))
      (format nil "order by ~a.id" grain-table))))

(defun measure-effective-sort (type-key sort)
  ":private: The user-facing effective sort for the :sort echo (plan
09 Step 5): the validated request sort, or the default first-sortable
measure when the request sent none, or nil. Never the grain-id
tiebreaker."
  (or (valid-measure-sort type-key sort)
    (measure-default-sort type-key)))

(defun measure-be-list (type-key user &key (form :list-form)
                         filters (limit 20) (offset 0) sort
                         skip-allowed-values)
  ":private: Measure-shape be-list body (08b Step 6): assemble the main
GROUP BY query and the standalone count query from the compiled parts,
splicing runtime WHERE predicates (compiled grain filters + :scope :user
+ request-time grain filters) between the SELECT part and the GROUP BY
part. No user-read-type-ids, no id IN (...), no Phase B. ORDER BY is
plan 09: the SELECT alias of the requested / default sortable field
(never :source :column-name / :name-sql), grain PK ASC tiebreaker,
NULLS LAST on :avg."
  (let* ((m *compiled-model*)
         (view (u:tree-get m type-key :views :main))
         (select-sql (getf view :measure-phase-a-select))
         (group-by-sql (getf view :measure-phase-a-group-by))
         (count-select-sql (getf view :measure-phase-a-count-select))
         (grain-where (getf view :measure-phase-a-grain-where))
         (grain (u:tree-get m type-key :grain))
         (grain-table (u:tree-get m grain :table-name))
         (page-limit (clamp-limit limit)))
    (multiple-value-bind (scope-sql scope-param)
      (measure-scope-predicate type-key user)
      (multiple-value-bind (req-conds req-params)
        (measure-request-grain-filters type-key filters user
          :start-at (if scope-sql 2 1))
        (let* ((predicates (append (mapcar
                                     (lambda (frag)
                                       (format nil "(~a)" frag))
                                     grain-where)
                            (when scope-sql (list scope-sql))
                            req-conds))
               (params (append (when scope-param (list scope-param))
                         req-params))
               (where-sql
                 (if predicates
                   (format nil "~a~%where~%  ~{~a~^~%  and ~}~%"
                     select-sql predicates)
                   select-sql))
               (order-by (measure-order-by-sql type-key grain-table
                           (valid-measure-sort type-key sort)))
               (page-query (add-page-clause
                              (format nil "~a~%~a~%~a"
                                where-sql group-by-sql order-by)
                              params page-limit offset))
               (count-query
                 (if predicates
                   (cons
                     (format nil "~a~%where~%  ~{~a~^~%  and ~}~%"
                       count-select-sql predicates)
                     params)
                   (cons count-select-sql nil))))
          (pl:pdebug :in "measure-be-list"
            :page-sql (car page-query)
            :count-sql (car count-query))
          (let* ((rows (a:with-rbac (*rbac*) (a:rbac-query page-query)))
                 (total (a:with-rbac (*rbac*)
                          (a:rbac-query count-query :single))))
            (list-result type-key user form
              :total total
              :sort (measure-effective-sort type-key sort)
              :skip-allowed-values skip-allowed-values
              :records (mapcar
                         (lambda (row)
                           (measure-row-to-plist type-key row))
                         rows))))))))

(defun page-row-value (type-key field-key value)
  ":private: Convert one raw page-row value for FIELD-KEY to its
JSON-ready form: SQL NULL (:null) stays :null, boolean t / nil
become :true / :false (never bare nil — plist-to-json renders nil as
[], which JS reads as truthy), and timestamps take the base
hydrator's string form so the wire value matches every other
surface. Not measure-row-value: its boolean arm requires :source
:agg :first, and a page field's :source has no :agg."
  (let* ((field-type (u:tree-get *compiled-model* type-key :fields
                     field-key :type)))
    (cond
      ((or (null value) (eq value :null)) :null)
      ((eq field-type :boolean) (if value :true :false))
      ((eq field-type :timestamp)
        (dt:timestamp-string :universal-time value))
      (t value))))

(defun page-row-to-plist (type-key row)
  ":private: Convert one raw page row (SQL aliases are field keys)
to a field-key plist with JSON-ready values (the
measure-row-to-plist shape)."
  (loop for key in row by #'cddr
    for value in (cdr row) by #'cddr
    append (list key (page-row-value type-key key value))))

(defun page-be-list (type-key user &key (form :list-form) filters
                      sort skip-allowed-values)
  ":private: Page-shape be-list body: one parameterized select
against the content table — no base-be-list (it wants a table and
per-row resources on the listed type), no measure Phase A (no GROUP
BY), no view machinery. Zero rows is success (the empty page: the
frontend renders 'This page has not been written yet.'); one row is
the page; two rows report-e (the composite identity index failed —
never silently show the first of several; the identity-pin compile
rule is the guarantee, limit 2 the backstop). The count is the row
count of that same select, not a second query and not
user-read-type-ids.

The FORM argument is ignored on purpose. rest-list defaults form to
\"list-form\", so the page's bare URL arrives here asking for a form
the page does not declare — the envelope is always built from the
page's :view-form (the same treatment a rollup gives its
:list-form). A later reader who 'fixes' this to honor the request
form breaks every page read.

filters and sort (user input; a page's selection is model-declared)
are report-ve here — the guard fires ahead of any form work, and
the compile-time walls (the matrix's :default-sort rejection and
valid-hide-list's form arm) are what keep the defaulted sort nil.
limit / offset never arrive (the frontend's bare-URL branch sends
none) and are not parameters here at all."
  (declare (ignore form))
  (when filters
    (report-ve "page-be-list"
      "Filters are not supported on page type ~s; the page's row ~
       selection is model-declared (:filter on the page type)."
      ~type-key))
  (when sort
    (report-ve "page-be-list"
      "Sort is not supported on page type ~s; a page renders one row."
      ~type-key))
  (let* ((m *compiled-model*)
         (columns (u:tree-get m type-key :page-columns))
         (table (u:tree-get m type-key :page-table))
         (where (u:tree-get m type-key :page-where))
         (params (u:tree-get m type-key :page-params))
         (sql (format nil
                 "select ~{~a~^, ~}~%from ~a~@[~%where ~{~a~^ and ~}~]~%limit 2"
                 columns table where))
         ;; (cons sql params), never a bare list: a zero-bind page
         ;; (boolean identity pinned by a truthiness form) is a
         ;; legal compiled shape and must query cleanly with an
         ;; empty parameter list.
         (query (cons sql params))
         (rows (a:with-rbac (*rbac*) (a:rbac-query query))))
    (when (cdr rows)
      (report-e "page-be-list"
        "Page ~s matched more than one row of ~s; the identity ~
         index guarantee failed. Refusing to pick one silently."
        ~type-key ~table))
    (let ((records (mapcar
                     (lambda (row) (page-row-to-plist type-key row))
                     rows)))
      (pl:pdebug :in "page-be-list"
        :type-key type-key :table table :rows (length records))
      ;; :page-type rides by append only, never as a nil pair
      ;; through add-to-plist — its (or new old) merge cannot pin a
      ;; nil, and a bare nil would serialize as the truthy [].
      (append
        (list-result type-key user :view-form
          :records records
          :total (length records)
          :sort nil
          :skip-allowed-values skip-allowed-values)
        (list :page-type :true)))))

(defun be-list-column (type-key field-key user
                       &key (form :list-form) 
                        sort limit offset
                        filters search
                        skip-allowed-values)
  ":public: Returns a list of the values in FIELD-KEY for the records of type
TYPE-KEY that match FILTERS and that USER has 'read' permissions for. This is
like BE-LIST, but it returns a list of values instead of a list of records."
  (valid-compiled-model)
  (valid-type-key type-key)
  ;; Page guard (not valid-non-rollup-type-key — that one rejects
  ;; rollups too, and /api/column stays legal on a rollup). A page
  ;; is not a column source: be-list-column feeds its form (default
  ;; :list-form) to both be-list (where the page's form-ignore
  ;; serves the read) and its own outer response metadata, where
  ;; nothing serves it — a page declares no :list-form, so the
  ;; metadata arm would wrap a nil field def around the values.
  ;; Rejecting is honest where form-fixing both layers would be
  ;; maintained illusion.
  (when (eq (u:tree-get *compiled-model* type-key :phase-a-shape)
          :page)
    (report-ve "be-list-column"
      "Page type ~s is not a column source; a page is a read-only ~
       projection of one content row."
      ~type-key))
  (valid-field-key type-key field-key)
  (valid-existing-user user)
  (if (uuid-p filters)
    (valid-existing-uuid filters)
    (valid-filters filters :type-key type-key))
  (let ((records (getf
                   (be-list type-key user :form form :filters filters
                     :limit limit :offset offset :sort sort
                     :search search
                     :skip-allowed-values skip-allowed-values)
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

(defun merge-data-effect (current effect)
  "Merge EFFECT plist into a copy of CURRENT plist. Each key in EFFECT
overwrites the corresponding key in CURRENT, including keys whose value is
nil. Returns the merged plist.

Unlike add-to-plist (which uses OR for value lookup and therefore treats nil in
the new plist as 'not specified'), this function treats nil as an intentional
value. This is correct for lifecycle hook effects, where a hook may need to set
a field to nil."
  (loop with merged = (copy-list current)
    for key in effect by #'cddr
    for val in (cdr effect) by #'cddr
    do (setf (getf merged key) val)
    finally (return merged)))

(defun run-lifecycle-hooks (hooks type-key data user
                           &key id roles record)
  "Run a list of compiled lifecycle HOOKS with the unified contract. Each hook
is called as

(funcall hook type-key data user :id id :roles roles :record record)

Returns the (possibly updated) data plist. When a hook returns a plist, its keys
are merged into the running data (overwriting).  When a hook returns nil, data
is unchanged. Non-plist non-nil returns signal a report-e error.

Only pre-create and pre-update call sites should capture the return value.
Post-* and delete paths should ignore it."
  (loop with current = data
    for hook in hooks
    when hook do
    (let ((effect (funcall hook type-key current user
                    :id id :roles roles :record record)))
      (cond
        ((null effect) nil)
        ((u:plistp effect)
          (setf current (merge-data-effect current effect)))
        (t (report-e "run-lifecycle-hooks"
             "Lifecycle hook returned non-plist effect ~s" ~effect))))
    finally (return current)))

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
  (valid-non-rollup-type-key type-key)
  (valid-user-roles user roles)
  (valid-user-permissions user type-key "create")
  (valid-file-token file-token)
  (let* ((m *compiled-model*)
          (data (full-data type-key data user))
          (pre-create (u:tree-get m type-key :pre-create))
          ;; Pre-create hooks run before validation so they can fill
          ;; required computed fields (e.g. :compose-string).
          (data (run-lifecycle-hooks pre-create type-key data user
                  :roles roles)))
    (valid-data type-key data)
    (let ((field-errors (validate-fields type-key data user)))
      (when field-errors
        (report-ve "be-insert"
          "Field validation failed:~{ ~a~}" ~field-errors)))
    (let ((id (id-from-data type-key data)))
      (if id
        (values id nil)
        (progn
          (pl:pdebug :in "be-insert" :step 1
            :type-key type-key :status "validated")
          (let* ((f (u:tree-get m type-key :create))
                  (base (u:tree-get m type-key :base))
                  (internal (u:tree-get m type-key :internal))
                  (path-field (path-field type-key))
                  (logical-path (when path-field (getf data path-field)))
                  (post-create (u:tree-get m type-key :post-create)))
            (valid-new-directory type-key logical-path file-token user roles)
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
              (run-lifecycle-hooks post-create type-key data user
                :id new-id :roles roles)
              (loop
                for field-key in (user-fields type-key)
                for field-def = (u:tree-get m type-key :fields field-key)
                when (getf field-def :write-to)
                do (handler-case
                     (execute-write-to type-key field-key data new-id user)
                     (error (e)
                       (pl:perror :in "be-insert"
                         :status "write-through failed"
                         :type-key type-key :field-key field-key :error e))))
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
  (valid-non-rollup-type-key type-key)
  (pl:pdebug :in "be-insert-internal"
    :type-key type-key :data data :roles roles)
  (valid-user-roles user roles)
  (valid-user-permissions user type-key "create")
  (let* ((m *compiled-model*)
          (pre-create (u:tree-get m type-key :pre-create))
          (data (full-data type-key data user))
          ;; Pre-create hooks run before validation so they can fill
          ;; required computed fields (e.g. :compose-string).
          (data (run-lifecycle-hooks pre-create type-key data user
                  :roles roles)))
    (valid-data type-key data)
    (let ((field-errors (validate-fields type-key data user)))
      (when field-errors
        (report-ve "be-insert-internal"
          "Field validation failed:~{ ~a~}" ~field-errors)))
    (let ((id (id-from-data type-key data)))
      (if id
        (values id nil)
        (let* ((f (u:tree-get m type-key :create))
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
          (run-lifecycle-hooks post-create type-key data user
            :id new-id :roles roles)
          (values new-id t))))))

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

The update is allowed when USER holds update on the matched record's own
resource (base types degenerate to the type resource; the opaque built-in
:tokens type, which has no row resource, is gated on the type resource
instead)."
  (valid-compiled-model)
  (valid-type-key type-key)
  (valid-non-rollup-type-key type-key)
  (if (stringp filters)
    (valid-uuid filters)
    (valid-filters filters :required t :type-key type-key))
  (valid-data type-key data)
  (valid-user-roles user (remove-existing-non-user-roles user roles))
  ;; :usage is append-only: a crafted /api/update from admin must not
  ;; rewrite the log even though insert-normal attaches admin to every
  ;; usage resource. A usage-specific gate, not a global :update nil
  ;; change — :tokens is :update nil and store-token is the internal
  ;; update path that must keep working.
  (when (equal type-key :usage)
    (report-ve "be-update"
      "Type ~s is append-only and cannot be updated." ~type-key))
  ;; Non-resource, non-base built-in types (e.g. :tokens) are invisible
  ;; to uuid-exists-p / id-to-type-key / user-allowed-resource, so the
  ;; record-level checks below would lock their callers (REST
  ;; store-token) out of login; they get the type-level check only.
  (let* ((m *compiled-model*)
          (opaque-type-p (and (built-in-p type-key)
                           (not (base-type-p type-key))
                           (not (u:tree-get m type-key :internal))))
          (uuid (let ((uuid (id-from-filters-and-data type-key filters data)))
                  (cond
                    ;; No row resource exists, so the type resource is
                    ;; the only thing to check (the :tokens carve-out).
                    (opaque-type-p
                      (valid-user-permissions user type-key "update")
                      (if (uuid-p uuid)
                        uuid
                        (report-ve "be-update"
                          "No record of type ~s matches FILTERS/DATA."
                          ~type-key)))
                    ((and uuid
                       (user-allowed-resource user uuid "update"))
                      (valid-existing-uuid uuid)
                      uuid)
                    (uuid
                      (report-ve "be-update"
                        "User ~a does not have update permission ~
                         on record ~a."
                        ~user ~uuid))
                    (t
                      (report-ve "be-update"
                        "No record of type ~s matches FILTERS/DATA."
                        ~type-key)))))
          (record (when (or opaque-type-p (uuid-p uuid))
                    (getf (rec uuid user :type-key type-key
                            :blank-passwords nil)
                      :record)))
          (record (if (and (not opaque-type-p) (not record))
                    (report-ve "be-update"
                      "Record ~a not found for type ~s." ~uuid ~type-key)
                    record))
          ;; FR-11 belt: a data-plisted :roles on be-update :users
          ;; rides update-join-tables (the role-users join) instead of
          ;; the keyword path. The frontend never sends this shape
          ;; (App.tsx destructures roles out of data), but a direct
          ;; caller could. Bound for effect inside the let*.
          (roles-belt (when (and (equal type-key :users)
                              (getf data :roles))
                        (valid-admin-only-user-roles user
                          (getf data :roles))))
          ;; FR-12 gate: system accounts and base roles are
          ;; admin-only to change. Runs immediately after the record
          ;; load — before hooks, before the main update, before
          ;; update-join-tables / update-roles, so the Edit-form
          ;; keyword :roles path and the data-plist belt are both
          ;; behind it. Bound for effect inside the let*.
          (protected (valid-protected-record type-key record user))
          (pre-update (u:tree-get m type-key :pre-update))
          (post-update (u:tree-get m type-key :post-update))
          ;; Pre-update hooks run before validation so they can fill
          ;; required computed fields (e.g. :compose-string).
          (full-data (run-lifecycle-hooks pre-update type-key
                        (full-data type-key data user :record record)
                        user :id uuid :roles roles :record record))
          (sql (car (u:tree-get m type-key :update-sql :main)))
          (values (local-values-for-update type-key full-data record user
                    :id uuid))
          (update-query (cons sql values)))
    (valid-existing-join-data type-key full-data)
    (let ((field-errors (validate-fields type-key full-data user)))
      (when field-errors
        (report-ve "be-update"
          "Field validation failed:~{ ~a~}" ~field-errors)))
    ;; Fs-backed path fields are immutable until moves are supported.
    (valid-updated-directory type-key record full-data)
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
    ;; Write-through: upsert to related tables
    (loop
      for field-key in (user-fields type-key)
      for field-def = (u:tree-get m type-key :fields field-key)
      when (getf field-def :write-to)
      do (handler-case
           (execute-write-to type-key field-key full-data uuid user)
           (error (e)
             (pl:perror :in "be-update"
               :status "Write-through failed"
               :type-key type-key :field-key field-key :error e))))
    ;; Post-update lifecycle hooks
    (run-lifecycle-hooks post-update type-key full-data user
      :id uuid :roles roles)
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
  (valid-non-rollup-type-key type-key)
  (if (stringp filters)
    (valid-uuid filters)
    (valid-filters filters :required t :type-key type-key))
  (valid-existing-user user)
  ;; :usage is append-only (the be-update gate's twin). be-delete
  ;; would already signal on the nil compiled delete function, but
  ;; the public attempt is its own error.
  (when (equal type-key :usage)
    (report-ve "be-delete"
      "Type ~s is append-only and cannot be deleted from." ~type-key))
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
    ;; FR-12 gate: system accounts and base roles are admin-only to
    ;; delete (record is loaded as "admin", so :name is always
    ;; present; uuid nil = nothing matched, nothing to protect).
    (valid-protected-record type-key record user)
    (valid-existing-file-or-directory type-key logical-path)
    (when (and uuid (user-allowed-resource user uuid "delete"))
      (run-lifecycle-hooks pre-delete type-key nil user
        :id uuid :record record)
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
      (run-lifecycle-hooks post-delete type-key nil user
        :id uuid :record record)
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

(defun validate-field-internal (type-key field-key value user)
  ":private: Core per-field validator. Returns a list of error strings
(nil if valid). This is the shared inner loop used by both the
validate API (be-validate-field) and the write path (validate-fields)."
  (loop
    with validations = (u:tree-get *compiled-model*
                         type-key :fields field-key :validations)
    for validation in validations
    for clean-value = (trim-maybe value)
    ;; Validation contract: (type-key field-key value user) → nil | error-string
    ;; See docs/hook-registry.md for full details.
    for e = (funcall validation type-key field-key clean-value user)
    when e collect e))

(defun validate-fields (type-key data user)
  ":private: Form-level field validator. Walks user-facing fields for
TYPE-KEY (with defaults from DATA), runs compiled :validations on each,
and returns a list of error strings (nil if all valid). Used by both
be-validate-form (REST validate API) and the write path (be-insert,
be-update, be-insert-internal) so behavior cannot drift.

Skips pure computed fields (no :column, no :write-to, no :join-table,
no :target) since those are read-only aggregations, not user-writable.
For write-to fields without a column, validation runs only when a
value is present, since those fields are optional on the main form."
  (loop
    with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in (user-fields type-key)
    for field-def = (u:tree-get fields field-key)
    for has-column = (getf field-def :column)
    for has-write-to = (getf field-def :write-to)
    for has-join-table = (getf field-def :join-table)
    for has-target = (getf field-def :target)
    for field-type = (getf field-def :type)
    for default = (u:tree-get fields field-key :default)
    for value = (getf data field-key default)
    ;; Skip pure computed/read-only fields
    unless (or has-column has-write-to has-join-table has-target)
    do (pl:pdebug :in "validate-fields"
         :status "skipping computed field" :field-key field-key)
    else
    ;; For write-to fields without a column, only validate when
    ;; a value is present (optional on the main form)
    ;; For write-to fields without a column, only validate when
    ;; a value is present (optional on the main form)
    unless (and has-write-to (not has-column)
                (or (null value) (equal value :null)))
    ;; Skip password validation when no value is provided —
    ;; password is required on insert but optional on update.
    unless (and (eq field-type :password)
                (or (null value) (equal value :null)))
    appending (validate-field-internal
                type-key field-key value user)))

(defun be-validate-field (type-key field-key value user)
  ":public: Validates that VALUE is acceptable for FIELD-KEY of TYPE-KEY. This
function returns

    (:valid t)

if VALUE is valid for the field, and

    (:valid nil :errors {error-message-list})

if VALUE is not valid for the field. `{error-message-list}` is a list of strings
describing the problems with VALUE."
  (valid-compiled-model)
  (valid-type-key type-key)
  (valid-non-rollup-type-key type-key)
  (valid-field-key type-key field-key)
  (let ((errors (validate-field-internal
                  type-key field-key value user)))
    (if errors
      (list :valid :false :errors errors)
      (list :valid :true))))

(defun be-validate-form (type-key values user)
  ":public: Validates that each value in the VALUES plist is acceptable for its
field in TYPE-KEY. In VALUES, the keys are field keys and the values are the
values to be validated for those fields. This function returns

    (:valid t)

when all values are valid, and

    (:valid nil :errors {error-message-list})

when even one value is invalid. `{error-message-list}` is a list of strings."
  (valid-compiled-model)
  (valid-type-key type-key)
  (valid-non-rollup-type-key type-key)
  (let ((errors (validate-fields type-key values user)))
    (if errors
      (list :valid :false :errors errors)
      (list :valid :true))))

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
    collect (append
              (list :name type-key
                    :category (getf type-def :category)
                    :user-setting (getf type-def :user-setting))
              ;; getf truthiness is safe here by contract:
              ;; valid-page-description guarantees a non-blank
              ;; string whenever :page-description is declared, so
              ;; a present key is always truthy. Do not "fix" this
              ;; to member — that would start emitting the key for
              ;; exactly the present-nil values the validator
              ;; exists to reject.
              (when (getf type-def :page-description)
                (list :page-description
                  (getf type-def :page-description)))
              ;; member, not getf: an explicit
              ;; :page-description-open nil is legal (collapsed
              ;; default written out) and must ride the wire as
              ;; false, never drop.
              (when (member :page-description-open type-def)
                (list :page-description-open
                  (getf type-def :page-description-open)))
              ;; Page projection flag: the :page-description
              ;; omit-when-absent pattern, not the always-emitted
              ;; :user-setting pattern — absent means not a page,
              ;; so every existing types payload stays
              ;; byte-identical. rest-types keyword-encodes it.
              (when (eq (getf type-def :phase-a-shape) :page)
                (list :page-type t)))))

(defun be-landing-page (user)
  ":public: Effective landing type-key for USER, or NIL.
Uses model :landing-page when allowed; else the first non-built-in
type from be-types (author types only — :base is the RBAC storage
scheme, not authorship; :secrets is :built-in t :base nil and must
not win the fallback); else first be-types entry; else NIL."
  (let* ((allowed (mapcar (lambda (e) (getf e :name)) (be-types user)))
         (configured (model-landing-page)))
    (cond
      ((and configured (member configured allowed :test #'eq))
       configured)
      ((find-if (lambda (k) (not (built-in-p k))) allowed))
      (t (first allowed)))))

(defun be-set-field-value (type-key id field-key value user)
  ":internal: Single-column update with no lifecycle, no form validations, no
write-through, no join/role updates.  Used by the action status path.  Requires
TYPE-KEY to be valid, ID to be an existing UUID, FIELD-KEY to exist with :column
t, and USER to exist."
  (valid-compiled-model)
  (valid-type-key type-key)
  (valid-non-rollup-type-key type-key)
  (valid-existing-uuid id)
  (valid-existing-user user)
  (valid-field-key type-key field-key)
  (let* ((m *compiled-model*)
         (field-def (u:tree-get m type-key :fields field-key))
         (column (getf field-def :column)))
    (unless column
      (report-ve "be-set-field-value"
        "Field ~s on type ~s is not a column."
        ~field-key ~type-key))
    (let* ((table (u:tree-get m type-key :table-name))
           (col-name (getf field-def :name-sql))
           (sql (format nil "update ~a set ~a = $1 where id = $2"
                  table col-name)))
      (a:with-rbac (*rbac*)
        (a:rbac-query (list sql value id))))))

;;
;; BEGIN :enabled-when evaluation (state-gated buttons)
;;
;; eval-condition-rows is the single runtime owner of the
;; (:field :op :value) row grammar for :ui (:enabled-when …) gates:
;; rows are AND-composed, values compare with equal after
;; nil/:null/"" normalization, ordering ops fail closed on type
;; mismatch, and a missing / nil row field fails the row (the
;; button renders disabled and be-action rejects). The frontend
;; mirrors these rules in evalEnabledWhen (App.tsx); drift between
;; the two is the paired-evaluator risk the shared fixture
;; (tests/fixtures/enabled-when-cases.json) exists to catch.
;;

(defun empty-value-p (v)
  ":private: T when V belongs to the single \"empty\" class: nil, the
:null keyword, or the empty string (Deploy's (:model :ne \"\") gate
answers the nil-vs-empty question: one class, not three)."
  (or (null v) (eq v :null) (and (stringp v) (string= v ""))))

(defun eval-condition-row (row record)
  ":private: Evaluate one (:field :op :value) row against RECORD (a
loaded plist of current DB state). Returns T when the row holds.
Fail-closed rules: a field absent from RECORD or holding SQL NULL
fails every row except an empty-literal :eq / :ne (the empty class
— nil / :null / \"\" — answers those directly, so (:field :eq \"\")
holds on a fresh nil row: Deploy's gate, and :eq is the plan's
stated mirror of the :ne empty rule); ordering ops fail closed on
type mismatch; :in / :not-in fail closed on a value whose type
matches no literal element. Never signals — the guard runs inside
the action path, not validation."
  (destructuring-bind (row-field op literal) row
    (let ((value (getf record row-field)))
      (cond
        ;; The empty class answers :eq / :ne directly — including a
        ;; nil record value (plan: ":eq is its mirror" of the :ne
        ;; empty rule; Deploy's fresh-row gate relies on it).
        ((and (member op '(:eq :ne)) (empty-value-p literal))
          (if (eq op :eq)
            (empty-value-p value)
            (not (empty-value-p value))))
        ;; Absent field or SQL NULL fails every other row (fail
        ;; closed).
        ((null value) nil)
        ((eq op :eq) (equal value literal))
        ((eq op :ne) (not (equal value literal)))
        ((member op '(:gt :lt :gte :lte))
          (and (typep value (type-of literal))
            (funcall
              (case op
                (:gt #'>)
                (:lt #'<)
                (:gte #'>=)
                (:lte #'<=))
              value literal)))
        ((eq op :in) (member value literal :test #'equal))
        ((eq op :not-in)
          (and (some (lambda (x) (typep value (type-of x))) literal)
            (not (member value literal :test #'equal))))
        (t nil)))))

(defun eval-condition-rows (rows record)
  ":private: AND-compose :enabled-when ROWS against RECORD. Appending
a row may only restrict — every row must hold for the gate to
pass. Returns T when all rows hold (or no gate was declared)."
  (every (lambda (row) (eval-condition-row row record)) rows))

(defun enabled-when-holds (type-key field-key record)
  ":private: T when the :ui (:enabled-when …) gate on FIELD-KEY
(when one exists) holds against RECORD. Reads the compiled field's
:ui plist — the same rows the frontend receives verbatim — and
evaluates them with eval-condition-rows. No gate means enabled
(the fail-closed rule governs rows inside a present gate, never
the gate's absence)."
  (let ((rows (u:tree-get *compiled-model* type-key :fields field-key
                :ui :enabled-when)))
    (eval-condition-rows (or rows '()) record)))

;;
;; END :enabled-when evaluation
;;

(defun be-action (type-key id field-key user &optional (data nil data-p))
  ":public: Execute the action hook(s) attached to FIELD-KEY (a :button field)
on the record ID of TYPE-KEY.

Sets the companion status field to \"running\", invokes each compiled action
hook with the unified contract, then transitions to \"complete\" (sync success)
or \"failed: <reason>\" (sync error).  Async hooks (:async t in the result)
leave status at \"running\" for a worker to complete.

DATA, when supplied, is the form values the user typed into the edit form
(a plist of field-key → value, forwarded from /api/actions' :data).  Hooks
that need typed input (e.g. :change-password) read it as the :data keyword;
hooks that don't simply ignore it.  An absent DATA is forwarded as nil.

Returns a plist:
  (:status \"complete\" :message <hook-message-or-nil>)
  (:status \"accepted\" :message <msg> :async t)
  (:status \"failed\" :message <reason>)

Signals a validation error if:
  - TYPE-KEY is invalid or lacks action hooks for FIELD-KEY
  - USER lacks update permission on the record
  - Status is already \"running\" (in-progress guard)"
  (valid-compiled-model)
  (valid-type-key type-key)
  (valid-non-rollup-type-key type-key)
  (valid-existing-uuid id)
  (valid-existing-user user)
  (valid-field-key type-key field-key)
  ;; Look up the compiled field definition
  (let* ((m *compiled-model*)
         (field-def (u:tree-get m type-key :fields field-key)))
    ;; Field must be a :button with a compiled hook
    (unless (equal (getf field-def :type) :button)
      (report-ve "be-action"
        "Field ~s on type ~s is not a :button field."
        ~field-key ~type-key))
    (let* ((hook (getf field-def :compiled-hook))
           (status-field (getf field-def :status-field)))
      (unless hook
        (report-ve "be-action"
          "Field ~s on type ~s has no action hook."
          ~field-key ~type-key))
      ;; Record-level access
      (unless (user-allowed-resource user id "update")
        (report-ve "be-action"
          "User ~a does not have update permission on record ~a."
          ~user ~id))
      ;; Load the record
      (let ((record (getf (rec id user :type-key type-key) :record)))
        (unless record
          (report-ve "be-action"
            "Record ~a not found for type ~s." ~id ~type-key))
        ;; In-progress guard
        (let ((current-status (getf record status-field)))
          (when (equal current-status "running")
            (report-ve "be-action"
              "Action already in progress for ~s ~s field ~s."
              ~type-key ~id ~field-key)))
        ;; State-gate guard (:ui :enabled-when). Runs before
        ;; "running" is stamped — a rejection that stamped the
        ;; status would strand the button (a cousin of the
        ;; double-click bug this gate exists to close). Same error
        ;; class as the in-progress guard so the REST layer renders
        ;; it identically.
        (let ((rows (u:tree-get m type-key :fields field-key
                      :ui :enabled-when)))
          (unless (eval-condition-rows (or rows '()) record)
            (let ((failed-value (loop for row in rows
                                  for v = (getf record (first row))
                                  when v return v)))
              (report-ve "be-action"
                "Button ~s is disabled for this record: condition ~
                 ~s does not hold (record value: ~s)."
                ~field-key ~rows ~failed-value))))
        ;; Build set-status closure
        (let ((set-status (lambda (message)
                            (be-set-field-value
                              type-key id status-field message user))))
          ;; Write "running"
          (funcall set-status "running")
          ;; Run the hook (non-transactional MVP)
          (handler-case
              (let* ((roles (a:list-user-role-names *rbac* user))
                     (result (funcall hook
                               type-key field-key record user
                               :roles roles
                               :status-field status-field
                               :set-status set-status
                               :data (when data-p data))))
                (cond
                  ;; Async: worker owns status via set-status
                  ((and (listp result) (getf result :async))
                   result)
                  ;; Sync: be-action owns status, derived from result
                  (t
                   (let* ((failed (and (listp result)
                                       (equal (getf result :status)
                                              "failed")))
                          (status (if failed "failed" "complete")))
                     (funcall set-status
                       (if failed
                           (format nil "failed: ~a"
                             (getf result :message))
                           status))
                     (if failed
                         result
                         (list :status "complete"))))))
            (error (e)
              (let* ((err-str (format nil "~a" e))
                     (msg (format nil "failed: ~a"
                            (subseq err-str 0
                              (min (length err-str) 200)))))
                (funcall set-status msg)
                (list :status "failed" :message err-str)))))))))

;;
;; BEGIN Usage log (:usage)
;;
;; A built-in, append-only admin-facing log of app usage. Written
;; only by record-usage (the REST layer calls it); read through the
;; ordinary be-list. Every helper here swallows its own errors:
;; there are no transactions, and a log write must never take the
;; user's request down with it.
;;

(defparameter *usage-events*
  '(:login :login-failed :list :view :insert :update :delete :action)
  "Closed event vocabulary for :usage rows (record-usage enforces it).")

(defparameter *usage-window-seconds* 60
  "Dedup window for :list rows: same user + event + type + detail
inside this many seconds collapses to one row. The window bound is
interpolated into the interval literal by usage-recent-p (an
integer this system owns, never user input), not a bind parameter:
interval $1 does not accept a bare integer.")

(defparameter *usage-detail-max* 80
  "Character cap on :usage :detail (usage-detail truncates).")

(defun usage-detail (value)
  ":private: Normalize a :usage :detail value: trim a string,
truncate to *usage-detail-max* characters, nil for blank,
non-string, or empty. Keywords downcase to strings (the action
field key). Nil stays nil. The same normalization runs before the
dedup query and the insert, so \" Milk \" and \"milk\" are one
key."
  (cond
    ((null value) nil)
    ((keywordp value) (string-downcase (string value)))
    ((not (stringp value)) nil)
    (t (let* ((trimmed (u:trim value))
              (capped (if (> (length trimmed) *usage-detail-max*)
                        (subseq trimmed 0 *usage-detail-max*)
                        trimmed)))
         (when (plusp (length capped)) capped)))))

(defun usage-title-column (type-key)
  ":private: The :title-field column expression of TYPE-KEY (a
declared row-titling field with :column t), or nil when the type
declares none (valid-title-field guarantees shape and the column
when the key is present). A single nullif/trim expression, same
shape as the identity expressions in usage-identity-string."
  (let* ((m *compiled-model*)
         (field-key (u:tree-get m type-key :title-field)))
    (when field-key
      (format nil "nullif(trim(~a::text), '')"
        (u:tree-get m type-key :fields field-key :name-sql)))))

(defun usage-identity-string (type-key id)
  ":private: Row-titling value of TYPE-KEY's row with uuid ID, as
trimmed text, or nil: the :title-field value when the type
declares one, else the first non-blank identity value. Direct SQL
beside usage-recent-p (same rationale: the backend read paths
hydrate forms and touch handler state — a REST-layer detail
enrichment must not depend on it, and record-usage would swallow
the failure and drop the row). Unknown table, unknown uuid, or
any error returns nil — the detail degrades to uuid-only. Never
signals."
  (when (and (uuid-p id) (type-key-p type-key))
    (handler-case
      (let* ((m *compiled-model*)
             (table (u:tree-get m type-key :table-name))
             (keys (identity-keys type-key))
             (cols (append
                     (when (usage-title-column type-key)
                       (list (usage-title-column type-key)))
                     (loop for k in keys
                       collect (format nil "nullif(trim(~a::text), '')"
                                 (u:tree-get m type-key :fields k
                                   :name-sql)))))
             (sql (format nil "select coalesce(~{~a~^, ~}) from ~a ~
                               where id = $1"
                     (append cols '("''")) table))
             (value (a:with-rbac (*rbac*)
                      (a:rbac-query (list sql id) :single))))
        (when (and (stringp value) (plusp (length value))) value))
      (error () nil))))

(defun usage-detail-for (type-key id &key extra identity)
  ":private: Detail string naming the touched row: \"<identity> /
<uuid>\" — EXTRA (an action field key) prefixes it, becoming
\"<extra> / <identity> / <uuid>\". The uuid is always last and
never truncated: the prefix is pre-fit so the composed string
never exceeds *usage-detail-max*, and usage-detail's cap can then
never eat the uuid. IDENTITY, when non-nil, is a pre-fetched
identity string (the delete path — the row is gone by lookup
time, so the caller read it before the write); otherwise the
identity is looked up. A blank or missing identity degrades to
uuid-only. Nil ID returns nil — record-usage stores a null
detail."
  (when id
    (let* ((uuid (if (uuid-p id) id (format nil "~a" id)))
           (room (max 0 (- *usage-detail-max* (length uuid) 3)))
           (identity (or identity (usage-identity-string type-key id)))
           (parts (remove nil
                     (list (when extra (string-downcase (string extra)))
                       identity)))
           (prefix (when parts
                     (let ((joined (format nil "~{~a~^ / ~}" parts)))
                       (subseq joined 0 (min (length joined) room))))))
      (if prefix (format nil "~a / ~a" prefix uuid) uuid))))

(defun usage-identity-from-record (type-key record)
  ":private: Row-titling value from a pre-fetched RECORD plist
(the delete path — the row is gone by detail time, so the caller
read it before the write): the :title-field value when the type
declares one and the value is non-blank, else the identity-field
value. Same precedence as usage-identity-string's SQL coalesce,
per row. Nil / blank degrade to nil (uuid-only detail)."
  (labels ((non-blank (key)
             (let* ((raw (and record key (getf record key)))
                    (s (and raw
                         (not (equal raw :null))
                         (u:trim (format nil "~a" raw)))))
               (when (and s (plusp (length s))) s))))
    (or (non-blank (u:tree-get *compiled-model* type-key :title-field))
      (non-blank (identity-field type-key)))))

(defun usage-recent-p (user-name event on detail)
  ":private: T when a :usage row with the same user-name + event +
on + detail was recorded inside *usage-window-seconds*. Arguments
are already normalized (record-usage downcases event / on and runs
detail through usage-detail). Raw SQL, not be-list: listing :usage
is what the self-log guard exists to avoid, and be-list would
page, hydrate, and check permissions. Column names come from the
compiled field defs (:name-sql), so a :force-sql-name rename
cannot desync the SQL from the type. NULL :on / :detail compare
with is not distinct from, so a login (both null) matches a prior
login; NULL binds as :null, never bare Lisp nil (the driver binds
nil as SQL false). Not atomic — two identical lists in the same
instant can both pass. Accept the rare duplicate; do not add a
lock."
  (let* ((m *compiled-model*)
         (table (u:tree-get m :usage :table-name))
         (name-col (u:tree-get m :usage :fields :user-name :name-sql))
         (event-col (u:tree-get m :usage :fields :event :name-sql))
         (on-col (u:tree-get m :usage :fields :on :name-sql))
         (detail-col (u:tree-get m :usage :fields :detail :name-sql))
         (sql (format nil
                 "select 1 from ~a where ~a = $1 and ~a = $2 ~
                  and ~a is not distinct from $3 ~
                  and ~a is not distinct from $4 ~
                  and created_at > now() - interval '~a seconds' ~
                  limit 1"
                 table name-col event-col on-col detail-col
                 *usage-window-seconds*)))
    (a:with-rbac (*rbac*)
      (a:rbac-query
        (list sql user-name event (or on :null) (or detail :null))
        :single))))

(defun usage-insert (data user)
  ":private: Insert one :usage row and return its resource uuid.
Inserts as USER (record-usage passes \"admin\" — :create nil
compiles to a nil create function, so be-insert / be-insert-internal
cannot write this type; store-token's :tokens precedent). Does not
call id-from-data: id-from-data filters on every local column, and
:when is one-second text, so two identical events in the same
second would return the existing id and drop the second row. The
resource insert (a:add-resource) supplies the uuid; the compiled
:insert-sql :main runs through insert-main-query for its side
effect (do not read the query result as the id — query returns
((:id \"<uuid>\")), and the uuid is already in hand). The role
attach mirrors insert-normal's (admin plus the writer's
exclusive) plus the type's own \"usage\" role — :type-roles
(\"usage\") gates the type, but a user granted it reads rows only
when each row's resource carries the role; without it, be-list
returns them an empty list. On any error after a:add-resource
succeeded, a:remove-resource the new resource before
re-signaling, so a failed insert does not leak a resources row.
Returns the uuid or signals; the caller swallows."
  (let* ((resource-name (make-resource-name :usage data))
         (uuid (a:add-resource *rbac* resource-name :roles nil))
         (insert (u:tree-get *compiled-model* :usage :insert-sql)))
    (handler-case
      (progn
        (a:with-rbac (*rbac*)
          (a:rbac-query (insert-main-query :usage insert uuid data user)))
        (update-roles :usage uuid user
          (list "admin" "usage" (a:exclusive-role-for user)))
        uuid)
      (error (e)
        (a:remove-resource *rbac* resource-name)
        (error e)))))

(defun record-usage (user-name event &key ip on detail (dedup t))
  ":public: Append one :usage row for USER-NAME, or return nil.
EVENT is one of *usage-events*, a keyword or a string (\"LIST\" and
:list are the same event). ON is the target type, a keyword or a
string, or nil for login. DETAIL is capped by usage-detail. IP is
the original caller address, trimmed but never truncated (a
truncated address is a lie); nil / blank / non-string store NULL.
It is not in the dedup key (shared NAT must keep collapsing
repeated lists). DEDUP, the list default, skips the insert when
the same user, event, type, and detail were recorded inside
*usage-window-seconds*. Inserts as admin. Swallows every error:
a log write must not fail the caller's request. Does not call
be-insert-internal: id-from-data would collapse two identical
events in the same second into one row (usage-insert is the
sanctioned writer — do not add :usage writes through any other
path). The helper does not log itself: on :usage (compared after
normalization, so a string spelling cannot sneak past) returns
before the dedup query, and an unknown event is a report-e raised
inside the handler-case, logged, and returned nil. Models with
:usage (:state :off) stop at the top: the state gate returns nil
before any work — no insert, no dedup read, no error (the log is
off, not broken); the unknown-event report-e fires only when
logging is on."
  (when (eq (model-usage-state) :off)
    (return-from record-usage nil))
  (handler-case
    (let* ((event-s (and (or (keywordp event) (stringp event))
                        (string-downcase (string event))))
           (event-key (and event-s (u:make-keyword event-s)))
           (on-s (and on
                   (or (keywordp on) (stringp on))
                   (string-downcase (string on))))
           (detail (usage-detail detail))
           (ip (let ((s (and (stringp ip) (u:trim ip))))
                 (and s (plusp (length s)) s)))
           (name (and (stringp user-name) (u:trim user-name))))
      (unless (member event-key *usage-events*)
        (report-e "record-usage"
          "Unknown usage event ~s." ~event))
      (unless (equal on-s "usage")
        (when (and name (plusp (length name))
                (or (not dedup)
                  (not (usage-recent-p name event-s on-s detail))))
          (let ((data (list :user-name name
                        :event event-s
                        :when (dt:timestamp-string))))
            (when on-s (setf (getf data :on) on-s))
            (when detail (setf (getf data :detail) detail))
            (when ip (setf (getf data :ip) ip))
            ;; :user is set to the username, not the uuid:
            ;; resolve-reference-id accepts a display name, and the
            ;; username matches every other :target :users writer.
            ;; An unknown name omits the key entirely — a nullable
            ;; FK with no default binds NULL, and passing the bad
            ;; name would signal inside db-value (swallowed, but
            ;; the row would be dropped).
            (when (a:get-id *rbac* "users" name)
              (setf (getf data :user) name))
            (usage-insert data "admin")))))
    (error (e)
      (pl:perror :in "record-usage"
        :status "usage write failed"
        :user-name user-name :event event :on on :error e)
      nil)))

(defun styling-slot (name)
  ":public: The sanitized :value of the built-in :styling row named
NAME, or nil when there is no such row, the value is blank, or the
read fails for any reason (logged — a chrome read never takes a
page or /api/info down). Direct SQL in the usage-recent-p shape,
not be-list: site-chrome runs this twice per document render and
render-record-document calls site-chrome twice (once directly,
once inside record-json-ld) — four reads per record page — and
the system's own authority should not route through the permission
machinery. Column names come from the compiled field defs
(:name-sql), so a :force-sql-name rename cannot desync the SQL
from the type. The stored value was sanitized by the :pre-create /
:pre-update hook; sanitize-styling-html runs again here as the
belt for rows written around the hook (REPL, SQL, a restored
snapshot)."
  (handler-case
    (let* ((m *compiled-model*)
           (table (u:tree-get m :styling :table-name))
           (name-col (u:tree-get m :styling :fields :name :name-sql))
           (value-col (u:tree-get m :styling :fields :value :name-sql))
           (value (a:with-rbac (*rbac*)
                    (a:rbac-query
                      (list (format nil
                              "select ~a from ~a where ~a = $1 limit 1"
                              value-col table name-col)
                            name)
                      :single)))
           (clean (and (stringp value)
                       (sanitize-styling-html value))))
      (when (and clean (plusp (length (u:trim clean)))) clean))
    (error (e)
      (pl:perror :in "styling-slot" :name name :error e)
      nil)))

;;
;; END Usage log
;;

;;
;; END Public backend functions
;;
