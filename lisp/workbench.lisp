(in-package :data-ui)

(defparameter *timestamp-regex*
  "^\\d{4}-[01][0-9]-[0-3][0-9][T ][0-2][[0-9]:[0-5][0-9]:[0-5][0-9]$")

(defparameter *uuid-regex*
  (ppcre:create-scanner
    (format nil "^~{~a~^-~}$"
      (list
        "[0-9a-fA-F]{8}"
        "[0-9a-fA-F]{4}"
        "[0-9a-fA-F]{4}"
        "[0-9a-fA-F]{4}"
        "[0-9a-fA-F]{12}"))))

(defparameter *base-model*
`(:users
     (:table t :base t
       ;; TODO: :create, :update, and :delete should use RBAC functions
       :create :auto :update :auto :delete :auto
       :views (:main (:tables (:users :role-users :roles)
                       :filter :id))
       :deletable t
       :fields (:name (:type :text
                        :source (:first (:view :main :column :user-name))
                        :ui (:label "Username" :input-type :line
                              :required t)
                        :column t :not-null t :unique t)
                 :password (:type :password
                             :ui (:label "Password" :input-type :password
                                   :required t)
                             :column t :not-null t)
                 :email (:type :text
                          :source (:first (:view :main :column :email))
                          :ui (:label "Email" :input-type :line
                                :required t)
                          :column t :not-null t)
                 :roles (:type :list
                          :ui (:label "Roles" :input-type :checkbox-list)
                          :source (:distinct-list
                                    (:view :main :column :role-name))
                          :checked (:list (:view :main :column :role-name))
                          :unchecked (:list (:table :roles :column :role-name))
                          :ids-table :roles
                          :join-table :role-users))
       :list-form (:fields (:name t :email t :roles t))
       :update-form (:fields (:name t :email t :password (:required nil)
                               :roles t))
       :add-form (:fields (:name t :email t :password t)))

     :resources
     (:table t :base t
       ;; TODO: :create, :update, and :delete should use RBAC functions
       :create :auto :update :auto :delete :auto
       :views (:main (:tables (:resources :resource-roles :roles)))
       :deletable t
       :fields (:name (:type :text
                        :ui (:label "Resource" :input-type :line)
                        :source (:first (:view :main :column :resource-name))
                        :column t :not-null t :unique t)
                 :roles (:type :list
                          :ui (:label "Roles" :input-type :checkbox-list)
                          :source (:list (:view :main :column :role-name))
                          :checked (:list (:view :main :column :role-name))
                          :unchecked (:list (:table :roles :column :role-name))
                          :ids-table :roles
                          :join-table :resource-roles))
       :list-form (:fields (:name t :roles t))
       :update-form (:fields (:name t :roles t))
       :add-form (:fields (:name t :roles t)))

     :permissions
     (:table t :base t
       :create :auto :update :auto :delete :auto
       :deletable t
       :fields (:name (:type :text
                        :ui (:label "Permission" :input-type :line)
                        :source (:column :self)
                        :column t :not-null t :unique t))
       :list-form (:fields (:name t))
       :update-form (:fields (:name t))
       :add-form (:fields (:name t)))

     :roles
     (:table t :base t
       ;; TODO: :create, :update, and :delete should use RBAC functions
       :create :auto :update :auto :delete :auto
       :views (:main (:tables (:roles :role-permissions :permissions)))
       :fields (:name (:type :text
                        :ui (:label "Role" :input-type :line)
                        :source (:first (:view :main :column :role-name))
                        :column t :not-null t :unique t)
                 :permissions (:type :list
                                :ui (:label "Permissions" :input-type :checkbox-list)
                                :source (:list (:view :main :column :permission-name))
                                :checked (:list (:view :main :column :permission-name))
                                :unchecked (:list (:table :permissions :column :permission-name))
                                :ids-table :permissions
                                :join-table :role-permissions))
       :list-form (:fields (:name t :permissions t))
       :update-form (:fields (:name t :permissions t))
       :add-form (:fields (:name t :permissions t)))

     :role-permissions
     (:table t :base t :is-joiner t
       :fields (:reference (:target :roles)
                 :reference (:target :permissions)))

     :resource-roles
     (:table t :base t :is-joiner t
       :fields (:reference (:target :resources)
                 :reference (:target :roles)))

     :role-users
     (:table t :base t :is-joiner t
       :fields (:reference (:target :roles)
                 :reference (:target :users)))))

(defparameter *model*
  `(:todos
     (:table t
       :create :auto :update :auto :delete :auto
       :views (:main (:tables (:todos :todo-tags :tags)))
       :fields (:name (:type :text
                        :ui (:label "To Do" :input-type :line)
                        :source (:first (:view :main :column :todo-name))
                        :column t :not-null t :unique t)
                 :tags (:type :list
                         :ui (:label "Tags" :input-type :checkbox-list)
                         :source (:list (:view :main :column :tag-name))
                         :checked (:list (:view :main :column :tag-name))
                         :unchecked (:list (:table :tags :column :tag-name))
                         :ids-table :tags
                         :join-table :todo-tags))
       :list-form (:fields (:name t))
       :update-form (:fields (:name t :tags t))
       :add-form (:fields (:name t :tags t)))

     :todo-tags
     (:table t :is-joiner t
       :fields (:reference (:target :todos)
                 :reference (:target :tags)))

     :tags
     (:table t
       :create :auto :update :auto :delete :auto
       :fields (:name (:type text
                        :ui (:label "Tag" :input-type :line)
                        :column t :not-null t :unique t))
       :list-form (:fields (:name t))
       :update-form (:fields (:name t))
       :add-form (:fields (:name t)))))


(defparameter *compiled-model* nil)

(defun placeholders (fields &key (start-at 1))
  (loop for a from start-at to (1- (+ start-at (length fields)))
    collect (format nil "$~d" a)))

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

(defun table-name (keyword base)
  (let ((format-string (if base "~a" "rt_~a")))
    (to-sql-identifier keyword :format-string format-string)))

(defun table-reference (keyword)
  (when keyword
    (u:make-keyword
      (format nil "~a-id" (u:singular (format nil "~a" keyword))))))

(defun table-reference-sql (keyword)
  (when keyword
    (format nil "~(~a_id~)" (u:singular (format nil "~a" keyword)))))

(defun column-name (type-key field-key field-definition)
  (let ((table-name (to-sql-identifier type-key :form :singular)))
    (if (or
          (getf field-definition :target)
          (member field-key (default-fields t)))
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

(defun create-table-sql (table fields)
  (list
    :table (format
             nil
             "~%create table if not exists ~a (~%    ~{~a~^,~%    ~}~%)~%"
             table
             (mapcar
               (lambda (field) (getf field :create-sql))
               (remove-if-not
                 (lambda (field) (getf field :column))
                 (u:plist-values fields))))
    :trigger (updated-at-trigger-sql table)))

(defun columns (fields &rest keys)
  "Returns the field key and SQL column name for fields that have non-NIL
values for all KEYS."
  (loop
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    when (every (lambda (k) (getf field-def k)) keys)
    collect (cons field-key (getf field-def :name-sql))))

(defun fk-columns (model type-key)
  (loop with fields = (u:tree-get model type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for joiner = (getf field-def :join-table)
    when joiner
    append (columns (u:tree-get model joiner :fields) :target)))

(defun insert-keys (model type-key &key for-update)
  (loop
    with fields = (u:tree-get model type-key :fields)
    and base = (u:tree-get model type-key :base)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    when (getf field-def :join-table)
    collect field-key into keys
    finally (return
              (if (or base for-update)
                (cons :main keys)
                (append '(:resource :main) keys)))))

(defun insert-sql (model type-key)
  (loop
    with fields = (u:tree-get model type-key :fields)
    with insert-keys = (insert-keys model type-key)
    with res-columns = (list
                         (cons
                           :name
                           (u:tree-get model :resources :fields :name :name-sql)))
    with main-columns = (cons '(:id . "id") (columns fields :column :ui))
    with fk-columns = (fk-columns model type-key)
    with sql = "insert into ~a (~{~a~^, ~}) values (~{~a~^, ~})"
    for key in insert-keys
    for table-key = (case key
                      (:resource :resources)
                      (:main type-key)
                      (otherwise (u:tree-get fields key :join-table)))
    for table-name = (table-name table-key (u:tree-get model table-key :base))
    for table-cols = (case key
                       (:resource res-columns)
                       (:main (if (u:tree-get model table-key :base)
                                (cdr main-columns)
                                main-columns))
                       (otherwise fk-columns))
    append (list key (cons
                       (format nil sql
                         table-name
                         (mapcar #'cdr table-cols)
                         (placeholders table-cols))
                       (mapcar #'car table-cols)))))

(defun zip (list-1 list-2)
  (loop
    for a in list-1
    for b in list-2
    collect a
    collect b))

(defun update-sql (model type-key)
  (loop
    with fields = (u:tree-get model type-key :fields)
    with update-keys = (insert-keys model type-key :for-update t)
    with columns = (columns fields :column :ui)
    with fk-columns = (fk-columns model type-key)
    with sql = "update ~a set ~{~a = ~a~^, ~} where id = $~d"
    for key in update-keys
    for table-key = (case key (:main type-key) (otherwise key))
    for table-name = (table-name table-key (u:tree-get model table-key :base))
    for table-cols = (case key (:main columns) (otherwise fk-columns))
    append
    (list key (cons
                (format nil sql
                  table-name
                  (zip (mapcar #'cdr table-cols) (placeholders table-cols))
                  (1+ (length table-cols)))
                (append (mapcar #'car table-cols) '(:id))))))


(defun view-sql (view model)
  "Converts a view like '(:users :user-roles :roles) into an SQL query string
that joins tables."
  (loop
    with filter = (or (u:tree-get view :filter) :id)
    and first-table-key = (car (getf view :tables))
    with filter-def = (u:tree-get model first-table-key :fields filter)
    for first = t then nil
    for prev-table-key in (getf view :tables)
    for prev-table-name = (table-name
                            prev-table-key
                            (u:tree-get model prev-table-key :base))
    for first-table-name = prev-table-name then first-table-name
    for prev-is-joiner = (u:tree-get model prev-table-key :is-joiner)
    for table-key in (cdr (getf view :tables))
    for table-name = (table-name table-key (u:tree-get model table-key :base))
    for is-joiner = (u:tree-get model table-key :is-joiner)
    for prev-table-ref = (if prev-is-joiner
                           (format nil "~a.~a"
                             prev-table-name
                             (to-sql-identifier
                               (table-reference table-key)))
                           (format nil "~a.id" prev-table-name))
    for table-ref = (if is-joiner
                      (format nil "~a.~a"
                        table-name
                        (to-sql-identifier
                          (table-reference prev-table-key)))
                      (format nil "~a.id" table-name))
    for sql = (format nil "join ~a on ~a = ~a"
                table-name
                prev-table-ref
                table-ref)
    unless (equal first-table-name table-name)
    collect sql into sql-lines
    finally
    (return
      (format nil "~%select * from ~a~%~{  ~a~%~}where ~a = $1~%"
                      first-table-name
                      sql-lines
                      (format nil "~a.~a"
                        first-table-name
                        (column-name first-table-key filter filter-def))))))

(defun keyword-matches (keyword regex)
  (re:scan (format nil "(?i)~a" regex) (format nil "~s" keyword)))

(defun views-sql (views model)
  (loop
    for view-key in views by #'cddr
    for view-def in (cdr views) by #'cddr
    for is-list = (re:scan "list" (format nil "~(~a~)" view-key))
    for list-view-key = (u:make-keyword (format nil "~a-list" view-key))
    append (list
             view-key
             (list
               :sql (view-sql view-def model)
               :filter (getf view-def :filter))
             list-view-key
             (list
               :sql (re:regex-replace
                      "where .+"
                      (view-sql view-def model) "")
               :filter nil))))

(defun delete-sql (model type-key)
  (let* ((base (u:tree-get model type-key :base))
          (table-name (table-name type-key base)))
    (if base
      (list (format nil "delete from ~a where id = $1" table-name) :id)
      (list "delete from resources where id = $1" :id))))

(defun field-type (field-def)
  (if (getf field-def :target)
    :uuid
    (or (getf field-def :type) :text)))

(defun sql-type (field-type-key)
  (to-sql-identifier field-type-key))

(defun value-sql (value field-type-key &key quote)
  (case field-type-key
    (:boolean (case value
                (:true (if quote "'true'" "true"))
                (:false (if quote "'false'" "false"))
                (otherwise (error "Unsupported boolean value '~a'" value))))
    (:integer (cond
                ((equal value :null) nil)
                ((integerp value) value)
                (t (error "Invalid integer value '~a'" value))))
    (:float (cond
              ((equal value :null) nil)
              ((numberp value) value)
              (t (error "Invalid float value '~a'" value))))
    (:timestamp (cond
                  ((equal value :null) nil)
                  ((equal value :now) "now()")
                  ((and
                     (stringp value)
                     (re:scan *timestamp-regex* value))
                    (if quote (format nil "'~a'" value) value))
                  (t (error "Invalid timestamp string '~a'" value))))
    (:text (cond
             ((equal value :null) nil)
             ((stringp value)
               (if quote (format nil "'~a'" value) value))
             (t (error "Invalid text value '~a'" value))))
    (:uuid (cond
             ((equal value :null) nil)
             ((equal value :generate-uuid) "uuid_generate_v4()")
             ((and
                (stringp value)
                (re:scan *uuid-regex* value))
               value)
             (t (error "Invalid UUID value '~a'" value))))
    (otherwise (error "Unknown FIELD-TYPE-KEY '~a'" field-type-key))))

(defun compile-field (model type-key old-field-key new-field-key field-def)
  (loop
    with name-sql = (column-name type-key new-field-key field-def)
    and type-sql = (sql-type
                     (if (getf field-def :target)
                       :uuid
                       (getf field-def :type)))
    and column = (if (getf field-def :target)
                   t
                   (getf field-def :column))
    and primary-key = (when (getf field-def :primary-key) "primary key")
    and not-null = (if (getf field-def :target)
                     "not null"
                     (when (getf field-def :not-null) "not null"))
    with references = (when (getf field-def :target)
                        (format nil "references ~a(id) on delete cascade"
                          (table-name
                            type-key
                            (u:tree-get model type-key :base))))
    and unique = (when (getf field-def :unique) "unique")
    and default = (when (getf field-def :default)
                    (format nil "default ~a"
                      (value-sql
                        (getf field-def :default)
                        (getf field-def :type)
                        :quote t)))
    with sql-parts = (remove-if-not #'identity
                       (list name-sql type-sql primary-key not-null
                         references unique default))
    with new-def = (let ((name-key (u:make-keyword name-sql)))
                     (list
                       :name-sql name-sql
                       :type-sql type-sql
                       :create-sql (format nil "~{~a~^ ~}" sql-parts)
                       :name-key (u:make-keyword name-sql)
                       :source (unless (getf field-def :target)
                                 (or
                                   (getf field-def :source)
                                   (when column
                                     `(:first (:view :main
                                                :column ,name-key)))))
                       :type (if (getf field-def :target)
                               :uuid
                               (getf field-def :type))
                       :column column
                       :not-null (if (getf field-def :target)
                                   t
                                   (getf field-def :not-null))
                       :reference (when (equal old-field-key :reference) t)))
    with attrs = '(:checked :unchecked :ui :unique
                    :primary-key :fs-backed :target :ids-table :join-table)
    for attr in attrs
    append (list attr (getf field-def attr)) into def
    finally (return (append def new-def))))

(defun default-fields (&optional keys-only)
  (let ((fields '(:id
                   (:type :uuid
                     :source (:first (:view :main :column :id))
                     :column t
                     :primary-key t
                     :update nil
                     :default :generate-uuid)
                   :created-at
                   (:type :timestamp
                     :source (:first (:view :main :column :created-at))
                     :column t
                     :update nil
                     :not-null t
                     :default :now)
                   :updated-at
                   (:type :timestamp
                     :source (:first (:view :main :column :updated-at))
                     :column t
                     :update nil
                     :not-null t
                     :default :now))))
    (if keys-only
      (u:plist-keys fields)
      fields)))

(defun add-default-fields (type-key model)
  (append
    (default-fields)
    (u:deep-copy (u:tree-get model type-key :fields))))

(defun compile-fields (type-key model)
  (loop with fields = (add-default-fields type-key model)
    for old-field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for new-field-key = (if (equal old-field-key :reference)
                          (u:make-keyword
                            (table-reference (getf field-def :target)))
                          old-field-key)
    appending (list
                new-field-key
                (compile-field model type-key
                  old-field-key new-field-key field-def))))

(defun compile-type-def (type-key model)
  (let* ((type-def (getf model type-key))
          (base (getf type-def :base))
          (fields (compile-fields type-key model))
          (table-name (table-name type-key base))
          (views (if (getf type-def :views)
                   (u:deep-copy (getf type-def :views))
                   (if (getf type-def :is-joiner)
                     nil
                     `(:main (:tables (,type-key)))))))
    (list
      :table (getf type-def :table)
      :base base
      :is-joiner (getf type-def :is-joiner)
      :insert-sql nil
      :views views
      :deletable (getf type-def :deletable)
      :fs-backed (getf type-def :fs-backed)
      :table-name table-name
      :views-sql (unless (getf type-def :is-joiner)
                   (views-sql views model))
      :create-table-sql (create-table-sql table-name fields)
      :fields fields)))

(defun compile-model (model)
  (loop
    with full-model = (append *base-model* model)
    for type-key in (u:plist-keys full-model)
    for type-def in (u:plist-values full-model)
    for compiled-def = (compile-type-def type-key full-model)
    appending (list type-key compiled-def) into compiled-model
    finally
    (return
      (loop
        for type-key in compiled-model by #'cddr
        for type-def = (getf compiled-model type-key)
        for fields = (u:tree-get compiled-model type-key :fields)
        unless (getf type-def :is-joiner)
        do (setf
             (getf type-def :insert-sql) (insert-sql compiled-model type-key)
             (getf type-def :update-sql) (update-sql compiled-model type-key)
             (getf type-def :delete-sql) (delete-sql compiled-model type-key))
        appending (list type-key type-def)))))

(defun set-model (model)
  (loop
    with compiled-model = (compile-model model)
    for type-key in (u:plist-keys compiled-model)
    for type-def in (u:plist-values compiled-model)
    for table-name = (getf type-def :table-name)
    collect (list type-key table-name) into summary
    finally
    (setf *compiled-model* compiled-model)
    (return summary)))

(defun create-tables ()
  (loop with m = *compiled-model*
    for type-key in m by #'cddr
    for table = (u:tree-get m type-key :create-table-sql :table)
    for trigger = (u:tree-get m type-key :create-table-sql :trigger)
    do (a:with-rbac (*rbac*)
         (db:query table)
         (db:query trigger))))

(defun be-list-internal (type-key &key schema-only keys column)
  (let* ((m *compiled-model*)
          (sql (u:tree-get m type-key :views-sql :main))
          (full-sql (if schema-only (format nil "~a limit 1" sql) sql))
          (result (a:with-rbac (*rbac*)
                    (a:rbac-query (list full-sql)))))
    (if schema-only
      (u:plist-keys (car result))
      (cond
        (column (u:distinct-values
                  (mapcar (lambda (row) (getf row column)) result)))
        (keys (loop for row in result
                collect (loop for key in keys
                          append (list key (getf row key)))))
        (t result)))))

(defun model-for (type-key)
  (getf *compiled-model* type-key))

(defun model-views-for (type-key)
  (list
    :views (u:tree-get *compiled-model* type-key :views)
    :views-sql (u:tree-get *compiled-model* type-key :views-sql)))

(defun be-list (type-key &key (view-key :main) form filters)
  (declare (ignore form))
  (let* ((m *compiled-model*)
          (k type-key)
          (sql (u:tree-get m k :views-sql view-key :sql))
          (expected-filters (u:tree-get m k :views view-key :filter))
          (filter-values (loop for k in expected-filters
                           for v = (getf filters k)
                           when v collect v)))
    (a:with-rbac (*rbac*)
      (a:rbac-query (list sql filter-values)))))
