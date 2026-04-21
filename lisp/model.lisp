(in-package :data-ui)

(defun general-type (field-type-key)
  (u:tree-get *field-types* field-type-key :general))

(defun sql-type (type-key field-key field-type-key)
  (or (u:tree-get *field-types* field-type-key :sql)
    (error "Unsupported field type key ~s for field ~s ~s."
      field-type-key type-key field-key)))

(defun validation-error-string (type-key field-key message)
  (let ((type (u:singular (format nil "~(~a~)" type-key)))
         (field (format nil "~(~a~)" field-key)))
    (format nil "~a ~a ~a" type field message)))

(defun v-required (type-key field-key value)
  (unless
    (case (general-type (u:tree-get *compiled-model*
                          type-key :fields field-key :type))
      (:text (and (not (null value)) (not (equal value ""))))
      (:number (not (null value)))
      (:list (not (zerop (length value)))))
    (validation-error-string type-key field-key "is required.")))

(defparameter *validation-map*
  (list
    :required #'v-required))

(defparameter *base-model*
`(:users
     (:table t :base t
       ;; TODO: :create, :update, and :delete should use RBAC functions
       :create :auto :update :auto :delete :auto
       :views (:main (:tables (:users :role-users :roles))
                :roles (:tables (:roles)))
       :deletable t
       :fields (:name (:type :text
                        :source (:view :main :column :name :agg :first)
                        :ui (:label "Username" :input-type :line)
                        :validations (:required)
                        :column t :not-null t :unique t)
                 :password (:type :password
                             :source (:view :main :column :password :agg :first)
                             :force-sql-name "password_hash"
                             :ui (:label "Password" :input-type :password)
                             :validations (:required)
                             :column t :not-null t)
                 :email (:type :text
                          :force-sql-name "email"
                          :source (:view :main :column :email :agg :first)
                          :ui (:label "Email" :input-type :line)
                          :validations (:required)
                          :column t :not-null t)
                 :roles (:type :list
                          :ui (:label "Roles" :input-type :checkbox-list)
                          :source (:view :main :table :roles :column :name :agg :list)
                          :source-all (:view :roles :table :roles :column :name :agg :list)
                          :ids-table :roles
                          :join-table :role-users))
       :list-form (:fields (:name :email :roles))
       :update-form (:fields t)
       :add-form (:fields t))

     :resources
     (:table t :base t
       ;; TODO: :create, :update, and :delete should use RBAC functions
       :create :auto :update :auto :delete :auto
       :views (:main (:tables (:resources :resource-roles :roles))
                :roles (:tables (:roles)))
       :deletable t
       :fields (:name (:type :text
                        :ui (:label "Resource" :input-type :line)
                        :source (:view :main :column :name :agg :first)
                        :validations (:required)
                        :column t :not-null t :unique t)
                 :roles (:type :list
                          :ui (:label "Roles" :input-type :checkbox-list)
                          :source (:view :main :table :roles :column :name :agg :list)
                          :source-all (:view :roles :table :roles :column :name :agg :list)
                          :validations (:required)
                          :ids-table :roles
                          :join-table :resource-roles))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :permissions
     (:table t :base t
       :create :auto :update :auto :delete :auto
       :deletable t
       :fields (:name (:type :text
                        :ui (:label "Permission" :input-type :line)
                        :source (:view :main :column :name :agg :first)
                        :column t :not-null t :unique t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :roles
     (:table t :base t
       ;; TODO: :create, :update, and :delete should use RBAC functions
       :create :auto :update :auto :delete :auto
       :views (:main (:tables (:roles :role-permissions :permissions))
                :permissions (:tables (:permissions)))
       :fields (:name (:type :text
                        :ui (:label "Role" :input-type :line)
                        :source (:view :main :column :name :agg :first)
                        :column t :not-null t :unique t)
                 :permissions (:type :list
                                :ui (:label "Permissions" :input-type :checkbox-list)
                                :source (:view :main
                                          :table :permissions
                                          :column :name
                                          :agg :list)
                                :source-all (:view :permissions
                                              :table :permissions
                                              :column :name
                                              :agg :list)
                                :ids-table :permissions
                                :join-table :role-permissions))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

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

;; TODO: Ensure tables are created in the right order. The :todo-tags table
;;       references the :todos and :tags tables, so those tables should
;;       already exist before the :todo-tags table is created.
(defparameter *model*
  `(:todos
     (:table t
       :create :auto :update :auto :delete :auto
       :views (:main (:tables (:todos :todo-tags :tags))
                :tags (:tables (:tags)))
       :fields (:name (:type :text
                        :ui (:label "To Do" :input-type :line)
                        :source (:view :main :column :name :agg :first)
                        :column t :not-null t :unique t)
                 :tags (:type :list
                         :ui (:label "Tags" :input-type :checkbox-list)
                         :source (:view :main :table :tags :column :name :agg :list)
                         :source-all (:view :tags :table :tags :column :name :agg :list)
                         :ids-table :tags
                         :join-table :todo-tags))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :tags
     (:table t
       :create :auto :update :auto :delete :auto
       :fields (:name (:type :text
                        :ui (:label "Tag" :input-type :line)
                        :source (:view :main :table :tags :column :name :agg :first)
                        :column t :not-null t :unique t))
       :list-form (:fields t)
       :update-form (:fields t)
       :add-form (:fields t))

     :todo-tags
     (:table t :is-joiner t
       :fields (:reference (:target :todos)
                 :reference (:target :tags)))))

(defun base-model-types (&optional keys-only)
  (if keys-only
    (u:plist-keys *base-model*)
    *base-model*))

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

(defun column-name (model type-key field-key field-def)
  (when (or (getf field-def :column) (getf field-def :target))
    (let* ((table-name (table-name type-key (u:tree-get model type-key :base)))
            (singular-table-name (re:regex-replace "^rt_" (u:singular table-name) ""))
            (force-sql-name (getf field-def :force-sql-name))
            (target (getf field-def :target))
            (field-name (to-sql-identifier field-key)))
      (cond
        (target (to-sql-identifier field-key))
        (force-sql-name force-sql-name)
        ((member field-key (default-fields :keys-only t))
          (to-sql-identifier field-key))
        (t (format nil "~a_~a" singular-table-name field-name))))))

(defun column-alias (model type-key field-key field-def)
  (let* ((table-name (table-name type-key (u:tree-get model type-key :base)))
          (singular-table-name (u:singular table-name))
          (force-sql-name (getf field-def :force-sql-name))
          (target (getf field-def :target))
          (field-name (to-sql-identifier field-key)))
    (cond
      (target (to-sql-identifier field-key))
      (force-sql-name (format nil "~a_~a" singular-table-name force-sql-name))
      ((member field-key (default-fields :keys-only t))
        (format nil "~a_~a" singular-table-name field-name))
      (t (format nil "~a_~a" singular-table-name field-name)))))

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

(defun filtered-fields (fields keys)
  "Returns field key and definition for fields that have non-NIL values for all
KEYS."
  (loop
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    when (every (lambda (k) (getf field-def k)) keys)
    append (list field-key field-def)))

(defun fields-attribute (fields have-keys &optional attribute)
  "Returns a list of cons pairs of field key and the value of ATTRIBUTE for
fields that have non-NIL values for all HAVE-KEYS."
  (loop with some-fields = (filtered-fields fields have-keys)
    for field-key in some-fields by #'cddr
    for field-def in (cdr some-fields) by #'cddr
    collect
    (cons
      field-key
      (if attribute
        (getf field-def attribute)
        field-def))))

(defun fk-columns (model type-key)
  (loop with fields = (u:tree-get model type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for joiner = (getf field-def :join-table)
    when joiner
    append (fields-attribute
             (u:tree-get model joiner :fields) '(:target) :name-sql)))

(defun insert-keys (model type-key)
  (loop
    with fields = (u:tree-get model type-key :fields)
    and base = (u:tree-get model type-key :base)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    when (getf field-def :join-table)
    collect field-key into keys
    finally (return
              (if base
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
    with main-columns = (cons '(:id . "id")
                          (fields-attribute fields '(:column :ui) :name-sql))
    with fk-columns = (fk-columns model type-key)
    with sql = "insert into ~a (~{~a~^, ~}) values (~{~a~^, ~}) returning id"
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

(defun insert-key (key)
  (u:make-keyword (format nil "~a-insert" key)))

(defun delete-key (key)
  (u:make-keyword (format nil "~a-delete" key)))

(defun original-key (key)
  (u:make-keyword
    (re:regex-replace "-(insert|delete)$" (format nil "~(~a~)" key) "")))

(defun update-keys (model type-key)
  (loop
    with fields = (u:tree-get model type-key :fields)
    and base = (u:tree-get model type-key :base)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for join-table = (getf field-def :join-table)
    when join-table
    append (list field-key join-table)
    into keys
    finally (return (append (list :main type-key) keys))))

(defun update-sql (model type-key)
  (loop
    with fields = (u:tree-get model type-key :fields)
    with update-keys = (update-keys model type-key)
    with columns = (fields-attribute fields '(:column :ui) :name-sql)
    with fk-columns = (fk-columns model type-key)
    with main-sql = "update ~a set ~{~a = ~a~^, ~} where id = $~d"
    with delete-sql = "delete from ~a where ~{~a = ~a~^ and ~}"
    with insert-sql = "insert into ~a (~{~a~^, ~}) values (~{~a~^, ~})"
    for key in update-keys by #'cddr
    for table in (cdr update-keys) by #'cddr
    for table-name = (table-name table (u:tree-get model table :base))
    for table-cols = (case key (:main columns) (otherwise fk-columns))
    for sql = (case key
                (:main main-sql)
                (otherwise (list
                             :insert insert-sql
                             :delete delete-sql)))
    append
    (list key (case key
                (:main
                  (cons
                    (format nil sql
                      table-name
                      (u:zip (mapcar #'cdr table-cols) (placeholders table-cols))
                      (1+ (length table-cols)))
                    (append (mapcar #'car table-cols) '(:id))))
                (t
                  (list
                    :delete (cons
                              (format nil (getf sql :delete)
                                table-name
                                (u:zip
                                  (mapcar #'cdr table-cols)
                                  (placeholders table-cols))
                                (mapcar #'car table-cols))
                              (mapcar #'car table-cols))
                    :insert (cons
                              (format nil (getf sql :insert)
                                table-name
                                (mapcar #'cdr table-cols)
                                (placeholders table-cols))
                              (mapcar #'car table-cols))))))))

(defun table-column (model type-key field-key)
  (let* ((type-def (u:tree-get model type-key))
          (base (u:tree-get type-def :base))
          (field-def (u:tree-get type-def :fields field-key))
          (table-name (table-name type-key base))
          (column-name (column-name model type-key field-key field-def))
          (column-list (list table-name column-name))
          (column-string (format nil "~{~a~^.~}" column-list))
          (alias-list (list table-name column-name))
          (alias-string (format nil "~{~a~^_~}" alias-list)))
    (values column-string alias-string)))

(defun table-columns (model type-key)
  (loop
    with base = (u:tree-get model type-key :base)
    and fields = (u:tree-get model type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for (column alias) = (multiple-value-bind (c a)
                           (table-column model type-key field-key)
                           (list c a))
    when (getf field-def :column)
    collect (list
              :table type-key
              :field-key field-key
              :alias alias
              :alias-key (u:make-keyword alias)
              :column column
              :column-key (u:make-keyword column))))

(defun max-string-width (strings)
  (loop for s in strings
    for l = (length s)
    for max = l then (if (> l max) l max)
    finally (return max)))

(defun formatted-table-columns (model type-keys)
  (loop for type-key in type-keys
    for table-columns = (table-columns model type-key)
    for cc = (mapcar (lambda (c) (getf c :column)) table-columns)
    for aa = (mapcar (lambda (c) (getf c :alias)) table-columns)
    append cc into columns
    append aa into aliases
    finally (let* ((width (max-string-width columns))
                    (format-string (format nil "~~~da" width))
                    (fcolumns (mapcar
                                (lambda (s) (format nil format-string s))
                                columns)))
              (return
                (mapcar
                  (lambda (a b) (format nil "~a ~b" a b))
                  fcolumns aliases)))))

(defun view-sql (model view)
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
    for sql = (format nil "left join ~a on ~a = ~a"
                table-name
                prev-table-ref
                table-ref)
    unless (equal first-table-name table-name)
    collect sql into sql-lines
    unless is-joiner collect table-key into table-keys
    finally
    (return
      (format nil "~%select~%~{  ~a~^,~%~}~%from ~a~%~{  ~a~%~}"
        (formatted-table-columns model (cons first-table-key table-keys))
        first-table-name
        sql-lines))))

(defun keyword-matches (keyword regex)
  (re:scan (format nil "(?i)~a" regex) (format nil "~s" keyword)))

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

(defun field-source (field-def name-key)
  (or
    (getf field-def :source)
    (when (getf field-def :column)
      `(:view :main :column ,name-key :agg :first))))

(defun compile-field (model type-key old-field-key new-field-key field-def)
  (loop
    with force-sql-name = (getf field-def :force-sql-name)
    with name-sql = (or
                      force-sql-name
                      (column-name model type-key new-field-key field-def))
    and type-sql = (sql-type
                     type-key
                     new-field-key
                     (if (getf field-def :target) :uuid (getf field-def :type)))
    and column = (if (getf field-def :target)
                   t
                   (getf field-def :column))
    and primary-key = (when (getf field-def :primary-key) "primary key")
    and not-null = (if (getf field-def :target)
                     "not null"
                     (when (getf field-def :not-null) "not null"))
    with target = (getf field-def :target)
    with references = (when target
                        (format nil "references ~a(id) on delete cascade"
                          (table-name
                            target
                            (u:tree-get model target :base))))
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
                       :force-sql-name force-sql-name
                       :name-sql name-sql
                       :type-sql type-sql
                       :create-sql (format nil "~{~a~^ ~}" sql-parts)
                       :source (field-source field-def name-key)
                       :source-sel (getf field-def :source-sel)
                       :source-all (getf field-def :source-all)
                       :type (if (getf field-def :target)
                               :uuid
                               (getf field-def :type))
                       :column column
                       :not-null (if (getf field-def :target)
                                   t
                                   (getf field-def :not-null))
                       :reference (when (equal old-field-key :reference) t)))
    with attrs = '(:base-field :checked :unchecked :ui :unique
                    :primary-key :fs-backed :target :ids-table :join-table)
    for attr in attrs
    append (list attr (getf field-def attr)) into def
    finally (return (append def new-def))))

(defun compile-field-stage-2 (model type-key field-def)
  (cond
    ((u:tree-get field-def :source :view)
      (let* ((table-key (or (u:tree-get field-def :source :table) type-key))
              (column-key (u:tree-get field-def :source :column))
              (view-key (u:tree-get field-def :source :view))
              (alias (u:tree-get model type-key :views view-key :aliases table-key column-key))
              (column (u:tree-get model type-key :views view-key :columns table-key column-key)))
        (add-to-plist
          field-def
          (list
            :source (add-to-plist
                      (u:tree-get field-def :source)
                      (list
                        :alias-key (u:make-keyword alias)
                        :column-name column))))))
    ((and
       (u:tree-get field-def :source-sel :view)
       (u:tree-get field-def :source-all :view))
      (let* ((sel-table-key (u:tree-get field-def :source-sel :table))
              (all-table-key (u:tree-get field-def :source-all :table))
              (sel-column-key (u:tree-get field-def :source-sel :column))
              (all-column-key (u:tree-get field-def :source-all :column))
              (sel-view-key (u:tree-get field-def :source-sel :view))
              (all-view-key (u:tree-get field-def :source-all :view))
              (sel-alias (u:tree-get model type-key :views sel-view-key
                           :aliases sel-table-key sel-column-key))
              (all-alias (u:tree-get model type-key :views all-view-key
                           :aliases all-table-key all-column-key)))
        (add-to-plist
          field-def
          (list
            :source-sel (add-to-plist
                          (u:tree-get field-def :source-sel)
                          (list :alias-key (u:make-keyword sel-alias)))
            :source-all (add-to-plist
                          (u:tree-get field-def :source-all)
                          (list :alias-key (u:make-keyword all-alias)))))))
    (t (copy-seq field-def))))

(defun compile-fields-stage-2 (model type-key)
  (loop with fields = (u:tree-get model type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    appending
    (list
      field-key
      (compile-field-stage-2 model type-key field-def))))

(defun default-fields (&key model type-key keys-only)
  (let* ((is-joiner (u:tree-get model type-key :is-joiner))
          (base (u:tree-get model type-key :base))
          (target-resources (when
                             (or
                               (not model)
                               (not type-key)
                               keys-only
                               (and (not is-joiner) (not base)))
                             t))
          (fields `(:id
                     (:type :uuid
                       :source (:view :main :column :id :agg :first)
                       :column t
                       :primary-key t
                       :update nil
                       :target ,(when target-resources :resources)
                       :base-field t
                       :default ,(when is-joiner :generate-uuid))
                     :created-at
                     (:type :timestamp
                       :source (:view :main :column :created-at :agg :first)
                       :column t
                       :update nil
                       :not-null t
                       :default :now
                       :base-field t)
                     :updated-at
                     (:type :timestamp
                       :source (:view :main :column :updated-at :agg :first)
                       :column t
                       :update nil
                       :not-null t
                       :default :now
                       :base-field t))))
    (if keys-only
      (u:plist-keys fields)
      fields)))

(defun add-default-fields (type-key model)
  (append
    (default-fields :model model :type-key type-key)
    (u:deep-copy (u:tree-get model type-key :fields))))

(defun compile-fields (type-key model)
  (loop with fields = (add-default-fields type-key model)
    for old-field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for new-field-key = (if (equal old-field-key :reference)
                          (u:make-keyword
                            (table-reference (getf field-def :target)))
                          old-field-key)
    appending
    (list
      new-field-key
      (compile-field model type-key old-field-key new-field-key field-def))))

(defun view-aliases (model view-def)
  (loop for table-key in (u:tree-get view-def :tables)
    unless (u:tree-get model table-key :is-joiner)
    append
    (list table-key
      (loop for column in (table-columns model table-key)
        append (list (getf column :field-key) (getf column :alias-key))))))

(defun view-columns (model view-def)
  (loop for table-key in (u:tree-get view-def :tables)
    unless (u:tree-get model table-key :is-joiner)
    append
    (list table-key
      (loop for column in (table-columns model table-key)
        append (list (getf column :field-key) (getf column :column))))))

(defun enrich-views (model type-key)
  (loop
    with type-def = (getf model type-key)
    with views = (let ((v (getf type-def :views))
                        (j (getf type-def :is-joiner)))
                   (if v
                     (u:deep-copy v)
                     (if j nil `(:main (:tables (,type-key))))))
    for view-key in views by #'cddr
    for view-def in (cdr views) by #'cddr
    for view-def-new = (list
                         :tables (getf view-def :tables)
                         :sql (view-sql model view-def)
                         :aliases (view-aliases model view-def)
                         :columns (view-columns model view-def))
    appending (list view-key view-def-new)))

(defun add-to-plist (plist plist-new)
  (loop with keys = (u:distinct-values
                      (append
                        (u:plist-keys plist)
                        (u:plist-keys plist-new)))
    for key in keys
    for value = (or (getf plist-new key) (getf plist key))
    appending (list key value)))

(defun remove-null-value-pairs (plist)
  (loop for k in plist by #'cddr
    for v in (cdr plist) by #'cddr
    when v append (list k v)))

(defun type-has-roles (type-key type-def)
  (unless (or
            (equal type-key :resources)
            (getf type-def :is-joiner))
    t))

(defun compile-type-def (model type-key)
  (let* ((type-def (getf model type-key))
          (base (getf type-def :base))
          (fields (compile-fields type-key model))
          (table-name (table-name type-key base))
          (roles (when (type-has-roles type-key type-def)
                   (getf type-def :type-roles '("admin")))))
    (add-to-plist
      type-def
      (list
        :type-roles roles
        :table-name table-name
        :fields fields))))

(defun stage-1 (model)
  (loop
    with full-model = (append *base-model* model)
    for type-key in full-model by #'cddr
    for type-def in (cdr full-model) by #'cddr
    for compiled-def = (compile-type-def full-model type-key)
    appending (list type-key compiled-def)))

(defun stage-2 (model)
  (loop
    for type-key in model by #'cddr
    for type-def in (cdr model) by #'cddr
    for base = (getf type-def :base)
    for fields = (getf type-def :fields)
    for joiner = (getf type-def :is-joiner)
    for table-name = (table-name type-key base)
    for views = (enrich-views model type-key)
    for insert-sql = (unless joiner (insert-sql model type-key))
    for update-sql = (unless joiner (update-sql model type-key))
    for delete-sql = (unless joiner (delete-sql model type-key))
    for create-table-sql = (create-table-sql table-name fields)
    for new-def = (remove-null-value-pairs
                    (list
                      :create-table-sql create-table-sql
                      :views views
                      :insert-sql insert-sql
                      :update-sql update-sql
                      :delete-sql delete-sql
                      :create-table-sql create-table-sql))
    appending (list type-key (add-to-plist type-def new-def))))

(defun stage-3 (model)
  (loop
    for type-key in model by #'cddr
    for type-def in (cdr model) by #'cddr
    for fields = (compile-fields-stage-2 model type-key)
    for new-def = (add-to-plist type-def (list :fields fields))
    appending (list type-key new-def)))

(defun compile-model (model)
  (let* ((model-1 (stage-1 model))
          (model-2 (stage-2 model-1))
          (model-3 (stage-3 model-2)))
    model-3))

(defun set-model (model)
  (loop
    initially (setf *compiled-model* nil)
    with compiled-model = (compile-model model)
    for type-key in (u:plist-keys compiled-model)
    for type-def in (u:plist-values compiled-model)
    for table-name = (getf type-def :table-name)
    appending (list type-key table-name) into summary
    finally
    (setf *compiled-model* compiled-model)
    (create-tables)
    (add-type-roles)
    (return summary)))

(defun create-tables ()
  (loop with m = *compiled-model*
    for type-key in m by #'cddr
    for table-name = (u:tree-get m type-key :table-name)
    for table = (u:tree-get m type-key :create-table-sql :table)
    for trigger = (u:tree-get m type-key :create-table-sql :trigger)
    unless (a:with-rbac (*rbac*)
             (a:rbac-query
               (list
                 "select 1 from information_schema.tables where table_name = $1"
                 table-name)
               :single))
    do
    (a:with-rbac (*rbac*) (db:query table) (db:query trigger))
    (pl:pdebug :in "create-tables" :state "added table and triggers"
      :table table-name :type-key type-key)))

(defun type-resource-name (type-key)
  (to-sql-identifier type-key :format-string "type-~a"))

(defun add-type-roles ()
  (loop with m = *compiled-model*
    for type-key in m by #'cddr
    for type-def in (cdr m) by #'cddr
    for roles = (getf type-def :type-roles)
    for resource-name = (type-resource-name type-key)
    when (and
           (type-has-roles type-key (getf m type-key))
           (not (a:get-id *rbac* "resources" resource-name)))
    do
    (pl:pdebug :in "add-type-roles" :state "adding type role"
      :type-key type-key :resource-name resource-name :type-roles roles)
    (a:add-resource *rbac* resource-name :roles roles)))

(defun model-for (type-key)
  (getf *compiled-model* type-key))

(defun model-views-for (type-key)
  (list :views (u:tree-get *compiled-model* type-key :views)))

(defun model-view-for (type-key view-key)
  (list view-key (u:tree-get *compiled-model* type-key :views view-key)))

(defun model-field-for (type-key field-key)
  (list type-key
    (list :fields
      (list field-key
        (u:tree-get *compiled-model* type-key :fields field-key)))))

(defun model-sql-for (type-key)
  (loop for key in '(:create-table-sql :insert-sql :update-sql :delete-sql)
    for value = (u:tree-get *compiled-model* type-key key)
    append (list key value) into plist
    finally (return (list type-key plist))))

(defun model-fields-for (type-key)
  (list type-key
    (list :fields (u:tree-get *compiled-model* type-key :fields))))

(defun model-view-sql-for (type-key &key (view-key :main))
  (u:tree-get *compiled-model* type-key :views view-key :sql))

(defun model-field-names-for (type-key &key
                               (keys '(:name-sql :alias-key)))
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    append (list field-key
             (loop for key in keys
               appending (list key (getf field-def key))))))

(defun model-field-create-sql-for (type-key)
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for sql = (getf field-def :create-sql)
    append (list field-key sql)))

(defun model-field-attribute-for (type-key attribute)
  (loop with fields = (u:tree-get *compiled-model* type-key :fields)
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    for attr-value = (getf field-def attribute)
    append (list field-key attr-value)))
