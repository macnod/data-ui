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

(defparameter *model*
  `(:users
     (:table t :base t
       ;; TODO: :create, :update, and :delete should use RBAC functions
       :create :auto :update :auto :delete :auto
       :views (:main (:users :role-users :roles))
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
                          :joiner :role-users))
       :rows-form (:render-mode :row :fields (:name t :email t :roles t))
       :update-form (:render-mode :form
                      :fields (:name t :email t :password t :roles t))
       :add-form (:render-mode :form
                   :fields (:name t :email t :password t)))

     :resources
     (:table t :base t
       ;; TODO: :create, :update, and :delete should use RBAC functions
       :create :auto :update :auto :delete :auto
       :views (:main (:resources :resource-roles :roles))
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
                          :joiner :role-users))
       :rows-form (:render-mode :row :fields (:name t :roles t))
       :update-form (:render-mode :form :fields (:name t :roles t))
       :add-form (:render-mode :form :fields (:name t :roles t)))

     :permissions
     (:table t :base t
       :create :auto :update :auto :delete :auto
       :deletable t
       :fields (:name (:type :text
                        :ui (:label "Permission" :input-type :line)
                        :source (:column :self)
                        :column t :not-null t :unique t))
       :rows-form (:render-mode :row :fields (:name t))
       :update-form (:render-mode :form :fields (:name t))
       :add-form (:render-mode :form :fields (:name t)))

     :roles
     (:table t :base t
       ;; TODO: :create, :update, and :delete should use RBAC functions
       :create :auto :update :auto :delete :auto
       :views (:main (:roles :role-permissions :permissions))
       :fields (:name (:type :text
                        :ui (:label "Role" :input-type :line)
                        :source (:first (:view :main :column :role-name))
                        :column t :not-null t :unique t)
                 :permissions (:type :list
                                :ui (:label "Permissions" :input-type :checkbox-list)
                                :source (:list (:view :main :column :permission-name))
                                :checked (:list (:view :main :column :permission-name))
                                :unchecked (:list (:table :permissions :column :permission-name))
                                :joiner :role-permissions))
       :rows-form (:render-mode :row :fields (:name t :permissions t))
       :update-form (:render-mode :form :fields (:name t :permissions t))
       :add-form (:render-mode :form :fields (:name t :permissions t)))

     :role-permissions
     (:table t :base t :is-joiner t
       :fields (:reference (:target :roles :lookup-field :name)
                 :reference (:target :permissions :lookup-field :name)))

     :resource-roles
     (:table t :base t :is-joiner t
       :fields (:reference (:target :resources :lookup-field :name)
                 :reference (:target :roles :lookup-field :name)))

     :role-users
     (:table t :base t :is-joiner t
       :fields (:reference (:target :roles :lookup-field :name)
                 :reference (:target :users :lookup-field :name)))

     :widget
     (:table t :base nil
       :create :auto :update :auto :delete :auto
       :views (:main (:widget :resource-roles :roles))
       :fields (:name (:type :text
                        :ui (:label "Widget" :input-type :line)
                        :source (:first (:view :main :column :widget-name))
                        :column t :not-null t :unique t)
                 :roles (:type :list
                          :ui (:label "Roles" :input-type :checkbox-list)
                          :source (:list (:view :main :column :role-name))
                          :checked (:list (:view :roles :column :role-name))
                          :unchecked (:list (:table :roles :column :role-name))))
       :rows-form (:render-mode :row :fields (:name t))
       :update-form (:render-mode :form :fields (:name t :roles t))
       :add-form (:render-form :row :fields (:name t :roles t)))))

(defparameter *compiled-model* nil)

(defun mapcar-key (key list-of-plists)
  (mapcar
    (lambda (plist) (getf plist key))
    list-of-plists))

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

(defun create-sql (table fields)
  (format
    nil
    "~%create table if not exists ~a (~%    ~{~a~^,~%    ~}~%)~%"
    table
    (mapcar
      (lambda (field) (getf field :create-sql))
      (remove-if-not
        (lambda (field) (getf field :column))
        (u:plist-values fields)))))

(defun sql-view (view model)
  "Converts a view like '(:users :user-roles :roles) into an SQL query string
that joins tables."
  (loop
    for first = t then nil
    for prev-table-key in view
    for prev-table-name = (table-name
                            prev-table-key
                            (u:tree-get model prev-table-key :base))
    for first-table-name = prev-table-name then first-table-name
    for prev-is-joiner = (u:tree-get model prev-table-key :is-joiner)
    for table-key in (cdr view)
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
    collect sql into sql-lines
    finally (return (format nil "~%select * from ~a~%~{  ~a~%~}"
                      first-table-name sql-lines))))

(defun sql-views (views model)
  (loop
    for view-key in views by #'cddr
    for view-def in (cdr views) by #'cddr
    append (list view-key (sql-view view-def model))))

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

(defun compile-field (type-key field-key field-def)
  (let* ((name-sql (column-name type-key field-key field-def))
          (type-sql (sql-type (getf field-def :type)))
          (primary-key (when (getf field-def :primary-key)
                         "primary key"))
          (not-null (when (getf field-def :not-null) "not null"))
          (unique (when (getf field-def :unique) "unique"))
          (default (when (getf field-def :default)
                     (format nil "default ~a"
                       (value-sql
                         (getf field-def :default)
                         (getf field-def :type)
                         :quote t))))
          (sql-parts (remove-if-not #'identity
                       (list name-sql type-sql primary-key not-null unique
                         default)))
          (new-def (list
                     :name-sql (column-name type-key field-key field-def)
                     :type-sql (sql-type (getf field-def :type))
                     :create-sql (format nil "~{~a~^ ~}" sql-parts)
                     :name-key (u:make-keyword name-sql))))
    (loop
      with attrs = '(:source :checked :unchecked :ui :column :not-null :unique
                      :joiner)
      for attr in attrs
      do (setf (getf new-def attr) (getf field-def attr)))
    new-def))

(defun default-fields (&optional keys-only)
  (let ((fields '(:id
                   (:type :uuid
                     :source (:first (:view :main :column :id))
                     :column t
                     :primary-key t
                     :default :generate-uuid)
                   :created-at
                   (:type :timestamp
                     :source (:first (:view :main :column :created-at))
                     :column t
                     :not-null t
                     :default :now)
                   :updated-at
                   (:type :timestamp
                     :source (:first (:view :main :column :updated-at))
                     :column t
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
    for field-key in fields by #'cddr
    for field-def in (cdr fields) by #'cddr
    appending (list field-key (compile-field type-key field-key field-def))))

(defun compile-type-def (type-key model)
  (let* ((type-def (getf model type-key))
          (base (getf type-def :base))
          (fields (compile-fields type-key model))
          (table-name (table-name type-key base))
          (views (if (getf type-def :views)
                   (u:deep-copy (getf type-def :views))
                   `(:main (,type-key)))))
    (list
      :table (getf type-def :table)
      :base base
      :views views
      :deletable (getf type-def :deletable)
      :fs-backed (getf type-def :fs-backed)
      :table-name table-name
      :sql-views (sql-views views model)
      :create-sql (create-sql table-name fields)
      :fields fields)))

(defun compile-model (model)
  (loop
    for type-key in (u:plist-keys model)
    for type-def in (u:plist-values model)
    for compiled-def = (compile-type-def type-key model)
    appending (list type-key compiled-def)))

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
