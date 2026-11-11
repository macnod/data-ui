;; Purpose: Timestamp presentation-override tests (backlog item
;;   "Timestamp field overrides"): ambient :sortable t on the
;;   injected timestamps, the closed override grammar on
;;   :created-at / :updated-at, the DML column exclusion (5a), and
;;   wire / behavior coverage on the timestamp-override-test
;;   fixture. Compile probes run bare against compile-model's
;;   return value; wire probes run under with-model.
;; Author: Rose (via implement-data-ui-plan)
;; Created: 2026-11-18
;; Prompt: (see ~/workbench/timestamp-field-overrides-plan.org)

(in-package :data-ui)

(def-suite timestamp-override-suite
  :description "Compile-time probes for ambient timestamp :sortable t
and presentation overrides on injected default fields.")

(in-suite timestamp-override-suite)

;; --------------------------------------------------------------------
;; Shared inline-model builders (types plist only — merge-with-base-model
;; runs inside compile-model; probes read the returned model, never a
;; global; see the sortable-target-field-compiles precedent).
;; --------------------------------------------------------------------

(defun tro-base-types ()
  "Types plist for a minimal ordinary rt_ type with no overrides."
  (list
    :notes
    (list :table t :create :auto :update :auto :delete :auto
      :views (list :main (list :tables '(:notes)))
      :fields (list
                :note (list :type :text :identity t
                        :source (list :view :main :column :note
                                  :agg :first)
                        :column t :not-null t))
      :list-form (list :fields t)
      :update-form (list :fields t)
      :add-form (list :fields t))))

(defun tro-posts-types ()
  "Types plist for the blog spelling: :created-at full :ui override,
:updated-at label-only override, :default-sort (:created-at :desc)."
  (list
    :posts
    (list :table t :create :auto :update :auto :delete :auto
      :views (list :main (list :tables '(:posts)))
      :default-sort '(:created-at :desc)
      :fields (list
                :title (list :type :text :identity t
                         :source (list :view :main :column :title
                                   :agg :first)
                         :column t :not-null t)
                :body (list :type :text
                        :source (list :view :main :column :body
                                  :agg :first)
                        :column t)
                :created-at (list
                              :ui (list :label "Posted"
                                    :widget :textbox
                                    :read-only t))
                :updated-at (list
                              :ui (list :label "Updated"
                                    :read-only t)))
      :list-form (list :fields '(:title :created-at :updated-at))
      :update-form (list :fields t)
      :add-form (list :fields t))))

;; --------------------------------------------------------------------
;; Ambient :sortable t (injection)
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; Ambient :sortable t (injection)
;; --------------------------------------------------------------------

(test tro-ambient-sortable-on-ordinary-type
  "The compiler injects :sortable t on :created-at / :updated-at of
an ordinary type; :id stays unsortable."
  (let ((m (compile-model (tro-base-types))))
    (is-true (u:tree-get m :notes :fields :created-at :sortable))
    (is-true (u:tree-get m :notes :fields :updated-at :sortable))
    (is-false (u:tree-get m :notes :fields :id :sortable))))

(test tro-ambient-sortable-on-users
  "The injection is shape-uniform: base built-in :users gets
:sortable t timestamps too."
  (let ((m (compile-model (list))))
    (is-true (u:tree-get m :users :fields :created-at :sortable))
    (is-true (u:tree-get m :users :fields :updated-at :sortable))))

(test tro-author-field-sortable-rule-unchanged
  "Not all fields sortable: an author field without :sortable t stays
nil (opt-in rule unchanged)."
  (let ((m (compile-model (tro-base-types))))
    (is-false (u:tree-get m :notes :fields :note :sortable))))

;; --------------------------------------------------------------------
;; Override merge (positive)
;; --------------------------------------------------------------------

(test tro-override-merge-both-keys
  "The blog spelling compiles: :created-at full :ui, :updated-at
label-only (finalize-ui fills :widget :textbox). The injection's
structural keys survive the merge."
  (let ((m (compile-model (tro-posts-types))))
    (is (equal :timestamp
           (u:tree-get m :posts :fields :created-at :type)))
    (is (equal "Posted"
           (u:tree-get m :posts :fields :created-at :ui :label)))
    (is (equal :textbox
           (u:tree-get m :posts :fields :created-at :ui :widget)))
    (is-true (u:tree-get m :posts :fields :created-at :ui :read-only))
    (is-true (u:tree-get m :posts :fields :created-at :sortable))
    (is-true (u:tree-get m :posts :fields :created-at :not-null))
    (is (equal :now (u:tree-get m :posts :fields :created-at :default)))
    (is-true (u:tree-get m :posts :fields :created-at :base-field))
    ;; label-only spelling on :updated-at
    (is (equal "Updated"
           (u:tree-get m :posts :fields :updated-at :ui :label)))
    (is (equal :textbox
           (u:tree-get m :posts :fields :updated-at :ui :widget)))
    (is-true (u:tree-get m :posts :fields :updated-at :ui :read-only))
    (is-true (u:tree-get m :posts :fields :updated-at :sortable))))

(test tro-single-emission-of-injected-keys
  "Each injected key occurs exactly once in the compiled :fields —
the merge never emits an author copy twice."
  (let ((m (compile-model (tro-posts-types))))
    (dolist (key '(:id :created-at :updated-at))
      (is (= 1
             (loop for field-key in (u:tree-get m :posts :fields)
                    by #'cddr
               count (eq field-key key)))))))

(test tro-sortable-restatement-inside-valid-override
  ":sortable t restated inside an otherwise valid override is a legal
no-op."
  (finishes
    (compile-model
      (list
        :notes
        (list :table t :create :auto :update :auto :delete :auto
          :views (list :main (list :tables '(:notes)))
          :fields (list
                    :note (list :type :text :identity t
                            :source (list :view :main :column :note
                                      :agg :first)
                            :column t :not-null t)
                    :created-at (list :sortable t
                                  :ui (list :label "Posted"
                                        :read-only t)))
          :list-form (list :fields t)
          :update-form (list :fields t)
          :add-form (list :fields t))))))

;; --------------------------------------------------------------------
;; DML exclusion (the regression guard for 5a)
;; --------------------------------------------------------------------

(test tro-dml-excludes-timestamps-overridden-type
  ":created-at / :updated-at absent from :insert-sql / :update-sql
:main parameter keys and statement strings on the overridden type
(non-vacuous: both timestamps carry :ui there)."
  (let ((m (compile-model (tro-posts-types))))
    (dolist (stmt-key '(:insert-sql :update-sql))
      (let ((main (u:tree-get m :posts stmt-key :main)))
        (is-false (member :created-at (cdr main)))
        (is-false (member :updated-at (cdr main)))
        (is-false (search "created_at" (car main)))
        (is-false (search "updated_at" (car main)))))))

(test tro-dml-excludes-timestamps-unoverridden-type
  "Same exclusion on the unoverridden type (:notes, no :ui on the
timestamps)."
  (let ((m (compile-model (tro-base-types))))
    (dolist (stmt-key '(:insert-sql :update-sql))
      (let ((main (u:tree-get m :notes stmt-key :main)))
        (is-false (member :created-at (cdr main)))
        (is-false (member :updated-at (cdr main)))
        (is-false (search "created_at" (car main)))
        (is-false (search "updated_at" (car main)))))))

(test tro-dml-id-cases
  "The three :id DML cases: present in insert params on an ordinary
type, absent from :users (base) insert params, present as the final
parameter key of update-sql :main on both."
  (let ((posts (compile-model (tro-posts-types)))
        (users (compile-model (list))))
    (let ((posts-main (u:tree-get posts :posts :insert-sql :main))
          (users-main (u:tree-get users :users :insert-sql :main)))
      (is (eq :id (car (last (cdr posts-main)))))
      (is-false (member :id (cdr users-main))))
    (is (eq :id
          (car (last (cdr (u:tree-get
                            posts :posts :update-sql :main))))))
    (is (eq :id
          (car (last (cdr (u:tree-get
                            users :users :update-sql :main))))))))

;; --------------------------------------------------------------------
;; Sort-index DDL and sort legality
;; --------------------------------------------------------------------

(test tro-sort-index-ddl-emitted
  "Fresh rt_ table DDL gains ix_rt_posts_created_at /
ix_rt_posts_updated_at."
  (let ((m (compile-model (tro-posts-types))))
    (let ((ddls (u:tree-get m :posts :create-table-sql :sort-index)))
      (is-true (some (lambda (ddl) (search "ix_rt_posts_created_at" ddl))
                  ddls))
      (is-true (some (lambda (ddl) (search "ix_rt_posts_updated_at" ddl))
                  ddls)))))

(test tro-default-sort-without-override-compiles
  ":default-sort naming :created-at compiles with no author
:created-at declaration (fails valid-default-sort today)."
  (finishes
    (compile-model
      (list
        :notes
        (list :table t :create :auto :update :auto :delete :auto
          :views (list :main (list :tables '(:notes)))
          :default-sort '(:created-at :desc)
          :fields (list
                    :note (list :type :text :identity t
                            :source (list :view :main :column :note
                                      :agg :first)
                            :column t :not-null t))
          :list-form (list :fields t)
          :update-form (list :fields t)
          :add-form (list :fields t))))))

;; --------------------------------------------------------------------
;; Rejections
;; --------------------------------------------------------------------

(defun tro-types-with (field-forms)
  "Types plist with :notes' :fields extended by FIELD-FORMS (the
override under test spliced after :note)."
  (list
    :notes
    (list :table t :create :auto :update :auto :delete :auto
      :views (list :main (list :tables '(:notes)))
      :fields (append
                (list
                  :note (list :type :text :identity t
                          :source (list :view :main :column :note
                                    :agg :first)
                          :column t :not-null t))
                field-forms)
      :list-form (list :fields t)
      :update-form (list :fields t)
      :add-form (list :fields t))))

(defun tro-error-string (thunk)
  "Run THUNK, return nil when it returns normally, else the error
message string."
  (handler-case
      (progn (funcall thunk) nil)
    (error (e) (format nil "~a" e))))

(test tro-reject-id-override
  "An author :id plist names :id and v1 in the error."
  (signals error
    (compile-model
      (tro-types-with
        (list :id (list :ui (list :widget :textbox :read-only t))))))
  (let ((msg (tro-error-string
               (lambda ()
                 (compile-model
                   (tro-types-with
                     (list :id (list :ui (list :widget :textbox
                                           :read-only t)))))))))
    (is (search ":ID" msg))
    (is (search "v1" msg))))

(test tro-reject-structural-keys
  "A structural key (:default, :source) names the legal set."
  (dolist (bad
            (list
              (list :created-at
                (list :default "2020-01-01T00:00:00"
                  :ui (list :label "Posted" :read-only t)))
              (list :created-at
                (list :source (list :view :main :column :id
                                :agg :first)
                  :ui (list :label "Posted" :read-only t)))))
    (signals error (compile-model (tro-types-with bad)))
    (let ((msg (tro-error-string
                 (lambda () (compile-model (tro-types-with bad))))))
      (is (search "only" msg))
      (is (search "may be overridden" msg)))))

(test tro-reject-sortable-nil
  ":sortable nil names the Model value vocabulary deferral."
  (signals error
    (compile-model
      (tro-types-with
        (list :created-at
          (list :sortable nil
            :ui (list :label "Posted" :read-only t))))))
  (let ((msg (tro-error-string
               (lambda ()
                 (compile-model
                   (tro-types-with
                     (list :created-at
                       (list :sortable nil
                         :ui (list :label "Posted"
                               :read-only t)))))))))
    (is (search "Model value vocabulary" msg))))

(test tro-reject-missing-ui
  "An override without :ui signals: (:created-at ()) and
(:created-at (:sortable t)) both."
  (signals error
    (compile-model (tro-types-with (list :created-at (list)))))
  (signals error
    (compile-model
      (tro-types-with (list :created-at (list :sortable t))))))

(test tro-reject-read-only-missing-or-nil
  ":ui without :read-only and :read-only nil both name :read-only."
  (dolist (bad-ui
            (list (list :widget :textbox)
              (list :widget :textbox :read-only nil)))
    (signals error
      (compile-model
        (tro-types-with
          (list :created-at (list :ui bad-ui)))))
    (let ((msg (tro-error-string
                 (lambda ()
                   (compile-model
                     (tro-types-with
                       (list :created-at (list :ui bad-ui))))))))
      (is (search "read-only" msg)))))

(test tro-reject-bad-widgets
  ":widget :textarea, :hidden, and explicit nil all name :widget."
  (dolist (bad-widget '(:textarea :hidden nil))
    (signals error
      (compile-model
        (tro-types-with
          (list :created-at
            (list :ui (list :widget bad-widget :read-only t))))))
    (let ((msg (tro-error-string
                 (lambda ()
                   (compile-model
                     (tro-types-with
                       (list :created-at
                         (list :ui (list :widget bad-widget
                                     :read-only t)))))))))
      (is (search "widget" msg)))))

(test tro-reject-duplicate-override
  "Two :created-at entries signal the duplicate message, not silent
first-wins."
  (signals error
    (compile-model
      (tro-types-with
        (list
          :created-at (list :ui (list :label "One" :read-only t))
          :created-at (list :ui (list :label "Two" :read-only t))))))
  (let ((msg (tro-error-string
               (lambda ()
                 (compile-model
                   (tro-types-with
                     (list
                       :created-at (list :ui (list :label "One"
                                              :read-only t))
                       :created-at (list :ui (list :label "Two"
                                              :read-only t)))))))))
    (is (search "redeclared more" msg))))

(test tro-joiner-reference-repeat-still-legal
  "A repeated :reference on an inline joiner type still compiles (the
M2M idiom stays legal)."
  (let ((types (append
                 (tro-base-types)
                 (list
                   :tags (list :table t :create :auto :update :auto
                           :delete :auto
                           :views (list :main (list :tables '(:tags)))
                           :fields (list
                                     :tag (list :type :text
                                            :identity t
                                            :source (list :view :main
                                                      :column :tag
                                                      :agg :first)
                                            :column t :not-null t))
                           :list-form (list :fields t)
                           :update-form (list :fields t)
                           :add-form (list :fields t))
                   :note-tags (list :table t :create :auto
                                :update :auto :delete :auto
                                :is-joiner t
                                :fields (list
                                          :reference (list :target :notes)
                                          :reference (list :target :tags)))))))
    (finishes (compile-model types))))

;; --------------------------------------------------------------------
;; Purity
;; --------------------------------------------------------------------

(test tro-no-base-model-mutation
  "*base-model* untouched after a compile with overrides."
  (let ((before (u:deep-copy *base-model*)))
    (compile-model (tro-posts-types))
    (is (equal before *base-model*))))

(test tro-phase-a-order-by-accepts-timestamp
  "phase-a-order-by-column accepts (:created-at :asc) on the fixture
(previously report-ve 'not sortable'). Runs inside with-model: the
function reads *compiled-model*, which compile-model does not set."
  (with-model "timestamp-override-test" nil
    (is (equal "rt_posts.created_at"
           (phase-a-order-by-column :posts '(:created-at :asc))))))

(def-suite timestamp-override-suite-wire
  :description "Wire / behavior probes for timestamp presentation
overrides on the timestamp-override-test fixture.")

(in-suite timestamp-override-suite-wire)

(test tro-wire-fe-fields-posts
  "fe-fields list-form on :posts shows the Posted / Updated
overrides with :sortable t and read-only (fe-fields spreads :ui
keys flat into the field entry, the sortable-forwarded precedent)."
  (let ((list-form (getf (fe-fields :posts "admin") :list-form)))
    (is (equal "Posted" (getf (getf list-form :created-at) :label)))
    (is-true (getf (getf list-form :created-at) :sortable))
    (is-true (getf (getf list-form :created-at) :read-only))
    (is (equal :textbox (getf (getf list-form :created-at) :widget)))
    (is (equal "Updated" (getf (getf list-form :updated-at) :label)))
    (is-true (getf (getf list-form :updated-at) :sortable))
    (is-true (getf (getf list-form :updated-at) :read-only))
    (is (equal :textbox (getf (getf list-form :updated-at) :widget)))))

(test tro-wire-notes-regression
  ":notes names both timestamps in its list form yet neither reaches
the wire (no :ui, gate still closed) — non-vacuous absence."
  (let ((list-form (getf (fe-fields :notes "admin") :list-form)))
    (is-false (u:tree-get list-form :created-at))
    (is-false (u:tree-get list-form :updated-at))))

(test tro-wire-users-regression
  "The base model's :users list form names the timestamps and gets
only the fields with widgets."
  (let ((list-form (getf (fe-fields :users "admin") :list-form)))
    (is (equal '(:name :email :roles)
           (u:plist-keys list-form)))))

(test tro-wire-insert-read
  "Insert works with the DML exclusion; timestamps read back
non-null (form :list-form passed explicitly — rec's default is
:update-form). Cleanup under unwind-protect via be-delete."
  (let ((id (be-insert :posts '(:title "a") "admin")))
    (unwind-protect
         (let* ((record (getf (rec id "admin" :type-key :posts
                              :form :list-form)
                            :record))
                (created (getf record :created-at))
                (updated (getf record :updated-at)))
           (is-true (and created (stringp created)))
           (is-true (and updated (stringp updated))))
      (be-delete :posts id "admin"))))

(test tro-wire-update-preserves-created-at
  "Smoke: seed created_at to 2020-01-01 (deterministic half — the
trigger makes updated_at seeding impossible), be-update, re-read
with :form :list-form: :created-at still 2020, :updated-at
non-null. Without 5a this probe still passes (UPDATE would rebind
the record's own created_at); the compiled parameter-key and
SQL-string assertions in the compile suite are the regression
guard. Seeding through the query helper (a:with-rbac context);
cleanup by id under unwind-protect."
  (let ((id (be-insert :posts '(:title "a") "admin")))
    (unwind-protect
         (progn
           (query "update rt_posts set created_at = '2020-01-01' where id = $1"
             :params (list id))
           (be-update :posts id '(:title "b") "admin")
           (let* ((record (getf (rec id "admin" :type-key :posts
                                :form :list-form)
                              :record)))
             (is (search "2020-01-01" (getf record :created-at)))
             (is-true (getf record :updated-at))))
      (be-delete :posts id "admin"))))

(test tro-wire-be-list-ordering
  "Three rows with fixed, distinct created_at values (test-only SQL
through query); exact title sequences for :asc / :desc; the
no-request path returns :default-sort order and the :sort echo.
Cleanup by id for all three under unwind-protect."
  (let ((id-a (be-insert :posts '(:title "oldest") "admin"))
        (id-b (be-insert :posts '(:title "middle") "admin"))
        (id-c (be-insert :posts '(:title "newest") "admin")))
    (unwind-protect
         (progn
           (query "update rt_posts set created_at = '2021-01-01' where id = $1"
             :params (list id-a))
           (query "update rt_posts set created_at = '2022-01-01' where id = $1"
             :params (list id-b))
           (query "update rt_posts set created_at = '2023-01-01' where id = $1"
             :params (list id-c))
           (let ((asc (mapcar (lambda (r) (getf r :title))
                           (getf (be-list :posts "admin"
                                    :sort '(:created-at :asc)
                                    :limit nil)
                             :records)))
                 (desc (mapcar (lambda (r) (getf r :title))
                            (getf (be-list :posts "admin"
                                     :sort '(:created-at :desc)
                                     :limit nil)
                              :records)))
                 (default (mapcar (lambda (r) (getf r :title))
                               (getf (be-list :posts "admin"
                                        :limit nil)
                                 :records))))
             (is (equal '("oldest" "middle" "newest") asc))
             (is (equal '("newest" "middle" "oldest") desc))
             (is (equal '("newest" "middle" "oldest") default))
             (is (equal '(:field :created-at :dir :desc)
                    (getf (be-list :posts "admin" :limit nil)
                      :sort)))))
      (dolist (id (list id-a id-b id-c))
        (be-delete :posts id "admin")))))
