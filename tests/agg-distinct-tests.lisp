(in-package :data-ui)

(def-suite agg-distinct-suite
  :description "M2M row-display :source :agg contract: :distinct is
the only legal value on a :join-table field; omission injects it.")

(def-suite agg-distinct-compile-tests
  :in agg-distinct-suite
  :description "Pure compile-time probes (inline mini-models,
validate-model only: no DB, no set-model, no model context).")

(in-suite agg-distinct-compile-tests)

;;; The two behavioral tests near the end of this file belong to
;;; agg-distinct-suite for bookkeeping, but they must never run
;;; under a bare (run 'agg-distinct-suite): they require a model
;;; context (with-model), which only run-agg-distinct-tests
;;; supplies.

;;; --- Compile-time helpers (inline mini-models, validate-model only:
;;; no DB, no set-model) ---

(defun ad-joiner-model (&key tags-agg assignees-agg omit-tags-agg)
  "Minimal two-joiner-chain base model (:items with :tags and
:assignees M2M list fields) for agg probes. TAGS-AGG is the
:agg written on :tags :source (omitted when OMIT-TAGS-AGG).
ASSIGNEES-AGG defaults to :distinct so only :tags varies."
  (flet ((with-agg (base)
           (if omit-tags-agg
             base
             (append base (list :agg tags-agg)))))
    (list
      :items
      (list :table t :create :auto :update :auto :delete :auto
        :type-roles '("item-user")
        :views (list :main (list :tables '(:items :item-tags :tags
                                           :item-users :users)))
        :fields (list
                  :name (list :type :text :identity t
                          :ui (list :label "Name" :widget :textbox)
                          :source (list :view :main :column :name
                                    :agg :first)
                          :column t :not-null t)
                  :tags (list :type :list
                          :ui (list :label "Tags"
                                :widget :checkbox-list)
                          :source (with-agg
                                    (list :view :main :table :tags
                                      :column :name))
                          :source-all (list :view :tags :table :tags
                                        :column :name :agg :list)
                          :join-table :item-tags)
                  :assignees (list :type :list
                               :ui (list :label "Assignees"
                                     :widget :checkbox-list)
                               :source (list :view :main :table :users
                                         :column :name
                                         :agg (or assignees-agg
                                              :distinct))
                               :source-all (list :view :users
                                             :table :users
                                             :column :name :agg :list)
                               :join-table :item-users))
        :list-form (list :fields t)
        :update-form (list :fields t)
        :add-form (list :fields t))

      :tags
      (list :table t :create :auto :update :auto :delete :auto
        :type-roles '("item-user")
        :fields (list
                  :name (list :type :text :identity t
                          :ui (list :label "Tag" :widget :textbox)
                          :source (list :view :main :column :name
                                    :agg :first)
                          :column t :not-null t))
        :list-form (list :fields t)
        :update-form (list :fields t)
        :add-form (list :fields t))

      :item-tags
      (list :table t :is-joiner t :internal t
        :fields (list :reference (list :target :items)
                  :reference (list :target :tags)))

      :item-users
      (list :table t :is-joiner t :internal t
        :fields (list :reference (list :target :items)
                  :reference (list :target :users))))))

(defun ad-compiled-agg (types field-key)
  "Stage-1 compile TYPES; return the compiled :agg of FIELD-KEY
on :items."
  (u:tree-get (validate-model types) :items :fields field-key
    :source :agg))

(defun ad-error-message (thunk)
  "Run THUNK; return its error message string, or NIL when it
finishes."
  (handler-case
      (progn (funcall thunk) nil)
    (error (e) (princ-to-string e))))

(test ad-omitted-agg-injects-distinct
  "join-table + :source without :agg: the compiled :agg is
:distinct (the one omission default this rule adds)."
  (is (eq :distinct
       (ad-compiled-agg (ad-joiner-model :omit-tags-agg t) :tags))))

(test ad-explicit-distinct-stays
  "join-table + :agg :distinct stays :distinct."
  (is (eq :distinct
       (ad-compiled-agg (ad-joiner-model :tags-agg :distinct) :tags))))

(test ad-explicit-list-rejected
  "join-table + :agg :list is a compile error naming :distinct
(sibling-chain fan-out duplicates values; set semantics)."
  (let ((msg (ad-error-message
               (lambda ()
                 (validate-model
                   (ad-joiner-model :tags-agg :list))))))
    (is-true (and msg
              (search "must be :distinct" msg :test #'char-equal)))))

(test ad-explicit-first-rejected
  "join-table + :agg :first (any non-:distinct, non-list value)
is a compile error."
  (signals error
    (validate-model (ad-joiner-model :tags-agg :first))))

(test ad-explicit-avg-rejected
  "join-table + :agg :avg is a compile error."
  (signals error
    (validate-model (ad-joiner-model :tags-agg :avg))))

(test ad-source-all-list-stays-legal
  "join-table + :source :agg :distinct keeps :source-all at
:list (single-table view, no fan-out to dedupe)."
  (is (eq :list
       (u:tree-get
         (validate-model (ad-joiner-model :tags-agg :distinct))
         :items :fields :tags :source-all :agg))))

(test ad-no-join-table-list-compiles
  "No :join-table, :type :list, :agg :list (Model Bank images
style, 1:N via FK): compiles; the rule did not overreach."
  (finishes
    (validate-model
      (list
        :models
        (list :table t :create :auto :update :auto :delete :auto
          :type-roles '("model-user")
          :views (list :main (list :tables '(:models :images)))
          :fields (list
                    :name (list :type :text :identity t
                            :ui (list :label "Name" :widget :textbox)
                            :source (list :view :main :column :name
                                      :agg :first)
                            :column t :not-null t)
                    :images (list :type :list
                              :ui (list :label "Images"
                                    :widget :image-list)
                              :source (list :view :main :table :images
                                        :column :name :agg :list)))
          :list-form (list :fields t)
          :update-form (list :fields t)
          :add-form (list :fields t))
        :images
        (list :table t :create :auto :update :auto :delete :auto
          :type-roles '("model-user")
          :fields (list
                    :name (list :type :text :identity t
                            :ui (list :label "Image" :widget :textbox)
                            :source (list :view :main :column :name
                                      :agg :first)
                            :column t :not-null t))
          :list-form (list :fields t)
          :update-form (list :fields t)
          :add-form (list :fields t))))))

;;; --- Behavioral (DB) ---
;;;
;;; in-suite back to the parent suite: these two tests need a
;;; model context (m2m-test for two joiner chains, test-model for
;;; the single-chain regression), which run-agg-distinct-tests
;;; supplies via with-model + fiveam:run by test name. See the
;;; suite-head note.

(in-suite agg-distinct-suite)

(test ad-two-chains-values-appear-once
  "The chores bug, locked in: one item with 2 tags and 2
assignees; be-list shows each tag once and each assignee once
(two independent one-to-many chains no longer cross-product the
:list values)."
  (let ((item-id (be-insert :items
                   '(:name "Distinct Item"
                     :tags ("red" "blue")
                     :assignees ("admin" "guest"))
                   "admin")))
    (unwind-protect
         (let* ((records (getf (be-list :items "admin") :records))
                (rec (find "Distinct Item" records
                       :key (lambda (r) (getf r :name))
                       :test 'equal)))
           (is-true rec)
           (is (equal 2 (length (getf rec :tags)))
             "Expected exactly 2 tag values, got ~a"
             (getf rec :tags))
           (is (equal 2 (length (getf rec :assignees)))
             "Expected exactly 2 assignee values, got ~a"
             (getf rec :assignees))
           (is (equal '("blue" "red")
                  (sort (copy-list (getf rec :tags)) #'string<)))
           (is (equal '("admin" "guest")
                  (sort (copy-list (getf rec :assignees)) #'string<))))
      (be-delete :items item-id "admin"))))

(test ad-single-chain-round-trip
  "Single-chain regression under test-model (todos-style tags):
a one-tag item shows that tag exactly once."
  (let ((todo-id (be-insert :todos
                   '(:name "One Tag Todo" :points 1
                     :tags ("red"))
                   "admin")))
    (unwind-protect
         (let* ((records (getf (be-list :todos "admin") :records))
                (rec (find "One Tag Todo" records
                       :key (lambda (r) (getf r :name))
                       :test 'equal)))
           (is-true rec)
           (is (equal '("red") (getf rec :tags))))
      (be-delete :todos todo-id "admin"))))
