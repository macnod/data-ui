(in-package :data-ui)

(def-suite default-sort-suite
  :description ":default-sort type attribute: compile validation
(shape, unknown / non-sortable field), nil-sort resolution in
be-list (base + rollup), :sort echo, absence regressions")

(in-suite default-sort-suite)

;;; --- Helpers ---

(defun dss-compile-base (default-sort)
  "Compile a minimal base type :dss with sortable :name / :points
and never-sortable :notes; :default-sort is DEFAULT-SORT. Returns
the compiled :dss type-def. Pure compile probe, no DB."
  (getf
    (compile-model
      (list :dss
        (list :table t :create :auto :update :auto :delete :auto
          :type-roles '("dss-user")
          :default-sort default-sort
          :views (list :main (list :tables '(:dss)))
          :fields (list
                    :name (list :type :text :identity t :sortable t
                            :ui (list :label "Name" :widget :textbox)
                            :source (list :view :main :column :name
                                      :agg :first)
                            :column t :not-null t)
                    :points (list :type :integer :sortable t
                              :ui (list :label "Points" :widget :textbox)
                              :source (list :view :main :column :points
                                        :agg :first)
                              :column t)
                    :notes (list :type :text
                             :ui (list :label "Notes" :widget :textbox)
                             :source (list :view :main :column :notes
                                       :agg :first)
                             :column t))
          :list-form (list :fields t)
          :update-form (list :fields t)
          :add-form (list :fields t))))
    :dss))

(defun dss-fact-type ()
  "Fact table for the rollup compile probes (grain :users comes
from *base-model*)."
  (list :dss-items
    (list :table t :create :auto :update :auto :delete :auto
      :type-roles '("dss-user")
      :views (list :main (list :tables '(:dss-items :users)))
      :fields (list
                :name (list :type :text :identity t
                        :ui (list :label "Item" :widget :textbox)
                        :source (list :view :main :column :name
                                  :agg :first)
                        :column t :not-null t)
                :user (list :type :text
                        :ui (list :label "User" :widget :textbox)
                        :target :users
                        :source (list :view :main :table :users
                                  :column :name :agg :first)
                        :column t :not-null t)
                :points (list :type :integer
                          :ui (list :label "Points" :widget :textbox)
                          :source (list :view :main :column :points
                                    :agg :first)
                          :column t :not-null t))
      :list-form (list :fields t)
      :update-form (list :fields t)
      :add-form (list :fields t))))

(defun dss-board (&rest overrides)
  "Sortable rollup board spec over :dss-items, with OVERRIDES
merged last (ru-with pattern; no duplicate keys)."
  (ru-with
    (list
      :rollup t
      :grain :users
      :type-roles '("board-viewers")
      :views (list :main (list :tables '(:users :dss-items)))
      :list-form (list :fields t)
      :fields (list
                (list :name
                  (list :source (list :view :main :table :users
                                  :column :name :agg :first)
                    :sortable t
                    :ui (list :label "User")))
                (list :item-count
                  (list :type :integer
                    :source (list :view :main :table :dss-items
                              :column :id :agg :count)
                    :sortable t
                    :ui (list :label "Items")))
                (list :total-points
                  (list :type :integer
                    :source (list :view :main :table :dss-items
                              :column :points :agg :sum)
                    :sortable t
                    :ui (list :label "Points")))))
    overrides))

(defun dss-compile-rollup (board-spec)
  "Compile a model of the fact table + BOARD-SPEC as :dss-board.
Returns the compiled :dss-board type-def. Pure compile probe."
  (getf
    (compile-model
      (append (dss-fact-type)
        (list :dss-board board-spec)))
    :dss-board))

(defun dss-seed-fixture ()
  "Seed the default-sort-test fixture:

- ash: 1 item, 10 points
- bo: 3 items, 2 points each (6 total)
- cy: 2 items, 4 points each (8 total)

The board orders differ on purpose: item-count desc is bo cy ash
while total-points desc is ash cy bo, so a declared default is
distinguishable from the first-sortable-measure policy. The three
:items-plain rows are inserted in ascending-points order, so their
id order differs from any points sort. Read-only suite: nothing
is inserted or deleted inside tests."
  (dolist (u '("ash" "bo" "cy"))
    (th-make-user u :roles '("item-users" "board-viewers")))
  (be-insert :items '(:name "ash-only" :user "ash" :points 10) "admin")
  (dolist (n '("bo-1" "bo-2" "bo-3"))
    (be-insert :items `(:name ,n :user "bo" :points 2) "admin"))
  (dolist (n '("cy-1" "cy-2"))
    (be-insert :items `(:name ,n :user "cy" :points 4) "admin"))
  (loop for (name . points) in '(("ip-low" . 1) ("ip-mid" . 5)
                                  ("ip-hi" . 10))
    do (be-insert :items-plain `(:name ,name :points ,points) "admin"))
  nil)

(defun dss-names (result)
  (loop for r in (getf result :records) collect (getf r :name)))

(defun dss-points (result)
  (loop for r in (getf result :records) collect (getf r :points)))

(defun dss-seeded-names (result)
  (remove-if-not
    (lambda (n) (member n '("ash" "bo" "cy") :test #'equal))
    (dss-names result)))

;;; --- Compile: happy path ---

(test dss-base-declaration-verbatim
  "A well-formed declaration compiles and survives verbatim into
the compiled base type-def."
  (let ((def (dss-compile-base '(:points :desc))))
    (is (equal '(:points :desc) (getf def :default-sort)))))

(test dss-base-direction-less-verbatim
  "The direction-less 1-element form compiles (walker arm +
valid-default-sort) and is stored verbatim, never normalized."
  (let ((def (dss-compile-base '(:points))))
    (is (equal '(:points) (getf def :default-sort)))))

(test dss-rollup-declaration-verbatim
  "A declaration naming a sortable measure compiles on a rollup
and survives verbatim."
  (let ((def (dss-compile-rollup
               (dss-board :default-sort '(:total-points :desc)))))
    (is (equal '(:total-points :desc) (getf def :default-sort)))))

(test dss-absent-is-nil
  "No declaration: the compiled defs carry :default-sort nil,
same as :tree nil."
  (is-false (getf (dss-compile-base nil) :default-sort))
  (is-false (getf (dss-compile-rollup (dss-board)) :default-sort)))

;;; --- Compile: rejection ---

(test dss-reject-unknown-field
  "A declaration naming an unknown field is a compile error."
  (signals error
    (dss-compile-base '(:nope :desc))))

(test dss-reject-non-sortable-base
  "A declaration naming a non-sortable base field (:notes) is a
compile error."
  (signals error
    (dss-compile-base '(:notes :desc))))

(test dss-reject-non-sortable-measure
  "A declaration naming a :list measure (never :sortable t, by the
09 compile rule) is a compile error."
  (signals error
    (dss-compile-rollup
      (list
        :rollup t
        :grain :users
        :type-roles '("board-viewers")
        :default-sort '(:titles :desc)
        :views (list :main (list :tables '(:users :dss-items)))
        :list-form (list :fields t)
        :fields (list
                  (list :name
                    (list :source (list :view :main :table :users
                                    :column :name :agg :first)
                      :sortable t
                      :ui (list :label "User")))
                  (list :titles
                    (list :type :text
                      :source (list :view :main :table :dss-items
                                :column :name :agg :list)
                      :ui (list :label "Titles"))))))))

(test dss-reject-bad-direction
  "A direction other than :asc / :desc is a compile error."
  (signals error
    (dss-compile-base '(:points :sideways))))

(test dss-reject-non-list
  "A non-list declaration (bare keyword) is a compile error."
  (signals error
    (dss-compile-base :points)))

(test dss-reject-three-elements
  "A three-element declaration is a compile error (multi-key sorts
are a non-goal)."
  (signals error
    (dss-compile-base '(:points :desc :more))))

;;; --- Base behavior ---

(test dss-base-nil-sort-uses-declaration
  "be-list with nil sort on a declared base type orders by the
declaration: points desc (10 4 4 2 2 2), not id order."
  (let ((r (be-list :items "admin" :limit 100)))
    (is (equal '(10 4 4 2 2 2) (dss-points r)))))

(test dss-base-echo-names-declaration
  "The :sort echo reports the effective sort (:field :points
:dir :desc), not :null."
  (let ((r (be-list :items "admin")))
    (is (equal '(:field :points :dir :desc) (getf r :sort)))))

(test dss-base-explicit-sort-wins
  "An explicit request sort overrides the declaration."
  (let ((r (be-list :items "admin" :limit 100 :sort '(:points :asc))))
    (is (equal '(2 2 2 4 4 10) (dss-points r)))
    (is (equal '(:field :points :dir :asc) (getf r :sort)))))

(test dss-base-explicit-other-field-wins
  "An explicit sort on a different field also wins."
  (let ((r (be-list :items "admin" :limit 100 :sort '(:name :asc))))
    (is (equal "ash-only" (first (dss-names r))))
    (is (equal '(:field :name :dir :asc) (getf r :sort)))))

(test dss-plain-base-absence-unchanged
  "No declaration: id-ASC order (UUID ids, so the order is the
table's id order, not insertion order) and :sort echo :null (JSON
null), byte-for-byte the old behavior."
  (let* ((r (be-list :items-plain "admin" :limit 100))
         (by-id (loop for row in (a:with-rbac (*rbac*)
                                  (a:rbac-query
                                    '("select id, items_plain_name from rt_items_plain order by id")
                                    :alists))
                      collect (cons (cdr (assoc :id row))
                                    (cdr (assoc :items-plain-name row)))))
         (id->name (lambda (id)
                     (cdr (assoc id by-id :test #'equal)))))
    ;; same record set, in table id order
    (is (equal (mapcar (lambda (pair) (cdr pair)) by-id)
             (remove-if-not
               (lambda (n) (member n '("ip-low" "ip-mid" "ip-hi")
                              :test #'equal))
               (mapcar (lambda (rec) (funcall id->name (getf rec :id)))
                 (getf r :records)))))
    ;; stable across repeated nil-sort calls
    (is (equal (mapcar (lambda (rec) (funcall id->name (getf rec :id)))
                 (getf r :records))
             (mapcar (lambda (rec) (funcall id->name (getf rec :id)))
               (getf (be-list :items-plain "admin" :limit 100)
                 :records))))
    (is (eq :null (getf r :sort)))))

;;; --- Rollup behavior ---

(test dss-rollup-declaration-beats-policy
  "Nil sort on a declared rollup orders by the declared measure
(total-points desc: ash cy bo), not the first-sortable-measure
policy (item-count desc: bo cy ash)."
  (let ((r (be-list :board "admin" :limit 100)))
    (is (equal '("ash" "cy" "bo") (dss-seeded-names r)))
    (is (equal '(:field :total-points :dir :desc) (getf r :sort)))))

(test dss-rollup-explicit-sort-wins
  "An explicit sort on a declared rollup overrides the
declaration."
  (let ((r (be-list :board "admin" :limit 100
               :sort '(:item-count :asc))))
    (is (equal '("ash" "cy" "bo") (dss-seeded-names r)))
    (is (equal '(:field :item-count :dir :asc) (getf r :sort)))))

(test dss-rollup-plain-absence-unchanged
  "No declaration on a rollup: the first-sortable-measure policy
(item-count desc: bo cy ash) is untouched."
  (let ((r (be-list :board-plain "admin" :limit 100)))
    (is (equal '("bo" "cy" "ash") (dss-seeded-names r)))
    (is (equal '(:field :item-count :dir :desc) (getf r :sort)))))

