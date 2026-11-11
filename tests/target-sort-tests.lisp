;; Joined-column sort — :sortable t on :target (FK) fields.
;; Backlog item "Sort by =:target= field display column"; plan:
;; ~/workbench/sort-by-target-field-display-column-plan.org
;;
;; Compile pins run pure compile-model on inline models (no DB);
;; SQL-shape and behavior pins run against the target-sort-test
;; fixture, seeded once by tst-seed-fixture (read-only suite, the
;; dss-seed-fixture precedent).

(in-package :data-ui)

(def-suite target-sort-suite
  :description "Joined-column sort: :sortable t on :target fields —
compile legality (valid-sortable-target-fields), Phase A SQL shape,
and be-list behavior on the target-sort-test fixture.")

(in-suite target-sort-suite)

;; --------------------------------------------------------------------
;; Helpers
;; --------------------------------------------------------------------

(defun tst-users-source ()
  (list :view :main :table :users :column :name :agg :first))

(defun tst-things (&key (tables '(:things :users))
                     (owner (list :type :text :target :users
                              :sortable t
                              :source (tst-users-source)
                              :column t))
                     extra)
  "Types plist for one inline :things type — the sortable-tests
inline shape: a :name identity plus :owner (by default the legal
sortable :target shape). TABLES is the :main view's table list;
EXTRA is appended to the field plist. Pure compile-probe input."
  (list
    :things
    (list :table t :create :auto :update :auto :delete :auto
      :views (list :main (list :tables tables))
      :fields (append
                (list
                  :name (list :type :text :identity t
                          :source (list :view :main :column :name
                                    :agg :first)
                          :column t :not-null t)
                  :owner owner)
                extra)
      :list-form (list :fields t)
      :update-form (list :fields t)
      :add-form (list :fields t))))

(defun tst-seed-fixture ()
  "Seed the target-sort-test fixture once; the suite is read-only.
item-a fans out to two join rows (two tags); item-b and item-c tie
on both targets; item-d has neither target (NULL FKs)."
  (dolist (tag '("red" "blue"))
    (be-insert :tags `(:name ,tag) "admin"))
  (dolist (category '("alpha" "zeta"))
    (be-insert :categories `(:name ,category) "admin"))
  (be-insert :items '(:name "item-a" :owner "admin"
                      :category "zeta" :tags ("red" "blue"))
    "admin")
  (be-insert :items '(:name "item-b" :owner "guest"
                      :category "alpha" :tags ("red"))
    "admin")
  (be-insert :items '(:name "item-c" :owner "guest"
                      :category "alpha")
    "admin")
  (be-insert :items '(:name "item-d" :tags ("blue")) "admin")
  nil)

(defun tst-names (result)
  "Record names of a be-list result (the dss-names shape)."
  (loop for r in (getf result :records) collect (getf r :name)))

(defun tst-ids (result)
  "Record ids of a be-list result."
  (loop for r in (getf result :records) collect (getf r :id)))

(defun tst-owners (result)
  ":owner display values of a be-list result through value-or-nil,
so a NULL FK reads as nil (the nullable-fk-tests convention)."
  (loop for r in (getf result :records)
    collect (value-or-nil (getf r :owner))))

;; --------------------------------------------------------------------
;; Compile pins (pure compile-model; no DB)
;; --------------------------------------------------------------------

(test tst-legal-shape-compiles
  "The legal shape compiles (the blanket reject is gone) and the
compiled :owner carries :sortable t plus the joined display column
as its sort column. Baseline for the rejections below: each differs
from this by one mutation."
  (finishes (compile-model (tst-things)))
  (let ((m (compile-model (tst-things))))
    (is-true (u:tree-get m :things :fields :owner :sortable))
    (is (equal "users.user_name"
           (u:tree-get m :things :fields :owner :source :column-name)))))

(test tst-reject-target-outside-main-view
  "A sortable :target whose table is not in the :main view is a
compile error — base Phase A joins exactly the view's tables."
  (error-matches
    (compile-model (tst-things :tables '(:things)))
    "in the :main view's :tables"
    "view membership not enforced"))

(test tst-reject-display-off-target
  "A display :source with no :table resolves the sort column on the
base table (the row's own column), not the target — compile error."
  (error-matches
    (compile-model
      (tst-things
        :owner (list :type :text :target :users
                 :sortable t
                 :source (list :view :main :column :name :agg :first)
                 :column t)))
    "display :source whose :table is the target"
    "omitted display table not rejected"))

(test tst-reject-self-target
  "A sortable :target equal to its own type is a compile error — no
self-join alias; the sort column would read the base row itself."
  (error-matches
    (compile-model
      (tst-things
        :tables '(:things)
        :owner (list :type :text :target :things
                 :sortable t
                 :source (list :view :main :table :things
                           :column :name :agg :first)
                 :column t)))
    "shares its target with another field or targets its own type"
    "self-target not rejected"))

(test tst-reject-shared-target
  "Two singular fields sharing one target are both rejected,
whichever carries :sortable t: the walker joins the target once,
through the first field's FK, so sorting the second would order by
the first's referent (and the count is flag-independent)."
  ;; :owner sortable, :reviewer not — the joined member
  (error-matches
    (compile-model
      (tst-things
        :extra (list :reviewer
                 (list :type :text :target :users
                   :source (tst-users-source)
                   :column t))))
    "shares its target with another field or targets its own type"
    "shared target (sortable first) not rejected")
  ;; flags reversed: only :reviewer sortable — the dangerous
  ;; direction (the join rides :owner's FK)
  (error-matches
    (compile-model
      (tst-things
        :owner (list :type :text :target :users
                 :source (tst-users-source)
                 :column t)
        :extra (list :reviewer
                 (list :type :text :target :users
                   :sortable t
                   :source (tst-users-source)
                   :column t))))
    "shares its target with another field or targets its own type"
    "shared target (sortable second) not rejected"))

(test tst-reject-display-wrong-table
  "A display :source naming a different table (explicitly, not by
omission) is the same compile error — stage 2 would resolve the
sort column on the named table."
  (error-matches
    (compile-model
      (tst-things
        :owner (list :type :text :target :users
                 :sortable t
                 :source (list :view :main :table :things
                           :column :name :agg :first)
                 :column t)))
    "display :source whose :table is the target"
    "wrong display table not rejected"))

(test tst-reject-unresolvable-display
  "A display :source whose :column is not a column of the target
(:roles is :users' M2M field) would sort by NIL at click time —
compile error. Same condition covers a source view missing the
target and a source with no :view at all."
  (error-matches
    (compile-model
      (tst-things
        :owner (list :type :text :target :users
                 :sortable t
                 :source (list :view :main :table :users
                           :column :roles :agg :first)
                 :column t)))
    "resolves to a column of the target"
    "unresolvable display column not rejected"))

(test tst-reject-resources-target
  "A sortable :target of :resources is a compile error: the
injected :id field already targets :resources and its edge is
first in field order, so the join would read the row's own
resource."
  (error-matches
    (compile-model
      (list
        :things
        (list :table t :create :auto :update :auto :delete :auto
          :views (list :main (list :tables '(:things :resources)))
          :fields (list
                    :name (list :type :text :identity t
                            :source (list :view :main :column :name
                                      :agg :first)
                            :column t :not-null t)
                    :cover (list :type :text :target :resources
                             :sortable t
                             :source (list :view :main :table :resources
                                       :column :name :agg :first)
                             :column t))
          :list-form (list :fields t)
          :update-form (list :fields t)
          :add-form (list :fields t))))
    "row's own resource"
    ":resources target not rejected"))

(test tst-rollup-target-stays-forbidden
  ":target on a rollup field stays forbidden (valid-rollup-fields,
before any sortable rule runs) — the premise that keeps a rollup
from ever carrying an unchecked sortable target."
  (error-matches
    (compile-model
      (list
        :users-board
        (list :rollup t
          :grain :users
          :views (list :main (list :tables '(:users)))
          :list-form (list :fields t)
          :fields (list
                    (list :name
                      (list :source (list :view :main :table :users
                                      :column :name :agg :first)
                        :target :users
                        :sortable t))
                    (list :user-count
                      (list :type :integer
                        :source (list :view :main :table :users
                                  :column :id :agg :count)))))))
    "Field attribute :TARGET is not allowed on rollup field"
    "rollup :target not rejected"))

;; --------------------------------------------------------------------
;; SQL-shape pins (fixture context; pure functions over
;; *compiled-model*)
;; --------------------------------------------------------------------

(test tst-flags-and-wire
  "The fixture's two target fields compile sortable and fe-fields
forwards :sortable for both (the clickable-header gate)."
  (is-true (u:tree-get *compiled-model* :items :fields :owner :sortable))
  (is-true (u:tree-get *compiled-model* :items :fields :category :sortable))
  (let ((list-form (getf (fe-fields :items "admin") :list-form)))
    (is-true (getf (getf list-form :owner) :sortable))
    (is-true (getf (getf list-form :category) :sortable))))

(test tst-no-sort-index-for-target
  "No sort index is emitted for a target field (the ORDER BY column
lives on the joined table; the join probes the target by primary
key). The timestamp indexes prove the list is live."
  (let ((sort-index (u:tree-get *compiled-model*
                       :items :create-table-sql :sort-index)))
    (is-false
      (some (lambda (ddl) (search "item_owner" ddl)) sort-index))
    (is-false
      (some (lambda (ddl) (search "item_category" ddl)) sort-index))
    (is-true
      (some (lambda (ddl) (search "ix_rt_items_created_at" ddl))
        sort-index))))

(test tst-join-selection
  "phase-a-join-required-p keys off join-filters and target sorts;
phase-a-select-sql picks join vs base SQL accordingly."
  (is-true (phase-a-join-required-p :items nil '(:owner :asc)))
  (is-false (phase-a-join-required-p :items nil '(:name :asc)))
  (is-false (phase-a-join-required-p :items nil nil))
  (is-true (phase-a-join-required-p :items
             '((:tags :name :in ("red"))) '(:name :asc)))
  (is (equal (u:tree-get *compiled-model* :items :views :main
               :phase-a-join-sql)
         (phase-a-select-sql :items nil '(:owner :asc))))
  (is (equal (u:tree-get *compiled-model* :items :views :main
               :phase-a-base-sql)
         (phase-a-select-sql :items nil '(:name :asc)))))

(test tst-order-by-display-column
  "The sort key is the joined display column (stage 2's
:source :column-name), never the local UUID column, with the base
id as tiebreaker."
  (is (equal "users.user_name"
         (phase-a-order-by-column :items '(:owner :asc))))
  (is (equal "rt_categories.category_name"
         (phase-a-order-by-column :items '(:category :asc))))
  (let ((clause (phase-a-build-order-by :items '(:owner :desc))))
    (is-true (search "users.user_name DESC" clause))
    (is-true (search "rt_items.id" clause))))

;; --------------------------------------------------------------------
;; Behavior pins (seeded rows; read-only)
;; --------------------------------------------------------------------

(test tst-owner-asc
  "Owner ASC orders by the joined user name with NULL last and the
base id tiebreaker; :total counts distinct base rows; the echo
reports the effective sort."
  (let ((result (be-list :items "admin" :sort '(:owner :asc)
                    :limit 100)))
    (is (equal "item-a" (first (tst-names result))))
    (let ((pair (subseq (tst-names result) 1 3)))
      (is-true (member "item-b" pair :test #'equal))
      (is-true (member "item-c" pair :test #'equal)))
    (is (equal "item-d" (fourth (tst-names result))))
    (is (equal '("admin" "guest" "guest" nil) (tst-owners result)))
    (is (= 4 (getf result :total)))
    (is (equal '(:field :owner :dir :asc) (getf result :sort)))))

(test tst-owner-desc
  "Owner DESC applies the direction to the display column only:
NULL first, item-a last, and the tied pair keeps its ascending
relative order (tiebreaker stays id ASC)."
  (let* ((asc (be-list :items "admin" :sort '(:owner :asc)
                  :limit 100))
         (desc (be-list :items "admin" :sort '(:owner :desc)
                   :limit 100)))
    (is (equal "item-d" (first (tst-names desc))))
    (is (equal "item-a" (fourth (tst-names desc))))
    (is (equal (subseq (tst-names asc) 1 3)
           (subseq (tst-names desc) 1 3)))))

(test tst-category-rides-its-own-edge
  "The category sort reads its own joined column, not the owner's:
a different order over the same rows."
  (let ((result (be-list :items "admin" :sort '(:category :asc)
                    :limit 100)))
    (let ((pair (subseq (tst-names result) 0 2)))
      (is-true (member "item-b" pair :test #'equal))
      (is-true (member "item-c" pair :test #'equal)))
    (is (equal "item-a" (third (tst-names result))))
    (is (equal "item-d" (fourth (tst-names result))))))

(test tst-fanout-total-and-paging
    "item-a's two join rows do not multiply the result: DISTINCT
absorbs the fan-out, the spliced projection keeps pages stable at
one record, and :total stays 4 on every page."
    (let ((result (be-list :items "admin" :sort '(:owner :asc)
                      :limit 100)))
      (is (= 4 (length (getf result :records))))
      (is (= 4 (getf result :total))))
    (let* ((unpaged (tst-ids
                      (be-list :items "admin" :sort '(:owner :asc)
                        :limit 100)))
           (pages (loop for offset from 0 below 4
                    collect (be-list :items "admin"
                              :sort '(:owner :asc)
                              :limit 1 :offset offset))))
      (is-true (every (lambda (p) (= 4 (getf p :total))) pages))
      (is-true (every (lambda (p) (= 1 (length (getf p :records))))
                   pages))
      (is (equal unpaged
             (apply #'append (mapcar #'tst-ids pages))))))

(test tst-join-filter-plus-target-sort
  "A join-table filter and a target sort in one query: the filter
narrows to item-a / item-b, the sort still orders by the joined
column, and paging stays honest."
  (let ((result (be-list :items "admin"
                   :filters '((:tags :name :in ("red")))
                   :sort '(:owner :asc) :limit 100)))
    (is (equal '("item-a" "item-b") (tst-names result)))
    (is (= 2 (getf result :total))))
  (let ((result (be-list :items "admin"
                   :filters '((:tags :name :in ("red")))
                   :sort '(:owner :asc) :limit 1 :offset 1)))
    (is (equal '("item-b") (tst-names result)))
    (is (= 2 (getf result :total)))))

(test tst-own-table-filter-plus-target-sort
  "A filter and a sort on the same joined table: the users filter
returns exactly item-b and item-c under the owner sort."
  (let ((result (be-list :items "admin"
                   :filters '((:users :name :in ("guest")))
                   :sort '(:owner :asc) :limit 100)))
    (is (= 2 (length (getf result :records))))
    (is-true (member "item-b" (tst-names result) :test #'equal))
    (is-true (member "item-c" (tst-names result) :test #'equal))
    (is (= 2 (getf result :total)))))

(test tst-default-sort-is-target
  "A :default-sort naming a target field flows through the request
path: no request sort returns the explicit-call id sequence and
echoes the declaration."
  (let* ((default (be-list :items "admin" :limit 100))
         (explicit (be-list :items "admin" :sort '(:owner :asc)
                       :limit 100)))
    (is (equal (tst-ids explicit) (tst-ids default)))
    (is (equal '(:field :owner :dir :asc) (getf default :sort)))))

(test tst-base-column-sort-unchanged
  "A base-column sort still runs on the base SQL and orders by the
stored column."
  (let ((result (be-list :items "admin" :sort '(:name :asc)
                    :limit 100)))
    (is (equal '("item-a" "item-b" "item-c" "item-d")
           (tst-names result)))))

(test tst-search-plus-target-sort
  "Search composes with a target sort: the OR group narrows, the
join still orders, and :total reflects the searched set."
  (let ((result (be-list :items "admin" :search "item-"
                   :sort '(:owner :asc) :limit 100)))
    (is (= 4 (length (getf result :records))))
    (is (equal '("admin" "guest" "guest" nil) (tst-owners result))))
  (let ((result (be-list :items "admin" :search "item-b"
                   :sort '(:owner :asc) :limit 100)))
    (is (= 1 (length (getf result :records))))
    (is (= 1 (getf result :total)))))
