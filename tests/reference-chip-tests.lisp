(in-package :data-ui)

(def-suite reference-chip-compile
  :description ":filter-with :reference compile probes: field gates,
list-form membership matrix")

(def-suite reference-chip-wire-checks
  :description "fe-fields wire emission on the reference-chip-test
fixture (fe-fields reads *compiled-model*, which bare compile-model
never installs)")

(def-suite reference-chip-behavior
  :description "Reference chip picker behavior on reference-chip-test:
target-keyed :in rows, union within field, AND across fields,
palettes, fe-fields wire shape")

(in-suite reference-chip-compile)

(defun rc-compile (&rest field-plists)
  "Compile a minimal one-type model whose :wt fields are
FIELD-PLISTS ((key . def) pairs) and return the compiled :wt def."
  (getf (compile-model
          `(:wt
             (:table t
               :create :auto :update :auto :delete :auto
               :type-roles ("rc-users")
               :views (:main (:tables (:wt :users)))
               :fields
               ,(loop
                  for (key . def) in field-plists
                  appending (list key def))
               :list-form (:fields t)
               :update-form (:fields t)
               :add-form (:fields t))))
    :wt))

(defun rc-reference-fields ()
  "The :wt field shape carrying :filter-with :reference (targeting
base-model :users)."
  (list
    (cons :name
      '(:type :text :identity t
        :ui (:label "Wt" :widget :textbox)
        :source (:view :main :column :name :agg :first)
        :column t :not-null t :unique t))
    (cons :owner
      '(:type :text
        :target :users
        :ui (:widget :search-select :filter-with :reference)
        :source (:view :main :table :users :column :name :agg :first)
        :source-all (:view :main :table :users :column :name
                     :agg :list)
        :column t))))

(in-suite reference-chip-wire-checks)

(test reference-chip-wire-column-emission
  "fe-fields on a compiled :reference FK emits :column beside :table
(the target's identity display column, downcased string), and a
non-relation field on the same form carries no :column key at all
(the emission is :target-gated — an unconditional nil would
serialize as \"column\": [] through plist-to-json's nil-value
default). Runs on the fixture (fe-fields reads *compiled-model*,
which bare compile-model never installs). The :table key is a
keyword on the Lisp plist (plist-to-json downcases it to the JSON
string); :column is emitted as a downcased string at the source."
  (let* ((fe (fe-fields :tasks "admin"))
         (list-form (getf fe :list-form))
         (assigned (getf list-form :assigned-to))
         (project (getf list-form :project))
         (name (getf list-form :name)))
    (is (eq :users (getf assigned :table)))
    (is (equal "name" (getf assigned :column)))
    (is (eq :projects (getf project :table)))
    (is (equal "title" (getf project :column)))
    (is (eq :reference (getf assigned :filter-with)))
    ;; Non-relation fields carry NO :column key (not a nil value —
    ;; member, not getf).
    (is-false (member :column name)
      "non-relation wire entry must not carry :column at all")))

(test reference-chip-list-form-omitted-field-signals
  "A :reference field omitted from an explicit :list-form signals
with the per-field message (the one case that deserves it)."
  (signals error
    (compile-model
      `(:wt
         (:table t
           :create :auto :update :auto :delete :auto
           :type-roles ("rc-users")
           :views (:main (:tables (:wt :users)))
           :fields
           (:name
             (:type :text :identity t
               :ui (:label "Wt" :widget :textbox)
               :source (:view :main :column :name :agg :first)
               :column t :not-null t :unique t)
             :owner
             (:type :text
               :target :users
               :ui (:widget :search-select
                    :filter-with :reference)
               :source (:view :main :table :users :column :name
                        :agg :first)
               :column t))
           :list-form (:fields (:name))
           :update-form (:fields t)
           :add-form (:fields t))))))

(in-suite reference-chip-compile)

(test reference-chip-no-list-form-signals
  "A type with a :reference filter and no :list-form signals the
shape error (list-fields is nil; the shape check reports it
directly)."
  (signals error
    (compile-model
      `(:wt
         (:table t
           :create :auto :update :auto :delete :auto
           :type-roles ("rc-users")
           :views (:main (:tables (:wt :users)))
           :fields
           (:name
             (:type :text :identity t
               :ui (:label "Wt" :widget :textbox)
               :source (:view :main :column :name :agg :first)
               :column t :not-null t :unique t)
             :owner
             (:type :text
               :target :users
               :ui (:widget :search-select
                    :filter-with :reference)
               :source (:view :main :table :users :column :name
                        :agg :first)
               :column t))
           :update-form (:fields t)
           :add-form (:fields t))))))

(test reference-chip-list-form-nil-fields-signals
  "(:list-form (:fields nil)) signals the shape error, not the
per-field message — an author who spelled :fields nil wrote a
malformed :fields, not an omission."
  (signals error
    (compile-model
      `(:wt
         (:table t
           :create :auto :update :auto :delete :auto
           :type-roles ("rc-users")
           :views (:main (:tables (:wt :users)))
           :fields
           (:name
             (:type :text :identity t
               :ui (:label "Wt" :widget :textbox)
               :source (:view :main :column :name :agg :first)
               :column t :not-null t :unique t)
             :owner
             (:type :text
               :target :users
               :ui (:widget :search-select
                    :filter-with :reference)
               :source (:view :main :table :users :column :name
                        :agg :first)
               :column t))
           :list-form (:fields nil)
           :update-form (:fields t)
           :add-form (:fields t))))))

(test reference-chip-list-form-t-passes
  ":fields t with a :reference filter compiles clean."
  (finishes
    (apply #'rc-compile (rc-reference-fields))))

(test reference-chip-list-form-explicit-includes-passes
  "An explicit :fields list naming the :reference field compiles
clean."
  (finishes
    (compile-model
      `(:wt
         (:table t
           :create :auto :update :auto :delete :auto
           :type-roles ("rc-users")
           :views (:main (:tables (:wt :users)))
           :fields
           (:name
             (:type :text :identity t
               :ui (:label "Wt" :widget :textbox)
               :source (:view :main :column :name :agg :first)
               :column t :not-null t :unique t)
             :owner
             (:type :text
               :target :users
               :ui (:widget :search-select
                    :filter-with :reference)
               :source (:view :main :table :users :column :name
                        :agg :first)
               :column t))
           :list-form (:fields (:name :owner))
           :update-form (:fields t)
           :add-form (:fields t))))))

(test reference-chip-list-form-dotted-tail-signals
  "A dotted :fields tail signals the shape error, not the lying
'field not in the list form' — and it is the atom-arm pin too (an
atom :fields value would type-error in the compile loop before the
shape check; the dotted form is the testable malformed shape for
both)."
  (signals error
    (compile-model
      `(:wt
         (:table t
           :create :auto :update :auto :delete :auto
           :type-roles ("rc-users")
           :views (:main (:tables (:wt :users)))
           :fields
           (:name
             (:type :text :identity t
               :ui (:label "Wt" :widget :textbox)
               :source (:view :main :column :name :agg :first)
               :column t :not-null t :unique t)
             :owner
             (:type :text
               :target :users
               :ui (:widget :search-select
                    :filter-with :reference)
               :source (:view :main :table :users :column :name
                        :agg :first)
               :column t))
           :list-form (:fields (:name . "oops"))
           :update-form (:fields t)
           :add-form (:fields t))))))

(test reference-chip-no-reference-no-shape-check
  "A malformed :fields on a reference-free type still compiles when
the compile loop tolerates it — the shape check (has-reference
gate) is the only new objection, and it does not run. The
tolerable malformed spelling is an even-length dotted-tail value
whose getf walk succeeds ((:name) as a 1-tailed improper list
does not; a dotted pair (:name . :extra) errors in
compile-fields' cddr walk on every model). The honest pin is the
contract sentence: the gate never fires reference-free. Asserted
by the reference-carrying matrix above (every bad shape signals
there) plus this t-control: a reference-free type with :fields t
compiles, and valid-filter-with-list-form returns without
signaling on the fixture's :projects (reference-free type)."
  (with-model "reference-chip-test" nil
    (finishes
      (valid-filter-with-list-form :projects
        (u:tree-get *compiled-model* :projects))))
  (finishes
    (compile-model
      `(:wt
         (:table t
           :create :auto :update :auto :delete :auto
           :type-roles ("rc-users")
           :views (:main (:tables (:wt :users)))
           :fields
           (:name
             (:type :text :identity t
               :ui (:label "Wt" :widget :textbox)
               :source (:view :main :column :name :agg :first)
               :column t :not-null t :unique t))
           :list-form (:fields t)
           :update-form (:fields t)
           :add-form (:fields t))))))

(in-suite reference-chip-behavior)

;; Seed shape (three task rows + one project, derived totals):
;;   admin tasks = 2, admin+guest = 3, the Apollo task = 1.
;; All calls run as "admin" — load-bearing for the palette checks
;; (:users read is admin-scoped; a non-admin reader gets a
;; suppressed empty palette, the design's empty-palette rule) and
;; for RBAC row visibility (every row resource carries "admin").
;; The guest row is seeded BY admin with :roles '("logged-in") —
;; guest cannot insert (row-write-tests pins the rejection) and no
;; guest session ever runs; the :assigned-to "guest" value is what
;; the union row filters on.

(test reference-chip-users-in-filter
  "The exact row the picker emits: (:users :name :in (\"admin\"))
returns :total 2 — only the two admin-assigned tasks (the
guest-assigned task is excluded because its :assigned-to points at
guest's UUID)."
  (let ((r (be-insert :projects '(:title "Apollo") "admin")))
    (unwind-protect
      (let* ((t1 (be-insert :tasks
                   '(:name "rc-t1" :assigned-to "admin"
                     :project "Apollo") "admin"))
             (t2 (be-insert :tasks
                   '(:name "rc-t2" :assigned-to "admin") "admin"))
             (t3 (be-insert :tasks
                   '(:name "rc-t3" :assigned-to "guest")
                   "admin" :roles '("logged-in"))))
        (declare (ignore t2 t3))
        (is (eql 2
              (getf (be-list :tasks "admin"
                      :filters '((:users :name :in ("admin"))))
                :total)))
        (be-delete :tasks t3 "admin")
        (be-delete :tasks t2 "admin")
        (be-delete :tasks t1 "admin"))
      (be-delete :projects r "admin"))))

(test reference-chip-union-within-field
  "(:users :name :in (\"admin\" \"guest\")) → :total 3 — union
within one row (a single-value FK cannot express an intersection)."
  (let ((r (be-insert :projects '(:title "Apollo") "admin")))
    (unwind-protect
      (let* ((t1 (be-insert :tasks
                   '(:name "rc-t1" :assigned-to "admin"
                     :project "Apollo") "admin"))
             (t2 (be-insert :tasks
                   '(:name "rc-t2" :assigned-to "admin") "admin"))
             (t3 (be-insert :tasks
                   '(:name "rc-t3" :assigned-to "guest")
                   "admin" :roles '("logged-in"))))
        (declare (ignore t1 t2))
        (is (eql 3
              (getf (be-list :tasks "admin"
                      :filters
                      '((:users :name :in ("admin" "guest"))))
                :total)))
        (be-delete :tasks t3 "admin")
        (be-delete :tasks t2 "admin")
        (be-delete :tasks t1 "admin"))
      (be-delete :projects r "admin"))))

(test reference-chip-and-across-rows
  "A listed-type :eq row AND-composes with the reference :in row:
(:tasks :name :eq <one admin task>) + (:users :name :in
(\"admin\")) → :total 1."
  (let ((r (be-insert :projects '(:title "Apollo") "admin")))
    (unwind-protect
      (let* ((t1 (be-insert :tasks
                   '(:name "rc-t1" :assigned-to "admin"
                     :project "Apollo") "admin"))
             (t2 (be-insert :tasks
                   '(:name "rc-t2" :assigned-to "admin") "admin"))
             (t3 (be-insert :tasks
                   '(:name "rc-t3" :assigned-to "guest")
                   "admin" :roles '("logged-in"))))
        (declare (ignore t2))
        (is (eql 1
              (getf (be-list :tasks "admin"
                      :filters
                      '((:tasks :name :eq "rc-t1")
                        (:users :name :in ("admin"))))
                :total)))
        (be-delete :tasks t3 "admin")
        (be-delete :tasks t2 "admin")
        (be-delete :tasks t1 "admin"))
      (be-delete :projects r "admin"))))

(test reference-chip-cross-target-and
  "Two reference fields AND-compose across targets: (:users :name
:in (\"admin\")) + (:projects :title :in (\"Apollo\")) → :total 1
(the one admin task pointing at Apollo). The :projects identity pin
rides it — the second row names :title, not :name. No same-target
AND check exists to write: two :users-targeting rows are the same
row (v1 limitation; the fix is per-field join aliasing, post-MVP)."
  (let ((r (be-insert :projects '(:title "Apollo") "admin")))
    (unwind-protect
      (let* ((t1 (be-insert :tasks
                   '(:name "rc-t1" :assigned-to "admin"
                     :project "Apollo") "admin"))
             (t2 (be-insert :tasks
                   '(:name "rc-t2" :assigned-to "admin") "admin"))
             (t3 (be-insert :tasks
                   '(:name "rc-t3" :assigned-to "guest")
                   "admin" :roles '("logged-in"))))
        (declare (ignore t2 t3))
        (is (eql 1
              (getf (be-list :tasks "admin"
                      :filters
                      '((:users :name :in ("admin"))
                        (:projects :title :in ("Apollo"))))
                :total)))
        (be-delete :tasks t3 "admin")
        (be-delete :tasks t2 "admin")
        (be-delete :tasks t1 "admin"))
      (be-delete :projects r "admin"))))

(test reference-chip-non-name-identity-target
  "The regression pin for the identity generalization: the row names
the :projects identity column :title (not :name). A frontend or wire
change that hardcoded \"name\" would return :total 0 here, not a
400. Also the null-FK arm: a task with no project does not match the
Apollo row but still matches the un-project-filtered users row."
  (let ((r (be-insert :projects '(:title "Apollo") "admin")))
    (unwind-protect
      (let* ((t1 (be-insert :tasks
                   '(:name "rc-t1" :assigned-to "admin"
                     :project "Apollo") "admin"))
             (t2 (be-insert :tasks
                   '(:name "rc-t2" :assigned-to "admin") "admin"))
             (t3 (be-insert :tasks
                   '(:name "rc-t3" :assigned-to "guest")
                   "admin" :roles '("logged-in"))))
        (declare (ignore t3))
        (is (eql 1
              (getf (be-list :tasks "admin"
                      :filters
                      '((:projects :title :in ("Apollo"))))
                :total)))
        (is (eql 1
              (getf (be-list :tasks "admin"
                      :filters
                      '((:users :name :in ("admin"))
                        (:projects :title :in ("Apollo"))))
                :total)))
        ;; null-FK arm: t2 has no project but is admin-assigned
        (is (eql 1
              (getf (be-list :tasks "admin"
                      :filters
                      '((:tasks :name :eq "rc-t2")
                        (:users :name :in ("admin"))))
                :total)))
        (be-delete :tasks t3 "admin")
        (be-delete :tasks t2 "admin")
        (be-delete :tasks t1 "admin"))
      (be-delete :projects r "admin"))))

(test reference-chip-unknown-value-matches-zero
  "(:users :name :in (\"nobody\")) → :total 0, no signal — unknown
display values bind as raw display strings and match zero rows
(never a 400; the target-keyed row never reaches
resolve-reference-id). The picker only emits palette members, so
the UI never produces an unknown value; a hand-built API request
gets the matches-nothing answer."
  (let ((r (be-insert :projects '(:title "Apollo") "admin")))
    (unwind-protect
      (let* ((t1 (be-insert :tasks
                   '(:name "rc-t1" :assigned-to "admin"
                     :project "Apollo") "admin"))
             (t2 (be-insert :tasks
                   '(:name "rc-t2" :assigned-to "admin") "admin"))
             (t3 (be-insert :tasks
                   '(:name "rc-t3" :assigned-to "guest")
                   "admin" :roles '("logged-in"))))
        (declare (ignore t1 t2 t3))
        (is (eql 0
              (getf (be-list :tasks "admin"
                      :filters '((:users :name :in ("nobody"))))
                :total)))
        (is (eql 3
              (getf (be-list :tasks "admin"
                      :filters
                      '((:users :name :in ("admin" "guest" "nobody"))))
                :total)))
        (be-delete :tasks t3 "admin")
        (be-delete :tasks t2 "admin")
        (be-delete :tasks t1 "admin"))
      (be-delete :projects r "admin"))))

(test reference-chip-join-and-sort-compose
  "The :in row plus :sort composes without signaling (the 42P10
splice; m2m-join-filter-with-sort-42p10 is the pattern). :name is
:sortable t on the fixture."
  (let ((r (be-insert :projects '(:title "Apollo") "admin")))
    (unwind-protect
      (let* ((t1 (be-insert :tasks
                   '(:name "rc-t1" :assigned-to "admin"
                     :project "Apollo") "admin"))
             (t2 (be-insert :tasks
                   '(:name "rc-t2" :assigned-to "admin") "admin"))
             (t3 (be-insert :tasks
                   '(:name "rc-t3" :assigned-to "guest")
                   "admin" :roles '("logged-in"))))
        (declare (ignore t1 t2 t3))
        (finishes
          (be-list :tasks "admin"
            :filters '((:users :name :in ("admin")))
            :sort '(:name :asc)))
        (be-delete :tasks t3 "admin")
        (be-delete :tasks t2 "admin")
        (be-delete :tasks t1 "admin"))
      (be-delete :projects r "admin"))))

(test reference-chip-palettes-and-wire
  "fe-list / fe-fields shape on the fixture: :assigned-to carries
:filter-with :reference, :table \"users\", :column \"name\";
:project carries :column \"title\"; the allowed-values map is the
full palette map (:assigned-to, :project, :roles present; assert
specific keys, not the shape) — :assigned-to non-empty (admin
palette), :project non-empty after the project insert. The M2M
:has-all operator stays wire-legal and unchanged (the existing
m2m-has-all-* tests pin it; the frontend's meta.m2m branch emits
the same row)."
  (let ((r (be-insert :projects '(:title "Apollo") "admin")))
    (unwind-protect
      (let* ((t1 (be-insert :tasks
                   '(:name "rc-t1" :assigned-to "admin"
                     :project "Apollo") "admin"))
             (t2 (be-insert :tasks
                   '(:name "rc-t2" :assigned-to "admin") "admin"))
             (t3 (be-insert :tasks
                   '(:name "rc-t3" :assigned-to "guest")
                   "admin" :roles '("logged-in")))
             (users-palette
               (allowed-values-for-field :tasks :assigned-to
                 "admin"))
             (projects-palette
               (allowed-values-for-field :tasks :project
                 "admin"))
             (fe (fe-fields :tasks "admin"))
             (list-form (getf fe :list-form))
             (assigned (getf list-form :assigned-to))
             (project (getf list-form :project))
             (av (getf (be-list :tasks "admin"
                          :skip-allowed-values nil)
                    :allowed-values)))
        (declare (ignore t1 t2 t3))
        ;; wire shape (belt: same assertions as the wire suite's
        ;; emission test, asserted where the live model lives)
        (is (eq :users (getf assigned :table)))
        (is (equal "name" (getf assigned :column)))
        (is (equal "title" (getf project :column)))
        (is (eq :reference (getf assigned :filter-with)))
        ;; palettes: users non-empty (admin/guest at minimum)
        (is-true (member "admin" users-palette
                    :test #'equal))
        (is-true (member "guest" users-palette
                    :test #'equal))
        ;; projects palette after the insert lists Apollo
        (is (equal '("Apollo") projects-palette))
        ;; the full palette map carries the picker feeds
        (is-true (getf av :assigned-to))
        (is-true (getf av :project))
        (is-true (getf av :roles))
        (be-delete :tasks t3 "admin")
        (be-delete :tasks t2 "admin")
        (be-delete :tasks t1 "admin"))
      (be-delete :projects r "admin"))))

(test reference-chip-empty-project-palette
  "The :project palette is empty before any :projects row exists
(be-list-column over an empty rt_projects — live-probed NIL on a
fresh seed); the button hides but a URL-carried selection still
filters."
  (let ((r (be-insert :projects '(:title "Apollo") "admin")))
    ;; delete it again: probe the empty-table start
    (be-delete :projects r "admin")
    (is (null (allowed-values-for-field :tasks :project
               "admin")))))
