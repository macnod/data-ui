(in-package :data-ui)

(def-suite children-suite
    :description "Embedded child region (plan 02): the :children
vocabulary, validation, wire emission, and the blog fixture.")

(def-suite children-compile
    :in children-suite
    :description "Compile probes (no model, no DB).")

(in-suite children-compile)

(defparameter *blog-base-types*
  '(:posts
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :type-roles ("public")
       :views (:main (:tables (:posts)))
       :fields
       (:title
         (:type :text :identity t
           :ui (:label "Title" :widget :textbox)
           :validations (:required)
           :source (:view :main :column :title :agg :first)
           :column t :not-null t :unique t :sortable t)
         :body
         (:type :text
           :ui (:label "Body" :widget :markdown)
           :validations (:required)
           :source (:view :main :column :body :agg :first)
           :column t :not-null t)
         :created-at
         (:ui (:label "Posted" :widget :textbox :read-only t)))
       :list-form (:fields (:title :created-at))
       :view-form (:fields (:title :body)
                   :hide-list t
                   :children ((:type :comments
                               :via :post
                               :sort (:created-at :asc))))
       :update-form (:fields (:title :body))
       :add-form (:fields (:title :body)))
     :tags
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :type-roles ("public")
       :fields
       (:name
         (:type :text :identity t
           :ui (:label "Tag" :widget :textbox)
           :validations (:required)
           :source (:view :main :column :name :agg :first)
           :column t :not-null t :unique t))
       :list-form (:fields (:name))
       :view-form (:fields (:name))
       :update-form (:fields (:name))
       :add-form (:fields (:name)))
     :comments
     (:table t
       :create :auto :update :auto :delete :auto :display t
       :type-roles ("public" "comments-user")
       :views
       (:main (:tables (:comments :posts :users :post-tags :tags)))
       :fields
       (:user
         (:type :text :target :users
           :ui (:label "Author" :widget :textbox :read-only t
             :filter-with (:kind :reference))
           :source (:view :main :table :users :column :name :agg :first)
           :column t :not-null t
           :autofill :user)
         :body
         (:type :text
           :ui (:label "Body" :widget :markdown)
           :validations (:required)
           :searchable t
           :source (:view :main :column :body :agg :first)
           :column t :not-null t)
         :post
         (:type :text :target :posts
           :ui (:label "Post" :widget :search-select
             :filter-with (:kind :reference))
           :source (:view :main :table :posts :column :title :agg :first)
           :column t :not-null t)
         :created-at
         (:ui (:label "Posted" :widget :textbox :read-only t)))
       :list-form (:fields (:post :user :body :created-at)
                   :default-sort (:created-at :desc))
       :view-form (:fields (:user :body :post))
       :update-form (:fields (:user :body :post))
       :add-form (:fields (:user :body :post)))
     :post-tags
     (:table t :is-joiner t :internal t
       :fields
       (:reference (:target :comments)
         :reference (:target :tags)))))

(defun blog-types (&rest mutations)
  "Deep copy of *blog-base-types* (the posts / comments / tags /
post-tags skeleton every compile probe mutates) with MUTATIONS
applied. Each mutation is (fn type-key); the fn receives the raw
type plist and returns the type plist to install (return the
mutated argument — SETF GETF appends a fresh list when the key is
absent, so in-place-only mutation would silently lose new keys
like :category or :via)."
  (let ((types (u:deep-copy *blog-base-types*)))
    (dolist (m mutations)
      (setf (getf types (second m))
        (funcall (first m) (getf types (second m)))))
    types))

(defun td-setf (key value)
  "Return a mutation fn setting KEY to VALUE on a type def."
  (lambda (td)
    (setf (getf td key) value)
    td))

(defun td-remf (key)
  "Return a mutation fn removing KEY from a type def."
  (lambda (td)
    (remf td key)
    td))

(defmacro blog-compile (mutations-form)
  `(compile-model ,mutations-form))

(defun remf-type-key (type-def key)
  (remf type-def key))

(test children-walker-accepts-well-formed-spec
  "A well-formed spec under :view-form compiles rather than dying
as the walker's 'Invalid value' (the walker arm stops at the
region list)."
  (is (getf (getf (blog-compile (blog-types)) :posts)
        :create-table-sql)))

(test children-legal-on-view-form-only
  ":children on :add-form / :list-form / :update-form is the
per-form reject (its own message), not merely the closed set."
  (dolist (form '(:add-form :list-form :update-form))
    (let ((types (blog-types
                   (list (lambda (td)
                           (setf (getf td form)
                             (list :fields (getf (getf td form) :fields)
                               :children
                               '((:type :comments :via :post)))) td)
                     :posts))))
      (error-matches (blog-compile types)
        "not legal on .* of type .* legal on :view-form"
        (format nil "~a accepted :children" form)))))

(test children-missing-view-form-key
  "A type-level :children with no :view-form dies in valid-children
(the one catch for this case — valid-hide-list skips a nil form and
the walker never descended; the plan's 'one catch, not two')."
  (let ((types (blog-types
                 (list (lambda (td)
                         (remf (getf td :view-form) :children)
                         (setf (getf td :children)
                           '((:type :comments :via :post))) td)
                   :posts))))
    (error-matches (blog-compile types)
      "legal only inside :view-form"
      "missing :view-form not caught")))

(test children-nil-and-shape-violations
  ":children present as nil, a zero list, two specs, an atom, a
dotted list, and a non-plist spec are all compile errors."
  (dolist (bad '(nil () (((:type :comments :via :post))
                        ((:type :comments :via :post)))
                atom-sym))
    (declare (ignore bad))
    nil)
  ;; nil (present-nil skips the walker; key-presence rejects)
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf (getf td :view-form) :children)
                              nil) td)
                      :posts)))
    "must declare exactly one region spec; got 0"
    "present-nil :children not caught")
  ;; zero specs
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf (getf td :view-form) :children)
                              '()) td)
                      :posts)))
    "exactly one region spec"
    "zero-spec :children not caught")
  ;; two specs
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf (getf td :view-form) :children)
                              '((:type :comments :via :post)
                                (:type :comments :via :post))) td)
                      :posts)))
    "exactly one region spec"
    "two-spec :children not caught")
  ;; atom
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf (getf td :view-form) :children)
                              :oops) td)
                      :posts)))
    "must be a proper list"
    "atom :children not caught")
  ;; dotted list of one spec
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf (getf td :view-form) :children)
                              (cons '((:type :comments :via :post)) 'tail)) td)
                      :posts)))
    "must be a proper list"
    "dotted :children not caught")
  ;; non-plist spec
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf (getf td :view-form) :children)
                              '(:type)) td)
                      :posts)))
    "Region spec on type :POSTS must be a plist"
    "non-plist spec not caught"))

(test children-type-and-via-required
  "Missing :type and missing :via each signal their own message."
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf (getf td :view-form) :children)
                              '((:via :post))) td)
                      :posts)))
    "missing the required :type"
    "missing :type not caught")
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf (getf td :view-form) :children)
                              '((:type :comments))) td)
                      :posts)))
    "missing the required :via"
    "missing :via not caught"))

(test children-unknown-spec-keys
  "The v1 alphabet is exactly :type, :via, :sort."
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf (getf td :view-form) :children)
                              '((:type :comments :via :post
                                  :style :card))) td)
                      :posts)))
    "Unknown key :STYLE in :children region spec"
    "unknown spec key not caught"))

(test children-unknown-child-type
  ":type names a type in the model."
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf (getf td :view-form) :children)
                              '((:type :nope :via :post))) td)
                      :posts)))
    "names no type"
    "unknown child type not caught"))

(test children-via-validations
  ":via must name a singular :target field on the child aimed at
the parent: a non-target field, a join-table field, a third-type
target, and a base type all fail."
  ;; non-target field
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf (getf td :view-form) :children)
                              '((:type :comments :via :body))) td)
                      :posts)))
    "must name a :target \\(FK\\)"
    "non-target :via not caught")
  ;; join-table field (the M2M :tags on comments — added by the
  ;; mutation so the field exists but is not a :target)
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf (getf td :view-form) :children)
                              '((:type :comments :via :tags))) td)
                      :posts)
                    (list (lambda (td)
                            (setf (getf td :fields)
                              (append (getf td :fields)
                                '(:tags
                                   (:type :list
                                     :ui (:label "Tags"
                                       :widget :checkbox-list)
                                     :source (:view :main :table :tags
                                              :column :name :agg :distinct)
                                     :join-table :post-tags)))) td)
                      :comments)))
    "must name a :target \\(FK\\)"
    "join-table :via not caught")
  ;; third-type target
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf td :fields)
                              (append (getf td :fields)
                                '(:further
                                   (:type :text :target :tags
                                     :source (:view :main :table :tags
                                              :column :name :agg :first)
                                     :column t)))) td)
                      :comments)
                    (list (lambda (td)
                            (setf (getf (getf td :view-form) :children)
                              '((:type :comments :via :further))) td)
                      :posts)))
    "must target the declaring parent :POSTS"
    "third-type :via not caught"))

(test children-via-naming-base-type-child
  ":type naming a base type fails the surface contract."
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf (getf td :view-form) :children)
                              '((:type :users :via :user))) td)
                      :posts)))
    "must be a non-base type"
    "base-type child not caught"))

(test children-parent-child-surface-contract
  "Parent and child each need: non-nil :list-form, :display t,
non-internal, user-category. The child additionally needs
:view-form and :add-form. Each probe mutates one property at a
time."
  (labels ((drop-form (type-key form)
             (list (lambda (td) (remf td form) td) type-key))
           (nil-form (type-key form)
             (list (lambda (td) (setf (getf td form) nil) td) type-key)))
    ;; parent missing :list-form
    (error-matches
      (blog-compile (blog-types (drop-form :posts :list-form)))
      "requires a declared, non-nil :list-form"
      "parent missing list-form not caught")
    ;; parent explicit nil :list-form
    (error-matches
      (blog-compile (blog-types (nil-form :posts :list-form)))
      "requires a declared, non-nil :list-form"
      "parent nil list-form not caught")
    ;; child missing :list-form (comments declares reference
    ;; filters, so valid-filter-with-list-form rejects the missing
    ;; :fields first — either message proves the form never passes)
    (error-matches
      (blog-compile (blog-types (drop-form :comments :list-form)))
      "(:list-form :fields on type :COMMENTS|requires a declared, non-nil :list-form)"
      "child missing list-form not caught")
    ;; child explicit nil :list-form
    (error-matches
      (blog-compile (blog-types (nil-form :comments :list-form)))
      ":list-form :fields on type :COMMENTS"
      "child nil list-form not caught")
    ;; parent :display omitted
    (error-matches
      (blog-compile (blog-types
                      (list (lambda (td) (remf td :display) td) :posts)))
      ":POSTS must be visible"
      "parent display omitted not caught")
    ;; parent :display nil
    (error-matches
      (blog-compile (blog-types
                      (list (lambda (td) (setf (getf td :display) nil) td)
                        :posts)))
      ":POSTS must be visible"
      "parent display nil not caught")
    ;; child :display omitted
    (error-matches
      (blog-compile (blog-types
                      (list (lambda (td) (remf td :display) td)
                        :comments)))
      ":COMMENTS must be visible"
      "child display omitted not caught")
    ;; child :display nil
    (error-matches
      (blog-compile (blog-types
                      (list (lambda (td) (setf (getf td :display) nil) td)
                        :comments)))
      ":COMMENTS must be visible"
      "child display nil not caught")
    ;; parent non-user category
    (error-matches
      (blog-compile (blog-types
                      (list (lambda (td)
                              (setf (getf td :category) :system) td)
                        :posts)))
      "user-category"
      "parent :category :system not caught")
    ;; child non-user category
    (error-matches
      (blog-compile (blog-types
                      (list (lambda (td)
                              (setf (getf td :category) :settings) td)
                        :comments)))
      "user-category"
      "child :category :settings not caught")
    ;; parent internal
    (error-matches
      (blog-compile (blog-types
                      (list (lambda (td) (setf (getf td :internal) t) td)
                        :posts)))
      "must not be :internal"
      "internal parent not caught")
    ;; child internal
    (error-matches
      (blog-compile (blog-types
                      (list (lambda (td) (setf (getf td :internal) t) td)
                        :comments)))
      "must not be :internal"
      "internal child not caught")
    ;; child missing :view-form
    (error-matches
      (blog-compile (blog-types (drop-form :comments :view-form)))
      "requires a declared, non-nil :VIEW-FORM"
      "child missing view-form not caught")
    ;; child nil :view-form
    (error-matches
      (blog-compile (blog-types (nil-form :comments :view-form)))
      "requires a declared, non-nil :VIEW-FORM"
      "child nil view-form not caught")
    ;; child missing :add-form
    (error-matches
      (blog-compile (blog-types (drop-form :comments :add-form)))
      "requires a declared, non-nil :ADD-FORM"
      "child missing add-form not caught")
    ;; child nil :add-form
    (error-matches
      (blog-compile (blog-types (nil-form :comments :add-form)))
      "requires a declared, non-nil :ADD-FORM"
      "child nil add-form not caught")))

(test children-hide-list-companion
  ":children without :hide-list t — missing and explicitly nil —
is the same error (key membership, not truthiness)."
  ;; missing
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (remf (getf td :view-form) :hide-list) td)
                      :posts)))
    "requires :hide-list t"
    "missing :hide-list not caught")
  ;; explicit nil
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf (getf td :view-form) :hide-list)
                              nil) td)
                      :posts)))
    "requires :hide-list t"
    "explicit :hide-list nil not caught"))

(test children-tree-and-fs-backed-rejected
  "Tree / fs-backed parents reject :children with the tree guard's
own message (validate-tree / mark-path-field run in stage 1
before the guard, so the probe types must be otherwise-valid tree
shapes; the parent posts type is in the plist)."
  (let ((types (blog-types
                 ;; make comments a valid tree parent of posts:
                 ;; comments :tree t :parent-type :posts
                 (list (lambda (td)
                         (setf (getf td :tree) t
                           (getf td :parent-type) :posts
                           (getf (getf td :view-form) :children)
                           '((:type :comments :via :post))) td)
                   :posts))))
    ;; posts is not a tree parent of anything — validate-tree
    ;; demands parent-type exists as non-base type; posts works.
    ;; But posts' own children spec targets comments whose :post
    ;; field targets posts — surface check order runs before the
    ;; tree guard? No: tree guard is first in valid-children.
    (error-matches (blog-compile types)
      "not legal on tree / fs-backed type"
      "tree parent not caught")))
  ;; fs-backed arm: not probeable from the compile surface today —
  ;; mark-path-field runs on COMPILED fields and compile-field
  ;; drops :path (not in the copied attrs), so an fs-backed probe
  ;; dies in mark-path-field's "at least one field must be :path t"
  ;; before the :children guard. The tree arm above covers the
  ;; shared guard message; a true fs-backed probe waits on compiled
  ;; :path survival (a mark-path-field ordering question, not this
  ;; item's).

(defun blog-sort-children-types (sort)
  "blog-types with posts' :children :sort set to SORT (omitted when nil)."
  (blog-types
    (list (lambda (td)
            (setf (getf (getf td :view-form) :children)
              (list (append '(:type :comments :via :post)
                      (when sort (list :sort sort)))))
            td)
      :posts)))

(test children-sort-grammar
  ":sort shape: omitted legal, (:created-at) legal,
(:created-at :asc)/(:created-at :desc) legal; present nil, a
non-list, a wrong direction keyword, three elements, and a
present-nil direction all reject."
  ;; legal spellings
  (is (blog-compile (blog-sort-children-types nil)))
  (is (blog-compile (blog-sort-children-types '(:created-at))))
  (is (blog-compile (blog-sort-children-types '(:created-at :asc))))
  (is (blog-compile (blog-sort-children-types '(:created-at :desc))))
  ;; present-nil sort value (a malformed sort, not omission)
  (error-matches
    (blog-compile
      (blog-types
        (list (lambda (td)
                (setf (getf (getf td :view-form) :children)
                  '((:type :comments :via :post :sort nil))) td)
          :posts)))
    "must be .:field.*got NIL"
    "present-nil sort value not caught")
  ;; non-list sort (keyword) — the shape message, not a type error
  (error-matches (blog-compile (blog-sort-children-types :created-at))
    "must be .:field.*:CREATED-AT"
    "keyword sort not caught")
  ;; present-nil direction
  (error-matches (blog-compile (blog-sort-children-types '(:created-at nil)))
    "direction"
    "nil direction not caught")
  ;; wrong direction keyword
  (error-matches (blog-compile (blog-sort-children-types '(:created-at :up)))
    "direction"
    "wrong direction not caught")
  ;; three elements
  (error-matches
    (blog-compile (blog-sort-children-types '(:created-at :asc :desc)))
    "must be .:field"
    "three-element sort not caught")
  ;; unknown field
  (error-matches (blog-compile (blog-sort-children-types '(:nope :asc)))
    "names an unknown field"
    "unknown sort field not caught")
  ;; non-sortable field (:user is a :target — never sortable)
  (error-matches (blog-compile (blog-sort-children-types '(:user :asc)))
    "not :sortable t"
    "non-sortable sort field not caught"))

(test children-declaration-order-independent
  "Child declared after parent still validates (the reason the
cross-type pass is post-stage-1)."
  ;; reorder: comments/tags/post-tags first, posts last
  (let* ((types (blog-types))
         (reordered (append
                      (list :comments (getf types :comments)
                        :tags (getf types :tags)
                        :post-tags (getf types :post-tags))
                      (list :posts (getf types :posts)))))
    (is (getf (getf (blog-compile reordered) :posts)
          :create-table-sql))))

(test children-stray-type-level-via
  "A type-level :via surviving into the compiled def is rejected."
  (error-matches
    (blog-compile (blog-types
                    (list (lambda (td)
                            (setf (getf td :via) :post) td)
                      :posts)))
    "type-level :via key"
    "stray type-level :via not caught"))

(test children-default-sort-nil-direction-rejected
  "The shared rule change: type-level / list-form
:default-sort (:created-at nil) now rejects (previously the
(and direction …) arm accepted it), while (:created-at) stays
legal in both positions."
  ;; legal: one-element
  (is (blog-compile
        (blog-types
          (list (lambda (td)
                  (setf (getf (getf td :list-form) :default-sort)
                    '(:created-at)) td)
            :comments))))
  ;; legal: two-element
  (is (blog-compile
        (blog-types
          (list (lambda (td)
                  (setf (getf (getf td :list-form) :default-sort)
                    '(:created-at :desc)) td)
            :comments))))
  ;; illegal: present-nil direction
  (error-matches
    (blog-compile
      (blog-types
        (list (lambda (td)
                (setf (getf (getf td :list-form) :default-sort)
                  '(:created-at nil)) td)
          :comments)))
    "direction"
    "default-sort present-nil direction not caught"))

(test children-zero-identity-matrix
  "valid-target identity matrix: the positive zero-identity
internal-joiner :reference probe (base fixture), an ordinary
:target to zero-identity comments, a joiner :reference to
multi-identity, and a non-joiner :reference to zero identity."
  ;; positive: the base fixture's post-tags references compile (the
  ;; with-model compile in the seeded suite is the behavioral
  ;; proof; here assert compile-model of the base skeleton).
  (is (getf (getf (blog-compile (blog-types)) :post-tags)
        :create-table-sql))
  ;; ordinary :target to zero-identity comments
  (error-matches
    (blog-compile
      (blog-types
        (list (lambda (td)
                (setf (getf td :fields)
                  (append (getf td :fields)
                    '(:refs
                       (:type :text :target :comments
                         :source (:view :main :table :comments
                                  :column :body :agg :first)
                         :column t)))) td)
          :tags)))
    "exactly 1 identity"
    "ordinary :target to zero-identity not caught")
  ;; joiner :reference to multi-identity target (posts has one;
  ;; add a second identity to tags)
  (error-matches
    (blog-compile
      (blog-types
        (list (lambda (td)
                (setf (getf (getf (getf td :fields) :name) :identity) t
                  (getf td :fields)
                  (append (getf td :fields)
                    '(:alias
                       (:type :text :identity t
                         :source (:view :main :column :alias :agg :first)
                         :column t :not-null t :unique t)))) td)
          :tags)
        (list (lambda (td)
                (setf (getf td :fields)
                  (append (getf td :fields)
                    '(:reference (:target :tags)))) td)
          :post-tags)))
    "exactly 1 identity"
    "joiner :reference to multi-identity not caught")
  ;; non-joiner type's :reference-named field to zero identity
  (error-matches
    (blog-compile
      (blog-types
        (list (lambda (td)
                (setf (getf td :fields)
                  (append (getf td :fields)
                    '(:reference (:target :comments)))) td)
          :tags)))
    "exactly 1 identity"
    "non-joiner :reference to zero-identity not caught"))

(test children-join-table-zero-identity-other-side
  "A :list join-table field whose :source :table is zero-identity
comments is a compile error with the pinned message."
  (error-matches
    (blog-compile
      (blog-types
        (list (lambda (td)
                (setf (getf td :fields)
                  (append (getf td :fields)
                    '(:comments
                       (:type :list
                         :ui (:label "Comments" :widget :checkbox-list)
                         :source (:view :main :table :comments
                                  :column :body :agg :distinct)
                         :join-table :post-tags)))) td)
          :tags)))
    "Join-table list field :COMMENTS on :TAGS requires an identity field on other side :COMMENTS; got zero identity fields"
    "zero-identity M2M other side not caught"))

(test children-compiled-def-carries-spec-verbatim
  "The stage-3 def keeps the author spec verbatim, explicit
:sort included; an omitted :sort stays omitted."
  ;; explicit sort
  (let ((compiled (blog-compile (blog-types))))
    (is (equal '((:type :comments :via :post :sort (:created-at :asc)))
          (getf (getf (getf compiled :posts) :view-form) :children))))
  ;; omitted sort
  (let* ((types (blog-types
                  (list (lambda (td)
                          (setf (getf (getf td :view-form) :children)
                            '((:type :comments :via :post))) td)
                    :posts)))
         (compiled (blog-compile types)))
    (is (equal '((:type :comments :via :post))
          (getf (getf (getf compiled :posts) :view-form) :children)))))

(test region-spec-emitter-shapes
  "Pure emitter probes on the three legal sort spellings (no nil
direction — the validator owns that)."
  (flet ((emit (sort)
           (region-spec-list
             (list (append '(:type :comments :via :post)
                     (when sort (list :sort sort)))))))
    (is (equal '((:type "comments" :via "post"
                   :sort ("created-at" "asc")))
          (emit nil)))
    (is (equal '((:type "comments" :via "post"
                   :sort ("created-at" "asc")))
          (emit '(:created-at))))
    (is (equal '((:type "comments" :via "post"
                   :sort ("created-at" "desc")))
          (emit '(:created-at :desc))))))

(in-suite children-suite)

(defun seed-blog-fixture ()
  "Seed the blog-test fixture. Must run after set-model (which runs
ensure-declared-roles from :new-roles) — th-make-user would
otherwise create comments-user with default permissions and the
:create :true assertion would be about the wrong role. Returns a
plist of ids and the member name."
  (let ((member-name "blog-member"))
    (th-make-user member-name :roles '("comments-user"))
    (let* ((post-1 (be-insert :posts
                      '(:title "First post"
                        :body "Hello world")
                      "admin" :roles '("public")))
           (post-2 (be-insert :posts
                      '(:title "Quiet post"
                        :body "No comments here")
                      "admin" :roles '("public")))
           (c1 (be-insert :comments
                '(:user "admin" :body "First!" :post "First post")
                "admin" :roles '("public")))
           (c2 (be-insert :comments
                `(:user ,member-name :body "Nice post!"
                  :post "First post")
                member-name :roles '("public")))
           (c3 (be-insert :comments
                '(:user "admin" :body "Me too"
                  :post "First post")
                "admin" :roles '("public")))
           (private (be-insert :comments
                       '(:user "admin" :body "secret note"
                         :post "First post")
                       "admin"))
           (tag (be-insert :tags '(:name "news") "admin"
                  :roles '("public"))))
      (be-update :comments `((:comments :id :eq ,c2))
        '(:tags ("news")) member-name)
      (list :post-1 post-1 :post-2 post-2
        :comment-1 c1 :comment-2 c2 :comment-3 c3
        :private-comment private :tag tag
        :member member-name))))

(test children-compiled-def-in-live-model
  "The seeded fixture's compiled :children spec, verbatim on the
stage-3 def, including its explicit :sort."
  (is (equal '((:type :comments :via :post :sort (:created-at :asc)))
        (getf (u:tree-get *compiled-model* :posts :view-form)
          :children))))

(test children-be-rec-view-form-emits
  ":form :view-form on a posts row emits :children with the
normalized pair."
  (let* ((result (be-rec (getf *fixture* :post-1) "admin"
                  :form :view-form :type-key :posts))
         (children (getf result :children)))
    (is (equal '((:type "comments" :via "post"
                   :sort ("created-at" "asc")))
          children))))

(test children-be-rec-update-form-omits
  ":form :update-form on the same row emits no :children key at
all (member on u:plist-keys, not getf)."
  (let* ((result (be-rec (getf *fixture* :post-1) "admin"
                  :form :update-form :type-key :posts)))
    (is-false (member :children (u:plist-keys result)))))

(test children-be-rec-no-children-type-omits
  "A :view-form fetch on :tags (no :children) emits no key."
  (let* ((result (be-rec (getf *fixture* :tag) "admin"
                  :form :view-form :type-key :tags)))
    (is-false (member :children (u:plist-keys result)))))

(test children-be-rec-inaccessible-returns-nil
  "An inaccessible record returns nil — no envelope at all. Pass
:type-key (the public caller's spelling) so the probe stays off
the silent-no-op path."
  (is (null (be-rec (getf *fixture* :private-comment) "guest"
              :form :view-form :type-key :comments))))

(test children-guest-reads
  "Guest (seeded with public) reads public rows; the private
comment is invisible; guest's :create on comments is :false."
  (let* ((comments (be-list :comments "guest"))
         (records (getf comments :records)))
    (is (= 3 (length records)))
    (is (not (find (getf *fixture* :private-comment) records
               :key (lambda (r) (getf r :id)) :test #'equal)))
    (is (eq :false (getf comments :create)))))

(test children-member-create-true
  "blog-member (comments-user: create+read) sees :create :true —
and the role existed with exactly those permissions before
th-make-user ran (set-model declared it)."
  (let ((comments (be-list :comments (getf *fixture* :member))))
    (is (eq :true (getf comments :create)))))

(test children-insert-without-roles-invisible-to-guest
  "An inserted-without-:roles comment is invisible to guest's
next be-list."
  (let ((id (be-insert :comments
               '(:user "admin" :body "hidden reply"
                 :post "First post")
               "admin")))
    (is-true id)
    (let ((records (getf (be-list :comments "guest") :records)))
      (is (not (find id records
                 :key (lambda (r) (getf r :id)) :test #'equal))))))

(test children-m2m-smoke
  "A comment updated with a seeded public tag returns that tag in
its row display, and the guest-readable tags palette contains
it. Passes solely because the exercised comments→tags direction's
other side (:tags) has the single :name identity the unchanged
write path uses."
  (let* ((c2 (getf *fixture* :comment-2))
         (row (find c2 (getf (be-list :comments "admin"
                                :limit 100) :records)
                   :key (lambda (r) (getf r :id)) :test #'equal)))
    (is (equal '("news") (getf row :tags))))
  (let* ((list (be-list :comments "guest"))
         (tags (getf (getf list :allowed-values) :tags)))
    (is (find "news" tags :test #'equal))))

(test children-json-round-trip
  "The emitted envelope's :children serializes exactly as
[{\"type\":\"comments\",\"via\":\"post\",\"sort\":[\"created-at\",\"asc\"]}]."
  (let* ((result (be-rec (getf *fixture* :post-1) "admin"
                  :form :view-form :type-key :posts))
         (json (plist-to-json `(:children ,(getf result :children)))))
    (is (equal
          "{\"children\":[{\"type\":\"comments\",\"via\":\"post\",\"sort\":[\"created-at\",\"asc\"]}]}"
          json))))
