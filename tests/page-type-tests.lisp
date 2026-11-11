(in-package :data-ui)

(def-suite page-type-compile
  :description ":page t compile-time probes (pure compile-model, no DB)")

(def-suite page-type-behavior
  :description ":page t backend / REST behavior on page-type-test")

;;; ---------------------------------------------------------------------------
;;; Compile-suite helpers
;;; ---------------------------------------------------------------------------

(defun pt-pages-type ()
  "The canonical :pages content type (the plan's worked model, with
the house :column t / :source idiom every table-type fixture
declares — a field without :column t is genuinely virtual, and the
page storage check is right to reject it)."
  '(:table t :create :auto :update :auto :delete :auto
    :display t
    :type-roles ("editor")
    :fields
    (:slug
      (:type :text :identity t :not-null t
        :ui (:label "Slug" :widget :textbox)
        :validations (:required)
        :source (:view :main :column :slug :agg :first)
        :column t)
     :title
      (:type :text :not-null t
        :ui (:label "Title" :widget :textbox)
        :validations (:required)
        :source (:view :main :column :title :agg :first)
        :column t)
     :body
      (:type :text
        :ui (:label "Body" :widget :textarea)
        :source (:view :main :column :body :agg :first)
        :column t)
     :published
      (:type :boolean
        :ui (:label "Published" :widget :checkbox)
        :source (:view :main :column :published :agg :first)
        :column t))
    :list-form (:fields t)
    :add-form (:fields t)
    :update-form (:fields t)
    :view-form (:fields (:title :body))))

(defun pt-page-def (&key (of :pages)
                      (filter '((:pages :slug :eq "about")))
                      (view-form '(:fields (:title :body)))
                      (fields '(:title (:ui (:label "Title"))
                                :body (:ui (:label "Body"))))
                      (type-roles '("public"))
                      extra)
  "A page def with the named parts replaced (no duplicate keys —
add-to-plist throws on them, so spliced-in overrides would die with
the wrong error). EXTRA is a tail of additional keys, spliced once
(the matrix-rejection probes: :list-form nil, :page-description,
:type-level :children, ...)."
  (append
    (list :page t
      :of of
      :filter filter
      :type-roles type-roles
      :view-form view-form
      :fields fields)
    extra))

(defun pt-model (&rest types)
  "Types plist for compile probes: the canonical :pages content type
plus TYPES spliced as key/def pairs."
  (apply #'list :pages (pt-pages-type) types))

(defun pt-pages-with-heading-title ()
  "The canonical :pages content type with :widget :heading on its
:title — an ordinary (non-page) type declaring the page-only
widget (the compile-field rejection probe: valid-widget-value is a
pure membership check, so the field passes it and the rejection
must fire in compile-field). Binds the field def first — setf of
getf expands its first argument as a place, and u:tree-get has no
setf expander; the bound variable is the legal place, and the
shared sublist is what the mutation reaches."
  (let* ((pages (copy-tree (pt-pages-type)))
         (title (u:tree-get pages :fields :title)))
    (setf (getf title :ui) '(:label "Title" :widget :heading))
    pages))

(defun pt-tcomp-type ()
  "Composite-identity content type: :slug (scalar) + :owner (a
:target identity field — the Model Bank :ratings shape)."
  '(:table t :create :auto :update :auto :delete :auto
    :display t
    :type-roles ("editor")
    :fields
    (:slug
      (:type :text :identity t :not-null t
        :ui (:label "Slug" :widget :textbox)
        :source (:view :main :column :slug :agg :first)
        :column t)
     :owner
      (:type :text :identity t :not-null t
        :ui (:label "Owner" :widget :textbox)
        :target :users
        :source (:view :main :table :users :column :name :agg :first)
        :column t))
    :list-form (:fields t)
    :add-form (:fields t)
    :update-form (:fields t)
    :view-form (:fields (:slug))))

(defun pt-signal-message (form)
  "Run FORM (a thunk), return the signaled error's message string, or
nil when it completes. For the probes that pin the message names a
field."
  (handler-case
      (progn (funcall form) nil)
    (error (e) (format nil "~a" e))))

;;; ---------------------------------------------------------------------------
;;; Compile errors
;;; ---------------------------------------------------------------------------

(in-suite page-type-compile)

(test pt-page-nil-signals
  ":page nil is not a declaration — t is the sole spelling (the
:rollup t precedent)."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def extra '(:page nil))))))

(test pt-page-plus-rollup-signals
  ":page t with :rollup t is a compile error."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def extra '(:rollup t))))))

(test pt-missing-of-signals
  "A page with no :of names no content type."
  (signals error
    (compile-model
      (list :pages (pt-pages-type)
        :about (list :page t
                 :filter '((:pages :slug :eq "about"))
                 :type-roles '("public")
                 :view-form '(:fields (:title :body))
                 :fields '(:title (:ui (:label "Title"))))))))

(test pt-of-rollup-signals
  ":of must be an ordinary author table — a rollup is not one. The
board itself compiles (grain :users, fact :tcomp), so the signal
comes from the page matrix."
  (signals error
    (compile-model
      (list
        :tcomp (pt-tcomp-type)
        :board '(:rollup t
                 :grain :users
                 :type-roles ("r")
                 :views (:main (:tables (:users :tcomp)))
                 :list-form (:fields t)
                 :fields
                 ((:name (:source (:view :main :table :users
                                     :column :name :agg :first)
                            :ui (:label "User")))
                  (:n (:type :integer
                       :source (:view :main :table :tcomp
                                 :column :id :agg :count)
                       :ui (:label "N")))))
        :pages (pt-pages-type)
        :about (pt-page-def :of :board)))))

(test pt-of-page-signals
  ":of a page is a compile error (a page has no content rows)."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def)
                     :about2 (pt-page-def :of :about)))))

(test pt-of-base-type-signals
  ":of :users is a compile error — base types are out of v1."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def :of :users)))))

(test pt-of-missing-type-signals
  ":of naming no type in the model is a compile error."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def :of :nope)))))

(test pt-of-self-signals
  ":of naming the page itself is a compile error."
  (signals error
    (compile-model (pt-model
                     :self (pt-page-def :of :self)))))

(test pt-of-on-ordinary-type-signals
  ":of on an ordinary (non-page) type is a compile error — the
valid-non-rollup-keys :of arm (without it the key would ride the
base overlay untouched, silently accepted junk)."
  (signals error
    (compile-model (pt-model :notes '(:table t :of :pages)))))

(test pt-of-on-rollup-signals
  ":of on a rollup is a compile error (page-only vocabulary)."
  (signals error
    (compile-model
      (list
        :tcomp (pt-tcomp-type)
        :board '(:rollup t
                 :grain :users
                 :type-roles ("r")
                 :views (:main (:tables (:users :tcomp)))
                 :list-form (:fields t)
                 :fields
                 ((:name (:source (:view :main :table :users
                                     :column :name :agg :first)
                            :ui (:label "User")))
                  (:n (:type :integer
                       :source (:view :main :table :tcomp
                                 :column :id :agg :count)
                       :ui (:label "N")))))
        :pages (pt-pages-type)
        :bad-board (append '(:rollup t :grain :users
                              :type-roles ("r")
                              :views (:main (:tables (:users :tcomp)))
                              :list-form (:fields t)
                              :of :pages)
                       '(:fields
                          ((:name (:source (:view :main :table :users
                                              :column :name :agg :first)
                                     :ui (:label "User")))
                           (:n (:type :integer
                                :source (:view :main :table :tcomp
                                          :column :id :agg :count)
                                :ui (:label "N"))))))))))

(test pt-missing-filter-signals
  "A page with no :filter has no row selection."
  (signals error
    (compile-model
      (list :pages (pt-pages-type)
        :about (list :page t
                 :of :pages
                 :type-roles '("public")
                 :view-form '(:fields (:title :body))
                 :fields '(:title (:ui (:label "Title"))))))))

(test pt-empty-filter-signals
  ":filter () is a compile error (nil / () are not declarations)."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def :filter '())))))

(test pt-short-clause-signals
  "A 3-element clause is not a 4-tuple."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :filter '((:pages :slug :eq)))))))

(test pt-clause-wrong-table-signals
  "A clause whose table is not the content type is a compile error."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :filter '((:other :slug :eq "x")))))))

(test pt-no-identity-eq-signals
  "A filter with no identity :eq never selects one row (here the
only :eq is the boolean ride-along)."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :filter '((:pages :published :eq t)))))))

(test pt-eq-non-identity-only-signals
  ":eq on a non-identity column never counts toward the pin — a
filter whose every :eq is a ride-along dies as an unpinned identity
set."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :filter '((:pages :title :eq "x")))))))

(test pt-multi-identity-partial-pin-signals
  "A multi-identity content type pinned on only one of its identity
columns is unsound — :identity t columns form one composite unique
index, so one pinned column of two guarantees nothing."
  (signals error
    (compile-model
      (list
        :multi '(:table t :create :auto :update :auto :delete :auto
                  :display t :type-roles ("editor")
                  :fields
                  (:slug (:type :text :identity t :not-null t
                           :ui (:label "Slug")
                           :source (:view :main :column :slug
                                     :agg :first)
                           :column t)
                    :lang (:type :text :identity t :not-null t
                            :ui (:label "Lang")
                            :source (:view :main :column :lang
                                      :agg :first)
                            :column t)
                    :title (:type :text
                             :ui (:label "Title")
                             :source (:view :main :column :title
                                       :agg :first)
                             :column t))
                  :list-form (:fields t) :add-form (:fields t)
                  :update-form (:fields t)
                  :view-form (:fields (:title)))
        :about (pt-page-def :of :multi)))))

(test pt-target-identity-unpinnable-names-field
  "A composite identity containing a :target identity field cannot
be pinned by a scalar literal at all — the unpinned arm names the
field."
  (let ((msg (pt-signal-message
               (lambda ()
                 (compile-model
                   (list :tcomp (pt-tcomp-type)
                     :about (pt-page-def :of :tcomp
                              :filter '((:tcomp :slug :eq "x")))))))))
    (is (and msg (search "OWNER" msg))
      "the error must name the unpinnable identity field: ~a" msg)))

(test pt-target-field-filter-names-field
  "A :target field in the filter — declared :type :text with
:target set, the shape that passes the shared validator — dies in
the page storage check, naming the field."
  (let ((msg (pt-signal-message
               (lambda ()
                 (compile-model
                   (list :tcomp (pt-tcomp-type)
                     :about (pt-page-def :of :tcomp
                              :filter '((:tcomp :owner :eq "admin")))))))))
    (is (and msg (search "OWNER" msg))
      "the error must name the field: ~a" msg)))

(test pt-virtual-field-filter-signals
  "A :column nil (virtual) field in the filter is a compile error —
declared type is not storage, and the shared validator cannot see
the difference (it checks :type only)."
  (signals error
    (compile-model
      (list
        :pages '(:table t :create :auto :update :auto :delete :auto
                  :display t :type-roles ("editor")
                  :fields
                  (:slug (:type :text :identity t :not-null t
                           :ui (:label "Slug")
                           :source (:view :main :column :slug
                                     :agg :first)
                           :column t)
                    :title (:type :text :not-null t
                             :ui (:label "Title")
                             :source (:view :main :column :title
                                       :agg :first)
                             :column t)
                    ;; virtual: no :column t, no :target — a
                    ;; display-only field with no storage
                    :vfield (:type :text :ui (:label "V")))
                  :list-form (:fields t) :add-form (:fields t)
                  :update-form (:fields t)
                  :view-form (:fields (:title)))
        :about (pt-page-def
                 :filter '((:pages :vfield :eq "x")))))))

(test pt-join-table-field-filter-signals
  "A :join-table field in the filter is a compile error — its
storage lives on another table."
  (signals error
    (compile-model
      (list
        :pages '(:table t :create :auto :update :auto :delete :auto
                  :display t :type-roles ("editor")
                  :fields
                  (:slug (:type :text :identity t :not-null t
                           :ui (:label "Slug")
                           :source (:view :main :column :slug
                                     :agg :first)
                           :column t)
                    :title (:type :text :not-null t
                             :ui (:label "Title")
                             :source (:view :main :column :title
                                       :agg :first)
                             :column t)
                    :tags (:type :list :join-table :tags
                            :ui (:label "Tags")))
                  :list-form (:fields t) :add-form (:fields t)
                  :update-form (:fields t)
                  :view-form (:fields (:title)))
        :tags '(:table t :display t :type-roles ("editor")
                 :fields (:name (:type :text :identity t
                                  :ui (:label "Name")
                                  :source (:view :main :column :name
                                            :agg :first)
                                  :column t))
                 :list-form (:fields t) :add-form (:fields t)
                 :update-form (:fields t) :view-form (:fields t))
        :about (pt-page-def
                 :filter '((:pages :tags :eq "x")))))))

(test pt-page-description-signals
  ":page-description on a :page t type is a compile error — the
page's content IS the description."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :extra '(:page-description "Hi."))))))

(test pt-page-description-open-signals
  ":page-description-open on a :page t type is a compile error."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :extra '(:page-description-open t))))))

(test pt-list-form-even-nil-signals
  ":list-form on a page is a compile error even as nil — a page
declares no list."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def :extra '(:list-form nil))))))

(test pt-add-form-even-nil-signals
  ":add-form on a page is a compile error even as nil."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def :extra '(:add-form nil))))))

(test pt-update-form-even-nil-signals
  ":update-form on a page is a compile error even as nil."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def :extra '(:update-form nil))))))

(test pt-missing-view-form-signals
  "A page without :view-form is a compile error (the
rollup-requires-:list-form precedent): the reader may not hold read
on the content type."
  (signals error
    (compile-model
      (list :pages (pt-pages-type)
        :about (list :page t :of :pages
                 :filter '((:pages :slug :eq "about"))
                 :type-roles '("public")
                 :fields '(:title (:ui (:label "Title"))))))))

(test pt-view-form-nil-signals
  ":view-form nil is the same missing-surface mistake."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def :view-form nil)))))

(test pt-hide-list-on-view-form-even-nil-signals
  ":hide-list on a page's :view-form is a compile error even as
nil — the key suppresses a list, and a page has no list."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :view-form '(:fields (:title :body)
                                           :hide-list nil))))))

(test pt-children-on-view-form-signals
  ":children on a page's :view-form is a compile error, with a
page-specific message (so valid-children never has to explain it as
a missing :list-form)."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :view-form '(:fields (:title :body)
                                           :children
                                           ((:type :notes
                                              :via :note))))))))

(test pt-type-level-children-signals
  "A type-level :children on a page is a compile error by key
membership — valid-children's message would not name pages."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :extra '(:children
                                        ((:type :notes
                                           :via :note))))))))

(test pt-type-level-via-signals
  "A type-level :via on a page is a compile error by key membership."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def :extra '(:via :note))))))

(test pt-display-nil-signals
  ":display nil on a page is a compile error — a page is visible or
it is not a page."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def :extra '(:display nil))))))

(test pt-user-setting-signals
  ":user-setting t on a page is a compile error — settings
auto-open and are the wrong surface."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def :extra '(:user-setting t))))))

(test pt-author-table-name-signals
  "An author :table-name on a page is a compile error — a lie about
a table that does not exist."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :extra '(:table-name "rt_about"))))))

(test pt-author-page-params-signals
  "An author :page-params on a page is a compile error — the
add-to-plist (or new old) merge cannot overwrite it on a zero-bind
page (computed :page-params is ()), and an author value would
corrupt the runtime bind list."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :extra '(:page-params ("evil")))))))

(test pt-author-id-field-signals
  "An author :id on a page is a compile error — the compiler
injects it as a pass-through of the content row's id."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :fields '(:id (:ui (:label "Id"))))))))

(test pt-unknown-field-key-signals
  "A page field naming a column the content type does not have is a
compile error."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :fields '(:nope (:ui (:label "N"))))))))

(test pt-target-field-projection-signals
  "Projecting a :target field is a compile error (a page field
cannot declare one; its storage is a UUID FK)."
  (signals error
    (compile-model
      (list :tcomp (pt-tcomp-type)
        :about (pt-page-def :of :tcomp
                            :fields '(:owner (:ui (:label "Owner")))
                            :view-form '(:fields (:owner)))))))

(test pt-file-field-projection-signals
  "Projecting a :file field is a compile error — :file is not a
projectable scalar."
  (let ((msg (pt-signal-message
               (lambda ()
                 (compile-model
                   (list
                     :pages '(:table t :create :auto :update :auto
                               :delete :auto :display t
                               :type-roles ("editor")
                               :fields
                               (:slug (:type :text :identity t
                                        :not-null t
                                        :ui (:label "Slug")
                                        :source (:view :main
                                                  :column :slug
                                                  :agg :first)
                                        :column t)
                                 :title (:type :text :not-null t
                                          :ui (:label "Title")
                                          :source (:view :main
                                                    :column :title
                                                    :agg :first)
                                          :column t)
                                 :attachment (:type :file
                                               :ui (:label "Attachment")
                                               :source (:view :main
                                                         :column
                                                         :attachment
                                                         :agg :first)
                                               :column t))
                               :list-form (:fields t)
                               :add-form (:fields t)
                               :update-form (:fields t)
                               :view-form (:fields (:title)))
                     :about (pt-page-def
                              :fields '(:attachment
                                         (:ui (:label "File")))
                              :view-form '(:fields (:attachment)))))))))
    (is (and msg (search "ATTACHMENT" msg))
      "the error must name the field: ~a" msg)))

(test pt-markdown-non-text-signals
  ":widget :markdown on a non-text content column is a compile
error."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :fields '(:published
                                         (:ui (:widget :markdown)))
                              :view-form '(:fields (:published)))))))

(test pt-image-widget-signals
  ":widget :image is not in the page allowlist (not a projectable
scalar)."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :fields '(:body (:ui (:widget :image)))
                              :view-form '(:fields (:body)))))))

(test pt-search-select-widget-signals
  ":widget :search-select is not in the page allowlist — it
requires :target, which a page field cannot declare (the negative
list admitted it; the positive list does not)."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :fields '(:body
                                         (:ui (:widget :search-select)))
                              :view-form '(:fields (:body)))))))

(test pt-select-without-options-signals
  ":widget :select without :options is a compile error — finalize-ui
checks shape only, the options requirement lives here (compile-field
never runs on a page field)."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :fields '(:title
                                         (:ui (:widget :select)))
                              :view-form '(:fields (:title)))))))

(test pt-heading-two-fields-signals
  "A page declaring two :widget :heading fields is a compile error,
not first-wins — a page is title and body, the title is the one
heading, and the markdown body makes all the headings it wants.
The message names the second field."
  (let ((msg (pt-signal-message
               (lambda ()
                 (compile-model
                   (pt-model
                     :about (pt-page-def
                              :fields '(:title (:ui (:widget :heading))
                                        :body (:ui (:widget :heading))
                                        :slug (:ui (:label "Slug")))
                              :view-form '(:fields (:title :body)))))))))
    (is (and msg (search "BODY" msg))
      "the error must name the second field: ~a" msg)))

(test pt-heading-on-ordinary-type-signals
  ":widget :heading on an ordinary (non-page) field signals in
compile-field — the page-only rejection lives there, beside the
:search-select requires-:target check, not in valid-widget-value
(a pure membership check: a page field must pass it to reach the
page allowlist, and compile-field never runs on a page field)."
  (signals error
    (compile-model
      (list :pages (pt-pages-with-heading-title)))))

(test pt-heading-non-text-signals
  ":widget :heading on a non-text content column is a compile error
— the :markdown / :textarea / :code arm's rule, without which
valid-page-field-ui's case entry can be deleted silently."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :fields '(:published
                                         (:ui (:widget :heading)))
                              :view-form '(:fields (:published)))))))

(test pt-non-ui-key-signals
  "A key other than :ui on a page field is a compile error — a page
field is a presentation overlay."
  (signals error
    (compile-model (pt-model
                     :about (pt-page-def
                              :fields '(:title (:ui (:label "T")
                                                 :sortable t)))))))

(test pt-filter-on-plain-type-signals
  "Regression: :filter stays illegal on a plain (non-page,
non-rollup) type — the valid-non-rollup-keys arm did not regress."
  (signals error
    (compile-model (pt-model :notes '(:table t
                                       :filter ((:notes :name :eq "x")))))))

;;; ---------------------------------------------------------------------------
;;; Compile successes
;;; ---------------------------------------------------------------------------

(defun pt-compiled-worked ()
  "The compiled worked model (canonical :pages + :about with the
markdown body and the boolean ride-along)."
  (compile-model
    (pt-model
      :about (pt-page-def
               :fields '(:title (:ui (:label "Title"))
                         :body (:ui (:widget :markdown :hide-label t))
                         :slug (:ui (:label "Slug")))))))

(test pt-compile-success-shape
  "The worked page compiles with the full overlay: :phase-a-shape
:page, no :table-name, read-only caps nil, :suppress-roles t,
:display t."
  (let ((about (getf (pt-compiled-worked) :about)))
    (is (eq :page (getf about :phase-a-shape)))
    (is-false (getf about :table-name))
    (is-false (getf about :create))
    (is-false (getf about :update))
    (is-false (getf about :delete))
    (is (eq t (getf about :suppress-roles)))
    (is (eq t (getf about :display)))))

(test pt-compile-success-no-phantom-sql
  "No phantom-table SQL survives compilation: no :views, no
:create-table-sql, no :insert-sql / :update-sql / :delete-sql /
:search-sql — the pins that catch a partial stage-2 widening."
  (let ((about (getf (pt-compiled-worked) :about)))
    (is-false (getf about :views))
    (is-false (getf about :create-table-sql))
    (is-false (getf about :insert-sql))
    (is-false (getf about :update-sql))
    (is-false (getf about :delete-sql))
    (is-false (getf about :search-sql))))

(test pt-compile-success-fields
  "Compiled fields: the injected :id is first, carries no :ui (so
fe-fields never emits it), and every field carries :default :null;
the body's finalized :ui routes :markdown."
  (let* ((about (getf (pt-compiled-worked) :about))
          (fields (getf about :fields)))
    (is (eq :id (first fields)))
    (is (eq :uuid (getf (getf fields :id) :type)))
    (is-false (getf (getf fields :id) :ui))
    (is (eq :markdown
          (u:tree-get fields :body :ui :widget)))
    (loop for def in (cdr fields) by #'cddr
      do (is (eq :null (getf def :default))))
    (is (equal "page_body"
          (u:tree-get fields :body :name-sql)))))

(test pt-heading-compiles
  "A page field with :widget :heading compiles through both
allowlists — *widgets* carries it past valid-widget-value (the
page-only rejection lives in compile-field, which never runs on a
page field) and *page-widgets* admits it — and the finalized :ui
carries the widget and :hide-label t verbatim."
  (let* ((m (compile-model
              (pt-model
                :about (pt-page-def
                         :fields '(:title
                                    (:ui (:widget :heading
                                           :hide-label t))
                                   :body (:ui (:widget :markdown)))))))
         (ui (u:tree-get m :about :fields :title :ui)))
    (is (eq :heading (getf ui :widget)))
    (is (eq t (getf ui :hide-label)))))

(test pt-compile-success-where-bind
  ":page-where for the text-slug clause is a bind placeholder, not
a raw-quoted literal (a slug with an apostrophe must not produce
broken SQL), and :page-params is exactly the one bind."
  (let* ((about (getf (pt-compiled-worked) :about))
          (fragments (getf about :page-where)))
    (let ((slug-frag (find-if
                       (lambda (f) (search "page_slug" f))
                       fragments)))
      (is (and slug-frag (search "$1" slug-frag))
        "the slug fragment must bind $1: ~a" slug-frag)
      (is-false (and slug-frag (search "about" slug-frag))
        "the slug literal must not be embedded"))
    (is (equal '("about") (getf about :page-params)))))

(test pt-compile-success-boolean-mappings
  "The four boolean mappings beside the identity clause — :eq t →
bare column, :eq nil → not column, :ne t → not column, :ne nil →
bare column — with :page-params still exactly the one slug bind in
all four (no placeholder ever; nil is never a bind)."
  (flet ((fragments-for (boolean-clause)
           (let* ((compiled
                    (compile-model
                      (pt-model
                        :about (pt-page-def
                                 :filter `((:pages :slug :eq "about")
                                           ,boolean-clause)))))
                   (about (getf compiled :about)))
             (values (getf about :page-where)
               (getf about :page-params)))))
    (multiple-value-bind (frags params)
      (fragments-for '(:pages :published :eq t))
      (is (equal '("rt_pages.page_slug = $1" "rt_pages.page_published")
        frags))
      (is (equal '("about") params)))
    (multiple-value-bind (frags params)
      (fragments-for '(:pages :published :eq nil))
      (is (equal '("rt_pages.page_slug = $1" "not rt_pages.page_published")
        frags))
      (is (equal '("about") params)))
    (multiple-value-bind (frags params)
      (fragments-for '(:pages :published :ne t))
      (is (equal '("rt_pages.page_slug = $1" "not rt_pages.page_published")
        frags))
      (is (equal '("about") params)))
    (multiple-value-bind (frags params)
      (fragments-for '(:pages :published :ne nil))
      (is (equal '("rt_pages.page_slug = $1" "rt_pages.page_published")
        frags))
      (is (equal '("about") params)))))

(test pt-compile-success-mixed-numbering
  "Bind numbering runs across clauses: a text bind, a boolean
no-bind, a text bind is $1, bare column, $2 — and :page-params is
(\"about\" \"x\")."
  (let* ((compiled
           (compile-model
             (pt-model
               :about (pt-page-def
                        :filter '((:pages :slug :eq "about")
                                  (:pages :published :eq t)
                                  (:pages :title :eq "x"))))))
          (about (getf compiled :about)))
    (is (equal '("rt_pages.page_slug = $1"
                 "rt_pages.page_published"
                 "rt_pages.page_title = $2")
      (getf about :page-where)))
    (is (equal '("about" "x") (getf about :page-params)))))

(test pt-compile-success-zero-params
  "A boolean identity column pins with a truthiness form and binds
nothing: :page-params is () and the page compiles (a zero-bind page
is a legal compiled shape)."
  (let* ((model
           (compile-model
             (list
               :flags '(:table t :create :auto :update :auto
                         :delete :auto :display t
                         :type-roles ("editor")
                         :fields (:live (:type :boolean :identity t
                                          :not-null t
                                          :ui (:label "Live"
                                            :widget :checkbox)
                                          :source (:view :main
                                                    :column :live
                                                    :agg :first)
                                          :column t))
                         :list-form (:fields t)
                         :add-form (:fields t)
                         :update-form (:fields t)
                         :view-form (:fields t))
               :flag-page (pt-page-def :of :flags
                             :filter '((:flags :live :eq t))
                             :view-form '(:fields (:live))
                             :fields '(:live
                                        (:ui (:widget :checkbox)))))))
          (flag-page (getf model :flag-page)))
    (is (equal '("rt_flags.flag_live") (getf flag-page :page-where)))
    (is (equal '() (getf flag-page :page-params)))))

(test pt-compile-success-hyphenated-alias
  "A hyphenated page field key aliases through to-sql-identifier —
:sub-title aliases as sub_title, never the raw key."
  (let* ((base (pt-pages-type))
          (base-fields (getf base :fields))
          (pub-at (position :published base-fields))
          (fields-with-sub
            (append
              (subseq base-fields 0 pub-at)
              '(:sub-title (:type :text
                             :ui (:label "Sub")
                             :source (:view :main :column :sub-title
                                       :agg :first)
                             :column t))
              (subseq base-fields pub-at)))
          (pages-with-sub
            (add-to-plist base (list :fields fields-with-sub)))
          (model (compile-model
                   (list :pages pages-with-sub
                     :about (pt-page-def
                              :fields '(:sub-title
                                         (:ui (:label "Sub")))
                              :view-form '(:fields (:sub-title)))))))
    (is (equal '("rt_pages.page_sub_title as sub_title"
                 "rt_pages.id as id")
      (getf (getf model :about) :page-columns)))))

(test pt-compile-success-projection-cap
  ":page-columns is the View-form projection plus :id — never the
full declared :fields when the view form omits one (list-result
passes supplied records through unfiltered; only the cap keeps the
omitted column off the wire)."
  (let ((about (getf (pt-compiled-worked) :about)))
    (is (equal '("rt_pages.page_title as title"
                 "rt_pages.page_body as body"
                 "rt_pages.id as id")
      (getf about :page-columns)))))

;;; ---------------------------------------------------------------------------
;;; Behavior suite: fixture seed + REST harness
;;; ---------------------------------------------------------------------------

(in-suite page-type-behavior)

(defun th-pt-seed ()
  "Seed the page-type-test fixture: about (published t) / other
(published SQL NULL — the bare-form NULL-exclusion row) / draft
(published false — the not-form row) as admin, the about and draft
rows carrying the editor role so the editor can update them; reader
holds public (page read, no role on :pages), pt-editor holds
editor. No guide row (the empty state)."
  (be-insert :pages
    '(:slug "about" :title "About Us"
      :body "We **build** apps." :published :true)
    "admin" :roles '("editor"))
  (be-insert :pages
    '(:slug "other" :title "Other Page"
      :body "Other body." :published :null)
    "admin")
  (be-insert :pages
    '(:slug "draft" :title "Draft Page"
      :body "Draft body." :published :false)
    "admin" :roles '("editor"))
  (th-make-user "reader" :roles '("public"))
  (th-make-user "pt-editor" :roles '("editor"))
  (th-make-user "pt-outsider")
  ;; Every user is born holding public (add-user's default roles);
  ;; the invisible-page probe needs one without it.
  (a:remove-user-role *rbac* "pt-outsider" "public")
  nil)

(define-condition pt-aborted (error)
  ((code :initarg :code :reader pt-aborted-code)
    (message :initarg :message :reader pt-aborted-message))
  (:documentation "Stand-in for hunchentoot's abort-request-handler
(the atr-aborted shape): the abort must not return."))

(defun th-pt-call-with-stubs (bindings thunk)
  "Install symbol-function stubs from BINDINGS for the duration of
THUNK, restoring on every exit including a signal (the
th-atr-call-with-stubs shape)."
  (let ((saved nil))
    (unwind-protect
        (progn
          (dolist (binding bindings)
            (push (cons (first binding)
                    (symbol-function (first binding)))
              saved)
            (setf (symbol-function (first binding)) (second binding)))
          (funcall thunk))
      (dolist (pair saved)
        (setf (symbol-function (car pair)) (cdr pair))))))

(defmacro th-pt-with-stubs (bindings &body body)
  `(th-pt-call-with-stubs ,bindings (lambda () ,@body)))

(defun th-pt-fake-request ()
  "A bare Hunchentoot request for direct handler calls (the
th-atr-fake-request shape)."
  (let ((h:*reply* (make-instance 'h:reply))
        (h:*session* nil)
        (h:*acceptor* *http-server*))
    (declare (dynamic-extent h:*reply* h:*session* h:*acceptor*))
    (make-instance 'h:request
      :headers-in (list (cons :x-forwarded-for "203.0.113.7"))
      :method :get
      :uri "/api/test"
      :server-protocol :http/1.1
      :local-addr "127.0.0.1"
      :local-port 8081
      :remote-addr "127.0.0.1"
      :remote-port 12345)))

(defun th-pt-call (fn &rest args &key (token "pt-token-admin")
                     tree render-json
                   &allow-other-keys)
  "Call REST easy-handler FN with ARGS under a transport-only stub
harness (the th-atr-call shape): token extraction / validation
against a fixed admin token, record-usage no-op, parse-posted-json
TREE, abort-request signals pt-aborted with the status, and
render-output identity (or real, with RENDER-JSON, so the caller
receives the actual JSON wire output). The fake request carries no
parameters, so every easy-handler :init-form default applies. Everything
authorization-shaped runs for real."
  (declare (ignore token tree render-json))
  (let ((handler-args
          (loop for (key val) on args by #'cddr
            unless (member key '(:token :tree :render-json))
            append (list key val)))
        (stubs
          (append
            (list
              (list 'get-bearer-token (lambda () "pt-token-admin"))
              (list 'get-access-token (lambda () "pt-token-admin"))
              (list 'validate-access-token
                (lambda (tok)
                  (declare (ignore tok))
                  "admin"))
              (list 'record-usage
                (lambda (&rest rest) (declare (ignore rest)) nil))
              (list 'parse-posted-json (lambda () (getf args :tree)))
              (list 'abort-request
                (lambda (error-code message &optional details-plist)
                  (declare (ignore details-plist))
                  (error 'pt-aborted :code error-code
                    :message message))))
            (unless (getf args :render-json)
              (list (list 'render-output (lambda (result) result)))))))
    (th-pt-call-with-stubs stubs
      (lambda ()
      ;; A fake request bound (the atr shape: h:real-remote-addr
      ;; evaluates for record-usage's :ip even under the stub), but
      ;; carrying NO parameters — compute-parameter returns nil for
      ;; every easy-handler var, so each :init-form default applies:
      ;; the exact wire default a bare URL produces (rest-list's
      ;; form = \"list-form\").
      (let ((h:*request* (th-pt-fake-request))
            (h:*reply* (make-instance 'h:reply)))
        (apply fn handler-args))))))

;;; ---------------------------------------------------------------------------
;;; Behavior: reads
;;; ---------------------------------------------------------------------------

(test ptb-reader-sees-page
  "The reader who cannot read :pages still sees the page: one
record, the content row's id, per-record update / delete :false,
envelope caps :false, view-form present, list-form absent,
:page-type :true — and no :slug (a declared page field deliberately
omitted from :view-form; only the :page-columns cap keeps it off
the wire, list-result does not filter records)."
  (let ((r (be-list :about "reader")))
    (is (equal 1 (getf r :total)))
    (is (eq :true (getf r :page-type)))
    (is (eq :false (getf r :create)))
    (is (eq :false (getf r :update)))
    (is (eq :false (getf r :delete)))
    (is-true (getf r :view-form))
    (is-false (getf r :list-form))
    (let ((record (first (getf r :records))))
      (is (equal "About Us" (getf record :title)))
      (is (equal "We **build** apps." (getf record :body)))
      (is (uuid-p (getf record :id)))
      (is-false (getf record :slug)
        "the unprojected declared field must not ride the record")
      (is (eq :false (getf record :update)))
      (is (eq :false (getf record :delete))))))

(test ptb-heading-wire
  "be-list on :about emits the finalized :widget :heading and
:hide-label t on :title in the view form — the :ui plist rides
verbatim (the markdown-wire precedent), and the page-body render
branches on exactly this pair."
  (let* ((r (be-list :about "reader"))
         (title (getf (getf r :view-form) :title)))
    (is (eq :heading (getf title :widget)))
    (is (eq t (getf title :hide-label)))))

(test ptb-null-exclusion
  "The bare-column truthiness form against SQL NULL: :other-page
reads total 0 THOUGH the other row exists (a missing row would also
be 0 — this is the discriminating assertion)."
  (let ((row (first
               (getf (be-list :pages "admin"
                        :filters '((:pages :slug :eq "other")))
                 :records))))
    (is-true row "the other row exists (admin, content type)")
    (is (equal 0 (getf (be-list :other-page "reader") :total)))))

(test ptb-not-form-row
  "The not-column form against a stored false: :draft-page returns
the draft row (a NULL or missing row would also match 0 — the
discriminating assertion runs the other way)."
  (let ((r (be-list :draft-page "reader")))
    (is (equal 1 (getf r :total)))
    (is (equal "Draft Page"
      (getf (first (getf r :records)) :title)))))

(test ptb-empty-state
  ":guide (whose :last-days ride-along names the unprojected
:created-at default field, and whose row does not exist) returns
:records nil, :total 0, and still emits view-form — zero rows is
success, not a validation error."
  (let ((r (be-list :guide "reader")))
    (is (equal 0 (getf r :total)))
    (is-false (getf r :records))
    (is-true (getf r :view-form))))

(test ptb-be-types-flag
  "be-types includes :about with :page-type t and does not include
the key on :pages."
  (let ((about (find :about (be-types "reader")
                  :key (lambda (e) (getf e :name))))
        (pages (find :pages (be-types "admin")
                  :key (lambda (e) (getf e :name)))))
    (is (eq t (getf about :page-type)))
    (is-false (getf pages :page-type))))

(test ptb-rest-types-encodes-true
  "rest-types encodes the flag as true in JSON — pinning the
[]-is-truthy trap explicitly (a bare nil would serialize as [], which
JS reads as truthy). The key appears exactly once per page type (the
fixture has four) and never on a non-page."
  (let ((json (th-pt-call #'rest-types :render-json t))
        (pages-count
          (count-if
            (lambda (e) (eq (getf e :page-type) t))
            (be-types "admin"))))
    (is (search "\"page-type\":true" json))
    (is (equal pages-count
          (loop for start = 0 then (1+ pos)
            for pos = (search "\"page-type\"" json :start2 start)
            count pos
            while pos)))))

(test ptb-rest-list-serialization
  "A rest-list response for :about serializes records / total /
sort / capability flags with no list-form member (the fake request
carries no form parameter, so the handler's :init-form \"list-form\"
default means be-list is asked for :list-form — and page-be-list's
form-ignore is the only thing that serves it)."
  (let ((json (th-pt-call #'rest-list :type "about" :render-json t)))
    (is (search "\"type-key\":\"about\"" json))
    (is (search "\"total\":1" json))
    (is (search "\"view-form\"" json))
    (is-false (search "\"list-form\"" json))
    (is (search "\"page-type\":true" json))))

(test ptb-form-default-ignore
  "The form-ignore pin: rest-list with NO form parameter (the bare
URL; the handler's :init-form \"list-form\" means be-list is asked
for :list-form) still returns the view-form envelope. A reader who
later 'fixes' page-be-list to honor the request form breaks this
test first."
  (let ((r (th-pt-call #'rest-list :type "about")))
    (is-true (getf r :view-form))
    (is-false (getf r :list-form))))

;;; ---------------------------------------------------------------------------
;;; Behavior: rejections
;;; ---------------------------------------------------------------------------

(test ptb-filters-rejected
  "Request-time filters — list form and bare UUID — and sort are
400s on a page; limit / offset are accepted and ignored (a page is
not a page of a list, and paging cannot hide a duplicate)."
  (signals validation-error
    (be-list :about "reader" :filters '((:about :title :eq "x"))))
  (signals validation-error
    (be-list :about "reader" :filters "74c4a992-fc4b-4b77-8564-ce5e5539b731"))
  (signals validation-error
    (be-list :about "reader" :sort '(:title :asc)))
  (let ((r (be-list :about "reader" :limit 1 :offset 5)))
    (is (equal 1 (getf r :total)))))

(test ptb-search-rejected
  "Non-blank search on a page 400s before the page branch (page
fields are not :searchable — the same path rollups take)."
  (signals validation-error
    (be-list :about "reader" :search "xx")))

(test ptb-write-endpoints-rejected
  "be-insert / be-rec on a page signal (the widened
valid-non-rollup-type-key), the way they 400 a rollup."
  (signals validation-error
    (be-insert :about '(:title "x") "admin"))
  (signals validation-error
    (be-rec "00000000-0000-0000-0000-000000000001" "admin"
      :type-key :about)))

(test ptb-column-read-rejected
  "be-list-column on a page signals (the page guard — a page is
not a column source; /api/column stays legal on rollups)."
  (signals validation-error
    (be-list-column :about :title "reader")))

(test ptb-upload-rejected
  "A page-targeted /api/upload is a deterministic 400 with no
nil-field work: the widened guard fires right after parse-type,
before file-field / path-field lookups would format nils and an
fs-path from a nil path."
  (let ((h:*request* (th-pt-fake-request))
        (h:*reply* (make-instance 'h:reply)))
    (th-pt-with-stubs
        (list
          (list 'h:header-in*
            (lambda (name &optional request)
              (declare (ignore request))
              (when (eq name :content-type) "multipart/form-data")))
          (list 'h:post-parameter
            (lambda (name &optional request)
              (declare (ignore request))
              (when (equal name "type") "about")))
          (list 'get-bearer-token (lambda () "pt-token-admin"))
          (list 'get-access-token (lambda () "pt-token-admin"))
          (list 'validate-access-token
            (lambda (tok)
              (declare (ignore tok))
              "admin"))
          (list 'abort-request
            (lambda (error-code message &optional details-plist)
              (declare (ignore details-plist))
              (error 'pt-aborted :code error-code
                :message message))))
      (handler-case
          (progn
            (rest-upload)
            (fail "page-targeted upload should have been rejected"))
        (pt-aborted (e) (is (= 400 (pt-aborted-code e))))))))

;;; ---------------------------------------------------------------------------
;;; Behavior: RBAC surface
;;; ---------------------------------------------------------------------------

(test ptb-landing-page
  "The configured :landing-page :about is the reader's effective
landing (be-landing-page returns whatever be-types can see)."
  (is (eq :about (be-landing-page "reader"))))

(test ptb-public-type-p-excludes-pages
  "public-type-p, de-vacuated: under :guest-allowed t the control
type :blurb qualifies, :about does not, and the guest DOES hold
read on type-about — every other leg passes, so the :page exclusion
is the only failing one."
  (is-true (public-type-p :blurb))
  (is-false (public-type-p :about))
  (is-true
    (a:user-allowed *rbac* *guest-user* "read"
      (type-resource-name :about))
    "without this probe the false above proves nothing (the guest
gate would vacuously exclude every type)"))

(test ptb-information-schema
  "The content table exists; the page never reaches
information_schema with a nil :table-name."
  (is-true (table-exists "rt_pages"))
  (is-false (table-exists "rt_about")))

(test ptb-invisible-page
  "A user who holds neither public nor admin does not see :about in
be-types, and be-list signals on it (the type-resource read gate)."
  (is-false
    (find :about (be-types "pt-outsider")
      :key (lambda (e) (getf e :name))))
  (signals validation-error (be-list :about "pt-outsider")))

(test ptb-content-type-list-no-signal
  "The reader's direct be-list on the content type does not signal
— after the API type-roles access fix an ordinary type needs only
an authenticated user and returns the row-filtered (here: empty)
list."
  (let ((r (be-list :pages "reader")))
    (is (equal 0 (getf r :total)))))

(test ptb-content-update-fresh-page-read
  "The content-update → fresh page-read pin: the editor updates the
about row's :body on :pages (a seeded row carrying the editor
role), and the next be-list on :about returns the new body.
Restored under unwind-protect so the suite stays
order-independent."
  (let ((id (be-value-id :pages :slug "about" "admin")))
    (is-true id)
    (unwind-protect
        (progn
          (be-update :pages id '(:body "Edited **body**.") "pt-editor")
          (is (equal "Edited **body**."
                (getf (first
                        (getf (be-list :about "reader") :records))
                  :body))))
      (be-update :pages id '(:body "We **build** apps.") "pt-editor"))))
