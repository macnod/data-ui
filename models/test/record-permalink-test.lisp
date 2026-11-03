;; Test fixture for record permalinks: the record-permalink sibling
;; ("<field>-id" UUID), the fe-fields :target wire key, suppression of
;; the ambiguous display shapes, and the alias-global conversion
;; exclusion.  Used by tests/record-permalink-tests.lisp.
;;
;; Distinctness rule: every non-suppressed target field on the child
;; aims at its own distinct target type (two working fields on one
;; target is suppression (a) and would hollow out the positive
;; matrix).  Seeding note: rows whose FK column is declared
;; :type :boolean / :timestamp must be inserted with a null FK and
;; populated afterwards through be-set-field-value (valid-data would
;; reject the UUID against the declared display type).
'(:title "Record Permalink Test"
  :name "record-permalink-test"
  :version "0.1"
  :domain "record-permalink-test.demo.data-ui.com"
  :repl nil
  :types
  (:writers
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:writers)))
      :fields
      (:pen-name
        (:type :text :identity t :sortable t :searchable t
          :ui (:label "Pen Name" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :pen-name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

   ;; Invisible-type case: :display nil keeps :editors out of
   ;; be-types (admin can read rows; the type never reaches the
   ;; frontend).
   :editors
    (:table t
      :create :auto :update :auto :delete :auto :display nil
      :type-roles ("logged-in")
      :views (:main (:tables (:editors)))
      :fields
      (:name
        (:type :text :identity t :sortable t
          :ui (:label "Name" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)))

   :topics
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:topics)))
      :fields
      (:name
        (:type :text :identity t :sortable t
          :ui (:label "Topic" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

   ;; Case (1): identity-column display source.
   :authors
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:authors)))
      :fields
      (:name
        (:type :text :identity t :sortable t
          :ui (:label "Name" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

   ;; Case (1): identity-column display source.
   :books
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:books :authors)))
      :fields
      (:title
        (:type :text :identity t :sortable t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t :unique t)
        :author
        (:type :text :target :authors
          :ui (:label "Author" :widget :search-select)
          :source (:view :main :table :authors :column :name
                   :agg :first)
          :column t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

   ;; Suppression (b): self-referencing target (no self-join alias).
   ;; Also the timestamp hazard owner's target: :magazines
   ;; :stamp-editor targets this type (declared before :magazines —
   ;; create-tables follows author order).
   :chapters
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:chapters)))
      :fields
      (:name
        (:type :text :identity t :sortable t
          :ui (:label "Name" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        :follows
        (:type :text :target :chapters
          :ui (:label "Follows" :widget :search-select)
          :source (:view :main :table :chapters :column :name
                   :agg :first)
          :column t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

   ;; Case (2): non-identity display column (display reads :pen-name,
   ;; the sibling must still be the UUID); case (10): an M2M list field
   ;; beside the singular targets (the no-:join-table leg of the
   ;; predicate).  Also the conversion hazards (owning declarations on
   ;; target fields).  Distinctness rule: every working field has its
   ;; own target type — writer owns :writers, flag-editor owns
   ;; :editors, stamp-editor owns :chapters (two fields on one target
   ;; is suppression (a) and would hollow out the positive matrix).
   ;; :chapters is declared above; author order only matters for
   ;; create-tables (both exist before any row is written).
   :magazines
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:magazines :writers :editors :chapters)))
      :fields
      (:title
        (:type :text :identity t :sortable t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t :unique t)
        :writer
        (:type :text :target :writers
          :ui (:label "Writer" :widget :search-select)
          :source (:view :main :table :writers :column :pen-name
                   :agg :first)
          :column t)
        ;; Conversion hazard, boolean-declared owner.
        :flag-editor
        (:type :boolean :target :editors
          :ui (:label "Flag Editor" :widget :checkbox)
          :source (:view :main :table :editors :column :name
                   :agg :first)
          :column t)
        ;; Conversion hazard, timestamp-declared owner (own target
        ;; type :chapters — a second :editors field would be
        ;; suppression (a)).
        :stamp-editor
        (:type :timestamp :target :chapters
          :ui (:label "Stamp Editor" :widget :textbox)
          :source (:view :main :table :chapters :column :name
                   :agg :first)
          :column t)
        ;; Alias-reuse: computed fields reading the writer storage
        ;; alias (a :uuid reuser on the identity-converted owner).
        :writer-ref
        (:type :uuid
          :ui (:label "Writer Ref" :widget :textbox :read-only t)
          :source (:view :main :table :magazines :column :writer
                   :agg :first))
        ;; Alias-reuse beside the boolean owner (any declared type on
        ;; the reuser; the hazard is the owner's declaration).
        :flag-editor-ref
        (:type :uuid
          :ui (:label "Flag Ref" :widget :textbox :read-only t)
          :source (:view :main :table :magazines :column :flag-editor
                   :agg :first))
        ;; Alias-reuse beside the timestamp owner.
        :stamp-editor-ref
        (:type :uuid
          :ui (:label "Stamp Ref" :widget :textbox :read-only t)
          :source (:view :main :table :magazines :column :stamp-editor
                   :agg :first))
        ;; M2M list field: never a sibling, never a :target wire key.
        :topics
        (:type :list
          :ui (:label "Topics" :widget :checkbox-list)
          :source (:view :main :table :topics :column :name
                   :agg :distinct)
          :join-table :child-topix))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

   ;; Suppression (a): two target fields sharing one target type.  Both
   ;; display through the single join edge (pre-existing engine gap);
   ;; no sibling on either, :target wire key on both.
   :reprints
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:reprints :authors)))
      :fields
      (:title
        (:type :text :identity t :sortable t
          :ui (:label "Title" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :title :agg :first)
          :column t :not-null t :unique t)
        :original-author
        (:type :text :target :authors
          :ui (:label "Original" :widget :search-select)
          :source (:view :main :table :authors :column :name
                   :agg :first)
          :column t)
        :translated-author
        (:type :text :target :authors
          :ui (:label "Translation" :widget :search-select)
          :source (:view :main :table :authors :column :name
                   :agg :first)
          :column t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

   ;; Target for (3): cross-ref's own-column suppression target —
   ;; visible, ordinary rows, but no test reads them through a join.
   :record-permalink-tags
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:record-permalink-tags)))
      :fields
      (:name
        (:type :text :identity t :sortable t
          :ui (:label "Tag" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

   ;; Target for (7): visible type, rows granted to admin only.
   :secret-stashes
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:secret-stashes)))
      :fields
      (:name
        (:type :text :identity t :sortable t
          :ui (:label "Stash" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

   ;; Target for (8): the :user column carries :target :users so the
   ;; scope binds the users.id UUID.
   :scoped-notes
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:scoped-notes :users)))
      :fields
      (:note
        (:type :text :identity t :sortable t
          :ui (:label "Note" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :note :agg :first)
          :column t :not-null t :unique t)
        :user
        (:type :text :target :users
          :ui (:label "User" :widget :textbox)
          :source (:view :main :table :users :column :name
                   :agg :first)
          :column t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

   ;; Target for (4): distinct target for the child's nullable FK
   ;; (sharing :record-permalink-tags with :cross-ref would be
   ;; suppression (a) and would hollow out the positive matrix).
   :maybe-tags
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views (:main (:tables (:maybe-tags)))
      :fields
      (:name
        (:type :text :identity t :sortable t
          :ui (:label "Maybe Tag" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

   ;; The child carrying the working matrix: cases (3) own-column
   ;; cross-source suppression + conversion retention, (4) nullable FK,
   ;; (5) :ui (:table ...) hint, (6) invisible target type, (7)
   ;; visible-type row-inaccessible target, (8) scope :user display,
   ;; plus alias-reuse probes on the boolean owner (any declared type).
   :record-permalink-child
    (:table t
      :create :auto :update :auto :delete :auto :display t
      :type-roles ("logged-in")
      :views
      (:main (:tables (:record-permalink-child :record-permalink-tags
                     :maybe-tags :authors :editors :secret-stashes
                     :child-topix :topics :scoped-notes :users)))
      :fields
      (:name
        (:type :text :identity t :sortable t
          :ui (:label "Name" :widget :textbox)
          :validations (:required)
          :source (:view :main :column :name :agg :first)
          :column t :not-null t :unique t)
        ;; (3) own-column cross-source shape: display :source is the
        ;; owning table's own column, so the display never described
        ;; the referent — suppression (c), and the own-column
        ;; conversion retention probe (a :boolean display whose alias
        ;; is its own storage column keeps today's conversion).
        :cross-ref
        (:type :boolean :target :record-permalink-tags
          :ui (:label "Cross" :widget :checkbox)
          :source (:view :main :table :record-permalink-child
                   :column :cross-ref :agg :first)
          :column t)
        ;; (4) nullable FK to a third target, left null on child-2
        ;; (distinct target type per the distinctness rule). Also the
        ;; list-cell chip leg of the browser matrix: :filter-with
        ;; :reference makes the Children list cell a filter chip (the
        ;; authored form of the reference-chip family), so plan step
        ;; 37's "a list cell for the same field still filters and
        ;; does not navigate" is exercisable on this fixture.
        :maybe-tag
        (:type :text :target :maybe-tags
          :ui (:label "Maybe Tag" :widget :search-select
            :filter-with :reference)
          :source (:view :main :table :maybe-tags
                   :column :name :agg :first)
          :column t)
        ;; (5) working target carrying a :ui (:table ...) hint (the
        ;; image-URL hint; :target and :table coexist).
        :hinted-author
        (:type :text :target :authors
          :ui (:label "Hinted" :widget :search-select
            :table :record-permalink-tags)
          :source (:view :main :table :authors :column :name
                   :agg :first)
          :column t)
        ;; (6) invisible target type (:editors has :display nil).
        :hidden-editor
        (:type :text :target :editors
          :ui (:label "Hidden Editor" :widget :search-select)
          :source (:view :main :table :editors :column :name
                   :agg :first)
          :column t)
        ;; (7) visible-type, row-inaccessible target: :secret-stashes
        ;; is visible but its rows are granted to admin only (seeded
        ;; with the admin-exclusive role), so a non-admin viewer sees
        ;; the type but no row.
        :stash
        (:type :text :target :secret-stashes
          :ui (:label "Stash" :widget :search-select)
          :source (:view :main :table :secret-stashes :column :name
                   :agg :first)
          :column t)
        ;; (8) scoped display: the target type declares a :user column
        ;; (:target :users) and resolve-scope-alias scopes the display
        ;; collapse on it; a viewer with no scoped row gets no
        ;; sibling.
        :scoped-note
        (:type :text :target :scoped-notes
          :ui (:label "Scoped Note" :widget :search-select)
          :source (:view :main :table :scoped-notes :column :note
                   :agg :first :scope :user)
          :column t)
        ;; Alias-reuse probe on the boolean owner (cross-ref): a
        ;; computed field reading cross-ref's storage alias.
        :cross-ref-echo
        (:type :uuid
          :ui (:label "Cross Echo" :widget :textbox :read-only t)
          :source (:view :main :table :record-permalink-child
                   :column :cross-ref :agg :first)))
      :list-form (:fields t)
      :update-form (:fields t)
      :view-form (:fields t)
      :add-form (:fields t))

   ;; Joiner for the child's M2M topics field.
   :child-topix
    (:table t :is-joiner t :internal t
      :fields
      (:reference (:target :record-permalink-child)
        :reference (:target :topics)))))
