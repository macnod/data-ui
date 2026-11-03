(in-package :data-ui)

(def-suite record-permalink-suite
  :description "Record permalinks: the \"<field>-id\" sibling UUID on
be-rec / be-list records, the fe-fields :target wire key, suppression
of the ambiguous display shapes, and the alias-global conversion
exclusion.")

(in-suite record-permalink-suite)

;;; ---------------------------------------------------------------------------
;;; Seeding
;;; ---------------------------------------------------------------------------

(defvar *rp-author* nil)
(defvar *rp-writer* nil)
(defvar *rp-tag-1* nil)
(defvar *rp-tag-2* nil)
(defvar *rp-editor* nil)
(defvar *rp-chapter* nil)
(defvar *rp-cross-tag* nil)
(defvar *rp-stash* nil)
(defvar *rp-note-mine* nil)
(defvar *rp-note-other* nil)
(defvar *rp-viewer* "rp-viewer")

(defun seed-record-permalink-fixture ()
  "Seed the record-permalink-test fixture. Boolean/timestamp-declared
FK rows are inserted with a null FK and populated afterwards via
be-set-field-value (valid-data would reject the UUID against the
declared display type). Returns the id alist bound into *fixture*."
  ;; A non-admin viewer for the scoped and row-access cases.
  (th-make-user *rp-viewer* :roles (list "logged-in"))
  ;; Authors / writers / editors / topics.
  (setf *rp-author* (be-insert :authors '(:name "Ursula") "admin"))
  (setf *rp-writer* (be-insert :writers '(:pen-name "Virginia")
                   "admin"))
  (setf *rp-editor* (be-insert :editors '(:name "Max") "admin"))
  ;; The tags live in their own distinct type (the distinctness
  ;; rule): maybe-tag's target. cross-ref still points its FK UUID
  ;; at *rp-tag-1* — its display is the own column, so a referent in
  ;; a different type never surfaces.
  (setf *rp-tag-1* (be-insert :maybe-tags '(:name "tag-1") "admin"))
  (setf *rp-tag-2* (be-insert :maybe-tags '(:name "tag-2") "admin"))
  ;; cross-ref's FK constraint is real: it needs an actual
  ;; :record-permalink-tags row (its display is the own column, so
  ;; this row is never read through a join).
  (setf *rp-cross-tag*
    (be-insert :record-permalink-tags '(:name "cross") "admin"))
  (be-insert :topics '(:name "science") "admin")
  ;; Books: case (1) identity-column display.
  (be-insert :books '(:title "Left Hand" :author "Ursula") "admin")
  (be-insert :books '(:title "Planet" :author "Ursula") "admin")
  ;; The chapter: stamp-editor's target row (its own distinct type —
  ;; feeding it an :editors UUID dies on the real FK constraint) and
  ;; the suppression (b) self-FK probe row.  Must exist before the
  ;; magazines: stamp-editor's FK constraint demands it.
  (setf *rp-chapter* (be-insert :chapters '(:name "C1") "admin"))
  ;; Magazines: non-identity display, boolean/timestamp hazards with
  ;; populated + null rows, alias-reuse reusers, M2M field.
  (let ((m1 (be-insert :magazines
              '(:title "M1" :writer "Virginia") "admin"))
         (m2 (be-insert :magazines '(:title "M2") "admin")))
    (be-set-field-value :magazines m1 :flag-editor *rp-editor*
      "admin")
    (be-set-field-value :magazines m1 :stamp-editor *rp-chapter*
      "admin"))
  ;; Reprints (suppression a).
  (be-insert :reprints
    '(:title "R1" :original-author "Ursula") "admin")
  ;; Child rows: per-record association (each row its own target).
  ;; The logged-in role grant lets the non-admin viewer read them.
  (be-insert :record-permalink-child
    `(:name "child-1" :maybe-tag "tag-1" :hinted-author "Ursula"
      :hidden-editor "Max") "admin"
    :roles (list "logged-in"))
  (be-insert :record-permalink-child
    `(:name "child-2" :maybe-tag "tag-2") "admin"
    :roles (list "logged-in"))
  ;; The stash row: visible type, row granted to admin only.
  (setf *rp-stash*
    (be-insert :secret-stashes '(:name "stash-1") "admin"))
  (be-update :secret-stashes *rp-stash* nil "admin"
    :roles (list "admin"))
  ;; Scoped notes: one owned by the viewer, one by admin.
  (setf *rp-note-mine*
    (be-insert :scoped-notes `(:note "mine" :user ,*rp-viewer*)
      "admin"))
  (setf *rp-note-other*
    (be-insert :scoped-notes '(:note "other" :user "admin")
      "admin"))
  ;; Point child-1 at the stash and the viewer's scoped note. No
  ;; be-update here: cross-ref's own-column boolean display converts
  ;; even a NULL FK to :TRUE in the record, and be-update's full-data
  ;; round-trip feeds that back through resolve-reference-id (which
  ;; expects a string) — every be-update on this type signals. Raw
  ;; single-column writes instead, the same escape hatch as the
  ;; hazard fields: stash / scoped-note bind the row UUIDs directly,
  ;; and the boolean own-column cross-ref stores the FK UUID on its
  ;; uuid column (compile forces :target storage to UUID; the
  ;; :type :boolean display conversion turns the stored UUID into
  ;; :TRUE on read — the own-column retention probe's value).
  (let ((c1 (be-id :record-permalink-child
              '((:record-permalink-child :name :eq "child-1"))
              "admin")))
    (be-set-field-value :record-permalink-child c1 :stash *rp-stash*
      "admin")
    (be-set-field-value :record-permalink-child c1 :scoped-note
      *rp-note-mine* "admin")
    (be-set-field-value :record-permalink-child c1 :cross-ref
      *rp-cross-tag* "admin"))
  (list :author *rp-author* :writer *rp-writer*
    :tag-1 *rp-tag-1* :tag-2 *rp-tag-2*
    :editor *rp-editor* :stash *rp-stash*
    :note-mine *rp-note-mine* :note-other *rp-note-other*))

(defun rp-child-id (name)
  (be-id :record-permalink-child
    `((:record-permalink-child :name :eq ,name)) "admin"))

(defun rp-sibling-p (record field-key)
  "Member test for the synthetic sibling key (getf cannot distinguish
absent from present-nil; the contract omits the key entirely when
the FK is null)."
  (member (u:make-keyword (format nil "~a-id" field-key)) record))

(defun rp-rec (type-key name &key (form :update-form)
                (user "admin"))
  (getf (be-rec (be-id type-key
                  `((,type-key
                      ,(identity-field type-key)
                      :eq ,name))
                  user)
                user
                :form form)
    :record))

;;; ---------------------------------------------------------------------------
;;; Sibling emission: the positive matrix
;;; ---------------------------------------------------------------------------

(test sibling-pairs-with-display
  "A populated FK emits \"<field>-id\" equal to the referenced row's
UUID AND the field's own value equal to that same row's identity
display — the pairing is the assertion (fetch the referenced row by
the sibling and check both halves)."
  (let* ((rec (rp-rec :books "Left Hand"))
         (sib (getf rec :author-id)))
    (is-true (rp-sibling-p rec :author)
      "populated FK must carry the :author-id key")
    (is (equalp *rp-author* sib)
      "sibling must equal the referenced row's UUID")
    ;; Fetch the referenced row by the sibling and check the display.
    (let ((target (getf (be-rec sib "admin") :record)))
      (is (equal "Ursula" (getf rec :author))
        "display value must be the target's identity string")
      (is (equal (getf rec :author) (getf target :name))
        "display and target identity must name the same row"))))

(test sibling-per-record-association
  "Each child row's sibling pairs with that row's own target
(multi-row be-list with different FKs per row)."
  (let ((rows (getf (be-list :record-permalink-child "admin")
                 :records)))
    (is (= 2 (length rows)))
    (let ((c1 (find "child-1" rows :key
                 (lambda (r) (getf r :name)) :test #'equal))
           (c2 (find "child-2" rows :key
                 (lambda (r) (getf r :name)) :test #'equal)))
      (is (equalp *rp-tag-1* (getf c1 :maybe-tag-id)))
      (is (equalp *rp-tag-2* (getf c2 :maybe-tag-id)))
      (is (equal "tag-1" (getf c1 :maybe-tag)))
      (is (equal "tag-2" (getf c2 :maybe-tag))))))

(test sibling-omitted-on-null-fk
  "A null FK omits the key entirely (assert absence, not nil)."
  (let ((rec (rp-rec :books "Planet")))
    ;; child-2 has no hinted-author; books both have authors. Use the
    ;; child's nullable maybe-tag via a null row instead: M2 magazine.
    (let ((m2 (rp-rec :magazines "M2")))
      (is-false (rp-sibling-p m2 :writer)
        "null FK must omit the :writer-id key entirely"))))

(test sibling-on-non-identity-display-column
  "A :source at a non-identity column (pen-name) does not change the
sibling: still the UUID, display still the pen-name."
  (let ((rec (rp-rec :magazines "M1")))
    (is (equalp *rp-writer* (getf rec :writer-id)))
    (is (equal "Virginia" (getf rec :writer)))))

(test invisible-target-still-emits
  "An invisible-target FK still emits the sibling when the child row
is readable (the frontend decides; the backend does not
second-guess)."
  (let ((rec (rp-rec :record-permalink-child "child-1")))
    (is (equalp *rp-editor* (getf rec :hidden-editor-id)))))

(test row-inaccessible-target-all-three-halves
  "The visible-type / row-inaccessible case: the target type is
present in be-types, direct be-rec on the target id returns no
record for that viewer, and the readable child's be-rec and be-list
records still contain the correct sibling UUID with the display
value unchanged."
  (is (find :secret-stashes (be-types *rp-viewer*)
        :key (lambda (e) (getf e :name)))
    ":secret-stashes must be visible to the viewer")
  (is-false (getf (be-rec *rp-stash* *rp-viewer*) :record)
    "the viewer must not read the stash row directly")
  (let* ((rec (rp-rec :record-permalink-child "child-1"
                :user *rp-viewer*))
         (rows (getf (be-list :record-permalink-child *rp-viewer*)
                  :records)))
    (is (equalp *rp-stash* (getf rec :stash-id))
      "the readable child still carries the correct sibling")
    (let ((c1 (find "child-1" rows :key
                 (lambda (r) (getf r :name)) :test #'equal)))
      (is (equalp *rp-stash* (getf c1 :stash-id))
        "be-list records carry it too"))))

;;; ---------------------------------------------------------------------------
;;; Conversion hazards: boolean / timestamp owners
;;; ---------------------------------------------------------------------------

(test boolean-owner-emits-uuid-populated
  "The boolean-declared working field emits the UUID on the populated
row (the conversion-bypass pin) and :true/:false retention only
where the display alias is the storage alias."
  (let ((m1 (rp-rec :magazines "M1")))
    (is (equalp *rp-editor* (getf m1 :flag-editor-id))
      "boolean owner's sibling must be the raw UUID, not :true")))

(test timestamp-owner-emits-uuid-populated
  "The timestamp-declared working field emits the UUID on the
populated row (its own distinct target :chapters); without the
exclusion the populated row would kill view-result outright (the
fixture compiling-and-returning is itself a pin)."
  (let ((m1 (rp-rec :magazines "M1")))
    (is (equalp *rp-chapter* (getf m1 :stamp-editor-id))
      "timestamp owner's sibling must be the raw UUID, not a
      timestamp string")))

(test hazard-owners-omit-on-null-rows
  "Both hazard owners omit the key on their null-FK rows."
  (let ((m2 (rp-rec :magazines "M2")))
    (is-false (rp-sibling-p m2 :flag-editor))
    (is-false (rp-sibling-p m2 :stamp-editor))))

(test own-column-boolean-keeps-conversion
  "The own-column boolean probe keeps today's conversion (:true, not
the raw UUID) — display alias = storage alias retains conversion —
and emits no sibling (suppression (c): its display source table is
the owning type)."
  (let* ((c1 (rp-rec :record-permalink-child "child-1"))
         (cross (getf c1 :cross-ref)))
    (is (eq cross :true)
      "own-column boolean display must stay :true (today's
      conversion), not the raw UUID")
    (is-false (rp-sibling-p c1 :cross-ref)
      "suppression (c) withholds the sibling on the own-column
      shape")))

;;; ---------------------------------------------------------------------------
;;; Alias-reuse: the two-layer pin
;;; ---------------------------------------------------------------------------

(test alias-reuse-uuid-identity-conversion
  "The :type :uuid alias-reuse field beside the identity-converted
owner emits the same value as the target field's sibling."
  (let ((m1 (rp-rec :magazines "M1")))
    (is (equalp (getf m1 :writer-id) (getf m1 :writer-ref)))))

(test alias-reuse-boolean-owner-two-layers
  "The reuser beside the boolean owner exposes the exclusion at its
two distinct layers: rec's second value carries the pristine UUID /
:null on the storage alias, while the collapsed public reuser field
carries the UUID (never :true — the boolean owner's conversion
cannot touch a claimed alias)."
  (multiple-value-bind (_ rows)
    (rec (be-id :magazines
          '((:magazines :title :eq "M1")) "admin")
      "admin")
    (declare (ignore _))
    (let* ((alias (u:tree-get *compiled-model* :magazines :views
                    :main :aliases :magazines :flag-editor))
           (raw (getf (first rows) alias))
           (m1 (rp-rec :magazines "M1")))
      (is (equalp *rp-editor* raw)
        "rec's second value must carry the pristine UUID")
      (is (equalp *rp-editor* (getf m1 :flag-editor-ref))
        "the collapsed public reuser must carry the UUID, not
        :true"))))

(test alias-reuse-null-rows
  "On the null-FK row the boolean owner's alias is :null in rec's
rows and the collapsed public reuser is nil (serialized [] on REST)
— never :true."
  (multiple-value-bind (_ rows)
    (rec (be-id :magazines
          '((:magazines :title :eq "M2")) "admin")
      "admin")
    (declare (ignore _))
    (let* ((alias (u:tree-get *compiled-model* :magazines :views
                    :main :aliases :magazines :flag-editor))
           (raw (getf (first rows) alias))
           (m2 (rp-rec :magazines "M2")))
      (is (eq :null raw)
        "rec's second value must carry the pristine :null")
      (is-false (getf m2 :flag-editor-ref)
        "the collapsed public reuser must be nil, not :true"))))

;;; ---------------------------------------------------------------------------
;;; Suppression shapes
;;; ---------------------------------------------------------------------------

(test shared-target-pair-suppressed
  "Two :target fields sharing one target type emit no sibling on
either, display value unchanged, both still carry fe-fields :target
equal to their common compiled :target."
  (let ((rec (rp-rec :reprints "R1")))
    (is-false (rp-sibling-p rec :original-author))
    (is-false (rp-sibling-p rec :translated-author))
    (is (equal "Ursula" (getf rec :original-author))
      "display value unchanged")
    (let ((fe (fe-fields :reprints "admin")))
      (is (eq :authors
           (u:tree-get fe :view-form :original-author :target)))
      (is (eq :authors
           (u:tree-get fe :view-form :translated-author :target))))))

(test self-fk-suppressed
  "A self-referencing :target emits no sibling and keeps its :target
wire key equal to the child type itself."
  (let ((rec (rp-rec :chapters "C1")))
    (is-false (rp-sibling-p rec :follows))
    (is (eq :chapters
         (u:tree-get (fe-fields :chapters "admin") :view-form
           :follows :target)))))

(test cross-source-suppressed-with-wire-key
  "A cross-source field emits no sibling (suppression (c)) and its
fe-fields metadata carries :target equal to the actual :target type
key — the wire key and suppression coexist."
  (let ((rec (rp-rec :record-permalink-child "child-1")))
    (is-false (rp-sibling-p rec :cross-ref)
      "cross-source shape must not emit a sibling")
    (is (eq :record-permalink-tags
         (u:tree-get (fe-fields :record-permalink-child "admin")
           :view-form :cross-ref :target))
      "the wire key still names the actual :target")))

(test m2m-field-no-sibling-no-wire-key
  "The M2M list field emits no sibling and its fe-fields entry has
no :target key at all."
  (let ((m1 (rp-rec :magazines "M1"))
         (fe (fe-fields :magazines "admin")))
    (is-false (rp-sibling-p m1 :topics))
    (is-false (member :target
                (u:tree-get fe :view-form :topics)))))

;;; ---------------------------------------------------------------------------
;;; The :target wire key
;;; ---------------------------------------------------------------------------

(test target-wire-key-on-working-field
  "A working target field wires :target as the actual :target."
  (is (eq :authors
       (u:tree-get (fe-fields :books "admin") :view-form :author
         :target))))

(test target-wire-key-with-table-hint
  "A target field carrying :ui (:table ...) still wires :target as
the actual :target while :table keeps its :ui-merged value (assert
both keys; control: an ordinary field's :ui (:table ...) passes
through unchanged with no :target key at all)."
  (let ((fe (fe-fields :record-permalink-child "admin")))
    (is (eq :authors
         (u:tree-get fe :view-form :hinted-author :target)))
    (is (eq :record-permalink-tags
         (u:tree-get fe :view-form :hinted-author :table)))
    (is-false (member :target
                (u:tree-get fe :view-form :name)))))

;;; ---------------------------------------------------------------------------
;;; rec stays clean; forms gate emission
;;; ---------------------------------------------------------------------------

(test internal-rec-carries-no-sibling
  "Internal rec output carries no sibling key, and its second value
is the view-result rows (the storage alias's value there is the
pristine UUID)."
  (multiple-value-bind (envelope rows)
    (rec (be-id :books '((:books :title :eq "Left Hand")) "admin")
      "admin")
    (declare (ignore rows))
    (let ((record (getf envelope :record)))
      (is-false (rp-sibling-p record :author)
        "internal rec must not decorate"))))

(test form-omitting-field-emits-no-sibling
  "A form that does not name the FK field emits no sibling: a
record with no target fields on the requested form grows no -id
keys at all."
  (let ((author-rec (rp-rec :authors "Ursula")))
    (is-false
      (loop for key in author-rec by #'cddr
        thereis (and (keywordp key)
                 (u:ends-with (format nil "~a" key) "-ID")
                 (not (member key '(:id)))))
      "a type with no target fields grows no -id keys")))

(test scoped-display-sibling-scope-aware
  "A scoped-target display emits the sibling on the viewer's own
scoped row and omits it on the other user's row."
  (let ((c1-viewer (rp-rec :record-permalink-child "child-1"
                     :user *rp-viewer*)))
    (is (equalp *rp-note-mine* (getf c1-viewer :scoped-note-id))
      "the viewer's own scoped row emits the sibling")
    ;; child-1's note belongs to the viewer, so admin's scope
    ;; (admin) filters him out: no sibling for admin on the same row.
    (let ((c1-admin (rp-rec :record-permalink-child "child-1" :user "admin")))
      (is-false (rp-sibling-p c1-admin :scoped-note)
        "a viewer with no scoped row gets no sibling"))))

;;; ---------------------------------------------------------------------------
;;; The one predicate: truth table
;;; ---------------------------------------------------------------------------

(test singular-target-predicate-truth-table
  "Direct predicate cases: a target field → t; a target +
:join-table field → nil (the M2M leg, even with :target present);
the injected :id field (on an RBAC type, :target :resources) → nil;
a non-target field → nil."
  (let ((m *compiled-model*))
    (is-true (singular-target-field-p
               (u:tree-get m :books :fields :author)))
    (is-false (singular-target-field-p
                (u:tree-get m :magazines :fields :topics)))
    (is-false (singular-target-field-p
                (u:tree-get m :users :fields :id)))
    (is-false (singular-target-field-p
                (u:tree-get m :books :fields :title)))))

;;; ---------------------------------------------------------------------------
;;; The compile guard: synthetic-key collisions
;;; ---------------------------------------------------------------------------

(defun rp-collision-model (fields)
  `(:posts (:table t
             :create :auto :update :auto :delete :auto
             :type-roles ("logged-in")
             :views (:main (:tables (:posts :targets)))
             :fields ,fields)
    ;; The target type named by :views must exist, or the "Unknown
    ;; target" error fires before the collision guard gets its turn.
    :targets (:table t
              :create :auto :update :auto :delete :auto
              :type-roles ("logged-in")
              :views (:main (:tables (:targets)))
              :fields
              (:name (:type :text :identity t
                     :ui (:label "Name" :widget :textbox)
                     :source (:view :main :column :name :agg :first)
                     :column t :not-null t :unique t)))))

(test collision-guard-reference-form
  "A plain :target :target beside a :reference to :targets (the
reference renames to its effective key :target-id, colliding with
:target's derived sibling) is a compile error naming the
colliding sibling key."
  (signals error
    (compile-model
      (rp-collision-model
        '(:name (:type :text :identity t
                 :ui (:label "N" :widget :textbox)
                 :source (:view :main :column :name :agg :first)
                 :column t :not-null t :unique t)
          :target (:type :text :target :targets
                  :ui (:label "P" :widget :search-select)
                  :source (:view :main :table :targets
                           :column :name :agg :first)
                  :column t)
          :reference (:target :targets))))))

(test collision-guard-mirror-form
  "The mirror model: a :target :post-id beside an ordinary field
named :post-id-id (the target's actual derived sibling is
:post-id-id; no :reference belongs in this model)."
  (signals error
    (compile-model
      (rp-collision-model
        '(:name (:type :text :identity t
                 :ui (:label "N" :widget :textbox)
                 :source (:view :main :column :name :agg :first)
                 :column t :not-null t :unique t)
          :post-id (:type :text :target :targets
                    :ui (:label "P" :widget :search-select)
                    :source (:view :main :table :targets
                             :column :name :agg :first)
                    :column t)
          :post-id-id (:type :text
                        :ui (:label "X" :widget :textbox)
                        :source (:view :main :column :name
                                 :agg :first)))))))

(defun rp-tiny-model ()
  "A minimal legal one-target model for the guard's wording probe."
  (rp-collision-model
    '(:name (:type :text :identity t
             :ui (:label "N" :widget :textbox)
             :source (:view :main :column :name :agg :first)
             :column t :not-null t :unique t)
      :target (:type :text :target :targets
              :ui (:label "P" :widget :search-select)
              :source (:view :main :table :targets :column :name
                       :agg :first)
              :column t)
      :target-id (:type :text
                :ui (:label "X" :widget :textbox)
                :source (:view :main :column :name :agg :first)))))

(test collision-guard-wording
  "The collision error text names the colliding sibling key."
  (let ((condition
          (handler-case
              (progn (compile-model (rp-tiny-model)) nil)
            (error (e) e))))
    (is-true condition "expected a compile error")
    (is (re:scan "TARGET-ID"
             (format nil "~a" condition))
      "the error must name the colliding sibling key")))
